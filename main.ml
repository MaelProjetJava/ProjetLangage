(* ----- Définition des types automates ----- *)

type etat = {accept: bool; t: char -> int};;

type afd = {sigma: char list; nQ: int; init: int; e: int -> etat};;


type etatN = {acceptN: bool; tN: char -> int list};;

type afn = {sigmaN: char list; nN: int; initN: int list; eN : int -> etatN};;

(* ----- Fonctions de lecture ----- *)

let tetec = function
| "" -> failwith "Erreur: chaine vide"
| s -> s.[0];;
(* val tetec : string -> char = <fun> *)

let reste = function
| "" -> failwith "Erreur: chaine vide"
| s -> String.sub s 1  ((String.length s) - 1);;
(* val reste : string -> string = <fun> *)

let rec not_in_list c = function
	x::l -> (c <> x) && (not_in_list c l)
	| _ -> true;;
(* val not_in_list : 'a -> 'a list -> bool = <fun> *)



exception NotInAlphabet of char;;

let accepte_afd afd str =
	let rec helper str state =
		(afd.e state).accept || (
			if (String.length str) = 0 then
				(afd.e state).accept
			else
				let current_char = tetec str in
					if not_in_list current_char afd.sigma then
						raise (NotInAlphabet current_char)
					else
						try helper (reste str) ((afd.e state).t current_char) with
							Match_failure _ -> false
		)
	in
		helper str afd.init;;
(* val accepte_afd : afd -> string -> bool = <fun> *)

let accepte_afn afn str =
	let rec helper str = function
		state::states ->
			(afn.eN state).acceptN  || (
				if (String.length str) = 0 then
					helper str states
				else
					let current_char = tetec str in
						if not_in_list current_char afn.sigmaN then
							raise (NotInAlphabet current_char)
						else
							(try helper (reste str) ((afn.eN state).tN current_char) with
								Match_failure _ -> false)
							|| (helper str states)
			)
		| _ -> false
	in
		helper str afn.initN;;
(* val accepte_afn : afn -> string -> bool = <fun> *)

(* ----- Création automate non-déterminite ----- *)

let creer_afn_base first_char = {
	sigmaN = ['A'; 'C'; 'G'; 'T'];
	nN = 1;
	initN = [1];
	eN = function
		1 ->  {
			acceptN = false;
			tN = function c ->
					if c <> first_char then
						[1]
					else
						[1;2]
		}
};;
(* val creer_afn_base : char -> afn = <fun> *)

let finaliser_afn seq_afn = {
	sigmaN = seq_afn.sigmaN;
	nN = seq_afn.nN + 1;
	initN = seq_afn.initN;
	eN = function x ->
		if x = seq_afn.nN + 1 then
			{
				acceptN = true;
				tN = function _ -> []
			}
		else
			seq_afn.eN x
};;
(* val finaliser_afn : afn -> afn = <fun> *)

let ajouter_ieme_char seq_afn str i = {
	sigmaN = seq_afn.sigmaN;
	nN = seq_afn.nN + 1;
	initN = seq_afn.initN;
	eN = function x ->
			if x = i then
				{
					acceptN = false;
					tN = function c ->
							if c = str.[i - 1] then
								[i + 1]
							else
								raise (Match_failure ("", 0, 0))
				}
			else
				seq_afn.eN x
};;
(* val ajouter_ieme_char : afn -> string -> int -> afn = <fun> *)

let creer_afn str =
	let rec helper seq_afn i =
		if i <= String.length str then
			helper (ajouter_ieme_char seq_afn str i) (i + 1)
		else
			seq_afn
	in
		finaliser_afn (helper (creer_afn_base str.[0]) 2);;
(* val creer_afn : string -> afn = <fun> *)

(* ----- Création automate déterministe ----- *)

let rec est_prefixe prefixe str = (String.length prefixe = 0) ||
	(String.length str <> 0 && prefixe.[0] = str.[0] && est_prefixe (reste prefixe) (reste str));;
(* val est_prefixe : string -> string -> bool = <fun> *)

let afd_base = {
	sigma = ['A'; 'C'; 'G'; 'T'];
	nQ = 0;
	init = 1;
	e = function _ -> raise (Match_failure ("", 0, 0))
};;
(* val afd_base : afd = {sigma = ['A'; 'C'; 'G'; 'T']; nQ = 0; init = 1; e = <fun>} *)

let ajouter_transition seq_afd i lettre j = {
	sigma = seq_afd.sigma;
	nQ = seq_afd.nQ;
	init = seq_afd.init;
	e = function x ->
		if x = i then
			let ancien_etat = try seq_afd.e x with
					Match_failure _ -> {
						accept = false;
						t = function _ -> raise (Match_failure ("", 0, 0))
					}
			in
				{
					accept = ancien_etat.accept;
					t = function c ->
						if c = lettre then
							j
						else
							ancien_etat.t c
				}
		else
			seq_afd.e x
};;
(* val ajouter_transition : afd -> int -> char -> int -> afd = <fun> *)

let calculer_transition seq_afd str i lettre =
	let rec helper j =
		if j > i then
			ajouter_transition seq_afd (i + 1) lettre 1
		else
			let len_prefixe = i - j in
				if est_prefixe (String.sub str j (len_prefixe)) str && str.[len_prefixe] = lettre then
					ajouter_transition seq_afd (i + 1) lettre (i - j + 2)
				else
					helper (j + 1)
	in
		helper 0;;
(* val calculer_transition : afd -> string -> int -> char -> afd = <fun> *)

let rec calculer_transitions_sigma seq_afd str i = function
	l::sigma_t -> calculer_transitions_sigma (calculer_transition seq_afd str i l) str i sigma_t
	| _ -> seq_afd
;;
(* val calculer_transitions_sigma : afd -> string -> int -> char list -> afd = <fun> *)

let finaliser_afd seq_afd len_str =
	let nQ = len_str + 1 in
		{
			sigma = seq_afd.sigma;
			nQ = nQ;
			init = seq_afd.init;
			e = function x ->
				if x = nQ then
					{
						accept = true;
						t = function _ -> raise (Match_failure ("", 0, 0))
					}
				else
					seq_afd.e x
		};;
(* val val finaliser_afd : afd -> int -> afd = <fun> *)

let creer_afd str =
	let len_str = String.length str in
	let rec helper seq_afd i =
		if i < len_str then
			helper (calculer_transitions_sigma seq_afd str i seq_afd.sigma) (i + 1)
		else
			seq_afd
	in
		finaliser_afd (helper afd_base 0) len_str
;;
(* val creer_afd : string -> afd = <fun> *)

(* ----- Fonctions d'affichage ----- *)

let rec afficher_transitions_afd t_func = function
	l::sigma_t ->
		(try
			"\t\t\t'" ^
			(String.make 1 l) ^
			"' -> " ^
			(string_of_int (t_func l)) ^
			"\n"
		with
			Match_failure _ -> "") ^
		(afficher_transitions_afd t_func sigma_t)
	| _ -> "";;

(*
 * On remercie chaleureusement la puissance de OCaml qui ne permet pas de factoriser les fonctions
 * afficher_liste_etats et afficher_sigma et bon nombre d'autres fonctions ci-dessous. Certains
 * forums suggèrent de recourir à une fonction en C externe pour outrepasser ces limitations, de
 * quoi se poser des questions...
 *)
let afficher_liste_etats liste =
	let rec helper = function
		l::liste_t -> let str_l = (string_of_int l) in
			(if List.length liste_t <> 0 then
				str_l ^ ", "
			else
				str_l) ^ helper liste_t
		| _ -> "]"
	in
		"[" ^ (helper liste)
;;

let rec afficher_transitions_afn t_func = function
	l::sigma_t ->
		(try
			"\t\t\t'" ^
			(String.make 1 l) ^
			"' -> " ^
			(afficher_liste_etats (t_func l)) ^
			"\n"
		with
			Match_failure _ -> "") ^
		(afficher_transitions_afn t_func sigma_t)
	| _ -> ""
;;

let afficher_etat_afd afd i =
	let etat = afd.e i in
		"\t\t" ^
		(string_of_int i) ^
		" -> {\n" ^
		"\t\t\taccept = " ^ (string_of_bool etat.accept) ^ "\n" ^
		(afficher_transitions_afd etat.t afd.sigma) ^
		"\t\t}\n"
;;

let afficher_etat_afn afn i =
	let etat = afn.eN i in
		"\t\t" ^
		(string_of_int i) ^
		" -> {\n" ^
		"\t\t\taccept = " ^ (string_of_bool etat.acceptN) ^ "\n" ^
		(afficher_transitions_afn etat.tN afn.sigmaN) ^
		"\t\t}\n"
;;

let afficher_tous_etats_afd afd =
	let rec helper i =
		if i <= afd.nQ then
			(afficher_etat_afd afd i) ^ helper (i + 1)
		else
			""
	in
		helper 1
;;

(* Et encore un peu plus de code doublon... *)
let afficher_tous_etats_afn afn =
	let rec helper i =
		if i <= afn.nN then
			(afficher_etat_afn afn i) ^ helper (i + 1)
		else
			""
	in
		helper 1
;;

let afficher_sigma sigma =
	let rec helper = function
		l::sigma_t -> let str_l = (String.make 1 l) in
			(if List.length sigma_t <> 0 then
				str_l ^ ", "
			else
				str_l) ^ helper sigma_t
		| _ -> "]"
	in
		"[" ^ (helper sigma)
;;

let afficher_afd afd =
print_string (
	"{\n" ^
	"\tsigma = " ^ (afficher_sigma afd.sigma) ^ "\n" ^
	"\tnQ = " ^ (string_of_int afd.nQ) ^ "\n" ^
	"\tinit = " ^ (string_of_int afd.init) ^ "\n" ^
	"\te = \n" ^
	(afficher_tous_etats_afd afd) ^
	"}\n"
);;

(* Désespérant... *)
let afficher_afn afn =
print_string (
	"{\n" ^
	"\tsigma = " ^ (afficher_sigma afn.sigmaN) ^ "\n" ^
	"\tnQ = " ^ (string_of_int afn.nN) ^ "\n" ^
	"\tinit = " ^ (afficher_liste_etats afn.initN) ^ "\n" ^
	"\te = \n" ^
	(afficher_tous_etats_afn afn) ^
	"}\n"
);;

(* ----- Tests pour les fonctions de lectures ----- *)

let afd_test = {
	sigma = ['A'; 'C'; 'G'; 'T'];
	nQ = 5;
	init = 1;
	e = function
		1 -> {
			accept = false;
			t = function
				'C' -> 2
				| 'A' -> 1
				| 'G' -> 1
				| 'T' -> 1
		}
		| 2 -> {
			accept = false;
			t = function
				'T' -> 3
				| 'A' -> 1
				| 'C' -> 2
				| 'G' -> 1
		}
		| 3 -> {
			accept = false;
			t = function
				'C' -> 4
				| 'A' -> 1
				| 'T' -> 1
				| 'G' -> 1
		}
		| 4 -> {
			accept = false;
			t = function
				'A' -> 5
				| 'C' -> 2
				| 'G' -> 1
				| 'T' -> 3
		}
		| 5 -> {
			accept = true;
			t = function _ -> raise (Match_failure ("", 0, 0))
		}
};;

let afn_test = {
	sigmaN = ['A'; 'C'; 'G'; 'T'];
	nN = 5;
	initN = [1];
	eN = function
		1 -> {
			acceptN = false;
			tN = function
				'C' -> [2;1]
				| 'A' -> [1]
				| 'G' -> [1]
				| 'T' -> [1]
		}
		| 2 -> {
			acceptN = false;
			tN = function
				'T' -> [3]
		}
		| 3 -> {
			acceptN = false;
			tN = function
				'C' -> [4]
		}
		| 4 -> {
			acceptN = false;
			tN = function
				'A' -> [5]
		}
		| 5 -> {
			acceptN = true;
			tN = function _ -> []
		}
};;

let matching_str = "GTGCCGAGCTGAGTTCGCTCTCATAAGAATTAATCTTAATTTTGTATTTTTTCCTGTAAGA";;
let unmatching_str = "GTGCCGAGCTGAGTTCCTCTCCTATAAGAATTAATCTTAATTTTGTATTTTTTCCTGTAAGA";;

accepte_afd afd_test matching_str;;
(* - : bool = true *)

accepte_afd afd_test unmatching_str;;
(* - : bool = false *)

accepte_afn afn_test matching_str;;
(* - : bool = true *)

accepte_afn afn_test unmatching_str;;
(* - : bool = false *)

(* ----- Tests pour les fonctions de créations ----- *)

let afd_auto = creer_afd "CTCA";;
afficher_afd afd_auto;;

let afn_auto = creer_afn "CTCA";;
afficher_afn afn_auto;;

accepte_afd afd_auto matching_str;;
(* - : bool = true *)

accepte_afd afd_auto unmatching_str;;
(* - : bool = false *)

accepte_afn afn_auto matching_str;;
(* - : bool = true *)

accepte_afn afn_auto unmatching_str;;
(* - : bool = false *)

(* ----- Tests de vitesse en 'situation réelle' ----- *)

let readfile filename =
	let channel = open_in filename in
		try (input_line channel) with
		End_of_file -> let _ = close_in channel in ""
;;

let dna_sequence = readfile "dna_sequence.txt";;
let w_match =   "ACTGTGCCACTGCA";;
let w_nomatch = "CTTACGTCATACCG";;



let start_afd_build = Sys.time();;
let afd_speed_match = creer_afd w_match;;
let afd_speed_nomatch = creer_afd w_nomatch;;
let end_afd_build = Sys.time();;
let afd_build_time = end_afd_build -. start_afd_build;;

let start_afd_match = Sys.time();;
accepte_afd afd_speed_match dna_sequence;;
accepte_afd afd_speed_nomatch dna_sequence;;
let end_afd_match = Sys.time();;
let afd_time_match = end_afd_match -. start_afd_match;;



let start_afn_build = Sys.time();;
let afn_speed_match = creer_afn w_match;;
let afn_speed_nomatch = creer_afn w_nomatch;;
let end_afn_build = Sys.time();;
let afn_build_time = end_afn_build -. start_afn_build;;

let start_afn_match = Sys.time();;
accepte_afn afn_speed_match dna_sequence;;
accepte_afn afn_speed_nomatch dna_sequence;;
let end_afn_match = Sys.time();;
let afn_time_match = end_afn_match -. start_afn_match;;

print_string (
	"Déterministe:\n\tTemps construction: " ^
	(string_of_float afd_build_time) ^
	"\n\tTemps exécution: " ^
	(string_of_float afd_time_match) ^
	"\n\tTotal: " ^
	(string_of_float (afd_build_time +. afd_time_match)) ^
	"\nNon-déterministe:\n\tTemps construction: " ^
	(string_of_float afn_build_time) ^
	"\n\tTemps exécution: " ^
	(string_of_float afn_time_match) ^
	"\n\tTotal: " ^
	(string_of_float (afn_build_time +. afn_time_match)) ^
	"\n"
);;
