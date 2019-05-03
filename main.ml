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
				| 'C' -> 1
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
				| 'C' -> 1
				| 'G' -> 1
				| 'T' -> 3
		}
		| 5 -> {
			accept = true;
			t = function _ -> 1
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
