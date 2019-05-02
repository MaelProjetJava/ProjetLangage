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

let creer_afn str =
	let rec helper seq_afn str i =
		if i <= String.length str then
			helper (ajouter_ieme_char seq_afn str i) str (i + 1)
		else
			seq_afn
	in
		finaliser_afn (helper (creer_afn_base str.[0]) str 2);;

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
				'T' -> 4
				| 'A' -> 1
				| 'C' -> 1
				| 'G' -> 1
		}
		| 4 -> {
			accept = false;
			t = function
				'A' -> 5
				| 'C' -> 1
				| 'G' -> 1
				| 'T' -> 1
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
				'T' -> [4]
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

let matching_str = "GTGCCGAGCTGAGTTCCTTATAAGAATTAATCTTAATTTTGTATTTTTTCCTGTAAGA";;
let unmatching_str = "GTGCCGAGCTGAGTTCCTATAAGAATTAATCTTTTTTGTATTTTTTCCTGTAAGA";;

accepte_afd afd_test matching_str;;
(* - : bool = true *)

accepte_afd afd_test unmatching_str;;
(* - : bool = false *)

accepte_afn afn_test matching_str;;
(* - : bool = true *)

accepte_afn afn_test unmatching_str;;
(* - : bool = false *)
