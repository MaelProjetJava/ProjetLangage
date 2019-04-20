(* ----- Définition des types automates ----- *)

type etat = {accept: bool; t: char -> int};;

type afd = {sigma: char list; nQ: int; init: int; e: int -> etat};;

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

accepte_afd afd_test "GTGCCGAGCTGAGTTCCTTATAAGAATTAATCTTAATTTTGTATTTTTTCCTGTAAGA";;
(* - : bool = true *)

accepte_afd afd_test "GTGCCGAGCTGAGTTCCTATAAGAATTAATCTTTTTTGTATTTTTTCCTGTAAGA";;
(* - : bool = false *)
