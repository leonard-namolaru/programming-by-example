type pos_expression = Forward of int | Backward of int
type expression = Const of string | Extract of pos_expression * pos_expression
type program = expression list

let const str = str
let forward i = i
let backward i str = (String.length str) - i
let extract str (pos_initial,pos_finale) = String.sub str pos_initial (pos_finale  - pos_initial)

let evaluation_expression expression chaine = match expression with
                                        |Const str -> const str
                                        |Extract (pos1, pos2) -> match (pos1, pos2) with
                                                                        |(Forward index1,Forward index2)  -> extract chaine(forward index1, forward index2)
                                                                        |(Backward index1,Backward index2)  -> extract chaine(backward index1 chaine,backward index2 chaine)
                                                                        |(Forward index1,Backward index2)  -> extract chaine(forward index1,backward index2 chaine)
                                                                        |(Backward index1,Forward index2)  -> extract chaine (backward index1 chaine,forward index2)
let evaluation_program program chaine = 
  let rec f program string_resultat = match program with
                                    |[] -> ""
                                    |expression::[] -> let resultat = (evaluation_expression expression chaine) in
                                                        string_resultat ^ resultat
                                    |expression::t -> let resultat = (evaluation_expression expression chaine) in
                                                                  f t (string_resultat ^ resultat)
in f program ""

(* TEST *)
let _ = evaluation_expression (Const "str") "str"
let _ = evaluation_expression (Extract (Forward 1,Forward 2)) "str" (* resultat : t *) 

(* TEST *)
let x= [Const "s";Const "s"] 
let _ = evaluation_program x "aymen"
let y = [Extract (Forward 1,Forward 2);Extract (Forward 2,Forward 4)] 
let _ =evaluation_program y "aymen"

let z = [Const("Hello, "); Extract(Forward(3), Backward(7))] 
let _ =evaluation_program z "Mr Smith junior"

(* ******* *)

(* dans la syntaxe abstraite de Concat, extract prend deux pos expressions en arguments,
mais dans les etiquettes d’un DAG, extract prend deux ensembles de pos expressions en arguments. *)
type expression_dag = Const of string | Extract of (pos_expression * pos_expression) * (pos_expression * pos_expression) 

type dag = {nodes : string list; aretes: (string * string * expression_dag list) list }

 (* Exemple *)
let _ = {nodes = ["";"d";"dx";"dxa"]; aretes =[("","d",[Const "d"]);("d","dx",[Const "x"]);("dx","dxa",[Const "a"])]}

(* ******* *)

let string_to_nodes str = let rec f liste str = match String.length str with
                         |0 -> liste
                         |_ -> let nouveau_element = (List.nth liste ((List.length liste) - 1))^(String.sub str 0 1) in
															 f (liste@[nouveau_element]) (String.sub str 1 ((String.length str) -1))
	in f [""] str;;

(* TEST *)
let nodes_liste = string_to_nodes "dxa" (* string list = [""; "d"; "dx"; "dxa"] *)

(* Une fonction qui renvoie l'index de la première occurrence de str2 dans str1 *)
let index_of str1 str2 = 
	let rec f index str1 str2 = match String.length str1 with
	                                   |0 -> -1
																		 |_ ->  let verification = (String.get str1 0) =  (String.get str2 0) in
																		           if not (verification) 
																									then f (index + 1) (String.sub str1 1 ((String.length str1) -1)) str2 
																							 else 
																										if (String.length str1 >= String.length str2) && ((String.sub str1 0 (String.length str2)) = str2)
																											then index
																										else f (index + 1) (String.sub str1 1 ((String.length str1) -1)) str2
	in if (String.length str2) > (String.length str1) then -1 else f 0 str1 str2
	
(* TEST *)
let _ = index_of "abad" "a"
let _ = index_of "abad" "z" (* -1 *)

(* Une fonction qui renvoie une liste de tous les emplacements de str2 dans str1. *)
let indexes_of str1 str2 = 
	let rec f index indexes_liste str1 str2 = match String.length str1 with
	                            |0 -> indexes_liste
															|_ -> let prochain_index_of = index_of str1 str2 in
															          if prochain_index_of = -1 
																					then indexes_liste
																				else 
																					let index_continuite_recherche = prochain_index_of + (String.length str2) in
																						if index_continuite_recherche > (String.length str1)
																							then indexes_liste
																						else f (index + index_continuite_recherche) (indexes_liste@[index + prochain_index_of]) (String.sub str1 (index_continuite_recherche) ((String.length str1)- index_continuite_recherche)) str2
	in if (String.length str2) > (String.length str1) then [] else f 0 [] str1 str2  
																					
(* TEST *)
let _ = indexes_of "abad" "a" (* int list = [0; 2] *)
let _ = indexes_of "abad" "7" (* int list = [] *)

let pos_expression_dag_of_indexes_liste indexes_liste str1 str2 = 
	let rec f pos_expression_liste indexes_liste str1_len str2_len = match indexes_liste with 
																										|[] -> pos_expression_liste
																										|h::t -> let expression = Extract ((Forward h,Backward (str1_len - h)),(Forward (h + str2_len),Backward (str1_len - (h + str2_len)))) in 
																										         f (pos_expression_liste@[expression]) t str1_len str2_len
	in f [] indexes_liste (String.length str1) (String.length str2)
	
(* TEST *)
let _ = pos_expression_dag_of_indexes_liste [0;2] "abad" "a"
(* expression_dag list = [Extract ((Forward 0, Backward 4), (Forward 1, Backward 3)); Extract ((Forward 2, Backward 2), (Forward 3, Backward 1))] *)
let _ = pos_expression_dag_of_indexes_liste [3] "abad" "d" 
(* - : expression_dag list = [Extract ((Forward 3, Backward 1), (Forward 4, Backward 0))] *)


(* ******* *)

let string_sub_first str_source str_to_sub = let len = String.length str_to_sub in
	String.sub str_source len ((String.length str_source) -len)

let string_to_aretes str1 str2 node_list= let rec f aretes_liste str1 str2 node_list = match node_list with 
                                               |[]-> aretes_liste
																							 |h::t -> if h = str2 then f aretes_liste str1 str2 t
																								        else if (String.length h) = 1 
																							          then let  nouveau_element = (str2, h, [Const h]@(pos_expression_dag_of_indexes_liste (indexes_of str1 h) str1 h)) in  f (aretes_liste@[nouveau_element]) str1 str2 t
																												else let  nouveau_element = (str2, h, [Const (string_sub_first h str2)]@(pos_expression_dag_of_indexes_liste (indexes_of str1 (string_sub_first h str2)) str1 (string_sub_first h str2))) in f (aretes_liste@[nouveau_element]) str1 str2 t
																							          
	in f [] str1 str2 node_list																						   
																							
let nodes_to_aretes nodes_liste str1 str2 = let rec f aretes_liste nodes_liste str1 str2 = match nodes_liste with
                                                |[] -> aretes_liste
                                                |h::[] -> aretes_liste
                                                |h::t -> let aretes = string_to_aretes str1 h t in
																								         f (aretes_liste@aretes) t str1 str2
		in f [] nodes_liste str1 str2

(* TEST *)
let aretes_liste = nodes_to_aretes nodes_liste "abad" "dxa"

(* ******* *)

(* Construction d'un graphe : representation d’un ensemble de programmes *)
let cons_dag str1 str2 = let nodes_liste = string_to_nodes str2 in {nodes = nodes_liste ; aretes = nodes_to_aretes nodes_liste str1 str2}
let _ = cons_dag "abad" "dxa"
let _ = cons_dag "efegh" "ghxe"


