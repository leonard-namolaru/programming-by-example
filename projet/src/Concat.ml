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

let dag1 = cons_dag "10/10/2017" "10"
let dag2 = cons_dag "05-15-2015" "15"

(* Intersection de deux ensembles *)
type intersection_dag = {nodes : string*string list; aretes: (string * string * expression_dag list) list }

let ensemble_noeuds nodes_liste1 nodes_liste2 = 
	let rec f nodes_intersection nodes_liste1 nodes_liste2 str1 str2 = match nodes_liste1,nodes_liste2 with
	                                                            |[],[] -> nodes_intersection
																															|h1::t1,h2::t2 -> f (nodes_intersection@[(h1,h2)]) t1 t2 str1 str2
																															|h1::t1,[] ->     f (nodes_intersection@[(h1,str2)]) t1 [] str1 str2
																															|[],h2::t2 ->   f (nodes_intersection@[(str1,h2)]) [] t2 str1 str2
	in f [] nodes_liste1 nodes_liste2 (List.nth nodes_liste1 ((List.length nodes_liste1) - 1)) (List.nth nodes_liste2 ((List.length nodes_liste2) - 1))
	
(* TEST *)
let liste_ensemble_noeuds = ensemble_noeuds (string_to_nodes "dxa") (string_to_nodes "ghxe")
(* (string * string) list = [("", ""); ("d", "g"); ("dx", "gh"); ("dxa", "ghx"); ("dxa", "ghxe")] *)

(* Une fonction qui permet de trouver une arrete compte tenu de ses 2 noeuds *)
let rec get_arete aretes_liste node1 node2 = match aretes_liste with
                                    |(node_debut , node_fin , expression_dag_liste)::t -> if (node_debut = node1) && (node_fin = node2)
																	                                                          then (node_debut , node_fin , expression_dag_liste)
																																													else
																																														get_arete t node1 node2
																		|[] -> (node1 , node2 , []) 

(* TEST *)
let _ = get_arete aretes_liste "" "d"
(* - : string * string * expression_dag list =
("", "d", [Const "d"; Extract ((Forward 3, Backward 1), (Forward 4, Backward 0))]) *)

let rec expression_dag_comparaison expression1 expression2 = match expression1,expression2 with
          |Const str1,Const str2 -> (str1 = str2)
				  |Extract ((Forward num1a,Backward num2a),(Forward num3a, Backward num4a)) , Extract ((Forward num1b, Backward num2b),(Forward num3b, Backward num4b))
					-> (num1a = num1b) && (num2a = num2b) && (num3a = num3b) && (num4a = num4b)
					| _ -> false 

(* TEST *)
let _ = expression_dag_comparaison (Const "x") (Const "y")
let _ = expression_dag_comparaison (Extract ((Forward 1, Backward 2),(Forward 3, Backward 4)) ) (Extract ((Forward 1, Backward 2),(Forward 3, Backward 4)) )
let _ = expression_dag_comparaison (Const "x") (Extract ((Forward 1, Backward 2),(Forward 3, Backward 4)) )

(* let arretes_partie_commune arrete1 arrete2 =  
	let rec f partie_commune expression_dag_liste1 expression_dag_liste2 = match expression_dag_liste1,expression_dag_liste2 with 
	        	|[],[] -> nodes_intersection
						|h1::t1,h2::t2 -> f (nodes_intersection@[(h1,h2)]) t1 t2 str1 str2
						|h1::t1,[] ->     f (nodes_intersection@[(h1,str2)]) t1 [] str1 str2
						|[],h2::t2 ->   f (nodes_intersection@[(str1,h2)]) [] t2 str1 str2

	                        |[],[] ->  partie_commune
													|(Const str1)::[], (Const str2)::t2 -> if (str1 = str2) then f partie_commune@[Const str1] (Const str1) t2
														                                     else f partie_commune (Const str1) t2
												  |(Const str1)::t1,(Const str2)::t2 -> if (str1 = str2) then f partie_commune@[Const str1] t1 t2
																																else f partie_commune t1 t2

	                        |(Const str1)::t1,(Const str2)::t2 -> if (str1 = str2) then f partie_commune@[Const str1] t1 t2
																																else f partie_commune t1 t2
*)
(*
let ensemble_aretes liste_ensemble_noeuds liste_aretes1 liste_aretes2 = 
	let rec f aretes_intersection liste_aretes1 liste_aretes2 = match liste_aretes1,liste_aretes2 with
	                    |[],[]         -> aretes_intersection
											|h1::t1,h2::t2 -> 
															 																
																																
																																f (nodes_intersection@[(h1,h2)]) t1 t2 str1 str2
																															|h1::t1,[] ->     f (nodes_intersection@[(h1,str2)]) t1 [] str1 str2
																															|[],h2::t2 ->   f (nodes_intersection@[(str1,h2)]) [] t2 str1 str2
	in f [] nodes_liste1 nodes_liste2 (List.nth nodes_liste1 ((List.length nodes_liste1) - 1)) (List.nth nodes_liste2 ((List.length nodes_liste2) - 1))
*)