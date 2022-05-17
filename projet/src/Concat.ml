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
let dag1 = cons_dag "abad" "dxa"
let dag2 = cons_dag "efegh" "ghxe"

let _ = cons_dag "10/10/2017" "10"
let _ = cons_dag "05-15-2015" "15"

(* Intersection de deux ensembles *)
type intersection_dag = {nodes : (string*string) list; aretes: ((string*string) * (string*string) * expression list) list }

let ensemble_noeuds2 h1 nodes_liste2 = 
	let rec f nodes_intersection h1 nodes_liste2 = match nodes_liste2 with
																															|h2::t2 -> f (nodes_intersection@[(h1,h2)]) h1 t2 
																															|[] -> nodes_intersection
																																
	in f [] h1 nodes_liste2

let ensemble_noeuds nodes_liste1 nodes_liste2 = 
	let rec f nodes_intersection nodes_liste1 nodes_liste2 = match nodes_liste1 with
																															|h1::t1 -> f (nodes_intersection@(ensemble_noeuds2 h1 nodes_liste2)) t1 nodes_liste2 
																															|[] -> nodes_intersection
																																
	in f [] nodes_liste1 nodes_liste2
	
(* TEST *)
let liste_ensemble_noeuds = ensemble_noeuds (string_to_nodes "dxa") (string_to_nodes "ghxe")
(* (string * string) list = 
  [("", ""); ("", "g"); ("", "gh"); ("", "ghx"); ("", "ghxe"); ("d", "");
   ("d", "g"); ("d", "gh"); ("d", "ghx"); ("d", "ghxe"); ("dx", "");
   ("dx", "g"); ("dx", "gh"); ("dx", "ghx"); ("dx", "ghxe"); ("dxa", "");
   ("dxa", "g"); ("dxa", "gh"); ("dxa", "ghx"); ("dxa", "ghxe")]
 *)

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

let expression_dag_extraire_partie_identique (expression1:expression_dag) (expression2:expression_dag) : expression option = match expression1,expression2 with
          |Const str1,Const str2 -> if (str1 = str2) then Some (Const str1) else None
				  |Extract ((Forward num1a,Backward num2a),(Forward num3a, Backward num4a)) , Extract ((Forward num1b, Backward num2b),(Forward num3b, Backward num4b))
					         -> if (num1a = num1b) && (num3a = num3b) then  Some (Extract (Forward num1a,Forward num3a)) 
									    else if (num2a = num2b) && (num4a = num4b) then Some (Extract (Backward num2a,Backward num4a))
											else if (num2a = num2b) && (num3a = num3b) then Some (Extract (Backward num2a,Forward num3a))
											else if (num1a = num1b) && (num4a = num4b) then Some (Extract (Forward num1a,Backward num4a))
											else None
					|_,_ -> None

(* TEST *)
let _ = expression_dag_extraire_partie_identique (Const "x") (Const "y")
let _ = expression_dag_extraire_partie_identique (Extract ((Forward 3, Backward 1), (Forward 4, Backward 0)) ) (Extract ((Forward 3, Backward 2), (Forward 4, Backward 1)) )
let _ = expression_dag_extraire_partie_identique (Extract ((Forward 3, Backward 1), (Forward 4, Backward 0)) ) (Extract ((Forward 3, Backward 2), (Forward 0, Backward 4)) )
let _ = expression_dag_extraire_partie_identique (Const "x") (Extract ((Forward 1, Backward 2),(Forward 3, Backward 4)) )

(* **** *)

let arretes_partie_commune arrete1 arrete2 =  
	let rec f partie_commune expression_dag_liste1 expression_dag_liste2 = match expression_dag_liste1 with 
	        	|[] -> partie_commune
						|h::t -> let extraire_partie_identique = List.find_opt (fun element_liste -> Option.is_some element_liste) (List.map (fun element_liste -> expression_dag_extraire_partie_identique h element_liste) expression_dag_liste2) in
						           if Option.is_some extraire_partie_identique then f (partie_commune@[Option.get extraire_partie_identique]) t expression_dag_liste2
											 else f partie_commune t expression_dag_liste2
											
	in match arrete1,arrete2 with (node_debut1 , node_fin1 , expression_dag_liste1),(node_debut2 , node_fin2 , expression_dag_liste2) -> 
		match (f [] expression_dag_liste1 expression_dag_liste2) with
		     |[] -> None
				 |h::t -> let extraire_extract = List.find_opt (fun element_liste -> if (Option.is_some element_liste) then match (Option.get element_liste) with |((Extract (x1,x2)): expression) -> true |_ -> false else false) (h::t) in
				            if Option.is_some extraire_extract then Option.get extraire_extract
										else let extraire_const = List.find_opt (fun element_liste -> if (Option.is_some element_liste) then match (Option.get element_liste) with |((Const x): expression) -> true |_ -> false else false) (h::t) in
										if Option.is_some extraire_const then Option.get extraire_const
										else None
(* TEST *)							
let _ = arretes_partie_commune ("", "d",[Const "d"; Extract ((Forward 3, Backward 1), (Forward 4, Backward 0))])	("", "g",[Const "g"; Extract ((Forward 3, Backward 2), (Forward 4, Backward 1))])					
(* - : expression option = Some (Extract (Forward 3, Forward 4)) *)

let ensemble_dag2 noeud liste_ensemble_noeuds liste_aretes1 liste_aretes2  = 
	let rec f nodes_nouvelle_intersection aretes_intersection (i1,j1) liste_ensemble_noeuds liste_aretes1 liste_aretes2 = match liste_ensemble_noeuds with
	                    |[] -> {nodes = nodes_nouvelle_intersection ; aretes = aretes_intersection}
	                    |(i2,j2)::[] -> let arrete_commune = arretes_partie_commune (get_arete liste_aretes1 i1 i2) (get_arete liste_aretes2 j1 j2) in 
																			 if Option.is_some arrete_commune 
												                   then {nodes = ((nodes_nouvelle_intersection@[(i1,j1)])@[(i2,j2)]) ; aretes = (aretes_intersection@[((i1,j1), (i2,j2), [Option.get arrete_commune])]) } 
																					 else {nodes = nodes_nouvelle_intersection ; aretes = aretes_intersection}
											|(i2,j2)::t -> let arrete_commune = arretes_partie_commune (get_arete liste_aretes1 i1 i2) (get_arete liste_aretes2 j1 j2) in 
											                       if Option.is_some arrete_commune 
																					      then f ((nodes_nouvelle_intersection@[i1,j1])@[i2,j2]) (aretes_intersection@[((i1,j1), (i2,j2), [Option.get arrete_commune])]) (i1,j1) t liste_aretes1 liste_aretes2
																								else f nodes_nouvelle_intersection aretes_intersection (i1,j1) t liste_aretes1 liste_aretes2
	in f [] [] noeud liste_ensemble_noeuds liste_aretes1 liste_aretes2

let ensemble_dag liste_ensemble_noeuds liste_aretes1 liste_aretes2 = 
	let rec f nodes_nouvelle_intersection aretes_intersection liste_ensemble_noeuds1 liste_ensemble_noeuds2 liste_aretes1 liste_aretes2 = match liste_ensemble_noeuds1 with
	                    |[]        -> {nodes = nodes_nouvelle_intersection ; aretes = aretes_intersection}
											|(node1,nod2)::t -> let dag_commune = ensemble_dag2 (node1,nod2) liste_ensemble_noeuds2 liste_aretes1 liste_aretes2 in 
																				 f (nodes_nouvelle_intersection@dag_commune.nodes) (aretes_intersection@dag_commune.aretes) t liste_ensemble_noeuds2 liste_aretes1 liste_aretes2
	in f [] [] liste_ensemble_noeuds liste_ensemble_noeuds liste_aretes1 liste_aretes2

(* TEST *)
let ensemble_dag_test = ensemble_dag liste_ensemble_noeuds dag1.aretes dag2.aretes
(* val ensemble_dag_test : intersection_dag =
  {nodes =
    [("", ""); ("d", "g"); ("", ""); ("d", "gh"); ("", "g"); ("d", "gh");
     ("d", "gh"); ("dx", "ghx"); ("dx", ""); ("dxa", "g"); ("dx", "ghx");
     ("dxa", "ghxe")];
   aretes =
    [(("", ""), ("d", "g"), [Extract (Forward 3, Forward 4)]);
     (("", ""), ("d", "gh"), [Extract (Forward 3, Backward 0)]);
     (("", "g"), ("d", "gh"), [Extract (Backward 1, Backward 0)]);
     (("d", "gh"), ("dx", "ghx"), [Const "x"]);
     (("dx", ""), ("dxa", "g"), [Extract (Backward 2, Backward 1)]);
     (("dx", "ghx"), ("dxa", "ghxe"), [Extract (Forward 0, Forward 1)])]}
*) 

(* **** *)

let rec get_arete_suivante aretes_liste node_fin_i node_fin_j
   = match aretes_liste with
   |((i1,j1), (i2,j2), expression_liste)::t -> 
		  if (i1 = node_fin_i) && (j1 = node_fin_j)
				 then Some ((i1,j1) , (i2,j2) , expression_liste)
			else
					get_arete_suivante t node_fin_i node_fin_j
	 |[] -> None

(* TEST *)
let _ = get_arete_suivante ensemble_dag_test.aretes "d" "gh"
(* Some (("d", "gh"), ("dx", "ghx"), [Const "x"]) *)
let _ = get_arete_suivante ensemble_dag_test.aretes "d" "g"
(* None *)

let rec get_arete_precedente aretes_liste node_debut_i node_debut_j
   = match aretes_liste with
   |((i1,j1), (i2,j2), expression_liste)::t -> 
		  if (i2 = node_debut_i) && (j2 = node_debut_j)
				 then Some ((i1,j1) , (i2,j2) , expression_liste)
			else
					get_arete_precedente t node_debut_i node_debut_j
	 |[] -> None

let _ = get_arete_precedente ensemble_dag_test.aretes "d" "g" 
(* Some (("", ""), ("d", "g"), [Extract (Forward 3, Forward 4)]) *)
let _ = get_arete_precedente ensemble_dag_test.aretes "dx" ""
(* None *)

(* *** *)

(* La fonction vérifie si une arete a la possibilité de remonter à la source
 (ou plus précisément : s'il est possible d'y accéder depuis la source) *)
let verification_source aretes_liste node_debut_i node_debut_j =
	let rec f aretes_liste node_debut_i node_debut_j =
	  let recherche = (get_arete_precedente aretes_liste node_debut_i node_debut_j) in
		match Option.is_some recherche with 
		|false -> false
		|true -> let resultat = Option.get recherche in 
		                          match resultat with ((i1,j1), (i2,j2), expression_liste) ->
															 if (i1 = "") && (j1 = "") 
																	then true
		                          else
                                f aretes_liste i1 j1
  in if (node_debut_i = "") && (node_debut_j = "") then true else f aretes_liste node_debut_i node_debut_j
		
(* TEST *)
let _ = verification_source ensemble_dag_test.aretes "dx" "ghx"
(* true *)
let _ = verification_source ensemble_dag_test.aretes "dx" ""
(* false *)

(* La fonction vérifie si une arete a la possibilité d'atteindre le puit *)
let verification_puits aretes_liste node_fin_i node_fin_j str1 str2 =
	let rec f aretes_liste node_fin_i node_fin_j str1 str2 =
	  let recherche = (get_arete_suivante aretes_liste node_fin_i node_fin_j) in
		match Option.is_some recherche with 
		|false -> false
		|true -> let resultat = Option.get recherche in 
		                          match resultat with ((i1,j1), (i2,j2), expression_liste) ->
															 if (i2 = str1) && (j2 = str2) 
																	then true
		                          else 
                                f aretes_liste i2 j2 str1 str2
  in if (node_fin_i = str1) && (node_fin_j = str2) then true else f aretes_liste node_fin_i node_fin_j str1 str2

(* TEST *)
let _ = verification_puits ensemble_dag_test.aretes "d" "g" "dxa" "ghxe"
(* false *)
let _ = verification_puits ensemble_dag_test.aretes "d" "gh" "dxa" "ghxe"
(* true *)

(* *** *)
type dag_final = {nodes : (string*string) list; aretes: ((string*string) * (string*string) * expression) list }

(* Une fonction qui nettoie le graphe DAG de toutes les arêtes qui ne sont pas connectées
   à la source ou au puits *)
let nettoyage_dag (ensemble_dag : intersection_dag) str1 str2 =
	let rec f nodes_final aretes_final aretes_liste_complete aretes str1 str2 =
		match aretes with 
		| [] ->  {nodes = nodes_final; aretes = aretes_final}
		| ((i1,j1), (i2,j2), expression_liste)::t -> 
			let verification1 = verification_puits aretes_liste_complete i2 j2 str1 str2 in
			let verification2 = verification_source aretes_liste_complete i1 j1 in
			if verification1 && verification2
				then
					begin 
					let recherche1 = List.find_opt (fun (i,j) -> (i = i1) && (j = j1)) nodes_final in 
					let recherche2 = List.find_opt (fun (i,j) -> (i = i2) && (j = j2)) nodes_final in
					
					if Option.is_none recherche1 && Option.is_none recherche2 then
						f (nodes_final@[(i1,j1)]@[(i2,j2)]) (aretes_final@[((i1,j1), (i2,j2), List.hd expression_liste)]) aretes_liste_complete t str1 str2
					else if Option.is_none recherche1 && (not (Option.is_none recherche2)) then
						f (nodes_final@[(i1,j1)]) (aretes_final@[((i1,j1), (i2,j2), List.hd expression_liste)]) aretes_liste_complete t str1 str2
					else if (not (Option.is_none recherche1)) && Option.is_none recherche2 then
						f (nodes_final@[(i2,j2)]) (aretes_final@[((i1,j1), (i2,j2), List.hd expression_liste)]) aretes_liste_complete t str1 str2
					else f nodes_final (aretes_final@[((i1,j1), (i2,j2), List.hd expression_liste)]) aretes_liste_complete t str1 str2
					end
			 else f nodes_final aretes_final aretes_liste_complete t str1 str2
			
	in f [] [] ensemble_dag.aretes ensemble_dag.aretes str1 str2

(* TEST *)
let nettoyage_dag_test = nettoyage_dag ensemble_dag_test "dxa" "ghxe"
(*   {nodes = [("", ""); ("d", "gh"); ("dx", "ghx"); ("dxa", "ghxe")];
   aretes =
    [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0));
     (("d", "gh"), ("dx", "ghx"), Const "x");
     (("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))]}
*)

(* *** *)

(* Extraire le programme du DAG *)
let extraire_programme_dag graphe_dag_final = 
	let rec f aretes_liste programme =
		match aretes_liste with
		| [] -> programme
		| ((i1,j1), (i2,j2), expression)::t -> f t (programme@[expression])
	in f graphe_dag_final.aretes [] 
	
(* TEST *)
let mon_programme = extraire_programme_dag nettoyage_dag_test
(* val mon_programme : expression list = [Extract (Forward 3, Backward 0); Const "x"; Extract (Forward 0, Forward 1)] *)

let resultat_pgm1 = evaluation_program mon_programme "abad"
(* val resultat_pgm1 : string = "dxa" *)
let resultat_pgm2 = evaluation_program mon_programme "efegh" 
(* val resultat_pgm2 : string = "ghxe" *)

(* *** *)

(* Conversion d'un "code" de programme en une liste de chaînes de caractères pouvant être imprimées à l'écran et affichées à l'utilisateur *)
let programme_to_string_liste programme  =
	let rec f programme string_liste = match (programme : expression list) with
	|[] -> string_liste 
	|(Const x):: t -> f t (string_liste@[("Const " ^ x)])
	|Extract (Forward num1, Backward num2)::t ->  f t (string_liste@[("Extract (Forward " ^ (string_of_int num1) ^ ", Backward " ^ (string_of_int num2) ^ ")" )])
	|Extract (Backward num1, Forward num2)::t ->  f t (string_liste@[("Extract (Backward " ^ (string_of_int num1) ^ ", Forward " ^ (string_of_int num2) ^ ")" )])
	|Extract (Forward num1, Forward num2)::t ->  f t (string_liste@[("Extract (Forward " ^ (string_of_int num1) ^ ", Forward " ^ (string_of_int num2) ^ ")" )])
	|Extract (Backward num1, Backward num2)::t ->  f t (string_liste@[("Extract (Forward " ^ (string_of_int num1) ^ ", Forward " ^ (string_of_int num2) ^ ")" )])
	
	in ((f programme ["DEBUT"])@["FIN"])

(* TEST *)	
let string_of_pgm_test = programme_to_string_liste mon_programme
(*   ["DEBUT"; "Extract (Forward 3, Backward 0)"; "Const x";
   "Extract (Forward 0, Forward 1)"; "FIN"] *)

(* *** *)

(* Générateur de programmes *)
let generateur_programme str_in1 str_out1 str_in2 str_out2 = 
	let dag1 = cons_dag str_in1 str_out1 and dag2 = cons_dag str_in2 str_out2 in 
	let noeuds_dag1_2 = ensemble_noeuds dag1.nodes dag2.nodes in
	(*let dag_1_2 = *) ensemble_dag noeuds_dag1_2 dag1.aretes dag2.aretes (* in *)
	(* let  dag_final =*) (* nettoyage_dag dag_1_2 str_out1 str_out2 *) (* in *)
	(*extraire_programme_dag dag_final *)
	
let pgm_exemple = generateur_programme "abad" "dxa" "efegh" "ghxe"

(* *** *)

(* Algorithme de Dijkstra ; Commentaires : Wikipedia - CC BY-SA 3.0 *)
type noeud_dijkstra = ((string * string) * int option)

(* Au départ, on considère que les distances de chaque sommet au sommet de départ
   sont infinies, sauf pour le sommet de départ pour lequel la distance est nulle. *)
let dijkstra_distance_provisoire noeuds =
	let rec f noeuds noeuds_dijkstra = match noeuds with
	                   |[] -> noeuds_dijkstra
										 |(noeud_dag1, noeud_dag2)::t -> if  noeud_dag1 = "" && noeud_dag2 = ""
										                                    then f t (noeuds_dijkstra@[((noeud_dag1, noeud_dag2),Some 0)])
																										 else f t (noeuds_dijkstra@[((noeud_dag1, noeud_dag2),None)])
	in f noeuds []
	
(* TEST *)
let noeuds_dijkstra_test = dijkstra_distance_provisoire [("", ""); ("d", "gh"); ("dx", "ghx"); ("dxa", "ghxe")]
(* val noeuds_dijkstra_test : ((string * string) * int option) list =
   [(("", ""), Some 0); (("d", "gh"), None); (("dx", "ghx"), None); (("dxa", "ghxe"), None)] *)
	
let get_noeuds_voisins (noeud : (string * string) * int option) (aretes : ((string*string) * (string*string) * expression) list ) noeuds =
	let rec f noeud aretes voisins noeuds = 
		match aretes with
	  |[] -> voisins
		|((i1,j1), (i2,j2), expression)::t -> match noeud with ((i,j),distance) ->
			if (i1 = i && j1 = j) then 
				let recherche = List.find_opt (fun ((i,j), distance) -> (i = i2) && (j = j2)) noeuds in
				if Option.is_some recherche then
					f noeud t (voisins@[Option.get recherche]) noeuds
				else 
					f noeud t voisins noeuds
      else f noeud t voisins noeuds

	in f noeud aretes ([]: ((string * string)* int option) list) noeuds
	
(* TEST *)
let voisins_test = get_noeuds_voisins (("", ""), Some 0) [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0)); (("d", "gh"), ("dx", "ghx"), Const "x");(("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))] noeuds_dijkstra_test
(* [(("d", "gh"), None)] *)
let voisins_test2 = get_noeuds_voisins (("d", "gh"), None) [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0)); (("d", "gh"), ("dx", "ghx"), Const "x");(("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))] noeuds_dijkstra_test
(*	[(("dx", "ghx"), None)] *)
let voisins_test3 = get_noeuds_voisins (("dx", "ghx"), None) [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0)); (("d", "gh"), ("dx", "ghx"), Const "x");(("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))] noeuds_dijkstra_test
(*	  [(("dxa", "ghxe"), None)] *)
let voisins_test4 = get_noeuds_voisins (("dxa", "ghxe"), None) [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0)); (("d", "gh"), ("dx", "ghx"), Const "x");(("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))] noeuds_dijkstra_test
(*	[] *)


let rec get_cout_arete_entre_2_noeuds (noeud1 : ((string * string) * (int option))) (noeud2 : ((string * string) * (int option))) (aretes : ((string*string) * (string*string) * expression) list) =
	match aretes with 
	|[] -> None
	|((i1,j1), (i2,j2), expression)::t -> match noeud1,noeud2 with ((a,b), distance1),((c,d), distance2) ->
																				if (i1 = a) && (j1 = b)	&& (i2 = c) && (j2 = d)	then
		                                       match expression with
																					 |Const x -> Some 2
																					 |Extract (pos_expression1, pos_expression2) -> Some 1
																				else get_cout_arete_entre_2_noeuds noeud1 noeud2 t  	
									
(* On met à jour les distances des sommets voisins de celui ajouté *)
let voisins_dernier_ajout_mis_a_jour (aretes : ((string*string) * (string*string) * expression) list ) (dernier_ajout : (string * string) * int option) noeuds =
	let rec f noeuds_voisins aretes noeuds_mis_a_jour dernier_ajout =
		match (noeuds_voisins : ((string * string) * int option) list) with
		|[] -> noeuds_mis_a_jour
		|((i,j), distance)::t -> match dernier_ajout with ((i1,j1), distance1) -> 
			                          let cout = get_cout_arete_entre_2_noeuds ((i1,j1), distance1) ((i,j), distance) aretes in
		                            if (Option.is_some cout) && (Option.is_some distance1)  then 
																	if Option.is_some distance then
																		if (Option.get distance1) + (Option.get cout) < (Option.get distance) then
																			f t aretes (noeuds_mis_a_jour@[((i,j), Some ((Option.get distance1) + (Option.get cout)) )]) dernier_ajout
																		else 	f t aretes noeuds_mis_a_jour dernier_ajout
																	else
																		f t aretes (noeuds_mis_a_jour@[((i,j), Some ((Option.get distance1) + (Option.get cout)) )]) dernier_ajout
																else 	f t aretes noeuds_mis_a_jour dernier_ajout
	in f (get_noeuds_voisins dernier_ajout aretes noeuds) aretes [] dernier_ajout
	
(* TEST *)
let noeuds_mis_a_jour_test = voisins_dernier_ajout_mis_a_jour [(("", ""), ("d", "gh"), Extract (Forward 3, Backward 0)); (("d", "gh"), ("dx", "ghx"), Const "x");(("dx", "ghx"), ("dxa", "ghxe"), Extract (Forward 0, Forward 1))] (("", ""), Some 0) noeuds_dijkstra_test
(* val noeuds_mis_a_jour_test : ((string * string) * int option) list = [(("d", "gh"), Some 1)] *)
	
let distances_maj noeud_visites noeuds_dijkstra noeuds_maj =
	let rec f noeud_visites noeuds_dijkstra noeuds_maj noeud_visites_maj noeuds_dijkstra_maj =
		match noeud_visites,noeuds_dijkstra with
		|[],[] -> (noeud_visites_maj, noeuds_dijkstra_maj)
		|((i1,j1), distance1)::t1 , ((i2,j2), distance2)::t2 -> 
			let verification1 = List.find_opt (fun ((i,j), distance) -> (i1 = i) && (j1 = j)) noeuds_maj
			and verification2 = List.find_opt (fun ((i,j), distance) -> (i2 = i) && (j2 = j)) noeuds_maj in
			if Option.is_some verification1 then f t1 t2 noeuds_maj (noeud_visites_maj@[Option.get verification1]) (noeuds_dijkstra_maj@[((i2,j2), distance2)])
			else if Option.is_some verification2 then f t1 t2 noeuds_maj (noeud_visites_maj@[((i1,j1), distance1)]) (noeuds_dijkstra_maj@[Option.get verification2])
		  else f t1 t2 noeuds_maj (noeud_visites_maj@[((i1,j1), distance1)]) (noeuds_dijkstra_maj@[((i2,j2), distance2)])
		|((i1,j1), distance1)::t1 , [] -> 
			let verification1 = List.find_opt (fun ((i,j), distance) -> (i1 = i) && (j1 = j)) noeuds_maj in
			if Option.is_some verification1 then f t1 [] noeuds_maj (noeud_visites_maj@[Option.get verification1]) noeuds_dijkstra_maj
		  else f t1 [] noeuds_maj (noeud_visites_maj@[((i1,j1), distance1)]) noeuds_dijkstra_maj
		|[] , ((i2,j2), distance2)::t2 -> 
			let verification2 = List.find_opt (fun ((i,j), distance) -> (i2 = i) && (j2 = j)) noeuds_maj in
      if Option.is_some verification2 then f [] t2 noeuds_maj noeud_visites_maj (noeuds_dijkstra_maj@[Option.get verification2])
		  else f [] t2 noeuds_maj noeud_visites_maj (noeuds_dijkstra_maj@[((i2,j2), distance2)])

	in f noeud_visites noeuds_dijkstra noeuds_maj [] []
	
(* TEST *)
let test_distances_maj = distances_maj [(("", ""), Some 0)] [(("d", "gh"), None); (("dx", "ghx"), None); (("dxa", "ghxe"), None)] noeuds_mis_a_jour_test
(* val test_distances_maj :
  ((string * string) * int option) list *
  ((string * string) * int option) list =
  ([(("", ""), Some 0)],
   [(("d", "gh"), Some 1); (("dx", "ghx"), None); (("dxa", "ghxe"), None)]) *)


let get_min_parmis_non_visites noeuds_dijkstra =
	let rec f min min_index index noeuds_dijkstra =
		match noeuds_dijkstra with
		|((i,j), distance)::t -> if  Option.is_some distance then
																				if Option.is_some min then
			                        						if ((Option.get distance) < (Option.get min)) then
			                            					f distance index (index + 1) t
																					else
																						f min min_index (index + 1) t
																				else
																					f distance index (index + 1) t
															       else
																        f min min_index (index + 1) t
	   |[] -> min_index
																
	in match noeuds_dijkstra with
	| [] -> -1
	| ((i,j), distance)::t -> f distance 0 1 (List.tl noeuds_dijkstra) 
	
	
let retirer_nouveau_ajout ((i1,j1), distance1) noeuds_liste =
	let rec f ((i1,j1), distance1) noeuds_liste nouvelle_noeuds_liste =
		match noeuds_liste with
		|[] -> nouvelle_noeuds_liste
		|((i2,j2), distance2)::t -> if (i1 = j1) && (i2 = j2) && (distance1 = distance2) then
			                             f ((i1,j1), distance1) t nouvelle_noeuds_liste
																else
			                             f ((i1,j1), distance1) t (nouvelle_noeuds_liste@[((i2,j2), distance2)])
  in f ((i1,j1), distance1) noeuds_liste []

let dijkstra dag_final = 
	let rec f noeud_visites noeuds_dijkstra noeuds_dag aretes dernier_ajout = 
		match ((List.length noeuds_dijkstra) = 0) with (* Tant que noeuds_visite ne contient pas tous les noeuds *)
		|true -> noeuds_dag
		|false -> let noeuds_maj = voisins_dernier_ajout_mis_a_jour aretes dernier_ajout (noeud_visites@noeuds_dijkstra) in
		          let maj = distances_maj noeud_visites noeuds_dijkstra noeuds_maj and
							nouveau_ajout = List.nth_opt noeuds_dijkstra (get_min_parmis_non_visites noeuds_dijkstra) in
							if Option.is_some nouveau_ajout then
								f (fst maj@[Option.get nouveau_ajout]) (retirer_nouveau_ajout (Option.get nouveau_ajout) (snd maj)) (noeuds_dag@[Option.get nouveau_ajout]) aretes (Option.get nouveau_ajout)
							else 
								f (fst maj) [] noeuds_dag aretes dernier_ajout

	in f [(("", ""),Some 0)] (retirer_nouveau_ajout (("", ""),Some 0) (dijkstra_distance_provisoire dag_final.nodes)) [] dag_final.aretes (("", ""),Some 0)
	
(* TEST *)
(* let dijkstra_test = dijkstra nettoyage_dag_test *)