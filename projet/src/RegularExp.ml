(* **** Extension de la syntaxe abstraite **** *)

(* Chaque class decrit une classe de caracteres *)
type classe = | Alphanumeric      (* les alpha-numeriques      *)
              | Numeric           (* les chiffres              *)
              | Alpha             (* les lettres de l’alphabet *)
              | Lower             (* les lettres minuscules    *)
              | Upper             (* les lettres majuscules    *)
              | Special of char   (* caracteres speciaux       *)

             (* Le token plus(classe) filtre un mot m quand m est non-vide, et tous les caracteres de m sont dans classe *)
type token = | Plus of classe 
             (* Le token neg(classe) filtre un mot m quand m est non-vide, et aucun caractere de m n’est dans classe. *)
             | Neg of classe

type regexp = token list (* Une expression reguliere est une sequence non-vide de tokens. *)

type before_after = | Before of regexp | After of regexp | BeforeLast of regexp | AfterLast of regexp

let filtrage_mot_par_token mot regexp_token = 
  let f mot nom_classe =
      match nom_classe with 
      | Alphanumeric (* les alpha-numeriques *) -> String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) mot
      | Numeric (* les chiffres *) -> String.for_all (fun c -> (c >= '0' && c <= '9')) mot
      | Alpha   (* les lettres de l’alphabet *) -> String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) mot
      | Lower   (* les lettres minuscules *) -> String.for_all (fun c ->  (c >= 'a' && c <= 'z')) mot
      | Upper   (* les lettres majuscules *) -> String.for_all (fun c ->  (c >= 'A' && c <= 'Z')) mot
      | Special caractere (* caracteres speciaux *) -> String.for_all (fun c ->  (c = caractere)) mot

  in match regexp_token with |Plus nom_classe -> f mot nom_classe
                             | Neg nom_classe -> not (f mot nom_classe)

(* TEST *)
let _ = print_endline (Bool.to_string (filtrage_mot_par_token "ocaml" (Plus Lower)))
let _ = print_endline (Bool.to_string (filtrage_mot_par_token "." (Plus (Special '.'))))
let _ = print_endline (Bool.to_string (filtrage_mot_par_token "." (Plus Alphanumeric)))


(* La partie maximale d'un mot qui est filtrée par un token d'une expression régulière *)
let rec partie_max_mot_filtrage_par_token mot regexp_token = 
    match (String.length mot) with
    |0 -> mot (* "" *)
    |_ -> if (not (filtrage_mot_par_token mot regexp_token))
          then partie_max_mot_filtrage_par_token (String.sub mot 0 ((String.length mot) - 1)) regexp_token
          else mot

(* TEST *)
let _ = print_endline ((partie_max_mot_filtrage_par_token "ocaML" (Plus Lower)))
let _ = print_endline ((partie_max_mot_filtrage_par_token "OCaml" (Plus Lower)))
let _ = print_endline ((partie_max_mot_filtrage_par_token "ocAml" (Plus Lower)))
let _ = print_endline ((partie_max_mot_filtrage_par_token "" (Plus Lower)))

(* Filtrage d’un mot par une expression reguliere *)
let rec filtrage_mot_par_expression_reguliere (mot:string) (expression:regexp) = match expression with 
                |[] -> false
                (* Une expression reguliere consistuee d’un unique token t filtre un mot m lorsque t filtre m *)
                |regexp_token::[] -> filtrage_mot_par_token mot regexp_token
                |regexp_token::fin_regexp -> 
                  let m1 = partie_max_mot_filtrage_par_token mot regexp_token 
                    in let longueur_m2 = ((String.length mot) - (String.length m1)) in
                      if (String.length m1) != 0
                        then filtrage_mot_par_expression_reguliere (String.sub mot ((String.length m1)) longueur_m2) fin_regexp
                      else
                        false

(* TEST *)
let _ = print_endline (Bool.to_string (filtrage_mot_par_expression_reguliere "ocaML33" [Plus Lower ; Plus Upper ; Plus Numeric]))
let _ = print_endline (Bool.to_string (filtrage_mot_par_expression_reguliere "ML33" [Plus Lower ; Plus Upper ; Plus Numeric]))
let _ = print_endline (Bool.to_string (filtrage_mot_par_expression_reguliere "" []))

(* le premier facteur de str qui est filtré par une expression reguliere *)
let before (str:string) (expression:regexp) = 
  let rec f index str expression =
    match (String.length str) with
      |0 -> -1
      |_ -> if (not (filtrage_mot_par_expression_reguliere str expression))
            then f (index + 1) (String.sub str 1 ((String.length str) - 1)) expression
            else index
          in f 0 str expression

let after (str:string) (expression:regexp) = 
  let rec f index str expression =
    match (String.length str) with
      |0 -> -1
      |_ -> if (not (filtrage_mot_par_expression_reguliere str expression))
            then f (index - 1) (String.sub str 0 ((String.length str) - 1)) expression
            else index + 1
          in let before_result = before str expression in
              match before_result with
                |(-1) -> -1
                |_ -> let before_str = String.sub str before_result ((String.length str) - before_result) in
                          (f ((String.length before_str) - 1) before_str expression) + before_result

(* TEST *)
let test_expression = [Plus Lower ; Plus Upper ; Plus Numeric]

let test_mot = "ocaML33t"
let _ = print_int (before test_mot test_expression) ; print_char ',' ; print_int (after test_mot test_expression)
let _ = print_newline ()

let test_mot = "ocaML33"
let _ = print_int (before test_mot test_expression) ; print_char ',' ; print_int (after test_mot test_expression)
let _ = print_newline ()


let test_mot = "+ocaML33"
let _ = print_int (before test_mot test_expression) ; print_char ',' ; print_int (after test_mot test_expression)
let _ = print_newline ()

(* le dernier facteur de str qui est filtré par une expression reguliere *)
let afterlast (str:string) (expression:regexp) = 
  after str expression

let beforelast (str:string) (expression:regexp) = 
  let rec f index str expression =
    match (String.length str) with
      |0 -> index - 1
      |_ -> if (filtrage_mot_par_expression_reguliere str expression)
            then f (index + 1) (String.sub str 1 ((String.length str) - 1)) expression
            else index - 1
          in let before_result = before str expression in
          match before_result with
            |(-1) -> -1
            |_ -> let before_str = String.sub str before_result ((String.length str) - before_result) in
              f before_result before_str expression



(* TEST *)
let test_expression = [Plus Lower ; Plus Upper ; Plus Numeric]

let test_mot = "ocaML33t"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

let test_mot = "ocaML33"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

let test_mot = "+ocaML33"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

let test_expression = [Plus Lower]

let test_mot = "ocaM"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

let test_mot = "oca"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

let test_mot = "+oca"
let _ = print_int (beforelast test_mot test_expression) ; print_char ',' ; print_int (afterlast test_mot test_expression)
let _ = print_newline ()

(* Connaitre l’ensemble des expressions régulières qui filtrent str *) 
(* Calcule de cette information de façon ascendante (bottom-up) en s’inspirant de la programmation dynamique *)

(* Fonction auxiliaire : Insère un élément à la position i dans une liste *)
let list_ajout_element_position_i liste element i = 
  let rec f liste element i counter = match liste with
                                            |[] -> 
                                              if (i == counter) 
                                                then [element] 
                                              else []
                                            |head::tail ->
                                              if (i == counter) 
                                                then (element::liste)
                                              else head::(f tail element i (counter + 1))
  in f liste element i 0

(* Fonction auxiliaire : Lorsqu'il s'agit d'une liste de listes (liste à deux dimensions), 
la fonction nous permet d'ajouter un élément spécifique dans la ligne et la colonne de notre choix *)
let list_list_ajout_element_position_ligne_colonne liste element ligne colonne =
  let rec f liste element ligne colonne counter_ligne = match liste with 
                                                     |[] -> if(ligne = counter_ligne)
                                                               then if(colonne = 0)
                                                                      then [[element]]
                                                                    else []
                                                            else []

                                                    |head::tail -> if (ligne == counter_ligne)
                                                                    then (list_ajout_element_position_i head element colonne)::tail
                                                                  else head::(f tail element ligne colonne (counter_ligne + 1))
  in f liste element ligne colonne 0

(* Fonction auxiliaire : Pour une liste de listes (liste à deux dimensions), 
la fonction permet d'obtenir un élément précis par numéro de ligne et numéro de colonne *)
let list_list_get liste ligne colonne = ((List.nth (List.nth liste (ligne)) colonne))

(* Test *)
let _ = list_list_ajout_element_position_ligne_colonne [[1;3];[4;5;6]] 2 0 1
let _ = list_ajout_element_position_i [Infinity;Infinity] (Entier 3) 1 
let _ = list_list_get (list_list_ajout_element_position_ligne_colonne [[1;3];[4;5;6]] 2 0 1) 0 1

(* ---------------------------------------------------------------------------- *)

(* Connaitre l’ensemble des expressions régulières qui filtrent str *) 
(* Calcule de cette information de façon ascendante (bottom-up) en s’inspirant de la programmation dynamique *)

let ensemble_expressions_regulieres_filtrent_str (str:string) (tokens:token list) =
  let rec construction_matrice index_str index_token nb_type_de_tokens longeur_str matrice tokens = match (index_token = nb_type_de_tokens)  with
    |true -> matrice
    |false -> match (index_str = longeur_str) with
              |true -> construction_matrice 0 (index_token + 1) nb_type_de_tokens longeur_str matrice tokens
              |false -> 
                let filtrage_resultat = 
                  if (filtrage_mot_par_token (String.sub str index_str 1) (List.nth tokens index_token)) 
                    then Option.some (List.nth tokens index_token)
                  else Option.none
                in construction_matrice (index_str + 1) index_token nb_type_de_tokens longeur_str (list_list_ajout_element_position_ligne_colonne matrice filtrage_resultat index_token index_str) tokens   
  
  
  construction_matrice 0 0 (List.length tokens) (String.length str) [] tokens 

let _ = ensemble_expressions_regulieres_filtrent_str "ab8" [Plus Lower; Plus Alpha; Plus Alphanumeric ; Plus Numeric]


(*
   
  in let rec construction_resultat index_str index_token nb_type_de_tokens longeur_str matrice tokens ensemble_expressions = match (index_token = nb_type_de_tokens)  with
  |true -> ensemble_expressions
  |false -> match (index_str = longeur_str) with
            |true -> construction_resultat 0 (index_token + 1) nb_type_de_tokens longeur_str matrice tokens
            |false -> 
              let element = list_list_get liste ligne colonne
                if (filtrage_mot_par_token (String.sub str index_str 1) (List.nth tokens index_token)) 
                  then Option.some (List.nth tokens index_token)
                else Option.none
              in construction_resultat (index_str + 1) index_token nb_type_de_tokens longeur_str (list_list_ajout_element_position_ligne_colonne matrice filtrage_resultat index_token index_str) tokens 

*)