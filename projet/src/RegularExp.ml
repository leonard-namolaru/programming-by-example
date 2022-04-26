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

(* TEST : Traduction d'un algo prog dynamique "Rendre la monnaie" en Ocaml *)
type entier = |Entier of int |Infinity

let get_entier entier = match entier with |Entier x -> x |Infinity -> Int.max_int 
let set_entier entier = Entier entier

let rendre_la_monnaie (somme:int) (nb_type_de_pieces:int) (valeurs_de_pieces:int list) =
  (* matrice[i, 0] = 0 ∀ i *) (* Faire la somme 0 avec les pièces de valeurs v1 ... vi ? il me faut combien de pièces ? 0 *)
  let rec cas_simple_1 index nb_type_de_pieces matrice  = match (index = (nb_type_de_pieces + 1))  with 
                                           |true -> matrice
                                           |false -> cas_simple_1 (index + 1) nb_type_de_pieces (matrice@[[set_entier 0]])

  (* matrice[0, s] = {0 si s = 0 , ∞ sinon *) (* Si je n’ai pas de pièces et on me demande de faire la somme S : je ne peux pas, (sauf si la somme est 0). *)
  in let rec cas_simple_2 s somme matrice = match (s = somme), matrice  with 
                        |false,(h::t) -> cas_simple_2 (s + 1) somme ([(h@[Infinity])]@t) 
                        |_,_-> matrice

  in let rec cas_general s i nb_type_de_pieces somme matrice valeurs_de_pieces = match (i = (nb_type_de_pieces + 1))  with
    |true -> matrice
    |false -> match (s = (somme + 1) ) with
              |true -> cas_general 1 (i + 1) nb_type_de_pieces somme matrice valeurs_de_pieces
              |false -> 
                let valeur_piece_i = List.nth valeurs_de_pieces (i-1) in 
                  if (s >= valeur_piece_i) then 
                    begin
                    let min_option1 = get_entier (list_list_get matrice (i-1) s) in 
                    let min_option2 = 1 + get_entier (list_list_get matrice i (s - valeur_piece_i)) in
                    let minimum = set_entier (min min_option1 min_option2) in
                    cas_general (s+1) i nb_type_de_pieces somme (list_list_ajout_element_position_ligne_colonne matrice minimum i s) valeurs_de_pieces
                    end
                  else 
                    cas_general (s+1) i nb_type_de_pieces somme (list_list_ajout_element_position_ligne_colonne matrice (list_list_get matrice (i-1) s) i s) valeurs_de_pieces

  in cas_general 1 1 nb_type_de_pieces somme (cas_simple_2 0 somme (cas_simple_1 0 nb_type_de_pieces [])) valeurs_de_pieces

let _ = rendre_la_monnaie 8 3 [1;4;6]


(* ---------------------------------------------------------------------------- *)

(* Connaitre l’ensemble des expressions régulières qui filtrent str *) 
(* Calcule de cette information de façon ascendante (bottom-up) en s’inspirant de la programmation dynamique *)

type reg = |Tokens of (token list) |Infinity |None

let get_reg_len reg = match reg with |Tokens token_list -> List.length token_list
                                     |Infinity -> Int.max_int 
                                     |None -> 0 

let concat_reg reg1 reg2 = match reg1, reg2 with
                     |Tokens token_list1,_ -> Tokens (token_list1@[])
                     |_ -> None

let ensemble_expressions_regulieres_filtrent_str (str:string) (nb_type_de_tokens:int) (tokens:token list) =
  (* matrice[i, 0] = 0 ∀ i *) (* Une expression régulière qui filtre les 0 premiers char de str ? il me faut combien de tokens ? 0, donc une liste vide *)
  let rec cas_simple_1 index nb_type_de_tokens matrice  = match (index = (nb_type_de_tokens + 1))  with 
                                           |true -> matrice
                                           |false -> cas_simple_1 (index + 1) nb_type_de_tokens (matrice@[[None]])

  (* matrice[0, s] = {0 si s = 0 , ∞ sinon *) 
  (* Si je n’ai pas de tokens et on me demande de construire l’expression régulière qui filtre les s premiers char de str ? : je ne peux pas, (sauf si s est 0). *)
  in let rec cas_simple_2 s longeur_str matrice = match (s = longeur_str), matrice  with 
                        |false,(h::t) -> cas_simple_2 (s + 1) longeur_str ([(h@[Infinity])]@t) 
                        |_,_-> matrice

  in let rec cas_general s i nb_type_de_tokens longeur_str matrice tokens = match (i = (nb_type_de_tokens + 1))  with
    |true -> matrice
    |false -> match (s = (longeur_str + 1) ) with
              |true -> cas_general 1 (i + 1) nb_type_de_tokens longeur_str matrice tokens
              |false -> 
                let filtrage = filtrage_mot_par_token (String.sub str 0 longeur_str) (List.nth tokens (i-1)) in 
                  if filtrage then 
                    begin
                    let min_option1 = get_reg_len (list_list_get matrice (i-1) s) in 
                    let min_option2 = 1 + get_reg_len (list_list_get matrice i (s -1)) in
                    if min_option2 != min_option1
                      then cas_general (s+1) i nb_type_de_tokens longeur_str (list_list_ajout_element_position_ligne_colonne matrice (concat_reg (Tokens [(List.nth tokens (i-1))]) (list_list_get matrice i (s -1)) ) i s) tokens
                    else
                      cas_general (s+1) i nb_type_de_tokens longeur_str (list_list_ajout_element_position_ligne_colonne matrice (list_list_get matrice (i-1) s) i s) tokens
                    end
                  else 
                    cas_general (s+1) i nb_type_de_tokens longeur_str (list_list_ajout_element_position_ligne_colonne matrice (list_list_get matrice (i-1) s) i s) tokens

  in cas_general 1 1 nb_type_de_tokens (String.length str) (cas_simple_2 0 (String.length str) (cas_simple_1 0 nb_type_de_tokens [])) tokens

let _ = ensemble_expressions_regulieres_filtrent_str "stlen" 4 [Plus Lower; Plus Upper; Plus Alphanumeric ; Plus Alpha]



