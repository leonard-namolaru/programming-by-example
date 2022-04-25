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


