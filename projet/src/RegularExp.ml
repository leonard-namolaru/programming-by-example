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
let test_mot = "ocaml"
let test_token = Plus Lower
let test_result = filtrage_mot_par_token test_mot test_token
let _ = if test_result then print_string "OK" else print_string "PBM"

(* La partie maximale d'un mot qui est filtrée par un token d'une expression régulière *)
let rec partie_max_mot_filtrage_par_token mot regexp_token = 
    match (String.length mot) with
    |0 -> mot (* "" *)
    |_ -> if (not (filtrage_mot_par_token mot regexp_token))
          then partie_max_mot_filtrage_par_token (String.sub mot 0 ((String.length mot) - 1)) regexp_token
          else mot

(* TEST *)
let test_mot = "ocaML"
let test_token = Plus Lower
let _ = print_string (partie_max_mot_filtrage_par_token test_mot test_token)

  




(*
let rec filtrage_mot_par_expression_reguliere mot:string expression:regexp = match expression with 
                |[] -> failwith "Une expression reguliere est une sequence non-vide de tokens"
                (* Une expression reguliere consistuee d’un unique token t filtre un mot m lorsque t filtre m *)
                |regexp_token::[] -> filtrage_mot_par_token mot regexp_token
                |regexp_token::fin_regexp -> 
*)
