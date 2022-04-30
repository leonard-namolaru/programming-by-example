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

(* test *)
let x= [Const "s";Const "s"] 
let _ = evaluation_program x "aymen"
let y = [Extract (Forward 1,Forward 2);Extract (Forward 2,Forward 4)] 
let _ =evaluation_program y "aymen"

(* TEST *)
let _ = evaluation_expression (Const "str") "str"
let _ = evaluation_expression (Extract (Forward 1,Forward 2)) "str" (* resultat : t *)



type fb = Forward of int | Backward of int;;
type operation = Const of string | Extract of int * int | Exctact of (fb * fb)                                                                                
type dag = {nodes : string list; aretes: (string * string * operation) list }
let exemple = {nodes = ["";"d";"dx";"dxa"]; aretes =[("","d",Const "d");("d","dx",Const "x");("dx","dxa",Const "a")]}

let _ = ("a","b",Const "c")::exemple.aretes


let ajouter_arete graph arete = {nodes = graph.nodes;aretes=arete::graph.aretes};;
(*let exemple = ajouter_arete exemple ("a","b",Const  "a" );;*)
(*let exemple = ajouter_arete exemple ("lenny","lenny",Extract (1,2))*)
