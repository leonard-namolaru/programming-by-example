
let const str = str;;
let forward i = i;;
let backward i str = (String.length str) - i;;
let extract str pos_initial pos_finale = String.sub str pos_initial (pos_finale  - pos_initial);;

type operation = Const of string | Forward of int | Backward of int | Extract of int * int                                                                               
type dag = {nodes : string list; aretes: (string * string * operation) list };;
let exemple = {nodes = ["";"d";"dx";"dxa"]; aretes =[("","d",Const "d");("d","dx",Const "x");("dx","dxa",Const "a")]};;

("a","b",Const "c")::exemple.aretes;;


let ajouter_arete graph arete = {nodes = graph.nodes;aretes=arete::graph.aretes};;
(*let exemple = ajouter_arete exemple ("a","b",Const  "a" );;*)
(*let exemple = ajouter_arete exemple ("lenny","lenny",Extract (1,2))*)
