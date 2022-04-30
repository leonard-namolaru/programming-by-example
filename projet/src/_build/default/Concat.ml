
let const str = str;;
let forward i = i;;
let backward i str = (String.length str) - i;;
let extract str pos_initial pos_finale = String.sub str pos_initial (pos_finale  - pos_initial);;

let x = extract "aymen" 1 3 ;;
print_string x ;; 
