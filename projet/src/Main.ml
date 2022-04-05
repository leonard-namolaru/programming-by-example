(* Le programme doit accepter les modes suivants de fonctionnement :
 * genconcat <fichier> : doit afficher le programme Concat produit à partir du contenu de <fichier>.
 * genconcat <fichier> <fichier> : doit créer un programme Concat à partir du contenu du premier fichier
 *                                 (contenant des lignes "input output") puis utiliser ce programme sur
 *                                 le second fichier qui ne contiendra que des lignes "input"
 *
 *)

(* Test : Imprimer tous les paramètres passés via la ligne de commande *)
let _ = print_string "Array.length : " ; print_int (Array.length Sys.argv) ; print_newline ()
let _ = Array.iter (fun x -> print_string x ; print_newline () ) Sys.argv




