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

(* Vérifier que les chemins qui nous ont été passés représentent bien un fichier *)
let rec verification_chemins chemins_liste = match chemins_liste with
                                                 |[] -> true
                                                 |h::t -> if Sys.file_exists h then
                                                             if not (Sys.is_directory h) then
                                                                verification_chemins t
                                                             else
                                                                false
                                                          else
                                                                false
let argv_list = Array.to_list Sys.argv
let _ = if (verification_chemins (List.tl argv_list)) then print_string "paths ok !" else print_string "paths pbm !" ; print_newline ()



