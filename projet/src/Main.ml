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

(* Vérifier si le nombre de paramètres passés via la ligne de commande est correct *)
let _ = let argc = Array.length Sys.argv in
               if ((argc < 2) || (argc > 3)) then
                  begin
                  print_string "Exemple d'utilisation : ./Main.exe ../exemples/etape1/date_to_month [../exemples/etape1/date_verbose]";
                  print_newline ();
                  exit 0;
                  end
               else
                   ()

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

let file_to_list fichier = let rec f liste input_channel = match input_line input_channel with
                                                | next_line -> f (liste@[next_line]) input_channel
                                                | exception End_of_file -> close_in input_channel ; liste
in f [] (open_in fichier)



let my_liste = file_to_list (List.nth (List.tl argv_list) 0)
let _ = print_string (List.nth my_liste 0) ; print_newline ()
