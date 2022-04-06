(* Le programme doit accepter les modes suivants de fonctionnement :
 * genconcat <fichier> : doit afficher le programme Concat produit à partir du contenu de <fichier>.
 * genconcat <fichier> <fichier> : doit créer un programme Concat à partir du contenu du premier fichier
 *                                 (contenant des lignes "input output") puis utiliser ce programme sur
 *                                 le second fichier qui ne contiendra que des lignes "input"
 *
 *)

type mode_de_fonctionnement = | Fichier_input_output | Deux_fichiers

(* Nombre de paramètres passés via la ligne de commande *)
let argc = Array.length Sys.argv

(* Verification si le nombre de paramètres passés via la ligne de commande est correct *)
let _ =  if ((argc < 2) || (argc > 3)) then
            begin
               Printf.printf "Utilisation : %s <fichier> [<fichier>] \n%!" Sys.argv.(0) ;
               Printf.printf "par exemple : %s ../exemples/etape1/date_to_month \n%!" Sys.argv.(0) ;
               exit 0;
            end
         else ()


(* Verification si les chemins qui nous ont été passés représentent bien un fichier *)
let rec verification_chemins liste_chemins = match liste_chemins with
                                                 |[] -> true
                                                 |h::t -> if Sys.file_exists h then
                                                             if not (Sys.is_directory h) then
                                                                verification_chemins t
                                                             else
                                                                false
                                                          else
                                                            false

(* On n'a pas besoin du premier élément de la liste car ce n'est pas un des paramètres 
que l'utilisateur a choisi de transférer au logiciel mais le nom du fichier exécutable *)
let liste_fichiers = List.tl (Array.to_list Sys.argv)

let _ = if not (verification_chemins liste_fichiers) then 
            begin
               print_endline "Le système a détecté qu'un chemin qui lui est transféré ne mène pas à un fichier valide.";
               exit 0; 
            end
        else ()

let input_file_to_list fichier = 
   let rec f liste input_channel = match input_line input_channel with
                                                | next_line -> f (liste@[next_line]) input_channel
                                                | exception End_of_file -> close_in input_channel ; liste
   in f [] (open_in fichier)

let input_output_file_to_lists fichier = 
   let rec f input_list output_list input_channel = match input_line input_channel with
                                                | next_line -> let tmp_list = String.split_on_char '\t' next_line in
                                                                  f (input_list@[List.nth tmp_list 0]) (output_list@[List.nth tmp_list 1]) input_channel
                                                | exception End_of_file -> close_in input_channel ; (input_list, output_list)
in f [] [] (open_in fichier)

let input_output_lists = input_output_file_to_lists (List.nth liste_fichiers 0)
let input_list = fst input_output_lists 
let output_list = snd input_output_lists 

let _ = print_endline (List.nth input_list 0) ; print_endline (List.nth output_list 0)
let _ = print_endline (List.nth input_list 1) ; print_endline (List.nth output_list 1)


let mode = if argc = 2 then Fichier_input_output else Deux_fichiers
let input_list2 = match mode with
                   |Deux_fichiers -> input_file_to_list (List.nth liste_fichiers 1)
                   |_ -> []