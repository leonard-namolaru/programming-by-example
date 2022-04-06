(* Le programme doit accepter les modes suivants de fonctionnement :
 * genconcat <fichier> : doit afficher le programme Concat produit à partir du contenu de <fichier>.
 * genconcat <fichier> <fichier> : doit créer un programme Concat à partir du contenu du premier fichier
 *                                 (contenant des lignes "input output") puis utiliser ce programme sur
 *                                 le second fichier qui ne contiendra que des lignes "input"
 *)

type mode_de_fonctionnement = | Fichier_input_output | Deux_fichiers

(* Nombre de paramètres passés via la ligne de commande *)
let argc = Array.length Sys.argv

(* Le programme a 2 modes de fonctionnement *)
let mode = if argc = 2 then Fichier_input_output else Deux_fichiers

(* Verification si le nombre de paramètres passés via la ligne de commande est correct *)
let _ =  if ((argc < 2) || (argc > 3)) then
            begin
               Printf.printf "Utilisation : %s <fichier> [<fichier>] \n%!" Sys.argv.(0) ;
               Printf.printf "par exemple : %s ../exemples/etape1/date_to_month [./'Exemples pour le second fichier'/identity] \n%!" Sys.argv.(0) ;
               exit 0;
            end
         else ()

(* ============================================================================================ *)
(* ***** VERIFICATION SI LES CHEMINS QUI NOUS ONT ÉTÉ PASSÉS REPRÉSENTENT BIEN UN FICHIER ***** *)
(* ============================================================================================ *)

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

(* ============================================================================================================ *)
(* ***** TRANSFORMER LES LISTES D'ENTRÉES (INPUTS) ET DE SORTIES (OUTPUTS) EN LISTES DE TYPE STRING LIST *****  *)
(* ============================================================================================================ *)
        
(* Une fonction qui reçoit en paramètre un fichier contenant une liste de inputs et renvoie une liste. Chaque cellule est l'une des entrées (inputs) *)       
let input_file_to_list fichier = 
   let rec f liste input_channel = match input_line input_channel with
                                                | next_line -> f (liste@[String.trim next_line]) input_channel
                                                | exception End_of_file -> close_in input_channel ; liste
   in f [] (open_in fichier)

(* Une fonction qui reçoit en paramètre un fichier contenant une liste de inputs et une liste de outputs
et renvoie un couple de listes : (input_list, output_list) *)       
let input_output_file_to_lists fichier = 
   let rec f input_list output_list input_channel = match input_line input_channel with
                                                | next_line -> let tmp_list = String.split_on_char '\t' (String.trim next_line) in
                                                                  f (input_list@[List.nth tmp_list 0]) (output_list@[List.nth tmp_list 1]) input_channel
                                                | exception End_of_file -> close_in input_channel ; (input_list, output_list)
in f [] [] (open_in fichier)

let input_output_lists = input_output_file_to_lists (List.nth liste_fichiers 0)
let input_list = fst input_output_lists 
let output_list = snd input_output_lists 

let input_list2 = match mode with
                   |Deux_fichiers -> input_file_to_list (List.nth liste_fichiers 1)
                   |_ -> []

(* ===================================================== *)
(* ***** IMPRIMER LE CONTENU DE TOUTES LES LISTES *****  *)
(* ===================================================== *)

(* Une fonction qui imprime le contenu d'une liste *)
let rec print_list liste = match liste with
                                |[] -> print_newline ()
                                |h::t -> print_endline h; print_list t


let _ = print_endline "Fichier 1 , inputs :" ; print_list input_list
let _ = print_endline "Fichier 1 , outputs :"; print_list output_list                 
let _ = if (List.length input_list2 > 0) then 
         begin
            print_endline "Fichier 2 , inputs :" ; 
            print_list input_list2
         end
        else ()
