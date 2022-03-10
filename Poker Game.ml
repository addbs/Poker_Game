(* ###################################### Types ###################################### *)

(* On définit les types nécessaires au bon fonctionnement du jeu *)
type carte = { mutable couleur : char; mutable valeur : int};;
type joueur  = { mutable clef : char; mutable etat : int; mutable main : carte array; mutable argent : int; mutable mise : int};;
type blind = { mutable big : int; mutable small : int; mutable montant : int};;
type jeu = { mutable joueurs : joueur array; mutable pack : carte array; mutable c_posees : carte array; mutable c_defaussees : carte array; mutable mise_table : int; mutable mise_tour : int; mutable  blinds : blind};; 
type action = { mutable fold : bool; mutable check : bool; mutable call : bool; mutable raise : bool; mutable all_in : bool; mutable amount : int} (* faire un type avec suivre, surenchere etc .... *);;
(* ce dernier type est utilisé lorsqu'un joueur veut surenchérir et a été nécessaire pour pouvoir permettre la fenêtre de mise *)
type action_raise = { mutable a_in : bool; mutable double : bool; mutable pot : bool; mutable half : bool; mutable cancel : bool};;


(* ###################################### Bric à Brac ###################################### *)
(* Dans cette partie on va définir des fonctions lambdas utiles pour le reste dans des focntions beaucoup plus complexes *)

(* Sys est nécessaire pour gérer le temps dans les animations *)
open Sys;;

let carte_vide () = {couleur = ' '; valeur = 0};;
let main_vide () =   Array.make 2 (carte_vide ());;
let tas_vide taille = Array.make taille (carte_vide ());;

let attente duree = (* une fonction pour attendre un temps x en secondes *)
	let compteur = ref 0 in
	let temps = time () in 
	while (time () -. temps) < duree do
	compteur := !compteur + 1 
	(* on attend *) done;;

let concatene_array a1 a2 = (* Une fonction qui prend en argument deux array et qui renvoie un array avec les éléments des deux arrays (l'un puis l'autre)  *)
	let x = a1.(0) in
	let d1 = Array.length a1 in 
	let d2 = Array.length a2 in
	let a = Array.make (d1 + d2) x in 
	for i=1 to (Array.length a1)-1 do a.(i) <- a1.(i) done;
	for j=0 to (Array.length a2)-1 do a.(d1+j) <- a2.(j) done;
	a;;

(* Les trois fonctions suivantes vnt être utiles pour copier des cartes qui sont des enregistrements *)	
let match_couleur ch = match ch with 
	| 'H' -> 'H'
	| 'D' -> 'D'
	| 'C' -> 'C'
	| 'S' -> 'S'
	| ' ' -> ' ';;

let match_valeur v = match v with
	| 0 -> 0 
	| 1 -> 1
	| 2 -> 2
	| 3 -> 3
	| 4 -> 4 
	| 5 -> 5 
	| 6 -> 6 
	| 7 -> 7 
	| 8 -> 8 
	| 9 -> 9 
	| 10 -> 10 
	| 11 -> 11 
	| 12 -> 12
	| 13 -> 13;;
	
let copie_carte { couleur =  x ; valeur = y} = { couleur =  (match_couleur x); valeur = (match_valeur y)};; 

let copie_cartes_array a = (* on copie un tableau de cartes (donc tableau d'enregistrements) fonction qui va être utilisé dans les fonctions de 
de calcul en fin de manche ou dans l'IA des joueurs ordinateurs *)
	let borne = Array.length a in
	let new_array = tas_vide borne in 
	for i=0 to borne-1 do (let carte = a.(i) in (new_array.(i) <- (copie_carte carte))) done;
	new_array;;

let ajoute_valeur_graphics_mise arr fonct = (* La graphiques nécessitait d'avoir un tableau de fonctions affichant les mises des joueurs *) 
	(* cette fonction peut donc prendre en argument une fonction ('a -> unit) et un ('a -> unit) array et renvoie un nouvel array avec la focntion  *)
	let f x = () in 
	let d = Array.length arr in 
	let n = Array.make (d+1) (f) in 
	for i=0 to d do (if i!=d then n.(i) <- arr.(i) else n.(i) <- fonct) done; 
	n;;
	
let indice_array valeur arr (* renvoie l'indice d'une valeur dans un array *)= 
	let x = ref 0 in 
	for i=0 to (Array.length arr)-1 do if arr.(i) = valeur then x:= i done; !x;;
	
(* ###################################### Gestion cartes ###################################### *)
(* Dans cette partie on retrouve les fonctions de gestion du jeu de carte et tout ce qui tourne autour des cartes en soi *)

let creer_deck () = 
	let deck = tas_vide 52 in 
	let couleurs = [| 'S'; 'C'; 'H'; 'D'|] in (* S = Spades; C = Clubs; H = Hearts; D = Diamonds *) 
	(* on donne  des valeurs allant de 1 à 13 pour les cartes,les têtes correspondent aux valeurs 11, 12 et 13 *)
	for j = 0 to 3 do (for i = 1 to 13 do deck.(i+(j*13)-1) <- {couleur = couleurs.(j); valeur = i} done) done; deck;;

let cree_defausse_vide () = tas_vide 52;; (* pile de défausse sur la table *)

let cree_cartes_posees_vide () = tas_vide 5;; (* cartes qui vont ensuite être affichées sur la table *)

let switch t i j = let memoire = t.(i) in t.(i) <- t.(j); t.(j)<-memoire; t;; (* une fonction qui intervertit deux valeurs d'un array  *)
  
let shuffle deck = 
(* on parcourt l'array de la fin jusqu'au début avec une variable n et on échange une carte comprise entre 0 et n-1 que l'on échange avec la carte d'indice n *)
	let rec aux compteur = match compteur with
	| 0 -> deck 
	| _ -> let indice = Random.int(compteur)+1 in switch (aux (compteur-1)) indice (compteur-1)
	in aux 52;;

let premiere_carte deck = (* comme les decks sont de tailles fixes il se peut qu'il y aie des cartes vides donc il faur parcourir le deck jusqu'à une carte non vide *)
	let i = ref 0 in 
	while deck.(!i).valeur = 0 do i:= !i + 1 done; !i;;

let distribue jeu = (* on parcourt chaque joueur et on place les deux premières cartes du deck dans leur main *) 
	for i = 0 to (Array.length jeu.joueurs)-1 do 
		for j= 0 to 1 do 
			let indice = premiere_carte jeu.pack in (jeu.joueurs.(i).main.(j) <- jeu.pack.(indice); jeu.pack.(indice) <- {couleur = ' '; valeur = 0})
		 done
	 done;;

let brule_carte jeu = (* à chaque carte affiché il faut en défausser une avant, on tire donc une carte du deck que l'on place dans la défausse *)
	let i = ref 0 in 
	while jeu.pack.(!i).valeur = 0 do i:= !i + 1 done;
	let j = ref 0 in 
	while jeu.c_defaussees.(!j).valeur != 0 do j:= !j + 1 done;
	jeu.c_defaussees.(!j) <- jeu.pack.(!i);
	jeu.pack.(!i) <- {couleur = ' '; valeur = 0};;


let fold_joueur jeu indice = (* on change l'état du joueur à 0, ses cartes sont défaussées et sa main devient la main vide *)
	jeu.joueurs.(indice).etat <- 0;
	let j = ref 0 in 
	while jeu.c_defaussees.(!j).valeur != 0 do j:= !j + 1 done;
	jeu.c_defaussees.(!j) <- jeu.joueurs.(indice).main.(0);
	jeu.c_defaussees.(!j+1) <- jeu.joueurs.(indice).main.(1);
	jeu.joueurs.(indice).main <- main_vide ();;

let flop jeu = (* on affiche 3 cartes sur la table *)
	let i = ref 0 in 
	while jeu.pack.(!i).valeur = 0 do i:= !i + 1 done;
	jeu.c_posees.(0) <- jeu.pack.(!i);
	jeu.c_posees.(1) <- jeu.pack.(!i + 1);
	jeu.c_posees.(2) <- jeu.pack.(!i + 2);
	jeu.pack.(!i) <- {couleur = ' '; valeur = 0};
	jeu.pack.(!i + 1) <- {couleur = ' '; valeur = 0};
	jeu.pack.(!i + 2) <- {couleur = ' '; valeur = 0};;

let turn jeu = (* on affiche une autre carte (elle correspond à la 4 ème carte *)
	let i = ref 0 in 
	while jeu.pack.(!i).valeur = 0 do i:= !i + 1 done;
	jeu.c_posees.(3) <- jeu.pack.(!i);
	jeu.pack.(!i) <- {couleur = ' '; valeur = 0};;

let river jeu = (* même fonction que précédemment mais pour afficher la 5 ème carte *)
	let i = ref 0 in 
	while jeu.pack.(!i).valeur = 0 do i:= !i + 1 done;
	jeu.c_posees.(4) <- jeu.pack.(!i);
	jeu.pack.(!i) <- {couleur = ' '; valeur = 0};;


(* ###################################### Gestion Jeu ###################################### *)
(* Cette partie est constituée des fonctions principales du déroulement du jeu *)


let nb_joueurs jeu = (* renvoie le nombre de joueurs n'ayant pas encore perdu (etat != 3) de l'array joueurs du jeu *) 
	let compteur = ref 0 in
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).etat != 3 then compteur:= !compteur + 1) done; !compteur;;
	
let changement_blinds jeu = (* augmente de 1 les indices des blinds jusqu'à arriver à un joueur n'ayant pas encore perdu *)
	let l = Array.length jeu.joueurs in 
	let ok = ref false in 
	(jeu.blinds.big <-  (jeu.blinds.big +  1) mod l; jeu.blinds.small <-  (jeu.blinds.small +  1) mod l);
	while !ok = false do begin (* la fonction ne sera appelée que si le nombre de joueurs encore en jeu est supérieure à 1, donc la boucle while terminera toujours *)
		if jeu.joueurs.(jeu.blinds.big).etat != 3 then ok := true 
		else jeu.blinds.big <-  (jeu.blinds.big +  1) mod l;
		if jeu.joueurs.(jeu.blinds.small).etat = 3 then (ok := false; jeu.blinds.small <-  (jeu.blinds.small +  1) mod l) 
	end
	done;;

let augmente_blinds jeu nb_tours = (* double la mise des blinds à chaque tour de table *) 
	let nouvel_indice = ref nb_tours in 
	if nb_tours = (nb_joueurs jeu) then (jeu.blinds.montant <- (jeu.blinds.montant)*2; nouvel_indice:=0);
	(* un tour de table correspond à n manche où n est le nombre de joueurs initalement *)
	!nouvel_indice;;

let blinds jeu = (* big blinds = montant;  small blinds = montant/2 *)
	(* La mise du joueur en big blind est changée *)
	jeu.joueurs.(jeu.blinds.big).mise <- jeu.blinds.montant;
	(* on retire la mise de son argent *)
	jeu.joueurs.(jeu.blinds.big).argent <- jeu.joueurs.(jeu.blinds.big).argent - jeu.blinds.montant;
	(* on fait de même pour le joueur en small blind *)
	jeu.joueurs.(jeu.blinds.small).mise <- (jeu.blinds.montant)/2;
	jeu.joueurs.(jeu.blinds.small).argent <- jeu.joueurs.(jeu.blinds.small).argent - (jeu.blinds.montant/2);
	jeu.mise_tour <- jeu.blinds.montant;;

let retirer_joueur jeu = (* tout joueur n'ayant olius d'argent voit son état changer à 3, il a donc perdu *)
	let compteur = ref 0 in 
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).argent = 0 then (jeu.joueurs.(i).etat <- 3; compteur := !compteur + 1)) done; !compteur;;

let nb_joueurs_couches jeu = (* décompte de tous les joueurs dont l'état est de 0, un tel joueur n'a pas perdu il ne oeut juste pas jouer la manche*)
	let compteur = ref 0 in
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).etat = 0 then compteur:= !compteur + 1) done; !compteur;;

let joueurs_couches jeu = (* renvoie un array des indices des joueurs couchées *) 
	let result = Array.make (nb_joueurs_couches jeu) 0 in 
	let compteur = ref 0 in 
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).etat = 0 then (result.(!compteur) <- i; compteur := !compteur + 1)) done; result;;

let nb_joueurs_actifs jeu = (* décompte de tous les joueurs dont l'état est de 1 *)
	let compteur = ref 0 in
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).etat = 1 then compteur:= !compteur + 1) done; !compteur;;

let joueurs_actifs jeu = (* renvoie un array des indices des joueurs actifs *)
	let result = Array.make (nb_joueurs_actifs jeu) 0 in 
	let compteur = ref 0 in 
	for i = 0 to Array.length jeu.joueurs-1 do (if jeu.joueurs.(i).etat = 1 then (result.(!compteur) <- i; compteur := !compteur + 1)) done; result;;

let tab_joueurs nb = (* créé un array de nb joueurs pour l'initialisation de la partie *)
	let t = Array.make nb {clef = ' '; main = main_vide(); etat = 1; argent = 10000; mise = 0} in
	for i = 0 to nb-2 do t.(i) <- {clef = 'A'; main = main_vide(); etat = 1; argent = 10000; mise = 0} done;
	(* 'A' : Artificial; 'H' : Human *)
	t.(nb-1) <- {clef = 'H'; main = main_vide(); etat = 1; argent = 10000; mise = 0};
	t;;

let fin_manche jeu = (* test pour savoir si une manche est terminé car il ne reste plus qu'un joueur dont l'état est égal à 1 *)
	let rep = ref false in
	if nb_joueurs_actifs jeu = 1 then rep:=true;
	!rep;;

let unique_joueur jeu = (* dans le cas précédent,l'argent de la table est remis à l'unique joueur restant *) 
	jeu.joueurs.((joueurs_actifs jeu).(0)).argent <- jeu.joueurs.((joueurs_actifs jeu).(0)).argent + jeu.mise_table;;

let fin jeu = (* test pour savoir si la partie est terminée *)
	let rep = ref false in
	if nb_joueurs jeu = 1 then rep:=true;
	!rep;;

let fin_joueur_humain jeu = (* Pour indiquer si le joueur humain à perdu *)
	let rep = ref false in 
	if jeu.joueurs.(7).etat = 3 then rep:=true; !rep;;
	
(* ###################################### Gestion fin de manche ###################################### *)
(* Partie constituée des fonctions de calculs de fin de manche *)
(* Pour savoir qui gagne on fait tout un décompte de points allant de 1 à 579 et le joueur ayant le plus grand score gagne la manche *)
(* La valeur de l'AS peut valoir 1 ou 14, ce qui explioque pourquoi on travaille sur des copies des mains et des cartes posées *)

let high_card main = (* on prend le maximum de la valeur des deux cartes *)
	let x = ref main.(0).valeur in 
	let y = ref main.(1).valeur in 
	if !x=1 then x:=14 else if !y=1 then y:=14;
	max (!x-1) (!y-1);; (* points: 1-13 *)

let pair main cartes_posees = 
	let pair = [|  { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0} |] in
	let score = ref 0 in 
	(* on créé des varaibels de valeurs dans le cas où un as est présent sur la table *)
	let v1 = ref main.(0).valeur in if !v1=1 then v1:=14;
	let v2 = ref main.(1).valeur in if !v2=1 then v2:=14;
	let v_c1 = ref cartes_posees.(0).valeur in if !v_c1=1 then v_c1:=14;
	let v_c2 = ref cartes_posees.(1).valeur in if !v_c2=1 then v_c2:=14;
	(* on regarde si le joueur possède une paire dans sa main *)
	if  main.(0).valeur = main.(1).valeur then (pair.(0) <- main.(0); pair.(1) <- main.(1); score:= 13*(!v1-1));
	for i=0 to 1 do (* on parcourt la main et les cartes posées et on regarde s'il y a une paire plus grande que celle enregistrée *)
		for j=0 to 2 do 
			if main.(i).valeur = cartes_posees.(j).valeur then 
				if (pair.(0).valeur != 0 && pair.(0).valeur < main.(i).valeur) then 
					(pair.(0) <- main.(i); pair.(1) <- cartes_posees.(j); if i=0 then score:= 13*(!v1-1)+(!v2)-1 else score:= 13*(!v2-1)+(!v1)-1)
				else if pair.(0).valeur = 0 then (pair.(0) <- main.(i); pair.(1) <- cartes_posees.(j); if i=0 then score:= 13*(!v1-1)+(!v2)-1 else score:= 13*(!v2-1)+(!v1)-1)
		(* Si il y a une nouvelle paire on la remplace, généralement il n'a pas de remplacement car cette fonction sera appelé que lorsqu'il n'y a qu'une seule paire *)
		done
	done;
	if (cartes_posees.(0) = cartes_posees.(1) && !v_c1 > pair.(0).valeur) then (pair.(0) <- cartes_posees.(0); pair.(1) <- cartes_posees.(1); score:= 13 + 13*(!v_c1-1)+(max !v1 !v2)-1)
	else if (cartes_posees.(0) = cartes_posees.(2) && !v_c1 > pair.(0).valeur) then (pair.(0) <- cartes_posees.(0); pair.(1) <- cartes_posees.(2); score:= 13 + 13*(!v_c1-1)+(max !v1 !v2)-1)
	else if (cartes_posees.(1) = cartes_posees.(2) && !v_c2 > pair.(0).valeur) then (pair.(0) <- cartes_posees.(1); pair.(1) <- cartes_posees.(2); score:= 13 + 13*(!v_c2-1)+(max !v1 !v2)-1);
	(* dans le décompte des points on regarde aussi la valeur max de la main poiur départager deux joueurs qui aurait une même paire *)
	let trouve = ref false in
	if !score != 0 then  trouve:= true;
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si une paire a été trouvée *)
	(!score, !trouve);; (* points: 14-182 *)
	
let double_pair main cartes_posees = (* même principe que la focntion pair*)
	if main.(0).valeur = 1 then main.(0).valeur <- 14;
	if main.(1).valeur = 1 then main.(0).valeur <- 14;
	if cartes_posees.(0).valeur = 1 then cartes_posees.(0).valeur <- 14;
	if cartes_posees.(1).valeur = 1 then cartes_posees.(1).valeur <- 14;
	if cartes_posees.(2).valeur = 1 then cartes_posees.(2).valeur <- 14;
	let pair = [|  { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0} |] in
	if  main.(0).valeur = main.(1).valeur then (pair.(0) <- main.(0); pair.(1) <- main.(1));
	for i=0 to 1 do 
		for j=0 to 4 do 
			if main.(i).valeur = cartes_posees.(j).valeur then 
				if pair.(0).valeur = 0 then (pair.(0) <- main.(i); pair.(1) <- cartes_posees.(j))
				else if pair.(2).valeur = 0 then (pair.(2) <- main.(i); pair.(3) <- cartes_posees.(j))
				else if (pair.(0).valeur != 0 && pair.(0).valeur < main.(i).valeur) then (pair.(0) <- main.(i); pair.(1) <- cartes_posees.(j))
				else if (pair.(2).valeur != 0 && pair.(2).valeur < main.(i).valeur) then (pair.(2) <- main.(i); pair.(3) <- cartes_posees.(j))
		done
	done;
	if (cartes_posees.(0) = cartes_posees.(1) && cartes_posees.(0).valeur > pair.(0).valeur) then (pair.(0) <- cartes_posees.(0); pair.(1) <- cartes_posees.(1))
	else if (cartes_posees.(0) = cartes_posees.(1) && cartes_posees.(0).valeur > pair.(2).valeur) then (pair.(2) <- cartes_posees.(0); pair.(3) <- cartes_posees.(1))
	else if (cartes_posees.(0) = cartes_posees.(2) && cartes_posees.(0).valeur > pair.(0).valeur) then (pair.(0) <- cartes_posees.(0); pair.(1) <- cartes_posees.(2))
	else if (cartes_posees.(0) = cartes_posees.(2) && cartes_posees.(0).valeur > pair.(2).valeur) then (pair.(2) <- cartes_posees.(0); pair.(3) <- cartes_posees.(2))
	else if (cartes_posees.(1) = cartes_posees.(2) && cartes_posees.(1).valeur > pair.(0).valeur) then (pair.(0) <- cartes_posees.(1); pair.(1) <- cartes_posees.(2))
	else if (cartes_posees.(1) = cartes_posees.(2) && cartes_posees.(1).valeur > pair.(2).valeur) then (pair.(2) <- cartes_posees.(1); pair.(2) <- cartes_posees.(2));
	let score = ref 0 in 
	let trouve = ref false in 
	if pair.(0).valeur != 0 && pair.(2).valeur != 0 then begin 
		score := 182 + (13 * ((max (pair.(0).valeur) (pair.(2).valeur))-1) +( min (pair.(0).valeur) (pair.(2).valeur)) - 1);
		trouve := true 
	end;
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si une double paire a été trouvée *)
	(!score,!trouve);; (* points: 183-351 *)

let toak main cartes_posees = (* fonction de teste de brelan: "three of a kind " = TOAK *)
	let t = [| { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0} |] in 
	(* ici on travaille sur la concatenation de la main et des cartes posées pour plus de simplcité *)
	(* le principe reste encore le même que les deux fonctions précédentes, on teste et enregistre les cartes faisant un brelan *)
	let a = concatene_array main cartes_posees in 
	for l=0 to 6 do if a.(l).valeur = 1 then a.(l).valeur <- 14 done;
	for i=0 to 4 do 
		for j=(i+1) to 5 do 
			for k=(j+1) to 6 do 
				if a.(i).valeur = a.(j).valeur && a.(i).valeur = a.(k).valeur && a.(i).valeur > t.(0).valeur then 
				(t.(0) <- a.(i); t.(1) <- a.(j); t.(2) <- a.(k))
			done 
		done
	done;
	let score = ref 0 in 
	let trouve = ref false in 
	if t.(0).valeur != 0 then begin 
		score := 351 + 13*(t.(0).valeur-1);
		trouve := true 
	end;
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si un brelan a été trouvée *)
	(!score,!trouve);; (* points: 352-364 *)

(* Pour tester les suites il était plus judicieux de passer par la structure des sets, la complexité est donc un oeu moindre que de parcourir en boucle un array *)
(* On a donc crée un set dont les éléments sont des cartes *)
open Set;;
module Carte = struct type t = carte let compare = compare end;;
module SC = Set.Make(Carte) ;;

let straight main cartes_posees = (* fonction de test de suites *)
(* ici on travaille sur la concatenation de la main et des cartes posées pour plus de simplcité *)
	let arr = concatene_array main cartes_posees in
	let s = Array.fold_right SC.add arr SC.empty in
	(* ici on enregistrera que la première carte car s'il y a une suite on connait les cartes qui suivent *)
	let k = ref {couleur = ' ' ; valeur = 0} in
	for i=0  to 6 do
		let d = ref arr.(i).valeur in
		if (!d-1) <= 10 then 
		(let trouve = ref true in
		let compteur = ref 0 in
		while !trouve = true do
		(* on cherche 5 cartes dont les valeurs sont consécutives *)
		(* la recherche s'arrete dès qu'il y a une erreur ou lorsqu'une suite à été trouvé *)
			if not (SC.mem {couleur = 'H' ; valeur = (!d)+1} s || SC.mem {couleur = 'D' ; valeur = (!d)+1} s || SC.mem {couleur = 'S' ; valeur = (!d)+1} s || SC.mem {couleur = 'c' ; valeur = (!d)+1} s) then
				trouve:=  false;
			if !compteur = 4 then k:= arr.(i);
			d:= !d +1; 
			compteur:= !compteur +1;
		done)
	done;
	let score  = ref 0 in 
	let ok = ref false in
	if !k.valeur != 0 then (score := 364 + (!k).valeur; ok:=true);
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si une suite a été trouvée *)
	(!score,!ok);; (* points: 365-374 *)

let flush main cartes_posees = (* fonction de test d'un flush *)
(* ici on travaille sur la concatenation de la main et des cartes posées pour plus de simplcité *)
	let arr = concatene_array main cartes_posees in 
	(* ici on va compter les cartes ayant les mêmes couleurs, la deuxième valeur du couple corresspond à la carte de plus haute valeur de la couleur correspondantes *)
	(* compteur.(0) -> hearts; compteur.(1) -> diamonds; compteur.(2) -> spades; compteur.(3) -> clubs *)
	let compteur = [| [|0;0|]; [|0;0|]; [|0;0|]; [|0;0|] |] in 
	for i=0 to 6 do 
		(if arr.(i).couleur = 'H' then (if arr.(i).valeur = 1 then arr.(i).valeur <- 14; compteur.(0).(0) <- compteur.(0).(0)+1; if arr.(i).valeur > compteur.(0).(1) then  compteur.(0).(1) <- arr.(i).valeur)
		else if arr.(i).couleur = 'D' then (if arr.(i).valeur = 1 then arr.(i).valeur <- 14; compteur.(1).(0) <- compteur.(1).(0)+1; if arr.(i).valeur > compteur.(1).(1) then  compteur.(1).(1) <- arr.(i).valeur)
		else if arr.(i).couleur = 'S' then (if arr.(i).valeur = 1 then arr.(i).valeur <- 14; compteur.(2).(0) <- compteur.(2).(0)+1; if arr.(i).valeur > compteur.(2).(1) then  compteur.(2).(1) <- arr.(i).valeur) 
		else if arr.(i).couleur = 'C' then (if arr.(i).valeur = 1 then arr.(i).valeur <- 14; compteur.(3).(0) <- compteur.(3).(0)+1; if arr.(i).valeur > compteur.(3).(1) then  compteur.(3).(1) <- arr.(i).valeur) ) done;
	let score = ref 0 in 
	let trouve = ref false in 
	(* on regarde chacun des compteurs pour voir s'il y a 5 cartes d'une même couleur *)
	if compteur.(0).(0) >= 5 then (score:= 374 + compteur.(0).(1); trouve:=true)
	else if compteur.(1).(0) >= 5 then (score:= 374 + compteur.(1).(1); trouve:=true)
	else if compteur.(2).(0) >= 5 then (score:= 374 + compteur.(2).(1); trouve:=true)
	else if compteur.(3).(0) >= 5 then (score:= 374 + compteur.(3).(1); trouve:=true);
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si un flush a été trouvée *)
	(!score, !trouve);; (* points: 375-387 *)


let full_house main cartes_posees = (* fonction de test d'un full house *)
	let t = [| { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0}; { couleur =  ' ' ; valeur = 0} |] in 
	(* ici on travaille sur la concatenation de la main et des cartes posées pour plus de simplcité *)
	let a = concatene_array main cartes_posees in 
	(* cela fonctionne de la même manière que les deux paires, ou toak, sauf qu'ici il faut d'abord trouver un brelan, puis on regarde dans les cartes qui restent
	et on  garde la plius grande pair *)
	for x=0 to 6 do if a.(x).valeur = 1 then a.(x).valeur <- 14 done;
	for i=0 to 4 do 
		(for j=(i+1) to 5 do 
			(for k=(j+1) to 6 do 
				(if a.(i).valeur = a.(j).valeur && a.(i).valeur = a.(k).valeur && a.(i).valeur >= t.(0).valeur then 
					(for l=0 to 6 do 
						(for  m=0 to 6 do 
							(if (l!=i && l!=j && l!=k && m!=i && m!=j && m!=k && a.(l).valeur = a.(m).valeur && a.(l).valeur >= t.(3).valeur) then 
								 (t.(0) <- a.(i); t.(1) <- a.(j); t.(2) <- a.(k); t.(3) <- a.(l); t.(4) <-a.(m))) done) done)) done) done) done;	
	let score = ref 0 in 
	let trouve = ref false in 
	if t.(0).valeur != 0 then begin 
		score := 387 + 13*(t.(0).valeur-1) + t.(3).valeur -1;
		trouve := true 
	end;
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si un full house a été trouvée *)
	(!score,!trouve);; (* points: 387-556 *)

let foak main cartes_posees = (* fonction de teste de quadruplé : "four of a kind " = FOAK *)
(* ici on travaille sur la concatenation de la main et des cartes posées pour plus de simplcité *)
	let a = concatene_array main cartes_posees in 
	(* ici on va compter les cartes ayant les mêmes valeurs, la deuxième valeur du couple corresspond à la valeur de la carte *)
	let compteur = [| [|0;0|]; [|0;0|]; [|0;0|]; [|0;0|]; [|0;0|]|] in
	let continue = ref true in
	let trouve = ref false in 
	let i = ref 0 in
	while !continue = true do
		if (compteur.(0).(1) = 0 || compteur.(0).(0) = a.(!i).valeur) then ( compteur.(0).(1) <- compteur.(0).(1) +1; compteur.(0).(0) <- a.(!i).valeur)
		else if (compteur.(1).(1) = 0 || compteur.(1).(0) = a.(!i).valeur) then ( compteur.(1).(1) <- compteur.(1).(1) +1; compteur.(1).(0) <- a.(!i).valeur)
		else if (compteur.(2).(1) = 0 || compteur.(2).(0) = a.(!i).valeur) then ( compteur.(2).(1) <- compteur.(2).(1) +1; compteur.(2).(0) <- a.(!i).valeur)
		else if (compteur.(3).(1) = 0 || compteur.(3).(0) = a.(!i).valeur) then ( compteur.(3).(1) <- compteur.(3).(1) +1; compteur.(3).(0) <- a.(!i).valeur)
		else if (compteur.(4).(1) = 0 || compteur.(4).(0) = a.(!i).valeur) then ( compteur.(4).(1) <- compteur.(4).(1) +1; compteur.(4).(0) <- a.(!i).valeur);
		if compteur.(4).(1) != 0 then (continue := false); (* si il y a 5 cartes différentes on ne peut avoir 4 cartes de même valeur donc on arrête de chercher *)
		if !i = 6 then (continue:=false; trouve:= true);
		i:= !i+1;
	done;
	let score = ref 0 in
	if !trouve = true then 
		if compteur.(0).(1) = 4 then (if compteur.(0).(0) = 1 then compteur.(0).(0) <- 14; score := 556 + compteur.(0).(0)-1 )
		else if compteur.(1).(1) = 4 then (if compteur.(1).(0) = 1 then compteur.(1).(0) <- 14; score := 556 + compteur.(1).(0)-1 )
		else if compteur.(2).(1) = 4 then (if compteur.(2).(0) = 1 then compteur.(2).(0) <- 14; score := 556 + compteur.(2).(0)-1 )
		else if compteur.(3).(1) = 4 then (if compteur.(3).(0) = 1 then compteur.(3).(0) <- 14; score := 556 + compteur.(3).(0)-1 );
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si un quadruplé a été trouvée *)
	(!score,!trouve);; (* points: 556-569 *)


let straight_flush main cartes_posees = (* fonction de test de suite à la couleur *)
(* on réitère le même processus qu'avec les suites, donc en travaillant avec un ensemble de cartes et en vérifiant l'apartenance de cartes successives 
ayant la même couleur dans l'ensemble *)
	let arr = concatene_array main cartes_posees in
	let s = Array.fold_right SC.add arr SC.empty in
	let k = ref {couleur = ' ' ; valeur = 0} in
	for i=0  to 6 do
		let d = ref arr.(i).valeur in
		let e = arr.(i).couleur in
		if (!d-1) <= 10 then 
		(let trouve = ref true in
		let compteur = ref 0 in
		while !trouve = true do
			if not (SC.mem {couleur = e ; valeur = (!d)+1} s) then
				trouve:=  false;
			if !compteur = 4 then k:= arr.(i);
			d:= !d +1; 
			compteur:= !compteur +1;
		done)
	done;
	let score  = ref 0 in 
	let ok = ref false in
	if !k.valeur != 0 then (score := 569 + (!k).valeur; ok:=true);
	(* renvoie un couple avec le score (nul s'il n'y a rien) et un booléen indiquant si une suite à la couleur a été trouvée *)
	(!score,!ok);; (* points: 570-579 *)

let points joueur jeu = (* la fonction d'attribution de points à un joueur *)
	(* comme les valeurs des cartes peuvent être modifiées, on travaille sur des copies *)  
	let cartes_posees = jeu.c_posees in
	let main = joueur.main in
	let score = ref 0 in 
	(* on va dans le sens décroissant des points, si une fonction renvoie un booléen indiquant qu'une combinaison a été trouvé, on s'arrête *) 
	let (a, b) = straight_flush (copie_cartes_array main) (copie_cartes_array cartes_posees) in
	if b = true then score := a else
		(let (c, d) = foak (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
		if d = true then score := c else
			(let (e, f) = full_house (copie_cartes_array main) (copie_cartes_array cartes_posees) in
			if f = true then score := e else
				(let (g, h) = flush (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
				if h = true then score := g else 
					(let (i, j) = straight (copie_cartes_array main) (copie_cartes_array cartes_posees) in
					if j = true then score := i else
						(let (k, l) = toak (copie_cartes_array main) (copie_cartes_array cartes_posees) in  
						if l = true then score := k else 
							(let (m, n) = double_pair (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
							if n = true then score := m else
								(let (o, p) = pair (copie_cartes_array main) (copie_cartes_array cartes_posees) in
								if p = true then score := o else score := high_card (copie_cartes_array main) )))))));
	!score;;

let valeur_score_max score = (* donne la valeur du score maximum dans un array des différents couples (x,y) où x est l'indice des joueurs et y leur score *) 
	let maxi = ref 0 in
	for i=0 to (Array.length score)-1 do (let (x,y) = score.(i) in if y > !maxi then maxi:=y) done;
	!maxi;;

let nombre_gagnant score_max scores = (* renvoie le nombre de joueurs ayant eu un score égal au score maximal de la manche *)
	let compteur = ref 0 in
	for i= 0 to (Array.length scores)-1 do (let (x, y) = scores.(i) in if y=score_max then compteur := !compteur +1) done; !compteur ;;

let gagnants score_max scores = (* renvoie un array des indices des joueurs ayant eu un score égal au score maximal de la manche, ie les gagants de la manche  *) 
	let  compteur = ref 0 in
	let arr = Array.make (nombre_gagnant score_max scores) 0 in 
	for i=0 to (Array.length arr)-1 do (let (x, y) = scores.(i) in if y=score_max then (arr.(!compteur) <- x; compteur := !compteur + 1)) done;
	arr;;

(* ###################################### Gestion joueurs ordi ###################################### *)
(* Cette partie est constituée des fonctions nécessaire à l'intelligence artificielle des ordinateurs *)

let proba main cartes_posees = (* en fonction de leur main et des cartes posées, on attribuera une probabilité de chance choisie arbitrairement *)
	(* on réutilise toutes les fonctions de résolutiuon de fin de manche *)
	let proba = ref 0.1 in
	let d = Array.length cartes_posees in
	if d != 0 then 
	(* on va dans le sens décroissant des points, si une fonction renvoie un booléen indiquant qu'une combinaison a été trouvé, on s'arrête *)
		(let (a, b) = straight_flush (copie_cartes_array main) (copie_cartes_array cartes_posees) in
		if b = true then proba := 1. else
			(let (c, d) = foak (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
			if d = true then proba := 0.95 else
				(let (e, f) = full_house (copie_cartes_array main) (copie_cartes_array cartes_posees) in
				if f = true then proba := 0.9 else
					(let (g, h) = flush (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
					if h = true then proba := 0.85 else 
						(let (i, j) = straight (copie_cartes_array main) (copie_cartes_array cartes_posees) in
						if j = true then proba := 0.8 else
							(let (k, l) = toak (copie_cartes_array main) (copie_cartes_array cartes_posees) in  
							if l = true then proba := 0.7 else 
								(let (m, n) = double_pair (copie_cartes_array main) (copie_cartes_array cartes_posees) in 
								if n = true then proba := 0.6 else
									(let (o, p) = pair (copie_cartes_array main) (copie_cartes_array cartes_posees) in
									if p = true then proba := 0.5 else proba := float_of_int (high_card (copie_cartes_array main))/.26. ))))))))
	else 
		if main.(0).valeur = main.(1).valeur && main.(0).valeur = 1  then proba := 1.
		else if main.(0).valeur = main.(1).valeur then proba := ((float_of_int main.(0).valeur)/. 22.4) +. 0.375
		else if main.(0).valeur > 8 && main.(1).valeur > 8 then (proba:= 0.6; if abs (main.(0).valeur - main.(1).valeur) > 4 then proba := !proba -. 0.1)
		else if main.(0).valeur < 5 && main.(1).valeur < 5 then (proba:= 0.2; if abs (main.(0).valeur - main.(1).valeur) > 4 then proba := !proba -. 0.1)
		else if main.(0).valeur > 8 || main.(1).valeur > 8 then (proba:= 0.5; if abs (main.(0).valeur - main.(1).valeur) > 4 then proba := !proba -. 0.1);
	(* renvoie la probabilité calculée *)
	!proba;;
	
let computer jeu indice_joueur = (* intelligence artificielle *)
	let act = {fold = false; check = false; call = false; raise = false; all_in = false; amount = 0} in 
	let main = jeu.joueurs.(indice_joueur).main in 
	let cartes_posees = jeu.c_posees in 
	let proba = (proba main cartes_posees) in 
	if jeu.mise_tour != 0 && jeu.joueurs.(indice_joueur).mise != jeu.mise_tour then 
		(if proba = 1. then (act.raise <- true; act.all_in <- true; act.amount <- jeu.joueurs.(indice_joueur).argent - jeu.joueurs.(indice_joueur).mise)
		else if jeu.mise_tour >= 2*jeu.blinds.montant then 
			(if proba >= 0.8 then (act.call <- true; act.amount <- (jeu.mise_tour - jeu.joueurs.(indice_joueur).mise)) else act.fold <- true)
		else if jeu.mise_tour >= jeu.blinds.montant/2 then 
			(if proba >= 0.6 then (act.call <- true; act.amount <- (jeu.mise_tour - jeu.joueurs.(indice_joueur).mise)) else act.fold <- true)
		else act.fold <- true)
	else if jeu.mise_tour != 0 && jeu.joueurs.(indice_joueur).mise = jeu.mise_tour then 
		(if proba = 1. then (act.raise <- true; act.all_in <- true; act.amount <- jeu.joueurs.(indice_joueur).argent )
		else act.check <- true)
	else if proba = 1. then (act.raise <- true; act.all_in <- true; act.amount <- jeu.joueurs.(indice_joueur).argent )
		else if proba > 0.9 then (act.raise <- true; act.amount <- 2*jeu.blinds.montant)
		else if proba > 0.8 then (act.raise <- true; act.amount <- jeu.blinds.montant)
		else if proba > 0.7 then (act.raise <- true; act.amount <- jeu.blinds.montant/2)
		else if proba > 0.3 then act.check <- true
		else act.fold <- true;
	attente 1.5;
	act;;
(* ###################################### Graphics ###################################### *)

#load "graphics.cma";;
open Graphics;;


let heart dim_x dim_y x y = (* graphique logo coeur *) 
	set_color red;
	fill_rect ((0*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((1*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((2*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (6*dim_y);
	fill_rect ((3*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (6*dim_y);
	fill_rect ((4*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (6*dim_y);
	fill_rect ((5*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((6*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (3*dim_y);;

let spade dim_x dim_y x y = (* graphique logo pique *) 
	set_color black;
	fill_rect ((0*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (2*dim_y);
	fill_rect ((1*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((2*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((3*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (7*dim_y);
	fill_rect ((4*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((5*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((6*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (2*dim_y);
	fill_rect ((2*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((4*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (1*dim_y);;

let club dim_x dim_y x y = (* graphique logo trèfle *) 
	set_color black; 
	fill_rect ((0*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((1*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((2*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((2*dim_x)+x) ((5*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((2*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((3*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (7*dim_y);
	fill_rect ((4*dim_x)+x) ((5*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((4*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((4*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((5*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((6*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);;

let diamond dim_x dim_y x y = (* graphique logo carreau *) 
	set_color red; 
	fill_rect ((0*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((1*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((2*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((3*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (7*dim_y);
	fill_rect ((4*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((5*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((6*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);;

let logo_inside dim_x dim_y x y = (* graphique logo dos de la carte *) 
	set_color (rgb 102 178 255); 
	fill_rect ((0*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((1*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((2*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((3*dim_x)+x) ((0*dim_y)+y) (1*dim_x) (7*dim_y);
	fill_rect ((4*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((5*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((6*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	set_color (rgb 127 0 255);
	fill_rect ((1*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);
	fill_rect ((2*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((3*dim_x)+x) ((1*dim_y)+y) (1*dim_x) (5*dim_y);
	fill_rect ((4*dim_x)+x) ((2*dim_y)+y) (1*dim_x) (3*dim_y);
	fill_rect ((5*dim_x)+x) ((3*dim_y)+y) (1*dim_x) (1*dim_y);;

let carte couleur valeur x y d = (* graphique carte *) 
	set_color white;
	fill_rect (int_of_float x)  (int_of_float y) (int_of_float (d *. 250.)) (int_of_float (d *. 400.));
	set_color black;
	draw_rect (int_of_float x)  (int_of_float y) (int_of_float (d *. 250.)) (int_of_float (d *. 400.));
	if couleur = 'S'  then (spade (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(10. *. d)))+2) ((int_of_float (y+.(292. *. d)))+2); spade (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(191. *. d)))+2) ((int_of_float (y+.(59. *. d)))+1));
	if couleur = 'C'  then (club (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(10. *. d)))+2) ((int_of_float (y+.(292. *. d)))+2); club (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(191. *. d)))+2) ((int_of_float (y+.(59. *. d)))+1));
	if couleur = 'H'  then (heart (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(10. *. d)))+2) ((int_of_float (y+.(292. *. d)))+2); heart (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(191. *. d)))+2) ((int_of_float (y+.(59. *. d)))+1));
	if couleur = 'D'  then (diamond (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(10. *. d)))+2) ((int_of_float (y+.(292. *. d)))+2); diamond (int_of_float (d *. 7.)) (int_of_float (d *. 7.)) ((int_of_float (x+.(191. *. d)))+2) ((int_of_float (y+.(59. *. d)))+1));
	let f = "-*-fixed-medium-r-semicondensed--" ^ string_of_int (int_of_float (d *. 50.)) ^ "-*-*-*-*-*-iso8859-1" in set_font f; (* texte redimensionnable *) 
	if valeur = 1 then (moveto (int_of_float (x+.(25. *. d))) (int_of_float (y+.(346. *. d))); draw_string "A"; moveto (int_of_float (x+.(206. *. d))) (int_of_float (y+.(5. *. d))); draw_string "A")
	else if valeur < 10 then (moveto (int_of_float (x+.(25. *. d))) (int_of_float (y+.(346. *. d))); draw_string (string_of_int valeur); moveto (int_of_float (x+.(206. *. d))) (int_of_float (y+.(5. *. d))); draw_string (string_of_int valeur)) 
	else if valeur = 10 then (moveto (int_of_float (x+.(15. *. d))) (int_of_float (y+.(346. *. d))); draw_string (string_of_int valeur); moveto (int_of_float (x+.(196. *. d))) (int_of_float (y+.(5. *. d))); draw_string (string_of_int valeur))
	else if valeur = 11 then (moveto (int_of_float (x+.(25. *. d))) (int_of_float (y+.(346. *. d))); draw_string "J"; moveto (int_of_float (x+.(206. *. d))) (int_of_float (y+.(5. *. d))); draw_string "J")
	else if valeur = 12 then (moveto (int_of_float (x+.(25. *. d))) (int_of_float (y+.(346. *. d))); draw_string "Q"; moveto (int_of_float (x+.(206. *. d))) (int_of_float (y+.(5. *. d))); draw_string "Q")
	else if valeur = 13 then (moveto (int_of_float (x+.(25. *. d))) (int_of_float (y+.(346. *. d))); draw_string "K"; moveto (int_of_float (x+.(206. *. d))) (int_of_float (y+.(5. *. d))); draw_string "K");
	if (couleur = 'S' || couleur = 'C') then set_color black else set_color red;
	fill_rect (int_of_float (x+.(31. *. d))) (int_of_float (y+.(25. *. d))) (int_of_float (d *. 7.)) (int_of_float (d *. 260.)); fill_rect (int_of_float (x+.(38. *. d))) (int_of_float (y+.(25. *. d))) (int_of_float (d *. 150.)) (int_of_float (d *. 7.)); 
	fill_rect (int_of_float (x+.(38. *. d))) (int_of_float (y+.(32. *. d))) (int_of_float (d *. 14.)) (int_of_float (d *. 7.));  fill_rect (int_of_float (x+.(38. *. d))) (int_of_float (y+.(39. *. d))) (int_of_float (d *. 7.)) (int_of_float (d *. 7.));fill_rect (int_of_float (x+.(213. *. d))) (int_of_float (y+.(115. *. d))) (int_of_float (d *. 7.)) (int_of_float (d *. 260.)); 
	fill_rect (int_of_float (x+.(70. *. d))) (int_of_float (y+.(375. *. d))) (int_of_float (d *. 150.)) (int_of_float (d *. 7.)); fill_rect (int_of_float (x+.(199. *. d))) (int_of_float (y+.(368. *. d))) (int_of_float (d *. 14.)) (int_of_float (d *. 7.)); fill_rect (int_of_float (x+.(206. *. d))) (int_of_float (y+.(361. *. d))) (int_of_float (d *. 7.)) (int_of_float (d *. 7.));
	let g = "-*-fixed-medium-r-semicondensed--" ^ string_of_int (int_of_float (d *. 70.)) ^ "-*-*-*-*-*-iso8859-1" in set_font g;
	set_color black; 
	moveto ((int_of_float x)+(int_of_float (d*.115.))) ((int_of_float y)+(int_of_float (d*.190.)));
	draw_string (String.make 1 couleur);;

let dos_carte_table x y = (* graphique dos de carte *) 
	set_color (rgb 0 102 204);
	fill_rect x y 62 100;
	set_color black;
	draw_rect x y 62 100;
	logo_inside 3 3 (x+21) (y+40);
	set_color white;
	fill_rect (x+5) (y+5) 3 87; fill_rect (x+53) (y+5) 3 87; fill_rect (x+6) (y+5) 50 3; fill_rect (x+6) (y+90) 50 3;;

let back () = (* graphique fenetre *)
	set_color (rgb 96 96 96);
	fill_rect 0 0 1200 720;
	set_color (rgb 102 51 0);
	fill_ellipse 300 350 115 215;
	fill_ellipse 900 350 115 215;
	fill_rect 290 135 620 430;
	set_color (rgb 0 153 51);
	fill_rect 300 150 600 400;
	fill_ellipse 300 350 100 200;
	fill_ellipse 900 350 100 200;;

let g_player x y joueur = (* prend en argument un type joueur pour afficher son argent *)
	set_color (rgb 167 162 161);
	fill_circle x y 55;
	set_color (rgb 31 175 218);
	fill_circle x y 50;
	set_color (rgb 167 162 161);
	fill_rect (x-62) (y-45) 124 30;
	set_color (rgb 198 190 190);
	fill_rect (x-57) (y-40) 114 20;
	set_color black;
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	let d = (string_of_int joueur.argent) ^" $" in 
	let h = String.length d in (* pour recentrer le texte en fonction de la longueur en caractèere du montant à afficher *)
	moveto (x-(4*h)) (y-39);
	draw_string d;;

let g_player_tour x y joueur = (* même fonction que précédemment, change juste la couleur du joueur pour indiquer que c'est son tour *)
	set_color (rgb 167 162 161);
	fill_circle x y 55;
	set_color (rgb 238 93 93);
	fill_circle x y 50;
	set_color (rgb 167 162 161);
	fill_rect (x-62) (y-45) 124 30;
	set_color (rgb 198 190 190);
	fill_rect (x-57) (y-40) 114 20;
	set_color black;
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	let d = (string_of_int joueur.argent) ^" $" in 
	let h = String.length d in
	moveto (x-(4*h)) (y-39);
	draw_string d;;

let g_player_fold x y joueur = (* même fonction que précédemment, change juste la couleur du joueur pour indiquer qu'il s'est couché *)
	set_color (rgb 167 162 161);
	fill_circle x y 55;
	set_color (rgb 93 93 93);
	fill_circle x y 50;
	set_color (rgb 167 162 161);
	fill_rect (x-62) (y-45) 124 30;
	set_color (rgb 198 190 190);
	fill_rect (x-57) (y-40) 114 20;
	set_color black;
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	let d = (string_of_int joueur.argent) ^" $" in 
	let h = String.length d in
	moveto (x-(4*h)) (y-39);
	draw_string d;;

let g_player_all_in x y joueur = (* même fonction que précédemment, change juste la couleur du joueur pour indiquer qu'il est tapis *)
	set_color (rgb 167 162 161);
	fill_circle x y 55;
	set_color (rgb 246 191 20);
	fill_circle x y 50;
	set_color (rgb 167 162 161);
	fill_rect (x-62) (y-45) 124 30;
	set_color (rgb 198 190 190);
	fill_rect (x-57) (y-40) 114 20;
	set_color black;
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	let d = (string_of_int joueur.argent) ^" $" in 
	let h = String.length d in
	moveto (x-(4*h)) (y-39);
	draw_string d;;

let g_player_winner x y = (* prend en argument un type joueur pour afficher son argent *)
	set_color red;
	fill_circle x y 55;
	set_color (rgb 31 175 218);
	fill_circle x y 50;
	set_color red;
	fill_rect (x-62) (y-45) 124 30;
	set_color (rgb 198 190 190);
	fill_rect (x-57) (y-40) 114 20;
	set_color black;
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	moveto (x-25) (y-39);
	draw_string "WINNER";;

let list_coord nb_joueur = match nb_joueur with (* array des coordonnées des places des joueurs en fonction du nombre de joueur *)
	| 5 -> [| [|120;350|]; [|430;630|]; [|770;630|]; [|1080;350|]; [|600; 70|] |]  
	| 8 -> [| [|140;210|]; [|140; 490|]; [|370; 630|]; [|600; 630|]; [|830;630|]; [|1060;490|]; [|1060;210|]; [|600; 70|] |];;

let list_coord_carte nb = match nb with(* array des coordonnées des cartes des joueurs en fonction du nombre de joueur *)
	| 5 -> [| (220, 300); (400, 440); (740, 440); (920, 300) |] 
	| 8 -> [| (230, 230); (230, 380); (350, 440); (570, 440); (770, 440); (900, 380); (900, 230) |];;
	
let graphics_main nb = 
	let l = list_coord_carte nb in
	for i=0 to nb-2 do (let (x,y) = l.(i) in (dos_carte_table x y; dos_carte_table (x+5) (y-5))) done;;

let graphics_main_progressive1 nb_joueurs indice = (* affiche la première carte d'une main des joueurs jusqu'à l'indice du joueur indiqué en argument, utile pour la distribution *)
	let l = list_coord_carte nb_joueurs in
	for i = 0 to indice-1 do (let (x,y) = l.(i) in dos_carte_table x y) done;;

let graphics_main_progressive2 nb_joueurs indice = (* affiche la deuxième carte d'une main des joueurs jusqu'à l'indice du joueur indiqué en argument, utile pour la distribution *)
	let l = list_coord_carte nb_joueurs in
	for i = 0 to indice-1 do (let (x,y) = l.(i) in dos_carte_table (x+5) (y-5)) done;;

let graphics_player jeu indice_joueur_actuel= (* prend en argument le jeu pour mettre les noms et les mises, ainsi que l'indice du joueur à qui c'est le tour *) 
	let nb = nb_joueurs jeu in 
	let l_coord = list_coord nb in 
	for i=0 to nb-1 do 
		if i=indice_joueur_actuel then (g_player_tour (l_coord.(i).(0)) (l_coord.(i).(1)) jeu.joueurs.(i)) 
		else if jeu.joueurs.(i).etat = 1 then (g_player (l_coord.(i).(0)) (l_coord.(i).(1)) jeu.joueurs.(i))
		else if jeu.joueurs.(i).etat = 0 then (g_player_fold (l_coord.(i).(0)) (l_coord.(i).(1)) jeu.joueurs.(i))
		else if jeu.joueurs.(i).etat = 2 then (g_player_all_in (l_coord.(i).(0)) (l_coord.(i).(1)) jeu.joueurs.(i))
	done;;

let graphics_mise indice_joueur nb_joueurs argent all_in fold check u = (* affiche les rectangles indiquant l'action d'un joueur lorsque c'est son tour *)
(* prend en argument un u ('a) pour pouvoir stocker les fonctions dans un array *)
	if indice_joueur != 7 then 
		let l = list_coord_carte nb_joueurs in
		let (x, y) = l.(indice_joueur) in
		set_color white;
		fill_rect (x+10) (y-25) 52 15;
		set_color black; 
		draw_rect (x+10) (y-25) 52 15;
		set_color black; 
		set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
		if all_in = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto (x+8) (y-25); draw_string " All in !")
		else if fold = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto (x+24) (y-25); draw_string "FOLD")
		else if check = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto (x+22) (y-25); draw_string "CHECK")
		else (let d = (string_of_int argent) ^" $" in let h = String.length d in (moveto (x+26-h-1) (y-25); draw_string d))
	else 
		(set_color white;
		fill_rect 574 160 52 15;
		set_color black; 
		draw_rect 574 160 52 15;
		set_color black; 
		set_font "-*-fixed-medium-r-semicondensed--15-*-*-*-*-*-iso8859-1";
		if all_in = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto 572 160; draw_string " All in !")
		else if fold = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto 588 160; draw_string "FOLD")
		else if check = true then (set_font "-*-fixed-medium-r-semicondensed--13-*-*-*-*-*-iso8859-1"; moveto 586 160; draw_string "CHECK")
		else (let d = (string_of_int argent) ^" $" in let h = String.length d in (moveto (590-h-1) 160; draw_string d)));;

let graphics_mise_vide indice_joueur nb_joueurs = (* pour réinitialiser l'affichage des actions des joueurs *)
	if indice_joueur != 7 then 
		let l = list_coord_carte nb_joueurs in
		let (x, y) = l.(indice_joueur) in
		set_color white;
		fill_rect (x+10) (y-25) 52 15;
		set_color black; 
		draw_rect (x+10) (y-25) 52 15;
	else 
		set_color white;
		fill_rect 574 160 52 15;
		set_color black; 
		draw_rect 574 160 52 15;;

let reset_graphics_mise jeu = (* réinitialisation des actions pour plus de lisibilté sur la table *)
	attente 2.;
	for i = 0 to (nb_joueurs jeu)-1 do (if jeu.joueurs.(i).etat = 1 then graphics_mise_vide i (nb_joueurs jeu)) done;;
	
(* les quatre fonctions suivantes sont les fonctions graphiques des bouttons *)		
let button_check x y = 
	set_color (rgb 153 153 102);
	fill_rect x y 200 50;
	set_color black;
	draw_rect x y 200 50;
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	moveto (x+8) (y+12);
	draw_string "[Space] - Check" ;;
	
let button_call x y = 
	set_color (rgb 153 153 102);
	fill_rect x y 200 50;
	set_color black;
	draw_rect x y 200 50;
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	moveto (x+40) (y+12);
	draw_string "[C] - Call" ;;

let button_raise x y = 
	set_color (rgb 153 153 102);
	fill_rect x y 200 50;
	set_color black;
	draw_rect x y 200 50;
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	moveto (x+35) (y+12);
	draw_string "[R] - Raise" ;;

let button_fold x y = 
	set_color (rgb 153 153 102);
	fill_rect x y 200 50;
	set_color black;
	draw_rect x y 200 50;
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	moveto (x+40) (y+12);
	draw_string "[F] - Fold" ;;

let click pos button = (* gestion d'un click sur un bouton, en indiquant la position de la souris au moment du click et d'un bouton (couple de couple de coordonées et de couple de dimensions *)
	let (x,y) = pos in 
	let ((coord_x, coord_y), (l_x, l_y)) = button in 
	let etat = ref false in
	if (x >= coord_x && x <= (coord_x + l_x) && y >= coord_y && y <= (coord_y + l_y)) then etat := true;
	!etat;;

let button_key_raise key = (* gestion de l'utilisation des touches dans la fenêtre de surenchère *)
	let act = { a_in = false; double = false; pot = false; half = false; cancel = false} in 
	if key = 'a' then (act.a_in <- true) 
	else if key = 'd' then (act.double <- true)
	else if key = 'p' then (act.pot <- true)
	else if key = 'h' then (act.half <- true)
	else if key = 'c' then (act.cancel <- true);
	act;;

let button_mouse_raise pos = (* gestion de l'utilisation de la souris dans la fenêtre de surenchère *)
	let act = { a_in = false; double = false; pot = false; half = false; cancel = false} in 
	if click pos ((400, 260), (195, 75)) then (act.a_in <- true)
	else if click pos ((400, 175), (195, 75)) then (act.pot <- true)
	else if click pos ((605, 260), (195, 75)) then (act.double <- true)
	else if click pos ((605, 175), (195, 75)) then (act.half <- true)
	else if click pos ((525, 110), (150, 50)) then (act.cancel <- true);
	act;;

let raise_window x y joueur jeu = (* graphisme de la fenêtre de surenchère *)
	set_color black;
	fill_rect (x-5) (y-5) 510 510;
	set_color (rgb 223 215 215);
	fill_rect x y 500 500;
	set_color (rgb 198 190 190);
	fill_rect (x+50) (y+400) 400 75;
	fill_rect (x+50) (y+160) 195 75;
	fill_rect (x+50) (y+75) 195 75;
	fill_rect (x+255) (y+160) 195 75;
	fill_rect (x+255) (y+75) 195 75;
	fill_rect (x+175) (y+10) 150 50;
	set_color black;
	draw_rect (x+50) (y+160) 195 75;
	draw_rect (x+50) (y+75) 195 75;
	draw_rect (x+255) (y+160) 195 75;
	draw_rect (x+255) (y+75) 195 75;
	draw_rect (x+175) (y+10) 150 50;
	set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
	moveto (x+135) (y+410);
	draw_string "Raise Menu";
	moveto (x+80) (y+330);
	set_font "-*-fixed-medium-r-semicondensed--30-*-*-*-*-*-iso8859-1";
	draw_string ("Your money  ->   " ^ (string_of_int (joueur.argent)) ^ " $");
	moveto (x+80) (y+280);
	draw_string ("Pot         ->   " ^ (string_of_int (jeu.mise_table)) ^ " $");
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	moveto (x+75) (y+200);
	draw_string "[A] - All in ";
	let d1 = String.length (string_of_int (joueur.argent)) in 
	moveto (x+110+((5-d1)*5)) (y+170); (* + 60 + taille caractère = 5  *)
	draw_string ((string_of_int (joueur.argent)) ^ " $");
	moveto (x+280) (y+200);
	draw_string "[D] - Double ";
	let d2 = String.length (string_of_int (2*(jeu.blinds.montant))) in
	moveto (x+330+((5-d2)*5)) (y+170);
	draw_string ((string_of_int (2*(jeu.blinds.montant))) ^ " $");
	moveto (x+95) (y+115);
	draw_string "[P] - Pot ";
	let d3 = String.length (string_of_int (jeu.blinds.montant)) in
	moveto (x+120+((5-d3)*5)) (y+85);
	draw_string ((string_of_int (jeu.blinds.montant)) ^ " $");
	moveto (x+295) (y+115);
	draw_string "[H] - Half ";
	let d4 = String.length (string_of_int ((jeu.blinds.montant)/2)) in
	moveto (x+330+((5-d4)*5)) (y+85);
	draw_string ((string_of_int ((jeu.blinds.montant)/2)) ^ " $");
	set_font "-*-fixed-medium-r-semicondensed--20-*-*-*-*-*-iso8859-1";
	moveto (x+190) (y+25);
	draw_string "[C] - Cancel";;

let affiche_combinaison_gagnante score = (* graphisme lors de la résoltuon en fin de manche *)
	set_color (rgb 223 215 215);
	fill_rect 473 148 254 104;
	set_color (rgb 198 190 190);
	fill_rect 475 150 250 100;
	set_color black;
	draw_rect 473 148 254 104;
	set_font "-*-fixed-medium-r-semicondensed--50-*-*-*-*-*-iso8859-1";
	moveto 480 160;
	if score = 579 then draw_string "Royal Flush"
	else if score > 569 then draw_string "Straight Flush"
	else if score > 556 then draw_string "Four of a Kind"
	else if score > 387 then draw_string "Full House"
	else if score > 374 then draw_string "Flush"
	else if score > 364 then draw_string "Straight"
	else if score > 351 then draw_string "Three of a Kind"
	else if score > 182 then draw_string "Double Pair"
	else if score > 13 then draw_string "Pair"
	else draw_string "High Card" ;;

let affiche_carte jeu b = (* affiche toutes les cartes de la table (deck, mains, cartes posees, etc... ), affiche celle du joueur Humain si carte_humain = true  *)
	let m = jeu.joueurs.(7).main in
	if (b = true && m.(0).valeur != 0) then (carte (m.(0).couleur) (m.(0).valeur) 20. (-200.) 0.75; carte (m.(1).couleur) (m.(1).valeur) 215. (-200.) 0.75);
	dos_carte_table 400 300;
	if Array.length jeu.c_defaussees != 0 then dos_carte_table 400 195;
	let c = [| 470.; 540.; 610.; 680.; 750. |] in
	if Array.length jeu.c_posees != 0 then 
		for i=0 to (Array.length jeu.c_posees)-1 do (if jeu.c_posees.(i).valeur != 0 then carte (jeu.c_posees.(i).couleur) (jeu.c_posees.(i).valeur) (c.(i)) 250. 0.25 )done;
	let l1 = joueurs_actifs jeu in 
	let l2 = list_coord_carte (nb_joueurs jeu) in
	let d = Array.length l1 in
	if d != 1 then (for j=0 to d-2 do (if l1.(j) != 7 then let (x,y) = l2.(l1.(j)) in (dos_carte_table x y; dos_carte_table (x+5) (y-5))) done) else let (x,y) = l2.(l1.(0)) in (dos_carte_table x y; dos_carte_table (x+5) (y-5));
	if d != 1 then (if l1.((Array.length l1)-1) != 7 then let (x,y) = l2.(l1.((Array.length l1)-2)) in (dos_carte_table x y; dos_carte_table (x+5) (y-5)));
	if d = 1 then let (x,y) = l2.(l1.(0)) in (dos_carte_table x y; dos_carte_table  (x+5)(y-5));;

let affiche_mise_table jeu = (* affiche la mise au milieu de la table *) 
	set_color (rgb 0 153 51);
	fill_rect 550 370 100 40; (* pour cacher la mise précédente *)
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	set_color white;
	moveto 550 370;
	draw_string ("Pot : " ^ (string_of_int (jeu.mise_table)) ^  " $");;

let affiche_table (jeu) (l_mises: ('a -> unit) array) carte_humain =  (* affiche tout ce qu'il y a besoin d'afficher sur la table en fonction des paramètres du jeu *)
	let borne = Array.length l_mises in 
	for i=0 to borne-1 do (l_mises.(i) ()) done;
	affiche_mise_table jeu;
	affiche_carte jeu carte_humain;;

let reveal_card jeu = (* affiche toutes les cartes des joueurs qui sont encore actifs *)
	let l = list_coord_carte (nb_joueurs jeu) in
	for i=0 to (nb_joueurs jeu)-1 do
		if (jeu.joueurs.(i).etat = 1 && i != 7) then let (x,y) = l.(i) in (carte (jeu.joueurs.(i).main.(0).couleur) (jeu.joueurs.(i).main.(0).valeur) (float_of_int x) (float_of_int y) 0.3; carte (jeu.joueurs.(i).main.(1).couleur) (jeu.joueurs.(i).main.(1).valeur) (float_of_int (x+15)) (float_of_int y) 0.3) done;;

let graphics_fold indice_joueur jeu (l_mises: ('a -> unit) array) sec = (* animation d'un joueur qui se couche *)
	if indice_joueur != 7 then
		(let l = list_coord_carte (nb_joueurs jeu) in 
		let (x, y) = l.(indice_joueur) in 
		let d_x = 400 - x and d_y = 195 - y in
		let borne = 25. *. sec in
		let h = (float_of_int d_x) /. borne and k = (float_of_int d_y) /. borne in
		(* en fonction du temps indiqué on calcule les positions des cartes pour avoir une animation de 25 images/sec *)
		for i = 1 to (int_of_float borne)-1 do
			(set_color (rgb 0 153 51); 
			if i=1 then  (fill_rect x (y-5) 67 105) 
			else fill_rect (x + (int_of_float ((float_of_int (i-1)) *. h))) (y + (int_of_float ((float_of_int (i-1)) *. k))) 67 105 ;
			(* on remplace les deux anciennes cartes par un rectangle vert de la couleur de la table *)
			dos_carte_table (x + (int_of_float ((float_of_int i) *. h))) (y + (int_of_float ((float_of_int i) *. k))) ;
			dos_carte_table (x+5 + (int_of_float ((float_of_int i) *. h))) (y+5 + (int_of_float ((float_of_int i) *. k))) ;
			(* on raffiche les deux nouvelles cartes *)
			affiche_table jeu l_mises false;
			attente 0.04;
			(* on attend un certain temps pour avoir les 25 images secondes *)
			if i = (int_of_float borne)-1 then (set_color (rgb 0 153 51); fill_rect (x + (int_of_float ((float_of_int i) *. h))) (y + (int_of_float ((float_of_int i) *. k)))) 67 105) done)
	else (set_color (rgb 96 96 96); fill_rect 0 0 415 100);
	affiche_table jeu l_mises false;;

let graphics_blinds jeu l_mise_g = (* complément d'affiche_mise mais avant les tours de mise pour indiquer les blinds *)
	let nb = nb_joueurs jeu in
	graphics_mise (jeu.blinds.big) nb (jeu.blinds.montant) false false false ();
	graphics_mise (jeu.blinds.small) nb (jeu.blinds.montant/2) false false false ();
	l_mise_g := ajoute_valeur_graphics_mise !l_mise_g (graphics_mise (jeu.blinds.big) nb (jeu.blinds.montant) false false false);
	l_mise_g := ajoute_valeur_graphics_mise !l_mise_g (graphics_mise (jeu.blinds.small) nb (jeu.blinds.montant/2) false false false);;

let act_raise jeu l_mise_g indice_joueur= (* gestion de la fenêtre de surenchère, fonction qui renvoit une action_raise *)
	raise_window 350 100 (jeu.joueurs.(7)) jeu;
	let call = ref false in 
	let amount = ref 0 in 
	let fin = ref false in 
	while !fin = false do 
		if button_down () then begin 
			let p = mouse_pos () in let a = button_mouse_raise p in
			if a.a_in = true then (call:=true; amount:= jeu.joueurs.(7).argent - jeu.joueurs.(7).mise; fin:= true)
			else if a.double = true then (call:=true; amount:= (jeu.blinds.montant)*2 - jeu.joueurs.(7).mise; fin:= true)
			else if a.pot = true then (call:=true; amount:= jeu.blinds.montant - jeu.joueurs.(7).mise; fin:= true)
			else if a.half = true then (call:=true; amount:= (jeu.blinds.montant)/2 - jeu.joueurs.(7).mise; fin:= true)
			else if a.cancel = true then fin:= true
		end;
		if  key_pressed () then begin
			let k = read_key () in let a = button_key_raise k in 
			if a.a_in = true then (call:=true; amount:= jeu.joueurs.(7).argent - jeu.joueurs.(7).mise; fin:= true)
			else if a.double = true then (call:=true; amount:= (jeu.blinds.montant)*2 - jeu.joueurs.(7).mise; fin:= true)
			else if a.pot = true then (call:=true; amount:= jeu.blinds.montant - jeu.joueurs.(7).mise; fin:= true)
			else if a.half = true then (call:=true; amount:= (jeu.blinds.montant)/2 - jeu.joueurs.(7).mise; fin:= true)
			else if a.cancel = true then fin:= true
		end 
		done;
		back ();
		button_fold 790 60;
		button_call 995 5;
		button_raise 790 5;
		button_check 995 60; 
		affiche_table jeu l_mise_g true;
		graphics_player jeu indice_joueur;
		(!call, !amount);;

let button_mouse pos jeu l_mise_g indice_joueur= (* gestion de l'utilisation de la souris ppur le tour de mise *)
	let act = {fold = false; check = false; call = false; raise = false; all_in = false; amount = 0} in
	if click pos ((790, 60), (200,50)) then act.fold <- true
	else if click pos ((995, 60), (200, 50)) then act.check <- true
	else if click pos ((995, 5), (200, 50)) then (act.call <- true; act.amount <- jeu.mise_tour - jeu.joueurs.(indice_joueur).mise)
	else if click pos ((790, 5), (200, 50)) then (let (c, a) = act_raise jeu l_mise_g indice_joueur in act.raise <- c; act.amount <- a);
	act;;

let button_key key jeu l_mise_g indice_joueur= (* gestion de l'utilisation des touches ppur le tour de mise *)
	let act = {fold = false; check = false; call = false; raise = false; all_in = false; amount = 0} in
	if key = 'f' then act.fold <- true
	else if key = ' ' then act.check <- true
	else if key = 'c' then (act.call <- true; act.amount <- jeu.mise_tour - jeu.joueurs.(indice_joueur).mise) 
	else if key = 'r' then (let (c, a) = act_raise jeu l_mise_g indice_joueur in act.raise <- c; act.amount <- a);
	act;;

let next_event jeu indice_joueur l_mise_g = (* gestion du tour de mise pour le joueur humain *)
	let fin = ref false in
	let temps = (time ()) in
	let act = ref {fold = false; check = false; call = false; raise = false; all_in = false; amount = 0} in
	while !fin = false do begin
	(* soit le joueur joue à la souris *)
		if button_down () then begin
			let p = mouse_pos () in let a = button_mouse p jeu l_mise_g indice_joueur in 
			if (a.fold = true || a.check = true || a.call = true || a.raise = true) then 
				if jeu.mise_tour != 0 then 
				(* on établit des conditions dans les actions, un joueur ne pas appeler une mise s'il n'y a pas de mise etc... *)
					(if (a.fold = true || a.raise = true || (jeu.joueurs.(indice_joueur).mise != jeu.mise_tour &&  a.call = true) || (jeu.joueurs.(indice_joueur).mise = jeu.mise_tour &&  a.check = true)) then (fin:= true; act := a))
				else if jeu.mise_tour = 0 then 
					(if (a.fold = true || a.raise = true || a.check = true) then (fin:= true; act := a));
				graphics_player jeu indice_joueur;
				affiche_table jeu l_mise_g true
		end;
		(* soit le joueur joue au clavier *)
		if key_pressed () then begin
			let k = read_key () in let a = button_key k jeu l_mise_g indice_joueur in
			if (a.fold = true || a.check = true || a.call = true || a.raise = true) then 
				if jeu.mise_tour != 0 then 
				(* on établit des conditions dans les actions, un joueur ne pas appeler une mise s'il n'y a pas de mise etc... *)
					(if (a.fold = true || a.raise = true ||(jeu.joueurs.(indice_joueur).mise != jeu.mise_tour &&  a.call = true) || (jeu.joueurs.(indice_joueur).mise = jeu.mise_tour &&  a.check = true)) then (fin:= true; act := a))
				else if jeu.mise_tour = 0 then 
					(if (a.fold = true || a.raise = true || a.check = true) then (fin:= true; act := a));
				graphics_player jeu indice_joueur;
				affiche_table jeu l_mise_g true
		end;
	if(time () -. temps) >= 120.  then (fin:= true; act := {fold = true; check = false; call = false; raise = false; all_in = false; amount = 0});
	end done; 
	!act;;

let action joueur jeu indice_joueur l_mise_g = (* sépare les actions d'un joueur humaind'un joueur artificiel *)
	let act = ref {fold = false; check = false; call = false; raise = false; all_in = false; amount = 0} in
	if joueur.clef = 'H' then act:= next_event jeu indice_joueur l_mise_g else 
	if joueur.clef = 'A' then act := computer jeu indice_joueur;
	!act;;

let graphics_carte_mvt jeu indice_joueur depart arrive sec s_carte= (* gestion de l'animation d'un déplacement de carte*)
	let l = list_coord_carte (Array.length jeu.joueurs) in
	let l2 = joueurs_actifs jeu in 
	let (x1, y1) = depart in 
	let (x2, y2) = arrive in
	let d_x = x2 - x1 and d_y = y2 - y1 in  
	let borne = 25. *. sec in
	let h = (float_of_int d_x) /. borne and k = (float_of_int d_y) /. borne in
	if indice_joueur != 7 then 
	(* fonctionne de la même manière que la fonction d'animation d'un joueur qui se couche *)
		for i = 1 to (int_of_float borne)-1 do begin
			set_color (rgb 0 153 51);
			fill_rect (x1 + (int_of_float ((float_of_int (i-1)) *. h))) (y1 + (int_of_float ((float_of_int (i-1)) *. k))) 62 100;
			(* on remplace l'ancienne carte par un rectangle vert de la couleur de la table *)
			let j = ref 0 in 
			while (!j <= 6 && l2.(!j) < indice_joueur) do (let (x, y) = l.(l2.(!j)) in (dos_carte_table x y; dos_carte_table (x+5) (y-5); j:= !j +1)) done;
			dos_carte_table (x1 + (int_of_float ((float_of_int i) *. h))) (y1 + (int_of_float ((float_of_int i) *. k)));
			dos_carte_table 400 300;
			(* on replace les cartes nécessaire et affiche la carte à sa nouvelle position pour l'animation *)
			attente 0.04;
			if i = (int_of_float borne)-1 then (set_color (rgb 0 153 51); fill_rect (x1 + (int_of_float ((float_of_int i) *. h))) (y1 + (int_of_float ((float_of_int i) *. k)))) 62 100; 
		end; done
	else (let n = ref 0 in if s_carte = true then  n:=1; let main = jeu.joueurs.(7).main in (carte (main.(!n).couleur) (main.(!n).valeur) (float_of_int x2) (float_of_int y2) 0.75));
	if s_carte = false then dos_carte_table x2 y2;;
	
let graphics_distribution jeu = (* fait une double animation de cartes pour faire l'animation de distribution *)
	let nb = Array.length jeu.joueurs in
	let l1 = list_coord_carte nb in 
	let l2 = joueurs_actifs jeu in 
	let borne = Array.length l2 in 
	for i = 0 to borne-1 do 
		if l2.(i) != 7 then 
		let (x,y) = l1.(l2.(i)) in (graphics_carte_mvt jeu (l2.(i)) (400, 300) l1.(l2.(i)) 0.5 false ; graphics_carte_mvt jeu (l2.(i)) (400, 300) (x+5, y+5) 0.5 true)
		else (graphics_carte_mvt jeu 7 (400, 300) (20, -200) 0.5 false; graphics_carte_mvt jeu 7 (400, 300) (215, -200) 0.5 true)
		done;
	graphics_main nb;;

(* ###################################### Gestion Globale ###################################### *)

let tour_mise jeu l_mise_g = (* fonction de gestion d'une manche *)
	let j_actifs = joueurs_actifs jeu in (* seuls les joueurs actifs peuvent jouer *)
	let borne = ref (Array.length j_actifs) in
	let i = ref ((indice_array (jeu.blinds.big) j_actifs)+1) in (* le joueur ayant la big blind est le premier joueur *)
	let compteur = ref 0 in
	let finit = ref false in
	let once = ref true in 
	while !finit = false do
		let j = jeu.joueurs.(j_actifs.(!i mod !borne)) in (* comme on peut surenchérir, il y a la possibiloté de parcourir plusieurs fois le tableau des joueurs *)
		if nb_joueurs_actifs jeu != 1 && j.etat != 2 then (* si un joueur est couché il ne peut pas jouer *)
			if  !once = true then graphics_player jeu (j_actifs.(!i mod !borne)); (* on affiche qu'une seule fois les graphismes des joueurs pour éviter d'avoir trop d'affichage *)
			once:= false;
			let a = action j jeu (j_actifs.(!i mod !borne)) !l_mise_g in begin (* on récupère un type action *)
				j.mise <- a.amount + j.mise; 
				jeu.joueurs.(j_actifs.(!i mod !borne)).argent <- jeu.joueurs.(j_actifs.(!i mod !borne)).argent - a.amount; (* l'argent est réactualisé *)
				if a.raise = true then begin
				(* un tour complet doit être à nouveau effectué *)
					compteur:= 0;
					jeu.mise_tour <- j.mise;
					if a.all_in = true then 
					(* on affiche le nécessaire et on enregistre la fonction d'affichage de mise dans notre array l_mise_g*)
					(j.etat <- 2; borne:= Array.length (joueurs_actifs jeu); graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) (j.mise) true false false (); l_mise_g := ajoute_valeur_graphics_mise (!l_mise_g) (graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) (j.mise) true false false))
					else graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) (j.mise) false false false () ;l_mise_g := ajoute_valeur_graphics_mise (!l_mise_g) (graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) (j.mise) false false false);
					(* on a aussi ré-évalué la borne pour travailler modulo le nombre de joueur actifs *)
					once:=true;
					end
				else if a.call = true then begin
				(* on affiche de même le nécessaire*) 
					graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) jeu.mise_tour false false false (); l_mise_g := ajoute_valeur_graphics_mise (!l_mise_g) (graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) jeu.mise_tour false false false);
					compteur:= !compteur + 1;
					once:=true;
					end
				else if a.check = true then (compteur:= !compteur + 1; graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) 0 false false true (); l_mise_g := ajoute_valeur_graphics_mise (!l_mise_g) (graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) 0 false false true); once:=true)
				else if a.fold = true then (compteur:= !compteur + 1; jeu.mise_table <- jeu.mise_table + jeu.joueurs.(j_actifs.(!i mod !borne)).mise; jeu.joueurs.(j_actifs.(!i mod !borne)).mise <-0; fold_joueur jeu (j_actifs.(!i mod !borne)); graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) 0 false true false (); l_mise_g := ajoute_valeur_graphics_mise (!l_mise_g) (graphics_mise (j_actifs.(!i mod !borne)) (nb_joueurs jeu) 0 false true false); graphics_fold (!i mod !borne) jeu (!l_mise_g) 0.5; once:=true);
				(* on établit les conditions d'arrêt de tour de mise  *)
				if !compteur >= !borne then (finit:= true); 
				if (nb_joueurs_actifs jeu) = 1 then (finit:= true);
				graphics_player jeu (j_actifs.(!i mod !borne));
				affiche_table jeu !l_mise_g true;
				i:= !i +1
			end; 
	done;
	(* fin de gestion de l'argent*)
	for k=0 to (nb_joueurs jeu)-1 do (jeu.mise_table <- jeu.mise_table + jeu.joueurs.(k).mise; jeu.joueurs.(k).mise <- 0) done;
	jeu.mise_tour <- 0;;

let showdown jeu = (* à la fin d'une manche, pour chacun des joueurs encore actifs, on calcul leur score, et en appliquant les fonctions précédentes,
on obtient un array des gagants, en parcourant ce dernier on partage la mise totale de la manche entre le ou les joueurs et on réinitilialise la mise totale *)
	reveal_card jeu;
	let l = list_coord (nb_joueurs jeu) in
	let j = joueurs_actifs jeu in  
	let borne = Array.length j in
	let score = Array.make borne (0, 0) in 
	for i=0 to borne-1 do score.(i) <- (j.(i), (points jeu.joueurs.(j.(i)) jeu)) done; (* on attribue à tous les joueurs encore actifs un score *)
	(* on compare les scores et détermine un/des gagant(s) *)
	let v_max = (valeur_score_max score) in 
	let d = (nombre_gagnant v_max score) in
	let g = (gagnants v_max score) in 
	(* on affiche le graphisme nécessaires *)
	(affiche_combinaison_gagnante v_max; for k=0 to d-1 do (jeu.joueurs.(g.(k)).argent <- jeu.joueurs.(g.(k)).argent + (jeu.mise_table)/d; g_player_winner l.(g.(k)).(0) l.(g.(k)).(1)) done);
	attente 5.;
	jeu.mise_table <- 0;;

let start nb_joueur =
	open_graph "";
	resize_window 1200 720;
	(* on initialise les paramètres de jeu *)
	let t = tab_joueurs nb_joueur in
	let p = shuffle(creer_deck ()) in
	let b = {big = 1; small = 0; montant = 50} in
	let f x = () in 
	let l_mise_g = ref [| f |] in 
	let j = { joueurs = t; pack = p; c_posees = tas_vide 5 ; c_defaussees = tas_vide 52; mise_table = 0; mise_tour = 0; blinds = b} in
	let continue = ref true in
	let count = ref 0 in
	while !continue = true do begin 
	(* on initialise les graphsime du fopnd d'écran *)
	back ();
	button_fold 790 60;
	button_call 995 5;
	button_raise 790 5;
	button_check 995 60;
	(* on affiche les joueurs, le nombre 10 est arbitraire, on ne veut pas afficher un joueur "en train de jouer"*)
	graphics_player j 10;
	count:= !count + 1;
	distribue j;
	graphics_distribution j;
	blinds j;
	graphics_blinds j l_mise_g;
	tour_mise j l_mise_g;
	if fin_manche j then unique_joueur j 
	else (brule_carte j; flop j; affiche_table j !l_mise_g true; reset_graphics_mise j; l_mise_g:= [| f |];  tour_mise j l_mise_g; graphics_player j 10; if fin_manche j then unique_joueur j 
		else (brule_carte j ; turn j; affiche_table j !l_mise_g true; reset_graphics_mise j; l_mise_g:= [| f |]; tour_mise j l_mise_g; graphics_player j 10; if fin_manche j then unique_joueur j 
			else (brule_carte j ; river j; affiche_table j !l_mise_g true; reset_graphics_mise j; l_mise_g:= [| f |]; tour_mise j l_mise_g; graphics_player j 10; if fin_manche j then unique_joueur j else showdown j)));
	if nb_joueurs j != 1 then changement_blinds j;
	count := augmente_blinds j (!count);
	j.pack <- shuffle(creer_deck ());
	j.c_posees <- tas_vide 5;
	j.c_defaussees <- tas_vide 52;
	for k=0 to (nb_joueur)-1 do (j.joueurs.(k).main <- main_vide (); if j.joueurs.(k).etat = 0 then j.joueurs.(k).etat <- 1) done;
	l_mise_g := [| f |];
	affiche_table j !l_mise_g false; 
	if fin j then continue:= false;
	if fin_joueur_humain j then continue:= false
	end
	done;;

start 8;;
(* problème fold indice joueur, problème raise *)q