/* =========================================================================
   SYSTÈME EXPERT SHERLOCK v3.0 - CODE PROLOG COMPLET
   
   Intègre: Logique des Prédicats, CSP, Arbres de Décision, Réseaux Bayésiens
   L'utilisateur saisit tout, Sherlock déduit le coupable
   ========================================================================= */

/* -------------------------------------------------------------------------
   PARTIE 1: BASE DE CONNAISSANCES DYNAMIQUE
   ------------------------------------------------------------------------- */

:- dynamic suspect/1.
:- dynamic indice/2.
:- dynamic caracteristique/3.

/* -------------------------------------------------------------------------
   PARTIE 2: ONTOLOGIE COMPLÈTE
   ------------------------------------------------------------------------- */

% === ATTRIBUTS PHYSIQUES ===
attribut_valide(pointure, V) :- number(V), V >= 30, V =< 50.
attribut_valide(taille, petite).
attribut_valide(taille, moyenne).
attribut_valide(taille, grande).
attribut_valide(corpulence, mince).
attribut_valide(corpulence, normale).
attribut_valide(corpulence, athletique).
attribut_valide(corpulence, forte).
attribut_valide(couleur_cheveux, blond).
attribut_valide(couleur_cheveux, brun).
attribut_valide(couleur_cheveux, roux).
attribut_valide(couleur_cheveux, noir).
attribut_valide(couleur_cheveux, gris).
attribut_valide(couleur_cheveux, chauve).

% === ATTRIBUTS DÉMOGRAPHIQUES ===
attribut_valide(genre, homme).
attribut_valide(genre, femme).
attribut_valide(age, jeune).
attribut_valide(age, adulte).
attribut_valide(age, senior).
attribut_valide(groupe_sanguin, a_positif).
attribut_valide(groupe_sanguin, a_negatif).
attribut_valide(groupe_sanguin, b_positif).
attribut_valide(groupe_sanguin, b_negatif).
attribut_valide(groupe_sanguin, ab_positif).
attribut_valide(groupe_sanguin, ab_negatif).
attribut_valide(groupe_sanguin, o_positif).
attribut_valide(groupe_sanguin, o_negatif).

% === ATTRIBUTS SOCIO-ÉCONOMIQUES ===
attribut_valide(profession, sans_emploi).
attribut_valide(profession, employe).
attribut_valide(profession, cadre).
attribut_valide(profession, medecin).
attribut_valide(profession, avocat).
attribut_valide(profession, policier).
attribut_valide(profession, militaire).
attribut_valide(profession, artisan).
attribut_valide(profession, commercant).
attribut_valide(profession, infirmier).
attribut_valide(profession, pharmacien).
attribut_valide(profession, chimiste).
attribut_valide(profession, vigile).
attribut_valide(situation_financiere, precaire).
attribut_valide(situation_financiere, stable).
attribut_valide(situation_financiere, aisee).
attribut_valide(situation_financiere, riche).
attribut_valide(niveau_education, primaire).
attribut_valide(niveau_education, secondaire).
attribut_valide(niveau_education, universitaire).
attribut_valide(niveau_education, post_universitaire).

% === ATTRIBUTS PSYCHOLOGIQUES ===
attribut_valide(temperament, calme).
attribut_valide(temperament, nerveux).
attribut_valide(temperament, impulsif).
attribut_valide(temperament, violent).
attribut_valide(casier_judiciaire, vierge).
attribut_valide(casier_judiciaire, leger).
attribut_valide(casier_judiciaire, moyen).
attribut_valide(casier_judiciaire, lourd).
attribut_valide(addiction, aucune).
attribut_valide(addiction, alcool).
attribut_valide(addiction, drogue).
attribut_valide(addiction, jeu).
attribut_valide(addiction, tabac).
attribut_valide(stabilite_mentale, stable).
attribut_valide(stabilite_mentale, fragile).
attribut_valide(stabilite_mentale, instable).

% === ATTRIBUTS CONTEXTUELS ===
attribut_valide(alibi, verifie).
attribut_valide(alibi, partiel).
attribut_valide(alibi, douteux).
attribut_valide(alibi, aucun).
attribut_valide(mobile, aucun).
attribut_valide(mobile, financier).
attribut_valide(mobile, passionnel).
attribut_valide(mobile, vengeance).
attribut_valide(mobile, politique).
attribut_valide(mobile, pathologique).
attribut_valide(relation_victime, inconnu).
attribut_valide(relation_victime, famille).
attribut_valide(relation_victime, ami).
attribut_valide(relation_victime, collegue).
attribut_valide(relation_victime, rival).
attribut_valide(relation_victime, ennemi).
attribut_valide(relation_victime, ex_conjoint).
attribut_valide(proximite_scene, resident).
attribut_valide(proximite_scene, voisin).
attribut_valide(proximite_scene, passant_regulier).
attribut_valide(proximite_scene, etranger).

% === ATTRIBUTS COMPORTEMENTAUX ===
attribut_valide(habitude_nocturne, actif).
attribut_valide(habitude_nocturne, inactif).
attribut_valide(competence_informatique, aucune).
attribut_valide(competence_informatique, basique).
attribut_valide(competence_informatique, avancee).
attribut_valide(competence_informatique, expert).
attribut_valide(acces_armes, aucun).
attribut_valide(acces_armes, legal).
attribut_valide(acces_armes, illegal).
attribut_valide(formation_combat, aucune).
attribut_valide(formation_combat, basique).
attribut_valide(formation_combat, avancee).
attribut_valide(formation_combat, professionnelle).

/* -------------------------------------------------------------------------
   PARTIE 3: RÈGLES D'INFÉRENCE
   ------------------------------------------------------------------------- */

% === INFÉRENCE DE FORCE PHYSIQUE ===
inferer_force(Suspect, tres_elevee) :-
    caracteristique(Suspect, genre, homme),
    caracteristique(Suspect, age, adulte),
    caracteristique(Suspect, corpulence, athletique).

inferer_force(Suspect, elevee) :-
    caracteristique(Suspect, genre, homme),
    caracteristique(Suspect, corpulence, forte),
    \+ caracteristique(Suspect, age, senior).

inferer_force(Suspect, elevee) :-
    caracteristique(Suspect, corpulence, athletique),
    \+ caracteristique(Suspect, age, senior).

inferer_force(Suspect, moyenne) :-
    caracteristique(Suspect, genre, homme),
    caracteristique(Suspect, corpulence, normale),
    caracteristique(Suspect, age, adulte).

inferer_force(Suspect, faible) :-
    caracteristique(Suspect, age, senior).

inferer_force(Suspect, faible) :-
    caracteristique(Suspect, corpulence, mince).

inferer_force(_, moyenne).  % Force par défaut

% === COMPATIBILITÉ INDICES PHYSIQUES ===
compatible_empreinte(Suspect) :-
    indice(empreinte_pointure, Pointure),
    Pointure \= inconnu,
    number(Pointure),
    caracteristique(Suspect, pointure, Pointure), !.

compatible_empreinte(Suspect) :-
    indice(empreinte_pointure, inconnu), !.

compatible_empreinte(_) :-
    \+ indice(empreinte_pointure, _).

compatible_adn(Suspect) :-
    indice(adn_genre, Genre),
    Genre \= inconnu,
    caracteristique(Suspect, genre, Genre), !.

compatible_adn(Suspect) :-
    indice(adn_genre, inconnu), !.

compatible_adn(_) :-
    \+ indice(adn_genre, _).

compatible_groupe_sanguin(Suspect) :-
    indice(groupe_sanguin_scene, Groupe),
    Groupe \= inconnu,
    caracteristique(Suspect, groupe_sanguin, Groupe), !.

compatible_groupe_sanguin(Suspect) :-
    indice(groupe_sanguin_scene, inconnu), !.

compatible_groupe_sanguin(_) :-
    \+ indice(groupe_sanguin_scene, _).

compatible_cheveux(Suspect) :-
    indice(cheveux_couleur, Couleur),
    Couleur \= inconnu,
    caracteristique(Suspect, couleur_cheveux, Couleur), !.

compatible_cheveux(Suspect) :-
    indice(cheveux_couleur, inconnu), !.

compatible_cheveux(_) :-
    \+ indice(cheveux_couleur, _).

% === CLASSIFICATION DES ARMES ===
arme_lourde(hache).
arme_lourde(masse).
arme_lourde(barre_fer).
arme_lourde(marteau).

arme_tranchante(couteau).
arme_tranchante(machete).
arme_tranchante(hache).

arme_feu(pistolet).
arme_feu(revolver).
arme_feu(fusil).

arme_silencieuse(couteau).
arme_silencieuse(poison).
arme_silencieuse(strangulation).

% === RÈGLES ARMES ===
compatible_arme_lourde(Suspect) :-
    indice(arme, Arme),
    arme_lourde(Arme),
    !,
    inferer_force(Suspect, Force),
    (Force = elevee ; Force = tres_elevee).

compatible_arme_lourde(_) :-
    \+ (indice(arme, Arme), arme_lourde(Arme)).

compatible_arme_feu(Suspect) :-
    indice(arme, Arme),
    arme_feu(Arme),
    !,
    (caracteristique(Suspect, acces_armes, legal);
     caracteristique(Suspect, acces_armes, illegal);
     caracteristique(Suspect, profession, policier);
     caracteristique(Suspect, profession, militaire)).

compatible_arme_feu(_) :-
    \+ (indice(arme, Arme), arme_feu(Arme)).

compatible_poison(Suspect) :-
    indice(arme, poison),
    !,
    (caracteristique(Suspect, profession, medecin);
     caracteristique(Suspect, profession, pharmacien);
     caracteristique(Suspect, profession, chimiste)).

compatible_poison(_) :-
    \+ indice(arme, poison).

% === RÈGLES MOBILES ===
mobile_coherent(Suspect) :-
    indice(mobile_presume, Mobile),
    Mobile \= inconnu,
    !,
    (
        (Mobile = financier, mobile_financier_ok(Suspect));
        (Mobile = passionnel, mobile_passionnel_ok(Suspect));
        (Mobile = vengeance, mobile_vengeance_ok(Suspect));
        (Mobile = pathologique, mobile_pathologique_ok(Suspect))
    ).

mobile_coherent(_) :-
    \+ indice(mobile_presume, _).

mobile_coherent(_) :-
    indice(mobile_presume, inconnu).

mobile_financier_ok(Suspect) :-
    (caracteristique(Suspect, situation_financiere, precaire);
     caracteristique(Suspect, addiction, jeu);
     caracteristique(Suspect, addiction, drogue)).

mobile_passionnel_ok(Suspect) :-
    (caracteristique(Suspect, temperament, impulsif);
     caracteristique(Suspect, temperament, violent);
     caracteristique(Suspect, stabilite_mentale, instable);
     caracteristique(Suspect, relation_victime, famille);
     caracteristique(Suspect, relation_victime, ex_conjoint)).

mobile_vengeance_ok(Suspect) :-
    (caracteristique(Suspect, relation_victime, ennemi);
     caracteristique(Suspect, relation_victime, rival);
     caracteristique(Suspect, casier_judiciaire, lourd)).

mobile_pathologique_ok(Suspect) :-
    (caracteristique(Suspect, stabilite_mentale, instable);
     caracteristique(Suspect, casier_judiciaire, lourd)).

% === RÈGLES TEMPORELLES ===
compatible_horaire(Suspect) :-
    indice(heure_crime, nuit),
    !,
    (caracteristique(Suspect, habitude_nocturne, actif);
     caracteristique(Suspect, profession, vigile);
     caracteristique(Suspect, profession, infirmier)).

compatible_horaire(_) :-
    \+ indice(heure_crime, nuit).

% === RÈGLES ALIBI ===
alibi_excluant(Suspect) :-
    caracteristique(Suspect, alibi, verifie).

/* -------------------------------------------------------------------------
   PARTIE 4: CSP - CONTRAINTES
   ------------------------------------------------------------------------- */

contrainte_csp_unaire(Suspect) :-
    \+ alibi_excluant(Suspect).

contrainte_csp_globale_physique(Suspect) :-
    compatible_empreinte(Suspect),
    compatible_adn(Suspect),
    compatible_groupe_sanguin(Suspect),
    compatible_cheveux(Suspect).

contrainte_csp_globale_arme(Suspect) :-
    compatible_arme_lourde(Suspect),
    compatible_arme_feu(Suspect),
    compatible_poison(Suspect).

contrainte_csp_globale_contexte(Suspect) :-
    compatible_horaire(Suspect),
    mobile_coherent(Suspect).

solution_csp_valide(Suspect) :-
    suspect(Suspect),
    contrainte_csp_unaire(Suspect),
    contrainte_csp_globale_physique(Suspect),
    contrainte_csp_globale_arme(Suspect),
    contrainte_csp_globale_contexte(Suspect).

/* -------------------------------------------------------------------------
   PARTIE 5: ARBRE DE DÉCISION
   ------------------------------------------------------------------------- */

evaluer_arbre_decision(Suspect, Classe) :-
    (caracteristique(Suspect, alibi, verifie) ->
        Classe = tres_peu_suspect
    ;
        mobile_coherent(Suspect) ->
            (inferer_force(Suspect, Force),
             (Force = elevee ; Force = tres_elevee) ->
                (compatible_empreinte(Suspect),
                 compatible_adn(Suspect) ->
                    Classe = tres_suspect
                ;
                    Classe = suspect
                )
            ;
                Classe = peu_suspect
            )
        ;
            Classe = peu_suspect
    ).

score_arbre_decision(tres_suspect, 90).
score_arbre_decision(suspect, 70).
score_arbre_decision(peu_suspect, 40).
score_arbre_decision(tres_peu_suspect, 10).

/* -------------------------------------------------------------------------
   PARTIE 6: RÉSEAU BAYÉSIEN
   ------------------------------------------------------------------------- */

probabilite_priori(coupable, 0.1).

facteur_bayes(mobile_coherent, 3.0).
facteur_bayes(alibi_faible, 4.0).
facteur_bayes(compatibilite_physique, 5.0).
facteur_bayes(casier_lourd, 1.8).
facteur_bayes(relation_conflictuelle, 2.2).

facteur_applicable(Suspect, mobile_coherent) :-
    mobile_coherent(Suspect).

facteur_applicable(Suspect, alibi_faible) :-
    (caracteristique(Suspect, alibi, douteux);
     caracteristique(Suspect, alibi, aucun)).

facteur_applicable(Suspect, compatibilite_physique) :-
    compatible_empreinte(Suspect),
    compatible_adn(Suspect).

facteur_applicable(Suspect, casier_lourd) :-
    caracteristique(Suspect, casier_judiciaire, lourd).

facteur_applicable(Suspect, relation_conflictuelle) :-
    caracteristique(Suspect, relation_victime, Rel),
    (Rel = ennemi ; Rel = rival ; Rel = ex_conjoint).

calculer_probabilite_bayesienne(Suspect, Probabilite) :-
    probabilite_priori(coupable, P0),
    findall(Facteur, (
        facteur_applicable(Suspect, NomFacteur),
        facteur_bayes(NomFacteur, Facteur)
    ), Facteurs),
    (Facteurs = [] ->
        Probabilite = P0
    ;
        multiply_list(Facteurs, Multiplicateur),
        PTemp is P0 * Multiplicateur,
        Probabilite is min(0.99, PTemp)
    ).

multiply_list([], 1).
multiply_list([H|T], Result) :-
    multiply_list(T, R),
    Result is H * R.

/* -------------------------------------------------------------------------
   PARTIE 7: MOTEUR HYBRIDE
   ------------------------------------------------------------------------- */

coupable_hybride(Suspect, Score, Explication) :-
    suspect(Suspect),
    solution_csp_valide(Suspect),
    evaluer_arbre_decision(Suspect, Classe),
    score_arbre_decision(Classe, ScoreArbre),
    calculer_probabilite_bayesienne(Suspect, ProbaBayes),
    ScoreBayes is ProbaBayes * 100,
    Score is (ScoreArbre * 0.6 + ScoreBayes * 0.4),
    format(atom(Explication), 
           'Classe: ~w | Score Arbre: ~w | Proba Bayes: ~2f% | Score Final: ~2f',
           [Classe, ScoreArbre, ProbaBayes*100, Score]).