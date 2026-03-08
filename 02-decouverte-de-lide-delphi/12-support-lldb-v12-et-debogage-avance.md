🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.12 Support LLDB v12 et débogage avancé

## Introduction

Même le meilleur développeur écrit du code avec des bugs. C'est inévitable et c'est normal ! Ce qui distingue un bon développeur d'un développeur débutant, c'est sa capacité à **trouver et corriger** ces bugs efficacement.

C'est là qu'intervient le **débogage** (debugging en anglais). Le débogage est l'art de traquer et d'éliminer les bugs dans votre code. Et pour cela, vous avez besoin d'outils puissants. Delphi 13 Florence intègre le support de **LLDB v12**, un débogueur moderne et performant qui va transformer votre façon de déboguer.

Dans cette section, nous allons découvrir ce qu'est LLDB, pourquoi c'est une amélioration importante, et comment utiliser les outils de débogage de Delphi, des techniques de base aux techniques avancées. Même si vous êtes débutant, vous allez apprendre les fondamentaux qui vous serviront tout au long de votre carrière.

## Qu'est-ce que le débogage ?

### Définition

Le débogage est le processus qui consiste à :

**Identifier** qu'il y a un problème (bug)

**Localiser** où dans le code se trouve le problème

**Comprendre** pourquoi le problème se produit

**Corriger** le code pour éliminer le problème

**Vérifier** que la correction fonctionne et n'a pas créé de nouveaux bugs

### Types de bugs

**Bugs de syntaxe** : erreurs de frappe, oubli de point-virgule, etc. Le compilateur les détecte.

**Bugs de logique** : le code compile mais ne fait pas ce qu'il devrait faire. Exemple : vous calculez une addition au lieu d'une soustraction.

**Bugs d'exécution** : le programme plante pendant l'exécution. Exemple : division par zéro, accès à une mémoire invalide.

**Bugs de performance** : le code fonctionne mais est trop lent.

**Bugs intermittents** : bugs qui n'apparaissent que dans certaines conditions ou de manière aléatoire.

Les bugs de syntaxe sont les plus faciles à corriger. Les bugs intermittents sont les plus difficiles. Le débogage vous aide avec tous ces types.

### Méthodes de débogage

**Débogage "à l'ancienne"** : ajouter des `ShowMessage` partout pour voir ce qui se passe

```pascal
procedure CalculerTotal;  
begin  
  ShowMessage('Début du calcul');
  Total := Prix * Quantite;
  ShowMessage('Total calculé : ' + FloatToStr(Total));
  Total := Total + Frais;
  ShowMessage('Après ajout des frais : ' + FloatToStr(Total));
end;
```

**Problème** : fastidieux, encombre le code, faut tout supprimer après

**Débogage moderne** : utiliser un débogueur qui permet d'inspecter le code en temps réel, sans modifier le code source

C'est ce que nous allons apprendre !

## Qu'est-ce que LLDB ?

### Définition

**LLDB** signifie **LLVM Debugger** (Débogueur LLVM). C'est un débogueur open source développé dans le cadre du projet LLVM, un écosystème de compilation moderne.

LLDB est capable de travailler au niveau des instructions machine, ce qui lui donne une grande puissance et flexibilité.

### Pourquoi LLDB v12 dans Delphi 13 ?

Avant Delphi 13, différentes versions de Delphi utilisaient différents débogueurs selon la plateforme :

**Windows** : débogueur intégré propriétaire de Delphi

**macOS, iOS** : débogueurs spécifiques à Apple

**Android** : débogueur Android

**Linux** : débogueur GDB

Avec LLDB v12, Delphi unifie le débogage :

**Un seul débogueur** pour toutes les plateformes (ou presque)

**Plus moderne** : LLDB est activement maintenu et amélioré

**Plus puissant** : fonctionnalités avancées de débogage

**Meilleure intégration** : expérience uniforme dans l'IDE

**Performance** : plus rapide et plus stable

### Avantages de LLDB v12

**Support multi-plateforme** : déboguer sur Windows, macOS, iOS, Android, Linux avec les mêmes outils

**Débogage distant amélioré** : déboguer sur un appareil mobile ou un serveur distant plus facilement

**Inspection avancée** : examiner les structures de données complexes plus facilement

**Performance** : démarrage plus rapide, pas à pas plus fluide

**Stabilité** : moins de crashs du débogueur

**Extensibilité** : possibilité d'ajouter des scripts et plugins

## Les bases du débogage dans Delphi

Avant d'explorer LLDB v12 spécifiquement, maîtrisons les bases du débogage qui fonctionnent avec tous les débogueurs.

### Lancer en mode débogage

Pour déboguer votre application :

**Appuyez sur F9** (ou cliquez sur le bouton vert "Exécuter")

Votre application se lance en mode débogage. À première vue, cela ressemble à une exécution normale, mais en coulisse, le débogueur est actif et attend vos instructions.

**Note** : assurez-vous d'être en configuration **Debug**, pas **Release**. En Release, les informations de débogage sont supprimées.

### Les points d'arrêt (Breakpoints)

Un **point d'arrêt** est un marqueur que vous placez sur une ligne de code. Quand l'exécution atteint cette ligne, le programme se met en pause, vous permettant d'inspecter l'état.

#### Définir un point d'arrêt

**Méthode 1** : Cliquez dans la **marge gauche** de l'éditeur de code, à côté de la ligne où vous voulez vous arrêter. Un point rouge apparaît.

**Méthode 2** : Placez votre curseur sur la ligne, puis appuyez sur **F5**.

**Méthode 3** : Menu **Exécuter > Ajouter un point d'arrêt**.

#### Supprimer un point d'arrêt

**Cliquez à nouveau** sur le point rouge, ou appuyez sur **F5** avec le curseur sur la ligne.

Pour **supprimer tous les points d'arrêt** : **Exécuter > Supprimer tous les points d'arrêt**.

#### Que se passe-t-il à un point d'arrêt ?

Quand le programme atteint un point d'arrêt :

1. **L'exécution se fige** : le programme est en pause
2. **La ligne est mise en surbrillance** : vous voyez où vous êtes
3. **L'IDE bascule au premier plan** : si votre application était au premier plan
4. **Les fenêtres de débogage s'affichent** : variables, pile d'appels, etc.

Vous pouvez maintenant :
- Inspecter les variables
- Avancer pas à pas
- Modifier des valeurs
- Reprendre l'exécution

### Exécution pas à pas

Une fois le programme en pause, vous pouvez avancer ligne par ligne :

#### Pas à pas détaillé (Step Into) - F7

**Avance d'une ligne**. Si la ligne est un appel de fonction, **entre dans** la fonction.

**Exemple** :
```pascal
Total := CalculerTotal(Prix, Quantite);  // Si vous appuyez sur F7 ici
```

Vous entrez dans la fonction `CalculerTotal` et pouvez la déboguer ligne par ligne.

**Utilisez F7 quand** : vous voulez voir ce qui se passe **à l'intérieur** des fonctions appelées.

#### Pas à pas principal (Step Over) - F8

**Avance d'une ligne**. Si la ligne est un appel de fonction, **exécute la fonction** complètement sans entrer dedans.

**Exemple** :
```pascal
Total := CalculerTotal(Prix, Quantite);  // Si vous appuyez sur F8 ici
```

La fonction s'exécute, et vous passez à la ligne suivante, sans entrer dans `CalculerTotal`.

**Utilisez F8 quand** : vous faites confiance à la fonction et ne voulez pas la déboguer. C'est le plus utilisé.

#### Exécuter jusqu'au curseur (Run to Cursor) - F4

Place un point d'arrêt temporaire à la position du curseur et reprend l'exécution.

**Pratique pour** : sauter plusieurs lignes d'un coup sans mettre de point d'arrêt permanent.

#### Sortir de la fonction (Step Out) - Shift + F8

Si vous êtes dans une fonction et que vous voulez **sortir rapidement**, utilisez Shift + F8. Le programme exécute le reste de la fonction et s'arrête après le retour.

#### Continuer l'exécution (Run) - F9

Reprend l'exécution normale du programme jusqu'au prochain point d'arrêt (ou jusqu'à la fin).

### Inspecter les variables

Pendant le débogage, vous voulez voir les valeurs des variables.

#### Info-bulle

**Passez la souris** sur une variable dans le code. Une info-bulle affiche sa valeur actuelle.

**Exemple** :
```pascal
var
  Total: Double;
begin
  Total := 100.50;  // Arrêt ici avec un point d'arrêt
  // Passez la souris sur Total, vous verrez "Total: 100.50"
end;
```

Simple et rapide !

#### Fenêtre Variables locales

**Affichage** : **Affichage > Fenêtres de débogage > Variables locales** (ou généralement affichée automatiquement)

Cette fenêtre liste **toutes les variables locales** de la fonction actuelle avec leurs valeurs.

Vous voyez :
- Le nom de la variable
- Son type
- Sa valeur actuelle

Vous pouvez **développer** les structures complexes (objets, enregistrements, tableaux) pour voir leurs contenus.

#### Fenêtre Espions (Watch List)

Si vous voulez **surveiller spécifiquement** certaines variables ou expressions :

1. **Affichage > Fenêtres de débogage > Espions**
2. **Cliquez sur "Ajouter un espion"**
3. **Tapez le nom de la variable** ou une expression (comme `Prix * Quantite`)

La fenêtre Espions affiche en permanence les valeurs de ces expressions pendant le débogage.

**Pratique pour** : surveiller des variables importantes tout au long du débogage.

#### Évaluer/Modifier (Evaluate/Modify)

Pendant le débogage, vous pouvez **évaluer des expressions** ou même **modifier des valeurs** :

1. **Exécuter > Évaluer/Modifier** (ou **Ctrl + F7**)
2. **Tapez une expression** : `Total + 10`, `Length(MaChaine)`, `MaListe.Count`, etc.
3. **Cliquez sur Évaluer** : voir le résultat
4. **Pour modifier** : changez la valeur, cliquez sur Modifier

**Exemple d'utilisation** : vous voulez tester ce qui se passe si une variable a une valeur différente, sans relancer le programme.

### La pile d'appels (Call Stack)

La **pile d'appels** montre le chemin d'exécution : quelle fonction a appelé quelle fonction.

**Affichage** : **Affichage > Fenêtres de débogage > Pile d'appels**

**Exemple** :
```
FormMain.ValiderDonnees  <- Vous êtes ici  
FormMain.CalculerTotal  
FormMain.Button1Click  
```

Cela signifie : `Button1Click` a appelé `CalculerTotal`, qui a appelé `ValiderDonnees`, et vous êtes actuellement dans `ValiderDonnees`.

**Double-cliquez** sur une ligne de la pile pour voir le code à cet endroit.

**Utilité** : comprendre comment vous êtes arrivé là, surtout quand le bug se produit profondément dans plusieurs niveaux de fonctions.

### Arrêter le débogage

Pour arrêter la session de débogage :

**Exécuter > Arrêt du programme** (ou **Ctrl + F2**)

Ou simplement fermez votre application normalement.

## Fonctionnalités avancées avec LLDB v12

Maintenant que vous maîtrisez les bases, explorons les fonctionnalités plus avancées disponibles avec LLDB v12.

### Points d'arrêt conditionnels

Un **point d'arrêt conditionnel** ne s'arrête que si une condition est vraie.

**Exemple** : vous avez une boucle qui s'exécute 1000 fois, mais le bug n'apparaît que quand `i = 873`.

```pascal
for i := 0 to 1000 do  
begin  
  TraiterElement(i);  // Bug quand i = 873
end;
```

**Sans condition** : vous devriez appuyer sur F8 873 fois !

**Avec condition** :

1. **Clic droit sur le point d'arrêt** (le point rouge)
2. **Propriétés du point d'arrêt**
3. **Condition** : `i = 873`
4. **OK**

Maintenant, le programme ne s'arrête que quand `i` vaut 873.

**Conditions possibles** :
- `Compteur > 100`
- `Nom = 'Martin'`
- `Liste.Count = 0`
- `(Age >= 18) and (Pays = 'France')`

### Points d'arrêt avec compteur

Un point d'arrêt peut être configuré pour ne s'activer qu'après **N passages**.

**Exemple** : vous voulez vous arrêter à la 10ème itération d'une boucle.

1. **Clic droit sur le point d'arrêt**
2. **Propriétés**
3. **Nombre de passages** : 10

Le point d'arrêt sera ignoré les 9 premières fois, et s'activera la 10ème.

### Points d'arrêt sur exception

Vous pouvez demander au débogueur de s'arrêter quand une exception se produit, même si elle est gérée.

**Configuration** :

1. **Outils > Options > Débogueur**
2. **Exceptions à ignorer** ou **Break on exception**
3. **Cochez/décochez** les types d'exceptions

**Utile pour** : déboguer des exceptions qui sont gérées mais ne devraient pas se produire.

### Fenêtre Threads

Si votre application utilise plusieurs threads (programmation parallèle), vous pouvez voir tous les threads actifs :

**Affichage > Fenêtres de débogage > Threads**

Cette fenêtre liste :
- Tous les threads
- Leur état (en cours, en pause)
- Où ils en sont dans le code

Vous pouvez **basculer entre threads** pour déboguer chacun.

**Note pour débutants** : si vous ne faites pas de programmation multi-thread, vous ne verrez qu'un seul thread (le thread principal). C'est normal !

### Fenêtre CPU (Assembly)

Pour les développeurs avancés, la **fenêtre CPU** montre le code assembleur (instructions machine).

**Affichage > Fenêtres de débogage > CPU**

Vous voyez :
- Le code assembleur
- Les registres du processeur
- La mémoire

**Utilisé pour** : débogage de très bas niveau, optimisations extrêmes, comprendre exactement ce que fait le processeur.

**Pour débuter** : vous n'en avez probablement pas besoin. Mais c'est bon de savoir que ça existe.

### Débogage à distance

LLDB v12 améliore le **débogage distant** : déboguer une application qui tourne sur un autre appareil.

**Cas d'usage** :

**Applications mobiles** : déboguer sur un iPhone ou un smartphone Android connecté

**Applications serveur** : déboguer sur un serveur Linux distant

**Machines virtuelles** : déboguer dans une VM

**Configuration** : varie selon la plateforme cible. Généralement, vous devez :

1. **Installer les outils de débogage** sur l'appareil cible
2. **Configurer la connexion** dans Delphi (réseau, USB, etc.)
3. **Déployer l'application** en mode Debug
4. **Déboguer** comme si c'était local

Nous verrons les détails dans les chapitres sur le développement mobile et multi-plateforme.

### Inspection de structures complexes

Avec LLDB v12, inspecter des structures de données complexes est plus facile.

**Objets** : développez un objet pour voir toutes ses propriétés et champs

**Tableaux dynamiques** : voyez tous les éléments, même s'il y en a des milliers

**Listes génériques** : TList, TStringList, TObjectList affichent leurs contenus de manière lisible

**Dictionnaires** : TDictionary montre les paires clé/valeur

**Exemple** :
```pascal
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  Liste.Add('Premier');
  Liste.Add('Deuxième');
  Liste.Add('Troisième');
  // Point d'arrêt ici
```

Dans la fenêtre Variables locales, développez `Liste`, puis `Strings`, et vous verrez chaque élément individuellement.

### Éditeur d'expression avancé

L'éditeur d'expression (Évaluer/Modifier) est plus puissant avec LLDB v12.

Vous pouvez évaluer :

**Expressions complexes** : `(Prix * Quantite) - (Prix * Quantite * Remise / 100)`

**Appels de fonctions** : `CalculerTVA(MontantHT, 20)` (appelle réellement la fonction !)

**Conversions de types** : `IntToStr(Total)`

**Accès aux propriétés d'objets** : `Formulaire.Button1.Caption`

**Note** : faire appeler des fonctions dans l'évaluateur peut avoir des effets de bord (modifier des variables). Utilisez avec prudence.

### Points de trace (Tracepoints)

Un **point de trace** est comme un point d'arrêt, mais au lieu de s'arrêter, il **enregistre une information** et continue.

**Utilisation** :

1. **Créez un point d'arrêt**
2. **Clic droit > Propriétés**
3. **Actions** : "Enregistrer dans le journal", "Enregistrer la pile d'appels", etc.
4. **Cochez "Ne pas arrêter l'exécution"**

Le programme continue, mais les informations sont enregistrées dans la **fenêtre Journal d'événements**.

**Utile pour** : tracer l'exécution sans ralentir avec des arrêts, comme un `ShowMessage` mais sans modifier le code.

## Débogage de situations courantes

### Déboguer un Access Violation

Un **Access Violation** est une erreur courante : tentative d'accès à une mémoire invalide.

**Causes courantes** :

**Objet non créé** :
```pascal
var
  Liste: TStringList;
begin
  Liste.Add('Test');  // ERREUR : Liste n'a pas été créée avec .Create
end;
```

**Objet déjà libéré** :
```pascal
Liste.Free;  
Liste.Add('Test');  // ERREUR : Liste a été libérée  
```

**Pointeur nil** :
```pascal
var
  P: PInteger;
begin
  P^ := 10;  // ERREUR : P n'a pas été initialisé
end;
```

**Comment déboguer** :

1. **Notez la ligne** où se produit l'Access Violation
2. **Mettez un point d'arrêt** quelques lignes avant
3. **Exécutez pas à pas** (F8) jusqu'à l'erreur
4. **Inspectez les objets** : sont-ils nil ? ont-ils été créés ?
5. **Vérifiez** que chaque objet est créé avant utilisation et libéré après

### Déboguer une boucle infinie

Votre programme semble figé, ne répondant plus ? C'est peut-être une **boucle infinie**.

```pascal
while Condition do  
begin  
  // Si Condition reste toujours True, boucle infinie !
  FaireQuelqueChose;
end;
```

**Comment déboguer** :

1. **Pausez l'exécution** : dans Delphi, cliquez sur le bouton "Pause" ou **Exécuter > Arrêter le programme**
2. Le débogueur montre **où le programme est bloqué**
3. **Inspectez la condition** : pourquoi reste-t-elle vraie ?
4. **Vérifiez** que quelque chose dans la boucle modifie effectivement la condition

**Astuce** : ajoutez un compteur de sécurité :
```pascal
var
  Compteur: Integer;
begin
  Compteur := 0;
  while (Condition) and (Compteur < 1000) do  // Max 1000 itérations
  begin
    FaireQuelqueChose;
    Inc(Compteur);
  end;

  if Compteur >= 1000 then
    ShowMessage('Attention : boucle a atteint la limite');
end;
```

### Déboguer des valeurs incorrectes

Votre calcul donne un résultat faux, mais vous ne savez pas où est l'erreur.

**Stratégie** :

1. **Identifiez le point où la valeur est fausse** : par exemple, après `CalculerTotal`, `Total` vaut 150 au lieu de 200

2. **Remontez en arrière** : mettez un point d'arrêt au début de `CalculerTotal`

3. **Avancez pas à pas** (F8), en vérifiant après chaque ligne que les valeurs sont correctes

4. **Trouvez la ligne coupable** : celle après laquelle la valeur devient incorrecte

5. **Corrigez** : erreur de calcul ? mauvaise variable ? opérateur incorrect ?

**Exemple** :
```pascal
function CalculerTotal(Prix, Quantite: Double): Double;  
var  
  SousTotal: Double;
begin
  SousTotal := Prix * Quantite;  // Vérifiez : SousTotal correct ?
  Result := SousTotal + 10;      // Vérifiez : devrait être * 1.10 pour TVA ?
end;
```

### Déboguer des problèmes intermittents

Le bug n'apparaît que parfois ? C'est le plus difficile à déboguer.

**Causes possibles** :

**Dépendance à des données externes** : fichier, base de données, réseau

**État non initialisé** : variable dont la valeur initiale est aléatoire

**Race condition** : dans les programmes multi-thread

**Timings** : dépend de la vitesse d'exécution

**Stratégies** :

**Ajoutez des logs** : même sans débogueur, enregistrez dans un fichier ce qui se passe

**Points d'arrêt conditionnels** : s'arrêter uniquement quand une condition suspecte est détectée

**Testez systématiquement** : reproductibilité d'abord. Si vous ne pouvez pas reproduire, vous ne pouvez pas déboguer

**Simplifiez** : réduisez le code au minimum qui reproduit le bug

## Conseils et bonnes pratiques de débogage

### Commencez simple

**Ne mettez pas 50 points d'arrêt** d'un coup. Commencez par un ou deux aux endroits stratégiques.

**Utilisez F8 (Step Over) par défaut** : c'est le plus pratique. N'utilisez F7 (Step Into) que quand vous devez entrer dans une fonction.

### Comprenez avant de corriger

**Ne corrigez pas au hasard** en espérant que ça marche. Comprenez d'abord **pourquoi** il y a un bug.

Une fois que vous comprenez, la correction est évidente.

### Reproductibilité

**Essayez de reproduire le bug de manière fiable** : mêmes actions, mêmes données, même résultat.

Si le bug est aléatoire, c'est beaucoup plus difficile.

### Divisez pour régner

**Si le bug est dans une grosse fonction** :

1. Mettez un point d'arrêt au début
2. Avancez pas à pas
3. Notez à partir d'où ça ne va plus
4. Concentrez-vous sur cette partie

### Isolez le problème

**Créez un petit projet de test** qui reproduit juste le problème, sans tout le reste de votre application.

Plus le code est simple, plus il est facile de trouver le bug.

### Utilisez le contrôle de version

Avec Git, vous pouvez :

**Comparer** : qu'est-ce qui a changé depuis la dernière version qui marchait ?

**Revenir en arrière** : testez une version précédente pour voir si le bug existait

**Bisect** : Git peut automatiquement trouver quel commit a introduit le bug

### Faites des pauses

Si vous êtes bloqué depuis 1 heure, **faites une pause** !

Souvent, la solution vient quand vous ne cherchez plus activement. Votre cerveau continue de travailler en arrière-plan.

### Expliquez à quelqu'un (ou à un canard)

Technique du **rubber duck debugging** : expliquez votre code ligne par ligne à quelqu'un (ou à un canard en plastique).

Souvent, en expliquant, vous réalisez vous-même où est l'erreur !

### Lisez les messages d'erreur

Les messages d'erreur donnent des indices. Ne les ignorez pas !

**"Access Violation at address 0x00000000"** → pointeur nil

**"List index out of bounds"** → vous essayez d'accéder à un élément qui n'existe pas

**"Cannot assign to read-only property"** → vous essayez de modifier une propriété en lecture seule

Cherchez le message sur Google si vous ne le comprenez pas.

### Testez vos hypothèses

Vous pensez savoir où est le bug ? **Testez** !

Mettez un point d'arrêt, vérifiez si votre hypothèse est correcte.

Ne supposez pas, vérifiez.

## Configuration du débogueur pour LLDB v12

### Options du débogueur

**Outils > Options > Débogueur**

Vous pouvez configurer :

**Débogueur par défaut** : assurez-vous que LLDB est sélectionné (si disponible pour votre plateforme)

**Options de débogage** :
- Arrêt sur les exceptions
- Évaluation d'expression
- Timeouts

**Symboles de débogage** : où chercher les informations de débogage

**Pour débuter** : les valeurs par défaut sont généralement bonnes.

### Options de projet

**Projet > Options > Compilation**

**Informations de débogage** : doit être activé en mode Debug

**Assertions** : activez-les en Debug pour détecter les conditions anormales

**Optimisations** : désactivées en Debug (le code optimisé est plus difficile à déboguer)

### Vérifier que LLDB est actif

Dans certaines configurations, vous pouvez voir quel débogueur est utilisé :

**Affichage > Fenêtres de débogage** : si vous voyez des fenêtres spécifiques à LLDB, c'est bon signe

**Messages du débogueur** : au démarrage du débogage, l'IDE peut afficher des messages indiquant "LLDB connected" ou similaire

## Limites et alternatives

### Limitations de LLDB dans Delphi

**Toutes les plateformes ne sont pas supportées** : Windows utilise encore parfois le débogueur natif Delphi dans certaines configurations

**Courbe d'apprentissage** : LLDB a plus de fonctionnalités, donc plus à apprendre

**Compatibilité** : certains anciens projets peuvent avoir des problèmes avec le nouveau débogueur

### Retour à l'ancien débogueur

Si vous avez des problèmes avec LLDB, vous pouvez parfois revenir à l'ancien débogueur :

**Outils > Options > Débogueur > Débogueur par défaut**

Mais pour Delphi 13 et les nouvelles fonctionnalités, LLDB est recommandé.

### Débogage sans débogueur

Parfois, le débogueur ne peut pas être utilisé (problème de timing, application en production, etc.).

**Alternatives** :

**Logging** : écrire dans un fichier journal

```pascal
procedure Log(const Msg: string);  
var  
  F: TextFile;
begin
  AssignFile(F, 'debug.log');
  if FileExists('debug.log') then
    Append(F)
  else
    Rewrite(F);
  WriteLn(F, FormatDateTime('hh:nn:ss', Now) + ' - ' + Msg);
  CloseFile(F);
end;
```

**Assertions** : vérifications dans le code

```pascal
Assert(Prix > 0, 'Le prix doit être positif');
```

**Télémétrie** : dans les applications en production, envoyez des rapports de crash et d'erreurs à un serveur

**Event Viewer Windows** : votre application peut écrire dans le journal des événements Windows

## Outils complémentaires

### EurekaLog et MadExcept

Ce sont des outils tiers pour capturer et analyser les exceptions :

**Rapport de crash détaillé** : pile d'appels, état des variables, informations système

**Envoi automatique** : les rapports peuvent être envoyés au développeur

**Analyse** : outils pour analyser les rapports reçus

**Très utile** pour déboguer les crashs en production, chez les utilisateurs finaux.

### AQTime et autres profileurs

Pour le débogage de **performance** :

**AQTime** : profileur puissant pour trouver les goulots d'étranglement

**Sampling Profiler** : inclus dans certaines éditions de Delphi

**Mesure** : quelle fonction prend le plus de temps ? Où sont les fuites mémoire ?

### FastMM en mode debug

FastMM est le gestionnaire de mémoire de Delphi. En mode debug, il peut détecter :

**Fuites mémoire** : objets créés mais jamais libérés

**Double Free** : libérer un objet deux fois

**Use after free** : utiliser un objet après l'avoir libéré

Activez FastMM en mode debug pour ces vérifications supplémentaires.

## Pour aller plus loin

### Tests automatisés

Le meilleur débogage est **d'éviter les bugs** dès le départ !

**Tests unitaires** (DUnitX) : testez automatiquement vos fonctions

**Tests d'intégration** : testez que les modules fonctionnent ensemble

**Tests de régression** : assurez-vous que les corrections n'ont pas cassé autre chose

Nous verrons les tests en détail dans un chapitre ultérieur.

### Programmation défensive

Écrivez du code qui **anticipe les problèmes** :

**Vérifiez les paramètres** :
```pascal
procedure TraiterClient(Client: TClient);  
begin  
  if not Assigned(Client) then
    raise Exception.Create('Client ne peut pas être nil');
  // ...
end;
```

**Gérez les cas limites** :
```pascal
function Diviser(A, B: Integer): Integer;  
begin  
  if B = 0 then
    raise Exception.Create('Division par zéro impossible');
  Result := A div B;
end;
```

**Utilisez des blocs try-except** pour gérer les erreurs gracieusement

### Documentation et commentaires

Documentez les **hypothèses** de votre code :

```pascal
// ATTENTION : cette fonction suppose que la liste est triée
function RechercherBinaire(Liste: TList; Valeur: Integer): Integer;
```

Cela aide au débogage : si ça ne marche pas, vous savez vérifier si l'hypothèse est respectée.

## Conclusion

Le débogage est une compétence essentielle pour tout développeur. LLDB v12 dans Delphi 13 vous donne des outils puissants pour traquer et éliminer les bugs efficacement.

Points essentiels à retenir :

- **Points d'arrêt** : arrêter l'exécution pour inspecter l'état
- **Pas à pas** : F8 (step over) pour avancer ligne par ligne
- **Inspection de variables** : info-bulles, fenêtre Variables locales, Espions
- **Pile d'appels** : comprendre le chemin d'exécution
- **Points d'arrêt conditionnels** : s'arrêter seulement dans certaines conditions
- **LLDB v12** : débogueur moderne et puissant pour toutes les plateformes
- **Stratégie** : comprenez avant de corriger, divisez pour régner, isolez le problème

Le débogage est autant un art qu'une science. Plus vous déboguerez, plus vous deviendrez rapide et efficace. N'ayez pas peur des bugs : ils sont des opportunités d'apprendre et d'améliorer votre code.

Avec les outils de débogage de Delphi et LLDB v12, vous êtes équipé pour affronter n'importe quel bug et créer des applications robustes et fiables.

Dans le prochain chapitre, nous plongerons dans le langage Object Pascal pour maîtriser la syntaxe et les concepts fondamentaux de la programmation avec Delphi !

⏭️ [Langage Object Pascal](/03-langage-object-pascal/README.md)
