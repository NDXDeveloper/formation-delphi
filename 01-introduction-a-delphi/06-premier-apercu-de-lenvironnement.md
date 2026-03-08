🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.6 Premier aperçu de l'environnement

## Introduction

Lorsque vous lancez Delphi pour la première fois, vous vous retrouvez face à un environnement riche et potentiellement intimidant. Pas de panique ! Cette section vous guide à travers les différentes parties de l'interface pour que vous vous sentiez rapidement à l'aise. Vous n'avez pas besoin de tout comprendre immédiatement - considérez cette section comme une première visite guidée de votre nouvel atelier de développement.

## Lancement de Delphi

Pour démarrer Delphi :
- Double-cliquez sur l'icône **RAD Studio** sur votre bureau
- Ou allez dans le menu Démarrer > Embarcadero RAD Studio 13

**Premier lancement :**
- Delphi peut prendre quelques secondes à démarrer (c'est normal)
- Une page de bienvenue peut s'afficher avec des liens vers la documentation
- Vous pouvez fermer cette page ou cocher "Ne plus afficher" pour les prochains lancements

## Vue d'ensemble de l'interface

L'environnement de développement Delphi (IDE) est composé de plusieurs zones distinctes, chacune ayant un rôle spécifique. Imaginez l'IDE comme un grand bureau de travail avec différentes sections organisées.

### Les zones principales

Quand vous créez un nouveau projet (nous verrons comment dans la section suivante), vous voyez typiquement ces zones :

**En haut :**
- La barre de menus et les barres d'outils

**Au centre :**
- Le concepteur de formulaires (Form Designer) où vous dessinez vos interfaces
- L'éditeur de code où vous écrivez votre programme

**À gauche :**
- La palette d'outils (Tool Palette) contenant les composants à glisser-déposer

**À droite :**
- L'inspecteur d'objets (Object Inspector) pour configurer les propriétés
- L'explorateur de structure (Structure View) pour voir l'organisation

**En bas :**
- La zone des messages et informations diverses

**Important :** Cette disposition peut varier selon la configuration choisie lors de l'installation. Vous pouvez la personnaliser à tout moment.

## La barre de menus

En haut de la fenêtre principale, vous trouvez la barre de menus classique avec plusieurs entrées principales.

### Menu File (Fichier)

C'est ici que vous gérez vos projets et fichiers :
- **New :** Créer de nouveaux projets, formulaires, unités
- **Open :** Ouvrir des projets existants
- **Save / Save As :** Sauvegarder votre travail
- **Close :** Fermer le projet en cours
- **Recent Projects :** Accéder rapidement aux projets récents

**Conseil pour débutants :** Sauvegardez souvent votre travail avec Ctrl+S !

### Menu Edit (Édition)

Les commandes d'édition classiques :
- Couper, Copier, Coller
- Annuler (Undo) et Refaire (Redo)
- Rechercher et Remplacer dans le code

### Menu Search (Recherche)

Outils de recherche avancés :
- Rechercher dans les fichiers
- Aller à un numéro de ligne
- Rechercher dans l'aide

### Menu View (Affichage)

Contrôle ce qui est visible dans l'IDE :
- **Tool Windows :** Afficher ou masquer les fenêtres d'outils
- **Desktops :** Changer la disposition de l'interface
- **Toggle Form/Unit :** Basculer entre le formulaire et le code (F12)

### Menu Project (Projet)

Gestion du projet en cours :
- **Add to Project :** Ajouter des fichiers au projet
- **Options :** Configurer les paramètres du projet
- **Compile :** Compiler sans exécuter
- **Build :** Recompiler entièrement le projet

### Menu Run (Exécuter)

Pour tester votre application :
- **Run (F9) :** Compiler et lancer l'application
- **Run Without Debugging (Ctrl+Shift+F9) :** Exécuter sans débogueur
- **Step Over (F8) :** Déboguer pas à pas
- **Run to Cursor (F4) :** Exécuter jusqu'au curseur

### Menu Tools (Outils)

Accès aux outils et configurations :
- **Options :** Paramètres généraux de l'IDE
- **GetIt Package Manager :** Installer des composants supplémentaires
- **IDE Fix Pack :** Corrections et améliorations communautaires (si installé)

### Menu Help (Aide)

Documentation et support :
- **RAD Studio Documentation (F1) :** Aide contextuelle
- **Check for Updates :** Vérifier les mises à jour
- **About :** Informations sur la version

## Les barres d'outils

Sous la barre de menus, vous trouvez plusieurs barres d'outils avec des boutons pour les actions courantes.

### Barre d'outils standard

Les icônes les plus utilisées :
- **Nouveau fichier** (icône de page blanche)
- **Ouvrir** (icône de dossier)
- **Sauvegarder** (icône de disquette)
- **Sauvegarder tout** (plusieurs disquettes)

### Barre d'outils de débogage

Pour exécuter et déboguer :
- **Run (triangle vert) :** Lance votre application (F9)
- **Pause :** Met en pause l'exécution
- **Stop :** Arrête l'application
- **Step Over, Step Into :** Pour déboguer ligne par ligne

### Barre d'outils de recherche

Une zone de recherche rapide pour trouver du texte dans votre code.

**Astuce :** Vous pouvez personnaliser les barres d'outils en faisant un clic droit dessus et en choisissant "Customize".

## Le concepteur de formulaires (Form Designer)

C'est la grande zone grise/blanche au centre où apparaît une fenêtre vide quand vous créez un nouveau projet.

### À quoi ça sert ?

Le Form Designer est votre **toile de travail visuelle**. C'est ici que vous :
- Dessinez l'interface de votre application
- Placez des boutons, zones de texte, images, etc.
- Organisez visuellement vos éléments

### Comment ça fonctionne ?

**Mode conception :**
- Vous voyez votre fenêtre telle qu'elle apparaîtra à l'utilisateur
- Vous pouvez déplacer, redimensionner les composants
- Chaque modification est reflétée dans le code automatiquement

**Grille d'alignement :**
- Des petits points vous aident à aligner les éléments
- Les composants "s'accrochent" à la grille (snap to grid)
- Peut être activée ou désactivée dans les options

### Formulaire par défaut

Quand vous créez un nouveau projet VCL, vous voyez :
- Une fenêtre grise/blanche appelée "Form1"
- Elle représente la fenêtre principale de votre future application
- Vous pouvez la redimensionner en tirant sur ses bords

## L'éditeur de code (Code Editor)

L'éditeur de code est l'endroit où vous écrivez le code Object Pascal de votre application.

### Accès à l'éditeur

Pour voir le code :
- Appuyez sur **F12** pour basculer entre le formulaire et le code
- Ou allez dans View > Toggle Form/Unit
- Ou double-cliquez sur le formulaire

### Caractéristiques de l'éditeur

**Coloration syntaxique :**
- Les mots-clés du langage apparaissent en couleur (généralement en bleu)
- Les commentaires en vert
- Les chaînes de caractères en rouge
- Facilite la lecture du code

**Numéros de ligne :**
- À gauche, vous voyez les numéros de ligne
- Utile pour localiser les erreurs
- Peut être activé/désactivé dans les options

**Auto-complétion :**
- Quand vous tapez, Delphi propose des suggestions
- Appuyez sur Ctrl+Espace pour forcer l'affichage des suggestions
- Utilisez les flèches et Entrée pour choisir

**Indentation automatique :**
- Delphi indente automatiquement votre code
- Garde votre code bien structuré et lisible

**Pliage de code :**
- Des petits symboles [-] et [+] permettent de plier/déplier des sections
- Utile pour naviguer dans du code long

### Structure typique d'une unité

Quand vous regardez le code pour la première fois, vous voyez une structure comme :

```pascal
unit Unit1;

interface

uses
  // Liste des unités utilisées

type
  TForm1 = class(TForm)
    // Déclarations
  end;

var
  Form1: TForm1;

implementation

// Le code de vos procédures et fonctions

end.
```

**Ne vous inquiétez pas** si cela semble cryptique maintenant. Nous détaillerons tout cela dans les sections sur le langage Object Pascal.

## La palette d'outils (Tool Palette)

Généralement située à gauche de l'écran, la palette d'outils contient tous les composants que vous pouvez ajouter à vos formulaires.

### Organisation en catégories

Les composants sont organisés par catégories :

**Standard :**
- Composants de base (Button, Label, Edit, Memo, etc.)
- Ce que vous utiliserez le plus souvent au début

**Additional :**
- Composants supplémentaires (Image, Shape, Bevel, etc.)

**Win32 :**
- Composants Windows spécifiques (TreeView, ListView, ProgressBar, etc.)

**System :**
- Composants système (Timer, PaintBox, etc.)

**Data Access :**
- Composants pour les bases de données

**Data Controls :**
- Composants visuels liés aux données (DBGrid, DBEdit, etc.)

Et bien d'autres catégories encore...

### Utilisation de la palette

**Pour ajouter un composant à votre formulaire :**
1. Cliquez sur le composant dans la palette
2. Cliquez sur le formulaire à l'endroit où vous voulez le placer
3. Le composant apparaît et vous pouvez le déplacer/redimensionner

**Mode de recherche :**
- En haut de la palette, une zone de recherche
- Tapez le nom d'un composant pour le trouver rapidement
- Exemple : tapez "button" pour trouver tous les types de boutons

**Réorganisation :**
- Vous pouvez épingler vos composants favoris
- Créer vos propres catégories
- Clic droit sur la palette pour les options

## L'inspecteur d'objets (Object Inspector)

Situé généralement à droite, l'inspecteur d'objets est **l'un des outils les plus importants** de Delphi.

### Son rôle

L'inspecteur d'objets vous permet de :
- Voir et modifier les **propriétés** des composants
- Définir les **événements** (actions réagissant aux clics, etc.)
- Configurer visuellement sans écrire de code

### Les deux onglets

**Onglet Properties (Propriétés) :**
- Liste toutes les caractéristiques du composant sélectionné
- Par exemple, pour un bouton : Caption (texte), Width (largeur), Height (hauteur), Color (couleur), etc.
- Modifiez une valeur et le changement est immédiat dans le formulaire

**Onglet Events (Événements) :**
- Liste les événements auxquels le composant peut réagir
- Par exemple : OnClick (quand on clique), OnMouseMove (quand on bouge la souris), etc.
- Double-cliquez dans la case vide pour créer automatiquement une procédure

### Propriétés courantes

Voici quelques propriétés que vous verrez souvent :

**Name :**
- Le nom du composant dans le code
- Par défaut : Button1, Edit1, etc.
- Changez-le pour quelque chose de plus parlant : btnValider, edtNom, etc.

**Caption / Text :**
- Le texte affiché sur le composant
- Caption pour les boutons, labels, formulaires
- Text pour les zones de saisie

**Width / Height :**
- Largeur et hauteur en pixels

**Left / Top :**
- Position horizontale et verticale

**Enabled :**
- True : composant actif
- False : composant grisé et non utilisable

**Visible :**
- True : composant visible
- False : composant caché

**Font :**
- Police de caractères, taille, style
- Cliquez sur [...] pour ouvrir l'éditeur de police

### Mode d'affichage

L'inspecteur peut afficher les propriétés de deux façons :
- **Par catégorie :** Les propriétés sont regroupées par thème (Apparence, Position, Comportement, etc.)
- **Alphabétique :** Toutes les propriétés dans l'ordre alphabétique

Basculez entre les deux avec les boutons en haut de l'inspecteur.

## L'explorateur de structure (Structure View)

Généralement sous l'inspecteur d'objets, cette fenêtre montre la **hiérarchie** des composants de votre formulaire.

### À quoi ça sert ?

- Voir tous les composants du formulaire sous forme d'arborescence
- Sélectionner facilement un composant, même s'il est caché derrière un autre
- Comprendre les relations parent-enfant entre composants
- Réorganiser les composants

### Exemple de structure

```
Form1 (TForm1)
├── Panel1 (TPanel)
│   ├── Button1 (TButton)
│   └── Label1 (TLabel)
└── Edit1 (TEdit)
```

Cela montre que Panel1 contient Button1 et Label1, tandis que Edit1 est directement sur Form1.

## L'explorateur de projets (Project Manager)

Cette fenêtre (souvent à droite aussi) montre la structure de votre projet complet.

### Contenu typique

**Votre projet :**
- Nom du projet (.dproj)
- Liste des unités (fichiers .pas)
- Ressources
- Dépendances

**Plateformes cibles :**
- Windows 32-bit
- Windows 64-bit
- Autres plateformes si installées (iOS, Android, etc.)

### Utilisation

- Double-cliquez sur une unité pour l'ouvrir
- Clic droit pour les options (ajouter fichier, compiler, etc.)
- Développez/réduisez les sections avec les [+] et [-]

## La fenêtre de messages (Messages)

En bas de l'IDE, vous trouvez plusieurs onglets d'information.

### Messages (Messages)

Affiche les messages du compilateur :
- **Erreurs :** En rouge, empêchent la compilation
- **Avertissements :** En jaune, points à vérifier
- **Conseils :** Suggestions d'amélioration

Double-cliquez sur un message pour aller directement à la ligne concernée dans le code.

### Tool Output

Informations détaillées sur la compilation :
- Fichiers compilés
- Temps de compilation
- Taille de l'exécutable

### Call Stack

Lors du débogage, montre la pile des appels de fonctions en cours.

### Local Variables

Lors du débogage, montre les variables locales et leurs valeurs.

### Watches

Lors du débogage, permet de surveiller des variables spécifiques.

### Event Log

Journal des événements de l'IDE :
- Ouverture/fermeture de projets
- Actions effectuées
- Erreurs système

## Raccourcis clavier essentiels

Voici les raccourcis les plus utiles à connaître dès le début :

**Navigation :**
- **F12 :** Basculer entre formulaire et code
- **Ctrl+F12 :** Liste des formulaires
- **Shift+F12 :** Liste des unités
- **F11 :** Inspecteur d'objets

**Édition :**
- **Ctrl+S :** Sauvegarder
- **Ctrl+Shift+S :** Sauvegarder tout
- **Ctrl+C / Ctrl+V :** Copier / Coller
- **Ctrl+Z :** Annuler
- **Ctrl+Shift+Z :** Refaire

**Code :**
- **Ctrl+Espace :** Auto-complétion
- **Ctrl+Shift+C :** Complétion de classe
- **Ctrl+Click :** Aller à la déclaration
- **Alt+Flèche Gauche/Droite :** Naviguer dans l'historique

**Compilation et exécution :**
- **F9 :** Compiler et exécuter
- **Ctrl+F9 :** Compiler sans exécuter
- **Shift+F9 :** Compiler le projet
- **Ctrl+F2 :** Arrêter l'exécution

**Recherche :**
- **Ctrl+F :** Rechercher
- **Ctrl+H :** Remplacer
- **F3 :** Rechercher suivant

**Débogage :**
- **F5 :** Placer/retirer un point d'arrêt
- **F7 :** Trace Into (entrer dans la fonction)
- **F8 :** Step Over (passer à la ligne suivante)

**Affichage :**
- **F11 :** Afficher l'inspecteur d'objets
- **Shift+Alt+F11 :** Palette d'outils

## Personnalisation de l'environnement

Delphi est hautement personnalisable.

### Dispositions (Desktops)

Delphi propose des dispositions prédéfinies :
- **View > Desktops > Classic Undocked :** Style Delphi 7 (fenêtres séparées)
- **View > Desktops > Default Layout :** Style moderne (tout intégré)
- **View > Desktops > Debug Layout :** Optimisé pour le débogage

Vous pouvez créer vos propres dispositions et les sauvegarder.

### Thèmes visuels

**Pour changer le thème :**
- Tools > Options > User Interface > IDE
- Choisissez entre Light (clair) et Dark (sombre)
- Appliquez et redémarrez l'IDE

### Taille des polices

**Pour ajuster la taille du texte :**
- Tools > Options > Editor Options > Display
- Modifiez "Font" et "Size"
- Prévisualisez avant d'appliquer

### Positionnement des fenêtres

Vous pouvez :
- Déplacer les fenêtres en les tirant par leur barre de titre
- Les ancrer en les faisant glisser vers les bords
- Les rendre flottantes en les éloignant
- Les masquer/afficher via le menu View

## Conseils pour les débutants

### Ne vous laissez pas submerger

L'IDE Delphi est riche en fonctionnalités. **C'est normal de ne pas tout comprendre au début**. Concentrez-vous sur :
- Le concepteur de formulaires
- L'inspecteur d'objets
- La palette d'outils
- L'éditeur de code basique

Le reste viendra progressivement avec la pratique.

### Explorez sans crainte

Vous ne risquez pas de "casser" Delphi en explorant l'interface. N'hésitez pas à :
- Cliquer sur les menus
- Ouvrir les fenêtres
- Essayer différentes dispositions
- Vous pouvez toujours revenir aux paramètres par défaut

### Utilisez l'aide (F1)

Delphi inclut une aide contextuelle complète :
- Placez le curseur sur un mot dans le code
- Appuyez sur F1
- La documentation s'ouvre sur le sujet concerné

### Gardez votre espace de travail organisé

Au début, gardez une disposition simple :
- Formulaire et code au centre
- Palette d'outils à gauche
- Inspecteur d'objets à droite
- Messages en bas

Plus tard, vous personnaliserez selon vos préférences.

### Apprenez les raccourcis progressivement

Commencez avec les raccourcis de base :
- F9 pour exécuter
- F12 pour basculer formulaire/code
- Ctrl+S pour sauvegarder

Vous en apprendrez d'autres naturellement avec le temps.

## Comparaison avec d'autres IDE

Si vous avez déjà utilisé d'autres environnements de développement :

**Visual Studio :**
- Structure similaire avec des zones ancrables
- Delphi est plus simple et moins "lourd"
- L'inspecteur d'objets est comparable à la fenêtre Propriétés

**Eclipse / IntelliJ :**
- Concept de perspectives similaire aux desktops de Delphi
- Delphi est plus orienté visuel pour les interfaces
- Moins d'extensions mais plus intégré

**Visual Basic (classique) :**
- Très similaire dans l'approche RAD
- Delphi est plus puissant et moderne
- Le principe du concepteur de formulaires est identique

## Récapitulatif des zones principales

Pour terminer, voici un résumé des zones que vous utiliserez le plus souvent :

| Zone | Utilisation | Emplacement typique |
|------|-------------|---------------------|
| Form Designer | Dessiner l'interface | Centre |
| Code Editor | Écrire le code | Centre (F12 pour basculer) |
| Tool Palette | Choisir des composants | Gauche |
| Object Inspector | Configurer les propriétés | Droite |
| Project Manager | Gérer les fichiers du projet | Droite |
| Messages | Voir les erreurs de compilation | Bas |
| Menu Bar | Accéder à toutes les fonctions | Haut |
| Toolbars | Accès rapide aux actions courantes | Haut |

## En résumé

L'environnement de développement Delphi peut sembler complexe au premier abord, mais il est en réalité très logique et bien organisé. Chaque zone a un rôle précis, et une fois que vous comprenez cette organisation, vous naviguez facilement dans l'IDE.

Les trois zones les plus importantes pour commencer sont :
1. **Le concepteur de formulaires** - où vous créez vos interfaces
2. **L'inspecteur d'objets** - où vous configurez vos composants
3. **L'éditeur de code** - où vous écrivez votre logique

Prenez le temps de vous familiariser avec l'interface en créant quelques projets simples. Plus vous utiliserez Delphi, plus l'environnement vous semblera naturel et intuitif.

Dans la section suivante, nous créerons ensemble votre premier projet et vous verrez concrètement comment toutes ces zones fonctionnent ensemble !

⏭️ [Comparaison avec d'autres environnements de développement](/01-introduction-a-delphi/07-comparaison-avec-dautres-environnements.md)
