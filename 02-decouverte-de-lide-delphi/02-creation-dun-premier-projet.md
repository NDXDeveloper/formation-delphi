🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.2 Création d'un premier projet

## Introduction

Maintenant que vous connaissez l'interface de Delphi, il est temps de créer votre premier projet ! Cette étape est excitante car vous allez passer de la théorie à la pratique. Ne vous inquiétez pas, créer un projet dans Delphi est très simple et intuitif.

Dans cette section, nous allons voir comment démarrer un nouveau projet, comprendre ce qui se passe en coulisses, et explorer les différents types de projets que vous pouvez créer.

## Qu'est-ce qu'un projet Delphi ?

Avant de commencer, clarifions ce qu'est un projet dans Delphi. Un projet n'est pas un simple fichier, mais un ensemble de fichiers qui travaillent ensemble pour créer votre application. Ces fichiers comprennent :

- **Le fichier projet** (.dpr) : c'est le fichier principal qui coordonne tous les autres
- **Les fiches** (.dfm) : les fichiers qui décrivent l'apparence de vos fenêtres
- **Les unités de code** (.pas) : les fichiers qui contiennent le code Object Pascal
- **Les ressources** : images, icônes, sons, etc.
- **Les fichiers de configuration** : paramètres du projet

Delphi gère automatiquement la plupart de ces fichiers pour vous. Vous n'avez pas besoin de vous préoccuper de leur organisation dès le début.

## Créer un nouveau projet : méthode pas à pas

### Étape 1 : Accéder au menu de création

Il existe plusieurs façons de créer un nouveau projet dans Delphi :

**Via le menu principal** : cliquez sur **File > New > VCL Forms Application** (Fichier > Nouveau > Application Windows VCL) — ou choisissez **Multi-Device Application - Delphi** pour un projet FireMonkey multi-plateforme.

**Via la Welcome Page** : si vous venez de lancer Delphi, la page d'accueil affiche des raccourcis pour créer rapidement un nouveau projet à partir de différents modèles.

**Via le menu File > New > Other** (Fichier > Nouveau > Autre) : ouvre la fenêtre **New Items** (Object Repository) qui contient l'ensemble des modèles disponibles, classés par catégorie. C'est la voie la plus complète.

**Via IDE Insight** : **Ctrl + .** puis tapez "VCL" ou "New Project".

> 💡 Le raccourci **Ctrl + N** dépend du contexte : dans certaines versions de Delphi il ouvre un nouveau fichier dans l'éditeur, dans d'autres il déroule le menu New. Pour un comportement constant, préférez le menu **File > New** ou **Ctrl + .** (IDE Insight).

Pour notre premier projet, nous allons choisir une **VCL Forms Application** (Application Windows VCL), qui est le type de projet le plus classique et le plus simple pour débuter sous Windows.

### Étape 2 : Choisir le type de projet

Quand vous demandez à créer un nouveau projet, Delphi vous présente différentes options. Voici les plus courantes pour débuter :

**Application Windows VCL** : pour créer des applications Windows traditionnelles avec une interface native. C'est le choix idéal pour débuter et pour créer des applications destinées uniquement à Windows.

**Application multi-plateforme (FireMonkey)** : pour créer des applications qui fonctionnent sur plusieurs systèmes : Windows, macOS, iOS, Android, Linux. Plus complexe, mais très puissant si vous visez plusieurs plateformes.

**Application console** : pour créer des programmes en ligne de commande, sans interface graphique. Utile pour des scripts ou des outils automatisés.

**Bibliothèque DLL** : pour créer des bibliothèques de code réutilisables.

Pour votre premier projet, choisissez **Application Windows VCL**. C'est le plus simple et le plus didactique.

### Étape 3 : La fiche principale est créée

Dès que vous validez votre choix, Delphi crée automatiquement plusieurs choses pour vous :

**Une fiche vide** apparaît dans le concepteur : c'est la fenêtre principale de votre future application. Elle est actuellement vide et porte le nom par défaut "Form1".

**Une unité de code associée** : derrière cette fiche, il y a déjà du code Pascal généré automatiquement. Vous pouvez le voir en appuyant sur **F12** ou en cliquant sur l'onglet "Unit1.pas" en bas.

**Un fichier projet** : Delphi a créé le fichier principal de votre projet, mais vous n'avez pas besoin d'y toucher pour l'instant.

À ce stade, même sans rien faire, vous avez déjà une application fonctionnelle ! Elle ne fait rien d'intéressant, mais elle peut être compilée et exécutée.

### Étape 4 : Enregistrer votre projet

Avant d'aller plus loin, il est important d'enregistrer votre projet. C'est une bonne habitude à prendre dès le début.

**File > Save All** (Fichier > Enregistrer tout) ou appuyez sur **Ctrl + Shift + S**.

Delphi vous demandera de choisir un emplacement et un nom pour :

1. **L'unité de code** (Unit1.pas) : par défaut, Delphi propose "Unit1". Il est recommandé de choisir un nom plus explicite, par exemple `uMain` ou `MainUnit` si c'est votre unité principale.

2. **Le projet** (.dpr) : Delphi propose "Project1". Donnez-lui un nom significatif, par exemple "MonPremierProjet".

**Conseil important** : créez toujours un dossier dédié pour chaque projet. Delphi génère de nombreux fichiers temporaires et de compilation, et il vaut mieux les avoir bien organisés dans des dossiers séparés.

### Étape 5 : Structure des fichiers créés

Après l'enregistrement, explorez le dossier de votre projet. Vous verrez plusieurs fichiers :

**MonPremierProjet.dpr** : le fichier projet principal

**MainUnit.pas** : votre unité de code

**MainUnit.dfm** : la description de votre fiche (Form1)

**MonPremierProjet.dproj** : les paramètres et options du projet

**MonPremierProjet.res** : les ressources du projet (icône, etc.)

D'autres fichiers et dossiers seront créés lors de la compilation, mais ce sont les fichiers principaux que Delphi utilise.

## Comprendre la fiche créée

Revenons à la fiche qui s'affiche dans le concepteur. Elle possède déjà plusieurs propriétés que vous pouvez modifier dans l'inspecteur d'objets :

**Caption** : le titre qui s'affiche dans la barre de titre de la fenêtre. Par défaut, c'est "Form1". Essayez de le changer en "Ma Première Application" pour voir l'effet.

**Width et Height** : les dimensions de la fenêtre en pixels. Vous pouvez les modifier soit en redimensionnant la fiche avec la souris, soit en changeant ces valeurs dans l'inspecteur.

**Color** : la couleur de fond de la fenêtre. Par défaut, c'est la couleur standard de Windows.

**Name** : le nom interne du formulaire dans le code. C'est par ce nom que vous ferez référence à cette fiche dans votre code.

**Position** : définit où la fenêtre apparaîtra à l'écran (centrée, position par défaut, etc.).

N'hésitez pas à explorer ces propriétés dans l'inspecteur d'objets pour voir leurs effets.

## Le code généré automatiquement

Appuyez sur **F12** pour voir le code associé à votre fiche. Vous verrez quelque chose comme ceci :

```pascal
unit MainUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
```

Ne vous inquiétez pas si vous ne comprenez pas tout ce code pour l'instant. Voici ce qu'il faut retenir :

**interface** : la section qui déclare ce que votre unité rend visible aux autres parties du programme.

**uses** : la liste des bibliothèques que cette unité utilise.

**type** : ici est déclarée votre classe TForm1, qui représente votre fiche.

**implementation** : la section où vous écrirez le code qui fait fonctionner votre fiche.

Delphi maintient automatiquement la synchronisation entre votre fiche visuelle et ce code. Quand vous ajoutez un bouton sur la fiche, Delphi ajoute automatiquement la déclaration correspondante dans le code.

## Compiler et exécuter votre premier projet

Maintenant, le moment que vous attendez : faire tourner votre application !

**Appuyez sur F9** ou cliquez sur le bouton de lecture verte dans la barre d'outils.

Delphi va alors :

1. **Compiler votre code** : transformer le code Object Pascal en langage machine compréhensible par l'ordinateur.

2. **Lier les bibliothèques** : assembler tous les composants nécessaires.

3. **Créer l'exécutable** : générer le fichier .exe de votre application.

4. **Lancer l'application** : exécuter automatiquement votre programme.

Si tout se passe bien, une nouvelle fenêtre s'ouvre : c'est votre application ! Certes, elle ne fait rien pour l'instant, mais c'est déjà votre création. Vous pouvez la déplacer, la redimensionner, la minimiser, la fermer, exactement comme n'importe quelle application Windows.

**En bas de l'IDE Delphi**, vous verrez la fenêtre de messages qui affiche les résultats de la compilation. Si tout s'est bien passé, vous verrez un message indiquant la réussite de la compilation et le nombre de lignes compilées.

## Premier vrai projet interactif : "Hello World" avec un bouton

Une application vide, c'est sympa, mais peu satisfaisant. Faisons un véritable mini-projet en 5 étapes : un bouton qui affiche un message quand on clique dessus. C'est l'équivalent du "Hello World" en Delphi.

### Étape 1 : Ajouter un bouton sur la fiche

1. Dans la **Tool Palette** (palette d'outils, à gauche), repérez la catégorie **Standard**
2. Cliquez sur le composant **TButton** (l'icône bouton)
3. Cliquez ensuite **au centre de la fiche** (Form1) — un bouton apparaît avec le texte par défaut "Button1"

### Étape 2 : Personnaliser le bouton

Le bouton est sélectionné automatiquement. Dans l'**Object Inspector** (à droite), modifiez les propriétés suivantes :

| Propriété | Valeur à mettre |
|-----------|------------------|
| `Name` | `btnSaluer` |
| `Caption` | `Dis bonjour` |
| `Width` | `120` |
| `Height` | `40` |

Vous verrez le bouton se transformer en temps réel sur la fiche.

### Étape 3 : Créer le code de l'événement clic

**Double-cliquez** sur le bouton dans la fiche. Delphi bascule automatiquement dans l'éditeur de code et crée une procédure vide :

```pascal
procedure TForm1.btnSaluerClick(Sender: TObject);
begin

end;
```

Le curseur est positionné entre `begin` et `end`. Tapez la ligne suivante :

```pascal
procedure TForm1.btnSaluerClick(Sender: TObject);
begin
  ShowMessage('Bonjour ! Bienvenue dans Delphi.');
end;
```

`ShowMessage` est une **procédure** (déclarée dans `Vcl.Dialogs`) qui affiche une petite boîte de dialogue avec votre texte. En Object Pascal, une procédure ne renvoie pas de valeur, contrairement à une fonction.

### Étape 4 : Sauvegarder et compiler

- **Ctrl + Shift + S** pour tout sauvegarder
- **F9** pour compiler et lancer l'application

### Étape 5 : Tester !

Votre application s'ouvre. Cliquez sur le bouton "Dis bonjour". Une boîte de dialogue apparaît avec votre message. Cliquez sur "OK" pour la fermer.

**Bravo ! Vous venez de créer votre première application Delphi interactive.**

> 💡 **Ce qui s'est passé en coulisses :**
> 1. Vous avez ajouté visuellement un composant à votre fiche (sans code)
> 2. Delphi a automatiquement déclaré ce bouton dans la classe `TForm1`
> 3. En double-cliquant, Delphi a créé une **procédure d'événement** et l'a liée au clic du bouton
> 4. Vous avez écrit **1 ligne** de code pour définir le comportement
> 5. La compilation a transformé tout cela en un `.exe` autonome
>
> Combien de lignes de code avez-vous écrites au total ? **Une seule.** C'est la productivité Delphi en action.

### Pour aller un peu plus loin

Modifiez le message pour qu'il affiche une réponse différente selon l'heure :

```pascal
procedure TForm1.btnSaluerClick(Sender: TObject);
var
  Heure: Word;
begin
  Heure := HourOf(Now);
  if Heure < 12 then
    ShowMessage('Bonjour ! Bonne matinée.')
  else if Heure < 18 then
    ShowMessage('Bon après-midi !')
  else
    ShowMessage('Bonsoir ! Bonne soirée.');
end;
```

Vous devrez ajouter `System.DateUtils` dans la clause `uses` (Delphi vous le proposera automatiquement avec une ampoule lumineuse dans la marge).

Recompilez (**F9**) et cliquez sur le bouton. Le message change selon l'heure de la journée !

## Les différents modes de compilation

Delphi offre plusieurs façons de compiler et exécuter votre projet :

**F9 (Exécuter)** : compile et lance immédiatement l'application.

**Ctrl + F9 (Compiler)** : compile le projet sans le lancer. Utile pour vérifier qu'il n'y a pas d'erreurs.

**Shift + F9 (Compiler et construire)** : recompile tout le projet depuis zéro, même les parties qui n'ont pas changé (équivalent à un Build complet).

Pour l'instant, **F9** sera votre raccourci le plus utilisé.

> 💡 **Rappel** : la touche **F12** ne déclenche pas de compilation — elle vous fait basculer entre la vue concepteur et la vue code. Pratique pour passer rapidement de l'un à l'autre pendant que vous travaillez.

## Où se trouve votre application ?

Quand vous compilez votre projet, Delphi crée un fichier exécutable (.exe). Mais où est-il ?

Par défaut, l'exécutable se trouve dans un sous-dossier de votre projet, organisé par **plateforme** puis **configuration** :

- **Win64\Debug** : compilation en mode Debug (développement) pour Windows 64 bits — **plateforme par défaut dans Delphi 13**
- **Win32\Debug** : Windows 32 bits — rarement nécessaire aujourd'hui (utile uniquement pour cibler des environnements anciens ou des contraintes d'intégration spécifiques)
- **Win64\Release** : version finale optimisée 64 bits
- **Win64Arm\Debug** : Windows on Arm (nouveauté Delphi 13.1)

Et sur d'autres plateformes :
- **OSX64\Debug** : pour macOS
- **Android64\Debug**, **iOSDevice64\Debug** : pour mobile
- **Linux64\Debug** : pour Linux

Vous pouvez naviguer vers ce dossier et double-cliquer sur votre .exe pour lancer votre application sans passer par Delphi. C'est le fichier que vous pourrez distribuer à d'autres personnes (même si pour l'instant, il ne fait pas grand-chose !).

> 💡 **Astuce :** le chemin de sortie est configurable dans **Project > Options > Building > Delphi Compiler > Output Directory**. Vous pouvez aussi y accéder rapidement via clic droit sur le projet > **Show in Explorer** pour ouvrir le dossier dans l'explorateur Windows.

## Les configurations de compilation

Delphi propose deux configurations principales :

**Debug** : mode de développement, avec des informations supplémentaires pour le débogage. L'exécutable est plus gros et moins optimisé, mais il est plus facile à déboguer.

**Release** : mode de production, optimisé pour la vitesse et la taille. C'est cette version que vous utiliserez pour distribuer votre application finale.

Vous pouvez changer de configuration dans la barre d'outils, où vous verrez un menu déroulant avec ces options. Pour le développement, restez en mode **Debug**.

## Types de projets courants

Bien que nous ayons créé une application VCL, voici un aperçu des autres types de projets que vous rencontrerez :

**VCL Forms Application** : applications Windows classiques avec interface native. Idéal pour commencer et pour les applications professionnelles Windows.

**FireMonkey (FMX) Application** : applications multi-plateformes avec interface moderne. Plus complexe mais permet de cibler Windows, macOS, iOS, Android et Linux avec le même code.

**Console Application** : programmes en ligne de commande, sans interface graphique. Parfait pour des outils, des scripts, ou pour apprendre les bases du langage sans se soucier de l'interface.

**Service Application** : pour créer des services Windows qui tournent en arrière-plan.

**DLL Library** : pour créer des bibliothèques de code réutilisables.

**Package** : pour créer des composants réutilisables dans l'IDE Delphi.

Pour débuter, concentrez-vous sur les **VCL Forms Application**. Vous explorerez les autres types au fur et à mesure de votre progression.

## Bonnes pratiques pour vos projets

Dès votre premier projet, prenez de bonnes habitudes :

**Un dossier par projet** : ne mélangez jamais les fichiers de différents projets dans le même dossier.

**Des noms significatifs** : évitez de garder les noms par défaut "Unit1", "Project1". Donnez des noms qui décrivent la fonction ("MainForm", "CalculatriceProjet").

**Sauvegardez régulièrement** : prenez l'habitude d'appuyer sur **Ctrl + S** fréquemment.

**Utilisez le contrôle de version** : même pour de petits projets personnels, un système comme Git peut vous sauver la mise. Nous en reparlerons plus tard.

**Commentez votre code** : même si pour l'instant il n'y a pas beaucoup de code, prenez l'habitude de commenter ce que vous faites.

**Testez souvent** : compilez et testez votre application fréquemment, même pour de petites modifications. Il est plus facile de corriger les erreurs au fur et à mesure que de chercher un bug dans 100 lignes de code modifiées.

## Les erreurs courantes et comment les éviter

Même dans un projet simple, vous pouvez rencontrer quelques problèmes :

**"Cannot create file"** : cela arrive si votre application est encore en cours d'exécution. Fermez-la avant de recompiler.

**Fichiers manquants** : assurez-vous d'avoir bien enregistré tous vos fichiers avant de fermer Delphi.

**Problèmes de droits d'accès** : évitez de créer vos projets dans des dossiers système comme "Program Files". Utilisez plutôt votre dossier "Documents" ou créez un dossier "Projets Delphi" dédié.

**Chemins avec des espaces ou des caractères spéciaux** : même si Delphi les gère, il vaut mieux éviter les espaces dans les noms de dossiers pour vos projets (utilisez "MonProjet" plutôt que "Mon Projet").

## Fermer et rouvrir un projet

Pour fermer votre projet actuel : **File > Close All** (Fichier > Fermer tout)

Pour rouvrir un projet existant : **File > Open Project** (Fichier > Ouvrir un projet), ou **Ctrl + F11**, puis naviguez jusqu'au fichier `.dproj` ou `.dpr` de votre projet.

Delphi garde aussi une liste des projets récents dans le menu **File > Reopen** (Fichier > Récemment ouverts), ce qui vous permet de les rouvrir rapidement.

## Gérer plusieurs projets

Au début, vous travaillerez probablement sur un projet à la fois. Mais Delphi permet aussi de gérer plusieurs projets simultanément grâce aux **groupes de projets** (`.groupproj`).

Un groupe de projets est utile quand vous développez plusieurs applications qui travaillent ensemble, par exemple une application principale et une DLL, ou un client et un serveur.

Pour créer un groupe de projets : **File > New > Other > Delphi Projects > Project Group** (Fichier > Nouveau > Autre > Projets Delphi > Groupe de projets)

Mais ne vous souciez pas de cela pour l'instant. Concentrez-vous sur la maîtrise d'un seul projet à la fois.

## Conclusion

Félicitations ! Vous venez de créer, compiler et exécuter votre premier projet Delphi. Même s'il ne fait encore rien de spectaculaire, vous avez franchi une étape importante.

Vous savez maintenant :

- Créer un nouveau projet
- Identifier les principaux fichiers générés (.dpr, .pas, .dfm, .dproj, .res)
- Naviguer entre la fiche (concepteur) et le code (éditeur)
- Compiler et exécuter une application
- Localiser votre exécutable dans le dossier de sortie

Dans les prochaines sections, nous allons enrichir cette application en ajoutant des composants visuels, en écrivant du code, et en créant quelque chose d'utile et d'interactif.

L'important à ce stade est de vous familiariser avec le processus de création et de compilation. N'hésitez pas à créer plusieurs projets tests pour vous entraîner. Chaque nouveau projet est une occasion d'explorer et d'apprendre.

La prochaine étape sera de découvrir la palette d'outils et l'inspecteur d'objets, les deux outils essentiels pour construire l'interface de vos applications !

⏭️ [La Palette d'outils et l'Inspecteur d'objets](/02-decouverte-de-lide-delphi/03-palette-doutils-et-inspecteur-dobjets.md)
