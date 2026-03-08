🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.2 Création d'un premier projet

## Introduction

Maintenant que vous connaissez l'interface de Delphi, il est temps de créer votre premier projet ! Cette étape est excitante car vous allez passer de la théorie à la pratique. Ne vous inquiétez pas, créer un projet dans Delphi est très simple et intuitif.

Dans cette section, nous allons voir comment démarrer un nouveau projet, comprendre ce qui se passe en coulisse, et explorer les différents types de projets que vous pouvez créer.

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

**Via le menu principal** : cliquez sur "Fichier" puis "Nouveau", et enfin "Application Windows VCL" (ou "Application multi-plateforme - FireMonkey" selon votre besoin).

**Via l'écran d'accueil** : si vous venez de lancer Delphi, l'écran d'accueil affiche des raccourcis pour créer rapidement un nouveau projet.

**Via le raccourci clavier** : vous pouvez utiliser **Ctrl + N** pour ouvrir le dialogue de création.

Pour notre premier projet, nous allons choisir une **Application Windows VCL**, qui est le type de projet le plus classique et le plus simple pour débuter sous Windows.

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

**Choisir "Fichier > Enregistrer tout"** ou appuyez sur **Ctrl + Shift + S**.

Delphi vous demandera de choisir un emplacement et un nom pour :

1. **L'unité de code** (Unit1.pas) : par défaut, Delphi propose "Unit1". Il est recommandé de choisir un nom plus explicite, par exemple "MainUnit" ou "PrincipaleUnit" si c'est votre unité principale.

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

```
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

## Les différents modes de compilation

Delphi offre plusieurs façons de compiler et exécuter votre projet :

**F9 (Exécuter)** : compile et lance immédiatement l'application.

**Ctrl + F9 (Compiler)** : compile le projet sans le lancer. Utile pour vérifier qu'il n'y a pas d'erreurs.

**Shift + F9 (Compiler et construire)** : recompile tout le projet depuis zéro, même les parties qui n'ont pas changé.

**F12** : vous ramène rapidement au concepteur de fiche si vous êtes dans le code.

Pour l'instant, **F9** sera votre raccourci le plus utilisé.

## Où se trouve votre application ?

Quand vous compilez votre projet, Delphi crée un fichier exécutable (.exe). Mais où est-il ?

Par défaut, l'exécutable se trouve dans un sous-dossier de votre projet :

- **Win32\Debug** pour une compilation en mode Debug (développement) pour Windows 32 bits
- **Win64\Debug** pour Windows 64 bits
- **Win32\Release** pour une version finale optimisée

Vous pouvez naviguer vers ce dossier et double-cliquer sur votre .exe pour lancer votre application sans passer par Delphi. C'est le fichier que vous pourrez distribuer à d'autres personnes (même si pour l'instant, il ne fait pas grand-chose !).

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

**Testez souvent** : compilez et testez votre application fréquemment, même pour de petites modifications. C'est plus facile de corriger les erreurs au fur et à mesure que de chercher un bug dans 100 lignes de code modifiées.

## Les erreurs courantes et comment les éviter

Même dans un projet simple, vous pouvez rencontrer quelques problèmes :

**"Cannot create file"** : cela arrive si votre application est encore en cours d'exécution. Fermez-la avant de recompiler.

**Fichiers manquants** : assurez-vous d'avoir bien enregistré tous vos fichiers avant de fermer Delphi.

**Problèmes de droits d'accès** : évitez de créer vos projets dans des dossiers système comme "Program Files". Utilisez plutôt votre dossier "Documents" ou créez un dossier "Projets Delphi" dédié.

**Chemins avec des espaces ou des caractères spéciaux** : même si Delphi les gère, il vaut mieux éviter les espaces dans les noms de dossiers pour vos projets (utilisez "MonProjet" plutôt que "Mon Projet").

## Fermer et rouvrir un projet

Pour fermer votre projet actuel : **Fichier > Fermer tout**

Pour rouvrir un projet existant : **Fichier > Ouvrir un projet** (ou **Ctrl + F11**), puis naviguez jusqu'au fichier .dproj ou .dpr de votre projet.

Delphi garde aussi une liste des projets récents dans le menu "Fichier", ce qui vous permet de les rouvrir rapidement.

## Gérer plusieurs projets

Au début, vous travaillerez probablement sur un projet à la fois. Mais Delphi permet aussi de gérer plusieurs projets simultanément grâce aux **groupes de projets** (.groupproj).

Un groupe de projets est utile quand vous développez plusieurs applications qui travaillent ensemble, par exemple une application principale et une DLL, ou un client et un serveur.

Pour créer un groupe de projets : **Fichier > Nouveau > Autre > Delphi Projects > Project Group**

Mais ne vous souciez pas de cela pour l'instant. Concentrez-vous sur la maîtrise d'un seul projet à la fois.

## Conclusion

Félicitations ! Vous venez de créer, compiler et exécuter votre premier projet Delphi. Même s'il ne fait encore rien de spectaculaire, vous avez franchi une étape importante.

Vous savez maintenant :

- Comment créer un nouveau projet
- Comprendre les fichiers générés
- Naviguer entre la fiche et le code
- Compiler et exécuter une application
- Où se trouve votre exécutable

Dans les prochaines sections, nous allons enrichir cette application en ajoutant des composants visuels, en écrivant du code, et en créant quelque chose d'utile et d'interactif.

L'important à ce stade est de vous familiariser avec le processus de création et de compilation. N'hésitez pas à créer plusieurs projets tests pour vous entraîner. Chaque nouveau projet est une occasion d'explorer et d'apprendre.

La prochaine étape sera de découvrir la palette d'outils et l'inspecteur d'objets, les deux outils essentiels pour construire l'interface de vos applications !

⏭️ [La Palette d'outils et l'Inspecteur d'objets](/02-decouverte-de-lide-delphi/03-palette-doutils-et-inspecteur-dobjets.md)
