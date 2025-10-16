🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.4 Explorateur de projets et gestionnaire de code

## Introduction

Au fur et à mesure que vos projets Delphi grandissent, vous allez créer de plus en plus de fichiers : plusieurs fiches, plusieurs unités de code, des fichiers de ressources, etc. Comment s'y retrouver dans tout cela ? Comment naviguer rapidement d'un fichier à l'autre, d'une procédure à une autre ?

C'est là qu'interviennent deux outils essentiels : l'Explorateur de projets et le Gestionnaire de code. Ces outils sont vos assistants pour organiser, naviguer et gérer efficacement tous les éléments de votre application.

Dans cette section, nous allons découvrir comment ces outils peuvent transformer votre façon de travailler et vous faire gagner un temps précieux.

## L'Explorateur de projets (Project Manager)

### Qu'est-ce que l'Explorateur de projets ?

L'Explorateur de projets est une fenêtre qui affiche la structure complète de votre projet sous forme d'arborescence. C'est comme l'explorateur de fichiers Windows, mais spécialement conçu pour les projets Delphi.

Il vous montre tous les fichiers qui composent votre application : les fiches, les unités de code, les ressources, les bibliothèques utilisées, et bien plus encore.

### Localisation de l'Explorateur de projets

L'Explorateur de projets se trouve généralement en haut à droite de l'IDE. Si vous ne le voyez pas, vous pouvez l'afficher via le menu **Affichage > Gestionnaire de projets** ou en appuyant sur **Ctrl + Alt + F11**.

### Structure de l'arborescence

Quand vous ouvrez l'Explorateur de projets, vous voyez une structure hiérarchique. Prenons l'exemple d'un projet simple nommé "MonApplication" :

```
ProjectGroup1 (le groupe de projets)
└── MonApplication.exe (votre projet)
    ├── MonApplication.dpr (le fichier projet principal)
    ├── Build Configurations (configurations de compilation)
    │   ├── Debug
    │   └── Release
    ├── FormPrincipale (votre fiche principale)
    │   ├── FormPrincipale.pas (le code)
    │   └── FormPrincipale.dfm (la définition visuelle)
    ├── Références (les bibliothèques utilisées)
    └── Contenu (autres fichiers et ressources)
```

Chaque élément de cette arborescence représente un fichier ou un groupe de fichiers de votre projet.

### Les différents types de fichiers dans un projet

#### Le fichier projet (.dpr)

C'est le fichier principal, le "chef d'orchestre" de votre application. Il contient le code qui initialise l'application et crée la fenêtre principale. Delphi gère automatiquement ce fichier pour vous, et vous n'aurez que rarement besoin de le modifier directement.

Double-cliquez dessus dans l'Explorateur de projets si vous voulez voir son contenu. Vous y trouverez du code qui ressemble à ceci :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  FormPrincipale in 'FormPrincipale.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
```

Ce code dit essentiellement : "Démarre l'application, crée la fenêtre principale, et lance la boucle d'événements".

#### Les unités (.pas)

Ce sont les fichiers qui contiennent votre code Object Pascal. Chaque fiche que vous créez a une unité associée. Vous pouvez aussi créer des unités "pures" qui ne contiennent que du code, sans interface visuelle.

Les unités sont les véritables travailleurs de votre application. C'est là que vous écrivez la logique, les calculs, les traitements de données, etc.

#### Les fiches (.dfm)

Ce sont des fichiers spéciaux qui décrivent l'apparence de vos fenêtres. Ils stockent toutes les propriétés que vous définissez dans l'Inspecteur d'objets : la position des composants, leurs couleurs, leurs tailles, etc.

Ces fichiers sont au format texte, mais dans un format spécifique à Delphi. Vous pouvez les voir en faisant un clic droit sur une fiche dans l'Explorateur de projets et en choisissant **Afficher en tant que texte**.

Généralement, vous ne modifiez pas ces fichiers directement. Vous utilisez le concepteur visuel et l'Inspecteur d'objets, et Delphi met à jour le .dfm automatiquement.

#### Les ressources (.res)

Les fichiers de ressources contiennent des éléments comme l'icône de votre application, des images intégrées, des sons, etc. Delphi crée automatiquement un fichier .res pour votre projet.

#### Les fichiers de configuration (.dproj, .cfg, .dof)

Ces fichiers contiennent les paramètres de votre projet : les options de compilation, les chemins de recherche, les configurations Debug/Release, etc. Delphi les gère automatiquement.

### Navigation dans l'Explorateur de projets

L'Explorateur de projets n'est pas qu'un simple affichage : c'est un outil interactif puissant.

#### Ouvrir un fichier

**Double-clic** sur un fichier pour l'ouvrir dans l'éditeur. Si c'est une fiche, elle s'ouvre dans le concepteur visuel. Si c'est une unité de code, elle s'ouvre dans l'éditeur de code.

**Clic droit** sur un élément pour voir toutes les actions disponibles : ouvrir, supprimer, renommer, compiler, etc.

#### Ajouter des fichiers au projet

Vous pouvez ajouter de nouveaux éléments à votre projet de plusieurs façons :

**Via le menu Fichier > Nouveau > Fiche** : crée une nouvelle fiche avec son unité de code.

**Via le menu Fichier > Nouveau > Unité** : crée une nouvelle unité de code sans interface visuelle.

**Clic droit dans l'Explorateur** : sur le nom du projet, choisissez **Ajouter** pour ajouter un fichier existant.

**Par glisser-déposer** : depuis l'explorateur Windows, vous pouvez faire glisser un fichier .pas dans l'Explorateur de projets.

#### Supprimer des fichiers du projet

**Clic droit** sur un fichier et choisissez **Retirer du projet**. Attention : cela ne supprime pas le fichier de votre disque dur, cela le retire simplement du projet.

Si vous voulez vraiment supprimer le fichier, choisissez **Supprimer** au lieu de **Retirer du projet**.

#### Organiser les fichiers

Pour les projets complexes, vous pouvez organiser vos fichiers en dossiers virtuels dans l'Explorateur de projets. Clic droit sur le projet, choisissez **Ajouter > Nouveau dossier**, donnez un nom au dossier, puis glissez-déposez vos fichiers dedans.

Attention : ce sont des dossiers virtuels dans l'Explorateur, pas de vrais dossiers sur votre disque dur. C'est simplement une aide visuelle pour organiser vos fichiers.

### Les configurations de compilation (Build Configurations)

Vous verrez dans l'Explorateur un nœud appelé **Build Configurations** qui contient généralement deux entrées :

**Debug** : configuration pour le développement. Le code n'est pas optimisé, mais il contient des informations supplémentaires qui facilitent le débogage.

**Release** : configuration pour la distribution finale. Le code est optimisé pour la vitesse et la taille, mais plus difficile à déboguer.

Vous pouvez basculer entre ces configurations via la barre d'outils ou le menu **Projet > Configurer la version**. Pendant le développement, restez en mode **Debug**.

### Groupes de projets

Un groupe de projets vous permet de travailler sur plusieurs projets en même temps. C'est utile quand vous développez, par exemple :

- Une application principale et une DLL qu'elle utilise
- Un client et un serveur
- Plusieurs outils qui partagent du code commun

Dans l'Explorateur de projets, vous verrez alors plusieurs projets listés sous le groupe. Vous pouvez définir quel projet est le projet actif (celui qui sera compilé quand vous appuyez sur F9) en faisant un clic droit et en choisissant **Activer**.

### Options du projet

Un clic droit sur le nom du projet dans l'Explorateur donne accès à **Options**. C'est là que vous configurez de nombreux paramètres :

**Application** : nom de l'application, icône, fichier de manifeste

**Formulaires** : quelle fiche se crée automatiquement au démarrage

**Compilateur** : options de compilation avancées

**Répertoires/Conditions** : chemins de recherche pour les fichiers

**Version Info** : informations de version de votre application

**Connexion** : pour les applications qui utilisent des bases de données

Ne modifiez ces options que si vous savez ce que vous faites. Les valeurs par défaut conviennent pour commencer.

## Le Gestionnaire de code (Code Navigation)

### Qu'est-ce que le Gestionnaire de code ?

Le Gestionnaire de code n'est pas une fenêtre unique, mais un ensemble d'outils et de fonctionnalités qui vous aident à naviguer et à éditer votre code efficacement.

Quand votre projet grandit et que vous avez des centaines ou des milliers de lignes de code réparties dans plusieurs fichiers, ces outils deviennent indispensables.

### L'Explorateur de structure (Structure View)

Cette fenêtre, souvent située sous l'Explorateur de projets, affiche la structure interne du fichier actuellement ouvert. Pour une unité de code, elle montre :

- Les unités utilisées (section **uses**)
- Les types définis (classes, records, énumérations)
- Les constantes
- Les variables
- Les procédures et fonctions

Vous pouvez double-cliquer sur n'importe quel élément pour naviguer directement vers sa définition dans le code. C'est extrêmement pratique pour les gros fichiers.

Si vous ne voyez pas cette fenêtre, affichez-la via **Affichage > Explorateur de structure**.

### Navigation dans le code

#### Onglets de fichiers

En haut de l'éditeur de code, vous voyez des onglets pour tous les fichiers ouverts. Cliquez sur un onglet pour basculer vers ce fichier. Vous pouvez fermer un onglet en cliquant sur le petit "x" à droite.

**Astuce** : faites un clic droit sur un onglet pour voir des options utiles comme "Fermer tous sauf celui-ci" ou "Ouvrir le dossier contenant".

#### Basculer entre interface et implémentation

Dans une unité Delphi, le code est organisé en deux sections principales :

- **interface** : les déclarations publiques (ce qui est visible par les autres unités)
- **implementation** : les implémentations réelles (le code qui fait le travail)

Pour naviguer rapidement entre la déclaration d'une procédure (dans **interface**) et son implémentation (dans **implementation**), placez votre curseur sur le nom de la procédure et appuyez sur **Ctrl + Shift + Flèche haut** ou **Flèche bas**.

Vous pouvez aussi faire un clic droit et choisir **Rechercher la déclaration** ou **Rechercher l'implémentation**.

#### Basculer entre fiche et code

Nous l'avons déjà vu, mais c'est tellement important que ça mérite d'être répété :

**F12** : bascule entre le concepteur de fiche et le code associé

**F11** : ouvre l'Inspecteur d'objets

Ces raccourcis sont parmi les plus utilisés dans Delphi.

#### Marque-pages (Bookmarks)

Pour marquer des endroits importants dans votre code, utilisez les marque-pages :

**Ctrl + Shift + [numéro de 0 à 9]** : définit un marque-page numéroté

**Ctrl + [numéro]** : saute au marque-page correspondant

Les marque-pages sont indiqués par un petit chiffre dans la marge gauche de l'éditeur.

#### Historique de navigation

Delphi garde en mémoire les endroits où vous avez navigué dans votre code. Vous pouvez revenir en arrière et avancer comme dans un navigateur web :

**Alt + Flèche gauche** : retour arrière dans l'historique

**Alt + Flèche droite** : avance dans l'historique

C'est très utile quand vous explorez du code et que vous voulez revenir à votre point de départ.

### Recherche dans le code

#### Recherche simple

**Ctrl + F** : ouvre la boîte de dialogue de recherche dans le fichier actuel.

Vous pouvez rechercher du texte, en respectant ou non la casse, et rechercher le mot complet ou une partie.

**F3** : trouve l'occurrence suivante

**Shift + F3** : trouve l'occurrence précédente

#### Recherche et remplacement

**Ctrl + H** : ouvre la boîte de dialogue de recherche et remplacement.

Vous pouvez remplacer une occurrence à la fois ou toutes les occurrences d'un coup. Soyez prudent avec "Remplacer tout" : prévisualisez toujours ce qui va être remplacé.

#### Recherche dans les fichiers

**Ctrl + Shift + F** : ouvre la recherche dans tous les fichiers du projet (ou dans un répertoire).

C'est extrêmement puissant pour trouver où une fonction est utilisée, où une variable est modifiée, etc. Les résultats s'affichent dans une fenêtre séparée, et vous pouvez double-cliquer sur un résultat pour naviguer directement vers cet emplacement.

#### Recherche de déclaration

Placez votre curseur sur un nom de variable, de fonction, de classe, etc., et :

**Ctrl + clic** : saute directement à la déclaration

Ou **clic droit > Rechercher la déclaration**

C'est l'une des fonctionnalités les plus utiles de Delphi. Vous pouvez naviguer dans votre code en suivant les références, comme si vous cliquiez sur des liens hypertexte.

#### Trouver toutes les références

Pour voir tous les endroits où un élément est utilisé :

**Clic droit sur l'élément > Trouver des références**

Ou **Shift + Ctrl + Entrée**

Cela affiche une liste de toutes les utilisations de cet élément dans votre projet. Très utile pour comprendre comment une fonction est utilisée ou pour vérifier l'impact d'une modification.

### Code Insight et auto-complétion

Code Insight est un ensemble de fonctionnalités qui vous aident à écrire du code plus rapidement et avec moins d'erreurs.

#### Auto-complétion (Code Completion)

Pendant que vous tapez du code, appuyez sur **Ctrl + Espace** pour activer l'auto-complétion. Delphi affiche une liste de suggestions basée sur le contexte :

- Les propriétés et méthodes disponibles pour un objet
- Les variables et fonctions accessibles
- Les mots-clés du langage

Utilisez les flèches pour naviguer dans la liste, puis **Entrée** pour insérer la suggestion sélectionnée.

L'auto-complétion se déclenche aussi automatiquement dans certaines situations, par exemple quand vous tapez un point après un nom d'objet : `Button1.` affichera automatiquement toutes les propriétés et méthodes de Button1.

#### Aide sur les paramètres (Parameter Hints)

Quand vous écrivez un appel de fonction ou de procédure, Delphi affiche une info-bulle avec la signature de la fonction : les paramètres requis, leurs types, et parfois une brève description.

Si la fonction a plusieurs surcharges (versions avec différents paramètres), vous pouvez naviguer entre elles avec **Ctrl + Shift + Flèche bas**.

#### Info-bulles des symboles (Tooltip Symbol Insight)

Passez votre souris sur un nom de variable, de fonction, de classe, etc., et une info-bulle s'affiche avec des informations sur cet élément : son type, sa déclaration, parfois une brève documentation.

C'est pratique pour rappeler rapidement à quoi sert un élément sans avoir à chercher sa déclaration.

#### Déclaration de variables automatique (Declare Variable)

Si vous écrivez du code qui utilise une variable non déclarée, Delphi peut la déclarer automatiquement pour vous.

Placez votre curseur sur le nom de la variable non déclarée, et vous verrez une petite ampoule ou un symbole d'erreur dans la marge. Cliquez dessus (ou appuyez sur **Alt + Entrée**) pour voir les actions possibles, dont "Déclarer la variable".

### Édition de code avancée

#### Indentation automatique

Delphi indente automatiquement votre code au fur et à mesure que vous tapez. Si vous voulez ré-indenter un bloc de code :

1. Sélectionnez le bloc
2. Appuyez sur **Ctrl + D** pour le formater automatiquement

Ou utilisez **Ctrl + Shift + I** pour indenter, et **Ctrl + Shift + U** pour désindenter.

#### Complétion des blocs

Quand vous tapez `begin` et que vous appuyez sur **Entrée**, Delphi ajoute automatiquement le `end;` correspondant et positionne votre curseur entre les deux. Pareil pour d'autres structures comme `if`, `while`, `try`, etc.

#### Commentaires rapides

Pour commenter ou décommenter rapidement une ligne ou un bloc de code :

**Ctrl + /** : commente la sélection en ajoutant `//` au début de chaque ligne

**Ctrl + Shift + /** : enlève les commentaires

Les commentaires sont essentiels pour expliquer votre code. N'hésitez pas à en mettre, surtout pour les parties complexes.

#### Sélection en colonne

Maintenez **Alt** enfoncé et faites glisser votre souris pour faire une sélection rectangulaire (en colonne) plutôt qu'une sélection linéaire classique.

C'est utile pour éditer plusieurs lignes en même temps, par exemple pour ajouter ou supprimer des caractères au même endroit sur plusieurs lignes.

#### Templates de code (Code Templates)

Delphi propose des templates pour insérer rapidement des structures de code courantes.

Tapez un raccourci et appuyez sur **Espace** pour l'expanser. Par exemple :

- `try` + **Espace** → insère un bloc try-finally complet
- `ife` + **Espace** → insère une structure if-then-else
- `fori` + **Espace** → insère une boucle for

Vous pouvez voir tous les templates disponibles et en créer de nouveaux dans **Outils > Options > Éditeur de code > Code Insight**.

### Refactoring

Le refactoring consiste à améliorer la structure de votre code sans changer son comportement. Delphi propose quelques outils de refactoring :

#### Renommer (Rename)

Pour renommer une variable, une fonction, une classe, etc. partout où elle est utilisée :

1. Placez votre curseur sur l'élément
2. Clic droit > **Refactoriser > Renommer** (ou **Ctrl + Shift + E**)
3. Tapez le nouveau nom
4. Delphi met à jour toutes les occurrences

C'est beaucoup plus sûr que de faire un simple rechercher/remplacer, car Delphi comprend la portée et le contexte.

#### Extraire une méthode

Si vous avez un bloc de code que vous voulez transformer en méthode séparée :

1. Sélectionnez le bloc
2. Clic droit > **Refactoriser > Extraire méthode**
3. Donnez un nom à la méthode
4. Delphi crée la méthode et remplace le bloc sélectionné par un appel à cette méthode

#### Déclarer une variable

Comme mentionné plus haut, si vous utilisez une variable non déclarée, Delphi peut la déclarer pour vous automatiquement.

### Analyse de code

#### Liste des erreurs et avertissements

En bas de l'IDE, vous trouvez la fenêtre **Messages** qui affiche :

- Les **erreurs** de compilation (en rouge) : des problèmes qui empêchent la compilation
- Les **avertissements** (en jaune) : des problèmes potentiels qui ne bloquent pas la compilation
- Les **indications** (en bleu) : des suggestions d'amélioration

Double-cliquez sur un message pour naviguer directement vers la ligne de code concernée.

#### Indicateurs dans le code

Dans la marge gauche de l'éditeur, vous verrez différents indicateurs :

- Des **points rouges** : erreurs de syntaxe
- Des **traits de soulignement ondulés** : avertissements
- Des **ampoules** : suggestions d'actions rapides

Ces indicateurs apparaissent en temps réel pendant que vous tapez, vous permettant de corriger les erreurs immédiatement.

### Gestion des unités utilisées

#### Clause uses

Au début de chaque unité, dans les sections **interface** et **implementation**, vous trouvez une clause **uses** qui liste toutes les unités dont votre code dépend.

```pascal
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
```

#### Ajouter une unité automatiquement

Quand vous utilisez un type ou une fonction qui n'est pas encore dans votre clause **uses**, Delphi souligne l'erreur. Cliquez sur l'ampoule ou appuyez sur **Alt + Entrée**, et choisissez **Ajouter l'unité à la clause uses**.

Delphi trouve automatiquement la bonne unité et l'ajoute pour vous.

#### Organiser les uses

Avec le temps, votre clause **uses** peut devenir longue et désordonnée. Vous pouvez la réorganiser :

**Clic droit dans le code > Refactoriser > Organiser les uses**

Delphi trie les unités alphabétiquement et supprime les doublons.

### Synchronisation entre code et concepteur

Un point important à comprendre : quand vous modifiez du code ou des propriétés, Delphi maintient une synchronisation constante entre :

- Le code de votre unité (.pas)
- La définition visuelle de votre fiche (.dfm)
- Le concepteur visuel

Par exemple :

- Si vous ajoutez un bouton dans le concepteur, Delphi ajoute automatiquement sa déclaration dans la classe de la fiche
- Si vous modifiez la propriété Caption dans le code, la modification se reflète dans le concepteur
- Si vous renommez un composant dans l'Inspecteur d'objets, Delphi met à jour toutes les références dans le code

Cette synchronisation est l'une des grandes forces de Delphi. Elle vous permet de travailler de manière visuelle ou par code selon vos préférences.

## Conseils et bonnes pratiques

### Organisation du code

**Un fichier, une responsabilité** : essayez de ne pas mettre trop de choses différentes dans une seule unité. Si une unité devient trop longue (plus de 1000 lignes), envisagez de la diviser.

**Nommage cohérent** : donnez des noms clairs et descriptifs à vos fichiers et à vos unités. Par exemple, `DataModule` plutôt que `Unit2`, `LoginForm` plutôt que `Form3`.

**Commentaires** : commentez les sections importantes de votre code, surtout les algorithmes complexes. Mais évitez les commentaires évidents comme `// Incrémente i`.

**Sections** : utilisez des commentaires de section pour diviser visuellement votre code :

```pascal
{$REGION 'Méthodes publiques'}
// Votre code ici
{$ENDREGION}
```

Ces régions peuvent être repliées dans l'éditeur pour une meilleure lisibilité.

### Navigation efficace

**Apprenez les raccourcis clavier** : les raccourcis mentionnés dans cette section vous feront gagner énormément de temps. Les plus importants sont :

- **Ctrl + Espace** : auto-complétion
- **Ctrl + clic** : aller à la déclaration
- **Ctrl + F** : rechercher
- **Ctrl + Shift + F** : rechercher dans les fichiers
- **F12** : basculer entre fiche et code
- **Alt + Flèche gauche/droite** : historique de navigation

**Utilisez les marque-pages** : pour marquer les endroits où vous travaillez activement.

**Exploitez la recherche** : au lieu de parcourir manuellement des centaines de lignes, utilisez la recherche pour trouver rapidement ce que vous cherchez.

### Gestion de projet

**Sauvegardez régulièrement** : **Ctrl + S** doit devenir un réflexe. Ou mieux, activez la sauvegarde automatique dans **Outils > Options > Éditeur de code**.

**Utilisez le contrôle de version** : Git, SVN, ou autre. Même pour des projets personnels, c'est une sécurité précieuse.

**Compilez souvent** : ne laissez pas des centaines de lignes de code s'accumuler sans compiler. Compilez régulièrement pour détecter les erreurs tôt.

**Organisez vos fichiers sur le disque** : créez une structure de dossiers claire (par exemple, un dossier Sources pour le code, un dossier Resources pour les images, etc.).

### Personnalisation de l'éditeur

Dans **Outils > Options > Éditeur de code**, vous pouvez personnaliser de nombreux aspects :

- La taille et la police du code
- Les couleurs de la coloration syntaxique
- Les options d'indentation
- Les templates de code
- Les fonctionnalités de Code Insight

N'hésitez pas à explorer ces options pour adapter l'éditeur à vos préférences.

## Erreurs courantes à éviter

**Modifier directement le fichier .dpr** : sauf cas particuliers, laissez Delphi gérer ce fichier.

**Ne pas compiler régulièrement** : si vous accumulez trop de modifications, il devient difficile de localiser les erreurs.

**Ignorer les avertissements** : les avertissements (warnings) ne bloquent pas la compilation, mais ils signalent souvent de vrais problèmes. Prenez-les au sérieux.

**Mauvaise organisation des uses** : avoir trop d'unités dans la clause uses peut ralentir la compilation. N'incluez que ce dont vous avez vraiment besoin.

**Ne pas utiliser les outils de navigation** : parcourir manuellement le code est lent et source d'erreurs. Utilisez les outils de navigation (Ctrl + clic, recherche, etc.).

## Conclusion

L'Explorateur de projets et les outils de gestion de code sont essentiels pour travailler efficacement avec Delphi. Au début, vous utiliserez peut-être principalement la souris et les menus, mais au fur et à mesure, vous découvrirez les raccourcis clavier et les fonctionnalités avancées qui vous feront gagner un temps considérable.

Voici ce qu'il faut retenir :

- L'**Explorateur de projets** vous donne une vue d'ensemble de tous les fichiers de votre application
- Les outils de **navigation dans le code** (Ctrl + clic, recherche, marque-pages) vous permettent de vous déplacer rapidement
- **Code Insight** (auto-complétion, aide sur les paramètres) vous aide à écrire du code plus rapidement et avec moins d'erreurs
- Les outils de **refactoring** vous permettent d'améliorer votre code en toute sécurité
- Les **raccourcis clavier** sont vos meilleurs amis pour la productivité

Plus vous utiliserez ces outils, plus ils deviendront naturels. N'hésitez pas à expérimenter et à découvrir les nombreuses fonctionnalités que Delphi met à votre disposition.

Dans la prochaine section, nous verrons comment compiler et exécuter votre application, et comprendre le processus de compilation de Delphi !

⏭️ [Compilation et exécution](/02-decouverte-de-lide-delphi/05-compilation-et-execution.md)
