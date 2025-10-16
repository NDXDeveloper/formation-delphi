🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.3 La Palette d'outils et l'Inspecteur d'objets

## Introduction

La Palette d'outils et l'Inspecteur d'objets sont vos deux meilleurs amis dans Delphi. Si le concepteur de fiche est votre toile, la Palette d'outils est votre boîte de peinture, et l'Inspecteur d'objets est votre panneau de contrôle. Ensemble, ces trois éléments vous permettent de créer des interfaces utilisateur de manière visuelle, sans écrire une seule ligne de code.

Dans cette section, nous allons explorer en détail ces deux outils essentiels et apprendre à les maîtriser.

## La Palette d'outils (Tool Palette)

### Qu'est-ce que la Palette d'outils ?

La Palette d'outils est une fenêtre qui contient tous les composants que vous pouvez ajouter à vos fiches. Un composant est un élément de base de votre interface : un bouton, une zone de texte, une image, une grille, etc.

Pensez à la Palette d'outils comme à une immense bibliothèque de briques Lego. Chaque composant est une brique que vous pouvez assembler pour construire votre application.

### Localisation de la Palette d'outils

La Palette d'outils se trouve généralement sur le côté droit de l'IDE, parfois sous forme d'onglet que vous pouvez dérouler. Si vous ne la voyez pas, vous pouvez l'afficher via le menu **Affichage > Palette d'outils**.

### Organisation de la Palette

Les composants dans la Palette ne sont pas présentés en vrac : ils sont organisés en catégories logiques. Chaque catégorie regroupe des composants ayant des fonctions similaires. Voici les catégories principales que vous rencontrerez :

#### Standard

C'est la catégorie la plus importante pour débuter. Elle contient les composants de base que vous utiliserez dans presque toutes vos applications :

**TButton** : un bouton classique sur lequel l'utilisateur peut cliquer. C'est probablement le composant le plus utilisé.

**TLabel** : une étiquette de texte pour afficher des informations. Contrairement à une zone de texte, l'utilisateur ne peut pas modifier son contenu.

**TEdit** : une zone de saisie d'une seule ligne, pour que l'utilisateur entre du texte (nom, email, etc.).

**TMemo** : une zone de saisie multi-lignes, pour des textes plus longs.

**TCheckBox** : une case à cocher pour des options oui/non.

**TRadioButton** : un bouton radio, utilisé quand l'utilisateur doit choisir une seule option parmi plusieurs.

**TListBox** : une liste d'éléments dans laquelle l'utilisateur peut sélectionner.

**TComboBox** : une liste déroulante qui combine une zone de texte et une liste.

**TGroupBox** : un cadre qui permet de regrouper visuellement d'autres composants.

**TPanel** : un panneau qui sert de conteneur pour d'autres composants.

**TMainMenu** : pour créer la barre de menus en haut de votre fenêtre.

**TPopupMenu** : pour créer des menus contextuels (clic droit).

#### Additional

Cette catégorie contient des composants plus spécialisés :

**TBitBtn** : un bouton qui peut contenir une image en plus du texte.

**TSpeedButton** : un bouton plat, souvent utilisé dans les barres d'outils.

**TImage** : pour afficher des images (JPEG, PNG, BMP, etc.).

**TBevel** : pour créer des séparateurs visuels.

**TProgressBar** : une barre de progression pour indiquer l'avancement d'une tâche.

**TTrackBar** : un curseur coulissant (comme un contrôle de volume).

**TSpinEdit** : une zone de saisie numérique avec des boutons +/-.

#### Win32

Composants qui utilisent les contrôles natifs de Windows :

**TPageControl** : pour créer des onglets, comme dans les navigateurs web.

**TTreeView** : pour afficher des données hiérarchiques (comme l'explorateur Windows).

**TListView** : pour afficher des listes avec icônes et détails.

**TToolBar** : pour créer des barres d'outils avec boutons.

**TStatusBar** : la barre d'état en bas de la fenêtre.

**TRichEdit** : un éditeur de texte enrichi (gras, italique, couleurs...).

#### System

Composants qui interagissent avec le système :

**TTimer** : pour déclencher des actions à intervalles réguliers.

**TMediaPlayer** : pour lire des fichiers audio et vidéo.

**TDriveComboBox** : pour sélectionner un lecteur (C:, D:, etc.).

**TDirectoryListBox** : pour naviguer dans les dossiers.

#### Data Access et Data Controls

Ces catégories contiennent des composants pour travailler avec des bases de données. Nous les explorerons en détail dans le chapitre sur les bases de données, mais voici les principaux :

**TDataSource** : le lien entre vos données et les contrôles visuels.

**TDBGrid** : une grille pour afficher et éditer des données tabulaires.

**TDBEdit**, **TDBMemo**, **TDBComboBox** : versions "connectées aux données" des composants standard.

#### Dialogs

Composants pour afficher des boîtes de dialogue standard :

**TOpenDialog** : pour ouvrir un fichier.

**TSaveDialog** : pour enregistrer un fichier.

**TFontDialog** : pour choisir une police.

**TColorDialog** : pour choisir une couleur.

**TPrintDialog** : pour configurer l'impression.

#### Internet et Indy

Composants pour les communications réseau et internet. Nous les verrons dans les chapitres avancés.

### Comment utiliser la Palette d'outils

Utiliser la Palette d'outils est très simple. Il existe plusieurs méthodes :

**Méthode 1 : Cliquer et placer**

1. Cliquez une fois sur le composant que vous voulez dans la Palette (par exemple, TButton)
2. Le curseur de la souris change de forme
3. Cliquez sur votre fiche à l'endroit où vous voulez placer le composant
4. Le composant apparaît à cet emplacement

**Méthode 2 : Double-cliquer**

1. Double-cliquez sur un composant dans la Palette
2. Le composant est automatiquement placé au centre de la fiche active

**Méthode 3 : Glisser-déposer**

1. Cliquez sur le composant dans la Palette et maintenez le bouton enfoncé
2. Faites glisser votre souris vers la fiche
3. Relâchez le bouton à l'endroit souhaité

La méthode 1 est généralement la plus précise et la plus utilisée.

### Rechercher dans la Palette

Avec des centaines de composants disponibles, il peut être difficile de trouver celui dont vous avez besoin. Heureusement, la Palette d'outils dispose d'une fonction de recherche.

En haut de la Palette, vous trouverez un champ de recherche. Tapez simplement le nom (ou une partie du nom) du composant que vous cherchez, et la Palette affichera uniquement les composants correspondants.

Par exemple, si vous tapez "button", vous verrez tous les composants dont le nom contient "button" : TButton, TBitBtn, TSpeedButton, etc.

### Personnaliser la Palette

Vous pouvez personnaliser la Palette selon vos besoins :

**Réorganiser les catégories** : cliquez avec le bouton droit sur la Palette pour accéder aux options de personnalisation.

**Masquer des catégories** : si certaines catégories ne vous intéressent pas, vous pouvez les masquer pour simplifier l'affichage.

**Créer vos propres catégories** : pour regrouper vos composants favoris.

**Ajouter des composants tiers** : quand vous installez de nouvelles bibliothèques de composants, elles apparaissent dans la Palette.

Pour l'instant, la configuration par défaut est largement suffisante pour apprendre.

## L'Inspecteur d'objets (Object Inspector)

### Qu'est-ce que l'Inspecteur d'objets ?

Si la Palette d'outils vous permet d'ajouter des composants, l'Inspecteur d'objets vous permet de les configurer. Chaque composant a des dizaines de paramètres (appelés "propriétés") que vous pouvez modifier : sa taille, sa couleur, son texte, sa position, son comportement, etc.

L'Inspecteur d'objets est votre centre de contrôle pour tous ces réglages.

### Localisation de l'Inspecteur

L'Inspecteur d'objets se trouve généralement à droite de l'IDE, souvent juste à côté de la Palette d'outils. Si vous ne le voyez pas, affichez-le via le menu **Affichage > Inspecteur d'objets**, ou appuyez sur **F11**.

### Structure de l'Inspecteur

L'Inspecteur d'objets se divise en plusieurs parties :

**Le sélecteur d'objets** : en haut, une liste déroulante qui affiche le nom du composant actuellement sélectionné. Vous pouvez utiliser cette liste pour sélectionner rapidement n'importe quel composant de votre fiche.

**Deux onglets principaux** :
- **Propriétés (Properties)** : pour configurer l'apparence et le comportement des composants
- **Événements (Events)** : pour définir ce qui se passe quand l'utilisateur interagit avec le composant

### L'onglet Propriétés

Cet onglet affiche toutes les propriétés du composant sélectionné. Les propriétés sont présentées en deux colonnes :

**Colonne de gauche** : le nom de la propriété

**Colonne de droite** : la valeur actuelle de la propriété

Les propriétés sont triées alphabétiquement par défaut, mais vous pouvez aussi les afficher par catégories en cliquant sur l'icône appropriée en haut de l'Inspecteur.

#### Types de propriétés

Il existe différents types de propriétés, chacune s'éditant différemment :

**Propriétés textuelles** : vous tapez simplement la valeur. Par exemple, la propriété **Caption** d'un bouton.

**Propriétés numériques** : vous entrez un nombre. Par exemple, **Width** (largeur) ou **Height** (hauteur).

**Propriétés booléennes** : vraies ou fausses (True/False). Par exemple, **Visible** (le composant est-il visible ?) ou **Enabled** (le composant est-il actif ?).

**Propriétés énumérées** : une liste de choix prédéfinis. Cliquez sur la flèche pour voir les options. Par exemple, **Align** définit comment le composant se positionne dans son conteneur.

**Propriétés couleurs** : un sélecteur de couleur s'ouvre quand vous cliquez sur la valeur.

**Propriétés polices** : un bouton avec "..." apparaît, qui ouvre un dialogue pour choisir la police.

**Propriétés complexes** : certaines propriétés peuvent être développées en cliquant sur le "+" à gauche. Par exemple, **Font** contient plusieurs sous-propriétés (Name, Size, Color, Style, etc.).

#### Propriétés courantes

Voici les propriétés que vous rencontrerez le plus souvent :

**Name** : le nom interne du composant dans le code. C'est important de donner des noms significatifs, comme "ButtonSave" plutôt que "Button1".

**Caption** : le texte affiché sur le composant (pour les boutons, labels, formulaires, etc.).

**Text** : le contenu textuel (pour les Edit, Memo, etc.).

**Width et Height** : les dimensions en pixels.

**Left et Top** : la position en pixels depuis le coin supérieur gauche.

**Enabled** : si False, le composant est grisé et non-cliquable.

**Visible** : si False, le composant est invisible.

**Color** : la couleur de fond.

**Font** : la police de caractères (famille, taille, style, couleur).

**Hint** : le texte de l'info-bulle qui apparaît quand on survole le composant.

**ShowHint** : active ou désactive l'info-bulle.

**TabOrder** : l'ordre de navigation quand l'utilisateur appuie sur la touche Tab.

**Align** : définit comment le composant s'aligne dans son conteneur (alNone, alTop, alBottom, alLeft, alRight, alClient).

**Anchors** : détermine comment le composant se redimensionne quand la fenêtre change de taille.

### L'onglet Événements

L'onglet Événements est tout aussi important que l'onglet Propriétés. C'est ici que vous définissez le comportement de votre application : que se passe-t-il quand l'utilisateur clique sur un bouton ? Quand il tape dans une zone de texte ? Quand il survole un élément ?

#### Qu'est-ce qu'un événement ?

Un événement est une action qui se produit pendant l'utilisation de votre application. Les événements les plus courants sont :

**OnClick** : déclenché quand l'utilisateur clique sur le composant. C'est l'événement le plus utilisé.

**OnDblClick** : déclenché par un double-clic.

**OnMouseEnter** : déclenché quand la souris entre dans la zone du composant.

**OnMouseLeave** : déclenché quand la souris quitte la zone du composant.

**OnKeyPress** : déclenché quand l'utilisateur appuie sur une touche (pour les composants qui acceptent la saisie).

**OnChange** : déclenché quand le contenu du composant change (pour les Edit, Memo, etc.).

**OnCreate** : pour les formulaires, déclenché à la création de la fiche.

**OnClose** : pour les formulaires, déclenché à la fermeture.

#### Comment associer du code à un événement

Voici comment faire réagir votre application aux actions de l'utilisateur :

1. Sélectionnez le composant sur la fiche
2. Dans l'Inspecteur d'objets, basculez sur l'onglet **Événements**
3. Trouvez l'événement qui vous intéresse (par exemple, OnClick pour un bouton)
4. Double-cliquez dans la case à droite du nom de l'événement

Delphi fait alors plusieurs choses automatiquement :

- Il bascule vers l'éditeur de code
- Il crée une procédure (appelée "gestionnaire d'événement") pour cet événement
- Il place votre curseur entre les lignes `begin` et `end` de cette procédure

C'est entre ces deux lignes que vous écrirez le code qui s'exécutera quand l'événement se produira.

Par exemple, pour un bouton nommé "Button1", Delphi créera :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Votre code ici
end;
```

Vous pouvez alors écrire, par exemple :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Vous avez cliqué sur le bouton !');
end;
```

#### Nommer les gestionnaires d'événements

Par défaut, Delphi nomme les gestionnaires d'événements en combinant le nom du composant et le type d'événement : `Button1Click`, `Edit1Change`, etc.

Vous pouvez donner un nom personnalisé à un gestionnaire d'événement en tapant directement ce nom dans la case de l'événement, avant de double-cliquer. C'est utile quand plusieurs composants doivent déclencher le même code.

## Travailler avec la Palette et l'Inspecteur ensemble

Ces deux outils fonctionnent en tandem. Voici un workflow typique :

1. **Choisir un composant** dans la Palette d'outils
2. **Le placer** sur la fiche
3. **Le sélectionner** (il est normalement déjà sélectionné)
4. **Configurer ses propriétés** dans l'Inspecteur d'objets
5. **Créer des gestionnaires d'événements** si nécessaire
6. **Tester** en compilant et exécutant (F9)

### Sélectionner des composants

Pour sélectionner un composant sur la fiche :

**Clic simple** : sélectionne le composant. Vous voyez des petites poignées carrées autour de lui.

**Ctrl + clic** : pour sélectionner plusieurs composants en même temps.

**Sélection par zone** : cliquez et faites glisser sur la fiche pour créer un rectangle de sélection.

**Via l'Inspecteur** : utilisez la liste déroulante en haut de l'Inspecteur pour sélectionner un composant par son nom.

### Modifier plusieurs composants à la fois

Quand vous sélectionnez plusieurs composants, l'Inspecteur d'objets n'affiche que les propriétés communes à tous ces composants. Vous pouvez alors modifier une propriété pour tous les composants sélectionnés en même temps.

C'est très pratique pour, par exemple, aligner plusieurs boutons, leur donner la même taille, ou la même police.

### Copier, couper, coller des composants

Vous pouvez copier des composants comme vous copiez du texte :

**Ctrl + C** : copier
**Ctrl + X** : couper
**Ctrl + V** : coller

Quand vous collez un composant, Delphi lui donne automatiquement un nouveau nom (Button2, Button3, etc.) et copie toutes ses propriétés.

### Aligner et disposer les composants

Delphi propose des outils pour aligner vos composants de manière professionnelle :

**Menu Edition > Aligner** : propose de nombreuses options d'alignement

**Grille magnétique** : par défaut, les composants se "collent" à une grille invisible pour faciliter l'alignement. Vous pouvez voir cette grille en allant dans **Outils > Options > Concepteur de fiches**.

**Guides d'alignement** : quand vous déplacez un composant, des lignes bleues apparaissent pour vous aider à l'aligner avec les autres composants.

## Astuces et conseils pratiques

### Nommage des composants

Prenez l'habitude de donner des noms significatifs à vos composants. Au lieu de garder "Button1", "Edit1", etc., utilisez des noms qui décrivent la fonction :

- `ButtonSave` au lieu de `Button1`
- `EditName` au lieu de `Edit1`
- `LabelTitle` au lieu de `Label1`

Cela rendra votre code beaucoup plus lisible.

### Convention de nommage

Une convention courante en Delphi est d'utiliser un préfixe indiquant le type de composant :

- `btnSave` pour un bouton
- `edtName` pour une zone de texte
- `lblTitle` pour un label
- `chkAccept` pour une case à cocher
- `cmbCountry` pour une liste déroulante

Choisissez une convention et respectez-la dans tout votre projet.

### Explorer les propriétés

N'hésitez pas à explorer toutes les propriétés d'un composant dans l'Inspecteur. Passez votre souris sur le nom d'une propriété : une info-bulle apparaîtra avec une brève description.

Essayez de modifier les valeurs pour voir ce qui se passe. C'est en expérimentant que vous apprendrez le mieux.

### Utiliser l'aide contextuelle

Si vous êtes dans l'Inspecteur d'objets et que vous voulez plus d'informations sur une propriété, sélectionnez-la et appuyez sur **F1**. L'aide de Delphi s'ouvrira avec la documentation de cette propriété.

### Propriétés au moment de la conception vs au moment de l'exécution

Certaines propriétés peuvent être modifiées uniquement dans l'Inspecteur d'objets (au moment de la conception). D'autres peuvent aussi être modifiées par le code pendant l'exécution de votre programme.

Par exemple, vous pouvez changer le Caption d'un bouton pendant l'exécution :

```pascal
Button1.Caption := 'Nouveau texte';
```

### Organiser visuellement vos composants

Utilisez des **TPanel** ou des **TGroupBox** pour regrouper logiquement vos composants. Cela rend l'interface plus claire et plus professionnelle.

Les **TPanel** peuvent aussi servir de zones redimensionnables en utilisant la propriété **Align**.

## Erreurs courantes à éviter

**Oublier de nommer ses composants** : avec des noms par défaut, votre code devient vite illisible.

**Trop de composants sur une seule fiche** : si votre interface devient trop chargée, envisagez d'utiliser des onglets (TPageControl) ou de diviser en plusieurs fiches.

**Négliger l'ordre de tabulation** : vérifiez la propriété **TabOrder** pour que l'utilisateur puisse naviguer logiquement avec la touche Tab.

**Oublier de définir Anchors** : si vous voulez que votre interface s'adapte au redimensionnement de la fenêtre, configurez les propriétés **Anchors** ou **Align**.

**Modifier directement les propriétés dans le fichier .dfm** : ce fichier est généré automatiquement. Utilisez toujours l'Inspecteur d'objets pour modifier les propriétés, sauf si vous savez exactement ce que vous faites.

## Fonctionnalités avancées

### Verrouiller les composants

Une fois que vous avez positionné vos composants, vous pouvez les verrouiller pour éviter de les déplacer accidentellement. Cliquez avec le bouton droit sur la fiche et sélectionnez **Verrouiller les contrôles**.

### Ordre Z (ordre d'empilement)

Quand plusieurs composants se superposent, vous pouvez contrôler lequel est au-dessus. Cliquez avec le bouton droit sur un composant et choisissez **Ordre de disposition > Amener au premier plan** ou **Envoyer à l'arrière-plan**.

### Modèles de composants

Si vous configurez souvent un composant de la même manière, vous pouvez créer un modèle. Configurez le composant comme vous le souhaitez, puis cliquez avec le bouton droit dessus et choisissez **Créer un modèle de composant**. Ce modèle apparaîtra dans la Palette d'outils pour une réutilisation facile.

## Conclusion

La Palette d'outils et l'Inspecteur d'objets sont au cœur du développement visuel dans Delphi. Maîtriser ces deux outils vous permettra de créer des interfaces rapidement et efficacement.

Voici ce que vous devez retenir :

- La **Palette d'outils** contient tous les composants disponibles, organisés par catégories
- L'**Inspecteur d'objets** vous permet de configurer les **propriétés** et les **événements** de chaque composant
- Les **propriétés** définissent l'apparence et le comportement
- Les **événements** définissent les réactions aux actions de l'utilisateur
- Donnez toujours des **noms significatifs** à vos composants
- **Explorez et expérimentez** pour découvrir toutes les possibilités

Dans la prochaine section, nous découvrirons l'Explorateur de projets et le Gestionnaire de code, pour comprendre comment organiser et naviguer dans des projets plus complexes.

N'hésitez pas à passer du temps à expérimenter avec différents composants et leurs propriétés. C'est en pratiquant que vous deviendrez à l'aise avec ces outils essentiels !

⏭️ [Explorateur de projets et gestionnaire de code](/02-decouverte-de-lide-delphi/04-explorateur-de-projets-et-gestionnaire-de-code.md)
