# 2.3 La Palette d'outils et l'Inspecteur d'objets

Deux des outils les plus importants dans l'environnement Delphi sont la **Palette d'outils** et l'**Inspecteur d'objets**. Ces outils sont essentiels pour le développement visuel et permettent de construire des interfaces utilisateur rapidement, sans écrire de code pour chaque élément. Dans cette section, nous allons explorer en détail ces deux composants fondamentaux de l'IDE Delphi.

## La Palette d'outils

La Palette d'outils est votre "boîte à outils" contenant tous les composants que vous pouvez ajouter à vos formulaires. C'est comme un catalogue organisé de tous les éléments visuels et non-visuels que vous pouvez utiliser dans vos applications.

### Emplacement et apparence

Par défaut, la Palette d'outils se trouve généralement sur le côté droit de l'IDE Delphi. Si elle n'est pas visible :

1. Allez dans le menu **Affichage > Palette d'outils**
2. Ou utilisez le raccourci clavier **Alt+F11**

![Palette d'outils](https://placeholder.com/delphi-tool-palette)

### Organisation de la Palette d'outils

La Palette est organisée en plusieurs onglets thématiques qui regroupent les composants par catégorie. Voici les principales catégories que vous utiliserez fréquemment :

- **Standard** : Composants de base comme Button, Label, Edit, Memo, CheckBox, etc.
- **Additional** : Composants supplémentaires comme BitBtn, ColorBox, etc.
- **Win32** : Composants spécifiques à Windows comme TrackBar, ProgressBar, etc.
- **System** : Composants non visuels comme Timer, ImageList, etc.
- **Data Access** : Composants pour l'accès aux bases de données
- **Data Controls** : Contrôles liés aux données comme DBGrid, DBEdit, etc.
- Et bien d'autres selon les fonctionnalités installées...

> **Astuce :** Au début, concentrez-vous sur l'onglet "Standard" qui contient les composants les plus couramment utilisés.

### Utilisation de la Palette d'outils

Pour utiliser un composant de la Palette d'outils :

1. **Recherche d'un composant** : Vous pouvez utiliser la zone de recherche en haut de la palette pour trouver rapidement un composant
2. **Sélection d'un composant** : Cliquez une fois sur le composant désiré dans la palette
3. **Placement sur le formulaire** : Cliquez à l'endroit du formulaire où vous souhaitez placer le composant
4. **Création de plusieurs instances** : Maintenez la touche **Shift** enfoncée pendant que vous cliquez sur le formulaire pour placer plusieurs instances du même composant
5. **Annulation de la sélection** : Appuyez sur la touche **Echap** ou cliquez sur le bouton de sélection (flèche) en haut de la palette pour revenir au mode sélection

> **Note pour les débutants :** Les composants avec un fond bleu sont des composants visuels (visibles à l'exécution). Ceux avec un fond gris sont des composants non visuels (invisibles à l'exécution mais fournissant des fonctionnalités).

### Personnalisation de la Palette d'outils

Delphi vous permet de personnaliser la Palette d'outils selon vos besoins :

1. Cliquez avec le bouton droit sur la palette et sélectionnez **Personnaliser...**
2. Vous pouvez réorganiser les onglets, masquer des composants rarement utilisés ou créer vos propres onglets personnalisés
3. Pour les débutants, il est recommandé de conserver la configuration par défaut jusqu'à ce que vous soyez plus familier avec l'environnement

## L'Inspecteur d'objets

L'Inspecteur d'objets est un outil essentiel qui vous permet de visualiser et de modifier les propriétés et les événements des composants sélectionnés sans écrire de code.

### Emplacement et apparence

Par défaut, l'Inspecteur d'objets se trouve généralement sur le côté gauche de l'IDE Delphi. Si vous ne le voyez pas :

1. Allez dans le menu **Affichage > Inspecteur d'objets**
2. Ou utilisez le raccourci clavier **F11**

![Inspecteur d'objets](https://placeholder.com/delphi-object-inspector)

### Structure de l'Inspecteur d'objets

L'Inspecteur d'objets comporte principalement deux onglets :

1. **Propriétés** : Liste toutes les caractéristiques du composant sélectionné que vous pouvez modifier
2. **Événements** : Liste tous les événements auxquels le composant peut répondre

En haut de l'Inspecteur d'objets, vous trouverez :
- Une liste déroulante montrant le composant actuellement sélectionné
- Une zone de recherche pour trouver rapidement une propriété ou un événement

### L'onglet Propriétés

L'onglet Propriétés vous permet de configurer l'apparence et le comportement d'un composant. Voici quelques propriétés communes que vous rencontrerez souvent :

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| Name | Nom du composant utilisé dans le code | ButtonOK |
| Caption | Texte affiché sur le composant | "Cliquez ici" |
| Color | Couleur d'arrière-plan | clBlue |
| Enabled | Active ou désactive le composant | True/False |
| Visible | Affiche ou cache le composant | True/False |
| Font | Police de caractères utilisée | Times New Roman, 12pt, Bold |
| Height, Width | Dimensions du composant | 25, 100 |

#### Types de propriétés spéciales

L'Inspecteur d'objets affiche différents types de propriétés avec des éditeurs spécifiques :

1. **Propriétés simples** : Valeurs que vous pouvez saisir directement (chaînes, nombres)
2. **Propriétés booléennes** : Choix entre True et False via une liste déroulante
3. **Propriétés énumérées** : Sélection parmi une liste de valeurs prédéfinies
4. **Propriétés composées** : Identifiées par un "+" à leur gauche, elles contiennent des sous-propriétés
5. **Propriétés avec éditeur spécial** : Identifiées par "..." à droite, elles ouvrent un éditeur dédié quand vous cliquez dessus

![Types de propriétés](https://placeholder.com/delphi-property-types)

> **Astuce pratique :** Double-cliquez sur une propriété booléenne pour basculer rapidement entre True et False.

### L'onglet Événements

L'onglet Événements liste tous les événements auxquels un composant peut répondre. Les événements sont des actions qui peuvent se produire pendant l'exécution de votre application, comme un clic de souris ou une pression de touche.

Pour créer un gestionnaire d'événement :

1. Sélectionnez l'onglet **Événements** dans l'Inspecteur d'objets
2. Double-cliquez dans la colonne de droite à côté de l'événement souhaité (par exemple, "OnClick")
3. Delphi crée automatiquement une procédure de gestion d'événement et vous amène à l'éditeur de code
4. Écrivez votre code entre les lignes `begin` et `end`

Voici quelques événements couramment utilisés :

| Événement | Se déclenche quand... | Utilisation typique |
|-----------|------------------------|---------------------|
| OnClick | L'utilisateur clique sur le composant | Réagir à une action de l'utilisateur |
| OnChange | Le contenu d'un contrôle de saisie change | Validation en temps réel |
| OnKeyPress | Une touche est pressée | Filtrer les caractères saisis |
| OnMouseMove | La souris se déplace sur le composant | Effets de survol |
| OnCreate | Le formulaire est créé | Initialisation du formulaire |

> **Pour les débutants :** Au début, l'événement OnClick sera probablement celui que vous utiliserez le plus souvent.

### Sélection de composants multiples

Vous pouvez sélectionner plusieurs composants à la fois pour modifier des propriétés communes :

1. Maintenez la touche **Shift** ou **Ctrl** enfoncée tout en cliquant sur plusieurs composants
2. Ou dessinez un cadre de sélection autour des composants à sélectionner
3. L'Inspecteur d'objets n'affichera que les propriétés communes à tous les composants sélectionnés

Cette fonctionnalité est particulièrement utile pour aligner plusieurs composants, leur donner la même taille ou la même couleur.

## Astuces et techniques efficaces

### Raccourcis clavier utiles

| Raccourci | Action |
|-----------|--------|
| F11 | Afficher/masquer l'Inspecteur d'objets |
| Alt+F11 | Afficher/masquer la Palette d'outils |
| F4 | Basculer entre l'onglet Propriétés et l'onglet Événements |
| Ctrl+F6 | Basculer entre les fenêtres de l'IDE |

### Bonnes pratiques

1. **Conventions de nommage** : Adoptez une convention cohérente pour nommer vos composants
   - Exemple : `ButtonSave`, `LabelNom`, `EditAdresse`
   - Préfixez le nom avec le type de composant pour faciliter l'autocomplétion

2. **Organisation des composants** : Utilisez des panneaux (TPanel) ou des GroupBox pour regrouper des composants liés

3. **Documentation rapide** : Utilisez la propriété "Hint" pour ajouter des infobulles à vos composants (pensez également à définir ShowHint = True)

4. **Utilisation des ancres** : La propriété "Anchors" vous permet de spécifier comment un composant doit se redimensionner lorsque son parent change de taille

## Exercice pratique

Pour vous familiariser avec ces outils, créez un petit formulaire de contact :

1. Ajoutez des Labels pour les champs : Nom, Prénom, Email, Message
2. Ajoutez les composants de saisie correspondants : Edit pour Nom, Prénom, Email et Memo pour Message
3. Ajoutez un Button "Envoyer"
4. Utilisez l'Inspecteur d'objets pour personnaliser l'apparence (couleurs, polices)
5. Créez un gestionnaire d'événement OnClick pour le bouton Envoyer qui affiche un message de confirmation

## Conclusion

La Palette d'outils et l'Inspecteur d'objets sont deux piliers de la philosophie RAD (Rapid Application Development) de Delphi. La maîtrise de ces outils est essentielle pour développer efficacement des applications. En pratiquant régulièrement, vous deviendrez de plus en plus à l'aise avec ces outils, ce qui accélérera considérablement votre processus de développement.

Dans la prochaine section, nous explorerons l'Explorateur de projets et le gestionnaire de code, qui vous aideront à organiser et naviguer dans les fichiers de votre projet.
