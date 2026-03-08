🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.2 Formulaires et fiches

## Introduction

Dans le monde de Delphi, les **formulaires** (ou **fiches** en français) sont les éléments fondamentaux de toute interface graphique. Si la VCL est la boîte à outils, les formulaires sont les toiles sur lesquelles vous allez peindre votre application. Comprendre les formulaires est absolument essentiel pour créer des applications Delphi.

## Qu'est-ce qu'un formulaire ?

Un **formulaire** (en anglais : Form) est une fenêtre Windows que vous créez dans votre application. C'est le conteneur principal qui va accueillir tous vos composants visuels : boutons, zones de texte, images, listes, etc.

### Analogie simple

Pensez à un formulaire comme à une **feuille de papier vierge** sur laquelle vous allez :
- Dessiner des éléments (composants visuels)
- Organiser l'information
- Permettre à l'utilisateur d'interagir avec votre application

Chaque fenêtre que vous voyez dans une application Windows (la fenêtre principale, les boîtes de dialogue, les fenêtres de paramètres) est un formulaire.

## Les différents types de formulaires

Dans Delphi, on distingue généralement plusieurs catégories de formulaires selon leur utilisation :

### 1. Le formulaire principal

C'est le **point d'entrée** de votre application, la première fenêtre qui s'affiche quand l'utilisateur lance votre programme. Chaque application Delphi possède obligatoirement un formulaire principal.

**Caractéristiques :**
- Créé automatiquement lors de la création d'un nouveau projet
- Généralement nommé `Form1` par défaut (mais vous pouvez le renommer)
- Sa fermeture entraîne la fermeture de toute l'application
- Souvent le formulaire le plus important et le plus complexe

### 2. Les formulaires secondaires

Ce sont des **fenêtres additionnelles** que vous créez pour organiser différentes parties de votre application.

**Exemples d'utilisation :**
- Fenêtre "À propos" affichant les informations sur l'application
- Fenêtre de paramètres ou préférences
- Fenêtre d'édition d'un enregistrement
- Fenêtre de recherche avancée
- Tableaux de bord ou statistiques

### 3. Les boîtes de dialogue (Dialogues)

Ce sont des **formulaires spéciaux** conçus pour une interaction ponctuelle avec l'utilisateur. Elles apparaissent, l'utilisateur effectue une action, et elles se ferment.

**Caractéristiques :**
- Généralement modales (bloquent l'accès aux autres fenêtres tant qu'elles sont ouvertes)
- Souvent de petite taille
- Comportent des boutons OK/Annuler ou Oui/Non
- Retournent un résultat à la fenêtre appelante

**Exemples :**
- Confirmation d'une suppression
- Saisie d'un mot de passe
- Choix d'options
- Messages d'erreur ou d'information

### 4. Les formulaires MDI (Multiple Document Interface)

Ce sont des **formulaires conteneurs** qui peuvent héberger plusieurs formulaires enfants, comme dans les anciennes versions de Word ou Excel où vous pouviez ouvrir plusieurs documents dans la même fenêtre principale.

**Note :** Ce type de formulaire est moins utilisé aujourd'hui, mais reste disponible dans Delphi.

## Anatomie d'un formulaire

Un formulaire Delphi est composé de plusieurs éléments :

### 1. La zone cliente (Client Area)

C'est la **surface de travail** du formulaire, la zone où vous placez vos composants. C'est la partie intérieure de la fenêtre, sans compter la barre de titre et les bordures.

### 2. La barre de titre (Title Bar)

C'est la **barre en haut** de la fenêtre qui contient :
- Le titre de la fenêtre (propriété `Caption`)
- L'icône de l'application
- Les boutons Réduire, Agrandir/Restaurer, Fermer

### 3. Les bordures

Ce sont les **contours** de la fenêtre qui permettent de la redimensionner (si le redimensionnement est activé).

### 4. La barre de menu (optionnelle)

Une **barre horizontale** sous la barre de titre qui contient les menus déroulants (Fichier, Édition, Affichage, etc.).

### 5. Les barres d'outils (optionnelles)

Des **barres de boutons** pour accéder rapidement aux fonctions courantes.

### 6. La barre d'état (optionnelle)

Une **barre en bas** de la fenêtre qui affiche des informations sur l'état de l'application.

## Propriétés essentielles d'un formulaire

Chaque formulaire possède de nombreuses propriétés que vous pouvez modifier dans l'Inspecteur d'objets ou par code. Voici les plus importantes pour débuter :

### Propriétés d'apparence

**Caption** : Le texte affiché dans la barre de titre
- Exemple : "Mon Application" ou "Gestion des clients"

**Width et Height** : La largeur et la hauteur du formulaire en pixels
- Exemple : Width = 800, Height = 600

**Left et Top** : La position du formulaire sur l'écran
- Left : distance depuis le bord gauche de l'écran
- Top : distance depuis le bord haut de l'écran

**Color** : La couleur de fond du formulaire
- Exemple : clWhite, clBtnFace, clSkyBlue

**Icon** : L'icône affichée dans la barre de titre et la barre des tâches

**Font** : La police de caractères par défaut pour tous les composants du formulaire

### Propriétés de comportement

**BorderStyle** : Le style de bordure du formulaire, qui détermine si l'utilisateur peut redimensionner la fenêtre
- `bsSizeable` : fenêtre redimensionnable (par défaut)
- `bsDialog` : fenêtre de dialogue, taille fixe
- `bsSingle` : bordure simple, taille fixe
- `bsNone` : pas de bordure (fenêtres splash screen)
- `bsToolWindow` : fenêtre outil avec petite barre de titre
- `bsSizeToolWin` : fenêtre outil redimensionnable

**Position** : La position initiale du formulaire à l'ouverture
- `poDesigned` : position définie lors de la conception
- `poScreenCenter` : centré sur l'écran
- `poMainFormCenter` : centré sur le formulaire principal
- `poOwnerFormCenter` : centré sur le formulaire propriétaire
- `poDesktopCenter` : centré sur le bureau

**WindowState** : L'état initial de la fenêtre
- `wsNormal` : taille normale
- `wsMinimized` : réduite dans la barre des tâches
- `wsMaximized` : agrandie en plein écran

**FormStyle** : Le comportement de la fenêtre
- `fsNormal` : formulaire standard
- `fsMDIForm` : formulaire parent MDI
- `fsMDIChild` : formulaire enfant MDI
- `fsStayOnTop` : reste toujours au premier plan

**Visible** : Détermine si le formulaire est visible ou caché
- `True` : visible
- `False` : caché

### Propriétés de fonctionnalité

**KeyPreview** : Si activé (True), le formulaire reçoit les événements clavier avant les composants
- Utile pour créer des raccourcis clavier globaux

**AutoScroll** : Active le défilement automatique si les composants dépassent la zone visible
- `True` : des barres de défilement apparaissent si nécessaire
- `False` : pas de défilement (par défaut)

**Constraints** : Définit les contraintes de taille minimale et maximale
- `MinWidth`, `MinHeight` : dimensions minimales
- `MaxWidth`, `MaxHeight` : dimensions maximales

## Le cycle de vie d'un formulaire

Un formulaire passe par différentes étapes depuis sa création jusqu'à sa destruction. Comprendre ce cycle de vie est important pour savoir où placer votre code.

### 1. Création (OnCreate)

C'est la **première étape**, quand le formulaire est créé en mémoire mais n'est pas encore visible. C'est ici que vous devez :
- Initialiser vos variables
- Charger des données depuis une base de données
- Configurer les composants
- Établir des connexions

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Code d'initialisation ici
  Caption := 'Bienvenue !';
  // Charger les paramètres, connecter à la BD, etc.
end;
```

### 2. Affichage (OnShow)

Déclenché juste **avant que le formulaire ne devienne visible**. Cet événement se produit à chaque fois que le formulaire est affiché (même après avoir été caché).

Utilisé pour :
- Actualiser des données
- Positionner le focus sur un composant
- Démarrer des animations

### 3. Activation (OnActivate)

Se produit quand le formulaire **obtient le focus** (devient la fenêtre active). Peut se produire plusieurs fois si l'utilisateur bascule entre plusieurs fenêtres.

### 4. Désactivation (OnDeactivate)

Se produit quand le formulaire **perd le focus** (l'utilisateur clique sur une autre fenêtre).

### 5. Masquage (OnHide)

Déclenché quand le formulaire **devient invisible** (mais n'est pas détruit).

### 6. Fermeture (OnClose et OnCloseQuery)

**OnCloseQuery** : Déclenché quand l'utilisateur tente de fermer le formulaire. Vous pouvez annuler la fermeture ici.

```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);  
begin  
  // Demander confirmation
  CanClose := MessageDlg('Voulez-vous vraiment quitter ?',
                          mtConfirmation, [mbYes, mbNo], 0) = mrYes;
end;
```

**OnClose** : Déclenché quand la fermeture est confirmée. Vous définissez ici ce qui se passe (fermer, cacher, libérer la mémoire).

### 7. Destruction (OnDestroy)

C'est la **dernière étape**, quand le formulaire est libéré de la mémoire. Utilisé pour :
- Libérer des ressources
- Fermer des connexions
- Sauvegarder des paramètres

```pascal
procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Libérer les ressources
  // Fermer les connexions
  // Sauvegarder les paramètres
end;
```

## Créer un nouveau formulaire

Dans Delphi, il existe plusieurs façons de créer un nouveau formulaire :

### Méthode 1 : Via le menu

1. Menu **Fichier → Nouveau → Formulaire VCL - Application Delphi**
2. Ou **Fichier → Nouveau → Autre** puis sélectionner "Formulaire VCL"

### Méthode 2 : Clic droit dans l'Explorateur de projets

1. Clic droit sur votre projet
2. Sélectionner **Ajouter** puis **Nouveau formulaire**

Delphi crée alors :
- Un fichier `.pas` contenant le code Object Pascal du formulaire
- Un fichier `.dfm` contenant la description visuelle du formulaire (propriétés, composants)

## Gérer plusieurs formulaires

Dans une application réelle, vous aurez souvent besoin de plusieurs formulaires. Voici comment les gérer :

### Créer et afficher un formulaire secondaire

Il existe deux méthodes principales pour afficher un formulaire :

**Méthode 1 : Affichage modal (ShowModal)**

Le formulaire **bloque** l'accès aux autres fenêtres de l'application tant qu'il n'est pas fermé. Idéal pour les dialogues.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Self);
  try
    if Form2.ShowModal = mrOk then
    begin
      // L'utilisateur a cliqué sur OK
      // Traiter les données du formulaire
    end;
  finally
    Form2.Free;  // Libérer la mémoire
  end;
end;
```

**Méthode 2 : Affichage non-modal (Show)**

Le formulaire s'affiche mais l'utilisateur peut **continuer à interagir** avec les autres fenêtres. Idéal pour les fenêtres d'outils ou de visualisation.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  Form2.Show;  // Affiche Form2 si déjà créé
end;
```

### Communication entre formulaires

Les formulaires ont souvent besoin d'échanger des informations. Voici les méthodes courantes :

**1. Propriétés publiques**

Créer des propriétés publiques dans votre formulaire pour partager des données :

```pascal
type
  TForm2 = class(TForm)
  private
    FNomClient: string;
  public
    property NomClient: string read FNomClient write FNomClient;
  end;

// Utilisation depuis Form1
Form2.NomClient := 'Dupont';
```

**2. Paramètres de constructeur**

Passer des données lors de la création du formulaire :

```pascal
type
  TForm2 = class(TForm)
  public
    constructor Create(AOwner: TComponent; const ANom: string); reintroduce;
  end;

// Utilisation
Form2 := TForm2.Create(Self, 'Dupont');
```

**3. Variables globales ou unités partagées**

Pour des données accessibles partout dans l'application (à utiliser avec modération).

## Bonnes pratiques

### 1. Nommage des formulaires

Donnez des **noms significatifs** à vos formulaires plutôt que de garder Form1, Form2, etc.

**Exemples :**
- `FormPrincipale` au lieu de `Form1`
- `FormLogin` pour une fenêtre de connexion
- `FormParametres` pour les paramètres
- `FormEditClient` pour l'édition d'un client

### 2. Gestion de la mémoire

**Important :** Libérez toujours la mémoire des formulaires créés dynamiquement avec `Free` ou `FreeAndNil`.

Pour les formulaires modaux, utilisez toujours un bloc `try...finally` :

```pascal
Form2 := TForm2.Create(Self);  
try  
  Form2.ShowModal;
finally
  Form2.Free;
end;
```

### 3. Séparation des responsabilités

Chaque formulaire devrait avoir une **responsabilité claire** :
- Un formulaire pour la liste des clients
- Un autre pour éditer un client
- Un autre pour les rapports
- Etc.

Évitez de créer des formulaires "fourre-tout" qui font trop de choses.

### 4. Utilisation de l'auto-création

Dans Delphi, vous pouvez configurer si un formulaire est créé automatiquement au démarrage de l'application ou à la demande.

**Menu Projet → Options → Formulaires** :
- **Formulaires créés automatiquement** : créés au démarrage (consomment de la mémoire)
- **Formulaires disponibles** : créés uniquement quand nécessaire (meilleure gestion de la mémoire)

**Recommandation :** Créez automatiquement uniquement le formulaire principal. Les autres formulaires devraient être créés à la demande.

### 5. Taille et position

Définissez des **tailles adaptées** à votre contenu :
- Pas trop petit (difficile à utiliser)
- Pas trop grand (prend trop de place à l'écran)
- Testez sur différentes résolutions d'écran

Utilisez la propriété `Position` pour un positionnement intelligent :
- `poScreenCenter` pour le formulaire principal
- `poMainFormCenter` pour les dialogues

### 6. Cohérence visuelle

Maintenez une **apparence cohérente** entre tous vos formulaires :
- Même police de caractères
- Mêmes couleurs
- Même disposition des boutons (OK en bas à droite, Annuler à côté, etc.)
- Mêmes icônes et images

## Formulaires et fichiers .DFM

Chaque formulaire Delphi est accompagné d'un fichier `.dfm` (Delphi Form Module). Ce fichier contient :
- La description de tous les composants du formulaire
- Leurs propriétés
- Leur position
- Leur hiérarchie

Ce fichier est en format texte (ou binaire selon la configuration) et est **automatiquement géré** par Delphi. Vous n'avez généralement pas besoin de le modifier manuellement.

Pour visualiser le fichier .dfm :
- Clic droit sur le formulaire → **Afficher en tant que texte**
- Pour revenir à la vue visuelle → clic droit → **Afficher le concepteur**

## Astuces pratiques

### Raccourcis clavier utiles

- **F12** : Basculer entre le code et le formulaire
- **Shift + F12** : Afficher la liste des formulaires
- **Ctrl + F12** : Afficher la liste des unités
- **F9** : Compiler et exécuter

### Copier-coller entre formulaires

Vous pouvez **copier des composants** d'un formulaire et les coller dans un autre :
1. Sélectionnez les composants à copier
2. Ctrl + C
3. Ouvrez l'autre formulaire
4. Ctrl + V

Les événements et le code associé ne sont pas copiés, seulement les composants et leurs propriétés.

### Aligner les composants

Utilisez la **palette d'alignement** pour organiser proprement vos composants :
- Clic droit → **Aligner**
- Ou utilisez la palette d'alignement de l'IDE

### Ancrage et amarrage

Utilisez les propriétés **Align** et **Anchors** pour créer des interfaces qui s'adaptent au redimensionnement :
- **Align** : aligne le composant sur un bord du formulaire
- **Anchors** : garde des distances fixes avec les bords du formulaire

## Conclusion

Les formulaires sont le **cœur de toute application Delphi**. Ils sont à la fois simples à utiliser pour les débutants et suffisamment puissants pour créer des applications professionnelles complexes.

**Points clés à retenir :**
- Un formulaire est une fenêtre Windows qui contient vos composants
- Chaque application a un formulaire principal et peut avoir plusieurs formulaires secondaires
- Les propriétés des formulaires contrôlent leur apparence et leur comportement
- Le cycle de vie d'un formulaire suit des étapes précises (création, affichage, fermeture, destruction)
- La bonne gestion de la mémoire est essentielle pour les formulaires créés dynamiquement
- Organisez votre application en plusieurs formulaires avec des responsabilités claires

Maintenant que vous maîtrisez les concepts fondamentaux des formulaires, vous êtes prêt à explorer les différents composants que vous allez placer dessus pour créer des interfaces utilisateur riches et interactives !

⏭️ [Composants standard et leur utilisation](/04-conception-dinterfaces-utilisateur-avec-la-vcl/03-composants-standard-et-leur-utilisation.md)
