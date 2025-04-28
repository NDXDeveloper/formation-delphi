# 4.2 Formulaires et fiches

Les formulaires (ou fiches) sont les éléments fondamentaux de toute application Delphi utilisant la VCL. Ils représentent les fenêtres que vos utilisateurs verront et avec lesquelles ils interagiront. Comprendre comment manipuler les formulaires est donc essentiel pour tout développeur Delphi.

## Qu'est-ce qu'un formulaire ?

Un **formulaire** (aussi appelé **fiche** ou **form** en anglais) est simplement une fenêtre dans votre application. C'est un conteneur visuel sur lequel vous placez d'autres composants comme des boutons, des champs de texte ou des images pour créer votre interface utilisateur.

En termes techniques, un formulaire dans Delphi est une instance de la classe `TForm`, qui hérite elle-même de `TCustomForm`.

## Création d'un formulaire

### Création automatique lors d'un nouveau projet

Lorsque vous créez un nouveau projet d'application VCL dans Delphi, un premier formulaire (`Form1`) est automatiquement généré. Deux fichiers sont alors créés :

- **Un fichier .pas** (par exemple, `Unit1.pas`) : contient le code Object Pascal de votre formulaire
- **Un fichier .dfm** (par exemple, `Unit1.dfm`) : contient la description visuelle de votre formulaire

### Ajouter un nouveau formulaire à un projet existant

Pour ajouter un nouveau formulaire à votre projet :

1. Cliquez sur **Fichier** > **Nouveau** > **Form - VCL**
2. Enregistrez la nouvelle unité (Delphi vous y invitera automatiquement)
3. Le nouveau formulaire est ajouté à votre projet

## Structure d'un formulaire

Examinons la structure du code généré pour un formulaire simple :

```pascal
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm1 = class(TForm)
    // Déclarations des composants placés sur le formulaire

    // Déclarations des gestionnaires d'événements
    procedure FormCreate(Sender: TObject);
  private
    // Déclarations privées
  public
    // Déclarations publiques
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Code exécuté lors de la création du formulaire
end;

end.
```

### Parties importantes :

1. **La section `interface`** : contient les déclarations des composants et méthodes
2. **La déclaration de classe `TForm1`** : définit votre formulaire personnalisé
3. **La variable globale `Form1`** : représente l'instance de votre formulaire
4. **La directive `{$R *.dfm}`** : lie le fichier de ressources .dfm (aspect visuel) au code
5. **Les sections `private` et `public`** : pour organiser vos méthodes et propriétés

## Propriétés importantes des formulaires

Les formulaires possèdent de nombreuses propriétés que vous pouvez configurer dans l'Inspecteur d'objets. Voici les plus couramment utilisées :

### Apparence

- **Caption** : texte affiché dans la barre de titre
- **Color** : couleur de fond du formulaire
- **Font** : police de caractères par défaut pour les composants
- **Icon** : icône affichée dans la barre de titre et la barre des tâches
- **BorderStyle** : style de bordure (redimensionnable, fixe, etc.)
- **BorderIcons** : boutons disponibles dans la barre de titre (minimiser, maximiser, fermer)

### Position et taille

- **Width** et **Height** : largeur et hauteur du formulaire
- **Left** et **Top** : position à l'écran
- **Position** : positionnement initial (centré, par défaut, etc.)
- **WindowState** : état initial (normal, minimisé, maximisé)

### Comportement

- **FormStyle** : style du formulaire (normal, MDI parent/enfant, etc.)
- **Visible** : si le formulaire est visible au démarrage
- **AlphaBlend** et **AlphaBlendValue** : transparence du formulaire
- **KeyPreview** : si le formulaire intercepte les frappes clavier avant les composants

## Événements importants des formulaires

Les formulaires répondent à de nombreux événements. Voici les plus courants :

- **OnCreate** : déclenché lors de la création du formulaire en mémoire
- **OnShow** : déclenché juste avant que le formulaire ne devienne visible
- **OnClose** : déclenché lorsque le formulaire est sur le point d'être fermé
- **OnCloseQuery** : permet de valider/annuler la fermeture du formulaire
- **OnResize** : déclenché lorsque la taille du formulaire change
- **OnActivate** et **OnDeactivate** : lorsque le formulaire gagne ou perd le focus

Pour gérer un événement, double-cliquez sur son nom dans l'Inspecteur d'objets, et Delphi créera automatiquement un gestionnaire d'événement vide.

## Cycle de vie d'un formulaire

Comprendre le cycle de vie d'un formulaire est important pour gérer correctement les ressources :

1. **Création** (`Create`) : allocation en mémoire du formulaire
2. **Initialisation** (événement `OnCreate`) : configuration initiale
3. **Affichage** (événement `OnShow`) : le formulaire devient visible
4. **Interactions utilisateur** : l'utilisateur interagit avec le formulaire
5. **Fermeture** (événements `OnCloseQuery` et `OnClose`) : validation et fermeture
6. **Destruction** (`Destroy`) : libération des ressources

## Manipulation des formulaires par code

### Création dynamique

Vous pouvez créer des formulaires dynamiquement pendant l'exécution :

```pascal
var
  Form2: TForm2;
begin
  Form2 := TForm2.Create(Application);
  try
    Form2.ShowModal; // Affiche comme boîte de dialogue modale
  finally
    Form2.Free; // Libère les ressources
  end;
end;
```

### Modes d'affichage

Il existe deux façons principales d'afficher un formulaire :

1. **Mode modal** (`ShowModal`) : bloque l'interaction avec les autres fenêtres jusqu'à la fermeture
   ```pascal
   if Form2.ShowModal = mrOk then
     // L'utilisateur a cliqué sur OK
   ```

2. **Mode non modal** (`Show`) : permet à l'utilisateur d'interagir avec d'autres fenêtres
   ```pascal
   Form2.Show; // Affiche le formulaire et continue l'exécution
   ```

## Les fiches (frames)

Les **fiches** (ou **frames** en anglais) représentent une autre approche pour organiser votre interface utilisateur.

### Qu'est-ce qu'une fiche (frame) ?

Une fiche est un conteneur réutilisable pour des groupes de composants. Contrairement aux formulaires, les fiches ne peuvent pas exister seules - elles doivent être placées sur un formulaire ou une autre fiche.

Les fiches sont des instances de la classe `TFrame`.

### Avantages des fiches

- **Réutilisabilité** : créez une fiche une fois, utilisez-la dans plusieurs formulaires
- **Encapsulation** : regroupez des composants liés et leur code
- **Modularité** : divisez une interface complexe en modules plus simples

### Création d'une fiche

1. Cliquez sur **Fichier** > **Nouveau** > **Frame - VCL**
2. Ajoutez des composants à la fiche comme vous le feriez pour un formulaire
3. Enregistrez la fiche

### Utilisation d'une fiche

Pour utiliser une fiche dans un formulaire :

1. Assurez-vous que l'unité de la fiche est dans la clause `uses` du formulaire
2. Placez un composant `TFrame` sur votre formulaire
3. Sélectionnez votre classe de fiche personnalisée dans l'Inspecteur d'objets

Ou créez-la dynamiquement par code :

```pascal
var
  MyFrame: TMyCustomFrame;
begin
  MyFrame := TMyCustomFrame.Create(Self);
  MyFrame.Parent := Panel1; // La fiche doit avoir un parent
  MyFrame.Align := alClient; // Remplir tout l'espace disponible
end;
```

## Conseils pratiques

### Organisation des formulaires multiples

- Donnez des noms significatifs à vos formulaires (ex: `TCustomerForm` au lieu de `TForm1`)
- Préfixez les fichiers .pas/.dfm pour faciliter leur identification (ex: `frm_Customers.pas`)
- Documentez le rôle de chaque formulaire en commentaire au début du fichier

### Performance

- Ne créez pas tous les formulaires au démarrage, créez-les à la demande
- Libérez les formulaires non utilisés pour économiser de la mémoire
- Utilisez `Application.CreateForm` uniquement pour le formulaire principal

## Conclusion

Les formulaires et les fiches sont les éléments de base de l'interface utilisateur dans une application Delphi VCL. Maîtriser leur création et leur manipulation est essentiel pour développer des applications professionnelles.

Dans le prochain chapitre, nous explorerons les composants standard de la VCL et apprendrons comment les utiliser efficacement sur vos formulaires.

---

*Astuce pour débutants : N'hésitez pas à explorer les différentes propriétés des formulaires dans l'Inspecteur d'objets. C'est le meilleur moyen d'apprendre leurs fonctionnalités. Essayez de modifier certaines propriétés et observez leur effet sur votre application.*
