🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.1 Gestion des formulaires multiples

## Introduction

Dans les applications Delphi, un formulaire (ou fiche) représente une fenêtre de votre application. Jusqu'à présent, vous avez peut-être travaillé avec un seul formulaire principal, mais la plupart des applications réelles nécessitent plusieurs fenêtres pour organiser les fonctionnalités de manière claire et intuitive.

La gestion des formulaires multiples est une compétence essentielle pour créer des applications professionnelles avec Delphi. Elle vous permet de :

- Séparer les différentes fonctionnalités de votre application
- Créer des boîtes de dialogue pour recueillir des informations
- Afficher des fenêtres d'options ou de paramètres
- Organiser votre interface utilisateur de manière modulaire

## Comprendre les formulaires dans Delphi

### Qu'est-ce qu'un formulaire ?

Un formulaire dans Delphi est une classe qui hérite de `TForm`. Chaque formulaire possède :

- Une partie visuelle (l'interface utilisateur)
- Une partie code (la logique de l'application)
- Des propriétés configurables (titre, taille, position, etc.)
- Des événements (OnCreate, OnShow, OnClose, etc.)

### Le formulaire principal

Lorsque vous créez un nouveau projet Delphi, un formulaire principal est automatiquement créé. C'est le point d'entrée de votre application. Lorsque ce formulaire se ferme, l'application se termine généralement.

## Créer un nouveau formulaire

### Étapes de création

1. Dans l'IDE Delphi, allez dans le menu **Fichier** → **Nouveau** → **Fiche VCL** (pour une application VCL) ou **Fiche FireMonkey** (pour une application multi-plateforme)

2. Un nouveau formulaire vierge apparaît dans l'éditeur

3. Deux fichiers sont créés automatiquement :
   - Un fichier `.pas` (le code Object Pascal)
   - Un fichier `.dfm` ou `.fmx` (la description visuelle du formulaire)

### Structure d'un formulaire

Voici à quoi ressemble le code d'un formulaire nouvellement créé :

```pascal
unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm2 = class(TForm)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

end.
```

**Points importants :**

- `TForm2` est le nom de la classe du formulaire
- `Form2` est une variable globale qui représente l'instance du formulaire
- La section `interface` contient les déclarations visibles par les autres unités
- La section `implementation` contient le code de mise en œuvre

## Afficher un formulaire secondaire

### Méthode 1 : Show (affichage non-modal)

Un formulaire non-modal permet à l'utilisateur d'interagir avec d'autres fenêtres de l'application pendant qu'il est affiché.

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.Show;
end;
```

**Caractéristiques de Show :**
- L'utilisateur peut passer d'une fenêtre à l'autre librement
- Le code continue son exécution immédiatement après l'appel
- Idéal pour des fenêtres d'outils ou de palettes

### Méthode 2 : ShowModal (affichage modal)

Un formulaire modal bloque l'interaction avec les autres fenêtres jusqu'à ce qu'il soit fermé.

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.ShowModal;
  // Le code ici ne s'exécute qu'après la fermeture de Form2
end;
```

**Caractéristiques de ShowModal :**
- L'utilisateur doit fermer cette fenêtre avant de continuer
- Le code s'arrête à cet appel jusqu'à la fermeture du formulaire
- Idéal pour les boîtes de dialogue et les formulaires de saisie
- Retourne une valeur (ModalResult) qui indique comment le formulaire a été fermé

### Récupérer le résultat d'un formulaire modal

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if Form2.ShowModal = mrOk then
  begin
    ShowMessage('L''utilisateur a cliqué sur OK');
  end
  else if Form2.ModalResult = mrCancel then
  begin
    ShowMessage('L''utilisateur a annulé');
  end;
end;
```

Les valeurs `ModalResult` courantes :
- `mrOk` : Validation (bouton OK)
- `mrCancel` : Annulation (bouton Annuler)
- `mrYes` / `mrNo` : Réponses Oui/Non
- `mrAbort`, `mrRetry`, `mrIgnore` : Autres options

## Gestion de la création et destruction des formulaires

### Auto-création des formulaires

Par défaut, Delphi crée automatiquement tous les formulaires au démarrage de l'application. Vous pouvez voir et modifier cette liste dans **Projet** → **Options** → **Formulaires**.

**Avantages :**
- Simple à utiliser
- Les formulaires sont toujours disponibles

**Inconvénients :**
- Consomme de la mémoire dès le démarrage
- Ralentit le démarrage de l'application si vous avez beaucoup de formulaires

### Création manuelle des formulaires

Pour une meilleure gestion de la mémoire, vous pouvez créer les formulaires à la demande :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  MonFormulaire: TForm2;
begin
  MonFormulaire := TForm2.Create(Self);
  try
    MonFormulaire.ShowModal;
  finally
    MonFormulaire.Free;
  end;
end;
```

**Explications :**
- `TForm2.Create(Self)` crée une nouvelle instance du formulaire
- `Self` indique que Form1 est le propriétaire (owner)
- Le bloc `try...finally` garantit que le formulaire est libéré même en cas d'erreur
- `Free` libère la mémoire occupée par le formulaire

### Vérifier si un formulaire existe

Avant d'utiliser un formulaire, il est prudent de vérifier s'il existe :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  if not Assigned(Form2) then
    Form2 := TForm2.Create(Application);

  Form2.Show;
end;
```

`Assigned()` retourne `True` si la variable contient une référence valide à un objet.

## Masquer et réafficher un formulaire

Plutôt que de créer et détruire un formulaire à chaque utilisation, vous pouvez le masquer et le réafficher :

```pascal
// Masquer un formulaire
Form2.Hide;

// ou
Form2.Visible := False;

// Réafficher un formulaire
Form2.Show;

// ou
Form2.Visible := True;
```

**Avantage :** Le formulaire conserve son état (valeurs des champs, position, etc.)

**Inconvénient :** Le formulaire reste en mémoire

## Fermer un formulaire

### Depuis le formulaire lui-même

```pascal
procedure TForm2.Button1Click(Sender: TObject);
begin
  Close;  // Ferme le formulaire
end;
```

### Pour un formulaire modal, définir le ModalResult

```pascal
procedure TForm2.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOk;  // Ferme automatiquement le formulaire
end;

procedure TForm2.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
```

**Astuce :** Vous pouvez définir la propriété `ModalResult` d'un bouton directement dans l'Inspecteur d'objets, ce qui évite d'écrire du code.

## Passer des données entre formulaires

### Méthode 1 : Utiliser les propriétés publiques

Dans Form2, ajoutez une propriété publique :

```pascal
type
  TForm2 = class(TForm)
    Edit1: TEdit;
  private
    FNomUtilisateur: string;
  public
    property NomUtilisateur: string read FNomUtilisateur write FNomUtilisateur;
  end;
```

Depuis Form1, accédez à cette propriété :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.NomUtilisateur := 'Jean Dupont';
  Form2.ShowModal;
end;
```

### Méthode 2 : Accéder directement aux composants

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.Edit1.Text := 'Valeur initiale';
  if Form2.ShowModal = mrOk then
  begin
    ShowMessage('Valeur saisie : ' + Form2.Edit1.Text);
  end;
end;
```

**Note :** Cette méthode couple fortement les formulaires. La première méthode est préférable pour un code plus maintenable.

### Méthode 3 : Constructeur personnalisé

Créez un constructeur qui accepte des paramètres :

```pascal
type
  TForm2 = class(TForm)
  private
    FNomUtilisateur: string;
  public
    constructor Create(AOwner: TComponent; const ANom: string); reintroduce;
  end;

implementation

constructor TForm2.Create(AOwner: TComponent; const ANom: string);
begin
  inherited Create(AOwner);
  FNomUtilisateur := ANom;
end;
```

Utilisation :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  MonForm: TForm2;
begin
  MonForm := TForm2.Create(Self, 'Jean Dupont');
  try
    MonForm.ShowModal;
  finally
    MonForm.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Gérer la mémoire correctement

Toujours libérer les formulaires créés manuellement :

```pascal
// BON
MonForm := TForm2.Create(Self);
try
  MonForm.ShowModal;
finally
  MonForm.Free;
end;

// MAUVAIS - Fuite mémoire
MonForm := TForm2.Create(Self);
MonForm.ShowModal;
// Le formulaire n'est jamais libéré !
```

### 2. Utiliser des variables locales pour les formulaires temporaires

```pascal
// BON - Variable locale
procedure TForm1.AfficherOptions;
var
  FormOptions: TFormOptions;
begin
  FormOptions := TFormOptions.Create(Self);
  try
    FormOptions.ShowModal;
  finally
    FormOptions.Free;
  end;
end;
```

### 3. Éviter les références circulaires

Si Form1 fait référence à Form2 et que Form2 fait référence à Form1, cela peut créer des problèmes. Utilisez plutôt des événements ou des interfaces pour communiquer.

### 4. Ne pas accéder directement aux variables globales des formulaires

```pascal
// MAUVAIS
procedure TForm1.Button1Click(Sender: TObject);
begin
  Form2.Edit1.Text := 'test';  // Couplage fort
end;

// BON
procedure TForm1.Button1Click(Sender: TObject);
var
  F: TForm2;
begin
  F := TForm2.Create(Self);
  try
    F.ConfigurerAvec('test');  // Méthode encapsulée
    F.ShowModal;
  finally
    F.Free;
  end;
end;
```

### 5. Gérer l'événement OnClose correctement

```pascal
procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Pour un formulaire modal, ne rien faire de spécial
  Action := caHide;

  // Pour un formulaire créé dynamiquement
  // Action := caFree;  // Libère automatiquement le formulaire
end;
```

## Position et taille des formulaires

### Définir la position d'affichage

```pascal
// Au centre de l'écran
Form2.Position := poScreenCenter;

// Au centre du formulaire parent
Form2.Position := poMainFormCenter;

// Position personnalisée
Form2.Position := poDesigned;  // Utilise les coordonnées définies
Form2.Left := 100;
Form2.Top := 100;
```

### Adapter la taille

```pascal
// Taille fixe
Form2.BorderStyle := bsDialog;  // L'utilisateur ne peut pas redimensionner

// Taille ajustable
Form2.BorderStyle := bsSizeable;

// Définir les contraintes
Form2.Constraints.MinWidth := 400;
Form2.Constraints.MinHeight := 300;
```

## Ordre d'affichage (Z-Order)

Pour mettre un formulaire au premier plan :

```pascal
Form2.BringToFront;
```

Pour l'envoyer à l'arrière-plan :

```pascal
Form2.SendToBack;
```

## Résumé

La gestion des formulaires multiples dans Delphi offre une grande flexibilité pour organiser votre application. Les points clés à retenir :

- **Show** pour les fenêtres non-modales (interaction libre)
- **ShowModal** pour les boîtes de dialogue (bloque l'interaction)
- Toujours gérer la création et la destruction des formulaires
- Utiliser des propriétés et méthodes pour passer des données entre formulaires
- Respecter les bonnes pratiques pour éviter les fuites mémoire
- Définir le `ModalResult` pour faciliter la communication avec les formulaires modaux

La maîtrise de ces concepts vous permettra de créer des applications Delphi avec des interfaces utilisateur riches et bien organisées.

⏭️ [Communication entre formulaires](/06-applications-multi-fenetres-et-navigation/02-communication-entre-formulaires.md)
