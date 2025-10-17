🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.5 Menus et barres d'outils

## Introduction

Les menus et les barres d'outils sont des éléments essentiels de toute application Windows professionnelle. Ils permettent à l'utilisateur d'accéder rapidement aux fonctionnalités de votre programme. Dans ce chapitre, nous allons découvrir comment créer et gérer ces composants dans Delphi.

## 4.5.1 Les menus principaux (MainMenu)

### Qu'est-ce qu'un MainMenu ?

Le **MainMenu** est le menu principal de votre application, généralement situé en haut de la fenêtre, juste sous la barre de titre. C'est le menu que vous voyez dans presque tous les logiciels Windows avec des éléments comme "Fichier", "Edition", "Affichage", etc.

### Pourquoi utiliser un menu ?

- Organisation logique des fonctionnalités
- Accès standardisé aux commandes
- Raccourcis clavier pour les utilisateurs avancés
- Conformité aux standards Windows
- Gain d'espace par rapport aux boutons

### Créer un menu principal

#### Étape 1 : Ajouter le composant MainMenu

1. Dans la palette d'outils, recherchez **MainMenu** (catégorie "Standard")
2. Double-cliquez dessus ou glissez-le sur votre formulaire
3. Le composant apparaît dans la zone des composants non visuels (en bas du formulaire)

#### Étape 2 : Concevoir la structure du menu

1. Double-cliquez sur le composant **MainMenu1**
2. Le concepteur de menu s'ouvre
3. Cliquez sur le rectangle vide pour créer un élément de menu
4. Dans l'Inspecteur d'objets, modifiez la propriété **Caption**

**Exemple de structure classique :**
```
Fichier
  ├─ Nouveau
  ├─ Ouvrir...
  ├─ Enregistrer
  ├─ Enregistrer sous...
  ├─ ──────────── (séparateur)
  └─ Quitter

Edition
  ├─ Annuler
  ├─ Rétablir
  ├─ ──────────── (séparateur)
  ├─ Couper
  ├─ Copier
  └─ Coller

Aide
  ├─ Aide
  └─ À propos...
```

#### Étape 3 : Créer des éléments de menu

**Pour créer un élément de premier niveau :**
- Tapez le nom dans le rectangle vide (ex: "Fichier")
- Appuyez sur Entrée

**Pour créer un sous-élément :**
- Cliquez sur l'élément parent
- Tapez le nom dans le rectangle qui apparaît en dessous
- Appuyez sur Entrée

**Pour créer un séparateur :**
- Créez un élément de menu
- Définissez sa propriété **Caption** à : `-`

### Propriétés importantes d'un élément de menu

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| **Caption** | Texte affiché | `&Fichier` (le & crée un raccourci Alt+F) |
| **Name** | Nom de l'élément dans le code | `MenuFichierOuvrir` |
| **ShortCut** | Raccourci clavier | `Ctrl+O` |
| **Checked** | Affiche une coche | `True` ou `False` |
| **Enabled** | Active/désactive l'élément | `True` ou `False` |
| **Visible** | Affiche/masque l'élément | `True` ou `False` |
| **ImageIndex** | Index de l'icône | `0, 1, 2...` |
| **RadioItem** | Comportement bouton radio | `True` ou `False` |
| **GroupIndex** | Groupe pour RadioItem | `1, 2, 3...` |

### Raccourcis clavier et mnémoniques

#### Mnémoniques (touches d'accès Alt)

Utilisez le caractère `&` avant une lettre pour créer un raccourci Alt :

```pascal
MenuFichier.Caption := '&Fichier';      // Alt+F
MenuOuvrir.Caption := '&Ouvrir...';     // Alt+O
MenuEnregistrer.Caption := '&Enregistrer'; // Alt+E
```

#### Raccourcis clavier (ShortCut)

Les raccourcis permettent d'accéder directement à une fonction :

**Définir un raccourci visuellement :**
1. Sélectionnez l'élément de menu
2. Dans l'Inspecteur d'objets, cliquez sur **ShortCut**
3. Choisissez la combinaison dans la liste déroulante

**Raccourcis standards :**
- `Ctrl+N` : Nouveau
- `Ctrl+O` : Ouvrir
- `Ctrl+S` : Enregistrer
- `Ctrl+Z` : Annuler
- `Ctrl+Y` : Rétablir
- `Ctrl+X` : Couper
- `Ctrl+C` : Copier
- `Ctrl+V` : Coller
- `F1` : Aide

### Gérer les événements de menu

L'événement le plus important est **OnClick**, déclenché quand l'utilisateur clique sur l'élément.

```pascal
// Double-cliquez sur l'élément de menu dans le concepteur
// pour créer automatiquement la procédure OnClick

procedure TForm1.MenuFichierNouveauClick(Sender: TObject);
begin
  ShowMessage('Création d''un nouveau document');
  // Votre code ici
end;

procedure TForm1.MenuFichierOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    // Charger le fichier sélectionné
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;

procedure TForm1.MenuFichierEnregistrerClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    ShowMessage('Fichier enregistré avec succès');
  end;
end;

procedure TForm1.MenuFichierQuitterClick(Sender: TObject);
begin
  Close; // Ferme l'application
end;
```

### Menus dynamiques

Vous pouvez activer/désactiver ou cocher des éléments par code :

```pascal
// Désactiver un élément
MenuEnregistrer.Enabled := False;

// Activer un élément
MenuAnnuler.Enabled := True;

// Cocher un élément
MenuAffichageBarreOutils.Checked := True;

// Décocher un élément
MenuAffichageBarreOutils.Checked := False;

// Alterner une coche
MenuAffichageBarreOutils.Checked := not MenuAffichageBarreOutils.Checked;

// Masquer un élément
MenuAdmin.Visible := False;
```

### Exemple complet : Menu avec gestion d'état

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser l'état des menus
  MenuFichierEnregistrer.Enabled := False; // Pas de document ouvert
  MenuEditionAnnuler.Enabled := False;
  MenuEditionColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TForm1.MenuFichierNouveauClick(Sender: TObject);
begin
  Memo1.Clear;
  MenuFichierEnregistrer.Enabled := True; // Activer l'enregistrement
  Caption := 'Mon Application - Nouveau document';
end;

procedure TForm1.MenuEditionAnnulerClick(Sender: TObject);
begin
  Memo1.Undo;
  // Désactiver si plus d'annulation possible
  MenuEditionAnnuler.Enabled := Memo1.CanUndo;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  // Activer "Annuler" si le contenu a changé
  MenuEditionAnnuler.Enabled := Memo1.Modified;
end;
```

---

## 4.5.2 Les menus contextuels (PopupMenu)

### Qu'est-ce qu'un PopupMenu ?

Le **PopupMenu** est un menu qui s'affiche lorsque l'utilisateur fait un clic droit sur un composant. C'est le menu contextuel que vous voyez dans l'Explorateur Windows, par exemple.

### Créer un menu contextuel

#### Étape 1 : Ajouter le composant

1. Recherchez **PopupMenu** dans la palette d'outils
2. Placez-le sur votre formulaire (zone des composants non visuels)

#### Étape 2 : Créer les éléments

1. Double-cliquez sur le **PopupMenu1**
2. Créez les éléments comme pour un MainMenu
3. Fermez le concepteur

#### Étape 3 : Associer le menu à un composant

Dans l'Inspecteur d'objets du composant cible :
1. Trouvez la propriété **PopupMenu**
2. Sélectionnez **PopupMenu1** dans la liste

### Exemple de menu contextuel pour un Memo

```pascal
// Structure du menu contextuel :
// - Couper
// - Copier
// - Coller
// ────────
// - Tout sélectionner

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Le menu contextuel est déjà lié via la propriété PopupMenu
  // du Memo1 en mode conception
end;

procedure TForm1.PopupCoupeClick(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TForm1.PopupCopierClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TForm1.PopupCollerClick(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TForm1.PopupToutSelectionnerClick(Sender: TObject);
begin
  Memo1.SelectAll;
end;

// Événement déclenché avant l'affichage du menu
procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  // Activer/désactiver les éléments selon le contexte
  PopupCouper.Enabled := Memo1.SelLength > 0;
  PopupCopier.Enabled := Memo1.SelLength > 0;
  PopupColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;
```

### Afficher un menu contextuel par code

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  P: TPoint;
begin
  // Obtenir la position de la souris
  P := Mouse.CursorPos;

  // Afficher le menu à cette position
  PopupMenu1.Popup(P.X, P.Y);
end;

// Ou afficher à la position du composant
procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    // Convertir les coordonnées locales en coordonnées écran
    PopupMenu1.Popup(Image1.ClientToScreen(Point(X, Y)).X,
                     Image1.ClientToScreen(Point(X, Y)).Y);
  end;
end;
```

---

## 4.5.3 Les barres d'outils (ToolBar)

### Qu'est-ce qu'une ToolBar ?

La **ToolBar** (barre d'outils) est une barre contenant des boutons avec icônes permettant un accès rapide aux fonctions les plus utilisées. Elle se trouve généralement juste sous le menu principal.

### Créer une barre d'outils

#### Étape 1 : Ajouter les composants nécessaires

Vous aurez besoin de :
- **ToolBar** : La barre elle-même
- **ImageList** : Pour stocker les icônes
- **ToolButton** : Les boutons de la barre

1. Ajoutez un **ToolBar** sur votre formulaire
2. Ajoutez un **ImageList** (zone non visuelle)
3. Reliez l'ImageList au ToolBar via la propriété **Images**

#### Étape 2 : Charger les images

1. Double-cliquez sur **ImageList1**
2. Cliquez sur "Add" pour ajouter des images
3. Chargez vos icônes (format PNG, BMP, ou ICO)
4. Taille recommandée : 16x16 ou 24x24 pixels

#### Étape 3 : Ajouter des boutons

**Méthode 1 : Visuelle**
1. Cliquez droit sur la ToolBar
2. Sélectionnez "New Button"
3. Dans l'Inspecteur d'objets :
   - Définissez **ImageIndex** (numéro de l'icône)
   - Définissez **Hint** (info-bulle)
   - Créez l'événement **OnClick**

**Méthode 2 : Par code**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  BoutonNouveau, BoutonOuvrir, BoutonEnregistrer: TToolButton;
  Separateur: TToolButton;
begin
  ToolBar1.Images := ImageList1;
  ToolBar1.ShowCaptions := False; // Masquer les textes

  // Bouton Nouveau
  BoutonNouveau := TToolButton.Create(ToolBar1);
  BoutonNouveau.Parent := ToolBar1;
  BoutonNouveau.ImageIndex := 0;
  BoutonNouveau.Hint := 'Nouveau (Ctrl+N)';
  BoutonNouveau.OnClick := MenuFichierNouveauClick; // Réutiliser le code du menu

  // Bouton Ouvrir
  BoutonOuvrir := TToolButton.Create(ToolBar1);
  BoutonOuvrir.Parent := ToolBar1;
  BoutonOuvrir.ImageIndex := 1;
  BoutonOuvrir.Hint := 'Ouvrir (Ctrl+O)';
  BoutonOuvrir.OnClick := MenuFichierOuvrirClick;

  // Séparateur
  Separateur := TToolButton.Create(ToolBar1);
  Separateur.Parent := ToolBar1;
  Separateur.Style := tbsSeparator;
  Separateur.Width := 8;

  // Bouton Enregistrer
  BoutonEnregistrer := TToolButton.Create(ToolBar1);
  BoutonEnregistrer.Parent := ToolBar1;
  BoutonEnregistrer.ImageIndex := 2;
  BoutonEnregistrer.Hint := 'Enregistrer (Ctrl+S)';
  BoutonEnregistrer.OnClick := MenuFichierEnregistrerClick;
end;
```

### Types de boutons (Style)

| Style | Description |
|-------|-------------|
| **tbsButton** | Bouton normal (par défaut) |
| **tbsCheck** | Bouton à bascule (pressé/relâché) |
| **tbsDropDown** | Bouton avec menu déroulant |
| **tbsSeparator** | Séparateur visuel |
| **tbsDivider** | Ligne de séparation verticale |

### Propriétés importantes du ToolBar

| Propriété | Description |
|-----------|-------------|
| **Images** | ImageList contenant les icônes |
| **ShowCaptions** | Afficher le texte sous les icônes |
| **Flat** | Style plat moderne |
| **List** | Affichage en mode liste |
| **Wrapable** | Permet le passage à la ligne |
| **ShowHint** | Afficher les info-bulles |
| **ButtonHeight** | Hauteur des boutons |
| **ButtonWidth** | Largeur des boutons |

### Propriétés importantes du ToolButton

| Propriété | Description |
|-----------|-------------|
| **ImageIndex** | Index de l'icône |
| **Caption** | Texte du bouton |
| **Hint** | Info-bulle |
| **Style** | Type de bouton |
| **Down** | État enfoncé (pour tbsCheck) |
| **Enabled** | Actif ou désactivé |
| **Grouped** | Grouper avec d'autres boutons |
| **DropdownMenu** | Menu pour tbsDropDown |

### Exemple complet : Barre d'outils avec états

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration de la barre d'outils
  ToolBar1.ShowCaptions := False;
  ToolBar1.ShowHint := True;
  ToolBar1.Flat := True;

  // Initialiser l'état des boutons
  ToolButtonEnregistrer.Enabled := False;
  ToolButtonAnnuler.Enabled := False;
end;

procedure TForm1.ToolButtonNouveauClick(Sender: TObject);
begin
  Memo1.Clear;
  ToolButtonEnregistrer.Enabled := True;
  Caption := 'Mon Éditeur - Nouveau document';
end;

procedure TForm1.ToolButtonAnnulerClick(Sender: TObject);
begin
  Memo1.Undo;
  ToolButtonAnnuler.Enabled := Memo1.CanUndo;
end;

// Bouton à bascule pour afficher/masquer une règle
procedure TForm1.ToolButtonRegleClick(Sender: TObject);
begin
  Panel1.Visible := ToolButtonRegle.Down;
end;

// Bouton avec menu déroulant
procedure TForm1.ToolButtonZoomClick(Sender: TObject);
begin
  // Le menu se déclenche automatiquement
  // si DropdownMenu est défini
end;
```

---

## 4.5.4 Les ActionList (Liste d'actions)

### Qu'est-ce qu'une ActionList ?

Une **ActionList** est un composant puissant qui centralise la gestion des actions de votre application. Au lieu de dupliquer le code entre un menu et un bouton de barre d'outils, vous créez une action unique qui peut être liée à plusieurs contrôles.

### Avantages des ActionList

- **Code centralisé** : Une seule procédure pour plusieurs contrôles
- **Gestion d'état automatique** : Activer/désactiver tous les contrôles liés en une fois
- **Raccourcis clavier intégrés** : Définis une seule fois
- **Maintenance facilitée** : Modifier une action met à jour tous les contrôles
- **Organisation** : Toutes les actions regroupées au même endroit

### Créer et utiliser une ActionList

#### Étape 1 : Ajouter le composant

1. Ajoutez un **ActionList** sur votre formulaire
2. Double-cliquez dessus pour ouvrir l'éditeur d'actions

#### Étape 2 : Créer des actions

1. Cliquez sur "Nouvelle Action" (icône +)
2. Définissez les propriétés :
   - **Name** : Nom de l'action (ex: `ActFichierNouveau`)
   - **Caption** : Texte affiché (`&Nouveau`)
   - **ShortCut** : Raccourci clavier (`Ctrl+N`)
   - **ImageIndex** : Index de l'icône
   - **Hint** : Info-bulle

#### Étape 3 : Lier les contrôles aux actions

**Pour un élément de menu :**
1. Sélectionnez l'élément
2. Dans la propriété **Action**, choisissez l'action

**Pour un bouton de barre d'outils :**
1. Sélectionnez le ToolButton
2. Dans la propriété **Action**, choisissez l'action

### Exemple complet avec ActionList

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Associer l'ImageList aux actions
  ActionList1.Images := ImageList1;

  // L'état initial peut être défini ici
  ActFichierEnregistrer.Enabled := False;
  ActEditionAnnuler.Enabled := False;
end;

// Action Nouveau
procedure TForm1.ActFichierNouveauExecute(Sender: TObject);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Voulez-vous enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActFichierEnregistrerExecute(Sender);
      mrCancel: Exit;
    end;
  end;

  Memo1.Clear;
  Memo1.Modified := False;
  ActFichierEnregistrer.Enabled := True;
  Caption := 'Mon Éditeur - Nouveau document';
end;

// Action Ouvrir
procedure TForm1.ActFichierOuvrirExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    Caption := 'Mon Éditeur - ' + ExtractFileName(OpenDialog1.FileName);
    ActFichierEnregistrer.Enabled := True;
  end;
end;

// Action Enregistrer
procedure TForm1.ActFichierEnregistrerExecute(Sender: TObject);
begin
  if SaveDialog1.FileName = '' then
  begin
    if SaveDialog1.Execute then
    begin
      Memo1.Lines.SaveToFile(SaveDialog1.FileName);
      Memo1.Modified := False;
      Caption := 'Mon Éditeur - ' + ExtractFileName(SaveDialog1.FileName);
    end;
  end
  else
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    Memo1.Modified := False;
  end;
end;

// Gestion automatique de l'état
procedure TForm1.ActFichierEnregistrerUpdate(Sender: TObject);
begin
  // OnUpdate est appelé régulièrement
  // pour mettre à jour l'état de l'action
  ActFichierEnregistrer.Enabled := Memo1.Modified;
end;

procedure TForm1.ActEditionAnnulerUpdate(Sender: TObject);
begin
  ActEditionAnnuler.Enabled := Memo1.CanUndo;
end;

procedure TForm1.ActEditionCollerUpdate(Sender: TObject);
begin
  ActEditionColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

// Actions d'édition
procedure TForm1.ActEditionCoupeExecute(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TForm1.ActEditionCopierExecute(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TForm1.ActEditionCollerExecute(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;

procedure TForm1.ActEditionAnnulerExecute(Sender: TObject);
begin
  Memo1.Undo;
end;
```

### Actions standard intégrées

Delphi propose des actions prédéfinies dans les catégories suivantes :

**Actions de fichier :**
- `TFileOpen`, `TFileSaveAs`, `TFileExit`

**Actions d'édition :**
- `TEditCut`, `TEditCopy`, `TEditPaste`, `TEditSelectAll`, `TEditUndo`, `TEditDelete`

**Actions de recherche :**
- `TSearchFind`, `TSearchReplace`, `TSearchFindNext`

**Actions d'aide :**
- `THelpContents`, `THelpTopicSearch`

Pour les utiliser :
1. Dans l'éditeur d'ActionList, cliquez sur la flèche à côté de "Nouvelle Action"
2. Sélectionnez "Nouvelle Action Standard"
3. Choisissez l'action dans la liste

### Catégories d'actions

Vous pouvez organiser vos actions en catégories :

```pascal
// Définir la catégorie d'une action
ActFichierNouveau.Category := 'Fichier';
ActFichierOuvrir.Category := 'Fichier';
ActEditionCouper.Category := 'Edition';
ActEditionCopier.Category := 'Edition';
```

---

## 4.5.5 Exemple complet d'application avec menus et barres d'outils

Voici un exemple d'éditeur de texte simple intégrant tous les concepts :

```pascal
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ToolWin, ComCtrls, ImgList, ActnList;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuEdition: TMenuItem;
    MenuAide: TMenuItem;
    PopupMenu1: TPopupMenu;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    ActionList1: TActionList;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;

    // Actions
    ActFichierNouveau: TAction;
    ActFichierOuvrir: TAction;
    ActFichierEnregistrer: TAction;
    ActFichierQuitter: TAction;
    ActEditionCouper: TAction;
    ActEditionCopier: TAction;
    ActEditionColler: TAction;
    ActEditionAnnuler: TAction;
    ActAideAPropos: TAction;

    // Éléments de menu liés aux actions
    MenuFichierNouveau: TMenuItem;
    MenuFichierOuvrir: TMenuItem;
    MenuFichierEnregistrer: TMenuItem;
    N1: TMenuItem; // Séparateur
    MenuFichierQuitter: TMenuItem;

    MenuEditionAnnuler: TMenuItem;
    N2: TMenuItem;
    MenuEditionCouper: TMenuItem;
    MenuEditionCopier: TMenuItem;
    MenuEditionColler: TMenuItem;

    MenuAideAPropos: TMenuItem;

    // Boutons de barre d'outils liés aux actions
    ToolButtonNouveau: TToolButton;
    ToolButtonOuvrir: TToolButton;
    ToolButtonEnregistrer: TToolButton;
    ToolButtonSep1: TToolButton;
    ToolButtonCouper: TToolButton;
    ToolButtonCopier: TToolButton;
    ToolButtonColler: TToolButton;

    procedure FormCreate(Sender: TObject);
    procedure ActFichierNouveauExecute(Sender: TObject);
    procedure ActFichierOuvrirExecute(Sender: TObject);
    procedure ActFichierEnregistrerExecute(Sender: TObject);
    procedure ActFichierQuitterExecute(Sender: TObject);
    procedure ActEditionCoupeExecute(Sender: TObject);
    procedure ActEditionCopierExecute(Sender: TObject);
    procedure ActEditionCollerExecute(Sender: TObject);
    procedure ActEditionAnnulerExecute(Sender: TObject);
    procedure ActAideAProposExecute(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure ActFichierEnregistrerUpdate(Sender: TObject);
    procedure ActEditionAnnulerUpdate(Sender: TObject);
    procedure ActEditionCollerUpdate(Sender: TObject);
    procedure ActEditionCoupeUpdate(Sender: TObject);
  private
    FNomFichier: string;
    procedure MettreAJourTitre;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration
  ActionList1.Images := ImageList1;
  Memo1.Clear;
  FNomFichier := '';
  MettreAJourTitre;

  // Configuration des dialogues
  OpenDialog1.Filter := 'Fichiers texte (*.txt)|*.TXT|Tous les fichiers (*.*)|*.*';
  SaveDialog1.Filter := OpenDialog1.Filter;
  SaveDialog1.DefaultExt := 'txt';
end;

procedure TForm1.MettreAJourTitre;
begin
  if FNomFichier = '' then
    Caption := 'Éditeur de texte - Sans titre'
  else
    Caption := 'Éditeur de texte - ' + ExtractFileName(FNomFichier);

  if Memo1.Modified then
    Caption := Caption + ' *';
end;

procedure TForm1.ActFichierNouveauExecute(Sender: TObject);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActFichierEnregistrerExecute(Sender);
      mrCancel: Exit;
    end;
  end;

  Memo1.Clear;
  FNomFichier := '';
  Memo1.Modified := False;
  MettreAJourTitre;
  StatusBar1.SimpleText := 'Nouveau document créé';
end;

procedure TForm1.ActFichierOuvrirExecute(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    FNomFichier := OpenDialog1.FileName;
    Memo1.Modified := False;
    MettreAJourTitre;
    StatusBar1.SimpleText := 'Fichier ouvert : ' + FNomFichier;
  end;
end;

procedure TForm1.ActFichierEnregistrerExecute(Sender: TObject);
begin
  if FNomFichier = '' then
  begin
    if SaveDialog1.Execute then
      FNomFichier := SaveDialog1.FileName
    else
      Exit;
  end;

  Memo1.Lines.SaveToFile(FNomFichier);
  Memo1.Modified := False;
  MettreAJourTitre;
  StatusBar1.SimpleText := 'Fichier enregistré : ' + FNomFichier;
end;

procedure TForm1.ActFichierQuitterExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.ActEditionCoupeExecute(Sender: TObject);
begin
  Memo1.CutToClipboard;
  StatusBar1.SimpleText := 'Texte coupé';
end;

procedure TForm1.ActEditionCopierExecute(Sender: TObject);
begin
  Memo1.CopyToClipboard;
  StatusBar1.SimpleText := 'Texte copié';
end;

procedure TForm1.ActEditionCollerExecute(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
  StatusBar1.SimpleText := 'Texte collé';
end;

procedure TForm1.ActEditionAnnulerExecute(Sender: TObject);
begin
  Memo1.Undo;
  StatusBar1.SimpleText := 'Annulation effectuée';
end;

procedure TForm1.ActAideAProposExecute(Sender: TObject);
begin
  ShowMessage('Éditeur de texte simple' + #13#10 +
              'Version 1.0' + #13#10 +
              'Créé avec Delphi');
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  MettreAJourTitre;
  StatusBar1.SimpleText := Format('Lignes: %d  Caractères: %d',
    [Memo1.Lines.Count, Length(Memo1.Text)]);
end;

// Mise à jour automatique des états
procedure TForm1.ActFichierEnregistrerUpdate(Sender: TObject);
begin
  ActFichierEnregistrer.Enabled := Memo1.Modified;
end;

procedure TForm1.ActEditionAnnulerUpdate(Sender: TObject);
begin
  ActEditionAnnuler.Enabled := Memo1.CanUndo;
end;

procedure TForm1.ActEditionCollerUpdate(Sender: TObject);
begin
  ActEditionColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TForm1.ActEditionCoupeUpdate(Sender: TObject);
begin
  ActEditionCouper.Enabled := Memo1.SelLength > 0;
end;

end.
```

---

## 4.5.6 Bonnes pratiques

### Organisation des menus

1. **Suivez les conventions Windows** :
   - Fichier, Edition, Affichage, Outils, Aide (dans cet ordre)
   - Placez "Quitter" en dernier dans le menu Fichier
   - Utilisez les raccourcis standards

2. **Groupez logiquement** :
   - Utilisez des séparateurs pour grouper les fonctions similaires
   - Limitez à 5-7 éléments par groupe

3. **Accessibilité** :
   - Définissez des mnémoniques (Alt+Lettre) uniques
   - Ajoutez des raccourcis clavier pour les fonctions fréquentes
   - Affichez les raccourcis dans les info-bulles

### Conception des barres d'outils

1. **Sélectionnez les actions principales** :
   - Limitez-vous aux 8-12 actions les plus utilisées
   - N'ajoutez pas toutes les fonctions du menu

2. **Icônes claires** :
   - Utilisez des icônes standard et reconnaissables
   - Taille cohérente (16x16 ou 24x24)
   - Style homogène

3. **Organisation logique** :
   - Groupez les boutons par fonction
   - Utilisez des séparateurs

4. **Info-bulles descriptives** :
   - Incluez le nom de la fonction et le raccourci
   - Exemple : "Enregistrer (Ctrl+S)"

### Utilisation des ActionList

1. **Centralisez la logique** :
   - Une action = une fonction
   - Évitez de dupliquer le code

2. **Nommage cohérent** :
   - Préfixez avec le nom du menu : `ActFichierNouveau`
   - Ou par catégorie : `ActNouveau`, `ActOuvrir`

3. **Utilisez OnUpdate** :
   - Pour activer/désactiver automatiquement
   - Vérifie régulièrement les conditions

4. **Organisez en catégories** :
   - Facilite la maintenance
   - Améliore la lisibilité

---

## 4.5.7 Personnalisation avancée

### Afficher/masquer la barre d'outils

```pascal
procedure TForm1.MenuAffichageBarreOutilsClick(Sender: TObject);
begin
  ToolBar1.Visible := not ToolBar1.Visible;
  MenuAffichageBarreOutils.Checked := ToolBar1.Visible;
end;
```

### Barre d'outils personnalisable

```pascal
// Permettre à l'utilisateur de personnaliser
procedure TForm1.ToolBar1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  // Afficher un menu de personnalisation
  PopupMenuToolBar.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;
```

### Menus dynamiques (liste récente)

```pascal
procedure TForm1.AjouterFichierRecent(const NomFichier: string);
var
  MenuItem: TMenuItem;
begin
  // Créer un nouvel élément de menu
  MenuItem := TMenuItem.Create(MenuFichier);
  MenuItem.Caption := ExtractFileName(NomFichier);
  MenuItem.OnClick := OuvrirFichierRecent;
  MenuItem.Tag := PtrInt(NomFichier); // Stocker le chemin complet

  // Insérer avant le séparateur et "Quitter"
  MenuFichier.Insert(MenuFichier.Count - 2, MenuItem);
end;

procedure TForm1.OuvrirFichierRecent(Sender: TObject);
var
  NomFichier: string;
begin
  NomFichier := string((Sender as TMenuItem).Tag);
  if FileExists(NomFichier) then
  begin
    Memo1.Lines.LoadFromFile(NomFichier);
    FNomFichier := NomFichier;
  end
  else
    ShowMessage('Fichier introuvable');
end;
```

---

## Conclusion

Les menus et barres d'outils sont des éléments essentiels d'une interface utilisateur professionnelle. En utilisant les composants MainMenu, PopupMenu, ToolBar et ActionList de manière cohérente, vous créerez des applications intuitives et conformes aux standards Windows.

### Points clés à retenir :

- **MainMenu** : Menu principal de l'application
- **PopupMenu** : Menu contextuel (clic droit)
- **ToolBar** : Accès rapide aux fonctions principales
- **ActionList** : Centralisation de la logique et des états
- **Cohérence** : Respectez les conventions et standards
- **Accessibilité** : Raccourcis clavier et mnémoniques

Avec ces outils, vous êtes prêt à créer des interfaces riches et efficaces pour vos applications Delphi !

⏭️ [Gestion des événements](/04-conception-dinterfaces-utilisateur-avec-la-vcl/06-gestion-des-evenements.md)
