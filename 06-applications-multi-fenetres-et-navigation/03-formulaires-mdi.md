# 6.3 Formulaires MDI (Multiple Document Interface)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'interface à documents multiples (MDI) est un modèle d'interface graphique qui permet d'ouvrir plusieurs documents dans des fenêtres enfants à l'intérieur d'une fenêtre principale. Ce chapitre vous guidera dans la création et la gestion d'applications MDI avec Delphi.

## Qu'est-ce qu'une interface MDI ?

Une interface MDI (Multiple Document Interface) se compose de :

- **Une fenêtre parent (conteneur)** : la fenêtre principale de l'application
- **Plusieurs fenêtres enfants** : chacune contenant un document ou une vue différente
- **Les fenêtres enfants sont confinées** à l'intérieur de la fenêtre parent
- **Un menu Fenêtre (Window)** permettant de basculer entre les différentes fenêtres enfants

Les applications MDI sont particulièrement adaptées pour :
- Éditeurs de texte/code avec multiples documents (comme Notepad++)
- Applications de dessin ou de CAO
- Gestionnaires de bases de données
- Applications où l'utilisateur travaille sur plusieurs documents similaires simultanément

## Créer une application MDI simple

### Étape 1 : Créer le formulaire parent MDI

1. Créez un nouveau projet VCL Forms Application
2. Sur le formulaire principal, définissez la propriété `FormStyle` à `fsMDIForm`
3. Ajoutez un menu principal (TMainMenu) avec une structure de base
4. Ajoutez une barre d'état (TStatusBar) pour afficher des informations

```pascal
// Structure de base du formulaire parent
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuExit: TMenuItem;
    mnuWindow: TMenuItem;
    mnuCascade: TMenuItem;
    mnuTileHorizontal: TMenuItem;
    mnuTileVertical: TMenuItem;
    mnuArrangeIcons: TMenuItem;
    StatusBar1: TStatusBar;
    procedure mnuNewClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuCascadeClick(Sender: TObject);
    procedure mnuTileHorizontalClick(Sender: TObject);
    procedure mnuTileVerticalClick(Sender: TObject);
    procedure mnuArrangeIconsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FChildCount: Integer;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses ChildForm; // Unité du formulaire enfant
```

### Étape 2 : Créer le formulaire enfant MDI

1. Ajoutez un nouveau formulaire au projet (File → New → Form)
2. Définissez sa propriété `FormStyle` à `fsMDIChild`

```pascal
// Structure de base du formulaire enfant
unit ChildForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmChild = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
  private
    FDocumentName: string;
  public
    property DocumentName: string read FDocumentName write FDocumentName;
  end;

implementation

{$R *.dfm}

uses MainForm;
```

### Étape 3 : Implémenter les fonctionnalités de base

Implémentez les gestionnaires d'événements dans le formulaire principal :

```pascal
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FChildCount := 0;
  Caption := 'Application MDI - Delphi';
  StatusBar1.SimpleText := 'Prêt';
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
var
  ChildForm: TfrmChild;
begin
  // Créer une nouvelle instance du formulaire enfant
  ChildForm := TfrmChild.Create(Application);
  Inc(FChildCount);

  // Configurer le formulaire enfant
  ChildForm.Caption := 'Document ' + IntToStr(FChildCount);
  ChildForm.DocumentName := 'Document ' + IntToStr(FChildCount);

  // Afficher le nombre de fenêtres ouvertes
  StatusBar1.SimpleText := 'Documents ouverts: ' + IntToStr(FChildCount);
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close; // Fermer l'application
end;

// Fonctions de gestion de la disposition des fenêtres
procedure TfrmMain.mnuCascadeClick(Sender: TObject);
begin
  Cascade; // Organiser les fenêtres en cascade
end;

procedure TfrmMain.mnuTileHorizontalClick(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile; // Organiser les fenêtres horizontalement
end;

procedure TfrmMain.mnuTileVerticalClick(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile; // Organiser les fenêtres verticalement
end;

procedure TfrmMain.mnuArrangeIconsClick(Sender: TObject);
begin
  ArrangeIcons; // Organiser les icônes des fenêtres minimisées
end;
```

Implémentez les gestionnaires d'événements dans le formulaire enfant :

```pascal
procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Décrémenter le compteur de fenêtres
  Dec(frmMain.FChildCount);
  frmMain.StatusBar1.SimpleText := 'Documents ouverts: ' + IntToStr(frmMain.FChildCount);

  // Définir l'action de fermeture pour libérer la mémoire
  Action := caFree;
end;

procedure TfrmChild.FormActivate(Sender: TObject);
begin
  // Mettre à jour la barre d'état avec le nom du document actif
  frmMain.StatusBar1.SimpleText := 'Document actif: ' + DocumentName;
end;
```

## Améliorer l'application MDI

### Ajouter un menu Window dynamique

Pour faciliter la navigation entre les fenêtres enfants, ajoutons une liste dynamique au menu Window :

1. Ajoutez une ligne séparatrice dans le menu Window
2. Ajoutez un gestionnaire d'événements sur le menu Window

```pascal
// Dans le formulaire principal
procedure TfrmMain.mnuWindowClick(Sender: TObject);
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  // Supprimer les anciens éléments de menu (au-delà des éléments fixes)
  while mnuWindow.Count > 5 do // 4 éléments fixes + 1 séparateur
    mnuWindow.Delete(5);

  // Ajouter une entrée pour chaque fenêtre enfant
  for i := 0 to MDIChildCount - 1 do
  begin
    MenuItem := TMenuItem.Create(mnuWindow);
    MenuItem.Caption := '&' + IntToStr(i + 1) + ' ' + MDIChildren[i].Caption;
    MenuItem.Tag := i; // Stocker l'index
    MenuItem.OnClick := MDIChildClick;
    MenuItem.Checked := MDIChildren[i] = ActiveMDIChild; // Cocher l'enfant actif
    mnuWindow.Add(MenuItem);
  end;
end;

procedure TfrmMain.MDIChildClick(Sender: TObject);
begin
  if Sender is TMenuItem then
    MDIChildren[TMenuItem(Sender).Tag].BringToFront;
end;
```

### Gestion plus avancée des fenêtres enfants

Ajoutons la possibilité d'ouvrir et sauvegarder des fichiers :

```pascal
// Dans le formulaire principal, ajoutez ces méthodes
procedure TfrmMain.mnuOpenClick(Sender: TObject);
var
  ChildForm: TfrmChild;
  OpenDialog: TOpenDialog;
  FileName: string;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    OpenDialog.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
    if OpenDialog.Execute then
    begin
      FileName := OpenDialog.FileName;

      // Vérifier si le fichier est déjà ouvert
      if IsFileOpen(FileName) then
      begin
        ShowMessage('Ce fichier est déjà ouvert !');
        Exit;
      end;

      // Créer une nouvelle fenêtre enfant
      ChildForm := TfrmChild.Create(Application);
      Inc(FChildCount);

      // Charger le fichier
      ChildForm.LoadFromFile(FileName);
      ChildForm.Caption := ExtractFileName(FileName);
      ChildForm.DocumentName := FileName;

      StatusBar1.SimpleText := 'Documents ouverts: ' + IntToStr(FChildCount);
    end;
  finally
    OpenDialog.Free;
  end;
end;

function TfrmMain.IsFileOpen(const FileName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to MDIChildCount - 1 do
  begin
    if TfrmChild(MDIChildren[i]).DocumentName = FileName then
    begin
      Result := True;
      MDIChildren[i].BringToFront;
      Break;
    end;
  end;
end;
```

Dans le formulaire enfant, ajoutez les méthodes pour charger et sauvegarder :

```pascal
procedure TfrmChild.LoadFromFile(const FileName: string);
begin
  Memo1.Lines.LoadFromFile(FileName);
  FDocumentName := FileName;
  Modified := False;
end;

procedure TfrmChild.SaveToFile(const FileName: string);
begin
  Memo1.Lines.SaveToFile(FileName);
  FDocumentName := FileName;
  Caption := ExtractFileName(FileName);
  Modified := False;
end;

procedure TfrmChild.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Response: Integer;
begin
  if Modified then
  begin
    Response := MessageDlg('Voulez-vous enregistrer les modifications apportées à "' +
                           ExtractFileName(DocumentName) + '" ?',
                           mtConfirmation, [mbYes, mbNo, mbCancel], 0);
    case Response of
      mrYes:
        begin
          // Enregistrer le document
          if DocumentName = '' then
          begin
            // Demander un nom de fichier
            with TSaveDialog.Create(Self) do
            try
              Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
              if Execute then
                SaveToFile(FileName)
              else
                CanClose := False;
            finally
              Free;
            end;
          end
          else
            SaveToFile(DocumentName);
        end;
      mrCancel:
        CanClose := False;
    end;
  end;
end;
```

## Exemple complet : Éditeur de texte MDI

Voici un exemple plus complet d'une application d'éditeur de texte MDI avec Delphi.

### Structure du formulaire principal (MainForm.pas)

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.ImgList, System.ImageList, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuSaveAs: TMenuItem;
    mnuSep1: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    mnuDelete: TMenuItem;
    mnuSep2: TMenuItem;
    mnuSelectAll: TMenuItem;
    mnuWindow: TMenuItem;
    mnuCascade: TMenuItem;
    mnuTileHorizontal: TMenuItem;
    mnuTileVertical: TMenuItem;
    mnuArrangeIcons: TMenuItem;
    mnuSep3: TMenuItem;
    mnuCloseAll: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ImageList1: TImageList;
    tbNew: TToolButton;
    tbOpen: TToolButton;
    tbSave: TToolButton;
    ToolButton1: TToolButton;
    tbCut: TToolButton;
    tbCopy: TToolButton;
    tbPaste: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure mnuNewClick(Sender: TObject);
    procedure mnuOpenClick(Sender: TObject);
    procedure mnuSaveClick(Sender: TObject);
    procedure mnuSaveAsClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
    procedure mnuDeleteClick(Sender: TObject);
    procedure mnuSelectAllClick(Sender: TObject);
    procedure mnuCascadeClick(Sender: TObject);
    procedure mnuTileHorizontalClick(Sender: TObject);
    procedure mnuTileVerticalClick(Sender: TObject);
    procedure mnuArrangeIconsClick(Sender: TObject);
    procedure mnuCloseAllClick(Sender: TObject);
    procedure mnuWindowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FChildCount: Integer;
    procedure MDIChildClick(Sender: TObject);
    function IsFileOpen(const FileName: string): Boolean;
    function ActiveEditor: TMemo;
    procedure EnableEditMenuItems;
    procedure UpdateStatusBar;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses ChildForm;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FChildCount := 0;
  Caption := 'Éditeur de texte MDI - Delphi';
  StatusBar1.Panels[0].Text := 'Prêt';
  StatusBar1.Panels[1].Text := 'Documents: 0';

  // Par défaut, créer un nouveau document
  mnuNewClick(nil);
end;

procedure TfrmMain.mnuNewClick(Sender: TObject);
var
  ChildForm: TfrmChild;
begin
  ChildForm := TfrmChild.Create(Application);
  Inc(FChildCount);

  ChildForm.Caption := 'Sans titre ' + IntToStr(FChildCount);
  ChildForm.DocumentName := '';

  UpdateStatusBar;
end;

// ... autres méthodes implémentées ...

function TfrmMain.ActiveEditor: TMemo;
begin
  if ActiveMDIChild <> nil then
    Result := TfrmChild(ActiveMDIChild).Memo1
  else
    Result := nil;
end;

procedure TfrmMain.EnableEditMenuItems;
var
  HasActiveChild: Boolean;
  HasSelection: Boolean;
begin
  HasActiveChild := ActiveMDIChild <> nil;

  // Activer/désactiver les commandes d'édition
  mnuSave.Enabled := HasActiveChild;
  mnuSaveAs.Enabled := HasActiveChild;

  if HasActiveChild and (ActiveEditor <> nil) then
  begin
    HasSelection := ActiveEditor.SelLength > 0;

    mnuCut.Enabled := HasSelection;
    mnuCopy.Enabled := HasSelection;
    mnuDelete.Enabled := HasSelection;
    mnuPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
    mnuSelectAll.Enabled := True;

    tbCut.Enabled := HasSelection;
    tbCopy.Enabled := HasSelection;
    tbPaste.Enabled := Clipboard.HasFormat(CF_TEXT);
  end
  else
  begin
    mnuCut.Enabled := False;
    mnuCopy.Enabled := False;
    mnuPaste.Enabled := False;
    mnuDelete.Enabled := False;
    mnuSelectAll.Enabled := False;

    tbCut.Enabled := False;
    tbCopy.Enabled := False;
    tbPaste.Enabled := False;
  end;
end;

procedure TfrmMain.UpdateStatusBar;
begin
  StatusBar1.Panels[1].Text := 'Documents: ' + IntToStr(FChildCount);

  if ActiveMDIChild <> nil then
    StatusBar1.Panels[0].Text := 'Document actif: ' +
                                TfrmChild(ActiveMDIChild).DocumentName
  else
    StatusBar1.Panels[0].Text := 'Prêt';

  EnableEditMenuItems;
end;

// ... autres méthodes implémentées ...
```

### Structure du formulaire enfant (ChildForm.pas)

```pascal
unit ChildForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmChild = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Memo1Change(Sender: TObject);
  private
    FDocumentName: string;
    FModified: Boolean;
    procedure SetModified(const Value: Boolean);
  public
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    property DocumentName: string read FDocumentName write FDocumentName;
    property Modified: Boolean read FModified write SetModified;
  end;

implementation

{$R *.dfm}

uses MainForm;

procedure TfrmChild.FormCreate(Sender: TObject);
begin
  FModified := False;

  // Configurer la position et la taille initiale
  Width := (frmMain.ClientWidth * 2) div 3;
  Height := (frmMain.ClientHeight * 2) div 3;
  Left := (frmMain.ClientWidth - Width) div 2;
  Top := (frmMain.ClientHeight - Height) div 2;
end;

procedure TfrmChild.SetModified(const Value: Boolean);
begin
  FModified := Value;

  // Ajouter ou supprimer l'astérisque pour indiquer les modifications
  if FModified and (Caption[Length(Caption)] <> '*') then
    Caption := Caption + '*'
  else if not FModified and (Caption[Length(Caption)] = '*') then
    Caption := Copy(Caption, 1, Length(Caption) - 1);
end;

procedure TfrmChild.Memo1Change(Sender: TObject);
begin
  Modified := True;
end;

// ... autres méthodes implémentées ...
```

## Gestion avancée des formulaires MDI

### Synchronisation entre les formulaires enfants

Si vous voulez que toutes les fenêtres enfants partagent certaines informations ou états :

```pascal
// Dans le formulaire principal, ajouter une méthode pour diffuser les paramètres
procedure TfrmMain.UpdateAllChildFonts(const NewFont: TFont);
var
  i: Integer;
begin
  for i := 0 to MDIChildCount - 1 do
  begin
    TfrmChild(MDIChildren[i]).Memo1.Font.Assign(NewFont);
  end;
end;
```

### MDI avec différents types de formulaires enfants

Vous pouvez créer une application MDI avec différents types de fenêtres enfants :

```pascal
// Différents types de formulaires
TChildFormType = (cftText, cftImage, cftTable);

// Méthode pour créer un nouveau formulaire selon le type
procedure TfrmMain.CreateChild(FormType: TChildFormType);
begin
  case FormType of
    cftText:
      begin
        // Créer un formulaire éditeur de texte
      end;
    cftImage:
      begin
        // Créer un formulaire éditeur d'image
      end;
    cftTable:
      begin
        // Créer un formulaire éditeur de tableau
      end;
  end;
end;
```

## Limitations et alternatives au MDI

### Limitations du MDI

- Style d'interface un peu daté pour certains utilisateurs
- Gestion parfois complexe des fenêtres multiples
- Inadapté aux interfaces modernes sur tablettes ou mobiles

### Alternatives au MDI

1. **Interface à onglets** : utiliser un `TPageControl` pour afficher plusieurs documents

```pascal
// Créer un nouvel onglet
procedure TMainForm.CreateTabDocument(const Title: string);
var
  NewTab: TTabSheet;
  NewMemo: TMemo;
begin
  NewTab := TTabSheet.Create(PageControl1);
  NewTab.Caption := Title;
  NewTab.PageControl := PageControl1;

  NewMemo := TMemo.Create(NewTab);
  NewMemo.Parent := NewTab;
  NewMemo.Align := alClient;

  PageControl1.ActivePage := NewTab;
end;
```

2. **Formulaires flottants** : des formulaires indépendants qui restent au premier plan

3. **Dock Panels** : utiliser des panneaux ancrables/flottants pour une UI plus flexible

## Bonnes pratiques

1. **Gestion de mémoire** : Libérez toujours les formulaires enfants correctement (`Action := caFree`)

2. **Nommage des formulaires** : Utilisez des noms explicites, pas juste "Document 1, 2, 3"

3. **État des menus** : Activez/désactivez les options de menu en fonction du contexte

4. **Messages d'état** : Affichez des informations utiles dans la barre d'état

5. **Gestion des modifications** : Demandez toujours à l'utilisateur s'il veut sauvegarder avant de fermer

## Exercices pratiques

1. **Exercice simple** : Créez un éditeur de texte MDI basique avec les fonctions de base (nouveau, ouvrir, enregistrer)

2. **Exercice intermédiaire** : Ajoutez des fonctionnalités d'édition avancées et la possibilité de définir le style de texte

3. **Exercice avancé** : Créez une application MDI avec différents types de documents (texte, images, tableaux)

---

L'interface MDI est un concept puissant pour créer des applications permettant de travailler sur plusieurs documents simultanément. Bien qu'elle soit moins utilisée dans les interfaces modernes, elle reste pertinente pour de nombreuses applications professionnelles, notamment les suites bureautiques et les environnements de développement.

⏭️ [Boîtes de dialogue standard et personnalisées](/06-applications-multi-fenetres-et-navigation/04-boites-de-dialogue-standard-et-personnalisees.md)
