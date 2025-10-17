🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.3 Formulaires MDI (Multiple Document Interface)

## Introduction

L'interface MDI (Multiple Document Interface) est un style d'application où plusieurs fenêtres enfants peuvent être ouvertes simultanément à l'intérieur d'une fenêtre parent principale. C'est comme avoir plusieurs documents ouverts dans une seule application conteneur.

Vous avez probablement déjà utilisé des applications MDI sans le savoir. Des exemples classiques incluent :

- Les anciennes versions de Microsoft Word (avec plusieurs documents dans une même fenêtre)
- Adobe Photoshop (avec plusieurs images ouvertes)
- Des applications de gestion avec plusieurs fiches de saisie ouvertes en même temps

Dans cette section, nous allons apprendre à créer et gérer des applications MDI avec Delphi.

## Comprendre le concept MDI

### MDI vs SDI

Il existe deux principaux types d'interfaces pour les applications Windows :

**SDI (Single Document Interface)**
- Chaque document s'ouvre dans sa propre fenêtre indépendante
- Les fenêtres peuvent être déplacées librement sur l'écran
- Exemple moderne : Google Chrome, la plupart des applications actuelles

**MDI (Multiple Document Interface)**
- Une fenêtre parent contient toutes les fenêtres enfants
- Les fenêtres enfants restent confinées dans la fenêtre parent
- Les enfants peuvent être minimisés, maximisés à l'intérieur du parent
- Exemple classique : anciennes versions de Microsoft Office

### Structure d'une application MDI

Une application MDI se compose de :

1. **Un formulaire parent (MDI Parent)** : La fenêtre principale qui contient tout
2. **Des formulaires enfants (MDI Child)** : Les fenêtres qui s'ouvrent à l'intérieur du parent
3. **Un menu principal** : Généralement dans le formulaire parent
4. **Optionnellement une barre d'outils** : Pour les actions communes

## Créer une application MDI

### Étape 1 : Configurer le formulaire parent

Créez un nouveau projet VCL et configurez le formulaire principal comme parent MDI :

1. Sélectionnez le formulaire principal (Form1)
2. Dans l'Inspecteur d'objets, trouvez la propriété `FormStyle`
3. Définissez `FormStyle` à `fsMDIForm`

```pascal
// Le code généré automatiquement
type
  TForm1 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
```

Dans le fichier DFM, vous verrez :
```
object Form1: TForm1
  FormStyle = fsMDIForm
  Caption = 'Application MDI - Fenêtre Principale'
end
```

### Étape 2 : Créer un formulaire enfant

1. Ajoutez un nouveau formulaire au projet (**Fichier** → **Nouveau** → **Fiche VCL**)
2. Sélectionnez ce nouveau formulaire (Form2)
3. Dans l'Inspecteur d'objets, définissez `FormStyle` à `fsMDIChild`

```pascal
type
  TForm2 = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;
```

Dans le fichier DFM :
```
object Form2: TForm2
  FormStyle = fsMDIChild
  Caption = 'Document 1'
end
```

### Étape 3 : Ouvrir des formulaires enfants

Pour ouvrir un formulaire enfant depuis le formulaire parent :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
var
  FormEnfant: TForm2;
begin
  FormEnfant := TForm2.Create(Self);
  FormEnfant.Show;  // Utiliser Show, pas ShowModal
end;
```

**Important :**
- N'utilisez jamais `ShowModal` avec des formulaires MDI enfants
- Utilisez toujours `Show`
- Ne libérez pas manuellement le formulaire enfant, il sera géré automatiquement

## Gérer plusieurs instances de formulaires enfants

### Créer plusieurs fenêtres enfants du même type

```pascal
procedure TForm1.NouveauDocumentClick(Sender: TObject);
var
  FormEnfant: TForm2;
  Compteur: Integer;
  static NumeroDocument: Integer = 0;
begin
  Inc(NumeroDocument);

  FormEnfant := TForm2.Create(Self);
  FormEnfant.Caption := 'Document ' + IntToStr(NumeroDocument);
  FormEnfant.Show;
end;
```

### Parcourir les formulaires enfants ouverts

```pascal
procedure TForm1.ListerDocumentsClick(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Clear;
  Memo1.Lines.Add('Documents ouverts :');

  for i := 0 to MDIChildCount - 1 do
  begin
    Memo1.Lines.Add('  - ' + MDIChildren[i].Caption);
  end;
end;
```

**Propriétés utiles :**
- `MDIChildCount` : Nombre de formulaires enfants ouverts
- `MDIChildren[i]` : Accès au i-ème formulaire enfant
- `ActiveMDIChild` : Le formulaire enfant actuellement actif

## Menu dans les applications MDI

### Menu principal

Le menu principal est généralement placé dans le formulaire parent :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer le menu principal
  with MainMenu1 do
  begin
    // Menu Fichier
    with Items.Add do
    begin
      Caption := '&Fichier';

      // Nouveau document
      with Add do
      begin
        Caption := '&Nouveau';
        OnClick := NouveauDocumentClick;
      end;

      // Fermer le document actif
      with Add do
      begin
        Caption := '&Fermer';
        OnClick := FermerDocumentClick;
      end;

      // Séparateur
      Add.Caption := '-';

      // Quitter
      with Add do
      begin
        Caption := '&Quitter';
        OnClick := QuitterClick;
      end;
    end;

    // Menu Fenêtre
    with Items.Add do
    begin
      Caption := 'Fe&nêtre';

      // Cascade
      with Add do
      begin
        Caption := '&Cascade';
        OnClick := CascadeClick;
      end;

      // Mosaïque horizontale
      with Add do
      begin
        Caption := 'Mosaïque &horizontale';
        OnClick := TileHorizontalClick;
      end;

      // Mosaïque verticale
      with Add do
      begin
        Caption := 'Mosaïque &verticale';
        OnClick := TileVerticalClick;
      end;
    end;
  end;
end;
```

### Fusion de menus (Menu Merging)

Les formulaires enfants peuvent avoir leur propre menu qui se fusionne avec celui du parent.

**Form2 (enfant) :**
```pascal
// Créer un menu spécifique au formulaire enfant
procedure TForm2.FormCreate(Sender: TObject);
begin
  with MainMenu1 do
  begin
    with Items.Add do
    begin
      Caption := '&Édition';

      with Add do
      begin
        Caption := '&Copier';
        OnClick := CopierClick;
      end;

      with Add do
      begin
        Caption := 'Co&ller';
        OnClick := CollerClick;
      end;
    end;
  end;
end;
```

Le menu "Édition" apparaîtra dans la barre de menu principale quand ce formulaire enfant est actif.

## Organisation des fenêtres MDI

### Disposition en cascade

```pascal
procedure TForm1.CascadeClick(Sender: TObject);
begin
  Cascade;
end;
```

Les fenêtres se chevauchent légèrement, comme des cartes disposées en éventail.

### Disposition en mosaïque horizontale

```pascal
procedure TForm1.TileHorizontalClick(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;
```

Les fenêtres sont disposées horizontalement sans se chevaucher.

### Disposition en mosaïque verticale

```pascal
procedure TForm1.TileVerticalClick(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;
```

Les fenêtres sont disposées verticalement sans se chevaucher.

### Réorganiser toutes les icônes

```pascal
procedure TForm1.RéorganiserIconesClick(Sender: TObject);
begin
  ArrangeIcons;
end;
```

Réorganise proprement les fenêtres minimisées au bas de la fenêtre parent.

## Gestion des formulaires enfants actifs

### Détecter le formulaire actif

```pascal
procedure TForm1.AfficherFormActifClick(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ShowMessage('Formulaire actif : ' + ActiveMDIChild.Caption)
  else
    ShowMessage('Aucun formulaire enfant ouvert');
end;
```

### Passer d'un formulaire à l'autre

```pascal
procedure TForm1.FormulaireSuivantClick(Sender: TObject);
begin
  if MDIChildCount > 0 then
    Next;
end;

procedure TForm1.FormulairePrécédentClick(Sender: TObject);
begin
  if MDIChildCount > 0 then
    Previous;
end;
```

### Fermer le formulaire actif

```pascal
procedure TForm1.FermerDocumentClick(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ActiveMDIChild.Close;
end;
```

### Fermer tous les formulaires

```pascal
procedure TForm1.FermerToutClick(Sender: TObject);
var
  i: Integer;
begin
  // Parcourir à l'envers pour éviter les problèmes d'index
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].Close;
end;
```

## Liste des fenêtres dans le menu

### Ajouter automatiquement la liste des fenêtres

Delphi peut automatiquement ajouter une liste des fenêtres ouvertes dans un menu :

```pascal
// Dans le formulaire parent
procedure TForm1.FormCreate(Sender: TObject);
var
  MenuFenetre: TMenuItem;
begin
  // Créer le menu Fenêtre
  MenuFenetre := TMenuItem.Create(MainMenu1);
  MenuFenetre.Caption := 'Fe&nêtre';
  MainMenu1.Items.Add(MenuFenetre);

  // Indiquer que ce menu contiendra la liste des fenêtres
  WindowMenu := MenuFenetre;
end;
```

Delphi ajoutera automatiquement :
- Une coche devant la fenêtre active
- Un numéro pour chaque fenêtre (1, 2, 3...)
- La possibilité de cliquer pour activer une fenêtre

## Exemple complet d'application MDI

### Formulaire principal (MDI Parent)

```pascal
unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuFichier: TMenuItem;
    MenuNouveauDoc: TMenuItem;
    MenuFermer: TMenuItem;
    N1: TMenuItem;
    MenuQuitter: TMenuItem;
    MenuFenetre: TMenuItem;
    MenuCascade: TMenuItem;
    MenuTileH: TMenuItem;
    MenuTileV: TMenuItem;
    N2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure MenuNouveauDocClick(Sender: TObject);
    procedure MenuFermerClick(Sender: TObject);
    procedure MenuQuitterClick(Sender: TObject);
    procedure MenuCascadeClick(Sender: TObject);
    procedure MenuTileHClick(Sender: TObject);
    procedure MenuTileVClick(Sender: TObject);
  private
    FCompteurDocuments: Integer;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses UnitDocument;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FCompteurDocuments := 0;
  Caption := 'Application MDI - Exemple';
  WindowMenu := MenuFenetre;  // Active la liste automatique des fenêtres
  Position := poScreenCenter;
  WindowState := wsMaximized;
end;

procedure TFormMain.MenuNouveauDocClick(Sender: TObject);
var
  FormDoc: TFormDocument;
begin
  Inc(FCompteurDocuments);

  FormDoc := TFormDocument.Create(Self);
  FormDoc.Caption := 'Document ' + IntToStr(FCompteurDocuments);
  FormDoc.Show;
end;

procedure TFormMain.MenuFermerClick(Sender: TObject);
begin
  if Assigned(ActiveMDIChild) then
    ActiveMDIChild.Close;
end;

procedure TFormMain.MenuQuitterClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.MenuCascadeClick(Sender: TObject);
begin
  Cascade;
end;

procedure TFormMain.MenuTileHClick(Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;

procedure TFormMain.MenuTileVClick(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;

end.
```

### Formulaire enfant (MDI Child)

```pascal
unit UnitDocument;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormDocument = class(TForm)
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FModifie: Boolean;
    procedure MemoChange(Sender: TObject);
  public
    property Modifie: Boolean read FModifie;
  end;

var
  FormDocument: TFormDocument;

implementation

{$R *.dfm}

procedure TFormDocument.FormCreate(Sender: TObject);
begin
  FModifie := False;
  Memo1.OnChange := MemoChange;
  Memo1.ScrollBars := ssBoth;
  Memo1.Align := alClient;
end;

procedure TFormDocument.MemoChange(Sender: TObject);
begin
  FModifie := True;
  if Pos('*', Caption) = 0 then
    Caption := Caption + ' *';
end;

procedure TFormDocument.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FModifie then
  begin
    case MessageDlg('Voulez-vous enregistrer les modifications ?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          // Code pour enregistrer
          ShowMessage('Document enregistré');
          Action := caFree;
        end;
      mrNo:
        Action := caFree;
      mrCancel:
        Action := caNone;  // Annule la fermeture
    end;
  end
  else
    Action := caFree;
end;

end.
```

## Propriétés importantes des formulaires MDI

### Formulaire parent (MDI Form)

| Propriété | Description |
|-----------|-------------|
| `FormStyle` | Doit être `fsMDIForm` |
| `MDIChildCount` | Nombre de formulaires enfants ouverts |
| `MDIChildren[Index]` | Accès aux formulaires enfants |
| `ActiveMDIChild` | Formulaire enfant actuellement actif |
| `WindowMenu` | MenuItem qui affichera la liste des fenêtres |
| `TileMode` | Mode de mosaïque (`tbHorizontal` ou `tbVertical`) |

### Formulaire enfant (MDI Child)

| Propriété | Description |
|-----------|-------------|
| `FormStyle` | Doit être `fsMDIChild` |
| `WindowState` | État de la fenêtre (`wsNormal`, `wsMinimized`, `wsMaximized`) |
| `Icon` | Icône affichée dans la barre de titre |

## Événements importants

### Dans le formulaire parent

```pascal
procedure TFormMain.FormActivate(Sender: TObject);
begin
  // Appelé quand le formulaire parent devient actif
end;

procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  // Appelé quand le formulaire parent perd le focus
end;
```

### Dans les formulaires enfants

```pascal
procedure TFormDocument.FormActivate(Sender: TObject);
begin
  // Appelé quand ce document devient actif
  StatusBar1.SimpleText := 'Document actif : ' + Caption;
end;

procedure TFormDocument.FormDeactivate(Sender: TObject);
begin
  // Appelé quand ce document perd le focus
end;

procedure TFormDocument.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Définir Action pour contrôler la fermeture
  // caHide : Cache le formulaire
  // caFree : Libère le formulaire (recommandé pour MDI)
  // caMinimize : Minimise le formulaire
  // caNone : Annule la fermeture
  Action := caFree;
end;
```

## Avantages et inconvénients du MDI

### Avantages

**Organisation**
- Toutes les fenêtres sont regroupées dans une seule fenêtre parent
- Facilite la gestion de plusieurs documents simultanés
- Interface cohérente et structurée

**Gestion de l'espace**
- Les fenêtres enfants ne peuvent pas sortir de la zone parent
- Utile sur de petits écrans
- Possibilité de maximiser un enfant dans la zone parent

**Fonctionnalités intégrées**
- Disposition automatique (cascade, mosaïque)
- Liste des fenêtres dans le menu
- Navigation facile entre les documents

### Inconvénients

**Style démodé**
- L'interface MDI est considérée comme ancienne
- Les applications modernes préfèrent les onglets ou les fenêtres SDI
- Moins intuitive pour les nouveaux utilisateurs

**Limitations**
- Les fenêtres enfants sont confinées au parent
- Difficile d'utiliser plusieurs écrans efficacement
- Peut être confus avec beaucoup de fenêtres ouvertes

**Complexité**
- Plus difficile à programmer qu'une interface à onglets
- Gestion de la fusion de menus parfois compliquée
- Problèmes potentiels avec le focus et l'activation

## Alternatives au MDI

### Interface à onglets (Tabbed Interface)

Plus moderne et généralement préférée :

```pascal
// Utilisation d'un TPageControl
procedure TForm1.NouveauDocumentClick(Sender: TObject);
var
  TabSheet: TTabSheet;
  Memo: TMemo;
begin
  TabSheet := TTabSheet.Create(PageControl1);
  TabSheet.PageControl := PageControl1;
  TabSheet.Caption := 'Document ' + IntToStr(PageControl1.PageCount);

  Memo := TMemo.Create(TabSheet);
  Memo.Parent := TabSheet;
  Memo.Align := alClient;

  PageControl1.ActivePage := TabSheet;
end;
```

### Interface SDI (Single Document Interface)

Chaque document dans sa propre fenêtre :

```pascal
procedure TForm1.NouveauDocumentClick(Sender: TObject);
var
  FormDoc: TFormDocument;
begin
  FormDoc := TFormDocument.Create(Application);
  FormDoc.Show;
end;
```

## Quand utiliser MDI ?

**Utilisez MDI si :**
- Vous développez une application de type "ancienne école" pour des utilisateurs habitués
- Vous avez besoin de gérer beaucoup de fenêtres similaires
- L'application doit fonctionner sur de petits écrans
- Vous devez maintenir une application MDI existante

**Évitez MDI si :**
- Vous créez une nouvelle application moderne
- Vous pouvez utiliser des onglets à la place
- L'application sera utilisée sur plusieurs écrans
- Vous visez une interface utilisateur moderne et intuitive

## Conseils de conception

### 1. Fournir des raccourcis clavier

```pascal
// Dans le formulaire parent
procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    case Key of
      Ord('N'): MenuNouveauDocClick(Sender);  // Ctrl+N : Nouveau
      Ord('W'): MenuFermerClick(Sender);      // Ctrl+W : Fermer
      VK_TAB: Next;                            // Ctrl+Tab : Document suivant
    end;
  end;
end;
```

### 2. Indiquer le document actif

```pascal
procedure TFormMain.UpdateStatusBar;
begin
  if Assigned(ActiveMDIChild) then
    StatusBar1.SimpleText := 'Document : ' + ActiveMDIChild.Caption
  else
    StatusBar1.SimpleText := 'Aucun document ouvert';
end;
```

### 3. Limiter le nombre de fenêtres

```pascal
procedure TFormMain.MenuNouveauDocClick(Sender: TObject);
begin
  if MDIChildCount >= 10 then
  begin
    ShowMessage('Vous avez atteint le nombre maximum de documents ouverts (10)');
    Exit;
  end;

  // Créer le nouveau document
end;
```

### 4. Sauvegarder l'état de l'application

```pascal
procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  DocsModifies: Boolean;
begin
  DocsModifies := False;

  // Vérifier si des documents ont été modifiés
  for i := 0 to MDIChildCount - 1 do
  begin
    if (MDIChildren[i] as TFormDocument).Modifie then
    begin
      DocsModifies := True;
      Break;
    end;
  end;

  if DocsModifies then
  begin
    if MessageDlg('Certains documents ont été modifiés. Quitter quand même ?',
      mtWarning, [mbYes, mbNo], 0) = mrNo then
      Action := caNone;
  end;
end;
```

## Résumé

Les formulaires MDI permettent de créer des applications avec plusieurs documents ouverts dans une seule fenêtre parent. Les points clés à retenir :

- **FormStyle** : `fsMDIForm` pour le parent, `fsMDIChild` pour les enfants
- **Gestion** : Utilisez `MDIChildCount`, `MDIChildren[]` et `ActiveMDIChild`
- **Organisation** : Méthodes `Cascade`, `Tile`, `ArrangeIcons`
- **Menus** : Utilisez `WindowMenu` pour la liste automatique des fenêtres
- **Moderne** : Considérez les alternatives (onglets, SDI) pour les nouvelles applications
- **Fermeture** : Définissez `Action := caFree` dans `OnClose` des enfants

Bien que le style MDI soit considéré comme un peu dépassé, il reste utile pour certains types d'applications, particulièrement dans les environnements professionnels où les utilisateurs sont habitués à cette interface.

⏭️ [Boîtes de dialogue standard et personnalisées](/06-applications-multi-fenetres-et-navigation/04-boites-de-dialogue-standard-et-personnalisees.md)
