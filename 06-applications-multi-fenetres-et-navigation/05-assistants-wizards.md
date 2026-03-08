🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.5 Assistants (Wizards)

## Introduction

Un assistant (wizard en anglais) est une interface utilisateur qui guide l'utilisateur à travers un processus complexe en le décomposant en plusieurs étapes simples. Chaque étape présente un ensemble limité d'options ou de saisies, rendant le processus global plus facile à comprendre et à compléter.

Vous avez probablement déjà utilisé des assistants :
- L'installation de logiciels (étapes de configuration)
- La création d'un nouveau compte (informations personnelles → adresse → confirmation)
- Les paramètres initiaux d'une application
- La configuration d'une imprimante ou d'un périphérique

Dans cette section, nous allons apprendre à créer des assistants professionnels avec Delphi.

## Caractéristiques d'un assistant

### Structure typique

Un assistant comprend généralement :

1. **Plusieurs pages (étapes)** : Chaque page représente une étape du processus
2. **Boutons de navigation** : Précédent, Suivant, Terminer, Annuler
3. **Indicateur de progression** : Montre à l'utilisateur où il en est dans le processus
4. **Validation** : Vérifie que les données de chaque étape sont correctes avant de continuer
5. **Page de bienvenue** : Présente l'objectif de l'assistant
6. **Page de confirmation** : Récapitule les choix avant de terminer

### Avantages des assistants

**Pour l'utilisateur :**
- Processus complexe divisé en étapes simples
- Pas de surcharge d'information
- Guidance claire
- Sentiment de progression

**Pour le développeur :**
- Code organisé et modulaire
- Validation étape par étape
- Facile à maintenir et à modifier
- Réutilisable

## Conception d'un assistant

### Approche 1 : Avec TPageControl

La méthode la plus simple utilise un composant `TPageControl` avec plusieurs `TTabSheet`.

**Avantages :**
- Simple à mettre en place
- Interface familière
- Facile à concevoir visuellement

**Inconvénients :**
- Les onglets sont visibles (il faut les masquer)
- Moins flexible pour des scénarios complexes

### Approche 2 : Avec des panels

Utiliser plusieurs `TPanel` superposés et afficher uniquement celui de l'étape courante.

**Avantages :**
- Plus de contrôle sur l'apparence
- Pas de composants visibles inutiles
- Plus flexible

**Inconvénients :**
- Plus de code à écrire
- Positionnement manuel

### Approche 3 : Avec des formulaires séparés

Créer un formulaire distinct pour chaque étape.

**Avantages :**
- Maximum de flexibilité
- Réutilisable
- Chaque étape est indépendante

**Inconvénients :**
- Plus complexe à gérer
- Plus de fichiers à maintenir

## Méthode 1 : Assistant avec TPageControl

### Étape 1 : Créer le formulaire de base

```pascal
unit UnitAssistant;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormAssistant = class(TForm)
    PageControl1: TPageControl;
    TabBienvenue: TTabSheet;
    TabEtape1: TTabSheet;
    TabEtape2: TTabSheet;
    TabFin: TTabSheet;
    PanelBoutons: TPanel;
    ButtonPrecedent: TButton;
    ButtonSuivant: TButton;
    ButtonAnnuler: TButton;
    ButtonTerminer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSuivantClick(Sender: TObject);
    procedure ButtonPrecedentClick(Sender: TObject);
    procedure ButtonTerminerClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    procedure MettreAJourBoutons;
    function ValiderPageCourante: Boolean;
  public
    { Public declarations }
  end;

var
  FormAssistant: TFormAssistant;

implementation

{$R *.dfm}

procedure TFormAssistant.FormCreate(Sender: TObject);  
begin  
  // Masquer les onglets
  PageControl1.Style := tsButtons;
  PageControl1.TabHeight := 1;
  PageControl1.TabWidth := 1;

  // Commencer à la première page
  PageControl1.ActivePageIndex := 0;

  // Configurer les boutons
  ButtonTerminer.Visible := False;
  MettreAJourBoutons;

  // Configuration du formulaire
  Position := poScreenCenter;
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];
end;

procedure TFormAssistant.MettreAJourBoutons;  
var  
  PageIndex: Integer;
begin
  PageIndex := PageControl1.ActivePageIndex;

  // Bouton Précédent
  ButtonPrecedent.Enabled := PageIndex > 0;

  // Bouton Suivant
  ButtonSuivant.Visible := PageIndex < PageControl1.PageCount - 1;

  // Bouton Terminer
  ButtonTerminer.Visible := PageIndex = PageControl1.PageCount - 1;

  // Texte du bouton Suivant
  ButtonSuivant.Caption := '&Suivant >';
end;

procedure TFormAssistant.ButtonSuivantClick(Sender: TObject);  
begin  
  // Valider la page courante
  if not ValiderPageCourante then
    Exit;

  // Passer à la page suivante
  if PageControl1.ActivePageIndex < PageControl1.PageCount - 1 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
    MettreAJourBoutons;
  end;
end;

procedure TFormAssistant.ButtonPrecedentClick(Sender: TObject);  
begin  
  // Revenir à la page précédente
  if PageControl1.ActivePageIndex > 0 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
    MettreAJourBoutons;
  end;
end;

procedure TFormAssistant.ButtonTerminerClick(Sender: TObject);  
begin  
  // Valider la dernière page
  if not ValiderPageCourante then
    Exit;

  // Traiter les données de l'assistant
  ModalResult := mrOk;
end;

procedure TFormAssistant.ButtonAnnulerClick(Sender: TObject);  
begin  
  if MessageDlg('Voulez-vous vraiment annuler l''assistant ?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ModalResult := mrCancel;
end;

procedure TFormAssistant.PageControl1Change(Sender: TObject);  
begin  
  MettreAJourBoutons;
end;

function TFormAssistant.ValiderPageCourante: Boolean;  
begin  
  Result := True;

  case PageControl1.ActivePageIndex of
    0: Result := True;  // Page de bienvenue, pas de validation
    1: Result := True;  // Validation de l'étape 1
    2: Result := True;  // Validation de l'étape 2
    3: Result := True;  // Page finale
  end;
end;

end.
```

### Étape 2 : Concevoir les pages

**Page de bienvenue (TabBienvenue) :**
```pascal
// Dans le concepteur de formulaire, ajoutez :
// - Un TLabel pour le titre (grande police, gras)
// - Un TLabel pour la description
// - Optionnellement une TImage pour une icône

procedure TFormAssistant.ConfigurerPageBienvenue;  
begin  
  with TabBienvenue do
  begin
    // Ajouté dans le concepteur visuel
    LabelTitre.Caption := 'Bienvenue dans l''assistant de configuration';
    LabelTitre.Font.Size := 12;
    LabelTitre.Font.Style := [fsBold];

    MemoDescription.Lines.Text :=
      'Cet assistant vous guidera à travers la configuration ' +
      'de votre application.'#13#10#13#10 +
      'Cliquez sur Suivant pour continuer.';
  end;
end;
```

**Pages d'étapes (TabEtape1, TabEtape2) :**
```pascal
// TabEtape1 : Informations personnelles
// Ajoutez des TEdit, TComboBox, etc.

// TabEtape2 : Préférences
// Ajoutez des TCheckBox, TRadioButton, etc.
```

**Page de fin (TabFin) :**
```pascal
procedure TFormAssistant.ConfigurerPageFin;  
begin  
  with TabFin do
  begin
    LabelTitre.Caption := 'Configuration terminée';
    LabelTitre.Font.Size := 12;
    LabelTitre.Font.Style := [fsBold];

    MemoRecapitulatif.Lines.Text :=
      'L''assistant a terminé la configuration.'#13#10#13#10 +
      'Cliquez sur Terminer pour appliquer les changements.';
  end;
end;
```

## Méthode 2 : Assistant avec des Panels

### Structure de base

```pascal
type
  TFormAssistantPanel = class(TForm)
    PanelContainer: TPanel;
    PanelBoutons: TPanel;
    ButtonPrecedent: TButton;
    ButtonSuivant: TButton;
    ButtonAnnuler: TButton;
    ButtonTerminer: TButton;
  private
    FPages: TList<TPanel>;
    FPageIndex: Integer;
    procedure CreerPages;
    procedure AfficherPage(Index: Integer);
    procedure MettreAJourBoutons;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

constructor TFormAssistantPanel.Create(AOwner: TComponent);  
begin  
  inherited;
  FPages := TList<TPanel>.Create;
  CreerPages;
  AfficherPage(0);
end;

destructor TFormAssistantPanel.Destroy;  
begin  
  FPages.Free;
  inherited;
end;

procedure TFormAssistantPanel.CreerPages;  
var  
  Page: TPanel;
begin
  // Page 1 : Bienvenue
  Page := TPanel.Create(Self);
  Page.Parent := PanelContainer;
  Page.Align := alClient;
  Page.BevelOuter := bvNone;
  Page.Visible := False;
  FPages.Add(Page);
  // Ajouter des composants à cette page...

  // Page 2 : Étape 1
  Page := TPanel.Create(Self);
  Page.Parent := PanelContainer;
  Page.Align := alClient;
  Page.BevelOuter := bvNone;
  Page.Visible := False;
  FPages.Add(Page);
  // Ajouter des composants à cette page...

  // ... autres pages
end;

procedure TFormAssistantPanel.AfficherPage(Index: Integer);  
var  
  i: Integer;
begin
  if (Index < 0) or (Index >= FPages.Count) then
    Exit;

  // Masquer toutes les pages
  for i := 0 to FPages.Count - 1 do
    FPages[i].Visible := False;

  // Afficher la page demandée
  FPages[Index].Visible := True;
  FPageIndex := Index;

  MettreAJourBoutons;
end;

procedure TFormAssistantPanel.MettreAJourBoutons;  
begin  
  ButtonPrecedent.Enabled := FPageIndex > 0;
  ButtonSuivant.Visible := FPageIndex < FPages.Count - 1;
  ButtonTerminer.Visible := FPageIndex = FPages.Count - 1;
end;
```

## Validation des données

### Validation par page

```pascal
function TFormAssistant.ValiderPageCourante: Boolean;  
begin  
  Result := True;

  case PageControl1.ActivePageIndex of
    1: // Étape 1 : Informations personnelles
    begin
      if Trim(EditNom.Text) = '' then
      begin
        ShowMessage('Le nom est obligatoire');
        EditNom.SetFocus;
        Result := False;
        Exit;
      end;

      if Trim(EditEmail.Text) = '' then
      begin
        ShowMessage('L''email est obligatoire');
        EditEmail.SetFocus;
        Result := False;
        Exit;
      end;

      if not ValiderEmail(EditEmail.Text) then
      begin
        ShowMessage('L''adresse email est invalide');
        EditEmail.SetFocus;
        Result := False;
        Exit;
      end;
    end;

    2: // Étape 2 : Préférences
    begin
      if RadioGroup1.ItemIndex = -1 then
      begin
        ShowMessage('Veuillez sélectionner une option');
        RadioGroup1.SetFocus;
        Result := False;
        Exit;
      end;
    end;
  end;
end;

function TFormAssistant.ValiderEmail(const Email: string): Boolean;  
begin  
  Result := Pos('@', Email) > 0;
  // Pour une validation plus robuste, utilisez TRegEx
end;
```

### Validation avec feedback visuel

```pascal
procedure TFormAssistant.EditNomExit(Sender: TObject);  
begin  
  if Trim(EditNom.Text) = '' then
  begin
    EditNom.Color := clYellow;
    LabelErreurNom.Caption := 'Le nom est obligatoire';
    LabelErreurNom.Visible := True;
  end
  else
  begin
    EditNom.Color := clWindow;
    LabelErreurNom.Visible := False;
  end;
end;
```

## Indicateur de progression

### Avec une ProgressBar

```pascal
procedure TFormAssistant.MettreAJourProgression;  
var  
  Pourcentage: Integer;
begin
  Pourcentage := Round((PageControl1.ActivePageIndex /
    (PageControl1.PageCount - 1)) * 100);
  ProgressBar1.Position := Pourcentage;
  LabelProgression.Caption := Format('Étape %d sur %d',
    [PageControl1.ActivePageIndex + 1, PageControl1.PageCount]);
end;
```

### Avec des labels numérotés

```pascal
procedure TFormAssistant.FormCreate(Sender: TObject);  
begin  
  // Créer des labels pour chaque étape
  CreerIndicateursEtapes;
end;

procedure TFormAssistant.CreerIndicateursEtapes;  
var  
  i: Integer;
  Lbl: TLabel;
begin
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := PanelIndicateurs;
    Lbl.Left := i * 80;
    Lbl.Top := 10;
    Lbl.Width := 70;
    Lbl.Height := 25;
    Lbl.Caption := PageControl1.Pages[i].Caption;
    Lbl.Alignment := taCenter;
    Lbl.Tag := i;
  end;
end;

procedure TFormAssistant.MettreAJourIndicateurs;  
var  
  i: Integer;
  Lbl: TLabel;
begin
  for i := 0 to PanelIndicateurs.ControlCount - 1 do
  begin
    if PanelIndicateurs.Controls[i] is TLabel then
    begin
      Lbl := TLabel(PanelIndicateurs.Controls[i]);

      if Lbl.Tag < PageControl1.ActivePageIndex then
        Lbl.Font.Color := clGreen  // Étape complétée
      else if Lbl.Tag = PageControl1.ActivePageIndex then
        Lbl.Font.Style := [fsBold]  // Étape courante
      else
        Lbl.Font.Color := clGray;  // Étape future
    end;
  end;
end;
```

## Exemple complet : Assistant d'installation

```pascal
unit UnitAssistantInstallation;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.FileCtrl;

type
  TInstallationParams = record
    Repertoire: string;
    RaccourciBureau: Boolean;
    MenuDemarrer: Boolean;
    AssocierFichiers: Boolean;
    LancerApresInstallation: Boolean;
  end;

  TFormInstallation = class(TForm)
    PageControl1: TPageControl;
    TabBienvenue: TTabSheet;
    TabLicence: TTabSheet;
    TabRepertoire: TTabSheet;
    TabOptions: TTabSheet;
    TabInstallation: TTabSheet;
    TabFin: TTabSheet;
    PanelBoutons: TPanel;
    ButtonPrecedent: TButton;
    ButtonSuivant: TButton;
    ButtonAnnuler: TButton;
    ButtonTerminer: TButton;

    // Page Bienvenue
    LabelTitreBienvenue: TLabel;
    MemoBienvenue: TMemo;

    // Page Licence
    MemoLicence: TMemo;
    CheckBoxAccepterLicence: TCheckBox;

    // Page Répertoire
    LabelRepertoire: TLabel;
    EditRepertoire: TEdit;
    ButtonParcourir: TButton;
    LabelEspace: TLabel;

    // Page Options
    GroupBoxOptions: TGroupBox;
    CheckBoxRaccourciBureau: TCheckBox;
    CheckBoxMenuDemarrer: TCheckBox;
    CheckBoxAssocierFichiers: TCheckBox;

    // Page Installation
    ProgressBarInstallation: TProgressBar;
    LabelProgression: TLabel;
    MemoJournal: TMemo;

    // Page Fin
    LabelFin: TLabel;
    CheckBoxLancerApplication: TCheckBox;

    procedure FormCreate(Sender: TObject);
    procedure ButtonSuivantClick(Sender: TObject);
    procedure ButtonPrecedentClick(Sender: TObject);
    procedure ButtonTerminerClick(Sender: TObject);
    procedure ButtonParcourirClick(Sender: TObject);
    procedure CheckBoxAccepterLicenceClick(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
  private
    procedure MettreAJourBoutons;
    function ValiderPageCourante: Boolean;
    procedure LancerInstallation;
    procedure AjouterJournal(const Message: string);
    function GetRepertoireInstallation: string;
  public
    class function Executer(var Parametres: TInstallationParams): Boolean;
  end;

implementation

{$R *.dfm}

class function TFormInstallation.Executer(
  var Parametres: TInstallationParams): Boolean;
var
  Form: TFormInstallation;
begin
  Form := TFormInstallation.Create(nil);
  try
    Result := Form.ShowModal = mrOk;
    if Result then
    begin
      Parametres.Repertoire := Form.EditRepertoire.Text;
      Parametres.RaccourciBureau := Form.CheckBoxRaccourciBureau.Checked;
      Parametres.MenuDemarrer := Form.CheckBoxMenuDemarrer.Checked;
      Parametres.AssocierFichiers := Form.CheckBoxAssocierFichiers.Checked;
      Parametres.LancerApresInstallation := Form.CheckBoxLancerApplication.Checked;
    end;
  finally
    Form.Free;
  end;
end;

procedure TFormInstallation.FormCreate(Sender: TObject);  
begin  
  // Masquer les onglets
  PageControl1.Style := tsButtons;
  PageControl1.TabHeight := 1;
  PageControl1.TabWidth := 1;
  PageControl1.ActivePageIndex := 0;

  // Configuration du formulaire
  Caption := 'Assistant d''installation';
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  // Textes
  LabelTitreBienvenue.Caption := 'Bienvenue dans l''assistant d''installation';
  MemoBienvenue.Lines.Text :=
    'Cet assistant va installer l''application sur votre ordinateur.'#13#10 +
    'Cliquez sur Suivant pour continuer.';

  MemoLicence.Lines.LoadFromFile('licence.txt');

  // Répertoire par défaut
  EditRepertoire.Text := 'C:\Program Files\MonApplication';

  // Options par défaut
  CheckBoxRaccourciBureau.Checked := True;
  CheckBoxMenuDemarrer.Checked := True;

  MettreAJourBoutons;
end;

procedure TFormInstallation.MettreAJourBoutons;  
var  
  PageIndex: Integer;
begin
  PageIndex := PageControl1.ActivePageIndex;

  ButtonPrecedent.Enabled := (PageIndex > 0) and (PageIndex <> 4);
  ButtonSuivant.Visible := PageIndex < 4;
  ButtonTerminer.Visible := PageIndex = 5;
  ButtonAnnuler.Visible := PageIndex <> 4;

  // Cas spécial : page d'installation
  if PageIndex = 4 then
  begin
    ButtonPrecedent.Enabled := False;
    ButtonSuivant.Enabled := False;
    ButtonAnnuler.Enabled := False;
  end;
end;

function TFormInstallation.ValiderPageCourante: Boolean;  
begin  
  Result := True;

  case PageControl1.ActivePageIndex of
    1: // Licence
    begin
      if not CheckBoxAccepterLicence.Checked then
      begin
        ShowMessage('Vous devez accepter les termes de la licence pour continuer');
        Result := False;
      end;
    end;

    2: // Répertoire
    begin
      if Trim(EditRepertoire.Text) = '' then
      begin
        ShowMessage('Veuillez spécifier un répertoire d''installation');
        EditRepertoire.SetFocus;
        Result := False;
        Exit;
      end;

      // Vérifier l'espace disque disponible
      LabelEspace.Caption := Format('Espace disponible : %d Mo',
        [GetEspaceDisque(EditRepertoire.Text)]);
    end;

    3: // Options - avant l'installation
    begin
      // Lancer l'installation
      PageControl1.ActivePageIndex := 4;
      LancerInstallation;
      Result := False; // Ne pas passer automatiquement à la page suivante
    end;
  end;
end;

procedure TFormInstallation.ButtonSuivantClick(Sender: TObject);  
begin  
  if not ValiderPageCourante then
    Exit;

  if PageControl1.ActivePageIndex < PageControl1.PageCount - 1 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
    MettreAJourBoutons;
  end;
end;

procedure TFormInstallation.ButtonPrecedentClick(Sender: TObject);  
begin  
  if PageControl1.ActivePageIndex > 0 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
    MettreAJourBoutons;
  end;
end;

procedure TFormInstallation.ButtonParcourirClick(Sender: TObject);  
var  
  Dossier: string;
begin
  Dossier := EditRepertoire.Text;
  if SelectDirectory('Sélectionnez le répertoire d''installation', '', Dossier) then
    EditRepertoire.Text := Dossier;
end;

procedure TFormInstallation.CheckBoxAccepterLicenceClick(Sender: TObject);  
begin  
  ButtonSuivant.Enabled := CheckBoxAccepterLicence.Checked;
end;

procedure TFormInstallation.LancerInstallation;  
var  
  i: Integer;
begin
  ProgressBarInstallation.Position := 0;
  ProgressBarInstallation.Max := 100;

  AjouterJournal('Début de l''installation...');
  AjouterJournal('Répertoire : ' + EditRepertoire.Text);

  // Simulation de l'installation
  for i := 1 to 100 do
  begin
    Sleep(50); // Simulation
    ProgressBarInstallation.Position := i;
    LabelProgression.Caption := Format('Installation en cours... %d%%', [i]);
    Application.ProcessMessages;

    if i mod 20 = 0 then
      AjouterJournal(Format('Installation : %d%% terminé', [i]));
  end;

  AjouterJournal('Installation terminée avec succès !');

  // Passer à la page de fin
  PageControl1.ActivePageIndex := 5;
  MettreAJourBoutons;
end;

procedure TFormInstallation.AjouterJournal(const Message: string);  
begin  
  MemoJournal.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + Message);
end;

procedure TFormInstallation.ButtonTerminerClick(Sender: TObject);  
begin  
  ModalResult := mrOk;
end;

procedure TFormInstallation.PageControl1Change(Sender: TObject);  
begin  
  MettreAJourBoutons;
end;

function TFormInstallation.GetRepertoireInstallation: string;  
begin  
  Result := EditRepertoire.Text;
end;

end.
```

### Utilisation de l'assistant

```pascal
procedure TFormMain.InstallationClick(Sender: TObject);  
var  
  Params: TInstallationParams;
begin
  if TFormInstallation.Executer(Params) then
  begin
    ShowMessage('Installation terminée dans : ' + Params.Repertoire);

    if Params.LancerApresInstallation then
    begin
      // Lancer l'application
    end;
  end
  else
    ShowMessage('Installation annulée');
end;
```

## Fonctionnalités avancées

### Navigation conditionnelle

Sauter certaines pages selon les choix de l'utilisateur :

```pascal
procedure TFormAssistant.ButtonSuivantClick(Sender: TObject);  
var  
  ProchainePage: Integer;
begin
  if not ValiderPageCourante then
    Exit;

  ProchainePage := PageControl1.ActivePageIndex + 1;

  // Si l'utilisateur a choisi "Installation personnalisée"
  if RadioButtonPersonnalisee.Checked then
    ProchainePage := ProchainePage + 0  // Page des options
  else
    ProchainePage := ProchainePage + 1; // Sauter la page des options

  if ProchainePage < PageControl1.PageCount then
  begin
    PageControl1.ActivePageIndex := ProchainePage;
    MettreAJourBoutons;
  end;
end;
```

### Sauvegarde et reprise

Permettre à l'utilisateur de sauvegarder sa progression :

```pascal
procedure TFormAssistant.SauvegarderProgression;  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    IniFile.WriteInteger('Assistant', 'PageIndex', PageControl1.ActivePageIndex);
    IniFile.WriteString('Etape1', 'Nom', EditNom.Text);
    IniFile.WriteString('Etape1', 'Email', EditEmail.Text);
    // ... autres valeurs
  finally
    IniFile.Free;
  end;
end;

procedure TFormAssistant.ChargerProgression;  
var  
  IniFile: TIniFile;
  PageIndex: Integer;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    PageIndex := IniFile.ReadInteger('Assistant', 'PageIndex', 0);
    PageControl1.ActivePageIndex := PageIndex;

    EditNom.Text := IniFile.ReadString('Etape1', 'Nom', '');
    EditEmail.Text := IniFile.ReadString('Etape1', 'Email', '');
    // ... autres valeurs
  finally
    IniFile.Free;
  end;
end;
```

### Aide contextuelle

Afficher une aide différente pour chaque page :

```pascal
procedure TFormAssistant.ButtonAideClick(Sender: TObject);  
var  
  TexteAide: string;
begin
  case PageControl1.ActivePageIndex of
    0: TexteAide := 'Cette page vous souhaite la bienvenue...';
    1: TexteAide := 'Veuillez saisir vos informations personnelles...';
    2: TexteAide := 'Choisissez vos préférences...';
    // ... autres pages
  end;

  ShowMessage(TexteAide);
  // ou afficher dans un panel dédié
end;
```

### Récapitulatif avant la fin

```pascal
procedure TFormAssistant.AfficherRecapitulatif;  
var  
  Recap: TStringList;
begin
  Recap := TStringList.Create;
  try
    Recap.Add('Récapitulatif de votre configuration :');
    Recap.Add('');
    Recap.Add('Nom : ' + EditNom.Text);
    Recap.Add('Email : ' + EditEmail.Text);
    Recap.Add('Option 1 : ' + BoolToStr(CheckBox1.Checked, True));
    Recap.Add('Option 2 : ' + BoolToStr(CheckBox2.Checked, True));

    MemoRecapitulatif.Lines.Assign(Recap);
  finally
    Recap.Free;
  end;
end;
```

## Design et ergonomie

### Principes de conception

**1. Clarté**
- Titre clair pour chaque page
- Instructions précises
- Pas de jargon technique inutile

**2. Cohérence**
- Même style visuel sur toutes les pages
- Boutons toujours au même endroit
- Navigation prévisible

**3. Feedback**
- Indicateur de progression visible
- Messages de validation clairs
- Confirmation des actions importantes

**4. Simplicité**
- Pas plus de 5-7 étapes
- Questions simples et directes
- Valeurs par défaut sensées

### Exemple de mise en page

```pascal
procedure TFormAssistant.ConfigurerMiseEnPage;  
begin  
  // En-tête avec titre et description
  PanelEntete.Height := 60;
  PanelEntete.Color := clWhite;
  LabelTitre.Font.Size := 12;
  LabelTitre.Font.Style := [fsBold];

  // Zone de contenu
  PanelContenu.Align := alClient;
  PanelContenu.Padding.Left := 20;
  PanelContenu.Padding.Right := 20;
  PanelContenu.Padding.Top := 10;

  // Pied de page avec boutons
  PanelBoutons.Height := 50;
  PanelBoutons.Align := alBottom;

  // Espacement des boutons
  ButtonTerminer.Left := PanelBoutons.Width - ButtonTerminer.Width - 10;
  ButtonSuivant.Left := ButtonTerminer.Left - ButtonSuivant.Width - 5;
  ButtonPrecedent.Left := ButtonSuivant.Left - ButtonPrecedent.Width - 5;
  ButtonAnnuler.Left := 10;
end;
```

## Bonnes pratiques

### 1. Nombre d'étapes optimal

```pascal
// BON : 3-5 étapes
// - Bienvenue
// - Configuration
// - Confirmation

// MAUVAIS : 10+ étapes
// L'utilisateur se décourage
```

### 2. Toujours permettre de revenir en arrière

```pascal
// Sauf pendant l'exécution d'une tâche
if PasEnCoursDeTraitement then
  ButtonPrecedent.Enabled := PageIndex > 0
else
  ButtonPrecedent.Enabled := False;
```

### 3. Validation immédiate

```pascal
// Valider pendant la saisie, pas seulement au clic sur Suivant
procedure TFormAssistant.EditEmailChange(Sender: TObject);  
begin  
  if ValiderEmail(EditEmail.Text) then
  begin
    ImageValidation.Picture.LoadFromFile('check.png');
    ButtonSuivant.Enabled := True;
  end
  else
  begin
    ImageValidation.Picture.LoadFromFile('cross.png');
    ButtonSuivant.Enabled := False;
  end;
end;
```

### 4. Sauvegarder les préférences

```pascal
// Se souvenir des choix de l'utilisateur
procedure TFormAssistant.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  if ModalResult = mrOk then
    SauvegarderPreferences;
end;
```

### 5. Tester tous les chemins

```pascal
// Tester :
// - Annulation à chaque étape
// - Navigation avant/arrière
// - Validation de chaque champ
// - Cas limites (champs vides, valeurs extrêmes)
```

### 6. Fournir une aide

```pascal
// Bouton d'aide ou lien vers la documentation
ButtonAide.Visible := True;  
ButtonAide.OnClick := AfficherAideContextuelle;  
```

## Alternatives modernes

### Interface par étapes sans wizard

Pour des applications modernes, considérez :

**1. Formulaire multi-pages simple**
- Pas de boutons Précédent/Suivant
- Navigation par étapes numérotées cliquables
- Plus moderne et flexible

**2. Formulaire progressif**
- Les champs apparaissent progressivement
- Validation en temps réel
- Plus fluide

**3. Onboarding interactif**
- Guidage contextuel dans l'application réelle
- Tooltips et highlights
- Plus engageant

## Résumé

Les assistants (wizards) sont parfaits pour guider les utilisateurs à travers des processus complexes. Les points clés à retenir :

- **Structure** : Pages multiples avec navigation Précédent/Suivant/Terminer
- **Validation** : Valider chaque étape avant de continuer
- **Progression** : Indicateur visuel de l'avancement
- **Simplicité** : 3-5 étapes maximum, questions claires
- **Flexibilité** : TPageControl (simple) ou Panels (flexible)
- **UX** : Valeurs par défaut, aide contextuelle, possibilité d'annuler
- **Design** : Cohérent, clair, professionnel
- **Bonnes pratiques** : Toujours permettre le retour en arrière, sauvegarder les préférences

Un assistant bien conçu améliore significativement l'expérience utilisateur pour les tâches complexes ou rarement effectuées.

⏭️ [Navigation dans les applications mobiles](/06-applications-multi-fenetres-et-navigation/06-navigation-dans-les-applications-mobiles.md)
