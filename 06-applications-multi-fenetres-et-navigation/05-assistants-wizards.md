# 6.5 Assistants (Wizards)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les assistants (ou wizards) sont des interfaces utilisateur spéciales qui guident l'utilisateur à travers un processus en plusieurs étapes. Ils sont particulièrement utiles pour les opérations complexes qui nécessitent de nombreuses informations ou décisions de la part de l'utilisateur.

## Pourquoi utiliser des assistants ?

Les assistants sont idéaux pour :
- Les processus d'installation
- La configuration initiale d'une application
- La création d'éléments complexes (rapports, connexions à des bases de données...)
- Les processus de saisie longs ou complexes
- Les situations où l'utilisateur a besoin d'être guidé étape par étape

## Structure d'un assistant

Un assistant typique se compose de :
1. **Une série de pages** (ou étapes) présentées dans un ordre logique
2. **Des boutons de navigation** (Précédent, Suivant, Terminer, Annuler)
3. **Une barre de progression** ou un indicateur d'étape
4. **Un titre et des instructions** clairs sur chaque page

## Création d'un assistant simple avec Delphi

### Approche 1 : Utiliser un TPageControl

La méthode la plus simple pour créer un assistant est d'utiliser un `TPageControl` avec des onglets masqués.

#### Étape 1 : Créer le formulaire de base

1. Créez un nouveau formulaire (`File → New → Form`)
2. Configurez-le avec les propriétés suivantes :
   - `BorderStyle = bsDialog` (non redimensionnable)
   - `Position = poScreenCenter` (centré à l'écran)
   - `Caption = 'Mon Assistant'`

#### Étape 2 : Ajouter le PageControl et les pages

1. Ajoutez un `TPageControl` au formulaire et positionnez-le pour qu'il occupe la majeure partie
2. Ajoutez au moins 3 `TTabSheet` (pages) au `PageControl`
3. Définissez la propriété `TabVisible` du `PageControl` à `False` pour masquer les onglets

```pascal
// Structure de base du formulaire assistant
unit WizardForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TWizardForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel1: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    procedure UpdateButtons;
  public
    { Public declarations }
  end;

var
  WizardForm: TWizardForm;

implementation

{$R *.dfm}
```

#### Étape 3 : Ajouter les contrôles de navigation

1. Ajoutez un `TPanel` en bas du formulaire pour contenir les boutons
2. Ajoutez les boutons : Précédent, Suivant, Terminer, Annuler
3. Le bouton "Terminer" partagera la même position que "Suivant" et sera caché initialement

```pascal
procedure TWizardForm.FormCreate(Sender: TObject);
begin
  // Assurez-vous que la première page est sélectionnée
  PageControl1.ActivePageIndex := 0;

  // Masquez les onglets
  PageControl1.TabVisible := False;

  // Configurez les boutons initialement
  UpdateButtons;
end;

procedure TWizardForm.UpdateButtons;
begin
  // Activer/désactiver le bouton Précédent
  btnPrevious.Enabled := PageControl1.ActivePageIndex > 0;

  // Changer le bouton Suivant en Terminer sur la dernière page
  if PageControl1.ActivePageIndex = PageControl1.PageCount - 1 then
  begin
    btnNext.Caption := 'Terminer';
    btnNext.ModalResult := mrOk;
  end
  else
  begin
    btnNext.Caption := 'Suivant >';
    btnNext.ModalResult := mrNone;
  end;
end;

procedure TWizardForm.btnPreviousClick(Sender: TObject);
begin
  if PageControl1.ActivePageIndex > 0 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
    UpdateButtons;
  end;
end;

procedure TWizardForm.btnNextClick(Sender: TObject);
begin
  // Si nous sommes sur la dernière page et que "Terminer" est cliqué
  if (PageControl1.ActivePageIndex = PageControl1.PageCount - 1) then
  begin
    // L'assistant sera fermé car ModalResult est défini à mrOk
  end
  else
  begin
    // Validation de la page actuelle avant de passer à la suivante
    if ValidateCurrentPage then
    begin
      // Passer à la page suivante
      PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
      UpdateButtons;
    end;
  end;
end;

procedure TWizardForm.btnCancelClick(Sender: TObject);
begin
  if MessageDlg('Voulez-vous vraiment annuler l''assistant ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ModalResult := mrCancel;
end;

function TWizardForm.ValidateCurrentPage: Boolean;
begin
  // Par défaut, autoriser le passage à la page suivante
  Result := True;

  // Validation spécifique selon la page active
  case PageControl1.ActivePageIndex of
    0: // Première page
      begin
        // Exemple : vérifier qu'un champ obligatoire est rempli
        if Edit1.Text = '' then
        begin
          ShowMessage('Veuillez remplir tous les champs obligatoires.');
          Edit1.SetFocus;
          Result := False;
        end;
      end;
    1: // Deuxième page
      begin
        // Validation de la deuxième page
      end;
    // ... Autres pages
  end;
end;
```

#### Étape 4 : Personnaliser chaque page

Sur chaque page (TabSheet), ajoutez :
- Un titre explicite (avec un TLabel en gras)
- Des instructions claires
- Les contrôles nécessaires pour recueillir les informations

Par exemple, pour une page de saisie d'informations personnelles :

```pascal
// Dans le gestionnaire d'événement FormCreate
procedure TWizardForm.FormCreate(Sender: TObject);
begin
  // ... Code existant

  // Configuration des pages
  TabSheet1.Caption := 'Bienvenue';
  TabSheet2.Caption := 'Informations personnelles';
  TabSheet3.Caption := 'Configuration';

  // Configurez les labels de titre sur chaque page (supposons qu'ils existent)
  lblTitle1.Caption := 'Bienvenue dans l''assistant de configuration';
  lblTitle2.Caption := 'Informations personnelles';
  lblTitle3.Caption := 'Paramètres de l''application';
end;
```

### Approche 2 : Utiliser plusieurs formulaires

Pour les assistants plus complexes, vous pouvez utiliser plusieurs formulaires :

1. Créez un formulaire "conteneur" qui affichera dynamiquement les autres formulaires
2. Créez un formulaire distinct pour chaque étape
3. Utilisez une variable pour suivre l'étape actuelle

```pascal
// Formulaire principal (conteneur)
unit WizardMain;

interface

// ... Imports

type
  TWizardStep = (wsWelcome, wsPersonalInfo, wsConfiguration, wsComplete);

  TWizardMainForm = class(TForm)
    // ... Composants
    pnlContent: TPanel;
    pnlButtons: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCurrentStep: TWizardStep;
    FStepForms: array[TWizardStep] of TForm;
    FWizardData: TMyWizardData;
    procedure ShowStep(Step: TWizardStep);
    procedure UpdateButtons;
    function ValidateCurrentStep: Boolean;
  public
    // ...
  end;

// ... Implementation

procedure TWizardMainForm.FormCreate(Sender: TObject);
begin
  // Créer les formulaires d'étape
  FStepForms[wsWelcome] := TWelcomeForm.Create(Self);
  FStepForms[wsPersonalInfo] := TPersonalInfoForm.Create(Self);
  FStepForms[wsConfiguration] := TConfigForm.Create(Self);
  FStepForms[wsComplete] := TCompleteForm.Create(Self);

  // Configurez chaque formulaire pour qu'il s'adapte au panneau de contenu
  for var Step := Low(TWizardStep) to High(TWizardStep) do
  begin
    FStepForms[Step].Parent := pnlContent;
    FStepForms[Step].BorderStyle := bsNone;
    FStepForms[Step].Align := alClient;
    FStepForms[Step].Visible := False;
  end;

  // Initialiser les données
  FWizardData := TMyWizardData.Create;

  // Démarrer par la première étape
  FCurrentStep := wsWelcome;
  ShowStep(FCurrentStep);
  UpdateButtons;
end;

procedure TWizardMainForm.ShowStep(Step: TWizardStep);
begin
  // Cacher toutes les étapes
  for var S := Low(TWizardStep) to High(TWizardStep) do
    FStepForms[S].Visible := False;

  // Afficher l'étape demandée
  FStepForms[Step].Visible := True;
  FCurrentStep := Step;

  // Mise à jour spécifique selon l'étape
  case Step of
    wsWelcome:
      Caption := 'Assistant - Bienvenue';
    wsPersonalInfo:
      begin
        Caption := 'Assistant - Informations personnelles';
        // Si nous avons des données existantes, les afficher
        TPersonalInfoForm(FStepForms[wsPersonalInfo]).LoadFromData(FWizardData);
      end;
    // ... autres étapes
  end;
end;

// ... autres méthodes
```

## Exemple complet : Assistant d'installation

Voici un exemple plus complet d'un assistant d'installation à 4 étapes :

1. Page de bienvenue avec présentation
2. Page de sélection du dossier d'installation
3. Page de sélection des composants à installer
4. Page de confirmation et début de l'installation

### Structure du formulaire principal

```pascal
unit InstallWizard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.FileCtrl, System.UITypes;

type
  TInstallData = record
    InstallPath: string;
    CreateDesktopShortcut: Boolean;
    CreateStartMenuShortcut: Boolean;
    InstallDocs: Boolean;
    InstallSamples: Boolean;
  end;

  TInstallWizardForm = class(TForm)
    PageControl1: TPageControl;
    tabWelcome: TTabSheet;
    tabFolder: TTabSheet;
    tabComponents: TTabSheet;
    tabInstall: TTabSheet;
    pnlButtons: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    lblTitleWelcome: TLabel;
    lblInfoWelcome: TLabel;
    Image1: TImage;
    lblTitleFolder: TLabel;
    lblInfoFolder: TLabel;
    lblInstallPath: TLabel;
    edtInstallPath: TEdit;
    btnBrowse: TButton;
    lblTitleComponents: TLabel;
    lblInfoComponents: TLabel;
    chkDesktopIcon: TCheckBox;
    chkStartMenu: TCheckBox;
    chkDocumentation: TCheckBox;
    chkSamples: TCheckBox;
    lblTitleInstall: TLabel;
    lblInfoInstall: TLabel;
    memoSummary: TMemo;
    ProgressBar1: TProgressBar;
    btnInstall: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FIsInstalling: Boolean;
    FInstallData: TInstallData;
    procedure UpdateButtons;
    function ValidateCurrentPage: Boolean;
    procedure UpdateSummary;
    procedure StartInstallation;
    procedure SimulateInstallStep(Step: Integer; const StepName: string);
  public
    { Public declarations }
  end;

var
  InstallWizardForm: TInstallWizardForm;

implementation

{$R *.dfm}
```

### Implémentation de l'assistant d'installation

```pascal
procedure TInstallWizardForm.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  PageControl1.ActivePageIndex := 0;
  PageControl1.TabVisible := False;
  FIsInstalling := False;

  // Valeurs par défaut pour les données d'installation
  FInstallData.InstallPath := 'C:\Program Files\MonApplication';
  FInstallData.CreateDesktopShortcut := True;
  FInstallData.CreateStartMenuShortcut := True;
  FInstallData.InstallDocs := True;
  FInstallData.InstallSamples := False;

  // Initialiser les contrôles avec les valeurs par défaut
  edtInstallPath.Text := FInstallData.InstallPath;
  chkDesktopIcon.Checked := FInstallData.CreateDesktopShortcut;
  chkStartMenu.Checked := FInstallData.CreateStartMenuShortcut;
  chkDocumentation.Checked := FInstallData.InstallDocs;
  chkSamples.Checked := FInstallData.InstallSamples;

  // Configurer les titres
  lblTitleWelcome.Caption := 'Bienvenue dans l''assistant d''installation';
  lblTitleFolder.Caption := 'Dossier d''installation';
  lblTitleComponents.Caption := 'Composants à installer';
  lblTitleInstall.Caption := 'Prêt à installer';

  // Mettre à jour les boutons
  UpdateButtons;
end;

procedure TInstallWizardForm.UpdateButtons;
begin
  // Gérer l'état des boutons selon la page active
  btnPrevious.Enabled := (PageControl1.ActivePageIndex > 0) and not FIsInstalling;

  // Sur la dernière page, masquer le bouton Suivant et afficher Installer
  // (Les boutons peuvent être sur la même position et l'un est caché selon le contexte)
  if PageControl1.ActivePageIndex = PageControl1.PageCount - 1 then
  begin
    btnNext.Visible := False;
    btnInstall.Visible := True and not FIsInstalling;
  end
  else
  begin
    btnNext.Visible := True;
    btnInstall.Visible := False;
  end;

  // Désactiver Annuler pendant l'installation
  btnCancel.Enabled := not FIsInstalling;
end;

procedure TInstallWizardForm.btnPreviousClick(Sender: TObject);
begin
  if PageControl1.ActivePageIndex > 0 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
    UpdateButtons;
  end;
end;

procedure TInstallWizardForm.btnNextClick(Sender: TObject);
begin
  if ValidateCurrentPage then
  begin
    // Si nous sommes sur l'avant-dernière page, préparer le résumé
    if PageControl1.ActivePageIndex = PageControl1.PageCount - 2 then
      UpdateSummary;

    // Passer à la page suivante
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
    UpdateButtons;
  end;
end;

function TInstallWizardForm.ValidateCurrentPage: Boolean;
begin
  Result := True;

  case PageControl1.ActivePageIndex of
    1: // Page du dossier d'installation
      begin
        if Trim(edtInstallPath.Text) = '' then
        begin
          ShowMessage('Veuillez spécifier un dossier d''installation valide.');
          edtInstallPath.SetFocus;
          Result := False;
          Exit;
        end;

        // Mettre à jour les données
        FInstallData.InstallPath := edtInstallPath.Text;
      end;
    2: // Page des composants
      begin
        // Mettre à jour les données
        FInstallData.CreateDesktopShortcut := chkDesktopIcon.Checked;
        FInstallData.CreateStartMenuShortcut := chkStartMenu.Checked;
        FInstallData.InstallDocs := chkDocumentation.Checked;
        FInstallData.InstallSamples := chkSamples.Checked;

        // Vérifier qu'au moins un composant est sélectionné
        if not (FInstallData.CreateDesktopShortcut or
                FInstallData.CreateStartMenuShortcut or
                FInstallData.InstallDocs or
                FInstallData.InstallSamples) then
        begin
          ShowMessage('Veuillez sélectionner au moins un composant à installer.');
          Result := False;
          Exit;
        end;
      end;
  end;
end;

procedure TInstallWizardForm.UpdateSummary;
begin
  memoSummary.Clear;
  memoSummary.Lines.Add('Résumé de l''installation :');
  memoSummary.Lines.Add('');
  memoSummary.Lines.Add('Dossier d''installation : ' + FInstallData.InstallPath);
  memoSummary.Lines.Add('');
  memoSummary.Lines.Add('Composants à installer :');

  if FInstallData.CreateDesktopShortcut then
    memoSummary.Lines.Add('- Raccourci sur le bureau');

  if FInstallData.CreateStartMenuShortcut then
    memoSummary.Lines.Add('- Raccourci dans le menu Démarrer');

  if FInstallData.InstallDocs then
    memoSummary.Lines.Add('- Documentation');

  if FInstallData.InstallSamples then
    memoSummary.Lines.Add('- Exemples et échantillons');

  memoSummary.Lines.Add('');
  memoSummary.Lines.Add('Cliquez sur "Installer" pour démarrer l''installation.');
end;

procedure TInstallWizardForm.btnBrowseClick(Sender: TObject);
var
  Dir: string;
begin
  Dir := edtInstallPath.Text;

  if SelectDirectory('Sélectionnez le dossier d''installation :', '', Dir,
                     [sdNewFolder, sdShowShares, sdNewUI, sdValidateDir]) then
  begin
    edtInstallPath.Text := Dir;
  end;
end;

procedure TInstallWizardForm.btnInstallClick(Sender: TObject);
begin
  // Démarrer le processus d'installation
  btnInstall.Enabled := False;
  btnPrevious.Enabled := False;
  btnCancel.Enabled := False;
  FIsInstalling := True;

  StartInstallation;
end;

procedure TInstallWizardForm.StartInstallation;
begin
  // Dans une application réelle, vous lanceriez un thread pour l'installation
  // Ici, nous simulons simplement avec un délai

  ProgressBar1.Min := 0;
  ProgressBar1.Max := 100;
  ProgressBar1.Position := 0;

  // Simuler les étapes d'installation
  SimulateInstallStep(10, 'Préparation de l''installation...');
  SimulateInstallStep(30, 'Copie des fichiers principaux...');

  if FInstallData.InstallDocs then
    SimulateInstallStep(50, 'Installation de la documentation...');

  if FInstallData.InstallSamples then
    SimulateInstallStep(70, 'Installation des exemples...');

  if FInstallData.CreateDesktopShortcut then
    SimulateInstallStep(80, 'Création du raccourci sur le bureau...');

  if FInstallData.CreateStartMenuShortcut then
    SimulateInstallStep(90, 'Création du raccourci dans le menu Démarrer...');

  SimulateInstallStep(100, 'Finalisation de l''installation...');

  // Installation terminée
  memoSummary.Lines.Add('');
  memoSummary.Lines.Add('Installation terminée avec succès !');

  // Afficher un bouton pour fermer l'assistant
  btnCancel.Caption := 'Terminer';
  btnCancel.Enabled := True;
  FIsInstalling := False;
end;

procedure TInstallWizardForm.SimulateInstallStep(Step: Integer; const StepName: string);
begin
  memoSummary.Lines.Add(StepName);
  Application.ProcessMessages;

  // Simuler un traitement qui prend du temps
  ProgressBar1.Position := Step;
  Sleep(1000);
  Application.ProcessMessages;
end;

procedure TInstallWizardForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TInstallWizardForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FIsInstalling then
  begin
    // Ne pas permettre de fermer pendant l'installation
    CanClose := False;
    ShowMessage('L''installation est en cours. Veuillez attendre qu''elle se termine.');
  end
  else if (PageControl1.ActivePageIndex < PageControl1.PageCount - 1) or
          (ProgressBar1.Position < 100) then
  begin
    // Demander confirmation avant d'annuler
    CanClose := MessageDlg('Voulez-vous vraiment annuler l''installation ?',
                           mtConfirmation, [mbYes, mbNo], 0) = mrYes;
  end;
  // Sinon, autoriser la fermeture
end;
```

## Bonnes pratiques pour les assistants

### 1. Navigation intuitive

- Utilisez des boutons clairement étiquetés (Précédent, Suivant, Terminer, Annuler)
- Activez/désactivez les boutons selon le contexte (par exemple, désactivez "Précédent" sur la première page)
- Proposez un moyen évident de quitter l'assistant à tout moment

### 2. Présentation claire

- Donnez à chaque page un titre descriptif
- Fournissez de brèves instructions sur ce que l'utilisateur doit faire
- Utilisez des icônes ou des images pour aider à la compréhension
- Gardez une cohérence visuelle entre toutes les pages

### 3. Validation des données

- Validez les entrées avant de passer à la page suivante
- Donnez des messages d'erreur clairs et des solutions
- Ne permettez pas à l'utilisateur d'avancer si les données requises sont invalides

### 4. Gestion de l'état

- Conservez les sélections de l'utilisateur lorsqu'il navigue entre les pages
- Permettez à l'utilisateur de revenir en arrière pour modifier ses choix
- Montrez un résumé des choix avant la finalisation

### 5. Retour d'information

- Montrez la progression globale (par exemple, "Étape 2 sur 4")
- Pour les opérations longues, utilisez des barres de progression
- Informez l'utilisateur une fois le processus terminé

## Techniques avancées

### Assistants dynamiques avec nombre de pages variable

```pascal
// Ajout ou suppression dynamique de pages selon les choix utilisateur
procedure TWizardForm.UpdateWizardPages;
begin
  // Si l'utilisateur a choisi l'installation personnalisée
  if rbCustomInstall.Checked then
  begin
    // Ajouter la page de sélection de composants si elle n'existe pas déjà
    if not HasPage('tabComponents') then
    begin
      var NewPage := TTabSheet.Create(PageControl1);
      NewPage.PageControl := PageControl1;
      NewPage.Name := 'tabComponents';
      NewPage.Caption := 'Composants';
      // ... configurer la page
    end;
  end
  else
  begin
    // Supprimer la page de sélection de composants
    var ComponentPage := FindPage('tabComponents');
    if ComponentPage <> nil then
      ComponentPage.Free;
  end;

  // Recalculer les indices de page après modification
  RecalculatePageIndices;
end;
```

### Gestion des dépendances entre options

```pascal
procedure TWizardForm.chkAdvancedFeaturesClick(Sender: TObject);
begin
  // Si les fonctionnalités avancées sont sélectionnées, certaines options deviennent disponibles
  chkCloudSync.Enabled := chkAdvancedFeatures.Checked;
  chkAutoBackup.Enabled := chkAdvancedFeatures.Checked;

  // Si on désactive les fonctionnalités avancées, désélectionner aussi les options dépendantes
  if not chkAdvancedFeatures.Checked then
  begin
    chkCloudSync.Checked := False;
    chkAutoBackup.Checked := False;
  end;
end;
```

### Persistance de l'état entre sessions

```pascal
// Sauvegarder l'état en cas d'annulation pour reprendre plus tard
procedure TWizardForm.SaveWizardState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApp\InstallWizard', True) then
    begin
      Registry.WriteString('InstallPath', edtInstallPath.Text);
      Registry.WriteBool('DesktopIcon', chkDesktopIcon.Checked);
      // ... autres valeurs
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

// Charger l'état sauvegardé
procedure TWizardForm.LoadWizardState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApp\InstallWizard', False) then
    begin
      if Registry.ValueExists('InstallPath') then
        edtInstallPath.Text := Registry.ReadString('InstallPath');

      if Registry.ValueExists('DesktopIcon') then
        chkDesktopIcon.Checked := Registry.ReadBool('DesktopIcon');

      // ... autres valeurs
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;
```

## Alternatives et améliorations

### Utilisation de frames au lieu de TabSheets

Les frames (`TFrame`) peuvent être une alternative plus modulaire aux `TTabSheet` :

```pascal
// Dans le formulaire principal
private
  FCurrentFrame: TFrame;
  FWelcomeFrame: TWelcomeFrame;
  FSettingsFrame: TSettingsFrame;
  FInstallFrame: TInstallFrame;

  procedure ShowFrame(AFrame: TFrame);

// Implémentation
procedure TWizardForm.ShowFrame(AFrame: TFrame);
begin
  if FCurrentFrame <> nil then
    FCurrentFrame.Visible := False;

  FCurrentFrame := AFrame;

  if FCurrentFrame <> nil then
  begin
    FCurrentFrame.Parent := pnlContent;
    FCurrentFrame.Align := alClient;
    FCurrentFrame.Visible := True;
  end;
end;
```

## Interface utilisateur plus moderne

Pour une interface plus moderne, vous pouvez ajouter :
- Une barre latérale montrant toutes les étapes
- Des transitions animées entre les pages
- Un design responsive qui s'adapte à différentes tailles d'écran

```pascal
// Exemple d'ajout d'une barre latérale
procedure TWizardForm.CreateSidebar;
var
  StepNames: array[0..3] of string = ('Bienvenue', 'Dossier', 'Composants', 'Installation');
  I: Integer;
begin
  // Créer les éléments de la barre latérale
  for I := 0 to High(StepNames) do
  begin
    var Item := TPanel.Create(Self);
    Item.Parent := pnlSidebar;
    Item.Align := alTop;
    Item.Height := 50;
    Item.Caption := StepNames[I];
    Item.Tag := I; // Pour identifier l'étape
    Item.OnClick := SidebarItemClick;

    // Ajouter une icône ou un indicateur
    var Indicator := TShape.Create(Self);
    Indicator.Parent := Item;
    Indicator.Shape := stCircle;
    Indicator.Width := 16;
    Indicator.Height := 16;
    Indicator.Left := 10;
    Indicator.Top := (Item.Height - Indicator.Height) div 2;

    // Stocker une référence à l'indicateur pour pouvoir le mettre à jour
    Item.Tag := NativeInt(Indicator);
  end;

  // Mettre à jour la barre latérale initiale
  UpdateSidebar;
end;

procedure TWizardForm.SidebarItemClick(Sender: TObject);
var
  TargetPage: Integer;
  CanNavigate: Boolean;
begin
  if Sender is TPanel then
  begin
    TargetPage := (Sender as TPanel).Tag;

    // Vérifier si on peut naviguer directement vers cette page
    // (généralement, on permet uniquement d'aller aux pages déjà visitées)
    CanNavigate := TargetPage <= FMaxVisitedPage;

    if CanNavigate then
    begin
      PageControl1.ActivePageIndex := TargetPage;
      UpdateButtons;
      UpdateSidebar;
    end;
  end;
end;

procedure TWizardForm.UpdateSidebar;
var
  I: Integer;
  Item: TPanel;
  Indicator: TShape;
begin
  for I := 0 to pnlSidebar.ControlCount - 1 do
  begin
    if pnlSidebar.Controls[I] is TPanel then
    begin
      Item := TPanel(pnlSidebar.Controls[I]);
      Indicator := TShape(Item.Tag);

      // Marquer l'étape actuelle
      if I = PageControl1.ActivePageIndex then
      begin
        Item.Font.Style := [fsBold];
        Indicator.Brush.Color := clHighlight;
      end
      // Marquer les étapes déjà visitées
      else if I <= FMaxVisitedPage then
      begin
        Item.Font.Style := [];
        Indicator.Brush.Color := clGreen;
      end
      // Marquer les étapes futures
      else
      begin
        Item.Font.Style := [];
        Indicator.Brush.Color := clGray;
      end;
    end;
  end;
end;
```

### Effets de transition entre les pages

Vous pouvez ajouter des effets de transition pour une expérience plus fluide :

```pascal
// Propriétés à ajouter à votre classe
private
  FTransitionTimer: TTimer;
  FTransitionStep: Integer;
  FTransitionFromPage, FTransitionToPage: TTabSheet;

  procedure StartTransition(FromPage, ToPage: TTabSheet);
  procedure TransitionTimerEvent(Sender: TObject);

// Implémentation
procedure TWizardForm.StartTransition(FromPage, ToPage: TTabSheet);
begin
  // Préparer la transition
  FTransitionFromPage := FromPage;
  FTransitionToPage := ToPage;
  FTransitionStep := 0;

  // Configurer un timer pour l'animation (30 images par seconde)
  if FTransitionTimer = nil then
  begin
    FTransitionTimer := TTimer.Create(Self);
    FTransitionTimer.Interval := 33; // ~30 FPS
    FTransitionTimer.OnTimer := TransitionTimerEvent;
  end;

  // Prendre une capture de l'état actuel
  FFromBitmap := TBitmap.Create;
  FToBitmap := TBitmap.Create;

  // Rendre les deux pages dans des bitmaps
  RenderPageToBitmap(FromPage, FFromBitmap);
  RenderPageToBitmap(ToPage, FToBitmap);

  // Démarrer l'animation
  FTransitionTimer.Enabled := True;
end;

procedure TWizardForm.TransitionTimerEvent(Sender: TObject);
var
  Alpha: Byte;
begin
  Inc(FTransitionStep);

  // 15 étapes pour l'animation complète
  if FTransitionStep >= 15 then
  begin
    // Fin de l'animation
    FTransitionTimer.Enabled := False;
    FFromBitmap.Free;
    FToBitmap.Free;

    // Activer réellement la nouvelle page
    PageControl1.ActivePage := FTransitionToPage;
    Exit;
  end;

  // Calculer l'alpha (transparence) pour le fondu
  Alpha := Round(FTransitionStep / 15 * 255);

  // Dessiner l'animation de transition
  with pnlTransition.Canvas do
  begin
    // Dessiner l'ancienne page
    Draw(0, 0, FFromBitmap);

    // Superposer la nouvelle page avec transparence
    DrawTransparent(0, 0, FToBitmap, Alpha);
  end;
end;

procedure TWizardForm.RenderPageToBitmap(Page: TTabSheet; Bitmap: TBitmap);
begin
  Bitmap.Width := Page.Width;
  Bitmap.Height := Page.Height;

  Bitmap.Canvas.Lock;
  try
    // Capturer le contenu de la page
    Page.PaintTo(Bitmap.Canvas.Handle, 0, 0);
  finally
    Bitmap.Canvas.Unlock;
  end;
end;
```

### Adaptation à différentes résolutions d'écran

Pour créer un assistant qui s'adapte bien à différentes tailles d'écran :

```pascal
procedure TWizardForm.FormCreate(Sender: TObject);
begin
  // ... Autre code d'initialisation

  // Ajuster la taille initiale
  AdjustFormSize;
end;

procedure TWizardForm.AdjustFormSize;
var
  ScreenWidth, ScreenHeight: Integer;
begin
  // Obtenir la taille de l'écran de travail (sans la barre des tâches)
  ScreenWidth := Screen.WorkAreaWidth;
  ScreenHeight := Screen.WorkAreaHeight;

  // Calculer la taille idéale (par exemple, 70% de l'écran)
  Width := Round(ScreenWidth * 0.7);
  Height := Round(ScreenHeight * 0.7);

  // Centrer le formulaire
  Position := poScreenCenter;

  // Ajuster la taille des contrôles si nécessaire
  AdjustControlSizes;
end;

procedure TWizardForm.AdjustControlSizes;
var
  MinWidth: Integer;
begin
  // S'assurer que les contrôles ont une taille minimale appropriée
  MinWidth := Round(Width * 0.6);

  // Ajuster les champs de saisie
  edtInstallPath.Width := MinWidth;

  // Ajuster les tailles des panels ou autres contrôles
  if Assigned(pnlSidebar) then
    pnlSidebar.Width := Round(Width * 0.2);
end;
```

## Exemple complet : Assistant de configuration d'application métier

Voici un exemple plus élaboré d'un assistant utilisé pour configurer une application métier :

### 1. Structure de l'assistant

Le wizard comprend 5 étapes :
1. Bienvenue et présentation
2. Configuration de la base de données
3. Paramètres utilisateur
4. Personnalisation de l'interface
5. Résumé et finalisation

### 2. Définition des données partagées

```pascal
// Structure pour stocker toutes les données de configuration
type
  TAppSettings = class
  public
    // Paramètres de base de données
    DatabaseType: string; // 'MySQL', 'SQLite', etc.
    ServerAddress: string;
    DatabaseName: string;
    Username: string;
    Password: string;

    // Paramètres utilisateur
    CompanyName: string;
    AdminEmail: string;
    EnableNotifications: Boolean;
    Language: string;

    // Personnalisation
    Theme: string;
    ShowStatusBar: Boolean;
    ShowToolbar: Boolean;
    CompactMode: Boolean;

    // Méthodes
    constructor Create;
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
    function TestDatabaseConnection: Boolean;
  end;
```

### 3. Formulaire principal de l'assistant

```pascal
type
  TSetupWizardForm = class(TForm)
    PageControl1: TPageControl;
    tabWelcome: TTabSheet;
    tabDatabase: TTabSheet;
    tabUser: TTabSheet;
    tabUI: TTabSheet;
    tabSummary: TTabSheet;
    pnlSidebar: TPanel;
    pnlButtons: TPanel;
    btnPrevious: TButton;
    btnNext: TButton;
    btnCancel: TButton;
    // Contrôles pour la page base de données
    grpDBType: TRadioGroup;
    lblServer: TLabel;
    edtServer: TEdit;
    lblDatabase: TLabel;
    edtDatabase: TEdit;
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    btnTestConnection: TButton;
    // Contrôles pour la page utilisateur
    lblCompany: TLabel;
    edtCompany: TEdit;
    lblEmail: TLabel;
    edtEmail: TEdit;
    chkNotifications: TCheckBox;
    cmbLanguage: TComboBox;
    // Contrôles pour la page interface
    grpTheme: TRadioGroup;
    chkStatusBar: TCheckBox;
    chkToolbar: TCheckBox;
    chkCompactMode: TCheckBox;
    // Contrôles pour la page résumé
    memoSummary: TMemo;
    btnFinish: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnTestConnectionClick(Sender: TObject);
    procedure btnFinishClick(Sender: TObject);
    procedure grpDBTypeClick(Sender: TObject);

  private
    FSettings: TAppSettings;
    FMaxVisitedPage: Integer;

    procedure CreateSidebar;
    procedure UpdateSidebar;
    procedure UpdateButtons;
    function ValidateCurrentPage: Boolean;
    procedure UpdateSummary;
    procedure SaveSettings;
    procedure LoadSettings;
  end;
```

### 4. Implémentation des fonctionnalités clés

```pascal
procedure TSetupWizardForm.FormCreate(Sender: TObject);
begin
  // Initialisation
  PageControl1.ActivePageIndex := 0;
  PageControl1.TabVisible := False;
  FMaxVisitedPage := 0;

  // Créer l'objet de paramètres
  FSettings := TAppSettings.Create;

  // Charger les paramètres sauvegardés (si existants)
  LoadSettings;

  // Créer la barre latérale
  CreateSidebar;

  // Initialiser les contrôles avec les valeurs actuelles
  InitializeControls;

  // Mettre à jour l'interface
  UpdateButtons;
end;

procedure TSetupWizardForm.UpdateButtons;
begin
  // Gérer l'état des boutons selon la page active
  btnPrevious.Enabled := PageControl1.ActivePageIndex > 0;

  // Sur la dernière page, masquer Suivant et afficher Terminer
  if PageControl1.ActivePageIndex = PageControl1.PageCount - 1 then
  begin
    btnNext.Visible := False;
    btnFinish.Visible := True;
  end
  else
  begin
    btnNext.Visible := True;
    btnFinish.Visible := False;
  end;

  // Mettre à jour la barre latérale
  UpdateSidebar;
end;

procedure TSetupWizardForm.btnNextClick(Sender: TObject);
begin
  // Valider la page actuelle
  if not ValidateCurrentPage then
    Exit;

  // Mettre à jour les paramètres depuis les contrôles
  UpdateSettingsFromUI;

  // Mémoriser la dernière page visitée
  if PageControl1.ActivePageIndex > FMaxVisitedPage then
    FMaxVisitedPage := PageControl1.ActivePageIndex;

  // Si on va à la page de résumé, mettre à jour le résumé
  if PageControl1.ActivePageIndex = PageControl1.PageCount - 2 then
    UpdateSummary;

  // Passer à la page suivante
  PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;

  // Mettre à jour l'interface
  UpdateButtons;
end;

function TSetupWizardForm.ValidateCurrentPage: Boolean;
begin
  Result := True;

  case PageControl1.ActivePageIndex of
    1: // Page base de données
      begin
        // Vérifier que les champs obligatoires sont remplis
        if (grpDBType.ItemIndex < 0) or
           (Trim(edtDatabase.Text) = '') then
        begin
          ShowMessage('Veuillez remplir tous les champs obligatoires.');
          Result := False;
          Exit;
        end;

        // Pour des bases distantes, vérifier l'adresse du serveur
        if (grpDBType.ItemIndex = 0) and (Trim(edtServer.Text) = '') then
        begin
          ShowMessage('Veuillez saisir l''adresse du serveur MySQL.');
          edtServer.SetFocus;
          Result := False;
          Exit;
        end;
      end;

    2: // Page utilisateur
      begin
        // Vérifier le format de l'email
        if (Trim(edtEmail.Text) <> '') and not IsValidEmail(edtEmail.Text) then
        begin
          ShowMessage('Veuillez saisir une adresse email valide.');
          edtEmail.SetFocus;
          Result := False;
          Exit;
        end;
      end;

    // Autres pages peuvent avoir des validations spécifiques
  end;
end;

procedure TSetupWizardForm.UpdateSummary;
begin
  memoSummary.Clear;
  memoSummary.Lines.Add('Résumé de la configuration :');
  memoSummary.Lines.Add('');

  // Base de données
  memoSummary.Lines.Add('== Base de données ==');
  memoSummary.Lines.Add('Type : ' + FSettings.DatabaseType);
  if FSettings.DatabaseType <> 'SQLite' then
  begin
    memoSummary.Lines.Add('Serveur : ' + FSettings.ServerAddress);
    memoSummary.Lines.Add('Nom d''utilisateur : ' + FSettings.Username);
    memoSummary.Lines.Add('Mot de passe : ' + StringOfChar('*', Length(FSettings.Password)));
  end
  memoSummary.Lines.Add('Base de données : ' + FSettings.DatabaseName);
  memoSummary.Lines.Add('');

  // Utilisateur
  memoSummary.Lines.Add('== Paramètres utilisateur ==');
  memoSummary.Lines.Add('Société : ' + FSettings.CompanyName);
  memoSummary.Lines.Add('Email admin : ' + FSettings.AdminEmail);
  memoSummary.Lines.Add('Notifications : ' + BoolToStr(FSettings.EnableNotifications, True));
  memoSummary.Lines.Add('Langue : ' + FSettings.Language);
  memoSummary.Lines.Add('');

  // Interface
  memoSummary.Lines.Add('== Interface ==');
  memoSummary.Lines.Add('Thème : ' + FSettings.Theme);
  memoSummary.Lines.Add('Barre d''état : ' + BoolToStr(FSettings.ShowStatusBar, True));
  memoSummary.Lines.Add('Barre d''outils : ' + BoolToStr(FSettings.ShowToolbar, True));
  memoSummary.Lines.Add('Mode compact : ' + BoolToStr(FSettings.CompactMode, True));

  memoSummary.Lines.Add('');
  memoSummary.Lines.Add('Cliquez sur "Terminer" pour appliquer ces paramètres.');
end;

procedure TSetupWizardForm.btnFinishClick(Sender: TObject);
begin
  // Enregistrer les paramètres
  SaveSettings;

  // Appliquer les paramètres à l'application
  ApplySettings;

  // Fermer l'assistant
  ModalResult := mrOk;
end;

procedure TSetupWizardForm.btnTestConnectionClick(Sender: TObject);
begin
  // Mettre à jour les paramètres de connexion depuis l'interface
  FSettings.DatabaseType := grpDBType.Items[grpDBType.ItemIndex];
  FSettings.ServerAddress := edtServer.Text;
  FSettings.DatabaseName := edtDatabase.Text;
  FSettings.Username := edtUsername.Text;
  FSettings.Password := edtPassword.Text;

  // Tester la connexion
  if FSettings.TestDatabaseConnection then
    ShowMessage('Connexion réussie à la base de données !')
  else
    ShowMessage('Erreur de connexion à la base de données. Vérifiez vos paramètres.');
end;

procedure TSetupWizardForm.grpDBTypeClick(Sender: TObject);
begin
  // Activer/désactiver les champs selon le type de base de données
  case grpDBType.ItemIndex of
    0: // MySQL
      begin
        lblServer.Enabled := True;
        edtServer.Enabled := True;
        lblUsername.Enabled := True;
        edtUsername.Enabled := True;
        lblPassword.Enabled := True;
        edtPassword.Enabled := True;
      end;
    1: // SQLite
      begin
        lblServer.Enabled := False;
        edtServer.Enabled := False;
        lblUsername.Enabled := False;
        edtUsername.Enabled := False;
        lblPassword.Enabled := False;
        edtPassword.Enabled := False;
      end;
  end;
end;
```

## Astuces supplémentaires

### Sauvegarde de progression

Il peut être utile de sauvegarder la progression de l'utilisateur, surtout pour les assistants longs :

```pascal
procedure TSetupWizardForm.SaveProgress;
var
  SettingsFile: string;
begin
  SettingsFile := ExtractFilePath(Application.ExeName) + 'wizard-temp.json';

  // Mettre à jour les paramètres depuis l'interface
  UpdateSettingsFromUI;

  // Enregistrer dans un fichier temporaire
  FSettings.SaveToFile(SettingsFile);
end;

procedure TSetupWizardForm.LoadProgress;
var
  SettingsFile: string;
begin
  SettingsFile := ExtractFilePath(Application.ExeName) + 'wizard-temp.json';

  if FileExists(SettingsFile) then
  begin
    // Charger depuis le fichier temporaire
    FSettings.LoadFromFile(SettingsFile);

    // Mettre à jour l'interface
    UpdateUIFromSettings;
  end;
end;
```

### Gestion des assistants longs

Pour les assistants très longs, considérez :

1. **Progression non linéaire** : permettre à l'utilisateur de sauter certaines pages optionnelles

```pascal
procedure TSetupWizardForm.btnSkipOptionalClick(Sender: TObject);
begin
  // Déterminer la prochaine page obligatoire
  var NextRequiredPage := FindNextRequiredPage(PageControl1.ActivePageIndex);

  // Aller directement à cette page
  PageControl1.ActivePageIndex := NextRequiredPage;
  UpdateButtons;
end;

function TSetupWizardForm.FindNextRequiredPage(CurrentPage: Integer): Integer;
begin
  // Logique pour déterminer la prochaine page obligatoire
  // Par exemple, ici on suppose que les pages 3 et 4 sont optionnelles
  if CurrentPage < 2 then
    Result := 2  // Aller à la page 2 (obligatoire)
  else
    Result := 5; // Aller à la page 5 (résumé)
end;
```

2. **Assistants en plusieurs parties** : diviser en plusieurs assistants distincts

```pascal
procedure TMainForm.RunSetupWizards;
begin
  // Premier assistant : configuration de base
  var BasicSetup := TBasicSetupWizard.Create(Self);
  try
    if BasicSetup.ShowModal <> mrOk then
      Exit; // L'utilisateur a annulé
  finally
    BasicSetup.Free;
  end;

  // Deuxième assistant : configuration avancée
  var AdvancedSetup := TAdvancedSetupWizard.Create(Self);
  try
    if AdvancedSetup.ShowModal <> mrOk then
      Exit; // L'utilisateur a annulé
  finally
    AdvancedSetup.Free;
  end;

  // Troisième assistant : personnalisation
  var CustomizationSetup := TCustomizationWizard.Create(Self);
  try
    CustomizationSetup.ShowModal;
  finally
    CustomizationSetup.Free;
  end;
end;
```

### Navigation conditionnelle

Parfois, vous voudrez que l'assistant suive différents chemins selon les choix de l'utilisateur :

```pascal
procedure TWizardForm.btnNextClick(Sender: TObject);
var
  NextPage: Integer;
begin
  if not ValidateCurrentPage then
    Exit;

  // Déterminer la page suivante en fonction des sélections
  NextPage := DetermineNextPage;

  // Aller à la page appropriée
  PageControl1.ActivePageIndex := NextPage;
  UpdateButtons;
end;

function TWizardForm.DetermineNextPage: Integer;
begin
  case PageControl1.ActivePageIndex of
    0: // Depuis la page d'accueil
      Result := 1; // Toujours aller à la page 1

    1: // Depuis la page de choix du type d'installation
      begin
        if rbBasicInstall.Checked then
          Result := 4 // Sauter les options avancées
        else if rbCustomInstall.Checked then
          Result := 2 // Aller aux options avancées
        else
          Result := 3; // Autre type d'installation
      end;

    // Autres cas
  end;
end;
```

## Bonnes pratiques pour des assistants professionnels

1. **Consistance visuelle** : Maintenez un design cohérent sur toutes les pages
2. **Messages d'erreur clairs** : Aidez l'utilisateur à comprendre et résoudre les problèmes
3. **Mémoire des choix** : Conservez les sélections lorsque l'utilisateur navigue entre les pages
4. **Préréglages** : Offrez des configurations prédéfinies pour les utilisateurs pressés
5. **Documentation contextuelle** : Fournissez une aide ou des infobulles pour les options complexes
6. **Validation en temps réel** : Validez les entrées dès qu'elles sont modifiées
7. **Retour visuel** : Montrez clairement où l'utilisateur se trouve dans le processus
8. **Conception flexible** : Permettez à l'utilisateur de revenir en arrière facilement

## Exercices pratiques

1. **Exercice simple** : Créez un assistant en 3 étapes pour configurer un profil utilisateur (nom, email, préférences)
2. **Exercice intermédiaire** : Développez un assistant d'installation avec validation de chemin d'installation et sélection de composants
3. **Exercice avancé** : Créez un assistant de configuration de base de données avec test de connexion et options conditionnelles

---

Les assistants sont un outil puissant pour guider vos utilisateurs à travers des processus complexes. Bien conçus, ils peuvent considérablement améliorer l'expérience utilisateur de votre application Delphi. En suivant les principes décrits dans ce chapitre, vous pourrez créer des assistants intuitifs et professionnels qui rendront vos applications plus accessibles et agréables à utiliser.

⏭️ [Navigation dans les applications mobiles](/06-applications-multi-fenetres-et-navigation/06-navigation-dans-les-applications-mobiles.md)
