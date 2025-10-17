🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.7 Gestion de l'état de l'application

## Introduction

L'état d'une application représente toutes les informations nécessaires pour décrire la situation actuelle de l'application à un moment donné. Cela inclut :

- Les données affichées à l'écran
- Les préférences utilisateur
- La position de défilement dans une liste
- L'onglet actif
- Les formulaires ouverts
- Les données en cours d'édition
- L'utilisateur connecté
- Les paramètres de configuration

Gérer correctement l'état de l'application est crucial pour :
- Offrir une expérience utilisateur fluide
- Éviter la perte de données
- Permettre la reprise après interruption
- Maintenir la cohérence des données
- Faciliter le débogage

## Comprendre les différents types d'état

### État de l'interface utilisateur (UI State)

L'état visuel de l'application : quel onglet est actif, quelle fenêtre est ouverte, etc.

```pascal
type
  TUIState = record
    ActiveTabIndex: Integer;
    ScrollPosition: Integer;
    SelectedItemIndex: Integer;
    IsMenuOpen: Boolean;
    CurrentPage: string;
  end;
```

### État des données (Data State)

Les données affichées ou manipulées par l'application.

```pascal
type
  TDataState = record
    CurrentUser: TUser;
    LoadedItems: TList<TItem>;
    FilterText: string;
    SortColumn: string;
    SortDirection: TSortDirection;
  end;
```

### État de session (Session State)

Les informations temporaires valables pour la session en cours.

```pascal
type
  TSessionState = record
    IsAuthenticated: Boolean;
    SessionToken: string;
    LoginTime: TDateTime;
    LastActivity: TDateTime;
  end;
```

### État persistant (Persistent State)

Les données qui doivent survivre à la fermeture de l'application.

```pascal
type
  TPersistentState = record
    UserPreferences: TUserPreferences;
    RecentFiles: TStringList;
    WindowPosition: TRect;
    Language: string;
  end;
```

## Gestion de l'état de l'interface utilisateur

### Sauvegarder la position et la taille des fenêtres

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SauvegarderPositionFenetre;
    procedure RestaurerPositionFenetre;
  end;

implementation

uses
  System.IniFiles, System.IOUtils;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  RestaurerPositionFenetre;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SauvegarderPositionFenetre;
end;

procedure TFormMain.SauvegarderPositionFenetre;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := TPath.Combine(TPath.GetHomePath, 'config.ini');
  IniFile := TIniFile.Create(ConfigPath);
  try
    IniFile.WriteInteger('Window', 'Left', Left);
    IniFile.WriteInteger('Window', 'Top', Top);
    IniFile.WriteInteger('Window', 'Width', Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteInteger('Window', 'WindowState', Ord(WindowState));
  finally
    IniFile.Free;
  end;
end;

procedure TFormMain.RestaurerPositionFenetre;
var
  IniFile: TIniFile;
  ConfigPath: string;
begin
  ConfigPath := TPath.Combine(TPath.GetHomePath, 'config.ini');

  if not TFile.Exists(ConfigPath) then
    Exit;

  IniFile := TIniFile.Create(ConfigPath);
  try
    Left := IniFile.ReadInteger('Window', 'Left', Left);
    Top := IniFile.ReadInteger('Window', 'Top', Top);
    Width := IniFile.ReadInteger('Window', 'Width', Width);
    Height := IniFile.ReadInteger('Window', 'Height', Height);
    WindowState := TWindowState(IniFile.ReadInteger('Window', 'WindowState', Ord(wsNormal)));
  finally
    IniFile.Free;
  end;
end;
```

### Mémoriser l'onglet actif

```pascal
procedure TFormMain.SauvegarderEtatUI;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Sauvegarder l'onglet actif
    IniFile.WriteInteger('UI', 'ActiveTab', PageControl1.ActivePageIndex);

    // Sauvegarder la position de défilement
    IniFile.WriteInteger('UI', 'ScrollPosition', ScrollBox1.VertScrollBar.Position);

    // Sauvegarder l'élément sélectionné
    IniFile.WriteInteger('UI', 'SelectedIndex', ListBox1.ItemIndex);
  finally
    IniFile.Free;
  end;
end;

procedure TFormMain.RestaurerEtatUI;
var
  IniFile: TIniFile;
  TabIndex: Integer;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Restaurer l'onglet actif
    TabIndex := IniFile.ReadInteger('UI', 'ActiveTab', 0);
    if TabIndex < PageControl1.PageCount then
      PageControl1.ActivePageIndex := TabIndex;

    // Restaurer la position de défilement
    ScrollBox1.VertScrollBar.Position :=
      IniFile.ReadInteger('UI', 'ScrollPosition', 0);

    // Restaurer l'élément sélectionné
    ListBox1.ItemIndex := IniFile.ReadInteger('UI', 'SelectedIndex', -1);
  finally
    IniFile.Free;
  end;
end;
```

### Gérer l'état des colonnes de grille

```pascal
procedure TFormMain.SauvegarderEtatGrille;
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Sauvegarder la largeur de chaque colonne
    for i := 0 to StringGrid1.ColumnCount - 1 do
    begin
      IniFile.WriteInteger('Grid', 'Column' + IntToStr(i) + 'Width',
        StringGrid1.ColWidths[i]);
    end;

    // Sauvegarder l'ordre des colonnes (si réorganisable)
    // Sauvegarder la colonne de tri
    IniFile.WriteInteger('Grid', 'SortColumn', FSortColumn);
    IniFile.WriteBool('Grid', 'SortAscending', FSortAscending);
  finally
    IniFile.Free;
  end;
end;

procedure TFormMain.RestaurerEtatGrille;
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Restaurer la largeur de chaque colonne
    for i := 0 to StringGrid1.ColumnCount - 1 do
    begin
      StringGrid1.ColWidths[i] :=
        IniFile.ReadInteger('Grid', 'Column' + IntToStr(i) + 'Width',
          StringGrid1.ColWidths[i]);
    end;

    // Restaurer la colonne de tri
    FSortColumn := IniFile.ReadInteger('Grid', 'SortColumn', 0);
    FSortAscending := IniFile.ReadBool('Grid', 'SortAscending', True);
    AppliquerTri;
  finally
    IniFile.Free;
  end;
end;
```

## Gestion des préférences utilisateur

### Créer une classe de préférences

```pascal
type
  TUserPreferences = class
  private
    FLanguage: string;
    FTheme: string;
    FAutoSave: Boolean;
    FAutoSaveInterval: Integer;
    FShowNotifications: Boolean;
    FConfigPath: string;
  public
    constructor Create;
    procedure Load;
    procedure Save;

    property Language: string read FLanguage write FLanguage;
    property Theme: string read FTheme write FTheme;
    property AutoSave: Boolean read FAutoSave write FAutoSave;
    property AutoSaveInterval: Integer read FAutoSaveInterval write FAutoSaveInterval;
    property ShowNotifications: Boolean read FShowNotifications write FShowNotifications;
  end;

implementation

constructor TUserPreferences.Create;
begin
  inherited;
  FConfigPath := TPath.Combine(TPath.GetHomePath, 'preferences.ini');

  // Valeurs par défaut
  FLanguage := 'fr';
  FTheme := 'Light';
  FAutoSave := True;
  FAutoSaveInterval := 300; // 5 minutes
  FShowNotifications := True;
end;

procedure TUserPreferences.Load;
var
  IniFile: TIniFile;
begin
  if not TFile.Exists(FConfigPath) then
    Exit;

  IniFile := TIniFile.Create(FConfigPath);
  try
    FLanguage := IniFile.ReadString('General', 'Language', FLanguage);
    FTheme := IniFile.ReadString('General', 'Theme', FTheme);
    FAutoSave := IniFile.ReadBool('General', 'AutoSave', FAutoSave);
    FAutoSaveInterval := IniFile.ReadInteger('General', 'AutoSaveInterval', FAutoSaveInterval);
    FShowNotifications := IniFile.ReadBool('General', 'ShowNotifications', FShowNotifications);
  finally
    IniFile.Free;
  end;
end;

procedure TUserPreferences.Save;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FConfigPath);
  try
    IniFile.WriteString('General', 'Language', FLanguage);
    IniFile.WriteString('General', 'Theme', FTheme);
    IniFile.WriteBool('General', 'AutoSave', FAutoSave);
    IniFile.WriteInteger('General', 'AutoSaveInterval', FAutoSaveInterval);
    IniFile.WriteBool('General', 'ShowNotifications', FShowNotifications);
  finally
    IniFile.Free;
  end;
end;
```

### Utilisation des préférences

```pascal
type
  TFormMain = class(TForm)
  private
    FPreferences: TUserPreferences;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited;
  FPreferences := TUserPreferences.Create;
  FPreferences.Load;

  // Appliquer les préférences
  AppliquerTheme(FPreferences.Theme);
  ConfigurerLangue(FPreferences.Language);

  if FPreferences.AutoSave then
    DemarrerAutoSauvegarde(FPreferences.AutoSaveInterval);
end;

destructor TFormMain.Destroy;
begin
  FPreferences.Save;
  FPreferences.Free;
  inherited;
end;
```

## Gestion de l'état des données en cours d'édition

### Détecter les modifications non sauvegardées

```pascal
type
  TFormEdit = class(TForm)
    EditNom: TEdit;
    MemoDescription: TMemo;
    ButtonSauvegarder: TButton;
    procedure EditNomChange(Sender: TObject);
    procedure MemoDescriptionChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FModifie: Boolean;
    FDonneesOriginales: TStringList;
    procedure MarquerCommeModifie;
    function DonneesModifiees: Boolean;
    procedure SauvegarderDonneesOriginales;
  end;

implementation

procedure TFormEdit.FormCreate(Sender: TObject);
begin
  FModifie := False;
  FDonneesOriginales := TStringList.Create;
  SauvegarderDonneesOriginales;
end;

procedure TFormEdit.SauvegarderDonneesOriginales;
begin
  FDonneesOriginales.Clear;
  FDonneesOriginales.Add(EditNom.Text);
  FDonneesOriginales.Add(MemoDescription.Text);
end;

procedure TFormEdit.EditNomChange(Sender: TObject);
begin
  MarquerCommeModifie;
end;

procedure TFormEdit.MemoDescriptionChange(Sender: TObject);
begin
  MarquerCommeModifie;
end;

procedure TFormEdit.MarquerCommeModifie;
begin
  if not FModifie then
  begin
    FModifie := True;
    Caption := Caption + ' *';  // Ajouter un astérisque au titre
    ButtonSauvegarder.Enabled := True;
  end;
end;

function TFormEdit.DonneesModifiees: Boolean;
begin
  Result := (EditNom.Text <> FDonneesOriginales[0]) or
            (MemoDescription.Text <> FDonneesOriginales[1]);
end;

procedure TFormEdit.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DonneesModifiees then
  begin
    case MessageDlg('Voulez-vous enregistrer les modifications ?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
      begin
        SauvegarderDonnees;
        CanClose := True;
      end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end;
end;
```

### Sauvegarde automatique

```pascal
type
  TFormEdit = class(TForm)
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDerniereSauvegarde: TDateTime;
    procedure SauvegardeAutomatique;
  end;

implementation

procedure TFormEdit.FormCreate(Sender: TObject);
begin
  // Sauvegarde automatique toutes les 5 minutes
  Timer1.Interval := 5 * 60 * 1000;  // 5 minutes en millisecondes
  Timer1.Enabled := True;
  FDerniereSauvegarde := Now;
end;

procedure TFormEdit.Timer1Timer(Sender: TObject);
begin
  if FModifie then
    SauvegardeAutomatique;
end;

procedure TFormEdit.SauvegardeAutomatique;
var
  CheminTemp: string;
begin
  try
    // Sauvegarder dans un fichier temporaire
    CheminTemp := TPath.Combine(TPath.GetTempPath, 'autosave.tmp');
    SauvegarderDans(CheminTemp);

    FDerniereSauvegarde := Now;
    FModifie := False;

    // Optionnel : afficher une notification discrète
    ShowMessage('Sauvegarde automatique effectuée');
  except
    on E: Exception do
      // Logger l'erreur sans interrompre l'utilisateur
      LogError('Erreur sauvegarde auto: ' + E.Message);
  end;
end;
```

## Gestion de l'état de session

### Authentification et session utilisateur

```pascal
type
  TSessionManager = class
  private
    class var FInstance: TSessionManager;
    FCurrentUser: TUser;
    FIsAuthenticated: Boolean;
    FSessionToken: string;
    FLoginTime: TDateTime;
    FLastActivity: TDateTime;
    FTimeout: Integer; // En minutes
  public
    class function Instance: TSessionManager;
    class destructor Destroy;

    function Login(const Username, Password: string): Boolean;
    procedure Logout;
    procedure UpdateActivity;
    function IsSessionExpired: Boolean;

    property CurrentUser: TUser read FCurrentUser;
    property IsAuthenticated: Boolean read FIsAuthenticated;
    property SessionToken: string read FSessionToken;
  end;

implementation

class function TSessionManager.Instance: TSessionManager;
begin
  if not Assigned(FInstance) then
    FInstance := TSessionManager.Create;
  Result := FInstance;
end;

class destructor TSessionManager.Destroy;
begin
  if Assigned(FInstance) then
    FInstance.Free;
end;

function TSessionManager.Login(const Username, Password: string): Boolean;
begin
  Result := False;

  // Vérifier les identifiants (exemple simplifié)
  if VerifierIdentifiants(Username, Password) then
  begin
    FIsAuthenticated := True;
    FLoginTime := Now;
    FLastActivity := Now;
    FSessionToken := GenerateToken;
    FCurrentUser := ChargerUtilisateur(Username);
    FTimeout := 30; // 30 minutes

    Result := True;
  end;
end;

procedure TSessionManager.Logout;
begin
  FIsAuthenticated := False;
  FSessionToken := '';
  FCurrentUser := nil;
end;

procedure TSessionManager.UpdateActivity;
begin
  FLastActivity := Now;
end;

function TSessionManager.IsSessionExpired: Boolean;
var
  MinutesInactives: Integer;
begin
  MinutesInactives := MinutesBetween(Now, FLastActivity);
  Result := MinutesInactives > FTimeout;
end;
```

### Utilisation du gestionnaire de session

```pascal
procedure TFormMain.FormCreate(Sender: TObject);
begin
  // Vérifier si une session existe
  if not TSessionManager.Instance.IsAuthenticated then
  begin
    // Afficher l'écran de connexion
    if not AfficherEcranConnexion then
    begin
      Application.Terminate;
      Exit;
    end;
  end;

  // Vérifier périodiquement l'expiration de la session
  TimerSession.Interval := 60000; // Toutes les minutes
  TimerSession.Enabled := True;
end;

procedure TFormMain.TimerSessionTimer(Sender: TObject);
begin
  if TSessionManager.Instance.IsSessionExpired then
  begin
    ShowMessage('Votre session a expiré. Veuillez vous reconnecter.');
    TSessionManager.Instance.Logout;
    AfficherEcranConnexion;
  end;
end;

procedure TFormMain.UneActionUtilisateur;
begin
  // Mettre à jour l'activité utilisateur
  TSessionManager.Instance.UpdateActivity;

  // Effectuer l'action...
end;
```

## État de l'application mobile

### Gérer les interruptions sur mobile

```pascal
uses
  FMX.Platform;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FAppEventService: IFMXApplicationEventService;
    function HandleAppEvent(AAppEvent: TApplicationEvent;
      AContext: TObject): Boolean;
    procedure SauvegarderEtatMobile;
    procedure RestaurerEtatMobile;
  end;

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // S'abonner aux événements d'application
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXApplicationEventService, IInterface(FAppEventService)) then
  begin
    FAppEventService.SetApplicationEventHandler(HandleAppEvent);
  end;
end;

function TFormMain.HandleAppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  Result := True;

  case AAppEvent of
    TApplicationEvent.WillBecomeInactive:
    begin
      // L'application va passer en arrière-plan
      SauvegarderEtatMobile;
    end;

    TApplicationEvent.BecameActive:
    begin
      // L'application revient au premier plan
      RestaurerEtatMobile;
      // Vérifier si des données doivent être rafraîchies
      VerifierMiseAJour;
    end;

    TApplicationEvent.WillTerminate:
    begin
      // L'application va se fermer
      SauvegarderEtatMobile;
      NettoierRessources;
    end;

    TApplicationEvent.LowMemory:
    begin
      // Mémoire faible : libérer des ressources
      LibererCachesInutiles;
    end;
  end;
end;

procedure TFormMain.SauvegarderEtatMobile;
var
  ConfigPath: string;
  IniFile: TIniFile;
begin
  ConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'state.ini');
  IniFile := TIniFile.Create(ConfigPath);
  try
    // Sauvegarder l'état de navigation
    IniFile.WriteInteger('Navigation', 'TabIndex', TabControl1.TabIndex);
    IniFile.WriteInteger('Navigation', 'ScrollPosition',
      Round(VertScrollBox1.ViewportPosition.Y));

    // Sauvegarder les données en cours d'édition
    IniFile.WriteString('Data', 'EditText', Edit1.Text);
    IniFile.WriteString('Data', 'SearchQuery', EditRecherche.Text);

    // Timestamp
    IniFile.WriteDateTime('Session', 'LastSave', Now);
  finally
    IniFile.Free;
  end;
end;

procedure TFormMain.RestaurerEtatMobile;
var
  ConfigPath: string;
  IniFile: TIniFile;
  LastSave: TDateTime;
begin
  ConfigPath := TPath.Combine(TPath.GetDocumentsPath, 'state.ini');

  if not TFile.Exists(ConfigPath) then
    Exit;

  IniFile := TIniFile.Create(ConfigPath);
  try
    // Vérifier si l'état n'est pas trop ancien
    LastSave := IniFile.ReadDateTime('Session', 'LastSave', 0);
    if DaysBetween(Now, LastSave) > 1 then
      Exit; // État trop ancien, ne pas restaurer

    // Restaurer l'état de navigation
    TabControl1.TabIndex := IniFile.ReadInteger('Navigation', 'TabIndex', 0);
    VertScrollBox1.ViewportPosition := PointF(0,
      IniFile.ReadInteger('Navigation', 'ScrollPosition', 0));

    // Restaurer les données
    Edit1.Text := IniFile.ReadString('Data', 'EditText', '');
    EditRecherche.Text := IniFile.ReadString('Data', 'SearchQuery', '');
  finally
    IniFile.Free;
  end;
end;
```

## Gestion centralisée de l'état avec un singleton

### Créer un gestionnaire d'état global

```pascal
type
  TApplicationState = class
  private
    class var FInstance: TApplicationState;
    FCurrentPage: string;
    FIsLoading: Boolean;
    FFilterText: string;
    FSortColumn: string;
    FSelectedItems: TList<Integer>;
    FOnStateChanged: TNotifyEvent;
    procedure DoStateChanged;
  public
    class function Instance: TApplicationState;
    class destructor Destroy;

    constructor Create;
    destructor Destroy; override;

    procedure SetCurrentPage(const Page: string);
    procedure SetLoading(IsLoading: Boolean);
    procedure SetFilter(const FilterText: string);
    procedure SetSort(const Column: string);

    procedure SaveState;
    procedure LoadState;
    procedure ResetState;

    property CurrentPage: string read FCurrentPage;
    property IsLoading: Boolean read FIsLoading;
    property FilterText: string read FFilterText;
    property SortColumn: string read FSortColumn;
    property SelectedItems: TList<Integer> read FSelectedItems;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

class function TApplicationState.Instance: TApplicationState;
begin
  if not Assigned(FInstance) then
    FInstance := TApplicationState.Create;
  Result := FInstance;
end;

class destructor TApplicationState.Destroy;
begin
  if Assigned(FInstance) then
  begin
    FInstance.SaveState;
    FInstance.Free;
  end;
end;

constructor TApplicationState.Create;
begin
  inherited;
  FSelectedItems := TList<Integer>.Create;
  LoadState;
end;

destructor TApplicationState.Destroy;
begin
  FSelectedItems.Free;
  inherited;
end;

procedure TApplicationState.DoStateChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TApplicationState.SetCurrentPage(const Page: string);
begin
  if FCurrentPage <> Page then
  begin
    FCurrentPage := Page;
    DoStateChanged;
  end;
end;

procedure TApplicationState.SetLoading(IsLoading: Boolean);
begin
  if FIsLoading <> IsLoading then
  begin
    FIsLoading := IsLoading;
    DoStateChanged;
  end;
end;

procedure TApplicationState.SetFilter(const FilterText: string);
begin
  if FFilterText <> FilterText then
  begin
    FFilterText := FilterText;
    DoStateChanged;
  end;
end;

procedure TApplicationState.SaveState;
var
  IniFile: TIniFile;
  i: Integer;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    IniFile.WriteString('State', 'CurrentPage', FCurrentPage);
    IniFile.WriteString('State', 'FilterText', FFilterText);
    IniFile.WriteString('State', 'SortColumn', FSortColumn);

    // Sauvegarder les éléments sélectionnés
    IniFile.WriteInteger('State', 'SelectedCount', FSelectedItems.Count);
    for i := 0 to FSelectedItems.Count - 1 do
      IniFile.WriteInteger('Selected', 'Item' + IntToStr(i), FSelectedItems[i]);
  finally
    IniFile.Free;
  end;
end;

procedure TApplicationState.LoadState;
var
  IniFile: TIniFile;
  i, Count: Integer;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    FCurrentPage := IniFile.ReadString('State', 'CurrentPage', 'Home');
    FFilterText := IniFile.ReadString('State', 'FilterText', '');
    FSortColumn := IniFile.ReadString('State', 'SortColumn', '');

    // Charger les éléments sélectionnés
    FSelectedItems.Clear;
    Count := IniFile.ReadInteger('State', 'SelectedCount', 0);
    for i := 0 to Count - 1 do
      FSelectedItems.Add(IniFile.ReadInteger('Selected', 'Item' + IntToStr(i), 0));
  finally
    IniFile.Free;
  end;
end;

procedure TApplicationState.ResetState;
begin
  FCurrentPage := 'Home';
  FIsLoading := False;
  FFilterText := '';
  FSortColumn := '';
  FSelectedItems.Clear;
  DoStateChanged;
end;
```

### Utilisation du gestionnaire d'état

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure OnStateChanged(Sender: TObject);
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  // S'abonner aux changements d'état
  TApplicationState.Instance.OnStateChanged := OnStateChanged;

  // Appliquer l'état actuel
  ApplierEtat;
end;

procedure TFormMain.OnStateChanged(Sender: TObject);
begin
  // Mettre à jour l'interface selon le nouvel état
  ApplierEtat;
end;

procedure TFormMain.ApplierEtat;
var
  State: TApplicationState;
begin
  State := TApplicationState.Instance;

  // Appliquer la page courante
  case State.CurrentPage of
    'Home': TabControl1.ActiveTab := TabHome;
    'Search': TabControl1.ActiveTab := TabSearch;
    'Profile': TabControl1.ActiveTab := TabProfile;
  end;

  // Appliquer le filtre
  EditFiltre.Text := State.FilterText;
  AppliquerFiltre(State.FilterText);

  // Afficher/masquer l'indicateur de chargement
  ActivityIndicator1.Visible := State.IsLoading;
end;

procedure TFormMain.ButtonRechercherClick(Sender: TObject);
begin
  // Modifier l'état
  TApplicationState.Instance.SetFilter(EditRecherche.Text);
  TApplicationState.Instance.SetLoading(True);

  // Lancer la recherche asynchrone
  TTask.Run(procedure
  begin
    // Rechercher...
    Sleep(1000);

    TThread.Synchronize(nil, procedure
    begin
      TApplicationState.Instance.SetLoading(False);
    end);
  end);
end;
```

## Fichiers de configuration

### JSON pour la configuration

```pascal
uses
  System.JSON, System.JSON.Serializers;

type
  TAppConfig = class
  public
    ServerURL: string;
    Timeout: Integer;
    MaxRetries: Integer;
    EnableLogging: Boolean;
    LogLevel: string;
  end;

procedure SauvegarderConfigJSON(const Config: TAppConfig);
var
  Serializer: TJsonSerializer;
  JsonString: string;
  ConfigPath: string;
begin
  ConfigPath := TPath.Combine(TPath.GetHomePath, 'config.json');

  Serializer := TJsonSerializer.Create;
  try
    JsonString := Serializer.Serialize(Config);
    TFile.WriteAllText(ConfigPath, JsonString, TEncoding.UTF8);
  finally
    Serializer.Free;
  end;
end;

function ChargerConfigJSON: TAppConfig;
var
  Serializer: TJsonSerializer;
  JsonString: string;
  ConfigPath: string;
begin
  ConfigPath := TPath.Combine(TPath.GetHomePath, 'config.json');

  if not TFile.Exists(ConfigPath) then
  begin
    // Créer une configuration par défaut
    Result := TAppConfig.Create;
    Result.ServerURL := 'https://api.example.com';
    Result.Timeout := 30;
    Result.MaxRetries := 3;
    Result.EnableLogging := True;
    Result.LogLevel := 'INFO';
    Exit;
  end;

  JsonString := TFile.ReadAllText(ConfigPath, TEncoding.UTF8);

  Serializer := TJsonSerializer.Create;
  try
    Result := Serializer.Deserialize<TAppConfig>(JsonString);
  finally
    Serializer.Free;
  end;
end;
```

### Fichiers INI structurés

```pascal
procedure SauvegarderConfigurationComplete;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Section Application
    IniFile.WriteString('Application', 'Version', GetVersionApp);
    IniFile.WriteDateTime('Application', 'InstallDate', FInstallDate);
    IniFile.WriteDateTime('Application', 'LastRun', Now);

    // Section User
    IniFile.WriteString('User', 'Name', FUserName);
    IniFile.WriteString('User', 'Email', FUserEmail);
    IniFile.WriteString('User', 'Role', FUserRole);

    // Section UI
    IniFile.WriteString('UI', 'Theme', FTheme);
    IniFile.WriteString('UI', 'Language', FLanguage);
    IniFile.WriteInteger('UI', 'FontSize', FFontSize);

    // Section Performance
    IniFile.WriteInteger('Performance', 'CacheSize', FCacheSize);
    IniFile.WriteBool('Performance', 'PreloadData', FPreloadData);

    // Section Network
    IniFile.WriteString('Network', 'ProxyServer', FProxyServer);
    IniFile.WriteInteger('Network', 'ProxyPort', FProxyPort);
    IniFile.WriteInteger('Network', 'Timeout', FTimeout);
  finally
    IniFile.Free;
  end;
end;
```

## Gestion de plusieurs profils utilisateur

```pascal
type
  TProfileManager = class
  private
    FCurrentProfile: string;
    FProfilesPath: string;
    function GetProfilePath(const ProfileName: string): string;
  public
    constructor Create;

    function LoadProfile(const ProfileName: string): Boolean;
    procedure SaveProfile;
    procedure CreateProfile(const ProfileName: string);
    procedure DeleteProfile(const ProfileName: string);
    function GetProfileList: TStringList;
    procedure SwitchProfile(const ProfileName: string);

    property CurrentProfile: string read FCurrentProfile;
  end;

implementation

constructor TProfileManager.Create;
begin
  inherited;
  FProfilesPath := TPath.Combine(TPath.GetHomePath, 'Profiles');

  if not TDirectory.Exists(FProfilesPath) then
    TDirectory.CreateDirectory(FProfilesPath);
end;

function TProfileManager.GetProfilePath(const ProfileName: string): string;
begin
  Result := TPath.Combine(FProfilesPath, ProfileName + '.ini');
end;

function TProfileManager.LoadProfile(const ProfileName: string): Boolean;
var
  ProfilePath: string;
  IniFile: TIniFile;
begin
  ProfilePath := GetProfilePath(ProfileName);
  Result := TFile.Exists(ProfilePath);

  if Result then
  begin
    IniFile := TIniFile.Create(ProfilePath);
    try
      FCurrentProfile := ProfileName;

      // Charger les données du profil
      TApplicationState.Instance.LoadState;
      LoadUserPreferences(ProfileName);
    finally
      IniFile.Free;
    end;
  end;
end;

procedure TProfileManager.CreateProfile(const ProfileName: string);
var
  ProfilePath: string;
  IniFile: TIniFile;
begin
  ProfilePath := GetProfilePath(ProfileName);

  IniFile := TIniFile.Create(ProfilePath);
  try
    IniFile.WriteDateTime('Profile', 'CreatedDate', Now);
    IniFile.WriteString('Profile', 'Name', ProfileName);
  finally
    IniFile.Free;
  end;
end;

function TProfileManager.GetProfileList: TStringList;
var
  Files: TArray<string>;
  FileName: string;
begin
  Result := TStringList.Create;

  Files := TDirectory.GetFiles(FProfilesPath, '*.ini');
  for FileName in Files do
    Result.Add(TPath.GetFileNameWithoutExtension(FileName));
end;

procedure TProfileManager.SwitchProfile(const ProfileName: string);
begin
  // Sauvegarder le profil actuel
  if FCurrentProfile <> '' then
    SaveProfile;

  // Charger le nouveau profil
  LoadProfile(ProfileName);
end;
```

## Bonnes pratiques

### 1. Séparer les types d'état

```pascal
// Différents gestionnaires pour différents types d'état
var
  UIState: TUIStateManager;
  DataState: TDataStateManager;
  SessionState: TSessionManager;
  ConfigState: TConfigManager;
```

### 2. Sauvegarder au bon moment

```pascal
// Sauvegarder :
// - À la fermeture de l'application
// - Après des modifications importantes
// - Périodiquement (auto-save)
// - Avant des opérations critiques

// Ne PAS sauvegarder :
// - À chaque frappe clavier
// - À chaque mouvement de souris
// - Trop fréquemment (impact performance)
```

### 3. Valider les données chargées

```pascal
function ChargerEtat: Boolean;
var
  Version: string;
begin
  Result := False;

  IniFile := TIniFile.Create(GetConfigPath);
  try
    // Vérifier la version
    Version := IniFile.ReadString('App', 'Version', '');
    if Version <> GetVersionActuelle then
    begin
      // Migration nécessaire
      if not MigrerConfiguration(Version) then
        Exit;
    end;

    // Valider les données
    if ValiderConfiguration then
      Result := True
    else
      UtiliserConfigurationParDefaut;
  finally
    IniFile.Free;
  end;
end;
```

### 4. Gérer les erreurs de lecture/écriture

```pascal
procedure SauvegarderEtatSecurise;
var
  TempPath, FinalPath: string;
begin
  FinalPath := GetConfigPath;
  TempPath := FinalPath + '.tmp';

  try
    // Sauvegarder dans un fichier temporaire
    SauvegarderDans(TempPath);

    // Si succès, remplacer l'ancien fichier
    if TFile.Exists(FinalPath) then
      TFile.Delete(FinalPath);
    TFile.Move(TempPath, FinalPath);
  except
    on E: Exception do
    begin
      // Logger l'erreur
      LogError('Erreur sauvegarde: ' + E.Message);

      // Nettoyer le fichier temporaire
      if TFile.Exists(TempPath) then
        TFile.Delete(TempPath);
    end;
  end;
end;
```

### 5. Nettoyer les anciennes données

```pascal
procedure NettoyerAnciensEtats;
var
  Files: TArray<string>;
  FileName: string;
  FileDate: TDateTime;
begin
  Files := TDirectory.GetFiles(TPath.GetTempPath, 'autosave_*.tmp');

  for FileName in Files do
  begin
    FileDate := TFile.GetCreationTime(FileName);

    // Supprimer les fichiers de plus de 7 jours
    if DaysBetween(Now, FileDate) > 7 then
      TFile.Delete(FileName);
  end;
end;
```

### 6. Utiliser des valeurs par défaut sensées

```pascal
procedure InitialiserValeursParDefaut;
begin
  // Toujours avoir des valeurs par défaut raisonnables
  FTheme := 'Light';
  FLanguage := 'fr';
  FAutoSave := True;
  FAutoSaveInterval := 300;
  FWindowWidth := 800;
  FWindowHeight := 600;
  FWindowState := wsNormal;
end;
```

### 7. Documenter le format des données

```pascal
{
  Format du fichier config.ini:

  [Application]
  Version=1.0.0
  InstallDate=2025-01-01

  [User]
  Name=Jean Dupont
  Email=jean@example.com

  [UI]
  Theme=Light|Dark
  Language=fr|en|de
  FontSize=8..20
}
```

## Résumé

La gestion de l'état est cruciale pour une application robuste. Les points clés à retenir :

- **Types d'état** : UI, données, session, configuration
- **Persistance** : Utiliser INI, JSON ou base de données selon les besoins
- **Sauvegarde** : À la fermeture, périodiquement, avant opérations critiques
- **Restauration** : Valider les données, gérer les erreurs, valeurs par défaut
- **Mobile** : Gérer les interruptions et le cycle de vie
- **Session** : Authentification, timeout, activité utilisateur
- **Modifications** : Détecter et demander confirmation avant perte
- **Centralisation** : Singleton pour l'état global de l'application
- **Sécurité** : Sauvegardes atomiques, validation, nettoyage
- **Performance** : Ne pas sauvegarder trop souvent

Une bonne gestion de l'état améliore l'expérience utilisateur et la fiabilité de l'application.

⏭️ [Gestion des fichiers et flux de données](/07-gestion-des-fichiers-et-flux-de-donnees/README.md)
