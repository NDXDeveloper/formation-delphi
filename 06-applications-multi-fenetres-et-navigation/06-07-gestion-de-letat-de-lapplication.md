# 6.7 Gestion de l'état de l'application

La gestion de l'état est un aspect fondamental du développement d'applications. Elle consiste à suivre et manipuler les données et configurations qui définissent comment votre application fonctionne à un moment donné. Dans ce chapitre, nous allons explorer différentes techniques pour gérer l'état d'une application Delphi, en partant des concepts de base jusqu'aux approches plus avancées.

## Qu'est-ce que l'état d'une application ?

L'état d'une application comprend toutes les données qui influencent son comportement et son apparence :

- **Données utilisateur** : informations saisies par l'utilisateur
- **Paramètres de configuration** : préférences, options
- **État de l'interface** : formulaires ouverts, onglets actifs, positions des fenêtres
- **État du processus** : étape actuelle dans un workflow, opérations en cours
- **Cache et données temporaires** : résultats de calculs, données mises en mémoire

Gérer efficacement cet état est essentiel pour créer des applications robustes et conviviales.

## Niveaux de gestion d'état

La gestion d'état peut être divisée en plusieurs niveaux :

1. **État local** : propre à un contrôle ou formulaire spécifique
2. **État au niveau formulaire** : partagé entre les contrôles d'un même formulaire
3. **État global de l'application** : accessible depuis n'importe où dans l'application
4. **État persistant** : conservé entre les sessions (fichiers, base de données, registre)

## Techniques de base pour la gestion d'état

### Variables pour l'état local

La méthode la plus simple consiste à utiliser des variables pour stocker l'état :

```pascal
// Dans un formulaire
private
  FIsEditing: Boolean;      // Mode d'édition actif ou non
  FCurrentUserID: Integer;  // ID de l'utilisateur actuel
  FLastSearchQuery: string; // Dernière recherche effectuée
```

### Propriétés pour l'état accessible

Pour rendre l'état accessible à d'autres unités, utilisez des propriétés :

```pascal
// Dans l'interface d'une classe
public
  property IsEditing: Boolean read FIsEditing write SetIsEditing;
  property CurrentUserID: Integer read FCurrentUserID write FCurrentUserID;
  property LastSearchQuery: string read FLastSearchQuery write FLastSearchQuery;

// Dans l'implémentation
procedure TMyForm.SetIsEditing(const Value: Boolean);
begin
  if FIsEditing <> Value then
  begin
    FIsEditing := Value;
    // Mettre à jour l'interface en fonction du mode d'édition
    UpdateUIForEditMode;
  end;
end;
```

### Transmission d'état entre formulaires

Lorsque vous ouvrez un nouveau formulaire, vous pouvez lui transmettre l'état actuel :

```pascal
procedure TMainForm.btnEditCustomerClick(Sender: TObject);
var
  CustomerForm: TCustomerForm;
begin
  CustomerForm := TCustomerForm.Create(Self);
  try
    // Transmettre l'état au nouveau formulaire
    CustomerForm.CustomerID := GetSelectedCustomerID;
    CustomerForm.IsNewCustomer := False;

    // Afficher le formulaire
    if CustomerForm.ShowModal = mrOk then
    begin
      // Récupérer d'éventuelles modifications de l'état
      RefreshCustomerList;
    end;
  finally
    CustomerForm.Free;
  end;
end;
```

## Gestion de l'état global de l'application

Pour un état qui doit être accessible depuis n'importe quelle partie de l'application, plusieurs approches sont possibles.

### 1. Module de données (DataModule)

Un module de données est parfait pour centraliser les données partagées :

```pascal
// Dans une unité DataModule.pas
unit DataModule;

interface

uses
  System.SysUtils, System.Classes, Data.DB, FireDAC.Comp.Client;

type
  TDM = class(TDataModule)
    // Composants de base de données, etc.
  private
    FLoggedUser: string;
    FAppSettings: TAppSettings;
  public
    property LoggedUser: string read FLoggedUser write FLoggedUser;
    property AppSettings: TAppSettings read FAppSettings write FAppSettings;

    // Méthodes de gestion d'état
    procedure LoadSettings;
    procedure SaveSettings;
    function IsUserLoggedIn: Boolean;
  end;

var
  DM: TDM;  // Variable globale accessible depuis toute l'application

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDM.LoadSettings;
begin
  // Charger les paramètres depuis un fichier ou la base de données
end;

procedure TDM.SaveSettings;
begin
  // Sauvegarder les paramètres
end;

function TDM.IsUserLoggedIn: Boolean;
begin
  Result := FLoggedUser <> '';
end;

end.
```

Pour utiliser ce module de données depuis n'importe quelle unité :

```pascal
uses DataModule;

procedure TMyForm.UpdateUserInfo;
begin
  if DM.IsUserLoggedIn then
    lblUserName.Text := 'Utilisateur : ' + DM.LoggedUser
  else
    lblUserName.Text := 'Non connecté';
end;
```

### 2. Classe singleton pour l'état global

Une approche plus structurée consiste à créer une classe singleton dédiée à la gestion de l'état :

```pascal
unit AppState;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TThemeType = (ttLight, ttDark, ttSystem);

  TAppStateManager = class
  private
    class var FInstance: TAppStateManager;

    FUsername: string;
    FIsLoggedIn: Boolean;
    FTheme: TThemeType;
    FRecentFiles: TList<string>;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TAppStateManager;
    class procedure ReleaseInstance;

    // Propriétés d'état
    property Username: string read FUsername write FUsername;
    property IsLoggedIn: Boolean read FIsLoggedIn write FIsLoggedIn;
    property Theme: TThemeType read FTheme write FTheme;

    // Méthodes de gestion des fichiers récents
    procedure AddRecentFile(const FileName: string);
    procedure ClearRecentFiles;
    function GetRecentFiles: TArray<string>;

    // Méthodes de persistance
    procedure LoadState;
    procedure SaveState;
  end;

// Fonction globale pour accéder facilement à l'état
function AppState: TAppStateManager;

implementation

constructor TAppStateManager.Create;
begin
  inherited Create;
  FRecentFiles := TList<string>.Create;
  // Initialiser avec des valeurs par défaut
  FTheme := ttLight;
  FIsLoggedIn := False;
end;

destructor TAppStateManager.Destroy;
begin
  FRecentFiles.Free;
  inherited;
end;

class function TAppStateManager.GetInstance: TAppStateManager;
begin
  if FInstance = nil then
    FInstance := TAppStateManager.Create;
  Result := FInstance;
end;

class procedure TAppStateManager.ReleaseInstance;
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

// ... autres méthodes implémentées ...

// Fonction globale pour accéder facilement à l'état
function AppState: TAppStateManager;
begin
  Result := TAppStateManager.GetInstance;
end;

// Libération du singleton à la fin de l'application
initialization

finalization
  TAppStateManager.ReleaseInstance;

end.
```

Utilisation dans votre code :

```pascal
uses AppState;

procedure TMainForm.ApplyTheme;
begin
  case AppState.Theme of
    ttLight:
      SetLightTheme;
    ttDark:
      SetDarkTheme;
    ttSystem:
      ApplySystemTheme;
  end;
end;

procedure TMainForm.btnLoginClick(Sender: TObject);
begin
  // Après une connexion réussie
  AppState.Username := edtUsername.Text;
  AppState.IsLoggedIn := True;
  UpdateUI;
end;
```

## Persistance de l'état entre les sessions

Pour conserver l'état entre les sessions, plusieurs options sont disponibles.

### 1. Fichier INI

Les fichiers INI sont simples et efficaces pour stocker des paramètres :

```pascal
procedure TAppStateManager.SaveState;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(GetSettingsFileName);
  try
    // Sauvegarder les paramètres de base
    IniFile.WriteString('User', 'Username', FUsername);
    IniFile.WriteBool('User', 'IsLoggedIn', FIsLoggedIn);
    IniFile.WriteInteger('UI', 'Theme', Ord(FTheme));

    // Sauvegarder la liste des fichiers récents
    IniFile.EraseSection('RecentFiles');
    for var I := 0 to FRecentFiles.Count - 1 do
      IniFile.WriteString('RecentFiles', 'File' + I.ToString, FRecentFiles[I]);
  finally
    IniFile.Free;
  end;
end;

procedure TAppStateManager.LoadState;
var
  IniFile: TIniFile;
  I: Integer;
  FileName: string;
begin
  if not FileExists(GetSettingsFileName) then
    Exit;

  IniFile := TIniFile.Create(GetSettingsFileName);
  try
    // Charger les paramètres de base
    FUsername := IniFile.ReadString('User', 'Username', '');
    FIsLoggedIn := IniFile.ReadBool('User', 'IsLoggedIn', False);
    FTheme := TThemeType(IniFile.ReadInteger('UI', 'Theme', 0));

    // Charger la liste des fichiers récents
    FRecentFiles.Clear;
    I := 0;
    while True do
    begin
      FileName := IniFile.ReadString('RecentFiles', 'File' + I.ToString, '');
      if FileName = '' then
        Break;
      FRecentFiles.Add(FileName);
      Inc(I);
    end;
  finally
    IniFile.Free;
  end;
end;

function TAppStateManager.GetSettingsFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(GetAppDataPath) + 'settings.ini';
end;

function TAppStateManager.GetAppDataPath: string;
begin
  Result := IncludeTrailingPathDelimiter(
    GetEnvironmentVariable('APPDATA')) + 'MyApplication';
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;
```

### 2. Registre Windows

Le registre est une autre option, surtout pour les applications Windows :

```pascal
procedure TAppStateManager.SaveState;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApplication', True) then
    begin
      Registry.WriteString('Username', FUsername);
      Registry.WriteBool('IsLoggedIn', FIsLoggedIn);
      Registry.WriteInteger('Theme', Ord(FTheme));

      // Sauvegarder les fichiers récents
      Registry.DeleteKey('RecentFiles');
      if Registry.OpenKey('RecentFiles', True) then
      begin
        for var I := 0 to FRecentFiles.Count - 1 do
          Registry.WriteString('File' + I.ToString, FRecentFiles[I]);
      end;
    end;
  finally
    Registry.Free;
  end;
end;

procedure TAppStateManager.LoadState;
var
  Registry: TRegistry;
  I: Integer;
  FileName: string;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApplication', False) then
    begin
      if Registry.ValueExists('Username') then
        FUsername := Registry.ReadString('Username');
      if Registry.ValueExists('IsLoggedIn') then
        FIsLoggedIn := Registry.ReadBool('IsLoggedIn');
      if Registry.ValueExists('Theme') then
        FTheme := TThemeType(Registry.ReadInteger('Theme'));

      // Charger les fichiers récents
      FRecentFiles.Clear;
      if Registry.OpenKey('RecentFiles', False) then
      begin
        I := 0;
        while Registry.ValueExists('File' + I.ToString) do
        begin
          FileName := Registry.ReadString('File' + I.ToString);
          FRecentFiles.Add(FileName);
          Inc(I);
        end;
      end;
    end;
  finally
    Registry.Free;
  end;
end;
```

### 3. Fichiers JSON

Le format JSON est plus flexible et lisible :

```pascal
procedure TAppStateManager.SaveState;
var
  JsonObj: TJSONObject;
  RecentFilesArray: TJSONArray;
  JsonString: string;
begin
  JsonObj := TJSONObject.Create;
  try
    // Paramètres de base
    JsonObj.AddPair('Username', FUsername);
    JsonObj.AddPair('IsLoggedIn', TJSONBool.Create(FIsLoggedIn));
    JsonObj.AddPair('Theme', TJSONNumber.Create(Ord(FTheme)));

    // Fichiers récents
    RecentFilesArray := TJSONArray.Create;
    for var FileName in FRecentFiles do
      RecentFilesArray.Add(FileName);
    JsonObj.AddPair('RecentFiles', RecentFilesArray);

    // Sérialiser en JSON
    JsonString := JsonObj.ToString;

    // Écrire dans un fichier
    TFile.WriteAllText(GetSettingsFileName, JsonString);
  finally
    JsonObj.Free;
  end;
end;

procedure TAppStateManager.LoadState;
var
  JsonString: string;
  JsonObj: TJSONObject;
  RecentFilesArray: TJSONArray;
  I: Integer;
begin
  if not TFile.Exists(GetSettingsFileName) then
    Exit;

  JsonString := TFile.ReadAllText(GetSettingsFileName);

  try
    JsonObj := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
    if JsonObj <> nil then
    try
      // Paramètres de base
      if JsonObj.TryGetValue<string>('Username', FUsername) = False then
        FUsername := '';

      if JsonObj.TryGetValue<Boolean>('IsLoggedIn', FIsLoggedIn) = False then
        FIsLoggedIn := False;

      var ThemeValue: Integer;
      if JsonObj.TryGetValue<Integer>('Theme', ThemeValue) then
        FTheme := TThemeType(ThemeValue)
      else
        FTheme := ttLight;

      // Fichiers récents
      FRecentFiles.Clear;
      if JsonObj.TryGetValue<TJSONArray>('RecentFiles', RecentFilesArray) and
         (RecentFilesArray <> nil) then
      begin
        for I := 0 to RecentFilesArray.Count - 1 do
          FRecentFiles.Add(RecentFilesArray.Items[I].Value);
      end;
    finally
      JsonObj.Free;
    end;
  except
    // Gérer les erreurs de parsing JSON
    on E: Exception do
      OutputDebugString(PChar('Erreur de chargement des paramètres : ' + E.Message));
  end;
end;
```

## Gestion d'état pour les applications multi-formulaires

Dans les applications avec plusieurs formulaires qui interagissent, il est important de maintenir une cohérence de l'état.

### Notification des changements d'état

Utilisez un système d'événements pour notifier les formulaires des changements d'état :

```pascal
unit AppState;

interface

// ... imports ...

type
  // Type d'événement pour les notifications de changement d'état
  TStateChangeEvent = procedure(Sender: TObject; const StateName: string) of object;

  TAppStateManager = class
  private
    // ... autres champs ...
    FOnStateChange: TStateChangeEvent;

    procedure NotifyStateChange(const StateName: string);
  public
    // ... autres propriétés et méthodes ...

    property OnStateChange: TStateChangeEvent read FOnStateChange write FOnStateChange;
  end;

implementation

procedure TAppStateManager.NotifyStateChange(const StateName: string);
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, StateName);
end;

// Dans les setters des propriétés
procedure TAppStateManager.SetTheme(const Value: TThemeType);
begin
  if FTheme <> Value then
  begin
    FTheme := Value;
    NotifyStateChange('Theme');
  end;
end;
```

Abonnement aux changements dans les formulaires :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // S'abonner aux notifications de changement d'état
  AppState.OnStateChange := HandleStateChange;

  // Initialiser l'interface avec l'état actuel
  ApplyTheme;
  UpdateUserInterface;
end;

procedure TMainForm.HandleStateChange(Sender: TObject; const StateName: string);
begin
  if StateName = 'Theme' then
    ApplyTheme
  else if StateName = 'IsLoggedIn' or StateName = 'Username' then
    UpdateUserInterface;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Se désabonner des notifications
  if AppState <> nil then
    AppState.OnStateChange := nil;
end;
```

### Utilisation d'un pattern Observer plus avancé

Pour les applications complexes, un mécanisme de notification plus élaboré peut être utile :

```pascal
unit StateObserver;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TStateChangedEvent = procedure(const StateKey: string; const StateValue: TValue) of object;

  IStateObserver = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure StateChanged(const StateKey: string; const StateValue: TValue);
  end;

  TStateManager = class
  private
    class var FInstance: TStateManager;

    FObservers: TList<IStateObserver>;
    FStateValues: TDictionary<string, TValue>;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TStateManager;
    class procedure ReleaseInstance;

    procedure RegisterObserver(Observer: IStateObserver);
    procedure UnregisterObserver(Observer: IStateObserver);

    procedure SetState(const Key: string; const Value: TValue);
    function GetState<T>(const Key: string; const DefaultValue: T): T;

    procedure Clear;
  end;

function StateManager: TStateManager;

implementation

// ... implémentation ...

end.
```

Utilisation dans un formulaire :

```pascal
unit MainForm;

interface

uses
  // ... autres uses ...
  StateObserver;

type
  TMainForm = class(TForm, IStateObserver)
    // ... composants ...
  private
    // Implémentation de l'interface IStateObserver
    procedure StateChanged(const StateKey: string; const StateValue: TValue);
  public
    // ...
  end;

implementation

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // S'enregistrer comme observateur
  StateManager.RegisterObserver(Self);

  // Initialiser l'interface avec l'état actuel
  UpdateTheme;
  UpdateUserInterface;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Se désinscrire
  StateManager.UnregisterObserver(Self);
end;

procedure TMainForm.StateChanged(const StateKey: string; const StateValue: TValue);
begin
  if StateKey = 'Theme' then
    UpdateTheme
  else if StateKey = 'LoggedInUser' then
    UpdateUserInterface;
end;

procedure TMainForm.btnChangeThemeClick(Sender: TObject);
var
  NewTheme: string;
begin
  if rbLightTheme.IsChecked then
    NewTheme := 'Light'
  else if rbDarkTheme.IsChecked then
    NewTheme := 'Dark'
  else
    NewTheme := 'System';

  // Mettre à jour l'état global
  StateManager.SetState('Theme', NewTheme);
end;
```

## Gestion des états transitoires

Certains états ne doivent exister que pendant la durée d'une opération. Par exemple, suivre l'état d'une opération asynchrone :

```pascal
procedure TMainForm.PerformLongOperation;
begin
  // Mettre à jour l'état pour indiquer qu'une opération est en cours
  StateManager.SetState('IsProcessing', True);

  // Mettre à jour l'interface pour refléter l'état de traitement
  UpdateProcessingUI;

  // Lancer une opération asynchrone
  TTask.Run(
    procedure
    begin
      try
        // Effectuer l'opération de longue durée
        // ...

        // Mettre à jour l'état dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            // Indique que l'opération est terminée
            StateManager.SetState('IsProcessing', False);

            // Mettre à jour d'autres états si nécessaire
            StateManager.SetState('LastOperationResult', 'Success');
          end);
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              StateManager.SetState('IsProcessing', False);
              StateManager.SetState('LastOperationResult', 'Error: ' + E.Message);
            end);
      end;
    end);
end;

procedure TMainForm.UpdateProcessingUI;
begin
  // Utiliser l'état actuel pour mettre à jour l'interface
  var IsProcessing := StateManager.GetState<Boolean>('IsProcessing', False);

  // Activer/désactiver les contrôles en fonction de l'état
  btnStart.Enabled := not IsProcessing;
  btnCancel.Enabled := IsProcessing;
  progressBar.Visible := IsProcessing;

  // Afficher le résultat de la dernière opération si disponible
  var LastResult := StateManager.GetState<string>('LastOperationResult', '');
  if LastResult <> '' then
    lblStatus.Text := LastResult;
end;
```

## Exemple complet : Application de gestion de tâches

Voici un exemple plus complet d'une application de gestion de tâches qui utilise différentes techniques de gestion d'état :

```pascal
unit TaskApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, System.JSON, System.Generics.Collections;

type
  TTaskPriority = (tpLow, tpNormal, tpHigh);

  TTaskItem = class
  private
    FId: Integer;
    FTitle: string;
    FDescription: string;
    FCompleted: Boolean;
    FPriority: TTaskPriority;
    FDueDate: TDateTime;
  public
    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
    property Priority: TTaskPriority read FPriority write FPriority;
    property DueDate: TDateTime read FDueDate write FDueDate;

    constructor Create(AId: Integer; const ATitle, ADescription: string;
                      APriority: TTaskPriority; ADueDate: TDateTime);
  end;

  TTaskManager = class
  private
    class var FInstance: TTaskManager;

    FTasks: TList<TTaskItem>;
    FCurrentFilter: string;
    FShowCompleted: Boolean;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TTaskManager;
    class procedure ReleaseInstance;

    function AddTask(const Title, Description: string;
                     Priority: TTaskPriority; DueDate: TDateTime): TTaskItem;
    procedure UpdateTask(Task: TTaskItem);
    procedure DeleteTask(TaskId: Integer);
    procedure ToggleTaskCompleted(TaskId: Integer);

    function GetFilteredTasks: TArray<TTaskItem>;

    property CurrentFilter: string read FCurrentFilter write FCurrentFilter;
    property ShowCompleted: Boolean read FShowCompleted write FShowCompleted;

    procedure LoadTasks;
    procedure SaveTasks;
  end;

  TTaskForm = class(TForm)
    ToolBar1: TToolBar;
    btnAdd: TButton;
    ListView1: TListView;
    SearchBox1: TSearchBox;
    Layout1: TLayout;
    SwitchCompleted: TSwitch;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure SearchBox1Change(Sender: TObject);
    procedure SwitchCompletedSwitch(Sender: TObject);
  private
    procedure RefreshTaskList;
  public
    { Public declarations }
  end;

var
  TaskForm: TTaskForm;

function TaskManager: TTaskManager;

implementation

{$R *.fmx}

uses
  TaskDetailForm;

function TaskManager: TTaskManager;
begin
  Result := TTaskManager.GetInstance;
end;

// ... implémentation des classes ...

procedure TTaskForm.FormCreate(Sender: TObject);
begin
  // Charger les tâches depuis le stockage persistant
  TaskManager.LoadTasks;

  // Initialiser l'interface
  SwitchCompleted.IsChecked := TaskManager.ShowCompleted;

  // Afficher les tâches
  RefreshTaskList;
end;

procedure TTaskForm.FormDestroy(Sender: TObject);
begin
  // Sauvegarder les tâches
  TaskManager.SaveTasks;
end;

procedure TTaskForm.RefreshTaskList;
var
  Tasks: TArray<TTaskItem>;
  ListItem: TListViewItem;
  Task: TTaskItem;
begin
  // Obtenir les tâches filtrées selon l'état actuel
  Tasks := TaskManager.GetFilteredTasks;

  // Vider la liste
  ListView1.Items.Clear;

  // Remplir la liste avec les tâches
  for Task in Tasks do
  begin
    ListItem := ListView1.Items.Add;
    ListItem.Text := Task.Title;
    ListItem.Detail := Task.Description;

    // Stocker l'ID de la tâche dans le Tag
    ListItem.Tag := Task.Id;

    // Ajouter un accessoire selon la priorité
    case Task.Priority of
      tpLow:
        ListItem.IconView.ImageIndex := 0;
      tpNormal:
        ListItem.IconView.ImageIndex := 1;
      tpHigh:
        ListItem.IconView.ImageIndex := 2;
    end;

    // Marquer les tâches complétées
    if Task.Completed then
      ListItem.TextStrikeThrough := TStrikeThrough.Single
    else
      ListItem.TextStrikeThrough := TStrikeThrough.None;
  end;
end;

procedure TTaskForm.SearchBox1Change(Sender: TObject);
begin
  // Mettre à jour le filtre et rafraîchir la liste
  TaskManager.CurrentFilter := SearchBox1.Text;
  RefreshTaskList;
end;

procedure TTaskForm.SwitchCompletedSwitch(Sender: TObject);
begin
  // Mettre à jour l'état d'affichage des tâches complétées
  TaskManager.ShowCompleted := SwitchCompleted.IsChecked;
  RefreshTaskList;
end;

procedure TTaskForm.btnAddClick(Sender: TObject);
var
  DetailForm: TTaskDetailForm;
begin
  DetailForm := TTaskDetailForm.Create(nil);
  try
    // Créer une nouvelle tâche
    if DetailForm.ShowModal = mrOk then
    begin
      // La tâche a été ajoutée dans le gestionnaire
      RefreshTaskList;
    end;
  finally
    DetailForm.Free;
  end;
end;

procedure TTaskForm.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  DetailForm: TTaskDetailForm;
  TaskId: Integer;
begin
  TaskId := AItem.Tag;

  DetailForm := TTaskDetailForm.Create(nil);
  try
    // Éditer une tâche existante
    DetailForm.LoadTask(TaskId);

    if DetailForm.ShowModal = mrOk then
    begin
      // La tâche a été mise à jour dans le gestionnaire
      RefreshTaskList;
    end;
  finally
    DetailForm.Free;
  end;
end;
```

## Bonnes pratiques pour la gestion d'état

1. **Séparez l'état de la logique** : Utilisez des classes dédiées pour gérer l'état
2. **Centralisez l'état global** : Évitez de disperser l'état à travers différentes unités
3. **Documentez l'état** : Décrivez clairement le rôle et l'impact de chaque élément d'état
4. **Validez les changements d'état** : Assurez-vous que les transitions d'état sont cohérentes
5. **Minimisez l'état global** : N'utilisez l'état global que lorsque c'est nécessaire
6. **Soyez cohérent** : Utilisez des conventions de nommage et des méthodes d'accès cohérentes
7. **Sauvegardez régulièrement** : Ne perdez pas l'état important en cas de plantage
8. **Utilisez des transactions** : Pour les changements d'état qui nécessitent plusieurs étapes
9. **Gérez les erreurs** : Prévoyez un comportement de repli en cas d'échec de chargement de l'état
10. **Testez les scénarios d'état** : Vérifiez que votre application se comporte correctement avec différents états

## Gestion d'état avancée avec le pattern MVVM

Le pattern MVVM (Model-View-ViewModel) est particulièrement utile pour gérer l'état de manière structurée, surtout dans les applications complexes.

### Structure MVVM pour la gestion d'état

```pascal
// Modèle : représente les données et les règles métier
unit TaskModel;

interface

type
  TTaskPriority = (tpLow, tpNormal, tpHigh);

  TTask = class
  private
    FId: Integer;
    FTitle: string;
    FDescription: string;
    FCompleted: Boolean;
    FPriority: TTaskPriority;
    FDueDate: TDateTime;
  public
    constructor Create(AId: Integer; const ATitle, ADescription: string;
                      APriority: TTaskPriority; ADueDate: TDateTime);

    property Id: Integer read FId write FId;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
    property Priority: TTaskPriority read FPriority write FPriority;
    property DueDate: TDateTime read FDueDate write FDueDate;
  end;

implementation

constructor TTask.Create(AId: Integer; const ATitle, ADescription: string;
                       APriority: TTaskPriority; ADueDate: TDateTime);
begin
  inherited Create;
  FId := AId;
  FTitle := ATitle;
  FDescription := ADescription;
  FPriority := APriority;
  FDueDate := ADueDate;
  FCompleted := False;
end;

end.

// ViewModel : gère l'état et la logique d'interaction
unit TaskViewModel;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, TaskModel;

type
  TTaskListChangedEvent = procedure(Sender: TObject) of object;

  TTaskViewModel = class
  private
    FTasks: TList<TTask>;
    FCurrentFilter: string;
    FShowCompleted: Boolean;
    FOnTaskListChanged: TTaskListChangedEvent;

    procedure SetCurrentFilter(const Value: string);
    procedure SetShowCompleted(Value: Boolean);
    procedure NotifyTaskListChanged;
  public
    constructor Create;
    destructor Destroy; override;

    function AddTask(const Title, Description: string;
                    Priority: TTaskPriority; DueDate: TDateTime): TTask;
    procedure UpdateTask(Task: TTask);
    procedure DeleteTask(TaskId: Integer);
    procedure ToggleTaskCompleted(TaskId: Integer);

    function GetFilteredTasks: TArray<TTask>;
    function GetTaskById(TaskId: Integer): TTask;

    property CurrentFilter: string read FCurrentFilter write SetCurrentFilter;
    property ShowCompleted: Boolean read FShowCompleted write SetShowCompleted;
    property OnTaskListChanged: TTaskListChangedEvent read FOnTaskListChanged
                                                     write FOnTaskListChanged;

    procedure LoadTasks;
    procedure SaveTasks;
  end;

implementation

// ... implémentation des méthodes ...

procedure TTaskViewModel.SetCurrentFilter(const Value: string);
begin
  if FCurrentFilter <> Value then
  begin
    FCurrentFilter := Value;
    NotifyTaskListChanged;
  end;
end;

procedure TTaskViewModel.SetShowCompleted(Value: Boolean);
begin
  if FShowCompleted <> Value then
  begin
    FShowCompleted := Value;
    NotifyTaskListChanged;
  end;
end;

procedure TTaskViewModel.NotifyTaskListChanged;
begin
  if Assigned(FOnTaskListChanged) then
    FOnTaskListChanged(Self);
end;

end.

// Vue : formulaire d'interface utilisateur
unit TaskView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, TaskViewModel, TaskModel;

type
  TTaskForm = class(TForm)
    // ... composants d'interface utilisateur ...

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure SearchBox1Change(Sender: TObject);
    procedure SwitchCompletedSwitch(Sender: TObject);
  private
    FViewModel: TTaskViewModel;
    procedure RefreshTaskList;
    procedure HandleTaskListChanged(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

procedure TTaskForm.FormCreate(Sender: TObject);
begin
  // Créer et initialiser le ViewModel
  FViewModel := TTaskViewModel.Create;
  FViewModel.OnTaskListChanged := HandleTaskListChanged;

  // Charger les tâches
  FViewModel.LoadTasks;

  // Initialiser l'interface
  SwitchCompleted.IsChecked := FViewModel.ShowCompleted;

  // Afficher les tâches
  RefreshTaskList;
end;

procedure TTaskForm.FormDestroy(Sender: TObject);
begin
  // Sauvegarder les tâches
  FViewModel.SaveTasks;

  // Libérer le ViewModel
  FViewModel.Free;
end;

procedure TTaskForm.HandleTaskListChanged(Sender: TObject);
begin
  // Rafraîchir la liste quand l'état change
  RefreshTaskList;
end;

// ... autres méthodes ...

end.
```

Ce pattern MVVM offre plusieurs avantages pour la gestion de l'état :
- **Séparation des préoccupations** : L'état est géré dans le ViewModel, séparé de l'interface
- **Réutilisabilité** : Le même ViewModel peut être utilisé par différentes vues
- **Testabilité** : Le ViewModel peut être testé indépendamment de l'interface utilisateur

## Gestion d'état pour différents types d'applications

### Applications à document unique (SDI)

Dans les applications à document unique, l'état est généralement centré autour du document actuel :

```pascal
// Gestionnaire d'état pour une application de traitement de texte
unit DocumentState;

interface

type
  TDocumentState = class
  private
    FFilePath: string;
    FIsModified: Boolean;
    FContent: string;
    FCurrentFont: TFont;
    FCursorPosition: TPoint;
    FSelectionStart: Integer;
    FSelectionLength: Integer;

    procedure SetIsModified(Value: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    property FilePath: string read FFilePath write FFilePath;
    property IsModified: Boolean read FIsModified write SetIsModified;
    property Content: string read FContent write FContent;
    property CurrentFont: TFont read FCurrentFont;
    property CursorPosition: TPoint read FCursorPosition write FCursorPosition;
    property SelectionStart: Integer read FSelectionStart write FSelectionStart;
    property SelectionLength: Integer read FSelectionLength write FSelectionLength;

    procedure NewDocument;
    function LoadFromFile(const FileName: string): Boolean;
    function SaveToFile(const FileName: string = ''): Boolean;
  end;

implementation

// ... implémentation ...

procedure TDocumentState.SetIsModified(Value: Boolean);
begin
  if FIsModified <> Value then
  begin
    FIsModified = Value;

    // Mettre à jour le titre de la fenêtre pour indiquer l'état modifié
    if Assigned(Application.MainForm) then
    begin
      if FIsModified then
        Application.MainForm.Caption := ExtractFileName(FFilePath) + ' *'
      else
        Application.MainForm.Caption := ExtractFileName(FFilePath);
    end;
  end;
end;

end.
```

### Applications à documents multiples (MDI)

Pour les applications gérant plusieurs documents, il faut un état global et un état par document :

```pascal
unit MDIAppState;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TDocumentInfo = class
  private
    FId: Integer;
    FFilePath: string;
    FIsModified: Boolean;
    FLastAccessed: TDateTime;
  public
    property Id: Integer read FId write FId;
    property FilePath: string read FFilePath write FFilePath;
    property IsModified: Boolean read FIsModified write FIsModified;
    property LastAccessed: TDateTime read FLastAccessed write FLastAccessed;
  end;

  TMDIAppState = class
  private
    class var FInstance: TMDIAppState;

    FOpenDocuments: TList<TDocumentInfo>;
    FActiveDocumentId: Integer;
    FRecentFiles: TList<string>;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TMDIAppState;

    function AddDocument(const FilePath: string): Integer;
    procedure RemoveDocument(DocumentId: Integer);
    procedure SetActiveDocument(DocumentId: Integer);
    function GetActiveDocument: TDocumentInfo;

    procedure AddRecentFile(const FilePath: string);
    function GetRecentFiles: TArray<string>;

    procedure SaveState;
    procedure LoadState;
  end;

function AppState: TMDIAppState;

implementation

// ... implémentation ...

end.
```

### Applications mobiles

Les applications mobiles ont des besoins spécifiques pour la gestion d'état, notamment pour s'adapter au cycle de vie de l'application :

```pascal
unit MobileAppState;

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TMobileAppState = class
  private
    class var FInstance: TMobileAppState;

    FIsFirstLaunch: Boolean;
    FLastActivePage: string;
    FUserSettings: TJSONObject;
    FTemporaryCache: TDictionary<string, TValue>;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TMobileAppState;

    // Gestion du cycle de vie
    procedure HandleAppBecomingActive;
    procedure HandleAppEnteringBackground;
    procedure HandleLowMemoryWarning;

    // Accès aux paramètres
    function GetSetting<T>(const Key: string; DefaultValue: T): T;
    procedure SetSetting<T>(const Key: string; Value: T);

    // Accès au cache temporaire
    function GetCacheItem<T>(const Key: string; DefaultValue: T): T;
    procedure SetCacheItem<T>(const Key: string; Value: T);
    procedure ClearCache;

    property IsFirstLaunch: Boolean read FIsFirstLaunch;
    property LastActivePage: string read FLastActivePage write FLastActivePage;

    // Persistance
    procedure SaveState;
    procedure RestoreState;
  end;

function MobileState: TMobileAppState;

implementation

// ... implémentation ...

procedure TMobileAppState.HandleAppEnteringBackground;
begin
  // Sauvegarder immédiatement l'état quand l'application passe en arrière-plan
  SaveState;
end;

procedure TMobileAppState.HandleLowMemoryWarning;
begin
  // Libérer des ressources non essentielles
  ClearCache;
end;

end.
```

## Gestion des préférences utilisateur

Les préférences utilisateur constituent un type d'état spécial qui doit être persistant et accessible à tous les niveaux de l'application.

```pascal
unit UserPreferences;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.JSON;

type
  TThemeMode = (tmLight, tmDark, tmSystem);
  TLanguage = (lnEnglish, lnFrench, lnGerman, lnSpanish, lnItalian);

  TUserPreferences = class
  private
    class var FInstance: TUserPreferences;

    FThemeMode: TThemeMode;
    FLanguage: TLanguage;
    FAutoSaveInterval: Integer;
    FShowStatusBar: Boolean;
    FShowToolbar: Boolean;
    FCustomSettings: TJSONObject;

    FPreferencesChanged: Boolean;
    FChangeCallbacks: TList<TProc>;

    constructor Create;
    destructor Destroy; override;

    procedure SetThemeMode(Value: TThemeMode);
    procedure SetLanguage(Value: TLanguage);
    procedure SetAutoSaveInterval(Value: Integer);
    procedure SetShowStatusBar(Value: Boolean);
    procedure SetShowToolbar(Value: Boolean);

    procedure NotifyChanges;
  public
    class function GetInstance: TUserPreferences;

    // Propriétés de base
    property ThemeMode: TThemeMode read FThemeMode write SetThemeMode;
    property Language: TLanguage read FLanguage write SetLanguage;
    property AutoSaveInterval: Integer read FAutoSaveInterval write SetAutoSaveInterval;
    property ShowStatusBar: Boolean read FShowStatusBar write SetShowStatusBar;
    property ShowToolbar: Boolean read FShowToolbar write SetShowToolbar;

    // Paramètres personnalisés
    function GetCustomSetting<T>(const Key: string; DefaultValue: T): T;
    procedure SetCustomSetting<T>(const Key: string; Value: T);

    // Gestion des callbacks
    procedure RegisterChangeCallback(Callback: TProc);
    procedure UnregisterChangeCallback(Callback: TProc);

    // Persistance
    procedure Load;
    procedure Save;
    procedure ResetToDefaults;
  end;

function Preferences: TUserPreferences;

implementation

// ... implémentation ...

procedure TUserPreferences.NotifyChanges;
var
  Callback: TProc;
begin
  for Callback in FChangeCallbacks do
    Callback();
end;

procedure TUserPreferences.SetThemeMode(Value: TThemeMode);
begin
  if FThemeMode <> Value then
  begin
    FThemeMode := Value;
    FPreferencesChanged := True;
    NotifyChanges;
  end;
end;

// ... autres méthodes ...

end.
```

Utilisation dans un formulaire :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Charger les préférences
  Preferences.Load;

  // S'abonner aux changements
  Preferences.RegisterChangeCallback(ApplyPreferences);

  // Appliquer les préférences initiales
  ApplyPreferences;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Se désabonner des changements
  Preferences.UnregisterChangeCallback(ApplyPreferences);

  // Sauvegarder les préférences si nécessaire
  Preferences.Save;
end;

procedure TMainForm.ApplyPreferences;
begin
  // Appliquer le thème
  case Preferences.ThemeMode of
    tmLight: SetLightTheme;
    tmDark: SetDarkTheme;
    tmSystem: ApplySystemTheme;
  end;

  // Appliquer la langue
  ApplyLanguage(Preferences.Language);

  // Configurer l'auto-sauvegarde
  if Preferences.AutoSaveInterval > 0 then
  begin
    AutoSaveTimer.Interval := Preferences.AutoSaveInterval * 1000; // en millisecondes
    AutoSaveTimer.Enabled := True;
  end
  else
    AutoSaveTimer.Enabled := False;

  // Configurer l'interface
  StatusBar1.Visible := Preferences.ShowStatusBar;
  ToolBar1.Visible := Preferences.ShowToolbar;
end;

procedure TMainForm.btnSettingsClick(Sender: TObject);
var
  SettingsForm: TSettingsForm;
begin
  SettingsForm := TSettingsForm.Create(Self);
  try
    if SettingsForm.ShowModal = mrOk then
    begin
      // Les modifications ont déjà été appliquées via les callbacks
      // Éventuellement, sauvegarder explicitement
      Preferences.Save;
    end;
  finally
    SettingsForm.Free;
  end;
end;
```

## Gestion d'état distribué avec des DataModules

Pour les applications complexes, vous pouvez répartir la gestion d'état entre plusieurs DataModules spécialisés :

```pascal
// Définir les différents modules de données
unit AppModules;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Data.DB, FireDAC.Comp.Client;

type
  // Module pour la gestion des utilisateurs
  TUserDataModule = class(TDataModule)
    // ... composants ...
  private
    FCurrentUserID: Integer;
    FCurrentUsername: string;
    FUserRoles: TList<string>;
  public
    property CurrentUserID: Integer read FCurrentUserID write FCurrentUserID;
    property CurrentUsername: string read FCurrentUsername write FCurrentUsername;
    function IsLoggedIn: Boolean;
    function HasRole(const Role: string): Boolean;
    procedure Login(const Username, Password: string);
    procedure Logout;
  end;

  // Module pour la gestion des documents
  TDocumentDataModule = class(TDataModule)
    // ... composants ...
  private
    FCurrentDocument: TDocument;
    FRecentDocuments: TList<string>;
  public
    property CurrentDocument: TDocument read FCurrentDocument;
    function OpenDocument(const FileName: string): Boolean;
    function SaveDocument: Boolean;
    procedure CloseDocument;
    function GetRecentDocuments: TArray<string>;
  end;

  // Module pour les paramètres de l'application
  TSettingsDataModule = class(TDataModule)
    // ... composants ...
  private
    FSettings: TDictionary<string, Variant>;
  public
    function GetSetting<T>(const Key: string; DefaultValue: T): T;
    procedure SetSetting<T>(const Key: string; Value: T);
    procedure LoadSettings;
    procedure SaveSettings;
  end;

var
  UserModule: TUserDataModule;
  DocumentModule: TDocumentDataModule;
  SettingsModule: TSettingsDataModule;

implementation

// ... implémentation ...

end.
```

## Persistance automatique de l'état

Pour les applications qui doivent sauvegarder l'état régulièrement, vous pouvez implémenter un système d'auto-sauvegarde :

```pascal
unit AutoSaveManager;

interface

uses
  System.SysUtils, System.Classes, System.Threading;

type
  TAutoSaveManager = class
  private
    class var FInstance: TAutoSaveManager;

    FAutoSaveInterval: Integer; // en secondes
    FEnabled: Boolean;
    FLastSaveTime: TDateTime;
    FTimer: TTimer;
    FSaveInProgress: Boolean;
    FStateModified: Boolean;

    procedure TimerEvent(Sender: TObject);
    procedure DoSave;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetInstance: TAutoSaveManager;

    property AutoSaveInterval: Integer read FAutoSaveInterval write FAutoSaveInterval;
    property Enabled: Boolean read FEnabled write FEnabled;

    procedure Start;
    procedure Stop;
    procedure SaveNow;
    procedure MarkStateModified;
  end;

function AutoSaveManager: TAutoSaveManager;

implementation

constructor TAutoSaveManager.Create;
begin
  inherited Create;
  FAutoSaveInterval := 300; // 5 minutes par défaut
  FTimer := TTimer.Create(nil);
  FTimer.Enabled := False;
  FTimer.OnTimer := TimerEvent;
  FLastSaveTime := Now;
  FSaveInProgress := False;
  FStateModified := False;
end;

destructor TAutoSaveManager.Destroy;
begin
  FTimer.Free;
  inherited;
end;

procedure TAutoSaveManager.TimerEvent(Sender: TObject);
begin
  // Vérifier si une sauvegarde est nécessaire
  if FEnabled and FStateModified and not FSaveInProgress then
  begin
    // Exécuter la sauvegarde en arrière-plan
    TTask.Run(
      procedure
      begin
        try
          FSaveInProgress := True;
          DoSave;
        finally
          FSaveInProgress := False;
        end;
      end);
  end;
end;

procedure TAutoSaveManager.DoSave;
begin
  // Sauvegarder l'état de l'application
  // (Appeler les méthodes de sauvegarde dans les différents gestionnaires d'état)

  // Mettre à jour le timestamp de la dernière sauvegarde
  FLastSaveTime := Now;
  FStateModified := False;
end;

procedure TAutoSaveManager.Start;
begin
  if not FTimer.Enabled then
  begin
    FTimer.Interval := FAutoSaveInterval * 1000; // en millisecondes
    FTimer.Enabled := True;
    FEnabled := True;
  end;
end;

procedure TAutoSaveManager.MarkStateModified;
begin
  FStateModified := True;
end;

// ... autres méthodes ...

end.
```

## Synchronisation de l'état entre les threads

Dans les applications multi-threads, vous devez protéger l'accès à l'état partagé :

```pascal
unit ThreadSafeState;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs, System.Generics.Collections;

type
  TThreadSafeState = class
  private
    FCriticalSection: TCriticalSection;
    FValues: TDictionary<string, TValue>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetValue<T>(const Key: string; const Value: T);
    function GetValue<T>(const Key: string; const DefaultValue: T): T;
    procedure Clear;

    function Lock: IInterface; // Retourne un objet qui déverrouille automatiquement à la sortie du bloc
  end;

implementation

type
  TAutoUnlocker = class(TInterfacedObject)
  private
    FCriticalSection: TCriticalSection;
  public
    constructor Create(ACriticalSection: TCriticalSection);
    destructor Destroy; override;
  end;

constructor TThreadSafeState.Create;
begin
  inherited;
  FCriticalSection := TCriticalSection.Create;
  FValues := TDictionary<string, TValue>.Create;
end;

destructor TThreadSafeState.Destroy;
begin
  FValues.Free;
  FCriticalSection.Free;
  inherited;
end;

function TThreadSafeState.Lock: IInterface;
begin
  FCriticalSection.Enter;
  Result := TAutoUnlocker.Create(FCriticalSection);
end;

procedure TThreadSafeState.SetValue<T>(const Key: string; const Value: T);
begin
  with Lock do // Verrouillage automatique pour la durée du bloc
  begin
    FValues.AddOrSetValue(Key, TValue.From<T>(Value));
  end;
end;

function TThreadSafeState.GetValue<T>(const Key: string; const DefaultValue: T): T;
var
  Value: TValue;
begin
  with Lock do
  begin
    if FValues.TryGetValue(Key, Value) and Value.IsType<T> then
      Result := Value.AsType<T>
    else
      Result := DefaultValue;
  end;
end;

// Implémentation de TAutoUnlocker
constructor TAutoUnlocker.Create(ACriticalSection: TCriticalSection);
begin
  inherited Create;
  FCriticalSection := ACriticalSection;
end;

destructor TAutoUnlocker.Destroy;
begin
  FCriticalSection.Leave;
  inherited;
end;

end.
```

## Exercices pratiques

1. **Exercice simple** : Créez une application avec un formulaire principal qui sauvegarde et restaure sa position, sa taille et d'autres paramètres d'interface utilisateur entre les sessions.

2. **Exercice intermédiaire** : Implémentez une application de gestion de contacts avec un gestionnaire d'état central qui maintient la liste des contacts et permet de filtrer, trier et modifier les contacts.

3. **Exercice avancé** : Développez une application multi-formulaires qui utilise le pattern MVVM pour gérer l'état, avec persistance automatique et synchronisation entre les différentes vues.

## Conclusion

La gestion d'état est un aspect fondamental du développement d'applications Delphi robustes et conviviales. En appliquant les techniques présentées dans ce chapitre, vous pourrez :

- Maintenir une cohérence dans l'expérience utilisateur
- Faciliter la maintenance et l'extension de votre code
- Améliorer la robustesse de vos applications
- Offrir des fonctionnalités comme l'annulation/rétablissement, l'auto-sauvegarde et la restauration de session

La complexité de votre approche de gestion d'état doit correspondre à la complexité de votre application. Pour les petites applications, des solutions simples comme les fichiers INI ou le registre peuvent suffire. Pour les applications plus complexes, envisagez des patterns comme MVVM ou une architecture modulaire avec des DataModules spécialisés.

Quelle que soit l'approche choisie, assurez-vous de suivre les bonnes pratiques en matière de séparation des préoccupations, de validation des données et de gestion des erreurs pour créer des applications fiables et performantes.
