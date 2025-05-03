# 15.6 Stockage local et synchronisation

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La gestion des données est un aspect fondamental des applications mobiles modernes. Les utilisateurs s'attendent à ce que leurs données soient préservées même lorsqu'ils ferment l'application et à pouvoir accéder à ces mêmes données sur différents appareils. Dans cette section, nous explorerons les techniques pour stocker les données localement et les synchroniser avec un serveur distant ou d'autres appareils.

## Défis du stockage et de la synchronisation sur mobile

Les applications mobiles présentent plusieurs défis spécifiques concernant le stockage et la synchronisation des données :

1. **Connectivité intermittente** : Les appareils mobiles peuvent régulièrement perdre leur connexion internet
2. **Ressources limitées** : L'espace de stockage et la mémoire sont plus restreints que sur un ordinateur
3. **Synchronisation bidirectionnelle** : Les données peuvent être modifiées à la fois sur l'appareil et sur le serveur
4. **Conflits de données** : Des modifications contradictoires peuvent survenir sur différents appareils
5. **Optimisation de la batterie** : Les opérations de synchronisation doivent être efficientes

## Options de stockage local avec Delphi

Delphi offre plusieurs approches pour stocker des données localement sur un appareil mobile. Chacune présente des avantages et des inconvénients selon les besoins de votre application.

### 1. Fichiers plats (texte, JSON, XML)

L'approche la plus simple consiste à stocker les données dans des fichiers texte, JSON ou XML.

#### Avantages
- Simple à mettre en œuvre
- Aucune configuration requise
- Facile à déboguer (vous pouvez lire le contenu des fichiers)

#### Inconvénients
- Limité pour les données complexes ou relationnelles
- Performances réduites pour les grands volumes de données
- Pas de requêtes ou de filtrage intégré

#### Exemple : Stockage avec JSON

```pascal
uses
  System.JSON, System.IOUtils, System.SysUtils;

type
  TUserSettings = class
  private
    FUsername: string;
    FLastLoginDate: TDateTime;
    FPreferDarkMode: Boolean;
    FNotificationsEnabled: Boolean;
  public
    constructor Create;

    // Propriétés
    property Username: string read FUsername write FUsername;
    property LastLoginDate: TDateTime read FLastLoginDate write FLastLoginDate;
    property PreferDarkMode: Boolean read FPreferDarkMode write FPreferDarkMode;
    property NotificationsEnabled: Boolean read FNotificationsEnabled
      write FNotificationsEnabled;

    // Sauvegarde et chargement
    procedure SaveToFile;
    procedure LoadFromFile;
  end;

constructor TUserSettings.Create;
begin
  inherited Create;
  // Valeurs par défaut
  FUsername := '';
  FLastLoginDate := Now;
  FPreferDarkMode := False;
  FNotificationsEnabled := True;
end;

procedure TUserSettings.SaveToFile;
var
  JsonObj: TJSONObject;
  FilePath: string;
begin
  // Créer un objet JSON
  JsonObj := TJSONObject.Create;
  try
    // Ajouter les propriétés
    JsonObj.AddPair('username', FUsername);
    JsonObj.AddPair('lastLoginDate', DateToISO8601(FLastLoginDate));
    JsonObj.AddPair('preferDarkMode', TJSONBool.Create(FPreferDarkMode));
    JsonObj.AddPair('notificationsEnabled', TJSONBool.Create(FNotificationsEnabled));

    // Déterminer le chemin du fichier selon la plateforme
    {$IFDEF ANDROID}
    FilePath := TPath.Combine(TPath.GetDocumentsPath, 'settings.json');
    {$ELSEIF DEFINED(IOS)}
    FilePath := TPath.Combine(TPath.GetDocumentsPath, 'settings.json');
    {$ELSE}
    FilePath := TPath.Combine(TPath.GetHomePath, 'settings.json');
    {$ENDIF}

    // Écrire dans le fichier
    TFile.WriteAllText(FilePath, JsonObj.ToString);
  finally
    JsonObj.Free;
  end;
end;

procedure TUserSettings.LoadFromFile;
var
  JsonStr: string;
  JsonObj: TJSONObject;
  FilePath: string;
begin
  // Déterminer le chemin du fichier selon la plateforme
  {$IFDEF ANDROID}
  FilePath := TPath.Combine(TPath.GetDocumentsPath, 'settings.json');
  {$ELSEIF DEFINED(IOS)}
  FilePath := TPath.Combine(TPath.GetDocumentsPath, 'settings.json');
  {$ELSE}
  FilePath := TPath.Combine(TPath.GetHomePath, 'settings.json');
  {$ENDIF}

  // Vérifier si le fichier existe
  if not TFile.Exists(FilePath) then
    Exit;

  try
    // Lire le contenu du fichier
    JsonStr := TFile.ReadAllText(FilePath);

    // Analyser le JSON
    JsonObj := TJSONObject.ParseJSONValue(JsonStr) as TJSONObject;
    if JsonObj <> nil then
    try
      // Extraire les valeurs
      if JsonObj.TryGetValue<string>('username', FUsername) then;

      var LastLoginStr: string;
      if JsonObj.TryGetValue<string>('lastLoginDate', LastLoginStr) then
        FLastLoginDate := ISO8601ToDate(LastLoginStr);

      if JsonObj.TryGetValue<Boolean>('preferDarkMode', FPreferDarkMode) then;
      if JsonObj.TryGetValue<Boolean>('notificationsEnabled', FNotificationsEnabled) then;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
    begin
      // Gestion des erreurs - réinitialiser avec les valeurs par défaut
      FUsername := '';
      FLastLoginDate := Now;
      FPreferDarkMode := False;
      FNotificationsEnabled := True;
    end;
  end;
end;
```

Pour utiliser cette classe dans votre application :

```pascal
var
  UserSettings: TUserSettings;
begin
  // Créer l'objet de paramètres
  UserSettings := TUserSettings.Create;
  try
    // Charger les paramètres existants
    UserSettings.LoadFromFile;

    // Modifier des paramètres
    UserSettings.Username := 'NouvelUtilisateur';
    UserSettings.PreferDarkMode := True;

    // Sauvegarder les modifications
    UserSettings.SaveToFile;
  finally
    UserSettings.Free;
  end;
end;
```

### 2. Base de données SQLite

SQLite est une solution légère et puissante pour stocker des données structurées localement. Delphi offre un excellent support pour SQLite via FireDAC.

#### Avantages
- Support complet des fonctionnalités SQL
- Excellent pour les données relationnelles
- Bonnes performances même pour de grands volumes de données
- Requêtes et filtrage puissants

#### Inconvénients
- Configuration initiale plus complexe
- Nécessite des connaissances en SQL
- Fichier binaire difficile à inspecter manuellement

#### Configuration d'une base de données SQLite

Commencez par ajouter les composants nécessaires à votre formulaire :

1. Un `TFDConnection` pour la connexion à la base de données
2. Un `TFDQuery` pour exécuter des requêtes
3. Un `TFDGUIxWaitCursor` pour afficher un indicateur de chargement
4. Un `TFDPhysSQLiteDriverLink` pour le pilote SQLite

```pascal
// Configuration de la connexion à la base de données
procedure TMainForm.SetupDatabase;
var
  DBPath: string;
begin
  // Déterminer le chemin de la base de données
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  DBPath := TPath.Combine(TPath.GetDocumentsPath, 'appdata.db');
  {$ELSE}
  DBPath := TPath.Combine(TPath.GetHomePath, 'appdata.db');
  {$ENDIF}

  // Configurer la connexion
  FDConnection1.Params.Clear;
  FDConnection1.Params.DriverID := 'SQLite';
  FDConnection1.Params.Database := DBPath;
  FDConnection1.Params.Add('OpenMode=CreateUTF8'); // Pour créer le fichier s'il n'existe pas

  try
    // Ouvrir la connexion
    FDConnection1.Connected := True;

    // Créer les tables si elles n'existent pas
    CreateTables;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion à la base de données: ' + E.Message);
  end;
end;

// Création des tables
procedure TMainForm.CreateTables;
begin
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS Tasks (' +
    '  ID INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  Title TEXT NOT NULL,' +
    '  Description TEXT,' +
    '  DueDate DATETIME,' +
    '  IsCompleted BOOLEAN DEFAULT 0,' +
    '  CreatedAt DATETIME DEFAULT CURRENT_TIMESTAMP,' +
    '  SyncStatus INTEGER DEFAULT 0' + // 0=NonSynchro, 1=Synchro, 2=EnConflit
    ')'
  );
end;
```

#### Opérations CRUD avec SQLite

```pascal
// Ajouter une tâche
function TMainForm.AddTask(const Title, Description: string; DueDate: TDateTime): Integer;
begin
  // Préparer la requête
  FDQuery1.SQL.Text :=
    'INSERT INTO Tasks (Title, Description, DueDate, IsCompleted, SyncStatus) ' +
    'VALUES (:Title, :Description, :DueDate, 0, 0)';

  // Définir les paramètres
  FDQuery1.ParamByName('Title').AsString := Title;
  FDQuery1.ParamByName('Description').AsString := Description;
  FDQuery1.ParamByName('DueDate').AsDateTime := DueDate;

  // Exécuter et obtenir l'ID généré
  FDQuery1.ExecSQL;

  // Obtenir l'ID de la dernière insertion
  Result := FDConnection1.GetLastAutoGenValue('');
end;

// Récupérer toutes les tâches
procedure TMainForm.LoadTasks;
begin
  // Préparer et exécuter la requête
  FDQuery1.SQL.Text :=
    'SELECT * FROM Tasks ORDER BY DueDate ASC';
  FDQuery1.Open;

  // Les données sont maintenant disponibles dans FDQuery1
  // Vous pouvez les lier à un contrôle visuel comme TListView ou TGrid
end;

// Mettre à jour une tâche
procedure TMainForm.UpdateTask(ID: Integer; const Title, Description: string;
                             DueDate: TDateTime; IsCompleted: Boolean);
begin
  // Préparer la requête
  FDQuery1.SQL.Text :=
    'UPDATE Tasks SET ' +
    'Title = :Title, Description = :Description, ' +
    'DueDate = :DueDate, IsCompleted = :IsCompleted, ' +
    'SyncStatus = 0 ' + // Marquer comme non synchronisé
    'WHERE ID = :ID';

  // Définir les paramètres
  FDQuery1.ParamByName('Title').AsString := Title;
  FDQuery1.ParamByName('Description').AsString := Description;
  FDQuery1.ParamByName('DueDate').AsDateTime := DueDate;
  FDQuery1.ParamByName('IsCompleted').AsBoolean := IsCompleted;
  FDQuery1.ParamByName('ID').AsInteger := ID;

  // Exécuter la requête
  FDQuery1.ExecSQL;
end;

// Supprimer une tâche
procedure TMainForm.DeleteTask(ID: Integer);
begin
  // Préparer la requête
  FDQuery1.SQL.Text := 'DELETE FROM Tasks WHERE ID = :ID';

  // Définir le paramètre
  FDQuery1.ParamByName('ID').AsInteger := ID;

  // Exécuter la requête
  FDQuery1.ExecSQL;
end;
```

### 3. ClientDataSet en mémoire

Pour des besoins plus simples, vous pouvez utiliser un `TClientDataSet` qui maintient les données en mémoire et peut les persister dans un fichier.

#### Avantages
- Facile à utiliser avec les composants visuels
- Pas besoin de SQL pour les opérations simples
- Bonnes performances pour les petits ensembles de données

#### Inconvénients
- Moins flexible que SQLite pour les requêtes complexes
- Moins performant pour les grands ensembles de données
- Fonctionnalités limitées pour les relations

#### Exemple d'utilisation de ClientDataSet

```pascal
uses
  Data.DB, Datasnap.DBClient, System.IOUtils;

type
  TNotesManager = class
  private
    FDataSet: TClientDataSet;
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddNote(const Title, Content: string);
    procedure UpdateNote(ID: Integer; const Title, Content: string);
    procedure DeleteNote(ID: Integer);
    procedure SaveToFile;
    procedure LoadFromFile;

    property DataSet: TClientDataSet read FDataSet;
  end;

constructor TNotesManager.Create;
begin
  inherited Create;

  // Créer le dataset
  FDataSet := TClientDataSet.Create(nil);

  // Définir la structure
  FDataSet.FieldDefs.Add('ID', ftInteger);
  FDataSet.FieldDefs.Add('Title', ftString, 100);
  FDataSet.FieldDefs.Add('Content', ftMemo);
  FDataSet.FieldDefs.Add('CreatedDate', ftDateTime);
  FDataSet.FieldDefs.Add('ModifiedDate', ftDateTime);

  // Créer une clé primaire
  var IndexDef := FDataSet.IndexDefs.AddIndexDef;
  IndexDef.Name := 'IDIndex';
  IndexDef.Fields := 'ID';
  IndexDef.Options := [ixPrimary, ixUnique];

  // Créer le dataset en mémoire
  FDataSet.CreateDataSet;

  // Déterminer le nom du fichier
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  FFileName := TPath.Combine(TPath.GetDocumentsPath, 'notes.cds');
  {$ELSE}
  FFileName := TPath.Combine(TPath.GetHomePath, 'notes.cds');
  {$ENDIF}

  // Charger les données existantes
  LoadFromFile;
end;

destructor TNotesManager.Destroy;
begin
  // Sauvegarder avant de libérer
  SaveToFile;

  // Libérer les ressources
  FDataSet.Free;

  inherited;
end;

procedure TNotesManager.AddNote(const Title, Content: string);
var
  NextID: Integer;
begin
  // Déterminer le prochain ID
  if FDataSet.RecordCount = 0 then
    NextID := 1
  else
  begin
    FDataSet.Last;
    NextID := FDataSet.FieldByName('ID').AsInteger + 1;
  end;

  // Ajouter un nouveau enregistrement
  FDataSet.Append;
  FDataSet.FieldByName('ID').AsInteger := NextID;
  FDataSet.FieldByName('Title').AsString := Title;
  FDataSet.FieldByName('Content').AsString := Content;
  FDataSet.FieldByName('CreatedDate').AsDateTime := Now;
  FDataSet.FieldByName('ModifiedDate').AsDateTime := Now;
  FDataSet.Post;
end;

procedure TNotesManager.UpdateNote(ID: Integer; const Title, Content: string);
begin
  // Localiser l'enregistrement par ID
  if FDataSet.Locate('ID', ID, []) then
  begin
    FDataSet.Edit;
    FDataSet.FieldByName('Title').AsString := Title;
    FDataSet.FieldByName('Content').AsString := Content;
    FDataSet.FieldByName('ModifiedDate').AsDateTime := Now;
    FDataSet.Post;
  end;
end;

procedure TNotesManager.DeleteNote(ID: Integer);
begin
  // Localiser et supprimer l'enregistrement
  if FDataSet.Locate('ID', ID, []) then
    FDataSet.Delete;
end;

procedure TNotesManager.SaveToFile;
begin
  try
    // Sauvegarder le dataset dans un fichier
    FDataSet.SaveToFile(FFileName);
  except
    on E: Exception do
      // Gérer l'erreur de sauvegarde
  end;
end;

procedure TNotesManager.LoadFromFile;
begin
  // Vérifier si le fichier existe
  if TFile.Exists(FFileName) then
  try
    // Charger le dataset depuis le fichier
    FDataSet.LoadFromFile(FFileName);
  except
    on E: Exception do
      // Gérer l'erreur de chargement
  end;
end;
```

Pour utiliser cette classe avec un contrôle visuel :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
var
  DataSource1: TDataSource;
begin
  // Créer le gestionnaire de notes
  FNotesManager := TNotesManager.Create;

  // Créer une source de données
  DataSource1 := TDataSource.Create(Self);
  DataSource1.DataSet := FNotesManager.DataSet;

  // Lier à un contrôle visuel (par exemple, une grille)
  DBGrid1.DataSource := DataSource1;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FNotesManager.Free;
end;
```

## Synchronisation avec un serveur distant

La synchronisation des données entre l'appareil mobile et un serveur distant est essentielle pour les applications modernes. Voici comment mettre en place une synchronisation efficace.

### Architecture générale de synchronisation

Une architecture de synchronisation robuste comprend généralement :

1. **Base de données locale** : Stocke les données sur l'appareil
2. **API REST** : Interface pour communiquer avec le serveur
3. **Mécanisme de détection des changements** : Identifie les données modifiées
4. **Gestion des conflits** : Résout les modifications contradictoires
5. **File d'attente de synchronisation** : Gère les échecs temporaires de connectivité

### Classe de synchronisation basique

Voici une implémentation simplifiée d'un service de synchronisation :

```pascal
uses
  System.Net.HttpClient, System.JSON, System.SysUtils, System.Classes,
  Data.DB, FireDAC.Comp.Client;

type
  TSyncStatus = (ssNotSynced, ssSynced, ssConflict);

  TSyncService = class
  private
    FConnection: TFDConnection;
    FBaseURL: string;
    FAuthToken: string;

    function GetModifiedData: TFDQuery;
    function SendToServer(const JSON: string): Boolean;
    function ReceiveFromServer(LastSyncDate: TDateTime): string;
    procedure UpdateLocalRecord(const JSONObj: TJSONObject);
    procedure MarkAsSynced(ID: Integer);
    procedure HandleConflict(LocalData, ServerData: TJSONObject);
  public
    constructor Create(AConnection: TFDConnection;
                     const ABaseURL, AAuthToken: string);

    function SynchronizeTasks: Boolean;
    property BaseURL: string read FBaseURL write FBaseURL;
    property AuthToken: string read FAuthToken write FAuthToken;
  end;

constructor TSyncService.Create(AConnection: TFDConnection;
                               const ABaseURL, AAuthToken: string);
begin
  inherited Create;
  FConnection := AConnection;
  FBaseURL := ABaseURL;
  FAuthToken := AAuthToken;
end;

function TSyncService.GetModifiedData: TFDQuery;
var
  Query: TFDQuery;
begin
  // Créer une requête pour récupérer les données modifiées
  Query := TFDQuery.Create(nil);
  Query.Connection := FConnection;

  // Récupérer les enregistrements non synchronisés
  Query.SQL.Text :=
    'SELECT * FROM Tasks WHERE SyncStatus = :Status';
  Query.ParamByName('Status').AsInteger := Ord(ssNotSynced);
  Query.Open;

  Result := Query;
end;

function TSyncService.SendToServer(const JSON: string): Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  Result := False;

  Client := THTTPClient.Create;
  try
    // Configurer l'en-tête pour l'authentification
    Client.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;
    Client.ContentType := 'application/json';

    // Construire l'URL
    URL := FBaseURL + '/api/tasks/sync';

    // Envoyer les données au serveur
    Response := Client.Post(URL, TStringStream.Create(JSON));

    // Vérifier la réponse
    Result := (Response.StatusCode = 200);
  finally
    Client.Free;
  end;
end;

function TSyncService.ReceiveFromServer(LastSyncDate: TDateTime): string;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
begin
  Client := THTTPClient.Create;
  try
    // Configurer l'en-tête pour l'authentification
    Client.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;

    // Construire l'URL avec la date de dernière synchronisation
    URL := Format('%s/api/tasks/sync?lastSync=%s',
                [FBaseURL, DateToISO8601(LastSyncDate)]);

    // Récupérer les données du serveur
    Response := Client.Get(URL);

    if Response.StatusCode = 200 then
      Result := Response.ContentAsString
    else
      Result := '';
  finally
    Client.Free;
  end;
end;

procedure TSyncService.UpdateLocalRecord(const JSONObj: TJSONObject);
var
  Query: TFDQuery;
  ID: Integer;
  ServerID: Integer;
  Title, Description: string;
  DueDate: TDateTime;
  IsCompleted: Boolean;
begin
  // Extraire les données du JSON
  ServerID := JSONObj.GetValue<Integer>('id');
  Title := JSONObj.GetValue<string>('title');
  Description := JSONObj.GetValue<string>('description');
  DueDate := ISO8601ToDate(JSONObj.GetValue<string>('dueDate'));
  IsCompleted := JSONObj.GetValue<Boolean>('isCompleted');

  // Créer une requête pour mise à jour locale
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Vérifier si l'enregistrement existe déjà
    Query.SQL.Text :=
      'SELECT ID FROM Tasks WHERE ServerID = :ServerID';
    Query.ParamByName('ServerID').AsInteger := ServerID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      // Mise à jour d'un enregistrement existant
      ID := Query.FieldByName('ID').AsInteger;

      Query.SQL.Text :=
        'UPDATE Tasks SET ' +
        'Title = :Title, Description = :Description, ' +
        'DueDate = :DueDate, IsCompleted = :IsCompleted, ' +
        'SyncStatus = :Status ' +
        'WHERE ID = :ID';
      Query.ParamByName('ID').AsInteger := ID;
    end
    else
    begin
      // Insertion d'un nouvel enregistrement
      Query.SQL.Text :=
        'INSERT INTO Tasks ' +
        '(Title, Description, DueDate, IsCompleted, ServerID, SyncStatus) ' +
        'VALUES ' +
        '(:Title, :Description, :DueDate, :IsCompleted, :ServerID, :Status)';
      Query.ParamByName('ServerID').AsInteger := ServerID;
    end;

    // Définir les paramètres communs
    Query.ParamByName('Title').AsString := Title;
    Query.ParamByName('Description').AsString := Description;
    Query.ParamByName('DueDate').AsDateTime := DueDate;
    Query.ParamByName('IsCompleted').AsBoolean := IsCompleted;
    Query.ParamByName('Status').AsInteger := Ord(ssSynced);

    // Exécuter la requête
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSyncService.MarkAsSynced(ID: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Marquer l'enregistrement comme synchronisé
    Query.SQL.Text :=
      'UPDATE Tasks SET SyncStatus = :Status WHERE ID = :ID';
    Query.ParamByName('Status').AsInteger := Ord(ssSynced);
    Query.ParamByName('ID').AsInteger := ID;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TSyncService.HandleConflict(LocalData, ServerData: TJSONObject);
var
  Query: TFDQuery;
  ID: Integer;
begin
  // Dans cet exemple simplifié, nous donnons la priorité aux données du serveur
  // Une application réelle pourrait avoir une logique plus sophistiquée pour résoudre les conflits

  // Mettre à jour l'enregistrement local avec les données du serveur
  UpdateLocalRecord(ServerData);

  // Marquer le conflit comme résolu
  ID := LocalData.GetValue<Integer>('id');

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Mettre à jour le statut de synchronisation
    Query.SQL.Text :=
      'UPDATE Tasks SET SyncStatus = :Status WHERE ID = :ID';
    Query.ParamByName('Status').AsInteger := Ord(ssSynced);
    Query.ParamByName('ID').AsInteger := ID;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TSyncService.SynchronizeTasks: Boolean;
var
  ModifiedData: TFDQuery;
  JsonArray: TJSONArray;
  JsonObj: TJSONObject;
  TaskObj: TJSONObject;
  I: Integer;
  ServerResponse: string;
  ServerData: TJSONArray;
  LastSyncDate: TDateTime;
begin
  Result := False;

  // Récupérer la date de dernière synchronisation
  // (Dans une implémentation réelle, stockez cette valeur de manière persistante)
  LastSyncDate := Now - 30; // Par exemple, les 30 derniers jours

  try
    // 1. Envoyer les modifications locales au serveur
    ModifiedData := GetModifiedData;
    try
      // Créer un tableau JSON pour les données modifiées
      JsonArray := TJSONArray.Create;
      try
        ModifiedData.First;
        while not ModifiedData.Eof do
        begin
          // Créer un objet JSON pour chaque tâche
          TaskObj := TJSONObject.Create;

          // Ajouter les propriétés
          TaskObj.AddPair('id', TJSONNumber.Create(ModifiedData.FieldByName('ID').AsInteger));
          TaskObj.AddPair('title', ModifiedData.FieldByName('Title').AsString);
          TaskObj.AddPair('description', ModifiedData.FieldByName('Description').AsString);
          TaskObj.AddPair('dueDate', DateToISO8601(ModifiedData.FieldByName('DueDate').AsDateTime));
          TaskObj.AddPair('isCompleted', TJSONBool.Create(ModifiedData.FieldByName('IsCompleted').AsBoolean));

          // Ajouter au tableau
          JsonArray.AddElement(TaskObj);

          ModifiedData.Next;
        end;

        // Envoyer les données au serveur
        if JsonArray.Count > 0 then
        begin
          if SendToServer(JsonArray.ToString) then
          begin
            // Marquer les enregistrements comme synchronisés
            ModifiedData.First;
            while not ModifiedData.Eof do
            begin
              MarkAsSynced(ModifiedData.FieldByName('ID').AsInteger);
              ModifiedData.Next;
            end;
          end;
        end;
      finally
        JsonArray.Free;
      end;

      // 2. Récupérer les modifications du serveur
      ServerResponse := ReceiveFromServer(LastSyncDate);
      if ServerResponse <> '' then
      begin
        ServerData := TJSONObject.ParseJSONValue(ServerResponse) as TJSONArray;
        if ServerData <> nil then
        try
          // Traiter chaque enregistrement reçu
          for I := 0 to ServerData.Count - 1 do
          begin
            JsonObj := ServerData.Items[I] as TJSONObject;

            // Mettre à jour les données locales
            UpdateLocalRecord(JsonObj);
          end;
        finally
          ServerData.Free;
        end;
      end;

      Result := True;
    finally
      ModifiedData.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer les erreurs de synchronisation
      // Une implémentation réelle pourrait journaliser l'erreur et réessayer plus tard
      Result := False;
    end;
  end;
end;
```

### Intégration du service de synchronisation dans l'application

Voici comment intégrer ce service de synchronisation dans une application :

```pascal
procedure TMainForm.SynchronizeData;
begin
  // Afficher un indicateur de progression
  ProgressBar1.Visible := True;
  lblStatus.Text := 'Synchronisation en cours...';

  // Utiliser un thread pour ne pas bloquer l'interface utilisateur
  TTask.Run(procedure
  begin
    var Success := False;

    try
      // Créer et utiliser le service de synchronisation
      var SyncService := TSyncService.Create(
        FDConnection1,           // Connexion à la base de données
        'https://api.example.com', // URL de base de l'API
        UserSession.AuthToken    // Token d'authentification
      );

      try
        // Effectuer la synchronisation
        Success := SyncService.SynchronizeTasks;
      finally
        SyncService.Free;
      end;
    except
      on E: Exception do
        Success := False;
    end;

    // Revenir au thread principal pour mettre à jour l'interface
    TThread.Synchronize(nil, procedure
    begin
      // Masquer l'indicateur de progression
      ProgressBar1.Visible := False;

      if Success then
      begin
        lblStatus.Text := 'Synchronisation réussie';

        // Rafraîchir les données affichées
        LoadTasks;
      end
      else
        lblStatus.Text := 'Échec de la synchronisation';
    end);
  end);
end;
```

## Stratégies pour une synchronisation efficace

Implémenter une synchronisation robuste peut être complexe. Voici quelques stratégies éprouvées :

### 1. Synchronisation incrémentielle

Au lieu d'envoyer et de recevoir toutes les données à chaque fois, utilisez une approche incrémentielle :

```pascal
// Stocker la date de dernière synchronisation
procedure SaveLastSyncDate(LastSync: TDateTime);
var
  IniFile: TIniFile;
  FilePath: string;
begin
  // Déterminer le chemin du fichier de configuration
  FilePath := TPath.Combine(TPath.GetDocumentsPath, 'config.ini');

  // Sauvegarder la date
  IniFile := TIniFile.Create(FilePath);
  try
    IniFile.WriteDateTime('Sync', 'LastSyncDate', LastSync);
  finally
    IniFile.Free;
  end;
end;

// Récupérer la date de dernière synchronisation
function GetLastSyncDate: TDateTime;
var
  IniFile: TIniFile;
  FilePath: string;
begin
  // Valeur par défaut (1 semaine en arrière)
  Result := Now - 7;

  // Déterminer le chemin du fichier de configuration
  FilePath := TPath.Combine(TPath.GetDocumentsPath, 'config.ini');

  if TFile.Exists(FilePath) then
  begin
    // Lire la date sauvegardée
    IniFile := TIniFile.Create(FilePath);
    try
      Result := IniFile.ReadDateTime('Sync', 'LastSyncDate', Result);
    finally
      IniFile.Free;
    end;
  end;
end;
```

### 2. Synchronisation optimiste vs pessimiste

#### Synchronisation optimiste

- Permet les modifications locales sans vérification préalable
- Détecte et résout les conflits après coup
- Meilleure expérience utilisateur en mode hors ligne
- Plus complexe à implémenter

#### Synchronisation pessimiste

- Verrouille les données avant modification
- Empêche les conflits en amont
- Nécessite une connexion constante
- Plus simple à implémenter

L'exemple précédent utilise une approche optimiste, idéale pour les applications mobiles.

### 3. Suivi des modifications avec des timestamps

Pour faciliter la détection des modifications, ajoutez des champs de suivi :

```pascal
// Modifier la structure de la table
procedure CreateTableWithTimestamps;
begin
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS Tasks (' +
    '  ID INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  Title TEXT NOT NULL,' +
    '  Description TEXT,' +
    '  DueDate DATETIME,' +
    '  IsCompleted BOOLEAN DEFAULT 0,' +
    '  CreatedAt DATETIME DEFAULT CURRENT_TIMESTAMP,' +
    '  ModifiedAt DATETIME DEFAULT CURRENT_TIMESTAMP,' +
    '  SyncStatus INTEGER DEFAULT 0,' +
    '  ServerID INTEGER,' +
    '  ServerModifiedAt DATETIME' +
    ')'
  );
end;

// Déclencher automatiquement la mise à jour du timestamp
procedure CreateUpdateTrigger;
begin
  FDConnection1.ExecSQL(
    'CREATE TRIGGER IF NOT EXISTS update_modified_at ' +
    'AFTER UPDATE ON Tasks ' +
    'FOR EACH ROW ' +
    'BEGIN ' +
    '  UPDATE Tasks SET ModifiedAt = CURRENT_TIMESTAMP, ' +
    '                   SyncStatus = CASE WHEN old.SyncStatus = 1 ' +
    '                                     THEN 0 ELSE SyncStatus END ' +
    '  WHERE ID = NEW.ID; ' +
    'END'
  );
end;
```

### 4. File d'attente de synchronisation

Pour gérer la connectivité intermittente, implémentez une file d'attente de synchronisation :

```pascal
type
  TSyncQueue = class
  private
    FQueue: TList<Integer>;
    FProcessing: Boolean;
    FSyncService: TSyncService;

    procedure ProcessNextItem;
  public
    constructor Create(ASyncService: TSyncService);
    destructor Destroy; override;

    procedure EnqueueItem(ID: Integer);
    procedure StartProcessing;
    procedure StopProcessing;

    property Processing: Boolean read FProcessing;
  end;

constructor TSyncQueue.Create(ASyncService: TSyncService);
begin
  inherited Create;
  FQueue := TList<Integer>.Create;
  FSyncService := ASyncService;
  FProcessing := False;
end;

destructor TSyncQueue.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TSyncQueue.EnqueueItem(ID: Integer);
begin
  if not FQueue.Contains(ID) then
    FQueue.Add(ID);

  if FProcessing and (FQueue.Count = 1) then
    ProcessNextItem;
end;

procedure TSyncQueue.StartProcessing;
begin
  if not FProcessing then
  begin
    FProcessing := True;

    if FQueue.Count > 0 then
      ProcessNextItem;
  end;
end;

procedure TSyncQueue.StopProcessing;
begin
  FProcessing := False;
end;

procedure TSyncQueue.ProcessNextItem;
var
  ID: Integer;
begin
  if (FQueue.Count = 0) or (not FProcessing) then
    Exit;

  // Obtenir le prochain élément
  ID := FQueue[0];
  FQueue.Delete(0);

  // Traiter l'élément dans un thread
  TTask.Run(procedure
  var
    Success: Boolean;
  begin
    try
      // Essayer de synchroniser cet élément spécifique
      Success := FSyncService.SynchronizeTask(ID);
    except
      Success := False;
    end;

    TThread.Synchronize(nil, procedure
    begin
      if not Success then
      begin
        // Remettre dans la file d'attente pour réessayer plus tard
        // (avec une limite de tentatives dans une implémentation réelle)
        FQueue.Add(ID);
      end;

      // Traiter l'élément suivant
      if FQueue.Count > 0 then
        ProcessNextItem;
    end);
  end);
end;
```

## Gestion du mode hors ligne

Une application mobile doit fonctionner efficacement même sans connexion internet. Voici quelques techniques pour gérer le mode hors ligne :

### 1. Détection de la connectivité

Commencez par détecter si l'appareil est connecté à internet :

```pascal
uses
  System.Net.HttpClient, System.Threading;

function IsNetworkAvailable: Boolean;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
begin
  Result := False;

  Client := THTTPClient.Create;
  try
    Client.ConnectionTimeout := 3000; // 3 secondes
    Client.ResponseTimeout := 3000;   // 3 secondes

    try
      // Essayer d'accéder à un service fiable
      Response := Client.Head('https://www.google.com');
      Result := (Response.StatusCode = 200);
    except
      // Une exception indique probablement une absence de connexion
      Result := False;
    end;
  finally
    Client.Free;
  end;
end;

// Utilisation avec vérification asynchrone
procedure TMainForm.CheckConnectivity;
begin
  // Vérifier la connectivité en arrière-plan
  TTask.Run(procedure
  var
    Connected: Boolean;
  begin
    Connected := IsNetworkAvailable;

    // Mettre à jour l'interface dans le thread principal
    TThread.Synchronize(nil, procedure
    begin
      if Connected then
      begin
        lblStatus.Text := 'Connecté';
        btnSync.Enabled := True;

        // Lancer la synchronisation automatique si nécessaire
        if ckAutoSync.IsChecked then
          SynchronizeData;
      end
      else
      begin
        lblStatus.Text := 'Hors ligne';
        btnSync.Enabled := False;
      end;
    end);
  end);
end;
```

### 2. Mise en file d'attente des opérations de synchronisation

Quand une opération de synchronisation échoue en raison d'une absence de connexion, mettez-la en file d'attente pour plus tard :

```pascal
procedure TMainForm.SyncDataWithRetry;
begin
  // Vérifier d'abord la connectivité
  TTask.Run(procedure
  var
    Connected: Boolean;
  begin
    Connected := IsNetworkAvailable;

    TThread.Synchronize(nil, procedure
    begin
      if Connected then
      begin
        // Si connecté, synchroniser normalement
        SynchronizeData;
      end
      else
      begin
        // Si hors ligne, enregistrer pour synchronisation ultérieure
        FSyncPending := True;
        lblStatus.Text := 'Synchronisation en attente de connexion';

        // Démarrer la surveillance de la connectivité
        if not FConnectivityTimer.Enabled then
          FConnectivityTimer.Enabled := True;
      end;
    end);
  end);
end;

// Timer pour vérifier périodiquement la connectivité
procedure TMainForm.ConnectivityTimerTimer(Sender: TObject);
begin
  // Vérifier si une synchronisation est en attente
  if FSyncPending then
  begin
    // Vérifier la connectivité
    TTask.Run(procedure
    var
      Connected: Boolean;
    begin
      Connected := IsNetworkAvailable;

      TThread.Synchronize(nil, procedure
      begin
        if Connected then
        begin
          // Connexion rétablie, synchroniser
          FSyncPending := False;
          FConnectivityTimer.Enabled := False;
          SynchronizeData;
        end;
      end);
    end);
  end
  else
    FConnectivityTimer.Enabled := False;
end;
```

## Encapsulation dans un modèle de données complet

Pour simplifier l'utilisation dans votre application, encapsulez toutes ces fonctionnalités dans un modèle de données complet :

```pascal
unit TaskModel;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections,
  FireDAC.Comp.Client, FireDAC.DApt, Data.DB;

type
  TTaskPriority = (tpLow, tpMedium, tpHigh);

  TTask = class
  private
    FID: Integer;
    FTitle: string;
    FDescription: string;
    FDueDate: TDateTime;
    FIsCompleted: Boolean;
    FPriority: TTaskPriority;
    FServerID: Integer;
    FSyncStatus: Integer;
  public
    constructor Create; overload;
    constructor Create(AID: Integer; const ATitle, ADescription: string;
      ADueDate: TDateTime; AIsCompleted: Boolean; APriority: TTaskPriority); overload;

    property ID: Integer read FID write FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property IsCompleted: Boolean read FIsCompleted write FIsCompleted;
    property Priority: TTaskPriority read FPriority write FPriority;
    property ServerID: Integer read FServerID write FServerID;
    property SyncStatus: Integer read FSyncStatus write FSyncStatus;

    function ToJSON: TJSONObject;
    procedure FromJSON(const JSONObj: TJSONObject);
  end;

  TTaskManager = class
  private
    FConnection: TFDConnection;
    FSyncService: TSyncService;
    FSyncQueue: TSyncQueue;

    procedure InitializeDatabase;
    procedure CreateTables;
    procedure CreateTriggers;
  public
    constructor Create(const DBPath: string);
    destructor Destroy; override;

    // Opérations CRUD
    function AddTask(const Task: TTask): Integer;
    function GetTask(ID: Integer): TTask;
    function GetAllTasks: TObjectList<TTask>;
    function GetTasksWithFilter(const Filter: string): TObjectList<TTask>;
    procedure UpdateTask(const Task: TTask);
    procedure DeleteTask(ID: Integer);
    procedure MarkTaskAsCompleted(ID: Integer; Completed: Boolean);

    // Synchronisation
    procedure SynchronizeTasks;
    procedure SynchronizeTasksAsync(CompletionCallback: TProc<Boolean>);
    function IsSynchronizing: Boolean;

    // Connexion
    function IsConnected: Boolean;
    procedure RegisterConnectionChangedCallback(Callback: TProc<Boolean>);
  end;

implementation

// ... Implémentation des méthodes ...

end.
```

Cette classe fournit une API simple pour interagir avec les tâches, tout en gérant la complexité du stockage local et de la synchronisation en arrière-plan.

## Exemple d'utilisation du modèle de données

Voici comment utiliser ce modèle de données dans votre application :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, TaskModel;

type
  TTaskForm = class(TForm)
    ListView1: TListView;
    ToolBar1: TToolBar;
    btnAdd: TButton;
    btnSync: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnSyncClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
    procedure ListView1DeleteItem(Sender: TObject; AIndex: Integer);
  private
    FTaskManager: TTaskManager;

    procedure LoadTasks;
    procedure UpdateConnectionStatus(Connected: Boolean);
  public
    { Public declarations }
  end;

var
  TaskForm: TTaskForm;

implementation

{$R *.fmx}

procedure TTaskForm.FormCreate(Sender: TObject);
var
  DBPath: string;
begin
  // Déterminer le chemin de la base de données
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  DBPath := TPath.Combine(TPath.GetDocumentsPath, 'tasks.db');
  {$ELSE}
  DBPath := TPath.Combine(TPath.GetHomePath, 'tasks.db');
  {$ENDIF}

  // Créer le gestionnaire de tâches
  FTaskManager := TTaskManager.Create(DBPath);

  // Enregistrer le callback de changement de connectivité
  FTaskManager.RegisterConnectionChangedCallback(UpdateConnectionStatus);

  // Charger les tâches
  LoadTasks;
end;

procedure TTaskForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FTaskManager.Free;
end;

procedure TTaskForm.LoadTasks;
var
  Tasks: TObjectList<TTask>;
  Task: TTask;
  Item: TListViewItem;
begin
  // Vider la liste
  ListView1.Items.Clear;

  // Récupérer toutes les tâches
  Tasks := FTaskManager.GetAllTasks;
  try
    // Remplir la liste
    for Task in Tasks do
    begin
      Item := ListView1.Items.Add;
      Item.Tag := Task.ID;
      Item.Text := Task.Title;
      Item.Detail := Task.Description;

      // Formatage selon priorité
      case Task.Priority of
        tpLow: Item.Data['Priority'] := '🟢';
        tpMedium: Item.Data['Priority'] := '🟠';
        tpHigh: Item.Data['Priority'] := '🔴';
      end;

      // Date d'échéance
      Item.Data['DueDate'] := FormatDateTime('dd/mm/yyyy', Task.DueDate);

      // Statut de complétion
      if Task.IsCompleted then
        Item.Data['Status'] := '✓'
      else
        Item.Data['Status'] := '';

      // Statut de synchronisation
      if Task.SyncStatus = 0 then
        Item.Data['Sync'] := '⚠️' // Non synchronisé
      else
        Item.Data['Sync'] := '✓'; // Synchronisé
    end;
  finally
    Tasks.Free;
  end;
end;

procedure TTaskForm.btnAddClick(Sender: TObject);
begin
  // Ouvrir un formulaire pour ajouter une tâche
  // ...

  // Après l'ajout, rafraîchir la liste
  LoadTasks;
end;

procedure TTaskForm.btnSyncClick(Sender: TObject);
begin
  // Désactiver le bouton pendant la synchronisation
  btnSync.Enabled := False;
  lblStatus.Text := 'Synchronisation en cours...';

  // Lancer la synchronisation asynchrone
  FTaskManager.SynchronizeTasksAsync(
    procedure(Success: Boolean)
    begin
      // Callback exécuté une fois la synchronisation terminée
      btnSync.Enabled := True;

      if Success then
      begin
        lblStatus.Text := 'Synchronisation réussie';
        LoadTasks; // Rafraîchir la liste
      end
      else
        lblStatus.Text := 'Échec de la synchronisation';
    end
  );
end;

procedure TTaskForm.UpdateConnectionStatus(Connected: Boolean);
begin
  // Mettre à jour l'interface selon l'état de la connexion
  if Connected then
  begin
    lblStatus.Text := 'Connecté';
    btnSync.Enabled := True;
  end
  else
  begin
    lblStatus.Text := 'Hors ligne';
    btnSync.Enabled := False;
  end;
end;

procedure TTaskForm.ListView1ItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  Task: TTask;
begin
  // Récupérer la tâche sélectionnée
  Task := FTaskManager.GetTask(AItem.Tag);
  if Task <> nil then
  try
    // Ouvrir un formulaire pour éditer la tâche
    // ...

    // Après l'édition, rafraîchir la liste
    LoadTasks;
  finally
    Task.Free;
  end;
end;

procedure TTaskForm.ListView1DeleteItem(Sender: TObject; AIndex: Integer);
var
  Item: TListViewItem;
begin
  Item := ListView1.Items[AIndex];

  // Supprimer la tâche
  FTaskManager.DeleteTask(Item.Tag);
end;
```

## Bonnes pratiques pour le stockage et la synchronisation

Pour créer une application robuste avec stockage local et synchronisation :

### 1. Planifiez votre modèle de données

- Pensez à la structure de données avant d'écrire du code
- Incluez des champs pour gérer la synchronisation (ServerID, SyncStatus, etc.)
- Utilisez des timestamps pour suivre les modifications

### 2. Optimisez pour l'expérience utilisateur

- Privilégiez les opérations locales immédiates pour une meilleure réactivité
- Effectuez la synchronisation en arrière-plan
- Montrez clairement à l'utilisateur l'état de synchronisation de ses données

### 3. Gérez bien les conflits

- Définissez une stratégie claire de résolution des conflits
- Donnez la priorité aux données du serveur ou aux données locales selon votre cas d'usage
- Envisagez de demander à l'utilisateur de choisir en cas de conflit important

### 4. Économisez les ressources

- Synchronisez seulement quand c'est nécessaire
- Utilisez la synchronisation incrémentielle
- Compressez les données transmises si le volume est important

### 5. Sécurisez les données

- Chiffrez les données sensibles stockées localement
- Utilisez HTTPS pour toutes les communications avec le serveur
- Validez toujours les données côté serveur

## Conclusion

Le stockage local et la synchronisation sont des aspects critiques des applications mobiles modernes. Delphi offre plusieurs options puissantes pour implémenter ces fonctionnalités, du simple stockage de fichiers JSON à l'utilisation de bases de données SQLite complètes.

En suivant les bonnes pratiques et en utilisant les approches décrites dans ce chapitre, vous pouvez créer des applications qui fonctionnent parfaitement en mode hors ligne tout en maintenant la synchronisation des données entre l'appareil et le serveur lorsqu'une connexion est disponible.

Les utilisateurs apprécieront particulièrement une application qui :
- Fonctionne rapidement même sans connexion internet
- Conserve leurs données en toute sécurité
- Synchronise automatiquement dès que possible
- Ne perd jamais leurs modifications

La prochaine section explorera comment publier vos applications mobiles sur l'App Store et le Play Store, une étape cruciale pour atteindre votre public cible.

⏭️ [Publication sur App Store / Play Store](/15-applications-mobiles-avec-delphi/07-publication-sur-app-store-play-store.md)
