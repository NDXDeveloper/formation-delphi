# 10.8 GraphQL et nouvelles API

## Introduction

Les API (Interfaces de Programmation d'Applications) modernes évoluent rapidement pour répondre aux besoins croissants des développeurs. Parmi ces évolutions, GraphQL se distingue comme une alternative puissante aux API REST traditionnelles. Dans ce chapitre, nous explorerons GraphQL et d'autres nouvelles API, ainsi que leur intégration dans les applications Delphi.

## Qu'est-ce que GraphQL ?

GraphQL est un langage de requête pour API et un environnement d'exécution développé par Facebook en 2015, puis rendu open source. Contrairement aux API REST, où plusieurs endpoints renvoient des structures de données fixes, GraphQL utilise un seul endpoint et permet aux clients de demander exactement les données dont ils ont besoin.

![Comparaison REST vs GraphQL](https://via.placeholder.com/800x400)

### Avantages de GraphQL

- **Requêtes précises** : Vous récupérez uniquement les données dont vous avez besoin
- **Une seule requête** : Évite les problèmes de sur-récupération ou sous-récupération de données
- **Forte typage** : Le schéma définit clairement la structure des données
- **Évolution sans versionnage** : Ajoutez des champs sans casser les applications existantes
- **Introspection** : L'API peut être interrogée pour ses propres capacités

## Bases de GraphQL

### Structure d'une requête GraphQL

Une requête GraphQL ressemble à ceci :

```graphql
{
  utilisateur(id: 123) {
    nom
    email
    articles {
      titre
      datePublication
    }
  }
}
```

Cette requête demande les informations d'un utilisateur avec l'ID 123, incluant son nom, son email, et les titres et dates de publication de ses articles.

### Les trois opérations principales

1. **Query** : Pour récupérer des données (lecture seule)
2. **Mutation** : Pour modifier des données (création, mise à jour, suppression)
3. **Subscription** : Pour écouter des événements en temps réel

## Intégration de GraphQL dans Delphi

### Configuration des outils nécessaires

Pour travailler avec GraphQL dans Delphi, vous utiliserez principalement les composants REST associés à un traitement JSON personnalisé :

1. Installez les packages REST client si ce n'est pas déjà fait
2. Utilisez les composants TRESTClient, TRESTRequest et TRESTResponse

### Création d'une classe utilitaire pour GraphQL

Commençons par créer une classe qui nous aidera à interagir avec les API GraphQL :

```pascal
unit GraphQLClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types;

type
  TGraphQLVariable = TPair<string, TJSONValue>;
  TGraphQLVariables = TArray<TGraphQLVariable>;

  TGraphQLClient = class
  private
    FRESTClient: TRESTClient;
    FEndpoint: string;
    FHeaders: TStrings;
    FOnError: TProc<string>;

    function CreateRequest: TRESTRequest;
    function WrapQuery(const Query: string; Variables: TJSONObject = nil): string;
  public
    constructor Create(const Endpoint: string);
    destructor Destroy; override;

    procedure AddHeader(const Name, Value: string);
    function ExecuteQuery(const Query: string; Variables: TJSONObject = nil): TJSONObject;
    function ExecuteMutation(const Mutation: string; Variables: TJSONObject = nil): TJSONObject;

    property Headers: TStrings read FHeaders;
    property OnError: TProc<string> read FOnError write FOnError;
  end;

implementation

{ TGraphQLClient }

constructor TGraphQLClient.Create(const Endpoint: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FHeaders := TStringList.Create;

  // Créer le client REST
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;

  // Par défaut, on ajoute l'entête Content-Type pour GraphQL
  AddHeader('Content-Type', 'application/json');
end;

destructor TGraphQLClient.Destroy;
begin
  FRESTClient.Free;
  FHeaders.Free;
  inherited;
end;

procedure TGraphQLClient.AddHeader(const Name, Value: string);
begin
  FHeaders.Values[Name] := Value;
end;

function TGraphQLClient.CreateRequest: TRESTRequest;
var
  Request: TRESTRequest;
  Header: string;
begin
  Request := TRESTRequest.Create(nil);
  try
    Request.Client := FRESTClient;
    Request.Method := TRESTRequestMethod.rmPOST;

    // Ajouter les entêtes
    for Header in FHeaders do
      Request.Params.AddHeader(FHeaders.Names[FHeaders.IndexOf(Header)],
                               FHeaders.ValueFromIndex[FHeaders.IndexOf(Header)]);

    Result := Request;
  except
    Request.Free;
    raise;
  end;
end;

function TGraphQLClient.WrapQuery(const Query: string; Variables: TJSONObject): string;
var
  RequestObj: TJSONObject;
begin
  RequestObj := TJSONObject.Create;
  try
    RequestObj.AddPair('query', Query);

    if Assigned(Variables) and (Variables.Count > 0) then
      RequestObj.AddPair('variables', Variables.Clone as TJSONObject);

    Result := RequestObj.ToString;
  finally
    RequestObj.Free;
  end;
end;

function TGraphQLClient.ExecuteQuery(const Query: string; Variables: TJSONObject): TJSONObject;
var
  Request: TRESTRequest;
  Response: TRESTResponse;
  RequestBody: string;
  ResponseObj: TJSONObject;
begin
  Result := nil;
  Request := CreateRequest;
  Response := TRESTResponse.Create(nil);

  try
    Request.Response := Response;

    // Préparer le corps de la requête
    RequestBody := WrapQuery(Query, Variables);
    Request.Body.Add(RequestBody, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter la requête
    Request.Execute;

    // Traiter la réponse
    if (Response.StatusCode = 200) then
    begin
      ResponseObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;

      // Vérifier s'il y a des erreurs
      if ResponseObj.GetValue('errors') <> nil then
      begin
        if Assigned(FOnError) then
          FOnError(Response.Content);
        ResponseObj.Free;
      end
      else
        Result := ResponseObj;
    end
    else
    begin
      if Assigned(FOnError) then
        FOnError(Format('Erreur HTTP %d: %s', [Response.StatusCode, Response.StatusText]));
    end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

function TGraphQLClient.ExecuteMutation(const Mutation: string; Variables: TJSONObject): TJSONObject;
begin
  // Les mutations utilisent le même mécanisme que les requêtes
  Result := ExecuteQuery(Mutation, Variables);
end;

end.
```

### Exemple d'utilisation de base

Voyons maintenant comment utiliser cette classe pour effectuer une requête GraphQL simple :

```pascal
procedure TForm1.ExecuterRequeteGraphQL;
var
  Client: TGraphQLClient;
  Response: TJSONObject;
  Query: string;
begin
  Client := TGraphQLClient.Create('https://api.example.com/graphql');
  try
    // Définir une requête GraphQL
    Query :=
      'query GetUser($id: ID!) {' +
      '  user(id: $id) {' +
      '    name' +
      '    email' +
      '  }' +
      '}';

    // Créer les variables
    var Variables := TJSONObject.Create;
    try
      Variables.AddPair('id', '123');

      // Exécuter la requête
      Response := Client.ExecuteQuery(Query, Variables);

      if Assigned(Response) then
      try
        // Afficher la réponse
        Memo1.Lines.Text := Response.ToString;

        // Accéder aux données spécifiques
        var Data := Response.GetValue('data') as TJSONObject;
        var User := Data.GetValue('user') as TJSONObject;
        var Name := User.GetValue('name').Value;

        ShowMessage('Nom de l''utilisateur: ' + Name);
      finally
        Response.Free;
      end;
    finally
      Variables.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

## Exemple concret : Application de liste de tâches avec GraphQL

Créons maintenant une application plus complète qui gère une liste de tâches en utilisant une API GraphQL :

### Étape 1 : Définir les requêtes GraphQL

```pascal
unit TaskQueries;

interface

const
  // Requête pour obtenir toutes les tâches
  QUERY_GET_TASKS =
    'query GetTasks {' +
    '  tasks {' +
    '    id' +
    '    title' +
    '    completed' +
    '  }' +
    '}';

  // Requête pour obtenir une tâche spécifique
  QUERY_GET_TASK =
    'query GetTask($id: ID!) {' +
    '  task(id: $id) {' +
    '    id' +
    '    title' +
    '    description' +
    '    completed' +
    '    dueDate' +
    '  }' +
    '}';

  // Mutation pour créer une nouvelle tâche
  MUTATION_CREATE_TASK =
    'mutation CreateTask($title: String!, $description: String, $dueDate: String) {' +
    '  createTask(input: {title: $title, description: $description, dueDate: $dueDate}) {' +
    '    id' +
    '    title' +
    '    completed' +
    '  }' +
    '}';

  // Mutation pour mettre à jour une tâche
  MUTATION_UPDATE_TASK =
    'mutation UpdateTask($id: ID!, $completed: Boolean!) {' +
    '  updateTask(id: $id, input: {completed: $completed}) {' +
    '    id' +
    '    completed' +
    '  }' +
    '}';

  // Mutation pour supprimer une tâche
  MUTATION_DELETE_TASK =
    'mutation DeleteTask($id: ID!) {' +
    '  deleteTask(id: $id) {' +
    '    success' +
    '  }' +
    '}';

implementation

end.
```

### Étape 2 : Créer un gestionnaire de tâches

```pascal
unit TaskManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  GraphQLClient, TaskQueries;

type
  TTask = record
    ID: string;
    Title: string;
    Description: string;
    Completed: Boolean;
    DueDate: TDateTime;

    procedure FromJSON(const JSON: TJSONObject);
    function ToJSON: TJSONObject;
  end;

  TTaskList = TArray<TTask>;

  TTaskManager = class
  private
    FGraphQLClient: TGraphQLClient;
  public
    constructor Create(const GraphQLEndpoint: string);
    destructor Destroy; override;

    function GetAllTasks: TTaskList;
    function GetTask(const ID: string): TTask;
    function CreateTask(const Title, Description: string; DueDate: TDateTime): string; // Retourne l'ID
    function UpdateTaskStatus(const ID: string; Completed: Boolean): Boolean;
    function DeleteTask(const ID: string): Boolean;
  end;

implementation

{ TTask }

procedure TTask.FromJSON(const JSON: TJSONObject);
begin
  ID := JSON.GetValue('id').Value;
  Title := JSON.GetValue('title').Value;

  // La description peut être null
  if JSON.GetValue('description') <> nil then
    Description := JSON.GetValue('description').Value
  else
    Description := '';

  Completed := (JSON.GetValue('completed') as TJSONBool).AsBoolean;

  // La date d'échéance peut être null
  if JSON.GetValue('dueDate') <> nil then
    DueDate := ISO8601ToDate(JSON.GetValue('dueDate').Value)
  else
    DueDate := 0;
end;

function TTask.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', ID);
  Result.AddPair('title', Title);

  if Description <> '' then
    Result.AddPair('description', Description);

  Result.AddPair('completed', TJSONBool.Create(Completed));

  if DueDate > 0 then
    Result.AddPair('dueDate', DateToISO8601(DueDate));
end;

{ TTaskManager }

constructor TTaskManager.Create(const GraphQLEndpoint: string);
begin
  inherited Create;
  FGraphQLClient := TGraphQLClient.Create(GraphQLEndpoint);
end;

destructor TTaskManager.Destroy;
begin
  FGraphQLClient.Free;
  inherited;
end;

function TTaskManager.GetAllTasks: TTaskList;
var
  Response: TJSONObject;
  Data, TasksArray: TJSONValue;
  I: Integer;
begin
  SetLength(Result, 0);

  Response := FGraphQLClient.ExecuteQuery(QUERY_GET_TASKS);
  if not Assigned(Response) then
    Exit;

  try
    Data := Response.GetValue('data');
    if not Assigned(Data) then
      Exit;

    TasksArray := (Data as TJSONObject).GetValue('tasks');
    if not (TasksArray is TJSONArray) then
      Exit;

    SetLength(Result, (TasksArray as TJSONArray).Count);

    for I := 0 to (TasksArray as TJSONArray).Count - 1 do
    begin
      Result[I].FromJSON((TasksArray as TJSONArray).Items[I] as TJSONObject);
    end;
  finally
    Response.Free;
  end;
end;

function TTaskManager.GetTask(const ID: string): TTask;
var
  Variables: TJSONObject;
  Response: TJSONObject;
  Data, TaskObj: TJSONValue;
begin
  // Initialiser le résultat
  Result.ID := '';

  // Créer les variables
  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', ID);

    // Exécuter la requête
    Response := FGraphQLClient.ExecuteQuery(QUERY_GET_TASK, Variables);
    if not Assigned(Response) then
      Exit;

    try
      Data := Response.GetValue('data');
      if not Assigned(Data) then
        Exit;

      TaskObj := (Data as TJSONObject).GetValue('task');
      if not (TaskObj is TJSONObject) then
        Exit;

      // Remplir l'objet tâche
      Result.FromJSON(TaskObj as TJSONObject);
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TTaskManager.CreateTask(const Title, Description: string; DueDate: TDateTime): string;
var
  Variables: TJSONObject;
  Response: TJSONObject;
  Data, TaskObj: TJSONValue;
begin
  Result := '';

  // Créer les variables
  Variables := TJSONObject.Create;
  try
    Variables.AddPair('title', Title);

    if Description <> '' then
      Variables.AddPair('description', Description);

    if DueDate > 0 then
      Variables.AddPair('dueDate', DateToISO8601(DueDate));

    // Exécuter la mutation
    Response := FGraphQLClient.ExecuteMutation(MUTATION_CREATE_TASK, Variables);
    if not Assigned(Response) then
      Exit;

    try
      Data := Response.GetValue('data');
      if not Assigned(Data) then
        Exit;

      TaskObj := (Data as TJSONObject).GetValue('createTask');
      if not (TaskObj is TJSONObject) then
        Exit;

      // Récupérer l'ID de la tâche créée
      Result := (TaskObj as TJSONObject).GetValue('id').Value;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TTaskManager.UpdateTaskStatus(const ID: string; Completed: Boolean): Boolean;
var
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  Result := False;

  // Créer les variables
  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', ID);
    Variables.AddPair('completed', TJSONBool.Create(Completed));

    // Exécuter la mutation
    Response := FGraphQLClient.ExecuteMutation(MUTATION_UPDATE_TASK, Variables);
    if not Assigned(Response) then
      Exit;

    try
      // Si nous avons une réponse, considérer que la mise à jour a réussi
      Result := True;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TTaskManager.DeleteTask(const ID: string): Boolean;
var
  Variables: TJSONObject;
  Response: TJSONObject;
  Data, DeleteResult: TJSONValue;
begin
  Result := False;

  // Créer les variables
  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', ID);

    // Exécuter la mutation
    Response := FGraphQLClient.ExecuteMutation(MUTATION_DELETE_TASK, Variables);
    if not Assigned(Response) then
      Exit;

    try
      Data := Response.GetValue('data');
      if not Assigned(Data) then
        Exit;

      DeleteResult := (Data as TJSONObject).GetValue('deleteTask');
      if not (DeleteResult is TJSONObject) then
        Exit;

      // Vérifier si la suppression a réussi
      if (DeleteResult as TJSONObject).GetValue('success') <> nil then
        Result := ((DeleteResult as TJSONObject).GetValue('success') as TJSONBool).AsBoolean;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

end.
```

### Étape 3 : Créer l'interface utilisateur

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls,
  Vcl.ComCtrls, TaskManager;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    btnAddTask: TButton;
    edtNewTask: TEdit;
    clbTasks: TCheckListBox;
    btnRefresh: TButton;
    btnDelete: TButton;
    btnDetails: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnAddTaskClick(Sender: TObject);
    procedure clbTasksClickCheck(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
  private
    FTaskManager: TTaskManager;
    FTasks: TTaskList;
    procedure RefreshTaskList;
    function GetSelectedTaskIndex: Integer;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  TaskDetailForm;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Remplacer par votre endpoint GraphQL
  FTaskManager := TTaskManager.Create('https://api.example.com/graphql');

  // Charger les tâches initiales
  RefreshTaskList;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FTaskManager.Free;
end;

procedure TForm1.RefreshTaskList;
var
  I: Integer;
begin
  StatusBar1.SimpleText := 'Chargement des tâches...';
  Screen.Cursor := crHourGlass;
  try
    // Obtenir les tâches
    FTasks := FTaskManager.GetAllTasks;

    // Mettre à jour l'interface
    clbTasks.Items.Clear;
    for I := 0 to Length(FTasks) - 1 do
    begin
      clbTasks.Items.Add(FTasks[I].Title);
      clbTasks.Checked[I] := FTasks[I].Completed;
    end;

    StatusBar1.SimpleText := Format('%d tâches chargées', [Length(FTasks)]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TForm1.GetSelectedTaskIndex: Integer;
begin
  Result := clbTasks.ItemIndex;
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  RefreshTaskList;
end;

procedure TForm1.btnAddTaskClick(Sender: TObject);
var
  Title: string;
  TaskID: string;
begin
  Title := Trim(edtNewTask.Text);
  if Title = '' then
  begin
    ShowMessage('Veuillez entrer un titre pour la tâche');
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    // Créer la tâche
    TaskID := FTaskManager.CreateTask(Title, '', 0);

    if TaskID <> '' then
    begin
      // Effacer le champ de texte
      edtNewTask.Text := '';

      // Actualiser la liste
      RefreshTaskList;

      StatusBar1.SimpleText := 'Tâche créée avec succès';
    end
    else
      StatusBar1.SimpleText := 'Erreur lors de la création de la tâche';
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.clbTasksClickCheck(Sender: TObject);
var
  Index: Integer;
  Success: Boolean;
begin
  Index := GetSelectedTaskIndex;
  if (Index >= 0) and (Index < Length(FTasks)) then
  begin
    Screen.Cursor := crHourGlass;
    try
      // Mettre à jour le statut de la tâche
      Success := FTaskManager.UpdateTaskStatus(
        FTasks[Index].ID,
        clbTasks.Checked[Index]
      );

      if Success then
      begin
        // Mettre à jour notre liste locale
        FTasks[Index].Completed := clbTasks.Checked[Index];

        if clbTasks.Checked[Index] then
          StatusBar1.SimpleText := 'Tâche marquée comme terminée'
        else
          StatusBar1.SimpleText := 'Tâche marquée comme non terminée';
      end
      else
      begin
        // Restaurer l'état précédent
        clbTasks.Checked[Index] := FTasks[Index].Completed;
        StatusBar1.SimpleText := 'Erreur lors de la mise à jour de la tâche';
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.btnDeleteClick(Sender: TObject);
var
  Index: Integer;
  Success: Boolean;
begin
  Index := GetSelectedTaskIndex;
  if (Index >= 0) and (Index < Length(FTasks)) then
  begin
    if MessageDlg('Êtes-vous sûr de vouloir supprimer cette tâche ?',
                 mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Screen.Cursor := crHourGlass;
      try
        // Supprimer la tâche
        Success := FTaskManager.DeleteTask(FTasks[Index].ID);

        if Success then
        begin
          // Actualiser la liste
          RefreshTaskList;
          StatusBar1.SimpleText := 'Tâche supprimée avec succès';
        end
        else
          StatusBar1.SimpleText := 'Erreur lors de la suppression de la tâche';
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  end;
end;

procedure TForm1.btnDetailsClick(Sender: TObject);
var
  Index: Integer;
  DetailForm: TTaskDetailForm;
  FullTask: TTask;
begin
  Index := GetSelectedTaskIndex;
  if (Index >= 0) and (Index < Length(FTasks)) then
  begin
    Screen.Cursor := crHourGlass;
    try
      // Charger les détails complets de la tâche
      FullTask := FTaskManager.GetTask(FTasks[Index].ID);

      if FullTask.ID <> '' then
      begin
        // Créer et afficher le formulaire de détails
        DetailForm := TTaskDetailForm.Create(Self);
        try
          DetailForm.LoadTask(FullTask);
          DetailForm.ShowModal;
        finally
          DetailForm.Free;
        end;
      end
      else
        StatusBar1.SimpleText := 'Erreur lors du chargement des détails de la tâche';
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

end.
```

## Autres types d'API modernes

### gRPC (Google Remote Procedure Call)

gRPC est un framework RPC (Remote Procedure Call) développé par Google qui utilise HTTP/2 pour le transport et Protocol Buffers pour la sérialisation.

```pascal
// REMARQUE : Nécessite un support tiers pour Delphi
// Exemple conceptuel d'utilisation de gRPC

// Définition du service dans un fichier .proto
// service TaskService {
//   rpc GetTasks(GetTasksRequest) returns (GetTasksResponse);
//   rpc CreateTask(CreateTaskRequest) returns (Task);
// }

// Utilisation dans Delphi (avec un wrapper généré)
var
  TaskClient: TTaskServiceClient;
  Request: TGetTasksRequest;
  Response: TGetTasksResponse;
begin
  TaskClient := TTaskServiceClient.Create('localhost:50051');
  try
    Request := TGetTasksRequest.Create;
    try
      Response := TaskClient.GetTasks(Request);
      // Traiter la réponse
    finally
      Request.Free;
    end;
  finally
    TaskClient.Free;
  end;
end;
```

### APIs WebSocket

Les WebSockets permettent une communication bidirectionnelle en temps réel :

```pascal
// Exemple d'utilisation des WebSockets
procedure TForm1.ConnecterWebSocket;
var
  WebSocket: TsgcWebSocketClient;
begin
  WebSocket := TsgcWebSocketClient.Create(nil);
  try
    WebSocket.URL := 'wss://api.example.com/ws';
    WebSocket.OnMessage := WebSocketMessage;
    WebSocket.OnConnect := WebSocketConnect;
    WebSocket.Active := True;
  except
    WebSocket.Free;
    raise;
  end;
end;

procedure TForm1.WebSocketConnect(Sender: TObject);
begin
  // Envoyer un message d'identification
  (Sender as TsgcWebSocketClient).WriteData(
    '{"type": "identify", "userId": "123"}'
  );
end;

procedure TForm1.WebSocketMessage(Sender: TObject; const Text: string);
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
  try
    // Traiter le message reçu
    if JSON.GetValue('type').Value = 'taskUpdate' then
    begin
      // Mettre à jour l'interface utilisateur
      RefreshTaskList;
    end;
  finally
    JSON.Free;
  end;
end;
```

## Considérations de performance

### Optimisation des requêtes GraphQL

1. **Demandez uniquement ce dont vous avez besoin** :

```graphql
# Mauvais exemple - récupère trop de données
{
  users {
    id
    name
    email
    phone
    address
    profilePicture
    orders {
      id
      date
      products {
        id
        name
        price
      }
    }
  }
}

# Bon exemple - récupère uniquement les données nécessaires
{
  users {
    id
    name
    email
  }
}
```

2. **Utilisez les fragments pour réutiliser des parties de requêtes** :

```graphql
fragment UserBasicInfo on User {
  id
  name
  email
}

query GetUsers {
  users {
    ...UserBasicInfo
  }
}

query GetUser($id: ID!) {
  user(id: $id) {
    ...UserBasicInfo
    phone
    address
  }
}
```

### Gestion du cache

Implémentez un système de cache pour éviter des requêtes répétitives :

```pascal
unit GraphQLCache;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON;

type
  TCacheEntry = record
    Data: TJSONObject;
    Expiration: TDateTime;
  end;

  TGraphQLCache = class
  private
    FCache: TDictionary<string, TCacheEntry>;
    FDefaultExpiration: Integer; // Secondes
  public
    constructor Create(DefaultExpiration: Integer = 300); // 5 minutes par défaut
    destructor Destroy; override;

    procedure StoreResult(const QueryKey: string; Data: TJSONObject;
                         ExpirationSeconds: Integer = -1);
    function GetResult(const QueryKey: string): TJSONObject;
    procedure Clear;
    procedure ClearExpired;
  end;

implementation

{ TGraphQLCache }

constructor TGraphQLCache.Create(DefaultExpiration: Integer);
begin
  inherited Create;
  FCache := TDictionary<string, TCacheEntry>.Create;
  FDefaultExpiration := DefaultExpiration;
end;

destructor TGraphQLCache.Destroy;
begin
  Clear;
  FCache.Free;
  inherited;
end;

procedure TGraphQLCache.StoreResult(const QueryKey: string; Data: TJSONObject;
  ExpirationSeconds: Integer);
var
  Entry: TCacheEntry;
  ExpirationTime: Integer;
begin
  // Supprimer l'entrée existante si elle existe
  if FCache.ContainsKey(QueryKey) then
  begin
    FCache[QueryKey].Data.Free;
    FCache.Remove(QueryKey);
  end;

  // Définir le délai d'expiration
  if ExpirationSeconds < 0 then
    ExpirationTime := FDefaultExpiration
  else
    ExpirationTime := ExpirationSeconds;

  // Créer la nouvelle entrée de cache
  Entry.Data := Data.Clone as TJSONObject;
  Entry.Expiration := Now + (ExpirationTime / 86400); // Convertir secondes en jours

  // Stocker l'entrée
  FCache.Add(QueryKey, Entry);
end;

function TGraphQLCache.GetResult(const QueryKey: string): TJSONObject;
var
  Entry: TCacheEntry;
begin
  Result := nil;

  if FCache.TryGetValue(QueryKey, Entry) then
  begin
    // Vérifier si l'entrée est expirée
    if Now > Entry.Expiration then
    begin
      // Supprimer l'entrée expirée
      Entry.Data.Free;
      FCache.Remove(QueryKey);
    end
    else
    begin
      // Retourner une copie des données
      Result := Entry.Data.Clone as TJSONObject;
    end;
  end;
end;

procedure TGraphQLCache.Clear;
var
  Entry: TCacheEntry;
begin
  for Entry in FCache.Values do
    Entry.Data.Free;

  FCache.Clear;
end;

procedure TGraphQLCache.ClearExpired;
var
  Keys: TArray<string>;
  Key: string;
  Entry: TCacheEntry;
begin
  // Obtenir toutes les clés
  Keys := FCache.Keys.ToArray;

  // Supprimer les entrées expirées
  for Key in Keys do
  begin
    if FCache.TryGetValue(Key, Entry) then
    begin
      if Now > Entry.Expiration then
      begin
        Entry.Data.Free;
        FCache.Remove(Key);
      end;
    end;
  end;
end;

end.
```

### Intégration du cache avec le client GraphQL

Maintenant, modifions notre client GraphQL pour utiliser ce cache :

```pascal
unit GraphQLClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types,
  GraphQLCache;

type
  TGraphQLClient = class
  private
    FRESTClient: TRESTClient;
    FEndpoint: string;
    FHeaders: TStrings;
    FOnError: TProc<string>;
    FCache: TGraphQLCache;
    FUseCaching: Boolean;

    function CreateRequest: TRESTRequest;
    function WrapQuery(const Query: string; Variables: TJSONObject = nil): string;
    function GenerateCacheKey(const Query: string; Variables: TJSONObject): string;
  public
    constructor Create(const Endpoint: string);
    destructor Destroy; override;

    procedure AddHeader(const Name, Value: string);
    function ExecuteQuery(const Query: string; Variables: TJSONObject = nil;
                         SkipCache: Boolean = False): TJSONObject;
    function ExecuteMutation(const Mutation: string; Variables: TJSONObject = nil): TJSONObject;
    procedure ClearCache;

    property Headers: TStrings read FHeaders;
    property OnError: TProc<string> read FOnError write FOnError;
    property UseCaching: Boolean read FUseCaching write FUseCaching default True;
  end;

implementation

uses
  System.Hash;

{ TGraphQLClient }

constructor TGraphQLClient.Create(const Endpoint: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FHeaders := TStringList.Create;
  FCache := TGraphQLCache.Create;
  FUseCaching := True;

  // Créer le client REST
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;

  // Par défaut, on ajoute l'entête Content-Type pour GraphQL
  AddHeader('Content-Type', 'application/json');
end;

destructor TGraphQLClient.Destroy;
begin
  FRESTClient.Free;
  FHeaders.Free;
  FCache.Free;
  inherited;
end;

function TGraphQLClient.GenerateCacheKey(const Query: string; Variables: TJSONObject): string;
var
  CacheKeyStr: string;
begin
  CacheKeyStr := Query;

  if Assigned(Variables) and (Variables.Count > 0) then
    CacheKeyStr := CacheKeyStr + '_' + Variables.ToString;

  // Générer une clé de hachage unique pour cette requête et ses variables
  Result := THashSHA2.GetHashString(CacheKeyStr, THashSHA2.TSHA2Version.SHA256);
end;

function TGraphQLClient.ExecuteQuery(const Query: string; Variables: TJSONObject;
  SkipCache: Boolean): TJSONObject;
var
  Request: TRESTRequest;
  Response: TRESTResponse;
  RequestBody: string;
  ResponseObj: TJSONObject;
  CacheKey: string;
begin
  Result := nil;

  // Vérifier le cache si activé et si ce n'est pas une requête qui ignore le cache
  if FUseCaching and not SkipCache then
  begin
    CacheKey := GenerateCacheKey(Query, Variables);
    Result := FCache.GetResult(CacheKey);

    if Assigned(Result) then
      Exit; // Résultat trouvé dans le cache, on le retourne directement
  end;

  // Si on arrive ici, soit le cache est désactivé, soit le résultat n'est pas dans le cache
  Request := CreateRequest;
  Response := TRESTResponse.Create(nil);

  try
    Request.Response := Response;

    // Préparer le corps de la requête
    RequestBody := WrapQuery(Query, Variables);
    Request.Body.Add(RequestBody, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter la requête
    Request.Execute;

    // Traiter la réponse
    if (Response.StatusCode = 200) then
    begin
      ResponseObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;

      // Vérifier s'il y a des erreurs
      if ResponseObj.GetValue('errors') <> nil then
      begin
        if Assigned(FOnError) then
          FOnError(Response.Content);
        ResponseObj.Free;
      end
      else
      begin
        Result := ResponseObj;

        // Stocker dans le cache si le caching est activé
        if FUseCaching and not SkipCache then
        begin
          CacheKey := GenerateCacheKey(Query, Variables);
          FCache.StoreResult(CacheKey, Result);
        end;
      end;
    end
    else
    begin
      if Assigned(FOnError) then
        FOnError(Format('Erreur HTTP %d: %s', [Response.StatusCode, Response.StatusText]));
    end;
  finally
    Request.Free;
    Response.Free;
  end;
end;

// Les autres méthodes restent inchangées
// ...

end.
```

## Gestion des erreurs avec GraphQL

GraphQL a une approche unique pour la gestion des erreurs. Contrairement aux API REST qui utilisent des codes d'état HTTP, GraphQL renvoie presque toujours un code 200 OK, mais inclut un champ `errors` dans la réponse s'il y a des problèmes.

### Structure d'une réponse d'erreur GraphQL

```json
{
  "errors": [
    {
      "message": "Task not found",
      "locations": [{"line": 2, "column": 3}],
      "path": ["task", "123"],
      "extensions": {
        "code": "NOT_FOUND",
        "classification": "DataFetchingException"
      }
    }
  ],
  "data": {
    "task": null
  }
}
```

### Traitement des erreurs dans Delphi

Créons une classe pour gérer ces erreurs :

```pascal
unit GraphQLErrors;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Generics.Collections;

type
  TGraphQLErrorLocation = record
    Line: Integer;
    Column: Integer;
  end;

  TGraphQLError = class
  private
    FMessage: string;
    FLocations: TArray<TGraphQLErrorLocation>;
    FPath: TArray<string>;
    FCode: string;
    FClassification: string;
  public
    constructor Create(ErrorObj: TJSONObject);

    property Message: string read FMessage;
    property Locations: TArray<TGraphQLErrorLocation> read FLocations;
    property Path: TArray<string> read FPath;
    property Code: string read FCode;
    property Classification: string read FClassification;
  end;

  TGraphQLErrorHandler = class
  private
    FErrors: TObjectList<TGraphQLError>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ParseResponse(Response: TJSONObject);
    function HasErrors: Boolean;
    function GetFirstMessage: string;
    function GetErrorsAsText: string;

    property Errors: TObjectList<TGraphQLError> read FErrors;
  end;

implementation

{ TGraphQLError }

constructor TGraphQLError.Create(ErrorObj: TJSONObject);
var
  LocationsArray: TJSONArray;
  PathArray: TJSONArray;
  ExtensionsObj: TJSONObject;
  I: Integer;
  Location: TGraphQLErrorLocation;
  PathItem: TJSONValue;
begin
  inherited Create;

  // Message est obligatoire
  FMessage := ErrorObj.GetValue('message').Value;

  // Traiter les locations (optionnel)
  if ErrorObj.TryGetValue<TJSONArray>('locations', LocationsArray) then
  begin
    SetLength(FLocations, LocationsArray.Count);
    for I := 0 to LocationsArray.Count - 1 do
    begin
      Location.Line := (LocationsArray.Items[I] as TJSONObject).GetValue<Integer>('line');
      Location.Column := (LocationsArray.Items[I] as TJSONObject).GetValue<Integer>('column');
      FLocations[I] := Location;
    end;
  end;

  // Traiter le chemin (optionnel)
  if ErrorObj.TryGetValue<TJSONArray>('path', PathArray) then
  begin
    SetLength(FPath, PathArray.Count);
    for I := 0 to PathArray.Count - 1 do
    begin
      PathItem := PathArray.Items[I];

      if PathItem is TJSONString then
        FPath[I] := PathItem.Value
      else if PathItem is TJSONNumber then
        FPath[I] := PathItem.ToString;
    end;
  end;

  // Traiter les extensions (optionnel)
  if ErrorObj.TryGetValue<TJSONObject>('extensions', ExtensionsObj) then
  begin
    if ExtensionsObj.TryGetValue<string>('code', FCode) then;
    if ExtensionsObj.TryGetValue<string>('classification', FClassification) then;
  end;
end;

{ TGraphQLErrorHandler }

constructor TGraphQLErrorHandler.Create;
begin
  inherited;
  FErrors := TObjectList<TGraphQLError>.Create(True);
end;

destructor TGraphQLErrorHandler.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TGraphQLErrorHandler.Clear;
begin
  FErrors.Clear;
end;

procedure TGraphQLErrorHandler.ParseResponse(Response: TJSONObject);
var
  ErrorsArray: TJSONArray;
  I: Integer;
  Error: TGraphQLError;
begin
  Clear;

  if not Assigned(Response) then
    Exit;

  if Response.TryGetValue<TJSONArray>('errors', ErrorsArray) then
  begin
    for I := 0 to ErrorsArray.Count - 1 do
    begin
      Error := TGraphQLError.Create(ErrorsArray.Items[I] as TJSONObject);
      FErrors.Add(Error);
    end;
  end;
end;

function TGraphQLErrorHandler.HasErrors: Boolean;
begin
  Result := FErrors.Count > 0;
end;

function TGraphQLErrorHandler.GetFirstMessage: string;
begin
  if HasErrors then
    Result := FErrors[0].Message
  else
    Result := '';
end;

function TGraphQLErrorHandler.GetErrorsAsText: string;
var
  I: Integer;
  SB: TStringBuilder;
begin
  if not HasErrors then
    Exit('');

  SB := TStringBuilder.Create;
  try
    for I := 0 to FErrors.Count - 1 do
    begin
      SB.AppendLine(Format('Erreur %d: %s', [I + 1, FErrors[I].Message]));

      if FErrors[I].Code <> '' then
        SB.AppendLine(Format('  Code: %s', [FErrors[I].Code]));

      if Length(FErrors[I].Path) > 0 then
        SB.AppendLine(Format('  Chemin: %s', [string.Join('.', FErrors[I].Path)]));

      SB.AppendLine('');
    end;

    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

end.
```

### Utilisation de notre gestionnaire d'erreurs

```pascal
procedure TForm1.ExecuterRequeteGraphQL;
var
  Client: TGraphQLClient;
  Response: TJSONObject;
  ErrorHandler: TGraphQLErrorHandler;
  Query: string;
begin
  Client := TGraphQLClient.Create('https://api.example.com/graphql');
  ErrorHandler := TGraphQLErrorHandler.Create;
  try
    // Définir une requête GraphQL
    Query :=
      'query GetUser($id: ID!) {' +
      '  user(id: $id) {' +
      '    name' +
      '    email' +
      '  }' +
      '}';

    // Créer les variables
    var Variables := TJSONObject.Create;
    try
      Variables.AddPair('id', '123');

      // Exécuter la requête
      Response := Client.ExecuteQuery(Query, Variables);

      // Vérifier les erreurs
      ErrorHandler.ParseResponse(Response);

      if ErrorHandler.HasErrors then
      begin
        ShowMessage('Des erreurs sont survenues :' + sLineBreak +
                   ErrorHandler.GetErrorsAsText);
      end
      else if Assigned(Response) then
      begin
        try
          // Afficher la réponse
          Memo1.Lines.Text := Response.ToString;

          // Accéder aux données spécifiques
          var Data := Response.GetValue('data') as TJSONObject;
          var User := Data.GetValue('user') as TJSONObject;
          var Name := User.GetValue('name').Value;

          ShowMessage('Nom de l''utilisateur: ' + Name);
        finally
          Response.Free;
        end;
      end;
    finally
      Variables.Free;
    end;
  finally
    Client.Free;
    ErrorHandler.Free;
  end;
end;
```

## Utilisation avancée de GraphQL dans Delphi

### Introspection : découvrir le schéma de l'API

GraphQL permet d'interroger le schéma de l'API grâce à l'introspection :

```pascal
procedure TForm1.ObtenirSchemaGraphQL;
var
  Client: TGraphQLClient;
  Response: TJSONObject;
  SchemaQuery: string;
begin
  Client := TGraphQLClient.Create('https://api.example.com/graphql');
  try
    // Requête d'introspection pour obtenir le schéma
    SchemaQuery :=
      'query IntrospectionQuery {' +
      '  __schema {' +
      '    types {' +
      '      name' +
      '      kind' +
      '      fields {' +
      '        name' +
      '        type {' +
      '          name' +
      '          kind' +
      '        }' +
      '      }' +
      '    }' +
      '  }' +
      '}';

    Response := Client.ExecuteQuery(SchemaQuery);

    if Assigned(Response) then
    try
      // Analyser le schéma pour comprendre l'API
      AfficherTypesDisponibles(Response);
    finally
      Response.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TForm1.AfficherTypesDisponibles(SchemaResponse: TJSONObject);
var
  Data, Schema, Types: TJSONValue;
  TypesArray: TJSONArray;
  I: Integer;
  TypeName, TypeKind: string;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Types disponibles dans l''API :');
  Memo1.Lines.Add('');

  Data := SchemaResponse.GetValue('data');
  if not Assigned(Data) then Exit;

  Schema := (Data as TJSONObject).GetValue('__schema');
  if not Assigned(Schema) then Exit;

  Types := (Schema as TJSONObject).GetValue('types');
  if not (Types is TJSONArray) then Exit;

  TypesArray := Types as TJSONArray;

  for I := 0 to TypesArray.Count - 1 do
  begin
    var TypeObj := TypesArray.Items[I] as TJSONObject;

    TypeName := TypeObj.GetValue('name').Value;
    TypeKind := TypeObj.GetValue('kind').Value;

    // Ne pas afficher les types internes de GraphQL
    if not TypeName.StartsWith('__') then
      Memo1.Lines.Add(Format('- %s (%s)', [TypeName, TypeKind]));
  end;
end;
```

### GraphQL Subscriptions

Les subscriptions GraphQL permettent d'obtenir des mises à jour en temps réel, généralement via WebSocket :

```pascal
unit GraphQLSubscription;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  sgcWebSocket_Classes, sgcWebSocket_Client;

type
  TSubscriptionMessageEvent = procedure(Sender: TObject; const Data: TJSONObject) of object;

  TGraphQLSubscription = class
  private
    FWebSocket: TsgcWebSocketClient;
    FSubscriptionId: string;
    FQuery: string;
    FVariables: TJSONObject;
    FOnMessageReceived: TSubscriptionMessageEvent;
    FOnConnected: TNotifyEvent;
    FOnDisconnected: TNotifyEvent;

    procedure WebSocketConnect(Sender: TObject);
    procedure WebSocketDisconnect(Sender: TObject);
    procedure WebSocketMessage(Sender: TObject; const Text: string);

    procedure SendInitMessage;
    procedure SendSubscriptionStart;
  public
    constructor Create(const WebSocketURL: string);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Subscribe(const SubscriptionQuery: string; Variables: TJSONObject = nil);
    procedure Unsubscribe;

    property OnMessageReceived: TSubscriptionMessageEvent read FOnMessageReceived write FOnMessageReceived;
    property OnConnected: TNotifyEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TNotifyEvent read FOnDisconnected write FOnDisconnected;
  end;

implementation

uses
  System.UITypes;

const
  MSG_TYPE_CONNECTION_INIT = 'connection_init';
  MSG_TYPE_CONNECTION_ACK = 'connection_ack';
  MSG_TYPE_START = 'start';
  MSG_TYPE_DATA = 'data';
  MSG_TYPE_STOP = 'stop';

{ TGraphQLSubscription }

constructor TGraphQLSubscription.Create(const WebSocketURL: string);
begin
  inherited Create;

  FWebSocket := TsgcWebSocketClient.Create(nil);
  FWebSocket.URL := WebSocketURL;
  FWebSocket.OnConnect := WebSocketConnect;
  FWebSocket.OnDisconnect := WebSocketDisconnect;
  FWebSocket.OnMessage := WebSocketMessage;

  // Générer un ID de souscription unique
  FSubscriptionId := TGUID.NewGuid.ToString;
  FSubscriptionId := StringReplace(FSubscriptionId, '{', '', [rfReplaceAll]);
  FSubscriptionId := StringReplace(FSubscriptionId, '}', '', [rfReplaceAll]);
end;

destructor TGraphQLSubscription.Destroy;
begin
  Disconnect;
  FWebSocket.Free;
  FVariables.Free;
  inherited;
end;

procedure TGraphQLSubscription.Connect;
begin
  if not FWebSocket.Active then
    FWebSocket.Active := True;
end;

procedure TGraphQLSubscription.Disconnect;
begin
  Unsubscribe;

  if FWebSocket.Active then
    FWebSocket.Active := False;
end;

procedure TGraphQLSubscription.WebSocketConnect(Sender: TObject);
begin
  // Envoyer le message d'initialisation
  SendInitMessage;

  if Assigned(FOnConnected) then
    FOnConnected(Self);
end;

procedure TGraphQLSubscription.WebSocketDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self);
end;

procedure TGraphQLSubscription.WebSocketMessage(Sender: TObject; const Text: string);
var
  MessageObj: TJSONObject;
  MessageType: string;
  Payload: TJSONObject;
begin
  MessageObj := TJSONObject.ParseJSONValue(Text) as TJSONObject;
  try
    if MessageObj.TryGetValue<string>('type', MessageType) then
    begin
      if MessageType = MSG_TYPE_CONNECTION_ACK then
      begin
        // Connexion acceptée, on peut démarrer la souscription
        if FQuery <> '' then
          SendSubscriptionStart;
      end
      else if MessageType = MSG_TYPE_DATA then
      begin
        // Données reçues pour notre souscription
        if MessageObj.TryGetValue<TJSONObject>('payload', Payload) then
        begin
          if Assigned(FOnMessageReceived) then
            FOnMessageReceived(Self, Payload);
        end;
      end;
    end;
  finally
    MessageObj.Free;
  end;
end;

procedure TGraphQLSubscription.SendInitMessage;
var
  InitMessage: TJSONObject;
begin
  InitMessage := TJSONObject.Create;
  try
    InitMessage.AddPair('type', MSG_TYPE_CONNECTION_INIT);
    InitMessage.AddPair('payload', TJSONObject.Create);

    FWebSocket.WriteData(InitMessage.ToString);
  finally
    InitMessage.Free;
  end;
end;

procedure TGraphQLSubscription.SendSubscriptionStart;
var
  StartMessage, PayloadObj: TJSONObject;
begin
  StartMessage := TJSONObject.Create;
  PayloadObj := TJSONObject.Create;

  try
    // Créer l'objet payload
    PayloadObj.AddPair('query', FQuery);

    if Assigned(FVariables) and (FVariables.Count > 0) then
      PayloadObj.AddPair('variables', FVariables.Clone as TJSONObject);

    // Créer le message complet
    StartMessage.AddPair('type', MSG_TYPE_START);
    StartMessage.AddPair('id', FSubscriptionId);
    StartMessage.AddPair('payload', PayloadObj);

    // Envoyer le message (PayloadObj est maintenant détenu par StartMessage)
    FWebSocket.WriteData(StartMessage.ToString);
  finally
    StartMessage.Free;
  end;
end;

procedure TGraphQLSubscription.Subscribe(const SubscriptionQuery: string;
  Variables: TJSONObject);
begin
  // Stocker la requête et les variables
  FQuery := SubscriptionQuery;

  FVariables.Free;
  if Assigned(Variables) then
    FVariables := Variables.Clone as TJSONObject
  else
    FVariables := nil;

  // Si déjà connecté, démarrer la souscription
  if FWebSocket.Active then
    SendSubscriptionStart
  else
    Connect; // La souscription sera démarrée après la connexion
end;

procedure TGraphQLSubscription.Unsubscribe;
var
  StopMessage: TJSONObject;
begin
  if FWebSocket.Active and (FSubscriptionId <> '') then
  begin
    StopMessage := TJSONObject.Create;
    try
      StopMessage.AddPair('type', MSG_TYPE_STOP);
      StopMessage.AddPair('id', FSubscriptionId);

      FWebSocket.WriteData(StopMessage.ToString);
    finally
      StopMessage.Free;
    end;
  end;
end;

end.
```

### Exemple d'utilisation des souscriptions

```pascal
procedure TForm1.SubscribeToTaskChanges;
var
  Subscription: TGraphQLSubscription;
begin
  Subscription := TGraphQLSubscription.Create('wss://api.example.com/graphql');

  // Stocker la référence à la souscription pour pouvoir la libérer plus tard
  FSubscription := Subscription;

  // Définir les gestionnaires d'événements
  Subscription.OnConnected := SubscriptionConnected;
  Subscription.OnDisconnected := SubscriptionDisconnected;
  Subscription.OnMessageReceived := TaskChangesReceived;

  // Définir la requête de souscription
  Subscription.Subscribe(
    'subscription TaskChanges {' +
    '  taskChanged {' +
    '    id' +
    '    title' +
    '    completed' +
    '    action' +
    '  }' +
    '}'
  );
end;

procedure TForm1.SubscriptionConnected(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Connecté aux mises à jour en temps réel';
end;

procedure TForm1.SubscriptionDisconnected(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Déconnecté des mises à jour en temps réel';
end;

procedure TForm1.TaskChangesReceived(Sender: TObject; const Data: TJSONObject);
var
  TaskData, Task: TJSONObject;
  Action, ID: string;
begin
  // Extraire les données de la tâche
  if Data.TryGetValue<TJSONObject>('data', TaskData) then
  begin
    if TaskData.TryGetValue<TJSONObject>('taskChanged', Task) then
    begin
      ID := Task.GetValue('id').Value;
      Action := Task.GetValue('action').Value;

      // Mettre à jour l'interface utilisateur
      StatusBar1.SimpleText := Format('Tâche %s: %s', [ID, Action]);

      // Actualiser la liste des tâches
      RefreshTaskList;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Libérer la souscription
  if Assigned(FSubscription) then
  begin
    FSubscription.Disconnect;
    FSubscription.Free;
  end;
end;
```

## Nouvelles tendances dans les API

### API REST avec Hypermedia (HATEOAS)

HATEOAS (Hypermedia as the Engine of Application State) est une contrainte de l'architecture REST qui améliore la découvrabilité des API :

```pascal
procedure TForm1.AppelerAPIHateoas;
var
  Client: TRESTClient;
  Request: TRESTRequest;
  Response: TRESTResponse;
  ResponseObj, LinksObj: TJSONObject;
  Links: TStringList;
  I: Integer;
begin
  Client := TRESTClient.Create('https://api.example.com');
  Request := TRESTRequest.Create(nil);
  Response := TRESTResponse.Create(nil);
  Links := TStringList.Create;

  try
    Request.Client := Client;
    Request.Response := Response;

    // Appeler l'endpoint principal
    Request.Method := TRESTRequestMethod.rmGET;
    Request.Resource := 'tasks';
    Request.Execute;

    // Analyser la réponse pour trouver les liens hypermedia
    ResponseObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;
    try
      // Récupérer les liens disponibles
      if ResponseObj.TryGetValue<TJSONObject>('_links', LinksObj) then
      begin
        // Remplir la liste des liens disponibles
        for I := 0 to LinksObj.Count - 1 do
        begin
          Links.Add(Format('%s: %s', [
            LinksObj.Pairs[I].JsonString.Value,
            (LinksObj.Pairs[I].JsonValue as TJSONObject).GetValue('href').Value
          ]));
        end;

        // Afficher les liens disponibles
        ShowMessage('Liens disponibles dans l''API:' + sLineBreak + Links.Text);

        // On peut maintenant suivre ces liens pour naviguer dans l'API
        // Par exemple, pour accéder au lien "create"
        if LinksObj.TryGetValue<TJSONObject>('create', TJSONObject(LinksObj)) then
        begin
          var CreateLink := LinksObj.GetValue('href').Value;
          // Utiliser ce lien pour créer une nouvelle tâche...
        end;
      end;
    finally
      ResponseObj.Free;
    end;
  finally
    Client.Free;
    Request.Free;
    Response.Free;
    Links.Free;
  end;
end;
```

### API GraphQL sur WebSocket

Les API GraphQL peuvent également fonctionner sur WebSocket pour des opérations en temps réel :

```pascal
procedure TForm1.ConnecterGraphQLWebSocket;
var
  WebSocket: TsgcWebSocketClient;
  InitMessage: TJSONObject;
begin
  WebSocket := TsgcWebSocketClient.Create(nil);

  // Stocker la référence pour pouvoir la libérer plus tard
  FWebSocketClient := WebSocket;

  WebSocket.URL := 'wss://api.example.com/graphql';
  WebSocket.OnConnect := WebSocketConnected;
  WebSocket.OnMessage := WebSocketMessage;
  WebSocket.OnDisconnect := WebSocketDisconnected;

  // Se connecter
  WebSocket.Active := True;
end;

procedure TForm1.WebSocketConnected(Sender: TObject);
var
  InitMessage, Payload: TJSONObject;
begin
  // Envoyer le message d'initialisation pour le protocole GraphQL sur WebSocket
  InitMessage := TJSONObject.Create;
  Payload := TJSONObject.Create;

  try
    InitMessage.AddPair('type', 'connection_init');
    InitMessage.AddPair('payload', Payload);

    FWebSocketClient.WriteData(InitMessage.ToString);

    StatusBar1.SimpleText := 'Connecté au serveur GraphQL via WebSocket';
  finally
    InitMessage.Free;
  end;
end;

procedure TForm1.WebSocketMessage(Sender: TObject; const Text: string);
var
  Message: TJSONObject;
  MsgType: string;
begin
  try
    Message := TJSONObject.ParseJSONValue(Text) as TJSONObject;

    if Message.TryGetValue<string>('type', MsgType) then
    begin
      if MsgType = 'connection_ack' then
      begin
        // Connexion confirmée, on peut envoyer des requêtes
        EnvoyerRequeteGraphQL('query { tasks { id title completed } }');
      end
      else if MsgType = 'data' then
      begin
        // Données reçues, les traiter
        TraiterReponseGraphQL(Message.GetValue('payload') as TJSONObject);
      end;
    end;
  finally
    Message.Free;
  end;
end;

procedure TForm1.EnvoyerRequeteGraphQL(const Query: string);
var
  Message, Payload: TJSONObject;
begin
  // Préparer le message à envoyer
  Message := TJSONObject.Create;
  Payload := TJSONObject.Create;

  try
    // Identifiant unique pour cette requête
    FCurrentQueryId := Format('query_%d', [GetTickCount]);

    Payload.AddPair('query', Query);

    Message.AddPair('type', 'start');
    Message.AddPair('id', FCurrentQueryId);
    Message.AddPair('payload', Payload);

    // Envoyer la requête
    FWebSocketClient.WriteData(Message.ToString);
  finally
    Message.Free;
  end;
end;

procedure TForm1.TraiterReponseGraphQL(const Payload: TJSONObject);
var
  Data, Tasks: TJSONValue;
begin
  // Extraire les données
  if Payload.TryGetValue<TJSONObject>('data', TJSONObject(Data)) then
  begin
    if (Data as TJSONObject).TryGetValue<TJSONArray>('tasks', TJSONArray(Tasks)) then
    begin
      // Mettre à jour l'interface utilisateur avec les tâches
      AfficherTaches(Tasks as TJSONArray);
    end;
  end;
end;
```

### API basées sur JSON-RPC

JSON-RPC est un protocole d'appel de procédure à distance (RPC) léger et sans état, utilisant JSON pour l'encodage :

```pascal
unit JsonRpcClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types;

type
  TJsonRpcResponse = record
    Result: TJSONValue;
    Error: TJSONObject;
    Id: string;
    HasError: Boolean;
  end;

  TJsonRpcClient = class
  private
    FRESTClient: TRESTClient;
    FEndpoint: string;
    FNextId: Integer;

    function GenerateId: string;
  public
    constructor Create(const Endpoint: string);
    destructor Destroy; override;

    function Call(const Method: string; Params: TJSONValue = nil): TJsonRpcResponse;
    function CallBatch(const Methods: TArray<string>; Params: TArray<TJSONValue>): TArray<TJsonRpcResponse>;
  end;

implementation

{ TJsonRpcClient }

constructor TJsonRpcClient.Create(const Endpoint: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FNextId := 1;

  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := FEndpoint;
end;

destructor TJsonRpcClient.Destroy;
begin
  FRESTClient.Free;
  inherited;
end;

function TJsonRpcClient.GenerateId: string;
begin
  Result := IntToStr(FNextId);
  Inc(FNextId);
end;

function TJsonRpcClient.Call(const Method: string; Params: TJSONValue): TJsonRpcResponse;
var
  Request: TRESTRequest;
  Response: TRESTResponse;
  RequestObj, ResponseObj: TJSONObject;
begin
  // Initialiser le résultat
  Result.Result := nil;
  Result.Error := nil;
  Result.HasError := False;

  // Créer l'objet de requête JSON-RPC
  RequestObj := TJSONObject.Create;
  try
    RequestObj.AddPair('jsonrpc', '2.0');
    RequestObj.AddPair('method', Method);

    if Assigned(Params) then
      RequestObj.AddPair('params', Params)
    else
      RequestObj.AddPair('params', TJSONArray.Create);

    Result.Id := GenerateId;
    RequestObj.AddPair('id', Result.Id);

    // Préparer la requête REST
    Request := TRESTRequest.Create(nil);
    Response := TRESTResponse.Create(nil);

    try
      Request.Client := FRESTClient;
      Request.Response := Response;
      Request.Method := TRESTRequestMethod.rmPOST;

      // Ajouter le corps de la requête
      Request.Body.Add(RequestObj.ToString, TRESTContentType.ctAPPLICATION_JSON);

      // Exécuter la requête
      Request.Execute;

      // Traiter la réponse
      if Response.StatusCode = 200 then
      begin
        ResponseObj := TJSONObject.ParseJSONValue(Response.Content) as TJSONObject;

        if ResponseObj.TryGetValue<TJSONObject>('error', Result.Error) then
        begin
          Result.HasError := True;
          // Cloner l'erreur pour qu'elle survive après la libération de ResponseObj
          Result.Error := Result.Error.Clone as TJSONObject;
        end
        else if ResponseObj.TryGetValue<TJSONValue>('result', Result.Result) then
        begin
          // Cloner le résultat pour qu'il survive après la libération de ResponseObj
          Result.Result := Result.Result.Clone;
        end;

        ResponseObj.Free;
      end
      else
      begin
        // Créer une erreur pour les problèmes HTTP
        Result.HasError := True;
        Result.Error := TJSONObject.Create;
        Result.Error.AddPair('code', TJSONNumber.Create(Response.StatusCode));
        Result.Error.AddPair('message', Format('HTTP Error: %d %s',
                                             [Response.StatusCode, Response.StatusText]));
      end;
    finally
      Request.Free;
      Response.Free;
    end;
  finally
    RequestObj.Free;
  end;
end;

function TJsonRpcClient.CallBatch(const Methods: TArray<string>;
  Params: TArray<TJSONValue>): TArray<TJsonRpcResponse>;
var
  Request: TRESTRequest;
  Response: TRESTResponse;
  BatchArray: TJSONArray;
  ResponseArray: TJSONArray;
  RequestObj: TJSONObject;
  I: Integer;
begin
  // Vérifier que les tableaux ont la même taille
  if Length(Methods) <> Length(Params) then
    raise Exception.Create('Le nombre de méthodes et de paramètres doit être identique');

  // Initialiser le résultat
  SetLength(Result, Length(Methods));

  // Créer le tableau de requêtes JSON-RPC
  BatchArray := TJSONArray.Create;
  try
    for I := 0 to Length(Methods) - 1 do
    begin
      RequestObj := TJSONObject.Create;
      RequestObj.AddPair('jsonrpc', '2.0');
      RequestObj.AddPair('method', Methods[I]);

      if Assigned(Params[I]) then
        RequestObj.AddPair('params', Params[I].Clone)
      else
        RequestObj.AddPair('params', TJSONArray.Create);

      Result[I].Id := GenerateId;
      RequestObj.AddPair('id', Result[I].Id);

      BatchArray.Add(RequestObj);
    end;

    // Préparer la requête REST
    Request := TRESTRequest.Create(nil);
    Response := TRESTResponse.Create(nil);

    try
      Request.Client := FRESTClient;
      Request.Response := Response;
      Request.Method := TRESTRequestMethod.rmPOST;

      // Ajouter le corps de la requête batch
      Request.Body.Add(BatchArray.ToString, TRESTContentType.ctAPPLICATION_JSON);

      // Exécuter la requête
      Request.Execute;

      // Traiter la réponse
      if Response.StatusCode = 200 then
      begin
        ResponseArray := TJSONObject.ParseJSONValue(Response.Content) as TJSONArray;

        for I := 0 to ResponseArray.Count - 1 do
        begin
          var ResponseObj := ResponseArray.Items[I] as TJSONObject;
          var ResponseId := ResponseObj.GetValue('id').Value;

          // Trouver l'index correspondant à cet ID
          var Index := -1;
          for var J := 0 to Length(Result) - 1 do
          begin
            if Result[J].Id = ResponseId then
            begin
              Index := J;
              Break;
            end;
          end;

          if Index >= 0 then
          begin
            if ResponseObj.TryGetValue<TJSONObject>('error', Result[Index].Error) then
            begin
              Result[Index].HasError := True;
              Result[Index].Error := Result[Index].Error.Clone as TJSONObject;
            end
            else if ResponseObj.TryGetValue<TJSONValue>('result', Result[Index].Result) then
            begin
              Result[Index].Result := Result[Index].Result.Clone;
            end;
          end;
        end;

        ResponseArray.Free;
      end
      else
      begin
        // Créer une erreur pour toutes les réponses en cas de problème HTTP
        for I := 0 to Length(Result) - 1 do
        begin
          Result[I].HasError := True;
          Result[I].Error := TJSONObject.Create;
          Result[I].Error.AddPair('code', TJSONNumber.Create(Response.StatusCode));
          Result[I].Error.AddPair('message', Format('HTTP Error: %d %s',
                                                  [Response.StatusCode, Response.StatusText]));
        end;
      end;
    finally
      Request.Free;
      Response.Free;
    end;
  finally
    BatchArray.Free;
  end;
end;

end.
```

### Exemple d'utilisation de JSON-RPC

```pascal
procedure TForm1.AppelerJsonRpc;
var
  Client: TJsonRpcClient;
  Response: TJsonRpcResponse;
  Params: TJSONObject;
begin
  Client := TJsonRpcClient.Create('https://api.example.com/rpc');
  try
    // Créer les paramètres
    Params := TJSONObject.Create;
    try
      Params.AddPair('title', 'Nouvelle tâche');
      Params.AddPair('completed', TJSONBool.Create(False));

      // Appeler la méthode RPC
      Response := Client.Call('createTask', Params);

      // Traiter la réponse
      if Response.HasError then
      begin
        ShowMessage(Format('Erreur: %s', [(Response.Error.GetValue('message') as TJSONString).Value]));
        Response.Error.Free;
      end
      else if Assigned(Response.Result) then
      begin
        // Afficher le résultat
        Memo1.Lines.Text := Response.Result.ToString;
        Response.Result.Free;
      end;
    finally
      Params.Free;
    end;

    // Exemple d'appel batch (plusieurs méthodes en une seule requête)
    var Methods: TArray<string> := ['getTasks', 'getUsers'];
    var BatchParams: TArray<TJSONValue> := [nil, nil];

    var BatchResponses := Client.CallBatch(Methods, BatchParams);

    // Traiter les réponses du batch
    for var I := 0 to Length(BatchResponses) - 1 do
    begin
      if not BatchResponses[I].HasError and Assigned(BatchResponses[I].Result) then
      begin
        Memo1.Lines.Add(Format('Résultat de %s:', [Methods[I]]));
        Memo1.Lines.Add(BatchResponses[I].Result.ToString);
        BatchResponses[I].Result.Free;
      end
      else if BatchResponses[I].HasError then
      begin
        Memo1.Lines.Add(Format('Erreur pour %s: %s',
                             [Methods[I], BatchResponses[I].Error.GetValue('message').Value]));
        BatchResponses[I].Error.Free;
      end;
    end;
  finally
    Client.Free;
  end;
end;
```

## Comparaison des différentes approches d'API

### REST vs GraphQL vs JSON-RPC

| Caractéristique | REST | GraphQL | JSON-RPC |
|-----------------|------|---------|----------|
| **Principe** | Ressources et état | Requêtes précises | Appels de procédure |
| **Endpoints** | Multiples | Un seul | Un seul |
| **Format des requêtes** | URL + paramètres | Langage de requête | Objets JSON |
| **Sur/sous-récupération** | Fréquente | Évitée | Dépend de l'implémentation |
| **Cache HTTP** | Natif | Difficile | Difficile |
| **Versionnage** | Souvent nécessaire | Évolution sans version | Souvent nécessaire |
| **Documentation** | OpenAPI/Swagger | Introspection | Varie |
| **Utilisation courante** | Applications générales | Applications complexes avec besoins précis | Services backend |

### Comment choisir la bonne API pour votre application Delphi

- **REST** : Idéal pour les applications simples et lorsque vous avez besoin de mise en cache HTTP
- **GraphQL** : Parfait pour les applications complexes nécessitant des données précises et évolutives
- **JSON-RPC** : Adapté aux communications système à système et aux architectures orientées services

## Bonnes pratiques pour travailler avec les API modernes

### 1. Gestion des tokens et de l'authentification

```pascal
type
  TAuthManager = class
  private
    FAccessToken: string;
    FRefreshToken: string;
    FTokenExpiry: TDateTime;
    FClientId: string;
    FClientSecret: string;
    FOnTokenRefreshed: TNotifyEvent;

    function IsTokenExpired: Boolean;
    function RefreshAccessToken: Boolean;
  public
    constructor Create(const AClientId, AClientSecret: string);

    function GetValidToken: string;
    function Login(const Username, Password: string): Boolean;
    procedure Logout;

    property OnTokenRefreshed: TNotifyEvent read FOnTokenRefreshed write FOnTokenRefreshed;
  end;
```

### 2. Gestion des erreurs et retries

```pascal
function TApiClient.ExecuteWithRetry(
  ExecuteFunc: TFunc<Boolean>; MaxRetries: Integer = 3): Boolean;
var
  RetryCount: Integer;
  ShouldRetry: Boolean;
  WaitTime: Integer;
begin
  RetryCount := 0;
  Result := False;

  repeat
    try
      Result := ExecuteFunc();
      ShouldRetry := False;
    except
      on E: ENetworkError do
      begin
        // Problème réseau, on peut réessayer
        Inc(RetryCount);
        ShouldRetry := RetryCount < MaxRetries;

        if ShouldRetry then
        begin
          // Attendre de plus en plus longtemps entre les tentatives (backoff exponentiel)
          WaitTime := 500 * (1 shl RetryCount); // 500ms, 1s, 2s, 4s, etc.
          Sleep(WaitTime);
        end
        else
          raise; // Plus de tentatives, on propage l'erreur
      end;
      on E: Exception do
        raise; // Pour les autres erreurs, on ne réessaie pas
    end;
  until Result or not ShouldRetry;
end;
```

### 3. Mise en cache intelligente

```pascal
type
  TCacheStrategy = (csDefault, csNoCache, csRefreshCache);

function TApiClient.FetchData(
  const Endpoint: string;
  CacheStrategy: TCacheStrategy = csDefault): TJSONValue;
var
  CacheKey: string;
  CachedData: TJSONValue;
begin
  // Générer une clé de cache basée sur l'endpoint
  CacheKey := GenerateCacheKey(Endpoint);

  case CacheStrategy of
    csNoCache:
      // Ignorer le cache et rafraîchir les données
      Result := FetchFromApi(Endpoint);

    csRefreshCache:
      begin
        // Récupérer les données et mettre à jour le cache
        Result := FetchFromApi(Endpoint);
        FCache.StoreData(CacheKey, Result);
      end;

    csDefault:
      begin
        // Vérifier le cache d'abord
        CachedData := FCache.GetData(CacheKey);

        if Assigned(CachedData) then
          Result := CachedData
        else
        begin
          // Rien dans le cache, récupérer les données et les stocker
          Result := FetchFromApi(Endpoint);
          FCache.StoreData(CacheKey, Result);
        end;
      end;
  end;
end;
```

### 4. Surveillance et journalisation des appels API

```pascal
type
  TApiLogEntry = record
    Timestamp: TDateTime;
    Endpoint: string;
    Method: string;
    RequestPayload: string;
    ResponsePayload: string;
    StatusCode: Integer;
    DurationMs: Int64;
  end;

procedure TApiLogger.LogApiCall(const LogEntry: TApiLogEntry);
var
  LogText: string;
begin
  LogText := Format('[%s] %s %s - %d ms - Status: %d', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', LogEntry.Timestamp),
    LogEntry.Method,
    LogEntry.Endpoint,
    LogEntry.DurationMs,
    LogEntry.StatusCode
  ]);

  // Ajouter au fichier journal
  FLogFile.Add(LogText);

  // Analyser les performances
  if LogEntry.DurationMs > 1000 then
    AddToSlowCallsReport(LogEntry);

  // Enregistrer les erreurs
  if LogEntry.StatusCode >= 400 then
    AddToErrorReport(LogEntry);
end;
```

## Exercice pratique : Créer un client d'API météo

Voici un exemple d'application complète utilisant GraphQL pour récupérer les données météo :

### 1. Interface de l'application

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  GraphQLClient;

type
  TForm1 = class(TForm)
    pnlTop: TPanel;
    edtCity: TEdit;
    btnSearch: TButton;
    pnlWeather: TPanel;
    lblCity: TLabel;
    lblTemperature: TLabel;
    lblCondition: TLabel;
    lblHumidity: TLabel;
    lblWind: TLabel;
    btnRefresh: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    FGraphQLClient: TGraphQLClient;
    FCurrentCity: string;

    procedure QueryWeatherData(const City: string);
    procedure DisplayWeatherData(const WeatherData: TJSONObject);
    procedure HandleError(const ErrorMessage: string);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
  WEATHER_QUERY =
    'query GetWeather($city: String!) {' +
    '  weather(city: $city) {' +
    '    city' +
    '    temperature' +
    '    condition' +
    '    humidity' +
    '    windSpeed' +
    '    windDirection' +
    '    lastUpdated' +
    '  }' +
    '}';

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGraphQLClient := TGraphQLClient.Create('https://api.weather.example.com/graphql');
  FGraphQLClient.OnError := HandleError;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FGraphQLClient.Free;
end;

procedure TForm1.btnSearchClick(Sender: TObject);
var
  City: string;
begin
  City := Trim(edtCity.Text);

  if City = '' then
  begin
    ShowMessage('Veuillez entrer une ville');
    Exit;
  end;

  FCurrentCity := City;
  QueryWeatherData(City);
end;

procedure TForm1.btnRefreshClick(Sender: TObject);
begin
  if FCurrentCity <> '' then
    QueryWeatherData(FCurrentCity);
end;

procedure TForm1.QueryWeatherData(const City: string);
var
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  StatusBar1.SimpleText := 'Chargement des données météo...';
  Screen.Cursor := crHourGlass;

  try
    // Créer les variables pour la requête
    Variables := TJSONObject.Create;
    try
      Variables.AddPair('city', City);

      // Exécuter la requête GraphQL
      Response := FGraphQLClient.ExecuteQuery(WEATHER_QUERY, Variables);

      if Assigned(Response) then
      try
        // Extraire les données météo
        var Data := Response.GetValue('data') as TJSONObject;
        var Weather := Data.GetValue('weather') as TJSONObject;

        // Afficher les données
        DisplayWeatherData(Weather);

        // Mettre à jour la barre d'état
        StatusBar1.SimpleText := Format('Données mises à jour le %s', [
          Weather.GetValue('lastUpdated').Value
        ]);
      finally
        Response.Free;
      end;
    finally
      Variables.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.DisplayWeatherData(const WeatherData: TJSONObject);
begin
  // Afficher les données dans l'interface
  lblCity.Caption := WeatherData.GetValue('city').Value;
  lblTemperature.Caption := Format('Température: %s°C', [WeatherData.GetValue('temperature').Value]);
  lblCondition.Caption := Format('Conditions: %s', [WeatherData.GetValue('condition').Value]);
  lblHumidity.Caption := Format('Humidité: %s%%', [WeatherData.GetValue('humidity').Value]);
  lblWind.Caption := Format('Vent: %s km/h %s', [
    WeatherData.GetValue('windSpeed').Value,
    WeatherData.GetValue('windDirection').Value
  ]);

  // Rendre le panneau météo visible
  pnlWeather.Visible := True;
end;

procedure TForm1.HandleError(const ErrorMessage: string);
begin
  StatusBar1.SimpleText := 'Erreur lors de la récupération des données';
  ShowMessage('Une erreur est survenue: ' + ErrorMessage);
end;

end.
```

## Conclusion

Les API modernes comme GraphQL et les nouvelles approches REST offrent des moyens puissants et flexibles pour connecter vos applications Delphi à des services externes. Voici ce que nous avons appris dans ce chapitre :

- **GraphQL** permet de demander exactement les données dont vous avez besoin, évitant la sur-récupération et réduisant la taille des réponses
- Les **souscriptions GraphQL** permettent des mises à jour en temps réel
- **REST avec HATEOAS** améliore la découvrabilité des API
- **JSON-RPC** offre une approche simple pour les appels de procédure à distance
- La **mise en cache** des réponses API peut améliorer considérablement les performances
- La **gestion des erreurs** est cruciale pour créer des applications robustes

Chaque approche a ses forces et ses faiblesses, et le choix dépendra des besoins spécifiques de votre application. Avec Delphi, vous avez tous les outils nécessaires pour travailler efficacement avec ces différentes API.

## Ressources supplémentaires

- Documentation officielle GraphQL: [graphql.org](https://graphql.org/)
- Spécification JSON-RPC 2.0: [jsonrpc.org](https://www.jsonrpc.org/specification)
- Documentation REST avec HATEOAS: [restfulapi.net/hateoas](https://restfulapi.net/hateoas/)
- Bibliothèques tierces pour Delphi:
  - GraphQL4Delphi sur GitHub
  - REST Debugger (inclus dans Delphi)

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria.*
