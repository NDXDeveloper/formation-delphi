🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.8 GraphQL et nouvelles API

## Introduction à GraphQL

### Qu'est-ce que GraphQL ?

**GraphQL** est un langage de requête pour les API et un runtime pour exécuter ces requêtes. Développé par Facebook en 2012 et rendu open source en 2015, GraphQL révolutionne la façon dont les clients communiquent avec les serveurs.

**Analogie simple :**
Imaginez un restaurant avec deux styles de service :

**REST (Style traditionnel) :**
- Menu fixe avec des plats prédéfinis
- Vous commandez "Menu 1" et recevez entrée + plat + dessert
- Même si vous voulez seulement le plat principal
- Plusieurs allers-retours pour commander boisson, pain, etc.

**GraphQL (Style à la carte) :**
- Vous dites exactement ce que vous voulez
- "Je veux le poulet sans la sauce, des légumes et un verre d'eau"
- Tout arrive en une seule fois
- Vous ne payez/recevez que ce que vous demandez

### Problèmes résolus par GraphQL

**1. Over-fetching (Trop de données)**
```
REST: GET /users/123
→ Reçoit TOUT: id, nom, email, adresse, téléphone, historique...
Mais vous voulez juste le nom !
```

**2. Under-fetching (Pas assez de données)**
```
REST:
GET /users/123 → Données utilisateur
GET /users/123/posts → Posts de l'utilisateur
GET /users/123/friends → Amis de l'utilisateur
= 3 requêtes !
```

**3. Versionning d'API**
```
REST:
/api/v1/users (ancienne version)
/api/v2/users (nouvelle version)
→ Maintenance de plusieurs versions

GraphQL:
/graphql (une seule entrée)
→ Ajout de champs sans casser l'existant
```

### Les concepts clés

**Query (Requête)**
- Lire des données
- Équivalent de GET en REST

**Mutation**
- Modifier des données (créer, mettre à jour, supprimer)
- Équivalent de POST, PUT, DELETE en REST

**Subscription**
- Recevoir des mises à jour en temps réel
- Utilise WebSockets
- Pas d'équivalent direct en REST

**Schema (Schéma)**
- Définit les types de données disponibles
- Contrat entre client et serveur
- Auto-documenté

## Syntaxe GraphQL de base

### Structure d'une requête

**Format général :**
```graphql
query {
  nomDeLaRessource(paramètres) {
    champ1
    champ2
    champImbriqué {
      sousChamp1
      sousChamp2
    }
  }
}
```

**Exemple concret :**
```graphql
query {
  user(id: "123") {
    name
    email
    posts {
      title
      createdAt
    }
  }
}
```

**Réponse :**
```json
{
  "data": {
    "user": {
      "name": "Jean Dupont",
      "email": "jean.dupont@example.com",
      "posts": [
        {
          "title": "Mon premier article",
          "createdAt": "2024-01-15"
        },
        {
          "title": "GraphQL c'est génial",
          "createdAt": "2024-01-20"
        }
      ]
    }
  }
}
```

### Query avec arguments

**Filtrage :**
```graphql
query {
  users(limit: 10, role: "admin") {
    id
    name
    email
  }
}
```

**Tri :**
```graphql
query {
  posts(orderBy: "createdAt", direction: DESC) {
    title
    createdAt
  }
}
```

**Variables :**
```graphql
query GetUser($userId: ID!) {
  user(id: $userId) {
    name
    email
  }
}

# Variables (envoyées séparément)
{
  "userId": "123"
}
```

### Mutations

**Créer :**
```graphql
mutation {
  createUser(input: {
    name: "Marie Martin"
    email: "marie@example.com"
    age: 28
  }) {
    id
    name
    email
  }
}
```

**Mettre à jour :**
```graphql
mutation {
  updateUser(id: "123", input: {
    name: "Marie Dupont-Martin"
  }) {
    id
    name
    updatedAt
  }
}
```

**Supprimer :**
```graphql
mutation {
  deleteUser(id: "123") {
    success
    message
  }
}
```

### Subscriptions

**S'abonner à des événements :**
```graphql
subscription {
  newMessage(chatId: "456") {
    id
    text
    sender {
      name
      avatar
    }
    timestamp
  }
}
```

### Fragments (réutilisation)

**Définir un fragment :**
```graphql
fragment UserInfo on User {
  id
  name
  email
  avatar
}

query {
  user(id: "123") {
    ...UserInfo
    posts {
      title
    }
  }

  friends {
    ...UserInfo
  }
}
```

### Aliases (renommage)

```graphql
query {
  adminUser: user(id: "1") {
    name
    role
  }

  regularUser: user(id: "2") {
    name
    role
  }
}
```

**Réponse :**
```json
{
  "data": {
    "adminUser": {
      "name": "Admin",
      "role": "ADMIN"
    },
    "regularUser": {
      "name": "User",
      "role": "USER"
    }
  }
}
```

## GraphQL vs REST : Comparaison détaillée

### Exemple comparatif

**Scénario :** Afficher un utilisateur avec ses 5 derniers posts et ses amis

**Approche REST :**
```
GET /users/123
{
  "id": 123,
  "name": "Jean",
  "email": "jean@example.com",
  "address": "...",        ← Non nécessaire
  "phone": "...",          ← Non nécessaire
  "birthdate": "...",      ← Non nécessaire
  "preferences": {...}     ← Non nécessaire
}

GET /users/123/posts?limit=5
[...]

GET /users/123/friends
[...]

Total: 3 requêtes HTTP
```

**Approche GraphQL :**
```graphql
query {
  user(id: "123") {
    name
    posts(limit: 5) {
      title
      createdAt
    }
    friends {
      name
      avatar
    }
  }
}

Total: 1 requête HTTP
Seulement les données demandées
```

### Tableau comparatif

| Critère | REST | GraphQL |
|---------|------|---------|
| **Requêtes** | Multiples endpoints | Un seul endpoint |
| **Données** | Fixes par endpoint | À la demande du client |
| **Over-fetching** | Fréquent | Jamais |
| **Under-fetching** | Fréquent (N+1) | Jamais |
| **Versionning** | /v1, /v2, /v3 | Pas nécessaire |
| **Documentation** | Manuelle (Swagger) | Auto-générée (Schema) |
| **Cache HTTP** | Natif (GET) | Nécessite implémentation |
| **Courbe apprentissage** | Faible | Moyenne |
| **Complexité serveur** | Simple | Plus complexe |
| **File upload** | Simple | Plus complexe |
| **Performance** | Bonne | Excellente (moins de requêtes) |
| **Temps réel** | Polling/SSE | Subscriptions natives |

### Quand utiliser GraphQL ?

**✅ GraphQL est idéal pour :**
- Applications mobiles (économie de bande passante)
- Interfaces riches avec données complexes
- Multiples clients avec besoins différents
- Développement rapide (pas besoin de modifier le serveur)
- Données relationnelles imbriquées
- Micro-frontends

**✅ REST est préférable pour :**
- APIs simples CRUD
- Cache HTTP important
- Upload de fichiers massif
- Services publics simples
- Équipe débutante
- Intégration avec systèmes legacy

## Implémentation GraphQL avec Delphi

### Client GraphQL de base

**Classe client GraphQL :**

```pascal
unit GraphQLClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient;

type
  TGraphQLClient = class
  private
    FEndpoint: string;
    FHTTPClient: THTTPClient;
    FAuthToken: string;
  public
    constructor Create(const Endpoint: string);
    destructor Destroy; override;

    function Query(const QueryText: string; const Variables: TJSONObject = nil): TJSONObject;
    function Mutate(const MutationText: string; const Variables: TJSONObject = nil): TJSONObject;

    property AuthToken: string read FAuthToken write FAuthToken;
  end;

implementation

uses
  System.NetEncoding;

constructor TGraphQLClient.Create(const Endpoint: string);
begin
  inherited Create;
  FEndpoint := Endpoint;
  FHTTPClient := THTTPClient.Create;
end;

destructor TGraphQLClient.Destroy;
begin
  FHTTPClient.Free;
  inherited;
end;

function TGraphQLClient.Query(const QueryText: string;
  const Variables: TJSONObject): TJSONObject;
var
  RequestBody: TJSONObject;
  RequestStream: TStringStream;
  Response: IHTTPResponse;
  ResponseJSON: TJSONObject;
begin
  Result := nil;

  // Construire le corps de la requête
  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('query', QueryText);

    if Assigned(Variables) then
      RequestBody.AddPair('variables', Variables);

    // Préparer la requête HTTP
    RequestStream := TStringStream.Create(RequestBody.ToString, TEncoding.UTF8);
    try
      FHTTPClient.ContentType := 'application/json';

      // Ajouter le token d'authentification si présent
      if not FAuthToken.IsEmpty then
        FHTTPClient.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;

      // Envoyer la requête POST
      Response := FHTTPClient.Post(FEndpoint, RequestStream);

      if Response.StatusCode = 200 then
      begin
        ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;

        // Vérifier les erreurs GraphQL
        if ResponseJSON.TryGetValue<TJSONObject>('errors', Result) then
        begin
          raise Exception.Create('Erreur GraphQL: ' +
            ResponseJSON.GetValue('errors').ToString);
        end;

        // Retourner les données
        Result := ResponseJSON.GetValue<TJSONObject>('data').Clone as TJSONObject;
        ResponseJSON.Free;
      end
      else
        raise Exception.CreateFmt('Erreur HTTP: %d - %s',
          [Response.StatusCode, Response.StatusText]);

    finally
      RequestStream.Free;
    end;
  finally
    RequestBody.Free;
  end;
end;

function TGraphQLClient.Mutate(const MutationText: string;
  const Variables: TJSONObject): TJSONObject;
begin
  // Les mutations utilisent le même mécanisme que les queries
  Result := Query(MutationText, Variables);
end;

end.
```

### Utilisation du client

**Exemple 1 : Query simple**

```pascal
var
  GraphQL: TGraphQLClient;
  QueryText: string;
  Result: TJSONObject;
  UserName: string;
begin
  GraphQL := TGraphQLClient.Create('https://api.example.com/graphql');
  try
    // Définir la requête
    QueryText :=
      'query {' +
      '  user(id: "123") {' +
      '    name' +
      '    email' +
      '  }' +
      '}';

    // Exécuter
    Result := GraphQL.Query(QueryText);
    try
      // Extraire les données
      UserName := Result.GetValue<TJSONObject>('user')
                        .GetValue<string>('name');

      ShowMessage('Utilisateur: ' + UserName);
    finally
      Result.Free;
    end;

  finally
    GraphQL.Free;
  end;
end;
```

**Exemple 2 : Query avec variables**

```pascal
var
  GraphQL: TGraphQLClient;
  QueryText: string;
  Variables: TJSONObject;
  Result: TJSONObject;
begin
  GraphQL := TGraphQLClient.Create('https://api.example.com/graphql');
  try
    // Query avec variable
    QueryText :=
      'query GetUser($userId: ID!) {' +
      '  user(id: $userId) {' +
      '    name' +
      '    email' +
      '    posts {' +
      '      title' +
      '    }' +
      '  }' +
      '}';

    // Définir les variables
    Variables := TJSONObject.Create;
    try
      Variables.AddPair('userId', '123');

      // Exécuter
      Result := GraphQL.Query(QueryText, Variables);
      try
        Memo1.Text := Result.Format(2);
      finally
        Result.Free;
      end;
    finally
      Variables.Free;
    end;

  finally
    GraphQL.Free;
  end;
end;
```

**Exemple 3 : Mutation**

```pascal
var
  GraphQL: TGraphQLClient;
  MutationText: string;
  Variables: TJSONObject;
  Result: TJSONObject;
  Input: TJSONObject;
begin
  GraphQL := TGraphQLClient.Create('https://api.example.com/graphql');
  try
    MutationText :=
      'mutation CreateUser($input: UserInput!) {' +
      '  createUser(input: $input) {' +
      '    id' +
      '    name' +
      '    email' +
      '  }' +
      '}';

    // Préparer les données
    Input := TJSONObject.Create;
    Input.AddPair('name', 'Marie Martin');
    Input.AddPair('email', 'marie@example.com');
    Input.AddPair('age', TJSONNumber.Create(28));

    Variables := TJSONObject.Create;
    try
      Variables.AddPair('input', Input);

      // Exécuter la mutation
      Result := GraphQL.Mutate(MutationText, Variables);
      try
        ShowMessage('Utilisateur créé avec ID: ' +
          Result.GetValue<TJSONObject>('createUser')
                .GetValue<string>('id'));
      finally
        Result.Free;
      end;
    finally
      Variables.Free;
    end;

  finally
    GraphQL.Free;
  end;
end;
```

### Helper pour simplifier les requêtes

```pascal
unit GraphQLHelper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, GraphQLClient;

type
  TGraphQLHelper = class
  private
    FClient: TGraphQLClient;
  public
    constructor Create(const Endpoint: string);
    destructor Destroy; override;

    // Méthodes simplifiées
    function GetUser(const UserID: string): TJSONObject;
    function GetUsers(Limit: Integer = 10): TJSONArray;
    function CreateUser(const Name, Email: string): string; // Retourne l'ID
    function UpdateUser(const UserID, Name: string): Boolean;
    function DeleteUser(const UserID: string): Boolean;
  end;

implementation

constructor TGraphQLHelper.Create(const Endpoint: string);
begin
  inherited Create;
  FClient := TGraphQLClient.Create(Endpoint);
end;

destructor TGraphQLHelper.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TGraphQLHelper.GetUser(const UserID: string): TJSONObject;
var
  QueryText: string;
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  QueryText :=
    'query GetUser($id: ID!) {' +
    '  user(id: $id) {' +
    '    id' +
    '    name' +
    '    email' +
    '    createdAt' +
    '  }' +
    '}';

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', UserID);

    Response := FClient.Query(QueryText, Variables);
    try
      Result := Response.GetValue<TJSONObject>('user').Clone as TJSONObject;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TGraphQLHelper.GetUsers(Limit: Integer): TJSONArray;
var
  QueryText: string;
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  QueryText :=
    'query GetUsers($limit: Int!) {' +
    '  users(limit: $limit) {' +
    '    id' +
    '    name' +
    '    email' +
    '  }' +
    '}';

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('limit', TJSONNumber.Create(Limit));

    Response := FClient.Query(QueryText, Variables);
    try
      Result := Response.GetValue<TJSONArray>('users').Clone as TJSONArray;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TGraphQLHelper.CreateUser(const Name, Email: string): string;
var
  MutationText: string;
  Variables, Input: TJSONObject;
  Response: TJSONObject;
begin
  MutationText :=
    'mutation CreateUser($input: UserInput!) {' +
    '  createUser(input: $input) {' +
    '    id' +
    '  }' +
    '}';

  Input := TJSONObject.Create;
  Input.AddPair('name', Name);
  Input.AddPair('email', Email);

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('input', Input);

    Response := FClient.Mutate(MutationText, Variables);
    try
      Result := Response.GetValue<TJSONObject>('createUser')
                       .GetValue<string>('id');
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TGraphQLHelper.UpdateUser(const UserID, Name: string): Boolean;
var
  MutationText: string;
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  MutationText :=
    'mutation UpdateUser($id: ID!, $name: String!) {' +
    '  updateUser(id: $id, name: $name) {' +
    '    success' +
    '  }' +
    '}';

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', UserID);
    Variables.AddPair('name', Name);

    Response := FClient.Mutate(MutationText, Variables);
    try
      Result := Response.GetValue<TJSONObject>('updateUser')
                       .GetValue<Boolean>('success');
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

function TGraphQLHelper.DeleteUser(const UserID: string): Boolean;
var
  MutationText: string;
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  MutationText :=
    'mutation DeleteUser($id: ID!) {' +
    '  deleteUser(id: $id) {' +
    '    success' +
    '  }' +
    '}';

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('id', UserID);

    Response := FClient.Mutate(MutationText, Variables);
    try
      Result := Response.GetValue<TJSONObject>('deleteUser')
                       .GetValue<Boolean>('success');
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;

end.
```

## API GraphQL publiques pour tester

### GitHub GraphQL API

**Configuration :**

```pascal
procedure ConfigurerGitHubGraphQL(Client: TGraphQLClient; const Token: string);
begin
  Client.AuthToken := Token; // Personal Access Token de GitHub
  // Endpoint: https://api.github.com/graphql
end;
```

**Exemple de requête :**

```pascal
var
  GraphQL: TGraphQLClient;
  QueryText: string;
  Result: TJSONObject;
begin
  GraphQL := TGraphQLClient.Create('https://api.github.com/graphql');
  try
    GraphQL.AuthToken := 'ghp_VotreTokenPersonnel';

    QueryText :=
      'query {' +
      '  viewer {' +
      '    login' +
      '    name' +
      '    repositories(first: 5) {' +
      '      nodes {' +
      '        name' +
      '        description' +
      '        stargazerCount' +
      '      }' +
      '    }' +
      '  }' +
      '}';

    Result := GraphQL.Query(QueryText);
    try
      Memo1.Text := Result.Format(2);
    finally
      Result.Free;
    end;

  finally
    GraphQL.Free;
  end;
end;
```

### SpaceX GraphQL API

**API publique sans authentification :**

```pascal
var
  GraphQL: TGraphQLClient;
  QueryText: string;
  Result: TJSONObject;
begin
  GraphQL := TGraphQLClient.Create('https://spacex-production.up.railway.app/');
  try
    QueryText :=
      'query {' +
      '  launches(limit: 5) {' +
      '    mission_name' +
      '    launch_date_local' +
      '    launch_success' +
      '    rocket {' +
      '      rocket_name' +
      '    }' +
      '  }' +
      '}';

    Result := GraphQL.Query(QueryText);
    try
      // Afficher les résultats
      DisplayLaunches(Result.GetValue<TJSONArray>('launches'));
    finally
      Result.Free;
    end;

  finally
    GraphQL.Free;
  end;
end;

procedure DisplayLaunches(Launches: TJSONArray);
var
  i: Integer;
  Launch: TJSONObject;
begin
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Derniers lancements SpaceX:');
  Memo1.Lines.Add('');

  for i := 0 to Launches.Count - 1 do
  begin
    Launch := Launches.Items[i] as TJSONObject;

    Memo1.Lines.Add(Format('Mission: %s',
      [Launch.GetValue<string>('mission_name')]));
    Memo1.Lines.Add(Format('Date: %s',
      [Launch.GetValue<string>('launch_date_local')]));
    Memo1.Lines.Add(Format('Fusée: %s',
      [Launch.GetValue<TJSONObject>('rocket').GetValue<string>('rocket_name')]));
    Memo1.Lines.Add('');
  end;
end;
```

## Autres types d'API modernes

### gRPC

**Qu'est-ce que gRPC ?**

**gRPC** (Google Remote Procedure Call) est un framework RPC haute performance développé par Google.

**Caractéristiques :**
- Utilise Protocol Buffers (protobuf) au lieu de JSON
- Communication binaire (plus rapide que JSON)
- Support du streaming bidirectionnel
- Génération automatique de code client/serveur
- Idéal pour microservices

**Comparaison :**

| Critère | REST | GraphQL | gRPC |
|---------|------|---------|------|
| **Format** | JSON/XML | JSON | Protobuf (binaire) |
| **Transport** | HTTP/1.1 | HTTP/1.1 | HTTP/2 |
| **Streaming** | Non natif | Subscriptions | Natif |
| **Performance** | Bonne | Bonne | Excellente |
| **Taille** | Grande | Moyenne | Petite |
| **Lisibilité** | Haute | Haute | Faible (binaire) |
| **Browser** | Oui | Oui | Non direct |

**Utilisation basique avec Delphi :**

gRPC nécessite des bibliothèques spécifiques. Voici un exemple conceptuel :

```pascal
// Définition du service (.proto)
service UserService {
  rpc GetUser (UserRequest) returns (UserResponse);
  rpc ListUsers (Empty) returns (stream UserResponse);
}

message UserRequest {
  string id = 1;
}

message UserResponse {
  string id = 1;
  string name = 2;
  string email = 3;
}
```

**Note :** L'implémentation complète de gRPC en Delphi nécessite des bibliothèques tierces ou des wrappers C++.

### WebSockets (Communication temps réel)

**Qu'est-ce que WebSocket ?**

WebSocket permet une communication bidirectionnelle persistante entre client et serveur.

**Caractéristiques :**
- Connexion persistante (pas de requêtes répétées)
- Communication temps réel
- Push du serveur vers le client
- Faible latence

**Implémentation avec Indy :**

```pascal
unit WebSocketClient;

interface

uses
  System.SysUtils, System.Classes, IdHTTP, IdTCPClient, IdGlobal,
  System.NetEncoding;

type
  TWebSocketClient = class
  private
    FTCPClient: TIdTCPClient;
    FConnected: Boolean;
    FOnMessage: TProc<string>;
    procedure PerformHandshake(const URL: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(const URL: string);
    procedure Send(const Message: string);
    procedure Disconnect;

    property OnMessage: TProc<string> read FOnMessage write FOnMessage;
    property Connected: Boolean read FConnected;
  end;

implementation

uses
  System.Hash, IdSSLOpenSSL;

constructor TWebSocketClient.Create;
begin
  inherited;
  FTCPClient := TIdTCPClient.Create(nil);
  FConnected := False;
end;

destructor TWebSocketClient.Destroy;
begin
  Disconnect;
  FTCPClient.Free;
  inherited;
end;

procedure TWebSocketClient.Connect(const URL: string);
begin
  // Parser l'URL
  // ws://example.com:8080/path ou wss://example.com/path

  FTCPClient.Host := 'example.com';
  FTCPClient.Port := 8080;
  FTCPClient.Connect;

  PerformHandshake(URL);
  FConnected := True;
end;

procedure TWebSocketClient.PerformHandshake(const URL: string);
var
  Key, Accept: string;
  Request, Response: string;
begin
  // Générer une clé aléatoire
  Key := TNetEncoding.Base64.Encode(THashSHA1.GetHashString(
    TGUID.NewGuid.ToString));

  // Construire la requête de handshake
  Request :=
    'GET /socket HTTP/1.1' + #13#10 +
    'Host: example.com' + #13#10 +
    'Upgrade: websocket' + #13#10 +
    'Connection: Upgrade' + #13#10 +
    'Sec-WebSocket-Key: ' + Key + #13#10 +
    'Sec-WebSocket-Version: 13' + #13#10 +
    #13#10;

  // Envoyer
  FTCPClient.IOHandler.WriteLn(Request);

  // Lire la réponse
  Response := FTCPClient.IOHandler.AllData;

  // Vérifier le handshake
  if not Response.Contains('101 Switching Protocols') then
    raise Exception.Create('Handshake WebSocket échoué');
end;

procedure TWebSocketClient.Send(const Message: string);
var
  Frame: TBytes;
  MessageBytes: TBytes;
  i: Integer;
begin
  if not FConnected then
    raise Exception.Create('Non connecté');

  MessageBytes := TEncoding.UTF8.GetBytes(Message);

  // Construire une frame WebSocket (texte)
  SetLength(Frame, 2 + 4 + Length(MessageBytes));

  // Byte 0: FIN=1, opcode=1 (text)
  Frame[0] := $81;

  // Byte 1: MASK=1, payload length
  Frame[1] := $80 or Byte(Length(MessageBytes));

  // Masking key (4 bytes aléatoires)
  for i := 2 to 5 do
    Frame[i] := Random(256);

  // Payload masqué
  for i := 0 to High(MessageBytes) do
    Frame[6 + i] := MessageBytes[i] xor Frame[2 + (i mod 4)];

  // Envoyer
  FTCPClient.IOHandler.Write(Frame);
end;

procedure TWebSocketClient.Disconnect;
begin
  if FConnected then
  begin
    // Envoyer frame de fermeture
    FTCPClient.Disconnect;
    FConnected := False;
  end;
end;

end.
```

**Utilisation :**

```pascal
var
  WS: TWebSocketClient;
begin
  WS := TWebSocketClient.Create;
  try
    WS.OnMessage := procedure(const Msg: string)
    begin
      TThread.Synchronize(nil, procedure
      begin
        Memo1.Lines.Add('Reçu: ' + Msg);
      end);
    end;

    WS.Connect('ws://echo.websocket.org');
    WS.Send('Hello WebSocket!');

    Sleep(2000);

    WS.Disconnect;
  finally
    WS.Free;
  end;
end;
```

### Server-Sent Events (SSE)

**Qu'est-ce que SSE ?**

Server-Sent Events permet au serveur d'envoyer des mises à jour au client via HTTP.

**Caractéristiques :**
- Unidirectionnel (serveur → client)
- Plus simple que WebSocket
- Reconnexion automatique
- Basé sur HTTP standard

**Implémentation :**

```pascal
unit SSEClient;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient;

type
  TSSEClient = class
  private
    FHTTPClient: THTTPClient;
    FStream: TStream;
    FOnMessage: TProc<string>;
    FActive: Boolean;
    procedure ProcessStream;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(const URL: string);
    procedure Disconnect;

    property OnMessage: TProc<string> read FOnMessage write FOnMessage;
  end;

implementation

constructor TSSEClient.Create;
begin
  inherited;
  FHTTPClient := THTTPClient.Create;
  FActive := False;
end;

destructor TSSEClient.Destroy;
begin
  Disconnect;
  FHTTPClient.Free;
  inherited;
end;

procedure TSSEClient.Connect(const URL: string);
var
  Response: IHTTPResponse;
begin
  FActive := True;

  TThread.CreateAnonymousThread(
    procedure
    begin
      Response := FHTTPClient.Get(URL, FStream);

      while FActive do
      begin
        ProcessStream;
        Sleep(100);
      end;
    end).Start;
end;

procedure TSSEClient.ProcessStream;
var
  Reader: TStreamReader;
  Line: string;
  EventData: string;
begin
  if not Assigned(FStream) then
    Exit;

  Reader := TStreamReader.Create(FStream, TEncoding.UTF8, False);
  try
    while not Reader.EndOfStream do
    begin
      Line := Reader.ReadLine;

      if Line.StartsWith('data: ') then
      begin
        EventData := Line.Substring(6);

        if Assigned(FOnMessage) then
        begin
          TThread.Synchronize(nil, procedure
          begin
            FOnMessage(EventData);
          end);
        end;
      end;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TSSEClient.Disconnect;
begin
  FActive := False;
end;

end.
```

## Bonnes pratiques GraphQL

### 1. Utiliser des fragments pour la réutilisation

```pascal
// ❌ Répétition
QueryText :=
  'query {' +
  '  user(id: "1") { id name email avatar }' +
  '  friend(id: "2") { id name email avatar }' +
  '}';

// ✅ Avec fragment
QueryText :=
  'fragment UserFields on User {' +
  '  id name email avatar' +
  '}' +
  'query {' +
  '  user(id: "1") { ...UserFields }' +
  '  friend(id: "2") { ...UserFields }' +
  '}';
```

### 2. Gérer les erreurs GraphQL

```pascal
function SafeGraphQLQuery(Client: TGraphQLClient;
  const Query: string): TJSONObject;
var
  Response: TJSONObject;
  Errors: TJSONArray;
begin
  Result := nil;

  try
    Response := Client.Query(Query);

    // GraphQL peut retourner des données partielles avec des erreurs
    if Response.TryGetValue<TJSONArray>('errors', Errors) then
    begin
      // Logger les erreurs
      LogGraphQLErrors(Errors);

      // Décider si on continue avec les données partielles
      if Response.TryGetValue<TJSONObject>('data', Result) then
        Result := Result.Clone as TJSONObject
      else
        raise Exception.Create('Erreur GraphQL sans données');
    end
    else
    begin
      Result := Response.GetValue<TJSONObject>('data').Clone as TJSONObject;
    end;

  finally
    Response.Free;
  end;
end;
```

### 3. Paginer les résultats

```pascal
function GetUsersPaginated(Page: Integer; PageSize: Integer): TJSONArray;
var
  QueryText: string;
  Variables: TJSONObject;
  Response: TJSONObject;
begin
  QueryText :=
    'query GetUsers($limit: Int!, $offset: Int!) {' +
    '  users(limit: $limit, offset: $offset) {' +
    '    id' +
    '    name' +
    '    email' +
    '  }' +
    '}';

  Variables := TJSONObject.Create;
  try
    Variables.AddPair('limit', TJSONNumber.Create(PageSize));
    Variables.AddPair('offset', TJSONNumber.Create((Page - 1) * PageSize));

    Response := GraphQL.Query(QueryText, Variables);
    try
      Result := Response.GetValue<TJSONArray>('users').Clone as TJSONArray;
    finally
      Response.Free;
    end;
  finally
    Variables.Free;
  end;
end;
```

### 4. Mettre en cache les requêtes

```pascal
type
  TGraphQLCache = class
  private
    FCache: TDictionary<string, TJSONObject>;
    FCacheDuration: Integer; // secondes
  public
    constructor Create(CacheDuration: Integer = 300);
    destructor Destroy; override;

    function GetCached(const QueryHash: string): TJSONObject;
    procedure SetCached(const QueryHash: string; Data: TJSONObject);
    procedure Clear;
  end;

function ExecuteWithCache(const QueryText: string): TJSONObject;
var
  QueryHash: string;
begin
  QueryHash := THashSHA2.GetHashString(QueryText);

  // Vérifier le cache
  Result := Cache.GetCached(QueryHash);

  if not Assigned(Result) then
  begin
    // Exécuter la requête
    Result := GraphQL.Query(QueryText);

    // Mettre en cache
    Cache.SetCached(QueryHash, Result);
  end;
end;
```

### 5. Typer les réponses

```pascal
type
  TUser = record
    ID: string;
    Name: string;
    Email: string;
    CreatedAt: TDateTime;
  end;

function JSONToUser(JSON: TJSONObject): TUser;
begin
  Result.ID := JSON.GetValue<string>('id');
  Result.Name := JSON.GetValue<string>('name');
  Result.Email := JSON.GetValue<string>('email');
  Result.CreatedAt := ISO8601ToDate(JSON.GetValue<string>('createdAt'));
end;

function GetUser(const UserID: string): TUser;
var
  Response: TJSONObject;
begin
  Response := GraphQLHelper.GetUser(UserID);
  try
    Result := JSONToUser(Response);
  finally
    Response.Free;
  end;
end;
```

### 6. Builder pattern pour les requêtes complexes

```pascal
type
  TGraphQLQueryBuilder = class
  private
    FQuery: TStringBuilder;
    FVariables: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    function AddField(const FieldName: string): TGraphQLQueryBuilder;
    function AddNestedField(const FieldName: string;
      Fields: TArray<string>): TGraphQLQueryBuilder;
    function AddVariable(const Name, Value: string): TGraphQLQueryBuilder;

    function Build: string;
    function GetVariables: TJSONObject;
  end;

// Utilisation
var
  Builder: TGraphQLQueryBuilder;
  Query: string;
begin
  Builder := TGraphQLQueryBuilder.Create;
  try
    Query := Builder
      .AddField('id')
      .AddField('name')
      .AddNestedField('posts', ['title', 'createdAt'])
      .AddVariable('userId', '123')
      .Build;
  finally
    Builder.Free;
  end;
end;
```

## Résumé

### Points clés GraphQL

✅ **Concepts fondamentaux :**
- **Query** : Lire des données (GET)
- **Mutation** : Modifier des données (POST/PUT/DELETE)
- **Subscription** : Temps réel (WebSocket)
- **Schema** : Contrat auto-documenté

✅ **Avantages GraphQL :**
- Une seule requête pour données complexes
- Pas d'over-fetching ni under-fetching
- Pas de versionning d'API
- Documentation automatique
- Typage fort

✅ **Implémentation Delphi :**
- Client HTTP simple (TNetHTTPClient)
- Format JSON standard
- Variables pour requêtes dynamiques
- Helper classes pour simplifier

✅ **API publiques pour tester :**
- GitHub GraphQL API
- SpaceX API
- Shopify GraphQL
- Pokemon GraphQL

✅ **Autres API modernes :**
- **gRPC** : Performance maximale, binaire
- **WebSocket** : Communication bidirectionnelle
- **SSE** : Push serveur simple

✅ **Bonnes pratiques :**
- Fragments pour réutilisation
- Gestion d'erreurs robuste
- Pagination systématique
- Cache intelligent
- Typage des réponses
- Builder pattern

GraphQL révolutionne la communication client-serveur en donnant au client le contrôle total sur les données récupérées, tout en maintenant un contrat fort via le schéma. C'est l'avenir des API modernes !

⏭️ [Intégration avec les services cloud (AWS, Azure, Google Cloud)](/10-communication-et-services-reseaux/09-integration-avec-les-services-cloud.md)
