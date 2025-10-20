🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.3 Création de services REST avec Delphi

## Introduction

Les services REST (Representational State Transfer) sont devenus le standard incontournable pour la communication entre applications modernes. Que ce soit pour connecter une application mobile à un serveur, permettre à un site web d'accéder à des données, ou faire dialoguer différents systèmes, les API REST sont partout.

La bonne nouvelle ? Delphi est parfaitement équipé pour créer des services REST professionnels, performants et sécurisés. Dans cette section, nous allons explorer comment transformer votre expertise Delphi en services web modernes.

## Qu'est-ce que REST ?

### Définition simple

REST est un **style d'architecture** pour créer des services web. Imaginez REST comme un ensemble de règles qui permettent à différentes applications de communiquer via Internet de manière simple et standardisée.

**Analogie :** Pensez à REST comme le menu d'un restaurant :
- Le menu (API) liste ce qui est disponible
- Vous passez commande (requête) en indiquant ce que vous voulez
- La cuisine (serveur) prépare votre plat
- On vous sert le résultat (réponse)

### Les principes fondamentaux de REST

**1. Architecture client-serveur**
```
┌─────────────┐         ┌─────────────┐
│   Client    │ ──────→ │   Serveur   │
│  (demande)  │         │  (répond)   │
└─────────────┘ ←────── └─────────────┘
```

**2. Sans état (Stateless)**
- Chaque requête est indépendante
- Le serveur ne conserve pas d'information entre les requêtes
- Toutes les informations nécessaires sont dans la requête

**3. Ressources identifiées par URL**
```
https://api.monapp.com/clients          → Liste des clients
https://api.monapp.com/clients/123      → Client n°123
https://api.monapp.com/clients/123/commandes → Commandes du client 123
```

**4. Utilisation des verbes HTTP**
- **GET** : Récupérer des données (lecture)
- **POST** : Créer une nouvelle ressource
- **PUT** : Modifier une ressource existante
- **DELETE** : Supprimer une ressource

**5. Représentation des données (généralement JSON)**
```json
{
  "id": 123,
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean.dupont@email.com"
}
```

### Exemple concret d'API REST

Imaginons une API de gestion de livres :

```
GET    /api/livres              → Obtenir tous les livres
GET    /api/livres/5            → Obtenir le livre n°5
POST   /api/livres              → Créer un nouveau livre
PUT    /api/livres/5            → Modifier le livre n°5
DELETE /api/livres/5            → Supprimer le livre n°5
```

## Pourquoi créer des services REST avec Delphi ?

### 1. Architecture moderne

Les services REST permettent de créer une **architecture découplée** :

```
┌──────────────┐
│ Application  │
│    Web       │──┐
└──────────────┘  │
                  │    ┌──────────────┐
┌──────────────┐  │    │   Service    │    ┌──────────────┐
│ Application  │  ├───→│     REST     │───→│  Base de     │
│   Mobile     │──┘    │   (Delphi)   │    │   Données    │
└──────────────┘       └──────────────┘    └──────────────┘
                             ↑
┌──────────────┐             │
│ Application  │─────────────┘
│   Desktop    │
└──────────────┘
```

**Avantages :**
- Un seul backend pour tous vos clients
- Évolution indépendante du frontend et backend
- Réutilisation de la logique métier
- Scalabilité horizontale

### 2. Interopérabilité

Un service REST Delphi peut être consommé par :
- Applications web (JavaScript, React, Angular, Vue.js)
- Applications mobiles (iOS, Android, Flutter)
- Applications desktop (Delphi, C#, Java, Python...)
- Autres services et systèmes

### 3. Performance et fiabilité

Delphi offre :
- Excellentes performances natives
- Faible consommation mémoire
- Stabilité éprouvée
- Support multi-thread efficace

### 4. Rapidité de développement

Avec Delphi :
- Développement rapide (RAD)
- Composants prêts à l'emploi (FireDAC pour bases de données)
- Sérialisation JSON automatique
- Déploiement simple

## Technologies Delphi pour REST

### RAD Server (EMS - Enterprise Mobility Services)

**RAD Server** est la solution officielle d'Embarcadero pour créer des services REST d'entreprise.

**Caractéristiques :**
- Framework complet pour services REST
- Gestion automatique des routes
- Authentification intégrée
- Support multi-utilisateurs
- Console d'administration
- Analytics et monitoring

**Éditions Delphi :**
- Disponible dans Enterprise et Architect
- Non inclus dans Community et Professional

**Idéal pour :**
- Applications d'entreprise
- Besoins d'authentification complexe
- Projets nécessitant monitoring
- Grandes organisations

### DataSnap

**DataSnap** est la technologie historique de Delphi pour les applications multi-tiers.

**Caractéristiques :**
- Communication client-serveur
- Support REST, TCP/IP, HTTP
- Callbacks et notifications
- Compatible anciennes versions Delphi

**Limitations :**
- Moins moderne que RAD Server
- Configuration plus complexe
- Orienté communication Delphi-to-Delphi

**Idéal pour :**
- Migration d'applications existantes
- Communication entre applications Delphi
- Projets legacy

### Frameworks tiers modernes

Plusieurs frameworks tiers excellents existent :

**Horse** (Open Source)
```pascal
uses Horse;

begin
  THorse.Get('/ping',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('pong');
    end);

  THorse.Listen(9000);
end.
```

**Avantages :**
- Simple et léger
- Syntaxe moderne et élégante
- Nombreux middlewares disponibles
- Communauté active
- Gratuit et open source

**MARS Curiosity** (Open Source)
```pascal
[Path('/hello')]
TMyResource = class
  [GET, Produces(TMediaType.TEXT_PLAIN)]
  function SayHello: string;
end;
```

**Avantages :**
- Architecture inspirée de JAX-RS (Java)
- Support des annotations
- Injection de dépendances
- Très structuré

**mORMot** (Open Source)
- Framework complet (ORM + REST)
- Très performant
- Nombreuses fonctionnalités
- Courbe d'apprentissage plus importante

## Création d'un service REST simple

### Exemple avec Horse Framework

#### 1. Installation

Via GetIt Package Manager ou installation manuelle depuis GitHub.

#### 2. Création du projet

```pascal
program SimpleRESTServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse;

begin
  // Route simple : GET /hello
  THorse.Get('/hello',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    begin
      Res.Send('Hello World!');
    end);

  // Route avec paramètre : GET /hello/Jean
  THorse.Get('/hello/:name',
    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
    var
      Name: string;
    begin
      Name := Req.Params['name'];
      Res.Send('Hello ' + Name + '!');
    end);

  // Démarrage du serveur sur le port 9000
  THorse.Listen(9000,
    procedure(Horse: THorse)
    begin
      Writeln('Serveur démarré sur http://localhost:9000');
      Writeln('Appuyez sur Entrée pour arrêter');
    end);

  // Attendre une touche pour arrêter
  Readln;
end.
```

#### 3. Test du service

Ouvrir un navigateur et accéder à :
- `http://localhost:9000/hello` → "Hello World!"
- `http://localhost:9000/hello/Jean` → "Hello Jean!"

### Structure d'une API REST complète

```pascal
program APIComplet;

uses
  Horse,
  Horse.Jhonson, // Middleware JSON
  System.JSON;

var
  App: THorse;

// Route GET - Liste
procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    // Simulation de données
    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('id', TJSONNumber.Create(1));
    JSONObject.AddPair('nom', 'Dupont');
    JSONObject.AddPair('prenom', 'Jean');
    JSONArray.Add(JSONObject);

    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('id', TJSONNumber.Create(2));
    JSONObject.AddPair('nom', 'Martin');
    JSONObject.AddPair('prenom', 'Marie');
    JSONArray.Add(JSONObject);

    Res.Send<TJSONArray>(JSONArray);
  finally
    // JSONArray sera libéré automatiquement
  end;
end;

// Route GET avec ID - Détail
procedure GetClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  ID: string;
  JSONObject: TJSONObject;
begin
  ID := Req.Params['id'];

  // Simulation de récupération depuis base de données
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('id', TJSONNumber.Create(StrToInt(ID)));
    JSONObject.AddPair('nom', 'Dupont');
    JSONObject.AddPair('prenom', 'Jean');
    JSONObject.AddPair('email', 'jean.dupont@email.com');

    Res.Send<TJSONObject>(JSONObject);
  finally
    // JSONObject sera libéré automatiquement
  end;
end;

// Route POST - Création
procedure CreateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Body: TJSONObject;
  Response: TJSONObject;
begin
  Body := Req.Body<TJSONObject>;

  // Traitement de la création
  // ... insertion en base de données ...

  Response := TJSONObject.Create;
  try
    Response.AddPair('success', TJSONBool.Create(True));
    Response.AddPair('message', 'Client créé avec succès');
    Response.AddPair('id', TJSONNumber.Create(123)); // ID généré

    Res.Status(201).Send<TJSONObject>(Response); // 201 Created
  finally
    // Response sera libéré automatiquement
  end;
end;

// Route PUT - Modification
procedure UpdateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  ID: string;
  Body: TJSONObject;
  Response: TJSONObject;
begin
  ID := Req.Params['id'];
  Body := Req.Body<TJSONObject>;

  // Traitement de la modification
  // ... mise à jour en base de données ...

  Response := TJSONObject.Create;
  try
    Response.AddPair('success', TJSONBool.Create(True));
    Response.AddPair('message', 'Client modifié avec succès');

    Res.Send<TJSONObject>(Response);
  finally
    // Response sera libéré automatiquement
  end;
end;

// Route DELETE - Suppression
procedure DeleteClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  ID: string;
  Response: TJSONObject;
begin
  ID := Req.Params['id'];

  // Traitement de la suppression
  // ... suppression en base de données ...

  Response := TJSONObject.Create;
  try
    Response.AddPair('success', TJSONBool.Create(True));
    Response.AddPair('message', 'Client supprimé avec succès');

    Res.Status(204).Send<TJSONObject>(Response); // 204 No Content
  finally
    // Response sera libéré automatiquement
  end;
end;

begin
  App := THorse.Create;

  // Middleware pour JSON
  App.Use(Jhonson);

  // Routes CRUD
  App.Get('/api/clients', GetClients);
  App.Get('/api/clients/:id', GetClient);
  App.Post('/api/clients', CreateClient);
  App.Put('/api/clients/:id', UpdateClient);
  App.Delete('/api/clients/:id', DeleteClient);

  App.Listen(9000);

  Writeln('API REST démarrée sur http://localhost:9000');
  Readln;
end.
```

## Intégration avec une base de données

### Utilisation de FireDAC

```pascal
unit ClientController;

interface

uses
  Horse,
  System.JSON,
  FireDAC.Comp.Client,
  System.SysUtils;

procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure GetClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure CreateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

uses
  DataModuleDB; // DataModule contenant la connexion

procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDatabase.Connection;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients';
    Query.Open;

    JSONArray := TJSONArray.Create;
    try
      while not Query.Eof do
      begin
        JSONObject := TJSONObject.Create;
        JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
        JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
        JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
        JSONObject.AddPair('email', Query.FieldByName('email').AsString);
        JSONArray.Add(JSONObject);

        Query.Next;
      end;

      Res.Send<TJSONArray>(JSONArray);
    finally
      // JSONArray sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

procedure GetClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  JSONObject: TJSONObject;
  ID: Integer;
begin
  ID := StrToInt(Req.Params['id']);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDatabase.Connection;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if Query.IsEmpty then
    begin
      Res.Status(404).Send('Client non trouvé');
      Exit;
    end;

    JSONObject := TJSONObject.Create;
    try
      JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
      JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
      JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
      JSONObject.AddPair('email', Query.FieldByName('email').AsString);

      Res.Send<TJSONObject>(JSONObject);
    finally
      // JSONObject sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

procedure CreateClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  Body: TJSONObject;
  Response: TJSONObject;
  Nom, Prenom, Email: string;
  NewID: Integer;
begin
  Body := Req.Body<TJSONObject>;

  // Extraction des données
  Nom := Body.GetValue<string>('nom');
  Prenom := Body.GetValue<string>('prenom');
  Email := Body.GetValue<string>('email');

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDatabase.Connection;
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email)';
    Query.ParamByName('nom').AsString := Nom;
    Query.ParamByName('prenom').AsString := Prenom;
    Query.ParamByName('email').AsString := Email;
    Query.ExecSQL;

    // Récupérer l'ID généré (dépend du SGBD)
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
    Query.Open;
    NewID := Query.FieldByName('id').AsInteger;

    Response := TJSONObject.Create;
    try
      Response.AddPair('success', TJSONBool.Create(True));
      Response.AddPair('message', 'Client créé avec succès');
      Response.AddPair('id', TJSONNumber.Create(NewID));

      Res.Status(201).Send<TJSONObject>(Response);
    finally
      // Response sera libéré automatiquement
    end;
  finally
    Query.Free;
  end;
end;

end.
```

## Gestion des erreurs et codes HTTP

### Codes de statut HTTP standards

| Code | Signification | Utilisation |
|------|---------------|-------------|
| 200 | OK | Requête réussie (GET, PUT, PATCH) |
| 201 | Created | Ressource créée (POST) |
| 204 | No Content | Succès sans contenu (DELETE) |
| 400 | Bad Request | Requête invalide |
| 401 | Unauthorized | Non authentifié |
| 403 | Forbidden | Non autorisé |
| 404 | Not Found | Ressource non trouvée |
| 500 | Internal Server Error | Erreur serveur |

### Gestion des erreurs

```pascal
procedure GetClient(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  JSONObject: TJSONObject;
  ID: Integer;
  ErrorResponse: TJSONObject;
begin
  try
    // Validation du paramètre
    if not TryStrToInt(Req.Params['id'], ID) then
    begin
      ErrorResponse := TJSONObject.Create;
      ErrorResponse.AddPair('error', 'ID invalide');
      Res.Status(400).Send<TJSONObject>(ErrorResponse);
      Exit;
    end;

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := DMDatabase.Connection;
      Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
      Query.ParamByName('id').AsInteger := ID;
      Query.Open;

      if Query.IsEmpty then
      begin
        ErrorResponse := TJSONObject.Create;
        ErrorResponse.AddPair('error', 'Client non trouvé');
        Res.Status(404).Send<TJSONObject>(ErrorResponse);
        Exit;
      end;

      // ... traitement normal ...

    finally
      Query.Free;
    end;

  except
    on E: Exception do
    begin
      ErrorResponse := TJSONObject.Create;
      ErrorResponse.AddPair('error', 'Erreur serveur');
      ErrorResponse.AddPair('message', E.Message);
      Res.Status(500).Send<TJSONObject>(ErrorResponse);
    end;
  end;
end;
```

## Authentification et sécurité

### Authentification par token JWT

**JWT (JSON Web Token)** est le standard pour sécuriser les API REST.

```pascal
uses
  Horse,
  Horse.JWT,
  JOSE.Core.JWT,
  JOSE.Core.Builder,
  System.DateUtils;

// Middleware d'authentification
procedure AuthMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Token: string;
  JWT: TJWT;
begin
  Token := Req.Headers['Authorization'];

  if Token.IsEmpty then
  begin
    Res.Status(401).Send('Token manquant');
    Exit;
  end;

  // Retirer "Bearer " du token
  Token := Token.Replace('Bearer ', '');

  try
    // Vérifier et décoder le token
    JWT := TJOSE.Verify('SECRET_KEY', Token);
    try
      // Token valide, continuer
      Next;
    finally
      JWT.Free;
    end;
  except
    Res.Status(401).Send('Token invalide');
  end;
end;

// Route de login
procedure Login(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Body: TJSONObject;
  Username, Password: string;
  JWT: TJWT;
  Token: string;
  Response: TJSONObject;
begin
  Body := Req.Body<TJSONObject>;
  Username := Body.GetValue<string>('username');
  Password := Body.GetValue<string>('password');

  // Vérifier les identifiants (à implémenter)
  if VerifyCredentials(Username, Password) then
  begin
    // Créer le token JWT
    JWT := TJWT.Create;
    try
      JWT.Claims.Subject := Username;
      JWT.Claims.Expiration := IncHour(Now, 24); // Expire dans 24h

      Token := TJOSE.SHA256CompactToken('SECRET_KEY', JWT);

      Response := TJSONObject.Create;
      Response.AddPair('token', Token);
      Response.AddPair('expires_in', '86400'); // 24h en secondes

      Res.Send<TJSONObject>(Response);
    finally
      JWT.Free;
    end;
  end
  else
  begin
    Response := TJSONObject.Create;
    Response.AddPair('error', 'Identifiants invalides');
    Res.Status(401).Send<TJSONObject>(Response);
  end;
end;

begin
  // Route publique
  THorse.Post('/api/login', Login);

  // Routes protégées
  THorse.AddCallback(AuthMiddleware)
    .Get('/api/clients', GetClients)
    .Get('/api/clients/:id', GetClient)
    .Post('/api/clients', CreateClient);

  THorse.Listen(9000);
end.
```

### CORS (Cross-Origin Resource Sharing)

Pour permettre à des applications web d'autres domaines d'accéder à votre API :

```pascal
uses
  Horse,
  Horse.CORS;

begin
  THorse
    .Use(CORS) // Active CORS pour toutes les routes
    .Get('/api/clients', GetClients);

  THorse.Listen(9000);
end.
```

Configuration CORS personnalisée :

```pascal
procedure ConfigureCORS(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');

  if Req.Method = 'OPTIONS' then
    Res.Status(200).Send('')
  else
    Next;
end;

begin
  THorse.Use(ConfigureCORS);
  // ... autres routes ...
end.
```

## Documentation de l'API

### Documentation avec Swagger/OpenAPI

La documentation est essentielle pour une API REST :

```pascal
uses
  Horse,
  Horse.Swagger;

begin
  // Configuration Swagger
  THorseSwagger
    .Register('/api/docs')
    .Title('Mon API REST')
    .Version('1.0.0')
    .Description('Documentation de l''API de gestion de clients');

  // Routes
  THorse.Get('/api/clients', GetClients);

  THorse.Listen(9000);

  Writeln('API disponible sur http://localhost:9000');
  Writeln('Documentation Swagger sur http://localhost:9000/api/docs');
end.
```

### Format standard de documentation

```yaml
openapi: 3.0.0
info:
  title: API Gestion Clients
  version: 1.0.0
paths:
  /api/clients:
    get:
      summary: Liste tous les clients
      responses:
        '200':
          description: Liste des clients
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Client'
    post:
      summary: Créer un nouveau client
      requestBody:
        required: true
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/ClientInput'
      responses:
        '201':
          description: Client créé
components:
  schemas:
    Client:
      type: object
      properties:
        id:
          type: integer
        nom:
          type: string
        prenom:
          type: string
        email:
          type: string
```

## Tests et validation

### Test avec des outils externes

**Postman** : Outil graphique pour tester les API
- Créer des collections de requêtes
- Tester différents scénarios
- Automatiser les tests

**cURL** : Ligne de commande

```bash
# GET
curl http://localhost:9000/api/clients

# POST
curl -X POST http://localhost:9000/api/clients \
  -H "Content-Type: application/json" \
  -d '{"nom":"Dupont","prenom":"Jean","email":"jean@email.com"}'

# PUT
curl -X PUT http://localhost:9000/api/clients/1 \
  -H "Content-Type: application/json" \
  -d '{"nom":"Durand","prenom":"Paul","email":"paul@email.com"}'

# DELETE
curl -X DELETE http://localhost:9000/api/clients/1
```

### Tests unitaires avec DUnitX

```pascal
unit ClientControllerTests;

interface

uses
  DUnitX.TestFramework,
  Horse,
  System.JSON;

type
  [TestFixture]
  TClientControllerTests = class
  private
    FApp: THorse;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestGetClients;
    [Test]
    procedure TestCreateClient;
    [Test]
    procedure TestClientNotFound;
  end;

implementation

procedure TClientControllerTests.Setup;
begin
  // Initialiser l'application de test
  FApp := THorse.Create;
  // ... configuration ...
end;

procedure TClientControllerTests.TearDown;
begin
  FApp.Free;
end;

procedure TClientControllerTests.TestGetClients;
var
  Response: string;
  JSON: TJSONArray;
begin
  // Simuler une requête GET /api/clients
  Response := SimulateRequest('GET', '/api/clients');

  JSON := TJSONObject.ParseJSONValue(Response) as TJSONArray;
  try
    Assert.IsNotNull(JSON, 'La réponse doit être un tableau JSON');
    Assert.IsTrue(JSON.Count > 0, 'Le tableau ne doit pas être vide');
  finally
    JSON.Free;
  end;
end;

end.
```

## Déploiement et production

### Options de déploiement

**1. Serveur Windows dédié**
- Application console ou service Windows
- IIS comme reverse proxy
- Certificat SSL/TLS

**2. Linux (via FMXLinux ou console)**
- Serveur Linux économique
- Nginx comme reverse proxy
- Certificat Let's Encrypt gratuit

**3. Docker**
```dockerfile
FROM ubuntu:20.04
COPY ./MonAPIREST /app/MonAPIREST
WORKDIR /app
EXPOSE 9000
CMD ["./MonAPIREST"]
```

**4. Cloud (AWS, Azure, Google Cloud)**
- EC2, Azure VM, Google Compute Engine
- Scaling automatique
- Load balancing

### Configuration HTTPS

Avec Nginx comme reverse proxy :

```nginx
server {
    listen 443 ssl;
    server_name api.monapp.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location /api/ {
        proxy_pass http://localhost:9000/api/;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
    }
}
```

### Service Windows

Convertir votre application console en service Windows :

```pascal
program APIService;

uses
  Vcl.SvcMgr,
  System.SysUtils,
  APIServiceUnit in 'APIServiceUnit.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TAPIService, APIService);
  Application.Run;
end.
```

## Bonnes pratiques

### 1. Versioning de l'API

```pascal
// Version dans l'URL
THorse.Get('/api/v1/clients', GetClientsV1);
THorse.Get('/api/v2/clients', GetClientsV2);

// Version dans le header
procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Version: string;
begin
  Version := Req.Headers['API-Version'];

  if Version = '2.0' then
    GetClientsV2(Req, Res, Next)
  else
    GetClientsV1(Req, Res, Next);
end;
```

### 2. Pagination

```pascal
procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Page, PageSize, Offset: Integer;
  Query: TFDQuery;
begin
  // Paramètres de pagination (avec valeurs par défaut)
  Page := StrToIntDef(Req.Query['page'], 1);
  PageSize := StrToIntDef(Req.Query['pageSize'], 20);
  Offset := (Page - 1) * PageSize;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDatabase.Connection;
    Query.SQL.Text :=
      'SELECT * FROM clients ' +
      'ORDER BY nom ' +
      'LIMIT :pageSize OFFSET :offset';
    Query.ParamByName('pageSize').AsInteger := PageSize;
    Query.ParamByName('offset').AsInteger := Offset;
    Query.Open;

    // ... conversion en JSON ...
  finally
    Query.Free;
  end;
end;

// Utilisation : GET /api/clients?page=2&pageSize=50
```

### 3. Filtrage et tri

```pascal
// Exemple : GET /api/clients?nom=Dupont&sort=prenom&order=asc

procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  Query: TFDQuery;
  SQL: string;
  NomFiltre, SortField, SortOrder: string;
begin
  // Récupérer les paramètres
  NomFiltre := Req.Query['nom'];
  SortField := Req.Query['sort'];
  SortOrder := Req.Query['order'];

  // Construction de la requête
  SQL := 'SELECT * FROM clients WHERE 1=1';

  if not NomFiltre.IsEmpty then
    SQL := SQL + ' AND nom LIKE :nom';

  if not SortField.IsEmpty then
  begin
    SQL := SQL + ' ORDER BY ' + SortField;
    if SortOrder.ToLower = 'desc' then
      SQL := SQL + ' DESC'
    else
      SQL := SQL + ' ASC';
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DMDatabase.Connection;
    Query.SQL.Text := SQL;

    if not NomFiltre.IsEmpty then
      Query.ParamByName('nom').AsString := '%' + NomFiltre + '%';

    Query.Open;
    // ... conversion en JSON ...
  finally
    Query.Free;
  end;
end;
```

### 4. Rate Limiting

Limiter le nombre de requêtes par utilisateur :

```pascal
uses
  System.Generics.Collections;

var
  RequestCounter: TDictionary<string, Integer>;

procedure RateLimitMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  ClientIP: string;
  RequestCount: Integer;
begin
  ClientIP := Req.RawWebRequest.RemoteIP;

  if not RequestCounter.TryGetValue(ClientIP, RequestCount) then
    RequestCount := 0;

  Inc(RequestCount);
  RequestCounter.AddOrSetValue(ClientIP, RequestCount);

  if RequestCount > 100 then // Max 100 requêtes
  begin
    Res.Status(429).Send('Trop de requêtes');
    Exit;
  end;

  Next;
end;
```

### 5. Logging

```pascal
procedure LogMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  StartTime: TDateTime;
  Duration: Integer;
begin
  StartTime := Now;

  try
    Next; // Exécuter la route
  finally
    Duration := MilliSecondsBetween(Now, StartTime);

    Writeln(Format('[%s] %s %s - %d (%dms)',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
       Req.Method,
       Req.Path,
       Res.Status,
       Duration]));
  end;
end;

begin
  THorse.Use(LogMiddleware);
  // ... routes ...
end.
```

## Conclusion

La création de services REST avec Delphi ouvre des possibilités infinies pour vos applications. Vous pouvez :

✅ **Créer des architectures modernes** découplées et scalables
✅ **Servir plusieurs types de clients** (web, mobile, desktop)
✅ **Utiliser votre expertise Delphi** pour le backend
✅ **Bénéficier de performances excellentes** grâce au code natif
✅ **Déployer facilement** sur différentes plateformes

Les frameworks comme Horse rendent le développement REST avec Delphi aussi simple et élégant que les frameworks modernes d'autres langages, tout en conservant les avantages de Delphi : performance, stabilité et productivité.

Que vous créiez une nouvelle application ou que vous modernisiez un système existant, les services REST Delphi sont une excellente solution pour entrer dans l'ère des architectures distribuées et du cloud.

Dans la section suivante, nous explorerons d'autres aspects du développement web avec Delphi, notamment WebBroker et DataSnap, des technologies qui peuvent compléter ou remplacer les approches présentées ici selon vos besoins spécifiques.

⏭️ [Utilisation de WebBroker et DataSnap](/23-conception-dapplications-web-avec-delphi/04-utilisation-de-webbroker-et-datasnap.md)
