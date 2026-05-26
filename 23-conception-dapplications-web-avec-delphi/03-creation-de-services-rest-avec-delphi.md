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
- **GET** : Récupérer des données (lecture, sans effet de bord)
- **POST** : Créer une nouvelle ressource
- **PUT** : Modifier une ressource existante (**remplacement complet**)
- **PATCH** : Modifier **partiellement** une ressource (un seul champ par ex.)
- **DELETE** : Supprimer une ressource
- **HEAD** : Comme GET, mais sans le corps de la réponse (vérification)
- **OPTIONS** : Demande au serveur quelles méthodes/options sont
  disponibles (utilisé par les navigateurs pour le *preflight* CORS)

💡 Distinction PUT vs PATCH : un `PUT /clients/1` envoie l'objet
**entier** (le serveur écrase toutes les colonnes) ; un `PATCH /clients/1`
n'envoie que les champs à modifier (`{"email":"nouveau@…"}`). Pour des  
modifications partielles fréquentes, PATCH est plus économique en  
bande passante.  

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
PUT    /api/livres/5            → Remplacer entièrement le livre n°5  
PATCH  /api/livres/5            → Modifier partiellement le livre n°5  
                                  (ex. uniquement le champ "stock")
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
  ID: Integer;
  JSONObject: TJSONObject;
begin
  // ⚠️ TryStrToInt évite l'exception si /api/clients/abc est appelé
  //    (qui se transformerait en 500 au lieu d'un propre 400).
  if not TryStrToInt(Req.Params['id'], ID) then
  begin
    Res.Status(400).Send('ID invalide');
    Exit;
  end;

  // Simulation de récupération depuis base de données
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('id', TJSONNumber.Create(ID));
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
  ID: Integer;
  Body: TJSONObject;
  Response: TJSONObject;
begin
  if not TryStrToInt(Req.Params['id'], ID) then
  begin
    Res.Status(400).Send('ID invalide');
    Exit;
  end;
  Body := Req.Body<TJSONObject>;

  // Traitement de la modification (ID = identifiant validé)
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
  ID: Integer;
begin
  if not TryStrToInt(Req.Params['id'], ID) then
  begin
    Res.Status(400).Send('ID invalide');
    Exit;
  end;

  // Traitement de la suppression (ID = identifiant validé)
  // ... suppression en base de données ...

  // ⚠️ 204 No Content DOIT être renvoyé SANS corps de réponse (RFC 9110).
  //    Si l'on veut accompagner d'un message JSON, utiliser 200 OK à la
  //    place. Ici on choisit la voie REST « pure » : statut 204, pas de body.
  Res.Status(204);
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
  // ⚠️ TryStrToInt évite l'EConvertError si l'URL contient un id non numérique.
  if not TryStrToInt(Req.Params['id'], ID) then
  begin
    Res.Status(400).Send('ID invalide');
    Exit;
  end;

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
  Response, ErrorResp: TJSONObject;
  Nom, Prenom, Email: string;
  NewID: Integer;
begin
  Body := Req.Body<TJSONObject>;

  // ⚠️ Body peut être nil si le client n'a pas envoyé de corps JSON.
  //    GetValue<string>('nom') lèverait alors une AV ; on protège avec
  //    TryGetValue + validation explicite.
  if not Assigned(Body) or
     not Body.TryGetValue<string>('nom', Nom) or
     not Body.TryGetValue<string>('prenom', Prenom) or
     not Body.TryGetValue<string>('email', Email) then
  begin
    ErrorResp := TJSONObject.Create;
    ErrorResp.AddPair('error', 'bad_request');
    ErrorResp.AddPair('message',
      'Champs requis : nom, prenom, email');
    Res.Status(400).Send<TJSONObject>(ErrorResp);
    Exit;
  end;

  // Validation métier minimale — refuser les chaînes vides
  if Nom.Trim.IsEmpty or Prenom.Trim.IsEmpty or Email.Trim.IsEmpty then
  begin
    ErrorResp := TJSONObject.Create;
    ErrorResp.AddPair('error', 'validation_failed');
    ErrorResp.AddPair('message',
      'nom, prenom et email ne peuvent pas être vides');
    Res.Status(422).Send<TJSONObject>(ErrorResp);  // 422 Unprocessable Entity
    Exit;
  end;

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

    // Récupérer l'ID généré — ⚠️ syntaxe spécifique au SGBD :
    //   MySQL / MariaDB : SELECT LAST_INSERT_ID()
    //   SQL Server      : SELECT SCOPE_IDENTITY() (ou OUTPUT INSERTED.id)
    //   PostgreSQL      : INSERT ... RETURNING id (récupéré directement)
    //   SQLite          : SELECT last_insert_rowid()
    //   Firebird        : INSERT ... RETURNING id
    //   Oracle          : utiliser une SEQUENCE + RETURNING
    // L'exemple ci-dessous est valable pour MySQL/MariaDB.
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
    Query.Open;
    NewID := Query.FieldByName('id').AsInteger;

    Response := TJSONObject.Create;
    try
      Response.AddPair('success', TJSONBool.Create(True));
      Response.AddPair('message', 'Client créé avec succès');
      Response.AddPair('id', TJSONNumber.Create(NewID));

      // 💡 Bonne pratique REST : un 201 Created doit inclure un header
      //    Location pointant sur la nouvelle ressource — le client sait
      //    où la consulter ensuite (HATEOAS, lien hypermedia).
      Res.RawWebResponse.SetCustomHeader('Location',
        Format('/api/clients/%d', [NewID]));
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
| **2xx** — Succès | | |
| 200 | OK | Requête réussie (GET, PUT, PATCH) |
| 201 | Created | Ressource créée (POST) — répondre avec un header `Location: /api/.../{id}` |
| 202 | Accepted | Demande acceptée, traitement asynchrone en cours |
| 204 | No Content | Succès sans contenu (DELETE, ou PUT sans corps de retour) |
| **3xx** — Redirection | | |
| 301 | Moved Permanently | URL changée définitivement |
| 304 | Not Modified | Cache valide (lié à `If-None-Match` / ETag) |
| **4xx** — Erreur côté client | | |
| 400 | Bad Request | JSON malformé, paramètre absent… |
| 401 | Unauthorized | Pas authentifié (token manquant/invalide) |
| 403 | Forbidden | Authentifié mais sans les droits |
| 404 | Not Found | Ressource inexistante |
| 409 | Conflict | Conflit (ex. email déjà utilisé, version obsolète) |
| 422 | Unprocessable Entity | Validation métier échouée (champs valides syntaxiquement mais refusés) |
| 429 | Too Many Requests | Rate-limit dépassé |
| **5xx** — Erreur côté serveur | | |
| 500 | Internal Server Error | Exception non capturée |
| 502 | Bad Gateway | Proxy / passerelle en échec |
| 503 | Service Unavailable | Maintenance, surcharge — répondre avec `Retry-After` |

💡 Distinction **401 vs 403** : 401 = « je ne sais pas qui tu es »,
403 = « je sais qui tu es mais ça ne te concerne pas ». Erreur fréquente.

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
      // ⚠️ NE PAS exposer E.Message brut au client en production :
      //    fuite d'info (structure SQL, version BDD, chemin disque) qui
      //    aide un attaquant. Logger l'erreur côté serveur et renvoyer
      //    un message générique. En dev, on peut conditionner sur un IFDEF.
      LogError(E.ClassName + ': ' + E.Message);  // log interne (fichier, sentry, syslog…)

      ErrorResponse := TJSONObject.Create;
      ErrorResponse.AddPair('error', 'internal_server_error');
      ErrorResponse.AddPair('message',
        'Une erreur interne est survenue. Réessayez plus tard.');
      {$IFDEF DEBUG}
      ErrorResponse.AddPair('debug', E.Message);  // visible uniquement en build DEBUG
      {$ENDIF}
      Res.Status(500).Send<TJSONObject>(ErrorResponse);
    end;
  end;
end;
```

## Authentification et sécurité

### Authentification par token JWT

**JWT (JSON Web Token)** est le standard pour sécuriser les API REST.

🚨 **Avertissements de sécurité pour JWT** :
1. **Ne jamais hardcoder la clé secrète** dans le code source (comme
   `'SECRET_KEY'` ci-dessous, fait UNIQUEMENT pour la lisibilité). En
   production, charger la clé depuis une variable d'environnement, un
   fichier de configuration hors du repository Git, ou un secret store
   (HashiCorp Vault, AWS Secrets Manager…).
2. **Utiliser une clé d'au moins 256 bits** (32 octets aléatoires) pour
   HS256, ou un RSA/ECDSA pour les algorithmes asymétriques.
3. **Vérifier la signature ET l'expiration** (`exp` claim) à chaque
   requête — un token expiré doit être refusé même si la signature est
   valide.
4. **Ne pas mettre d'informations sensibles dans le payload** : le JWT
   est *signé*, pas *chiffré*. N'importe qui peut lire son contenu
   (base64) — y stocker seulement l'identité et les rôles.
5. **Préférer des durées d'expiration courtes** (15 min à 1 h) +
   refresh token pour les sessions longues.
6. **Forcer l'algorithme attendu** côté vérification : ne JAMAIS
   accepter `alg: none` (attaque historique CVE-2015-9235) ni laisser
   le décodeur choisir l'algorithme depuis le header du token (attaque
   par *algorithm confusion* HS256↔RS256). La bibliothèque JOSE permet
   d'imposer l'algorithme attendu — toujours le préciser explicitement.

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

  // 💡 RFC 6750 : on annonce le schéma attendu via WWW-Authenticate
  //    quand on renvoie un 401 (aide les clients à comprendre).
  if Token.IsEmpty then
  begin
    Res.RawWebResponse.SetCustomHeader('WWW-Authenticate', 'Bearer');
    Res.Status(401).Send('Token manquant');
    Exit;
  end;

  // RFC 6750 exige le préfixe « Bearer  » — refuser les tokens
  // sans préfixe pour éviter des soumissions ambiguës (base64 brut,
  // « Token xyz », etc.). IgnoreCase pour tolérer « bearer ».
  if not Token.StartsWith('Bearer ', True) then
  begin
    Res.RawWebResponse.SetCustomHeader('WWW-Authenticate',
      'Bearer error="invalid_request"');
    Res.Status(401).Send('Format Authorization invalide (attendu : Bearer <token>)');
    Exit;
  end;
  Token := Token.Substring(7);

  try
    // Vérifier la signature ET l'expiration (`exp` claim).
    // TJOSE.Verify lève une exception si l'un des deux échoue.
    JWT := TJOSE.Verify('SECRET_KEY', Token);
    try
      // Token valide ET non expiré → on continue
      Next;
    finally
      JWT.Free;
    end;
  except
    Res.RawWebResponse.SetCustomHeader('WWW-Authenticate',
      'Bearer error="invalid_token"');
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

  // ⚠️ Body peut être nil (pas de corps envoyé) — protéger avec TryGetValue
  //    plutôt que GetValue qui lèverait une AccessViolation.
  if not Assigned(Body) or
     not Body.TryGetValue<string>('username', Username) or
     not Body.TryGetValue<string>('password', Password) then
  begin
    Response := TJSONObject.Create;
    Response.AddPair('error', 'invalid_request');
    Response.AddPair('message', 'username et password requis');
    Res.Status(400).Send<TJSONObject>(Response);
    Exit;
  end;

  // Vérifier les identifiants (à implémenter)
  if VerifyCredentials(Username, Password) then
  begin
    // Créer le token JWT.
    // ⚠️ Durée courte conforme aux bonnes pratiques (cf. point 5).
    //    Pour des sessions plus longues, utiliser un refresh token
    //    en parallèle (token d'accès court + refresh token long).
    JWT := TJWT.Create;
    try
      // Claims standards JWT (RFC 7519) :
      //   sub (Subject)    : identité du porteur
      //   iat (IssuedAt)   : date d'émission — permet d'invalider en bloc
      //                      tous les tokens émis avant une certaine date
      //   nbf (NotBefore)  : date de début de validité (ici = maintenant)
      //   exp (Expiration) : date d'expiration
      //   iss (Issuer)     : émetteur — utile en environnement multi-tenant
      JWT.Claims.Subject    := Username;
      JWT.Claims.IssuedAt   := Now;
      JWT.Claims.NotBefore  := Now;
      JWT.Claims.Expiration := IncHour(Now, 1); // Expire dans 1 h
      JWT.Claims.Issuer     := 'monapp.com';

      Token := TJOSE.SHA256CompactToken('SECRET_KEY', JWT);

      Response := TJSONObject.Create;
      Response.AddPair('token', Token);
      Response.AddPair('token_type', 'Bearer');   // RFC 6750
      Response.AddPair('expires_in', '3600');     // 1 h en secondes

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
  // ⚠️ '*' ouvre l'API à TOUS les domaines : acceptable pour une API
  //    publique sans cookies, MAIS interdit avec Access-Control-Allow-
  //    Credentials=true (le navigateur rejettera la combinaison).
  //    En production, restreindre à la liste des domaines autorisés :
  //    Res.RawWebResponse.SetCustomHeader(
  //      'Access-Control-Allow-Origin', 'https://app.monsite.com');
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Methods',
    'GET, POST, PUT, PATCH, DELETE, HEAD, OPTIONS');
  Res.RawWebResponse.SetCustomHeader('Access-Control-Allow-Headers',
    'Content-Type, Authorization, Accept, X-Requested-With');
  // Cache du preflight CORS pendant 1 heure — évite que le navigateur
  // refasse un OPTIONS avant CHAQUE requête « non simple ».
  Res.RawWebResponse.SetCustomHeader('Access-Control-Max-Age', '3600');

  if Req.Method = 'OPTIONS' then
    Res.Status(204)  // 204 No Content — pas de body pour un preflight
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

**Postman** : outil graphique populaire pour tester les API
- Créer des collections de requêtes
- Tester différents scénarios
- Automatiser les tests
- ⚠️ Compte cloud obligatoire depuis ~2023 pour beaucoup de
  fonctionnalités ; certaines équipes lui préfèrent des alternatives
  open source 100 % locales :

**Alternatives modernes (open source) :**
- **Bruno** (bruno.app) — fichiers `.bru` versionnables en Git, aucun
  compte requis
- **Insomnia** — racheté par Kong, version Core gratuite et locale
- **Hoppscotch** — équivalent web, auto-hébergeable
- **REST Client** (extension VS Code) — requêtes dans un fichier `.http`

**cURL** : Ligne de commande

```bash
# GET
curl http://localhost:9000/api/clients

# POST
curl -X POST http://localhost:9000/api/clients \
  -H "Content-Type: application/json" \
  -d '{"nom":"Dupont","prenom":"Jean","email":"jean@email.com"}'

# PUT (remplacement complet)
curl -X PUT http://localhost:9000/api/clients/1 \
  -H "Content-Type: application/json" \
  -d '{"nom":"Durand","prenom":"Paul","email":"paul@email.com"}'

# PATCH (mise à jour partielle d'un seul champ)
curl -X PATCH http://localhost:9000/api/clients/1 \
  -H "Content-Type: application/json" \
  -d '{"email":"nouveau@email.com"}'

# DELETE
curl -X DELETE http://localhost:9000/api/clients/1

# Pour voir les headers de la réponse (debug)
curl -i http://localhost:9000/api/clients

# Pour voir requête ET réponse complètes
curl -v http://localhost:9000/api/clients
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

**2. Linux (application console)**
- Serveur Linux économique
- Nginx comme reverse proxy
- Certificat Let's Encrypt gratuit (via `certbot`)
- Service systemd pour démarrage automatique et redémarrage en cas de crash

> 💡 Pour un service REST, **pas besoin de FMXLinux** : c'est une  
> application console. FMXLinux est nécessaire uniquement pour les  
> applications graphiques FireMonkey sur Linux.

**3. Docker**

Le binaire Linux est produit par la cible *Linux 64-bit* de Delphi
(éditions Professional / Enterprise / Architect). Image conseillée en
2026 : **Ubuntu 24.04 LTS** ou la plus légère **debian:stable-slim**.

```dockerfile
FROM ubuntu:24.04  
RUN apt-get update && apt-get install -y --no-install-recommends \  
        libssl3 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    # Créer un utilisateur non-root — bonne pratique sécurité Docker :
    # un binaire compromis exécuté en root pourrait s'évader plus
    # facilement du container vers l'hôte.
    && useradd --system --user-group --no-create-home --uid 1001 apiuser

WORKDIR /app  
COPY --chown=apiuser:apiuser ./MonAPIREST /app/MonAPIREST  
RUN chmod +x /app/MonAPIREST  

USER apiuser  
EXPOSE 9000  
CMD ["/app/MonAPIREST"]  
```

> ⚠️ Le binaire doit être compilé pour **Linux x86_64** dans Delphi  
> (cible *Linux 64-bit*). Pour ARM (Raspberry Pi, AWS Graviton), il  
> faut une image `arm64` et la nouvelle cible Linux ARM de Delphi.

> 💡 **Bonne pratique sécurité Docker** : exécuter le container avec un  
> utilisateur non-root (`USER apiuser`). Note : `EXPOSE 9000` reste OK  
> car les ports ≥ 1024 ne nécessitent pas root sur Linux ; pour écouter  
> sur le port 80/443, faire écouter Nginx (en root sur l'hôte) sur ces  
> ports et proxy vers le port 9000 du container (recommandé).

**4. Cloud (AWS, Azure, Google Cloud)**
- EC2, Azure VM, Google Compute Engine
- Scaling automatique
- Load balancing

### Configuration HTTPS

Avec Nginx comme reverse proxy (config minimaliste mais correcte) :

```nginx
# Redirection HTTP → HTTPS
server {
    listen 80;
    server_name api.monapp.com;
    return 301 https://$host$request_uri;
}

server {
    listen 443 ssl;
    http2 on;                       # Nginx 1.25+ (syntaxe moderne)
    server_name api.monapp.com;

    # Certificat — par exemple via Let's Encrypt / certbot
    ssl_certificate     /etc/letsencrypt/live/api.monapp.com/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.monapp.com/privkey.pem;

    # Bonnes pratiques TLS 2026 : TLS 1.2 minimum, TLS 1.3 préféré
    ssl_protocols       TLSv1.2 TLSv1.3;
    ssl_prefer_server_ciphers off;

    # Header de sécurité — force HTTPS pendant 1 an pour les visites suivantes
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;

    location /api/ {
        proxy_pass http://localhost:9000/api/;
        proxy_set_header Host              $host;
        proxy_set_header X-Real-IP         $remote_addr;
        proxy_set_header X-Forwarded-For   $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;  # le backend sait qu'on est en HTTPS
        proxy_read_timeout 60s;
    }
}
```

> ⚠️ `X-Forwarded-For` / `X-Forwarded-Proto` permettent au backend Delphi  
> de connaître l'IP réelle du client et le protocole d'origine, sans  
> quoi tous les clients apparaîtront comme `127.0.0.1` (le proxy local)  
> et le backend croira recevoir du HTTP en clair.

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
const  
  DEFAULT_PAGE_SIZE = 20;
  MAX_PAGE_SIZE     = 100;  // ⚠️ Borne anti-DoS, sinon ?pageSize=999999
var
  Page, PageSize, Offset: Integer;
  Query: TFDQuery;
begin
  // Paramètres de pagination — toujours borner les entiers reçus du client.
  Page := StrToIntDef(Req.Query['page'], 1);
  if Page < 1 then Page := 1;

  PageSize := StrToIntDef(Req.Query['pageSize'], DEFAULT_PAGE_SIZE);
  if PageSize < 1 then PageSize := DEFAULT_PAGE_SIZE;
  if PageSize > MAX_PAGE_SIZE then PageSize := MAX_PAGE_SIZE;

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

uses System.Generics.Collections, System.SysUtils;

procedure GetClients(Req: THorseRequest; Res: THorseResponse; Next: TProc);  
const  
  // 🚨 ANTI INJECTION SQL : un nom de colonne ne peut PAS être passé en
  //    paramètre lié — FireDAC ne paramètre que les VALEURS, pas les
  //    identifiants. Sans whitelist, GET /api/clients?sort=...; DROP TABLE
  //    clients-- expose l'application à une injection critique.
  ALLOWED_SORT_FIELDS: array[0..3] of string =
    ('nom', 'prenom', 'email', 'date_creation');
var
  Query: TFDQuery;
  SQL: string;
  NomFiltre, SortField, SortOrder: string;
  i: Integer;
  SortAllowed: Boolean;
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
    // Vérification stricte contre la whitelist
    SortAllowed := False;
    for i := 0 to High(ALLOWED_SORT_FIELDS) do
      if SameText(SortField, ALLOWED_SORT_FIELDS[i]) then
      begin
        SortAllowed := True;
        SortField := ALLOWED_SORT_FIELDS[i];  // forme canonique connue
        Break;
      end;

    if SortAllowed then
    begin
      SQL := SQL + ' ORDER BY ' + SortField;
      // Pour order, seules deux valeurs sont possibles — pas besoin de
      // whitelist plus élaborée, on borne au strict nécessaire.
      if SameText(SortOrder, 'desc') then
        SQL := SQL + ' DESC'
      else
        SQL := SQL + ' ASC';
    end;
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
  System.Generics.Collections, System.DateUtils, System.SyncObjs;

type
  TRateLimitEntry = record
    Count: Integer;
    WindowStart: TDateTime;
  end;

var
  RequestCounter: TDictionary<string, TRateLimitEntry>;
  CounterLock: TCriticalSection;

// Helper : récupère l'IP réelle du client, même derrière un reverse proxy.
function GetRealClientIP(Req: THorseRequest): string;  
var  
  Forwarded: string;
  P: Integer;
begin
  // ⚠️ Derrière Nginx/HAProxy/Cloudflare, RemoteIP vaut 127.0.0.1
  //    (l'IP du proxy local) — TOUT le monde apparaîtrait pareil et
  //    serait rate-limité comme un seul client. Le proxy doit ajouter
  //    X-Forwarded-For (cf. config Nginx plus haut). On garde la PREMIÈRE
  //    IP de la liste (la plus à gauche = client d'origine).
  Forwarded := Req.Headers['X-Forwarded-For'];
  if Forwarded <> '' then
  begin
    P := Pos(',', Forwarded);
    if P > 0 then
      Result := Trim(Copy(Forwarded, 1, P - 1))
    else
      Result := Trim(Forwarded);
  end
  else
    Result := Req.RawWebRequest.RemoteIP;
end;

procedure RateLimitMiddleware(Req: THorseRequest; Res: THorseResponse; Next: TProc);  
const  
  WINDOW_SECONDS = 60;  // Fenêtre glissante de 60 s
  MAX_REQUESTS   = 100; // 100 requêtes max par fenêtre
var
  ClientIP: string;
  Entry: TRateLimitEntry;
begin
  ClientIP := GetRealClientIP(Req);

  // ⚠️ Le serveur étant multi-thread, l'accès au dictionnaire DOIT
  //    être protégé par un verrou (sinon EAccessViolation aléatoire
  //    sous charge). Une vraie implémentation utiliserait plutôt
  //    TDictionary thread-safe ou Redis pour partager entre instances.
  CounterLock.Enter;
  try
    if not RequestCounter.TryGetValue(ClientIP, Entry) then
    begin
      Entry.Count := 0;
      Entry.WindowStart := Now;
    end;

    // Réinitialiser le compteur si la fenêtre est expirée — sans cela,
    // le compteur ne redescend JAMAIS et bloque définitivement à 100.
    if SecondsBetween(Now, Entry.WindowStart) > WINDOW_SECONDS then
    begin
      Entry.Count := 0;
      Entry.WindowStart := Now;
    end;

    Inc(Entry.Count);
    RequestCounter.AddOrSetValue(ClientIP, Entry);

    if Entry.Count > MAX_REQUESTS then
    begin
      // RFC 6585 : 429 doit s'accompagner d'un header Retry-After
      Res.RawWebResponse.SetCustomHeader('Retry-After', IntToStr(WINDOW_SECONDS));
      Res.Status(429).Send('Trop de requêtes — réessayez plus tard');
      Exit;
    end;
  finally
    CounterLock.Leave;
  end;

  Next;
end;
```

> ⚠️ **Limite de cet exemple pédagogique** : `RequestCounter` grossit  
> indéfiniment (une entrée par IP rencontrée), ce qui est un risque  
> mémoire en production. Pour une vraie implémentation, prévoir :  
> - une **purge périodique** des entrées dont `WindowStart` est ancien  
>   (via un timer ou un compteur d'appels) ;  
> - ou utiliser **Redis** (qui gère le TTL nativement) pour partager le  
>   compteur entre plusieurs instances du backend (load balancing) ;  
> - ou un **token bucket** au lieu d'un compteur à fenêtre fixe — plus  
>   souple pour absorber les pics.

### 5. Logging

```pascal
uses System.SyncObjs;

var
  // ⚠️ Writeln n'est pas thread-safe sous charge HTTP multi-thread :
  //    plusieurs threads peuvent entrelacer leurs sorties dans la console.
  //    On sérialise les écritures avec un verrou.
  LogLock: TCriticalSection;

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

    LogLock.Enter;
    try
      Writeln(Format('[%s] %s %s - %d (%dms)',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
         Req.Method,
         Req.PathInfo,
         Res.Status,
         Duration]));
    finally
      LogLock.Leave;
    end;
  end;
end;

begin
  LogLock := TCriticalSection.Create;
  THorse.Use(LogMiddleware);
  // ... routes ...
end.
```

> 💡 En production, préférer une bibliothèque de log dédiée (**TLogger**  
> avec rotation de fichiers, **CodeSiteLogging** d'Embarcadero,  
> **mORMot Logger**, ou export vers ELK / Loki via syslog). Les  
> bibliothèques de log gèrent déjà la concurrence, la rotation, et  
> les niveaux (DEBUG/INFO/WARN/ERROR).

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
