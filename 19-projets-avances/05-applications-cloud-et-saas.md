🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.5 Applications cloud et SaaS

## Introduction

Bienvenue dans l'univers passionnant des applications cloud et SaaS (Software as a Service) ! Dans ce chapitre, vous allez apprendre à créer des applications modernes qui fonctionnent dans le cloud, accessibles depuis n'importe où et par n'importe qui.

### Qu'est-ce que le cloud et le SaaS ?

#### Le Cloud Computing

Le **cloud computing** (informatique en nuage) consiste à utiliser des serveurs distants pour stocker, gérer et traiter des données au lieu de le faire sur votre ordinateur local.

**Analogie simple** : Au lieu d'avoir votre propre générateur électrique à la maison, vous utilisez le réseau électrique. De même, au lieu d'avoir vos propres serveurs, vous utilisez les serveurs cloud.

**Les trois principaux fournisseurs** :
- ☁️ **AWS** (Amazon Web Services)
- ☁️ **Azure** (Microsoft)
- ☁️ **Google Cloud** (Google)

#### Le SaaS (Software as a Service)

Le **SaaS** est un modèle de distribution où l'application est hébergée dans le cloud et accessible via internet, généralement par abonnement.

**Exemples familiers** :
- 📧 Gmail (email)
- 📊 Google Docs (traitement de texte)
- 💬 Slack (messagerie)
- 📦 Dropbox (stockage)
- 🎵 Spotify (musique)

### Pourquoi créer des applications cloud/SaaS ?

**Avantages pour les développeurs** :
✅ **Accessibilité** : Accessible de partout, sur tout appareil
✅ **Évolutivité** : S'adapte à la charge (scaling)
✅ **Maintenance facilitée** : Une seule version à maintenir
✅ **Revenus récurrents** : Modèle d'abonnement
✅ **Mises à jour instantanées** : Tous les utilisateurs bénéficient immédiatement des nouveautés

**Avantages pour les utilisateurs** :
✅ **Pas d'installation** : Fonctionne dans le navigateur ou via API
✅ **Données synchronisées** : Accessible depuis plusieurs appareils
✅ **Toujours à jour** : Mises à jour automatiques
✅ **Coût prévisible** : Abonnement mensuel/annuel

### Objectifs de ce chapitre

À la fin de ce tutoriel, vous serez capable de :

✅ Comprendre l'architecture des applications cloud
✅ Créer des API REST avec Delphi
✅ Implémenter l'authentification JWT
✅ Gérer une base de données cloud
✅ Créer un système d'abonnement
✅ Déployer votre application dans le cloud
✅ Gérer la scalabilité
✅ Monitorer et maintenir votre SaaS

### Prérequis

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Compréhension des bases de données
- ✅ Notions de HTTP et JSON
- ✅ Bases de SQL

**Outils** :
- ✅ Delphi 13 Florence installé
- ✅ Compte cloud (AWS, Azure, ou autre)
- ✅ Postman (pour tester les API)
- ✅ Git pour la gestion de versions

### Durée estimée

**20 à 30 heures** de travail, réparties ainsi :
- Compréhension des concepts : 3-4 heures
- Création de l'API REST : 6-8 heures
- Authentification et sécurité : 4-5 heures
- Base de données et logique métier : 4-5 heures
- Déploiement et tests : 3-4 heures

---

## Partie 1 : Architecture cloud moderne

### 1.1 Architecture d'une application SaaS

Voici l'architecture typique d'une application SaaS :

```
┌────────────────────────────────────────────┐
│         Clients (Frontend)                 │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐    │
│  │   Web    │ │  Mobile  │ │ Desktop  │    │
│  └──────────┘ └──────────┘ └──────────┘    │
└────────────┬───────────────────────────────┘
             │
             │ HTTPS / REST API
             ↓
┌────────────────────────────────────────────┐
│         Load Balancer / API Gateway        │
└────────────┬───────────────────────────────┘
             │
             ↓
┌────────────────────────────────────────────┐
│         Backend API (Delphi)               │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐    │
│  │ Auth API │ │ User API │ │Data API  │    │
│  └──────────┘ └──────────┘ └──────────┘    │
└────────────┬───────────────────────────────┘
             │
             ↓
┌────────────────────────────────────────────┐
│         Base de données Cloud              │
│         (MySQL, PostgreSQL, etc.)          │
└────────────────────────────────────────────┘
```

**Composants clés** :

1. **Frontend** : Interface utilisateur (web, mobile, desktop)
2. **API Gateway** : Point d'entrée unique, gère le routage
3. **Backend API** : Logique métier développée en Delphi
4. **Base de données** : Stockage des données
5. **Services additionnels** : Email, stockage fichiers, cache, etc.

### 1.2 REST API : Le cœur du système

**REST** (Representational State Transfer) est un style d'architecture pour les API web.

#### Principes REST

**1. Ressources** : Tout est une ressource (utilisateur, produit, commande)

```
Utilisateurs : /api/users
Produit #42 : /api/products/42
Commandes : /api/orders
```

**2. Méthodes HTTP** : Actions sur les ressources

| Méthode | Action | Exemple |
|---------|--------|---------|
| GET | Lire | GET /api/users → Liste des utilisateurs |
| POST | Créer | POST /api/users → Créer un utilisateur |
| PUT | Modifier | PUT /api/users/42 → Modifier l'utilisateur 42 |
| DELETE | Supprimer | DELETE /api/users/42 → Supprimer l'utilisateur 42 |

**3. Stateless** : Chaque requête est indépendante

**4. Format JSON** : Échange de données en JSON

```json
{
  "id": 42,
  "name": "Jean Dupont",
  "email": "jean@example.com"
}
```

### 1.3 Modèle SaaS typique

Notre projet : **TaskMaster** - Un gestionnaire de tâches collaboratif en SaaS

**Fonctionnalités** :
- Gestion de tâches personnelles et d'équipe
- Collaboration en temps réel
- Abonnement mensuel (Free, Pro, Enterprise)
- API REST complète
- Applications web et mobile

**Stack technique** :
- **Backend** : Delphi (API REST)
- **Base de données** : PostgreSQL (cloud)
- **Authentification** : JWT (JSON Web Tokens)
- **Déploiement** : Docker + Cloud (AWS/Azure)
- **Frontend** : React/Vue.js (non couvert ici)

---

## Partie 2 : Création de l'API REST

### 2.1 Configuration du projet

**Étape 1 : Créer un projet DataSnap REST**

1. **Fichier → Nouveau → Autre...**
2. **Delphi Projects → DataSnap Server**
3. Sélectionnez **DataSnap REST Application**
4. Type de serveur : **Standalone VCL Application**
5. Nom : `TaskMasterAPI`

Delphi génère un serveur HTTP complet !

**Étape 2 : Structure du projet**

```
TaskMasterAPI/
├── Source/
│   ├── API/
│   │   ├── uAuthAPI.pas          (Authentification)
│   │   ├── uUserAPI.pas          (Gestion utilisateurs)
│   │   ├── uTaskAPI.pas          (Gestion tâches)
│   │   └── uTeamAPI.pas          (Gestion équipes)
│   ├── Models/
│   │   ├── uUserModel.pas        (Modèle utilisateur)
│   │   ├── uTaskModel.pas        (Modèle tâche)
│   │   └── uTeamModel.pas        (Modèle équipe)
│   ├── Database/
│   │   └── uDatabaseManager.pas  (Connexion BDD)
│   ├── Utils/
│   │   ├── uJWT.pas              (Gestion JWT)
│   │   └── uLogger.pas           (Logs)
│   └── TaskMasterAPI.dpr
└── Config/
    └── database.ini
```

### 2.2 Configuration du serveur HTTP

```pascal
unit uServerContainer;

interface

uses
  System.SysUtils, System.Classes,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  IPPeerServer, IPPeerAPI,
  Datasnap.DSHTTPCommon, Datasnap.DSHTTP;

type
  TServerContainer = class(TDataModule)
    DSServer1: TDSServer;
    DSHTTPService1: TDSHTTPService;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ServerContainer: TServerContainer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  // Configuration du serveur HTTP
  DSHTTPService1.HttpPort := 8080;

  // CORS pour accepter les requêtes cross-domain
  DSHTTPService1.Filters := 'cors';

  // Démarrer le serveur
  DSServer1.Start;
end;

end.
```

### 2.3 Modèles de données

#### Modèle Utilisateur

```pascal
unit uUserModel;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections;

type
  TUserRole = (urFree, urPro, urEnterprise);

  TUser = class
  private
    FID: Integer;
    FEmail: string;
    FPasswordHash: string;
    FName: string;
    FRole: TUserRole;
    FCreatedAt: TDateTime;
    FLastLogin: TDateTime;
  public
    constructor Create;

    function ToJSON: TJSONObject;
    procedure FromJSON(AJSON: TJSONObject);

    property ID: Integer read FID write FID;
    property Email: string read FEmail write FEmail;
    property PasswordHash: string read FPasswordHash write FPasswordHash;
    property Name: string read FName write FName;
    property Role: TUserRole read FRole write FRole;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property LastLogin: TDateTime read FLastLogin write FLastLogin;
  end;

implementation

{ TUser }

constructor TUser.Create;
begin
  inherited;
  FID := 0;
  FRole := urFree;
  FCreatedAt := Now;
end;

function TUser.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', TJSONNumber.Create(FID));
  Result.AddPair('email', FEmail);
  Result.AddPair('name', FName);
  Result.AddPair('role', TJSONNumber.Create(Ord(FRole)));
  Result.AddPair('createdAt', DateTimeToStr(FCreatedAt));
  Result.AddPair('lastLogin', DateTimeToStr(FLastLogin));
  // Ne pas inclure le hash du mot de passe !
end;

procedure TUser.FromJSON(AJSON: TJSONObject);
begin
  if AJSON.TryGetValue<Integer>('id', FID) then;
  if AJSON.TryGetValue<string>('email', FEmail) then;
  if AJSON.TryGetValue<string>('name', FName) then;

  var RoleInt: Integer;
  if AJSON.TryGetValue<Integer>('role', RoleInt) then
    FRole := TUserRole(RoleInt);
end;

end.
```

#### Modèle Tâche

```pascal
unit uTaskModel;

interface

uses
  System.SysUtils, System.JSON, System.Generics.Collections;

type
  TTaskStatus = (tsNew, tsInProgress, tsCompleted, tsCancelled);
  TTaskPriority = (tpLow, tpMedium, tpHigh, tpUrgent);

  TTask = class
  private
    FID: Integer;
    FTitle: string;
    FDescription: string;
    FStatus: TTaskStatus;
    FPriority: TTaskPriority;
    FUserID: Integer;
    FTeamID: Integer;
    FDueDate: TDateTime;
    FCreatedAt: TDateTime;
    FCompletedAt: TDateTime;
  public
    constructor Create;

    function ToJSON: TJSONObject;
    procedure FromJSON(AJSON: TJSONObject);

    property ID: Integer read FID write FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property Status: TTaskStatus read FStatus write FStatus;
    property Priority: TTaskPriority read FPriority write FPriority;
    property UserID: Integer read FUserID write FUserID;
    property TeamID: Integer read FTeamID write FTeamID;
    property DueDate: TDateTime read FDueDate write FDueDate;
    property CreatedAt: TDateTime read FCreatedAt write FCreatedAt;
    property CompletedAt: TDateTime read FCompletedAt write FCompletedAt;
  end;

implementation

{ TTask }

constructor TTask.Create;
begin
  inherited;
  FID := 0;
  FStatus := tsNew;
  FPriority := tpMedium;
  FCreatedAt := Now;
end;

function TTask.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', TJSONNumber.Create(FID));
  Result.AddPair('title', FTitle);
  Result.AddPair('description', FDescription);
  Result.AddPair('status', TJSONNumber.Create(Ord(FStatus)));
  Result.AddPair('priority', TJSONNumber.Create(Ord(FPriority)));
  Result.AddPair('userId', TJSONNumber.Create(FUserID));
  Result.AddPair('teamId', TJSONNumber.Create(FTeamID));
  Result.AddPair('dueDate', DateTimeToStr(FDueDate));
  Result.AddPair('createdAt', DateTimeToStr(FCreatedAt));

  if FStatus = tsCompleted then
    Result.AddPair('completedAt', DateTimeToStr(FCompletedAt));
end;

procedure TTask.FromJSON(AJSON: TJSONObject);
begin
  AJSON.TryGetValue<Integer>('id', FID);
  AJSON.TryGetValue<string>('title', FTitle);
  AJSON.TryGetValue<string>('description', FDescription);
  AJSON.TryGetValue<Integer>('userId', FUserID);
  AJSON.TryGetValue<Integer>('teamId', FTeamID);

  var StatusInt, PriorityInt: Integer;
  if AJSON.TryGetValue<Integer>('status', StatusInt) then
    FStatus := TTaskStatus(StatusInt);
  if AJSON.TryGetValue<Integer>('priority', PriorityInt) then
    FPriority := TTaskPriority(PriorityInt);
end;

end.
```

### 2.4 API REST : Endpoints

#### API d'authentification

```pascal
unit uAuthAPI;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  Datasnap.DSServer, Datasnap.DSAuth;

type
  TAuthAPI = class(TDSServerClass)
  public
    function Login(const AEmail, APassword: string): string;
    function Register(const AEmail, APassword, AName: string): string;
    function RefreshToken(const ARefreshToken: string): string;
    function ValidateToken(const AToken: string): Boolean;
  end;

implementation

uses
  uDatabaseManager, uJWT, uUserModel, System.Hash;

{ TAuthAPI }

function TAuthAPI.Login(const AEmail, APassword: string): string;
var
  User: TUser;
  Response: TJSONObject;
  PasswordHash: string;
begin
  try
    // Valider les entrées
    if AEmail.IsEmpty or APassword.IsEmpty then
      raise Exception.Create('Email et mot de passe requis');

    // Hasher le mot de passe pour comparaison
    PasswordHash := THashSHA2.GetHashString(APassword);

    // Chercher l'utilisateur
    User := DatabaseManager.GetUserByEmail(AEmail);
    if not Assigned(User) then
      raise Exception.Create('Email ou mot de passe incorrect');

    try
      // Vérifier le mot de passe
      if User.PasswordHash <> PasswordHash then
        raise Exception.Create('Email ou mot de passe incorrect');

      // Mettre à jour la dernière connexion
      User.LastLogin := Now;
      DatabaseManager.UpdateUser(User);

      // Générer les tokens JWT
      Response := TJSONObject.Create;
      try
        Response.AddPair('token', TJWT.GenerateToken(User.ID, User.Email));
        Response.AddPair('refreshToken', TJWT.GenerateRefreshToken(User.ID));
        Response.AddPair('user', User.ToJSON);

        Result := Response.ToString;
      finally
        Response.Free;
      end;

    finally
      User.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TAuthAPI.Register(const AEmail, APassword, AName: string): string;
var
  User: TUser;
  Response: TJSONObject;
  PasswordHash: string;
begin
  try
    // Valider les entrées
    if AEmail.IsEmpty or APassword.IsEmpty or AName.IsEmpty then
      raise Exception.Create('Tous les champs sont requis');

    // Vérifier que l'email n'existe pas déjà
    if DatabaseManager.EmailExists(AEmail) then
      raise Exception.Create('Cet email est déjà utilisé');

    // Créer le nouvel utilisateur
    User := TUser.Create;
    try
      User.Email := AEmail;
      User.Name := AName;
      User.PasswordHash := THashSHA2.GetHashString(APassword);
      User.Role := urFree; // Compte gratuit par défaut

      // Sauvegarder en base
      User.ID := DatabaseManager.CreateUser(User);

      // Générer les tokens
      Response := TJSONObject.Create;
      try
        Response.AddPair('token', TJWT.GenerateToken(User.ID, User.Email));
        Response.AddPair('refreshToken', TJWT.GenerateRefreshToken(User.ID));
        Response.AddPair('user', User.ToJSON);

        Result := Response.ToString;
      finally
        Response.Free;
      end;

    finally
      User.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TAuthAPI.RefreshToken(const ARefreshToken: string): string;
var
  UserID: Integer;
  Response: TJSONObject;
begin
  try
    // Valider le refresh token
    if not TJWT.ValidateRefreshToken(ARefreshToken, UserID) then
      raise Exception.Create('Refresh token invalide');

    // Générer un nouveau token
    Response := TJSONObject.Create;
    try
      var User := DatabaseManager.GetUserByID(UserID);
      try
        Response.AddPair('token', TJWT.GenerateToken(UserID, User.Email));
        Result := Response.ToString;
      finally
        User.Free;
      end;
    finally
      Response.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TAuthAPI.ValidateToken(const AToken: string): Boolean;
var
  UserID: Integer;
  Email: string;
begin
  Result := TJWT.ValidateToken(AToken, UserID, Email);
end;

end.
```

#### API des tâches

```pascal
unit uTaskAPI;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  System.Generics.Collections,
  Datasnap.DSServer, Datasnap.DSAuth;

type
  TTaskAPI = class(TDSServerClass)
  public
    // CRUD des tâches
    function GetTasks(const AToken: string): string;
    function GetTask(const AToken: string; ATaskID: Integer): string;
    function CreateTask(const AToken, ATaskJSON: string): string;
    function UpdateTask(const AToken: string; ATaskID: Integer;
      const ATaskJSON: string): string;
    function DeleteTask(const AToken: string; ATaskID: Integer): string;

    // Actions spécifiques
    function CompleteTask(const AToken: string; ATaskID: Integer): string;
    function AssignTask(const AToken: string; ATaskID, AUserID: Integer): string;
  end;

implementation

uses
  uDatabaseManager, uJWT, uTaskModel;

{ TTaskAPI }

function TTaskAPI.GetTasks(const AToken: string): string;
var
  UserID: Integer;
  Email: string;
  Tasks: TObjectList<TTask>;
  Task: TTask;
  Response: TJSONObject;
  TasksArray: TJSONArray;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Récupérer les tâches de l'utilisateur
    Tasks := DatabaseManager.GetUserTasks(UserID);
    try
      TasksArray := TJSONArray.Create;
      try
        for Task in Tasks do
          TasksArray.AddElement(Task.ToJSON);

        Response := TJSONObject.Create;
        try
          Response.AddPair('tasks', TasksArray);
          Response.AddPair('count', TJSONNumber.Create(Tasks.Count));
          Result := Response.ToString;
        finally
          Response.Free;
        end;
      except
        TasksArray.Free;
        raise;
      end;
    finally
      Tasks.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.GetTask(const AToken: string; ATaskID: Integer): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Récupérer la tâche
    Task := DatabaseManager.GetTask(ATaskID);
    try
      // Vérifier que l'utilisateur a accès à cette tâche
      if Task.UserID <> UserID then
        raise Exception.Create('Accès refusé');

      Result := Task.ToJSON.ToString;
    finally
      Task.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.CreateTask(const AToken, ATaskJSON: string): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  JSON: TJSONObject;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Parser le JSON
    JSON := TJSONObject.ParseJSONValue(ATaskJSON) as TJSONObject;
    try
      Task := TTask.Create;
      try
        Task.FromJSON(JSON);
        Task.UserID := UserID; // Forcer l'utilisateur actuel
        Task.CreatedAt := Now;

        // Sauvegarder en base
        Task.ID := DatabaseManager.CreateTask(Task);

        // Retourner la tâche créée
        Result := Task.ToJSON.ToString;
      finally
        Task.Free;
      end;
    finally
      JSON.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.UpdateTask(const AToken: string; ATaskID: Integer;
  const ATaskJSON: string): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  JSON: TJSONObject;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Récupérer la tâche existante
    Task := DatabaseManager.GetTask(ATaskID);
    try
      // Vérifier les droits
      if Task.UserID <> UserID then
        raise Exception.Create('Accès refusé');

      // Mettre à jour avec les nouvelles données
      JSON := TJSONObject.ParseJSONValue(ATaskJSON) as TJSONObject;
      try
        Task.FromJSON(JSON);
        Task.ID := ATaskID; // Garder l'ID original

        // Sauvegarder
        DatabaseManager.UpdateTask(Task);

        Result := Task.ToJSON.ToString;
      finally
        JSON.Free;
      end;
    finally
      Task.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.DeleteTask(const AToken: string; ATaskID: Integer): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Vérifier les droits
    Task := DatabaseManager.GetTask(ATaskID);
    try
      if Task.UserID <> UserID then
        raise Exception.Create('Accès refusé');
    finally
      Task.Free;
    end;

    // Supprimer
    DatabaseManager.DeleteTask(ATaskID);

    Response := TJSONObject.Create;
    try
      Response.AddPair('success', TJSONBool.Create(True));
      Response.AddPair('message', 'Tâche supprimée');
      Result := Response.ToString;
    finally
      Response.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.CompleteTask(const AToken: string; ATaskID: Integer): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Marquer comme complétée
    Task := DatabaseManager.GetTask(ATaskID);
    try
      if Task.UserID <> UserID then
        raise Exception.Create('Accès refusé');

      Task.Status := tsCompleted;
      Task.CompletedAt := Now;

      DatabaseManager.UpdateTask(Task);

      Result := Task.ToJSON.ToString;
    finally
      Task.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

function TTaskAPI.AssignTask(const AToken: string;
  ATaskID, AUserID: Integer): string;
var
  UserID: Integer;
  Email: string;
  Task: TTask;
  Response: TJSONObject;
begin
  try
    // Valider le token
    if not TJWT.ValidateToken(AToken, UserID, Email) then
      raise Exception.Create('Token invalide');

    // Assigner la tâche
    Task := DatabaseManager.GetTask(ATaskID);
    try
      // Vérifier les droits (propriétaire ou membre de l'équipe)
      if Task.UserID <> UserID then
        raise Exception.Create('Accès refusé');

      Task.UserID := AUserID;
      DatabaseManager.UpdateTask(Task);

      Result := Task.ToJSON.ToString;
    finally
      Task.Free;
    end;

  except
    on E: Exception do
    begin
      Response := TJSONObject.Create;
      try
        Response.AddPair('error', E.Message);
        Result := Response.ToString;
      finally
        Response.Free;
      end;
    end;
  end;
end;

end.
```

---

## Partie 3 : Authentification JWT

### 3.1 Qu'est-ce que JWT ?

**JWT** (JSON Web Token) est un standard pour créer des tokens d'authentification.

**Structure d'un JWT** :
```
eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VySWQiOjQyLCJlbWFpbCI6ImpvaG5AZXhhbXBsZS5jb20ifQ.signature
│                Header                │            Payload           │ Signature │
```

**Avantages** :
- ✅ Pas besoin de session côté serveur
- ✅ Stateless (chaque requête est indépendante)
- ✅ Scalable facilement
- ✅ Peut contenir des informations utilisateur

### 3.2 Implémentation JWT

```pascal
unit uJWT;

interface

uses
  System.SysUtils, System.JSON, System.NetEncoding, System.Hash;

type
  TJWT = class
  private
    class var FSecretKey: string;

    class function Base64UrlEncode(const AInput: string): string;
    class function Base64UrlDecode(const AInput: string): string;
    class function CreateSignature(const AHeader, APayload: string): string;
  public
    class constructor Create;

    class function GenerateToken(AUserID: Integer;
      const AEmail: string): string;
    class function GenerateRefreshToken(AUserID: Integer): string;
    class function ValidateToken(const AToken: string;
      out AUserID: Integer; out AEmail: string): Boolean;
    class function ValidateRefreshToken(const AToken: string;
      out AUserID: Integer): Boolean;
  end;

implementation

uses
  System.DateUtils;

{ TJWT }

class constructor TJWT.Create;
begin
  // En production, stocker dans un fichier de config sécurisé
  FSecretKey := 'VotreCléSecrèteTrèsLongueEtComplexe123!@#';
end;

class function TJWT.Base64UrlEncode(const AInput: string): string;
begin
  Result := TNetEncoding.Base64.Encode(AInput);
  // Adapter pour Base64Url
  Result := Result.Replace('+', '-').Replace('/', '_').Replace('=', '');
end;

class function TJWT.Base64UrlDecode(const AInput: string): string;
var
  Input: string;
begin
  Input := AInput.Replace('-', '+').Replace('_', '/');

  // Ajouter le padding si nécessaire
  case Length(Input) mod 4 of
    2: Input := Input + '==';
    3: Input := Input + '=';
  end;

  Result := TNetEncoding.Base64.Decode(Input);
end;

class function TJWT.CreateSignature(const AHeader, APayload: string): string;
var
  Data: string;
begin
  Data := AHeader + '.' + APayload + '.' + FSecretKey;
  Result := Base64UrlEncode(THashSHA2.GetHashString(Data));
end;

class function TJWT.GenerateToken(AUserID: Integer;
  const AEmail: string): string;
var
  Header, Payload: TJSONObject;
  HeaderStr, PayloadStr, Signature: string;
  ExpirationTime: TDateTime;
begin
  // Header
  Header := TJSONObject.Create;
  try
    Header.AddPair('alg', 'HS256');
    Header.AddPair('typ', 'JWT');
    HeaderStr := Base64UrlEncode(Header.ToString);
  finally
    Header.Free;
  end;

  // Payload
  Payload := TJSONObject.Create;
  try
    Payload.AddPair('userId', TJSONNumber.Create(AUserID));
    Payload.AddPair('email', AEmail);

    // Expiration dans 24 heures
    ExpirationTime := IncHour(Now, 24);
    Payload.AddPair('exp', TJSONNumber.Create(DateTimeToUnix(ExpirationTime)));

    PayloadStr := Base64UrlEncode(Payload.ToString);
  finally
    Payload.Free;
  end;

  // Signature
  Signature := CreateSignature(HeaderStr, PayloadStr);

  // Token complet
  Result := HeaderStr + '.' + PayloadStr + '.' + Signature;
end;

class function TJWT.GenerateRefreshToken(AUserID: Integer): string;
var
  Payload: TJSONObject;
  PayloadStr: string;
  ExpirationTime: TDateTime;
begin
  Payload := TJSONObject.Create;
  try
    Payload.AddPair('userId', TJSONNumber.Create(AUserID));
    Payload.AddPair('type', 'refresh');

    // Expiration dans 30 jours
    ExpirationTime := IncDay(Now, 30);
    Payload.AddPair('exp', TJSONNumber.Create(DateTimeToUnix(ExpirationTime)));

    PayloadStr := Base64UrlEncode(Payload.ToString);
  finally
    Payload.Free;
  end;

  Result := 'refresh.' + PayloadStr + '.' +
    CreateSignature('refresh', PayloadStr);
end;

class function TJWT.ValidateToken(const AToken: string;
  out AUserID: Integer; out AEmail: string): Boolean;
var
  Parts: TArray<string>;
  PayloadJSON: string;
  Payload: TJSONObject;
  ExpTimestamp: Int64;
  ExpTime: TDateTime;
  ExpectedSignature, ActualSignature: string;
begin
  Result := False;
  AUserID := 0;
  AEmail := '';

  try
    // Séparer les parties
    Parts := AToken.Split(['.']);
    if Length(Parts) <> 3 then
      Exit;

    // Vérifier la signature
    ExpectedSignature := CreateSignature(Parts[0], Parts[1]);
    ActualSignature := Parts[2];

    if ExpectedSignature <> ActualSignature then
      Exit;

    // Décoder le payload
    PayloadJSON := Base64UrlDecode(Parts[1]);
    Payload := TJSONObject.ParseJSONValue(PayloadJSON) as TJSONObject;
    try
      // Vérifier l'expiration
      ExpTimestamp := Payload.GetValue<Int64>('exp');
      ExpTime := UnixToDateTime(ExpTimestamp);

      if Now > ExpTime then
        Exit; // Token expiré

      // Extraire les données
      AUserID := Payload.GetValue<Integer>('userId');
      AEmail := Payload.GetValue<string>('email');

      Result := True;
    finally
      Payload.Free;
    end;

  except
    Result := False;
  end;
end;

class function TJWT.ValidateRefreshToken(const AToken: string;
  out AUserID: Integer): Boolean;
var
  Parts: TArray<string>;
  PayloadJSON: string;
  Payload: TJSONObject;
  TokenType: string;
  ExpTimestamp: Int64;
  ExpTime: TDateTime;
begin
  Result := False;
  AUserID := 0;

  try
    Parts := AToken.Split(['.']);
    if Length(Parts) <> 3 then
      Exit;

    if Parts[0] <> 'refresh' then
      Exit;

    PayloadJSON := Base64UrlDecode(Parts[1]);
    Payload := TJSONObject.ParseJSONValue(PayloadJSON) as TJSONObject;
    try
      TokenType := Payload.GetValue<string>('type');
      if TokenType <> 'refresh' then
        Exit;

      ExpTimestamp := Payload.GetValue<Int64>('exp');
      ExpTime := UnixToDateTime(ExpTimestamp);

      if Now > ExpTime then
        Exit;

      AUserID := Payload.GetValue<Integer>('userId');
      Result := True;
    finally
      Payload.Free;
    end;

  except
    Result := False;
  end;
end;

end.
```

### 3.3 Middleware d'authentification

```pascal
// Dans chaque endpoint API, valider le token

function RequireAuth(const AToken: string; out AUserID: Integer): Boolean;
var
  Email: string;
begin
  Result := TJWT.ValidateToken(AToken, AUserID, Email);

  if not Result then
    raise Exception.Create('Authentification requise');
end;

// Utilisation
function TTaskAPI.GetTasks(const AToken: string): string;
var
  UserID: Integer;
begin
  // Vérifier l'authentification
  if not RequireAuth(AToken, UserID) then
    Exit('{"error": "Non autorisé"}');

  // Continuer avec la logique métier
  // ...
end;
```

---

## Partie 4 : Base de données cloud

### 4.1 Configuration PostgreSQL cloud

**Options populaires** :
- **AWS RDS** (PostgreSQL)
- **Azure Database for PostgreSQL**
- **Google Cloud SQL**
- **ElephantSQL** (gratuit pour petits projets)

#### Configuration de connexion

```pascal
unit uDatabaseManager;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles,
  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Comp.Client, FireDAC.Phys.PG, FireDAC.Phys.PGDef,
  System.Generics.Collections,
  uUserModel, uTaskModel;

type
  TDatabaseManager = class
  private
    FConnection: TFDConnection;
    procedure InitializeConnection;
    procedure CreateTables;
  public
    constructor Create;
    destructor Destroy; override;

    // Utilisateurs
    function GetUserByEmail(const AEmail: string): TUser;
    function GetUserByID(AID: Integer): TUser;
    function CreateUser(AUser: TUser): Integer;
    function UpdateUser(AUser: TUser): Boolean;
    function EmailExists(const AEmail: string): Boolean;

    // Tâches
    function GetUserTasks(AUserID: Integer): TObjectList<TTask>;
    function GetTask(ATaskID: Integer): TTask;
    function CreateTask(ATask: TTask): Integer;
    function UpdateTask(ATask: TTask): Boolean;
    function DeleteTask(ATaskID: Integer): Boolean;
  end;

var
  DatabaseManager: TDatabaseManager;

implementation

{ TDatabaseManager }

constructor TDatabaseManager.Create;
begin
  inherited;
  FConnection := TFDConnection.Create(nil);
  InitializeConnection;
  CreateTables;
end;

destructor TDatabaseManager.Destroy;
begin
  FConnection.Free;
  inherited;
end;

procedure TDatabaseManager.InitializeConnection;
var
  IniFile: TIniFile;
  Host, Database, Username, Password: string;
  Port: Integer;
begin
  // Lire la configuration
  IniFile := TIniFile.Create('database.ini');
  try
    Host := IniFile.ReadString('Database', 'Host', 'localhost');
    Port := IniFile.ReadInteger('Database', 'Port', 5432);
    Database := IniFile.ReadString('Database', 'Database', 'taskmaster');
    Username := IniFile.ReadString('Database', 'Username', 'postgres');
    Password := IniFile.ReadString('Database', 'Password', '');
  finally
    IniFile.Free;
  end;

  // Configurer la connexion PostgreSQL
  FConnection.Params.Clear;
  FConnection.Params.Add('DriverID=PG');
  FConnection.Params.Add('Server=' + Host);
  FConnection.Params.Add('Port=' + Port.ToString);
  FConnection.Params.Add('Database=' + Database);
  FConnection.Params.Add('User_Name=' + Username);
  FConnection.Params.Add('Password=' + Password);
  FConnection.Params.Add('CharacterSet=UTF8');

  // Connexion
  FConnection.Connected := True;
end;

procedure TDatabaseManager.CreateTables;
begin
  // Créer la table users si elle n'existe pas
  FConnection.ExecSQL(
    'CREATE TABLE IF NOT EXISTS users (' +
    '  id SERIAL PRIMARY KEY,' +
    '  email VARCHAR(255) UNIQUE NOT NULL,' +
    '  password_hash VARCHAR(255) NOT NULL,' +
    '  name VARCHAR(255) NOT NULL,' +
    '  role INTEGER DEFAULT 0,' +
    '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
    '  last_login TIMESTAMP' +
    ')'
  );

  // Créer la table tasks
  FConnection.ExecSQL(
    'CREATE TABLE IF NOT EXISTS tasks (' +
    '  id SERIAL PRIMARY KEY,' +
    '  title VARCHAR(255) NOT NULL,' +
    '  description TEXT,' +
    '  status INTEGER DEFAULT 0,' +
    '  priority INTEGER DEFAULT 1,' +
    '  user_id INTEGER REFERENCES users(id) ON DELETE CASCADE,' +
    '  team_id INTEGER,' +
    '  due_date TIMESTAMP,' +
    '  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
    '  completed_at TIMESTAMP' +
    ')'
  );

  // Index pour améliorer les performances
  FConnection.ExecSQL(
    'CREATE INDEX IF NOT EXISTS idx_tasks_user_id ON tasks(user_id)'
  );
  FConnection.ExecSQL(
    'CREATE INDEX IF NOT EXISTS idx_tasks_status ON tasks(status)'
  );
end;

function TDatabaseManager.GetUserByEmail(const AEmail: string): TUser;
var
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT * FROM users WHERE email = :email';
    Query.ParamByName('email').AsString := AEmail;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Result := TUser.Create;
      Result.ID := Query.FieldByName('id').AsInteger;
      Result.Email := Query.FieldByName('email').AsString;
      Result.PasswordHash := Query.FieldByName('password_hash').AsString;
      Result.Name := Query.FieldByName('name').AsString;
      Result.Role := TUserRole(Query.FieldByName('role').AsInteger);
      Result.CreatedAt := Query.FieldByName('created_at').AsDateTime;

      if not Query.FieldByName('last_login').IsNull then
        Result.LastLogin := Query.FieldByName('last_login').AsDateTime;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.GetUserByID(AID: Integer): TUser;
var
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM users WHERE id = :id';
    Query.ParamByName('id').AsInteger := AID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Result := TUser.Create;
      Result.ID := Query.FieldByName('id').AsInteger;
      Result.Email := Query.FieldByName('email').AsString;
      Result.Name := Query.FieldByName('name').AsString;
      Result.Role := TUserRole(Query.FieldByName('role').AsInteger);
      Result.CreatedAt := Query.FieldByName('created_at').AsDateTime;

      if not Query.FieldByName('last_login').IsNull then
        Result.LastLogin := Query.FieldByName('last_login').AsDateTime;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.CreateUser(AUser: TUser): Integer;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO users (email, password_hash, name, role, created_at) ' +
      'VALUES (:email, :password_hash, :name, :role, :created_at) ' +
      'RETURNING id';

    Query.ParamByName('email').AsString := AUser.Email;
    Query.ParamByName('password_hash').AsString := AUser.PasswordHash;
    Query.ParamByName('name').AsString := AUser.Name;
    Query.ParamByName('role').AsInteger := Ord(AUser.Role);
    Query.ParamByName('created_at').AsDateTime := AUser.CreatedAt;

    Query.Open;
    Result := Query.FieldByName('id').AsInteger;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.UpdateUser(AUser: TUser): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'UPDATE users SET ' +
      '  email = :email,' +
      '  name = :name,' +
      '  role = :role,' +
      '  last_login = :last_login ' +
      'WHERE id = :id';

    Query.ParamByName('id').AsInteger := AUser.ID;
    Query.ParamByName('email').AsString := AUser.Email;
    Query.ParamByName('name').AsString := AUser.Name;
    Query.ParamByName('role').AsInteger := Ord(AUser.Role);
    Query.ParamByName('last_login').AsDateTime := AUser.LastLogin;

    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.EmailExists(const AEmail: string): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT COUNT(*) as count FROM users WHERE email = :email';
    Query.ParamByName('email').AsString := AEmail;
    Query.Open;

    Result := Query.FieldByName('count').AsInteger > 0;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.GetUserTasks(AUserID: Integer): TObjectList<TTask>;
var
  Query: TFDQuery;
  Task: TTask;
begin
  Result := TObjectList<TTask>.Create(True);
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT * FROM tasks WHERE user_id = :user_id ORDER BY created_at DESC';
    Query.ParamByName('user_id').AsInteger := AUserID;
    Query.Open;

    while not Query.Eof do
    begin
      Task := TTask.Create;
      Task.ID := Query.FieldByName('id').AsInteger;
      Task.Title := Query.FieldByName('title').AsString;
      Task.Description := Query.FieldByName('description').AsString;
      Task.Status := TTaskStatus(Query.FieldByName('status').AsInteger);
      Task.Priority := TTaskPriority(Query.FieldByName('priority').AsInteger);
      Task.UserID := Query.FieldByName('user_id').AsInteger;
      Task.TeamID := Query.FieldByName('team_id').AsInteger;
      Task.CreatedAt := Query.FieldByName('created_at').AsDateTime;

      if not Query.FieldByName('due_date').IsNull then
        Task.DueDate := Query.FieldByName('due_date').AsDateTime;

      if not Query.FieldByName('completed_at').IsNull then
        Task.CompletedAt := Query.FieldByName('completed_at').AsDateTime;

      Result.Add(Task);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.GetTask(ATaskID: Integer): TTask;
var
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM tasks WHERE id = :id';
    Query.ParamByName('id').AsInteger := ATaskID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Result := TTask.Create;
      Result.ID := Query.FieldByName('id').AsInteger;
      Result.Title := Query.FieldByName('title').AsString;
      Result.Description := Query.FieldByName('description').AsString;
      Result.Status := TTaskStatus(Query.FieldByName('status').AsInteger);
      Result.Priority := TTaskPriority(Query.FieldByName('priority').AsInteger);
      Result.UserID := Query.FieldByName('user_id').AsInteger;
      Result.TeamID := Query.FieldByName('team_id').AsInteger;
      Result.CreatedAt := Query.FieldByName('created_at').AsDateTime;

      if not Query.FieldByName('due_date').IsNull then
        Result.DueDate := Query.FieldByName('due_date').AsDateTime;

      if not Query.FieldByName('completed_at').IsNull then
        Result.CompletedAt := Query.FieldByName('completed_at').AsDateTime;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.CreateTask(ATask: TTask): Integer;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO tasks (title, description, status, priority, user_id, ' +
      'team_id, due_date, created_at) ' +
      'VALUES (:title, :description, :status, :priority, :user_id, ' +
      ':team_id, :due_date, :created_at) ' +
      'RETURNING id';

    Query.ParamByName('title').AsString := ATask.Title;
    Query.ParamByName('description').AsString := ATask.Description;
    Query.ParamByName('status').AsInteger := Ord(ATask.Status);
    Query.ParamByName('priority').AsInteger := Ord(ATask.Priority);
    Query.ParamByName('user_id').AsInteger := ATask.UserID;
    Query.ParamByName('team_id').AsInteger := ATask.TeamID;
    Query.ParamByName('due_date').AsDateTime := ATask.DueDate;
    Query.ParamByName('created_at').AsDateTime := ATask.CreatedAt;

    Query.Open;
    Result := Query.FieldByName('id').AsInteger;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.UpdateTask(ATask: TTask): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'UPDATE tasks SET ' +
      '  title = :title,' +
      '  description = :description,' +
      '  status = :status,' +
      '  priority = :priority,' +
      '  user_id = :user_id,' +
      '  team_id = :team_id,' +
      '  due_date = :due_date,' +
      '  completed_at = :completed_at ' +
      'WHERE id = :id';

    Query.ParamByName('id').AsInteger := ATask.ID;
    Query.ParamByName('title').AsString := ATask.Title;
    Query.ParamByName('description').AsString := ATask.Description;
    Query.ParamByName('status').AsInteger := Ord(ATask.Status);
    Query.ParamByName('priority').AsInteger := Ord(ATask.Priority);
    Query.ParamByName('user_id').AsInteger := ATask.UserID;
    Query.ParamByName('team_id').AsInteger := ATask.TeamID;
    Query.ParamByName('due_date').AsDateTime := ATask.DueDate;

    if ATask.Status = tsCompleted then
      Query.ParamByName('completed_at').AsDateTime := ATask.CompletedAt
    else
      Query.ParamByName('completed_at').Clear;

    Query.ExecSQL;
    Result := True;
  finally
    Query.Free;
  end;
end;

function TDatabaseManager.DeleteTask(ATaskID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'DELETE FROM tasks WHERE id = :id';
    Query.ParamByName('id').AsInteger := ATaskID;
    Query.ExecSQL;

    Result := True;
  finally
    Query.Free;
  end;
end;

initialization
  DatabaseManager := TDatabaseManager.Create;

finalization
  DatabaseManager.Free;

end.
```

**Fichier database.ini** :

```ini
[Database]
Host=your-postgres-server.postgres.database.azure.com
Port=5432
Database=taskmaster
Username=admin@your-server
Password=YourSecurePassword123!
```

---

## Partie 5 : Tests de l'API

### 5.1 Tests avec Postman

#### Test de l'inscription

**Requête** :
```
POST http://localhost:8080/datasnap/rest/TAuthAPI/Register
Content-Type: application/json

{
  "email": "john@example.com",
  "password": "SecurePass123!",
  "name": "John Doe"
}
```

**Réponse attendue** :
```json
{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "refreshToken": "refresh.eyJ1c2VySWQiOjF9...",
  "user": {
    "id": 1,
    "email": "john@example.com",
    "name": "John Doe",
    "role": 0,
    "createdAt": "2025-01-15 10:30:00"
  }
}
```

#### Test de connexion

**Requête** :
```
POST http://localhost:8080/datasnap/rest/TAuthAPI/Login
Content-Type: application/json

{
  "email": "john@example.com",
  "password": "SecurePass123!"
}
```

#### Test de création de tâche

**Requête** :
```
POST http://localhost:8080/datasnap/rest/TTaskAPI/CreateTask
Content-Type: application/json

{
  "token": "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...",
  "taskJSON": {
    "title": "Terminer le rapport",
    "description": "Rapport mensuel à rendre avant vendredi",
    "priority": 2,
    "dueDate": "2025-01-20 17:00:00"
  }
}
```

### 5.2 Tests unitaires

```pascal
unit uAPITests;

interface

uses
  DUnitX.TestFramework, System.SysUtils, System.JSON,
  uAuthAPI, uTaskAPI, uDatabaseManager;

type
  [TestFixture]
  TAPITests = class
  private
    FAuthAPI: TAuthAPI;
    FTaskAPI: TTaskAPI;
    FTestToken: string;
    FTestUserID: Integer;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestUserRegistration;

    [Test]
    procedure TestUserLogin;

    [Test]
    procedure TestInvalidLogin;

    [Test]
    procedure TestCreateTask;

    [Test]
    procedure TestGetTasks;

    [Test]
    procedure TestCompleteTask;
  end;

implementation

uses
  uJWT;

{ TAPITests }

procedure TAPITests.Setup;
begin
  FAuthAPI := TAuthAPI.Create;
  FTaskAPI := TTaskAPI.Create;
end;

procedure TAPITests.TearDown;
begin
  FAuthAPI.Free;
  FTaskAPI.Free;

  // Nettoyer les données de test
  if FTestUserID > 0 then
    DatabaseManager.DeleteUser(FTestUserID);
end;

procedure TAPITests.TestUserRegistration;
var
  Response: string;
  JSON: TJSONObject;
  Email: string;
begin
  Email := Format('test%d@example.com', [Random(10000)]);

  Response := FAuthAPI.Register(Email, 'TestPass123!', 'Test User');

  JSON := TJSONObject.ParseJSONValue(Response) as TJSONObject;
  try
    Assert.IsFalse(JSON.TryGetValue<string>('error', Email),
      'Registration should not return error');

    Assert.IsTrue(JSON.TryGetValue<string>('token', FTestToken),
      'Registration should return token');

    var UserJSON: TJSONObject;
    Assert.IsTrue(JSON.TryGetValue<TJSONObject>('user', UserJSON),
      'Registration should return user');

    Assert.IsTrue(UserJSON.TryGetValue<Integer>('id', FTestUserID),
      'User should have ID');
  finally
    JSON.Free;
  end;
end;

procedure TAPITests.TestUserLogin;
var
  RegisterResponse, LoginResponse: string;
  JSON: TJSONObject;
  Email, Token: string;
begin
  Email := Format('test%d@example.com', [Random(10000)]);

  // D'abord s'inscrire
  RegisterResponse := FAuthAPI.Register(Email, 'TestPass123!', 'Test User');

  // Ensuite se connecter
  LoginResponse := FAuthAPI.Login(Email, 'TestPass123!');

  JSON := TJSONObject.ParseJSONValue(LoginResponse) as TJSONObject;
  try
    Assert.IsTrue(JSON.TryGetValue<string>('token', Token),
      'Login should return token');

    Assert.IsFalse(Token.IsEmpty, 'Token should not be empty');
  finally
    JSON.Free;
  end;
end;

procedure TAPITests.TestInvalidLogin;
var
  Response: string;
  JSON: TJSONObject;
  ErrorMsg: string;
begin
  Response := FAuthAPI.Login('invalid@example.com', 'WrongPassword');

  JSON := TJSONObject.ParseJSONValue(Response) as TJSONObject;
  try
    Assert.IsTrue(JSON.TryGetValue<string>('error', ErrorMsg),
      'Invalid login should return error');
  finally
    JSON.Free;
  end;
end;

procedure TAPITests.TestCreateTask;
var
  RegisterResponse, CreateResponse: string;
  JSON, UserJSON, TaskJSON: TJSONObject;
  Token: string;
  TaskID: Integer;
begin
  // S'inscrire et obtenir le token
  RegisterResponse := FAuthAPI.Register(
    Format('test%d@example.com', [Random(10000)]),
    'TestPass123!',
    'Test User'
  );

  JSON := TJSONObject.ParseJSONValue(RegisterResponse) as TJSONObject;
  try
    JSON.TryGetValue<string>('token', Token);
    JSON.TryGetValue<TJSONObject>('user', UserJSON);
    UserJSON.TryGetValue<Integer>('id', FTestUserID);
  finally
    JSON.Free;
  end;

  // Créer une tâche
  TaskJSON := TJSONObject.Create;
  try
    TaskJSON.AddPair('title', 'Test Task');
    TaskJSON.AddPair('description', 'This is a test task');
    TaskJSON.AddPair('priority', TJSONNumber.Create(2));

    CreateResponse := FTaskAPI.CreateTask(Token, TaskJSON.ToString);

    JSON := TJSONObject.ParseJSONValue(CreateResponse) as TJSONObject;
    try
      Assert.IsTrue(JSON.TryGetValue<Integer>('id', TaskID),
        'Created task should have ID');

      Assert.IsTrue(TaskID > 0, 'Task ID should be positive');
    finally
      JSON.Free;
    end;
  finally
    TaskJSON.Free;
  end;
end;

procedure TAPITests.TestGetTasks;
var
  RegisterResponse, TasksResponse: string;
  JSON, UserJSON, TaskJSON: TJSONObject;
  TasksArray: TJSONArray;
  Token: string;
  TaskCount: Integer;
begin
  // S'inscrire
  RegisterResponse := FAuthAPI.Register(
    Format('test%d@example.com', [Random(10000)]),
    'TestPass123!',
    'Test User'
  );

  JSON := TJSONObject.ParseJSONValue(RegisterResponse) as TJSONObject;
  try
    JSON.TryGetValue<string>('token', Token);
  finally
    JSON.Free;
  end;

  // Créer quelques tâches
  for var I := 1 to 3 do
  begin
    TaskJSON := TJSONObject.Create;
    try
      TaskJSON.AddPair('title', Format('Test Task %d', [I]));
      FTaskAPI.CreateTask(Token, TaskJSON.ToString);
    finally
      TaskJSON.Free;
    end;
  end;

  // Récupérer les tâches
  TasksResponse := FTaskAPI.GetTasks(Token);

  JSON := TJSONObject.ParseJSONValue(TasksResponse) as TJSONObject;
  try
    Assert.IsTrue(JSON.TryGetValue<TJSONArray>('tasks', TasksArray),
      'Response should contain tasks array');

    Assert.IsTrue(JSON.TryGetValue<Integer>('count', TaskCount),
      'Response should contain count');

    Assert.AreEqual(3, TaskCount, 'Should have 3 tasks');
  finally
    JSON.Free;
  end;
end;

procedure TAPITests.TestCompleteTask;
var
  RegisterResponse, CreateResponse, CompleteResponse: string;
  JSON: TJSONObject;
  Token: string;
  TaskID, Status: Integer;
begin
  // S'inscrire
  RegisterResponse := FAuthAPI.Register(
    Format('test%d@example.com', [Random(10000)]),
    'TestPass123!',
    'Test User'
  );

  JSON := TJSONObject.ParseJSONValue(RegisterResponse) as TJSONObject;
  try
    JSON.TryGetValue<string>('token', Token);
  finally
    JSON.Free;
  end;

  // Créer une tâche
  var TaskJSON := TJSONObject.Create;
  try
    TaskJSON.AddPair('title', 'Task to Complete');
    CreateResponse := FTaskAPI.CreateTask(Token, TaskJSON.ToString);

    JSON := TJSONObject.ParseJSONValue(CreateResponse) as TJSONObject;
    try
      JSON.TryGetValue<Integer>('id', TaskID);
    finally
      JSON.Free;
    end;
  finally
    TaskJSON.Free;
  end;

  // Compléter la tâche
  CompleteResponse := FTaskAPI.CompleteTask(Token, TaskID);

  JSON := TJSONObject.ParseJSONValue(CompleteResponse) as TJSONObject;
  try
    Assert.IsTrue(JSON.TryGetValue<Integer>('status', Status),
      'Response should contain status');

    Assert.AreEqual(2, Status, 'Status should be Completed (2)');
  finally
    JSON.Free;
  end;
end;

end.
```

---

## Partie 6 : Déploiement dans le cloud

### 6.1 Préparation pour le déploiement

#### Dockerfile

```dockerfile
# Dockerfile pour l'API Delphi
FROM debian:bullseye-slim

# Installer les dépendances
RUN apt-get update && apt-get install -y \
    libpq5 \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# Créer le dossier de l'application
WORKDIR /app

# Copier l'exécutable
COPY TaskMasterAPI /app/
COPY database.ini /app/

# Exposer le port
EXPOSE 8080

# Démarrer l'application
CMD ["./TaskMasterAPI"]
```

#### Docker Compose

```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "8080:8080"
    environment:
      - DATABASE_HOST=db
      - DATABASE_PORT=5432
      - DATABASE_NAME=taskmaster
      - DATABASE_USER=postgres
      - DATABASE_PASSWORD=secure_password
    depends_on:
      - db
    restart: unless-stopped

  db:
    image: postgres:14-alpine
    environment:
      - POSTGRES_DB=taskmaster
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=secure_password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf
      - ./ssl:/etc/nginx/ssl
    depends_on:
      - api
    restart: unless-stopped

volumes:
  postgres_data:
```

### 6.2 Déploiement sur AWS

#### Utilisation d'AWS Elastic Beanstalk

```bash
# Installer AWS CLI
pip install awscli

# Configurer
aws configure

# Créer l'application
aws elasticbeanstalk create-application --application-name taskmaster-api

# Créer l'environnement
aws elasticbeanstalk create-environment \
  --application-name taskmaster-api \
  --environment-name production \
  --solution-stack-name "64bit Amazon Linux 2 v3.x.x running Docker"

# Déployer
zip taskmaster.zip Dockerfile TaskMasterAPI database.ini
aws elasticbeanstalk create-application-version \
  --application-name taskmaster-api \
  --version-label v1.0 \
  --source-bundle S3Bucket=my-bucket,S3Key=taskmaster.zip

aws elasticbeanstalk update-environment \
  --environment-name production \
  --version-label v1.0
```

### 6.3 Déploiement sur Azure

```bash
# Installer Azure CLI
curl -sL https://aka.ms/InstallAzureCLIDeb | sudo bash

# Se connecter
az login

# Créer un groupe de ressources
az group create --name taskmaster-rg --location westeurope

# Créer un App Service Plan
az appservice plan create \
  --name taskmaster-plan \
  --resource-group taskmaster-rg \
  --sku B1 \
  --is-linux

# Créer une Web App avec Docker
az webapp create \
  --resource-group taskmaster-rg \
  --plan taskmaster-plan \
  --name taskmaster-api \
  --deployment-container-image-name your-docker-hub/taskmaster:latest

# Configurer la base de données PostgreSQL
az postgres server create \
  --resource-group taskmaster-rg \
  --name taskmaster-db \
  --location westeurope \
  --admin-user adminuser \
  --admin-password SecurePass123! \
  --sku-name B_Gen5_1
```

### 6.4 Configuration HTTPS avec Let's Encrypt

```nginx
# nginx.conf
events {
    worker_connections 1024;
}

http {
    upstream api {
        server api:8080;
    }

    server {
        listen 80;
        server_name api.taskmaster.com;

        location /.well-known/acme-challenge/ {
            root /var/www/certbot;
        }

        location / {
            return 301 https://$host$request_uri;
        }
    }

    server {
        listen 443 ssl;
        server_name api.taskmaster.com;

        ssl_certificate /etc/nginx/ssl/fullchain.pem;
        ssl_certificate_key /etc/nginx/ssl/privkey.pem;

        location / {
            proxy_pass http://api;
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
        }
    }
}
```

---

## Partie 7 : Scalabilité et performance

### 7.1 Load Balancing

Avec plusieurs instances de votre API :

```yaml
# docker-compose-scale.yml
version: '3.8'

services:
  api:
    build: .
    environment:
      - DATABASE_HOST=db
    depends_on:
      - db
    deploy:
      replicas: 3  # 3 instances de l'API

  nginx:
    image: nginx:alpine
    ports:
      - "80:80"
    volumes:
      - ./nginx-lb.conf:/etc/nginx/nginx.conf
    depends_on:
      - api

  db:
    image: postgres:14-alpine
    # ...
```

**nginx-lb.conf** :

```nginx
http {
    upstream api_backend {
        least_conn;  # Algorithme de répartition de charge
        server api:8080 max_fails=3 fail_timeout=30s;
        server api:8081 max_fails=3 fail_timeout=30s;
        server api:8082 max_fails=3 fail_timeout=30s;
    }

    server {
        listen 80;

        location / {
            proxy_pass http://api_backend;
            # Headers...
        }
    }
}
```

### 7.2 Mise en cache (Redis)

```pascal
unit uCacheManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TCacheManager = class
  private
    // Implémentation avec Redis client
    procedure ConnectToRedis;
  public
    function Get(const AKey: string): string;
    procedure Set(const AKey, AValue: string; AExpireSeconds: Integer = 300);
    procedure Delete(const AKey: string);
    function Exists(const AKey: string): Boolean;
  end;

implementation

// Dans vos API, utiliser le cache
function TTaskAPI.GetTasks(const AToken: string): string;
var
  CacheKey: string;
  CachedData: string;
begin
  CacheKey := Format('tasks:user:%d', [UserID]);

  // Vérifier le cache
  if CacheManager.Exists(CacheKey) then
  begin
    Result := CacheManager.Get(CacheKey);
    Exit;
  end;

  // Récupérer de la base de données
  Tasks := DatabaseManager.GetUserTasks(UserID);
  Result := SerializeTasks(Tasks);

  // Mettre en cache pour 5 minutes
  CacheManager.Set(CacheKey, Result, 300);
end;
```

### 7.3 Optimisation des requêtes

```pascal
// Pagination
function TTaskAPI.GetTasksPaginated(const AToken: string;
  APage, APageSize: Integer): string;
var
  Query: TFDQuery;
  Offset: Integer;
begin
  Offset := (APage - 1) * APageSize;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DatabaseManager.Connection;
    Query.SQL.Text :=
      'SELECT * FROM tasks ' +
      'WHERE user_id = :user_id ' +
      'ORDER BY created_at DESC ' +
      'LIMIT :limit OFFSET :offset';

    Query.ParamByName('user_id').AsInteger := UserID;
    Query.ParamByName('limit').AsInteger := APageSize;
    Query.ParamByName('offset').AsInteger := Offset;

    Query.Open;

    // Convertir en JSON...
  finally
    Query.Free;
  end;
end;
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous avez créé une application cloud SaaS complète avec Delphi. Vous maîtrisez maintenant :

✅ **Architecture cloud** : Compréhension des systèmes distribués
✅ **API REST** : Création d'endpoints professionnels
✅ **Authentification JWT** : Sécurisation des API
✅ **Base de données cloud** : PostgreSQL en production
✅ **Déploiement** : Docker, AWS, Azure
✅ **Scalabilité** : Load balancing, cache
✅ **Tests** : Unitaires et d'intégration

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Créer des API REST robustes et scalables
🎯 Implémenter l'authentification sécurisée
🎯 Gérer des bases de données cloud
🎯 Déployer sur AWS/Azure
🎯 Optimiser les performances
🎯 Maintenir un système en production

### Prochaines étapes

**Améliorations suggérées** :
- WebSockets pour le temps réel
- Notifications push
- Système de paiement (Stripe)
- Analytics et monitoring
- API GraphQL
- Microservices

### Ressources

**Documentation** :
- [AWS Documentation](https://docs.aws.amazon.com)
- [Azure Documentation](https://docs.microsoft.com/azure)
- [PostgreSQL Documentation](https://www.postgresql.org/docs/)

**Outils** :
- Postman pour tester les API
- Docker Desktop
- AWS CLI / Azure CLI
- Redis Desktop Manager

### Message final

Les applications cloud et SaaS représentent l'avenir du logiciel. Avec Delphi, vous pouvez créer des backends performants et fiables qui rivalisent avec n'importe quelle technologie moderne.

**Bon développement cloud avec Delphi !** ☁️🚀

---

⏭️ [Applications PWA (Progressive Web Apps) avec Delphi](/19-projets-avances/06-applications-pwa-avec-delphi.md)
