🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 23.4 Utilisation de WebBroker et DataSnap

## Introduction

WebBroker et DataSnap sont deux technologies historiques mais toujours pertinentes de Delphi pour créer des applications web et des systèmes distribués. Bien qu'elles soient apparues dans les années 1990 et 2000, elles continuent d'alimenter de nombreuses applications en production aujourd'hui.

Dans cette section, nous allons explorer ces technologies, comprendre quand et pourquoi les utiliser, et voir comment elles se comparent aux approches plus modernes présentées dans les sections précédentes.

## WebBroker : Les fondations du web avec Delphi

### Qu'est-ce que WebBroker ?

**WebBroker** est le framework de base intégré à Delphi pour créer des applications web côté serveur. C'est la "fondation" sur laquelle d'autres technologies comme IntraWeb ont été construites.

**Analogie :** Si vous comparez le développement web à la construction d'une maison :
- WebBroker = Les fondations et la structure de base
- IntraWeb = Une maison préfabriquée clé en main
- Frameworks modernes (Horse) = Construction modulaire moderne

**Historique :**
- Introduit dans Delphi 3 (1997)
- Conçu pour CGI et ISAPI
- Toujours inclus dans toutes les versions de Delphi
- Base pour de nombreuses autres technologies Delphi

### Pourquoi WebBroker existe-t-il ?

À l'époque de sa création, les applications web se développaient principalement avec :
- **CGI (Common Gateway Interface)** : Scripts exécutables appelés par le serveur web
- **ISAPI (Internet Server API)** : Extensions pour IIS (Internet Information Services)
- **Apache modules** : Modules pour le serveur Apache

WebBroker a permis aux développeurs Delphi de créer ces types d'applications sans quitter leur environnement familier.

### Architecture WebBroker

```
┌─────────────────────────────────────────────┐
│         Serveur Web (IIS / Apache)          │
└────────────────┬────────────────────────────┘
                 │
                 │ Requête HTTP
                 │
┌────────────────┴────────────────────────────┐
│      Application WebBroker (Delphi)         │
│  ┌──────────────────────────────────────┐   │
│  │      TWebModule                      │   │
│  │  ┌────────────────────────────────┐  │   │
│  │  │   TWebActionItem (Actions)     │  │   │
│  │  │   - /home                      │  │   │
│  │  │   - /clients                   │  │   │
│  │  │   - /api/data                  │  │   │
│  │  └────────────────────────────────┘  │   │
│  └──────────────────────────────────────┘   │
└─────────────────────────────────────────────┘
                 │
                 │ Réponse HTML/JSON
                 ↓
         Navigateur client
```

### Types d'applications WebBroker

**1. Application CGI**
```
Application exécutable (.exe)
- Lancée pour chaque requête
- Simple mais peu performante
- Idéale pour tests et développement
```

**2. Application ISAPI (Windows/IIS)**
```
DLL chargée en mémoire par IIS
- Reste en mémoire
- Très performante
- Windows uniquement
```

**3. Module Apache**
```
DLL/SO chargée par Apache
- Multi-plateforme (Windows/Linux)
- Bonne performance
- Plus complexe à déployer
```

**4. Application Standalone**
```
Serveur HTTP intégré
- Auto-hébergée
- Parfaite pour développement
- Déploiement simple
```

### Création d'une application WebBroker

#### Étape 1 : Créer le projet

Dans Delphi :
1. File → New → Other
2. Delphi Projects → WebBroker
3. Choisir le type (Standalone pour débuter)

#### Étape 2 : Le WebModule

Le **TWebModule** est le cœur de votre application. C'est un conteneur pour vos actions web.

```pascal
unit WebModuleUnit1;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp;

type
  TWebModule1 = class(TWebModule)
    procedure WebModule1DefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  WebModuleClass: TComponentClass = TWebModule1;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Ma première application WebBroker</title></head>' +
    '<body>' +
    '<h1>Bonjour depuis WebBroker !</h1>' +
    '<p>Vous avez accédé à : ' + Request.PathInfo + '</p>' +
    '</body>' +
    '</html>';
  Handled := True;
end;

end.
```

#### Étape 3 : Gérer les actions

Les **Actions** permettent de router les requêtes vers différents gestionnaires :

```pascal
type
  TWebModule1 = class(TWebModule)
    ActionHome: TWebActionItem;
    ActionClients: TWebActionItem;
    ActionAPI: TWebActionItem;

    procedure ActionHomeAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionClientsAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure ActionAPIAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  end;

implementation

// Action pour la page d'accueil : /home
procedure TWebModule1.ActionHomeAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<body>' +
    '<h1>Page d''accueil</h1>' +
    '<ul>' +
    '<li><a href="/clients">Liste des clients</a></li>' +
    '<li><a href="/api/data">API JSON</a></li>' +
    '</ul>' +
    '</body>' +
    '</html>';
  Handled := True;
end;

// Action pour la liste des clients : /clients
procedure TWebModule1.ActionClientsAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  HTML: string;
begin
  HTML := '<html><body><h1>Liste des clients</h1><ul>';

  // Ici, vous ajouteriez le code pour récupérer les clients depuis la base
  HTML := HTML + '<li>Client 1</li>';
  HTML := HTML + '<li>Client 2</li>';
  HTML := HTML + '<li>Client 3</li>';

  HTML := HTML + '</ul></body></html>';

  Response.Content := HTML;
  Handled := True;
end;

// Action pour une API REST : /api/data
procedure TWebModule1.ActionAPIAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'application/json';
  Response.Content :=
    '{' +
    '  "success": true,' +
    '  "data": [' +
    '    {"id": 1, "nom": "Dupont"},' +
    '    {"id": 2, "nom": "Martin"}' +
    '  ]' +
    '}';
  Handled := True;
end;
```

### Fonctionnalités avancées de WebBroker

#### 1. Gestion des méthodes HTTP

```pascal
procedure TWebModule1.ActionClientsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  case Request.MethodType of
    mtGet:
      begin
        // Récupérer les clients
        Response.Content := GetClientsList;
      end;
    mtPost:
      begin
        // Créer un nouveau client
        CreateClient(Request.Content);
        Response.StatusCode := 201;
      end;
    mtPut:
      begin
        // Modifier un client
        UpdateClient(Request.Content);
      end;
    mtDelete:
      begin
        // Supprimer un client
        DeleteClient(Request.PathInfo);
        Response.StatusCode := 204;
      end;
  end;
  Handled := True;
end;
```

#### 2. Récupération des paramètres

```pascal
procedure TWebModule1.ActionSearchAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  SearchTerm: string;
  Page: Integer;
begin
  // Paramètres GET : /search?q=dupont&page=2
  SearchTerm := Request.QueryFields.Values['q'];
  Page := StrToIntDef(Request.QueryFields.Values['page'], 1);

  // Paramètres POST (formulaire)
  if Request.MethodType = mtPost then
  begin
    SearchTerm := Request.ContentFields.Values['search'];
  end;

  Response.Content := Format('<p>Recherche : %s (page %d)</p>',
    [SearchTerm, Page]);
  Handled := True;
end;
```

#### 3. Sessions utilisateur

```pascal
procedure TWebModule1.ActionLoginAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Username: string;
  SessionID: string;
begin
  Username := Request.ContentFields.Values['username'];

  // Créer ou récupérer la session
  SessionID := Request.Cookie;
  if SessionID = '' then
  begin
    SessionID := CreateGUID; // Générer un ID unique
    Response.SetCookieField('SessionID', SessionID, 0, '/', '', False, False);
  end;

  // Stocker les données de session
  // (nécessite un système de cache côté serveur)
  StoreSessionData(SessionID, 'username', Username);

  Response.Content := '<p>Connexion réussie !</p>';
  Handled := True;
end;
```

#### 4. Intégration avec base de données

```pascal
uses
  FireDAC.Comp.Client, System.JSON;

procedure TWebModule1.ActionAPIClientsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1; // Connexion FireDAC
    Query.SQL.Text := 'SELECT id, nom, prenom FROM clients';
    Query.Open;

    JSONArray := TJSONArray.Create;
    try
      while not Query.Eof do
      begin
        JSONObject := TJSONObject.Create;
        JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
        JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
        JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
        JSONArray.Add(JSONObject);
        Query.Next;
      end;

      Response.ContentType := 'application/json';
      Response.Content := JSONArray.ToString;
    finally
      JSONArray.Free;
    end;
  finally
    Query.Free;
  end;

  Handled := True;
end;
```

### Avantages de WebBroker

✅ **Inclus dans Delphi** - Aucune dépendance externe
✅ **Léger et simple** - Peu de concepts à apprendre
✅ **Contrôle total** - Vous gérez tout le flux HTTP
✅ **Multi-plateforme** - Fonctionne sur Windows et Linux
✅ **Stable et éprouvé** - Plus de 25 ans d'existence
✅ **Base pour d'autres frameworks** - IntraWeb, DataSnap REST

### Limitations de WebBroker

❌ **Bas niveau** - Génération HTML manuelle
❌ **Pas de composants visuels** - Tout en code
❌ **Gestion manuelle** - Sessions, sécurité, validation
❌ **Interface datée** - Approche années 1990
❌ **Verbeux** - Beaucoup de code pour des tâches simples

## DataSnap : Architecture client-serveur multi-tiers

### Qu'est-ce que DataSnap ?

**DataSnap** est une technologie Delphi pour créer des applications distribuées multi-tiers. Elle permet de séparer la logique métier du client et de la base de données.

**Concept clé :** DataSnap crée un "serveur applicatif" qui expose des méthodes que les clients peuvent appeler à distance.

### Architecture DataSnap

```
┌─────────────────┐
│  Client Delphi  │
│   (Windows/     │
│   Mobile/Web)   │
└────────┬────────┘
         │
         │ Appel de méthodes distantes
         │ (REST, TCP/IP, HTTP)
         │
┌────────┴────────────────────────┐
│    Serveur DataSnap             │
│  ┌──────────────────────────┐   │
│  │  Server Methods          │   │
│  │  (Logique métier)        │   │
│  └──────────┬───────────────┘   │
└─────────────┼───────────────────┘
              │
              │ Requêtes SQL
              │
┌─────────────┴───────────────┐
│    Base de données          │
│    (MySQL, SQL Server...)   │
└─────────────────────────────┘
```

### Concepts fondamentaux

**1. Server Methods (Méthodes serveur)**
- Classes exposant des méthodes appelables à distance
- Contiennent la logique métier
- Sécurisent l'accès aux données

**2. Protocoles de communication**
- **REST/JSON** : Pour clients web et mobiles
- **TCP/IP** : Pour clients Delphi (très rapide)
- **HTTP/HTTPS** : Communication standard web

**3. Sérialisation automatique**
- Conversion automatique des types Delphi en JSON
- Marshaling/Unmarshaling des objets
- Support des types complexes

### Création d'un serveur DataSnap

#### Étape 1 : Créer le projet serveur

Dans Delphi :
1. File → New → Other
2. DataSnap Server → Wizard
3. Choisir le type de serveur (Standalone pour débuter)
4. Sélectionner les protocoles (REST, TCP/IP)

#### Étape 2 : Créer les Server Methods

```pascal
unit ServerMethodsUnit1;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer,
  Datasnap.DSAuth, FireDAC.Comp.Client;

type
  {$METHODINFO ON}
  TServerMethods1 = class(TDSServerModule)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function EchoString(Value: string): string;
    function GetServerTime: TDateTime;
    function GetClients: TJSONArray;
    function CreateClient(const Nom, Prenom, Email: string): Integer;
  end;
  {$METHODINFO OFF}

implementation

uses
  System.JSON, Data.DBXPlatform;

{$R *.dfm}

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := 'Écho : ' + Value;
end;

function TServerMethods1.GetServerTime: TDateTime;
begin
  Result := Now;
end;

function TServerMethods1.GetClients: TJSONArray;
var
  Query: TFDQuery;
  JSONObject: TJSONObject;
begin
  Result := TJSONArray.Create;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT id, nom, prenom, email FROM clients';
    Query.Open;

    while not Query.Eof do
    begin
      JSONObject := TJSONObject.Create;
      JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
      JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
      JSONObject.AddPair('prenom', Query.FieldByName('prenom').AsString);
      JSONObject.AddPair('email', Query.FieldByName('email').AsString);
      Result.Add(JSONObject);

      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TServerMethods1.CreateClient(const Nom, Prenom, Email: string): Integer;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email)';
    Query.ParamByName('nom').AsString := Nom;
    Query.ParamByName('prenom').AsString := Prenom;
    Query.ParamByName('email').AsString := Email;
    Query.ExecSQL;

    // Récupérer l'ID généré
    Query.SQL.Text := 'SELECT LAST_INSERT_ID() as id';
    Query.Open;
    Result := Query.FieldByName('id').AsInteger;
  finally
    Query.Free;
  end;
end;

end.
```

**Note importante :** Les directives `{$METHODINFO ON}` et `{$METHODINFO OFF}` sont essentielles pour que DataSnap puisse exposer les méthodes publiques.

#### Étape 3 : Démarrer le serveur

```pascal
// Le serveur standalone démarre automatiquement
// ou via un bouton dans l'interface

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  DSServer1.Start;
  ShowMessage('Serveur DataSnap démarré sur le port ' +
    IntToStr(DSHTTPService1.HttpPort));
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  DSServer1.Stop;
  ShowMessage('Serveur DataSnap arrêté');
end;
```

### Création d'un client DataSnap

#### Client Delphi (VCL/FMX)

```pascal
unit ClientUnit1;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Data.DB, Datasnap.DSConnect, IPPeerClient, System.JSON;

type
  TForm1 = class(TForm)
    SQLConnection1: TSQLConnection;
    DSProviderConnection1: TDSProviderConnection;
    ButtonTestEcho: TButton;
    ButtonGetClients: TButton;
    Memo1: TMemo;

    procedure ButtonTestEchoClick(Sender: TObject);
    procedure ButtonGetClientsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FServerMethods: TServerMethods1Client;
  public
    { Public declarations }
  end;

implementation

uses
  ClientModuleUnit1; // Proxy généré automatiquement

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Connexion au serveur
  SQLConnection1.Connected := True;

  // Créer l'instance du proxy
  FServerMethods := TServerMethods1Client.Create(SQLConnection1.DBXConnection);
end;

procedure TForm1.ButtonTestEchoClick(Sender: TObject);
var
  Response: string;
begin
  // Appel de la méthode distante
  Response := FServerMethods.EchoString('Bonjour depuis le client');
  ShowMessage(Response);
end;

procedure TForm1.ButtonGetClientsClick(Sender: TObject);
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: Integer;
begin
  Memo1.Clear;

  // Appel de la méthode distante qui retourne JSON
  JSONArray := FServerMethods.GetClients;
  try
    for i := 0 to JSONArray.Count - 1 do
    begin
      JSONObject := JSONArray.Items[i] as TJSONObject;
      Memo1.Lines.Add(Format('ID: %s, Nom: %s %s',
        [JSONObject.GetValue('id').Value,
         JSONObject.GetValue('prenom').Value,
         JSONObject.GetValue('nom').Value]));
    end;
  finally
    JSONArray.Free;
  end;
end;

end.
```

#### Client REST (JavaScript, autre langage)

DataSnap expose automatiquement une API REST :

```javascript
// JavaScript - Appel REST vers DataSnap
fetch('http://localhost:8080/datasnap/rest/TServerMethods1/GetClients')
  .then(response => response.json())
  .then(data => {
    console.log('Clients:', data.result);
  });

// Avec paramètres
fetch('http://localhost:8080/datasnap/rest/TServerMethods1/EchoString/["Hello"]')
  .then(response => response.json())
  .then(data => {
    console.log('Réponse:', data.result);
  });
```

### Fonctionnalités avancées de DataSnap

#### 1. Authentification

```pascal
unit AuthenticationUnit;

interface

uses
  Datasnap.DSAuth;

type
  TMyAuthentication = class(TDSAuthenticationManager)
  public
    function Authenticate(const AuthUserName, AuthPassword: string;
      const AdditionalData: string): Boolean; override;
  end;

implementation

function TMyAuthentication.Authenticate(const AuthUserName, AuthPassword: string;
  const AdditionalData: string): Boolean;
begin
  // Vérifier les identifiants
  // (en production, vérifier contre une base de données)
  Result := (AuthUserName = 'admin') and (AuthPassword = 'secret123');
end;

end.
```

Configuration dans le serveur :

```pascal
procedure TServerContainer1.DSServer1Connect(DSConnectEventObject: TDSConnectEventObject);
begin
  // Activer l'authentification
  DSConnectEventObject.ChannelInfo.LoginRequired := True;
end;
```

#### 2. Callbacks (notifications serveur → client)

```pascal
// Côté serveur - Envoyer une notification
type
  TServerMethods1 = class(TDSServerModule)
  public
    function RegisterCallback(const ClientId: string): Boolean;
    procedure BroadcastMessage(const Message: string);
  end;

var
  Callbacks: TList<TDSServerCallbackChannelManager>;

procedure TServerMethods1.BroadcastMessage(const Message: string);
var
  i: Integer;
begin
  for i := 0 to Callbacks.Count - 1 do
  begin
    // Envoyer le message à tous les clients connectés
    Callbacks[i].Broadcast(TJSONString.Create(Message));
  end;
end;

// Côté client - Recevoir les notifications
procedure TForm1.OnServerCallback(Sender: TObject; const Arg: TJSONValue);
begin
  ShowMessage('Notification serveur : ' + Arg.Value);
end;
```

#### 3. Pooling de connexions

```pascal
procedure TServerContainer1.DSServer1Prepare(Sender: TObject);
begin
  // Configuration du pool de connexions
  DSServer1.AutoStart := True;

  // Nombre maximum de connexions simultanées
  DSHTTPService1.HttpServer.MaxConnections := 100;

  // Timeout des connexions inactives
  DSHTTPService1.HttpServer.KeepAlive := True;
  DSHTTPService1.HttpServer.Timeout := 30000; // 30 secondes
end;
```

#### 4. Filtres et intercepteurs

```pascal
type
  TMyServerFilter = class(TTransportFilter)
  public
    function ProcessInput(const Data: TBytes): Boolean; override;
    function ProcessOutput(const Data: TBytes): Boolean; override;
  end;

function TMyServerFilter.ProcessInput(const Data: TBytes): Boolean;
begin
  // Traiter les données entrantes
  // (logging, validation, décompression...)
  Result := True;
end;

function TMyServerFilter.ProcessOutput(const Data: TBytes): Boolean;
begin
  // Traiter les données sortantes
  // (compression, chiffrement...)
  Result := True;
end;
```

### Avantages de DataSnap

✅ **Architecture multi-tiers** - Séparation claire des responsabilités
✅ **Protocoles multiples** - REST, TCP/IP, HTTP
✅ **Clients multiples** - Delphi, JavaScript, autres langages
✅ **Sérialisation automatique** - Conversion types Delphi ↔ JSON
✅ **Callbacks bidirectionnels** - Communication serveur → client
✅ **Authentification intégrée** - Sécurité native
✅ **Pooling de connexions** - Gestion efficace des ressources

### Limitations de DataSnap

❌ **Complexité** - Configuration plus lourde que frameworks modernes
❌ **Verbosité** - Beaucoup de code pour des tâches simples
❌ **Performance REST** - Moins rapide que Horse ou frameworks dédiés
❌ **Documentation limitée** - Moins de ressources que technologies modernes
❌ **Courbe d'apprentissage** - Nombreux concepts à maîtriser
❌ **Orienté Delphi** - Optimisé pour communication Delphi-to-Delphi

## Comparaison WebBroker, DataSnap et technologies modernes

### Tableau comparatif

| Critère | WebBroker | DataSnap | Horse/MARS | RAD Server |
|---------|-----------|----------|------------|------------|
| **Année d'introduction** | 1997 | 2001 | 2018+ | 2016 |
| **Courbe d'apprentissage** | Moyenne | Élevée | Faible | Moyenne |
| **Performance** | Bonne | Bonne | Excellente | Très bonne |
| **Simplicité** | Bas niveau | Complexe | Très simple | Simple |
| **REST natif** | Manuel | Oui | Oui | Oui |
| **Client Delphi natif** | Non | Oui | Non | Oui |
| **Callbacks** | Non | Oui | Avec WebSockets | Oui |
| **Documentation** | Moyenne | Moyenne | Excellente | Bonne |
| **Communauté** | Stable | Stable | Croissante | Active |
| **Coût** | Gratuit | Gratuit | Gratuit | Payant (Enterprise) |

### Matrice de décision

**Utiliser WebBroker quand :**
- Vous avez besoin d'un contrôle bas niveau sur HTTP
- Vous créez un composant ou framework web
- Vous avez des contraintes de compatibilité anciennes
- Vous voulez éviter les dépendances externes
- Votre application est très simple

**Utiliser DataSnap quand :**
- Vous créez une architecture multi-tiers classique
- Vous avez besoin de callbacks bidirectionnels
- Vos clients sont principalement Delphi
- Vous migrez une application DataSnap existante
- Vous voulez la communication TCP/IP rapide

**Utiliser frameworks modernes (Horse, MARS) quand :**
- Vous créez une nouvelle API REST
- Vous privilégiez la simplicité et la rapidité de développement
- Vous voulez une syntaxe moderne et élégante
- Vos clients sont web/mobile/multi-technologie
- Vous voulez rejoindre une communauté active

**Utiliser RAD Server quand :**
- Vous êtes une entreprise avec budget
- Vous avez besoin d'analytics et monitoring intégrés
- Vous voulez une console d'administration
- Vous développez pour mobile (EMS)
- Vous avez besoin de support commercial

## Migration et modernisation

### Migrer de WebBroker vers Horse

**Avant (WebBroker) :**
```pascal
procedure TWebModule1.ActionClientsAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'application/json';
  Response.Content := '{"clients": [...]}';
  Handled := True;
end;
```

**Après (Horse) :**
```pascal
THorse.Get('/clients',
  procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
  begin
    Res.Send('{"clients": [...]}');
  end);
```

### Moderniser DataSnap

**Option 1 : Garder DataSnap, améliorer l'API REST**
```pascal
// Ajouter un WebModule pour exposer une API REST moderne
// en utilisant les Server Methods existants
```

**Option 2 : Migrer vers Horse + conserver la logique**
```pascal
// Extraire la logique métier des Server Methods
// Créer des contrôleurs Horse qui appellent cette logique
```

**Option 3 : Architecture hybride**
```
┌─────────────────┐
│  Clients Delphi │──→ DataSnap (TCP/IP rapide)
└─────────────────┘

┌─────────────────┐
│ Clients Web/    │──→ Horse API REST
│ Mobile          │
└─────────────────┘
         ↓
    ┌─────────────────┐
    │ Logique métier  │
    │    partagée     │
    └─────────────────┘
```

## Cas d'usage réels

### Scénario 1 : Application legacy

**Situation :** Application WebBroker en production depuis 15 ans

**Recommandation :**
- ✅ Continuer WebBroker si stable et fonctionnel
- ✅ Moderniser progressivement l'UI avec CSS/JavaScript
- ✅ Ajouter une API REST Horse pour nouveaux clients
- ❌ Ne pas tout réécrire si ce n'est pas nécessaire

### Scénario 2 : Système multi-tiers existant

**Situation :** Architecture DataSnap avec clients Delphi lourds

**Recommandation :**
- ✅ Garder DataSnap pour clients Delphi existants
- ✅ Ajouter des clients web légers (TMS Web Core)
- ✅ Exposer API REST pour mobilité
- ✅ Moderniser progressivement

### Scénario 3 : Nouveau projet

**Situation :** Création d'un nouveau système web

**Recommandation :**
- ✅ Privilégier Horse ou MARS pour l'API
- ✅ Frontend moderne (TMS Web Core, React, Vue.js)
- ❌ Éviter WebBroker/DataSnap sauf besoin spécifique
- ✅ Architecture REST découplée

## Bonnes pratiques

### Pour WebBroker

**1. Utiliser des templates HTML**
```pascal
function LoadHTMLTemplate(const FileName: string): string;
var
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  try
    StringStream := TStringStream.Create;
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure TWebModule1.ActionHomeAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  HTML: string;
begin
  HTML := LoadHTMLTemplate('templates/home.html');
  HTML := StringReplace(HTML, '{{TITLE}}', 'Accueil', [rfReplaceAll]);
  Response.Content := HTML;
  Handled := True;
end;
```

**2. Séparer la logique métier**
```pascal
// BusinessLogic.pas
unit BusinessLogic;

interface

function GetClientsList: string; // Retourne JSON

implementation

function GetClientsList: string;
begin
  // Logique de récupération des clients
  Result := '{"clients": [...]}';
end;

end.

// Dans WebModule
procedure TWebModule1.ActionAPIAction(...);
begin
  Response.Content := BusinessLogic.GetClientsList;
end;
```

### Pour DataSnap

**1. Limiter la complexité des méthodes**
```pascal
// ❌ Mauvais - Trop complexe
function GetEverything: TJSONObject;

// ✅ Bon - Méthodes spécialisées
function GetClients: TJSONArray;
function GetOrders: TJSONArray;
function GetClientOrders(ClientID: Integer): TJSONArray;
```

**2. Gérer les erreurs proprement**
```pascal
function TServerMethods1.GetClient(ID: Integer): TJSONObject;
begin
  try
    Result := FetchClientFromDB(ID);
    if Result = nil then
      raise Exception.Create('Client non trouvé');
  except
    on E: Exception do
    begin
      // Logger l'erreur
      LogError(E.Message);
      raise; // Re-lever pour que le client soit informé
    end;
  end;
end;
```

**3. Implémenter un timeout**
```pascal
procedure TServerMethods1.LongRunningOperation;
var
  StartTime: TDateTime;
  Timeout: Integer;
begin
  StartTime := Now;
  Timeout := 30; // 30 secondes

  while ProcessingNotComplete do
  begin
    // Vérifier le timeout
    if SecondsBetween(Now, StartTime) > Timeout then
      raise Exception.Create('Timeout dépassé');

    // Continuer le traitement
    DoSomeWork;
  end;
end;
```

## Conclusion

WebBroker et DataSnap représentent les **fondations historiques** du développement web et distribué avec Delphi. Bien que des solutions plus modernes existent aujourd'hui, ces technologies conservent leur pertinence dans certains contextes :

**WebBroker** reste utile pour :
- ✅ Comprendre les fondamentaux du web avec Delphi
- ✅ Créer des composants de bas niveau
- ✅ Maintenir des applications legacy
- ✅ Projets nécessitant un contrôle total

**DataSnap** reste pertinent pour :
- ✅ Applications multi-tiers existantes
- ✅ Communication rapide entre clients Delphi
- ✅ Systèmes nécessitant callbacks bidirectionnels
- ✅ Architectures nécessitant plusieurs protocoles

Pour de **nouveaux projets**, il est généralement recommandé d'utiliser des frameworks plus modernes comme **Horse, MARS ou RAD Server**, qui offrent une meilleure productivité, une syntaxe plus élégante et une communauté plus active.

Cependant, comprendre WebBroker et DataSnap reste précieux car :
- De nombreuses applications en production les utilisent
- Ils constituent la base de nombreuses autres technologies Delphi
- Leur maîtrise vous donne une compréhension approfondie du web avec Delphi
- Vous serez capable de maintenir et moderniser des systèmes existants

Dans la section suivante, nous explorerons le développement de sites web dynamiques avec des techniques modernes de templating et de génération de contenu.

⏭️ [Développement de sites Web dynamiques](/23-conception-dapplications-web-avec-delphi/05-developpement-de-sites-web-dynamiques.md)
