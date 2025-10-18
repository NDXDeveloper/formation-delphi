🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.5 Architecture client-serveur

## Introduction à l'architecture client-serveur

### Qu'est-ce qu'une architecture client-serveur ?

L'**architecture client-serveur** est un modèle de conception où les tâches sont réparties entre deux types d'entités :

**Le Serveur :**
- Fournit des services et des ressources
- Traite les demandes
- Gère les données centralisées
- Tourne en permanence
- Exemple : serveur de base de données, serveur web

**Le Client :**
- Demande des services
- Présente l'interface utilisateur
- Effectue des traitements locaux
- Se connecte au besoin
- Exemple : application desktop, application mobile

**Analogie simple :**
Imaginez un restaurant : le **client** (vous) consulte le menu et passe commande, tandis que le **serveur** (le personnel) prend la commande, la transmet en cuisine, et vous apporte votre plat. La cuisine représente le serveur de données.

### Avantages de l'architecture client-serveur

**Centralisation :**
- Une seule source de vérité pour les données
- Mise à jour simplifiée
- Cohérence des données garantie

**Sécurité :**
- Contrôle d'accès centralisé
- Données protégées sur le serveur
- Sauvegarde facilitée

**Évolutivité :**
- Ajout de clients sans modifier le serveur
- Montée en charge progressive
- Répartition de charge possible

**Maintenance :**
- Correction de bugs centralisée
- Déploiement simplifié
- Monitoring centralisé

**Partage de ressources :**
- Plusieurs utilisateurs accèdent aux mêmes données
- Collaboration facilitée
- Économie de ressources

### Inconvénients à considérer

**Dépendance au réseau :**
- Nécessite une connexion fiable
- Latence possible
- Gestion de la déconnexion

**Point de défaillance unique :**
- Si le serveur tombe, tous les clients sont affectés
- Nécessité de redondance

**Coût :**
- Infrastructure serveur à maintenir
- Bande passante réseau
- Expertise technique requise

**Complexité :**
- Plus difficile à développer qu'une application standalone
- Gestion des versions client/serveur
- Synchronisation nécessaire

## Les différents modèles d'architecture

### Architecture 2-tiers (deux niveaux)

**Structure :**
```
┌─────────────┐         ┌──────────────┐
│   Client    │ ←────→  │   Serveur    │
│  (UI + BL)  │         │  (Database)  │
└─────────────┘         └──────────────┘
```

**Caractéristiques :**
- Le client communique directement avec la base de données
- La logique métier est dans le client
- Simple à mettre en œuvre
- Convient aux petites applications

**Exemple typique :**
```pascal
// Client se connecte directement à MySQL
FDConnection1.Params.Database := 'mabase';
FDConnection1.Params.UserName := 'utilisateur';
FDConnection1.Params.Password := 'motdepasse';
FDConnection1.Connected := True;

// Le client fait les requêtes directement
FDQuery1.SQL.Text := 'SELECT * FROM clients';
FDQuery1.Open;
```

**Avantages :**
- Simple et rapide à développer
- Performances excellentes en LAN
- Peu de couches = moins de complexité

**Inconvénients :**
- Logique métier dispersée dans chaque client
- Difficile à maintenir
- Problèmes de sécurité (accès direct à la BD)
- Scalabilité limitée
- Dépendance forte au SGBD

### Architecture 3-tiers (trois niveaux)

**Structure :**
```
┌─────────────┐         ┌──────────────┐         ┌──────────────┐
│   Client    │ ←────→  │  Serveur     │ ←────→  │   Serveur    │
│  (UI only)  │         │  Application │         │   Database   │
│             │         │  (Logique)   │         │              │
└─────────────┘         └──────────────┘         └──────────────┘
  Présentation            Logique Métier            Données
```

**Caractéristiques :**
- Séparation claire des responsabilités
- Le client ne connaît pas la base de données
- Logique métier centralisée sur le serveur applicatif
- Architecture professionnelle standard

**Les trois niveaux :**

1. **Niveau Présentation (Client)**
   - Interface utilisateur
   - Validation des saisies
   - Affichage des données

2. **Niveau Logique Métier (Serveur Application)**
   - Règles métier
   - Traitement des données
   - Validation avancée
   - Orchestration

3. **Niveau Données (Serveur BD)**
   - Stockage des données
   - Intégrité référentielle
   - Transactions
   - Sécurité des données

**Avantages :**
- Logique métier réutilisable
- Maintenance facilitée
- Sécurité renforcée
- Scalabilité améliorée
- Indépendance du SGBD

**Inconvénients :**
- Plus complexe à développer
- Plus de serveurs à gérer
- Latence supplémentaire
- Infrastructure plus coûteuse

### Architecture n-tiers (multi-niveaux)

**Structure :**
```
┌──────────┐    ┌────────────┐    ┌──────────┐    ┌─────────┐    ┌──────────┐
│  Client  │ ←→ │  API       │ ←→ │ Business │ ←→ │  Data   │ ←→ │ Database │
│   Web    │    │  Gateway   │    │  Layer   │    │ Access  │    │          │
└──────────┘    └────────────┘    └──────────┘    └─────────┘    └──────────┘
                                        ↕
                                   ┌──────────┐
                                   │ Services │
                                   │ Externes │
                                   └──────────┘
```

**Caractéristiques :**
- Architecture complexe pour grandes applications
- Chaque couche a une responsabilité spécifique
- Haute modularité
- Microservices possibles

**Couches typiques :**
- **Présentation** : Interface utilisateur
- **API Gateway** : Point d'entrée unique
- **Services Métier** : Logique métier modulaire
- **Intégration** : Services externes, APIs tierces
- **Accès aux données** : Repository pattern
- **Persistance** : Base(s) de données

**Avantages :**
- Très grande scalabilité
- Maintenance par équipe
- Remplacement de couches facile
- Test unitaire facilité
- Résilience élevée

**Inconvénients :**
- Complexité importante
- Coût de développement élevé
- Nécessite une expertise avancée
- Temps de développement long
- Infrastructure complexe

## Architecture client-serveur avec Delphi

### DataSnap : La solution Delphi

**DataSnap** est le framework Delphi pour créer des applications client-serveur multi-niveaux. Il permet de :
- Créer des serveurs d'applications
- Exposer des méthodes distantes
- Gérer la communication client-serveur
- Supporter plusieurs protocoles (TCP/IP, HTTP)

**Composants principaux :**

**Côté Serveur :**
- `TDSServer` : Le serveur DataSnap
- `TDSServerClass` : Définit les classes exposées
- `TDSServerMethod` : Méthodes exposées aux clients
- `TDSTCPServerTransport` : Transport TCP/IP
- `TDSHTTPService` : Service HTTP

**Côté Client :**
- `TSQLConnection` : Connexion au serveur
- `TDSProviderConnection` : Connexion DataSnap spécifique
- `TClientDataSet` : Dataset côté client
- `TDataSetProvider` : Fournisseur de données

### Créer un serveur DataSnap simple

**Étape 1 : Créer le projet serveur**

File → New → Other → DataSnap Server → DataSnap Server

Choisir "VCL Forms Application" pour un serveur standalone.

**Étape 2 : Créer une classe de méthodes serveur**

```pascal
unit ServerMethodsUnit;

interface

uses
  System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth;

type
  {$METHODINFO ON}
  TServerMethods = class(TDSServerModule)
  private
  public
    // Méthodes exposées aux clients
    function Echo(const Message: string): string;
    function Addition(A, B: Integer): Integer;
    function GetServerTime: TDateTime;
    function GetUserInfo(UserID: Integer): TJSONObject;
  end;
  {$METHODINFO OFF}

implementation

{$R *.dfm}

function TServerMethods.Echo(const Message: string): string;
begin
  Result := 'Serveur répond: ' + Message;
end;

function TServerMethods.Addition(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TServerMethods.GetServerTime: TDateTime;
begin
  Result := Now;
end;

function TServerMethods.GetUserInfo(UserID: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  try
    Result.AddPair('id', TJSONNumber.Create(UserID));
    Result.AddPair('nom', 'Utilisateur ' + IntToStr(UserID));
    Result.AddPair('email', 'user' + IntToStr(UserID) + '@example.com');
  except
    Result.Free;
    raise;
  end;
end;

end.
```

**Points importants :**
- `{$METHODINFO ON}` : Active les informations RTTI nécessaires
- Les méthodes publiques sont automatiquement exposées
- Peut retourner des types simples ou JSON

**Étape 3 : Configuration du serveur**

```pascal
unit ServerContainerUnit;

interface

uses
  System.SysUtils, System.Classes,
  Datasnap.DSTCPServerTransport,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Datasnap.DSAuth, IPPeerServer;

type
  TServerContainer = class(TDataModule)
    DSServer1: TDSServer;
    DSTCPServerTransport1: TDSTCPServerTransport;
    DSServerClass1: TDSServerClass;
    procedure DataModuleCreate(Sender: TObject);
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
  private
  public
  end;

var
  ServerContainer: TServerContainer;

implementation

uses
  ServerMethodsUnit;

{$R *.dfm}

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  // Configuration du port
  DSTCPServerTransport1.Port := 211;

  // Démarrer le serveur
  DSServer1.Start;
end;

procedure TServerContainer.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  // Associer la classe de méthodes serveur
  PersistentClass := TServerMethods;
end;

end.
```

**Étape 4 : Interface du serveur**

```pascal
unit FormServerUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormServer = class(TForm)
    ButtonStart: TButton;
    ButtonStop: TButton;
    MemoLog: TMemo;
    LabelStatus: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    procedure AjouterLog(const Message: string);
  public
  end;

var
  FormServer: TFormServer;

implementation

uses
  ServerContainerUnit;

{$R *.dfm}

procedure TFormServer.FormCreate(Sender: TObject);
begin
  ButtonStop.Enabled := False;
  LabelStatus.Caption := 'Serveur arrêté';
end;

procedure TFormServer.ButtonStartClick(Sender: TObject);
begin
  try
    ServerContainer.DSServer1.Start;

    ButtonStart.Enabled := False;
    ButtonStop.Enabled := True;
    LabelStatus.Caption := 'Serveur actif sur le port ' +
      IntToStr(ServerContainer.DSTCPServerTransport1.Port);

    AjouterLog('Serveur démarré avec succès');

  except
    on E: Exception do
    begin
      ShowMessage('Erreur de démarrage: ' + E.Message);
      AjouterLog('Erreur: ' + E.Message);
    end;
  end;
end;

procedure TFormServer.ButtonStopClick(Sender: TObject);
begin
  try
    ServerContainer.DSServer1.Stop;

    ButtonStart.Enabled := True;
    ButtonStop.Enabled := False;
    LabelStatus.Caption := 'Serveur arrêté';

    AjouterLog('Serveur arrêté');

  except
    on E: Exception do
      AjouterLog('Erreur d''arrêt: ' + E.Message);
  end;
end;

procedure TFormServer.AjouterLog(const Message: string);
begin
  MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
end;

end.
```

### Créer un client DataSnap

**Étape 1 : Générer le proxy client**

File → New → Other → DataSnap Server → DataSnap Client Module

Connecter au serveur et générer automatiquement les classes proxy.

**Étape 2 : Code client généré**

```pascal
unit ClientClassesUnit;

interface

uses
  System.Json, Data.DBXCommon, Data.DBXClient, Datasnap.DSProxy,
  System.Classes, System.SysUtils, Data.DB, Data.SqlExpr,
  Data.DBXDataSnap, Data.DBXJSON;

type
  TServerMethodsClient = class(TDSAdminClient)
  private
    FEchoCommand: TDBXCommand;
    FAdditionCommand: TDBXCommand;
    FGetServerTimeCommand: TDBXCommand;
    FGetUserInfoCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection;
      AInstanceOwner: Boolean); overload;
    destructor Destroy; override;

    function Echo(Message: string): string;
    function Addition(A: Integer; B: Integer): Integer;
    function GetServerTime: TDateTime;
    function GetUserInfo(UserID: Integer): TJSONObject;
  end;

implementation

{ TServerMethodsClient }

constructor TServerMethodsClient.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create(ADBXConnection);
end;

constructor TServerMethodsClient.Create(ADBXConnection: TDBXConnection;
  AInstanceOwner: Boolean);
begin
  inherited Create(ADBXConnection, AInstanceOwner);
end;

destructor TServerMethodsClient.Destroy;
begin
  FEchoCommand.Free;
  FAdditionCommand.Free;
  FGetServerTimeCommand.Free;
  FGetUserInfoCommand.Free;
  inherited;
end;

function TServerMethodsClient.Echo(Message: string): string;
begin
  if FEchoCommand = nil then
  begin
    FEchoCommand := FDBXConnection.CreateCommand;
    FEchoCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEchoCommand.Text := 'TServerMethods.Echo';
    FEchoCommand.Prepare;
  end;

  FEchoCommand.Parameters[0].Value.SetWideString(Message);
  FEchoCommand.ExecuteUpdate;
  Result := FEchoCommand.Parameters[1].Value.GetWideString;
end;

function TServerMethodsClient.Addition(A, B: Integer): Integer;
begin
  if FAdditionCommand = nil then
  begin
    FAdditionCommand := FDBXConnection.CreateCommand;
    FAdditionCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FAdditionCommand.Text := 'TServerMethods.Addition';
    FAdditionCommand.Prepare;
  end;

  FAdditionCommand.Parameters[0].Value.SetInt32(A);
  FAdditionCommand.Parameters[1].Value.SetInt32(B);
  FAdditionCommand.ExecuteUpdate;
  Result := FAdditionCommand.Parameters[2].Value.GetInt32;
end;

function TServerMethodsClient.GetServerTime: TDateTime;
begin
  if FGetServerTimeCommand = nil then
  begin
    FGetServerTimeCommand := FDBXConnection.CreateCommand;
    FGetServerTimeCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FGetServerTimeCommand.Text := 'TServerMethods.GetServerTime';
    FGetServerTimeCommand.Prepare;
  end;

  FGetServerTimeCommand.ExecuteUpdate;
  Result := FGetServerTimeCommand.Parameters[0].Value.AsDateTime;
end;

function TServerMethodsClient.GetUserInfo(UserID: Integer): TJSONObject;
begin
  if FGetUserInfoCommand = nil then
  begin
    FGetUserInfoCommand := FDBXConnection.CreateCommand;
    FGetUserInfoCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FGetUserInfoCommand.Text := 'TServerMethods.GetUserInfo';
    FGetUserInfoCommand.Prepare;
  end;

  FGetUserInfoCommand.Parameters[0].Value.SetInt32(UserID);
  FGetUserInfoCommand.ExecuteUpdate;
  Result := TJSONObject.ParseJSONValue(
    FGetUserInfoCommand.Parameters[1].Value.GetWideString) as TJSONObject;
end;

end.
```

**Étape 3 : Utiliser le client**

```pascal
unit FormClientUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Data.DBXDataSnap, Data.DBXCommon, Data.DB,
  Data.SqlExpr, ClientClassesUnit, System.Json;

type
  TFormClient = class(TForm)
    SQLConnection1: TSQLConnection;
    ButtonConnecter: TButton;
    ButtonDeconnecter: TButton;
    EditMessage: TEdit;
    ButtonEcho: TButton;
    MemoResultat: TMemo;
    Edit1: TEdit;
    Edit2: TEdit;
    ButtonAddition: TButton;
    ButtonHeureServeur: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonDeconnecterClick(Sender: TObject);
    procedure ButtonEchoClick(Sender: TObject);
    procedure ButtonAdditionClick(Sender: TObject);
    procedure ButtonHeureServeurClick(Sender: TObject);
  private
    FServerMethods: TServerMethodsClient;
    procedure AjouterLog(const Message: string);
  public
  end;

var
  FormClient: TFormClient;

implementation

{$R *.dfm}

procedure TFormClient.FormCreate(Sender: TObject);
begin
  // Configuration de la connexion
  SQLConnection1.DriverName := 'DataSnap';
  SQLConnection1.Params.Values['HostName'] := 'localhost';
  SQLConnection1.Params.Values['Port'] := '211';
  SQLConnection1.Params.Values['CommunicationProtocol'] := 'tcp/ip';

  ButtonDeconnecter.Enabled := False;
  ButtonEcho.Enabled := False;
  ButtonAddition.Enabled := False;
  ButtonHeureServeur.Enabled := False;
end;

procedure TFormClient.ButtonConnecterClick(Sender: TObject);
begin
  try
    SQLConnection1.Connected := True;

    // Créer le proxy client
    FServerMethods := TServerMethodsClient.Create(SQLConnection1.DBXConnection);

    AjouterLog('Connecté au serveur');

    ButtonConnecter.Enabled := False;
    ButtonDeconnecter.Enabled := True;
    ButtonEcho.Enabled := True;
    ButtonAddition.Enabled := True;
    ButtonHeureServeur.Enabled := True;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion: ' + E.Message);
      AjouterLog('Erreur: ' + E.Message);
    end;
  end;
end;

procedure TFormClient.ButtonDeconnecterClick(Sender: TObject);
begin
  try
    FServerMethods.Free;
    FServerMethods := nil;

    SQLConnection1.Connected := False;

    AjouterLog('Déconnecté du serveur');

    ButtonConnecter.Enabled := True;
    ButtonDeconnecter.Enabled := False;
    ButtonEcho.Enabled := False;
    ButtonAddition.Enabled := False;
    ButtonHeureServeur.Enabled := False;

  except
    on E: Exception do
      AjouterLog('Erreur de déconnexion: ' + E.Message);
  end;
end;

procedure TFormClient.ButtonEchoClick(Sender: TObject);
var
  Reponse: string;
begin
  try
    Reponse := FServerMethods.Echo(EditMessage.Text);
    AjouterLog(Reponse);
  except
    on E: Exception do
      AjouterLog('Erreur: ' + E.Message);
  end;
end;

procedure TFormClient.ButtonAdditionClick(Sender: TObject);
var
  A, B, Resultat: Integer;
begin
  try
    A := StrToInt(Edit1.Text);
    B := StrToInt(Edit2.Text);

    Resultat := FServerMethods.Addition(A, B);

    AjouterLog(Format('%d + %d = %d', [A, B, Resultat]));

  except
    on E: Exception do
      AjouterLog('Erreur: ' + E.Message);
  end;
end;

procedure TFormClient.ButtonHeureServeurClick(Sender: TObject);
var
  HeureServeur: TDateTime;
begin
  try
    HeureServeur := FServerMethods.GetServerTime;
    AjouterLog('Heure serveur: ' + DateTimeToStr(HeureServeur));
  except
    on E: Exception do
      AjouterLog('Erreur: ' + E.Message);
  end;
end;

procedure TFormClient.AjouterLog(const Message: string);
begin
  MemoResultat.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
end;

end.
```

## Gestion des données avec DataSnap

### Transmettre des DataSets

DataSnap permet de transmettre facilement des ensembles de données :

**Côté Serveur :**

```pascal
type
  TServerMethods = class(TDSServerModule)
  private
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
  public
    function GetClients: TDataSet;
    function GetClientByID(ClientID: Integer): TDataSet;
    procedure UpdateClient(ClientData: TDataSet);
  end;

implementation

function TServerMethods.GetClients: TDataSet;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
  FDQuery1.Open;
  Result := FDQuery1;
end;

function TServerMethods.GetClientByID(ClientID: Integer): TDataSet;
begin
  FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
  FDQuery1.ParamByName('id').AsInteger := ClientID;
  FDQuery1.Open;
  Result := FDQuery1;
end;

procedure TServerMethods.UpdateClient(ClientData: TDataSet);
begin
  // Appliquer les modifications à la base de données
  // Logique de mise à jour ici
end;
```

**Côté Client :**

```pascal
procedure TFormClient.ChargerClients;
var
  DataSet: TDataSet;
begin
  try
    DataSet := FServerMethods.GetClients;

    ClientDataSet1.Data := TDataSetProvider.Create(nil).Data;
    // Afficher dans une grille
    DBGrid1.DataSource.DataSet := ClientDataSet1;

  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

### Utiliser des callbacks

Pour notifier les clients des changements :

**Serveur :**

```pascal
type
  IServerEvents = interface
    ['{GUID-HERE}']
    procedure OnDataChanged(const TableName: string);
  end;

  TServerMethods = class(TDSServerModule)
  private
    FCallbacks: TList<IServerEvents>;
  public
    procedure RegisterCallback(const Callback: IServerEvents);
    procedure UnregisterCallback(const Callback: IServerEvents);
    procedure NotifierChangements(const TableName: string);
  end;

procedure TServerMethods.NotifierChangements(const TableName: string);
var
  Callback: IServerEvents;
begin
  for Callback in FCallbacks do
  begin
    try
      Callback.OnDataChanged(TableName);
    except
      // Ignorer les callbacks défaillants
    end;
  end;
end;
```

## Sécurité dans l'architecture client-serveur

### Authentification

**Côté Serveur :**

```pascal
type
  TServerMethods = class(TDSServerModule)
  public
    function AuthenticateUser(const Username, Password: string): Boolean;
    function GetUserRole(const Username: string): string;
  end;

function TServerMethods.AuthenticateUser(const Username, Password: string): Boolean;
var
  Query: TFDQuery;
  PasswordHash: string;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT password_hash FROM users WHERE username = :username';
    Query.ParamByName('username').AsString := Username;
    Query.Open;

    if not Query.IsEmpty then
    begin
      PasswordHash := Query.FieldByName('password_hash').AsString;
      // Vérifier le hash du mot de passe
      Result := VerifierHash(Password, PasswordHash);
    end;

  finally
    Query.Free;
  end;
end;
```

**Côté Client :**

```pascal
procedure TFormClient.SeConnecter;
var
  Username, Password: string;
  Authentifie: Boolean;
begin
  Username := EditUsername.Text;
  Password := EditPassword.Text;

  try
    Authentifie := FServerMethods.AuthenticateUser(Username, Password);

    if Authentifie then
    begin
      ShowMessage('Connexion réussie');
      // Stocker le contexte utilisateur
      FCurrentUser := Username;
      FUserRole := FServerMethods.GetUserRole(Username);
    end
    else
      ShowMessage('Identifiants incorrects');

  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

### Autorisation

**Vérifier les permissions :**

```pascal
type
  TServerMethods = class(TDSServerModule)
  private
    function GetCurrentUser: string;
    function UserHasPermission(const Permission: string): Boolean;
  public
    function DeleteClient(ClientID: Integer): Boolean;
  end;

function TServerMethods.DeleteClient(ClientID: Integer): Boolean;
begin
  Result := False;

  // Vérifier les permissions
  if not UserHasPermission('DELETE_CLIENT') then
    raise Exception.Create('Permission refusée');

  // Exécuter la suppression
  FDQuery1.SQL.Text := 'DELETE FROM clients WHERE id = :id';
  FDQuery1.ParamByName('id').AsInteger := ClientID;
  FDQuery1.ExecSQL;

  Result := True;
end;
```

### Chiffrement des communications

**Utiliser SSL/TLS :**

```pascal
// Côté Serveur
procedure ConfigurerSSL;
begin
  DSHTTPService1.HTTPPort := 443;
  DSHTTPService1.CertFile := 'certificat.pem';
  DSHTTPService1.KeyFile := 'cle.pem';
  DSHTTPService1.RootCertFile := 'ca.pem';
end;

// Côté Client
SQLConnection1.Params.Values['HostName'] := 'https://serveur.com';
SQLConnection1.Params.Values['Port'] := '443';
SQLConnection1.Params.Values['UseSSL'] := 'True';
```

## Gestion des sessions et état

### Maintenir l'état de session

**Serveur avec gestion de session :**

```pascal
type
  TSessionInfo = class
    UserID: Integer;
    Username: string;
    LoginTime: TDateTime;
    LastActivity: TDateTime;
    SessionData: TDictionary<string, string>;
  end;

  TServerMethods = class(TDSServerModule)
  private
    class var FSessions: TObjectDictionary<string, TSessionInfo>;
    function GetSessionID: string;
    function GetSession: TSessionInfo;
  public
    function Login(const Username, Password: string): string; // Retourne SessionID
    procedure Logout;
    function GetSessionData(const Key: string): string;
    procedure SetSessionData(const Key, Value: string);
  end;

implementation

class constructor TServerMethods.Create;
begin
  FSessions := TObjectDictionary<string, TSessionInfo>.Create([doOwnsValues]);
end;

function TServerMethods.Login(const Username, Password: string): string;
var
  Session: TSessionInfo;
begin
  // Vérifier les identifiants
  if not AuthenticateUser(Username, Password) then
    raise Exception.Create('Identifiants invalides');

  // Créer une nouvelle session
  Result := TGUID.NewGuid.ToString;

  Session := TSessionInfo.Create;
  Session.Username := Username;
  Session.LoginTime := Now;
  Session.LastActivity := Now;
  Session.SessionData := TDictionary<string, string>.Create;

  FSessions.Add(Result, Session);
end;

function TServerMethods.GetSession: TSessionInfo;
var
  SessionID: string;
begin
  SessionID := GetSessionID;

  if not FSessions.TryGetValue(SessionID, Result) then
    raise Exception.Create('Session invalide ou expirée');

  Result.LastActivity := Now;
end;
```

**Client avec gestion de session :**

```pascal
type
  TFormClient = class(TForm)
  private
    FSessionID: string;
  end;

procedure TFormClient.Login;
begin
  try
    FSessionID := FServerMethods.Login(EditUsername.Text, EditPassword.Text);

    // Stocker le SessionID pour les appels suivants
    SQLConnection1.Params.Values['SessionID'] := FSessionID;

    ShowMessage('Connecté avec succès');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  end;
end;
```

## Performance et optimisation

### Mise en cache côté client

**Cache local des données fréquentes :**

```pascal
type
  TFormClient = class(TForm)
  private
    FCacheClients: TClientDataSet;
    FCacheLastUpdate: TDateTime;
    function GetClients(ForceRefresh: Boolean = False): TClientDataSet;
  end;

function TFormClient.GetClients(ForceRefresh: Boolean): TClientDataSet;
const
  CACHE_DURATION = 5 / (24 * 60); // 5 minutes
begin
  // Vérifier si le cache est valide
  if (not ForceRefresh) and
     (FCacheClients <> nil) and
     (Now - FCacheLastUpdate < CACHE_DURATION) then
  begin
    Result := FCacheClients;
    Exit;
  end;

  // Rafraîchir depuis le serveur
  FCacheClients := FServerMethods.GetClients;
  FCacheLastUpdate := Now;

  Result := FCacheClients;
end;
```

### Pagination des résultats

**Serveur :**

```pascal
function TServerMethods.GetClientsPaginated(Page, PageSize: Integer): TDataSet;
var
  Offset: Integer;
begin
  Offset := (Page - 1) * PageSize;

  FDQuery1.SQL.Text :=
    'SELECT * FROM clients ' +
    'ORDER BY nom ' +
    'LIMIT :pagesize OFFSET :offset';

  FDQuery1.ParamByName('pagesize').AsInteger := PageSize;
  FDQuery1.ParamByName('offset').AsInteger := Offset;
  FDQuery1.Open;

  Result := FDQuery1;
end;

function TServerMethods.GetTotalClients: Integer;
begin
  FDQuery1.SQL.Text := 'SELECT COUNT(*) as total FROM clients';
  FDQuery1.Open;
  Result := FDQuery1.FieldByName('total').AsInteger;
end;
```

**Client :**

```pascal
procedure TFormClient.ChargerPage(NumeroPage: Integer);
const
  PAGE_SIZE = 50;
var
  DataSet: TDataSet;
  Total, TotalPages: Integer;
begin
  DataSet := FServerMethods.GetClientsPaginated(NumeroPage, PAGE_SIZE);
  Total := FServerMethods.GetTotalClients;
  TotalPages := (Total + PAGE_SIZE - 1) div PAGE_SIZE;

  ClientDataSet1.Data := DataSet;
  LabelPage.Caption := Format('Page %d / %d', [NumeroPage, TotalPages]);
end;
```

### Compression des données

**Compresser avant transmission :**

```pascal
uses
  System.ZLib;

function CompresserDonnees(const Data: TBytes): TBytes;
var
  InputStream, OutputStream: TMemoryStream;
  Compressor: TZCompressionStream;
begin
  InputStream := TMemoryStream.Create;
  OutputStream := TMemoryStream.Create;
  try
    InputStream.Write(Data[0], Length(Data));
    InputStream.Position := 0;

    Compressor := TZCompressionStream.Create(OutputStream, zcDefault);
    try
      Compressor.CopyFrom(InputStream, InputStream.Size);
    finally
      Compressor.Free;
    end;

    SetLength(Result, OutputStream.Size);
    OutputStream.Position := 0;
    OutputStream.Read(Result[0], OutputStream.Size);

  finally
    InputStream.Free;
    OutputStream.Free;
  end;
end;
```

## Monitoring et logs

### Logger les activités serveur

```pascal
type
  TServerMethods = class(TDSServerModule)
  private
    procedure LogActivity(const Activity: string);
  end;

procedure TServerMethods.LogActivity(const Activity: string);
var
  LogFile: TextFile;
  LogFileName: string;
begin
  LogFileName := ExtractFilePath(ParamStr(0)) + 'server_log.txt';

  AssignFile(LogFile, LogFileName);
  try
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, Format('[%s] %s - %s',
      [DateTimeToStr(Now), GetCurrentUser, Activity]));

  finally
    CloseFile(LogFile);
  end;
end;
```

### Statistiques serveur

```pascal
type
  TServerStats = class
    TotalConnections: Integer;
    ActiveConnections: Integer;
    TotalRequests: Int64;
    AverageResponseTime: Double;
    ErrorCount: Integer;
  end;

  TServerMethods = class(TDSServerModule)
  private
    class var FStats: TServerStats;
  public
    class function GetStats: TServerStats;
  end;
```

## Bonnes pratiques

### 1. Séparer les responsabilités

```pascal
// ✅ Bon - Logique métier sur le serveur
function TServerMethods.CalculerPrixTotal(CommandeID: Integer): Currency;
begin
  // Calculs complexes côté serveur
  Result := CalculerSousTotal(CommandeID) +
            CalculerTaxes(CommandeID) +
            CalculerFraisLivraison(CommandeID);
end;

// ❌ Éviter - Logique métier côté client
// Le client ne devrait faire que l'affichage
```

### 2. Valider les données

```pascal
// Toujours valider côté serveur
function TServerMethods.CreateUser(const Username, Email: string): Integer;
begin
  // Validation serveur (obligatoire)
  if Username.IsEmpty or (Length(Username) < 3) then
    raise Exception.Create('Nom d''utilisateur invalide');

  if not IsValidEmail(Email) then
    raise Exception.Create('Email invalide');

  // Créer l'utilisateur
  Result := InsertUser(Username, Email);
end;
```

### 3. Gérer les transactions

```pascal
function TServerMethods.TransfererArgent(FromAccount, ToAccount: Integer;
  Montant: Currency): Boolean;
begin
  FDConnection1.StartTransaction;
  try
    // Débiter le compte source
    DebitAccount(FromAccount, Montant);

    // Créditer le compte destination
    CreditAccount(ToAccount, Montant);

    // Enregistrer la transaction
    LogTransaction(FromAccount, ToAccount, Montant);

    FDConnection1.Commit;
    Result := True;
  except
    FDConnection1.Rollback;
    raise;
  end;
end;
```

### 4. Limiter la taille des réponses

```pascal
// ✅ Bon - Retourner seulement ce qui est nécessaire
function TServerMethods.GetClientSummary(ClientID: Integer): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.AddPair('id', TJSONNumber.Create(ClientID));
  Result.AddPair('nom', GetClientName(ClientID));
  Result.AddPair('solde', TJSONNumber.Create(GetClientBalance(ClientID)));
end;

// ❌ Éviter - Tout retourner même si inutile
function GetClientComplete(ClientID: Integer): TDataSet; // Trop lourd
```

### 5. Gérer les timeouts

```pascal
// Client
SQLConnection1.Params.Values['ConnectionTimeout'] := '5000';
SQLConnection1.Params.Values['CommandTimeout'] := '30000';
```

### 6. Versionner l'API

```pascal
type
  TServerMethodsV1 = class(TDSServerModule)
  public
    function GetData: TDataSet; // Version 1
  end;

  TServerMethodsV2 = class(TDSServerModule)
  public
    function GetDataPaginated(Page: Integer): TDataSet; // Version 2
  end;
```

### 7. Documenter les méthodes

```pascal
/// <summary>
/// Récupère les informations d'un client
/// </summary>
/// <param name="ClientID">Identifiant unique du client</param>
/// <returns>Objet JSON contenant les données du client</returns>
/// <exception cref="Exception">Si le client n'existe pas</exception>
function TServerMethods.GetClientInfo(ClientID: Integer): TJSONObject;
```

## Résumé

### Points clés de l'architecture client-serveur

✅ **Modèles d'architecture :**
- **2-tiers** : Simple, client ↔ base de données
- **3-tiers** : Standard, présentation ↔ logique ↔ données
- **n-tiers** : Complexe, très modulaire

✅ **DataSnap avec Delphi :**
- Serveur avec `TDSServer`
- Client avec proxies générés
- Support TCP/IP et HTTP
- Transmission de datasets facilitée

✅ **Sécurité :**
- Authentification obligatoire
- Autorisation par rôles
- Chiffrement SSL/TLS
- Validation des données

✅ **Performance :**
- Mise en cache côté client
- Pagination des résultats
- Compression des données
- Monitoring et logs

✅ **Bonnes pratiques :**
- Séparer les responsabilités
- Valider côté serveur
- Gérer les transactions
- Versionner l'API
- Documenter le code

L'architecture client-serveur est fondamentale pour les applications professionnelles modernes, offrant centralisation, sécurité et évolutivité !

⏭️ [Applications distribuées](/10-communication-et-services-reseaux/06-applications-distribuees.md)
