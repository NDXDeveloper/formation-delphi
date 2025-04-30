# 23.4 Utilisation de WebBroker et DataSnap

## Introduction à WebBroker et DataSnap

Dans les sections précédentes, nous avons exploré les fondamentaux des applications web avec Delphi. Maintenant, nous allons plonger dans deux technologies importantes pour le développement d'applications web côté serveur : WebBroker et DataSnap. Ces outils puissants vous permettent de créer des services web robustes et des solutions complètes de partage de données.

## Qu'est-ce que WebBroker ?

WebBroker est l'une des plus anciennes technologies de Delphi pour le développement d'applications web. Malgré son âge, elle reste pertinente et puissante pour plusieurs raisons :

- Elle est légère et performante
- Elle offre un contrôle détaillé sur les requêtes HTTP
- Elle peut être déployée de plusieurs façons
- Elle est disponible dans toutes les éditions de Delphi

En termes simples, WebBroker est un framework qui vous permet de traiter les requêtes HTTP et de générer des réponses dynamiques. C'est la fondation sur laquelle d'autres technologies web de Delphi (comme DataSnap REST) sont construites.

## Qu'est-ce que DataSnap ?

DataSnap est une technologie plus récente qui se concentre sur la création de serveurs d'applications multi-niveaux. Elle permet de :

- Exposer des méthodes distantes qui peuvent être appelées par des clients
- Partager des ensembles de données (datasets) entre le client et le serveur
- Créer des services web, y compris des services REST
- Gérer les connexions clients, l'authentification et les sessions

Dans la version moderne de Delphi, DataSnap s'appuie sur WebBroker pour ses fonctionnalités web, ce qui explique pourquoi nous les abordons ensemble.

## Quand utiliser WebBroker ?

WebBroker est idéal pour :

- Créer des pages web dynamiques simples
- Développer des API web personnalisées et de bas niveau
- Générer des réponses HTTP spécifiques (JSON, XML, images, etc.)
- Des scénarios où vous avez besoin d'un contrôle total sur le traitement HTTP

## Quand utiliser DataSnap ?

DataSnap est préférable pour :

- Créer des applications multi-niveaux avec une logique métier partagée
- Exposer des ensembles de données à des clients distants
- Développer des services REST structurés
- Scénarios où l'authentification et la gestion des sessions sont importantes

## Création d'une application WebBroker simple

Commençons par créer une application WebBroker basique pour comprendre les concepts fondamentaux.

### Étape 1 : Créer un nouveau projet WebBroker

1. Ouvrez Delphi et sélectionnez **Fichier** > **Nouveau** > **Autres**
2. Naviguez vers **Delphi Projects** > **Web Server Application**
3. Cliquez sur **OK**
4. Dans la boîte de dialogue suivante, sélectionnez **WebBroker Application**
5. Pour le type de serveur, choisissez **Stand Alone Application** (le plus simple pour le développement)
6. Cliquez sur **OK**

Delphi va générer un projet avec deux fichiers principaux :
- `WebModuleUnit1.pas` : Le module web qui traitera les requêtes
- Le fichier de projet principal (`.dpr`)

### Étape 2 : Comprendre la structure du projet

Ouvrez `WebModuleUnit1.pas`. Vous verrez un module qui ressemble à ceci :

```delphi
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

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TWebModule1.WebModule1DefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Response.Content :=
    '<html>' +
    '<head><title>Web Server Application</title></head>' +
    '<body>Web Server Application</body>' +
    '</html>';
end;
```

Ce code définit un module web avec un gestionnaire d'action par défaut qui renvoie une page HTML simple.

### Étape 3 : Ajouter des actions et des gestionnaires

Les actions dans WebBroker correspondent à différentes URL ou chemins que votre application peut traiter. Ajoutons quelques actions :

1. Double-cliquez sur le fichier `WebModuleUnit1.dfm` pour ouvrir le concepteur visuel
2. Faites un clic droit sur le module et sélectionnez **Nouveau WebAction**
3. Dans l'Inspecteur d'objets, définissez les propriétés :
   - **Name** : `HelloAction`
   - **PathInfo** : `/hello`
   - **MethodType** : `mtGet` (pour les requêtes GET)
4. Répétez pour créer une autre action :
   - **Name** : `TimeAction`
   - **PathInfo** : `/time`
   - **MethodType** : `mtGet`

Maintenant, implémentons ces actions :

1. Double-cliquez sur `HelloAction` pour créer un gestionnaire d'événement
2. Ajoutez le code suivant :

```delphi
procedure TWebModule1.HelloActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Name: string;
begin
  // Vérifie si un paramètre 'name' existe dans la requête
  Name := Request.QueryFields.Values['name'];
  if Name = '' then
    Name := 'Monde';

  Response.ContentType := 'text/html';
  Response.Content :=
    '<html>' +
    '<head><title>Bonjour</title></head>' +
    '<body>' +
    '<h1>Bonjour, ' + Name + '!</h1>' +
    '<p>Bienvenue dans notre application WebBroker.</p>' +
    '</body>' +
    '</html>';
  Handled := True;
end;
```

3. Double-cliquez sur `TimeAction` et ajoutez :

```delphi
procedure TWebModule1.TimeActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content :=
    '<html>' +
    '<head><title>Heure actuelle</title></head>' +
    '<body>' +
    '<h1>Heure actuelle</h1>' +
    '<p>' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now) + '</p>' +
    '</body>' +
    '</html>';
  Handled := True;
end;
```

### Étape 4 : Compiler et exécuter l'application

1. Appuyez sur F9 pour compiler et exécuter
2. Une fenêtre d'application console apparaîtra, indiquant que le serveur est en cours d'exécution (généralement sur le port 8080)
3. Ouvrez votre navigateur et testez les URLs :
   - `http://localhost:8080/hello` - Affiche "Bonjour, Monde!"
   - `http://localhost:8080/hello?name=Alice` - Affiche "Bonjour, Alice!"
   - `http://localhost:8080/time` - Affiche l'heure actuelle

### Étape 5 : Créer un API JSON avec WebBroker

Modifions notre application pour retourner des données JSON, ce qui est courant dans les applications web modernes :

1. Ajoutez une nouvelle action :
   - **Name** : `UsersAction`
   - **PathInfo** : `/users`
   - **MethodType** : `mtGet`

2. Implémentez le gestionnaire :

```delphi
procedure TWebModule1.UsersActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  // Créer un tableau JSON
  JSONArray := TJSONArray.Create;
  try
    // Ajouter quelques utilisateurs
    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('id', TJSONNumber.Create(1));
    JSONObject.AddPair('name', 'Alice Dupont');
    JSONObject.AddPair('email', 'alice@exemple.fr');
    JSONArray.AddElement(JSONObject);

    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('id', TJSONNumber.Create(2));
    JSONObject.AddPair('name', 'Bob Martin');
    JSONObject.AddPair('email', 'bob@exemple.fr');
    JSONArray.AddElement(JSONObject);

    // Configurer la réponse
    Response.ContentType := 'application/json';
    Response.Content := JSONArray.ToString;
  finally
    JSONArray.Free; // Le Free va également libérer les objets enfants
  end;

  Handled := True;
end;
```

3. Exécutez l'application et visitez `http://localhost:8080/users` pour voir les données JSON

## Introduction à DataSnap avec WebBroker

Maintenant que nous comprenons les bases de WebBroker, voyons comment DataSnap s'intègre dans ce framework.

### Qu'est-ce qu'un serveur DataSnap ?

Un serveur DataSnap est une application qui expose des méthodes et des données à des clients distants. Il utilise WebBroker comme base pour son fonctionnement HTTP, mais ajoute plusieurs fonctionnalités :

- **Méthodes distantes** : Exposer des méthodes que les clients peuvent appeler
- **Partage de données** : Envoyer des ensembles de données aux clients
- **Sessions** : Suivre l'état des clients connectés
- **Authentification** : Sécuriser les accès au serveur
- **Callbacks** : Permettre au serveur d'appeler des méthodes sur le client

### Création d'un serveur DataSnap simple

Créons un serveur DataSnap qui expose quelques méthodes et données :

### Étape 1 : Créer un nouveau projet DataSnap

1. Sélectionnez **Fichier** > **Nouveau** > **Autres**
2. Naviguez vers **Delphi Projects** > **DataSnap Server**
3. Choisissez **DataSnap Server**
4. Sélectionnez **WebBroker Application**
5. Pour le type de serveur, choisissez **Stand Alone Application**
6. Laissez les options avancées par défaut et cliquez sur **Suivant** puis **Terminer**

Delphi va générer un projet avec plusieurs fichiers :
- `ServerMethodsUnit1.pas` : C'est ici que vous définirez vos méthodes serveur
- `WebModuleUnit1.pas` : Le module web qui intègre DataSnap à WebBroker
- `ServerContainerUnit1.pas` : Le conteneur qui configure le serveur DataSnap

### Étape 2 : Comprendre les méthodes du serveur

Ouvrez `ServerMethodsUnit1.pas`. Vous verrez une classe vide :

```delphi
type
  TServerMethods1 = class(TDSServerModule)
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;
```

Ajoutons quelques méthodes utiles :

```delphi
type
  TServerMethods1 = class(TDSServerModule)
  private
    { Déclarations privées }
  public
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function AddNumbers(A, B: Integer): Integer;
    function GetCurrentTime: TDateTime;
  end;

implementation

function TServerMethods1.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TServerMethods1.ReverseString(Value: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := Length(Value) downto 1 do
    Result := Result + Value[I];
end;

function TServerMethods1.AddNumbers(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TServerMethods1.GetCurrentTime: TDateTime;
begin
  Result := Now;
end;
```

Ces méthodes sont maintenant disponibles pour les clients distants.

### Étape 3 : Exposer un ensemble de données

Ajoutons une méthode qui renvoie un ensemble de données :

```delphi
private
  FEmployees: TFDMemTable;
  procedure CreateEmployeesTable;
public
  constructor Create(AOwner: TComponent); override;
  destructor Destroy; override;
  function GetEmployees: TDataSet;
```

Puis implémentons ces méthodes :

```delphi
constructor TServerMethods1.Create(AOwner: TComponent);
begin
  inherited;
  FEmployees := TFDMemTable.Create(Self);
  CreateEmployeesTable;
end;

destructor TServerMethods1.Destroy;
begin
  FEmployees.Free;
  inherited;
end;

procedure TServerMethods1.CreateEmployeesTable;
begin
  // Définir la structure
  FEmployees.FieldDefs.Clear;
  FEmployees.FieldDefs.Add('ID', ftInteger);
  FEmployees.FieldDefs.Add('Nom', ftString, 50);
  FEmployees.FieldDefs.Add('Prenom', ftString, 50);
  FEmployees.FieldDefs.Add('Email', ftString, 100);
  FEmployees.FieldDefs.Add('Salaire', ftCurrency);
  FEmployees.CreateDataSet;

  // Ajouter des données d'exemple
  FEmployees.AppendRecord([1, 'Dupont', 'Jean', 'jean.dupont@exemple.fr', 45000]);
  FEmployees.AppendRecord([2, 'Martin', 'Sophie', 'sophie.martin@exemple.fr', 52000]);
  FEmployees.AppendRecord([3, 'Dubois', 'Michel', 'michel.dubois@exemple.fr', 38000]);
  FEmployees.AppendRecord([4, 'Leroy', 'Emma', 'emma.leroy@exemple.fr', 61000]);
  FEmployees.AppendRecord([5, 'Petit', 'Thomas', 'thomas.petit@exemple.fr', 47500]);
end;

function TServerMethods1.GetEmployees: TDataSet;
begin
  Result := FEmployees;
end;
```

N'oubliez pas d'ajouter les unités nécessaires :

```delphi
uses
  System.SysUtils, System.Classes, System.Json,
  DataSnap.DSProviderDataModuleAdapter,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;
```

### Étape 4 : Tester le serveur DataSnap

1. Compilez et exécutez le projet
2. Ouvrez votre navigateur et accédez à :
   - `http://localhost:8080/datasnap/rest/TServerMethods1/EchoString/Bonjour`
   - `http://localhost:8080/datasnap/rest/TServerMethods1/ReverseString/Delphi`
   - `http://localhost:8080/datasnap/rest/TServerMethods1/AddNumbers/10/20`
   - `http://localhost:8080/datasnap/rest/TServerMethods1/GetCurrentTime`

Pour tester la méthode `GetEmployees`, vous aurez besoin d'un client DataSnap, car elle renvoie un ensemble de données qui n'est pas directement accessible via REST.

## Création d'un client DataSnap

Maintenant, créons un client simple pour notre serveur DataSnap.

### Étape 1 : Créer un nouveau projet VCL

1. Sélectionnez **Fichier** > **Nouveau** > **VCL Forms Application - Delphi**
2. Enregistrez le projet

### Étape 2 : Ajouter des composants clients DataSnap

1. Placez un `TSQLConnection` sur le formulaire et configurez-le :
   - **Driver** : `DataSnap`
   - **HostName** : `localhost`
   - **Port** : `8080`
   - **CommunicationProtocol** : `http`

2. Placez un `TDSProviderConnection` et configurez-le :
   - **SQLConnection** : `SQLConnection1`
   - **ServerClassName** : `TServerMethods1`

3. Ajoutez un `TClientDataSet` et configurez-le :
   - **ProviderName** : `DataSnapProvider`
   - **RemoteServer** : `DSProviderConnection1`

4. Ajoutez un `TDataSource` :
   - **DataSet** : `ClientDataSet1`

5. Placez un `TDBGrid` et définissez :
   - **DataSource** : `DataSource1`

6. Ajoutez quelques boutons sur le formulaire :
   - Un bouton "Connecter"
   - Un bouton "Charger Employés"
   - Un bouton "Écho"
   - Un bouton "Inverser"

### Étape 3 : Implémenter les gestionnaires d'événements

```delphi
procedure TForm1.btnConnecterClick(Sender: TObject);
begin
  try
    SQLConnection1.Connected := True;
    DSProviderConnection1.Connected := True;
    ShowMessage('Connexion réussie!');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion: ' + E.Message);
  end;
end;

procedure TForm1.btnChargerEmployesClick(Sender: TObject);
begin
  if not SQLConnection1.Connected then
  begin
    ShowMessage('Veuillez vous connecter d''abord.');
    Exit;
  end;

  ClientDataSet1.Close;
  ClientDataSet1.CommandText := 'GetEmployees';
  ClientDataSet1.Open;
end;

procedure TForm1.btnEchoClick(Sender: TObject);
var
  InputText: string;
  Result: OleVariant;
begin
  if not SQLConnection1.Connected then
  begin
    ShowMessage('Veuillez vous connecter d''abord.');
    Exit;
  end;

  InputText := InputBox('Écho', 'Entrez un texte:', '');
  if InputText <> '' then
  begin
    Result := SQLConnection1.AppServer.EchoString(InputText);
    ShowMessage('Résultat: ' + Result);
  end;
end;

procedure TForm1.btnInverserClick(Sender: TObject);
var
  InputText: string;
  Result: OleVariant;
begin
  if not SQLConnection1.Connected then
  begin
    ShowMessage('Veuillez vous connecter d''abord.');
    Exit;
  end;

  InputText := InputBox('Inverser', 'Entrez un texte:', '');
  if InputText <> '' then
  begin
    Result := SQLConnection1.AppServer.ReverseString(InputText);
    ShowMessage('Résultat inversé: ' + Result);
  end;
end;
```

### Étape 4 : Tester l'application client

1. Assurez-vous que votre serveur DataSnap est en cours d'exécution
2. Exécutez l'application client
3. Cliquez sur "Connecter"
4. Testez les différentes fonctionnalités

## Fonctionnalités avancées de DataSnap

### Authentification des clients

Pour sécuriser votre serveur DataSnap, vous pouvez ajouter une authentification :

1. Ouvrez `ServerContainerUnit1.pas`
2. Dans le gestionnaire d'événement `ServerContainer1Create`, ajoutez :

```delphi
DSServer1.AuthenticationManager := TDSAuthenticationManager.Create(DSServer1);
DSServer1.AuthenticationManager.OnUserAuthenticate := OnUserAuthenticate;
DSServer1.AuthenticationManager.OnUserAuthorize := OnUserAuthorize;
```

3. Implémentez ces gestionnaires :

```delphi
function TServerContainer1.OnUserAuthenticate(UserName, Password: string): Boolean;
begin
  // Exemple simple - dans un cas réel, vérifiez dans une base de données
  Result := (UserName = 'admin') and (Password = 'password123');
end;

function TServerContainer1.OnUserAuthorize(UserName: string;
  AuthorizeRoles: TStrings): Boolean;
begin
  // Exemple simple - dans un cas réel, vérifiez les rôles dans une base de données
  Result := True; // Autoriser tous les utilisateurs authentifiés
end;
```

### Filtres DataSnap

Les filtres vous permettent d'intercepter et de modifier les communications client-serveur. Par exemple, vous pourriez ajouter un filtre de compression :

```delphi
procedure TServerContainer1.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
var
  FilterName: string;
begin
  PersistentClass := TServerMethods1;

  // Ajouter un filtre de compression
  FilterName := DSServer1.FilterManager.RegisterFilter('Compression',
    TDSCompressionFilter, nil);
  DSServer1.FilterManager.SetFilter(FilterName, True);
end;
```

### Callbacks client

DataSnap permet au serveur d'appeler des méthodes sur le client, ce qui est utile pour les notifications en temps réel :

1. Côté client, créez une classe de callback :

```delphi
type
  TMyCallback = class(TDBXCallback)
  public
    function Execute(const Arg: TJSONValue): TJSONValue; override;
  end;
```

2. Implémentez la méthode d'exécution :

```delphi
function TMyCallback.Execute(const Arg: TJSONValue): TJSONValue;
begin
  // Traiter la notification
  TThread.Synchronize(nil, procedure
  begin
    ShowMessage('Notification du serveur: ' + Arg.ToString);
  end);

  Result := TJSONTrue.Create;
end;
```

3. Côté serveur, ajoutez une méthode pour enregistrer le callback :

```delphi
procedure TServerMethods1.RegisterCallback(CallbackId: Integer;
  Callback: TDBXCallback);
begin
  if ServerContainer.CallbackManager <> nil then
    ServerContainer.CallbackManager.RegisterCallback(CallbackId, Callback);
end;
```

4. Et une méthode pour notifier les clients :

```delphi
procedure TServerMethods1.NotifyClients(Message: string);
var
  Callbacks: TDBXCallbackList;
  I: Integer;
  JSONValue: TJSONValue;
begin
  Callbacks := ServerContainer.CallbackManager.LockCallbackList;
  try
    for I := 0 to Callbacks.Count - 1 do
    begin
      JSONValue := TJSONString.Create(Message);
      try
        Callbacks[I].Execute(JSONValue);
      finally
        JSONValue.Free;
      end;
    end;
  finally
    ServerContainer.CallbackManager.UnlockCallbackList;
  end;
end;
```

## Déploiement des applications WebBroker et DataSnap

Vous pouvez déployer vos applications WebBroker et DataSnap de plusieurs façons :

### Application autonome

L'option la plus simple est de déployer comme une application autonome :

1. Compilez votre application en mode Release
2. Copiez l'exécutable sur le serveur
3. Exécutez-le (manuellement ou comme un service Windows)

Pour configurer comme un service Windows, vous pouvez utiliser des outils comme NSSM (Non-Sucking Service Manager) ou modifier le code pour utiliser `TServiceApplication`.

### Module ISAPI pour IIS

Pour les déploiements sur IIS :

1. Dans l'assistant de création du projet, choisissez **ISAPI/NSAPI Dynamic Link Library**
2. Compilez votre application en mode Release
3. Copiez le fichier DLL dans le dossier approprié d'IIS
4. Configurez IIS pour gérer cette DLL

### Module Apache

Pour les déploiements sur Apache :

1. Dans l'assistant de création du projet, choisissez **Apache Shared Module**
2. Compilez votre application en mode Release
3. Copiez le fichier DLL dans le dossier modules d'Apache
4. Configurez Apache pour charger ce module

## Bonnes pratiques pour WebBroker et DataSnap

### Gestion des erreurs

Toujours implémenter une gestion d'erreurs robuste :

```delphi
try
  // Votre code
except
  on E: Exception do
  begin
    Response.StatusCode := 500;
    Response.Content := '{"error": "' + E.Message + '"}';
    Response.ContentType := 'application/json';
    Handled := True;
  end;
end;
```

### Sécurité

- Utilisez HTTPS pour les communications
- Implémentez une authentification appropriée
- Validez toutes les entrées utilisateur
- Utilisez des paramètres préparés pour les requêtes SQL

### Performance

- Mettez en cache les réponses lorsque c'est possible
- Utilisez des pools de connexions pour les bases de données
- Considérez utiliser des filtres de compression
- Limitez la taille des ensembles de données renvoyés

### Versionnement

Pour les API publiques, implémentez un versionnement :

```delphi
procedure TWebModule1.WebModule1WebActionItem1Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  Version: string;
begin
  Version := Request.QueryFields.Values['version'];

  if Version = '1' then
  begin
    // Logique pour la version 1
  end
  else if Version = '2' then
  begin
    // Logique pour la version 2
  end
  else
  begin
    // Version par défaut ou erreur
  end;
end;
```

## Exemple complet : Application de gestion de contacts

Pour illustrer l'utilisation de WebBroker et DataSnap ensemble, créons une application simple de gestion de contacts.

### Côté serveur (DataSnap/WebBroker)

```delphi
// Dans ServerMethodsUnit1.pas
type
  TContact = class
  private
    FId: Integer;
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
  published
    property Id: Integer read FId write FId;
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Email: string read FEmail write FEmail;
    property Telephone: string read FTelephone write FTelephone;
  end;

  TServerMethods1 = class(TDSServerModule)
  private
    FContacts: TFDMemTable;
    procedure InitializeContacts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetContacts: TDataSet;
    function GetContactById(Id: Integer): TContact;
    function AddContact(Contact: TContact): Integer;
    function UpdateContact(Contact: TContact): Boolean;
    function DeleteContact(Id: Integer): Boolean;
  end;
```

### Côté client (Application VCL)

```delphi
procedure TMainForm.FormCreate(Sender: TObject);
begin
  SQLConnection1.Connected := True;
  ClientDataSet1.CommandText := 'GetContacts';
  ClientDataSet1.Open;
end;

procedure TMainForm.btnAddClick(Sender: TObject);
var
  Contact: TContact;
  Result: OleVariant;
begin
  Contact := TContact.Create;
  try
    Contact.Nom := edtNom.Text;
    Contact.Prenom := edtPrenom.Text;
    Contact.Email := edtEmail.Text;
    Contact.Telephone := edtTelephone.Text;

    Result := SQLConnection1.AppServer.AddContact(Contact);
    ShowMessage('Contact ajouté avec ID: ' + IntToStr(Result));

    // Rafraîchir la liste
    ClientDataSet1.Refresh;
  finally
    Contact.Free;
  end;
end;
```

## Conclusion

Dans cette section, nous avons exploré WebBroker et DataSnap, deux technologies complémentaires pour le développement d'applications web avec Delphi. WebBroker offre un contrôle de bas niveau sur les requêtes HTTP, tandis que DataSnap ajoute des fonctionnalités avancées pour le partage de données et les applications multi-niveaux.

Ces technologies vous permettent de créer des solutions web puissantes tout en tirant parti de vos compétences Delphi existantes. Que vous développiez une API REST simple, une application web complète ou un système d'entreprise multi-niveaux, WebBroker et DataSnap offrent les outils nécessaires.

## Exercices pratiques

1. Créez une application WebBroker qui affiche la température actuelle (simulée) en formats JSON et HTML selon le paramètre de requête `format=json` ou `format=html`.

2. Développez un serveur DataSnap qui expose une méthode pour convertir des devises et un ensemble de données de taux de change.

3. Créez un client DataSnap pour votre serveur de l'exercice 2, avec une interface utilisateur permettant de convertir des montants entre différentes devises.

4. Améliorez le serveur de l'exercice 2 en ajoutant une authentification et des autorisations basées sur les rôles d'utilisateurs.

5. Étendez l'application de gestion de contacts avec une fonctionnalité de recherche côté serveur qui renvoie uniquement les contacts correspondant à un critère donné.

## Intégration de WebBroker avec les bases de données

Un cas d'utilisation courant pour WebBroker et DataSnap est l'accès aux bases de données. Voyons comment intégrer une base de données MySQL/MariaDB à notre application WebBroker.

### Connexion à MySQL/MariaDB

```delphi
procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  // Créer et configurer la connexion
  FDConnection1 := TFDConnection.Create(Self);
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Values['Server'] := 'localhost';
  FDConnection1.Params.Values['Database'] := 'mabasededonnees';
  FDConnection1.Params.Values['User_Name'] := 'utilisateur';
  FDConnection1.Params.Values['Password'] := 'motdepasse';
  FDConnection1.LoginPrompt := False;

  try
    FDConnection1.Connected := True;
  except
    on E: Exception do
      LogMessage('Erreur de connexion à la base de données: ' + E.Message);
  end;
end;
```

### Création d'un endpoint qui renvoie des données de la base de données

```delphi
procedure TWebModule1.ProduitActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  Query: TFDQuery;
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
begin
  if not FDConnection1.Connected then
  begin
    Response.StatusCode := 500;
    Response.Content := '{"error": "Base de données non disponible"}';
    Response.ContentType := 'application/json';
    Handled := True;
    Exit;
  end;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text := 'SELECT id, nom, description, prix FROM produits';

    // Filtrer par catégorie si spécifié
    if Request.QueryFields.Values['categorie'] <> '' then
    begin
      Query.SQL.Add('WHERE categorie = :categorie');
      Query.ParamByName('categorie').AsString := Request.QueryFields.Values['categorie'];
    end;

    Query.Open;

    JSONArray := TJSONArray.Create;
    while not Query.Eof do
    begin
      JSONObject := TJSONObject.Create;
      JSONObject.AddPair('id', TJSONNumber.Create(Query.FieldByName('id').AsInteger));
      JSONObject.AddPair('nom', Query.FieldByName('nom').AsString);
      JSONObject.AddPair('description', Query.FieldByName('description').AsString);
      JSONObject.AddPair('prix', TJSONNumber.Create(Query.FieldByName('prix').AsFloat));
      JSONArray.AddElement(JSONObject);

      Query.Next;
    end;

    Response.ContentType := 'application/json';
    Response.Content := JSONArray.ToString;
  finally
    Query.Free;
    JSONArray.Free; // N'est pas libéré si une exception se produit avant l'assignation à Response.Content
  end;

  Handled := True;
end;
```

## Création d'applications web hybrides

Vous pouvez également créer des applications hybrides qui combinent des pages web traditionnelles avec des services REST :

### Page HTML avec JavaScript qui appelle un service REST

```delphi
procedure TWebModule1.AccueilActionAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
begin
  Response.ContentType := 'text/html';
  Response.Content :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <title>Application WebBroker Hybride</title>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 20px; }' +
    '    .product { border: 1px solid #ddd; padding: 10px; margin: 10px 0; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <h1>Liste des Produits</h1>' +
    '  <div id="products"></div>' +
    '' +
    '  <script>' +
    '    fetch("/produits")' +
    '      .then(response => response.json())' +
    '      .then(data => {' +
    '        const container = document.getElementById("products");' +
    '        data.forEach(product => {' +
    '          const div = document.createElement("div");' +
    '          div.className = "product";' +
    '          div.innerHTML = `' +
    '            <h3>${product.nom}</h3>' +
    '            <p>${product.description}</p>' +
    '            <p><strong>Prix: ${product.prix} €</strong></p>' +
    '          `;' +
    '          container.appendChild(div);' +
    '        });' +
    '      })' +
    '      .catch(error => {' +
    '        console.error("Erreur:", error);' +
    '        document.getElementById("products").innerHTML = "Erreur lors du chargement des produits.";' +
    '      });' +
    '  </script>' +
    '</body>' +
    '</html>';

  Handled := True;
end;
```

## Utilisation avancée de DataSnap

### Mise en cache des données côté serveur

Pour améliorer les performances, vous pouvez mettre en cache les données fréquemment demandées :

```delphi
type
  TServerMethods1 = class(TDSServerModule)
  private
    FCache: TDictionary<string, TJSONValue>;
    FCacheLock: TCriticalSection;
    function GetFromCache(const Key: string): TJSONValue;
    procedure AddToCache(const Key: string; Value: TJSONValue; TimeoutMinutes: Integer = 5);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetProductsWithCache(Category: string): TJSONValue;
  end;

constructor TServerMethods1.Create(AOwner: TComponent);
begin
  inherited;
  FCache := TDictionary<string, TJSONValue>.Create;
  FCacheLock := TCriticalSection.Create;
end;

destructor TServerMethods1.Destroy;
begin
  FCacheLock.Free;

  // Libérer tous les objets JSON dans le cache
  for var Item in FCache do
    Item.Value.Free;

  FCache.Free;
  inherited;
end;

function TServerMethods1.GetFromCache(const Key: string): TJSONValue;
begin
  Result := nil;
  FCacheLock.Enter;
  try
    if FCache.ContainsKey(Key) then
      Result := FCache[Key].Clone as TJSONValue;
  finally
    FCacheLock.Leave;
  end;
end;

procedure TServerMethods1.AddToCache(const Key: string; Value: TJSONValue; TimeoutMinutes: Integer);
var
  ExistingValue: TJSONValue;
begin
  FCacheLock.Enter;
  try
    if FCache.TryGetValue(Key, ExistingValue) then
    begin
      ExistingValue.Free;
      FCache.Remove(Key);
    end;

    FCache.Add(Key, Value.Clone as TJSONValue);

    // Optionnellement, configurer un timer pour supprimer l'élément du cache après expiration
  finally
    FCacheLock.Leave;
  end;
end;

function TServerMethods1.GetProductsWithCache(Category: string): TJSONValue;
var
  CacheKey: string;
  Result: TJSONValue;
begin
  CacheKey := 'Products_' + Category;

  // Tenter de récupérer du cache
  Result := GetFromCache(CacheKey);
  if Result <> nil then
    Exit;

  // Si pas en cache, récupérer de la base de données
  // ... Code pour récupérer les produits ...

  // Ajouter au cache pour les requêtes futures
  AddToCache(CacheKey, Result, 10); // Cache pour 10 minutes
end;
```

### Gestion des sessions DataSnap

DataSnap permet de gérer les sessions utilisateur :

```delphi
procedure TServerMethods1.StartSession(UserName: string);
var
  SessionID: string;
begin
  // Générer un identifiant de session unique
  SessionID := CreateGUID;

  // Stocker dans le gestionnaire de sessions DataSnap
  if DSServer.LifeCycle <> nil then
  begin
    DSServer.LifeCycle.CreateSession(SessionID);
    DSServer.LifeCycle.SetSessionData(SessionID, 'UserName', UserName);
    // Ajouter d'autres données de session si nécessaire
  end;
end;

function TServerMethods1.GetCurrentUser(SessionID: string): string;
begin
  Result := '';
  if (DSServer.LifeCycle <> nil) and DSServer.LifeCycle.HasSession(SessionID) then
    Result := DSServer.LifeCycle.GetSessionData(SessionID, 'UserName');
end;
```

## Débogage des applications WebBroker et DataSnap

Le débogage des applications web peut être délicat. Voici quelques techniques utiles :

### Journalisation

Ajoutez une journalisation détaillée pour suivre les demandes et les réponses :

```delphi
procedure LogToFile(const Message: string);
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

    WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Message);
  finally
    CloseFile(LogFile);
  end;
end;

procedure TWebModule1.WebModule1BeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  LogToFile('Requête reçue: ' + Request.PathInfo + ' [' + Request.Method + ']');
  LogToFile('Paramètres: ' + Request.QueryFields.Text);
end;
```

### Utilisation des outils de développement du navigateur

Les outils de développement du navigateur (F12) sont essentiels pour déboguer le côté client :

1. Onglet "Réseau" pour voir les requêtes HTTP
2. Onglet "Console" pour les erreurs JavaScript
3. Onglet "Application" pour examiner les cookies et le stockage local

### Tester avec Postman ou des outils similaires

Pour tester les API REST, utilisez Postman ou des outils similaires qui vous permettent de :

1. Envoyer des requêtes avec différentes méthodes HTTP
2. Configurer des paramètres et des corps de requête
3. Visualiser les réponses formatées
4. Enregistrer les requêtes pour les tests futurs

## Meilleures pratiques pour les applications en production

### Sécurité

1. **HTTPS** : Configurez votre serveur web pour utiliser HTTPS avec un certificat valide
2. **Protection contre les injections** : Utilisez des paramètres préparés pour toutes les requêtes SQL
3. **Validation des entrées** : Validez toutes les entrées utilisateur côté serveur
4. **Protection CSRF** : Implémentez des jetons anti-CSRF pour les formulaires
5. **Authentification robuste** : Utilisez des méthodes d'authentification sécurisées comme JWT

### Performance

1. **Mise en cache** : Utilisez la mise en cache pour les données fréquemment accédées
2. **Compression** : Activez la compression HTTP (gzip) pour réduire la taille des réponses
3. **Pagination** : Limitez la quantité de données renvoyées pour les grandes collections
4. **Optimisation des requêtes SQL** : Assurez-vous que vos requêtes sont optimisées et indexées
5. **Réduisez le nombre de requêtes** : Combinez plusieurs opérations en une seule requête lorsque c'est possible

### Maintenance et surveillance

1. **Journalisation** : Implémentez une journalisation complète pour le débogage
2. **Surveillance** : Utilisez des outils pour surveiller les performances et la disponibilité
3. **Sauvegardes** : Sauvegardez régulièrement vos données et votre configuration
4. **Mises à jour** : Maintenez votre serveur et vos bibliothèques à jour

## Tendances et évolutions

### WebBroker et DataSnap dans Delphi moderne

Bien que WebBroker et DataSnap soient des technologies plus anciennes, elles continuent d'évoluer :

1. **Support REST amélioré** : Les versions récentes de Delphi ont considérablement amélioré le support REST
2. **Intégration avec les technologies web modernes** : Support pour JSON, JWT, etc.
3. **Performances améliorées** : Optimisations pour une meilleure scalabilité

### Alternatives et compléments

Si vous développez beaucoup d'applications web avec Delphi, vous pourriez également envisager :

1. **Horse** : Un framework web minimaliste et performant pour Delphi
2. **MARS-Curiosity** : Un framework REST inspiré de JAX-RS
3. **mORMot** : Une solution complète pour les services REST et SOA
4. **XData** : Une solution moderne pour créer des API REST
5. **TMS Sparkle** : Un framework HTTP pour créer des services web

## Conclusion

WebBroker et DataSnap sont des technologies puissantes qui permettent aux développeurs Delphi de créer des applications web et des services sans avoir à apprendre un tout nouveau langage ou framework. Que vous développiez une simple API REST, une application web complète ou un système d'entreprise multi-niveaux, ces outils offrent la familiarité et la productivité de Delphi associées à la puissance du développement web.

Dans cette section, nous avons exploré les fondamentaux de WebBroker et DataSnap, appris à créer des serveurs et des clients, et découvert des techniques avancées pour améliorer vos applications. Avec ces connaissances, vous êtes maintenant prêt à développer des solutions web puissantes avec Delphi.

## Ressources supplémentaires

- Documentation Embarcadero sur WebBroker : [Lien DocWiki](https://docwiki.embarcadero.com/RADStudio/en/Web_Broker)
- Documentation Embarcadero sur DataSnap : [Lien DocWiki](https://docwiki.embarcadero.com/RADStudio/en/DataSnap)
- Tutoriels vidéo Embarcadero : [Chaîne YouTube](https://www.youtube.com/user/EmbarcaderoTechNet)
- Blogs et forums communautaires :
  - [DelphiPraxis](https://en.delphipraxis.net/)
  - [DelphiFeeds](https://www.delphifeeds.com/)
  - [Stack Overflow - Tag Delphi](https://stackoverflow.com/questions/tagged/delphi)
- Livres recommandés :
  - "Delphi Cookbook" par Daniele Spinetti et Daniele Teti
  - "Delphi Event-Based and Asynchronous Programming" par Andrea Magni
