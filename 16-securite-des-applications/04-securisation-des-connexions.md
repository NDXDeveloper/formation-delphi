# 16. Sécurité des applications
## 16.4 Sécurisation des connexions

Dans ce chapitre, nous allons explorer comment sécuriser les différents types de connexions réseau dans vos applications Delphi. La sécurisation des communications est essentielle pour protéger les données sensibles lorsqu'elles transitent entre votre application et d'autres systèmes.

### Pourquoi sécuriser les connexions ?

Les données en transit sont particulièrement vulnérables car elles peuvent être interceptées par des attaquants utilisant diverses techniques comme :

- **L'écoute passive (sniffing)** : Capture du trafic réseau pour extraire des informations sensibles
- **L'attaque de l'homme du milieu (Man-in-the-Middle)** : Interception et potentielle modification des communications
- **L'usurpation d'identité (spoofing)** : Prétendre être un système ou un utilisateur légitime

La sécurisation des connexions permet de :
- Garantir la **confidentialité** des données (personne ne peut les lire)
- Assurer l'**intégrité** des données (personne ne peut les modifier)
- Vérifier l'**authenticité** des parties communicantes (vous parlez à la bonne personne/système)

### Utilisation de HTTPS pour les connexions Web

Le protocole HTTPS (HTTP Secure) est la méthode standard pour sécuriser les communications Web. Il utilise TLS (Transport Layer Security) pour chiffrer les données.

#### Configuration d'une requête REST sécurisée

Voici comment configurer une requête REST pour utiliser HTTPS dans Delphi :

```pas
procedure MakeSecureRESTRequest;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configurer le client REST pour utiliser HTTPS
    RESTClient.BaseURL := 'https://api.exemple.com'; // Notez le "https://"
    RESTClient.Accept := 'application/json';

    // Spécifier la version TLS (TLS 1.2 ou supérieur est recommandé)
    RESTClient.SecureProtocols := [TLSv1_2, TLSv1_3];

    // Activer la vérification des certificats
    RESTClient.ValidateServerCertificate := True;

    // Configurer la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Resource := 'users';

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
      ShowMessage('Requête réussie : ' + RESTResponse.Content)
    else
      ShowMessage('Erreur: ' + IntToStr(RESTResponse.StatusCode) + ' - ' +
                  RESTResponse.StatusText);

  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

> ⚠️ **Important** : Utilisez toujours `https://` au lieu de `http://` pour vos API et services Web contenant des données sensibles.

#### Vérification des certificats SSL/TLS

Par défaut, Delphi vérifie les certificats SSL/TLS. Cependant, il est parfois nécessaire de personnaliser ce comportement :

```pas
procedure ConfigureSSLHandling;
var
  RESTClient: TRESTClient;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  RESTClient := TRESTClient.Create(nil);

  // Configuration SSL avancée en utilisant Indy
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configurer les options SSL
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2; // Utiliser TLS 1.2
    SSLIOHandler.SSLOptions.Mode := sslmClient;

    // Vérification des certificats
    SSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer]; // Vérifier le certificat
    SSLIOHandler.SSLOptions.VerifyDepth := 9; // Profondeur max de la chaîne de certification

    // Vous pouvez également spécifier un magasin de certificats personnalisé
    // SSLIOHandler.SSLOptions.RootCertFile := 'path/to/ca-certificates.pem';

    // Assigner le gestionnaire SSL au client REST
    RESTClient.HTTPOptions.ProtocolVersion := pv1_1;
    // Attacher l'IOHandler au client REST (nécessite un peu plus de code)

    // Reste de la configuration...

  finally
    // Ne pas libérer SSLIOHandler tant que le RESTClient l'utilise
    // La libération de mémoire dépend de votre logique d'application
  end;
end;
```

> 💡 **Note** : Pour des configurations SSL avancées, vous pourriez avoir besoin d'utiliser directement les composants Indy comme `TIdHTTP` et `TIdSSLIOHandlerSocketOpenSSL`.

#### Gestion des certificats auto-signés

Dans certains environnements de développement ou intranets, vous pourriez rencontrer des certificats auto-signés. Bien que ce ne soit pas recommandé pour la production, voici comment les gérer :

```pas
procedure ConfigureSelfSignedCertificate;
var
  RESTClient: TRESTClient;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  RESTClient := TRESTClient.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configuration pour accepter les certificats auto-signés
    SSLIOHandler.OnVerifyPeer := HandleVerifyPeer;

    // Reste de la configuration...

  finally
    // Gestion de la libération mémoire...
  end;
end;

// Fonction de rappel pour la vérification des certificats
function HandleVerifyPeer(Certificate: TIdX509; AOk: Boolean;
  ADepth, AError: Integer): Boolean;
begin
  // En développement uniquement : accepter tous les certificats
  Result := True;

  // En production, vous devriez implémenter une logique plus stricte
  // Par exemple, vérifier l'empreinte du certificat auto-signé connu
  // if (Certificate.Fingerprint = 'known_fingerprint') then
  //   Result := True;
end;
```

> ⚠️ **Attention** : L'acceptation de tous les certificats est dangereuse et ne devrait jamais être utilisée en production. Utilisez cette approche uniquement en développement.

### Authentification sécurisée pour les API REST

#### Authentification par jeton (Token)

L'authentification par jeton est largement utilisée pour les API RESTful. Voici comment l'implémenter :

```pas
type
  TAuthenticationService = class
  private
    FToken: string;
    FTokenExpiry: TDateTime;
    FBaseURL: string;
  public
    constructor Create(const BaseURL: string);

    function Authenticate(const Username, Password: string): Boolean;
    function MakeAuthenticatedRequest(const Resource: string; Method: TRESTRequestMethod): string;
    function IsTokenValid: Boolean;
  end;

constructor TAuthenticationService.Create(const BaseURL: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FToken := '';
  FTokenExpiry := 0;
end;

function TAuthenticationService.Authenticate(const Username, Password: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
begin
  Result := False;
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration pour l'authentification
    RESTClient.BaseURL := FBaseURL;
    RESTClient.Accept := 'application/json';

    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := 'auth/login';

    // Ajouter les informations d'authentification
    RESTRequest.AddBody('{"username":"' + Username +
                         '","password":"' + Password + '"}',
                         TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      // Extraire le jeton et sa date d'expiration
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        if JSONValue <> nil then
        begin
          FToken := JSONValue.GetValue<string>('token');

          // Calculer l'expiration (exemple : convertir les secondes en TDateTime)
          var ExpiresIn := JSONValue.GetValue<Integer>('expires_in');
          FTokenExpiry := Now + (ExpiresIn / 86400); // Convertir secondes en jours

          Result := FToken <> '';
        end;
      finally
        JSONValue.Free;
      end;
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TAuthenticationService.IsTokenValid: Boolean;
begin
  // Vérifier si le jeton existe et n'est pas expiré
  Result := (FToken <> '') and (Now < FTokenExpiry);
end;

function TAuthenticationService.MakeAuthenticatedRequest(
  const Resource: string; Method: TRESTRequestMethod): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := '';

  // Vérifier si nous avons un jeton valide
  if not IsTokenValid then
    raise Exception.Create('Non authentifié ou jeton expiré');

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration pour la requête authentifiée
    RESTClient.BaseURL := FBaseURL;
    RESTClient.Accept := 'application/json';

    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := Method;
    RESTRequest.Resource := Resource;

    // Ajouter le jeton d'authentification (généralement dans l'en-tête Authorization)
    RESTRequest.Params.AddHeader('Authorization', 'Bearer ' + FToken);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
      Result := RESTResponse.Content
    else if RESTResponse.StatusCode = 401 then
      raise Exception.Create('Authentification invalide. Veuillez vous reconnecter.')
    else
      raise Exception.Create('Erreur: ' + IntToStr(RESTResponse.StatusCode) +
                             ' - ' + RESTResponse.StatusText);
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

#### Utilisation du service d'authentification

```pas
procedure TMainForm.ButtonLoginClick(Sender: TObject);
begin
  // Créer le service d'authentification
  if FAuthService = nil then
    FAuthService := TAuthenticationService.Create('https://api.exemple.com');

  // Tenter l'authentification
  if FAuthService.Authenticate(EditUsername.Text, EditPassword.Text) then
  begin
    ShowMessage('Authentification réussie !');
    UpdateUIForAuthenticatedUser;
  end
  else
    ShowMessage('Authentification échouée. Vérifiez vos identifiants.');
end;

procedure TMainForm.ButtonGetDataClick(Sender: TObject);
var
  Response: string;
begin
  try
    // Faire une requête authentifiée
    Response := FAuthService.MakeAuthenticatedRequest('users/profile', rmGET);

    // Afficher les données
    MemoResult.Text := Response;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);

      // Si le jeton est expiré, rediriger vers la connexion
      if not FAuthService.IsTokenValid then
        UpdateUIForUnauthenticatedUser;
    end;
  end;
end;
```

### Sécurisation des connexions à la base de données

#### Configuration SSL pour MySQL/MariaDB

Pour sécuriser vos connexions à MySQL ou MariaDB avec SSL :

```pas
procedure ConfigureMySQLSSLConnection;
var
  Connection: TFDConnection;
begin
  Connection := TFDConnection.Create(nil);
  try
    Connection.Params.Clear;
    Connection.Params.Add('DriverID=MySQL');
    Connection.Params.Add('Server=serveur.exemple.com');
    Connection.Params.Add('Port=3306');
    Connection.Params.Add('Database=ma_base');
    Connection.Params.Add('User_Name=utilisateur');
    Connection.Params.Add('Password=mot_de_passe');

    // Activer SSL
    Connection.Params.Add('UseSSL=True');

    // Chemin vers les certificats SSL (si nécessaire)
    Connection.Params.Add('SSLCa=chemin/vers/ca-cert.pem');
    Connection.Params.Add('SSLCert=chemin/vers/client-cert.pem');
    Connection.Params.Add('SSLKey=chemin/vers/client-key.pem');

    // Vérifier le certificat du serveur
    Connection.Params.Add('SSLVerify=True');

    // Tester la connexion
    Connection.Connected := True;
    ShowMessage('Connexion MySQL sécurisée établie !');
  finally
    Connection.Free;
  end;
end;
```

#### Pour d'autres bases de données

Les paramètres exacts peuvent varier selon la base de données :

- **PostgreSQL** : Utilisez `SSLMode` avec des valeurs comme 'require', 'verify-ca' ou 'verify-full'
- **Microsoft SQL Server** : Utilisez `Encrypt=Yes` et `TrustServerCertificate=No`
- **SQLite** : SQLite n'est pas un système client-serveur et ne nécessite généralement pas de SSL, mais vous pouvez chiffrer la base de données elle-même

### Sécurisation des communications par socket

Pour les applications qui utilisent des communications socket directes, vous pouvez utiliser les sockets SSL/TLS d'Indy :

```pas
procedure ConfigureSecureSocketClient;
var
  TCPClient: TIdTCPClient;
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  TCPClient := TIdTCPClient.Create(nil);
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  try
    // Configurer le gestionnaire SSL
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.Mode := sslmClient;
    SSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer];
    SSLIOHandler.SSLOptions.VerifyDepth := 9;

    // Configurer le client TCP
    TCPClient.IOHandler := SSLIOHandler;
    TCPClient.Host := 'serveur.exemple.com';
    TCPClient.Port := 8080;

    // Se connecter au serveur
    TCPClient.Connect;

    // Envoyer des données
    TCPClient.IOHandler.WriteLn('Hello, secure world!');

    // Recevoir des données
    var Response := TCPClient.IOHandler.ReadLn;
    ShowMessage('Réponse du serveur : ' + Response);

    // Déconnecter
    TCPClient.Disconnect;
  finally
    // Libérer les ressources (dans cet ordre)
    TCPClient.Free; // Ceci libère aussi le IOHandler
    // Ne pas libérer SSLIOHandler ici, il est déjà libéré par TCPClient
  end;
end;
```

### Création d'un serveur sécurisé

Si votre application agit comme un serveur, vous pouvez également configurer un serveur TCP sécurisé :

```pas
// Nécessite Delphi 11 ou supérieur pour certaines fonctionnalités
procedure ConfigureSecureSocketServer;
var
  TCPServer: TIdTCPServer;
  SSLIOHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  TCPServer := TIdTCPServer.Create(nil);
  SSLIOHandler := TIdServerIOHandlerSSLOpenSSL.Create(nil);

  try
    // Configurer le gestionnaire SSL
    SSLIOHandler.SSLOptions.CertFile := 'chemin/vers/certificat.pem';
    SSLIOHandler.SSLOptions.KeyFile := 'chemin/vers/cle_privee.pem';
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.Mode := sslmServer;

    // Configurer le serveur TCP
    TCPServer.IOHandler := SSLIOHandler;
    TCPServer.DefaultPort := 8080;

    // Définir la méthode de gestion des connexions
    TCPServer.OnExecute := HandleClientConnection;

    // Démarrer le serveur
    TCPServer.Active := True;

    ShowMessage('Serveur sécurisé démarré sur le port 8080');

    // À ce stade, le serveur attend les connexions dans un thread séparé
    // Vous devriez attendre que l'utilisateur décide d'arrêter le serveur
    // Par exemple: Application.MessageLoop ou une boucle similaire

  finally
    // Libérer les ressources quand l'application se termine
    TCPServer.Active := False;
    TCPServer.Free;
  end;
end;

procedure HandleClientConnection(AContext: TIdContext);
var
  Line: string;
begin
  // Lire les données envoyées par le client
  Line := AContext.Connection.IOHandler.ReadLn;

  // Traiter les données (exemple simple)
  Line := 'Vous avez envoyé : ' + Line;

  // Répondre au client
  AContext.Connection.IOHandler.WriteLn(Line);
end;
```

### Sécurisation des WebSockets

Les WebSockets sont souvent utilisés pour les communications en temps réel. Voici comment les sécuriser en utilisant la librairie Sgclib ou une librairie similaire :

```pas
procedure ConfigureSecureWebSocket;
var
  WebSocketClient: TsgcWebSocketClient;
begin
  WebSocketClient := TsgcWebSocketClient.Create(nil);
  try
    // Configurer le client WebSocket pour utiliser WSS (WebSocket Secure)
    WebSocketClient.URL := 'wss://echo.websocket.org'; // Notez le 'wss://' au lieu de 'ws://'
    WebSocketClient.TLS := True;

    // Événements pour gérer les messages et connexions
    WebSocketClient.OnMessage := HandleWebSocketMessage;
    WebSocketClient.OnConnect := HandleWebSocketConnect;
    WebSocketClient.OnDisconnect := HandleWebSocketDisconnect;

    // TLS options
    WebSocketClient.Options.TLS.Version := tls1_2;
    WebSocketClient.Options.TLS.VerifyCertificate := True;

    // Se connecter au serveur
    WebSocketClient.Active := True;

    // Attendre que la connexion soit établie
    if not WebSocketClient.Connected then
      // Gérer l'erreur de connexion
    else
    begin
      // Envoyer un message
      WebSocketClient.WriteData('Hello, secure WebSocket!');

      // Le traitement des réponses se fait via l'événement OnMessage
      // ...

      // Déconnecter quand c'est terminé
      WebSocketClient.Active := False;
    end;
  finally
    WebSocketClient.Free;
  end;
end;

procedure HandleWebSocketMessage(Connection: TsgcWSConnection; const Text: string);
begin
  ShowMessage('Message reçu : ' + Text);
end;

procedure HandleWebSocketConnect(Connection: TsgcWSConnection);
begin
  ShowMessage('Connecté au serveur WebSocket');
end;

procedure HandleWebSocketDisconnect(Connection: TsgcWSConnection);
begin
  ShowMessage('Déconnecté du serveur WebSocket');
end;
```

> 💡 **Note** : Pour utiliser des WebSockets sécurisés, vous devrez peut-être installer une bibliothèque tierce comme sgcWebSockets via GetIt ou un autre gestionnaire de paquets.

### Certificats et autorités de certification (CA)

#### Comprendre les certificats SSL/TLS

Un certificat numérique contient plusieurs informations importantes :
- L'identité du titulaire (nom de domaine, organisation, etc.)
- La clé publique du titulaire
- La période de validité
- L'identité de l'émetteur (Autorité de Certification)
- Une signature numérique de l'émetteur

#### Génération d'un certificat auto-signé pour les tests

Bien que non recommandés pour la production, les certificats auto-signés sont utiles pour le développement :

```pas
procedure GenerateSelfSignedCertificate;
var
  CertGenerator: TIdX509Generator;
  Certificate: TIdX509;
  PrivateKey: TIdRSAKeyPrivate;
begin
  CertGenerator := TIdX509Generator.Create;
  try
    // Configurer les informations du certificat
    CertGenerator.State := 'Votre État';
    CertGenerator.Country := 'FR';
    CertGenerator.Organization := 'Votre Organisation';
    CertGenerator.OrganizationalUnit := 'Votre Département';
    CertGenerator.CommonName := 'localhost'; // Ou le nom de domaine de votre serveur
    CertGenerator.EmailAddress := 'email@exemple.com';

    // Générer le certificat (valide 365 jours)
    Certificate := CertGenerator.GenerateCertificate(365);
    PrivateKey := CertGenerator.GeneratedKey;

    // Sauvegarder le certificat et la clé privée
    Certificate.SaveToFile('certificat.pem');
    PrivateKey.SaveToFile('cle_privee.pem');

    ShowMessage('Certificat et clé privée générés avec succès.');
  finally
    CertGenerator.Free;
  end;
end;
```

### Exemple complet : Application de messagerie sécurisée

Voici un exemple qui combine plusieurs concepts de sécurité dans une application de messagerie sécurisée :

```pas
unit SecureChatForm;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, IdTCPClient, IdSSLOpenSSL, IdGlobal,
  System.JSON, System.NetEncoding, ModernEncryption;

type
  TFormSecureChat = class(TForm)
    PanelTop: TPanel;
    EditServer: TEdit;
    EditPort: TEdit;
    EditUsername: TEdit;
    ButtonConnect: TButton;
    MemoMessages: TMemo;
    PanelBottom: TPanel;
    EditMessage: TEdit;
    ButtonSend: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
  private
    FTCPClient: TIdTCPClient;
    FSSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FConnected: Boolean;
    FUsername: string;

    procedure HandleIncomingMessages;
    function EncryptMessage(const Text: string): string;
    function DecryptMessage(const EncryptedText: string): string;
  end;

var
  FormSecureChat: TFormSecureChat;

implementation

{$R *.dfm}

procedure TFormSecureChat.FormCreate(Sender: TObject);
begin
  FTCPClient := TIdTCPClient.Create(nil);
  FSSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);

  FConnected := False;

  EditServer.Text := 'chat.exemple.com';
  EditPort.Text := '8443';
  EditUsername.Text := 'Utilisateur' + IntToStr(Random(1000));
end;

procedure TFormSecureChat.FormDestroy(Sender: TObject);
begin
  if FConnected then
  begin
    FTCPClient.Disconnect;
    FConnected := False;
  end;

  FTCPClient.Free;
  // FSSLIOHandler est libéré par le client TCP
end;

procedure TFormSecureChat.ButtonConnectClick(Sender: TObject);
begin
  if FConnected then
  begin
    // Déconnecter
    FTCPClient.Disconnect;
    FConnected := False;
    ButtonConnect.Caption := 'Connecter';
    EditServer.Enabled := True;
    EditPort.Enabled := True;
    EditUsername.Enabled := True;
    ButtonSend.Enabled := False;

    MemoMessages.Lines.Add('*** Déconnecté du serveur ***');
  end
  else
  begin
    // Configurer le gestionnaire SSL
    FSSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    FSSLIOHandler.SSLOptions.Mode := sslmClient;
    FSSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer];
    FSSLIOHandler.SSLOptions.VerifyDepth := 9;

    // Configurer le client TCP
    FTCPClient.IOHandler := FSSLIOHandler;
    FTCPClient.Host := EditServer.Text;
    FTCPClient.Port := StrToIntDef(EditPort.Text, 8443);

    // Se connecter au serveur
    try
      FTCPClient.Connect;
      FConnected := True;

      // Envoyer les informations d'authentification
      FUsername := EditUsername.Text;

      // Préparer le message d'authentification en JSON
      var AuthMessage := TJSONObject.Create;
      try
        AuthMessage.AddPair('type', 'auth');
        AuthMessage.AddPair('username', FUsername);

        // Envoyer le message d'authentification
        FTCPClient.IOHandler.WriteLn(AuthMessage.ToString);
      finally
        AuthMessage.Free;
      end;

      // Mettre à jour l'interface
      ButtonConnect.Caption := 'Déconnecter';
      EditServer.Enabled := False;
      EditPort.Enabled := False;
      EditUsername.Enabled := False;
      ButtonSend.Enabled := True;

      MemoMessages.Lines.Add('*** Connecté au serveur en tant que ' + FUsername + ' ***');

      // Démarrer un thread pour recevoir les messages
      TThread.CreateAnonymousThread(HandleIncomingMessages).Start;
    except
      on E: Exception do
      begin
        ShowMessage('Erreur de connexion: ' + E.Message);
        FConnected := False;
      end;
    end;
  end;
end;

procedure TFormSecureChat.HandleIncomingMessages;
var
  Line: string;
  JSONValue: TJSONValue;
  MessageType, Username, Content: string;
begin
  while FConnected do
  begin
    try
      // Lire un message du serveur
      Line := FTCPClient.IOHandler.ReadLn;

      // Parser le JSON
      JSONValue := TJSONObject.ParseJSONValue(Line);
      try
        if JSONValue <> nil then
        begin
          // Extraire les informations du message
          MessageType := JSONValue.GetValue<string>('type');

          if MessageType = 'message' then
          begin
            Username := JSONValue.GetValue<string>('username');
            Content := JSONValue.GetValue<string>('content');

            // Déchiffrer le message si nécessaire
            if JSONValue.GetValue<Boolean>('encrypted') then
              Content := DecryptMessage(Content);

            // Afficher le message
            TThread.Synchronize(nil, procedure
            begin
              MemoMessages.Lines.Add(Username + ': ' + Content);
            end);
          end
          else if MessageType = 'system' then
          begin
            Content := JSONValue.GetValue<string>('content');

            // Afficher le message système
            TThread.Synchronize(nil, procedure
            begin
              MemoMessages.Lines.Add('*** ' + Content + ' ***');
            end);
          end;
        end;
      finally
        JSONValue.Free;
      end;
    except
      on E: Exception do
      begin
        if FConnected then
        begin
          // Afficher l'erreur seulement si nous sommes toujours censés être connectés
          TThread.Synchronize(nil, procedure
          begin
            MemoMessages.Lines.Add('*** Erreur: ' + E.Message + ' ***');
          end);

          // Si l'erreur est due à une déconnexion, on réinitialise l'état
          if (E is EIdConnClosedGracefully) or
             (E is EIdSocketError) then
          begin
            FConnected := False;

            TThread.Synchronize(nil, procedure
            begin
              ButtonConnect.Caption := 'Connecter';
              EditServer.Enabled := True;
              EditPort.Enabled := True;
              EditUsername.Enabled := True;
              ButtonSend.Enabled := False;

              MemoMessages.Lines.Add('*** Déconnecté du serveur ***');
            end);

            Break; // Sortir de la boucle
          end;
        end
        else
          Break; // Sortir de la boucle si nous sommes déconnectés
      end;
    end;
  end;
end;

procedure TFormSecureChat.ButtonSendClick(Sender: TObject);
var
  Message: string;
  JSONMessage: TJSONObject;
  EncryptedContent: string;
begin
  Message := EditMessage.Text;

  if (Message = '') or not FConnected then
    Exit;

  // Préparer le message en JSON
  JSONMessage := TJSONObject.Create;
  try
    JSONMessage.AddPair('type', 'message');
    JSONMessage.AddPair('username', FUsername);

    // Chiffrer le contenu du message
    EncryptedContent := EncryptMessage(Message);
    JSONMessage.AddPair('content', EncryptedContent);
    JSONMessage.AddPair('encrypted', TJSONBool.Create(True));

    // Envoyer le message au serveur
    FTCPClient.IOHandler.WriteLn(JSONMessage.ToString);

    // Afficher notre propre message dans la zone de messages
    MemoMessages.Lines.Add('Moi: ' + Message);

    // Effacer le champ de saisie
    EditMessage.Text := '';
    EditMessage.SetFocus;
  finally
    JSONMessage.Free;
  end;
end;

function TFormSecureChat.EncryptMessage(const Text: string): string;
begin
  // Utiliser l'algorithme AES pour chiffrer le message
  // Dans une application réelle, vous utiliseriez un chiffrement
  // de bout en bout avec des clés partagées entre les utilisateurs
  Result := TModernCrypto.EncryptString(Text, 'cleSecreteDuChat');

  // Encoder en Base64 pour s'assurer que le JSON reste valide
  Result := TNetEncoding.Base64.Encode(Result);
end;

function TFormSecureChat.DecryptMessage(const EncryptedText: string): string;
begin
  try
    // Décoder de Base64
    var DecodedText := TNetEncoding.Base64.Decode(EncryptedText);

    // Déchiffrer avec AES
    Result := TModernCrypto.DecryptString(DecodedText, 'cleSecreteDuChat');
  except
    on E: Exception do
    begin
      Result := '[Message chiffré non déchiffrable]';
    end;
  end;
end;
```

### Protection contre les attaques courantes

#### Protection contre l'attaque de l'homme du milieu (MITM)

L'attaque de l'homme du milieu survient lorsqu'un attaquant intercepte les communications entre deux parties, potentiellement en modifiant les messages transmis. Voici comment s'en protéger :

1. **Toujours utiliser TLS/SSL** : Assurez-vous que toutes les communications sensibles utilisent HTTPS ou des sockets sécurisés.

2. **Vérifier les certificats** : Validez toujours les certificats des serveurs auxquels vous vous connectez.

3. **Certificate Pinning** : Associez explicitement un certificat spécifique à un serveur pour empêcher la substitution par un certificat frauduleux :

```pas
procedure ConfigureCertificatePinning;
var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  ExpectedFingerprint: string;
begin
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Définir l'empreinte attendue du certificat du serveur
    ExpectedFingerprint := 'AA:BB:CC:DD:EE:FF:00:11:22:33:44:55:66:77:88:99:AA:BB:CC:DD';

    // Configurer un gestionnaire de vérification personnalisé
    SSLIOHandler.OnVerifyPeer := procedure(Certificate: TIdX509; AOk: Boolean;
                                         ADepth, AError: Integer; var AResult: Boolean)
    begin
      // Vérifier si l'empreinte correspond à celle attendue
      AResult := SameText(Certificate.Fingerprint, ExpectedFingerprint);

      if not AResult then
        LogSecurityWarning('Empreinte de certificat non valide : ' + Certificate.Fingerprint);
    end;

    // Reste de la configuration...
  finally
    // Gestion de la libération mémoire...
  end;
end;
```

#### Protection contre les attaques par injection

Les attaques par injection peuvent survenir lorsque des données non filtrées sont utilisées dans des requêtes SQL ou des constructions similaires :

1. **Requêtes préparées** : Utilisez toujours des requêtes paramétrées pour les requêtes SQL.

2. **Validation des entrées** : Validez toutes les entrées utilisateur avant de les utiliser.

```pas
procedure SecureRESTRequest;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  SafeParameter: string;
begin
  // Valider et nettoyer le paramètre d'entrée
  SafeParameter := ValidateAndSanitizeInput(EditUserInput.Text);

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration pour la requête REST sécurisée
    RESTClient.BaseURL := 'https://api.exemple.com';

    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;

    // Utiliser des paramètres pour éviter les injections
    RESTRequest.AddParameter('userId', SafeParameter, pkGETorPOST);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse de manière sécurisée...
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function ValidateAndSanitizeInput(const Input: string): string;
begin
  // Exemple simple : enlever les caractères potentiellement dangereux
  Result := Input;

  // Supprimer les caractères d'injection potentiels
  Result := StringReplace(Result, '''', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '--', '', [rfReplaceAll]);

  // Vérifier que l'entrée respecte un format attendu (par exemple, numérique)
  if not TRegEx.IsMatch(Result, '^[0-9]+$') then
    raise Exception.Create('Format d''entrée invalide. Seuls les chiffres sont autorisés.');
end;
```

#### Gérer les timeouts et les limites de connexion

Il est important de gérer correctement les délais d'attente pour éviter les attaques par déni de service :

```pas
procedure ConfigureConnectionTimeouts;
var
  RESTClient: TRESTClient;
begin
  RESTClient := TRESTClient.Create(nil);
  try
    // Configurer les délais d'attente
    RESTClient.ConnectTimeout := 5000;  // 5 secondes pour établir la connexion
    RESTClient.ReadTimeout := 10000;    // 10 secondes pour lire les données

    // Limiter la taille des réponses pour éviter les attaques par consommation de ressources
    RESTClient.ResponseMaxSize := 10 * 1024 * 1024;  // 10 MB maximum

    // Reste de la configuration...
  finally
    RESTClient.Free;
  end;
end;
```

### Journalisation et audit de sécurité

Pour détecter les tentatives d'attaque et résoudre les problèmes, une journalisation adéquate est essentielle :

```pas
type
  TSecurityLogLevel = (slInfo, slWarning, slError, slCritical);

procedure LogSecurityEvent(Level: TSecurityLogLevel; const Message, Source: string);
var
  LogFile: TextFile;
  LogFileName, LogMessage: string;
  TimeStamp: string;
begin
  // Générer un nom de fichier basé sur la date
  LogFileName := FormatDateTime('yyyy-mm-dd', Date) + '_security.log';

  // Préparer le message de journal
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);

  case Level of
    slInfo: LogMessage := 'INFO';
    slWarning: LogMessage := 'WARNING';
    slError: LogMessage := 'ERROR';
    slCritical: LogMessage := 'CRITICAL';
  end;

  LogMessage := Format('[%s] [%s] [%s] %s',
                       [TimeStamp, LogMessage, Source, Message]);

  // Écrire dans le fichier de journal
  AssignFile(LogFile, LogFileName);
  try
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, LogMessage);
  finally
    CloseFile(LogFile);
  end;

  // Pour les événements critiques, prendre des mesures supplémentaires
  if Level = slCritical then
  begin
    // Par exemple, envoyer une notification à l'administrateur
    SendSecurityAlert(LogMessage);
  end;
end;
```

### Bonnes pratiques pour la sécurisation des connexions

#### 1. Utiliser des versions récentes des protocoles de sécurité

```pas
procedure ConfigureModernSecurity;
var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Utiliser TLS 1.2 ou supérieur uniquement
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;

    // Désactiver les protocoles obsolètes
    SSLIOHandler.SSLOptions.SSLVersions := [sslvTLSv1_2, sslvTLSv1_3];

    // Utiliser des suites de chiffrement fortes
    SSLIOHandler.SSLOptions.CipherList := 'HIGH:!aNULL:!eNULL:!MD5:!RC4';

    // Reste de la configuration...
  finally
    // Gestion de la libération mémoire...
  end;
end;
```

#### 2. Utiliser l'authentification mutuelle

Dans certains scénarios, il est utile que les deux parties s'authentifient l'une envers l'autre :

```pas
procedure ConfigureMutualTLSAuthentication;
var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Configuration de base TLS
    SSLIOHandler.SSLOptions.Method := sslvTLSv1_2;
    SSLIOHandler.SSLOptions.Mode := sslmClient;

    // Certificat client pour l'authentification mutuelle
    SSLIOHandler.SSLOptions.CertFile := 'chemin/vers/client-cert.pem';
    SSLIOHandler.SSLOptions.KeyFile := 'chemin/vers/client-key.pem';

    // Vérification du certificat serveur
    SSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer];
    SSLIOHandler.SSLOptions.VerifyDepth := 9;

    // Fichier CA pour vérifier le certificat du serveur
    SSLIOHandler.SSLOptions.RootCertFile := 'chemin/vers/ca-cert.pem';

    // Reste de la configuration...
  finally
    // Gestion de la libération mémoire...
  end;
end;
```

#### 3. Implémenter une révocation de certificat

Vérifiez que les certificats n'ont pas été révoqués en utilisant des listes de révocation de certificats (CRL) ou OCSP :

```pas
procedure ConfigureCertificateRevocationCheck;
var
  SSLIOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  try
    // Activer la vérification de révocation
    SSLIOHandler.SSLOptions.VerifyMode := [sslvrfPeer];

    // Chemin vers les fichiers CRL
    SSLIOHandler.SSLOptions.CRLFile := 'chemin/vers/crl.pem';

    // Utiliser OCSP (Online Certificate Status Protocol)
    // Nécessite le support OpenSSL
    SSLIOHandler.SSLOptions.EnableOCSP := True;

    // Reste de la configuration...
  finally
    // Gestion de la libération mémoire...
  end;
end;
```

#### 4. Rotation régulière des clés et certificats

Mettez en place un système pour renouveler régulièrement vos certificats et clés :

```pas
procedure CheckCertificateExpiration;
var
  Certificate: TIdX509;
  DaysUntilExpiration: Integer;
begin
  Certificate := LoadCertificateFromFile('chemin/vers/certificat.pem');
  try
    // Calculer le nombre de jours avant expiration
    DaysUntilExpiration := Trunc(Certificate.NotAfter - Now);

    // Avertir si l'expiration approche
    if DaysUntilExpiration <= 30 then
    begin
      LogSecurityEvent(slWarning,
                      Format('Le certificat expire dans %d jours', [DaysUntilExpiration]),
                      'CertManager');

      // Notifier les administrateurs
      if DaysUntilExpiration <= 7 then
        SendCertExpirationAlert(Certificate.Subject, DaysUntilExpiration);
    end;
  finally
    Certificate.Free;
  end;
end;
```

### Exemple concret : Client API sécurisé

Voici un exemple de classe réutilisable pour effectuer des appels API sécurisés :

```pas
unit SecureAPIClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TSecureAPIClient = class
  private
    FBaseURL: string;
    FAuthToken: string;
    FIsAuthenticated: Boolean;

    function GetAuthorizationHeader: string;
  public
    constructor Create(const ABaseURL: string);

    // Authentification
    function Authenticate(const Username, Password: string): Boolean;
    procedure Logout;

    // Requêtes HTTP sécurisées
    function Get(const Resource: string): TJSONValue;
    function Post(const Resource: string; const Data: TJSONValue): TJSONValue;
    function Put(const Resource: string; const Data: TJSONValue): TJSONValue;
    function Delete(const Resource: string): Boolean;

    property IsAuthenticated: Boolean read FIsAuthenticated;
  end;

implementation

constructor TSecureAPIClient.Create(const ABaseURL: string);
begin
  inherited Create;
  FBaseURL := ABaseURL;
  FAuthToken := '';
  FIsAuthenticated := False;
end;

function TSecureAPIClient.Authenticate(const Username, Password: string): Boolean;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
begin
  Result := False;
  FAuthToken := '';
  FIsAuthenticated := False;

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration sécurisée
    RESTClient.BaseURL := FBaseURL;
    RESTClient.Accept := 'application/json';
    RESTClient.SecureProtocols := [TLSv1_2, TLSv1_3];
    RESTClient.ValidateServerCertificate := True;

    // Configurer les délais d'attente
    RESTClient.ConnectTimeout := 5000;
    RESTClient.ReadTimeout := 10000;

    // Configurer la requête d'authentification
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := 'auth/login';

    // Ajouter les informations d'authentification
    var AuthJSON := TJSONObject.Create;
    try
      AuthJSON.AddPair('username', Username);
      AuthJSON.AddPair('password', Password);

      RESTRequest.AddBody(AuthJSON.ToString,
                         TRESTContentType.ctAPPLICATION_JSON);
    finally
      AuthJSON.Free;
    end;

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      if JSONValue <> nil then
      try
        // Extraire le jeton d'authentification
        FAuthToken := JSONValue.GetValue<string>('token');
        FIsAuthenticated := FAuthToken <> '';
        Result := FIsAuthenticated;
      finally
        JSONValue.Free;
      end;
    end
    else
    begin
      LogSecurityEvent(slWarning,
                      Format('Échec d''authentification: %d - %s',
                            [RESTResponse.StatusCode, RESTResponse.StatusText]),
                      'SecureAPIClient');
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

procedure TSecureAPIClient.Logout;
begin
  FAuthToken := '';
  FIsAuthenticated := False;

  // Dans une implémentation complète, vous pourriez envoyer une requête
  // au serveur pour invalider le jeton
end;

function TSecureAPIClient.GetAuthorizationHeader: string;
begin
  if FAuthToken <> '' then
    Result := 'Bearer ' + FAuthToken
  else
    Result := '';
end;

function TSecureAPIClient.Get(const Resource: string): TJSONValue;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := nil;

  if not FIsAuthenticated then
    raise Exception.Create('Non authentifié. Connectez-vous d''abord.');

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration sécurisée
    RESTClient.BaseURL := FBaseURL;
    RESTClient.Accept := 'application/json';
    RESTClient.SecureProtocols := [TLSv1_2, TLSv1_3];

    // Configurer la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmGET;
    RESTRequest.Resource := Resource;

    // Ajouter l'en-tête d'autorisation
    RESTRequest.Params.AddHeader('Authorization', GetAuthorizationHeader);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      Result := TJSONObject.ParseJSONValue(RESTResponse.Content);
    end
    else if RESTResponse.StatusCode = 401 then
    begin
      // Jeton invalide ou expiré
      FIsAuthenticated := False;
      FAuthToken := '';
      raise Exception.Create('Session expirée. Veuillez vous reconnecter.');
    end
    else
    begin
      raise Exception.Create(Format('Erreur API: %d - %s',
                                   [RESTResponse.StatusCode, RESTResponse.StatusText]));
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

function TSecureAPIClient.Post(const Resource: string; const Data: TJSONValue): TJSONValue;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
begin
  Result := nil;

  if not FIsAuthenticated then
    raise Exception.Create('Non authentifié. Connectez-vous d''abord.');

  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration sécurisée
    RESTClient.BaseURL := FBaseURL;
    RESTClient.Accept := 'application/json';
    RESTClient.SecureProtocols := [TLSv1_2, TLSv1_3];

    // Configurer la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := rmPOST;
    RESTRequest.Resource := Resource;

    // Ajouter l'en-tête d'autorisation
    RESTRequest.Params.AddHeader('Authorization', GetAuthorizationHeader);

    // Ajouter les données
    if Data <> nil then
      RESTRequest.AddBody(Data.ToString, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if (RESTResponse.StatusCode >= 200) and (RESTResponse.StatusCode < 300) then
    begin
      Result := TJSONObject.ParseJSONValue(RESTResponse.Content);
    end
    else if RESTResponse.StatusCode = 401 then
    begin
      // Jeton invalide ou expiré
      FIsAuthenticated := False;
      FAuthToken := '';
      raise Exception.Create('Session expirée. Veuillez vous reconnecter.');
    end
    else
    begin
      raise Exception.Create(Format('Erreur API: %d - %s',
                                   [RESTResponse.StatusCode, RESTResponse.StatusText]));
    end;
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;

// Les méthodes Put et Delete suivent le même modèle que Post et Get...

end.
```

### Utilisation du client API sécurisé

```pas
procedure TMainForm.ButtonGetUserProfileClick(Sender: TObject);
var
  APIClient: TSecureAPIClient;
  UserProfile: TJSONValue;
begin
  APIClient := TSecureAPIClient.Create('https://api.exemple.com');
  try
    try
      // S'authentifier auprès de l'API
      if not APIClient.Authenticate(Username, Password) then
      begin
        ShowMessage('Erreur d''authentification');
        Exit;
      end;

      // Récupérer le profil utilisateur
      UserProfile := APIClient.Get('users/profile');
      try
        if UserProfile <> nil then
        begin
          // Afficher les informations de profil
          DisplayUserProfile(UserProfile);
        end;
      finally
        UserProfile.Free;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Erreur: ' + E.Message);

        // Gérer automatiquement les problèmes d'authentification
        if not APIClient.IsAuthenticated then
          ShowLoginForm;
      end;
    end;
  finally
    APIClient.Free;
  end;
end;
```

### Conclusion

La sécurisation des connexions est un aspect fondamental de toute application moderne. En suivant les bonnes pratiques présentées dans ce chapitre, vous pouvez protéger efficacement les communications de vos applications Delphi.

Les points clés à retenir :

1. **Utilisez toujours HTTPS** ou des connexions chiffrées pour transmettre des données sensibles.

2. **Vérifiez les certificats** des serveurs auxquels vous vous connectez.

3. **Implémentez une authentification robuste** pour vos API et services.

4. **Utilisez des versions récentes des protocoles de sécurité** comme TLS 1.2 ou supérieur.

5. **Journalisez les événements de sécurité** pour détecter et répondre aux problèmes potentiels.

6. **Gérez correctement les délais d'attente** pour prévenir les attaques par déni de service.

7. **Validez toutes les entrées utilisateur** avant de les inclure dans des requêtes réseau.

8. **Renouvelez régulièrement vos certificats et clés** pour maintenir un niveau de sécurité élevé.

Dans le prochain chapitre, nous aborderons la protection contre les vulnérabilités courantes, qui complète notre approche globale de la sécurité des applications.

### Exercices pratiques

1. Créez une application cliente qui se connecte à une API REST via HTTPS et authentifie l'utilisateur à l'aide d'un jeton.

2. Modifiez une application existante pour utiliser le certificat pinning afin de renforcer la sécurité des connexions.

3. Implémentez un système de journalisation des événements de sécurité dans votre application.

4. Créez une classe réutilisable pour gérer les connexions SSL/TLS sécurisées à un serveur.

5. Pour les plus avancés : Implémentez un système de messagerie chiffré de bout en bout utilisant le chiffrement asymétrique pour l'échange de clés et le chiffrement symétrique pour les messages.
