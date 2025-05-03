# 10.5 Architecture client-serveur

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'architecture client-serveur est l'un des modèles fondamentaux de la programmation réseau et des applications distribuées. Dans ce chapitre, nous allons explorer les principes de cette architecture, comprendre ses avantages et inconvénients, et apprendre à l'implémenter dans nos applications.

## Qu'est-ce que l'architecture client-serveur ?

### Définition et concept de base

L'architecture client-serveur est un modèle de communication où les tâches sont réparties entre deux types de composants :

- **Le serveur** : Fournit des ressources, des services ou des données
- **Le client** : Utilise ces ressources, services ou données

Cette séparation des responsabilités permet de créer des applications plus modulaires, évolutives et faciles à maintenir.

### Analogie de la vie quotidienne

Pour mieux comprendre, imaginez un restaurant :
- **Le serveur** est comme le cuisinier en cuisine, qui prépare les plats (traitement des données)
- **Le client** est comme le client du restaurant, qui passe commande et reçoit son repas (demande et utilisation des données)
- **Le serveur de restaurant** est comme le réseau, qui transporte les commandes et les plats entre la salle et la cuisine

## Types d'architectures client-serveur

### Architecture à deux niveaux (2-tiers)

C'est la forme la plus simple, où le client communique directement avec le serveur :

```
[Client] <-----> [Serveur]
```

Exemple : Une application de bureau qui se connecte directement à une base de données.

### Architecture à trois niveaux (3-tiers)

Cette architecture ajoute une couche intermédiaire entre le client et le serveur de données :

```
[Client] <-----> [Serveur d'application] <-----> [Serveur de données]
```

Exemple : Une application web où le navigateur (client) communique avec un serveur web (application), qui interagit avec une base de données (données).

### Architecture multi-niveaux (n-tiers)

Des architectures plus complexes peuvent comporter de nombreuses couches, chacune ayant un rôle spécifique :

```
[Client] <---> [Serveur Web] <---> [Serveur d'application] <---> [Serveur de données] <---> [Services externes]
```

Exemple : Une application d'entreprise moderne avec microservices.

## Avantages et inconvénients

### Avantages

1. **Centralisation des données** : Les données sont stockées et gérées à un seul endroit
2. **Sécurité améliorée** : Plus facile de protéger les données sur un serveur central
3. **Maintenance simplifiée** : Les mises à jour du serveur sont appliquées à tous les clients automatiquement
4. **Évolutivité** : Possibilité d'ajouter des clients sans changer l'infrastructure
5. **Répartition des ressources** : Le serveur peut gérer les tâches intensives, libérant les ressources du client

### Inconvénients

1. **Dépendance au serveur** : Si le serveur tombe en panne, tous les clients sont affectés
2. **Coût initial plus élevé** : Nécessite une infrastructure serveur
3. **Complexité** : Plus complexe à mettre en place qu'une application autonome
4. **Latence** : La communication réseau peut introduire des délais
5. **Bande passante** : Nécessite une connexion réseau adéquate

## Implémentation d'une architecture client-serveur

### Composants essentiels

Pour mettre en place une architecture client-serveur, vous aurez besoin de :

1. **Un protocole de communication** : Comment les messages sont formatés (HTTP, TCP/IP, etc.)
2. **Un mécanisme de transport** : Comment les messages sont transmis (sockets, WebSockets, etc.)
3. **Un format de données** : Comment les données sont structurées (JSON, XML, binaire, etc.)
4. **Une logique de serveur** : Pour traiter les requêtes et renvoyer des réponses
5. **Une logique de client** : Pour envoyer des requêtes et traiter les réponses

### Exemple simple : Application de chat

Voyons un exemple concret d'architecture client-serveur avec une application de chat simple.

#### Côté serveur

```pascal
unit ChatServer;

interface

uses
  System.SysUtils, System.Classes, IdTCPServer, IdContext, IdGlobal;

type
  TChatServer = class
  private
    FServer: TIdTCPServer;
    FClients: TList<TIdContext>;
    FLock: TCriticalSection;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);
    procedure BroadcastMessage(const ASender: TIdContext; const AMessage: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;

    property Server: TIdTCPServer read FServer;
  end;

implementation

constructor TChatServer.Create;
begin
  inherited Create;

  // Créer la liste des clients et le verrou
  FClients := TList<TIdContext>.Create;
  FLock := TCriticalSection.Create;

  // Configurer le serveur TCP
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnExecute := OnExecute;
end;

destructor TChatServer.Destroy;
begin
  // Libérer les ressources
  Stop;
  FServer.Free;
  FLock.Free;
  FClients.Free;

  inherited;
end;

procedure TChatServer.Start(APort: Integer);
begin
  // Configurer le port et démarrer l'écoute
  FServer.DefaultPort := APort;
  FServer.Active := True;
end;

procedure TChatServer.Stop;
begin
  // Arrêter le serveur
  FServer.Active := False;

  // Vider la liste des clients
  FLock.Acquire;
  try
    FClients.Clear;
  finally
    FLock.Release;
  end;
end;

procedure TChatServer.OnConnect(AContext: TIdContext);
begin
  // Ajouter le client à la liste
  FLock.Acquire;
  try
    FClients.Add(AContext);
  finally
    FLock.Release;
  end;

  // Envoyer un message de bienvenue
  AContext.Connection.IOHandler.WriteLn('Bienvenue sur le serveur de chat!');

  // Annoncer la connexion aux autres clients
  BroadcastMessage(AContext, 'Un nouveau participant a rejoint le chat.');
end;

procedure TChatServer.OnDisconnect(AContext: TIdContext);
begin
  // Annoncer la déconnexion
  BroadcastMessage(AContext, 'Un participant a quitté le chat.');

  // Retirer le client de la liste
  FLock.Acquire;
  try
    FClients.Remove(AContext);
  finally
    FLock.Release;
  end;
end;

procedure TChatServer.OnExecute(AContext: TIdContext);
var
  Message: string;
begin
  // Lire le message envoyé par le client
  Message := AContext.Connection.IOHandler.ReadLn;

  // Si le message n'est pas vide, le diffuser
  if Message <> '' then
    BroadcastMessage(AContext, Message);
end;

procedure TChatServer.BroadcastMessage(const ASender: TIdContext; const AMessage: string);
var
  Client: TIdContext;
  FormattedMessage: string;
  ClientIP: string;
  i: Integer;
begin
  // Obtenir l'adresse IP du client émetteur
  ClientIP := ASender.Connection.Socket.Binding.PeerIP;

  // Formater le message avec l'adresse de l'émetteur
  FormattedMessage := Format('[%s]: %s', [ClientIP, AMessage]);

  // Envoyer le message à tous les clients
  FLock.Acquire;
  try
    for i := 0 to FClients.Count - 1 do
    begin
      Client := FClients[i];

      // Ne pas renvoyer le message à l'émetteur
      if Client <> ASender then
      begin
        try
          Client.Connection.IOHandler.WriteLn(FormattedMessage);
        except
          // Ignorer les erreurs d'envoi
        end;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

end.
```

#### Côté client

```pascal
unit ChatClient;

interface

uses
  System.SysUtils, System.Classes, IdTCPClient, IdGlobal;

type
  TMessageEvent = procedure(Sender: TObject; const Message: string) of object;

  TChatClient = class
  private
    FClient: TIdTCPClient;
    FConnected: Boolean;
    FOnMessage: TMessageEvent;
    FReceiverThread: TThread;

    procedure StartReceiver;
    procedure StopReceiver;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AHost: string; APort: Integer): Boolean;
    procedure Disconnect;
    function SendMessage(const AMessage: string): Boolean;

    property Connected: Boolean read FConnected;
    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

type
  TReceiverThread = class(TThread)
  private
    FClient: TIdTCPClient;
    FOnMessage: TMessageEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient; AOnMessage: TMessageEvent);
  end;

constructor TReceiverThread.Create(AClient: TIdTCPClient; AOnMessage: TMessageEvent);
begin
  inherited Create(False);
  FClient := AClient;
  FOnMessage := AOnMessage;
  FreeOnTerminate := False;
end;

procedure TReceiverThread.Execute;
var
  Message: string;
begin
  while not Terminated do
  begin
    try
      // Lire les messages du serveur
      Message := FClient.IOHandler.ReadLn;

      // Déclencher l'événement
      if Assigned(FOnMessage) then
        TThread.Synchronize(nil, procedure
                               begin
                                 FOnMessage(Self, Message);
                               end);
    except
      // En cas d'erreur, sortir de la boucle
      Break;
    end;
  end;
end;

constructor TChatClient.Create;
begin
  inherited Create;

  // Créer le client TCP
  FClient := TIdTCPClient.Create(nil);
  FConnected := False;
end;

destructor TChatClient.Destroy;
begin
  // Se déconnecter si nécessaire
  if FConnected then
    Disconnect;

  // Libérer les ressources
  FClient.Free;

  inherited;
end;

function TChatClient.Connect(const AHost: string; APort: Integer): Boolean;
begin
  Result := False;

  // Si déjà connecté, ne rien faire
  if FConnected then
    Exit;

  try
    // Configurer la connexion
    FClient.Host := AHost;
    FClient.Port := APort;

    // Se connecter au serveur
    FClient.Connect;
    FConnected := True;

    // Démarrer le thread de réception
    StartReceiver;

    Result := True;
  except
    FConnected := False;
  end;
end;

procedure TChatClient.Disconnect;
begin
  // Si pas connecté, ne rien faire
  if not FConnected then
    Exit;

  // Arrêter le thread de réception
  StopReceiver;

  // Se déconnecter du serveur
  FClient.Disconnect;
  FConnected := False;
end;

function TChatClient.SendMessage(const AMessage: string): Boolean;
begin
  Result := False;

  // Si pas connecté, ne rien faire
  if not FConnected then
    Exit;

  try
    // Envoyer le message au serveur
    FClient.IOHandler.WriteLn(AMessage);
    Result := True;
  except
    // En cas d'erreur, marquer comme déconnecté
    FConnected := False;
  end;
end;

procedure TChatClient.StartReceiver;
begin
  // Créer et démarrer le thread de réception
  FReceiverThread := TReceiverThread.Create(FClient, FOnMessage);
  FReceiverThread.Start;
end;

procedure TChatClient.StopReceiver;
begin
  // Arrêter le thread de réception
  if Assigned(FReceiverThread) then
  begin
    TReceiverThread(FReceiverThread).Terminate;
    FReceiverThread.WaitFor;
    FreeAndNil(FReceiverThread);
  end;
end;

end.
```

#### Interface utilisateur du client

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ChatClient;

type
  TFormChat = class(TForm)
    MemoChat: TMemo;
    EditMessage: TEdit;
    BtnSend: TButton;
    PanelConnection: TPanel;
    EditHost: TEdit;
    EditPort: TEdit;
    BtnConnect: TButton;
    BtnDisconnect: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: Char);
  private
    FChatClient: TChatClient;
    procedure OnChatMessage(Sender: TObject; const Message: string);
    procedure UpdateControls;
  end;

var
  FormChat: TFormChat;

implementation

{$R *.dfm}

procedure TFormChat.FormCreate(Sender: TObject);
begin
  // Créer le client de chat
  FChatClient := TChatClient.Create;
  FChatClient.OnMessage := OnChatMessage;

  // Configurer les valeurs par défaut
  EditHost.Text := '127.0.0.1';
  EditPort.Text := '8080';

  // Mettre à jour l'interface
  UpdateControls;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  // Libérer le client
  FChatClient.Free;
end;

procedure TFormChat.BtnConnectClick(Sender: TObject);
var
  Host: string;
  Port: Integer;
begin
  // Récupérer les paramètres de connexion
  Host := EditHost.Text;
  Port := StrToIntDef(EditPort.Text, 8080);

  // Se connecter au serveur
  if FChatClient.Connect(Host, Port) then
  begin
    MemoChat.Lines.Add('Connecté au serveur ' + Host + ':' + IntToStr(Port));
    UpdateControls;
  end
  else
    ShowMessage('Impossible de se connecter au serveur');
end;

procedure TFormChat.BtnDisconnectClick(Sender: TObject);
begin
  // Se déconnecter du serveur
  FChatClient.Disconnect;
  MemoChat.Lines.Add('Déconnecté du serveur');
  UpdateControls;
end;

procedure TFormChat.BtnSendClick(Sender: TObject);
var
  Message: string;
begin
  // Récupérer le message à envoyer
  Message := EditMessage.Text;

  if (Message <> '') and FChatClient.Connected then
  begin
    // Envoyer le message
    if FChatClient.SendMessage(Message) then
    begin
      // Afficher le message localement
      MemoChat.Lines.Add('[Moi]: ' + Message);
      EditMessage.Clear;
    end
    else
    begin
      ShowMessage('Erreur lors de l''envoi du message');
      UpdateControls;
    end;
  end;
end;

procedure TFormChat.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  // Envoyer le message quand on appuie sur Entrée
  if Key = #13 then
  begin
    Key := #0;  // Supprimer le bip
    BtnSendClick(Sender);
  end;
end;

procedure TFormChat.OnChatMessage(Sender: TObject; const Message: string);
begin
  // Afficher le message reçu
  MemoChat.Lines.Add(Message);

  // Faire défiler vers le bas
  SendMessage(MemoChat.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFormChat.UpdateControls;
begin
  // Activer/désactiver les contrôles selon l'état de la connexion
  BtnConnect.Enabled := not FChatClient.Connected;
  BtnDisconnect.Enabled := FChatClient.Connected;
  BtnSend.Enabled := FChatClient.Connected;
  EditMessage.Enabled := FChatClient.Connected;
  EditHost.Enabled := not FChatClient.Connected;
  EditPort.Enabled := not FChatClient.Connected;
end;

end.
```

#### Interface utilisateur du serveur

```pascal
unit ServerForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ChatServer;

type
  TFormServer = class(TForm)
    MemoLog: TMemo;
    PanelControl: TPanel;
    EditPort: TEdit;
    BtnStart: TButton;
    BtnStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnStopClick(Sender: TObject);
  private
    FChatServer: TChatServer;
    procedure UpdateControls;
  end;

var
  FormServer: TFormServer;

implementation

{$R *.dfm}

procedure TFormServer.FormCreate(Sender: TObject);
begin
  // Créer le serveur de chat
  FChatServer := TChatServer.Create;

  // Configurer les valeurs par défaut
  EditPort.Text := '8080';

  // Mettre à jour l'interface
  UpdateControls;
end;

procedure TFormServer.FormDestroy(Sender: TObject);
begin
  // Libérer le serveur
  FChatServer.Free;
end;

procedure TFormServer.BtnStartClick(Sender: TObject);
var
  Port: Integer;
begin
  // Récupérer le port d'écoute
  Port := StrToIntDef(EditPort.Text, 8080);

  try
    // Démarrer le serveur
    FChatServer.Start(Port);
    MemoLog.Lines.Add('Serveur démarré sur le port ' + IntToStr(Port));

    // Mettre à jour l'interface
    UpdateControls;
  except
    on E: Exception do
      ShowMessage('Erreur lors du démarrage du serveur: ' + E.Message);
  end;
end;

procedure TFormServer.BtnStopClick(Sender: TObject);
begin
  // Arrêter le serveur
  FChatServer.Stop;
  MemoLog.Lines.Add('Serveur arrêté');

  // Mettre à jour l'interface
  UpdateControls;
end;

procedure TFormServer.UpdateControls;
begin
  // Activer/désactiver les contrôles selon l'état du serveur
  BtnStart.Enabled := not FChatServer.Server.Active;
  BtnStop.Enabled := FChatServer.Server.Active;
  EditPort.Enabled := not FChatServer.Server.Active;
end;

end.
```

## Modèles d'architecture client-serveur

### Modèle basé sur les requêtes-réponses

C'est le modèle le plus courant, où le client envoie une requête et attend une réponse du serveur.

Exemple : Une application qui récupère des données d'une base de données.

```
Client                      Serveur
  |                            |
  |------- Requête ----------->|
  |                            | (Traitement)
  |<------- Réponse -----------|
  |                            |
```

### Modèle basé sur les événements (Publish-Subscribe)

Dans ce modèle, le client s'abonne à certains événements et le serveur envoie des notifications lorsque ces événements se produisent.

Exemple : Une application de surveillance qui notifie lorsque des seuils sont dépassés.

```
Client A            Serveur            Client B
  |                   |                   |
  |-- S'abonner à X ->|                   |
  |                   |<-- S'abonner à Y -|
  |                   |                   |
  |                   | (X se produit)    |
  |<- Notification X -|                   |
  |                   |                   |
  |                   | (Y se produit)    |
  |                   |- Notification Y ->|
```

### Modèle basé sur les messages (Message Queuing)

Ce modèle utilise des files d'attente pour stocker les messages avant qu'ils ne soient traités, permettant une communication asynchrone.

Exemple : Un système de traitement de commandes.

```
Client                      File d'attente                     Serveur
  |                              |                                |
  |------- Message ------------->|                                |
  |                              |                                |
  |                              |<-------- Récupérer message ----|
  |                              |                                | (Traitement)
  |                              |<-------- Ack ------------------|
  |                              |                                |
```

## Conception d'une architecture client-serveur robuste

### Gestion des erreurs et reconnexion

Pour créer une application client-serveur robuste, il est crucial de gérer les erreurs de connexion :

```pascal
procedure TMyClient.TryConnect;
const
  MAX_RETRIES = 3;
var
  RetryCount: Integer;
begin
  RetryCount := 0;

  while (RetryCount < MAX_RETRIES) and (not Connected) do
  begin
    try
      // Tentative de connexion
      Connect(ServerHost, ServerPort);

      if Connected then
        Exit;  // Connexion réussie
    except
      on E: Exception do
      begin
        // Journaliser l'erreur
        Log('Erreur de connexion: ' + E.Message);
      end;
    end;

    // Attendre avant de réessayer
    Sleep(1000 * (RetryCount + 1));
    Inc(RetryCount);
  end;

  if not Connected then
    // Informer l'utilisateur après plusieurs échecs
    RaiseEvent(OnConnectionFailed, 'Impossible de se connecter au serveur après plusieurs tentatives');
end;
```

### Sécurité

La sécurité est un aspect crucial de toute architecture client-serveur :

```pascal
procedure TSecureServer.OnClientConnect(AContext: TIdContext);
var
  Username, Password, Token: string;
  Authenticated: Boolean;
begin
  // Lire les informations d'authentification
  Username := AContext.Connection.IOHandler.ReadLn;
  Password := AContext.Connection.IOHandler.ReadLn;

  // Vérifier les identifiants
  Authenticated := AuthenticateUser(Username, Password);

  if Authenticated then
  begin
    // Générer un jeton d'authentification
    Token := GenerateToken(Username);

    // Envoyer le jeton au client
    AContext.Connection.IOHandler.WriteLn('AUTH_OK:' + Token);

    // Stocker les informations dans le contexte
    AContext.Data := TClientData.Create(Username, Token);
  end
  else
  begin
    // Échec d'authentification
    AContext.Connection.IOHandler.WriteLn('AUTH_FAILED');

    // Déconnecter le client
    AContext.Connection.Disconnect;
  end;
end;
```

### Équilibrage de charge

Pour les applications à grande échelle, l'équilibrage de charge est essentiel :

```pascal
TLoadBalancer = class
private
  FServers: TList<TServerInfo>;
  FCurrentIndex: Integer;
  FLock: TCriticalSection;

  function SelectServer: TServerInfo;
public
  constructor Create;
  destructor Destroy; override;

  procedure AddServer(const AHost: string; APort: Integer);
  procedure RemoveServer(const AHost: string; APort: Integer);
  function GetNextServer: TServerInfo;
end;

function TLoadBalancer.GetNextServer: TServerInfo;
begin
  FLock.Acquire;
  try
    if FServers.Count = 0 then
      raise Exception.Create('Aucun serveur disponible');

    // Algorithme simple de round-robin
    FCurrentIndex := (FCurrentIndex + 1) mod FServers.Count;
    Result := FServers[FCurrentIndex];
  finally
    FLock.Release;
  end;
end;
```

### Haute disponibilité

Pour assurer la haute disponibilité, implémentez des mécanismes de basculement :

```pascal
procedure THighAvailabilityClient.Connect;
var
  i: Integer;
  Server: TServerInfo;
begin
  // Essayer chaque serveur dans la liste
  for i := 0 to FServers.Count - 1 do
  begin
    Server := FServers[i];

    try
      // Tenter de se connecter au serveur
      FClient.Host := Server.Host;
      FClient.Port := Server.Port;
      FClient.Connect;

      // Si connecté, sortir de la boucle
      if FClient.Connected then
      begin
        FCurrentServer := i;
        FConnected := True;

        // Déclencher l'événement
        if Assigned(FOnConnect) then
          FOnConnect(Self);

        Exit;
      end;
    except
      // Continuer avec le serveur suivant
    end;
  end;

  // Aucun serveur disponible
  raise Exception.Create('Impossible de se connecter à un serveur');
end;
```

## Exemples d'architectures client-serveur dans le monde réel

### Applications de bureau

De nombreuses applications de bureau utilisent une architecture client-serveur pour accéder à des données centralisées :

- **CRM** (Customer Relationship Management)
- **ERP** (Enterprise Resource Planning)
- **Logiciels de comptabilité**
- **Applications de gestion de projet**

### Applications web

Presque toutes les applications web suivent une architecture client-serveur :

- **Navigateur web** (client) communiquant avec un **serveur web**
- **Applications mobiles** communiquant avec des **API REST**
- **Applications SPA** (Single-Page Application) utilisant des **services backend**

### Applications distribuées

Les systèmes distribués complexes utilisent souvent des architectures client-serveur avancées :

- **Microservices**
- **Systèmes basés sur les événements**
- **Architecture orientée services (SOA)**

## Bonnes pratiques

### Conception

1. **Séparation des préoccupations** : Divisez clairement les responsabilités entre client et serveur
2. **Interface cohérente** : Définissez un contrat clair entre client et serveur
3. **Idempotence** : Assurez-vous que les opérations peuvent être répétées sans effets secondaires
4. **Stateless vs Stateful** : Choisissez judicieusement selon vos besoins

### Implémentation

1. **Gestion des timeouts** : Définissez des délais d'attente appropriés
2. **Reconnexion automatique** : Implémentez des mécanismes de reconnexion
3. **Mise en cache** : Réduisez les appels réseau en mettant en cache les résultats
4. **Compression** : Réduisez la quantité de données transmises
5. **Traitement par lots** : Regroupez plusieurs opérations en une seule requête

### Sécurité

1. **Authentification** : Vérifiez l'identité des clients
2. **Autorisation** : Contrôlez l'accès aux ressources
3. **Chiffrement** : Protégez les données en transit
4. **Validation des entrées** : Vérifiez toutes les données provenant des clients
5. **Protection contre les attaques** : Implémentez des mécanismes contre les attaques courantes (injection SQL, XSS, etc.)

## Tendances actuelles

### Microservices

L'architecture de microservices décompose les applications en petits services autonomes qui communiquent entre eux via des API :

```
[Client] <---> [API Gateway] <---> [Service A]
                    ^
                    |
                    v
              [Service B] <---> [Service C]
```

### Serverless

L'architecture serverless (sans serveur) permet aux développeurs de se concentrer sur le code sans se soucier de l'infrastructure :

```
[Client] <---> [API Gateway] <---> [Fonction A] --> [Base de données]
                    ^
                    |
                    v
              [Fonction B] <---> [Service externe]
```

### Edge Computing

Le edge computing rapproche le traitement des données de leur source, réduisant la latence :

```
[Appareil IoT] <---> [Serveur Edge] <---> [Cloud]
```

# 10.5 Architecture client-serveur (Suite)

## Conclusion

L'architecture client-serveur est un modèle fondamental pour développer des applications distribuées. En comprenant ses principes, ses avantages et ses inconvénients, vous pouvez concevoir des systèmes évolutifs, maintenables et performants.

Ce modèle, bien qu'ancien, continue d'évoluer pour répondre aux besoins modernes des applications, que ce soit dans le cloud, sur les appareils mobiles ou dans les systèmes IoT. La maîtrise de cette architecture vous donne une base solide pour comprendre et implémenter des systèmes distribués plus complexes.

N'oubliez pas que la communication entre le client et le serveur est au cœur de cette architecture. Une bonne conception de cette communication est essentielle pour le succès de votre application.

## Exercices pratiques

Pour consolider vos connaissances, voici quelques exercices pratiques :

### Exercice 1 : Application de prise de notes

Créez une application client-serveur simple de prise de notes avec les fonctionnalités suivantes :
- Le serveur stocke les notes dans un fichier texte
- Le client permet de créer, lire, modifier et supprimer des notes
- Implémentez une authentification basique

### Exercice 2 : Moniteur système

Développez un système de surveillance avec :
- Un serveur qui collecte des informations système (CPU, mémoire, disque)
- Plusieurs clients qui peuvent se connecter pour visualiser ces informations
- Des alertes envoyées aux clients lorsque des seuils sont dépassés

### Exercice 3 : Jeu multijoueur simple

Créez un jeu tic-tac-toe (morpion) multijoueur :
- Un serveur qui gère l'état du jeu et les connexions des joueurs
- Des clients qui permettent aux joueurs d'interagir avec le jeu
- Un mécanisme pour apparier les joueurs

## Approches alternatives à l'architecture client-serveur

### Architecture peer-to-peer (P2P)

Contrairement au modèle client-serveur, l'architecture P2P distribue les responsabilités entre tous les participants :

```
   [Nœud A] <-----> [Nœud B]
      ^               ^
      |               |
      v               v
   [Nœud C] <-----> [Nœud D]
```

#### Avantages du P2P :
- Pas de point unique de défaillance
- Évolutivité naturelle
- Coûts d'infrastructure réduits

#### Inconvénients du P2P :
- Plus difficile à sécuriser
- Performances variables
- Complexité accrue

#### Exemple d'implémentation P2P simplifiée :

```pascal
type
  TPeerNode = class
  private
    FPeers: TList<TPeerConnection>;
    FServer: TIdTCPServer;
    FLock: TCriticalSection;

    procedure OnPeerConnect(AContext: TIdContext);
    procedure OnPeerMessage(AContext: TIdContext);
  public
    constructor Create(APort: Integer);
    destructor Destroy; override;

    procedure AddPeer(const AHost: string; APort: Integer);
    procedure BroadcastMessage(const AMessage: string);
    procedure ProcessMessage(const AMessage: string);
  end;

procedure TPeerNode.BroadcastMessage(const AMessage: string);
var
  Peer: TPeerConnection;
  i: Integer;
begin
  FLock.Acquire;
  try
    // Envoyer le message à tous les pairs connectés
    for i := 0 to FPeers.Count - 1 do
    begin
      Peer := FPeers[i];

      try
        Peer.SendMessage(AMessage);
      except
        // Si erreur, marquer le pair comme déconnecté
        Peer.Connected := False;
      end;
    end;

    // Nettoyer les pairs déconnectés
    for i := FPeers.Count - 1 downto 0 do
      if not FPeers[i].Connected then
        FPeers.Delete(i);
  finally
    FLock.Release;
  end;
end;
```

### Architecture hybride

De nombreux systèmes modernes combinent les approches client-serveur et P2P pour profiter des avantages de chacune :

```
                 [Serveur central]
                 /      |       \
                /       |        \
[Client/Pair A] <---> [Client/Pair B] <---> [Client/Pair C]
```

Le serveur central gère l'authentification, la découverte des pairs et la coordination, tandis que certaines communications se font directement entre les pairs.

#### Exemple d'utilisation :
- Applications de visioconférence (Zoom, Teams)
- Jeux en ligne
- Applications de partage de fichiers

## Étude de cas : Application de chat évoluée

Pour illustrer un cas réel, développons une application de chat plus évoluée avec des fonctionnalités supplémentaires.

### Fonctionnalités avancées

1. **Salons de discussion** : Plusieurs canaux de discussion
2. **Messages privés** : Communication directe entre utilisateurs
3. **Historique** : Stockage des messages pour consultation ultérieure
4. **Statut utilisateur** : En ligne, absent, occupé
5. **Transfert de fichiers** : Partage de documents entre utilisateurs

### Architecture du système

```
[Client] <---> [Serveur d'authentification] <---> [Base de données]
   ^                     ^
   |                     |
   v                     v
[Serveur de messages] <---> [Serveur de stockage]
```

### Implémentation du protocole de communication

Pour cette application, nous utiliserons un protocole basé sur JSON :

```pascal
type
  TMessageType = (mtChat, mtPrivate, mtSystem, mtJoinRoom, mtLeaveRoom, mtFileTransfer);

  TChatMessage = record
    MessageType: TMessageType;
    Sender: string;
    Recipient: string;  // Utilisateur ou salon
    Content: string;
    Timestamp: TDateTime;

    function ToJSON: string;
    procedure FromJSON(const AJSONString: string);
  end;

function TChatMessage.ToJSON: string;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', Integer(MessageType));
    JSON.AddPair('sender', Sender);
    JSON.AddPair('recipient', Recipient);
    JSON.AddPair('content', Content);
    JSON.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Timestamp));

    Result := JSON.ToString;
  finally
    JSON.Free;
  end;
end;

procedure TChatMessage.FromJSON(const AJSONString: string);
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
  try
    MessageType := TMessageType(JSON.GetValue<Integer>('type'));
    Sender := JSON.GetValue<string>('sender');
    Recipient := JSON.GetValue<string>('recipient');
    Content := JSON.GetValue<string>('content');
    Timestamp := ISO8601ToDate(JSON.GetValue<string>('timestamp'));
  finally
    JSON.Free;
  end;
end;
```

### Serveur de chat avancé

Le serveur de chat gère plusieurs salons et les communications privées :

```pascal
type
  TChatRoom = class
  private
    FName: string;
    FUsers: TList<string>;
    FMessages: TList<TChatMessage>;
    FLock: TCriticalSection;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure AddUser(const AUsername: string);
    procedure RemoveUser(const AUsername: string);
    procedure AddMessage(const AMessage: TChatMessage);
    function GetRecentMessages(Count: Integer): TArray<TChatMessage>;

    property Name: string read FName;
    property Users: TList<string> read FUsers;
  end;

  TChatServer = class
  private
    FServer: TIdTCPServer;
    FUsers: TDictionary<string, TIdContext>;
    FRooms: TObjectDictionary<string, TChatRoom>;
    FPrivateMessages: TDictionary<string, TList<TChatMessage>>;
    FLock: TCriticalSection;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);

    procedure ProcessMessage(AContext: TIdContext; const AMessageStr: string);
    procedure SendToRoom(const ARoomName: string; const AMessage: TChatMessage);
    procedure SendPrivateMessage(const ARecipient: string; const AMessage: TChatMessage);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;

    function CreateRoom(const ARoomName: string): Boolean;
    procedure DeleteRoom(const ARoomName: string);
  end;

procedure TChatServer.ProcessMessage(AContext: TIdContext; const AMessageStr: string);
var
  Message: TChatMessage;
  Username: string;
begin
  // Extraire le nom d'utilisateur du contexte
  Username := TClientData(AContext.Data).Username;

  // Convertir la chaîne JSON en message
  Message.FromJSON(AMessageStr);

  // Traiter selon le type de message
  case Message.MessageType of
    mtChat:
      // Message de salon
      SendToRoom(Message.Recipient, Message);

    mtPrivate:
      // Message privé
      SendPrivateMessage(Message.Recipient, Message);

    mtJoinRoom:
      // Rejoindre un salon
      if FRooms.ContainsKey(Message.Recipient) then
      begin
        FRooms[Message.Recipient].AddUser(Username);

        // Envoyer l'historique récent
        SendRoomHistory(AContext, Message.Recipient);

        // Annoncer l'arrivée
        SendSystemMessage(Message.Recipient, Username + ' a rejoint le salon');
      end;

    mtLeaveRoom:
      // Quitter un salon
      if FRooms.ContainsKey(Message.Recipient) then
      begin
        FRooms[Message.Recipient].RemoveUser(Username);

        // Annoncer le départ
        SendSystemMessage(Message.Recipient, Username + ' a quitté le salon');
      end;

    mtFileTransfer:
      // Transfert de fichier (simplifié)
      if Message.Recipient <> '' then
        SendPrivateMessage(Message.Recipient, Message)
      else
        SendToRoom(Message.Recipient, Message);
  end;
end;
```

### Client de chat avancé

Le client doit gérer les différentes fonctionnalités :

```pascal
type
  TChatClient = class
  private
    FClient: TIdTCPClient;
    FUsername: string;
    FPassword: string;
    FCurrentRoom: string;
    FRooms: TList<string>;
    FOnlineUsers: TList<string>;
    FReceiverThread: TThread;

    FOnChatMessage: TMessageEvent;
    FOnPrivateMessage: TMessageEvent;
    FOnSystemMessage: TMessageEvent;
    FOnUserListUpdate: TNotifyEvent;
    FOnRoomListUpdate: TNotifyEvent;

    procedure StartReceiver;
    procedure StopReceiver;
    procedure ProcessMessage(const AMessageStr: string);
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AHost: string; APort: Integer): Boolean;
    function Login(const AUsername, APassword: string): Boolean;
    procedure Disconnect;

    function JoinRoom(const ARoomName: string): Boolean;
    procedure LeaveRoom(const ARoomName: string);
    procedure SendChatMessage(const AMessage: string);
    procedure SendPrivateMessage(const ARecipient, AMessage: string);
    function SendFile(const ARecipient, AFilePath: string): Boolean;

    property CurrentRoom: string read FCurrentRoom;
    property Rooms: TList<string> read FRooms;
    property OnlineUsers: TList<string> read FOnlineUsers;

    property OnChatMessage: TMessageEvent read FOnChatMessage write FOnChatMessage;
    property OnPrivateMessage: TMessageEvent read FOnPrivateMessage write FOnPrivateMessage;
    property OnSystemMessage: TMessageEvent read FOnSystemMessage write FOnSystemMessage;
  end;

procedure TChatClient.ProcessMessage(const AMessageStr: string);
var
  Message: TChatMessage;
begin
  // Convertir la chaîne JSON en message
  Message.FromJSON(AMessageStr);

  // Traiter selon le type de message
  case Message.MessageType of
    mtChat:
      // Message de salon
      if Assigned(FOnChatMessage) then
        TThread.Synchronize(nil, procedure
                               begin
                                 FOnChatMessage(Self, Message.Sender, Message.Content);
                               end);

    mtPrivate:
      // Message privé
      if Assigned(FOnPrivateMessage) then
        TThread.Synchronize(nil, procedure
                               begin
                                 FOnPrivateMessage(Self, Message.Sender, Message.Content);
                               end);

    mtSystem:
      // Message système
      if Assigned(FOnSystemMessage) then
        TThread.Synchronize(nil, procedure
                               begin
                                 FOnSystemMessage(Self, Message.Sender, Message.Content);
                               end);

    mtFileTransfer:
      // Réception d'un fichier
      ProcessFileTransfer(Message);
  end;
end;
```

### Interface utilisateur avec onglets de salon

L'interface utilisateur doit permettre de naviguer entre les salons et les conversations privées :

```pascal
type
  TFormChat = class(TForm)
    PageControl: TPageControl;
    TabRooms: TTabSheet;
    TabPrivate: TTabSheet;
    ListBoxRooms: TListBox;
    ListBoxUsers: TListBox;
    MemoChat: TMemo;
    EditMessage: TEdit;
    BtnSend: TButton;
    BtnJoinRoom: TButton;
    BtnCreateRoom: TButton;
    BtnSendFile: TButton;
    // ... autres composants ...

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnJoinRoomClick(Sender: TObject);
    procedure BtnCreateRoomClick(Sender: TObject);
    procedure BtnSendFileClick(Sender: TObject);
    procedure ListBoxUsersDoubleClick(Sender: TObject);
    // ... autres gestionnaires d'événements ...
  private
    FChatClient: TChatClient;
    FPrivateTabs: TDictionary<string, TTabSheet>;

    procedure OnChatMessage(Sender: TObject; const Username, Message: string);
    procedure OnPrivateMessage(Sender: TObject; const Username, Message: string);
    procedure OnSystemMessage(Sender: TObject; const Username, Message: string);
    procedure OnUserListUpdate(Sender: TObject);
    procedure OnRoomListUpdate(Sender: TObject);

    procedure OpenPrivateChat(const Username: string);
  end;

procedure TFormChat.OnChatMessage(Sender: TObject; const Username, Message: string);
begin
  // Afficher le message dans le salon courant
  MemoChat.Lines.Add(Format('[%s] %s', [Username, Message]));

  // Faire défiler vers le bas
  SendMessage(MemoChat.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFormChat.OpenPrivateChat(const Username: string);
var
  Tab: TTabSheet;
  Memo: TMemo;
  Edit: TEdit;
  BtnSend: TButton;
  Panel: TPanel;
begin
  // Vérifier si l'onglet existe déjà
  if not FPrivateTabs.TryGetValue(Username, Tab) then
  begin
    // Créer un nouvel onglet
    Tab := TTabSheet.Create(PageControl);
    Tab.PageControl := PageControl;
    Tab.Caption := Username;

    // Créer les composants
    Memo := TMemo.Create(Tab);
    Memo.Parent := Tab;
    Memo.Align := alClient;
    Memo.ReadOnly := True;
    Memo.ScrollBars := ssVertical;

    Panel := TPanel.Create(Tab);
    Panel.Parent := Tab;
    Panel.Align := alBottom;
    Panel.Height := 40;

    Edit := TEdit.Create(Panel);
    Edit.Parent := Panel;
    Edit.Align := alClient;
    Edit.AlignWithMargins := True;

    BtnSend := TButton.Create(Panel);
    BtnSend.Parent := Panel;
    BtnSend.Align := alRight;
    BtnSend.Caption := 'Envoyer';
    BtnSend.Width := 80;

    // Configurer les tags pour identifier les contrôles
    Memo.Tag := Integer(Tab);
    Edit.Tag := Integer(Tab);
    BtnSend.Tag := Integer(Tab);

    // Configurer les événements
    BtnSend.OnClick := PrivateSendClick;
    Edit.OnKeyPress := PrivateEditKeyPress;

    // Stocker l'onglet
    FPrivateTabs.Add(Username, Tab);
  end;

  // Sélectionner l'onglet
  PageControl.ActivePage := Tab;
end;
```

## Sécurité dans l'architecture client-serveur

### Authentification

L'authentification vérifie l'identité des utilisateurs :

```pascal
function TAuthServer.AuthenticateUser(const AUsername, APassword: string): Boolean;
var
  HashedPassword, StoredHash: string;
begin
  // Hacher le mot de passe
  HashedPassword := HashPassword(APassword);

  // Récupérer le hash stocké
  StoredHash := GetStoredPasswordHash(AUsername);

  // Comparer les hash
  Result := (StoredHash <> '') and (HashedPassword = StoredHash);
end;

function HashPassword(const APassword: string): string;
var
  SHA256: THashSHA2;
begin
  SHA256 := THashSHA2.Create;
  try
    Result := SHA256.GetHashString(APassword);
  finally
    SHA256.Free;
  end;
end;
```

### Chiffrement des communications

Le chiffrement protège les données en transit :

```pascal
procedure ConfigureSSL(Socket: TIdTCPClient);
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Socket);

  // Configurer SSL/TLS
  IOHandler.SSLOptions.Method := sslvTLSv1_2;
  IOHandler.SSLOptions.Mode := sslmClient;

  // Assigner le gestionnaire
  Socket.IOHandler := IOHandler;
end;
```

### Protection contre les attaques

Différentes mesures peuvent être prises pour se protéger contre les attaques courantes :

```pascal
procedure TSecureServer.ValidateInput(const AInput: string);
begin
  // Vérifier la longueur
  if Length(AInput) > MAX_INPUT_LENGTH then
    raise EInputValidationException.Create('Entrée trop longue');

  // Vérifier les caractères dangereux
  if ContainsInvalidChars(AInput) then
    raise EInputValidationException.Create('Caractères non autorisés');

  // Vérifier les motifs d'injection
  if ContainsInjectionPattern(AInput) then
    raise EInputValidationException.Create('Motif d'injection détecté');
end;

procedure TSecureServer.ProtectAgainstDoS;
var
  CurrentTime: TDateTime;
  IPAddress: string;
begin
  CurrentTime := Now;
  IPAddress := Context.Connection.Socket.Binding.PeerIP;

  // Vérifier si l'IP a dépassé le nombre de requêtes autorisées
  if IsRateLimited(IPAddress, CurrentTime) then
  begin
    // Journaliser la tentative
    LogRateLimitExceeded(IPAddress);

    // Rejeter la connexion
    Context.Connection.Disconnect;
    Exit;
  end;

  // Enregistrer cette requête
  RecordRequest(IPAddress, CurrentTime);
end;
```

## Déploiement et configuration

### Déploiement d'une application client-serveur

Le déploiement implique la mise en place du serveur et la distribution des clients :

```pascal
procedure TDeploymentManager.DeployServer(const AConfig: TServerConfig);
begin
  // Créer la structure de répertoires
  ForceDirectories(AConfig.DataDirectory);
  ForceDirectories(AConfig.LogDirectory);

  // Copier les fichiers du serveur
  CopyServerFiles(AConfig.SourceDirectory, AConfig.TargetDirectory);

  // Créer le fichier de configuration
  CreateConfigFile(AConfig);

  // Installer le service Windows (si applicable)
  if AConfig.InstallAsService then
    InstallWindowsService(AConfig.ServiceName, AConfig.TargetDirectory);

  // Démarrer le serveur
  if AConfig.StartAfterDeploy then
    StartServer(AConfig);
end;

procedure TDeploymentManager.DeployClient(const AConfig: TClientConfig);
begin
  // Préparer l'installateur du client
  PrepareClientInstaller(AConfig);

  // Publier l'installateur
  PublishClientInstaller(AConfig.InstallerPath, AConfig.PublishLocation);

  // Notifier les utilisateurs (si applicable)
  if AConfig.NotifyUsers then
    SendUpdateNotification(AConfig.UserList, AConfig.UpdateMessage);
end;
```

### Configuration centralisée

Pour faciliter la gestion, une configuration centralisée peut être mise en place :

```pascal
type
  TConfigManager = class
  private
    FConfigServer: string;
    FConfigPort: Integer;
    FLocalCache: TDictionary<string, string>;

    function FetchConfigFromServer(const AKey: string): string;
    procedure UpdateLocalCache(const AKey, AValue: string);
  public
    constructor Create(const AServer: string; APort: Integer);
    destructor Destroy; override;

    function GetConfig(const AKey: string; const ADefaultValue: string = ''): string;
    procedure SetConfig(const AKey, AValue: string);
    procedure RefreshAllConfigs;
  end;

function TConfigManager.GetConfig(const AKey: string; const ADefaultValue: string = ''): string;
begin
  // Vérifier d'abord le cache local
  if FLocalCache.TryGetValue(AKey, Result) then
    Exit;

  // Essayer de récupérer du serveur
  try
    Result := FetchConfigFromServer(AKey);
    UpdateLocalCache(AKey, Result);
  except
    // En cas d'erreur, utiliser la valeur par défaut
    Result := ADefaultValue;
  end;
end;
```

## Surveillance et maintenance

### Journalisation

Une bonne journalisation est essentielle pour le débogage et la surveillance :

```pascal
type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogger = class
  private
    FLogFile: string;
    FMinLevel: TLogLevel;
    FLock: TCriticalSection;
  public
    constructor Create(const ALogFile: string; AMinLevel: TLogLevel = llInfo);
    destructor Destroy; override;

    procedure Log(ALevel: TLogLevel; const AMessage: string); overload;
    procedure Log(ALevel: TLogLevel; const AFormat: string; const AArgs: array of const); overload;

    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Critical(const AMessage: string);
  end;

procedure TLogger.Log(ALevel: TLogLevel; const AMessage: string);
var
  LogMsg, LevelStr: string;
begin
  // Si le niveau est inférieur au minimum, ne rien faire
  if ALevel < FMinLevel then
    Exit;

  // Convertir le niveau en chaîne
  case ALevel of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
    llCritical: LevelStr := 'CRITICAL';
  end;

  // Formater le message
  LogMsg := Format('[%s] [%s] %s',
                  [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
                   LevelStr,
                   AMessage]);

  // Écrire dans le fichier journal
  FLock.Acquire;
  try
    with TStreamWriter.Create(FLogFile, True, TEncoding.UTF8) do
    try
      WriteLine(LogMsg);
    finally
      Free;
    end;
  finally
    FLock.Release;
  end;
end;
```

### Surveillance des performances

Pour garantir les performances, une surveillance est nécessaire :

```pascal
type
  TPerformanceMonitor = class
  private
    FMetrics: TDictionary<string, TMetric>;
    FReportInterval: Integer;
    FTimer: TTimer;

    procedure OnTimer(Sender: TObject);
    procedure ReportMetrics;
  public
    constructor Create(AReportInterval: Integer = 60000);  // 1 minute par défaut
    destructor Destroy; override;

    procedure RecordRequestTime(const AEndpoint: string; AMilliseconds: Integer);
    procedure IncrementCounter(const AMetric: string; AIncrement: Integer = 1);
    procedure RecordValue(const AMetric: string; AValue: Double);

    function GetAverageRequestTime(const AEndpoint: string): Double;
    function GetTotalRequests(const AEndpoint: string): Integer;
  end;

procedure TPerformanceMonitor.RecordRequestTime(const AEndpoint: string; AMilliseconds: Integer);
var
  Metric: TMetric;
  MetricName: string;
begin
  MetricName := 'request_time.' + AEndpoint;

  if not FMetrics.TryGetValue(MetricName, Metric) then
  begin
    Metric := TMetric.Create(MetricName, mtAverage);
    FMetrics.Add(MetricName, Metric);
  end;

  Metric.AddValue(AMilliseconds);
end;
```

## Défis de l'architecture client-serveur

### Évolutivité (Scalability)

Gérer un nombre croissant d'utilisateurs ou de données :

```pascal
type
  TScalableServer = class
  private
    FWorkerPool: TThreadPool;
    FLoadBalancer: TLoadBalancer;
    FConnectionQueue: TThreadedQueue<TConnection>;

    procedure ProcessConnections;
    procedure HandleConnection(AConnection: TConnection);
  public
    constructor Create(AWorkerCount: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    procedure AddBackendServer(const AServer: TServerInfo);
    procedure RemoveBackendServer(const AServer: TServerInfo);
  end;

constructor TScalableServer.Create(AWorkerCount: Integer);
begin
  inherited Create;

  // Créer le pool de workers
  FWorkerPool := TThreadPool.Create(AWorkerCount);

  // Créer l'équilibreur de charge
  FLoadBalancer := TLoadBalancer.Create;

  // Créer la file d'attente de connexions
  FConnectionQueue := TThreadedQueue<TConnection>.Create(1000, 0);

  // Démarrer le thread de traitement
  TThread.CreateAnonymousThread(ProcessConnections).Start;
end;
```

### Disponibilité (Availability)

Maintenir le service même en cas de panne :

```pascal
type
  THighAvailabilityManager = class
  private
    FServers: TList<TServerInstance>;
    FHeartbeatTimer: TTimer;

    procedure CheckServerHealth(Sender: TObject);
    procedure ActivateBackupServer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddServer(const AServer: TServerInstance);
    procedure RemoveServer(const AServer: TServerInstance);

    function GetActiveServer: TServerInstance;
  end;

procedure THighAvailabilityManager.CheckServerHealth(Sender: TObject);
var
  ActiveServer: TServerInstance;
  i: Integer;
begin
  ActiveServer := GetActiveServer;

  // Vérifier l'état du serveur actif
  if not ActiveServer.IsHealthy then
  begin
    // Journaliser la panne
    Log(llWarning, 'Serveur actif en panne: %s', [ActiveServer.Name]);

    // Désactiver le serveur
    ActiveServer.Active := False;

    // Activer un serveur de secours
    ActivateBackupServer;
  end;

  // Vérifier l'état des serveurs de secours
  for i := 0 to FServers.Count - 1 do
  begin
    if (not FServers[i].Active) and FServers[i].IsReady then
      Log(llInfo, 'Serveur de secours prêt: %s', [FServers[i].Name]);
  end;
end;
```


### Résilience et reprise après sinistre

Se remettre des pannes et des erreurs :

```pascal
type
  TDisasterRecoveryManager = class
  private
    FDataBackupSchedule: TScheduledTask;
    FConfigBackupSchedule: TScheduledTask;
    FLastBackupTime: TDateTime;
    FBackupLocation: string;

    procedure PerformDataBackup;
    procedure PerformConfigBackup;
    procedure TestRecovery;
  public
    constructor Create(const ABackupLocation: string);
    destructor Destroy; override;

    procedure ScheduleBackups;
    function Restore(const ABackupFile: string): Boolean;
    procedure PerformEmergencyRecovery;

    property LastBackupTime: TDateTime read FLastBackupTime;
  end;

procedure TDisasterRecoveryManager.PerformDataBackup;
var
  BackupFile: string;
  ZipFile: TZipFile;
begin
  // Générer un nom de fichier de sauvegarde
  BackupFile := Format('%s\data_backup_%s.zip',
                      [FBackupLocation,
                       FormatDateTime('yyyymmdd_hhnnss', Now)]);

  try
    // Créer un fichier ZIP
    ZipFile := TZipFile.Create;
    try
      // Ajouter les fichiers de données
      ZipFile.Add(GetDatabasePath, 'database.db');
      ZipFile.Add(GetUserDataPath, 'userdata.json');

      // Ajouter les journaux
      ZipFile.Add(GetLogFilePath, 'application.log');

      // Enregistrer le fichier ZIP
      ZipFile.SaveToFile(BackupFile);

      // Mettre à jour l'heure de la dernière sauvegarde
      FLastBackupTime := Now;

      // Journaliser le succès
      Log(llInfo, 'Sauvegarde des données réussie: %s', [BackupFile]);
    finally
      ZipFile.Free;
    end;

    // Nettoyer les anciennes sauvegardes
    CleanupOldBackups(FBackupLocation, 'data_backup_*.zip', 5);
  except
    on E: Exception do
      Log(llError, 'Erreur lors de la sauvegarde des données: %s', [E.Message]);
  end;
end;

function TDisasterRecoveryManager.Restore(const ABackupFile: string): Boolean;
var
  ZipFile: TZipFile;
  TempDir: string;
begin
  Result := False;

  // Vérifier si le fichier existe
  if not FileExists(ABackupFile) then
  begin
    Log(llError, 'Fichier de sauvegarde introuvable: %s', [ABackupFile]);
    Exit;
  end;

  // Créer un répertoire temporaire
  TempDir := GetTemporaryDirectory;
  try
    // Extraire les fichiers
    ZipFile := TZipFile.Create;
    try
      ZipFile.ExtractAll(ABackupFile, TempDir);
    finally
      ZipFile.Free;
    end;

    // Arrêter les services
    StopServices;

    try
      // Restaurer la base de données
      if FileExists(TempDir + '\database.db') then
        CopyFile(PChar(TempDir + '\database.db'), PChar(GetDatabasePath), False);

      // Restaurer les données utilisateur
      if FileExists(TempDir + '\userdata.json') then
        CopyFile(PChar(TempDir + '\userdata.json'), PChar(GetUserDataPath), False);

      // Journaliser la restauration
      Log(llInfo, 'Restauration réussie depuis: %s', [ABackupFile]);

      Result := True;
    finally
      // Redémarrer les services
      StartServices;
    end;
  finally
    // Nettoyer le répertoire temporaire
    DeleteDirectory(TempDir);
  end;
end;
```

### Partitionnement et sharding des données

Pour gérer de grandes quantités de données :

```pascal
type
  TShardManager = class
  private
    FShards: TDictionary<Integer, TDatabaseConnection>;
    FShardCount: Integer;

    function GetShardId(const AKey: string): Integer;
    function GetConnection(AShardId: Integer): TDatabaseConnection;
  public
    constructor Create(AShardCount: Integer);
    destructor Destroy; override;

    procedure AddShard(AShardId: Integer; const AConnectionString: string);
    function StoreData(const AKey, AValue: string): Boolean;
    function RetrieveData(const AKey: string; out AValue: string): Boolean;
    procedure Rebalance;
  end;

function TShardManager.GetShardId(const AKey: string): Integer;
var
  Hash: Cardinal;
begin
  // Calculer un hachage de la clé
  Hash := CalculateHash(AKey);

  // Mapper le hachage à un ID de shard
  Result := Hash mod FShardCount;
end;

function TShardManager.StoreData(const AKey, AValue: string): Boolean;
var
  ShardId: Integer;
  Connection: TDatabaseConnection;
begin
  Result := False;

  // Déterminer le shard approprié
  ShardId := GetShardId(AKey);

  // Obtenir la connexion
  Connection := GetConnection(ShardId);
  if not Assigned(Connection) then
  begin
    Log(llError, 'Shard non disponible: %d', [ShardId]);
    Exit;
  end;

  try
    // Stocker les données dans le shard
    Result := Connection.ExecuteQuery(
      'INSERT OR REPLACE INTO data_store (key, value) VALUES (?, ?)',
      [AKey, AValue]);
  except
    on E: Exception do
    begin
      Log(llError, 'Erreur lors du stockage des données dans le shard %d: %s',
          [ShardId, E.Message]);
    end;
  end;
end;
```

## Modèles avancés d'architecture client-serveur

### Microservices

L'architecture de microservices décompose une application en services spécialisés :

```pascal
type
  TServiceRegistry = class
  private
    FServices: TDictionary<string, TServiceInfo>;
    FLock: TCriticalSection;

    procedure CleanupExpiredEntries;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterService(const AServiceName, AEndpoint: string; TTL: Integer);
    function DiscoverService(const AServiceName: string): TServiceInfo;
    procedure Heartbeat(const AServiceName, AEndpoint: string);
  end;

procedure TServiceRegistry.RegisterService(const AServiceName, AEndpoint: string; TTL: Integer);
var
  ServiceInfo: TServiceInfo;
begin
  // Créer l'information de service
  ServiceInfo.Name := AServiceName;
  ServiceInfo.Endpoint := AEndpoint;
  ServiceInfo.RegisterTime := Now;
  ServiceInfo.ExpiryTime := Now + EncodeTime(0, 0, TTL, 0);
  ServiceInfo.Status := ssActive;

  // Enregistrer le service
  FLock.Acquire;
  try
    FServices.AddOrSetValue(AServiceName + '|' + AEndpoint, ServiceInfo);
  finally
    FLock.Release;
  end;

  // Journaliser l'enregistrement
  Log(llInfo, 'Service enregistré: %s à %s (TTL: %d secondes)',
      [AServiceName, AEndpoint, TTL]);
end;

function TServiceRegistry.DiscoverService(const AServiceName: string): TServiceInfo;
var
  Candidates: TList<TServiceInfo>;
  KeyPrefix: string;
  Pair: TPair<string, TServiceInfo>;
begin
  // Nettoyer les entrées expirées
  CleanupExpiredEntries;

  // Préfixe pour trouver tous les endpoints de ce service
  KeyPrefix := AServiceName + '|';

  // Créer une liste de candidats
  Candidates := TList<TServiceInfo>.Create;
  try
    // Remplir la liste des candidats
    FLock.Acquire;
    try
      for Pair in FServices do
      begin
        if Pair.Key.StartsWith(KeyPrefix) and (Pair.Value.Status = ssActive) then
          Candidates.Add(Pair.Value);
      end;
    finally
      FLock.Release;
    end;

    // Si aucun candidat, lever une exception
    if Candidates.Count = 0 then
      raise EServiceNotFoundException.CreateFmt('Service non trouvé: %s', [AServiceName]);

    // Choisir un candidat aléatoirement pour l'équilibrage de charge simple
    Result := Candidates[Random(Candidates.Count)];
  finally
    Candidates.Free;
  end;
end;
```

### API Gateway

Un point d'entrée unique pour différents services :

```pascal
type
  TAPIGateway = class
  private
    FServer: TIdHTTPServer;
    FServiceRegistry: TServiceRegistry;
    FRouteTable: TDictionary<string, TRouteInfo>;

    procedure OnCommandGet(AContext: TIdContext;
                           ARequestInfo: TIdHTTPRequestInfo;
                           AResponseInfo: TIdHTTPResponseInfo);
    procedure OnCommandPost(AContext: TIdContext;
                           ARequestInfo: TIdHTTPRequestInfo;
                           AResponseInfo: TIdHTTPResponseInfo);
    function RouteRequest(const ARoute: string;
                         ARequestInfo: TIdHTTPRequestInfo): TMemoryStream;
  public
    constructor Create(AServiceRegistry: TServiceRegistry);
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;

    procedure AddRoute(const ARoute, AServiceName, APath: string);
    procedure RemoveRoute(const ARoute: string);
  end;

function TAPIGateway.RouteRequest(const ARoute: string;
                                 ARequestInfo: TIdHTTPRequestInfo): TMemoryStream;
var
  RouteInfo: TRouteInfo;
  ServiceInfo: TServiceInfo;
  Client: TIdHTTP;
  Response: TMemoryStream;
  URL: string;
begin
  // Initialiser le flux de sortie
  Result := TMemoryStream.Create;

  // Vérifier si la route existe
  if not FRouteTable.TryGetValue(ARoute, RouteInfo) then
  begin
    // Route non trouvée
    Log(llWarning, 'Route non trouvée: %s', [ARoute]);
    Exit;
  end;

  try
    // Découvrir le service
    ServiceInfo := FServiceRegistry.DiscoverService(RouteInfo.ServiceName);

    // Construire l'URL complète
    URL := ServiceInfo.Endpoint + RouteInfo.Path;

    // Créer un client HTTP
    Client := TIdHTTP.Create(nil);
    try
      // Copier les en-têtes de la requête
      CopyRequestHeaders(ARequestInfo, Client);

      // Effectuer la requête au service
      try
        case ARequestInfo.CommandType of
          hcGET:
            Client.Get(URL, Result);
          hcPOST:
            Client.Post(URL, ARequestInfo.PostStream, Result);
          // Autres méthodes HTTP...
        end;
      except
        on E: EIdHTTPProtocolException do
        begin
          // Gérer les erreurs HTTP
          Log(llError, 'Erreur HTTP %d lors de l''appel à %s: %s',
              [E.ErrorCode, URL, E.Message]);

          // Le service a renvoyé une erreur, la transmettre
          Result.Clear;
          Result.Write(E.ErrorMessage[1], Length(E.ErrorMessage));
          Result.Position := 0;
        end;
        on E: Exception do
        begin
          // Autres erreurs
          Log(llError, 'Erreur lors de l''appel à %s: %s',
              [URL, E.Message]);

          // Créer une réponse d'erreur générique
          Result.Clear;
          WriteErrorToStream(Result, 'Service Unavailable', 503);
        end;
      end;
    finally
      Client.Free;
    end;
  except
    on E: Exception do
    begin
      // Erreur générale
      Log(llError, 'Erreur lors du routage de %s: %s',
          [ARoute, E.Message]);

      // Créer une réponse d'erreur
      Result.Clear;
      WriteErrorToStream(Result, 'Internal Server Error', 500);
    end;
  end;

  // Réinitialiser la position du flux pour la lecture
  Result.Position := 0;
end;
```

### Circuit Breaker

Un patron de conception pour gérer les défaillances de services :

```pascal
type
  TCircuitState = (csOpen, csHalfOpen, csClosed);

  TCircuitBreaker = class
  private
    FServiceName: string;
    FFailureThreshold: Integer;
    FResetTimeout: Integer;
    FFailureCount: Integer;
    FLastFailureTime: TDateTime;
    FState: TCircuitState;
    FLock: TCriticalSection;

    procedure Trip;
    procedure Reset;
    procedure RecordSuccess;
    procedure RecordFailure;
    function ShouldAllowExecution: Boolean;
  public
    constructor Create(const AServiceName: string;
                      AFailureThreshold: Integer = 5;
                      AResetTimeout: Integer = 60);
    destructor Destroy; override;

    function Execute<T>(const AOperation: TFunc<T>): T;
    procedure ExecuteVoid(const AOperation: TProc);

    property State: TCircuitState read FState;
  end;

function TCircuitBreaker.Execute<T>(const AOperation: TFunc<T>): T;
begin
  FLock.Acquire;
  try
    // Vérifier si l'exécution est autorisée
    if not ShouldAllowExecution then
      raise ECircuitOpenException.CreateFmt('Circuit ouvert pour %s', [FServiceName]);
  finally
    FLock.Release;
  end;

  try
    // Exécuter l'opération
    Result := AOperation();

    // Enregistrer le succès
    RecordSuccess;
  except
    // Enregistrer l'échec
    RecordFailure;

    // Relancer l'exception
    raise;
  end;
end;

function TCircuitBreaker.ShouldAllowExecution: Boolean;
begin
  case FState of
    csClosed:
      // Circuit fermé, autoriser l'exécution
      Result := True;

    csOpen:
      // Circuit ouvert, vérifier le délai d'attente
      if SecondsBetween(Now, FLastFailureTime) > FResetTimeout then
      begin
        // Basculer en état semi-ouvert
        FState := csHalfOpen;
        Result := True;
      end
      else
        Result := False;

    csHalfOpen:
      // Circuit semi-ouvert, autoriser une seule tentative
      Result := True;
  end;
end;

procedure TCircuitBreaker.RecordSuccess;
begin
  FLock.Acquire;
  try
    // Si semi-ouvert, fermer le circuit
    if FState = csHalfOpen then
    begin
      Reset;
      Log(llInfo, 'Circuit fermé pour %s après récupération', [FServiceName]);
    end;

    // Réinitialiser le compteur d'échecs
    FFailureCount := 0;
  finally
    FLock.Release;
  end;
end;

procedure TCircuitBreaker.RecordFailure;
begin
  FLock.Acquire;
  try
    // Incrémenter le compteur d'échecs
    Inc(FFailureCount);
    FLastFailureTime := Now;

    // Si semi-ouvert, ouvrir immédiatement
    if FState = csHalfOpen then
    begin
      Trip;
      Log(llWarning, 'Circuit rouvert pour %s après échec de récupération', [FServiceName]);
    end
    // Si fermé et seuil atteint, ouvrir
    else if (FState = csClosed) and (FFailureCount >= FFailureThreshold) then
    begin
      Trip;
      Log(llWarning, 'Circuit ouvert pour %s après %d échecs',
          [FServiceName, FFailureCount]);
    end;
  finally
    FLock.Release;
  end;
end;
```

### Cache distribué

Pour améliorer les performances et réduire la charge serveur :

```pascal
type
  TCacheEntry = record
    Value: string;
    ExpiryTime: TDateTime;
  end;

  TDistributedCache = class
  private
    FLocalCache: TDictionary<string, TCacheEntry>;
    FRemoteCache: TRedisClient;
    FLock: TCriticalSection;

    function IsExpired(const AEntry: TCacheEntry): Boolean;
  public
    constructor Create(const ARedisHost: string; ARedisPort: Integer);
    destructor Destroy; override;

    function Get(const AKey: string; out AValue: string): Boolean;
    procedure Set(const AKey, AValue: string; TTL: Integer = 300);
    procedure Remove(const AKey: string);
    procedure Clear;
  end;

function TDistributedCache.Get(const AKey: string; out AValue: string): Boolean;
var
  Entry: TCacheEntry;
begin
  Result := False;

  // Vérifier d'abord le cache local
  FLock.Acquire;
  try
    if FLocalCache.TryGetValue(AKey, Entry) then
    begin
      // Vérifier si l'entrée est expirée
      if not IsExpired(Entry) then
      begin
        AValue := Entry.Value;
        Result := True;
        Exit;
      end
      else
        // Supprimer l'entrée expirée
        FLocalCache.Remove(AKey);
    end;
  finally
    FLock.Release;
  end;

  // Si non trouvé localement, vérifier le cache distant
  try
    AValue := FRemoteCache.Get(AKey);
    Result := AValue <> '';

    // Si trouvé, stocker dans le cache local
    if Result then
    begin
      // Obtenir le TTL restant
      Entry.Value := AValue;
      Entry.ExpiryTime := Now + EncodeTime(0, 0, FRemoteCache.TTL(AKey), 0);

      FLock.Acquire;
      try
        FLocalCache.AddOrSetValue(AKey, Entry);
      finally
        FLock.Release;
      end;
    end;
  except
    on E: Exception do
      Log(llError, 'Erreur lors de l''accès au cache distant: %s', [E.Message]);
  end;
end;

procedure TDistributedCache.Set(const AKey, AValue: string; TTL: Integer = 300);
var
  Entry: TCacheEntry;
begin
  // Définir dans le cache distant
  try
    FRemoteCache.SetEx(AKey, AValue, TTL);
  except
    on E: Exception do
      Log(llError, 'Erreur lors de la définition dans le cache distant: %s', [E.Message]);
  end;

  // Définir dans le cache local
  Entry.Value := AValue;
  Entry.ExpiryTime := Now + EncodeTime(0, 0, TTL, 0);

  FLock.Acquire;
  try
    FLocalCache.AddOrSetValue(AKey, Entry);
  finally
    FLock.Release;
  end;
end;
```

## Implémentation d'une architecture client-serveur pour une application métier

### Cas d'utilisation : Application de gestion d'inventaire

Une application où les clients peuvent gérer un inventaire partagé :

```pascal
// Définition du modèle de données
type
  TProductCategory = (pcElectronics, pcClothing, pcFood, pcOffice, pcOther);

  TProduct = class
  private
    FID: Integer;
    FName: string;
    FDescription: string;
    FCategory: TProductCategory;
    FQuantity: Integer;
    FUnitPrice: Currency;
    FLastUpdated: TDateTime;
  published
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Category: TProductCategory read FCategory write FCategory;
    property Quantity: Integer read FQuantity write FQuantity;
    property UnitPrice: Currency read FUnitPrice write FUnitPrice;
    property LastUpdated: TDateTime read FLastUpdated write FLastUpdated;
  end;

  TInventoryService = class
  private
    FDatabase: TSQLConnection;
    FProducts: TObjectList<TProduct>;
    FLock: TCriticalSection;
  public
    constructor Create(const AConnectionString: string);
    destructor Destroy; override;

    function GetAllProducts: TObjectList<TProduct>;
    function GetProductByID(AID: Integer): TProduct;
    function AddProduct(AProduct: TProduct): Integer;
    function UpdateProduct(AProduct: TProduct): Boolean;
    function DeleteProduct(AID: Integer): Boolean;
    function SearchProducts(const AQuery: string): TObjectList<TProduct>;
  end;
```

### Implémentation du serveur d'inventaire

```pascal
// Serveur d'inventaire
type
  TInventoryServer = class
  private
    FServer: TIdTCPServer;
    FInventoryService: TInventoryService;

    procedure ProcessRequest(AContext: TIdContext);
    procedure HandleGetAllProducts(AContext: TIdContext);
    procedure HandleGetProduct(AContext: TIdContext; ARequestData: TJSONObject);
    procedure HandleAddProduct(AContext: TIdContext; ARequestData: TJSONObject);
    procedure HandleUpdateProduct(AContext: TIdContext; ARequestData: TJSONObject);
    procedure HandleDeleteProduct(AContext: TIdContext; ARequestData: TJSONObject);
    procedure HandleSearchProducts(AContext: TIdContext; ARequestData: TJSONObject);
  public
    constructor Create(const ADatabaseConnectionString: string);
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;
  end;

procedure TInventoryServer.ProcessRequest(AContext: TIdContext);
var
  RequestStr: string;
  RequestJSON: TJSONObject;
  CommandType: string;
begin
  try
    // Lire la requête
    RequestStr := AContext.Connection.IOHandler.ReadLn;

    // Parser le JSON
    RequestJSON := TJSONObject.ParseJSONValue(RequestStr) as TJSONObject;
    try
      // Extraire le type de commande
      CommandType := RequestJSON.GetValue('command').Value;

      // Traiter selon le type de commande
      if CommandType = 'get_all_products' then
        HandleGetAllProducts(AContext)
      else if CommandType = 'get_product' then
        HandleGetProduct(AContext, RequestJSON)
      else if CommandType = 'add_product' then
        HandleAddProduct(AContext, RequestJSON)
      else if CommandType = 'update_product' then
        HandleUpdateProduct(AContext, RequestJSON)
      else if CommandType = 'delete_product' then
        HandleDeleteProduct(AContext, RequestJSON)
      else if CommandType = 'search_products' then
        HandleSearchProducts(AContext, RequestJSON)
      else
        SendErrorResponse(AContext, 'Commande inconnue: ' + CommandType);
    finally
      RequestJSON.Free;
    end;
  except
    on E: Exception do
      SendErrorResponse(AContext, 'Erreur de traitement: ' + E.Message);
  end;
end;

procedure TInventoryServer.HandleGetAllProducts(AContext: TIdContext);
var
  Products: TObjectList<TProduct>;
  Response: TJSONObject;
  ProductsArray: TJSONArray;
  i: Integer;
begin
  // Récupérer tous les produits
  Products := FInventoryService.GetAllProducts;
  try
    // Créer la réponse JSON
    Response := TJSONObject.Create;
    try
      Response.AddPair('status', 'success');

      // Créer un tableau pour les produits
      ProductsArray := TJSONArray.Create;
      Response.AddPair('products', ProductsArray);

      // Ajouter chaque produit au tableau
      for i := 0 to Products.Count - 1 do
        ProductsArray.AddElement(ProductToJSON(Products[i]));

      // Envoyer la réponse
      AContext.Connection.IOHandler.WriteLn(Response.ToString);
    finally
      Response.Free;
    end;
  finally
    Products.Free;
  end;
end;
```

### Implémentation du client d'inventaire

```pascal
// Client d'inventaire
type
  TInventoryClient = class
  private
    FClient: TIdTCPClient;
    FConnected: Boolean;

    function SendRequest(ARequest: TJSONObject): TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AHost: string; APort: Integer): Boolean;
    procedure Disconnect;

    function GetAllProducts: TObjectList<TProduct>;
    function GetProduct(AID: Integer): TProduct;
    function AddProduct(AProduct: TProduct): Integer;
    function UpdateProduct(AProduct: TProduct): Boolean;
    function DeleteProduct(AID: Integer): Boolean;
    function SearchProducts(const AQuery: string): TObjectList<TProduct>;

    property Connected: Boolean read FConnected;
  end;

function TInventoryClient.GetAllProducts: TObjectList<TProduct>;
var
  Request, Response: TJSONObject;
  ProductsArray: TJSONArray;
  ProductObj: TJSONObject;
  Product: TProduct;
  i: Integer;
begin
  Result := TObjectList<TProduct>.Create(True);

  // Créer la requête
  Request := TJSONObject.Create;
  try
    Request.AddPair('command', 'get_all_products');

    // Envoyer la requête et obtenir la réponse
    Response := SendRequest(Request);
    try
      // Vérifier le statut
      if Response.GetValue('status').Value <> 'success' then
      begin
        // Gérer l'erreur
        if Response.TryGetValue('error', TJSONValue(ProductsArray)) then
          raise Exception.Create(ProductsArray.Value)
        else
          raise Exception.Create('Erreur inconnue');
      end;

      // Extraire le tableau de produits
      ProductsArray := Response.GetValue('products') as TJSONArray;

      // Convertir chaque élément en objet TProduct
      for i := 0 to ProductsArray.Count - 1 do
      begin
        ProductObj := ProductsArray.Items[i] as TJSONObject;
        Product := JSONToProduct(ProductObj);
        Result.Add(Product);
      end;
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;

function TInventoryClient.AddProduct(AProduct: TProduct): Integer;
var
  Request, Response, ProductObj: TJSONObject;
  Error: string;
begin
  Result := -1;

  // Créer la requête
  Request := TJSONObject.Create;
  try
    Request.AddPair('command', 'add_product');

    // Convertir le produit en JSON
    ProductObj := ProductToJSON(AProduct);
    Request.AddPair('product', ProductObj);

    // Envoyer la requête et obtenir la réponse
    Response := SendRequest(Request);
    try
      // Vérifier le statut
      if Response.GetValue('status').Value = 'success' then
        Result := Response.GetValue<Integer>('product_id')
      else
      begin
        // Gérer l'erreur
        if Response.TryGetValue('error', Error) then
          raise Exception.Create(Error)
        else
          raise Exception.Create('Erreur inconnue');
      end;
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;
```

### Interface utilisateur pour le client d'inventaire

```pascal
type
  TFormInventory = class(TForm)
    GridProducts: TStringGrid;
    PanelTop: TPanel;
    EditSearch: TEdit;
    BtnSearch: TButton;
    BtnAdd: TButton;
    BtnEdit: TButton;
    BtnDelete: TButton;
    BtnRefresh: TButton;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure GridProductsSelectCell(Sender: TObject; ACol, ARow: Integer;
                                   var CanSelect: Boolean);
  private
    FClient: TInventoryClient;
    FProducts: TObjectList<TProduct>;

    procedure UpdateGrid;
    function GetSelectedProduct: TProduct;
    procedure UpdateButtons;
  public
    // ...
  end;

procedure TFormInventory.FormCreate(Sender: TObject);
begin
  // Créer le client
  FClient := TInventoryClient.Create;

  // Se connecter au serveur
  if not FClient.Connect('localhost', 8080) then
    ShowMessage('Impossible de se connecter au serveur. ' +
                'Veuillez vérifier que le serveur est en cours d''exécution.');

  // Initialiser la liste des produits
  FProducts := TObjectList<TProduct>.Create(True);

  // Configurer la grille
  GridProducts.Cells[0, 0] := 'ID';
  GridProducts.Cells[1, 0] := 'Nom';
  GridProducts.Cells[2, 0] := 'Catégorie';
  GridProducts.Cells[3, 0] := 'Quantité';
  GridProducts.Cells[4, 0] := 'Prix unitaire';

  // Charger les produits
  BtnRefreshClick(nil);

  // Mettre à jour les boutons
  UpdateButtons;
end;

procedure TFormInventory.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  FClient.Disconnect;
  FClient.Free;
  FProducts.Free;
end;

procedure TFormInventory.UpdateGrid;
var
  i: Integer;
begin
  // Effacer la grille (sauf les en-têtes)
  GridProducts.RowCount := 2;
  GridProducts.Cells[0, 1] := '';
  GridProducts.Cells[1, 1] := '';
  GridProducts.Cells[2, 1] := '';
  GridProducts.Cells[3, 1] := '';
  GridProducts.Cells[4, 1] := '';

  // Remplir avec les données des produits
  if FProducts.Count > 0 then
  begin
    GridProducts.RowCount := FProducts.Count + 1;

    for i := 0 to FProducts.Count - 1 do
    begin
      GridProducts.Cells[0, i + 1] := IntToStr(FProducts[i].ID);
      GridProducts.Cells[1, i + 1] := FProducts[i].Name;
      GridProducts.Cells[2, i + 1] := CategoryToString(FProducts[i].Category);
      GridProducts.Cells[3, i + 1] := IntToStr(FProducts[i].Quantity);
      GridProducts.Cells[4, i + 1] := FormatFloat('#,##0.00 €', FProducts[i].UnitPrice);
    end;
  end;

  // Mettre à jour le statut
  StatusBar.SimpleText := Format('%d produits trouvés', [FProducts.Count]);
end;

function TFormInventory.GetSelectedProduct: TProduct;
var
  SelectedRow: Integer;
begin
  Result := nil;

  // Obtenir la ligne sélectionnée
  SelectedRow := GridProducts.Row;

  // Vérifier si une ligne valide est sélectionnée
  if (SelectedRow > 0) and (SelectedRow <= FProducts.Count) then
    Result := FProducts[SelectedRow - 1];
end;

procedure TFormInventory.UpdateButtons;
var
  HasSelection: Boolean;
begin
  // Déterminer si un produit est sélectionné
  HasSelection := GetSelectedProduct <> nil;

  // Activer/désactiver les boutons en conséquence
  BtnEdit.Enabled := HasSelection;
  BtnDelete.Enabled := HasSelection;
end;

procedure TFormInventory.BtnRefreshClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    // Effacer la liste actuelle
    FProducts.Clear;

    try
      // Récupérer tous les produits
      FProducts := FClient.GetAllProducts;

      // Mettre à jour la grille
      UpdateGrid;
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement des produits: ' + E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  // Mettre à jour les boutons
  UpdateButtons;
end;

procedure TFormInventory.BtnSearchClick(Sender: TObject);
var
  Query: string;
begin
  // Récupérer le texte de recherche
  Query := EditSearch.Text;

  if Query = '' then
  begin
    // Si vide, récupérer tous les produits
    BtnRefreshClick(Sender);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    // Effacer la liste actuelle
    FProducts.Clear;

    try
      // Rechercher les produits
      FProducts := FClient.SearchProducts(Query);

      // Mettre à jour la grille
      UpdateGrid;
    except
      on E: Exception do
        ShowMessage('Erreur lors de la recherche: ' + E.Message);
    end;
  finally
    Screen.Cursor := crDefault;
  end;

  // Mettre à jour les boutons
  UpdateButtons;
end;

procedure TFormInventory.BtnAddClick(Sender: TObject);
var
  Product: TProduct;
  ProductID: Integer;
begin
  // Créer le formulaire d'ajout de produit
  with TFormProductEdit.Create(Self) do
  try
    // Configurer en mode ajout
    Mode := pemAdd;

    // Afficher le formulaire
    if ShowModal = mrOK then
    begin
      // Récupérer les données du formulaire
      Product := TProduct.Create;
      try
        Product.Name := ProductName;
        Product.Description := ProductDescription;
        Product.Category := ProductCategory;
        Product.Quantity := ProductQuantity;
        Product.UnitPrice := ProductPrice;

        try
          // Ajouter le produit
          Screen.Cursor := crHourGlass;
          try
            ProductID := FClient.AddProduct(Product);

            if ProductID > 0 then
            begin
              ShowMessage('Produit ajouté avec succès.');

              // Rafraîchir la liste
              BtnRefreshClick(nil);
            end
            else
              ShowMessage('Erreur lors de l''ajout du produit.');
          finally
            Screen.Cursor := crDefault;
          end;
        except
          on E: Exception do
            ShowMessage('Erreur: ' + E.Message);
        end;
      finally
        Product.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TFormInventory.BtnEditClick(Sender: TObject);
var
  Product: TProduct;
  Success: Boolean;
begin
  // Récupérer le produit sélectionné
  Product := GetSelectedProduct;
  if Product = nil then
    Exit;

  // Créer le formulaire d'édition de produit
  with TFormProductEdit.Create(Self) do
  try
    // Configurer en mode édition
    Mode := pemEdit;

    // Remplir les champs
    ProductID := Product.ID;
    ProductName := Product.Name;
    ProductDescription := Product.Description;
    ProductCategory := Product.Category;
    ProductQuantity := Product.Quantity;
    ProductPrice := Product.UnitPrice;

    // Afficher le formulaire
    if ShowModal = mrOK then
    begin
      // Mettre à jour les données du produit
      Product.Name := ProductName;
      Product.Description := ProductDescription;
      Product.Category := ProductCategory;
      Product.Quantity := ProductQuantity;
      Product.UnitPrice := ProductPrice;

      try
        // Mettre à jour le produit
        Screen.Cursor := crHourGlass;
        try
          Success := FClient.UpdateProduct(Product);

          if Success then
          begin
            ShowMessage('Produit mis à jour avec succès.');

            // Rafraîchir la liste
            BtnRefreshClick(nil);
          end
          else
            ShowMessage('Erreur lors de la mise à jour du produit.');
        finally
          Screen.Cursor := crDefault;
        end;
      except
        on E: Exception do
          ShowMessage('Erreur: ' + E.Message);
      end;
    end;
  finally
    Free;
  end;
end;

procedure TFormInventory.BtnDeleteClick(Sender: TObject);
var
  Product: TProduct;
  Success: Boolean;
begin
  // Récupérer le produit sélectionné
  Product := GetSelectedProduct;
  if Product = nil then
    Exit;

  // Demander confirmation
  if MessageDlg(Format('Êtes-vous sûr de vouloir supprimer le produit "%s" ?',
                      [Product.Name]),
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    // Supprimer le produit
    Screen.Cursor := crHourGlass;
    try
      Success := FClient.DeleteProduct(Product.ID);

      if Success then
      begin
        ShowMessage('Produit supprimé avec succès.');

        // Rafraîchir la liste
        BtnRefreshClick(nil);
      end
      else
        ShowMessage('Erreur lors de la suppression du produit.');
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;

procedure TFormInventory.GridProductsSelectCell(Sender: TObject; ACol, ARow: Integer;
                                              var CanSelect: Boolean);
begin
  // Mettre à jour les boutons lorsque la sélection change
  UpdateButtons;
end;
```

### Formulaire d'édition de produit

```pascal
type
  TProductEditMode = (pemAdd, pemEdit);

  TFormProductEdit = class(TForm)
    LabelName: TLabel;
    EditName: TEdit;
    LabelDescription: TLabel;
    MemoDescription: TMemo;
    LabelCategory: TLabel;
    ComboCategory: TComboBox;
    LabelQuantity: TLabel;
    EditQuantity: TEdit;
    LabelPrice: TLabel;
    EditPrice: TEdit;
    BtnOK: TButton;
    BtnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnOKClick(Sender: TObject);
  private
    FMode: TProductEditMode;
    FProductID: Integer;

    function ValidateInput: Boolean;
  public
    property Mode: TProductEditMode read FMode write FMode;
    property ProductID: Integer read FProductID write FProductID;
    property ProductName: string read GetProductName write SetProductName;
    property ProductDescription: string read GetProductDescription write SetProductDescription;
    property ProductCategory: TProductCategory read GetProductCategory write SetProductCategory;
    property ProductQuantity: Integer read GetProductQuantity write SetProductQuantity;
    property ProductPrice: Currency read GetProductPrice write SetProductPrice;
  end;

procedure TFormProductEdit.FormCreate(Sender: TObject);
var
  Category: TProductCategory;
begin
  // Initialiser les valeurs par défaut
  FMode := pemAdd;
  FProductID := 0;

  // Remplir les catégories
  ComboCategory.Items.Clear;
  for Category := Low(TProductCategory) to High(TProductCategory) do
    ComboCategory.Items.Add(CategoryToString(Category));

  ComboCategory.ItemIndex := 0;
end;

procedure TFormProductEdit.FormShow(Sender: TObject);
begin
  // Configurer le titre selon le mode
  if FMode = pemAdd then
    Caption := 'Ajouter un produit'
  else
    Caption := 'Modifier un produit';

  // Mettre le focus sur le premier champ
  EditName.SetFocus;
end;

function TFormProductEdit.ValidateInput: Boolean;
var
  Quantity: Integer;
  Price: Currency;
begin
  Result := False;

  // Vérifier le nom
  if Trim(EditName.Text) = '' then
  begin
    ShowMessage('Veuillez entrer un nom de produit.');
    EditName.SetFocus;
    Exit;
  end;

  // Vérifier la quantité
  if not TryStrToInt(EditQuantity.Text, Quantity) or (Quantity < 0) then
  begin
    ShowMessage('Veuillez entrer une quantité valide (nombre entier positif).');
    EditQuantity.SetFocus;
    Exit;
  end;

  // Vérifier le prix
  if not TryStrToCurr(EditPrice.Text, Price) or (Price <= 0) then
  begin
    ShowMessage('Veuillez entrer un prix valide (nombre positif).');
    EditPrice.SetFocus;
    Exit;
  end;

  Result := True;
end;

procedure TFormProductEdit.BtnOKClick(Sender: TObject);
begin
  // Valider les entrées
  if not ValidateInput then
    Exit;

  // Fermer avec succès
  ModalResult := mrOK;
end;

function TFormProductEdit.GetProductName: string;
begin
  Result := EditName.Text;
end;

procedure TFormProductEdit.SetProductName(const Value: string);
begin
  EditName.Text := Value;
end;

// Implémentations similaires pour les autres getters/setters...
```

## Optimisation des performances

### Optimisation côté serveur

Pour améliorer les performances du serveur :

```pascal
// Utilisation d'un pool de threads
type
  TThreadPoolServer = class
  private
    FServer: TIdTCPServer;
    FThreadPool: TThreadPool;

    procedure OnConnect(AContext: TIdContext);
    procedure ProcessRequest(AClient: TIdTCPClientCustom);
  public
    constructor Create(AThreadCount: Integer);
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;
  end;

constructor TThreadPoolServer.Create(AThreadCount: Integer);
begin
  inherited Create;

  // Créer le pool de threads
  FThreadPool := TThreadPool.Create(AThreadCount);

  // Créer le serveur
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := OnConnect;
end;

procedure TThreadPoolServer.OnConnect(AContext: TIdContext);
var
  Client: TIdTCPClientCustom;
begin
  // Extraire le client de la connexion
  Client := AContext.Connection;

  // Ajouter la tâche au pool
  FThreadPool.QueueWorkItem(
    procedure
    begin
      ProcessRequest(Client);
    end);
end;

// Mise en cache des résultats fréquemment demandés
type
  TProductCache = class
  private
    FCache: TDictionary<Integer, TProduct>;
    FLastUpdate: TDateTime;
    FUpdateInterval: TDateTime;
    FLock: TCriticalSection;

    procedure UpdateCache;
  public
    constructor Create(AUpdateIntervalSeconds: Integer = 60);
    destructor Destroy; override;

    function GetProduct(AID: Integer): TProduct;
    procedure InvalidateProduct(AID: Integer);
    procedure InvalidateAll;
  end;

function TProductCache.GetProduct(AID: Integer): TProduct;
begin
  Result := nil;

  // Vérifier si une mise à jour est nécessaire
  if Now - FLastUpdate > FUpdateInterval then
    UpdateCache;

  // Chercher dans le cache
  FLock.Acquire;
  try
    if FCache.TryGetValue(AID, Result) then
      Result := Result.Clone;  // Retourner une copie
  finally
    FLock.Release;
  end;

  // Si non trouvé, charger individuellement
  if Result = nil then
  begin
    try
      Result := FInventoryService.GetProductByID(AID);

      // Ajouter au cache
      if Result <> nil then
      begin
        FLock.Acquire;
        try
          FCache.AddOrSetValue(AID, Result.Clone);
        finally
          FLock.Release;
        end;
      end;
    except
      // Ignorer les erreurs
    end;
  end;
end;
```

### Optimisation côté client

Pour améliorer les performances du client :

```pascal
// Chargement asynchrone des données
procedure TFormInventory.LoadProductsAsync;
begin
  // Désactiver les contrôles pendant le chargement
  EnableControls(False);

  // Afficher un indicateur de chargement
  StatusBar.SimpleText := 'Chargement des produits...';

  // Démarrer le chargement dans un thread
  TTask.Run(
    procedure
    var
      Products: TObjectList<TProduct>;
    begin
      try
        // Charger les produits
        Products := FClient.GetAllProducts;

        // Mettre à jour l'interface sur le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            // Remplacer la liste actuelle
            FProducts.Free;
            FProducts := Products;

            // Mettre à jour la grille
            UpdateGrid;

            // Réactiver les contrôles
            EnableControls(True);

            // Mettre à jour le statut
            StatusBar.SimpleText := Format('%d produits chargés', [FProducts.Count]);
          end);
      except
        on E: Exception do
        begin
          // Gérer les erreurs sur le thread principal
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur lors du chargement des produits: ' + E.Message);

              // Réactiver les contrôles
              EnableControls(True);

              // Mettre à jour le statut
              StatusBar.SimpleText := 'Erreur de chargement';
            end);
        end;
      end;
    end);
end;

// Mise en cache des résultats côté client
type
  TClientCache = class
  private
    FCache: TObjectDictionary<string, TObject>;
    FExpiryTimes: TDictionary<string, TDateTime>;
    FLock: TCriticalSection;

    procedure CleanupExpiredItems;
  public
    constructor Create;
    destructor Destroy; override;

    function GetItem<T: class>(const AKey: string): T;
    procedure SetItem<T: class>(const AKey: string; AItem: T; AExpirySeconds: Integer = 300);
    procedure InvalidateItem(const AKey: string);
    procedure Clear;
  end;

function TClientCache.GetItem<T>(const AKey: string): T;
var
  Item: TObject;
begin
  Result := nil;

  // Nettoyer les éléments expirés
  CleanupExpiredItems;

  // Vérifier le cache
  FLock.Acquire;
  try
    if FCache.TryGetValue(AKey, Item) then
      Result := Item as T;
  finally
    FLock.Release;
  end;
end;

procedure TClientCache.SetItem<T>(const AKey: string; AItem: T; AExpirySeconds: Integer = 300);
begin
  FLock.Acquire;
  try
    // Supprimer l'élément existant s'il y en a un
    FCache.Remove(AKey);

    // Ajouter le nouvel élément
    FCache.Add(AKey, AItem);

    // Définir l'heure d'expiration
    FExpiryTimes.AddOrSetValue(AKey, Now + EncodeTime(0, 0, AExpirySeconds, 0));
  finally
    FLock.Release;
  end;
end;
```

## Gestion des erreurs et résilience

### Gestion des erreurs côté serveur

Pour rendre le serveur plus robuste :

```pascal
// Gestionnaire d'erreurs global
procedure TInventoryServer.HandleRequest(AContext: TIdContext);
begin
  try
    // Traiter la requête
    ProcessRequest(AContext);
  except
    on E: EProductNotFoundException do
    begin
      // Produit non trouvé
      SendErrorResponse(AContext, 'Produit non trouvé', 404);
      Log(llWarning, 'Produit non trouvé: %s', [E.Message]);
    end;
    on E: EValidationException do
    begin
      // Erreur de validation
      SendErrorResponse(AContext, 'Erreur de validation: ' + E.Message, 400);
      Log(llWarning, 'Erreur de validation: %s', [E.Message]);
    end;
    on E: EDatabaseException do
    begin
      // Erreur de base de données
      SendErrorResponse(AContext, 'Erreur de base de données', 500);
      Log(llError, 'Erreur de base de données: %s', [E.Message]);
    end;
    on E: Exception do
    begin
      // Autres erreurs
      SendErrorResponse(AContext, 'Erreur interne du serveur', 500);
      Log(llError, 'Erreur non gérée: %s', [E.Message]);
    end;
  end;
end;

// Validation des entrées
procedure TInventoryService.ValidateProduct(AProduct: TProduct);
begin
  if AProduct = nil then
    raise EValidationException.Create('Produit non spécifié');

  if Trim(AProduct.Name) = '' then
    raise EValidationException.Create('Le nom du produit est obligatoire');

  if AProduct.Quantity < 0 then
    raise EValidationException.Create('La quantité doit être positive');

  if AProduct.UnitPrice <= 0 then
    raise EValidationException.Create('Le prix unitaire doit être positif');
end;
```

### Gestion des erreurs côté client

Pour rendre le client plus résilient :

```pascal
// Reconnexion automatique
type
  TResillientClient = class
  private
    FClient: TIdTCPClient;
    FHost: string;
    FPort: Integer;
    FMaxRetries: Integer;
    FRetryDelay: Integer;

    function EnsureConnected: Boolean;
  public
    constructor Create(AMaxRetries: Integer = 3; ARetryDelay: Integer = 1000);
    destructor Destroy; override;

    function SendRequest(ARequest: TJSONObject): TJSONObject;
    procedure SetServer(const AHost: string; APort: Integer);
  end;

function TResillientClient.EnsureConnected: Boolean;
var
  RetryCount: Integer;
begin
  // Si déjà connecté, rien à faire
  if FClient.Connected then
    Exit(True);

  Result := False;
  RetryCount := 0;

  while (RetryCount < FMaxRetries) and (not Result) do
  begin
    try
      // Tenter de se connecter
      FClient.Host := FHost;
      FClient.Port := FPort;
      FClient.Connect;

      Result := True;
    except
      on E: Exception do
      begin
        // Journaliser l'erreur
        Log(llWarning, 'Tentative de connexion %d échouée: %s',
            [RetryCount + 1, E.Message]);

        // Attendre avant de réessayer
        Sleep(FRetryDelay);

        // Incrémenter le compteur
        Inc(RetryCount);
      end;
    end;
  end;

  if not Result then
    Log(llError, 'Impossible de se connecter à %s:%d après %d tentatives',
        [FHost, FPort, FMaxRetries]);
end;

function TResillientClient.SendRequest(ARequest: TJSONObject): TJSONObject;
var
  RequestStr: string;
  ResponseStr: string;
begin
  Result := nil;

  // S'assurer d'être connecté
  if not EnsureConnected then
    raise EConnectionException.Create('Impossible de se connecter au serveur');

  // Convertir la requête en chaîne
  RequestStr := ARequest.ToString;

  try
    // Envoyer la requête
    FClient.IOHandler.WriteLn(RequestStr);

    // Lire la réponse
    ResponseStr := FClient.IOHandler.ReadLn;

    // Parser la réponse
    Result := TJSONObject.ParseJSONValue(ResponseStr) as TJSONObject;
  except
    on E: Exception do
    begin
      // En cas d'erreur, marquer comme déconnecté
      FClient.Disconnect;

      // Relancer l'exception
      raise ETransportException.Create('Erreur de communication: ' + E.Message);
    end;
  end;
end;
```

## Sécurité

### Authentification et autorisation

Pour sécuriser l'accès aux fonctionnalités :

```pascal
// Système d'authentification
type
  TUserRole = (urGuest, urUser, urManager, urAdmin);

  TAuthService = class
  private
    FUsers: TDictionary<string, TUserInfo>;
    FSessions: TDictionary<string, TSessionInfo>;
    FLock: TCriticalSection;

    function GenerateToken: string;
    procedure CleanupExpiredSessions;
  public
    constructor Create;
    destructor Destroy; override;

    function Authenticate(const AUsername, APassword: string): string;
    function ValidateToken(const AToken: string; out AUserInfo: TUserInfo): Boolean;
    procedure Logout(const AToken: string);

    function IsAuthorized(const AUserInfo: TUserInfo; ARequiredRole: TUserRole): Boolean;
  end;

function TAuthService.Authenticate(const AUsername, APassword: string): string;
var
  UserInfo: TUserInfo;
  HashedPassword: string;
  SessionInfo: TSessionInfo;
begin
  Result := '';

  // Nettoyer les sessions expirées
  CleanupExpiredSessions;

  // Vérifier si l'utilisateur existe
  FLock.Acquire;
  try
    if not FUsers.TryGetValue(AUsername, UserInfo) then
      Exit;
  finally
    FLock.Release;
  end;

  // Vérifier le mot de passe
  HashedPassword := HashPassword(APassword, UserInfo.Salt);
  if HashedPassword <> UserInfo.PasswordHash then
    Exit;

  // Créer une nouvelle session
  Result := GenerateToken;

  SessionInfo.Token := Result;
  SessionInfo.Username := AUsername;
  SessionInfo.ExpiryTime := Now + EncodeTime(1, 0, 0, 0);  // Expire dans 1 heure

  // Enregistrer la session
  FLock.Acquire;
  try
    FSessions.AddOrSetValue(Result, SessionInfo);
  finally
    FLock.Release;
  end;

  // Journaliser la connexion
  Log(llInfo, 'Utilisateur connecté: %s', [AUsername]);
end;

function TAuthService.IsAuthorized(const AUserInfo: TUserInfo; ARequiredRole: TUserRole): Boolean;
begin
  // Vérifier si l'utilisateur a un rôle suffisant
  Result := Integer(AUserInfo.Role) >= Integer(ARequiredRole);
end;
```

### Protection contre les attaques courantes

Pour renforcer la sécurité :

```pascal
// Protection contre les injections
function SanitizeInput(const AInput: string): string;
begin
  // Enlever les caractères potentiellement dangereux
  Result := StringReplace(AInput, ';', '', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '', [rfReplaceAll]);
  Result := StringReplace(Result, '--', '', [rfReplaceAll]);
end;

// Protection contre les attaques par force brute
type
  TBruteForceProtection = class
  private
    FAttempts: TDictionary<string, TList<TDateTime>>;
    FMaxAttempts: Integer;
    FLockoutDuration: TDateTime;
    FLock: TCriticalSection;

    procedure CleanupOldAttempts;
  public
    constructor Create(AMaxAttempts: Integer = 5; ALockoutMinutes: Integer = 15);
    destructor Destroy; override;

    function IsLocked(const AUsername: string): Boolean;
    procedure RecordAttempt(const AUsername: string);
    procedure ResetAttempts(const AUsername: string);
  end;

function TBruteForceProtection.IsLocked(const AUsername: string): Boolean;
var
  Attempts: TList<TDateTime>;
  AttemptCount: Integer;
  OldestValidAttempt: TDateTime;
begin
  Result := False;

  // Nettoyer les anciennes tentatives
  CleanupOldAttempts;

  // Vérifier si l'utilisateur est dans la liste
  FLock.Acquire;
  try
    if not FAttempts.TryGetValue(AUsername, Attempts) then
      Exit;

    // Compter les tentatives récentes
    AttemptCount := 0;
    OldestValidAttempt := Now - FLockoutDuration;

    for var AttemptTime in Attempts do
    begin
      if AttemptTime > OldestValidAttempt then
        Inc(AttemptCount);
    end;

    // Vérifier si le nombre de tentatives dépasse le maximum
    Result := AttemptCount >= FMaxAttempts;

    if Result then
      Log(llWarning, 'Compte verrouillé pour %d minutes: %s',
          [MinutesBetween(0, FLockoutDuration), AUsername]);
  finally
    FLock.Release;
  end;
end;

procedure TBruteForceProtection.RecordAttempt(const AUsername: string);
var
  Attempts: TList<TDateTime>;
begin
  FLock.Acquire;
  try
    // Obtenir ou créer la liste des tentatives
    if not FAttempts.TryGetValue(AUsername, Attempts) then
    begin
      Attempts := TList<TDateTime>.Create;
      FAttempts.Add(AUsername, Attempts);
    end;

    // Ajouter la tentative actuelle
    Attempts.Add(Now);

    Log(llInfo, 'Tentative d''authentification échouée pour: %s', [AUsername]);
  finally
    FLock.Release;
  end;
end;

// Protection contre CSRF (Cross-Site Request Forgery)
type
  TCSRFProtection = class
  private
    FTokens: TDictionary<string, TDateTime>;
    FTokenValidity: TDateTime;
    FLock: TCriticalSection;

    procedure CleanupExpiredTokens;
  public
    constructor Create(ATokenValidityMinutes: Integer = 30);
    destructor Destroy; override;

    function GenerateToken: string;
    function ValidateToken(const AToken: string): Boolean;
  end;

function TCSRFProtection.GenerateToken: string;
begin
  // Nettoyer les jetons expirés
  CleanupExpiredTokens;

  // Générer un jeton aléatoire
  Result := CreateGUID.ToString;

  // Enregistrer le jeton
  FLock.Acquire;
  try
    FTokens.Add(Result, Now + FTokenValidity);
  finally
    FLock.Release;
  end;
end;

function TCSRFProtection.ValidateToken(const AToken: string): Boolean;
var
  ExpiryTime: TDateTime;
begin
  Result := False;

  // Vérifier si le jeton existe
  FLock.Acquire;
  try
    if not FTokens.TryGetValue(AToken, ExpiryTime) then
      Exit;

    // Vérifier si le jeton est valide
    Result := Now < ExpiryTime;

    // Supprimer le jeton pour qu'il ne puisse pas être réutilisé
    FTokens.Remove(AToken);
  finally
    FLock.Release;
  end;
end;
```

### Chiffrement des communications

Pour sécuriser les échanges :

```pascal
// Configuration TLS
procedure ConfigureServerTLS(Server: TIdTCPServer);
var
  IOHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  IOHandler := TIdServerIOHandlerSSLOpenSSL.Create(Server);

  // Configurer les fichiers de certificat et clé
  IOHandler.SSLOptions.CertFile := 'server.crt';
  IOHandler.SSLOptions.KeyFile := 'server.key';

  // Configurer la version de TLS
  IOHandler.SSLOptions.Method := sslvTLSv1_2;

  // Assigner le gestionnaire au serveur
  Server.IOHandler := IOHandler;
end;

procedure ConfigureClientTLS(Client: TIdTCPClient);
var
  IOHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  IOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Client);

  // Configurer la version de TLS
  IOHandler.SSLOptions.Method := sslvTLSv1_2;

  // Valider le certificat du serveur
  IOHandler.SSLOptions.VerifyMode := [];
  IOHandler.SSLOptions.VerifyDepth := 9;

  // Si vous avez un certificat client
  // IOHandler.SSLOptions.CertFile := 'client.crt';
  // IOHandler.SSLOptions.KeyFile := 'client.key';

  // Assigner le gestionnaire au client
  Client.IOHandler := IOHandler;
end;

// Chiffrement des données sensibles
function EncryptData(const AData, AKey: string): string;
var
  Cipher: TCipher_AES;
  Key: TBytes;
begin
  // Dériver une clé à partir du mot de passe
  Key := PBKDF2(AKey, 'salt', 1000, 32);

  // Créer le chiffreur
  Cipher := TCipher_AES.Create;
  try
    // Configurer le chiffreur
    Cipher.Mode := cmCBC;
    Cipher.Key := Key;

    // Chiffrer les données
    Result := Cipher.EncodeString(AData, TEncoding.UTF8);
  finally
    Cipher.Free;
  end;
end;

function DecryptData(const AEncryptedData, AKey: string): string;
var
  Cipher: TCipher_AES;
  Key: TBytes;
begin
  // Dériver une clé à partir du mot de passe
  Key := PBKDF2(AKey, 'salt', 1000, 32);

  // Créer le chiffreur
  Cipher := TCipher_AES.Create;
  try
    // Configurer le chiffreur
    Cipher.Mode := cmCBC;
    Cipher.Key := Key;

    // Déchiffrer les données
    Result := Cipher.DecodeString(AEncryptedData, TEncoding.UTF8);
  finally
    Cipher.Free;
  end;
end;
```

## Tests et débogage

### Tests unitaires pour l'architecture client-serveur

Pour assurer la qualité du code :

```pascal
// Test du service d'inventaire
type
  TInventoryServiceTest = class(TTestCase)
  private
    FService: TInventoryService;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetAllProducts;
    procedure TestGetProductByID;
    procedure TestAddProduct;
    procedure TestUpdateProduct;
    procedure TestDeleteProduct;
    procedure TestSearchProducts;
  end;

procedure TInventoryServiceTest.SetUp;
begin
  // Créer une instance de test avec une base de données en mémoire
  FService := TInventoryService.Create(':memory:');

  // Initialiser des données de test
  FService.AddProduct(CreateTestProduct('Test Product 1', 10, 9.99));
  FService.AddProduct(CreateTestProduct('Test Product 2', 20, 19.99));
  FService.AddProduct(CreateTestProduct('Another Item', 5, 4.99));
end;

procedure TInventoryServiceTest.TestGetAllProducts;
var
  Products: TObjectList<TProduct>;
begin
  // Obtenir tous les produits
  Products := FService.GetAllProducts;
  try
    // Vérifier les résultats
    CheckEquals(3, Products.Count, 'Nombre incorrect de produits');

    // Vérifier les détails du premier produit
    CheckEquals('Test Product 1', Products[0].Name, 'Nom du produit incorrect');
    CheckEquals(10, Products[0].Quantity, 'Quantité incorrecte');
    CheckEquals(9.99, Products[0].UnitPrice, 0.01, 'Prix incorrect');
  finally
    Products.Free;
  end;
end;

procedure TInventoryServiceTest.TestAddProduct;
var
  Product: TProduct;
  ProductID: Integer;
  RetrievedProduct: TProduct;
begin
  // Créer un produit de test
  Product := CreateTestProduct('New Test Product', 15, 29.99);
  try
    // Ajouter le produit
    ProductID := FService.AddProduct(Product);

    // Vérifier que l'ID est valide
    CheckTrue(ProductID > 0, 'ID de produit invalide');

    // Récupérer le produit
    RetrievedProduct := FService.GetProductByID(ProductID);
    try
      // Vérifier les détails
      CheckEquals('New Test Product', RetrievedProduct.Name, 'Nom du produit incorrect');
      CheckEquals(15, RetrievedProduct.Quantity, 'Quantité incorrecte');
      CheckEquals(29.99, RetrievedProduct.UnitPrice, 0.01, 'Prix incorrect');
    finally
      RetrievedProduct.Free;
    end;
  finally
    Product.Free;
  end;
end;
```

### Test d'intégration

Pour vérifier l'interaction entre les composants :

```pascal
// Test d'intégration client-serveur
type
  TClientServerIntegrationTest = class(TTestCase)
  private
    FServer: TInventoryServer;
    FClient: TInventoryClient;
    FServerPort: Integer;

    procedure StartServer;
    procedure StopServer;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClientConnection;
    procedure TestGetAllProducts;
    procedure TestAddAndRetrieveProduct;
  end;

procedure TClientServerIntegrationTest.SetUp;
begin
  // Utiliser un port aléatoire pour éviter les conflits
  FServerPort := 10000 + Random(10000);

  // Démarrer le serveur
  StartServer;

  // Créer et connecter le client
  FClient := TInventoryClient.Create;
  CheckTrue(FClient.Connect('localhost', FServerPort), 'Échec de connexion');
end;

procedure TClientServerIntegrationTest.StartServer;
begin
  // Créer et démarrer le serveur avec une base de données en mémoire
  FServer := TInventoryServer.Create(':memory:');

  // Ajouter des données de test
  var Service := FServer.InventoryService;
  Service.AddProduct(CreateTestProduct('Test Product 1', 10, 9.99));
  Service.AddProduct(CreateTestProduct('Test Product 2', 20, 19.99));

  // Démarrer l'écoute
  FServer.Start(FServerPort);

  // Laisser le temps au serveur de démarrer
  Sleep(100);
end;

procedure TClientServerIntegrationTest.TestGetAllProducts;
var
  Products: TObjectList<TProduct>;
begin
  // Récupérer tous les produits via le client
  Products := FClient.GetAllProducts;
  try
    // Vérifier les résultats
    CheckEquals(2, Products.Count, 'Nombre incorrect de produits');

    // Vérifier les détails
    if Products.Count > 0 then
    begin
      CheckEquals('Test Product 1', Products[0].Name, 'Nom du produit incorrect');
      CheckEquals(10, Products[0].Quantity, 'Quantité incorrecte');
    end;
  finally
    Products.Free;
  end;
end;

procedure TClientServerIntegrationTest.TestAddAndRetrieveProduct;
var
  Product: TProduct;
  ProductID: Integer;
  RetrievedProduct: TProduct;
begin
  // Créer un produit de test
  Product := CreateTestProduct('New Integration Test Product', 15, 29.99);
  try
    // Ajouter le produit via le client
    ProductID := FClient.AddProduct(Product);

    // Vérifier que l'ID est valide
    CheckTrue(ProductID > 0, 'ID de produit invalide');

    // Récupérer le produit
    RetrievedProduct := FClient.GetProduct(ProductID);
    try
      // Vérifier les détails
      CheckEquals('New Integration Test Product', RetrievedProduct.Name, 'Nom du produit incorrect');
      CheckEquals(15, RetrievedProduct.Quantity, 'Quantité incorrecte');
      CheckEquals(29.99, RetrievedProduct.UnitPrice, 0.01, 'Prix incorrect');
    finally
      RetrievedProduct.Free;
    end;
  finally
    Product.Free;
  end;
end;
```

### Journalisation pour le débogage

Pour faciliter le diagnostic des problèmes :

```pascal
// Système de journalisation avancé
type
  TLogLevel = (llDebug, llInfo, llWarning, llError, llCritical);

  TLogTarget = (ltConsole, ltFile, ltDatabase, ltRemote);
  TLogTargets = set of TLogTarget;

  TLogger = class
  private
    FLogLevel: TLogLevel;
    FTargets: TLogTargets;
    FLogFile: string;
    FDatabaseConnection: TSQLConnection;
    FRemoteLogger: TRemoteLogger;
    FLock: TCriticalSection;

    procedure LogToConsole(const AMessage: string);
    procedure LogToFile(const AMessage: string);
    procedure LogToDatabase(ALevel: TLogLevel; const AMessage: string);
    procedure LogToRemote(ALevel: TLogLevel; const AMessage: string);
  public
    constructor Create(ALogLevel: TLogLevel = llInfo; ATargets: TLogTargets = [ltConsole, ltFile]);
    destructor Destroy; override;

    procedure Configure(ALogLevel: TLogLevel; ATargets: TLogTargets);
    procedure SetLogFile(const AFileName: string);
    procedure SetDatabaseConnection(AConnection: TSQLConnection);
    procedure SetRemoteLogger(ALogger: TRemoteLogger);

    procedure Log(ALevel: TLogLevel; const AMessage: string); overload;
    procedure Log(ALevel: TLogLevel; const AFormat: string; const Args: array of const); overload;

    procedure Debug(const AMessage: string);
    procedure Info(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Critical(const AMessage: string);
  end;

procedure TLogger.Log(ALevel: TLogLevel; const AMessage: string);
var
  FormattedMessage, LevelStr: string;
begin
  // Si le niveau est inférieur au minimum, ne rien faire
  if ALevel < FLogLevel then
    Exit;

  // Convertir le niveau en chaîne
  case ALevel of
    llDebug: LevelStr := 'DEBUG';
    llInfo: LevelStr := 'INFO';
    llWarning: LevelStr := 'WARNING';
    llError: LevelStr := 'ERROR';
    llCritical: LevelStr := 'CRITICAL';
  end;

  // Formater le message avec date, heure et niveau
  FormattedMessage := Format('[%s] [%s] %s',
                            [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
                             LevelStr,
                             AMessage]);

  // Journaliser vers les cibles configurées
  FLock.Acquire;
  try
    if ltConsole in FTargets then
      LogToConsole(FormattedMessage);

    if ltFile in FTargets then
      LogToFile(FormattedMessage);

    if ltDatabase in FTargets then
      LogToDatabase(ALevel, AMessage);

    if ltRemote in FTargets then
      LogToRemote(ALevel, AMessage);
  finally
    FLock.Release;
  end;
end;

procedure TLogger.LogToConsole(const AMessage: string);
begin
  // Afficher le message dans la console
  WriteLn(AMessage);
end;

procedure TLogger.LogToFile(const AMessage: string);
begin
  // Vérifier si le fichier journal est spécifié
  if FLogFile = '' then
    Exit;

  try
    // Ouvrir le fichier en mode ajout
    with TStreamWriter.Create(FLogFile, True, TEncoding.UTF8) do
    try
      // Écrire le message
      WriteLine(AMessage);
    finally
      Free;
    end;
  except
    // Ignorer les erreurs d'écriture dans le fichier
    on E: Exception do
      LogToConsole(Format('Erreur d''écriture dans le fichier journal: %s', [E.Message]));
  end;
end;
```

## Déploiement

### Packager l'application

Pour préparer l'application au déploiement :

```pascal
// Script de création d'installateur
procedure CreateInstaller;
var
  InstallerScript: TStringList;
begin
  InstallerScript := TStringList.Create;
  try
    // En-tête du script InnoSetup
    InstallerScript.Add('[Setup]');
    InstallerScript.Add('AppName=Inventory Management System');
    InstallerScript.Add('AppVersion=1.0');
    InstallerScript.Add('DefaultDirName={pf}\InventorySystem');
    InstallerScript.Add('DefaultGroupName=Inventory System');
    InstallerScript.Add('OutputDir=installer');
    InstallerScript.Add('OutputBaseFilename=InventorySystem-Setup');
    InstallerScript.Add('Compression=lzma');
    InstallerScript.Add('SolidCompression=yes');
    InstallerScript.Add('');

    // Fichiers à inclure
    InstallerScript.Add('[Files]');
    InstallerScript.Add('Source: "bin\InventoryClient.exe"; DestDir: "{app}"; Flags: ignoreversion');
    InstallerScript.Add('Source: "bin\InventoryServer.exe"; DestDir: "{app}"; Flags: ignoreversion');
    InstallerScript.Add('Source: "config\client.ini"; DestDir: "{app}\config"; Flags: ignoreversion');
    InstallerScript.Add('Source: "config\server.ini"; DestDir: "{app}\config"; Flags: ignoreversion');
    InstallerScript.Add('Source: "db\inventory.db"; DestDir: "{app}\db"; Flags: ignoreversion');
    InstallerScript.Add('Source: "lib\*.dll"; DestDir: "{app}\lib"; Flags: ignoreversion recursesubdirs');
    InstallerScript.Add('');

    // Icônes du menu démarrer
    InstallerScript.Add('[Icons]');
    InstallerScript.Add('Name: "{group}\Inventory Client"; Filename: "{app}\InventoryClient.exe"');
    InstallerScript.Add('Name: "{group}\Inventory Server"; Filename: "{app}\InventoryServer.exe"');
    InstallerScript.Add('Name: "{group}\Uninstall"; Filename: "{uninstallexe}"');

    // Enregistrer le script
    InstallerScript.SaveToFile('installer.iss');

    // Compiler l'installateur (nécessite Inno Setup)
    if FileExists('C:\Program Files (x86)\Inno Setup 6\ISCC.exe') then
      ExecuteProcess('C:\Program Files (x86)\Inno Setup 6\ISCC.exe', 'installer.iss');
  finally
    InstallerScript.Free;
  end;
end;
```

### Configuration du déploiement

Pour faciliter la configuration des installations :

```pascal
// Gestionnaire de configuration
type
  TConfigManager = class
  private
    FConfigFile: string;
    FLock: TCriticalSection;
    FConfig: TMemIniFile;

    procedure LoadConfig;
    procedure SaveConfig;
  public
    constructor Create(const AConfigFile: string);
    destructor Destroy; override;

    function GetString(const ASection, AName: string; const ADefault: string = ''): string;
    function GetInteger(const ASection, AName: string; ADefault: Integer = 0): Integer;
    function GetBoolean(const ASection, AName: string; ADefault: Boolean = False): Boolean;

    procedure SetString(const ASection, AName, AValue: string);
    procedure SetInteger(const ASection, AName: string; AValue: Integer);
    procedure SetBoolean(const ASection, AName: string; AValue: Boolean);

    procedure Save;
  end;

constructor TConfigManager.Create(const AConfigFile: string);
begin
  inherited Create;

  FConfigFile := AConfigFile;
  FLock := TCriticalSection.Create;

  // Créer le répertoire de configuration si nécessaire
  ForceDirectories(ExtractFilePath(FConfigFile));

  // Charger la configuration
  LoadConfig;
end;

procedure TConfigManager.LoadConfig;
begin
  FLock.Acquire;
  try
    // Créer le fichier de configuration s'il n'existe pas
    if not FileExists(FConfigFile) then
    begin
      FConfig := TMemIniFile.Create(FConfigFile);
      SaveConfig;
    end
    else
      FConfig := TMemIniFile.Create(FConfigFile);
  finally
    FLock.Release;
  end;
end;

function TConfigManager.GetString(const ASection, AName: string; const ADefault: string = ''): string;
begin
  FLock.Acquire;
  try
    Result := FConfig.ReadString(ASection, AName, ADefault);
  finally
    FLock.Release;
  end;
end;

procedure TConfigManager.SetString(const ASection, AName, AValue: string);
begin
  FLock.Acquire;
  try
    FConfig.WriteString(ASection, AName, AValue);
  finally
    FLock.Release;
  end;
end;

procedure TConfigManager.Save;
begin
  FLock.Acquire;
  try
    SaveConfig;
  finally
    FLock.Release;
  end;
end;
```

## Problèmes courants et solutions

### Résolution des problèmes de connexion

Conseils pour résoudre les problèmes de connexion :

```pascal
// Vérificateur de connexion
procedure TConnectionTester.TestConnection(const AHost: string; APort: Integer);
var
  Client: TIdTCPClient;
  StartTime, EndTime: TDateTime;
  ElapsedTime: Integer;
begin
  Client := TIdTCPClient.Create(nil);
  try
    // Configurer le client
    Client.Host := AHost;
    Client.Port := APort;
    Client.ConnectTimeout := 5000;  // 5 secondes

    // Essayer de se connecter
    try
      StartTime := Now;
      Client.Connect;
      EndTime := Now;

      // Calculer le temps de connexion en millisecondes
      ElapsedTime := MilliSecondsBetween(EndTime, StartTime);

      // Journaliser le succès
      Log(llInfo, 'Connexion réussie à %s:%d en %d ms', [AHost, APort, ElapsedTime]);

      // Si connecté, essayer d'envoyer et recevoir des données
      if Client.Connected then
      begin
        // Envoyer un message de test
        Client.IOHandler.WriteLn('TEST');

        // Essayer de lire une réponse
        try
          Client.IOHandler.ReadTimeout := 5000;  // 5 secondes
          var Response := Client.IOHandler.ReadLn;

          Log(llInfo, 'Réponse reçue: %s', [Response]);
        except
          on E: Exception do
            Log(llWarning, 'Aucune réponse reçue: %s', [E.Message]);
        end;
      end;
    except
      on E: Exception do
      begin
        // Journaliser l'échec
        Log(llError, 'Échec de connexion à %s:%d - %s', [AHost, APort, E.Message]);

        // Vérifier les causes possibles
        if E is EIdConnectTimeout then
          Log(llInfo, 'Cause possible: Serveur inactif ou pare-feu bloquant')
        else if E is EIdSocketError then
          Log(llInfo, 'Cause possible: Port incorrect ou fermé');
      end;
    end;
  finally
    Client.Free;
  end;
end;
```

### Conseils de dépannage

Liste de conseils de dépannage pour les problèmes courants :

1. **Problème de connexion**
   - Vérifiez que le serveur est en cours d'exécution
   - Vérifiez que le pare-feu autorise les connexions
   - Assurez-vous que l'adresse IP et le port sont corrects
   - Essayez de ping le serveur pour vérifier la connectivité réseau

2. **Erreurs de communication**
   - Augmentez les délais d'attente
   - Vérifiez le format des messages
   - Journalisez les messages envoyés et reçus pour débogage
   - Assurez-vous que le protocole est correctement implémenté

3. **Problèmes de performance**
   - Identifiez les goulots d'étranglement avec des mesures de performance
   - Optimisez les requêtes de base de données
   - Utilisez la mise en cache pour les données fréquemment accédées
   - Utilisez des connexions persistantes plutôt que de créer une nouvelle connexion pour chaque requête

4. **Problèmes de mémoire**
   - Recherchez les fuites de mémoire avec des outils de profilage
   - Assurez-vous de libérer toutes les ressources dans les blocs `finally`
   - Évitez de créer de nombreux objets temporaires

5. **Problèmes de concurrence**
   - Utilisez des verrous pour protéger l'accès aux ressources partagées
   - Gérez correctement les conditions de course
   - Utilisez des files d'attente thread-safe pour la communication entre threads

## Récapitulatif et conclusion

L'architecture client-serveur est un modèle de conception puissant pour créer des applications distribuées. Au cours de ce chapitre, nous avons exploré :

1. **Les fondamentaux de l'architecture client-serveur**
   - Définition et concepts de base
   - Types d'architectures (2-tier, 3-tier, n-tier)
   - Avantages et inconvénients

2. **Implémentation pratique**
   - Communication réseau avec sockets
   - Traitement asynchrone et multithreadé
   - Protocoles de communication

3. **Modèles avancés**
   - Microservices
   - API Gateway
   - Cache distribué
   - Circuit Breaker

4. **Sécurité**
   - Authentification et autorisation
   - Protection contre les attaques courantes
   - Chiffrement des communications

5. **Optimisation et tests**
   - Optimisations de performance
   - Tests unitaires et d'intégration
   - Journalisation et débogage

En maîtrisant ces concepts et techniques, vous pouvez concevoir et implémenter des applications client-serveur robustes, performantes et sécurisées. Bien que l'exemple de l'application d'inventaire soit relativement simple, les principes que nous avons couverts s'appliquent aux systèmes plus complexes.

Rappelez-vous que la bonne conception d'une architecture client-serveur dépend fortement des besoins spécifiques de votre application. Il n'existe pas de solution unique qui convient à tous les cas. L'équilibre entre simplicité, performance, sécurité et maintenabilité est essentiel pour créer une application réussie.

## Exercices pratiques

1. **Application de chat avancée**
   - Étendez l'exemple de chat pour inclure des salons de discussion, des messages privés et le transfert de fichiers
   - Implémentez la persistance des messages
   - Ajoutez une interface utilisateur avec onglets pour chaque conversation

2. **Système de réservation**
   - Créez un système client-serveur pour gérer des réservations (hôtel, restaurant, etc.)
   - Implémentez la concurrence pour gérer les réservations simultanées
   - Ajoutez des notifications en temps réel des nouvelles réservations

3. **Monitoring système distribué**
   - Développez une application client-serveur pour surveiller les métriques système (CPU, mémoire, disque) de plusieurs machines
   - Implémentez des alertes basées sur des seuils configurables
   - Créez une interface utilisateur avec des graphiques pour visualiser les données historiques

4. **Jeu multijoueur simple**
   - Créez un jeu simple (comme Tic-tac-toe ou Bataille navale) utilisant une architecture client-serveur
   - Implémentez un mécanisme de matchmaking pour apparier les joueurs
   - Ajoutez un tableau des scores persistant

5. **Système de fichiers partagés**
   - Développez un système permettant à plusieurs utilisateurs de partager et synchroniser des fichiers
   - Implémentez un mécanisme de verrouillage pour éviter les conflits d'édition
   - Ajoutez un système de versions pour conserver l'historique des modifications

## Guide d'implémentation pour les exercices

### Application de chat avancée

Voici des conseils pour implémenter l'application de chat avancée :

#### Structure de la base de données

```sql
-- Utilisateurs
CREATE TABLE Users (
    UserID INTEGER PRIMARY KEY AUTOINCREMENT,
    Username TEXT UNIQUE NOT NULL,
    PasswordHash TEXT NOT NULL,
    LastSeen DATETIME
);

-- Salons de discussion
CREATE TABLE Rooms (
    RoomID INTEGER PRIMARY KEY AUTOINCREMENT,
    RoomName TEXT NOT NULL,
    IsPrivate BOOLEAN DEFAULT 0,
    CreatedBy INTEGER,
    CreatedAt DATETIME,
    FOREIGN KEY (CreatedBy) REFERENCES Users(UserID)
);

-- Messages de salon
CREATE TABLE RoomMessages (
    MessageID INTEGER PRIMARY KEY AUTOINCREMENT,
    RoomID INTEGER NOT NULL,
    SenderID INTEGER NOT NULL,
    Content TEXT NOT NULL,
    SentAt DATETIME NOT NULL,
    FOREIGN KEY (RoomID) REFERENCES Rooms(RoomID),
    FOREIGN KEY (SenderID) REFERENCES Users(UserID)
);

-- Messages privés
CREATE TABLE PrivateMessages (
    MessageID INTEGER PRIMARY KEY AUTOINCREMENT,
    SenderID INTEGER NOT NULL,
    RecipientID INTEGER NOT NULL,
    Content TEXT NOT NULL,
    SentAt DATETIME NOT NULL,
    IsRead BOOLEAN DEFAULT 0,
    FOREIGN KEY (SenderID) REFERENCES Users(UserID),
    FOREIGN KEY (RecipientID) REFERENCES Users(UserID)
);

-- Fichiers partagés
CREATE TABLE SharedFiles (
    FileID INTEGER PRIMARY KEY AUTOINCREMENT,
    Filename TEXT NOT NULL,
    FileSize INTEGER NOT NULL,
    ContentType TEXT NOT NULL,
    FileData BLOB NOT NULL,
    UploadedBy INTEGER NOT NULL,
    UploadedAt DATETIME NOT NULL,
    RoomID INTEGER,
    RecipientID INTEGER,
    FOREIGN KEY (UploadedBy) REFERENCES Users(UserID),
    FOREIGN KEY (RoomID) REFERENCES Rooms(RoomID),
    FOREIGN KEY (RecipientID) REFERENCES Users(UserID)
);

-- Membres des salons
CREATE TABLE RoomMembers (
    RoomID INTEGER NOT NULL,
    UserID INTEGER NOT NULL,
    JoinedAt DATETIME NOT NULL,
    PRIMARY KEY (RoomID, UserID),
    FOREIGN KEY (RoomID) REFERENCES Rooms(RoomID),
    FOREIGN KEY (UserID) REFERENCES Users(UserID)
);
```

#### Protocole de communication

Définissez un protocole JSON pour la communication entre le client et le serveur :

```pascal
type
  TMessageType = (
    mtAuth,        // Authentification
    mtJoinRoom,    // Rejoindre un salon
    mtLeaveRoom,   // Quitter un salon
    mtRoomMessage, // Message de salon
    mtPrivateMsg,  // Message privé
    mtFileStart,   // Début de transfert de fichier
    mtFileChunk,   // Fragment de fichier
    mtFileEnd,     // Fin de transfert de fichier
    mtUserList,    // Liste d'utilisateurs
    mtRoomList,    // Liste de salons
    mtError,       // Message d'erreur
    mtStatus       // Message de statut
  );

  TMessage = record
    MsgType: TMessageType;
    Sender: string;
    Recipient: string; // Utilisateur ou salon
    Content: string;
    Timestamp: TDateTime;

    // Métadonnées pour les fichiers
    FileName: string;
    FileSize: Int64;
    ChunkIndex: Integer;
    TotalChunks: Integer;

    function ToJSON: string;
    procedure FromJSON(const JSONStr: string);
  end;
```

#### Interface utilisateur avec onglets

Voici comment implémenter l'interface utilisateur avec des onglets :

```pascal
type
  TFormChat = class(TForm)
    PageControl: TPageControl;
    TabRooms: TTabSheet;
    TabUsers: TTabSheet;
    PanelRoomList: TPanel;
    ListBoxRooms: TListBox;
    PanelUserList: TPanel;
    ListBoxUsers: TListBox;
    PanelBottom: TPanel;
    EditMessage: TEdit;
    BtnSend: TButton;
    BtnFile: TButton;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnFileClick(Sender: TObject);
    procedure ListBoxRoomsClick(Sender: TObject);
    procedure ListBoxUsersClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: Char);
  private
    FClient: TChatClient;
    FRoomTabs: TDictionary<string, TTabSheet>;
    FPrivateTabs: TDictionary<string, TTabSheet>;

    procedure OnRoomMessage(Sender: TObject; const ARoomName, AUsername, AMessage: string);
    procedure OnPrivateMessage(Sender: TObject; const AUsername, AMessage: string);
    procedure OnUserListUpdate(Sender: TObject; AUsers: TArray<string>);
    procedure OnRoomListUpdate(Sender: TObject; ARooms: TArray<string>);
    procedure OnFileReceived(Sender: TObject; const AFrom, AFileName: string; AFileData: TBytes);

    function GetOrCreateRoomTab(const ARoomName: string): TTabSheet;
    function GetOrCreatePrivateTab(const AUsername: string): TTabSheet;
    procedure CreateChatControls(ATab: TTabSheet);
  end;

function TFormChat.GetOrCreateRoomTab(const ARoomName: string): TTabSheet;
var
  Tab: TTabSheet;
begin
  // Vérifier si l'onglet existe déjà
  if not FRoomTabs.TryGetValue(ARoomName, Tab) then
  begin
    // Créer un nouvel onglet
    Tab := TTabSheet.Create(PageControl);
    Tab.PageControl := PageControl;
    Tab.Caption := 'Salon: ' + ARoomName;

    // Créer les contrôles de chat
    CreateChatControls(Tab);

    // Stocker l'onglet
    FRoomTabs.Add(ARoomName, Tab);
  end;

  Result := Tab;
end;

procedure TFormChat.CreateChatControls(ATab: TTabSheet);
var
  Memo: TMemo;
begin
  // Créer la zone de texte pour les messages
  Memo := TMemo.Create(ATab);
  Memo.Parent := ATab;
  Memo.Align := alClient;
  Memo.ReadOnly := True;
  Memo.ScrollBars := ssVertical;
  Memo.Tag := Integer(ATab);  // Pour identifier l'onglet associé
end;

procedure TFormChat.OnRoomMessage(Sender: TObject; const ARoomName, AUsername, AMessage: string);
var
  Tab: TTabSheet;
  Memo: TMemo;
begin
  // Obtenir ou créer l'onglet du salon
  Tab := GetOrCreateRoomTab(ARoomName);

  // Trouver la zone de texte
  Memo := FindMemoInTab(Tab);

  // Ajouter le message
  if Assigned(Memo) then
  begin
    Memo.Lines.Add(Format('[%s] %s: %s',
                          [FormatDateTime('hh:nn:ss', Now),
                           AUsername,
                           AMessage]));

    // Faire défiler vers le bas
    SendMessage(Memo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
  end;
end;
```

### Système de réservation

Voici des conseils pour implémenter le système de réservation :

#### Structure de la base de données

```sql
-- Utilisateurs
CREATE TABLE Users (
    UserID INTEGER PRIMARY KEY AUTOINCREMENT,
    Username TEXT UNIQUE NOT NULL,
    PasswordHash TEXT NOT NULL,
    Email TEXT NOT NULL,
    FullName TEXT NOT NULL,
    Phone TEXT
);

-- Ressources réservables (chambres, tables, etc.)
CREATE TABLE Resources (
    ResourceID INTEGER PRIMARY KEY AUTOINCREMENT,
    ResourceName TEXT NOT NULL,
    ResourceType TEXT NOT NULL,
    Capacity INTEGER NOT NULL,
    Location TEXT,
    Description TEXT,
    IsActive BOOLEAN DEFAULT 1
);

-- Réservations
CREATE TABLE Reservations (
    ReservationID INTEGER PRIMARY KEY AUTOINCREMENT,
    ResourceID INTEGER NOT NULL,
    UserID INTEGER NOT NULL,
    StartTime DATETIME NOT NULL,
    EndTime DATETIME NOT NULL,
    NumPeople INTEGER NOT NULL,
    Notes TEXT,
    Status TEXT NOT NULL, -- 'Pending', 'Confirmed', 'Cancelled'
    CreatedAt DATETIME NOT NULL,
    ModifiedAt DATETIME,
    FOREIGN KEY (ResourceID) REFERENCES Resources(ResourceID),
    FOREIGN KEY (UserID) REFERENCES Users(UserID)
);

-- Notifications
CREATE TABLE Notifications (
    NotificationID INTEGER PRIMARY KEY AUTOINCREMENT,
    UserID INTEGER NOT NULL,
    Message TEXT NOT NULL,
    IsRead BOOLEAN DEFAULT 0,
    CreatedAt DATETIME NOT NULL,
    FOREIGN KEY (UserID) REFERENCES Users(UserID)
);
```

#### Gestion de la concurrence

Pour gérer les réservations simultanées :

```pascal
type
  TReservationManager = class
  private
    FDatabase: TSQLConnection;
    FLock: TCriticalSection;

    function IsResourceAvailable(AResourceID: Integer; AStartTime, AEndTime: TDateTime): Boolean;
  public
    constructor Create(ADatabase: TSQLConnection);
    destructor Destroy; override;

    function MakeReservation(AResourceID, AUserID: Integer; AStartTime, AEndTime: TDateTime;
                           ANumPeople: Integer; const ANotes: string): Integer;
    function CancelReservation(AReservationID, AUserID: Integer): Boolean;
    function GetAvailableResources(AStartTime, AEndTime: TDateTime; ACapacity: Integer): TObjectList<TResource>;
  end;

function TReservationManager.MakeReservation(AResourceID, AUserID: Integer; AStartTime, AEndTime: TDateTime;
                                          ANumPeople: Integer; const ANotes: string): Integer;
var
  Query: TSQLQuery;
  StartTime, EndTime: TDateTime;
begin
  Result := -1;

  // Vérifier les paramètres
  if (AResourceID <= 0) or (AUserID <= 0) or
     (AStartTime >= AEndTime) or (ANumPeople <= 0) then
    Exit;

  // Arrondir les heures
  StartTime := RoundToNearest(AStartTime, 30/1440);  // Arrondir à 30 minutes
  EndTime := RoundToNearest(AEndTime, 30/1440);

  FLock.Acquire;
  try
    // Vérifier la disponibilité
    if not IsResourceAvailable(AResourceID, StartTime, EndTime) then
      Exit;

    // Créer la réservation
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      Query.SQL.Text := 'INSERT INTO Reservations ' +
                        '(ResourceID, UserID, StartTime, EndTime, NumPeople, Notes, Status, CreatedAt) ' +
                        'VALUES (?, ?, ?, ?, ?, ?, ?, ?)';

      Query.Params[0].AsInteger := AResourceID;
      Query.Params[1].AsInteger := AUserID;
      Query.Params[2].AsDateTime := StartTime;
      Query.Params[3].AsDateTime := EndTime;
      Query.Params[4].AsInteger := ANumPeople;
      Query.Params[5].AsString := ANotes;
      Query.Params[6].AsString := 'Confirmed';
      Query.Params[7].AsDateTime := Now;

      Query.ExecSQL;

      // Récupérer l'ID de la réservation
      Query.SQL.Text := 'SELECT last_insert_rowid()';
      Query.Open;
      Result := Query.Fields[0].AsInteger;
      Query.Close;

      // Créer une notification pour l'utilisateur
      Query.SQL.Text := 'INSERT INTO Notifications ' +
                        '(UserID, Message, IsRead, CreatedAt) ' +
                        'VALUES (?, ?, ?, ?)';

      Query.Params[0].AsInteger := AUserID;
      Query.Params[1].AsString := 'Votre réservation a été confirmée pour ' +
                                 FormatDateTime('dd/mm/yyyy hh:nn', StartTime);
      Query.Params[2].AsBoolean := False;
      Query.Params[3].AsDateTime := Now;

      Query.ExecSQL;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TReservationManager.IsResourceAvailable(AResourceID: Integer; AStartTime, AEndTime: TDateTime): Boolean;
var
  Query: TSQLQuery;
begin
  Result := False;

  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := FDatabase;

    // Vérifier les chevauchements
    Query.SQL.Text := 'SELECT COUNT(*) FROM Reservations ' +
                      'WHERE ResourceID = ? AND Status <> ''Cancelled'' ' +
                      'AND ((StartTime < ? AND EndTime > ?) OR ' +
                      '     (StartTime < ? AND EndTime > ?) OR ' +
                      '     (StartTime >= ? AND EndTime <= ?))';

    Query.Params[0].AsInteger := AResourceID;
    Query.Params[1].AsDateTime := AEndTime;
    Query.Params[2].AsDateTime := AStartTime;
    Query.Params[3].AsDateTime := AStartTime;
    Query.Params[4].AsDateTime := AStartTime;
    Query.Params[5].AsDateTime := AStartTime;
    Query.Params[6].AsDateTime := AEndTime;

    Query.Open;
    Result := Query.Fields[0].AsInteger = 0;
    Query.Close;
  finally
    Query.Free;
  end;
end;
```

#### Notifications en temps réel

Pour implémenter les notifications en temps réel :

```pascal
type
  TNotificationServer = class
  private
    FClients: TDictionary<Integer, TList<TIdContext>>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterClient(AUserID: Integer; AContext: TIdContext);
    procedure UnregisterClient(AUserID: Integer; AContext: TIdContext);
    procedure SendNotification(AUserID: Integer; const AMessage: string);
    procedure BroadcastNotification(const AMessage: string);
  end;

procedure TNotificationServer.SendNotification(AUserID: Integer; const AMessage: string);
var
  Clients: TList<TIdContext>;
  i: Integer;
  Notification: TJSONObject;
begin
  // Créer l'objet de notification
  Notification := TJSONObject.Create;
  try
    Notification.AddPair('type', 'notification');
    Notification.AddPair('message', AMessage);
    Notification.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Trouver les clients de cet utilisateur
    FLock.Acquire;
    try
      if FClients.TryGetValue(AUserID, Clients) then
      begin
        // Envoyer à tous les clients connectés de cet utilisateur
        for i := 0 to Clients.Count - 1 do
        begin
          try
            Clients[i].Connection.IOHandler.WriteLn(Notification.ToString);
          except
            // Ignorer les erreurs d'envoi
          end;
        end;
      end;
    finally
      FLock.Release;
    end;
  finally
    Notification.Free;
  end;
end;
```

### Monitoring système distribué

Voici des conseils pour implémenter le système de monitoring :

#### Collecte de métriques

Pour collecter des métriques système :

```pascal
type
  TSystemMetrics = class
  private
    function GetCPUUsage: Double;
    function GetMemoryUsage: TMemoryStatus;
    function GetDiskUsage: TDiskStatus;
    function GetNetworkUsage: TNetworkStatus;
  public
    function CollectMetrics: TJSONObject;
  end;

function TSystemMetrics.GetCPUUsage: Double;
var
  PrevIdleTime, PrevKernelTime, PrevUserTime: Int64;
  IdleTime, KernelTime, UserTime: Int64;
  SystemTime, IdleDiff, TotalDiff: Int64;
begin
  // Première mesure
  GetSystemTimes(PrevIdleTime, PrevKernelTime, PrevUserTime);

  // Attendre un court instant
  Sleep(500);

  // Deuxième mesure
  GetSystemTimes(IdleTime, KernelTime, UserTime);

  // Calculer les différences
  IdleDiff := IdleTime - PrevIdleTime;
  TotalDiff := (KernelTime - PrevKernelTime) + (UserTime - PrevUserTime);

  if TotalDiff > 0 then
    Result := 100.0 - (IdleDiff * 100.0 / TotalDiff)
  else
    Result := 0.0;
end;

function TSystemMetrics.CollectMetrics: TJSONObject;
var
  CPUUsage: Double;
  MemStatus: TMemoryStatus;
  DiskStatus: TDiskStatus;
  NetworkStatus: TNetworkStatus;
  Memory, Disk, Network: TJSONObject;
begin
  // Récupérer les métriques
  CPUUsage := GetCPUUsage;
  MemStatus := GetMemoryUsage;
  DiskStatus := GetDiskUsage;
  NetworkStatus := GetNetworkUsage;

  // Créer l'objet JSON
  Result := TJSONObject.Create;

  // Ajouter les métriques CPU
  Result.AddPair('cpu_usage', TJSONNumber.Create(CPUUsage));

  // Ajouter les métriques mémoire
  Memory := TJSONObject.Create;
  Memory.AddPair('total', TJSONNumber.Create(MemStatus.TotalPhys));
  Memory.AddPair('available', TJSONNumber.Create(MemStatus.AvailPhys));
  Memory.AddPair('usage_percent', TJSONNumber.Create(100.0 - (MemStatus.AvailPhys * 100.0 / MemStatus.TotalPhys)));
  Result.AddPair('memory', Memory);

  // Ajouter les métriques disque
  Disk := TJSONObject.Create;
  Disk.AddPair('total', TJSONNumber.Create(DiskStatus.TotalBytes));
  Disk.AddPair('free', TJSONNumber.Create(DiskStatus.FreeBytes));
  Disk.AddPair('usage_percent', TJSONNumber.Create(100.0 - (DiskStatus.FreeBytes * 100.0 / DiskStatus.TotalBytes)));
  Result.AddPair('disk', Disk);

  // Ajouter les métriques réseau
  Network := TJSONObject.Create;
  Network.AddPair('bytes_sent', TJSONNumber.Create(NetworkStatus.BytesSent));
  Network.AddPair('bytes_received', TJSONNumber.Create(NetworkStatus.BytesReceived));
  Result.AddPair('network', Network);

  // Ajouter l'horodatage
  Result.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
  Result.AddPair('hostname', GetComputerName);
end;
```

#### Stockage et analyse des métriques

Pour stocker et analyser les métriques :

```pascal
type
  TMetricsDatabase = class
  private
    FDatabase: TSQLConnection;
    FLock: TCriticalSection;
  public
    constructor Create(const ADatabaseFile: string);
    destructor Destroy; override;

    procedure StoreMetrics(const AHostname: string; AMetrics: TJSONObject);
    function GetMetricsHistory(const AHostname: string; AStartTime, AEndTime: TDateTime): TArray<TJSONObject>;
    function GetAlerts(const AHostname: string; AStartTime, AEndTime: TDateTime): TArray<TJSONObject>;
  end;

procedure TMetricsDatabase.StoreMetrics(const AHostname: string; AMetrics: TJSONObject);
var
  Query: TSQLQuery;
  CPUUsage: Double;
  MemoryUsage, DiskUsage: Double;
  BytesSent, BytesReceived: Int64;
  Timestamp: TDateTime;
begin
  // Extraire les valeurs du JSON
  CPUUsage := AMetrics.GetValue<Double>('cpu_usage');
  MemoryUsage := AMetrics.GetValue<TJSONObject>('memory').GetValue<Double>('usage_percent');
  DiskUsage := AMetrics.GetValue<TJSONObject>('disk').GetValue<Double>('usage_percent');
  BytesSent := AMetrics.GetValue<TJSONObject>('network').GetValue<Int64>('bytes_sent');
  BytesReceived := AMetrics.GetValue<TJSONObject>('network').GetValue<Int64>('bytes_received');
  Timestamp := ISO8601ToDate(AMetrics.GetValue<string>('timestamp'));

  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Insérer les métriques
      Query.SQL.Text := 'INSERT INTO Metrics ' +
                        '(Hostname, Timestamp, CPUUsage, MemoryUsage, DiskUsage, BytesSent, BytesReceived) ' +
                        'VALUES (?, ?, ?, ?, ?, ?, ?)';

      Query.Params[0].AsString := AHostname;
      Query.Params[1].AsDateTime := Timestamp;
      Query.Params[2].AsFloat := CPUUsage;
      Query.Params[3].AsFloat := MemoryUsage;
      Query.Params[4].AsFloat := DiskUsage;
      Query.Params[5].AsLargeInt := BytesSent;
      Query.Params[6].AsLargeInt := BytesReceived;

      Query.ExecSQL;

      // Vérifier les alertes
      CheckAlerts(AHostname, CPUUsage, MemoryUsage, DiskUsage);
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;
```

#### Interface utilisateur avec graphiques

Pour créer une interface avec des graphiques :

```pascal
type
  TFormMonitoring = class(TForm)
    PageControl: TPageControl;
    TabOverview: TTabSheet;
    TabCPU: TTabSheet;
    TabMemory: TTabSheet;
    TabDisk: TTabSheet;
    TabNetwork: TTabSheet;
    TabAlerts: TTabSheet;
    ChartCPU: TChart;
    ChartMemory: TChart;
    ChartDisk: TChart;
    ChartNetwork: TChart;
    ListBoxHosts: TListBox;
    DateTimePickerStart: TDateTimePicker;
    DateTimePickerEnd: TDateTimePicker;
    BtnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure ListBoxHostsClick(Sender: TObject);
  private
    FMonitoringClient: TMonitoringClient;

    procedure LoadHosts;
    procedure UpdateCharts(const AHostname: string; AStartTime, AEndTime: TDateTime);
  end;

procedure TFormMonitoring.UpdateCharts(const AHostname: string; AStartTime, AEndTime: TDateTime);
var
  Metrics: TArray<TJSONObject>;
  CPUSeries, MemorySeries, DiskSeries: TLineSeries;
  NetworkSentSeries, NetworkRecvSeries: TLineSeries;
  i: Integer;
  Timestamp: TDateTime;
begin
  // Effacer les graphiques existants
  ChartCPU.SeriesList.Clear;
  ChartMemory.SeriesList.Clear;
  ChartDisk.SeriesList.Clear;
  ChartNetwork.SeriesList.Clear;

  // Créer les séries
  CPUSeries := TLineSeries.Create(ChartCPU);
  CPUSeries.Title := 'CPU Usage (%)';
  ChartCPU.AddSeries(CPUSeries);

  MemorySeries := TLineSeries.Create(ChartMemory);
  MemorySeries.Title := 'Memory Usage (%)';
  ChartMemory.AddSeries(MemorySeries);

  DiskSeries := TLineSeries.Create(ChartDisk);
  DiskSeries.Title := 'Disk Usage (%)';
  ChartDisk.AddSeries(DiskSeries);

  NetworkSentSeries := TLineSeries.Create(ChartNetwork);
  NetworkSentSeries.Title := 'Bytes Sent';
  ChartNetwork.AddSeries(NetworkSentSeries);

  NetworkRecvSeries := TLineSeries.Create(ChartNetwork);
  NetworkRecvSeries.Title := 'Bytes Received';
  ChartNetwork.AddSeries(NetworkRecvSeries);

  // Récupérer les métriques
  Metrics := FMonitoringClient.GetMetricsHistory(AHostname, AStartTime, AEndTime);

  // Remplir les séries
  for i := 0 to Length(Metrics) - 1 do
  begin
    Timestamp := ISO8601ToDate(Metrics[i].GetValue<string>('timestamp'));

    CPUSeries.AddXY(Timestamp, Metrics[i].GetValue<Double>('cpu_usage'));
    MemorySeries.AddXY(Timestamp, Metrics[i].GetValue<TJSONObject>('memory').GetValue<Double>('usage_percent'));
    DiskSeries.AddXY(Timestamp, Metrics[i].GetValue<TJSONObject>('disk').GetValue<Double>('usage_percent'));
    NetworkSentSeries.AddXY(Timestamp, Metrics[i].GetValue<TJSONObject>('network').GetValue<Int64>('bytes_sent'));
    NetworkRecvSeries.AddXY(Timestamp, Metrics[i].GetValue<TJSONObject>('network').GetValue<Int64>('bytes_received'));
  end;

  // Mettre à jour les axes
  ChartCPU.BottomAxis.Automatic := True;
  ChartCPU.LeftAxis.Automatic := True;
  ChartMemory.BottomAxis.Automatic := True;
  ChartMemory.LeftAxis.Automatic := True;
  ChartDisk.BottomAxis.Automatic := True;
  ChartDisk.LeftAxis.Automatic := True;
  ChartNetwork.BottomAxis.Automatic := True;
  ChartNetwork.LeftAxis.Automatic := True;
end;
```

### Jeu multijoueur simple

Voici des conseils pour implémenter un jeu multijoueur :

#### Structure du jeu Tic-tac-toe

```pascal
type
  TGameState = (gsWaiting, gsPlaying, gsFinished);
  TPlayer = (pNone, pX, pO);
  TGameBoard = array[0..2, 0..2] of TPlayer;

  TGame = class
  private
    FGameID: string;
    FBoard: TGameBoard;
    FPlayerX: string;
    FPlayerO: string;
    FCurrentPlayer: TPlayer;
    FState: TGameState;
    FWinner: TPlayer;

    function CheckWinner: TPlayer;
    function IsBoardFull: Boolean;
  public
    constructor Create(const AGameID, APlayerX, APlayerO: string);

    function MakeMove(const APlayerID: string; ARow, ACol: Integer): Boolean;
    function GetBoardAsJSON: TJSONObject;

    property GameID: string read FGameID;
    property PlayerX: string read FPlayerX;
    property PlayerO: string read FPlayerO;
    property CurrentPlayer: TPlayer read FCurrentPlayer;
    property State: TGameState read FState;
    property Winner: TPlayer read FWinner;
  end;

constructor TGame.Create(const AGameID, APlayerX, APlayerO: string);
var
  Row, Col: Integer;
begin
  inherited Create;

  // Initialiser les propriétés
  FGameID := AGameID;
  FPlayerX := APlayerX;
  FPlayerO := APlayerO;
  FCurrentPlayer := pX;  // X commence toujours
  FState := gsPlaying;
  FWinner := pNone;

  // Initialiser le plateau vide
  for Row := 0 to 2 do
    for Col := 0 to 2 do
      FBoard[Row, Col] := pNone;
end;

function TGame.MakeMove(const APlayerID: string; ARow, ACol: Integer): Boolean;
var
  CurrentPlayerID: string;
begin
  Result := False;

  // Vérifier si le jeu est en cours
  if FState <> gsPlaying then
    Exit;

  // Vérifier si c'est bien le tour du joueur
  if FCurrentPlayer = pX then
    CurrentPlayerID := FPlayerX
  else
    CurrentPlayerID := FPlayerO;

  if APlayerID <> CurrentPlayerID then
    Exit;

  // Vérifier si la position est valide
  if (ARow < 0) or (ARow > 2) or (ACol < 0) or (ACol > 2) then
    Exit;

  // Vérifier si la case est libre
  if FBoard[ARow, ACol] <> pNone then
    Exit;

  // Effectuer le mouvement
  FBoard[ARow, ACol] := FCurrentPlayer;

  // Vérifier si le jeu est terminé
  FWinner := CheckWinner;
  if FWinner <> pNone then
  begin
    FState := gsFinished;
    Result := True;
    Exit;
  end;

  // Vérifier si le plateau est plein (match nul)
  if IsBoardFull then
  begin
    FState := gsFinished;
    Result := True;
    Exit;
  end;

  // Passer au joueur suivant
  if FCurrentPlayer = pX then
    FCurrentPlayer := pO
  else
    FCurrentPlayer := pX;

  Result := True;
end;

function TGame.CheckWinner: TPlayer;
const
  WinPatterns: array[0..7, 0..2, 0..1] of Integer = (
    ((0, 0), (0, 1), (0, 2)),  // Ligne 0
    ((1, 0), (1, 1), (1, 2)),  // Ligne 1
    ((2, 0), (2, 1), (2, 2)),  // Ligne 2
    ((0, 0), (1, 0), (2, 0)),  // Colonne 0
    ((0, 1), (1, 1), (2, 1)),  // Colonne 1
    ((0, 2), (1, 2), (2, 2)),  // Colonne 2
    ((0, 0), (1, 1), (2, 2)),  // Diagonale principale
    ((0, 2), (1, 1), (2, 0))   // Diagonale secondaire
  );
var
  i, j: Integer;
  Row1, Col1, Row2, Col2, Row3, Col3: Integer;
  Player: TPlayer;
begin
  Result := pNone;

  // Vérifier toutes les combinaisons gagnantes
  for i := 0 to 7 do
  begin
    Row1 := WinPatterns[i, 0, 0];
    Col1 := WinPatterns[i, 0, 1];
    Row2 := WinPatterns[i, 1, 0];
    Col2 := WinPatterns[i, 1, 1];
    Row3 := WinPatterns[i, 2, 0];
    Col3 := WinPatterns[i, 2, 1];

    Player := FBoard[Row1, Col1];
    if (Player <> pNone) and
       (Player = FBoard[Row2, Col2]) and
       (Player = FBoard[Row3, Col3]) then
    begin
      Result := Player;
      Exit;
    end;
  end;
end;

function TGame.IsBoardFull: Boolean;
var
  Row, Col: Integer;
begin
  Result := True;

  // Vérifier s'il reste des cases vides
  for Row := 0 to 2 do
    for Col := 0 to 2 do
      if FBoard[Row, Col] = pNone then
      begin
        Result := False;
        Exit;
      end;
end;

function TGame.GetBoardAsJSON: TJSONObject;
var
  BoardArray: TJSONArray;
  RowArray: TJSONArray;
  Row, Col: Integer;
  CellValue: Integer;
begin
  Result := TJSONObject.Create;

  // Ajouter les informations du jeu
  Result.AddPair('game_id', FGameID);
  Result.AddPair('player_x', FPlayerX);
  Result.AddPair('player_o', FPlayerO);
  Result.AddPair('current_player', Integer(FCurrentPlayer));
  Result.AddPair('state', Integer(FState));
  Result.AddPair('winner', Integer(FWinner));

  // Créer un tableau pour le plateau
  BoardArray := TJSONArray.Create;

  // Remplir le tableau
  for Row := 0 to 2 do
  begin
    RowArray := TJSONArray.Create;

    for Col := 0 to 2 do
    begin
      CellValue := Integer(FBoard[Row, Col]);
      RowArray.Add(CellValue);
    end;

    BoardArray.Add(RowArray);
  end;

  Result.AddPair('board', BoardArray);
end;
```

#### Serveur de jeu

```pascal
type
  TGameServer = class
  private
    FGames: TDictionary<string, TGame>;
    FPlayerGames: TDictionary<string, string>;
    FWaitingPlayer: string;
    FLock: TCriticalSection;
    FServer: TIdTCPServer;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);

    function ProcessJoinGame(const APlayerID: string): string;
    function ProcessMakeMove(const APlayerID, AGameID: string; ARow, ACol: Integer): Boolean;
    function ProcessGetGame(const APlayerID, AGameID: string): TJSONObject;

    function CreateGame(const APlayerX, APlayerO: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;
  end;

constructor TGameServer.Create;
begin
  inherited Create;

  // Initialiser les collections
  FGames := TDictionary<string, TGame>.Create;
  FPlayerGames := TDictionary<string, string>.Create;
  FWaitingPlayer := '';
  FLock := TCriticalSection.Create;

  // Créer et configurer le serveur
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnExecute := OnExecute;
end;

procedure TGameServer.OnExecute(AContext: TIdContext);
var
  Request: string;
  RequestObj: TJSONObject;
  Command: string;
  PlayerID, GameID: string;
  Row, Col: Integer;
  Response: TJSONObject;
  Success: Boolean;
begin
  // Lire la requête
  Request := AContext.Connection.IOHandler.ReadLn;

  try
    // Parser la requête JSON
    RequestObj := TJSONObject.ParseJSONValue(Request) as TJSONObject;
    try
      // Extraire les informations principales
      Command := RequestObj.GetValue('command').Value;
      PlayerID := RequestObj.GetValue('player_id').Value;

      // Traiter selon la commande
      if Command = 'join_game' then
      begin
        // Rejoindre ou créer une partie
        GameID := ProcessJoinGame(PlayerID);

        // Préparer la réponse
        Response := TJSONObject.Create;
        Response.AddPair('success', True);
        Response.AddPair('game_id', GameID);
      end
      else if Command = 'make_move' then
      begin
        // Extraire les informations du mouvement
        GameID := RequestObj.GetValue('game_id').Value;
        Row := RequestObj.GetValue<Integer>('row');
        Col := RequestObj.GetValue<Integer>('col');

        // Effectuer le mouvement
        Success := ProcessMakeMove(PlayerID, GameID, Row, Col);

        // Préparer la réponse
        Response := TJSONObject.Create;
        Response.AddPair('success', Success);

        // Si réussi, inclure l'état du jeu
        if Success then
          Response.AddPair('game', ProcessGetGame(PlayerID, GameID));
      end
      else if Command = 'get_game' then
      begin
        // Extraire l'ID du jeu
        GameID := RequestObj.GetValue('game_id').Value;

        // Préparer la réponse
        Response := TJSONObject.Create;
        Response.AddPair('success', True);
        Response.AddPair('game', ProcessGetGame(PlayerID, GameID));
      end
      else
      begin
        // Commande inconnue
        Response := TJSONObject.Create;
        Response.AddPair('success', False);
        Response.AddPair('error', 'Commande inconnue');
      end;
    finally
      RequestObj.Free;
    end;
  except
    on E: Exception do
    begin
      // Erreur lors du traitement
      Response := TJSONObject.Create;
      Response.AddPair('success', False);
      Response.AddPair('error', E.Message);
    end;
  end;

  try
    // Envoyer la réponse
    AContext.Connection.IOHandler.WriteLn(Response.ToString);
  finally
    Response.Free;
  end;
end;

function TGameServer.ProcessJoinGame(const APlayerID: string): string;
var
  Game: TGame;
begin
  FLock.Acquire;
  try
    // Vérifier si le joueur est déjà dans une partie
    if FPlayerGames.TryGetValue(APlayerID, Result) then
      Exit;

    // S'il y a un joueur en attente, créer une partie
    if FWaitingPlayer <> '' then
    begin
      // Créer une nouvelle partie
      Result := CreateGame(FWaitingPlayer, APlayerID);

      // Associer les joueurs à la partie
      FPlayerGames.Add(FWaitingPlayer, Result);
      FPlayerGames.Add(APlayerID, Result);

      // Réinitialiser le joueur en attente
      FWaitingPlayer := '';
    end
    else
    begin
      // Mettre le joueur en attente
      FWaitingPlayer := APlayerID;
      Result := '';
    end;
  finally
    FLock.Release;
  end;
end;

function TGameServer.CreateGame(const APlayerX, APlayerO: string): string;
var
  Game: TGame;
  GameID: string;
begin
  // Générer un ID unique pour la partie
  GameID := CreateGUID.ToString;

  // Créer une nouvelle partie
  Game := TGame.Create(GameID, APlayerX, APlayerO);

  // Ajouter à la collection
  FGames.Add(GameID, Game);

  Result := GameID;
end;

function TGameServer.ProcessMakeMove(const APlayerID, AGameID: string; ARow, ACol: Integer): Boolean;
var
  Game: TGame;
begin
  Result := False;

  FLock.Acquire;
  try
    // Vérifier si la partie existe
    if not FGames.TryGetValue(AGameID, Game) then
      Exit;

    // Effectuer le mouvement
    Result := Game.MakeMove(APlayerID, ARow, ACol);
  finally
    FLock.Release;
  end;
end;
```

#### Client de jeu

```pascal
type
  TTicTacToeClient = class
  private
    FClient: TIdTCPClient;
    FPlayerID: string;
    FGameID: string;
    FIsConnected: Boolean;

    function SendRequest(ARequest: TJSONObject): TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AHost: string; APort: Integer): Boolean;
    procedure Disconnect;

    function JoinGame: Boolean;
    function MakeMove(ARow, ACol: Integer): Boolean;
    function GetGameState: TJSONObject;

    property PlayerID: string read FPlayerID;
    property GameID: string read FGameID;
    property IsConnected: Boolean read FIsConnected;
  end;

constructor TTicTacToeClient.Create;
begin
  inherited Create;

  // Créer le client TCP
  FClient := TIdTCPClient.Create(nil);

  // Générer un ID unique pour le joueur
  FPlayerID := CreateGUID.ToString;
  FGameID := '';
  FIsConnected := False;
end;

function TTicTacToeClient.Connect(const AHost: string; APort: Integer): Boolean;
begin
  Result := False;

  try
    // Configurer la connexion
    FClient.Host := AHost;
    FClient.Port := APort;

    // Se connecter au serveur
    FClient.Connect;
    FIsConnected := True;

    Result := True;
  except
    on E: Exception do
    begin
      FIsConnected := False;
      // Gérer l'erreur (afficher un message, journaliser, etc.)
    end;
  end;
end;

function TTicTacToeClient.JoinGame: Boolean;
var
  Request, Response: TJSONObject;
begin
  Result := False;

  // Vérifier si connecté
  if not FIsConnected then
    Exit;

  // Créer la requête
  Request := TJSONObject.Create;
  try
    Request.AddPair('command', 'join_game');
    Request.AddPair('player_id', FPlayerID);

    // Envoyer la requête
    Response := SendRequest(Request);
    try
      // Traiter la réponse
      if Response.GetValue<Boolean>('success') then
      begin
        FGameID := Response.GetValue<string>('game_id');
        Result := FGameID <> '';
      end;
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;

function TTicTacToeClient.MakeMove(ARow, ACol: Integer): Boolean;
var
  Request, Response: TJSONObject;
begin
  Result := False;

  // Vérifier si connecté et dans une partie
  if (not FIsConnected) or (FGameID = '') then
    Exit;

  // Créer la requête
  Request := TJSONObject.Create;
  try
    Request.AddPair('command', 'make_move');
    Request.AddPair('player_id', FPlayerID);
    Request.AddPair('game_id', FGameID);
    Request.AddPair('row', TJSONNumber.Create(ARow));
    Request.AddPair('col', TJSONNumber.Create(ACol));

    // Envoyer la requête
    Response := SendRequest(Request);
    try
      // Traiter la réponse
      Result := Response.GetValue<Boolean>('success');
    finally
      Response.Free;
    end;
  finally
    Request.Free;
  end;
end;

function TTicTacToeClient.SendRequest(ARequest: TJSONObject): TJSONObject;
var
  RequestStr, ResponseStr: string;
begin
  // Convertir la requête en chaîne
  RequestStr := ARequest.ToString;

  try
    // Envoyer la requête
    FClient.IOHandler.WriteLn(RequestStr);

    // Lire la réponse
    ResponseStr := FClient.IOHandler.ReadLn;

    // Parser la réponse
    Result := TJSONObject.ParseJSONValue(ResponseStr) as TJSONObject;
  except
    on E: Exception do
    begin
      // En cas d'erreur, créer une réponse d'erreur
      Result := TJSONObject.Create;
      Result.AddPair('success', False);
      Result.AddPair('error', E.Message);

      // Marquer comme déconnecté
      FIsConnected := False;
    end;
  end;
end;
```

#### Interface utilisateur du jeu

```pascal
type
  TFormTicTacToe = class(TForm)
    PanelTop: TPanel;
    LabelStatus: TLabel;
    PanelBoard: TPanel;
    BtnConnect: TButton;
    BtnJoinGame: TButton;
    TimerRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnJoinGameClick(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure BoardButtonClick(Sender: TObject);
  private
    FClient: TTicTacToeClient;
    FButtons: array[0..2, 0..2] of TButton;
    FCurrentGameState: TJSONObject;

    procedure CreateBoardButtons;
    procedure UpdateBoard;
    procedure EnableBoard(AEnable: Boolean);
    function IsMyTurn: Boolean;
  end;

procedure TFormTicTacToe.FormCreate(Sender: TObject);
begin
  // Créer le client
  FClient := TTicTacToeClient.Create;

  // Créer les boutons du plateau
  CreateBoardButtons;

  // Initialiser l'état
  FCurrentGameState := nil;

  // Désactiver initialement
  EnableBoard(False);
  BtnJoinGame.Enabled := False;
  TimerRefresh.Enabled := False;
end;

procedure TFormTicTacToe.CreateBoardButtons;
var
  Button: TButton;
  Row, Col: Integer;
  ButtonWidth, ButtonHeight: Integer;
begin
  // Calculer la taille des boutons
  ButtonWidth := PanelBoard.Width div 3;
  ButtonHeight := PanelBoard.Height div 3;

  // Créer une grille 3x3 de boutons
  for Row := 0 to 2 do
    for Col := 0 to 2 do
    begin
      Button := TButton.Create(Self);
      Button.Parent := PanelBoard;
      Button.Left := Col * ButtonWidth;
      Button.Top := Row * ButtonHeight;
      Button.Width := ButtonWidth;
      Button.Height := ButtonHeight;
      Button.Font.Size := 24;
      Button.Tag := Row * 3 + Col;  // Pour identifier la position
      Button.OnClick := BoardButtonClick;

      FButtons[Row, Col] := Button;
    end;
end;

procedure TFormTicTacToe.BtnConnectClick(Sender: TObject);
var
  Host: string;
  Port: Integer;
begin
  // Demander l'adresse du serveur
  Host := InputBox('Connexion', 'Adresse du serveur:', '127.0.0.1');

  // Demander le port
  if not TryStrToInt(InputBox('Connexion', 'Port:', '8080'), Port) then
  begin
    ShowMessage('Port invalide');
    Exit;
  end;

  // Se connecter
  if FClient.Connect(Host, Port) then
  begin
    ShowMessage('Connecté au serveur');
    BtnConnect.Enabled := False;
    BtnJoinGame.Enabled := True;
  end
  else
    ShowMessage('Erreur de connexion');
end;

procedure TFormTicTacToe.BtnJoinGameClick(Sender: TObject);
begin
  // Rejoindre une partie
  if FClient.JoinGame then
  begin
    if FClient.GameID <> '' then
    begin
      ShowMessage('Partie trouvée! ID: ' + FClient.GameID);
      TimerRefresh.Enabled := True;
      BtnJoinGame.Enabled := False;
    end
    else
    begin
      ShowMessage('En attente d''un autre joueur...');
      TimerRefresh.Enabled := True;
      BtnJoinGame.Enabled := False;
    end;
  end
  else
    ShowMessage('Erreur lors de la recherche d''une partie');
end;

procedure TFormTicTacToe.TimerRefreshTimer(Sender: TObject);
var
  GameState: TJSONObject;
begin
  // Si pas encore dans une partie, vérifier si une partie est disponible
  if (FClient.GameID = '') and FClient.JoinGame and (FClient.GameID <> '') then
  begin
    ShowMessage('Partie trouvée! ID: ' + FClient.GameID);
    BtnJoinGame.Enabled := False;
  end;

  // Si dans une partie, récupérer l'état actuel
  if FClient.GameID <> '' then
  begin
    GameState := FClient.GetGameState;
    if GameState <> nil then
    begin
      // Libérer l'ancien état
      FreeAndNil(FCurrentGameState);

      // Stocker le nouvel état
      FCurrentGameState := GameState;

      // Mettre à jour l'affichage
      UpdateBoard;
    end;
  end;
end;

procedure TFormTicTacToe.UpdateBoard;
var
  Row, Col: Integer;
  BoardArray: TJSONArray;
  RowArray: TJSONArray;
  CellValue: Integer;
  Symbol: string;
  GameState, Winner: Integer;
  CurrentPlayer: Integer;
  IsMyTurn: Boolean;
begin
  if FCurrentGameState = nil then
    Exit;

  // Extraire les informations du jeu
  GameState := FCurrentGameState.GetValue<TJSONObject>('game').GetValue<Integer>('state');
  CurrentPlayer := FCurrentGameState.GetValue<TJSONObject>('game').GetValue<Integer>('current_player');
  Winner := FCurrentGameState.GetValue<TJSONObject>('game').GetValue<Integer>('winner');

  // Déterminer si c'est le tour du joueur
  IsMyTurn := (CurrentPlayer = 1) and
             (FClient.PlayerID = FCurrentGameState.GetValue<TJSONObject>('game').GetValue<string>('player_x')) or
             (CurrentPlayer = 2) and
             (FClient.PlayerID = FCurrentGameState.GetValue<TJSONObject>('game').GetValue<string>('player_o'));

  // Activer/désactiver le plateau en fonction du tour
  EnableBoard(IsMyTurn and (GameState = 1));  // 1 = gsPlaying

  // Mettre à jour l'état du jeu
  case GameState of
    0: LabelStatus.Caption := 'En attente d''un autre joueur...';
    1: begin
         if IsMyTurn then
           LabelStatus.Caption := 'C''est votre tour'
         else
           LabelStatus.Caption := 'En attente de l''adversaire...';
       end;
    2: begin
         case Winner of
           0: LabelStatus.Caption := 'Match nul!';
           1: begin
                if FClient.PlayerID = FCurrentGameState.GetValue<TJSONObject>('game').GetValue<string>('player_x') then
                  LabelStatus.Caption := 'Vous avez gagné!'
                else
                  LabelStatus.Caption := 'Vous avez perdu!';
              end;
           2: begin
                if FClient.PlayerID = FCurrentGameState.GetValue<TJSONObject>('game').GetValue<string>('player_o') then
                  LabelStatus.Caption := 'Vous avez gagné!'
                else
                  LabelStatus.Caption := 'Vous avez perdu!';
              end;
         end;
       end;
  end;

  // Mettre à jour le plateau de jeu
  BoardArray := FCurrentGameState.GetValue<TJSONObject>('game').GetValue<TJSONArray>('board');

  for Row := 0 to 2 do
  begin
    RowArray := BoardArray.Items[Row] as TJSONArray;

    for Col := 0 to 2 do
    begin
      CellValue := RowArray.Items[Col].AsInteger;

      case CellValue of
        0: Symbol := '';    // Vide
        1: Symbol := 'X';   // Joueur X
        2: Symbol := 'O';   // Joueur O
      end;

      FButtons[Row, Col].Caption := Symbol;
    end;
  end;
end;

procedure TFormTicTacToe.EnableBoard(AEnable: Boolean);
var
  Row, Col: Integer;
begin
  for Row := 0 to 2 do
    for Col := 0 to 2 do
      FButtons[Row, Col].Enabled := AEnable and (FButtons[Row, Col].Caption = '');
end;

procedure TFormTicTacToe.BoardButtonClick(Sender: TObject);
var
  Button: TButton;
  Row, Col: Integer;
begin
  Button := Sender as TButton;

  // Calculer la position
  Row := Button.Tag div 3;
  Col := Button.Tag mod 3;

  // Effectuer le mouvement
  if FClient.MakeMove(Row, Col) then
  begin
    // La mise à jour se fera automatiquement via le timer
  end
  else
    ShowMessage('Erreur lors du mouvement');
end;
```

#### Système de matchmaking

Pour apparier les joueurs de manière plus sophistiquée :

```pascal
type
  TPlayerSkill = record
    PlayerID: string;
    Rating: Integer;
    GamesPlayed: Integer;
  end;

  TMatchmaker = class
  private
    FWaitingPlayers: TList<TPlayerSkill>;
    FPlayerRatings: TDictionary<string, TPlayerSkill>;
    FLock: TCriticalSection;

    function CalculateMatchScore(const Player1, Player2: TPlayerSkill): Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPlayer(const APlayerID: string);
    procedure RemovePlayer(const APlayerID: string);
    function FindMatch(const APlayerID: string): string;
    procedure UpdateRating(const APlayerID: string; AWon: Boolean);
  end;

function TMatchmaker.FindMatch(const APlayerID: string): string;
var
  CurrentPlayer: TPlayerSkill;
  BestMatch: string;
  BestScore: Double;
  Score: Double;
  i: Integer;
begin
  Result := '';
  BestMatch := '';
  BestScore := -1;

  // Récupérer les informations du joueur
  if not FPlayerRatings.TryGetValue(APlayerID, CurrentPlayer) then
    Exit;

  FLock.Acquire;
  try
    // Parcourir tous les joueurs en attente
    for i := 0 to FWaitingPlayers.Count - 1 do
    begin
      // Ne pas s'apparier avec soi-même
      if FWaitingPlayers[i].PlayerID = APlayerID then
        Continue;

      // Calculer le score d'appariement
      Score := CalculateMatchScore(CurrentPlayer, FWaitingPlayers[i]);

      // Mettre à jour le meilleur match
      if (Score > BestScore) or (BestMatch = '') then
      begin
        BestScore := Score;
        BestMatch := FWaitingPlayers[i].PlayerID;
      end;
    end;

    // Si un match est trouvé, retirer les deux joueurs de la liste d'attente
    if BestMatch <> '' then
    begin
      RemovePlayer(APlayerID);
      RemovePlayer(BestMatch);
      Result := BestMatch;
    end;
  finally
    FLock.Release;
  end;
end;

function TMatchmaker.CalculateMatchScore(const Player1, Player2: TPlayerSkill): Double;
var
  RatingDiff: Integer;
begin
  // Calculer la différence de classement
  RatingDiff := Abs(Player1.Rating - Player2.Rating);

  // Un score plus élevé indique un meilleur match
  // Plus la différence est faible, meilleur est le match
  if RatingDiff = 0 then
    Result := 1.0  // Match parfait
  else
    Result := 1.0 / RatingDiff;

  // Ajuster en fonction du nombre de parties jouées
  // Favoriser les joueurs avec un nombre similaire d'expérience
  Result := Result * (1.0 - Abs(Player1.GamesPlayed - Player2.GamesPlayed) /
                           Max(Player1.GamesPlayed + Player2.GamesPlayed, 1));
end;

procedure TMatchmaker.UpdateRating(const APlayerID: string; AWon: Boolean);
var
  Player: TPlayerSkill;
begin
  FLock.Acquire;
  try
    // Vérifier si le joueur existe
    if FPlayerRatings.TryGetValue(APlayerID, Player) then
    begin
      // Mettre à jour le classement (système ELO simplifié)
      if AWon then
        Player.Rating := Player.Rating + 15
      else
        Player.Rating := Player.Rating - 10;

      // Limiter la plage du classement
      Player.Rating := Max(100, Min(Player.Rating, 2000));

      // Incrémenter le nombre de parties
      Inc(Player.GamesPlayed);

      // Mettre à jour le dictionnaire
      FPlayerRatings[APlayerID] := Player;
    end;
  finally
    FLock.Release;
  end;
end;
```

#### Tableau des scores

Pour implémenter un tableau des scores persistant :

```pascal
type
  TScoreEntry = record
    PlayerID: string;
    PlayerName: string;
    Wins: Integer;
    Losses: Integer;
    Rating: Integer;
  end;

  TScoreboard = class
  private
    FDatabase: TSQLConnection;
    FLock: TCriticalSection;
  public
    constructor Create(const ADatabaseFile: string);
    destructor Destroy; override;

    procedure RecordGameResult(const AGameID, AWinnerID, ALoserID: string; ATie: Boolean = False);
    function GetTopPlayers(ACount: Integer): TArray<TScoreEntry>;
    function GetPlayerStats(const APlayerID: string): TScoreEntry;
    procedure UpdatePlayerName(const APlayerID, APlayerName: string);
  end;

constructor TScoreboard.Create(const ADatabaseFile: string);
begin
  inherited Create;

  FLock := TCriticalSection.Create;

  // Créer la connexion à la base de données
  FDatabase := TSQLConnection.Create(nil);
  FDatabase.DriverName := 'SQLite';
  FDatabase.Params.Values['Database'] := ADatabaseFile;
  FDatabase.Connected := True;

  // Créer les tables si elles n'existent pas
  CreateTables;
end;

procedure TScoreboard.CreateTables;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := FDatabase;

    // Table des joueurs
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Players (' +
      '  PlayerID TEXT PRIMARY KEY, ' +
      '  PlayerName TEXT, ' +
      '  Wins INTEGER DEFAULT 0, ' +
      '  Losses INTEGER DEFAULT 0, ' +
      '  Ties INTEGER DEFAULT 0, ' +
      '  Rating INTEGER DEFAULT 1000, ' +
      '  LastPlayed DATETIME' +
      ')';
    Query.ExecSQL;

    // Table des parties
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Games (' +
      '  GameID TEXT PRIMARY KEY, ' +
      '  PlayerX TEXT, ' +
      '  PlayerO TEXT, ' +
      '  Winner TEXT, ' +
      '  IsTie BOOLEAN, ' +
      '  PlayedAt DATETIME' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

procedure TScoreboard.RecordGameResult(const AGameID, AWinnerID, ALoserID: string; ATie: Boolean = False);
var
  Query: TSQLQuery;
  PlayerX, PlayerO: string;
begin
  // Déterminer les joueurs X et O (basé sur l'implémentation du jeu)
  PlayerX := AWinnerID;
  PlayerO := ALoserID;

  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Ajouter l'entrée de la partie
      Query.SQL.Text :=
        'INSERT INTO Games (GameID, PlayerX, PlayerO, Winner, IsTie, PlayedAt) ' +
        'VALUES (?, ?, ?, ?, ?, ?)';

      Query.Params[0].AsString := AGameID;
      Query.Params[1].AsString := PlayerX;
      Query.Params[2].AsString := PlayerO;

      if ATie then
        Query.Params[3].AsString := ''
      else
        Query.Params[3].AsString := AWinnerID;

      Query.Params[4].AsBoolean := ATie;
      Query.Params[5].AsDateTime := Now;

      Query.ExecSQL;

      // Mettre à jour les statistiques des joueurs

      // Vérifier si le gagnant existe déjà
      Query.SQL.Text := 'SELECT COUNT(*) FROM Players WHERE PlayerID = ?';
      Query.Params[0].AsString := AWinnerID;
      Query.Open;

      if Query.Fields[0].AsInteger = 0 then
      begin
        // Ajouter le joueur
        Query.Close;
        Query.SQL.Text :=
          'INSERT INTO Players (PlayerID, Wins, Losses, Ties, LastPlayed) ' +
          'VALUES (?, ?, ?, ?, ?)';

        Query.Params[0].AsString := AWinnerID;

        if ATie then
        begin
          Query.Params[1].AsInteger := 0;
          Query.Params[2].AsInteger := 0;
          Query.Params[3].AsInteger := 1;
        end
        else
        begin
          Query.Params[1].AsInteger := 1;
          Query.Params[2].AsInteger := 0;
          Query.Params[3].AsInteger := 0;
        end;

        Query.Params[4].AsDateTime := Now;

        Query.ExecSQL;
      end
      else
      begin
        // Mettre à jour le joueur
        Query.Close;

        if ATie then
          Query.SQL.Text :=
            'UPDATE Players SET Ties = Ties + 1, LastPlayed = ? ' +
            'WHERE PlayerID = ?'
        else
          Query.SQL.Text :=
            'UPDATE Players SET Wins = Wins + 1, LastPlayed = ? ' +
            'WHERE PlayerID = ?';

        Query.Params[0].AsDateTime := Now;
        Query.Params[1].AsString := AWinnerID;

        Query.ExecSQL;
      end;

      // Vérifier si le perdant existe déjà
      Query.SQL.Text := 'SELECT COUNT(*) FROM Players WHERE PlayerID = ?';
      Query.Params[0].AsString := ALoserID;
      Query.Open;

      if Query.Fields[0].AsInteger = 0 then
      begin
        // Ajouter le joueur
        Query.Close;
        Query.SQL.Text :=
          'INSERT INTO Players (PlayerID, Wins, Losses, Ties, LastPlayed) ' +
          'VALUES (?, ?, ?, ?, ?)';

        Query.Params[0].AsString := ALoserID;

        if ATie then
        begin
          Query.Params[1].AsInteger := 0;
          Query.Params[2].AsInteger := 0;
          Query.Params[3].AsInteger := 1;
        end
        else
        begin
          Query.Params[1].AsInteger := 0;
          Query.Params[2].AsInteger := 1;
          Query.Params[3].AsInteger := 0;
        end;

        Query.Params[4].AsDateTime := Now;

        Query.ExecSQL;
      end
      else
      begin
        // Mettre à jour le joueur
        Query.Close;

        if ATie then
          Query.SQL.Text :=
            'UPDATE Players SET Ties = Ties + 1, LastPlayed = ? ' +
            'WHERE PlayerID = ?'
        else
          Query.SQL.Text :=
            'UPDATE Players SET Losses = Losses + 1, LastPlayed = ? ' +
            'WHERE PlayerID = ?';

        Query.Params[0].AsDateTime := Now;
        Query.Params[1].AsString := ALoserID;

        Query.ExecSQL;
      end;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TScoreboard.GetTopPlayers(ACount: Integer): TArray<TScoreEntry>;
var
  Query: TSQLQuery;
  i: Integer;
begin
  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Récupérer les meilleurs joueurs
      Query.SQL.Text :=
        'SELECT PlayerID, PlayerName, Wins, Losses, Rating ' +
        'FROM Players ' +
        'ORDER BY Rating DESC, Wins DESC ' +
        'LIMIT ?';

      Query.Params[0].AsInteger := ACount;
      Query.Open;

      // Préparer le tableau des résultats
      SetLength(Result, Query.RecordCount);

      // Remplir le tableau
      i := 0;
      while not Query.Eof do
      begin
        Result[i].PlayerID := Query.FieldByName('PlayerID').AsString;
        Result[i].PlayerName := Query.FieldByName('PlayerName').AsString;
        Result[i].Wins := Query.FieldByName('Wins').AsInteger;
        Result[i].Losses := Query.FieldByName('Losses').AsInteger;
        Result[i].Rating := Query.FieldByName('Rating').AsInteger;

        Inc(i);
        Query.Next;
      end;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;
```

#### Interface du tableau des scores

```pascal
type
  TFormScoreboard = class(TForm)
    StringGridScores: TStringGrid;
    ComboBoxFilter: TComboBox;
    LabelFilter: TLabel;
    BtnRefresh: TButton;
    BtnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure ComboBoxFilterChange(Sender: TObject);
  private
    FScoreboard: TScoreboard;

    procedure UpdateScoreboard;
  end;

procedure TFormScoreboard.FormCreate(Sender: TObject);
begin
  // Configurer la grille
  StringGridScores.Cells[0, 0] := 'Rang';
  StringGridScores.Cells[1, 0] := 'Joueur';
  StringGridScores.Cells[2, 0] := 'Victoires';
  StringGridScores.Cells[3, 0] := 'Défaites';
  StringGridScores.Cells[4, 0] := 'Classement';

  // Configurer le filtre
  ComboBoxFilter.Items.Add('Top 10');
  ComboBoxFilter.Items.Add('Top 20');
  ComboBoxFilter.Items.Add('Top 50');
  ComboBoxFilter.Items.Add('Tous');
  ComboBoxFilter.ItemIndex := 0;

  // Créer l'objet de tableau des scores
  FScoreboard := TScoreboard.Create('tictactoe.db');

  // Mettre à jour l'affichage
  UpdateScoreboard;
end;

procedure TFormScoreboard.UpdateScoreboard;
var
  Scores: TArray<TScoreEntry>;
  i: Integer;
  Count: Integer;
begin
  // Déterminer le nombre de joueurs à afficher
  case ComboBoxFilter.ItemIndex of
    0: Count := 10;
    1: Count := 20;
    2: Count := 50;
    3: Count := 1000;  // Pratiquement tous
  end;

  // Récupérer les scores
  Scores := FScoreboard.GetTopPlayers(Count);

  // Configurer la grille
  StringGridScores.RowCount := Length(Scores) + 1;

  // Remplir la grille
  for i := 0 to Length(Scores) - 1 do
  begin
    StringGridScores.Cells[0, i + 1] := IntToStr(i + 1);

    if Scores[i].PlayerName <> '' then
      StringGridScores.Cells[1, i + 1] := Scores[i].PlayerName
    else
      StringGridScores.Cells[1, i + 1] := 'Joueur ' + Copy(Scores[i].PlayerID, 1, 8);

    StringGridScores.Cells[2, i + 1] := IntToStr(Scores[i].Wins);
    StringGridScores.Cells[3, i + 1] := IntToStr(Scores[i].Losses);
    StringGridScores.Cells[4, i + 1] := IntToStr(Scores[i].Rating);
  end;
end;

procedure TFormScoreboard.BtnRefreshClick(Sender: TObject);
begin
  UpdateScoreboard;
end;

procedure TFormScoreboard.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormScoreboard.ComboBoxFilterChange(Sender: TObject);
begin
  UpdateScoreboard;
end;
```

#### Intégration dans le serveur de jeu

Pour intégrer le matchmaking et le tableau des scores dans le serveur de jeu :

```pascal
type
  TGameServer = class
  private
    FGames: TDictionary<string, TGame>;
    FPlayerGames: TDictionary<string, string>;
    FLock: TCriticalSection;
    FServer: TIdTCPServer;
    FMatchmaker: TMatchmaker;
    FScoreboard: TScoreboard;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);

    function ProcessJoinGame(const APlayerID: string): string;
    function ProcessMakeMove(const APlayerID, AGameID: string; ARow, ACol: Integer): Boolean;
    function ProcessGetGame(const APlayerID, AGameID: string): TJSONObject;
    function ProcessGetScoreboard(ACount: Integer): TJSONArray;
    function ProcessSetPlayerName(const APlayerID, APlayerName: string): Boolean;

    function CreateGame(const APlayerX, APlayerO: string): string;
    procedure GameOver(AGame: TGame);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;
  end;

constructor TGameServer.Create;
begin
  inherited Create;

  // Initialiser les collections
  FGames := TDictionary<string, TGame>.Create;
  FPlayerGames := TDictionary<string, string>.Create;
  FLock := TCriticalSection.Create;

  // Créer le matchmaker
  FMatchmaker := TMatchmaker.Create;

  // Créer le tableau des scores
  FScoreboard := TScoreboard.Create('tictactoe.db');

  // Créer et configurer le serveur
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnExecute := OnExecute;
end;

function TGameServer.ProcessJoinGame(const APlayerID: string): string;
var
  OpponentID: string;
begin
  FLock.Acquire;
  try
    // Vérifier si le joueur est déjà dans une partie
    if FPlayerGames.TryGetValue(APlayerID, Result) then
      Exit;

    // Ajouter le joueur au matchmaker
    FMatchmaker.AddPlayer(APlayerID);

    // Essayer de trouver un adversaire
    OpponentID := FMatchmaker.FindMatch(APlayerID);

    if OpponentID <> '' then
    begin
      // Créer une nouvelle partie avec les deux joueurs
      Result := CreateGame(APlayerID, OpponentID);

      // Associer les joueurs à la partie
      FPlayerGames.Add(APlayerID, Result);
      FPlayerGames.Add(OpponentID, Result);
    end
    else
      Result := '';
  finally
    FLock.Release;
  end;
end;

procedure TGameServer.GameOver(AGame: TGame);
var
  Winner, Loser: string;
  IsTie: Boolean;
begin
  // Déterminer le résultat de la partie
  IsTie := AGame.Winner = pNone;

  if not IsTie then
  begin
    if AGame.Winner = pX then
    begin
      Winner := AGame.PlayerX;
      Loser := AGame.PlayerO;
    end
    else
    begin
      Winner := AGame.PlayerO;
      Loser := AGame.PlayerX;
    end;

    // Mettre à jour les classements
    FMatchmaker.UpdateRating(Winner, True);
    FMatchmaker.UpdateRating(Loser, False);
  end
  else
  begin
    // Match nul
    Winner := AGame.PlayerX;
    Loser := AGame.PlayerO;
  end;

  // Enregistrer le résultat dans le tableau des scores
  FScoreboard.RecordGameResult(AGame.GameID, Winner, Loser, IsTie);
end;

function TGameServer.ProcessGetScoreboard(ACount: Integer): TJSONArray;
var
  Scores: TArray<TScoreEntry>;
  Entry: TJSONObject;
  i: Integer;
begin
  // Récupérer les meilleurs joueurs
  Scores := FScoreboard.GetTopPlayers(ACount);

  // Créer un tableau JSON
  Result := TJSONArray.Create;

  // Remplir le tableau
  for i := 0 to Length(Scores) - 1 do
  begin
    Entry := TJSONObject.Create;

    Entry.AddPair('player_id', Scores[i].PlayerID);

    if Scores[i].PlayerName <> '' then
      Entry.AddPair('player_name', Scores[i].PlayerName)
    else
      Entry.AddPair('player_name', 'Joueur ' + Copy(Scores[i].PlayerID, 1, 8));

    Entry.AddPair('wins', TJSONNumber.Create(Scores[i].Wins));
    Entry.AddPair('losses', TJSONNumber.Create(Scores[i].Losses));
    Entry.AddPair('rating', TJSONNumber.Create(Scores[i].Rating));

    Result.Add(Entry);
  end;
end;
```

### Système de fichiers partagés

Pour développer un système de fichiers partagés, nous pouvons étendre les concepts vus précédemment :

#### Structure du serveur de fichiers

```pascal
type
  TFileVersion = record
    VersionID: string;
    Filename: string;
    UserID: string;
    CreatedAt: TDateTime;
    FileSize: Int64;
    Comment: string;
  end;

  TFileServer = class
  private
    FServer: TIdTCPServer;
    FDatabase: TSQLConnection;
    FStoragePath: string;
    FLock: TCriticalSection;

    procedure OnConnect(AContext: TIdContext);
    procedure OnDisconnect(AContext: TIdContext);
    procedure OnExecute(AContext: TIdContext);

    function ProcessUploadFile(const AUserID, AFilename: string;
                              const AFileData: TStream;
                              const AComment: string): Boolean;
    function ProcessDownloadFile(const AUserID, AFilename: string;
                                AVersion: Integer;
                                AResponseStream: TStream): Boolean;
    function ProcessListFiles(const AUserID: string): TJSONArray;
    function ProcessGetVersions(const AUserID, AFilename: string): TJSONArray;
    function ProcessLockFile(const AUserID, AFilename: string): Boolean;
    function ProcessUnlockFile(const AUserID, AFilename: string): Boolean;

    function GetFilePath(const AFilename, AVersionID: string): string;
    function CreateVersion(const AUserID, AFilename: string;
                          AFileSize: Int64;
                          const AComment: string): string;
  public
    constructor Create(const AStoragePath, ADatabasePath: string);
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;
  end;

constructor TFileServer.Create(const AStoragePath, ADatabasePath: string);
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FStoragePath := IncludeTrailingPathDelimiter(AStoragePath);

  // S'assurer que le répertoire de stockage existe
  ForceDirectories(FStoragePath);

  // Créer la connexion à la base de données
  FDatabase := TSQLConnection.Create(nil);
  FDatabase.DriverName := 'SQLite';
  FDatabase.Params.Values['Database'] := ADatabasePath;
  FDatabase.Connected := True;

  // Créer les tables nécessaires
  CreateTables;

  // Créer et configurer le serveur
  FServer := TIdTCPServer.Create(nil);
  FServer.OnConnect := OnConnect;
  FServer.OnDisconnect := OnDisconnect;
  FServer.OnExecute := OnExecute;
end;

procedure TFileServer.CreateTables;
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := FDatabase;

    // Table des fichiers
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS Files (' +
      '  Filename TEXT NOT NULL, ' +
      '  CurrentVersion TEXT, ' +
      '  LockedBy TEXT, ' +
      '  LockedAt DATETIME, ' +
      '  PRIMARY KEY (Filename)' +
      ')';
    Query.ExecSQL;

    // Table des versions de fichiers
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS FileVersions (' +
      '  VersionID TEXT PRIMARY KEY, ' +
      '  Filename TEXT NOT NULL, ' +
      '  UserID TEXT NOT NULL, ' +
      '  CreatedAt DATETIME NOT NULL, ' +
      '  FileSize INTEGER NOT NULL, ' +
      '  Comment TEXT, ' +
      '  FOREIGN KEY (Filename) REFERENCES Files(Filename)' +
      ')';
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TFileServer.ProcessUploadFile(const AUserID, AFilename: string;
                                    const AFileData: TStream;
                                    const AComment: string): Boolean;
var
  Query: TSQLQuery;
  FileExists: Boolean;
  IsLocked: Boolean;
  LockedBy: string;
  FilePath: string;
  FileStream: TFileStream;
  VersionID: string;
  FileSize: Int64;
begin
  Result := False;

  // Vérifier si le fichier est verrouillé par un autre utilisateur
  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Vérifier si le fichier existe déjà
      Query.SQL.Text := 'SELECT COUNT(*) FROM Files WHERE Filename = ?';
      Query.Params[0].AsString := AFilename;
      Query.Open;

      FileExists := Query.Fields[0].AsInteger > 0;
      Query.Close;

      if FileExists then
      begin
        // Vérifier si le fichier est verrouillé
        Query.SQL.Text := 'SELECT LockedBy FROM Files WHERE Filename = ?';
        Query.Params[0].AsString := AFilename;
        Query.Open;

        LockedBy := Query.FieldByName('LockedBy').AsString;
        IsLocked := LockedBy <> '';
        Query.Close;

        // Si verrouillé par un autre utilisateur, échouer
        if IsLocked and (LockedBy <> AUserID) then
          Exit;
      end;

      // Obtenir la taille du fichier
      FileSize := AFileData.Size;

      // Créer une nouvelle version
      VersionID := CreateVersion(AUserID, AFilename, FileSize, AComment);

      // Obtenir le chemin du fichier
      FilePath := GetFilePath(AFilename, VersionID);

      // Créer le répertoire si nécessaire
      ForceDirectories(ExtractFilePath(FilePath));

      // Enregistrer le fichier
      FileStream := TFileStream.Create(FilePath, fmCreate);
      try
        AFileData.Position := 0;
        FileStream.CopyFrom(AFileData, AFileData.Size);
      finally
        FileStream.Free;
      end;

      // Mettre à jour la base de données
      if FileExists then
      begin
        // Mettre à jour la version actuelle
        Query.SQL.Text := 'UPDATE Files SET CurrentVersion = ? WHERE Filename = ?';
        Query.Params[0].AsString := VersionID;
        Query.Params[1].AsString := AFilename;
      end
      else
      begin
        // Ajouter le nouveau fichier
        Query.SQL.Text := 'INSERT INTO Files (Filename, CurrentVersion) VALUES (?, ?)';
        Query.Params[0].AsString := AFilename;
        Query.Params[1].AsString := VersionID;
      end;

      Query.ExecSQL;

      Result := True;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;

function TFileServer.CreateVersion(const AUserID, AFilename: string;
                                 AFileSize: Int64;
                                 const AComment: string): string;
var
  Query: TSQLQuery;
begin
  // Générer un ID unique pour la version
  Result := CreateGUID.ToString;

  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := FDatabase;

    // Ajouter l'entrée de version
    Query.SQL.Text :=
      'INSERT INTO FileVersions (VersionID, Filename, UserID, CreatedAt, FileSize, Comment) ' +
      'VALUES (?, ?, ?, ?, ?, ?)';

    Query.Params[0].AsString := Result;
    Query.Params[1].AsString := AFilename;
    Query.Params[2].AsString := AUserID;
    Query.Params[3].AsDateTime := Now;
    Query.Params[4].AsLargeInt := AFileSize;
    Query.Params[5].AsString := AComment;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TFileServer.ProcessLockFile(const AUserID, AFilename: string): Boolean;
var
  Query: TSQLQuery;
  IsLocked: Boolean;
  LockedBy: string;
begin
  Result := False;

  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Vérifier si le fichier est déjà verrouillé
      Query.SQL.Text := 'SELECT LockedBy FROM Files WHERE Filename = ?';
      Query.Params[0].AsString := AFilename;
      Query.Open;

      if not Query.Eof then
      begin
        LockedBy := Query.FieldByName('LockedBy').AsString;
        IsLocked := LockedBy <> '';
        Query.Close;

        // Si déjà verrouillé par un autre utilisateur, échouer
        if IsLocked and (LockedBy <> AUserID) then
          Exit;

        // Si déjà verrouillé par cet utilisateur, réussir
        if IsLocked and (LockedBy = AUserID) then
        begin
          Result := True;
          Exit;
        end;

        // Verrouiller le fichier
        Query.SQL.Text := 'UPDATE Files SET LockedBy = ?, LockedAt = ? WHERE Filename = ?';
        Query.Params[0].AsString := AUserID;
        Query.Params[1].AsDateTime := Now;
        Query.Params[2].AsString := AFilename;

        Query.ExecSQL;

        Result := True;
      end;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;
```

## Système de réservation (suite)

Pour le système de réservation, nous pouvons ajouter des fonctionnalités de notification temps réel avancées :

#### Notification par WebSocket

```pascal
type
  TNotificationType = (ntReservationConfirmed, ntReservationCancelled,
                      ntReservationReminder, ntResourceAvailable, ntSystemMessage);

  TNotification = class
  private
    FID: Integer;
    FUserID: Integer;
    FType: TNotificationType;
    FMessage: string;
    FDetails: TJSONObject;
    FCreatedAt: TDateTime;
    FRead: Boolean;
  public
    constructor Create(AUserID: Integer; AType: TNotificationType;
                     const AMessage: string; ADetails: TJSONObject = nil);
    destructor Destroy; override;

    function ToJSON: TJSONObject;

    property ID: Integer read FID write FID;
    property UserID: Integer read FUserID;
    property NotificationType: TNotificationType read FType;
    property Message: string read FMessage;
    property Details: TJSONObject read FDetails;
    property CreatedAt: TDateTime read FCreatedAt;
    property Read: Boolean read FRead write FRead;
  end;

  TWebSocketNotificationServer = class
  private
    FServer: TIdWebsocketServer;
    FClients: TDictionary<Integer, TList<TIdContext>>;
    FLock: TCriticalSection;
    FNotificationQueue: TThreadedQueue<TNotification>;
    FProcessingThread: TThread;

    procedure ProcessNotificationQueue;
    procedure OnClientConnect(AContext: TIdContext);
    procedure OnClientDisconnect(AContext: TIdContext);
    procedure OnClientMessage(AContext: TIdContext; const AMessage: string);
    procedure RegisterClient(AUserID: Integer; AContext: TIdContext);
    procedure SendToClient(AContext: TIdContext; ANotification: TNotification);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(APort: Integer);
    procedure Stop;

    procedure EnqueueNotification(ANotification: TNotification);
    procedure BroadcastNotification(AType: TNotificationType; const AMessage: string);
    procedure SendNotificationToUser(AUserID: Integer; AType: TNotificationType;
                                   const AMessage: string; ADetails: TJSONObject = nil);
  end;

constructor TNotification.Create(AUserID: Integer; AType: TNotificationType;
                               const AMessage: string; ADetails: TJSONObject = nil);
begin
  inherited Create;

  FUserID := AUserID;
  FType := AType;
  FMessage := AMessage;
  FDetails := ADetails;
  FCreatedAt := Now;
  FRead := False;
end;

function TNotification.ToJSON: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('id', TJSONNumber.Create(FID));
  Result.AddPair('user_id', TJSONNumber.Create(FUserID));
  Result.AddPair('type', TJSONNumber.Create(Ord(FType)));
  Result.AddPair('message', FMessage);
  Result.AddPair('created_at', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', FCreatedAt));
  Result.AddPair('read', TJSONBool.Create(FRead));

  if Assigned(FDetails) then
    Result.AddPair('details', FDetails.Clone as TJSONObject);
end;

constructor TWebSocketNotificationServer.Create;
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FClients := TDictionary<Integer, TList<TIdContext>>.Create;

  // Créer la file d'attente des notifications
  FNotificationQueue := TThreadedQueue<TNotification>.Create(1000, INFINITE, INFINITE);

  // Créer le serveur WebSocket
  FServer := TIdWebsocketServer.Create(nil);
  FServer.OnConnect := OnClientConnect;
  FServer.OnDisconnect := OnClientDisconnect;
  FServer.OnMessage := OnClientMessage;

  // Démarrer le thread de traitement des notifications
  FProcessingThread := TThread.CreateAnonymousThread(ProcessNotificationQueue);
  FProcessingThread.Start;
end;

procedure TWebSocketNotificationServer.ProcessNotificationQueue;
var
  Notification: TNotification;
  Clients: TList<TIdContext>;
  Context: TIdContext;
  i: Integer;
begin
  while not TThread.CheckTerminated do
  begin
    if FNotificationQueue.PopItem(Notification) = wrSignaled then
    try
      FLock.Acquire;
      try
        // Récupérer les clients de l'utilisateur
        if FClients.TryGetValue(Notification.UserID, Clients) then
        begin
          // Envoyer la notification à tous les clients de l'utilisateur
          for i := 0 to Clients.Count - 1 do
          begin
            Context := Clients[i];
            SendToClient(Context, Notification);
          end;
        end;
      finally
        FLock.Release;
      end;
    finally
      Notification.Free;
    end;
  end;
end;

procedure TWebSocketNotificationServer.SendToClient(AContext: TIdContext; ANotification: TNotification);
var
  JSONObject: TJSONObject;
  JSONString: string;
begin
  // Convertir la notification en JSON
  JSONObject := ANotification.ToJSON;
  try
    JSONString := JSONObject.ToString;

    // Envoyer au client via WebSocket
    try
      AContext.Connection.IOHandler.WriteLn(JSONString);
    except
      // Ignorer les erreurs d'envoi
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TWebSocketNotificationServer.SendNotificationToUser(AUserID: Integer; AType: TNotificationType;
                                                           const AMessage: string; ADetails: TJSONObject = nil);
var
  Notification: TNotification;
begin
  // Créer la notification
  Notification := TNotification.Create(AUserID, AType, AMessage, ADetails);

  // Ajouter à la file d'attente
  EnqueueNotification(Notification);

  // Note: L'objet sera libéré après traitement dans le thread
end;

procedure TWebSocketNotificationServer.OnClientMessage(AContext: TIdContext; const AMessage: string);
var
  JSONObject: TJSONObject;
  Command, Token: string;
  UserID: Integer;
begin
  try
    // Parser le message JSON
    JSONObject := TJSONObject.ParseJSONValue(AMessage) as TJSONObject;
    try
      // Extraire la commande
      Command := JSONObject.GetValue('command').Value;

      // Traiter la commande
      if Command = 'register' then
      begin
        // Authentifier l'utilisateur via le token
        Token := JSONObject.GetValue('token').Value;
        UserID := AuthenticateToken(Token);

        if UserID > 0 then
        begin
          // Enregistrer le client
          RegisterClient(UserID, AContext);

          // Envoyer confirmation
          AContext.Connection.IOHandler.WriteLn('{"status":"registered"}');
        end
        else
        begin
          // Authentification échouée
          AContext.Connection.IOHandler.WriteLn('{"status":"error","message":"Invalid token"}');
        end;
      end
      else if Command = 'mark_read' then
      begin
        // Marquer une notification comme lue
        // (Implémentation omise pour simplifier)
      end;
    finally
      JSONObject.Free;
    end;
  except
    // Ignorer les messages invalides
  end;
end;

procedure TWebSocketNotificationServer.RegisterClient(AUserID: Integer; AContext: TIdContext);
var
  Clients: TList<TIdContext>;
begin
  FLock.Acquire;
  try
    // Récupérer ou créer la liste des clients pour cet utilisateur
    if not FClients.TryGetValue(AUserID, Clients) then
    begin
      Clients := TList<TIdContext>.Create;
      FClients.Add(AUserID, Clients);
    end;

    // Ajouter ce client à la liste
    if Clients.IndexOf(AContext) < 0 then
      Clients.Add(AContext);

    // Stocker l'ID utilisateur dans le contexte
    TUserData(AContext.Data).UserID := AUserID;
  finally
    FLock.Release;
  end;
end;
```

#### Intégration des notifications dans le système de réservation

```pascal
type
  TReservationManager = class
  private
    FDatabase: TSQLConnection;
    FLock: TCriticalSection;
    FNotificationServer: TWebSocketNotificationServer;

    function IsResourceAvailable(AResourceID: Integer; AStartTime, AEndTime: TDateTime): Boolean;
    procedure SendReservationNotification(AReservationID: Integer; AType: TNotificationType);
  public
    constructor Create(ADatabase: TSQLConnection; ANotificationServer: TWebSocketNotificationServer);
    destructor Destroy; override;

    function MakeReservation(AResourceID, AUserID: Integer; AStartTime, AEndTime: TDateTime;
                           ANumPeople: Integer; const ANotes: string): Integer;
    function CancelReservation(AReservationID, AUserID: Integer): Boolean;
    function GetAvailableResources(AStartTime, AEndTime: TDateTime; ACapacity: Integer): TObjectList<TResource>;
    procedure SendReservationReminders;
  end;

function TReservationManager.MakeReservation(AResourceID, AUserID: Integer; AStartTime, AEndTime: TDateTime;
                                          ANumPeople: Integer; const ANotes: string): Integer;
var
  Query: TSQLQuery;
  ReservationID: Integer;
  ResourceName: string;
  Details: TJSONObject;
begin
  Result := -1;

  // Vérifier les paramètres
  if (AResourceID <= 0) or (AUserID <= 0) or
     (AStartTime >= AEndTime) or (ANumPeople <= 0) then
    Exit;

  FLock.Acquire;
  try
    // Vérifier la disponibilité
    if not IsResourceAvailable(AResourceID, AStartTime, AEndTime) then
      Exit;

    // Créer la réservation
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Insérer la réservation
      Query.SQL.Text := 'INSERT INTO Reservations ' +
                        '(ResourceID, UserID, StartTime, EndTime, NumPeople, Notes, Status, CreatedAt) ' +
                        'VALUES (?, ?, ?, ?, ?, ?, ?, ?)';

      Query.Params[0].AsInteger := AResourceID;
      Query.Params[1].AsInteger := AUserID;
      Query.Params[2].AsDateTime := AStartTime;
      Query.Params[3].AsDateTime := AEndTime;
      Query.Params[4].AsInteger := ANumPeople;
      Query.Params[5].AsString := ANotes;
      Query.Params[6].AsString := 'Confirmed';
      Query.Params[7].AsDateTime := Now;

      Query.ExecSQL;

      // Récupérer l'ID de la réservation
      Query.SQL.Text := 'SELECT last_insert_rowid()';
      Query.Open;
      ReservationID := Query.Fields[0].AsInteger;
      Query.Close;

      // Récupérer le nom de la ressource
      Query.SQL.Text := 'SELECT ResourceName FROM Resources WHERE ResourceID = ?';
      Query.Params[0].AsInteger := AResourceID;
      Query.Open;
      ResourceName := Query.FieldByName('ResourceName').AsString;
      Query.Close;

      Result := ReservationID;

      // Créer les détails pour la notification
      Details := TJSONObject.Create;
      Details.AddPair('reservation_id', TJSONNumber.Create(ReservationID));
      Details.AddPair('resource_id', TJSONNumber.Create(AResourceID));
      Details.AddPair('resource_name', ResourceName);
      Details.AddPair('start_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', AStartTime));
      Details.AddPair('end_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', AEndTime));

      // Envoyer une notification de confirmation
      FNotificationServer.SendNotificationToUser(
        AUserID,
        ntReservationConfirmed,
        Format('Votre réservation de "%s" a été confirmée pour le %s',
               [ResourceName, FormatDateTime('dd/mm/yyyy à hh:nn', AStartTime)]),
        Details);
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TReservationManager.SendReservationReminders;
var
  Query: TSQLQuery;
  ReservationID, UserID, ResourceID: Integer;
  StartTime: TDateTime;
  ResourceName: string;
  Details: TJSONObject;
  NowPlus24h: TDateTime;
begin
  // Rappels 24h avant la réservation
  NowPlus24h := Now + 1.0;  // +1 jour

  FLock.Acquire;
  try
    Query := TSQLQuery.Create(nil);
    try
      Query.SQLConnection := FDatabase;

      // Trouver les réservations à venir dans les prochaines 24h
      Query.SQL.Text :=
        'SELECT R.ReservationID, R.UserID, R.ResourceID, R.StartTime, RS.ResourceName ' +
        'FROM Reservations R ' +
        'JOIN Resources RS ON R.ResourceID = RS.ResourceID ' +
        'WHERE R.Status = ''Confirmed'' ' +
        'AND R.StartTime BETWEEN ? AND ? ' +
        'AND R.ReservationID NOT IN (SELECT ReservationID FROM NotificationsSent WHERE Type = ?)';

      Query.Params[0].AsDateTime := Now;
      Query.Params[1].AsDateTime := NowPlus24h;
      Query.Params[2].AsInteger := Ord(ntReservationReminder);

      Query.Open;

      // Envoyer un rappel pour chaque réservation
      while not Query.Eof do
      begin
        ReservationID := Query.FieldByName('ReservationID').AsInteger;
        UserID := Query.FieldByName('UserID').AsInteger;
        ResourceID := Query.FieldByName('ResourceID').AsInteger;
        StartTime := Query.FieldByName('StartTime').AsDateTime;
        ResourceName := Query.FieldByName('ResourceName').AsString;

        // Créer les détails pour la notification
        Details := TJSONObject.Create;
        Details.AddPair('reservation_id', TJSONNumber.Create(ReservationID));
        Details.AddPair('resource_id', TJSONNumber.Create(ResourceID));
        Details.AddPair('resource_name', ResourceName);
        Details.AddPair('start_time', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', StartTime));

        // Envoyer le rappel
        FNotificationServer.SendNotificationToUser(
          UserID,
          ntReservationReminder,
          Format('Rappel: Vous avez réservé "%s" pour demain à %s',
                 [ResourceName, FormatDateTime('hh:nn', StartTime)]),
          Details);

        // Marquer comme envoyé
        MarkNotificationSent(ReservationID, ntReservationReminder);

        Query.Next;
      end;
    finally
      Query.Free;
    end;
  finally
    FLock.Release;
  end;
end;

procedure TReservationManager.MarkNotificationSent(AReservationID: Integer; AType: TNotificationType);
var
  Query: TSQLQuery;
begin
  Query := TSQLQuery.Create(nil);
  try
    Query.SQLConnection := FDatabase;

    // Ajouter l'entrée
    Query.SQL.Text := 'INSERT INTO NotificationsSent (ReservationID, Type, SentAt) VALUES (?, ?, ?)';
    Query.Params[0].AsInteger := AReservationID;
    Query.Params[1].AsInteger := Ord(AType);
    Query.Params[2].AsDateTime := Now;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

#### Interface utilisateur avec notifications en temps réel

```pascal
type
  TFormReservation = class(TForm)
    PageControl: TPageControl;
    TabReservations: TTabSheet;
    TabCalendar: TTabSheet;
    TabResources: TTabSheet;
    PanelTop: TPanel;
    BtnNewReservation: TButton;
    GridReservations: TStringGrid;
    CalendarView: TCalendarView;
    ListBoxResources: TListBox;
    PanelNotification: TPanel;
    LabelNotification: TLabel;
    BtnCloseNotification: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnNewReservationClick(Sender: TObject);
    procedure BtnCloseNotificationClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FClient: TReservationClient;
    FWebSocketClient: TIdWebSocketClient;
    FNotifications: TObjectList<TNotification>;
    FThread: TThread;
    FConnected: Boolean;

    procedure LoadReservations;
    procedure LoadResources;
    procedure UpdateCalendar;
    procedure OnNotificationReceived(Sender: TObject; const ANotification: string);
    procedure ProcessNotification(const ANotificationJSON: string);
    procedure ShowNotification(const AMessage: string);
    procedure ConnectWebSocket;
    procedure DisconnectWebSocket;
  end;

procedure TFormReservation.FormCreate(Sender: TObject);
begin
  // Initialiser
  FClient := TReservationClient.Create;
  FNotifications := TObjectList<TNotification>.Create(True);
  FConnected := False;

  // Cacher le panneau de notification
  PanelNotification.Visible := False;

  // Connecter au serveur
  if not FClient.Connect('localhost', 8080) then
  begin
    ShowMessage('Impossible de se connecter au serveur de réservation.');
    Exit;
  end;

  // Charger les données
  LoadReservations;
  LoadResources;
  UpdateCalendar;

  // Connecter aux notifications
  ConnectWebSocket;
end;

procedure TFormReservation.ConnectWebSocket;
begin
  // Créer le client WebSocket
  FWebSocketClient := TIdWebSocketClient.Create(nil);

  // Configurer les événements
  FWebSocketClient.OnMessage := OnNotificationReceived;

  // Créer un thread pour connecter et maintenir la connexion
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        // Se connecter au serveur WebSocket
        FWebSocketClient.Connect('ws://localhost:8081');

        // Si connexion réussie, s'enregistrer
        if FWebSocketClient.Connected then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              FConnected := True;
            end);

          // Envoyer la commande d'enregistrement avec le token d'authentification
          FWebSocketClient.SendText(Format('{"command":"register","token":"%s"}',
                                          [FClient.AuthToken]));

          // Boucle pour maintenir la connexion
          while not TThread.CheckTerminated and FWebSocketClient.Connected do
          begin
            // Envoyer un ping périodique pour maintenir la connexion
            if FWebSocketClient.Connected then
              FWebSocketClient.SendText('{"command":"ping"}');

            // Attendre 30 secondes
            Sleep(30000);
          end;
        end;
      except
        // Gérer les erreurs de connexion
        TThread.Synchronize(nil,
          procedure
          begin
            FConnected := False;
          end);
      end;
    end);

  // Démarrer le thread
  FThread.Start;
end;

procedure TFormReservation.OnNotificationReceived(Sender: TObject; const ANotification: string);
begin
  // Traiter la notification dans le thread principal
  TThread.Queue(nil,
    procedure
    begin
      ProcessNotification(ANotification);
    end);
end;

procedure TFormReservation.ProcessNotification(const ANotificationJSON: string);
var
  JSONObject: TJSONObject;
  NotificationType: Integer;
  Message: string;
  Notification: TNotification;
begin
  try
    // Parser le JSON
    JSONObject := TJSONObject.ParseJSONValue(ANotificationJSON) as TJSONObject;
    try
      // Extraire les informations
      NotificationType := JSONObject.GetValue<Integer>('type');
      Message := JSONObject.GetValue<string>('message');

      // Créer l'objet de notification
      Notification := TNotification.Create;
      Notification.ID := JSONObject.GetValue<Integer>('id');
      Notification.Message := Message;
      Notification.NotificationType := TNotificationType(NotificationType);

      // Ajouter à la liste
      FNotifications.Add(Notification);

      // Afficher la notification
      ShowNotification(Message);

      // Si c'est une notification de réservation, mettre à jour la vue
      if (NotificationType = Ord(ntReservationConfirmed)) or
         (NotificationType = Ord(ntReservationCancelled)) then
      begin
        // Recharger les réservations
        LoadReservations;
        UpdateCalendar;
      end;
    finally
      JSONObject.Free;
    end;
  except
    // Ignorer les erreurs de parsing
  end;
end;

procedure TFormReservation.ShowNotification(const AMessage: string);
begin
  // Afficher le message
  LabelNotification.Caption := AMessage;

  // Afficher le panneau
  PanelNotification.Visible := True;

  // Optionnellement, ajouter un son ou une animation
  PlaySound('notification.wav', 0, SND_ASYNC);
end;

procedure TFormReservation.BtnCloseNotificationClick(Sender: TObject);
begin
  // Cacher le panneau de notification
  PanelNotification.Visible := False;
end;
```

## Structure du tutoriel complet

Voici une structure résumée du tutoriel complet sur l'architecture client-serveur que nous avons développé :

### 1. Introduction à l'architecture client-serveur
- Définition et concepts de base
- Types d'architectures (2-tier, 3-tier, n-tier)
- Avantages et inconvénients

### 2. Composants essentiels
- Protocoles de communication
- Mécanismes de transport
- Formats de données
- Logique serveur et client

### 3. Exemples d'implémentation
- Application de chat simple
- Gestion d'inventaire
- Jeu multijoueur
- Système de réservation
- Monitoring distribué

### 4. Modèles avancés
- Microservices
- API Gateway
- Circuit Breaker
- Cache distribué

### 5. Traitement de la concurrence
- Multithreading
- Pools de threads
- Synchronisation
- Gestion des verrous

### 6. Sécurité
- Authentification et autorisation
- Protection contre les attaques courantes
- Chiffrement des communications
- Gestion des tokens

### 7. Optimisation des performances
- Stratégies côté serveur
- Stratégies côté client
- Mise en cache
- Traitement asynchrone

### 8. Gestion des erreurs et résilience
- Gestion des erreurs côté serveur
- Gestion des erreurs côté client
- Reconnexion automatique
- Circuit Breaker

### 9. Notifications en temps réel
- WebSockets
- Système de notification
- Intégration dans les applications client-serveur

### 10. Déploiement et configuration
- Packaging des applications
- Configuration centralisée
- Monitoring et journalisation

## Conclusion

L'architecture client-serveur est un modèle puissant et flexible pour créer des applications distribuées. Elle permet de séparer clairement les responsabilités entre les différents composants du système, facilitant ainsi la maintenance, l'évolution et la mise à l'échelle.

À travers ce tutoriel, nous avons exploré diverses approches pour implémenter cette architecture, des plus simples aux plus avancées. Nous avons également abordé des aspects critiques comme la sécurité, la performance, la gestion des erreurs et les notifications en temps réel.

La mise en œuvre de ces concepts demande une bonne compréhension des mécanismes sous-jacents, mais offre en retour des applications robustes, évolutives et réactives. Les exercices pratiques proposés vous permettront d'appliquer ces connaissances et de développer vos propres solutions client-serveur adaptées à vos besoins spécifiques.

N'oubliez pas que la conception d'une architecture client-serveur est un processus itératif. Commencez simplement, puis évoluez progressivement en fonction des retours d'utilisation et des nouveaux besoins qui apparaîtront.

## Ressources complémentaires

Pour approfondir vos connaissances sur l'architecture client-serveur, voici quelques ressources utiles :

- **Livres**
  - "Pattern-Oriented Software Architecture: Patterns for Concurrent and Networked Objects"
  - "Building Microservices: Designing Fine-Grained Systems"
  - "Enterprise Integration Patterns"

- **Bibliothèques**
  - Indy (pour Delphi/Pascal)
  - Spring4D (pour Delphi/Pascal)
  - SuperObject/JsonDataObjects (pour le traitement JSON)

- **Outils**
  - Postman (pour tester les API)
  - Wireshark (pour analyser le trafic réseau)
  - JMeter (pour les tests de charge)

Avec ces connaissances et ces outils, vous êtes maintenant prêt à concevoir et implémenter vos propres applications client-serveur de manière professionnelle et efficace.

⏭️ [Applications distribuées](/10-communication-et-services-reseaux/06-applications-distribuees.md)
