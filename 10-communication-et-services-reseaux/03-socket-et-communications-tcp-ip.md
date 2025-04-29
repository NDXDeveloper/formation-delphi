# 10.3 Socket et communications TCP/IP

## Introduction

Les sockets constituent la base des communications réseau modernes. Ils permettent à deux applications, qu'elles soient sur le même ordinateur ou sur des machines distantes, de communiquer entre elles via le réseau. Dans ce chapitre, nous allons explorer les bases des sockets et des communications TCP/IP, et comment les implémenter dans vos applications.

## Comprendre les bases

### Qu'est-ce qu'un socket?

Un socket est un point de terminaison dans une communication bidirectionnelle entre deux programmes à travers un réseau. C'est comme une "prise" virtuelle à laquelle on peut se connecter pour échanger des données.

### TCP/IP en bref

Le protocole TCP/IP (Transmission Control Protocol/Internet Protocol) est l'ensemble des règles qui gouvernent les communications sur Internet. Il s'assure que les données sont transmises de manière fiable et dans le bon ordre.

- **TCP** : Assure que les données sont transmises sans erreur et dans l'ordre correct
- **IP** : S'occupe de l'adressage et du routage des paquets de données

### Différence entre TCP et UDP

Il existe deux types principaux de sockets:

1. **Sockets TCP** (Transmission Control Protocol)
   - Communication fiable et ordonnée
   - Vérification d'erreurs et retransmission automatique
   - Parfait pour les applications nécessitant l'intégrité des données (transfert de fichiers, web, email)

2. **Sockets UDP** (User Datagram Protocol)
   - Communication plus rapide mais moins fiable
   - Pas de garantie d'ordre ou de livraison
   - Idéal pour les applications où la vitesse est prioritaire (jeux en ligne, streaming vidéo)

Dans ce tutoriel, nous nous concentrerons principalement sur les sockets TCP, car ils sont plus couramment utilisés et plus simples à comprendre pour les débutants.

## Architecture Client-Serveur

La plupart des communications socket suivent le modèle client-serveur:

- **Serveur**: Attend passivement les connexions des clients
- **Client**: Initie activement la communication avec le serveur

### Processus de communication

1. Le serveur crée un socket et l'associe à un port spécifique
2. Le serveur écoute les connexions entrantes
3. Le client crée un socket et se connecte au serveur
4. Une fois connectés, les deux peuvent envoyer et recevoir des données
5. À la fin, la connexion est fermée

## Implémenter un serveur TCP simple

Voyons comment créer un serveur TCP basique:

```pascal
procedure DemarrerServeur;
var
  ServerSocket: TTcpServer;
  ClientSocket: TTcpClient;
  Buffer: array[0..1023] of Byte;
  BytesLus: Integer;
  Reponse: string;
begin
  // Créer le socket serveur
  ServerSocket := TTcpServer.Create(nil);
  try
    // Configurer le serveur
    ServerSocket.LocalPort := '8080';  // Choisir un port disponible
    ServerSocket.Active := True;       // Démarrer l'écoute

    Memo1.Lines.Add('Serveur démarré. En attente de connexions sur le port 8080...');

    // Attendre et accepter une connexion cliente
    ClientSocket := ServerSocket.Accept;
    try
      Memo1.Lines.Add('Client connecté!');

      // Lire les données envoyées par le client
      BytesLus := ClientSocket.ReceiveBuf(Buffer, SizeOf(Buffer));

      if BytesLus > 0 then
      begin
        // Convertir les octets reçus en chaîne de caractères
        SetString(Reponse, PAnsiChar(@Buffer), BytesLus);
        Memo1.Lines.Add('Message reçu: ' + Reponse);

        // Répondre au client
        Reponse := 'Message bien reçu!';
        ClientSocket.SendBuf(PAnsiChar(Reponse)^, Length(Reponse));
      end;

    finally
      // Fermer la connexion cliente
      ClientSocket.Free;
    end;

  finally
    // Fermer le serveur
    ServerSocket.Active := False;
    ServerSocket.Free;
  end;
end;
```

## Implémenter un client TCP simple

Maintenant, créons un client qui se connecte à notre serveur:

```pascal
procedure ConnecterAuServeur;
var
  ClientSocket: TTcpClient;
  Buffer: array[0..1023] of Byte;
  BytesLus: Integer;
  Message, Reponse: string;
begin
  // Créer le socket client
  ClientSocket := TTcpClient.Create(nil);
  try
    // Configurer et se connecter au serveur
    ClientSocket.RemoteHost := '127.0.0.1';  // Adresse locale (localhost)
    ClientSocket.RemotePort := '8080';       // Même port que le serveur

    try
      // Établir la connexion
      ClientSocket.Connect;
      Memo1.Lines.Add('Connecté au serveur!');

      // Envoyer un message
      Message := EditMessage.Text;
      ClientSocket.SendBuf(PAnsiChar(Message)^, Length(Message));

      // Attendre et lire la réponse
      BytesLus := ClientSocket.ReceiveBuf(Buffer, SizeOf(Buffer));

      if BytesLus > 0 then
      begin
        // Convertir les octets reçus en chaîne de caractères
        SetString(Reponse, PAnsiChar(@Buffer), BytesLus);
        Memo1.Lines.Add('Réponse du serveur: ' + Reponse);
      end;

    except
      on E: Exception do
        ShowMessage('Erreur de connexion: ' + E.Message);
    end;

  finally
    // Fermer la connexion
    ClientSocket.Free;
  end;
end;
```

## Traitement asynchrone avec des événements

Les exemples précédents sont synchrones, ce qui signifie qu'ils bloquent l'exécution jusqu'à ce qu'une opération soit terminée. Pour une application plus réactive, nous pouvons utiliser des événements:

### Serveur TCP avec événements

```pascal
// Dans la section private de la fiche
private
  FServerSocket: TTcpServer;

// Dans le constructeur ou FormCreate
procedure TForm1.FormCreate(Sender: TObject);
begin
  FServerSocket := TTcpServer.Create(Self);
  FServerSocket.LocalPort := '8080';
  FServerSocket.OnAccept := ServerAcceptClient;
  FServerSocket.Active := True;

  Memo1.Lines.Add('Serveur démarré sur le port 8080...');
end;

// Gestionnaire d'événement pour les connexions entrantes
procedure TForm1.ServerAcceptClient(Sender: TObject; ClientSocket: TTcpClient);
var
  ClientHandler: TClientHandler;
begin
  Memo1.Lines.Add('Nouveau client connecté!');

  // Créer un gestionnaire spécifique pour ce client
  ClientHandler := TClientHandler.Create(ClientSocket);
  ClientHandler.OnReceiveData := ClientDataReceived;
  ClientHandler.OnDisconnect := ClientDisconnected;

  // Le gestionnaire s'exécute dans un thread séparé
  ClientHandler.Start;
end;

// Gestionnaire pour les données reçues d'un client
procedure TForm1.ClientDataReceived(Sender: TObject; const Data: string);
begin
  // Synchronise l'affichage avec le thread principal
  TThread.Synchronize(nil, procedure
  begin
    Memo1.Lines.Add('Message reçu: ' + Data);
  end);

  // Traiter les données et envoyer une réponse
  TClientHandler(Sender).SendData('Votre message de ' + Length(Data) +
                                 ' caractères a été reçu.');
end;

// Gestionnaire pour la déconnexion d'un client
procedure TForm1.ClientDisconnected(Sender: TObject);
begin
  TThread.Synchronize(nil, procedure
  begin
    Memo1.Lines.Add('Un client s\'est déconnecté');
  end);
end;
```

### Classe gestionnaire de client

```pascal
// Définition de la classe TClientHandler
TClientHandler = class(TThread)
private
  FClientSocket: TTcpClient;
  FBuffer: array[0..4095] of Byte;
  FOnReceiveData: TReceiveDataEvent;
  FOnDisconnect: TNotifyEvent;
protected
  procedure Execute; override;
public
  constructor Create(ClientSocket: TTcpClient);
  destructor Destroy; override;
  procedure SendData(const Data: string);

  property OnReceiveData: TReceiveDataEvent read FOnReceiveData write FOnReceiveData;
  property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
end;

// Implémentation
constructor TClientHandler.Create(ClientSocket: TTcpClient);
begin
  inherited Create(False);
  FClientSocket := ClientSocket;
  FreeOnTerminate := True;
end;

destructor TClientHandler.Destroy;
begin
  FClientSocket.Free;
  inherited;
end;

procedure TClientHandler.Execute;
var
  BytesLus: Integer;
  DataStr: string;
begin
  while not Terminated do
  try
    // Lire les données du client
    BytesLus := FClientSocket.ReceiveBuf(FBuffer, SizeOf(FBuffer));

    if BytesLus <= 0 then
    begin
      // Connexion fermée
      if Assigned(FOnDisconnect) then
        FOnDisconnect(Self);
      Break;
    end;

    // Traiter les données reçues
    SetString(DataStr, PAnsiChar(@FBuffer), BytesLus);

    if Assigned(FOnReceiveData) then
      FOnReceiveData(Self, DataStr);

  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Break;
    end;
  end;
end;

procedure TClientHandler.SendData(const Data: string);
begin
  if Assigned(FClientSocket) and FClientSocket.Connected then
    FClientSocket.SendBuf(PAnsiChar(Data)^, Length(Data));
end;
```

### Client TCP avec événements

```pascal
// Dans la section private de la fiche
private
  FClientSocket: TTcpClient;

// Connexion au serveur
procedure TForm1.BtnConnecterClick(Sender: TObject);
begin
  FClientSocket := TTcpClient.Create(Self);
  FClientSocket.RemoteHost := EditIP.Text;
  FClientSocket.RemotePort := EditPort.Text;

  try
    FClientSocket.Connect;

    // Configurer les gestionnaires d'événements
    FClientSocket.OnDisconnect := ClientDisconnected;
    FClientSocket.OnReceive := ClientDataReceived;

    Memo1.Lines.Add('Connecté au serveur!');
    BtnEnvoyer.Enabled := True;
    BtnConnecter.Enabled := False;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion: ' + E.Message);
      FClientSocket.Free;
      FClientSocket := nil;
    end;
  end;
end;

// Envoi de message
procedure TForm1.BtnEnvoyerClick(Sender: TObject);
var
  Message: string;
begin
  if not Assigned(FClientSocket) or not FClientSocket.Connected then
  begin
    ShowMessage('Non connecté au serveur');
    Exit;
  end;

  Message := EditMessage.Text;
  FClientSocket.SendBuf(PAnsiChar(Message)^, Length(Message));

  Memo1.Lines.Add('Envoyé: ' + Message);
  EditMessage.Clear;
end;

// Événement de réception de données
procedure TForm1.ClientDataReceived(Sender: TObject);
var
  Buffer: array[0..1023] of Byte;
  BytesLus: Integer;
  Reponse: string;
begin
  BytesLus := FClientSocket.ReceiveBuf(Buffer, SizeOf(Buffer));

  if BytesLus > 0 then
  begin
    SetString(Reponse, PAnsiChar(@Buffer), BytesLus);
    Memo1.Lines.Add('Reçu: ' + Reponse);
  end;
end;

// Événement de déconnexion
procedure TForm1.ClientDisconnected(Sender: TObject);
begin
  Memo1.Lines.Add('Déconnecté du serveur');
  BtnConnecter.Enabled := True;
  BtnEnvoyer.Enabled := False;
end;

// Nettoyage lors de la fermeture
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FClientSocket) then
  begin
    FClientSocket.Close;
    FClientSocket.Free;
  end;
end;
```

## Application de chat simple

Combinons tout ce que nous avons appris pour créer une application de chat simple:

### Serveur de chat

```pascal
unit ChatServer;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls, System.Generics.Collections,
  System.SyncObjs;

type
  TClientConnection = class;

  TMessageEvent = procedure(Sender: TObject; const ClientName, Message: string) of object;
  TClientEvent = procedure(Sender: TObject; Client: TClientConnection) of object;

  TChatServer = class
  private
    FServerSocket: TTcpServer;
    FClients: TList<TClientConnection>;
    FLock: TCriticalSection;
    FOnClientConnected: TClientEvent;
    FOnClientDisconnected: TClientEvent;
    FOnMessageReceived: TMessageEvent;

    procedure AcceptClient(Sender: TObject; ClientSocket: TTcpClient);
    procedure RemoveClient(Client: TClientConnection);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start(Port: Integer);
    procedure Stop;
    procedure BroadcastMessage(const SenderName, Message: string);

    property OnClientConnected: TClientEvent read FOnClientConnected write FOnClientConnected;
    property OnClientDisconnected: TClientEvent read FOnClientDisconnected write FOnClientDisconnected;
    property OnMessageReceived: TMessageEvent read FOnMessageReceived write FOnMessageReceived;
  end;

  TClientConnection = class(TThread)
  private
    FOwner: TChatServer;
    FClientSocket: TTcpClient;
    FClientName: string;
    FBuffer: array[0..2047] of Byte;

    procedure ProcessCommand(const Command: string);
  protected
    procedure Execute; override;
  public
    constructor Create(Owner: TChatServer; ClientSocket: TTcpClient);
    destructor Destroy; override;

    procedure SendMessage(const Message: string);

    property ClientName: string read FClientName;
  end;

implementation

{ TChatServer }

constructor TChatServer.Create;
begin
  inherited Create;
  FClients := TList<TClientConnection>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TChatServer.Destroy;
begin
  Stop;
  FClients.Free;
  FLock.Free;
  inherited;
end;

procedure TChatServer.Start(Port: Integer);
begin
  // Arrêter le serveur s'il est déjà en cours
  Stop;

  // Créer et configurer le socket serveur
  FServerSocket := TTcpServer.Create(nil);
  FServerSocket.LocalPort := IntToStr(Port);
  FServerSocket.OnAccept := AcceptClient;
  FServerSocket.Active := True;
end;

procedure TChatServer.Stop;
begin
  if Assigned(FServerSocket) then
  begin
    FServerSocket.Active := False;
    FServerSocket.Free;
    FServerSocket := nil;
  end;

  // Déconnecter tous les clients
  FLock.Acquire;
  try
    while FClients.Count > 0 do
    begin
      FClients[0].Terminate;
      FClients[0].WaitFor;
      FClients.Delete(0);
    end;
  finally
    FLock.Release;
  end;
end;

procedure TChatServer.AcceptClient(Sender: TObject; ClientSocket: TTcpClient);
var
  NewClient: TClientConnection;
begin
  // Créer un gestionnaire pour le nouveau client
  NewClient := TClientConnection.Create(Self, ClientSocket);

  // Ajouter à la liste des clients
  FLock.Acquire;
  try
    FClients.Add(NewClient);
  finally
    FLock.Release;
  end;

  // Déclencher l'événement
  if Assigned(FOnClientConnected) then
    FOnClientConnected(Self, NewClient);
end;

procedure TChatServer.RemoveClient(Client: TClientConnection);
begin
  FLock.Acquire;
  try
    FClients.Remove(Client);
  finally
    FLock.Release;
  end;

  // Déclencher l'événement
  if Assigned(FOnClientDisconnected) then
    FOnClientDisconnected(Self, Client);
end;

procedure TChatServer.BroadcastMessage(const SenderName, Message: string);
var
  FullMessage: string;
  i: Integer;
begin
  FullMessage := Format('[%s] %s', [SenderName, Message]);

  FLock.Acquire;
  try
    for i := 0 to FClients.Count - 1 do
      FClients[i].SendMessage(FullMessage);
  finally
    FLock.Release;
  end;
end;

{ TClientConnection }

constructor TClientConnection.Create(Owner: TChatServer; ClientSocket: TTcpClient);
begin
  inherited Create(False);
  FOwner := Owner;
  FClientSocket := ClientSocket;
  FClientName := 'Invité' + IntToStr(Random(1000));
  FreeOnTerminate := False;
end;

destructor TClientConnection.Destroy;
begin
  FClientSocket.Free;
  inherited;
end;

procedure TClientConnection.Execute;
var
  BytesLus: Integer;
  Command: string;
begin
  try
    // Envoyer un message de bienvenue
    SendMessage('Bienvenue sur le serveur de chat! Votre nom est "' +
                FClientName + '". Pour le changer, tapez /nom VotreNouveauNom');

    // Boucle principale pour recevoir les messages
    while not Terminated do
    begin
      try
        BytesLus := FClientSocket.ReceiveBuf(FBuffer, SizeOf(FBuffer));

        if BytesLus <= 0 then
          Break; // Connexion fermée

        SetString(Command, PAnsiChar(@FBuffer), BytesLus);
        ProcessCommand(Command);
      except
        Break; // Erreur de socket
      end;
    end;
  finally
    // Se retirer de la liste des clients
    FOwner.RemoveClient(Self);
  end;
end;

procedure TClientConnection.ProcessCommand(const Command: string);
var
  CmdLower: string;
begin
  // Vérifier si c'est une commande
  if (Length(Command) > 1) and (Command[1] = '/') then
  begin
    CmdLower := LowerCase(Command);

    // Commande pour changer de nom
    if Pos('/nom ', CmdLower) = 1 then
    begin
      FClientName := Copy(Command, 6, Length(Command));
      SendMessage('Votre nom a été changé en "' + FClientName + '"');
    end
    // Autres commandes possibles...
    else
      SendMessage('Commande inconnue');
  end
  else
  begin
    // Message normal, le diffuser à tous
    if Assigned(FOwner.OnMessageReceived) then
      FOwner.OnMessageReceived(FOwner, FClientName, Command);

    FOwner.BroadcastMessage(FClientName, Command);
  end;
end;

procedure TClientConnection.SendMessage(const Message: string);
begin
  if Assigned(FClientSocket) and FClientSocket.Connected then
    FClientSocket.SendBuf(PAnsiChar(Message)^, Length(Message));
end;

end.
```

### Client de chat

```pascal
unit ChatClient;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls;

type
  TMessageEvent = procedure(Sender: TObject; const Message: string) of object;
  TStatusEvent = procedure(Sender: TObject; const Status: string) of object;

  TChatClient = class(TThread)
  private
    FClientSocket: TTcpClient;
    FHost: string;
    FPort: Integer;
    FConnected: Boolean;
    FBuffer: array[0..2047] of Byte;
    FOnMessage: TMessageEvent;
    FOnStatus: TStatusEvent;

    procedure SetStatus(const Status: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const Host: string; Port: Integer);
    destructor Destroy; override;

    function Connect: Boolean;
    procedure Disconnect;
    procedure SendMessage(const Message: string);

    property OnMessage: TMessageEvent read FOnMessage write FOnMessage;
    property OnStatus: TStatusEvent read FOnStatus write FOnStatus;
    property Connected: Boolean read FConnected;
  end;

implementation

constructor TChatClient.Create(const Host: string; Port: Integer);
begin
  inherited Create(False);
  FHost := Host;
  FPort := Port;
  FConnected := False;
  FreeOnTerminate := False;
  Suspended := True; // Ne pas démarrer tout de suite
end;

destructor TChatClient.Destroy;
begin
  Disconnect;
  inherited;
end;

function TChatClient.Connect: Boolean;
begin
  Result := False;

  try
    FClientSocket := TTcpClient.Create(nil);
    FClientSocket.RemoteHost := FHost;
    FClientSocket.RemotePort := IntToStr(FPort);
    FClientSocket.Connect;

    FConnected := True;
    Result := True;

    SetStatus('Connecté au serveur');
    Suspended := False; // Démarrer le thread
  except
    on E: Exception do
    begin
      SetStatus('Erreur de connexion: ' + E.Message);
      if Assigned(FClientSocket) then
        FClientSocket.Free;
      FClientSocket := nil;
    end;
  end;
end;

procedure TChatClient.Disconnect;
begin
  if Assigned(FClientSocket) then
  begin
    FClientSocket.Close;
    FClientSocket.Free;
    FClientSocket := nil;
  end;

  FConnected := False;
  SetStatus('Déconnecté');
end;

procedure TChatClient.Execute;
var
  BytesLus: Integer;
  Message: string;
begin
  while not Terminated and FConnected do
  try
    BytesLus := FClientSocket.ReceiveBuf(FBuffer, SizeOf(FBuffer));

    if BytesLus <= 0 then
    begin
      FConnected := False;
      SetStatus('Connexion perdue');
      Break;
    end;

    SetString(Message, PAnsiChar(@FBuffer), BytesLus);

    if Assigned(FOnMessage) then
      TThread.Synchronize(nil, procedure
      begin
        FOnMessage(Self, Message);
      end);

  except
    on E: Exception do
    begin
      SetStatus('Erreur: ' + E.Message);
      FConnected := False;
      Break;
    end;
  end;
end;

procedure TChatClient.SendMessage(const Message: string);
begin
  if FConnected and Assigned(FClientSocket) then
    FClientSocket.SendBuf(PAnsiChar(Message)^, Length(Message));
end;

procedure TChatClient.SetStatus(const Status: string);
begin
  if Assigned(FOnStatus) then
    TThread.Synchronize(nil, procedure
    begin
      FOnStatus(Self, Status);
    end);
end;

end.
```

### Formulaire principal

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ChatServer, ChatClient;

type
  TFormChat = class(TForm)
    PageControl: TPageControl;
    TabServer: TTabSheet;
    TabClient: TTabSheet;
    // Composants du serveur
    EditServerPort: TEdit;
    BtnStartServer: TButton;
    BtnStopServer: TButton;
    MemoServerLog: TMemo;
    // Composants du client
    EditClientHost: TEdit;
    EditClientPort: TEdit;
    EditClientName: TEdit;
    BtnConnect: TButton;
    BtnDisconnect: TButton;
    MemoClientChat: TMemo;
    EditClientMessage: TEdit;
    BtnSend: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtnStartServerClick(Sender: TObject);
    procedure BtnStopServerClick(Sender: TObject);
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
  private
    FChatServer: TChatServer;
    FChatClient: TChatClient;

    // Événements serveur
    procedure ServerClientConnected(Sender: TObject; Client: TClientConnection);
    procedure ServerClientDisconnected(Sender: TObject; Client: TClientConnection);
    procedure ServerMessageReceived(Sender: TObject; const ClientName, Message: string);

    // Événements client
    procedure ClientMessageReceived(Sender: TObject; const Message: string);
    procedure ClientStatusChanged(Sender: TObject; const Status: string);
  public
    { Public declarations }
  end;

var
  FormChat: TFormChat;

implementation

{$R *.dfm}

procedure TFormChat.FormCreate(Sender: TObject);
begin
  // Initialiser les valeurs par défaut
  EditServerPort.Text := '8080';
  EditClientHost.Text := '127.0.0.1';
  EditClientPort.Text := '8080';
  EditClientName.Text := 'Utilisateur' + IntToStr(Random(1000));

  FChatServer := nil;
  FChatClient := nil;

  BtnStopServer.Enabled := False;
  BtnDisconnect.Enabled := False;
  BtnSend.Enabled := False;
end;

procedure TFormChat.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Nettoyer
  if Assigned(FChatServer) then
  begin
    FChatServer.Stop;
    FChatServer.Free;
  end;

  if Assigned(FChatClient) then
  begin
    FChatClient.Terminate;
    FChatClient.WaitFor;
    FChatClient.Free;
  end;
end;

// Fonctions du serveur
procedure TFormChat.BtnStartServerClick(Sender: TObject);
var
  Port: Integer;
begin
  if not TryStrToInt(EditServerPort.Text, Port) then
  begin
    ShowMessage('Port invalide');
    Exit;
  end;

  FChatServer := TChatServer.Create;
  FChatServer.OnClientConnected := ServerClientConnected;
  FChatServer.OnClientDisconnected := ServerClientDisconnected;
  FChatServer.OnMessageReceived := ServerMessageReceived;

  try
    FChatServer.Start(Port);
    MemoServerLog.Lines.Add('Serveur démarré sur le port ' + IntToStr(Port));

    BtnStartServer.Enabled := False;
    BtnStopServer.Enabled := True;
  except
    on E: Exception do
    begin
      MemoServerLog.Lines.Add('Erreur: ' + E.Message);
      FChatServer.Free;
      FChatServer := nil;
    end;
  end;
end;

procedure TFormChat.BtnStopServerClick(Sender: TObject);
begin
  if Assigned(FChatServer) then
  begin
    FChatServer.Stop;
    FChatServer.Free;
    FChatServer := nil;

    MemoServerLog.Lines.Add('Serveur arrêté');

    BtnStartServer.Enabled := True;
    BtnStopServer.Enabled := False;
  end;
end;

procedure TFormChat.ServerClientConnected(Sender: TObject; Client: TClientConnection);
begin
  TThread.Synchronize(nil, procedure
  begin
    MemoServerLog.Lines.Add('Client connecté: ' + Client.ClientName);
  end);
end;

procedure TFormChat.ServerClientDisconnected(Sender: TObject; Client: TClientConnection);
begin
  TThread.Synchronize(nil, procedure
  begin
    MemoServerLog.Lines.Add('Client déconnecté: ' + Client.ClientName);
  end);
end;

procedure TFormChat.ServerMessageReceived(Sender: TObject; const ClientName, Message: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    MemoServerLog.Lines.Add(Format('[%s] %s', [ClientName, Message]));
  end);
end;

// Fonctions du client
procedure TFormChat.BtnConnectClick(Sender: TObject);
var
  Port: Integer;
begin
  if not TryStrToInt(EditClientPort.Text, Port) then
  begin
    ShowMessage('Port invalide');
    Exit;
  end;

  FChatClient := TChatClient.Create(EditClientHost.Text, Port);
  FChatClient.OnMessage := ClientMessageReceived;
  FChatClient.OnStatus := ClientStatusChanged;

  if FChatClient.Connect then
  begin
    // Mettre à jour l'interface utilisateur
    BtnConnect.Enabled := False;
    BtnDisconnect.Enabled := True;
    BtnSend.Enabled := True;

    // Envoyer notre nom d'utilisateur
    FChatClient.SendMessage('/nom ' + EditClientName.Text);
  end
  else
  begin
    FChatClient.Free;
    FChatClient := nil;
  end;
end;

procedure TFormChat.BtnDisconnectClick(Sender: TObject);
begin
  if Assigned(FChatClient) then
  begin
    FChatClient.Terminate;
    FChatClient.WaitFor;
    FChatClient.Free;
    FChatClient := nil;

    MemoClientChat.Lines.Add('Déconnecté du serveur');

    BtnConnect.Enabled := True;
    BtnDisconnect.Enabled := False;
    BtnSend.Enabled := False;
  end;
end;

procedure TFormChat.BtnSendClick(Sender: TObject);
var
  Message: string;
begin
  if not Assigned(FChatClient) or not FChatClient.Connected then
  begin
    ShowMessage('Non connecté au serveur');
    Exit;
  end;

  Message := EditClientMessage.Text;
  if Message = '' then
    Exit;

  FChatClient.SendMessage(Message);
  EditClientMessage.Clear;
end;

procedure TFormChat.ClientMessageReceived(Sender: TObject; const Message: string);
begin
  MemoClientChat.Lines.Add(Message);

  // Faire défiler vers le bas
  SendMessage(MemoClientChat.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

procedure TFormChat.ClientStatusChanged(Sender: TObject; const Status: string);
begin
  MemoClientChat.Lines.Add('*** ' + Status + ' ***');
end;

// Gestion des touches pour envoyer un message avec Entrée
procedure TFormChat.EditClientMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;  // Supprimer le bip
    BtnSendClick(Sender);
  end;
end;
```

## Concepts avancés

### Protocole de communication personnalisé

Pour des applications plus complexes, il est souvent utile de définir un protocole personnalisé. Par exemple, pour notre application de chat, nous pourrions définir un format JSON pour les messages:

```pascal
type
  TMessageType = (mtChat, mtSystem, mtPrivate, mtUserList);

  TChatMessage = record
    MsgType: TMessageType;
    Sender: string;
    Recipient: string;  // Pour les messages privés
    Content: string;
    Timestamp: TDateTime;

    function ToJSON: string;
    procedure FromJSON(const JSONStr: string);
  end;

function TChatMessage.ToJSON: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('type', Integer(MsgType));
    JSONObj.AddPair('sender', Sender);
    JSONObj.AddPair('recipient', Recipient);
    JSONObj.AddPair('content', Content);
    JSONObj.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp));

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TChatMessage.FromJSON(const JSONStr: string);
var
  JSONValue: TJSONValue;
  JSONObj: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONStr);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObj := JSONValue as TJSONObject;

      MsgType := TMessageType(JSONObj.GetValue<Integer>('type'));
      Sender := JSONObj.GetValue<string>('sender');
      Recipient := JSONObj.GetValue<string>('recipient');
      Content := JSONObj.GetValue<string>('content');

      try
        Timestamp := StrToDateTime(JSONObj.GetValue<string>('timestamp'));
      except
        Timestamp := Now;
      end;
    end;
  finally
    JSONValue.Free;
  end;
end;
```

### Gestion de la sécurité avec SSL/TLS

Pour sécuriser les communications, vous pouvez utiliser SSL/TLS:

```pascal
procedure ConfigSSLClient(Client: TIdTCPClient);
var
  SSLHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  SSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(Client);

  // Configurer le gestionnaire SSL
  SSLHandler.SSLOptions.Method := sslvTLSv1_2;
  SSLHandler.SSLOptions.Mode := sslmClient;

  // Assigner le gestionnaire au client
  Client.IOHandler := SSLHandler;
end;

procedure ConfigSSLServer(Server: TIdTCPServer);
var
  SSLHandler: TIdServerIOHandlerSSLOpenSSL;
begin
  SSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(Server);

  // Configurer le gestionnaire SSL
  SSLHandler.SSLOptions.CertFile := 'server.crt';
  SSLHandler.SSLOptions.KeyFile := 'server.key';
  SSLHandler.SSLOptions.Method := sslvTLSv1_2;
  SSLHandler.SSLOptions.Mode := sslmServer;

  // Assigner le gestionnaire au serveur
  Server.IOHandler := SSLHandler;
end;
```

### Transfert de fichiers

Voici un exemple simple de transfert de fichier via socket:

```pascal
// Côté émetteur
procedure EnvoyerFichier(Socket: TTcpClient; const NomFichier: string);
var
  Fichier: TFileStream;
  TailleFichier: Int64;
  Tampon: array[0..8191] of Byte;
  BytesLus: Integer;
begin
  Fichier := TFileStream.Create(NomFichier, fmOpenRead or fmShareDenyWrite);
  try
    // Envoyer le nom du fichier
    Socket.SendBuf(PAnsiChar(ExtractFileName(NomFichier))^,
                   Length(ExtractFileName(NomFichier)));

    // Attendre confirmation
    Socket.ReceiveBuf(Tampon, 1);

    // Envoyer la taille du fichier
    TailleFichier := Fichier.Size;
    Socket.SendBuf(TailleFichier, SizeOf(TailleFichier));

    // Attendre confirmation
    Socket.ReceiveBuf(Tampon, 1);

    // Envoyer le contenu du fichier
    while Fichier.Position < Fichier.Size do
    begin
      BytesLus := Fichier.Read(Tampon, SizeOf(Tampon));
      Socket.SendBuf(Tampon, BytesLus);
    end;

  finally
    Fichier.Free;
  end;
end;

// Côté récepteur
procedure RecevoirFichier(Socket: TTcpClient; const DossierDestination: string);
var
  Fichier: TFileStream;
  NomFichier: string;
  TailleFichier: Int64;
  Tampon: array[0..8191] of Byte;
  BytesLus, TotalLu: Integer;
  Confirmation: Byte;
begin
  // Recevoir le nom du fichier
  BytesLus := Socket.ReceiveBuf(Tampon, SizeOf(Tampon));
  SetString(NomFichier, PAnsiChar(@Tampon), BytesLus);

  // Envoyer confirmation
  Confirmation := 1;
  Socket.SendBuf(Confirmation, 1);

  // Recevoir la taille du fichier
  Socket.ReceiveBuf(TailleFichier, SizeOf(TailleFichier));

  // Envoyer confirmation
  Socket.SendBuf(Confirmation, 1);

  // Créer le fichier de destination
  Fichier := TFileStream.Create(IncludeTrailingPathDelimiter(DossierDestination) +
                               NomFichier, fmCreate);
  try
    // Recevoir le contenu du fichier
    TotalLu := 0;
    while TotalLu < TailleFichier do
    begin
      BytesLus := Socket.ReceiveBuf(Tampon, Min(SizeOf(Tampon), TailleFichier - TotalLu));

      if BytesLus <= 0 then
        raise Exception.Create('Connexion perdue');

      Fichier.WriteBuffer(Tampon, BytesLus);
      Inc(TotalLu, BytesLus);
    end;

  finally
    Fichier.Free;
  end;
end;
```

## Traitement de plusieurs clients simultanément

Pour gérer efficacement plusieurs clients, il existe différentes approches:

### Approche par thread

Comme dans notre exemple de chat, chaque client est géré par un thread séparé:

```pascal
procedure TMyServer.AcceptClient(ClientSocket: TTcpClient);
var
  ClientHandler: TClientThread;
begin
  // Créer un thread pour gérer ce client
  ClientHandler := TClientThread.Create(ClientSocket);
  ClientHandler.FreeOnTerminate := True;  // Se libère automatiquement
  ClientHandler.Start;
end;
```

### Approche asynchrone avec des gestionnaires d'événements

Une autre approche est d'utiliser des sockets asynchrones avec des gestionnaires d'événements:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  ServerSocket := TServerSocket.Create(Self);
  ServerSocket.Port := 8080;
  ServerSocket.ServerType := stNonBlocking;  // Mode asynchrone
  ServerSocket.OnClientConnect := ServerClientConnect;
  ServerSocket.OnClientDisconnect := ServerClientDisconnect;
  ServerSocket.OnClientRead := ServerClientRead;
  ServerSocket.Active := True;
end;

procedure TForm1.ServerClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Memo1.Lines.Add('Client connecté: ' + Socket.RemoteAddress);
end;

procedure TForm1.ServerClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Memo1.Lines.Add('Client déconnecté: ' + Socket.RemoteAddress);
end;

procedure TForm1.ServerClientRead(Sender: TObject; Socket: TCustomWinSocket);
var
  Data: string;
begin
  Data := Socket.ReceiveText;
  Memo1.Lines.Add('Message reçu de ' + Socket.RemoteAddress + ': ' + Data);
  // Répondre au client
  Socket.SendText('Message reçu!');
end;
```

### Pool de threads

Pour les applications à haute performance, un pool de threads peut être plus efficace:

```pascal
type
  TClientTask = class
  private
    FClientSocket: TTcpClient;
    FData: TBytes;
  public
    constructor Create(ClientSocket: TTcpClient; const Data: TBytes);
    procedure Execute;
  end;

TThreadPool = class
private
  FThreads: array of TThread;
  FTasks: TThreadedQueue<TClientTask>;
  procedure WorkerProc(WorkerThread: TThread);
public
  constructor Create(ThreadCount: Integer);
  destructor Destroy; override;
  procedure AddTask(Task: TClientTask);
end;

constructor TThreadPool.Create(ThreadCount: Integer);
var
  i: Integer;
begin
  inherited Create;

  // Créer la file d'attente des tâches
  FTasks := TThreadedQueue<TClientTask>.Create(100, INFINITE, INFINITE);

  // Créer les threads de travail
  SetLength(FThreads, ThreadCount);
  for i := 0 to ThreadCount - 1 do
  begin
    FThreads[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        WorkerProc(TThread.CurrentThread);
      end);
    FThreads[i].Start;
  end;
end;

procedure TThreadPool.WorkerProc(WorkerThread: TThread);
var
  Task: TClientTask;
begin
  while not WorkerThread.CheckTerminated do
  begin
    if FTasks.PopItem(Task) = wrSignaled then
    try
      Task.Execute;
    finally
      Task.Free;
    end;
  end;
end;

procedure TThreadPool.AddTask(Task: TClientTask);
begin
  FTasks.PushItem(Task);
end;
```

## Applications pratiques des sockets

### Serveur Web simple

Voici un exemple minimaliste de serveur Web HTTP:

```pascal
procedure TWebServer.HandleRequest(ClientSocket: TTcpClient);
var
  Request, Response, FilePath: string;
  FileContent: TStringList;
  Buffer: array[0..4095] of Byte;
  BytesLus: Integer;
begin
  // Lire la requête HTTP
  BytesLus := ClientSocket.ReceiveBuf(Buffer, SizeOf(Buffer));
  SetString(Request, PAnsiChar(@Buffer), BytesLus);

  // Analyser la première ligne pour obtenir le chemin demandé
  FilePath := ExtractRequestPath(Request);

  // Si aucun fichier spécifié, utiliser index.html
  if (FilePath = '') or (FilePath = '/') then
    FilePath := '/index.html';

  // Convertir en chemin local
  FilePath := StringReplace(FilePath, '/', PathDelim, [rfReplaceAll]);
  FilePath := FDocumentRoot + FilePath;

  // Vérifier si le fichier existe
  if FileExists(FilePath) then
  begin
    // Lire le contenu du fichier
    FileContent := TStringList.Create;
    try
      FileContent.LoadFromFile(FilePath);

      // Construire la réponse HTTP
      Response := 'HTTP/1.1 200 OK' + #13#10 +
                  'Content-Type: ' + GetContentType(FilePath) + #13#10 +
                  'Content-Length: ' + IntToStr(Length(FileContent.Text)) + #13#10 +
                  'Connection: close' + #13#10 +
                  #13#10 +
                  FileContent.Text;
    finally
      FileContent.Free;
    end;
  end
  else
  begin
    // Fichier non trouvé
    Response := 'HTTP/1.1 404 Not Found' + #13#10 +
                'Content-Type: text/html' + #13#10 +
                'Content-Length: 23' + #13#10 +
                'Connection: close' + #13#10 +
                #13#10 +
                '<h1>404 Not Found</h1>';
  end;

  // Envoyer la réponse
  ClientSocket.SendBuf(PAnsiChar(Response)^, Length(Response));
end;

function TWebServer.ExtractRequestPath(const Request: string): string;
var
  Lines: TStringList;
  FirstLine, Method, Path: string;
  SpacePos1, SpacePos2: Integer;
begin
  Result := '';

  Lines := TStringList.Create;
  try
    Lines.Text := Request;

    if Lines.Count > 0 then
    begin
      FirstLine := Lines[0];

      // Extraire la méthode et le chemin
      SpacePos1 := Pos(' ', FirstLine);
      if SpacePos1 > 0 then
      begin
        Method := Copy(FirstLine, 1, SpacePos1 - 1);

        SpacePos2 := PosEx(' ', FirstLine, SpacePos1 + 1);
        if SpacePos2 > 0 then
          Path := Copy(FirstLine, SpacePos1 + 1, SpacePos2 - SpacePos1 - 1)
        else
          Path := Copy(FirstLine, SpacePos1 + 1);

        // Retourner le chemin si c'est une requête GET
        if Method = 'GET' then
          Result := Path;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

function TWebServer.GetContentType(const FilePath: string): string;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FilePath));

  if Ext = '.html' then
    Result := 'text/html'
  else if Ext = '.css' then
    Result := 'text/css'
  else if Ext = '.js' then
    Result := 'application/javascript'
  else if (Ext = '.jpg') or (Ext = '.jpeg') then
    Result := 'image/jpeg'
  else if Ext = '.png' then
    Result := 'image/png'
  else if Ext = '.gif' then
    Result := 'image/gif'
  else
    Result := 'application/octet-stream';
end;
```

### Client de serveur de temps

Voici un exemple de client qui synchronise l'heure avec un serveur NTP:

```pascal
function GetNTPTime(const NTPServer: string): TDateTime;
var
  Client: TTcpClient;
  NTPData: array[0..47] of Byte;
  Response: array[0..47] of Byte;
  Seconds: Cardinal;
  i: Integer;
begin
  Result := 0;

  // Initialiser le paquet NTP
  FillChar(NTPData, SizeOf(NTPData), 0);
  NTPData[0] := $1B;  // Version 3, Mode 3 (client)

  Client := TTcpClient.Create(nil);
  try
    Client.RemoteHost := NTPServer;
    Client.RemotePort := '123';  // Port NTP standard

    try
      Client.Connect;

      // Envoyer la requête
      Client.SendBuf(NTPData, SizeOf(NTPData));

      // Recevoir la réponse
      if Client.ReceiveBuf(Response, SizeOf(Response)) = SizeOf(Response) then
      begin
        // Extraire l'horodatage de la réponse
        // L'horodatage se trouve aux octets 40-43
        Seconds := 0;
        for i := 40 to 43 do
          Seconds := (Seconds shl 8) + Response[i];

        // Convertir en DateTime
        // NTP utilise le 1er janvier 1900 comme référence, alors que
        // Delphi utilise le 30 décembre 1899
        Result := (Seconds / 86400) + 2;  // 2 jours d'écart entre 1899 et 1900
        Result := Result - TimeZoneBias / 1440;  // Ajuster pour le fuseau horaire local
      end;
    except
      on E: Exception do
        ShowMessage('Erreur: ' + E.Message);
    end;
  finally
    Client.Free;
  end;
end;
```

## Dépannage des problèmes courants

### Erreurs de connexion

1. **Connexion refusée**
   - Vérifiez que le serveur est en cours d'exécution
   - Assurez-vous que le pare-feu ne bloque pas le port
   - Confirmez que l'adresse IP et le port sont corrects

2. **Socket en attente**
   - Augmentez le délai d'attente de connexion
   - Vérifiez la connectivité réseau

```pascal
procedure ConfigurerDelaisSocket(Socket: TTcpClient);
begin
  Socket.ConnectTimeout := 5000;  // 5 secondes
  Socket.ReadTimeout := 10000;    // 10 secondes
end;
```

### Problèmes de concurrence

Dans les applications avec plusieurs threads, protégez les ressources partagées:

```pascal
procedure TServerHandler.AddClient(Client: TClientConnection);
begin
  FLock.Acquire;
  try
    FClients.Add(Client);
  finally
    FLock.Release;
  end;
end;
```

### Débogage des communications socket

Pour déboguer les communications socket:

```pascal
procedure DebuggerSocket(const Operation, Data: string);
begin
  if FDebugMode then
  begin
    TFile.AppendAllText('socket_log.txt',
                       Format('[%s] %s: %s',
                             [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
                              Operation,
                              Data]));
  end;
end;

// Utilisation
procedure TMyClient.EnvoyerDonnees(const Data: string);
begin
  DebuggerSocket('ENVOI', Data);
  FClientSocket.SendBuf(PAnsiChar(Data)^, Length(Data));
end;

procedure TMyClient.RecevoirDonnees;
var
  Buffer: array[0..1023] of Byte;
  BytesLus: Integer;
  Data: string;
begin
  BytesLus := FClientSocket.ReceiveBuf(Buffer, SizeOf(Buffer));

  if BytesLus > 0 then
  begin
    SetString(Data, PAnsiChar(@Buffer), BytesLus);
    DebuggerSocket('RECEPTION', Data);
    // Traiter les données...
  end;
end;
```

## Bonnes pratiques

1. **Toujours libérer les ressources**
   - Utilisez try..finally pour assurer la libération des sockets
   - Fermez proprement les connexions

2. **Gérer les délais d'expiration**
   - Définissez des délais raisonnables pour éviter les blocages

3. **Gestion des erreurs**
   - Attrapez et traitez toutes les exceptions possibles
   - Prévoyez des mécanismes de reconnexion automatique

4. **Documentation du protocole**
   - Documentez clairement votre protocole de communication
   - Incluez des numéros de version dans vos messages

5. **Sécurité**
   - Utilisez SSL/TLS pour les communications sensibles
   - Validez toutes les entrées provenant du réseau

## Conclusion

Les sockets sont un outil puissant pour créer des applications réseau, qu'il s'agisse de simples chats ou de services complexes. En comprenant les principes fondamentaux et en suivant les bonnes pratiques, vous pouvez créer des communications robustes et efficaces entre vos applications.

## Exercices pratiques

1. **Application de chat améliorée**
   - Ajoutez la prise en charge des messages privés
   - Implémentez un système de salons de discussion

2. **Surveillance de serveur**
   - Créez une application cliente qui vérifie si un serveur est en ligne
   - Ajoutez des notifications en cas de panne

3. **Transfert de fichiers**
   - Implémentez un client et un serveur FTP simplifié
   - Ajoutez une barre de progression pour le suivi des transferts

4. **Service de sauvegarde**
   - Créez un système qui permet à un client d'envoyer des fichiers à sauvegarder sur un serveur
   - Implémentez un mécanisme de synchronisation qui n'envoie que les fichiers modifiés

5. **Extension du serveur Web**
   - Ajoutez la prise en charge des méthodes POST
   - Implémentez un mécanisme simple d'authentification
