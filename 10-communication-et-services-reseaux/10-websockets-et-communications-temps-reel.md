🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.10 WebSockets et communications temps réel

## Introduction aux WebSockets

### Qu'est-ce qu'un WebSocket ?

Un **WebSocket** est un protocole de communication bidirectionnel qui permet une connexion persistante entre un client et un serveur, permettant l'échange de données en temps réel.

**Analogie simple :**
Imaginez la différence entre envoyer des lettres (HTTP) et avoir une conversation téléphonique (WebSocket) :

**HTTP traditionnel (Lettres) :**
```
Client → "Bonjour, as-tu des nouveaux messages ?"
Serveur → "Non, aucun nouveau message"

Client → "Et maintenant ?"
Serveur → "Non, toujours rien"

Client → "Et là ?"
Serveur → "Oui ! Nouveau message : 'Salut'"
```
Vous devez demander constamment (polling).

**WebSocket (Téléphone) :**
```
Client → "Bonjour" (connexion établie)
Serveur → "Salut, je suis là"

[Connexion reste ouverte]

Serveur → "Nouveau message : 'Salut'" (push instantané)
Client → "Message reçu, merci !"
```
La connexion reste ouverte, communication instantanée dans les deux sens.

### Problèmes résolus par WebSocket

**1. Polling inefficace**
```
HTTP Polling :
┌────────┐     ┌────────┐     ┌────────┐     ┌────────┐
│ Client │────►│Serveur │     │ Client │────►│Serveur │
└────────┘     └────────┘     └────────┘     └────────┘
    ↑ Requête      ↓ Pas de     ↑ Requête      ↓ Nouveaux
                   nouveauté                     données

Problème : Beaucoup de requêtes inutiles

WebSocket :
┌────────┐═══════════════════►┌────────┐
│ Client │                    │Serveur │
└────────┘◄═══════════════════└────────┘
    Connexion persistante
    Données échangées uniquement quand nécessaire
```

**2. Latence réduite**
- HTTP : Nouvelle connexion à chaque requête (3-way handshake)
- WebSocket : Connexion établie une fois

**3. Overhead réduit**
- HTTP : En-têtes volumineux à chaque requête (~800 octets)
- WebSocket : Frames légères (~2-6 octets)

**4. Communication bidirectionnelle**
- HTTP : Client doit initier
- WebSocket : Serveur peut pousser les données

### Cas d'usage WebSocket

**Applications temps réel :**
- 💬 **Chat en direct** : Messages instantanés
- 📊 **Dashboards** : Données en temps réel (bourse, monitoring)
- 🎮 **Jeux multijoueurs** : Synchronisation des joueurs
- 📝 **Éditeurs collaboratifs** : Google Docs, Notion
- 🔔 **Notifications push** : Alertes instantanées
- 📺 **Streaming** : Diffusion en direct
- 🗺️ **Tracking GPS** : Suivi de véhicules/livreurs
- 📈 **Trading** : Cotations boursières en temps réel

## Le protocole WebSocket

### Handshake (Établissement de connexion)

WebSocket commence comme une requête HTTP normale, puis "upgrade" vers WebSocket.

**Étape 1 : Requête du client**
```http
GET /chat HTTP/1.1
Host: server.example.com
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
Sec-WebSocket-Version: 13
Origin: http://example.com
```

**Étape 2 : Réponse du serveur**
```http
HTTP/1.1 101 Switching Protocols
Upgrade: websocket
Connection: Upgrade
Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=
```

**Code 101** = "Switching Protocols" : La connexion passe d'HTTP à WebSocket.

### Format des messages (Frames)

Une fois connecté, les données sont échangées via des **frames** :

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-------+-+-------------+-------------------------------+
|F|R|R|R| opcode|M| Payload len |    Extended payload length    |
|I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
|N|V|V|V|       |S|             |   (if payload len==126/127)   |
| |1|2|3|       |K|             |                               |
+-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
|     Extended payload length continued, if payload len == 127  |
+ - - - - - - - - - - - - - - - +-------------------------------+
|                               |Masking-key, if MASK set to 1  |
+-------------------------------+-------------------------------+
| Masking-key (continued)       |          Payload Data         |
+-------------------------------- - - - - - - - - - - - - - - - +
:                     Payload Data continued ...                :
+ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
|                     Payload Data continued ...                |
+---------------------------------------------------------------+
```

**Opcodes principaux :**
- `0x0` : Continuation frame
- `0x1` : Text frame (UTF-8)
- `0x2` : Binary frame
- `0x8` : Close
- `0x9` : Ping
- `0xA` : Pong

### États de connexion

```
┌──────────┐
│CONNECTING│ Handshake en cours
└─────┬────┘
      │
      ▼
┌──────────┐
│   OPEN   │ Connexion établie, peut échanger des données
└─────┬────┘
      │
      ▼
┌──────────┐
│ CLOSING  │ Fermeture en cours
└─────┬────┘
      │
      ▼
┌──────────┐
│  CLOSED  │ Connexion fermée
└──────────┘
```

## Implémentation WebSocket avec Delphi

### Client WebSocket de base

```pascal
unit WebSocketClient;

interface

uses
  System.SysUtils, System.Classes, IdTCPClient, IdGlobal, IdSSL,
  IdSSLOpenSSL, System.Hash, System.NetEncoding, System.SyncObjs;

type
  TWebSocketState = (wsConnecting, wsOpen, wsClosing, wsClosed);

  TWebSocketClient = class
  private
    FTCPClient: TIdTCPClient;
    FSSLHandler: TIdSSLIOHandlerSocketOpenSSL;
    FState: TWebSocketState;
    FOnMessage: TProc<string>;
    FOnBinaryMessage: TProc<TBytes>;
    FOnConnect: TProc;
    FOnDisconnect: TProc;
    FOnError: TProc<string>;
    FLock: TCriticalSection;
    FReceiveThread: TThread;

    procedure PerformHandshake(const Host, Path: string);
    procedure StartReceiveThread;
    procedure ProcessFrame;
    function BuildFrame(const Data: string; OpCode: Byte = $01): TBytes; overload;
    function BuildFrame(const Data: TBytes; OpCode: Byte = $02): TBytes; overload;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Connect(const URL: string);
    procedure Send(const Message: string); overload;
    procedure Send(const Data: TBytes); overload;
    procedure Close;

    property State: TWebSocketState read FState;
    property OnMessage: TProc<string> read FOnMessage write FOnMessage;
    property OnBinaryMessage: TProc<TBytes> read FOnBinaryMessage write FOnBinaryMessage;
    property OnConnect: TProc read FOnConnect write FOnConnect;
    property OnDisconnect: TProc read FOnDisconnect write FOnDisconnect;
    property OnError: TProc<string> read FOnError write FOnError;
  end;

implementation

uses
  System.StrUtils;

constructor TWebSocketClient.Create;
begin
  inherited;
  FTCPClient := TIdTCPClient.Create(nil);
  FSSLHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FTCPClient.IOHandler := FSSLHandler;
  FState := wsClosed;
  FLock := TCriticalSection.Create;
end;

destructor TWebSocketClient.Destroy;
begin
  Close;
  FLock.Free;
  FTCPClient.Free;
  FSSLHandler.Free;
  inherited;
end;

procedure TWebSocketClient.Connect(const URL: string);
var
  Protocol, Host, Path: string;
  Port: Integer;
  UseSSL: Boolean;
begin
  FState := wsConnecting;

  // Parser l'URL
  if URL.StartsWith('wss://') then
  begin
    UseSSL := True;
    Host := URL.Substring(6);
  end
  else if URL.StartsWith('ws://') then
  begin
    UseSSL := False;
    Host := URL.Substring(5);
  end
  else
    raise Exception.Create('URL invalide. Doit commencer par ws:// ou wss://');

  // Extraire host, port et path
  if Host.Contains('/') then
  begin
    Path := '/' + Host.Substring(Host.IndexOf('/') + 1);
    Host := Host.Substring(0, Host.IndexOf('/'));
  end
  else
    Path := '/';

  if Host.Contains(':') then
  begin
    Port := StrToInt(Host.Substring(Host.IndexOf(':') + 1));
    Host := Host.Substring(0, Host.IndexOf(':'));
  end
  else
    Port := IfThen(UseSSL, 443, 80);

  // Connexion TCP
  FTCPClient.Host := Host;
  FTCPClient.Port := Port;

  if UseSSL then
  begin
    FSSLHandler.SSLOptions.Method := sslvTLSv1_2;
    FSSLHandler.SSLOptions.Mode := sslmClient;
  end;

  try
    FTCPClient.Connect;

    // Handshake WebSocket
    PerformHandshake(Host, Path);

    FState := wsOpen;

    // Démarrer le thread de réception
    StartReceiveThread;

    if Assigned(FOnConnect) then
      TThread.Synchronize(nil, procedure begin FOnConnect(); end);

  except
    on E: Exception do
    begin
      FState := wsClosed;
      if Assigned(FOnError) then
        TThread.Synchronize(nil, procedure begin FOnError(E.Message); end);
      raise;
    end;
  end;
end;

procedure TWebSocketClient.PerformHandshake(const Host, Path: string);
var
  Key, Accept, ExpectedAccept: string;
  Request, Response: string;
  Lines: TArray<string>;
  Line: string;
  HandshakeOK: Boolean;
begin
  // Générer une clé aléatoire
  Key := TNetEncoding.Base64.Encode(THashSHA1.GetHashString(TGUID.NewGuid.ToString));

  // Calculer l'Accept attendu
  ExpectedAccept := TNetEncoding.Base64.Encode(
    THashSHA1.GetHashBytes(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));

  // Construire la requête de handshake
  Request :=
    'GET ' + Path + ' HTTP/1.1' + #13#10 +
    'Host: ' + Host + #13#10 +
    'Upgrade: websocket' + #13#10 +
    'Connection: Upgrade' + #13#10 +
    'Sec-WebSocket-Key: ' + Key + #13#10 +
    'Sec-WebSocket-Version: 13' + #13#10 +
    'Origin: http://' + Host + #13#10 +
    #13#10;

  // Envoyer la requête
  FTCPClient.IOHandler.Write(Request);

  // Lire la réponse
  Response := '';
  repeat
    Line := FTCPClient.IOHandler.ReadLn;
    Response := Response + Line + #13#10;
  until Line.IsEmpty;

  // Vérifier la réponse
  HandshakeOK := False;
  Lines := Response.Split([#13#10]);

  for Line in Lines do
  begin
    if Line.StartsWith('HTTP/1.1 101') then
      HandshakeOK := True;

    if Line.ToLower.StartsWith('sec-websocket-accept:') then
    begin
      Accept := Line.Substring(Line.IndexOf(':') + 1).Trim;
      if Accept <> ExpectedAccept then
        raise Exception.Create('Handshake invalide');
    end;
  end;

  if not HandshakeOK then
    raise Exception.Create('Handshake échoué');
end;

procedure TWebSocketClient.StartReceiveThread;
begin
  FReceiveThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while (FState = wsOpen) and FTCPClient.Connected do
      begin
        try
          ProcessFrame;
        except
          on E: Exception do
          begin
            if Assigned(FOnError) then
              TThread.Synchronize(nil, procedure begin FOnError(E.Message); end);
            Break;
          end;
        end;
      end;

      FState := wsClosed;
      if Assigned(FOnDisconnect) then
        TThread.Synchronize(nil, procedure begin FOnDisconnect(); end);
    end);

  FReceiveThread.FreeOnTerminate := False;
  FReceiveThread.Start;
end;

procedure TWebSocketClient.ProcessFrame;
var
  Byte1, Byte2: Byte;
  Fin, Masked: Boolean;
  OpCode: Byte;
  PayloadLength: Int64;
  MaskingKey: array[0..3] of Byte;
  Payload: TBytes;
  i: Integer;
  TextMessage: string;
begin
  // Lire les 2 premiers octets
  Byte1 := FTCPClient.IOHandler.ReadByte;
  Byte2 := FTCPClient.IOHandler.ReadByte;

  Fin := (Byte1 and $80) <> 0;
  OpCode := Byte1 and $0F;
  Masked := (Byte2 and $80) <> 0;
  PayloadLength := Byte2 and $7F;

  // Longueur étendue
  if PayloadLength = 126 then
    PayloadLength := FTCPClient.IOHandler.ReadWord
  else if PayloadLength = 127 then
    PayloadLength := FTCPClient.IOHandler.ReadInt64;

  // Masking key (si présent)
  if Masked then
    FTCPClient.IOHandler.ReadBytes(TIdBytes(MaskingKey), 4, False);

  // Payload
  SetLength(Payload, PayloadLength);
  if PayloadLength > 0 then
    FTCPClient.IOHandler.ReadBytes(TIdBytes(Payload), PayloadLength, False);

  // Démasquer si nécessaire
  if Masked then
    for i := 0 to High(Payload) do
      Payload[i] := Payload[i] xor MaskingKey[i mod 4];

  // Traiter selon l'opcode
  case OpCode of
    $01: // Text
      begin
        TextMessage := TEncoding.UTF8.GetString(Payload);
        if Assigned(FOnMessage) then
          TThread.Synchronize(nil, procedure begin FOnMessage(TextMessage); end);
      end;

    $02: // Binary
      if Assigned(FOnBinaryMessage) then
        TThread.Synchronize(nil, procedure begin FOnBinaryMessage(Payload); end);

    $08: // Close
      begin
        FState := wsClosing;
        Close;
      end;

    $09: // Ping
      begin
        // Répondre avec Pong
        Send(Payload);
      end;
  end;
end;

function TWebSocketClient.BuildFrame(const Data: string; OpCode: Byte): TBytes;
begin
  Result := BuildFrame(TEncoding.UTF8.GetBytes(Data), OpCode);
end;

function TWebSocketClient.BuildFrame(const Data: TBytes; OpCode: Byte): TBytes;
var
  Frame: TBytes;
  PayloadLength: Int64;
  MaskingKey: array[0..3] of Byte;
  i, Offset: Integer;
begin
  PayloadLength := Length(Data);

  // Générer masking key
  for i := 0 to 3 do
    MaskingKey[i] := Random(256);

  // Calculer la taille de la frame
  if PayloadLength < 126 then
    SetLength(Frame, 2 + 4 + PayloadLength)
  else if PayloadLength < 65536 then
    SetLength(Frame, 4 + 4 + PayloadLength)
  else
    SetLength(Frame, 10 + 4 + PayloadLength);

  // Byte 1 : FIN + OpCode
  Frame[0] := $80 or OpCode;

  // Byte 2 : MASK + Payload length
  Offset := 2;
  if PayloadLength < 126 then
  begin
    Frame[1] := $80 or Byte(PayloadLength);
  end
  else if PayloadLength < 65536 then
  begin
    Frame[1] := $80 or 126;
    Frame[2] := (PayloadLength shr 8) and $FF;
    Frame[3] := PayloadLength and $FF;
    Offset := 4;
  end
  else
  begin
    Frame[1] := $80 or 127;
    // 8 octets pour la longueur
    for i := 7 downto 0 do
    begin
      Frame[2 + (7 - i)] := (PayloadLength shr (i * 8)) and $FF;
    end;
    Offset := 10;
  end;

  // Masking key
  Move(MaskingKey[0], Frame[Offset], 4);
  Inc(Offset, 4);

  // Payload masqué
  for i := 0 to High(Data) do
    Frame[Offset + i] := Data[i] xor MaskingKey[i mod 4];

  Result := Frame;
end;

procedure TWebSocketClient.Send(const Message: string);
var
  Frame: TBytes;
begin
  if FState <> wsOpen then
    raise Exception.Create('WebSocket non connecté');

  FLock.Enter;
  try
    Frame := BuildFrame(Message, $01); // Text frame
    FTCPClient.IOHandler.Write(TIdBytes(Frame));
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketClient.Send(const Data: TBytes);
var
  Frame: TBytes;
begin
  if FState <> wsOpen then
    raise Exception.Create('WebSocket non connecté');

  FLock.Enter;
  try
    Frame := BuildFrame(Data, $02); // Binary frame
    FTCPClient.IOHandler.Write(TIdBytes(Frame));
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketClient.Close;
var
  CloseFrame: TBytes;
begin
  if FState = wsClosed then
    Exit;

  FState := wsClosing;

  try
    // Envoyer frame de fermeture
    SetLength(CloseFrame, 2);
    CloseFrame[0] := $88; // FIN + Close opcode
    CloseFrame[1] := $00; // Pas de payload

    if FTCPClient.Connected then
      FTCPClient.IOHandler.Write(TIdBytes(CloseFrame));
  finally
    if Assigned(FReceiveThread) then
    begin
      FReceiveThread.Terminate;
      FReceiveThread.WaitFor;
      FReceiveThread.Free;
      FReceiveThread := nil;
    end;

    FTCPClient.Disconnect;
    FState := wsClosed;
  end;
end;

end.
```

### Utilisation du client

```pascal
unit FormMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, WebSocketClient;

type
  TFormMain = class(TForm)
    ButtonConnect: TButton;
    ButtonDisconnect: TButton;
    ButtonSend: TButton;
    EditURL: TEdit;
    EditMessage: TEdit;
    MemoMessages: TMemo;
    LabelStatus: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonDisconnectClick(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMessageKeyPress(Sender: TObject; var Key: Char);
  private
    FWebSocket: TWebSocketClient;
    procedure OnWebSocketConnect;
    procedure OnWebSocketMessage(const Message: string);
    procedure OnWebSocketDisconnect;
    procedure OnWebSocketError(const Error: string);
    procedure UpdateUI;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FWebSocket := TWebSocketClient.Create;

  // Configurer les callbacks
  FWebSocket.OnConnect := OnWebSocketConnect;
  FWebSocket.OnMessage := OnWebSocketMessage;
  FWebSocket.OnDisconnect := OnWebSocketDisconnect;
  FWebSocket.OnError := OnWebSocketError;

  // URL par défaut
  EditURL.Text := 'wss://echo.websocket.org';

  UpdateUI;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FWebSocket.Free;
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
begin
  try
    MemoMessages.Lines.Add('Connexion en cours...');
    FWebSocket.Connect(EditURL.Text);
  except
    on E: Exception do
    begin
      MemoMessages.Lines.Add('Erreur: ' + E.Message);
      UpdateUI;
    end;
  end;
end;

procedure TFormMain.ButtonDisconnectClick(Sender: TObject);
begin
  FWebSocket.Close;
end;

procedure TFormMain.ButtonSendClick(Sender: TObject);
var
  Message: string;
begin
  Message := EditMessage.Text;

  if Message.IsEmpty then
    Exit;

  try
    FWebSocket.Send(Message);
    MemoMessages.Lines.Add('→ ' + Message);
    EditMessage.Clear;
  except
    on E: Exception do
      MemoMessages.Lines.Add('Erreur envoi: ' + E.Message);
  end;
end;

procedure TFormMain.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    ButtonSendClick(nil);
    Key := #0;
  end;
end;

procedure TFormMain.OnWebSocketConnect;
begin
  MemoMessages.Lines.Add('✓ Connecté !');
  LabelStatus.Caption := 'Connecté';
  UpdateUI;
end;

procedure TFormMain.OnWebSocketMessage(const Message: string);
begin
  MemoMessages.Lines.Add('← ' + Message);
end;

procedure TFormMain.OnWebSocketDisconnect;
begin
  MemoMessages.Lines.Add('✗ Déconnecté');
  LabelStatus.Caption := 'Déconnecté';
  UpdateUI;
end;

procedure TFormMain.OnWebSocketError(const Error: string);
begin
  MemoMessages.Lines.Add('⚠ Erreur: ' + Error);
end;

procedure TFormMain.UpdateUI;
var
  Connected: Boolean;
begin
  Connected := FWebSocket.State = wsOpen;

  ButtonConnect.Enabled := not Connected;
  ButtonDisconnect.Enabled := Connected;
  ButtonSend.Enabled := Connected;
  EditMessage.Enabled := Connected;
  EditURL.Enabled := not Connected;
end;

end.
```

## Serveur WebSocket simple

```pascal
unit WebSocketServer;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IdHTTPServer, IdContext, IdCustomHTTPServer, IdHashSHA,
  System.NetEncoding, System.SyncObjs;

type
  TWebSocketConnection = class
  private
    FContext: TIdContext;
    FID: string;
  public
    constructor Create(AContext: TIdContext);

    procedure Send(const Message: string);
    procedure Close;

    property ID: string read FID;
    property Context: TIdContext read FContext;
  end;

  TWebSocketServer = class
  private
    FHTTPServer: TIdHTTPServer;
    FConnections: TObjectList<TWebSocketConnection>;
    FLock: TCriticalSection;
    FOnClientConnect: TProc<string>;
    FOnClientDisconnect: TProc<string>;
    FOnMessage: TProc<string, string>; // ClientID, Message

    procedure HTTPServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HandleWebSocketFrame(Connection: TWebSocketConnection);
  public
    constructor Create(Port: Integer);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
    procedure Broadcast(const Message: string);
    procedure SendTo(const ClientID, Message: string);

    property OnClientConnect: TProc<string> read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TProc<string> read FOnClientDisconnect write FOnClientDisconnect;
    property OnMessage: TProc<string, string> read FOnMessage write FOnMessage;
  end;

implementation

{ TWebSocketConnection }

constructor TWebSocketConnection.Create(AContext: TIdContext);
begin
  inherited Create;
  FContext := AContext;
  FID := TGUID.NewGuid.ToString;
end;

procedure TWebSocketConnection.Send(const Message: string);
var
  Frame: TBytes;
  MessageBytes: TBytes;
  PayloadLength: Int64;
begin
  MessageBytes := TEncoding.UTF8.GetBytes(Message);
  PayloadLength := Length(MessageBytes);

  // Construire la frame
  if PayloadLength < 126 then
    SetLength(Frame, 2 + PayloadLength)
  else if PayloadLength < 65536 then
    SetLength(Frame, 4 + PayloadLength)
  else
    SetLength(Frame, 10 + PayloadLength);

  // Byte 1: FIN + Text opcode
  Frame[0] := $81;

  // Byte 2+: Payload length (serveur ne masque pas)
  if PayloadLength < 126 then
  begin
    Frame[1] := Byte(PayloadLength);
    Move(MessageBytes[0], Frame[2], PayloadLength);
  end
  else if PayloadLength < 65536 then
  begin
    Frame[1] := 126;
    Frame[2] := (PayloadLength shr 8) and $FF;
    Frame[3] := PayloadLength and $FF;
    Move(MessageBytes[0], Frame[4], PayloadLength);
  end;

  FContext.Connection.IOHandler.Write(TIdBytes(Frame));
end;

procedure TWebSocketConnection.Close;
var
  CloseFrame: TBytes;
begin
  SetLength(CloseFrame, 2);
  CloseFrame[0] := $88; // Close opcode
  CloseFrame[1] := $00;

  FContext.Connection.IOHandler.Write(TIdBytes(CloseFrame));
  FContext.Connection.Disconnect;
end;

{ TWebSocketServer }

constructor TWebSocketServer.Create(Port: Integer);
begin
  inherited Create;

  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := Port;
  FHTTPServer.OnCommandGet := HTTPServerCommandGet;

  FConnections := TObjectList<TWebSocketConnection>.Create(True);
  FLock := TCriticalSection.Create;
end;

destructor TWebSocketServer.Destroy;
begin
  Stop;
  FConnections.Free;
  FLock.Free;
  FHTTPServer.Free;
  inherited;
end;

procedure TWebSocketServer.Start;
begin
  FHTTPServer.Active := True;
end;

procedure TWebSocketServer.Stop;
begin
  FHTTPServer.Active := False;

  FLock.Enter;
  try
    FConnections.Clear;
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketServer.HTTPServerCommandGet(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Key, Accept: string;
  Connection: TWebSocketConnection;
begin
  // Vérifier si c'est une requête WebSocket
  if (ARequestInfo.Connection.ToUpper = 'UPGRADE') and
     (ARequestInfo.Upgrade.ToUpper = 'WEBSOCKET') then
  begin
    // Récupérer la clé
    Key := ARequestInfo.RawHeaders.Values['Sec-WebSocket-Key'];

    // Calculer l'Accept
    Accept := TNetEncoding.Base64.Encode(
      TIdHashSHA1.HashString(Key + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));

    // Réponse de handshake
    AResponseInfo.ResponseNo := 101;
    AResponseInfo.ResponseText := 'Switching Protocols';
    AResponseInfo.CustomHeaders.AddValue('Upgrade', 'websocket');
    AResponseInfo.CustomHeaders.AddValue('Connection', 'Upgrade');
    AResponseInfo.CustomHeaders.AddValue('Sec-WebSocket-Accept', Accept);

    AResponseInfo.WriteHeader;

    // Créer la connexion WebSocket
    Connection := TWebSocketConnection.Create(AContext);

    FLock.Enter;
    try
      FConnections.Add(Connection);
    finally
      FLock.Leave;
    end;

    if Assigned(FOnClientConnect) then
      TThread.Synchronize(nil, procedure begin FOnClientConnect(Connection.ID); end);

    // Gérer les frames
    try
      while AContext.Connection.Connected do
        HandleWebSocketFrame(Connection);
    finally
      FLock.Enter;
      try
        FConnections.Remove(Connection);
      finally
        FLock.Leave;
      end;

      if Assigned(FOnClientDisconnect) then
        TThread.Synchronize(nil, procedure begin FOnClientDisconnect(Connection.ID); end);
    end;
  end
  else
  begin
    // Requête HTTP normale
    AResponseInfo.ContentText := '<html><body>WebSocket Server</body></html>';
  end;
end;

procedure TWebSocketServer.HandleWebSocketFrame(Connection: TWebSocketConnection);
var
  Byte1, Byte2: Byte;
  OpCode: Byte;
  Masked: Boolean;
  PayloadLength: Int64;
  MaskingKey: array[0..3] of Byte;
  Payload: TBytes;
  i: Integer;
  Message: string;
begin
  Byte1 := Connection.Context.Connection.IOHandler.ReadByte;
  Byte2 := Connection.Context.Connection.IOHandler.ReadByte;

  OpCode := Byte1 and $0F;
  Masked := (Byte2 and $80) <> 0;
  PayloadLength := Byte2 and $7F;

  if PayloadLength = 126 then
    PayloadLength := Connection.Context.Connection.IOHandler.ReadWord
  else if PayloadLength = 127 then
    PayloadLength := Connection.Context.Connection.IOHandler.ReadInt64;

  if Masked then
    Connection.Context.Connection.IOHandler.ReadBytes(TIdBytes(MaskingKey), 4, False);

  SetLength(Payload, PayloadLength);
  if PayloadLength > 0 then
    Connection.Context.Connection.IOHandler.ReadBytes(TIdBytes(Payload), PayloadLength, False);

  if Masked then
    for i := 0 to High(Payload) do
      Payload[i] := Payload[i] xor MaskingKey[i mod 4];

  case OpCode of
    $01: // Text
      begin
        Message := TEncoding.UTF8.GetString(Payload);
        if Assigned(FOnMessage) then
          TThread.Synchronize(nil, procedure
          begin
            FOnMessage(Connection.ID, Message);
          end);
      end;

    $08: // Close
      Connection.Close;

    $09: // Ping
      begin
        // Répondre avec Pong
        Payload[0] := $8A; // Pong opcode
        Connection.Context.Connection.IOHandler.Write(TIdBytes(Payload));
      end;
  end;
end;

procedure TWebSocketServer.Broadcast(const Message: string);
var
  Connection: TWebSocketConnection;
begin
  FLock.Enter;
  try
    for Connection in FConnections do
    begin
      try
        Connection.Send(Message);
      except
        // Ignorer les erreurs d'envoi
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketServer.SendTo(const ClientID, Message: string);
var
  Connection: TWebSocketConnection;
begin
  FLock.Enter;
  try
    for Connection in FConnections do
    begin
      if Connection.ID = ClientID then
      begin
        Connection.Send(Message);
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

end.
```

**Utilisation du serveur :**

```pascal
var
  Server: TWebSocketServer;
begin
  Server := TWebSocketServer.Create(8080);
  try
    Server.OnClientConnect := procedure(const ID: string)
    begin
      Memo1.Lines.Add('Client connecté: ' + ID);
    end;

    Server.OnClientDisconnect := procedure(const ID: string)
    begin
      Memo1.Lines.Add('Client déconnecté: ' + ID);
    end;

    Server.OnMessage := procedure(const ID, Message: string)
    begin
      Memo1.Lines.Add(ID + ': ' + Message);

      // Echo le message à tous les clients
      Server.Broadcast('Echo: ' + Message);
    end;

    Server.Start;
    ShowMessage('Serveur démarré sur le port 8080');

    ReadLn; // Attendre

  finally
    Server.Free;
  end;
end;
```

## Application de Chat complète

```pascal
unit ChatApplication;

interface

uses
  System.SysUtils, System.Classes, System.JSON, WebSocketClient;

type
  TChatMessage = record
    Username: string;
    Message: string;
    Timestamp: TDateTime;
  end;

  TChatClient = class
  private
    FWebSocket: TWebSocketClient;
    FUsername: string;
    FOnChatMessage: TProc<TChatMessage>;
    FOnUserJoined: TProc<string>;
    FOnUserLeft: TProc<string>;

    procedure HandleMessage(const JSONMessage: string);
  public
    constructor Create(const ServerURL, Username: string);
    destructor Destroy; override;

    procedure SendMessage(const Message: string);
    procedure Disconnect;

    property OnChatMessage: TProc<TChatMessage> read FOnChatMessage write FOnChatMessage;
    property OnUserJoined: TProc<string> read FOnUserJoined write FOnUserJoined;
    property OnUserLeft: TProc<string> read FOnUserLeft write FOnUserLeft;
  end;

implementation

constructor TChatClient.Create(const ServerURL, Username: string);
var
  JoinMessage: TJSONObject;
begin
  inherited Create;
  FUsername := Username;

  FWebSocket := TWebSocketClient.Create;
  FWebSocket.OnMessage := HandleMessage;

  FWebSocket.Connect(ServerURL);

  // Envoyer message de join
  JoinMessage := TJSONObject.Create;
  try
    JoinMessage.AddPair('type', 'join');
    JoinMessage.AddPair('username', Username);

    FWebSocket.Send(JoinMessage.ToString);
  finally
    JoinMessage.Free;
  end;
end;

destructor TChatClient.Destroy;
begin
  FWebSocket.Free;
  inherited;
end;

procedure TChatClient.HandleMessage(const JSONMessage: string);
var
  JSON: TJSONObject;
  MessageType: string;
  ChatMsg: TChatMessage;
  User: string;
begin
  JSON := TJSONObject.ParseJSONValue(JSONMessage) as TJSONObject;
  try
    MessageType := JSON.GetValue<string>('type');

    if MessageType = 'message' then
    begin
      ChatMsg.Username := JSON.GetValue<string>('username');
      ChatMsg.Message := JSON.GetValue<string>('message');
      ChatMsg.Timestamp := Now; // Ou parser depuis JSON

      if Assigned(FOnChatMessage) then
        FOnChatMessage(ChatMsg);
    end
    else if MessageType = 'join' then
    begin
      User := JSON.GetValue<string>('username');
      if Assigned(FOnUserJoined) then
        FOnUserJoined(User);
    end
    else if MessageType = 'leave' then
    begin
      User := JSON.GetValue<string>('username');
      if Assigned(FOnUserLeft) then
        FOnUserLeft(User);
    end;

  finally
    JSON.Free;
  end;
end;

procedure TChatClient.SendMessage(const Message: string);
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', 'message');
    JSON.AddPair('username', FUsername);
    JSON.AddPair('message', Message);

    FWebSocket.Send(JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TChatClient.Disconnect;
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', 'leave');
    JSON.AddPair('username', FUsername);

    FWebSocket.Send(JSON.ToString);
  finally
    JSON.Free;
  end;

  FWebSocket.Close;
end;

end.
```

## Reconnexion automatique

```pascal
type
  TAutoReconnectWebSocket = class(TWebSocketClient)
  private
    FAutoReconnect: Boolean;
    FReconnectDelay: Integer;
    FMaxRetries: Integer;
    FRetryCount: Integer;
    FURL: string;
    FReconnectTimer: TTimer;

    procedure OnTimerReconnect(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ConnectWithRetry(const URL: string);

    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;
    property ReconnectDelay: Integer read FReconnectDelay write FReconnectDelay;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries;
  end;

implementation

constructor TAutoReconnectWebSocket.Create;
begin
  inherited;
  FAutoReconnect := True;
  FReconnectDelay := 3000; // 3 secondes
  FMaxRetries := 5;
  FRetryCount := 0;

  FReconnectTimer := TTimer.Create(nil);
  FReconnectTimer.Enabled := False;
  FReconnectTimer.OnTimer := OnTimerReconnect;
end;

destructor TAutoReconnectWebSocket.Destroy;
begin
  FReconnectTimer.Free;
  inherited;
end;

procedure TAutoReconnectWebSocket.ConnectWithRetry(const URL: string);
begin
  FURL := URL;
  FRetryCount := 0;

  try
    Connect(URL);
  except
    on E: Exception do
    begin
      if FAutoReconnect and (FRetryCount < FMaxRetries) then
      begin
        Inc(FRetryCount);
        FReconnectTimer.Interval := FReconnectDelay;
        FReconnectTimer.Enabled := True;
      end
      else
        raise;
    end;
  end;
end;

procedure TAutoReconnectWebSocket.OnTimerReconnect(Sender: TObject);
begin
  FReconnectTimer.Enabled := False;

  try
    Connect(FURL);
    FRetryCount := 0; // Reset sur succès
  except
    if FRetryCount < FMaxRetries then
    begin
      Inc(FRetryCount);
      FReconnectTimer.Enabled := True;
    end;
  end;
end;
```

## Heartbeat (Ping/Pong)

```pascal
type
  THeartbeatWebSocket = class(TWebSocketClient)
  private
    FHeartbeatTimer: TTimer;
    FLastPong: TDateTime;
    FPingInterval: Integer;
    FPingTimeout: Integer;

    procedure OnHeartbeatTimer(Sender: TObject);
    procedure SendPing;
  public
    constructor Create;
    destructor Destroy; override;

    procedure StartHeartbeat;
    procedure StopHeartbeat;

    property PingInterval: Integer read FPingInterval write FPingInterval;
    property PingTimeout: Integer read FPingTimeout write FPingTimeout;
  end;

implementation

constructor THeartbeatWebSocket.Create;
begin
  inherited;
  FPingInterval := 30000; // 30 secondes
  FPingTimeout := 5000;   // 5 secondes

  FHeartbeatTimer := TTimer.Create(nil);
  FHeartbeatTimer.Enabled := False;
  FHeartbeatTimer.OnTimer := OnHeartbeatTimer;
end;

destructor THeartbeatWebSocket.Destroy;
begin
  FHeartbeatTimer.Free;
  inherited;
end;

procedure THeartbeatWebSocket.StartHeartbeat;
begin
  FLastPong := Now;
  FHeartbeatTimer.Interval := FPingInterval;
  FHeartbeatTimer.Enabled := True;
end;

procedure THeartbeatWebSocket.StopHeartbeat;
begin
  FHeartbeatTimer.Enabled := False;
end;

procedure THeartbeatWebSocket.OnHeartbeatTimer(Sender: TObject);
begin
  // Vérifier si le dernier pong est trop ancien
  if MilliSecondsBetween(Now, FLastPong) > (FPingInterval + FPingTimeout) then
  begin
    // Connexion probablement morte
    if Assigned(OnError) then
      OnError('Heartbeat timeout');

    Close;
  end
  else
  begin
    SendPing;
  end;
end;

procedure THeartbeatWebSocket.SendPing;
var
  PingFrame: TBytes;
begin
  SetLength(PingFrame, 2);
  PingFrame[0] := $89; // Ping opcode
  PingFrame[1] := $80; // Masked, no payload

  Send(PingFrame);
end;
```

## Bonnes pratiques

### 1. Toujours utiliser WSS (WebSocket Secure)

```pascal
// ✅ Bon - Connexion sécurisée
WebSocket.Connect('wss://secure.example.com/chat');

// ❌ Éviter en production
WebSocket.Connect('ws://insecure.example.com/chat');
```

### 2. Implémenter le heartbeat

```pascal
// Détecter les connexions mortes
procedure ConfigurerHeartbeat;
begin
  HeartbeatWS.PingInterval := 30000; // 30s
  HeartbeatWS.PingTimeout := 5000;   // 5s
  HeartbeatWS.StartHeartbeat;
end;
```

### 3. Gérer la reconnexion

```pascal
// Reconnexion automatique avec backoff exponentiel
var
  RetryDelay: Integer;
begin
  RetryDelay := 1000; // 1 seconde

  while not Connected do
  begin
    try
      WebSocket.Connect(URL);
    except
      Sleep(RetryDelay);
      RetryDelay := Min(RetryDelay * 2, 60000); // Max 60 secondes
    end;
  end;
end;
```

### 4. Limiter la taille des messages

```pascal
const
  MAX_MESSAGE_SIZE = 1024 * 1024; // 1 MB

procedure SendSafe(const Message: string);
begin
  if Length(Message) > MAX_MESSAGE_SIZE then
    raise Exception.Create('Message trop grand');

  WebSocket.Send(Message);
end;
```

### 5. Valider les messages JSON

```pascal
procedure HandleMessage(const Message: string);
var
  JSON: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Message) as TJSONObject;
    try
      // Valider la structure
      if not JSON.TryGetValue<string>('type') then
        raise Exception.Create('Champ "type" manquant');

      // Traiter...
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      // Logger mais ne pas crasher
      LogError('Message invalide: ' + E.Message);
  end;
end;
```

### 6. Gérer les ressources

```pascal
procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Fermer proprement
  if Assigned(FWebSocket) then
  begin
    FWebSocket.Close;
    FWebSocket.Free;
  end;
end;
```

### 7. Logger les événements

```pascal
procedure LogWebSocketEvent(const Event, Details: string);
begin
  WriteLn(Format('[%s] WebSocket %s: %s',
    [DateTimeToStr(Now), Event, Details]));
end;

// Utilisation
WebSocket.OnConnect := procedure
begin
  LogWebSocketEvent('CONNECT', 'Connexion établie');
end;

WebSocket.OnMessage := procedure(const Msg: string)
begin
  LogWebSocketEvent('MESSAGE', Msg);
end;
```

## Résumé

### Points clés WebSocket

✅ **Concepts fondamentaux :**
- **WebSocket** = Connexion bidirectionnelle persistante
- **Handshake** = Upgrade HTTP → WebSocket
- **Frames** = Format de messages léger
- **Opcodes** = Text, Binary, Close, Ping, Pong

✅ **Avantages :**
- Communication temps réel
- Latence minimale
- Overhead réduit vs polling
- Push serveur natif
- Bidirectionnel

✅ **Cas d'usage :**
- Chat en direct
- Dashboards temps réel
- Jeux multijoueurs
- Notifications push
- Collaboration temps réel

✅ **Implémentation Delphi :**
- Client avec Indy (TIdTCPClient)
- Serveur avec TIdHTTPServer
- Gestion des frames manuellement
- Threading pour réception asynchrone

✅ **Bonnes pratiques :**
- Toujours WSS (sécurisé)
- Heartbeat (ping/pong)
- Reconnexion automatique
- Limiter taille des messages
- Valider les données JSON
- Logger les événements
- Gérer les ressources proprement

✅ **Sécurité :**
- WSS obligatoire en production
- Authentification par token
- Validation des messages
- Rate limiting côté serveur
- CORS configuré correctement

WebSocket révolutionne les applications temps réel en permettant une communication instantanée et efficace entre client et serveur. C'est le standard pour toute application nécessitant des mises à jour en direct !

⏭️ [Multithreading et programmation asynchrone](/11-multithreading-et-programmation-asynchrone/README.md)
