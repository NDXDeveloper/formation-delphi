# 10.10 WebSockets et communications temps réel

## Introduction

Dans le développement d'applications modernes, la communication en temps réel est devenue essentielle. Que ce soit pour des applications de messagerie instantanée, des tableaux de bord en direct, des jeux multi-joueurs ou des applications IoT (Internet des Objets), la capacité à échanger des données instantanément entre le client et le serveur est primordiale.

Dans ce chapitre, nous allons explorer la technologie WebSocket et d'autres méthodes de communication en temps réel que vous pouvez intégrer dans vos applications Delphi. Nous commencerons par les concepts de base avant de nous plonger dans des exemples concrets.

## Qu'est-ce que WebSocket ?

WebSocket est un protocole de communication qui permet l'établissement d'une connexion bidirectionnelle persistante entre un client et un serveur. Contrairement au modèle traditionnel HTTP requête-réponse, WebSocket maintient la connexion ouverte, permettant ainsi l'échange de données dans les deux sens sans avoir à établir une nouvelle connexion à chaque fois.

![Comparaison HTTP vs WebSocket](https://via.placeholder.com/800x400)

Les avantages des WebSockets incluent :

- **Communication bidirectionnelle** : Les données peuvent circuler du serveur vers le client et vice versa.
- **Faible latence** : Moins de surcharge car la connexion reste ouverte.
- **Temps réel** : Idéal pour les applications nécessitant des mises à jour instantanées.
- **Efficacité** : Moins de bande passante utilisée par rapport au polling HTTP.

## WebSockets dans Delphi

### Bibliothèques disponibles

Pour travailler avec WebSockets dans Delphi, plusieurs bibliothèques sont disponibles :

1. **Indy (Internet Direct)** : Inclus dans Delphi, mais nécessite des extensions pour le support WebSocket.
2. **sgcWebSockets** : Une bibliothèque tierce complète pour WebSockets.
3. **ICS (Internet Component Suite)** : Une autre bibliothèque tierce avec support WebSocket.
4. **Delphi REST Components** avec la nouvelle classe `TRESTClient` qui supporte WebSocket.

Dans ce chapitre, nous allons principalement utiliser sgcWebSockets car elle offre une bonne combinaison de facilité d'utilisation et de fonctionnalités avancées.

### Installation de sgcWebSockets

Pour commencer à utiliser sgcWebSockets :

1. Téléchargez la bibliothèque depuis [https://www.esegece.com/websockets](https://www.esegece.com/websockets) ou installez-la via GetIt Package Manager.
2. Dans Delphi, allez dans `Component > Install Packages`.
3. Cliquez sur `Add` et sélectionnez le fichier `.dpk` de sgcWebSockets correspondant à votre version de Delphi.
4. Compilez et installez le package.

Une fois installée, vous verrez de nouveaux composants dans votre palette de composants.

## Création d'un client WebSocket simple

Commençons par créer une application cliente WebSocket simple qui se connecte à un serveur WebSocket public d'écho, qui renvoie simplement les messages que vous lui envoyez.

### Étape 1 : Créer un nouveau projet VCL

Créez une nouvelle application VCL Forms dans Delphi.

### Étape 2 : Ajouter les composants à votre formulaire

Ajoutez les composants suivants à votre formulaire :

1. Un `TsgcWebSocketClient` (depuis la palette de composants après installation).
2. Un `TMemo` pour afficher les messages reçus.
3. Un `TEdit` pour saisir les messages à envoyer.
4. Un `TButton` pour envoyer les messages.
5. Un autre `TButton` pour se connecter/déconnecter.
6. Un `TLabel` pour afficher l'état de la connexion.

Votre formulaire pourrait ressembler à ceci :

```
[Label: État de la connexion: Déconnecté]
[Button: Connecter]

[Memo pour les messages reçus]

[Edit pour la saisie]   [Button: Envoyer]
```

### Étape 3 : Configurer le composant WebSocket

Double-cliquez sur le composant `TsgcWebSocketClient` et configurez-le comme suit :

```pascal
// Définir l'URL du serveur WebSocket d'écho
sgcWebSocketClient1.URL := 'wss://echo.websocket.org';

// Activer les options TLS pour une connexion sécurisée (wss://)
sgcWebSocketClient1.TLS := True;

// Définir les événements
sgcWebSocketClient1.OnConnect := sgcWebSocketClient1Connect;
sgcWebSocketClient1.OnDisconnect := sgcWebSocketClient1Disconnect;
sgcWebSocketClient1.OnMessage := sgcWebSocketClient1Message;
sgcWebSocketClient1.OnException := sgcWebSocketClient1Exception;
```

### Étape 4 : Implémenter les gestionnaires d'événements

Maintenant, implémentez les gestionnaires d'événements pour traiter les différents événements WebSocket.

```pascal
procedure TForm1.sgcWebSocketClient1Connect(Connection: TsgcWSConnection);
begin
  // Mettre à jour l'interface utilisateur lorsque connecté
  lblStatus.Caption := 'État de la connexion: Connecté';
  btnConnect.Caption := 'Déconnecter';
  Memo1.Lines.Add('Connecté au serveur WebSocket');
end;

procedure TForm1.sgcWebSocketClient1Disconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  // Mettre à jour l'interface utilisateur lorsque déconnecté
  lblStatus.Caption := 'État de la connexion: Déconnecté';
  btnConnect.Caption := 'Connecter';
  Memo1.Lines.Add('Déconnecté du serveur WebSocket. Code: ' + IntToStr(Code));
end;

procedure TForm1.sgcWebSocketClient1Exception(Connection: TsgcWSConnection; E: Exception);
begin
  // Gérer les exceptions
  Memo1.Lines.Add('Erreur: ' + E.Message);
end;

procedure TForm1.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  // Traiter les messages reçus
  Memo1.Lines.Add('Reçu: ' + Text);
end;
```

### Étape 5 : Implémenter les boutons

```pascal
procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if sgcWebSocketClient1.Active then
  begin
    // Si déjà connecté, déconnecter
    sgcWebSocketClient1.Active := False;
  end
  else
  begin
    // Sinon, se connecter
    sgcWebSocketClient1.Active := True;
  end;
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  // Vérifier si connecté et si le message n'est pas vide
  if sgcWebSocketClient1.Active and (Trim(Edit1.Text) <> '') then
  begin
    // Envoyer le message
    sgcWebSocketClient1.WriteData(Edit1.Text);

    // Afficher le message envoyé dans le mémo
    Memo1.Lines.Add('Envoyé: ' + Edit1.Text);

    // Effacer le champ de saisie
    Edit1.Text := '';
  end
  else if not sgcWebSocketClient1.Active then
  begin
    ShowMessage('Veuillez vous connecter d''abord au serveur WebSocket');
  end;
end;
```

### Étape 6 : Tester l'application

Compilez et exécutez votre application. Cliquez sur le bouton "Connecter" pour établir une connexion WebSocket avec le serveur d'écho. Une fois connecté, saisissez un message dans le champ de texte et cliquez sur "Envoyer". Vous devriez voir votre message apparaître dans le mémo, suivi du même message renvoyé par le serveur.

## Création d'un serveur WebSocket

Maintenant, créons un serveur WebSocket simple qui pourra accepter les connexions des clients WebSocket.

### Étape 1 : Créer un nouveau projet VCL

Créez une nouvelle application VCL Forms pour le serveur.

### Étape 2 : Ajouter les composants au formulaire

Ajoutez les composants suivants à votre formulaire :

1. Un `TsgcWebSocketServer` (depuis la palette de composants).
2. Un `TMemo` pour afficher les journaux.
3. Un `TButton` pour démarrer/arrêter le serveur.
4. Un `TEdit` pour spécifier le port d'écoute.
5. Un `TLabel` pour afficher l'état du serveur.

### Étape 3 : Configurer le composant serveur WebSocket

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration initiale du serveur
  sgcWebSocketServer1.Port := 8080;
  sgcWebSocketServer1.NotifyEvents := neAsynchronous; // Événements asynchrones
  sgcWebSocketServer1.WatchdogInterval := 10; // Intervalle de surveillance en secondes

  // Mettre à jour le champ de port
  edtPort.Text := IntToStr(sgcWebSocketServer1.Port);

  // Définir les événements
  sgcWebSocketServer1.OnStart := sgcWebSocketServer1Start;
  sgcWebSocketServer1.OnStop := sgcWebSocketServer1Stop;
  sgcWebSocketServer1.OnConnect := sgcWebSocketServer1Connect;
  sgcWebSocketServer1.OnDisconnect := sgcWebSocketServer1Disconnect;
  sgcWebSocketServer1.OnMessage := sgcWebSocketServer1Message;
  sgcWebSocketServer1.OnError := sgcWebSocketServer1Error;
end;
```

### Étape 4 : Implémenter les gestionnaires d'événements

```pascal
procedure TForm1.sgcWebSocketServer1Start(Sender: TObject);
begin
  // Mettre à jour l'interface quand le serveur démarre
  lblStatus.Caption := 'État du serveur: En cours d''exécution';
  btnStartStop.Caption := 'Arrêter le serveur';
  edtPort.Enabled := False;
  Memo1.Lines.Add('Serveur démarré sur le port ' + IntToStr(sgcWebSocketServer1.Port));
end;

procedure TForm1.sgcWebSocketServer1Stop(Sender: TObject);
begin
  // Mettre à jour l'interface quand le serveur s'arrête
  lblStatus.Caption := 'État du serveur: Arrêté';
  btnStartStop.Caption := 'Démarrer le serveur';
  edtPort.Enabled := True;
  Memo1.Lines.Add('Serveur arrêté');
end;

procedure TForm1.sgcWebSocketServer1Connect(Connection: TsgcWSConnection);
begin
  // Journaliser quand un client se connecte
  Memo1.Lines.Add('Client connecté: ' + Connection.Guid);
end;

procedure TForm1.sgcWebSocketServer1Disconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  // Journaliser quand un client se déconnecte
  Memo1.Lines.Add('Client déconnecté: ' + Connection.Guid + ' avec le code: ' + IntToStr(Code));
end;

procedure TForm1.sgcWebSocketServer1Error(Connection: TsgcWSConnection; const Error: string);
begin
  // Gérer les erreurs
  Memo1.Lines.Add('Erreur: ' + Error);
end;

procedure TForm1.sgcWebSocketServer1Message(Connection: TsgcWSConnection; const Text: string);
begin
  // Traiter les messages reçus
  Memo1.Lines.Add('Message reçu de ' + Connection.Guid + ': ' + Text);

  // Exemple: Renvoyer le message à tous les clients connectés (broadcast)
  sgcWebSocketServer1.Broadcast(Text);

  // Ou renvoyer uniquement à l'expéditeur
  // Connection.WriteData('Écho: ' + Text);
end;
```

### Étape 5 : Implémenter le bouton Démarrer/Arrêter

```pascal
procedure TForm1.btnStartStopClick(Sender: TObject);
begin
  if sgcWebSocketServer1.Active then
  begin
    // Si le serveur est actif, l'arrêter
    sgcWebSocketServer1.Active := False;
  end
  else
  begin
    // Sinon, définir le port et démarrer le serveur
    try
      sgcWebSocketServer1.Port := StrToInt(edtPort.Text);
      sgcWebSocketServer1.Active := True;
    except
      on E: Exception do
        ShowMessage('Erreur lors du démarrage du serveur: ' + E.Message);
    end;
  end;
end;
```

### Étape 6 : Tester le serveur

Compilez et exécutez l'application serveur. Cliquez sur le bouton "Démarrer le serveur" pour lancer le serveur WebSocket sur le port spécifié. Vous pouvez maintenant modifier votre application cliente pour vous connecter à ce serveur local au lieu du serveur d'écho public.

## Communication client-serveur

Maintenant que nous avons créé un client et un serveur WebSocket, voyons comment les faire communiquer.

### Modifier le client pour se connecter au serveur local

Dans l'application cliente, modifiez l'URL du WebSocket pour vous connecter à votre serveur local :

```pascal
sgcWebSocketClient1.URL := 'ws://localhost:8080';
sgcWebSocketClient1.TLS := False; // Désactivez TLS pour une connexion locale non sécurisée
```

Lorsque vous envoyez un message depuis le client, le serveur va le recevoir et le diffuser à tous les clients connectés (y compris l'expéditeur).

## Formats de données pour WebSocket

Les WebSockets peuvent transmettre des données sous forme de texte ou binaire. Pour les applications modernes, il est courant d'utiliser JSON pour structurer les données textuelles.

### Envoi de données JSON

```pascal
procedure TForm1.SendJsonMessage(const MsgType, Content: string);
var
  JSON: TJSONObject;
begin
  // Créer un objet JSON
  JSON := TJSONObject.Create;
  try
    // Ajouter des propriétés
    JSON.AddPair('type', MsgType);
    JSON.AddPair('content', Content);
    JSON.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

    // Envoyer le JSON sous forme de chaîne
    sgcWebSocketClient1.WriteData(JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TForm1.btnSendJsonClick(Sender: TObject);
begin
  SendJsonMessage('message', Edit1.Text);
  Edit1.Text := '';
end;
```

### Réception et traitement de données JSON

```pascal
procedure TForm1.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
var
  JSON: TJSONObject;
  MessageType, Content, Timestamp: string;
begin
  try
    // Analyser le texte en JSON
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      // Extraire les valeurs
      if JSON.TryGetValue<string>('type', MessageType) and
         JSON.TryGetValue<string>('content', Content) and
         JSON.TryGetValue<string>('timestamp', Timestamp) then
      begin
        // Afficher le message formaté
        Memo1.Lines.Add(Format('[%s] %s: %s', [Timestamp, MessageType, Content]));
      end
      else
      begin
        // Format JSON incorrect
        Memo1.Lines.Add('Message reçu avec format incorrect: ' + Text);
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
    begin
      // Erreur d'analyse JSON
      Memo1.Lines.Add('Erreur d''analyse JSON: ' + E.Message);
      Memo1.Lines.Add('Texte reçu: ' + Text);
    end;
  end;
end;
```

## Exemple concret : Application de chat

Maintenant, assemblons tout pour créer une application de chat simple.

### Interface utilisateur du client de chat

Pour l'application cliente, créez une interface avec :

1. Une zone de liste (`TListBox` ou `TListView`) pour afficher les utilisateurs connectés
2. Un `TMemo` pour afficher les messages du chat
3. Un `TEdit` pour saisir les messages
4. Un `TButton` pour envoyer les messages
5. Un `TEdit` pour entrer un nom d'utilisateur
6. Un `TButton` pour se connecter/déconnecter

### Types de messages JSON pour le chat

Définissons plusieurs types de messages pour notre application de chat :

1. **login** : Pour s'identifier auprès du serveur
2. **message** : Pour envoyer un message de chat
3. **userlist** : Pour recevoir la liste des utilisateurs
4. **join** : Pour être notifié qu'un utilisateur a rejoint
5. **leave** : Pour être notifié qu'un utilisateur est parti

### Implémentation du client de chat

```pascal
unit MainFormClient;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, sgcWebSocket_Classes,
  sgcWebSocket_Client, sgcWebSocket, System.JSON;

type
  TfrmChatClient = class(TForm)
    sgcWebSocketClient1: TsgcWebSocketClient;
    lbUsers: TListBox;
    memoChat: TMemo;
    edtMessage: TEdit;
    btnSend: TButton;
    edtUsername: TEdit;
    btnConnect: TButton;
    lblStatus: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure sgcWebSocketClient1Connect(Connection: TsgcWSConnection);
    procedure sgcWebSocketClient1Disconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure FormCreate(Sender: TObject);
  private
    FConnected: Boolean;
    FUsername: string;
    procedure SendJson(const MessageType, Content: string);
    procedure ProcessUserList(const UserListJson: TJSONArray);
    procedure UpdateUIState;
  public

  end;

var
  frmChatClient: TfrmChatClient;

implementation

{$R *.dfm}

procedure TfrmChatClient.FormCreate(Sender: TObject);
begin
  FConnected := False;
  UpdateUIState;

  // Configuration du client WebSocket
  sgcWebSocketClient1.URL := 'ws://localhost:8080';
  sgcWebSocketClient1.TLS := False;
end;

procedure TfrmChatClient.UpdateUIState;
begin
  btnConnect.Caption := IfThen(FConnected, 'Déconnecter', 'Connecter');
  edtUsername.Enabled := not FConnected;
  btnSend.Enabled := FConnected;
  edtMessage.Enabled := FConnected;

  if FConnected then
    lblStatus.Caption := 'Connecté en tant que: ' + FUsername
  else
    lblStatus.Caption := 'Déconnecté';
end;

procedure TfrmChatClient.btnConnectClick(Sender: TObject);
begin
  if FConnected then
  begin
    // Déconnexion
    sgcWebSocketClient1.Active := False;
  end
  else
  begin
    // Connexion
    if Trim(edtUsername.Text) = '' then
    begin
      ShowMessage('Veuillez entrer un nom d''utilisateur');
      Exit;
    end;

    FUsername := Trim(edtUsername.Text);
    sgcWebSocketClient1.Active := True;
  end;
end;

procedure TfrmChatClient.sgcWebSocketClient1Connect(Connection: TsgcWSConnection);
begin
  FConnected := True;
  UpdateUIState;

  // S'identifier auprès du serveur
  SendJson('login', FUsername);

  memoChat.Lines.Add('* Connecté au serveur de chat *');
end;

procedure TfrmChatClient.sgcWebSocketClient1Disconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  FConnected := False;
  UpdateUIState;

  lbUsers.Items.Clear();
  memoChat.Lines.Add('* Déconnecté du serveur de chat *');
end;

procedure TfrmChatClient.SendJson(const MessageType, Content: string);
var
  JSON: TJSONObject;
begin
  if not FConnected then
    Exit;

  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', MessageType);
    JSON.AddPair('content', Content);
    JSON.AddPair('username', FUsername);

    sgcWebSocketClient1.WriteData(JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TfrmChatClient.btnSendClick(Sender: TObject);
var
  Message: string;
begin
  Message := Trim(edtMessage.Text);
  if (Message <> '') and FConnected then
  begin
    SendJson('message', Message);
    edtMessage.Text := '';
  end;
end;

procedure TfrmChatClient.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
var
  JSON: TJSONObject;
  MessageType, Content, Username: string;
  UserList: TJSONArray;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      if JSON.TryGetValue<string>('type', MessageType) then
      begin
        if MessageType = 'message' then
        begin
          // Message de chat
          if JSON.TryGetValue<string>('content', Content) and
             JSON.TryGetValue<string>('username', Username) then
          begin
            memoChat.Lines.Add(Username + ': ' + Content);
          end;
        end
        else if MessageType = 'userlist' then
        begin
          // Liste d'utilisateurs
          if JSON.TryGetValue<TJSONArray>('users', UserList) then
          begin
            ProcessUserList(UserList);
          end;
        end
        else if MessageType = 'join' then
        begin
          // Un utilisateur a rejoint
          if JSON.TryGetValue<string>('username', Username) then
          begin
            memoChat.Lines.Add('* ' + Username + ' a rejoint le chat *');
          end;
        end
        else if MessageType = 'leave' then
        begin
          // Un utilisateur est parti
          if JSON.TryGetValue<string>('username', Username) then
          begin
            memoChat.Lines.Add('* ' + Username + ' a quitté le chat *');
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      memoChat.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TfrmChatClient.ProcessUserList(const UserListJson: TJSONArray);
var
  I: Integer;
  Username: string;
begin
  lbUsers.Items.Clear;

  for I := 0 to UserListJson.Count - 1 do
  begin
    Username := UserListJson.Items[I].Value;
    lbUsers.Items.Add(Username);
  end;
end;

end.
```

### Implémentation du serveur de chat

```pascal
unit MainFormServer;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, sgcWebSocket_Classes, sgcWebSocket_Server,
  sgcWebSocket, Vcl.StdCtrls, System.JSON, System.Generics.Collections;

type
  TUser = record
    Username: string;
    ConnectionGuid: string;
  end;

  TfrmChatServer = class(TForm)
    sgcWebSocketServer1: TsgcWebSocketServer;
    memoLog: TMemo;
    btnStartStop: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartStopClick(Sender: TObject);
    procedure sgcWebSocketServer1Connect(Connection: TsgcWSConnection);
    procedure sgcWebSocketServer1Disconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure sgcWebSocketServer1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgcWebSocketServer1Start(Sender: TObject);
    procedure sgcWebSocketServer1Stop(Sender: TObject);
  private
    FUsers: TDictionary<string, TUser>;

    procedure BroadcastUserList;
    procedure BroadcastJson(const MessageType, Content, Username: string);
    function GetUsernameByConnection(const ConnectionGuid: string): string;
  public

  end;

var
  frmChatServer: TfrmChatServer;

implementation

{$R *.dfm}

procedure TfrmChatServer.FormCreate(Sender: TObject);
begin
  // Initialiser la liste des utilisateurs
  FUsers := TDictionary<string, TUser>.Create;

  // Configuration du serveur WebSocket
  sgcWebSocketServer1.Port := 8080;
  sgcWebSocketServer1.NotifyEvents := neAsynchronous;

  lblStatus.Caption := 'Serveur arrêté';
end;

procedure TfrmChatServer.btnStartStopClick(Sender: TObject);
begin
  if sgcWebSocketServer1.Active then
    sgcWebSocketServer1.Active := False
  else
    sgcWebSocketServer1.Active := True;
end;

procedure TfrmChatServer.sgcWebSocketServer1Start(Sender: TObject);
begin
  lblStatus.Caption := 'Serveur démarré sur le port ' + IntToStr(sgcWebSocketServer1.Port);
  btnStartStop.Caption := 'Arrêter';
  memoLog.Lines.Add('Serveur démarré');
end;

procedure TfrmChatServer.sgcWebSocketServer1Stop(Sender: TObject);
begin
  lblStatus.Caption := 'Serveur arrêté';
  btnStartStop.Caption := 'Démarrer';
  memoLog.Lines.Add('Serveur arrêté');

  // Vider la liste des utilisateurs
  FUsers.Clear;
end;

procedure TfrmChatServer.sgcWebSocketServer1Connect(Connection: TsgcWSConnection);
begin
  memoLog.Lines.Add('Nouvelle connexion: ' + Connection.Guid);

  // L'utilisateur va s'identifier via un message de type "login"
end;

procedure TfrmChatServer.sgcWebSocketServer1Disconnect(Connection: TsgcWSConnection; Code: Integer);
var
  Username: string;
  User: TUser;
begin
  // Chercher l'utilisateur dans notre dictionnaire
  Username := GetUsernameByConnection(Connection.Guid);

  if Username <> '' then
  begin
    // Informer les autres qu'un utilisateur est parti
    BroadcastJson('leave', '', Username);

    // Supprimer l'utilisateur de notre liste
    FUsers.Remove(Connection.Guid);

    // Mettre à jour la liste des utilisateurs pour tout le monde
    BroadcastUserList;

    memoLog.Lines.Add('Utilisateur déconnecté: ' + Username);
  end;

  memoLog.Lines.Add('Connexion fermée: ' + Connection.Guid + ' avec code: ' + IntToStr(Code));
end;

function TfrmChatServer.GetUsernameByConnection(const ConnectionGuid: string): string;
var
  User: TUser;
begin
  Result := '';
  if FUsers.TryGetValue(ConnectionGuid, User) then
    Result := User.Username;
end;

procedure TfrmChatServer.BroadcastUserList;
var
  JSON: TJSONObject;
  UserArray: TJSONArray;
  Pair: TPair<string, TUser>;
begin
  JSON := TJSONObject.Create;
  UserArray := TJSONArray.Create;

  try
    // Ajouter chaque nom d'utilisateur au tableau JSON
    for Pair in FUsers do
      UserArray.Add(Pair.Value.Username);

    // Créer l'objet message complet
    JSON.AddPair('type', 'userlist');
    JSON.AddPair('users', UserArray);

    // Diffuser à tous les clients
    sgcWebSocketServer1.Broadcast(JSON.ToString);
  finally
    JSON.Free; // UserArray sera libéré par JSON
  end;
end;

procedure TfrmChatServer.BroadcastJson(const MessageType, Content, Username: string);
var
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('type', MessageType);

    if Content <> '' then
      JSON.AddPair('content', Content);

    if Username <> '' then
      JSON.AddPair('username', Username);

    sgcWebSocketServer1.Broadcast(JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TfrmChatServer.sgcWebSocketServer1Message(Connection: TsgcWSConnection; const Text: string);
var
  JSON: TJSONObject;
  MessageType, Content, Username: string;
  User: TUser;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      if JSON.TryGetValue<string>('type', MessageType) then
      begin
        if MessageType = 'login' then
        begin
          // Un utilisateur s'identifie
          if JSON.TryGetValue<string>('content', Username) then
          begin
            // Vérifier que le nom d'utilisateur n'est pas déjà utilisé
            var UsernameExists := False;
            for var Pair in FUsers do
            begin
              if SameText(Pair.Value.Username, Username) then
              begin
                UsernameExists := True;
                Break;
              end;
            end;

            if UsernameExists then
            begin
              // Envoyer une erreur à cet utilisateur spécifique
              var ErrorJson := TJSONObject.Create;
              try
                ErrorJson.AddPair('type', 'error');
                ErrorJson.AddPair('content', 'Ce nom d''utilisateur est déjà utilisé');
                Connection.WriteData(ErrorJson.ToString);
              finally
                ErrorJson.Free;
              end;

              // Fermer la connexion
              Connection.Disconnect;
              Exit;
            end;

            // Enregistrer l'utilisateur
            User.Username := Username;
            User.ConnectionGuid := Connection.Guid;
            FUsers.Add(Connection.Guid, User);

            memoLog.Lines.Add('Utilisateur connecté: ' + Username);

            // Informer tout le monde qu'un nouvel utilisateur a rejoint
            BroadcastJson('join', '', Username);

            // Envoyer la liste mise à jour des utilisateurs
            BroadcastUserList;
          end;
        end
        else if MessageType = 'message' then
        begin
          // Message de chat
          if JSON.TryGetValue<string>('content', Content) and
             JSON.TryGetValue<string>('username', Username) then
          begin
            // Vérifier que l'utilisateur est bien celui qui prétend être
            var ActualUsername := GetUsernameByConnection(Connection.Guid);
            if ActualUsername = Username then
            begin
              memoLog.Lines.Add('Message de ' + Username + ': ' + Content);

              // Diffuser à tous les clients
              BroadcastJson('message', Content, Username);
            end
            else
            begin
              // Tentative d'usurpation d'identité
              memoLog.Lines.Add('Tentative d''usurpation d''identité: ' +
                               Connection.Guid + ' prétend être ' + Username);
            end;
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      memoLog.Lines.Add('Erreur de traitement du message: ' + E.Message);
  end;
end;
```

## Communications binaires avec WebSockets

Jusqu'à présent, nous avons utilisé WebSockets pour échanger des messages textuels. Mais WebSockets peut également transmettre des données binaires, ce qui est utile pour des applications comme le partage de fichiers, les jeux en temps réel ou la diffusion de médias.

### Envoi de données binaires

```pascal
procedure TForm1.SendBinaryData(const FileName: string);
var
  Stream: TFileStream;
  FileHeader: TJSONObject;
  HeaderStr: string;
  HeaderBytes, CombinedData: TBytes;
begin
  if not FileExists(FileName) then
    Exit;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    // Créer un en-tête JSON pour décrire le fichier
    FileHeader := TJSONObject.Create;
    try
      FileHeader.AddPair('type', 'file');
      FileHeader.AddPair('filename', ExtractFileName(FileName));
      FileHeader.AddPair('size', TJSONNumber.Create(Stream.Size));
      FileHeader.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

      HeaderStr := FileHeader.ToString;
    finally
      FileHeader.Free;
    end;

    // Convertir l'en-tête en bytes
    HeaderBytes := TEncoding.UTF8.GetBytes(HeaderStr);

    // Préparer un tableau combiné avec la taille de l'en-tête (4 bytes) + en-tête + données
    SetLength(CombinedData, 4 + Length(HeaderBytes) + Stream.Size);

    // Écrire la taille de l'en-tête (pour que le récepteur sache où commence le contenu)
    var HeaderSize := Length(HeaderBytes);
    Move(HeaderSize, CombinedData[0], 4);

    // Écrire l'en-tête
    Move(HeaderBytes[0], CombinedData[4], HeaderSize);

    // Lire le contenu du fichier
    Stream.Position := 0;
    Stream.ReadBuffer(CombinedData[4 + HeaderSize], Stream.Size);

    // Envoyer les données binaires
    sgcWebSocketClient1.WriteData(CombinedData);
  finally
    Stream.Free;
  end;
end;

procedure TForm1.btnSendFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    SendBinaryData(OpenDialog1.FileName);
    Memo1.Lines.Add('Envoi du fichier: ' + ExtractFileName(OpenDialog1.FileName));
  end;
end;
```

### Réception de données binaires

```pascal
procedure TForm1.sgcWebSocketClient1Binary(Connection: TsgcWSConnection; const Binary: TBytes);
var
  HeaderSize: Integer;
  HeaderBytes: TBytes;
  HeaderStr: string;
  Header: TJSONObject;
  FileData: TBytes;
  FileName, FileType: string;
  FileSize: Int64;
  SaveStream: TFileStream;
begin
  // Vérifier que nous avons au moins 4 bytes pour la taille de l'en-tête
  if Length(Binary) < 4 then
    Exit;

  // Lire la taille de l'en-tête
  Move(Binary[0], HeaderSize, 4);

  // Vérifier que nous avons assez de données
  if Length(Binary) < 4 + HeaderSize then
    Exit;

  // Extraire l'en-tête
  SetLength(HeaderBytes, HeaderSize);
  Move(Binary[4], HeaderBytes[0], HeaderSize);

  // Convertir en chaîne et analyser le JSON
  HeaderStr := TEncoding.UTF8.GetString(HeaderBytes);

  try
    Header := TJSONObject.ParseJSONValue(HeaderStr) as TJSONObject;
    try
      // Vérifier le type
      if Header.GetValue('type').Value <> 'file' then
        Exit;

      // Extraire les informations du fichier
      FileName := Header.GetValue('filename').Value;
      FileSize := (Header.GetValue('size') as TJSONNumber).AsInt64;

      // Extraire les données du fichier
      SetLength(FileData, Length(Binary) - (4 + HeaderSize));
      Move(Binary[4 + HeaderSize], FileData[0], Length(FileData));

      // Déterminer où sauvegarder le fichier
      if SaveDialog1.Execute then
      begin
        SaveStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
        try
          // Écrire les données
          SaveStream.WriteBuffer(FileData[0], Length(FileData));
          Memo1.Lines.Add('Fichier reçu et sauvegardé: ' + SaveDialog1.FileName);
        finally
          SaveStream.Free;
        end;
      end;
    finally
      Header.Free;
    end;
  except
    on E: Exception do
      Memo1.Lines.Add('Erreur lors du traitement des données binaires: ' + E.Message);
  end;
end;
```

## Gestion des événements et des erreurs

La gestion appropriée des erreurs et des événements est cruciale pour des applications WebSocket robustes.

### Reconnexion automatique

La connexion WebSocket peut se perdre pour diverses raisons. Voici comment mettre en place une reconnexion automatique :

```pascal
unit WebSocketClient;

interface

uses
  System.SysUtils, System.Classes, sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket,
  System.Generics.Collections;

type
  TWebSocketEvent = procedure(Sender: TObject) of object;
  TWebSocketMessageEvent = procedure(Sender: TObject; const Text: string) of object;
  TWebSocketErrorEvent = procedure(Sender: TObject; const Error: string) of object;

  TWebSocketClient = class
  private
    FClient: TsgcWebSocketClient;
    FURL: string;
    FReconnectAttempts: Integer;
    FMaxReconnectAttempts: Integer;
    FReconnectInterval: Integer;  // en millisecondes
    FReconnectTimer: TTimer;
    FAutoReconnect: Boolean;

    FOnConnect: TWebSocketEvent;
    FOnDisconnect: TWebSocketEvent;
    FOnMessage: TWebSocketMessageEvent;
    FOnError: TWebSocketErrorEvent;

    procedure DoConnect(Connection: TsgcWSConnection);
    procedure DoDisconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure DoMessage(Connection: TsgcWSConnection; const Text: string);
    procedure DoException(Connection: TsgcWSConnection; E: Exception);
    procedure DoReconnectTimer(Sender: TObject);
  public
    constructor Create(const URL: string);
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    procedure Send(const Text: string);

    property URL: string read FURL write FURL;
    property MaxReconnectAttempts: Integer read FMaxReconnectAttempts write FMaxReconnectAttempts;
    property ReconnectInterval: Integer read FReconnectInterval write FReconnectInterval;
    property AutoReconnect: Boolean read FAutoReconnect write FAutoReconnect;

    property OnConnect: TWebSocketEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TWebSocketEvent read FOnDisconnect write FOnDisconnect;
    property OnMessage: TWebSocketMessageEvent read FOnMessage write FOnMessage;
    property OnError: TWebSocketErrorEvent read FOnError write FOnError;
  end;

implementation

constructor TWebSocketClient.Create(const URL: string);
begin
  inherited Create;

  FURL := URL;
  FReconnectAttempts := 0;
  FMaxReconnectAttempts := 5;
  FReconnectInterval := 5000;  // 5 secondes par défaut
  FAutoReconnect := True;

  // Créer le client WebSocket
  FClient := TsgcWebSocketClient.Create(nil);
  FClient.URL := FURL;

  // TLS automatique en fonction de l'URL (ws:// ou wss://)
  FClient.TLS := FURL.StartsWith('wss://');

  // Configurer les événements
  FClient.OnConnect := DoConnect;
  FClient.OnDisconnect := DoDisconnect;
  FClient.OnMessage := DoMessage;
  FClient.OnException := DoException;

  // Créer le timer de reconnexion (désactivé par défaut)
  FReconnectTimer := TTimer.Create(nil);
  FReconnectTimer.Enabled := False;
  FReconnectTimer.OnTimer := DoReconnectTimer;
end;

destructor TWebSocketClient.Destroy;
begin
  Disconnect;

  FReconnectTimer.Free;
  FClient.Free;

  inherited;
end;

procedure TWebSocketClient.Connect;
begin
  if not FClient.Active then
  begin
    // Réinitialiser le compteur de tentatives
    FReconnectAttempts := 0;

    // Se connecter
    FClient.URL := FURL;
    FClient.Active := True;
  end;
end;

procedure TWebSocketClient.Disconnect;
begin
  // Désactiver le timer de reconnexion
  FReconnectTimer.Enabled := False;

  // Déconnecter si actif
  if FClient.Active then
    FClient.Active := False;
end;

procedure TWebSocketClient.Send(const Text: string);
begin
  if FClient.Active then
    FClient.WriteData(Text);
end;

procedure TWebSocketClient.DoConnect(Connection: TsgcWSConnection);
begin
  // Réinitialiser le compteur de tentatives
  FReconnectAttempts := 0;

  // Désactiver le timer de reconnexion
  FReconnectTimer.Enabled := False;

  // Déclencher l'événement
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TWebSocketClient.DoDisconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  // Déclencher l'événement
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);

  // Démarrer la reconnexion automatique si activée
  if FAutoReconnect and (FReconnectAttempts < FMaxReconnectAttempts) then
  begin
    Inc(FReconnectAttempts);

    // Configurer le timer (avec backoff exponentiel)
    FReconnectTimer.Interval := FReconnectInterval * (1 shl (FReconnectAttempts - 1));
    FReconnectTimer.Enabled := True;
  end;
end;

procedure TWebSocketClient.DoReconnectTimer(Sender: TObject);
begin
  // Désactiver le timer
  FReconnectTimer.Enabled := False;

  // Nouvelle tentative de connexion
  if not FClient.Active and (FReconnectAttempts <= FMaxReconnectAttempts) then
  begin
    // Notifier de la tentative de reconnexion
    if Assigned(FOnError) then
      FOnError(Self, 'Tentative de reconnexion ' + IntToStr(FReconnectAttempts) +
                     ' sur ' + IntToStr(FMaxReconnectAttempts));

    // Essayer de se reconnecter
    try
      FClient.Active := True;
    except
      on E: Exception do
      begin
        if Assigned(FOnError) then
          FOnError(Self, 'Erreur de reconnexion: ' + E.Message);

        // Planifier une autre tentative
        if FReconnectAttempts < FMaxReconnectAttempts then
        begin
          FReconnectTimer.Interval := FReconnectInterval * (1 shl FReconnectAttempts);
          FReconnectTimer.Enabled := True;
        end;
      end;
    end;
  end;
end;

procedure TWebSocketClient.DoMessage(Connection: TsgcWSConnection; const Text: string);
begin
  // Déclencher l'événement
  if Assigned(FOnMessage) then
    FOnMessage(Self, Text);
end;

procedure TWebSocketClient.DoException(Connection: TsgcWSConnection; E: Exception);
begin
  // Déclencher l'événement
  if Assigned(FOnError) then
    FOnError(Self, E.Message);
end;

end.
```

### Utilisation de la classe de client WebSocket robuste

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer le client WebSocket
  FWebSocketClient := TWebSocketClient.Create('ws://localhost:8080');

  // Configurer les options
  FWebSocketClient.AutoReconnect := True;
  FWebSocketClient.MaxReconnectAttempts := 5;
  FWebSocketClient.ReconnectInterval := 3000; // 3 secondes

  // Définir les gestionnaires d'événements
  FWebSocketClient.OnConnect := HandleConnect;
  FWebSocketClient.OnDisconnect := HandleDisconnect;
  FWebSocketClient.OnMessage := HandleMessage;
  FWebSocketClient.OnError := HandleError;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FWebSocketClient.Free;
end;

procedure TForm1.HandleConnect(Sender: TObject);
begin
  Memo1.Lines.Add('Connecté au serveur WebSocket');

  // Mettre à jour l'interface utilisateur
  btnConnect.Caption := 'Déconnecter';
  btnSend.Enabled := True;
  edtMessage.Enabled := True;
end;

procedure TForm1.HandleDisconnect(Sender: TObject);
begin
  Memo1.Lines.Add('Déconnecté du serveur WebSocket');

  // Mettre à jour l'interface utilisateur
  btnConnect.Caption := 'Connecter';
  btnSend.Enabled := False;
  edtMessage.Enabled := False;
end;

procedure TForm1.HandleMessage(Sender: TObject; const Text: string);
begin
  Memo1.Lines.Add('Message reçu: ' + Text);
end;

procedure TForm1.HandleError(Sender: TObject; const Error: string);
begin
  Memo1.Lines.Add('Erreur: ' + Error);
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if btnConnect.Caption = 'Connecter' then
    FWebSocketClient.Connect
  else
    FWebSocketClient.Disconnect;
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  if edtMessage.Text.Trim <> '' then
  begin
    FWebSocketClient.Send(edtMessage.Text);
    Memo1.Lines.Add('Message envoyé: ' + edtMessage.Text);
    edtMessage.Text := '';
  end;
end;
```

## Sécurité et meilleures pratiques

### Sécurisation des connexions WebSocket

1. **Utiliser WSS au lieu de WS** : Tout comme HTTPS est plus sûr que HTTP, WSS (WebSocket Secure) chiffre la communication.

```pascal
// Configuration pour une connexion sécurisée
sgcWebSocketClient1.URL := 'wss://exemple.com/websocket';
sgcWebSocketClient1.TLS := True;

// Personnaliser les options TLS
sgcWebSocketClient1.TLSOptions.Version := tls1_2;
sgcWebSocketClient1.TLSOptions.VerifyCertificate := True;
```

2. **Authentification** : Implémentez un système d'authentification robuste.

```pascal
procedure TForm1.ConnecterAvecToken;
var
  Token: string;
begin
  // Obtenir le token JWT ou autre
  Token := ObtenirTokenAuthentification;

  // Configurer les en-têtes pour l'authentification
  sgcWebSocketClient1.CustomHeaders.Clear;
  sgcWebSocketClient1.CustomHeaders.Add('Authorization: Bearer ' + Token);

  // Connecter
  sgcWebSocketClient1.Active := True;
end;
```

3. **Validation des données** : Vérifiez toujours les données entrantes.

```pascal
procedure TForm1.ValidateJson(const Text: string): Boolean;
var
  JSON: TJSONObject;
begin
  Result := False;

  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      // Vérifier les champs requis
      if not (JSON.GetValue('type') is TJSONString) then
        Exit;

      if JSON.GetValue('type').Value = 'message' then
      begin
        if not (JSON.GetValue('content') is TJSONString) then
          Exit;

        if not (JSON.GetValue('username') is TJSONString) then
          Exit;
      end;

      Result := True;
    finally
      JSON.Free;
    end;
  except
    Result := False;
  end;
end;
```

### Meilleures pratiques

1. **Limiter la taille des messages** : Protégez-vous contre les attaques par déni de service.

```pascal
procedure TForm1.sgcWebSocketServer1Message(Connection: TsgcWSConnection; const Text: string);
begin
  // Vérifier la taille du message
  if Length(Text) > 1024 * 10 then // 10 KB
  begin
    // Message trop grand
    Connection.WriteData('{"type":"error","content":"Message trop volumineux"}');
    Exit;
  end;

  // Traiter le message normalement
  // ...
end;
```

2. **Gestion des timeouts** : Configurez des timeouts appropriés.

```pascal
// Configuration côté serveur
sgcWebSocketServer1.HeartBeatEnabled := True;
sgcWebSocketServer1.HeartBeatInterval := 30; // 30 secondes
sgcWebSocketServer1.PingInterval := 120; // 2 minutes

// Configuration côté client
sgcWebSocketClient1.HeartBeatEnabled := True;
sgcWebSocketClient1.HeartBeatInterval := 30; // 30 secondes
sgcWebSocketClient1.PingInterval := 120; // 2 minutes
```

3. **Gérer les déconnexions inattendues** : Mettez en place un système de détection des clients "zombies".

```pascal
procedure TForm1.TimerCheckConnectionsTimer(Sender: TObject);
var
  Connection: TsgcWSConnection;
  CurrentTime: TDateTime;
begin
  CurrentTime := Now;

  // Parcourir toutes les connexions
  for Connection in sgcWebSocketServer1.Connections do
  begin
    // Vérifier si la connexion est inactive depuis trop longtemps
    if SecondsBetween(CurrentTime, Connection.LastActivity) > 300 then // 5 minutes
    begin
      // Déconnecter le client inactif
      memoLog.Lines.Add('Déconnexion de client inactif: ' + Connection.Guid);
      Connection.Disconnect;
    end;
  end;
end;
```

## Autres technologies de communication en temps réel

### SignalR

SignalR est une bibliothèque Microsoft qui simplifie l'ajout de fonctionnalités en temps réel aux applications web. Bien que principalement conçue pour .NET, vous pouvez l'utiliser avec Delphi via des clients REST.

```pascal
procedure TForm1.ConnecterASignalR;
var
  NegotiateResponse: TJSONObject;
  ConnectionId, URL: string;
begin
  // Étape 1: Négociation pour obtenir un connectionId
  NegotiateResponse := ObtenirNegotiationSignalR;
  try
    ConnectionId := NegotiateResponse.GetValue('connectionId').Value;

    // Étape 2: Se connecter au hub
    URL := 'https://exemple.com/signalr/connect?transport=webSockets&connectionId=' + ConnectionId;

    sgcWebSocketClient1.URL := URL;
    sgcWebSocketClient1.Active := True;
  finally
    NegotiateResponse.Free;
  end;
end;

function TForm1.ObtenirNegotiationSignalR: TJSONObject;
var
  HTTP: TIdHTTP;
  Response: string;
begin
  HTTP := TIdHTTP.Create(nil);
  try
    Response := HTTP.Get('https://exemple.com/signalr/negotiate');
    Result := TJSONObject.ParseJSONValue(Response) as TJSONObject;
  finally
    HTTP.Free;
  end;
end;
```

### MQTT (Message Queuing Telemetry Transport)

MQTT est un protocole de messagerie léger idéal pour l'IoT et les applications mobiles. Des bibliothèques comme TMQTTClient peuvent être utilisées avec Delphi.

```pascal
procedure TForm1.ConnecterAMQTT;
begin
  // Créer un client MQTT
  FMQTTClient := TMQTTClient.Create('mqtt.exemple.com', 1883);

  // Définir les gestionnaires d'événements
  FMQTTClient.OnConnected := HandleMQTTConnected;
  FMQTTClient.OnDisconnected := HandleMQTTDisconnected;
  FMQTTClient.OnMessage := HandleMQTTMessage;

  // Se connecter
  FMQTTClient.Connect('ClientID', 'username', 'password');
end;

procedure TForm1.HandleMQTTConnected(Sender: TObject);
begin
  Memo1.Lines.Add('Connecté au broker MQTT');

  // S'abonner à des topics
  FMQTTClient.Subscribe('maison/salon/temperature');
  FMQTTClient.Subscribe('maison/salon/humidite');
end;

procedure TForm1.HandleMQTTMessage(Sender: TObject; Topic, Payload: string);
begin
  Memo1.Lines.Add('Message MQTT - Topic: ' + Topic + ', Contenu: ' + Payload);

  // Traiter les différents topics
  if Topic = 'maison/salon/temperature' then
    lblTemperature.Caption := 'Température: ' + Payload + '°C'
  else if Topic = 'maison/salon/humidite' then
    lblHumidite.Caption := 'Humidité: ' + Payload + '%';
end;

procedure TForm1.btnPublishClick(Sender: TObject);
begin
  // Publier un message
  FMQTTClient.Publish('maison/cuisine/lumiere', IfThen(Switch1.IsOn, 'ON', 'OFF'));
end;
```

### Server-Sent Events (SSE)

SSE est une technologie qui permet à un serveur de pousser des mises à jour vers un client via HTTP. C'est unidirectionnel (serveur vers client uniquement).

```pascal
procedure TForm1.ConnecterASSE;
var
  HTTP: TIdHTTP;
  SSEStream: TMemoryStream;
  Buffer: TBytes;
  ReadCount: Integer;
  ResponseContent: string;
begin
  HTTP := TIdHTTP.Create(nil);
  SSEStream := TMemoryStream.Create;

  try
    HTTP.Request.Accept := 'text/event-stream';
    HTTP.HandleRedirects := True;
    HTTP.ConnectTimeout := 30000; // 30 secondes
    HTTP.ReadTimeout := 0; // Pas de timeout

    // Commencer la requête SSE
    HTTP.Get('https://exemple.com/events', SSEStream);

    // Traiter les données SSE (dans un thread séparé dans une application réelle)
    SetLength(Buffer, 1024);

    while not Application.Terminated do
    begin
      SSEStream.Position := 0;
      ReadCount := SSEStream.Read(Buffer[0], Length(Buffer));

      if ReadCount > 0 then
      begin
        ResponseContent := TEncoding.UTF8.GetString(Buffer, 0, ReadCount);

        // Traiter chaque ligne SSE
        var Lines := ResponseContent.Split([#10, #13]);
        for var Line in Lines do
        begin
          if Line.StartsWith('data:') then
          begin
            var Data := Line.Substring(5).Trim;
            Memo1.Lines.Add('Événement SSE reçu: ' + Data);
          end;
        end;
      end;

      Sleep(100); // Pause pour éviter de surcharger le CPU
    end;
  finally
    HTTP.Free;
    SSEStream.Free;
  end;
end;
```

## Comparaison des technologies en temps réel

| Technologie   | Forces                                           | Faiblesses                                 | Cas d'utilisation                                  |
|---------------|--------------------------------------------------|--------------------------------------------|----------------------------------------------------|
| WebSockets    | Bidirectionnel, faible latence                   | Peut être bloqué par certains pare-feux    | Chat, jeux en ligne, applications collaboratives   |
| SignalR       | Abstraction simplifiée, fallback automatique     | Principalement pour .NET                   | Applications d'entreprise avec écosystème Microsoft|
| MQTT          | Ultra léger, efficace pour les ressources limitées| Moins adapté pour le web                  | IoT, capteurs, systèmes embarqués                  |
| SSE           | Simple, fonctionne sur HTTP standard             | Unidirectionnel (serveur vers client)      | Flux de notifications, mises à jour en direct      |

## Conclusion

Les communications en temps réel sont un élément essentiel des applications modernes. Avec Delphi, vous disposez de plusieurs options pour implémenter ces fonctionnalités, WebSocket étant la plus polyvalente et la plus largement supportée.

Dans ce chapitre, nous avons appris à :

1. Créer des clients et des serveurs WebSocket
2. Échanger des données textuelles (JSON) et binaires
3. Mettre en place une gestion robuste des erreurs et des reconnexions
4. Sécuriser les communications WebSocket
5. Explorer d'autres technologies de communication en temps réel

Ces connaissances vous permettront de créer des applications interactives et réactives qui répondent aux attentes des utilisateurs modernes.

## Ressources supplémentaires

- Documentation de sgcWebSockets : [https://www.esegece.com/websockets/docs](https://www.esegece.com/websockets/docs)
- Spécification WebSocket : [https://tools.ietf.org/html/rfc6455](https://tools.ietf.org/html/rfc6455)
- Bibliothèques MQTT pour Delphi : [https://github.com/emqx/mqtt-client-delphi](https://github.com/emqx/mqtt-client-delphi)
- SignalR : [https://docs.microsoft.com/en-us/aspnet/signalr/](https://docs.microsoft.com/en-us/aspnet/signalr/)

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria. Les fonctionnalités avancées de WebSockets peuvent nécessiter l'utilisation de bibliothèques tierces comme sgcWebSockets.*

## Exercices pratiques

Pour consolider vos connaissances sur les WebSockets et les communications en temps réel, voici quelques exercices pratiques à essayer par vous-même :

### Exercice 1 : Moniteur système en temps réel

Créez une application qui surveille et affiche en temps réel les ressources système (utilisation CPU, mémoire, disque) d'un ordinateur distant :

1. Créez un serveur WebSocket qui collecte les données système et les envoie aux clients connectés
2. Créez un client qui affiche ces données dans des graphiques en temps réel
3. Implémentez une fonctionnalité d'alerte lorsque l'utilisation dépasse un certain seuil

### Exercice 2 : Tableau blanc collaboratif

Créez une application de tableau blanc partagé où plusieurs utilisateurs peuvent dessiner ensemble en temps réel :

1. Créez un serveur WebSocket qui relaie les actions de dessin entre les clients
2. Créez un client avec une interface de dessin (lignes, cercles, texte)
3. Synchronisez les actions de dessin entre tous les clients connectés
4. Bonus : Ajoutez la possibilité de sauvegarder et charger des tableaux précédents

### Exercice 3 : Notification push pour base de données

Créez un système qui envoie des notifications en temps réel lorsque des changements sont apportés à une base de données :

1. Configurez une base de données avec des déclencheurs (triggers) qui détectent les modifications
2. Créez un serveur WebSocket qui reçoit ces notifications et les transmet aux clients intéressés
3. Créez un client qui s'abonne à certaines tables ou types de modifications
4. Affichez les notifications de manière non intrusive (comme des "toasts")

## Cas d'utilisation avancés

### Connexion à des services WebSocket tiers

De nombreux services fournissent des API WebSocket. Voici comment vous pourriez vous connecter à une API de cryptomonnaie pour obtenir les prix en temps réel :

```pascal
procedure TForm1.ConnecterABinance;
begin
  // Configurer le client
  sgcWebSocketClient1.URL := 'wss://stream.binance.com:9443/ws/btcusdt@trade';
  sgcWebSocketClient1.TLS := True;

  // Se connecter
  sgcWebSocketClient1.Active := True;
end;

procedure TForm1.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
var
  JSON: TJSONObject;
  Price: Double;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      if JSON.TryGetValue<Double>('p', Price) then
      begin
        // Mettre à jour l'interface utilisateur avec le nouveau prix
        lblPrice.Caption := 'BTC/USDT: ' + FormatFloat('#,##0.00', Price) + ' $';

        // Ajouter à l'historique des prix pour le graphique
        AddPriceToChart(Price);
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      Memo1.Lines.Add('Erreur: ' + E.Message);
  end;
end;
```

### WebSockets avec le framework FireMonkey (FMX)

FireMonkey permet de créer des applications multi-plateformes. Voici comment adapter votre client WebSocket pour fonctionner avec FireMonkey :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Edit,
  sgcWebSocket_Classes, sgcWebSocket_Client, sgcWebSocket;

type
  TForm1 = class(TForm)
    sgcWebSocketClient1: TsgcWebSocketClient;
    memMessages: TMemo;
    edtMessage: TEdit;
    btnSend: TButton;
    btnConnect: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
    procedure sgcWebSocketClient1Connect(Connection: TsgcWSConnection);
    procedure sgcWebSocketClient1Disconnect(Connection: TsgcWSConnection; Code: Integer);
    procedure sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
    procedure sgcWebSocketClient1Exception(Connection: TsgcWSConnection; E: Exception);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du client WebSocket
  sgcWebSocketClient1.URL := 'ws://echo.websocket.org';
  sgcWebSocketClient1.TLS := False;
end;

procedure TForm1.btnConnectClick(Sender: TObject);
begin
  if sgcWebSocketClient1.Active then
  begin
    sgcWebSocketClient1.Active := False;
  end
  else
  begin
    sgcWebSocketClient1.Active := True;
  end;
end;

procedure TForm1.sgcWebSocketClient1Connect(Connection: TsgcWSConnection);
begin
  lblStatus.Text := 'Connecté';
  btnConnect.Text := 'Déconnecter';
  memMessages.Lines.Add('Connecté à ' + sgcWebSocketClient1.URL);

  // Activer les contrôles
  edtMessage.Enabled := True;
  btnSend.Enabled := True;
end;

procedure TForm1.sgcWebSocketClient1Disconnect(Connection: TsgcWSConnection; Code: Integer);
begin
  lblStatus.Text := 'Déconnecté';
  btnConnect.Text := 'Connecter';
  memMessages.Lines.Add('Déconnecté. Code: ' + IntToStr(Code));

  // Désactiver les contrôles
  edtMessage.Enabled := False;
  btnSend.Enabled := False;
end;

procedure TForm1.sgcWebSocketClient1Exception(Connection: TsgcWSConnection; E: Exception);
begin
  memMessages.Lines.Add('Erreur: ' + E.Message);
end;

procedure TForm1.sgcWebSocketClient1Message(Connection: TsgcWSConnection; const Text: string);
begin
  memMessages.Lines.Add('Reçu: ' + Text);
end;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  if (Trim(edtMessage.Text) <> '') and sgcWebSocketClient1.Active then
  begin
    sgcWebSocketClient1.WriteData(edtMessage.Text);
    memMessages.Lines.Add('Envoyé: ' + edtMessage.Text);
    edtMessage.Text := '';
  end;
end;

end.
```

### WebSockets sur des appareils mobiles

Lorsque vous utilisez des WebSockets sur des appareils mobiles, vous devez prendre en compte des défis supplémentaires comme les connexions intermittentes et la gestion de l'énergie.

```pascal
procedure TForm1.InitMobileWebSocket;
begin
  // Configuration supplémentaire pour les appareils mobiles

  // 1. Gestion des transitions réseau
  FConnectivityManager := TMobileConnectivityManager.Create;
  FConnectivityManager.OnNetworkChanged := HandleNetworkChanged;

  // 2. Optimisation de la batterie
  sgcWebSocketClient1.HeartBeatInterval := 60; // 1 minute

  // 3. Réduction de la consommation de données
  sgcWebSocketClient1.Compression := True;

  // 4. Gestion de l'application qui passe en arrière-plan
  Application.OnEnteredBackground := HandleAppBackground;
  Application.OnEnteredForeground := HandleAppForeground;
end;

procedure TForm1.HandleNetworkChanged(Sender: TObject; NetworkType: TNetworkType; Connected: Boolean);
begin
  if Connected then
  begin
    if not sgcWebSocketClient1.Active then
    begin
      // Reconnexion au WebSocket si le réseau est disponible
      sgcWebSocketClient1.Active := True;
    end;
  end
  else
  begin
    // Mettre à jour l'interface utilisateur pour indiquer l'absence de connexion
    lblStatus.Text := 'Pas de connexion réseau';
  end;
end;

procedure TForm1.HandleAppBackground(Sender: TObject);
begin
  // Réduire l'activité du WebSocket lorsque l'application est en arrière-plan
  sgcWebSocketClient1.HeartBeatInterval := 300; // 5 minutes

  // Éventuellement se déconnecter pour économiser la batterie
  if FDisconnectInBackground then
  begin
    FWasConnected := sgcWebSocketClient1.Active;
    sgcWebSocketClient1.Active := False;
  end;
end;

procedure TForm1.HandleAppForeground(Sender: TObject);
begin
  // Restaurer l'activité normale du WebSocket
  sgcWebSocketClient1.HeartBeatInterval := 60;

  // Reconnecter si nécessaire
  if FDisconnectInBackground and FWasConnected then
  begin
    sgcWebSocketClient1.Active := True;
  end;
end;
```

## Optimisation des performances

### Compression des données

Pour réduire la quantité de données transmises, activez la compression :

```pascal
// Côté client
sgcWebSocketClient1.Extensions.PerMessage_Deflate := True;

// Côté serveur
sgcWebSocketServer1.Extensions.PerMessage_Deflate := True;
```

### Mise en tampon des messages

Regroupez les petits messages pour réduire la surcharge réseau :

```pascal
type
  TMessageBuffer = class
  private
    FMessages: TList<string>;
    FTimer: TTimer;
    FOnFlush: TProc<TArray<string>>;
    procedure OnTimerTick(Sender: TObject);
  public
    constructor Create(FlushInterval: Integer = 200); // en millisecondes
    destructor Destroy; override;

    procedure AddMessage(const Message: string);
    procedure Flush;

    property OnFlush: TProc<TArray<string>> read FOnFlush write FOnFlush;
  end;

constructor TMessageBuffer.Create(FlushInterval: Integer);
begin
  inherited Create;

  FMessages := TList<string>.Create;

  FTimer := TTimer.Create(nil);
  FTimer.Interval := FlushInterval;
  FTimer.OnTimer := OnTimerTick;
  FTimer.Enabled := True;
end;

destructor TMessageBuffer.Destroy;
begin
  FTimer.Free;
  FMessages.Free;

  inherited;
end;

procedure TMessageBuffer.AddMessage(const Message: string);
begin
  FMessages.Add(Message);

  // Si nous avons beaucoup de messages, vidons le tampon immédiatement
  if FMessages.Count >= 10 then
    Flush;
end;

procedure TMessageBuffer.Flush;
begin
  if FMessages.Count > 0 then
  begin
    if Assigned(FOnFlush) then
      FOnFlush(FMessages.ToArray);

    FMessages.Clear;
  end;
end;

procedure TMessageBuffer.OnTimerTick(Sender: TObject);
begin
  Flush;
end;

// Utilisation dans votre application
procedure TForm1.FormCreate(Sender: TObject);
begin
  FMessageBuffer := TMessageBuffer.Create(100); // Vider toutes les 100ms
  FMessageBuffer.OnFlush := HandleMessageBatch;
end;

procedure TForm1.HandleMessageBatch(Messages: TArray<string>);
var
  BatchJSON: TJSONObject;
  MessageArray: TJSONArray;
  I: Integer;
begin
  if Length(Messages) = 0 then
    Exit;

  // Si un seul message, pas besoin de regrouper
  if Length(Messages) = 1 then
  begin
    sgcWebSocketClient1.WriteData(Messages[0]);
    Exit;
  end;

  // Créer un lot de messages
  BatchJSON := TJSONObject.Create;
  MessageArray := TJSONArray.Create;

  try
    BatchJSON.AddPair('type', 'batch');

    // Ajouter chaque message au tableau
    for I := 0 to High(Messages) do
      MessageArray.Add(Messages[I]);

    BatchJSON.AddPair('messages', MessageArray);

    // Envoyer le lot
    sgcWebSocketClient1.WriteData(BatchJSON.ToString);
  finally
    BatchJSON.Free; // Libère aussi MessageArray
  end;
end;
```

### Multi-threading pour les serveurs WebSocket

Pour gérer de nombreuses connexions simultanées, utilisez le multi-threading :

```pascal
procedure TForm1.ConfigureServerForHighLoad;
begin
  // Augmenter le nombre de threads de travail
  sgcWebSocketServer1.ThreadPool.MaxThreads := 32;

  // Utiliser le mode asynchrone pour les événements
  sgcWebSocketServer1.NotifyEvents := neAsynchronous;

  // Activer le traitement des messages par lots
  sgcWebSocketServer1.Options.BatchMode := bmBatchAll;
  sgcWebSocketServer1.Options.BatchWaitTime := 50; // 50ms

  // Activer le délai de garde pour éviter les surcharges
  sgcWebSocketServer1.WatchDog.Enabled := True;
  sgcWebSocketServer1.WatchDog.Interval := 1000; // 1 seconde

  // Limiter le nombre de connexions par IP
  sgcWebSocketServer1.Authentication.Enabled := True;
  sgcWebSocketServer1.Authentication.MaxConnectionsPerIP := 5;

  // Configurer une file d'attente pour les messages à envoyer
  sgcWebSocketServer1.QueueOptions.Enabled := True;
  sgcWebSocketServer1.QueueOptions.MaxSize := 1000; // messages
end;
```

## Diagnostics et débogage

### Journalisation des messages WebSocket

Pour déboguer les communications WebSocket, mettez en place une journalisation détaillée :

```pascal
unit WebSocketLogger;

interface

uses
  System.SysUtils, System.Classes, System.SyncObjs;

type
  TLogLevel = (llError, llWarning, llInfo, llDebug);

  TWebSocketLogger = class
  private
    FLogFile: TStreamWriter;
    FLock: TCriticalSection;
    FMinLogLevel: TLogLevel;

    class var FInstance: TWebSocketLogger;

    constructor Create(const LogFilePath: string);
  public
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Message: string); overload;
    procedure Log(Level: TLogLevel; const Fmt: string; const Args: array of const); overload;

    procedure Error(const Message: string);
    procedure Warning(const Message: string);
    procedure Info(const Message: string);
    procedure Debug(const Message: string);

    property MinLogLevel: TLogLevel read FMinLogLevel write FMinLogLevel;

    class function GetInstance: TWebSocketLogger;
    class procedure FreeInstance;
  end;

implementation

{ TWebSocketLogger }

constructor TWebSocketLogger.Create(const LogFilePath: string);
begin
  inherited Create;

  FLock := TCriticalSection.Create;
  FMinLogLevel := llInfo; // Par défaut, journaliser les infos et plus critiques

  // Créer ou ouvrir le fichier de log
  FLogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);

  // Écrire l'en-tête
  FLogFile.WriteLine('');
  FLogFile.WriteLine('===== Démarrage de la journalisation WebSocket =====');
  FLogFile.WriteLine('Date: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FLogFile.WriteLine('');
  FLogFile.Flush;
end;

destructor TWebSocketLogger.Destroy;
begin
  FLogFile.WriteLine('');
  FLogFile.WriteLine('===== Fin de la journalisation WebSocket =====');
  FLogFile.WriteLine('Date: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));

  FLogFile.Free;
  FLock.Free;

  inherited;
end;

procedure TWebSocketLogger.Log(Level: TLogLevel; const Message: string);
const
  LogLevelNames: array[TLogLevel] of string = ('ERROR', 'WARNING', 'INFO', 'DEBUG');
begin
  if Level > FMinLogLevel then
    Exit;

  FLock.Enter;
  try
    FLogFile.WriteLine(
      Format('[%s] [%s] %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
         LogLevelNames[Level],
         Message])
    );
    FLogFile.Flush;
  finally
    FLock.Leave;
  end;
end;

procedure TWebSocketLogger.Log(Level: TLogLevel; const Fmt: string; const Args: array of const);
begin
  Log(Level, Format(Fmt, Args));
end;

procedure TWebSocketLogger.Error(const Message: string);
begin
  Log(llError, Message);
end;

procedure TWebSocketLogger.Warning(const Message: string);
begin
  Log(llWarning, Message);
end;

procedure TWebSocketLogger.Info(const Message: string);
begin
  Log(llInfo, Message);
end;

procedure TWebSocketLogger.Debug(const Message: string);
begin
  Log(llDebug, Message);
end;

class function TWebSocketLogger.GetInstance: TWebSocketLogger;
var
  LogPath: string;
begin
  if FInstance = nil then
  begin
    LogPath := ExtractFilePath(ParamStr(0)) + 'websocket_' +
               FormatDateTime('yyyymmdd_hhnnss', Now) + '.log';
    FInstance := TWebSocketLogger.Create(LogPath);
  end;

  Result := FInstance;
end;

class procedure TWebSocketLogger.FreeInstance;
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

end.
```

### Application de surveillance WebSocket

Créez une application dédiée pour surveiller et déboguer vos connexions WebSocket en temps réel :

```pascal
procedure TForm1.ConfigureWebSocketMonitoring;
begin
  // Configurer le serveur pour journaliser tous les messages
  sgcWebSocketServer1.LogFile.Enabled := True;
  sgcWebSocketServer1.LogFile.FileName := 'server_log.txt';

  // Activer la surveillance des connexions
  sgcWebSocketServer1.MonitorConnection := True;

  // Configurer le niveau de détail de surveillance
  if FVerboseLogging then
    sgcWebSocketServer1.LogFile.Level := TEcLevel.eclDebug
  else
    sgcWebSocketServer1.LogFile.Level := TEcLevel.eclError;

  // S'abonner aux événements de surveillance
  sgcWebSocketServer1.OnMonitor := HandleMonitorEvent;
end;

procedure TForm1.HandleMonitorEvent(Connection: TsgcWSConnection; const Action, Value: string);
begin
  memoMonitor.Lines.Add(
    Format('[%s] [%s] %s - %s',
      [FormatDateTime('hh:nn:ss.zzz', Now),
       IfThen(Connection = nil, 'Server', Connection.Guid),
       Action,
       Value])
  );
end;
```

## Au-delà des WebSockets

### WebSocket avec Redis Pub/Sub

Pour créer un système hautement évolutif, vous pouvez combiner WebSockets avec Redis Pub/Sub pour la communication entre plusieurs instances de serveurs :

```pascal
unit RedisWebSocketBridge;

interface

uses
  System.SysUtils, System.Classes, sgcWebSocket_Classes, sgcWebSocket_Server,
  sgcRedis_Client, sgcRedis_Commands;

type
  TRedisWebSocketBridge = class
  private
    FWebSocketServer: TsgcWebSocketServer;
    FRedisClient: TsgcRedisClient;
    FRedisCommands: TsgcRedisCommands;
    FChannels: TStringList;

    procedure HandleRedisMessage(const Channel, Message: string);
    procedure HandleWebSocketMessage(Connection: TsgcWSConnection; const Text: string);
  public
    constructor Create(WebSocketServer: TsgcWebSocketServer);
    destructor Destroy; override;

    procedure Connect(const RedisHost: string; RedisPort: Integer);
    procedure Disconnect;
    procedure SubscribeToChannel(const Channel: string);
    procedure PublishToChannel(const Channel, Message: string);
  end;

implementation

{ TRedisWebSocketBridge }

constructor TRedisWebSocketBridge.Create(WebSocketServer: TsgcWebSocketServer);
begin
  inherited Create;

  FWebSocketServer := WebSocketServer;
  FWebSocketServer.OnMessage := HandleWebSocketMessage;

  FRedisClient := TsgcRedisClient.Create(nil);
  FRedisCommands := TsgcRedisCommands.Create(nil);
  FRedisCommands.Redis := FRedisClient;

  FChannels := TStringList.Create;
end;

destructor TRedisWebSocketBridge.Destroy;
begin
  Disconnect;

  FChannels.Free;
  FRedisCommands.Free;
  FRedisClient.Free;

  inherited;
end;

procedure TRedisWebSocketBridge.Connect(const RedisHost: string; RedisPort: Integer);
begin
  FRedisClient.Host := RedisHost;
  FRedisClient.Port := RedisPort;

  FRedisClient.Connect;

  // S'abonner aux canaux déjà configurés
  for var I := 0 to FChannels.Count - 1 do
    FRedisCommands.SUBSCRIBE(FChannels[I]);

  // Configurer le gestionnaire de messages
  FRedisClient.OnMessage := HandleRedisMessage;
end;

procedure TRedisWebSocketBridge.Disconnect;
begin
  if FRedisClient.Connected then
  begin
    // Se désabonner de tous les canaux
    for var I := 0 to FChannels.Count - 1 do
      FRedisCommands.UNSUBSCRIBE(FChannels[I]);

    FRedisClient.Disconnect;
  end;
end;

procedure TRedisWebSocketBridge.SubscribeToChannel(const Channel: string);
begin
  if FChannels.IndexOf(Channel) < 0 then
  begin
    FChannels.Add(Channel);

    if FRedisClient.Connected then
      FRedisCommands.SUBSCRIBE(Channel);
  end;
end;

procedure TRedisWebSocketBridge.PublishToChannel(const Channel, Message: string);
begin
  if FRedisClient.Connected then
    FRedisCommands.PUBLISH(Channel, Message);
end;

procedure TRedisWebSocketBridge.HandleRedisMessage(const Channel, Message: string);
begin
  // Diffuser le message Redis à tous les clients WebSocket
  FWebSocketServer.Broadcast(Message);
end;

procedure TRedisWebSocketBridge.HandleWebSocketMessage(Connection: TsgcWSConnection; const Text: string);
begin
  // Publier le message WebSocket sur le canal Redis
  // Ici, vous pourriez analyser le message pour déterminer le canal

  // Exemple simple : tous les messages vont sur le canal "websocket"
  PublishToChannel('websocket', Text);
end;

end.
```

### Utilisation de WebSockets dans une architecture microservices

Dans une architecture de microservices, vous pouvez utiliser les WebSockets pour la communication entre services :

```pascal
procedure TServiceGateway.InitializeWebSocketGateway;
var
  ServiceConfig: TServiceConfiguration;
begin
  // Charger la configuration des services
  ServiceConfig := LoadServiceConfiguration;

  // Configurer les connexions à chaque service
  for var Service in ServiceConfig.Services do
  begin
    var Client := TServiceWebSocketClient.Create(Self);
    Client.ServiceName := Service.Name;
    Client.URL := Service.WebSocketURL;
    Client.OnMessage := HandleServiceMessage;
    Client.OnServiceStatus := HandleServiceStatusChange;

    // Ajouter à notre liste de services
    FServiceClients.Add(Service.Name, Client);

    // Se connecter
    Client.Connect;
  end;

  // Configurer le serveur pour les clients frontend
  FClientServer := TsgcWebSocketServer.Create(nil);
  FClientServer.Port := ServiceConfig.GatewayPort;
  FClientServer.OnMessage := HandleClientMessage;
  FClientServer.Active := True;
end;

procedure TServiceGateway.HandleClientMessage(Connection: TsgcWSConnection; const Text: string);
var
  JSON: TJSONObject;
  ServiceName, Action, RequestID: string;
  ServiceClient: TServiceWebSocketClient;
  RequestJSON: TJSONObject;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      // Extraire les informations de routage
      if JSON.TryGetValue<string>('service', ServiceName) and
         JSON.TryGetValue<string>('action', Action) then
      begin
        // Générer un ID de requête si non fourni
        if not JSON.TryGetValue<string>('requestId', RequestID) then
        begin
          RequestID := TGuid.NewGuid.ToString;
          JSON.AddPair('requestId', RequestID);
        end;

        // Stocker l'ID de connexion pour pouvoir répondre plus tard
        FRequests.Add(RequestID, Connection.Guid);

        // Transférer au service approprié
        if FServiceClients.TryGetValue(ServiceName, ServiceClient) then
        begin
          if ServiceClient.IsConnected then
          begin
            // Ajouter l'ID client à la requête
            JSON.AddPair('clientId', Connection.Guid);

            // Transmettre la requête
            ServiceClient.Send(JSON.ToString);
          end
          else
          begin
            // Service non disponible
            SendErrorToClient(Connection, 'Service non disponible', RequestID);
          end;
        end
        else
        begin
          // Service inconnu
          SendErrorToClient(Connection, 'Service inconnu: ' + ServiceName, RequestID);
        end;
      end
      else
      begin
        // Format de requête invalide
        SendErrorToClient(Connection, 'Format de requête invalide', '');
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      SendErrorToClient(Connection, 'Erreur de traitement: ' + E.Message, '');
  end;
end;

procedure TServiceGateway.HandleServiceMessage(Sender: TObject; const Text: string);
var
  JSON: TJSONObject;
  RequestID, ClientID: string;
  ClientConnection: TsgcWSConnection;
begin
  try
    JSON := TJSONObject.ParseJSONValue(Text) as TJSONObject;
    try
      // Extraire les informations de routage
      if JSON.TryGetValue<string>('requestId', RequestID) then
      begin
        // Trouver le client destination
        if FRequests.TryGetValue(RequestID, ClientID) then
        begin
          // Transmettre la réponse au client
          FClientServer.WriteData(Text, ClientID);

          // Nettoyer la requête si c'est une réponse finale
          if not JSON.GetValue<Boolean>('partial', False) then
            FRequests.Remove(RequestID);
        end;
      end
      else if JSON.TryGetValue<string>('broadcast', ClientID) then
      begin
        // Message de diffusion pour tous les clients
        FClientServer.Broadcast(Text);
      end;
    finally
      JSON.Free;
    end;
  except
    on E: Exception do
      // Journaliser l'erreur
      Logger.Error('Erreur de traitement de message service: ' + E.Message);
  end;
end;
```

## Tendances futures

Les WebSockets continuent d'évoluer avec de nouvelles fonctionnalités et cas d'utilisation. Voici quelques tendances à surveiller :

### 1. WebSockets sur HTTP/3 (QUIC)

HTTP/3 utilise le protocole QUIC basé sur UDP pour améliorer les performances. Les WebSockets sur HTTP/3 offriront une meilleure résilience aux changements de réseau et aux problèmes de latence.

### 2. WebTransport

WebTransport est une nouvelle API qui combine les avantages des WebSockets et de WebRTC, offrant des communications bidirectionnelles fiables et non fiables sur QUIC.

### 3. Integration avec WebAssembly

La combinaison de WebSockets et WebAssembly permet des applications très performantes qui peuvent traiter les données en temps réel avec une performance proche du natif.

### 4. WebSockets sur les appareils IoT

Avec la croissance de l'IoT, les WebSockets sont de plus en plus utilisés pour les communications entre appareils à ressources limitées, grâce à leur légèreté et leur efficacité.

## Problèmes courants et solutions

Voici quelques problèmes fréquemment rencontrés lors de l'utilisation des WebSockets et leurs solutions :

### Problème 1 : Connexion perdue sans notification

**Symptôme** : La connexion WebSocket se ferme inopinément sans déclencher d'événement de déconnexion.

**Solution** : Mettre en place un mécanisme de ping/pong pour vérifier régulièrement l'état de la connexion.

```pascal
procedure TForm1.ImplementHeartbeat;
begin
  // Configurer un timer pour envoyer des pings réguliers
  FHeartbeatTimer := TTimer.Create(nil);
  FHeartbeatTimer.Interval := 30000; // 30 secondes
  FHeartbeatTimer.OnTimer := HeartbeatTimerTick;
  FHeartbeatTimer.Enabled := True;

  // Timer de surveillance des pongs
  FPongWatchdog := TTimer.Create(nil);
  FPongWatchdog.Interval := 10000; // 10 secondes
  FPongWatchdog.OnTimer := PongWatchdogTick;
  FPongWatchdog.Enabled := False;

  // Activer les heartbeats dans le client WebSocket
  sgcWebSocketClient1.HeartBeatEnabled := True;
end;

procedure TForm1.HeartbeatTimerTick(Sender: TObject);
begin
  if sgcWebSocketClient1.Active then
  begin
    // Envoyer un ping et démarrer le watchdog
    FLastPingTime := Now;
    sgcWebSocketClient1.Ping;
    FPongWatchdog.Enabled := True;
  end;
end;

procedure TForm1.PongWatchdogTick(Sender: TObject);
begin
  // Si on arrive ici, on n'a pas reçu de pong dans le délai imparti
  FPongWatchdog.Enabled := False;

  if sgcWebSocketClient1.Active then
  begin
    // Considérer la connexion comme perdue
    Memo1.Lines.Add('Connexion perdue (pas de réponse ping/pong)');

    // Forcer une déconnexion/reconnexion
    sgcWebSocketClient1.Active := False;
    sgcWebSocketClient1.Active := True;
  end;
end;

procedure TForm1.sgcWebSocketClient1Pong(Connection: TsgcWSConnection);
begin
  // Désactiver le watchdog car on a reçu un pong
  FPongWatchdog.Enabled := False;

  // Calculer la latence
  var LatencyMs := Round((Now - FLastPingTime) * 24 * 60 * 60 * 1000);
  lblLatency.Caption := 'Latence: ' + IntToStr(LatencyMs) + ' ms';
end;
```

### Problème 2 : Blocage par les pare-feux ou proxys

**Symptôme** : Impossibilité d'établir une connexion WebSocket dans certains environnements réseau.

**Solution** : Utiliser une stratégie de repli (fallback) vers des technologies alternatives comme le long polling.

```pascal
type
  TCommunicationMode = (cmWebSocket, cmLongPolling);

procedure TForm1.ConnectWithFallback;
begin
  // Essayer d'abord les WebSockets
  FCommunicationMode := cmWebSocket;

  // Configurer un timeout pour l'échec de connexion WebSocket
  FWebSocketFallbackTimer := TTimer.Create(nil);
  FWebSocketFallbackTimer.Interval := 5000; // 5 secondes
  FWebSocketFallbackTimer.OnTimer := WebSocketFallbackTimerTick;
  FWebSocketFallbackTimer.Enabled := True;

  // Tenter la connexion WebSocket
  sgcWebSocketClient1.Active := True;
end;

procedure TForm1.WebSocketFallbackTimerTick(Sender: TObject);
begin
  FWebSocketFallbackTimer.Enabled := False;

  // Si toujours pas connecté via WebSocket, passer au long polling
  if (FCommunicationMode = cmWebSocket) and (not sgcWebSocketClient1.Active) then
  begin
    Memo1.Lines.Add('WebSocket bloqué, passage en mode long polling');
    sgcWebSocketClient1.Active := False;

    // Passer en mode long polling
    FCommunicationMode := cmLongPolling;
    StartLongPolling;
  end;
end;

procedure TForm1.StartLongPolling;
begin
  // Initialiser le client HTTP pour le long polling
  FHTTPClient := TIdHTTP.Create(nil);
  FHTTPClient.ConnectTimeout := 30000;
  FHTTPClient.ReadTimeout := 90000; // 90 secondes pour le long poll

  // Démarrer la première requête de polling
  PerformLongPoll;
end;

procedure TForm1.PerformLongPoll;
begin
  if FCommunicationMode <> cmLongPolling then
    Exit;

  // Exécuter en thread séparé pour ne pas bloquer l'UI
  TThread.CreateAnonymousThread(
    procedure
    var
      Response: string;
    begin
      try
        // Effectuer la requête de long polling
        Response := FHTTPClient.Get('https://api.exemple.com/poll?clientId=' + FClientId);

        // Traiter la réponse sur le thread UI
        TThread.Queue(nil,
          procedure
          begin
            if Response <> '' then
              HandleServerMessage(Response);

            // Continuer le polling
            PerformLongPoll;
          end
        );
      except
        on E: Exception do
        begin
          // Gérer les erreurs sur le thread UI
          TThread.Queue(nil,
            procedure
            begin
              Memo1.Lines.Add('Erreur de long polling: ' + E.Message);

              // Attendre un peu avant de réessayer
              TTimer.Create(
                procedure(Sender: TObject)
                begin
                  TTimer(Sender).Enabled := False;
                  TTimer(Sender).Free;
                  PerformLongPoll;
                end
              ).Interval := 5000; // 5 secondes
            end
          );
        end;
      end;
    end
  ).Start;
end;

procedure TForm1.SendMessageWithFallback(const Message: string);
begin
  case FCommunicationMode of
    cmWebSocket:
      // Envoyer via WebSocket
      sgcWebSocketClient1.WriteData(Message);

    cmLongPolling:
      // Envoyer via HTTP POST
      TThread.CreateAnonymousThread(
        procedure
        begin
          try
            FHTTPClient.Post('https://api.exemple.com/send?clientId=' + FClientId, Message);
          except
            on E: Exception do
            begin
              TThread.Queue(nil,
                procedure
                begin
                  Memo1.Lines.Add('Erreur d''envoi: ' + E.Message);
                end
              );
            end;
          end;
        end
      ).Start;
  end;
end;
```

### Problème 3 : Gestion de la concurrence côté serveur

**Symptôme** : Problèmes de performance ou de corruption de données lorsque plusieurs clients modifient les mêmes données simultanément.

**Solution** : Utiliser un système de verrouillage ou de concurrence optimiste.

```pascal
type
  TLockManager = class
  private
    FLocks: TDictionary<string, string>; // Ressource -> ID de connexion
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    function AcquireLock(const Resource, ConnectionId: string): Boolean;
    procedure ReleaseLock(const Resource, ConnectionId: string);
    function GetLockOwner(const Resource: string): string;
  end;

constructor TLockManager.Create;
begin
  inherited;
  FLocks := TDictionary<string, string>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TLockManager.Destroy;
begin
  FLocks.Free;
  FLock.Free;
  inherited;
end;

function TLockManager.AcquireLock(const Resource, ConnectionId: string): Boolean;
begin
  FLock.Enter;
  try
    // Vérifier si la ressource est déjà verrouillée
    if FLocks.ContainsKey(Resource) then
    begin
      // Déjà verrouillée par quelqu'un d'autre?
      Result := FLocks[Resource] = ConnectionId;
    end
    else
    begin
      // Acquérir le verrou
      FLocks.Add(Resource, ConnectionId);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

procedure TLockManager.ReleaseLock(const Resource, ConnectionId: string);
begin
  FLock.Enter;
  try
    // Ne libérer que si nous sommes le propriétaire
    if FLocks.ContainsKey(Resource) and (FLocks[Resource] = ConnectionId) then
      FLocks.Remove(Resource);
  finally
    FLock.Leave;
  end;
end;

function TLockManager.GetLockOwner(const Resource: string): string;
begin
  FLock.Enter;
  try
    if FLocks.TryGetValue(Resource, Result) then
      // Retourner le propriétaire
    else
      Result := '';
  finally
    FLock.Leave;
  end;
end;

// Utilisation dans le serveur WebSocket
procedure TServerModule.HandleDocumentEditRequest(Connection: TsgcWSConnection;
                                               const DocumentId, Operation: string);
var
  LockResult: TJSONObject;
begin
  // Essayer d'acquérir le verrou
  if FLockManager.AcquireLock('document:' + DocumentId, Connection.Guid) then
  begin
    // Verrou acquis, effectuer l'opération
    PerformDocumentOperation(DocumentId, Operation);

    // Informer le client
    LockResult := TJSONObject.Create;
    try
      LockResult.AddPair('type', 'lock_result');
      LockResult.AddPair('resource', DocumentId);
      LockResult.AddPair('status', 'acquired');
      LockResult.AddPair('operation', 'completed');

      // Envoyer uniquement à ce client
      Connection.WriteData(LockResult.ToString);
    finally
      LockResult.Free;
    end;
  end
  else
  begin
    // Verrou non disponible
    LockResult := TJSONObject.Create;
    try
      LockResult.AddPair('type', 'lock_result');
      LockResult.AddPair('resource', DocumentId);
      LockResult.AddPair('status', 'denied');
      LockResult.AddPair('owner', FLockManager.GetLockOwner('document:' + DocumentId));

      // Envoyer uniquement à ce client
      Connection.WriteData(LockResult.ToString);
    finally
      LockResult.Free;
    end;
  end;
end;
```

## Récapitulatif

Dans ce chapitre, nous avons exploré en profondeur les WebSockets et les communications en temps réel avec Delphi :

1. **Principes fondamentaux** : Nous avons compris ce que sont les WebSockets et leurs avantages par rapport aux méthodes traditionnelles.

2. **Implémentation de base** : Nous avons créé des clients et des serveurs WebSocket simples.

3. **Avancé** : Nous avons vu comment gérer les connexions, les erreurs et les reconnexions, ainsi que l'échange de données structurées au format JSON.

4. **Optimisation** : Nous avons exploré différentes techniques pour améliorer les performances, comme la compression et le regroupement de messages.

5. **Sécurité** : Nous avons abordé les meilleures pratiques pour sécuriser les communications WebSocket.

6. **Intégration** : Nous avons vu comment intégrer les WebSockets dans une architecture plus large, notamment avec des systèmes de messagerie comme Redis.

7. **Alternatives** : Nous avons présenté d'autres technologies de communication en temps réel comme MQTT et SignalR.

8. **Solutions aux problèmes courants** : Nous avons fourni des solutions pratiques pour résoudre les défis typiques rencontrés avec les WebSockets.

## Exemples de projets complets

Pour mettre en pratique ces concepts, envisagez de créer l'un de ces projets :

1. **Application de chat en temps réel** : Créez un système de messagerie instantanée avec des salons, des messages privés et des indicateurs de présence.

2. **Tableau de bord en direct** : Développez une application qui affiche des données en temps réel provenant de différentes sources.

3. **Système de collaboration** : Créez un outil permettant à plusieurs utilisateurs de travailler simultanément sur un document ou un dessin.

4. **Jeu multijoueur** : Développez un jeu simple où plusieurs joueurs peuvent interagir en temps réel.

## Où aller maintenant ?

Pour approfondir vos connaissances sur les WebSockets et les communications en temps réel, voici quelques pistes :

1. **Apprendre les frameworks serveur** : Explorez des frameworks comme Node.js, ASP.NET Core ou Django Channels pour créer des backends WebSocket robustes.

2. **Explorer le multithreading avancé** : Approfondissez la gestion de la concurrence pour des applications WebSocket hautement performantes.

3. **Découvrir l'écosystème IoT** : Connectez vos applications Delphi à des dispositifs IoT en utilisant WebSockets ou MQTT.

4. **Étudier les architectures distribuées** : Apprenez à utiliser les WebSockets dans des architectures de microservices avec des systèmes de messagerie comme Kafka ou RabbitMQ.

Les communications en temps réel sont essentielles dans le paysage applicatif moderne, et avec les connaissances acquises dans ce chapitre, vous êtes maintenant équipé pour créer des applications Delphi interactives et réactives qui répondent aux attentes des utilisateurs d'aujourd'hui.

---

*Note : Ce tutoriel est basé sur Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria. Les fonctionnalités avancées de WebSockets peuvent nécessiter l'utilisation de bibliothèques tierces comme sgcWebSockets.*
