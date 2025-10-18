🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 10.3 Socket et communications TCP/IP

## Introduction aux communications réseau

### Qu'est-ce qu'un Socket ?

Un **socket** est un point de connexion permettant à deux programmes de communiquer entre eux via un réseau. Imaginez-le comme une prise téléphonique : un programme "appelle" et l'autre "décroche" pour établir une conversation.

Les sockets permettent de créer des applications qui communiquent :
- Sur le même ordinateur (localhost)
- Sur un réseau local (LAN)
- Sur Internet

### Le modèle Client-Serveur

La plupart des communications réseau fonctionnent selon le modèle client-serveur :

**Le Serveur :**
- Écoute les connexions entrantes
- Attend qu'un client se connecte
- Peut gérer plusieurs clients simultanément
- Exemple : un serveur web, un serveur de jeu

**Le Client :**
- Initie la connexion vers le serveur
- Envoie des requêtes
- Reçoit des réponses
- Exemple : votre navigateur web, une application de chat

### TCP/IP : Le protocole de communication

**TCP** (Transmission Control Protocol) est un protocole qui garantit :
- **Fiabilité** : Les données arrivent dans le bon ordre
- **Intégrité** : Pas de perte de données
- **Connexion** : Une connexion établie avant l'échange

**IP** (Internet Protocol) gère l'adressage :
- Chaque ordinateur a une **adresse IP** (ex: 192.168.1.10)
- Un **port** identifie l'application (ex: 8080)
- Format complet : `192.168.1.10:8080`

**Ports courants :**
- Port 80 : HTTP (Web)
- Port 443 : HTTPS (Web sécurisé)
- Port 21 : FTP
- Port 25 : SMTP (Email)
- Port 3306 : MySQL
- Ports 1024-65535 : Disponibles pour vos applications

### UDP vs TCP

Delphi supporte deux types de protocoles :

**TCP (Transmission Control Protocol) :**
- ✅ Connexion établie
- ✅ Données garanties et ordonnées
- ✅ Détection d'erreurs
- ❌ Plus lent
- Usage : Chat, transfert de fichiers, web

**UDP (User Datagram Protocol) :**
- ✅ Très rapide
- ✅ Pas de connexion nécessaire
- ❌ Pas de garantie de livraison
- ❌ Pas d'ordre garanti
- Usage : Jeux en ligne, streaming vidéo, DNS

Nous nous concentrerons sur **TCP** qui est le plus utilisé.

## Les composants réseau dans Delphi

### Indy Components (Recommandé pour débuter)

**Indy** (Internet Direct) est une bibliothèque de composants réseau incluse avec Delphi. Elle est puissante et facile à utiliser.

**Composants principaux :**
- `TIdTCPServer` : Serveur TCP
- `TIdTCPClient` : Client TCP
- `TIdUDPServer` : Serveur UDP
- `TIdUDPClient` : Client UDP

Vous trouverez ces composants dans la palette **Indy Servers** et **Indy Clients**.

### Autres alternatives

- **System.Net.Socket** : API bas niveau de Delphi
- **Synapse** : Bibliothèque tierce légère
- **WinSock** : API Windows native

Pour ce tutoriel, nous utiliserons **Indy** qui offre le meilleur rapport simplicité/puissance pour les débutants.

## Créer un serveur TCP simple

### Configuration du serveur

Commençons par créer un serveur qui écoute les connexions :

**Étape 1 : Ajouter les composants**

Sur votre formulaire serveur, ajoutez :
- Un `TIdTCPServer` (palette Indy Servers)
- Un `TMemo` pour afficher les logs
- Un `TButton` pour démarrer/arrêter le serveur

**Étape 2 : Configuration de base**

```pascal
unit UnitServeur;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext;

type
  TFormServeur = class(TForm)
    IdTCPServer1: TIdTCPServer;
    MemoLog: TMemo;
    ButtonDemarrer: TButton;
    ButtonArreter: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonDemarrerClick(Sender: TObject);
    procedure ButtonArreterClick(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Execute(AContext: TIdContext);
  private
    procedure AjouterLog(const Message: string);
  public
  end;

var
  FormServeur: TFormServeur;

implementation

{$R *.dfm}

procedure TFormServeur.FormCreate(Sender: TObject);
begin
  // Configuration du serveur
  IdTCPServer1.DefaultPort := 8080;  // Port d'écoute
  IdTCPServer1.Active := False;

  ButtonArreter.Enabled := False;

  AjouterLog('Serveur prêt. Port: ' + IntToStr(IdTCPServer1.DefaultPort));
end;

procedure TFormServeur.ButtonDemarrerClick(Sender: TObject);
begin
  try
    IdTCPServer1.Active := True;

    ButtonDemarrer.Enabled := False;
    ButtonArreter.Enabled := True;

    AjouterLog('Serveur démarré sur le port ' + IntToStr(IdTCPServer1.DefaultPort));
  except
    on E: Exception do
    begin
      AjouterLog('Erreur de démarrage: ' + E.Message);
      ShowMessage('Impossible de démarrer le serveur: ' + E.Message);
    end;
  end;
end;

procedure TFormServeur.ButtonArreterClick(Sender: TObject);
begin
  IdTCPServer1.Active := False;

  ButtonDemarrer.Enabled := True;
  ButtonArreter.Enabled := False;

  AjouterLog('Serveur arrêté');
end;

procedure TFormServeur.AjouterLog(const Message: string);
begin
  // Cette méthode peut être appelée depuis n'importe quel thread
  TThread.Synchronize(nil, procedure
  begin
    MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
  end);
end;

procedure TFormServeur.IdTCPServer1Connect(AContext: TIdContext);
begin
  // Un client vient de se connecter
  AjouterLog('Client connecté: ' + AContext.Binding.PeerIP);
end;

procedure TFormServeur.IdTCPServer1Disconnect(AContext: TIdContext);
begin
  // Un client vient de se déconnecter
  AjouterLog('Client déconnecté: ' + AContext.Binding.PeerIP);
end;

procedure TFormServeur.IdTCPServer1Execute(AContext: TIdContext);
var
  MessageRecu: string;
begin
  // Cette méthode s'exécute en boucle pour chaque client connecté
  // Elle tourne dans un thread séparé pour chaque client

  // Lire les données envoyées par le client
  MessageRecu := AContext.Connection.IOHandler.ReadLn;

  if MessageRecu <> '' then
  begin
    AjouterLog('Message reçu de ' + AContext.Binding.PeerIP + ': ' + MessageRecu);

    // Renvoyer une réponse au client
    AContext.Connection.IOHandler.WriteLn('Serveur: Message reçu - ' + MessageRecu);
  end;
end;

end.
```

**Explication du code :**

- `DefaultPort` : Le port sur lequel le serveur écoute
- `Active` : Démarre/arrête le serveur
- `OnConnect` : Déclenché quand un client se connecte
- `OnDisconnect` : Déclenché quand un client se déconnecte
- `OnExecute` : Boucle principale pour chaque client
- `AContext` : Contient les informations sur le client
- `ReadLn` : Lit une ligne de texte
- `WriteLn` : Envoie une ligne de texte
- `TThread.Synchronize` : Permet de mettre à jour l'interface depuis un thread

## Créer un client TCP simple

### Configuration du client

Créons maintenant un client qui se connecte au serveur :

**Étape 1 : Ajouter les composants**

Sur votre formulaire client, ajoutez :
- Un `TIdTCPClient` (palette Indy Clients)
- Un `TEdit` pour saisir les messages
- Un `TButton` pour connecter/déconnecter
- Un `TButton` pour envoyer des messages
- Un `TMemo` pour afficher les réponses

**Étape 2 : Code du client**

```pascal
unit UnitClient;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TFormClient = class(TForm)
    IdTCPClient1: TIdTCPClient;
    EditMessage: TEdit;
    ButtonConnecter: TButton;
    ButtonDeconnecter: TButton;
    ButtonEnvoyer: TButton;
    MemoReponses: TMemo;
    EditServeur: TEdit;
    EditPort: TEdit;
    LabelServeur: TLabel;
    LabelPort: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonDeconnecterClick(Sender: TObject);
    procedure ButtonEnvoyerClick(Sender: TObject);
  private
    procedure AjouterLog(const Message: string);
  public
  end;

var
  FormClient: TFormClient;

implementation

{$R *.dfm}

procedure TFormClient.FormCreate(Sender: TObject);
begin
  // Configuration par défaut
  EditServeur.Text := 'localhost';  // ou '127.0.0.1'
  EditPort.Text := '8080';

  ButtonDeconnecter.Enabled := False;
  ButtonEnvoyer.Enabled := False;

  AjouterLog('Client prêt');
end;

procedure TFormClient.ButtonConnecterClick(Sender: TObject);
begin
  try
    // Configuration de la connexion
    IdTCPClient1.Host := EditServeur.Text;
    IdTCPClient1.Port := StrToInt(EditPort.Text);

    // Timeout de connexion (5 secondes)
    IdTCPClient1.ConnectTimeout := 5000;
    IdTCPClient1.ReadTimeout := 10000;

    // Connexion au serveur
    IdTCPClient1.Connect;

    if IdTCPClient1.Connected then
    begin
      AjouterLog('Connecté au serveur ' + IdTCPClient1.Host + ':' + IntToStr(IdTCPClient1.Port));

      ButtonConnecter.Enabled := False;
      ButtonDeconnecter.Enabled := True;
      ButtonEnvoyer.Enabled := True;
      EditServeur.Enabled := False;
      EditPort.Enabled := False;
    end;

  except
    on E: Exception do
    begin
      AjouterLog('Erreur de connexion: ' + E.Message);
      ShowMessage('Impossible de se connecter: ' + E.Message);
    end;
  end;
end;

procedure TFormClient.ButtonDeconnecterClick(Sender: TObject);
begin
  try
    if IdTCPClient1.Connected then
      IdTCPClient1.Disconnect;

    AjouterLog('Déconnecté du serveur');

    ButtonConnecter.Enabled := True;
    ButtonDeconnecter.Enabled := False;
    ButtonEnvoyer.Enabled := False;
    EditServeur.Enabled := True;
    EditPort.Enabled := True;

  except
    on E: Exception do
      AjouterLog('Erreur lors de la déconnexion: ' + E.Message);
  end;
end;

procedure TFormClient.ButtonEnvoyerClick(Sender: TObject);
var
  Message, Reponse: string;
begin
  if not IdTCPClient1.Connected then
  begin
    ShowMessage('Non connecté au serveur');
    Exit;
  end;

  Message := EditMessage.Text;

  if Message.IsEmpty then
  begin
    ShowMessage('Veuillez saisir un message');
    Exit;
  end;

  try
    // Envoyer le message
    IdTCPClient1.IOHandler.WriteLn(Message);
    AjouterLog('Envoyé: ' + Message);

    // Recevoir la réponse
    Reponse := IdTCPClient1.IOHandler.ReadLn;
    AjouterLog('Reçu: ' + Reponse);

    // Effacer le champ de saisie
    EditMessage.Clear;
    EditMessage.SetFocus;

  except
    on E: Exception do
    begin
      AjouterLog('Erreur: ' + E.Message);
      ShowMessage('Erreur de communication: ' + E.Message);
    end;
  end;
end;

procedure TFormClient.AjouterLog(const Message: string);
begin
  MemoReponses.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
end;

end.
```

**Explication du code :**

- `Host` : Adresse IP ou nom du serveur
- `Port` : Port du serveur
- `ConnectTimeout` : Délai maximum pour se connecter
- `ReadTimeout` : Délai maximum pour lire des données
- `Connect` : Établit la connexion
- `Disconnect` : Ferme la connexion
- `Connected` : Vérifie si connecté
- `IOHandler.WriteLn` : Envoie une ligne
- `IOHandler.ReadLn` : Reçoit une ligne

## Communication bidirectionnelle

### Serveur qui envoie des messages

Pour permettre au serveur d'envoyer des messages à tous les clients connectés :

```pascal
procedure TFormServeur.ButtonDiffuserClick(Sender: TObject);
var
  Liste: TList;
  i: Integer;
  Context: TIdContext;
  Message: string;
begin
  Message := EditMessageServeur.Text;

  if Message.IsEmpty then
    Exit;

  // Obtenir la liste de tous les clients connectés
  Liste := IdTCPServer1.Contexts.LockList;
  try
    AjouterLog('Diffusion à ' + IntToStr(Liste.Count) + ' client(s): ' + Message);

    // Envoyer le message à chaque client
    for i := 0 to Liste.Count - 1 do
    begin
      Context := TIdContext(Liste[i]);
      try
        Context.Connection.IOHandler.WriteLn('Serveur: ' + Message);
      except
        on E: Exception do
          AjouterLog('Erreur envoi à ' + Context.Binding.PeerIP + ': ' + E.Message);
      end;
    end;

  finally
    IdTCPServer1.Contexts.UnlockList;
  end;

  EditMessageServeur.Clear;
end;
```

**Points importants :**
- `Contexts.LockList` : Verrouille la liste des clients (thread-safe)
- `UnlockList` : Déverrouille obligatoirement dans `finally`
- Toujours gérer les exceptions lors de l'envoi (client peut se déconnecter)

### Client qui écoute en continu

Pour qu'un client reçoive des messages du serveur en continu, utilisez un thread :

```pascal
type
  TThreadReception = class(TThread)
  private
    FClient: TIdTCPClient;
    FForm: TFormClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient; AForm: TFormClient);
  end;

constructor TThreadReception.Create(AClient: TIdTCPClient; AForm: TFormClient);
begin
  inherited Create(False); // Démarrer immédiatement
  FreeOnTerminate := True;
  FClient := AClient;
  FForm := AForm;
end;

procedure TThreadReception.Execute;
var
  Message: string;
begin
  while not Terminated and FClient.Connected do
  begin
    try
      // Lire les messages du serveur
      Message := FClient.IOHandler.ReadLn;

      if not Message.IsEmpty then
      begin
        // Mettre à jour l'interface dans le thread principal
        TThread.Synchronize(nil, procedure
        begin
          FForm.AjouterLog('Reçu: ' + Message);
        end);
      end;

    except
      on E: Exception do
      begin
        if not Terminated then
        begin
          TThread.Synchronize(nil, procedure
          begin
            FForm.AjouterLog('Erreur réception: ' + E.Message);
          end);
          Terminate;
        end;
      end;
    end;
  end;
end;
```

**Utilisation dans le formulaire :**

```pascal
type
  TFormClient = class(TForm)
    // ...
  private
    FThreadReception: TThreadReception;
  end;

procedure TFormClient.ButtonConnecterClick(Sender: TObject);
begin
  // ... code de connexion ...

  if IdTCPClient1.Connected then
  begin
    // Démarrer le thread de réception
    FThreadReception := TThreadReception.Create(IdTCPClient1, Self);
  end;
end;

procedure TFormClient.ButtonDeconnecterClick(Sender: TObject);
begin
  // Arrêter le thread
  if Assigned(FThreadReception) then
  begin
    FThreadReception.Terminate;
    FThreadReception := nil;
  end;

  // ... code de déconnexion ...
end;
```

## Envoi de données binaires

### Envoyer des fichiers

Pour transférer des fichiers entre client et serveur :

```pascal
procedure TFormClient.EnvoyerFichier(const NomFichier: string);
var
  Stream: TFileStream;
  Taille: Int64;
begin
  if not FileExists(NomFichier) then
  begin
    ShowMessage('Fichier introuvable');
    Exit;
  end;

  Stream := TFileStream.Create(NomFichier, fmOpenRead);
  try
    Taille := Stream.Size;

    // Envoyer d'abord le nom du fichier et sa taille
    IdTCPClient1.IOHandler.WriteLn(ExtractFileName(NomFichier));
    IdTCPClient1.IOHandler.Write(Taille);

    // Envoyer le contenu du fichier
    IdTCPClient1.IOHandler.Write(Stream, Taille);

    AjouterLog('Fichier envoyé: ' + NomFichier + ' (' + FormatFloat('#,##0', Taille) + ' octets)');

  finally
    Stream.Free;
  end;
end;
```

**Réception côté serveur :**

```pascal
procedure TFormServeur.IdTCPServer1Execute(AContext: TIdContext);
var
  NomFichier: string;
  Taille: Int64;
  Stream: TFileStream;
begin
  // Recevoir le nom du fichier
  NomFichier := AContext.Connection.IOHandler.ReadLn;

  // Recevoir la taille
  Taille := AContext.Connection.IOHandler.ReadInt64;

  // Recevoir le contenu
  Stream := TFileStream.Create('C:\Temp\' + NomFichier, fmCreate);
  try
    AContext.Connection.IOHandler.ReadStream(Stream, Taille);

    AjouterLog('Fichier reçu: ' + NomFichier + ' (' + FormatFloat('#,##0', Taille) + ' octets)');

    // Confirmer la réception
    AContext.Connection.IOHandler.WriteLn('Fichier reçu avec succès');

  finally
    Stream.Free;
  end;
end;
```

### Envoyer des structures de données

Pour envoyer des données structurées, utilisez JSON ou un format personnalisé :

```pascal
procedure TFormClient.EnvoyerDonnees;
var
  JSONObject: TJSONObject;
  Donnees: string;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('type', 'utilisateur');
    JSONObject.AddPair('nom', 'Dupont');
    JSONObject.AddPair('age', TJSONNumber.Create(30));

    Donnees := JSONObject.ToString;

    IdTCPClient1.IOHandler.WriteLn(Donnees);

  finally
    JSONObject.Free;
  end;
end;
```

## Gestion des erreurs et déconnexions

### Détecter une déconnexion

```pascal
procedure TFormClient.VerifierConnexion;
begin
  if IdTCPClient1.Connected then
  begin
    try
      // Envoyer un ping
      IdTCPClient1.IOHandler.WriteLn('PING');

      // Attendre la réponse
      if IdTCPClient1.IOHandler.ReadLn(500) = 'PONG' then
        AjouterLog('Connexion active')
      else
        AjouterLog('Pas de réponse du serveur');

    except
      on E: Exception do
      begin
        AjouterLog('Connexion perdue: ' + E.Message);
        // Déclencher une déconnexion propre
        ButtonDeconnecterClick(nil);
      end;
    end;
  end
  else
    AjouterLog('Non connecté');
end;
```

### Gérer les timeouts

```pascal
procedure TFormClient.ConfigurerTimeouts;
begin
  // Timeout de connexion (5 secondes)
  IdTCPClient1.ConnectTimeout := 5000;

  // Timeout de lecture (10 secondes)
  IdTCPClient1.ReadTimeout := 10000;

  // Garder la connexion active
  IdTCPClient1.IOHandler.DefStringEncoding := TEncoding.UTF8;
end;
```

### Reconnexion automatique

```pascal
type
  TFormClient = class(TForm)
    TimerReconnexion: TTimer;
    procedure TimerReconnexionTimer(Sender: TObject);
  private
    FNombreEssais: Integer;
  end;

procedure TFormClient.TimerReconnexionTimer(Sender: TObject);
begin
  if not IdTCPClient1.Connected then
  begin
    Inc(FNombreEssais);

    if FNombreEssais <= 5 then
    begin
      AjouterLog('Tentative de reconnexion ' + IntToStr(FNombreEssais) + '/5...');

      try
        IdTCPClient1.Connect;

        if IdTCPClient1.Connected then
        begin
          AjouterLog('Reconnecté avec succès');
          TimerReconnexion.Enabled := False;
          FNombreEssais := 0;
        end;

      except
        on E: Exception do
          AjouterLog('Échec de reconnexion: ' + E.Message);
      end;
    end
    else
    begin
      TimerReconnexion.Enabled := False;
      AjouterLog('Impossible de se reconnecter après 5 tentatives');
      FNombreEssais := 0;
    end;
  end;
end;

procedure TFormClient.ButtonDeconnecterClick(Sender: TObject);
begin
  // ... déconnexion ...

  // Activer la reconnexion automatique
  TimerReconnexion.Interval := 3000; // 3 secondes
  TimerReconnexion.Enabled := True;
  FNombreEssais := 0;
end;
```

## Sécurité et bonnes pratiques

### Validation des données

Toujours valider les données reçues :

```pascal
procedure TFormServeur.IdTCPServer1Execute(AContext: TIdContext);
var
  Commande: string;
begin
  Commande := AContext.Connection.IOHandler.ReadLn;

  // Limiter la taille des commandes
  if Length(Commande) > 1024 then
  begin
    AjouterLog('Commande trop longue reçue de ' + AContext.Binding.PeerIP);
    AContext.Connection.Disconnect;
    Exit;
  end;

  // Valider le format
  if not CommadeValide(Commande) then
  begin
    AjouterLog('Commande invalide: ' + Commande);
    AContext.Connection.IOHandler.WriteLn('ERREUR: Commande invalide');
    Exit;
  end;

  // Traiter la commande validée
  TraiterCommande(AContext, Commande);
end;

function TFormServeur.CommadeValide(const Commande: string): Boolean;
begin
  // Implémenter votre logique de validation
  Result := not Commande.IsEmpty;
end;
```

### Limiter le nombre de connexions

```pascal
procedure TFormServeur.IdTCPServer1Connect(AContext: TIdContext);
var
  NombreClients: Integer;
const
  MAX_CLIENTS = 10;
begin
  NombreClients := IdTCPServer1.Contexts.LockList.Count;
  IdTCPServer1.Contexts.UnlockList;

  if NombreClients > MAX_CLIENTS then
  begin
    AjouterLog('Nombre maximum de clients atteint. Refus de ' + AContext.Binding.PeerIP);
    AContext.Connection.IOHandler.WriteLn('Serveur plein');
    AContext.Connection.Disconnect;
    Exit;
  end;

  AjouterLog('Client connecté: ' + AContext.Binding.PeerIP + ' (' + IntToStr(NombreClients) + '/' + IntToStr(MAX_CLIENTS) + ')');
end;
```

### Encoder correctement les chaînes

```pascal
procedure TFormServeur.FormCreate(Sender: TObject);
begin
  // Utiliser UTF-8 pour l'encodage
  IdTCPServer1.IOHandler.DefStringEncoding := TEncoding.UTF8;
end;

procedure TFormClient.FormCreate(Sender: TObject);
begin
  // Idem pour le client
  IdTCPClient1.IOHandler.DefStringEncoding := TEncoding.UTF8;
end;
```

### Nettoyer les ressources

```pascal
procedure TFormServeur.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Arrêter le serveur proprement
  if IdTCPServer1.Active then
  begin
    IdTCPServer1.Active := False;

    // Attendre que tous les threads se terminent
    Sleep(500);
  end;
end;

procedure TFormClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Arrêter le thread de réception
  if Assigned(FThreadReception) then
  begin
    FThreadReception.Terminate;
    FThreadReception.WaitFor;
  end;

  // Déconnecter
  if IdTCPClient1.Connected then
    IdTCPClient1.Disconnect;
end;
```

## Exemple complet : Application de chat

### Serveur de chat

```pascal
unit UnitChatServeur;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, IdContext,
  System.Generics.Collections;

type
  TFormChatServeur = class(TForm)
    IdTCPServer1: TIdTCPServer;
    MemoLog: TMemo;
    ButtonDemarrer: TButton;
    ButtonArreter: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonDemarrerClick(Sender: TObject);
    procedure ButtonArreterClick(Sender: TObject);
    procedure IdTCPServer1Connect(AContext: TIdContext);
    procedure IdTCPServer1Disconnect(AContext: TIdContext);
    procedure IdTCPServer1Execute(AContext: TIdContext);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FUtilisateurs: TDictionary<string, string>; // IP -> Pseudo
    procedure AjouterLog(const Message: string);
    procedure DiffuserMessage(const Message: string; ExclureContext: TIdContext = nil);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormChatServeur: TFormChatServeur;

implementation

{$R *.dfm}

constructor TFormChatServeur.Create(AOwner: TComponent);
begin
  inherited;
  FUtilisateurs := TDictionary<string, string>.Create;
end;

destructor TFormChatServeur.Destroy;
begin
  FUtilisateurs.Free;
  inherited;
end;

procedure TFormChatServeur.FormCreate(Sender: TObject);
begin
  IdTCPServer1.DefaultPort := 8080;
  IdTCPServer1.Active := False;
  ButtonArreter.Enabled := False;

  AjouterLog('Serveur de chat prêt');
end;

procedure TFormChatServeur.ButtonDemarrerClick(Sender: TObject);
begin
  try
    IdTCPServer1.Active := True;
    ButtonDemarrer.Enabled := False;
    ButtonArreter.Enabled := True;
    AjouterLog('Serveur démarré sur le port ' + IntToStr(IdTCPServer1.DefaultPort));
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;

procedure TFormChatServeur.ButtonArreterClick(Sender: TObject);
begin
  IdTCPServer1.Active := False;
  ButtonDemarrer.Enabled := True;
  ButtonArreter.Enabled := False;
  AjouterLog('Serveur arrêté');
end;

procedure TFormChatServeur.IdTCPServer1Connect(AContext: TIdContext);
var
  Pseudo: string;
begin
  // Recevoir le pseudo du nouvel utilisateur
  Pseudo := AContext.Connection.IOHandler.ReadLn;

  // Stocker le pseudo
  TMonitor.Enter(FUtilisateurs);
  try
    FUtilisateurs.Add(AContext.Binding.PeerIP, Pseudo);
  finally
    TMonitor.Exit(FUtilisateurs);
  end;

  AjouterLog(Pseudo + ' s''est connecté (' + AContext.Binding.PeerIP + ')');

  // Informer tous les autres utilisateurs
  DiffuserMessage('*** ' + Pseudo + ' a rejoint le chat ***', AContext);

  // Envoyer un message de bienvenue
  AContext.Connection.IOHandler.WriteLn('Bienvenue ' + Pseudo + ' !');
end;

procedure TFormChatServeur.IdTCPServer1Disconnect(AContext: TIdContext);
var
  Pseudo: string;
begin
  TMonitor.Enter(FUtilisateurs);
  try
    if FUtilisateurs.TryGetValue(AContext.Binding.PeerIP, Pseudo) then
    begin
      FUtilisateurs.Remove(AContext.Binding.PeerIP);
      AjouterLog(Pseudo + ' s''est déconnecté');
      DiffuserMessage('*** ' + Pseudo + ' a quitté le chat ***');
    end;
  finally
    TMonitor.Exit(FUtilisateurs);
  end;
end;

procedure TFormChatServeur.IdTCPServer1Execute(AContext: TIdContext);
var
  Message, Pseudo: string;
begin
  Message := AContext.Connection.IOHandler.ReadLn;

  if not Message.IsEmpty then
  begin
    TMonitor.Enter(FUtilisateurs);
    try
      if FUtilisateurs.TryGetValue(AContext.Binding.PeerIP, Pseudo) then
      begin
        AjouterLog(Pseudo + ': ' + Message);
        DiffuserMessage(Pseudo + ': ' + Message, AContext);
      end;
    finally
      TMonitor.Exit(FUtilisateurs);
    end;
  end;
end;

procedure TFormChatServeur.DiffuserMessage(const Message: string; ExclureContext: TIdContext);
var
  Liste: TList;
  i: Integer;
  Context: TIdContext;
begin
  Liste := IdTCPServer1.Contexts.LockList;
  try
    for i := 0 to Liste.Count - 1 do
    begin
      Context := TIdContext(Liste[i]);

      // Ne pas renvoyer au client qui a envoyé le message
      if Context <> ExclureContext then
      begin
        try
          Context.Connection.IOHandler.WriteLn(Message);
        except
          // Ignorer les erreurs (client déconnecté)
        end;
      end;
    end;
  finally
    IdTCPServer1.Contexts.UnlockList;
  end;
end;

procedure TFormChatServeur.AjouterLog(const Message: string);
begin
  TThread.Synchronize(nil, procedure
  begin
    MemoLog.Lines.Add('[' + TimeToStr(Now) + '] ' + Message);
  end);
end;

procedure TFormChatServeur.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if IdTCPServer1.Active then
    IdTCPServer1.Active := False;
end;

end.
```

### Client de chat

```pascal
unit UnitChatClient;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient;

type
  TThreadReceptionChat = class(TThread)
  private
    FClient: TIdTCPClient;
    FForm: TFormChatClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TIdTCPClient; AForm: TFormChatClient);
  end;

  TFormChatClient = class(TForm)
    IdTCPClient1: TIdTCPClient;
    EditPseudo: TEdit;
    EditServeur: TEdit;
    ButtonConnecter: TButton;
    ButtonDeconnecter: TButton;
    MemoChat: TMemo;
    EditMessage: TEdit;
    ButtonEnvoyer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnecterClick(Sender: TObject);
    procedure ButtonDeconnecterClick(Sender: TObject);
    procedure ButtonEnvoyerClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditMessageKeyPress(Sender: TObject; var Key: Char);
  private
    FThreadReception: TThreadReceptionChat;
  public
    procedure AfficherMessage(const Message: string);
  end;

var
  FormChatClient: TFormChatClient;

implementation

{$R *.dfm}

{ TThreadReceptionChat }

constructor TThreadReceptionChat.Create(AClient: TIdTCPClient; AForm: TFormChatClient);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := AClient;
  FForm := AForm;
end;

procedure TThreadReceptionChat.Execute;
var
  Message: string;
begin
  while not Terminated and FClient.Connected do
  begin
    try
      Message := FClient.IOHandler.ReadLn;

      if not Message.IsEmpty then
      begin
        TThread.Synchronize(nil, procedure
        begin
          FForm.AfficherMessage(Message);
        end);
      end;

    except
      on E: Exception do
      begin
        if not Terminated then
          Terminate;
      end;
    end;
  end;
end;

{ TFormChatClient }

procedure TFormChatClient.FormCreate(Sender: TObject);
begin
  EditServeur.Text := 'localhost';
  EditPseudo.Text := 'Utilisateur' + IntToStr(Random(1000));

  ButtonDeconnecter.Enabled := False;
  ButtonEnvoyer.Enabled := False;
  EditMessage.Enabled := False;
end;

procedure TFormChatClient.ButtonConnecterClick(Sender: TObject);
begin
  if EditPseudo.Text.IsEmpty then
  begin
    ShowMessage('Veuillez saisir un pseudo');
    Exit;
  end;

  try
    IdTCPClient1.Host := EditServeur.Text;
    IdTCPClient1.Port := 8080;
    IdTCPClient1.ConnectTimeout := 5000;

    IdTCPClient1.Connect;

    if IdTCPClient1.Connected then
    begin
      // Envoyer le pseudo
      IdTCPClient1.IOHandler.WriteLn(EditPseudo.Text);

      // Démarrer le thread de réception
      FThreadReception := TThreadReceptionChat.Create(IdTCPClient1, Self);

      MemoChat.Lines.Add('Connecté au serveur de chat');

      ButtonConnecter.Enabled := False;
      ButtonDeconnecter.Enabled := True;
      ButtonEnvoyer.Enabled := True;
      EditMessage.Enabled := True;
      EditPseudo.Enabled := False;
      EditServeur.Enabled := False;

      EditMessage.SetFocus;
    end;

  except
    on E: Exception do
      ShowMessage('Impossible de se connecter: ' + E.Message);
  end;
end;

procedure TFormChatClient.ButtonDeconnecterClick(Sender: TObject);
begin
  if Assigned(FThreadReception) then
  begin
    FThreadReception.Terminate;
    FThreadReception := nil;
  end;

  if IdTCPClient1.Connected then
    IdTCPClient1.Disconnect;

  MemoChat.Lines.Add('Déconnecté du serveur');

  ButtonConnecter.Enabled := True;
  ButtonDeconnecter.Enabled := False;
  ButtonEnvoyer.Enabled := False;
  EditMessage.Enabled := False;
  EditPseudo.Enabled := True;
  EditServeur.Enabled := True;
end;

procedure TFormChatClient.ButtonEnvoyerClick(Sender: TObject);
var
  Message: string;
begin
  Message := EditMessage.Text;

  if Message.IsEmpty then
    Exit;

  try
    IdTCPClient1.IOHandler.WriteLn(Message);
    MemoChat.Lines.Add('Moi: ' + Message);
    EditMessage.Clear;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur d''envoi: ' + E.Message);
      ButtonDeconnecterClick(nil);
    end;
  end;
end;

procedure TFormChatClient.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Touche Entrée
  begin
    ButtonEnvoyerClick(nil);
    Key := #0;
  end;
end;

procedure TFormChatClient.AfficherMessage(const Message: string);
begin
  MemoChat.Lines.Add(Message);
end;

procedure TFormChatClient.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FThreadReception) then
    FThreadReception.Terminate;

  if IdTCPClient1.Connected then
    IdTCPClient1.Disconnect;
end;

end.
```

## Résumé

### Points clés TCP/IP

✅ **Concepts de base :**
- Socket = point de connexion réseau
- TCP = protocole fiable et ordonné
- Client-Serveur = modèle de communication
- IP:Port = adresse complète

✅ **Composants Indy :**
- `TIdTCPServer` pour créer un serveur
- `TIdTCPClient` pour créer un client
- `ReadLn/WriteLn` pour les données texte
- `ReadStream/WriteStream` pour les données binaires

✅ **Multithreading :**
- Le serveur gère chaque client dans un thread
- Utiliser `TThread.Synchronize` pour l'interface
- `Contexts.LockList` pour accéder aux clients

✅ **Bonnes pratiques :**
- Toujours gérer les exceptions
- Configurer les timeouts
- Valider les données reçues
- Nettoyer les ressources
- Utiliser UTF-8 pour l'encodage

✅ **Sécurité :**
- Limiter la taille des données
- Limiter le nombre de connexions
- Valider toutes les entrées
- Gérer proprement les erreurs

Avec ces connaissances, vous pouvez créer des applications réseau robustes : serveurs de jeux, applications de chat, systèmes de transfert de fichiers, et bien plus encore !

⏭️ [Services SOAP et WebServices](/10-communication-et-services-reseaux/04-services-soap-et-webservices.md)
