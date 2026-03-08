🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.5 Protocoles IoT (MQTT, CoAP)

## Introduction aux protocoles IoT

Dans les sections précédentes, nous avons utilisé des communications simples point-à-point : un Arduino connecté directement à une application Delphi via USB ou Bluetooth. Mais dans le monde réel de l'IoT, les besoins sont souvent plus complexes :

- Plusieurs dispositifs doivent communiquer entre eux
- Les données doivent être partagées avec plusieurs applications
- Les connexions peuvent être instables ou intermittentes
- La bande passante est limitée
- Les dispositifs fonctionnent sur batterie (consommation critique)

C'est là qu'interviennent les **protocoles IoT spécialisés**. Ces protocoles ont été conçus spécifiquement pour répondre aux contraintes de l'Internet des Objets.

### Pourquoi pas HTTP ?

HTTP est le protocole du web, vous pourriez vous demander pourquoi ne pas l'utiliser pour l'IoT. HTTP présente plusieurs limitations dans un contexte IoT :

**Inconvénients de HTTP pour l'IoT :**
- **Verbeux** : beaucoup de données échangées pour chaque requête (headers, etc.)
- **Gourmand** : consomme beaucoup de bande passante et d'énergie
- **Modèle client-serveur** : le client doit interroger le serveur régulièrement (polling)
- **Pas de push natif** : difficile de notifier instantanément les clients
- **Connexions multiples** : une nouvelle connexion TCP pour chaque requête (sans keep-alive)

HTTP reste utilisable et utilisé en IoT, mais pour des cas spécifiques où ses avantages (universalité, simplicité) l'emportent sur ses inconvénients.

### Protocoles IoT populaires

Les deux protocoles les plus importants pour l'IoT sont :

1. **MQTT (Message Queuing Telemetry Transport)**
   - Le plus populaire en IoT
   - Léger et efficace
   - Architecture publish/subscribe
   - Idéal pour la plupart des applications IoT

2. **CoAP (Constrained Application Protocol)**
   - Conçu pour les dispositifs très contraints
   - Basé sur UDP (encore plus léger)
   - Similaire à HTTP dans son approche
   - Moins populaire que MQTT mais très efficace

D'autres protocoles existent (AMQP, XMPP, etc.) mais MQTT et CoAP sont les plus pertinents pour débuter.

## MQTT : Le protocole star de l'IoT

### Qu'est-ce que MQTT ?

MQTT est un protocole de messagerie léger, inventé par IBM en 1999 et devenu standard en 2013. Il a été spécifiquement conçu pour les réseaux à faible bande passante et les dispositifs à ressources limitées.

**Caractéristiques principales :**
- **Léger** : header minimal (2 octets minimum)
- **Fiable** : 3 niveaux de qualité de service (QoS)
- **Bidirectionnel** : communication dans les deux sens
- **Indépendant du contenu** : peut transporter n'importe quelle donnée
- **Basé sur TCP** : connexion fiable

### Architecture publish/subscribe

MQTT utilise une architecture différente du modèle client-serveur classique. C'est le modèle **publish/subscribe** (publication/abonnement).

#### Acteurs du système

**1. Le Broker (courtier)**
- Serveur central qui gère tous les messages
- Reçoit les messages des publishers
- Distribue les messages aux subscribers
- Exemples : Mosquitto, HiveMQ, EMQX

**2. Les Publishers (éditeurs)**
- Dispositifs ou applications qui publient des messages
- Envoient des données sur des "topics" spécifiques
- Ne savent pas qui reçoit leurs messages

**3. Les Subscribers (abonnés)**
- Dispositifs ou applications qui reçoivent des messages
- S'abonnent à des "topics" d'intérêt
- Ne savent pas qui publie les messages

**Avantage majeur** : Les publishers et subscribers ne se connaissent pas. Cette indépendance facilite grandement l'ajout ou la suppression de dispositifs.

#### Schéma de fonctionnement

```
[Capteur Température] --publish--> [Broker MQTT] --subscribe--> [App Delphi]
                                        |
                                        +--------subscribe--> [App Mobile]
                                        |
                                        +--------subscribe--> [Serveur Analytics]

[Capteur Humidité] ----publish--> [Broker MQTT]
```

### Concepts MQTT

#### Topics (sujets)

Les topics sont comme des "canaux" ou des "adresses" pour les messages. Ils utilisent une structure hiérarchique avec des slashes (/).

**Exemples de topics :**
```
maison/salon/temperature  
maison/salon/humidite  
maison/chambre/temperature  
jardin/capteur1/temperature  
usine/zone-a/machine-01/etat  
```

**Structure recommandée :**
```
[lieu]/[pièce]/[dispositif]/[mesure]
[organisation]/[zone]/[équipement]/[métrique]
```

#### Wildcards (caractères génériques)

Les subscribers peuvent utiliser des wildcards pour s'abonner à plusieurs topics :

**+ (plus)** : remplace un seul niveau
```
maison/+/temperature
→ reçoit : maison/salon/temperature
→ reçoit : maison/chambre/temperature
→ ne reçoit pas : maison/salon/capteur1/temperature
```

**# (dièse)** : remplace un ou plusieurs niveaux
```
maison/#
→ reçoit tout sous maison/
→ maison/salon/temperature
→ maison/chambre/capteur1/temperature
→ maison/jardin/portail/etat
```

#### Quality of Service (QoS)

MQTT offre 3 niveaux de garantie de livraison :

**QoS 0 : At most once (au plus une fois)**
- Le message est envoyé une seule fois
- Aucune garantie de livraison
- Pas d'accusé de réception
- **Usage** : données non critiques, fréquemment mises à jour (température toutes les secondes)

**QoS 1 : At least once (au moins une fois)**
- Le message est garanti d'être livré au moins une fois
- Peut être livré plusieurs fois (duplicatas possibles)
- Accusé de réception requis
- **Usage** : données importantes où les duplicatas peuvent être gérés

**QoS 2 : Exactly once (exactement une fois)**
- Le message est garanti d'être livré exactement une fois
- Pas de duplicata
- Plus gourmand en ressources (4 échanges)
- **Usage** : données critiques (commandes de facturation, contrôles critiques)

#### Retained Messages

Un message peut être marqué comme "retained" :
- Le broker garde une copie du dernier message publié sur ce topic
- Tout nouveau subscriber reçoit immédiatement ce message
- **Usage** : état actuel d'un dispositif (ON/OFF), dernière valeur connue

#### Last Will and Testament (LWT)

Le LWT est un message que le broker publie automatiquement si un client se déconnecte de manière inattendue :
- Défini lors de la connexion
- Publié par le broker en cas de déconnexion anormale
- **Usage** : détecter les dispositifs hors ligne, alertes

### MQTT avec Delphi

Pour utiliser MQTT en Delphi, plusieurs bibliothèques sont disponibles :

#### Bibliothèques MQTT pour Delphi

1. **TMQTTClient** - Bibliothèque open source populaire
2. **Indy MQTT** - Extension pour Indy
3. **mORMot MQTT** - Partie du framework mORMot

Pour ce tutoriel, nous utiliserons une approche générique applicable à la plupart des bibliothèques MQTT.

### Installation d'un broker MQTT

Avant de programmer, vous avez besoin d'un broker MQTT.

#### Option 1 : Broker public gratuit (pour tests)

Plusieurs services offrent des brokers publics gratuits :
- **broker.hivemq.com** (port 1883)
- **test.mosquitto.org** (port 1883)

⚠️ **Attention** : Ces brokers sont publics, n'importe qui peut voir vos messages. À utiliser uniquement pour des tests.

#### Option 2 : Installer Mosquitto localement

**Mosquitto** est le broker MQTT open source le plus populaire.

**Installation sur Windows :**
1. Télécharger depuis mosquitto.org
2. Installer
3. Démarrer le service Mosquitto
4. Le broker écoute sur localhost:1883

**Configuration de base :**
```
# mosquitto.conf
listener 1883  
allow_anonymous true  
```

Pour la production, configurez l'authentification et le chiffrement (TLS/SSL).

### Exemple complet : Publisher avec Delphi

```pascal
unit MQTTPublisher;

interface

uses
  System.SysUtils, System.Classes;

type
  TMQTTPublisher = class
  private
    FMQTTClient: TMQTTClient;  // Supposons une bibliothèque MQTT
    FBrokerAddress: string;
    FBrokerPort: Integer;
    FConnected: Boolean;
  public
    constructor Create(const BrokerAddress: string; BrokerPort: Integer = 1883);
    destructor Destroy; override;

    procedure Connect(const ClientID: string);
    procedure Disconnect;
    procedure Publish(const Topic: string; const Payload: string; QoS: Integer = 0);

    property Connected: Boolean read FConnected;
  end;

implementation

constructor TMQTTPublisher.Create(const BrokerAddress: string; BrokerPort: Integer);  
begin  
  inherited Create;
  FBrokerAddress := BrokerAddress;
  FBrokerPort := BrokerPort;
  FConnected := False;

  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := FBrokerAddress;
  FMQTTClient.Port := FBrokerPort;
end;

destructor TMQTTPublisher.Destroy;  
begin  
  if FConnected then
    Disconnect;
  FMQTTClient.Free;
  inherited;
end;

procedure TMQTTPublisher.Connect(const ClientID: string);  
begin  
  try
    FMQTTClient.ClientID := ClientID;
    FMQTTClient.Connect;
    FConnected := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur de connexion MQTT: ' + E.Message);
  end;
end;

procedure TMQTTPublisher.Disconnect;  
begin  
  if FConnected then
  begin
    FMQTTClient.Disconnect;
    FConnected := False;
  end;
end;

procedure TMQTTPublisher.Publish(const Topic, Payload: string; QoS: Integer);  
begin  
  if not FConnected then
    raise Exception.Create('Client MQTT non connecté');

  FMQTTClient.Publish(Topic, Payload, QoS);
end;

end.
```

### Exemple : Application de monitoring

```pascal
unit FormMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, MQTTPublisher;

type
  TFormMain = class(TForm)
    ButtonConnect: TButton;
    EditBroker: TEdit;
    EditTopic: TEdit;
    EditMessage: TEdit;
    ButtonPublish: TButton;
    MemoLog: TMemo;
    Timer1: TTimer;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonPublishClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPublisher: TMQTTPublisher;
    procedure Log(const Message: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Log(const Message: string);  
begin  
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Message);
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);  
begin  
  if not Assigned(FPublisher) then
  begin
    try
      FPublisher := TMQTTPublisher.Create(EditBroker.Text);
      FPublisher.Connect('DelphiPublisher_' + IntToStr(Random(9999)));

      Log('Connecté au broker MQTT');
      ButtonConnect.Caption := 'Déconnecter';
      ButtonPublish.Enabled := True;
      Timer1.Enabled := True;
    except
      on E: Exception do
      begin
        Log('Erreur: ' + E.Message);
        ShowMessage('Impossible de se connecter au broker');
      end;
    end;
  end
  else
  begin
    FPublisher.Disconnect;
    FreeAndNil(FPublisher);

    Log('Déconnecté du broker');
    ButtonConnect.Caption := 'Connecter';
    ButtonPublish.Enabled := False;
    Timer1.Enabled := False;
  end;
end;

procedure TFormMain.ButtonPublishClick(Sender: TObject);  
var  
  Topic, Message: string;
begin
  if not Assigned(FPublisher) or not FPublisher.Connected then
  begin
    ShowMessage('Veuillez d''abord vous connecter au broker');
    Exit;
  end;

  Topic := EditTopic.Text;
  Message := EditMessage.Text;

  try
    FPublisher.Publish(Topic, Message, 0);  // QoS 0
    Log(Format('Publié sur "%s": %s', [Topic, Message]));
  except
    on E: Exception do
      Log('Erreur de publication: ' + E.Message);
  end;
end;

procedure TFormMain.Timer1Timer(Sender: TObject);  
var  
  Temperature: Double;
  Humidity: Double;
  JSONPayload: string;
begin
  // Simuler des données de capteur
  Temperature := 20 + Random * 5;
  Humidity := 50 + Random * 20;

  // Créer un payload JSON
  JSONPayload := Format('{"temperature":%.1f,"humidity":%.1f,"timestamp":%d}',
    [Temperature, Humidity, DateTimeToUnix(Now)]);

  // Publier sur le topic
  FPublisher.Publish('maison/salon/sensors', JSONPayload, 0);

  Log(Format('Données publiées: T=%.1f°C, H=%.1f%%', [Temperature, Humidity]));
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  if Assigned(FPublisher) then
    FreeAndNil(FPublisher);
end;

end.
```

### Exemple complet : Subscriber avec Delphi

```pascal
unit MQTTSubscriber;

interface

uses
  System.SysUtils, System.Classes;

type
  TMQTTMessageEvent = procedure(const Topic: string; const Payload: string) of object;

  TMQTTSubscriber = class
  private
    FMQTTClient: TMQTTClient;
    FBrokerAddress: string;
    FBrokerPort: Integer;
    FConnected: Boolean;
    FOnMessage: TMQTTMessageEvent;
    procedure HandleMessage(const Topic: string; const Payload: string);
  public
    constructor Create(const BrokerAddress: string; BrokerPort: Integer = 1883);
    destructor Destroy; override;

    procedure Connect(const ClientID: string);
    procedure Disconnect;
    procedure Subscribe(const Topic: string; QoS: Integer = 0);
    procedure Unsubscribe(const Topic: string);

    property Connected: Boolean read FConnected;
    property OnMessage: TMQTTMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

constructor TMQTTSubscriber.Create(const BrokerAddress: string; BrokerPort: Integer);  
begin  
  inherited Create;
  FBrokerAddress := BrokerAddress;
  FBrokerPort := BrokerPort;
  FConnected := False;

  FMQTTClient := TMQTTClient.Create;
  FMQTTClient.Host := FBrokerAddress;
  FMQTTClient.Port := FBrokerPort;
  FMQTTClient.OnMessage := HandleMessage;  // Callback pour les messages
end;

destructor TMQTTSubscriber.Destroy;  
begin  
  if FConnected then
    Disconnect;
  FMQTTClient.Free;
  inherited;
end;

procedure TMQTTSubscriber.Connect(const ClientID: string);  
begin  
  try
    FMQTTClient.ClientID := ClientID;
    FMQTTClient.Connect;
    FConnected := True;
  except
    on E: Exception do
      raise Exception.Create('Erreur de connexion MQTT: ' + E.Message);
  end;
end;

procedure TMQTTSubscriber.Disconnect;  
begin  
  if FConnected then
  begin
    FMQTTClient.Disconnect;
    FConnected := False;
  end;
end;

procedure TMQTTSubscriber.Subscribe(const Topic: string; QoS: Integer);  
begin  
  if not FConnected then
    raise Exception.Create('Client MQTT non connecté');

  FMQTTClient.Subscribe(Topic, QoS);
end;

procedure TMQTTSubscriber.Unsubscribe(const Topic: string);  
begin  
  if FConnected then
    FMQTTClient.Unsubscribe(Topic);
end;

procedure TMQTTSubscriber.HandleMessage(const Topic: string; const Payload: string);  
begin  
  // Appeler le callback défini par l'utilisateur
  if Assigned(FOnMessage) then
    FOnMessage(Topic, Payload);
end;

end.
```

### Application de tableau de bord MQTT

```pascal
unit DashboardForm;

interface

uses
  System.SysUtils, System.Classes, System.JSON, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ExtCtrls, MQTTSubscriber;

type
  TFormDashboard = class(TForm)
    LabelTemperature: TLabel;
    LabelHumidity: TLabel;
    MemoLog: TMemo;
    ButtonConnect: TButton;
    EditBroker: TEdit;
    ListBoxTopics: TListBox;
    ButtonSubscribe: TButton;
    EditTopic: TEdit;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonSubscribeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FSubscriber: TMQTTSubscriber;
    procedure HandleMQTTMessage(const Topic: string; const Payload: string);
    procedure ProcessSensorData(const JSONPayload: string);
    procedure Log(const Message: string);
  end;

var
  FormDashboard: TFormDashboard;

implementation

{$R *.dfm}

procedure TFormDashboard.Log(const Message: string);  
begin  
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Message);
end;

procedure TFormDashboard.ButtonConnectClick(Sender: TObject);  
begin  
  if not Assigned(FSubscriber) then
  begin
    try
      FSubscriber := TMQTTSubscriber.Create(EditBroker.Text);
      FSubscriber.OnMessage := HandleMQTTMessage;
      FSubscriber.Connect('DelphiSubscriber_' + IntToStr(Random(9999)));

      Log('Connecté au broker MQTT');
      ButtonConnect.Caption := 'Déconnecter';
      ButtonSubscribe.Enabled := True;
    except
      on E: Exception do
      begin
        Log('Erreur: ' + E.Message);
        ShowMessage('Impossible de se connecter');
      end;
    end;
  end
  else
  begin
    FSubscriber.Disconnect;
    FreeAndNil(FSubscriber);

    Log('Déconnecté');
    ButtonConnect.Caption := 'Connecter';
    ButtonSubscribe.Enabled := False;
  end;
end;

procedure TFormDashboard.ButtonSubscribeClick(Sender: TObject);  
var  
  Topic: string;
begin
  if not Assigned(FSubscriber) then Exit;

  Topic := EditTopic.Text;

  try
    FSubscriber.Subscribe(Topic, 0);
    ListBoxTopics.Items.Add(Topic);
    Log('Abonné au topic: ' + Topic);
  except
    on E: Exception do
      Log('Erreur d''abonnement: ' + E.Message);
  end;
end;

procedure TFormDashboard.HandleMQTTMessage(const Topic: string; const Payload: string);  
begin  
  // Appeler depuis le thread principal
  TThread.Synchronize(nil, procedure
  begin
    Log(Format('Reçu [%s]: %s', [Topic, Payload]));

    // Si c'est un message de capteur, traiter
    if Topic.Contains('sensors') then
      ProcessSensorData(Payload);
  end);
end;

procedure TFormDashboard.ProcessSensorData(const JSONPayload: string);  
var  
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Temp, Hum: Double;
begin
  try
    JSONValue := TJSONObject.ParseJSONValue(JSONPayload);
    try
      if JSONValue is TJSONObject then
      begin
        JSONObject := JSONValue as TJSONObject;

        // Extraire les valeurs
        if JSONObject.TryGetValue<Double>('temperature', Temp) then
          LabelTemperature.Caption := Format('Température: %.1f °C', [Temp]);

        if JSONObject.TryGetValue<Double>('humidity', Hum) then
          LabelHumidity.Caption := Format('Humidité: %.1f %%', [Hum]);
      end;
    finally
      JSONValue.Free;
    end;
  except
    on E: Exception do
      Log('Erreur de parsing JSON: ' + E.Message);
  end;
end;

procedure TFormDashboard.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  if Assigned(FSubscriber) then
    FreeAndNil(FSubscriber);
end;

end.
```

### Configuration Last Will and Testament

```pascal
procedure TMQTTPublisher.ConnectWithLWT(const ClientID: string);  
var  
  LWTTopic: string;
  LWTMessage: string;
begin
  LWTTopic := 'devices/' + ClientID + '/status';
  LWTMessage := 'offline';

  // Configurer le LWT avant de se connecter
  FMQTTClient.ClientID := ClientID;
  FMQTTClient.WillTopic := LWTTopic;
  FMQTTClient.WillMessage := LWTMessage;
  FMQTTClient.WillQoS := 1;
  FMQTTClient.WillRetain := True;

  // Se connecter
  FMQTTClient.Connect;

  // Publier immédiatement le statut "online"
  FMQTTClient.Publish(LWTTopic, 'online', 1, True);

  FConnected := True;
end;
```

## CoAP : Le protocole pour dispositifs contraints

### Qu'est-ce que CoAP ?

CoAP (Constrained Application Protocol) est un protocole web spécialisé pour l'IoT, conçu par l'IETF (Internet Engineering Task Force). Il est optimisé pour les dispositifs à ressources très limitées.

**Caractéristiques principales :**
- **Basé sur UDP** : plus léger que TCP
- **Similaire à HTTP** : modèle request/response
- **RESTful** : utilise GET, POST, PUT, DELETE
- **Compact** : header de 4 octets
- **Découverte de ressources** : mécanisme intégré
- **Observation** : notifications de changements

### Différences avec MQTT

| Caractéristique | MQTT | CoAP |
|-----------------|------|------|
| Architecture | Publish/Subscribe | Client/Server (REST) |
| Transport | TCP | UDP |
| Header minimal | 2 octets | 4 octets |
| QoS | 3 niveaux | 2 niveaux (CON/NON) |
| Broker | Nécessaire | Pas nécessaire |
| Modèle | Messages | Ressources |
| Complexité | Moyenne | Faible |
| Popularité IoT | Très haute | Moyenne |

### Quand utiliser CoAP ?

CoAP est idéal pour :
- Dispositifs très contraints en ressources
- Réseaux avec perte de paquets élevée (UDP résiste mieux)
- Communication directe sans broker
- Intégration avec des systèmes REST existants
- Réseaux 6LoWPAN (IPv6 over Low-Power Wireless)

### Architecture CoAP

CoAP suit le modèle client-serveur REST :

```
[Client CoAP] ----request---> [Serveur CoAP]
              <---response---
```

Les ressources sont identifiées par des URIs :
```
coap://serveur.local/temperature  
coap://192.168.1.50/sensors/room1/humidity  
coap://[2001:db8::1]/actuators/led  
```

### Méthodes CoAP

Comme HTTP, CoAP utilise des méthodes :

- **GET** : lire une ressource
- **POST** : créer une ressource
- **PUT** : mettre à jour une ressource
- **DELETE** : supprimer une ressource

### Types de messages CoAP

**CON (Confirmable)**
- Message qui requiert un accusé de réception (ACK)
- Similaire à QoS 1 de MQTT
- Fiabilité garantie

**NON (Non-confirmable)**
- Message sans accusé de réception
- Similaire à QoS 0 de MQTT
- Plus rapide, moins fiable

**ACK (Acknowledgement)**
- Accusé de réception d'un message CON

**RST (Reset)**
- Indique une erreur ou un rejet

### Observation de ressources

Une fonctionnalité unique de CoAP est l'observation :
- Un client "observe" une ressource
- Le serveur notifie le client à chaque changement
- Plus efficace que le polling

### CoAP avec Delphi

L'utilisation de CoAP en Delphi est moins commune que MQTT, mais possible avec des bibliothèques tierces ou en implémentant le protocole avec Indy UDP.

#### Structure basique d'une requête CoAP

```pascal
type
  TCoapMethod = (cmGet, cmPost, cmPut, cmDelete);
  TCoapType = (ctConfirmable, ctNonConfirmable, ctAcknowledgement, ctReset);

  TCoapMessage = packed record
    Version: Byte;       // 2 bits
    MessageType: Byte;   // 2 bits
    TokenLength: Byte;   // 4 bits
    Code: Byte;          // 8 bits
    MessageID: Word;     // 16 bits
    Token: array[0..7] of Byte;
    // Options et Payload suivent...
  end;
```

#### Exemple simplifié de client CoAP

```pascal
unit SimpleCoapClient;

interface

uses
  System.SysUtils, IdUDPClient;

type
  TSimpleCoapClient = class
  private
    FUDPClient: TIdUDPClient;
    FHost: string;
    FPort: Integer;
  public
    constructor Create(const Host: string; Port: Integer = 5683);
    destructor Destroy; override;

    function Get(const Resource: string): string;
    procedure Put(const Resource: string; const Value: string);
  end;

implementation

constructor TSimpleCoapClient.Create(const Host: string; Port: Integer);  
begin  
  inherited Create;
  FHost := Host;
  FPort := Port;

  FUDPClient := TIdUDPClient.Create(nil);
  FUDPClient.Host := FHost;
  FUDPClient.Port := FPort;
end;

destructor TSimpleCoapClient.Destroy;  
begin  
  FUDPClient.Free;
  inherited;
end;

function TSimpleCoapClient.Get(const Resource: string): string;  
var  
  Request: TBytes;
  Response: TBytes;
begin
  // Construire le message CoAP GET
  // (Simplification - une vraie implémentation serait plus complexe)
  SetLength(Request, 4);
  Request[0] := $40;  // Version 1, Type CON
  Request[1] := $01;  // Code GET
  Request[2] := $00;  // Message ID (MSB)
  Request[3] := $01;  // Message ID (LSB)

  // Ajouter le chemin de ressource comme option
  // ... (code simplifié)

  // Envoyer
  FUDPClient.SendBuffer(Request);

  // Recevoir la réponse
  Response := FUDPClient.ReceiveBytes;

  // Parser la réponse
  // ... (code simplifié)

  Result := 'Response data';
end;

procedure TSimpleCoapClient.Put(const Resource, Value: string);  
var  
  Request: TBytes;
begin
  // Construire et envoyer un message PUT
  // ... (implémentation similaire à Get)
end;

end.
```

**Note** : L'exemple ci-dessus est très simplifié. Une implémentation complète de CoAP nécessite de gérer les options, le format des messages, les tokens, etc. Il est recommandé d'utiliser une bibliothèque existante.

## Comparaison pratique : MQTT vs CoAP

### Scénario 1 : Réseau de capteurs domestiques

**Besoins :**
- 10 capteurs de température/humidité
- 1 application Delphi pour supervision
- 1 application mobile
- Réseau WiFi stable

**Meilleur choix : MQTT**
- Architecture publish/subscribe idéale
- Plusieurs clients peuvent s'abonner facilement
- Broker centralise et distribue les données
- Largement supporté

### Scénario 2 : Dispositif autonome sur batterie

**Besoins :**
- 1 capteur sur batterie
- Communication très rare (1x par heure)
- Réseau instable
- Ressources très limitées

**Meilleur choix : CoAP**
- Pas besoin de maintenir une connexion TCP
- UDP consomme moins
- Pas de broker à gérer
- Message direct au serveur

### Scénario 3 : Système industriel temps réel

**Besoins :**
- Centaines de capteurs/actionneurs
- Données critiques
- Faible latence
- Haute fiabilité

**Meilleur choix : MQTT avec QoS approprié**
- Scalable pour grand nombre de dispositifs
- QoS 1 ou 2 pour garantir la livraison
- Broker professionnel peut gérer la charge
- Écosystème mature

## Sécurité des protocoles IoT

### Sécurité MQTT

#### Authentification par username/password

```pascal
procedure TMQTTPublisher.ConnectSecure(const ClientID, Username, Password: string);  
begin  
  FMQTTClient.ClientID := ClientID;
  FMQTTClient.Username := Username;
  FMQTTClient.Password := Password;
  FMQTTClient.Connect;
end;
```

#### TLS/SSL

Pour chiffrer les communications :
- Port standard TLS : 8883
- Configure le client pour utiliser SSL

```pascal
FMQTTClient.Port := 8883;  
FMQTTClient.UseSSL := True;  
FMQTTClient.SSLVerifyMode := sslvmPeer;  
```

#### Topics et contrôle d'accès

Configurez votre broker pour limiter l'accès aux topics :
```
# Mosquitto ACL
user sensor1  
topic write sensors/room1/#  

user dashboard  
topic read sensors/#  
```

### Sécurité CoAP

CoAP peut utiliser DTLS (Datagram Transport Layer Security) :
- Équivalent de TLS pour UDP
- Chiffrement et authentification
- Port DTLS standard : 5684

## Bonnes pratiques

### Structure des topics MQTT

**Utilisez une hiérarchie claire :**
```
[organisation]/[site]/[zone]/[device]/[metric]

exemple:  
acme/paris/entrepot-a/temp-sensor-01/temperature  
acme/paris/entrepot-a/temp-sensor-01/battery  
```

**Évitez :**
- Topics trop longs
- Caractères spéciaux (sauf _ et -)
- Espaces
- Majuscules (par convention, utiliser minuscules)

### Gestion des connexions

#### Reconnexion automatique

```pascal
procedure TMQTTPublisher.EnsureConnected;  
var  
  Attempts: Integer;
begin
  Attempts := 0;

  while not FConnected and (Attempts < 3) do
  begin
    try
      Connect(FClientID);
      Break;
    except
      Inc(Attempts);
      Sleep(1000 * Attempts);  // Backoff exponentiel
    end;
  end;

  if not FConnected then
    raise Exception.Create('Impossible de se reconnecter après 3 tentatives');
end;
```

#### Keep-alive

Configurez le keep-alive pour maintenir la connexion :
```pascal
FMQTTClient.KeepAlive := 60;  // Secondes
```

### Optimisation des performances

#### Batch publishing

Groupez plusieurs messages pour réduire la latence :
```pascal
procedure PublishSensorBatch(const Sensors: array of TSensorData);  
var  
  Sensor: TSensorData;
begin
  for Sensor in Sensors do
  begin
    FPublisher.Publish(
      Format('sensors/%s/data', [Sensor.ID]),
      Sensor.ToJSON,
      0  // QoS 0 pour performance
    );
  end;
end;
```

#### Compression des payloads

Pour des données volumineuses, compressez :
```pascal
function CompressJSON(const JSONString: string): TBytes;  
var  
  Input, Output: TMemoryStream;
  Compressor: TZCompressionStream;
begin
  Input := TMemoryStream.Create;
  Output := TMemoryStream.Create;
  try
    // Écrire JSON dans le stream
    Input.WriteBuffer(JSONString[1], Length(JSONString) * SizeOf(Char));
    Input.Position := 0;

    // Compresser
    Compressor := TZCompressionStream.Create(Output);
    try
      Compressor.CopyFrom(Input, Input.Size);
    finally
      Compressor.Free;
    end;

    // Résultat
    Output.Position := 0;
    SetLength(Result, Output.Size);
    Output.ReadBuffer(Result[0], Output.Size);
  finally
    Input.Free;
    Output.Free;
  end;
end;
```

### Monitoring et logging

Toujours logger les événements importants :
```pascal
procedure TMQTTPublisher.Publish(const Topic, Payload: string; QoS: Integer);  
begin  
  try
    FMQTTClient.Publish(Topic, Payload, QoS);
    LogEvent('Published', Format('Topic: %s, Size: %d bytes', [Topic, Length(Payload)]));
  except
    on E: Exception do
    begin
      LogError('Publish failed', E.Message);
      raise;
    end;
  end;
end;
```

## Choix du protocole : Guide de décision

```
Avez-vous besoin de communiquer avec plusieurs clients simultanément ?
│
├─ OUI → MQTT
│   └─ Architecture publish/subscribe idéale
│
└─ NON → Dispositif sur batterie avec ressources très limitées ?
    │
    ├─ OUI → CoAP
    │   └─ UDP plus efficace, pas de broker
    │
    └─ NON → MQTT quand même
        └─ Plus d'outils, meilleur support, communauté plus large
```

## Conclusion

Les protocoles IoT spécialisés comme MQTT et CoAP sont essentiels pour créer des systèmes IoT efficaces et scalables.

**MQTT** est le choix par défaut pour la plupart des projets IoT :
- Architecture flexible publish/subscribe
- Excellent support et écosystème
- Adapté à la majorité des cas d'usage
- Parfaitement intégrable avec Delphi

**CoAP** est une alternative pour des cas spécifiques :
- Dispositifs très contraints
- Communication directe sans broker
- Réseaux à pertes élevées
- Intégration avec systèmes REST

**Points clés à retenir :**

1. **MQTT** : protocole star de l'IoT, basé sur publish/subscribe
2. **Broker** : serveur central qui distribue les messages MQTT
3. **Topics** : structure hiérarchique pour organiser les messages
4. **QoS** : trois niveaux de garantie de livraison
5. **CoAP** : alternative légère basée sur UDP pour dispositifs contraints
6. **Sécurité** : toujours utiliser authentification et TLS/SSL en production
7. **Reconnexion** : implémenter une stratégie de reconnexion automatique
8. **Logging** : monitorer toutes les communications

Avec ces protocoles et les outils Delphi, vous pouvez créer des systèmes IoT professionnels, robustes et scalables, capables de gérer des centaines ou milliers de dispositifs connectés.

⏭️ [Gestion de dispositifs connectés](/21-delphi-et-liot/06-gestion-de-dispositifs-connectes.md)
