# 21.5 Protocoles IoT (MQTT, CoAP)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans le monde de l'Internet des Objets (IoT), la communication efficace entre les dispositifs est cruciale. Pour cela, des protocoles spécialisés ont été développés pour répondre aux contraintes spécifiques des objets connectés. Dans cette section, nous allons explorer deux des protocoles les plus populaires - MQTT et CoAP - et voir comment les implémenter dans vos applications Delphi.

## Qu'est-ce qu'un protocole IoT ?

Un protocole IoT est un ensemble de règles qui définit comment les dispositifs connectés communiquent entre eux. Ces protocoles sont conçus pour être:
- **Légers** - Adaptés aux appareils à ressources limitées
- **Efficaces** - Minimisant la consommation d'énergie et de bande passante
- **Fiables** - Assurant la livraison des messages même dans des conditions réseau instables
- **Sécurisés** - Protégeant les données sensibles

## MQTT : Message Queuing Telemetry Transport

### Introduction à MQTT

MQTT est un protocole de messagerie légère basé sur le modèle publication/abonnement (publish/subscribe), spécialement conçu pour les environnements à bande passante limitée et les appareils à faibles ressources.

![Modèle MQTT](https://via.placeholder.com/600x300.png?text=Modèle+MQTT+Publish/Subscribe)

#### Caractéristiques principales de MQTT:
- **Architecture Publish/Subscribe**: Les dispositifs publient des messages sur des "topics" (sujets) et s'abonnent à des topics pour recevoir des messages
- **Broker central**: Un serveur central (broker) qui reçoit tous les messages et les redistribue aux abonnés
- **QoS (Quality of Service)**: Trois niveaux de garantie de livraison des messages
- **Très faible empreinte réseau**: En-têtes minimalistes pour économiser la bande passante
- **Sessions persistantes**: Possibilité de conserver l'état des connexions

### Implémentation de MQTT avec Delphi

Pour utiliser MQTT dans vos applications Delphi, plusieurs options s'offrent à vous:

1. **Bibliothèques tierces**: TMQTTClient, mqtt4delphi
2. **API REST vers un broker MQTT**: Communication indirecte via HTTP
3. **Intégration de bibliothèques C/C++**: Paho MQTT

Nous allons explorer l'approche utilisant TMQTTClient, une bibliothèque populaire et facile à utiliser.

#### Installation de TMQTTClient

TMQTTClient est disponible via GetIt Package Manager dans Delphi 12:

1. Dans Delphi, allez dans `Tools` > `GetIt Package Manager`
2. Recherchez "MQTT"
3. Installez "TMQTTClient" de Daniele Teti

Alternativement, vous pouvez télécharger la bibliothèque depuis GitHub et l'installer manuellement:
```
https://github.com/danieleteti/mqtt4delphi
```

#### Exemple: Application MQTT basique

Créons une application simple qui se connecte à un broker MQTT, publie un message et s'abonne à un topic.

##### Étape 1: Créer un nouveau projet VCL

Créez un nouveau projet VCL et ajoutez les composants suivants sur votre formulaire:
- 2 TButton (btnConnect, btnPublish)
- 1 TMemo (memoLog)
- 2 TEdit (editTopic, editMessage)
- 2 TLabel (pour les zones d'édition)

##### Étape 2: Ajouter la bibliothèque MQTT

Ajoutez la clause `uses` pour TMQTTClient:

```pascal
uses
  // ... autres unités ...
  MQTT.Client;
```

##### Étape 3: Déclarer les variables nécessaires

Dans la section privée de votre formulaire, ajoutez:

```pascal
private
  FMQTTClient: IMQTTClient;
  procedure OnMQTTConnect(Sender: TObject; ReturnCode: integer);
  procedure OnMQTTDisconnect(Sender: TObject);
  procedure OnMQTTMessage(Sender: TObject; topic: string; payload: TBytes);
```

##### Étape 4: Implémentation de la connexion

```pascal
procedure TForm1.btnConnectClick(Sender: TObject);
begin
  memoLog.Lines.Add('Tentative de connexion au broker MQTT...');

  // Création du client MQTT
  FMQTTClient := TMQTTClient.Create('test.mosquitto.org', 1883);

  // Définition d'un identifiant unique pour le client
  FMQTTClient.ClientID := 'DelphiClient_' + IntToStr(Random(1000));

  // Attribution des gestionnaires d'événements
  FMQTTClient.OnConnected := OnMQTTConnect;
  FMQTTClient.OnDisconnected := OnMQTTDisconnect;
  FMQTTClient.OnPublishReceived := OnMQTTMessage;

  // Connexion au broker
  try
    FMQTTClient.Connect;
  except
    on E: Exception do
    begin
      memoLog.Lines.Add('Erreur de connexion: ' + E.Message);
      FMQTTClient := nil;
    end;
  end;
end;

procedure TForm1.OnMQTTConnect(Sender: TObject; ReturnCode: integer);
begin
  // Exécuté dans un thread secondaire
  TThread.Synchronize(nil, procedure
  begin
    if ReturnCode = 0 then
    begin
      memoLog.Lines.Add('Connecté au broker MQTT');
      btnConnect.Enabled := False;
      btnPublish.Enabled := True;

      // S'abonner à un topic
      FMQTTClient.Subscribe('delphi/test/#');
      memoLog.Lines.Add('Abonné au topic: delphi/test/#');
    end
    else
      memoLog.Lines.Add('Erreur de connexion, code: ' + IntToStr(ReturnCode));
  end);
end;
```

##### Étape 5: Implémentation de la publication et réception de messages

```pascal
procedure TForm1.btnPublishClick(Sender: TObject);
var
  Topic, Message: string;
begin
  if not Assigned(FMQTTClient) then
  begin
    ShowMessage('Non connecté au broker MQTT');
    Exit;
  end;

  Topic := editTopic.Text;
  Message := editMessage.Text;

  if (Topic = '') or (Message = '') then
  begin
    ShowMessage('Veuillez saisir un topic et un message');
    Exit;
  end;

  try
    // Publication du message (QoS 0, non retenu)
    FMQTTClient.Publish(Topic, TEncoding.UTF8.GetBytes(Message), TMQTTQosLevel.AtMostOnce, False);
    memoLog.Lines.Add('Message publié sur: ' + Topic);
  except
    on E: Exception do
      memoLog.Lines.Add('Erreur de publication: ' + E.Message);
  end;
end;

procedure TForm1.OnMQTTMessage(Sender: TObject; topic: string; payload: TBytes);
var
  Message: string;
begin
  // Exécuté dans un thread secondaire
  Message := TEncoding.UTF8.GetString(payload);

  TThread.Synchronize(nil, procedure
  begin
    memoLog.Lines.Add('Message reçu:');
    memoLog.Lines.Add('  Topic: ' + topic);
    memoLog.Lines.Add('  Contenu: ' + Message);
  end);
end;
```

##### Étape 6: Gestion de la déconnexion

```pascal
procedure TForm1.OnMQTTDisconnect(Sender: TObject);
begin
  // Exécuté dans un thread secondaire
  TThread.Synchronize(nil, procedure
  begin
    memoLog.Lines.Add('Déconnecté du broker MQTT');
    btnConnect.Enabled := True;
    btnPublish.Enabled := False;
  end);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FMQTTClient) then
  begin
    try
      FMQTTClient.Disconnect;
    except
      // Ignorer les erreurs lors de la fermeture
    end;
    FMQTTClient := nil;
  end;
end;
```

### Niveaux de QoS dans MQTT

MQTT offre trois niveaux de qualité de service (QoS) pour la livraison des messages:

1. **QoS 0 (At most once)**: Le message est envoyé une seule fois, sans confirmation. Il peut être perdu.
2. **QoS 1 (At least once)**: Le message est envoyé et doit être confirmé. Il peut être reçu plusieurs fois.
3. **QoS 2 (Exactly once)**: Le message est envoyé exactement une fois grâce à un mécanisme de négociation en 4 étapes.

Pour spécifier le niveau de QoS dans Delphi:

```pascal
// QoS 0
FMQTTClient.Publish(Topic, Payload, TMQTTQosLevel.AtMostOnce, Retained);

// QoS 1
FMQTTClient.Publish(Topic, Payload, TMQTTQosLevel.AtLeastOnce, Retained);

// QoS 2
FMQTTClient.Publish(Topic, Payload, TMQTTQosLevel.ExactlyOnce, Retained);
```

### Messages retenus (Retained Messages)

MQTT permet de définir des messages "retenus" qui sont stockés par le broker et envoyés immédiatement aux nouveaux abonnés:

```pascal
// Message non retenu
FMQTTClient.Publish(Topic, Payload, QoS, False);

// Message retenu
FMQTTClient.Publish(Topic, Payload, QoS, True);
```

### Sécurité dans MQTT

Pour une communication sécurisée, MQTT supporte TLS/SSL. Voici comment se connecter à un broker sécurisé:

```pascal
// Création d'un client MQTT sécurisé (port 8883 est standard pour MQTT+TLS)
FMQTTClient := TMQTTClient.Create('secure.example.org', 8883, True);

// Pour une authentification avec nom d'utilisateur et mot de passe
FMQTTClient.Username := 'mon_utilisateur';
FMQTTClient.Password := 'mon_mot_de_passe';

// Connexion sécurisée
FMQTTClient.Connect;
```

## CoAP: Constrained Application Protocol

### Introduction à CoAP

CoAP est un protocole de transfert web spécialisé pour les environnements contraints, similaire à HTTP mais optimisé pour l'IoT. Il est basé sur le modèle requête/réponse.

#### Caractéristiques principales de CoAP:
- **Léger**: Conçu pour fonctionner sur des appareils à ressources limitées
- **Basé sur UDP**: Utilise généralement UDP plutôt que TCP
- **Similaire à HTTP**: Utilise des méthodes GET, POST, PUT, DELETE
- **Support de l'observabilité**: Équivalent aux abonnements MQTT
- **Découverte de ressources**: Permet aux clients de découvrir les ressources disponibles

### Implémentation de CoAP avec Delphi

L'implémentation de CoAP en Delphi est moins courante que celle de MQTT, mais quelques bibliothèques sont disponibles:

1. **Bibliothèques tierces**: DelphiCoAP
2. **Implémentation personnalisée**: Utilisant les sockets UDP

#### Exemple: Client CoAP basique

Voici un exemple simplifié d'un client CoAP en Delphi:

```pascal
uses
  System.SysUtils, System.Classes, System.Net.Socket, System.Net.IPv4;

type
  TCoAPMessageType = (Confirmable, NonConfirmable, Acknowledgement, Reset);
  TCoAPMethod = (Get, Post, Put, Delete);

  TCoAPClient = class
  private
    FSocket: TUDPSocket;
    FMessageID: Word;
    function BuildCoAPPacket(MessageType: TCoAPMessageType; Method: TCoAPMethod;
      const URI: string; const Payload: string = ''): TBytes;
    function ExtractResponsePayload(const Response: TBytes): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Get(const URI: string): string;
    function Post(const URI, Payload: string): string;
    function Put(const URI, Payload: string): string;
    function Delete(const URI: string): string;
  end;

constructor TCoAPClient.Create;
begin
  inherited Create;
  FSocket := TUDPSocket.Create;
  FMessageID := 0;
end;

destructor TCoAPClient.Destroy;
begin
  FSocket.Free;
  inherited;
end;

function TCoAPClient.BuildCoAPPacket(MessageType: TCoAPMessageType; Method: TCoAPMethod;
  const URI: string; const Payload: string = ''): TBytes;
var
  Packet: TByteDynArray;
  Header: Byte;
begin
  // Ceci est une implémentation simplifiée pour l'exemple
  // Une implémentation réelle devrait tenir compte des options CoAP, tokens, etc.

  // Paquet d'exemple avec un en-tête de base
  // Version 1, type, méthode, messageID
  Header := (1 shl 6) or (Ord(MessageType) shl 4) or (Ord(Method) and $0F);

  SetLength(Packet, 4 + Length(Payload));
  Packet[0] := Header;
  Packet[1] := 0; // Code
  Packet[2] := (FMessageID shr 8) and $FF; // Message ID (MSB)
  Packet[3] := FMessageID and $FF; // Message ID (LSB)

  Inc(FMessageID);

  if Payload <> '' then
    Move(Payload[1], Packet[4], Length(Payload));

  Result := Packet;
end;

function TCoAPClient.ExtractResponsePayload(const Response: TBytes): string;
var
  PayloadMarkerPos: Integer;
begin
  // Recherche du marqueur de payload (0xFF)
  PayloadMarkerPos := -1;
  for var i := 0 to Length(Response) - 1 do
    if Response[i] = $FF then
    begin
      PayloadMarkerPos := i;
      Break;
    end;

  if PayloadMarkerPos >= 0 then
    Result := TEncoding.UTF8.GetString(Response, PayloadMarkerPos + 1,
                                      Length(Response) - PayloadMarkerPos - 1)
  else
    Result := '';
end;

function TCoAPClient.Get(const URI: string): string;
var
  Packet, Response: TBytes;
  EndPoint: TIPv4Endpoint;
  ReceivedBytes: Integer;
begin
  // Extraction de l'hôte et du port depuis l'URI
  // Implémentation simplifiée - une version réelle devrait analyser l'URI correctement
  EndPoint := TIPv4Endpoint.Create('coap.example.org', 5683);

  Packet := BuildCoAPPacket(TCoAPMessageType.Confirmable, TCoAPMethod.Get, URI);

  FSocket.SendTo(EndPoint, Packet);

  // Attente de la réponse avec timeout
  SetLength(Response, 1024);
  ReceivedBytes := FSocket.ReceiveFrom(EndPoint, Response, 5000); // 5s timeout

  if ReceivedBytes > 0 then
  begin
    SetLength(Response, ReceivedBytes);
    Result := ExtractResponsePayload(Response);
  end
  else
    Result := '';
end;

// Les méthodes Post, Put et Delete seraient similaires
```

> **Note:** Ce code est simplifié à des fins éducatives. Une implémentation complète de CoAP nécessiterait la gestion des tokens, options, timeouts et retransmissions.

## Comparaison entre MQTT et CoAP

| Caractéristique | MQTT | CoAP |
|-----------------|------|------|
| **Modèle** | Publish/Subscribe | Request/Response |
| **Transport** | TCP | UDP (principalement) |
| **Architecture** | Nécessite un broker central | Peer-to-peer |
| **Verbosité** | Très compact | Compact |
| **Fiabilité** | QoS à plusieurs niveaux | Confirmable/Non-confirmable |
| **Découverte** | Limitée (wildcards) | Intégrée (/.well-known/core) |
| **Sécurité** | TLS/SSL | DTLS |
| **Cas d'usage idéal** | Télémétrie, notifications | États des ressources, API REST légère |

## Brokers MQTT populaires

Pour tester vos applications MQTT, vous pouvez utiliser ces brokers publics:

1. **Eclipse Mosquitto**: `test.mosquitto.org` (port 1883 pour non-TLS, 8883 pour TLS)
2. **HiveMQ**: `broker.hivemq.com` (port 1883)
3. **EMQ X**: `broker.emqx.io` (port 1883)

Pour des projets réels, envisagez d'installer votre propre broker:

- **Mosquitto**: Broker léger open-source (https://mosquitto.org/)
- **HiveMQ**: Broker commercial avec version communautaire (https://www.hivemq.com/)
- **EMQ X**: Broker haute performance (https://www.emqx.io/)

## Exemple pratique: Station météo IoT

Voici un exemple plus complet montrant comment créer une application qui communique avec une station météo IoT via MQTT:

```pascal
type
  TWeatherData = record
    Temperature: Double;
    Humidity: Double;
    Pressure: Double;
    Timestamp: TDateTime;
    procedure FromJSON(const JSONString: string);
  end;

procedure TWeatherData.FromJSON(const JSONString: string);
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
  try
    Temperature := JSONObj.GetValue<Double>('temperature');
    Humidity := JSONObj.GetValue<Double>('humidity');
    Pressure := JSONObj.GetValue<Double>('pressure');
    Timestamp := Now;
  finally
    JSONObj.Free;
  end;
end;

procedure TForm1.OnMQTTWeatherMessage(Sender: TObject; topic: string; payload: TBytes);
var
  JSONStr: string;
  WeatherData: TWeatherData;
begin
  JSONStr := TEncoding.UTF8.GetString(payload);

  try
    WeatherData.FromJSON(JSONStr);

    TThread.Synchronize(nil, procedure
    begin
      // Mettre à jour l'interface utilisateur
      lblTemperature.Caption := Format('Température: %.1f°C', [WeatherData.Temperature]);
      lblHumidity.Caption := Format('Humidité: %.1f%%', [WeatherData.Humidity]);
      lblPressure.Caption := Format('Pression: %.0f hPa', [WeatherData.Pressure]);
      lblLastUpdate.Caption := 'Dernière mise à jour: ' +
                               FormatDateTime('dd/mm/yyyy hh:nn:ss', WeatherData.Timestamp);

      // Ajouter au graphique (si vous utilisez TChart)
      AddPointToChart(WeatherData);
    end);
  except
    on E: Exception do
      TThread.Synchronize(nil, procedure
      begin
        memoLog.Lines.Add('Erreur de parsage JSON: ' + E.Message);
      end);
  end;
end;

procedure TForm1.ConnectToWeatherStation;
begin
  FMQTTClient := TMQTTClient.Create('broker.example.com', 1883);
  FMQTTClient.ClientID := 'DelphiWeatherStation_' + IntToStr(Random(1000));
  FMQTTClient.OnConnected := OnMQTTConnect;
  FMQTTClient.OnDisconnected := OnMQTTDisconnect;
  FMQTTClient.OnPublishReceived := OnMQTTWeatherMessage;

  try
    FMQTTClient.Connect;
  except
    on E: Exception do
      memoLog.Lines.Add('Erreur de connexion: ' + E.Message);
  end;
end;

procedure TForm1.OnMQTTConnect(Sender: TObject; ReturnCode: integer);
begin
  TThread.Synchronize(nil, procedure
  begin
    if ReturnCode = 0 then
    begin
      memoLog.Lines.Add('Connecté au broker MQTT');

      // S'abonner au topic de la station météo
      FMQTTClient.Subscribe('weather/station1/#');
      memoLog.Lines.Add('Abonné au topic: weather/station1/#');
    end
    else
      memoLog.Lines.Add('Erreur de connexion, code: ' + IntToStr(ReturnCode));
  end);
end;
```

## Bonnes pratiques pour les protocoles IoT

1. **Hiérarchie des topics MQTT**: Organisez vos topics de manière hiérarchique (ex: `maison/salon/temperature`)
2. **Nommage significatif**: Utilisez des noms descriptifs pour les topics et les ressources
3. **Gestion de la qualité de service**: Choisissez le bon niveau de QoS selon l'importance des données
4. **Sécurité**: Utilisez TLS/SSL et authentification si possible
5. **Format de données standardisé**: Utilisez JSON ou CBOR pour la sérialisation
6. **Last Will and Testament**: En MQTT, configurez un message de "dernière volonté" pour détecter les déconnexions anormales
7. **Gestion des reconnexions**: Implémentez une logique de reconnexion avec backoff exponentiel
8. **Utilisation de threads dédiés**: Isolez la communication IoT de l'interface utilisateur

### Exemple de reconnexion automatique

```pascal
procedure TForm1.AttemptReconnection;
begin
  if FReconnectAttempts >= MAX_RECONNECT_ATTEMPTS then
  begin
    TThread.Synchronize(nil, procedure
    begin
      memoLog.Lines.Add('Nombre maximum de tentatives de reconnexion atteint');
      btnConnect.Enabled := True;
    end);
    Exit;
  end;

  Inc(FReconnectAttempts);

  // Délai exponentiel
  var Delay := 1000 * Power(2, FReconnectAttempts - 1);
  if Delay > 30000 then Delay := 30000; // Max 30 secondes

  TThread.Synchronize(nil, procedure
  begin
    memoLog.Lines.Add(Format('Tentative de reconnexion dans %d ms (essai %d/%d)...',
      [Delay, FReconnectAttempts, MAX_RECONNECT_ATTEMPTS]));
  end);

  // Planifier la reconnexion
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(Delay);
    try
      if Assigned(FMQTTClient) then
        FMQTTClient.Connect;
    except
      on E: Exception do
        TThread.Synchronize(nil, procedure
        begin
          memoLog.Lines.Add('Échec de reconnexion: ' + E.Message);
          AttemptReconnection; // Réessayer
        end);
    end;
  end).Start;
end;
```

## Conclusion

Les protocoles IoT comme MQTT et CoAP offrent des solutions efficaces pour la communication entre dispositifs connectés. MQTT est particulièrement adapté pour la télémétrie et les notifications, tandis que CoAP est idéal pour les interactions de type RESTful.

Avec Delphi, vous pouvez facilement implémenter ces protocoles et créer des applications IoT puissantes qui peuvent s'intégrer à un écosystème plus large d'objets connectés.

Dans la prochaine section, nous explorerons la gestion des dispositifs connectés et la création d'interfaces pour contrôler plusieurs appareils IoT simultanément.

## Ressources complémentaires

- [MQTT Essentials (HiveMQ)](https://www.hivemq.com/mqtt-essentials/)
- [CoAP Technology (CoAP.technology)](https://coap.technology/)
- [TMQTTClient GitHub](https://github.com/danieleteti/mqtt4delphi)
- [Mosquitto MQTT Broker](https://mosquitto.org/)

## Exercices pratiques

1. Créez un dashboard qui surveille la température et l'humidité d'une pièce via MQTT
2. Implémentez un système de contrôle d'éclairage qui utilise MQTT pour allumer/éteindre des LED
3. Développez un client CoAP simple qui interroge un capteur et affiche ses données

⏭️ [Gestion de dispositifs connectés](/21-delphi-et-liot/06-gestion-de-dispositifs-connectes.md)
