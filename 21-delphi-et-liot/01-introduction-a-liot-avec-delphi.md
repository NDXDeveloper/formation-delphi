# 21.1 Introduction à l'IoT avec Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Qu'est-ce que l'IoT (Internet des Objets) ?

L'Internet des Objets, ou IoT (*Internet of Things* en anglais), désigne l'ensemble des appareils connectés à Internet capables de collecter et d'échanger des données. Ces objets peuvent être des capteurs, des actionneurs, des appareils ménagers intelligents, des montres connectées, ou même des voitures autonomes.

L'IoT transforme notre façon d'interagir avec le monde physique en créant un pont entre les objets réels et le monde numérique.

## Pourquoi utiliser Delphi pour l'IoT ?

Delphi présente plusieurs avantages pour le développement d'applications IoT :

1. **Performances natives** : Les applications Delphi sont compilées en code natif, ce qui assure d'excellentes performances, particulièrement importantes pour traiter les flux de données IoT en temps réel.

2. **Développement multi-plateforme** : Avec FireMonkey (FMX), vous pouvez créer des applications de contrôle IoT pour Windows, macOS, iOS et Android avec un seul code source.

3. **Interface utilisateur riche** : Delphi permet de créer facilement des interfaces utilisateur intuitives pour visualiser et contrôler vos dispositifs IoT.

4. **Bibliothèques de communication** : Delphi dispose de composants natifs et tiers pour gérer différents protocoles de communication utilisés dans l'IoT.

5. **Écosystème mature** : L'écosystème Delphi offre de nombreux composants pour interagir avec les dispositifs IoT.

## Architecture typique d'une solution IoT avec Delphi

Une solution IoT typique construite avec Delphi comprend généralement :

```
[Dispositifs IoT] <---> [Application Delphi] <---> [Serveur/Cloud]
  (Capteurs,            (Interface utilisateur,     (Stockage de données,
   Actionneurs)          Traitement de données)      Analytics)
```

## Protocoles de communication courants en IoT

Delphi peut travailler avec différents protocoles utilisés dans l'écosystème IoT :

- **MQTT** (Message Queuing Telemetry Transport) : Protocole léger basé sur le modèle publication/abonnement, idéal pour les dispositifs à ressources limitées.
- **HTTP/REST** : Pour les communications basées sur le web.
- **WebSockets** : Pour les communications bidirectionnelles en temps réel.
- **Bluetooth LE** : Pour les communications à courte portée et faible consommation d'énergie.
- **Serial/USB** : Pour les communications directes avec des microcontrôleurs comme Arduino.
- **CoAP** (Constrained Application Protocol) : Alternative légère à HTTP pour les dispositifs contraints.

## Bibliothèques et composants Delphi pour l'IoT

Plusieurs bibliothèques sont disponibles pour faciliter le développement IoT avec Delphi :

1. **TMSIntraIoT** : Composants pour l'IoT par TMS Software.
2. **MQTT Client Library** : Bibliothèque pour la communication MQTT.
3. **IndyComponents** : Pour les communications réseau (TCP/IP, UDP).
4. **Bluetooth Components** : Pour la communication Bluetooth.
5. **CrossTalk** : Pour la communication avec les appareils série/USB.

## Premier projet : Connexion à un broker MQTT

Voici un exemple simple de connexion à un broker MQTT public avec Delphi :

```delphi
uses
  System.SysUtils, System.Classes, MQTT;

procedure TForm1.ConnectToMQTT;
var
  MQTTClient: TMQTTClient;
begin
  MQTTClient := TMQTTClient.Create('test.mosquitto.org', 1883);
  try
    MQTTClient.Connect('DelphiIoTClient');
    if MQTTClient.IsConnected then
    begin
      ShowMessage('Connecté au broker MQTT !');

      // S'abonner à un topic
      MQTTClient.Subscribe('delphi/iot/test');

      // Publier un message
      MQTTClient.Publish('delphi/iot/test', 'Hello IoT World from Delphi!');
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

> **Note** : Cet exemple utilise une bibliothèque MQTT générique. En pratique, vous devrez installer et utiliser une bibliothèque MQTT spécifique comme [MQTT Client Library for Delphi](https://github.com/pjde/delphi-mqtt).

## Visualisation des données IoT

Une des forces de Delphi est sa capacité à créer des interfaces utilisateur riches pour visualiser des données IoT en temps réel :

```delphi
procedure TForm1.OnMQTTMessageReceived(Sender: TObject; topic, payload: string);
var
  temperature: Double;
begin
  if topic = 'delphi/iot/temperature' then
  begin
    if TryStrToFloat(payload, temperature) then
    begin
      // Mise à jour de l'interface utilisateur
      GaugeTemperature.Value := temperature;
      ChartTemperature.Series[0].Add(temperature, '', clRed);

      // Alerte si température trop élevée
      if temperature > 30 then
        ShowNotification('Alerte température', 'Température élevée détectée!');
    end;
  end;
end;
```

## Communication avec Arduino

Delphi peut facilement communiquer avec des microcontrôleurs comme Arduino via une connexion série :

```delphi
uses
  System.SysUtils, Vcl.Dialogs, System.Win.ComObj;

procedure TForm1.ConnectToArduino;
var
  SerialPort: TComPort;
begin
  SerialPort := TComPort.Create(nil);
  try
    SerialPort.Port := 'COM3'; // Le port peut varier
    SerialPort.BaudRate := br9600;
    SerialPort.Open;

    if SerialPort.Connected then
    begin
      ShowMessage('Connecté à Arduino !');

      // Envoyer une commande
      SerialPort.WriteStr('LED:ON' + #13#10);
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

> **Note** : Cet exemple nécessite un composant comme TComPort de [ComPort Library](https://sourceforge.net/projects/comport/) ou similaire.

## Exemple concret : Station météo IoT

Voici un scénario typique : une station météo IoT basée sur Arduino qui envoie des données à une application Delphi pour visualisation.

1. **Côté Arduino** : Le microcontrôleur collecte les données des capteurs (température, humidité, pression) et les envoie via série ou MQTT.

2. **Côté Delphi** : L'application reçoit les données, les traite et les affiche :

```delphi
procedure TWeatherStationForm.ProcessWeatherData(const JSONData: string);
var
  WeatherData: TJSONObject;
  Temperature, Humidity, Pressure: Double;
begin
  // Analyser les données JSON
  WeatherData := TJSONObject.ParseJSONValue(JSONData) as TJSONObject;
  try
    Temperature := WeatherData.GetValue<Double>('temperature');
    Humidity := WeatherData.GetValue<Double>('humidity');
    Pressure := WeatherData.GetValue<Double>('pressure');

    // Mise à jour de l'interface
    lblTemperature.Text := Format('%.1f°C', [Temperature]);
    lblHumidity.Text := Format('%.1f%%', [Humidity]);
    lblPressure.Text := Format('%.0f hPa', [Pressure]);

    // Stockage dans la base de données
    StoreWeatherData(Temperature, Humidity, Pressure);

    // Mise à jour des graphiques
    UpdateCharts(Temperature, Humidity, Pressure);
  finally
    WeatherData.Free;
  end;
end;
```

## Bonnes pratiques pour le développement IoT avec Delphi

1. **Utiliser une architecture asynchrone** : Évitez de bloquer l'interface utilisateur pendant les communications avec les dispositifs IoT.

2. **Gérer la perte de connexion** : Prévoyez toujours la possibilité de perte de connexion avec les dispositifs IoT.

3. **Sécuriser les communications** : Utilisez des protocoles sécurisés comme MQTTS, HTTPS pour les communications IoT.

4. **Économiser les ressources** : Les dispositifs IoT ont souvent des ressources limitées, optimisez vos communications.

5. **Journalisation** : Mettez en place un système de journalisation pour suivre les activités de vos dispositifs IoT.

## Ressources pour aller plus loin

- [Site officiel de Delphi](https://www.embarcadero.com/products/delphi)
- [MQTT Client Library for Delphi](https://github.com/pjde/delphi-mqtt)
- [TMS Software IoT Components](https://www.tmssoftware.com)
- [Exemples de projets IoT avec Delphi](https://github.com/topics/delphi-iot)

## Conclusion

Delphi offre un environnement puissant et flexible pour développer des applications IoT. Grâce à sa compilation native, son développement multi-plateforme et son écosystème riche, vous pouvez créer rapidement des solutions IoT professionnelles allant de simples interfaces de contrôle à des systèmes complexes de surveillance en temps réel.

Dans les prochaines sections, nous explorerons plus en détail les protocoles spécifiques et les intégrations matérielles pour l'IoT avec Delphi.

⏭️ [Communication Bluetooth / série](21-delphi-et-liot/02-communication-bluetooth-serie.md)
