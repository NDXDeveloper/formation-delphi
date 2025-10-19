🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.3 Intégration avec Arduino / Raspberry Pi

## Introduction

Arduino et Raspberry Pi sont les deux plateformes les plus populaires dans le monde de l'électronique et de l'IoT. Bien qu'elles soient différentes dans leur conception et leur utilisation, toutes deux peuvent être facilement intégrées avec vos applications Delphi pour créer des systèmes IoT puissants et polyvalents.

## Arduino : Le microcontrôleur accessible

### Qu'est-ce qu'Arduino ?

Arduino est une plateforme électronique open source basée sur un microcontrôleur. C'est essentiellement un petit ordinateur capable d'exécuter un seul programme en boucle, conçu pour interagir avec le monde physique.

**Caractéristiques principales :**
- **Simplicité** : facile à apprendre et à utiliser
- **Prix** : très abordable (à partir de quelques euros)
- **Communauté** : énorme base d'utilisateurs et de projets
- **Bibliothèques** : milliers de bibliothèques disponibles
- **Entrées/Sorties** : nombreuses broches GPIO (General Purpose Input/Output)

### Modèles Arduino populaires

#### Arduino Uno
- **Microcontrôleur** : ATmega328P
- **Tension** : 5V
- **Broches digitales** : 14 (dont 6 PWM)
- **Broches analogiques** : 6
- **Mémoire** : 32 KB Flash, 2 KB SRAM
- **Utilisation** : idéal pour débuter, projets simples

#### Arduino Mega
- **Microcontrôleur** : ATmega2560
- **Broches digitales** : 54 (dont 15 PWM)
- **Broches analogiques** : 16
- **Mémoire** : 256 KB Flash, 8 KB SRAM
- **Utilisation** : projets complexes nécessitant beaucoup de broches

#### Arduino Nano
- **Format** : compact, breadboard-friendly
- **Similaire** : à l'Uno mais plus petit
- **Utilisation** : projets embarqués compacts

#### ESP32/ESP8266
- **WiFi/Bluetooth** : intégrés
- **Performance** : beaucoup plus puissants
- **Prix** : très compétitifs
- **Utilisation** : projets IoT connectés

### Pourquoi combiner Arduino et Delphi ?

Arduino excelle dans :
- Lire des capteurs en temps réel
- Contrôler des actionneurs
- Gérer des signaux à basse latence
- Fonctionner en continu sans OS

Delphi excelle dans :
- Créer des interfaces utilisateur riches
- Gérer des bases de données
- Traiter et analyser des données
- Générer des rapports et graphiques

**Ensemble, ils forment une solution complète** : Arduino collecte les données et contrôle les dispositifs, tandis que Delphi fournit l'interface, le stockage et l'intelligence.

## Intégration Arduino avec Delphi

### Architecture de communication

L'architecture typique est la suivante :

```
[Capteurs] <---> [Arduino] <--USB/Série--> [Application Delphi] <---> [Base de données]
                                                     |
                                                     v
                                              [Interface utilisateur]
```

### Protocole de communication

Pour une communication efficace, définissez un protocole simple :

#### Format de message recommandé

```
COMMANDE:VALEUR\n
```

**Exemples :**
```
TEMP:23.5\n       // Température
HUM:65\n          // Humidité
LED:ON\n          // Allumer LED
MOTOR:150\n       // Vitesse moteur (0-255)
```

**Avantages de ce format :**
- Lisible par l'humain (facilite le débogage)
- Facile à parser
- Extensible
- Compatible avec le moniteur série Arduino

### Code Arduino : Lecture d'un capteur de température

```cpp
// Capteur de température LM35 sur la broche A0

void setup() {
  Serial.begin(9600);  // Initialiser la communication série
  pinMode(A0, INPUT);  // Broche A0 en entrée
}

void loop() {
  // Lire la valeur du capteur (0-1023)
  int sensorValue = analogRead(A0);

  // Convertir en température (LM35: 10mV par degré)
  float temperature = (sensorValue * 5.0 * 100.0) / 1024.0;

  // Envoyer au format COMMANDE:VALEUR
  Serial.print("TEMP:");
  Serial.println(temperature, 1);  // 1 décimale

  delay(1000);  // Attendre 1 seconde
}
```

### Code Delphi : Réception des données

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, CPort;

type
  TFormMain = class(TForm)
    ComPort1: TComPort;
    LabelTemp: TLabel;
    ButtonConnect: TButton;
    ComboBoxPorts: TComboBox;
    PanelDisplay: TPanel;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ComPort1RxChar(Sender: TObject; Count: Integer);
    procedure FormCreate(Sender: TObject);
  private
    FReceivedData: string;
    procedure ProcessArduinoMessage(const Message: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FReceivedData := '';

  // Configurer le port série
  ComPort1.BaudRate := br9600;
  ComPort1.DataBits := dbEight;
  ComPort1.Parity.Bits := prNone;
  ComPort1.StopBits := sbOneStopBit;
end;

procedure TFormMain.ButtonConnectClick(Sender: TObject);
begin
  if not ComPort1.Connected then
  begin
    try
      ComPort1.Port := ComboBoxPorts.Text;
      ComPort1.Open;
      ButtonConnect.Caption := 'Déconnecter';
      ShowMessage('Connecté à ' + ComboBoxPorts.Text);
    except
      on E: Exception do
        ShowMessage('Erreur de connexion : ' + E.Message);
    end;
  end
  else
  begin
    ComPort1.Close;
    ButtonConnect.Caption := 'Connecter';
  end;
end;

procedure TFormMain.ComPort1RxChar(Sender: TObject; Count: Integer);
var
  Str: string;
  P: Integer;
begin
  // Lire les données reçues
  ComPort1.ReadStr(Str, Count);

  // Accumuler dans le buffer
  FReceivedData := FReceivedData + Str;

  // Chercher le séparateur de ligne
  P := Pos(#10, FReceivedData);
  while P > 0 do
  begin
    // Extraire le message complet
    Str := Copy(FReceivedData, 1, P - 1);
    Delete(FReceivedData, 1, P);

    // Nettoyer (enlever retour chariot si présent)
    Str := Trim(Str);

    // Traiter le message
    if Str <> '' then
      ProcessArduinoMessage(Str);

    // Chercher le prochain message
    P := Pos(#10, FReceivedData);
  end;
end;

procedure TFormMain.ProcessArduinoMessage(const Message: string);
var
  Command, Value: string;
  P: Integer;
  TempValue: Double;
begin
  // Parser le message au format COMMANDE:VALEUR
  P := Pos(':', Message);
  if P > 0 then
  begin
    Command := Copy(Message, 1, P - 1);
    Value := Copy(Message, P + 1, Length(Message));

    // Traiter selon la commande
    if Command = 'TEMP' then
    begin
      if TryStrToFloat(Value, TempValue) then
      begin
        LabelTemp.Caption := Format('Température : %.1f °C', [TempValue]);

        // Changer la couleur selon la température
        if TempValue < 20 then
          PanelDisplay.Color := clBlue
        else if TempValue > 25 then
          PanelDisplay.Color := clRed
        else
          PanelDisplay.Color := clLime;
      end;
    end;
  end;
end;

end.
```

### Code Arduino : Contrôle d'une LED

```cpp
// LED sur la broche 13

const int LED_PIN = 13;

void setup() {
  Serial.begin(9600);
  pinMode(LED_PIN, OUTPUT);
  digitalWrite(LED_PIN, LOW);  // LED éteinte au démarrage
}

void loop() {
  // Vérifier si des données sont disponibles
  if (Serial.available() > 0) {
    // Lire la ligne complète
    String command = Serial.readStringUntil('\n');
    command.trim();  // Enlever espaces

    // Parser la commande
    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      // Traiter la commande LED
      if (cmd == "LED") {
        if (value == "ON") {
          digitalWrite(LED_PIN, HIGH);
          Serial.println("STATUS:LED_ON");
        }
        else if (value == "OFF") {
          digitalWrite(LED_PIN, LOW);
          Serial.println("STATUS:LED_OFF");
        }
      }
    }
  }
}
```

### Code Delphi : Envoi de commandes

```pascal
procedure TFormMain.ButtonLedOnClick(Sender: TObject);
begin
  if ComPort1.Connected then
  begin
    ComPort1.WriteStr('LED:ON' + #13#10);
  end
  else
    ShowMessage('Port série non connecté');
end;

procedure TFormMain.ButtonLedOffClick(Sender: TObject);
begin
  if ComPort1.Connected then
  begin
    ComPort1.WriteStr('LED:OFF' + #13#10);
  end;
end;
```

### Projet complet : Station météo Arduino

#### Code Arduino

```cpp
#include <DHT.h>  // Bibliothèque pour DHT22

#define DHTPIN 2
#define DHTTYPE DHT22
#define LDR_PIN A0

DHT dht(DHTPIN, DHTTYPE);

void setup() {
  Serial.begin(9600);
  dht.begin();
  pinMode(LDR_PIN, INPUT);
}

void loop() {
  // Lire température et humidité
  float temp = dht.readTemperature();
  float humidity = dht.readHumidity();

  // Lire luminosité (LDR)
  int lightLevel = analogRead(LDR_PIN);
  int lightPercent = map(lightLevel, 0, 1023, 0, 100);

  // Vérifier la validité des lectures
  if (!isnan(temp) && !isnan(humidity)) {
    // Envoyer les données
    Serial.print("TEMP:");
    Serial.println(temp, 1);

    Serial.print("HUM:");
    Serial.println(humidity, 1);

    Serial.print("LIGHT:");
    Serial.println(lightPercent);
  }

  delay(2000);  // Lecture toutes les 2 secondes
}
```

#### Code Delphi (ajouts)

```pascal
type
  TWeatherData = record
    Temperature: Double;
    Humidity: Double;
    Light: Integer;
    Timestamp: TDateTime;
  end;

var
  CurrentWeather: TWeatherData;

procedure TFormMain.ProcessArduinoMessage(const Message: string);
var
  Command, Value: string;
  P: Integer;
begin
  P := Pos(':', Message);
  if P > 0 then
  begin
    Command := Copy(Message, 1, P - 1);
    Value := Copy(Message, P + 1, Length(Message));

    CurrentWeather.Timestamp := Now;

    if Command = 'TEMP' then
    begin
      TryStrToFloat(Value, CurrentWeather.Temperature);
      LabelTemp.Caption := Format('%.1f °C', [CurrentWeather.Temperature]);
    end
    else if Command = 'HUM' then
    begin
      TryStrToFloat(Value, CurrentWeather.Humidity);
      LabelHum.Caption := Format('%.1f %%', [CurrentWeather.Humidity]);
    end
    else if Command = 'LIGHT' then
    begin
      TryStrToInt(Value, CurrentWeather.Light);
      LabelLight.Caption := Format('%d %%', [CurrentWeather.Light]);
      ProgressBarLight.Position := CurrentWeather.Light;
    end;

    // Sauvegarder dans la base de données
    SaveToDatabase(CurrentWeather);
  end;
end;

procedure TFormMain.SaveToDatabase(const Data: TWeatherData);
begin
  // Utiliser FireDAC pour enregistrer
  FDQuery1.SQL.Text :=
    'INSERT INTO weather_data (timestamp, temperature, humidity, light) ' +
    'VALUES (:ts, :temp, :hum, :light)';

  FDQuery1.ParamByName('ts').AsDateTime := Data.Timestamp;
  FDQuery1.ParamByName('temp').AsFloat := Data.Temperature;
  FDQuery1.ParamByName('hum').AsFloat := Data.Humidity;
  FDQuery1.ParamByName('light').AsInteger := Data.Light;

  FDQuery1.ExecSQL;
end;
```

## Raspberry Pi : Le mini-ordinateur

### Qu'est-ce que Raspberry Pi ?

Le Raspberry Pi est un ordinateur complet de la taille d'une carte de crédit. Contrairement à Arduino, c'est un véritable ordinateur capable d'exécuter un système d'exploitation (généralement Linux).

**Caractéristiques principales :**
- **Système d'exploitation** : Raspberry Pi OS (basé sur Debian), Ubuntu, etc.
- **Processeur** : ARM multi-cœurs (selon le modèle)
- **Mémoire** : de 1 à 8 GB de RAM
- **Connectivité** : WiFi, Ethernet, Bluetooth
- **GPIO** : 40 broches pour interfacer avec l'électronique
- **Périphériques** : USB, HDMI, caméra, etc.

### Modèles Raspberry Pi populaires

#### Raspberry Pi 4 Model B
- **RAM** : 2, 4 ou 8 GB
- **CPU** : Quad-core ARM Cortex-A72
- **USB** : 2x USB 3.0, 2x USB 2.0
- **Réseau** : Gigabit Ethernet, WiFi, Bluetooth
- **Utilisation** : serveur, desktop, projets gourmands

#### Raspberry Pi Zero W
- **Format** : ultra-compact
- **RAM** : 512 MB
- **Connectivité** : WiFi, Bluetooth
- **Utilisation** : projets embarqués, IoT compact

### Différences Arduino vs Raspberry Pi

| Caractéristique | Arduino | Raspberry Pi |
|-----------------|---------|--------------|
| Type | Microcontrôleur | Ordinateur |
| Système d'exploitation | Non | Oui (Linux) |
| Entrées/Sorties | Temps réel | Indirect (via OS) |
| Langage | C/C++ | Tous (Python, C, Java, etc.) |
| Consommation | Très faible | Moyenne |
| Prix | Bas (~10-30€) | Moyen (~40-80€) |
| Complexité | Simple | Plus complexe |
| Boot | Instantané | ~30 secondes |

**En résumé :**
- **Arduino** : parfait pour le contrôle temps réel, capteurs, actionneurs
- **Raspberry Pi** : parfait pour traitement, réseau, interface, intelligence

### Approches d'intégration Raspberry Pi avec Delphi

Il existe plusieurs façons d'intégrer Raspberry Pi avec Delphi :

#### Approche 1 : Raspberry Pi comme serveur

Le Raspberry Pi héberge un serveur (REST API, WebSocket, etc.) et l'application Delphi s'y connecte via le réseau.

**Avantages :**
- Communication réseau standard
- Raspberry Pi peut fonctionner de manière autonome
- Plusieurs clients peuvent se connecter
- Facile à déployer

**Schéma :**
```
[Capteurs] <-> [Raspberry Pi + Serveur Python/Node.js] <-Réseau-> [Application Delphi]
```

#### Approche 2 : Application Delphi sur Raspberry Pi

Delphi peut compiler pour Linux ARM, vous pouvez donc exécuter votre application Delphi directement sur le Raspberry Pi.

**Avantages :**
- Application complète sur un seul dispositif
- Pas besoin de serveur séparé
- Contrôle direct du GPIO via bibliothèques

**Limitations :**
- Nécessite Delphi Pro ou Enterprise
- Interface graphique possible mais limitée en performance

#### Approche 3 : Raspberry Pi comme gateway

Le Raspberry Pi collecte les données de plusieurs Arduino et les transmet à l'application Delphi.

**Schéma :**
```
[Arduino 1] --\
[Arduino 2] ----> [Raspberry Pi] <-Réseau-> [Application Delphi]
[Arduino 3] --/
```

### Intégration via API REST

#### Code Python sur Raspberry Pi (serveur Flask)

```python
from flask import Flask, jsonify, request
import RPi.GPIO as GPIO
import time

app = Flask(__name__)

# Configuration GPIO
LED_PIN = 18
GPIO.setmode(GPIO.BCM)
GPIO.setup(LED_PIN, GPIO.OUT)

# Simulation de capteur
def read_sensor():
    # Ici vous liriez un vrai capteur
    return {
        'temperature': 23.5,
        'humidity': 65,
        'timestamp': time.time()
    }

@app.route('/api/sensor', methods=['GET'])
def get_sensor_data():
    data = read_sensor()
    return jsonify(data)

@app.route('/api/led', methods=['POST'])
def control_led():
    data = request.json
    state = data.get('state', 'OFF')

    if state == 'ON':
        GPIO.output(LED_PIN, GPIO.HIGH)
    else:
        GPIO.output(LED_PIN, GPIO.LOW)

    return jsonify({'status': 'success', 'led_state': state})

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000)
```

#### Code Delphi (client REST)

```pascal
unit RaspberryPiClient;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Client, REST.Types, REST.Json;

type
  TSensorData = record
    Temperature: Double;
    Humidity: Double;
    Timestamp: TDateTime;
  end;

  TRaspberryPiClient = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FBaseURL: string;
  public
    constructor Create(const BaseURL: string);
    destructor Destroy; override;

    function GetSensorData: TSensorData;
    function SetLEDState(State: Boolean): Boolean;
  end;

implementation

constructor TRaspberryPiClient.Create(const BaseURL: string);
begin
  inherited Create;
  FBaseURL := BaseURL;

  FRESTClient := TRESTClient.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTResponse := TRESTResponse.Create(nil);

  FRESTClient.BaseURL := FBaseURL;
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

destructor TRaspberryPiClient.Destroy;
begin
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
  inherited;
end;

function TRaspberryPiClient.GetSensorData: TSensorData;
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  FRESTRequest.Method := rmGET;
  FRESTRequest.Resource := '/api/sensor';

  try
    FRESTRequest.Execute;

    if FRESTResponse.StatusCode = 200 then
    begin
      JSONValue := TJSONObject.ParseJSONValue(FRESTResponse.Content);
      try
        if JSONValue is TJSONObject then
        begin
          JSONObject := JSONValue as TJSONObject;

          Result.Temperature := JSONObject.GetValue<Double>('temperature');
          Result.Humidity := JSONObject.GetValue<Double>('humidity');
          Result.Timestamp := Now; // Ou convertir le timestamp Unix
        end;
      finally
        JSONValue.Free;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('Erreur de communication avec Raspberry Pi: ' + E.Message);
  end;
end;

function TRaspberryPiClient.SetLEDState(State: Boolean): Boolean;
var
  JSONObject: TJSONObject;
  StateStr: string;
begin
  Result := False;

  if State then
    StateStr := 'ON'
  else
    StateStr := 'OFF';

  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('state', StateStr);

    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := '/api/led';
    FRESTRequest.Body.Add(JSONObject.ToString, ContentTypeFromString('application/json'));

    FRESTRequest.Execute;

    Result := FRESTResponse.StatusCode = 200;
  finally
    JSONObject.Free;
  end;
end;

end.
```

#### Utilisation dans le formulaire

```pascal
procedure TFormMain.ButtonReadSensorClick(Sender: TObject);
var
  RpiClient: TRaspberryPiClient;
  SensorData: TSensorData;
begin
  RpiClient := TRaspberryPiClient.Create('http://192.168.1.100:5000');
  try
    SensorData := RpiClient.GetSensorData;

    LabelTemp.Caption := Format('Température : %.1f °C', [SensorData.Temperature]);
    LabelHum.Caption := Format('Humidité : %.1f %%', [SensorData.Humidity]);
  finally
    RpiClient.Free;
  end;
end;

procedure TFormMain.ButtonLedOnClick(Sender: TObject);
var
  RpiClient: TRaspberryPiClient;
begin
  RpiClient := TRaspberryPiClient.Create('http://192.168.1.100:5000');
  try
    if RpiClient.SetLEDState(True) then
      ShowMessage('LED allumée')
    else
      ShowMessage('Erreur lors du contrôle de la LED');
  finally
    RpiClient.Free;
  end;
end;
```

### Intégration via MQTT

MQTT est idéal pour l'IoT car il est léger et supporte le publish/subscribe.

#### Code Python sur Raspberry Pi (MQTT Publisher)

```python
import paho.mqtt.client as mqtt
import time
import json

# Configuration MQTT
MQTT_BROKER = "broker.hivemq.com"  # Ou votre broker local
MQTT_PORT = 1883
MQTT_TOPIC = "home/raspberry/sensors"

client = mqtt.Client("RaspberryPi")

def publish_sensor_data():
    # Simuler lecture de capteur
    data = {
        'temperature': 23.5,
        'humidity': 65,
        'device': 'raspberry_pi_salon',
        'timestamp': time.time()
    }

    # Publier en JSON
    client.publish(MQTT_TOPIC, json.dumps(data))
    print(f"Données publiées : {data}")

# Connexion au broker
client.connect(MQTT_BROKER, MQTT_PORT)
client.loop_start()

# Boucle principale
try:
    while True:
        publish_sensor_data()
        time.sleep(5)  # Publier toutes les 5 secondes
except KeyboardInterrupt:
    print("Arrêt du programme")
    client.loop_stop()
    client.disconnect()
```

#### Code Delphi (MQTT Subscriber)

Vous pouvez utiliser une bibliothèque MQTT pour Delphi comme TMQTTClient ou d'autres disponibles.

```pascal
// Pseudo-code avec une bibliothèque MQTT
procedure TFormMain.FormCreate(Sender: TObject);
begin
  MQTTClient := TMQTTClient.Create;
  MQTTClient.OnMessageReceived := MQTTMessageReceived;

  MQTTClient.Host := 'broker.hivemq.com';
  MQTTClient.Port := 1883;
  MQTTClient.Connect;

  // S'abonner au topic
  MQTTClient.Subscribe('home/raspberry/sensors');
end;

procedure TFormMain.MQTTMessageReceived(const Topic: string; const Payload: string);
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
  Temp, Hum: Double;
begin
  // Parser le JSON reçu
  JSONValue := TJSONObject.ParseJSONValue(Payload);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      Temp := JSONObject.GetValue<Double>('temperature');
      Hum := JSONObject.GetValue<Double>('humidity');

      // Mettre à jour l'interface (depuis le thread principal)
      TThread.Synchronize(nil, procedure
      begin
        LabelTemp.Caption := Format('%.1f °C', [Temp]);
        LabelHum.Caption := Format('%.1f %%', [Hum]);
      end);
    end;
  finally
    JSONValue.Free;
  end;
end;
```

## Projets combinant Arduino et Raspberry Pi

### Architecture Gateway

Le Raspberry Pi agit comme un concentrateur central :

```
[Arduino 1] --USB--> [Raspberry Pi] <--WiFi--> [Application Delphi]
[Arduino 2] --USB--> [          ]
[Arduino 3] --Serial->          ]
```

**Avantages :**
- Plusieurs Arduino gérés par un seul Raspberry Pi
- Raspberry Pi traite et agrège les données
- Communication réseau avec Delphi
- Raspberry Pi peut fonctionner de manière autonome

### Exemple : Système de surveillance multi-zones

**Arduino (dans chaque pièce) :**
- Lit température, humidité, mouvement
- Envoie les données via série au Raspberry Pi

**Raspberry Pi (central) :**
- Collecte les données de tous les Arduino
- Stocke localement (SQLite)
- Expose une API REST
- Publie sur MQTT

**Application Delphi :**
- Interface de supervision
- Graphiques en temps réel
- Alertes
- Historique dans base de données centrale

## Bonnes pratiques

### Gestion de la connexion

Toujours gérer les déconnexions et reconnexions :

```pascal
type
  TConnectionManager = class
  private
    FConnected: Boolean;
    FReconnectTimer: TTimer;
    FLastConnectionAttempt: TDateTime;
    procedure TryReconnect(Sender: TObject);
  public
    procedure Connect;
    procedure Disconnect;
    property Connected: Boolean read FConnected;
  end;

procedure TConnectionManager.TryReconnect(Sender: TObject);
begin
  if not FConnected and (SecondsBetween(Now, FLastConnectionAttempt) > 10) then
  begin
    FLastConnectionAttempt := Now;

    try
      Connect;
    except
      // Logger l'erreur mais continuer à essayer
    end;
  end;
end;
```

### Gestion des erreurs réseau

```pascal
function TRaspberryPiClient.GetSensorDataSafe: TSensorData;
var
  RetryCount: Integer;
  MaxRetries: Integer;
begin
  MaxRetries := 3;
  RetryCount := 0;

  while RetryCount < MaxRetries do
  begin
    try
      Result := GetSensorData;
      Break; // Succès
    except
      on E: Exception do
      begin
        Inc(RetryCount);
        if RetryCount >= MaxRetries then
          raise Exception.Create('Impossible de communiquer après ' +
                                 IntToStr(MaxRetries) + ' tentatives: ' + E.Message);

        Sleep(1000); // Attendre avant de réessayer
      end;
    end;
  end;
end;
```

### Cache des dernières valeurs

En cas de perte de connexion, continuez à afficher les dernières valeurs connues :

```pascal
type
  TSensorCache = class
  private
    FLastTemperature: Double;
    FLastHumidity: Double;
    FLastUpdate: TDateTime;
    FMaxAge: Integer; // Secondes
  public
    procedure Update(Temp, Hum: Double);
    function IsValid: Boolean;
    property LastTemperature: Double read FLastTemperature;
    property LastHumidity: Double read FLastHumidity;
  end;

function TSensorCache.IsValid: Boolean;
begin
  Result := SecondsBetween(Now, FLastUpdate) < FMaxAge;
end;

procedure TFormMain.UpdateDisplay;
begin
  if SensorCache.IsValid then
  begin
    LabelTemp.Caption := Format('%.1f °C', [SensorCache.LastTemperature]);
    LabelStatus.Caption := 'Dernière mise à jour : ' +
                          FormatDateTime('hh:nn:ss', SensorCache.FLastUpdate);
  end
  else
  begin
    LabelStatus.Caption := 'Données obsolètes - vérifier connexion';
    PanelStatus.Color := clYellow;
  end;
end;
```

### Logging complet

Toujours logger les communications pour faciliter le débogage :

```pascal
procedure TFormMain.LogMessage(const Source, MessageType, Content: string);
var
  LogEntry: string;
begin
  LogEntry := Format('[%s] %s - %s: %s',
                     [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
                      Source,
                      MessageType,
                      Content]);

  MemoLog.Lines.Add(LogEntry);

  // Sauvegarder aussi dans un fichier
  TFile.AppendAllText('iot_log.txt', LogEntry + sLineBreak);
end;
```

## Conseils pour le développement

### Démarrer simple

1. **Test basique** : faire clignoter une LED
2. **Lecture simple** : lire un seul capteur
3. **Communication** : envoyer/recevoir des messages texte
4. **Protocol** : implémenter votre protocole de communication
5. **Interface** : créer l'interface Delphi
6. **Base de données** : ajouter la persistance
7. **Fonctionnalités avancées** : graphiques, alertes, etc.

### Prototypage rapide

Utilisez le moniteur série Arduino IDE pour tester la communication avant d'écrire le code Delphi.

### Documentation

Documentez votre protocole de communication :

```
# Protocole de communication Station Météo

## Messages Arduino -> Delphi
TEMP:xx.x       // Température en °C
HUM:xx.x        // Humidité en %
LIGHT:xxx       // Luminosité 0-100%
ERROR:message   // Message d'erreur

## Messages Delphi -> Arduino
LED:ON/OFF      // Contrôle LED
RESET           // Redémarrer les capteurs
CONFIG:param:value  // Configuration
```

### Tests

Testez séparément chaque composant :
1. Arduino seul (avec moniteur série)
2. Communication série (programme simple)
3. Traitement des données (parser)
4. Interface graphique
5. Base de données
6. Intégration complète

## Conclusion

L'intégration d'Arduino et Raspberry Pi avec Delphi ouvre un monde de possibilités pour créer des systèmes IoT sophistiqués. Arduino excelle dans le contrôle temps réel et l'interface avec les capteurs, Raspberry Pi offre la puissance d'un ordinateur complet pour le traitement et la connectivité, et Delphi fournit des outils professionnels pour créer des interfaces riches et gérer les données.

**Points clés à retenir :**

1. **Arduino** : parfait pour le contrôle matériel et la lecture de capteurs
2. **Raspberry Pi** : idéal comme gateway ou serveur IoT
3. **Communication** : définir un protocole clair et simple
4. **Robustesse** : gérer les erreurs et les déconnexions
5. **Architecture** : choisir l'approche adaptée à votre projet
6. **Tests** : valider chaque composant séparément

Dans les prochaines sections, nous approfondirons le contrôle de périphériques externes et l'utilisation de protocoles IoT comme MQTT et CoAP pour créer des solutions encore plus évoluées.

⏭️ [Contrôle de périphériques externes](/21-delphi-et-liot/04-controle-de-peripheriques-externes.md)
