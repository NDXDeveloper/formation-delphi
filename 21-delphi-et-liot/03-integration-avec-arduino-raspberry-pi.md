# 21.3 Intégration avec Arduino / Raspberry Pi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'une des forces de Delphi dans le domaine de l'IoT est sa capacité à s'intégrer avec des plateformes matérielles populaires comme Arduino et Raspberry Pi. Cette intégration vous permet de créer des solutions IoT complètes en combinant la puissance de développement d'interface de Delphi avec la flexibilité matérielle de ces plateformes.

Dans cette section, nous allons explorer comment connecter et communiquer avec ces appareils depuis vos applications Delphi, en nous concentrant sur des exemples pratiques et accessibles.

## Intégration avec Arduino

### Qu'est-ce qu'Arduino ?

Arduino est une plateforme open-source de prototypage électronique basée sur du matériel et des logiciels flexibles et faciles à utiliser. Elle est idéale pour créer des objets interactifs ou pour s'initier à la programmation et à l'électronique.

### Matériel nécessaire

Pour suivre cette section, vous aurez besoin de :
- Une carte Arduino (Uno, Nano, Mega, etc.)
- Un câble USB
- Quelques composants de base (LEDs, résistances, capteurs)
- Un ordinateur avec Delphi installé

### Méthodes de communication avec Arduino

Il existe plusieurs façons de connecter Delphi à Arduino :

1. **Communication série USB** : La méthode la plus simple et la plus courante
2. **Bluetooth** : Via des modules comme HC-05 ou HC-06
3. **WiFi** : Avec des cartes comme ESP8266 ou Arduino MKR WiFi
4. **Ethernet** : Avec un shield Ethernet pour Arduino

### Exemple 1 : Communication série simple

#### Côté Arduino

Commençons par un sketch Arduino simple qui permet de contrôler une LED et de lire la valeur d'un capteur :

```cpp
const int ledPin = 13;      // LED sur la broche 13
const int sensorPin = A0;   // Capteur sur la broche analogique A0

void setup() {
  Serial.begin(9600);       // Initialiser la communication série à 9600 bauds
  pinMode(ledPin, OUTPUT);  // Configurer la broche LED comme sortie
  digitalWrite(ledPin, LOW); // LED initialement éteinte
}

void loop() {
  // Vérifier si des données sont disponibles
  if (Serial.available() > 0) {
    // Lire la commande
    char command = Serial.read();

    // Traiter la commande
    if (command == 'A') {
      digitalWrite(ledPin, HIGH);  // Allumer la LED
      Serial.println("LED ON");
    }
    else if (command == 'E') {
      digitalWrite(ledPin, LOW);   // Éteindre la LED
      Serial.println("LED OFF");
    }
    else if (command == 'R') {
      // Lire et envoyer la valeur du capteur
      int sensorValue = analogRead(sensorPin);
      Serial.print("SENSOR:");
      Serial.println(sensorValue);
    }
  }

  delay(50);  // Petit délai pour stabiliser la communication
}
```

#### Côté Delphi

Maintenant, créons une application Delphi pour communiquer avec notre Arduino :

```delphi
unit MainUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms,
  Vcl.ExtCtrls, Vcl.ComCtrls, ComPort;

type
  TMainForm = class(TForm)
    ComboBoxPorts: TComboBox;
    ButtonConnect: TButton;
    ButtonLedOn: TButton;
    ButtonLedOff: TButton;
    ButtonReadSensor: TButton;
    MemoLog: TMemo;
    LabelSensorValue: TLabel;
    TrackBarSensor: TTrackBar;
    TimerRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonLedOnClick(Sender: TObject);
    procedure ButtonLedOffClick(Sender: TObject);
    procedure ButtonReadSensorClick(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
  private
    FComPort: TComPort;
    FBuffer: string;
    procedure ComPortRxChar(Sender: TObject; Count: Integer);
    procedure RefreshPortList;
    procedure ProcessArduinoResponse(const Response: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Créer le composant ComPort
  FComPort := TComPort.Create(Self);
  FComPort.OnRxChar := ComPortRxChar;
  FComPort.BaudRate := br9600;
  FBuffer := '';

  // Remplir la liste des ports COM
  RefreshPortList;

  // Désactiver les boutons initialement
  ButtonLedOn.Enabled := False;
  ButtonLedOff.Enabled := False;
  ButtonReadSensor.Enabled := False;

  // Configurer la barre de capteur
  TrackBarSensor.Min := 0;
  TrackBarSensor.Max := 1023;  // Valeur max pour un capteur analogique Arduino
  TrackBarSensor.Position := 0;
  TrackBarSensor.Enabled := False;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if FComPort.Connected then
    FComPort.Close;
  FComPort.Free;
end;

procedure TMainForm.RefreshPortList;
var
  I: Integer;
  Registry: TRegistry;
  KeyNames: TStringList;
begin
  ComboBoxPorts.Items.Clear;

  Registry := TRegistry.Create;
  KeyNames := TStringList.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;

    // Rechercher les ports COM disponibles dans le registre
    if Registry.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    begin
      Registry.GetValueNames(KeyNames);

      for I := 0 to KeyNames.Count - 1 do
        ComboBoxPorts.Items.Add(Registry.ReadString(KeyNames[I]));

      Registry.CloseKey;
    end;

    // Sélectionner le premier port s'il y en a
    if ComboBoxPorts.Items.Count > 0 then
      ComboBoxPorts.ItemIndex := 0;
  finally
    Registry.Free;
    KeyNames.Free;
  end;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  // Si déjà connecté, déconnecter
  if FComPort.Connected then
  begin
    FComPort.Close;
    ButtonConnect.Caption := 'Connecter';
    ComboBoxPorts.Enabled := True;
    ButtonLedOn.Enabled := False;
    ButtonLedOff.Enabled := False;
    ButtonReadSensor.Enabled := False;
    TrackBarSensor.Enabled := False;
    MemoLog.Lines.Add('Déconnecté');
    Exit;
  end;

  // Si pas connecté, établir la connexion
  if ComboBoxPorts.ItemIndex >= 0 then
  begin
    FComPort.Port := ComboBoxPorts.Items[ComboBoxPorts.ItemIndex];
    try
      FComPort.Open;
      ButtonConnect.Caption := 'Déconnecter';
      ComboBoxPorts.Enabled := False;
      ButtonLedOn.Enabled := True;
      ButtonLedOff.Enabled := True;
      ButtonReadSensor.Enabled := True;
      TrackBarSensor.Enabled := True;
      MemoLog.Lines.Add('Connecté à ' + FComPort.Port);
    except
      on E: Exception do
        MemoLog.Lines.Add('Erreur de connexion: ' + E.Message);
    end;
  end;
end;

procedure TMainForm.ComPortRxChar(Sender: TObject; Count: Integer);
var
  Temp: string;
  EndLinePos: Integer;
begin
  // Lire les données reçues
  FComPort.ReadStr(Temp, Count);
  FBuffer := FBuffer + Temp;

  // Traiter les lignes complètes
  EndLinePos := Pos(#13#10, FBuffer);
  while EndLinePos > 0 do
  begin
    // Extraire la ligne
    Temp := Copy(FBuffer, 1, EndLinePos - 1);
    // Supprimer la ligne traitée du buffer
    FBuffer := Copy(FBuffer, EndLinePos + 2, Length(FBuffer));

    // Traiter la réponse
    ProcessArduinoResponse(Temp);

    // Chercher la prochaine ligne
    EndLinePos := Pos(#13#10, FBuffer);
  end;
end;

procedure TMainForm.ProcessArduinoResponse(const Response: string);
var
  SensorValue: Integer;
begin
  // Afficher la réponse brute
  MemoLog.Lines.Add('Reçu: ' + Response);

  // Traiter les différentes réponses possibles
  if Response = 'LED ON' then
  begin
    // La LED a été allumée
  end
  else if Response = 'LED OFF' then
  begin
    // La LED a été éteinte
  end
  else if Pos('SENSOR:', Response) = 1 then
  begin
    // Réponse du capteur
    if TryStrToInt(Copy(Response, 8, Length(Response)), SensorValue) then
    begin
      TrackBarSensor.Position := SensorValue;
      LabelSensorValue.Caption := 'Valeur du capteur: ' + IntToStr(SensorValue);
    end;
  end;
end;

procedure TMainForm.ButtonLedOnClick(Sender: TObject);
begin
  if FComPort.Connected then
  begin
    FComPort.WriteStr('A');
    MemoLog.Lines.Add('Envoyé: Allumer LED');
  end;
end;

procedure TMainForm.ButtonLedOffClick(Sender: TObject);
begin
  if FComPort.Connected then
  begin
    FComPort.WriteStr('E');
    MemoLog.Lines.Add('Envoyé: Éteindre LED');
  end;
end;

procedure TMainForm.ButtonReadSensorClick(Sender: TObject);
begin
  if FComPort.Connected then
  begin
    FComPort.WriteStr('R');
    MemoLog.Lines.Add('Envoyé: Lire capteur');
  end;
end;

procedure TMainForm.TimerRefreshTimer(Sender: TObject);
begin
  // Lecture automatique du capteur à intervalle régulier
  if FComPort.Connected and ButtonReadSensor.Enabled then
    ButtonReadSensorClick(nil);
end;
```

### Exemple 2 : Station météo avec Arduino

Construisons un exemple plus complet : une station météo utilisant un capteur DHT11 (température et humidité).

#### Côté Arduino

```cpp
#include <DHT.h>

#define DHTPIN 2      // Broche de connexion du DHT
#define DHTTYPE DHT11 // Type de capteur DHT (DHT11 ou DHT22)

DHT dht(DHTPIN, DHTTYPE);

void setup() {
  Serial.begin(9600);
  dht.begin();
}

void loop() {
  if (Serial.available() > 0) {
    char command = Serial.read();

    if (command == 'T') {
      // Lire la température
      float temp = dht.readTemperature();
      if (!isnan(temp)) {
        Serial.print("TEMP:");
        Serial.println(temp);
      }
      else {
        Serial.println("ERROR:DHT_TEMP");
      }
    }
    else if (command == 'H') {
      // Lire l'humidité
      float hum = dht.readHumidity();
      if (!isnan(hum)) {
        Serial.print("HUM:");
        Serial.println(hum);
      }
      else {
        Serial.println("ERROR:DHT_HUM");
      }
    }
    else if (command == 'A') {
      // Lire les deux valeurs
      float temp = dht.readTemperature();
      float hum = dht.readHumidity();

      if (!isnan(temp) && !isnan(hum)) {
        Serial.print("DATA:");
        Serial.print(temp);
        Serial.print(";");
        Serial.println(hum);
      }
      else {
        Serial.println("ERROR:DHT_READ");
      }
    }
  }

  delay(100);
}
```

#### Côté Delphi

Pour cet exemple, nous allons créer une interface plus élaborée avec des graphiques pour afficher les données de température et d'humidité :

```delphi
// Ajouter aux uses les unités nécessaires pour les graphiques
uses
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
  VCLTee.Chart;

// Dans la classe TMainForm, ajouter les composants suivants :
private
  FTemperatureSeries: TLineSeries;
  FHumiditySeries: TLineSeries;
  FTimestamps: TStringList;
  procedure UpdateCharts(Temperature, Humidity: Double);

// Implémentation de UpdateCharts
procedure TMainForm.UpdateCharts(Temperature, Humidity: Double);
var
  Timestamp: string;
begin
  // Limiter le nombre de points affichés (garder les 30 derniers)
  while FTemperatureSeries.Count >= 30 do
  begin
    FTemperatureSeries.Delete(0);
    FHumiditySeries.Delete(0);
    FTimestamps.Delete(0);
  end;

  // Ajouter un nouveau point
  Timestamp := FormatDateTime('hh:nn:ss', Now);
  FTimestamps.Add(Timestamp);

  FTemperatureSeries.AddY(Temperature, Timestamp);
  FHumiditySeries.AddY(Humidity, Timestamp);
end;

// Modification de ProcessArduinoResponse
procedure TMainForm.ProcessArduinoResponse(const Response: string);
var
  Prefix: string;
  DataStr: string;
  Parts: TArray<string>;
  Temperature, Humidity: Double;
begin
  MemoLog.Lines.Add('Reçu: ' + Response);

  // Trouver le préfixe et les données
  if Pos(':', Response) > 0 then
  begin
    Prefix := Copy(Response, 1, Pos(':', Response) - 1);
    DataStr := Copy(Response, Pos(':', Response) + 1, Length(Response));

    if Prefix = 'TEMP' then
    begin
      if TryStrToFloat(DataStr, Temperature) then
      begin
        LabelTemperature.Caption := Format('Température: %.1f°C', [Temperature]);
      end;
    end
    else if Prefix = 'HUM' then
    begin
      if TryStrToFloat(DataStr, Humidity) then
      begin
        LabelHumidity.Caption := Format('Humidité: %.1f%%', [Humidity]);
      end;
    end
    else if Prefix = 'DATA' then
    begin
      // Format DATA:temp;hum
      Parts := DataStr.Split([';']);
      if Length(Parts) = 2 then
      begin
        if TryStrToFloat(Parts[0], Temperature) and TryStrToFloat(Parts[1], Humidity) then
        begin
          LabelTemperature.Caption := Format('Température: %.1f°C', [Temperature]);
          LabelHumidity.Caption := Format('Humidité: %.1f%%', [Humidity]);

          // Mettre à jour les graphiques
          UpdateCharts(Temperature, Humidity);
        end;
      end;
    end
    else if Prefix = 'ERROR' then
    begin
      LabelStatus.Caption := 'Erreur: ' + DataStr;
    end;
  end;
end;

// Initialisation des composants dans FormCreate
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // ... code existant ...

  // Initialiser les graphiques
  FTemperatureSeries := TLineSeries.Create(Self);
  FTemperatureSeries.Title := 'Température (°C)';
  FTemperatureSeries.Color := clRed;
  ChartData.AddSeries(FTemperatureSeries);

  FHumiditySeries := TLineSeries.Create(Self);
  FHumiditySeries.Title := 'Humidité (%)';
  FHumiditySeries.Color := clBlue;
  ChartData.AddSeries(FHumiditySeries);

  FTimestamps := TStringList.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // ... code existant ...

  FTimestamps.Free;
end;
```

## Intégration avec Raspberry Pi

### Qu'est-ce que Raspberry Pi ?

Raspberry Pi est un ordinateur monocarte à processeur ARM développé pour promouvoir l'enseignement de l'informatique. Contrairement à l'Arduino, le Raspberry Pi est un ordinateur complet capable d'exécuter un système d'exploitation comme Linux.

### Approches d'intégration Delphi-Raspberry Pi

Il existe plusieurs façons d'intégrer Delphi et Raspberry Pi :

1. **Communication réseau** : La méthode la plus courante
2. **Web API/REST** : Créer une API sur le Raspberry Pi et y accéder depuis Delphi
3. **Communication série** (via USB ou GPIO) : Similaire à Arduino mais moins courante

### Exemple : Communication réseau avec Raspberry Pi

#### Côté Raspberry Pi

Créons un script Python simple pour exposer les données du capteur sur le réseau :

```python
import socket
import json
import time
import Adafruit_DHT  # Bibliothèque pour le capteur DHT

# Configuration du capteur
sensor = Adafruit_DHT.DHT11
pin = 4

# Configuration du serveur
HOST = '0.0.0.0'  # Écoute sur toutes les interfaces
PORT = 8080      # Port à utiliser

# Créer un socket TCP/IP
sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.bind((HOST, PORT))
sock.listen(1)

print(f"Serveur démarré sur {HOST}:{PORT}")

while True:
    # Attendre une connexion
    connection, client_address = sock.accept()
    print(f"Connexion de {client_address}")

    try:
        # Recevoir les données
        data = connection.recv(16)
        command = data.decode('utf-8').strip()

        if command == "READ":
            # Lire les données du capteur
            humidity, temperature = Adafruit_DHT.read_retry(sensor, pin)

            if humidity is not None and temperature is not None:
                response = json.dumps({
                    "temperature": round(temperature, 1),
                    "humidity": round(humidity, 1),
                    "timestamp": time.time()
                })
            else:
                response = json.dumps({
                    "error": "Échec de lecture du capteur"
                })

            connection.sendall(response.encode('utf-8'))

    finally:
        # Fermer la connexion
        connection.close()
```

#### Côté Delphi

```delphi
unit MainUnit;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.DateUtils,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  IdTCPClient, IdGlobal;

type
  TMainForm = class(TForm)
    EditIP: TEdit;
    EditPort: TEdit;
    ButtonConnect: TButton;
    ButtonRead: TButton;
    MemoLog: TMemo;
    Timer1: TTimer;
    LabelTemperature: TLabel;
    LabelHumidity: TLabel;
    CheckBoxAutoRefresh: TCheckBox;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonReadClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CheckBoxAutoRefreshClick(Sender: TObject);
  private
    FTCPClient: TIdTCPClient;
    function ReadSensorData: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  FTCPClient := TIdTCPClient.Create(nil);

  EditIP.Text := '192.168.1.100';  // Remplacer par l'IP de votre Raspberry Pi
  EditPort.Text := '8080';

  Timer1.Enabled := False;
  Timer1.Interval := 5000;  // Refresh toutes les 5 secondes

  ButtonRead.Enabled := False;
  CheckBoxAutoRefresh.Enabled := False;
end;

destructor TMainForm.Destroy;
begin
  FTCPClient.Free;
  inherited;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  if FTCPClient.Connected then
  begin
    FTCPClient.Disconnect;
    ButtonConnect.Caption := 'Connecter';
    ButtonRead.Enabled := False;
    CheckBoxAutoRefresh.Enabled := False;
    Timer1.Enabled := False;
    MemoLog.Lines.Add('Déconnecté');
  end
  else
  begin
    try
      FTCPClient.Host := EditIP.Text;
      FTCPClient.Port := StrToInt(EditPort.Text);

      // Tester la connexion
      FTCPClient.Connect;
      FTCPClient.Disconnect;

      ButtonConnect.Caption := 'Déconnecter';
      ButtonRead.Enabled := True;
      CheckBoxAutoRefresh.Enabled := True;

      MemoLog.Lines.Add('Connecté avec succès à ' + FTCPClient.Host + ':' + IntToStr(FTCPClient.Port));
    except
      on E: Exception do
        MemoLog.Lines.Add('Erreur de connexion: ' + E.Message);
    end;
  end;
end;

function TMainForm.ReadSensorData: Boolean;
var
  Response: string;
  JSONObj: TJSONObject;
  Temperature, Humidity: Double;
  Timestamp: TDateTime;
begin
  Result := False;

  try
    // Se connecter et envoyer la commande
    FTCPClient.Connect;
    try
      FTCPClient.IOHandler.WriteLn('READ');

      // Lire la réponse
      Response := FTCPClient.IOHandler.ReadLn;
      MemoLog.Lines.Add('Réponse: ' + Response);

      // Analyser le JSON
      JSONObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
      try
        if JSONObj.FindValue('error') <> nil then
        begin
          MemoLog.Lines.Add('Erreur: ' + JSONObj.GetValue<string>('error'));
        end
        else
        begin
          Temperature := JSONObj.GetValue<Double>('temperature');
          Humidity := JSONObj.GetValue<Double>('humidity');
          Timestamp := UnixToDateTime(JSONObj.GetValue<Int64>('timestamp'));

          LabelTemperature.Caption := Format('Température: %.1f°C', [Temperature]);
          LabelHumidity.Caption := Format('Humidité: %.1f%%', [Humidity]);
          MemoLog.Lines.Add(Format('Données lues à %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Timestamp)]));

          Result := True;
        end;
      finally
        JSONObj.Free;
      end;
    finally
      FTCPClient.Disconnect;
    end;
  except
    on E: Exception do
    begin
      MemoLog.Lines.Add('Erreur: ' + E.Message);
      if FTCPClient.Connected then
        FTCPClient.Disconnect;
    end;
  end;
end;

procedure TMainForm.ButtonReadClick(Sender: TObject);
begin
  ReadSensorData;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  ReadSensorData;
end;

procedure TMainForm.CheckBoxAutoRefreshClick(Sender: TObject);
begin
  Timer1.Enabled := CheckBoxAutoRefresh.Checked;
end;
```

### Exemple 2 : REST API avec Raspberry Pi

#### Côté Raspberry Pi

Utilisons Flask pour créer une API REST simple :

```python
from flask import Flask, jsonify
import Adafruit_DHT

app = Flask(__name__)

# Configuration du capteur
sensor = Adafruit_DHT.DHT11
pin = 4

@app.route('/api/sensor', methods=['GET'])
def get_sensor_data():
    humidity, temperature = Adafruit_DHT.read_retry(sensor, pin)

    if humidity is not None and temperature is not None:
        return jsonify({
            'temperature': round(temperature, 1),
            'humidity': round(humidity, 1),
            'status': 'success'
        })
    else:
        return jsonify({
            'status': 'error',
            'message': 'Failed to read sensor'
        }), 500

@app.route('/api/status', methods=['GET'])
def get_status():
    return jsonify({
        'status': 'online'
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=5000, debug=True)
```

Démarrer le serveur sur le Raspberry Pi :
```bash
python sensor_api.py
```

#### Côté Delphi

Utilisons un client REST pour accéder à l'API :

```delphi
uses
  REST.Client, REST.Types, REST.Response.Adapter, System.JSON;

// Dans la classe TMainForm, ajouter les composants REST
private
  FRESTClient: TRESTClient;
  FRESTRequest: TRESTRequest;
  FRESTResponse: TRESTResponse;

// Dans le constructeur
constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;

  // Initialiser les composants REST
  FRESTClient := TRESTClient.Create(Self);
  FRESTResponse := TRESTResponse.Create(Self);
  FRESTRequest := TRESTRequest.Create(Self);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;

  EditIP.Text := '192.168.1.100';  // Remplacer par l'IP de votre Raspberry Pi
  EditPort.Text := '5000';         // Port du serveur Flask

  // ... reste du code ...
end;

// Méthode pour vérifier l'état du serveur
function TMainForm.CheckServerStatus: Boolean;
begin
  Result := False;

  try
    // Configurer la requête
    FRESTClient.BaseURL := Format('http://%s:%s', [EditIP.Text, EditPort.Text]);
    FRESTRequest.Resource := 'api/status';
    FRESTRequest.Method := rmGET;

    // Exécuter la requête
    FRESTRequest.Execute;

    if FRESTResponse.StatusCode = 200 then
    begin
      MemoLog.Lines.Add('Serveur en ligne');
      Result := True;
    end
    else
    begin
      MemoLog.Lines.Add('Erreur serveur: ' + FRESTResponse.StatusText);
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur de connexion: ' + E.Message);
  end;
end;

// Méthode pour lire les données du capteur
function TMainForm.ReadSensorData: Boolean;
var
  JSONObj: TJSONObject;
  Temperature, Humidity: Double;
  Status: string;
begin
  Result := False;

  try
    // Configurer la requête
    FRESTClient.BaseURL := Format('http://%s:%s', [EditIP.Text, EditPort.Text]);
    FRESTRequest.Resource := 'api/sensor';
    FRESTRequest.Method := rmGET;

    // Exécuter la requête
    FRESTRequest.Execute;

    if FRESTResponse.StatusCode = 200 then
    begin
      // Analyser la réponse JSON
      JSONObj := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
      try
        Status := JSONObj.GetValue<string>('status');

        if Status = 'success' then
        begin
          Temperature := JSONObj.GetValue<Double>('temperature');
          Humidity := JSONObj.GetValue<Double>('humidity');

          LabelTemperature.Caption := Format('Température: %.1f°C', [Temperature]);
          LabelHumidity.Caption := Format('Humidité: %.1f%%', [Humidity]);
          MemoLog.Lines.Add('Données récupérées avec succès');

          Result := True;
        end
        else
        begin
          MemoLog.Lines.Add('Erreur: ' + JSONObj.GetValue<string>('message'));
        end;
      finally
        JSONObj.Free;
      end;
    end
    else
    begin
      MemoLog.Lines.Add('Erreur serveur: ' + FRESTResponse.StatusText);
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur: ' + E.Message);
  end;
end;
```

## Projets pratiques avancés

### Système de contrôle domestique intelligent

Voici un exemple plus complet qui intègre Delphi avec Arduino et Raspberry Pi pour créer un système de contrôle domotique simple :

#### Architecture du système

```
[Capteurs et Actionneurs] <---> [Arduino] <---> [Raspberry Pi] <---> [Application Delphi]
   (Température,             (Contrôleur        (Serveur central     (Interface utilisateur)
    Humidité, Lumière,        de capteurs)       avec base de
    Relais, LEDs)                                données SQLite)
```

#### Composants du système
1. **Arduino** : Collecte les données des capteurs et contrôle les actionneurs
2. **Raspberry Pi** : Fait office de serveur central, stocke les données et expose une API REST
3. **Application Delphi** : Interface utilisateur pour visualiser et contrôler le système

#### Côté Arduino
```cpp
#include <ArduinoJson.h>
#include <DHT.h>

// Configuration des broches
#define DHTPIN 2           // Capteur DHT11
#define LIGHT_SENSOR_PIN A0 // Capteur de lumière
#define RELAY_PIN 7        // Relais pour contrôler un appareil
#define LED_PIN 13         // LED de statut

DHT dht(DHTPIN, DHT11);

void setup() {
  Serial.begin(9600);
  dht.begin();

  pinMode(LIGHT_SENSOR_PIN, INPUT);
  pinMode(RELAY_PIN, OUTPUT);
  pinMode(LED_PIN, OUTPUT);

  digitalWrite(RELAY_PIN, LOW);  // Relais désactivé au démarrage
  digitalWrite(LED_PIN, LOW);    // LED éteinte au démarrage
}

void loop() {
  if (Serial.available() > 0) {
    // Lire la commande JSON
    String jsonStr = Serial.readStringUntil('\n');

    // Allouer un buffer pour le document JSON
    StaticJsonDocument<200> doc;
    DeserializationError error = deserializeJson(doc, jsonStr);

    if (!error) {
      // Traiter la commande
      String cmd = doc["cmd"];

      if (cmd == "READ") {
        // Lire les capteurs et envoyer les données
        float humidity = dht.readHumidity();
        float temperature = dht.readTemperature();
        int lightLevel = analogRead(LIGHT_SENSOR_PIN);

        // Créer la réponse JSON
        StaticJsonDocument<200> response;
        response["temperature"] = temperature;
        response["humidity"] = humidity;
        response["light"] = lightLevel;
        response["relay"] = digitalRead(RELAY_PIN) == HIGH;

        // Sérialiser et envoyer
        serializeJson(response, Serial);
        Serial.println();
      }
      else if (cmd == "RELAY") {
        // Contrôler le relais
        bool state = doc["state"];
        digitalWrite(RELAY_PIN, state ? HIGH : LOW);

        // Confirmer l'action
        StaticJsonDocument<100> response;
        response["status"] = "OK";
        response["relay"] = state;

        serializeJson(response, Serial);
        Serial.println();
      }
      else if (cmd == "LED") {
        // Contrôler la LED
        bool state = doc["state"];
        digitalWrite(LED_PIN, state ? HIGH : LOW);

        // Confirmer l'action
        StaticJsonDocument<100> response;
        response["status"] = "OK";
        response["led"] = state;

        serializeJson(response, Serial);
        Serial.println();
      }
    }
  }

  delay(100);  // Petit délai pour stabiliser
}
```

#### Côté Raspberry Pi

Fichier Python pour communiquer avec Arduino et exposer l'API REST :

```python
import serial
import json
import time
import sqlite3
from flask import Flask, jsonify, request

# Configuration
ARDUINO_PORT = '/dev/ttyACM0'  # Adapter selon votre système
BAUD_RATE = 9600
DB_FILE = 'homecontrol.db'

# Initialiser la connexion série
ser = serial.Serial(ARDUINO_PORT, BAUD_RATE, timeout=1)
time.sleep(2)  # Attendre l'initialisation d'Arduino

# Initialiser la base de données
def init_db():
    conn = sqlite3.connect(DB_FILE)
    c = conn.cursor()
    c.execute('''
    CREATE TABLE IF NOT EXISTS sensor_data (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp DATETIME DEFAULT CURRENT_TIMESTAMP,
        temperature REAL,
        humidity REAL,
        light INTEGER,
        relay INTEGER
    )
    ''')
    conn.commit()
    conn.close()

# Fonction pour lire les capteurs Arduino
def read_sensors():
    # Envoyer la commande de lecture
    cmd = json.dumps({"cmd": "READ"})
    ser.write((cmd + '\n').encode())

    # Lire la réponse
    response = ser.readline().decode().strip()

    try:
        data = json.loads(response)

        # Enregistrer dans la base de données
        conn = sqlite3.connect(DB_FILE)
        c = conn.cursor()
        c.execute(
            'INSERT INTO sensor_data (temperature, humidity, light, relay) VALUES (?, ?, ?, ?)',
            (data['temperature'], data['humidity'], data['light'], 1 if data['relay'] else 0)
        )
        conn.commit()
        conn.close()

        return data
    except json.JSONDecodeError:
        return {'error': 'Invalid response', 'raw': response}
    except Exception as e:
        return {'error': str(e)}

# Contrôler le relais
def set_relay(state):
    cmd = json.dumps({"cmd": "RELAY", "state": state})
    ser.write((cmd + '\n').encode())
    response = ser.readline().decode().strip()

    try:
        return json.loads(response)
    except:
        return {'error': 'Invalid response'}

# Contrôler la LED
def set_led(state):
    cmd = json.dumps({"cmd": "LED", "state": state})
    ser.write((cmd + '\n').encode())
    response = ser.readline().decode().strip()

    try:
        return json.loads(response)
    except:
        return {'error': 'Invalid response'}

# Créer l'application Flask
app = Flask(__name__)

@app.route('/api/sensors', methods=['GET'])
def get_sensors():
    return jsonify(read_sensors())

@app.route('/api/history', methods=['GET'])
def get_history():
    limit = request.args.get('limit', default=10, type=int)

    conn = sqlite3.connect(DB_FILE)
    conn.row_factory = sqlite3.Row
    c = conn.cursor()
    c.execute('SELECT * FROM sensor_data ORDER BY timestamp DESC LIMIT ?', (limit,))
    rows = c.fetchall()
    conn.close()

    # Convertir en liste de dictionnaires
    result = [dict(row) for row in rows]
    return jsonify(result)

@app.route('/api/relay', methods=['POST'])
def control_relay():
    data = request.get_json()
    state = data.get('state', False)
    return jsonify(set_relay(state))

@app.route('/api/led', methods=['POST'])
def control_led():
    data = request.get_json()
    state = data.get('state', False)
    return jsonify(set_led(state))

# Initialiser la base de données et démarrer le serveur
if __name__ == '__main__':
    init_db()
    app.run(host='0.0.0.0', port=5000)
```

#### Côté Delphi

Pour l'application Delphi, nous allons créer une interface plus élaborée avec :
- Un tableau de bord pour afficher les données des capteurs
- Un historique avec graphiques
- Des contrôles pour les relais et LEDs

```delphi
unit MainUnit;

interface

uses
  System.SysUtils, System.Classes, System.JSON, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, VCLTee.TeeGDIPlus, VCLTee.TeEngine,
  VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart, REST.Types, REST.Client,
  REST.Response.Adapter, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabDashboard: TTabSheet;
    TabHistory: TTabSheet;
    PanelConnection: TPanel;
    EditIP: TEdit;
    EditPort: TEdit;
    ButtonConnect: TButton;
    PanelDashboard: TPanel;
    LabelTemperature: TLabel;
    LabelHumidity: TLabel;
    LabelLight: TLabel;
    LabelRelayStatus: TLabel;
    ButtonRelayOn: TButton;
    ButtonRelayOff: TButton;
    ButtonLedOn: TButton;
    ButtonLedOff: TButton;
    TimerRefresh: TTimer;
    ChartTemp: TChart;
    ChartHumidity: TChart;
    ChartLight: TChart;
    ButtonRefreshHistory: TButton;
    ComboBoxHistoryLimit: TComboBox;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnectClick(Sender: TObject);
    procedure TimerRefreshTimer(Sender: TObject);
    procedure ButtonRelayOnClick(Sender: TObject);
    procedure ButtonRelayOffClick(Sender: TObject);
    procedure ButtonLedOnClick(Sender: TObject);
    procedure ButtonLedOffClick(Sender: TObject);
    procedure ButtonRefreshHistoryClick(Sender: TObject);
  private
    FRESTClient: TRESTClient;
    FSensorsRequest: TRESTRequest;
    FHistoryRequest: TRESTRequest;
    FControlRequest: TRESTRequest;
    FResponse: TRESTResponse;

    FTempSeries: TLineSeries;
    FHumSeries: TLineSeries;
    FLightSeries: TLineSeries;

    function ReadSensors: Boolean;
    function GetHistory(Limit: Integer): Boolean;
    function ControlRelay(State: Boolean): Boolean;
    function ControlLed(State: Boolean): Boolean;
    procedure UpdateDashboard(const JSONObj: TJSONObject);
    procedure UpdateHistoryCharts(const JSONArray: TJSONArray);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialiser les composants REST
  FRESTClient := TRESTClient.Create(Self);
  FResponse := TRESTResponse.Create(Self);

  FSensorsRequest := TRESTRequest.Create(Self);
  FSensorsRequest.Client := FRESTClient;
  FSensorsRequest.Response := FResponse;
  FSensorsRequest.Resource := 'api/sensors';
  FSensorsRequest.Method := rmGET;

  FHistoryRequest := TRESTRequest.Create(Self);
  FHistoryRequest.Client := FRESTClient;
  FHistoryRequest.Response := FResponse;
  FHistoryRequest.Resource := 'api/history';
  FHistoryRequest.Method := rmGET;

  FControlRequest := TRESTRequest.Create(Self);
  FControlRequest.Client := FRESTClient;
  FControlRequest.Response := FResponse;
  FControlRequest.Method := rmPOST;

  // Initialiser les séries de graphiques
  FTempSeries := TLineSeries.Create(Self);
  FTempSeries.Title := 'Température (°C)';
  FTempSeries.Color := clRed;
  ChartTemp.AddSeries(FTempSeries);

  FHumSeries := TLineSeries.Create(Self);
  FHumSeries.Title := 'Humidité (%)';
  FHumSeries.Color := clBlue;
  ChartHumidity.AddSeries(FHumSeries);

  FLightSeries := TLineSeries.Create(Self);
  FLightSeries.Title := 'Niveau de lumière';
  FLightSeries.Color := clGreen;
  ChartLight.AddSeries(FLightSeries);

  // Remplir le combobox pour l'historique
  ComboBoxHistoryLimit.Items.Add('10 derniers points');
  ComboBoxHistoryLimit.Items.Add('20 derniers points');
  ComboBoxHistoryLimit.Items.Add('50 derniers points');
  ComboBoxHistoryLimit.Items.Add('100 derniers points');
  ComboBoxHistoryLimit.ItemIndex := 0;

  EditIP.Text := '192.168.1.100';  // Remplacer par l'IP de votre Raspberry Pi
  EditPort.Text := '5000';

  TimerRefresh.Enabled := False;
  ButtonRelayOn.Enabled := False;
  ButtonRelayOff.Enabled := False;
  ButtonLedOn.Enabled := False;
  ButtonLedOff.Enabled := False;
  ButtonRefreshHistory.Enabled := False;
end;

procedure TMainForm.ButtonConnectClick(Sender: TObject);
begin
  if ButtonConnect.Caption = 'Connecter' then
  begin
    // Configuration de l'URL de base
    FRESTClient.BaseURL := Format('http://%s:%s', [EditIP.Text, EditPort.Text]);

    // Tester la connexion
    try
      FSensorsRequest.Execute;
      if FResponse.StatusCode = 200 then
      begin
        ButtonConnect.Caption := 'Déconnecter';
        TimerRefresh.Enabled := True;
        ButtonRelayOn.Enabled := True;
        ButtonRelayOff.Enabled := True;
        ButtonLedOn.Enabled := True;
        ButtonLedOff.Enabled := True;
        ButtonRefreshHistory.Enabled := True;

        ReadSensors;  // Lecture initiale
        MemoLog.Lines.Add('Connecté avec succès');
      end
      else
      begin
        MemoLog.Lines.Add('Erreur: ' + FResponse.StatusText);
      end;
    except
      on E: Exception do
        MemoLog.Lines.Add('Erreur de connexion: ' + E.Message);
    end;
  end
  else
  begin
    ButtonConnect.Caption := 'Connecter';
    TimerRefresh.Enabled := False;
    ButtonRelayOn.Enabled := False;
    ButtonRelayOff.Enabled := False;
    ButtonLedOn.Enabled := False;
    ButtonLedOff.Enabled := False;
    ButtonRefreshHistory.Enabled := False;
    MemoLog.Lines.Add('Déconnecté');
  end;
end;

function TMainForm.ReadSensors: Boolean;
var
  JSONObj: TJSONObject;
begin
  Result := False;

  try
    FSensorsRequest.Execute;

    if FResponse.StatusCode = 200 then
    begin
      JSONObj := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
      try
        if JSONObj.FindValue('error') <> nil then
        begin
          MemoLog.Lines.Add('Erreur: ' + JSONObj.GetValue<string>('error'));
        end
        else
        begin
          UpdateDashboard(JSONObj);
          Result := True;
        end;
      finally
        JSONObj.Free;
      end;
    end
    else
    begin
      MemoLog.Lines.Add('Erreur serveur: ' + FResponse.StatusText);
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TMainForm.UpdateDashboard(const JSONObj: TJSONObject);
begin
  LabelTemperature.Caption := Format('Température: %.1f°C', [JSONObj.GetValue<Double>('temperature')]);
  LabelHumidity.Caption := Format('Humidité: %.1f%%', [JSONObj.GetValue<Double>('humidity')]);
  LabelLight.Caption := Format('Niveau de lumière: %d', [JSONObj.GetValue<Integer>('light')]);

  if JSONObj.GetValue<Boolean>('relay') then
    LabelRelayStatus.Caption := 'Relais: ACTIVÉ'
  else
    LabelRelayStatus.Caption := 'Relais: DÉSACTIVÉ';
end;

function TMainForm.GetHistory(Limit: Integer): Boolean;
var
  JSONArray: TJSONArray;
begin
  Result := False;

  try
    // Ajouter le paramètre de limite
    FHistoryRequest.Params.Clear;
    FHistoryRequest.AddParameter('limit', IntToStr(Limit), pkGETorPOST);

    FHistoryRequest.Execute;

    if FResponse.StatusCode = 200 then
    begin
      JSONArray := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONArray;
      try
        UpdateHistoryCharts(JSONArray);
        Result := True;
      finally
        JSONArray.Free;
      end;
    end
    else
    begin
      MemoLog.Lines.Add('Erreur serveur: ' + FResponse.StatusText);
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TMainForm.UpdateHistoryCharts(const JSONArray: TJSONArray);
var
  I: Integer;
  Item: TJSONObject;
  Timestamp: string;
begin
  // Effacer les séries existantes
  FTempSeries.Clear;
  FHumSeries.Clear;
  FLightSeries.Clear;

  // Ajouter les points en ordre chronologique
  for I := JSONArray.Count - 1 downto 0 do
  begin
    Item := JSONArray.Items[I] as TJSONObject;
    Timestamp := Item.GetValue<string>('timestamp');

    FTempSeries.AddXY(I, Item.GetValue<Double>('temperature'), Timestamp);
    FHumSeries.AddXY(I, Item.GetValue<Double>('humidity'), Timestamp);
    FLightSeries.AddXY(I, Item.GetValue<Integer>('light'), Timestamp);
  end;
end;

function TMainForm.ControlRelay(State: Boolean): Boolean;
var
  JSONBody, JSONResponse: TJSONObject;
begin
  Result := False;

  try
    // Préparer le corps de la requête
    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('state', TJSONBool.Create(State));

      FControlRequest.Resource := 'api/relay';
      FControlRequest.Body.ClearBody;
      FControlRequest.Body.Add(JSONBody.ToString, ContentTypeFromString('application/json'));

      FControlRequest.Execute;

      if FResponse.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
        try
          if JSONResponse.FindValue('error') <> nil then
          begin
            MemoLog.Lines.Add('Erreur: ' + JSONResponse.GetValue<string>('error'));
          end
          else
          begin
            if State then
              MemoLog.Lines.Add('Relais activé')
            else
              MemoLog.Lines.Add('Relais désactivé');

            Result := True;
            ReadSensors;  // Mettre à jour le tableau de bord
          end;
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        MemoLog.Lines.Add('Erreur serveur: ' + FResponse.StatusText);
      end;
    finally
      JSONBody.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur: ' + E.Message);
  end;
end;

function TMainForm.ControlLed(State: Boolean): Boolean;
var
  JSONBody, JSONResponse: TJSONObject;
begin
  Result := False;

  try
    // Préparer le corps de la requête
    JSONBody := TJSONObject.Create;
    try
      JSONBody.AddPair('state', TJSONBool.Create(State));

      FControlRequest.Resource := 'api/led';
      FControlRequest.Body.ClearBody;
      FControlRequest.Body.Add(JSONBody.ToString, ContentTypeFromString('application/json'));

      FControlRequest.Execute;

      if FResponse.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(FResponse.Content) as TJSONObject;
        try
          if JSONResponse.FindValue('error') <> nil then
          begin
            MemoLog.Lines.Add('Erreur: ' + JSONResponse.GetValue<string>('error'));
          end
          else
          begin
            if State then
              MemoLog.Lines.Add('LED activée')
            else
              MemoLog.Lines.Add('LED désactivée');

            Result := True;
          end;
        finally
          JSONResponse.Free;
        end;
      end
      else
      begin
        MemoLog.Lines.Add('Erreur serveur: ' + FResponse.StatusText);
      end;
    finally
      JSONBody.Free;
    end;
  except
    on E: Exception do
      MemoLog.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TMainForm.TimerRefreshTimer(Sender: TObject);
begin
  ReadSensors;
end;

procedure TMainForm.ButtonRelayOnClick(Sender: TObject);
begin
  ControlRelay(True);
end;

procedure TMainForm.ButtonRelayOffClick(Sender: TObject);
begin
  ControlRelay(False);
end;

procedure TMainForm.ButtonLedOnClick(Sender: TObject);
begin
  ControlLed(True);
end;

procedure TMainForm.ButtonLedOffClick(Sender: TObject);
begin
  ControlLed(False);
end;

procedure TMainForm.ButtonRefreshHistoryClick(Sender: TObject);
var
  Limit: Integer;
begin
  case ComboBoxHistoryLimit.ItemIndex of
    0: Limit := 10;
    1: Limit := 20;
    2: Limit := 50;
    3: Limit := 100;
    else Limit := 10;
  end;

  GetHistory(Limit);
end;
```

### Système de surveillance environnementale

Un autre projet pratique consiste à créer un système de surveillance environnementale avec multiples capteurs distribués et une application centralisée pour la visualisation.

#### Architecture
- **Plusieurs Arduinos** avec capteurs connectés à un Raspberry Pi central
- **Raspberry Pi** comme passerelle collectant les données et les stockant
- **Application Delphi** pour visualiser les données et configurer des alertes

Ce type de projet pourrait être étendu pour inclure :
- Alertes par email ou SMS
- Interface Web supplémentaire
- Rapports automatiques
- Intégration avec des services cloud

## Bonnes pratiques et conseils

### Conseils pour l'intégration Arduino-Delphi

1. **Protocole de communication robuste** : Définissez un protocole clair entre Delphi et l'Arduino, idéalement basé sur JSON pour faciliter le traitement.

2. **Gestion des erreurs** : Implémentez une gestion d'erreurs complète des deux côtés.

3. **Temporisation** : Ajoutez des délais appropriés pour éviter de surcharger l'Arduino avec trop de commandes.

4. **Surveillance de la connexion** : Implémentez un mécanisme de surveillance pour détecter les déconnexions.

5. **Mise en mémoire tampon** : Utilisez des tampons pour accumuler les données et éviter les pertes.

### Conseils pour l'intégration Raspberry Pi-Delphi

1. **API RESTful** : Préférez une API REST bien structurée pour la communication entre Delphi et Raspberry Pi.

2. **Authentification** : Ajoutez une authentification simple pour sécuriser l'accès à l'API.

3. **Mise en cache** : Implémentez un mécanisme de mise en cache côté Delphi pour réduire les appels réseau.

4. **Connexions persistantes** : Utilisez des connexions HTTP persistantes pour améliorer les performances.

5. **Format des données** : Standardisez le format des données échangées (JSON est recommandé).

## Résolution des problèmes courants

### Problèmes de connexion Arduino

| Problème | Solution |
|----------|----------|
| Arduino non détecté | Vérifiez le câble USB et les pilotes |
| Déconnexions fréquentes | Ajoutez des délais et réduisez la fréquence des communications |
| Données corrompues | Utilisez un protocole avec validation (CRC ou checksum) |
| Arduino bloqué | Implémentez un watchdog dans le code Arduino |

### Problèmes de connexion Raspberry Pi

| Problème | Solution |
|----------|----------|
| Connexion refusée | Vérifiez les pare-feu et les règles de routage |
| Latence élevée | Optimisez la taille des requêtes et implémentez la mise en cache |
| Erreurs HTTP | Vérifiez les logs du serveur sur le Raspberry Pi |
| Charge CPU élevée | Réduisez la fréquence des requêtes ou optimisez le code serveur |

## Ressources complémentaires

### Bibliothèques et outils pour Arduino
- [ArduinoJson](https://arduinojson.org/) - Bibliothèque pour traiter le JSON
- [PlatformIO](https://platformio.org/) - Alternative à l'IDE Arduino avec gestion de dépendances

### Bibliothèques et outils pour Raspberry Pi
- [Flask](https://flask.palletsprojects.com/) - Framework léger pour API web
- [pySerial](https://pyserial.readthedocs.io/) - Pour la communication série avec Arduino

### Bibliothèques Delphi pour IoT
- [REST Components](https://www.embarcadero.com/products/rad-studio/features/rest-client-components) - Pour communiquer avec des API REST
- [TComPort](https://sourceforge.net/projects/comport/) - Pour la communication série
- [TMSIntraIoT](https://www.tmssoftware.com) - Composants IoT complets

## Conclusion

L'intégration de Delphi avec Arduino et Raspberry Pi offre des possibilités infinies pour créer des solutions IoT complètes. Que vous construisiez un système simple de surveillance ou une solution domotique complète, la combinaison de ces technologies permet de tirer parti des forces de chacune :

- **Arduino** pour l'interaction directe avec les capteurs et actionneurs
- **Raspberry Pi** pour le traitement, le stockage et la connectivité réseau
- **Delphi** pour créer des interfaces utilisateur riches et professionnelles

Dans les prochaines sections, nous explorerons les protocoles IoT spécialisés comme MQTT et CoAP, ainsi que la gestion des dispositifs connectés à plus grande échelle.

⏭️ [Contrôle de périphériques externes](21-delphi-et-liot/04-controle-de-peripheriques-externes.md)
