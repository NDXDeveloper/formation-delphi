🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.4 Contrôle de périphériques externes

## Introduction

Dans cette section, nous allons explorer comment contrôler différents types de périphériques externes depuis votre application Delphi via Arduino ou Raspberry Pi. Le contrôle de dispositifs physiques est au cœur de l'IoT : allumer des lumières, activer des moteurs, ouvrir des vannes, afficher des informations, produire des sons... Les possibilités sont infinies.

## Notions d'électronique de base

Avant de commencer à contrôler des périphériques, il est important de comprendre quelques concepts électroniques fondamentaux.

### Tension, courant et puissance

#### Tension (Volt - V)
La tension représente la "pression" électrique qui pousse le courant dans un circuit. C'est comme la pression de l'eau dans un tuyau.

- **5V** : tension standard Arduino
- **3.3V** : tension logique de nombreux capteurs modernes
- **12V, 24V** : tensions courantes pour moteurs et relais
- **220V** : tension secteur (DANGER - nécessite des précautions)

#### Courant (Ampère - A)
Le courant représente la quantité d'électricité qui circule. C'est comme le débit d'eau dans un tuyau.

- **mA (milliampère)** : 1000 mA = 1 A
- Une LED consomme environ 20 mA
- Un moteur petit peut consommer 100-500 mA
- Un moteur puissant peut consommer plusieurs ampères

#### Puissance (Watt - W)
La puissance est le produit de la tension et du courant : **P = V × I**

- Une LED : 0.1 W
- Un petit moteur : 5-10 W
- Un relais : 0.5-2 W

### Limitations importantes

#### Broches Arduino
Les broches digitales d'un Arduino peuvent fournir :
- **Maximum 40 mA** par broche
- **Maximum 200 mA** au total pour toutes les broches

**Important** : Ne jamais connecter directement un dispositif gourmand (moteur, relais puissant) à une broche Arduino. Vous risquez d'endommager la carte.

#### Protection nécessaire
Pour contrôler des charges importantes, utilisez :
- **Transistors** : pour des courants jusqu'à quelques ampères
- **MOSFETs** : pour des courants plus élevés
- **Relais** : pour isoler complètement les circuits
- **Modules de puissance** : solutions intégrées prêtes à l'emploi

### Signaux de contrôle

#### Signal digital (ON/OFF)
- **HIGH (1)** : 5V sur Arduino Uno, 3.3V sur Arduino Due
- **LOW (0)** : 0V (masse/GND)
- Utilisation : LED, relais, actionneurs simples

#### Signal PWM (Pulse Width Modulation)
Le PWM permet de simuler une tension variable en alternant rapidement entre HIGH et LOW.

- **Duty cycle** : pourcentage du temps à HIGH
  - 0% : toujours LOW (éteint)
  - 50% : moitié du temps HIGH, moitié LOW
  - 100% : toujours HIGH (pleine puissance)
- **Utilisation** : variation d'intensité LED, contrôle vitesse moteur, servomoteurs

Sur Arduino, le PWM va de 0 (0%) à 255 (100%).

## Types de périphériques

### 1. LEDs (Light Emitting Diodes)

Les LEDs sont les périphériques les plus simples à contrôler.

#### Caractéristiques
- **Tension** : environ 2-3V selon la couleur
- **Courant** : 10-20 mA
- **Polarité** : attention, la LED a un sens (anode +, cathode -)
- **Résistance** : toujours utiliser une résistance de limitation (220-330 Ohms)

#### Code Arduino : Contrôle simple

```cpp
const int LED_PIN = 13;

void setup() {
  Serial.begin(9600);
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    if (command == "LED:ON") {
      digitalWrite(LED_PIN, HIGH);
      Serial.println("STATUS:LED_ON");
    }
    else if (command == "LED:OFF") {
      digitalWrite(LED_PIN, LOW);
      Serial.println("STATUS:LED_OFF");
    }
  }
}
```

#### Code Arduino : Variation d'intensité (PWM)

```cpp
const int LED_PIN = 9;  // Doit être une broche PWM (marquée ~)

void setup() {
  Serial.begin(9600);
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      if (cmd == "BRIGHTNESS") {
        int brightness = value.toInt();  // 0-255
        brightness = constrain(brightness, 0, 255);
        analogWrite(LED_PIN, brightness);
        Serial.println("STATUS:BRIGHTNESS_SET");
      }
    }
  }
}
```

#### Code Delphi : Interface de contrôle

```pascal
unit LEDControl;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.StdCtrls, Vcl.ComCtrls, CPort;

type
  TFormLEDControl = class(TForm)
    ComPort1: TComPort;
    ButtonOn: TButton;
    ButtonOff: TButton;
    TrackBarBrightness: TTrackBar;
    LabelBrightness: TLabel;
    procedure ButtonOnClick(Sender: TObject);
    procedure ButtonOffClick(Sender: TObject);
    procedure TrackBarBrightnessChange(Sender: TObject);
  private
    procedure SendCommand(const Command: string);
  end;

var
  FormLEDControl: TFormLEDControl;

implementation

{$R *.dfm}

procedure TFormLEDControl.SendCommand(const Command: string);  
begin  
  if ComPort1.Connected then
    ComPort1.WriteStr(Command + #13#10);
end;

procedure TFormLEDControl.ButtonOnClick(Sender: TObject);  
begin  
  SendCommand('LED:ON');
end;

procedure TFormLEDControl.ButtonOffClick(Sender: TObject);  
begin  
  SendCommand('LED:OFF');
end;

procedure TFormLEDControl.TrackBarBrightnessChange(Sender: TObject);  
var  
  Brightness: Integer;
begin
  Brightness := TrackBarBrightness.Position;
  LabelBrightness.Caption := Format('Intensité : %d%%', [(Brightness * 100) div 255]);
  SendCommand('BRIGHTNESS:' + IntToStr(Brightness));
end;

end.
```

### 2. Relais (Relays)

Les relais sont des interrupteurs électromécaniques qui permettent de contrôler des charges importantes (lampes 220V, moteurs puissants) avec un signal faible.

#### Principe de fonctionnement
Un relais contient une bobine électromagnétique qui, lorsqu'elle est alimentée, attire un contact métallique pour fermer (ou ouvrir) un circuit électrique séparé.

**Avantages :**
- Isolation électrique complète entre contrôle et charge
- Peut commuter des tensions et courants élevés
- Compatible avec AC et DC

**Inconvénients :**
- Usure mécanique
- Bruit de commutation (clic)
- Vitesse de commutation limitée
- Consomme plus d'énergie qu'un transistor

#### Types de relais

**Relais simple (SPST - Single Pole Single Throw)**
- 2 bornes de sortie
- ON ou OFF simple

**Relais inverseur (SPDT - Single Pole Double Throw)**
- 3 bornes de sortie : COM (commun), NO (normalement ouvert), NC (normalement fermé)
- Permet de basculer entre deux circuits

#### Module relais pour Arduino

Les modules relais intègrent tout le nécessaire :
- Relais électromécanique
- Circuit de protection
- LED d'indication
- Optoisolateur (sur certains modèles)

**Connexions typiques :**
- VCC : 5V
- GND : Masse
- IN : Signal de commande (broche Arduino)

#### Code Arduino : Contrôle de relais

```cpp
const int RELAY_PIN = 7;

void setup() {
  Serial.begin(9600);
  pinMode(RELAY_PIN, OUTPUT);
  digitalWrite(RELAY_PIN, LOW);  // Relais désactivé au démarrage
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String device = command.substring(0, colonPos);
      String state = command.substring(colonPos + 1);

      if (device == "RELAY") {
        if (state == "ON") {
          digitalWrite(RELAY_PIN, HIGH);
          Serial.println("STATUS:RELAY_ON");
        }
        else if (state == "OFF") {
          digitalWrite(RELAY_PIN, LOW);
          Serial.println("STATUS:RELAY_OFF");
        }
      }
    }
  }
}
```

#### Code Delphi : Contrôle de relais multiple

```pascal
type
  TRelayController = class
  private
    FComPort: TComPort;
    FRelayStates: array[1..4] of Boolean;
  public
    constructor Create(AComPort: TComPort);
    procedure SetRelayState(RelayNumber: Integer; State: Boolean);
    function GetRelayState(RelayNumber: Integer): Boolean;
  end;

constructor TRelayController.Create(AComPort: TComPort);  
var  
  I: Integer;
begin
  FComPort := AComPort;
  for I := 1 to 4 do
    FRelayStates[I] := False;
end;

procedure TRelayController.SetRelayState(RelayNumber: Integer; State: Boolean);  
var  
  Command: string;
  StateStr: string;
begin
  if (RelayNumber < 1) or (RelayNumber > 4) then
    raise Exception.Create('Numéro de relais invalide');

  if State then
    StateStr := 'ON'
  else
    StateStr := 'OFF';

  Command := Format('RELAY%d:%s', [RelayNumber, StateStr]);

  if FComPort.Connected then
  begin
    FComPort.WriteStr(Command + #13#10);
    FRelayStates[RelayNumber] := State;
  end;
end;

function TRelayController.GetRelayState(RelayNumber: Integer): Boolean;  
begin  
  Result := FRelayStates[RelayNumber];
end;

// Utilisation dans le formulaire
procedure TFormMain.ButtonRelay1Click(Sender: TObject);  
var  
  NewState: Boolean;
begin
  NewState := not RelayController.GetRelayState(1);
  RelayController.SetRelayState(1, NewState);

  if NewState then
    ButtonRelay1.Caption := 'Relais 1: ON'
  else
    ButtonRelay1.Caption := 'Relais 1: OFF';
end;
```

### 3. Moteurs DC (à courant continu)

Les moteurs DC sont utilisés pour créer du mouvement rotatif : robots, ventilateurs, pompes, convoyeurs, etc.

#### Caractéristiques
- **Tension** : 3V à 24V selon le moteur
- **Courant** : de 100 mA à plusieurs ampères
- **Contrôle** : vitesse variable avec PWM, sens avec pont en H

#### Pont en H (H-Bridge)

Un pont en H permet de :
- Contrôler la direction du moteur (avant/arrière)
- Contrôler la vitesse (avec PWM)
- Protéger l'Arduino

**Modules populaires :**
- **L298N** : jusqu'à 2A par moteur, 2 moteurs
- **L293D** : jusqu'à 600mA par moteur, 2 moteurs
- **DRV8833** : plus moderne, plus efficace

#### Code Arduino : Contrôle moteur DC avec L298N

```cpp
// Connexions L298N
const int MOTOR_IN1 = 8;   // Direction  
const int MOTOR_IN2 = 9;   // Direction  
const int MOTOR_ENA = 10;  // PWM vitesse (broche ~)  

void setup() {
  Serial.begin(9600);

  pinMode(MOTOR_IN1, OUTPUT);
  pinMode(MOTOR_IN2, OUTPUT);
  pinMode(MOTOR_ENA, OUTPUT);

  // Arrêter le moteur au démarrage
  digitalWrite(MOTOR_IN1, LOW);
  digitalWrite(MOTOR_IN2, LOW);
  analogWrite(MOTOR_ENA, 0);
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      if (cmd == "MOTOR") {
        if (value == "STOP") {
          digitalWrite(MOTOR_IN1, LOW);
          digitalWrite(MOTOR_IN2, LOW);
          analogWrite(MOTOR_ENA, 0);
          Serial.println("STATUS:MOTOR_STOPPED");
        }
        else if (value.startsWith("FWD:")) {
          // Avant avec vitesse
          int speed = value.substring(4).toInt();
          speed = constrain(speed, 0, 255);

          digitalWrite(MOTOR_IN1, HIGH);
          digitalWrite(MOTOR_IN2, LOW);
          analogWrite(MOTOR_ENA, speed);
          Serial.println("STATUS:MOTOR_FORWARD");
        }
        else if (value.startsWith("REV:")) {
          // Arrière avec vitesse
          int speed = value.substring(4).toInt();
          speed = constrain(speed, 0, 255);

          digitalWrite(MOTOR_IN1, LOW);
          digitalWrite(MOTOR_IN2, HIGH);
          analogWrite(MOTOR_ENA, speed);
          Serial.println("STATUS:MOTOR_REVERSE");
        }
      }
    }
  }
}
```

#### Code Delphi : Interface de contrôle moteur

```pascal
unit MotorControl;

interface

uses
  System.SysUtils, Vcl.Forms, Vcl.StdCtrls, Vcl.ComCtrls, CPort;

type
  TMotorDirection = (mdStop, mdForward, mdReverse);

  TFormMotorControl = class(TForm)
    ComPort1: TComPort;
    ButtonForward: TButton;
    ButtonReverse: TButton;
    ButtonStop: TButton;
    TrackBarSpeed: TTrackBar;
    LabelSpeed: TLabel;
    procedure ButtonForwardClick(Sender: TObject);
    procedure ButtonReverseClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure TrackBarSpeedChange(Sender: TObject);
  private
    FCurrentDirection: TMotorDirection;
    FCurrentSpeed: Integer;
    procedure SetMotor(Direction: TMotorDirection; Speed: Integer);
  end;

implementation

{$R *.dfm}

procedure TFormMotorControl.SetMotor(Direction: TMotorDirection; Speed: Integer);  
var  
  Command: string;
begin
  if not ComPort1.Connected then Exit;

  FCurrentDirection := Direction;
  FCurrentSpeed := Speed;

  case Direction of
    mdStop:
      Command := 'MOTOR:STOP';
    mdForward:
      Command := Format('MOTOR:FWD:%d', [Speed]);
    mdReverse:
      Command := Format('MOTOR:REV:%d', [Speed]);
  end;

  ComPort1.WriteStr(Command + #13#10);
end;

procedure TFormMotorControl.ButtonForwardClick(Sender: TObject);  
begin  
  SetMotor(mdForward, TrackBarSpeed.Position);
end;

procedure TFormMotorControl.ButtonReverseClick(Sender: TObject);  
begin  
  SetMotor(mdReverse, TrackBarSpeed.Position);
end;

procedure TFormMotorControl.ButtonStopClick(Sender: TObject);  
begin  
  SetMotor(mdStop, 0);
  TrackBarSpeed.Position := 0;
end;

procedure TFormMotorControl.TrackBarSpeedChange(Sender: TObject);  
begin  
  LabelSpeed.Caption := Format('Vitesse : %d%%',
    [(TrackBarSpeed.Position * 100) div 255]);

  // Appliquer la nouvelle vitesse si le moteur tourne
  if FCurrentDirection <> mdStop then
    SetMotor(FCurrentDirection, TrackBarSpeed.Position);
end;

end.
```

### 4. Servomoteurs

Les servomoteurs sont des moteurs avec contrôle de position précis. Ils sont très populaires en robotique.

#### Caractéristiques
- **Angle** : généralement 0° à 180° (certains 360° continus)
- **Tension** : 4.8V à 6V
- **Courant** : 100mA à 1A selon le couple
- **Signal** : PWM spécifique (pulse de 1-2ms toutes les 20ms)

#### Types de servomoteurs

**Servo standard (180°)**
- Rotation limitée à 180°
- Position contrôlable précisément
- Usage : bras robotiques, direction, caméra pan/tilt

**Servo continu (360°)**
- Rotation continue dans les deux sens
- Contrôle de vitesse plutôt que position
- Usage : roues de robot, convoyeur

#### Code Arduino : Contrôle servomoteur

```cpp
#include <Servo.h>

Servo myServo;  
const int SERVO_PIN = 9;  

void setup() {
  Serial.begin(9600);
  myServo.attach(SERVO_PIN);
  myServo.write(90);  // Position centrale au démarrage
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      if (cmd == "SERVO") {
        int angle = value.toInt();
        angle = constrain(angle, 0, 180);
        myServo.write(angle);
        Serial.print("STATUS:SERVO_");
        Serial.println(angle);
      }
    }
  }
}
```

#### Code Delphi : Interface servomoteur

```pascal
procedure TFormServoControl.TrackBarAngleChange(Sender: TObject);  
var  
  Angle: Integer;
begin
  Angle := TrackBarAngle.Position;
  LabelAngle.Caption := Format('Angle : %d°', [Angle]);

  if ComPort1.Connected then
    ComPort1.WriteStr(Format('SERVO:%d', [Angle]) + #13#10);
end;

// Boutons de position prédéfinie
procedure TFormServoControl.ButtonCenterClick(Sender: TObject);  
begin  
  TrackBarAngle.Position := 90;
end;

procedure TFormServoControl.ButtonLeftClick(Sender: TObject);  
begin  
  TrackBarAngle.Position := 0;
end;

procedure TFormServoControl.ButtonRightClick(Sender: TObject);  
begin  
  TrackBarAngle.Position := 180;
end;
```

### 5. Écrans LCD et OLED

Les écrans permettent d'afficher des informations directement sur le dispositif, sans avoir besoin d'un ordinateur.

#### LCD 16x2 ou 20x4 (avec I2C)

**Caractéristiques :**
- Affichage texte uniquement
- 16 ou 20 caractères par ligne
- 2 ou 4 lignes
- Rétroéclairage (souvent bleu ou vert)
- Interface I2C (2 fils seulement : SDA et SCL)

#### OLED (SSD1306)

**Caractéristiques :**
- Écran graphique (128x64 pixels typiquement)
- Très bon contraste
- Pas besoin de rétroéclairage
- Interface I2C ou SPI
- Consommation faible

#### Code Arduino : Affichage LCD I2C

```cpp
#include <Wire.h>
#include <LiquidCrystal_I2C.h>

// Adresse I2C : 0x27 ou 0x3F selon le module
LiquidCrystal_I2C lcd(0x27, 16, 2);

void setup() {
  Serial.begin(9600);

  lcd.init();
  lcd.backlight();
  lcd.setCursor(0, 0);
  lcd.print("Pret!");
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      if (cmd == "LCD") {
        // Format: LCD:ligne,colonne,texte
        int commaPos1 = value.indexOf(',');
        int commaPos2 = value.indexOf(',', commaPos1 + 1);

        if (commaPos1 > 0 && commaPos2 > 0) {
          int line = value.substring(0, commaPos1).toInt();
          int col = value.substring(commaPos1 + 1, commaPos2).toInt();
          String text = value.substring(commaPos2 + 1);

          lcd.setCursor(col, line);
          lcd.print(text);

          // Effacer le reste de la ligne
          for (int i = text.length(); i < 16; i++) {
            lcd.print(" ");
          }

          Serial.println("STATUS:LCD_UPDATED");
        }
      }
      else if (cmd == "LCD_CLEAR") {
        lcd.clear();
        Serial.println("STATUS:LCD_CLEARED");
      }
    }
  }
}
```

#### Code Delphi : Affichage sur LCD

```pascal
type
  TLCDController = class
  private
    FComPort: TComPort;
  public
    constructor Create(AComPort: TComPort);
    procedure DisplayText(Line, Column: Integer; const Text: string);
    procedure ClearScreen;
  end;

constructor TLCDController.Create(AComPort: TComPort);  
begin  
  FComPort := AComPort;
end;

procedure TLCDController.DisplayText(Line, Column: Integer; const Text: string);  
var  
  Command: string;
begin
  if not FComPort.Connected then Exit;

  // Limiter la longueur du texte
  Command := Format('LCD:%d,%d,%s', [Line, Column, Copy(Text, 1, 16)]);
  FComPort.WriteStr(Command + #13#10);
end;

procedure TLCDController.ClearScreen;  
begin  
  if FComPort.Connected then
    FComPort.WriteStr('LCD_CLEAR:' + #13#10);
end;

// Utilisation
procedure TFormMain.UpdateLCDDisplay;  
var  
  TempText, HumText: string;
begin
  TempText := Format('Temp: %.1fC', [CurrentTemperature]);
  HumText := Format('Hum: %.0f%%', [CurrentHumidity]);

  LCDController.DisplayText(0, 0, TempText);
  LCDController.DisplayText(1, 0, HumText);
end;
```

### 6. Buzzers et sons

Les buzzers permettent de produire des sons : alarmes, notifications, feedback utilisateur.

#### Types de buzzers

**Buzzer passif**
- Nécessite un signal PWM pour produire un son
- Peut jouer différentes notes (fréquences)
- Plus flexible

**Buzzer actif**
- Produit un son fixe quand alimenté
- Plus simple à utiliser
- Son unique

#### Code Arduino : Buzzer avec mélodies

```cpp
const int BUZZER_PIN = 8;

// Notes de musique (fréquences en Hz)
#define NOTE_C4  262
#define NOTE_D4  294
#define NOTE_E4  330
#define NOTE_F4  349
#define NOTE_G4  392
#define NOTE_A4  440
#define NOTE_B4  494

void setup() {
  Serial.begin(9600);
  pinMode(BUZZER_PIN, OUTPUT);
}

void playTone(int frequency, int duration) {
  tone(BUZZER_PIN, frequency, duration);
  delay(duration);
  noTone(BUZZER_PIN);
}

void playBeep() {
  playTone(1000, 100);
}

void playSuccess() {
  playTone(NOTE_C4, 200);
  playTone(NOTE_E4, 200);
  playTone(NOTE_G4, 400);
}

void playError() {
  playTone(500, 200);
  delay(100);
  playTone(500, 200);
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    int colonPos = command.indexOf(':');
    if (colonPos > 0) {
      String cmd = command.substring(0, colonPos);
      String value = command.substring(colonPos + 1);

      if (cmd == "BUZZER") {
        if (value == "BEEP") {
          playBeep();
        }
        else if (value == "SUCCESS") {
          playSuccess();
        }
        else if (value == "ERROR") {
          playError();
        }
        else if (value.startsWith("TONE:")) {
          // Format: TONE:frequency,duration
          int commaPos = value.indexOf(',', 5);
          if (commaPos > 0) {
            int freq = value.substring(5, commaPos).toInt();
            int dur = value.substring(commaPos + 1).toInt();
            playTone(freq, dur);
          }
        }
        Serial.println("STATUS:BUZZER_PLAYED");
      }
    }
  }
}
```

#### Code Delphi : Contrôle audio

```pascal
procedure TFormMain.PlaySound(const SoundType: string);  
begin  
  if ComPort1.Connected then
    ComPort1.WriteStr('BUZZER:' + SoundType + #13#10);
end;

// Utilisation
procedure TFormMain.ShowAlert(const Message: string);  
begin  
  PlaySound('ERROR');
  ShowMessage(Message);
end;

procedure TFormMain.OperationSuccess;  
begin  
  PlaySound('SUCCESS');
  StatusBar1.Panels[0].Text := 'Opération réussie';
end;
```

## Gestion de plusieurs périphériques

### Architecture multi-dispositifs

Lorsque vous contrôlez plusieurs périphériques, organisez votre code de manière structurée.

#### Code Arduino : Gestionnaire de dispositifs

```cpp
// Configuration des broches
const int LED_PIN = 13;  
const int RELAY1_PIN = 7;  
const int RELAY2_PIN = 8;  
const int MOTOR_IN1 = 9;  
const int MOTOR_IN2 = 10;  
const int MOTOR_ENA = 11;  
const int BUZZER_PIN = 6;  

void setup() {
  Serial.begin(9600);

  // Initialiser toutes les broches
  pinMode(LED_PIN, OUTPUT);
  pinMode(RELAY1_PIN, OUTPUT);
  pinMode(RELAY2_PIN, OUTPUT);
  pinMode(MOTOR_IN1, OUTPUT);
  pinMode(MOTOR_IN2, OUTPUT);
  pinMode(MOTOR_ENA, OUTPUT);
  pinMode(BUZZER_PIN, OUTPUT);

  // État initial : tout éteint
  digitalWrite(LED_PIN, LOW);
  digitalWrite(RELAY1_PIN, LOW);
  digitalWrite(RELAY2_PIN, LOW);
  digitalWrite(MOTOR_IN1, LOW);
  digitalWrite(MOTOR_IN2, LOW);
  analogWrite(MOTOR_ENA, 0);
}

void processCommand(String device, String action, String param) {
  if (device == "LED") {
    handleLED(action);
  }
  else if (device == "RELAY1") {
    handleRelay(RELAY1_PIN, action);
  }
  else if (device == "RELAY2") {
    handleRelay(RELAY2_PIN, action);
  }
  else if (device == "MOTOR") {
    handleMotor(action, param);
  }
  else if (device == "BUZZER") {
    handleBuzzer(action);
  }
  else if (device == "ALL") {
    if (action == "OFF") {
      emergencyStop();
    }
  }
}

void handleLED(String action) {
  if (action == "ON") {
    digitalWrite(LED_PIN, HIGH);
    Serial.println("STATUS:LED_ON");
  }
  else if (action == "OFF") {
    digitalWrite(LED_PIN, LOW);
    Serial.println("STATUS:LED_OFF");
  }
}

void handleRelay(int pin, String action) {
  if (action == "ON") {
    digitalWrite(pin, HIGH);
  }
  else if (action == "OFF") {
    digitalWrite(pin, LOW);
  }
  Serial.print("STATUS:RELAY_");
  Serial.println(action);
}

void handleMotor(String action, String param) {
  if (action == "STOP") {
    digitalWrite(MOTOR_IN1, LOW);
    digitalWrite(MOTOR_IN2, LOW);
    analogWrite(MOTOR_ENA, 0);
  }
  else if (action == "FWD") {
    int speed = param.toInt();
    digitalWrite(MOTOR_IN1, HIGH);
    digitalWrite(MOTOR_IN2, LOW);
    analogWrite(MOTOR_ENA, speed);
  }
  else if (action == "REV") {
    int speed = param.toInt();
    digitalWrite(MOTOR_IN1, LOW);
    digitalWrite(MOTOR_IN2, HIGH);
    analogWrite(MOTOR_ENA, speed);
  }
  Serial.println("STATUS:MOTOR_" + action);
}

void handleBuzzer(String action) {
  if (action == "BEEP") {
    tone(BUZZER_PIN, 1000, 100);
  }
  Serial.println("STATUS:BUZZER_OK");
}

void emergencyStop() {
  // Tout éteindre immédiatement
  digitalWrite(LED_PIN, LOW);
  digitalWrite(RELAY1_PIN, LOW);
  digitalWrite(RELAY2_PIN, LOW);
  digitalWrite(MOTOR_IN1, LOW);
  digitalWrite(MOTOR_IN2, LOW);
  analogWrite(MOTOR_ENA, 0);
  Serial.println("STATUS:EMERGENCY_STOP");
}

void loop() {
  if (Serial.available() > 0) {
    String command = Serial.readStringUntil('\n');
    command.trim();

    // Parser: DEVICE:ACTION:PARAM
    int colon1 = command.indexOf(':');
    if (colon1 > 0) {
      String device = command.substring(0, colon1);
      int colon2 = command.indexOf(':', colon1 + 1);

      String action, param;
      if (colon2 > 0) {
        action = command.substring(colon1 + 1, colon2);
        param = command.substring(colon2 + 1);
      }
      else {
        action = command.substring(colon1 + 1);
        param = "";
      }

      processCommand(device, action, param);
    }
  }
}
```

#### Code Delphi : Contrôleur unifié

```pascal
unit DeviceController;

interface

uses
  System.SysUtils, System.Classes, CPort;

type
  TDeviceController = class
  private
    FComPort: TComPort;
    function SendCommand(const Device, Action: string; const Param: string = ''): Boolean;
  public
    constructor Create(AComPort: TComPort);

    // LED
    procedure LEDOn;
    procedure LEDOff;

    // Relais
    procedure RelayOn(RelayNumber: Integer);
    procedure RelayOff(RelayNumber: Integer);

    // Moteur
    procedure MotorForward(Speed: Integer);
    procedure MotorReverse(Speed: Integer);
    procedure MotorStop;

    // Buzzer
    procedure PlayBeep;

    // Urgence
    procedure EmergencyStop;
  end;

implementation

constructor TDeviceController.Create(AComPort: TComPort);  
begin  
  FComPort := AComPort;
end;

function TDeviceController.SendCommand(const Device, Action, Param: string): Boolean;  
var  
  Command: string;
begin
  Result := False;
  if not FComPort.Connected then Exit;

  if Param <> '' then
    Command := Format('%s:%s:%s', [Device, Action, Param])
  else
    Command := Format('%s:%s', [Device, Action]);

  FComPort.WriteStr(Command + #13#10);
  Result := True;
end;

procedure TDeviceController.LEDOn;  
begin  
  SendCommand('LED', 'ON');
end;

procedure TDeviceController.LEDOff;  
begin  
  SendCommand('LED', 'OFF');
end;

procedure TDeviceController.RelayOn(RelayNumber: Integer);  
begin  
  SendCommand('RELAY' + IntToStr(RelayNumber), 'ON');
end;

procedure TDeviceController.RelayOff(RelayNumber: Integer);  
begin  
  SendCommand('RELAY' + IntToStr(RelayNumber), 'OFF');
end;

procedure TDeviceController.MotorForward(Speed: Integer);  
begin  
  Speed := EnsureRange(Speed, 0, 255);
  SendCommand('MOTOR', 'FWD', IntToStr(Speed));
end;

procedure TDeviceController.MotorReverse(Speed: Integer);  
begin  
  Speed := EnsureRange(Speed, 0, 255);
  SendCommand('MOTOR', 'REV', IntToStr(Speed));
end;

procedure TDeviceController.MotorStop;  
begin  
  SendCommand('MOTOR', 'STOP');
end;

procedure TDeviceController.PlayBeep;  
begin  
  SendCommand('BUZZER', 'BEEP');
end;

procedure TDeviceController.EmergencyStop;  
begin  
  SendCommand('ALL', 'OFF');
end;

end.
```

## Sécurité et bonnes pratiques

### Sécurité électrique

#### Protection contre les surcharges
- Toujours utiliser des fusibles ou disjoncteurs
- Dimensionner correctement les alimentations
- Prévoir des marges de sécurité (20-30%)

#### Isolation
- Utiliser des relais ou optocoupleurs pour isoler les circuits haute tension
- Ne jamais mélanger circuits de contrôle et de puissance
- Respecter les distances de sécurité

#### Câblage
- Utiliser des fils de section adaptée au courant
- Bien isoler toutes les connexions
- Utiliser des connecteurs fiables
- Identifier clairement tous les câbles

### Sécurité logicielle

#### État sécurisé par défaut
```cpp
void setup() {
  // Tout éteindre au démarrage
  digitalWrite(RELAY_PIN, LOW);
  digitalWrite(MOTOR_PIN, LOW);

  // Initialiser en position sûre
  servo.write(90);
}
```

#### Watchdog
Implémenter un système de surveillance :

```cpp
unsigned long lastCommandTime = 0;  
const unsigned long TIMEOUT = 5000; // 5 secondes  

void loop() {
  // Vérifier le timeout
  if (millis() - lastCommandTime > TIMEOUT) {
    emergencyStop();
  }

  if (Serial.available() > 0) {
    lastCommandTime = millis();
    // Traiter la commande...
  }
}
```

#### Validation des commandes
```cpp
void handleMotor(String action, String param) {
  int speed = param.toInt();

  // Validation stricte
  if (speed < 0 || speed > 255) {
    Serial.println("ERROR:INVALID_SPEED");
    return;
  }

  // Limiter les changements brusques
  static int currentSpeed = 0;
  int maxChange = 50;

  if (abs(speed - currentSpeed) > maxChange) {
    speed = currentSpeed + (speed > currentSpeed ? maxChange : -maxChange);
  }

  currentSpeed = speed;
  analogWrite(MOTOR_ENA, speed);
}
```

### Bonnes pratiques Delphi

#### Gestion des erreurs complète

```pascal
procedure TFormMain.SafeControlDevice(const Command: string);  
var  
  RetryCount: Integer;
  MaxRetries: Integer;
begin
  MaxRetries := 3;
  RetryCount := 0;

  while RetryCount < MaxRetries do
  begin
    try
      if not ComPort1.Connected then
        raise Exception.Create('Port série non connecté');

      ComPort1.WriteStr(Command + #13#10);

      // Attendre confirmation (avec timeout)
      if WaitForConfirmation(1000) then
      begin
        LogMessage('Commande envoyée: ' + Command);
        Break;
      end
      else
        raise Exception.Create('Pas de confirmation reçue');

    except
      on E: Exception do
      begin
        Inc(RetryCount);
        if RetryCount >= MaxRetries then
        begin
          LogError('Échec après ' + IntToStr(MaxRetries) + ' tentatives: ' + E.Message);
          ShowMessage('Impossible de communiquer avec le dispositif');
        end
        else
          Sleep(500);
      end;
    end;
  end;
end;
```

#### Bouton d'arrêt d'urgence

```pascal
procedure TFormMain.ButtonEmergencyStopClick(Sender: TObject);  
begin  
  try
    DeviceController.EmergencyStop;

    // Désactiver tous les contrôles
    DisableAllControls;

    // Affichage visuel
    PanelStatus.Color := clRed;
    LabelStatus.Caption := 'ARRÊT D''URGENCE ACTIVÉ';

    // Son d'alerte
    Beep;

    LogMessage('Arrêt d''urgence activé par l''utilisateur');
  except
    on E: Exception do
      ShowMessage('ERREUR CRITIQUE lors de l''arrêt d''urgence: ' + E.Message);
  end;
end;
```

## Projet complet : Système domotique simple

### Fonctionnalités
- Contrôle de 2 lampes (via relais)
- Contrôle d'un ventilateur (moteur DC avec vitesse variable)
- Affichage température et humidité sur LCD
- Buzzer pour les alertes
- Interface Delphi complète

### Architecture
```
[Capteur DHT22] --\
[Relais x2]      ---> [Arduino Mega] <--USB--> [Application Delphi]
[Moteur DC]      --/       |
[LCD I2C]        ----------/
[Buzzer]         ----------/
```

Cette architecture vous permet de créer un système complet de contrôle domotique avec une interface utilisateur professionnelle développée en Delphi.

## Conclusion

Le contrôle de périphériques externes est au cœur des applications IoT. Avec Delphi et Arduino/Raspberry Pi, vous disposez d'une solution complète pour :

- Contrôler des dispositifs simples (LED, relais)
- Piloter des moteurs avec précision
- Afficher des informations localement
- Créer des feedbacks sonores
- Gérer plusieurs périphériques simultanément

**Points clés à retenir :**

1. **Électronique** : comprendre les bases (tension, courant, puissance)
2. **Protection** : toujours protéger l'Arduino (transistors, relais, drivers)
3. **PWM** : pour contrôler vitesse et intensité
4. **Communication** : protocole clair et validation stricte
5. **Sécurité** : état sûr par défaut, arrêt d'urgence, validation
6. **Organisation** : code structuré pour gérer plusieurs dispositifs
7. **Robustesse** : gestion d'erreurs, retry, timeout

Dans la section suivante, nous explorerons les protocoles IoT avancés comme MQTT et CoAP pour créer des systèmes encore plus sophistiqués et distribués.

⏭️ [Protocoles IoT (MQTT, CoAP)](/21-delphi-et-liot/05-protocoles-iot.md)
