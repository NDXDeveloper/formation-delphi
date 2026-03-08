🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.2 Communication Bluetooth / série

## Introduction

La communication entre votre application Delphi et les dispositifs IoT peut s'effectuer de plusieurs manières. Dans cette section, nous allons explorer deux technologies fondamentales : la communication série (port COM) et le Bluetooth. Ces deux méthodes sont largement utilisées pour connecter des capteurs, des microcontrôleurs et d'autres périphériques à votre application.

## Communication série : Les fondamentaux

### Qu'est-ce que la communication série ?

La communication série est une méthode de transmission de données où les bits d'information sont envoyés l'un après l'autre sur un seul canal de communication. C'est l'une des méthodes les plus anciennes et les plus fiables pour connecter des dispositifs électroniques.

### Types de connexions série

#### RS-232 (Port série classique)

Le RS-232 est le standard historique de communication série :
- **Connecteur** : DB-9 (9 broches) ou DB-25 (25 broches)
- **Distance** : jusqu'à 15 mètres
- **Vitesse** : généralement de 9600 à 115200 bauds
- **Utilisation** : équipements industriels, anciens périphériques
- **Tension** : ±12V

Aujourd'hui, les ordinateurs modernes n'ont souvent plus de port RS-232 physique, mais on utilise des adaptateurs USB-Série.

#### USB-Série (Virtual COM Port)

La plupart des dispositifs modernes utilisent l'USB :
- **Arduino** : se connecte via USB et crée un port COM virtuel
- **ESP32/ESP8266** : également via USB
- **Modules FTDI** : convertisseurs USB vers série dédiés
- **Pilotes** : installation de pilotes souvent nécessaire (CH340, CP2102, FTDI)

L'avantage : vous utilisez le même code que pour le RS-232 classique.

#### RS-485

Standard industriel pour les longues distances :
- **Distance** : jusqu'à 1200 mètres
- **Multi-dispositifs** : plusieurs appareils sur le même bus
- **Robustesse** : résistant aux interférences électromagnétiques
- **Utilisation** : industries, bâtiments intelligents, systèmes SCADA

### Paramètres de la communication série

Pour établir une communication série, vous devez configurer plusieurs paramètres :

#### 1. Port COM
- **Windows** : COM1, COM2, COM3, etc.
- **macOS/Linux** : /dev/ttyUSB0, /dev/ttyACM0, etc.

#### 2. Vitesse (Baud Rate)
La vitesse de transmission en bits par seconde :
- **9600 bauds** : vitesse standard, fiable
- **19200 bauds** : deux fois plus rapide
- **38400, 57600, 115200 bauds** : vitesses élevées
- **Note** : les deux dispositifs doivent utiliser la même vitesse

#### 3. Bits de données
Nombre de bits pour chaque caractère :
- **8 bits** : standard moderne (recommandé)
- **7 bits** : ancien standard ASCII

#### 4. Parité
Contrôle d'erreur simple :
- **None (Aucune)** : pas de vérification (le plus courant)
- **Even (Paire)** : vérification de parité paire
- **Odd (Impaire)** : vérification de parité impaire

#### 5. Bits d'arrêt
Bits de synchronisation :
- **1 bit** : standard
- **2 bits** : pour des vitesses lentes ou des lignes bruitées

#### 6. Contrôle de flux
Gestion du flux de données :
- **None** : pas de contrôle (le plus simple)
- **Hardware (RTS/CTS)** : utilise des signaux matériels
- **Software (XON/XOFF)** : utilise des caractères de contrôle

**Configuration typique** : 9600,N,8,1 signifie 9600 bauds, pas de parité, 8 bits de données, 1 bit d'arrêt.

## Communication série avec Delphi

### Bibliothèques disponibles

Il existe plusieurs options pour la communication série en Delphi :

#### 1. TComPort (AsyncPro)
Bibliothèque open source populaire :
- Facile à utiliser
- Bien documentée
- Compatible avec de nombreuses versions de Delphi
- Installation via composant

#### 2. Composants tiers commerciaux
- **TMS Async32** : suite complète de communications
- **Synapser Serial** : bibliothèque légère
- **ComPort Library** : gratuite et efficace

#### 3. Solution native
Utilisation directe de l'API Windows ou des bibliothèques système.

### Installation de TComPort (exemple)

Pour utiliser TComPort :

1. Téléchargez la bibliothèque depuis SourceForge ou GitHub
2. Installez le package dans Delphi
3. Les composants apparaissent dans la palette d'outils
4. Déposez un composant `TComPort` sur votre formulaire

### Exemple basique : Lecture de données série

```pascal
unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CPort;

type
  TForm1 = class(TForm)
    ComPort1: TComPort;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ComPort1RxChar(Sender: TObject; Count: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Ouvrir le port série
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  try
    // Configuration du port
    ComPort1.Port := 'COM3';           // Port à utiliser
    ComPort1.BaudRate := br9600;        // Vitesse 9600 bauds
    ComPort1.DataBits := dbEight;       // 8 bits de données
    ComPort1.Parity.Bits := prNone;     // Pas de parité
    ComPort1.StopBits := sbOneStopBit;  // 1 bit d'arrêt
    ComPort1.FlowControl.FlowControl := fcNone; // Pas de contrôle de flux

    // Ouverture du port
    ComPort1.Open;

    if ComPort1.Connected then
      Memo1.Lines.Add('Port série ouvert avec succès')
    else
      Memo1.Lines.Add('Échec de l''ouverture du port');
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

// Fermer le port série
procedure TForm1.Button2Click(Sender: TObject);  
begin  
  if ComPort1.Connected then
  begin
    ComPort1.Close;
    Memo1.Lines.Add('Port série fermé');
  end;
end;

// Réception de données
procedure TForm1.ComPort1RxChar(Sender: TObject; Count: Integer);  
var  
  Str: string;
begin
  // Lire les données reçues
  ComPort1.ReadStr(Str, Count);

  // Afficher dans le Memo
  Memo1.Lines.Add('Reçu : ' + Str);
end;

end.
```

### Envoi de données série

Pour envoyer des données au dispositif :

```pascal
procedure TForm1.ButtonSendClick(Sender: TObject);  
var  
  DataToSend: string;
begin
  if ComPort1.Connected then
  begin
    DataToSend := EditMessage.Text + #13#10; // Ajouter retour à la ligne
    ComPort1.WriteStr(DataToSend);
    Memo1.Lines.Add('Envoyé : ' + DataToSend);
  end
  else
    ShowMessage('Le port série n''est pas ouvert');
end;
```

### Détection automatique des ports disponibles

```pascal
procedure TForm1.DetectPortsClick(Sender: TObject);  
var  
  I: Integer;
begin
  ComboBoxPorts.Clear;

  // Parcourir les ports de COM1 à COM20
  for I := 1 to 20 do
  begin
    try
      ComPort1.Port := 'COM' + IntToStr(I);
      ComPort1.Open;

      if ComPort1.Connected then
      begin
        ComboBoxPorts.Items.Add('COM' + IntToStr(I));
        ComPort1.Close;
      end;
    except
      // Port non disponible, on continue
    end;
  end;

  if ComboBoxPorts.Items.Count > 0 then
    ComboBoxPorts.ItemIndex := 0;
end;
```

### Communication avec un Arduino (exemple)

Code Arduino simple qui envoie la température :

```cpp
void setup() {
  Serial.begin(9600);
}

void loop() {
  float temperature = analogRead(A0) * 0.48828125; // Conversion pour LM35
  Serial.print("TEMP:");
  Serial.println(temperature);
  delay(1000); // Envoi chaque seconde
}
```

Code Delphi pour recevoir ces données :

```pascal
procedure TForm1.ComPort1RxChar(Sender: TObject; Count: Integer);  
var  
  ReceivedData: string;
  TempValue: Double;
begin
  ComPort1.ReadStr(ReceivedData, Count);

  // Parser les données reçues
  if Pos('TEMP:', ReceivedData) > 0 then
  begin
    Delete(ReceivedData, 1, 5); // Supprimer "TEMP:"
    ReceivedData := Trim(ReceivedData);

    if TryStrToFloat(ReceivedData, TempValue) then
    begin
      LabelTemperature.Caption := Format('Température : %.1f °C', [TempValue]);
    end;
  end;
end;
```

## Communication Bluetooth

### Introduction au Bluetooth

Le Bluetooth est une technologie sans fil courte portée qui permet de connecter des dispositifs électroniques. Il existe deux principales versions :

#### Bluetooth Classic (BR/EDR)

Caractéristiques :
- **Portée** : 10 à 100 mètres selon la classe
- **Débit** : jusqu'à 3 Mbps
- **Consommation** : moyenne à élevée
- **Utilisation** : streaming audio, transfert de fichiers, connexions série SPP
- **Appairage** : nécessite souvent un code PIN

#### Bluetooth Low Energy (BLE)

Caractéristiques :
- **Portée** : 10 à 50 mètres
- **Débit** : jusqu'à 1 Mbps (suffisant pour l'IoT)
- **Consommation** : très faible (idéal pour capteurs sur batterie)
- **Utilisation** : capteurs, montres connectées, beacons
- **Appairage** : simplifié ou sans appairage

Pour l'IoT, le BLE est généralement préféré car il consomme beaucoup moins d'énergie.

### Bluetooth Serial Port Profile (SPP)

Le SPP est un profil Bluetooth qui émule un port série :
- Fonctionne comme un câble série sans fil
- Compatible avec les modules HC-05, HC-06
- Facilite la migration d'applications série vers Bluetooth
- Utilise le Bluetooth Classic

### Composants Delphi pour le Bluetooth

Delphi offre des composants natifs pour le Bluetooth depuis les versions récentes :

#### TBluetoothLE

Pour le Bluetooth Low Energy :
- Inclus dans la VCL et FireMonkey
- Multi-plateforme (Windows, macOS, iOS, Android)
- Scan des dispositifs BLE
- Connexion et communication

#### TBluetooth (Classic)

Pour le Bluetooth Classic :
- Disponible sur certaines plateformes
- Moins utilisé pour l'IoT moderne

### Exemple : Scanner les dispositifs BLE

```pascal
unit Unit1;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, System.Bluetooth, System.Bluetooth.Components;

type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    BluetoothLE1: TBluetoothLE;
    procedure Button1Click(Sender: TObject);
    procedure BluetoothLE1EndDiscoverDevices(const Sender: TObject;
      const ADeviceList: TBluetoothLEDeviceList);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// Démarrer le scan
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ListBox1.Clear;
  ListBox1.Items.Add('Scan en cours...');

  // Lancer la découverte des dispositifs BLE (10 secondes)
  BluetoothLE1.DiscoverDevices(10000);
end;

// Dispositifs trouvés
procedure TForm1.BluetoothLE1EndDiscoverDevices(const Sender: TObject;
  const ADeviceList: TBluetoothLEDeviceList);
var
  Device: TBluetoothLEDevice;
begin
  ListBox1.Clear;

  if ADeviceList.Count = 0 then
  begin
    ListBox1.Items.Add('Aucun dispositif trouvé');
    Exit;
  end;

  // Afficher les dispositifs trouvés
  for Device in ADeviceList do
  begin
    ListBox1.Items.Add(Device.DeviceName + ' - ' + Device.Identifier);
  end;
end;

end.
```

### Connexion à un dispositif BLE

```pascal
procedure TForm1.ConnectToDevice(Device: TBluetoothLEDevice);  
var  
  Service: TBluetoothGattService;
  Characteristic: TBluetoothGattCharacteristic;
begin
  try
    // Récupérer les services du dispositif
    for Service in Device.Services do
    begin
      MemoLog.Lines.Add('Service : ' + Service.UUID.ToString);

      // Récupérer les caractéristiques
      for Characteristic in Service.Characteristics do
      begin
        MemoLog.Lines.Add('  Caractéristique : ' + Characteristic.UUID.ToString);

        // Si c'est une caractéristique de lecture
        if TBluetoothProperty.Read in Characteristic.Properties then
        begin
          // Lire la valeur
          Device.ReadCharacteristic(Characteristic);
        end;

        // Si on peut recevoir des notifications
        if TBluetoothProperty.Notify in Characteristic.Properties then
        begin
          // S'abonner aux notifications
          Device.SubscribeToCharacteristic(Characteristic);
        end;
      end;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

### Lecture de données BLE

```pascal
procedure TForm1.BluetoothLE1CharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; AGattStatus: TBluetoothGattStatus);
var
  Data: TBytes;
  Value: Integer;
begin
  if AGattStatus = TBluetoothGattStatus.Success then
  begin
    Data := ACharacteristic.Value;

    // Exemple : conversion en entier (selon votre protocole)
    if Length(Data) >= 2 then
    begin
      Value := (Data[0] shl 8) or Data[1];
      LabelValue.Caption := 'Valeur : ' + IntToStr(Value);
    end;
  end;
end;
```

### Écriture de données BLE

```pascal
procedure TForm1.SendCommandClick(Sender: TObject);  
var  
  Device: TBluetoothLEDevice;
  Characteristic: TBluetoothGattCharacteristic;
  Data: TBytes;
begin
  // Supposons que vous avez déjà une référence au dispositif et à la caractéristique

  // Préparer les données à envoyer
  SetLength(Data, 1);
  Data[0] := $01; // Commande : allumer LED

  // Écrire les données
  Device.WriteCharacteristic(Characteristic, Data);
end;
```

## Bluetooth SPP avec modules HC-05/HC-06

Pour utiliser des modules Bluetooth classiques comme le HC-05 ou HC-06 :

### Configuration du module

Les modules HC-05/HC-06 se configurent via des commandes AT :

```
AT                  // Test de communication  
AT+NAME=MonDevice   // Changer le nom  
AT+BAUD4            // Définir la vitesse à 9600 bauds  
AT+PIN1234          // Définir le code PIN  
```

### Connexion depuis Delphi

Une fois le module appairé avec votre ordinateur :

1. Windows crée deux ports COM virtuels (entrant et sortant)
2. Utilisez le port COM sortant dans votre application
3. La communication fonctionne exactement comme un port série classique

```pascal
procedure TForm1.ConnectBluetoothSPP;  
begin  
  // Le module HC-05 apparaît comme un port COM (ex: COM7)
  ComPort1.Port := 'COM7';
  ComPort1.BaudRate := br9600;
  ComPort1.Open;

  // Ensuite, utilisez comme un port série normal
end;
```

## Comparaison : Série vs Bluetooth

### Quand utiliser le port série (USB/RS-232) ?

**Avantages :**
- Connexion stable et fiable
- Pas d'appairage nécessaire
- Pas d'interférences
- Débit garanti
- Idéal pour développement et tests

**Inconvénients :**
- Nécessite un câble physique
- Distance limitée
- Moins pratique pour les dispositifs mobiles

**Cas d'usage :**
- Développement et prototypage
- Applications fixes (stations de contrôle)
- Environnements industriels bruyants (avec RS-485)
- Transfert de grandes quantités de données

### Quand utiliser le Bluetooth ?

**Avantages :**
- Sans fil (pas de câble)
- Portée raisonnable (10-50m)
- BLE : très faible consommation
- Pratique pour applications mobiles

**Inconvénients :**
- Appairage parfois complexe
- Interférences possibles
- Latence variable
- Consommation d'énergie (Classic)

**Cas d'usage :**
- Capteurs sur batterie (BLE)
- Applications mobiles
- Dispositifs portables
- Quand le câble est contraignant

## Conseils pratiques

### Gestion des erreurs de communication

Toujours gérer les erreurs possibles :

```pascal
procedure TForm1.SafeSerialCommunication;  
var  
  MaxRetries: Integer;
  RetryCount: Integer;
begin
  MaxRetries := 3;
  RetryCount := 0;

  while RetryCount < MaxRetries do
  begin
    try
      ComPort1.Open;

      if ComPort1.Connected then
      begin
        // Succès
        Break;
      end;
    except
      on E: Exception do
      begin
        Inc(RetryCount);
        Sleep(1000); // Attendre 1 seconde avant de réessayer

        if RetryCount >= MaxRetries then
          raise Exception.Create('Impossible d''ouvrir le port après ' +
                                 IntToStr(MaxRetries) + ' tentatives');
      end;
    end;
  end;
end;
```

### Utilisation de threads pour la réception

Pour éviter de bloquer l'interface utilisateur :

```pascal
type
  TSerialReadThread = class(TThread)
  private
    FComPort: TComPort;
    FOnDataReceived: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AComPort: TComPort);
    property OnDataReceived: TNotifyEvent read FOnDataReceived write FOnDataReceived;
  end;

procedure TSerialReadThread.Execute;  
var  
  Buffer: string;
begin
  while not Terminated do
  begin
    if FComPort.Connected then
    begin
      if FComPort.InputCount > 0 then
      begin
        FComPort.ReadStr(Buffer, FComPort.InputCount);

        // Traiter les données reçues
        if Assigned(FOnDataReceived) then
          Synchronize(procedure
          begin
            // Mise à jour de l'interface utilisateur
          end);
      end;
    end;

    Sleep(50); // Petite pause pour ne pas surcharger le CPU
  end;
end;
```

### Déconnexion propre

Toujours fermer les ports avant de quitter :

```pascal
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);  
begin  
  if Assigned(ComPort1) and ComPort1.Connected then
  begin
    try
      ComPort1.Close;
    except
      // Ignorer les erreurs lors de la fermeture
    end;
  end;
end;
```

### Timeout de lecture

Définir un timeout pour éviter de bloquer indéfiniment :

```pascal
ComPort1.Timeouts.ReadTotalMultiplier := 0;  
ComPort1.Timeouts.ReadTotalConstant := 1000; // 1 seconde  
ComPort1.Timeouts.ReadInterval := 100;  
```

### Buffer de réception

Pour des données volumineuses, ajustez la taille du buffer :

```pascal
ComPort1.Buffer.InputSize := 8192;  // 8 Ko  
ComPort1.Buffer.OutputSize := 8192;  
```

## Protocoles de communication personnalisés

### Protocole simple basé sur des délimiteurs

Utiliser des caractères spéciaux pour délimiter les messages :

```
<START>TEMP:23.5<END>
<START>HUM:65<END>
```

Code de parsing :

```pascal
procedure ParseMessage(const Data: string);  
var  
  StartPos, EndPos: Integer;
  Message: string;
begin
  StartPos := Pos('<START>', Data);
  EndPos := Pos('<END>', Data);

  if (StartPos > 0) and (EndPos > 0) and (EndPos > StartPos) then
  begin
    Message := Copy(Data, StartPos + 7, EndPos - StartPos - 7);
    // Traiter le message
    ProcessCommand(Message);
  end;
end;
```

### Protocole binaire compact

Pour optimiser la bande passante :

```pascal
// Structure du message : [Header][Type][Longueur][Données][Checksum]
procedure SendBinaryMessage(MessageType: Byte; const Data: TBytes);  
var  
  Packet: TBytes;
  Checksum: Byte;
  I: Integer;
begin
  SetLength(Packet, 4 + Length(Data));

  Packet[0] := $AA;           // Header
  Packet[1] := MessageType;   // Type de message
  Packet[2] := Length(Data);  // Longueur des données

  // Copier les données
  for I := 0 to Length(Data) - 1 do
    Packet[3 + I] := Data[I];

  // Calculer checksum simple
  Checksum := 0;
  for I := 0 to Length(Packet) - 2 do
    Checksum := Checksum xor Packet[I];

  Packet[Length(Packet) - 1] := Checksum;

  // Envoyer
  ComPort1.Write(Packet[0], Length(Packet));
end;
```

## Débogage des communications

### Moniteur série

Pour déboguer, affichez toutes les données échangées :

```pascal
procedure TForm1.LogCommunication(const Direction: string; const Data: string);  
var  
  Timestamp: string;
begin
  Timestamp := FormatDateTime('hh:nn:ss.zzz', Now);
  MemoDebug.Lines.Add(Format('[%s] %s: %s', [Timestamp, Direction, Data]));
end;

// Utilisation
procedure TForm1.ComPort1RxChar(Sender: TObject; Count: Integer);  
var  
  Data: string;
begin
  ComPort1.ReadStr(Data, Count);
  LogCommunication('RX', Data); // RX = Réception
  // Traiter les données...
end;

procedure TForm1.SendData(const Data: string);  
begin  
  ComPort1.WriteStr(Data);
  LogCommunication('TX', Data); // TX = Transmission
end;
```

### Affichage hexadécimal

Pour les protocoles binaires :

```pascal
function BytesToHex(const Data: TBytes): string;  
var  
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Data) - 1 do
    Result := Result + IntToHex(Data[I], 2) + ' ';
end;
```

## Conclusion

La communication série et Bluetooth sont deux piliers essentiels de l'IoT avec Delphi. Le port série offre une fiabilité maximale et est idéal pour le développement, tandis que le Bluetooth (surtout BLE) offre la flexibilité du sans fil avec une faible consommation.

**Points clés à retenir :**

1. **Port série** : simple, fiable, parfait pour débuter
2. **Configuration** : vitesse, parité et bits doivent correspondre des deux côtés
3. **Bluetooth Classic** : pour streaming et transferts volumineux
4. **BLE** : pour capteurs sur batterie et applications mobiles
5. **Gestion d'erreurs** : toujours prévoir les déconnexions et timeouts
6. **Threads** : pour ne pas bloquer l'interface utilisateur
7. **Protocoles** : définir un format de message clair

Dans la prochaine section, nous verrons comment utiliser ces techniques de communication pour interfacer Delphi avec Arduino et Raspberry Pi, ouvrant ainsi la porte à d'innombrables projets IoT passionnants !

⏭️ [Intégration avec Arduino / Raspberry Pi](/21-delphi-et-liot/03-integration-avec-arduino-raspberry-pi.md)
