# 21.2 Communication Bluetooth / série

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

La communication série et Bluetooth représente l'une des méthodes les plus courantes pour connecter des dispositifs IoT à vos applications Delphi. Que vous souhaitiez interagir avec un Arduino, un Raspberry Pi, ou tout autre appareil intelligent, maîtriser ces protocoles est essentiel dans votre boîte à outils IoT.

## Communication série (Serial)

### Qu'est-ce que la communication série ?

La communication série est une méthode de transmission de données où les bits sont envoyés séquentiellement sur un seul canal. C'est l'une des formes de communication les plus anciennes et les plus fiables entre un ordinateur et des périphériques.

### Ports série courants

- **COM (Windows)** : COM1, COM2, COM3, etc.
- **TTY (Linux/macOS)** : /dev/ttyUSB0, /dev/ttyACM0, etc.
- **Port USB-Série** : Adaptateurs permettant de connecter des périphériques série via USB

### Composants Delphi pour la communication série

Delphi ne dispose pas de composants natifs pour la communication série, mais plusieurs bibliothèques tierces sont disponibles :

1. **Composant TComPort** : Une bibliothèque open source populaire
2. **Synaser** : Bibliothèque cross-platform pour la communication série
3. **Async Professional** : Composant commercial avec support avancé

### Exemple de communication série avec TComPort

Voici comment utiliser le composant TComPort pour établir une communication série :

```delphi
uses
  ComPort;

var
  ComPort: TComPort;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComPort := TComPort.Create(Self);
  ComPort.Port := 'COM3';  // Changez selon votre configuration
  ComPort.BaudRate := br9600;
  ComPort.DataBits := dbEight;
  ComPort.StopBits := sbOneStopBit;
  ComPort.Parity.Bits := prNone;
  ComPort.FlowControl.FlowControl := fcNone;
  ComPort.OnRxChar := ComPortRxChar;

  try
    ComPort.Open;
    Memo1.Lines.Add('Port série ouvert');
  except
    on E: Exception do
      Memo1.Lines.Add('Erreur: ' + E.Message);
  end;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  ReceivedData: string;
begin
  ComPort.ReadStr(ReceivedData, Count);
  Memo1.Lines.Add('Reçu: ' + ReceivedData);
end;

procedure TForm1.SendButtonClick(Sender: TObject);
begin
  if ComPort.Connected then
  begin
    ComPort.WriteStr(EditMessage.Text + #13#10);  // Ajout d'un retour chariot
    Memo1.Lines.Add('Envoyé: ' + EditMessage.Text);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ComPort.Connected then
    ComPort.Close;
  ComPort.Free;
end;
```

### Installation du composant TComPort

1. Téléchargez TComPort depuis [GitHub](https://github.com/CWBudde/ComPort-Library) ou [SourceForge](https://sourceforge.net/projects/comport/)
2. Installez le package dans Delphi via Composants > Installer des packages
3. Sélectionnez le fichier .dpk du composant TComPort
4. Après installation, le composant apparaîtra dans la palette d'outils

## Communication Bluetooth

### Types de Bluetooth

Il existe deux types principaux de Bluetooth pour l'IoT :

1. **Bluetooth Classic** : Pour le transfert de données volumineux (audio, fichiers)
2. **Bluetooth Low Energy (BLE)** : Pour les appareils à faible consommation d'énergie (la plupart des dispositifs IoT modernes)

### Composants Delphi pour Bluetooth

Delphi 12 Athens propose des composants natifs pour la communication Bluetooth :

1. **Bluetooth Framework** : Intégré à Delphi à partir de la version 10.4
2. **Composants FireMonkey** : TBluetooth, TBluetoothLE, TBluetoothLEDevice
3. **Bibliothèques tierces** : Diverses solutions disponibles sur GetIt Package Manager

### Exemple de communication Bluetooth Classic

```delphi
uses
  System.Bluetooth;

var
  BluetoothManager: TBluetoothManager;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BluetoothManager := TBluetoothManager.Current;

  if BluetoothManager <> nil then
  begin
    BluetoothManager.OnDiscoveryEnd := BluetoothDiscoveryEnd;
    ListBox1.Items.Clear;
    Memo1.Lines.Add('Recherche de périphériques Bluetooth...');
    BluetoothManager.StartDiscovery(10000);  // 10 secondes de recherche
  end
  else
    Memo1.Lines.Add('Bluetooth non disponible sur cet appareil');
end;

procedure TForm1.BluetoothDiscoveryEnd(const Sender: TObject);
var
  Device: TBluetoothDevice;
begin
  Memo1.Lines.Add('Fin de la recherche. Appareils trouvés:');

  for Device in BluetoothManager.LastDiscoveredDevices do
  begin
    ListBox1.Items.AddObject(Device.DeviceName + ' (' + Device.Address + ')', Device);
    Memo1.Lines.Add('- ' + Device.DeviceName + ' (' + Device.Address + ')');
  end;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  SelectedDevice: TBluetoothDevice;
  Socket: TBluetoothSocket;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    SelectedDevice := TBluetoothDevice(ListBox1.Items.Objects[ListBox1.ItemIndex]);

    try
      Socket := SelectedDevice.CreateClientSocket(StringToGUID('{00001101-0000-1000-8000-00805F9B34FB}'), False);
      if Socket <> nil then
      begin
        Socket.Connect;
        Memo1.Lines.Add('Connecté à ' + SelectedDevice.DeviceName);

        // Envoi de données
        Socket.SendData(TEncoding.UTF8.GetBytes('Hello from Delphi!'));

        // Lecture de la réponse
        // ... (code de lecture)

        Socket.Close;
      end;
    except
      on E: Exception do
        Memo1.Lines.Add('Erreur de connexion: ' + E.Message);
    end;
  end;
end;
```

> **Note** : Cet exemple nécessite Delphi 10.4 ou supérieur.

### Exemple de communication Bluetooth Low Energy (BLE)

```delphi
uses
  System.Bluetooth, System.Bluetooth.Components;

var
  BluetoothLE: TBluetoothLE;
  LEDevice: TBluetoothLEDevice;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BluetoothLE := TBluetoothLE.Current;

  if BluetoothLE <> nil then
  begin
    BluetoothLE.OnDiscoveredDevice := LEDiscoveredDevice;
    Memo1.Lines.Add('Recherche de périphériques BLE...');
    BluetoothLE.Enabled := True;
    BluetoothLE.ScanMode := TScanMode.smLowPower;
    BluetoothLE.StartScan;
  end
  else
    Memo1.Lines.Add('Bluetooth LE non disponible sur cet appareil');
end;

procedure TForm1.LEDiscoveredDevice(const Sender: TObject; const ADevice: TBluetoothLEDevice);
begin
  TThread.Synchronize(nil, procedure
  begin
    if not ListBox1.Items.Contains(ADevice.Identifier) then
      ListBox1.Items.AddObject(ADevice.DeviceName + ' (' + ADevice.Identifier + ')', ADevice);
  end);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  SelectedDevice: TBluetoothLEDevice;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    BluetoothLE.StopScan;

    SelectedDevice := TBluetoothLEDevice(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    LEDevice := SelectedDevice;

    LEDevice.OnConnected := LEDeviceConnected;
    LEDevice.OnServicesDiscovered := LEServicesDiscovered;
    LEDevice.OnCharacteristicRead := LECharacteristicRead;

    Memo1.Lines.Add('Connexion à ' + LEDevice.DeviceName + '...');
    LEDevice.Connect;
  end;
end;

procedure TForm1.LEDeviceConnected(const Sender: TObject; const ASuccess: Boolean);
begin
  TThread.Synchronize(nil, procedure
  begin
    if ASuccess then
    begin
      Memo1.Lines.Add('Connecté à ' + LEDevice.DeviceName);
      Memo1.Lines.Add('Découverte des services...');
      LEDevice.DiscoverServices;
    end
    else
      Memo1.Lines.Add('Échec de la connexion');
  end);
end;

procedure TForm1.LEServicesDiscovered(const Sender: TObject; const AServiceList: TBluetoothGattServiceList);
var
  Service: TBluetoothGattService;
begin
  TThread.Synchronize(nil, procedure
  begin
    Memo1.Lines.Add('Services découverts:');

    for Service in AServiceList do
      Memo1.Lines.Add('- ' + Service.UUID);

    // Exemple: Lecture d'une caractéristique spécifique
    // (à adapter selon votre périphérique BLE)
    LEDevice.ReadCharacteristic(
      StringToGUID('{00002a00-0000-1000-8000-00805f9b34fb}'),  // Service UUID
      StringToGUID('{00002a01-0000-1000-8000-00805f9b34fb}')   // Characteristic UUID
    );
  end);
end;

procedure TForm1.LECharacteristicRead(const Sender: TObject;
  const ACharacteristic: TBluetoothGattCharacteristic; const AGattStatus: TBluetoothGattStatus);
var
  Value: TBytes;
begin
  TThread.Synchronize(nil, procedure
  begin
    if AGattStatus = TBluetoothGattStatus.Success then
    begin
      Value := ACharacteristic.Value;
      Memo1.Lines.Add('Valeur lue: ' + TEncoding.UTF8.GetString(Value));
    end
    else
      Memo1.Lines.Add('Erreur lors de la lecture');
  end);
end;
```

> **Note** : Cet exemple nécessite Delphi 11 ou supérieur pour toutes les fonctionnalités BLE.

## Projet pratique : Contrôle d'un Arduino par Bluetooth

### Matériel nécessaire
- Arduino (Uno, Nano, etc.)
- Module Bluetooth HC-05 ou HC-06
- LEDs, résistances, et breadboard

### Étape 1 : Code Arduino
Voici un exemple de code Arduino pour contrôler une LED via Bluetooth :

```cpp
char commande;

void setup() {
  Serial.begin(9600);  // Communication avec le module Bluetooth
  pinMode(13, OUTPUT); // LED intégrée sur la plupart des Arduino
}

void loop() {
  if (Serial.available() > 0) {
    commande = Serial.read();

    if (commande == '1') {
      digitalWrite(13, HIGH);
      Serial.println("LED allumée");
    }
    else if (commande == '0') {
      digitalWrite(13, LOW);
      Serial.println("LED éteinte");
    }
  }
}
```

### Étape 2 : Interface Delphi

Créez une interface Delphi simple avec :
- Un ComboBox pour sélectionner le port
- Deux boutons : "Allumer" et "Éteindre"
- Un bouton "Connecter"
- Un Memo pour afficher les messages

### Étape 3 : Code Delphi

```delphi
uses
  ComPort;

procedure TForm1.ConnectButtonClick(Sender: TObject);
begin
  if not ComPort1.Connected then
  begin
    ComPort1.Port := ComboBox1.Text;
    ComPort1.BaudRate := br9600;

    try
      ComPort1.Open;
      Memo1.Lines.Add('Connecté au port ' + ComboBox1.Text);
      ConnectButton.Caption := 'Déconnecter';
      LedOnButton.Enabled := True;
      LedOffButton.Enabled := True;
    except
      on E: Exception do
        Memo1.Lines.Add('Erreur: ' + E.Message);
    end;
  end
  else
  begin
    ComPort1.Close;
    Memo1.Lines.Add('Déconnecté');
    ConnectButton.Caption := 'Connecter';
    LedOnButton.Enabled := False;
    LedOffButton.Enabled := False;
  end;
end;

procedure TForm1.LedOnButtonClick(Sender: TObject);
begin
  if ComPort1.Connected then
  begin
    ComPort1.WriteStr('1');
    Memo1.Lines.Add('Commande envoyée: Allumer LED');
  end;
end;

procedure TForm1.LedOffButtonClick(Sender: TObject);
begin
  if ComPort1.Connected then
  begin
    ComPort1.WriteStr('0');
    Memo1.Lines.Add('Commande envoyée: Éteindre LED');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ComPort1 := TComPort.Create(Self);
  ComPort1.OnRxChar := ComPortRxChar;

  // Remplir le ComboBox avec les ports disponibles
  for i := 1 to 20 do
    ComboBox1.Items.Add('COM' + IntToStr(i));

  LedOnButton.Enabled := False;
  LedOffButton.Enabled := False;
end;

procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  ReceivedData: string;
begin
  ComPort1.ReadStr(ReceivedData, Count);
  Memo1.Lines.Add('Reçu: ' + ReceivedData);
end;
```

## Bonnes pratiques pour la communication série/Bluetooth

1. **Toujours vérifier la disponibilité** : Vérifiez toujours si le port est disponible avant de tenter une connexion.

2. **Gérer les erreurs** : Utilisez des blocs try-except pour gérer les erreurs de connexion.

3. **Libérer les ressources** : Fermez toujours les connexions et libérez les ports lorsqu'ils ne sont plus nécessaires.

4. **Utiliser des fils d'exécution séparés** : Pour les opérations de longue durée, utilisez des threads pour éviter de bloquer l'interface utilisateur.

5. **Timeout et reconnexion** : Prévoyez des mécanismes de timeout et de reconnexion automatique.

6. **Protocole de communication** : Définissez un protocole clair entre votre application et l'appareil distant.

7. **Débogage** : Ajoutez des fonctionnalités de journalisation pour faciliter le débogage des communications.

## Recherche et sélection de ports

Pour aider les utilisateurs à choisir le bon port série, vous pouvez lister tous les ports disponibles :

```delphi
procedure TForm1.ListAvailablePorts;
var
  Registry: TRegistry;
  KeyList: TStringList;
  i: Integer;
begin
  Registry := TRegistry.Create;
  KeyList := TStringList.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    Registry.OpenKey('HARDWARE\DEVICEMAP\SERIALCOMM', False);
    Registry.GetValueNames(KeyList);

    ComboBox1.Items.Clear;
    for i := 0 to KeyList.Count - 1 do
      ComboBox1.Items.Add(Registry.ReadString(KeyList[i]));

    if ComboBox1.Items.Count > 0 then
      ComboBox1.ItemIndex := 0;
  finally
    Registry.Free;
    KeyList.Free;
  end;
end;
```

## Défis courants et solutions

### Problème : Déconnexions inattendues

**Solution** : Implémentez un mécanisme de surveillance de la connexion et de reconnexion automatique.

```delphi
procedure TForm1.ConnectionWatchdogTimer(Sender: TObject);
begin
  if not ComPort1.Connected and AutoReconnect then
  begin
    try
      ComPort1.Open;
      Memo1.Lines.Add('Reconnecté');
    except
      on E: Exception do
        Memo1.Lines.Add('Tentative de reconnexion échouée: ' + E.Message);
    end;
  end;
end;
```

### Problème : Données incomplètes

**Solution** : Implémentez un protocole avec des délimiteurs ou préfixes de longueur.

```delphi
procedure TForm1.ComPortRxChar(Sender: TObject; Count: Integer);
var
  ReceivedData: string;
begin
  ComPort1.ReadStr(ReceivedData, Count);
  BufferIncoming := BufferIncoming + ReceivedData;

  if Pos(#13#10, BufferIncoming) > 0 then
  begin
    // Message complet reçu
    Memo1.Lines.Add('Message complet: ' + Copy(BufferIncoming, 1, Pos(#13#10, BufferIncoming) - 1));
    BufferIncoming := Copy(BufferIncoming, Pos(#13#10, BufferIncoming) + 2, Length(BufferIncoming));
  end;
end;
```

## Ressources et bibliothèques

### Bibliothèques Delphi pour la communication série/Bluetooth

1. [ComPort Library](https://sourceforge.net/projects/comport/) - Bibliothèque populaire pour la communication série
2. [Synaser](https://sourceforge.net/projects/synalist/) - Bibliothèque série cross-platform
3. [Bluetooth Framework](https://www.btframework.com/) - Bibliothèque commerciale pour Bluetooth
4. [TMSIntraIoT](https://www.tmssoftware.com) - Composants IoT incluant le support Bluetooth et série

### Documentation et tutoriels

1. [Documentation officielle Delphi](https://docwiki.embarcadero.com/RADStudio/en/Main_Page)
2. [Guide de programmation série](https://www.developerfusion.com/article/99/serial-port-programming-in-delphi/)
3. [Tutoriels Bluetooth BLE](https://www.embarcadero.com/embedded-iot/bluetooth)

## Conclusion

La communication série et Bluetooth sont des éléments fondamentaux pour connecter vos applications Delphi à des dispositifs IoT. Bien que la configuration initiale puisse sembler complexe, les bibliothèques et composants disponibles simplifient considérablement le processus.

Dans la prochaine section, nous explorerons l'intégration avec des plateformes IoT populaires comme Arduino et Raspberry Pi, en nous appuyant sur ces connaissances de communication série et Bluetooth.

> **Note pour le tutoriel complet** : Cette section se concentre sur les communications série et Bluetooth de base. Pour des protocoles plus avancés comme MQTT et CoAP, référez-vous aux sections dédiées dans ce tutoriel.

⏭️ [Intégration avec Arduino / Raspberry Pi](21-delphi-et-liot/03-integration-avec-arduino-raspberry-pi.md)
