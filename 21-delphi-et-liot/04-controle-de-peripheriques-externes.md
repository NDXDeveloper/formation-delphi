# 21.4 Contrôle de périphériques externes

L'un des aspects les plus passionnants de l'Internet des Objets (IoT) avec Delphi est la possibilité de contrôler des périphériques externes. Cette section vous guidera à travers les bases du contrôle de périphériques externes en utilisant Delphi, avec des exemples pratiques et accessibles.

## Introduction

Le contrôle de périphériques externes permet à vos applications Delphi d'interagir avec le monde physique. Cela peut inclure:
- Des capteurs (température, humidité, lumière...)
- Des actionneurs (moteurs, relais, LED...)
- Des périphériques intelligents (caméras, imprimantes, dispositifs médicaux...)

## Méthodes de connexion prises en charge

Delphi offre plusieurs façons de communiquer avec des périphériques externes:

1. **Communication série (UART/COM)** - La méthode la plus courante et simple
2. **USB** - Pour les périphériques modernes
3. **Bluetooth/BLE** - Pour les communications sans fil à courte portée
4. **WiFi/Réseau** - Pour les périphériques compatibles réseau
5. **GPIO** - Pour une interaction directe avec des broches électroniques (via des cartes adaptées)

## Communication série avec Delphi

La communication série reste l'une des méthodes les plus simples et universelles pour contrôler des périphériques externes. Voyons comment l'implémenter:

### Exemple: Communication série basique

#### Étape 1: Préparer votre projet

Créez un nouveau projet VCL ou FMX et ajoutez les composants suivants sur votre formulaire:

- Un composant `TComPort` (disponible via [ComPort Library](https://sourceforge.net/projects/comport/) ou d'autres bibliothèques similaires)
- Quelques boutons et zones de texte pour l'interface utilisateur

Si vous n'avez pas accès à une bibliothèque de port COM tierce, vous pouvez utiliser l'approche native Windows avec la bibliothèque standard:

```pascal
uses
  Winapi.Windows, System.SysUtils;
```

#### Étape 2: Ouvrir le port série

```pascal
procedure TForm1.BtnOpenPortClick(Sender: TObject);
begin
  // Si vous utilisez TComPort
  ComPort1.Port := 'COM3'; // Adaptez au port détecté sur votre système
  ComPort1.BaudRate := br9600;
  ComPort1.DataBits := dbEight;
  ComPort1.StopBits := sbOne;
  ComPort1.Parity.Bits := prNone;
  try
    ComPort1.Open;
    StatusBar1.SimpleText := 'Port ouvert';
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ouverture du port: ' + E.Message);
  end;
end;
```

#### Étape 3: Envoyer des commandes

```pascal
procedure TForm1.BtnSendCommandClick(Sender: TObject);
var
  Command: String;
begin
  if not ComPort1.Connected then
  begin
    ShowMessage('Port COM non connecté!');
    Exit;
  end;

  Command := EditCommand.Text + #13#10; // Ajoute retour à la ligne
  try
    ComPort1.WriteStr(Command);
    MemoLog.Lines.Add('Envoyé: ' + Command);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''envoi: ' + E.Message);
  end;
end;
```

#### Étape 4: Recevoir des données

Pour recevoir des données du périphérique, configurez un gestionnaire d'événements:

```pascal
procedure TForm1.ComPort1RxChar(Sender: TObject; Count: Integer);
var
  Response: String;
begin
  ComPort1.ReadStr(Response, Count);
  // Traiter les données reçues
  MemoLog.Lines.Add('Reçu: ' + Response);
end;
```

#### Étape 5: Fermer proprement la connexion

```pascal
procedure TForm1.BtnClosePortClick(Sender: TObject);
begin
  if ComPort1.Connected then
  begin
    ComPort1.Close;
    StatusBar1.SimpleText := 'Port fermé';
  end;
end;
```

### Approche sans bibliothèque tierce (Windows API)

Si vous préférez ne pas utiliser de bibliothèque tierce, voici comment communiquer avec un port série en utilisant l'API Windows:

```pascal
// Déclaration des variables
var
  ComHandle: THandle;
  DCB: TDCB;
  CommTimeouts: TCommTimeouts;
  BytesWritten, BytesRead: DWORD;
  Buffer: array[0..255] of Char;

// Ouverture du port
ComHandle := CreateFile(
  'COM3',
  GENERIC_READ or GENERIC_WRITE,
  0,
  nil,
  OPEN_EXISTING,
  FILE_ATTRIBUTE_NORMAL,
  0);

if ComHandle <> INVALID_HANDLE_VALUE then
begin
  // Configuration du port
  FillChar(DCB, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);
  GetCommState(ComHandle, DCB);
  DCB.BaudRate := CBR_9600;
  DCB.ByteSize := 8;
  DCB.Parity := NOPARITY;
  DCB.StopBits := ONESTOPBIT;
  SetCommState(ComHandle, DCB);

  // Configuration des timeouts
  GetCommTimeouts(ComHandle, CommTimeouts);
  CommTimeouts.ReadIntervalTimeout := 50;
  CommTimeouts.ReadTotalTimeoutMultiplier := 10;
  CommTimeouts.ReadTotalTimeoutConstant := 100;
  SetCommTimeouts(ComHandle, CommTimeouts);

  // Écriture
  WriteFile(ComHandle, 'AT'#13#10, 4, BytesWritten, nil);

  // Lecture (bloquante)
  ReadFile(ComHandle, Buffer, 256, BytesRead, nil);

  // Fermeture
  CloseHandle(ComHandle);
end;
```

## Contrôle via USB

Pour les périphériques USB, plusieurs approches sont possibles selon le type de périphérique:

1. **Périphériques USB émulant un port série** - Utilisez l'approche série ci-dessus
2. **API USB spécifique au périphérique** - Utilisez les DLL ou SDK fournis par le fabricant
3. **LibUSB** - Une bibliothèque open-source pour l'accès générique aux périphériques USB

### Exemple d'utilisation d'une DLL USB fournisseur

```pascal
// Déclaration des fonctions externes
type
  TUsbOpen = function(DeviceID: Integer): Integer; stdcall;
  TUsbWrite = function(Handle: Integer; Buffer: PByte; Length: Integer): Integer; stdcall;
  TUsbClose = procedure(Handle: Integer); stdcall;

var
  UsbDll: THandle;
  UsbOpen: TUsbOpen;
  UsbWrite: TUsbWrite;
  UsbClose: TUsbClose;
  DevHandle: Integer;

// Chargement de la DLL
UsbDll := LoadLibrary('deviceusb.dll');
if UsbDll <> 0 then
begin
  @UsbOpen := GetProcAddress(UsbDll, 'DevOpen');
  @UsbWrite := GetProcAddress(UsbDll, 'DevWrite');
  @UsbClose := GetProcAddress(UsbDll, 'DevClose');

  if Assigned(UsbOpen) and Assigned(UsbWrite) and Assigned(UsbClose) then
  begin
    // Utilisation
    DevHandle := UsbOpen(0);
    if DevHandle > 0 then
    begin
      UsbWrite(DevHandle, @Command[1], Length(Command));
      UsbClose(DevHandle);
    end;
  end;

  FreeLibrary(UsbDll);
end;
```

## Contrôle de périphériques Bluetooth

Delphi 12 Athens offre une prise en charge améliorée du Bluetooth, notamment pour les appareils BLE (Bluetooth Low Energy).

### Exemple avec Bluetooth (FMX)

```pascal
uses
  System.Bluetooth,
  System.Bluetooth.Components;

// Dans votre formulaire, ajoutez:
// TBluetoothLE et TBluetoothLEDevice

procedure TForm1.ScanAndConnectDevice;
begin
  BluetoothLE1.Enabled := True;
  BluetoothLE1.DiscoverDevices(5000); // Scan pendant 5 secondes
end;

procedure TForm1.BluetoothLE1DiscoveryEnd(const Sender: TObject; const ADeviceList: TBluetoothLEDeviceList);
var
  I: Integer;
begin
  for I := 0 to ADeviceList.Count - 1 do
  begin
    if ADeviceList[I].DeviceName.Contains('MonPériphérique') then
    begin
      // Périphérique trouvé, connexion
      BluetoothLEDevice1.Enabled := True;
      BluetoothLEDevice1.DeviceID := ADeviceList[I].Identifier;
      BluetoothLEDevice1.Connect;
      Break;
    end;
  end;
end;

procedure TForm1.BluetoothLEDevice1EndDiscoverServices(const Sender: TObject; const AServiceList: TBluetoothGattServiceList; const AError: string);
var
  Service: TBluetoothGattService;
  Characteristic: TBluetoothGattCharacteristic;
begin
  // Recherche du service et de la caractéristique souhaités
  for Service in AServiceList do
  begin
    if Service.UUID = '{6E400001-B5A3-F393-E0A9-E50E24DCCA9E}' then // UUID du service
    begin
      for Characteristic in Service.Characteristics do
      begin
        if Characteristic.UUID = '{6E400002-B5A3-F393-E0A9-E50E24DCCA9E}' then // UUID pour écriture
        begin
          // Envoyer une commande
          BluetoothLEDevice1.WriteCharacteristic(Characteristic,
            TEncoding.UTF8.GetBytes('ON'));
          Break;
        end;
      end;
    end;
  end;
end;
```

> **Note:** Le code Bluetooth ci-dessus nécessite Delphi 12 ou supérieur pour la prise en charge complète BLE.

## Contrôle via réseau (WiFi/Ethernet)

Pour les périphériques connectés au réseau (comme les modules ESP8266/ESP32, caméras IP, imprimantes réseau), utilisez les composants réseau de Delphi:

```pascal
uses
  System.Net.Socket;

var
  TCPClient: TSocket;
  Buffer: TBytes;

// Connexion au périphérique
TCPClient := TSocket.Create(TSocketType.TCP, TEncoding.UTF8);
TCPClient.Connect('192.168.1.100', 80);

// Envoi d'une commande
Buffer := TEncoding.UTF8.GetBytes('SET RELAY=ON');
TCPClient.Send(Buffer);

// Réception de la réponse
SetLength(Buffer, 1024);
TCPClient.Receive(Buffer);
ResponseStr := TEncoding.UTF8.GetString(Buffer);

// Fermeture
TCPClient.Close;
```

## Exemple pratique: Contrôle d'une lampe connectée

Voici un exemple complet pour contrôler une lampe connectée via HTTP (typique pour appareils IoT comme ESP8266):

```pascal
uses
  System.Net.HttpClient, System.Net.URLClient;

procedure TForm1.BtnTurnOnClick(Sender: TObject);
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
begin
  HTTP := THTTPClient.Create;
  try
    try
      // Adresse IP et port du périphérique IoT
      Response := HTTP.Get('http://192.168.1.100/control?cmd=ON');

      if Response.StatusCode = 200 then
        ShowMessage('Lampe allumée avec succès!')
      else
        ShowMessage('Erreur: ' + Response.StatusText);
    except
      on E: Exception do
        ShowMessage('Erreur de connexion: ' + E.Message);
    end;
  finally
    HTTP.Free;
  end;
end;

procedure TForm1.BtnTurnOffClick(Sender: TObject);
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
begin
  HTTP := THTTPClient.Create;
  try
    try
      Response := HTTP.Get('http://192.168.1.100/control?cmd=OFF');

      if Response.StatusCode = 200 then
        ShowMessage('Lampe éteinte avec succès!')
      else
        ShowMessage('Erreur: ' + Response.StatusText);
    except
      on E: Exception do
        ShowMessage('Erreur de connexion: ' + E.Message);
    end;
  finally
    HTTP.Free;
  end;
end;
```

## Bonnes pratiques pour le contrôle de périphériques

1. **Gestion des erreurs** - Prévoyez toujours des mécanismes robustes pour gérer les erreurs de communication
2. **Timeouts** - Définissez des délais d'attente adaptés pour éviter que votre application ne se bloque
3. **Thread séparé** - Effectuez les communications dans un thread séparé pour ne pas bloquer l'interface utilisateur
4. **Mode asynchrone** - Utilisez des méthodes asynchrones quand c'est possible
5. **Journalisation** - Enregistrez les communications pour faciliter le débogage
6. **Reconnexion automatique** - Implémentez des mécanismes de reconnexion en cas de perte de connexion

### Exemple de communication dans un thread séparé

```pascal
type
  TDeviceThread = class(TThread)
  private
    FPort: String;
    FCommand: String;
    FResponse: String;
    procedure SyncDisplayResponse;
  protected
    procedure Execute; override;
  public
    constructor Create(const APort, ACommand: String);
  end;

constructor TDeviceThread.Create(const APort, ACommand: String);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FPort := APort;
  FCommand := ACommand;
end;

procedure TDeviceThread.Execute;
var
  ComPort: TComPort; // ou utilisez l'API Windows comme vu précédemment
begin
  // Communication dans le thread
  ComPort := TComPort.Create(nil);
  try
    ComPort.Port := FPort;
    ComPort.BaudRate := br9600;
    ComPort.Open;

    if ComPort.Connected then
    begin
      ComPort.WriteStr(FCommand + #13#10);
      Sleep(100); // Attente courte pour la réponse
      ComPort.ReadStr(FResponse, 1024);

      // Affichage dans le thread principal
      Synchronize(SyncDisplayResponse);
    end;
  finally
    if ComPort.Connected then
      ComPort.Close;
    ComPort.Free;
  end;
end;

procedure TDeviceThread.SyncDisplayResponse;
begin
  Form1.MemoLog.Lines.Add('Réponse: ' + FResponse);
end;

// Utilisation:
procedure TForm1.ButtonSendClick(Sender: TObject);
begin
  TDeviceThread.Create('COM3', EditCommand.Text);
  // Pas besoin de stocker l'instance car FreeOnTerminate = True
end;
```

## Conclusion

Le contrôle de périphériques externes avec Delphi ouvre un monde de possibilités pour créer des applications interactives avec l'environnement physique. Les approches présentées ici vous permettent de démarrer, mais n'hésitez pas à explorer davantage en fonction de vos besoins spécifiques.

N'oubliez pas que Delphi excelle dans ce domaine grâce à sa compilation native, sa gestion efficace des threads et son accès direct aux API système.

## Ressources complémentaires

- Bibliothèque [Synaser](https://github.com/synopse/synapse) pour la communication série
- Bibliothèque [TComPort](https://sourceforge.net/projects/comport/) pour une interface complète avec les ports COM
- Composants [JEDI](https://www.delphi-jedi.org/) pour l'accès à diverses API système

## Exercices pratiques

1. Créez une application qui communique avec un Arduino via port série
2. Développez une interface pour contrôler une lampe connectée via HTTP
3. Implémentez un scanner de périphériques Bluetooth disponibles

La prochaine section couvrira les protocoles IoT spécialisés comme MQTT et CoAP.
