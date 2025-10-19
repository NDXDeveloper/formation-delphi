🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.6 Gestion de dispositifs connectés

## Introduction

Jusqu'à présent, nous avons travaillé avec un ou deux dispositifs à la fois. Dans un environnement IoT réel, vous devrez souvent gérer des dizaines, voire des centaines de dispositifs connectés simultanément : capteurs, actionneurs, passerelles, etc.

Cette section vous apprendra à créer des applications Delphi capables de :
- Découvrir automatiquement les dispositifs sur le réseau
- Gérer des connexions multiples simultanées
- Superviser l'état et la santé de chaque dispositif
- Configurer et mettre à jour les dispositifs à distance
- Organiser et grouper les dispositifs logiquement
- Gérer les pannes et déconnexions

## Défis de la gestion multi-dispositifs

### Problématiques courantes

#### 1. Découverte des dispositifs
Comment trouver automatiquement tous les dispositifs disponibles sur le réseau sans configuration manuelle ?

#### 2. Identification unique
Comment identifier de manière unique chaque dispositif parmi des centaines d'appareils similaires ?

#### 3. État de connexion
Comment savoir en temps réel quels dispositifs sont en ligne, hors ligne, ou en erreur ?

#### 4. Scalabilité
Comment gérer efficacement 10, 100 ou 1000 dispositifs sans ralentir l'application ?

#### 5. Configuration
Comment configurer ou reconfigurer des dispositifs à distance ?

#### 6. Mises à jour
Comment déployer des mises à jour logicielles sur plusieurs dispositifs ?

#### 7. Supervision
Comment monitorer la santé globale du système et détecter les anomalies ?

## Architecture de gestion de dispositifs

### Modèle de données

Commençons par définir la structure de données pour représenter un dispositif :

```pascal
unit IoTDevice;

interface

uses
  System.SysUtils, System.DateUtils, System.Classes;

type
  TDeviceType = (dtSensor, dtActuator, dtGateway, dtController);
  TDeviceStatus = (dsUnknown, dsOnline, dsOffline, dsError, dsMaintenance);

  TIoTDevice = class
  private
    FID: string;
    FName: string;
    FDeviceType: TDeviceType;
    FStatus: TDeviceStatus;
    FIPAddress: string;
    FPort: Integer;
    FMACAddress: string;
    FFirmwareVersion: string;
    FLastSeen: TDateTime;
    FLastHeartbeat: TDateTime;
    FProperties: TStringList;
    FBatteryLevel: Integer;
    FSignalStrength: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateHeartbeat;
    function IsOnline(TimeoutSeconds: Integer = 30): Boolean;
    function GetProperty(const Name: string): string;
    procedure SetProperty(const Name, Value: string);

    property ID: string read FID write FID;
    property Name: string read FName write FName;
    property DeviceType: TDeviceType read FDeviceType write FDeviceType;
    property Status: TDeviceStatus read FStatus write FStatus;
    property IPAddress: string read FIPAddress write FIPAddress;
    property Port: Integer read FPort write FPort;
    property MACAddress: string read FMACAddress write FMACAddress;
    property FirmwareVersion: string read FFirmwareVersion write FFirmwareVersion;
    property LastSeen: TDateTime read FLastSeen write FLastSeen;
    property LastHeartbeat: TDateTime read FLastHeartbeat;
    property BatteryLevel: Integer read FBatteryLevel write FBatteryLevel;
    property SignalStrength: Integer read FSignalStrength write FSignalStrength;
  end;

implementation

constructor TIoTDevice.Create;
begin
  inherited Create;
  FID := '';
  FName := '';
  FStatus := dsUnknown;
  FLastSeen := Now;
  FLastHeartbeat := Now;
  FProperties := TStringList.Create;
  FProperties.Duplicates := dupIgnore;
  FProperties.NameValueSeparator := '=';
  FBatteryLevel := -1;  // -1 = non applicable
  FSignalStrength := -1;
end;

destructor TIoTDevice.Destroy;
begin
  FProperties.Free;
  inherited;
end;

procedure TIoTDevice.UpdateHeartbeat;
begin
  FLastHeartbeat := Now;
  FLastSeen := Now;
  FStatus := dsOnline;
end;

function TIoTDevice.IsOnline(TimeoutSeconds: Integer): Boolean;
begin
  Result := SecondsBetween(Now, FLastHeartbeat) <= TimeoutSeconds;

  if Result then
    FStatus := dsOnline
  else if FStatus = dsOnline then
    FStatus := dsOffline;
end;

function TIoTDevice.GetProperty(const Name: string): string;
begin
  Result := FProperties.Values[Name];
end;

procedure TIoTDevice.SetProperty(const Name, Value: string);
begin
  FProperties.Values[Name] := Value;
end;

end.
```

### Gestionnaire de dispositifs

Créons maintenant un gestionnaire central pour tous les dispositifs :

```pascal
unit DeviceManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IoTDevice;

type
  TDeviceEvent = procedure(Device: TIoTDevice) of object;
  TDeviceErrorEvent = procedure(Device: TIoTDevice; const ErrorMessage: string) of object;

  TDeviceManager = class
  private
    FDevices: TObjectList<TIoTDevice>;
    FMonitorThread: TThread;
    FOnDeviceAdded: TDeviceEvent;
    FOnDeviceRemoved: TDeviceEvent;
    FOnDeviceStatusChanged: TDeviceEvent;
    FOnDeviceError: TDeviceErrorEvent;

    function FindDeviceByID(const ID: string): TIoTDevice;
    procedure StartMonitoring;
    procedure StopMonitoring;
  public
    constructor Create;
    destructor Destroy; override;

    // Gestion des dispositifs
    function AddDevice(Device: TIoTDevice): Boolean;
    function RemoveDevice(const ID: string): Boolean;
    function GetDevice(const ID: string): TIoTDevice;
    function GetDeviceCount: Integer;
    function GetAllDevices: TArray<TIoTDevice>;

    // Filtres et recherche
    function GetDevicesByType(DeviceType: TDeviceType): TArray<TIoTDevice>;
    function GetDevicesByStatus(Status: TDeviceStatus): TArray<TIoTDevice>;
    function GetOnlineDevices: TArray<TIoTDevice>;

    // Opérations groupées
    procedure UpdateAllDeviceStatus;
    procedure SendCommandToAll(const Command: string);
    procedure SendCommandToType(DeviceType: TDeviceType; const Command: string);

    // Événements
    property OnDeviceAdded: TDeviceEvent read FOnDeviceAdded write FOnDeviceAdded;
    property OnDeviceRemoved: TDeviceEvent read FOnDeviceRemoved write FOnDeviceRemoved;
    property OnDeviceStatusChanged: TDeviceEvent read FOnDeviceStatusChanged write FOnDeviceStatusChanged;
    property OnDeviceError: TDeviceErrorEvent read FOnDeviceError write FOnDeviceError;
  end;

implementation

constructor TDeviceManager.Create;
begin
  inherited Create;
  FDevices := TObjectList<TIoTDevice>.Create(True);  // Owns objects
  StartMonitoring;
end;

destructor TDeviceManager.Destroy;
begin
  StopMonitoring;
  FDevices.Free;
  inherited;
end;

function TDeviceManager.FindDeviceByID(const ID: string): TIoTDevice;
var
  Device: TIoTDevice;
begin
  Result := nil;
  for Device in FDevices do
  begin
    if SameText(Device.ID, ID) then
    begin
      Result := Device;
      Break;
    end;
  end;
end;

function TDeviceManager.AddDevice(Device: TIoTDevice): Boolean;
begin
  Result := False;

  // Vérifier si le dispositif existe déjà
  if FindDeviceByID(Device.ID) <> nil then
    Exit;

  FDevices.Add(Device);
  Result := True;

  // Déclencher l'événement
  if Assigned(FOnDeviceAdded) then
    FOnDeviceAdded(Device);
end;

function TDeviceManager.RemoveDevice(const ID: string): Boolean;
var
  Device: TIoTDevice;
begin
  Device := FindDeviceByID(ID);
  Result := Device <> nil;

  if Result then
  begin
    // Déclencher l'événement avant de supprimer
    if Assigned(FOnDeviceRemoved) then
      FOnDeviceRemoved(Device);

    FDevices.Remove(Device);
  end;
end;

function TDeviceManager.GetDevice(const ID: string): TIoTDevice;
begin
  Result := FindDeviceByID(ID);
end;

function TDeviceManager.GetDeviceCount: Integer;
begin
  Result := FDevices.Count;
end;

function TDeviceManager.GetAllDevices: TArray<TIoTDevice>;
begin
  Result := FDevices.ToArray;
end;

function TDeviceManager.GetDevicesByType(DeviceType: TDeviceType): TArray<TIoTDevice>;
var
  Device: TIoTDevice;
  List: TList<TIoTDevice>;
begin
  List := TList<TIoTDevice>.Create;
  try
    for Device in FDevices do
    begin
      if Device.DeviceType = DeviceType then
        List.Add(Device);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TDeviceManager.GetDevicesByStatus(Status: TDeviceStatus): TArray<TIoTDevice>;
var
  Device: TIoTDevice;
  List: TList<TIoTDevice>;
begin
  List := TList<TIoTDevice>.Create;
  try
    for Device in FDevices do
    begin
      if Device.Status = Status then
        List.Add(Device);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TDeviceManager.GetOnlineDevices: TArray<TIoTDevice>;
var
  Device: TIoTDevice;
  List: TList<TIoTDevice>;
begin
  List := TList<TIoTDevice>.Create;
  try
    for Device in FDevices do
    begin
      if Device.IsOnline then
        List.Add(Device);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

procedure TDeviceManager.UpdateAllDeviceStatus;
var
  Device: TIoTDevice;
  OldStatus: TDeviceStatus;
begin
  for Device in FDevices do
  begin
    OldStatus := Device.Status;

    // Vérifier si le dispositif est toujours en ligne
    Device.IsOnline(30);  // Timeout de 30 secondes

    // Si le statut a changé, déclencher l'événement
    if (OldStatus <> Device.Status) and Assigned(FOnDeviceStatusChanged) then
      FOnDeviceStatusChanged(Device);
  end;
end;

procedure TDeviceManager.SendCommandToAll(const Command: string);
var
  Device: TIoTDevice;
begin
  for Device in FDevices do
  begin
    if Device.IsOnline then
    begin
      try
        // Envoyer la commande au dispositif
        // Implémentation dépend du protocole utilisé
      except
        on E: Exception do
        begin
          if Assigned(FOnDeviceError) then
            FOnDeviceError(Device, E.Message);
        end;
      end;
    end;
  end;
end;

procedure TDeviceManager.SendCommandToType(DeviceType: TDeviceType; const Command: string);
var
  Device: TIoTDevice;
begin
  for Device in FDevices do
  begin
    if (Device.DeviceType = DeviceType) and Device.IsOnline then
    begin
      try
        // Envoyer la commande
      except
        on E: Exception do
        begin
          if Assigned(FOnDeviceError) then
            FOnDeviceError(Device, E.Message);
        end;
      end;
    end;
  end;
end;

procedure TDeviceManager.StartMonitoring;
begin
  FMonitorThread := TThread.CreateAnonymousThread(procedure
  begin
    while not TThread.CurrentThread.CheckTerminated do
    begin
      TThread.Synchronize(nil, procedure
      begin
        UpdateAllDeviceStatus;
      end);

      Sleep(5000);  // Vérifier toutes les 5 secondes
    end;
  end);

  FMonitorThread.FreeOnTerminate := False;
  FMonitorThread.Start;
end;

procedure TDeviceManager.StopMonitoring;
begin
  if Assigned(FMonitorThread) then
  begin
    FMonitorThread.Terminate;
    FMonitorThread.WaitFor;
    FMonitorThread.Free;
    FMonitorThread := nil;
  end;
end;

end.
```

## Découverte automatique de dispositifs

### Méthodes de découverte

#### 1. Scan réseau (IP Scan)

Balayer une plage d'adresses IP pour détecter les dispositifs :

```pascal
unit NetworkScanner;

interface

uses
  System.SysUtils, System.Classes, IdTCPClient, IdICMPClient;

type
  TDeviceFoundEvent = procedure(const IPAddress: string; const DeviceInfo: string) of object;

  TNetworkScanner = class
  private
    FBaseIP: string;
    FStartRange: Integer;
    FEndRange: Integer;
    FPort: Integer;
    FOnDeviceFound: TDeviceFoundEvent;
    FScanning: Boolean;

    procedure ScanIP(const IPAddress: string);
  public
    constructor Create(const BaseIP: string; StartRange, EndRange: Integer; Port: Integer = 80);

    procedure StartScan;
    procedure StopScan;

    property OnDeviceFound: TDeviceFoundEvent read FOnDeviceFound write FOnDeviceFound;
    property Scanning: Boolean read FScanning;
  end;

implementation

constructor TNetworkScanner.Create(const BaseIP: string; StartRange, EndRange: Integer; Port: Integer);
begin
  inherited Create;
  FBaseIP := BaseIP;
  FStartRange := StartRange;
  FEndRange := EndRange;
  FPort := Port;
  FScanning := False;
end;

procedure TNetworkScanner.ScanIP(const IPAddress: string);
var
  TCPClient: TIdTCPClient;
  DeviceInfo: string;
begin
  TCPClient := TIdTCPClient.Create(nil);
  try
    TCPClient.Host := IPAddress;
    TCPClient.Port := FPort;
    TCPClient.ConnectTimeout := 1000;  // 1 seconde

    try
      TCPClient.Connect;

      if TCPClient.Connected then
      begin
        // Dispositif trouvé, tenter d'obtenir des informations
        // Envoyer une requête d'identification
        TCPClient.IOHandler.WriteLn('INFO');
        DeviceInfo := TCPClient.IOHandler.ReadLn;

        if Assigned(FOnDeviceFound) then
          FOnDeviceFound(IPAddress, DeviceInfo);

        TCPClient.Disconnect;
      end;
    except
      // Pas de dispositif à cette adresse
    end;
  finally
    TCPClient.Free;
  end;
end;

procedure TNetworkScanner.StartScan;
var
  I: Integer;
  IPAddress: string;
begin
  if FScanning then Exit;

  FScanning := True;

  TThread.CreateAnonymousThread(procedure
  var
    J: Integer;
  begin
    for J := FStartRange to FEndRange do
    begin
      if not FScanning then Break;

      IPAddress := Format('%s.%d', [FBaseIP, J]);

      TThread.Synchronize(nil, procedure
      begin
        ScanIP(IPAddress);
      end);
    end;

    FScanning := False;
  end).Start;
end;

procedure TNetworkScanner.StopScan;
begin
  FScanning := False;
end;

end.
```

#### 2. Découverte mDNS/Bonjour

mDNS (Multicast DNS) permet aux dispositifs de s'annoncer automatiquement :

```pascal
unit MDNSDiscovery;

interface

uses
  System.SysUtils, System.Classes;

type
  TServiceFoundEvent = procedure(const ServiceName, IPAddress: string; Port: Integer) of object;

  TMDNSDiscovery = class
  private
    FServiceType: string;
    FOnServiceFound: TServiceFoundEvent;

    procedure ProcessMDNSResponse(const Response: TBytes);
  public
    constructor Create(const ServiceType: string);

    procedure StartDiscovery;
    procedure StopDiscovery;

    property OnServiceFound: TServiceFoundEvent read FOnServiceFound write FOnServiceFound;
  end;

implementation

// Note: Une implémentation complète de mDNS nécessiterait
// une bibliothèque spécialisée ou l'utilisation de l'API système

constructor TMDNSDiscovery.Create(const ServiceType: string);
begin
  inherited Create;
  FServiceType := ServiceType;  // Ex: "_http._tcp.local."
end;

procedure TMDNSDiscovery.StartDiscovery;
begin
  // Envoyer une requête mDNS multicast
  // Écouter les réponses
  // Parser les réponses et déclencher OnServiceFound
end;

procedure TMDNSDiscovery.StopDiscovery;
begin
  // Arrêter l'écoute
end;

procedure TMDNSDiscovery.ProcessMDNSResponse(const Response: TBytes);
begin
  // Parser la réponse mDNS
  // Extraire le nom du service, l'IP, le port
  // Déclencher l'événement OnServiceFound
end;

end.
```

#### 3. Découverte MQTT (via topics spécifiques)

Les dispositifs peuvent s'annoncer via MQTT :

```pascal
procedure TDeviceManager.DiscoverViaMQTT(MQTTClient: TMQTTClient);
begin
  // S'abonner au topic de découverte
  MQTTClient.Subscribe('devices/announce');

  // Publier une requête de découverte
  MQTTClient.Publish('devices/discovery/request', 'DISCOVER', 0);
end;

// Les dispositifs répondent en publiant leurs informations
// sur devices/announce
```

### Interface de découverte

```pascal
procedure TFormMain.ButtonScanClick(Sender: TObject);
var
  Scanner: TNetworkScanner;
begin
  ListBoxDevices.Clear;
  ButtonScan.Enabled := False;

  Scanner := TNetworkScanner.Create('192.168.1', 1, 254, 80);
  try
    Scanner.OnDeviceFound := procedure(const IPAddress, DeviceInfo: string)
    begin
      ListBoxDevices.Items.Add(Format('%s - %s', [IPAddress, DeviceInfo]));

      // Créer et ajouter le dispositif au gestionnaire
      var Device := TIoTDevice.Create;
      Device.ID := IPAddress;
      Device.Name := DeviceInfo;
      Device.IPAddress := IPAddress;
      Device.Status := dsOnline;

      DeviceManager.AddDevice(Device);
    end;

    Scanner.StartScan;

    // Attendre la fin du scan
    while Scanner.Scanning do
    begin
      Application.ProcessMessages;
      Sleep(100);
    end;
  finally
    Scanner.Free;
    ButtonScan.Enabled := True;
  end;

  ShowMessage(Format('%d dispositifs trouvés', [ListBoxDevices.Items.Count]));
end;
```

## Visualisation et supervision

### Interface de tableau de bord

Créons une interface pour visualiser tous les dispositifs :

```pascal
unit DashboardForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.Grids, Vcl.StdCtrls, Vcl.ExtCtrls,
  DeviceManager, IoTDevice;

type
  TFormDashboard = class(TForm)
    StringGridDevices: TStringGrid;
    PanelTop: TPanel;
    ButtonRefresh: TButton;
    LabelOnline: TLabel;
    LabelOffline: TLabel;
    LabelTotal: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure StringGridDevicesDblClick(Sender: TObject);
  private
    FDeviceManager: TDeviceManager;
    procedure UpdateDeviceGrid;
    procedure UpdateStatistics;
    procedure ShowDeviceDetails(Device: TIoTDevice);
  public
    property DeviceManager: TDeviceManager read FDeviceManager write FDeviceManager;
  end;

implementation

{$R *.dfm}

procedure TFormDashboard.FormCreate(Sender: TObject);
begin
  // Configurer la grille
  StringGridDevices.ColCount := 7;
  StringGridDevices.RowCount := 1;

  StringGridDevices.Cells[0, 0] := 'État';
  StringGridDevices.Cells[1, 0] := 'ID';
  StringGridDevices.Cells[2, 0] := 'Nom';
  StringGridDevices.Cells[3, 0] := 'Type';
  StringGridDevices.Cells[4, 0] := 'IP';
  StringGridDevices.Cells[5, 0] := 'Dernière activité';
  StringGridDevices.Cells[6, 0] := 'Batterie';

  StringGridDevices.ColWidths[0] := 50;
  StringGridDevices.ColWidths[1] := 150;
  StringGridDevices.ColWidths[2] := 200;
  StringGridDevices.ColWidths[3] := 100;
  StringGridDevices.ColWidths[4] := 120;
  StringGridDevices.ColWidths[5] := 150;
  StringGridDevices.ColWidths[6] := 80;

  Timer1.Enabled := True;
end;

procedure TFormDashboard.UpdateDeviceGrid;
var
  Devices: TArray<TIoTDevice>;
  Device: TIoTDevice;
  Row: Integer;
  StatusText, TypeText: string;
begin
  if not Assigned(FDeviceManager) then Exit;

  Devices := FDeviceManager.GetAllDevices;

  // Ajuster le nombre de lignes
  StringGridDevices.RowCount := Length(Devices) + 1;

  Row := 1;
  for Device in Devices do
  begin
    // Statut avec couleur
    case Device.Status of
      dsOnline: StatusText := '🟢';
      dsOffline: StatusText := '🔴';
      dsError: StatusText := '⚠️';
      dsMaintenance: StatusText := '🔧';
      else StatusText := '❓';
    end;

    // Type
    case Device.DeviceType of
      dtSensor: TypeText := 'Capteur';
      dtActuator: TypeText := 'Actionneur';
      dtGateway: TypeText := 'Passerelle';
      dtController: TypeText := 'Contrôleur';
    end;

    StringGridDevices.Cells[0, Row] := StatusText;
    StringGridDevices.Cells[1, Row] := Device.ID;
    StringGridDevices.Cells[2, Row] := Device.Name;
    StringGridDevices.Cells[3, Row] := TypeText;
    StringGridDevices.Cells[4, Row] := Device.IPAddress;
    StringGridDevices.Cells[5, Row] := FormatDateTime('dd/mm/yyyy hh:nn:ss', Device.LastSeen);

    if Device.BatteryLevel >= 0 then
      StringGridDevices.Cells[6, Row] := Format('%d%%', [Device.BatteryLevel])
    else
      StringGridDevices.Cells[6, Row] := 'N/A';

    Inc(Row);
  end;

  UpdateStatistics;
end;

procedure TFormDashboard.UpdateStatistics;
var
  Total, Online, Offline: Integer;
begin
  if not Assigned(FDeviceManager) then Exit;

  Total := FDeviceManager.GetDeviceCount;
  Online := Length(FDeviceManager.GetOnlineDevices);
  Offline := Length(FDeviceManager.GetDevicesByStatus(dsOffline));

  LabelTotal.Caption := Format('Total: %d', [Total]);
  LabelOnline.Caption := Format('En ligne: %d', [Online]);
  LabelOffline.Caption := Format('Hors ligne: %d', [Offline]);

  // Changer la couleur si des dispositifs sont hors ligne
  if Offline > 0 then
    LabelOffline.Font.Color := clRed
  else
    LabelOffline.Font.Color := clGreen;
end;

procedure TFormDashboard.ButtonRefreshClick(Sender: TObject);
begin
  UpdateDeviceGrid;
end;

procedure TFormDashboard.Timer1Timer(Sender: TObject);
begin
  // Mise à jour automatique toutes les 5 secondes
  UpdateDeviceGrid;
end;

procedure TFormDashboard.StringGridDevicesDblClick(Sender: TObject);
var
  Row: Integer;
  DeviceID: string;
  Device: TIoTDevice;
begin
  Row := StringGridDevices.Row;
  if Row < 1 then Exit;

  DeviceID := StringGridDevices.Cells[1, Row];
  Device := FDeviceManager.GetDevice(DeviceID);

  if Assigned(Device) then
    ShowDeviceDetails(Device);
end;

procedure TFormDashboard.ShowDeviceDetails(Device: TIoTDevice);
var
  Details: string;
begin
  Details := Format(
    'Dispositif: %s' + sLineBreak +
    'ID: %s' + sLineBreak +
    'Adresse IP: %s' + sLineBreak +
    'MAC: %s' + sLineBreak +
    'Firmware: %s' + sLineBreak +
    'Batterie: %d%%' + sLineBreak +
    'Signal: %d%%' + sLineBreak +
    'Dernière activité: %s',
    [Device.Name, Device.ID, Device.IPAddress, Device.MACAddress,
     Device.FirmwareVersion, Device.BatteryLevel, Device.SignalStrength,
     FormatDateTime('dd/mm/yyyy hh:nn:ss', Device.LastSeen)]
  );

  ShowMessage(Details);
end;

end.
```

## Configuration à distance

### Système de configuration

```pascal
unit DeviceConfiguration;

interface

uses
  System.SysUtils, System.Classes, System.JSON, IoTDevice;

type
  TDeviceConfig = class
  private
    FDeviceID: string;
    FSamplingInterval: Integer;
    FReportingInterval: Integer;
    FThresholdMin: Double;
    FThresholdMax: Double;
    FEnabled: Boolean;
  public
    function ToJSON: string;
    procedure FromJSON(const JSONString: string);

    property DeviceID: string read FDeviceID write FDeviceID;
    property SamplingInterval: Integer read FSamplingInterval write FSamplingInterval;
    property ReportingInterval: Integer read FReportingInterval write FReportingInterval;
    property ThresholdMin: Double read FThresholdMin write FThresholdMin;
    property ThresholdMax: Double read FThresholdMax write FThresholdMax;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TDeviceConfigurator = class
  public
    class function GetConfiguration(Device: TIoTDevice): TDeviceConfig;
    class function SetConfiguration(Device: TIoTDevice; Config: TDeviceConfig): Boolean;
  end;

implementation

function TDeviceConfig.ToJSON: string;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('device_id', FDeviceID);
    JSONObject.AddPair('sampling_interval', TJSONNumber.Create(FSamplingInterval));
    JSONObject.AddPair('reporting_interval', TJSONNumber.Create(FReportingInterval));
    JSONObject.AddPair('threshold_min', TJSONNumber.Create(FThresholdMin));
    JSONObject.AddPair('threshold_max', TJSONNumber.Create(FThresholdMax));
    JSONObject.AddPair('enabled', TJSONBool.Create(FEnabled));

    Result := JSONObject.ToString;
  finally
    JSONObject.Free;
  end;
end;

procedure TDeviceConfig.FromJSON(const JSONString: string);
var
  JSONValue: TJSONValue;
  JSONObject: TJSONObject;
begin
  JSONValue := TJSONObject.ParseJSONValue(JSONString);
  try
    if JSONValue is TJSONObject then
    begin
      JSONObject := JSONValue as TJSONObject;

      FDeviceID := JSONObject.GetValue<string>('device_id');
      FSamplingInterval := JSONObject.GetValue<Integer>('sampling_interval');
      FReportingInterval := JSONObject.GetValue<Integer>('reporting_interval');
      FThresholdMin := JSONObject.GetValue<Double>('threshold_min');
      FThresholdMax := JSONObject.GetValue<Double>('threshold_max');
      FEnabled := JSONObject.GetValue<Boolean>('enabled');
    end;
  finally
    JSONValue.Free;
  end;
end;

class function TDeviceConfigurator.GetConfiguration(Device: TIoTDevice): TDeviceConfig;
begin
  Result := TDeviceConfig.Create;

  // Envoyer une requête de configuration au dispositif
  // Via MQTT, HTTP, ou autre protocole

  // Exemple simplifié:
  // Envoyer: GET /config
  // Recevoir la configuration en JSON

  Result.DeviceID := Device.ID;
  // Parser la réponse et remplir Result
end;

class function TDeviceConfigurator.SetConfiguration(Device: TIoTDevice; Config: TDeviceConfig): Boolean;
var
  ConfigJSON: string;
begin
  Result := False;

  try
    ConfigJSON := Config.ToJSON;

    // Envoyer la configuration au dispositif
    // Via MQTT: Publish sur 'devices/{id}/config'
    // Via HTTP: POST /config

    // Attendre confirmation
    Result := True;
  except
    on E: Exception do
      Result := False;
  end;
end;

end.
```

### Interface de configuration

```pascal
procedure TFormConfig.ButtonApplyClick(Sender: TObject);
var
  Device: TIoTDevice;
  Config: TDeviceConfig;
begin
  Device := DeviceManager.GetDevice(EditDeviceID.Text);
  if not Assigned(Device) then
  begin
    ShowMessage('Dispositif non trouvé');
    Exit;
  end;

  Config := TDeviceConfig.Create;
  try
    Config.DeviceID := Device.ID;
    Config.SamplingInterval := StrToInt(EditSamplingInterval.Text);
    Config.ReportingInterval := StrToInt(EditReportingInterval.Text);
    Config.ThresholdMin := StrToFloat(EditThresholdMin.Text);
    Config.ThresholdMax := StrToFloat(EditThresholdMax.Text);
    Config.Enabled := CheckBoxEnabled.Checked;

    if TDeviceConfigurator.SetConfiguration(Device, Config) then
      ShowMessage('Configuration appliquée avec succès')
    else
      ShowMessage('Erreur lors de l''application de la configuration');
  finally
    Config.Free;
  end;
end;
```

## Groupes et organisation

### Gestion de groupes de dispositifs

```pascal
unit DeviceGroups;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  IoTDevice;

type
  TDeviceGroup = class
  private
    FName: string;
    FDescription: string;
    FDevices: TList<TIoTDevice>;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;

    procedure AddDevice(Device: TIoTDevice);
    procedure RemoveDevice(Device: TIoTDevice);
    function ContainsDevice(Device: TIoTDevice): Boolean;
    function GetDeviceCount: Integer;

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Devices: TList<TIoTDevice> read FDevices;
  end;

  TGroupManager = class
  private
    FGroups: TObjectList<TDeviceGroup>;
  public
    constructor Create;
    destructor Destroy; override;

    function CreateGroup(const Name: string): TDeviceGroup;
    function DeleteGroup(const Name: string): Boolean;
    function GetGroup(const Name: string): TDeviceGroup;
    function GetAllGroups: TArray<TDeviceGroup>;
  end;

implementation

constructor TDeviceGroup.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
  FDevices := TList<TIoTDevice>.Create;
end;

destructor TDeviceGroup.Destroy;
begin
  FDevices.Free;
  inherited;
end;

procedure TDeviceGroup.AddDevice(Device: TIoTDevice);
begin
  if not ContainsDevice(Device) then
    FDevices.Add(Device);
end;

procedure TDeviceGroup.RemoveDevice(Device: TIoTDevice);
begin
  FDevices.Remove(Device);
end;

function TDeviceGroup.ContainsDevice(Device: TIoTDevice): Boolean;
begin
  Result := FDevices.Contains(Device);
end;

function TDeviceGroup.GetDeviceCount: Integer;
begin
  Result := FDevices.Count;
end;

constructor TGroupManager.Create;
begin
  inherited Create;
  FGroups := TObjectList<TDeviceGroup>.Create(True);
end;

destructor TGroupManager.Destroy;
begin
  FGroups.Free;
  inherited;
end;

function TGroupManager.CreateGroup(const Name: string): TDeviceGroup;
begin
  Result := TDeviceGroup.Create(Name);
  FGroups.Add(Result);
end;

function TGroupManager.DeleteGroup(const Name: string): Boolean;
var
  Group: TDeviceGroup;
begin
  Result := False;
  Group := GetGroup(Name);
  if Assigned(Group) then
  begin
    FGroups.Remove(Group);
    Result := True;
  end;
end;

function TGroupManager.GetGroup(const Name: string): TDeviceGroup;
var
  Group: TDeviceGroup;
begin
  Result := nil;
  for Group in FGroups do
  begin
    if SameText(Group.Name, Name) then
    begin
      Result := Group;
      Break;
    end;
  end;
end;

function TGroupManager.GetAllGroups: TArray<TDeviceGroup>;
begin
  Result := FGroups.ToArray;
end;

end.
```

### Utilisation des groupes

```pascal
// Créer des groupes
var
  Salon, Chambre, Jardin: TDeviceGroup;
begin
  Salon := GroupManager.CreateGroup('Salon');
  Salon.Description := 'Dispositifs du salon';

  Chambre := GroupManager.CreateGroup('Chambre');
  Jardin := GroupManager.CreateGroup('Jardin');

  // Ajouter des dispositifs aux groupes
  Salon.AddDevice(TempSensorSalon);
  Salon.AddDevice(HumSensorSalon);
  Salon.AddDevice(LEDStripSalon);

  // Envoyer une commande à tous les dispositifs d'un groupe
  for Device in Salon.Devices do
  begin
    if Device.DeviceType = dtActuator then
      SendCommand(Device, 'TURN_OFF');
  end;
end;
```

## Alertes et notifications

### Système d'alertes

```pascal
unit AlertSystem;

interface

uses
  System.SysUtils, System.Classes, IoTDevice;

type
  TAlertLevel = (alInfo, alWarning, alError, alCritical);

  TAlert = class
  private
    FDevice: TIoTDevice;
    FLevel: TAlertLevel;
    FMessage: string;
    FTimestamp: TDateTime;
    FAcknowledged: Boolean;
  public
    property Device: TIoTDevice read FDevice write FDevice;
    property Level: TAlertLevel read FLevel write FLevel;
    property Message: string read FMessage write FMessage;
    property Timestamp: TDateTime read FTimestamp write FTimestamp;
    property Acknowledged: Boolean read FAcknowledged write FAcknowledged;
  end;

  TAlertEvent = procedure(Alert: TAlert) of object;

  TAlertManager = class
  private
    FAlerts: TObjectList<TAlert>;
    FOnAlert: TAlertEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RaiseAlert(Device: TIoTDevice; Level: TAlertLevel; const Message: string);
    procedure AcknowledgeAlert(Alert: TAlert);
    function GetUnacknowledgedAlerts: TArray<TAlert>;
    function GetAlertsByLevel(Level: TAlertLevel): TArray<TAlert>;

    property OnAlert: TAlertEvent read FOnAlert write FOnAlert;
  end;

implementation

constructor TAlertManager.Create;
begin
  inherited Create;
  FAlerts := TObjectList<TAlert>.Create(True);
end;

destructor TAlertManager.Destroy;
begin
  FAlerts.Free;
  inherited;
end;

procedure TAlertManager.RaiseAlert(Device: TIoTDevice; Level: TAlertLevel; const Message: string);
var
  Alert: TAlert;
begin
  Alert := TAlert.Create;
  Alert.Device := Device;
  Alert.Level := Level;
  Alert.Message := Message;
  Alert.Timestamp := Now;
  Alert.Acknowledged := False;

  FAlerts.Add(Alert);

  if Assigned(FOnAlert) then
    FOnAlert(Alert);
end;

procedure TAlertManager.AcknowledgeAlert(Alert: TAlert);
begin
  Alert.Acknowledged := True;
end;

function TAlertManager.GetUnacknowledgedAlerts: TArray<TAlert>;
var
  Alert: TAlert;
  List: TList<TAlert>;
begin
  List := TList<TAlert>.Create;
  try
    for Alert in FAlerts do
    begin
      if not Alert.Acknowledged then
        List.Add(Alert);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

function TAlertManager.GetAlertsByLevel(Level: TAlertLevel): TArray<TAlert>;
var
  Alert: TAlert;
  List: TList<TAlert>;
begin
  List := TList<TAlert>.Create;
  try
    for Alert in FAlerts do
    begin
      if Alert.Level = Level then
        List.Add(Alert);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

end.
```

### Règles d'alerte automatiques

```pascal
procedure TDeviceManager.CheckAlertRules;
var
  Device: TIoTDevice;
  Temp: Double;
begin
  for Device in FDevices do
  begin
    // Batterie faible
    if (Device.BatteryLevel >= 0) and (Device.BatteryLevel < 20) then
      AlertManager.RaiseAlert(Device, alWarning,
        Format('Batterie faible: %d%%', [Device.BatteryLevel]));

    // Dispositif hors ligne
    if not Device.IsOnline(60) then
      AlertManager.RaiseAlert(Device, alError, 'Dispositif hors ligne');

    // Température hors limites
    if Device.GetProperty('temperature') <> '' then
    begin
      Temp := StrToFloatDef(Device.GetProperty('temperature'), 0);
      if (Temp < 0) or (Temp > 40) then
        AlertManager.RaiseAlert(Device, alCritical,
          Format('Température anormale: %.1f°C', [Temp]));
    end;
  end;
end;
```

## Mise à jour OTA (Over-The-Air)

### Système de mise à jour à distance

```pascal
unit FirmwareUpdate;

interface

uses
  System.SysUtils, System.Classes, IoTDevice;

type
  TUpdateStatus = (usIdle, usDownloading, usInstalling, usSuccess, usError);
  TUpdateProgressEvent = procedure(Device: TIoTDevice; Progress: Integer) of object;

  TFirmwareUpdater = class
  private
    FOnProgress: TUpdateProgressEvent;
    FCurrentStatus: TUpdateStatus;
  public
    function UpdateDevice(Device: TIoTDevice; const FirmwareURL: string): Boolean;
    function GetUpdateStatus(Device: TIoTDevice): TUpdateStatus;

    property OnProgress: TUpdateProgressEvent read FOnProgress write FOnProgress;
    property CurrentStatus: TUpdateStatus read FCurrentStatus;
  end;

implementation

function TFirmwareUpdater.UpdateDevice(Device: TIoTDevice; const FirmwareURL: string): Boolean;
var
  Progress: Integer;
begin
  Result := False;
  FCurrentStatus := usDownloading;

  try
    // 1. Télécharger le firmware
    Progress := 0;
    while Progress < 100 do
    begin
      // Simuler le téléchargement
      Inc(Progress, 10);
      if Assigned(FOnProgress) then
        FOnProgress(Device, Progress);
      Sleep(100);
    end;

    // 2. Envoyer le firmware au dispositif
    FCurrentStatus := usInstalling;

    // Commande de mise à jour via MQTT ou HTTP
    // Exemple: MQTT publish sur 'devices/{id}/update' avec l'URL du firmware

    // 3. Attendre confirmation
    Sleep(5000);  // Simuler l'installation

    FCurrentStatus := usSuccess;
    Result := True;
  except
    on E: Exception do
    begin
      FCurrentStatus := usError;
      Result := False;
    end;
  end;
end;

function TFirmwareUpdater.GetUpdateStatus(Device: TIoTDevice): TUpdateStatus;
begin
  // Interroger le dispositif pour son statut de mise à jour
  Result := FCurrentStatus;
end;

end.
```

## Persistance des données

### Sauvegarde et restauration

```pascal
unit DevicePersistence;

interface

uses
  System.SysUtils, System.Classes, System.JSON, DeviceManager, IoTDevice;

type
  TDevicePersistence = class
  public
    class procedure SaveDevices(Manager: TDeviceManager; const FileName: string);
    class procedure LoadDevices(Manager: TDeviceManager; const FileName: string);
  end;

implementation

class procedure TDevicePersistence.SaveDevices(Manager: TDeviceManager; const FileName: string);
var
  JSONArray: TJSONArray;
  Devices: TArray<TIoTDevice>;
  Device: TIoTDevice;
  JSONDevice: TJSONObject;
begin
  JSONArray := TJSONArray.Create;
  try
    Devices := Manager.GetAllDevices;

    for Device in Devices do
    begin
      JSONDevice := TJSONObject.Create;
      JSONDevice.AddPair('id', Device.ID);
      JSONDevice.AddPair('name', Device.Name);
      JSONDevice.AddPair('type', TJSONNumber.Create(Ord(Device.DeviceType)));
      JSONDevice.AddPair('ip', Device.IPAddress);
      JSONDevice.AddPair('port', TJSONNumber.Create(Device.Port));
      JSONDevice.AddPair('mac', Device.MACAddress);
      JSONDevice.AddPair('firmware', Device.FirmwareVersion);

      JSONArray.AddElement(JSONDevice);
    end;

    TFile.WriteAllText(FileName, JSONArray.ToString);
  finally
    JSONArray.Free;
  end;
end;

class procedure TDevicePersistence.LoadDevices(Manager: TDeviceManager; const FileName: string);
var
  JSONContent: string;
  JSONArray: TJSONArray;
  JSONValue: TJSONValue;
  JSONDevice: TJSONObject;
  Device: TIoTDevice;
  I: Integer;
begin
  if not TFile.Exists(FileName) then Exit;

  JSONContent := TFile.ReadAllText(FileName);
  JSONArray := TJSONObject.ParseJSONValue(JSONContent) as TJSONArray;
  try
    for I := 0 to JSONArray.Count - 1 do
    begin
      JSONDevice := JSONArray.Items[I] as TJSONObject;

      Device := TIoTDevice.Create;
      Device.ID := JSONDevice.GetValue<string>('id');
      Device.Name := JSONDevice.GetValue<string>('name');
      Device.DeviceType := TDeviceType(JSONDevice.GetValue<Integer>('type'));
      Device.IPAddress := JSONDevice.GetValue<string>('ip');
      Device.Port := JSONDevice.GetValue<Integer>('port');
      Device.MACAddress := JSONDevice.GetValue<string>('mac');
      Device.FirmwareVersion := JSONDevice.GetValue<string>('firmware');

      Manager.AddDevice(Device);
    end;
  finally
    JSONArray.Free;
  end;
end;

end.
```

## Conclusion

La gestion efficace de dispositifs connectés est un pilier fondamental des applications IoT professionnelles. Avec Delphi, vous disposez de tous les outils nécessaires pour créer des systèmes de gestion sophistiqués capables de gérer des centaines de dispositifs.

**Points clés à retenir :**

1. **Architecture** : utiliser un gestionnaire central pour coordonner tous les dispositifs
2. **Découverte** : implémenter plusieurs méthodes (scan IP, mDNS, MQTT) pour la robustesse
3. **Identification** : chaque dispositif doit avoir un ID unique et stable
4. **Monitoring** : surveiller continuellement l'état et la santé des dispositifs
5. **Organisation** : utiliser des groupes pour gérer logiquement les dispositifs
6. **Configuration** : permettre la configuration à distance pour faciliter la maintenance
7. **Alertes** : système d'alertes proactif pour détecter rapidement les problèmes
8. **Mise à jour** : prévoir un mécanisme de mise à jour OTA dès la conception
9. **Persistance** : sauvegarder régulièrement la configuration et l'état
10. **Scalabilité** : concevoir dès le départ pour supporter la croissance

Dans la section suivante, nous verrons comment traiter et analyser les flux de données IoT en temps réel pour en extraire des informations utiles et créer des tableaux de bord interactifs.

⏭️ [Traitement des données IoT en temps réel](/21-delphi-et-liot/07-traitement-des-donnees-iot-en-temps-reel.md)
