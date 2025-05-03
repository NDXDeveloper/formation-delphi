# 21.6 Gestion de dispositifs connectés

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Une fois que vous maîtrisez la communication avec des périphériques individuels et les protocoles IoT comme MQTT et CoAP, l'étape suivante consiste à développer une solution pour gérer plusieurs dispositifs connectés simultanément. Cette section vous guidera à travers la conception et l'implémentation d'une application Delphi capable de gérer un réseau de dispositifs IoT de manière efficace et évolutive.

## Introduction à la gestion de dispositifs

La gestion de dispositifs connectés comprend plusieurs aspects essentiels :

- **Découverte** - Trouver et identifier les dispositifs disponibles
- **Inventaire** - Maintenir une liste des dispositifs avec leurs caractéristiques
- **Configuration** - Paramétrer les dispositifs selon les besoins
- **Surveillance** - Observer l'état et les performances des dispositifs
- **Contrôle** - Envoyer des commandes et recevoir des réponses
- **Mise à jour** - Déployer des mises à jour logicielles (firmware)
- **Sécurité** - Protéger les communications et l'accès aux dispositifs

## Conception d'une architecture de gestion de dispositifs

Avant de commencer à coder, établissons une architecture robuste pour notre système de gestion de dispositifs.

### Architecture en couches

Une approche efficace consiste à utiliser une architecture en couches :

1. **Couche de présentation** - Interface utilisateur (formulaires Delphi)
2. **Couche logique** - Gestion des dispositifs et orchestration
3. **Couche de communication** - Protocoles et échanges de données (MQTT, CoAP, série, etc.)
4. **Couche de persistance** - Stockage des configurations et données historiques

![Architecture en couches](https://via.placeholder.com/600x350.png?text=Architecture+en+couches+pour+gestion+IoT)

### Modèle de dispositif générique

Commençons par définir une interface générique pour représenter n'importe quel type de dispositif connecté :

```pascal
unit DeviceInterfaces;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TDeviceStatus = (dsOffline, dsOnline, dsError, dsMaintenance);
  TDeviceCapability = (dcSensor, dcActuator, dcConfiguration, dcFirmwareUpdate);
  TDeviceCapabilities = set of TDeviceCapability;

  TDeviceValueType = (dvtString, dvtInteger, dvtFloat, dvtBoolean, dvtDateTime, dvtBinary);

  TDeviceValue = record
    Name: string;
    ValueType: TDeviceValueType;
    StringValue: string;
    IntegerValue: Integer;
    FloatValue: Double;
    BooleanValue: Boolean;
    DateTimeValue: TDateTime;
    BinaryValue: TBytes;
    function AsString: string;
    function AsInteger: Integer;
    function AsFloat: Double;
    function AsBoolean: Boolean;
    function AsDateTime: TDateTime;
    function AsBinary: TBytes;
    procedure SetValue(const Value: string); overload;
    procedure SetValue(const Value: Integer); overload;
    procedure SetValue(const Value: Double); overload;
    procedure SetValue(const Value: Boolean); overload;
    procedure SetValue(const Value: TDateTime); overload;
    procedure SetValue(const Value: TBytes); overload;
  end;

  TDevicePropertyList = TDictionary<string, TDeviceValue>;

  TDeviceCommandEvent = procedure(Sender: TObject; const Command: string;
                               Params: TDevicePropertyList) of object;

  IDevice = interface
    ['{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}'] // Générez un GUID unique
    function GetID: string;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetStatus: TDeviceStatus;
    function GetCapabilities: TDeviceCapabilities;
    function GetType: string;
    function GetManufacturer: string;
    function GetModel: string;
    function GetFirmwareVersion: string;

    // Propriétés et valeurs
    function GetProperties: TDevicePropertyList;
    function GetProperty(const PropertyName: string): TDeviceValue;
    procedure SetProperty(const PropertyName: string; const Value: TDeviceValue);

    // Commandes
    function SupportsCommand(const Command: string): Boolean;
    function ExecuteCommand(const Command: string;
                           Params: TDevicePropertyList = nil): Boolean;

    // Connexion
    function Connect: Boolean;
    procedure Disconnect;
    function IsConnected: Boolean;

    // Événements
    procedure SetOnStatusChange(const Handler: TNotifyEvent);
    procedure SetOnPropertyChange(const Handler: TNotifyEvent);
    procedure SetOnCommandReceived(const Handler: TDeviceCommandEvent);

    // Propriétés accessibles
    property ID: string read GetID;
    property Name: string read GetName write SetName;
    property Status: TDeviceStatus read GetStatus;
    property Capabilities: TDeviceCapabilities read GetCapabilities;
    property DeviceType: string read GetType;
    property Manufacturer: string read GetManufacturer;
    property Model: string read GetModel;
    property FirmwareVersion: string read GetFirmwareVersion;
    property Properties: TDevicePropertyList read GetProperties;
  end;
end;
```

### Implémentation de base pour les dispositifs MQTT

Voici une implémentation de base pour un dispositif MQTT générique :

```pascal
unit MQTTDevice;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DeviceInterfaces, MQTT.Client;

type
  TMQTTDevice = class(TInterfacedObject, IDevice)
  private
    FID: string;
    FName: string;
    FStatus: TDeviceStatus;
    FCapabilities: TDeviceCapabilities;
    FDeviceType: string;
    FManufacturer: string;
    FModel: string;
    FFirmwareVersion: string;
    FProperties: TDevicePropertyList;
    FMQTTClient: IMQTTClient;
    FBaseTopic: string;
    FOnStatusChange: TNotifyEvent;
    FOnPropertyChange: TNotifyEvent;
    FOnCommandReceived: TDeviceCommandEvent;

    procedure HandleMQTTMessage(Sender: TObject; Topic: string; Payload: TBytes);
    procedure HandleStatusChange;
    procedure HandlePropertyChange;
    procedure ParseTelemetryMessage(const Payload: string);

  public
    constructor Create(const AID, AName, ABaseTopic: string;
                    ABrokerHost: string; ABrokerPort: Integer = 1883);
    destructor Destroy; override;

    // Implémentation de IDevice
    function GetID: string;
    function GetName: string;
    procedure SetName(const Value: string);
    function GetStatus: TDeviceStatus;
    function GetCapabilities: TDeviceCapabilities;
    function GetType: string;
    function GetManufacturer: string;
    function GetModel: string;
    function GetFirmwareVersion: string;

    function GetProperties: TDevicePropertyList;
    function GetProperty(const PropertyName: string): TDeviceValue;
    procedure SetProperty(const PropertyName: string; const Value: TDeviceValue);

    function SupportsCommand(const Command: string): Boolean;
    function ExecuteCommand(const Command: string;
                          Params: TDevicePropertyList = nil): Boolean;

    function Connect: Boolean;
    procedure Disconnect;
    function IsConnected: Boolean;

    procedure SetOnStatusChange(const Handler: TNotifyEvent);
    procedure SetOnPropertyChange(const Handler: TNotifyEvent);
    procedure SetOnCommandReceived(const Handler: TDeviceCommandEvent);
  end;

implementation

uses
  System.JSON;

// ... implémentation des méthodes ...

end;
```

Pour l'implémentation des méthodes, voici un exemple de quelques-unes des plus importantes :

```pascal
constructor TMQTTDevice.Create(const AID, AName, ABaseTopic: string;
                           ABrokerHost: string; ABrokerPort: Integer = 1883);
begin
  inherited Create;
  FID := AID;
  FName := AName;
  FBaseTopic := ABaseTopic;
  FStatus := dsOffline;
  FProperties := TDevicePropertyList.Create;

  // Création du client MQTT
  FMQTTClient := TMQTTClient.Create(ABrokerHost, ABrokerPort);
  FMQTTClient.ClientID := 'DevMgr_' + AID + '_' + IntToStr(Random(1000));
  FMQTTClient.OnPublishReceived := HandleMQTTMessage;
end;

destructor TMQTTDevice.Destroy;
begin
  if IsConnected then
    Disconnect;

  FMQTTClient := nil;
  FProperties.Free;
  inherited;
end;

function TMQTTDevice.Connect: Boolean;
begin
  Result := False;
  try
    FMQTTClient.Connect;

    // S'abonner aux topics pertinents
    FMQTTClient.Subscribe(FBaseTopic + '/status');
    FMQTTClient.Subscribe(FBaseTopic + '/telemetry/#');
    FMQTTClient.Subscribe(FBaseTopic + '/attributes');

    // Demander les informations du dispositif
    FMQTTClient.Publish(FBaseTopic + '/command',
                      TEncoding.UTF8.GetBytes('{"cmd":"get_info"}'),
                      TMQTTQosLevel.AtLeastOnce,
                      False);

    FStatus := dsOnline;
    HandleStatusChange;
    Result := True;
  except
    on E: Exception do
    begin
      FStatus := dsError;
      HandleStatusChange;
    end;
  end;
end;

procedure TMQTTDevice.HandleMQTTMessage(Sender: TObject; Topic: string; Payload: TBytes);
var
  PayloadStr: string;
begin
  PayloadStr := TEncoding.UTF8.GetString(Payload);

  if Topic = FBaseTopic + '/status' then
  begin
    // Traiter le message de statut
    if PayloadStr = 'online' then
      FStatus := dsOnline
    else if PayloadStr = 'offline' then
      FStatus := dsOffline
    else if PayloadStr = 'maintenance' then
      FStatus := dsMaintenance
    else
      FStatus := dsError;

    HandleStatusChange;
  end
  else if Topic.StartsWith(FBaseTopic + '/telemetry') then
  begin
    // Traiter les données de télémétrie
    ParseTelemetryMessage(PayloadStr);
    HandlePropertyChange;
  end
  else if Topic = FBaseTopic + '/attributes' then
  begin
    // Traiter les attributs du dispositif
    var JSONObj := TJSONObject.ParseJSONValue(PayloadStr) as TJSONObject;
    try
      if Assigned(JSONObj) then
      begin
        if JSONObj.TryGetValue<string>('type', FDeviceType) then ;
        if JSONObj.TryGetValue<string>('manufacturer', FManufacturer) then ;
        if JSONObj.TryGetValue<string>('model', FModel) then ;
        if JSONObj.TryGetValue<string>('firmware', FFirmwareVersion) then ;

        // Déterminer les capacités
        FCapabilities := [];
        var CapArray := JSONObj.GetValue<TJSONArray>('capabilities');
        if Assigned(CapArray) then
        begin
          for var I := 0 to CapArray.Count - 1 do
          begin
            var CapStr := CapArray.Items[I].Value;
            if CapStr = 'sensor' then Include(FCapabilities, dcSensor);
            if CapStr = 'actuator' then Include(FCapabilities, dcActuator);
            if CapStr = 'config' then Include(FCapabilities, dcConfiguration);
            if CapStr = 'firmware' then Include(FCapabilities, dcFirmwareUpdate);
          end;
        end;
      end;
    finally
      JSONObj.Free;
    end;
  end;
end;

function TMQTTDevice.ExecuteCommand(const Command: string;
                                Params: TDevicePropertyList = nil): Boolean;
var
  JSONObj: TJSONObject;
  JSONParams: TJSONObject;
  ParamPair: TPair<string, TDeviceValue>;
begin
  Result := False;

  if not IsConnected then
    Exit;

  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('cmd', Command);

    if Assigned(Params) and (Params.Count > 0) then
    begin
      JSONParams := TJSONObject.Create;
      for ParamPair in Params do
      begin
        case ParamPair.Value.ValueType of
          dvtString: JSONParams.AddPair(ParamPair.Key, ParamPair.Value.StringValue);
          dvtInteger: JSONParams.AddPair(ParamPair.Key, TJSONNumber.Create(ParamPair.Value.IntegerValue));
          dvtFloat: JSONParams.AddPair(ParamPair.Key, TJSONNumber.Create(ParamPair.Value.FloatValue));
          dvtBoolean: JSONParams.AddPair(ParamPair.Key, TJSONBool.Create(ParamPair.Value.BooleanValue));
          // Pour les autres types, on pourrait convertir en string
        end;
      end;
      JSONObj.AddPair('params', JSONParams);
    end;

    var JSONStr := JSONObj.ToString;
    Result := FMQTTClient.Publish(FBaseTopic + '/command',
                                TEncoding.UTF8.GetBytes(JSONStr),
                                TMQTTQosLevel.AtLeastOnce,
                                False);
  finally
    JSONObj.Free;
  end;
end;

procedure TMQTTDevice.ParseTelemetryMessage(const Payload: string);
var
  JSONObj: TJSONObject;
  Value: TDeviceValue;
  Pair: TJSONPair;
begin
  JSONObj := TJSONObject.ParseJSONValue(Payload) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    for Pair in JSONObj do
    begin
      Value.Name := Pair.JsonString.Value;

      if Pair.JsonValue is TJSONString then
      begin
        Value.ValueType := dvtString;
        Value.StringValue := Pair.JsonValue.Value;
      end
      else if Pair.JsonValue is TJSONNumber then
      begin
        var NumValue := (Pair.JsonValue as TJSONNumber).AsDouble;
        // Déterminer si c'est un entier ou un float
        if Frac(NumValue) = 0 then
        begin
          Value.ValueType := dvtInteger;
          Value.IntegerValue := Trunc(NumValue);
        end
        else
        begin
          Value.ValueType := dvtFloat;
          Value.FloatValue := NumValue;
        end;
      end
      else if Pair.JsonValue is TJSONBool then
      begin
        Value.ValueType := dvtBoolean;
        Value.BooleanValue := (Pair.JsonValue as TJSONBool).AsBoolean;
      end;

      SetProperty(Value.Name, Value);
    end;
  finally
    JSONObj.Free;
  end;
end;
```

## Gestionnaire de dispositifs

Maintenant que nous avons défini notre interface de dispositif et créé une implémentation pour MQTT, créons un gestionnaire de dispositifs qui peut gérer plusieurs dispositifs simultanément :

```pascal
unit DeviceManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DeviceInterfaces;

type
  TDeviceDiscoveryEvent = procedure(Sender: TObject; Device: IDevice) of object;

  TDeviceManager = class
  private
    FDevices: TDictionary<string, IDevice>;
    FOnDeviceDiscovered: TDeviceDiscoveryEvent;
    FOnDeviceStatusChanged: TNotifyEvent;

    procedure HandleDeviceStatusChange(Sender: TObject);
    procedure HandleDevicePropertyChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    // Gestion des dispositifs
    procedure AddDevice(Device: IDevice);
    procedure RemoveDevice(const DeviceID: string);
    function GetDevice(const DeviceID: string): IDevice;
    function GetDeviceByName(const DeviceName: string): IDevice;
    function GetAllDevices: TArray<IDevice>;
    function GetDevicesByType(const DeviceType: string): TArray<IDevice>;
    function GetDevicesByStatus(Status: TDeviceStatus): TArray<IDevice>;

    // Découverte de dispositifs
    procedure StartDiscovery(const NetworkType: string = '');
    procedure StopDiscovery;

    // Commandes groupées
    function ExecuteCommandOnDevices(const DeviceIDs: TArray<string>;
                                  const Command: string;
                                  Params: TDevicePropertyList = nil): Boolean;

    // Événements
    property OnDeviceDiscovered: TDeviceDiscoveryEvent read FOnDeviceDiscovered
                                                     write FOnDeviceDiscovered;
    property OnDeviceStatusChanged: TNotifyEvent read FOnDeviceStatusChanged
                                               write FOnDeviceStatusChanged;
  end;

implementation

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation de quelques méthodes importantes :

```pascal
constructor TDeviceManager.Create;
begin
  inherited Create;
  FDevices := TDictionary<string, IDevice>.Create;
end;

destructor TDeviceManager.Destroy;
begin
  // Note: Les devices gérés ne sont pas libérés automatiquement car ce sont des interfaces
  FDevices.Free;
  inherited;
end;

procedure TDeviceManager.AddDevice(Device: IDevice);
begin
  if not FDevices.ContainsKey(Device.ID) then
  begin
    // Ajouter le dispositif à la collection
    FDevices.Add(Device.ID, Device);

    // Configurer les gestionnaires d'événements
    Device.SetOnStatusChange(HandleDeviceStatusChange);
    Device.SetOnPropertyChange(HandleDevicePropertyChange);

    // Tenter de se connecter au dispositif
    Device.Connect;
  end;
end;

procedure TDeviceManager.HandleDeviceStatusChange(Sender: TObject);
var
  Device: IDevice;
begin
  if Sender is TInterfacedObject then
  begin
    Device := Sender as IDevice;
    // Ici vous pourriez aussi journaliser le changement de statut

    if Assigned(FOnDeviceStatusChanged) then
      FOnDeviceStatusChanged(Self);
  end;
end;

function TDeviceManager.GetDevicesByStatus(Status: TDeviceStatus): TArray<IDevice>;
var
  ResultList: TList<IDevice>;
  Device: IDevice;
begin
  ResultList := TList<IDevice>.Create;
  try
    for Device in FDevices.Values do
    begin
      if Device.Status = Status then
        ResultList.Add(Device);
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TDeviceManager.ExecuteCommandOnDevices(const DeviceIDs: TArray<string>;
                                            const Command: string;
                                            Params: TDevicePropertyList = nil): Boolean;
var
  Device: IDevice;
  DeviceID: string;
  AllSuccessful: Boolean;
begin
  AllSuccessful := True;

  for DeviceID in DeviceIDs do
  begin
    if FDevices.TryGetValue(DeviceID, Device) then
    begin
      if not Device.ExecuteCommand(Command, Params) then
        AllSuccessful := False;
    end
    else
      AllSuccessful := False;
  end;

  Result := AllSuccessful;
end;
```

## Implémentation de la découverte automatique

La découverte de dispositifs est une fonctionnalité clé d'un système de gestion IoT. Voici comment vous pourriez implémenter une découverte MQTT simple :

```pascal
procedure TDeviceManager.StartDiscoveryMQTT(const BrokerHost: string; BrokerPort: Integer = 1883);
var
  Client: IMQTTClient;

  procedure HandleDiscoveryMessage(Sender: TObject; Topic: string; Payload: TBytes);
  var
    PayloadStr: string;
    JSONObj: TJSONObject;
    DeviceID, DeviceName, DeviceBaseTopic: string;
    NewDevice: IDevice;
  begin
    PayloadStr := TEncoding.UTF8.GetString(Payload);
    JSONObj := TJSONObject.ParseJSONValue(PayloadStr) as TJSONObject;

    try
      if Assigned(JSONObj) and
         JSONObj.TryGetValue<string>('id', DeviceID) and
         JSONObj.TryGetValue<string>('name', DeviceName) and
         JSONObj.TryGetValue<string>('base_topic', DeviceBaseTopic) then
      begin
        // Vérifier si on connaît déjà ce dispositif
        if not FDevices.ContainsKey(DeviceID) then
        begin
          // Créer un nouveau dispositif MQTT
          NewDevice := TMQTTDevice.Create(DeviceID, DeviceName,
                                       DeviceBaseTopic, BrokerHost, BrokerPort);

          // Ajouter au gestionnaire
          AddDevice(NewDevice);

          // Déclencher l'événement de découverte
          if Assigned(FOnDeviceDiscovered) then
            FOnDeviceDiscovered(Self, NewDevice);
        end;
      end;
    finally
      JSONObj.Free;
    end;
  end;

begin
  // Créer un client MQTT spécifique pour la découverte
  Client := TMQTTClient.Create(BrokerHost, BrokerPort);
  Client.ClientID := 'DeviceDiscovery_' + IntToStr(Random(1000));
  Client.OnPublishReceived := HandleDiscoveryMessage;

  try
    Client.Connect;

    // S'abonner au topic de découverte
    Client.Subscribe('discovery/announce');

    // Envoyer une demande de découverte
    Client.Publish('discovery/request',
                 TEncoding.UTF8.GetBytes('{"action":"announce"}'),
                 TMQTTQosLevel.AtMostOnce,
                 False);
  except
    on E: Exception do
    begin
      // Gérer les erreurs de découverte
    end;
  end;
end;
```

## Interface utilisateur pour la gestion de dispositifs

Créons maintenant une interface utilisateur simple mais efficace pour notre gestionnaire de dispositifs :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, DeviceManager, DeviceInterfaces;

type
  TfrmDeviceManager = class(TForm)
    pgcMain: TPageControl;
    tabDevices: TTabSheet;
    tabDashboard: TTabSheet;
    tabSettings: TTabSheet;
    lvDevices: TListView;
    pnlDeviceActions: TPanel;
    btnAddDevice: TButton;
    btnRemoveDevice: TButton;
    btnDiscoverDevices: TButton;
    btnRefresh: TButton;
    splVertical: TSplitter;
    pnlDeviceDetails: TPanel;
    lblDeviceName: TLabel;
    edtDeviceName: TEdit;
    lblDeviceType: TLabel;
    lblDeviceTypeValue: TLabel;
    lblStatus: TLabel;
    lblStatusValue: TLabel;
    lblProperties: TLabel;
    lvProperties: TListView;
    grpCommands: TGroupBox;
    cmbCommand: TComboBox;
    btnExecute: TButton;
    mmoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddDeviceClick(Sender: TObject);
    procedure btnRemoveDeviceClick(Sender: TObject);
    procedure btnDiscoverDevicesClick(Sender: TObject);
    procedure lvDevicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
  private
    FDeviceManager: TDeviceManager;
    FSelectedDevice: IDevice;

    procedure HandleDeviceDiscovered(Sender: TObject; Device: IDevice);
    procedure HandleDeviceStatusChanged(Sender: TObject);
    procedure UpdateDeviceList;
    procedure UpdateDeviceDetails;
  public
    { Public declarations }
  end;

var
  frmDeviceManager: TfrmDeviceManager;

implementation

uses
  MQTTDevice, AddDeviceForm;

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

Voici quelques méthodes clés de l'interface utilisateur :

```pascal
procedure TfrmDeviceManager.FormCreate(Sender: TObject);
begin
  FDeviceManager := TDeviceManager.Create;
  FDeviceManager.OnDeviceDiscovered := HandleDeviceDiscovered;
  FDeviceManager.OnDeviceStatusChanged := HandleDeviceStatusChanged;

  UpdateDeviceList;
end;

procedure TfrmDeviceManager.HandleDeviceDiscovered(Sender: TObject; Device: IDevice);
begin
  // Journaliser la découverte
  mmoLog.Lines.Add(Format('Dispositif découvert: %s (%s)', [Device.Name, Device.ID]));

  // Mettre à jour la liste des dispositifs
  UpdateDeviceList;
end;

procedure TfrmDeviceManager.UpdateDeviceList;
var
  Devices: TArray<IDevice>;
  Device: IDevice;
  Item: TListItem;
  StatusText: string;
begin
  lvDevices.Items.BeginUpdate;
  try
    lvDevices.Items.Clear;

    Devices := FDeviceManager.GetAllDevices;
    for Device in Devices do
    begin
      Item := lvDevices.Items.Add;
      Item.Caption := Device.Name;
      Item.Data := Pointer(Device); // Attention: ne fonctionne pas directement avec des interfaces

      // Convertir le statut en texte
      case Device.Status of
        dsOffline: StatusText := 'Hors ligne';
        dsOnline: StatusText := 'En ligne';
        dsError: StatusText := 'Erreur';
        dsMaintenance: StatusText := 'Maintenance';
      end;

      Item.SubItems.Add(StatusText);
      Item.SubItems.Add(Device.DeviceType);
      Item.SubItems.Add(Device.Manufacturer);
      Item.SubItems.Add(Device.Model);
    end;
  finally
    lvDevices.Items.EndUpdate;
  end;
end;

procedure TfrmDeviceManager.lvDevicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Devices: TArray<IDevice>;
  I: Integer;
begin
  if Selected and Assigned(Item) then
  begin
    // Rechercher le dispositif par son nom (car on ne peut pas stocker l'interface directement)
    Devices := FDeviceManager.GetAllDevices;
    for I := 0 to High(Devices) do
    begin
      if Devices[I].Name = Item.Caption then
      begin
        FSelectedDevice := Devices[I];
        UpdateDeviceDetails;
        Break;
      end;
    end;
  end
  else
    FSelectedDevice := nil;
end;

procedure TfrmDeviceManager.UpdateDeviceDetails;
var
  PropertyPair: TPair<string, TDeviceValue>;
  Item: TListItem;
  PropValue: string;
begin
  if not Assigned(FSelectedDevice) then
  begin
    // Effacer les détails
    edtDeviceName.Text := '';
    lblDeviceTypeValue.Caption := '';
    lblStatusValue.Caption := '';
    lvProperties.Items.Clear;
    Exit;
  end;

  // Mettre à jour les informations de base
  edtDeviceName.Text := FSelectedDevice.Name;
  lblDeviceTypeValue.Caption := FSelectedDevice.DeviceType;

  // Statut
  case FSelectedDevice.Status of
    dsOffline: lblStatusValue.Caption := 'Hors ligne';
    dsOnline: lblStatusValue.Caption := 'En ligne';
    dsError: lblStatusValue.Caption := 'Erreur';
    dsMaintenance: lblStatusValue.Caption := 'Maintenance';
  end;

  // Propriétés
  lvProperties.Items.BeginUpdate;
  try
    lvProperties.Items.Clear;

    for PropertyPair in FSelectedDevice.Properties do
    begin
      Item := lvProperties.Items.Add;
      Item.Caption := PropertyPair.Key;

      // Convertir la valeur en chaîne selon son type
      case PropertyPair.Value.ValueType of
        dvtString: PropValue := PropertyPair.Value.StringValue;
        dvtInteger: PropValue := IntToStr(PropertyPair.Value.IntegerValue);
        dvtFloat: PropValue := FormatFloat('0.##', PropertyPair.Value.FloatValue);
        dvtBoolean: PropValue := BoolToStr(PropertyPair.Value.BooleanValue, True);
        dvtDateTime: PropValue := DateTimeToStr(PropertyPair.Value.DateTimeValue);
        dvtBinary: PropValue := Format('<%d bytes>', [Length(PropertyPair.Value.BinaryValue)]);
      end;

      Item.SubItems.Add(PropValue);
    end;
  finally
    lvProperties.Items.EndUpdate;
  end;

  // Commandes disponibles
  cmbCommand.Items.Clear;

  if Assigned(FSelectedDevice) then
  begin
    // Ajouter les commandes standard
    cmbCommand.Items.Add('refresh');
    cmbCommand.Items.Add('reboot');

    // Ajouter des commandes spécifiques selon les capacités
    if dcActuator in FSelectedDevice.Capabilities then
    begin
      cmbCommand.Items.Add('turn_on');
      cmbCommand.Items.Add('turn_off');
      cmbCommand.Items.Add('toggle');
    end;

    if dcConfiguration in FSelectedDevice.Capabilities then
    begin
      cmbCommand.Items.Add('get_config');
      cmbCommand.Items.Add('set_config');
    end;

    if dcFirmwareUpdate in FSelectedDevice.Capabilities then
    begin
      cmbCommand.Items.Add('check_update');
      cmbCommand.Items.Add('start_update');
    end;
  end;

  if cmbCommand.Items.Count > 0 then
    cmbCommand.ItemIndex := 0;
end;

procedure TfrmDeviceManager.btnExecuteClick(Sender: TObject);
var
  Command: string;
  Success: Boolean;
begin
  if not Assigned(FSelectedDevice) then
  begin
    ShowMessage('Veuillez sélectionner un dispositif');
    Exit;
  end;

  if cmbCommand.ItemIndex < 0 then
  begin
    ShowMessage('Veuillez sélectionner une commande');
    Exit;
  end;

  Command := cmbCommand.Items[cmbCommand.ItemIndex];

  // Pour certaines commandes, on pourrait ajouter une boîte de dialogue pour les paramètres
  Success := FSelectedDevice.ExecuteCommand(Command);

  if Success then
    mmoLog.Lines.Add(Format('Commande "%s" envoyée avec succès à %s', [Command, FSelectedDevice.Name]))
  else
    mmoLog.Lines.Add(Format('Échec de l''envoi de la commande "%s" à %s', [Command, FSelectedDevice.Name]));
end;
```

## Ajout manuel de dispositifs

Pour ajouter manuellement des dispositifs, nous avons besoin d'un formulaire dédié :

```pascal
unit AddDeviceForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  DeviceInterfaces, MQTTDevice;

type
  TfrmAddDevice = class(TForm)
    lblDeviceType: TLabel;
    cmbDeviceType: TComboBox;
    lblDeviceID: TLabel;
    edtDeviceID: TEdit;
    lblDeviceName: TLabel;
    edtDeviceName: TEdit;
    grpMQTTSettings: TGroupBox;
    lblBrokerHost: TLabel;
    edtBrokerHost: TEdit;
    lblBrokerPort: TLabel;
    edtBrokerPort: TEdit;
    lblBaseTopic: TLabel;
    edtBaseTopic: TEdit;
    btnCancel: TButton;
    btnAdd: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmbDeviceTypeChange(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
  private
    FDevice: IDevice;
  public
    property Device: IDevice read FDevice;
  end;

implementation

{$R *.dfm}

procedure TfrmAddDevice.FormCreate(Sender: TObject);
begin
  // Initialiser les types de dispositifs disponibles
  cmbDeviceType.Items.Clear;
  cmbDeviceType.Items.Add('MQTT');
  cmbDeviceType.Items.Add('Serial');
  cmbDeviceType.Items.Add('Bluetooth');
  cmbDeviceType.Items.Add('CoAP');

  // Type par défaut
  cmbDeviceType.ItemIndex := 0;
  cmbDeviceTypeChange(nil);

  // Valeurs par défaut
  edtBrokerHost.Text := 'localhost';
  edtBrokerPort.Text := '1883';

  // Générer un ID unique par défaut
  edtDeviceID.Text := 'device_' + IntToStr(Random(10000));
end;

procedure TfrmAddDevice.cmbDeviceTypeChange(Sender: TObject);
begin
  // Afficher/masquer les paramètres selon le type de dispositif
  case cmbDeviceType.ItemIndex of
    0: // MQTT
      begin
        grpMQTTSettings.Visible := True;
      end;
    // Ajouter des cas pour d'autres types de dispositifs
  end;
end;

procedure TfrmAddDevice.btnAddClick(Sender: TObject);
var
  DeviceID, DeviceName, BrokerHost, BaseTopic: string;
  BrokerPort: Integer;
begin
  // Validation de base
  DeviceID := Trim(edtDeviceID.Text);
  DeviceName := Trim(edtDeviceName.Text);

  if (DeviceID = '') or (DeviceName = '') then
  begin
    ShowMessage('Veuillez remplir l''ID et le nom du dispositif');
    Exit;
  end;

  case cmbDeviceType.ItemIndex of
    0: // MQTT
      begin
        BrokerHost := Trim(edtBrokerHost.Text);
        BaseTopic := Trim(edtBaseTopic.Text);

        if (BrokerHost = '') or (BaseTopic = '') then
        begin
          ShowMessage('Veuillez remplir tous les champs MQTT');
          Exit;
        end;

        if not TryStrToInt(edtBrokerPort.Text, BrokerPort) then
        begin
          ShowMessage('Le port doit être un nombre');
          Exit;
        end;

        // Créer le dispositif MQTT
        FDevice := TMQTTDevice.Create(DeviceID, DeviceName, BaseTopic, BrokerHost, BrokerPort);
      end;
    // Autres types de dispositifs
  end;

  if Assigned(FDevice) then
    ModalResult := mrOk
  else
    ShowMessage('Erreur lors de la création du dispositif');
end;
```

Et voici comment utiliser ce formulaire dans notre application principale :

```pascal
procedure TfrmDeviceManager.btnAddDeviceClick(Sender: TObject);
var
  AddForm: TfrmAddDevice;
begin
  AddForm := TfrmAddDevice.Create(Self);
  try
    if AddForm.ShowModal = mrOk then
    begin
      // Ajouter le nouveau dispositif au gestionnaire
      FDeviceManager.AddDevice(AddForm.Device);

      // Mettre à jour la liste
      UpdateDeviceList;

      mmoLog.Lines.Add('Dispositif ajouté: ' + AddForm.Device.Name);
    end;
  finally
    AddForm.Free;
  end;
end;
```

## Tableau de bord pour visualiser les dispositifs

Ajoutons un tableau de bord simple mais efficace pour visualiser l'état des dispositifs et leurs données :

```pascal
unit DashboardFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  DeviceInterfaces, System.Generics.Collections;

type
  TDevicePanel = class(TPanel)
  private
    FDevice: IDevice;
    FNameLabel: TLabel;
    FStatusShape: TShape;
    FValueLabels: TDictionary<string, TLabel>;

    procedure UpdateUI;
    procedure HandlePropertyChange(Sender: TObject);
    procedure HandleStatusChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; ADevice: IDevice); reintroduce;
    destructor Destroy; override;

    property Device: IDevice read FDevice;
  end;

  TDashboardFrame = class(TFrame)
    scrDashboard: TScrollBox;
    pnlAddWidget: TPanel;
    btnAddWidget: TButton;
    procedure btnAddWidgetClick(Sender: TObject);
  private
    FDevicePanels: TObjectList<TDevicePanel>;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddDevice(ADevice: IDevice);
    procedure RemoveDevice(const DeviceID: string);
    procedure RefreshDevices;
  end;

implementation

{$R *.dfm}

constructor TDashboardFrame.Create(AOwner: TComponent);
begin
  inherited;
  FDevicePanels := TObjectList<TDevicePanel>.Create;
end;

destructor TDashboardFrame.Destroy;
begin
  FDevicePanels.Free;
  inherited;
end;

procedure TDashboardFrame.AddDevice(ADevice: IDevice);
var
  DevicePanel: TDevicePanel;
begin
  // Vérifier si le dispositif est déjà sur le tableau de bord
  for DevicePanel in FDevicePanels do
  begin
    if DevicePanel.Device.ID = ADevice.ID then
      Exit; // Déjà présent
  end;

  // Créer un nouveau panel pour ce dispositif
  DevicePanel := TDevicePanel.Create(scrDashboard, ADevice);
  DevicePanel.Parent := scrDashboard;
  DevicePanel.Align := alTop;
  DevicePanel.Height := 100;
  DevicePanel.Margins.SetBounds(5, 5, 5, 5);
  DevicePanel.AlignWithMargins := True;

  // Ajouter à notre liste
  FDevicePanels.Add(DevicePanel);
end;

// ...autres méthodes...

constructor TDevicePanel.Create(AOwner: TComponent; ADevice: IDevice);
begin
  inherited Create(AOwner);
  FDevice := ADevice;
  FValueLabels := TDictionary<string, TLabel>.Create;

  // Configurer le panel
  BevelOuter := bvRaised;

  // Créer les contrôles pour ce dispositif
  FNameLabel := TLabel.Create(Self);
  FNameLabel.Parent := Self;
  FNameLabel.Align := alTop;
  FNameLabel.Alignment := taCenter;
  FNameLabel.Font.Style := [fsBold];
  FNameLabel.Caption := FDevice.Name;

  FStatusShape := TShape.Create(Self);
  FStatusShape.Parent := Self;
  FStatusShape.Width := 15;
  FStatusShape.Height := 15;
  FStatusShape.Left := 10;
  FStatusShape.Top := 30;

  // S'abonner aux événements du dispositif
  FDevice.SetOnPropertyChange(HandlePropertyChange);
  FDevice.SetOnStatusChange(HandleStatusChange);

  // Initialiser l'interface
  UpdateUI;
end;

destructor TDevicePanel.Destroy;
begin
  FValueLabels.Free;
  inherited;
end;

procedure TDevicePanel.UpdateUI;
var
  Y: Integer;
  PropertyPair: TPair<string, TDeviceValue>;
  ValueLabel: TLabel;
  DisplayValue: string;
begin
  // Mise à jour du statut
  case FDevice.Status of
    dsOffline: FStatusShape.Brush.Color := clGray;
    dsOnline: FStatusShape.Brush.Color := clGreen;
    dsError: FStatusShape.Brush.Color := clRed;
    dsMaintenance: FStatusShape.Brush.Color := clYellow;
  end;

  // Mise à jour des valeurs
  Y := 30;

  for PropertyPair in FDevice.Properties do
  begin
    // Ne pas afficher toutes les propriétés, seulement celles intéressantes
    if not PropertyPair.Key.StartsWith('_') then // Ignorer les propriétés internes
    begin
      // Convertir la valeur en texte d'affichage
      case PropertyPair.Value.ValueType of
        dvtString: DisplayValue := PropertyPair.Value.StringValue;
        dvtInteger: DisplayValue := IntToStr(PropertyPair.Value.IntegerValue);
        dvtFloat: DisplayValue := FormatFloat('0.##', PropertyPair.Value.FloatValue);
        dvtBoolean: DisplayValue := BoolToStr(PropertyPair.Value.BooleanValue, True);
        dvtDateTime: DisplayValue := DateTimeToStr(PropertyPair.Value.DateTimeValue);
        else DisplayValue := '?';
      end;

      // Créer ou mettre à jour le label pour cette propriété
      if not FValueLabels.TryGetValue(PropertyPair.Key, ValueLabel) then
      begin
        ValueLabel := TLabel.Create(Self);
        ValueLabel.Parent := Self;
        ValueLabel.Left := 30;
        ValueLabel.Top := Y;
        ValueLabel.AutoSize := True;

        FValueLabels.Add(PropertyPair.Key, ValueLabel);
      end;

      ValueLabel.Caption := Format('%s: %s', [PropertyPair.Key, DisplayValue]);

      Inc(Y, 20);
    end;
  end;
end;

procedure TDevicePanel.HandlePropertyChange(Sender: TObject);
begin
  // Mise à jour de l'interface dans le thread principal
  TThread.Queue(nil, procedure
  begin
    UpdateUI;
  end);
end;

procedure TDevicePanel.HandleStatusChange(Sender: TObject);
begin
  // Mise à jour de l'interface dans le thread principal
  TThread.Queue(nil, procedure
  begin
    UpdateUI;
  end);
end;
```

## Gestion des groupes de dispositifs

Pour les systèmes plus complexes, il peut être utile de regrouper les dispositifs par fonctionnalité ou par emplacement. Voici une implémentation simple de la gestion des groupes :

```pascal
unit DeviceGroups;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DeviceInterfaces;

type
  TDeviceGroup = class
  private
    FName: string;
    FDescription: string;
    FDevices: TList<IDevice>;
  public
    constructor Create(const AName, ADescription: string);
    destructor Destroy; override;

    procedure AddDevice(Device: IDevice);
    procedure RemoveDevice(const DeviceID: string);
    function ContainsDevice(const DeviceID: string): Boolean;
    function GetDevices: TArray<IDevice>;

    function ExecuteCommandOnAll(const Command: string;
                              Params: TDevicePropertyList = nil): Boolean;

    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
  end;

  TDeviceGroupManager = class
  private
    FGroups: TObjectDictionary<string, TDeviceGroup>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddGroup(const GroupName, Description: string);
    procedure RemoveGroup(const GroupName: string);
    function GetGroup(const GroupName: string): TDeviceGroup;
    function GetAllGroups: TArray<TDeviceGroup>;

    procedure AddDeviceToGroup(const GroupName: string; Device: IDevice);
    procedure RemoveDeviceFromGroup(const GroupName, DeviceID: string);
    function GetDevicesInGroup(const GroupName: string): TArray<IDevice>;

    function ExecuteCommandOnGroup(const GroupName, Command: string;
                                Params: TDevicePropertyList = nil): Boolean;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

## Automatisation et planification de tâches

Un aspect important de la gestion de dispositifs est l'automatisation, permettant d'exécuter des commandes à des moments précis ou en réponse à des événements. Voici une implémentation simple :

```pascal
unit AutomationManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.DateUtils,
  DeviceInterfaces, DeviceManager;

type
  TTriggerType = (ttScheduled, ttEvent, ttCondition);

  TAutomationAction = class
  private
    FDeviceID: string;
    FCommand: string;
    FParams: TDevicePropertyList;
  public
    constructor Create(const ADeviceID, ACommand: string);
    destructor Destroy; override;

    procedure AddParam(const ParamName: string; const Value: TDeviceValue);

    property DeviceID: string read FDeviceID;
    property Command: string read FCommand;
    property Params: TDevicePropertyList read FParams;
  end;

  TAutomationRule = class
  private
    FName: string;
    FEnabled: Boolean;
    FTriggerType: TTriggerType;
    FScheduledTime: TDateTime;
    FTriggerCondition: string;
    FActions: TObjectList<TAutomationAction>;
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    procedure AddAction(Action: TAutomationAction);
    function ShouldExecute(const CurrentTime: TDateTime): Boolean;
    procedure Execute(DeviceManager: TDeviceManager);

    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property TriggerType: TTriggerType read FTriggerType write FTriggerType;
    property ScheduledTime: TDateTime read FScheduledTime write FScheduledTime;
    property TriggerCondition: string read FTriggerCondition write FTriggerCondition;
  end;

  TAutomationManager = class
  private
    FRules: TObjectList<TAutomationRule>;
    FDeviceManager: TDeviceManager;
    FTimer: TTimer;

    procedure TimerEvent(Sender: TObject);
  public
    constructor Create(ADeviceManager: TDeviceManager);
    destructor Destroy; override;

    procedure AddRule(Rule: TAutomationRule);
    procedure RemoveRule(const RuleName: string);
    function GetRule(const RuleName: string): TAutomationRule;

    procedure Start;
    procedure Stop;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

## Définition des règles et déclencheurs

Créons maintenant un exemple de règle d'automatisation :

```pascal
// Exemple : Allumer une lampe à 18h00 tous les jours
var
  Rule: TAutomationRule;
  Action: TAutomationAction;
begin
  Rule := TAutomationRule.Create('AllumageSoir');
  Rule.TriggerType := ttScheduled;
  Rule.ScheduledTime := EncodeTime(18, 0, 0, 0); // 18:00:00
  Rule.Enabled := True;

  Action := TAutomationAction.Create('lampe_salon', 'turn_on');
  Rule.AddAction(Action);

  AutomationManager.AddRule(Rule);
end;

// Exemple : Éteindre le chauffage si la température dépasse 22°C
var
  Rule: TAutomationRule;
  Action: TAutomationAction;
begin
  Rule := TAutomationRule.Create('ControleChauffage');
  Rule.TriggerType := ttCondition;
  Rule.TriggerCondition := 'temperature > 22';
  Rule.Enabled := True;

  Action := TAutomationAction.Create('chauffage_salon', 'turn_off');
  Rule.AddAction(Action);

  AutomationManager.AddRule(Rule);
end;
```

## Journalisation et surveillance

La journalisation est essentielle pour suivre l'activité des dispositifs et détecter les problèmes. Voici une classe simple de journalisation :

```pascal
unit DeviceLogger;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  DeviceInterfaces;

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogEntry = record
    Timestamp: TDateTime;
    Level: TLogLevel;
    Source: string;
    Message: string;
  end;

  TDeviceLogger = class
  private
    FLogFile: string;
    FMemoryLog: TList<TLogEntry>;
    FMaxMemoryEntries: Integer;
    FLogToFile: Boolean;

    procedure WriteToFile(const Entry: TLogEntry);
  public
    constructor Create(const ALogFile: string = ''; AMaxMemoryEntries: Integer = 1000);
    destructor Destroy; override;

    procedure Log(Level: TLogLevel; const Source, Message: string);
    procedure Debug(const Source, Message: string);
    procedure Info(const Source, Message: string);
    procedure Warning(const Source, Message: string);
    procedure Error(const Source, Message: string);

    function GetRecentEntries(Count: Integer = 100): TArray<TLogEntry>;
    procedure ClearMemoryLog;

    property LogToFile: Boolean read FLogToFile write FLogToFile;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

## Stockage persistant des configurations

Pour conserver les configurations des dispositifs et les réglages du système, nous avons besoin d'un système de stockage persistant :

```pascal
unit ConfigStorage;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.IOUtils,
  DeviceInterfaces, DeviceManager, DeviceGroups, AutomationManager;

type
  TConfigStorage = class
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string = 'config.json');

    // Sauvegarde
    procedure SaveDeviceManager(DeviceManager: TDeviceManager);
    procedure SaveDeviceGroups(GroupManager: TDeviceGroupManager);
    procedure SaveAutomationRules(AutomationManager: TAutomationManager);
    procedure SaveSystemSettings(const Settings: TJSONObject);

    // Chargement
    procedure LoadDeviceManager(DeviceManager: TDeviceManager);
    procedure LoadDeviceGroups(GroupManager: TDeviceGroupManager);
    procedure LoadAutomationRules(AutomationManager: TAutomationManager);
    function LoadSystemSettings: TJSONObject;

    // Utilité
    function Backup(const BackupPath: string): Boolean;
    function Restore(const BackupPath: string): Boolean;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

## Sécurité et authentification

Pour les systèmes professionnels, il est important d'ajouter une couche de sécurité :

```pascal
unit SecurityManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.Hash;

type
  TUserRole = (urGuest, urUser, urAdmin);

  TUser = record
    Username: string;
    PasswordHash: string;
    Role: TUserRole;
    LastLogin: TDateTime;
    function HasPermission(const Permission: string): Boolean;
  end;

  TSecurityManager = class
  private
    FUsers: TDictionary<string, TUser>;
    FCurrentUser: TUser;
    FIsAuthenticated: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Authenticate(const Username, Password: string): Boolean;
    procedure Logout;

    function CreateUser(const Username, Password: string; Role: TUserRole): Boolean;
    function DeleteUser(const Username: string): Boolean;
    function ChangePassword(const Username, OldPassword, NewPassword: string): Boolean;

    function CheckPermission(const Permission: string): Boolean;

    property IsAuthenticated: Boolean read FIsAuthenticated;
    property CurrentUser: TUser read FCurrentUser;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

## Application complète: tableau de bord IoT

Maintenant, assemblons tout pour créer un tableau de bord IoT complet. Voici la structure du projet principal :

```pascal
unit MainDashboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Menus,

  // Nos unités personnalisées
  DeviceInterfaces, DeviceManager, MQTTDevice, DeviceGroups,
  AutomationManager, DeviceLogger, ConfigStorage, SecurityManager,
  DashboardFrame;

type
  TfrmMainDashboard = class(TForm)
    pgcMain: TPageControl;
    tabDashboard: TTabSheet;
    tabDevices: TTabSheet;
    tabGroups: TTabSheet;
    tabAutomation: TTabSheet;
    tabSettings: TTabSheet;
    mainMenu: TMainMenu;
    mnuFile: TMenuItem;
    mnuFileExit: TMenuItem;
    mnuFileBackup: TMenuItem;
    mnuFileRestore: TMenuItem;
    mnuTools: TMenuItem;
    mnuToolsDiscoverDevices: TMenuItem;
    mnuToolsLog: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    stsMain: TStatusBar;
    // ... autres composants ...

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure mnuFileBackupClick(Sender: TObject);
    procedure mnuFileRestoreClick(Sender: TObject);
    procedure mnuToolsDiscoverDevicesClick(Sender: TObject);
    procedure mnuToolsLogClick(Sender: TObject);
    // ... autres gestionnaires ...

  private
    FDeviceManager: TDeviceManager;
    FGroupManager: TDeviceGroupManager;
    FAutomationManager: TAutomationManager;
    FLogger: TDeviceLogger;
    FConfigStorage: TConfigStorage;
    FSecurityManager: TSecurityManager;

    // Frames contenus dans les onglets
    FDashboardFrame: TDashboardFrame;
    // ... autres frames ...

    procedure InitializeManagers;
    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure HandleDeviceDiscovered(Sender: TObject; Device: IDevice);
    procedure HandleDeviceStatusChanged(Sender: TObject);
    procedure UpdateStatusBar;
  public
    { Public declarations }
  end;

var
  frmMainDashboard: TfrmMainDashboard;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

## Conclusion

La création d'un système de gestion de dispositifs connectés est un processus complexe qui implique plusieurs couches logicielles. Dans ce tutoriel, nous avons exploré :

1. **L'architecture d'un système de gestion de dispositifs** - Avec des interfaces génériques permettant d'intégrer n'importe quel type de dispositif
2. **L'implémentation de dispositifs MQTT** - Permettant la communication avec une variété d'appareils IoT
3. **L'interface utilisateur de gestion** - Pour visualiser et contrôler les dispositifs
4. **Le tableau de bord dynamique** - Pour surveiller l'état des dispositifs en temps réel
5. **L'automatisation et les règles** - Pour créer des comportements intelligents
6. **La journalisation et la persistance** - Pour suivre l'activité et conserver les configurations
7. **La sécurité** - Pour protéger l'accès au système

Ce framework peut être étendu pour répondre à des besoins spécifiques, comme l'intégration avec d'autres protocoles IoT, l'ajout de visualisations avancées, ou la mise en œuvre de fonctionnalités d'intelligence artificielle pour l'analyse prédictive.

## Bonnes pratiques

Pour finir, voici quelques bonnes pratiques à suivre lors de la création d'un système de gestion de dispositifs :

1. **Architecture modulaire** - Concevez votre système avec des composants distincts et bien définis
2. **Interfaces génériques** - Utilisez des interfaces pour permettre l'extension future
3. **Gestion des erreurs robuste** - Les communications réseau peuvent échouer, prévoyez des mécanismes de récupération
4. **Performances** - Utilisez des threads séparés pour les communications afin de garantir une interface réactive
5. **Sécurité** - Protégez les communications et les accès aux dispositifs
6. **Flexibilité** - Prévoyez l'ajout de nouveaux types de dispositifs et protocoles
7. **Documentation** - Documentez clairement l'architecture et les interfaces pour faciliter la maintenance

## Exercices pratiques

1. Étendez le système pour prendre en charge les dispositifs Bluetooth Low Energy (BLE)
2. Ajoutez une carte interactive qui montre l'emplacement des dispositifs
3. Implémentez un système d'alertes pour les conditions anormales
4. Créez un module de rapports pour analyser les données historiques
5. Développez une application mobile compagnon qui se connecte au gestionnaire de dispositifs

## Ressources supplémentaires

- [Documentation MQTT](https://mqtt.org/)
- [Spécification CoAP](https://tools.ietf.org/html/rfc7252)
- [IoT Design Patterns](https://www.oreilly.com/library/view/design-patterns-for/9781492079576/)
- [Sécurité IoT](https://www.iotsecurityfoundation.org/)

Dans la prochaine section, nous explorerons le traitement des données IoT en temps réel pour extraire des informations utiles de vos dispositifs connectés.

⏭️ [Traitement des données IoT en temps réel](/21-delphi-et-liot/07-traitement-des-donnees-iot-en-temps-reel.md)
