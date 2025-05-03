# 21.7 Traitement des données IoT en temps réel

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans les sections précédentes, nous avons appris à communiquer avec des périphériques IoT et à les gérer. L'étape suivante consiste à traiter et à analyser efficacement le flux continu de données que ces dispositifs génèrent. Cette section vous guidera à travers les concepts et techniques fondamentaux pour transformer des données IoT brutes en informations utiles et exploitables.

## Introduction au traitement de données IoT

Les dispositifs IoT peuvent générer d'énormes quantités de données en temps réel. Par exemple :
- Un capteur de température peut envoyer une mesure toutes les secondes
- Un réseau de capteurs dans une usine peut produire des milliers de points de données par minute
- Des appareils portables peuvent transmettre continuellement des données physiologiques

Le traitement de ces données en temps réel présente plusieurs défis :
- **Volume** : gérer de grandes quantités de données
- **Vélocité** : traiter les données à mesure qu'elles arrivent
- **Variété** : manipuler différents types de données
- **Fiabilité** : assurer que le traitement ne manque aucune donnée importante
- **Performance** : maintenir l'interface utilisateur réactive pendant le traitement

## Architecture de traitement en temps réel

Pour traiter efficacement les données IoT, nous allons utiliser une architecture en couches :

1. **Acquisition de données** - Réception des données brutes des dispositifs
2. **Filtrage et validation** - Élimination des données erronées ou aberrantes
3. **Traitement** - Transformation des données brutes en informations utiles
4. **Analyse** - Extraction d'insights à partir des données traitées
5. **Visualisation** - Présentation des informations sous forme visuelle
6. **Stockage** - Conservation des données pour analyse historique

![Architecture de traitement](https://via.placeholder.com/800x400.png?text=Architecture+de+traitement+en+temps+r%C3%A9el)

## Implémentation d'un système de traitement de données

Commençons par créer une architecture de base pour notre système de traitement de données IoT.

### Définition des types de données

```pascal
unit IoTDataTypes;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.JSON,
  System.DateUtils;

type
  // Type de donnée IoT
  TIoTDataType = (dtUnknown, dtNumeric, dtBoolean, dtText, dtLocation, dtImage, dtBinary);

  // Structure générique pour une donnée IoT
  TIoTData = record
    DeviceID: string;     // Identifiant du dispositif source
    SensorID: string;     // Identifiant du capteur spécifique
    Timestamp: TDateTime; // Horodatage de la mesure
    DataType: TIoTDataType; // Type de la donnée

    // Valeurs selon le type (union)
    case Integer of
      0: (NumericValue: Double);    // Pour dtNumeric
      1: (BooleanValue: Boolean);   // Pour dtBoolean
      2: (TextValue: string);       // Pour dtText
      3: (                          // Pour dtLocation
        Latitude: Double;
        Longitude: Double;
        Altitude: Double;
      );
      4: (BinaryData: TBytes);      // Pour dtBinary/dtImage

    // Méthodes
    function AsJSON: TJSONObject;
    function AsString: string;
    function AgeInSeconds: Integer;
  end;

  // File de données IoT thread-safe
  TIoTDataQueue = class
  private
    FQueue: TQueue<TIoTData>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(const Data: TIoTData);
    function TryDequeue(out Data: TIoTData): Boolean;
    function Count: Integer;
    procedure Clear;
  end;

  // Gestionnaire d'événements pour les nouvelles données
  TIoTDataEvent = procedure(Sender: TObject; const Data: TIoTData) of object;

implementation

// ... implémentation des méthodes ...

end;
```

### Création d'un processeur de données de base

```pascal
unit IoTDataProcessor;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  IoTDataTypes;

type
  // Interface pour un processeur de données IoT
  IIoTDataProcessor = interface
    ['{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}'] // Générez un GUID unique
    procedure ProcessData(const Data: TIoTData);
    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;
  end;

  // Classe de base pour les processeurs
  TIoTDataProcessorBase = class(TInterfacedObject, IIoTDataProcessor)
  private
    FIsRunning: Boolean;
    FOnDataProcessed: TIoTDataEvent;
  protected
    procedure DoDataProcessed(const Data: TIoTData); virtual;
  public
    constructor Create;

    // Implémentation de IIoTDataProcessor
    procedure ProcessData(const Data: TIoTData); virtual; abstract;
    procedure Start; virtual;
    procedure Stop; virtual;
    function IsRunning: Boolean;

    property OnDataProcessed: TIoTDataEvent read FOnDataProcessed write FOnDataProcessed;
  end;

  // Processeur qui filtre les valeurs hors limites
  TRangeLimitProcessor = class(TIoTDataProcessorBase)
  private
    FMinValue: Double;
    FMaxValue: Double;
    FSensorID: string;
  public
    constructor Create(const ASensorID: string; AMinValue, AMaxValue: Double);
    procedure ProcessData(const Data: TIoTData); override;
  end;

  // Processeur qui calcule une moyenne mobile
  TMovingAverageProcessor = class(TIoTDataProcessorBase)
  private
    FValues: TQueue<Double>;
    FWindowSize: Integer;
    FSensorID: string;
    FSumValues: Double;
  public
    constructor Create(const ASensorID: string; AWindowSize: Integer = 10);
    destructor Destroy; override;
    procedure ProcessData(const Data: TIoTData); override;
  end;

  // ... autres processeurs spécialisés ...

implementation

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation de quelques méthodes importantes :

```pascal
constructor TIoTDataProcessorBase.Create;
begin
  inherited Create;
  FIsRunning := False;
end;

procedure TIoTDataProcessorBase.Start;
begin
  FIsRunning := True;
end;

procedure TIoTDataProcessorBase.Stop;
begin
  FIsRunning := False;
end;

function TIoTDataProcessorBase.IsRunning: Boolean;
begin
  Result := FIsRunning;
end;

procedure TIoTDataProcessorBase.DoDataProcessed(const Data: TIoTData);
begin
  if Assigned(FOnDataProcessed) then
    FOnDataProcessed(Self, Data);
end;

// Implémentation du processeur de limites
constructor TRangeLimitProcessor.Create(const ASensorID: string; AMinValue, AMaxValue: Double);
begin
  inherited Create;
  FSensorID := ASensorID;
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
end;

procedure TRangeLimitProcessor.ProcessData(const Data: TIoTData);
begin
  // Vérifier si c'est le bon capteur et le bon type de données
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ne traiter que si la valeur est dans les limites
    if (Data.NumericValue >= FMinValue) and (Data.NumericValue <= FMaxValue) then
      DoDataProcessed(Data);
    // On pourrait aussi enregistrer ou signaler les valeurs hors limites
  end
  else
    // Si ce n'est pas notre capteur, laisser passer la donnée sans modification
    DoDataProcessed(Data);
end;

// Implémentation du processeur de moyenne mobile
constructor TMovingAverageProcessor.Create(const ASensorID: string; AWindowSize: Integer = 10);
begin
  inherited Create;
  FSensorID := ASensorID;
  FWindowSize := AWindowSize;
  FValues := TQueue<Double>.Create;
  FSumValues := 0;
end;

destructor TMovingAverageProcessor.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TMovingAverageProcessor.ProcessData(const Data: TIoTData);
var
  AverageData: TIoTData;
  OldestValue: Double;
begin
  // Vérifier si c'est le bon capteur et le bon type de données
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter la nouvelle valeur à la file
    FValues.Enqueue(Data.NumericValue);
    FSumValues := FSumValues + Data.NumericValue;

    // Si la file dépasse la taille de la fenêtre, retirer la plus ancienne valeur
    if FValues.Count > FWindowSize then
    begin
      OldestValue := FValues.Dequeue;
      FSumValues := FSumValues - OldestValue;
    end;

    // Créer une nouvelle structure de données avec la moyenne calculée
    AverageData := Data;
    AverageData.NumericValue := FSumValues / FValues.Count;

    // Envoyer la donnée traitée
    DoDataProcessed(AverageData);
  end
  else
    // Si ce n'est pas notre capteur, laisser passer la donnée sans modification
    DoDataProcessed(Data);
end;
```

### Création d'une chaîne de traitement

Nous pouvons maintenant créer une chaîne de traitement (pipeline) qui connecte plusieurs processeurs ensemble :

```pascal
unit IoTDataPipeline;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  IoTDataTypes, IoTDataProcessor;

type
  TIoTDataPipeline = class
  private
    FProcessors: TList<IIoTDataProcessor>;
    FInputQueue: TIoTDataQueue;
    FOutputQueue: TIoTDataQueue;
    FProcessingThread: TThread;
    FLock: TCriticalSection;
    FIsRunning: Boolean;
    FOnDataProcessed: TIoTDataEvent;

    procedure ProcessQueuedData;
    procedure HandleProcessedData(Sender: TObject; const Data: TIoTData);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddProcessor(Processor: IIoTDataProcessor);
    procedure RemoveProcessor(Processor: IIoTDataProcessor);
    procedure ClearProcessors;

    procedure EnqueueData(const Data: TIoTData);

    procedure Start;
    procedure Stop;
    function IsRunning: Boolean;

    property OnDataProcessed: TIoTDataEvent read FOnDataProcessed write FOnDataProcessed;
    property InputQueue: TIoTDataQueue read FInputQueue;
    property OutputQueue: TIoTDataQueue read FOutputQueue;
  end;

implementation

type
  TProcessingThread = class(TThread)
  private
    FPipeline: TIoTDataPipeline;
  protected
    procedure Execute; override;
  public
    constructor Create(APipeline: TIoTDataPipeline);
  end;

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation des méthodes principales :

```pascal
constructor TIoTDataPipeline.Create;
begin
  inherited Create;
  FProcessors := TList<IIoTDataProcessor>.Create;
  FInputQueue := TIoTDataQueue.Create;
  FOutputQueue := TIoTDataQueue.Create;
  FLock := TCriticalSection.Create;
  FIsRunning := False;
end;

destructor TIoTDataPipeline.Destroy;
begin
  Stop;
  FProcessors.Free;
  FInputQueue.Free;
  FOutputQueue.Free;
  FLock.Free;
  inherited;
end;

procedure TIoTDataPipeline.AddProcessor(Processor: IIoTDataProcessor);
begin
  FLock.Enter;
  try
    FProcessors.Add(Processor);
    // Connecter les processeurs ensemble
    if Processor is TIoTDataProcessorBase then
      (Processor as TIoTDataProcessorBase).OnDataProcessed := HandleProcessedData;
  finally
    FLock.Release;
  end;
end;

procedure TIoTDataPipeline.EnqueueData(const Data: TIoTData);
begin
  FInputQueue.Enqueue(Data);
end;

procedure TIoTDataPipeline.HandleProcessedData(Sender: TObject; const Data: TIoTData);
begin
  // Mettre les données traitées dans la file de sortie
  FOutputQueue.Enqueue(Data);

  // Déclencher l'événement
  if Assigned(FOnDataProcessed) then
    FOnDataProcessed(Self, Data);
end;

procedure TIoTDataPipeline.Start;
begin
  if not FIsRunning then
  begin
    FIsRunning := True;

    // Démarrer tous les processeurs
    for var Processor in FProcessors do
      Processor.Start;

    // Créer et démarrer le thread de traitement
    FProcessingThread := TProcessingThread.Create(Self);
    FProcessingThread.FreeOnTerminate := False;
    FProcessingThread.Start;
  end;
end;

procedure TIoTDataPipeline.Stop;
begin
  if FIsRunning then
  begin
    FIsRunning := False;

    // Arrêter tous les processeurs
    for var Processor in FProcessors do
      Processor.Stop;

    // Attendre la fin du thread et le libérer
    if Assigned(FProcessingThread) then
    begin
      FProcessingThread.Terminate;
      FProcessingThread.WaitFor;
      FProcessingThread.Free;
      FProcessingThread := nil;
    end;
  end;
end;

// Thread de traitement
constructor TProcessingThread.Create(APipeline: TIoTDataPipeline);
begin
  inherited Create(True); // Créer suspendu
  FPipeline := APipeline;
end;

procedure TProcessingThread.Execute;
begin
  while not Terminated do
  begin
    // Traiter les données en attente
    FPipeline.ProcessQueuedData;

    // Pause courte pour éviter de consommer trop de CPU
    Sleep(1);
  end;
end;

procedure TIoTDataPipeline.ProcessQueuedData;
var
  Data: TIoTData;
  TempData: TIoTData;
begin
  // Traiter les données en attente dans la file d'entrée
  while FInputQueue.TryDequeue(Data) and not FProcessingThread.Terminated do
  begin
    // Passer les données à travers la chaîne de processeurs
    TempData := Data;

    FLock.Enter;
    try
      for var Processor in FProcessors do
      begin
        if Processor.IsRunning then
          Processor.ProcessData(TempData);
      end;
    finally
      FLock.Release;
    end;
  end;
end;
```

## Visualisation des données en temps réel

Une fois que nous avons traité les données, nous voulons les visualiser de manière interactive. Voici comment créer un graphique en temps réel pour les données IoT :

```pascal
unit RealTimeChartFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart,
  IoTDataTypes;

type
  TRealTimeChartFrame = class(TFrame)
    Chart: TChart;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    FSeries: TFastLineSeries;
    FDataQueue: TIoTDataQueue;
    FMaxPoints: Integer;
    FAutoScroll: Boolean;
    FSensorID: string;

    procedure HandleNewData(const Data: TIoTData);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDataQueue(AQueue: TIoTDataQueue);
    procedure SetSensorID(const ID: string);
    procedure Clear;

    property MaxPoints: Integer read FMaxPoints write FMaxPoints;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
  end;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation de quelques méthodes clés :

```pascal
constructor TRealTimeChartFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Créer la série pour le graphique
  FSeries := TFastLineSeries.Create(Chart);
  FSeries.Title := 'Données en temps réel';
  FSeries.LinePen.Width := 2;
  FSeries.LinePen.Color := clBlue;
  Chart.AddSeries(FSeries);

  // Configuration par défaut
  FMaxPoints := 100; // 100 points maximum à l'écran
  FAutoScroll := True;

  // Configurer le graphique
  Chart.Title.Text.Text := 'Données capteur en temps réel';
  Chart.BottomAxis.Title.Caption := 'Temps (secondes)';
  Chart.LeftAxis.Title.Caption := 'Valeur';
  Chart.Legend.Visible := True;

  // Activer le timer de mise à jour
  Timer.Interval := 100; // Mise à jour 10 fois par seconde
  Timer.Enabled := True;
end;

procedure TRealTimeChartFrame.SetDataQueue(AQueue: TIoTDataQueue);
begin
  FDataQueue := AQueue;
end;

procedure TRealTimeChartFrame.SetSensorID(const ID: string);
begin
  FSensorID := ID;
  FSeries.Title := 'Données ' + ID;
  Chart.Title.Text.Text := 'Données capteur ' + ID + ' en temps réel';
end;

procedure TRealTimeChartFrame.HandleNewData(const Data: TIoTData);
var
  TimeValue: Double;
begin
  // Vérifier si c'est le capteur que nous suivons
  if (FSensorID = '') or (Data.SensorID = FSensorID) then
  begin
    if Data.DataType = dtNumeric then
    begin
      // Calculer le temps relatif (en secondes depuis le début)
      if FSeries.Count = 0 then
        TimeValue := 0
      else
        TimeValue := FSeries.XValues[FSeries.Count - 1] +
                    (SecondsBetween(Data.Timestamp, Now) -
                     SecondsBetween(Data.Timestamp, Now));

      // Ajouter le point au graphique
      FSeries.AddXY(TimeValue, Data.NumericValue);

      // Limiter le nombre de points si nécessaire
      if (FMaxPoints > 0) and (FSeries.Count > FMaxPoints) then
      begin
        FSeries.Delete(0);

        // Ajuster l'axe X pour faire défiler automatiquement
        if FAutoScroll then
        begin
          Chart.BottomAxis.Automatic := False;
          Chart.BottomAxis.SetMinMax(FSeries.XValues[0],
                                    FSeries.XValues[FSeries.Count - 1]);
        end;
      end;
    end;
  end;
end;

procedure TRealTimeChartFrame.TimerTimer(Sender: TObject);
var
  Data: TIoTData;
begin
  // Vérifier s'il y a des données à traiter
  if Assigned(FDataQueue) then
  begin
    // Traiter toutes les données en attente
    while FDataQueue.TryDequeue(Data) do
      HandleNewData(Data);
  end;
end;

procedure TRealTimeChartFrame.Clear;
begin
  FSeries.Clear;
end;
```

## Intégration avec un tableau de bord IoT

Maintenant, créons un tableau de bord complet qui intègre notre pipeline de traitement et nos visualisations :

```pascal
unit IoTDashboardForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons,

  // Nos unités
  IoTDataTypes, IoTDataProcessor, IoTDataPipeline, RealTimeChartFrame,
  DeviceInterfaces; // Notre interface de dispositif des sections précédentes

type
  TfrmIoTDashboard = class(TForm)
    pnlTop: TPanel;
    pnlCharts: TPanel;
    pnlControls: TPanel;
    btnStartProcessing: TButton;
    btnStopProcessing: TButton;
    lblStatus: TLabel;
    cmbDevices: TComboBox;
    cmbSensors: TComboBox;
    btnAddChart: TButton;
    pgcCharts: TPageControl;
    statusBar: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnStartProcessingClick(Sender: TObject);
    procedure btnStopProcessingClick(Sender: TObject);
    procedure btnAddChartClick(Sender: TObject);
    procedure cmbDevicesChange(Sender: TObject);
    procedure cmbSensorsChange(Sender: TObject);

  private
    FPipeline: TIoTDataPipeline;
    FDeviceManager: TDeviceManager; // Gestionnaire de dispositifs de la section précédente
    FCharts: TList<TRealTimeChartFrame>;

    procedure HandleNewData(Sender: TObject; const Data: TIoTData);
    procedure HandleDeviceData(Device: IDevice; const SensorID: string;
                             const Value: TDeviceValue);
    procedure RefreshDeviceList;
    procedure RefreshSensorList;
  public
    { Public declarations }
  end;

var
  frmIoTDashboard: TfrmIoTDashboard;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation des méthodes principales :

```pascal
procedure TfrmIoTDashboard.FormCreate(Sender: TObject);
begin
  // Initialiser les listes et structures
  FCharts := TList<TRealTimeChartFrame>.Create;

  // Créer notre pipeline de traitement
  FPipeline := TIoTDataPipeline.Create;
  FPipeline.OnDataProcessed := HandleNewData;

  // Ajouter quelques processeurs
  // Par exemple, un filtre pour éliminer les valeurs aberrantes de température
  FPipeline.AddProcessor(TRangeLimitProcessor.Create('temperature', -50, 100));

  // Et un processeur de moyenne mobile pour lisser les données
  FPipeline.AddProcessor(TMovingAverageProcessor.Create('temperature', 10));

  // Initialiser l'interface
  RefreshDeviceList;

  // Démarrer automatiquement le traitement
  btnStartProcessingClick(nil);
end;

procedure TfrmIoTDashboard.FormDestroy(Sender: TObject);
begin
  // Arrêter le traitement
  if Assigned(FPipeline) then
  begin
    FPipeline.Stop;
    FPipeline.Free;
  end;

  // Libérer les ressources
  FCharts.Free;
end;

procedure TfrmIoTDashboard.RefreshDeviceList;
var
  Devices: TArray<IDevice>;
begin
  // Vider la liste
  cmbDevices.Items.Clear;

  // Ajouter un élément pour "Tous les dispositifs"
  cmbDevices.Items.Add('Tous les dispositifs');

  // Obtenir la liste des dispositifs
  if Assigned(FDeviceManager) then
  begin
    Devices := FDeviceManager.GetAllDevices;
    for var Device in Devices do
    begin
      cmbDevices.Items.Add(Device.Name + ' (' + Device.ID + ')');
    end;
  end;

  // Sélectionner le premier élément
  if cmbDevices.Items.Count > 0 then
    cmbDevices.ItemIndex := 0;

  // Mettre à jour la liste des capteurs
  RefreshSensorList;
end;

procedure TfrmIoTDashboard.RefreshSensorList;
var
  DeviceID: string;
  Device: IDevice;
  Devices: TArray<IDevice>;
  PropertyPair: TPair<string, TDeviceValue>;
begin
  // Vider la liste
  cmbSensors.Items.Clear;

  // Ajouter un élément pour "Tous les capteurs"
  cmbSensors.Items.Add('Tous les capteurs');

  // Obtenir l'ID du dispositif sélectionné
  if cmbDevices.ItemIndex > 0 then
  begin
    // Extraire l'ID
    var S := cmbDevices.Items[cmbDevices.ItemIndex];
    var StartPos := Pos('(', S) + 1;
    var EndPos := Pos(')', S) - 1;
    if (StartPos > 0) and (EndPos > StartPos) then
    begin
      DeviceID := Copy(S, StartPos, EndPos - StartPos + 1);

      // Obtenir le dispositif
      Device := FDeviceManager.GetDevice(DeviceID);
      if Assigned(Device) then
      begin
        // Parcourir les propriétés du dispositif pour trouver les capteurs
        for PropertyPair in Device.Properties do
        begin
          if (PropertyPair.Value.ValueType = dvtNumeric) or
             (PropertyPair.Value.ValueType = dvtFloat) then
            cmbSensors.Items.Add(PropertyPair.Key);
        end;
      end;
    end;
  end
  else
  begin
    // Pour "Tous les dispositifs", obtenir tous les capteurs uniques
    Devices := FDeviceManager.GetAllDevices;
    var SensorNames := TDictionary<string, Boolean>.Create;
    try
      for Device in Devices do
      begin
        for PropertyPair in Device.Properties do
        begin
          if ((PropertyPair.Value.ValueType = dvtNumeric) or
              (PropertyPair.Value.ValueType = dvtFloat)) and
              not SensorNames.ContainsKey(PropertyPair.Key) then
          begin
            SensorNames.Add(PropertyPair.Key, True);
            cmbSensors.Items.Add(PropertyPair.Key);
          end;
        end;
      end;
    finally
      SensorNames.Free;
    end;
  end;

  // Sélectionner le premier élément
  if cmbSensors.Items.Count > 0 then
    cmbSensors.ItemIndex := 0;
end;

procedure TfrmIoTDashboard.btnStartProcessingClick(Sender: TObject);
begin
  // Démarrer le pipeline
  FPipeline.Start;

  // Mettre à jour l'interface
  btnStartProcessing.Enabled := False;
  btnStopProcessing.Enabled := True;
  lblStatus.Caption := 'Statut : Traitement en cours';
end;

procedure TfrmIoTDashboard.btnStopProcessingClick(Sender: TObject);
begin
  // Arrêter le pipeline
  FPipeline.Stop;

  // Mettre à jour l'interface
  btnStartProcessing.Enabled := True;
  btnStopProcessing.Enabled := False;
  lblStatus.Caption := 'Statut : Traitement arrêté';
end;

procedure TfrmIoTDashboard.btnAddChartClick(Sender: TObject);
var
  NewTab: TTabSheet;
  ChartFrame: TRealTimeChartFrame;
  SensorID: string;
begin
  // Créer un nouvel onglet
  NewTab := TTabSheet.Create(pgcCharts);
  NewTab.PageControl := pgcCharts;

  // Déterminer l'ID du capteur
  if cmbSensors.ItemIndex > 0 then
    SensorID := cmbSensors.Items[cmbSensors.ItemIndex]
  else
    SensorID := '';

  // Définir le nom de l'onglet
  if SensorID <> '' then
    NewTab.Caption := 'Graphique - ' + SensorID
  else
    NewTab.Caption := 'Graphique - Tous les capteurs';

  // Créer le frame de graphique
  ChartFrame := TRealTimeChartFrame.Create(NewTab);
  ChartFrame.Parent := NewTab;
  ChartFrame.Align := alClient;
  ChartFrame.SetDataQueue(FPipeline.OutputQueue);
  ChartFrame.SetSensorID(SensorID);

  // Ajouter à notre liste
  FCharts.Add(ChartFrame);

  // Sélectionner le nouvel onglet
  pgcCharts.ActivePage := NewTab;
end;

procedure TfrmIoTDashboard.HandleNewData(Sender: TObject; const Data: TIoTData);
begin
  // Cette méthode est appelée chaque fois qu'une donnée traverse le pipeline
  // Elle est exécutée dans le thread de traitement, donc nous devons synchroniser
  // si nous voulons mettre à jour l'interface utilisateur

  TThread.Queue(nil, procedure
  begin
    // Mettre à jour la barre d'état
    statusBar.Panels[0].Text := 'Dernière donnée: ' + Data.SensorID;
    statusBar.Panels[1].Text := 'Valeur: ' + Data.AsString;
    statusBar.Panels[2].Text := 'Horodatage: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Data.Timestamp);
  end);
end;

procedure TfrmIoTDashboard.HandleDeviceData(Device: IDevice; const SensorID: string;
                                         const Value: TDeviceValue);
var
  Data: TIoTData;
begin
  // Convertir les données du dispositif en format IoTData
  FillChar(Data, SizeOf(Data), 0);
  Data.DeviceID := Device.ID;
  Data.SensorID := SensorID;
  Data.Timestamp := Now;

  // Déterminer le type de données
  case Value.ValueType of
    dvtString:
      begin
        Data.DataType := dtText;
        Data.TextValue := Value.StringValue;
      end;
    dvtInteger:
      begin
        Data.DataType := dtNumeric;
        Data.NumericValue := Value.IntegerValue;
      end;
    dvtFloat:
      begin
        Data.DataType := dtNumeric;
        Data.NumericValue := Value.FloatValue;
      end;
    dvtBoolean:
      begin
        Data.DataType := dtBoolean;
        Data.BooleanValue := Value.BooleanValue;
      end;
    // ... autres conversions selon les besoins ...
  end;

  // Ajouter les données à notre pipeline
  FPipeline.EnqueueData(Data);
end;
```

## Création de processeurs de données avancés

Notre système de base est fonctionnel, mais pour tirer pleinement parti des données IoT, nous avons besoin de processeurs plus avancés. Voici quelques exemples :

### Détection de seuils et alertes

```pascal
// Processeur qui détecte les franchissements de seuil
TThresholdDetectorProcessor = class(TIoTDataProcessorBase)
private
  FThreshold: Double;
  FIsAboveThreshold: Boolean;
  FHysteresis: Double;
  FSensorID: string;
  FOnThresholdCrossed: TNotifyEvent;
public
  constructor Create(const ASensorID: string; AThreshold: Double; AHysteresis: Double = 0);
  procedure ProcessData(const Data: TIoTData); override;

  property OnThresholdCrossed: TNotifyEvent read FOnThresholdCrossed write FOnThresholdCrossed;
end;

constructor TThresholdDetectorProcessor.Create(const ASensorID: string; AThreshold: Double; AHysteresis: Double = 0);
begin
  inherited Create;
  FSensorID := ASensorID;
  FThreshold := AThreshold;
  FHysteresis := AHysteresis;
  FIsAboveThreshold := False;
end;

procedure TThresholdDetectorProcessor.ProcessData(const Data: TIoTData);
var
  CrossedThreshold: Boolean;
begin
  // Passer la donnée au processeur suivant
  DoDataProcessed(Data);

  // Puis vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    CrossedThreshold := False;

    if not FIsAboveThreshold and (Data.NumericValue >= FThreshold) then
    begin
      // Passage de en-dessous à au-dessus du seuil
      FIsAboveThreshold := True;
      CrossedThreshold := True;
    end
    else if FIsAboveThreshold and (Data.NumericValue <= (FThreshold - FHysteresis)) then
    begin
      // Passage de au-dessus à en-dessous du seuil, avec hystérésis
      FIsAboveThreshold := False;
      CrossedThreshold := True;
    end;

    // Signaler le franchissement s'il y a lieu
    if CrossedThreshold and Assigned(FOnThresholdCrossed) then
    begin
      TThread.Queue(nil, procedure
      begin
        FOnThresholdCrossed(Self);
      end);
    end;
  end;
end;
```

### Détection de tendances

```pascal
// Processeur qui détecte les tendances à la hausse ou à la baisse
TTrendDetectorProcessor = class(TIoTDataProcessorBase)
private
  FValues: TQueue<Double>;
  FWindowSize: Integer;
  FSensorID: string;
  FMinSlope: Double;
  FOnTrendDetected: TNotifyEvent;
  FCurrentTrend: Double; // Positive = hausse, Negative = baisse, Zero = stable
public
  constructor Create(const ASensorID: string; AWindowSize: Integer = 10; AMinSlope: Double = 0.1);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;

  property CurrentTrend: Double read FCurrentTrend;
  property OnTrendDetected: TNotifyEvent read FOnTrendDetected write FOnTrendDetected;
end;

constructor TTrendDetectorProcessor.Create(const ASensorID: string; AWindowSize: Integer = 10; AMinSlope: Double = 0.1);
begin
  inherited Create;
  FSensorID := ASensorID;
  FWindowSize := AWindowSize;
  FMinSlope := AMinSlope;
  FValues := TQueue<Double>.Create;
  FCurrentTrend := 0;
end;

destructor TTrendDetectorProcessor.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TTrendDetectorProcessor.ProcessData(const Data: TIoTData);
var
  XValues, YValues: array of Double;
  SumX, SumY, SumXY, SumX2: Double;
  Slope: Double;
  N: Integer;
  OldTrend: Double;
  I: Integer;
begin
  // Passer la donnée au processeur suivant
  DoDataProcessed(Data);

  // Puis vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter la nouvelle valeur
    FValues.Enqueue(Data.NumericValue);

    // Limiter la taille de la fenêtre
    while FValues.Count > FWindowSize do
      FValues.Dequeue;

    // Calculer la tendance linéaire si nous avons assez de données
    if FValues.Count >= 3 then // Minimum pour une régression linéaire
    begin
      OldTrend := FCurrentTrend;

      // Préparer les données pour la régression
      N := FValues.Count;
      SetLength(XValues, N);
      SetLength(YValues, N);

      // Remplir les tableaux avec les données
      I := 0;
      for var Value in FValues do
      begin
        XValues[I] := I;
        YValues[I] := Value;
        Inc(I);
      end;

      // Calculer les sommes pour la régression linéaire
      SumX := 0;
      SumY := 0;
      SumXY := 0;
      SumX2 := 0;

      for I := 0 to N - 1 do
      begin
        SumX := SumX + XValues[I];
        SumY := SumY + YValues[I];
        SumXY := SumXY + XValues[I] * YValues[I];
        SumX2 := SumX2 + XValues[I] * XValues[I];
      end;

      // Calculer la pente (m) dans y = mx + b
      Slope := (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX);

      // Mettre à jour la tendance actuelle
      FCurrentTrend := Slope;

      // Vérifier si la tendance a changé significativement
      if (Abs(Slope) >= FMinSlope) and
         ((OldTrend * Slope <= 0) or        // Changement de signe
          (Abs(Slope - OldTrend) >= FMinSlope)) // Changement significatif de magnitude
      then
      begin
        // Signaler la détection d'une tendance
        if Assigned(FOnTrendDetected) then
        begin
          TThread.Queue(nil, procedure
          begin
            FOnTrendDetected(Self);
          end);
        end;
      end;
    end;
  end;
end;
```

### Agrégation de données

```pascal
// Processeur qui agrège les données sur une période définie
TDataAggregatorProcessor = class(TIoTDataProcessorBase)
private
  FSensorID: string;
  FPeriod: TDateTime;  // Période d'agrégation en jours
  FLastProcessTime: TDateTime;
  FValues: TList<Double>;

  // Statistiques agrégées
  FCount: Integer;
  FSum: Double;
  FMin: Double;
  FMax: Double;

  procedure ResetAggregation;
  procedure SendAggregatedData;
public
  constructor Create(const ASensorID: string; APeriodInMinutes: Integer = 60);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;
end;

constructor TDataAggregatorProcessor.Create(const ASensorID: string; APeriodInMinutes: Integer = 60);
begin
  inherited Create;
  FSensorID := ASensorID;
  FPeriod := APeriodInMinutes / (24 * 60); // Convertir minutes en jours
  FLastProcessTime := 0;
  FValues := TList<Double>.Create;
  ResetAggregation;
end;

destructor TDataAggregatorProcessor.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TDataAggregatorProcessor.ResetAggregation;
begin
  FCount := 0;
  FSum := 0;
  FMin := MaxDouble;
  FMax := -MaxDouble;
  FValues.Clear;
end;

procedure TDataAggregatorProcessor.SendAggregatedData;
var
  Data: TIoTData;
  Avg, StdDev, Variance, Sum2: Double;
  Value: Double;
begin
  if FCount > 0 then
  begin
    // Calculer les statistiques
    Avg := FSum / FCount;

    // Calculer l'écart-type
    Sum2 := 0;
    for Value in FValues do
      Sum2 := Sum2 + Sqr(Value - Avg);

    if FCount > 1 then
      Variance := Sum2 / (FCount - 1)
    else
      Variance := 0;

    StdDev := Sqrt(Variance);

    // Créer des entrées pour chaque statistique

    // Moyenne
    FillChar(Data, SizeOf(Data), 0);
    Data.DeviceID := 'aggregator';
    Data.SensorID := FSensorID + '_avg';
    Data.Timestamp := Now;
    Data.DataType := dtNumeric;
    Data.NumericValue := Avg;
    DoDataProcessed(Data);

    // Minimum
    Data.SensorID := FSensorID + '_min';
    Data.NumericValue := FMin;
    DoDataProcessed(Data);

    // Maximum
    Data.SensorID := FSensorID + '_max';
    Data.NumericValue := FMax;
    DoDataProcessed(Data);

    // Écart-type
    Data.SensorID := FSensorID + '_stddev';
    Data.NumericValue := StdDev;
    DoDataProcessed(Data);

    // Nombre de points
    Data.SensorID := FSensorID + '_count';
    Data.NumericValue := FCount;
    DoDataProcessed(Data);
  end;
end;

procedure TDataAggregatorProcessor.ProcessData(const Data: TIoTData);
var
  CurrentTime: TDateTime;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Initialiser le temps de début si c'est la première donnée
    if FLastProcessTime = 0 then
      FLastProcessTime := Data.Timestamp;

    // Mettre à jour les statistiques
    Inc(FCount);
    FSum := FSum + Data.NumericValue;
    if Data.NumericValue < FMin then FMin := Data.NumericValue;
    if Data.NumericValue > FMax then FMax := Data.NumericValue;
    FValues.Add(Data.NumericValue);

    // Vérifier si la période d'agrégation est écoulée
    CurrentTime := Data.Timestamp;
    if CurrentTime - FLastProcessTime >= FPeriod then
    begin
      // Envoyer les statistiques agrégées
      SendAggregatedData;

      // Réinitialiser pour la prochaine période
      ResetAggregation;
      FLastProcessTime := CurrentTime;
    end;
  end;
end;
```

## Stockage des données pour analyse historique

Les systèmes IoT produisent souvent des données qui doivent être conservées pour analyse ultérieure. Voici comment implémenter un stockage simple :

```pascal
unit IoTDataStorage;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, System.DateUtils,
  IoTDataTypes;

type
  TIoTDataStorage = class
  private
    FStoragePath: string;
    FCurrentFile: TextFile;
    FCurrentFileName: string;
    FIsFileOpen: Boolean;

    procedure EnsureFileOpen(const Timestamp: TDateTime);
    procedure CloseCurrentFile;
  public
    constructor Create(const AStoragePath: string);
    destructor Destroy; override;

    procedure StoreData(const Data: TIoTData);
    function QueryData(const StartTime, EndTime: TDateTime;
                      const DeviceID, SensorID: string): TArray<TIoTData>;
  end;

implementation

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation des méthodes principales :

```pascal
constructor TIoTDataStorage.Create(const AStoragePath: string);
begin
  inherited Create;
  FStoragePath := AStoragePath;

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(FStoragePath) then
    ForceDirectories(FStoragePath);

  FIsFileOpen := False;
end;

destructor TIoTDataStorage.Destroy;
begin
  // Fermer le fichier ouvert
  if FIsFileOpen then
    CloseCurrentFile;

  inherited;
end;

procedure TIoTDataStorage.EnsureFileOpen(const Timestamp: TDateTime);
var
  NewFileName: string;
  NeedNewFile: Boolean;
begin
  // Générer un nom de fichier basé sur la date (un fichier par jour)
  NewFileName := FormatDateTime('yyyy-mm-dd', Timestamp) + '.csv';
  NewFileName := TPath.Combine(FStoragePath, NewFileName);

  NeedNewFile := False;

  // Vérifier si nous devons ouvrir un nouveau fichier
  if not FIsFileOpen then
    NeedNewFile := True
  else if NewFileName <> FCurrentFileName then
  begin
    // Fermer le fichier actuel et ouvrir un nouveau
    CloseCurrentFile;
    NeedNewFile := True;
  end;

  if NeedNewFile then
  begin
    FCurrentFileName := NewFileName;

    // Vérifier si le fichier existe déjà
    if FileExists(FCurrentFileName) then
    begin
      // Ouvrir pour ajout
      AssignFile(FCurrentFile, FCurrentFileName);
      Append(FCurrentFile);
    end
    else
    begin
      // Créer un nouveau fichier avec en-tête
      AssignFile(FCurrentFile, FCurrentFileName);
      Rewrite(FCurrentFile);

      // Écrire l'en-tête CSV
      WriteLn(FCurrentFile, 'Timestamp,DeviceID,SensorID,DataType,NumericValue,BooleanValue,TextValue,Latitude,Longitude,Altitude');
    end;

    FIsFileOpen := True;
  end;
end;

procedure TIoTDataStorage.CloseCurrentFile;
begin
  if FIsFileOpen then
  begin
    CloseFile(FCurrentFile);
    FIsFileOpen := False;
  end;
end;

procedure TIoTDataStorage.StoreData(const Data: TIoTData);
var
  DataTypeStr, NumericValue, BooleanValue, TextValue, Location: string;
begin
  // Ouvrir le fichier approprié
  EnsureFileOpen(Data.Timestamp);

  // Convertir le type de données en chaîne
  case Data.DataType of
    dtUnknown: DataTypeStr := 'unknown';
    dtNumeric: DataTypeStr := 'numeric';
    dtBoolean: DataTypeStr := 'boolean';
    dtText: DataTypeStr := 'text';
    dtLocation: DataTypeStr := 'location';
    dtImage: DataTypeStr := 'image';
    dtBinary: DataTypeStr := 'binary';
  end;

  // Préparer les valeurs selon le type
  NumericValue := '';
  BooleanValue := '';
  TextValue := '';
  Location := ',,,'; // Lat,Long,Alt vides par défaut

  case Data.DataType of
    dtNumeric: NumericValue := FloatToStr(Data.NumericValue);
    dtBoolean: BooleanValue := BoolToStr(Data.BooleanValue, True);
    dtText: TextValue := '"' + StringReplace(Data.TextValue, '"', '""', [rfReplaceAll]) + '"';
    dtLocation: Location := Format(',%f,%f,%f', [Data.Latitude, Data.Longitude, Data.Altitude]);
  end;

  // Écrire la ligne CSV
  WriteLn(FCurrentFile, Format('%s,%s,%s,%s,%s,%s,%s%s', [
    FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Data.Timestamp),
    Data.DeviceID,
    Data.SensorID,
    DataTypeStr,
    NumericValue,
    BooleanValue,
    TextValue,
    Location
  ]));

  // Forcer l'écriture physique (important pour les données en temps réel)
  Flush(FCurrentFile);
end;

function TIoTDataStorage.QueryData(const StartTime, EndTime: TDateTime;
                                const DeviceID, SensorID: string): TArray<TIoTData>;
var
  ResultList: TList<TIoTData>;
  StartDate, EndDate, CurrentDate: TDateTime;
  FileName: string;
  FileContents: TStringList;
  Line, Parts: TArray<string>;
  Data: TIoTData;
  TimeStr, DataTypeStr: string;
  I: Integer;
begin
  ResultList := TList<TIoTData>.Create;
  try
    // Trouver tous les fichiers dans la plage de dates
    StartDate := Trunc(StartTime);
    EndDate := Trunc(EndTime);

    CurrentDate := StartDate;
    while CurrentDate <= EndDate do
    begin
      FileName := FormatDateTime('yyyy-mm-dd', CurrentDate) + '.csv';
      FileName := TPath.Combine(FStoragePath, FileName);

      if FileExists(FileName) then
      begin
        // Charger le fichier et parcourir chaque ligne
        FileContents := TStringList.Create;
        try
          FileContents.LoadFromFile(FileName);

          // Sauter l'en-tête
          for I := 1 to FileContents.Count - 1 do
          begin
            Line := FileContents[I].Split([',']);

            // Vérifier si la ligne a assez de colonnes
            if Length(Line) >= 6 then
            begin
              // Vérifier le filtre DeviceID
              if (DeviceID <> '') and (Line[1] <> DeviceID) then
                Continue;

              // Vérifier le filtre SensorID
              if (SensorID <> '') and (Line[2] <> SensorID) then
                Continue;

              // Analyser l'horodatage
              TimeStr := Line[0];
              Data.Timestamp := StrToDateTime(TimeStr);

              // Vérifier la plage de temps
              if (Data.Timestamp < StartTime) or (Data.Timestamp > EndTime) then
                Continue;

              // OK, cette ligne nous intéresse
              Data.DeviceID := Line[1];
              Data.SensorID := Line[2];

              // Type de données
              DataTypeStr := Line[3];

              if DataTypeStr = 'numeric' then
              begin
                Data.DataType := dtNumeric;
                if Line[4] <> '' then
                  Data.NumericValue := StrToFloat(Line[4]);
              end
              else if DataTypeStr = 'boolean' then
              begin
                Data.DataType := dtBoolean;
                if Line[5] <> '' then
                  Data.BooleanValue := StrToBool(Line[5]);
              end
              else if DataTypeStr = 'text' then
              begin
                Data.DataType := dtText;
                Data.TextValue := Line[6];
              end
              else if DataTypeStr = 'location' then
              begin
                Data.DataType := dtLocation;
                if Line[7] <> '' then Data.Latitude := StrToFloat(Line[7]);
                if Line[8] <> '' then Data.Longitude := StrToFloat(Line[8]);
                if Line[9] <> '' then Data.Altitude := StrToFloat(Line[9]);
              end
              else
                Data.DataType := dtUnknown;

              // Ajouter au résultat
              ResultList.Add(Data);
            end;
          end;
        finally
          FileContents.Free;
        end;
      end;

      // Passer au jour suivant
      CurrentDate := IncDay(CurrentDate);
    end;

    // Convertir en tableau
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;
```

## Exemple pratique: surveillance de température et humidité

Assemblons maintenant tout ce que nous avons appris pour créer une application complète de surveillance de température et d'humidité :

```pascal
unit TempHumidMonitorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, System.DateUtils,

  // Nos unités
  IoTDataTypes, IoTDataProcessor, IoTDataPipeline, RealTimeChartFrame,
  IoTDataStorage, DeviceInterfaces, MQTTDevice;

type
  TfrmTempHumidMonitor = class(TForm)
    pnlTop: TPanel;
    pnlCharts: TPanel;
    pnlControls: TPanel;
    lblTemperatureStatus: TLabel;
    lblHumidityStatus: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    edtMQTTBroker: TEdit;
    lblBroker: TLabel;
    edtTopic: TEdit;
    lblTopic: TLabel;
    statusBar: TStatusBar;
    pgcCharts: TPageControl;
    tabTemperature: TTabSheet;
    tabHumidity: TTabSheet;
    tabHistory: TTabSheet;
    dtpStartDate: TDateTimePicker;
    dtpEndDate: TDateTimePicker;
    btnLoadHistory: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnLoadHistoryClick(Sender: TObject);

  private
    FTempChart: TRealTimeChartFrame;
    FHumidChart: TRealTimeChartFrame;
    FHistoryChart: TRealTimeChartFrame;

    FTempPipeline: TIoTDataPipeline;
    FHumidPipeline: TIoTDataPipeline;

    FMQTTDevice: IMQTTDevice;
    FDataStorage: TIoTDataStorage;

    FTempThresholdDetector: TThresholdDetectorProcessor;
    FHumidThresholdDetector: TThresholdDetectorProcessor;

    procedure HandleNewTemperatureData(Sender: TObject; const Data: TIoTData);
    procedure HandleNewHumidityData(Sender: TObject; const Data: TIoTData);
    procedure HandleTemperatureThreshold(Sender: TObject);
    procedure HandleHumidityThreshold(Sender: TObject);
    procedure HandleMQTTMessage(Sender: TObject; topic: string; payload: TBytes);
  public
    { Public declarations }
  end;

var
  frmTempHumidMonitor: TfrmTempHumidMonitor;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end;
```

Voici l'implémentation des méthodes principales :

```pascal
procedure TfrmTempHumidMonitor.FormCreate(Sender: TObject);
var
  AppPath: string;
begin
  // Initialiser le chemin de stockage
  AppPath := ExtractFilePath(Application.ExeName);
  FDataStorage := TIoTDataStorage.Create(TPath.Combine(AppPath, 'IoTData'));

  // Créer les pipelines de traitement
  FTempPipeline := TIoTDataPipeline.Create;
  FHumidPipeline := TIoTDataPipeline.Create;

  // Configurer les gestionnaires d'événements
  FTempPipeline.OnDataProcessed := HandleNewTemperatureData;
  FHumidPipeline.OnDataProcessed := HandleNewHumidityData;

  // Ajouter des processeurs pour la température

  // Filtre pour éliminer les valeurs aberrantes
  FTempPipeline.AddProcessor(TRangeLimitProcessor.Create('temperature', -40, 80));

  // Moyenne mobile pour lisser les données
  FTempPipeline.AddProcessor(TMovingAverageProcessor.Create('temperature', 5));

  // Détecteur de seuil pour les alertes
  FTempThresholdDetector := TThresholdDetectorProcessor.Create('temperature', 30, 2);
  FTempThresholdDetector.OnThresholdCrossed := HandleTemperatureThreshold;
  FTempPipeline.AddProcessor(FTempThresholdDetector);

  // Agrégateur pour les statistiques
  FTempPipeline.AddProcessor(TDataAggregatorProcessor.Create('temperature', 15));

  // Ajouter des processeurs pour l'humidité

  // Filtre pour éliminer les valeurs aberrantes
  FHumidPipeline.AddProcessor(TRangeLimitProcessor.Create('humidity', 0, 100));

  // Moyenne mobile pour lisser les données
  FHumidPipeline.AddProcessor(TMovingAverageProcessor.Create('humidity', 5));

  // Détecteur de seuil pour les alertes
  FHumidThresholdDetector := TThresholdDetectorProcessor.Create('humidity', 70, 5);
  FHumidThresholdDetector.OnThresholdCrossed := HandleHumidityThreshold;
  FHumidPipeline.AddProcessor(FHumidThresholdDetector);

  // Agrégateur pour les statistiques
  FHumidPipeline.AddProcessor(TDataAggregatorProcessor.Create('humidity', 15));

  // Créer les graphiques
  FTempChart := TRealTimeChartFrame.Create(tabTemperature);
  FTempChart.Parent := tabTemperature;
  FTempChart.Align := alClient;
  FTempChart.SetDataQueue(FTempPipeline.OutputQueue);
  FTempChart.SetSensorID('temperature');

  FHumidChart := TRealTimeChartFrame.Create(tabHumidity);
  FHumidChart.Parent := tabHumidity;
  FHumidChart.Align := alClient;
  FHumidChart.SetDataQueue(FHumidPipeline.OutputQueue);
  FHumidChart.SetSensorID('humidity');

  FHistoryChart := TRealTimeChartFrame.Create(tabHistory);
  FHistoryChart.Parent := tabHistory;
  FHistoryChart.Align := alClient;
  FHistoryChart.AutoScroll := False;

  // Démarrer les pipelines
  FTempPipeline.Start;
  FHumidPipeline.Start;

  // Initialiser l'interface utilisateur
  dtpStartDate.Date := Date - 7; // Dernière semaine
  dtpEndDate.Date := Date;

  // Initialiser les valeurs par défaut pour MQTT
  edtMQTTBroker.Text := 'localhost';
  edtTopic.Text := 'sensors/#';

  // Initialiser les statuts
  lblTemperatureStatus.Caption := 'Température: En attente de données...';
  lblHumidityStatus.Caption := 'Humidité: En attente de données...';
end;

procedure TfrmTempHumidMonitor.FormDestroy(Sender: TObject);
begin
  // Déconnecter le dispositif MQTT si connecté
  if Assigned(FMQTTDevice) then
    FMQTTDevice.Disconnect;

  // Arrêter et libérer les pipelines
  if Assigned(FTempPipeline) then
  begin
    FTempPipeline.Stop;
    FTempPipeline.Free;
  end;

  if Assigned(FHumidPipeline) then
  begin
    FHumidPipeline.Stop;
    FHumidPipeline.Free;
  end;

  // Libérer le stockage
  if Assigned(FDataStorage) then
    FDataStorage.Free;
end;

procedure TfrmTempHumidMonitor.btnConnectClick(Sender: TObject);
var
  BrokerHost: string;
  Topic: string;
begin
  // Vérifier les entrées
  BrokerHost := Trim(edtMQTTBroker.Text);
  Topic := Trim(edtTopic.Text);

  if (BrokerHost = '') or (Topic = '') then
  begin
    ShowMessage('Veuillez saisir l''adresse du broker MQTT et le topic');
    Exit;
  end;

  try
    // Créer et connecter le dispositif MQTT
    FMQTTDevice := TMQTTDevice.Create('temphumsensor', 'Capteur Temp/Humid', Topic, BrokerHost, 1883);

    // Configurer le gestionnaire de message
    TMQTTDevice(FMQTTDevice).SetCustomMessageHandler(HandleMQTTMessage);

    if FMQTTDevice.Connect then
    begin
      // Mettre à jour l'interface utilisateur
      btnConnect.Enabled := False;
      btnDisconnect.Enabled := True;
      edtMQTTBroker.Enabled := False;
      edtTopic.Enabled := False;

      statusBar.Panels[0].Text := 'Connecté au broker MQTT';
    end
    else
    begin
      ShowMessage('Erreur de connexion au broker MQTT');
      FMQTTDevice := nil;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);
      FMQTTDevice := nil;
    end;
  end;
end;

procedure TfrmTempHumidMonitor.btnDisconnectClick(Sender: TObject);
begin
  if Assigned(FMQTTDevice) then
  begin
    FMQTTDevice.Disconnect;
    FMQTTDevice := nil;

    // Mettre à jour l'interface utilisateur
    btnConnect.Enabled := True;
    btnDisconnect.Enabled := False;
    edtMQTTBroker.Enabled := True;
    edtTopic.Enabled := True;

    statusBar.Panels[0].Text := 'Déconnecté';
  end;
end;

procedure TfrmTempHumidMonitor.HandleMQTTMessage(Sender: TObject; topic: string; payload: TBytes);
var
  PayloadStr: string;
  TempData, HumidData: TIoTData;
  Temperature, Humidity: Double;
  DeviceID: string;
  Parts: TArray<string>;
  JSONObj: TJSONObject;
begin
  // Convertir le payload en chaîne
  PayloadStr := TEncoding.UTF8.GetString(payload);

  try
    // Analyser le chemin du topic pour obtenir l'ID du dispositif
    // Format attendu: sensors/{deviceID}
    Parts := topic.Split(['/']);
    if Length(Parts) >= 2 then
      DeviceID := Parts[1]
    else
      DeviceID := 'unknown';

    // Analyser le JSON
    JSONObj := TJSONObject.ParseJSONValue(PayloadStr) as TJSONObject;
    if Assigned(JSONObj) then
    try
      // Extraire la température et l'humidité
      if JSONObj.TryGetValue<Double>('temperature', Temperature) then
      begin
        // Créer une structure de données IoT pour la température
        FillChar(TempData, SizeOf(TempData), 0);
        TempData.DeviceID := DeviceID;
        TempData.SensorID := 'temperature';
        TempData.Timestamp := Now;
        TempData.DataType := dtNumeric;
        TempData.NumericValue := Temperature;

        // Ajouter à la pipeline de température
        FTempPipeline.EnqueueData(TempData);

        // Stocker pour référence future
        FDataStorage.StoreData(TempData);
      end;

      if JSONObj.TryGetValue<Double>('humidity', Humidity) then
      begin
        // Créer une structure de données IoT pour l'humidité
        FillChar(HumidData, SizeOf(HumidData), 0);
        HumidData.DeviceID := DeviceID;
        HumidData.SensorID := 'humidity';
        HumidData.Timestamp := Now;
        HumidData.DataType := dtNumeric;
        HumidData.NumericValue := Humidity;

        // Ajouter à la pipeline d'humidité
        FHumidPipeline.EnqueueData(HumidData);

        // Stocker pour référence future
        FDataStorage.StoreData(HumidData);
      end;
    finally
      JSONObj.Free;
    end;
  except
    on E: Exception do
    begin
      // Journaliser l'erreur
      OutputDebugString(PChar('Erreur de traitement MQTT: ' + E.Message));
    end;
  end;
end;

procedure TfrmTempHumidMonitor.HandleNewTemperatureData(Sender: TObject; const Data: TIoTData);
begin
  // Mettre à jour l'interface avec la dernière valeur de température
  TThread.Queue(nil, procedure
  begin
    lblTemperatureStatus.Caption := Format('Température: %.1f°C', [Data.NumericValue]);

    // Mettre en évidence si la température est élevée
    if Data.NumericValue > 30 then
      lblTemperatureStatus.Font.Color := clRed
    else if Data.NumericValue < 10 then
      lblTemperatureStatus.Font.Color := clBlue
    else
      lblTemperatureStatus.Font.Color := clWindowText;

    statusBar.Panels[1].Text := Format('Dernière mise à jour: %s',
                                      [FormatDateTime('hh:nn:ss', Now)]);
  end);
end;

procedure TfrmTempHumidMonitor.HandleNewHumidityData(Sender: TObject; const Data: TIoTData);
begin
  // Mettre à jour l'interface avec la dernière valeur d'humidité
  TThread.Queue(nil, procedure
  begin
    lblHumidityStatus.Caption := Format('Humidité: %.1f%%', [Data.NumericValue]);

    // Mettre en évidence si l'humidité est élevée
    if Data.NumericValue > 70 then
      lblHumidityStatus.Font.Color := clBlue
    else if Data.NumericValue < 30 then
      lblHumidityStatus.Font.Color := clRed
    else
      lblHumidityStatus.Font.Color := clWindowText;
  end);
end;

procedure TfrmTempHumidMonitor.HandleTemperatureThreshold(Sender: TObject);
begin
  // Appelé lorsque la température franchit le seuil configuré
  TThread.Queue(nil, procedure
  begin
    if FTempThresholdDetector.FIsAboveThreshold then
    begin
      // Alerte pour température élevée
      ShowMessage('ALERTE: Température au dessus du seuil critique!');
      // On pourrait aussi déclencher une alarme sonore, envoyer un email, etc.
    end
    else
    begin
      // Retour à la normale
      ShowMessage('Information: Température revenue à un niveau normal');
    end;
  end);
end;

procedure TfrmTempHumidMonitor.HandleHumidityThreshold(Sender: TObject);
begin
  // Appelé lorsque l'humidité franchit le seuil configuré
  TThread.Queue(nil, procedure
  begin
    if FHumidThresholdDetector.FIsAboveThreshold then
    begin
      // Alerte pour humidité élevée
      ShowMessage('ALERTE: Humidité au dessus du seuil critique!');
    end
    else
    begin
      // Retour à la normale
      ShowMessage('Information: Humidité revenue à un niveau normal');
    end;
  end);
end;

procedure TfrmTempHumidMonitor.btnLoadHistoryClick(Sender: TObject);
var
  StartTime, EndTime: TDateTime;
  TempData, HumidData: TArray<TIoTData>;
  TempSeries, HumidSeries: TLineSeries;
  I: Integer;
begin
  // Récupérer la plage de dates
  StartTime := Trunc(dtpStartDate.Date);
  EndTime := Trunc(dtpEndDate.Date) + 0.9999; // Fin de la journée

  // Vérifier que la plage est valide
  if StartTime > EndTime then
  begin
    ShowMessage('La date de début doit être antérieure à la date de fin');
    Exit;
  end;

  // Effacer le graphique existant
  FHistoryChart.Clear;

  try
    // Créer de nouvelles séries
    TempSeries := TLineSeries.Create(FHistoryChart.Chart);
    TempSeries.Title := 'Température';
    TempSeries.LinePen.Width := 2;
    TempSeries.LinePen.Color := clRed;
    FHistoryChart.Chart.AddSeries(TempSeries);

    HumidSeries := TLineSeries.Create(FHistoryChart.Chart);
    HumidSeries.Title := 'Humidité';
    HumidSeries.LinePen.Width := 2;
    HumidSeries.LinePen.Color := clBlue;
    FHistoryChart.Chart.AddSeries(HumidSeries);

    // Interroger le stockage pour les données historiques
    TempData := FDataStorage.QueryData(StartTime, EndTime, '', 'temperature');
    HumidData := FDataStorage.QueryData(StartTime, EndTime, '', 'humidity');

    // Ajouter les données de température au graphique
    for I := 0 to Length(TempData) - 1 do
    begin
      TempSeries.AddXY(TempData[I].Timestamp, TempData[I].NumericValue,
                       FormatDateTime('dd/mm hh:nn', TempData[I].Timestamp));
    end;

    // Ajouter les données d'humidité au graphique
    for I := 0 to Length(HumidData) - 1 do
    begin
      HumidSeries.AddXY(HumidData[I].Timestamp, HumidData[I].NumericValue,
                        FormatDateTime('dd/mm hh:nn', HumidData[I].Timestamp));
    end;

    // Configurer l'axe X pour afficher les dates correctement
    FHistoryChart.Chart.BottomAxis.DateTimeFormat := 'dd/mm/yyyy hh:nn';
    FHistoryChart.Chart.BottomAxis.ExactDateTime := True;

    // Passer à l'onglet historique
    pgcCharts.ActivePage := tabHistory;

    // Afficher un message de statut
    statusBar.Panels[2].Text := Format('Historique chargé: %d points de température, %d points d''humidité',
                                      [Length(TempData), Length(HumidData)]);
  except
    on E: Exception do
      ShowMessage('Erreur lors du chargement des données historiques: ' + E.Message);
  end;
end;
```

## Techniques avancées de traitement des données IoT

Maintenant que nous avons une application fonctionnelle, explorons quelques techniques avancées pour améliorer notre traitement des données IoT.

### Détection d'anomalies

La détection d'anomalies permet d'identifier automatiquement les valeurs inhabituelles dans un flux de données. Voici un exemple simple basé sur la méthode des écarts-types :

```pascal
// Processeur qui détecte les anomalies statistiques
TAnomalyDetectorProcessor = class(TIoTDataProcessorBase)
private
  FValues: TList<Double>;
  FWindowSize: Integer;
  FSensorID: string;
  FStdDevFactor: Double;
  FOnAnomalyDetected: TNotifyEvent;
  FLastAnomaly: TDateTime;
  FMinTimeBetweenAlerts: TDateTime;
public
  constructor Create(const ASensorID: string;
                   AWindowSize: Integer = 100;
                   AStdDevFactor: Double = 3.0;
                   AMinTimeInMinutes: Double = 5);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;

  property OnAnomalyDetected: TNotifyEvent read FOnAnomalyDetected write FOnAnomalyDetected;
end;

constructor TAnomalyDetectorProcessor.Create(const ASensorID: string;
                                         AWindowSize: Integer = 100;
                                         AStdDevFactor: Double = 3.0;
                                         AMinTimeInMinutes: Double = 5);
begin
  inherited Create;
  FSensorID := ASensorID;
  FWindowSize := AWindowSize;
  FStdDevFactor := AStdDevFactor;
  FValues := TList<Double>.Create;
  FLastAnomaly := 0;
  FMinTimeBetweenAlerts := AMinTimeInMinutes / (24 * 60); // Convertir minutes en jours
end;

destructor TAnomalyDetectorProcessor.Destroy;
begin
  FValues.Free;
  inherited;
end;

procedure TAnomalyDetectorProcessor.ProcessData(const Data: TIoTData);
var
  Mean, StdDev, Sum, Sum2, ThresholdHigh, ThresholdLow: Double;
  I: Integer;
  IsAnomaly: Boolean;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter la valeur à notre fenêtre
    FValues.Add(Data.NumericValue);

    // Limiter la taille de la fenêtre
    while FValues.Count > FWindowSize do
      FValues.Delete(0);

    // Il faut un minimum de points pour calculer des statistiques fiables
    if FValues.Count >= 10 then
    begin
      // Calculer la moyenne
      Sum := 0;
      for I := 0 to FValues.Count - 1 do
        Sum := Sum + FValues[I];
      Mean := Sum / FValues.Count;

      // Calculer l'écart-type
      Sum2 := 0;
      for I := 0 to FValues.Count - 1 do
        Sum2 := Sum2 + Sqr(FValues[I] - Mean);

      StdDev := Sqrt(Sum2 / (FValues.Count - 1));

      // Définir les seuils d'anomalie (±N écarts-types de la moyenne)
      ThresholdHigh := Mean + FStdDevFactor * StdDev;
      ThresholdLow := Mean - FStdDevFactor * StdDev;

      // Vérifier si la valeur actuelle est une anomalie
      IsAnomaly := (Data.NumericValue > ThresholdHigh) or (Data.NumericValue < ThresholdLow);

      // Déclencher l'événement si c'est une anomalie et si assez de temps s'est écoulé
      // depuis la dernière alerte
      if IsAnomaly and
         ((FLastAnomaly = 0) or ((Now - FLastAnomaly) >= FMinTimeBetweenAlerts)) and
         Assigned(FOnAnomalyDetected) then
      begin
        FLastAnomaly := Now;

        TThread.Queue(nil, procedure
        begin
          FOnAnomalyDetected(Self);
        end);
      end;
    end;
  end;
end;
```

Pour utiliser ce détecteur d'anomalies, vous pouvez l'ajouter à votre pipeline de traitement :

```pascal
// Ajouter un détecteur d'anomalies pour la température
var AnomalyDetector: TAnomalyDetectorProcessor;
AnomalyDetector := TAnomalyDetectorProcessor.Create('temperature', 100, 3.0, 5);
AnomalyDetector.OnAnomalyDetected := HandleTemperatureAnomaly;
FTempPipeline.AddProcessor(AnomalyDetector);

// Et le gestionnaire d'événement
procedure TfrmTempHumidMonitor.HandleTemperatureAnomaly(Sender: TObject);
begin
  // Appelé lorsqu'une anomalie de température est détectée
  TThread.Queue(nil, procedure
  begin
    ShowMessage('ALERTE: Anomalie détectée dans les données de température!');
    // Actions supplémentaires: journal, notification, etc.
  end);
end;
```

### Fusion de données multi-capteurs

La fusion de données permet de combiner les informations de plusieurs capteurs pour obtenir une mesure plus précise ou une information de plus haut niveau. Voici un exemple simple de fusion de température et d'humidité pour calculer l'indice de chaleur :

```pascal
// Processeur qui fusionne température et humidité pour calculer l'indice de chaleur
THeatIndexProcessor = class(TIoTDataProcessorBase)
private
  FLastTemperature: Double;
  FLastHumidity: Double;
  FHasTemperature: Boolean;
  FHasHumidity: Boolean;
  FMaxAge: TDateTime; // Âge maximum des données en jours
  FLastTempTime: TDateTime;
  FLastHumidTime: TDateTime;
public
  constructor Create(AMaxAgeInMinutes: Double = 5);
  procedure ProcessData(const Data: TIoTData); override;
  function CalculateHeatIndex(Temperature, Humidity: Double): Double;
end;

constructor THeatIndexProcessor.Create(AMaxAgeInMinutes: Double = 5);
begin
  inherited Create;
  FMaxAge := AMaxAgeInMinutes / (24 * 60); // Convertir minutes en jours
  FHasTemperature := False;
  FHasHumidity := False;
end;

procedure THeatIndexProcessor.ProcessData(const Data: TIoTData);
var
  HeatIndex: Double;
  HeatIndexData: TIoTData;
  CurrentTime: TDateTime;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Stocker la valeur selon le type de capteur
  if Data.DataType = dtNumeric then
  begin
    CurrentTime := Now;

    if Data.SensorID = 'temperature' then
    begin
      FLastTemperature := Data.NumericValue;
      FLastTempTime := CurrentTime;
      FHasTemperature := True;
    end
    else if Data.SensorID = 'humidity' then
    begin
      FLastHumidity := Data.NumericValue;
      FLastHumidTime := CurrentTime;
      FHasHumidity := True;
    end;

    // Vérifier si nous avons les deux valeurs et si elles sont assez récentes
    if FHasTemperature and FHasHumidity and
       ((CurrentTime - FLastTempTime) <= FMaxAge) and
       ((CurrentTime - FLastHumidTime) <= FMaxAge) then
    begin
      // Calculer l'indice de chaleur
      HeatIndex := CalculateHeatIndex(FLastTemperature, FLastHumidity);

      // Créer une nouvelle donnée pour l'indice de chaleur
      FillChar(HeatIndexData, SizeOf(HeatIndexData), 0);
      HeatIndexData.DeviceID := Data.DeviceID;
      HeatIndexData.SensorID := 'heatindex';
      HeatIndexData.Timestamp := CurrentTime;
      HeatIndexData.DataType := dtNumeric;
      HeatIndexData.NumericValue := HeatIndex;

      // Envoyer la donnée calculée
      DoDataProcessed(HeatIndexData);
    end;
  end;
end;

function THeatIndexProcessor.CalculateHeatIndex(Temperature, Humidity: Double): Double;
begin
  // Formule simplifiée pour l'indice de chaleur
  // Pour une formule plus précise, consulter la NOAA (National Oceanic and Atmospheric Administration)
  if Temperature < 27 then
    Result := Temperature // En dessous de 27°C, l'indice = température
  else
  begin
    // Formule approximative pour l'indice de chaleur
    Result := -8.784695 + 1.61139411 * Temperature + 2.338549 * Humidity -
              0.14611605 * Temperature * Humidity - 0.012308094 * Sqr(Temperature) -
              0.016424828 * Sqr(Humidity) + 0.002211732 * Sqr(Temperature) * Humidity +
              0.00072546 * Temperature * Sqr(Humidity) - 0.000003582 * Sqr(Temperature) * Sqr(Humidity);
  end;
end;
```

Pour utiliser ce processeur de fusion de données, ajoutez-le aux deux pipelines :

```pascal
// Créer un processeur d'indice de chaleur partagé entre les pipelines
var HeatIndexProcessor: THeatIndexProcessor;
HeatIndexProcessor := THeatIndexProcessor.Create(5);

// L'ajouter aux deux pipelines pour qu'il reçoive les deux types de données
FTempPipeline.AddProcessor(HeatIndexProcessor);
FHumidPipeline.AddProcessor(HeatIndexProcessor);
```

### Prédiction de valeurs futures

Nous pouvons également utiliser les techniques d'analyse de séries temporelles pour prédire les valeurs futures. Voici un exemple simple utilisant une régression linéaire :

```pascal
// Processeur qui prédit les valeurs futures par régression linéaire
TPredictorProcessor = class(TIoTDataProcessorBase)
private
  FValues: TList<Double>;
  FTimes: TList<TDateTime>;
  FWindowSize: Integer;
  FSensorID: string;
  FPredictionHorizon: Double; // Horizon de prédiction en minutes
public
  constructor Create(const ASensorID: string; AWindowSize: Integer = 60; APredictionHorizonInMinutes: Double = 15);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;
  function PredictValue(HorizonInMinutes: Double): Double;
end;

constructor TPredictorProcessor.Create(const ASensorID: string; AWindowSize: Integer = 60; APredictionHorizonInMinutes: Double = 15);
begin
  inherited Create;
  FSensorID := ASensorID;
  FWindowSize := AWindowSize;
  FPredictionHorizon := APredictionHorizonInMinutes;
  FValues := TList<Double>.Create;
  FTimes := TList<TDateTime>.Create;
end;

destructor TPredictorProcessor.Destroy;
begin
  FValues.Free;
  FTimes.Free;
  inherited;
end;

procedure TPredictorProcessor.ProcessData(const Data: TIoTData);
var
  PredictedValue: Double;
  PredictionData: TIoTData;
  MinutesInDay: Double;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter la valeur et l'heure à nos listes
    FValues.Add(Data.NumericValue);
    FTimes.Add(Data.Timestamp);

    // Limiter la taille de la fenêtre
    while FValues.Count > FWindowSize do
    begin
      FValues.Delete(0);
      FTimes.Delete(0);
    end;

    // Il faut un minimum de points pour faire une prédiction fiable
    if FValues.Count >= 10 then
    begin
      // Calculer la prédiction
      PredictedValue := PredictValue(FPredictionHorizon);

      // Créer une nouvelle donnée pour la prédiction
      FillChar(PredictionData, SizeOf(PredictionData), 0);
      PredictionData.DeviceID := Data.DeviceID;
      PredictionData.SensorID := FSensorID + '_predicted';

      // Calculer l'horodatage futur
      MinutesInDay := FPredictionHorizon / (24 * 60);
      PredictionData.Timestamp := Data.Timestamp + MinutesInDay;

      PredictionData.DataType := dtNumeric;
      PredictionData.NumericValue := PredictedValue;

      // Envoyer la donnée prédite
      DoDataProcessed(PredictionData);
    end;
  end;
end;

function TPredictorProcessor.PredictValue(HorizonInMinutes: Double): Double;
var
  XValues, YValues: array of Double;
  SumX, SumY, SumXY, SumX2: Double;
  Slope, Intercept: Double;
  N, I: Integer;
  BaseTime, FutureTime, MinutesInDay: Double;
begin
  N := FValues.Count;
  SetLength(XValues, N);
  SetLength(YValues, N);

  // Convertir les horodatages en minutes depuis le premier point
  BaseTime := FTimes[0];

  for I := 0 to N - 1 do
  begin
    XValues[I] := (FTimes[I] - BaseTime) * 24 * 60; // Convertir jours en minutes
    YValues[I] := FValues[I];
  end;

  // Calculer les sommes pour la régression linéaire
  SumX := 0;
  SumY := 0;
  SumXY := 0;
  SumX2 := 0;

  for I := 0 to N - 1 do
  begin
    SumX := SumX + XValues[I];
    SumY := SumY + YValues[I];
    SumXY := SumXY + XValues[I] * YValues[I];
    SumX2 := SumX2 + XValues[I] * XValues[I];
  end;

  // Calculer la pente et l'ordonnée à l'origine
  Slope := (N * SumXY - SumX * SumY) / (N * SumX2 - SumX * SumX);
  Intercept := (SumY - Slope * SumX) / N;

  // Calculer la prédiction pour l'horizon spécifié
  // Le X pour la prédiction est le temps du dernier point + l'horizon
  FutureTime := (FTimes[N-1] - BaseTime) * 24 * 60 + HorizonInMinutes;

  // Calculer la valeur prédite
  Result := Intercept + Slope * FutureTime;
end;
```

## Conclusion et bonnes pratiques

Le traitement des données IoT en temps réel est un domaine vaste et passionnant. Voici quelques bonnes pratiques pour vos projets :

### Architecture et conception

1. **Séparation des préoccupations** : Séparez clairement l'acquisition, le traitement, l'analyse et la visualisation des données.
2. **Modularité** : Utilisez des interfaces et des composants modulaires pour faciliter les mises à jour et l'évolution du système.
3. **Évolutivité** : Concevez votre système pour qu'il puisse gérer un nombre croissant de dispositifs et de données.
4. **Fiabilité** : Implémentez des mécanismes de journalisation, de surveillance et de récupération d'erreurs.

### Performance

1. **Traitement asynchrone** : Utilisez des threads et des files d'attente pour éviter de bloquer l'interface utilisateur.
2. **Échantillonnage intelligent** : Adaptez la fréquence d'échantillonnage en fonction des besoins réels.
3. **Filtrage précoce** : Éliminez les données non pertinentes ou redondantes le plus tôt possible dans le pipeline.
4. **Optimisation de la mémoire** : Limitez la taille des tampons et utilisez des structures de données efficaces.

### Qualité des données

1. **Validation** : Vérifiez systématiquement les données entrantes pour détecter les valeurs manquantes, aberrantes ou incohérentes.
2. **Horodatage précis** : Utilisez des horodatages précis et cohérents, idéalement synchronisés avec une source de temps fiable.
3. **Métadonnées** : Conservez les métadonnées sur la provenance des données, leur qualité et leur contexte.
4. **Détection des anomalies** : Implémentez des algorithmes de détection d'anomalies pour identifier les problèmes potentiels.

### Sécurité et confidentialité

1. **Chiffrement** : Chiffrez les données sensibles, tant au repos qu'en transit.
2. **Authentification** : Vérifiez l'identité des dispositifs sources et des destinataires.
3. **Anonymisation** : Si nécessaire, anonymisez les données pour protéger la vie privée.
4. **Audit** : Conservez des traces d'audit pour les opérations sensibles.

## Traitement avancé avec l'intelligence artificielle

L'intelligence artificielle peut considérablement améliorer l'analyse des données IoT. Voici un exemple simplifié d'utilisation d'un réseau de neurones pour la prédiction de valeurs futures, utilisant une bibliothèque tierce :

```pascal
// Note: Ce code est conceptuel et nécessite l'intégration d'une bibliothèque
// de machine learning compatible avec Delphi, comme TensorFlow.pas

// Processeur utilisant un réseau de neurones pour la prédiction
TNeuralNetworkProcessor = class(TIoTDataProcessorBase)
private
  FModel: TTensorFlowModel;
  FInputBuffer: TList<Double>;
  FSequenceLength: Integer;
  FSensorID: string;
  FPredictionHorizon: Integer;
public
  constructor Create(const ASensorID: string;
                   const AModelPath: string;
                   ASequenceLength: Integer = 24;
                   APredictionHorizon: Integer = 6);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;
  function PredictNextValues(Count: Integer): TArray<Double>;
end;

constructor TNeuralNetworkProcessor.Create(const ASensorID: string;
                                        const AModelPath: string;
                                        ASequenceLength: Integer = 24;
                                        APredictionHorizon: Integer = 6);
begin
  inherited Create;
  FSensorID := ASensorID;
  FSequenceLength := ASequenceLength;
  FPredictionHorizon := APredictionHorizon;
  FInputBuffer := TList<Double>.Create;

  // Charger le modèle pré-entraîné
  FModel := TTensorFlowModel.Create;
  FModel.LoadFromFile(AModelPath);
end;

destructor TNeuralNetworkProcessor.Destroy;
begin
  FInputBuffer.Free;
  FModel.Free;
  inherited;
end;

procedure TNeuralNetworkProcessor.ProcessData(const Data: TIoTData);
var
  Predictions: TArray<Double>;
  PredictionData: TIoTData;
  I: Integer;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter la valeur à notre tampon d'entrée
    FInputBuffer.Add(Data.NumericValue);

    // Limiter la taille du tampon à la longueur de séquence
    while FInputBuffer.Count > FSequenceLength do
      FInputBuffer.Delete(0);

    // Faire une prédiction si nous avons assez de données
    if FInputBuffer.Count = FSequenceLength then
    begin
      Predictions := PredictNextValues(FPredictionHorizon);

      // Créer des données pour chaque point prédit
      for I := 0 to Length(Predictions) - 1 do
      begin
        FillChar(PredictionData, SizeOf(PredictionData), 0);
        PredictionData.DeviceID := Data.DeviceID;
        PredictionData.SensorID := FSensorID + '_nn_predicted';
        PredictionData.Timestamp := Data.Timestamp + (I + 1) / (24 * 60); // +1, +2, ... minutes
        PredictionData.DataType := dtNumeric;
        PredictionData.NumericValue := Predictions[I];

        // Envoyer la donnée prédite
        DoDataProcessed(PredictionData);
      end;
    end;
  end;
end;

function TNeuralNetworkProcessor.PredictNextValues(Count: Integer): TArray<Double>;
var
  Input, Output: TTensor;
  I: Integer;
begin
  SetLength(Result, Count);

  // Préparer le tenseur d'entrée (normalisation recommandée dans un cas réel)
  Input := TTensor.Create([1, FSequenceLength, 1]); // [batch_size, sequence_length, features]

  // Remplir le tenseur avec les valeurs du tampon
  for I := 0 to FInputBuffer.Count - 1 do
    Input.SetValue(0, I, 0, FInputBuffer[I]);

  // Exécuter l'inférence
  Output := FModel.Predict(Input);

  // Extraire les prédictions
  for I := 0 to Count - 1 do
    Result[I] := Output.GetValue(0, I);

  // Libérer les ressources
  Input.Free;
  Output.Free;
end;
```

## Intégration avec les services cloud

Pour les projets IoT à grande échelle, l'intégration avec des services cloud comme AWS IoT, Azure IoT Hub ou Google Cloud IoT Core peut offrir une extensibilité et des capacités supplémentaires. Voici un exemple simple d'envoi de données à Azure IoT Hub :

```pascal
unit AzureIoTProcessor;

interface

uses
  System.Classes, System.SysUtils, System.JSON,
  System.Net.HttpClient, System.Net.URLClient,
  IoTDataTypes, IoTDataProcessor;

type
  TAzureIoTProcessor = class(TIoTDataProcessorBase)
  private
    FConnectionString: string;
    FDeviceID: string;
    FHttpClient: THTTPClient;
    FLastSendTime: TDateTime;
    FMinTimeBetweenSends: TDateTime;
    FPendingData: TList<TIoTData>;
    FMaxBatchSize: Integer;

    function BuildMessagePayload(const DataList: TArray<TIoTData>): string;
    function SendToAzure(const Payload: string): Boolean;
  public
    constructor Create(const AConnectionString, ADeviceID: string;
                     AMinTimeBetweenSendsInSeconds: Double = 10;
                     AMaxBatchSize: Integer = 100);
    destructor Destroy; override;
    procedure ProcessData(const Data: TIoTData); override;
    procedure Flush; // Forcer l'envoi des données en attente
  end;

implementation

constructor TAzureIoTProcessor.Create(const AConnectionString, ADeviceID: string;
                                   AMinTimeBetweenSendsInSeconds: Double = 10;
                                   AMaxBatchSize: Integer = 100);
begin
  inherited Create;
  FConnectionString := AConnectionString;
  FDeviceID := ADeviceID;
  FMinTimeBetweenSends := AMinTimeBetweenSendsInSeconds / (24 * 60 * 60); // Convertir en jours
  FMaxBatchSize := AMaxBatchSize;
  FPendingData := TList<TIoTData>.Create;
  FHttpClient := THTTPClient.Create;

  // Configurer le client HTTP
  FHttpClient.ConnectionTimeout := 10000; // 10 secondes
  FHttpClient.ResponseTimeout := 30000; // 30 secondes

  FLastSendTime := 0;
end;

destructor TAzureIoTProcessor.Destroy;
begin
  FPendingData.Free;
  FHttpClient.Free;
  inherited;
end;

procedure TAzureIoTProcessor.ProcessData(const Data: TIoTData);
var
  CurrentTime: TDateTime;
begin
  // Passer la donnée au processeur suivant
  DoDataProcessed(Data);

  // Ajouter la donnée à la liste en attente
  FPendingData.Add(Data);

  // Vérifier si c'est le moment d'envoyer les données
  CurrentTime := Now;

  if (FPendingData.Count >= FMaxBatchSize) or
     ((CurrentTime - FLastSendTime) >= FMinTimeBetweenSends) and (FPendingData.Count > 0) then
  begin
    Flush;
  end;
end;

procedure TAzureIoTProcessor.Flush;
var
  Payload: string;
  Success: Boolean;
begin
  if FPendingData.Count = 0 then
    Exit;

  // Construire le payload JSON pour l'envoi
  Payload := BuildMessagePayload(FPendingData.ToArray);

  // Envoyer à Azure IoT Hub
  Success := SendToAzure(Payload);

  if Success then
  begin
    // Vider la liste si l'envoi a réussi
    FPendingData.Clear;
    FLastSendTime := Now;
  end;
  // Si échec, on réessaiera à la prochaine occasion
end;

function TAzureIoTProcessor.BuildMessagePayload(const DataList: TArray<TIoTData>): string;
var
  RootObj, DataObj: TJSONObject;
  DataArray: TJSONArray;
  I: Integer;
begin
  RootObj := TJSONObject.Create;
  try
    RootObj.AddPair('deviceId', FDeviceID);

    DataArray := TJSONArray.Create;
    RootObj.AddPair('data', DataArray);

    for I := 0 to Length(DataList) - 1 do
    begin
      DataObj := TJSONObject.Create;

      // Ajouter les données de base
      DataObj.AddPair('sensorId', DataList[I].SensorID);
      DataObj.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', DataList[I].Timestamp));

      // Ajouter la valeur selon le type
      case DataList[I].DataType of
        dtNumeric: DataObj.AddPair('value', TJSONNumber.Create(DataList[I].NumericValue));
        dtBoolean: DataObj.AddPair('value', TJSONBool.Create(DataList[I].BooleanValue));
        dtText: DataObj.AddPair('value', DataList[I].TextValue);
        // Autres types...
      end;

      DataArray.Add(DataObj);
    end;

    Result := RootObj.ToString;
  finally
    RootObj.Free;
  end;
end;

function TAzureIoTProcessor.SendToAzure(const Payload: string): Boolean;
var
  URL: string;
  Response: IHTTPResponse;
  SASToken: string;
begin
  Result := False;

  try
    // Dans un cas réel, il faudrait générer un jeton SAS à partir de la chaîne de connexion
    // Pour cet exemple, nous supposerons que la fonction GenerateSASToken existe
    SASToken := GenerateSASToken(FConnectionString);

    // Construire l'URL
    URL := Format('https://%s.azure-devices.net/devices/%s/messages/events?api-version=2018-06-30',
                 [ExtractHostName(FConnectionString), FDeviceID]);

    // Préparer les en-têtes
    FHttpClient.CustomHeaders['Authorization'] := SASToken;
    FHttpClient.CustomHeaders['Content-Type'] := 'application/json';

    // Envoyer la requête
    Response := FHttpClient.Post(URL, TStringStream.Create(Payload));

    // Vérifier le code de statut
    Result := (Response.StatusCode >= 200) and (Response.StatusCode < 300);

    if not Result then
    begin
      // Journaliser l'erreur
      OutputDebugString(PChar('Erreur envoi Azure IoT: ' + Response.StatusText));
    end;
  except
    on E: Exception do
    begin
      OutputDebugString(PChar('Exception envoi Azure IoT: ' + E.Message));
      Result := False;
    end;
  end;
end;
```

## Surveillance de la santé des capteurs

Un aspect important des systèmes IoT est de surveiller la santé des capteurs eux-mêmes. Voici un processeur qui détecte les problèmes potentiels avec les capteurs :

```pascal
// Processeur qui surveille la santé des capteurs
TSensorHealthProcessor = class(TIoTDataProcessorBase)
private
  FSensorStats: TDictionary<string, record
                             LastUpdate: TDateTime;
                             UpdateCount: Integer;
                             ValueChanges: Integer;
                             MinValue: Double;
                             MaxValue: Double;
                             LastValue: Double;
                           end>;
  FMaxSilenceTime: TDateTime;
  FMinValueChanges: Integer;
  FCheckInterval: TDateTime;
  FLastCheckTime: TDateTime;
  FOnSensorHealthAlert: TNotifyEvent;

  procedure CheckSensorsHealth;
public
  constructor Create(AMaxSilenceInMinutes: Double = 30;
                   AMinValueChanges: Integer = 5;
                   ACheckIntervalInMinutes: Double = 15);
  destructor Destroy; override;
  procedure ProcessData(const Data: TIoTData); override;

  property OnSensorHealthAlert: TNotifyEvent read FOnSensorHealthAlert write FOnSensorHealthAlert;
end;

constructor TSensorHealthProcessor.Create(AMaxSilenceInMinutes: Double = 30;
                                       AMinValueChanges: Integer = 5;
                                       ACheckIntervalInMinutes: Double = 15);
begin
  inherited Create;
  FSensorStats := TDictionary<string, record
                               LastUpdate: TDateTime;
                               UpdateCount: Integer;
                               ValueChanges: Integer;
                               MinValue: Double;
                               MaxValue: Double;
                               LastValue: Double;
                             end>.Create;

  FMaxSilenceTime := AMaxSilenceInMinutes / (24 * 60); // Convertir en jours
  FMinValueChanges := AMinValueChanges;
  FCheckInterval := ACheckIntervalInMinutes / (24 * 60); // Convertir en jours
  FLastCheckTime := Now;
end;

destructor TSensorHealthProcessor.Destroy;
begin
  FSensorStats.Free;
  inherited;
end;

procedure TSensorHealthProcessor.ProcessData(const Data: TIoTData);
var
  Key: string;
  Stats: record
           LastUpdate: TDateTime;
           UpdateCount: Integer;
           ValueChanges: Integer;
           MinValue: Double;
           MaxValue: Double;
           LastValue: Double;
         end;
  CurrentTime: TDateTime;
begin
  // Passer la donnée originale sans modification
  DoDataProcessed(Data);

  // Vérifier si c'est un type de données numériques
  if Data.DataType = dtNumeric then
  begin
    // Créer une clé unique pour ce capteur
    Key := Data.DeviceID + '.' + Data.SensorID;

    // Mise à jour des statistiques
    if FSensorStats.TryGetValue(Key, Stats) then
    begin
      // Mettre à jour les stats existantes
      Stats.LastUpdate := Data.Timestamp;
      Inc(Stats.UpdateCount);

      // Vérifier si la valeur a changé
      if Abs(Stats.LastValue - Data.NumericValue) > 0.001 then
        Inc(Stats.ValueChanges);

      // Mettre à jour les min/max
      if Data.NumericValue < Stats.MinValue then Stats.MinValue := Data.NumericValue;
      if Data.NumericValue > Stats.MaxValue then Stats.MaxValue := Data.NumericValue;

      Stats.LastValue := Data.NumericValue;
    end
    else
    begin
      // Créer de nouvelles stats
      Stats.LastUpdate := Data.Timestamp;
      Stats.UpdateCount := 1;
      Stats.ValueChanges := 0;
      Stats.MinValue := Data.NumericValue;
      Stats.MaxValue := Data.NumericValue;
      Stats.LastValue := Data.NumericValue;
    end;

    // Mettre à jour le dictionnaire
    FSensorStats.AddOrSetValue(Key, Stats);

    // Vérifier périodiquement la santé des capteurs
    CurrentTime := Now;
    if CurrentTime - FLastCheckTime >= FCheckInterval then
    begin
      CheckSensorsHealth;
      FLastCheckTime := CurrentTime;
    end;
  end;
end;

procedure TSensorHealthProcessor.CheckSensorsHealth;
var
  Key: string;
  Stats: record
           LastUpdate: TDateTime;
           UpdateCount: Integer;
           ValueChanges: Integer;
           MinValue: Double;
           MaxValue: Double;
           LastValue: Double;
         end;
  CurrentTime: TDateTime;
  HasAlert: Boolean;
  AlertMessages: TStringList;
begin
  CurrentTime := Now;
  HasAlert := False;
  AlertMessages := TStringList.Create;
  try
    for Key in FSensorStats.Keys do
    begin
      Stats := FSensorStats[Key];

      // Vérifier si le capteur est silencieux
      if CurrentTime - Stats.LastUpdate > FMaxSilenceTime then
      begin
        AlertMessages.Add(Format('Alerte: Capteur %s inactif depuis %d minutes',
                                [Key, Round((CurrentTime - Stats.LastUpdate) * 24 * 60)]));
        HasAlert := True;
      end;

      // Vérifier si le capteur semble bloqué (ne change pas de valeur)
      if (Stats.UpdateCount > 10) and (Stats.ValueChanges < FMinValueChanges) then
      begin
        AlertMessages.Add(Format('Alerte: Capteur %s potentiellement bloqué (valeur fixe: %f)',
                                [Key, Stats.LastValue]));
        HasAlert := True;
      end;

      // Vérifier si le capteur fournit des valeurs dans une plage raisonnable
      if Stats.MinValue = Stats.MaxValue then
      begin
        AlertMessages.Add(Format('Alerte: Capteur %s renvoie toujours la même valeur (%f)',
                                [Key, Stats.MinValue]));
        HasAlert := True;
      end;
    end;

    // Déclencher l'alerte si nécessaire
    if HasAlert and Assigned(FOnSensorHealthAlert) then
    begin
      // On pourrait passer les messages d'alerte via un paramètre supplémentaire
      // Pour cet exemple, nous les affichons simplement
      for var Msg in AlertMessages do
        OutputDebugString(PChar(Msg));

      TThread.Queue(nil, procedure
      begin
        FOnSensorHealthAlert(Self);
      end);
    end;
  finally
    AlertMessages.Free;
  end;
end;
```

## Visualisation avancée avec des tableaux de bord

Pour créer des tableaux de bord IoT professionnels, vous pouvez améliorer notre exemple de base avec des visualisations plus avancées :

```pascal
unit DashboardComponents;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart,
  Vcl.StdCtrls, IoTDataTypes;

type
  // Jauge circulaire pour afficher une valeur unique
  TGaugeFrame = class(TFrame)
    Panel: TPanel;
    Chart: TChart;
    Series: TPieSeries;
    lblTitle: TLabel;
    lblValue: TLabel;
    procedure FrameResize(Sender: TObject);
  private
    FMinValue: Double;
    FMaxValue: Double;
    FCurrentValue: Double;
    FTitle: string;
    FUnit: string;
    FThresholdLow: Double;
    FThresholdHigh: Double;

    procedure UpdateGauge;
    procedure SetTitle(const Value: string);
    procedure SetMinValue(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetCurrentValue(const Value: Double);
    procedure SetUnit(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;

    property Title: string read FTitle write SetTitle;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property CurrentValue: Double read FCurrentValue write SetCurrentValue;
    property ValueUnit: string read FUnit write SetUnit;
    property ThresholdLow: Double read FThresholdLow write FThresholdLow;
    property ThresholdHigh: Double read FThresholdHigh write FThresholdHigh;
  end;

  // Carte de chaleur 2D
  THeatMapFrame = class(TFrame)
    Chart: TChart;
    pnlControls: TPanel;
    cmbXAxis: TComboBox;
    cmbYAxis: TComboBox;
    lblXAxis: TLabel;
    lblYAxis: TLabel;
    btnRefresh: TButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure cmbXAxisChange(Sender: TObject);
    procedure cmbYAxisChange(Sender: TObject);
  private
    FSeries: TColorGridSeries;
    FDataQueue: TIoTDataQueue;
    FSensorMapping: TDictionary<string, TList<TIoTData>>;

    procedure RebuildHeatMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDataQueue(AQueue: TIoTDataQueue);
    procedure AddData(const Data: TIoTData);
    procedure ClearData;
  end;

  // Graphique de tendance avec annotations
  TTrendChartFrame = class(TFrame)
    Chart: TChart;
    pnlControls: TPanel;
    cmbSensor: TComboBox;
    lblSensor: TLabel;
    chkShowPredictions: TCheckBox;
    chkShowThresholds: TCheckBox;
    chkShowTrend: TCheckBox;
    btnExportData: TButton;
    procedure cmbSensorChange(Sender: TObject);
    procedure chkShowPredictionsClick(Sender: TObject);
    procedure chkShowThresholdsClick(Sender: TObject);
    procedure chkShowTrendClick(Sender: TObject);
    procedure btnExportDataClick(Sender: TObject);
  private
    FDataSeries: TFastLineSeries;
    FPredictionSeries: TFastLineSeries;
    FTrendSeries: TPointSeries;
    FThresholdHighLine: TLineSeries;
    FThresholdLowLine: TLineSeries;
    FDataQueue: TIoTDataQueue;
    FSelectedSensor: string;
    FMaxPoints: Integer;
    FThresholdHigh: Double;
    FThresholdLow: Double;

    procedure UpdateChart;
    procedure SetThresholdHigh(const Value: Double);
    procedure SetThresholdLow(const Value: Double);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetDataQueue(AQueue: TIoTDataQueue);
    procedure SetSensor(const ASensor: string);
    procedure AddAnnotation(const Text: string; const DateTime: TDateTime);
    procedure Clear;

    property MaxPoints: Integer read FMaxPoints write FMaxPoints;
    property ThresholdHigh: Double read FThresholdHigh write SetThresholdHigh;
    property ThresholdLow: Double read FThresholdLow write SetThresholdLow;
  end;

// ... implémentation des méthodes ...

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

## Projet complet : Centre de contrôle IoT

Pour conclure ce tutoriel, assemblons tout ce que nous avons appris dans un projet complet de centre de contrôle IoT. Ce projet intégrera :

1. La connexion à plusieurs dispositifs IoT via MQTT
2. Le traitement en temps réel des données avec diverses analyses
3. La visualisation avec des tableaux de bord avancés
4. Le stockage des données historiques
5. La détection des anomalies et des alertes
6. La prédiction des tendances futures

Vous pouvez utiliser ce projet comme base pour vos propres applications IoT. N'hésitez pas à le personnaliser et à l'étendre selon vos besoins spécifiques.

## Exercices pratiques

Pour vous aider à approfondir vos connaissances, voici quelques exercices pratiques :

1. **Implémentez un système de surveillance de la consommation d'énergie** qui collecte des données à partir de capteurs de puissance et calcule la consommation journalière, hebdomadaire et mensuelle.

2. **Créez un système de surveillance météorologique** qui collecte des données de température, d'humidité, de pression et de précipitations, puis génère des alertes météo basées sur des conditions prédéfinies.

3. **Développez un tableau de bord pour la domotique** qui affiche et contrôle l'état de divers dispositifs dans une maison intelligente (lumières, thermostats, serrures, etc.).

4. **Concevez un système de maintenance prédictive** qui analyse les données de vibration, de température et d'autres paramètres d'une machine pour prédire les défaillances potentielles avant qu'elles ne se produisent.

5. **Implémentez un système de suivi de la qualité de l'air** qui surveille les niveaux de CO2, de particules et d'autres polluants, puis génère des rapports de qualité de l'air en temps réel.

## Ressources complémentaires

Voici quelques ressources utiles pour approfondir vos connaissances sur le traitement des données IoT :

- [MQTT.org](https://mqtt.org/) - Informations sur le protocole MQTT
- [Azure IoT Hub Documentation](https://docs.microsoft.com/fr-fr/azure/iot-hub/) - Documentation sur Azure IoT Hub
- [AWS IoT Core](https://aws.amazon.com/fr/iot-core/) - Service IoT d'Amazon Web Services
- [TensorFlow](https://www.tensorflow.org/) - Bibliothèque d'apprentissage automatique
- [Grafana](https://grafana.com/) - Plateforme de visualisation et de surveillance
- [InfluxDB](https://www.influxdata.com/) - Base de données de séries temporelles pour les données IoT

Dans la prochaine section, nous explorerons la création de tableaux de bord IoT complets pour visualiser et interagir avec vos dispositifs et données.

⏭️ [Tableaux de bord pour solutions IoT](/21-delphi-et-liot/08-tableaux-de-bord-pour-solutions-iot.md)
