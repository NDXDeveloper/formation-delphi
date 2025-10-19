🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.7 Traitement des données IoT en temps réel

## Introduction

Dans les applications IoT, les données affluent en continu depuis de multiples sources : capteurs de température, détecteurs de mouvement, compteurs, GPS, etc. Ces données brutes doivent être traitées, filtrées, analysées et transformées en informations exploitables, le tout en temps réel.

Cette section vous apprendra à :
- Gérer des flux de données continues
- Filtrer et nettoyer les données en temps réel
- Calculer des statistiques et agrégations
- Détecter des anomalies et tendances
- Déclencher des actions automatiques
- Optimiser les performances avec le multithreading

## Défis du traitement temps réel

### Volume et vélocité

Les systèmes IoT génèrent énormément de données :
- Un capteur envoyant des données toutes les secondes produit 86 400 mesures par jour
- 100 capteurs = 8,6 millions de mesures par jour
- Chaque mesure doit être traitée rapidement sans bloquer l'interface

### Qualité des données

Les données IoT présentent souvent des problèmes :
- **Valeurs aberrantes** : lectures erronées du capteur
- **Données manquantes** : déconnexions temporaires
- **Bruit** : fluctuations aléatoires
- **Dérives** : décalibrage progressif du capteur

### Performance

Le traitement doit être :
- **Rapide** : réponse en quelques millisecondes
- **Efficace** : faible consommation CPU et mémoire
- **Non bloquant** : l'interface reste réactive
- **Scalable** : capable de gérer plus de données

## Architecture de traitement

### Pipeline de traitement

Le traitement des données suit généralement un pipeline :

```
[Données brutes] → [Validation] → [Filtrage] → [Transformation] → [Agrégation] → [Stockage/Action]
```

Chaque étape a un rôle spécifique :

1. **Validation** : vérifier que les données sont correctes
2. **Filtrage** : éliminer le bruit et les valeurs aberrantes
3. **Transformation** : convertir, normaliser, calculer
4. **Agrégation** : moyennes, min/max, tendances
5. **Stockage/Action** : sauvegarder ou déclencher une action

### Implémentation du pipeline

```pascal
unit DataPipeline;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TSensorReading = record
    Timestamp: TDateTime;
    DeviceID: string;
    SensorType: string;
    Value: Double;
    Unit_: string;
  end;

  TProcessedData = record
    Reading: TSensorReading;
    IsValid: Boolean;
    FilteredValue: Double;
    AnomalyScore: Double;
  end;

  TDataValidator = class
  public
    class function Validate(const Reading: TSensorReading): Boolean;
  end;

  TDataFilter = class
  public
    class function ApplyMedianFilter(const Values: TArray<Double>): Double;
    class function ApplyMovingAverage(const Values: TArray<Double>; WindowSize: Integer): Double;
  end;

  TDataProcessor = class
  private
    FReadingBuffer: TList<TSensorReading>;
    FBufferSize: Integer;
  public
    constructor Create(BufferSize: Integer = 10);
    destructor Destroy; override;

    function Process(const Reading: TSensorReading): TProcessedData;
  end;

implementation

uses
  System.Math;

{ TDataValidator }

class function TDataValidator.Validate(const Reading: TSensorReading): Boolean;
begin
  Result := True;

  // Vérifier que le timestamp est valide
  if Reading.Timestamp = 0 then
  begin
    Result := False;
    Exit;
  end;

  // Vérifier que la valeur n'est pas NaN ou Infinite
  if IsNan(Reading.Value) or IsInfinite(Reading.Value) then
  begin
    Result := False;
    Exit;
  end;

  // Vérifier les limites selon le type de capteur
  if Reading.SensorType = 'temperature' then
  begin
    // Température doit être entre -50°C et 100°C (limites physiques raisonnables)
    if (Reading.Value < -50) or (Reading.Value > 100) then
      Result := False;
  end
  else if Reading.SensorType = 'humidity' then
  begin
    // Humidité doit être entre 0% et 100%
    if (Reading.Value < 0) or (Reading.Value > 100) then
      Result := False;
  end
  else if Reading.SensorType = 'pressure' then
  begin
    // Pression atmosphérique entre 300 et 1100 hPa
    if (Reading.Value < 300) or (Reading.Value > 1100) then
      Result := False;
  end;
end;

{ TDataFilter }

class function TDataFilter.ApplyMedianFilter(const Values: TArray<Double>): Double;
var
  SortedValues: TArray<Double>;
  Count: Integer;
begin
  if Length(Values) = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Copier et trier le tableau
  SortedValues := Copy(Values);
  TArray.Sort<Double>(SortedValues);

  Count := Length(SortedValues);

  // Retourner la valeur médiane
  if Count mod 2 = 0 then
    Result := (SortedValues[Count div 2 - 1] + SortedValues[Count div 2]) / 2
  else
    Result := SortedValues[Count div 2];
end;

class function TDataFilter.ApplyMovingAverage(const Values: TArray<Double>; WindowSize: Integer): Double;
var
  I, Count, StartIdx: Integer;
  Sum: Double;
begin
  Count := Length(Values);
  if Count = 0 then
  begin
    Result := 0;
    Exit;
  end;

  // Limiter la fenêtre au nombre de valeurs disponibles
  if WindowSize > Count then
    WindowSize := Count;

  StartIdx := Count - WindowSize;
  Sum := 0;

  // Calculer la moyenne des WindowSize dernières valeurs
  for I := StartIdx to Count - 1 do
    Sum := Sum + Values[I];

  Result := Sum / WindowSize;
end;

{ TDataProcessor }

constructor TDataProcessor.Create(BufferSize: Integer);
begin
  inherited Create;
  FBufferSize := BufferSize;
  FReadingBuffer := TList<TSensorReading>.Create;
end;

destructor TDataProcessor.Destroy;
begin
  FReadingBuffer.Free;
  inherited;
end;

function TDataProcessor.Process(const Reading: TSensorReading): TProcessedData;
var
  Values: TArray<Double>;
  I: Integer;
begin
  Result.Reading := Reading;
  Result.IsValid := TDataValidator.Validate(Reading);
  Result.AnomalyScore := 0;

  if not Result.IsValid then
  begin
    Result.FilteredValue := Reading.Value;
    Exit;
  end;

  // Ajouter au buffer
  FReadingBuffer.Add(Reading);

  // Limiter la taille du buffer
  while FReadingBuffer.Count > FBufferSize do
    FReadingBuffer.Delete(0);

  // Extraire les valeurs pour le filtrage
  SetLength(Values, FReadingBuffer.Count);
  for I := 0 to FReadingBuffer.Count - 1 do
    Values[I] := FReadingBuffer[I].Value;

  // Appliquer le filtre (moyenne mobile)
  Result.FilteredValue := TDataFilter.ApplyMovingAverage(Values, 5);

  // Calculer le score d'anomalie (écart à la moyenne)
  if FReadingBuffer.Count >= 5 then
  begin
    Result.AnomalyScore := Abs(Reading.Value - Result.FilteredValue);
  end;
end;

end.
```

## Filtrage des données

### Moyenne mobile (Moving Average)

La moyenne mobile lisse les fluctuations en calculant la moyenne des N dernières valeurs :

```pascal
unit MovingAverageFilter;

interface

uses
  System.Generics.Collections;

type
  TMovingAverageFilter = class
  private
    FValues: TQueue<Double>;
    FWindowSize: Integer;
    FSum: Double;
  public
    constructor Create(WindowSize: Integer);
    destructor Destroy; override;

    function AddValue(Value: Double): Double;
    function GetAverage: Double;
    procedure Reset;
  end;

implementation

constructor TMovingAverageFilter.Create(WindowSize: Integer);
begin
  inherited Create;
  FWindowSize := WindowSize;
  FValues := TQueue<Double>.Create;
  FSum := 0;
end;

destructor TMovingAverageFilter.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TMovingAverageFilter.AddValue(Value: Double): Double;
var
  OldValue: Double;
begin
  // Ajouter la nouvelle valeur
  FValues.Enqueue(Value);
  FSum := FSum + Value;

  // Si le buffer est plein, retirer la valeur la plus ancienne
  if FValues.Count > FWindowSize then
  begin
    OldValue := FValues.Dequeue;
    FSum := FSum - OldValue;
  end;

  Result := GetAverage;
end;

function TMovingAverageFilter.GetAverage: Double;
begin
  if FValues.Count > 0 then
    Result := FSum / FValues.Count
  else
    Result := 0;
end;

procedure TMovingAverageFilter.Reset;
begin
  FValues.Clear;
  FSum := 0;
end;

end.
```

### Filtre de Kalman simplifié

Pour des données très bruitées, un filtre de Kalman peut améliorer la précision :

```pascal
unit SimpleKalmanFilter;

interface

type
  TKalmanFilter = class
  private
    FEstimate: Double;
    FEstimateError: Double;
    FMeasurementNoise: Double;
    FProcessNoise: Double;
    FInitialized: Boolean;
  public
    constructor Create(MeasurementNoise, ProcessNoise: Double);

    function Update(Measurement: Double): Double;
    procedure Reset;

    property Estimate: Double read FEstimate;
  end;

implementation

constructor TKalmanFilter.Create(MeasurementNoise, ProcessNoise: Double);
begin
  inherited Create;
  FMeasurementNoise := MeasurementNoise;
  FProcessNoise := ProcessNoise;
  FEstimateError := 1;
  FInitialized := False;
end;

function TKalmanFilter.Update(Measurement: Double): Double;
var
  KalmanGain: Double;
begin
  // Initialiser avec la première mesure
  if not FInitialized then
  begin
    FEstimate := Measurement;
    FInitialized := True;
    Result := FEstimate;
    Exit;
  end;

  // Prédiction
  FEstimateError := FEstimateError + FProcessNoise;

  // Calcul du gain de Kalman
  KalmanGain := FEstimateError / (FEstimateError + FMeasurementNoise);

  // Mise à jour
  FEstimate := FEstimate + KalmanGain * (Measurement - FEstimate);
  FEstimateError := (1 - KalmanGain) * FEstimateError;

  Result := FEstimate;
end;

procedure TKalmanFilter.Reset;
begin
  FInitialized := False;
  FEstimateError := 1;
end;

end.
```

### Utilisation des filtres

```pascal
var
  MAFilter: TMovingAverageFilter;
  KalmanFilter: TKalmanFilter;
  RawValue, SmoothedMA, SmoothedKalman: Double;
begin
  MAFilter := TMovingAverageFilter.Create(10);
  KalmanFilter := TKalmanFilter.Create(4.0, 0.1);
  try
    // Traiter chaque nouvelle valeur
    RawValue := ReadSensorValue;

    SmoothedMA := MAFilter.AddValue(RawValue);
    SmoothedKalman := KalmanFilter.Update(RawValue);

    // Afficher
    MemoLog.Lines.Add(Format('Brute: %.2f | MA: %.2f | Kalman: %.2f',
      [RawValue, SmoothedMA, SmoothedKalman]));
  finally
    MAFilter.Free;
    KalmanFilter.Free;
  end;
end;
```

## Agrégation de données

### Calcul de statistiques en temps réel

```pascal
unit StreamingStats;

interface

type
  TStreamingStatistics = class
  private
    FCount: Int64;
    FSum: Double;
    FSumSquares: Double;
    FMin: Double;
    FMax: Double;
  public
    constructor Create;

    procedure AddValue(Value: Double);
    procedure Reset;

    function GetMean: Double;
    function GetVariance: Double;
    function GetStdDev: Double;
    function GetMin: Double;
    function GetMax: Double;
    function GetCount: Int64;
  end;

implementation

uses
  System.Math;

constructor TStreamingStatistics.Create;
begin
  inherited Create;
  Reset;
end;

procedure TStreamingStatistics.AddValue(Value: Double);
begin
  if FCount = 0 then
  begin
    FMin := Value;
    FMax := Value;
  end
  else
  begin
    if Value < FMin then FMin := Value;
    if Value > FMax then FMax := Value;
  end;

  Inc(FCount);
  FSum := FSum + Value;
  FSumSquares := FSumSquares + (Value * Value);
end;

procedure TStreamingStatistics.Reset;
begin
  FCount := 0;
  FSum := 0;
  FSumSquares := 0;
  FMin := 0;
  FMax := 0;
end;

function TStreamingStatistics.GetMean: Double;
begin
  if FCount > 0 then
    Result := FSum / FCount
  else
    Result := 0;
end;

function TStreamingStatistics.GetVariance: Double;
var
  Mean: Double;
begin
  if FCount > 1 then
  begin
    Mean := GetMean;
    Result := (FSumSquares - (FSum * FSum / FCount)) / (FCount - 1);
  end
  else
    Result := 0;
end;

function TStreamingStatistics.GetStdDev: Double;
begin
  Result := Sqrt(GetVariance);
end;

function TStreamingStatistics.GetMin: Double;
begin
  Result := FMin;
end;

function TStreamingStatistics.GetMax: Double;
begin
  Result := FMax;
end;

function TStreamingStatistics.GetCount: Int64;
begin
  Result := FCount;
end;

end.
```

### Agrégation par fenêtres temporelles

```pascal
unit TimeWindowAggregator;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.DateUtils;

type
  TTimeWindow = record
    StartTime: TDateTime;
    EndTime: TDateTime;
    Count: Integer;
    Sum: Double;
    Min: Double;
    Max: Double;
    Mean: Double;
  end;

  TWindowAggregator = class
  private
    FWindowDuration: Integer; // en secondes
    FCurrentWindow: TTimeWindow;
    FWindowHistory: TList<TTimeWindow>;
    FMaxHistorySize: Integer;

    procedure InitializeWindow(StartTime: TDateTime);
    procedure FinalizeWindow;
  public
    constructor Create(WindowDurationSeconds: Integer; MaxHistory: Integer = 100);
    destructor Destroy; override;

    procedure AddValue(Timestamp: TDateTime; Value: Double);
    function GetCurrentWindow: TTimeWindow;
    function GetWindowHistory: TArray<TTimeWindow>;
    function GetWindowAt(Timestamp: TDateTime): TTimeWindow;
  end;

implementation

constructor TWindowAggregator.Create(WindowDurationSeconds, MaxHistory: Integer);
begin
  inherited Create;
  FWindowDuration := WindowDurationSeconds;
  FMaxHistorySize := MaxHistory;
  FWindowHistory := TList<TTimeWindow>.Create;
  InitializeWindow(Now);
end;

destructor TWindowAggregator.Destroy;
begin
  FWindowHistory.Free;
  inherited;
end;

procedure TWindowAggregator.InitializeWindow(StartTime: TDateTime);
begin
  FCurrentWindow.StartTime := StartTime;
  FCurrentWindow.EndTime := IncSecond(StartTime, FWindowDuration);
  FCurrentWindow.Count := 0;
  FCurrentWindow.Sum := 0;
  FCurrentWindow.Min := MaxDouble;
  FCurrentWindow.Max := -MaxDouble;
end;

procedure TWindowAggregator.FinalizeWindow;
begin
  if FCurrentWindow.Count > 0 then
  begin
    FCurrentWindow.Mean := FCurrentWindow.Sum / FCurrentWindow.Count;

    // Ajouter à l'historique
    FWindowHistory.Add(FCurrentWindow);

    // Limiter la taille de l'historique
    while FWindowHistory.Count > FMaxHistorySize do
      FWindowHistory.Delete(0);
  end;
end;

procedure TWindowAggregator.AddValue(Timestamp: TDateTime; Value: Double);
begin
  // Si la valeur est en dehors de la fenêtre actuelle, finaliser et créer une nouvelle
  if Timestamp >= FCurrentWindow.EndTime then
  begin
    FinalizeWindow;
    InitializeWindow(FCurrentWindow.EndTime);
  end;

  // Ajouter la valeur à la fenêtre courante
  Inc(FCurrentWindow.Count);
  FCurrentWindow.Sum := FCurrentWindow.Sum + Value;

  if Value < FCurrentWindow.Min then
    FCurrentWindow.Min := Value;

  if Value > FCurrentWindow.Max then
    FCurrentWindow.Max := Value;
end;

function TWindowAggregator.GetCurrentWindow: TTimeWindow;
begin
  Result := FCurrentWindow;
end;

function TWindowAggregator.GetWindowHistory: TArray<TTimeWindow>;
begin
  Result := FWindowHistory.ToArray;
end;

function TWindowAggregator.GetWindowAt(Timestamp: TDateTime): TTimeWindow;
var
  Window: TTimeWindow;
begin
  // Chercher la fenêtre contenant ce timestamp
  for Window in FWindowHistory do
  begin
    if (Timestamp >= Window.StartTime) and (Timestamp < Window.EndTime) then
    begin
      Result := Window;
      Exit;
    end;
  end;

  // Si non trouvé, retourner la fenêtre actuelle si le timestamp y correspond
  if (Timestamp >= FCurrentWindow.StartTime) and (Timestamp < FCurrentWindow.EndTime) then
    Result := FCurrentWindow;
end;

end.
```

## Détection d'anomalies

### Détection par seuil statique

La méthode la plus simple : définir des seuils min/max :

```pascal
unit AnomalyDetector;

interface

type
  TAnomalyType = (atNone, atTooLow, atTooHigh, atRateOfChange);

  TAnomalyResult = record
    IsAnomaly: Boolean;
    AnomalyType: TAnomalyType;
    Value: Double;
    Threshold: Double;
    Severity: Double; // 0-1
  end;

  TThresholdDetector = class
  private
    FMinThreshold: Double;
    FMaxThreshold: Double;
  public
    constructor Create(MinThreshold, MaxThreshold: Double);

    function Detect(Value: Double): TAnomalyResult;
  end;

implementation

uses
  System.Math;

constructor TThresholdDetector.Create(MinThreshold, MaxThreshold: Double);
begin
  inherited Create;
  FMinThreshold := MinThreshold;
  FMaxThreshold := MaxThreshold;
end;

function TThresholdDetector.Detect(Value: Double): TAnomalyResult;
begin
  Result.Value := Value;
  Result.IsAnomaly := False;
  Result.AnomalyType := atNone;
  Result.Severity := 0;

  if Value < FMinThreshold then
  begin
    Result.IsAnomaly := True;
    Result.AnomalyType := atTooLow;
    Result.Threshold := FMinThreshold;
    Result.Severity := Min(1.0, (FMinThreshold - Value) / FMinThreshold);
  end
  else if Value > FMaxThreshold then
  begin
    Result.IsAnomaly := True;
    Result.AnomalyType := atTooHigh;
    Result.Threshold := FMaxThreshold;
    Result.Severity := Min(1.0, (Value - FMaxThreshold) / FMaxThreshold);
  end;
end;

end.
```

### Détection par écart-type (Z-Score)

Plus sophistiqué : détecter les valeurs qui s'écartent significativement de la moyenne :

```pascal
type
  TStatisticalDetector = class
  private
    FStats: TStreamingStatistics;
    FZScoreThreshold: Double;
    FLastValue: Double;
  public
    constructor Create(ZScoreThreshold: Double = 3.0);
    destructor Destroy; override;

    function Detect(Value: Double): TAnomalyResult;
    procedure Reset;
  end;

implementation

constructor TStatisticalDetector.Create(ZScoreThreshold: Double);
begin
  inherited Create;
  FStats := TStreamingStatistics.Create;
  FZScoreThreshold := ZScoreThreshold;
end;

destructor TStatisticalDetector.Destroy;
begin
  FStats.Free;
  inherited;
end;

function TStatisticalDetector.Detect(Value: Double): TAnomalyResult;
var
  Mean, StdDev, ZScore: Double;
begin
  Result.Value := Value;
  Result.IsAnomaly := False;
  Result.AnomalyType := atNone;
  Result.Severity := 0;

  // Ajouter la valeur aux statistiques
  FStats.AddValue(Value);

  // Nécessite au moins 10 valeurs pour une détection fiable
  if FStats.GetCount < 10 then
  begin
    FLastValue := Value;
    Exit;
  end;

  Mean := FStats.GetMean;
  StdDev := FStats.GetStdDev;

  // Éviter division par zéro
  if StdDev < 0.0001 then
  begin
    FLastValue := Value;
    Exit;
  end;

  // Calculer le Z-Score
  ZScore := Abs(Value - Mean) / StdDev;

  if ZScore > FZScoreThreshold then
  begin
    Result.IsAnomaly := True;

    if Value > Mean then
      Result.AnomalyType := atTooHigh
    else
      Result.AnomalyType := atTooLow;

    Result.Threshold := Mean;
    Result.Severity := Min(1.0, ZScore / (FZScoreThreshold * 2));
  end;

  FLastValue := Value;
end;

procedure TStatisticalDetector.Reset;
begin
  FStats.Reset;
end;

end.
```

### Détection de taux de variation

Détecter des changements trop rapides :

```pascal
type
  TRateOfChangeDetector = class
  private
    FMaxChangeRate: Double;
    FLastValue: Double;
    FLastTimestamp: TDateTime;
    FInitialized: Boolean;
  public
    constructor Create(MaxChangeRate: Double);

    function Detect(Timestamp: TDateTime; Value: Double): TAnomalyResult;
    procedure Reset;
  end;

constructor TRateOfChangeDetector.Create(MaxChangeRate: Double);
begin
  inherited Create;
  FMaxChangeRate := MaxChangeRate;
  FInitialized := False;
end;

function TRateOfChangeDetector.Detect(Timestamp: TDateTime; Value: Double): TAnomalyResult;
var
  TimeDelta, ValueDelta, ChangeRate: Double;
begin
  Result.Value := Value;
  Result.IsAnomaly := False;
  Result.AnomalyType := atNone;
  Result.Severity := 0;

  if not FInitialized then
  begin
    FLastValue := Value;
    FLastTimestamp := Timestamp;
    FInitialized := True;
    Exit;
  end;

  // Calculer les deltas
  TimeDelta := SecondsBetween(Timestamp, FLastTimestamp);
  if TimeDelta < 0.1 then // Éviter division par des valeurs trop petites
  begin
    FLastValue := Value;
    FLastTimestamp := Timestamp;
    Exit;
  end;

  ValueDelta := Abs(Value - FLastValue);
  ChangeRate := ValueDelta / TimeDelta;

  if ChangeRate > FMaxChangeRate then
  begin
    Result.IsAnomaly := True;
    Result.AnomalyType := atRateOfChange;
    Result.Threshold := FMaxChangeRate;
    Result.Severity := Min(1.0, ChangeRate / (FMaxChangeRate * 2));
  end;

  FLastValue := Value;
  FLastTimestamp := Timestamp;
end;

procedure TRateOfChangeDetector.Reset;
begin
  FInitialized := False;
end;

end.
```

## Traitement multi-thread

### Architecture thread pour traitement temps réel

```pascal
unit DataProcessingThread;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.SyncObjs, DataPipeline;

type
  TDataProcessedEvent = procedure(const Data: TProcessedData) of object;

  TDataProcessingThread = class(TThread)
  private
    FInputQueue: TThreadedQueue<TSensorReading>;
    FProcessor: TDataProcessor;
    FOnDataProcessed: TDataProcessedEvent;

    procedure ProcessData;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddReading(const Reading: TSensorReading);

    property OnDataProcessed: TDataProcessedEvent read FOnDataProcessed write FOnDataProcessed;
  end;

implementation

constructor TDataProcessingThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;

  FInputQueue := TThreadedQueue<TSensorReading>.Create(1000, INFINITE, 100);
  FProcessor := TDataProcessor.Create(20);
end;

destructor TDataProcessingThread.Destroy;
begin
  Terminate;
  WaitFor;

  FInputQueue.Free;
  FProcessor.Free;
  inherited;
end;

procedure TDataProcessingThread.AddReading(const Reading: TSensorReading);
begin
  FInputQueue.PushItem(Reading);
end;

procedure TDataProcessingThread.ProcessData;
var
  Reading: TSensorReading;
  Processed: TProcessedData;
begin
  if FInputQueue.PopItem(Reading) = wrSignaled then
  begin
    // Traiter la donnée
    Processed := FProcessor.Process(Reading);

    // Notifier via événement (synchronisé avec le thread principal)
    if Assigned(FOnDataProcessed) then
    begin
      TThread.Synchronize(nil, procedure
      begin
        FOnDataProcessed(Processed);
      end);
    end;
  end;
end;

procedure TDataProcessingThread.Execute;
begin
  while not Terminated do
  begin
    ProcessData;
  end;
end;

end.
```

### Pool de threads pour traitement parallèle

Pour des volumes très élevés, utiliser plusieurs threads :

```pascal
unit DataProcessingPool;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DataProcessingThread, DataPipeline;

type
  TProcessingPool = class
  private
    FThreads: TObjectList<TDataProcessingThread>;
    FCurrentThread: Integer;
  public
    constructor Create(ThreadCount: Integer = 4);
    destructor Destroy; override;

    procedure AddReading(const Reading: TSensorReading);
    procedure SetOnDataProcessed(Event: TDataProcessedEvent);
  end;

implementation

constructor TProcessingPool.Create(ThreadCount: Integer);
var
  I: Integer;
  Thread: TDataProcessingThread;
begin
  inherited Create;
  FThreads := TObjectList<TDataProcessingThread>.Create(True);
  FCurrentThread := 0;

  for I := 0 to ThreadCount - 1 do
  begin
    Thread := TDataProcessingThread.Create;
    FThreads.Add(Thread);
  end;
end;

destructor TProcessingPool.Destroy;
begin
  FThreads.Free;
  inherited;
end;

procedure TProcessingPool.AddReading(const Reading: TSensorReading);
begin
  // Distribution round-robin
  FThreads[FCurrentThread].AddReading(Reading);

  FCurrentThread := (FCurrentThread + 1) mod FThreads.Count;
end;

procedure TProcessingPool.SetOnDataProcessed(Event: TDataProcessedEvent);
var
  Thread: TDataProcessingThread;
begin
  for Thread in FThreads do
    Thread.OnDataProcessed := Event;
end;

end.
```

## Actions automatiques et règles métier

### Moteur de règles

```pascal
unit RulesEngine;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  DataPipeline;

type
  TRuleCondition = (rcEquals, rcGreaterThan, rcLessThan, rcBetween, rcOutside);
  TRuleAction = (raLog, raAlert, raEmail, raCommand, raCustom);

  TRule = class
  private
    FName: string;
    FEnabled: Boolean;
    FCondition: TRuleCondition;
    FThreshold1: Double;
    FThreshold2: Double;
    FAction: TRuleAction;
    FActionParameter: string;
  public
    function Evaluate(const Data: TProcessedData): Boolean;
    procedure Execute(const Data: TProcessedData);

    property Name: string read FName write FName;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Condition: TRuleCondition read FCondition write FCondition;
    property Threshold1: Double read FThreshold1 write FThreshold1;
    property Threshold2: Double read FThreshold2 write FThreshold2;
    property Action: TRuleAction read FAction write FAction;
    property ActionParameter: string read FActionParameter write FActionParameter;
  end;

  TRulesEngine = class
  private
    FRules: TObjectList<TRule>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRule(Rule: TRule);
    procedure ProcessData(const Data: TProcessedData);
  end;

implementation

{ TRule }

function TRule.Evaluate(const Data: TProcessedData): Boolean;
begin
  if not FEnabled then
  begin
    Result := False;
    Exit;
  end;

  Result := False;

  case FCondition of
    rcEquals:
      Result := Abs(Data.FilteredValue - FThreshold1) < 0.01;

    rcGreaterThan:
      Result := Data.FilteredValue > FThreshold1;

    rcLessThan:
      Result := Data.FilteredValue < FThreshold1;

    rcBetween:
      Result := (Data.FilteredValue >= FThreshold1) and
                (Data.FilteredValue <= FThreshold2);

    rcOutside:
      Result := (Data.FilteredValue < FThreshold1) or
                (Data.FilteredValue > FThreshold2);
  end;
end;

procedure TRule.Execute(const Data: TProcessedData);
begin
  case FAction of
    raLog:
      begin
        // Logger l'événement
        WriteLn(Format('[%s] Règle "%s" déclenchée: %.2f',
          [FormatDateTime('hh:nn:ss', Now), FName, Data.FilteredValue]));
      end;

    raAlert:
      begin
        // Créer une alerte
        // (Implémentation dépend du système d'alertes)
      end;

    raEmail:
      begin
        // Envoyer un email
        // (Implémentation dépend du système d'email)
      end;

    raCommand:
      begin
        // Exécuter une commande
        // Par exemple: envoyer une commande à un dispositif
      end;
  end;
end;

{ TRulesEngine }

constructor TRulesEngine.Create;
begin
  inherited Create;
  FRules := TObjectList<TRule>.Create(True);
end;

destructor TRulesEngine.Destroy;
begin
  FRules.Free;
  inherited;
end;

procedure TRulesEngine.AddRule(Rule: TRule);
begin
  FRules.Add(Rule);
end;

procedure TRulesEngine.ProcessData(const Data: TProcessedData);
var
  Rule: TRule;
begin
  for Rule in FRules do
  begin
    if Rule.Evaluate(Data) then
      Rule.Execute(Data);
  end;
end;

end.
```

### Exemple d'utilisation du moteur de règles

```pascal
procedure TFormMain.SetupRules;
var
  Rule: TRule;
begin
  RulesEngine := TRulesEngine.Create;

  // Règle 1: Alerte si température > 30°C
  Rule := TRule.Create;
  Rule.Name := 'Température élevée';
  Rule.Enabled := True;
  Rule.Condition := rcGreaterThan;
  Rule.Threshold1 := 30.0;
  Rule.Action := raAlert;
  Rule.ActionParameter := 'Température trop élevée!';
  RulesEngine.AddRule(Rule);

  // Règle 2: Email si température < 10°C
  Rule := TRule.Create;
  Rule.Name := 'Risque de gel';
  Rule.Enabled := True;
  Rule.Condition := rcLessThan;
  Rule.Threshold1 := 10.0;
  Rule.Action := raEmail;
  Rule.ActionParameter := 'admin@example.com';
  RulesEngine.AddRule(Rule);

  // Règle 3: Activer ventilation si température > 25°C
  Rule := TRule.Create;
  Rule.Name := 'Ventilation automatique';
  Rule.Enabled := True;
  Rule.Condition := rcGreaterThan;
  Rule.Threshold1 := 25.0;
  Rule.Action := raCommand;
  Rule.ActionParameter := 'FAN:ON';
  RulesEngine.AddRule(Rule);
end;

procedure TFormMain.OnDataProcessed(const Data: TProcessedData);
begin
  // Appliquer les règles
  RulesEngine.ProcessData(Data);

  // Mettre à jour l'affichage
  UpdateDisplay(Data);
end;
```

## Mise en cache et optimisation

### Cache LRU (Least Recently Used)

Pour accélérer l'accès aux données fréquemment consultées :

```pascal
unit LRUCache;

interface

uses
  System.SysUtils, System.Generics.Collections;

type
  TLRUCache<TKey, TValue> = class
  private
    FCapacity: Integer;
    FCache: TDictionary<TKey, TValue>;
    FAccessOrder: TList<TKey>;

    procedure UpdateAccess(const Key: TKey);
  public
    constructor Create(Capacity: Integer);
    destructor Destroy; override;

    procedure Put(const Key: TKey; const Value: TValue);
    function TryGet(const Key: TKey; out Value: TValue): Boolean;
    procedure Clear;
  end;

implementation

constructor TLRUCache<TKey, TValue>.Create(Capacity: Integer);
begin
  inherited Create;
  FCapacity := Capacity;
  FCache := TDictionary<TKey, TValue>.Create;
  FAccessOrder := TList<TKey>.Create;
end;

destructor TLRUCache<TKey, TValue>.Destroy;
begin
  FCache.Free;
  FAccessOrder.Free;
  inherited;
end;

procedure TLRUCache<TKey, TValue>.UpdateAccess(const Key: TKey);
begin
  FAccessOrder.Remove(Key);
  FAccessOrder.Add(Key);
end;

procedure TLRUCache<TKey, TValue>.Put(const Key: TKey; const Value: TValue);
var
  OldestKey: TKey;
begin
  // Si la clé existe, la mettre à jour
  if FCache.ContainsKey(Key) then
  begin
    FCache[Key] := Value;
    UpdateAccess(Key);
    Exit;
  end;

  // Si le cache est plein, supprimer l'entrée la moins récemment utilisée
  if FCache.Count >= FCapacity then
  begin
    OldestKey := FAccessOrder[0];
    FCache.Remove(OldestKey);
    FAccessOrder.Delete(0);
  end;

  // Ajouter la nouvelle entrée
  FCache.Add(Key, Value);
  FAccessOrder.Add(Key);
end;

function TLRUCache<TKey, TValue>.TryGet(const Key: TKey; out Value: TValue): Boolean;
begin
  Result := FCache.TryGetValue(Key, Value);
  if Result then
    UpdateAccess(Key);
end;

procedure TLRUCache<TKey, TValue>.Clear;
begin
  FCache.Clear;
  FAccessOrder.Clear;
end;

end.
```

## Application complète : Monitoring en temps réel

### Formulaire principal

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, VCLTee.Chart, VCLTee.Series,
  DataProcessingThread, DataPipeline, AnomalyDetector, RulesEngine,
  StreamingStats;

type
  TFormMonitoring = class(TForm)
    Chart1: TChart;
    Series1: TLineSeries;  // Données brutes
    Series2: TLineSeries;  // Données filtrées
    PanelStats: TPanel;
    LabelMean: TLabel;
    LabelStdDev: TLabel;
    LabelMin: TLabel;
    LabelMax: TLabel;
    MemoAlerts: TMemo;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FProcessingThread: TDataProcessingThread;
    FStats: TStreamingStatistics;
    FAnomalyDetector: TStatisticalDetector;
    FRulesEngine: TRulesEngine;

    procedure OnDataProcessed(const Data: TProcessedData);
    procedure UpdateStatistics;
    procedure SimulateDataInput;
  end;

var
  FormMonitoring: TFormMonitoring;

implementation

{$R *.dfm}

procedure TFormMonitoring.FormCreate(Sender: TObject);
begin
  // Initialiser les composants
  FProcessingThread := TDataProcessingThread.Create;
  FProcessingThread.OnDataProcessed := OnDataProcessed;

  FStats := TStreamingStatistics.Create;
  FAnomalyDetector := TStatisticalDetector.Create(3.0);
  FRulesEngine := TRulesEngine.Create;

  // Configurer le graphique
  Series1.Clear;
  Series2.Clear;
  Chart1.Title.Text.Text := 'Monitoring en temps réel';
  Series1.Title := 'Données brutes';
  Series2.Title := 'Données filtrées';

  // Démarrer la simulation
  Timer1.Enabled := True;
end;

procedure TFormMonitoring.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  FProcessingThread.Free;
  FStats.Free;
  FAnomalyDetector.Free;
  FRulesEngine.Free;
end;

procedure TFormMonitoring.OnDataProcessed(const Data: TProcessedData);
var
  AnomalyResult: TAnomalyResult;
begin
  // Ajouter aux statistiques
  FStats.AddValue(Data.FilteredValue);

  // Mettre à jour le graphique (garder 100 points)
  Series1.AddXY(Now, Data.Reading.Value);
  Series2.AddXY(Now, Data.FilteredValue);

  if Series1.Count > 100 then
  begin
    Series1.Delete(0);
    Series2.Delete(0);
  end;

  // Détecter les anomalies
  AnomalyResult := FAnomalyDetector.Detect(Data.FilteredValue);
  if AnomalyResult.IsAnomaly then
  begin
    MemoAlerts.Lines.Add(Format('[%s] ANOMALIE: %.2f (sévérité: %.0f%%)',
      [FormatDateTime('hh:nn:ss', Now),
       AnomalyResult.Value,
       AnomalyResult.Severity * 100]));
  end;

  // Appliquer les règles métier
  FRulesEngine.ProcessData(Data);
end;

procedure TFormMonitoring.UpdateStatistics;
begin
  LabelMean.Caption := Format('Moyenne: %.2f', [FStats.GetMean]);
  LabelStdDev.Caption := Format('Écart-type: %.2f', [FStats.GetStdDev]);
  LabelMin.Caption := Format('Min: %.2f', [FStats.GetMin]);
  LabelMax.Caption := Format('Max: %.2f', [FStats.GetMax]);
end;

procedure TFormMonitoring.SimulateDataInput;
var
  Reading: TSensorReading;
  BaseValue, Noise: Double;
begin
  // Simuler des données de capteur avec du bruit
  BaseValue := 22 + Sin(Now * 100) * 3;  // Oscillation autour de 22
  Noise := (Random - 0.5) * 2;  // Bruit aléatoire ±1

  // Ajouter occasionnellement des valeurs aberrantes
  if Random < 0.05 then
    Noise := Noise * 10;

  Reading.Timestamp := Now;
  Reading.DeviceID := 'SENSOR_001';
  Reading.SensorType := 'temperature';
  Reading.Value := BaseValue + Noise;
  Reading.Unit_ := '°C';

  // Envoyer au thread de traitement
  FProcessingThread.AddReading(Reading);
end;

procedure TFormMonitoring.Timer1Timer(Sender: TObject);
begin
  SimulateDataInput;
  UpdateStatistics;
end;

end.
```

## Performance et optimisation

### Conseils pour optimiser le traitement temps réel

#### 1. Batch processing

Traiter les données par lots plutôt qu'une par une :

```pascal
procedure ProcessBatch(const Readings: TArray<TSensorReading>);
var
  Reading: TSensorReading;
begin
  TParallel.For(0, Length(Readings) - 1, procedure(Index: Integer)
  begin
    ProcessSingleReading(Readings[Index]);
  end);
end;
```

#### 2. Échantillonnage adaptatif

Réduire la fréquence d'échantillonnage quand les valeurs sont stables :

```pascal
function ShouldSample(CurrentValue, LastValue: Double): Boolean;
var
  Change: Double;
begin
  Change := Abs(CurrentValue - LastValue);

  if Change < 0.1 then
    Result := Random < 0.1  // Échantillonner 10% du temps
  else
    Result := True;  // Échantillonner toujours si changement significatif
end;
```

#### 3. Utiliser des structures de données efficaces

Préférer les tableaux circulaires aux listes pour les buffers :

```pascal
type
  TCircularBuffer<T> = class
  private
    FBuffer: TArray<T>;
    FCapacity: Integer;
    FHead: Integer;
    FCount: Integer;
  public
    constructor Create(Capacity: Integer);
    procedure Add(const Item: T);
    function GetLast(Count: Integer): TArray<T>;
  end;
```

## Conclusion

Le traitement de données IoT en temps réel est un défi technique passionnant qui nécessite de bien comprendre les algorithmes de traitement de signal, les statistiques, et la programmation concurrente.

**Points clés à retenir :**

1. **Pipeline** : structurer le traitement en étapes (validation, filtrage, agrégation)
2. **Filtrage** : utiliser des filtres adaptés (moyenne mobile, Kalman) pour réduire le bruit
3. **Agrégation** : calculer des statistiques en streaming sans stocker toutes les valeurs
4. **Anomalies** : combiner plusieurs méthodes de détection pour plus de fiabilité
5. **Multithreading** : ne jamais bloquer l'interface utilisateur, utiliser des threads de traitement
6. **Règles métier** : automatiser les actions avec un moteur de règles flexible
7. **Performance** : optimiser les structures de données et algorithmes
8. **Scalabilité** : concevoir pour gérer des volumes croissants

Dans la section suivante, nous verrons comment créer des tableaux de bord visuels impressionnants pour présenter toutes ces données traitées de manière claire et intuitive.

⏭️ [Tableaux de bord pour solutions IoT](/21-delphi-et-liot/08-tableaux-de-bord-pour-solutions-iot.md)
