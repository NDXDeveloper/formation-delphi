🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 21.8 Tableaux de bord pour solutions IoT

## Introduction

Un tableau de bord IoT est bien plus qu'un simple écran d'affichage : c'est l'interface centrale qui permet de superviser, comprendre et agir sur votre système IoT. Un bon tableau de bord transforme des milliers de données brutes en informations exploitables en un coup d'œil.

Dans cette section, nous allons apprendre à créer des tableaux de bord professionnels avec Delphi, en utilisant :
- Des graphiques en temps réel
- Des indicateurs visuels (jauges, KPI)
- Des cartes interactives
- Des systèmes d'alertes visuelles
- Des mises en page responsives

## Principes de conception

### Les 5 règles d'or

#### 1. Clarté avant tout
Un tableau de bord doit être compréhensible en moins de 5 secondes. Évitez la surcharge d'informations.

**Bon exemple :**
```
┌─────────────────────────────────────┐
│ 🏠 Température Maison               │
│                                     │
│  ████████████████░░░░  22.5°C      │
│  Cible: 21°C          Normal       │
└─────────────────────────────────────┘
```

**Mauvais exemple :**
```
┌─────────────────────────────────────┐
│ Temp: 22.5°C | Min:20.1 | Max:23.4 │
│ Avg:21.8 | StdDev:0.8 | Samples:842│
│ Trend:↗ | Forecast:22.7 | Conf:87% │
└─────────────────────────────────────┘
```

#### 2. Hiérarchie de l'information
Les informations les plus importantes doivent être les plus visibles.

**Niveaux d'importance :**
1. **Alertes critiques** : rouge, grandes, en haut
2. **KPI principaux** : grands, colorés, zone centrale
3. **Graphiques de tendance** : taille moyenne
4. **Détails** : petits, en bas ou dans des onglets

#### 3. Utilisation des couleurs

Les couleurs doivent avoir une signification immédiate :
- **Vert** : tout va bien, objectif atteint
- **Jaune/Orange** : attention, surveillance nécessaire
- **Rouge** : problème, action immédiate requise
- **Bleu** : information neutre
- **Gris** : inactif, désactivé, pas de donnée

#### 4. Contexte et comparaison

Toujours donner du contexte aux valeurs :
- **Mauvais** : "22.5°C"
- **Bon** : "22.5°C (objectif: 21°C)"
- **Meilleur** : "22.5°C (objectif: 21°C, hier: 23.1°C)"

#### 5. Interaction intuitive

L'utilisateur doit pouvoir :
- Voir les détails au survol
- Cliquer pour approfondir
- Filtrer rapidement
- Exporter facilement

## Composants visuels essentiels

### 1. Indicateurs KPI (Key Performance Indicators)

Les KPI affichent les métriques les plus importantes de manière immédiate.

```pascal
unit KPIWidget;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics,
  Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TKPIStatus = (ksNormal, ksWarning, ksCritical, ksUnknown);

  TKPIWidget = class(TPanel)
  private
    FTitleLabel: TLabel;
    FValueLabel: TLabel;
    FStatusLabel: TLabel;
    FTrendLabel: TLabel;

    FTitle: string;
    FValue: Double;
    FUnit: string;
    FStatus: TKPIStatus;
    FTrend: Double; // Pourcentage de variation

    procedure UpdateDisplay;
    procedure SetTitle(const Value: string);
    procedure SetValue(const Value: Double);
    procedure SetStatus(const Value: TKPIStatus);
    procedure SetTrend(const Value: Double);
  public
    constructor Create(AOwner: TComponent); override;

    property Title: string read FTitle write SetTitle;
    property Value: Double read FValue write SetValue;
    property Unit_: string read FUnit write FUnit;
    property Status: TKPIStatus read FStatus write SetStatus;
    property Trend: Double read FTrend write SetTrend;
  end;

implementation

constructor TKPIWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 200;
  Height := 120;
  BevelOuter := bvNone;
  Color := clWhite;
  ParentBackground := False;

  // Titre
  FTitleLabel := TLabel.Create(Self);
  FTitleLabel.Parent := Self;
  FTitleLabel.Align := alTop;
  FTitleLabel.Height := 25;
  FTitleLabel.AlignWithMargins := True;
  FTitleLabel.Margins.SetBounds(10, 10, 10, 5);
  FTitleLabel.Font.Size := 10;
  FTitleLabel.Font.Color := clGray;

  // Valeur principale
  FValueLabel := TLabel.Create(Self);
  FValueLabel.Parent := Self;
  FValueLabel.Align := alClient;
  FValueLabel.AlignWithMargins := True;
  FValueLabel.Margins.SetBounds(10, 5, 10, 5);
  FValueLabel.Font.Size := 24;
  FValueLabel.Font.Style := [fsBold];
  FValueLabel.Alignment := taCenter;
  FValueLabel.Layout := tlCenter;

  // Statut et tendance
  FStatusLabel := TLabel.Create(Self);
  FStatusLabel.Parent := Self;
  FStatusLabel.Align := alBottom;
  FStatusLabel.Height := 20;
  FStatusLabel.AlignWithMargins := True;
  FStatusLabel.Margins.SetBounds(10, 0, 10, 5);
  FStatusLabel.Font.Size := 9;

  FTrendLabel := TLabel.Create(Self);
  FTrendLabel.Parent := Self;
  FTrendLabel.Align := alBottom;
  FTrendLabel.Height := 20;
  FTrendLabel.AlignWithMargins := True;
  FTrendLabel.Margins.SetBounds(10, 0, 10, 5);
  FTrendLabel.Font.Size := 9;

  FStatus := ksUnknown;
  FTrend := 0;

  UpdateDisplay;
end;

procedure TKPIWidget.UpdateDisplay;
var
  StatusColor: TColor;
  StatusText, TrendArrow: string;
begin
  // Titre
  FTitleLabel.Caption := FTitle;

  // Valeur
  if FUnit <> '' then
    FValueLabel.Caption := Format('%.1f %s', [FValue, FUnit])
  else
    FValueLabel.Caption := Format('%.1f', [FValue]);

  // Statut avec couleur
  case FStatus of
    ksNormal:
      begin
        StatusColor := $00C5E1A5; // Vert clair
        StatusText := '● Normal';
      end;
    ksWarning:
      begin
        StatusColor := $0080D8FF; // Orange
        StatusText := '● Attention';
      end;
    ksCritical:
      begin
        StatusColor := $004C4CFF; // Rouge
        StatusText := '● Critique';
      end;
    else
      begin
        StatusColor := clGray;
        StatusText := '● Inconnu';
      end;
  end;

  Self.Color := StatusColor;
  FStatusLabel.Caption := StatusText;

  // Tendance
  if FTrend > 0 then
    TrendArrow := '↗'
  else if FTrend < 0 then
    TrendArrow := '↘'
  else
    TrendArrow := '→';

  FTrendLabel.Caption := Format('%s %.1f%%', [TrendArrow, Abs(FTrend)]);

  if FTrend > 0 then
    FTrendLabel.Font.Color := clRed
  else if FTrend < 0 then
    FTrendLabel.Font.Color := clBlue
  else
    FTrendLabel.Font.Color := clGray;
end;

procedure TKPIWidget.SetTitle(const Value: string);
begin
  FTitle := Value;
  UpdateDisplay;
end;

procedure TKPIWidget.SetValue(const Value: Double);
begin
  FValue := Value;
  UpdateDisplay;
end;

procedure TKPIWidget.SetStatus(const Value: TKPIStatus);
begin
  FStatus := Value;
  UpdateDisplay;
end;

procedure TKPIWidget.SetTrend(const Value: Double);
begin
  FTrend := Value;
  UpdateDisplay;
end;

end.
```

### Utilisation du widget KPI

```pascal
procedure TFormDashboard.CreateKPIs;
var
  KPI: TKPIWidget;
begin
  // KPI Température
  KPI := TKPIWidget.Create(Self);
  KPI.Parent := PanelKPIs;
  KPI.Align := alLeft;
  KPI.Title := 'Température';
  KPI.Value := 22.5;
  KPI.Unit_ := '°C';
  KPI.Status := ksNormal;
  KPI.Trend := 2.3;

  // KPI Humidité
  KPI := TKPIWidget.Create(Self);
  KPI.Parent := PanelKPIs;
  KPI.Align := alLeft;
  KPI.Title := 'Humidité';
  KPI.Value := 65;
  KPI.Unit_ := '%';
  KPI.Status := ksWarning;
  KPI.Trend := -1.5;
end;
```

### 2. Jauges circulaires

Les jauges sont parfaites pour montrer un pourcentage ou une valeur par rapport à un maximum.

```pascal
unit GaugeWidget;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics,
  Vcl.ExtCtrls, System.Math;

type
  TGaugeWidget = class(TCustomControl)
  private
    FValue: Double;
    FMinValue: Double;
    FMaxValue: Double;
    FTitle: string;
    FWarningThreshold: Double;
    FCriticalThreshold: Double;

    procedure SetValue(const Value: Double);
    procedure Paint; override;
    function GetPercentage: Double;
  public
    constructor Create(AOwner: TComponent); override;

    property Value: Double read FValue write SetValue;
    property MinValue: Double read FMinValue write FMinValue;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property Title: string read FTitle write FTitle;
    property WarningThreshold: Double read FWarningThreshold write FWarningThreshold;
    property CriticalThreshold: Double read FCriticalThreshold write FCriticalThreshold;
  end;

implementation

constructor TGaugeWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 200;
  Height := 200;
  FMinValue := 0;
  FMaxValue := 100;
  FValue := 0;
  FWarningThreshold := 70;
  FCriticalThreshold := 90;
end;

procedure TGaugeWidget.SetValue(const Value: Double);
begin
  FValue := EnsureRange(Value, FMinValue, FMaxValue);
  Invalidate;
end;

function TGaugeWidget.GetPercentage: Double;
begin
  if FMaxValue > FMinValue then
    Result := (FValue - FMinValue) / (FMaxValue - FMinValue) * 100
  else
    Result := 0;
end;

procedure TGaugeWidget.Paint;
var
  CenterX, CenterY, Radius: Integer;
  StartAngle, SweepAngle: Integer;
  Rect: TRect;
  GaugeColor: TColor;
  Percentage: Double;
  ValueText: string;
begin
  inherited;

  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  CenterX := Width div 2;
  CenterY := Height div 2;
  Radius := Min(Width, Height) div 2 - 20;

  Rect := Bounds(CenterX - Radius, CenterY - Radius, Radius * 2, Radius * 2);

  // Fond de la jauge (gris)
  Canvas.Pen.Width := 15;
  Canvas.Pen.Color := $00E0E0E0;
  Canvas.Brush.Style := bsClear;
  Canvas.Arc(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom,
             Rect.Right, CenterY, Rect.Left, CenterY);

  // Déterminer la couleur selon la valeur
  Percentage := GetPercentage;
  if Percentage >= FCriticalThreshold then
    GaugeColor := clRed
  else if Percentage >= FWarningThreshold then
    GaugeColor := clYellow
  else
    GaugeColor := $0000C000; // Vert

  // Arc de la valeur
  Canvas.Pen.Color := GaugeColor;

  StartAngle := 180;
  SweepAngle := Round(180 * Percentage / 100);

  Canvas.AngleArc(CenterX, CenterY, Radius, StartAngle, -SweepAngle);

  // Texte central - Titre
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Size := 10;
  Canvas.Font.Color := clGray;
  Canvas.Font.Style := [];
  Canvas.TextOut(CenterX - Canvas.TextWidth(FTitle) div 2,
                 CenterY - 30, FTitle);

  // Texte central - Valeur
  ValueText := Format('%.1f', [FValue]);
  Canvas.Font.Size := 24;
  Canvas.Font.Color := clBlack;
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(CenterX - Canvas.TextWidth(ValueText) div 2,
                 CenterY - 10, ValueText);

  // Pourcentage
  ValueText := Format('%.0f%%', [Percentage]);
  Canvas.Font.Size := 12;
  Canvas.Font.Style := [];
  Canvas.TextOut(CenterX - Canvas.TextWidth(ValueText) div 2,
                 CenterY + 20, ValueText);
end;

end.
```

### 3. Graphiques en temps réel avec TeeChart

TeeChart est inclus avec Delphi et parfait pour les graphiques IoT.

```pascal
unit RealtimeChart;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, VCLTee.Chart,
  VCLTee.Series, VCLTee.TeEngine, System.DateUtils;

type
  TRealtimeChartManager = class
  private
    FChart: TChart;
    FSeries: TLineSeries;
    FMaxPoints: Integer;

    procedure ConfigureChart;
  public
    constructor Create(Chart: TChart; MaxPoints: Integer = 100);

    procedure AddDataPoint(Timestamp: TDateTime; Value: Double);
    procedure Clear;
  end;

implementation

constructor TRealtimeChartManager.Create(Chart: TChart; MaxPoints: Integer);
begin
  inherited Create;
  FChart := Chart;
  FMaxPoints := MaxPoints;

  // Créer la série
  FSeries := TLineSeries.Create(FChart);
  FSeries.ParentChart := FChart;

  ConfigureChart;
end;

procedure TRealtimeChartManager.ConfigureChart;
begin
  // Configuration du graphique
  FChart.Title.Visible := False;
  FChart.Legend.Visible := False;
  FChart.View3D := False;
  FChart.Color := clWhite;

  // Axe X (temps)
  FChart.BottomAxis.DateTimeFormat := 'hh:nn:ss';
  FChart.BottomAxis.Labels := True;
  FChart.BottomAxis.LabelsAngle := 45;
  FChart.BottomAxis.Grid.Visible := True;
  FChart.BottomAxis.Grid.Color := $00F0F0F0;

  // Axe Y
  FChart.LeftAxis.Grid.Visible := True;
  FChart.LeftAxis.Grid.Color := $00F0F0F0;
  FChart.LeftAxis.Automatic := True;

  // Série
  FSeries.LinePen.Width := 2;
  FSeries.LinePen.Color := $00FF8000; // Orange
  FSeries.Pointer.Visible := False;
  FSeries.XValues.DateTime := True;
end;

procedure TRealtimeChartManager.AddDataPoint(Timestamp: TDateTime; Value: Double);
begin
  FSeries.AddXY(Timestamp, Value);

  // Limiter le nombre de points
  while FSeries.Count > FMaxPoints do
    FSeries.Delete(0);

  // Ajuster l'axe X pour toujours montrer les dernières données
  if FSeries.Count > 0 then
  begin
    FChart.BottomAxis.SetMinMax(
      FSeries.XValue[0],
      FSeries.XValue[FSeries.Count - 1]
    );
  end;
end;

procedure TRealtimeChartManager.Clear;
begin
  FSeries.Clear;
end;

end.
```

### 4. Carte de statut des dispositifs

Affichage visuel de l'état de plusieurs dispositifs.

```pascal
unit DeviceStatusGrid;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Grids,
  Vcl.Graphics, System.Generics.Collections, IoTDevice;

type
  TDeviceStatusGrid = class(TStringGrid)
  private
    FDevices: TList<TIoTDevice>;

    procedure UpdateGrid;
    function GetStatusColor(Status: TDeviceStatus): TColor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetDevices(Devices: TArray<TIoTDevice>);
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect;
                      AState: TGridDrawState); override;
  end;

implementation

constructor TDeviceStatusGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDevices := TList<TIoTDevice>.Create;

  // Configuration de la grille
  ColCount := 5;
  RowCount := 1;
  FixedCols := 0;
  FixedRows := 1;
  Options := Options + [goDrawFocusSelected, goRowSelect];

  // En-têtes
  Cells[0, 0] := 'État';
  Cells[1, 0] := 'Nom';
  Cells[2, 0] := 'Type';
  Cells[3, 0] := 'IP';
  Cells[4, 0] := 'Batterie';

  ColWidths[0] := 60;
  ColWidths[1] := 200;
  ColWidths[2] := 100;
  ColWidths[3] := 120;
  ColWidths[4] := 80;
end;

destructor TDeviceStatusGrid.Destroy;
begin
  FDevices.Free;
  inherited;
end;

procedure TDeviceStatusGrid.SetDevices(Devices: TArray<TIoTDevice>);
var
  Device: TIoTDevice;
begin
  FDevices.Clear;
  for Device in Devices do
    FDevices.Add(Device);

  UpdateGrid;
end;

procedure TDeviceStatusGrid.UpdateGrid;
var
  I: Integer;
  Device: TIoTDevice;
  TypeStr: string;
begin
  RowCount := FDevices.Count + 1;

  for I := 0 to FDevices.Count - 1 do
  begin
    Device := FDevices[I];

    // Statut (sera dessiné avec une couleur)
    Cells[0, I + 1] := '';

    // Nom
    Cells[1, I + 1] := Device.Name;

    // Type
    case Device.DeviceType of
      dtSensor: TypeStr := 'Capteur';
      dtActuator: TypeStr := 'Actionneur';
      dtGateway: TypeStr := 'Passerelle';
      dtController: TypeStr := 'Contrôleur';
      else TypeStr := 'Inconnu';
    end;
    Cells[2, I + 1] := TypeStr;

    // IP
    Cells[3, I + 1] := Device.IPAddress;

    // Batterie
    if Device.BatteryLevel >= 0 then
      Cells[4, I + 1] := Format('%d%%', [Device.BatteryLevel])
    else
      Cells[4, I + 1] := 'N/A';
  end;
end;

function TDeviceStatusGrid.GetStatusColor(Status: TDeviceStatus): TColor;
begin
  case Status of
    dsOnline: Result := $0000C000;     // Vert
    dsOffline: Result := $00808080;    // Gris
    dsError: Result := $000000FF;      // Rouge
    dsMaintenance: Result := $0000FFFF; // Jaune
    else Result := clSilver;
  end;
end;

procedure TDeviceStatusGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
                                     AState: TGridDrawState);
var
  Device: TIoTDevice;
  StatusColor: TColor;
  CircleRect: TRect;
begin
  inherited;

  // Dessiner un cercle coloré dans la colonne État
  if (ACol = 0) and (ARow > 0) and (ARow - 1 < FDevices.Count) then
  begin
    Device := FDevices[ARow - 1];
    StatusColor := GetStatusColor(Device.Status);

    // Dessiner un cercle
    Canvas.Brush.Color := StatusColor;
    Canvas.Pen.Color := StatusColor;

    CircleRect := ARect;
    InflateRect(CircleRect, -15, -8);
    Canvas.Ellipse(CircleRect);
  end;
end;

end.
```

## Tableau de bord complet

### Structure d'un tableau de bord professionnel

```pascal
unit MainDashboard;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, VCLTee.Chart,
  KPIWidget, GaugeWidget, DeviceStatusGrid, RealtimeChart,
  DeviceManager, IoTDevice;

type
  TFormMainDashboard = class(TForm)
    PanelTop: TPanel;
    PanelKPIs: TPanel;
    PanelCharts: TPanel;
    PanelDevices: TPanel;
    PanelAlerts: TPanel;
    TimerUpdate: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
  private
    FDeviceManager: TDeviceManager;

    // KPIs
    FKPITemp: TKPIWidget;
    FKPIHumidity: TKPIWidget;
    FKPIPower: TKPIWidget;

    // Jauges
    FGaugeCPU: TGaugeWidget;
    FGaugeMemory: TGaugeWidget;

    // Graphiques
    FChart: TChart;
    FChartManager: TRealtimeChartManager;

    // Grille de dispositifs
    FDeviceGrid: TDeviceStatusGrid;

    // Alertes
    FMemoAlerts: TMemo;

    procedure CreateWidgets;
    procedure UpdateDashboard;
  public
    property DeviceManager: TDeviceManager read FDeviceManager write FDeviceManager;
  end;

var
  FormMainDashboard: TFormMainDashboard;

implementation

{$R *.dfm}

procedure TFormMainDashboard.FormCreate(Sender: TObject);
begin
  CreateWidgets;
  TimerUpdate.Enabled := True;
end;

procedure TFormMainDashboard.FormDestroy(Sender: TObject);
begin
  FChartManager.Free;
end;

procedure TFormMainDashboard.CreateWidgets;
begin
  // === Zone KPIs (en haut) ===
  PanelKPIs.Height := 140;
  PanelKPIs.Align := alTop;

  FKPITemp := TKPIWidget.Create(Self);
  FKPITemp.Parent := PanelKPIs;
  FKPITemp.Align := alLeft;
  FKPITemp.Width := 220;
  FKPITemp.AlignWithMargins := True;
  FKPITemp.Margins.SetBounds(10, 10, 5, 10);
  FKPITemp.Title := 'Température Moyenne';
  FKPITemp.Unit_ := '°C';

  FKPIHumidity := TKPIWidget.Create(Self);
  FKPIHumidity.Parent := PanelKPIs;
  FKPIHumidity.Align := alLeft;
  FKPIHumidity.Width := 220;
  FKPIHumidity.AlignWithMargins := True;
  FKPIHumidity.Margins.SetBounds(5, 10, 5, 10);
  FKPIHumidity.Title := 'Humidité Moyenne';
  FKPIHumidity.Unit_ := '%';

  FKPIPower := TKPIWidget.Create(Self);
  FKPIPower.Parent := PanelKPIs;
  FKPIPower.Align := alLeft;
  FKPIPower.Width := 220;
  FKPIPower.AlignWithMargins := True;
  FKPIPower.Margins.SetBounds(5, 10, 10, 10);
  FKPIPower.Title := 'Consommation';
  FKPIPower.Unit_ := 'W';

  // === Zone Jauges et Graphiques ===
  PanelCharts.Height := 300;
  PanelCharts.Align := alTop;

  // Jauge CPU
  FGaugeCPU := TGaugeWidget.Create(Self);
  FGaugeCPU.Parent := PanelCharts;
  FGaugeCPU.Align := alLeft;
  FGaugeCPU.Width := 220;
  FGaugeCPU.AlignWithMargins := True;
  FGaugeCPU.Margins.SetBounds(10, 10, 5, 10);
  FGaugeCPU.Title := 'CPU';
  FGaugeCPU.MaxValue := 100;
  FGaugeCPU.WarningThreshold := 70;
  FGaugeCPU.CriticalThreshold := 90;

  // Jauge Mémoire
  FGaugeMemory := TGaugeWidget.Create(Self);
  FGaugeMemory.Parent := PanelCharts;
  FGaugeMemory.Align := alLeft;
  FGaugeMemory.Width := 220;
  FGaugeMemory.AlignWithMargins := True;
  FGaugeMemory.Margins.SetBounds(5, 10, 5, 10);
  FGaugeMemory.Title := 'Mémoire';
  FGaugeMemory.MaxValue := 100;
  FGaugeMemory.WarningThreshold := 75;
  FGaugeMemory.CriticalThreshold := 90;

  // Graphique temps réel
  FChart := TChart.Create(Self);
  FChart.Parent := PanelCharts;
  FChart.Align := alClient;
  FChart.AlignWithMargins := True;
  FChart.Margins.SetBounds(5, 10, 10, 10);

  FChartManager := TRealtimeChartManager.Create(FChart, 100);

  // === Zone Dispositifs ===
  PanelDevices.Height := 250;
  PanelDevices.Align := alTop;

  FDeviceGrid := TDeviceStatusGrid.Create(Self);
  FDeviceGrid.Parent := PanelDevices;
  FDeviceGrid.Align := alClient;
  FDeviceGrid.AlignWithMargins := True;
  FDeviceGrid.Margins.SetBounds(10, 10, 10, 10);

  // === Zone Alertes ===
  PanelAlerts.Align := alClient;

  FMemoAlerts := TMemo.Create(Self);
  FMemoAlerts.Parent := PanelAlerts;
  FMemoAlerts.Align := alClient;
  FMemoAlerts.AlignWithMargins := True;
  FMemoAlerts.Margins.SetBounds(10, 10, 10, 10);
  FMemoAlerts.ScrollBars := ssVertical;
  FMemoAlerts.ReadOnly := True;
  FMemoAlerts.Color := $00FFF5EE; // Beige clair
end;

procedure TFormMainDashboard.UpdateDashboard;
var
  Devices: TArray<TIoTDevice>;
  Device: TIoTDevice;
  TempSum, HumSum: Double;
  TempCount, HumCount: Integer;
  AvgTemp, AvgHum: Double;
  CPUUsage, MemoryUsage: Double;
begin
  if not Assigned(FDeviceManager) then Exit;

  // Récupérer les dispositifs
  Devices := FDeviceManager.GetAllDevices;

  // Calculer les moyennes
  TempSum := 0;
  HumSum := 0;
  TempCount := 0;
  HumCount := 0;

  for Device in Devices do
  begin
    if Device.IsOnline then
    begin
      // Température
      if Device.GetProperty('temperature') <> '' then
      begin
        TempSum := TempSum + StrToFloatDef(Device.GetProperty('temperature'), 0);
        Inc(TempCount);
      end;

      // Humidité
      if Device.GetProperty('humidity') <> '' then
      begin
        HumSum := HumSum + StrToFloatDef(Device.GetProperty('humidity'), 0);
        Inc(HumCount);
      end;
    end;
  end;

  // Mettre à jour les KPIs
  if TempCount > 0 then
  begin
    AvgTemp := TempSum / TempCount;
    FKPITemp.Value := AvgTemp;

    if AvgTemp > 28 then
      FKPITemp.Status := ksCritical
    else if AvgTemp > 25 then
      FKPITemp.Status := ksWarning
    else
      FKPITemp.Status := ksNormal;
  end;

  if HumCount > 0 then
  begin
    AvgHum := HumSum / HumCount;
    FKPIHumidity.Value := AvgHum;

    if (AvgHum > 70) or (AvgHum < 30) then
      FKPIHumidity.Status := ksWarning
    else
      FKPIHumidity.Status := ksNormal;
  end;

  // Simuler consommation électrique
  FKPIPower.Value := 150 + Random(50);
  FKPIPower.Status := ksNormal;

  // Mettre à jour les jauges
  CPUUsage := 30 + Random(40);
  FGaugeCPU.Value := CPUUsage;

  MemoryUsage := 50 + Random(30);
  FGaugeMemory.Value := MemoryUsage;

  // Ajouter un point au graphique
  if TempCount > 0 then
    FChartManager.AddDataPoint(Now, AvgTemp);

  // Mettre à jour la grille de dispositifs
  FDeviceGrid.SetDevices(Devices);
end;

procedure TFormMainDashboard.TimerUpdateTimer(Sender: TObject);
begin
  UpdateDashboard;
end;

end.
```

## Tableaux de bord multi-plateformes avec FireMonkey

Pour créer un tableau de bord qui fonctionne sur Windows, macOS, iOS et Android :

```pascal
unit FMXDashboard;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls, FMX.Forms,
  FMX.Objects, FMX.Layouts, FMX.StdCtrls, FMX.Graphics;

type
  TFormFMXDashboard = class(TForm)
    LayoutMain: TLayout;
    procedure FormCreate(Sender: TObject);
  private
    procedure CreateResponsiveLayout;
    procedure CreateKPICard(Parent: TControl; const Title: string;
                           Value: Double; const Unit_: string);
  end;

implementation

{$R *.fmx}

procedure TFormFMXDashboard.FormCreate(Sender: TObject);
begin
  CreateResponsiveLayout;
end;

procedure TFormFMXDashboard.CreateResponsiveLayout;
var
  ScrollBox: TVertScrollBox;
  Layout: TLayout;
begin
  // ScrollBox principal
  ScrollBox := TVertScrollBox.Create(Self);
  ScrollBox.Parent := Self;
  ScrollBox.Align := TAlignLayout.Client;

  // Layout pour les KPIs
  Layout := TLayout.Create(Self);
  Layout.Parent := ScrollBox;
  Layout.Align := TAlignLayout.Top;
  Layout.Height := 150;

  // Créer 3 KPIs
  CreateKPICard(Layout, 'Température', 22.5, '°C');
  CreateKPICard(Layout, 'Humidité', 65, '%');
  CreateKPICard(Layout, 'Pression', 1013, 'hPa');
end;

procedure TFormFMXDashboard.CreateKPICard(Parent: TControl;
  const Title: string; Value: Double; const Unit_: string);
var
  Card: TRectangle;
  TitleLabel, ValueLabel: TLabel;
begin
  // Carte
  Card := TRectangle.Create(Self);
  Card.Parent := Parent;
  Card.Align := TAlignLayout.Left;
  Card.Width := 200;
  Card.Margins.Rect := RectF(10, 10, 10, 10);
  Card.Fill.Color := $FFF5F5F5;
  Card.Stroke.Color := $FFE0E0E0;
  Card.XRadius := 10;
  Card.YRadius := 10;

  // Titre
  TitleLabel := TLabel.Create(Self);
  TitleLabel.Parent := Card;
  TitleLabel.Align := TAlignLayout.Top;
  TitleLabel.Height := 30;
  TitleLabel.Margins.Rect := RectF(10, 10, 10, 5);
  TitleLabel.Text := Title;
  TitleLabel.TextSettings.FontColor := $FF808080;

  // Valeur
  ValueLabel := TLabel.Create(Self);
  ValueLabel.Parent := Card;
  ValueLabel.Align := TAlignLayout.Client;
  ValueLabel.Margins.Rect := RectF(10, 5, 10, 10);
  ValueLabel.Text := Format('%.1f %s', [Value, Unit_]);
  ValueLabel.TextSettings.Font.Size := 32;
  ValueLabel.TextSettings.FontColor := $FF000000;
  ValueLabel.StyledSettings := [];
end;

end.
```

## Visualisation de données géographiques

### Affichage sur carte

Pour afficher des dispositifs sur une carte :

```pascal
unit MapVisualization;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.Graphics, System.Generics.Collections, IoTDevice;

type
  TMapPoint = record
    Latitude: Double;
    Longitude: Double;
    Device: TIoTDevice;
  end;

  TMapWidget = class(TCustomControl)
  private
    FPoints: TList<TMapPoint>;
    FCenterLat: Double;
    FCenterLon: Double;
    FZoomLevel: Integer;

    function LatLonToPixel(Lat, Lon: Double): TPoint;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddPoint(const Point: TMapPoint);
    procedure Clear;

    property CenterLatitude: Double read FCenterLat write FCenterLat;
    property CenterLongitude: Double read FCenterLon write FCenterLon;
    property ZoomLevel: Integer read FZoomLevel write FZoomLevel;
  end;

implementation

constructor TMapWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPoints := TList<TMapPoint>.Create;
  FCenterLat := 48.8566; // Paris par défaut
  FCenterLon := 2.3522;
  FZoomLevel := 10;
  Width := 800;
  Height := 600;
end;

destructor TMapWidget.Destroy;
begin
  FPoints.Free;
  inherited;
end;

procedure TMapWidget.AddPoint(const Point: TMapPoint);
begin
  FPoints.Add(Point);
  Invalidate;
end;

procedure TMapWidget.Clear;
begin
  FPoints.Clear;
  Invalidate;
end;

function TMapWidget.LatLonToPixel(Lat, Lon: Double): TPoint;
var
  Scale: Double;
begin
  // Projection simplifiée (pour une vraie carte, utiliser une projection Mercator)
  Scale := Power(2, FZoomLevel) * 10;

  Result.X := Round((Lon - FCenterLon) * Scale) + Width div 2;
  Result.Y := Round((FCenterLat - Lat) * Scale) + Height div 2;
end;

procedure TMapWidget.Paint;
var
  Point: TMapPoint;
  PixelPos: TPoint;
  MarkerRect: TRect;
  MarkerColor: TColor;
begin
  inherited;

  // Fond
  Canvas.Brush.Color := $00F0F0F0;
  Canvas.FillRect(ClientRect);

  // Dessiner une grille simple
  Canvas.Pen.Color := $00E0E0E0;
  Canvas.Pen.Width := 1;

  // Lignes verticales
  var X := 0;
  while X < Width do
  begin
    Canvas.MoveTo(X, 0);
    Canvas.LineTo(X, Height);
    X := X + 50;
  end;

  // Lignes horizontales
  var Y := 0;
  while Y < Height do
  begin
    Canvas.MoveTo(0, Y);
    Canvas.LineTo(Width, Y);
    Y := Y + 50;
  end;

  // Dessiner les points
  for Point in FPoints do
  begin
    PixelPos := LatLonToPixel(Point.Latitude, Point.Longitude);

    // Couleur selon le statut
    if Assigned(Point.Device) then
    begin
      case Point.Device.Status of
        dsOnline: MarkerColor := clGreen;
        dsOffline: MarkerColor := clGray;
        dsError: MarkerColor := clRed;
        else MarkerColor := clSilver;
      end;
    end
    else
      MarkerColor := clBlue;

    // Dessiner le marqueur
    MarkerRect := Rect(PixelPos.X - 8, PixelPos.Y - 8,
                       PixelPos.X + 8, PixelPos.Y + 8);

    Canvas.Brush.Color := MarkerColor;
    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 2;
    Canvas.Ellipse(MarkerRect);

    // Nom du dispositif
    if Assigned(Point.Device) then
    begin
      Canvas.Font.Size := 8;
      Canvas.Font.Color := clBlack;
      Canvas.Brush.Style := bsClear;
      Canvas.TextOut(PixelPos.X + 12, PixelPos.Y - 6, Point.Device.Name);
    end;
  end;
end;

end.
```

## Système d'alertes visuelles

### Panel d'alertes avec priorités

```pascal
unit AlertPanel;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Graphics, System.Generics.Collections;

type
  TAlertSeverity = (asInfo, asWarning, asError, asCritical);

  TAlertItem = class
  public
    Severity: TAlertSeverity;
    Message: string;
    Timestamp: TDateTime;
    DeviceName: string;
    Acknowledged: Boolean;
  end;

  TAlertPanel = class(TScrollBox)
  private
    FAlerts: TObjectList<TAlertItem>;
    FMaxAlerts: Integer;

    procedure CreateAlertWidget(Alert: TAlertItem);
    procedure RebuildDisplay;
    function GetSeverityColor(Severity: TAlertSeverity): TColor;
    function GetSeverityText(Severity: TAlertSeverity): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddAlert(Severity: TAlertSeverity; const DeviceName, Message: string);
    procedure Clear;
    procedure AcknowledgeAlert(Alert: TAlertItem);

    property MaxAlerts: Integer read FMaxAlerts write FMaxAlerts;
  end;

implementation

constructor TAlertPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAlerts := TObjectList<TAlertItem>.Create(True);
  FMaxAlerts := 50;
  Color := clWhite;
end;

destructor TAlertPanel.Destroy;
begin
  FAlerts.Free;
  inherited;
end;

procedure TAlertPanel.AddAlert(Severity: TAlertSeverity;
  const DeviceName, Message: string);
var
  Alert: TAlertItem;
begin
  Alert := TAlertItem.Create;
  Alert.Severity := Severity;
  Alert.DeviceName := DeviceName;
  Alert.Message := Message;
  Alert.Timestamp := Now;
  Alert.Acknowledged := False;

  // Ajouter en début de liste (plus récent en premier)
  FAlerts.Insert(0, Alert);

  // Limiter le nombre d'alertes
  while FAlerts.Count > FMaxAlerts do
    FAlerts.Delete(FAlerts.Count - 1);

  RebuildDisplay;
end;

procedure TAlertPanel.RebuildDisplay;
var
  Alert: TAlertItem;
begin
  // Supprimer tous les contrôles existants
  while ControlCount > 0 do
    Controls[0].Free;

  // Recréer les widgets d'alerte
  for Alert in FAlerts do
    CreateAlertWidget(Alert);
end;

function TAlertPanel.GetSeverityColor(Severity: TAlertSeverity): TColor;
begin
  case Severity of
    asInfo: Result := $00E8F5E9;      // Vert très clair
    asWarning: Result := $00E1F5FE;   // Bleu clair
    asError: Result := $00FFE0B2;     // Orange clair
    asCritical: Result := $00FFCDD2;  // Rouge clair
    else Result := clWhite;
  end;
end;

function TAlertPanel.GetSeverityText(Severity: TAlertSeverity): string;
begin
  case Severity of
    asInfo: Result := 'ℹ️ Info';
    asWarning: Result := '⚠️ Attention';
    asError: Result := '❌ Erreur';
    asCritical: Result := '🚨 Critique';
    else Result := '?';
  end;
end;

procedure TAlertPanel.CreateAlertWidget(Alert: TAlertItem);
var
  Panel: TPanel;
  LabelSeverity, LabelDevice, LabelMessage, LabelTime: TLabel;
  ButtonAck: TButton;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 80;
  Panel.AlignWithMargins := True;
  Panel.Margins.SetBounds(5, 5, 5, 0);
  Panel.Color := GetSeverityColor(Alert.Severity);
  Panel.BevelOuter := bvNone;
  Panel.BorderStyle := bsSingle;

  // Sévérité
  LabelSeverity := TLabel.Create(Panel);
  LabelSeverity.Parent := Panel;
  LabelSeverity.Left := 10;
  LabelSeverity.Top := 10;
  LabelSeverity.Caption := GetSeverityText(Alert.Severity);
  LabelSeverity.Font.Style := [fsBold];
  LabelSeverity.Font.Size := 10;

  // Dispositif
  LabelDevice := TLabel.Create(Panel);
  LabelDevice.Parent := Panel;
  LabelDevice.Left := 120;
  LabelDevice.Top := 10;
  LabelDevice.Caption := Alert.DeviceName;
  LabelDevice.Font.Style := [fsBold];

  // Message
  LabelMessage := TLabel.Create(Panel);
  LabelMessage.Parent := Panel;
  LabelMessage.Left := 10;
  LabelMessage.Top := 35;
  LabelMessage.Caption := Alert.Message;
  LabelMessage.Font.Size := 9;

  // Timestamp
  LabelTime := TLabel.Create(Panel);
  LabelTime.Parent := Panel;
  LabelTime.Left := 10;
  LabelTime.Top := 55;
  LabelTime.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', Alert.Timestamp);
  LabelTime.Font.Size := 8;
  LabelTime.Font.Color := clGray;

  // Bouton Acquitter
  if not Alert.Acknowledged then
  begin
    ButtonAck := TButton.Create(Panel);
    ButtonAck.Parent := Panel;
    ButtonAck.Left := Panel.Width - 110;
    ButtonAck.Top := 25;
    ButtonAck.Width := 100;
    ButtonAck.Caption := 'Acquitter';
    ButtonAck.Tag := Integer(Pointer(Alert));
    ButtonAck.OnClick := procedure(Sender: TObject)
    begin
      AcknowledgeAlert(TAlertItem(Pointer((Sender as TButton).Tag)));
    end;
  end;
end;

procedure TAlertPanel.AcknowledgeAlert(Alert: TAlertItem);
begin
  Alert.Acknowledged := True;
  RebuildDisplay;
end;

procedure TAlertPanel.Clear;
begin
  FAlerts.Clear;
  RebuildDisplay;
end;

end.
```

## Export et rapports

### Génération de rapports PDF

```pascal
procedure TFormDashboard.GenerateReport;
var
  Report: TStringList;
  FileName: string;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== RAPPORT TABLEAU DE BORD IoT ===');
    Report.Add('Généré le : ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Now));
    Report.Add('');

    Report.Add('--- INDICATEURS CLÉS ---');
    Report.Add(Format('Température moyenne : %.1f°C', [FKPITemp.Value]));
    Report.Add(Format('Humidité moyenne : %.1f%%', [FKPIHumidity.Value]));
    Report.Add(Format('Consommation : %.1f W', [FKPIPower.Value]));
    Report.Add('');

    Report.Add('--- DISPOSITIFS ---');
    Report.Add(Format('Total : %d', [FDeviceManager.GetDeviceCount]));
    Report.Add(Format('En ligne : %d', [Length(FDeviceManager.GetOnlineDevices)]));
    Report.Add(Format('Hors ligne : %d', [Length(FDeviceManager.GetDevicesByStatus(dsOffline))]));
    Report.Add('');

    Report.Add('--- ALERTES RÉCENTES ---');
    Report.Add(FMemoAlerts.Text);

    FileName := Format('rapport_%s.txt', [FormatDateTime('yyyymmdd_hhnnss', Now)]);
    Report.SaveToFile(FileName);

    ShowMessage('Rapport généré : ' + FileName);
  finally
    Report.Free;
  end;
end;
```

### Export des données en CSV

```pascal
procedure TFormDashboard.ExportToCSV;
var
  CSV: TStringList;
  Devices: TArray<TIoTDevice>;
  Device: TIoTDevice;
  FileName: string;
begin
  CSV := TStringList.Create;
  try
    // En-tête
    CSV.Add('ID,Nom,Type,Status,IP,Batterie,Dernière activité');

    // Données
    Devices := FDeviceManager.GetAllDevices;
    for Device in Devices do
    begin
      CSV.Add(Format('%s,%s,%d,%d,%s,%d,%s',
        [Device.ID,
         Device.Name,
         Ord(Device.DeviceType),
         Ord(Device.Status),
         Device.IPAddress,
         Device.BatteryLevel,
         FormatDateTime('yyyy-mm-dd hh:nn:ss', Device.LastSeen)]));
    end;

    FileName := Format('devices_%s.csv', [FormatDateTime('yyyymmdd_hhnnss', Now)]);
    CSV.SaveToFile(FileName);

    ShowMessage('Données exportées : ' + FileName);
  finally
    CSV.Free;
  end;
end;
```

## Responsive design

### Adaptation à différentes tailles d'écran

```pascal
procedure TFormDashboard.AdaptLayout;
var
  IsSmallScreen: Boolean;
begin
  IsSmallScreen := Width < 1024;

  if IsSmallScreen then
  begin
    // Mode compact
    PanelCharts.Height := 250;
    PanelDevices.Height := 200;

    // Masquer certains éléments détaillés
    FGaugeMemory.Visible := False;
  end
  else
  begin
    // Mode normal
    PanelCharts.Height := 300;
    PanelDevices.Height := 250;

    FGaugeMemory.Visible := True;
  end;
end;

procedure TFormDashboard.FormResize(Sender: TObject);
begin
  AdaptLayout;
end;
```

## Thèmes et personnalisation

### Thèmes sombre et clair

```pascal
type
  TDashboardTheme = (dtLight, dtDark);

procedure TFormDashboard.ApplyTheme(Theme: TDashboardTheme);
begin
  case Theme of
    dtLight:
      begin
        Self.Color := clWhite;
        PanelKPIs.Color := $00F5F5F5;
        PanelCharts.Color := clWhite;
        PanelDevices.Color := $00F5F5F5;
        PanelAlerts.Color := clWhite;
      end;

    dtDark:
      begin
        Self.Color := $00202020;
        PanelKPIs.Color := $00303030;
        PanelCharts.Color := $00252525;
        PanelDevices.Color := $00303030;
        PanelAlerts.Color := $00252525;

        // Adapter les couleurs de texte
        FKPITemp.Font.Color := clWhite;
        FKPIHumidity.Font.Color := clWhite;
        FKPIPower.Font.Color := clWhite;
      end;
  end;
end;
```

## Performance et optimisation

### Mise à jour sélective

```pascal
procedure TFormDashboard.UpdateDashboardOptimized;
var
  UpdateInterval: Integer;
begin
  Inc(FUpdateCounter);

  // KPIs : mise à jour à chaque tick (1 seconde)
  UpdateKPIs;

  // Graphiques : mise à jour toutes les 5 secondes
  if (FUpdateCounter mod 5) = 0 then
    UpdateCharts;

  // Grille de dispositifs : mise à jour toutes les 10 secondes
  if (FUpdateCounter mod 10) = 0 then
    UpdateDeviceGrid;

  // Réinitialiser le compteur toutes les minutes
  if FUpdateCounter >= 60 then
    FUpdateCounter := 0;
end;
```

### Double buffering pour éviter le scintillement

```pascal
constructor TCustomWidget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DoubleBuffered := True; // Évite le scintillement
end;
```

## Conclusion

Un tableau de bord IoT efficace est essentiel pour superviser et gérer vos systèmes connectés. Avec Delphi, vous avez tous les outils pour créer des interfaces professionnelles, réactives et esthétiques.

**Points clés à retenir :**

1. **Clarté** : afficher l'information essentielle en premier
2. **Hiérarchie visuelle** : utiliser taille, couleur et position pour guider l'œil
3. **Couleurs significatives** : vert/orange/rouge pour le statut
4. **KPI** : indicateurs clés immédiatement visibles
5. **Graphiques temps réel** : pour les tendances et l'historique
6. **Interactivité** : permettre l'exploration et le drill-down
7. **Alertes visuelles** : impossibles à manquer
8. **Performance** : mise à jour fluide sans ralentissement
9. **Responsive** : adaptation aux différentes tailles d'écran
10. **Export** : permettre la génération de rapports

Un bon tableau de bord transforme des données brutes en intelligence exploitable, permettant une prise de décision rapide et éclairée. C'est l'interface qui fait la différence entre un système IoT fonctionnel et un système IoT véritablement utile.

Vous avez maintenant toutes les connaissances pour créer des solutions IoT complètes avec Delphi, de la collecte des données sur les capteurs jusqu'à leur visualisation dans des tableaux de bord professionnels !

⏭️ [Intelligence Artificielle et Machine Learning avec Delphi](/22-intelligence-artificielle-et-machine-learning-avec-delphi/README.md)
