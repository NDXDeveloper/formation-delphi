# 21.8 Tableaux de bord pour solutions IoT

Une solution IoT n'est complète que lorsque les données collectées sont présentées de manière claire, intuitive et exploitable. Les tableaux de bord constituent l'interface utilisateur qui transforme des données brutes en informations visuelles permettant de prendre des décisions. Cette section vous guidera pas à pas dans la création de tableaux de bord professionnels pour vos solutions IoT avec Delphi.

## Introduction aux tableaux de bord IoT

Un tableau de bord IoT est une interface visuelle qui présente les données collectées par vos dispositifs connectés. Contrairement à un simple affichage de données, un bon tableau de bord doit :

- **Organiser** les informations de manière logique et hiérarchique
- **Visualiser** les données de façon à faire ressortir les tendances et anomalies
- **Alerter** l'utilisateur en cas de situation nécessitant une attention
- **Interagir** avec les dispositifs pour prendre des actions
- **Personnaliser** l'affichage selon les préférences de l'utilisateur

![Exemple de tableau de bord IoT](https://via.placeholder.com/800x450.png?text=Exemple+de+tableau+de+bord+IoT)

## Architecture d'un tableau de bord IoT

Avant de commencer à coder, il est important de comprendre l'architecture générale d'un tableau de bord IoT efficace :

1. **Couche de données** : Connexion aux sources de données (dispositifs, base de données)
2. **Couche de traitement** : Transformation, filtrage et analyse des données
3. **Couche de présentation** : Composants visuels et mise en page
4. **Couche d'interaction** : Contrôles et éléments interactifs
5. **Couche de configuration** : Personnalisation et préférences utilisateur

## Conception de l'interface utilisateur

Commençons par la conception d'une interface de base pour notre tableau de bord. Nous allons utiliser une approche modulaire avec des cadres (frames) pour faciliter la réutilisation des composants.

### Structure de base du tableau de bord

Créez un nouveau projet VCL et concevez le formulaire principal :

```pascal
unit MainDashboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.Buttons, System.ImageList, Vcl.ImgList, Vcl.Imaging.pngimage,
  // Nos unités personnalisées
  DashboardWidgets, IoTDataConnection, IoTDataTypes;

type
  TfrmMainDashboard = class(TForm)
    pnlSidebar: TPanel;
    pnlContent: TPanel;
    pnlHeader: TPanel;
    sbtnDashboard: TSpeedButton;
    sbtnDevices: TSpeedButton;
    sbtnAlerts: TSpeedButton;
    sbtnSettings: TSpeedButton;
    imgLogo: TImage;
    lblTitle: TLabel;
    pnlUser: TPanel;
    imgUser: TImage;
    lblUsername: TLabel;
    pnlContentHeader: TPanel;
    lblPageTitle: TLabel;
    lblPageDescription: TLabel;
    pnlDashboardContainer: TPanel;
    scrDashboard: TScrollBox;
    statusBar: TStatusBar;
    tmrRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure sbtnDashboardClick(Sender: TObject);
    procedure sbtnDevicesClick(Sender: TObject);
    procedure sbtnAlertsClick(Sender: TObject);
    procedure sbtnSettingsClick(Sender: TObject);
    procedure tmrRefreshTimer(Sender: TObject);
  private
    FDataConnection: TIoTDataConnection;
    FWidgets: TList<TDashboardWidgetBase>;
    FCurrentView: string;

    procedure LoadDashboardConfiguration;
    procedure SaveDashboardConfiguration;
    procedure CreateDashboardWidgets;
    procedure ClearDashboardWidgets;
    procedure SwitchView(const ViewName: string);
    procedure UpdateWidgets;
    procedure HandleNewData(Sender: TObject; const Data: TIoTData);
    procedure HandleAlert(Sender: TObject; const AlertType, AlertMessage: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMainDashboard: TfrmMainDashboard;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

### Création de widgets réutilisables

Pour rendre notre tableau de bord flexible, nous allons créer des widgets modulaires. Un widget est un composant visuel auto-contenu qui affiche un certain type de données.

```pascal
unit DashboardWidgets;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart,
  IoTDataTypes;

type
  // Classe de base pour tous les widgets
  TDashboardWidgetBase = class(TFrame)
    pnlHeader: TPanel;
    lblTitle: TLabel;
    btnSettings: TButton;
    btnClose: TButton;
    procedure btnSettingsClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    FTitle: string;
    FIsConfigurable: Boolean;
    FOnClose: TNotifyEvent;
    FOnSettings: TNotifyEvent;

    procedure SetTitle(const Value: string);
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;

    // Méthode virtuelle pour mettre à jour les données
    procedure UpdateData(const Data: TIoTData); virtual;

    // Méthode virtuelle pour configurer le widget
    procedure Configure; virtual;

    // Méthode virtuelle pour sauvegarder la configuration
    function SaveConfiguration: string; virtual;

    // Méthode virtuelle pour charger la configuration
    procedure LoadConfiguration(const Config: string); virtual;

    property Title: string read FTitle write SetTitle;
    property IsConfigurable: Boolean read FIsConfigurable write FIsConfigurable;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnSettings: TNotifyEvent read FOnSettings write FOnSettings;
  end;

  // Widget pour afficher une valeur simple
  TValueWidgetFrame = class(TDashboardWidgetBase)
    lblValue: TLabel;
    lblUnit: TLabel;
    shpIndicator: TShape;
  private
    FSensorID: string;
    FUnit: string;
    FDisplayFormat: string;
    FThresholdLow: Double;
    FThresholdHigh: Double;
    FLastValue: Double;

    procedure SetSensorID(const Value: string);
    procedure SetUnit(const Value: string);
    procedure SetDisplayFormat(const Value: string);
    procedure SetThresholdLow(const Value: Double);
    procedure SetThresholdHigh(const Value: Double);
    procedure UpdateDisplay;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateData(const Data: TIoTData); override;
    procedure Configure; override;
    function SaveConfiguration: string; override;
    procedure LoadConfiguration(const Config: string); override;

    property SensorID: string read FSensorID write SetSensorID;
    property ValueUnit: string read FUnit write SetUnit;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property ThresholdLow: Double read FThresholdLow write SetThresholdLow;
    property ThresholdHigh: Double read FThresholdHigh write SetThresholdHigh;
  end;

  // Widget pour afficher un graphique linéaire
  TChartWidgetFrame = class(TDashboardWidgetBase)
    Chart: TChart;
    Series1: TFastLineSeries;
  private
    FSensorID: string;
    FTimeRange: Integer;  // Minutes
    FMaxPoints: Integer;
    FAutoScale: Boolean;
    FYAxisMin: Double;
    FYAxisMax: Double;
    FDisplayUnit: string;

    procedure SetSensorID(const Value: string);
    procedure SetTimeRange(const Value: Integer);
    procedure SetMaxPoints(const Value: Integer);
    procedure SetAutoScale(const Value: Boolean);
    procedure SetYAxisRange(Min, Max: Double);
    procedure SetDisplayUnit(const Value: string);
    procedure UpdateChart;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateData(const Data: TIoTData); override;
    procedure Configure; override;
    function SaveConfiguration: string; override;
    procedure LoadConfiguration(const Config: string); override;
    procedure ClearData;

    property SensorID: string read FSensorID write SetSensorID;
    property TimeRange: Integer read FTimeRange write SetTimeRange;
    property MaxPoints: Integer read FMaxPoints write SetMaxPoints;
    property AutoScale: Boolean read FAutoScale write SetAutoScale;
    property DisplayUnit: string read FDisplayUnit write SetDisplayUnit;
  end;

  // Widget pour afficher une jauge
  TGaugeWidgetFrame = class(TDashboardWidgetBase)
    Chart: TChart;
    Series1: TPieSeries;
    lblValue: TLabel;
  private
    FSensorID: string;
    FMinValue: Double;
    FMaxValue: Double;
    FCurrentValue: Double;
    FDisplayUnit: string;
    FDisplayFormat: string;
    FThresholdLow: Double;
    FThresholdHigh: Double;

    procedure SetSensorID(const Value: string);
    procedure SetMinValue(const Value: Double);
    procedure SetMaxValue(const Value: Double);
    procedure SetCurrentValue(const Value: Double);
    procedure SetDisplayUnit(const Value: string);
    procedure SetDisplayFormat(const Value: string);
    procedure SetThresholdLow(const Value: Double);
    procedure SetThresholdHigh(const Value: Double);
    procedure UpdateGauge;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateData(const Data: TIoTData); override;
    procedure Configure; override;
    function SaveConfiguration: string; override;
    procedure LoadConfiguration(const Config: string); override;

    property SensorID: string read FSensorID write SetSensorID;
    property MinValue: Double read FMinValue write SetMinValue;
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property DisplayUnit: string read FDisplayUnit write SetDisplayUnit;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property ThresholdLow: Double read FThresholdLow write SetThresholdLow;
    property ThresholdHigh: Double read FThresholdHigh write SetThresholdHigh;
  end;

  // Widget pour afficher l'état d'un dispositif (On/Off)
  TDeviceStatusWidgetFrame = class(TDashboardWidgetBase)
    shpStatus: TShape;
    lblStatusText: TLabel;
    btnToggle: TButton;
    procedure btnToggleClick(Sender: TObject);
  private
    FDeviceID: string;
    FPropertyName: string;
    FIsOnline: Boolean;
    FIsOn: Boolean;
    FOnToggleRequest: TNotifyEvent;

    procedure SetDeviceID(const Value: string);
    procedure SetPropertyName(const Value: string);
    procedure SetIsOnline(const Value: Boolean);
    procedure SetIsOn(const Value: Boolean);
    procedure UpdateDisplay;
  public
    constructor Create(AOwner: TComponent); override;

    procedure UpdateData(const Data: TIoTData); override;
    procedure Configure; override;
    function SaveConfiguration: string; override;
    procedure LoadConfiguration(const Config: string); override;

    property DeviceID: string read FDeviceID write SetDeviceID;
    property PropertyName: string read FPropertyName write SetPropertyName;
    property IsOnline: Boolean read FIsOnline write SetIsOnline;
    property IsOn: Boolean read FIsOn write SetIsOn;
    property OnToggleRequest: TNotifyEvent read FOnToggleRequest write FOnToggleRequest;
  end;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end;
```

## Implémentation des widgets

Maintenant, implémentons les fonctionnalités clés de quelques-uns de nos widgets. Commençons par le widget d'affichage de valeur simple :

```pascal
constructor TValueWidgetFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDisplayFormat := '%.1f';  // Format par défaut
  FThresholdLow := -1E20;    // Valeur très basse par défaut
  FThresholdHigh := 1E20;    // Valeur très haute par défaut
  FLastValue := 0;

  // Configuration visuelle
  Width := 200;
  Height := 120;
  lblValue.Font.Size := 24;
  lblValue.Font.Style := [fsBold];
  shpIndicator.Brush.Color := clGray;  // Indicateur gris par défaut
end;

procedure TValueWidgetFrame.UpdateData(const Data: TIoTData);
begin
  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    FLastValue := Data.NumericValue;
    UpdateDisplay;
  end;
end;

procedure TValueWidgetFrame.UpdateDisplay;
begin
  // Mettre à jour la valeur affichée
  lblValue.Caption := Format(FDisplayFormat, [FLastValue]);
  lblUnit.Caption := FUnit;

  // Mettre à jour l'indicateur de couleur selon les seuils
  if FLastValue < FThresholdLow then
    shpIndicator.Brush.Color := clBlue
  else if FLastValue > FThresholdHigh then
    shpIndicator.Brush.Color := clRed
  else
    shpIndicator.Brush.Color := clGreen;
end;

procedure TValueWidgetFrame.Configure;
var
  ConfigForm: TForm;
  edtSensorID, edtUnit, edtFormat: TEdit;
  edtThresholdLow, edtThresholdHigh: TEdit;
  lblSensorID, lblUnit, lblFormat, lblThresholdLow, lblThresholdHigh: TLabel;
  btnOK, btnCancel: TButton;
begin
  // Créer un formulaire de configuration simple
  ConfigForm := TForm.Create(Application);
  try
    ConfigForm.Caption := 'Configuration du widget';
    ConfigForm.Position := poOwnerFormCenter;
    ConfigForm.BorderStyle := bsDialog;
    ConfigForm.Width := 350;
    ConfigForm.Height := 250;

    // Créer les contrôles
    lblSensorID := TLabel.Create(ConfigForm);
    lblSensorID.Parent := ConfigForm;
    lblSensorID.Left := 20;
    lblSensorID.Top := 20;
    lblSensorID.Caption := 'ID du capteur:';

    edtSensorID := TEdit.Create(ConfigForm);
    edtSensorID.Parent := ConfigForm;
    edtSensorID.Left := 150;
    edtSensorID.Top := 20;
    edtSensorID.Width := 180;
    edtSensorID.Text := FSensorID;

    lblUnit := TLabel.Create(ConfigForm);
    lblUnit.Parent := ConfigForm;
    lblUnit.Left := 20;
    lblUnit.Top := 50;
    lblUnit.Caption := 'Unité:';

    edtUnit := TEdit.Create(ConfigForm);
    edtUnit.Parent := ConfigForm;
    edtUnit.Left := 150;
    edtUnit.Top := 50;
    edtUnit.Width := 180;
    edtUnit.Text := FUnit;

    lblFormat := TLabel.Create(ConfigForm);
    lblFormat.Parent := ConfigForm;
    lblFormat.Left := 20;
    lblFormat.Top := 80;
    lblFormat.Caption := 'Format d''affichage:';

    edtFormat := TEdit.Create(ConfigForm);
    edtFormat.Parent := ConfigForm;
    edtFormat.Left := 150;
    edtFormat.Top := 80;
    edtFormat.Width := 180;
    edtFormat.Text := FDisplayFormat;

    lblThresholdLow := TLabel.Create(ConfigForm);
    lblThresholdLow.Parent := ConfigForm;
    lblThresholdLow.Left := 20;
    lblThresholdLow.Top := 110;
    lblThresholdLow.Caption := 'Seuil bas:';

    edtThresholdLow := TEdit.Create(ConfigForm);
    edtThresholdLow.Parent := ConfigForm;
    edtThresholdLow.Left := 150;
    edtThresholdLow.Top := 110;
    edtThresholdLow.Width := 180;
    edtThresholdLow.Text := FloatToStr(FThresholdLow);

    lblThresholdHigh := TLabel.Create(ConfigForm);
    lblThresholdHigh.Parent := ConfigForm;
    lblThresholdHigh.Left := 20;
    lblThresholdHigh.Top := 140;
    lblThresholdHigh.Caption := 'Seuil haut:';

    edtThresholdHigh := TEdit.Create(ConfigForm);
    edtThresholdHigh.Parent := ConfigForm;
    edtThresholdHigh.Left := 150;
    edtThresholdHigh.Top := 140;
    edtThresholdHigh.Width := 180;
    edtThresholdHigh.Text := FloatToStr(FThresholdHigh);

    btnOK := TButton.Create(ConfigForm);
    btnOK.Parent := ConfigForm;
    btnOK.Caption := 'OK';
    btnOK.ModalResult := mrOk;
    btnOK.Default := True;
    btnOK.Left := ConfigForm.Width - 170;
    btnOK.Top := ConfigForm.Height - 70;
    btnOK.Width := 75;

    btnCancel := TButton.Create(ConfigForm);
    btnCancel.Parent := ConfigForm;
    btnCancel.Caption := 'Annuler';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := ConfigForm.Width - 85;
    btnCancel.Top := ConfigForm.Height - 70;
    btnCancel.Width := 75;

    // Afficher le formulaire
    if ConfigForm.ShowModal = mrOk then
    begin
      // Appliquer la configuration
      SensorID := edtSensorID.Text;
      ValueUnit := edtUnit.Text;
      DisplayFormat := edtFormat.Text;

      try
        ThresholdLow := StrToFloat(edtThresholdLow.Text);
        ThresholdHigh := StrToFloat(edtThresholdHigh.Text);
      except
        on E: Exception do
          ShowMessage('Erreur de conversion: ' + E.Message);
      end;

      // Mettre à jour l'affichage
      UpdateDisplay;
    end;
  finally
    ConfigForm.Free;
  end;
end;

function TValueWidgetFrame.SaveConfiguration: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('type', 'value');
    JSONObj.AddPair('sensorID', FSensorID);
    JSONObj.AddPair('unit', FUnit);
    JSONObj.AddPair('format', FDisplayFormat);
    JSONObj.AddPair('thresholdLow', TJSONNumber.Create(FThresholdLow));
    JSONObj.AddPair('thresholdHigh', TJSONNumber.Create(FThresholdHigh));

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TValueWidgetFrame.LoadConfiguration(const Config: string);
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(Config) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    if JSONObj.TryGetValue<string>('sensorID', FSensorID) then ;
    if JSONObj.TryGetValue<string>('unit', FUnit) then ;
    if JSONObj.TryGetValue<string>('format', FDisplayFormat) then ;
    if JSONObj.TryGetValue<Double>('thresholdLow', FThresholdLow) then ;
    if JSONObj.TryGetValue<Double>('thresholdHigh', FThresholdHigh) then ;

    UpdateDisplay;
  finally
    JSONObj.Free;
  end;
end;
```

Maintenant, implémentons le widget de graphique pour visualiser les tendances :

```pascal
constructor TChartWidgetFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimeRange := 60;    // 60 minutes par défaut
  FMaxPoints := 100;   // 100 points maximum
  FAutoScale := True;
  FYAxisMin := 0;
  FYAxisMax := 100;

  // Configuration visuelle
  Width := 400;
  Height := 250;

  // Configuration du graphique
  Chart.Title.Text.Text := 'Tendance';
  Chart.Legend.Visible := False;
  Chart.BottomAxis.Title.Caption := 'Temps';
  Chart.LeftAxis.Title.Caption := 'Valeur';
  Chart.View3D := False;

  Series1.Title := 'Données';
  Series1.LinePen.Width := 2;
  Series1.LinePen.Color := clBlue;

  UpdateChart;
end;

procedure TChartWidgetFrame.UpdateData(const Data: TIoTData);
begin
  // Vérifier si c'est le capteur qui nous intéresse
  if (Data.SensorID = FSensorID) and (Data.DataType = dtNumeric) then
  begin
    // Ajouter le point au graphique
    Series1.AddXY(Data.Timestamp, Data.NumericValue);

    // Limiter le nombre de points
    if Series1.Count > FMaxPoints then
      Series1.Delete(0);

    UpdateChart;
  end;
end;

procedure TChartWidgetFrame.UpdateChart;
var
  MinTime, MaxTime: TDateTime;
begin
  // Configurer l'axe X (temps)
  if Series1.Count > 0 then
  begin
    // Définir la plage de temps
    MaxTime := Now;
    MinTime := MaxTime - FTimeRange / (24 * 60); // Convertir minutes en jours

    Chart.BottomAxis.Automatic := False;
    Chart.BottomAxis.SetMinMax(MinTime, MaxTime);

    // Format de date/heure
    Chart.BottomAxis.DateTimeFormat := 'hh:nn';
  end;

  // Configurer l'axe Y
  if not FAutoScale then
  begin
    Chart.LeftAxis.Automatic := False;
    Chart.LeftAxis.SetMinMax(FYAxisMin, FYAxisMax);
  end
  else
    Chart.LeftAxis.Automatic := True;

  // Configurer le titre et l'unité
  Chart.LeftAxis.Title.Caption := 'Valeur' + IfThen(FDisplayUnit <> '', ' (' + FDisplayUnit + ')', '');

  // Mettre à jour le titre du graphique
  Chart.Title.Text.Text := Title + IfThen(FSensorID <> '', ' - ' + FSensorID, '');
end;

procedure TChartWidgetFrame.Configure;
var
  ConfigForm: TForm;
  edtSensorID, edtTimeRange, edtMaxPoints, edtYAxisMin, edtYAxisMax, edtUnit: TEdit;
  lblSensorID, lblTimeRange, lblMaxPoints, lblYAxisMin, lblYAxisMax, lblUnit: TLabel;
  chkAutoScale: TCheckBox;
  btnOK, btnCancel: TButton;
begin
  // Créer un formulaire de configuration
  ConfigForm := TForm.Create(Application);
  try
    ConfigForm.Caption := 'Configuration du graphique';
    ConfigForm.Position := poOwnerFormCenter;
    ConfigForm.BorderStyle := bsDialog;
    ConfigForm.Width := 350;
    ConfigForm.Height := 300;

    // Créer les contrôles
    lblSensorID := TLabel.Create(ConfigForm);
    lblSensorID.Parent := ConfigForm;
    lblSensorID.Left := 20;
    lblSensorID.Top := 20;
    lblSensorID.Caption := 'ID du capteur:';

    edtSensorID := TEdit.Create(ConfigForm);
    edtSensorID.Parent := ConfigForm;
    edtSensorID.Left := 150;
    edtSensorID.Top := 20;
    edtSensorID.Width := 180;
    edtSensorID.Text := FSensorID;

    lblTimeRange := TLabel.Create(ConfigForm);
    lblTimeRange.Parent := ConfigForm;
    lblTimeRange.Left := 20;
    lblTimeRange.Top := 50;
    lblTimeRange.Caption := 'Plage de temps (min):';

    edtTimeRange := TEdit.Create(ConfigForm);
    edtTimeRange.Parent := ConfigForm;
    edtTimeRange.Left := 150;
    edtTimeRange.Top := 50;
    edtTimeRange.Width := 180;
    edtTimeRange.Text := IntToStr(FTimeRange);

    lblMaxPoints := TLabel.Create(ConfigForm);
    lblMaxPoints.Parent := ConfigForm;
    lblMaxPoints.Left := 20;
    lblMaxPoints.Top := 80;
    lblMaxPoints.Caption := 'Points maximum:';

    edtMaxPoints := TEdit.Create(ConfigForm);
    edtMaxPoints.Parent := ConfigForm;
    edtMaxPoints.Left := 150;
    edtMaxPoints.Top := 80;
    edtMaxPoints.Width := 180;
    edtMaxPoints.Text := IntToStr(FMaxPoints);

    chkAutoScale := TCheckBox.Create(ConfigForm);
    chkAutoScale.Parent := ConfigForm;
    chkAutoScale.Left := 20;
    chkAutoScale.Top := 110;
    chkAutoScale.Width := 200;
    chkAutoScale.Caption := 'Échelle automatique';
    chkAutoScale.Checked := FAutoScale;

    lblYAxisMin := TLabel.Create(ConfigForm);
    lblYAxisMin.Parent := ConfigForm;
    lblYAxisMin.Left := 20;
    lblYAxisMin.Top := 140;
    lblYAxisMin.Caption := 'Valeur Y minimum:';

    edtYAxisMin := TEdit.Create(ConfigForm);
    edtYAxisMin.Parent := ConfigForm;
    edtYAxisMin.Left := 150;
    edtYAxisMin.Top := 140;
    edtYAxisMin.Width := 180;
    edtYAxisMin.Text := FloatToStr(FYAxisMin);
    edtYAxisMin.Enabled := not FAutoScale;

    lblYAxisMax := TLabel.Create(ConfigForm);
    lblYAxisMax.Parent := ConfigForm;
    lblYAxisMax.Left := 20;
    lblYAxisMax.Top := 170;
    lblYAxisMax.Caption := 'Valeur Y maximum:';

    edtYAxisMax := TEdit.Create(ConfigForm);
    edtYAxisMax.Parent := ConfigForm;
    edtYAxisMax.Left := 150;
    edtYAxisMax.Top := 170;
    edtYAxisMax.Width := 180;
    edtYAxisMax.Text := FloatToStr(FYAxisMax);
    edtYAxisMax.Enabled := not FAutoScale;

    lblUnit := TLabel.Create(ConfigForm);
    lblUnit.Parent := ConfigForm;
    lblUnit.Left := 20;
    lblUnit.Top := 200;
    lblUnit.Caption := 'Unité:';

    edtUnit := TEdit.Create(ConfigForm);
    edtUnit.Parent := ConfigForm;
    edtUnit.Left := 150;
    edtUnit.Top := 200;
    edtUnit.Width := 180;
    edtUnit.Text := FDisplayUnit;

    // Ajouter une gestion d'événement pour activer/désactiver les champs selon l'échelle auto
    chkAutoScale.OnClick := procedure(Sender: TObject)
    begin
      edtYAxisMin.Enabled := not chkAutoScale.Checked;
      edtYAxisMax.Enabled := not chkAutoScale.Checked;
    end;

    btnOK := TButton.Create(ConfigForm);
    btnOK.Parent := ConfigForm;
    btnOK.Caption := 'OK';
    btnOK.ModalResult := mrOk;
    btnOK.Default := True;
    btnOK.Left := ConfigForm.Width - 170;
    btnOK.Top := ConfigForm.Height - 70;
    btnOK.Width := 75;

    btnCancel := TButton.Create(ConfigForm);
    btnCancel.Parent := ConfigForm);
    btnCancel.Caption := 'Annuler';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := ConfigForm.Width - 85;
    btnCancel.Top := ConfigForm.Height - 70;
    btnCancel.Width := 75;

    // Afficher le formulaire
    if ConfigForm.ShowModal = mrOk then
    begin
      // Appliquer la configuration
      SensorID := edtSensorID.Text;

      try
        TimeRange := StrToInt(edtTimeRange.Text);
        MaxPoints := StrToInt(edtMaxPoints.Text);

        AutoScale := chkAutoScale.Checked;

        if not AutoScale then
        begin
          FYAxisMin := StrToFloat(edtYAxisMin.Text);
          FYAxisMax := StrToFloat(edtYAxisMax.Text);
        end;

        DisplayUnit := edtUnit.Text;

        // Mettre à jour le graphique
        UpdateChart;
      except
        on E: Exception do
          ShowMessage('Erreur de conversion: ' + E.Message);
      end;
    end;
  finally
    ConfigForm.Free;
  end;
end;

function TChartWidgetFrame.SaveConfiguration: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('type', 'chart');
    JSONObj.AddPair('sensorID', FSensorID);
    JSONObj.AddPair('timeRange', TJSONNumber.Create(FTimeRange));
    JSONObj.AddPair('maxPoints', TJSONNumber.Create(FMaxPoints));
    JSONObj.AddPair('autoScale', TJSONBool.Create(FAutoScale));
    JSONObj.AddPair('yAxisMin', TJSONNumber.Create(FYAxisMin));
    JSONObj.AddPair('yAxisMax', TJSONNumber.Create(FYAxisMax));
    JSONObj.AddPair('displayUnit', FDisplayUnit);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TChartWidgetFrame.LoadConfiguration(const Config: string);
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(Config) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    if JSONObj.TryGetValue<string>('sensorID', FSensorID) then ;
    if JSONObj.TryGetValue<Integer>('timeRange', FTimeRange) then ;
    if JSONObj.TryGetValue<Integer>('maxPoints', FMaxPoints) then ;
    if JSONObj.TryGetValue<Boolean>('autoScale', FAutoScale) then ;
    if JSONObj.TryGetValue<Double>('yAxisMin', FYAxisMin) then ;
    if JSONObj.TryGetValue<Double>('yAxisMax', FYAxisMax) then ;
    if JSONObj.TryGetValue<string>('displayUnit', FDisplayUnit) then ;

    UpdateChart;
  finally
    JSONObj.Free;
  end;
end;

procedure TChartWidgetFrame.ClearData;
begin
  Series1.Clear;
  UpdateChart;
end;
```

Enfin, implémentons le widget d'état d'un dispositif qui permet également le contrôle :

```pascal
constructor TDeviceStatusWidgetFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsOnline := False;
  FIsOn := False;

  // Configuration visuelle
  Width := 200;
  Height := 120;
  shpStatus.Brush.Color := clGray;
  btnToggle.Enabled := False;

  // Configuration du bouton de basculement
  btnToggle.Caption := 'Basculer';

  UpdateDisplay;
end;

procedure TDeviceStatusWidgetFrame.UpdateData(const Data: TIoTData);
begin
  // Vérifier si c'est le dispositif qui nous intéresse
  if (Data.DeviceID = FDeviceID) then
  begin
    // Mettre à jour l'état en ligne
    FIsOnline := (Data.DataType <> dtUnknown);

    // Mettre à jour l'état on/off si la donnée correspond à la propriété
    if (Data.SensorID = FPropertyName) and (Data.DataType = dtBoolean) then
      FIsOn := Data.BooleanValue;

    UpdateDisplay;
  end;
end;

procedure TDeviceStatusWidgetFrame.UpdateDisplay;
begin
  // Mettre à jour l'affichage selon l'état
  btnToggle.Enabled := FIsOnline;

  if not FIsOnline then
  begin
    // Dispositif hors ligne
    shpStatus.Brush.Color := clGray;
    lblStatusText.Caption := 'Hors ligne';
    btnToggle.Caption := 'Basculer';
  end
  else if FIsOn then
  begin
    // Dispositif allumé
    shpStatus.Brush.Color := clGreen;
    lblStatusText.Caption := 'Allumé';
    btnToggle.Caption := 'Éteindre';
  end
  else
  begin
    // Dispositif éteint
    shpStatus.Brush.Color := clRed;
    lblStatusText.Caption := 'Éteint';
    btnToggle.Caption := 'Allumer';
  end;
end;

procedure TDeviceStatusWidgetFrame.btnToggleClick(Sender: TObject);
begin
  // Changer l'état localement
  if FIsOnline then
    FIsOn := not FIsOn;

  // Mettre à jour l'affichage
  UpdateDisplay;

  // Déclencher l'événement pour notifier le changement
  if Assigned(FOnToggleRequest) then
    FOnToggleRequest(Self);
end;

procedure TDeviceStatusWidgetFrame.Configure;
var
  ConfigForm: TForm;
  edtDeviceID, edtPropertyName: TEdit;
  lblDeviceID, lblPropertyName: TLabel;
  btnOK, btnCancel: TButton;
begin
  // Créer un formulaire de configuration
  ConfigForm := TForm.Create(Application);
  try
    ConfigForm.Caption := 'Configuration du widget d''état';
    ConfigForm.Position := poOwnerFormCenter;
    ConfigForm.BorderStyle := bsDialog;
    ConfigForm.Width := 350;
    ConfigForm.Height := 150;

    // Créer les contrôles
    lblDeviceID := TLabel.Create(ConfigForm);
    lblDeviceID.Parent := ConfigForm;
    lblDeviceID.Left := 20;
    lblDeviceID.Top := 20;
    lblDeviceID.Caption := 'ID du dispositif:';

    edtDeviceID := TEdit.Create(ConfigForm);
    edtDeviceID.Parent := ConfigForm;
    edtDeviceID.Left := 150;
    edtDeviceID.Top := 20;
    edtDeviceID.Width := 180;
    edtDeviceID.Text := FDeviceID;

    lblPropertyName := TLabel.Create(ConfigForm);
    lblPropertyName.Parent := ConfigForm;
    lblPropertyName.Left := 20;
    lblPropertyName.Top := 50;
    lblPropertyName.Caption := 'Nom de la propriété:';

    edtPropertyName := TEdit.Create(ConfigForm);
    edtPropertyName.Parent := ConfigForm);
    edtPropertyName.Left := 150;
    edtPropertyName.Top := 50;
    edtPropertyName.Width := 180;
    edtPropertyName.Text := FPropertyName;

    btnOK := TButton.Create(ConfigForm);
    btnOK.Parent := ConfigForm;
    btnOK.Caption := 'OK';
    btnOK.ModalResult := mrOk;
    btnOK.Default := True;
    btnOK.Left := ConfigForm.Width - 170;
    btnOK.Top := ConfigForm.Height - 70;
    btnOK.Width := 75;

    btnCancel := TButton.Create(ConfigForm);
    btnCancel.Parent := ConfigForm;
    btnCancel.Caption := 'Annuler';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := ConfigForm.Width - 85;
    btnCancel.Top := ConfigForm.Height - 70;
    btnCancel.Width := 75;

    // Afficher le formulaire
    if ConfigForm.ShowModal = mrOk then
    begin
      // Appliquer la configuration
      DeviceID := edtDeviceID.Text;
      PropertyName := edtPropertyName.Text;

      // Réinitialiser l'état
      FIsOnline := False;
      FIsOn := False;

      // Mettre à jour l'affichage
      UpdateDisplay;
    end;
  finally
    ConfigForm.Free;
  end;
end;

function TDeviceStatusWidgetFrame.SaveConfiguration: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('type', 'deviceStatus');
    JSONObj.AddPair('deviceID', FDeviceID);
    JSONObj.AddPair('propertyName', FPropertyName);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TDeviceStatusWidgetFrame.LoadConfiguration(const Config: string);
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(Config) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    if JSONObj.TryGetValue<string>('deviceID', FDeviceID) then ;
    if JSONObj.TryGetValue<string>('propertyName', FPropertyName) then ;

    // Réinitialiser l'état
    FIsOnline := False;
    FIsOn := False;

    UpdateDisplay;
  finally
    JSONObj.Free;
  end;
end;
```

## Gestion des données pour le tableau de bord

Maintenant que nous avons créé nos widgets, nous avons besoin d'une classe pour gérer la connexion aux données IoT :

```pascal
unit IoTDataConnection;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, System.Threading,
  // Nos unités
  IoTDataTypes, MQTTClient;

type
  TIoTDataEvent = procedure(Sender: TObject; const Data: TIoTData) of object;
  TIoTAlertEvent = procedure(Sender: TObject; const AlertType, AlertMessage: string) of object;

  TIoTDataConnection = class
  private
    FMQTTClient: TMQTTClient;
    FConnected: Boolean;
    FBrokerHost: string;
    FBrokerPort: Integer;
    FTopicPrefix: string;
    FReconnectTimer: TTimer;
    FReconnectAttempts: Integer;
    FMaxReconnectAttempts: Integer;
    FDataCache: TDictionary<string, TIoTData>;

    FOnDataReceived: TIoTDataEvent;
    FOnAlert: TIoTAlertEvent;
    FOnConnectionStatusChanged: TNotifyEvent;

    procedure HandleMQTTMessage(Sender: TObject; const Topic: string; const Payload: TBytes);
    procedure HandleMQTTConnect(Sender: TObject; ReturnCode: Integer);
    procedure HandleMQTTDisconnect(Sender: TObject);
    procedure HandleReconnectTimer(Sender: TObject);

    procedure ParseTelemetryMessage(const DeviceID, SensorID: string; const Payload: string);
    procedure ParseStatusMessage(const DeviceID: string; const Payload: string);
    procedure ParseAlertMessage(const DeviceID: string; const Payload: string);
  public
    constructor Create;
    destructor Destroy; override;

    function Connect(const AHost: string; APort: Integer = 1883;
                   const ATopicPrefix: string = 'iot'): Boolean;
    procedure Disconnect;
    function IsConnected: Boolean;

    function SendCommand(const DeviceID, CommandName: string;
                       Params: TJSONObject = nil): Boolean;

    function GetLatestData(const DeviceID, SensorID: string; out Data: TIoTData): Boolean;
    function GetAllDeviceIDs: TArray<string>;
    function GetDeviceSensors(const DeviceID: string): TArray<string>;

    property OnDataReceived: TIoTDataEvent read FOnDataReceived write FOnDataReceived;
    property OnAlert: TIoTAlertEvent read FOnAlert write FOnAlert;
    property OnConnectionStatusChanged: TNotifyEvent read FOnConnectionStatusChanged
                                                  write FOnConnectionStatusChanged;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

L'implémentation de quelques méthodes clés :

```pascal
constructor TIoTDataConnection.Create;
begin
  inherited Create;
  FConnected := False;
  FReconnectAttempts := 0;
  FMaxReconnectAttempts := 5;
  FDataCache := TDictionary<string, TIoTData>.Create;

  // Créer le timer de reconnexion
  FReconnectTimer := TTimer.Create(nil);
  FReconnectTimer.Enabled := False;
  FReconnectTimer.Interval := 5000; // 5 secondes
  FReconnectTimer.OnTimer := HandleReconnectTimer;
end;

destructor TIoTDataConnection.Destroy;
begin
  Disconnect;
  FReconnectTimer.Free;
  FDataCache.Free;
  inherited;
end;

function TIoTDataConnection.Connect(const AHost: string; APort: Integer = 1883;
                                 const ATopicPrefix: string = 'iot'): Boolean;
begin
  Result := False;

  // Sauvegarder les paramètres de connexion
  FBrokerHost := AHost;
  FBrokerPort := APort;
  FTopicPrefix := ATopicPrefix;

  // Créer le client MQTT s'il n'existe pas
  if not Assigned(FMQTTClient) then
  begin
    FMQTTClient := TMQTTClient.Create;
    FMQTTClient.OnMessageReceived := HandleMQTTMessage;
    FMQTTClient.OnConnected := HandleMQTTConnect;
    FMQTTClient.OnDisconnected := HandleMQTTDisconnect;
  end;

  // Configurer le client
  FMQTTClient.BrokerHostname := FBrokerHost;
  FMQTTClient.BrokerPort := FBrokerPort;
  FMQTTClient.ClientID := 'IoTDashboard_' + IntToStr(Random(10000));

  // Se connecter au broker
  try
    Result := FMQTTClient.Connect;
    FConnected := Result;
    FReconnectAttempts := 0;

    if Result then
    begin
      // S'abonner aux topics pertinents
      FMQTTClient.Subscribe(FTopicPrefix + '/+/telemetry/#');
      FMQTTClient.Subscribe(FTopicPrefix + '/+/status');
      FMQTTClient.Subscribe(FTopicPrefix + '/+/alert');

      // Déclencher l'événement de changement de statut
      if Assigned(FOnConnectionStatusChanged) then
        FOnConnectionStatusChanged(Self);
    end;
  except
    on E: Exception do
    begin
      Result := False;
      FConnected := False;

      // Commencer les tentatives de reconnexion
      if not FReconnectTimer.Enabled then
        FReconnectTimer.Enabled := True;
    end;
  end;
end;

procedure TIoTDataConnection.HandleMQTTMessage(Sender: TObject; const Topic: string; const Payload: TBytes);
var
  PayloadStr: string;
  Parts: TArray<string>;
  DeviceID, MessageType, SensorID: string;
begin
  // Convertir le payload en chaîne
  PayloadStr := TEncoding.UTF8.GetString(Payload);

  // Analyser le topic pour déterminer le type de message
  // Format attendu: iot/{deviceID}/{messageType}[/{sensorID}]
  Parts := Topic.Split(['/']);

  if Length(Parts) < 3 then
    Exit;

  DeviceID := Parts[1];
  MessageType := Parts[2];

  if MessageType = 'telemetry' then
  begin
    // Message de télémétrie, format: iot/{deviceID}/telemetry/{sensorID}
    if Length(Parts) < 4 then
      Exit;

    SensorID := Parts[3];
    ParseTelemetryMessage(DeviceID, SensorID, PayloadStr);
  end
  else if MessageType = 'status' then
  begin
    // Message de statut
    ParseStatusMessage(DeviceID, PayloadStr);
  end
  else if MessageType = 'alert' then
  begin
    // Message d'alerte
    ParseAlertMessage(DeviceID, PayloadStr);
  end;
end;

procedure TIoTDataConnection.ParseTelemetryMessage(const DeviceID, SensorID: string; const Payload: string);
var
  JSONObj: TJSONObject;
  Data: TIoTData;
  CacheKey: string;
begin
  JSONObj := TJSONObject.ParseJSONValue(Payload) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    // Créer une structure de données IoT
    FillChar(Data, SizeOf(Data), 0);
    Data.DeviceID := DeviceID;
    Data.SensorID := SensorID;
    Data.Timestamp := Now;

    // Déterminer le type de données
    if JSONObj.TryGetValue<Double>('value', Data.NumericValue) then
      Data.DataType := dtNumeric
    else if JSONObj.TryGetValue<Boolean>('value', Data.BooleanValue) then
      Data.DataType := dtBoolean
    else if JSONObj.TryGetValue<string>('value', Data.TextValue) then
      Data.DataType := dtText
    else
      Data.DataType := dtUnknown;

    // Stocker dans le cache
    CacheKey := DeviceID + '.' + SensorID;
    FDataCache.AddOrSetValue(CacheKey, Data);

    // Déclencher l'événement
    if Assigned(FOnDataReceived) then
      FOnDataReceived(Self, Data);
  finally
    JSONObj.Free;
  end;
end;

function TIoTDataConnection.SendCommand(const DeviceID, CommandName: string;
                                     Params: TJSONObject = nil): Boolean;
var
  CommandObj: TJSONObject;
  CommandStr: string;
  Topic: string;
begin
  Result := False;

  if not IsConnected then
    Exit;

  // Créer l'objet de commande
  CommandObj := TJSONObject.Create;
  try
    CommandObj.AddPair('command', CommandName);

    if Assigned(Params) then
      CommandObj.AddPair('params', Params.Clone as TJSONValue);

    CommandStr := CommandObj.ToString;
  finally
    CommandObj.Free;
  end;

  // Construire le topic
  Topic := Format('%s/%s/command', [FTopicPrefix, DeviceID]);

  // Envoyer la commande
  try
    Result := FMQTTClient.Publish(Topic, TEncoding.UTF8.GetBytes(CommandStr));
  except
    Result := False;
  end;
end;

function TIoTDataConnection.GetLatestData(const DeviceID, SensorID: string; out Data: TIoTData): Boolean;
var
  CacheKey: string;
begin
  CacheKey := DeviceID + '.' + SensorID;
  Result := FDataCache.TryGetValue(CacheKey, Data);
end;
```

## Assemblage du tableau de bord principal

Maintenant, nous allons assembler tous ces composants pour créer notre tableau de bord principal :

```pascal
constructor TfrmMainDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidgets := TList<TDashboardWidgetBase>.Create;
  FDataConnection := TIoTDataConnection.Create;
  FDataConnection.OnDataReceived := HandleNewData;
  FDataConnection.OnAlert := HandleAlert;
  FDataConnection.OnConnectionStatusChanged := HandleConnectionStatusChanged;
end;

destructor TfrmMainDashboard.Destroy;
begin
  SaveDashboardConfiguration;
  ClearDashboardWidgets;
  FWidgets.Free;
  FDataConnection.Free;
  inherited;
end;

procedure TfrmMainDashboard.FormCreate(Sender: TObject);
begin
  // Initialisation de l'interface
  FCurrentView := 'dashboard';

  // Charger la configuration sauvegardée
  LoadDashboardConfiguration;

  // Créer les widgets initiaux ou restaurer ceux sauvegardés
  CreateDashboardWidgets;

  // Se connecter au broker MQTT (valeurs par défaut pour l'exemple)
  FDataConnection.Connect('localhost', 1883, 'iot');

  // Démarrer le timer de rafraîchissement
  tmrRefresh.Enabled := True;
end;

procedure TfrmMainDashboard.LoadDashboardConfiguration;
var
  ConfigFile: string;
  ConfigList: TStringList;
  JSONArray: TJSONArray;
  I: Integer;
  WidgetConfig: TJSONObject;
  WidgetType, ConfigStr: string;
begin
  ConfigFile := ChangeFileExt(Application.ExeName, '.dashboard');

  if not FileExists(ConfigFile) then
    Exit;

  ConfigList := TStringList.Create;
  try
    ConfigList.LoadFromFile(ConfigFile);

    JSONArray := TJSONObject.ParseJSONValue(ConfigList.Text) as TJSONArray;
    if not Assigned(JSONArray) then
      Exit;

    try
      // Parcourir les configurations de widgets
      for I := 0 to JSONArray.Count - 1 do
      begin
        WidgetConfig := JSONArray.Items[I] as TJSONObject;
        if not Assigned(WidgetConfig) then
          Continue;

        if WidgetConfig.TryGetValue<string>('type', WidgetType) and
           WidgetConfig.TryGetValue<string>('config', ConfigStr) then
        begin
          // Créer un widget selon le type
          if WidgetType = 'value' then
            AddValueWidget(ConfigStr)
          else if WidgetType = 'chart' then
            AddChartWidget(ConfigStr)
          else if WidgetType = 'gauge' then
            AddGaugeWidget(ConfigStr)
          else if WidgetType = 'deviceStatus' then
            AddDeviceStatusWidget(ConfigStr);
        end;
      end;
    finally
      JSONArray.Free;
    end;
  finally
    ConfigList.Free;
  end;
end;

procedure TfrmMainDashboard.SaveDashboardConfiguration;
var
  ConfigFile: string;
  ConfigList: TStringList;
  JSONArray: TJSONArray;
  I: Integer;
  Widget: TDashboardWidgetBase;
  WidgetObj: TJSONObject;
  WidgetType, ConfigStr: string;
begin
  ConfigFile := ChangeFileExt(Application.ExeName, '.dashboard');

  JSONArray := TJSONArray.Create;
  try
    // Parcourir tous les widgets
    for I := 0 to FWidgets.Count - 1 do
    begin
      Widget := FWidgets[I];

      WidgetObj := TJSONObject.Create;

      // Déterminer le type de widget
      if Widget is TValueWidgetFrame then
        WidgetType := 'value'
      else if Widget is TChartWidgetFrame then
        WidgetType := 'chart'
      else if Widget is TGaugeWidgetFrame then
        WidgetType := 'gauge'
      else if Widget is TDeviceStatusWidgetFrame then
        WidgetType := 'deviceStatus'
      else
        WidgetType := 'unknown';

      // Obtenir la configuration du widget
      ConfigStr := Widget.SaveConfiguration;

      // Ajouter au tableau JSON
      WidgetObj.AddPair('type', WidgetType);
      WidgetObj.AddPair('config', ConfigStr);

      JSONArray.Add(WidgetObj);
    end;

    // Sauvegarder dans un fichier
    ConfigList := TStringList.Create;
    try
      ConfigList.Text := JSONArray.ToString;
      ConfigList.SaveToFile(ConfigFile);
    finally
      ConfigList.Free;
    end;
  finally
    JSONArray.Free;
  end;
end;

procedure TfrmMainDashboard.CreateDashboardWidgets;
begin
  // Cette méthode est appelée au démarrage
  // Si aucun widget n'a été chargé depuis la configuration, créer des widgets par défaut
  if FWidgets.Count = 0 then
  begin
    // Créer quelques widgets d'exemple
    AddValueWidget('{"sensorID":"temperature","unit":"°C","format":"%.1f","thresholdLow":10,"thresholdHigh":30}');
    AddValueWidget('{"sensorID":"humidity","unit":"%","format":"%.0f","thresholdLow":30,"thresholdHigh":70}');
    AddChartWidget('{"sensorID":"temperature","timeRange":60,"maxPoints":100,"autoScale":true,"displayUnit":"°C"}');
    AddGaugeWidget('{"sensorID":"humidity","minValue":0,"maxValue":100,"displayUnit":"%","thresholdLow":30,"thresholdHigh":70}');
    AddDeviceStatusWidget('{"deviceID":"lamp1","propertyName":"power"}');
  end;
end;

procedure TfrmMainDashboard.ClearDashboardWidgets;
var
  I: Integer;
begin
  // Libérer tous les widgets
  for I := 0 to FWidgets.Count - 1 do
    FWidgets[I].Free;

  FWidgets.Clear;
end;

procedure TfrmMainDashboard.AddValueWidget(const ConfigStr: string = '');
var
  Widget: TValueWidgetFrame;
begin
  Widget := TValueWidgetFrame.Create(scrDashboard);
  Widget.Parent := scrDashboard;
  Widget.Title := 'Valeur';
  Widget.OnClose := HandleWidgetClose;
  Widget.OnSettings := HandleWidgetSettings;

  // Charger la configuration si fournie
  if ConfigStr <> '' then
    Widget.LoadConfiguration(ConfigStr);

  // Positionner le widget
  PositionNewWidget(Widget);

  // Ajouter à la liste
  FWidgets.Add(Widget);
end;

procedure TfrmMainDashboard.AddChartWidget(const ConfigStr: string = '');
var
  Widget: TChartWidgetFrame;
begin
  Widget := TChartWidgetFrame.Create(scrDashboard);
  Widget.Parent := scrDashboard;
  Widget.Title := 'Graphique';
  Widget.OnClose := HandleWidgetClose;
  Widget.OnSettings := HandleWidgetSettings;

  // Charger la configuration si fournie
  if ConfigStr <> '' then
    Widget.LoadConfiguration(ConfigStr);

  // Positionner le widget
  PositionNewWidget(Widget);

  // Ajouter à la liste
  FWidgets.Add(Widget);
end;

procedure TfrmMainDashboard.AddGaugeWidget(const ConfigStr: string = '');
var
  Widget: TGaugeWidgetFrame;
begin
  Widget := TGaugeWidgetFrame.Create(scrDashboard);
  Widget.Parent := scrDashboard;
  Widget.Title := 'Jauge';
  Widget.OnClose := HandleWidgetClose;
  Widget.OnSettings := HandleWidgetSettings;

  // Charger la configuration si fournie
  if ConfigStr <> '' then
    Widget.LoadConfiguration(ConfigStr);

  // Positionner le widget
  PositionNewWidget(Widget);

  // Ajouter à la liste
  FWidgets.Add(Widget);
end;

procedure TfrmMainDashboard.AddDeviceStatusWidget(const ConfigStr: string = '');
var
  Widget: TDeviceStatusWidgetFrame;
begin
  Widget := TDeviceStatusWidgetFrame.Create(scrDashboard);
  Widget.Parent := scrDashboard;
  Widget.Title := 'État dispositif';
  Widget.OnClose := HandleWidgetClose;
  Widget.OnSettings := HandleWidgetSettings;
  Widget.OnToggleRequest := HandleDeviceToggle;

  // Charger la configuration si fournie
  if ConfigStr <> '' then
    Widget.LoadConfiguration(ConfigStr);

  // Positionner le widget
  PositionNewWidget(Widget);

  // Ajouter à la liste
  FWidgets.Add(Widget);
end;

procedure TfrmMainDashboard.PositionNewWidget(Widget: TDashboardWidgetBase);
const
  MARGIN = 10;
var
  MaxBottom, X, Y: Integer;
  I: Integer;
  ExistingWidget: TDashboardWidgetBase;
begin
  // Placer le widget à une position appropriée
  // Implémentation simple: placer en dessous du dernier widget
  MaxBottom := 0;

  for I := 0 to FWidgets.Count - 1 do
  begin
    ExistingWidget := FWidgets[I];
    MaxBottom := Max(MaxBottom, ExistingWidget.Top + ExistingWidget.Height);
  end;

  if MaxBottom = 0 then
  begin
    // Premier widget, le placer en haut à gauche
    Widget.Left := MARGIN;
    Widget.Top := MARGIN;
  end
  else
  begin
    // Placer en dessous du dernier widget
    Widget.Left := MARGIN;
    Widget.Top := MaxBottom + MARGIN;
  end;
end;

procedure TfrmMainDashboard.HandleNewData(Sender: TObject; const Data: TIoTData);
var
  I: Integer;
begin
  // Mettre à jour tous les widgets avec les nouvelles données
  for I := 0 to FWidgets.Count - 1 do
    FWidgets[I].UpdateData(Data);

  // Mettre à jour la barre d'état
  statusBar.Panels[0].Text := Format('Dernière mise à jour: %s',
                                    [FormatDateTime('hh:nn:ss', Now)]);
  statusBar.Panels[1].Text := Format('Capteur: %s.%s',
                                    [Data.DeviceID, Data.SensorID]);

  case Data.DataType of
    dtNumeric: statusBar.Panels[2].Text := Format('Valeur: %.2f', [Data.NumericValue]);
    dtBoolean: statusBar.Panels[2].Text := Format('État: %s', [BoolToStr(Data.BooleanValue, True)]);
    dtText: statusBar.Panels[2].Text := Format('Texte: %s', [Data.TextValue]);
    else statusBar.Panels[2].Text := 'Valeur: (type inconnu)';
  end;
end;

procedure TfrmMainDashboard.HandleAlert(Sender: TObject; const AlertType, AlertMessage: string);
var
  AlertColor: TColor;
begin
  // Déterminer la couleur selon le type d'alerte
  if AlertType = 'error' then
    AlertColor := clRed
  else if AlertType = 'warning' then
    AlertColor := clYellow
  else
    AlertColor := clLime;

  // Afficher l'alerte dans la barre d'état
  statusBar.Panels[0].Text := 'ALERTE: ' + AlertType;
  statusBar.Panels[1].Text := AlertMessage;

  // Vous pourriez aussi ajouter une notification visuelle ou sonore ici

  // Optionnellement, créer une entrée dans un journal des alertes
  // LogAlert(AlertType, AlertMessage);
end;

procedure TfrmMainDashboard.HandleConnectionStatusChanged(Sender: TObject);
begin
  // Mettre à jour l'affichage selon l'état de la connexion
  if FDataConnection.IsConnected then
  begin
    statusBar.Panels[0].Text := 'Connecté';
    statusBar.Panels[1].Text := 'Broker: ' + FDataConnection.BrokerHost;
  end
  else
  begin
    statusBar.Panels[0].Text := 'Déconnecté';
    statusBar.Panels[1].Text := 'Tentative de reconnexion...';
  end;
end;

procedure TfrmMainDashboard.HandleWidgetClose(Sender: TObject);
var
  Widget: TDashboardWidgetBase;
  Index: Integer;
begin
  if Sender is TDashboardWidgetBase then
  begin
    Widget := TDashboardWidgetBase(Sender);

    // Trouver l'index dans la liste
    Index := FWidgets.IndexOf(Widget);
    if Index >= 0 then
    begin
      // Retirer de la liste
      FWidgets.Delete(Index);

      // Libérer le widget
      Widget.Free;

      // Sauvegarder la configuration mise à jour
      SaveDashboardConfiguration;
    end;
  end;
end;

procedure TfrmMainDashboard.HandleWidgetSettings(Sender: TObject);
var
  Widget: TDashboardWidgetBase;
begin
  if Sender is TDashboardWidgetBase then
  begin
    Widget := TDashboardWidgetBase(Sender);

    // Appeler la méthode de configuration
    Widget.Configure;

    // Sauvegarder la configuration mise à jour
    SaveDashboardConfiguration;
  end;
end;

procedure TfrmMainDashboard.HandleDeviceToggle(Sender: TObject);
var
  Widget: TDeviceStatusWidgetFrame;
  Params: TJSONObject;
begin
  if Sender is TDeviceStatusWidgetFrame then
  begin
    Widget := TDeviceStatusWidgetFrame(Sender);

    // Créer les paramètres de la commande
    Params := TJSONObject.Create;
    try
      Params.AddPair('state', TJSONBool.Create(Widget.IsOn));

      // Envoyer la commande au dispositif
      if not FDataConnection.SendCommand(Widget.DeviceID, 'setState', Params) then
        ShowMessage('Erreur lors de l''envoi de la commande au dispositif');
    finally
      Params.Free;
    end;
  end;
end;

procedure TfrmMainDashboard.tmrRefreshTimer(Sender: TObject);
begin
  // Cette méthode est appelée périodiquement pour mettre à jour l'interface

  // Vérifier la connexion et tenter de se reconnecter si nécessaire
  if not FDataConnection.IsConnected then
  begin
    statusBar.Panels[0].Text := 'Déconnecté';
    statusBar.Panels[1].Text := 'Tentative de reconnexion...';

    // Tenter de se reconnecter
    FDataConnection.Connect('localhost', 1883, 'iot');
  end;
end;

procedure TfrmMainDashboard.SwitchView(const ViewName: string);
begin
  // Changer la vue actuelle
  FCurrentView := ViewName;

  // Mettre à jour l'interface
  pnlDashboardContainer.Visible := (ViewName = 'dashboard');
  // ... d'autres panneaux à afficher/masquer selon la vue

  // Mettre à jour les titres
  if ViewName = 'dashboard' then
  begin
    lblPageTitle.Caption := 'Tableau de bord';
    lblPageDescription.Caption := 'Visualisation en temps réel des données IoT';
  end
  else if ViewName = 'devices' then
  begin
    lblPageTitle.Caption := 'Gestion des dispositifs';
    lblPageDescription.Caption := 'Configurer et contrôler vos dispositifs IoT';
  end
  else if ViewName = 'alerts' then
  begin
    lblPageTitle.Caption := 'Alertes';
    lblPageDescription.Caption := 'Historique des alertes et notifications';
  end
  else if ViewName = 'settings' then
  begin
    lblPageTitle.Caption := 'Paramètres';
    lblPageDescription.Caption := 'Configuration du tableau de bord';
  end;
end;

procedure TfrmMainDashboard.sbtnDashboardClick(Sender: TObject);
begin
  SwitchView('dashboard');
end;

procedure TfrmMainDashboard.sbtnDevicesClick(Sender: TObject);
begin
  SwitchView('devices');
  // Ici, vous pourriez implémenter une vue de gestion des dispositifs
end;

procedure TfrmMainDashboard.sbtnAlertsClick(Sender: TObject);
begin
  SwitchView('alerts');
  // Ici, vous pourriez implémenter une vue des alertes
end;

procedure TfrmMainDashboard.sbtnSettingsClick(Sender: TObject);
begin
  SwitchView('settings');
  // Ici, vous pourriez implémenter une vue des paramètres
end;
```

## Création d'un menu pour ajouter de nouveaux widgets

Ajoutons maintenant un menu contextuel qui permettra d'ajouter facilement de nouveaux widgets à notre tableau de bord :

```pascal
procedure TfrmMainDashboard.FormCreate(Sender: TObject);
var
  AddWidgetMenu: TPopupMenu;
  MenuItem: TMenuItem;
  btnAddWidget: TButton;
begin
  // ... code existant ...

  // Créer un menu popup pour ajouter des widgets
  AddWidgetMenu := TPopupMenu.Create(Self);

  MenuItem := TMenuItem.Create(AddWidgetMenu);
  MenuItem.Caption := 'Ajouter un widget de valeur';
  MenuItem.OnClick := AddValueWidgetClick;
  AddWidgetMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(AddWidgetMenu);
  MenuItem.Caption := 'Ajouter un graphique';
  MenuItem.OnClick := AddChartWidgetClick;
  AddWidgetMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(AddWidgetMenu);
  MenuItem.Caption := 'Ajouter une jauge';
  MenuItem.OnClick := AddGaugeWidgetClick;
  AddWidgetMenu.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(AddWidgetMenu);
  MenuItem.Caption := 'Ajouter un état de dispositif';
  MenuItem.OnClick := AddDeviceStatusWidgetClick;
  AddWidgetMenu.Items.Add(MenuItem);

  // Créer un bouton pour ajouter des widgets
  btnAddWidget := TButton.Create(Self);
  btnAddWidget.Parent := pnlContentHeader;
  btnAddWidget.Caption := 'Ajouter Widget';
  btnAddWidget.Width := 100;
  btnAddWidget.Height := 25;
  btnAddWidget.Left := pnlContentHeader.Width - 120;
  btnAddWidget.Top := 10;
  btnAddWidget.PopupMenu := AddWidgetMenu;
  btnAddWidget.OnClick := btnAddWidgetClick;

  // ... reste du code ...
end;

procedure TfrmMainDashboard.btnAddWidgetClick(Sender: TObject);
var
  Button: TButton;
  Point: TPoint;
begin
  Button := Sender as TButton;

  // Afficher le menu popup sous le bouton
  Point := Button.ClientToScreen(Point(0, Button.Height));
  Button.PopupMenu.Popup(Point.X, Point.Y);
end;

procedure TfrmMainDashboard.AddValueWidgetClick(Sender: TObject);
begin
  AddValueWidget();
  SaveDashboardConfiguration;
end;

procedure TfrmMainDashboard.AddChartWidgetClick(Sender: TObject);
begin
  AddChartWidget();
  SaveDashboardConfiguration;
end;

procedure TfrmMainDashboard.AddGaugeWidgetClick(Sender: TObject);
begin
  AddGaugeWidget();
  SaveDashboardConfiguration;
end;

procedure TfrmMainDashboard.AddDeviceStatusWidgetClick(Sender: TObject);
begin
  AddDeviceStatusWidget();
  SaveDashboardConfiguration;
end;
```

## Mise en place d'une vue de gestion des dispositifs

Créons maintenant une vue permettant de gérer les dispositifs IoT connectés :

```pascal
unit DevicesView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, IoTDataConnection, IoTDataTypes;

type
  TDevicesViewFrame = class(TFrame)
    pnlTop: TPanel;
    btnRefresh: TButton;
    btnAdd: TButton;
    btnRemove: TButton;
    lvDevices: TListView;
    splVertical: TSplitter;
    pnlDeviceDetail: TPanel;
    lblDeviceName: TLabel;
    edtDeviceName: TEdit;
    lblDeviceID: TLabel;
    edtDeviceID: TEdit;
    lblDeviceType: TLabel;
    cmbDeviceType: TComboBox;
    lblStatus: TLabel;
    shpStatus: TShape;
    lblStatusText: TLabel;
    btnConnect: TButton;
    btnDisconnect: TButton;
    lvProperties: TListView;
    lblProperties: TLabel;
    grpCommands: TGroupBox;
    cmbCommand: TComboBox;
    btnExecute: TButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure lvDevicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
  private
    FDataConnection: TIoTDataConnection;
    FSelectedDeviceID: string;

    procedure RefreshDeviceList;
    procedure RefreshDeviceDetail;
    procedure HandleNewData(Sender: TObject; const Data: TIoTData);
  public
    constructor Create(AOwner: TComponent); override;

    property DataConnection: TIoTDataConnection read FDataConnection write FDataConnection;
  end;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

Voici l'implémentation de la vue des dispositifs :

```pascal
constructor TDevicesViewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialiser la liste des dispositifs
  lvDevices.ViewStyle := vsReport;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Nom';
    Width := 150;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'ID';
    Width := 100;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'Type';
    Width := 100;
  end;

  with lvDevices.Columns.Add do
  begin
    Caption := 'État';
    Width := 80;
  end;

  // Initialiser la liste des propriétés
  lvProperties.ViewStyle := vsReport;

  with lvProperties.Columns.Add do
  begin
    Caption := 'Propriété';
    Width := 150;
  end;

  with lvProperties.Columns.Add do
  begin
    Caption := 'Valeur';
    Width := 150;
  end;

  with lvProperties.Columns.Add do
  begin
    Caption := 'Dernière mise à jour';
    Width := 150;
  end;

  // Désactiver les détails jusqu'à la sélection d'un dispositif
  pnlDeviceDetail.Enabled := False;
end;

procedure TDevicesViewFrame.RefreshDeviceList;
var
  DeviceIDs: TArray<string>;
  DeviceID: string;
  Item: TListItem;
  Data: TIoTData;
  DeviceName, DeviceType, DeviceStatus: string;
  IsOnline: Boolean;
begin
  // Effacer la liste actuelle
  lvDevices.Items.Clear;

  if not Assigned(FDataConnection) then
    Exit;

  // Obtenir la liste des dispositifs
  DeviceIDs := FDataConnection.GetAllDeviceIDs;

  for DeviceID in DeviceIDs do
  begin
    // Obtenir les informations de base
    if FDataConnection.GetLatestData(DeviceID, 'info.name', Data) then
      DeviceName := Data.TextValue
    else
      DeviceName := DeviceID;

    if FDataConnection.GetLatestData(DeviceID, 'info.type', Data) then
      DeviceType := Data.TextValue
    else
      DeviceType := 'Inconnu';

    // Vérifier si le dispositif est en ligne
    if FDataConnection.GetLatestData(DeviceID, 'status', Data) and (Data.DataType = dtBoolean) then
      IsOnline := Data.BooleanValue
    else
      IsOnline := False;

    if IsOnline then
      DeviceStatus := 'En ligne'
    else
      DeviceStatus := 'Hors ligne';

    // Ajouter à la liste
    Item := lvDevices.Items.Add;
    Item.Caption := DeviceName;
    Item.SubItems.Add(DeviceID);
    Item.SubItems.Add(DeviceType);
    Item.SubItems.Add(DeviceStatus);

    // Stocker l'ID comme data
    Item.Data := Pointer(DeviceID);
  end;
end;

procedure TDevicesViewFrame.lvDevicesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected and Assigned(Item) then
  begin
    // Stocker l'ID du dispositif sélectionné
    FSelectedDeviceID := string(Item.Data);

    // Activer les détails
    pnlDeviceDetail.Enabled := True;

    // Mettre à jour les détails
    RefreshDeviceDetail;
  end
  else
  begin
    // Aucun dispositif sélectionné
    FSelectedDeviceID := '';
    pnlDeviceDetail.Enabled := False;
  end;
end;

procedure TDevicesViewFrame.RefreshDeviceDetail;
var
  Data: TIoTData;
  SensorIDs: TArray<string>;
  SensorID: string;
  Item: TListItem;
  PropValue, DateStr: string;
begin
  if (FSelectedDeviceID = '') or not Assigned(FDataConnection) then
    Exit;

  // Mettre à jour les champs de base
  if FDataConnection.GetLatestData(FSelectedDeviceID, 'info.name', Data) then
    edtDeviceName.Text := Data.TextValue
  else
    edtDeviceName.Text := FSelectedDeviceID;

  edtDeviceID.Text := FSelectedDeviceID;

  if FDataConnection.GetLatestData(FSelectedDeviceID, 'info.type', Data) then
  begin
    // Trouver l'index dans la liste des types
    var TypeIndex := cmbDeviceType.Items.IndexOf(Data.TextValue);
    if TypeIndex >= 0 then
      cmbDeviceType.ItemIndex := TypeIndex
    else
    begin
      // Ajouter le type s'il n'existe pas
      cmbDeviceType.Items.Add(Data.TextValue);
      cmbDeviceType.ItemIndex := cmbDeviceType.Items.Count - 1;
    end;
  end
  else
    cmbDeviceType.ItemIndex := -1;

  // Vérifier si le dispositif est en ligne
  if FDataConnection.GetLatestData(FSelectedDeviceID, 'status', Data) and (Data.DataType = dtBoolean) then
  begin
    if Data.BooleanValue then
    begin
      shpStatus.Brush.Color := clGreen;
      lblStatusText.Caption := 'En ligne';
      btnConnect.Enabled := False;
      btnDisconnect.Enabled := True;
    end
    else
    begin
      shpStatus.Brush.Color := clRed;
      lblStatusText.Caption := 'Hors ligne';
      btnConnect.Enabled := True;
      btnDisconnect.Enabled := False;
    end;
  end
  else
  begin
    shpStatus.Brush.Color := clGray;
    lblStatusText.Caption := 'Inconnu';
    btnConnect.Enabled := True;
    btnDisconnect.Enabled := False;
  end;

  // Mettre à jour la liste des propriétés
  lvProperties.Items.Clear;

  // Obtenir les capteurs/propriétés du dispositif
  SensorIDs := FDataConnection.GetDeviceSensors(FSelectedDeviceID);

  for SensorID in SensorIDs do
  begin
    if FDataConnection.GetLatestData(FSelectedDeviceID, SensorID, Data) then
    begin
      Item := lvProperties.Items.Add;
      Item.Caption := SensorID;

      // Convertir la valeur en texte selon le type
      case Data.DataType of
        dtNumeric: PropValue := FormatFloat('0.##', Data.NumericValue);
        dtBoolean: PropValue := BoolToStr(Data.BooleanValue, True);
        dtText: PropValue := Data.TextValue;
        else PropValue := '?';
      end;

      Item.SubItems.Add(PropValue);

      // Formater la date
      DateStr := FormatDateTime('dd/mm/yyyy hh:nn:ss', Data.Timestamp);
      Item.SubItems.Add(DateStr);
    end;
  end;

  // Mettre à jour la liste des commandes disponibles
  cmbCommand.Items.Clear();

  // Ajouter les commandes standards
  cmbCommand.Items.Add('refresh');
  cmbCommand.Items.Add('reboot');

  // Ajouter des commandes spécifiques selon le type
  if cmbDeviceType.Text = 'Capteur' then
  begin
    cmbCommand.Items.Add('calibrate');
  end
  else if cmbDeviceType.Text = 'Actionneur' then
  begin
    cmbCommand.Items.Add('turnOn');
    cmbCommand.Items.Add('turnOff');
  end;

  if cmbCommand.Items.Count > 0 then
    cmbCommand.ItemIndex := 0;
end;

procedure TDevicesViewFrame.btnRefreshClick(Sender: TObject);
begin
  // Rafraîchir la liste des dispositifs
  RefreshDeviceList;

  // Si un dispositif est sélectionné, rafraîchir ses détails
  if FSelectedDeviceID <> '' then
    RefreshDeviceDetail;
end;

procedure TDevicesViewFrame.btnAddClick(Sender: TObject);
var
  DeviceID, DeviceName, DeviceType: string;
  Params: TJSONObject;
begin
  // Demander les informations du nouveau dispositif
  DeviceID := InputBox('Nouveau dispositif', 'ID du dispositif:', '');
  if DeviceID = '' then
    Exit;

  DeviceName := InputBox('Nouveau dispositif', 'Nom du dispositif:', DeviceID);

  DeviceType := 'Generic';
  if cmbDeviceType.Items.Count > 0 then
    DeviceType := cmbDeviceType.Items[0];

  // Créer le dispositif via une commande au système de gestion IoT
  Params := TJSONObject.Create;
  try
    Params.AddPair('deviceID', DeviceID);
    Params.AddPair('name', DeviceName);
    Params.AddPair('type', DeviceType);

    if Assigned(FDataConnection) and
       FDataConnection.SendCommand('system', 'addDevice', Params) then
    begin
      ShowMessage('Dispositif ajouté avec succès!');
      RefreshDeviceList;
    end
    else
      ShowMessage('Erreur lors de l''ajout du dispositif');
  finally
    Params.Free;
  end;
end;

procedure TDevicesViewFrame.btnRemoveClick(Sender: TObject);
var
  Params: TJSONObject;
begin
  if (FSelectedDeviceID = '') or not Assigned(FDataConnection) then
    Exit;

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce dispositif?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Supprimer le dispositif via une commande au système de gestion IoT
  Params := TJSONObject.Create;
  try
    Params.AddPair('deviceID', FSelectedDeviceID);

    if FDataConnection.SendCommand('system', 'removeDevice', Params) then
    begin
      ShowMessage('Dispositif supprimé avec succès!');
      FSelectedDeviceID := '';
      pnlDeviceDetail.Enabled := False;
      RefreshDeviceList;
    end
    else
      ShowMessage('Erreur lors de la suppression du dispositif');
  finally
    Params.Free;
  end;
end;

procedure TDevicesViewFrame.btnConnectClick(Sender: TObject);
var
  Params: TJSONObject;
begin
  if (FSelectedDeviceID = '') or not Assigned(FDataConnection) then
    Exit;

  // Envoyer une commande de connexion au dispositif
  if FDataConnection.SendCommand(FSelectedDeviceID, 'connect') then
  begin
    // La mise à jour de l'état sera gérée via les notifications de données
    ShowMessage('Commande de connexion envoyée');
  end
  else
    ShowMessage('Erreur lors de l''envoi de la commande de connexion');
end;

procedure TDevicesViewFrame.btnDisconnectClick(Sender: TObject);
begin
  if (FSelectedDeviceID = '') or not Assigned(FDataConnection) then
    Exit;

  // Envoyer une commande de déconnexion au dispositif
  if FDataConnection.SendCommand(FSelectedDeviceID, 'disconnect') then
  begin
    // La mise à jour de l'état sera gérée via les notifications de données
    ShowMessage('Commande de déconnexion envoyée');
  end
  else
    ShowMessage('Erreur lors de l''envoi de la commande de déconnexion');
end;

procedure TDevicesViewFrame.btnExecuteClick(Sender: TObject);
var
  Command: string;
begin
  if (FSelectedDeviceID = '') or not Assigned(FDataConnection) or
     (cmbCommand.ItemIndex < 0) then
    Exit;

  Command := cmbCommand.Items[cmbCommand.ItemIndex];

  // Envoyer la commande au dispositif
  if FDataConnection.SendCommand(FSelectedDeviceID, Command) then
    ShowMessage('Commande "' + Command + '" envoyée avec succès')
  else
    ShowMessage('Erreur lors de l''envoi de la commande');
end;

procedure TDevicesViewFrame.HandleNewData(Sender: TObject; const Data: TIoTData);
begin
  // Si les données concernent le dispositif actuellement sélectionné, mettre à jour les détails
  if (Data.DeviceID = FSelectedDeviceID) then
    RefreshDeviceDetail;

  // Si les données concernent le statut d'un dispositif, mettre à jour la liste
  if (Data.SensorID = 'status') then
    RefreshDeviceList;
end;
```

## Intégration des vues dans le tableau de bord principal

Pour intégrer la vue des dispositifs dans notre tableau de bord, nous allons modifier le formulaire principal :

```pascal
// Dans l'unité MainDashboard, ajouter:
uses
  // ... autres unités ...
  DevicesView;

// Ajouter une variable membre:
private
  // ... autres variables ...
  FDevicesView: TDevicesViewFrame;

// Dans la méthode FormCreate:
procedure TfrmMainDashboard.FormCreate(Sender: TObject);
begin
  // ... code existant ...

  // Créer la vue des dispositifs
  FDevicesView := TDevicesViewFrame.Create(Self);
  FDevicesView.Parent := Self;
  FDevicesView.Align := alClient;
  FDevicesView.Visible := False;
  FDevicesView.DataConnection := FDataConnection;

  // ... reste du code ...
end;

// Modifier la méthode SwitchView:
procedure TfrmMainDashboard.SwitchView(const ViewName: string);
begin
  // Changer la vue actuelle
  FCurrentView := ViewName;

  // Masquer toutes les vues
  pnlDashboardContainer.Visible := False;
  FDevicesView.Visible := False;
  // ... autres vues ...

  // Afficher la vue appropriée
  if ViewName = 'dashboard' then
  begin
    pnlDashboardContainer.Visible := True;
    lblPageTitle.Caption := 'Tableau de bord';
    lblPageDescription.Caption := 'Visualisation en temps réel des données IoT';
  end
  else if ViewName = 'devices' then
  begin
    FDevicesView.Visible := True;
    lblPageTitle.Caption := 'Gestion des dispositifs';
    lblPageDescription.Caption := 'Configurer et contrôler vos dispositifs IoT';
  end
  // ... autres vues ...
end;
```

## Création d'alertes et notifications

Pour améliorer l'expérience utilisateur, ajoutons un système d'alertes qui notifie l'utilisateur des événements importants :

```pascal
unit AlertManager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.JSON,
  System.DateUtils;

type
  TAlertLevel = (alInfo, alWarning, alError, alCritical);

  TAlertEvent = procedure(Sender: TObject; Level: TAlertLevel;
                        const Source, Message: string) of object;

  TAlertRecord = record
    ID: Integer;
    Timestamp: TDateTime;
    Level: TAlertLevel;
    Source: string;
    Message: string;
    Acknowledged: Boolean;
  end;

  TAlertManager = class
  private
    FAlerts: TList<TAlertRecord>;
    FNextID: Integer;
    FMaxAlerts: Integer;
    FOnNewAlert: TAlertEvent;

    function GetAlertCount: Integer;
    function GetUnacknowledgedCount: Integer;
  public
    constructor Create(AMaxAlerts: Integer = 1000);
    destructor Destroy; override;

    procedure AddAlert(Level: TAlertLevel; const Source, Message: string);
    procedure AcknowledgeAlert(ID: Integer);
    procedure AcknowledgeAllAlerts;
    procedure ClearAlerts;

    function GetAlerts(Count: Integer = 100;
                     UnacknowledgedOnly: Boolean = False): TArray<TAlertRecord>;
    function GetAlertsByLevel(Level: TAlertLevel): TArray<TAlertRecord>;
    function GetAlertsBySource(const Source: string): TArray<TAlertRecord>;

    function SaveToJSON: string;
    procedure LoadFromJSON(const JSONStr: string);

    property AlertCount: Integer read GetAlertCount;
    property UnacknowledgedCount: Integer read GetUnacknowledgedCount;
    property OnNewAlert: TAlertEvent read FOnNewAlert write FOnNewAlert;
  end;

implementation

constructor TAlertManager.Create(AMaxAlerts: Integer = 1000);
begin
  inherited Create;
  FAlerts := TList<TAlertRecord>.Create;
  FNextID := 1;
  FMaxAlerts := AMaxAlerts;
end;

destructor TAlertManager.Destroy;
begin
  FAlerts.Free;
  inherited;
end;

function TAlertManager.GetAlertCount: Integer;
begin
  Result := FAlerts.Count;
end;

function TAlertManager.GetUnacknowledgedCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FAlerts.Count - 1 do
    if not FAlerts[I].Acknowledged then
      Inc(Result);
end;

procedure TAlertManager.AddAlert(Level: TAlertLevel; const Source, Message: string);
var
  Alert: TAlertRecord;
begin
  // Préparer la nouvelle alerte
  Alert.ID := FNextID;
  Inc(FNextID);
  Alert.Timestamp := Now;
  Alert.Level := Level;
  Alert.Source := Source;
  Alert.Message := Message;
  Alert.Acknowledged := False;

  // Ajouter à la liste
  FAlerts.Add(Alert);

  // Limiter le nombre d'alertes
  while FAlerts.Count > FMaxAlerts do
    FAlerts.Delete(0);

  // Déclencher l'événement
  if Assigned(FOnNewAlert) then
    FOnNewAlert(Self, Level, Source, Message);
end;

procedure TAlertManager.AcknowledgeAlert(ID: Integer);
var
  I: Integer;
begin
  for I := 0 to FAlerts.Count - 1 do
  begin
    if FAlerts[I].ID = ID then
    begin
      var Alert := FAlerts[I];
      Alert.Acknowledged := True;
      FAlerts[I] := Alert;
      Break;
    end;
  end;
end;

procedure TAlertManager.AcknowledgeAllAlerts;
var
  I: Integer;
begin
  for I := 0 to FAlerts.Count - 1 do
  begin
    var Alert := FAlerts[I];
    Alert.Acknowledged := True;
    FAlerts[I] := Alert;
  end;
end;

procedure TAlertManager.ClearAlerts;
begin
  FAlerts.Clear;
end;

function TAlertManager.GetAlerts(Count: Integer = 100;
                               UnacknowledgedOnly: Boolean = False): TArray<TAlertRecord>;
var
  ResultList: TList<TAlertRecord>;
  I, RemainingCount: Integer;
begin
  ResultList := TList<TAlertRecord>.Create;
  try
    // Partir des alertes les plus récentes (fin de la liste)
    RemainingCount := Count;
    for I := FAlerts.Count - 1 downto 0 do
    begin
      if RemainingCount <= 0 then
        Break;

      if not UnacknowledgedOnly or not FAlerts[I].Acknowledged then
      begin
        ResultList.Add(FAlerts[I]);
        Dec(RemainingCount);
      end;
    end;

    // Convertir en tableau
    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TAlertManager.GetAlertsByLevel(Level: TAlertLevel): TArray<TAlertRecord>;
var
  ResultList: TList<TAlertRecord>;
  I: Integer;
begin
  ResultList := TList<TAlertRecord>.Create;
  try
    for I := 0 to FAlerts.Count - 1 do
    begin
      if FAlerts[I].Level = Level then
        ResultList.Add(FAlerts[I]);
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TAlertManager.GetAlertsBySource(const Source: string): TArray<TAlertRecord>;
var
  ResultList: TList<TAlertRecord>;
  I: Integer;
begin
  ResultList := TList<TAlertRecord>.Create;
  try
    for I := 0 to FAlerts.Count - 1 do
    begin
      if FAlerts[I].Source = Source then
        ResultList.Add(FAlerts[I]);
    end;

    Result := ResultList.ToArray;
  finally
    ResultList.Free;
  end;
end;

function TAlertManager.SaveToJSON: string;
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  I: Integer;
  LevelStr: string;
begin
  JSONArray := TJSONArray.Create;
  try
    for I := 0 to FAlerts.Count - 1 do
    begin
      JSONObj := TJSONObject.Create;

      JSONObj.AddPair('id', TJSONNumber.Create(FAlerts[I].ID));
      JSONObj.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz', FAlerts[I].Timestamp));

      case FAlerts[I].Level of
        alInfo: LevelStr := 'info';
        alWarning: LevelStr := 'warning';
        alError: LevelStr := 'error';
        alCritical: LevelStr := 'critical';
      end;

      JSONObj.AddPair('level', LevelStr);
      JSONObj.AddPair('source', FAlerts[I].Source);
      JSONObj.AddPair('message', FAlerts[I].Message);
      JSONObj.AddPair('acknowledged', TJSONBool.Create(FAlerts[I].Acknowledged));

      JSONArray.Add(JSONObj);
    end;

    Result := JSONArray.ToString;
  finally
    JSONArray.Free;
  end;
end;

procedure TAlertManager.LoadFromJSON(const JSONStr: string);
var
  JSONArray: TJSONArray;
  JSONObj: TJSONObject;
  I: Integer;
  Alert: TAlertRecord;
  LevelStr: string;
  MaxID: Integer;
begin
  FAlerts.Clear;

  JSONArray := TJSONObject.ParseJSONValue(JSONStr) as TJSONArray;
  if not Assigned(JSONArray) then
    Exit;

  try
    MaxID := 0;

    for I := 0 to JSONArray.Count - 1 do
    begin
      JSONObj := JSONArray.Items[I] as TJSONObject;
      if not Assigned(JSONObj) then
        Continue;

      FillChar(Alert, SizeOf(Alert), 0);

      if JSONObj.TryGetValue<Integer>('id', Alert.ID) then
        MaxID := Max(MaxID, Alert.ID);

      JSONObj.TryGetValue<string>('timestamp', LevelStr);
      Alert.Timestamp := StrToDateTimeDef(LevelStr, Now);

      if JSONObj.TryGetValue<string>('level', LevelStr) then
      begin
        if LevelStr = 'info' then
          Alert.Level := alInfo
        else if LevelStr = 'warning' then
          Alert.Level := alWarning
        else if LevelStr = 'error' then
          Alert.Level := alError
        else if LevelStr = 'critical' then
          Alert.Level := alCritical;
      end;

      JSONObj.TryGetValue<string>('source', Alert.Source);
      JSONObj.TryGetValue<string>('message', Alert.Message);
      JSONObj.TryGetValue<Boolean>('acknowledged', Alert.Acknowledged);

      FAlerts.Add(Alert);
    end;

    // Mettre à jour le prochain ID
    FNextID := MaxID + 1;
  finally
    JSONArray.Free;
  end;
end;
```

Maintenant, créons une vue pour afficher et gérer les alertes :

```pascal
unit AlertsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, System.ImageList, Vcl.ImgList, AlertManager;

type
  TAlertsViewFrame = class(TFrame)
    pnlTop: TPanel;
    btnRefresh: TButton;
    btnAcknowledgeAll: TButton;
    btnClear: TButton;
    cmbFilterLevel: TComboBox;
    lblFilter: TLabel;
    edtFilterSource: TEdit;
    btnFilter: TButton;
    lvAlerts: TListView;
    ilAlertIcons: TImageList;
    splHorizontal: TSplitter;
    pnlDetail: TPanel;
    lblDetailTitle: TLabel;
    mmoAlertDetail: TMemo;
    btnAcknowledge: TButton;
    procedure btnRefreshClick(Sender: TObject);
    procedure btnAcknowledgeAllClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure lvAlertsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnAcknowledgeClick(Sender: TObject);
  private
    FAlertManager: TAlertManager;
    FSelectedAlertID: Integer;

    procedure RefreshAlertList;
    procedure HandleNewAlert(Sender: TObject; Level: TAlertLevel;
                          const Source, Message: string);
  public
    constructor Create(AOwner: TComponent); override;

    property AlertManager: TAlertManager read FAlertManager write FAlertManager;
  end;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

Voici l'implémentation de la vue des alertes :

```pascal
constructor TAlertsViewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialiser la liste des alertes
  lvAlerts.ViewStyle := vsReport;

  with lvAlerts.Columns.Add do
  begin
    Caption := 'Niveau';
    Width := 70;
  end;

  with lvAlerts.Columns.Add do
  begin
    Caption := 'Horodatage';
    Width := 150;
  end;

  with lvAlerts.Columns.Add do
  begin
    Caption := 'Source';
    Width := 120;
  end;

  with lvAlerts.Columns.Add do
  begin
    Caption := 'Message';
    Width := 300;
  end;

  with lvAlerts.Columns.Add do
  begin
    Caption := 'État';
    Width := 80;
  end;

  // Initialiser le combo de filtrage par niveau
  cmbFilterLevel.Items.Add('Tous');
  cmbFilterLevel.Items.Add('Info');
  cmbFilterLevel.Items.Add('Avertissement');
  cmbFilterLevel.Items.Add('Erreur');
  cmbFilterLevel.Items.Add('Critique');
  cmbFilterLevel.ItemIndex := 0;

  // Désactiver les détails jusqu'à la sélection d'une alerte
  pnlDetail.Enabled := False;
  FSelectedAlertID := -1;
end;

procedure TAlertsViewFrame.RefreshAlertList;
var
  Alerts: TArray<TAlertRecord>;
  Alert: TAlertRecord;
  Item: TListItem;
  LevelStr, StatusStr: string;
  IconIndex: Integer;
  FilterLevel: TAlertLevel;
  FilterSource: string;
  ApplyLevelFilter, ApplySourceFilter: Boolean;
begin
  // Effacer la liste actuelle
  lvAlerts.Items.Clear;

  if not Assigned(FAlertManager) then
    Exit;

  // Déterminer les filtres à appliquer
  ApplyLevelFilter := cmbFilterLevel.ItemIndex > 0;

  if ApplyLevelFilter then
    FilterLevel := TAlertLevel(cmbFilterLevel.ItemIndex - 1);

  FilterSource := Trim(edtFilterSource.Text);
  ApplySourceFilter := FilterSource <> '';

  // Obtenir les alertes
  if ApplyLevelFilter then
    Alerts := FAlertManager.GetAlertsByLevel(FilterLevel)
  else if ApplySourceFilter then
    Alerts := FAlertManager.GetAlertsBySource(FilterSource)
  else
    Alerts := FAlertManager.GetAlerts(1000);  // Récupérer les 1000 dernières alertes

  // Appliquer le filtre source si nécessaire (en plus du filtre niveau)
  if ApplyLevelFilter and ApplySourceFilter then
  begin
    var FilteredAlerts: TList<TAlertRecord> := TList<TAlertRecord>.Create;
    try
      for Alert in Alerts do
      begin
        if AnsiContainsText(Alert.Source, FilterSource) then
          FilteredAlerts.Add(Alert);
      end;

      Alerts := FilteredAlerts.ToArray;
    finally
      FilteredAlerts.Free;
    end;
  end;

  // Ajouter les alertes à la liste
  for Alert in Alerts do
  begin
    Item := lvAlerts.Items.Add;

    // Déterminer l'icône et le texte du niveau
    case Alert.Level of
      alInfo:
        begin
          IconIndex := 0;
          LevelStr := 'Info';
        end;
      alWarning:
        begin
          IconIndex := 1;
          LevelStr := 'Avert.';
        end;
      alError:
        begin
          IconIndex := 2;
          LevelStr := 'Erreur';
        end;
      alCritical:
        begin
          IconIndex := 3;
          LevelStr := 'Critique';
        end;
    end;

    Item.ImageIndex := IconIndex;
    Item.Caption := LevelStr;

    // Ajouter les autres informations
    Item.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn:ss', Alert.Timestamp));
    Item.SubItems.Add(Alert.Source);
    Item.SubItems.Add(Alert.Message);

    if Alert.Acknowledged then
      StatusStr := 'Acquittée'
    else
      StatusStr := 'Non acquittée';

    Item.SubItems.Add(StatusStr);

    // Stocker l'ID comme data
    Item.Data := Pointer(Alert.ID);
  end;
end;

procedure TAlertsViewFrame.lvAlertsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Alerts: TArray<TAlertRecord>;
  Alert: TAlertRecord;
  I: Integer;
  Found: Boolean;
begin
  if Selected and Assigned(Item) then
  begin
    // Stocker l'ID de l'alerte sélectionnée
    FSelectedAlertID := Integer(Item.Data);

    // Activer les détails
    pnlDetail.Enabled := True;

    // Obtenir toutes les alertes pour trouver celle sélectionnée
    Alerts := FAlertManager.GetAlerts(1000);

    Found := False;
    for I := 0 to Length(Alerts) - 1 do
    begin
      if Alerts[I].ID = FSelectedAlertID then
      begin
        Alert := Alerts[I];
        Found := True;
        Break;
      end;
    end;

    if Found then
    begin
      // Mettre à jour le titre
      lblDetailTitle.Caption := Format('Détails de l''alerte #%d', [Alert.ID]);

      // Mettre à jour le contenu
      mmoAlertDetail.Lines.Clear;
      mmoAlertDetail.Lines.Add('Niveau: ' +
                              case Alert.Level of
                                alInfo: 'Information';
                                alWarning: 'Avertissement';
                                alError: 'Erreur';
                                alCritical: 'Critique';
                              end);
      mmoAlertDetail.Lines.Add('Horodatage: ' + FormatDateTime('dd/mm/yyyy hh:nn:ss', Alert.Timestamp));
      mmoAlertDetail.Lines.Add('Source: ' + Alert.Source);
      mmoAlertDetail.Lines.Add('État: ' + IfThen(Alert.Acknowledged, 'Acquittée', 'Non acquittée'));
      mmoAlertDetail.Lines.Add('');
      mmoAlertDetail.Lines.Add('Message:');
      mmoAlertDetail.Lines.Add(Alert.Message);

      // Activer/désactiver le bouton d'acquittement
      btnAcknowledge.Enabled := not Alert.Acknowledged;
    end;
  end
  else
  begin
    // Aucune alerte sélectionnée
    FSelectedAlertID := -1;
    pnlDetail.Enabled := False;
  end;
end;

procedure TAlertsViewFrame.btnRefreshClick(Sender: TObject);
begin
  RefreshAlertList;
end;

procedure TAlertsViewFrame.btnAcknowledgeAllClick(Sender: TObject);
begin
  if not Assigned(FAlertManager) then
    Exit;

  // Acquitter toutes les alertes
  FAlertManager.AcknowledgeAllAlerts;

  // Rafraîchir la liste
  RefreshAlertList;

  // Désactiver le bouton d'acquittement
  btnAcknowledge.Enabled := False;
end;

procedure TAlertsViewFrame.btnClearClick(Sender: TObject);
begin
  if not Assigned(FAlertManager) then
    Exit;

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir effacer toutes les alertes?',
               mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Effacer toutes les alertes
  FAlertManager.ClearAlerts;

  // Rafraîchir la liste
  RefreshAlertList;

  // Désactiver les détails
  pnlDetail.Enabled := False;
  FSelectedAlertID := -1;
end;

procedure TAlertsViewFrame.btnFilterClick(Sender: TObject);
begin
  // Appliquer les filtres
  RefreshAlertList;
end;

procedure TAlertsViewFrame.btnAcknowledgeClick(Sender: TObject);
begin
  if (FSelectedAlertID < 0) or not Assigned(FAlertManager) then
    Exit;

  // Acquitter l'alerte sélectionnée
  FAlertManager.AcknowledgeAlert(FSelectedAlertID);

  // Rafraîchir la liste
  RefreshAlertList;

  // Désactiver le bouton d'acquittement
  btnAcknowledge.Enabled := False;
end;

procedure TAlertsViewFrame.HandleNewAlert(Sender: TObject; Level: TAlertLevel;
                                       const Source, Message: string);
begin
  // Rafraîchir la liste lors de la réception d'une nouvelle alerte
  RefreshAlertList;

  // Vous pourriez aussi ajouter une notification sonore ou visuelle ici
end;
```

## Intégration du gestionnaire d'alertes dans le tableau de bord principal

```pascal
// Dans l'unité MainDashboard, ajouter:
uses
  // ... autres unités ...
  AlertManager, AlertsView;

// Ajouter des variables membres:
private
  // ... autres variables ...
  FAlertManager: TAlertManager;
  FAlertsView: TAlertsViewFrame;

// Dans le constructeur:
constructor TfrmMainDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ... code existant ...

  // Créer le gestionnaire d'alertes
  FAlertManager := TAlertManager.Create;
  FAlertManager.OnNewAlert := HandleAlertNotification;

  // Configurer la connexion de données pour utiliser le gestionnaire d'alertes
  FDataConnection.OnAlert := HandleDataAlert;

  // Créer la vue des alertes
  FAlertsView := TAlertsViewFrame.Create(Self);
  FAlertsView.Parent := Self;
  FAlertsView.Align := alClient;
  FAlertsView.Visible := False;
  FAlertsView.AlertManager := FAlertManager;

  // ... reste du code ...
end;

// Modifier la méthode SwitchView:
procedure TfrmMainDashboard.SwitchView(const ViewName: string);
begin
  // ... code existant ...

  else if ViewName = 'alerts' then
  begin
    FAlertsView.Visible := True;
    lblPageTitle.Caption := 'Alertes';
    lblPageDescription.Caption := 'Gestion des alertes et notifications';

    // Rafraîchir la liste des alertes
    FAlertsView.RefreshAlertList;
  end

  // ... reste du code ...
end;

// Ajouter les méthodes de gestion des alertes:
procedure TfrmMainDashboard.HandleDataAlert(Sender: TObject; const AlertType, AlertMessage: string);
var
  Level: TAlertLevel;
begin
  // Convertir le type d'alerte en niveau
  if AlertType = 'info' then
    Level := alInfo
  else if AlertType = 'warning' then
    Level := alWarning
  else if AlertType = 'error' then
    Level := alError
  else if AlertType = 'critical' then
    Level := alCritical
  else
    Level := alInfo;

  // Ajouter l'alerte au gestionnaire
  FAlertManager.AddAlert(Level, 'IoTData', AlertMessage);
end;

procedure TfrmMainDashboard.HandleAlertNotification(Sender: TObject; Level: TAlertLevel;
                                               const Source, Message: string);
var
  AlertIcon: TIcon;
  IconIndex: Integer;
begin
  // Déterminer l'icône selon le niveau
  case Level of
    alInfo: IconIndex := 0;
    alWarning: IconIndex := 1;
    alError: IconIndex := 2;
    alCritical: IconIndex := 3;
  else
    IconIndex := 0;
  end;

  // Mettre à jour le compteur d'alertes non acquittées dans la barre d'état
  statusBar.Panels[3].Text := Format('Alertes: %d', [FAlertManager.UnacknowledgedCount]);

  // Pour les niveaux d'alerte élevés, afficher une notification système
  if Level >= alWarning then
  begin
    try
      // Créer une icône à partir de la liste d'images
      AlertIcon := TIcon.Create;
      try
        if IconIndex < FAlertsView.ilAlertIcons.Count then
          FAlertsView.ilAlertIcons.GetIcon(IconIndex, AlertIcon);

        // Afficher la notification
        ShowBalloonTip('Alerte ' + Source,
                      Message,
                      AlertIcon,
                      Level >= alError);
      finally
        AlertIcon.Free;
      end;
    except
      // Ignorer les erreurs de notification
    end;
  end;
end;

// Ajouter une méthode pour afficher les notifications système (Windows):
procedure TfrmMainDashboard.ShowBalloonTip(const Title, Message: string; Icon: TIcon; Sound: Boolean);
var
  NotifyIconData: TNotifyIconData;
begin
  ZeroMemory(@NotifyIconData, SizeOf(NotifyIconData));
  NotifyIconData.cbSize := SizeOf(NotifyIconData);
  NotifyIconData.Wnd := Handle;
  NotifyIconData.uID := 1;

  // Configurer l'affichage de l'icône dans la zone de notification
  NotifyIconData.uFlags := NIF_ICON or NIF_TIP or NIF_INFO;
  NotifyIconData.hIcon := Icon.Handle;
  StrLCopy(NotifyIconData.szTip, PChar(Application.Title), Length(NotifyIconData.szTip) - 1);

  // Configurer la bulle de notification
  StrLCopy(NotifyIconData.szInfo, PChar(Message), Length(NotifyIconData.szInfo) - 1);
  StrLCopy(NotifyIconData.szInfoTitle, PChar(Title), Length(NotifyIconData.szInfoTitle) - 1);

  if Sound then
    NotifyIconData.dwInfoFlags := NIIF_INFO or NIIF_NOSOUND
  else
    NotifyIconData.dwInfoFlags := NIIF_INFO;

  // Afficher la notification
  Shell_NotifyIcon(NIM_MODIFY, @NotifyIconData);
end;
```

## Personnalisation et thèmes

Pour rendre notre tableau de bord plus attrayant et personnalisable, ajoutons la prise en charge des thèmes :

```pascal
unit ThemeManager;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, Vcl.Graphics, System.JSON;

type
  TThemeColors = record
    Background: TColor;
    Foreground: TColor;
    Primary: TColor;
    Secondary: TColor;
    Success: TColor;
    Warning: TColor;
    Error: TColor;
    Info: TColor;
    Sidebar: TColor;
    SidebarText: TColor;
    Header: TColor;
    HeaderText: TColor;
    WidgetHeader: TColor;
    WidgetHeaderText: TColor;
    WidgetBackground: TColor;
  end;

  TThemeManager = class
  private
    FThemes: TDictionary<string, TThemeColors>;
    FCurrentTheme: string;
    FOnThemeChanged: TNotifyEvent;

    function GetCurrentColors: TThemeColors;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTheme(const Name: string; const Colors: TThemeColors);
    procedure RemoveTheme(const Name: string);
    function ThemeExists(const Name: string): Boolean;

    procedure ApplyTheme(const Name: string);
    procedure ApplyToForm(Form: TForm);
    procedure ApplyToControl(Control: TControl);

    function SaveToJSON: string;
    procedure LoadFromJSON(const JSONStr: string);

    property CurrentTheme: string read FCurrentTheme;
    property CurrentColors: TThemeColors read GetCurrentColors;
    property OnThemeChanged: TNotifyEvent read FOnThemeChanged write FOnThemeChanged;
  end;

implementation

// ... implémentation des méthodes ...

end.
```

Voici l'implémentation du gestionnaire de thèmes :

```pascal
constructor TThemeManager.Create;
begin
  inherited Create;
  FThemes := TDictionary<string, TThemeColors>.Create;

  // Ajouter des thèmes par défaut

  // Thème clair
  var LightTheme: TThemeColors;
  LightTheme.Background := clWhite;
  LightTheme.Foreground := clBlack;
  LightTheme.Primary := RGB(0, 120, 215);
  LightTheme.Secondary := RGB(170, 170, 170);
  LightTheme.Success := RGB(92, 184, 92);
  LightTheme.Warning := RGB(240, 173, 78);
  LightTheme.Error := RGB(217, 83, 79);
  LightTheme.Info := RGB(91, 192, 222);
  LightTheme.Sidebar := RGB(240, 240, 240);
  LightTheme.SidebarText := clBlack;
  LightTheme.Header := RGB(245, 245, 245);
  LightTheme.HeaderText := clBlack;
  LightTheme.WidgetHeader := RGB(230, 230, 230);
  LightTheme.WidgetHeaderText := clBlack;
  LightTheme.WidgetBackground := clWhite;

  AddTheme('Clair', LightTheme);

  // Thème sombre
  var DarkTheme: TThemeColors;
  DarkTheme.Background := RGB(30, 30, 30);
  DarkTheme.Foreground := clWhite;
  DarkTheme.Primary := RGB(0, 120, 215);
  DarkTheme.Secondary := RGB(100, 100, 100);
  DarkTheme.Success := RGB(92, 184, 92);
  DarkTheme.Warning := RGB(240, 173, 78);
  DarkTheme.Error := RGB(217, 83, 79);
  DarkTheme.Info := RGB(91, 192, 222);
  DarkTheme.Sidebar := RGB(50, 50, 50);
  DarkTheme.SidebarText := clWhite;
  DarkTheme.Header := RGB(40, 40, 40);
  DarkTheme.HeaderText := clWhite;
  DarkTheme.WidgetHeader := RGB(60, 60, 60);
  DarkTheme.WidgetHeaderText := clWhite;
  DarkTheme.WidgetBackground := RGB(45, 45, 45);

  AddTheme('Sombre', DarkTheme);

  // Thème bleu
  var BlueTheme: TThemeColors;
  BlueTheme.Background := RGB(240, 248, 255);
  BlueTheme.Foreground := clBlack;
  BlueTheme.Primary := RGB(0, 102, 204);
  BlueTheme.Secondary := RGB(153, 204, 255);
  BlueTheme.Success := RGB(92, 184, 92);
  BlueTheme.Warning := RGB(240, 173, 78);
  BlueTheme.Error := RGB(217, 83, 79);
  BlueTheme.Info := RGB(91, 192, 222);
  BlueTheme.Sidebar := RGB(204, 229, 255);
  BlueTheme.SidebarText := clBlack;
  BlueTheme.Header := RGB(217, 236, 255);
  BlueTheme.HeaderText := clBlack;
  BlueTheme.WidgetHeader := RGB(179, 217, 255);
  BlueTheme.WidgetHeaderText := clBlack;
  BlueTheme.WidgetBackground := RGB(240, 248, 255);

  AddTheme('Bleu', BlueTheme);

  // Thème vert
  var GreenTheme: TThemeColors;
  GreenTheme.Background := RGB(240, 255, 240);
  GreenTheme.Foreground := clBlack;
  GreenTheme.Primary := RGB(46, 139, 87);
  GreenTheme.Secondary := RGB(152, 251, 152);
  GreenTheme.Success := RGB(92, 184, 92);
  GreenTheme.Warning := RGB(240, 173, 78);
  GreenTheme.Error := RGB(217, 83, 79);
  GreenTheme.Info := RGB(91, 192, 222);
  GreenTheme.Sidebar := RGB(220, 255, 220);
  GreenTheme.SidebarText := clBlack;
  GreenTheme.Header := RGB(230, 255, 230);
  GreenTheme.HeaderText := clBlack;
  GreenTheme.WidgetHeader := RGB(200, 250, 200);
  GreenTheme.WidgetHeaderText := clBlack;
  GreenTheme.WidgetBackground := RGB(240, 255, 240);

  AddTheme('Vert', GreenTheme);

  // Définir le thème par défaut
  FCurrentTheme := 'Clair';
end;

destructor TThemeManager.Destroy;
begin
  FThemes.Free;
  inherited;
end;

function TThemeManager.GetCurrentColors: TThemeColors;
begin
  if FThemes.TryGetValue(FCurrentTheme, Result) then
    Exit
  else if FThemes.Count > 0 then
  begin
    // Utiliser le premier thème disponible si le thème actuel n'existe pas
    for var Theme in FThemes do
    begin
      Result := Theme.Value;
      FCurrentTheme := Theme.Key;
      Break;
    end;
  end
  else
  begin
    // Créer un thème par défaut si aucun n'existe
    Result.Background := clWhite;
    Result.Foreground := clBlack;
    Result.Primary := clNavy;
    Result.Secondary := clSilver;
    Result.Success := clGreen;
    Result.Warning := clOlive;
    Result.Error := clMaroon;
    Result.Info := clTeal;
    Result.Sidebar := clSilver;
    Result.SidebarText := clBlack;
    Result.Header := clSilver;
    Result.HeaderText := clBlack;
    Result.WidgetHeader := clSilver;
    Result.WidgetHeaderText := clBlack;
    Result.WidgetBackground := clWhite;
  end;
end;

procedure TThemeManager.AddTheme(const Name: string; const Colors: TThemeColors);
begin
  FThemes.AddOrSetValue(Name, Colors);
end;

procedure TThemeManager.RemoveTheme(const Name: string);
begin
  if Name <> FCurrentTheme then
    FThemes.Remove(Name);
end;

function TThemeManager.ThemeExists(const Name: string): Boolean;
begin
  Result := FThemes.ContainsKey(Name);
end;

procedure TThemeManager.ApplyTheme(const Name: string);
begin
  if ThemeExists(Name) then
  begin
    FCurrentTheme := Name;

    // Déclencher l'événement de changement de thème
    if Assigned(FOnThemeChanged) then
      FOnThemeChanged(Self);
  end;
end;

procedure TThemeManager.ApplyToForm(Form: TForm);
var
  Colors: TThemeColors;
begin
  Colors := GetCurrentColors;

  // Appliquer les couleurs de base au formulaire
  Form.Color := Colors.Background;
  Form.Font.Color := Colors.Foreground;

  // Parcourir récursivement tous les contrôles du formulaire
  for var I := 0 to Form.ComponentCount - 1 do
  begin
    if Form.Components[I] is TControl then
      ApplyToControl(TControl(Form.Components[I]));
  end;
end;

procedure TThemeManager.ApplyToControl(Control: TControl);
var
  Colors: TThemeColors;
begin
  Colors := GetCurrentColors;

  // Appliquer les couleurs selon le type de contrôle et son nom
  if Control is TPanel then
  begin
    var Panel := TPanel(Control);

    if Panel.Name.Contains('Sidebar') then
    begin
      Panel.Color := Colors.Sidebar;
      Panel.Font.Color := Colors.SidebarText;
    end
    else if Panel.Name.Contains('Header') then
    begin
      Panel.Color := Colors.Header;
      Panel.Font.Color := Colors.HeaderText;
    end
    else if Panel.Name.Contains('Widget') then
    begin
      Panel.Color := Colors.WidgetBackground;
      Panel.Font.Color := Colors.Foreground;
    end
    else
    begin
      Panel.Color := Colors.Background;
      Panel.Font.Color := Colors.Foreground;
    end;
  end
  else if Control is TButton then
  begin
    var Button := TButton(Control);

    Button.Font.Color := Colors.Foreground;
  end
  else if Control is TLabel then
  begin
    var Label1 := TLabel(Control);

    if Label1.Name.Contains('Title') then
      Label1.Font.Color := Colors.Primary
    else
      Label1.Font.Color := Colors.Foreground;
  end
  else if Control is TShape then
  begin
    var Shape := TShape(Control);

    if Shape.Name.Contains('Status') then
    begin
      // Ne pas changer la couleur des indicateurs de statut
    end
    else
    begin
      Shape.Brush.Color := Colors.Secondary;
      Shape.Pen.Color := Colors.Primary;
    end;
  end;

  // Appliquer récursivement aux contrôles enfants
  if Control is TWinControl then
  begin
    var WinControl := TWinControl(Control);

    for var I := 0 to WinControl.ControlCount - 1 do
      ApplyToControl(WinControl.Controls[I]);
  end;
end;

function TThemeManager.SaveToJSON: string;
var
  JSONObj: TJSONObject;
  ThemesObj: TJSONObject;
  Theme: TPair<string, TThemeColors>;
  ColorObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('currentTheme', FCurrentTheme);

    ThemesObj := TJSONObject.Create;
    for Theme in FThemes do
    begin
      ColorObj := TJSONObject.Create;

      ColorObj.AddPair('background', TJSONNumber.Create(Theme.Value.Background));
      ColorObj.AddPair('foreground', TJSONNumber.Create(Theme.Value.Foreground));
      ColorObj.AddPair('primary', TJSONNumber.Create(Theme.Value.Primary));
      ColorObj.AddPair('secondary', TJSONNumber.Create(Theme.Value.Secondary));
      ColorObj.AddPair('success', TJSONNumber.Create(Theme.Value.Success));
      ColorObj.AddPair('warning', TJSONNumber.Create(Theme.Value.Warning));
      ColorObj.AddPair('error', TJSONNumber.Create(Theme.Value.Error));
      ColorObj.AddPair('info', TJSONNumber.Create(Theme.Value.Info));
      ColorObj.AddPair('sidebar', TJSONNumber.Create(Theme.Value.Sidebar));
      ColorObj.AddPair('sidebarText', TJSONNumber.Create(Theme.Value.SidebarText));
      ColorObj.AddPair('header', TJSONNumber.Create(Theme.Value.Header));
      ColorObj.AddPair('headerText', TJSONNumber.Create(Theme.Value.HeaderText));
      ColorObj.AddPair('widgetHeader', TJSONNumber.Create(Theme.Value.WidgetHeader));
      ColorObj.AddPair('widgetHeaderText', TJSONNumber.Create(Theme.Value.WidgetHeaderText));
      ColorObj.AddPair('widgetBackground', TJSONNumber.Create(Theme.Value.WidgetBackground));

      ThemesObj.AddPair(Theme.Key, ColorObj);
    end;

    JSONObj.AddPair('themes', ThemesObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TThemeManager.LoadFromJSON(const JSONStr: string);
var
  JSONObj, ThemesObj, ColorObj: TJSONObject;
  ThemeNames: TArray<string>;
  ThemeName: string;
  ThemeColors: TThemeColors;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    JSONObj.TryGetValue<string>('currentTheme', FCurrentTheme);

    if JSONObj.TryGetValue<TJSONObject>('themes', ThemesObj) then
    begin
      ThemeNames := ThemesObj.GetValueNames;

      for ThemeName in ThemeNames do
      begin
        if ThemesObj.TryGetValue<TJSONObject>(ThemeName, ColorObj) then
        begin
          FillChar(ThemeColors, SizeOf(ThemeColors), 0);

          ColorObj.TryGetValue<TColor>('background', ThemeColors.Background);
          ColorObj.TryGetValue<TColor>('foreground', ThemeColors.Foreground);
          ColorObj.TryGetValue<TColor>('primary', ThemeColors.Primary);
          ColorObj.TryGetValue<TColor>('secondary', ThemeColors.Secondary);
          ColorObj.TryGetValue<TColor>('success', ThemeColors.Success);
          ColorObj.TryGetValue<TColor>('warning', ThemeColors.Warning);
          ColorObj.TryGetValue<TColor>('error', ThemeColors.Error);
          ColorObj.TryGetValue<TColor>('info', ThemeColors.Info);
          ColorObj.TryGetValue<TColor>('sidebar', ThemeColors.Sidebar);
          ColorObj.TryGetValue<TColor>('sidebarText', ThemeColors.SidebarText);
          ColorObj.TryGetValue<TColor>('header', ThemeColors.Header);
          ColorObj.TryGetValue<TColor>('headerText', ThemeColors.HeaderText);
          ColorObj.TryGetValue<TColor>('widgetHeader', ThemeColors.WidgetHeader);
          ColorObj.TryGetValue<TColor>('widgetHeaderText', ThemeColors.WidgetHeaderText);
          ColorObj.TryGetValue<TColor>('widgetBackground', ThemeColors.WidgetBackground);

          AddTheme(ThemeName, ThemeColors);
        end;
      end;
    end;
  finally
    JSONObj.Free;
  end;
end;
```

## Intégration du gestionnaire de thèmes dans le tableau de bord

```pascal
// Dans l'unité MainDashboard, ajouter:
uses
  // ... autres unités ...
  ThemeManager;

// Ajouter une variable membre:
private
  // ... autres variables ...
  FThemeManager: TThemeManager;

// Dans le constructeur:
constructor TfrmMainDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ... code existant ...

  // Créer le gestionnaire de thèmes
  FThemeManager := TThemeManager.Create;
  FThemeManager.OnThemeChanged := HandleThemeChanged;

  // Charger la configuration des thèmes
  LoadThemeConfiguration;

  // Appliquer le thème actuel
  FThemeManager.ApplyToForm(Self);

  // ... reste du code ...
end;

// Ajouter les méthodes de gestion des thèmes:
procedure TfrmMainDashboard.LoadThemeConfiguration;
var
  ConfigFile: string;
  ConfigList: TStringList;
begin
  ConfigFile := ChangeFileExt(Application.ExeName, '.themes');

  if not FileExists(ConfigFile) then
    Exit;

  ConfigList := TStringList.Create;
  try
    ConfigList.LoadFromFile(ConfigFile);
    FThemeManager.LoadFromJSON(ConfigList.Text);
  finally
    ConfigList.Free;
  end;
end;

procedure TfrmMainDashboard.SaveThemeConfiguration;
var
  ConfigFile: string;
  ConfigList: TStringList;
begin
  ConfigFile := ChangeFileExt(Application.ExeName, '.themes');

  ConfigList := TStringList.Create;
  try
    ConfigList.Text := FThemeManager.SaveToJSON;
    ConfigList.SaveToFile(ConfigFile);
  finally
    ConfigList.Free;
  end;
end;

procedure TfrmMainDashboard.HandleThemeChanged(Sender: TObject);
begin
  // Appliquer le thème à tous les contrôles
  FThemeManager.ApplyToForm(Self);

  // Sauvegarder la configuration
  SaveThemeConfiguration;
end;
```

## Création d'une vue de paramètres

Pour permettre à l'utilisateur de personnaliser l'interface, nous allons créer une vue de paramètres :

```pascal
unit SettingsView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, System.JSON, ThemeManager, IoTDataConnection;

type
  TSettingsViewFrame = class(TFrame)
    pgcSettings: TPageControl;
    tabGeneral: TTabSheet;
    tabTheme: TTabSheet;
    tabConnection: TTabSheet;
    tabAdvanced: TTabSheet;
    pnlActions: TPanel;
    btnSave: TButton;
    btnReset: TButton;
    grpTheme: TGroupBox;
    lblTheme: TLabel;
    cmbTheme: TComboBox;
    btnCustomizeTheme: TButton;
    lblPreview: TLabel;
    pnlPreview: TPanel;
    grpConnection: TGroupBox;
    lblBroker: TLabel;
    edtBrokerHost: TEdit;
    lblPort: TLabel;
    edtBrokerPort: TEdit;
    lblPrefix: TLabel;
    edtTopicPrefix: TEdit;
    chkAutoConnect: TCheckBox;
    btnTestConnection: TButton;
    grpGeneral: TGroupBox;
    lblUsername: TLabel;
    edtUsername: TEdit;
    lblLanguage: TLabel;
    cmbLanguage: TComboBox;
    chkStartupDashboard: TCheckBox;
    chkMinimizeToTray: TCheckBox;
    grpAdvanced: TGroupBox;
    lblRefreshInterval: TLabel;
    edtRefreshInterval: TEdit;
    lblHistoryDays: TLabel;
    edtHistoryDays: TEdit;
    chkDebugMode: TCheckBox;
    chkEnableLogging: TCheckBox;
    procedure cmbThemeChange(Sender: TObject);
    procedure btnCustomizeThemeClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnTestConnectionClick(Sender: TObject);
  private
    FThemeManager: TThemeManager;
    FDataConnection: TIoTDataConnection;
    FOnSettingsChanged: TNotifyEvent;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateThemePreview;
    procedure LoadThemeList;
  public
    constructor Create(AOwner: TComponent); override;

    property ThemeManager: TThemeManager read FThemeManager write FThemeManager;
    property DataConnection: TIoTDataConnection read FDataConnection write FDataConnection;
    property OnSettingsChanged: TNotifyEvent read FOnSettingsChanged write FOnSettingsChanged;
  end;

implementation

{$R *.dfm}

// ... implémentation des méthodes ...

end.
```

Voici l'implémentation de la vue des paramètres :

```pascal
constructor TSettingsViewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialiser les langues disponibles
  cmbLanguage.Items.Add('Français');
  cmbLanguage.Items.Add('English');
  cmbLanguage.Items.Add('Español');
  cmbLanguage.Items.Add('Deutsch');
  cmbLanguage.ItemIndex := 0;
end;

procedure TSettingsViewFrame.LoadSettings;
var
  SettingsFile: string;
  SettingsList: TStringList;
  JSONObj: TJSONObject;
begin
  // Charger les paramètres depuis un fichier
  SettingsFile := ChangeFileExt(Application.ExeName, '.settings');

  if not FileExists(SettingsFile) then
  begin
    // Utiliser des paramètres par défaut
    edtUsername.Text := 'Utilisateur';
    cmbLanguage.ItemIndex := 0;
    chkStartupDashboard.Checked := True;
    chkMinimizeToTray.Checked := False;

    edtBrokerHost.Text := 'localhost';
    edtBrokerPort.Text := '1883';
    edtTopicPrefix.Text := 'iot';
    chkAutoConnect.Checked := True;

    edtRefreshInterval.Text := '5';
    edtHistoryDays.Text := '30';
    chkDebugMode.Checked := False;
    chkEnableLogging.Checked := True;

    Exit;
  end;

  SettingsList := TStringList.Create;
  try
    SettingsList.LoadFromFile(SettingsFile);

    JSONObj := TJSONObject.ParseJSONValue(SettingsList.Text) as TJSONObject;
    if not Assigned(JSONObj) then
      Exit;

    try
      // Paramètres généraux
      if JSONObj.TryGetValue<string>('username', edtUsername.Text) then ;

      var LanguageStr: string;
      if JSONObj.TryGetValue<string>('language', LanguageStr) then
      begin
        var Index := cmbLanguage.Items.IndexOf(LanguageStr);
        if Index >= 0 then
          cmbLanguage.ItemIndex := Index;
      end;

      JSONObj.TryGetValue<Boolean>('startupDashboard', chkStartupDashboard.Checked);
      JSONObj.TryGetValue<Boolean>('minimizeToTray', chkMinimizeToTray.Checked);

      // Paramètres de connexion
      JSONObj.TryGetValue<string>('brokerHost', edtBrokerHost.Text);

      var PortStr: string;
      if JSONObj.TryGetValue<string>('brokerPort', PortStr) then
        edtBrokerPort.Text := PortStr;

      JSONObj.TryGetValue<string>('topicPrefix', edtTopicPrefix.Text);
      JSONObj.TryGetValue<Boolean>('autoConnect', chkAutoConnect.Checked);

      // Paramètres avancés
      var RefreshStr: string;
      if JSONObj.TryGetValue<string>('refreshInterval', RefreshStr) then
        edtRefreshInterval.Text := RefreshStr;

      var HistoryStr: string;
      if JSONObj.TryGetValue<string>('historyDays', HistoryStr) then
        edtHistoryDays.Text := HistoryStr;

      JSONObj.TryGetValue<Boolean>('debugMode', chkDebugMode.Checked);
      JSONObj.TryGetValue<Boolean>('enableLogging', chkEnableLogging.Checked);
    finally
      JSONObj.Free;
    end;
  finally
    SettingsList.Free;
  end;
end;

procedure TSettingsViewFrame.SaveSettings;
var
  SettingsFile: string;
  SettingsList: TStringList;
  JSONObj: TJSONObject;
begin
  SettingsFile := ChangeFileExt(Application.ExeName, '.settings');

  JSONObj := TJSONObject.Create;
  try
    // Paramètres généraux
    JSONObj.AddPair('username', edtUsername.Text);
    JSONObj.AddPair('language', cmbLanguage.Items[cmbLanguage.ItemIndex]);
    JSONObj.AddPair('startupDashboard', TJSONBool.Create(chkStartupDashboard.Checked));
    JSONObj.AddPair('minimizeToTray', TJSONBool.Create(chkMinimizeToTray.Checked));

    // Paramètres de connexion
    JSONObj.AddPair('brokerHost', edtBrokerHost.Text);
    JSONObj.AddPair('brokerPort', edtBrokerPort.Text);
    JSONObj.AddPair('topicPrefix', edtTopicPrefix.Text);
    JSONObj.AddPair('autoConnect', TJSONBool.Create(chkAutoConnect.Checked));

    // Paramètres avancés
    JSONObj.AddPair('refreshInterval', edtRefreshInterval.Text);
    JSONObj.AddPair('historyDays', edtHistoryDays.Text);
    JSONObj.AddPair('debugMode', TJSONBool.Create(chkDebugMode.Checked));
    JSONObj.AddPair('enableLogging', TJSONBool.Create(chkEnableLogging.Checked));

    // Sauvegarder dans un fichier
    SettingsList := TStringList.Create;
    try
      SettingsList.Text := JSONObj.ToString;
      SettingsList.SaveToFile(SettingsFile);
    finally
      SettingsList.Free;
    end;

    // Notifier les changements
    if Assigned(FOnSettingsChanged) then
      FOnSettingsChanged(Self);
  finally
    JSONObj.Free;
  end;
end;

procedure TSettingsViewFrame.LoadThemeList;
var
  ThemeNames: TArray<string>;
  ThemeName: string;
begin
  if not Assigned(FThemeManager) then
    Exit;

  // Sauvegarder le thème sélectionné actuel
  var CurrentTheme := '';
  if cmbTheme.ItemIndex >= 0 then
    CurrentTheme := cmbTheme.Items[cmbTheme.ItemIndex];

  // Charger la liste des thèmes disponibles
  cmbTheme.Items.Clear;

  // Méthode fictive pour obtenir la liste des thèmes
  // Dans une implémentation réelle, vous auriez besoin d'une méthode pour récupérer
  // les noms de tous les thèmes disponibles
  ThemeNames := ['Clair', 'Sombre', 'Bleu', 'Vert'];

  for ThemeName in ThemeNames do
    cmbTheme.Items.Add(ThemeName);

  // Restaurer la sélection
  if CurrentTheme <> '' then
  begin
    var Index := cmbTheme.Items.IndexOf(CurrentTheme);
    if Index >= 0 then
      cmbTheme.ItemIndex := Index
    else
      cmbTheme.ItemIndex := 0;
  end
  else
  begin
    // Sélectionner le thème actuel
    var Index := cmbTheme.Items.IndexOf(FThemeManager.CurrentTheme);
    if Index >= 0 then
      cmbTheme.ItemIndex := Index
    else
      cmbTheme.ItemIndex := 0;
  end;

  UpdateThemePreview;
end;

procedure TSettingsViewFrame.UpdateThemePreview;
begin
  if not Assigned(FThemeManager) or (cmbTheme.ItemIndex < 0) then
    Exit;

  var ThemeName := cmbTheme.Items[cmbTheme.ItemIndex];
  var Colors := FThemeManager.CurrentColors;

  // Mettre à jour le panneau de prévisualisation avec les couleurs du thème
  pnlPreview.Color := Colors.Background;
  pnlPreview.Font.Color := Colors.Foreground;

  // Vous pourriez ajouter plus d'éléments dans le panneau pour montrer d'autres couleurs
end;

procedure TSettingsViewFrame.cmbThemeChange(Sender: TObject);
begin
  if not Assigned(FThemeManager) or (cmbTheme.ItemIndex < 0) then
    Exit;

  // Appliquer le thème sélectionné
  var ThemeName := cmbTheme.Items[cmbTheme.ItemIndex];
  FThemeManager.ApplyTheme(ThemeName);

  // Mettre à jour la prévisualisation
  UpdateThemePreview;
end;

procedure TSettingsViewFrame.btnCustomizeThemeClick(Sender: TObject);
begin
  // Ici, vous pourriez ouvrir un formulaire pour personnaliser le thème actuel
  ShowMessage('Fonctionnalité de personnalisation du thème à implémenter');
end;

procedure TSettingsViewFrame.btnTestConnectionClick(Sender: TObject);
var
  Host: string;
  Port: Integer;
  Connected: Boolean;
begin
  if not Assigned(FDataConnection) then
    Exit;

  Host := edtBrokerHost.Text;

  if not TryStrToInt(edtBrokerPort.Text, Port) then
  begin
    ShowMessage('Port invalide!');
    Exit;
  end;

  btnTestConnection.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    // Tester la connexion
    Connected := FDataConnection.TestConnection(Host, Port);

    if Connected then
      ShowMessage('Connexion réussie!')
    else
      ShowMessage('Échec de la connexion. Vérifiez les paramètres ou le broker.');
  finally
    Screen.Cursor := crDefault;
    btnTestConnection.Enabled := True;
  end;
end;

procedure TSettingsViewFrame.btnSaveClick(Sender: TObject);
begin
  // Vérifier les entrées numériques
  var Port, RefreshInterval, HistoryDays: Integer;

  if not TryStrToInt(edtBrokerPort.Text, Port) or (Port <= 0) or (Port > 65535) then
  begin
    ShowMessage('Port invalide! Veuillez entrer un nombre entre 1 et 65535.');
    pgcSettings.ActivePage := tabConnection;
    edtBrokerPort.SetFocus;
    Exit;
  end;

  if not TryStrToInt(edtRefreshInterval.Text, RefreshInterval) or (RefreshInterval <= 0) then
  begin
    ShowMessage('Intervalle de rafraîchissement invalide! Veuillez entrer un nombre positif.');
    pgcSettings.ActivePage := tabAdvanced;
    edtRefreshInterval.SetFocus;
    Exit;
  end;

  if not TryStrToInt(edtHistoryDays.Text, HistoryDays) or (HistoryDays <= 0) then
  begin
    ShowMessage('Nombre de jours d''historique invalide! Veuillez entrer un nombre positif.');
    pgcSettings.ActivePage := tabAdvanced;
    edtHistoryDays.SetFocus;
    Exit;
  end;

  // Sauvegarder les paramètres
  SaveSettings();

  ShowMessage('Paramètres sauvegardés avec succès!');
end;

procedure TSettingsViewFrame.btnResetClick(Sender: TObject);
begin
  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir réinitialiser tous les paramètres?',
               mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Réinitialiser les paramètres
  LoadSettings();

  ShowMessage('Paramètres réinitialisés!');
end;
```

## Intégration de la vue des paramètres dans le tableau de bord principal

```pascal
// Dans l'unité MainDashboard, ajouter:
uses
  // ... autres unités ...
  SettingsView;

// Ajouter une variable membre:
private
  // ... autres variables ...
  FSettingsView: TSettingsViewFrame;

// Dans le constructeur:
constructor TfrmMainDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ... code existant ...

  // Créer la vue des paramètres
  FSettingsView := TSettingsViewFrame.Create(Self);
  FSettingsView.Parent := Self;
  FSettingsView.Align := alClient;
  FSettingsView.Visible := False;
  FSettingsView.ThemeManager := FThemeManager;
  FSettingsView.DataConnection := FDataConnection;
  FSettingsView.OnSettingsChanged := HandleSettingsChanged;

  // ... reste du code ...
end;

// Modifier la méthode SwitchView:
procedure TfrmMainDashboard.SwitchView(const ViewName: string);
begin
  // ... code existant ...

  else if ViewName = 'settings' then
  begin
    FSettingsView.Visible := True;
    lblPageTitle.Caption := 'Paramètres';
    lblPageDescription.Caption := 'Configuration du tableau de bord IoT';

    // Charger les paramètres à jour
    FSettingsView.LoadSettings;
    FSettingsView.LoadThemeList;
  end

  // ... reste du code ...
end;

// Ajouter la méthode de gestion des paramètres:
procedure TfrmMainDashboard.HandleSettingsChanged(Sender: TObject);
var
  SettingsFile: string;
  SettingsList: TStringList;
  JSONObj: TJSONObject;
  Host, Port, Prefix: string;
  AutoConnect: Boolean;
  RefreshInterval: Integer;
begin
  // Charger les nouveaux paramètres
  SettingsFile := ChangeFileExt(Application.ExeName, '.settings');

  if not FileExists(SettingsFile) then
    Exit;

  SettingsList := TStringList.Create;
  try
    SettingsList.LoadFromFile(SettingsFile);

    JSONObj := TJSONObject.ParseJSONValue(SettingsList.Text) as TJSONObject;
    if not Assigned(JSONObj) then
      Exit;

    try
      // Appliquer les paramètres pertinents

      // Nom d'utilisateur
      if JSONObj.TryGetValue<string>('username', Host) then
        lblUsername.Caption := Host;

      // Paramètres de connexion
      if JSONObj.TryGetValue<string>('brokerHost', Host) and
         JSONObj.TryGetValue<string>('brokerPort', Port) and
         JSONObj.TryGetValue<string>('topicPrefix', Prefix) and
         JSONObj.TryGetValue<Boolean>('autoConnect', AutoConnect) then
      begin
        // Reconfigurer la connexion
        if FDataConnection.IsConnected then
          FDataConnection.Disconnect;

        // Enregistrer les paramètres
        FDataConnection.SetConnectionParams(Host, StrToIntDef(Port, 1883), Prefix);

        // Se reconnecter si nécessaire
        if AutoConnect then
          FDataConnection.Connect;
      end;

      // Intervalle de rafraîchissement
      if JSONObj.TryGetValue<Integer>('refreshInterval', RefreshInterval) then
        tmrRefresh.Interval := RefreshInterval * 1000; // Convertir en millisecondes

      // Autres paramètres...

    finally
      JSONObj.Free;
    end;
  finally
    SettingsList.Free;
  end;
end;
```

## Ajout d'une vue d'aide et d'informations

Pour compléter notre tableau de bord, ajoutons une vue d'aide pour guider les utilisateurs :

```pascal
unit HelpView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.pngimage;

type
  THelpViewFrame = class(TFrame)
    pgcHelp: TPageControl;
    tabIntro: TTabSheet;
    tabDevices: TTabSheet;
    tabWidgets: TTabSheet;
    tabAlerts: TTabSheet;
    tabTroubleshooting: TTabSheet;
    tabAbout: TTabSheet;
    pnlIntro: TPanel;
    lblIntroTitle: TLabel;
    imgLogo: TImage;
    mmoIntro: TMemo;
    pnlDevices: TPanel;
    lblDevicesTitle: TLabel;
    mmoDevices: TMemo;
    imgDevices: TImage;
    pnlWidgets: TPanel;
    lblWidgetsTitle: TLabel;
    mmoWidgets: TMemo;
    imgWidgets: TImage;
    pnlAlerts: TPanel;
    lblAlertsTitle: TLabel;
    mmoAlerts: TMemo;
    imgAlerts: TImage;
    pnlTroubleshooting: TPanel;
    lblTroubleshootingTitle: TLabel;
    mmoTroubleshooting: TMemo;
    lvFAQ: TListView;
    pnlAbout: TPanel;
    lblAboutTitle: TLabel;
    imgAboutLogo: TImage;
    lblVersion: TLabel;
    lblCopyright: TLabel;
    mmoCredits: TMemo;
    btnOpenWebsite: TButton;
    btnContactSupport: TButton;
    procedure btnOpenWebsiteClick(Sender: TObject);
    procedure btnContactSupportClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.dfm}

uses
  Winapi.ShellAPI;

constructor THelpViewFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initialiser les textes d'aide
  mmoIntro.Lines.Clear;
  mmoIntro.Lines.Add('Bienvenue dans le tableau de bord IoT!');
  mmoIntro.Lines.Add('');
  mmoIntro.Lines.Add('Ce tableau de bord vous permet de surveiller et contrôler vos dispositifs IoT en temps réel. Voici un aperçu des principales fonctionnalités:');
  mmoIntro.Lines.Add('');
  mmoIntro.Lines.Add('• Vue tableau de bord: Visualisation en temps réel de vos données IoT avec widgets personnalisables');
  mmoIntro.Lines.Add('• Gestion des dispositifs: Ajout, configuration et contrôle de vos dispositifs IoT');
  mmoIntro.Lines.Add('• Alertes: Notification des événements importants et gestion des alertes');
  mmoIntro.Lines.Add('• Paramètres: Personnalisation du tableau de bord selon vos préférences');
  mmoIntro.Lines.Add('');
  mmoIntro.Lines.Add('Utilisez les onglets ci-dessus pour obtenir de l''aide sur des sujets spécifiques.');

  mmoDevices.Lines.Clear;
  mmoDevices.Lines.Add('Gestion des dispositifs IoT');
  mmoDevices.Lines.Add('');
  mmoDevices.Lines.Add('La vue Dispositifs vous permet de:');
  mmoDevices.Lines.Add('');
  mmoDevices.Lines.Add('• Voir tous vos dispositifs IoT connectés');
  mmoDevices.Lines.Add('• Ajouter de nouveaux dispositifs');
  mmoDevices.Lines.Add('• Configurer les paramètres des dispositifs');
  mmoDevices.Lines.Add('• Envoyer des commandes aux dispositifs');
  mmoDevices.Lines.Add('• Surveiller l''état des dispositifs');
  mmoDevices.Lines.Add('');
  mmoDevices.Lines.Add('Pour ajouter un nouveau dispositif, cliquez sur le bouton "Ajouter" dans la vue Dispositifs et suivez les instructions.');

  mmoWidgets.Lines.Clear;
  mmoWidgets.Lines.Add('Widgets du tableau de bord');
  mmoWidgets.Lines.Add('');
  mmoWidgets.Lines.Add('Le tableau de bord utilise des widgets pour visualiser vos données IoT:');
  mmoWidgets.Lines.Add('');
  mmoWidgets.Lines.Add('• Widget de valeur: Affiche une valeur numérique simple avec indicateurs de seuil');
  mmoWidgets.Lines.Add('• Widget de graphique: Affiche l''évolution d''une valeur dans le temps');
  mmoWidgets.Lines.Add('• Widget de jauge: Affiche une valeur sous forme de jauge circulaire');
  mmoWidgets.Lines.Add('• Widget d''état: Affiche et contrôle l''état d''un dispositif');
  mmoWidgets.Lines.Add('');
  mmoWidgets.Lines.Add('Pour ajouter un widget, cliquez sur le bouton "Ajouter Widget" dans la vue tableau de bord.');
  mmoWidgets.Lines.Add('Pour configurer un widget, cliquez sur l''icône d''engrenage dans son coin supérieur droit.');

  mmoAlerts.Lines.Clear;
  mmoAlerts.Lines.Add('Système d''alertes');
  mmoAlerts.Lines.Add('');
  mmoAlerts.Lines.Add('Le système d''alertes vous informe des événements importants:');
  mmoAlerts.Lines.Add('');
  mmoAlerts.Lines.Add('• Alertes de niveau Information: Événements informatifs normaux');
  mmoAlerts.Lines.Add('• Alertes de niveau Avertissement: Situations qui méritent attention');
  mmoAlerts.Lines.Add('• Alertes de niveau Erreur: Problèmes nécessitant une intervention');
  mmoAlerts.Lines.Add('• Alertes de niveau Critique: Situations urgentes requérant une action immédiate');
  mmoAlerts.Lines.Add('');
  mmoAlerts.Lines.Add('Les alertes non acquittées apparaissent dans la barre d''état et peuvent déclencher des notifications.');
  mmoAlerts.Lines.Add('Pour gérer vos alertes, accédez à la vue Alertes depuis le menu principal.');

  mmoTroubleshooting.Lines.Clear;
  mmoTroubleshooting.Lines.Add('Problèmes courants et solutions');
  mmoTroubleshooting.Lines.Add('');
  mmoTroubleshooting.Lines.Add('Si vous rencontrez des problèmes avec le tableau de bord, consultez ces solutions:');

  // Initialiser la liste FAQ
  lvFAQ.ViewStyle := vsReport;

  with lvFAQ.Columns.Add do
  begin
    Caption := 'Problème';
    Width := 300;
  end;

  with lvFAQ.Columns.Add do
  begin
    Caption := 'Solution';
    Width := 500;
  end;

  var Item: TListItem;

  Item := lvFAQ.Items.Add;
  Item.Caption := 'Impossible de se connecter au broker MQTT';
  Item.SubItems.Add('Vérifiez que le broker est en cours d''exécution, que l''adresse et le port sont corrects, et qu''aucun pare-feu ne bloque la connexion.');

  Item := lvFAQ.Items.Add;
  Item.Caption := 'Les widgets ne se mettent pas à jour';
  Item.SubItems.Add('Assurez-vous que les dispositifs envoient des données et que vous êtes abonné aux bons topics. Vérifiez l''ID du capteur dans la configuration du widget.');

  Item := lvFAQ.Items.Add;
  Item.Caption := 'Les alertes ne s''affichent pas';
  Item.SubItems.Add('Vérifiez que le système d''alertes est activé dans les paramètres et que les filtres d''alertes n''excluent pas les notifications que vous attendez.');

  Item := lvFAQ.Items.Add;
  Item.Caption := 'L''application est lente';
  Item.SubItems.Add('Réduisez le nombre de widgets sur le tableau de bord, augmentez l''intervalle de rafraîchissement, ou limitez l''historique des données.');

  Item := lvFAQ.Items.Add;
  Item.Caption := 'Les commandes aux dispositifs échouent';
  Item.SubItems.Add('Vérifiez que le dispositif est en ligne, que le format de commande est correct, et que les permissions sont suffisantes.');

  // Informations sur l'application
  lblVersion.Caption := 'Version 1.0.0';
  lblCopyright.Caption := 'Copyright © 2025 Votre Entreprise';

  mmoCredits.Lines.Clear;
  mmoCredits.Lines.Add('Développé avec Delphi 12 Athens');
  mmoCredits.Lines.Add('');
  mmoCredits.Lines.Add('Bibliothèques utilisées:');
  mmoCredits.Lines.Add('• MQTT Client Library');
  mmoCredits.Lines.Add('• VCL Styles');
  mmoCredits.Lines.Add('• TeeChart');
  mmoCredits.Lines.Add('• JSON Library');
  mmoCredits.Lines.Add('');
  mmoCredits.Lines.Add('Remerciements à la communauté Delphi pour son soutien!');
end;

procedure THelpViewFrame.btnOpenWebsiteClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'https://www.votre-site-web.com', nil, nil, SW_SHOWNORMAL);
end;

procedure THelpViewFrame.btnContactSupportClick(Sender: TObject);
begin
  ShellExecute(0, 'open', 'mailto:support@votre-entreprise.com?subject=Support%20Tableau%20de%20Bord%20IoT', nil, nil, SW_SHOWNORMAL);
end;
```

## Intégration de la vue d'aide dans le tableau de bord principal

```pascal
// Dans l'unité MainDashboard, ajouter:
uses
  // ... autres unités ...
  HelpView;

// Ajouter une variable membre:
private
  // ... autres variables ...
  FHelpView: THelpViewFrame;

// Ajouter un bouton d'aide dans la barre d'outils ou le menu

// Dans le constructeur:
constructor TfrmMainDashboard.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ... code existant ...

  // Créer la vue d'aide
  FHelpView := THelpViewFrame.Create(Self);
  FHelpView.Parent := Self;
  FHelpView.Align := alClient;
  FHelpView.Visible := False;

  // ... reste du code ...

  // Ajouter un bouton d'aide dans le menu
  mnuHelp := TMenuItem.Create(mainMenu);
  mnuHelp.Caption := 'Aide';
  mainMenu.Items.Add(mnuHelp);

  mnuHelpContents := TMenuItem.Create(mnuHelp);
  mnuHelpContents.Caption := 'Contenu de l''aide';
  mnuHelpContents.OnClick := mnuHelpContentsClick;
  mnuHelp.Add(mnuHelpContents);

  mnuHelpAbout := TMenuItem.Create(mnuHelp);
  mnuHelpAbout.Caption := 'À propos';
  mnuHelpAbout.OnClick := mnuHelpAboutClick;
  mnuHelp.Add(mnuHelpAbout);
end;

// Ajouter les méthodes pour gérer les clics sur le menu d'aide:
procedure TfrmMainDashboard.mnuHelpContentsClick(Sender: TObject);
begin
  // Afficher la vue d'aide
  SwitchView('help');
end;

procedure TfrmMainDashboard.mnuHelpAboutClick(Sender: TObject);
begin
  // Afficher l'onglet À propos dans la vue d'aide
  SwitchView('help');
  FHelpView.pgcHelp.ActivePage := FHelpView.tabAbout;
end;

// Modifier la méthode SwitchView:
procedure TfrmMainDashboard.SwitchView(const ViewName: string);
begin
  // Masquer toutes les vues
  pnlDashboardContainer.Visible := False;
  FDevicesView.Visible := False;
  FAlertsView.Visible := False;
  FSettingsView.Visible := False;
  FHelpView.Visible := False;

  // Afficher la vue appropriée
  if ViewName = 'dashboard' then
  begin
    pnlDashboardContainer.Visible := True;
    lblPageTitle.Caption := 'Tableau de bord';
    lblPageDescription.Caption := 'Visualisation en temps réel des données IoT';
  end
  else if ViewName = 'devices' then
  begin
    FDevicesView.Visible := True;
    lblPageTitle.Caption := 'Gestion des dispositifs';
    lblPageDescription.Caption := 'Configurer et contrôler vos dispositifs IoT';

    // Rafraîchir la liste des dispositifs
    FDevicesView.RefreshDeviceList;
  end
  else if ViewName = 'alerts' then
  begin
    FAlertsView.Visible := True;
    lblPageTitle.Caption := 'Alertes';
    lblPageDescription.Caption := 'Gestion des alertes et notifications';

    // Rafraîchir la liste des alertes
    FAlertsView.RefreshAlertList;
  end
  else if ViewName = 'settings' then
  begin
    FSettingsView.Visible := True;
    lblPageTitle.Caption := 'Paramètres';
    lblPageDescription.Caption := 'Configuration du tableau de bord IoT';

    // Charger les paramètres à jour
    FSettingsView.LoadSettings;
    FSettingsView.LoadThemeList;
  end
  else if ViewName = 'help' then
  begin
    FHelpView.Visible := True;
    lblPageTitle.Caption := 'Aide';
    lblPageDescription.Caption := 'Guide d''utilisation du tableau de bord IoT';
  end;

  // Mettre à jour l'état des boutons de navigation
  sbtnDashboard.Down := (ViewName = 'dashboard');
  sbtnDevices.Down := (ViewName = 'devices');
  sbtnAlerts.Down := (ViewName = 'alerts');
  sbtnSettings.Down := (ViewName = 'settings');

  // Enregistrer la vue actuelle
  FCurrentView := ViewName;
end;
```

## Finalisation et packaging

Pour terminer notre tableau de bord IoT, nous allons ajouter quelques touches finales et préparer l'application pour la distribution.

### Icônes et ressources visuelles

Ajoutez des icônes et des ressources visuelles attrayantes pour votre application :

```pascal
// Dans l'événement FormCreate, après l'initialisation des composants:
procedure TfrmMainDashboard.FormCreate(Sender: TObject);
begin
  // ... code existant ...

  // Charger l'icône de l'application
  Application.Icon.LoadFromFile('assets\app_icon.ico');

  // Charger le logo
  imgLogo.Picture.LoadFromFile('assets\logo.png');

  // ... reste du code ...
end;
```

### Écran de démarrage

Créez un écran de démarrage pour améliorer l'expérience utilisateur :

```pascal
unit SplashScreen;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Imaging.pngimage;

type
  TfrmSplash = class(TForm)
    pnlBackground: TPanel;
    imgLogo: TImage;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblLoading: TLabel;
    prgLoading: TProgressBar;
    tmrClose: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrCloseTimer(Sender: TObject);
  private
    FProgress: Integer;
  public
    procedure UpdateProgress(const Status: string; Progress: Integer);
  end;

var
  frmSplash: TfrmSplash;

implementation

{$R *.dfm}

procedure TfrmSplash.FormCreate(Sender: TObject);
begin
  // Configurer le formulaire
  BorderStyle := bsNone;
  Position := poScreenCenter;

  // Initialiser la barre de progression
  prgLoading.Position := 0;
  FProgress := 0;

  // Configurer le timer
  tmrClose.Enabled := False;

  // Afficher la version
  lblVersion.Caption := 'Version 1.0.0';
end;

procedure TfrmSplash.UpdateProgress(const Status: string; Progress: Integer);
begin
  // Mettre à jour le texte et la progression
  lblLoading.Caption := Status;
  FProgress := Progress;
  prgLoading.Position := Progress;

  // Rafraîchir le formulaire
  Application.ProcessMessages;

  // Si la progression est complète, démarrer le timer de fermeture
  if Progress >= 100 then
    tmrClose.Enabled := True;
end;

procedure TfrmSplash.tmrCloseTimer(Sender: TObject);
begin
  // Fermer l'écran de démarrage
  Close;
end;
```

Dans le fichier projet (.dpr), ajoutez le code pour afficher l'écran de démarrage :

```pascal
program IoTDashboard;

uses
  Vcl.Forms,
  MainDashboard in 'MainDashboard.pas' {frmMainDashboard},
  SplashScreen in 'SplashScreen.pas' {frmSplash},
  // ... autres unités ...

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Tableau de Bord IoT';

  // Afficher l'écran de démarrage
  frmSplash := TfrmSplash.Create(nil);
  frmSplash.Show;
  frmSplash.Update;

  // Initialiser l'application
  frmSplash.UpdateProgress('Initialisation...', 10);
  Sleep(300); // Simuler un chargement

  // Charger les composants
  frmSplash.UpdateProgress('Chargement des composants...', 30);
  Sleep(300);

  // Charger les paramètres
  frmSplash.UpdateProgress('Chargement des paramètres...', 50);
  Sleep(300);

  // Connexion au broker
  frmSplash.UpdateProgress('Connexion au broker MQTT...', 70);
  Sleep(300);

  // Finalisation
  frmSplash.UpdateProgress('Finalisation...', 90);
  Sleep(300);

  // Créer le formulaire principal
  Application.CreateForm(TfrmMainDashboard, frmMainDashboard);

  // Terminer le chargement
  frmSplash.UpdateProgress('Prêt!', 100);

  Application.Run;
end.
```

### Création d'un installateur

Pour distribuer votre application, vous pouvez utiliser un outil comme Inno Setup pour créer un installateur :

```pascal
; Script Inno Setup pour le Tableau de Bord IoT

#define MyAppName "Tableau de Bord IoT"
#define MyAppVersion "1.0.0"
#define MyAppPublisher "Votre Entreprise"
#define MyAppURL "https://www.votre-site-web.com"
#define MyAppExeName "IoTDashboard.exe"

[Setup]
AppId={{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputDir=dist
OutputBaseFilename=IoTDashboard_Setup
Compression=lzma
SolidCompression=yes
WizardStyle=modern

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 6.1; Check: not IsAdminInstallMode

[Files]
Source: "bin\Release\{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
Source: "bin\Release\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "assets\*"; DestDir: "{app}\assets"; Flags: ignoreversion recursesubdirs createallsubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
```

## Conclusion

Félicitations ! Vous avez créé un tableau de bord IoT complet avec Delphi. Cette application vous permet de :

1. **Visualiser en temps réel** les données de vos dispositifs IoT avec des widgets personnalisables
2. **Gérer vos dispositifs** en les ajoutant, configurant et contrôlant
3. **Surveiller les alertes** pour être informé des événements importants
4. **Personnaliser l'interface** selon vos préférences avec différents thèmes
5. **Accéder à une aide contextuelle** pour vous guider dans l'utilisation

Ce tableau de bord est une base solide que vous pouvez étendre selon vos besoins spécifiques, par exemple en ajoutant :

- Des types de widgets supplémentaires
- Des visualisations avancées (cartes, diagrammes complexes)
- Des analyses prédictives
- Des rapports automatiques
- Des intégrations avec d'autres systèmes

## Ressources supplémentaires

Pour aller plus loin avec votre tableau de bord IoT, voici quelques ressources utiles :

- [Documentation MQTT](https://mqtt.org/)
- [TMS Components](https://www.tmssoftware.com/) - Bibliothèques de composants visuels pour Delphi
- [DevExpress VCL](https://www.devexpress.com/products/vcl/) - Composants avancés pour tableaux de bord
- [TeeChart](https://www.steema.com/product/vcl) - Bibliothèque de graphiques professionnels
- [Embarcadero Community](https://community.embarcadero.com/) - Forums et ressources pour développeurs Delphi

Ce tutoriel vous a guidé à travers la création d'un tableau de bord IoT fonctionnel et professionnel. N'hésitez pas à explorer davantage pour adapter cette solution à vos besoins spécifiques et créer des applications IoT encore plus puissantes avec Delphi!
