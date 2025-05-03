# 22.5 Développement de modèles prédictifs

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction aux modèles prédictifs

Les modèles prédictifs sont des outils puissants qui permettent à vos applications de faire des prévisions basées sur des données historiques. Dans cette section, nous allons explorer comment intégrer cette capacité à vos applications Delphi.

## Qu'est-ce qu'un modèle prédictif ?

Un modèle prédictif est un algorithme qui utilise des données historiques pour prévoir des résultats futurs ou inconnus. Ces modèles peuvent aider à :

- Prévoir des ventes futures
- Anticiper les comportements des utilisateurs
- Détecter des anomalies ou des fraudes
- Estimer des valeurs manquantes
- Classer automatiquement des données

## Approches pour intégrer des modèles prédictifs dans Delphi

### 1. Utilisation de bibliothèques statistiques intégrées

Pour les modèles simples, vous pouvez implémenter des algorithmes statistiques directement en Delphi :

```delphi
// Implémentation d'une régression linéaire simple
type
  TLinearRegression = class
  private
    FSlope: Double;     // Pente (coefficient directeur)
    FIntercept: Double; // Ordonnée à l'origine

    procedure CalculateCoefficients(const X, Y: array of Double);
  public
    procedure Train(const X, Y: array of Double);
    function Predict(X: Double): Double;

    property Slope: Double read FSlope;
    property Intercept: Double read FIntercept;
  end;

procedure TLinearRegression.Train(const X, Y: array of Double);
begin
  if Length(X) <> Length(Y) then
    raise Exception.Create('Les tableaux X et Y doivent avoir la même taille');

  if Length(X) < 2 then
    raise Exception.Create('Au moins deux points sont nécessaires');

  CalculateCoefficients(X, Y);
end;

procedure TLinearRegression.CalculateCoefficients(const X, Y: array of Double);
var
  SumX, SumY, SumXY, SumXX: Double;
  N, i: Integer;
begin
  // Initialisation des variables
  SumX := 0;
  SumY := 0;
  SumXY := 0;
  SumXX := 0;
  N := Length(X);

  // Calcul des sommes
  for i := 0 to N - 1 do
  begin
    SumX := SumX + X[i];
    SumY := SumY + Y[i];
    SumXY := SumXY + X[i] * Y[i];
    SumXX := SumXX + X[i] * X[i];
  end;

  // Calcul de la pente
  FSlope := (N * SumXY - SumX * SumY) / (N * SumXX - SumX * SumX);

  // Calcul de l'ordonnée à l'origine
  FIntercept := (SumY - FSlope * SumX) / N;
end;

function TLinearRegression.Predict(X: Double): Double;
begin
  Result := FSlope * X + FIntercept;
end;
```

Utilisation de cette classe de régression linéaire :

```delphi
procedure TFormPrediction.ButtonTrainClick(Sender: TObject);
var
  X, Y: array of Double;
  i: Integer;
  Model: TLinearRegression;
begin
  // Préparation des données d'entraînement
  SetLength(X, 10);
  SetLength(Y, 10);

  // Exemple de données (année et ventes)
  X[0] := 2010; Y[0] := 100;
  X[1] := 2011; Y[1] := 120;
  X[2] := 2012; Y[2] := 140;
  X[3] := 2013; Y[3] := 180;
  X[4] := 2014; Y[4] := 200;
  X[5] := 2015; Y[5] := 230;
  X[6] := 2016; Y[6] := 270;
  X[7] := 2017; Y[7] := 300;
  X[8] := 2018; Y[8] := 350;
  X[9] := 2019; Y[9] := 390;

  // Création et entraînement du modèle
  Model := TLinearRegression.Create;
  try
    Model.Train(X, Y);

    // Affichage des coefficients du modèle
    MemoResults.Lines.Add('Modèle de régression linéaire entraîné :');
    MemoResults.Lines.Add(Format('Pente (coefficient directeur) : %.4f', [Model.Slope]));
    MemoResults.Lines.Add(Format('Ordonnée à l''origine : %.4f', [Model.Intercept]));
    MemoResults.Lines.Add('');

    // Prédictions pour les années futures
    MemoResults.Lines.Add('Prédictions :');
    for i := 2020 to 2025 do
      MemoResults.Lines.Add(Format('Année %d : %.2f', [i, Model.Predict(i)]));

    // Représentation graphique
    Chart1.Series[0].Clear;
    Chart1.Series[1].Clear;

    // Tracé des données historiques
    for i := 0 to 9 do
      Chart1.Series[0].AddXY(X[i], Y[i], '', clBlue);

    // Tracé de la ligne de régression et des prédictions
    for i := 2010 to 2025 do
      Chart1.Series[1].AddXY(i, Model.Predict(i), '', clRed);

  finally
    Model.Free;
  end;
end;
```

### 2. Utilisation de bibliothèques externes via DLL

Pour des algorithmes plus complexes, vous pouvez utiliser des bibliothèques spécialisées via des DLLs :

```delphi
// Déclaration des fonctions de la DLL
function ML_CreateModel(ModelType: PAnsiChar): Integer; cdecl; external 'ml_engine.dll';
function ML_TrainModel(ModelHandle: Integer; DataPtr: Pointer; RowCount, ColCount: Integer): Boolean; cdecl; external 'ml_engine.dll';
function ML_Predict(ModelHandle: Integer; Features: Pointer; FeaturesCount: Integer; Result: PDouble): Boolean; cdecl; external 'ml_engine.dll';
procedure ML_DestroyModel(ModelHandle: Integer); cdecl; external 'ml_engine.dll';

type
  TExternalMLModel = class
  private
    FModelHandle: Integer;
    FModelType: string;
    FIsValid: Boolean;
  public
    constructor Create(const ModelType: string);
    destructor Destroy; override;

    function Train(const Data: TArray<TArray<Double>>; const Target: TArray<Double>): Boolean;
    function Predict(const Features: TArray<Double>): Double;

    property IsValid: Boolean read FIsValid;
  end;

constructor TExternalMLModel.Create(const ModelType: string);
begin
  inherited Create;
  FModelType := ModelType;
  FModelHandle := ML_CreateModel(PAnsiChar(AnsiString(ModelType)));
  FIsValid := FModelHandle > 0;
end;

destructor TExternalMLModel.Destroy;
begin
  if FIsValid then
    ML_DestroyModel(FModelHandle);
  inherited;
end;

function TExternalMLModel.Train(const Data: TArray<TArray<Double>>; const Target: TArray<Double>): Boolean;
var
  TrainingData: TArray<Double>;
  RowCount, ColCount, i, j, idx: Integer;
begin
  if not FIsValid then
    Exit(False);

  // Vérifier les dimensions
  RowCount := Length(Data);
  if RowCount = 0 then
    Exit(False);

  ColCount := Length(Data[0]);
  if Length(Target) <> RowCount then
    Exit(False);

  // Préparer les données d'entraînement dans un tableau plat
  SetLength(TrainingData, RowCount * (ColCount + 1));

  idx := 0;
  for i := 0 to RowCount - 1 do
  begin
    // Caractéristiques
    for j := 0 to ColCount - 1 do
    begin
      TrainingData[idx] := Data[i][j];
      Inc(idx);
    end;

    // Valeur cible
    TrainingData[idx] := Target[i];
    Inc(idx);
  end;

  // Appeler la fonction d'entraînement
  Result := ML_TrainModel(FModelHandle, @TrainingData[0], RowCount, ColCount + 1);
end;

function TExternalMLModel.Predict(const Features: TArray<Double>): Double;
var
  ResultValue: Double;
begin
  if not FIsValid then
    Exit(0);

  if ML_Predict(FModelHandle, @Features[0], Length(Features), @ResultValue) then
    Result := ResultValue
  else
    Result := 0;
end;
```

> 🔹 **Note** : Cet exemple suppose l'existence d'une DLL `ml_engine.dll` qui met en œuvre les fonctionnalités d'apprentissage automatique. Dans la pratique, il faudrait créer cette DLL ou en utiliser une existante.

### 3. Utilisation de Python pour l'apprentissage automatique

L'approche la plus flexible consiste à utiliser Python avec des bibliothèques comme scikit-learn ou TensorFlow, et à communiquer avec ce code depuis Delphi :

```delphi
procedure TFormML.TrainModelWithPython(const DataPath: string; const ModelType: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('train_model.py');
    Process.Parameters.Add(DataPath);
    Process.Parameters.Add(ModelType);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    StatusBar1.SimpleText := 'Entraînement du modèle en cours...';
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    if Process.ExitCode = 0 then
    begin
      MemoResults.Lines.Add('Modèle entraîné avec succès !');
      MemoResults.Lines.Add('');
      MemoResults.Lines.Add('Résultats de validation :');

      for var i := 0 to Output.Count - 1 do
        MemoResults.Lines.Add(Output[i]);

      // Activer le bouton de prédiction
      ButtonPredict.Enabled := True;
      StatusBar1.SimpleText := 'Modèle prêt';
    end
    else
    begin
      MemoResults.Lines.Add('Erreur lors de l''entraînement :');
      MemoResults.Lines.Add(Output.Text);
      StatusBar1.SimpleText := 'Erreur';
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormML.PredictWithPython(const InputValues: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('predict.py');
    Process.Parameters.Add(InputValues);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    if (Process.ExitCode = 0) and (Output.Count > 0) then
    begin
      LabelPrediction.Caption := 'Prédiction : ' + Output[0];
    end
    else
    begin
      LabelPrediction.Caption := 'Erreur de prédiction';
      ShowMessage('Erreur lors de la prédiction : ' + Output.Text);
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormML.ButtonTrainClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    var ModelType := 'random_forest';  // Par défaut, on utilise Random Forest

    // Déterminer le type de modèle en fonction de la sélection de l'utilisateur
    if RadioButtonLinear.Checked then
      ModelType := 'linear'
    else if RadioButtonSVM.Checked then
      ModelType := 'svm'
    else if RadioButtonGradientBoosting.Checked then
      ModelType := 'gradient_boosting';

    TrainModelWithPython(OpenDialog1.FileName, ModelType);
  end;
end;

procedure TFormML.ButtonPredictClick(Sender: TObject);
var
  InputValues: string;
begin
  // Récupérer les valeurs saisies par l'utilisateur
  InputValues := EditFeature1.Text + ',' +
                 EditFeature2.Text + ',' +
                 EditFeature3.Text + ',' +
                 EditFeature4.Text;

  PredictWithPython(InputValues);
end;
```

Voici un exemple de script Python (train_model.py) pour l'entraînement :

```python
import sys
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.svm import SVR
from sklearn.metrics import mean_squared_error, r2_score
import pickle

def train_model(data_path, model_type):
    # Charger les données
    data = pd.read_csv(data_path)

    # Supposons que la dernière colonne contient les valeurs cibles
    X = data.iloc[:, :-1].values  # Caractéristiques
    y = data.iloc[:, -1].values   # Valeurs cibles

    # Diviser les données en ensembles d'entraînement et de test
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Sélectionner le modèle en fonction du type spécifié
    if model_type == 'linear':
        model = LinearRegression()
    elif model_type == 'svm':
        model = SVR(kernel='rbf')
    elif model_type == 'gradient_boosting':
        model = GradientBoostingRegressor(n_estimators=100, random_state=42)
    else:  # 'random_forest' par défaut
        model = RandomForestRegressor(n_estimators=100, random_state=42)

    # Entraîner le modèle
    model.fit(X_train, y_train)

    # Faire des prédictions sur l'ensemble de test
    y_pred = model.predict(X_test)

    # Évaluer les performances
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)

    # Afficher les résultats
    print(f"Type de modèle : {model_type}")
    print(f"Erreur quadratique moyenne (MSE) : {mse:.4f}")
    print(f"Coefficient de détermination (R²) : {r2:.4f}")

    # Sauvegarder le modèle
    with open('trained_model.pkl', 'wb') as f:
        pickle.dump(model, f)

    # Sauvegarder également les colonnes pour référence future
    with open('model_columns.pkl', 'wb') as f:
        pickle.dump(list(data.columns[:-1]), f)

    return 0  # Succès

if __name__ == "__main__":
    if len(sys.argv) > 2:
        exit(train_model(sys.argv[1], sys.argv[2]))
    else:
        print("Usage: python train_model.py <data_path> <model_type>")
        exit(1)
```

Et le script Python pour la prédiction (predict.py) :

```python
import sys
import pickle
import numpy as np

def predict(input_values):
    try:
        # Charger le modèle
        with open('trained_model.pkl', 'rb') as f:
            model = pickle.load(f)

        # Charger les noms de colonnes
        with open('model_columns.pkl', 'rb') as f:
            model_columns = pickle.load(f)

        # Convertir la chaîne d'entrée en tableau numpy
        values = [float(x) for x in input_values.split(',')]

        # Vérifier que nous avons le bon nombre de caractéristiques
        if len(values) != len(model_columns):
            print(f"Erreur: {len(values)} valeurs fournies, {len(model_columns)} attendues")
            return 1

        # Faire la prédiction
        features = np.array(values).reshape(1, -1)
        prediction = model.predict(features)[0]

        # Afficher la prédiction
        print(f"{prediction:.2f}")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(predict(sys.argv[1]))
    else:
        print("Usage: python predict.py <input_values>")
        exit(1)
```

## Exemple pratique : Prédiction des ventes

Créons une application complète pour prédire les ventes futures :

```delphi
unit SalesPrediction;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, VclTee.TeeGDIPlus, VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs,
  VclTee.Chart, VclTee.DBChart, Data.DB, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids;

type
  TFormSalesPrediction = class(TForm)
    PanelControls: TPanel;
    PanelChart: TPanel;
    PanelData: TPanel;
    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TLineSeries;
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ButtonTrain: TButton;
    ButtonPredict: TButton;
    GroupBoxFeatures: TGroupBox;
    LabelMonth: TLabel;
    ComboBoxMonth: TComboBox;
    LabelYear: TLabel;
    EditYear: TEdit;
    LabelHoliday: TLabel;
    CheckBoxHoliday: TCheckBox;
    LabelPromotion: TLabel;
    ComboBoxPromotion: TComboBox;
    GroupBoxResults: TGroupBox;
    LabelPrediction: TLabel;
    PanelModelInfo: TPanel;
    MemoModelInfo: TMemo;
    ButtonLoad: TButton;
    ButtonSave: TButton;
    StatusBar1: TStatusBar;
    ButtonClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonTrainClick(Sender: TObject);
    procedure ButtonPredictClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
  private
    procedure SetupData;
    procedure TrainModel;
    procedure PredictSales;
    function EncodeMonth(const Month: string): Integer;
    function EncodePromotion(const Promotion: string): Integer;
    procedure UpdateChart;
  public
    { Public declarations }
  end;

var
  FormSalesPrediction: TFormSalesPrediction;

implementation

{$R *.dfm}

procedure TFormSalesPrediction.FormCreate(Sender: TObject);
begin
  // Initialisation des contrôles
  ComboBoxMonth.Items.Clear;
  ComboBoxMonth.Items.AddStrings(['Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin',
                                 'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre']);
  ComboBoxMonth.ItemIndex := 0;

  ComboBoxPromotion.Items.Clear;
  ComboBoxPromotion.Items.AddStrings(['Aucune', 'Faible', 'Moyenne', 'Forte']);
  ComboBoxPromotion.ItemIndex := 0;

  EditYear.Text := '2025';

  // Préparer les données
  SetupData;

  // Initialiser le graphique
  UpdateChart;

  // Désactiver le bouton de prédiction tant qu'aucun modèle n'est entraîné
  ButtonPredict.Enabled := False;
end;

procedure TFormSalesPrediction.SetupData;
begin
  // Créer la structure des données
  ClientDataSet1.Close;
  ClientDataSet1.FieldDefs.Clear;
  ClientDataSet1.FieldDefs.Add('Year', ftInteger);
  ClientDataSet1.FieldDefs.Add('Month', ftInteger);
  ClientDataSet1.FieldDefs.Add('Holiday', ftBoolean);
  ClientDataSet1.FieldDefs.Add('Promotion', ftInteger);
  ClientDataSet1.FieldDefs.Add('Sales', ftFloat);
  ClientDataSet1.CreateDataSet;

  // Ajouter quelques données historiques fictives
  ClientDataSet1.AppendRecord([2023, 1, False, 0, 10500.0]);
  ClientDataSet1.AppendRecord([2023, 2, False, 1, 11200.0]);
  ClientDataSet1.AppendRecord([2023, 3, False, 0, 12100.0]);
  ClientDataSet1.AppendRecord([2023, 4, True, 2, 15300.0]);
  ClientDataSet1.AppendRecord([2023, 5, False, 1, 13200.0]);
  ClientDataSet1.AppendRecord([2023, 6, False, 0, 12800.0]);
  ClientDataSet1.AppendRecord([2023, 7, False, 1, 13500.0]);
  ClientDataSet1.AppendRecord([2023, 8, False, 2, 14200.0]);
  ClientDataSet1.AppendRecord([2023, 9, False, 1, 13800.0]);
  ClientDataSet1.AppendRecord([2023, 10, False, 0, 12900.0]);
  ClientDataSet1.AppendRecord([2023, 11, True, 3, 18500.0]);
  ClientDataSet1.AppendRecord([2023, 12, True, 3, 22300.0]);

  ClientDataSet1.AppendRecord([2024, 1, False, 0, 11200.0]);
  ClientDataSet1.AppendRecord([2024, 2, False, 1, 12000.0]);
  ClientDataSet1.AppendRecord([2024, 3, True, 2, 16800.0]);
  ClientDataSet1.AppendRecord([2024, 4, False, 1, 14200.0]);
  ClientDataSet1.AppendRecord([2024, 5, False, 0, 13500.0]);
  ClientDataSet1.AppendRecord([2024, 6, False, 1, 14100.0]);
  ClientDataSet1.AppendRecord([2024, 7, False, 2, 15600.0]);
  ClientDataSet1.AppendRecord([2024, 8, False, 3, 17200.0]);
  ClientDataSet1.AppendRecord([2024, 9, False, 1, 14500.0]);
  ClientDataSet1.AppendRecord([2024, 10, True, 2, 17300.0]);

  // On s'arrête à octobre 2024 (les deux derniers mois sont inconnus)
end;

procedure TFormSalesPrediction.ButtonTrainClick(Sender: TObject);
begin
  TrainModel;

  // Activer le bouton de prédiction
  ButtonPredict.Enabled := True;

  StatusBar1.SimpleText := 'Modèle entraîné avec succès';
end;

procedure TFormSalesPrediction.TrainModel;
var
  Process: TProcess;
  Output: TStringList;
  CSVFile: TStringList;
begin
  // Créer un fichier CSV temporaire avec les données
  CSVFile := TStringList.Create;
  try
    // En-tête du CSV
    CSVFile.Add('Year,Month,Holiday,Promotion,Sales');

    // Ajouter les données
    ClientDataSet1.First;
    while not ClientDataSet1.Eof do
    begin
      CSVFile.Add(
        ClientDataSet1.FieldByName('Year').AsString + ',' +
        ClientDataSet1.FieldByName('Month').AsString + ',' +
        BoolToStr(ClientDataSet1.FieldByName('Holiday').AsBoolean, True) + ',' +
        ClientDataSet1.FieldByName('Promotion').AsString + ',' +
        ClientDataSet1.FieldByName('Sales').AsString
      );
      ClientDataSet1.Next;
    end;

    // Sauvegarder le fichier CSV
    CSVFile.SaveToFile('sales_data.csv');
  finally
    CSVFile.Free;
  end;

  // Maintenant, utiliser Python pour entraîner le modèle
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('train_sales_model.py');
    Process.Parameters.Add('sales_data.csv');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    StatusBar1.SimpleText := 'Entraînement du modèle en cours...';
    Application.ProcessMessages;
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Afficher les informations du modèle
    MemoModelInfo.Clear;
    MemoModelInfo.Lines.Add('INFORMATIONS DU MODÈLE :');
    MemoModelInfo.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
      MemoModelInfo.Lines.Add(Output[i]);

    // Mise à jour du graphique pour inclure la courbe du modèle
    UpdateChart;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormSalesPrediction.ButtonPredictClick(Sender: TObject);
begin
  PredictSales;
end;

procedure TFormSalesPrediction.PredictSales;
var
  Process: TProcess;
  Output: TStringList;
  Year, Month, Promotion: Integer;
  Holiday: Boolean;
  InputValues: string;
  PredictedSales: Double;
begin
  // Récupérer les valeurs saisies par l'utilisateur
  Year := StrToIntDef(EditYear.Text, 2025);
  Month := EncodeMonth(ComboBoxMonth.Text);
  Holiday := CheckBoxHoliday.Checked;
  Promotion := EncodePromotion(ComboBoxPromotion.Text);

  // Créer la chaîne d'entrée pour Python
  InputValues := Format('%d,%d,%s,%d', [
    Year,
    Month,
    BoolToStr(Holiday, True),
    Promotion
  ]);

  // Utiliser Python pour faire la prédiction
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('predict_sales.py');
    Process.Parameters.Add(InputValues);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    if (Process.ExitCode = 0) and (Output.Count > 0) and TryStrToFloat(Output[0], PredictedSales) then
    begin
      // Afficher la prédiction
      LabelPrediction.Caption := Format('Ventes prévues : %.2f €', [PredictedSales]);

      // Ajouter cette prédiction au graphique
      Series2.AddXY(Year + (Month - 1) / 12, PredictedSales,
                   Format('%s %d', [ComboBoxMonth.Text, Year]), clRed);

      StatusBar1.SimpleText := 'Prédiction effectuée';
    end
    else
    begin
      LabelPrediction.Caption := 'Erreur de prédiction';
      ShowMessage('Erreur lors de la prédiction : ' + Output.Text);
      StatusBar1.SimpleText := 'Erreur de prédiction';
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormSalesPrediction.UpdateChart;
begin
  // Effacer les séries existantes
  Series1.Clear;
  Series2.Clear;

  // Configuration du graphique
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('Évolution des ventes');
  Chart1.BottomAxis.Title.Caption := 'Période';
  Chart1.LeftAxis.Title.Caption := 'Ventes (€)';

  // Ajouter les données historiques
  ClientDataSet1.First;
  while not ClientDataSet1.Eof do
  begin
    var Year := ClientDataSet1.FieldByName('Year').AsInteger;
    var Month := ClientDataSet1.FieldByName('Month').AsInteger;
    var Sales := ClientDataSet1.FieldByName('Sales').AsFloat;

    Series1.AddXY(Year + (Month - 1) / 12, Sales,
                 Format('%d-%02d', [Year, Month]), clBlue);

    ClientDataSet1.Next;
  end;

  // Vérifier si un modèle est déjà entraîné
  if FileExists('sales_model.pkl') then
  begin
    // Charger les prédictions historiques (si disponibles)
    if FileExists('historical_predictions.csv') then
    begin
      var PredictionsFile := TStringList.Create;
      try
        PredictionsFile.LoadFromFile('historical_predictions.csv');

        // Ignorer l'en-tête
        for var i := 1 to PredictionsFile.Count - 1 do
        begin
          var Parts := PredictionsFile[i].Split([',']);
          if Length(Parts) >= 3 then
          begin
            var Year := StrToIntDef(Parts[0], 0);
            var Month := StrToIntDef(Parts[1], 0);
            var Predicted := StrToFloatDef(Parts[2], 0);

            if (Year > 0) and (Month > 0) then
              Series2.AddXY(Year + (Month - 1) / 12, Predicted,
                           Format('%d-%02d', [Year, Month]), clRed);
          end;
        end;
      finally
        PredictionsFile.Free;
      end;
    end;
  end;
end;

function TFormSalesPrediction.EncodeMonth(const Month: string): Integer;
begin
  if Month = 'Janvier' then Result := 1
  else if Month = 'Février' then Result := 2
  else if Month = 'Mars' then Result := 3
  else if Month = 'Avril' then Result := 4
  else if Month = 'Mai' then Result := 5
  else if Month = 'Juin' then Result := 6
  else if Month = 'Juillet' then Result := 7
  else if Month = 'Août' then Result := 8
  else if Month = 'Septembre' then Result := 9
  else if Month = 'Octobre' then Result := 10
  else if Month = 'Novembre' then Result := 11
  else if Month = 'Décembre' then Result := 12
  else Result := 1;  // Par défaut Janvier
end;

function TFormSalesPrediction.EncodePromotion(const Promotion: string): Integer;
begin
  if Promotion = 'Aucune' then Result := 0
  else if Promotion = 'Faible' then Result := 1
  else if Promotion = 'Moyenne' then Result := 2
  else if Promotion = 'Forte' then Result := 3
  else Result := 0;  // Par défaut Aucune
end;

procedure TFormSalesPrediction.ButtonLoadClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers CSV (*.csv)|*.csv';
    OpenDialog.Title := 'Charger des données de ventes';

    if OpenDialog.Execute then
    begin
      // Réinitialiser les données
      ClientDataSet1.EmptyDataSet;

      // Charger le fichier CSV
      var DataFile := TStringList.Create;
      try
        DataFile.LoadFromFile(OpenDialog.FileName);

        // Vérifier l'en-tête
        if (DataFile.Count > 0) and
           (DataFile[0] = 'Year,Month,Holiday,Promotion,Sales') then
        begin
          // Parcourir les lignes de données
          for var i := 1 to DataFile.Count - 1 do
          begin
            var Parts := DataFile[i].Split([',']);
            if Length(Parts) >= 5 then
            begin
              var Year := StrToIntDef(Parts[0], 0);
              var Month := StrToIntDef(Parts[1], 0);
              var Holiday := UpperCase(Parts[2]) = 'TRUE';
              var Promotion := StrToIntDef(Parts[3], 0);
              var Sales := StrToFloatDef(Parts[4], 0);

              if (Year > 0) and (Month > 0) and (Month <= 12) then
              begin
                ClientDataSet1.Append;
                ClientDataSet1.FieldByName('Year').AsInteger := Year;
                ClientDataSet1.FieldByName('Month').AsInteger := Month;
                ClientDataSet1.FieldByName('Holiday').AsBoolean := Holiday;
                ClientDataSet1.FieldByName('Promotion').AsInteger := Promotion;
                ClientDataSet1.FieldByName('Sales').AsFloat := Sales;
                ClientDataSet1.Post;
              end;
            end;
          end;

          // Mettre à jour le graphique
          UpdateChart;

          StatusBar1.SimpleText := 'Données chargées depuis ' + OpenDialog.FileName;

          // Désactiver le bouton Prédire (nécessite un ré-entraînement)
          ButtonPredict.Enabled := False;
          MemoModelInfo.Clear;
          MemoModelInfo.Lines.Add('Veuillez entraîner un nouveau modèle avec ces données.');
        end
        else
          ShowMessage('Format de fichier invalide. L''en-tête doit être "Year,Month,Holiday,Promotion,Sales"');
      finally
        DataFile.Free;
      end;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormSalesPrediction.ButtonSaveClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers CSV (*.csv)|*.csv';
    SaveDialog.DefaultExt := 'csv';
    SaveDialog.Title := 'Enregistrer les données de ventes';

    if SaveDialog.Execute then
    begin
      var DataFile := TStringList.Create;
      try
        // Ajouter l'en-tête
        DataFile.Add('Year,Month,Holiday,Promotion,Sales');

        // Ajouter les données
        ClientDataSet1.First;
        while not ClientDataSet1.Eof do
        begin
          DataFile.Add(
            ClientDataSet1.FieldByName('Year').AsString + ',' +
            ClientDataSet1.FieldByName('Month').AsString + ',' +
            BoolToStr(ClientDataSet1.FieldByName('Holiday').AsBoolean, True) + ',' +
            ClientDataSet1.FieldByName('Promotion').AsString + ',' +
            ClientDataSet1.FieldByName('Sales').AsString
          );
          ClientDataSet1.Next;
        end;

        // Sauvegarder le fichier
        DataFile.SaveToFile(SaveDialog.FileName);

        StatusBar1.SimpleText := 'Données enregistrées dans ' + SaveDialog.FileName;
      finally
        DataFile.Free;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormSalesPrediction.ButtonClearClick(Sender: TObject);
begin
  // Effacer les prédictions du graphique
  Series2.Clear;

  // Réinitialiser la prédiction affichée
  LabelPrediction.Caption := 'Prédiction : -';

  StatusBar1.SimpleText := 'Prédictions effacées';
end;
```

Voici maintenant les scripts Python complémentaires pour notre application.

Le script pour l'entraînement du modèle (train_sales_model.py) :

```python
import sys
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
import pickle
import os

def train_sales_model(data_path):
    # Charger les données
    data = pd.read_csv(data_path)

    # Convertir les booléens en nombres
    data['Holiday'] = data['Holiday'].map({'TRUE': 1, 'FALSE': 0})

    # Caractéristiques et cible
    X = data[['Year', 'Month', 'Holiday', 'Promotion']]
    y = data['Sales']

    # Diviser les données en ensembles d'entraînement et de test
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

    # Créer et entraîner le modèle
    model = RandomForestRegressor(n_estimators=100, random_state=42)
    model.fit(X_train, y_train)

    # Évaluer le modèle
    y_pred = model.predict(X_test)
    mae = mean_absolute_error(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    rmse = np.sqrt(mse)
    r2 = r2_score(y_test, y_pred)

    # Afficher les résultats
    print("Modèle de prédiction des ventes entraîné")
    print("----------------------------------------")
    print(f"Erreur absolue moyenne (MAE): {mae:.2f} €")
    print(f"Erreur quadratique moyenne (RMSE): {rmse:.2f} €")
    print(f"Coefficient de détermination (R²): {r2:.4f}")
    print("")

    # Déterminer l'importance des caractéristiques
    feature_importance = pd.DataFrame({
        'Feature': X.columns,
        'Importance': model.feature_importances_
    }).sort_values('Importance', ascending=False)

    print("Importance des caractéristiques:")
    for i, row in feature_importance.iterrows():
        print(f"- {row['Feature']}: {row['Importance']*100:.2f}%")

    # Sauvegarder le modèle
    with open('sales_model.pkl', 'wb') as f:
        pickle.dump(model, f)

    # Générer des prédictions pour toutes les données historiques
    all_predictions = model.predict(X)

    # Sauvegarder les prédictions historiques
    predictions_df = pd.DataFrame({
        'Year': data['Year'],
        'Month': data['Month'],
        'Predicted': all_predictions,
        'Actual': data['Sales']
    })
    predictions_df.to_csv('historical_predictions.csv', index=False)

    return 0

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(train_sales_model(sys.argv[1]))
    else:
        print("Usage: python train_sales_model.py <data_path>")
        exit(1)
```

Le script pour la prédiction (predict_sales.py) :

```python
import sys
import pandas as pd
import numpy as np
import pickle

def predict_sales(input_values):
    try:
        # Charger le modèle
        with open('sales_model.pkl', 'rb') as f:
            model = pickle.load(f)

        # Parser les valeurs d'entrée
        values = input_values.split(',')
        if len(values) != 4:
            print(f"Erreur: 4 valeurs attendues, {len(values)} reçues")
            return 1

        # Convertir les valeurs
        year = int(values[0])
        month = int(values[1])
        holiday = 1 if values[2].upper() == 'TRUE' else 0
        promotion = int(values[3])

        # Créer le DataFrame pour la prédiction
        input_df = pd.DataFrame({
            'Year': [year],
            'Month': [month],
            'Holiday': [holiday],
            'Promotion': [promotion]
        })

        # Faire la prédiction
        prediction = model.predict(input_df)[0]

        # Afficher le résultat
        print(f"{prediction:.2f}")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(predict_sales(sys.argv[1]))
    else:
        print("Usage: python predict_sales.py <year,month,holiday,promotion>")
        exit(1)
```

## Types de modèles prédictifs courants

Voici quelques types de modèles prédictifs populaires que vous pouvez intégrer à vos applications Delphi :

### 1. Régression linéaire

La régression linéaire est l'un des algorithmes les plus simples et les plus interprétables.

```python
from sklearn.linear_model import LinearRegression

# Entraînement
model = LinearRegression()
model.fit(X_train, y_train)

# Coefficients
print(f"Intercept: {model.intercept_}")
for i, feature in enumerate(X.columns):
    print(f"{feature}: {model.coef_[i]}")
```

**Avantages** :
- Simple et facile à comprendre
- Rapide à entraîner même sur de grands ensembles de données
- Hautement interprétable (on peut voir l'impact de chaque variable)

**Inconvénients** :
- Ne capture pas les relations non linéaires
- Sensible aux valeurs aberrantes (outliers)
- Hypothèses statistiques parfois contraignantes

### 2. Arbre de décision

Les arbres de décision divisent les données en branches pour faire des prédictions.

```python
from sklearn.tree import DecisionTreeRegressor

# Entraînement
model = DecisionTreeRegressor(max_depth=5)
model.fit(X_train, y_train)

# Importance des caractéristiques
for i, feature in enumerate(X.columns):
    print(f"{feature}: {model.feature_importances_[i]*100:.2f}%")
```

**Avantages** :
- Capture les relations non linéaires
- Ne nécessite pas de mise à l'échelle des données
- Facile à visualiser et à interpréter

**Inconvénients** :
- Tendance au surapprentissage (overfitting)
- Peut être instable (de petits changements dans les données peuvent donner des arbres très différents)

### 3. Random Forest (Forêt aléatoire)

Random Forest combine plusieurs arbres de décision pour améliorer les prédictions.

```python
from sklearn.ensemble import RandomForestRegressor

# Entraînement
model = RandomForestRegressor(n_estimators=100, random_state=42)
model.fit(X_train, y_train)
```

**Avantages** :
- Généralement plus précis que les arbres de décision individuels
- Résistant au surapprentissage
- Peut gérer un grand nombre de variables et de données manquantes

**Inconvénients** :
- Moins interprétable qu'un arbre de décision unique
- Plus lent à entraîner et à exécuter
- Prend plus de mémoire

### 4. Gradient Boosting

Les algorithmes de boosting construisent des modèles séquentiellement, chacun corrigeant les erreurs du précédent.

```python
from sklearn.ensemble import GradientBoostingRegressor

# Entraînement
model = GradientBoostingRegressor(n_estimators=100, learning_rate=0.1)
model.fit(X_train, y_train)
```

**Avantages** :
- Souvent les meilleures performances sur des problèmes divers
- Gère bien les données mixtes (numériques et catégorielles)
- Robuste contre le surapprentissage avec un bon réglage

**Inconvénients** :
- Plus complexe à configurer (plusieurs hyperparamètres)
- Peut être lent à entraîner
- Moins interprétable que des modèles plus simples

### 5. SVM (Support Vector Machine)

Les SVM cherchent à trouver le meilleur hyperplan qui sépare les données.

```python
from sklearn.svm import SVR

# Entraînement
model = SVR(kernel='rbf', C=100, gamma=0.1)
model.fit(X_train, y_train)
```

**Avantages** :
- Efficace dans les espaces à haute dimension
- Versatile (différents noyaux pour différents problèmes)
- Bonne généralisation avec le bon réglage

**Inconvénients** :
- Difficile à interpréter
- Sensible au choix des hyperparamètres
- Ne s'adapte pas bien aux grands ensembles de données

## Application pratique : Segmentation de clientèle

Voici un exemple d'application de segmentation de clientèle utilisant le clustering (apprentissage non supervisé) :

```delphi
procedure TFormCustomerSegmentation.SegmentCustomers;
var
  Process: TProcess;
  Output: TStringList;
  CSVFile: TStringList;
begin
  // Créer un fichier CSV avec les données clients
  CSVFile := TStringList.Create;
  try
    // En-tête
    CSVFile.Add('CustomerID,AnnualSpend,VisitFrequency,LastVisitDays,ProductCategories');

    // Récupérer les données clients
    CustomersDataset.First;
    while not CustomersDataset.Eof do
    begin
      CSVFile.Add(
        CustomersDataset.FieldByName('CustomerID').AsString + ',' +
        CustomersDataset.FieldByName('AnnualSpend').AsString + ',' +
        CustomersDataset.FieldByName('VisitFrequency').AsString + ',' +
        CustomersDataset.FieldByName('LastVisitDays').AsString + ',' +
        CustomersDataset.FieldByName('ProductCategories').AsString
      );
      CustomersDataset.Next;
    end;

    // Sauvegarder le fichier
    CSVFile.SaveToFile('customers.csv');
  finally
    CSVFile.Free;
  end;

  // Utiliser Python pour la segmentation
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('segment_customers.py');
    Process.Parameters.Add('customers.csv');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution
    StatusBar1.SimpleText := 'Segmentation en cours...';
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Afficher les résultats de la segmentation
    MemoResults.Clear;
    MemoResults.Lines.Add('RÉSULTATS DE LA SEGMENTATION :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
      MemoResults.Lines.Add(Output[i]);

    // Charger les segments dans la grille
    if FileExists('customer_segments.csv') then
      LoadSegmentsToGrid('customer_segments.csv');

    StatusBar1.SimpleText := 'Segmentation terminée';
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormCustomerSegmentation.LoadSegmentsToGrid(const FileName: string);
var
  SegmentsFile: TStringList;
begin
  // Vider le ClientDataSet des segments
  SegmentsDataset.EmptyDataSet;

  // Charger le fichier CSV
  SegmentsFile := TStringList.Create;
  try
    SegmentsFile.LoadFromFile(FileName);

    // Vérifier l'en-tête
    if (SegmentsFile.Count > 0) and
       (SegmentsFile[0] = 'CustomerID,Segment,Description') then
    begin
      // Parcourir les lignes de données
      for var i := 1 to SegmentsFile.Count - 1 do
      begin
        var Parts := SegmentsFile[i].Split([',']);
        if Length(Parts) >= 3 then
        begin
          SegmentsDataset.Append;
          SegmentsDataset.FieldByName('CustomerID').AsString := Parts[0];
          SegmentsDataset.FieldByName('Segment').AsInteger := StrToIntDef(Parts[1], 0);
          SegmentsDataset.FieldByName('Description').AsString := Parts[2];
          SegmentsDataset.Post;
        end;
      end;
    end;
  finally
    SegmentsFile.Free;
  end;

  // Mettre à jour le graphique de distribution des segments
  UpdateSegmentChart;
end;

procedure TFormCustomerSegmentation.UpdateSegmentChart;
var
  SegmentCounts: array[0..4] of Integer;
  i: Integer;
begin
  // Initialiser les compteurs
  for i := 0 to 4 do
    SegmentCounts[i] := 0;

  // Compter les clients dans chaque segment
  SegmentsDataset.First;
  while not SegmentsDataset.Eof do
  begin
    i := SegmentsDataset.FieldByName('Segment').AsInteger;
    if (i >= 0) and (i <= 4) then
      Inc(SegmentCounts[i]);
    SegmentsDataset.Next;
  end;

  // Mettre à jour le graphique
  ChartSegments.Series[0].Clear;

  ChartSegments.Series[0].Add(SegmentCounts[0], 'Segment 0', clRed);
  ChartSegments.Series[0].Add(SegmentCounts[1], 'Segment 1', clGreen);
  ChartSegments.Series[0].Add(SegmentCounts[2], 'Segment 2', clBlue);
  ChartSegments.Series[0].Add(SegmentCounts[3], 'Segment 3', clYellow);
  ChartSegments.Series[0].Add(SegmentCounts[4], 'Segment 4', clPurple);
end;
```

Le script Python pour la segmentation des clients (segment_customers.py) :

```python
import sys
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

def segment_customers(data_path):
    # Charger les données
    data = pd.read_csv(data_path)

    # Sélectionner les caractéristiques pour la segmentation
    features = data[['AnnualSpend', 'VisitFrequency', 'LastVisitDays', 'ProductCategories']]

    # Mise à l'échelle des données (standardisation)
    scaler = StandardScaler()
    scaled_features = scaler.fit_transform(features)

    # Déterminer le nombre optimal de clusters avec la méthode du coude
    wcss = []
    for i in range(1, 11):
        kmeans = KMeans(n_clusters=i, init='k-means++', max_iter=300, n_init=10, random_state=42)
        kmeans.fit(scaled_features)
        wcss.append(kmeans.inertia_)

    # Créer le graphique de l'inertie
    plt.figure(figsize=(10, 6))
    plt.plot(range(1, 11), wcss, marker='o', linestyle='--')
    plt.title('Méthode du coude pour déterminer le nombre optimal de clusters')
    plt.xlabel('Nombre de clusters')
    plt.ylabel('WCSS')
    plt.savefig('elbow_method.png')

    # Appliquer K-means avec le nombre optimal de clusters (ici 5 pour l'exemple)
    kmeans = KMeans(n_clusters=5, init='k-means++', max_iter=300, n_init=10, random_state=42)
    data['Segment'] = kmeans.fit_predict(scaled_features)

    # Analyser les caractéristiques de chaque segment
    segment_profiles = data.groupby('Segment').mean()

    # Afficher les profils des segments
    print("Profil des segments:")
    print("-------------------")
    for segment, profile in segment_profiles.iterrows():
        print(f"Segment {segment}:")
        print(f"  - Dépense annuelle moyenne: {profile['AnnualSpend']:.2f} €")
        print(f"  - Fréquence de visite: {profile['VisitFrequency']:.2f} fois par mois")
        print(f"  - Jours depuis dernière visite: {profile['LastVisitDays']:.1f} jours")
        print(f"  - Catégories de produits: {profile['ProductCategories']:.1f}")
        print()

    # Déterminer les descriptions des segments
    segment_descriptions = {
        0: "",
        1: "",
        2: "",
        3: "",
        4: ""
    }

    # Attribuer des descriptions significatives en fonction des profils
    for segment, profile in segment_profiles.iterrows():
        if profile['AnnualSpend'] > 5000 and profile['VisitFrequency'] > 8:
            segment_descriptions[segment] = "Clients Premium Fidèles"
        elif profile['AnnualSpend'] > 3000 and profile['VisitFrequency'] > 5:
            segment_descriptions[segment] = "Clients Réguliers"
        elif profile['LastVisitDays'] < 30 and profile['AnnualSpend'] < 1000:
            segment_descriptions[segment] = "Nouveaux Clients Potentiels"
        elif profile['LastVisitDays'] > 90:
            segment_descriptions[segment] = "Clients Inactifs"
        else:
            segment_descriptions[segment] = "Clients Occasionnels"

    # Créer un DataFrame avec les segments et leurs descriptions
    segments_output = pd.DataFrame({
        'CustomerID': data['CustomerID'],
        'Segment': data['Segment'],
        'Description': data['Segment'].map(segment_descriptions)
    })

    # Sauvegarder les résultats
    segments_output.to_csv('customer_segments.csv', index=False)

    # Calculer la distribution des segments
    segment_distribution = data['Segment'].value_counts(normalize=True) * 100

    print("Distribution des segments:")
    print("-----------------------")
    for segment, percentage in segment_distribution.items():
        description = segment_descriptions[segment]
        print(f"Segment {segment} ({description}): {percentage:.1f}% des clients")

    # Visualisation PCA pour réduire à 2 dimensions
    pca = PCA(n_components=2)
    pca_result = pca.fit_transform(scaled_features)

    # Créer un DataFrame avec les résultats PCA
    pca_df = pd.DataFrame(data=pca_result, columns=['PC1', 'PC2'])
    pca_df['Segment'] = data['Segment']

    # Créer le graphique de dispersion
    plt.figure(figsize=(10, 8))
    colors = ['red', 'green', 'blue', 'purple', 'orange']

    for segment, color in enumerate(colors):
        plt.scatter(
            pca_df[pca_df['Segment'] == segment]['PC1'],
            pca_df[pca_df['Segment'] == segment]['PC2'],
            c=color, label=f"Segment {segment}: {segment_descriptions[segment]}"
        )

    plt.title('Visualisation des segments de clients (PCA)')
    plt.xlabel('Composante principale 1')
    plt.ylabel('Composante principale 2')
    plt.legend()
    plt.grid(True)
    plt.savefig('segment_visualization.png')

    return 0

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(segment_customers(sys.argv[1]))
    else:
        print("Usage: python segment_customers.py <data_path>")
        exit(1)
```

## Conseils pour développer des modèles prédictifs efficaces

### 1. Préparation des données

La qualité des prédictions dépend fortement de la qualité des données :

```delphi
procedure TFormDataPrep.PrepareDataForML;
begin
  // 1. Vérifier les données manquantes
  ClientDataset1.First;
  while not ClientDataset1.Eof do
  begin
    // Remplacer les valeurs manquantes par des moyennes ou des valeurs par défaut
    if ClientDataset1.FieldByName('Income').IsNull then
      ClientDataset1.Edit;
      ClientDataset1.FieldByName('Income').AsFloat := FMeanIncome;
      ClientDataset1.Post;
    end;

    // 2. Détecter et traiter les valeurs aberrantes (outliers)
    if (ClientDataset1.FieldByName('Age').AsInteger > 120) or
       (ClientDataset1.FieldByName('Age').AsInteger < 18) then
    begin
      // Marquer comme suspect ou corriger
      ClientDataset1.Edit;
      ClientDataset1.FieldByName('IsSuspect').AsBoolean := True;
      ClientDataset1.Post;
    end;

    // 3. Normaliser les données numériques si nécessaire
    if NormalizeCheckBox.Checked then
    begin
      ClientDataset1.Edit;
      // Z-score normalization: (x - mean) / std_dev
      ClientDataset1.FieldByName('NormalizedIncome').AsFloat :=
        (ClientDataset1.FieldByName('Income').AsFloat - FMeanIncome) / FStdDevIncome;
      ClientDataset1.Post;
    end;

    // 4. Encoder les variables catégorielles
    if ClientDataset1.FieldByName('Gender').AsString = 'Male' then
      ClientDataset1.Edit;
      ClientDataset1.FieldByName('GenderEncoded').AsInteger := 0;
      ClientDataset1.Post;
    else
      ClientDataset1.Edit;
      ClientDataset1.FieldByName('GenderEncoded').AsInteger := 1;
      ClientDataset1.Post;
    end;

    ClientDataset1.Next;
  end;

  // 5. Diviser les données en ensembles d'entraînement et de test
  SplitDataIntoTrainTest(0.8);  // 80% pour l'entraînement, 20% pour les tests

  StatusBar1.SimpleText := 'Données préparées pour l''apprentissage automatique';
end;

procedure TFormDataPrep.SplitDataIntoTrainTest(TrainRatio: Double);
var
  TrainFile, TestFile: TStringList;
  TotalCount, TrainCount, i: Integer;
  RandomIndices: TList<Integer>;
begin
  // Initialiser les listes
  TrainFile := TStringList.Create;
  TestFile := TStringList.Create;
  RandomIndices := TList<Integer>.Create;

  try
    // Créer l'en-tête
    var Header := 'ID,Age,NormalizedIncome,GenderEncoded,HasPurchased';
    TrainFile.Add(Header);
    TestFile.Add(Header);

    // Déterminer combien d'enregistrements iront dans l'ensemble d'entraînement
    TotalCount := ClientDataset1.RecordCount;
    TrainCount := Round(TotalCount * TrainRatio);

    // Créer une liste d'indices et la mélanger aléatoirement
    for i := 0 to TotalCount - 1 do
      RandomIndices.Add(i);

    // Mélanger les indices (Fisher-Yates shuffle)
    for i := TotalCount - 1 downto 1 do
    begin
      var j := Random(i + 1);
      var Temp := RandomIndices[i];
      RandomIndices[i] := RandomIndices[j];
      RandomIndices[j] := Temp;
    end;

    // Parcourir les données et les diviser en ensembles d'entraînement et de test
    ClientDataset1.First;
    i := 0;
    while not ClientDataset1.Eof do
    begin
      var Line :=
        ClientDataset1.FieldByName('ID').AsString + ',' +
        ClientDataset1.FieldByName('Age').AsString + ',' +
        ClientDataset1.FieldByName('NormalizedIncome').AsString + ',' +
        ClientDataset1.FieldByName('GenderEncoded').AsString + ',' +
        ClientDataset1.FieldByName('HasPurchased').AsString;

      // Si l'indice est dans les premiers TrainCount indices mélangés, c'est un ensemble d'entraînement
      if RandomIndices.IndexOf(i) < TrainCount then
        TrainFile.Add(Line)
      else
        TestFile.Add(Line);

      ClientDataset1.Next;
      Inc(i);
    end;

    // Sauvegarder les fichiers
    TrainFile.SaveToFile('train_data.csv');
    TestFile.SaveToFile('test_data.csv');

    StatusBar1.SimpleText := Format('Données divisées : %d entraînement, %d test',
                                  [TrainCount, TotalCount - TrainCount]);
  finally
    TrainFile.Free;
    TestFile.Free;
    RandomIndices.Free;
  end;
end;
```

### 2. Évaluation et validation du modèle

Il est essentiel de bien évaluer vos modèles pour s'assurer qu'ils sont fiables :

```delphi
procedure TFormModelEvaluation.EvaluateModel;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('evaluate_model.py');
    Process.Parameters.Add('train_data.csv');
    Process.Parameters.Add('test_data.csv');
    Process.Parameters.Add(GetSelectedModelType);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage des métriques d'évaluation
    MemoResults.Clear;
    MemoResults.Lines.Add('ÉVALUATION DU MODÈLE :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
      MemoResults.Lines.Add(Output[i]);

    // Charger les graphiques de performance si disponibles
    if FileExists('model_evaluation.png') then
      Image1.Picture.LoadFromFile('model_evaluation.png');

    if FileExists('confusion_matrix.png') then
      Image2.Picture.LoadFromFile('confusion_matrix.png');

    StatusBar1.SimpleText := 'Évaluation terminée';
  finally
    Process.Free;
    Output.Free;
  end;
end;

function TFormModelEvaluation.GetSelectedModelType: string;
begin
  if RadioButtonLogistic.Checked then
    Result := 'logistic'
  else if RadioButtonRandomForest.Checked then
    Result := 'random_forest'
  else if RadioButtonSVM.Checked then
    Result := 'svm'
  else if RadioButtonNN.Checked then
    Result := 'neural_network'
  else
    Result := 'random_forest';  // Par défaut
end;
```

Le script Python correspondant (evaluate_model.py) :

```python
import sys
import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier
from sklearn.svm import SVC
from sklearn.neural_network import MLPClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
from sklearn.metrics import confusion_matrix, classification_report, roc_curve, auc
import matplotlib.pyplot as plt
import seaborn as sns

def evaluate_model(train_path, test_path, model_type):
    # Charger les données
    train_data = pd.read_csv(train_path)
    test_data = pd.read_csv(test_path)

    # Séparer les caractéristiques et la cible
    X_train = train_data.drop(['ID', 'HasPurchased'], axis=1)
    y_train = train_data['HasPurchased']

    X_test = test_data.drop(['ID', 'HasPurchased'], axis=1)
    y_test = test_data['HasPurchased']

    # Sélectionner le type de modèle
    if model_type == 'logistic':
        model = LogisticRegression(random_state=42)
        model_name = "Régression Logistique"
    elif model_type == 'svm':
        model = SVC(probability=True, random_state=42)
        model_name = "Support Vector Machine"
    elif model_type == 'neural_network':
        model = MLPClassifier(hidden_layer_sizes=(10, 5), random_state=42)
        model_name = "Réseau de Neurones"
    else:  # random_forest par défaut
        model = RandomForestClassifier(n_estimators=100, random_state=42)
        model_name = "Random Forest"

    # Entraîner le modèle
    model.fit(X_train, y_train)

    # Faire des prédictions
    y_pred = model.predict(X_test)
    y_prob = model.predict_proba(X_test)[:, 1]  # Probabilités pour la classe positive

    # Calculer les métriques
    accuracy = accuracy_score(y_test, y_pred)
    precision = precision_score(y_test, y_pred)
    recall = recall_score(y_test, y_pred)
    f1 = f1_score(y_test, y_pred)

    # Afficher les résultats
    print(f"Modèle: {model_name}")
    print(f"Précision (accuracy): {accuracy:.4f}")
    print(f"Precision: {precision:.4f}")
    print(f"Recall (sensibilité): {recall:.4f}")
    print(f"F1 Score: {f1:.4f}")
    print("")
    print("Rapport de classification:")
    print(classification_report(y_test, y_pred))

    # Créer des visualisations

    # 1. Matrice de confusion
    plt.figure(figsize=(8, 6))
    cm = confusion_matrix(y_test, y_pred)
    sns.heatmap(cm, annot=True, fmt='d', cmap='Blues')
    plt.title(f'Matrice de confusion - {model_name}')
    plt.ylabel('Valeur réelle')
    plt.xlabel('Valeur prédite')
    plt.savefig('confusion_matrix.png')

    # 2. Courbe ROC
    plt.figure(figsize=(8, 6))
    fpr, tpr, _ = roc_curve(y_test, y_prob)
    roc_auc = auc(fpr, tpr)

    plt.plot(fpr, tpr, color='darkorange', lw=2, label=f'ROC curve (area = {roc_auc:.3f})')
    plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('Taux de faux positifs')
    plt.ylabel('Taux de vrais positifs')
    plt.title(f'Courbe ROC - {model_name}')
    plt.legend(loc="lower right")
    plt.savefig('model_evaluation.png')

    return 0

if __name__ == "__main__":
    if len(sys.argv) > 3:
        exit(evaluate_model(sys.argv[1], sys.argv[2], sys.argv[3]))
    else:
        print("Usage: python evaluate_model.py <train_path> <test_path> <model_type>")
        exit(1)
```

### 3. Validation croisée pour une évaluation plus robuste

La validation croisée aide à obtenir une évaluation plus fiable de votre modèle :

```python
from sklearn.model_selection import cross_val_score

def cross_validate_model(X, y, model_type):
    # Sélectionner le modèle
    if model_type == 'logistic':
        model = LogisticRegression(random_state=42)
    elif model_type == 'svm':
        model = SVC(random_state=42)
    else:  # random_forest par défaut
        model = RandomForestClassifier(n_estimators=100, random_state=42)

    # Effectuer la validation croisée avec 5 plis
    cv_scores = cross_val_score(model, X, y, cv=5, scoring='accuracy')

    print(f"Résultats de validation croisée (5 plis):")
    print(f"Scores individuels: {cv_scores}")
    print(f"Précision moyenne: {cv_scores.mean():.4f}")
    print(f"Écart-type: {cv_scores.std():.4f}")
```

### 4. Optimisation des hyperparamètres

Pour obtenir les meilleures performances, il faut ajuster les paramètres du modèle :

```python
from sklearn.model_selection import GridSearchCV

def optimize_hyperparameters(X, y, model_type):
    if model_type == 'random_forest':
        model = RandomForestClassifier(random_state=42)
        param_grid = {
            'n_estimators': [50, 100, 200],
            'max_depth': [None, 10, 20, 30],
            'min_samples_split': [2, 5, 10],
            'min_samples_leaf': [1, 2, 4]
        }
    elif model_type == 'svm':
        model = SVC(random_state=42)
        param_grid = {
            'C': [0.1, 1, 10, 100],
            'gamma': ['scale', 'auto', 0.1, 0.01],
            'kernel': ['rbf', 'linear', 'poly']
        }
    else:  # logistic
        model = LogisticRegression(random_state=42)
        param_grid = {
            'C': [0.01, 0.1, 1, 10, 100],
            'solver': ['liblinear', 'lbfgs'],
            'penalty': ['l1', 'l2']
        }

    # Recherche par grille avec validation croisée
    grid_search = GridSearchCV(
        model, param_grid, cv=5, scoring='accuracy', n_jobs=-1
    )
    grid_search.fit(X, y)

    print(f"Meilleurs paramètres: {grid_search.best_params_}")
    print(f"Meilleur score: {grid_search.best_score_:.4f}")

    return grid_search.best_estimator_
```

### 5. Déploiement du modèle en production

Pour utiliser votre modèle en production, vous pouvez le sauvegarder et le charger quand nécessaire :

```delphi
procedure TFormPrediction.SaveModel;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('save_model.py');
    Process.Parameters.Add(EditModelName.Text);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    if Process.ExitCode = 0 then
      ShowMessage('Modèle sauvegardé avec succès : ' + EditModelName.Text)
    else
      ShowMessage('Erreur lors de la sauvegarde : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormPrediction.LoadModel;
var
  Process: TProcess;
  Output: TStringList;
begin
  if OpenDialog1.Execute then
  begin
    Process := TProcess.Create(nil);
    Output := TStringList.Create;

    try
      // Configuration du processus Python
      Process.Executable := 'python';
      Process.Parameters.Add('load_model.py');
      Process.Parameters.Add(OpenDialog1.FileName);
      Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

      // Exécution du script Python
      Process.Execute;

      // Lecture des résultats
      Output.LoadFromStream(Process.Output);

      if Process.ExitCode = 0 then
      begin
        ShowMessage('Modèle chargé avec succès');
        ButtonPredict.Enabled := True;
        StatusBar1.SimpleText := 'Modèle actif : ' + ExtractFileName(OpenDialog1.FileName);
      end
      else
        ShowMessage('Erreur lors du chargement : ' + Output.Text);
    finally
      Process.Free;
      Output.Free;
    end;
  end;
end;
```

Les scripts Python correspondants :

```python
# save_model.py
import sys
import joblib

def save_model(model_name):
    try:
        # Charger le modèle depuis la variable temporaire
        model = joblib.load('temp_model.pkl')

        # Sauvegarder le modèle avec le nom spécifié
        joblib.dump(model, f"{model_name}.pkl")

        print(f"Modèle sauvegardé sous '{model_name}.pkl'")
        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(save_model(sys.argv[1]))
    else:
        print("Usage: python save_model.py <model_name>")
        exit(1)
```

```python
# load_model.py
import sys
import joblib

def load_model(model_path):
    try:
        # Charger le modèle depuis le fichier
        model = joblib.load(model_path)

        # Sauvegarder dans un fichier temporaire pour être utilisé par d'autres scripts
        joblib.dump(model, 'temp_model.pkl')

        # Afficher les informations du modèle
        print(f"Type de modèle: {type(model).__name__}")

        if hasattr(model, 'feature_importances_'):
            print("Caractéristiques importantes:")
            for i, importance in enumerate(model.feature_importances_):
                print(f"  Feature {i}: {importance:.4f}")

        print("Modèle chargé avec succès")
        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(load_model(sys.argv[1]))
    else:
        print("Usage: python load_model.py <model_path>")
        exit(1)
```

## Exemples de cas d'utilisation concrets

### 1. Prédiction de départ de clients (Churn Prediction)

```delphi
procedure TFormChurnPrediction.PredictChurn;
var
  CSVFile: TStringList;
  Process: TProcess;
  Output: TStringList;
begin
  // Préparation des données
  CSVFile := TStringList.Create;
  try
    // En-tête
    CSVFile.Add('CustomerID,Tenure,MonthlyCharges,TotalCharges,Contract,OnlineBackup,TechSupport,StreamingTV,PaperlessBilling');

    // Ajouter les clients actuels
    CustomersDataset.First;
    while not CustomersDataset.Eof do
    begin
      CSVFile.Add(
        CustomersDataset.FieldByName('CustomerID').AsString + ',' +
        CustomersDataset.FieldByName('Tenure').AsString + ',' +
        CustomersDataset.FieldByName('MonthlyCharges').AsString + ',' +
        CustomersDataset.FieldByName('TotalCharges').AsString + ',' +
        IntToStr(EncodeContract(CustomersDataset.FieldByName('Contract').AsString)) + ',' +
        BoolToStr(CustomersDataset.FieldByName('OnlineBackup').AsBoolean, '1', '0') + ',' +
        BoolToStr(CustomersDataset.FieldByName('TechSupport').AsBoolean, '1', '0') + ',' +
        BoolToStr(CustomersDataset.FieldByName('StreamingTV').AsBoolean, '1', '0') + ',' +
        BoolToStr(CustomersDataset.FieldByName('PaperlessBilling').AsBoolean, '1', '0')
      );
      CustomersDataset.Next;
    end;

    // Sauvegarder le fichier temporaire
    CSVFile.SaveToFile('customers_to_predict.csv');
  finally
    CSVFile.Free;
  end;

  // Exécuter la prédiction
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('predict_churn.py');
    Process.Parameters.Add('customers_to_predict.csv');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    StatusBar1.SimpleText := 'Prédiction en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    if Process.ExitCode = 0 then
    begin
      // Charger les résultats
      LoadChurnPredictions('churn_predictions.csv');

      StatusBar1.SimpleText := 'Prédictions de départ terminées';
    end
    else
      ShowMessage('Erreur : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;

function TFormChurnPrediction.EncodeContract(const ContractType: string): Integer;
begin
  if ContractType = 'Month-to-month' then Result := 0
  else if ContractType = 'One year' then Result := 1
  else if ContractType = 'Two year' then Result := 2
  else Result := 0;
end;

procedure TFormChurnPrediction.LoadChurnPredictions(const FileName: string);
var
  ResultsFile: TStringList;
  HighRiskCount: Integer;
begin
  // Réinitialiser l'ensemble de données des résultats
  PredictionsDataset.EmptyDataSet;

  ResultsFile := TStringList.Create;
  HighRiskCount := 0;

  try
    ResultsFile.LoadFromFile(FileName);

    // Vérifier l'en-tête
    if (ResultsFile.Count > 0) and
       (ResultsFile[0] = 'CustomerID,ChurnProbability,RiskCategory') then
    begin
      // Parcourir les résultats
      for var i := 1 to ResultsFile.Count - 1 do
      begin
        var Parts := ResultsFile[i].Split([',']);
        if Length(Parts) >= 3 then
        begin
          var CustomerID := Parts[0];
          var ChurnProb := StrToFloatDef(Parts[1], 0);
          var RiskCategory := Parts[2];

          PredictionsDataset.Append;
          PredictionsDataset.FieldByName('CustomerID').AsString := CustomerID;
          PredictionsDataset.FieldByName('ChurnProbability').AsFloat := ChurnProb;
          PredictionsDataset.FieldByName('RiskCategory').AsString := RiskCategory;
          PredictionsDataset.Post;

          // Compter les clients à haut risque
          if RiskCategory = 'High' then
            Inc(HighRiskCount);
        end;
      end;
    end;

    // Mettre à jour le résumé
    LabelHighRisk.Caption := 'Clients à haut risque : ' + IntToStr(HighRiskCount);
    LabelTotalAnalyzed.Caption := 'Total analysé : ' + IntToStr(ResultsFile.Count - 1);

    // Mettre à jour le graphique
    UpdateChurnChart;
  finally
    ResultsFile.Free;
  end;
end;

procedure TFormChurnPrediction.UpdateChurnChart;
var
  LowRisk, MediumRisk, HighRisk: Integer;
begin
  // Compter les clients dans chaque catégorie
  LowRisk := 0;
  MediumRisk := 0;
  HighRisk := 0;

  PredictionsDataset.First;
  while not PredictionsDataset.Eof do
  begin
    if PredictionsDataset.FieldByName('RiskCategory').AsString = 'Low' then
      Inc(LowRisk)
    else if PredictionsDataset.FieldByName('RiskCategory').AsString = 'Medium' then
      Inc(MediumRisk)
    else if PredictionsDataset.FieldByName('RiskCategory').AsString = 'High' then
      Inc(HighRisk);

    PredictionsDataset.Next;
  end;

  // Mettre à jour le graphique
  ChartChurn.Series[0].Clear;

  ChartChurn.Series[0].Add(LowRisk, 'Faible risque', clGreen);
  ChartChurn.Series[0].Add(MediumRisk, 'Risque moyen', clYellow);
  ChartChurn.Series[0].Add(HighRisk, 'Risque élevé', clRed);
end;
```

Le script Python pour la prédiction d'attrition (predict_churn.py) :

```python
import sys
import pandas as pd
import numpy as np
import joblib

def predict_churn(customers_path):
    try:
        # Charger le modèle
        model = joblib.load('churn_model.pkl')

        # Charger les données des clients
        customers = pd.read_csv(customers_path)

        # Faire des prédictions
        churn_probabilities = model.predict_proba(customers.drop('CustomerID', axis=1))[:, 1]

        # Catégoriser les risques
        risk_categories = []
        for prob in churn_probabilities:
            if prob < 0.3:
                risk_categories.append('Low')
            elif prob < 0.7:
                risk_categories.append('Medium')
            else:
                risk_categories.append('High')

        # Créer un DataFrame avec les résultats
        results = pd.DataFrame({
            'CustomerID': customers['CustomerID'],
            'ChurnProbability': churn_probabilities,
            'RiskCategory': risk_categories
        })

        # Sauvegarder les résultats
        results.to_csv('churn_predictions.csv', index=False)

        # Résumé des résultats
        print(f"Total de clients analysés: {len(results)}")
        print(f"Clients à faible risque: {results['RiskCategory'].value_counts().get('Low', 0)}")
        print(f"Clients à risque moyen: {results['RiskCategory'].value_counts().get('Medium', 0)}")
        print(f"Clients à haut risque: {results['RiskCategory'].value_counts().get('High', 0)}")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(predict_churn(sys.argv[1]))
    else:
        print("Usage: python predict_churn.py <customers_path>")
        exit(1)
```

### 2. Prévision de la demande pour la gestion des stocks

```delphi
procedure TFormDemandForecasting.ForecastDemand;
var
  Process: TProcess;
  Output: TStringList;
begin
  // Préparer les données historiques
  if not FileExists('historical_demand.csv') then
    PrepareHistoricalData;

  // Exécuter la prévision
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('forecast_demand.py');
    Process.Parameters.Add(IntToStr(UpDownForecastHorizon.Position));  // Nombre de périodes à prévoir
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    StatusBar1.SimpleText := 'Prévision en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    if Process.ExitCode = 0 then
    begin
      // Afficher les résultats
      MemoResults.Lines.Clear;
      MemoResults.Lines.Add('PRÉVISION DE LA DEMANDE :');
      MemoResults.Lines.Add('');

      for var i := 0 to Output.Count - 1 do
        MemoResults.Lines.Add(Output[i]);

      // Charger le graphique de prévision
      if FileExists('demand_forecast.png') then
        Image1.Picture.LoadFromFile('demand_forecast.png');

      // Charger les données de prévision dans la grille
      LoadForecastData('demand_forecast.csv');

      StatusBar1.SimpleText := 'Prévision terminée';
    end
    else
      ShowMessage('Erreur : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormDemandForecasting.PrepareHistoricalData;
var
  CSVFile: TStringList;
begin
  CSVFile := TStringList.Create;
  try
    // En-tête
    CSVFile.Add('Date,ProductID,Quantity,Temperature,Holiday,Promotion,WeekDay');

    // Ajouter les données historiques depuis la base de données
    HistoricalDataset.First;
    while not HistoricalDataset.Eof do
    begin
      // Formater la date au format YYYY-MM-DD
      var DateStr := FormatDateTime('yyyy-mm-dd', HistoricalDataset.FieldByName('Date').AsDateTime);

      CSVFile.Add(
        DateStr + ',' +
        HistoricalDataset.FieldByName('ProductID').AsString + ',' +
        HistoricalDataset.FieldByName('Quantity').AsString + ',' +
        HistoricalDataset.FieldByName('Temperature').AsString + ',' +
        BoolToStr(HistoricalDataset.FieldByName('Holiday').AsBoolean, '1', '0') + ',' +
        HistoricalDataset.FieldByName('Promotion').AsString + ',' +
        HistoricalDataset.FieldByName('WeekDay').AsString
      );

      HistoricalDataset.Next;
    end;

    // Sauvegarder le fichier
    CSVFile.SaveToFile('historical_demand.csv');
  finally
    CSVFile.Free;
  end;
end;

procedure TFormDemandForecasting.LoadForecastData(const FileName: string);
var
  ForecastFile: TStringList;
begin
  // Réinitialiser l'ensemble de données des prévisions
  ForecastDataset.EmptyDataSet;

  ForecastFile := TStringList.Create;
  try
    ForecastFile.LoadFromFile(FileName);

    // Vérifier l'en-tête
    if (ForecastFile.Count > 0) and
       (ForecastFile[0] = 'Date,ProductID,Quantity,LowerBound,UpperBound') then
    begin
      // Parcourir les résultats
      for var i := 1 to ForecastFile.Count - 1 do
      begin
        var Parts := ForecastFile[i].Split([',']);
        if Length(Parts) >= 5 then
        begin
          var DateStr := Parts[0];
          var ProductID := StrToIntDef(Parts[1], 0);
          var Quantity := StrToFloatDef(Parts[2], 0);
          var LowerBound := StrToFloatDef(Parts[3], 0);
          var UpperBound := StrToFloatDef(Parts[4], 0);

          // Convertir la date du format 'YYYY-MM-DD' au format TDateTime
          var Year, Month, Day: Word;
          Year := StrToIntDef(Copy(DateStr, 1, 4), 0);
          Month := StrToIntDef(Copy(DateStr, 6, 2), 0);
          Day := StrToIntDef(Copy(DateStr, 9, 2), 0);
          var Date := EncodeDate(Year, Month, Day);

          // Ajouter à l'ensemble de données
          ForecastDataset.Append;
          ForecastDataset.FieldByName('Date').AsDateTime := Date;
          ForecastDataset.FieldByName('ProductID').AsInteger := ProductID;
          ForecastDataset.FieldByName('Quantity').AsFloat := Quantity;
          ForecastDataset.FieldByName('LowerBound').AsFloat := LowerBound;
          ForecastDataset.FieldByName('UpperBound').AsFloat := UpperBound;
          ForecastDataset.Post;
        end;
      end;
    end;

    // Mettre à jour les statistiques
    UpdateForecastStats;
  finally
    ForecastFile.Free;
  end;
end;

procedure TFormDemandForecasting.UpdateForecastStats;
var
  TotalQuantity, MaxQuantity, MinQuantity: Double;
  Count: Integer;
begin
  TotalQuantity := 0;
  MaxQuantity := 0;
  MinQuantity := 999999;
  Count := 0;

  ForecastDataset.First;
  while not ForecastDataset.Eof do
  begin
    var Quantity := ForecastDataset.FieldByName('Quantity').AsFloat;

    TotalQuantity := TotalQuantity + Quantity;

    if Quantity > MaxQuantity then
      MaxQuantity := Quantity;

    if Quantity < MinQuantity then
      MinQuantity := Quantity;

    Inc(Count);

    ForecastDataset.Next;
  end;

  // Mettre à jour les statistiques
  if Count > 0 then
  begin
    LabelTotalForecast.Caption := Format('Demande totale prévue : %.0f unités', [TotalQuantity]);
    LabelAvgForecast.Caption := Format('Demande moyenne : %.2f unités', [TotalQuantity / Count]);
    LabelMaxForecast.Caption := Format('Pic de demande : %.0f unités', [MaxQuantity]);
    LabelMinForecast.Caption := Format('Demande minimale : %.0f unités', [MinQuantity]);
  end;
end;
```

Le script Python pour la prévision de la demande (forecast_demand.py) :

```python
import sys
import pandas as pd
import numpy as np
from prophet import Prophet
import matplotlib.pyplot as plt
from sklearn.preprocessing import OneHotEncoder

def forecast_demand(forecast_periods):
    try:
        # Charger les données historiques
        data = pd.read_csv('historical_demand.csv')

        # Convertir la date en format datetime
        data['Date'] = pd.to_datetime(data['Date'])

        # Créer des variables pour chaque produit
        products = data['ProductID'].unique()

        # Créer un DataFrame pour stocker les prévisions
        all_forecasts = pd.DataFrame()

        # Pour chaque produit, créer un modèle de prévision
        for product in products:
            # Filtrer les données pour ce produit
            product_data = data[data['ProductID'] == product].copy()

            # Préparer les données pour Prophet
            prophet_data = product_data[['Date', 'Quantity']].rename(columns={'Date': 'ds', 'Quantity': 'y'})

            # Ajouter des variables supplémentaires
            prophet_data['temperature'] = product_data['Temperature']
            prophet_data['holiday'] = product_data['Holiday']
            prophet_data['promotion'] = product_data['Promotion']

            # Créer et ajuster le modèle
            model = Prophet(
                yearly_seasonality=True,
                weekly_seasonality=True,
                daily_seasonality=False
            )

            # Ajouter des régresseurs
            model.add_regressor('temperature')
            model.add_regressor('holiday')
            model.add_regressor('promotion')

            model.fit(prophet_data)

            # Créer un DataFrame pour les périodes futures
            future = model.make_future_dataframe(periods=int(forecast_periods))

            # Ajouter les variables pour les périodes futures
            # Pour cet exemple, on utilise des valeurs moyennes ou typiques
            avg_temp = product_data['Temperature'].mean()
            avg_promotion = product_data['Promotion'].mean()

            future['temperature'] = avg_temp
            future['holiday'] = 0  # Par défaut, pas de jour férié
            future['promotion'] = avg_promotion

            # Si nous connaissons des jours fériés futurs ou des promotions, nous pourrions les ajouter ici

            # Faire la prévision
            forecast = model.predict(future)

            # Ajouter l'ID du produit
            forecast['ProductID'] = product

            # Ajouter à nos prévisions globales
            all_forecasts = pd.concat([all_forecasts, forecast[['ds', 'ProductID', 'yhat', 'yhat_lower', 'yhat_upper']]])

        # Renommer les colonnes
        all_forecasts = all_forecasts.rename(columns={
            'ds': 'Date',
            'yhat': 'Quantity',
            'yhat_lower': 'LowerBound',
            'yhat_upper': 'UpperBound'
        })

        # Filtrer seulement les périodes futures
        today = pd.Timestamp.today()
        future_forecasts = all_forecasts[all_forecasts['Date'] > today]

        # Enregistrer les prévisions dans un fichier CSV
        future_forecasts.to_csv('demand_forecast.csv', index=False)

        # Créer un graphique
        plt.figure(figsize=(12, 8))

        for product in products:
            product_forecast = future_forecasts[future_forecasts['ProductID'] == product]
            plt.plot(product_forecast['Date'], product_forecast['Quantity'], label=f'Produit {product}')

            # Ajouter l'intervalle de confiance
            plt.fill_between(
                product_forecast['Date'],
                product_forecast['LowerBound'],
                product_forecast['UpperBound'],
                alpha=0.2
            )

        plt.title('Prévision de la demande')
        plt.xlabel('Date')
        plt.ylabel('Quantité')
        plt.legend()
        plt.grid(True)
        plt.savefig('demand_forecast.png')

        # Afficher un résumé
        print(f"Prévisions générées pour {len(products)} produits sur {int(forecast_periods)} périodes")

        # Afficher les totaux prévus par produit
        print("\nDemande totale prévue par produit :")
        for product in products:
            product_forecast = future_forecasts[future_forecasts['ProductID'] == product]
            total_demand = product_forecast['Quantity'].sum()
            print(f"Produit {product}: {total_demand:.0f} unités")

        # Afficher les pics de demande
        print("\nPics de demande :")
        for product in products:
            product_forecast = future_forecasts[future_forecasts['ProductID'] == product]
            max_demand = product_forecast['Quantity'].max()
            max_date = product_forecast.loc[product_forecast['Quantity'].idxmax(), 'Date']
            print(f"Produit {product}: {max_demand:.0f} unités le {max_date.strftime('%Y-%m-%d')}")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(forecast_demand(sys.argv[1]))
    else:
        print("Usage: python forecast_demand.py <forecast_periods>")
        exit(1)
```

### 3. Détection d'anomalies pour la sécurité et la qualité

```delphi
procedure TFormAnomalyDetection.DetectAnomalies;
var
  CSVFile: TStringList;
  Process: TProcess;
  Output: TStringList;
begin
  // Préparer les données pour l'analyse
  CSVFile := TStringList.Create;
  try
    // En-tête
    CSVFile.Add('Timestamp,Temperature,Pressure,Vibration,PowerConsumption,ProductionRate');

    // Ajouter les données de surveillance
    SensorDataset.First;
    while not SensorDataset.Eof do
    begin
      CSVFile.Add(
        FormatDateTime('yyyy-mm-dd hh:nn:ss', SensorDataset.FieldByName('Timestamp').AsDateTime) + ',' +
        SensorDataset.FieldByName('Temperature').AsString + ',' +
        SensorDataset.FieldByName('Pressure').AsString + ',' +
        SensorDataset.FieldByName('Vibration').AsString + ',' +
        SensorDataset.FieldByName('PowerConsumption').AsString + ',' +
        SensorDataset.FieldByName('ProductionRate').AsString
      );

      SensorDataset.Next;
    end;

    // Sauvegarder le fichier
    CSVFile.SaveToFile('sensor_data.csv');
  finally
    CSVFile.Free;
  end;

  // Exécuter la détection d'anomalies
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('detect_anomalies.py');
    Process.Parameters.Add('sensor_data.csv');
    Process.Parameters.Add(FloatToStr(TrackBarSensitivity.Position / 10)); // Sensibilité (0.1 à 1.0)
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    StatusBar1.SimpleText := 'Détection d''anomalies en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    if Process.ExitCode = 0 then
    begin
      // Afficher le résumé des anomalies
      MemoResults.Lines.Clear;
      MemoResults.Lines.Add('DÉTECTION D''ANOMALIES :');
      MemoResults.Lines.Add('');

      for var i := 0 to Output.Count - 1 do
        MemoResults.Lines.Add(Output[i]);

      // Charger les graphiques
      if FileExists('anomalies_chart.png') then
        Image1.Picture.LoadFromFile('anomalies_chart.png');

      // Charger les anomalies détectées
      LoadAnomalies('anomalies.csv');

      StatusBar1.SimpleText := 'Détection d''anomalies terminée';
    end
    else
      ShowMessage('Erreur : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormAnomalyDetection.LoadAnomalies(const FileName: string);
var
  AnomaliesFile: TStringList;
  AnomalyCount: Integer;
begin
  // Réinitialiser l'ensemble de données des anomalies
  AnomaliesDataset.EmptyDataSet;

  AnomaliesFile := TStringList.Create;
  AnomalyCount := 0;

  try
    AnomaliesFile.LoadFromFile(FileName);

    // Vérifier l'en-tête
    if (AnomaliesFile.Count > 0) and
       (AnomaliesFile[0] = 'Timestamp,Temperature,Pressure,Vibration,PowerConsumption,ProductionRate,IsAnomaly,AnomalyScore,PrimaryFactor') then
    begin
      // Parcourir les résultats
      for var i := 1 to AnomaliesFile.Count - 1 do
      begin
        var Parts := AnomaliesFile[i].Split([',']);
        if Length(Parts) >= 9 then
        begin
          var TimestampStr := Parts[0];
          var Temperature := StrToFloatDef(Parts[1], 0);
          var Pressure := StrToFloatDef(Parts[2], 0);
          var Vibration := StrToFloatDef(Parts[3], 0);
          var PowerConsumption := StrToFloatDef(Parts[4], 0);
          var ProductionRate := StrToFloatDef(Parts[5], 0);
          var IsAnomaly := Parts[6] = 'True';
          var AnomalyScore := StrToFloatDef(Parts[7], 0);
          var PrimaryFactor := Parts[8];

          // Si c'est une anomalie, l'ajouter à l'ensemble de données
          if IsAnomaly then
          begin
            // Convertir la date du format 'YYYY-MM-DD HH:MM:SS' au format TDateTime
            var Timestamp := StrToDateTime(TimestampStr);

            AnomaliesDataset.Append;
            AnomaliesDataset.FieldByName('Timestamp').AsDateTime := Timestamp;
            AnomaliesDataset.FieldByName('Temperature').AsFloat := Temperature;
            AnomaliesDataset.FieldByName('Pressure').AsFloat := Pressure;
            AnomaliesDataset.FieldByName('Vibration').AsFloat := Vibration;
            AnomaliesDataset.FieldByName('PowerConsumption').AsFloat := PowerConsumption;
            AnomaliesDataset.FieldByName('ProductionRate').AsFloat := ProductionRate;
            AnomaliesDataset.FieldByName('AnomalyScore').AsFloat := AnomalyScore;
            AnomaliesDataset.FieldByName('PrimaryFactor').AsString := PrimaryFactor;
            AnomaliesDataset.Post;

            Inc(AnomalyCount);
          end;
        end;
      end;
    end;

    // Mettre à jour les informations
    LabelAnomalyCount.Caption := 'Anomalies détectées : ' + IntToStr(AnomalyCount);
    LabelTotalRecords.Caption := 'Total d''enregistrements : ' + IntToStr(AnomaliesFile.Count - 1);

    // Mettre à jour le résumé des facteurs d'anomalie
    UpdateAnomalyFactors;
  finally
    AnomaliesFile.Free;
  end;
end;

procedure TFormAnomalyDetection.UpdateAnomalyFactors;
var
  FactorCounts: TDictionary<string, Integer>;
  TopFactor: string;
  TopCount: Integer;
begin
  // Compter les facteurs d'anomalie
  FactorCounts := TDictionary<string, Integer>.Create;
  TopFactor := '';
  TopCount := 0;

  try
    AnomaliesDataset.First;
    while not AnomaliesDataset.Eof do
    begin
      var Factor := AnomaliesDataset.FieldByName('PrimaryFactor').AsString;

      if FactorCounts.ContainsKey(Factor) then
        FactorCounts[Factor] := FactorCounts[Factor] + 1
      else
        FactorCounts.Add(Factor, 1);

      AnomaliesDataset.Next;
    end;

    // Trouver le facteur le plus courant
    for var Factor in FactorCounts.Keys do
    begin
      if FactorCounts[Factor] > TopCount then
      begin
        TopCount := FactorCounts[Factor];
        TopFactor := Factor;
      end;
    end;

    // Mettre à jour l'étiquette
    if TopFactor <> '' then
      LabelPrimaryFactor.Caption := 'Cause principale : ' + TopFactor + ' (' + IntToStr(TopCount) + ' occurrences)';
  finally
    FactorCounts.Free;
  end;
end;
```

Le script Python pour la détection d'anomalies (detect_anomalies.py) :

```python
import sys
import pandas as pd
import numpy as np
from sklearn.ensemble import IsolationForest
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
from matplotlib.dates import DateFormatter

def detect_anomalies(data_path, sensitivity):
    try:
        # Charger les données
        data = pd.read_csv(data_path)

        # Convertir la colonne timestamp en datetime
        data['Timestamp'] = pd.to_datetime(data['Timestamp'])

        # Préparer les caractéristiques pour la détection d'anomalies
        features = ['Temperature', 'Pressure', 'Vibration', 'PowerConsumption', 'ProductionRate']
        X = data[features]

        # Standardiser les données
        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X)

        # Configurer et entraîner le modèle d'Isolation Forest
        # La sensibilité contrôle la facilité avec laquelle le modèle identifie les anomalies
        # Plus la contamination est basse, moins il y aura d'anomalies détectées
        contamination = 1.0 - float(sensitivity)  # Inverser la sensibilité

        model = IsolationForest(
            contamination=contamination,
            random_state=42,
            n_estimators=100
        )

        # Entraîner le modèle
        model.fit(X_scaled)

        # Prédire les anomalies
        # -1 pour les anomalies, 1 pour les observations normales
        predictions = model.predict(X_scaled)
        anomaly_scores = model.decision_function(X_scaled)

        # Convertir les scores pour qu'ils soient plus intuitifs
        # Un score élevé indique une anomalie plus probable
        anomaly_scores = -anomaly_scores

        # Ajouter les résultats au DataFrame
        data['IsAnomaly'] = predictions == -1
        data['AnomalyScore'] = anomaly_scores

        # Déterminer le facteur principal de chaque anomalie
        primary_factors = []

        for idx, row in data.iterrows():
            if row['IsAnomaly']:
                # Calculer l'écart par rapport à la moyenne pour chaque caractéristique
                deviations = {}
                for feature in features:
                    mean_value = X[feature].mean()
                    std_value = X[feature].std()
                    z_score = (row[feature] - mean_value) / std_value
                    deviations[feature] = abs(z_score)

                # Le facteur avec la plus grande déviation est considéré comme la cause principale
                primary_factor = max(deviations, key=deviations.get)
                primary_factors.append(primary_factor)
            else:
                primary_factors.append("")

        data['PrimaryFactor'] = primary_factors

        # Sauvegarder les résultats
        data.to_csv('anomalies.csv', index=False)

        # Créer des visualisations

        # 1. Graphique des séries temporelles avec anomalies marquées
        plt.figure(figsize=(15, 10))

        # Sous-graphique pour chaque caractéristique
        for i, feature in enumerate(features):
            plt.subplot(len(features), 1, i+1)

            # Tracer la série temporelle
            plt.plot(data['Timestamp'], data[feature], label=feature, color='blue')

            # Marquer les anomalies
            anomalies = data[data['IsAnomaly']]
            plt.scatter(anomalies['Timestamp'], anomalies[feature], color='red', label='Anomalies')

            plt.title(feature)
            plt.grid(True)
            plt.legend()

            # Formater l'axe des x
            plt.gca().xaxis.set_major_formatter(DateFormatter('%Y-%m-%d'))
            plt.xticks(rotation=45)

        plt.tight_layout()
        plt.savefig('anomalies_chart.png')

        # Afficher un résumé
        anomaly_count = data['IsAnomaly'].sum()
        total_records = len(data)
        anomaly_percentage = (anomaly_count / total_records) * 100

        print(f"Détection d'anomalies terminée")
        print(f"Total d'enregistrements analysés : {total_records}")
        print(f"Anomalies détectées : {anomaly_count} ({anomaly_percentage:.2f}%)")

        # Afficher les facteurs principaux d'anomalie
        if anomaly_count > 0:
            factor_counts = data[data['IsAnomaly']]['PrimaryFactor'].value_counts()
            print("\nFacteurs principaux d'anomalie :")
            for factor, count in factor_counts.items():
                print(f"{factor}: {count} occurrences ({(count/anomaly_count)*100:.1f}%)")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 2:
        exit(detect_anomalies(sys.argv[1], sys.argv[2]))
    else:
        print("Usage: python detect_anomalies.py <data_path> <sensitivity>")
        exit(1)
```

## Intégration des modèles prédictifs dans des applications réelles

### 1. Création d'un service de prédiction

Pour intégrer vos modèles dans plusieurs applications, vous pouvez créer un service Web :

```delphi
// Créer un service REST avec DataSnap
type
  TPredictionService = class(TDataModule)
    DSServer1: TDSServer;
    DSServerClass1: TDSServerClass;
    procedure DataModuleCreate(Sender: TObject);
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TPredictionMethods = class(TDSServerModule)
  private
    function ExecutePythonScript(const ScriptName: string; const Params: array of string): string;
  public
    function Predict(const ModelName, InputData: string): string;
    function Train(const ModelName, TrainingData: string): string;
    function GetModelInfo(const ModelName: string): string;
  end;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TPredictionService.DataModuleCreate(Sender: TObject);
begin
  DSServer1.Start;
end;

procedure TPredictionService.DSServerClass1GetClass(
  DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := TPredictionMethods;
end;

{ TPredictionMethods }

function TPredictionMethods.ExecutePythonScript(const ScriptName: string;
  const Params: array of string): string;
var
  Process: TProcess;
  Output: TStringList;
  Param: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add(ScriptName);

    // Ajouter les paramètres
    for Param in Params do
      Process.Parameters.Add(Param);

    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécuter le script
    Process.Execute;

    // Lire la sortie
    Output.LoadFromStream(Process.Output);
    Result := Output.Text;
  finally
    Process.Free;
    Output.Free;
  end;
end;

function TPredictionMethods.Predict(const ModelName, InputData: string): string;
var
  DataFile: TStringList;
  TempFileName: string;
begin
  // Sauvegarder les données d'entrée dans un fichier temporaire
  TempFileName := 'temp_input_' + FormatDateTime('yyyymmddhhnnss', Now) + '.csv';
  DataFile := TStringList.Create;
  try
    DataFile.Text := InputData;
    DataFile.SaveToFile(TempFileName);
  finally
    DataFile.Free;
  end;

  // Exécuter la prédiction
  Result := ExecutePythonScript('service_predict.py', [ModelName, TempFileName]);

  // Nettoyer le fichier temporaire
  if FileExists(TempFileName) then
    DeleteFile(TempFileName);
end;

function TPredictionMethods.Train(const ModelName, TrainingData: string): string;
var
  DataFile: TStringList;
  TempFileName: string;
begin
  // Sauvegarder les données d'entraînement dans un fichier temporaire
  TempFileName := 'temp_training_' + FormatDateTime('yyyymmddhhnnss', Now) + '.csv';
  DataFile := TStringList.Create;
  try
    DataFile.Text := TrainingData;
    DataFile.SaveToFile(TempFileName);
  finally
    DataFile.Free;
  end;

  // Exécuter l'entraînement
  Result := ExecutePythonScript('service_train.py', [ModelName, TempFileName]);

  // Nettoyer le fichier temporaire
  if FileExists(TempFileName) then
    DeleteFile(TempFileName);
end;

function TPredictionMethods.GetModelInfo(const ModelName: string): string;
begin
  // Récupérer les informations sur le modèle
  Result := ExecutePythonScript('service_model_info.py', [ModelName]);
end;
```

Les scripts Python correspondants pour le service :

```python
# service_predict.py
import sys
import os
import pandas as pd
import numpy as np
import joblib
import json

def predict(model_name, input_file):
    try:
        # Vérifier si le modèle existe
        model_path = f"models/{model_name}.pkl"
        if not os.path.exists(model_path):
            return json.dumps({"error": f"Le modèle '{model_name}' n'existe pas"})

        # Charger le modèle
        model = joblib.load(model_path)

        # Charger les données d'entrée
        input_data = pd.read_csv(input_file)

        # Faire la prédiction
        predictions = model.predict(input_data)

        # Créer un dictionnaire de résultats
        results = {
            "predictions": predictions.tolist(),
            "model": model_name,
            "rows_processed": len(input_data)
        }

        # Si le modèle supporte les probabilités, les inclure
        if hasattr(model, 'predict_proba'):
            try:
                probabilities = model.predict_proba(input_data)
                results["probabilities"] = probabilities.tolist()
            except:
                pass  # Ignorer si predict_proba échoue

        return json.dumps(results)
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 2:
        print(predict(sys.argv[1], sys.argv[2]))
    else:
        print(json.dumps({"error": "Usage: python service_predict.py <model_name> <input_file>"}))
```

```python
# service_train.py
import sys
import os
import pandas as pd
import numpy as np
import joblib
import json
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn.metrics import accuracy_score, r2_score, mean_squared_error

def train(model_name, training_file):
    try:
        # Créer le dossier models s'il n'existe pas
        os.makedirs("models", exist_ok=True)

        # Charger les données d'entraînement
        data = pd.read_csv(training_file)

        # Déterminer si c'est un problème de classification ou de régression
        # Supposons que la dernière colonne est la cible
        X = data.iloc[:, :-1]
        y = data.iloc[:, -1]

        # Déterminer le type de tâche (classification ou régression)
        unique_values = y.nunique()
        is_classification = unique_values <= 10  # Heuristique : si moins de 10 valeurs uniques, c'est une classification

        # Diviser les données
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

        # Sélectionner et entraîner le modèle
        if is_classification:
            if 'logistic' in model_name.lower():
                model = LogisticRegression(random_state=42)
            else:
                model = RandomForestClassifier(n_estimators=100, random_state=42)
        else:
            if 'linear' in model_name.lower():
                model = LinearRegression()
            else:
                model = RandomForestRegressor(n_estimators=100, random_state=42)

        # Entraîner le modèle
        model.fit(X_train, y_train)

        # Évaluer le modèle
        y_pred = model.predict(X_test)

        # Calculer les métriques appropriées
        metrics = {}
        if is_classification:
            metrics["accuracy"] = accuracy_score(y_test, y_pred)
        else:
            metrics["r2"] = r2_score(y_test, y_pred)
            metrics["mse"] = mean_squared_error(y_test, y_pred)
            metrics["rmse"] = np.sqrt(metrics["mse"])

        # Sauvegarder le modèle
        joblib.dump(model, f"models/{model_name}.pkl")

        # Sauvegarder les noms des colonnes
        with open(f"models/{model_name}_columns.json", 'w') as f:
            json.dump(list(X.columns), f)

        # Sauvegarder les métriques et autres informations
        model_info = {
            "name": model_name,
            "type": "classification" if is_classification else "regression",
            "features": list(X.columns),
            "target": data.columns[-1],
            "metrics": metrics,
            "training_samples": len(X_train),
            "testing_samples": len(X_test)
        }

        with open(f"models/{model_name}_info.json", 'w') as f:
            json.dump(model_info, f)

        return json.dumps({
            "success": True,
            "message": f"Modèle '{model_name}' entraîné avec succès",
            "metrics": metrics,
            "type": "classification" if is_classification else "regression"
        })
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 2:
        print(train(sys.argv[1], sys.argv[2]))
    else:
        print(json.dumps({"error": "Usage: python service_train.py <model_name> <training_file>"}))
```

```python
# service_model_info.py
import sys
import os
import json
import joblib

def get_model_info(model_name):
    try:
        # Vérifier si le modèle existe
        model_path = f"models/{model_name}.pkl"
        info_path = f"models/{model_name}_info.json"

        if not os.path.exists(model_path):
            return json.dumps({"error": f"Le modèle '{model_name}' n'existe pas"})

        # Charger les informations du modèle si disponibles
        if os.path.exists(info_path):
            with open(info_path, 'r') as f:
                info = json.load(f)
            return json.dumps(info)

        # Sinon, charger le modèle et extraire des informations de base
        model = joblib.load(model_path)
        model_type = type(model).__name__

        basic_info = {
            "name": model_name,
            "type": model_type,
            "creation_date": os.path.getctime(model_path)
        }

        # Ajouter des informations spécifiques selon le type de modèle
        if hasattr(model, 'feature_importances_'):
            basic_info["has_feature_importances"] = True

        if hasattr(model, 'coef_'):
            basic_info["has_coefficients"] = True

        return json.dumps(basic_info)
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print(get_model_info(sys.argv[1]))
    else:
        print(json.dumps({"error": "Usage: python service_model_info.py <model_name>"}))
```

### 2. Création d'une interface utilisateur pour la gestion des modèles

Pour faciliter la gestion des modèles prédictifs, vous pouvez créer une interface dédiée :

```delphi
procedure TFormModelManager.ListModels;
var
  Process: TProcess;
  Output: TStringList;
  Models: TJSONArray;
  Model: TJSONObject;
  i: Integer;
begin
  // Lister tous les modèles disponibles
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('list_models.py');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    // Effacer la liste actuelle
    ListViewModels.Items.Clear;

    // Analyser la réponse JSON
    if Output.Count > 0 then
    begin
      try
        Models := TJSONObject.ParseJSONValue(Output.Text) as TJSONArray;
        try
          for i := 0 to Models.Count - 1 do
          begin
            Model := Models.Items[i] as TJSONObject;

            var Item := ListViewModels.Items.Add;
            Item.Caption := Model.GetValue<string>('name');
            Item.SubItems.Add(Model.GetValue<string>('type'));
            Item.SubItems.Add(FormatDateTime('dd/mm/yyyy hh:nn',
                              UnixToDateTime(Model.GetValue<Int64>('creation_date'))));

            // Ajouter des métriques si disponibles
            if Model.TryGetValue<TJSONObject>('metrics', var Metrics) then
            begin
              var MetricsStr := '';

              if Model.GetValue<string>('type') = 'classification' then
                MetricsStr := Format('Accuracy: %.4f', [Metrics.GetValue<Double>('accuracy')])
              else
                MetricsStr := Format('R²: %.4f, RMSE: %.4f',
                             [Metrics.GetValue<Double>('r2'), Metrics.GetValue<Double>('rmse')]);

              Item.SubItems.Add(MetricsStr);
            end
            else
              Item.SubItems.Add('');
          end;
        finally
          Models.Free;
        end;
      except
        ShowMessage('Erreur lors de l''analyse de la liste des modèles');
      end;
    end;

    // Mettre à jour le statut
    StatusBar1.SimpleText := Format('%d modèles trouvés', [ListViewModels.Items.Count]);
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormModelManager.ButtonTestModelClick(Sender: TObject);
var
  Process: TProcess;
  Output: TStringList;
  TestFile: TStringList;
  SelectedModel: string;
begin
  // Vérifier qu'un modèle est sélectionné
  if ListViewModels.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un modèle à tester');
    Exit;
  end;

  SelectedModel := ListViewModels.Selected.Caption;

  // Créer un fichier de test avec les données de l'interface
  TestFile := TStringList.Create;
  try
    // En-tête avec les noms de colonnes
    TestFile.Add(MemoFeatureNames.Text);

    // Valeurs de test
    TestFile.Add(EditTestValues.Text);

    // Sauvegarder dans un fichier temporaire
    TestFile.SaveToFile('temp_test.csv');
  finally
    TestFile.Free;
  end;

  // Exécuter le test
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('test_model.py');
    Process.Parameters.Add(SelectedModel);
    Process.Parameters.Add('temp_test.csv');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    StatusBar1.SimpleText := 'Test en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    // Analyser la réponse JSON
    if Output.Count > 0 then
    begin
      try
        var ResultObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
        try
          if ResultObj.TryGetValue<string>('error', var ErrorMsg) then
            ShowMessage('Erreur : ' + ErrorMsg)
          else
          begin
            // Afficher la prédiction
            var PredictionsArray := ResultObj.GetValue<TJSONArray>('predictions');
            var Prediction := PredictionsArray.Items[0];

            // Afficher les probabilités si disponibles
            var ProbsStr := '';
            if ResultObj.TryGetValue<TJSONArray>('probabilities', var ProbsArray) then
            begin
              var Probs := ProbsArray.Items[0] as TJSONArray;
              for var i := 0 to Probs.Count - 1 do
              begin
                if i > 0 then ProbsStr := ProbsStr + ', ';
                ProbsStr := ProbsStr + Format('Classe %d: %.2f%%', [i, Probs.Items[i].GetValue<Double> * 100]);
              end;
            end;

            LabelPrediction.Caption := 'Prédiction : ' + Prediction.ToString;

            if ProbsStr <> '' then
              LabelProbabilities.Caption := 'Probabilités : ' + ProbsStr
            else
              LabelProbabilities.Caption := '';

            StatusBar1.SimpleText := 'Test terminé';
          end;
        finally
          ResultObj.Free;
        end;
      except
        ShowMessage('Erreur lors de l''analyse du résultat');
      end;
    end;
  finally
    Process.Free;
    Output.Free;

    // Supprimer le fichier temporaire
    if FileExists('temp_test.csv') then
      DeleteFile('temp_test.csv');
  end;
end;

procedure TFormModelManager.ViewModelDetails(const ModelName: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('service_model_info.py');
    Process.Parameters.Add(ModelName);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    // Analyser la réponse JSON
    if Output.Count > 0 then
    begin
      try
        var InfoObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
        try
          // Créer une fenêtre de détails
          var FormDetails := TFormModelDetails.Create(Self);
          try
            FormDetails.Caption := 'Détails du modèle : ' + ModelName;

            // Remplir les informations de base
            FormDetails.LabelModelName.Caption := 'Nom du modèle : ' + ModelName;
            FormDetails.LabelModelType.Caption := 'Type : ' + InfoObj.GetValue<string>('type');

            // Ajouter les caractéristiques si disponibles
            if InfoObj.TryGetValue<TJSONArray>('features', var Features) then
            begin
              FormDetails.MemoFeatures.Lines.Clear;
              for var i := 0 to Features.Count - 1 do
                FormDetails.MemoFeatures.Lines.Add(Features.Items[i].Value);
            end;

            // Ajouter les métriques si disponibles
            if InfoObj.TryGetValue<TJSONObject>('metrics', var Metrics) then
            begin
              FormDetails.MemoMetrics.Lines.Clear;

              // Parcourir toutes les métriques
              for var Metric in Metrics do
                FormDetails.MemoMetrics.Lines.Add(Metric.JsonString.Value + ' : ' + Metric.JsonValue.ToString);
            end;

            // Afficher la fenêtre
            FormDetails.ShowModal;
          finally
            FormDetails.Free;
          end;
        finally
          InfoObj.Free;
        end;
      except
        ShowMessage('Erreur lors de l''analyse des détails du modèle');
      end;
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python pour lister les modèles :

```python
import os
import json
import glob

def list_models():
    # Récupérer tous les fichiers .pkl dans le dossier models
    model_files = glob.glob("models/*.pkl")

    # Extraire les noms des modèles
    models = []

    for model_file in model_files:
        # Extraire le nom de base du modèle (sans le chemin ni l'extension)
        model_name = os.path.splitext(os.path.basename(model_file))[0]

        # Créer une entrée de base
        model_entry = {
            "name": model_name,
            "creation_date": os.path.getctime(model_file)
        }

        # Ajouter des informations détaillées si disponibles
        info_file = f"models/{model_name}_info.json"
        if os.path.exists(info_file):
            with open(info_file, 'r') as f:
                model_info = json.load(f)

                # Fusionner les informations
                model_entry.update(model_info)
        else:
            model_entry["type"] = "unknown"

        models.append(model_entry)

    # Trier par date de création (du plus récent au plus ancien)
    models.sort(key=lambda x: x["creation_date"], reverse=True)

    return json.dumps(models)

if __name__ == "__main__":
    print(list_models())
```

### 3. Intégration dans une application métier

Enfin, voici comment intégrer un modèle prédictif dans une application métier :

```delphi
procedure TFormSalesApp.PredictProductDemand;
var
  ClientService: TPredictionClient;
  Response: string;
  ResultObj: TJSONObject;
begin
  // Créer le client du service de prédiction
  ClientService := TPredictionClient.Create;
  try
    // Préparer les données d'entrée
    var InputData := 'Date,ProductID,Season,Holiday,Promotion,WeekDay' + sLineBreak;

    // Ajouter une ligne pour chaque produit
    ProductsDataset.First;
    while not ProductsDataset.Eof do
    begin
      InputData := InputData +
                  FormatDateTime('yyyy-mm-dd', DateTimePicker1.Date) + ',' +
                  ProductsDataset.FieldByName('ProductID').AsString + ',' +
                  GetSeasonValue + ',' +
                  BoolToStr(CheckBoxHoliday.Checked, '1', '0') + ',' +
                  ComboBoxPromotion.Text + ',' +
                  IntToStr(DayOfWeek(DateTimePicker1.Date)) + sLineBreak;

      ProductsDataset.Next;
    end;

    // Appeler le service de prédiction
    Response := ClientService.Predict('demand_forecast_model', InputData);

    // Analyser la réponse
    ResultObj := TJSONObject.ParseJSONValue(Response) as TJSONObject;
    try
      if ResultObj.TryGetValue<string>('error', var ErrorMsg) then
        ShowMessage('Erreur : ' + ErrorMsg)
      else
      begin
        // Récupérer les prédictions
        var Predictions := ResultObj.GetValue<TJSONArray>('predictions');

        // Mettre à jour les prévisions dans l'ensemble de données
        ProductsDataset.First;
        for var i := 0 to Predictions.Count - 1 do
        begin
          if not ProductsDataset.Eof then
          begin
            ProductsDataset.Edit;
            ProductsDataset.FieldByName('ForecastDemand').AsFloat := Predictions.Items[i].GetValue<Double>;

            // Calculer le stock recommandé (exemple simplifié)
            var CurrentStock := ProductsDataset.FieldByName('CurrentStock').AsFloat;
            var Demand := ProductsDataset.FieldByName('ForecastDemand').AsFloat;
            var LeadTime := ProductsDataset.FieldByName('SupplierLeadTime').AsInteger;
            var SafetyStock := Demand * 0.2;  // 20% de marge de sécurité

            var RecommendedOrder := Max(0, Demand * LeadTime - CurrentStock + SafetyStock);
            ProductsDataset.FieldByName('RecommendedOrder').AsFloat := Round(RecommendedOrder);

            ProductsDataset.Post;
            ProductsDataset.Next;
          end;
        end;

        // Mettre à jour le statut
        StatusBar1.SimpleText := 'Prévisions mises à jour pour ' + Predictions.Count.ToString + ' produits';

        // Mettre à jour le graphique
        UpdateDemandChart;
      end;
    finally
      ResultObj.Free;
    end;
  finally
    ClientService.Free;
  end;
end;

procedure TFormSalesApp.UpdateDemandChart;
begin
  // Effacer les séries existantes
  Chart1.Series[0].Clear;  // Demande actuelle
  Chart1.Series[1].Clear;  // Demande prévue

  // Ajouter les données
  ProductsDataset.First;
  while not ProductsDataset.Eof do
  begin
    var ProductName := ProductsDataset.FieldByName('ProductName').AsString;
    var CurrentDemand := ProductsDataset.FieldByName('CurrentDemand').AsFloat;
    var ForecastDemand := ProductsDataset.FieldByName('ForecastDemand').AsFloat;

    Chart1.Series[0].Add(CurrentDemand, ProductName, clBlue);
    Chart1.Series[1].Add(ForecastDemand, ProductName, clRed);

    ProductsDataset.Next;
  end;
end;

function TFormSalesApp.GetSeasonValue: string;
var
  Month: Integer;
begin
  Month := MonthOf(DateTimePicker1.Date);

  // Déterminer la saison en fonction du mois
  case Month of
    12, 1, 2: Result := 'Winter';
    3, 4, 5: Result := 'Spring';
    6, 7, 8: Result := 'Summer';
    9, 10, 11: Result := 'Fall';
  else
    Result := 'Unknown';
  end;
end;

procedure TFormSalesApp.GenerateOrderProposals;
begin
  // Créer un rapport de propositions de commandes
  OrderReportDataset.EmptyDataSet;

  ProductsDataset.First;
  while not ProductsDataset.Eof do
  begin
    var RecommendedOrder := ProductsDataset.FieldByName('RecommendedOrder').AsFloat;

    // Ne créer une proposition que si une commande est recommandée
    if RecommendedOrder > 0 then
    begin
      OrderReportDataset.Append;
      OrderReportDataset.FieldByName('ProductID').AsInteger :=
        ProductsDataset.FieldByName('ProductID').AsInteger;
      OrderReportDataset.FieldByName('ProductName').AsString :=
        ProductsDataset.FieldByName('ProductName').AsString;
      OrderReportDataset.FieldByName('CurrentStock').AsFloat :=
        ProductsDataset.FieldByName('CurrentStock').AsFloat;
      OrderReportDataset.FieldByName('ForecastDemand').AsFloat :=
        ProductsDataset.FieldByName('ForecastDemand').AsFloat;
      OrderReportDataset.FieldByName('RecommendedOrder').AsFloat := RecommendedOrder;
      OrderReportDataset.FieldByName('UnitPrice').AsFloat :=
        ProductsDataset.FieldByName('UnitPrice').AsFloat;
      OrderReportDataset.FieldByName('TotalCost').AsFloat :=
        RecommendedOrder * ProductsDataset.FieldByName('UnitPrice').AsFloat;
      OrderReportDataset.FieldByName('SupplierID').AsInteger :=
        ProductsDataset.FieldByName('SupplierID').AsInteger;
      OrderReportDataset.FieldByName('SupplierName').AsString :=
        ProductsDataset.FieldByName('SupplierName').AsString;
      OrderReportDataset.Post;
    end;

    ProductsDataset.Next;
  end;

  // Afficher le rapport
  if OrderReportDataset.RecordCount > 0 then
  begin
    StatusBar1.SimpleText := Format('%d propositions de commandes générées',
                                 [OrderReportDataset.RecordCount]);

    // Calculer le coût total
    var TotalCost := 0.0;
    OrderReportDataset.First;
    while not OrderReportDataset.Eof do
    begin
      TotalCost := TotalCost + OrderReportDataset.FieldByName('TotalCost').AsFloat;
      OrderReportDataset.Next;
    end;

    LabelTotalCost.Caption := Format('Coût total : %.2f €', [TotalCost]);

    // Afficher l'onglet rapport
    PageControl1.ActivePage := TabSheetOrderReport;
  end
  else
    ShowMessage('Aucune commande n''est recommandée pour le moment');
end;
```

## Bonnes pratiques pour les modèles prédictifs en production

### 1. Surveillance et maintenance des modèles

Les modèles de ML doivent être régulièrement évalués et mis à jour :

```delphi
procedure TFormModelMonitoring.CheckModelPerformance;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('monitor_models.py');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    // Afficher les résultats du monitoring
    MemoResults.Lines.Clear;
    MemoResults.Lines.AddStrings(Output);

    // Analyser la réponse pour la mise à jour du tableau de bord
    try
      var ResultsObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
      try
        var ModelsArray := ResultsObj.GetValue<TJSONArray>('models');

        // Mettre à jour les statistiques
        LabelTotalModels.Caption := Format('Nombre total de modèles : %d', [ModelsArray.Count]);

        var NeedsAttention := 0;
        var RecentDrift := 0;

        for var i := 0 to ModelsArray.Count - 1 do
        begin
          var ModelObj := ModelsArray.Items[i] as TJSONObject;

          if ModelObj.GetValue<Boolean>('needs_attention') then
            Inc(NeedsAttention);

          if ModelObj.GetValue<Boolean>('recent_drift') then
            Inc(RecentDrift);
        end;

        LabelNeedsAttention.Caption := Format('Modèles nécessitant attention : %d', [NeedsAttention]);
        LabelRecentDrift.Caption := Format('Modèles avec dérive récente : %d', [RecentDrift]);
      finally
        ResultsObj.Free;
      end;
    except
      // Ignorer les erreurs d'analyse JSON
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Script Python pour la surveillance des modèles :

```python
import os
import json
import glob
import pandas as pd
import numpy as np
import joblib
from datetime import datetime, timedelta

def monitor_models():
    # Récupérer tous les fichiers .pkl dans le dossier models
    model_files = glob.glob("models/*.pkl")

    models_status = []

    for model_file in model_files:
        # Extraire le nom de base du modèle
        model_name = os.path.splitext(os.path.basename(model_file))[0]
        info_file = f"models/{model_name}_info.json"

        model_status = {
            "name": model_name,
            "last_checked": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "needs_attention": False,
            "recent_drift": False,
            "performance_trend": "stable",
            "messages": []
        }

        # Vérifier l'âge du modèle
        model_age_days = (datetime.now() - datetime.fromtimestamp(os.path.getctime(model_file))).days

        if model_age_days > 180:  # Plus de 6 mois
            model_status["needs_attention"] = True
            model_status["messages"].append(f"Le modèle a {model_age_days} jours - considérez de le réentraîner")

        # Vérifier les performances du modèle si les informations sont disponibles
        if os.path.exists(info_file):
            with open(info_file, 'r') as f:
                model_info = json.load(f)

            # Vérifier si le fichier de suivi des performances existe
            perf_file = f"models/{model_name}_performance.csv"
            if os.path.exists(perf_file):
                perf_data = pd.read_csv(perf_file)

                # Vérifier si les performances se dégradent
                if len(perf_data) >= 2:
                    # Comparer les deux dernières mesures de performance
                    last_perf = perf_data.iloc[-1]
                    prev_perf = perf_data.iloc[-2]

                    # Choisir la métrique en fonction du type de modèle
                    if model_info.get("type") == "classification":
                        metric = "accuracy"
                    else:
                        metric = "r2"

                    if last_perf[metric] < prev_perf[metric] * 0.95:  # Dégradation de 5% ou plus
                        model_status["recent_drift"] = True
                        model_status["performance_trend"] = "declining"
                        model_status["needs_attention"] = True
                        model_status["messages"].append(
                            f"Dégradation des performances détectée: {prev_perf[metric]:.4f} -> {last_perf[metric]:.4f}"
                        )
                    elif last_perf[metric] > prev_perf[metric] * 1.05:  # Amélioration de 5% ou plus
                        model_status["performance_trend"] = "improving"

                    # Vérifier la date du dernier test
                    last_test_date = datetime.strptime(last_perf["date"], "%Y-%m-%d")
                    days_since_last_test = (datetime.now() - last_test_date).days

                    if days_since_last_test > 30:  # Plus d'un mois
                        model_status["messages"].append(
                            f"Dernier test de performance il y a {days_since_last_test} jours"
                        )
            else:
                model_status["messages"].append("Aucun historique de performance disponible")
        else:
            model_status["messages"].append("Informations détaillées non disponibles")

        models_status.append(model_status)

    # Préparer le résultat avec tous les modèles analysés
    result = {
        "timestamp": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
        "models_count": len(models_status),
        "models": models_status
    }

    return json.dumps(result)

if __name__ == "__main__":
    print(monitor_models())
```

### 2. Réentraînement périodique des modèles

Pour maintenir la précision de vos modèles au fil du temps :

```delphi
procedure TFormModelManagement.ScheduleRetraining;
begin
  // Sauvegarder le calendrier de réentraînement
  var RetrainingSettings := TStringList.Create;
  try
    // Configuration de base
    RetrainingSettings.Add('[General]');
    RetrainingSettings.Add('Enabled=' + BoolToStr(CheckBoxEnableSchedule.Checked, True));
    RetrainingSettings.Add('Frequency=' + ComboBoxFrequency.Text);

    // Liste des modèles à réentraîner
    RetrainingSettings.Add('[Models]');

    // Parcourir tous les modèles sélectionnés dans la liste
    for var i := 0 to CheckListBoxModels.Items.Count - 1 do
    begin
      if CheckListBoxModels.Checked[i] then
        RetrainingSettings.Add(CheckListBoxModels.Items[i] + '=True')
      else
        RetrainingSettings.Add(CheckListBoxModels.Items[i] + '=False');
    end;

    // Configuration des sources de données
    RetrainingSettings.Add('[DataSources]');
    RetrainingSettings.Add('UseHistorical=' + BoolToStr(RadioButtonHistorical.Checked, True));
    RetrainingSettings.Add('UseNewData=' + BoolToStr(RadioButtonNewData.Checked, True));
    RetrainingSettings.Add('DataFolder=' + EditDataFolder.Text);

    // Notification
    RetrainingSettings.Add('[Notification]');
    RetrainingSettings.Add('SendEmail=' + BoolToStr(CheckBoxEmailNotification.Checked, True));
    RetrainingSettings.Add('EmailAddress=' + EditEmailAddress.Text);

    // Sauvegarder le fichier de configuration
    RetrainingSettings.SaveToFile('retraining_schedule.ini');

    // Créer une tâche Windows si activé
    if CheckBoxEnableSchedule.Checked then
      CreateWindowsTask;

    ShowMessage('Calendrier de réentraînement enregistré avec succès');
  finally
    RetrainingSettings.Free;
  end;
end;

procedure TFormModelManagement.CreateWindowsTask;
var
  ScriptFile: TStringList;
  Process: TProcess;
  Output: TStringList;
begin
  // Créer un script batch pour le réentraînement
  ScriptFile := TStringList.Create;
  try
    ScriptFile.Add('@echo off');
    ScriptFile.Add('echo Démarrage du réentraînement des modèles - %date% %time%');
    ScriptFile.Add('python retrain_models.py');
    ScriptFile.Add('echo Réentraînement terminé - %date% %time%');

    ScriptFile.SaveToFile('retrain_models.bat');
  finally
    ScriptFile.Free;
  end;

  // Déterminer la fréquence
  var Schedule := '';
  case ComboBoxFrequency.ItemIndex of
    0: Schedule := '/sc DAILY';     // Quotidien
    1: Schedule := '/sc WEEKLY';    // Hebdomadaire
    2: Schedule := '/sc MONTHLY';   // Mensuel
  end;

  // Créer la tâche Windows
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'schtasks';
    Process.Parameters.Add('/create');
    Process.Parameters.Add('/tn MLModelsRetraining');
    Process.Parameters.Add('/tr "' + GetCurrentDir + '\retrain_models.bat"');
    Process.Parameters.Add(Schedule);
    Process.Parameters.Add('/st 02:00');  // À 2h du matin
    Process.Parameters.Add('/f');         // Forcer la création même si la tâche existe
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    // Vérifier si la création a réussi
    if Process.ExitCode = 0 then
      StatusBar1.SimpleText := 'Tâche de réentraînement programmée avec succès'
    else
      ShowMessage('Erreur lors de la création de la tâche Windows : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormModelManagement.RetrainSelectedModel;
var
  SelectedModel: string;
  Process: TProcess;
  Output: TStringList;
begin
  // Vérifier qu'un modèle est sélectionné
  if ListViewModels.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un modèle à réentraîner');
    Exit;
  end;

  SelectedModel := ListViewModels.Selected.Caption;

  if MessageDlg('Voulez-vous réentraîner le modèle "' + SelectedModel + '" ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Process := TProcess.Create(nil);
    Output := TStringList.Create;

    try
      Process.Executable := 'python';
      Process.Parameters.Add('manual_retrain.py');
      Process.Parameters.Add(SelectedModel);
      Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

      StatusBar1.SimpleText := 'Réentraînement en cours...';
      Screen.Cursor := crHourGlass;

      Process.Execute;
      Output.LoadFromStream(Process.Output);

      Screen.Cursor := crDefault;

      // Analyser la réponse JSON
      if Output.Count > 0 then
      begin
        try
          var ResultObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
          try
            if ResultObj.TryGetValue<string>('error', var ErrorMsg) then
              ShowMessage('Erreur : ' + ErrorMsg)
            else if ResultObj.TryGetValue<Boolean>('success', var Success) and Success then
            begin
              ShowMessage('Modèle réentraîné avec succès !');

              // Afficher les métriques mises à jour
              if ResultObj.TryGetValue<TJSONObject>('metrics', var Metrics) then
              begin
                var MetricsStr := 'Nouvelles métriques :' + sLineBreak;

                for var Metric in Metrics do
                  MetricsStr := MetricsStr + Metric.JsonString.Value + ' : ' +
                               Format('%.4f', [Metric.JsonValue.GetValue<Double>]) + sLineBreak;

                ShowMessage(MetricsStr);
              end;

              // Rafraîchir la liste des modèles
              ListModels;

              StatusBar1.SimpleText := 'Réentraînement terminé';
            end
            else
              ShowMessage('Erreur inconnue lors du réentraînement');
          finally
            ResultObj.Free;
          end;
        except
          ShowMessage('Erreur lors de l''analyse du résultat');
        end;
      end;
    finally
      Process.Free;
      Output.Free;
    end;
  end;
end;
```

Le script Python pour le réentraînement manuel :

```python
import sys
import os
import json
import joblib
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
from sklearn.linear_model import LogisticRegression, LinearRegression
from sklearn.metrics import accuracy_score, r2_score, mean_squared_error
import numpy as np

def retrain_model(model_name):
    try:
        # Vérifier si le modèle existe
        model_path = f"models/{model_name}.pkl"
        info_path = f"models/{model_name}_info.json"

        if not os.path.exists(model_path) or not os.path.exists(info_path):
            return json.dumps({"error": f"Le modèle '{model_name}' n'existe pas ou manque d'informations"})

        # Charger les informations du modèle
        with open(info_path, 'r') as f:
            model_info = json.load(f)

        # Déterminer le fichier de données à utiliser pour le réentraînement
        training_data_path = model_info.get("last_training_file", "data/training_data.csv")

        # Si le fichier spécifique n'existe pas, chercher un fichier récent
        if not os.path.exists(training_data_path):
            # Chercher des fichiers CSV dans le dossier data
            csv_files = [f for f in os.listdir("data") if f.endswith(".csv")]
            if not csv_files:
                return json.dumps({"error": "Aucun fichier de données trouvé pour le réentraînement"})

            # Utiliser le fichier le plus récent
            training_data_path = f"data/{sorted(csv_files)[-1]}"

        # Charger les données
        data = pd.read_csv(training_data_path)

        # Préparer les caractéristiques et la cible
        target_column = model_info.get("target", data.columns[-1])
        features = model_info.get("features", list(data.columns[:-1]))

        # S'assurer que toutes les colonnes existent
        for feature in features:
            if feature not in data.columns:
                return json.dumps({"error": f"Colonne '{feature}' non trouvée dans les données d'entraînement"})

        if target_column not in data.columns:
            return json.dumps({"error": f"Colonne cible '{target_column}' non trouvée dans les données d'entraînement"})

        X = data[features]
        y = data[target_column]

        # Diviser les données
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

        # Déterminer le type de modèle à créer
        model_type = model_info.get("type", "classification")

        if model_type == "classification":
            # Modèle de classification
            if "logistic" in model_name.lower():
                model = LogisticRegression(random_state=42)
            else:
                model = RandomForestClassifier(n_estimators=100, random_state=42)
        else:
            # Modèle de régression
            if "linear" in model_name.lower():
                model = LinearRegression()
            else:
                model = RandomForestRegressor(n_estimators=100, random_state=42)

        # Entraîner le modèle
        model.fit(X_train, y_train)

        # Évaluer le modèle
        y_pred = model.predict(X_test)

        # Calculer les métriques
        metrics = {}
        if model_type == "classification":
            metrics["accuracy"] = float(accuracy_score(y_test, y_pred))
        else:
            metrics["r2"] = float(r2_score(y_test, y_pred))
            metrics["mse"] = float(mean_squared_error(y_test, y_pred))
            metrics["rmse"] = float(np.sqrt(metrics["mse"]))

        # Sauvegarder le nouveau modèle (écraser l'ancien)
        joblib.dump(model, model_path)

        # Mettre à jour les informations du modèle
        model_info["last_retrained"] = pd.Timestamp.now().strftime("%Y-%m-%d %H:%M:%S")
        model_info["last_training_file"] = training_data_path
        model_info["metrics"] = metrics
        model_info["training_samples"] = len(X_train)
        model_info["testing_samples"] = len(X_test)

        with open(info_path, 'w') as f:
            json.dump(model_info, f)

        # Mettre à jour le fichier de suivi des performances
        perf_file = f"models/{model_name}_performance.csv"
        perf_data = []

        if os.path.exists(perf_file):
            perf_df = pd.read_csv(perf_file)
            perf_data = perf_df.to_dict('records')

        # Ajouter l'entrée actuelle
        perf_entry = {
            "date": pd.Timestamp.now().strftime("%Y-%m-%d"),
            "training_samples": len(X_train),
            "testing_samples": len(X_test)
        }
        perf_entry.update(metrics)

        perf_data.append(perf_entry)
        pd.DataFrame(perf_data).to_csv(perf_file, index=False)

        return json.dumps({
            "success": True,
            "message": f"Modèle '{model_name}' réentraîné avec succès",
            "metrics": metrics
        })
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print(retrain_model(sys.argv[1]))
    else:
        print(json.dumps({"error": "Usage: python manual_retrain.py <model_name>"}))
```

### 3. Gestion des versions de modèles

Une bonne pratique consiste à gérer les versions de vos modèles pour faciliter le retour à une version précédente en cas de problème :

```delphi
procedure TFormModelVersions.CreateModelSnapshot;
var
  Process: TProcess;
  Output: TStringList;
  SelectedModel: string;
  SnapshotName: string;
begin
  // Vérifier qu'un modèle est sélectionné
  if ListViewModels.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un modèle');
    Exit;
  end;

  SelectedModel := ListViewModels.Selected.Caption;

  // Demander un nom pour le snapshot
  SnapshotName := InputBox('Créer un snapshot',
                         'Nom du snapshot (laissez vide pour un nom automatique):',
                         '');

  // Si pas de nom, générer un nom automatique
  if SnapshotName = '' then
    SnapshotName := Format('%s_snapshot_%s',
                         [SelectedModel, FormatDateTime('yyyymmdd_hhnnss', Now)]);

  // Exécuter le script Python pour créer le snapshot
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('create_snapshot.py');
    Process.Parameters.Add(SelectedModel);
    Process.Parameters.Add(SnapshotName);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    StatusBar1.SimpleText := 'Création du snapshot en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    // Analyser la réponse
    if Output.Count > 0 then
    begin
      try
        var ResultObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
        try
          if ResultObj.TryGetValue<string>('error', var ErrorMsg) then
            ShowMessage('Erreur : ' + ErrorMsg)
          else if ResultObj.TryGetValue<Boolean>('success', var Success) and Success then
          begin
            ShowMessage('Snapshot créé avec succès : ' + SnapshotName);
            LoadSnapshots;  // Rafraîchir la liste des snapshots
            StatusBar1.SimpleText := 'Snapshot créé';
          end
          else
            ShowMessage('Erreur inconnue lors de la création du snapshot');
        finally
          ResultObj.Free;
        end;
      except
        ShowMessage('Erreur lors de l''analyse du résultat');
      end;
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormModelVersions.RestoreSnapshot;
var
  Process: TProcess;
  Output: TStringList;
  SelectedSnapshot: string;
begin
  // Vérifier qu'un snapshot est sélectionné
  if ListViewSnapshots.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un snapshot à restaurer');
    Exit;
  end;

  SelectedSnapshot := ListViewSnapshots.Selected.Caption;

  // Demander confirmation
  if MessageDlg('Voulez-vous restaurer le snapshot "' + SelectedSnapshot + '" ?'+sLineBreak+
              'Cette action remplacera le modèle actuel.',
              mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Exécuter le script Python pour restaurer le snapshot
    Process := TProcess.Create(nil);
    Output := TStringList.Create;

    try
      Process.Executable := 'python';
      Process.Parameters.Add('restore_snapshot.py');
      Process.Parameters.Add(SelectedSnapshot);
      Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

      StatusBar1.SimpleText := 'Restauration du snapshot en cours...';
      Process.Execute;

      Output.LoadFromStream(Process.Output);

      // Analyser la réponse
      if Output.Count > 0 then
      begin
        try
          var ResultObj := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
          try
            if ResultObj.TryGetValue<string>('error', var ErrorMsg) then
              ShowMessage('Erreur : ' + ErrorMsg)
            else if ResultObj.TryGetValue<Boolean>('success', var Success) and Success then
            begin
              ShowMessage('Snapshot restauré avec succès');
              StatusBar1.SimpleText := 'Snapshot restauré';

              // Récupérer le nom du modèle restauré
              var ModelName := ResultObj.GetValue<string>('model_name');
              LabelRestoredModel.Caption := 'Modèle restauré : ' + ModelName;
            end
            else
              ShowMessage('Erreur inconnue lors de la restauration du snapshot');
          finally
            ResultObj.Free;
          end;
        except
          ShowMessage('Erreur lors de l''analyse du résultat');
        end;
      end;
    finally
      Process.Free;
      Output.Free;
    end;
  end;
end;

procedure TFormModelVersions.LoadSnapshots;
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('list_snapshots.py');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    // Effacer la liste actuelle
    ListViewSnapshots.Items.Clear;

    // Analyser la réponse JSON
    if Output.Count > 0 then
    begin
      try
        var SnapshotsArray := TJSONObject.ParseJSONValue(Output.Text) as TJSONArray;
        try
          for var i := 0 to SnapshotsArray.Count - 1 do
          begin
            var SnapshotObj := SnapshotsArray.Items[i] as TJSONObject;

            var Item := ListViewSnapshots.Items.Add;
            Item.Caption := SnapshotObj.GetValue<string>('name');
            Item.SubItems.Add(SnapshotObj.GetValue<string>('model_name'));
            Item.SubItems.Add(SnapshotObj.GetValue<string>('date'));

            // Ajouter les métriques si disponibles
            if SnapshotObj.TryGetValue<TJSONObject>('metrics', var Metrics) then
            begin
              var MetricsStr := '';

              if SnapshotObj.GetValue<string>('type') = 'classification' then
                MetricsStr := Format('Accuracy: %.4f', [Metrics.GetValue<Double>('accuracy')])
              else
                MetricsStr := Format('R²: %.4f', [Metrics.GetValue<Double>('r2')]);

              Item.SubItems.Add(MetricsStr);
            end
            else
              Item.SubItems.Add('');
          end;
        finally
          SnapshotsArray.Free;
        end;
      except
        ShowMessage('Erreur lors de l''analyse de la liste des snapshots');
      end;
    end;

    // Mettre à jour le statut
    StatusBar1.SimpleText := Format('%d snapshots trouvés', [ListViewSnapshots.Items.Count]);
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Les scripts Python pour la gestion des versions :

```python
# create_snapshot.py
import sys
import os
import json
import shutil
import joblib
from datetime import datetime

def create_snapshot(model_name, snapshot_name):
    try:
        # Vérifier si le modèle existe
        model_path = f"models/{model_name}.pkl"
        info_path = f"models/{model_name}_info.json"

        if not os.path.exists(model_path):
            return json.dumps({"error": f"Le modèle '{model_name}' n'existe pas"})

        # Créer le dossier des snapshots s'il n'existe pas
        snapshot_dir = "snapshots"
        os.makedirs(snapshot_dir, exist_ok=True)

        # Charger les informations du modèle si disponibles
        model_info = {}
        if os.path.exists(info_path):
            with open(info_path, 'r') as f:
                model_info = json.load(f)

        # Créer un dictionnaire avec les informations sur le snapshot
        snapshot_info = {
            "name": snapshot_name,
            "model_name": model_name,
            "date": datetime.now().strftime("%Y-%m-%d %H:%M:%S"),
            "metrics": model_info.get("metrics", {})
        }

        # Ajouter d'autres informations utiles du modèle
        for key in ["type", "features", "target"]:
            if key in model_info:
                snapshot_info[key] = model_info[key]

        # Sauvegarder le modèle
        shutil.copy(model_path, f"{snapshot_dir}/{snapshot_name}.pkl")

        # Sauvegarder les informations sur le snapshot
        with open(f"{snapshot_dir}/{snapshot_name}_info.json", 'w') as f:
            json.dump(snapshot_info, f)

        return json.dumps({
            "success": True,
            "message": f"Snapshot '{snapshot_name}' créé avec succès",
            "model_name": model_name,
            "snapshot_name": snapshot_name
        })
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 2:
        print(create_snapshot(sys.argv[1], sys.argv[2]))
    else:
        print(json.dumps({"error": "Usage: python create_snapshot.py <model_name> <snapshot_name>"}))
```

```python
# restore_snapshot.py
import sys
import os
import json
import shutil
import pandas as pd

def restore_snapshot(snapshot_name):
    try:
        # Vérifier si le snapshot existe
        snapshot_dir = "snapshots"
        snapshot_path = f"{snapshot_dir}/{snapshot_name}.pkl"
        snapshot_info_path = f"{snapshot_dir}/{snapshot_name}_info.json"

        if not os.path.exists(snapshot_path) or not os.path.exists(snapshot_info_path):
            return json.dumps({"error": f"Le snapshot '{snapshot_name}' n'existe pas"})

        # Charger les informations du snapshot
        with open(snapshot_info_path, 'r') as f:
            snapshot_info = json.load(f)

        # Récupérer le nom du modèle d'origine
        model_name = snapshot_info.get("model_name")
        if not model_name:
            return json.dumps({"error": "Impossible de déterminer le modèle d'origine"})

        # Créer le dossier des modèles s'il n'existe pas
        os.makedirs("models", exist_ok=True)

        # Restaurer le modèle
        shutil.copy(snapshot_path, f"models/{model_name}.pkl")

        # Mettre à jour les informations du modèle
        model_info_path = f"models/{model_name}_info.json"

        # Créer ou mettre à jour les informations du modèle
        model_info = {
            "name": model_name,
            "restored_from": snapshot_name,
            "restored_date": pd.Timestamp.now().strftime("%Y-%m-%d %H:%M:%S")
        }

        # Copier les informations pertinentes du snapshot
        for key in ["type", "features", "target", "metrics"]:
            if key in snapshot_info:
                model_info[key] = snapshot_info[key]

        with open(model_info_path, 'w') as f:
            json.dump(model_info, f)

        return json.dumps({
            "success": True,
            "message": f"Snapshot '{snapshot_name}' restauré avec succès",
            "model_name": model_name,
            "snapshot_name": snapshot_name
        })
    except Exception as e:
        return json.dumps({"error": str(e)})

if __name__ == "__main__":
    if len(sys.argv) > 1:
        print(restore_snapshot(sys.argv[1]))
    else:
        print(json.dumps({"error": "Usage: python restore_snapshot.py <snapshot_name>"}))
```

```python
# list_snapshots.py
import os
import json
import glob

def list_snapshots():
    # Récupérer tous les fichiers .pkl dans le dossier snapshots
    snapshot_dir = "snapshots"
    if not os.path.exists(snapshot_dir):
        return json.dumps([])

    snapshot_files = glob.glob(f"{snapshot_dir}/*.pkl")

    # Extraire les informations des snapshots
    snapshots = []

    for snapshot_file in snapshot_files:
        # Extraire le nom de base du snapshot (sans le chemin ni l'extension)
        snapshot_name = os.path.splitext(os.path.basename(snapshot_file))[0]

        # Créer une entrée de base
        snapshot_entry = {
            "name": snapshot_name,
            "date": os.path.getctime(snapshot_file)
        }

        # Ajouter des informations détaillées si disponibles
        info_file = f"{snapshot_dir}/{snapshot_name}_info.json"
        if os.path.exists(info_file):
            with open(info_file, 'r') as f:
                snapshot_info = json.load(f)

                # Fusionner les informations
                snapshot_entry.update(snapshot_info)

        snapshots.append(snapshot_entry)

    # Trier par date de création (du plus récent au plus ancien)
    snapshots.sort(key=lambda x: x["date"], reverse=True)

    # Convertir les dates en chaînes pour la sérialisation JSON
    for snapshot in snapshots:
        if isinstance(snapshot["date"], (int, float)):
            import datetime
            snapshot["date"] = datetime.datetime.fromtimestamp(snapshot["date"]).strftime("%Y-%m-%d %H:%M:%S")

    return json.dumps(snapshots)

if __name__ == "__main__":
    print(list_snapshots())
```

### 4. Documentation des modèles déployés

Documenter vos modèles est essentiel pour maintenir la connaissance et faciliter le travail en équipe :

```delphi
procedure TFormModelDocumentation.GenerateDocumentation;
var
  SelectedModel: string;
  Process: TProcess;
  Output: TStringList;
  DocText: TStringList;
begin
  // Vérifier qu'un modèle est sélectionné
  if ListViewModels.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un modèle à documenter');
    Exit;
  end;

  SelectedModel := ListViewModels.Selected.Caption;

  // Récupérer les informations sur le modèle
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('service_model_info.py');
    Process.Parameters.Add(SelectedModel);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    Process.Execute;
    Output.LoadFromStream(Process.Output);

    if Output.Count = 0 then
    begin
      ShowMessage('Erreur lors de la récupération des informations du modèle');
      Exit;
    end;

    // Analyser la réponse JSON
    var ModelInfo := TJSONObject.ParseJSONValue(Output.Text) as TJSONObject;
    if ModelInfo = nil then
    begin
      ShowMessage('Erreur lors de l''analyse des informations du modèle');
      Exit;
    end;

    try
      // Générer la documentation
      DocText := TStringList.Create;
      try
        DocText.Add('# Documentation du modèle : ' + SelectedModel);
        DocText.Add('');
        DocText.Add('## Informations générales');
        DocText.Add('');
        DocText.Add('- **Type de modèle** : ' + ModelInfo.GetValue<string>('type', 'Non spécifié'));

        // Dates
        if ModelInfo.TryGetValue<string>('creation_date', var CreateDate) then
          DocText.Add('- **Date de création** : ' + CreateDate);

        if ModelInfo.TryGetValue<string>('last_retrained', var RetrainDate) then
          DocText.Add('- **Dernière mise à jour** : ' + RetrainDate);

        if ModelInfo.TryGetValue<string>('restored_from', var RestoredFrom) then
        begin
          DocText.Add('- **Restauré depuis** : ' + RestoredFrom);

          if ModelInfo.TryGetValue<string>('restored_date', var RestoredDate) then
            DocText.Add('- **Date de restauration** : ' + RestoredDate);
        end;

        DocText.Add('');
        DocText.Add('## Variables utilisées');
        DocText.Add('');

        // Caractéristiques
        if ModelInfo.TryGetValue<TJSONArray>('features', var Features) then
        begin
          DocText.Add('### Variables d''entrée');
          DocText.Add('');

          for var i := 0 to Features.Count - 1 do
            DocText.Add('- ' + Features.Items[i].Value);
        end;

        // Variable cible
        if ModelInfo.TryGetValue<string>('target', var Target) then
        begin
          DocText.Add('');
          DocText.Add('### Variable à prédire');
          DocText.Add('');
          DocText.Add('- ' + Target);
        end;

        DocText.Add('');
        DocText.Add('## Performances du modèle');
        DocText.Add('');

        // Métriques
        if ModelInfo.TryGetValue<TJSONObject>('metrics', var Metrics) then
        begin
          for var Metric in Metrics do
          begin
            var MetricName := Metric.JsonString.Value;
            var MetricValue := 0.0;

            if Metric.JsonValue is TJSONNumber then
              MetricValue := (Metric.JsonValue as TJSONNumber).AsDouble
            else
              continue;

            // Formater le nom de la métrique
            var FormattedName := '';
            case LowerCase(MetricName) of
              'accuracy': FormattedName := 'Précision (accuracy)';
              'precision': FormattedName := 'Précision (precision)';
              'recall': FormattedName := 'Rappel (recall)';
              'f1': FormattedName := 'Score F1';
              'r2': FormattedName := 'Coefficient de détermination (R²)';
              'mse': FormattedName := 'Erreur quadratique moyenne (MSE)';
              'rmse': FormattedName := 'Racine de l''erreur quadratique moyenne (RMSE)';
              'mae': FormattedName := 'Erreur absolue moyenne (MAE)';
            else
              FormattedName := MetricName;
            end;

            DocText.Add(Format('- **%s** : %.4f', [FormattedName, MetricValue]));
          end;
        end;

        DocText.Add('');
        DocText.Add('## Utilisation du modèle');
        DocText.Add('');
        DocText.Add('### Format des données d''entrée');
        DocText.Add('');

        // Format d'entrée
        if ModelInfo.TryGetValue<TJSONArray>('features', var InputFeatures) then
        begin
          DocText.Add('```');
          var Header := '';
          for var i := 0 to InputFeatures.Count - 1 do
          begin
            if i > 0 then Header := Header + ',';
            Header := Header + InputFeatures.Items[i].Value;
          end;
          DocText.Add(Header);
          DocText.Add('valeur1,valeur2,valeur3,...');
          DocText.Add('```');
        end;

        DocText.Add('');
        DocText.Add('### Exemple d''utilisation');
        DocText.Add('');
        DocText.Add('```delphi');
        DocText.Add('// Préparation des données');
        DocText.Add('var InputData := ''feature1,feature2,feature3'' + sLineBreak +');
        DocText.Add('                ''valeur1,valeur2,valeur3'';');
        DocText.Add('');
        DocText.Add('// Appel du service de prédiction');
        DocText.Add('var PredictionService := TPredictionClient.Create;');
        DocText.Add('try');
        DocText.Add('  var Result := PredictionService.Predict(''' + SelectedModel + ''', InputData);');
        DocText.Add('  // Traitement du résultat');
        DocText.Add('  var ResultObj := TJSONObject.ParseJSONValue(Result) as TJSONObject;');
        DocText.Add('  try');
        DocText.Add('    var Predictions := ResultObj.GetValue<TJSONArray>(''predictions'');');
        DocText.Add('    ShowMessage(''Prédiction : '' + Predictions.Items[0].ToString);');
        DocText.Add('  finally');
        DocText.Add('    ResultObj.Free;');
        DocText.Add('  end;');
        DocText.Add('finally');
        DocText.Add('  PredictionService.Free;');
        DocText.Add('end;');
        DocText.Add('```');

        DocText.Add('');
        DocText.Add('## Remarques et limitations');
        DocText.Add('');

        // Ajouter des remarques en fonction du type de modèle
        if ModelInfo.GetValue<string>('type', '') = 'classification' then
        begin
          DocText.Add('- Ce modèle est conçu pour la classification et retourne une catégorie parmi plusieurs possibles.');
          DocText.Add('- Les valeurs de sortie sont discrètes et non continues.');
        end
        else if ModelInfo.GetValue<string>('type', '') = 'regression' then
        begin
          DocText.Add('- Ce modèle est conçu pour la régression et prédit une valeur numérique continue.');
          DocText.Add('- Les prédictions représentent des estimations et peuvent avoir une marge d''erreur.');
        end;

        DocText.Add('- Le modèle a été entraîné sur un ensemble de données spécifique et peut ne pas bien généraliser à des cas très différents.');
        DocText.Add('- Pour obtenir les meilleures performances, les données d''entrée doivent être dans un format similaire à celui utilisé lors de l''entraînement.');

        // Ajouter les notes de l'utilisateur
        DocText.Add('');
        DocText.Add('## Notes additionnelles');
        DocText.Add('');
        DocText.Add(MemoNotes.Text);

        // Sauvegarder la documentation
        var DocFileName := 'documentation_' + SelectedModel + '.md';
        DocText.SaveToFile(DocFileName);

        ShowMessage('Documentation générée avec succès : ' + DocFileName);

        // Ouvrir le fichier dans le navigateur par défaut
        ShellExecute(0, 'open', PChar(DocFileName), nil, nil, SW_SHOW);
      finally
        DocText.Free;
      end;
    finally
      ModelInfo.Free;
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

## Exemples avancés de modèles prédictifs pour applications d'entreprise

### 1. Système de recommandation de produits

```delphi
procedure TFormRecommendation.GenerateRecommendations;
var
  ClientID: Integer;
  InputData: string;
  Process: TProcess;
  Output: TStringList;
begin
  // Récupérer l'ID client sélectionné
  ClientID := GetSelectedClientID;
  if ClientID = 0 then
  begin
    ShowMessage('Veuillez sélectionner un client');
    Exit;
  end;

  // Préparer les données d'entrée
  InputData := 'client_id,age,gender,total_purchases,last_purchase_days,average_value,preferred_category' + sLineBreak;

  // Récupérer les informations client
  CustomersQuery.Close;
  CustomersQuery.ParamByName('ClientID').AsInteger := ClientID;
  CustomersQuery.Open;

  if not CustomersQuery.IsEmpty then
  begin
    InputData := InputData +
                CustomersQuery.FieldByName('ClientID').AsString + ',' +
                CustomersQuery.FieldByName('Age').AsString + ',' +
                IntToStr(EncodeGender(CustomersQuery.FieldByName('Gender').AsString)) + ',' +
                CustomersQuery.FieldByName('TotalPurchases').AsString + ',' +
                CustomersQuery.FieldByName('LastPurchaseDays').AsString + ',' +
                CustomersQuery.FieldByName('AverageValue').AsString + ',' +
                IntToStr(EncodeCategoryPref(CustomersQuery.FieldByName('PreferredCategory').AsString));
  end
  else
  begin
    ShowMessage('Informations client introuvables');
    Exit;
  end;

  // Exécuter le script de recommandation
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    Process.Executable := 'python';
    Process.Parameters.Add('recommend_products.py');
    Process.Parameters.Add('temp_client_data.csv');
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Sauvegarder les données dans un fichier temporaire
    var TempFile := TStringList.Create;
    try
      TempFile.Text := InputData;
      TempFile.SaveToFile('temp_client_data.csv');
    finally
      TempFile.Free;
    end;

    StatusBar1.SimpleText := 'Génération des recommandations en cours...';
    Process.Execute;

    Output.LoadFromStream(Process.Output);

    // Traiter les résultats
    if Process.ExitCode = 0 then
    begin
      // Charger les recommandations dans la grille
      LoadRecommendations('recommendations.csv');

      // Afficher les détails
      if Output.Count > 0 then
      begin
        MemoDetails.Lines.Clear;
        MemoDetails.Lines.AddStrings(Output);
      end;

      StatusBar1.SimpleText := 'Recommandations générées';
    end
    else
      ShowMessage('Erreur lors de la génération des recommandations : ' + Output.Text);
  finally
    Process.Free;
    Output.Free;

    // Supprimer le fichier temporaire
    if FileExists('temp_client_data.csv') then
      DeleteFile('temp_client_data.csv');
  end;
end;

function TFormRecommendation.EncodeGender(const Gender: string): Integer;
begin
  if LowerCase(Gender) = 'male' then Result := 0
  else if LowerCase(Gender) = 'female' then Result := 1
  else Result := 2;  // Autre ou non spécifié
end;

function TFormRecommendation.EncodeCategoryPref(const Category: string): Integer;
begin
  // Encoder la catégorie préférée en nombre
  // Cette fonction dépend des catégories spécifiques de votre application
  case LowerCase(Category) of
    'electronics': Result := 0;
    'clothing': Result := 1;
    'home': Result := 2;
    'sports': Result := 3;
    'books': Result := 4;
  else
    Result := 5;  // Autre
  end;
end;

procedure TFormRecommendation.LoadRecommendations(const FileName: string);
var
  RecommendationFile: TStringList;
begin
  // Réinitialiser l'ensemble de données des recommandations
  RecommendationsDataset.EmptyDataSet;

  RecommendationFile := TStringList.Create;
  try
    RecommendationFile.LoadFromFile(FileName);

    // Vérifier l'en-tête
    if (RecommendationFile.Count > 0) and
       (RecommendationFile[0] = 'product_id,product_name,category,price,score,reason') then
    begin
      // Parcourir les résultats
      for var i := 1 to RecommendationFile.Count - 1 do
      begin
        var Parts := RecommendationFile[i].Split([',']);
        if Length(Parts) >= 6 then
        begin
          RecommendationsDataset.Append;
          RecommendationsDataset.FieldByName('ProductID').AsInteger := StrToIntDef(Parts[0], 0);
          RecommendationsDataset.FieldByName('ProductName').AsString := Parts[1];
          RecommendationsDataset.FieldByName('Category').AsString := Parts[2];
          RecommendationsDataset.FieldByName('Price').AsFloat := StrToFloatDef(Parts[3], 0);
          RecommendationsDataset.FieldByName('Score').AsFloat := StrToFloatDef(Parts[4], 0);
          RecommendationsDataset.FieldByName('Reason').AsString := Parts[5];
          RecommendationsDataset.Post;
        end;
      end;
    end;
  finally
    RecommendationFile.Free;
  end;
end;
```

Script Python pour les recommandations de produits (recommend_products.py) :

```python
import sys
import pandas as pd
import numpy as np
import joblib
from sklearn.metrics.pairwise import cosine_similarity

def recommend_products(client_data_file):
    try:
        # Charger les données du client
        client_data = pd.read_csv(client_data_file)

        # Vérifier qu'il y a au moins un client
        if len(client_data) == 0:
            print("Aucune donnée client fournie")
            return 1

        # Pour cet exemple, nous allons utiliser seulement le premier client
        client = client_data.iloc[0]

        # Charger le catalogue de produits
        products = pd.read_csv("data/products.csv")

        # Charger l'historique des achats (pour éviter de recommander des produits déjà achetés)
        purchase_history = pd.read_csv("data/purchase_history.csv")
        client_purchases = purchase_history[purchase_history['client_id'] == client['client_id']]['product_id'].unique()

        # Charger les vecteurs d'incorporation pré-calculés
        # Note: Ces vecteurs seraient normalement générés par un modèle d'apprentissage non supervisé
        product_vectors = pd.read_csv("data/product_vectors.csv")

        # Créer un vecteur pour le client basé sur ses caractéristiques
        client_vector = np.array([
            client['age'] / 100,  # Normaliser l'âge
            client['gender'],
            client['total_purchases'] / 100,  # Normaliser le nombre d'achats
            min(client['last_purchase_days'] / 365, 1),  # Normaliser en années, max 1
            min(client['average_value'] / 1000, 1),  # Normaliser en milliers, max 1
            client['preferred_category']
        ]).reshape(1, -1)

        # Calculer la similarité entre le client et tous les produits
        # Supposons que les 3 premiers éléments du vecteur produit correspondent aux mêmes caractéristiques
        similarities = []

        for _, product in product_vectors.iterrows():
            product_id = product['product_id']
            vector = product[['vector1', 'vector2', 'vector3', 'vector4', 'vector5']].values.reshape(1, -1)

            # Calculer la similarité cosinus
            sim = cosine_similarity(client_vector, vector)[0][0]

            # Ajouter un bonus si la catégorie correspond à la préférence du client
            product_info = products[products['product_id'] == product_id].iloc[0]
            if product_info['category_id'] == client['preferred_category']:
                sim += 0.2

            # Ajouter le produit et son score à la liste
            similarities.append({
                'product_id': product_id,
                'similarity': sim
            })

        # Convertir en DataFrame et trier par similarité
        sim_df = pd.DataFrame(similarities)
        sim_df = sim_df.sort_values('similarity', ascending=False)

        # Filtrer les produits déjà achetés
        sim_df = sim_df[~sim_df['product_id'].isin(client_purchases)]

        # Prendre les 10 meilleures recommandations
        top_recommendations = sim_df.head(10)

        # Joindre avec les informations produit
        recommendations = top_recommendations.merge(products, on='product_id')

        # Ajouter une raison pour chaque recommandation
        reasons = []

        for _, rec in recommendations.iterrows():
            if rec['category_id'] == client['preferred_category']:
                reasons.append("Basé sur votre catégorie préférée")
            elif rec['similarity'] > 0.8:
                reasons.append("Très apprécié par des clients similaires")
            elif rec['average_rating'] > 4.0:
                reasons.append("Produit très bien noté")
            else:
                reasons.append("Pourrait vous intéresser")

        recommendations['reason'] = reasons

        # Sauvegarder les recommandations dans un fichier
        result = recommendations[['product_id', 'name', 'category_name', 'price', 'similarity', 'reason']]
        result.columns = ['product_id', 'product_name', 'category', 'price', 'score', 'reason']
        result.to_csv('recommendations.csv', index=False)

        # Afficher un résumé
        print(f"Recommandations pour le client {client['client_id']}")
        print(f"Âge: {client['age']}, Genre: {'Homme' if client['gender'] == 0 else 'Femme'}")
        print(f"Achats totaux: {client['total_purchases']}, Valeur moyenne: {client['average_value']:.2f} €")
        print(f"Dernière commande: il y a {client['last_purchase_days']} jours")
        print("\nPrincipales recommandations:")

        for i, (_, rec) in enumerate(result.head(5).iterrows()):
            print(f"{i+1}. {rec['product_name']} ({rec['category']}) - {rec['price']:.2f} €")
            print(f"   Raison: {rec['reason']}")

        return 0
    except Exception as e:
        print(f"Erreur: {str(e)}")
        return 1

if __name__ == "__main__":
    if len(sys.argv) > 1:
        exit(recommend_products(sys.argv[1]))
    else:
        print("Usage: python recommend_products.py <client_data_file>")
        exit(1)
```

## Conclusion

L'intégration de modèles prédictifs dans vos applications Delphi ouvre la porte à une nouvelle génération de logiciels plus intelligents et plus utiles. Dans ce chapitre, nous avons exploré :

1. **Les fondamentaux des modèles prédictifs** - Comprendre ce que sont les modèles prédictifs et comment ils fonctionnent.

2. **Différentes approches d'intégration** - Des solutions intégrées simples aux services cloud sophistiqués en passant par les bibliothèques spécialisées via Python.

3. **Implémentation pratique** - Comment créer, entraîner et utiliser des modèles pour divers cas d'usage : prévision des ventes, détection d'anomalies, segmentation de clientèle, etc.

4. **Bonnes pratiques pour la production** - Comment maintenir, surveiller et documenter vos modèles en environnement de production.

Les modèles prédictifs peuvent apporter une valeur ajoutée significative à vos applications en permettant :
- D'anticiper les besoins des utilisateurs
- D'automatiser des tâches d'analyse complexes
- De prendre des décisions éclairées basées sur les données
- D'adapter dynamiquement votre application au comportement de l'utilisateur

Bien que l'apprentissage automatique puisse sembler intimidant au premier abord, l'approche par étapes présentée dans ce chapitre vous permet de commencer avec des solutions simples et d'évoluer progressivement vers des modèles plus sophistiqués.

Les compétences acquises ici vous permettront d'ajouter une dimension prédictive à pratiquement n'importe quelle application Delphi, qu'il s'agisse d'un logiciel de gestion, d'un système d'analyse ou d'une application métier spécialisée.

Dans les prochaines sections, nous explorerons comment combiner ces capacités prédictives avec d'autres technologies avancées comme l'Internet des Objets (IoT) et les services cloud pour créer des solutions encore plus puissantes et connectées.

---

> **Remarque** : Les exemples de code présentés dans ce chapitre sont conçus pour être didactiques et illustratifs. Dans un environnement de production réel, vous devriez prêter une attention particulière à la sécurité, aux performances et à la robustesse de votre implémentation.

⏭️ [Intégration avec des services d'IA cloud (Azure AI, Google AI, etc.)](22-intelligence-artificielle-et-machine-learning-avec-delphi/06-integration-avec-des-services-dia-cloud.md)
