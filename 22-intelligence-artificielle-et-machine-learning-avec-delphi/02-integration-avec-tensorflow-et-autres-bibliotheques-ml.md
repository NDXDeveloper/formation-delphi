# 22.2 Intégration avec TensorFlow et autres bibliothèques ML

## Introduction aux bibliothèques ML pour Delphi

Dans cette section, nous allons explorer comment intégrer des bibliothèques de Machine Learning (ML) populaires, en particulier TensorFlow, dans vos applications Delphi. L'objectif est de vous permettre d'exploiter la puissance de ces technologies tout en restant dans l'environnement de développement Delphi que vous connaissez.

## Qu'est-ce que TensorFlow ?

TensorFlow est l'une des bibliothèques de Machine Learning les plus populaires, développée initialement par Google. Elle permet de créer et d'entraîner des modèles de ML pour diverses applications comme la reconnaissance d'images, le traitement du langage naturel ou l'analyse prédictive.

## Options d'intégration de bibliothèques ML dans Delphi

Il existe plusieurs approches pour intégrer TensorFlow et d'autres bibliothèques ML dans vos applications Delphi :

### 1. Wrappers et librairies tierces

Plusieurs développeurs ont créé des wrappers Delphi qui facilitent l'utilisation de TensorFlow :

- **Delphi-TensorFlow** : Une interface pour TensorFlow en Delphi
- **DelphiDL** : Un wrapper pour les opérations de Deep Learning
- **FANN4Delphi** : Interface pour la bibliothèque Fast Artificial Neural Network

### 2. Utilisation via DLL ou bibliothèques partagées

Vous pouvez directement accéder aux fonctions des bibliothèques ML à travers leurs DLL :

```delphi
// Exemple de déclaration pour accéder à une fonction TensorFlow via sa DLL
function TF_Version: PAnsiChar; cdecl; external 'tensorflow.dll';

procedure TForm1.ButtonGetVersionClick(Sender: TObject);
var
  Version: string;
begin
  try
    // Appel de la fonction de la DLL TensorFlow
    Version := string(TF_Version);
    ShowMessage('Version TensorFlow : ' + Version);
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### 3. Communication via processus externe

Une autre approche consiste à exécuter du code Python (qui utilise TensorFlow) depuis votre application Delphi :

```delphi
procedure TFormML.RunPythonModel(InputData: string; var Output: string);
const
  PYTHON_SCRIPT = 'C:\ML\predict.py';
var
  Process: TProcess;
  OutputLines: TStringList;
begin
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add(PYTHON_SCRIPT);
    Process.Parameters.Add(InputData);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture de la sortie
    OutputLines.LoadFromStream(Process.Output);
    Output := OutputLines.Text;
  finally
    Process.Free;
    OutputLines.Free;
  end;
end;
```

### 4. Bibliothèques spécialisées pour Delphi

Des bibliothèques d'apprentissage automatique spécifiques à Delphi sont également disponibles :

- **NeuralNet for Delphi** : Implémentation native de réseaux de neurones
- **TensorFlow.pas** : Wrapper Object Pascal pour TensorFlow

## Intégration de TensorFlow avec Delphi : Approche pas à pas

Voyons comment intégrer TensorFlow dans une application Delphi :

### Étape 1 : Préparation de l'environnement

1. **Installez TensorFlow** sur votre système :
   - Vous pouvez télécharger la version C de TensorFlow (libtensorflow)
   - Ou installer Python avec TensorFlow pour l'approche par processus externe

2. **Configurez les chemins** :
   - Assurez-vous que les DLL sont accessibles (PATH système ou copie dans le dossier de votre exécutable)

### Étape 2 : Création d'une interface pour les fonctions TensorFlow

```delphi
unit uTensorFlow;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows;

// Types TensorFlow
type
  TTF_Status = Pointer;
  TTF_Buffer = Pointer;
  TTF_Graph = Pointer;
  TTF_OperationDescription = Pointer;
  TTF_Operation = Pointer;
  TTF_Session = Pointer;
  TTF_SessionOptions = Pointer;
  TTF_Tensor = Pointer;
  TTF_DataType = Integer;

const
  // Quelques constantes de types de données TensorFlow
  TF_FLOAT = 1;
  TF_DOUBLE = 2;
  TF_INT32 = 3;
  TF_INT64 = 9;

// Déclarations des fonctions TensorFlow
function TF_Version: PAnsiChar; cdecl; external 'tensorflow.dll';
function TF_NewStatus: TTF_Status; cdecl; external 'tensorflow.dll';
procedure TF_DeleteStatus(status: TTF_Status); cdecl; external 'tensorflow.dll';
function TF_GetCode(status: TTF_Status): Integer; cdecl; external 'tensorflow.dll';
function TF_NewGraph: TTF_Graph; cdecl; external 'tensorflow.dll';
procedure TF_DeleteGraph(graph: TTF_Graph); cdecl; external 'tensorflow.dll';
function TF_NewSessionOptions: TTF_SessionOptions; cdecl; external 'tensorflow.dll';
procedure TF_DeleteSessionOptions(opts: TTF_SessionOptions); cdecl; external 'tensorflow.dll';
function TF_NewSession(graph: TTF_Graph; opts: TTF_SessionOptions; status: TTF_Status): TTF_Session; cdecl; external 'tensorflow.dll';
procedure TF_CloseSession(session: TTF_Session; status: TTF_Status); cdecl; external 'tensorflow.dll';
procedure TF_DeleteSession(session: TTF_Session; status: TTF_Status); cdecl; external 'tensorflow.dll';

// Fonctions supplémentaires nécessaires à l'utilisation de modèles...

implementation

end.
```

### Étape 3 : Chargement et utilisation d'un modèle pré-entraîné

```delphi
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  uTensorFlow;

type
  TFormMain = class(TForm)
    ButtonLoadModel: TButton;
    ButtonPredict: TButton;
    EditInput: TEdit;
    LabelResult: TLabel;
    procedure ButtonLoadModelClick(Sender: TObject);
    procedure ButtonPredictClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FGraph: TTF_Graph;
    FSession: TTF_Session;
    FStatus: TTF_Status;
    FModelLoaded: Boolean;
    procedure LoadModel(const ModelPath: string);
    function Predict(const Input: Single): Single;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FStatus := TF_NewStatus;
  FModelLoaded := False;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if FModelLoaded then
  begin
    TF_DeleteSession(FSession, FStatus);
    TF_DeleteGraph(FGraph);
  end;
  TF_DeleteStatus(FStatus);
end;

procedure TFormMain.ButtonLoadModelClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Modèles TensorFlow (*.pb)|*.pb';
    if OpenDialog.Execute then
    begin
      LoadModel(OpenDialog.FileName);
      ButtonPredict.Enabled := FModelLoaded;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TFormMain.LoadModel(const ModelPath: string);
begin
  // Code de chargement du modèle TensorFlow...
  // Ceci est simplifié et devrait être complété avec le code réel
  // de lecture du fichier .pb et d'initialisation du graphe

  try
    FGraph := TF_NewGraph;

    // Lire le fichier du modèle et l'importer dans le graphe...

    // Créer une session
    FSession := TF_NewSession(FGraph, TF_NewSessionOptions, FStatus);

    if TF_GetCode(FStatus) = 0 then
    begin
      FModelLoaded := True;
      ShowMessage('Modèle chargé avec succès !');
    end
    else
    begin
      ShowMessage('Erreur lors du chargement du modèle.');
      if FSession <> nil then TF_DeleteSession(FSession, FStatus);
      if FGraph <> nil then TF_DeleteGraph(FGraph);
      FModelLoaded := False;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
      FModelLoaded := False;
    end;
  end;
end;

procedure TFormMain.ButtonPredictClick(Sender: TObject);
var
  Input, Result: Single;
begin
  if not FModelLoaded then
  begin
    ShowMessage('Veuillez d''abord charger un modèle.');
    Exit;
  end;

  if TryStrToFloat(EditInput.Text, Input) then
  begin
    Result := Predict(Input);
    LabelResult.Caption := 'Résultat : ' + FloatToStr(Result);
  end
  else
    ShowMessage('Veuillez entrer une valeur numérique valide.');
end;

function TFormMain.Predict(const Input: Single): Single;
begin
  // Code de prédiction avec TensorFlow...
  // Ceci est un exemple simplifié

  Result := 0; // Valeur par défaut

  // Créer un tenseur d'entrée avec la valeur Input
  // Exécuter l'inférence dans la session TensorFlow
  // Récupérer le résultat de la prédiction

  // Pour l'exemple, simulons une prédiction simple
  Result := Input * 2.5 + 10;
end;

end.
```

> 🔹 **Note**: L'exemple ci-dessus est un modèle simplifié qui montre la structure générale d'intégration avec TensorFlow. En pratique, l'implémentation complète nécessiterait davantage de code pour gérer les tenseurs et interagir avec le modèle.

## Solution alternative : TensorFlow via Python

Si l'intégration directe avec la DLL TensorFlow vous semble complexe, l'approche via Python peut être plus simple. Voici un exemple complet :

### 1. Création du script Python (predict.py)

```python
import sys
import tensorflow as tf
import numpy as np

# Fonction principale qui charge le modèle et fait une prédiction
def predict(input_value):
    # Charger le modèle préentraîné
    model = tf.keras.models.load_model('mon_modele.h5')

    # Préparer l'entrée
    input_data = np.array([[float(input_value)]])

    # Faire la prédiction
    result = model.predict(input_data)

    # Retourner le résultat
    print(result[0][0])  # Pour que Delphi puisse lire la sortie

# Point d'entrée
if __name__ == "__main__":
    if len(sys.argv) > 1:
        predict(sys.argv[1])
    else:
        print("Erreur: aucune valeur d'entrée fournie")
```

### 2. Utilisation depuis Delphi

```delphi
procedure TFormML.ButtonPredictClick(Sender: TObject);
var
  InputData: string;
  OutputData: string;
  Result: Double;
begin
  // Obtenir les données d'entrée
  InputData := EditInput.Text;

  // Appeler le script Python
  RunPythonModel(InputData, OutputData);

  // Traiter la sortie
  if TryStrToFloat(OutputData, Result) then
    LabelResult.Caption := 'Prédiction : ' + FormatFloat('0.00', Result)
  else
    ShowMessage('Erreur dans la prédiction : ' + OutputData);
end;

procedure TFormML.RunPythonModel(InputData: string; var Output: string);
var
  Process: TProcess;
  ExitCode: Integer;
  OutputLines: TStringList;
begin
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('predict.py');
    Process.Parameters.Add(InputData);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;
    ExitCode := Process.ExitCode;

    // Lecture de la sortie
    OutputLines.LoadFromStream(Process.Output);
    Output := Trim(OutputLines.Text);

    if (ExitCode <> 0) or (Output = '') then
      Output := 'Erreur d''exécution';
  finally
    Process.Free;
    OutputLines.Free;
  end;
end;
```

> ⚠️ **Attention** : Cette approche nécessite que Python et TensorFlow soient installés sur l'ordinateur où l'application sera exécutée.

## Autres bibliothèques ML pour Delphi

Outre TensorFlow, d'autres bibliothèques ML peuvent être intégrées à Delphi :

### 1. ONNX Runtime

ONNX (Open Neural Network Exchange) est un format ouvert pour représenter des modèles d'apprentissage automatique. ONNX Runtime est un moteur d'inférence pour exécuter des modèles ONNX.

```delphi
// Exemple simplifié d'utilisation d'ONNX Runtime
function TF_OnnxRuntimeVersion: PAnsiChar; cdecl; external 'onnxruntime.dll';

procedure TFormONNX.ButtonGetVersionClick(Sender: TObject);
var
  Version: string;
begin
  Version := string(TF_OnnxRuntimeVersion);
  ShowMessage('Version ONNX Runtime : ' + Version);
end;
```

### 2. LibTorch (PyTorch C++)

PyTorch est une autre bibliothèque ML populaire, et sa version C++ (LibTorch) peut être utilisée avec Delphi.

### 3. ML.NET

Pour les environnements Windows, ML.NET (bibliothèque Microsoft) peut être utilisée via COM ou des wrappers.

## Exemple pratique : Classification d'image avec TensorFlow

Voici un exemple plus complet utilisant un modèle TensorFlow pour la classification d'images :

```delphi
// Cette fonction utilise l'approche Python pour simplifier l'exemple
procedure TFormImageClassifier.ClassifyImage(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
  Classification: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;
  try
    // Appeler le script Python de classification
    Process.Executable := 'python';
    Process.Parameters.Add('classify_image.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécuter le script
    Process.Execute;

    // Récupérer les résultats
    Output.LoadFromStream(Process.Output);
    Classification := Output.Text;

    // Afficher les résultats
    MemoResults.Lines.Clear;
    MemoResults.Lines.Add('Résultats de classification :');
    MemoResults.Lines.Add('');
    MemoResults.Lines.AddStrings(Output);
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python correspondant (classify_image.py) pourrait être :

```python
import sys
import tensorflow as tf
from tensorflow.keras.applications.mobilenet_v2 import MobileNetV2, preprocess_input, decode_predictions
from tensorflow.keras.preprocessing import image
import numpy as np

def classify_image(image_path):
    # Charger le modèle pré-entraîné
    model = MobileNetV2(weights='imagenet')

    # Charger l'image
    img = image.load_img(image_path, target_size=(224, 224))
    img_array = image.img_to_array(img)
    img_array = np.expand_dims(img_array, axis=0)
    img_array = preprocess_input(img_array)

    # Prédiction
    predictions = model.predict(img_array)

    # Afficher les 5 meilleures prédictions
    results = decode_predictions(predictions, top=5)[0]

    for i, (imagenet_id, label, score) in enumerate(results):
        print(f"{i+1}. {label}: {score*100:.2f}%")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        image_path = sys.argv[1]
        classify_image(image_path)
    else:
        print("Erreur: chemin d'image non spécifié")
```

## Comment choisir la bonne approche ?

Voici quelques critères pour vous aider à choisir la meilleure méthode d'intégration :

1. **Niveau de complexité**
   - Pour les débutants : l'approche Python est généralement plus simple
   - Pour les projets avancés : l'intégration directe via DLL offre de meilleures performances

2. **Exigences de déploiement**
   - Si vous pouvez installer Python sur l'ordinateur cible : l'approche Python est viable
   - Si vous avez besoin d'une solution autonome : préférez l'intégration directe des DLL

3. **Performances**
   - Pour des performances optimales : utilisez les DLL natives
   - Pour le prototypage rapide : l'approche Python est plus flexible

4. **Facilité de maintenance**
   - Le code Python est généralement plus facile à maintenir pour les algorithmes ML
   - L'intégration directe offre un meilleur contrôle mais nécessite plus de code

## Conseils pour débuter avec TensorFlow dans Delphi

1. **Commencez petit** : Utilisez d'abord des modèles simples et pré-entraînés

2. **Infrastructure de test** : Créez une application de test dédiée pour expérimenter avec TensorFlow avant de l'intégrer à votre projet principal

3. **Isolation des dépendances** : Encapsulez l'accès à TensorFlow dans une unité ou une classe séparée

4. **Gestion des erreurs** : Implémentez une gestion robuste des erreurs, car les opérations ML peuvent échouer pour de nombreuses raisons

5. **Utilisation asynchrone** : Pour les modèles complexes, exécutez les opérations ML dans un thread séparé pour éviter de bloquer l'interface utilisateur

```delphi
// Exemple d'utilisation asynchrone
procedure TFormML.ButtonPredictClick(Sender: TObject);
begin
  ButtonPredict.Enabled := False;
  StatusBar1.SimpleText := 'Prédiction en cours...';

  // Lancer la prédiction dans un thread séparé
  TThread.CreateAnonymousThread(procedure
  var
    InputData, OutputData: string;
    ThreadResult: Double;
  begin
    InputData := EditInput.Text;
    RunPythonModel(InputData, OutputData);

    if TryStrToFloat(OutputData, ThreadResult) then
    begin
      // Mettre à jour l'interface depuis le thread principal
      TThread.Synchronize(nil, procedure
      begin
        LabelResult.Caption := 'Prédiction : ' + FormatFloat('0.00', ThreadResult);
        StatusBar1.SimpleText := 'Prédiction terminée';
        ButtonPredict.Enabled := True;
      end);
    end
    else
    begin
      TThread.Synchronize(nil, procedure
      begin
        ShowMessage('Erreur dans la prédiction : ' + OutputData);
        StatusBar1.SimpleText := 'Erreur';
        ButtonPredict.Enabled := True;
      end);
    end;
  end).Start;
end;
```

> 💡 **Astuce**: L'exécution des opérations ML dans un thread séparé est particulièrement importante pour les modèles complexes qui peuvent prendre plusieurs secondes à s'exécuter.

## Projets de démonstration

Pour vous aider à démarrer, voici quelques idées de projets simples :

1. **Classificateur d'images** : Application qui identifie le contenu d'une image
2. **Prédicteur de séries temporelles** : Prévision de valeurs futures basée sur des données historiques
3. **Analyseur de sentiments** : Déterminer si un texte exprime une opinion positive ou négative
4. **Détecteur d'objets** : Identifier et localiser des objets dans une image

## Conclusion

L'intégration de TensorFlow et d'autres bibliothèques ML dans vos applications Delphi ouvre un monde de possibilités pour créer des logiciels plus intelligents. Bien que l'apprentissage de ces technologies puisse sembler intimidant au début, les approches présentées dans cette section vous permettront de commencer progressivement et d'augmenter la complexité à mesure que vous vous familiarisez avec ces outils.

Dans la prochaine section, nous explorerons des applications spécifiques de traitement du langage naturel (NLP) avec Delphi, une autre branche passionnante de l'intelligence artificielle.

---

> **Remarque**: Les exemples de code présentés sont destinés à illustrer les concepts. Pour une implémentation complète et fonctionnelle, des ajustements spécifiques à votre environnement et à vos besoins peuvent être nécessaires.
