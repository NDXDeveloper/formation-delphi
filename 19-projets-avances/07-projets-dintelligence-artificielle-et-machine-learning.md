# 19.7 Projets d'intelligence artificielle et machine learning avec Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction à l'IA et au ML dans Delphi

L'intelligence artificielle (IA) et le machine learning (ML) sont des domaines en pleine expansion qui transforment de nombreux secteurs. Grâce à Delphi, vous pouvez intégrer ces technologies avancées dans vos applications sans avoir à maîtriser des langages comme Python ou R. Ce chapitre vous guidera dans la création de projets d'IA et de ML avec Delphi, en commençant par les concepts fondamentaux.

## Pourquoi utiliser l'IA et le ML dans vos projets Delphi?

L'intégration de l'intelligence artificielle et du machine learning dans vos applications Delphi peut apporter plusieurs avantages:

- **Automatisation des tâches répétitives** que les utilisateurs effectuent manuellement
- **Extraction d'informations précieuses** à partir de grandes quantités de données
- **Amélioration de l'expérience utilisateur** grâce à des fonctionnalités intelligentes
- **Prise de décision assistée** dans des domaines complexes
- **Création d'applications prédictives** qui anticipent les besoins des utilisateurs

## Approches pour intégrer l'IA et le ML dans Delphi

Il existe trois approches principales pour intégrer l'IA et le ML dans vos applications Delphi:

1. **Utilisation d'API d'IA cloud** (Azure AI, Google AI, OpenAI, etc.)
2. **Bibliothèques ML natives** pour Delphi
3. **Liaison avec des bibliothèques Python** ou d'autres langages spécialisés

## Projet 1: Intégration d'une API d'IA cloud

L'approche la plus simple pour les débutants consiste à utiliser des services d'IA cloud via des API REST. Commençons par un exemple simple d'intégration de l'API d'OpenAI pour créer une application de chat IA.

### Étape 1: Configuration du projet

1. Créez un nouveau projet d'application VCL dans Delphi
2. Ajoutez les composants nécessaires pour les appels API:
   - Ajoutez `RESTClient`, `RESTRequest`, `RESTResponse` depuis la palette d'outils (onglet REST Client)
   - Placez un `Memo` pour la saisie utilisateur, un `Button` pour envoyer et un `Memo` pour afficher les réponses

### Étape 2: Configuration de l'API REST

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  RESTClient1.BaseURL := 'https://api.openai.com/v1';
  RESTClient1.Accept := 'application/json';
  RESTClient1.ContentType := 'application/json';

  RESTRequest1.Method := rmPOST;
  RESTRequest1.Resource := '/chat/completions';
  RESTRequest1.Client := RESTClient1;
  RESTRequest1.Response := RESTResponse1;

  // Ajoutez votre clé API dans l'en-tête
  RESTRequest1.Params.AddHeader('Authorization', 'Bearer VOTRE_CLE_API');
end;
```

### Étape 3: Envoi d'une requête à l'API

```pascal
procedure TForm1.btnEnvoyerClick(Sender: TObject);
var
  RequestBody: TJSONObject;
  MessagesArray: TJSONArray;
  MessageObj: TJSONObject;
begin
  RequestBody := TJSONObject.Create;
  MessagesArray := TJSONArray.Create;
  MessageObj := TJSONObject.Create;

  try
    // Création du corps de la requête
    MessageObj.AddPair('role', 'user');
    MessageObj.AddPair('content', MemoQuestion.Text);
    MessagesArray.AddElement(MessageObj);

    RequestBody.AddPair('model', 'gpt-3.5-turbo');
    RequestBody.AddPair('messages', MessagesArray);
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(1000));

    // Ajout du corps à la requête
    RESTRequest1.Body.ClearBody;
    RESTRequest1.Body.Add(RequestBody.ToString, ContentTypeUTF8);

    // Envoi de la requête
    RESTRequest1.Execute;

    // Traitement de la réponse
    ProcessResponse;
  finally
    // Ne pas libérer MessageObj car il a été ajouté à MessagesArray
    RequestBody.Free; // Cela libère aussi MessagesArray et MessageObj
  end;
end;
```

### Étape 4: Traitement de la réponse

```pascal
procedure TForm1.ProcessResponse;
var
  ResponseObj, ChoiceObj, MessageObj: TJSONObject;
begin
  if RESTResponse1.StatusCode = 200 then
  begin
    try
      ResponseObj := TJSONObject.ParseJSONValue(RESTResponse1.Content) as TJSONObject;
      if Assigned(ResponseObj) then
      begin
        ChoiceObj := (ResponseObj.GetValue('choices') as TJSONArray).Items[0] as TJSONObject;
        MessageObj := ChoiceObj.GetValue('message') as TJSONObject;

        MemoResponse.Lines.Text := MessageObj.GetValue('content').Value;
      end;
    except
      on E: Exception do
        MemoResponse.Lines.Text := 'Erreur: ' + E.Message;
    end;
  end
  else
    MemoResponse.Lines.Text := 'Erreur: ' + RESTResponse1.StatusText;
end;
```

Ce projet simple permet d'interagir avec un modèle GPT via l'API OpenAI. Vous pouvez l'étendre pour créer un assistant IA personnalisé ou un outil de génération de contenu.

## Projet 2: Analyse de sentiment avec REST API

Voici un exemple légèrement plus avancé d'une application qui analyse le sentiment d'un texte en utilisant l'API Azure Text Analytics:

```pascal
procedure TForm1.btnAnalyzerClick(Sender: TObject);
var
  RequestBody: TJSONObject;
  DocumentsArray, DocObj: TJSONArray;
  Document: TJSONObject;
begin
  RESTClient1.BaseURL := 'https://VOTRE_NOM_RESSOURCE.cognitiveservices.azure.com';
  RESTClient1.Accept := 'application/json';
  RESTClient1.ContentType := 'application/json';

  RESTRequest1.Resource := '/text/analytics/v3.0/sentiment';
  RESTRequest1.Method := rmPOST;
  RESTRequest1.Params.AddHeader('Ocp-Apim-Subscription-Key', 'VOTRE_CLE');

  RequestBody := TJSONObject.Create;
  DocumentsArray := TJSONArray.Create;
  Document := TJSONObject.Create;

  try
    // Préparation du document à analyser
    Document.AddPair('id', '1');
    Document.AddPair('language', 'fr');
    Document.AddPair('text', MemoText.Text);

    DocumentsArray.AddElement(Document);
    RequestBody.AddPair('documents', DocumentsArray);

    // Envoi de la requête
    RESTRequest1.Body.ClearBody;
    RESTRequest1.Body.Add(RequestBody.ToString, ContentTypeUTF8);
    RESTRequest1.Execute;

    // Affichage du résultat avec une représentation visuelle
    DisplaySentimentResult;
  finally
    RequestBody.Free; // Cela libère aussi DocumentsArray et Document
  end;
end;

procedure TForm1.DisplaySentimentResult;
var
  ResponseObj: TJSONObject;
  DocumentsArray: TJSONArray;
  Document, SentimentObj: TJSONObject;
  Positive, Neutral, Negative: Double;
begin
  if RESTResponse1.StatusCode = 200 then
  begin
    try
      ResponseObj := TJSONObject.ParseJSONValue(RESTResponse1.Content) as TJSONObject;
      DocumentsArray := ResponseObj.GetValue('documents') as TJSONArray;
      Document := DocumentsArray.Items[0] as TJSONObject;
      SentimentObj := Document.GetValue('confidenceScores') as TJSONObject;

      // Récupération des scores
      Positive := (SentimentObj.GetValue('positive') as TJSONNumber).AsDouble;
      Neutral := (SentimentObj.GetValue('neutral') as TJSONNumber).AsDouble;
      Negative := (SentimentObj.GetValue('negative') as TJSONNumber).AsDouble;

      // Affichage des résultats
      ProgressBarPositive.Position := Round(Positive * 100);
      ProgressBarNeutral.Position := Round(Neutral * 100);
      ProgressBarNegative.Position := Round(Negative * 100);

      // Affichage du sentiment dominant
      if (Positive > Neutral) and (Positive > Negative) then
        LabelSentiment.Caption := 'Sentiment: Positif'
      else if (Negative > Neutral) and (Negative > Positive) then
        LabelSentiment.Caption := 'Sentiment: Négatif'
      else
        LabelSentiment.Caption := 'Sentiment: Neutre';

    finally
      ResponseObj.Free;
    end;
  end;
end;
```

## Projet 3: Utilisation de bibliothèques ML natives pour Delphi

Delphi dispose de quelques bibliothèques de machine learning natives. Un exemple est FANN (Fast Artificial Neural Network) qui peut être utilisé via des wrappers Delphi. Voici comment créer un projet de reconnaissance de chiffres manuscrits:

### Étape 1: Installation de la bibliothèque

1. Téléchargez et installez DelphiFANN depuis [GitHub](https://github.com/joaopauloschuler/neural-api)
2. Ajoutez le chemin de la bibliothèque dans Library Path de Delphi

### Étape 2: Création du projet de reconnaissance

```pascal
uses
  System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  neuralnetwork, neuralvolume;

type
  TForm1 = class(TForm)
    DrawPanel: TPanel;
    btnClear: TButton;
    btnRecognize: TButton;
    lblResult: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure DrawPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DrawPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnClearClick(Sender: TObject);
    procedure btnRecognizeClick(Sender: TObject);
  private
    FDrawing: Boolean;
    FBitmap: TBitmap;
    FNeuralNetwork: TNeuralNetwork;
    procedure LoadNeuralNetwork;
    function PreprocessImage: TNeuralVolume;
  public
    destructor Destroy; override;
  end;

{ ... }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Création du bitmap pour le dessin
  FBitmap := TBitmap.Create;
  FBitmap.SetSize(DrawPanel.Width, DrawPanel.Height);
  FBitmap.Canvas.Brush.Color := clWhite;
  FBitmap.Canvas.FillRect(Rect(0, 0, FBitmap.Width, FBitmap.Height));

  // Chargement du réseau neural pré-entraîné
  LoadNeuralNetwork;

  // Configuration du panel de dessin
  DrawPanel.OnPaint :=
    procedure(Sender: TObject)
    begin
      DrawPanel.Canvas.Draw(0, 0, FBitmap);
    end;
end;

procedure TForm1.LoadNeuralNetwork;
begin
  FNeuralNetwork := TNeuralNetwork.Create;
  // Chargez ici un réseau pré-entraîné ou créez-en un nouveau
  // Exemple simplifié:
  FNeuralNetwork.AddLayer(TNeuralLayer.Create(28*28, 100, 'Sigmoid'));
  FNeuralNetwork.AddLayer(TNeuralLayer.Create(100, 10, 'Softmax'));

  // Dans un cas réel, vous chargeriez des poids pré-entraînés
  if FileExists('digit_model.nn') then
    FNeuralNetwork.LoadFromFile('digit_model.nn');
end;

procedure TForm1.btnRecognizeClick(Sender: TObject);
var
  Input: TNeuralVolume;
  Output: TNeuralVolume;
  MaxProbability: Single;
  PredictedDigit: Integer;
  I: Integer;
begin
  // Prétraitement de l'image
  Input := PreprocessImage;
  try
    // Prédiction
    Output := FNeuralNetwork.Predict(Input);

    // Recherche du chiffre avec la plus haute probabilité
    MaxProbability := Output[0];
    PredictedDigit := 0;

    for I := 1 to 9 do
    begin
      if Output[I] > MaxProbability then
      begin
        MaxProbability := Output[I];
        PredictedDigit := I;
      end;
    end;

    lblResult.Caption := 'Chiffre reconnu: ' + IntToStr(PredictedDigit) +
                         ' (Confiance: ' + FormatFloat('0.00%', MaxProbability) + ')';
  finally
    Input.Free;
    Output.Free;
  end;
end;

function TForm1.PreprocessImage: TNeuralVolume;
var
  ResizedImg: TBitmap;
  X, Y: Integer;
  Brightness: Byte;
  GrayImg: array[0..27, 0..27] of Byte;
begin
  // Création d'une version réduite de 28x28 pixels
  ResizedImg := TBitmap.Create;
  try
    ResizedImg.PixelFormat := pf24bit;
    ResizedImg.SetSize(28, 28);

    // Redimensionnement de l'image
    ResizedImg.Canvas.StretchDraw(
      Rect(0, 0, 28, 28),
      FBitmap
    );

    // Conversion en niveaux de gris et normalisation
    for Y := 0 to 27 do
      for X := 0 to 27 do
      begin
        Brightness :=
          (GetRValue(ResizedImg.Canvas.Pixels[X, Y]) +
           GetGValue(ResizedImg.Canvas.Pixels[X, Y]) +
           GetBValue(ResizedImg.Canvas.Pixels[X, Y])) div 3;

        // Inversion (les chiffres MNIST ont fond noir et chiffres blancs)
        GrayImg[Y, X] := 255 - Brightness;
      end;

    // Création du volume d'entrée
    Result := TNeuralVolume.Create(28, 28, 1);

    // Remplissage et normalisation (0-1 au lieu de 0-255)
    for Y := 0 to 27 do
      for X := 0 to 27 do
        Result[X, Y, 0] := GrayImg[Y, X] / 255;

  finally
    ResizedImg.Free;
  end;
end;

// ... code de dessin ...
```

## Projet 4: Utilisation d'une API de Vision par Ordinateur

La vision par ordinateur est un domaine populaire de l'IA. Voici comment créer une application de détection d'objets avec l'API Azure Computer Vision:

```pascal
procedure TForm1.btnAnalyzeImageClick(Sender: TObject);
var
  MemStream: TMemoryStream;
  Base64: string;
  RequestBody: TJSONObject;
begin
  // Préparation de l'image
  MemStream := TMemoryStream.Create;
  try
    Image1.Picture.Bitmap.SaveToStream(MemStream);
    MemStream.Position := 0;

    // Configuration de l'API
    RESTClient1.BaseURL := 'https://VOTRE_RESSOURCE.cognitiveservices.azure.com';
    RESTRequest1.Resource := '/vision/v3.2/analyze';
    RESTRequest1.AddParameter('visualFeatures', 'Objects,Tags', pkQuery);
    RESTRequest1.AddParameter('language', 'fr', pkQuery);

    RESTRequest1.Params.AddHeader('Ocp-Apim-Subscription-Key', 'VOTRE_CLE');
    RESTRequest1.Method := rmPOST;

    // Envoi de l'image
    RESTRequest1.Body.ClearBody;
    RESTRequest1.AddBody(MemStream, 'image/jpeg');
    RESTRequest1.Execute;

    // Affichage des résultats
    DisplayVisionResults;
  finally
    MemStream.Free;
  end;
end;

procedure TForm1.DisplayVisionResults;
var
  ResponseObj: TJSONObject;
  ObjectsArray, TagsArray: TJSONArray;
  I: Integer;
  ObjectItem, TagItem: TJSONObject;
  ObjectName: string;
  Confidence: Double;
begin
  if RESTResponse1.StatusCode = 200 then
  begin
    ResponseObj := TJSONObject.ParseJSONValue(RESTResponse1.Content) as TJSONObject;
    try
      // Affichage des objets détectés
      MemoResults.Lines.Add('Objets détectés:');
      ObjectsArray := ResponseObj.GetValue('objects') as TJSONArray;

      for I := 0 to ObjectsArray.Count - 1 do
      begin
        ObjectItem := ObjectsArray.Items[I] as TJSONObject;
        ObjectName := ObjectItem.GetValue('object').Value;
        Confidence := (ObjectItem.GetValue('confidence') as TJSONNumber).AsDouble;

        MemoResults.Lines.Add(Format('- %s (confiance: %.1f%%)',
          [ObjectName, Confidence * 100]));

        // En option: dessiner des rectangles autour des objets
        DrawObjectRectangle(ObjectItem.GetValue('rectangle') as TJSONObject);
      end;

      // Affichage des tags
      MemoResults.Lines.Add('');
      MemoResults.Lines.Add('Tags identifiés:');
      TagsArray := ResponseObj.GetValue('tags') as TJSONArray;

      for I := 0 to Min(5, TagsArray.Count - 1) do
      begin
        TagItem := TagsArray.Items[I] as TJSONObject;
        MemoResults.Lines.Add(Format('- %s (%.1f%%)',
          [TagItem.GetValue('name').Value,
           (TagItem.GetValue('confidence') as TJSONNumber).AsDouble * 100]));
      end;
    finally
      ResponseObj.Free;
    end;
  end;
end;

procedure TForm1.DrawObjectRectangle(RectObj: TJSONObject);
var
  Rect: TRect;
  X, Y, W, H: Integer;
begin
  X := (RectObj.GetValue('x') as TJSONNumber).AsInt;
  Y := (RectObj.GetValue('y') as TJSONNumber).AsInt;
  W := (RectObj.GetValue('w') as TJSONNumber).AsInt;
  H := (RectObj.GetValue('h') as TJSONNumber).AsInt;

  // Ajustement à l'échelle de l'image affichée
  Rect.Left := Round(X * Image1.Width / Image1.Picture.Width);
  Rect.Top := Round(Y * Image1.Height / Image1.Picture.Height);
  Rect.Right := Rect.Left + Round(W * Image1.Width / Image1.Picture.Width);
  Rect.Bottom := Rect.Top + Round(H * Image1.Height / Image1.Picture.Height);

  // Dessin du rectangle
  Image1.Canvas.Pen.Color := clRed;
  Image1.Canvas.Pen.Width := 2;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(Rect);
end;
```

## Projet 5: Liaison avec Python pour des fonctionnalités ML avancées

Pour des fonctionnalités d'IA plus avancées, la liaison avec Python est une excellente option. Voici comment créer une interface entre Delphi et Python pour utiliser scikit-learn:

### Étape 1: Installation de Python4Delphi

1. Téléchargez et installez [Python4Delphi](https://github.com/pyscripter/python4delphi)
2. Installez Python et les bibliothèques nécessaires (numpy, scikit-learn)

### Étape 2: Création de l'interface Python-Delphi

```pascal
uses
  Winapi.Windows, System.SysUtils, Vcl.Forms,
  PythonEngine, PythonDLL, VarPyth;

type
  TForm1 = class(TForm)
    PythonEngine1: TPythonEngine;
    PythonModule1: TPythonModule;
    PythonGUIInputOutput1: TPythonGUIInputOutput;
    Button1: TButton;
    Memo1: TMemo;
    // ...
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure InitializePython;
    function TrainMLModel(const X, Y: Variant): Variant;
    function PredictWithModel(const Model, X: Variant): Variant;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializePython;
end;

procedure TForm1.InitializePython;
begin
  PythonEngine1.DllName := 'python39.dll'; // Ajustez selon votre version de Python
  PythonEngine1.LoadDll;

  // Import des bibliothèques Python nécessaires
  PythonEngine1.ExecString(
    'import numpy as np' + sLineBreak +
    'from sklearn.ensemble import RandomForestClassifier' + sLineBreak +
    'from sklearn.preprocessing import StandardScaler'
  );
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  X, Y, Model, NewX, Predictions: Variant;
begin
  try
    // Données d'exemple (caractéristiques et cibles)
    PythonEngine1.ExecString(
      'X_train = np.array([[1, 2, 3], [4, 5, 6], [7, 8, 9], [10, 11, 12]])' + sLineBreak +
      'y_train = np.array([0, 0, 1, 1])'
    );

    // Récupération des variables Python
    X := PythonEngine1.EvalString('X_train');
    Y := PythonEngine1.EvalString('y_train');

    // Entraînement du modèle
    Model := TrainMLModel(X, Y);

    // Données de test
    PythonEngine1.ExecString(
      'X_test = np.array([[5, 5, 5], [9, 8, 7]])'
    );
    NewX := PythonEngine1.EvalString('X_test');

    // Prédiction
    Predictions := PredictWithModel(Model, NewX);

    // Affichage des résultats
    Memo1.Lines.Add('Prédictions:');
    PythonEngine1.ExecString('print_result = "- " + str(predictions.tolist())');
    Memo1.Lines.Add(VarPythonAsString(PythonEngine1.EvalString('print_result')));

    // Probabilités (optionnel)
    PythonEngine1.ExecString('proba = model.predict_proba(X_test)');
    PythonEngine1.ExecString('print_proba = "Probabilités: " + str(proba.tolist())');
    Memo1.Lines.Add(VarPythonAsString(PythonEngine1.EvalString('print_proba')));
  except
    on E: Exception do
      Memo1.Lines.Add('Erreur: ' + E.Message);
  end;
end;

function TForm1.TrainMLModel(const X, Y: Variant): Variant;
begin
  // Création et entraînement du modèle
  PythonEngine1.ExecString(
    'model = RandomForestClassifier(n_estimators=100, random_state=42)' + sLineBreak +
    'model.fit(X_train, y_train)'
  );

  Result := PythonEngine1.EvalString('model');
end;

function TForm1.PredictWithModel(const Model, X: Variant): Variant;
begin
  // Utilisation de VarPyth pour passer les variables à Python
  PythonEngine1.SetVar('model', Model);
  PythonEngine1.SetVar('X_test', X);

  // Prédiction
  PythonEngine1.ExecString('predictions = model.predict(X_test)');

  Result := PythonEngine1.EvalString('predictions');
end;
```

## Utilisation de TensorFlow avec Delphi

Vous pouvez utiliser TensorFlow, l'une des bibliothèques d'apprentissage profond les plus populaires, via Python4Delphi:

```pascal
procedure TForm1.InitializeTensorFlow;
begin
  PythonEngine1.ExecString(
    'import tensorflow as tf' + sLineBreak +
    'import numpy as np' + sLineBreak +
    'from tensorflow.keras import layers, models'
  );
end;

procedure TForm1.CreateSimpleNeuralNetwork;
begin
  PythonEngine1.ExecString(
    'model = models.Sequential([' + sLineBreak +
    '    layers.Dense(64, activation="relu", input_shape=(10,)),' + sLineBreak +
    '    layers.Dense(32, activation="relu"),' + sLineBreak +
    '    layers.Dense(16, activation="relu"),' + sLineBreak +
    '    layers.Dense(1, activation="sigmoid")' + sLineBreak +
    '])' + sLineBreak +
    'model.compile(optimizer="adam", loss="binary_crossentropy", metrics=["accuracy"])'
  );

  Memo1.Lines.Add('Réseau de neurones créé avec succès');

  // Affichage de la structure
  PythonEngine1.ExecString('model.summary()');
end;
```

## Projet 6: Application de recommandation de produits

Voici un exemple d'application de recommandation de produits utilisant un algorithme de filtrage collaboratif simple:

```pascal
procedure TForm1.btnGenerateRecommendationsClick(Sender: TObject);
begin
  // Initialisation de Python et numpy
  InitializePython;

  // Chargement des données (notes des utilisateurs pour différents produits)
  LoadRatingsData;

  // Calcul des recommandations
  CalculateRecommendations(ComboBoxUsers.Text);
end;

procedure TForm1.LoadRatingsData;
var
  CsvFile: TStringList;
  Line, User, Product, Rating: string;
  I, P: Integer;
begin
  // Dans un cas réel, chargez depuis une base de données
  // Ici nous simulons avec du code Python
  PythonEngine1.ExecString(
    '# Matrice utilisateurs-produits (lignes=utilisateurs, colonnes=produits)' + sLineBreak +
    'ratings = np.array([' + sLineBreak +
    '    [5, 4, 0, 0, 1, 0, 0],' + sLineBreak +  // Utilisateur 1
    '    [0, 0, 5, 4, 0, 0, 0],' + sLineBreak +  // Utilisateur 2
    '    [0, 0, 0, 0, 0, 5, 4],' + sLineBreak +  // Utilisateur 3
    '    [0, 0, 0, 0, 4, 5, 0],' + sLineBreak +  // Utilisateur 4
    '    [5, 0, 0, 0, 0, 2, 0],' + sLineBreak +  // Utilisateur 5
    '])' + sLineBreak +
    '' + sLineBreak +
    '# Liste des produits' + sLineBreak +
    'products = ["Delphi 11", "Delphi 12", "C++ Builder", "RAD Studio", ' +
    '"GetIt Package Manager", "FireMonkey", "VCL"]'
  );

  // Remplissage du ComboBox des utilisateurs
  ComboBoxUsers.Clear;
  for I := 1 to 5 do
    ComboBoxUsers.Items.Add('Utilisateur ' + IntToStr(I));

  if ComboBoxUsers.Items.Count > 0 then
    ComboBoxUsers.ItemIndex := 0;
end;

procedure TForm1.CalculateRecommendations(const UserName: string);
var
  UserIndex: Integer;
begin
  // Extraction du numéro d'utilisateur
  UserIndex := StrToIntDef(UserName.Replace('Utilisateur ', ''), 1) - 1;

  // Code Python pour calculer les recommandations par filtrage collaboratif
  PythonEngine1.SetVar('user_idx', UserIndex);
  PythonEngine1.ExecString(
    '# Calcul de similarité entre utilisateurs (corrélation)' + sLineBreak +
    'from scipy.spatial.distance import cosine' + sLineBreak +
    '' + sLineBreak +
    'user_ratings = ratings[user_idx]' + sLineBreak +
    '' + sLineBreak +
    '# Trouver des utilisateurs similaires' + sLineBreak +
    'similarities = []' + sLineBreak +
    'for i in range(len(ratings)):' + sLineBreak +
    '    if i != user_idx:' + sLineBreak +
    '        # Ignorer les positions où les deux utilisateurs n''ont pas noté' + sLineBreak +
    '        mask = (ratings[i] > 0) & (user_ratings > 0)' + sLineBreak +
    '        if mask.sum() > 0:  # S''ils ont au moins un produit en commun' + sLineBreak +
    '            similarity = 1 - cosine(ratings[i], user_ratings)' + sLineBreak +
    '            similarities.append((i, similarity))' + sLineBreak +
    '        else:' + sLineBreak +
    '            similarities.append((i, 0))' + sLineBreak +
    '' + sLineBreak +
    '# Trier par similarité décroissante' + sLineBreak +
    'similarities.sort(key=lambda x: x[1], reverse=True)' + sLineBreak +
    '' + sLineBreak +
    '# Générer des recommandations' + sLineBreak +
    'recommendations = {}' + sLineBreak +
    'for i, sim in similarities:' + sLineBreak +
    '    if sim <= 0:  # Ignorer les utilisateurs non similaires' + sLineBreak +
    '        continue' + sLineBreak +
    '    ' + sLineBreak +
    '    # Parcourir les produits que l''utilisateur n''a pas noté' + sLineBreak +
    '    for j in range(len(user_ratings)):' + sLineBreak +
    '        if user_ratings[j] == 0 and ratings[i, j] > 0:' + sLineBreak +
    '            if j not in recommendations:' + sLineBreak +
    '                recommendations[j] = 0' + sLineBreak +
    '            # Pondérer la note par la similarité' + sLineBreak +
    '            recommendations[j] += ratings[i, j] * sim' + sLineBreak +
    '' + sLineBreak +
    '# Trier les recommandations' + sLineBreak +
    'sorted_recommendations = sorted(recommendations.items(), key=lambda x: x[1], reverse=True)' + sLineBreak +
    '' + sLineBreak +
    '# Noms des produits recommandés' + sLineBreak +
    'recommended_products = []' + sLineBreak +
    'recommended_scores = []' + sLineBreak +
    'for prod_idx, score in sorted_recommendations:' + sLineBreak +
    '    recommended_products.append(products[prod_idx])' + sLineBreak +
    '    recommended_scores.append(score)'
  );

  // Affichage des résultats
  DisplayRecommendations;
end;

procedure TForm1.DisplayRecommendations;
var
  Products: Variant;
  Scores: Variant;
  I: Integer;
begin
  // Récupération des résultats depuis Python
  Products := PythonEngine1.EvalString('recommended_products');
  Scores := PythonEngine1.EvalString('recommended_scores');

  ListViewRecommendations.Clear;

  // Si aucune recommandation
  if VarIsPythonList(Products) and (PythonEngine1.PyList_Size(Products) = 0) then
  begin
    Memo1.Lines.Add('Aucune recommandation disponible pour cet utilisateur.');
    Exit;
  end;

  // Affichage des recommandations
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Produits recommandés pour ' + ComboBoxUsers.Text + ':');

  for I := 0 to PythonEngine1.PyList_Size(Products) - 1 do
  begin
    Memo1.Lines.Add(Format('%d. %s (Score: %.2f)', [
      I + 1,
      VarPythonAsString(PythonEngine1.PyList_GetItem(Products, I)),
      VarPythonAsFloat(PythonEngine1.PyList_GetItem(Scores, I))
    ]));

    // Ajout dans la ListView avec barre de score
    with ListViewRecommendations.Items.Add do
    begin
      Caption := VarPythonAsString(PythonEngine1.PyList_GetItem(Products, I));
      // Création d'une sous-colonne pour le score visuel
      SubItems.Add(Format('%.2f', [VarPythonAsFloat(PythonEngine1.PyList_GetItem(Scores, I))]));
    end;
  end;

  // Explication simple de l'algorithme
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Comment fonctionne cette recommandation :');
  Memo1.Lines.Add('1. Nous identifions les utilisateurs qui ont des goûts similaires');
  Memo1.Lines.Add('2. Nous recommandons des produits qu''ils ont aimés mais que vous n''avez pas encore notés');
  Memo1.Lines.Add('3. Plus un utilisateur est similaire à vous, plus son opinion compte');
end;
```

## Projet 7: Détection d'anomalies dans les données

La détection d'anomalies est une application courante du machine learning dans les applications métier. Voici comment implémenter un système de détection d'anomalies pour surveiller les données financières ou de capteurs:

```pascal
procedure TForm1.DetectAnomalies;
begin
  // Initialisation de l'environnement Python
  PythonEngine1.ExecString(
    'import numpy as np' + sLineBreak +
    'from sklearn.ensemble import IsolationForest' + sLineBreak +
    'import matplotlib.pyplot as plt' + sLineBreak +
    'from io import BytesIO' + sLineBreak +
    'import base64'
  );

  // Génération ou chargement de données (simulées ici)
  PythonEngine1.ExecString(
    '# Données normales (distribution gaussienne)' + sLineBreak +
    'np.random.seed(42)' + sLineBreak +
    'normal_data = np.random.normal(0, 0.5, 1000).reshape(-1, 1)' + sLineBreak +
    '' + sLineBreak +
    '# Ajout d''anomalies' + sLineBreak +
    'anomalies = np.random.uniform(low=-4, high=4, size=50).reshape(-1, 1)' + sLineBreak +
    'all_data = np.vstack([normal_data, anomalies])' + sLineBreak +
    '' + sLineBreak +
    '# On mélange les données' + sLineBreak +
    'np.random.shuffle(all_data)'
  );

  // Entraînement du modèle Isolation Forest
  PythonEngine1.ExecString(
    '# Création et entraînement du modèle' + sLineBreak +
    'model = IsolationForest(contamination=0.05, random_state=42)' + sLineBreak +
    'model.fit(all_data)' + sLineBreak +
    '' + sLineBreak +
    '# Prédiction (-1 pour anomalie, 1 pour normal)' + sLineBreak +
    'predictions = model.predict(all_data)' + sLineBreak +
    'anomaly_indices = np.where(predictions == -1)[0]' + sLineBreak +
    '' + sLineBreak +
    '# Score d''anomalie (plus le score est négatif, plus c''est une anomalie)' + sLineBreak +
    'scores = model.decision_function(all_data)' + sLineBreak +
    'anomaly_scores = scores[anomaly_indices]'
  );

  // Affichage des résultats textuels
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Détection d''anomalies avec Isolation Forest:');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Nombre total de points: ' +
    VarPythonAsString(PythonEngine1.EvalString('len(all_data)')));
  Memo1.Lines.Add('Nombre d''anomalies détectées: ' +
    VarPythonAsString(PythonEngine1.EvalString('len(anomaly_indices)')));
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Top 5 anomalies les plus significatives:');

  // Affichage des 5 anomalies les plus significatives
  PythonEngine1.ExecString(
    '# Trier les anomalies par score' + sLineBreak +
    'sorted_indices = anomaly_indices[np.argsort(anomaly_scores)]' + sLineBreak +
    'top_anomalies = sorted_indices[:5]'  // Les 5 anomalies les plus significatives
  );

  PythonEngine1.ExecString(
    'for i, idx in enumerate(top_anomalies):' + sLineBreak +
    '    print(f"{i+1}. Point #{idx}: Valeur={all_data[idx][0]:.4f}, Score={scores[idx]:.4f}")'
  );

  // Génération d'un graphique pour visualiser les résultats
  CreateAnomalyGraph;
end;

procedure TForm1.CreateAnomalyGraph;
begin
  // Création du graphique avec matplotlib
  PythonEngine1.ExecString(
    '# Création d''un graphique pour visualiser les résultats' + sLineBreak +
    'plt.figure(figsize=(10, 6))' + sLineBreak +
    '' + sLineBreak +
    '# Points normaux en bleu' + sLineBreak +
    'normal_indices = np.where(predictions == 1)[0]' + sLineBreak +
    'plt.scatter(normal_indices, all_data[normal_indices], color="blue", alpha=0.5, label="Normal")' + sLineBreak +
    '' + sLineBreak +
    '# Anomalies en rouge' + sLineBreak +
    'plt.scatter(anomaly_indices, all_data[anomaly_indices], color="red", alpha=0.7, label="Anomalie")' + sLineBreak +
    '' + sLineBreak +
    '# Mise en évidence des 5 anomalies principales' + sLineBreak +
    'plt.scatter(top_anomalies, all_data[top_anomalies], color="darkred", s=100, edgecolors="black", label="Top anomalies")' + sLineBreak +
    '' + sLineBreak +
    '# Ajout des étiquettes pour les 5 anomalies principales' + sLineBreak +
    'for idx in top_anomalies:' + sLineBreak +
    '    plt.text(idx+5, all_data[idx][0], f"#{idx}: {all_data[idx][0]:.2f}", fontsize=9)' + sLineBreak +
    '' + sLineBreak +
    'plt.title("Détection d''anomalies avec Isolation Forest")' + sLineBreak +
    'plt.xlabel("Index")' + sLineBreak +
    'plt.ylabel("Valeur")' + sLineBreak +
    'plt.legend()' + sLineBreak +
    'plt.grid(True, alpha=0.3)' + sLineBreak +
    '' + sLineBreak +
    '# Sauvegarde en mémoire au format PNG' + sLineBreak +
    'buf = BytesIO()' + sLineBreak +
    'plt.savefig(buf, format="png", dpi=100)' + sLineBreak +
    'buf.seek(0)' + sLineBreak +
    'img_base64 = base64.b64encode(buf.read()).decode("utf-8")' + sLineBreak +
    'plt.close()'
  );

  // Récupération de l'image en base64
  var ImgBase64 := VarPythonAsString(PythonEngine1.EvalString('img_base64'));

  // Affichage du graphique dans une TImage (il faudrait d'abord implémenter une fonction Base64ToImage)
  Image1.Picture.Assign(Base64ToImage(ImgBase64));
end;

// Fonction utilitaire pour convertir une image Base64 en TImage
function TForm1.Base64ToImage(const Base64: string): TPicture;
var
  Stream: TMemoryStream;
  Decoder: TBase64Encoding;
  DecodedBytes: TBytes;
begin
  Result := TPicture.Create;
  Stream := TMemoryStream.Create;
  try
    Decoder := TBase64Encoding.Create;
    try
      DecodedBytes := Decoder.DecodeStringToBytes(Base64);
      Stream.WriteBuffer(DecodedBytes, Length(DecodedBytes));
      Stream.Position := 0;
      Result.LoadFromStream(Stream);
    finally
      Decoder.Free;
    end;
  finally
    Stream.Free;
  end;
end;
```

## Projet 8: Classification d'images avec un modèle pré-entraîné

La classification d'images est l'une des applications les plus courantes de l'IA. Voici comment utiliser un modèle pré-entraîné pour classifier des images dans Delphi:

```pascal
procedure TForm1.ClassifyImage(const ImagePath: string);
begin
  // Vérification que le fichier existe
  if not FileExists(ImagePath) then
  begin
    ShowMessage('Fichier image introuvable: ' + ImagePath);
    Exit;
  end;

  // Chargement de l'image dans le composant
  Image1.Picture.LoadFromFile(ImagePath);

  // Initialisation de l'environnement Python avec TensorFlow
  InitializeTensorFlow;

  // Chargement d'un modèle pré-entraîné (MobileNetV2)
  PythonEngine1.ExecString(
    '# Chargement du modèle MobileNetV2 pré-entraîné' + sLineBreak +
    'from tensorflow.keras.applications import MobileNetV2' + sLineBreak +
    'from tensorflow.keras.applications.mobilenet_v2 import preprocess_input, decode_predictions' + sLineBreak +
    'from tensorflow.keras.preprocessing import image' + sLineBreak +
    'import numpy as np' + sLineBreak +
    '' + sLineBreak +
    '# Chargement du modèle' + sLineBreak +
    'model = MobileNetV2(weights="imagenet")'
  );

  // Préparation et prétraitement de l'image
  PythonEngine1.SetVar('image_path', ImagePath);
  PythonEngine1.ExecString(
    '# Chargement et prétraitement de l''image' + sLineBreak +
    'img = image.load_img(image_path, target_size=(224, 224))' + sLineBreak +
    'img_array = image.img_to_array(img)' + sLineBreak +
    'img_array = np.expand_dims(img_array, axis=0)' + sLineBreak +
    'img_array = preprocess_input(img_array)'
  );

  // Prédiction avec le modèle
  PythonEngine1.ExecString(
    '# Prédiction' + sLineBreak +
    'predictions = model.predict(img_array)' + sLineBreak +
    'decoded = decode_predictions(predictions, top=5)[0]' + sLineBreak +
    '' + sLineBreak +
    '# Formatage des résultats' + sLineBreak +
    'formatted_predictions = []' + sLineBreak +
    'for i, (id, label, score) in enumerate(decoded):' + sLineBreak +
    '    formatted_predictions.append(f"{i+1}. {label} ({score*100:.2f}%)")'
  );

  // Affichage des résultats
  var Predictions := PythonEngine1.EvalString('formatted_predictions');

  ListViewResults.Clear;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Classification de l''image:');
  Memo1.Lines.Add('');

  for var I := 0 to PythonEngine1.PyList_Size(Predictions) - 1 do
  begin
    var Prediction := VarPythonAsString(PythonEngine1.PyList_GetItem(Predictions, I));
    Memo1.Lines.Add(Prediction);

    // Extraction du label et du score pour l'affichage visuel
    var Parts := Prediction.Split(['(', ')'], TStringSplitOptions.None);
    var Label := Parts[0].Substring(3).Trim;
    var Score := StrToFloatDef(Parts[1].Replace('%', ''), 0) / 100;

    with ListViewResults.Items.Add do
    begin
      Caption := Label;
      // Sous-élément pour la barre de progression
      SubItems.Add(FormatFloat('0.00%', Score));
      // Stockage du score pour l'affichage visuel
      Data := Pointer(Round(Score * 100));
    end;
  end;

  // Ajout d'une description pour l'utilisateur
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Cette classification utilise MobileNetV2, un modèle de réseau neuronal convolutif pré-entraîné sur la base de données ImageNet contenant plus d''un million d''images.');
end;

// Événement pour le bouton de sélection d'image
procedure TForm1.btnSelectImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    ClassifyImage(OpenPictureDialog1.FileName);
  end;
end;
```

## Création d'un chatbot simple avec traitement du langage naturel

Créons maintenant un chatbot simple en utilisant le traitement du langage naturel (NLP) pour interpréter les requêtes des utilisateurs:

```pascal
procedure TForm1.InitializeNLP;
begin
  PythonEngine1.ExecString(
    'import nltk' + sLineBreak +
    'import numpy as np' + sLineBreak +
    'from nltk.stem import WordNetLemmatizer' + sLineBreak +
    'from nltk.tokenize import word_tokenize' + sLineBreak +
    '' + sLineBreak +
    '# Téléchargement des ressources NLTK nécessaires' + sLineBreak +
    'nltk.download("punkt", quiet=True)' + sLineBreak +
    'nltk.download("wordnet", quiet=True)' + sLineBreak +
    '' + sLineBreak +
    '# Initialisation du lemmatiseur' + sLineBreak +
    'lemmatizer = WordNetLemmatizer()'
  );

  // Définition des intentions du chatbot
  PythonEngine1.ExecString(
    '# Définition des intents (intentions) du chatbot' + sLineBreak +
    'intents = {' + sLineBreak +
    '    "salutations": {' + sLineBreak +
    '        "patterns": ["bonjour", "salut", "hello", "hey", "coucou", "bonsoir"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Bonjour! Comment puis-je vous aider aujourd''hui?",' + sLineBreak +
    '            "Salut! Que puis-je faire pour vous?",' + sLineBreak +
    '            "Hello! Comment puis-je vous assister?"' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "aide_delphi": {' + sLineBreak +
    '        "patterns": ["help delphi", "aide delphi", "documentation delphi", "tutoriel", "comment utiliser delphi", "apprendre delphi"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "La documentation Delphi est disponible dans le menu Aide de l''IDE.",' + sLineBreak +
    '            "Vous pouvez trouver des tutoriels Delphi sur le site d''Embarcadero.",' + sLineBreak +
    '            "Je recommande de consulter la documentation ou les forums Delphi pour obtenir de l''aide spécifique."' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "compiler": {' + sLineBreak +
    '        "patterns": ["compiler", "build", "erreur compilation", "problème compilation", "comment compiler"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Pour compiler un projet Delphi, utilisez F9 ou le menu Projet > Compiler.",' + sLineBreak +
    '            "Si vous rencontrez des erreurs de compilation, vérifiez la fenêtre des messages pour plus de détails.",' + sLineBreak +
    '            "La compilation peut échouer si vous avez des erreurs de syntaxe ou des bibliothèques manquantes."' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "database": {' + sLineBreak +
    '        "patterns": ["base de données", "sql", "mysql", "connexion db", "database", "requête", "query"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Delphi offre FireDAC pour la connexion aux bases de données comme MySQL, SQLite, etc.",' + sLineBreak +
    '            "Pour accéder à une base de données, vous aurez besoin d''un composant TFDConnection et d''un pilote approprié.",' + sLineBreak +
    '            "Les requêtes SQL peuvent être exécutées via TFDQuery en Delphi."' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "au_revoir": {' + sLineBreak +
    '        "patterns": ["au revoir", "bye", "à bientôt", "ciao", "à plus", "adieu"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Au revoir! N''hésitez pas à revenir si vous avez d''autres questions.",' + sLineBreak +
    '            "À bientôt! Bonne programmation avec Delphi!",' + sLineBreak +
    '            "Au plaisir de vous aider à nouveau!"' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "merci": {' + sLineBreak +
    '        "patterns": ["merci", "thanks", "thank you", "super", "génial", "excellent"],' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Je vous en prie! Heureux d''avoir pu vous aider.",' + sLineBreak +
    '            "De rien! N''hésitez pas si vous avez d''autres questions.",' + sLineBreak +
    '            "Avec plaisir! Je suis là pour ça."' + sLineBreak +
    '        ]' + sLineBreak +
    '    },' + sLineBreak +
    '    "default": {' + sLineBreak +
    '        "responses": [' + sLineBreak +
    '            "Désolé, je ne comprends pas votre demande. Pourriez-vous reformuler?",' + sLineBreak +
    '            "Je ne suis pas sûr de comprendre. Pouvez-vous être plus précis?",' + sLineBreak +
    '            "Je n''ai pas saisi votre question. Essayez d''utiliser d''autres termes."' + sLineBreak +
    '        ]' + sLineBreak +
    '    }' + sLineBreak +
    '}'
  );

  // Fonction pour prétraiter le texte
  PythonEngine1.ExecString(
    'def preprocess_text(text):' + sLineBreak +
    '    # Conversion en minuscules' + sLineBreak +
    '    text = text.lower()' + sLineBreak +
    '    ' + sLineBreak +
    '    # Tokenisation (séparation en mots)' + sLineBreak +
    '    tokens = word_tokenize(text)' + sLineBreak +
    '    ' + sLineBreak +
    '    # Lemmatisation (réduction des mots à leur forme de base)' + sLineBreak +
    '    lemmas = [lemmatizer.lemmatize(token) for token in tokens]' + sLineBreak +
    '    ' + sLineBreak +
    '    return lemmas'
  );

  // Fonction pour prédire l'intention
  PythonEngine1.ExecString(
    'def predict_intent(text):' + sLineBreak +
    '    # Prétraitement du texte utilisateur' + sLineBreak +
    '    processed_input = preprocess_text(text)' + sLineBreak +
    '    ' + sLineBreak +
    '    best_intent = "default"' + sLineBreak +
    '    max_overlap = 0' + sLineBreak +
    '    ' + sLineBreak +
    '    # Parcourir les intentions définies' + sLineBreak +
    '    for intent, data in intents.items():' + sLineBreak +
    '        if intent == "default":' + sLineBreak +
    '            continue' + sLineBreak +
    '        ' + sLineBreak +
    '        # Compter le nombre de mots correspondants' + sLineBreak +
    '        for pattern in data["patterns"]:' + sLineBreak +
    '            pattern_tokens = preprocess_text(pattern)' + sLineBreak +
    '            overlap = len(set(processed_input).intersection(set(pattern_tokens)))' + sLineBreak +
    '            ' + sLineBreak +
    '            # Si meilleure correspondance, mettre à jour l''intention' + sLineBreak +
    '            if overlap > max_overlap:' + sLineBreak +
    '                max_overlap = overlap' + sLineBreak +
    '                best_intent = intent' + sLineBreak +
    '    ' + sLineBreak +
    '    # Si aucune correspondance significative' + sLineBreak +
    '    if max_overlap == 0:' + sLineBreak +
    '        best_intent = "default"' + sLineBreak +
    '    ' + sLineBreak +
    '    return best_intent'
  );

  // Fonction pour générer une réponse
  PythonEngine1.ExecString(
    'def generate_response(text):' + sLineBreak +
    '    # Prédire l''intention de l''utilisateur' + sLineBreak +
    '    intent = predict_intent(text)' + sLineBreak +
    '    ' + sLineBreak +
    '    # Sélectionner une réponse aléatoire pour cette intention' + sLineBreak +
    '    responses = intents[intent]["responses"]' + sLineBreak +
    '    response = np.random.choice(responses)' + sLineBreak +
    '    ' + sLineBreak +
    '    return response, intent'
  );

  Memo1.Lines.Add('Système de chatbot NLP initialisé avec succès!');
end;

// Traitement du message de l'utilisateur
procedure TForm1.btnSendMessageClick(Sender: TObject);
var
  UserMessage: string;
  BotResponse, Intent: Variant;
begin
  UserMessage := EditUserMessage.Text;

  if UserMessage.Trim.IsEmpty then
    Exit;

  // Ajout du message de l'utilisateur au chat
  MemoChat.Lines.Add('Vous: ' + UserMessage);

  // Obtention de la réponse du chatbot
  PythonEngine1.SetVar('user_input', UserMessage);
  PythonEngine1.ExecString('response, intent = generate_response(user_input)');

  BotResponse := PythonEngine1.EvalString('response');
  Intent := PythonEngine1.EvalString('intent');

  // Ajout d'un délai pour simuler la "réflexion" du chatbot
  Sleep(500);

  // Affichage de la réponse
  MemoChat.Lines.Add('Chatbot: ' + VarPythonAsString(BotResponse));
  MemoChat.Lines.Add('');

  // Mise à jour du log de débogage
  MemoChatDebug.Lines.Add('Message: "' + UserMessage + '"');
  MemoChatDebug.Lines.Add('→ Intent détectée: ' + VarPythonAsString(Intent));
  MemoChatDebug.Lines.Add('');

  // Effacement du champ de message
  EditUserMessage.Clear;
  EditUserMessage.SetFocus;

  // Défilement automatique vers le bas
  MemoChat.SelStart := Length(MemoChat.Text);
  SendMessage(MemoChat.Handle, EM_SCROLLCARET, 0, 0);
end;
```

## Projet 9: Prédiction de séries temporelles

La prédiction de séries temporelles est utile pour l'analyse de tendances, les prévisions financières ou la maintenance prédictive. Voici comment implémenter un modèle simple de prédiction de séries temporelles avec ARIMA (AutoRegressive Integrated Moving Average) :

```pascal
procedure TForm1.PredictTimeSeries;
begin
  // Configuration de l'environnement Python
  PythonEngine1.ExecString(
    'import numpy as np' + sLineBreak +
    'import pandas as pd' + sLineBreak +
    'from statsmodels.tsa.arima.model import ARIMA' + sLineBreak +
    'import matplotlib.pyplot as plt' + sLineBreak +
    'from io import BytesIO' + sLineBreak +
    'import base64' + sLineBreak +
    'import warnings' + sLineBreak +
    'warnings.filterwarnings("ignore")'  // Ignorer les avertissements pour simplifier
  );

  // Génération de données de série temporelle simulées ou chargement de vos propres données
  PythonEngine1.ExecString(
    '# Création d''une série temporelle synthétique pour démonstration' + sLineBreak +
    'np.random.seed(42)' + sLineBreak +
    'dates = pd.date_range(start="2023-01-01", periods=100, freq="D")' + sLineBreak +
    '' + sLineBreak +
    '# Tendance linéaire + composante saisonnière + bruit' + sLineBreak +
    'trend = np.linspace(10, 30, 100)  # Tendance croissante' + sLineBreak +
    'seasonality = 5 * np.sin(np.linspace(0, 12*np.pi, 100))  # Composante saisonnière' + sLineBreak +
    'noise = np.random.normal(0, 1, 100)  # Bruit aléatoire' + sLineBreak +
    '' + sLineBreak +
    '# Combinaison des composantes' + sLineBreak +
    'data = trend + seasonality + noise' + sLineBreak +
    '' + sLineBreak +
    '# Création d''une série pandas' + sLineBreak +
    'time_series = pd.Series(data, index=dates)' + sLineBreak +
    '' + sLineBreak +
    '# Séparation train/test (80% train, 20% test)' + sLineBreak +
    'train_size = int(len(time_series) * 0.8)' + sLineBreak +
    'train, test = time_series[:train_size], time_series[train_size:]'
  );

  // Modèle ARIMA pour la prédiction
  PythonEngine1.ExecString(
    '# Entraînement du modèle ARIMA' + sLineBreak +
    '# Paramètres (p,d,q): p=ordre autorégressif, d=différence, q=moyenne mobile' + sLineBreak +
    'model = ARIMA(train, order=(5,1,0))' + sLineBreak +
    'model_fit = model.fit()' + sLineBreak +
    '' + sLineBreak +
    '# Prédiction sur la période de test et au-delà' + sLineBreak +
    'forecast_steps = len(test) + 20  # 20 jours supplémentaires' + sLineBreak +
    'forecast = model_fit.forecast(steps=forecast_steps)' + sLineBreak +
    '' + sLineBreak +
    '# Dates pour la prédiction future' + sLineBreak +
    'future_dates = pd.date_range(start=time_series.index[-1] + pd.Timedelta("1 day"), ' +
    'periods=20, freq="D")' + sLineBreak +
    '' + sLineBreak +
    '# Calcul des erreurs sur la période de test' + sLineBreak +
    'test_forecast = forecast[:len(test)]' + sLineBreak +
    'mse = ((test - test_forecast) ** 2).mean()' + sLineBreak +
    'rmse = np.sqrt(mse)' + sLineBreak +
    'mape = np.abs((test - test_forecast) / test).mean() * 100'
  );

  // Création d'un graphique
  PythonEngine1.ExecString(
    '# Visualisation des résultats' + sLineBreak +
    'plt.figure(figsize=(12, 6))' + sLineBreak +
    '' + sLineBreak +
    '# Données d''entraînement' + sLineBreak +
    'plt.plot(train.index, train, label="Données d''entraînement", color="blue")' + sLineBreak +
    '' + sLineBreak +
    '# Données de test' + sLineBreak +
    'plt.plot(test.index, test, label="Données de test", color="green")' + sLineBreak +
    '' + sLineBreak +
    '# Prédictions sur période de test' + sLineBreak +
    'plt.plot(test.index, test_forecast, label="Prédictions (test)", color="red", linestyle="--")' + sLineBreak +
    '' + sLineBreak +
    '# Prédictions futures' + sLineBreak +
    'plt.plot(future_dates, forecast[len(test):], label="Prédictions futures", color="orange", linestyle="--")' + sLineBreak +
    '' + sLineBreak +
    '# Zone d''incertitude pour les prédictions futures (simplifiée)' + sLineBreak +
    'future_std = np.std(test - test_forecast)' + sLineBreak +
    'upper = forecast[len(test):] + 1.96 * future_std' + sLineBreak +
    'lower = forecast[len(test):] - 1.96 * future_std' + sLineBreak +
    'plt.fill_between(future_dates, lower, upper, color="orange", alpha=0.2)' + sLineBreak +
    '' + sLineBreak +
    '# Amélioration du graphique' + sLineBreak +
    'plt.title("Prédiction de série temporelle avec ARIMA")' + sLineBreak +
    'plt.xlabel("Date")' + sLineBreak +
    'plt.ylabel("Valeur")' + sLineBreak +
    'plt.legend()' + sLineBreak +
    'plt.grid(True, alpha=0.3)' + sLineBreak +
    '' + sLineBreak +
    '# Affichage des métriques dans le graphique' + sLineBreak +
    'plt.figtext(0.15, 0.15, f"RMSE: {rmse:.2f}\\nMAPE: {mape:.2f}%", ' +
    'bbox={"facecolor":"white", "alpha":0.8, "pad":5})' + sLineBreak +
    '' + sLineBreak +
    '# Sauvegarde en mémoire au format PNG' + sLineBreak +
    'buf = BytesIO()' + sLineBreak +
    'plt.savefig(buf, format="png", dpi=100)' + sLineBreak +
    'buf.seek(0)' + sLineBreak +
    'img_base64 = base64.b64encode(buf.read()).decode("utf-8")' + sLineBreak +
    'plt.close()'
  );

  // Affichage des résultats
  var
    RMSE := VarPythonAsFloat(PythonEngine1.EvalString('rmse'));
    MAPE := VarPythonAsFloat(PythonEngine1.EvalString('mape'));
    ImgBase64 := VarPythonAsString(PythonEngine1.EvalString('img_base64'));

  Memo1.Lines.Clear;
  Memo1.Lines.Add('Résultats de la prédiction de série temporelle avec ARIMA:');
  Memo1.Lines.Add('');
  Memo1.Lines.Add(Format('Erreur quadratique moyenne (RMSE): %.2f', [RMSE]));
  Memo1.Lines.Add(Format('Erreur absolue moyenne en pourcentage (MAPE): %.2f%%', [MAPE]));
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Le graphique vous montre:');
  Memo1.Lines.Add('- Données d''entraînement (bleu)');
  Memo1.Lines.Add('- Données de test (vert)');
  Memo1.Lines.Add('- Prédictions sur la période de test (rouge pointillé)');
  Memo1.Lines.Add('- Prédictions futures (orange pointillé)');
  Memo1.Lines.Add('- Zone d''incertitude pour les prédictions futures (orange clair)');

  // Affichage du graphique (même approche que précédemment)
  Image1.Picture.Assign(Base64ToImage(ImgBase64));
end;
```

## Projet 10: Regroupement de données (Clustering)

Le clustering est utile pour segmenter vos clients, regrouper des produits similaires, ou analyser des comportements. Voici comment implémenter un algorithme de clustering K-means :

```pascal
procedure TForm1.PerformClustering;
begin
  // Initialisation de l'environnement Python
  PythonEngine1.ExecString(
    'import numpy as np' + sLineBreak +
    'import pandas as pd' + sLineBreak +
    'from sklearn.cluster import KMeans' + sLineBreak +
    'from sklearn.preprocessing import StandardScaler' + sLineBreak +
    'import matplotlib.pyplot as plt' + sLineBreak +
    'from io import BytesIO' + sLineBreak +
    'import base64'
  );

  // Génération de données simulées (ou chargement de vos données réelles)
  PythonEngine1.ExecString(
    '# Génération de 3 groupes de données pour démonstration' + sLineBreak +
    'np.random.seed(42)' + sLineBreak +
    '' + sLineBreak +
    '# Groupe 1: clients jeunes à revenu faible/moyen' + sLineBreak +
    'n1 = 100' + sLineBreak +
    'age1 = np.random.normal(25, 5, n1)' + sLineBreak +
    'income1 = np.random.normal(35000, 10000, n1)' + sLineBreak +
    'purchase1 = np.random.normal(200, 50, n1)' + sLineBreak +
    '' + sLineBreak +
    '# Groupe 2: clients d''âge moyen à revenu moyen/élevé' + sLineBreak +
    'n2 = 80' + sLineBreak +
    'age2 = np.random.normal(40, 7, n2)' + sLineBreak +
    'income2 = np.random.normal(70000, 15000, n2)' + sLineBreak +
    'purchase2 = np.random.normal(500, 100, n2)' + sLineBreak +
    '' + sLineBreak +
    '# Groupe 3: clients plus âgés à revenu élevé' + sLineBreak +
    'n3 = 60' + sLineBreak +
    'age3 = np.random.normal(55, 6, n3)' + sLineBreak +
    'income3 = np.random.normal(100000, 20000, n3)' + sLineBreak +
    'purchase3 = np.random.normal(1000, 200, n3)' + sLineBreak +
    '' + sLineBreak +
    '# Combinaison des groupes' + sLineBreak +
    'age = np.concatenate([age1, age2, age3])' + sLineBreak +
    'income = np.concatenate([income1, income2, income3])' + sLineBreak +
    'purchase = np.concatenate([purchase1, purchase2, purchase3])' + sLineBreak +
    '' + sLineBreak +
    '# Création du dataframe' + sLineBreak +
    'data = pd.DataFrame({' + sLineBreak +
    '    "Age": age,' + sLineBreak +
    '    "Revenu": income,' + sLineBreak +
    '    "Achat": purchase' + sLineBreak +
    '})'
  );

  // Prétraitement des données
  PythonEngine1.ExecString(
    '# Normalisation des données' + sLineBreak +
    'scaler = StandardScaler()' + sLineBreak +
    'scaled_data = scaler.fit_transform(data)' + sLineBreak +
    '' + sLineBreak +
    '# Choix du nombre de clusters' + sLineBreak +
    'k = 3'
  );

  // Clustering avec K-means
  PythonEngine1.ExecString(
    '# Application de l''algorithme K-means' + sLineBreak +
    'kmeans = KMeans(n_clusters=k, random_state=42, n_init=10)' + sLineBreak +
    'kmeans.fit(scaled_data)' + sLineBreak +
    '' + sLineBreak +
    '# Ajout des clusters au dataframe' + sLineBreak +
    'data["Cluster"] = kmeans.labels_' + sLineBreak +
    '' + sLineBreak +
    '# Centres des clusters (dans l''espace normalisé)' + sLineBreak +
    'centers = kmeans.cluster_centers_' + sLineBreak +
    '' + sLineBreak +
    '# Conversion des centres dans l''espace d''origine' + sLineBreak +
    'centers_original = scaler.inverse_transform(centers)'
  );

  // Analyse des clusters
  PythonEngine1.ExecString(
    '# Résumé des clusters' + sLineBreak +
    'cluster_summary = data.groupby("Cluster").mean().round(2)' + sLineBreak +
    'cluster_counts = data["Cluster"].value_counts().sort_index()' + sLineBreak +
    '' + sLineBreak +
    '# Caractérisation des clusters pour l''affichage' + sLineBreak +
    'cluster_names = []' + sLineBreak +
    'for i in range(k):' + sLineBreak +
    '    age_val = cluster_summary.loc[i, "Age"]' + sLineBreak +
    '    income_val = cluster_summary.loc[i, "Revenu"]' + sLineBreak +
    '    purchase_val = cluster_summary.loc[i, "Achat"]' + sLineBreak +
    '    ' + sLineBreak +
    '    # Classification par âge' + sLineBreak +
    '    if age_val < 30:' + sLineBreak +
    '        age_segment = "Jeunes"' + sLineBreak +
    '    elif age_val < 50:' + sLineBreak +
    '        age_segment = "Âge moyen"' + sLineBreak +
    '    else:' + sLineBreak +
    '        age_segment = "Seniors"' + sLineBreak +
    '    ' + sLineBreak +
    '    # Classification par revenu' + sLineBreak +
    '    if income_val < 50000:' + sLineBreak +
    '        income_segment = "revenu faible"' + sLineBreak +
    '    elif income_val < 80000:' + sLineBreak +
    '        income_segment = "revenu moyen"' + sLineBreak +
    '    else:' + sLineBreak +
    '        income_segment = "revenu élevé"' + sLineBreak +
    '    ' + sLineBreak +
    '    # Classification par achat' + sLineBreak +
    '    if purchase_val < 300:' + sLineBreak +
    '        purchase_segment = "achats faibles"' + sLineBreak +
    '    elif purchase_val < 700:' + sLineBreak +
    '        purchase_segment = "achats moyens"' + sLineBreak +
    '    else:' + sLineBreak +
    '        purchase_segment = "achats élevés"' + sLineBreak +
    '    ' + sLineBreak +
    '    cluster_name = f"Segment {i+1}: {age_segment}, {income_segment}, {purchase_segment}"' + sLineBreak +
    '    cluster_names.append(cluster_name)'
  );

  // Visualisation des clusters
  PythonEngine1.ExecString(
    '# Création d''un graphique en nuage de points' + sLineBreak +
    'plt.figure(figsize=(12, 10))' + sLineBreak +
    '' + sLineBreak +
    '# Sous-graphique 1: Âge vs Revenu' + sLineBreak +
    'plt.subplot(2, 2, 1)' + sLineBreak +
    'for i in range(k):' + sLineBreak +
    '    plt.scatter(data[data["Cluster"] == i]["Age"], ' + sLineBreak +
    '                data[data["Cluster"] == i]["Revenu"], ' + sLineBreak +
    '                label=f"Segment {i+1}", alpha=0.7)' + sLineBreak +
    '    ' + sLineBreak +
    '    # Affichage des centres' + sLineBreak +
    '    plt.scatter(centers_original[i, 0], centers_original[i, 1], ' + sLineBreak +
    '                marker="X", s=100, c="black", label=f"Centre {i+1}" if i == 0 else "")' + sLineBreak +
    '' + sLineBreak +
    'plt.title("Âge vs Revenu")' + sLineBreak +
    'plt.xlabel("Âge")' + sLineBreak +
    'plt.ylabel("Revenu (€)")' + sLineBreak +
    'plt.legend()' + sLineBreak +
    'plt.grid(alpha=0.3)' + sLineBreak +
    '' + sLineBreak +
    '# Sous-graphique 2: Âge vs Achat' + sLineBreak +
    'plt.subplot(2, 2, 2)' + sLineBreak +
    'for i in range(k):' + sLineBreak +
    '    plt.scatter(data[data["Cluster"] == i]["Age"], ' + sLineBreak +
    '                data[data["Cluster"] == i]["Achat"], ' + sLineBreak +
    '                label=f"Segment {i+1}", alpha=0.7)' + sLineBreak +
    '    ' + sLineBreak +
    '    # Affichage des centres' + sLineBreak +
    '    plt.scatter(centers_original[i, 0], centers_original[i, 2], ' + sLineBreak +
    '                marker="X", s=100, c="black", label="_")' + sLineBreak +
    '' + sLineBreak +
    'plt.title("Âge vs Achat")' + sLineBreak +
    'plt.xlabel("Âge")' + sLineBreak +
    'plt.ylabel("Montant achat (€)")' + sLineBreak +
    'plt.grid(alpha=0.3)' + sLineBreak +
    '' + sLineBreak +
    '# Sous-graphique 3: Revenu vs Achat' + sLineBreak +
    'plt.subplot(2, 2, 3)' + sLineBreak +
    'for i in range(k):' + sLineBreak +
    '    plt.scatter(data[data["Cluster"] == i]["Revenu"], ' + sLineBreak +
    '                data[data["Cluster"] == i]["Achat"], ' + sLineBreak +
    '                label=f"Segment {i+1}", alpha=0.7)' + sLineBreak +
    '    ' + sLineBreak +
    '    # Affichage des centres' + sLineBreak +
    '    plt.scatter(centers_original[i, 1], centers_original[i, 2], ' + sLineBreak +
    '                marker="X", s=100, c="black", label="_")' + sLineBreak +
    '' + sLineBreak +
    'plt.title("Revenu vs Achat")' + sLineBreak +
    'plt.xlabel("Revenu (€)")' + sLineBreak +
    'plt.ylabel("Montant achat (€)")' + sLineBreak +
    'plt.grid(alpha=0.3)' + sLineBreak +
    '' + sLineBreak +
    '# Sous-graphique 4: Répartition des segments' + sLineBreak +
    'plt.subplot(2, 2, 4)' + sLineBreak +
    'plt.pie(cluster_counts, autopct="%1.1f%%", labels=[f"Segment {i+1}" for i in range(k)])' + sLineBreak +
    'plt.title("Répartition des segments de clients")' + sLineBreak +
    '' + sLineBreak +
    '# Ajustement de la mise en page' + sLineBreak +
    'plt.tight_layout(pad=3)' + sLineBreak +
    'plt.suptitle("Segmentation de la clientèle par K-means", fontsize=16, y=1.02)' + sLineBreak +
    '' + sLineBreak +
    '# Sauvegarde en mémoire au format PNG' + sLineBreak +
    'buf = BytesIO()' + sLineBreak +
    'plt.savefig(buf, format="png", dpi=100)' + sLineBreak +
    'buf.seek(0)' + sLineBreak +
    'img_base64 = base64.b64encode(buf.read()).decode("utf-8")' + sLineBreak +
    'plt.close()'
  );

  // Affichage des résultats
  var
    ClusterSummary := VarPythonAsString(PythonEngine1.EvalString('cluster_summary.to_string()'));
    ClusterNames := VarPythonToVariant(PythonEngine1.EvalString('cluster_names'));
    ImgBase64 := VarPythonAsString(PythonEngine1.EvalString('img_base64'));

  Memo1.Lines.Clear;
  Memo1.Lines.Add('Résultats du clustering K-means:');
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Segments identifiés:');

  for var I := 0 to VarArrayHighBound(ClusterNames, 1) do
    Memo1.Lines.Add('- ' + VarToStr(ClusterNames[I]));

  Memo1.Lines.Add('');
  Memo1.Lines.Add('Statistiques par segment:');
  Memo1.Lines.Add(ClusterSummary);

  // Affichage du graphique
  Image1.Picture.Assign(Base64ToImage(ImgBase64));
end;
```

## Bonnes pratiques pour les projets d'IA/ML avec Delphi

Voici quelques conseils pour réussir vos projets combinant Delphi et l'IA/ML:

### 1. Architecture en couches

Organisez votre code selon une architecture en couches pour séparer:
- La couche interface utilisateur (UI VCL/FMX)
- La couche métier (Business Logic)
- La couche de service d'IA/ML (encapsulant les appels Python ou API)
- La couche d'accès aux données (pour stocker/récupérer les données)

```pascal
// Exemple d'interface pour le service IA
IMLService = interface
  ['{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}']
  function ClassifyImage(const ImagePath: string): TClassificationResult;
  function PredictTimeSeries(const Data: TArray<Double>): TPredictionResult;
  // Autres méthodes...
end;

// Implémentation avec Python
TPythonMLService = class(TInterfacedObject, IMLService)
private
  FPythonEngine: TPythonEngine;
  procedure InitializePython;
public
  constructor Create;
  destructor Destroy; override;

  function ClassifyImage(const ImagePath: string): TClassificationResult;
  function PredictTimeSeries(const Data: TArray<Double>): TPredictionResult;
  // Autres méthodes...
end;

// Implémentation avec API Cloud
TCloudMLService = class(TInterfacedObject, IMLService)
private
  FRESTClient: TRESTClient;
  // ...
public
  // Mêmes méthodes que TPythonMLService, mais implémentées avec des API REST
end;
```

### 2. Gestion des dépendances

Pour les projets utilisant Python:
- Créez un script d'installation des dépendances Python
- Documentez clairement les bibliothèques requises
- Envisagez d'embarquer un environnement Python avec votre application

```pascal
procedure TMLServiceSetup.InstallPythonDependencies;
var
  PythonPath: string;
  RequirementsFile: TStringList;
begin
  // Création du fichier requirements.txt
  RequirementsFile := TStringList.Create;
  try
    RequirementsFile.Add('numpy==1.21.0');
    RequirementsFile.Add('pandas==1.3.0');
    RequirementsFile.Add('scikit-learn==1.0.1');
    RequirementsFile.Add('matplotlib==3.4.2');
    RequirementsFile.Add('tensorflow==2.6.0');
    RequirementsFile.Add('statsmodels==0.13.0');
    RequirementsFile.SaveToFile('requirements.txt');

    // Installation des packages
    PythonPath := GetPythonExecutablePath;
    ExecuteCommand(PythonPath + ' -m pip install -r requirements.txt');
  finally
    RequirementsFile.Free;
  end;
end;
```

### 3. Gestion asynchrone pour les opérations longues

Les opérations d'IA peuvent être longues; utilisez des threads ou des tâches asynchrones:

```pascal
procedure TForm1.btnClassifyImageClick(Sender: TObject);
begin
  // Désactivation des contrôles pendant le traitement
  btnClassifyImage.Enabled := False;
  ProgressBar1.Visible := True;

  // Exécution asynchrone
  TTask.Run(
    procedure
    var
      Results: TClassificationResult;
    begin
      try
        // Appel au service d'IA (potentiellement long)
        Results := FMLService.ClassifyImage(ImagePath);

        // Mise à jour de l'interface (thread principal)
        TThread.Synchronize(nil,
          procedure
          begin
            DisplayResults(Results);
            btnClassifyImage.Enabled := True;
            ProgressBar1.Visible := False;
          end
        );
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur: ' + E.Message);
              btnClassifyImage.Enabled := True;
              ProgressBar1.Visible := False;
            end
          );
      end;
    end
  );
end;
```

### 4. Gestion des erreurs

Les opérations d'IA/ML peuvent échouer pour de nombreuses raisons:
- Dépendances Python manquantes
- Problèmes de connexion réseau (API)
- Données invalides ou insuffisantes

```pascal
try
  // Tentative d'initialisation Python
  InitializePython;
except
  on E: EPythonError do
  begin
    Log('Erreur Python: ' + E.Message);
    if Pos('No module named', E.Message) > 0 then
    begin
      if MessageDlg('Bibliothèque Python manquante. Voulez-vous l''installer?',
                    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        InstallPythonDependencies;
        // Nouvelle tentative après installation
        try
          InitializePython;
        except
          on E2: Exception do
          begin
            Log('Échec de l''installation: ' + E2.Message);
            ShowMessage('Impossible d''initialiser Python. Veuillez consulter le journal pour plus de détails.');
          end;
        end;
      end;
    end
    else
      ShowMessage('Erreur d''initialisation Python: ' + E.Message);
  end;
  on E: Exception do
  begin
    Log('Erreur générale: ' + E.Message);
    ShowMessage('Une erreur s''est produite: ' + E.Message);
  end;
end;
```

### 5. Entraînement vs. inférence

Séparez clairement la phase d'entraînement et la phase d'inférence (prédiction):

- **Entraînement**: généralement effectué hors ligne, plus gourmand en ressources
- **Inférence**: utilisée dans l'application, optimisée pour la rapidité

```pascal
// Classe pour la gestion des modèles ML
TMLModelManager = class
private
  FModelPath: string;
  FPythonEngine: TPythonEngine;
public
  constructor Create(const AModelPath: string);
  destructor Destroy; override;

  // Phase d'entraînement (à lancer rarement, idéalement pendant la configuration)
  procedure TrainModel(const TrainingDataPath: string);

  // Phase d'inférence (utilisation quotidienne)
  function Predict(const InputData: TJSONObject): TJSONObject;

  // Gestion du modèle
  procedure SaveModel;
  procedure LoadModel;
  function GetLastTrainingDate: TDateTime;
end;
```

## Projet 11: Application de synthèse - Tableau de bord prédictif

Pour illustrer l'intégration complète, créons une application de tableau de bord prédictif qui combine:
- Interface utilisateur interactive
- Analyse de données historiques
- Modèles prédictifs
- Visualisation des résultats

Cet exemple montre comment organiser un projet plus complexe avec une architecture propre.

### Étape 1: Structure du projet

```pascal
// Unités du projet
unit PredictiveDashboard.Main;  // Formulaire principal
unit PredictiveDashboard.Data;  // Gestion des données
unit PredictiveDashboard.ML;    // Services d'IA/ML
unit PredictiveDashboard.Charts; // Visualisations
unit PredictiveDashboard.Types;  // Types et interfaces partagés
```

### Étape 2: Types et interfaces (PredictiveDashboard.Types)

```pascal
unit PredictiveDashboard.Types;

interface

uses
  System.Classes, System.JSON, System.SysUtils;

type
  // Types de prédiction supportés
  TPredictionType = (ptSales, ptInventory, ptCustomerChurn, ptMaintenance);

  // Résultat d'une prédiction
  TPredictionResult = record
    PredictionType: TPredictionType;
    Values: TArray<Double>;
    Dates: TArray<TDateTime>;
    Accuracy: Double;
    ModelName: string;
    LastUpdated: TDateTime;
  end;

  // Interface pour le service de prédiction
  IPredictionService = interface
    ['{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}']
    function Predict(APredictionType: TPredictionType;
                    const AHistoricalData: TJSONArray;
                    ADaysAhead: Integer): TPredictionResult;
    function GetModelAccuracy(APredictionType: TPredictionType): Double;
    procedure TrainModel(APredictionType: TPredictionType;
                        const ATrainingData: TJSONArray);
  end;

  // Événement de progression
  TProgressEvent = procedure(Sender: TObject; AProgress: Integer;
                           const AMessage: string) of object;

implementation

end.
```

### Étape 3: Service de prédiction (PredictiveDashboard.ML)

```pascal
unit PredictiveDashboard.ML;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.DateUtils,
  PythonEngine, PythonGUIInputOutput, VarPyth,
  PredictiveDashboard.Types;

type
  TPythonPredictionService = class(TInterfacedObject, IPredictionService)
  private
    FPythonEngine: TPythonEngine;
    FPythonGUIIO: TPythonGUIInputOutput;
    FModelsPath: string;
    FOnProgress: TProgressEvent;

    procedure InitializePython;
    procedure LoadModelForType(APredictionType: TPredictionType);
    function GetPythonScriptForModel(APredictionType: TPredictionType): string;
    function ConvertJSONToDataFrame(const AJSON: TJSONArray): Variant;
  public
    constructor Create(const AModelsPath: string);
    destructor Destroy; override;

    // Implémentation de IPredictionService
    function Predict(APredictionType: TPredictionType;
                    const AHistoricalData: TJSONArray;
                    ADaysAhead: Integer): TPredictionResult;
    function GetModelAccuracy(APredictionType: TPredictionType): Double;
    procedure TrainModel(APredictionType: TPredictionType;
                        const ATrainingData: TJSONArray);

    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

constructor TPythonPredictionService.Create(const AModelsPath: string);
begin
  inherited Create;
  FModelsPath := AModelsPath;

  FPythonEngine := TPythonEngine.Create(nil);
  FPythonGUIIO := TPythonGUIInputOutput.Create(nil);

  FPythonEngine.IO := FPythonGUIIO;
  FPythonEngine.DllName := 'python39.dll';  // Ajustez selon votre version

  InitializePython;
end;

destructor TPythonPredictionService.Destroy;
begin
  FPythonEngine.Free;
  FPythonGUIIO.Free;
  inherited;
end;

procedure TPythonPredictionService.InitializePython;
begin
  try
    FPythonEngine.LoadDll;

    // Importation des bibliothèques essentielles
    FPythonEngine.ExecString(
      'import numpy as np' + sLineBreak +
      'import pandas as pd' + sLineBreak +
      'from sklearn.preprocessing import StandardScaler' + sLineBreak +
      'import pickle' + sLineBreak +
      'import os' + sLineBreak +
      'from datetime import datetime, timedelta'
    );
  except
    on E: Exception do
      raise Exception.Create('Erreur d''initialisation Python: ' + E.Message);
  end;
end;

function TPythonPredictionService.Predict(APredictionType: TPredictionType;
                                      const AHistoricalData: TJSONArray;
                                      ADaysAhead: Integer): TPredictionResult;
var
  DataFrame: Variant;
  ModelPath: string;
  I: Integer;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, 0, 'Chargement des données...');

  // Conversion des données JSON en DataFrame Python
  DataFrame := ConvertJSONToDataFrame(AHistoricalData);

  if Assigned(FOnProgress) then
    FOnProgress(Self, 30, 'Chargement du modèle...');

  // Chargement du modèle approprié
  ModelPath := FModelsPath + '\model_' +
               GetEnumName(TypeInfo(TPredictionType), Ord(APredictionType)) + '.pkl';

  // Vérification si le modèle existe, sinon entraînez un modèle de base
  FPythonEngine.SetVar('model_path', ModelPath);
  FPythonEngine.ExecString(
    'if not os.path.exists(model_path):' + sLineBreak +
    '    raise ValueError("Modèle non trouvé. Veuillez entraîner le modèle d''abord.")'
  );

  if Assigned(FOnProgress) then
    FOnProgress(Self, 50, 'Préparation des données...');

  // Configuration des paramètres de prédiction
  FPythonEngine.SetVar('df', DataFrame);
  FPythonEngine.SetVar('days_ahead', ADaysAhead);
  FPythonEngine.SetVar('model_path', ModelPath);

  if Assigned(FOnProgress) then
    FOnProgress(Self, 70, 'Exécution de la prédiction...');

  // Exécution du script de prédiction spécifique au type
  FPythonEngine.ExecString(GetPythonScriptForModel(APredictionType));

  if Assigned(FOnProgress) then
    FOnProgress(Self, 90, 'Traitement des résultats...');

  // Récupération des résultats
  Result.PredictionType := APredictionType;
  Result.ModelName := GetEnumName(TypeInfo(TPredictionType), Ord(APredictionType));
  Result.Accuracy := VarPythonAsFloat(FPythonEngine.EvalString('accuracy'));
  Result.LastUpdated := Now;

  // Récupération des valeurs prédites
  var PredValues := FPythonEngine.EvalString('predictions.tolist()');
  SetLength(Result.Values, VarArrayHighBound(PredValues, 1) + 1);
  for I := 0 to VarArrayHighBound(PredValues, 1) do
    Result.Values[I] := VarPythonAsFloat(PredValues[I]);

  // Récupération des dates
  var PredDates := FPythonEngine.EvalString('prediction_dates');
  SetLength(Result.Dates, ADaysAhead);
  for I := 0 to ADaysAhead - 1 do
  begin
    var DateStr := VarPythonAsString(FPythonEngine.PyList_GetItem(PredDates, I));
    Result.Dates[I] := StrToDateTime(DateStr);
  end;

  if Assigned(FOnProgress) then
    FOnProgress(Self, 100, 'Prédiction terminée');
end;

function TPythonPredictionService.GetPythonScriptForModel(
  APredictionType: TPredictionType): string;
begin
  case APredictionType of
    ptSales:
      Result :=
        '# Chargement du modèle' + sLineBreak +
        'with open(model_path, "rb") as f:' + sLineBreak +
        '    model = pickle.load(f)' + sLineBreak +
        '' + sLineBreak +
        '# Prétraitement des données' + sLineBreak +
        'scaler = StandardScaler()' + sLineBreak +
        'df_numeric = df.select_dtypes(include=["number"])' + sLineBreak +
        'df_scaled = scaler.fit_transform(df_numeric)' + sLineBreak +
        '' + sLineBreak +
        '# Génération des caractéristiques' + sLineBreak +
        '# ... (code spécifique aux prévisions de vente)' + sLineBreak +
        '' + sLineBreak +
        '# Prédiction' + sLineBreak +
        'predictions = model.predict(df_scaled[-30:].mean(axis=0).reshape(1, -1))' + sLineBreak +
        'for i in range(1, days_ahead):' + sLineBreak +
        '    next_pred = model.predict(predictions[-1].reshape(1, -1))' + sLineBreak +
        '    predictions = np.append(predictions, next_pred)' + sLineBreak +
        '' + sLineBreak +
        '# Calcul des dates de prédiction' + sLineBreak +
        'last_date = datetime.strptime(df.iloc[-1]["Date"], "%Y-%m-%d")' + sLineBreak +
        'prediction_dates = []' + sLineBreak +
        'for i in range(days_ahead):' + sLineBreak +
        '    next_date = last_date + timedelta(days=i+1)' + sLineBreak +
        '    prediction_dates.append(next_date.strftime("%Y-%m-%d"))' + sLineBreak +
        '' + sLineBreak +
        '# Évaluation de la précision (sur les dernières données connues)' + sLineBreak +
        'from sklearn.metrics import mean_absolute_percentage_error' + sLineBreak +
        'y_true = df_numeric["Sales"].values[-10:]' + sLineBreak +
        'y_pred = model.predict(df_scaled[-10:])' + sLineBreak +
        'accuracy = 1 - mean_absolute_percentage_error(y_true, y_pred)';

    ptInventory:
      Result :=
        '# Chargement du modèle' + sLineBreak +
        'with open(model_path, "rb") as f:' + sLineBreak +
        '    model = pickle.load(f)' + sLineBreak +
        '' + sLineBreak +
        '# Code spécifique aux prédictions d''inventaire' + sLineBreak +
        '# ...';

    // Ajoutez d'autres cas pour les différents types de prédiction

    else
      raise Exception.Create('Type de prédiction non pris en charge');
  end;
end;

// Autres méthodes de la classe...

end.
```

### Étape 4: Gestionnaire de données (PredictiveDashboard.Data)

```pascal
unit PredictiveDashboard.Data;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.DateUtils,
  FireDAC.Comp.Client, FireDAC.Stan.Param,
  PredictiveDashboard.Types;

type
  TDataManager = class
  private
    FConnection: TFDConnection;
    function QueryToJSON(const ASQL: string): TJSONArray;
  public
    constructor Create(const AConnectionString: string);
    destructor Destroy; override;

    // Données historiques
    function GetSalesData(AStartDate, AEndDate: TDateTime): TJSONArray;
    function GetInventoryData(AProductID: Integer;
                            AStartDate, AEndDate: TDateTime): TJSONArray;
    function GetCustomerData: TJSONArray;

    // Données pour l'apprentissage
    function GetTrainingData(APredictionType: TPredictionType): TJSONArray;

    // Sauvegarde des prédictions
    procedure SavePrediction(const APrediction: TPredictionResult);
    function GetLastPrediction(APredictionType: TPredictionType): TPredictionResult;
  end;

implementation

// Implémentation des méthodes...

end.
```

### Étape 5: Visualisation des données (PredictiveDashboard.Charts)

```pascal
unit PredictiveDashboard.Charts;

interface

uses
  System.Classes, System.SysUtils, Vcl.ExtCtrls, Vcl.Graphics,
  VCLTee.Chart, VCLTee.Series, VCLTee.TeEngine,
  PredictiveDashboard.Types;

type
  TChartManager = class
  private
    FChart: TChart;
  public
    constructor Create(AChart: TChart);

    // Création des graphiques
    procedure CreateTimeSeriesChart(const APrediction: TPredictionResult;
                                 const AHistoricalValues: TArray<Double>;
                                 const AHistoricalDates: TArray<TDateTime>);
    procedure CreateComparisonChart(const AActualValues, APredictedValues: TArray<Double>;
                                 const ADates: TArray<TDateTime>);
    procedure CreateAccuracyChart(const APredictionTypes: TArray<TPredictionType>;
                               const AAccuracies: TArray<Double>);
  end;

implementation

constructor TChartManager.Create(AChart: TChart);
begin
  inherited Create;
  FChart := AChart;
end;

procedure TChartManager.CreateTimeSeriesChart(const APrediction: TPredictionResult;
                                         const AHistoricalValues: TArray<Double>;
                                         const AHistoricalDates: TArray<TDateTime>);
var
  HistorySeries, PredictionSeries: TLineSeries;
  I: Integer;
begin
  // Configuration du graphique
  FChart.ClearChart;
  FChart.Title.Text.Clear;
  FChart.Title.Text.Add('Prévision ' +
    GetEnumName(TypeInfo(TPredictionType), Ord(APrediction.PredictionType)));
  FChart.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';
  FChart.BottomAxis.ExactDateTime := True;

  // Série pour les données historiques
  HistorySeries := TLineSeries.Create(FChart);
  HistorySeries.Title := 'Données historiques';
  HistorySeries.LinePen.Width := 2;
  HistorySeries.LinePen.Color := clBlue;
  HistorySeries.XValues.DateTime := True;

  for I := 0 to High(AHistoricalValues) do
    HistorySeries.AddXY(AHistoricalDates[I], AHistoricalValues[I]);

  FChart.AddSeries(HistorySeries);

  // Série pour les prédictions
  PredictionSeries := TLineSeries.Create(FChart);
  PredictionSeries.Title := 'Prédictions';
  PredictionSeries.LinePen.Width := 2;
  PredictionSeries.LinePen.Color := clRed;
  PredictionSeries.LinePen.Style := psDot;
  PredictionSeries.XValues.DateTime := True;

  // Premier point: dernier point des données historiques pour assurer la continuité
  if Length(AHistoricalValues) > 0 then
    PredictionSeries.AddXY(AHistoricalDates[High(AHistoricalValues)],
                          AHistoricalValues[High(AHistoricalValues)]);

  // Ajout des prédictions
  for I := 0 to High(APrediction.Values) do
    PredictionSeries.AddXY(APrediction.Dates[I], APrediction.Values[I]);

  FChart.AddSeries(PredictionSeries);

  // Ajout d'une annotation pour la précision
  var Annotation := TAnnotationTool.Create(FChart);
  Annotation.Text := Format('Précision du modèle: %.2f%%', [APrediction.Accuracy * 100]);
  Annotation.Shape.Font.Size := 9;
  Annotation.Shape.Font.Style := [fsBold];
  Annotation.Shape.Color := clInfoBk;
  Annotation.Position := Annotation.Position.Create(10, 10);

  FChart.Tools.Add(Annotation);
end;

// Autres méthodes de visualisation...

end.
```

### Étape 6: Interface principale (PredictiveDashboard.Main)

```pascal
unit PredictiveDashboard.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, VCLTee.Chart, VCLTee.Series,
  PredictiveDashboard.Types, PredictiveDashboard.Data, PredictiveDashboard.ML,
  PredictiveDashboard.Charts;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    tabSalesForecast: TTabSheet;
    tabInventory: TTabSheet;
    tabCustomers: TTabSheet;
    pnlSettings: TPanel;
    pnlCharts: TPanel;
    Chart1: TChart;
    btnPredict: TButton;
    btnTrain: TButton;
    cmbPredictionType: TComboBox;
    dtpStartDate: TDateTimePicker;
    dtpEndDate: TDateTimePicker;
    edtDaysAhead: TEdit;
    lblDaysAhead: TLabel;
    lblDateRange: TLabel;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPredictClick(Sender: TObject);
    procedure btnTrainClick(Sender: TObject);
    procedure cmbPredictionTypeChange(Sender: TObject);
  private
    FDataManager: TDataManager;
    FPredictionService: IPredictionService;
    FChartManager: TChartManager;

    procedure OnProgress(Sender: TObject; AProgress: Integer; const AMessage: string);
    procedure UpdateUIForPredictionType;
    procedure LoadHistoricalData;
    procedure DisplayPrediction(const APrediction: TPredictionResult);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Initialisation des gestionnaires
  FDataManager := TDataManager.Create('Fichier=DashboardData.db');

  // Création du service de prédiction (ici Python)
  FPredictionService := TPythonPredictionService.Create(ExtractFilePath(Application.ExeName) + '\Models');
  (FPredictionService as TPythonPredictionService).OnProgress := OnProgress;

  FChartManager := TChartManager.Create(Chart1);

  // Configuration de l'UI
  cmbPredictionType.Items.Clear;
  cmbPredictionType.Items.Add('Prévisions de ventes');
  cmbPredictionType.Items.Add('Prévisions d''inventaire');
  cmbPredictionType.Items.Add('Prédiction d''attrition clients');
  cmbPredictionType.Items.Add('Maintenance prédictive');
  cmbPredictionType.ItemIndex := 0;

  dtpStartDate.Date := IncMonth(Date, -3);  // 3 mois en arrière
  dtpEndDate.Date := Date;  // Aujourd'hui

  UpdateUIForPredictionType;
  LoadHistoricalData;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FChartManager.Free;
  FDataManager.Free;
  // L'interface est libérée automatiquement
end;

procedure TMainForm.btnPredictClick(Sender: TObject);
var
  PredictionType: TPredictionType;
  HistoricalData: TJSONArray;
  DaysAhead: Integer;
  Prediction: TPredictionResult;
begin
  // Mise à jour de l'interface
  btnPredict.Enabled := False;
  btnTrain.Enabled := False;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;

  try
    // Récupération des paramètres
    PredictionType := TPredictionType(cmbPredictionType.ItemIndex);
    DaysAhead := StrToIntDef(edtDaysAhead.Text, 30);

    // Récupération des données historiques selon le type de prédiction
    case PredictionType of
      ptSales:
        HistoricalData := FDataManager.GetSalesData(dtpStartDate.Date, dtpEndDate.Date);
      ptInventory:
        HistoricalData := FDataManager.GetInventoryData(1, dtpStartDate.Date, dtpEndDate.Date);
      ptCustomerChurn:
        HistoricalData := FDataManager.GetCustomerData;
      // Ajoutez d'autres cas selon vos besoins
      else
        raise Exception.Create('Type de prédiction non pris en charge');
    end;

    // Exécution asynchrone de la prédiction pour ne pas bloquer l'UI
    TTask.Run(
      procedure
      begin
        try
          // Appel au service de prédiction
          Prediction := FPredictionService.Predict(PredictionType, HistoricalData, DaysAhead);

          // Sauvegarde du résultat
          FDataManager.SavePrediction(Prediction);

          // Mise à jour de l'interface (thread principal)
          TThread.Synchronize(nil,
            procedure
            begin
              DisplayPrediction(Prediction);
              btnPredict.Enabled := True;
              btnTrain.Enabled := True;
              ProgressBar1.Visible := False;
              StatusBar1.SimpleText := 'Prédiction terminée avec succès.';
            end
          );
        except
          on E: Exception do
            TThread.Synchronize(nil,
              procedure
              begin
                ShowMessage('Erreur lors de la prédiction: ' + E.Message);
                btnPredict.Enabled := True;
                btnTrain.Enabled := True;
                ProgressBar1.Visible := False;
                StatusBar1.SimpleText := 'Erreur: ' + E.Message;
              end
            );
        end;
      end
    );

  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);
      btnPredict.Enabled := True;
      btnTrain.Enabled := True;
      ProgressBar1.Visible := False;
    end;
  end;
end;

procedure TMainForm.DisplayPrediction(const APrediction: TPredictionResult);
var
  I: Integer;
  HistoricalValues: TArray<Double>;
  HistoricalDates: TArray<TDateTime>;
  HistoricalData: TJSONArray;
begin
  // Récupération des données historiques pour l'affichage
  case APrediction.PredictionType of
    ptSales:
      HistoricalData := FDataManager.GetSalesData(dtpStartDate.Date, dtpEndDate.Date);
    // Ajoutez d'autres cas selon vos besoins
    else
      raise Exception.Create('Type de prédiction non pris en charge');
  end;

  // Extraction des valeurs et dates pour le graphique
  SetLength(HistoricalValues, HistoricalData.Count);
  SetLength(HistoricalDates, HistoricalData.Count);

  for I := 0 to HistoricalData.Count - 1 do
  begin
    var Obj := HistoricalData.Items[I] as TJSONObject;

    case APrediction.PredictionType of
      ptSales:
        HistoricalValues[I] := Obj.GetValue('Sales').AsType<Double>;
      // Autres cas...
    end;

    HistoricalDates[I] := ISO8601ToDate(Obj.GetValue('Date').Value);
  end;

  // Affichage du graphique
  FChartManager.CreateTimeSeriesChart(APrediction, HistoricalValues, HistoricalDates);
end;

procedure TMainForm.OnProgress(Sender: TObject; AProgress: Integer;
                             const AMessage: string);
begin
  // Mise à jour de l'interface pour montrer la progression
  TThread.Queue(nil,
    procedure
    begin
      ProgressBar1.Position := AProgress;
      StatusBar1.SimpleText := AMessage;
      Application.ProcessMessages;
    end
  );
end;

// Autres méthodes...

end.
```

## Considérations pour les projets IA/ML en production

Pour des projets destinés à un environnement de production, considérez ces aspects supplémentaires:

### 1. Déploiement simplifié

Créez un installateur qui configure automatiquement l'environnement Python:

```pascal
procedure TSetupForm.btnInstallClick(Sender: TObject);
begin
  TTask.Run(
    procedure
    begin
      try
        // Mise à jour de l'interface
        TThread.Synchronize(nil,
          procedure begin lblStatus.Caption := 'Téléchargement de Python...'; end);

        // Téléchargement de Python si nécessaire
        if not IsPythonInstalled then
          DownloadPython(PYTHON_VERSION);

        TThread.Synchronize(nil,
          procedure begin lblStatus.Caption := 'Installation des dépendances...'; end);

        // Installation des bibliothèques Python
        InstallPythonLibraries(['numpy', 'pandas', 'scikit-learn', 'matplotlib']);

        TThread.Synchronize(nil,
          procedure begin lblStatus.Caption := 'Configuration de l''application...'; end);

        // Configuration de l'application
        ConfigureApplication;

        TThread.Synchronize(nil,
          procedure
          begin
            lblStatus.Caption := 'Installation terminée.';
            btnNext.Enabled := True;
          end);
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              lblStatus.Caption := 'Erreur: ' + E.Message;
              btnRetry.Visible := True;
            end);
      end;
    end
  );
end;
```

### 2. Surveillance et maintenance des modèles

Les modèles d'IA/ML peuvent se dégrader avec le temps, notamment lorsque les données changent. Implémentez un système pour surveiller la performance des modèles:

```pascal
// Classe pour surveiller la performance des modèles
TModelMonitor = class
private
  FConnection: TFDConnection;
  FModelsPath: string;
  FPredictionService: IPredictionService;

  procedure LogModelPerformance(APredictionType: TPredictionType;
                               AAccuracy: Double;
                               const ATimestamp: TDateTime);
public
  constructor Create(const AConnectionString, AModelsPath: string;
                   APredictionService: IPredictionService);

  // Vérifier si un modèle doit être ré-entraîné
  function ShouldRetrainModel(APredictionType: TPredictionType): Boolean;

  // Évaluer la performance du modèle sur de nouvelles données
  function EvaluateModelPerformance(APredictionType: TPredictionType;
                                 const ATestData: TJSONArray): Double;

  // Générer un rapport de performance
  procedure GeneratePerformanceReport(const AOutputPath: string);
end;

function TModelMonitor.ShouldRetrainModel(APredictionType: TPredictionType): Boolean;
var
  LastTrainingDate: TDateTime;
  CurrentAccuracy, PreviousAccuracy: Double;
  Query: TFDQuery;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'SELECT TrainingDate, Accuracy FROM ModelPerformance ' +
      'WHERE PredictionType = :type ORDER BY TrainingDate DESC LIMIT 2';
    Query.ParamByName('type').AsInteger := Ord(APredictionType);
    Query.Open;

    if Query.RecordCount = 0 then
      Exit(True); // Jamais entraîné

    LastTrainingDate := Query.FieldByName('TrainingDate').AsDateTime;
    CurrentAccuracy := Query.FieldByName('Accuracy').AsFloat;

    // Si le modèle n'a pas été entraîné depuis plus de 30 jours
    if DaysBetween(Now, LastTrainingDate) > 30 then
      Exit(True);

    // Si l'enregistrement précédent existe, vérifiez la dégradation des performances
    if not Query.Eof then
    begin
      Query.Next;
      PreviousAccuracy := Query.FieldByName('Accuracy').AsFloat;

      // Si la précision a chuté de plus de 5%
      if (CurrentAccuracy < PreviousAccuracy) and
         ((PreviousAccuracy - CurrentAccuracy) / PreviousAccuracy > 0.05) then
        Exit(True);
    end;
  finally
    Query.Free;
  end;
end;
```

### 3. Optimisation des performances

Pour des applications avec des contraintes de temps réel, optimisez les performances:

```pascal
procedure TOptimizationForm.btnOptimizeClick(Sender: TObject);
begin
  memLog.Lines.Add('Début de l''optimisation...');

  // 1. Convertir les modèles en ONNX pour accélérer l'inférence
  memLog.Lines.Add('Conversion des modèles en format ONNX...');
  PythonEngine1.ExecString(
    'import onnxruntime as ort' + sLineBreak +
    'import onnxmltools' + sLineBreak +
    'from skl2onnx import convert_sklearn' + sLineBreak +
    'from skl2onnx.common.data_types import FloatTensorType' + sLineBreak +
    '' + sLineBreak +
    '# Chargement du modèle scikit-learn' + sLineBreak +
    'import pickle' + sLineBreak +
    'with open("Models/model_ptSales.pkl", "rb") as f:' + sLineBreak +
    '    model = pickle.load(f)' + sLineBreak +
    '' + sLineBreak +
    '# Conversion en ONNX' + sLineBreak +
    'initial_type = [("float_input", FloatTensorType([None, 10]))]' + sLineBreak +
    'onnx_model = convert_sklearn(model, initial_types=initial_type)' + sLineBreak +
    '' + sLineBreak +
    '# Sauvegarde du modèle ONNX' + sLineBreak +
    'with open("Models/model_ptSales.onnx", "wb") as f:' + sLineBreak +
    '    f.write(onnx_model.SerializeToString())'
  );

  // 2. Précalculer les transformations de données courantes
  memLog.Lines.Add('Précalcul des transformations de données...');
  PythonEngine1.ExecString(
    'import numpy as np' + sLineBreak +
    'import joblib' + sLineBreak +
    '' + sLineBreak +
    '# Chargement des données d''exemple' + sLineBreak +
    'sample_data = np.load("Data/sample_input.npy")' + sLineBreak +
    '' + sLineBreak +
    '# Enregistrement du scaler préentraîné' + sLineBreak +
    'from sklearn.preprocessing import StandardScaler' + sLineBreak +
    'scaler = StandardScaler()' + sLineBreak +
    'scaler.fit(sample_data)' + sLineBreak +
    'joblib.dump(scaler, "Models/scaler.pkl")'
  );

  // 3. Précharger les modèles au démarrage
  memLog.Lines.Add('Configuration du préchargement des modèles...');
  chkPreloadModels.Checked := True;
  SaveSettings;

  // 4. Implémenter une mise en cache des prédictions récentes
  memLog.Lines.Add('Activation du cache de prédictions...');
  tbCacheSize.Position := 50;  // 50 Mo
  SaveSettings;

  memLog.Lines.Add('Optimisation terminée!');
end;
```

### 4. Partage et déploiement des modèles

Pour les équipes distribuées travaillant sur le même projet, ajoutez des fonctionnalités de partage de modèle:

```pascal
procedure TModelSharingForm.btnShareModelClick(Sender: TObject);
var
  ModelStream: TMemoryStream;
  ModelData: TBytes;
  CompressedStream: TMemoryStream;
  ModelJSON: TJSONObject;
  ModelStr: string;
begin
  // Récupération du modèle sélectionné
  if not FileExists(FModelsPath + '\model_' +
                  cmbModels.Text + '.pkl') then
  begin
    ShowMessage('Modèle introuvable.');
    Exit;
  end;

  // Chargement du modèle en mémoire
  ModelStream := TMemoryStream.Create;
  CompressedStream := TMemoryStream.Create;
  try
    ModelStream.LoadFromFile(FModelsPath + '\model_' +
                           cmbModels.Text + '.pkl');
    ModelStream.Position := 0;

    // Compression du modèle pour réduire la taille
    with TZCompressionStream.Create(CompressedStream, zcMax) do
    try
      CopyFrom(ModelStream, ModelStream.Size);
    finally
      Free;
    end;

    // Conversion en base64
    CompressedStream.Position := 0;
    SetLength(ModelData, CompressedStream.Size);
    CompressedStream.ReadBuffer(ModelData[0], CompressedStream.Size);

    // Création du JSON avec métadonnées
    ModelJSON := TJSONObject.Create;
    try
      ModelJSON.AddPair('model_name', cmbModels.Text);
      ModelJSON.AddPair('version', '1.0');
      ModelJSON.AddPair('created_by', edtAuthor.Text);
      ModelJSON.AddPair('creation_date', DateTimeToStr(Now));
      ModelJSON.AddPair('description', memoDescription.Text);
      ModelJSON.AddPair('accuracy', FloatToStr(FAccuracy));
      ModelJSON.AddPair('framework', 'scikit-learn');
      ModelJSON.AddPair('data', TNetEncoding.Base64.EncodeBytesToString(ModelData));

      // Sauvegarde dans un fichier
      ModelStr := ModelJSON.ToString;
      SaveDialog1.FileName := cmbModels.Text + '_model.json';

      if SaveDialog1.Execute then
        TFile.WriteAllText(SaveDialog1.FileName, ModelStr);

      ShowMessage('Modèle exporté avec succès!');
    finally
      ModelJSON.Free;
    end;
  finally
    ModelStream.Free;
    CompressedStream.Free;
  end;
end;

procedure TModelSharingForm.btnImportModelClick(Sender: TObject);
var
  ModelJSON: TJSONObject;
  ModelStr, ModelName, Base64Data: string;
  ModelData: TBytes;
  DecompressedStream: TMemoryStream;
  CompressedStream: TMemoryStream;
begin
  if OpenDialog1.Execute then
  begin
    try
      // Chargement du fichier JSON
      ModelStr := TFile.ReadAllText(OpenDialog1.FileName);
      ModelJSON := TJSONObject.ParseJSONValue(ModelStr) as TJSONObject;

      try
        // Récupération des métadonnées
        ModelName := ModelJSON.GetValue('model_name').Value;
        Base64Data := ModelJSON.GetValue('data').Value;

        // Affichage des informations
        memModelInfo.Lines.Clear;
        memModelInfo.Lines.Add('Nom: ' + ModelName);
        memModelInfo.Lines.Add('Version: ' + ModelJSON.GetValue('version').Value);
        memModelInfo.Lines.Add('Auteur: ' + ModelJSON.GetValue('created_by').Value);
        memModelInfo.Lines.Add('Date: ' + ModelJSON.GetValue('creation_date').Value);
        memModelInfo.Lines.Add('Précision: ' + ModelJSON.GetValue('accuracy').Value);
        memModelInfo.Lines.Add('Framework: ' + ModelJSON.GetValue('framework').Value);
        memModelInfo.Lines.Add('Description: ' + ModelJSON.GetValue('description').Value);

        // Confirmation
        if MessageDlg('Voulez-vous importer ce modèle?', mtConfirmation,
                     [mbYes, mbNo], 0) = mrYes then
        begin
          // Décodage et décompression
          ModelData := TNetEncoding.Base64.DecodeStringToBytes(Base64Data);

          CompressedStream := TMemoryStream.Create;
          DecompressedStream := TMemoryStream.Create;
          try
            CompressedStream.WriteBuffer(ModelData[0], Length(ModelData));
            CompressedStream.Position := 0;

            // Décompression
            with TZDecompressionStream.Create(CompressedStream) do
            try
              DecompressedStream.CopyFrom(Self, 0);
            finally
              Free;
            end;

            // Sauvegarde du modèle
            DecompressedStream.Position := 0;
            DecompressedStream.SaveToFile(FModelsPath + '\model_' +
                                        ModelName + '.pkl');

            ShowMessage('Modèle importé avec succès!');
            LoadAvailableModels;
          finally
            CompressedStream.Free;
            DecompressedStream.Free;
          end;
        end;
      finally
        ModelJSON.Free;
      end;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''importation: ' + E.Message);
    end;
  end;
end;
```

## Intégration avec d'autres technologies

Voici quelques exemples d'intégration de solutions IA/ML avec d'autres technologies:

### IA et IoT

```pascal
// Exemple de système prédictif pour maintenance basée sur des capteurs IoT
procedure TIoTMaintenanceForm.ProcessSensorData(const ASensorData: TArray<TSensorReading>);
var
  JSONData: TJSONArray;
  JSONItem: TJSONObject;
  I: Integer;
  Prediction: TPredictionResult;
begin
  // Conversion des données de capteurs en JSON pour le modèle
  JSONData := TJSONArray.Create;
  try
    for I := 0 to High(ASensorData) do
    begin
      JSONItem := TJSONObject.Create;
      JSONItem.AddPair('timestamp', FormatDateTime('yyyy-mm-dd hh:nn:ss', ASensorData[I].Timestamp));
      JSONItem.AddPair('temperature', TJSONNumber.Create(ASensorData[I].Temperature));
      JSONItem.AddPair('vibration', TJSONNumber.Create(ASensorData[I].Vibration));
      JSONItem.AddPair('pressure', TJSONNumber.Create(ASensorData[I].Pressure));
      JSONItem.AddPair('humidity', TJSONNumber.Create(ASensorData[I].Humidity));
      JSONItem.AddPair('noise_level', TJSONNumber.Create(ASensorData[I].NoiseLevel));

      JSONData.AddElement(JSONItem);
    end;

    // Prédiction de défaillance
    Prediction := FPredictionService.Predict(ptMaintenance, JSONData, 7);

    // Affichage des résultats
    UpdateMaintenanceChart(Prediction);

    // Alerte si nécessaire
    if NeedsMaintenanceAlert(Prediction) then
      SendMaintenanceAlert('Maintenance préventive recommandée',
        'Le système prévoit une défaillance possible dans les ' +
        IntToStr(DaysUntilFailure(Prediction)) + ' jours.');
  finally
    JSONData.Free;
  end;
end;
```

### IA et bases de données

```pascal
// Service d'analyse de base de données et détection d'anomalies
TDatabaseAnalysisService = class
private
  FConnection: TFDConnection;
  FPredictionService: IPredictionService;

  function BuildDatasetFromQuery(const ASQL: string): TJSONArray;
public
  constructor Create(AConnection: TFDConnection;
                   APredictionService: IPredictionService);

  // Analyse des tendances dans les données
  function AnalyzeTrends(const ATableName, ATimeColumn, AValueColumn: string;
                       ATimeWindow: Integer): TTrendAnalysisResult;

  // Détection des valeurs aberrantes dans les données
  function DetectOutliers(const ATableName, AColumnName: string;
                       ASensitivity: Double = 0.05): TOutlierResult;

  // Analyse des corrélations entre colonnes
  function AnalyzeCorrelations(const ATableName: string;
                            const AColumns: TArray<string>): TCorrelationResult;
end;

function TDatabaseAnalysisService.DetectOutliers(const ATableName, AColumnName: string;
                                            ASensitivity: Double): TOutlierResult;
var
  JSONData: TJSONArray;
  SQL: string;
begin
  // Préparation de la requête SQL pour extraire les données
  SQL := Format('SELECT %s FROM %s', [AColumnName, ATableName]);

  // Conversion des données en format JSON pour le modèle
  JSONData := BuildDatasetFromQuery(SQL);
  try
    // Configuration de Python pour la détection d'anomalies
    PythonEngine1.ExecString(
      'import numpy as np' + sLineBreak +
      'from sklearn.ensemble import IsolationForest' + sLineBreak +
      '' + sLineBreak +
      '# Préparation des données' + sLineBreak +
      'data = np.array(json_data["' + AColumnName + '"]).reshape(-1, 1)' + sLineBreak +
      '' + sLineBreak +
      '# Détection des anomalies avec Isolation Forest' + sLineBreak +
      'model = IsolationForest(contamination=' + FloatToStr(ASensitivity) +
      ', random_state=42)' + sLineBreak +
      'preds = model.fit_predict(data)' + sLineBreak +
      '' + sLineBreak +
      '# Identification des indices des valeurs aberrantes' + sLineBreak +
      'outlier_indices = np.where(preds == -1)[0]' + sLineBreak +
      'outlier_values = data[outlier_indices].flatten()' + sLineBreak +
      '' + sLineBreak +
      '# Calcul des statistiques' + sLineBreak +
      'mean_value = np.mean(data)' + sLineBreak +
      'std_value = np.std(data)' + sLineBreak +
      'percentile_low = np.percentile(data, 5)' + sLineBreak +
      'percentile_high = np.percentile(data, 95)'
    );

    // Récupération des résultats
    Result.OutlierCount := VarPythonAsInteger(PythonEngine1.EvalString('len(outlier_indices)'));
    Result.MeanValue := VarPythonAsFloat(PythonEngine1.EvalString('float(mean_value)'));
    Result.StdDeviation := VarPythonAsFloat(PythonEngine1.EvalString('float(std_value)'));

    // Récupération des indices des valeurs aberrantes
    var OutlierIndices := PythonEngine1.EvalString('outlier_indices.tolist()');
    var OutlierValues := PythonEngine1.EvalString('outlier_values.tolist()');

    SetLength(Result.OutlierIndices, Result.OutlierCount);
    SetLength(Result.OutlierValues, Result.OutlierCount);

    for var I := 0 to Result.OutlierCount - 1 do
    begin
      Result.OutlierIndices[I] := VarPythonAsInteger(
        PythonEngine1.PyList_GetItem(OutlierIndices, I));
      Result.OutlierValues[I] := VarPythonAsFloat(
        PythonEngine1.PyList_GetItem(OutlierValues, I));
    end;
  finally
    JSONData.Free;
  end;
end;
```

## Conclusion: L'avenir de l'IA/ML avec Delphi

L'intégration de l'intelligence artificielle et du machine learning dans vos applications Delphi offre de nouvelles possibilités pour résoudre des problèmes complexes et apporter plus de valeur à vos utilisateurs. Bien que Delphi ne soit pas traditionnellement associé au développement d'IA, les approches présentées dans ce chapitre montrent comment:

1. **Utiliser des services d'IA via des API REST**: solution simple et rapide pour intégrer des capacités d'IA avancées
2. **Exploiter Python et ses bibliothèques** grâce à Python4Delphi: accès à l'écosystème ML le plus riche
3. **Développer des composants natifs** pour les algorithmes plus simples: performance et intégration optimales

À mesure que l'IA devient incontournable dans de nombreux domaines, savoir l'intégrer dans vos applications Delphi existantes représente un avantage concurrentiel significatif. Les projets présentés ici ne sont que le début de ce qu'il est possible de réaliser.

### Pour aller plus loin

Voici quelques pistes pour approfondir vos connaissances:

- Explorez les **modèles de deep learning** comme les réseaux de neurones convolutifs (CNN) pour la vision par ordinateur ou les transformers pour le NLP
- Intégrez des modèles pré-entraînés disponibles sur des plateformes comme Hugging Face
- Explorez d'autres bibliothèques Python spécialisées (PyTorch, Keras, spaCy)
- Créez vos propres composants VCL/FMX spécialisés pour l'IA
- Envisagez des approches hybrides mêlant traitement côté serveur et côté client

Avec les bonnes pratiques et architectures présentées dans ce chapitre, vous avez maintenant les outils pour commencer à développer des applications Delphi intelligentes qui exploitent la puissance de l'IA et du machine learning.

---

_Note: Certains exemples avancés nécessitent Delphi 12 Athens ou supérieur._

⏭️ [Intégration de plateformes de paiement](19-projets-avances/08-integration-de-plateformes-de-paiement.md)
