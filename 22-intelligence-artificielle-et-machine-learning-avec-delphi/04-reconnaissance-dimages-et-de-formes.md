# 22.4 Reconnaissance d'images et de formes

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction à la reconnaissance d'images

La reconnaissance d'images est une branche passionnante de l'intelligence artificielle qui permet à vos applications d'identifier et de comprendre le contenu visuel. Dans cette section, nous allons explorer comment intégrer cette technologie à vos applications Delphi.

## Qu'est-ce que la reconnaissance d'images ?

La reconnaissance d'images (ou vision par ordinateur) est la capacité d'un système informatique à :
- Identifier des objets dans une image
- Détecter des visages et expressions
- Lire du texte à partir d'images (OCR)
- Reconnaître des formes et des patterns
- Analyser le contenu d'une scène

## Approches pour intégrer la reconnaissance d'images dans Delphi

Plusieurs méthodes s'offrent à vous pour ajouter des capacités de vision par ordinateur à vos applications Delphi :

### 1. Services cloud de vision par ordinateur

La méthode la plus simple pour débuter est d'utiliser des API cloud spécialisées :

#### Exemple avec Google Cloud Vision API

```delphi
procedure TFormImageRecognition.AnalyzeImage(const ImagePath: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  FileStream: TFileStream;
  Base64Encoder: TBase64Encoding;
  EncodedImage: string;
  ResponseJSON: TJSONValue;
begin
  // Lire l'image et la convertir en Base64
  FileStream := TFileStream.Create(ImagePath, fmOpenRead);
  try
    Base64Encoder := TBase64Encoding.Create;
    try
      SetLength(EncodedImage, Base64Encoder.EncodedLength(FileStream.Size));
      FileStream.ReadBuffer(EncodedImage[1], FileStream.Size);
      EncodedImage := Base64Encoder.Encode(EncodedImage);
    finally
      Base64Encoder.Free;
    end;
  finally
    FileStream.Free;
  end;

  // Préparer la requête REST
  RESTClient := TRESTClient.Create('https://vision.googleapis.com/v1/images:annotate');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajouter la clé API dans l'URL
    RESTClient.BaseURL := RESTClient.BaseURL + '?key=' + GOOGLE_API_KEY;

    // Construire le corps de la requête
    var RequestsArray := TJSONArray.Create;
    var RequestObj := TJSONObject.Create;
    var ImageObj := TJSONObject.Create;
    var FeaturesArray := TJSONArray.Create;

    // Spécifier l'image
    ImageObj.AddPair('content', EncodedImage);

    // Spécifier les caractéristiques à détecter
    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'LABEL_DETECTION')
        .AddPair('maxResults', TJSONNumber.Create(10))
    );

    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'OBJECT_LOCALIZATION')
        .AddPair('maxResults', TJSONNumber.Create(5))
    );

    // Assembler la requête
    RequestObj.AddPair('image', ImageObj);
    RequestObj.AddPair('features', FeaturesArray);
    RequestsArray.Add(RequestObj);
    RequestBody.AddPair('requests', RequestsArray);

    RESTRequest.Body.Add(RequestBody.ToJSON);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Parcourir les résultats
        var Responses := ResponseJSON.GetValue<TJSONArray>('responses');
        var Response := Responses.Items[0] as TJSONObject;

        // Afficher les étiquettes (labels) trouvées
        if Response.TryGetValue<TJSONArray>('labelAnnotations', var Labels) then
        begin
          MemoResults.Lines.Add('Objets détectés :');
          for var i := 0 to Labels.Count - 1 do
          begin
            var Label := Labels.Items[i] as TJSONObject;
            var Description := Label.GetValue<string>('description');
            var Score := Label.GetValue<Double>('score') * 100;

            MemoResults.Lines.Add(Format('- %s (confiance: %.1f%%)', [Description, Score]));
          end;
        end;

        // Afficher les objets localisés
        if Response.TryGetValue<TJSONArray>('localizedObjectAnnotations', var Objects) then
        begin
          MemoResults.Lines.Add('');
          MemoResults.Lines.Add('Objets localisés :');
          for var i := 0 to Objects.Count - 1 do
          begin
            var Obj := Objects.Items[i] as TJSONObject;
            var Name := Obj.GetValue<string>('name');
            var Score := Obj.GetValue<Double>('score') * 100;

            MemoResults.Lines.Add(Format('- %s (confiance: %.1f%%)', [Name, Score]));
          end;
        end;
      finally
        ResponseJSON.Free;
      end;
    end
    else
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;
```

> 🔹 **Note** : Ce code nécessite une clé API Google Cloud Vision que vous pouvez obtenir en créant un projet sur Google Cloud Platform.

#### Exemple avec Azure Computer Vision

```delphi
procedure TFormImageRecognition.AnalyzeWithAzure(const ImagePath: string);
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Content: TMultipartFormData;
  ResponseJSON: TJSONValue;
begin
  HttpClient := THTTPClient.Create;
  Content := TMultipartFormData.Create;

  try
    // Configuration des en-têtes
    HttpClient.CustomHeaders['Ocp-Apim-Subscription-Key'] := AZURE_API_KEY;

    // Ajouter l'image
    Content.AddFile('image', ImagePath);

    // Exécuter la requête
    Response := HttpClient.Post(
      'https://votreressource.cognitiveservices.azure.com/vision/v3.2/analyze?visualFeatures=Objects,Faces,Description',
      Content
    );

    // Traiter la réponse
    if Response.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString);
      try
        // Traitement de la réponse (voir documentation Azure)
        // ...
      finally
        ResponseJSON.Free;
      end;
    end
    else
      ShowMessage('Erreur : ' + Response.StatusText);
  finally
    HttpClient.Free;
    Content.Free;
  end;
end;
```

### 2. Bibliothèques locales de vision par ordinateur

Pour les applications qui nécessitent une solution sans connexion internet ou pour des raisons de confidentialité, vous pouvez utiliser des bibliothèques exécutées localement :

#### Utilisation d'OpenCV via FFI (Foreign Function Interface)

```delphi
// Déclaration des fonctions OpenCV
function cv_imread(filename: PAnsiChar): Pointer; cdecl; external 'opencv_wrapper.dll';
procedure cv_face_detect(image: Pointer; faces: PRect; max_faces: Integer; var count: Integer); cdecl; external 'opencv_wrapper.dll';
procedure cv_release_image(image: Pointer); cdecl; external 'opencv_wrapper.dll';

procedure TFormFaceDetection.DetectFaces(const ImagePath: string);
var
  Image: Pointer;
  Faces: array[0..20] of TRect; // Maximum 20 visages
  Count: Integer;
  i: Integer;
begin
  // Charger l'image via OpenCV
  Image := cv_imread(PAnsiChar(AnsiString(ImagePath)));
  if Image = nil then
  begin
    ShowMessage('Impossible de charger l''image');
    Exit;
  end;

  try
    // Détecter les visages
    Count := 0;
    cv_face_detect(Image, @Faces[0], 20, Count);

    // Afficher les résultats
    MemoResults.Lines.Clear;
    MemoResults.Lines.Add(Format('Détection terminée : %d visage(s) trouvé(s)', [Count]));

    // Dessiner les rectangles sur l'image
    LoadImageToTImage(ImagePath, Image1);

    for i := 0 to Count - 1 do
    begin
      // Dessiner un rectangle autour du visage
      DrawRectangle(Image1.Canvas, Faces[i], clRed, 2);

      MemoResults.Lines.Add(Format('Visage %d : Position X=%d, Y=%d, Largeur=%d, Hauteur=%d',
        [i+1, Faces[i].Left, Faces[i].Top, Faces[i].Width, Faces[i].Height]));
    end;
  finally
    // Libérer la mémoire
    cv_release_image(Image);
  end;
end;

procedure TFormFaceDetection.DrawRectangle(Canvas: TCanvas; Rect: TRect; Color: TColor; Thickness: Integer);
begin
  Canvas.Pen.Color := Color;
  Canvas.Pen.Width := Thickness;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(Rect);
end;
```

> ⚠️ **Note** : Cette approche nécessite la création d'une DLL wrapper pour OpenCV, car l'interface directe avec OpenCV est complexe. Cette DLL n'est pas incluse dans cet exemple.

### 3. Communication avec Python pour la vision par ordinateur

Une approche hybride consiste à utiliser Python avec des bibliothèques comme OpenCV, PIL ou TensorFlow, et à communiquer avec ce code depuis Delphi :

```delphi
procedure TFormImageProcessing.ProcessImage(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('image_process.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage des résultats
    MemoResults.Lines.Clear;
    MemoResults.Lines.Add('Résultats de l''analyse d''image :');
    MemoResults.Lines.Add('');
    MemoResults.Lines.AddStrings(Output);

    // Charger l'image traitée si elle existe
    if FileExists(ChangeFileExt(ImagePath, '_processed.jpg')) then
      Image1.Picture.LoadFromFile(ChangeFileExt(ImagePath, '_processed.jpg'));
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python correspondant (image_process.py) pourrait être :

```python
import sys
import cv2
import numpy as np

def process_image(image_path):
    # Charger l'image avec OpenCV
    image = cv2.imread(image_path)
    if image is None:
        print("Erreur: Impossible de charger l'image")
        return

    # Convertir en niveaux de gris
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Détection des visages
    face_cascade = cv2.CascadeClassifier(cv2.data.haarcascades + 'haarcascade_frontalface_default.xml')
    faces = face_cascade.detectMultiScale(gray, 1.3, 5)

    # Dessiner les rectangles autour des visages
    for (x, y, w, h) in faces:
        cv2.rectangle(image, (x, y), (x+w, y+h), (255, 0, 0), 2)

    # Enregistrer l'image modifiée
    output_path = image_path.replace('.jpg', '_processed.jpg').replace('.png', '_processed.jpg')
    cv2.imwrite(output_path, image)

    # Afficher les résultats
    print(f"Nombre de visages détectés : {len(faces)}")
    for i, (x, y, w, h) in enumerate(faces):
        print(f"Visage {i+1}: Position X={x}, Y={y}, Largeur={w}, Hauteur={h}")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        process_image(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

## Exemple pratique : Application de reconnaissance d'objets

Créons une application simple qui permet de charger une image et d'identifier les objets qu'elle contient :

```delphi
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ExtDlgs, System.JSON, REST.Types, REST.Client, System.Net.URLClient,
  System.Net.HttpClient, System.Net.HttpClientComponent;

type
  TFormObjectRecognition = class(TForm)
    PanelLeft: TPanel;
    PanelRight: TPanel;
    Image1: TImage;
    MemoResults: TMemo;
    ButtonLoad: TButton;
    ButtonAnalyze: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    ButtonClear: TButton;
    StatusBar1: TStatusBar;
    LabelImageInfo: TLabel;
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FImagePath: string;
    procedure AnalyzeImage;
    function EncodeImageToBase64(const ImagePath: string): string;
  public
    { Public declarations }
  end;

var
  FormObjectRecognition: TFormObjectRecognition;

implementation

{$R *.dfm}

const
  API_KEY = 'votre_cle_api'; // Remplacez par votre clé API

procedure TFormObjectRecognition.FormCreate(Sender: TObject);
begin
  ButtonAnalyze.Enabled := False;
end;

procedure TFormObjectRecognition.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    try
      // Charger l'image
      Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      FImagePath := OpenPictureDialog1.FileName;

      // Afficher les informations sur l'image
      var ImgWidth := Image1.Picture.Width;
      var ImgHeight := Image1.Picture.Height;
      LabelImageInfo.Caption := Format('Dimensions: %d x %d pixels', [ImgWidth, ImgHeight]);

      // Activer le bouton d'analyse
      ButtonAnalyze.Enabled := True;

      // Effacer les résultats précédents
      MemoResults.Clear;
      StatusBar1.SimpleText := 'Image chargée';
    except
      on E: Exception do
        ShowMessage('Erreur lors du chargement de l''image : ' + E.Message);
    end;
  end;
end;

procedure TFormObjectRecognition.ButtonAnalyzeClick(Sender: TObject);
begin
  if FImagePath = '' then
  begin
    ShowMessage('Veuillez d''abord charger une image.');
    Exit;
  end;

  ButtonAnalyze.Enabled := False;
  StatusBar1.SimpleText := 'Analyse en cours...';
  MemoResults.Clear;

  // Lancer l'analyse dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    begin
      try
        AnalyzeImage;
      finally
        // Réactiver le bouton dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            ButtonAnalyze.Enabled := True;
            StatusBar1.SimpleText := 'Analyse terminée';
          end
        );
      end;
    end
  ).Start;
end;

procedure TFormObjectRecognition.ButtonClearClick(Sender: TObject);
begin
  Image1.Picture := nil;
  MemoResults.Clear;
  FImagePath := '';
  ButtonAnalyze.Enabled := False;
  LabelImageInfo.Caption := 'Aucune image chargée';
  StatusBar1.SimpleText := 'Prêt';
end;

procedure TFormObjectRecognition.AnalyzeImage;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  EncodedImage: string;
  ResponseJSON: TJSONValue;
begin
  EncodedImage := EncodeImageToBase64(FImagePath);

  RESTClient := TRESTClient.Create('https://vision.googleapis.com/v1/images:annotate');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajouter la clé API dans l'URL
    RESTClient.BaseURL := RESTClient.BaseURL + '?key=' + API_KEY;

    // Construire le corps de la requête
    var RequestsArray := TJSONArray.Create;
    var RequestObj := TJSONObject.Create;
    var ImageObj := TJSONObject.Create;
    var FeaturesArray := TJSONArray.Create;

    // Spécifier l'image
    ImageObj.AddPair('content', EncodedImage);

    // Spécifier les caractéristiques à détecter
    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'LABEL_DETECTION')
        .AddPair('maxResults', TJSONNumber.Create(10))
    );

    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'OBJECT_LOCALIZATION')
        .AddPair('maxResults', TJSONNumber.Create(5))
    );

    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'TEXT_DETECTION')
        .AddPair('maxResults', TJSONNumber.Create(10))
    );

    // Assembler la requête
    RequestObj.AddPair('image', ImageObj);
    RequestObj.AddPair('features', FeaturesArray);
    RequestsArray.Add(RequestObj);
    RequestBody.AddPair('requests', RequestsArray);

    RESTRequest.Body.Add(RequestBody.ToJSON);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse dans le thread principal
    TThread.Synchronize(nil,
      procedure
      begin
        if RESTResponse.StatusCode = 200 then
        begin
          ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
          try
            // Parcourir les résultats
            var Responses := ResponseJSON.GetValue<TJSONArray>('responses');
            var Response := Responses.Items[0] as TJSONObject;

            // Afficher les étiquettes (labels) trouvées
            if Response.TryGetValue<TJSONArray>('labelAnnotations', var Labels) then
            begin
              MemoResults.Lines.Add('OBJETS DÉTECTÉS :');
              for var i := 0 to Labels.Count - 1 do
              begin
                var Label := Labels.Items[i] as TJSONObject;
                var Description := Label.GetValue<string>('description');
                var Score := Label.GetValue<Double>('score') * 100;

                MemoResults.Lines.Add(Format('- %s (confiance: %.1f%%)', [Description, Score]));
              end;
            end;

            // Afficher les objets localisés
            if Response.TryGetValue<TJSONArray>('localizedObjectAnnotations', var Objects) then
            begin
              MemoResults.Lines.Add('');
              MemoResults.Lines.Add('OBJETS LOCALISÉS :');
              for var i := 0 to Objects.Count - 1 do
              begin
                var Obj := Objects.Items[i] as TJSONObject;
                var Name := Obj.GetValue<string>('name');
                var Score := Obj.GetValue<Double>('score') * 100;

                MemoResults.Lines.Add(Format('- %s (confiance: %.1f%%)', [Name, Score]));
              end;
            end;

            // Afficher le texte détecté
            if Response.TryGetValue<TJSONArray>('textAnnotations', var Texts) and (Texts.Count > 0) then
            begin
              MemoResults.Lines.Add('');
              MemoResults.Lines.Add('TEXTE DÉTECTÉ :');

              // Le premier élément contient généralement tout le texte
              var FullText := (Texts.Items[0] as TJSONObject).GetValue<string>('description');
              MemoResults.Lines.Add(FullText);
            end;
          finally
            ResponseJSON.Free;
          end;
        end
        else
        begin
          MemoResults.Lines.Add('Erreur : ' + RESTResponse.StatusText);
          if RESTResponse.Content <> '' then
            MemoResults.Lines.Add(RESTResponse.Content);
        end;
      end
    );
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;

function TFormObjectRecognition.EncodeImageToBase64(const ImagePath: string): string;
var
  FileStream: TFileStream;
  Bytes: TBytes;
  Base64: TBase64Encoding;
begin
  FileStream := TFileStream.Create(ImagePath, fmOpenRead);
  try
    SetLength(Bytes, FileStream.Size);
    FileStream.ReadBuffer(Bytes, 0, Length(Bytes));
    Base64 := TBase64Encoding.Create;
    try
      Result := Base64.EncodeBytesToString(Bytes);
    finally
      Base64.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
```

Et le fichier DFM correspondant :

```delphi
object FormObjectRecognition: TFormObjectRecognition
  Left = 0
  Top = 0
  Caption = 'Reconnaissance d'#39'objets'
  ClientHeight = 550
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 400
    Height = 531
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 611
    object Image1: TImage
      AlignWithMargins = True
      Left = 11
      Top = 11
      Width = 378
      Height = 430
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Center = True
      Proportional = True
      Stretch = True
      ExplicitLeft = 8
      ExplicitTop = 8
      ExplicitWidth = 385
      ExplicitHeight = 489
    end
    object LabelImageInfo: TLabel
      Left = 11
      Top = 451
      Width = 378
      Height = 15
      Align = alBottom
      Caption = 'Aucune image charg'#233'e'
      ExplicitWidth = 113
    end
    object ButtonLoad: TButton
      Left = 16
      Top = 480
      Width = 113
      Height = 33
      Caption = 'Charger image'
      TabOrder = 0
      OnClick = ButtonLoadClick
    end
    object ButtonAnalyze: TButton
      Left = 144
      Top = 480
      Width = 113
      Height = 33
      Caption = 'Analyser'
      TabOrder = 1
      OnClick = ButtonAnalyzeClick
    end
    object ButtonClear: TButton
      Left = 272
      Top = 480
      Width = 113
      Height = 33
      Caption = 'Effacer'
      TabOrder = 2
      OnClick = ButtonClearClick
    end
  end
  object PanelRight: TPanel
    Left = 400
    Top = 0
    Width = 400
    Height = 531
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 484
    ExplicitHeight = 611
    object MemoResults: TMemo
      AlignWithMargins = True
      Left = 11
      Top = 11
      Width = 378
      Height = 509
      Margins.Left = 10
      Margins.Top = 10
      Margins.Right = 10
      Margins.Bottom = 10
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitWidth = 462
      ExplicitHeight = 589
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 531
    Width = 800
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Pr'#234't'
    ExplicitTop = 611
    ExplicitWidth = 884
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter =
      'Tous les formats|*.jpg;*.jpeg;*.png;*.bmp|JPEG (*.jpg;*.jpeg)|*.' +
      'jpg;*.jpeg|PNG (*.png)|*.png|Bitmap (*.bmp)|*.bmp'
    Title = 'S'#233'lectionner une image'
    Left = 32
    Top = 32
  end
end
```

## Cas d'utilisation de la reconnaissance d'images

### 1. Reconnaissance de codes-barres et QR codes

```delphi
procedure TFormBarcodeScanner.ScanBarcode(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('barcode_scanner.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Traitement des résultats
    if Output.Count > 0 then
    begin
      LabelResult.Caption := 'Code détecté: ' + Output[0];

      // Si c'est une URL, proposer de l'ouvrir
      if Output[0].StartsWith('http') then
        ButtonOpenURL.Visible := True
      else
        ButtonOpenURL.Visible := False;
    end
    else
      LabelResult.Caption := 'Aucun code détecté';
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python correspondant (barcode_scanner.py) pourrait être :

```python
import sys
import cv2
from pyzbar.pyzbar import decode

def scan_barcode(image_path):
    # Charger l'image
    image = cv2.imread(image_path)

    # Rechercher des codes-barres ou QR codes
    barcodes = decode(image)

    # Afficher les résultats
    for barcode in barcodes:
        # Récupérer les données du code
        barcode_data = barcode.data.decode('utf-8')
        print(barcode_data)
        return

    # Si aucun code n'est trouvé
    print("")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        scan_barcode(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

# 22.4 Reconnaissance d'images et de formes - OCR et traitement avancé

### 2. OCR (Reconnaissance optique de caractères)

L'OCR est une technologie qui permet d'extraire du texte à partir d'images. Voici comment l'intégrer dans vos applications Delphi :

#### Utilisation de l'API Google Cloud Vision pour l'OCR

```delphi
procedure TFormOCR.PerformOCR(const ImagePath: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  EncodedImage: string;
  ResponseJSON: TJSONValue;
begin
  // Encoder l'image en Base64
  EncodedImage := EncodeImageToBase64(ImagePath);

  // Préparer la requête REST
  RESTClient := TRESTClient.Create('https://vision.googleapis.com/v1/images:annotate');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajouter la clé API dans l'URL
    RESTClient.BaseURL := RESTClient.BaseURL + '?key=' + GOOGLE_API_KEY;

    // Construire le corps de la requête
    var RequestsArray := TJSONArray.Create;
    var RequestObj := TJSONObject.Create;
    var ImageObj := TJSONObject.Create;
    var FeaturesArray := TJSONArray.Create;

    // Spécifier l'image
    ImageObj.AddPair('content', EncodedImage);

    // Spécifier que nous voulons faire de l'OCR
    FeaturesArray.Add(
      TJSONObject.Create
        .AddPair('type', 'TEXT_DETECTION')
        .AddPair('maxResults', TJSONNumber.Create(10))
    );

    // Assembler la requête
    RequestObj.AddPair('image', ImageObj);
    RequestObj.AddPair('features', FeaturesArray);
    RequestsArray.Add(RequestObj);
    RequestBody.AddPair('requests', RequestsArray);

    RESTRequest.Body.Add(RequestBody.ToJSON);

    // Exécuter la requête
    RESTRequest.Execute;

    // Traiter la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraire le texte de la réponse
        var Responses := ResponseJSON.GetValue<TJSONArray>('responses');
        var Response := Responses.Items[0] as TJSONObject;

        if Response.TryGetValue<TJSONArray>('textAnnotations', var TextAnnotations) and
           (TextAnnotations.Count > 0) then
        begin
          // Le premier élément contient généralement tout le texte
          var FullText := (TextAnnotations.Items[0] as TJSONObject).GetValue<string>('description');

          // Afficher le texte extrait
          MemoExtractedText.Lines.Text := FullText;

          // Afficher les informations détaillées pour chaque bloc de texte
          MemoDetails.Lines.Clear;
          MemoDetails.Lines.Add('Détails de l''extraction :');

          for var i := 1 to TextAnnotations.Count - 1 do // On commence à 1 car 0 est le texte complet
          begin
            var Annotation := TextAnnotations.Items[i] as TJSONObject;
            var WordText := Annotation.GetValue<string>('description');

            MemoDetails.Lines.Add(Format('Mot "%s"', [WordText]));

            // Optionnel : extraire et afficher la position du texte
            if Annotation.TryGetValue<TJSONObject>('boundingPoly', var BoundingPoly) and
               BoundingPoly.TryGetValue<TJSONArray>('vertices', var Vertices) then
            begin
              var X1 := (Vertices.Items[0] as TJSONObject).GetValue<Integer>('x');
              var Y1 := (Vertices.Items[0] as TJSONObject).GetValue<Integer>('y');
              var X2 := (Vertices.Items[2] as TJSONObject).GetValue<Integer>('x');
              var Y2 := (Vertices.Items[2] as TJSONObject).GetValue<Integer>('y');

              MemoDetails.Lines.Add(Format('  Position: (%d,%d) à (%d,%d)', [X1, Y1, X2, Y2]));

              // Dessiner un rectangle autour du mot sur l'image
              DrawRectangleOnImage(TRect.Create(TPoint.Create(X1, Y1), TPoint.Create(X2, Y2)));
            end;

            MemoDetails.Lines.Add('');
          end;
        end
        else
          ShowMessage('Aucun texte détecté dans l''image.');
      finally
        ResponseJSON.Free;
      end;
    end
    else
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;

procedure TFormOCR.DrawRectangleOnImage(const Rect: TRect);
begin
  // Dessiner un rectangle sur l'image
  Image1.Canvas.Pen.Color := clRed;
  Image1.Canvas.Pen.Width := 2;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(Rect);
end;
```

#### Utilisation de Tesseract OCR via Python

Tesseract est une bibliothèque OCR open-source très puissante. Voici comment l'utiliser via Python :

```delphi
procedure TFormOCR.PerformTesseractOCR(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('tesseract_ocr.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage du texte extrait
    MemoExtractedText.Clear;
    MemoExtractedText.Lines.AddStrings(Output);

    // Si un fichier de coordonnées a été généré, le lire
    if FileExists(ChangeFileExt(ImagePath, '_boxes.txt')) then
    begin
      var BoxesFile := TStringList.Create;
      try
        BoxesFile.LoadFromFile(ChangeFileExt(ImagePath, '_boxes.txt'));

        // Dessiner les rectangles sur l'image
        for var i := 0 to BoxesFile.Count - 1 do
        begin
          var Parts := BoxesFile[i].Split([',']);
          if Length(Parts) = 5 then // x,y,width,height,text
          begin
            var X := StrToIntDef(Parts[0], 0);
            var Y := StrToIntDef(Parts[1], 0);
            var Width := StrToIntDef(Parts[2], 0);
            var Height := StrToIntDef(Parts[3], 0);

            DrawRectangleOnImage(TRect.Create(X, Y, X + Width, Y + Height));
          end;
        end;
      finally
        BoxesFile.Free;
      end;
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python (tesseract_ocr.py) :

```python
import sys
import cv2
import pytesseract
from pytesseract import Output

def perform_ocr(image_path):
    # Charger l'image
    image = cv2.imread(image_path)

    # Convertir en niveaux de gris
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Appliquer un léger flou pour réduire le bruit
    gray = cv2.GaussianBlur(gray, (5, 5), 0)

    # Appliquer un seuillage adaptatif pour améliorer la reconnaissance
    thresh = cv2.adaptiveThreshold(gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C,
                                  cv2.THRESH_BINARY, 11, 2)

    # Effectuer l'OCR avec Tesseract
    config = r'--oem 3 --psm 6'  # Mode d'opération et segmentation de page
    text = pytesseract.image_to_string(thresh, config=config)

    # Obtenir des informations sur les boîtes de texte
    boxes = pytesseract.image_to_data(thresh, config=config, output_type=Output.DICT)

    # Sauvegarder les coordonnées des boîtes dans un fichier
    with open(image_path.replace('.jpg', '_boxes.txt').replace('.png', '_boxes.txt'), 'w') as f:
        for i in range(len(boxes['text'])):
            # Ignorer les entrées vides
            if boxes['text'][i].strip() != '':
                x = boxes['left'][i]
                y = boxes['top'][i]
                w = boxes['width'][i]
                h = boxes['height'][i]
                text = boxes['text'][i]
                f.write(f"{x},{y},{w},{h},{text}\n")

    # Retourner le texte extrait
    print(text)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        perform_ocr(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

> ⚠️ **Note** : Ce script nécessite l'installation de Tesseract OCR et de la bibliothèque Python pytesseract. Vous pouvez installer Tesseract depuis [https://github.com/UB-Mannheim/tesseract/wiki](https://github.com/UB-Mannheim/tesseract/wiki).

### 3. Détection de visages et d'expressions

La détection de visages est très utile pour de nombreuses applications, comme les systèmes de sécurité ou les filtres photo :

```delphi
procedure TFormFaceDetection.DetectFaces(const ImagePath: string);
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  Content: TMultipartFormData;
  ResponseJSON: TJSONValue;
begin
  HttpClient := THTTPClient.Create;
  Content := TMultipartFormData.Create;

  try
    // Configuration des en-têtes pour Azure Face API
    HttpClient.CustomHeaders['Ocp-Apim-Subscription-Key'] := AZURE_FACE_API_KEY;

    // Paramètres de détection (visages, points de repère, attributs)
    var Parameters := 'returnFaceId=true&returnFaceLandmarks=true&returnFaceAttributes=age,gender,emotion';

    // Ajouter l'image
    Content.AddFile('image', ImagePath);

    // Exécuter la requête
    Response := HttpClient.Post(
      'https://votreressource.cognitiveservices.azure.com/face/v1.0/detect?' + Parameters,
      Content
    );

    // Traiter la réponse
    if Response.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString);
      try
        if ResponseJSON is TJSONArray then
        begin
          var FacesArray := ResponseJSON as TJSONArray;
          var FaceCount := FacesArray.Count;

          MemoResults.Clear;
          MemoResults.Lines.Add(Format('Nombre de visages détectés : %d', [FaceCount]));
          MemoResults.Lines.Add('');

          // Charger l'image originale
          Image1.Picture.LoadFromFile(ImagePath);

          // Traiter chaque visage
          for var i := 0 to FaceCount - 1 do
          begin
            var Face := FacesArray.Items[i] as TJSONObject;

            // Récupérer le rectangle du visage
            var FaceRectObj := Face.GetValue<TJSONObject>('faceRectangle');
            var Left := FaceRectObj.GetValue<Integer>('left');
            var Top := FaceRectObj.GetValue<Integer>('top');
            var Width := FaceRectObj.GetValue<Integer>('width');
            var Height := FaceRectObj.GetValue<Integer>('height');

            // Récupérer les attributs du visage
            var Attributes := Face.GetValue<TJSONObject>('faceAttributes');
            var Age := Attributes.GetValue<Double>('age');
            var Gender := Attributes.GetValue<string>('gender');

            // Récupérer les émotions
            var Emotions := Attributes.GetValue<TJSONObject>('emotion');
            var Happiness := Emotions.GetValue<Double>('happiness');
            var Sadness := Emotions.GetValue<Double>('sadness');
            var Surprise := Emotions.GetValue<Double>('surprise');
            var Anger := Emotions.GetValue<Double>('anger');

            // Déterminer l'émotion dominante
            var DominantEmotion := 'neutre';
            var MaxEmotionValue := 0.0;

            if Happiness > MaxEmotionValue then
            begin
              MaxEmotionValue := Happiness;
              DominantEmotion := 'heureux';
            end;

            if Sadness > MaxEmotionValue then
            begin
              MaxEmotionValue := Sadness;
              DominantEmotion := 'triste';
            end;

            if Surprise > MaxEmotionValue then
            begin
              MaxEmotionValue := Surprise;
              DominantEmotion := 'surpris';
            end;

            if Anger > MaxEmotionValue then
            begin
              MaxEmotionValue := Anger;
              DominantEmotion := 'en colère';
            end;

            // Dessiner un rectangle autour du visage
            DrawRectangleOnImage(Left, Top, Width, Height, clRed);

            // Afficher les informations sur le visage
            MemoResults.Lines.Add(Format('Visage %d :', [i + 1]));
            MemoResults.Lines.Add(Format('  Âge estimé : %.0f ans', [Age]));
            MemoResults.Lines.Add(Format('  Genre : %s', [
              IfThen(Gender = 'male', 'Homme', 'Femme')
            ]));
            MemoResults.Lines.Add(Format('  Émotion : %s (%.0f%%)', [
              DominantEmotion, MaxEmotionValue * 100
            ]));
            MemoResults.Lines.Add('');
          end;
        end
        else
          ShowMessage('Aucun visage détecté dans l''image.');
      finally
        ResponseJSON.Free;
      end;
    end
    else
      ShowMessage('Erreur : ' + Response.StatusText);
  finally
    HttpClient.Free;
    Content.Free;
  end;
end;

procedure TFormFaceDetection.DrawRectangleOnImage(Left, Top, Width, Height: Integer; Color: TColor);
begin
  // Dessiner un rectangle sur l'image
  Image1.Canvas.Pen.Color := Color;
  Image1.Canvas.Pen.Width := 3;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(Left, Top, Left + Width, Top + Height);

  // Optionnel : ajouter une étiquette
  Image1.Canvas.Font.Color := clWhite;
  Image1.Canvas.Brush.Color := Color;
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.TextOut(Left, Top - 20, Format('Visage %d', [Tag + 1]));

  // Incrémenter le compteur de visages
  Tag := Tag + 1;
end;
```

### 4. Détection d'objets et segmentation d'image

La détection d'objets permet d'identifier et de localiser des objets spécifiques dans une image. Voici un exemple utilisant une approche hybride :

```delphi
procedure TFormObjectDetection.DetectObjects(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
  ResultImagePath: string;
begin
  // Réinitialiser l'interface
  MemoResults.Clear;
  Image1.Picture.LoadFromFile(ImagePath);
  StatusBar1.SimpleText := 'Détection en cours...';

  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('object_detection.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Traitement des résultats
    MemoResults.Lines.Add('Objets détectés :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
    begin
      if Output[i].Trim <> '' then
        MemoResults.Lines.Add(Output[i]);
    end;

    // Charger l'image avec les annotations
    ResultImagePath := ChangeFileExt(ImagePath, '_detected.jpg');
    if FileExists(ResultImagePath) then
      Image2.Picture.LoadFromFile(ResultImagePath);

    StatusBar1.SimpleText := 'Détection terminée';
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python (object_detection.py) utilisant YOLOv5 :

```python
import sys
import torch
import cv2
import numpy as np

def detect_objects(image_path):
    # Charger le modèle YOLOv5
    model = torch.hub.load('ultralytics/yolov5', 'yolov5s')

    # Charger l'image
    image = cv2.imread(image_path)

    # Effectuer la détection
    results = model(image)

    # Convertir les résultats en pandas dataframe
    df = results.pandas().xyxy[0]

    # Créer une copie de l'image pour dessiner dessus
    output_image = image.copy()

    # Parcourir les détections et les afficher
    for i, row in df.iterrows():
        # Extraire les informations
        x1, y1, x2, y2 = int(row['xmin']), int(row['ymin']), int(row['xmax']), int(row['ymax'])
        conf = row['confidence']
        cls = row['class']
        name = row['name']

        # Dessiner le rectangle
        cv2.rectangle(output_image, (x1, y1), (x2, y2), (0, 255, 0), 2)

        # Ajouter une étiquette
        label = f"{name} {conf:.2f}"
        cv2.putText(output_image, label, (x1, y1 - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 255, 0), 2)

        # Afficher les détails en console (qui seront récupérés par Delphi)
        print(f"{name} (confiance: {conf*100:.1f}%) à la position ({x1},{y1})-({x2},{y2})")

    # Sauvegarder l'image avec les annotations
    output_path = image_path.replace('.jpg', '_detected.jpg').replace('.png', '_detected.jpg')
    cv2.imwrite(output_path, output_image)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        detect_objects(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

> 🔹 **Note** : Ce script nécessite l'installation de PyTorch, OpenCV et YOLOv5. Vous pouvez les installer avec pip : `pip install torch opencv-python ultralytics`.

## Traitement avancé d'images

### 1. Filtres et effets en temps réel

Voici comment appliquer des filtres en temps réel à une image dans Delphi :

```delphi
type
  TFilterType = (ftOriginal, ftGrayscale, ftSepia, ftBlur, ftSharpen, ftEdgeDetection);

procedure TFormImageFilters.ApplyFilter(FilterType: TFilterType);
var
  SourceBitmap, ResultBitmap: TBitmap;
  X, Y: Integer;
  SourcePixel, ResultPixel: TRGBTriple;
  GrayValue: Byte;
  Kernel: array[-1..1, -1..1] of Integer;
  KernelSum: Integer;
  TempR, TempG, TempB: Integer;
  Line, PrevLine, NextLine: PRGBTriple;
begin
  // Créer des bitmaps pour le traitement
  SourceBitmap := TBitmap.Create;
  ResultBitmap := TBitmap.Create;

  try
    // Configurer les bitmaps
    SourceBitmap.Assign(Image1.Picture.Bitmap);
    ResultBitmap.Width := SourceBitmap.Width;
    ResultBitmap.Height := SourceBitmap.Height;
    ResultBitmap.PixelFormat := pf24bit;

    // Appliquer le filtre sélectionné
    case FilterType of
      ftOriginal:
        // Pas de traitement, juste une copie
        ResultBitmap.Assign(SourceBitmap);

      ftGrayscale:
      begin
        // Convertir en niveaux de gris
        for Y := 0 to SourceBitmap.Height - 1 do
        begin
          Line := SourceBitmap.ScanLine[Y];
          for X := 0 to SourceBitmap.Width - 1 do
          begin
            // Calculer la valeur de gris (pondérée par luminosité perçue)
            GrayValue := Round(
              0.299 * Line[X].rgbtRed +
              0.587 * Line[X].rgbtGreen +
              0.114 * Line[X].rgbtBlue
            );

            // Appliquer à tous les canaux
            Line[X].rgbtRed := GrayValue;
            Line[X].rgbtGreen := GrayValue;
            Line[X].rgbtBlue := GrayValue;
          end;
        end;
        ResultBitmap.Assign(SourceBitmap);
      end;

      ftSepia:
      begin
        // Effet sépia
        for Y := 0 to SourceBitmap.Height - 1 do
        begin
          Line := SourceBitmap.ScanLine[Y];
          for X := 0 to SourceBitmap.Width - 1 do
          begin
            // Calculer la valeur de gris
            GrayValue := Round(
              0.299 * Line[X].rgbtRed +
              0.587 * Line[X].rgbtGreen +
              0.114 * Line[X].rgbtBlue
            );

            // Appliquer l'effet sépia
            TempR := Round(GrayValue * 1.07);
            if TempR > 255 then TempR := 255;

            TempG := Round(GrayValue * 0.74);
            if TempG > 255 then TempG := 255;

            TempB := Round(GrayValue * 0.43);
            if TempB > 255 then TempB := 255;

            Line[X].rgbtRed := TempR;
            Line[X].rgbtGreen := TempG;
            Line[X].rgbtBlue := TempB;
          end;
        end;
        ResultBitmap.Assign(SourceBitmap);
      end;

      ftBlur:
      begin
        // Filtre de flou (moyenne 3x3)
        for Y := 1 to SourceBitmap.Height - 2 do
        begin
          for X := 1 to SourceBitmap.Width - 2 do
          begin
            TempR := 0;
            TempG := 0;
            TempB := 0;

            // Parcourir le voisinage 3x3
            for var j := -1 to 1 do
            begin
              for var i := -1 to 1 do
              begin
                Line := SourceBitmap.ScanLine[Y + j];
                TempR := TempR + Line[X + i].rgbtRed;
                TempG := TempG + Line[X + i].rgbtGreen;
                TempB := TempB + Line[X + i].rgbtBlue;
              end;
            end;

            // Calculer la moyenne
            Line := ResultBitmap.ScanLine[Y];
            Line[X].rgbtRed := TempR div 9;
            Line[X].rgbtGreen := TempG div 9;
            Line[X].rgbtBlue := TempB div 9;
          end;
        end;
      end;

      ftSharpen:
      begin
        // Filtre d'accentuation
        // Définir le noyau (kernel)
        Kernel[0, 0] := -1; Kernel[0, 1] := -1; Kernel[0, 2] := -1;
        Kernel[1, 0] := -1; Kernel[1, 1] :=  9; Kernel[1, 2] := -1;
        Kernel[2, 0] := -1; Kernel[2, 1] := -1; Kernel[2, 2] := -1;

        // Appliquer le noyau
        for Y := 1 to SourceBitmap.Height - 2 do
        begin
          for X := 1 to SourceBitmap.Width - 2 do
          begin
            TempR := 0;
            TempG := 0;
            TempB := 0;

            // Parcourir le voisinage 3x3
            for var j := -1 to 1 do
            begin
              for var i := -1 to 1 do
              begin
                Line := SourceBitmap.ScanLine[Y + j];
                TempR := TempR + Line[X + i].rgbtRed * Kernel[j+1, i+1];
                TempG := TempG + Line[X + i].rgbtGreen * Kernel[j+1, i+1];
                TempB := TempB + Line[X + i].rgbtBlue * Kernel[j+1, i+1];
              end;
            end;

            // Limiter les valeurs entre 0 et 255
            if TempR < 0 then TempR := 0 else if TempR > 255 then TempR := 255;
            if TempG < 0 then TempG := 0 else if TempG > 255 then TempG := 255;
            if TempB < 0 then TempB := 0 else if TempB > 255 then TempB := 255;

            // Assigner les nouvelles valeurs
            Line := ResultBitmap.ScanLine[Y];
            Line[X].rgbtRed := TempR;
            Line[X].rgbtGreen := TempG;
            Line[X].rgbtBlue := TempB;
          end;
        end;
      end;

      ftEdgeDetection:
      begin
        // Filtre de détection de contours (Sobel)
        // Convertir d'abord en niveaux de gris
        for Y := 0 to SourceBitmap.Height - 1 do
        begin
          Line := SourceBitmap.ScanLine[Y];
          for X := 0 to SourceBitmap.Width - 1 do
          begin
            GrayValue := Round(
              0.299 * Line[X].rgbtRed +
              0.587 * Line[X].rgbtGreen +
              0.114 * Line[X].rgbtBlue
            );

            Line[X].rgbtRed := GrayValue;
            Line[X].rgbtGreen := GrayValue;
            Line[X].rgbtBlue := GrayValue;
          end;
        end;

        // Appliquer le filtre de Sobel
        for Y := 1 to SourceBitmap.Height - 2 do
        begin
          for X := 1 to SourceBitmap.Width - 2 do
          begin
            // Gradient horizontal
            var Gx :=
              -1 * SourceBitmap.ScanLine[Y-1][X-1].rgbtRed +
              -2 * SourceBitmap.ScanLine[Y][X-1].rgbtRed +
              -1 * SourceBitmap.ScanLine[Y+1][X-1].rgbtRed +
               1 * SourceBitmap.ScanLine[Y-1][X+1].rgbtRed +
               2 * SourceBitmap.ScanLine[Y][X+1].rgbtRed +
               1 * SourceBitmap.ScanLine[Y+1][X+1].rgbtRed;

            // Gradient vertical
            var Gy :=
              -1 * SourceBitmap.ScanLine[Y-1][X-1].rgbtRed +
              -2 * SourceBitmap.ScanLine[Y-1][X].rgbtRed +
              -1 * SourceBitmap.ScanLine[Y-1][X+1].rgbtRed +
               1 * SourceBitmap.ScanLine[Y+1][X-1].rgbtRed +
               2 * SourceBitmap.ScanLine[Y+1][X].rgbtRed +
               1 * SourceBitmap.ScanLine[Y+1][X+1].rgbtRed;

            // Magnitude du gradient
            var Magnitude := Round(Sqrt(Sqr(Gx) + Sqr(Gy)));

            // Limiter les valeurs entre 0 et 255
            if Magnitude > 255 then Magnitude := 255;

            // Assigner la valeur calculée
            ResultBitmap.ScanLine[Y][X].rgbtRed := Magnitude;
            ResultBitmap.ScanLine[Y][X].rgbtGreen := Magnitude;
            ResultBitmap.ScanLine[Y][X].rgbtBlue := Magnitude;
          end;
        end;
      end;
    end;

    // Afficher le résultat
    Image2.Picture.Assign(ResultBitmap);
  finally
    SourceBitmap.Free;
    ResultBitmap.Free;
  end;
end;
```

Ce code permet d'appliquer différents filtres à une image, notamment :
- Conversion en niveaux de gris
- Effet sépia
- Flou
- Accentuation (sharpening)
- Détection de contours (avec l'opérateur de Sobel)

### 2. Segmentation et extraction d'objets

La segmentation d'image consiste à séparer les différentes parties d'une image en régions significatives. Voici un exemple utilisant l'algorithme de seuillage :

```delphi
procedure TFormSegmentation.SegmentImage(const ImagePath: string; Threshold: Integer);
var
  SourceBitmap, ResultBitmap: TBitmap;
  X, Y: Integer;
  GrayValue: Byte;
  SourceLine: PRGBTriple;
  ResultLine: PRGBTriple;
begin
  // Charger l'image source
  SourceBitmap := TBitmap.Create;
  ResultBitmap := TBitmap.Create;
  try
    SourceBitmap.LoadFromFile(ImagePath);

    // Configurer le bitmap résultant
    ResultBitmap.Width := SourceBitmap.Width;
    ResultBitmap.Height := SourceBitmap.Height;
    ResultBitmap.PixelFormat := pf24bit;

    // Appliquer le seuillage
    for Y := 0 to SourceBitmap.Height - 1 do
    begin
      SourceLine := SourceBitmap.ScanLine[Y];
      ResultLine := ResultBitmap.ScanLine[Y];

      for X := 0 to SourceBitmap.Width - 1 do
      begin
        // Convertir en niveaux de gris
        GrayValue := Round(
          0.299 * SourceLine[X].rgbtRed +
          0.587 * SourceLine[X].rgbtGreen +
          0.114 * SourceLine[X].rgbtBlue
        );

        // Appliquer le seuillage
        if GrayValue > Threshold then
        begin
          // Pixel au-dessus du seuil (objet)
          ResultLine[X].rgbtRed := 255;
          ResultLine[X].rgbtGreen := 255;
          ResultLine[X].rgbtBlue := 255;
        end
        else
        begin
          // Pixel en dessous du seuil (fond)
          ResultLine[X].rgbtRed := 0;
          ResultLine[X].rgbtGreen := 0;
          ResultLine[X].rgbtBlue := 0;
        end;
      end;
    end;

    // Afficher le résultat
    Image2.Picture.Assign(ResultBitmap);

    // Facultatif : Identifier les objets connectés (composants connexes)
    if CheckBoxIdentifyObjects.Checked then
      IdentifyConnectedComponents(ResultBitmap);

  finally
    SourceBitmap.Free;
    ResultBitmap.Free;
  end;
end;

procedure TFormSegmentation.IdentifyConnectedComponents(const BinaryImage: TBitmap);
var
  Labels: array of array of Integer;
  CurrentLabel: Integer;
  X, Y: Integer;
  Line: PRGBTriple;
  ObjectsFound: TDictionary<Integer, TRect>;
  ObjectLabel: Integer;
  ObjectRect: TRect;
begin
  // Initialiser la matrice des étiquettes
  SetLength(Labels, BinaryImage.Height, BinaryImage.Width);
  for Y := 0 to BinaryImage.Height - 1 do
    for X := 0 to BinaryImage.Width - 1 do
      Labels[Y][X] := 0;

  // Préparer le dictionnaire pour stocker les rectangles englobants
  ObjectsFound := TDictionary<Integer, TRect>.Create;
  try
    // Étiquetage des composants connexes (algorithme simplifié)
    CurrentLabel := 0;

    for Y := 0 to BinaryImage.Height - 1 do
    begin
      Line := BinaryImage.ScanLine[Y];
      for X := 0 to BinaryImage.Width - 1 do
      begin
        // Si le pixel est blanc (objet) et n'a pas encore d'étiquette
        if (Line[X].rgbtRed = 255) and (Labels[Y][X] = 0) then
        begin
          // Nouvelle étiquette
          Inc(CurrentLabel);

          // Initialiser le rectangle englobant pour ce nouvel objet
          ObjectRect := TRect.Create(X, Y, X, Y);
          ObjectsFound.Add(CurrentLabel, ObjectRect);

          // Appliquer un algorithme de remplissage par diffusion (flood fill)
          FloodFill(BinaryImage, Labels, X, Y, CurrentLabel, ObjectsFound[CurrentLabel]);
        end;
      end;
    end;

    // Afficher les résultats
    MemoResults.Clear;
    MemoResults.Lines.Add(Format('Nombre d''objets identifiés : %d', [CurrentLabel]));
    MemoResults.Lines.Add('');

    // Dessiner les rectangles englobants sur l'image
    for ObjectLabel in ObjectsFound.Keys do
    begin
      ObjectRect := ObjectsFound[ObjectLabel];

      // Dessiner le rectangle
      BinaryImage.Canvas.Pen.Color := clRed;
      BinaryImage.Canvas.Pen.Width := 2;
      BinaryImage.Canvas.Rectangle(ObjectRect);

      // Ajouter une étiquette
      BinaryImage.Canvas.Brush.Color := clRed;
      BinaryImage.Canvas.Font.Color := clWhite;
      BinaryImage.Canvas.TextOut(ObjectRect.Left, ObjectRect.Top - 20,
                                Format('Objet %d', [ObjectLabel]));

      // Afficher les informations dans le mémo
      MemoResults.Lines.Add(Format('Objet %d :', [ObjectLabel]));
      MemoResults.Lines.Add(Format('  Position : (%d, %d) à (%d, %d)',
                            [ObjectRect.Left, ObjectRect.Top, ObjectRect.Right, ObjectRect.Bottom]));
      MemoResults.Lines.Add(Format('  Dimensions : %d x %d pixels',
                            [ObjectRect.Width, ObjectRect.Height]));
      MemoResults.Lines.Add('');
    end;

    // Mettre à jour l'affichage
    Image2.Picture.Assign(BinaryImage);
  finally
    ObjectsFound.Free;
  end;
end;

procedure TFormSegmentation.FloodFill(const Image: TBitmap; var Labels: array of array of Integer;
                                  StartX, StartY, Label: Integer; var ObjectRect: TRect);
var
  Stack: TStack<TPoint>;
  Current, Neighbor: TPoint;
  Directions: array[0..3] of TPoint;
  i: Integer;
  Line: PRGBTriple;
begin
  // Initialiser la pile
  Stack := TStack<TPoint>.Create;
  try
    // Définir les directions (4-connexité : haut, droite, bas, gauche)
    Directions[0] := TPoint.Create(0, -1);  // Haut
    Directions[1] := TPoint.Create(1, 0);   // Droite
    Directions[2] := TPoint.Create(0, 1);   // Bas
    Directions[3] := TPoint.Create(-1, 0);  // Gauche

    // Commencer par le pixel de départ
    Stack.Push(TPoint.Create(StartX, StartY));
    Labels[StartY][StartX] := Label;

    // Mettre à jour le rectangle englobant
    UpdateBoundingRect(ObjectRect, StartX, StartY);

    // Traiter tant que la pile n'est pas vide
    while Stack.Count > 0 do
    begin
      Current := Stack.Pop;

      // Explorer les voisins
      for i := 0 to 3 do
      begin
        Neighbor.X := Current.X + Directions[i].X;
        Neighbor.Y := Current.Y + Directions[i].Y;

        // Vérifier que le voisin est dans les limites de l'image
        if (Neighbor.X >= 0) and (Neighbor.X < Image.Width) and
           (Neighbor.Y >= 0) and (Neighbor.Y < Image.Height) then
        begin
          // Vérifier si le voisin est un pixel d'objet non étiqueté
          Line := Image.ScanLine[Neighbor.Y];
          if (Line[Neighbor.X].rgbtRed = 255) and (Labels[Neighbor.Y][Neighbor.X] = 0) then
          begin
            // Étiqueter ce pixel
            Labels[Neighbor.Y][Neighbor.X] := Label;

            // Mettre à jour le rectangle englobant
            UpdateBoundingRect(ObjectRect, Neighbor.X, Neighbor.Y);

            // Ajouter à la pile pour explorer ses voisins
            Stack.Push(Neighbor);
          end;
        end;
      end;
    end;
  finally
    Stack.Free;
  end;
end;

procedure TFormSegmentation.UpdateBoundingRect(var Rect: TRect; X, Y: Integer);
begin
  // Mettre à jour les limites du rectangle
  if X < Rect.Left then Rect.Left := X;
  if X > Rect.Right then Rect.Right := X;
  if Y < Rect.Top then Rect.Top := Y;
  if Y > Rect.Bottom then Rect.Bottom := Y;
end;
```

Cette implémentation permet de :
1. Convertir l'image en noir et blanc avec un seuil spécifié
2. Identifier les objets distincts dans l'image binaire
3. Calculer un rectangle englobant pour chaque objet
4. Afficher les statistiques des objets trouvés

### 3. Reconnaissance de formes géométriques

Voici un exemple de code pour détecter des formes géométriques simples dans une image :

```delphi
procedure TFormShapeDetection.DetectShapes(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
  ResultImagePath: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('shape_detection.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage des résultats
    MemoResults.Clear;
    MemoResults.Lines.Add('Formes détectées :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
    begin
      if Output[i].Trim <> '' then
        MemoResults.Lines.Add(Output[i]);
    end;

    // Charger l'image avec les annotations
    ResultImagePath := ChangeFileExt(ImagePath, '_shapes.jpg');
    if FileExists(ResultImagePath) then
      Image2.Picture.LoadFromFile(ResultImagePath);
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python (shape_detection.py) :

```python
import sys
import cv2
import numpy as np

def detect_shapes(image_path):
    # Charger l'image
    image = cv2.imread(image_path)
    original = image.copy()

    # Convertir en niveaux de gris
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Appliquer un flou gaussien
    blurred = cv2.GaussianBlur(gray, (5, 5), 0)

    # Détecter les contours avec Canny
    edges = cv2.Canny(blurred, 50, 150)

    # Trouver les contours
    contours, _ = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    # Analyser chaque contour pour déterminer la forme
    for i, contour in enumerate(contours):
        # Ignorer les petits contours
        if cv2.contourArea(contour) < 100:
            continue

        # Approximer le contour par un polygone
        epsilon = 0.04 * cv2.arcLength(contour, True)
        approx = cv2.approxPolyDP(contour, epsilon, True)

        # Déterminer la forme en fonction du nombre de sommets
        vertices = len(approx)

        # Calculer les caractéristiques du contour
        x, y, w, h = cv2.boundingRect(contour)
        aspect_ratio = float(w) / h

        # Déterminer la forme
        shape = "Indéterminée"

        if vertices == 3:
            shape = "Triangle"
        elif vertices == 4:
            # Distinguer carré et rectangle avec le ratio largeur/hauteur
            if 0.95 <= aspect_ratio <= 1.05:
                shape = "Carré"
            else:
                shape = "Rectangle"
        elif vertices == 5:
            shape = "Pentagone"
        elif vertices == 6:
            shape = "Hexagone"
        elif vertices >= 10:
            # Détecter les cercles
            area = cv2.contourArea(contour)
            perimeter = cv2.arcLength(contour, True)
            circularity = 4 * np.pi * area / (perimeter * perimeter)

            if circularity > 0.8:
                shape = "Cercle"

        # Dessiner le contour
        cv2.drawContours(image, [approx], 0, (0, 255, 0), 2)

        # Ajouter l'étiquette
        cv2.putText(image, shape, (x, y - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (0, 0, 255), 2)

        # Afficher les détails en console
        print(f"{shape} à la position ({x}, {y}), dimensions: {w}x{h} pixels")

    # Sauvegarder l'image avec les annotations
    output_path = image_path.replace('.jpg', '_shapes.jpg').replace('.png', '_shapes.jpg')
    cv2.imwrite(output_path, image)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        detect_shapes(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

### 4. Alignement et recadrage automatique de documents

Cet exemple montre comment détecter automatiquement un document dans une image et le recadrer :

```delphi
procedure TFormDocumentScanner.ScanDocument(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
  ScannedDocumentPath: string;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('document_scanner.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Traitement des résultats
    StatusBar1.SimpleText := 'Traitement terminé';

    // Vérifier si le traitement a réussi
    if (Output.Count > 0) and (Output[0] = 'OK') then
    begin
      // Charger l'image du document scanné
      ScannedDocumentPath := ChangeFileExt(ImagePath, '_scanned.jpg');
      if FileExists(ScannedDocumentPath) then
      begin
        Image2.Picture.LoadFromFile(ScannedDocumentPath);

        // Ajouter l'option de sauvegarder au format PDF
        ButtonSavePDF.Enabled := True;
      end;
    end
    else
    begin
      ShowMessage('Erreur lors du traitement : ' + Output.Text);
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;

procedure TFormDocumentScanner.ButtonSavePDFClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  ScannedDocumentPath: string;
  Process: TProcess;
begin
  // Vérifier que nous avons une image traitée
  ScannedDocumentPath := ChangeFileExt(FImagePath, '_scanned.jpg');
  if not FileExists(ScannedDocumentPath) then
  begin
    ShowMessage('Aucun document scanné disponible.');
    Exit;
  end;

  // Demander où sauvegarder le PDF
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';
    SaveDialog.DefaultExt := 'pdf';

    if SaveDialog.Execute then
    begin
      // Utiliser Python pour convertir l'image en PDF
      Process := TProcess.Create(nil);
      try
        Process.Executable := 'python';
        Process.Parameters.Add('image_to_pdf.py');
        Process.Parameters.Add(ScannedDocumentPath);
        Process.Parameters.Add(SaveDialog.FileName);
        Process.Options := Process.Options + [poWaitOnExit];

        Process.Execute;

        if Process.ExitCode = 0 then
          ShowMessage('Document sauvegardé avec succès en PDF.')
        else
          ShowMessage('Erreur lors de la conversion en PDF.');
      finally
        Process.Free;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;
```

Le script Python (document_scanner.py) :

```python
import sys
import cv2
import numpy as np

def scan_document(image_path):
    # Charger l'image
    image = cv2.imread(image_path)
    original = image.copy()

    # Convertir en niveaux de gris
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Appliquer un flou gaussien
    blurred = cv2.GaussianBlur(gray, (5, 5), 0)

    # Détecter les contours
    edges = cv2.Canny(blurred, 75, 200)

    # Trouver les contours
    contours, _ = cv2.findContours(edges, cv2.RETR_LIST, cv2.CHAIN_APPROX_SIMPLE)

    # Trier les contours par aire (du plus grand au plus petit)
    contours = sorted(contours, key=cv2.contourArea, reverse=True)

    # Initialiser le contour du document
    document_contour = None

    # Parcourir les contours pour trouver celui qui ressemble à un document
    for contour in contours:
        # Approximer le contour
        perimeter = cv2.arcLength(contour, True)
        approx = cv2.approxPolyDP(contour, 0.02 * perimeter, True)

        # Si le contour a 4 sommets, on considère que c'est notre document
        if len(approx) == 4:
            document_contour = approx
            break

    # Si aucun contour de document n'est trouvé
    if document_contour is None:
        print("Aucun document détecté")
        return

    # Dessiner le contour sur l'image originale
    cv2.drawContours(image, [document_contour], -1, (0, 255, 0), 2)

    # Sauvegarder l'image avec le contour
    contour_path = image_path.replace('.jpg', '_contour.jpg').replace('.png', '_contour.jpg')
    cv2.imwrite(contour_path, image)

    # Réordonner les points pour qu'ils soient dans l'ordre [haut-gauche, haut-droite, bas-droite, bas-gauche]
    # Cette étape est nécessaire pour la transformation de perspective
    document_contour = document_contour.reshape(4, 2)
    rect = np.zeros((4, 2), dtype="float32")

    # Calculer la somme et la différence des coordonnées
    s = document_contour.sum(axis=1)
    rect[0] = document_contour[np.argmin(s)]  # Haut-gauche
    rect[2] = document_contour[np.argmax(s)]  # Bas-droite

    diff = np.diff(document_contour, axis=1)
    rect[1] = document_contour[np.argmin(diff)]  # Haut-droite
    rect[3] = document_contour[np.argmax(diff)]  # Bas-gauche

    # Calculer les dimensions du document
    width_a = np.sqrt(((rect[2][0] - rect[3][0]) ** 2) + ((rect[2][1] - rect[3][1]) ** 2))
    width_b = np.sqrt(((rect[1][0] - rect[0][0]) ** 2) + ((rect[1][1] - rect[0][1]) ** 2))
    max_width = max(int(width_a), int(width_b))

    height_a = np.sqrt(((rect[1][0] - rect[2][0]) ** 2) + ((rect[1][1] - rect[2][1]) ** 2))
    height_b = np.sqrt(((rect[0][0] - rect[3][0]) ** 2) + ((rect[0][1] - rect[3][1]) ** 2))
    max_height = max(int(height_a), int(height_b))

    # Coordonnées de destination pour la transformation de perspective
    dst = np.array([
        [0, 0],
        [max_width - 1, 0],
        [max_width - 1, max_height - 1],
        [0, max_height - 1]
    ], dtype="float32")

    # Calculer la matrice de transformation
    M = cv2.getPerspectiveTransform(rect, dst)

    # Appliquer la transformation
    warped = cv2.warpPerspective(original, M, (max_width, max_height))

    # Convertir en niveaux de gris
    warped_gray = cv2.cvtColor(warped, cv2.COLOR_BGR2GRAY)

    # Appliquer un seuillage adaptatif pour améliorer la lisibilité
    T = cv2.adaptiveThreshold(warped_gray, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 21, 10)

    # Sauvegarder le document scanné
    scanned_path = image_path.replace('.jpg', '_scanned.jpg').replace('.png', '_scanned.jpg')
    cv2.imwrite(scanned_path, T)

    # Indiquer que tout s'est bien passé
    print("OK")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        scan_document(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

Et le script pour convertir l'image en PDF (image_to_pdf.py) :

```python
import sys
from PIL import Image
from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter, A4

def convert_to_pdf(image_path, pdf_path):
    # Ouvrir l'image
    img = Image.open(image_path)

    # Déterminer le format de page en fonction des dimensions de l'image
    width, height = img.size
    if width > height:
        pagesize = letter
    else:
        pagesize = A4

    # Créer un nouveau document PDF
    c = canvas.Canvas(pdf_path, pagesize=pagesize)

    # Calculer les dimensions pour adapter l'image à la page
    page_width, page_height = pagesize

    # Adapter l'image à la page tout en conservant les proportions
    ratio = min(page_width / width, page_height / height)
    new_width = width * ratio * 0.9  # 90% de la largeur de la page
    new_height = height * ratio * 0.9  # 90% de la hauteur de la page

    # Calculer les marges pour centrer l'image
    x = (page_width - new_width) / 2
    y = (page_height - new_height) / 2

    # Ajouter l'image au PDF
    c.drawImage(image_path, x, y, width=new_width, height=new_height)

    # Finaliser le document
    c.save()

    return True

if __name__ == "__main__":
    if len(sys.argv) > 2:
        success = convert_to_pdf(sys.argv[1], sys.argv[2])
        sys.exit(0 if success else 1)
    else:
        print("Erreur: Paramètres manquants")
        print("Usage: python image_to_pdf.py <image_path> <pdf_path>")
        sys.exit(1)
```

## Intégration avec les modèles de Deep Learning

Pour des tâches plus avancées, vous pouvez utiliser des modèles de deep learning pré-entraînés :

```delphi
procedure TFormDeepLearning.ClassifyImage(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('image_classifier.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage des résultats
    MemoResults.Clear;
    MemoResults.Lines.Add('Classification avec un modèle neural :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
    begin
      if Output[i].Trim <> '' then
        MemoResults.Lines.Add(Output[i]);
    end;
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

## Intégration avec les modèles de Deep Learning

Le script Python (image_classifier.py) utilisant un modèle ResNet pré-entraîné :

```python
import sys
import torch
from torchvision import models, transforms
from PIL import Image

def classify_image(image_path):
    # Charger un modèle pré-entraîné
    model = models.resnet50(pretrained=True)
    model.eval()

    # Préparation de l'image
    transform = transforms.Compose([
        transforms.Resize(256),
        transforms.CenterCrop(224),
        transforms.ToTensor(),
        transforms.Normalize(
            mean=[0.485, 0.456, 0.406],
            std=[0.229, 0.224, 0.225]
        )
    ])

    # Ouvrir et transformer l'image
    img = Image.open(image_path)
    img_t = transform(img)
    batch_t = torch.unsqueeze(img_t, 0)

    # Prédiction
    with torch.no_grad():
        output = model(batch_t)

    # Charger les classes ImageNet
    with open('imagenet_classes.txt') as f:
        classes = [line.strip() for line in f.readlines()]

    # Obtenir les 5 meilleures prédictions
    _, indices = torch.sort(output, descending=True)
    percentages = torch.nn.functional.softmax(output, dim=1)[0] * 100

    # Afficher les résultats
    for idx in indices[0][:5]:
        print(f"{classes[idx]}: {percentages[idx].item():.2f}%")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        classify_image(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

> 🔹 **Note** : Ce script nécessite PyTorch et le fichier `imagenet_classes.txt` contenant les noms des 1000 classes ImageNet.

### Détection d'objets avec YOLO

YOLO (You Only Look Once) est l'un des modèles de détection d'objets les plus performants et rapides :

```delphi
procedure TFormObjectDetection.DetectObjectsWithYOLO(const ImagePath: string);
var
  Process: TProcess;
  Output: TStringList;
begin
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('yolo_detection.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Afficher un indicateur de chargement
    StatusBar1.SimpleText := 'Détection en cours (peut prendre quelques secondes)...';
    Application.ProcessMessages;

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Affichage des résultats
    MemoResults.Clear;
    MemoResults.Lines.Add('Détection avec YOLO :');
    MemoResults.Lines.Add('');

    for var i := 0 to Output.Count - 1 do
    begin
      if Output[i].Trim <> '' then
        MemoResults.Lines.Add(Output[i]);
    end;

    // Charger l'image avec les annotations
    var ResultImagePath := ChangeFileExt(ImagePath, '_detected.jpg');
    if FileExists(ResultImagePath) then
      Image2.Picture.LoadFromFile(ResultImagePath);

    StatusBar1.SimpleText := 'Détection terminée';
  finally
    Process.Free;
    Output.Free;
  end;
end;
```

Le script Python pour la détection avec YOLO (yolo_detection.py) :

```python
import sys
import cv2
import numpy as np

def detect_with_yolo(image_path):
    # Charger l'image
    image = cv2.imread(image_path)
    height, width = image.shape[:2]

    # Charger le modèle YOLO
    net = cv2.dnn.readNetFromDarknet('yolov3.cfg', 'yolov3.weights')

    # Charger les noms des classes
    with open('coco.names', 'r') as f:
        classes = [line.strip() for line in f.readlines()]

    # Configurer le modèle
    layer_names = net.getLayerNames()
    output_layers = [layer_names[i - 1] for i in net.getUnconnectedOutLayers()]

    # Préparer l'image pour le réseau de neurones
    blob = cv2.dnn.blobFromImage(image, 1/255.0, (416, 416), swapRB=True, crop=False)
    net.setInput(blob)

    # Obtenir les prédictions
    outputs = net.forward(output_layers)

    # Préparer les listes pour stocker les informations
    class_ids = []
    confidences = []
    boxes = []

    # Parcourir toutes les détections
    for output in outputs:
        for detection in output:
            scores = detection[5:]
            class_id = np.argmax(scores)
            confidence = scores[class_id]

            # Filtrer les détections avec une confiance suffisante
            if confidence > 0.5:
                # Coordonnées du centre et dimensions
                center_x = int(detection[0] * width)
                center_y = int(detection[1] * height)
                w = int(detection[2] * width)
                h = int(detection[3] * height)

                # Coordonnées du rectangle
                x = int(center_x - w / 2)
                y = int(center_y - h / 2)

                boxes.append([x, y, w, h])
                confidences.append(float(confidence))
                class_ids.append(class_id)

    # Appliquer la suppression des non-maximums pour éviter les détections multiples
    indices = cv2.dnn.NMSBoxes(boxes, confidences, 0.5, 0.4)

    # Dessiner les rectangles et étiquettes
    colors = np.random.uniform(0, 255, size=(len(classes), 3))

    # Préparer la liste des objets détectés pour les retourner à Delphi
    detected_objects = []

    if len(indices) > 0:
        for i in indices.flatten():
            x, y, w, h = boxes[i]
            label = str(classes[class_ids[i]])
            confidence = confidences[i]
            color = colors[class_ids[i]]

            # S'assurer que les coordonnées sont dans les limites de l'image
            x = max(0, x)
            y = max(0, y)

            # Dessiner le rectangle
            cv2.rectangle(image, (x, y), (x + w, y + h), color, 2)

            # Ajouter l'étiquette
            cv2.putText(image, f"{label} {confidence:.2f}", (x, y - 10),
                        cv2.FONT_HERSHEY_SIMPLEX, 0.5, color, 2)

            # Ajouter à la liste des objets détectés
            detected_objects.append(f"{label} (confiance: {confidence*100:.1f}%) à la position ({x},{y})-({x+w},{y+h})")

    # Sauvegarder l'image avec les annotations
    output_path = image_path.replace('.jpg', '_detected.jpg').replace('.png', '_detected.jpg')
    cv2.imwrite(output_path, image)

    # Retourner la liste des objets détectés
    for obj in detected_objects:
        print(obj)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        detect_with_yolo(sys.argv[1])
    else:
        print("Erreur: Chemin d'image non spécifié")
```

> ⚠️ **Note** : Ce script nécessite les fichiers `yolov3.cfg`, `yolov3.weights` et `coco.names` qui peuvent être téléchargés depuis le dépôt GitHub de YOLO.

## Application pratique : Système de contrôle qualité visuel

Voici un exemple d'application complète pour le contrôle qualité visuel dans un environnement industriel :

```delphi
type
  TDefectType = (dtScratch, dtDent, dtDiscoloration, dtMissing, dtOther);

  TDefect = record
    DefectType: TDefectType;
    Position: TRect;
    Confidence: Double;
  end;

procedure TFormQualityControl.InspectProduct(const ImagePath: string);
var
  Defects: TList<TDefect>;
  TotalDefects: Integer;
  DefectStats: array[TDefectType] of Integer;
  IsAcceptable: Boolean;
begin
  // Réinitialiser l'interface
  Image1.Picture.LoadFromFile(ImagePath);
  MemoResults.Clear;
  ChartDefects.Series[0].Clear;

  // Détecter les défauts
  Defects := DetectDefects(ImagePath);
  try
    // Initialiser les statistiques
    for var DefectType := Low(TDefectType) to High(TDefectType) do
      DefectStats[DefectType] := 0;

    // Analyser les défauts détectés
    TotalDefects := Defects.Count;

    // Compter les types de défauts
    for var Defect in Defects do
    begin
      // Incrémenter le compteur pour ce type de défaut
      Inc(DefectStats[Defect.DefectType]);

      // Dessiner un rectangle autour du défaut
      DrawDefectOnImage(Defect);
    end;

    // Déterminer si le produit est acceptable (exemple : max 2 défauts mineurs)
    IsAcceptable := (TotalDefects <= 2) and (DefectStats[dtScratch] + DefectStats[dtDent] <= 1);

    // Afficher les résultats
    MemoResults.Lines.Add('Rapport de contrôle qualité :');
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add(Format('Nombre total de défauts : %d', [TotalDefects]));
    MemoResults.Lines.Add('');
    MemoResults.Lines.Add('Types de défauts :');
    MemoResults.Lines.Add(Format('- Rayures : %d', [DefectStats[dtScratch]]));
    MemoResults.Lines.Add(Format('- Bosses : %d', [DefectStats[dtDent]]));
    MemoResults.Lines.Add(Format('- Décolorations : %d', [DefectStats[dtDiscoloration]]));
    MemoResults.Lines.Add(Format('- Pièces manquantes : %d', [DefectStats[dtMissing]]));
    MemoResults.Lines.Add(Format('- Autres défauts : %d', [DefectStats[dtOther]]));
    MemoResults.Lines.Add('');

    // Verdict final
    if IsAcceptable then
    begin
      MemoResults.Lines.Add('VERDICT : PRODUIT ACCEPTABLE');
      StatusBar1.SimpleText := 'Produit acceptable';
      LabelVerdict.Caption := 'ACCEPTÉ';
      LabelVerdict.Font.Color := clGreen;
    end
    else
    begin
      MemoResults.Lines.Add('VERDICT : PRODUIT DÉFECTUEUX');
      StatusBar1.SimpleText := 'Produit défectueux';
      LabelVerdict.Caption := 'REJETÉ';
      LabelVerdict.Font.Color := clRed;
    end;

    // Mettre à jour le graphique
    for var DefectType := Low(TDefectType) to High(TDefectType) do
    begin
      case DefectType of
        dtScratch: ChartDefects.Series[0].Add(DefectStats[DefectType], 'Rayures');
        dtDent: ChartDefects.Series[0].Add(DefectStats[DefectType], 'Bosses');
        dtDiscoloration: ChartDefects.Series[0].Add(DefectStats[DefectType], 'Décolorations');
        dtMissing: ChartDefects.Series[0].Add(DefectStats[DefectType], 'Manquants');
        dtOther: ChartDefects.Series[0].Add(DefectStats[DefectType], 'Autres');
      end;
    end;
  finally
    Defects.Free;
  end;
end;

function TFormQualityControl.DetectDefects(const ImagePath: string): TList<TDefect>;
var
  Process: TProcess;
  Output: TStringList;
  Defects: TList<TDefect>;
  Line, TypeStr: string;
  X, Y, W, H: Integer;
  Confidence: Double;
  DefectType: TDefectType;
begin
  Defects := TList<TDefect>.Create;
  Process := TProcess.Create(nil);
  Output := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('defect_detection.py');
    Process.Parameters.Add(ImagePath);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture des résultats
    Output.LoadFromStream(Process.Output);

    // Parser les résultats (format attendu : "Type,X,Y,Width,Height,Confidence")
    for Line in Output do
    begin
      if Line.Trim = '' then Continue;

      var Parts := Line.Split([',']);
      if Length(Parts) = 6 then
      begin
        TypeStr := Parts[0];

        // Convertir le type de défaut
        if TypeStr = 'scratch' then DefectType := dtScratch
        else if TypeStr = 'dent' then DefectType := dtDent
        else if TypeStr = 'discoloration' then DefectType := dtDiscoloration
        else if TypeStr = 'missing' then DefectType := dtMissing
        else DefectType := dtOther;

        // Convertir les coordonnées et la confiance
        if TryStrToInt(Parts[1], X) and
           TryStrToInt(Parts[2], Y) and
           TryStrToInt(Parts[3], W) and
           TryStrToInt(Parts[4], H) and
           TryStrToFloat(Parts[5], Confidence) then
        begin
          var Defect: TDefect;
          Defect.DefectType := DefectType;
          Defect.Position := TRect.Create(X, Y, X + W, Y + H);
          Defect.Confidence := Confidence;

          Defects.Add(Defect);
        end;
      end;
    end;
  finally
    Process.Free;
    Output.Free;
  end;

  Result := Defects;
end;

procedure TFormQualityControl.DrawDefectOnImage(const Defect: TDefect);
var
  Color: TColor;
  Label1: string;
begin
  // Choisir la couleur en fonction du type de défaut
  case Defect.DefectType of
    dtScratch: Color := clRed;
    dtDent: Color := clBlue;
    dtDiscoloration: Color := clPurple;
    dtMissing: Color := clYellow;
    dtOther: Color := clGray;
  end;

  // Choisir l'étiquette
  case Defect.DefectType of
    dtScratch: Label1 := 'Rayure';
    dtDent: Label1 := 'Bosse';
    dtDiscoloration: Label1 := 'Décol.';
    dtMissing: Label1 := 'Manquant';
    dtOther: Label1 := 'Défaut';
  end;

  // Dessiner le rectangle
  Image1.Canvas.Pen.Color := Color;
  Image1.Canvas.Pen.Width := 2;
  Image1.Canvas.Brush.Style := bsClear;
  Image1.Canvas.Rectangle(Defect.Position);

  // Ajouter l'étiquette
  Image1.Canvas.Brush.Color := Color;
  Image1.Canvas.Brush.Style := bsSolid;
  Image1.Canvas.Font.Color := clWhite;
  Image1.Canvas.TextOut(
    Defect.Position.Left,
    Defect.Position.Top - 20,
    Format('%s (%.1f%%)', [Label1, Defect.Confidence * 100])
  );
end;
```

## Conseils pour l'implémentation efficace

### 1. Performances et optimisation

Pour des applications de traitement d'image en temps réel, il est important d'optimiser les performances :

```delphi
// Utiliser TThread pour le traitement d'image en arrière-plan
procedure TFormImageProcessing.ProcessImageAsync(const ImagePath: string);
begin
  // Désactiver les contrôles d'interface utilisateur
  ButtonProcess.Enabled := False;
  StatusBar1.SimpleText := 'Traitement en cours...';

  // Lancer le traitement dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    var
      ProcessedImage: TBitmap;
    begin
      try
        // Effectuer le traitement d'image (opération potentiellement longue)
        ProcessedImage := ProcessImage(ImagePath);

        // Mettre à jour l'UI dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            // Afficher l'image traitée
            Image2.Picture.Assign(ProcessedImage);

            // Mettre à jour l'interface
            StatusBar1.SimpleText := 'Traitement terminé';
            ButtonProcess.Enabled := True;
          end
        );
      finally
        ProcessedImage.Free;
      end;
    end
  ).Start;
end;

// Pour les opérations de filtrage intensives, utiliser un traitement multi-threads
procedure TFormImageProcessing.ApplyComplexFilterMT(var Bitmap: TBitmap; FilterType: TFilterType);
var
  ThreadCount: Integer;
  TaskArray: array of ITask;
  LinesPerThread: Integer;
  StartLine, EndLine: Integer;
begin
  // Déterminer le nombre de threads à utiliser
  ThreadCount := TThread.ProcessorCount;
  SetLength(TaskArray, ThreadCount);

  // Calculer combien de lignes chaque thread doit traiter
  LinesPerThread := Bitmap.Height div ThreadCount;

  // Créer et lancer les tâches
  for var i := 0 to ThreadCount - 1 do
  begin
    StartLine := i * LinesPerThread;

    if i = ThreadCount - 1 then
      EndLine := Bitmap.Height - 1
    else
      EndLine := StartLine + LinesPerThread - 1;

    // Capturer les variables locales
    var LocalStartLine := StartLine;
    var LocalEndLine := EndLine;

    // Créer une tâche qui traite une partie de l'image
    TaskArray[i] := TTask.Create(
      procedure
      begin
        // Appliquer le filtre sur la plage de lignes assignée
        ProcessImageRegion(Bitmap, FilterType, LocalStartLine, LocalEndLine);
      end
    );

    // Démarrer la tâche
    TaskArray[i].Start;
  end;

  // Attendre que toutes les tâches soient terminées
  TTask.WaitForAll(TaskArray);
end;
```

### 2. Gestion de la mémoire

Le traitement d'images peut consommer beaucoup de mémoire, donc une bonne gestion est essentielle :

```delphi
procedure TFormImageProcessing.EnsureBitmapReleased(var Bitmap: TBitmap);
begin
  if Assigned(Bitmap) then
  begin
    Bitmap.Free;
    Bitmap := nil;
  end;
end;

function TFormImageProcessing.CreateOptimizedBitmap(Width, Height: Integer): TBitmap;
begin
  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;  // Format optimal pour le traitement
  Result.Width := Width;
  Result.Height := Height;
  Result.AlphaFormat := afIgnored;  // Ignorer le canal alpha pour plus de performances
end;

// Exemple d'utilisation dans une méthode de traitement d'image
function TFormImageProcessing.ProcessImage(const ImagePath: string): TBitmap;
var
  SourceBitmap: TBitmap;
begin
  SourceBitmap := TBitmap.Create;
  try
    // Charger l'image
    SourceBitmap.LoadFromFile(ImagePath);

    // Créer un bitmap optimisé pour le résultat
    Result := CreateOptimizedBitmap(SourceBitmap.Width, SourceBitmap.Height);

    // Effectuer le traitement
    // ...
  finally
    SourceBitmap.Free;  // Libérer la mémoire dès que possible
  end;
end;
```

### 3. Choix technique selon le contexte

Selon vos besoins, différentes approches sont préférables :

1. **Pour les applications simples ou les prototypes rapides** :
   - Utilisez les APIs cloud comme Google Vision ou Azure Cognitive Services
   - Avantages : mise en œuvre rapide, pas besoin d'installer des bibliothèques
   - Inconvénients : nécessite une connexion internet, peut avoir des coûts

2. **Pour les applications autonomes ou avec des contraintes de confidentialité** :
   - Utilisez OpenCV via Python ou des DLLs natives
   - Avantages : fonctionne hors ligne, meilleure confidentialité
   - Inconvénients : installation et configuration plus complexes

3. **Pour les opérations de base ou temps réel** :
   - Implémentez les algorithmes simples directement en Delphi
   - Avantages : performances optimales, contrôle total
   - Inconvénients : implémentation plus laborieuse pour les algorithmes complexes

## Conclusion

La reconnaissance d'images et le traitement visuel représentent un domaine puissant qui peut considérablement enrichir vos applications Delphi. Les exemples présentés dans ce chapitre vous ont montré comment :

1. Utiliser les services cloud de vision par ordinateur pour une intégration rapide
2. Mettre en œuvre des algorithmes de traitement d'image directement en Delphi
3. Exploiter la puissance de Python et de ses bibliothèques spécialisées
4. Intégrer des modèles de deep learning pour des tâches avancées

En fonction de vos besoins spécifiques, vous pouvez choisir l'approche la plus appropriée ou combiner différentes techniques pour créer des applications visuellement intelligentes.

Les possibilités d'application sont vastes : contrôle qualité industriel, sécurité, analyse de documents, applications médicales, et bien plus encore. La vision par ordinateur continue d'évoluer rapidement, offrant de nouvelles opportunités pour les développeurs Delphi qui souhaitent intégrer ces technologies dans leurs applications.

---

> **Remarque** : Les exemples de code présentés dans ce chapitre sont conçus pour être didactiques et accessibles aux débutants. Dans un contexte de production, certaines optimisations supplémentaires pourraient être nécessaires en fonction des exigences spécifiques de votre application.

⏭️ [Développement de modèles prédictifs](22-intelligence-artificielle-et-machine-learning-avec-delphi/05-developpement-de-modeles-predictifs.md)
