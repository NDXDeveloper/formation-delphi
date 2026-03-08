🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.7 Projets d'intelligence artificielle et machine learning

## Introduction

Bienvenue dans le monde fascinant de l'Intelligence Artificielle (IA) et du Machine Learning (ML) ! Dans ce chapitre, vous allez découvrir comment intégrer des fonctionnalités d'IA dans vos applications Delphi pour créer des solutions intelligentes et innovantes.

### Qu'est-ce que l'Intelligence Artificielle ?

L'**Intelligence Artificielle** est la capacité d'une machine à imiter l'intelligence humaine : apprendre, raisonner, résoudre des problèmes, comprendre le langage, reconnaître des images, etc.

**Analogie simple** : Imaginez que vous montrez des milliers de photos de chats à un enfant. Après un certain temps, l'enfant peut reconnaître un chat même dans une photo qu'il n'a jamais vue. C'est exactement ce que fait l'IA : elle apprend à partir d'exemples !

#### Types d'IA que nous allons explorer

🤖 **NLP** (Natural Language Processing) : Comprendre et générer du texte
- Chatbots intelligents
- Traduction automatique
- Analyse de sentiment
- Résumés automatiques

👁️ **Computer Vision** : Voir et comprendre les images
- Reconnaissance d'objets
- Détection de visages
- Classification d'images
- OCR (reconnaissance de texte)

📊 **Machine Learning** : Prédire et classifier
- Prédictions de ventes
- Détection d'anomalies
- Recommandations personnalisées
- Classification de données

### Pourquoi intégrer l'IA dans vos applications ?

**Avantages** :
✅ **Automatisation** : Tâches complexes effectuées automatiquement  
✅ **Personnalisation** : Expériences adaptées à chaque utilisateur  
✅ **Insights** : Découvrir des patterns cachés dans les données  
✅ **Efficacité** : Traiter des volumes énormes rapidement  
✅ **Innovation** : Créer des fonctionnalités impossibles avant

**Exemples concrets** :
- 📧 **Email** : Filtrage du spam automatique
- 📸 **Photos** : Reconnaissance de visages, recherche par contenu
- 🛒 **E-commerce** : Recommandations personnalisées
- 🚗 **GPS** : Prédiction du trafic, itinéraires optimaux
- 🎵 **Musique** : Suggestions basées sur vos goûts

### Objectifs de ce chapitre

À la fin de ce tutoriel, vous serez capable de :

✅ Comprendre les concepts de base de l'IA/ML  
✅ Intégrer des APIs d'IA (OpenAI, Google AI, etc.)  
✅ Créer un chatbot intelligent  
✅ Analyser des images avec Computer Vision  
✅ Implémenter des prédictions ML  
✅ Traiter du texte (NLP)  
✅ Utiliser des modèles pré-entraînés  
✅ Déployer vos applications IA

### Prérequis

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Compréhension des API REST
- ✅ Notions de JSON
- ✅ Bases de statistiques (utile mais pas obligatoire)

**Outils nécessaires** :
- ✅ Delphi 13 Florence
- ✅ Compte OpenAI (pour GPT)
- ✅ Compte Google Cloud (pour Vision API)
- ✅ Postman (pour tester les APIs)

### Durée estimée

**15 à 25 heures** de travail, réparties ainsi :
- Compréhension des concepts IA/ML : 3-4 heures
- Configuration des APIs : 2-3 heures
- Développement chatbot : 4-6 heures
- Computer Vision : 3-4 heures
- Machine Learning pratique : 3-5 heures
- Intégration et tests : 2-3 heures

---

## Partie 1 : Comprendre l'IA et le ML

### 1.1 Intelligence Artificielle vs Machine Learning

```
┌───────────────────────────────────────┐
│   Intelligence Artificielle (IA)      │
│   Machines qui imitent l'intelligence │
│                                       │
│  ┌─────────────────────────────────┐  │
│  │   Machine Learning (ML)         │  │
│  │   Apprendre à partir de données │  │
│  │                                 │  │
│  │  ┌───────────────────────────┐  │  │
│  │  │   Deep Learning (DL)      │  │  │
│  │  │   Réseaux de neurones     │  │  │
│  │  └───────────────────────────┘  │  │
│  └─────────────────────────────────┘  │
└───────────────────────────────────────┘
```

**IA** : Le concept général (machines intelligentes)  
**ML** : Sous-ensemble de l'IA (apprentissage automatique)  
**DL** : Sous-ensemble du ML (réseaux de neurones profonds)  

### 1.2 Comment fonctionne le Machine Learning ?

**Processus d'apprentissage** :

```
[Données d'entraînement] → [Algorithme ML] → [Modèle entraîné]
                                                     ↓
[Nouvelles données] ──────────────────→ [Prédiction/Classification]
```

**Exemple simple** :

Vous voulez prédire si un email est un spam :

1. **Données d'entraînement** : 10 000 emails étiquetés (spam / non-spam)
2. **Apprentissage** : L'algorithme apprend les patterns des spams
3. **Modèle** : Règles apprises pour identifier les spams
4. **Prédiction** : Nouveau email → Le modèle prédit spam ou non

### 1.3 Types d'apprentissage

#### Apprentissage supervisé

Vous fournissez les réponses pendant l'entraînement.

**Exemple** : Photos de chats et chiens étiquetées
```
[Photo] → [Label: chat]
[Photo] → [Label: chien]
```

**Applications** :
- Classification d'images
- Prédiction de prix
- Diagnostic médical

#### Apprentissage non supervisé

L'algorithme trouve lui-même les patterns.

**Exemple** : Grouper des clients similaires
```
[Données clients] → [Algorithme] → [Groupes découverts]
```

**Applications** :
- Segmentation de marché
- Détection d'anomalies
- Compression de données

#### Apprentissage par renforcement

Apprendre par essai-erreur avec récompenses.

**Exemple** : Un robot apprend à marcher
```
[Action] → [Résultat] → [Récompense/Punition] → [Amélioration]
```

**Applications** :
- Jeux vidéo
- Voitures autonomes
- Robots

### 1.4 Delphi et l'IA : Approches possibles

**1. Utiliser des APIs cloud** ⭐ (Recommandé pour débuter)
- OpenAI (GPT, DALL-E)
- Google Cloud AI
- Azure Cognitive Services
- AWS AI Services

**Avantages** :
- Facile à intégrer
- Modèles pré-entraînés puissants
- Pas besoin d'expertise ML
- Mise à jour automatique

**2. Bibliothèques ML en DLL**
- TensorFlow via DLL
- ONNX Runtime
- PyTorch via interface

**Avantages** :
- Contrôle total
- Pas de coût API
- Fonctionne offline

**3. Modèles personnalisés**
- Entraîner vos propres modèles
- Intégration Python + Delphi

**Avantages** :
- Personnalisation totale
- Données privées

---

## Partie 2 : Notre projet fil rouge - Assistant IA

### 2.1 Présentation du projet

Nous allons créer **"Smart Assistant"** - Un assistant intelligent multi-fonctions :

**Fonctionnalités** :
1. 💬 **Chatbot intelligent** : Conversations en langage naturel
2. 👁️ **Analyse d'images** : Détection d'objets, texte, visages
3. 📝 **Traitement de texte** : Résumés, traductions, sentiment
4. 📊 **Prédictions** : Analyse de données et prévisions

### 2.2 Architecture de l'application

```
┌─────────────────────────────────────────┐
│         Interface Delphi (VCL)          │
│   ┌──────────┐  ┌──────────┐            │
│   │ Chatbot  │  │ Vision   │  ...       │
│   └──────────┘  └──────────┘            │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│       Gestionnaire d'APIs IA            │
│   ┌──────────┐  ┌──────────┐            │
│   │ OpenAI   │  │ Google   │  ...       │
│   └──────────┘  └──────────┘            │
└──────────┬──────────────────────────────┘
           │
           ↓
┌─────────────────────────────────────────┐
│         Services Cloud IA               │
│   GPT-4, Vision API, etc.               │
└─────────────────────────────────────────┘
```

### 2.3 Configuration des APIs

#### OpenAI API (GPT)

1. Créez un compte sur [platform.openai.com](https://platform.openai.com)
2. Obtenez une clé API
3. Notez votre clé : `sk-...`

**Tarifs** (2025) :
- GPT-3.5-turbo : ~0.002 $ / 1K tokens
- GPT-4 : ~0.03 $ / 1K tokens
- Budget débutant : 5-10 $ suffisent pour tester

#### Google Cloud Vision API

1. Créez un projet sur [console.cloud.google.com](https://console.cloud.google.com)
2. Activez **Vision API**
3. Créez des credentials
4. Téléchargez le fichier JSON

**Gratuit** : 1000 requêtes/mois

---

## Partie 3 : Chatbot intelligent avec GPT

### 3.1 Gestionnaire OpenAI

Créons une unité pour gérer les appels à OpenAI :

```pascal
unit uOpenAI;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.Generics.Collections;

type
  TOpenAIMessage = record
    Role: string;     // 'system', 'user', 'assistant'
    Content: string;
  end;

  TOpenAI = class
  private
    FAPIKey: string;
    FModel: string;
    FConversationHistory: TList<TOpenAIMessage>;

    function BuildRequest(const AMessages: TArray<TOpenAIMessage>): TJSONObject;
    function ParseResponse(const AResponse: string): string;
  public
    constructor Create(const AAPIKey: string);
    destructor Destroy; override;

    function SendMessage(const AMessage: string): string;
    function SendMessageWithContext(const AMessage: string): string;
    procedure ClearHistory;
    procedure SetSystemPrompt(const APrompt: string);

    property Model: string read FModel write FModel;
  end;

implementation

{ TOpenAI }

constructor TOpenAI.Create(const AAPIKey: string);  
begin  
  inherited Create;
  FAPIKey := AAPIKey;
  FModel := 'gpt-3.5-turbo';  // ou 'gpt-4' pour plus de puissance
  FConversationHistory := TList<TOpenAIMessage>.Create;
end;

destructor TOpenAI.Destroy;  
begin  
  FConversationHistory.Free;
  inherited;
end;

procedure TOpenAI.SetSystemPrompt(const APrompt: string);  
var  
  Msg: TOpenAIMessage;
begin
  // Le prompt système définit le comportement de l'IA
  Msg.Role := 'system';
  Msg.Content := APrompt;
  FConversationHistory.Insert(0, Msg);
end;

procedure TOpenAI.ClearHistory;  
begin  
  FConversationHistory.Clear;
end;

function TOpenAI.BuildRequest(const AMessages: TArray<TOpenAIMessage>): TJSONObject;  
var  
  MessagesArray: TJSONArray;
  Msg: TOpenAIMessage;
  MsgObj: TJSONObject;
begin
  Result := TJSONObject.Create;

  Result.AddPair('model', FModel);
  Result.AddPair('temperature', TJSONNumber.Create(0.7));
  Result.AddPair('max_tokens', TJSONNumber.Create(1000));

  MessagesArray := TJSONArray.Create;

  for Msg in AMessages do
  begin
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('role', Msg.Role);
    MsgObj.AddPair('content', Msg.Content);
    MessagesArray.AddElement(MsgObj);
  end;

  Result.AddPair('messages', MessagesArray);
end;

function TOpenAI.ParseResponse(const AResponse: string): string;  
var  
  JSONResponse: TJSONObject;
  Choices: TJSONArray;
  FirstChoice: TJSONObject;
  Message: TJSONObject;
begin
  Result := '';

  JSONResponse := TJSONObject.ParseJSONValue(AResponse) as TJSONObject;
  try
    if Assigned(JSONResponse) then
    begin
      Choices := JSONResponse.GetValue<TJSONArray>('choices');
      if Assigned(Choices) and (Choices.Count > 0) then
      begin
        FirstChoice := Choices.Items[0] as TJSONObject;
        Message := FirstChoice.GetValue<TJSONObject>('message');
        Result := Message.GetValue<string>('content');
      end;
    end;
  finally
    JSONResponse.Free;
  end;
end;

function TOpenAI.SendMessage(const AMessage: string): string;  
var  
  HTTP: THTTPClient;
  Request: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  Messages: TArray<TOpenAIMessage>;
  Msg: TOpenAIMessage;
begin
  HTTP := THTTPClient.Create;
  try
    // Configuration
    HTTP.CustomHeaders['Authorization'] := 'Bearer ' + FAPIKey;
    HTTP.CustomHeaders['Content-Type'] := 'application/json';

    // Message utilisateur
    SetLength(Messages, 1);
    Messages[0].Role := 'user';
    Messages[0].Content := AMessage;

    // Construire la requête
    Request := BuildRequest(Messages);
    try
      RequestBody := TStringStream.Create(Request.ToString, TEncoding.UTF8);
      try
        // Envoyer à l'API OpenAI
        Response := HTTP.Post(
          'https://api.openai.com/v1/chat/completions',
          RequestBody
        );

        if Response.StatusCode = 200 then
          Result := ParseResponse(Response.ContentAsString)
        else
          raise Exception.CreateFmt('Erreur API: %d - %s',
            [Response.StatusCode, Response.ContentAsString]);

      finally
        RequestBody.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TOpenAI.SendMessageWithContext(const AMessage: string): string;  
var  
  HTTP: THTTPClient;
  Request: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  Messages: TArray<TOpenAIMessage>;
  UserMsg, AssistantMsg: TOpenAIMessage;
  I: Integer;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := 'Bearer ' + FAPIKey;
    HTTP.CustomHeaders['Content-Type'] := 'application/json';

    // Ajouter le message utilisateur à l'historique
    UserMsg.Role := 'user';
    UserMsg.Content := AMessage;
    FConversationHistory.Add(UserMsg);

    // Convertir l'historique en tableau
    SetLength(Messages, FConversationHistory.Count);
    for I := 0 to FConversationHistory.Count - 1 do
      Messages[I] := FConversationHistory[I];

    // Construire et envoyer
    Request := BuildRequest(Messages);
    try
      RequestBody := TStringStream.Create(Request.ToString, TEncoding.UTF8);
      try
        Response := HTTP.Post(
          'https://api.openai.com/v1/chat/completions',
          RequestBody
        );

        if Response.StatusCode = 200 then
        begin
          Result := ParseResponse(Response.ContentAsString);

          // Ajouter la réponse à l'historique
          AssistantMsg.Role := 'assistant';
          AssistantMsg.Content := Result;
          FConversationHistory.Add(AssistantMsg);
        end
        else
          raise Exception.CreateFmt('Erreur API: %d - %s',
            [Response.StatusCode, Response.ContentAsString]);

      finally
        RequestBody.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

end.
```

### 3.2 Interface du Chatbot

```pascal
unit uChatbotForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, uOpenAI;

type
  TChatbotForm = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    ButtonClear: TButton;
    ComboBoxModel: TComboBox;

    PanelChat: TPanel;
    RichEditChat: TRichEdit;

    PanelInput: TPanel;
    MemoInput: TMemo;
    ButtonSend: TButton;
    ButtonVoice: TButton;

    StatusBar1: TStatusBar;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure MemoInputKeyPress(Sender: TObject; var Key: Char);
    procedure ComboBoxModelChange(Sender: TObject);
  private
    FOpenAI: TOpenAI;
    procedure AddMessage(const ARole, AMessage: string);
    procedure SendMessageAsync(const AMessage: string);
  public
  end;

var
  ChatbotForm: TChatbotForm;

implementation

{$R *.dfm}

uses
  System.Threading;

procedure TChatbotForm.FormCreate(Sender: TObject);  
begin  
  // Initialiser OpenAI avec votre clé API
  FOpenAI := TOpenAI.Create('sk-votre-cle-api-ici');

  // Définir le comportement de l'assistant
  FOpenAI.SetSystemPrompt(
    'Tu es un assistant intelligent et serviable. ' +
    'Réponds de manière concise et claire en français. ' +
    'Si tu ne sais pas quelque chose, dis-le honnêtement.'
  );

  // Configurer l'interface
  RichEditChat.Clear;
  RichEditChat.ReadOnly := True;

  ComboBoxModel.Items.Add('GPT-3.5 Turbo (rapide)');
  ComboBoxModel.Items.Add('GPT-4 (plus intelligent)');
  ComboBoxModel.ItemIndex := 0;

  // Message de bienvenue
  AddMessage('Assistant', 'Bonjour ! Je suis votre assistant IA. Comment puis-je vous aider ?');
end;

procedure TChatbotForm.FormDestroy(Sender: TObject);  
begin  
  FOpenAI.Free;
end;

procedure TChatbotForm.ComboBoxModelChange(Sender: TObject);  
begin  
  case ComboBoxModel.ItemIndex of
    0: FOpenAI.Model := 'gpt-3.5-turbo';
    1: FOpenAI.Model := 'gpt-4';
  end;
end;

procedure TChatbotForm.AddMessage(const ARole, AMessage: string);  
var  
  TimeStamp: string;
begin
  TimeStamp := FormatDateTime('hh:nn', Now);

  RichEditChat.SelStart := Length(RichEditChat.Text);

  // Style pour le rôle
  if ARole = 'Vous' then
  begin
    RichEditChat.SelAttributes.Color := clBlue;
    RichEditChat.SelAttributes.Style := [fsBold];
  end
  else
  begin
    RichEditChat.SelAttributes.Color := clGreen;
    RichEditChat.SelAttributes.Style := [fsBold];
  end;

  RichEditChat.SelText := Format('[%s] %s:', [TimeStamp, ARole]) + #13#10;

  // Style pour le message
  RichEditChat.SelAttributes.Color := clBlack;
  RichEditChat.SelAttributes.Style := [];
  RichEditChat.SelText := AMessage + #13#10#13#10;

  // Scroller vers le bas
  RichEditChat.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TChatbotForm.SendMessageAsync(const AMessage: string);  
begin  
  // Désactiver l'interface pendant le traitement
  ButtonSend.Enabled := False;
  MemoInput.Enabled := False;
  StatusBar1.SimpleText := 'L''assistant réfléchit...';

  // Exécuter dans un thread séparé
  TTask.Run(
    procedure
    var
      Response: string;
    begin
      try
        // Appel à l'API (avec contexte de conversation)
        Response := FOpenAI.SendMessageWithContext(AMessage);

        // Retour au thread principal pour l'UI
        TThread.Synchronize(nil,
          procedure
          begin
            AddMessage('Assistant', Response);

            // Réactiver l'interface
            ButtonSend.Enabled := True;
            MemoInput.Enabled := True;
            MemoInput.SetFocus;
            StatusBar1.SimpleText := 'Prêt';
          end);

      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              AddMessage('Erreur', 'Désolé, une erreur est survenue : ' + E.Message);
              ButtonSend.Enabled := True;
              MemoInput.Enabled := True;
              StatusBar1.SimpleText := 'Erreur';
            end);
        end;
      end;
    end);
end;

procedure TChatbotForm.ButtonSendClick(Sender: TObject);  
var  
  UserMessage: string;
begin
  UserMessage := MemoInput.Text.Trim;

  if UserMessage.IsEmpty then
    Exit;

  // Afficher le message utilisateur
  AddMessage('Vous', UserMessage);

  // Vider le champ de saisie
  MemoInput.Clear;

  // Envoyer à l'IA
  SendMessageAsync(UserMessage);
end;

procedure TChatbotForm.MemoInputKeyPress(Sender: TObject; var Key: Char);  
begin  
  // Envoyer avec Ctrl+Enter
  if (Key = #13) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    Key := #0;
    ButtonSendClick(nil);
  end;
end;

procedure TChatbotForm.ButtonClearClick(Sender: TObject);  
begin  
  if MessageDlg('Effacer la conversation ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    RichEditChat.Clear;
    FOpenAI.ClearHistory;

    // Remettre le prompt système
    FOpenAI.SetSystemPrompt(
      'Tu es un assistant intelligent et serviable. ' +
      'Réponds de manière concise et claire en français.'
    );

    AddMessage('Assistant', 'Nouvelle conversation démarrée. Comment puis-je vous aider ?');
  end;
end;

end.
```

### 3.3 Fonctionnalités avancées du Chatbot

#### Analyse de sentiment

```pascal
function TOpenAI.AnalyzeSentiment(const AText: string): string;  
var  
  Prompt: string;
begin
  Prompt := Format(
    'Analyse le sentiment du texte suivant et réponds uniquement par ' +
    '"POSITIF", "NÉGATIF" ou "NEUTRE":%s%s',
    [#13#10, AText]
  );

  Result := SendMessage(Prompt).Trim.ToUpper;
end;

// Utilisation
var
  Sentiment: string;
begin
  Sentiment := FOpenAI.AnalyzeSentiment('Ce produit est formidable !');
  // Résultat: "POSITIF"
end;
```

#### Résumé de texte

```pascal
function TOpenAI.SummarizeText(const AText: string; AMaxLength: Integer = 100): string;  
var  
  Prompt: string;
begin
  Prompt := Format(
    'Résume le texte suivant en maximum %d mots:%s%s',
    [AMaxLength, #13#10, AText]
  );

  Result := SendMessage(Prompt);
end;
```

#### Traduction

```pascal
function TOpenAI.Translate(const AText, ATargetLanguage: string): string;  
var  
  Prompt: string;
begin
  Prompt := Format(
    'Traduis le texte suivant en %s:%s%s',
    [ATargetLanguage, #13#10, AText]
  );

  Result := SendMessage(Prompt);
end;
```

---

## Partie 4 : Computer Vision - Analyse d'images

### 4.1 Google Cloud Vision API

```pascal
unit uGoogleVision;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.NetEncoding, Vcl.Graphics;

type
  TDetectedObject = record
    Name: string;
    Confidence: Double;
    BoundingBox: TRect;
  end;

  TDetectedText = record
    Text: string;
    Confidence: Double;
  end;

  TGoogleVision = class
  private
    FAPIKey: string;

    function EncodeImageToBase64(ABitmap: TBitmap): string;
    function BuildRequest(const ABase64Image: string;
      const AFeatures: TArray<string>): TJSONObject;
  public
    constructor Create(const AAPIKey: string);

    function DetectLabels(ABitmap: TBitmap): TArray<TDetectedObject>;
    function DetectText(ABitmap: TBitmap): TArray<TDetectedText>;
    function DetectFaces(ABitmap: TBitmap): Integer;
    function DescribeImage(ABitmap: TBitmap): string;
  end;

implementation

uses
  System.Types;

{ TGoogleVision }

constructor TGoogleVision.Create(const AAPIKey: string);  
begin  
  inherited Create;
  FAPIKey := AAPIKey;
end;

function TGoogleVision.EncodeImageToBase64(ABitmap: TBitmap): string;  
var  
  Stream: TMemoryStream;
  JPEGImage: TJPEGImage;
begin
  Stream := TMemoryStream.Create;
  try
    JPEGImage := TJPEGImage.Create;
    try
      JPEGImage.Assign(ABitmap);
      JPEGImage.CompressionQuality := 85;
      JPEGImage.SaveToStream(Stream);
    finally
      JPEGImage.Free;
    end;

    Stream.Position := 0;
    Result := TNetEncoding.Base64.EncodeBytesToString(
      Stream.Memory, Stream.Size
    );
  finally
    Stream.Free;
  end;
end;

function TGoogleVision.BuildRequest(const ABase64Image: string;
  const AFeatures: TArray<string>): TJSONObject;
var
  RequestsArray: TJSONArray;
  RequestObj: TJSONObject;
  ImageObj: TJSONObject;
  FeaturesArray: TJSONArray;
  Feature: string;
  FeatureObj: TJSONObject;
begin
  Result := TJSONObject.Create;

  RequestsArray := TJSONArray.Create;
  RequestObj := TJSONObject.Create;

  // Image
  ImageObj := TJSONObject.Create;
  ImageObj.AddPair('content', ABase64Image);
  RequestObj.AddPair('image', ImageObj);

  // Features
  FeaturesArray := TJSONArray.Create;
  for Feature in AFeatures do
  begin
    FeatureObj := TJSONObject.Create;
    FeatureObj.AddPair('type', Feature);
    FeatureObj.AddPair('maxResults', TJSONNumber.Create(10));
    FeaturesArray.AddElement(FeatureObj);
  end;
  RequestObj.AddPair('features', FeaturesArray);

  RequestsArray.AddElement(RequestObj);
  Result.AddPair('requests', RequestsArray);
end;

function TGoogleVision.DetectLabels(ABitmap: TBitmap): TArray<TDetectedObject>;  
var  
  HTTP: THTTPClient;
  Request: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  Base64Image: string;
  Features: TArray<string>;
  ResponseJSON: TJSONObject;
  ResponsesArray: TJSONArray;
  FirstResponse: TJSONObject;
  LabelsArray: TJSONArray;
  I: Integer;
  LabelObj: TJSONObject;
  Obj: TDetectedObject;
begin
  HTTP := THTTPClient.Create;
  try
    Base64Image := EncodeImageToBase64(ABitmap);

    SetLength(Features, 1);
    Features[0] := 'LABEL_DETECTION';

    Request := BuildRequest(Base64Image, Features);
    try
      RequestBody := TStringStream.Create(Request.ToString, TEncoding.UTF8);
      try
        Response := HTTP.Post(
          Format('https://vision.googleapis.com/v1/images:annotate?key=%s', [FAPIKey]),
          RequestBody
        );

        if Response.StatusCode = 200 then
        begin
          ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
          try
            ResponsesArray := ResponseJSON.GetValue<TJSONArray>('responses');
            FirstResponse := ResponsesArray.Items[0] as TJSONObject;

            if FirstResponse.TryGetValue<TJSONArray>('labelAnnotations', LabelsArray) then
            begin
              SetLength(Result, LabelsArray.Count);

              for I := 0 to LabelsArray.Count - 1 do
              begin
                LabelObj := LabelsArray.Items[I] as TJSONObject;

                Obj.Name := LabelObj.GetValue<string>('description');
                Obj.Confidence := LabelObj.GetValue<Double>('score');

                Result[I] := Obj;
              end;
            end;
          finally
            ResponseJSON.Free;
          end;
        end;

      finally
        RequestBody.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TGoogleVision.DetectText(ABitmap: TBitmap): TArray<TDetectedText>;  
var  
  HTTP: THTTPClient;
  Request: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  Base64Image: string;
  Features: TArray<string>;
  ResponseJSON: TJSONObject;
  ResponsesArray: TJSONArray;
  FirstResponse: TJSONObject;
  TextArray: TJSONArray;
  I: Integer;
  TextObj: TJSONObject;
  Text: TDetectedText;
begin
  HTTP := THTTPClient.Create;
  try
    Base64Image := EncodeImageToBase64(ABitmap);

    SetLength(Features, 1);
    Features[0] := 'TEXT_DETECTION';

    Request := BuildRequest(Base64Image, Features);
    try
      RequestBody := TStringStream.Create(Request.ToString, TEncoding.UTF8);
      try
        Response := HTTP.Post(
          Format('https://vision.googleapis.com/v1/images:annotate?key=%s', [FAPIKey]),
          RequestBody
        );

        if Response.StatusCode = 200 then
        begin
          ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
          try
            ResponsesArray := ResponseJSON.GetValue<TJSONArray>('responses');
            FirstResponse := ResponsesArray.Items[0] as TJSONObject;

            if FirstResponse.TryGetValue<TJSONArray>('textAnnotations', TextArray) then
            begin
              SetLength(Result, TextArray.Count);

              for I := 0 to TextArray.Count - 1 do
              begin
                TextObj := TextArray.Items[I] as TJSONObject;

                Text.Text := TextObj.GetValue<string>('description');

                if I = 0 then
                  Text.Confidence := 1.0  // Le premier est le texte complet
                else
                  Text.Confidence := 0.9;

                Result[I] := Text;
              end;
            end;
          finally
            ResponseJSON.Free;
          end;
        end;

      finally
        RequestBody.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TGoogleVision.DetectFaces(ABitmap: TBitmap): Integer;  
var  
  HTTP: THTTPClient;
  Request: TJSONObject;
  RequestBody: TStringStream;
  Response: IHTTPResponse;
  Base64Image: string;
  Features: TArray<string>;
  ResponseJSON: TJSONObject;
  ResponsesArray: TJSONArray;
  FirstResponse: TJSONObject;
  FacesArray: TJSONArray;
begin
  Result := 0;
  HTTP := THTTPClient.Create;
  try
    Base64Image := EncodeImageToBase64(ABitmap);

    SetLength(Features, 1);
    Features[0] := 'FACE_DETECTION';

    Request := BuildRequest(Base64Image, Features);
    try
      RequestBody := TStringStream.Create(Request.ToString, TEncoding.UTF8);
      try
        Response := HTTP.Post(
          Format('https://vision.googleapis.com/v1/images:annotate?key=%s', [FAPIKey]),
          RequestBody
        );

        if Response.StatusCode = 200 then
        begin
          ResponseJSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
          try
            ResponsesArray := ResponseJSON.GetValue<TJSONArray>('responses');
            FirstResponse := ResponsesArray.Items[0] as TJSONObject;

            if FirstResponse.TryGetValue<TJSONArray>('faceAnnotations', FacesArray) then
              Result := FacesArray.Count;
          finally
            ResponseJSON.Free;
          end;
        end;

      finally
        RequestBody.Free;
      end;
    finally
      Request.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TGoogleVision.DescribeImage(ABitmap: TBitmap): string;  
var  
  Objects: TArray<TDetectedObject>;
  Obj: TDetectedObject;
  Description: TStringList;
begin
  Objects := DetectLabels(ABitmap);

  Description := TStringList.Create;
  try
    Description.Add('Cette image contient :');

    for Obj in Objects do
    begin
      if Obj.Confidence > 0.7 then  // Confiance > 70%
        Description.Add(Format('- %s (%.0f%%)',
          [Obj.Name, Obj.Confidence * 100]));
    end;

    Result := Description.Text;
  finally
    Description.Free;
  end;
end;

end.
```

### 4.2 Interface d'analyse d'images

```pascal
unit uVisionForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ExtDlgs, Vcl.ComCtrls, uGoogleVision;

type
  TVisionForm = class(TForm)
    PanelTop: TPanel;
    ButtonLoad: TButton;
    ButtonAnalyze: TButton;
    ButtonOCR: TButton;
    ButtonFaces: TButton;

    ImagePreview: TImage;
    MemoResults: TMemo;
    ProgressBar1: TProgressBar;

    OpenPictureDialog1: TOpenPictureDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure ButtonOCRClick(Sender: TObject);
    procedure ButtonFacesClick(Sender: TObject);
  private
    FVision: TGoogleVision;
    procedure AnalyzeImageAsync(AAnalysisType: string);
  public
  end;

var
  VisionForm: TVisionForm;

implementation

{$R *.dfm}

uses
  System.Threading;

procedure TVisionForm.FormCreate(Sender: TObject);  
begin  
  FVision := TGoogleVision.Create('votre-cle-api-google');
  MemoResults.Clear;
end;

procedure TVisionForm.FormDestroy(Sender: TObject);  
begin  
  FVision.Free;
end;

procedure TVisionForm.ButtonLoadClick(Sender: TObject);  
begin  
  if OpenPictureDialog1.Execute then
  begin
    ImagePreview.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    MemoResults.Lines.Add('Image chargée : ' +
      ExtractFileName(OpenPictureDialog1.FileName));
  end;
end;

procedure TVisionForm.AnalyzeImageAsync(AAnalysisType: string);  
begin  
  if not Assigned(ImagePreview.Picture.Graphic) then
  begin
    ShowMessage('Veuillez charger une image d''abord');
    Exit;
  end;

  // Désactiver les boutons
  ButtonAnalyze.Enabled := False;
  ButtonOCR.Enabled := False;
  ButtonFaces.Enabled := False;
  ProgressBar1.Style := pbstMarquee;
  MemoResults.Lines.Add('Analyse en cours...');

  TTask.Run(
    procedure
    var
      Bitmap: TBitmap;
      Result: string;
      Objects: TArray<TDetectedObject>;
      Texts: TArray<TDetectedText>;
      FaceCount: Integer;
      Obj: TDetectedObject;
      Text: TDetectedText;
    begin
      try
        // Convertir en Bitmap
        Bitmap := TBitmap.Create;
        try
          Bitmap.Assign(ImagePreview.Picture.Graphic);

          if AAnalysisType = 'labels' then
          begin
            Objects := FVision.DetectLabels(Bitmap);

            TThread.Synchronize(nil,
              procedure
              begin
                MemoResults.Lines.Add('');
                MemoResults.Lines.Add('=== Objets détectés ===');
                for Obj in Objects do
                begin
                  MemoResults.Lines.Add(Format('%s : %.1f%%',
                    [Obj.Name, Obj.Confidence * 100]));
                end;
              end);
          end
          else if AAnalysisType = 'text' then
          begin
            Texts := FVision.DetectText(Bitmap);

            TThread.Synchronize(nil,
              procedure
              begin
                MemoResults.Lines.Add('');
                MemoResults.Lines.Add('=== Texte détecté ===');
                if Length(Texts) > 0 then
                  MemoResults.Lines.Add(Texts[0].Text)
                else
                  MemoResults.Lines.Add('Aucun texte trouvé');
              end);
          end
          else if AAnalysisType = 'faces' then
          begin
            FaceCount := FVision.DetectFaces(Bitmap);

            TThread.Synchronize(nil,
              procedure
              begin
                MemoResults.Lines.Add('');
                MemoResults.Lines.Add(Format('=== %d visage(s) détecté(s) ===',
                  [FaceCount]));
              end);
          end;

        finally
          Bitmap.Free;
        end;

        // Réactiver l'interface
        TThread.Synchronize(nil,
          procedure
          begin
            ButtonAnalyze.Enabled := True;
            ButtonOCR.Enabled := True;
            ButtonFaces.Enabled := True;
            ProgressBar1.Style := pbstNormal;
          end);

      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              MemoResults.Lines.Add('Erreur : ' + E.Message);
              ButtonAnalyze.Enabled := True;
              ButtonOCR.Enabled := True;
              ButtonFaces.Enabled := True;
              ProgressBar1.Style := pbstNormal;
            end);
        end;
      end;
    end);
end;

procedure TVisionForm.ButtonAnalyzeClick(Sender: TObject);  
begin  
  AnalyzeImageAsync('labels');
end;

procedure TVisionForm.ButtonOCRClick(Sender: TObject);  
begin  
  AnalyzeImageAsync('text');
end;

procedure TVisionForm.ButtonFacesClick(Sender: TObject);  
begin  
  AnalyzeImageAsync('faces');
end;

end.
```

---

## Partie 5 : Machine Learning pratique

### 5.1 Prédictions avec régression linéaire

Créons un système de prédiction simple :

```pascal
unit uMachineLearning;

interface

uses
  System.SysUtils, System.Math, System.Generics.Collections;

type
  TDataPoint = record
    X: Double;
    Y: Double;
  end;

  TLinearRegression = class
  private
    FSlope: Double;        // Pente
    FIntercept: Double;    // Ordonnée à l'origine
    FTrained: Boolean;

    function Mean(const AValues: TArray<Double>): Double;
    function Variance(const AValues: TArray<Double>): Double;
    function Covariance(const AX, AY: TArray<Double>): Double;
  public
    procedure Train(const AData: TArray<TDataPoint>);
    function Predict(AX: Double): Double;
    function Score(const ATestData: TArray<TDataPoint>): Double; // R²

    property Trained: Boolean read FTrained;
    property Slope: Double read FSlope;
    property Intercept: Double read FIntercept;
  end;

implementation

{ TLinearRegression }

function TLinearRegression.Mean(const AValues: TArray<Double>): Double;  
var  
  Sum: Double;
  Value: Double;
begin
  Sum := 0;
  for Value in AValues do
    Sum := Sum + Value;
  Result := Sum / Length(AValues);
end;

function TLinearRegression.Variance(const AValues: TArray<Double>): Double;  
var  
  Avg: Double;
  Sum: Double;
  Value: Double;
begin
  Avg := Mean(AValues);
  Sum := 0;

  for Value in AValues do
    Sum := Sum + Sqr(Value - Avg);

  Result := Sum / Length(AValues);
end;

function TLinearRegression.Covariance(const AX, AY: TArray<Double>): Double;  
var  
  MeanX, MeanY: Double;
  Sum: Double;
  I: Integer;
begin
  MeanX := Mean(AX);
  MeanY := Mean(AY);
  Sum := 0;

  for I := 0 to High(AX) do
    Sum := Sum + (AX[I] - MeanX) * (AY[I] - MeanY);

  Result := Sum / Length(AX);
end;

procedure TLinearRegression.Train(const AData: TArray<TDataPoint>);  
var  
  XValues, YValues: TArray<Double>;
  I: Integer;
  VarX, Cov: Double;
begin
  if Length(AData) < 2 then
    raise Exception.Create('Pas assez de données pour l''entraînement');

  // Extraire X et Y
  SetLength(XValues, Length(AData));
  SetLength(YValues, Length(AData));

  for I := 0 to High(AData) do
  begin
    XValues[I] := AData[I].X;
    YValues[I] := AData[I].Y;
  end;

  // Calculer la pente et l'ordonnée
  VarX := Variance(XValues);
  Cov := Covariance(XValues, YValues);

  FSlope := Cov / VarX;
  FIntercept := Mean(YValues) - FSlope * Mean(XValues);

  FTrained := True;
end;

function TLinearRegression.Predict(AX: Double): Double;  
begin  
  if not FTrained then
    raise Exception.Create('Le modèle n''est pas entraîné');

  Result := FSlope * AX + FIntercept;
end;

function TLinearRegression.Score(const ATestData: TArray<TDataPoint>): Double;  
var  
  YTrue, YPred: TArray<Double>;
  I: Integer;
  SSRes, SSTot: Double;
  MeanY: Double;
begin
  if not FTrained then
    raise Exception.Create('Le modèle n''est pas entraîné');

  // Préparer les données
  SetLength(YTrue, Length(ATestData));
  SetLength(YPred, Length(ATestData));

  for I := 0 to High(ATestData) do
  begin
    YTrue[I] := ATestData[I].Y;
    YPred[I] := Predict(ATestData[I].X);
  end;

  // Calculer R²
  MeanY := Mean(YTrue);
  SSRes := 0;
  SSTot := 0;

  for I := 0 to High(YTrue) do
  begin
    SSRes := SSRes + Sqr(YTrue[I] - YPred[I]);
    SSTot := SSTot + Sqr(YTrue[I] - MeanY);
  end;

  Result := 1 - (SSRes / SSTot);
end;

end.
```

### 5.2 Application de prédiction

```pascal
unit uPredictionForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs,
  VCLTee.Chart, uMachineLearning;

type
  TPredictionForm = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;

    StringGridData: TStringGrid;
    ButtonTrain: TButton;
    ButtonPredict: TButton;
    ButtonLoadData: TButton;

    EditPredictValue: TEdit;
    LabelPrediction: TLabel;

    Chart1: TChart;
    Series1: TLineSeries;
    Series2: TPointSeries;

    MemoInfo: TMemo;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonTrainClick(Sender: TObject);
    procedure ButtonPredictClick(Sender: TObject);
    procedure ButtonLoadDataClick(Sender: TObject);
  private
    FModel: TLinearRegression;
    procedure InitializeGrid;
    procedure LoadSampleData;
    procedure UpdateChart;
  public
  end;

var
  PredictionForm: TPredictionForm;

implementation

{$R *.dfm}

procedure TPredictionForm.FormCreate(Sender: TObject);  
begin  
  FModel := TLinearRegression.Create;
  InitializeGrid;
  LoadSampleData;
end;

procedure TPredictionForm.FormDestroy(Sender: TObject);  
begin  
  FModel.Free;
end;

procedure TPredictionForm.InitializeGrid;  
begin  
  StringGridData.ColCount := 2;
  StringGridData.RowCount := 11;
  StringGridData.FixedRows := 1;

  StringGridData.Cells[0, 0] := 'X (Mois)';
  StringGridData.Cells[1, 0] := 'Y (Ventes)';

  StringGridData.ColWidths[0] := 100;
  StringGridData.ColWidths[1] := 100;
end;

procedure TPredictionForm.LoadSampleData;  
var  
  I: Integer;
begin
  // Données d'exemple : ventes sur 10 mois
  for I := 1 to 10 do
  begin
    StringGridData.Cells[0, I] := IntToStr(I);
    // Tendance croissante avec un peu de bruit
    StringGridData.Cells[1, I] := IntToStr(
      Round(1000 + I * 150 + Random(100) - 50)
    );
  end;

  MemoInfo.Lines.Add('Données d''exemple chargées');
  MemoInfo.Lines.Add('Vous pouvez modifier les valeurs manuellement');
end;

procedure TPredictionForm.ButtonLoadDataClick(Sender: TObject);  
begin  
  LoadSampleData;
  UpdateChart;
end;

procedure TPredictionForm.ButtonTrainClick(Sender: TObject);  
var  
  Data: TArray<TDataPoint>;
  I: Integer;
  X, Y: Double;
  Score: Double;
begin
  // Collecter les données depuis la grille
  SetLength(Data, StringGridData.RowCount - 1);

  for I := 1 to StringGridData.RowCount - 1 do
  begin
    if not TryStrToFloat(StringGridData.Cells[0, I], X) then
      Continue;
    if not TryStrToFloat(StringGridData.Cells[1, I], Y) then
      Continue;

    Data[I - 1].X := X;
    Data[I - 1].Y := Y;
  end;

  // Entraîner le modèle
  try
    FModel.Train(Data);

    // Calculer le score
    Score := FModel.Score(Data);

    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add('=== Modèle entraîné ===');
    MemoInfo.Lines.Add(Format('Pente : %.2f', [FModel.Slope]));
    MemoInfo.Lines.Add(Format('Ordonnée : %.2f', [FModel.Intercept]));
    MemoInfo.Lines.Add(Format('Score R² : %.4f', [Score]));

    if Score > 0.8 then
      MemoInfo.Lines.Add('Qualité : Excellente')
    else if Score > 0.6 then
      MemoInfo.Lines.Add('Qualité : Bonne')
    else
      MemoInfo.Lines.Add('Qualité : Moyenne');

    UpdateChart;
    ButtonPredict.Enabled := True;

    ShowMessage('Modèle entraîné avec succès !');

  except
    on E: Exception do
      ShowMessage('Erreur d''entraînement : ' + E.Message);
  end;
end;

procedure TPredictionForm.ButtonPredictClick(Sender: TObject);  
var  
  X, Prediction: Double;
begin
  if not FModel.Trained then
  begin
    ShowMessage('Veuillez d''abord entraîner le modèle');
    Exit;
  end;

  if not TryStrToFloat(EditPredictValue.Text, X) then
  begin
    ShowMessage('Valeur invalide');
    Exit;
  end;

  try
    Prediction := FModel.Predict(X);

    LabelPrediction.Caption := Format('Prédiction : %.2f ventes', [Prediction]);

    MemoInfo.Lines.Add('');
    MemoInfo.Lines.Add(Format('Prédiction pour X=%.0f : %.2f', [X, Prediction]));

  except
    on E: Exception do
      ShowMessage('Erreur de prédiction : ' + E.Message);
  end;
end;

procedure TPredictionForm.UpdateChart;  
var  
  I: Integer;
  X, Y: Double;
  MinX, MaxX: Double;
begin
  // Effacer les séries
  Series1.Clear;
  Series2.Clear;

  // Ajouter les points de données
  MinX := 1000;
  MaxX := -1000;

  for I := 1 to StringGridData.RowCount - 1 do
  begin
    if TryStrToFloat(StringGridData.Cells[0, I], X) and
       TryStrToFloat(StringGridData.Cells[1, I], Y) then
    begin
      Series2.AddXY(X, Y);

      if X < MinX then MinX := X;
      if X > MaxX then MaxX := X;
    end;
  end;

  // Ajouter la ligne de régression si le modèle est entraîné
  if FModel.Trained then
  begin
    for I := 0 to 100 do
    begin
      X := MinX + (MaxX - MinX) * I / 100;
      Y := FModel.Predict(X);
      Series1.AddXY(X, Y);
    end;
  end;
end;

end.
```

### 5.3 Classification avec K-Nearest Neighbors

```pascal
unit uKNN;

interface

uses
  System.SysUtils, System.Math, System.Generics.Collections;

type
  TDataPoint = record
    Features: TArray<Double>;
    Category: string;
  end;

  TKNN = class
  private
    FTrainingData: TArray<TDataPoint>;
    FK: Integer;

    function EuclideanDistance(const A, B: TArray<Double>): Double;
  public
    constructor Create(AK: Integer = 3);

    procedure Train(const AData: TArray<TDataPoint>);
    function Predict(const AFeatures: TArray<Double>): string;
    function Accuracy(const ATestData: TArray<TDataPoint>): Double;

    property K: Integer read FK write FK;
  end;

implementation

{ TKNN }

constructor TKNN.Create(AK: Integer);  
begin  
  inherited Create;
  FK := AK;
end;

function TKNN.EuclideanDistance(const A, B: TArray<Double>): Double;  
var  
  Sum: Double;
  I: Integer;
begin
  Sum := 0;
  for I := 0 to High(A) do
    Sum := Sum + Sqr(A[I] - B[I]);
  Result := Sqrt(Sum);
end;

procedure TKNN.Train(const AData: TArray<TDataPoint>);  
begin  
  FTrainingData := Copy(AData);
end;

function TKNN.Predict(const AFeatures: TArray<Double>): string;  
type  
  TNeighbor = record
    Distance: Double;
    Category: string;
  end;
var
  Neighbors: TArray<TNeighbor>;
  I: Integer;
  Neighbor: TNeighbor;
  Votes: TDictionary<string, Integer>;
  MaxVotes: Integer;
  BestLabel: string;
  Vote: string;
begin
  if Length(FTrainingData) = 0 then
    raise Exception.Create('Le modèle n''est pas entraîné');

  // Calculer les distances
  SetLength(Neighbors, Length(FTrainingData));

  for I := 0 to High(FTrainingData) do
  begin
    Neighbors[I].Distance := EuclideanDistance(AFeatures,
      FTrainingData[I].Features);
    Neighbors[I].Category := FTrainingData[I].Category;
  end;

  // Trier par distance
  TArray.Sort<TNeighbor>(Neighbors,
    TComparer<TNeighbor>.Construct(
      function(const A, B: TNeighbor): Integer
      begin
        Result := CompareValue(A.Distance, B.Distance);
      end
    ));

  // Voter avec les K plus proches
  Votes := TDictionary<string, Integer>.Create;
  try
    for I := 0 to Min(FK - 1, High(Neighbors)) do
    begin
      if Votes.ContainsKey(Neighbors[I].Category) then
        Votes[Neighbors[I].Category] := Votes[Neighbors[I].Category] + 1
      else
        Votes.Add(Neighbors[I].Category, 1);
    end;

    // Trouver la classe avec le plus de votes
    MaxVotes := 0;
    BestLabel := '';

    for Vote in Votes.Keys do
    begin
      if Votes[Vote] > MaxVotes then
      begin
        MaxVotes := Votes[Vote];
        BestLabel := Vote;
      end;
    end;

    Result := BestLabel;
  finally
    Votes.Free;
  end;
end;

function TKNN.Accuracy(const ATestData: TArray<TDataPoint>): Double;  
var  
  Correct: Integer;
  I: Integer;
  Predicted: string;
begin
  Correct := 0;

  for I := 0 to High(ATestData) do
  begin
    Predicted := Predict(ATestData[I].Features);
    if Predicted = ATestData[I].Category then
      Inc(Correct);
  end;

  Result := Correct / Length(ATestData);
end;

end.
```

---

## Partie 6 : Intégration et déploiement

### 6.1 Application complète "Smart Assistant"

Assemblons tous les composants :

```pascal
unit uSmartAssistantMain;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, uOpenAI, uGoogleVision, uMachineLearning;

type
  TSmartAssistantMain = class(TForm)
    PageControl1: TPageControl;

    TabSheetChat: TTabSheet;
    // Composants du chat

    TabSheetVision: TTabSheet;
    // Composants de vision

    TabSheetML: TTabSheet;
    // Composants ML

    TabSheetSettings: TTabSheet;
    // Paramètres

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOpenAI: TOpenAI;
    FVision: TGoogleVision;
    FMLModel: TLinearRegression;

    procedure LoadSettings;
    procedure SaveSettings;
  public
  end;

var
  SmartAssistantMain: TSmartAssistantMain;

implementation

{$R *.dfm}

uses
  System.IniFiles;

procedure TSmartAssistantMain.FormCreate(Sender: TObject);  
begin  
  LoadSettings;

  // Initialiser les services IA
  FOpenAI := TOpenAI.Create('votre-cle-openai');
  FVision := TGoogleVision.Create('votre-cle-google');
  FMLModel := TLinearRegression.Create;
end;

procedure TSmartAssistantMain.FormDestroy(Sender: TObject);  
begin  
  SaveSettings;

  FOpenAI.Free;
  FVision.Free;
  FMLModel.Free;
end;

procedure TSmartAssistantMain.LoadSettings;  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Charger les clés API (à sécuriser en production !)
    // Mieux : stocker dans le registre chiffré ou variables d'environnement
  finally
    IniFile.Free;
  end;
end;

procedure TSmartAssistantMain.SaveSettings;  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Sauvegarder les préférences
  finally
    IniFile.Free;
  end;
end;

end.
```

### 6.2 Optimisation et cache

```pascal
unit uAICache;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.JSON, System.Hash;

type
  TAICache = class
  private
    FCache: TDictionary<string, string>;
    FCacheFile: string;

    function GenerateKey(const AInput: string): string;
  public
    constructor Create(const ACacheFile: string);
    destructor Destroy; override;

    function Get(const AInput: string; out AOutput: string): Boolean;
    procedure Put(const AInput, AOutput: string);
    procedure Clear;
    procedure SaveToFile;
    procedure LoadFromFile;
  end;

implementation

{ TAICache }

constructor TAICache.Create(const ACacheFile: string);  
begin  
  inherited Create;
  FCache := TDictionary<string, string>.Create;
  FCacheFile := ACacheFile;

  if FileExists(FCacheFile) then
    LoadFromFile;
end;

destructor TAICache.Destroy;  
begin  
  SaveToFile;
  FCache.Free;
  inherited;
end;

function TAICache.GenerateKey(const AInput: string): string;  
begin  
  Result := THashMD5.GetHashString(AInput.ToLower.Trim);
end;

function TAICache.Get(const AInput: string; out AOutput: string): Boolean;  
var  
  Key: string;
begin
  Key := GenerateKey(AInput);
  Result := FCache.TryGetValue(Key, AOutput);
end;

procedure TAICache.Put(const AInput, AOutput: string);  
var  
  Key: string;
begin
  Key := GenerateKey(AInput);
  FCache.AddOrSetValue(Key, AOutput);
end;

procedure TAICache.Clear;  
begin  
  FCache.Clear;
end;

procedure TAICache.SaveToFile;  
var  
  JSON: TJSONObject;
  Pair: TPair<string, string>;
begin
  JSON := TJSONObject.Create;
  try
    for Pair in FCache do
      JSON.AddPair(Pair.Key, Pair.Value);

    TFile.WriteAllText(FCacheFile, JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TAICache.LoadFromFile;  
var  
  JSONText: string;
  JSON: TJSONObject;
  Pair: TJSONPair;
begin
  JSONText := TFile.ReadAllText(FCacheFile);
  JSON := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
  try
    for Pair in JSON do
      FCache.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
  finally
    JSON.Free;
  end;
end;

end.
```

### 6.3 Gestion des erreurs et retry

```pascal
function CallAPIWithRetry(ACallFunc: TFunc<string>; AMaxRetries: Integer = 3): string;  
var  
  Attempt: Integer;
  LastError: string;
begin
  for Attempt := 1 to AMaxRetries do
  begin
    try
      Result := ACallFunc();
      Exit; // Succès
    except
      on E: Exception do
      begin
        LastError := E.Message;

        if Attempt < AMaxRetries then
        begin
          // Attendre avant de réessayer (backoff exponentiel)
          Sleep(1000 * Attempt);
        end;
      end;
    end;
  end;

  // Échec après tous les essais
  raise Exception.CreateFmt('Échec après %d tentatives: %s',
    [AMaxRetries, LastError]);
end;

// Utilisation
var
  Response: string;
begin
  Response := CallAPIWithRetry(
    function: string
    begin
      Result := FOpenAI.SendMessage('Hello');
    end
  );
end;
```

### 6.4 Monitoring et statistiques

```pascal
unit uAIStats;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TAPIStats = class
  private
    FTotalCalls: Integer;
    FSuccessfulCalls: Integer;
    FFailedCalls: Integer;
    FTotalTokens: Integer;
    FTotalCost: Double;

    FCallHistory: TList<TDateTime>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RecordCall(ASuccess: Boolean; ATokens: Integer; ACost: Double);

    function GetAverageDuration: Double;
    function GetCallsPerMinute: Double;

    property TotalCalls: Integer read FTotalCalls;
    property SuccessfulCalls: Integer read FSuccessfulCalls;
    property FailedCalls: Integer read FFailedCalls;
    property TotalTokens: Integer read FTotalTokens;
    property TotalCost: Double read FTotalCost;
  end;

implementation

{ TAPIStats }

constructor TAPIStats.Create;  
begin  
  inherited;
  FCallHistory := TList<TDateTime>.Create;
end;

destructor TAPIStats.Destroy;  
begin  
  FCallHistory.Free;
  inherited;
end;

procedure TAPIStats.RecordCall(ASuccess: Boolean; ATokens: Integer; ACost: Double);  
begin  
  Inc(FTotalCalls);

  if ASuccess then
    Inc(FSuccessfulCalls)
  else
    Inc(FFailedCalls);

  Inc(FTotalTokens, ATokens);
  FTotalCost := FTotalCost + ACost;

  FCallHistory.Add(Now);
end;

function TAPIStats.GetCallsPerMinute: Double;  
var  
  RecentCalls: Integer;
  StartTime: TDateTime;
begin
  StartTime := Now - (1 / 24 / 60); // Il y a 1 minute
  RecentCalls := 0;

  for var CallTime in FCallHistory do
  begin
    if CallTime >= StartTime then
      Inc(RecentCalls);
  end;

  Result := RecentCalls;
end;

function TAPIStats.GetAverageDuration: Double;  
begin  
  // À implémenter selon vos besoins
  Result := 0;
end;

end.
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous avez découvert comment intégrer l'intelligence artificielle dans vos applications Delphi. Vous maîtrisez maintenant :

✅ **Concepts IA/ML** : Compréhension des bases  
✅ **APIs cloud** : OpenAI (GPT), Google Vision  
✅ **Chatbots** : Conversations intelligentes avec contexte  
✅ **Computer Vision** : Analyse d'images, OCR, détection  
✅ **Machine Learning** : Prédictions et classifications  
✅ **Intégration** : APIs REST, JSON, async  
✅ **Optimisation** : Cache, retry, monitoring

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Créer des chatbots intelligents  
🎯 Analyser des images automatiquement  
🎯 Faire des prédictions basées sur des données  
🎯 Intégrer des services IA dans vos apps  
🎯 Gérer les appels API efficacement  
🎯 Optimiser les coûts et performances

### Applications pratiques

**Exemples d'applications IA que vous pouvez créer** :

1. **Assistant client intelligent** : Support automatisé 24/7
2. **Analyse de documents** : Extraction automatique d'informations
3. **Modération de contenu** : Filtrage automatique
4. **Prédiction de ventes** : Business intelligence
5. **Classification d'emails** : Tri automatique
6. **Reconnaissance de produits** : E-commerce visuel
7. **Traduction en temps réel** : Communication multilingue
8. **Détection d'anomalies** : Sécurité et surveillance

### Coûts et considérations

**Budget type pour débuter** :

| Service | Coût mensuel | Usage |
|---------|--------------|-------|
| OpenAI GPT-3.5 | 5-20 $ | Chatbot ~1000 messages |
| Google Vision | Gratuit | 1000 images/mois |
| Hébergement | 5-10 $ | Serveur basique |
| **Total** | **10-30 $** | Développement et tests |

**En production** : Adapter selon le volume

### Bonnes pratiques

**Sécurité** :
- ✅ Ne jamais exposer vos clés API
- ✅ Utiliser des variables d'environnement
- ✅ Chiffrer les données sensibles
- ✅ Valider toutes les entrées utilisateur

**Performance** :
- ✅ Implémenter un cache
- ✅ Traiter en arrière-plan
- ✅ Limiter les requêtes (rate limiting)
- ✅ Optimiser les prompts

**Coûts** :
- ✅ Monitorer l'utilisation
- ✅ Définir des limites
- ✅ Utiliser le cache agressivement
- ✅ Choisir le bon modèle (GPT-3.5 vs GPT-4)

### Limitations et éthique

**Limitations techniques** :
- L'IA peut se tromper (hallucinations)
- Dépendance aux services cloud
- Coûts variables selon l'usage
- Latence réseau

**Considérations éthiques** :
- Transparence : Informer les utilisateurs
- Biais : Être conscient des biais possibles
- Vie privée : Protéger les données
- Responsabilité : Supervision humaine

### Ressources complémentaires

**Documentation** :
- [OpenAI API Documentation](https://platform.openai.com/docs)
- [Google Cloud Vision](https://cloud.google.com/vision/docs)
- [Azure Cognitive Services](https://azure.microsoft.com/en-us/services/cognitive-services/)

**Apprentissage** :
- [Coursera ML Course](https://www.coursera.org/learn/machine-learning)
- [Fast.ai](https://www.fast.ai/)
- [Kaggle Learn](https://www.kaggle.com/learn)

**Communautés** :
- r/MachineLearning
- OpenAI Community Forum
- Delphi Forums (section IA)

### Prochaines étapes

**Pour aller plus loin** :

1. **Deep Learning** : TensorFlow, PyTorch
2. **Computer Vision avancée** : Segmentation, pose estimation
3. **NLP avancé** : Fine-tuning de modèles
4. **MLOps** : Déploiement et monitoring
5. **Edge AI** : IA sur appareil (offline)
6. **Reinforcement Learning** : Agents intelligents

### Message final

L'Intelligence Artificielle n'est plus réservée aux géants de la tech. Avec Delphi et les APIs modernes, vous pouvez créer des applications intelligentes impressionnantes rapidement.

L'IA est un outil puissant, mais c'est **votre créativité** qui fait la différence. Pensez aux problèmes que vous pouvez résoudre, aux expériences que vous pouvez améliorer.

N'ayez pas peur d'expérimenter. Commencez petit, testez, itérez. L'IA évolue rapidement, et maintenant vous avez les bases pour évoluer avec elle.

**Bon développement IA avec Delphi !** 🤖🚀

---

⏭️ [Intégration de plateformes de paiement](/19-projets-avances/08-integration-de-plateformes-de-paiement.md)
