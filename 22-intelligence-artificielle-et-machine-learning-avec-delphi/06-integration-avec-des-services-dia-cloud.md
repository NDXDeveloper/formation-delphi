🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.6 Intégration avec des services d'IA cloud (Azure AI, Google AI, etc.)

## Introduction aux services d'IA cloud

### Qu'est-ce qu'un service d'IA cloud ?

Un service d'IA cloud est une API accessible via internet qui vous permet d'utiliser des modèles d'intelligence artificielle sophistiqués sans avoir à les développer, entraîner ou héberger vous-même. C'est comme louer une intelligence artificielle "prête à l'emploi".

**Analogie simple** : Imaginez que vous voulez utiliser de l'électricité. Vous avez deux options :
- Construire votre propre centrale électrique (développer vos propres modèles IA)
- Brancher votre équipement sur le réseau électrique existant (utiliser des services cloud)

Les services d'IA cloud sont comme le réseau électrique : vous payez ce que vous consommez, l'infrastructure est gérée par le fournisseur, et vous bénéficiez instantanément des dernières technologies.

### Pourquoi choisir les services cloud ?

**Rapidité de mise en œuvre** :
Vous pouvez intégrer des fonctionnalités IA en quelques heures au lieu de plusieurs mois de développement.

**Pas d'expertise ML requise** :
Les modèles sont déjà entraînés et optimisés par des experts. Vous n'avez qu'à les utiliser.

**Mise à jour automatique** :
Les fournisseurs améliorent constamment leurs modèles. Vous en bénéficiez automatiquement sans rien changer.

**Scalabilité** :
Gérez 10 ou 10 millions de requêtes sans vous soucier de l'infrastructure.

**Coût prévisible** :
Modèle de paiement à l'usage (pay-as-you-go). Pas d'investissement initial lourd.

**Fiabilité** :
SLA (Service Level Agreement) garantis, haute disponibilité, support technique.

### Les inconvénients à considérer

**Dépendance internet** :
Nécessite une connexion pour fonctionner (sauf certains services hybrides).

**Coûts récurrents** :
Si vous avez de gros volumes, les coûts peuvent augmenter significativement.

**Confidentialité des données** :
Vos données transitent par des serveurs tiers (problématique pour données sensibles).

**Latence** :
Temps de réponse légèrement supérieur aux solutions locales (réseau).

**Dépendance au fournisseur** :
Changement de fournisseur peut nécessiter du travail de migration.

## Les grands acteurs du cloud IA

### Vue d'ensemble comparative

| Fournisseur | Points forts | Idéal pour | Pricing |
|------------|--------------|------------|---------|
| **Google Cloud AI** | Vision, NLP, qualité modèles | Applications grand public | Compétitif, généreux en gratuit |
| **Azure AI** | Intégration écosystème MS | Entreprises Microsoft | Moyen, crédits gratuits |
| **AWS AI** | Scalabilité, infrastructure | Gros volumes, scaling | Variable selon service |
| **OpenAI** | GPT-4, modèles génératifs | Chatbots, génération contenu | Premium mais puissant |
| **IBM Watson** | Secteur entreprise, compliance | Grandes entreprises | Premium |
| **Hugging Face** | Open source, communauté | Développeurs, recherche | Freemium |

## Google Cloud AI Platform

### Présentation

Google Cloud AI est la plateforme d'intelligence artificielle de Google, bénéficiant de l'expertise du géant du web en matière d'IA. Elle offre des services de reconnaissance d'images, traitement du langage, traduction, et bien plus.

### Services principaux

**Cloud Vision API** :
- Analyse d'images et détection d'objets
- OCR (extraction de texte)
- Détection de visages et émotions
- Reconnaissance de logos et monuments
- Détection de contenu inapproprié

**Cloud Natural Language API** :
- Analyse de sentiments
- Extraction d'entités nommées
- Analyse syntaxique
- Classification de contenu

**Cloud Translation API** :
- Traduction automatique
- Plus de 100 langues supportées
- Détection de langue

**Cloud Speech-to-Text / Text-to-Speech** :
- Reconnaissance vocale
- Synthèse vocale

**Dialogflow** :
- Création de chatbots et assistants vocaux
- Gestion du dialogue conversationnel

**AutoML** :
- Entraînement de modèles personnalisés sans expertise ML

### Configuration initiale

**1. Créer un compte Google Cloud** :
- Rendez-vous sur https://cloud.google.com
- Inscrivez-vous (300$ de crédits gratuits pour débuter)
- Créez un projet

**2. Activer les API** :
- Dans la console, accédez à "API & Services"
- Activez les API dont vous avez besoin (Vision, Natural Language, etc.)

**3. Créer des identifiants** :
- Générez une clé API (pour tests)
- Ou créez un compte de service (pour production)

**4. Sécuriser la clé** :
- Ne jamais commiter la clé dans le code source
- Utiliser des variables d'environnement ou configuration sécurisée

### Intégration avec Delphi

**Configuration des composants REST** :

```pascal
unit GoogleCloudAI;

interface

uses
  System.SysUtils, System.Classes, REST.Client, REST.Types,
  System.JSON, System.NetEncoding;

type
  TGoogleCloudVision = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAPIKey: string;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function AnalyserImage(const CheminImage: string): TJSONObject;
    function ExtraireTexte(const CheminImage: string): string;
    function DetecterVisages(const CheminImage: string): TJSONArray;
  end;

implementation

constructor TGoogleCloudVision.Create(const APIKey: string);  
begin  
  inherited Create;
  FAPIKey := APIKey;

  FRESTClient := TRESTClient.Create('https://vision.googleapis.com');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

destructor TGoogleCloudVision.Destroy;  
begin  
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;
  inherited;
end;

function TGoogleCloudVision.AnalyserImage(const CheminImage: string): TJSONObject;  
var  
  FileStream: TFileStream;
  MemStream: TMemoryStream;
  Base64Image: string;
  RequestBody: TJSONObject;
  RequestArray: TJSONArray;
  ImageObj: TJSONObject;
  FeaturesArray: TJSONArray;
  FeatureObj: TJSONObject;
begin
  // 1. Charger et encoder l'image en Base64
  MemStream := TMemoryStream.Create;
  FileStream := TFileStream.Create(CheminImage, fmOpenRead);
  try
    MemStream.CopyFrom(FileStream, FileStream.Size);
    MemStream.Position := 0;
    Base64Image := TNetEncoding.Base64.EncodeBytesToString(
      MemStream.Memory, MemStream.Size);
  finally
    FileStream.Free;
    MemStream.Free;
  end;

  // 2. Construire la requête JSON
  RequestArray := TJSONArray.Create;
  RequestBody := TJSONObject.Create;
  ImageObj := TJSONObject.Create;
  FeaturesArray := TJSONArray.Create;

  try
    // Image
    ImageObj.AddPair('content', Base64Image);

    // Features demandées
    FeatureObj := TJSONObject.Create;
    FeatureObj.AddPair('type', 'LABEL_DETECTION');
    FeatureObj.AddPair('maxResults', TJSONNumber.Create(10));
    FeaturesArray.AddElement(FeatureObj);

    // Assemblage
    RequestBody.AddPair('image', ImageObj);
    RequestBody.AddPair('features', FeaturesArray);
    RequestArray.AddElement(RequestBody);

    // 3. Configurer la requête
    FRESTRequest.Resource := 'v1/images:annotate';
    FRESTRequest.AddParameter('key', FAPIKey, pkGETorPOST);
    FRESTRequest.Method := rmPOST;
    FRESTRequest.ClearBody;

    FRESTRequest.Body.Add(
      '{"requests":' + RequestArray.ToString + '}',
      TRESTContentType.ctAPPLICATION_JSON
    );

    // 4. Exécuter
    FRESTRequest.Execute;

    // 5. Retourner le résultat
    if FRESTResponse.StatusCode = 200 then
      Result := FRESTResponse.JSONValue as TJSONObject
    else
      raise Exception.CreateFmt('Erreur API: %d - %s',
        [FRESTResponse.StatusCode, FRESTResponse.Content]);
  finally
    RequestArray.Free;
  end;
end;

function TGoogleCloudVision.ExtraireTexte(const CheminImage: string): string;  
var  
  Response: TJSONObject;
  Responses: TJSONArray;
  TextAnnotations: TJSONArray;
begin
  Result := '';

  // Modifier pour demander TEXT_DETECTION
  Response := AnalyserImage(CheminImage);
  try
    Responses := Response.GetValue<TJSONArray>('responses');
    if Responses.Count > 0 then
    begin
      TextAnnotations := Responses.Items[0].GetValue<TJSONArray>('textAnnotations');
      if (TextAnnotations <> nil) and (TextAnnotations.Count > 0) then
        Result := TextAnnotations.Items[0].GetValue<string>('description');
    end;
  finally
    Response.Free;
  end;
end;
```

**Utilisation dans votre application** :

```pascal
procedure TFormPrincipal.BtnAnalyserImageClick(Sender: TObject);  
var  
  Vision: TGoogleCloudVision;
  Resultat: TJSONObject;
  Labels: TJSONArray;
  i: Integer;
  Label_: TJSONObject;
begin
  Vision := TGoogleCloudVision.Create('VOTRE_CLE_API');
  try
    // Analyser l'image
    Resultat := Vision.AnalyserImage(EditCheminImage.Text);
    try
      // Extraire les labels
      Labels := Resultat.GetValue<TJSONArray>('responses[0].labelAnnotations');

      MemoResultats.Lines.Clear;
      MemoResultats.Lines.Add('Objets détectés :');

      for i := 0 to Labels.Count - 1 do
      begin
        Label_ := Labels.Items[i] as TJSONObject;
        MemoResultats.Lines.Add(Format('- %s (%.0f%%)', [
          Label_.GetValue<string>('description'),
          Label_.GetValue<Double>('score') * 100
        ]));
      end;
    finally
      Resultat.Free;
    end;
  finally
    Vision.Free;
  end;
end;
```

### Tarification Google Cloud AI

**Niveau gratuit** :
- Cloud Vision : 1000 requêtes/mois
- Natural Language : 5000 requêtes/mois
- Translation : 500 000 caractères/mois

**Au-delà** :
- Vision : ~1,50€ / 1000 images
- NLP : ~1€ / 1000 requêtes
- Translation : ~20€ / million de caractères

## Microsoft Azure AI Services

### Présentation

Azure AI Services (anciennement Cognitive Services) est la suite d'IA de Microsoft, parfaitement intégrée à l'écosystème Azure. Excellente pour les entreprises déjà dans l'environnement Microsoft.

### Services principaux

**Computer Vision** :
- Analyse d'images avancée
- OCR multilingue
- Reconnaissance de formes manuscrites
- Analyse spatiale

**Face API** :
- Détection et reconnaissance faciale
- Détection d'émotions
- Vérification et identification

**Text Analytics** :
- Analyse de sentiments
- Extraction de phrases clés
- Reconnaissance d'entités nommées
- Détection de langue

**Translator** :
- Traduction de texte
- Translittération
- Détection de langue

**Speech Services** :
- Speech-to-Text
- Text-to-Speech
- Traduction vocale

**Language Understanding (LUIS)** :
- Compréhension du langage naturel
- Détection d'intentions

**Azure OpenAI Service** :
- Accès aux modèles GPT-4, GPT-3.5
- DALL-E pour génération d'images
- Whisper pour transcription audio

### Configuration initiale

**1. Créer un compte Azure** :
- Rendez-vous sur https://azure.microsoft.com
- Inscription avec 200$ de crédits gratuits
- Créez un groupe de ressources

**2. Créer une ressource Cognitive Services** :
- Dans le portail Azure
- "Créer une ressource" → "AI + Machine Learning" → "Cognitive Services"
- Sélectionnez la région (choisir Europe West pour la France)

**3. Récupérer les clés** :
- Dans la ressource créée, section "Keys and Endpoint"
- Notez Key1 et l'endpoint

### Intégration avec Delphi

**Classe wrapper pour Azure Computer Vision** :

```pascal
unit AzureAI;

interface

uses
  System.SysUtils, System.Classes, REST.Client, REST.Types, System.JSON;

type
  TAzureComputerVision = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FSubscriptionKey: string;
    FEndpoint: string;
  public
    constructor Create(const SubscriptionKey, Endpoint: string);
    destructor Destroy; override;

    function AnalyserImage(const URLImage: string): TJSONObject; overload;
    function AnalyserImage(const CheminImage: string): TJSONObject; overload;
    function ExtraireTexteOCR(const CheminImage: string): string;
  end;

implementation

constructor TAzureComputerVision.Create(const SubscriptionKey, Endpoint: string);  
begin  
  inherited Create;
  FSubscriptionKey := SubscriptionKey;
  FEndpoint := Endpoint;

  FRESTClient := TRESTClient.Create(FEndpoint);
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

destructor TAzureComputerVision.Destroy;  
begin  
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;
  inherited;
end;

function TAzureComputerVision.AnalyserImage(const URLImage: string): TJSONObject;  
var  
  RequestBody: TJSONObject;
begin
  // Analyse via URL d'image
  FRESTRequest.Resource := 'vision/v3.2/analyze';
  FRESTRequest.AddParameter('visualFeatures',
    'Categories,Description,Color,Tags,Objects', pkGETorPOST);
  FRESTRequest.AddParameter('Ocp-Apim-Subscription-Key',
    FSubscriptionKey, pkHTTPHEADER, [poDoNotEncode]);

  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  RequestBody := TJSONObject.Create;
  try
    RequestBody.AddPair('url', URLImage);
    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
  finally
    RequestBody.Free;
  end;

  FRESTRequest.Execute;

  if FRESTResponse.StatusCode = 200 then
    Result := FRESTResponse.JSONValue.Clone as TJSONObject
  else
    raise Exception.CreateFmt('Erreur Azure: %d - %s',
      [FRESTResponse.StatusCode, FRESTResponse.Content]);
end;

function TAzureComputerVision.AnalyserImage(const CheminImage: string): TJSONObject;  
var  
  FileStream: TFileStream;
begin
  // Analyse via upload d'image
  FRESTRequest.Resource := 'vision/v3.2/analyze';
  FRESTRequest.AddParameter('visualFeatures',
    'Categories,Description,Color,Tags,Objects', pkGETorPOST);
  FRESTRequest.AddParameter('Ocp-Apim-Subscription-Key',
    FSubscriptionKey, pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('Content-Type',
    'application/octet-stream', pkHTTPHEADER, [poDoNotEncode]);

  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  FileStream := TFileStream.Create(CheminImage, fmOpenRead);
  try
    FRESTRequest.AddBody(FileStream, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
    FRESTRequest.Execute;
  finally
    FileStream.Free;
  end;

  if FRESTResponse.StatusCode = 200 then
    Result := FRESTResponse.JSONValue.Clone as TJSONObject
  else
    raise Exception.CreateFmt('Erreur Azure: %d - %s',
      [FRESTResponse.StatusCode, FRESTResponse.Content]);
end;

function TAzureComputerVision.ExtraireTexteOCR(const CheminImage: string): string;  
var  
  FileStream: TFileStream;
  Response: TJSONObject;
  ReadResults: TJSONArray;
  Lines: TJSONArray;
  i, j: Integer;
begin
  Result := '';

  // Utiliser Read API pour OCR avancé
  FRESTRequest.Resource := 'vision/v3.2/read/analyze';
  FRESTRequest.AddParameter('Ocp-Apim-Subscription-Key',
    FSubscriptionKey, pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('Content-Type',
    'application/octet-stream', pkHTTPHEADER, [poDoNotEncode]);

  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  FileStream := TFileStream.Create(CheminImage, fmOpenRead);
  try
    FRESTRequest.AddBody(FileStream, TRESTContentType.ctAPPLICATION_OCTET_STREAM);
    FRESTRequest.Execute;
  finally
    FileStream.Free;
  end;

  // L'API Read est asynchrone, nécessite un second appel pour récupérer le résultat
  // Pour simplifier, on pourrait implémenter un polling
  // (code simplifié pour l'exemple)
end;
```

**Utilisation** :

```pascal
procedure TFormMain.BtnAnalyserAzureClick(Sender: TObject);  
var  
  Azure: TAzureComputerVision;
  Resultat: TJSONObject;
  Description: string;
  Tags: TJSONArray;
  i: Integer;
begin
  Azure := TAzureComputerVision.Create(
    'VOTRE_SUBSCRIPTION_KEY',
    'https://VOTRE_REGION.api.cognitive.microsoft.com/'
  );
  try
    Resultat := Azure.AnalyserImage(EditImagePath.Text);
    try
      // Description
      Description := Resultat.GetValue<string>('description.captions[0].text');
      LabelDescription.Caption := Description;

      // Tags
      Tags := Resultat.GetValue<TJSONArray>('tags');
      ListBoxTags.Items.Clear;
      for i := 0 to Tags.Count - 1 do
      begin
        ListBoxTags.Items.Add(Format('%s (%.0f%%)', [
          Tags.Items[i].GetValue<string>('name'),
          Tags.Items[i].GetValue<Double>('confidence') * 100
        ]));
      end;
    finally
      Resultat.Free;
    end;
  finally
    Azure.Free;
  end;
end;
```

### Tarification Azure AI

**Niveau gratuit (F0)** :
- Computer Vision : 5000 transactions/mois
- Text Analytics : 5000 transactions/mois
- Translator : 2M caractères/mois

**Niveaux payants (S0, S1, etc.)** :
- Vision : à partir de 0,85€ / 1000 transactions
- Text Analytics : 1,70€ / 1000 requêtes
- Face API : 0,85€ / 1000 transactions

## Amazon Web Services (AWS) AI

### Présentation

AWS propose une suite complète de services d'IA, réputée pour sa scalabilité et son infrastructure robuste. Idéal pour applications nécessitant haute disponibilité et gros volumes.

### Services principaux

**Amazon Rekognition** :
- Analyse d'images et vidéos
- Reconnaissance faciale
- Détection de célébrités
- Modération de contenu

**Amazon Comprehend** :
- Analyse de sentiments
- Extraction d'entités
- Détection de langue
- Classification personnalisée

**Amazon Translate** :
- Traduction automatique
- 75+ langues

**Amazon Transcribe** :
- Speech-to-Text
- Reconnaissance de locuteurs

**Amazon Polly** :
- Text-to-Speech
- Voix naturelles

**Amazon Lex** :
- Chatbots conversationnels
- Même technologie qu'Alexa

**Amazon SageMaker** :
- Plateforme complète ML
- Entraînement et déploiement de modèles personnalisés

### Configuration et intégration

AWS utilise un système d'authentification plus complexe (AWS Signature Version 4), mais il existe des SDK et bibliothèques pour simplifier.

**Approche recommandée pour Delphi** :
1. Utiliser AWS SDK pour .NET via COM Interop
2. Ou créer un micro-service Node.js/Python qui sert d'intermédiaire
3. Ou utiliser des wrappers communautaires Delphi

```pascal
// Exemple conceptuel d'appel à Rekognition via wrapper
function DetecterVisagesAWS(const CheminImage: string): TJSONArray;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ImageBase64: string;
begin
  RESTClient := TRESTClient.Create('https://rekognition.us-east-1.amazonaws.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;

    // AWS nécessite une signature complexe
    // Il est recommandé d'utiliser un wrapper ou SDK

    // Corps de la requête
    ImageBase64 := EncodeImageToBase64(CheminImage);
    // ... Configuration AWS Signature ...

    RESTRequest.Execute;
    Result := RESTResponse.JSONValue.GetValue<TJSONArray>('FaceDetails');
  finally
    RESTResponse.Free;
    RESTRequest.Free;
    RESTClient.Free;
  end;
end;
```

### Tarification AWS AI

**Niveau gratuit (12 mois)** :
- Rekognition : 5000 images/mois
- Comprehend : 50K unités/mois
- Translate : 2M caractères/mois

**Tarifs standard** :
- Rekognition : 1€ / 1000 images
- Comprehend : 0,0001€ par unité
- Translate : 15€ / million de caractères

## OpenAI API

### Présentation

OpenAI propose les modèles de langage les plus avancés au monde, notamment GPT-4, DALL-E pour la génération d'images, et Whisper pour la transcription audio.

### Services disponibles

**GPT-4 / GPT-3.5** :
- Génération de texte
- Compréhension du langage
- Traduction, résumés
- Code generation
- GPT-4 Vision (analyse d'images)

**DALL-E 3** :
- Génération d'images à partir de descriptions
- Édition d'images

**Whisper** :
- Transcription audio
- Traduction audio

**Text-to-Speech** :
- Voix naturelles de haute qualité

**Embeddings** :
- Création de vecteurs sémantiques pour recherche

### Intégration avec Delphi

```pascal
unit OpenAIAPI;

interface

uses
  System.SysUtils, System.Classes, REST.Client, REST.Types, System.JSON;

type
  TOpenAI = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FAPIKey: string;
  public
    constructor Create(const APIKey: string);
    destructor Destroy; override;

    function Chat(const Prompt: string; const Model: string = 'gpt-3.5-turbo'): string;
    function AnalyserImage(const URLImage: string; const Question: string): string;
    function GenererImage(const Description: string): string; // Retourne URL
  end;

implementation

constructor TOpenAI.Create(const APIKey: string);  
begin  
  inherited Create;
  FAPIKey := APIKey;

  FRESTClient := TRESTClient.Create('https://api.openai.com');
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

destructor TOpenAI.Destroy;  
begin  
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;
  inherited;
end;

function TOpenAI.Chat(const Prompt: string; const Model: string): string;  
var  
  RequestBody: TJSONObject;
  Messages: TJSONArray;
  Message: TJSONObject;
  Choices: TJSONArray;
begin
  FRESTRequest.Resource := 'v1/chat/completions';
  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  // Header d'authentification
  FRESTRequest.Params.Clear;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey,
    pkHTTPHEADER, [poDoNotEncode]);
  FRESTRequest.AddParameter('Content-Type', 'application/json',
    pkHTTPHEADER, [poDoNotEncode]);

  // Corps de la requête
  RequestBody := TJSONObject.Create;
  Messages := TJSONArray.Create;
  Message := TJSONObject.Create;
  try
    Message.AddPair('role', 'user');
    Message.AddPair('content', Prompt);
    Messages.AddElement(Message);

    RequestBody.AddPair('model', Model);
    RequestBody.AddPair('messages', Messages);
    RequestBody.AddPair('temperature', TJSONNumber.Create(0.7));

    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
  finally
    RequestBody.Free;
  end;

  FRESTRequest.Execute;

  if FRESTResponse.StatusCode = 200 then
  begin
    Choices := FRESTResponse.JSONValue.GetValue<TJSONArray>('choices');
    Result := Choices.Items[0].GetValue<string>('message.content');
  end
  else
    raise Exception.CreateFmt('Erreur OpenAI: %d - %s',
      [FRESTResponse.StatusCode, FRESTResponse.Content]);
end;

function TOpenAI.AnalyserImage(const URLImage: string; const Question: string): string;  
var  
  RequestBody: TJSONObject;
  Messages: TJSONArray;
  Message: TJSONObject;
  Content: TJSONArray;
  TextPart, ImagePart: TJSONObject;
  ImageURL: TJSONObject;
begin
  // GPT-4 Vision
  FRESTRequest.Resource := 'v1/chat/completions';
  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  FRESTRequest.Params.Clear;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey,
    pkHTTPHEADER, [poDoNotEncode]);

  // Construire le message avec image
  RequestBody := TJSONObject.Create;
  Messages := TJSONArray.Create;
  Message := TJSONObject.Create;
  Content := TJSONArray.Create;
  try
    // Partie texte
    TextPart := TJSONObject.Create;
    TextPart.AddPair('type', 'text');
    TextPart.AddPair('text', Question);
    Content.AddElement(TextPart);

    // Partie image
    ImagePart := TJSONObject.Create;
    ImageURL := TJSONObject.Create;
    ImageURL.AddPair('url', URLImage);
    ImagePart.AddPair('type', 'image_url');
    ImagePart.AddPair('image_url', ImageURL);
    Content.AddElement(ImagePart);

    Message.AddPair('role', 'user');
    Message.AddPair('content', Content);
    Messages.AddElement(Message);

    RequestBody.AddPair('model', 'gpt-4-vision-preview');
    RequestBody.AddPair('messages', Messages);
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(300));

    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
  finally
    RequestBody.Free;
  end;

  FRESTRequest.Execute;

  if FRESTResponse.StatusCode = 200 then
    Result := FRESTResponse.JSONValue.GetValue<string>('choices[0].message.content')
  else
    raise Exception.CreateFmt('Erreur: %s', [FRESTResponse.Content]);
end;
```

**Utilisation - Chatbot avec GPT** :

```pascal
procedure TFormChat.BtnEnvoyerClick(Sender: TObject);  
var  
  OpenAI: TOpenAI;
  Reponse: string;
  UserMessage: string;
begin
  UserMessage := EditMessage.Text;
  if UserMessage.Trim.IsEmpty then Exit;

  // Afficher le message de l'utilisateur
  MemoChat.Lines.Add('Vous: ' + UserMessage);
  EditMessage.Clear;

  // Désactiver pendant le traitement
  BtnEnvoyer.Enabled := False;
  ProgressBar.Visible := True;

  // Traitement asynchrone
  TTask.Run(procedure
  var
    AI: TOpenAI;
    Response: string;
  begin
    AI := TOpenAI.Create('VOTRE_CLE_API');
    try
      Response := AI.Chat(UserMessage, 'gpt-3.5-turbo');

      TThread.Synchronize(nil, procedure
      begin
        MemoChat.Lines.Add('Assistant: ' + Response);
        MemoChat.Lines.Add('');
        BtnEnvoyer.Enabled := True;
        ProgressBar.Visible := False;
      end);
    finally
      AI.Free;
    end;
  end);
end;
```

### Tarification OpenAI

**GPT-3.5-Turbo** :
- Input : 0,50$ / million de tokens
- Output : 1,50$ / million de tokens

**GPT-4** :
- Input : 30$ / million de tokens
- Output : 60$ / million de tokens

**GPT-4 Vision** :
- 10$ / million de tokens (variable selon résolution image)

**DALL-E 3** :
- Standard (1024×1024) : 0,040$ par image
- HD (1024×1024) : 0,080$ par image

## Autres services cloud d'IA

### Hugging Face Inference API

**Avantages** :
- Accès à des milliers de modèles open source
- Gratuit avec limitations, payant pour volumes importants
- Communauté active

**Cas d'usage** :
- Expérimentation avec différents modèles
- Modèles spécialisés (langues rares, domaines spécifiques)

### IBM Watson

**Points forts** :
- Focus entreprise et conformité
- Excellente documentation
- Support professionnel

**Services** :
- Watson Natural Language Understanding
- Watson Speech to Text
- Watson Discovery

### Anthropic Claude

**Caractéristiques** :
- Concurrent de GPT-4
- Excellente compréhension contextuelle
- Fenêtre de contexte très large (200K tokens)

**Utilisation** : API similaire à OpenAI

## Gestion des coûts

### Stratégies d'optimisation

**1. Mise en cache agressive**

```pascal
type
  TCacheAPI = class
  private
    FCache: TDictionary<string, string>;
    FDureeValidite: TDateTime;
  public
    function ObtenirOuAppeler(const Cle: string;
      const FonctionAPI: TFunc<string>): string;
  end;

function TCacheAPI.ObtenirOuAppeler(const Cle: string;
  const FonctionAPI: TFunc<string>): string;
begin
  if FCache.ContainsKey(Cle) then
    Result := FCache[Cle]
  else
  begin
    Result := FonctionAPI();
    FCache.Add(Cle, Result);
  end;
end;
```

**2. Traitement par lots**

Groupez les requêtes quand c'est possible pour réduire les appels API.

**3. Compression des données**

Réduisez la taille des images avant envoi.

```pascal
procedure OptimiserImagePourAPI(var Bitmap: TBitmap);  
const  
  MAX_DIMENSION = 800; // pixels
var
  Ratio: Double;
begin
  if (Bitmap.Width > MAX_DIMENSION) or (Bitmap.Height > MAX_DIMENSION) then
  begin
    Ratio := Min(MAX_DIMENSION / Bitmap.Width, MAX_DIMENSION / Bitmap.Height);
    Bitmap.SetSize(Round(Bitmap.Width * Ratio), Round(Bitmap.Height * Ratio));
  end;
end;
```

**4. Choisir le bon niveau de service**

- Utilisez les modèles "lite" ou "standard" quand possible
- GPT-3.5 au lieu de GPT-4 pour tâches simples
- Prétraitez localement quand possible

**5. Monitoring et alertes**

```pascal
type
  TAPIUsageMonitor = class
  private
    FUsageAujourdhui: Integer;
    FLimiteQuotidienne: Integer;
    procedure VerifierLimite;
  public
    procedure IncrementerUsage(const Cout: Double);
    function PeutEffectuerAppel: Boolean;
  end;

procedure TAPIUsageMonitor.IncrementerUsage(const Cout: Double);  
begin  
  FUsageAujourdhui := FUsageAujourdhui + Round(Cout * 100);
  VerifierLimite;

  if FUsageAujourdhui >= FLimiteQuotidienne * 0.8 then
    // Alerte : 80% de la limite atteinte
    EnvoyerAlerteAdministrateur('Limite API proche');
end;
```

### Estimation des coûts

**Exemple pour une application de chatbot** :

```
Utilisateurs : 1000  
Messages moyens par jour : 5  
Tokens moyens par message : 150 (input + output)  

Total tokens/jour = 1000 × 5 × 150 = 750 000 tokens  
Total tokens/mois = 750 000 × 30 = 22,5 millions  

Coût GPT-3.5 :
- 22,5M tokens ≈ 11,25$ input + 33,75$ output = 45$/mois

Coût GPT-4 :
- 22,5M tokens ≈ 675$ input + 1350$ output = 2025$/mois

→ GPT-3.5 est 45× moins cher !
```

## Sécurité et confidentialité

### Protection des clés API

**JAMAIS dans le code source** :

```pascal
// ❌ MAUVAIS
const
  API_KEY = 'sk-1234567890abcdef';

// ✅ BON
function ObtenirCleAPI: string;  
begin  
  // Lire depuis configuration chiffrée
  Result := ConfigurationManager.GetEncryptedValue('OpenAI_Key');
end;
```

**Utiliser des variables d'environnement** :

```pascal
function ObtenirCleAPIDepuisEnvironnement: string;  
begin  
  Result := GetEnvironmentVariable('OPENAI_API_KEY');
  if Result.IsEmpty then
    raise Exception.Create('Clé API non configurée');
end;
```

### Gestion des données sensibles

**Anonymisation** :

```pascal
function AnonymiserTexte(const Texte: string): string;  
begin  
  Result := Texte;

  // Remplacer emails
  Result := TRegEx.Replace(Result, '\b[\w\.-]+@[\w\.-]+\.\w+\b', '[EMAIL]');

  // Remplacer numéros de téléphone
  Result := TRegEx.Replace(Result, '\b\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}\b', '[TÉLÉPHONE]');

  // Remplacer IBAN
  Result := TRegEx.Replace(Result, '\b[A-Z]{2}\d{2}[\s]?[\d\s]{20,}\b', '[IBAN]');
end;

procedure EnvoyerAuServiceIA(const Texte: string);  
var  
  TexteAnonyme: string;
begin
  TexteAnonyme := AnonymiserTexte(Texte);
  // Maintenant envoyer à l'API
end;
```

### Conformité RGPD

**Informer les utilisateurs** :

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  if not ConfigManager.GetValue('ConsentementIA', False) then
  begin
    if MessageDlg(
      'Cette application utilise des services d''IA cloud. ' +
      'Vos données seront traitées par des serveurs tiers. ' +
      'Acceptez-vous ?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ConfigManager.SetValue('ConsentementIA', True);
    end
    else
    begin
      // Désactiver les fonctionnalités IA
      BtnAnalyserIA.Enabled := False;
    end;
  end;
end;
```

**Droit à l'effacement** :

Documentez comment supprimer les données déjà envoyées (la plupart des API ne stockent pas indéfiniment).

## Comparaison et choix du service

### Matrice de décision

**Utilisez Google Cloud AI si** :
- Vous voulez le meilleur rapport qualité/prix
- Vous travaillez beaucoup avec images et texte
- Vous appréciez la simplicité d'intégration

**Utilisez Azure AI si** :
- Vous êtes dans l'écosystème Microsoft
- Vous avez besoin d'intégration avec Azure
- Vous voulez accès à Azure OpenAI Service

**Utilisez AWS AI si** :
- Vous avez déjà de l'infrastructure AWS
- Vous avez besoin de haute scalabilité
- Vous voulez SageMaker pour ML personnalisé

**Utilisez OpenAI directement si** :
- Vous voulez les meilleurs modèles de langage (GPT-4)
- Vous développez un chatbot avancé
- Le coût n'est pas la première priorité

**Approche hybride recommandée** :
- Google Vision pour analyse d'images (rapport qualité/prix)
- OpenAI pour chatbot et NLP avancé (qualité)
- Azure pour intégration entreprise Microsoft

## Conclusion

Les services d'IA cloud transforment radicalement ce qui est possible avec Delphi. En quelques heures, vous pouvez intégrer des capacités qui auraient nécessité des mois de développement il y a encore quelques années.

**Points essentiels** :
- Les services cloud offrent des capacités IA de pointe sans expertise ML
- Delphi s'intègre parfaitement via TRESTClient
- Le coût est maîtrisable avec cache et optimisation
- Choisissez le service selon vos besoins spécifiques
- Sécurité et confidentialité sont critiques

**Recommandations pour démarrer** :
1. Commencez avec les niveaux gratuits pour expérimenter
2. Créez des wrappers réutilisables pour vos API favorites
3. Implémentez cache et monitoring dès le début
4. Testez plusieurs services pour comparer
5. Documentez vos choix et coûts

Dans la section suivante, nous explorerons spécifiquement l'intégration des grands modèles de langage (LLM) comme GPT-4, qui révolutionnent les interfaces conversationnelles et la génération de contenu !

⏭️ [Utilisation des grands modèles de langage (LLM) via API](/22-intelligence-artificielle-et-machine-learning-avec-delphi/07-utilisation-des-grands-modeles-de-langage-via-api.md)
