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
| **OpenAI** | GPT-4o/GPT-5, modèles génératifs | Chatbots, génération contenu | Premium mais puissant |
| **Anthropic** | Claude (long contexte, code) | Documents longs, code, raisonnement | Premium |
| **IBM watsonx** | Secteur entreprise, gouvernance, hybride | Grandes entreprises, secteurs régulés | Premium |
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

**Conversational Agents (Dialogflow CX + Vertex AI Agent Builder)** :
- Création de chatbots et assistants vocaux
- Gestion du dialogue conversationnel (flows / pages)
- ⚠️ **Dialogflow ES** est en mode "frozen" depuis 2024 — toutes les nouvelles fonctionnalités vont sur **Dialogflow CX**, désormais accessible via la console unifiée **Conversational Agents** (l'ancienne console standalone Dialogflow CX a été dépréciée le 31 octobre 2025)
- Pour les nouveaux projets : utiliser directement Conversational Agents / Dialogflow CX

**Vertex AI** (plateforme ML unifiée) :
- Entraînement de modèles personnalisés (AutoML inclus)
- Déploiement et serving de modèles
- Pipelines ML, model registry, monitoring
- Remplace progressivement les anciens services AI Platform / AutoML Tables

### Configuration initiale

**1. Créer un compte Google Cloud** :
- Rendez-vous sur [cloud.google.com](https://cloud.google.com)
- Inscrivez-vous (300$ de crédits Welcome gratuits, valables 90 jours)
- Créez un projet

**2. Activer les API** :
- Dans la console, accédez à "API & Services" → "Bibliothèque"
- Activez les API dont vous avez besoin (Cloud Vision API, Cloud Natural Language API, etc.)
- ⚠️ Chaque API doit être activée individuellement pour le projet

**3. Créer des identifiants** :
- Pour des tests/prototypes : générez une **clé API** (rapide à mettre en place)
- Pour la production : créez un **compte de service** (service account) avec un fichier JSON de credentials et les rôles minimaux nécessaires
- Pour des applications utilisateur : **OAuth 2.0** si vos utilisateurs doivent s'authentifier

**4. Sécuriser la clé** :
- Ne jamais commiter la clé dans le code source ou dans un dépôt Git
- Utiliser des variables d'environnement, Secret Manager, ou Configuration chiffrée
- Restreindre la clé API : limitez-la à votre adresse IP et aux API utilisées dans la console GCP

### Intégration avec Delphi

> 💡 **Alternatives officielles via GetIt** : Avant de coder votre propre wrapper, vérifiez si une bibliothèque existe déjà :  
> - **SmartCore AI Component Pack** (officiel Embarcadero) — supporte OpenAI, Claude, Gemini, Ollama  
> - **OpenAI for Delphi** (communautaire, GetIt) — wrapper REST OpenAI (ChatGPT, DALL-E)  
> - **Anthropic API wrapper for Delphi** (communautaire, GetIt) — wrapper Claude avec vision et MCP  
>  
> Le code ci-dessous reste utile comme exemple d'apprentissage ou pour des cas d'usage spécifiques.

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
  // 1. Charger et encoder l'image en Base64.
  // ⚠️ Pattern try/finally imbriqué : si TFileStream.Create échoue (fichier
  // introuvable, droits…), MemStream resterait non libéré sans cette structure.
  MemStream := TMemoryStream.Create;
  try
    FileStream := TFileStream.Create(CheminImage, fmOpenRead);
    try
      MemStream.CopyFrom(FileStream, FileStream.Size);
      MemStream.Position := 0;
      Base64Image := TNetEncoding.Base64.EncodeBytesToString(
        MemStream.Memory, MemStream.Size);
    finally
      FileStream.Free;
    end;
  finally
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

    // 5. Retourner le résultat.
    // ⚠️ FRESTResponse.JSONValue appartient à FRESTResponse : on doit le
    //    **cloner** avant de le retourner, sinon l'appelant qui ferait `.Free`
    //    libérerait un objet géré par le composant, et la requête suivante
    //    crasherait (référence dangling). Pattern identique à TAzureComputerVision.
    if FRESTResponse.StatusCode = 200 then
    begin
      // Garde nil sur JSONValue (réponse non-JSON possible via proxy)
      if not Assigned(FRESTResponse.JSONValue) then
        raise Exception.Create('Réponse Google Vision : JSON invalide ou vide');
      Result := FRESTResponse.JSONValue.Clone as TJSONObject;
    end
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

  // ⚠️ Cette méthode suppose qu'AnalyserImage soit adaptée pour demander
  //    `TEXT_DETECTION` (ou `DOCUMENT_TEXT_DETECTION` pour des documents
  //    avec mise en page complexe) au lieu de `LABEL_DETECTION`. Sans cette
  //    adaptation, le champ `textAnnotations` est absent et Result restera
  //    vide. Exemple : remplacer 'LABEL_DETECTION' par 'TEXT_DETECTION' dans
  //    le FeatureObj de AnalyserImage avant d'appeler cette méthode.
  Response := AnalyserImage(CheminImage);
  try
    Responses := Response.GetValue<TJSONArray>('responses');
    if (Responses = nil) or (Responses.Count = 0) then
      Exit;

    // ⚠️ `TJSONArray.Items[i]` retourne un `TJSONValue` ; on doit caster en
    //    `TJSONObject` pour appeler `GetValue<T>`. Sans cast, le compilateur
    //    refuse l'appel `GetValue<TJSONArray>` (méthode définie sur TJSONObject).
    TextAnnotations := (Responses.Items[0] as TJSONObject)
                         .GetValue<TJSONArray>('textAnnotations');
    if (TextAnnotations <> nil) and (TextAnnotations.Count > 0) then
      // Premier élément = texte complet (les suivants sont les mots individuels)
      Result := (TextAnnotations.Items[0] as TJSONObject)
                  .GetValue<string>('description');
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
      // ⚠️ Extraire les labels via FindValue qui supporte les paths JSONPath
      // (`GetValue<T>` ne supporte que les clés simples, pas `responses[0]...`).
      Labels := Resultat.FindValue('responses[0].labelAnnotations') as TJSONArray;

      MemoResultats.Lines.Clear;
      if not Assigned(Labels) then
      begin
        MemoResultats.Lines.Add('Aucun objet détecté ou format de réponse inattendu.');
        Exit;
      end;
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

### Tarification Google Cloud AI (ordres de grandeur)

> ⚠️ Tarifs en USD, susceptibles d'évoluer. Consultez [cloud.google.com/pricing](https://cloud.google.com/pricing) pour les chiffres à jour.

**Niveau gratuit** :
- Cloud Vision : 1000 requêtes/mois **par feature** (LABEL_DETECTION, FACE_DETECTION…)
- Natural Language : 5000 unités/mois
- Translation : 500 000 caractères/mois

**Au-delà** :
- Vision : ~1,50$ / 1000 unités (jusqu'à 5M/mois)
- NLP : ~1$ / 1000 unités
- Translation : ~20$ / million de caractères (modèle NMT standard)

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
- Détection de visages (présence, position, attributs publics)
- Reconnaissance faciale (identification/vérification) — **accès limité depuis juin 2022**, demande Microsoft requise
- ⚠️ La **détection d'émotion et de genre a été retirée** par Microsoft en 2022 dans le cadre de ses engagements Responsible AI

**Text Analytics** (intégré au service **Azure AI Language**) :
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

**Conversational Language Understanding (CLU)** :
- Compréhension du langage naturel
- Détection d'intentions
- ⚠️ **Remplace LUIS** qui a été **complètement retiré le 31 mars 2026**. Toutes les nouvelles applications doivent utiliser CLU. Migration officielle : [docs.microsoft.com/azure/ai-services/language-service/conversational-language-understanding/how-to/migrate-from-luis](https://learn.microsoft.com/en-us/azure/ai-services/language-service/conversational-language-understanding/how-to/migrate-from-luis)

**Azure OpenAI Service** :
- Accès aux modèles OpenAI (famille GPT-4o, GPT-5, gpt-3.5-turbo legacy)
- ⚠️ DALL-E retiré le 12 mai 2026 ; utiliser `gpt-image-1` pour la génération d'images
- Whisper pour transcription audio (Speech-to-Text)
- Embeddings (`text-embedding-3-small`, `text-embedding-3-large`) pour la recherche sémantique / RAG
- Déploiement zonal : les modèles disponibles dépendent de la région Azure choisie

### Configuration initiale

**1. Créer un compte Azure** :
- Rendez-vous sur [azure.microsoft.com](https://azure.microsoft.com)
- Inscription avec ~200$ de crédits gratuits pour les nouveaux comptes
- Créez un groupe de ressources

**2. Créer une ressource Azure AI services** :
- Dans le portail Azure, "Créer une ressource"
- Catégorie : **Azure AI services** → ressource multi-service **Azure AI services** (anciennement Cognitive Services multi-service)
- Sélectionnez la région (utilisez **West Europe** ou **France Central** pour les utilisateurs francophones)
- Choisissez un tier (F0 gratuit avec quotas limités, ou S0 payant)

> ℹ️ Pour les nouveaux projets centrés sur l'IA générative, Microsoft recommande désormais **Azure AI Foundry** (anciennement Azure AI Studio) qui unifie l'accès aux modèles GenAI, agents et outils. Documentation : [learn.microsoft.com/azure/ai-foundry](https://learn.microsoft.com/en-us/azure/ai-foundry/).

**3. Récupérer les clés** :
- Dans la ressource créée, section **Keys and Endpoint** (gauche)
- Notez **Key1** (ou Key2) et l'**endpoint** (URL spécifique à votre région)
- ⚠️ Stockez les clés de manière sécurisée (Key Vault, variables d'environnement) — JAMAIS dans le code source

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

    // ⚠️ Deux méthodes distinctes plutôt que deux `overload` : le compilateur
    // Delphi ne peut pas distinguer deux surcharges qui diffèrent uniquement
    // par le nom du paramètre (les deux signatures sont `(const _: string)`,
    // identiques pour la résolution de surcharge).
    function AnalyserImageDepuisURL(const URLImage: string): TJSONObject;
    function AnalyserImageDepuisFichier(const CheminImage: string): TJSONObject;
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

function TAzureComputerVision.AnalyserImageDepuisURL(const URLImage: string): TJSONObject;  
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
  begin
    // Garde nil : JSONValue peut être nil si la réponse n'est pas JSON
    if not Assigned(FRESTResponse.JSONValue) then
      raise Exception.Create('Réponse Azure : JSON invalide ou vide');
    Result := FRESTResponse.JSONValue.Clone as TJSONObject;
  end
  else
    raise Exception.CreateFmt('Erreur Azure: %d - %s',
      [FRESTResponse.StatusCode, FRESTResponse.Content]);
end;

function TAzureComputerVision.AnalyserImageDepuisFichier(const CheminImage: string): TJSONObject;  
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
  begin
    // Garde nil : JSONValue peut être nil si la réponse n'est pas JSON
    if not Assigned(FRESTResponse.JSONValue) then
      raise Exception.Create('Réponse Azure : JSON invalide ou vide');
    Result := FRESTResponse.JSONValue.Clone as TJSONObject;
  end
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

  // Utiliser Read API pour OCR avancé.
  // ⚠️ L'API v3.2 reste disponible mais les versions 1.0 à 3.1 seront **retirées
  // le 13 septembre 2026**. Pour les nouveaux projets, Microsoft recommande
  // l'**Image Analysis 4.0 GA** (endpoint : `imageanalysis:analyze?api-version=2024-02-01`).
  // Voir : https://learn.microsoft.com/en-us/azure/ai-services/computer-vision/whats-new
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
    // Format moderne 2026 (privilégié pour les nouvelles ressources) :
    //   'https://VOTRE_NOM_RESSOURCE.cognitiveservices.azure.com/'
    // Format historique (encore valide) :
    'https://VOTRE_REGION.api.cognitive.microsoft.com/'
  );
  try
    // EditImagePath contient un chemin de fichier local → AnalyserImageDepuisFichier.
    // Pour analyser via une URL distante (image hébergée), utilisez plutôt
    // AnalyserImageDepuisURL(EditURL.Text).
    Resultat := Azure.AnalyserImageDepuisFichier(EditImagePath.Text);
    try
      // ⚠️ FindValue (et non GetValue) pour les paths dotted/indexés
      // Description
      var DescVal := Resultat.FindValue('description.captions[0].text');
      if Assigned(DescVal) then
      begin
        Description := DescVal.Value;
        LabelDescription.Caption := Description;
      end;

      // Tags (clé simple : GetValue<T> fonctionne)
      Tags := Resultat.GetValue<TJSONArray>('tags');
      ListBoxTags.Items.Clear;
      // ⚠️ Garde nil : GetValue<TJSONArray> retourne nil si la clé est absente
      if Assigned(Tags) then
        for i := 0 to Tags.Count - 1 do
        begin
          ListBoxTags.Items.Add(Format('%s (%.0f%%)', [
            (Tags.Items[i] as TJSONObject).GetValue<string>('name'),
            (Tags.Items[i] as TJSONObject).GetValue<Double>('confidence') * 100
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

### Tarification Azure AI (ordres de grandeur)

> ⚠️ Tarifs en USD, susceptibles d'évoluer. Voir [azure.microsoft.com/pricing/details/cognitive-services](https://azure.microsoft.com/pricing/details/cognitive-services/) pour les chiffres à jour.

**Niveau gratuit (F0)** :
- Computer Vision : 5000 transactions/mois
- Azure AI Language (ex-Text Analytics) : 5000 enregistrements/mois
- Translator : 2M caractères/mois

**Niveaux payants (S0, S1, etc.)** :
- Vision : à partir de ~1$ / 1000 transactions selon la feature
- Azure AI Language : ~1$-2$ / 1000 enregistrements selon la feature
- Face API : ~1$ / 1000 transactions (Face Detect/Verify), accès limité requis pour Identify

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

AWS utilise un système d'authentification complexe (**AWS Signature Version 4** sur chaque requête), il n'existe pas de SDK officiel AWS pour Delphi. Plusieurs stratégies possibles :

**Approche recommandée pour Delphi** :
1. **Implémenter SigV4 manuellement** en Delphi (faisable mais fastidieux — algorithme HMAC-SHA256 + sérialisation canonique des headers)
2. **Micro-service intermédiaire** Python/Node.js qui appelle AWS et que Delphi consomme via REST simple (le plus simple en pratique)
3. **AWS SDK pour C++** wrappé dans une DLL exposée à Delphi (similaire au pattern ONNX Runtime)
4. **Wrappers communautaires Delphi** (rechercher `TAWS4D` ou équivalents sur GitHub, qualité variable)
5. **Présigned URLs** : si vous voulez juste uploader/downloader S3, faites générer des URLs présignées côté serveur (qui dispose d'un vrai SDK AWS) et utilisez-les depuis Delphi sans authentification

```pascal
// Exemple conceptuel d'appel à Rekognition via wrapper
// (la partie AWS Signature V4 est omise pour simplifier)
function DetecterVisagesAWS(const CheminImage: string): TJSONArray;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ImageBase64: string;
  FaceDetailsValue: TJSONValue;
  CloneValue: TJSONValue;
begin
  Result := nil;

  // ⚠️ Triple try/finally imbriqué : si l'une des créations échoue, les
  // objets déjà créés doivent être libérés. Sans cette structure, on aurait
  // potentiellement des fuites mémoire.
  RESTClient := TRESTClient.Create('https://rekognition.us-east-1.amazonaws.com');
  try
    RESTRequest := TRESTRequest.Create(nil);
    try
      RESTResponse := TRESTResponse.Create(nil);
      try
        RESTRequest.Client := RESTClient;
        RESTRequest.Response := RESTResponse;

        // AWS nécessite une signature complexe (SigV4 sur chaque requête)
        // Il est recommandé d'utiliser un wrapper, un SDK, ou un proxy

        // Corps de la requête
        ImageBase64 := EncodeImageToBase64(CheminImage);
        // ... Configuration AWS Signature V4 ici ...

        RESTRequest.Execute;

        // ⚠️ Garde nil : si la réponse n'est pas un JSON valide
        //    (erreur HTML d'un proxy, par exemple), JSONValue vaut nil.
        if not Assigned(RESTResponse.JSONValue) then
          Exit;

        // ⚠️ RESTResponse.JSONValue appartient à RESTResponse. On doit
        //    **cloner** le sous-arbre avant de le retourner, sinon la valeur
        //    sera libérée par RESTResponse.Free juste en dessous (référence
        //    dangling côté caller).
        FaceDetailsValue := RESTResponse.JSONValue.FindValue('FaceDetails');
        if Assigned(FaceDetailsValue) and (FaceDetailsValue is TJSONArray) then
        begin
          // Clone explicite avec vérification du type pour éviter EInvalidCast
          // (qui produirait une fuite du Clone si le cast `as` échouait).
          CloneValue := FaceDetailsValue.Clone;
          try
            Result := CloneValue as TJSONArray;
          except
            CloneValue.Free;
            raise;
          end;
        end;
      finally
        RESTResponse.Free;
      end;
    finally
      RESTRequest.Free;
    end;
  finally
    RESTClient.Free;
  end;
end;
```

### Tarification AWS AI

**Niveau gratuit (12 mois)** :
- Rekognition : 5000 images/mois
- Comprehend : 50K unités/mois
- Translate : 2M caractères/mois

**Tarifs standard (ordres de grandeur USD)** :
- Rekognition : ~1$ / 1000 images
- Comprehend : ~0,0001$ par unité
- Translate : ~15$ / million de caractères
- Voir [aws.amazon.com/pricing](https://aws.amazon.com/pricing/) pour les chiffres à jour

## OpenAI API

### Présentation

OpenAI propose des modèles de langage et multimodaux parmi les plus avancés du marché : la série GPT (GPT-4o, GPT-5+), des modèles de génération d'images, et Whisper pour la transcription audio. Les noms et capacités évoluent rapidement ; consultez toujours [platform.openai.com/docs/models](https://platform.openai.com/docs/models) pour la liste à jour.

### Services disponibles

**Famille GPT (chat completions)** :
- Génération de texte
- Compréhension du langage
- Traduction, résumés
- Génération de code
- Vision multimodale (depuis GPT-4o : texte + image natif)
- Modèles courants en 2026 : `gpt-4o`, `gpt-4o-mini`, série `gpt-5*`. `gpt-3.5-turbo` reste disponible mais legacy.

**Génération d'images** :
- ⚠️ `dall-e-2` et `dall-e-3` ont été **retirés de l'API le 12 mai 2026**
- Successeurs actuels : `gpt-image-1` (qualité maximale) et `gpt-image-1-mini` (économique)
- Génération et édition d'images à partir de descriptions

**Whisper** :
- Transcription audio (Speech-to-Text)
- Traduction audio multilingue

**Text-to-Speech** (TTS) :
- Voix naturelles de haute qualité

**Embeddings** :
- Création de vecteurs sémantiques pour recherche / RAG

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

    // Valeur par défaut : `gpt-4o-mini` (économique, multimodal, GA en 2026).
    // `gpt-3.5-turbo` reste accessible mais est désormais considéré comme legacy.
    function Chat(const Prompt: string; const Model: string = 'gpt-4o-mini'): string;
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
  UserMessage: TJSONObject;
  ContentValue: TJSONValue;
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
  UserMessage := TJSONObject.Create;
  try
    UserMessage.AddPair('role', 'user');
    UserMessage.AddPair('content', Prompt);
    Messages.AddElement(UserMessage);

    RequestBody.AddPair('model', Model);
    RequestBody.AddPair('messages', Messages);
    RequestBody.AddPair('temperature', TJSONNumber.Create(0.7));

    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
  finally
    RequestBody.Free; // Libère récursivement Messages et UserMessage
  end;

  FRESTRequest.Execute;

  if FRESTResponse.StatusCode = 200 then
  begin
    // ⚠️ Si la réponse n'est pas un JSON valide (ex: erreur HTML retournée par
    //    un proxy intermédiaire), FRESTResponse.JSONValue vaut nil → AV.
    //    On vérifie systématiquement avant d'accéder à FindValue.
    if not Assigned(FRESTResponse.JSONValue) then
      raise Exception.Create('Réponse OpenAI : JSON invalide ou vide');

    // ⚠️ TJSONObject.GetValue<T>(name) ne supporte PAS les paths dotted.
    //    Pour `choices[0].message.content`, il faut utiliser FindValue qui
    //    implémente la spécification JSONPath simplifiée.
    ContentValue := FRESTResponse.JSONValue.FindValue('choices[0].message.content');
    if Assigned(ContentValue) then
      Result := ContentValue.Value
    else
      raise Exception.Create('Réponse OpenAI : champ "content" introuvable');
  end
  else
    raise Exception.CreateFmt('Erreur OpenAI: %d - %s',
      [FRESTResponse.StatusCode, FRESTResponse.Content]);
end;

function TOpenAI.AnalyserImage(const URLImage: string; const Question: string): string;  
var  
  RequestBody: TJSONObject;
  Messages: TJSONArray;
  Msg: TJSONObject; // évite le nom 'Message' (collision potentielle avec System.SysUtils)
  Content: TJSONArray;
  TextPart, ImagePart: TJSONObject;
  ImageURL: TJSONObject;
begin
  // Vision multimodale via gpt-4o (cf. note plus bas)
  FRESTRequest.Resource := 'v1/chat/completions';
  FRESTRequest.Method := rmPOST;
  FRESTRequest.ClearBody;

  FRESTRequest.Params.Clear;
  FRESTRequest.AddParameter('Authorization', 'Bearer ' + FAPIKey,
    pkHTTPHEADER, [poDoNotEncode]);

  // Construire le message avec image
  RequestBody := TJSONObject.Create;
  Messages := TJSONArray.Create;
  Msg := TJSONObject.Create;
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

    Msg.AddPair('role', 'user');
    Msg.AddPair('content', Content);
    Messages.AddElement(Msg);

    // ⚠️ `gpt-4-vision-preview` a été **déprécié par OpenAI en juin 2024**.
    // Le successeur multimodal officiel est `gpt-4o` (ou `gpt-4o-mini` pour
    // une option plus économique). Adaptez le nom du modèle aux disponibilités
    // actuelles de votre compte : https://platform.openai.com/docs/models
    RequestBody.AddPair('model', 'gpt-4o');
    RequestBody.AddPair('messages', Messages);
    // ℹ️ Sur les modèles GPT-5 et certains modèles récents, le paramètre
    //    `max_tokens` est remplacé par `max_completion_tokens` (cf. doc OpenAI).
    //    `max_tokens` reste accepté par `gpt-4o*` à la date de cet exemple.
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(300));

    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);
  finally
    RequestBody.Free;
  end;

  FRESTRequest.Execute;

  if FRESTResponse.StatusCode = 200 then
  begin
    // ⚠️ Garde nil sur JSONValue (réponse non-JSON possible via proxy)
    if not Assigned(FRESTResponse.JSONValue) then
      raise Exception.Create('Réponse Vision : JSON invalide ou vide');
    // FindValue (et non GetValue) pour les paths JSONPath dotted
    var V := FRESTResponse.JSONValue.FindValue('choices[0].message.content');
    if Assigned(V) then
      Result := V.Value
    else
      raise Exception.Create('Réponse Vision : champ "content" introuvable');
  end
  else
    raise Exception.CreateFmt('Erreur: %s', [FRESTResponse.Content]);
end;
```

**Utilisation - Chatbot avec GPT** :

```pascal
procedure TFormChat.BtnEnvoyerClick(Sender: TObject);  
var  
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
  // ⚠️ UserMessage est capturé par la closure : Delphi crée une copie de la
  // chaîne, donc même si EditMessage est modifié pendant le traitement, la
  // valeur envoyée à l'API reste celle au moment du clic.
  TTask.Run(procedure
  var
    AI: TOpenAI;
    Response: string;
    ErrMsg: string;
  begin
    AI := TOpenAI.Create('VOTRE_CLE_API'); // ⚠️ En prod : lire depuis configuration sécurisée
    try
      try
        // `gpt-4o-mini` : compromis qualité/coût excellent pour un chatbot
        Response := AI.Chat(UserMessage, 'gpt-4o-mini');

        TThread.Synchronize(nil, procedure
        begin
          MemoChat.Lines.Add('Assistant: ' + Response);
          MemoChat.Lines.Add('');
          BtnEnvoyer.Enabled := True;
          ProgressBar.Visible := False;
        end);
      except
        on E: Exception do
        begin
          // ⚠️ Sans cet `except`, une erreur API laisserait BtnEnvoyer désactivé
          //    et ProgressBar visible — l'utilisateur croirait l'app figée.
          ErrMsg := E.Message;
          TThread.Synchronize(nil, procedure
          begin
            MemoChat.Lines.Add('Erreur : ' + ErrMsg);
            MemoChat.Lines.Add('');
            BtnEnvoyer.Enabled := True;
            ProgressBar.Visible := False;
          end);
        end;
      end;
    finally
      AI.Free;
    end;
  end);
end;
```

### Tarification OpenAI

> ⚠️ **La tarification OpenAI évolue très fréquemment.** Les chiffres ci-dessous sont fournis à titre d'**ordre de grandeur pédagogique** et peuvent être désactualisés. Consultez toujours la page officielle pour les tarifs actuels : [platform.openai.com/docs/pricing](https://platform.openai.com/docs/pricing).

**Modèles GPT (ordre de grandeur 2025-2026)** :
- `gpt-4o-mini` (recommandé pour la plupart des usages) : ~0,15$ input / ~0,60$ output par million de tokens
- `gpt-4o` (multimodal, qualité supérieure) : ~2,50$ input / ~10$ output par million de tokens
- `gpt-3.5-turbo` (legacy) : ~0,50$ input / ~1,50$ output par million de tokens
- Série `gpt-5*` : tarifs supérieurs à `gpt-4o` (consulter la doc)

**Vision** : intégré nativement à `gpt-4o` et `gpt-4o-mini` (pas de modèle vision séparé). Coût additionnel basé sur la résolution de l'image (~0,001-0,008$ par image selon `detail: low/high`).

**Génération d'images** :
- ⚠️ DALL-E 2 et DALL-E 3 ont été **retirés de l'API OpenAI le 12 mai 2026**
- Successeurs : `gpt-image-1` et `gpt-image-1-mini`
- Tarification basée sur la qualité demandée (low / medium / high) et la résolution

## Autres services cloud d'IA

### Hugging Face

**Offres principales** :
- **Inference API (Serverless)** : appel direct REST à des centaines de milliers de modèles, gratuit avec quotas mensuels
- **Inference Endpoints** : déploiement dédié payant pour la production (latence garantie, scaling automatique)
- **Spaces** : applications/démos déployées (Gradio, Streamlit)

**Avantages** :
- Accès à des milliers de modèles open source (transformers, vision, audio)
- Communauté très active, hub de modèles ouvert
- Compatible avec les modèles open source majeurs (Llama, Mistral, Phi, etc.)

**Cas d'usage** :
- Expérimentation avec différents modèles
- Modèles spécialisés (langues rares, domaines spécifiques)
- Alternative open source aux LLMs propriétaires

### IBM watsonx (anciennement IBM Watson)

**Points forts** :
- Focus entreprise et conformité (RGPD, sécurité, gouvernance)
- Hybride : cloud, on-premises ou hybride
- Support professionnel

**Plateforme watsonx 2026** (rebranding depuis 2023) :
- **watsonx.ai** : développement et déploiement de modèles IA (LLMs, deep learning)
- **watsonx.data** : gouvernance et préparation des données
- **watsonx.governance** : outils d'IA responsable (audit, biais, conformité)

**Services historiques toujours disponibles** :
- Watson Natural Language Understanding
- Watson Speech to Text / Text to Speech
- Watson Discovery

### Anthropic Claude

**Caractéristiques** :
- Concurrent direct des modèles GPT (OpenAI)
- Excellente compréhension contextuelle, particulièrement performant pour le code et l'analyse de documents longs
- Fenêtre de contexte très large : 200K tokens en standard, **1M tokens disponible** sur les modèles Claude 4.x récents
- Famille de modèles : `claude-opus-4` (le plus capable), `claude-sonnet-4` (équilibré), `claude-haiku-4` (rapide et économique)

**Utilisation** : API REST similaire à OpenAI, mais avec quelques différences notables :
- Header `x-api-key` (au lieu de `Authorization: Bearer`)
- Header `anthropic-version` obligatoire (ex: `2023-06-01`)
- Le rôle `system` est passé **au niveau racine** de la requête, pas dans le tableau `messages` (voir section 22.7)

Plus de détails et exemples Delphi complets dans la section [22.7 - Utilisation des grands modèles de langage via API](/22-intelligence-artificielle-et-machine-learning-avec-delphi/07-utilisation-des-grands-modeles-de-langage-via-api.md).

## Gestion des coûts

### Stratégies d'optimisation

**1. Mise en cache agressive**

```pascal
type
  TCacheAPI = class
  private
    FCache: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function ObtenirOuAppeler(const Cle: string;
      const FonctionAPI: TFunc<string>): string;
  end;

constructor TCacheAPI.Create;  
begin  
  inherited;
  FCache := TDictionary<string, string>.Create;
end;

destructor TCacheAPI.Destroy;  
begin  
  FCache.Free;
  inherited;
end;

function TCacheAPI.ObtenirOuAppeler(const Cle: string;
  const FonctionAPI: TFunc<string>): string;
begin
  // ⚠️ Cache sans expiration : version pédagogique. En production, ajouter
  //    une TTL (ex: TDictionary<string, record Value:string; Expire:TDateTime end>).
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
uses
  System.Math, Vcl.Graphics;

procedure OptimiserImagePourAPI(var Bitmap: TBitmap);  
const  
  MAX_DIMENSION = 800; // pixels
var
  Ratio: Double;
  NewWidth, NewHeight: Integer;
  Dest: TBitmap;
begin
  // ⚠️ TBitmap.SetSize() ne redimensionne PAS le contenu de l'image, il
  // coupe ou étend le canvas. Pour un vrai redimensionnement, il faut
  // un bitmap temporaire et StretchDraw.
  if (Bitmap.Width <= MAX_DIMENSION) and (Bitmap.Height <= MAX_DIMENSION) then
    Exit;

  Ratio := Min(MAX_DIMENSION / Bitmap.Width, MAX_DIMENSION / Bitmap.Height);
  NewWidth := Round(Bitmap.Width * Ratio);
  NewHeight := Round(Bitmap.Height * Ratio);

  Dest := TBitmap.Create;
  try
    Dest.PixelFormat := Bitmap.PixelFormat;
    Dest.SetSize(NewWidth, NewHeight);
    Dest.Canvas.StretchDraw(Rect(0, 0, NewWidth, NewHeight), Bitmap);
    Bitmap.Assign(Dest);
  finally
    Dest.Free;
  end;
end;
```

**4. Choisir le bon niveau de service**

- Utilisez les modèles "mini" / "lite" quand la qualité reste suffisante
- `gpt-4o-mini` au lieu de `gpt-4o` pour les tâches simples (rapport coût/qualité excellent)
- `claude-haiku-4` au lieu de `claude-opus-4` pour les requêtes répétitives
- Prétraitez localement quand possible (extraction OCR locale avant envoi au LLM, etc.)

**5. Monitoring et alertes**

```pascal
type
  TAPIUsageMonitor = class
  private
    // ⚠️ Toutes les valeurs sont stockées en CENTIMES (Integer) pour éviter
    //    les imprécisions de calcul flottant sur les cumuls.
    FUsageAujourdhui: Integer;     // en centimes
    FLimiteQuotidienne: Integer;   // en centimes
    procedure VerifierLimite;
  public
    procedure IncrementerUsage(const Cout: Double);
    function PeutEffectuerAppel: Boolean;
  end;

procedure TAPIUsageMonitor.IncrementerUsage(const Cout: Double);  
begin  
  // Convertir le coût (USD) en centimes pour cumul en Integer
  FUsageAujourdhui := FUsageAujourdhui + Round(Cout * 100);
  VerifierLimite;

  // Alerte si 80% de la limite quotidienne atteinte
  if FUsageAujourdhui >= Round(FLimiteQuotidienne * 0.8) then
    EnvoyerAlerteAdministrateur('Limite API proche');
end;
```

### Estimation des coûts

**Exemple pour une application de chatbot** (ordres de grandeur 2025-2026) :

```
Utilisateurs : 1000  
Messages moyens par jour : 5  
Tokens moyens par message : 150 (50 input + 100 output)  

Total tokens/jour = 1000 × 5 × 150 = 750 000 tokens  
Répartition mensuelle = 7,5M input + 15M output sur 30 jours  

Coût gpt-4o-mini (~0,15$ in / 0,60$ out par million) :
- 7,5M × 0,15$ + 15M × 0,60$ ≈ 1,13$ + 9$ = ~10$/mois

Coût gpt-4o (~2,50$ in / 10$ out par million) :
- 7,5M × 2,50$ + 15M × 10$ ≈ 18,75$ + 150$ = ~169$/mois

→ gpt-4o-mini est ~17× moins cher que gpt-4o pour ce profil d'usage.
   À adapter selon la complexité réelle des prompts et la qualité requise.
```

> Note : ces chiffres dépendent fortement de la longueur des prompts et des réponses. Réalisez toujours une **mesure réelle** sur quelques centaines d'appels représentatifs avant de dimensionner un budget production.

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
uses
  System.RegularExpressions; // TRegEx

function AnonymiserTexte(const Texte: string): string;  
begin  
  Result := Texte;

  // Remplacer emails
  Result := TRegEx.Replace(Result, '\b[\w\.-]+@[\w\.-]+\.\w+\b', '[EMAIL]');

  // Remplacer numéros de téléphone (format français 10 chiffres groupés par 2)
  Result := TRegEx.Replace(Result, '\b\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}[\s\.-]?\d{2}\b', '[TÉLÉPHONE]');

  // Remplacer IBAN (regex simplifiée : 2 lettres pays + 2 chiffres + BBAN)
  // ⚠️ Cette regex ne couvre PAS tous les formats IBAN (certains pays comme
  //    le Royaume-Uni utilisent des lettres dans le BBAN — ex: GB29 NWBK ...).
  //    Pour une couverture complète, utiliser : `\b[A-Z]{2}\d{2}[\s]?[A-Z0-9\s]{11,30}\b`.
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
- Vous voulez les modèles de langage les plus avancés (famille GPT-4o / GPT-5)
- Vous développez un chatbot avancé ou une assistance multimodale
- Le coût n'est pas la première priorité (privilégiez `gpt-4o-mini` pour optimiser)

**Utilisez Anthropic Claude si** :
- Vous traitez des documents très longs (contexte 200K-1M tokens)
- Vous voulez d'excellentes performances sur le code et le raisonnement structuré
- La qualité de la rédaction et la fiabilité comptent autant que la rapidité

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

Dans la section suivante, nous explorerons spécifiquement l'intégration des grands modèles de langage (LLM) comme la famille GPT-4o, Claude ou Gemini, qui révolutionnent les interfaces conversationnelles et la génération de contenu !

⏭️ [Utilisation des grands modèles de langage (LLM) via API](/22-intelligence-artificielle-et-machine-learning-avec-delphi/07-utilisation-des-grands-modeles-de-langage-via-api.md)
