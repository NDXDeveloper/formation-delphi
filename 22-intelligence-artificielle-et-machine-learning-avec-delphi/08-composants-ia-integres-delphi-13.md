🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.8 SmartCore AI Component Pack : composants IA officiels de Delphi 13

## Introduction au SmartCore AI Component Pack

Delphi 13 Florence (sorti le **10 septembre 2025**) introduit le **SmartCore AI Component Pack**, un ensemble officiel de composants permettant d'ajouter des fonctionnalités IA dans vos applications Delphi et C++Builder.

### Documentation officielle

- Page produit : [getitnow.embarcadero.com/SmartCoreAI-1.0](https://getitnow.embarcadero.com/SmartCoreAI-1.0/)
- Article d'introduction : [blogs.embarcadero.com/introducing-the-smartcore-ai-components-pack](https://blogs.embarcadero.com/introducing-the-smartcore-ai-components-pack/)
- Wiki documentation : [docwiki.embarcadero.com/.../SmartCore_AI_Component_Pack](https://docwiki.embarcadero.com/RADStudio/Florence/en/SmartCore_AI_Component_Pack)

### Installation

Le pack s'installe via **GetIt Package Manager** :
1. Menu : **Outils → GetIt Package Manager**
2. Rechercher "SmartCore AI"
3. Installer la version compatible avec votre RAD Studio (v1.0 pour 13.0-13.1)

### Architecture du pack

Le SmartCore AI Component Pack suit une architecture similaire à FireDAC :
- Un composant **`TAIConnection`** (équivalent au `TFDConnection`) qui lie un Driver concret
- Plusieurs **drivers** spécifiques par fournisseur IA (OpenAI, Claude, Gemini, Ollama)
- Des **composants de requête** (Request) pour chaque type de tâche (chat, image, JSON, stream)

### Fournisseurs IA supportés

| Fournisseur | Driver | Type |
|------------|--------|------|
| OpenAI | `TAIOpenAIDriver` | En ligne (cloud) |
| Anthropic Claude | `TAIClaudeDriver` | En ligne (cloud) |
| Google Gemini | `TAIGeminiDriver` | En ligne (cloud) |
| Ollama | `TAIOllamaDriver` | **Local** (hors ligne) |

> 💡 Le support d'**Ollama** permet d'utiliser des LLM hors ligne (Llama, Mistral, Phi, etc.) pour des cas d'usage avec contraintes de confidentialité.

## Composants principaux

### TAIConnection

**Description** : Composant central qui gère la connexion à un fournisseur IA, agnostique du driver utilisé. Similaire à `TFDConnection` pour les bases de données.

**Configuration** : Via l'éditeur de design `TAIConnectionEditor` qui lance le wizard `TfrmAIConnectionWizard` (clic droit sur le composant dans le designer).

**Propriétés clés** :
- `Driver` : Le driver concret (`TAIOpenAIDriver`, `TAIClaudeDriver`, etc.)
- `Params` : Paramètres typés selon le driver (`TAIOpenAIParams`, `TAIClaudeParams`, etc.)
- `Connected` : Boolean indiquant si la connexion est active

### TAIChatRequest

**Description** : Composant de requête pour les conversations chat (texte → texte).

**Méthode principale** : `Chat(const Prompt: string)`

**Événement principal** : `OnResponse(Sender: TObject; const Text: string)`

**Exemple officiel d'utilisation** :

```pascal
// Envoi d'un message
AIChatRequest1.Chat('Where is Florence?');

// Réception de la réponse
procedure TForm27.AIChatRequest1Response(Sender: TObject;
  const Text: string);
begin
  Memo1.Lines.Add(Text);
end;
```

### TAIImageRequest

**Description** : Composant de requête pour la génération d'images (texte → image).

**Méthode principale** : `Execute`

**Événement principal** : `OnSuccess` (donne accès au stream image ou à l'URL)

**Exemple officiel d'utilisation** :

```pascal
var
  ReqOpenAI: TAIOpenAIImageGenerationRequest;
begin
  ReqOpenAI := TAIOpenAIImageGenerationRequest.Create;
  AIImageRequest1.APIRequestObject := ReqOpenAI;
  ReqOpenAI.Prompt := 'Un coucher de soleil sur Florence, style aquarelle';
  ReqOpenAI.Model := 'gpt-image-1'; // dall-e-3 retiré en mai 2026 → utiliser gpt-image-1
  AIImageRequest1.Execute;
end;
```

> ⚠️ L'exemple original du blog Embarcadero (septembre 2025) utilisait `dall-e-3`. Depuis le **12 mai 2026**, OpenAI a retiré DALL-E de son API. Utilisez **`gpt-image-1`** ou `gpt-image-1-mini` pour la génération d'images en 2026.

### TAIJSONRequest

**Description** : Composant de requête pour les endpoints JSON génériques (sortie structurée).

**Cas d'usage** : Extraction de données structurées, réponses JSON forcées, function calling.

### TAIStreamRequest

**Description** : Composant de requête pour le streaming de réponse (token-par-token).

**Cas d'usage** : Affichage progressif des réponses LLM (effet "frappe") pour améliorer l'UX.

## Drivers et paramètres

Chaque driver a sa classe `TAI<Provider>Params` avec ses propres propriétés typées :

### TAIOpenAIDriver / TAIOpenAIParams

```pascal
// Configuration via le wizard de design OU programmatique :
var
  Conn: TAIConnection;
  Driver: TAIOpenAIDriver;
  Params: TAIOpenAIParams;
begin
  Conn := AIConnection1;
  Driver := TAIOpenAIDriver.Create(Conn);
  Params := TAIOpenAIParams(Driver.Params);

  Params.APIKey := ObtenirCleAPI; // ⚠️ Depuis configuration sécurisée
  Params.Model := 'gpt-4o-mini';
  Params.Temperature := 0.7;
  Params.MaxTokens := 1000;
end;
```

### TAIClaudeDriver / TAIClaudeParams

Configuration similaire mais avec les spécificités Anthropic (`x-api-key`, `anthropic-version`, etc.).

### TAIGeminiDriver / TAIGeminiParams

Configuration pour Google Gemini.

### TAIOllamaDriver / TAIOllamaParams

Configuration pour Ollama (LLM local) :

```pascal
Params.Endpoint := 'http://localhost:11434'; // Endpoint Ollama par défaut  
Params.Model := 'llama3'; // ou autre modèle installé localement  
```

## Classes de base et extensibilité

Le pack expose une architecture extensible :

- **`TAIRequest`** : Classe de base abstraite pour tous les composants de requête
- **`IAIDriver`** : Interface implémentée par tous les drivers
- **`TAIDriver`** : Classe de base abstraite pour créer un driver personnalisé
- **`TAIDriverRegistry`** : Registre permettant d'enregistrer/retrouver des drivers à l'exécution

Vous pouvez créer vos propres drivers personnalisés en héritant de `TAIDriver` et en s'enregistrant dans `TAIDriverRegistry`.

---

## Wrappers personnalisés (template pédagogique)

> ℹ️ **Section pédagogique** : Si vous avez besoin de wrappers personnalisés au-delà de SmartCore AI (cas spécifiques, optimisations métier), voici un template d'architecture inspiré de FireDAC que vous pouvez adapter. Ce code n'utilise PAS les classes officielles ci-dessus.

### TAIRestClient (wrapper custom illustratif)

**Description** : Composant spécialisé dérivé de TRESTClient, optimisé pour les appels aux services d'IA avec gestion automatique de nombreux aspects techniques.

**Avantages sur TRESTClient standard** :
- Configuration simplifiée pour les providers IA populaires
- Gestion automatique des tokens et de l'authentification
- Retry automatique en cas d'erreur temporaire
- Timeout adaptatif selon la complexité de la requête
- Métriques d'utilisation intégrées (coûts, tokens)

**Propriétés principales** :

```pascal
type
  TAIRestClient = class(TRESTClient)
  private
    FAIProvider: TAIProvider; // OpenAI, Anthropic, Google, etc.
    FAPIKey: string;
    FAutoRetry: Boolean;
    FMaxRetries: Integer;
    FUsageTracking: Boolean;
    FOnUsageUpdate: TUsageUpdateEvent;
  published
    property AIProvider: TAIProvider read FAIProvider write SetAIProvider;
    property APIKey: string read FAPIKey write SetAPIKey;
    property AutoRetry: Boolean read FAutoRetry write FAutoRetry default True;
    property MaxRetries: Integer read FMaxRetries write FMaxRetries default 3;
    property UsageTracking: Boolean read FUsageTracking write FUsageTracking;
    property OnUsageUpdate: TUsageUpdateEvent read FOnUsageUpdate write FOnUsageUpdate;
  end;
```

**Utilisation simple** :

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Configuration ultra-simple
  AIRestClient1.AIProvider := apOpenAI;
  AIRestClient1.APIKey := 'votre-clé-api'; // Mieux: depuis config sécurisée
  AIRestClient1.AutoRetry := True;
  AIRestClient1.UsageTracking := True;
end;

procedure TFormMain.AIRestClient1UsageUpdate(Sender: TObject;
  const Usage: TAIUsageInfo);
begin
  // Mise à jour automatique des métriques
  LabelTokensUtilises.Caption := Format('Tokens: %d', [Usage.TotalTokens]);
  LabelCoutEstime.Caption := Format('Coût: %.4f$', [Usage.EstimatedCost]);
end;
```

### TAIChat

**Description** : Composant haut niveau pour créer des interfaces de chat avec LLM sans gérer manuellement l'historique et le contexte.

**Caractéristiques** :
- Gestion automatique de l'historique de conversation
- Support des messages système, utilisateur et assistant
- Compression automatique de l'historique si dépassement de contexte
- Événements pour personnaliser le comportement

**Propriétés** :

```pascal
type
  TAIChat = class(TComponent)
  private
    FAIClient: TAIRestClient;
    FModel: string;
    FSystemMessage: string;
    FTemperature: Double;
    FMaxTokens: Integer;
    FConversationHistory: TAIMessageList;
    FAutoManageContext: Boolean;
    FOnMessageSent: TAIMessageEvent;
    FOnResponseReceived: TAIMessageEvent;
  published
    property AIClient: TAIRestClient read FAIClient write SetAIClient;
    property Model: string read FModel write FModel;
    property SystemMessage: string read FSystemMessage write SetSystemMessage;
    property Temperature: Double read FTemperature write FTemperature;
    property MaxTokens: Integer read FMaxTokens write FMaxTokens;
    property AutoManageContext: Boolean read FAutoManageContext write FAutoManageContext;
    property OnMessageSent: TAIMessageEvent read FOnMessageSent write FOnMessageSent;
    property OnResponseReceived: TAIMessageEvent read FOnResponseReceived write FOnResponseReceived;
  end;
```

**Méthodes principales** :

```pascal
// Envoyer un message (asynchrone)
procedure SendMessage(const UserMessage: string);

// Obtenir la réponse (bloquant)
function SendMessageSync(const UserMessage: string): string;

// Gestion de l'historique
procedure ClearHistory;  
function GetConversationAsJSON: TJSONArray;  
procedure LoadConversationFromJSON(const JSON: TJSONArray);  
```

**Exemple d'utilisation** :

```pascal
procedure TFormChat.FormCreate(Sender: TObject);  
begin  
  // Configuration du client IA
  AIRestClient1.AIProvider := apOpenAI;
  AIRestClient1.APIKey := Config.GetAPIKey('openai');

  // Configuration du chat
  AIChat1.AIClient := AIRestClient1;
  AIChat1.Model := 'gpt-4o-mini'; // ou 'gpt-4o' pour qualité supérieure
  AIChat1.SystemMessage :=
    'Tu es un assistant technique spécialisé en Delphi. ' +
    'Réponds de manière concise et professionnelle.';
  AIChat1.Temperature := 0.7;
  AIChat1.MaxTokens := 1000;
  AIChat1.AutoManageContext := True; // Gestion auto de la limite de tokens
end;

procedure TFormChat.BtnEnvoyerClick(Sender: TObject);  
begin  
  if EditMessage.Text.Trim.IsEmpty then Exit;

  // Afficher le message de l'utilisateur
  AjouterMessage('Vous', EditMessage.Text);

  // ⚠️ Désactiver le bouton AVANT l'envoi pour éviter une race condition :
  //    si la réponse arrive très vite, l'événement OnResponseReceived
  //    pourrait être déclenché AVANT l'instruction de désactivation et
  //    le bouton serait réactivé puis désactivé (donc inutilisable).
  BtnEnvoyer.Enabled := False;

  // Envoyer de manière asynchrone
  AIChat1.SendMessage(EditMessage.Text);
  EditMessage.Clear;
end;

procedure TFormChat.AIChat1ResponseReceived(Sender: TObject;
  const Message: TAIMessage);
begin
  // Réponse reçue
  AjouterMessage('Assistant', Message.Content);
  BtnEnvoyer.Enabled := True;
  ScrollToBottom;
end;

procedure TFormChat.AIChat1MessageSent(Sender: TObject;
  const Message: TAIMessage);
begin
  // Message envoyé, on peut afficher un indicateur de chargement
  ProgressBar.Visible := True;
end;
```

### TAIImageAnalyzer

**Description** : Composant spécialisé pour l'analyse d'images avec différents providers (Google Vision, Azure, OpenAI Vision).

**Propriétés** :

```pascal
type
  TAIImageAnalyzer = class(TComponent)
  private
    FAIClient: TAIRestClient;
    FAnalysisType: TAIImageAnalysisType; // Labels, OCR, Faces, Objects, Scene
    FMaxResults: Integer;
    FConfidenceThreshold: Double;
    FOnAnalysisComplete: TAIImageAnalysisEvent;
  published
    property AIClient: TAIRestClient read FAIClient write SetAIClient;
    property AnalysisType: TAIImageAnalysisType read FAnalysisType write FAnalysisType;
    property MaxResults: Integer read FMaxResults write FMaxResults default 10;
    property ConfidenceThreshold: Double read FConfidenceThreshold write FConfidenceThreshold;
    property OnAnalysisComplete: TAIImageAnalysisEvent read FOnAnalysisComplete write FOnAnalysisComplete;
  end;
```

**Utilisation** :

```pascal
procedure TFormImageAnalysis.BtnAnalyzerClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenDialog1.FileName);

    // Configuration
    AIImageAnalyzer1.AnalysisType := atLabels; // ou atOCR, atFaces, etc.
    AIImageAnalyzer1.MaxResults := 10;
    AIImageAnalyzer1.ConfidenceThreshold := 0.7; // 70%

    // Analyser
    AIImageAnalyzer1.AnalyzeImageFile(OpenDialog1.FileName);
  end;
end;

procedure TFormImageAnalysis.AIImageAnalyzer1AnalysisComplete(Sender: TObject;
  const Results: TAIImageAnalysisResults);
var
  i: Integer;
begin
  ListBoxResults.Items.Clear;

  case AIImageAnalyzer1.AnalysisType of
    atLabels:
      for i := 0 to High(Results.Labels) do
        ListBoxResults.Items.Add(Format('%s (%.0f%%)',
          [Results.Labels[i].Description,
           Results.Labels[i].Confidence * 100]));

    atOCR:
      MemoOCR.Text := Results.ExtractedText;

    atFaces:
      LabelFaceCount.Caption := Format('%d visage(s) détecté(s)',
        [Length(Results.Faces)]);

    atObjects:
      DessinerBoundingBoxes(Results.Objects);
  end;
end;
```

### TAITextAnalyzer

**Description** : Composant pour l'analyse de texte (sentiment, entités, classification, résumé).

**Fonctionnalités** :
- Analyse de sentiments
- Extraction d'entités nommées
- Classification de texte
- Résumé automatique
- Détection de langue

**Exemple** :

```pascal
procedure TFormTextAnalysis.BtnAnalyzerSentimentClick(Sender: TObject);  
begin  
  AITextAnalyzer1.AnalysisType := atSentiment;
  AITextAnalyzer1.AnalyzeText(MemoTexte.Text);
end;

procedure TFormTextAnalysis.AITextAnalyzer1AnalysisComplete(Sender: TObject;
  const Results: TAITextAnalysisResults);
begin
  case Results.Sentiment of
    stPositive:
    begin
      LabelSentiment.Caption := 'Positif';
      LabelSentiment.Font.Color := clGreen;
    end;
    stNegative:
    begin
      LabelSentiment.Caption := 'Négatif';
      LabelSentiment.Font.Color := clRed;
    end;
    stNeutral:
    begin
      LabelSentiment.Caption := 'Neutre';
      LabelSentiment.Font.Color := clGray;
    end;
  end;

  GaugeConfidence.Progress := Round(Results.Confidence * 100);
end;
```

### TAITranslator

**Description** : Composant simplifié pour la traduction de texte.

**Utilisation ultra-simple** :

```pascal
procedure TFormTraduction.BtnTraduireClick(Sender: TObject);  
begin  
  AITranslator1.SourceLanguage := 'fr'; // Auto-détection si vide
  AITranslator1.TargetLanguage := ComboBoxLangueCible.Text; // 'en', 'es', etc.
  AITranslator1.Translate(MemoSource.Text);
end;

procedure TFormTraduction.AITranslator1TranslationComplete(Sender: TObject;
  const TranslatedText: string);
begin
  MemoDestination.Text := TranslatedText;
end;
```

### TAISpeechToText et TAITextToSpeech

**Description** : Composants pour la reconnaissance et la synthèse vocale.

**TAISpeechToText** :

```pascal
procedure TFormVocal.BtnDemarrerEnregistrementClick(Sender: TObject);  
begin  
  AISpeechToText1.Language := 'fr-FR';
  AISpeechToText1.StartRecording; // Utilise le micro par défaut
end;

procedure TFormVocal.BtnArreterEnregistrementClick(Sender: TObject);  
begin  
  AISpeechToText1.StopRecording;
end;

procedure TFormVocal.AISpeechToText1TranscriptionComplete(Sender: TObject;
  const Text: string; const Confidence: Double);
begin
  MemoTranscription.Text := Text;
  LabelConfiance.Caption := Format('Confiance: %.0f%%', [Confidence * 100]);
end;
```

**TAITextToSpeech** :

```pascal
procedure TFormSyntheseVocale.BtnLireClick(Sender: TObject);  
begin  
  AITextToSpeech1.Voice := 'fr-FR-DeniseNeural'; // Voix française
  AITextToSpeech1.SpeakText(MemoTexte.Text);
end;

procedure TFormSyntheseVocale.AITextToSpeech1SpeechComplete(Sender: TObject);  
begin  
  ShowMessage('Lecture terminée');
end;
```

## Gestionnaire de clés API intégré

### Le problème de la gestion des clés

**Mauvaises pratiques courantes** :
```pascal
// ❌ Ne JAMAIS faire ça
const
  OPENAI_KEY = 'sk-1234567890abcdef';
  AZURE_KEY = 'abc123def456';
```

**Risques** :
- Clés dans le code source (visible dans VCS)
- Clés en clair dans les exécutables
- Partage accidentel de clés
- Difficile de changer les clés

### TAICredentialsManager

**Description** : Gestionnaire intégré pour stocker de manière sécurisée les clés API.

**Caractéristiques** :
- Chiffrement automatique des clés
- Stockage sécurisé (Windows: Credential Manager, macOS: Keychain)
- Interface visuelle de gestion
- Support multi-utilisateurs

**Configuration visuelle** :

1. Menu : Outils → Gestionnaire de clés API
2. Ajouter une nouvelle clé
3. Sélectionner le provider (OpenAI, Azure, etc.)
4. Entrer la clé
5. La clé est automatiquement chiffrée et stockée

**Utilisation dans le code** :

```pascal
uses
  AI.Credentials;

procedure TFormMain.FormCreate(Sender: TObject);  
var  
  CredManager: TAICredentialsManager;
  OpenAIKey: string;
begin
  CredManager := TAICredentialsManager.GetInstance;

  // Récupérer une clé stockée
  if CredManager.TryGetCredential('OpenAI', 'default', OpenAIKey) then
  begin
    AIRestClient1.APIKey := OpenAIKey;
  end
  else
  begin
    // Demander à l'utilisateur d'entrer la clé
    if CredManager.ShowCredentialDialog('OpenAI') then
      AIRestClient1.APIKey := CredManager.GetCredential('OpenAI', 'default');
  end;
end;
```

**Dialogue de configuration** :

```pascal
// Afficher le dialogue de configuration des clés
procedure TFormSettings.BtnConfigurerClesClick(Sender: TObject);  
begin  
  TAICredentialsManager.ShowConfigurationDialog;
end;
```

## Inspecteur de réponses IA

### Description

L'Inspecteur de réponses IA est un outil de débogage visuel intégré à l'IDE qui permet de visualiser et analyser les requêtes et réponses des API IA en temps réel.

### Fonctionnalités

**1. Visualisation des requêtes/réponses** :
- Format JSON indenté et colorisé
- Temps de réponse
- Codes de statut HTTP
- Headers

**2. Analyse des tokens** :
- Nombre de tokens d'entrée
- Nombre de tokens de sortie
- Coût estimé

**3. Historique** :
- Conservation des derniers appels
- Recherche dans l'historique
- Export en JSON

**4. Comparaison** :
- Comparer différentes réponses
- A/B testing de prompts

### Activation

```pascal
// Activer l'inspecteur pour un composant
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  AIRestClient1.EnableInspector := True; // Active l'inspecteur
  AIRestClient1.InspectorAutoShow := False; // Ne pas afficher automatiquement
end;

// Afficher manuellement l'inspecteur
procedure TFormMain.BtnDebugClick(Sender: TObject);  
begin  
  AIRestClient1.ShowInspector;
end;
```

### Utilisation durant le développement

**Mode Debug** : L'inspecteur est automatiquement activé en mode debug.

**Mode Release** : Désactivé par défaut pour éviter les fuites d'informations.

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  {$IFDEF DEBUG}
  AIRestClient1.EnableInspector := True;
  AIRestClient1.InspectorAutoShow := True;
  {$ENDIF}
end;
```

## Idées de projets IA à construire

> ℹ️ **Note** : Cette section présente des **idées de projets** que vous pouvez construire avec le SmartCore AI Component Pack et les outils de Delphi 13. Embarcadero ne fournit pas officiellement des templates de projets IA prédéfinis dans l'IDE — c'est à vous d'utiliser les composants `TAIConnection`, `TAIChatRequest`, etc. pour bâtir ces applications.

### Idées de projets recommandées pour démarrer

**1. Application Chatbot**
- Interface de chat complète (VCL ou FMX)
- Gestion de l'historique de conversation
- Configuration multi-providers (basculer entre OpenAI, Claude, Gemini, Ollama)
- Sauvegarde des conversations
- Composants : `TAIConnection` + `TAIChatRequest` (SmartCore AI)

**2. Outil d'analyse d'images**
- Upload d'images depuis l'utilisateur
- Analyse via Vision API (OpenAI gpt-4o, Google Vision, Azure Computer Vision)
- Affichage des résultats (labels, OCR, descriptions)
- Export des données vers CSV/JSON
- Composants : `TAIConnection` + `TAIJSONRequest` ou wrappers spécifiques

**3. Document OCR & Extraction**
- Import de PDF/images
- OCR multilingue (Google Vision, Azure OCR ou Tesseract local)
- Extraction de données structurées via LLM (Structured Outputs)
- Export vers Excel/CSV
- Composants : `TAIConnection` + `TAIChatRequest` (avec prompts de structured extraction)

**4. Dashboard d'analyse de sentiments**
- Import de données textuelles (CSV, base de données)
- Analyse de sentiments en batch via LLM
- Visualisations (graphiques avec TeeChart)
- Rapports exportables
- Composants : `TAIChatRequest` avec prompts de classification

**5. Assistant intelligent pour formulaires**
- Formulaire métier classique enrichi par IA
- Auto-complétion contextuelle des champs
- Validation intelligente (détection d'incohérences)
- Suggestions contextuelles selon les autres champs
- Composants : `TAIStreamRequest` pour des suggestions inline

### Démarrer un projet IA

**Procédure générale** :
1. **Fichier → Nouveau → Application VCL/FMX** (selon votre cible)
2. **Outils → GetIt Package Manager** : installer **SmartCore AI Component Pack**
3. Déposer un `TAIConnection` sur le formulaire et configurer son driver via clic droit → "Configure Connection..." (wizard `TAIConnectionEditor`)
4. Déposer un composant de requête (`TAIChatRequest`, `TAIImageRequest`, etc.)
5. Lier la requête à la connexion (propriété `Connection`)
6. Écrire le code métier dans les événements `OnResponse` / `OnSuccess`

## Support JSON amélioré

### Problème avec JSON complexe

Les API IA retournent souvent des structures JSON très imbriquées :

```json
{
  "choices": [
    {
      "message": {
        "role": "assistant",
        "content": "Réponse...",
        "tool_calls": [
          {
            "id": "call_abc123",
            "type": "function",
            "function": {
              "name": "get_weather",
              "arguments": "{\"location\":\"Paris\"}"
            }
          }
        ]
      },
      "finish_reason": "tool_calls"
    }
  ],
  "usage": {
    "prompt_tokens": 150,
    "completion_tokens": 75,
    "total_tokens": 225
  }
}
```

> ℹ️ **Note historique** : OpenAI utilisait auparavant `function_call` (singulier) pour les appels de fonctions. Depuis novembre 2023, le format officiel est `tool_calls` (tableau), permettant des appels parallèles. L'ancien format reste accepté mais déprécié.

### TAIJSONHelper

**Description** : Classe helper qui simplifie l'accès aux données JSON imbriquées.

```pascal
uses
  AI.JSON.Helper;

procedure TraiterReponseAPI;  
var  
  Response: TJSONObject;
  Content: string;
  TotalTokens: Integer;
begin
  Response := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;
  // ⚠️ ParseJSONValue retourne nil si le texte n'est pas un JSON valide ;
  //    le cast `as TJSONObject` retourne aussi nil si le résultat est par
  //    exemple un tableau racine. On vérifie pour éviter une AV.
  if not Assigned(Response) then
    raise Exception.Create('Réponse API : JSON invalide ou format inattendu');
  try
    // Accès simplifié avec notation "point"
    Content := Response.GetPath<string>('choices[0].message.content');
    TotalTokens := Response.GetPath<Integer>('usage.total_tokens');

    // Avec valeur par défaut si chemin n'existe pas
    FinishReason := Response.GetPathOrDefault<string>(
      'choices[0].finish_reason', 'unknown');

    // Vérifier existence d'un chemin (format moderne `tool_calls`)
    if Response.PathExists('choices[0].message.tool_calls') then
      TraiterToolCalls(Response.GetPath<TJSONArray>(
        'choices[0].message.tool_calls'));
  finally
    Response.Free;
  end;
end;
```

**Méthodes disponibles** :

```pascal
// Récupérer une valeur (exception si inexistant)
function GetPath<T>(const Path: string): T;

// Récupérer avec valeur par défaut
function GetPathOrDefault<T>(const Path: string; const Default: T): T;

// Vérifier existence
function PathExists(const Path: string): Boolean;

// Récupérer un tableau
function GetPathArray(const Path: string): TJSONArray;

// Itérer sur un tableau
procedure ForEachInPath(const Path: string; const Proc: TProc<TJSONValue>);
```

**Exemple d'utilisation avancée** :

```pascal
procedure AfficherResultatsAnalyse;  
var  
  Response: TJSONObject;
begin
  Response := GetAPIResponse;
  try
    // Parcourir tous les labels détectés
    Response.ForEachInPath('responses[0].labelAnnotations',
      procedure(Item: TJSONValue)
      var
        Obj: TJSONObject;
        Description: string;
        Score: Double;
      begin
        Obj := Item as TJSONObject;
        Description := Obj.GetValue<string>('description');
        Score := Obj.GetValue<Double>('score');

        ListBox1.Items.Add(Format('%s (%.0f%%)', [Description, Score * 100]));
      end);
  finally
    Response.Free;
  end;
end;
```

## Assistant de prompts

### Description

L'Assistant de prompts est un outil visuel pour construire, tester et optimiser vos prompts LLM directement dans l'IDE.

### Fonctionnalités

**1. Éditeur de prompts** :
- Coloration syntaxique pour variables
- Templates prédéfinis
- Variables paramétrables

**2. Zone de test** :
- Tester le prompt en temps réel
- Comparer différentes versions
- Voir l'impact des modifications

**3. Bibliothèque de prompts** :
- Sauvegarder vos prompts
- Partager entre projets
- Importer/exporter

**4. Analyseur** :
- Estimer le nombre de tokens
- Suggérer des améliorations
- Détecter les ambiguïtés

### Utilisation

**Ouvrir l'assistant** :
- Menu : Outils → Assistant de prompts IA
- Ou raccourci : Ctrl+Shift+P (configurable)

**Créer un prompt** :

```
1. Cliquer sur "Nouveau prompt"
2. Entrer le prompt avec variables : {nom_variable}
3. Tester avec différentes valeurs
4. Sauvegarder dans la bibliothèque
```

**Exemple de prompt avec variables** :

```
Tu es un {role}.  
Réponds à la question suivante de manière {style}.  

Contexte : {contexte}  
Question : {question}  

Réponds en {langue} avec {format}.
```

**Variables définies** :
- role : "expert Delphi"
- style : "concise et technique"
- contexte : "Application de gestion VCL"
- question : "Comment créer un thread ?"
- langue : "français"
- format : "exemple de code commenté"

**Génération du code** :

L'assistant peut générer le code Delphi correspondant :

```pascal
function GenererPrompt_ConseilTechnique(const Role, Style, Contexte,
  Question, Langue, Format: string): string;
const
  TEMPLATE =
    'Tu es un {role}.'#13#10 +
    'Réponds à la question suivante de manière {style}.'#13#10 +
    #13#10 +
    'Contexte : {contexte}'#13#10 +
    'Question : {question}'#13#10 +
    #13#10 +
    'Réponds en {langue} avec {format}.';
begin
  Result := TEMPLATE;
  Result := StringReplace(Result, '{role}', Role, [rfReplaceAll]);
  Result := StringReplace(Result, '{style}', Style, [rfReplaceAll]);
  Result := StringReplace(Result, '{contexte}', Contexte, [rfReplaceAll]);
  Result := StringReplace(Result, '{question}', Question, [rfReplaceAll]);
  Result := StringReplace(Result, '{langue}', Langue, [rfReplaceAll]);
  Result := StringReplace(Result, '{format}', Format, [rfReplaceAll]);
end;
```

## Exemples complets d'utilisation

### Exemple 1 : Application de chat complète

```pascal
unit FormChatBot;

interface

uses
  Winapi.Windows, Winapi.Messages, // Messages requis pour WM_VSCROLL / SB_BOTTOM
  System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls,
  AI.RestClient, AI.Chat, AI.Types;

type
  TFormChatBot = class(TForm)
    PanelTop: TPanel;
    ComboBoxModel: TComboBox;
    LabelModel: TLabel;
    MemoChat: TMemo;
    PanelBottom: TPanel;
    EditMessage: TEdit;
    BtnEnvoyer: TButton;
    AIRestClient1: TAIRestClient;
    AIChat1: TAIChat;
    LabelUsage: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure BtnEnvoyerClick(Sender: TObject);
    procedure AIChat1ResponseReceived(Sender: TObject; const Message: TAIMessage);
    procedure AIRestClient1UsageUpdate(Sender: TObject; const Usage: TAIUsageInfo);
    procedure ComboBoxModelChange(Sender: TObject);
  private
    procedure AjouterMessage(const Role, Content: string);
  end;

implementation

uses
  AI.Credentials;

{$R *.dfm}

procedure TFormChatBot.FormCreate(Sender: TObject);  
var  
  CredManager: TAICredentialsManager;
  APIKey: string;
begin
  // Configuration du client
  CredManager := TAICredentialsManager.GetInstance;
  if CredManager.TryGetCredential('OpenAI', 'default', APIKey) then
  begin
    AIRestClient1.AIProvider := apOpenAI;
    AIRestClient1.APIKey := APIKey;
    AIRestClient1.AutoRetry := True;
    AIRestClient1.UsageTracking := True;
  end
  else
  begin
    ShowMessage('Veuillez configurer votre clé API OpenAI dans les paramètres.');
    Exit;
  end;

  // Configuration du chat
  AIChat1.AIClient := AIRestClient1;
  AIChat1.Model := 'gpt-4o-mini'; // multimodal, économique
  AIChat1.SystemMessage :=
    'Tu es un assistant Delphi expert et amical. ' +
    'Tu aides les développeurs avec leurs questions techniques.';
  AIChat1.Temperature := 0.7;
  AIChat1.MaxTokens := 1000;
  AIChat1.AutoManageContext := True;

  // Remplir les modèles disponibles (liste illustrative — adapter aux modèles
  // actuellement disponibles sur votre compte)
  ComboBoxModel.Items.AddStrings(['gpt-4o-mini', 'gpt-4o', 'gpt-3.5-turbo']);
  ComboBoxModel.ItemIndex := 0;

  // Message de bienvenue
  AjouterMessage('Assistant',
    'Bonjour ! Je suis votre assistant Delphi. Comment puis-je vous aider ?');
end;

procedure TFormChatBot.BtnEnvoyerClick(Sender: TObject);  
var  
  UserMessage: string;
begin
  UserMessage := EditMessage.Text.Trim;
  if UserMessage.IsEmpty then Exit;

  // Afficher le message de l'utilisateur
  AjouterMessage('Vous', UserMessage);
  EditMessage.Clear;

  // Désactiver pendant le traitement
  BtnEnvoyer.Enabled := False;
  EditMessage.Enabled := False;

  // Envoyer au chat (asynchrone)
  AIChat1.SendMessage(UserMessage);
end;

procedure TFormChatBot.AIChat1ResponseReceived(Sender: TObject;
  const Message: TAIMessage);
begin
  // Afficher la réponse
  AjouterMessage('Assistant', Message.Content);

  // Réactiver l'interface
  BtnEnvoyer.Enabled := True;
  EditMessage.Enabled := True;
  EditMessage.SetFocus;
end;

procedure TFormChatBot.AIRestClient1UsageUpdate(Sender: TObject;
  const Usage: TAIUsageInfo);
begin
  // Mettre à jour les statistiques
  LabelUsage.Caption := Format(
    'Tokens: %d | Coût: %.4f$',
    [Usage.TotalTokens, Usage.EstimatedCost]
  );
end;

procedure TFormChatBot.ComboBoxModelChange(Sender: TObject);  
var  
  AncienModel: string;
begin
  // ⚠️ Sauvegarder l'ancien modèle AVANT toute modification, pour pouvoir
  // restaurer en cas d'annulation. Sans ça, AIChat1.Model serait modifié
  // même quand l'utilisateur clique "Non".
  AncienModel := AIChat1.Model;

  // Demander confirmation AVANT de changer
  if MessageDlg(
    'Changer de modèle réinitialisera la conversation. Continuer ?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    AIChat1.Model := ComboBoxModel.Text;
    AIChat1.ClearHistory;
    MemoChat.Clear;
    AjouterMessage('Assistant',
      'Conversation réinitialisée avec le modèle ' + ComboBoxModel.Text);
  end
  else
  begin
    // L'utilisateur annule : restaurer la sélection sans retrigger OnChange.
    // On désactive temporairement l'événement pour éviter une boucle infinie.
    ComboBoxModel.OnChange := nil;
    try
      ComboBoxModel.ItemIndex := ComboBoxModel.Items.IndexOf(AncienModel);
    finally
      ComboBoxModel.OnChange := ComboBoxModelChange;
    end;
  end;
end;

procedure TFormChatBot.AjouterMessage(const Role, Content: string);  
begin  
  MemoChat.Lines.Add(Format('[%s] %s', [Role, FormatDateTime('hh:nn:ss', Now)]));
  MemoChat.Lines.Add(Content);
  MemoChat.Lines.Add('');

  // Scroll vers le bas
  // ℹ️ `SendMessage` ici fait référence à la fonction Windows API (4 paramètres).
  //    Pas de collision avec `AIChat1.SendMessage(string)` car la signature
  //    diffère. Sur macOS/Linux, utilisez plutôt `MemoChat.SelStart := MemoChat.GetTextLen`
  //    puis `MemoChat.Perform(EM_SCROLLCARET, 0, 0)` (multi-plateforme).
  Winapi.Windows.SendMessage(MemoChat.Handle, WM_VSCROLL, SB_BOTTOM, 0);
end;

end.
```

### Exemple 2 : Analyse d'images par lots

```pascal
procedure TFormBatchImageAnalysis.BtnAnalyserDossierClick(Sender: TObject);  
var  
  Files: TStringList;
  FilePath, Pattern, FolderPath: string;
  i: Integer;
  Found: TArray<string>;
begin
  // ⚠️ Pour SelectDirectory (de Vcl.FileCtrl), le 3e paramètre est `var`
  //    et doit être initialisé (ne serait-ce qu'à une chaîne vide).
  FolderPath := '';
  if not SelectDirectory('Sélectionnez un dossier', '', FolderPath) then
    Exit;

  Files := TStringList.Create;
  try
    // ⚠️ TDirectory.GetFiles n'accepte qu'UN seul pattern à la fois (contrairement
    // à FindFirst). Pour combiner plusieurs extensions, on itère sur chaque pattern.
    for Pattern in ['*.jpg', '*.jpeg', '*.png'] do
    begin
      Found := TDirectory.GetFiles(FolderPath, Pattern);
      for FilePath in Found do
        Files.Add(FilePath);
    end;

    ProgressBar.Max := Files.Count;
    ProgressBar.Position := 0;
    ListBoxResults.Items.Clear;

    // Analyser chaque image
    for i := 0 to Files.Count - 1 do
    begin
      FilePath := Files[i];

      // Analyser l'image
      AIImageAnalyzer1.AnalysisType := atLabels;
      AIImageAnalyzer1.AnalyzeImageFileSync(FilePath); // Version synchrone pour batch

      // Les résultats sont traités dans l'événement
      Application.ProcessMessages;
      ProgressBar.Position := i + 1;
    end;

    ShowMessage(Format('Analyse terminée : %d images traitées', [Files.Count]));
  finally
    Files.Free;
  end;
end;

procedure TFormBatchImageAnalysis.AIImageAnalyzer1AnalysisComplete(
  Sender: TObject; const Results: TAIImageAnalysisResults);
var
  i: Integer;
  Summary: string;
begin
  // Résumer les résultats
  Summary := ExtractFileName(Results.SourceFile) + ': ';

  for i := 0 to Min(2, High(Results.Labels)) do // Top 3
    Summary := Summary + Results.Labels[i].Description + ', ';

  Summary := Summary.TrimRight([',', ' ']);
  ListBoxResults.Items.Add(Summary);

  // Sauvegarder en base ou fichier si nécessaire
  SauvegarderResultats(Results);
end;
```

## Avantages des composants intégrés

### Comparaison avant/après

**Avant Delphi 13 (code manuel)** :

```pascal
// ~50 lignes de code pour un simple appel
procedure AppelerOpenAI;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  // ... beaucoup de configuration
begin
  // Code complexe de configuration
  // Gestion manuelle des erreurs
  // Parsing JSON manuel
  // etc.
end;
```

**Avec Delphi 13 (composants)** :

```pascal
// 3-4 lignes pour le même résultat
procedure AppelerOpenAI;  
begin  
  AIChat1.SendMessage('Ma question');
  // C'est tout !
end;
```

### Gains principaux

**1. Productivité** :
- Développement 5-10× plus rapide
- Moins de code boilerplate
- Focus sur la logique métier

**2. Fiabilité** :
- Gestion d'erreurs robuste intégrée
- Retry automatique
- Validation des données

**3. Maintenabilité** :
- Code plus lisible
- Moins de bugs potentiels
- Mises à jour automatiques

**4. Sécurité** :
- Gestion sécurisée des clés
- Chiffrement intégré
- Bonnes pratiques appliquées

## Bonnes pratiques

### 1. Configuration centralisée

```pascal
// Créer une unité de configuration
unit App.AIConfig;

interface

uses
  AI.RestClient, AI.Chat, AI.Types;

type
  TAIConfig = class
  private
    class var FInstance: TAIConfig;
  public
    class function GetInstance: TAIConfig;

    procedure ConfigureAIClient(AIClient: TAIRestClient);
    procedure ConfigureAIChat(AIChat: TAIChat);
  end;

implementation

class function TAIConfig.GetInstance: TAIConfig;  
begin  
  if FInstance = nil then
    FInstance := TAIConfig.Create;
  Result := FInstance;
end;

procedure TAIConfig.ConfigureAIClient(AIClient: TAIRestClient);  
begin  
  AIClient.AIProvider := apOpenAI;
  AIClient.APIKey := TAICredentialsManager.GetInstance.GetCredential('OpenAI');
  AIClient.AutoRetry := True;
  AIClient.MaxRetries := 3;
  AIClient.UsageTracking := True;
  AIClient.EnableInspector := {$IFDEF DEBUG}True{$ELSE}False{$ENDIF};
end;

procedure TAIConfig.ConfigureAIChat(AIChat: TAIChat);  
begin  
  AIChat.Model := 'gpt-4o-mini';
  AIChat.Temperature := 0.7;
  AIChat.MaxTokens := 1000;
  AIChat.AutoManageContext := True;
end;
```

### 2. Gestion centralisée des erreurs

```pascal
procedure TFormBase.ConfigurerGestionErreurs;  
begin  
  AIRestClient1.OnError := GestionnaireErreurGlobal;
end;

procedure TFormBase.GestionnaireErreurGlobal(Sender: TObject;
  const Error: TAIError);
begin
  case Error.ErrorType of
    etAuthentication:
      ShowMessage('Erreur d''authentification. Vérifiez votre clé API.');

    etQuotaExceeded:
      ShowMessage('Quota dépassé. Attendez ou passez à un plan supérieur.');

    etRateLimited:
      ShowMessage('Trop de requêtes. Ralentissez les appels.');

    etInvalidRequest:
      ShowMessage('Requête invalide : ' + Error.Message);

    etNetwork:
      ShowMessage('Erreur réseau. Vérifiez votre connexion.');

  else
    ShowMessage('Erreur : ' + Error.Message);
  end;

  // Logger l'erreur
  LogError(Error);
end;
```

### 3. Monitoring et alertes

> ℹ️ **Précision Delphi** : pour pouvoir assigner une **anonymous method** à un événement (comme dans l'exemple ci-dessous), le type de l'événement doit être déclaré en `reference to procedure(...)` (équivalent à `TProc<...>`). Les événements VCL classiques (`procedure(...) of object`) n'acceptent que des méthodes d'instance. On suppose ici que `OnUsageUpdate` est déclaré en `reference to procedure` ; si ce n'est pas le cas dans votre version, créez plutôt une méthode classique sur le formulaire et assignez-la.

```pascal
procedure TFormMain.SurveillerUtilisation;  
begin  
  AIRestClient1.OnUsageUpdate := procedure(Sender: TObject;
    const Usage: TAIUsageInfo)
  begin
    // Mettre à jour l'interface
    StatusBar1.Panels[0].Text := Format('Tokens: %d', [Usage.TotalTokens]);
    StatusBar1.Panels[1].Text := Format('Coût: %.4f$', [Usage.EstimatedCost]);

    // Alerter si dépassement
    if Usage.EstimatedCost > 10.0 then
      ShowMessage('Attention : coût élevé détecté !');
  end;
end;
```

## Conclusion

Les composants IA intégrés de Delphi 13 Florence transforment radicalement la façon dont vous intégrez l'intelligence artificielle dans vos applications. Ce qui nécessitait auparavant des dizaines de lignes de code complexe se résume maintenant à quelques propriétés à configurer et quelques événements à gérer.

**Points essentiels** :
- Composants visuels prêts à l'emploi pour tous les cas d'usage courants
- Gestion sécurisée des clés API intégrée
- Outils de débogage et d'inspection performants
- Templates de projets pour démarrer rapidement
- Support JSON amélioré pour structures complexes
- Assistant de prompts pour optimiser vos interactions avec les LLM

**Avantages majeurs** :
- Développement 5-10× plus rapide
- Code plus maintenable et lisible
- Sécurité renforcée
- Gestion d'erreurs robuste
- Focus sur la logique métier plutôt que sur la plomberie technique

**Pour aller plus loin** :
1. Explorez les templates de projets fournis
2. Utilisez l'Assistant de prompts pour optimiser vos interactions
3. Activez l'Inspecteur pendant le développement
4. Consultez la documentation détaillée des composants
5. Rejoignez la communauté Delphi pour partager vos expériences

Avec ces outils, vous êtes parfaitement équipé pour créer des applications Delphi modernes et intelligentes qui tirent pleinement parti de la révolution de l'IA !

Dans la section suivante, nous explorerons le **RAD AI Companion** qui vous assiste directement dans votre développement Delphi.

⏭️ [RAD AI Companion : assistant IA intégré pour le développement](/22-intelligence-artificielle-et-machine-learning-avec-delphi/09-site-web-companion-ia-pour-developpement-assiste.md)
