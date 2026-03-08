🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 22.7 Utilisation des grands modèles de langage (LLM) via API

## Introduction aux grands modèles de langage

### Qu'est-ce qu'un LLM ?

Un LLM (Large Language Model ou Grand Modèle de Langage) est un modèle d'intelligence artificielle entraîné sur d'immenses quantités de texte provenant d'internet, de livres, d'articles scientifiques et d'autres sources. Ces modèles ont appris à comprendre et générer du langage humain de manière remarquablement naturelle.

**Analogie simple** : Imaginez quelqu'un qui aurait lu presque tout ce qui existe sur internet et qui pourrait instantanément vous aider sur n'importe quel sujet en conversant naturellement. C'est essentiellement ce qu'est un LLM.

### La révolution des LLM

**Avant les LLM (2010-2022)** :
- IA spécialisée pour chaque tâche
- Nécessitait entraînement spécifique
- Compréhension limitée du contexte
- Réponses rigides et scriptées

**Avec les LLM (2023+)** :
- Un seul modèle pour des milliers de tâches
- Compréhension profonde du contexte
- Conversations naturelles
- Capacités de raisonnement
- Génération créative

**Ce que les LLM peuvent faire** :
- Converser naturellement sur n'importe quel sujet
- Écrire du code dans n'importe quel langage (y compris Delphi !)
- Traduire entre langues
- Résumer des documents longs
- Analyser et extraire des informations
- Raisonner sur des problèmes complexes
- Générer du contenu créatif
- Répondre à des questions
- Et bien plus encore...

### Pourquoi c'est important pour vos applications Delphi

**Avant** : Pour créer un assistant intelligent, vous deviez programmer manuellement chaque scénario possible.

**Maintenant** : Avec un LLM, votre application peut comprendre et répondre à des demandes en langage naturel que vous n'avez jamais anticipées.

**Exemples concrets** :
```
Utilisateur : "Trouve-moi tous les clients qui n'ont pas commandé depuis 3 mois et qui habitent à Paris"  
Application : Comprend, génère la requête SQL, l'exécute, présente les résultats  

Utilisateur : "Rédige un email professionnel pour relancer ce client"  
Application : Génère un email personnalisé basé sur l'historique du client  

Utilisateur : "Explique-moi pourquoi les ventes ont baissé ce trimestre"  
Application : Analyse les données et fournit une explication détaillée  
```

## Les principaux LLM disponibles

### GPT-4 (OpenAI)

**Caractéristiques** :
- Le plus connu et largement utilisé
- Excellent pour la plupart des tâches
- GPT-4 Turbo : version plus rapide et moins chère
- GPT-4 Vision : analyse d'images

**Points forts** :
- Qualité générale exceptionnelle
- Suivi d'instructions précis
- Raisonnement avancé
- Large adoption et documentation

**Limitations** :
- Coûteux pour gros volumes
- Connaissances limitées à sa date d'entraînement (sauf avec tools)
- Parfois verbeux

**Tarification** :
- GPT-4 : ~60$ / million tokens
- GPT-4 Turbo : ~10-30$ / million tokens
- GPT-3.5 Turbo : ~1-2$ / million tokens

### Claude (Anthropic)

**Caractéristiques** :
- Concurrent direct de GPT-4
- Fenêtre de contexte massive (200K tokens)
- Focus sur la sécurité et l'éthique

**Points forts** :
- Excellent pour documents longs
- Refus approprié des requêtes inappropriées
- Très bon en raisonnement
- Conversations naturelles

**Limitations** :
- Moins connu que GPT
- API parfois en liste d'attente

**Tarification** :
- Claude 3 Opus : ~15-75$ / million tokens
- Claude 3 Sonnet : ~3-15$ / million tokens
- Claude 3 Haiku : ~0.25-1.25$ / million tokens

### Gemini (Google)

**Caractéristiques** :
- LLM de Google
- Multimodal natif (texte, image, audio, vidéo)
- Intégration avec l'écosystème Google

**Points forts** :
- Gratuit avec quotas généreux
- Multimodalité native
- Performances compétitives

**Limitations** :
- Plus récent, moins mature que GPT
- API en évolution

**Tarification** :
- Gemini Pro : Gratuit jusqu'à 60 requêtes/min
- Au-delà : ~0.50$ / million tokens

### Llama 3 (Meta)

**Caractéristiques** :
- Open source (poids du modèle publics)
- Peut être hébergé localement
- Plusieurs tailles (8B, 70B paramètres)

**Points forts** :
- Gratuit si auto-hébergé
- Contrôle total
- Pas de limite de requêtes
- Confidentialité maximale

**Limitations** :
- Nécessite infrastructure GPU
- Complexité de déploiement
- Qualité inférieure aux modèles propriétaires

**Utilisation via API** :
- Plusieurs fournisseurs offrent Llama hébergé (Together AI, Replicate)

### Mistral AI

**Caractéristiques** :
- Startup française
- Modèles performants et compacts
- Open source et API

**Points forts** :
- Excellent rapport qualité/prix
- Modèles européens (RGPD)
- Performants sur français

**Tarification** :
- Mistral Large : ~8$ / million tokens
- Mistral Medium : ~2.7$ / million tokens

## Concepts fondamentaux des LLM

### Tokens

**Qu'est-ce qu'un token ?**

Un token est une unité de texte que le modèle traite. Ce n'est pas exactement un mot ni un caractère.

**Règle approximative** :
- 1 token ≈ 4 caractères en anglais
- 1 token ≈ 0.75 mot en anglais
- 1 token ≈ 2-3 caractères en français (plus de caractères accentués)

**Exemples** :
```
"Bonjour" → 1-2 tokens
"Intelligence artificielle" → 3-4 tokens
"J'utilise Delphi pour développer" → 7-9 tokens
```

**Pourquoi c'est important** :
- La tarification est basée sur les tokens
- Les modèles ont une limite de tokens par requête
- Plus de tokens = coût plus élevé et traitement plus long

**Calculer les tokens** :

```pascal
// Estimation approximative (pas exacte)
function EstimerNombreTokens(const Texte: string): Integer;  
begin  
  // Règle empirique : 1 token ≈ 4 caractères
  Result := Round(Length(Texte) / 4);
end;
```

Pour un calcul exact, utilisez des API de tokenisation (tiktoken pour OpenAI).

### Prompts

**Qu'est-ce qu'un prompt ?**

Le prompt est l'instruction ou la question que vous donnez au LLM. C'est l'art de formuler votre demande pour obtenir la meilleure réponse possible.

**Anatomie d'un bon prompt** :

```
[Rôle/Contexte] + [Instruction claire] + [Contraintes] + [Format de sortie]
```

**Exemple basique** :
```
"Résume ce texte"
```

**Exemple avancé** :
```
Tu es un assistant commercial expert.  
Analyse l'email client ci-dessous et détermine :  
1. Le sentiment (positif/neutre/négatif)
2. L'urgence (faible/moyenne/haute)
3. La catégorie de demande (SAV/Vente/Technique/Autre)
4. Une suggestion de réponse en 2-3 phrases

Format de sortie : JSON avec les champs sentiment, urgence, categorie, suggestion

Email : [texte de l'email]
```

**Résultat** : Le prompt détaillé donne des réponses beaucoup plus structurées et utiles.

### Température

**Qu'est-ce que c'est ?**

Un paramètre qui contrôle le caractère aléatoire/créatif des réponses.

**Échelle** : 0.0 à 2.0 (généralement)

**Température basse (0.0 - 0.3)** :
- Réponses déterministes et prévisibles
- Idéal pour : extraction de données, classification, tâches factuelles
- Exemple : "Quelle est la capitale de la France ?" → Toujours "Paris"

**Température moyenne (0.5 - 0.8)** :
- Équilibre créativité/cohérence
- Idéal pour : conversations générales, assistance

**Température haute (0.9 - 2.0)** :
- Très créatif et varié
- Idéal pour : écriture créative, brainstorming
- Risque : réponses moins cohérentes

```pascal
// Exemple de configuration
RequestBody.AddPair('temperature', TJSONNumber.Create(0.3)); // Factuel
// ou
RequestBody.AddPair('temperature', TJSONNumber.Create(1.0)); // Créatif
```

### Contexte et fenêtre de contexte

**Fenêtre de contexte** : Le nombre maximum de tokens que le modèle peut "voir" à la fois (prompt + réponse).

**Exemples** :
- GPT-3.5 Turbo : 16K tokens (~12 000 mots)
- GPT-4 Turbo : 128K tokens (~96 000 mots)
- Claude 3 : 200K tokens (~150 000 mots)

**Importance** :
- Détermine la longueur des documents analysables
- Limite la longueur des conversations
- Plus grand = plus cher

**Gestion pratique** :

```pascal
const
  MAX_CONTEXT_TOKENS = 16000; // GPT-3.5 Turbo
  MAX_OUTPUT_TOKENS = 4000;

function VerifierLimiteContexte(const Prompt: string): Boolean;  
var  
  EstimationTokens: Integer;
begin
  EstimationTokens := EstimerNombreTokens(Prompt);
  Result := EstimationTokens < (MAX_CONTEXT_TOKENS - MAX_OUTPUT_TOKENS);

  if not Result then
    ShowMessage('Texte trop long pour le modèle');
end;
```

### System message vs User message

Les LLM distinguent différents types de messages :

**System** : Instructions globales qui définissent le comportement
```json
{
  "role": "system",
  "content": "Tu es un assistant Delphi expert. Réponds toujours avec du code Object Pascal commenté."
}
```

**User** : Messages de l'utilisateur
```json
{
  "role": "user",
  "content": "Comment créer un bouton dynamiquement ?"
}
```

**Assistant** : Réponses du modèle
```json
{
  "role": "assistant",
  "content": "Voici comment créer un bouton..."
}
```

## Intégration pratique avec Delphi

### Classe wrapper universelle pour LLM

```pascal
unit LLMClient;

interface

uses
  System.SysUtils, System.Classes, REST.Client, REST.Types,
  System.JSON, System.Generics.Collections;

type
  TLLMProvider = (lpOpenAI, lpAnthropic, lpGoogle, lpMistral);

  TLLMMessage = record
    Role: string;      // 'system', 'user', 'assistant'
    Content: string;
  end;

  TLLMConfig = record
    Provider: TLLMProvider;
    APIKey: string;
    Model: string;
    Temperature: Double;
    MaxTokens: Integer;
  end;

  TLLMClient = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FConfig: TLLMConfig;
    FMessages: TList<TLLMMessage>;

    function GetEndpoint: string;
    function BuildRequestBody: TJSONObject;
    function ExtractResponse(const JSON: TJSONObject): string;
  public
    constructor Create(const Config: TLLMConfig);
    destructor Destroy; override;

    procedure AddMessage(const Role, Content: string);
    procedure ClearMessages;
    function SendRequest: string;
    function Chat(const UserMessage: string): string; // Méthode simple

    property Messages: TList<TLLMMessage> read FMessages;
  end;

implementation

constructor TLLMClient.Create(const Config: TLLMConfig);  
begin  
  inherited Create;
  FConfig := Config;
  FMessages := TList<TLLMMessage>.Create;

  FRESTClient := TRESTClient.Create(GetEndpoint);
  FRESTResponse := TRESTResponse.Create(nil);
  FRESTRequest := TRESTRequest.Create(nil);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
  FRESTRequest.Method := rmPOST;
end;

destructor TLLMClient.Destroy;  
begin  
  FMessages.Free;
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;
  inherited;
end;

function TLLMClient.GetEndpoint: string;  
begin  
  case FConfig.Provider of
    lpOpenAI: Result := 'https://api.openai.com';
    lpAnthropic: Result := 'https://api.anthropic.com';
    lpGoogle: Result := 'https://generativelanguage.googleapis.com';
    lpMistral: Result := 'https://api.mistral.ai';
  else
    raise Exception.Create('Provider non supporté');
  end;
end;

procedure TLLMClient.AddMessage(const Role, Content: string);  
var  
  Msg: TLLMMessage;
begin
  Msg.Role := Role;
  Msg.Content := Content;
  FMessages.Add(Msg);
end;

procedure TLLMClient.ClearMessages;  
begin  
  FMessages.Clear;
end;

function TLLMClient.BuildRequestBody: TJSONObject;  
var  
  Messages: TJSONArray;
  Msg: TLLMMessage;
  MsgObj: TJSONObject;
begin
  Result := TJSONObject.Create;
  Messages := TJSONArray.Create;

  try
    // Ajouter tous les messages
    for Msg in FMessages do
    begin
      MsgObj := TJSONObject.Create;
      MsgObj.AddPair('role', Msg.Role);
      MsgObj.AddPair('content', Msg.Content);
      Messages.AddElement(MsgObj);
    end;

    // Construction selon le provider
    case FConfig.Provider of
      lpOpenAI, lpMistral:
      begin
        Result.AddPair('model', FConfig.Model);
        Result.AddPair('messages', Messages);
        Result.AddPair('temperature', TJSONNumber.Create(FConfig.Temperature));
        Result.AddPair('max_tokens', TJSONNumber.Create(FConfig.MaxTokens));
      end;

      lpAnthropic:
      begin
        Result.AddPair('model', FConfig.Model);
        Result.AddPair('messages', Messages);
        Result.AddPair('max_tokens', TJSONNumber.Create(FConfig.MaxTokens));
        Result.AddPair('temperature', TJSONNumber.Create(FConfig.Temperature));
      end;

      lpGoogle:
      begin
        // Google Gemini a une structure différente
        // Adapté selon leur API
      end;
    end;
  except
    Result.Free;
    Messages.Free;
    raise;
  end;
end;

function TLLMClient.ExtractResponse(const JSON: TJSONObject): string;  
begin  
  case FConfig.Provider of
    lpOpenAI, lpMistral:
      Result := JSON.GetValue<string>('choices[0].message.content');

    lpAnthropic:
      Result := JSON.GetValue<string>('content[0].text');

    lpGoogle:
      Result := JSON.GetValue<string>('candidates[0].content.parts[0].text');
  end;
end;

function TLLMClient.SendRequest: string;  
var  
  RequestBody: TJSONObject;
begin
  FRESTRequest.Params.Clear;

  // Configuration des headers selon provider
  case FConfig.Provider of
    lpOpenAI, lpMistral:
    begin
      FRESTRequest.Resource := 'v1/chat/completions';
      FRESTRequest.AddParameter('Authorization', 'Bearer ' + FConfig.APIKey,
        pkHTTPHEADER, [poDoNotEncode]);
      FRESTRequest.AddParameter('Content-Type', 'application/json',
        pkHTTPHEADER, [poDoNotEncode]);
    end;

    lpAnthropic:
    begin
      FRESTRequest.Resource := 'v1/messages';
      FRESTRequest.AddParameter('x-api-key', FConfig.APIKey,
        pkHTTPHEADER, [poDoNotEncode]);
      FRESTRequest.AddParameter('anthropic-version', '2023-06-01',
        pkHTTPHEADER, [poDoNotEncode]);
      FRESTRequest.AddParameter('content-type', 'application/json',
        pkHTTPHEADER, [poDoNotEncode]);
    end;
  end;

  RequestBody := BuildRequestBody;
  try
    FRESTRequest.ClearBody;
    FRESTRequest.AddBody(RequestBody.ToString, TRESTContentType.ctAPPLICATION_JSON);

    FRESTRequest.Execute;

    if FRESTResponse.StatusCode = 200 then
      Result := ExtractResponse(FRESTResponse.JSONValue as TJSONObject)
    else
      raise Exception.CreateFmt('Erreur API: %d - %s',
        [FRESTResponse.StatusCode, FRESTResponse.Content]);
  finally
    RequestBody.Free;
  end;
end;

function TLLMClient.Chat(const UserMessage: string): string;  
begin  
  AddMessage('user', UserMessage);
  Result := SendRequest;
  AddMessage('assistant', Result);
end;
```

### Utilisation simple

```pascal
procedure TFormChat.InitialiserLLM;  
var  
  Config: TLLMConfig;
begin
  Config.Provider := lpOpenAI;
  Config.APIKey := 'votre-clé-api';
  Config.Model := 'gpt-3.5-turbo';
  Config.Temperature := 0.7;
  Config.MaxTokens := 1000;

  FLLMClient := TLLMClient.Create(Config);

  // Message système optionnel
  FLLMClient.AddMessage('system',
    'Tu es un assistant technique spécialisé en Delphi. ' +
    'Réponds de manière concise et professionnelle.');
end;

procedure TFormChat.BtnEnvoyerClick(Sender: TObject);  
var  
  Reponse: string;
begin
  if EditMessage.Text.Trim.IsEmpty then Exit;

  // Afficher message utilisateur
  AjouterMessageChat('Vous', EditMessage.Text);

  // Traitement asynchrone
  TTask.Run(procedure
  var
    Response: string;
  begin
    try
      Response := FLLMClient.Chat(EditMessage.Text);

      TThread.Synchronize(nil, procedure
      begin
        AjouterMessageChat('Assistant', Response);
        EditMessage.Clear;
      end);
    except
      on E: Exception do
        TThread.Synchronize(nil, procedure
        begin
          ShowMessage('Erreur: ' + E.Message);
        end);
    end;
  end);
end;
```

## Techniques de prompting avancées

### Zero-shot prompting

Demander directement sans exemple.

```pascal
const
  PROMPT_ZERO_SHOT =
    'Classe le sentiment de ce texte comme positif, négatif ou neutre : %s';

function ClasserSentiment(const Texte: string): string;  
begin  
  Result := LLM.Chat(Format(PROMPT_ZERO_SHOT, [Texte]));
end;
```

### Few-shot prompting

Fournir des exemples pour guider le modèle.

```pascal
const
  PROMPT_FEW_SHOT =
    'Classe le sentiment des textes suivants.'#13#10 +
    #13#10 +
    'Texte: "Ce produit est excellent !"'#13#10 +
    'Sentiment: Positif'#13#10 +
    #13#10 +
    'Texte: "Service catastrophique, très déçu."'#13#10 +
    'Sentiment: Négatif'#13#10 +
    #13#10 +
    'Texte: "Le colis est arrivé hier."'#13#10 +
    'Sentiment: Neutre'#13#10 +
    #13#10 +
    'Texte: "%s"'#13#10 +
    'Sentiment:';

function ClasserSentimentAvecExemples(const Texte: string): string;  
begin  
  Result := LLM.Chat(Format(PROMPT_FEW_SHOT, [Texte]));
end;
```

### Chain-of-Thought (CoT)

Demander au modèle de "réfléchir à voix haute".

```pascal
const
  PROMPT_COT =
    'Résous ce problème étape par étape:'#13#10 +
    '%s'#13#10 +
    #13#10 +
    'Raisonnement:';

function ResoudreProbleme(const Probleme: string): string;  
begin  
  Result := LLM.Chat(Format(PROMPT_COT, [Probleme]));
end;
```

### Structured output

Demander une sortie formatée (JSON, XML, etc.).

```pascal
const
  PROMPT_JSON =
    'Analyse ce texte et retourne un JSON avec ces champs:'#13#10 +
    '- sujet: le sujet principal'#13#10 +
    '- sentiment: positif/négatif/neutre'#13#10 +
    '- mots_cles: liste de 3-5 mots-clés'#13#10 +
    '- resume: résumé en une phrase'#13#10 +
    #13#10 +
    'Texte: %s'#13#10 +
    #13#10 +
    'JSON:';

function AnalyserTexteStructure(const Texte: string): TJSONObject;  
var  
  Reponse: string;
begin
  Reponse := LLM.Chat(Format(PROMPT_JSON, [Texte]));

  // Parser le JSON retourné
  Result := TJSONObject.ParseJSONValue(Reponse) as TJSONObject;
end;
```

### Prompt templates réutilisables

```pascal
type
  TPromptTemplate = class
  private
    FTemplate: string;
    FVariables: TDictionary<string, string>;
  public
    constructor Create(const Template: string);
    destructor Destroy; override;

    procedure SetVariable(const Name, Value: string);
    function Build: string;
  end;

constructor TPromptTemplate.Create(const Template: string);  
begin  
  inherited Create;
  FTemplate := Template;
  FVariables := TDictionary<string, string>.Create;
end;

destructor TPromptTemplate.Destroy;  
begin  
  FVariables.Free;
  inherited;
end;

procedure TPromptTemplate.SetVariable(const Name, Value: string);  
begin  
  FVariables.AddOrSetValue(Name, Value);
end;

function TPromptTemplate.Build: string;  
var  
  Pair: TPair<string, string>;
begin
  Result := FTemplate;
  for Pair in FVariables do
    Result := StringReplace(Result, '{' + Pair.Key + '}',
      Pair.Value, [rfReplaceAll]);
end;

// Utilisation
procedure ExempleTemplate;  
var  
  Template: TPromptTemplate;
  Prompt: string;
begin
  Template := TPromptTemplate.Create(
    'Tu es un {role}. {instruction}'#13#10 +
    'Contexte: {contexte}'#13#10 +
    'Question: {question}'
  );
  try
    Template.SetVariable('role', 'expert Delphi');
    Template.SetVariable('instruction', 'Réponds avec du code commenté');
    Template.SetVariable('contexte', 'Application VCL Windows');
    Template.SetVariable('question', 'Comment créer un thread ?');

    Prompt := Template.Build;
    // Utiliser le prompt...
  finally
    Template.Free;
  end;
end;
```

## Cas d'usage avancés

### 1. Assistant de rédaction d'emails

```pascal
type
  TEmailAssistant = class
  private
    FLLM: TLLMClient;
  public
    constructor Create(const LLMClient: TLLMClient);

    function RedacterEmail(const Destinataire, Contexte, Ton: string): string;
    function RepondreEmail(const EmailOriginal, Instructions: string): string;
    function CorrigerEmail(const Email: string): string;
  end;

function TEmailAssistant.RedacterEmail(const Destinataire, Contexte, Ton: string): string;  
var  
  Prompt: string;
begin
  Prompt := Format(
    'Rédige un email professionnel.'#13#10 +
    'Destinataire: %s'#13#10 +
    'Contexte: %s'#13#10 +
    'Ton souhaité: %s'#13#10 +
    #13#10 +
    'Email:',
    [Destinataire, Contexte, Ton]
  );

  Result := FLLM.Chat(Prompt);
end;

function TEmailAssistant.CorrigerEmail(const Email: string): string;  
var  
  Prompt: string;
begin
  Prompt :=
    'Corrige ce email (orthographe, grammaire, style professionnel).'#13#10 +
    'Retourne uniquement la version corrigée.'#13#10 +
    #13#10 +
    Email;

  Result := FLLM.Chat(Prompt);
end;
```

### 2. Extracteur intelligent de données

```pascal
function ExtraireInformationsFacture(const TexteFacture: string): TJSONObject;  
var  
  Prompt: string;
  Reponse: string;
begin
  Prompt :=
    'Extrais les informations de cette facture et retourne un JSON avec:'#13#10 +
    '- numero_facture'#13#10 +
    '- date'#13#10 +
    '- montant_ht'#13#10 +
    '- montant_ttc'#13#10 +
    '- fournisseur'#13#10 +
    '- client'#13#10 +
    #13#10 +
    'Facture:'#13#10 +
    TexteFacture +
    #13#10#13#10 +
    'JSON:';

  Reponse := LLM.Chat(Prompt);

  // Parser et nettoyer la réponse
  // (le LLM peut ajouter du texte autour du JSON)
  Reponse := ExtraireJSON(Reponse);

  Result := TJSONObject.ParseJSONValue(Reponse) as TJSONObject;
end;

function ExtraireJSON(const Texte: string): string;  
var  
  StartPos, EndPos: Integer;
begin
  StartPos := Pos('{', Texte);
  EndPos := LastDelimiter('}', Texte);

  if (StartPos > 0) and (EndPos > StartPos) then
    Result := Copy(Texte, StartPos, EndPos - StartPos + 1)
  else
    Result := Texte;
end;
```

### 3. Générateur de requêtes SQL

```pascal
function GenererRequeteSQL(const DemandeNaturelle: string;
  const Schema: string): string;
var
  Prompt: string;
begin
  Prompt := Format(
    'Tu es un expert SQL. Génère une requête SQL basée sur cette demande.'#13#10 +
    #13#10 +
    'Schéma de base de données:'#13#10 +
    '%s'#13#10 +
    #13#10 +
    'Demande: %s'#13#10 +
    #13#10 +
    'Retourne uniquement la requête SQL, sans explication.',
    [Schema, DemandeNaturelle]
  );

  Result := LLM.Chat(Prompt);

  // Nettoyer (enlever les backticks, etc.)
  Result := StringReplace(Result, '```sql', '', [rfReplaceAll]);
  Result := StringReplace(Result, '```', '', [rfReplaceAll]);
  Result := Trim(Result);
end;

// Utilisation
procedure TFormMain.BtnGenererSQLClick(Sender: TObject);  
var  
  Schema, DemandeNaturelle, SQL: string;
begin
  Schema :=
    'Table: Clients (id, nom, prenom, email, ville, date_creation)'#13#10 +
    'Table: Commandes (id, client_id, date, montant, statut)';

  DemandeNaturelle := EditDemande.Text;
  // Ex: "Trouve tous les clients de Paris qui ont commandé plus de 1000€"

  SQL := GenererRequeteSQL(DemandeNaturelle, Schema);
  MemoSQL.Text := SQL;

  // Option: demander confirmation avant exécution
  if MessageDlg('Exécuter cette requête ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ExecuterRequete(SQL);
end;
```

### 4. Chatbot contextuel pour application

```pascal
type
  TApplicationChatbot = class
  private
    FLLM: TLLMClient;
    FContexteApplication: string;

    function ObtenirContexteDynamique: string;
  public
    constructor Create(const LLMClient: TLLMClient);

    procedure InitialiserContexte;
    function Repondre(const Question: string): string;
  end;

procedure TApplicationChatbot.InitialiserContexte;  
begin  
  FContexteApplication :=
    'Tu es l''assistant de l''application de gestion ACME Corp.'#13#10 +
    'Base de connaissances:'#13#10 +
    '- Pour créer un client: Menu Clients > Nouveau'#13#10 +
    '- Pour générer un rapport: Menu Rapports > sélectionner type'#13#10 +
    '- Support technique: support@acme.com'#13#10 +
    #13#10 +
    'Réponds toujours de manière concise et actionnable.';

  FLLM.ClearMessages;
  FLLM.AddMessage('system', FContexteApplication);
end;

function TApplicationChatbot.ObtenirContexteDynamique: string;  
begin  
  // Ajouter des infos sur l'état actuel de l'application
  Result := Format(
    'Contexte actuel:'#13#10 +
    '- Utilisateur connecté: %s'#13#10 +
    '- Écran actif: %s'#13#10 +
    '- Dernière action: %s',
    [UserManager.CurrentUser.Name,
     Screen.ActiveForm.Name,
     ActionHistory.GetLast]
  );
end;

function TApplicationChatbot.Repondre(const Question: string): string;  
var  
  Contexte: string;
  QuestionComplete: string;
begin
  Contexte := ObtenirContexteDynamique;
  QuestionComplete := Contexte + #13#10#13#10 + 'Question: ' + Question;

  Result := FLLM.Chat(QuestionComplete);
end;
```

### 5. Analyseur de logs et diagnostiqueur

```pascal
function DiagnostiquerErreur(const MessageErreur, StackTrace: string): string;  
var  
  Prompt: string;
begin
  Prompt := Format(
    'Tu es un expert en débogage Delphi.'#13#10 +
    'Analyse cette erreur et suggère des solutions.'#13#10 +
    #13#10 +
    'Erreur: %s'#13#10 +
    #13#10 +
    'Stack trace:'#13#10 +
    '%s'#13#10 +
    #13#10 +
    'Fournis:'#13#10 +
    '1. Cause probable'#13#10 +
    '2. Solutions suggérées'#13#10 +
    '3. Code d''exemple si pertinent',
    [MessageErreur, StackTrace]
  );

  Result := LLM.Chat(Prompt);
end;

// Utilisation dans un exception handler
procedure TFormMain.ApplicationExceptionHandler(Sender: TObject; E: Exception);  
var  
  Diagnostic: string;
begin
  // Logger l'erreur normalement
  LogError(E);

  // Demander diagnostic à l'IA (en arrière-plan)
  TTask.Run(procedure
  var
    Diag: string;
  begin
    Diag := DiagnostiquerErreur(E.Message, E.StackTrace);

    TThread.Synchronize(nil, procedure
    begin
      // Afficher dans un panneau d'aide
      PanelDiagnostic.Visible := True;
      MemoDiagnostic.Text := Diag;
    end);
  end);

  // Afficher l'erreur à l'utilisateur
  ShowMessage(E.Message);
end;
```

## Gestion des conversations

### Système de mémoire conversationnelle

```pascal
type
  TConversationManager = class
  private
    FLLM: TLLMClient;
    FMaxMessages: Integer;
    FMaxTokens: Integer;

    function EstimerTokensConversation: Integer;
    procedure TronquerConversation;
  public
    constructor Create(const LLMClient: TLLMClient;
      MaxMessages: Integer = 20; MaxTokens: Integer = 4000);

    function Chat(const UserMessage: string): string;
    procedure ReinitialiserConversation;
    procedure SauvegarderConversation(const Fichier: string);
    procedure ChargerConversation(const Fichier: string);
  end;

function TConversationManager.EstimerTokensConversation: Integer;  
var  
  Msg: TLLMMessage;
  Total: string;
begin
  Total := '';
  for Msg in FLLM.Messages do
    Total := Total + Msg.Content;

  Result := EstimerNombreTokens(Total);
end;

procedure TConversationManager.TronquerConversation;  
var  
  TokensActuels: Integer;
begin
  TokensActuels := EstimerTokensConversation;

  // Si dépassement, supprimer les anciens messages (sauf system)
  while (TokensActuels > FMaxTokens) and (FLLM.Messages.Count > 2) do
  begin
    // Supprimer le 2ème message (1er après system)
    if FLLM.Messages[1].Role <> 'system' then
      FLLM.Messages.Delete(1);

    TokensActuels := EstimerTokensConversation;
  end;
end;

function TConversationManager.Chat(const UserMessage: string): string;  
begin  
  // Vérifier la limite de messages
  if FLLM.Messages.Count >= FMaxMessages then
    TronquerConversation;

  Result := FLLM.Chat(UserMessage);
end;

procedure TConversationManager.SauvegarderConversation(const Fichier: string);  
var  
  JSON: TJSONArray;
  Msg: TLLMMessage;
  MsgObj: TJSONObject;
  FileStream: TFileStream;
  Writer: TStreamWriter;
begin
  JSON := TJSONArray.Create;
  try
    for Msg in FLLM.Messages do
    begin
      MsgObj := TJSONObject.Create;
      MsgObj.AddPair('role', Msg.Role);
      MsgObj.AddPair('content', Msg.Content);
      JSON.AddElement(MsgObj);
    end;

    FileStream := TFileStream.Create(Fichier, fmCreate);
    Writer := TStreamWriter.Create(FileStream);
    try
      Writer.Write(JSON.ToString);
    finally
      Writer.Free;
      FileStream.Free;
    end;
  finally
    JSON.Free;
  end;
end;
```

### Résumé automatique de conversation

```pascal
function ResumerConversation(const Messages: TList<TLLMMessage>): string;  
var  
  Historique: string;
  Msg: TLLMMessage;
  Prompt: string;
begin
  // Construire l'historique
  Historique := '';
  for Msg in Messages do
  begin
    if Msg.Role <> 'system' then
      Historique := Historique + Format('%s: %s'#13#10, [Msg.Role, Msg.Content]);
  end;

  // Demander résumé
  Prompt :=
    'Résume cette conversation en 2-3 phrases, en conservant les points clés:'#13#10 +
    #13#10 +
    Historique;

  Result := LLM.Chat(Prompt);
end;

// Utilisation pour compresser l'historique
procedure CompresserConversation;  
var  
  Resume: string;
begin
  if ConversationManager.FLLM.Messages.Count > 10 then
  begin
    Resume := ResumerConversation(ConversationManager.FLLM.Messages);

    // Remplacer l'historique par le résumé
    ConversationManager.ReinitialiserConversation;
    ConversationManager.FLLM.AddMessage('system',
      'Résumé de la conversation précédente: ' + Resume);
  end;
end;
```

## Optimisation et bonnes pratiques

### 1. Streaming des réponses

Pour une meilleure expérience utilisateur, affichez les réponses progressivement.

```pascal
// La plupart des API LLM supportent le streaming
// Configuration pour OpenAI:
RequestBody.AddPair('stream', TJSONBool.Create(True));

// Gérer les réponses partielles
procedure TraiterStreamingResponse;  
var  
  Lines: TStringList;
  Line: string;
  JSONObj: TJSONObject;
  Delta: string;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := FRESTResponse.Content;

    for Line in Lines do
    begin
      if Line.StartsWith('data: ') then
      begin
        Delta := Copy(Line, 7, Length(Line));
        if Delta = '[DONE]' then Break;

        JSONObj := TJSONObject.ParseJSONValue(Delta) as TJSONObject;
        try
          Delta := JSONObj.GetValue<string>('choices[0].delta.content');

          // Afficher progressivement
          TThread.Synchronize(nil, procedure
          begin
            MemoReponse.Text := MemoReponse.Text + Delta;
          end);
        finally
          JSONObj.Free;
        end;
      end;
    end;
  finally
    Lines.Free;
  end;
end;
```

### 2. Gestion robuste des erreurs

```pascal
function AppelerLLMAvecRetry(const Prompt: string;
  MaxRetries: Integer = 3): string;
var
  Tentatives: Integer;
  Erreur: Exception;
begin
  Tentatives := 0;

  repeat
    try
      Result := LLM.Chat(Prompt);
      Exit; // Succès
    except
      on E: Exception do
      begin
        Inc(Tentatives);
        Erreur := E;

        // Attendre avant retry (backoff exponentiel)
        Sleep(1000 * Tentatives);

        if Tentatives >= MaxRetries then
          raise; // Propager l'exception après max retries
      end;
    end;
  until False;
end;
```

### 3. Cache intelligent

```pascal
type
  TLLMCache = class
  private
    FCache: TDictionary<string, string>;
    FHits: Integer;
    FMisses: Integer;

    function CalculerHash(const Prompt: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const Prompt: string; out Response: string): Boolean;
    procedure Put(const Prompt, Response: string);

    property HitRate: Double read GetHitRate;
  end;

function TLLMCache.CalculerHash(const Prompt: string): string;  
var  
  HashBytes: TBytes;
begin
  // Utiliser MD5 ou SHA pour le hash
  HashBytes := THashMD5.GetHashBytes(Prompt);
  Result := TNetEncoding.Base64.EncodeBytesToString(HashBytes);
end;

function TLLMCache.Get(const Prompt: string; out Response: string): Boolean;  
var  
  Hash: string;
begin
  Hash := CalculerHash(Prompt);
  Result := FCache.TryGetValue(Hash, Response);

  if Result then
    Inc(FHits)
  else
    Inc(FMisses);
end;

// Utilisation avec cache
function ChatAvecCache(const Prompt: string): string;  
begin  
  if not Cache.Get(Prompt, Result) then
  begin
    Result := LLM.Chat(Prompt);
    Cache.Put(Prompt, Result);
  end;
end;
```

### 4. Limitation de débit (Rate limiting)

```pascal
type
  TRateLimiter = class
  private
    FMaxRequetesParMinute: Integer;
    FRequetesMinuteActuelle: Integer;
    FDerniereReset: TDateTime;
  public
    constructor Create(MaxRequetesParMinute: Integer);

    procedure AttendreDisponibilite;
    procedure IncrementerCompteur;
  end;

procedure TRateLimiter.AttendreDisponibilite;  
var  
  MinutesEcoulees: Integer;
begin
  MinutesEcoulees := MinutesBetween(Now, FDerniereReset);

  if MinutesEcoulees >= 1 then
  begin
    // Nouvelle minute
    FRequetesMinuteActuelle := 0;
    FDerniereReset := Now;
  end
  else if FRequetesMinuteActuelle >= FMaxRequetesParMinute then
  begin
    // Attendre la prochaine minute
    Sleep(60000 - (SecondsBetween(Now, FDerniereReset) * 1000));
    FRequetesMinuteActuelle := 0;
    FDerniereReset := Now;
  end;
end;

// Utilisation
function AppelerLLMAvecLimite(const Prompt: string): string;  
begin  
  RateLimiter.AttendreDisponibilite;
  Result := LLM.Chat(Prompt);
  RateLimiter.IncrementerCompteur;
end;
```

## Limitations et considérations

### Limites techniques

**1. Hallucinations**

Les LLM peuvent inventer des informations fausses avec confiance.

**Mitigation** :
```pascal
// Toujours vérifier les faits critiques
function VerifierFactsAvecLLM(const Info: string): Boolean;  
var  
  Prompt: string;
  Reponse: string;
begin
  Prompt := Format(
    'Cette information est-elle vérifiable et correcte ? ' +
    'Réponds uniquement par OUI ou NON.'#13#10 +
    'Information: %s',
    [Info]
  );

  Reponse := LLM.Chat(Prompt);
  Result := Reponse.ToUpper.Contains('OUI');

  // Pour info critique, validation humaine obligatoire
  if not Result then
    ShowMessage('Information à vérifier manuellement');
end;
```

**2. Connaissances datées**

Les LLM ont une date de coupure des connaissances.

**Solution** : Intégrer avec recherche web ou bases de données à jour.

**3. Biais**

Les modèles peuvent avoir des biais culturels ou sociaux.

**Mitigation** : Tests diversifiés, prompts équilibrés.

### Considérations éthiques

**1. Transparence**

Informez toujours l'utilisateur qu'il interagit avec une IA.

```pascal
procedure TFormChat.FormCreate(Sender: TObject);  
begin  
  LabelInfo.Caption :=
    '💡 Vous discutez avec un assistant IA. ' +
    'Les réponses sont générées automatiquement.';
end;
```

**2. Données sensibles**

Ne jamais envoyer de données confidentielles sans consentement.

```pascal
function TexteContientDonneesSensibles(const Texte: string): Boolean;  
begin  
  Result :=
    TRegEx.IsMatch(Texte, '\b[\w\.-]+@[\w\.-]+\.\w+\b') or // Email
    TRegEx.IsMatch(Texte, '\b\d{16}\b') or // Numéro carte
    TRegEx.IsMatch(Texte, '\b\d{2}/\d{2}/\d{4}\b'); // Date naissance

  if Result then
    ShowMessage('Attention: Le texte contient des données sensibles');
end;
```

**3. Responsabilité**

Pour les décisions importantes, toujours avoir validation humaine.

```pascal
function PrendreDecisionCritique(const Contexte: string): string;  
var  
  SuggestionIA: string;
begin
  SuggestionIA := LLM.Chat('Suggère une décision pour: ' + Contexte);

  Result := SuggestionIA;

  // Demander validation humaine
  if MessageDlg(
    'L''IA suggère: ' + SuggestionIA + #13#10#13#10 +
    'Approuvez-vous cette décision ?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
  begin
    Result := InputBox('Décision', 'Entrez votre décision:', '');
  end;
end;
```

## Conclusion

Les grands modèles de langage représentent une révolution pour le développement d'applications intelligentes avec Delphi. Ils permettent de créer des interfaces conversationnelles naturelles, d'automatiser des tâches complexes et d'enrichir l'expérience utilisateur de manière inédite.

**Points essentiels** :
- Les LLM comprennent et génèrent du langage naturel de manière sophistiquée
- L'intégration avec Delphi se fait facilement via API REST
- Le prompting (art de formuler les instructions) est crucial pour la qualité
- La gestion du contexte et des conversations nécessite une architecture réfléchie
- Cache, rate limiting et gestion d'erreurs sont indispensables
- Toujours considérer les aspects éthiques et de confidentialité

**Recommandations** :
1. Commencez avec GPT-3.5 Turbo (bon compromis coût/qualité)
2. Testez différents providers pour trouver le meilleur pour votre cas
3. Investissez du temps dans le crafting de bons prompts
4. Implémentez cache et monitoring dès le début
5. Gardez toujours un contrôle humain sur les décisions critiques

Les LLM ne sont pas une solution magique à tous les problèmes, mais utilisés judicieusement, ils transforment radicalement ce qui est possible dans vos applications Delphi. Avec les techniques présentées dans ce chapitre, vous êtes maintenant équipé pour créer des applications véritablement intelligentes et conversationnelles.

Dans la section suivante, nous explorerons les composants IA intégrés de Delphi 13 Florence qui facilitent encore davantage ces intégrations !

⏭️ [Composants IA intégrés de Delphi 13](/22-intelligence-artificielle-et-machine-learning-avec-delphi/08-composants-ia-integres-delphi-13.md)
