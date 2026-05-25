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

> ⚠️ **Le marché des LLM évolue très vite.** Les noms de modèles, capacités et tarifs ci-dessous reflètent l'état général du marché 2025-2026 mais sont fournis à titre indicatif. Consultez systématiquement la documentation officielle de chaque fournisseur avant de figer un choix de modèle ou un budget.

### Famille GPT (OpenAI)

**Caractéristiques** :
- Le plus connu et largement utilisé
- Famille de modèles : `gpt-4o`, `gpt-4o-mini`, série `gpt-5*` (selon disponibilités)
- Vision multimodale **native** (intégrée à GPT-4o, plus de modèle vision séparé)
- `gpt-3.5-turbo` reste accessible mais legacy

**Points forts** :
- Qualité générale exceptionnelle
- Suivi d'instructions précis
- Raisonnement avancé
- Large adoption et documentation
- Outils d'agent (function calling, file search, code interpreter)

**Limitations** :
- Coûteux pour gros volumes (privilégier `mini`)
- Connaissances limitées à la date de coupure d'entraînement (sauf web search/tools)
- Parfois verbeux

**Tarification** (ordres de grandeur 2025-2026) :
- `gpt-4o` : ~2,50$ in / ~10$ out par million de tokens
- `gpt-4o-mini` : ~0,15$ in / ~0,60$ out par million de tokens
- Documentation tarifaire à jour : [platform.openai.com/docs/pricing](https://platform.openai.com/docs/pricing)

### Claude (Anthropic)

**Caractéristiques** :
- Concurrent direct des modèles GPT
- Fenêtre de contexte massive : 200K tokens en standard, **1M tokens** sur les modèles Claude 4.x récents
- Focus sur la sécurité et l'éthique
- Famille Claude 4.x : `claude-opus-4` (haut de gamme), `claude-sonnet-4` (équilibré), `claude-haiku-4` (rapide/économique)

**Points forts** :
- Excellent pour documents longs (très grands contextes)
- Refus approprié des requêtes inappropriées
- Très bon en raisonnement et génération de code
- Conversations naturelles

**Limitations** :
- API tarifée (pas de tier gratuit étendu)
- Disponibilité régionale variable

**Tarification** (ordres de grandeur 2025-2026) :
- Claude Opus 4 : tarif premium (~15$ in / ~75$ out par million de tokens)
- Claude Sonnet 4 : intermédiaire (~3$ in / ~15$ out)
- Claude Haiku 4 : économique (sub-1$ in / quelques $ out)
- Documentation à jour : [docs.anthropic.com/claude/docs](https://docs.anthropic.com/)

### Gemini (Google)

**Caractéristiques** :
- LLM de Google
- Multimodal natif (texte, image, audio, vidéo)
- Intégration avec l'écosystème Google Cloud (Vertex AI)

**Points forts** :
- Niveau gratuit avec quotas généreux (Gemini API)
- Multimodalité native (vidéo notamment)
- Performances compétitives sur de nombreux benchmarks

**Limitations** :
- API en évolution (changements de noms de modèles fréquents)
- Disponibilité régionale variable selon les modèles

### Llama (Meta)

**Caractéristiques** :
- Open source (poids du modèle publics)
- Peut être hébergé localement ou via fournisseurs tiers
- Plusieurs tailles (de quelques milliards à plusieurs centaines de milliards de paramètres)

**Points forts** :
- Gratuit si auto-hébergé
- Contrôle total
- Pas de limite de requêtes
- Confidentialité maximale

**Limitations** :
- Nécessite infrastructure GPU (ou utilisation via fournisseurs hébergés)
- Complexité de déploiement
- Qualité variable selon la taille du modèle

**Utilisation via API** :
- Plusieurs fournisseurs offrent Llama hébergé (Together AI, Groq, Replicate, Fireworks…)

### Mistral AI

**Caractéristiques** :
- Startup française
- Modèles performants et compacts (Mistral, Mixtral, Codestral pour le code)
- Modèles open weight + API SaaS

**Points forts** :
- Excellent rapport qualité/prix
- Modèles européens (RGPD, souveraineté)
- Performants sur français
- Modèles spécialisés (code, multilingue, …)

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
// Estimation approximative (pas exacte).
// Pour le français, prévoir une marge supplémentaire : les accents et la
// morphologie produisent en moyenne plus de tokens par caractère qu'en anglais.
function EstimerNombreTokens(const Texte: string; FrancaisOuLatin: Boolean = True): Integer;  
const  
  CARS_PAR_TOKEN_EN = 4;  // ~4 caractères/token en anglais
  CARS_PAR_TOKEN_FR = 3;  // ~3 caractères/token en français (plus pessimiste)
var
  CarsParToken: Integer;
begin
  if FrancaisOuLatin then
    CarsParToken := CARS_PAR_TOKEN_FR
  else
    CarsParToken := CARS_PAR_TOKEN_EN;
  Result := Round(Length(Texte) / CarsParToken);
end;
```

> ℹ️ **Pour un calcul exact** : utilisez la bibliothèque de tokenisation officielle du fournisseur — par exemple `tiktoken` (OpenAI) ou `anthropic.tokenizers` (Anthropic). En Delphi, vous pouvez les appeler via Python4Delphi, ou exposer un micro-service Python local qui retourne le nombre exact de tokens.

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

**Exemples (ordres de grandeur)** :
- `gpt-3.5-turbo` : 16K tokens (~12 000 mots)
- `gpt-4o` / `gpt-4o-mini` : 128K tokens (~96 000 mots)
- Claude 4.x : 200K tokens en standard, jusqu'à **1M tokens** (~750 000 mots) sur certains modèles récents
- Gemini 1.5 Pro / 2.0 : jusqu'à 1M-2M tokens selon le modèle

**Importance** :
- Détermine la longueur des documents analysables
- Limite la longueur des conversations
- Plus grand = plus cher

**Gestion pratique** :

```pascal
const
  // Limites types pour `gpt-4o-mini` / `gpt-4o` (128K). Adapter selon le modèle
  // utilisé : Claude 4 → 200K-1M, Gemini → jusqu'à 2M, gpt-3.5-turbo → 16K, etc.
  MAX_CONTEXT_TOKENS = 128000;
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

> 💡 **Solutions officielles disponibles** : Avant d'écrire un wrapper personnalisé, considérez les options officielles :  
> - **SmartCore AI Component Pack** (officiel Embarcadero via GetIt) — multi-providers (OpenAI, Claude, Gemini, Ollama) avec `TAIConnection`, `TAIChatRequest`, etc. Voir section 22.8.  
> - **OpenAI for Delphi** (communautaire via GetIt) — wrapper spécifique OpenAI  
> - **Anthropic API wrapper for Delphi** (communautaire via GetIt) — wrapper spécifique Claude avec vision et MCP  
>  
> Le code ci-dessous reste précieux pour **comprendre le mécanisme** des APIs LLM et pour créer des wrappers personnalisés répondant à des besoins métier spécifiques.

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
  SystemPrompt: string;
  MessagesAjouteAResult: Boolean;
begin
  Result := TJSONObject.Create;
  Messages := TJSONArray.Create;
  MessagesAjouteAResult := False;

  try
    // ⚠️ L'API Anthropic Claude exige que les messages 'system' soient passés
    // dans un paramètre TOP-LEVEL `system`, PAS dans le tableau messages
    // (sinon erreur : "system role not allowed in messages"). On extrait donc
    // le system prompt et on l'ajoute séparément pour Anthropic.
    SystemPrompt := '';
    for Msg in FMessages do
    begin
      if (FConfig.Provider = lpAnthropic) and (Msg.Role = 'system') then
      begin
        // Pour Anthropic : concaténer les system prompts dans une variable
        if SystemPrompt <> '' then
          SystemPrompt := SystemPrompt + #13#10;
        SystemPrompt := SystemPrompt + Msg.Content;
        Continue;
      end;

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
        MessagesAjouteAResult := True;
        Result.AddPair('temperature', TJSONNumber.Create(FConfig.Temperature));
        // ℹ️ `max_tokens` reste accepté par `gpt-4o*` et `gpt-3.5-turbo`.
        //    Sur les modèles GPT-5 et les modèles de raisonnement (`o1`, `o3`),
        //    OpenAI exige désormais `max_completion_tokens` à la place.
        Result.AddPair('max_tokens', TJSONNumber.Create(FConfig.MaxTokens));
      end;

      lpAnthropic:
      begin
        Result.AddPair('model', FConfig.Model);
        if SystemPrompt <> '' then
          Result.AddPair('system', SystemPrompt);
        Result.AddPair('messages', Messages);
        MessagesAjouteAResult := True;
        Result.AddPair('max_tokens', TJSONNumber.Create(FConfig.MaxTokens));
        Result.AddPair('temperature', TJSONNumber.Create(FConfig.Temperature));
      end;

      lpGoogle:
      begin
        // Google Gemini : structure différente, non implémentée ici
        // (voir https://ai.google.dev/api/generate-content pour les détails)
        raise ENotImplemented.Create(
          'Provider Google : structure de requête Gemini non implémentée dans cet exemple');
      end;
    end;
  except
    // ⚠️ Si on n'a pas encore transféré Messages à Result (cas Google ou
    // exception précoce), il faut le libérer manuellement.
    if not MessagesAjouteAResult then
      Messages.Free;
    Result.Free;
    raise;
  end;
end;

function TLLMClient.ExtractResponse(const JSON: TJSONObject): string;  
var  
  Path: string;
  V: TJSONValue;
begin
  // ⚠️ Important : `TJSONObject.GetValue<T>(Name)` ne supporte PAS les paths
  // dotted (il cherche une clé exactement nommée `choices[0]...`). C'est la
  // méthode `FindValue(Path)` qui implémente la spécification JSONPath
  // (`root.child`, `array[0]`, etc.). On l'utilise systématiquement ici.
  case FConfig.Provider of
    lpOpenAI, lpMistral:
      Path := 'choices[0].message.content';
    lpAnthropic:
      Path := 'content[0].text';
    lpGoogle:
      Path := 'candidates[0].content.parts[0].text';
  else
    Exit('');
  end;

  V := JSON.FindValue(Path);
  if Assigned(V) then
    Result := V.Value
  else
    Result := ''; // Format de réponse inattendu
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
      // ℹ️ `2023-06-01` reste la version stable et conseillée pour la majorité
      //    des intégrations (toujours valide en 2026). Anthropic publie
      //    régulièrement de nouvelles versions ; voir la doc officielle :
      //    https://docs.anthropic.com/en/api/versioning
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
    begin
      // ⚠️ Garde nil : si l'API renvoie un 200 mais avec un body non-JSON
      //    (rare mais possible via proxy/middleware), JSONValue est nil.
      if not Assigned(FRESTResponse.JSONValue) then
        raise Exception.Create('Réponse API : JSON invalide ou body vide');
      Result := ExtractResponse(FRESTResponse.JSONValue as TJSONObject);
    end
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
  try
    Result := SendRequest;
  except
    // ⚠️ Si SendRequest échoue, retirer le message 'user' ajouté pour ne pas
    //    polluer l'historique avec un message orphelin sans réponse assistant
    //    (l'API renverrait alors une erreur "messages must alternate" au
    //    prochain appel).
    if (FMessages.Count > 0) and (FMessages.Last.Role = 'user') then
      FMessages.Delete(FMessages.Count - 1);
    raise;
  end;
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
  Config.APIKey := 'votre-clé-api'; // ⚠️ En production : lire depuis configuration sécurisée
  Config.Model := 'gpt-4o-mini'; // ou 'gpt-4o' / 'claude-haiku-4' / 'mistral-large-latest'
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
  UserText: string;
begin
  if EditMessage.Text.Trim.IsEmpty then Exit;

  // ⚠️ Lire EditMessage.Text dans le thread principal AVANT TTask.Run.
  //    Accéder à un contrôle VCL/FMX depuis un thread secondaire n'est pas
  //    thread-safe et peut produire des comportements indéterminés.
  UserText := EditMessage.Text;

  // Afficher message utilisateur et vider le champ tout de suite côté UI
  AjouterMessageChat('Vous', UserText);
  EditMessage.Clear;

  // ⚠️ Désactiver le bouton AVANT de lancer la tâche : sans cela, l'utilisateur
  //    pourrait cliquer plusieurs fois et déclencher plusieurs `FLLMClient.Chat`
  //    en parallèle. TLLMClient n'est pas thread-safe (Messages et FRESTRequest
  //    partagés) → corruption de l'historique de conversation et de la requête.
  BtnEnvoyer.Enabled := False;

  // Traitement asynchrone
  TTask.Run(procedure
  var
    Response: string;
  begin
    try
      Response := FLLMClient.Chat(UserText);

      TThread.Synchronize(nil, procedure
      begin
        AjouterMessageChat('Assistant', Response);
        BtnEnvoyer.Enabled := True; // Réactiver après succès
      end);
    except
      on E: Exception do
      begin
        // Capturer le message AVANT Synchronize pour éviter de référencer E
        // depuis le thread principal (E n'existe plus après le `on E:` parent).
        var ErrMsg := E.Message;
        TThread.Synchronize(nil, procedure
        begin
          ShowMessage('Erreur: ' + ErrMsg);
          BtnEnvoyer.Enabled := True; // Réactiver aussi en cas d'erreur
        end);
      end;
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

> 💡 **Astuce 2026** : OpenAI propose un mode **JSON Mode** (`response_format: { type: "json_object" }`) et même un mode **Structured Outputs** (`response_format: { type: "json_schema", json_schema: {...} }`) qui **garantit** un JSON valide conforme à un schéma. Anthropic Claude propose un équivalent via le système de tools. Ces modes éliminent la plupart des erreurs de parsing JSON.

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

  // ⚠️ Le LLM enrobe souvent le JSON de texte explicatif ou de blocs ```json :
  // on extrait d'abord la portion utile, puis on parse.
  Reponse := ExtraireJSON(Reponse); // Définie plus bas (cas d'usage extracteur)

  // Parser le JSON retourné. Si ParseJSONValue échoue (JSON malformé) ou si
  // le résultat n'est pas un objet (ex. tableau), le cast renvoie nil.
  Result := TJSONObject.ParseJSONValue(Reponse) as TJSONObject;
  if not Assigned(Result) then
    raise Exception.Create('Le LLM n''a pas retourné un JSON valide.');
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
    'Corrige cet email (orthographe, grammaire, style professionnel).'#13#10 +
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

  // ⚠️ Le cast `as TJSONObject` renvoie nil si ParseJSONValue échoue ou si la
  // valeur n'est pas un objet (ex. tableau). On lève une exception explicite
  // plutôt que de retourner nil silencieusement.
  Result := TJSONObject.ParseJSONValue(Reponse) as TJSONObject;
  if not Assigned(Result) then
    raise Exception.Create(
      'Le LLM n''a pas retourné un JSON d''objet valide pour la facture.');
end;

function ExtraireJSON(const Texte: string): string;  
var  
  StartObj, StartArr, StartPos, EndPos: Integer;
begin
  // ⚠️ Cette fonction extrait soit un objet `{...}` soit un tableau `[...]`
  //    selon ce qui apparaît en premier. Pour des structures plus complexes
  //    (JSON imbriqué dans du markdown), envisagez une regex ou la
  //    fonctionnalité "JSON Mode" du LLM (response_format).
  StartObj := Pos('{', Texte);
  StartArr := Pos('[', Texte);

  if (StartObj > 0) and ((StartArr = 0) or (StartObj < StartArr)) then
  begin
    // Objet : extraire de { à la dernière }
    StartPos := StartObj;
    EndPos := LastDelimiter('}', Texte);
  end
  else if StartArr > 0 then
  begin
    // Tableau : extraire de [ à la dernière ]
    StartPos := StartArr;
    EndPos := LastDelimiter(']', Texte);
  end
  else
  begin
    Result := Texte;
    Exit;
  end;

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

// ⚠️⚠️⚠️ AVERTISSEMENT SÉCURITÉ CRITIQUE ⚠️⚠️⚠️
//
// Exécuter une requête SQL générée par un LLM est EXTRÊMEMENT DANGEREUX en
// production. Le LLM peut :
//   - Halluciner et générer `DROP TABLE Clients;` ou `DELETE FROM ...`
//   - Être manipulé par injection de prompt dans la demande utilisateur
//   - Ignorer les contraintes métier que vous n'avez pas explicitement listées
//
// Mesures de protection obligatoires en production :
//   1. Utiliser un compte BD en LECTURE SEULE pour les requêtes générées
//   2. Whitelist : autoriser uniquement SELECT, refuser tout DDL/DML
//   3. Parser le SQL côté serveur et rejeter les mots-clés interdits
//   4. Limiter aux tables explicitement listées dans le schéma fourni
//   5. Toujours afficher la requête à l'utilisateur AVANT exécution
//   6. Logger toutes les exécutions pour audit
//
// L'exemple ci-dessous est PÉDAGOGIQUE et n'inclut pas ces protections.

function EstSQLSeulementLecture(const SQL: string): Boolean;  
var  
  Normalized: string;
begin
  // Vérification minimale : on n'accepte que les SELECT. Cette fonction est un
  // garde-fou supplémentaire, PAS une protection complète (parser SQL recommandé).
  Normalized := UpperCase(Trim(SQL));
  // On enlève les commentaires SQL avant test (le LLM peut camoufler du DML)
  Normalized := TRegEx.Replace(Normalized, '--[^\r\n]*', '');
  Normalized := TRegEx.Replace(Normalized, '/\*.*?\*/', '', [roSingleLine]);
  Normalized := Trim(Normalized);

  Result := Normalized.StartsWith('SELECT')
        and not Normalized.Contains('DROP ')
        and not Normalized.Contains('DELETE ')
        and not Normalized.Contains('UPDATE ')
        and not Normalized.Contains('INSERT ')
        and not Normalized.Contains('ALTER ')
        and not Normalized.Contains('TRUNCATE ')
        and not Normalized.Contains('CREATE ')
        and not Normalized.Contains('EXEC ');
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

  // Garde-fou : refuser toute requête qui n'est pas un SELECT pur
  if not EstSQLSeulementLecture(SQL) then
  begin
    ShowMessage('⚠️ Requête refusée : seules les requêtes SELECT sont autorisées.');
    Exit;
  end;

  // Confirmation utilisateur explicite avant exécution (sur compte BD lecture seule)
  if MessageDlg('Exécuter cette requête ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ExecuterRequete(SQL); // ⚠️ Utiliser une connexion BD en lecture seule
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
  ErrMessage, ErrStackTrace: string;
begin
  // Logger l'erreur normalement
  LogError(E);

  // ⚠️ Capturer Message et StackTrace AVANT TTask.Run : l'objet E sera
  // libéré dès que ce handler retourne. Capturer E directement dans la
  // closure produirait une référence dangling consultée en arrière-plan.
  ErrMessage := E.Message;
  ErrStackTrace := E.StackTrace;

  // Demander diagnostic à l'IA (en arrière-plan)
  TTask.Run(procedure
  var
    Diag: string;
    DiagErrMsg: string;
  begin
    // ⚠️ Encapsuler dans try/except : si l'API IA est indisponible, l'erreur
    //    ne doit pas être silencieusement perdue (mais ne doit pas non plus
    //    interrompre l'affichage de l'erreur principale via ShowMessage).
    try
      Diag := DiagnostiquerErreur(ErrMessage, ErrStackTrace);

      TThread.Synchronize(nil, procedure
      begin
        // Afficher dans un panneau d'aide
        PanelDiagnostic.Visible := True;
        MemoDiagnostic.Text := Diag;
      end);
    except
      on E: Exception do
      begin
        DiagErrMsg := E.Message;
        TThread.Queue(nil, procedure
        begin
          // L'erreur principale a déjà été montrée — ici on log juste
          // l'échec du diagnostic IA.
          LogError('Diagnostic IA indisponible : ' + DiagErrMsg);
        end);
      end;
    end;
  end);

  // Afficher l'erreur à l'utilisateur
  ShowMessage(ErrMessage);
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

constructor TConversationManager.Create(const LLMClient: TLLMClient;
  MaxMessages: Integer; MaxTokens: Integer);
begin
  inherited Create;
  FLLM := LLMClient;            // ownership conservé par l'appelant
  FMaxMessages := MaxMessages;
  FMaxTokens := MaxTokens;
end;

procedure TConversationManager.ReinitialiserConversation;  
begin  
  FLLM.ClearMessages;
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
  I: Integer;
begin
  TokensActuels := EstimerTokensConversation;

  // Si dépassement, supprimer les plus anciens messages non-system.
  // ⚠️ La version naïve `while ... do if Role<>'system' then Delete(1)`
  // boucle INFINIMENT si Messages[1] est aussi un message system : la
  // condition reste vraie mais aucun message n'est supprimé.
  // On cherche donc le PREMIER index non-system à supprimer.
  while (TokensActuels > FMaxTokens) and (FLLM.Messages.Count > 1) do
  begin
    // Trouver le premier message non-system
    I := -1;
    for var J := 0 to FLLM.Messages.Count - 1 do
      if FLLM.Messages[J].Role <> 'system' then
      begin
        I := J;
        Break;
      end;

    if I < 0 then
      Break; // Plus que des messages system : on ne peut plus tronquer

    FLLM.Messages.Delete(I);
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

    // ⚠️ Pattern try/finally imbriqué : si TStreamWriter.Create ou Writer.Free
    // lèvent une exception, on doit garantir la libération de FileStream.
    FileStream := TFileStream.Create(Fichier, fmCreate);
    try
      Writer := TStreamWriter.Create(FileStream);
      try
        Writer.Write(JSON.ToString);
      finally
        Writer.Free;
      end;
    finally
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

> ⚠️ **Limite de TRESTClient avec SSE** : `TRESTClient` ne supporte pas nativement le streaming Server-Sent Events (SSE) — il attend la fin du transfert avant de remettre la réponse. Pour du **vrai** streaming token-par-token, utilisez `TNetHTTPClient` avec un `TStream` consommé ligne par ligne, ou la bibliothèque `Indy.IdHTTP` avec gestion de l'événement `OnWork`. L'exemple ci-dessous fait du **pseudo-streaming** : il parse la réponse SSE complète après réception, ce qui n'apporte pas l'effet "frappe progressive" mais reste utile pour découper la réponse en morceaux exploitables.

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
        // ⚠️ ParseJSONValue peut retourner nil si le fragment SSE est malformé
        if not Assigned(JSONObj) then
          Continue;
        try
          // FindValue (et non GetValue) pour les paths JSONPath
          var DeltaVal := JSONObj.FindValue('choices[0].delta.content');
          if not Assigned(DeltaVal) then
            Continue;
          Delta := DeltaVal.Value;

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
  Delai: Integer;
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

        if Tentatives >= MaxRetries then
          raise; // Propager l'exception après max retries

        // Backoff exponentiel : 1s, 2s, 4s, 8s... (et non linéaire)
        Delai := 1000 * (1 shl (Tentatives - 1)); // 2^(N-1) secondes
        Sleep(Delai);
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
    function GetHitRate: Double;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const Prompt: string; out Response: string): Boolean;
    procedure Put(const Prompt, Response: string);

    // Ratio hits/(hits+misses), entre 0.0 et 1.0
    property HitRate: Double read GetHitRate;
  end;

constructor TLLMCache.Create;  
begin  
  inherited;
  FCache := TDictionary<string, string>.Create;
end;

destructor TLLMCache.Destroy;  
begin  
  FCache.Free;
  inherited;
end;

function TLLMCache.CalculerHash(const Prompt: string): string;  
var  
  HashBytes: TBytes;
begin
  // ⚠️ Utiliser TEncoding.UTF8.GetBytes pour gérer correctement les accents
  //    français : sans cela, BytesOf utilise l'encodage ANSI et deux chaînes
  //    contenant "é" peuvent produire des hash différents selon la code page.
  HashBytes := THashMD5.GetHashBytes(TEncoding.UTF8.GetBytes(Prompt));
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

procedure TLLMCache.Put(const Prompt, Response: string);  
begin  
  FCache.AddOrSetValue(CalculerHash(Prompt), Response);
end;

function TLLMCache.GetHitRate: Double;  
begin  
  if (FHits + FMisses) = 0 then
    Exit(0.0);
  Result := FHits / (FHits + FMisses);
end;

// Utilisation avec cache (Cache et LLM sont des variables globales pédagogiques)
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

constructor TRateLimiter.Create(MaxRequetesParMinute: Integer);  
begin  
  inherited Create;
  FMaxRequetesParMinute := MaxRequetesParMinute;
  FRequetesMinuteActuelle := 0;
  FDerniereReset := Now;
end;

procedure TRateLimiter.IncrementerCompteur;  
begin  
  Inc(FRequetesMinuteActuelle);
end;

procedure TRateLimiter.AttendreDisponibilite;  
var  
  MinutesEcoulees: Integer;
  MillisAttendre: Int64;
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
    // Attendre la prochaine minute.
    // ⚠️ Sleep(Cardinal) interprète une valeur négative comme un très grand
    // entier non signé → blocage de ~50 jours. On clamp donc à [0, 60000].
    MillisAttendre := 60000 - (SecondsBetween(Now, FDerniereReset) * 1000);
    if MillisAttendre < 0 then
      MillisAttendre := 0
    else if MillisAttendre > 60000 then
      MillisAttendre := 60000;
    Sleep(MillisAttendre);
    FRequetesMinuteActuelle := 0;
    FDerniereReset := Now;
  end;
end;

// Utilisation
// ⚠️ Cette fonction est SYNCHRONE et peut bloquer jusqu'à ~60 secondes.
//    À appeler depuis un thread secondaire (TTask.Run) — JAMAIS depuis le
//    thread UI, sinon l'interface se gèle pendant l'attente.
function AppelerLLMAvecLimite(const Prompt: string): string;  
begin  
  RateLimiter.AttendreDisponibilite;
  Result := LLM.Chat(Prompt);
  RateLimiter.IncrementerCompteur;
end;
```

> ℹ️ **Note thread-safety** : Le `TRateLimiter` ci-dessus n'est PAS thread-safe. Si plusieurs threads l'appellent simultanément, `FRequetesMinuteActuelle` peut être incrémenté de manière incohérente. Pour un usage multi-thread, encapsulez les opérations dans un `TCriticalSection` ou utilisez `TInterlocked.Increment`.

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
1. Commencez avec un modèle économique : `gpt-4o-mini` ou `claude-haiku-4` (bon compromis coût/qualité)
2. Testez différents providers pour trouver le meilleur pour votre cas d'usage et votre langue
3. Investissez du temps dans le crafting de bons prompts (souvent plus rentable qu'un modèle plus cher)
4. Implémentez cache et monitoring dès le début
5. Gardez toujours un contrôle humain sur les décisions critiques
6. Surveillez les annonces de dépréciation : la liste des modèles évolue plusieurs fois par an

Les LLM ne sont pas une solution magique à tous les problèmes, mais utilisés judicieusement, ils transforment radicalement ce qui est possible dans vos applications Delphi. Avec les techniques présentées dans ce chapitre, vous êtes maintenant équipé pour créer des applications véritablement intelligentes et conversationnelles.

Dans la section suivante, nous explorerons les composants IA intégrés de Delphi 13 Florence qui facilitent encore davantage ces intégrations !

⏭️ [Composants IA intégrés de Delphi 13](/22-intelligence-artificielle-et-machine-learning-avec-delphi/08-composants-ia-integres-delphi-13.md)
