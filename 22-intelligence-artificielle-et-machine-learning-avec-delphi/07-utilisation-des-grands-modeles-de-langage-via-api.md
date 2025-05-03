# 22.7 Utilisation des grands modèles de langage (LLM) via API

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les grands modèles de langage (Large Language Models ou LLM) représentent une avancée majeure dans le domaine de l'intelligence artificielle. Ces modèles, comme GPT-4, Claude, ou LLaMA, peuvent comprendre et générer du texte de façon quasi-humaine. Dans cette section, nous allons découvrir comment intégrer ces puissants outils à vos applications Delphi.

## Qu'est-ce qu'un grand modèle de langage ?

Un LLM est un type d'intelligence artificielle entraîné sur d'immenses quantités de texte pour apprendre les structures et les relations du langage humain. Ces modèles peuvent :

- Générer du texte cohérent et contextuel
- Répondre à des questions
- Résumer des informations
- Traduire des langues
- Écrire différents types de contenu (code, articles, etc.)
- Analyser des textes et en extraire des informations

## Pourquoi intégrer des LLM dans vos applications ?

L'intégration de LLM peut considérablement améliorer les capacités de vos applications :

- **Assistance utilisateur** : Créez des chatbots ou assistants intelligents
- **Automatisation de contenu** : Générez des descriptions, des résumés ou des rapports
- **Analyse de données textuelles** : Extrayez des informations, des sentiments ou des tendances
- **Amélioration de l'expérience utilisateur** : Offrez des fonctionnalités de recherche avancée ou d'aide contextuelle

## Principaux fournisseurs de LLM accessibles via API

Plusieurs entreprises proposent des API pour accéder à leurs LLM :

1. **OpenAI** (GPT-3.5, GPT-4)
2. **Anthropic** (Claude)
3. **Google** (Gemini)
4. **Cohere** (Command)
5. **Meta** (LLaMA)
6. **Mistral AI** (Mistral, Mixtral)

Dans cette section, nous nous concentrerons principalement sur l'API OpenAI, la plus populaire et accessible, mais les principes s'appliquent à tous les fournisseurs.

## Configuration préalable

Avant de commencer, vous aurez besoin de :

1. Un compte chez le fournisseur d'API de votre choix
2. Une clé API (obtenue après inscription)
3. Delphi 11 Alexandria ou supérieur
4. Connaissance de base des composants REST de Delphi

## Intégration avec l'API OpenAI

### Étape 1 : Configurer un projet Delphi

Créez un nouveau projet d'application VCL ou FireMonkey et ajoutez les composants nécessaires pour les requêtes REST :

```pascal
uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Types, REST.Client, REST.Response.Adapter;
```

### Étape 2 : Créer une fonction pour appeler l'API GPT

```pascal
function CallGPTAPI(const APIKey, Prompt: string; MaxTokens: Integer = 500;
  Temperature: Double = 0.7): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestJSON: TJSONObject;
  MessagesArray: TJSONArray;
  MessageObject: TJSONObject;
  ResponseJSON: TJSONValue;
begin
  Result := '';

  // Création des objets REST
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  // Création des objets JSON pour la requête
  RequestJSON := TJSONObject.Create;
  MessagesArray := TJSONArray.Create;
  MessageObject := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://api.openai.com/v1/chat/completions';
    RESTClient.Accept := 'application/json';
    RESTClient.ContentType := 'application/json';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de l'en-tête d'autorisation avec la clé API
    RESTRequest.AddParameter('Authorization', 'Bearer ' + APIKey,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    // Préparation du message dans le format attendu par l'API
    MessageObject.AddPair('role', 'user');
    MessageObject.AddPair('content', Prompt);
    MessagesArray.Add(MessageObject);

    // Configuration des paramètres de la requête
    RequestJSON.AddPair('model', 'gpt-3.5-turbo'); // ou 'gpt-4' pour une version plus avancée
    RequestJSON.AddPair('messages', MessagesArray);
    RequestJSON.AddPair('max_tokens', TJSONNumber.Create(MaxTokens));
    RequestJSON.AddPair('temperature', TJSONNumber.Create(Temperature));

    // Ajout du corps de la requête
    RESTRequest.Body.Add(RequestJSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    try
      // Exécution de la requête
      RESTRequest.Execute;

      // Traitement de la réponse si le statut est OK (200)
      if RESTResponse.StatusCode = 200 then
      begin
        // Analyse de la réponse JSON
        ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
        try
          // Extraction du texte généré par le modèle
          Result := ResponseJSON.GetValue<string>('choices[0].message.content');
        finally
          ResponseJSON.Free;
        end;
      end
      else
      begin
        // En cas d'erreur, renvoyer le message d'erreur
        Result := 'Erreur ' + RESTResponse.StatusCode.ToString + ': ' +
                  RESTResponse.StatusText + #13#10 + RESTResponse.Content;
      end;
    except
      on E: Exception do
        Result := 'Exception: ' + E.Message;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    // Notez que MessageObject sera libéré avec MessagesArray
    MessagesArray.Free;
    RequestJSON.Free;
  end;
end;
```

### Étape 3 : Utiliser la fonction dans votre application

```pascal
procedure TFormMain.ButtonGenerateClick(Sender: TObject);
const
  API_KEY = 'sk-votre_clé_api_ici'; // Ne jamais exposer votre clé API en production !
var
  Prompt: string;
  Response: string;
begin
  // Désactivation du bouton pendant le traitement
  ButtonGenerate.Enabled := False;
  Cursor := crHourGlass;
  try
    // Récupération du prompt saisi par l'utilisateur
    Prompt := MemoPrompt.Text;

    // Vérification que le prompt n'est pas vide
    if Prompt.Trim.IsEmpty then
    begin
      ShowMessage('Veuillez entrer un prompt !');
      Exit;
    end;

    // Appel à l'API GPT (dans un thread séparé pour une meilleure réactivité)
    TThread.CreateAnonymousThread(
      procedure
      var
        GeneratedText: string;
      begin
        // Appel à l'API
        GeneratedText := CallGPTAPI(API_KEY, Prompt);

        // Mise à jour de l'interface utilisateur dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            MemoResponse.Text := GeneratedText;
            ButtonGenerate.Enabled := True;
            Cursor := crDefault;
          end);
      end).Start;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur: ' + E.Message);
      ButtonGenerate.Enabled := True;
      Cursor := crDefault;
    end;
  end;
end;
```

## Création d'une interface utilisateur simple

Pour une application de démonstration, nous pouvons créer une interface simple de type "chatbot" :

```pascal
procedure TFormChatbot.FormCreate(Sender: TObject);
begin
  // Configuration de l'interface
  Caption := 'Assistant IA - Delphi LLM Demo';

  // Conteneur principal
  PanelMain := TPanel.Create(Self);
  PanelMain.Parent := Self;
  PanelMain.Align := alClient;

  // Panneau pour la conversation
  PanelChat := TPanel.Create(Self);
  PanelChat.Parent := PanelMain;
  PanelChat.Align := alClient;
  PanelChat.Height := ClientHeight - 100;

  // Memo pour afficher la conversation
  MemoChat := TMemo.Create(Self);
  MemoChat.Parent := PanelChat;
  MemoChat.Align := alClient;
  MemoChat.ReadOnly := True;
  MemoChat.ScrollBars := ssVertical;

  // Panneau pour la saisie
  PanelInput := TPanel.Create(Self);
  PanelInput.Parent := PanelMain;
  PanelInput.Align := alBottom;
  PanelInput.Height := 100;

  // Zone de saisie du message
  EditMessage := TEdit.Create(Self);
  EditMessage.Parent := PanelInput;
  EditMessage.Align := alClient;
  EditMessage.AlignWithMargins := True;

  // Bouton d'envoi
  ButtonSend := TButton.Create(Self);
  ButtonSend.Parent := PanelInput;
  ButtonSend.Align := alRight;
  ButtonSend.Caption := 'Envoyer';
  ButtonSend.Width := 100;
  ButtonSend.AlignWithMargins := True;
  ButtonSend.OnClick := ButtonSendClick;

  // Message d'accueil
  MemoChat.Lines.Add('Assistant: Bonjour ! Comment puis-je vous aider aujourd''hui ?');

  // Définir la touche Entrée pour envoyer le message
  EditMessage.OnKeyPress := EditMessageKeyPress;
end;

procedure TFormChatbot.EditMessageKeyPress(Sender: TObject; var Key: Char);
begin
  // Permettre d'envoyer le message avec la touche Entrée
  if Key = #13 then
  begin
    Key := #0; // Empêcher le son de la touche Entrée
    ButtonSendClick(Sender);
  end;
end;

procedure TFormChatbot.ButtonSendClick(Sender: TObject);
var
  UserMessage: string;
begin
  UserMessage := EditMessage.Text.Trim;

  // Vérifier si le message n'est pas vide
  if UserMessage = '' then Exit;

  // Afficher le message de l'utilisateur
  MemoChat.Lines.Add('Vous: ' + UserMessage);
  MemoChat.Lines.Add('');
  MemoChat.Lines.Add('Assistant: En cours de réflexion...');

  // Effacer le champ de saisie
  EditMessage.Text := '';

  // Désactiver le bouton pendant le traitement
  ButtonSend.Enabled := False;

  // Exécuter la requête API dans un thread séparé
  TThread.CreateAnonymousThread(
    procedure
    var
      Response: string;
    begin
      // Appel à l'API LLM
      Response := CallGPTAPI(LoadAPIKey, UserMessage);

      // Mise à jour de l'interface dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          // Supprimer le message "En cours de réflexion..."
          MemoChat.Lines.Delete(MemoChat.Lines.Count - 1);

          // Afficher la réponse
          MemoChat.Lines.Add('Assistant: ' + Response);
          MemoChat.Lines.Add('');

          // Faire défiler jusqu'au bas
          SendMessage(MemoChat.Handle, WM_VSCROLL, SB_BOTTOM, 0);

          // Réactiver le bouton
          ButtonSend.Enabled := True;

          // Remettre le focus sur le champ de saisie
          EditMessage.SetFocus;
        end);
    end).Start;
end;

// Fonction pour charger la clé API depuis un fichier sécurisé
function TFormChatbot.LoadAPIKey: string;
var
  KeyFile: TStringList;
  ConfigFile: string;
begin
  Result := '';
  KeyFile := TStringList.Create;
  try
    ConfigFile := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'config.ini';

    // Vérifier si le fichier existe
    if FileExists(ConfigFile) then
    begin
      KeyFile.LoadFromFile(ConfigFile);
      if KeyFile.Count > 0 then
        Result := KeyFile[0];
    end;

    // Si la clé n'est pas trouvée, demander à l'utilisateur
    if Result = '' then
    begin
      Result := InputBox('Configuration', 'Veuillez entrer votre clé API OpenAI:', '');

      // Sauvegarder la clé pour la prochaine utilisation
      if Result <> '' then
      begin
        KeyFile.Clear;
        KeyFile.Add(Result);
        try
          KeyFile.SaveToFile(ConfigFile);
        except
          // Ignorer les erreurs d'écriture
        end;
      end;
    end;
  finally
    KeyFile.Free;
  end;
end;
```

## Conseils de sécurité

### Ne jamais exposer votre clé API dans le code source
Au lieu de coder en dur votre clé API, stockez-la de manière sécurisée :

```pascal
// Méthode recommandée pour stocker la clé API
function GetAPIKeySecurely: string;
var
  Registry: TRegistry;
begin
  Result := '';
  Registry := TRegistry.Create(KEY_READ);
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\YourCompany\YourApp', False) then
    begin
      if Registry.ValueExists('APIKey') then
        Result := Registry.ReadString('APIKey');
    end;
  finally
    Registry.Free;
  end;

  // Si la clé n'existe pas encore, demander à l'utilisateur
  if Result = '' then
    Result := PromptForAPIKey;
end;

// Fonction pour demander et stocker la clé API
function PromptForAPIKey: string;
var
  Registry: TRegistry;
begin
  Result := InputBox('Configuration',
    'Veuillez entrer votre clé API OpenAI (elle sera stockée de façon sécurisée):', '');

  if Result <> '' then
  begin
    Registry := TRegistry.Create(KEY_WRITE);
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      if Registry.OpenKey('Software\YourCompany\YourApp', True) then
        Registry.WriteString('APIKey', Result);
    finally
      Registry.Free;
    end;
  end;
end;
```

## Gestion des coûts

L'utilisation des API LLM est généralement facturée selon ces critères :
- Le nombre de jetons (tokens) traités
- Le modèle utilisé (les plus avancés sont plus coûteux)

Pour contrôler vos dépenses :

```pascal
// Fonctions utilitaires pour estimer les coûts
function EstimateTokenCount(const Text: string): Integer;
begin
  // Estimation approximative : 1 jeton ≈ 4 caractères pour les langues occidentales
  Result := Length(Text) div 4;
end;

function EstimateCost(InputTokens, OutputTokens: Integer; const Model: string): Double;
begin
  // Prix approximatifs (à mettre à jour selon les tarifs actuels)
  if Model = 'gpt-3.5-turbo' then
    Result := (InputTokens * 0.0015 + OutputTokens * 0.002) / 1000
  else if Model = 'gpt-4' then
    Result := (InputTokens * 0.03 + OutputTokens * 0.06) / 1000
  else
    Result := 0;
end;
```

## Exemples pratiques d'utilisation des LLM

### 1. Assistant d'écriture

```pascal
procedure GenerateContentIdeas(const Topic: string);
const
  PROMPT_TEMPLATE = 'Génère 5 idées d''articles sur le sujet: %s. ' +
                    'Pour chaque idée, donne un titre accrocheur et ' +
                    'un court résumé de 2-3 phrases.';
var
  Prompt: string;
  Response: string;
begin
  Prompt := Format(PROMPT_TEMPLATE, [Topic]);
  Response := CallGPTAPI(GetAPIKeySecurely, Prompt);

  MemoResults.Lines.Clear;
  MemoResults.Lines.Add('IDÉES D''ARTICLES SUR: ' + Topic);
  MemoResults.Lines.Add('');
  MemoResults.Lines.Add(Response);
end;
```

### 2. Analyse de sentiment

```pascal
procedure AnalyzeSentiment(const Text: string);
const
  PROMPT_TEMPLATE = 'Analyse le sentiment du texte suivant et donne ' +
                    'une évaluation (positif, négatif ou neutre) avec ' +
                    'une explication brève: "%s"';
var
  Prompt: string;
  Response: string;
begin
  Prompt := Format(PROMPT_TEMPLATE, [Text]);
  Response := CallGPTAPI(GetAPIKeySecurely, Prompt);

  LabelSentiment.Caption := 'Analyse: ' + Response;
end;
```

### 3. Traduction avancée avec contexte

```pascal
procedure TranslateWithContext(const Text, SourceLang, TargetLang, Context: string);
const
  PROMPT_TEMPLATE = 'Traduis le texte suivant du %s vers le %s. ' +
                    'Contexte: %s. ' +
                    'Texte à traduire: "%s"';
var
  Prompt: string;
  Response: string;
begin
  Prompt := Format(PROMPT_TEMPLATE, [SourceLang, TargetLang, Context, Text]);
  Response := CallGPTAPI(GetAPIKeySecurely, Prompt);

  MemoTranslation.Text := Response;
end;
```

### 4. Génération de code

```pascal
procedure GenerateDelphiCode(const Requirement: string);
const
  PROMPT_TEMPLATE = 'Écris une fonction en Delphi (Object Pascal) qui: %s. ' +
                    'Inclus des commentaires pour expliquer le code. ' +
                    'Utilise les conventions de nommage standard de Delphi.';
var
  Prompt: string;
  Response: string;
begin
  Prompt := Format(PROMPT_TEMPLATE, [Requirement]);
  Response := CallGPTAPI(GetAPIKeySecurely, Prompt, 1000, 0.5); // Tokens plus élevés, température plus basse pour du code

  MemoCode.Text := Response;
end;
```

## Avancé : création d'un système de chat avec historique

Pour les applications nécessitant une "mémoire" des échanges précédents :

```pascal
type
  TChatMessage = record
    Role: string; // 'system', 'user', ou 'assistant'
    Content: string;
  end;

  TChatHistory = class
  private
    FMessages: TArray<TChatMessage>;
  public
    procedure AddMessage(const Role, Content: string);
    function GetMessagesAsJSON: TJSONArray;
    procedure Clear;
    property Messages: TArray<TChatMessage> read FMessages;
  end;

{ TChatHistory }

procedure TChatHistory.AddMessage(const Role, Content: string);
var
  Msg: TChatMessage;
begin
  Msg.Role := Role;
  Msg.Content := Content;
  SetLength(FMessages, Length(FMessages) + 1);
  FMessages[High(FMessages)] := Msg;
end;

function TChatHistory.GetMessagesAsJSON: TJSONArray;
var
  Msg: TChatMessage;
  MsgObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for Msg in FMessages do
  begin
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('role', Msg.Role);
    MsgObj.AddPair('content', Msg.Content);
    Result.Add(MsgObj);
  end;
end;

procedure TChatHistory.Clear;
begin
  SetLength(FMessages, 0);
end;

// Fonction d'appel API modifiée pour utiliser l'historique
function CallGPTAPIWithHistory(const APIKey: string; ChatHistory: TChatHistory;
  const NewUserMessage: string; MaxTokens: Integer = 500; Temperature: Double = 0.7): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestJSON: TJSONObject;
  MessagesArray: TJSONArray;
  ResponseJSON: TJSONValue;
begin
  Result := '';

  // Ajout du nouveau message à l'historique
  ChatHistory.AddMessage('user', NewUserMessage);

  // Création des objets REST
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  // Création de l'objet JSON pour la requête
  RequestJSON := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://api.openai.com/v1/chat/completions';
    RESTClient.Accept := 'application/json';
    RESTClient.ContentType := 'application/json';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de l'en-tête d'autorisation avec la clé API
    RESTRequest.AddParameter('Authorization', 'Bearer ' + APIKey,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    // Obtention des messages de l'historique au format JSON
    MessagesArray := ChatHistory.GetMessagesAsJSON;

    // Configuration des paramètres de la requête
    RequestJSON.AddPair('model', 'gpt-3.5-turbo');
    RequestJSON.AddPair('messages', MessagesArray);
    RequestJSON.AddPair('max_tokens', TJSONNumber.Create(MaxTokens));
    RequestJSON.AddPair('temperature', TJSONNumber.Create(Temperature));

    // Ajout du corps de la requête
    RESTRequest.Body.Add(RequestJSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    try
      // Exécution de la requête
      RESTRequest.Execute;

      // Traitement de la réponse si le statut est OK (200)
      if RESTResponse.StatusCode = 200 then
      begin
        // Analyse de la réponse JSON
        ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
        try
          // Extraction du texte généré par le modèle
          Result := ResponseJSON.GetValue<string>('choices[0].message.content');

          // Ajout de la réponse à l'historique
          ChatHistory.AddMessage('assistant', Result);
        finally
          ResponseJSON.Free;
        end;
      end
      else
      begin
        // En cas d'erreur, renvoyer le message d'erreur
        Result := 'Erreur ' + RESTResponse.StatusCode.ToString + ': ' +
                  RESTResponse.StatusText + #13#10 + RESTResponse.Content;
      end;
    except
      on E: Exception do
        Result := 'Exception: ' + E.Message;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestJSON.Free;
    // Note: MessagesArray est géré par RequestJSON
  end;
end;
```

## Optimisation du contenu généré

Les LLM sont des outils puissants mais peuvent parfois être verbeux ou imprécis. Pour optimiser les résultats :

### 1. Instructions système

```pascal
// Initialiser l'historique avec une instruction système
ChatHistory := TChatHistory.Create;
ChatHistory.AddMessage('system',
  'Tu es un assistant spécialisé en programmation Delphi. ' +
  'Donne des réponses concises, techniques et avec des exemples de code ' +
  'quand c''est pertinent. Ne fais pas d''introduction inutile.');
```

### 2. Paramétrage de la température

```pascal
// Pour des réponses créatives (brainstorming, idées)
Response := CallGPTAPI(APIKey, Prompt, 500, 0.8); // Température plus élevée

// Pour des réponses factuelles (code, données techniques)
Response := CallGPTAPI(APIKey, Prompt, 500, 0.2); // Température plus basse
```

### 3. Prompt engineering

Le "prompt engineering" est l'art de formuler des instructions claires pour obtenir les meilleurs résultats :

```pascal
// Mauvais prompt
const BAD_PROMPT = 'Comment analyser un CSV ?';

// Bon prompt
const GOOD_PROMPT = 'Écris une fonction Delphi qui lit un fichier CSV, ' +
                    'valide que les colonnes attendues sont présentes, ' +
                    'et charge les données dans un TClientDataSet. ' +
                    'Inclus la gestion des erreurs et des formats de dates.';
```

## Projet complet : Assistant de programmation Delphi

Voici un exemple complet d'assistant de programmation pour Delphi qui utilise l'API OpenAI :

```pascal
unit UnitAssistant;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Threading,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.Objects, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo.Types, FMX.TabControl,
  REST.Types, REST.Client, REST.Response.Adapter;

type
  TChatMessage = record
    Role: string;
    Content: string;
  end;

  TChatHistory = class
  private
    FMessages: TArray<TChatMessage>;
  public
    constructor Create;
    procedure AddMessage(const Role, Content: string);
    function GetMessagesAsJSON: TJSONArray;
    procedure Clear;
    property Messages: TArray<TChatMessage> read FMessages;
  end;

  TFormAssistant = class(TForm)
    TabControl1: TTabControl;
    TabItemChat: TTabItem;
    TabItemCode: TTabItem;
    LayoutChat: TLayout;
    LayoutChatTop: TLayout;
    LayoutChatBottom: TLayout;
    MemoChat: TMemo;
    EditMessage: TEdit;
    ButtonSend: TButton;
    LayoutCodeTop: TLayout;
    LayoutCodeBottom: TLayout;
    LayoutCodeCenter: TLayout;
    EditCodePrompt: TEdit;
    ButtonGenerateCode: TButton;
    MemoCodeResult: TMemo;
    LabelCodeTitle: TLabel;
    LabelChatTitle: TLabel;
    ComboBoxModel: TComboBox;
    LabelModel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMessageKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure ButtonGenerateCodeClick(Sender: TObject);
    procedure EditCodePromptKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    FChatHistory: TChatHistory;
    function GetAPIKey: string;
    procedure AppendChatMessage(const Role, Content: string);
    function CallOpenAI(const Prompt: string; IsCodeGeneration: Boolean = False): string;
  public
    { Public declarations }
  end;

var
  FormAssistant: TFormAssistant;

implementation

{$R *.fmx}

{ TChatHistory }

constructor TChatHistory.Create;
begin
  inherited;
  // Instruction système initiale
  AddMessage('system',
    'Tu es DelphiGPT, un assistant spécialisé en programmation Delphi. ' +
    'Tu réponds de manière concise et précise aux questions techniques. ' +
    'Pour les exemples de code, tu utilises Object Pascal avec la syntaxe et ' +
    'les conventions de nommage de Delphi. Tu es particulièrement bien informé ' +
    'sur les API, composants et frameworks Delphi comme VCL, FMX, FireDAC, etc.');
end;

procedure TChatHistory.AddMessage(const Role, Content: string);
var
  Msg: TChatMessage;
begin
  Msg.Role := Role;
  Msg.Content := Content;
  SetLength(FMessages, Length(FMessages) + 1);
  FMessages[High(FMessages)] := Msg;
end;

function TChatHistory.GetMessagesAsJSON: TJSONArray;
var
  Msg: TChatMessage;
  MsgObj: TJSONObject;
begin
  Result := TJSONArray.Create;
  for Msg in FMessages do
  begin
    MsgObj := TJSONObject.Create;
    MsgObj.AddPair('role', Msg.Role);
    MsgObj.AddPair('content', Msg.Content);
    Result.Add(MsgObj);
  end;
end;

procedure TChatHistory.Clear;
begin
  SetLength(FMessages, 0);
end;

{ TFormAssistant }

procedure TFormAssistant.FormCreate(Sender: TObject);
begin
  // Initialisation de l'interface
  Caption := 'DelphiGPT - Assistant de programmation Delphi';

  // Initialisation de l'historique de chat
  FChatHistory := TChatHistory.Create;

  // Remplissage du combobox des modèles
  ComboBoxModel.Items.Clear;
  ComboBoxModel.Items.Add('gpt-3.5-turbo (rapide)');
  ComboBoxModel.Items.Add('gpt-4 (avancé)');
  ComboBoxModel.ItemIndex := 0;

  // Message d'accueil
  MemoChat.Lines.Add('DelphiGPT: Bonjour ! Je suis votre assistant de programmation Delphi. Comment puis-je vous aider aujourd''hui ?');
  MemoChat.Lines.Add('');

  // Vérification de la clé API
  if GetAPIKey = '' then
  begin
    ShowMessage('Aucune clé API OpenAI trouvée. Veuillez la configurer avant d''utiliser l''assistant.');
  end;
end;

function TFormAssistant.GetAPIKey: string;
var
  IniFile: TIniFile;
  ConfigFile: string;
begin
  Result := '';

  // Chemin du fichier de configuration
  ConfigFile := IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0))) + 'config.ini';

  // Vérifier si le fichier existe
  if FileExists(ConfigFile) then
  begin
    IniFile := TIniFile.Create(ConfigFile);
    try
      Result := IniFile.ReadString('OpenAI', 'APIKey', '');
    finally
      IniFile.Free;
    end;
  end;

  // Si la clé n'est pas trouvée, demander à l'utilisateur
  if Result = '' then
  begin
    Result := InputBox('Configuration',
      'Veuillez entrer votre clé API OpenAI:', '');

    // Sauvegarder la clé pour la prochaine utilisation
    if Result <> '' then
    begin
      IniFile := TIniFile.Create(ConfigFile);
      try
        IniFile.WriteString('OpenAI', 'APIKey', Result);
      finally
        IniFile.Free;
      end;
    end;
  end;
end;

procedure TFormAssistant.AppendChatMessage(const Role, Content: string);
var
  Prefix: string;
begin
  // Déterminer le préfixe selon le rôle
  if Role = 'user' then
    Prefix := 'Vous: '
  else if Role = 'assistant' then
    Prefix := 'DelphiGPT: '
  else
    Prefix := '';

  // Ajouter le message au mémo
  MemoChat.Lines.Add(Prefix + Content);
  MemoChat.Lines.Add('');

  // Faire défiler jusqu'au bas
  MemoChat.GoToTextEnd;
end;

function TFormAssistant.CallOpenAI(const Prompt: string; IsCodeGeneration: Boolean): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestJSON: TJSONObject;
  MessagesArray: TJSONArray;
  MessageObject: TJSONObject;
  ResponseJSON: TJSONValue;
  ModelName: string;
  MaxTokens: Integer;
  Temperature: Double;
begin
  Result := '';

  // Déterminer le modèle à utiliser
  if ComboBoxModel.ItemIndex = 1 then
    ModelName := 'gpt-4'
  else
    ModelName := 'gpt-3.5-turbo';

  // Paramètres selon le type de génération
  if IsCodeGeneration then
  begin
    MaxTokens := 1500;  // Plus de tokens pour le code
    Temperature := 0.2;  // Réponses plus déterministes pour le code

    // Pour la génération de code, on crée un historique temporaire avec une instruction spécifique
    MessagesArray := TJSONArray.Create;

    // Instruction système pour la génération de code
    MessageObject := TJSONObject.Create;
    MessageObject.AddPair('role', 'system');
    MessageObject.AddPair('content',
      'Tu es un expert en programmation Delphi. Génère uniquement du code ' +
      'Object Pascal selon les conventions Delphi, bien commenté et ' +
      'correctement indenté. Explique brièvement le fonctionnement du code ' +
      'en début de réponse puis fournis le code complet.');
    MessagesArray.Add(MessageObject);

    // Message utilisateur
    MessageObject := TJSONObject.Create;
    MessageObject.AddPair('role', 'user');
    MessageObject.AddPair('content', Prompt);
    MessagesArray.Add(MessageObject);
  end
  else
  begin
    MaxTokens := 1000;  // Tokens standard pour la conversation
    Temperature := 0.7;  // Température standard pour la conversation

    // Ajouter le message utilisateur à l'historique
    FChatHistory.AddMessage('user', Prompt);

    // Utiliser l'historique complet pour la conversation
    MessagesArray := FChatHistory.GetMessagesAsJSON;
  end;

  // Création des objets REST
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestJSON := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://api.openai.com/v1/chat/completions';
    RESTClient.Accept := 'application/json';
    RESTClient.ContentType := 'application/json';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de l'en-tête d'autorisation avec la clé API
    RESTRequest.AddParameter('Authorization', 'Bearer ' + GetAPIKey,
      TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    // Configuration des paramètres de la requête
    RequestJSON.AddPair('model', ModelName);
    RequestJSON.AddPair('messages', MessagesArray);
    RequestJSON.AddPair('max_tokens', TJSONNumber.Create(MaxTokens));
    RequestJSON.AddPair('temperature', TJSONNumber.Create(Temperature));

    // Ajout du corps de la requête
    RESTRequest.Body.Add(RequestJSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    try
      // Exécution de la requête
      RESTRequest.Execute;

      // Traitement de la réponse si le statut est OK (200)
      if RESTResponse.StatusCode = 200 then
      begin
        // Analyse de la réponse JSON
        ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
        try
          // Extraction du texte généré par le modèle
          Result := ResponseJSON.GetValue<string>('choices[0].message.content');

          // Ajout de la réponse à l'historique (seulement pour le chat, pas pour la génération de code)
          if not IsCodeGeneration then
            FChatHistory.AddMessage('assistant', Result);
        finally
          ResponseJSON.Free;
        end;
      end
      else
      begin
        // En cas d'erreur, renvoyer le message d'erreur
        Result := 'Erreur ' + RESTResponse.StatusCode.ToString + ': ' +
                  RESTResponse.StatusText + #13#10 + RESTResponse.Content;
      end;
    except
      on E: Exception do
        Result := 'Exception: ' + E.Message;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestJSON.Free;
    // Note: MessagesArray est libéré avec RequestJSON si c'est un historique temporaire
    if IsCodeGeneration then
      MessagesArray.Free;
  end;
end;

procedure TFormAssistant.ButtonSendClick(Sender: TObject);
var
  UserMessage: string;
begin
  UserMessage := EditMessage.Text.Trim;

  // Vérifier si le message n'est pas vide
  if UserMessage = '' then
    Exit;

  // Désactiver les contrôles pendant le traitement
  ButtonSend.Enabled := False;
  EditMessage.Enabled := False;
  EditMessage.Text := '';

  // Afficher le message utilisateur
  AppendChatMessage('user', UserMessage);

  // Indiquer que l'assistant réfléchit
  MemoChat.Lines.Add('DelphiGPT: En cours de réflexion...');

  // Appel à l'API dans un thread séparé
  TTask.Run(
    procedure
    var
      Response: string;
    begin
      // Appel à l'API OpenAI
      Response := CallOpenAI(UserMessage);

      // Mise à jour de l'interface dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          // Supprimer le message "En cours de réflexion..."
          MemoChat.Lines.Delete(MemoChat.Lines.Count - 1);

          // Afficher la réponse
          AppendChatMessage('assistant', Response);

          // Réactiver les contrôles
          ButtonSend.Enabled := True;
          EditMessage.Enabled := True;
          EditMessage.SetFocus;
        end);
    end);
end;

procedure TFormAssistant.EditMessageKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  // Envoyer le message avec Entrée
  if (Key = vkReturn) and ButtonSend.Enabled then
  begin
    Key := 0;
    ButtonSendClick(Sender);
  end;
end;

procedure TFormAssistant.ButtonGenerateCodeClick(Sender: TObject);
var
  CodePrompt: string;
begin
  CodePrompt := EditCodePrompt.Text.Trim;

  // Vérifier si le prompt n'est pas vide
  if CodePrompt = '' then
  begin
    ShowMessage('Veuillez entrer une description du code à générer.');
    Exit;
  end;

  // Désactiver les contrôles pendant le traitement
  ButtonGenerateCode.Enabled := False;
  EditCodePrompt.Enabled := False;

  // Effacer le résultat précédent et indiquer que le traitement est en cours
  MemoCodeResult.Lines.Clear;
  MemoCodeResult.Lines.Add('Génération du code en cours...');

  // Appel à l'API dans un thread séparé
  TTask.Run(
    procedure
    var
      CodeResult: string;
    begin
      // Préparation du prompt pour la génération de code
      CodeResult := CallOpenAI('Génère du code Delphi (Object Pascal) qui: ' + CodePrompt +
        '. Inclus des commentaires explicatifs et suit les conventions de nommage Delphi.', True);

      // Mise à jour de l'interface dans le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          // Afficher le code généré
          MemoCodeResult.Lines.Clear;
          MemoCodeResult.Lines.Add(CodeResult);

          // Réactiver les contrôles
          ButtonGenerateCode.Enabled := True;
          EditCodePrompt.Enabled := True;
          EditCodePrompt.SetFocus;
        end);
    end);
end;

procedure TFormAssistant.EditCodePromptKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  // Générer le code avec Entrée
  if (Key = vkReturn) and ButtonGenerateCode.Enabled then
  begin
    Key := 0;
    ButtonGenerateCodeClick(Sender);
  end;
end;
```

## Création de l'interface utilisateur

Pour implémenter cette application, vous devrez créer un formulaire FireMonkey avec les composants suivants :

1. `TabControl1` avec deux onglets : "Chat" et "Code Generator"
2. Dans l'onglet "Chat" :
   - `MemoChat` pour afficher la conversation
   - `EditMessage` pour saisir les messages
   - `ButtonSend` pour envoyer les messages
3. Dans l'onglet "Code Generator" :
   - `EditCodePrompt` pour saisir la description du code à générer
   - `ButtonGenerateCode` pour lancer la génération
   - `MemoCodeResult` pour afficher le code généré
4. Un `ComboBoxModel` pour sélectionner le modèle à utiliser

## Bonnes pratiques pour l'utilisation des LLM dans vos applications

### Gestion des erreurs robuste

Mettez en place une gestion d'erreurs complète pour gérer les problèmes courants des API :

```pascal
procedure TryApiCall(const Callback: TProc; const ErrorCallback: TProc<string>);
begin
  try
    Callback();
  except
    on E: EHTTPProtocolException do
    begin
      case E.ErrorCode of
        401: ErrorCallback('Erreur d''authentification. Vérifiez votre clé API.');
        429: ErrorCallback('Trop de requêtes. Attendez un moment avant de réessayer.');
        500..599: ErrorCallback('Erreur du serveur. Réessayez plus tard.');
        else ErrorCallback('Erreur HTTP: ' + E.Message);
      end;
    end;
    on E: Exception do
      ErrorCallback('Erreur: ' + E.Message);
  end;
end;
```

### Configuration du modèle selon l'usage

Adaptez les paramètres du modèle selon le type de tâche :

```pascal
// Fonction pour déterminer les paramètres optimaux selon l'usage
procedure GetOptimalParameters(const TaskType: string;
  out ModelName: string; out MaxTokens: Integer; out Temperature: Double);
begin
  if TaskType = 'code' then
  begin
    ModelName := 'gpt-4';  // Plus précis pour le code
    MaxTokens := 1500;     // Code plus long
    Temperature := 0.2;    // Moins de créativité, plus de précision
  end
  else if TaskType = 'creative' then
  begin
    ModelName := 'gpt-3.5-turbo';  // Rapide et bon pour le créatif
    MaxTokens := 1000;
    Temperature := 0.8;    // Plus de créativité
  end
  else  // conversation
  begin
    ModelName := 'gpt-3.5-turbo';  // Bon équilibre coût/performance
    MaxTokens := 800;
    Temperature := 0.7;    // Équilibre créativité/précision
  end;
end;
```

### Suivi des coûts et limitation d'utilisation

Pour les applications professionnelles, il est crucial de suivre les coûts :

```pascal
type
  TAPIUsage = class
  private
    FTotalTokens: Integer;
    FTotalCost: Double;
    FUsageLimit: Double;
    FOnLimitReached: TNotifyEvent;

    procedure UpdateUsage(const InputTokens, OutputTokens: Integer; const Model: string);
    function IsLimitReached: Boolean;
  public
    constructor Create(const UsageLimit: Double = 5.0);  // Limite par défaut: 5 $

    procedure TrackUsage(const InputText, OutputText, Model: string);
    procedure ResetUsage;

    property TotalTokens: Integer read FTotalTokens;
    property TotalCost: Double read FTotalCost;
    property UsageLimit: Double read FUsageLimit write FUsageLimit;
    property OnLimitReached: TNotifyEvent read FOnLimitReached write FOnLimitReached;
  end;

// Approximation du nombre de tokens
function EstimateTokens(const Text: string): Integer;
begin
  // Approximation grossière: ~4 caractères par token pour l'anglais,
  // ~3 caractères par token pour les langues latines
  Result := Length(Text) div 4;
end;

// Estimation du coût selon le modèle
function EstimateCost(const InputTokens, OutputTokens: Integer; const Model: string): Double;
begin
  if Model = 'gpt-3.5-turbo' then
    Result := (InputTokens * 0.0015 + OutputTokens * 0.002) / 1000
  else if Model = 'gpt-4' then
    Result := (InputTokens * 0.03 + OutputTokens * 0.06) / 1000
  else
    Result := 0;
end;

{ TAPIUsage }

constructor TAPIUsage.Create(const UsageLimit: Double);
begin
  inherited Create;
  FTotalTokens := 0;
  FTotalCost := 0;
  FUsageLimit := UsageLimit;
end;

procedure TAPIUsage.UpdateUsage(const InputTokens, OutputTokens: Integer; const Model: string);
var
  Cost: Double;
begin
  // Calcul du coût de cette requête
  Cost := EstimateCost(InputTokens, OutputTokens, Model);

  // Mise à jour des compteurs
  FTotalTokens := FTotalTokens + InputTokens + OutputTokens;
  FTotalCost := FTotalCost + Cost;

  // Vérification du dépassement de limite
  if IsLimitReached and Assigned(FOnLimitReached) then
    FOnLimitReached(Self);
end;

function TAPIUsage.IsLimitReached: Boolean;
begin
  Result := FTotalCost >= FUsageLimit;
end;

procedure TAPIUsage.TrackUsage(const InputText, OutputText, Model: string);
var
  InputTokens, OutputTokens: Integer;
begin
  // Estimation des tokens
  InputTokens := EstimateTokens(InputText);
  OutputTokens := EstimateTokens(OutputText);

  // Mise à jour de l'usage
  UpdateUsage(InputTokens, OutputTokens, Model);
end;

procedure TAPIUsage.ResetUsage;
begin
  FTotalTokens := 0;
  FTotalCost := 0;
end;
```

## Techniques avancées et optimisations

### Text Embeddings pour la recherche sémantique

Les embeddings sont des représentations numériques de texte qui capturent le sens. Vous pouvez les utiliser pour créer des recherches sémantiques :

```pascal
// Obtenir les embeddings d'un texte via l'API OpenAI
function GetEmbeddings(const Text: string; const APIKey: string): TArray<Double>;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestJSON, ResponseJSON: TJSONObject;
  EmbeddingsArray: TJSONArray;
  I: Integer;
begin
  RESTClient := TRESTClient.Create('https://api.openai.com/v1/embeddings');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestJSON := TJSONObject.Create;

  try
    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de l'en-tête d'autorisation
    RESTRequest.AddParameter('Authorization', 'Bearer ' + APIKey,
      TRESTRequestParameterKind.pkHTTPHEADER);

    // Préparation du corps de la requête
    RequestJSON.AddPair('input', Text);
    RequestJSON.AddPair('model', 'text-embedding-ada-002');

    RESTRequest.AddBody(RequestJSON.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    // Exécution de la requête
    RESTRequest.Execute;

    // Analyse de la réponse
    ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
    try
      // Extraction des embeddings
      EmbeddingsArray := ResponseJSON.GetValue<TJSONArray>('data[0].embedding');

      // Conversion en tableau de doubles
      SetLength(Result, EmbeddingsArray.Count);
      for I := 0 to EmbeddingsArray.Count - 1 do
        Result[I] := (EmbeddingsArray.Items[I] as TJSONNumber).AsDouble;
    finally
      ResponseJSON.Free;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestJSON.Free;
  end;
end;

// Calcul de similarité cosinus entre deux vecteurs d'embeddings
function CosineSimilarity(const Vec1, Vec2: TArray<Double>): Double;
var
  DotProduct, Norm1, Norm2: Double;
  I: Integer;
begin
  DotProduct := 0;
  Norm1 := 0;
  Norm2 := 0;

  for I := 0 to High(Vec1) do
  begin
    DotProduct := DotProduct + Vec1[I] * Vec2[I];
    Norm1 := Norm1 + Vec1[I] * Vec1[I];
    Norm2 := Norm2 + Vec2[I] * Vec2[I];
  end;

  Norm1 := Sqrt(Norm1);
  Norm2 := Sqrt(Norm2);

  if (Norm1 > 0) and (Norm2 > 0) then
    Result := DotProduct / (Norm1 * Norm2)
  else
    Result := 0;
end;
```

### Mise en cache des réponses pour des requêtes similaires

Pour économiser des appels d'API et améliorer la réactivité :

```pascal
type
  TCacheEntry = record
    Prompt: string;
    Response: string;
    Timestamp: TDateTime;
  end;

  TResponseCache = class
  private
    FCache: TList<TCacheEntry>;
    FMaxEntries: Integer;
    FExpirationHours: Double;
  public
    constructor Create(MaxEntries: Integer = 100; ExpirationHours: Double = 24);
    destructor Destroy; override;

    procedure AddToCache(const Prompt, Response: string);
    function TryGetFromCache(const Prompt: string; out Response: string): Boolean;
    procedure ClearExpiredEntries;
  end;

constructor TResponseCache.Create(MaxEntries: Integer; ExpirationHours: Double);
begin
  inherited Create;
  FCache := TList<TCacheEntry>.Create;
  FMaxEntries := MaxEntries;
  FExpirationHours := ExpirationHours;
end;

destructor TResponseCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TResponseCache.AddToCache(const Prompt, Response: string);
var
  Entry: TCacheEntry;
begin
  // Créer la nouvelle entrée
  Entry.Prompt := Prompt;
  Entry.Response := Response;
  Entry.Timestamp := Now;

  // Ajouter au cache
  FCache.Add(Entry);

  // Si le cache est plein, supprimer la plus ancienne entrée
  if FCache.Count > FMaxEntries then
    FCache.Delete(0);
end;

function TResponseCache.TryGetFromCache(const Prompt: string; out Response: string): Boolean;
var
  Entry: TCacheEntry;
  I: Integer;
begin
  Result := False;
  Response := '';

  // Nettoyer les entrées expirées
  ClearExpiredEntries;

  // Rechercher une entrée correspondante
  for I := 0 to FCache.Count - 1 do
  begin
    Entry := FCache[I];

    // Correspondance exacte
    if Entry.Prompt = Prompt then
    begin
      Response := Entry.Response;
      Result := True;
      Exit;
    end;
  end;
end;

procedure TResponseCache.ClearExpiredEntries;
var
  I: Integer;
  ExpirationTime: TDateTime;
begin
  ExpirationTime := Now - (FExpirationHours / 24);  // Convertir heures en jours

  // Parcourir dans l'ordre inverse pour éviter les problèmes d'index
  for I := FCache.Count - 1 downto 0 do
  begin
    if FCache[I].Timestamp < ExpirationTime then
      FCache.Delete(I);
  end;
end;
```

## Ressources supplémentaires

### Documentation des API LLM
- [Documentation API OpenAI](https://platform.openai.com/docs/api-reference)
- [Documentation API Claude d'Anthropic](https://docs.anthropic.com/claude/reference/)
- [Documentation API Gemini de Google](https://ai.google.dev/docs)

### Bibliothèques et composants
- [REST Debugger](https://docwiki.embarcadero.com/RADStudio/Alexandria/en/REST_Debugger) - Outil inclus dans Delphi pour tester les API REST
- [DelphiOpenAI](https://github.com/HemulGM/DelphiOpenAI) - Bibliothèque open-source pour faciliter l'accès à l'API OpenAI depuis Delphi

### Tutoriels et exemples
- [RAD Studio Blogs](https://blogs.embarcadero.com/) - Recherchez des articles sur l'intégration d'API AI
- [Delphi Discord](https://discord.gg/KRVHkjf) - Communauté active pour poser des questions

## Conclusion

L'intégration des grands modèles de langage via API dans vos applications Delphi ouvre un monde de possibilités. Que ce soit pour créer des assistants intelligents, automatiser la génération de contenu ou analyser des textes complexes, les LLM peuvent considérablement améliorer vos applications.

En suivant les bonnes pratiques présentées dans ce chapitre, vous pourrez créer des intégrations robustes, sécurisées et économiques. N'oubliez pas que le domaine de l'IA évolue rapidement - restez à l'affût des nouveaux modèles et capacités pour tirer le meilleur parti de ces technologies dans vos applications Delphi.

## Exercices pratiques

1. **Exercice débutant** : Créez une simple application "Question-Réponse" qui utilise l'API OpenAI pour répondre aux questions des utilisateurs.

2. **Exercice intermédiaire** : Implémentez un assistant de programmation Delphi qui peut générer du code, expliquer des concepts et aider au débogage.

3. **Exercice avancé** : Développez une application qui utilise les embeddings pour indexer et rechercher sémantiquement dans une base de documentation Delphi locale.

4. **Projet pratique** : Créez un outil de génération de commentaires de code qui analyse un fichier source Delphi et génère des commentaires explicatifs pour les méthodes et classes.

⏭️ [Conception d'applications Web avec Delphi](/23-conception-dapplications-web-avec-delphi/README.md)
