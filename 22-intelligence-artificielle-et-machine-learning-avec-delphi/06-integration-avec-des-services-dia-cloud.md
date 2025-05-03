# 22.6 Intégration avec des services d'IA cloud (Azure AI, Google AI, etc.)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'intelligence artificielle est aujourd'hui facilement accessible via des services cloud qui proposent des API REST. Dans cette section, nous allons découvrir comment intégrer ces services dans vos applications Delphi, en nous concentrant sur les plateformes les plus populaires : Azure AI, Google AI et OpenAI.

## Prérequis

Avant de commencer, assurez-vous de disposer des éléments suivants :
- Delphi 11 Alexandria ou supérieur
- Une connexion Internet active
- Un compte sur la plateforme cloud que vous souhaitez utiliser (Azure, Google Cloud ou OpenAI)
- Des connaissances de base sur les appels REST avec Delphi

## Principes communs d'intégration

Quelle que soit la plateforme cloud choisie, l'intégration suit généralement ces étapes :

1. Création d'un compte sur la plateforme et obtention d'une clé API
2. Configuration des composants REST dans votre application Delphi
3. Envoi des requêtes au service d'IA
4. Traitement des réponses obtenues

## Intégration avec Azure AI Services

Microsoft Azure propose une gamme complète de services d'IA accessibles via des API REST.

### Étape 1 : Configuration du compte Azure

1. Créez un compte sur le [portail Azure](https://portal.azure.com/)
2. Créez une ressource Azure AI Services
3. Récupérez votre clé API et l'URL de point de terminaison

### Étape 2 : Intégration avec Delphi

Voici un exemple d'intégration avec Azure Computer Vision pour analyser une image :

```pascal
procedure TFormAzureAI.AnalyzeImage(const ImageFilePath: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  ImageStream: TMemoryStream;
  JSONValue: TJSONValue;
  JSONArray: TJSONArray;
  I: Integer;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  ImageStream := TMemoryStream.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://votre-ressource.cognitiveservices.azure.com/vision/v3.2/analyze';
    RESTClient.ContentType := 'application/octet-stream';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Paramètres de la requête
    RESTRequest.AddParameter('visualFeatures', 'Categories,Description,Objects');

    // Ajout de la clé API dans les en-têtes
    RESTRequest.AddParameter('Ocp-Apim-Subscription-Key', 'votre-clé-api', TRESTRequestParameterKind.pkHTTPHEADER);

    // Chargement de l'image
    ImageStream.LoadFromFile(ImageFilePath);
    RESTRequest.AddBody(ImageStream, TRESTContentType.ctAPPLICATION_OCTET_STREAM);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      // Parsing JSON
      JSONValue := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction de la description
        Memo1.Lines.Add('Description : ' +
          JSONValue.GetValue<string>('description.captions[0].text'));

        // Extraction des catégories
        JSONArray := JSONValue.GetValue<TJSONArray>('categories');
        Memo1.Lines.Add('Catégories :');
        for I := 0 to JSONArray.Count - 1 do
        begin
          Memo1.Lines.Add('- ' + JSONArray.Items[I].GetValue<string>('name') +
            ' (' + FormatFloat('0.00', JSONArray.Items[I].GetValue<double>('score') * 100) + '%)');
        end;
      finally
        JSONValue.Free;
      end;
    end
    else
    begin
      ShowMessage('Erreur : ' + RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    ImageStream.Free;
  end;
end;
```

## Intégration avec Google AI

Google Cloud Platform propose également de nombreux services d'IA accessibles via API.

### Étape 1 : Configuration du compte Google Cloud

1. Créez un compte sur la [console Google Cloud](https://console.cloud.google.com/)
2. Activez les API souhaitées (Vision AI, Speech-to-Text, etc.)
3. Créez une clé API ou configurez l'authentification OAuth2

### Étape 2 : Intégration avec Delphi

Voici un exemple d'utilisation de l'API Google Cloud Translation :

```pascal
procedure TFormGoogleAI.TranslateText(const TextToTranslate, SourceLang, TargetLang: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  ResponseJSON: TJSONValue;
  TranslatedText: string;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://translation.googleapis.com/language/translate/v2';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de la clé API comme paramètre de requête
    RESTRequest.AddParameter('key', 'votre-clé-api');

    // Préparation du corps de la requête en JSON
    RequestBody.AddPair('q', TextToTranslate);
    RequestBody.AddPair('source', SourceLang);
    RequestBody.AddPair('target', TargetLang);
    RequestBody.AddPair('format', 'text');

    RESTRequest.AddBody(RequestBody.ToJSON);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction du texte traduit
        TranslatedText := ResponseJSON.GetValue<string>('data.translations[0].translatedText');
        Memo1.Lines.Add('Texte original: ' + TextToTranslate);
        Memo1.Lines.Add('Traduction: ' + TranslatedText);
      finally
        ResponseJSON.Free;
      end;
    end
    else
    begin
      ShowMessage('Erreur: ' + RESTResponse.StatusText);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;
```

## Intégration avec OpenAI (ChatGPT, DALL-E)

OpenAI propose des modèles d'IA très puissants pour la génération de texte et d'images.

### Étape 1 : Configuration du compte OpenAI

1. Créez un compte sur [OpenAI](https://platform.openai.com/)
2. Générez une clé API dans la section API keys

### Étape 2 : Intégration avec Delphi

Voici un exemple d'utilisation de l'API ChatGPT :

```pascal
procedure TFormOpenAI.GenerateText(const Prompt: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  MessagesArray: TJSONArray;
  MessageObject: TJSONObject;
  ResponseJSON: TJSONValue;
  GeneratedText: string;
begin
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;
  MessagesArray := TJSONArray.Create;
  MessageObject := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://api.openai.com/v1/chat/completions';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de la clé API dans les en-têtes
    RESTRequest.AddParameter('Authorization', 'Bearer votre-clé-api', TRESTRequestParameterKind.pkHTTPHEADER);

    // Préparation du message
    MessageObject.AddPair('role', 'user');
    MessageObject.AddPair('content', Prompt);
    MessagesArray.Add(MessageObject);

    // Préparation du corps de la requête en JSON
    RequestBody.AddPair('model', 'gpt-4');
    RequestBody.AddPair('messages', MessagesArray);
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(500));
    RequestBody.AddPair('temperature', TJSONNumber.Create(0.7));

    RESTRequest.AddBody(RequestBody.ToJSON);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction du texte généré
        GeneratedText := ResponseJSON.GetValue<string>('choices[0].message.content');
        Memo1.Lines.Add('Prompt: ' + Prompt);
        Memo1.Lines.Add('Réponse:');
        Memo1.Lines.Add(GeneratedText);
      finally
        ResponseJSON.Free;
      end;
    end
    else
    begin
      ShowMessage('Erreur: ' + RESTResponse.StatusText + #13#10 + RESTResponse.Content);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    // Attention : MessagesArray va libérer MessageObject
    RequestBody.Free;
    MessagesArray.Free;
  end;
end;
```

## Exemple complet : Application de traduction intelligente

Voici un exemple d'application complète qui utilise l'API Google Translate pour traduire du texte :

![Application de traduction](images/traduction_app.png)

```pascal
unit UnitTranslator;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts, FMX.Memo, FMX.Edit,
  FMX.ComboEdit, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.Types, REST.Client, REST.Response.Adapter, System.JSON;

type
  TFormTranslator = class(TForm)
    LayoutTop: TLayout;
    LayoutBottom: TLayout;
    MemoSource: TMemo;
    MemoTarget: TMemo;
    ComboBoxSourceLang: TComboEdit;
    ComboBoxTargetLang: TComboEdit;
    ButtonTranslate: TButton;
    LabelSource: TLabel;
    LabelTarget: TLabel;
    LabelSourceLang: TLabel;
    LabelTargetLang: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonTranslateClick(Sender: TObject);
  private
    procedure PopulateLanguages;
    function GetLanguageCode(const LanguageName: string): string;
    function TranslateText(const Text, SourceLang, TargetLang: string): string;
  public
    { Déclarations publiques }
  end;

var
  FormTranslator: TFormTranslator;

implementation

{$R *.fmx}

procedure TFormTranslator.FormCreate(Sender: TObject);
begin
  PopulateLanguages;
end;

procedure TFormTranslator.PopulateLanguages;
begin
  // Ajout des langues supportées
  ComboBoxSourceLang.Items.AddStrings(['Français', 'Anglais', 'Espagnol', 'Allemand', 'Italien', 'Portugais', 'Russe', 'Chinois', 'Japonais', 'Arabe']);
  ComboBoxTargetLang.Items.AddStrings(['Français', 'Anglais', 'Espagnol', 'Allemand', 'Italien', 'Portugais', 'Russe', 'Chinois', 'Japonais', 'Arabe']);

  // Sélection par défaut
  ComboBoxSourceLang.ItemIndex := 0; // Français
  ComboBoxTargetLang.ItemIndex := 1; // Anglais
end;

function TFormTranslator.GetLanguageCode(const LanguageName: string): string;
begin
  // Conversion des noms de langues en codes ISO
  if LanguageName = 'Français' then Result := 'fr'
  else if LanguageName = 'Anglais' then Result := 'en'
  else if LanguageName = 'Espagnol' then Result := 'es'
  else if LanguageName = 'Allemand' then Result := 'de'
  else if LanguageName = 'Italien' then Result := 'it'
  else if LanguageName = 'Portugais' then Result := 'pt'
  else if LanguageName = 'Russe' then Result := 'ru'
  else if LanguageName = 'Chinois' then Result := 'zh'
  else if LanguageName = 'Japonais' then Result := 'ja'
  else if LanguageName = 'Arabe' then Result := 'ar'
  else Result := 'en'; // Par défaut
end;

procedure TFormTranslator.ButtonTranslateClick(Sender: TObject);
var
  SourceText: string;
  SourceLangCode, TargetLangCode: string;
  TranslatedText: string;
begin
  // Récupération des paramètres
  SourceText := MemoSource.Text;
  if SourceText.Trim.IsEmpty then
  begin
    ShowMessage('Veuillez entrer un texte à traduire.');
    Exit;
  end;

  SourceLangCode := GetLanguageCode(ComboBoxSourceLang.Text);
  TargetLangCode := GetLanguageCode(ComboBoxTargetLang.Text);

  // Indication visuelle que la traduction est en cours
  ButtonTranslate.Enabled := False;
  ButtonTranslate.Text := 'Traduction en cours...';
  Application.ProcessMessages;

  try
    // Appel à la fonction de traduction
    TranslatedText := TranslateText(SourceText, SourceLangCode, TargetLangCode);
    MemoTarget.Text := TranslatedText;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la traduction: ' + E.Message);
    end;
  end;

  // Rétablissement du bouton
  ButtonTranslate.Text := 'Traduire';
  ButtonTranslate.Enabled := True;
end;

function TFormTranslator.TranslateText(const Text, SourceLang, TargetLang: string): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  ResponseJSON: TJSONValue;
  TranslatedText: string;
begin
  Result := '';
  RESTClient := TRESTClient.Create(nil);
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTClient.BaseURL := 'https://translation.googleapis.com/language/translate/v2';

    // Configuration de la requête
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de la clé API comme paramètre de requête
    RESTRequest.AddParameter('key', 'VOTRE_CLE_API_GOOGLE');

    // Préparation du corps de la requête en JSON
    RequestBody.AddPair('q', Text);
    RequestBody.AddPair('source', SourceLang);
    RequestBody.AddPair('target', TargetLang);
    RequestBody.AddPair('format', 'text');

    RESTRequest.AddBody(RequestBody.ToJSON);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction du texte traduit
        TranslatedText := ResponseJSON.GetValue<string>('data.translations[0].translatedText');
        Result := TranslatedText;
      finally
        ResponseJSON.Free;
      end;
    end
    else
    begin
      raise Exception.Create(RESTResponse.StatusText + #13#10 + RESTResponse.Content);
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;

end.
```

## Bonnes pratiques pour l'intégration d'IA

1. **Gestion des clés API** : Ne codez jamais en dur vos clés API dans le code source. Utilisez plutôt un fichier de configuration sécurisé ou un gestionnaire de secrets.

2. **Gestion des erreurs** : Les services d'IA cloud peuvent parfois être indisponibles ou renvoyer des erreurs. Prévoyez toujours une gestion robuste des erreurs.

3. **Contrôle des coûts** : La plupart des services d'IA cloud sont facturés à l'usage. Implémentez des mécanismes pour suivre et contrôler l'utilisation.

4. **Compatibilité multi-versions** : Les API d'IA évoluent rapidement. Utilisez des techniques de versionnement et prévoyez la compatibilité avec les futures versions.

5. **Interface utilisateur réactive** : Les appels d'API peuvent prendre du temps. Utilisez des threads séparés ou des appels asynchrones pour maintenir votre interface réactive :

```pascal
// Exemple d'appel asynchrone avec TTask
uses
  System.Threading;

procedure TFormAI.ButtonProcessClick(Sender: TObject);
begin
  ButtonProcess.Enabled := False;
  ProgressBar1.Visible := True;

  TTask.Run(
    procedure
    begin
      // Code d'appel à l'API ici

      // Mise à jour de l'interface utilisateur
      TThread.Synchronize(TThread.CurrentThread,
        procedure
        begin
          // Code pour mettre à jour l'UI
          ButtonProcess.Enabled := True;
          ProgressBar1.Visible := False;
        end);
    end);
end;
```

## Exploration avancée : création d'un assistant IA

Une application plus avancée serait un assistant IA qui combine plusieurs services :

1. Reconnaissance vocale pour convertir la parole en texte
2. Analyse du texte pour comprendre l'intention de l'utilisateur
3. Génération de réponses adaptées
4. Synthèse vocale pour convertir le texte en parole

Cette approche multi-services montre comment combiner plusieurs API d'IA pour créer une expérience utilisateur riche et interactive.

## Conclusion

L'intégration des services d'IA cloud dans vos applications Delphi ouvre de nombreuses possibilités : traduction automatique, analyse d'images, reconnaissance vocale, génération de contenu, et bien plus encore. En suivant les exemples et les bonnes pratiques présentés dans ce chapitre, vous pouvez facilement ajouter des capacités d'IA à vos applications existantes ou en créer de nouvelles centrées sur l'intelligence artificielle.

N'oubliez pas que le domaine de l'IA évolue rapidement. Consultez régulièrement la documentation des fournisseurs de services d'IA pour rester à jour avec les dernières fonctionnalités et les meilleures pratiques.

## Exercices pratiques

1. Créez une application simple qui utilise l'API Vision d'Azure ou de Google pour analyser des images et détecter des objets.
2. Développez un assistant virtuel simple qui utilise l'API ChatGPT pour répondre à des questions.
3. Implémentez un système de traduction multilingue pour une application existante.
4. Utilisez une API de reconnaissance vocale pour créer une interface contrôlée par la voix.

## Ressources supplémentaires

- [Documentation Azure AI Services](https://docs.microsoft.com/fr-fr/azure/cognitive-services/)
- [Documentation Google Cloud AI](https://cloud.google.com/products/ai)
- [Documentation OpenAI](https://platform.openai.com/docs/introduction)
- [Composants REST pour Delphi](https://docwiki.embarcadero.com/RADStudio/Alexandria/fr/REST_Client_Library)

⏭️ [Utilisation des grands modèles de langage (LLM) via API](/22-intelligence-artificielle-et-machine-learning-avec-delphi/07-utilisation-des-grands-modeles-de-langage-via-api.md)
