# 22.3 Traitement du langage naturel (NLP)

## Qu'est-ce que le traitement du langage naturel ?

Le traitement du langage naturel (NLP - Natural Language Processing) est une branche de l'intelligence artificielle qui permet aux ordinateurs de comprendre, interpréter et générer le langage humain. Dans cette section, nous allons découvrir comment intégrer des capacités de NLP dans vos applications Delphi.

## Pourquoi intégrer le NLP dans vos applications ?

Le NLP peut considérablement améliorer vos applications en ajoutant des fonctionnalités comme :

- **Analyse de sentiments** : déterminer si un texte exprime une opinion positive ou négative
- **Classification de texte** : catégoriser automatiquement des documents ou messages
- **Extraction d'informations** : identifier des entités ou des données spécifiques dans un texte
- **Résumé automatique** : créer des résumés de textes longs
- **Traduction** : traduire du texte entre différentes langues
- **Chatbots** : créer des assistants conversationnels

## Approches pour intégrer le NLP dans Delphi

### 1. Utiliser des services cloud d'IA

La méthode la plus simple pour les débutants est d'utiliser des API cloud spécialisées en NLP :

#### Exemple avec l'API Google Cloud Natural Language

```delphi
procedure TFormNLP.AnalyzeSentiment(const Text: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody, ResponseJSON: TJSONObject;
begin
  RESTClient := TRESTClient.Create('https://language.googleapis.com/v1/documents:analyzeSentiment');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration de la requête REST
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout de la clé API dans l'URL
    RESTClient.BaseURL := RESTClient.BaseURL + '?key=' + GOOGLE_API_KEY;

    // Préparation du corps de la requête
    RequestBody.AddPair('document',
      TJSONObject.Create
        .AddPair('type', 'PLAIN_TEXT')
        .AddPair('content', Text)
    );
    RequestBody.AddPair('encodingType', 'UTF8');

    RESTRequest.Body.Add(RequestBody.ToJSON);
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
      try
        // Extraction du score de sentiment (entre -1 et 1)
        var DocumentSentiment := ResponseJSON.GetValue<TJSONObject>('documentSentiment');
        var Score := DocumentSentiment.GetValue<Double>('score');
        var Magnitude := DocumentSentiment.GetValue<Double>('magnitude');

        // Affichage des résultats
        MemoResult.Lines.Add('Analyse de sentiment :');
        MemoResult.Lines.Add(Format('Score : %.2f (négatif < 0 < positif)', [Score]));
        MemoResult.Lines.Add(Format('Magnitude : %.2f (intensité)', [Magnitude]));

        // Interprétation simple
        if Score > 0.25 then
          MemoResult.Lines.Add('Sentiment global : Positif')
        else if Score < -0.25 then
          MemoResult.Lines.Add('Sentiment global : Négatif')
        else
          MemoResult.Lines.Add('Sentiment global : Neutre');
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

#### Exemple avec l'API OpenAI (GPT)

```delphi
procedure TFormNLP.ClassifyText(const Text: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody: TJSONObject;
  ResponseJSON: TJSONValue;
begin
  RESTClient := TRESTClient.Create('https://api.openai.com/v1/chat/completions');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout des en-têtes
    RESTRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Authorization', 'Bearer ' + OPENAI_API_KEY, TRESTRequestParameterKind.pkHTTPHEADER);

    // Création du tableau de messages
    var MessagesArray := TJSONArray.Create;
    MessagesArray.Add(
      TJSONObject.Create
        .AddPair('role', 'system')
        .AddPair('content', 'Vous êtes un assistant qui classifie le texte dans une de ces catégories: Business, Technologie, Sport, Santé, Divertissement. Répondez seulement avec le nom de la catégorie.')
    );
    MessagesArray.Add(
      TJSONObject.Create
        .AddPair('role', 'user')
        .AddPair('content', Text)
    );

    // Préparation du corps de la requête
    RequestBody.AddPair('model', 'gpt-3.5-turbo');
    RequestBody.AddPair('messages', MessagesArray);
    RequestBody.AddPair('temperature', TJSONNumber.Create(0));
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(10));

    RESTRequest.Body.Add(RequestBody.ToJSON);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction de la réponse
        var Content :=
          ResponseJSON.GetValue<TJSONArray>('choices')
                     .Items[0]
                     .GetValue<TJSONObject>('message')
                     .GetValue<string>('content');

        // Affichage du résultat
        MemoResult.Lines.Add('Classification :');
        MemoResult.Lines.Add(Trim(Content));
      finally
        ResponseJSON.Free;
      end;
    end
    else
      ShowMessage('Erreur : ' + RESTResponse.Content);
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
    RequestBody.Free;
  end;
end;
```

### 2. Bibliothèques NLP intégrées

Pour les solutions autonomes qui ne nécessitent pas d'accès internet, vous pouvez utiliser des bibliothèques NLP directement dans votre application :

#### Utilisation de la bibliothèque SpaCy via Python

```delphi
procedure TFormNLP.ExtractEntities(const Text: string);
var
  Process: TProcess;
  OutputLines: TStringList;
begin
  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;

  try
    // Configuration du processus Python
    Process.Executable := 'python';
    Process.Parameters.Add('nlp_entities.py');
    Process.Parameters.Add(Text);
    Process.Options := Process.Options + [poWaitOnExit, poUsePipes];

    // Exécution du script Python
    Process.Execute;

    // Lecture de la sortie
    OutputLines.LoadFromStream(Process.Output);

    // Affichage des résultats
    MemoResult.Lines.Add('Entités détectées :');
    MemoResult.Lines.Add('');
    MemoResult.Lines.AddStrings(OutputLines);
  finally
    Process.Free;
    OutputLines.Free;
  end;
end;
```

Le script Python (nlp_entities.py) pourrait être :

```python
import sys
import spacy

def extract_entities(text):
    # Charger le modèle SpaCy (assurez-vous de l'avoir installé)
    nlp = spacy.load("fr_core_news_sm")

    # Traiter le texte
    doc = nlp(text)

    # Extraire les entités
    for entity in doc.ents:
        print(f"{entity.text} ({entity.label_})")

if __name__ == "__main__":
    if len(sys.argv) > 1:
        # Reconstruire le texte à partir des arguments de la ligne de commande
        text = " ".join(sys.argv[1:])
        extract_entities(text)
    else:
        print("Erreur: aucun texte fourni")
```

> ⚠️ **Note** : Cette approche nécessite que Python et la bibliothèque SpaCy soient installés sur l'ordinateur cible.

### 3. Solutions simplifiées intégrées à Delphi

Pour des fonctionnalités NLP simples, vous pouvez implémenter certains algorithmes directement en Delphi :

#### Exemple : Analyse de sentiment basique

```delphi
type
  TWordSentiment = record
    Word: string;
    Score: Double;
  end;

function SimpleSentimentAnalysis(const Text: string): Double;
const
  // Dictionnaire simplifié de mots avec leur score de sentiment
  SentimentWords: array[0..9] of TWordSentiment = (
    (Word: 'excellent'; Score: 1.0),
    (Word: 'bon'; Score: 0.7),
    (Word: 'super'; Score: 0.8),
    (Word: 'mauvais'; Score: -0.7),
    (Word: 'terrible'; Score: -1.0),
    (Word: 'horrible'; Score: -0.9),
    (Word: 'aimer'; Score: 0.6),
    (Word: 'détester'; Score: -0.8),
    (Word: 'content'; Score: 0.6),
    (Word: 'déçu'; Score: -0.6)
  );

  // Mots négatifs qui inversent le sentiment
  NegationWords: array[0..3] of string = ('ne', 'pas', 'jamais', 'aucun');
var
  Words: TArray<string>;
  TotalScore, WordScore: Double;
  WordCount, i, j: Integer;
  WordLower: string;
  IsNegated: Boolean;
begin
  // Séparation du texte en mots
  Words := Text.ToLower.Split([' ', '.', ',', '!', '?', ';', ':', '-'], TStringSplitOptions.ExcludeEmpty);

  TotalScore := 0;
  WordCount := 0;

  for i := 0 to Length(Words) - 1 do
  begin
    WordLower := Words[i];

    // Vérifier si ce mot est précédé d'une négation
    IsNegated := False;
    if (i > 0) then
    begin
      for j := 0 to High(NegationWords) do
      begin
        if (i > j) and (Words[i-j-1] = NegationWords[j]) then
        begin
          IsNegated := True;
          Break;
        end;
      end;
    end;

    // Rechercher le mot dans notre dictionnaire
    WordScore := 0;
    for j := 0 to High(SentimentWords) do
    begin
      if SentimentWords[j].Word = WordLower then
      begin
        WordScore := SentimentWords[j].Score;
        // Inverser le score si le mot est nié
        if IsNegated then
          WordScore := -WordScore;
        Break;
      end;
    end;

    // Si on a trouvé un score pour ce mot, l'ajouter au total
    if WordScore <> 0 then
    begin
      TotalScore := TotalScore + WordScore;
      Inc(WordCount);
    end;
  end;

  // Calcul du score moyen (éviter la division par zéro)
  if WordCount > 0 then
    Result := TotalScore / WordCount
  else
    Result := 0;
end;
```

> 🔹 **Note** : Cette implémentation est très simplifiée et ne remplace pas une vraie bibliothèque NLP, mais elle peut être suffisante pour des besoins basiques.

## Application pratique : Création d'un analyseur de texte

Assemblons ces différentes approches pour créer une application complète d'analyse de texte :

```delphi
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, REST.Client, REST.Types, System.JSON;

type
  TFormTextAnalyzer = class(TForm)
    MemoInput: TMemo;
    MemoOutput: TMemo;
    ButtonAnalyze: TButton;
    RadioGroupAnalysis: TRadioGroup;
    StatusBar: TStatusBar;
    ButtonClear: TButton;
    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AnalyzeSentiment;
    procedure ExtractEntities;
    procedure SummarizeText;
    procedure ClassifyText;
    procedure ShowError(const ErrorMsg: string);
    function SimpleSentimentAnalysis(const Text: string): Double;
  public
    { Public declarations }
  end;

var
  FormTextAnalyzer: TFormTextAnalyzer;

implementation

{$R *.dfm}

const
  API_KEY = 'votre_cle_api'; // Remplacez par votre clé API

procedure TFormTextAnalyzer.FormCreate(Sender: TObject);
begin
  RadioGroupAnalysis.ItemIndex := 0;
end;

procedure TFormTextAnalyzer.ButtonAnalyzeClick(Sender: TObject);
begin
  if MemoInput.Text.Trim = '' then
  begin
    ShowMessage('Veuillez entrer du texte à analyser.');
    Exit;
  end;

  MemoOutput.Clear;
  StatusBar.SimpleText := 'Analyse en cours...';
  ButtonAnalyze.Enabled := False;

  try
    case RadioGroupAnalysis.ItemIndex of
      0: AnalyzeSentiment;
      1: ExtractEntities;
      2: SummarizeText;
      3: ClassifyText;
    end;
  finally
    ButtonAnalyze.Enabled := True;
    StatusBar.SimpleText := 'Prêt';
  end;
end;

procedure TFormTextAnalyzer.AnalyzeSentiment;
var
  Score: Double;
  Message: string;
begin
  // On utilise notre implémentation locale simple
  Score := SimpleSentimentAnalysis(MemoInput.Text);

  MemoOutput.Lines.Add('Analyse de sentiment :');
  MemoOutput.Lines.Add(Format('Score : %.2f (de -1 négatif à +1 positif)', [Score]));

  // Interprétation du score
  if Score > 0.25 then
    Message := 'Le texte exprime un sentiment globalement positif.'
  else if Score < -0.25 then
    Message := 'Le texte exprime un sentiment globalement négatif.'
  else
    Message := 'Le texte exprime un sentiment plutôt neutre.';

  MemoOutput.Lines.Add('');
  MemoOutput.Lines.Add(Message);
end;

procedure TFormTextAnalyzer.ExtractEntities;
begin
  // Cette implémentation utiliserait idéalement une API ou un script Python
  // Pour l'exemple, nous simulons la détection
  MemoOutput.Lines.Add('Entités détectées :');
  MemoOutput.Lines.Add('');

  // Code simplifié qui recherche des patterns courants
  var Text := MemoInput.Text.ToLower;

  // Recherche de dates potentielles (très simplifié)
  var DatePattern := TRegEx.Create('\d{1,2}[\/\.-]\d{1,2}[\/\.-]\d{2,4}');
  var Matches := DatePattern.Matches(Text);
  if Matches.Count > 0 then
  begin
    MemoOutput.Lines.Add('Dates :');
    for var Match in Matches do
      MemoOutput.Lines.Add('- ' + Match.Value);
    MemoOutput.Lines.Add('');
  end;

  // Recherche d'emails (simplifié)
  var EmailPattern := TRegEx.Create('[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}');
  Matches := EmailPattern.Matches(Text);
  if Matches.Count > 0 then
  begin
    MemoOutput.Lines.Add('Emails :');
    for var Match in Matches do
      MemoOutput.Lines.Add('- ' + Match.Value);
    MemoOutput.Lines.Add('');
  end;

  // Dans une implémentation réelle, on utiliserait une API NLP complète
  MemoOutput.Lines.Add('Note : Cette fonctionnalité utilise une détection basique.');
  MemoOutput.Lines.Add('Pour une analyse complète, l''intégration d''une API NLP professionnelle est recommandée.');
end;

procedure TFormTextAnalyzer.SummarizeText;
begin
  // Pour la démo, on fait un résumé très basique basé sur la première phrase
  var Sentences := TRegEx.Split(MemoInput.Text, '[.!?]');

  MemoOutput.Lines.Add('Résumé automatique :');
  MemoOutput.Lines.Add('');

  if Length(Sentences) > 0 then
  begin
    // On prend la première phrase comme résumé (très simplifié)
    MemoOutput.Lines.Add(Sentences[0].Trim + '.');

    // Si le texte est plus long, on ajoute une note
    if Length(Sentences) > 1 then
    begin
      MemoOutput.Lines.Add('');
      MemoOutput.Lines.Add('Note : Ceci est un résumé très simplifié basé sur la première phrase.');
      MemoOutput.Lines.Add('Un vrai système de résumé utiliserait des algorithmes plus sophistiqués');
      MemoOutput.Lines.Add('ou une API comme GPT pour générer un résumé pertinent.');
    end;
  end
  else
    MemoOutput.Lines.Add('Texte trop court pour être résumé.');
end;

procedure TFormTextAnalyzer.ClassifyText;
var
  Categories: array[0..4] of string;
  Scores: array[0..4] of Integer;
  MaxScore, MaxIndex, i: Integer;
  Words: TArray<string>;
  WordLower: string;
begin
  // Définition de catégories simples avec des mots-clés associés
  Categories[0] := 'Business';
  Categories[1] := 'Technologie';
  Categories[2] := 'Sport';
  Categories[3] := 'Santé';
  Categories[4] := 'Divertissement';

  // Initialisation des scores
  for i := 0 to 4 do
    Scores[i] := 0;

  // Séparation du texte en mots
  Words := MemoInput.Text.ToLower.Split([' ', '.', ',', '!', '?', ';', ':', '-'], TStringSplitOptions.ExcludeEmpty);

  // Analyse des mots
  for var Word in Words do
  begin
    WordLower := Word.ToLower;

    // Business
    if (WordLower = 'entreprise') or (WordLower = 'affaires') or
       (WordLower = 'économie') or (WordLower = 'finance') or
       (WordLower = 'marché') or (WordLower = 'investissement') then
      Inc(Scores[0]);

    // Technologie
    if (WordLower = 'informatique') or (WordLower = 'logiciel') or
       (WordLower = 'ordinateur') or (WordLower = 'internet') or
       (WordLower = 'technologie') or (WordLower = 'innovation') then
      Inc(Scores[1]);

    // Sport
    if (WordLower = 'match') or (WordLower = 'équipe') or
       (WordLower = 'football') or (WordLower = 'joueur') or
       (WordLower = 'compétition') or (WordLower = 'champion') then
      Inc(Scores[2]);

    // Santé
    if (WordLower = 'médecin') or (WordLower = 'santé') or
       (WordLower = 'maladie') or (WordLower = 'traitement') or
       (WordLower = 'hôpital') or (WordLower = 'patient') then
      Inc(Scores[3]);

    // Divertissement
    if (WordLower = 'film') or (WordLower = 'musique') or
       (WordLower = 'concert') or (WordLower = 'artiste') or
       (WordLower = 'spectacle') or (WordLower = 'cinéma') then
      Inc(Scores[4]);
  end;

  // Recherche de la catégorie avec le score le plus élevé
  MaxScore := -1;
  MaxIndex := -1;

  for i := 0 to 4 do
  begin
    if Scores[i] > MaxScore then
    begin
      MaxScore := Scores[i];
      MaxIndex := i;
    end;
  end;

  // Affichage des résultats
  MemoOutput.Lines.Add('Classification de texte :');
  MemoOutput.Lines.Add('');

  if MaxScore > 0 then
  begin
    MemoOutput.Lines.Add('Catégorie principale : ' + Categories[MaxIndex]);
    MemoOutput.Lines.Add('');
    MemoOutput.Lines.Add('Scores par catégorie :');
    for i := 0 to 4 do
      MemoOutput.Lines.Add(Format('- %s : %d', [Categories[i], Scores[i]]));
  end
  else
    MemoOutput.Lines.Add('Impossible de déterminer une catégorie claire.');

  MemoOutput.Lines.Add('');
  MemoOutput.Lines.Add('Note : Cette classification est très basique.');
  MemoOutput.Lines.Add('Pour une analyse précise, l''utilisation d''une API comme GPT est recommandée.');
end;

function TFormTextAnalyzer.SimpleSentimentAnalysis(const Text: string): Double;
type
  TWordSentiment = record
    Word: string;
    Score: Double;
  end;
const
  // Dictionnaire simplifié de mots avec leur score de sentiment
  SentimentWords: array[0..19] of TWordSentiment = (
    (Word: 'excellent'; Score: 1.0),
    (Word: 'bon'; Score: 0.7),
    (Word: 'super'; Score: 0.8),
    (Word: 'formidable'; Score: 0.9),
    (Word: 'agréable'; Score: 0.6),
    (Word: 'correct'; Score: 0.3),
    (Word: 'satisfaisant'; Score: 0.5),
    (Word: 'médiocre'; Score: -0.3),
    (Word: 'mauvais'; Score: -0.7),
    (Word: 'terrible'; Score: -1.0),
    (Word: 'horrible'; Score: -0.9),
    (Word: 'aimer'; Score: 0.6),
    (Word: 'adorer'; Score: 0.8),
    (Word: 'détester'; Score: -0.8),
    (Word: 'apprécier'; Score: 0.5),
    (Word: 'déplaire'; Score: -0.5),
    (Word: 'content'; Score: 0.6),
    (Word: 'heureux'; Score: 0.7),
    (Word: 'triste'; Score: -0.6),
    (Word: 'déçu'; Score: -0.6)
  );

  // Mots négatifs qui inversent le sentiment
  NegationWords: array[0..3] of string = ('ne', 'pas', 'jamais', 'aucun');
var
  Words: TArray<string>;
  TotalScore, WordScore: Double;
  WordCount, i, j: Integer;
  WordLower: string;
  IsNegated: Boolean;
begin
  // Séparation du texte en mots
  Words := Text.ToLower.Split([' ', '.', ',', '!', '?', ';', ':', '-'], TStringSplitOptions.ExcludeEmpty);

  TotalScore := 0;
  WordCount := 0;

  for i := 0 to Length(Words) - 1 do
  begin
    WordLower := Words[i];

    // Vérifier si ce mot est précédé d'une négation
    IsNegated := False;
    if (i > 0) then
    begin
      for j := 0 to High(NegationWords) do
      begin
        if (i > j) and (Words[i-j-1] = NegationWords[j]) then
        begin
          IsNegated := True;
          Break;
        end;
      end;
    end;

    // Rechercher le mot dans notre dictionnaire
    WordScore := 0;
    for j := 0 to High(SentimentWords) do
    begin
      if SentimentWords[j].Word = WordLower then
      begin
        WordScore := SentimentWords[j].Score;
        // Inverser le score si le mot est nié
        if IsNegated then
          WordScore := -WordScore;
        Break;
      end;
    end;

    // Si on a trouvé un score pour ce mot, l'ajouter au total
    if WordScore <> 0 then
    begin
      TotalScore := TotalScore + WordScore;
      Inc(WordCount);
    end;
  end;

  // Calcul du score moyen (éviter la division par zéro)
  if WordCount > 0 then
    Result := TotalScore / WordCount
  else
    Result := 0;
end;

procedure TFormTextAnalyzer.ButtonClearClick(Sender: TObject);
begin
  MemoInput.Clear;
  MemoOutput.Clear;
end;

procedure TFormTextAnalyzer.ShowError(const ErrorMsg: string);
begin
  MemoOutput.Lines.Add('ERREUR :');
  MemoOutput.Lines.Add(ErrorMsg);
  StatusBar.SimpleText := 'Erreur';
end;

end.
```

Le fichier DFM correspondant pourrait être :

```delphi
object FormTextAnalyzer: TFormTextAnalyzer
  Left = 0
  Top = 0
  Caption = 'Analyseur de Texte NLP'
  ClientHeight = 472
  ClientWidth = 757
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object MemoInput: TMemo
    Left = 16
    Top = 16
    Width = 353
    Height = 401
    Lines.Strings = (
      'Entrez votre texte ici...')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object MemoOutput: TMemo
    Left = 384
    Top = 80
    Width = 353
    Height = 337
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ButtonAnalyze: TButton
    Left = 384
    Top = 423
    Width = 153
    Height = 33
    Caption = 'Analyser'
    TabOrder = 2
    OnClick = ButtonAnalyzeClick
  end
  object RadioGroupAnalysis: TRadioGroup
    Left = 384
    Top = 16
    Width = 353
    Height = 58
    Caption = 'Type d'#39'analyse'
    Columns = 2
    Items.Strings = (
      'Sentiment'
      'Entit'#233's'
      'R'#233'sum'#233
      'Classification')
    TabOrder = 3
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 453
    Width = 757
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Pr'#234't'
    ExplicitLeft = 384
    ExplicitTop = 392
    ExplicitWidth = 185
  end
  object ButtonClear: TButton
    Left = 584
    Top = 423
    Width = 153
    Height = 33
    Caption = 'Effacer'
    TabOrder = 5
    OnClick = ButtonClearClick
  end
end
```

## Amélioration de l'application : Utilisation de grands modèles de langage (LLM)

Les approches intégrées que nous avons vues précédemment ont leurs limites. Pour des fonctionnalités NLP vraiment avancées, l'utilisation des grands modèles de langage (LLM) comme GPT offre des possibilités bien plus puissantes.

### Qu'est-ce qu'un LLM ?

Un grand modèle de langage (LLM) est une intelligence artificielle entraînée sur d'énormes quantités de texte. Ces modèles sont capables de :

- Comprendre le contexte d'une conversation
- Générer du texte de qualité humaine
- Répondre à des questions complexes
- Effectuer des tâches linguistiques sophistiquées

Les LLM les plus connus sont GPT (OpenAI), LLaMA (Meta) et Claude (Anthropic).

### Intégration d'un LLM dans votre application Delphi

Voyons comment intégrer ces modèles puissants à notre application :

#### 1. Utilisation de l'API OpenAI avec Delphi

```delphi
unit LLMHelper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, REST.Client, REST.Types;

type
  TLLMFunction = (lfSentimentAnalysis, lfSummarization, lfEntityExtraction,
                  lfClassification, lfTranslation, lfQuestionAnswering);

  TLLMHelper = class
  private
    FAPI_Key: string;
    FLastError: string;
    function GetSystemPrompt(FunctionType: TLLMFunction): string;
    function FormatContent(const Content: string; FunctionType: TLLMFunction): string;
  public
    constructor Create(const API_Key: string);
    function ExecuteLLMFunction(const InputText: string; FunctionType: TLLMFunction): string;
    property LastError: string read FLastError;
  end;

implementation

constructor TLLMHelper.Create(const API_Key: string);
begin
  FAPI_Key := API_Key;
  FLastError := '';
end;

function TLLMHelper.GetSystemPrompt(FunctionType: TLLMFunction): string;
begin
  case FunctionType of
    lfSentimentAnalysis:
      Result := 'Analyser le sentiment du texte. Répondre avec un score entre -1 (très négatif) et 1 (très positif), ' +
                'suivi d''une brève explication. Format: "Score: X.XX\nExplication: ..."';

    lfSummarization:
      Result := 'Résumer le texte suivant en 2-3 phrases maximum, en capturant les points principaux.';

    lfEntityExtraction:
      Result := 'Extraire toutes les entités du texte suivant. Les classer par type (Personnes, Organisations, Lieux, Dates, etc.) ' +
                'et répondre au format JSON simple.';

    lfClassification:
      Result := 'Classifier le texte dans une des catégories suivantes: Business, Technologie, Sport, Santé, Divertissement, ' +
                'Politique, Science, Art, Éducation, Autre. Inclure un score de confiance et une brève justification.';

    lfTranslation:
      Result := 'Traduire le texte suivant en français.';

    lfQuestionAnswering:
      Result := 'Répondre à la question de manière concise et factuelle, basée uniquement sur les informations contenues dans le texte.';
  end;
end;

function TLLMHelper.FormatContent(const Content: string; FunctionType: TLLMFunction): string;
var
  ContentJSON: TJSONObject;
  MessagesArray: TJSONArray;
  SystemMessageObj, UserMessageObj: TJSONObject;
begin
  // Création du contenu JSON pour l'API GPT
  ContentJSON := TJSONObject.Create;
  try
    // Spécification du modèle
    ContentJSON.AddPair('model', 'gpt-3.5-turbo');

    // Configuration de la température (déterminisme)
    ContentJSON.AddPair('temperature', TJSONNumber.Create(0.2));

    // Création du tableau de messages
    MessagesArray := TJSONArray.Create;

    // Message système qui définit le rôle
    SystemMessageObj := TJSONObject.Create;
    SystemMessageObj.AddPair('role', 'system');
    SystemMessageObj.AddPair('content', GetSystemPrompt(FunctionType));
    MessagesArray.Add(SystemMessageObj);

    // Message utilisateur avec le contenu à analyser
    UserMessageObj := TJSONObject.Create;
    UserMessageObj.AddPair('role', 'user');
    UserMessageObj.AddPair('content', Content);
    MessagesArray.Add(UserMessageObj);

    ContentJSON.AddPair('messages', MessagesArray);

    Result := ContentJSON.ToJSON;
  finally
    ContentJSON.Free;
  end;
end;

function TLLMHelper.ExecuteLLMFunction(const InputText: string; FunctionType: TLLMFunction): string;
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestContent: string;
  ResponseJSON: TJSONValue;
begin
  Result := '';
  FLastError := '';

  // Vérification des entrées
  if InputText.Trim = '' then
  begin
    FLastError := 'Le texte d''entrée est vide';
    Exit;
  end;

  RESTClient := TRESTClient.Create('https://api.openai.com/v1/chat/completions');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    // Configuration du client REST
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout des en-têtes
    RESTRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Authorization', 'Bearer ' + FAPI_Key, TRESTRequestParameterKind.pkHTTPHEADER);

    // Préparation du corps de la requête
    RequestContent := FormatContent(InputText, FunctionType);
    RESTRequest.Body.Add(RequestContent);

    // Exécution de la requête
    RESTRequest.Execute;

    // Traitement de la réponse
    if RESTResponse.StatusCode = 200 then
    begin
      ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
      try
        // Extraction de la réponse
        Result := ResponseJSON.GetValue<TJSONArray>('choices')
                            .Items[0]
                            .GetValue<TJSONObject>('message')
                            .GetValue<string>('content');
        Result := Result.Trim;
      finally
        ResponseJSON.Free;
      end;
    end
    else
    begin
      FLastError := Format('Erreur %d: %s', [RESTResponse.StatusCode, RESTResponse.StatusText]);
      if RESTResponse.Content <> '' then
      begin
        try
          ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content);
          if ResponseJSON <> nil then
          begin
            if ResponseJSON.TryGetValue<string>('error.message', Result) then
              FLastError := FLastError + ' - ' + Result;
            ResponseJSON.Free;
          end;
        except
          // Ignorer les erreurs de parsing JSON ici
        end;
      end;
    end;
  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;

end.
```

#### 2. Utilisation dans notre application

Modifions notre formulaire pour intégrer cette nouvelle fonctionnalité :

```delphi
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  Vcl.ExtCtrls, REST.Client, REST.Types, System.JSON, LLMHelper;

type
  TFormTextAnalyzer = class(TForm)
    MemoInput: TMemo;
    MemoOutput: TMemo;
    ButtonAnalyze: TButton;
    RadioGroupAnalysis: TRadioGroup;
    StatusBar: TStatusBar;
    ButtonClear: TButton;
    CheckBoxUseLLM: TCheckBox;
    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FLLMHelper: TLLMHelper;
    procedure AnalyzeSentiment;
    procedure ExtractEntities;
    procedure SummarizeText;
    procedure ClassifyText;
    procedure ShowError(const ErrorMsg: string);
    function SimpleSentimentAnalysis(const Text: string): Double;
    procedure AnalyzeWithLLM;
  public
    { Public declarations }
  end;

var
  FormTextAnalyzer: TFormTextAnalyzer;

implementation

{$R *.dfm}

const
  API_KEY = 'votre_cle_api'; // Remplacez par votre clé API

procedure TFormTextAnalyzer.FormCreate(Sender: TObject);
begin
  RadioGroupAnalysis.ItemIndex := 0;
  FLLMHelper := TLLMHelper.Create(API_KEY);
end;

procedure TFormTextAnalyzer.FormDestroy(Sender: TObject);
begin
  FLLMHelper.Free;
end;

procedure TFormTextAnalyzer.ButtonAnalyzeClick(Sender: TObject);
begin
  if MemoInput.Text.Trim = '' then
  begin
    ShowMessage('Veuillez entrer du texte à analyser.');
    Exit;
  end;

  MemoOutput.Clear;
  StatusBar.SimpleText := 'Analyse en cours...';
  ButtonAnalyze.Enabled := False;
  Application.ProcessMessages;

  try
    if CheckBoxUseLLM.Checked then
      AnalyzeWithLLM
    else
    begin
      case RadioGroupAnalysis.ItemIndex of
        0: AnalyzeSentiment;
        1: ExtractEntities;
        2: SummarizeText;
        3: ClassifyText;
      end;
    end;
  finally
    ButtonAnalyze.Enabled := True;
    StatusBar.SimpleText := 'Prêt';
  end;
end;

procedure TFormTextAnalyzer.AnalyzeWithLLM;
var
  LLMFunction: TLLMFunction;
  Result: string;
begin
  // Détermination de la fonction LLM à utiliser
  case RadioGroupAnalysis.ItemIndex of
    0: LLMFunction := lfSentimentAnalysis;
    1: LLMFunction := lfEntityExtraction;
    2: LLMFunction := lfSummarization;
    3: LLMFunction := lfClassification;
    else LLMFunction := lfSentimentAnalysis;
  end;

  // Appel au LLM
  Result := FLLMHelper.ExecuteLLMFunction(MemoInput.Text, LLMFunction);

  // Affichage des résultats
  if Result <> '' then
  begin
    MemoOutput.Lines.Add('Résultat de l''analyse IA :');
    MemoOutput.Lines.Add('');
    MemoOutput.Lines.Add(Result);
  end
  else
  begin
    ShowError('Erreur lors de l''appel au modèle de langage: ' + FLLMHelper.LastError);
  end;
end;

// Les autres méthodes restent les mêmes...
```

Et modifions le fichier DFM pour ajouter la case à cocher :

```delphi
object FormTextAnalyzer: TFormTextAnalyzer
  // ...les autres propriétés restent identiques
  object CheckBoxUseLLM: TCheckBox
    Left = 16
    Top = 423
    Width = 353
    Height = 17
    Caption = 'Utiliser l''IA avancée (GPT) pour l''analyse'
    TabOrder = 6
  end
  // ...
end
```

## Cas d'utilisation pratiques du NLP en Delphi

Voyons maintenant quelques cas d'utilisation concrets du NLP dans vos applications Delphi :

### 1. Chatbot intelligent pour le support client

```delphi
procedure TChatbotForm.HandleUserMessage(const UserMessage: string);
var
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  RequestBody, ResponseJSON: TJSONObject;
  MessagesArray: TJSONArray;
  BotResponse: string;
begin
  // Ajouter le message de l'utilisateur à l'historique
  AddMessageToChat('Utilisateur', UserMessage, True);

  // Préparation de la requête à l'API
  RESTClient := TRESTClient.Create('https://api.openai.com/v1/chat/completions');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);
  RequestBody := TJSONObject.Create;

  try
    // Configuration du client REST
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Method := TRESTRequestMethod.rmPOST;

    // Ajout des en-têtes
    RESTRequest.AddParameter('Content-Type', 'application/json', TRESTRequestParameterKind.pkHTTPHEADER);
    RESTRequest.AddParameter('Authorization', 'Bearer ' + OPENAI_API_KEY, TRESTRequestParameterKind.pkHTTPHEADER);

    // Création du tableau de messages
    MessagesArray := TJSONArray.Create;

    // Message système qui définit le rôle
    MessagesArray.Add(
      TJSONObject.Create
        .AddPair('role', 'system')
        .AddPair('content', 'Vous êtes un assistant de support client pour notre logiciel de gestion. ' +
                           'Répondez de manière concise, professionnelle et utile. ' +
                           'Si vous ne connaissez pas la réponse, proposez de transférer à un agent humain.')
    );

    // Ajout du contexte de la conversation (historique limité)
    for var i := Max(0, FChatHistory.Count - 5) to FChatHistory.Count - 1 do
    begin
      MessagesArray.Add(
        TJSONObject.Create
          .AddPair('role', FChatHistory[i].Role)
          .AddPair('content', FChatHistory[i].Content)
      );
    end;

    // Message utilisateur actuel
    MessagesArray.Add(
      TJSONObject.Create
        .AddPair('role', 'user')
        .AddPair('content', UserMessage)
    );

    // Préparation du corps de la requête
    RequestBody.AddPair('model', 'gpt-3.5-turbo');
    RequestBody.AddPair('messages', MessagesArray);
    RequestBody.AddPair('temperature', TJSONNumber.Create(0.7));
    RequestBody.AddPair('max_tokens', TJSONNumber.Create(300));

    RESTRequest.Body.Add(RequestBody.ToJSON);

    // Affichage d'un indicateur "en cours de frappe"
    AddMessageToChat('Assistant', '...', False);

    // Exécution de la requête
    RESTRequest.ExecuteAsync(
      procedure
      begin
        if RESTResponse.StatusCode = 200 then
        begin
          ResponseJSON := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;
          try
            BotResponse :=
              ResponseJSON.GetValue<TJSONArray>('choices')
                         .Items[0]
                         .GetValue<TJSONObject>('message')
                         .GetValue<string>('content');

            // Mise à jour de l'interface utilisateur
            TThread.Synchronize(nil,
              procedure
              begin
                // Supprimer l'indicateur "en cours de frappe"
                RemoveLastMessage;

                // Ajouter la vraie réponse
                AddMessageToChat('Assistant', BotResponse, False);

                // Activer le champ de saisie
                EditUserInput.Enabled := True;
                ButtonSend.Enabled := True;
              end
            );
          finally
            ResponseJSON.Free;
          end;
        end
        else
        begin
          // Gestion de l'erreur
          TThread.Synchronize(nil,
            procedure
            begin
              RemoveLastMessage;
              AddMessageToChat('Assistant', 'Désolé, je rencontre un problème technique. Veuillez réessayer.', False);
              EditUserInput.Enabled := True;
              ButtonSend.Enabled := True;
            end
          );
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
```

### 2. Système de classification automatique de documents

```delphi
// Cette classe gère la classification automatique de documents texte
TDocumentClassifier = class
private
  FLLMHelper: TLLMHelper;
  FDocumentTypes: TStringList;
  FCategoryRules: TObjectDictionary<string, TRegEx>;

  function ClassifyWithRules(const DocumentText: string): string;
  function ClassifyWithLLM(const DocumentText: string): string;
public
  constructor Create(const API_Key: string);
  destructor Destroy; override;

  // Ajoute une règle de classification basée sur des expressions régulières
  procedure AddCategoryRule(const Category: string; const RegexPattern: string);

  // Classifie un document, en utilisant d'abord les règles puis le LLM si nécessaire
  function ClassifyDocument(const DocumentPath: string): string;
end;

constructor TDocumentClassifier.Create(const API_Key: string);
begin
  FLLMHelper := TLLMHelper.Create(API_Key);
  FDocumentTypes := TStringList.Create;
  FCategoryRules := TObjectDictionary<string, TRegEx>.Create([doOwnsValues]);

  // Ajout de quelques catégories par défaut
  FDocumentTypes.Add('Facture');
  FDocumentTypes.Add('Contrat');
  FDocumentTypes.Add('Rapport');
  FDocumentTypes.Add('Email');
  FDocumentTypes.Add('Demande');
  FDocumentTypes.Add('Autre');
end;

destructor TDocumentClassifier.Destroy;
begin
  FLLMHelper.Free;
  FDocumentTypes.Free;
  FCategoryRules.Free;
  inherited;
end;

procedure TDocumentClassifier.AddCategoryRule(const Category: string; const RegexPattern: string);
begin
  // Ajoute une règle pour identifier une catégorie
  if not FDocumentTypes.Contains(Category) then
    FDocumentTypes.Add(Category);

  FCategoryRules.AddOrSetValue(Category, TRegEx.Create(RegexPattern, [roIgnoreCase]));
end;

function TDocumentClassifier.ClassifyWithRules(const DocumentText: string): string;
var
  Category: string;
  Matches: TMatchCollection;
begin
  // Essaie de classifier le document en utilisant des règles
  for Category in FCategoryRules.Keys do
  begin
    Matches := FCategoryRules[Category].Matches(DocumentText);
    if Matches.Count > 0 then
      Exit(Category);
  end;

  // Si aucune règle ne correspond
  Result := '';
end;

function TDocumentClassifier.ClassifyWithLLM(const DocumentText: string): string;
var
  LLMResponse, CleanResponse: string;
  ResponseParts: TArray<string>;
begin
  // Utilise le LLM pour classifier le document
  LLMResponse := FLLMHelper.ExecuteLLMFunction(DocumentText, lfClassification);

  // Essaie d'extraire la catégorie de la réponse
  // Format attendu : "Catégorie: X\nConfiance: Y%\nExplication: Z"
  if LLMResponse <> '' then
  begin
    ResponseParts := LLMResponse.Split([#10, #13], TStringSplitOptions.ExcludeEmpty);
    if Length(ResponseParts) > 0 then
    begin
      CleanResponse := ResponseParts[0];
      if CleanResponse.Contains(':') then
        Result := CleanResponse.Split([':'])[1].Trim
      else
        Result := CleanResponse.Trim;

      // Vérifier si la catégorie est dans notre liste
      if not FDocumentTypes.Contains(Result) then
        Result := 'Autre';
    end
    else
      Result := 'Autre';
  end
  else
    Result := 'Autre';
end;

function TDocumentClassifier.ClassifyDocument(const DocumentPath: string): string;
var
  DocumentText: string;
  FileLines: TStringList;
begin
  // Lecture du document
  FileLines := TStringList.Create;
  try
    FileLines.LoadFromFile(DocumentPath);
    DocumentText := FileLines.Text;

    // Première tentative avec les règles
    Result := ClassifyWithRules(DocumentText);

    // Si les règles ne donnent rien, on utilise le LLM
    if Result = '' then
      Result := ClassifyWithLLM(DocumentText);
  finally
    FileLines.Free;
  end;
end;
```

### 3. Création d'un outil d'analyse de retours clients

Le NLP peut être utilisé pour analyser automatiquement les commentaires des clients et en extraire des informations précieuses :

```delphi
// Exemple simplifié d'analyse de commentaires clients
procedure TFeedbackAnalyzer.AnalyzeFeedback(const FeedbackList: TStrings; var Report: TStrings);
var
  LLMHelper: TLLMHelper;
  OverallSentiment: Double;
  FeedbackText, Response: string;
  TopicCounts: TDictionary<string, Integer>;
  Topic: string;
  AllComments, AllResponses: TStringList;
begin
  if FeedbackList.Count = 0 then
    Exit;

  LLMHelper := TLLMHelper.Create(API_KEY);
  TopicCounts := TDictionary<string, Integer>.Create;
  AllComments := TStringList.Create;
  AllResponses := TStringList.Create;

  try
    // Regrouper tous les commentaires
    for var i := 0 to FeedbackList.Count - 1 do
      AllComments.Add(FeedbackList[i]);

    FeedbackText := AllComments.Text;

    // 1. Extraire les sujets principaux
    Report.Add('Analyse des commentaires clients');
    Report.Add('===============================');
    Report.Add('');

    Response := LLMHelper.ExecuteLLMFunction(
      'Analyser ces commentaires clients et identifier les 5 principaux sujets abordés, ' +
      'avec le nombre d''occurrences pour chacun: ' + FeedbackText,
      lfQuestionAnswering
    );

    Report.Add('SUJETS PRINCIPAUX:');
    Report.Add(Response);
    Report.Add('');

    // 2. Analyser le sentiment global
    Response := LLMHelper.ExecuteLLMFunction(
      'Analyser le sentiment global dans ces commentaires clients. ' +
      'Donner un score entre -1 (très négatif) et 1 (très positif): ' + FeedbackText,
      lfSentimentAnalysis
    );

    Report.Add('SENTIMENT GLOBAL:');
    Report.Add(Response);
    Report.Add('');

    // 3. Extraire les points positifs et négatifs
    Response := LLMHelper.ExecuteLLMFunction(
      'Lister les 3 principaux points positifs et les 3 principaux points négatifs ' +
      'mentionnés dans ces commentaires clients: ' + FeedbackText,
      lfQuestionAnswering
    );

    Report.Add('POINTS POSITIFS ET NÉGATIFS:');
    Report.Add(Response);
    Report.Add('');

    // 4. Suggestions d'amélioration
    Response := LLMHelper.ExecuteLLMFunction(
      'En te basant sur ces commentaires clients, quelles seraient les 3 principales ' +
      'suggestions d''amélioration pour notre produit/service: ' + FeedbackText,
      lfQuestionAnswering
    );

    Report.Add('SUGGESTIONS D''AMÉLIORATION:');
    Report.Add(Response);

  finally
    LLMHelper.Free;
    TopicCounts.Free;
    AllComments.Free;
    AllResponses.Free;
  end;
end;
```

## Bonnes pratiques pour l'utilisation du NLP dans Delphi

### Gestion des API keys

Ne stockez jamais les clés API directement dans votre code. Utilisez plutôt :

```delphi
function LoadAPIKey: string;
var
  KeyFile: TStringList;
begin
  Result := '';
  // Vérifie d'abord les variables d'environnement
  Result := GetEnvironmentVariable('OPENAI_API_KEY');

  // Sinon, essaie de charger depuis un fichier
  if Result = '' then
  begin
    KeyFile := TStringList.Create;
    try
      if FileExists(APIKeyFilePath) then
      begin
        KeyFile.LoadFromFile(APIKeyFilePath);
        if KeyFile.Count > 0 then
          Result := KeyFile[0].Trim;
      end;
    finally
      KeyFile.Free;
    end;
  end;

  // Toujours vérifier si la clé a été trouvée
  if Result = '' then
    ShowMessage('API key introuvable. Veuillez configurer votre clé API.');
end;
```

### Traitement asynchrone

Pour éviter de bloquer l'interface utilisateur pendant les appels API qui peuvent prendre du temps :

```delphi
procedure TFormNLP.ButtonAnalyzeClick(Sender: TObject);
begin
  ButtonAnalyze.Enabled := False;
  StatusBar.SimpleText := 'Analyse en cours...';

  // Lancer l'analyse dans un thread séparé
  TTask.Run(
    procedure
    var
      Result: string;
    begin
      try
        // Appel à l'API dans un thread séparé
        Result := LLMHelper.ExecuteLLMFunction(MemoInput.Text, lfSentimentAnalysis);

        // Mettre à jour l'UI dans le thread principal
        TThread.Queue(nil,
          procedure
          begin
            MemoOutput.Lines.Add('Résultat :');
            MemoOutput.Lines.Add(Result);
            ButtonAnalyze.Enabled := True;
            StatusBar.SimpleText := 'Prêt';
          end
        );
      except
        on E: Exception do
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Erreur : ' + E.Message);
              ButtonAnalyze.Enabled := True;
              StatusBar.SimpleText := 'Erreur';
            end
          );
        end;
      end;
    end
  );
end;
```

### Mise en cache des résultats

Pour économiser des appels API et améliorer les performances :

```delphi
type
  TNLPCache = class
  private
    FCache: TDictionary<string, TCacheEntry>;
    FMaxAge: Integer; // En minutes

    procedure CleanupExpiredEntries;
  public
    constructor Create(MaxAgeMinutes: Integer = 60);
    destructor Destroy; override;

    procedure Add(const Request, Response: string);
    function TryGetResponse(const Request: string; out Response: string): Boolean;
  end;

constructor TNLPCache.Create(MaxAgeMinutes: Integer);
begin
  FCache := TDictionary<string, TCacheEntry>.Create;
  FMaxAge := MaxAgeMinutes;
end;

destructor TNLPCache.Destroy;
begin
  FCache.Free;
  inherited;
end;

procedure TNLPCache.Add(const Request, Response: string);
var
  Entry: TCacheEntry;
begin
  Entry.Response := Response;
  Entry.Timestamp := Now;
  FCache.AddOrSetValue(Request, Entry);

  // Nettoyer périodiquement
  if FCache.Count mod 20 = 0 then
    CleanupExpiredEntries;
end;

function TNLPCache.TryGetResponse(const Request: string; out Response: string): Boolean;
var
  Entry: TCacheEntry;
begin
  Result := FCache.TryGetValue(Request, Entry);

  if Result then
  begin
    // Vérifier si l'entrée n'est pas expirée
    if MinutesBetween(Now, Entry.Timestamp) > FMaxAge then
    begin
      FCache.Remove(Request);
      Result := False;
    end
    else
      Response := Entry.Response;
  end;
end;

procedure TNLPCache.CleanupExpiredEntries;
var
  KeysToRemove: TList<string>;
  Key: string;
begin
  KeysToRemove := TList<string>.Create;
  try
    for Key in FCache.Keys do
    begin
      if MinutesBetween(Now, FCache[Key].Timestamp) > FMaxAge then
        KeysToRemove.Add(Key);
    end;

    // Supprimer les entrées expirées
    for Key in KeysToRemove do
      FCache.Remove(Key);
  finally
    KeysToRemove.Free;
  end;
end;
```

Voici comment utiliser cette classe de cache :

```delphi
procedure TFormNLP.ButtonAnalyzeClick(Sender: TObject);
var
  Request, Response: string;
begin
  Request := MemoInput.Text;

  // Vérifier d'abord dans le cache
  if FNLPCache.TryGetResponse(Request, Response) then
  begin
    // Utiliser la réponse mise en cache
    MemoOutput.Lines.Add('Résultat (depuis cache) :');
    MemoOutput.Lines.Add(Response);
    Exit;
  end;

  // Sinon, effectuer l'appel API
  ButtonAnalyze.Enabled := False;
  StatusBar.SimpleText := 'Analyse en cours...';

  TTask.Run(
    procedure
    begin
      try
        // Appel à l'API
        Response := LLMHelper.ExecuteLLMFunction(Request, lfSentimentAnalysis);

        // Stocker dans le cache
        FNLPCache.Add(Request, Response);

        // Mettre à jour l'UI
        TThread.Queue(nil,
          procedure
          begin
            MemoOutput.Lines.Add('Résultat :');
            MemoOutput.Lines.Add(Response);
            ButtonAnalyze.Enabled := True;
            StatusBar.SimpleText := 'Prêt';
          end
        );
      except
        on E: Exception do
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Erreur : ' + E.Message);
              ButtonAnalyze.Enabled := True;
              StatusBar.SimpleText := 'Erreur';
            end
          );
        end;
      end;
    end
  );
end;
```

### Gestion de la limite de tokens

Les API de LLM ont souvent des limites sur la taille des entrées. Voici comment gérer cette contrainte :

```delphi
function TruncateInputForLLM(const Input: string; MaxTokens: Integer = 3000): string;
var
  Words: TArray<string>;
  Result: TStringBuilder;
  TokenCount, i: Integer;
begin
  // Estimation très approximative : 1 token ≈ 0.75 mots
  Words := Input.Split([' ', #10, #13], TStringSplitOptions.ExcludeEmpty);

  // Si le texte est déjà assez court, le retourner tel quel
  if Length(Words) < MaxTokens * 0.75 then
    Exit(Input);

  // Sinon, tronquer tout en gardant un message explicatif
  Result := TStringBuilder.Create;
  try
    // Ajouter un avertissement
    Result.Append('[Texte tronqué pour respecter les limites de l''API] ');

    // Calculer combien de mots on peut garder (en laissant de la marge)
    TokenCount := Round(MaxTokens * 0.6);

    // Ajouter les mots jusqu'à la limite
    for i := 0 to Min(Length(Words) - 1, TokenCount - 1) do
      Result.Append(Words[i]).Append(' ');

    // Retourner le résultat
    Exit(Result.ToString);
  finally
    Result.Free;
  end;
end;
```

### Traitement d'erreur robuste

Les appels à des API externes peuvent échouer pour diverses raisons. Une gestion d'erreur robuste est essentielle :

```delphi
function SafeExecuteLLMFunction(const LLMHelper: TLLMHelper;
                               const InputText: string;
                               FunctionType: TLLMFunction;
                               RetryCount: Integer = 3): string;
var
  Attempts: Integer;
  Success: Boolean;
  WaitTime: Integer;
begin
  Result := '';
  Attempts := 0;
  Success := False;

  while (not Success) and (Attempts < RetryCount) do
  begin
    try
      Inc(Attempts);
      Result := LLMHelper.ExecuteLLMFunction(InputText, FunctionType);
      Success := (Result <> '') and (LLMHelper.LastError = '');
    except
      on E: Exception do
      begin
        // Log de l'erreur
        TLogger.Log('Erreur LLM (tentative ' + Attempts.ToString + '): ' + E.Message);

        if Attempts < RetryCount then
        begin
          // Attente exponentielle entre les tentatives
          WaitTime := 500 * (1 shl (Attempts - 1)); // 500ms, puis 1s, puis 2s...
          Sleep(WaitTime);
        end;
      end;
    end;
  end;

  if not Success then
    Result := 'Erreur: Impossible d''obtenir une réponse après ' + RetryCount.ToString + ' tentatives.';
end;
```

## Intégration du NLP pour des applications spécifiques

### 1. Suggestion de contenu pendant la saisie

```delphi
procedure TMemoWithSuggestions.DoSuggestion;
var
  CurrentText, Suggestion: string;
  LastChar: Char;
begin
  // Vérifier si on doit faire une suggestion
  CurrentText := Text;
  if Length(CurrentText) = 0 then
    Exit;

  LastChar := CurrentText[Length(CurrentText)];

  // Déclencher la suggestion après un point, deux-points ou point-virgule
  if not (LastChar in ['.', ':', ';']) then
    Exit;

  // Lancer la suggestion dans un thread séparé
  TTask.Run(
    procedure
    var
      SuggestionResult: string;
    begin
      try
        // Appel à l'API pour obtenir une suggestion
        SuggestionResult := FLLMHelper.ExecuteLLMFunction(
          'Suggérer une suite cohérente à ce texte (max 10 mots): ' + CurrentText,
          lfQuestionAnswering
        );

        // Mettre à jour l'UI dans le thread principal
        TThread.Queue(nil,
          procedure
          begin
            if SuggestionResult <> '' then
            begin
              // Afficher la suggestion en grisé
              FGhostText := SuggestionResult;
              Invalidate; // Déclenche un repeint du contrôle
            end;
          end
        );
      except
        // Ignorer les erreurs pour ne pas perturber l'utilisateur
      end;
    end
  );
end;

// Méthode de dessin personnalisée pour afficher le texte fantôme
procedure TMemoWithSuggestions.Paint;
var
  R: TRect;
  TextWidth: Integer;
  GhostTextRect: TRect;
begin
  // Dessiner le memo normalement
  inherited;

  // Si on a un texte fantôme à afficher
  if FGhostText <> '' then
  begin
    // Calculer la position où afficher le texte fantôme
    Canvas.Font := Font;
    R := ClientRect;

    // Trouver la largeur du texte actuel
    TextWidth := Canvas.TextWidth(Text);

    // Définir le rectangle pour le texte fantôme
    GhostTextRect := R;
    GhostTextRect.Left := GhostTextRect.Left + TextWidth;

    // Dessiner le texte fantôme en gris clair
    Canvas.Font.Color := clSilver;
    Canvas.TextOut(GhostTextRect.Left, GhostTextRect.Top, FGhostText);
  end;
end;

// Accepter la suggestion avec Tab
procedure TMemoWithSuggestions.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (Key = VK_TAB) and (FGhostText <> '') then
  begin
    // Accepter la suggestion
    Text := Text + FGhostText;
    FGhostText := '';
    SelStart := Length(Text);
    Key := 0; // Consommer l'événement
  end
  else if Key = VK_ESCAPE then
  begin
    // Rejeter la suggestion
    FGhostText := '';
    Invalidate;
    Key := 0; // Consommer l'événement
  end;
end;
```

### 2. Extraction automatique d'informations des emails

```delphi
procedure TEmailProcessor.ExtractInformation(const EmailText: string; var Info: TEmailInfo);
var
  LLMResponse: string;
  JSONResponse: TJSONObject;
begin
  // Demander au LLM d'extraire des informations structurées
  LLMResponse := FLLMHelper.ExecuteLLMFunction(
    'Extrais les informations suivantes de cet email et réponds au format JSON: ' +
    '{"expediteur": "nom", "date": "date", "objet": "objet", ' +
    '"demande_principale": "résumé de la demande", ' +
    '"urgence": "haute/moyenne/basse", "action_requise": "oui/non", ' +
    '"deadline": "date limite si mentionnée"}. ' +
    'Voici l''email: ' + EmailText,
    lfQuestionAnswering
  );

  // Analyser la réponse JSON
  try
    // Nettoyer la réponse si nécessaire (enlever les backticks, etc.)
    LLMResponse := CleanJSONResponse(LLMResponse);

    JSONResponse := TJSONObject.ParseJSONValue(LLMResponse) as TJSONObject;
    try
      // Remplir la structure avec les informations extraites
      Info.Expediteur := JSONResponse.GetValue<string>('expediteur');
      Info.Date := JSONResponse.GetValue<string>('date');
      Info.Objet := JSONResponse.GetValue<string>('objet');
      Info.DemandePrincipale := JSONResponse.GetValue<string>('demande_principale');
      Info.Urgence := JSONResponse.GetValue<string>('urgence');
      Info.ActionRequise := JSONResponse.GetValue<string>('action_requise') = 'oui';
      Info.Deadline := JSONResponse.GetValue<string>('deadline');
    finally
      JSONResponse.Free;
    end;
  except
    on E: Exception do
    begin
      // En cas d'erreur de parsing JSON
      Info.Expediteur := 'Erreur d''extraction';
      Info.DemandePrincipale := 'Impossible d''analyser cet email: ' + E.Message;
    end;
  end;
end;

// Fonction utilitaire pour nettoyer les réponses JSON
function TEmailProcessor.CleanJSONResponse(const Response: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := Response.Trim;

  // Supprimer les backticks qui encadrent parfois le JSON
  if Result.StartsWith('```json') or Result.StartsWith('```') then
  begin
    StartPos := Pos('```', Result) + 3;
    if Result.StartsWith('```json') then
      StartPos := StartPos + 4;

    EndPos := LastPos('```', Result);
    if EndPos > StartPos then
      Result := Copy(Result, StartPos, EndPos - StartPos).Trim
    else
      Result := Copy(Result, StartPos, Length(Result)).Trim;
  end;

  // Vérifier que le résultat est bien un JSON valide
  if not (Result.StartsWith('{') and Result.EndsWith('}')) then
    raise Exception.Create('Format JSON invalide');
end;
```

### 3. Analyse des sentiments sur les réseaux sociaux

```delphi
// Analyse des sentiments pour un tableau de messages
procedure TSocialMediaAnalyzer.AnalyzeTrends(const Messages: TArray<TSocialMessage>;
                                           var Report: TStringList);
var
  AllText, BatchText, Response: string;
  MessagesJSON, BatchJSON: TJSONArray;
  SentimentByTopic: TDictionary<string, Double>;
  Topic: string;
  Score: Double;
  i, BatchSize, BatchCount: Integer;
begin
  // Initialisation
  SentimentByTopic := TDictionary<string, Double>.Create;
  MessagesJSON := TJSONArray.Create;

  try
    // Création d'un tableau JSON avec tous les messages
    for i := 0 to High(Messages) do
    begin
      MessagesJSON.Add(
        TJSONObject.Create
          .AddPair('text', Messages[i].Text)
          .AddPair('source', Messages[i].Source)
          .AddPair('date', FormatDateTime('yyyy-mm-dd', Messages[i].PostDate))
      );
    end;

    // Traitement par lots pour éviter les limites de tokens
    BatchSize := 20; // Nombre de messages par lot
    BatchCount := Ceil(Length(Messages) / BatchSize);

    Report.Add('ANALYSE DES TENDANCES SUR LES RÉSEAUX SOCIAUX');
    Report.Add('=============================================');
    Report.Add('');
    Report.Add('Période analysée: ' +
              FormatDateTime('dd/mm/yyyy', Messages[Low(Messages)].PostDate) + ' au ' +
              FormatDateTime('dd/mm/yyyy', Messages[High(Messages)].PostDate));
    Report.Add('Nombre de messages analysés: ' + Length(Messages).ToString);
    Report.Add('');

    // 1. Identifier les sujets principaux
    Response := FLLMHelper.ExecuteLLMFunction(
      'Identifie les 5 principaux sujets discutés dans ces messages des réseaux sociaux. ' +
      'Réponds simplement avec une liste numérotée des sujets: ' +
      MessagesJSON.ToJSON,
      lfQuestionAnswering
    );

    Report.Add('SUJETS PRINCIPAUX DÉTECTÉS:');
    Report.Add(Response);
    Report.Add('');

    // 2. Analyse du sentiment par lot
    Report.Add('ANALYSE DES SENTIMENTS:');

    for i := 0 to BatchCount - 1 do
    begin
      // Créer un lot de messages
      BatchJSON := TJSONArray.Create;
      try
        for var j := i * BatchSize to Min((i+1) * BatchSize - 1, High(Messages)) do
          BatchJSON.Add(
            TJSONObject.Create
              .AddPair('text', Messages[j].Text)
          );

        BatchText := BatchJSON.ToJSON;

        // Analyser ce lot
        Response := FLLMHelper.ExecuteLLMFunction(
          'Analyse le sentiment global de ces messages et donne un score entre -1 (très négatif) ' +
          'et 1 (très positif): ' + BatchText,
          lfSentimentAnalysis
        );

        // Extraire le score de sentiment (simplifié)
        if TryExtractSentimentScore(Response, Score) then
        begin
          Report.Add(Format('Lot %d: Score = %.2f - %s', [
            i + 1,
            Score,
            SentimentToText(Score)
          ]));
        end;
      finally
        BatchJSON.Free;
      end;
    end;

    Report.Add('');

    // 3. Recommandations marketing
    Response := FLLMHelper.ExecuteLLMFunction(
      'En te basant sur ces messages des réseaux sociaux, quelles sont les 3 recommandations ' +
      'marketing les plus pertinentes que tu pourrais faire ? ' +
      MessagesJSON.ToJSON,
      lfQuestionAnswering
    );

    Report.Add('RECOMMANDATIONS MARKETING:');
    Report.Add(Response);

  finally
    MessagesJSON.Free;
    SentimentByTopic.Free;
  end;
end;

function TSocialMediaAnalyzer.TryExtractSentimentScore(const Response: string; out Score: Double): Boolean;
var
  Match: TMatch;
begin
  // Tente d'extraire un score au format -1.00 à 1.00 de la réponse
  Match := TRegEx.Match(Response, '[-]?\d+\.\d+');
  Result := Match.Success and TryStrToFloat(Match.Value, Score);

  // Vérifier que le score est dans la plage valide
  if Result then
    Result := (Score >= -1.0) and (Score <= 1.0);

  // Si on n'a pas trouvé de score, essayer d'analyser le texte
  if not Result then
  begin
    if Response.Contains('positif') and not Response.Contains('négatif') then
    begin
      if Response.Contains('très positif') or Response.Contains('extrêmement positif') then
        Score := 0.8
      else
        Score := 0.5;
      Result := True;
    end
    else if Response.Contains('négatif') and not Response.Contains('positif') then
    begin
      if Response.Contains('très négatif') or Response.Contains('extrêmement négatif') then
        Score := -0.8
      else
        Score := -0.5;
      Result := True;
    end
    else if Response.Contains('neutre') then
    begin
      Score := 0;
      Result := True;
    end;
  end;
end;

function TSocialMediaAnalyzer.SentimentToText(Score: Double): string;
begin
  if Score >= 0.7 then
    Result := 'Très positif'
  else if Score >= 0.3 then
    Result := 'Positif'
  else if Score > -0.3 then
    Result := 'Neutre'
  else if Score > -0.7 then
    Result := 'Négatif'
  else
    Result := 'Très négatif';
end;
```

## Limites et considérations

Lorsque vous intégrez des fonctionnalités NLP dans vos applications Delphi, gardez à l'esprit ces limitations importantes :

### 1. Confidentialité et sécurité des données

Les API de NLP nécessitent l'envoi de données à des serveurs externes. Assurez-vous de :

- Obtenir le consentement des utilisateurs pour le traitement de leurs données
- Ne pas envoyer d'informations sensibles ou confidentielles
- Informer les utilisateurs que leurs textes peuvent être transmis à des tiers
- Vérifier la conformité avec les réglementations comme le RGPD

### 2. Coûts des API

Les API de NLP ont généralement un coût basé sur l'utilisation. Pour gérer ces coûts :

- Mettez en place un système de mise en cache efficace
- Limitez la taille des textes envoyés
- Implémentez des quotas d'utilisation
- Surveillez votre consommation d'API

### 3. Fiabilité des résultats

Les modèles de NLP ne sont pas parfaits :

- Ils peuvent produire des résultats inexacts ou biaisés
- Les performances varient selon la langue (meilleurs en anglais)
- Ils peuvent "halluciner" des informations
- La qualité des résultats dépend de la clarté des instructions

### 4. Performance et considérations techniques

- Les appels API introduisent une latence
- Les grands modèles peuvent avoir des limites de tokens
- Les API peuvent être temporairement indisponibles
- La qualité de la connexion internet affecte l'expérience utilisateur

## Ressources et bibliothèques NLP pour Delphi

Pour aller plus loin dans l'intégration du NLP à vos applications Delphi :

### Bibliothèques et wrappers

- **DelphiOpenAI** : Wrapper pour l'API OpenAI
- **GreenAI** : Bibliothèque pour intégrer plusieurs API de NLP
- **NLPVision** : Composants Delphi pour le traitement du langage naturel
- **SpaCy4Delphi** : Wrapper pour la bibliothèque SpaCy

### Services Cloud NLP populaires

- **OpenAI (ChatGPT, GPT-4)** : Modèles avancés de génération de texte
- **Google Cloud Natural Language** : Analyse de sentiments, entités, syntaxe
- **Azure Language Services** : Services NLP de Microsoft
- **Amazon Comprehend** : Service NLP d'AWS
- **HuggingFace** : Plateforme open-source avec des milliers de modèles

### Forums et groupes de discussion

- **DelphiPraxis** : Forum Delphi avec section IA/ML
- **Stack Overflow** : Questions et réponses sur l'intégration NLP avec Delphi
- **Reddit r/Delphi** : Communauté Delphi avec discussions sur l'IA

## Conclusion

L'intégration du traitement du langage naturel dans vos applications Delphi ouvre de nouvelles possibilités pour créer des logiciels plus intelligents et interactifs. Que vous choisissiez d'utiliser des API cloud avancées ou d'implémenter des solutions simples directement en Delphi, le NLP peut transformer l'expérience utilisateur de vos applications.

Les exemples présentés dans ce chapitre vous ont donné une introduction complète aux différentes approches, avec des cas d'utilisation pratiques que vous pouvez adapter à vos propres projets. En suivant les bonnes pratiques de mise en œuvre et en tenant compte des limites de ces technologies, vous pourrez exploiter efficacement la puissance du NLP dans vos applications Delphi.

Dans le prochain chapitre, nous explorerons la reconnaissance d'images et de formes, une autre branche fascinante de l'intelligence artificielle qui peut être intégrée à vos applications Delphi.

---

> **Remarque**: Les exemples de code présentés sont destinés à illustrer les concepts et peuvent nécessiter des adaptations pour fonctionner dans votre environnement spécifique.
