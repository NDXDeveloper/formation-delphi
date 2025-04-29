# 13.8 Outils de traduction et flux de travail

L'internationalisation d'une application ne se limite pas à l'aspect technique. Elle implique également un processus de traduction et une organisation du travail adaptés. Dans cette section, nous explorerons les outils et méthodes qui facilitent la traduction de vos applications Delphi ainsi que les meilleures pratiques pour gérer ce processus.

## Organisation des ressources de traduction

Avant de commencer le processus de traduction, il est essentiel de bien organiser vos ressources linguistiques.

### Structure des fichiers de traduction

Voici une structure de projet recommandée pour faciliter la gestion des traductions :

```
MonProjet/
├── Source/              # Code source de l'application
├── Languages/           # Dossier de ressources linguistiques
│   ├── fr/             # Français
│   │   ├── strings.ini
│   │   └── messages.ini
│   ├── en/             # Anglais
│   │   ├── strings.ini
│   │   └── messages.ini
│   ├── de/             # Allemand
│   │   ├── strings.ini
│   │   └── messages.ini
│   └── templates/      # Modèles pour nouvelles traductions
├── Assets/             # Images et autres ressources
└── Bin/                # Exécutables et DLLs
```

> 💡 Structurer vos fichiers de traduction par langue facilite l'ajout de nouvelles langues et la maintenance.

### Format des fichiers de traduction

Plusieurs formats sont possibles pour stocker vos traductions :

#### 1. Fichiers INI

Simple et facile à éditer manuellement :

```ini
[Common]
OK=OK
Cancel=Annuler
Save=Enregistrer
Open=Ouvrir

[MainForm]
Title=Mon Application
Welcome=Bienvenue dans mon application
```

#### 2. Fichiers XML

Plus structuré et adapté aux outils de traduction :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<translation language="fr">
  <section name="Common">
    <item id="OK">OK</item>
    <item id="Cancel">Annuler</item>
    <item id="Save">Enregistrer</item>
    <item id="Open">Ouvrir</item>
  </section>
  <section name="MainForm">
    <item id="Title">Mon Application</item>
    <item id="Welcome">Bienvenue dans mon application</item>
  </section>
</translation>
```

#### 3. Fichiers PO (GNU gettext)

Format standard pour les traductions :

```
msgid "OK"
msgstr "OK"

msgid "Cancel"
msgstr "Annuler"

msgid "Save"
msgstr "Enregistrer"

msgid "Open"
msgstr "Ouvrir"

msgid "Welcome"
msgstr "Bienvenue dans mon application"
```

#### 4. Fichiers de ressources (.rc)

Format natif de Windows :

```
STRINGTABLE
BEGIN
  1, "OK"
  2, "Annuler"
  3, "Enregistrer"
  4, "Ouvrir"
  5, "Bienvenue dans mon application"
END
```

> ⚠️ Choisissez un format adapté à votre équipe et à vos outils. Pour les projets simples, les fichiers INI sont souvent suffisants. Pour des projets plus complexes, le format PO offre des fonctionnalités avancées.

## Extraction des chaînes à traduire

La première étape consiste à extraire toutes les chaînes à traduire de votre code source.

### Extraction manuelle

Pour les petits projets, vous pouvez extraire manuellement les chaînes :

```pascal
// Remplacer
Label1.Caption := 'Bienvenue';
Button1.Caption := 'Enregistrer';

// Par
Label1.Caption := TranslationManager.GetString('MainForm.Welcome');
Button1.Caption := TranslationManager.GetString('Common.Save');
```

### Extraction automatique

Pour les projets plus importants, utilisez des outils d'extraction automatique :

1. **GNUGettext for Delphi** : Peut analyser votre code et extraire les chaînes marquées avec la fonction `_()`
2. **ITE (Integrated Translation Environment)** : Outil commercial avec extraction automatique
3. **TsiLang** : Solution commerciale complète pour l'internationalisation

### Exemple d'extraction avec scripts personnalisés

Vous pouvez créer un script simple pour extraire les chaînes :

```pascal
procedure ExtractStringsFromCode(const SourceDir, OutputFile: string);
var
  FileList: TStringList;
  SourceFile: TStringList;
  OutputList: TStringList;
  FileName, Line: string;
  I, J, Start, Finish: Integer;
  StringsFound: TDictionary<string, string>;
begin
  FileList := TStringList.Create;
  SourceFile := TStringList.Create;
  OutputList := TStringList.Create;
  StringsFound := TDictionary<string, string>.Create;

  try
    // Trouver tous les fichiers .pas dans le répertoire source
    FindAllFiles(SourceDir, '*.pas', FileList);

    // Parcourir chaque fichier
    for I := 0 to FileList.Count - 1 do
    begin
      FileName := FileList[I];
      SourceFile.LoadFromFile(FileName);

      // Analyser chaque ligne
      for J := 0 to SourceFile.Count - 1 do
      begin
        Line := SourceFile[J];

        // Rechercher les chaînes entre apostrophes
        Start := Pos('''', Line);
        while Start > 0 do
        begin
          Finish := PosEx('''', Line, Start + 1);
          if Finish > Start then
          begin
            // Extraire la chaîne
            StringsFound.AddOrSetValue(
              Copy(Line, Start + 1, Finish - Start - 1),
              ExtractFileName(FileName) + ':' + IntToStr(J + 1)
            );
          end;

          // Continuer la recherche
          Start := PosEx('''', Line, Finish + 1);
        end;
      end;
    end;

    // Générer le fichier de sortie
    OutputList.Add('[Strings]');
    for var Pair in StringsFound do
      OutputList.Add(
        Format('"%s"="%s" ; %s', [Pair.Key, Pair.Key, Pair.Value])
      );

    // Enregistrer le fichier
    OutputList.SaveToFile(OutputFile);

  finally
    FileList.Free;
    SourceFile.Free;
    OutputList.Free;
    StringsFound.Free;
  end;
end;
```

## Utilisation d'outils de traduction spécialisés

Plusieurs outils facilitent la traduction des applications Delphi.

### 1. Outils intégrés à Delphi

Delphi propose quelques outils intégrés pour la gestion des traductions :

- **Resource DLL Wizard** : Pour créer des DLL de ressources localisées
- **Translation Manager** : Disponible dans certaines éditions, aide à gérer les traductions

### 2. Solutions tierces pour Delphi

Plusieurs solutions tierces existent spécifiquement pour Delphi :

#### dxgettext

[dxgettext](https://sourceforge.net/projects/dxgettext/) est une implémentation de GNU gettext pour Delphi :

```pascal
uses
  gnugettext;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisation de gettext
  TranslateComponent(Self);

  // Utilisation
  Label1.Caption := _('Welcome to my application');  // Fonction _() pour la traduction
end;
```

#### TsiLang

Solution commerciale complète avec éditeur visuel et outils d'extraction :

```pascal
uses
  TsiLang;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // TsiLangComponent ajouté au formulaire
  siLang1.Active := True;
  siLang1.SwitchTo('fr');  // Passer au français
end;
```

#### ITE (Integrated Translation Environment)

Éditeur de traduction spécialisé pour Delphi avec prévisualisation :

```pascal
uses
  ITE;

// ITE fournit des composants visuels et des outils d'extraction
```

### 3. Outils génériques de traduction

Des outils génériques peuvent également être utilisés :

- **Poedit** : Éditeur pour les fichiers PO (GNU gettext)
- **OmegaT** : Outil de traduction assistée par ordinateur (TAO)
- **SDL Trados** : Solution professionnelle de traduction
- **memoQ** : Outil de traduction professionnel

## Flux de travail de traduction

Voici un flux de travail typique pour la traduction d'une application Delphi :

### 1. Préparation du code

Avant même de commencer la traduction :

```pascal
// ÉVITER
Label1.Caption := 'Bienvenue ' + UserName + '! Vous avez ' + IntToStr(MessageCount) + ' messages.';

// PRÉFÉRER
Label1.Caption := Format(GetTranslatedString('Welcome'), [UserName, MessageCount]);
```

Où `'Welcome'` dans le fichier de traduction serait :
```
Welcome=Bienvenue %s ! Vous avez %d messages.
```

### 2. Extraction des chaînes

Utilisez un des outils mentionnés pour extraire les chaînes à traduire :

```
ExtractStrings.exe -src C:\MyProject\Source -out C:\MyProject\Languages\templates\strings.ini
```

### 3. Création des fichiers modèles

Préparez des fichiers modèles pour chaque langue :

```pascal
procedure CreateLanguageTemplate(const TemplateFile, OutputFile: string);
var
  Template, Output: TStringList;
  I: Integer;
  Key, Value: string;
begin
  Template := TStringList.Create;
  Output := TStringList.Create;

  try
    Template.LoadFromFile(TemplateFile);

    // Copier la structure mais vider les valeurs
    for I := 0 to Template.Count - 1 do
    begin
      if Template[I].StartsWith('[') then
        // Conserver les sections
        Output.Add(Template[I])
      else
      begin
        // Extraire la clé, mais laisser la valeur vide pour traduction
        Key := Template.Names[I];
        if Key <> '' then
          Output.Add(Key + '=');
      end;
    end;

    // Enregistrer le modèle
    Output.SaveToFile(OutputFile);

  finally
    Template.Free;
    Output.Free;
  end;
end;
```

### 4. Traduction

Maintenant, vous pouvez procéder à la traduction elle-même :

#### Option 1 : Traduction manuelle

Éditez directement les fichiers de traduction avec un éditeur de texte ou un tableur.

#### Option 2 : Utilisation d'un outil de traduction

Utilisez des outils comme Poedit ou OmegaT pour remplir les traductions manquantes.

#### Option 3 : Services de traduction professionnels

Confiez les fichiers à des traducteurs professionnels qui comprennent le format de vos fichiers.

#### Option 4 : Services de traduction automatique

Pour un premier jet rapide, utilisez des services comme DeepL ou Google Translate :

```pascal
procedure TranslateUsingAPI(const SourceFile, TargetFile, SourceLang, TargetLang: string);
var
  Source, Target: TStringList;
  I: Integer;
  Line, Key, Value, TranslatedValue: string;
begin
  Source := TStringList.Create;
  Target := TStringList.Create;

  try
    Source.LoadFromFile(SourceFile);

    // Copier la structure
    for I := 0 to Source.Count - 1 do
    begin
      Line := Source[I];

      // Conserver les sections et commentaires
      if Line.StartsWith('[') or Line.StartsWith(';') then
        Target.Add(Line)
      else
      begin
        // Extraire la clé et la valeur
        Key := Source.Names[I];
        Value := Source.ValueFromIndex[I];

        if (Key <> '') and (Value <> '') then
        begin
          // Appeler l'API de traduction (à implémenter)
          TranslatedValue := TranslateText(Value, SourceLang, TargetLang);
          Target.Add(Key + '=' + TranslatedValue);
        end
        else
          Target.Add(Line);
      end;
    end;

    // Enregistrer le fichier traduit
    Target.SaveToFile(TargetFile);

  finally
    Source.Free;
    Target.Free;
  end;
end;
```

> ⚠️ La traduction automatique n'est qu'un point de départ. Une révision humaine est toujours nécessaire pour garantir la qualité.

### 5. Intégration et tests

Une fois les traductions prêtes, intégrez-les dans votre application :

```pascal
procedure TForm1.LoadTranslations;
begin
  // Charger les traductions pour la langue actuelle
  TranslationManager.LoadLanguage(CurrentLanguage);

  // Appliquer les traductions à l'interface
  UpdateUITranslations;
end;

procedure TForm1.UpdateUITranslations;
begin
  // Mettre à jour le formulaire principal
  Caption := TranslationManager.GetString('MainForm.Title');

  // Mettre à jour les composants
  lblWelcome.Caption := TranslationManager.GetString('MainForm.Welcome');
  btnSave.Caption := TranslationManager.GetString('Common.Save');
  btnCancel.Caption := TranslationManager.GetString('Common.Cancel');

  // Etc.
end;
```

## Gestion des mises à jour de traduction

La gestion des mises à jour est une tâche importante, surtout lorsque votre application évolue.

### Détection des chaînes modifiées ou supprimées

Utilisez un outil de comparaison pour identifier les différences :

```pascal
procedure CompareTwoVersions(const OldFile, NewFile, ReportFile: string);
var
  OldStrings, NewStrings, Report: TStringList;
  OldDict, NewDict: TDictionary<string, string>;
  Key: string;
begin
  OldStrings := TStringList.Create;
  NewStrings := TStringList.Create;
  Report := TStringList.Create;
  OldDict := TDictionary<string, string>.Create;
  NewDict := TDictionary<string, string>.Create;

  try
    // Charger les fichiers
    OldStrings.LoadFromFile(OldFile);
    NewStrings.LoadFromFile(NewFile);

    // Construire les dictionnaires pour faciliter la comparaison
    BuildDictionaryFromIniFile(OldStrings, OldDict);
    BuildDictionaryFromIniFile(NewStrings, NewDict);

    // Rapport des chaînes ajoutées
    Report.Add('=== CHAÎNES AJOUTÉES ===');
    for Key in NewDict.Keys do
    begin
      if not OldDict.ContainsKey(Key) then
        Report.Add('+ ' + Key + '=' + NewDict[Key]);
    end;

    // Rapport des chaînes supprimées
    Report.Add('');
    Report.Add('=== CHAÎNES SUPPRIMÉES ===');
    for Key in OldDict.Keys do
    begin
      if not NewDict.ContainsKey(Key) then
        Report.Add('- ' + Key + '=' + OldDict[Key]);
    end;

    // Rapport des chaînes modifiées
    Report.Add('');
    Report.Add('=== CHAÎNES MODIFIÉES ===');
    for Key in NewDict.Keys do
    begin
      if OldDict.ContainsKey(Key) and (NewDict[Key] <> OldDict[Key]) then
        Report.Add('* ' + Key + '=' + OldDict[Key] + ' -> ' + NewDict[Key]);
    end;

    // Enregistrer le rapport
    Report.SaveToFile(ReportFile);

  finally
    OldStrings.Free;
    NewStrings.Free;
    Report.Free;
    OldDict.Free;
    NewDict.Free;
  end;
end;
```

### Mise à jour des fichiers de traduction existants

Mettez à jour les fichiers de traduction sans perdre le travail déjà effectué :

```pascal
procedure UpdateTranslationFile(const TemplateFile, ExistingFile, OutputFile: string);
var
  Template, Existing, Output: TStringList;
  TemplateDict, ExistingDict: TDictionary<string, string>;
  I: Integer;
  Key, Value, Section: string;
  CurrentSection: string;
begin
  Template := TStringList.Create;
  Existing := TStringList.Create;
  Output := TStringList.Create;
  TemplateDict := TDictionary<string, string>.Create;
  ExistingDict := TDictionary<string, string>.Create;

  try
    Template.LoadFromFile(TemplateFile);
    Existing.LoadFromFile(ExistingFile);

    // Construire un dictionnaire à partir du fichier existant
    BuildDictionaryFromIniFile(Existing, ExistingDict);

    // Suivre la section actuelle
    CurrentSection := '';

    // Parcourir le modèle et créer un nouveau fichier
    for I := 0 to Template.Count - 1 do
    begin
      Line := Template[I];

      // Détecter les sections
      if Line.StartsWith('[') and Line.EndsWith(']') then
      begin
        CurrentSection := Line;
        Output.Add(Line);
      end
      else
      begin
        // Traiter les entrées de traduction
        Key := Template.Names[I];
        if Key <> '' then
        begin
          // Construire la clé complète avec la section
          FullKey := CurrentSection + '.' + Key;

          // Vérifier si la traduction existe déjà
          if ExistingDict.TryGetValue(Key, Value) and (Value <> '') then
            Output.Add(Key + '=' + Value)
          else
            Output.Add(Key + '=' + Template.ValueFromIndex[I]);
        end
        else
          Output.Add(Line);  // Lignes vides ou commentaires
      end;
    end;

    // Enregistrer le fichier mis à jour
    Output.SaveToFile(OutputFile);

  finally
    Template.Free;
    Existing.Free;
    Output.Free;
    TemplateDict.Free;
    ExistingDict.Free;
  end;
end;
```

## Création d'un éditeur de traduction simple

Pour faciliter le travail des traducteurs, vous pouvez créer un éditeur de traduction simple :

```pascal
unit TranslationEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmTranslationEditor = class(TForm)
    pnlTop: TPanel;
    lblLanguages: TLabel;
    cmbSourceLang: TComboBox;
    cmbTargetLang: TComboBox;
    btnLoad: TButton;
    btnSave: TButton;
    lvwStrings: TListView;
    pnlEdit: TPanel;
    lblKey: TLabel;
    lblSource: TLabel;
    lblTranslation: TLabel;
    edtKey: TEdit;
    memSource: TMemo;
    memTranslation: TMemo;
    btnUpdate: TButton;
    stsBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure lvwStringsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure btnUpdateClick(Sender: TObject);
  private
    FSourceFile: string;
    FTargetFile: string;
    procedure LoadTranslationFiles;
    procedure UpdateListItem(Index: Integer);
  end;

var
  frmTranslationEditor: TfrmTranslationEditor;

implementation

{$R *.dfm}

procedure TfrmTranslationEditor.FormCreate(Sender: TObject);
begin
  // Configurer la liste des langues
  cmbSourceLang.Items.Clear;
  cmbSourceLang.Items.Add('English');
  cmbSourceLang.Items.Add('French');
  cmbSourceLang.Items.Add('German');
  cmbSourceLang.ItemIndex := 0;

  cmbTargetLang.Items := cmbSourceLang.Items;
  cmbTargetLang.ItemIndex := 1;

  // Configurer la liste des chaînes
  lvwStrings.Columns.Clear;
  with lvwStrings.Columns.Add do
  begin
    Caption := 'Key';
    Width := 150;
  end;
  with lvwStrings.Columns.Add do
  begin
    Caption := 'Source';
    Width := 200;
  end;
  with lvwStrings.Columns.Add do
  begin
    Caption := 'Translation';
    Width := 200;
  end;
  with lvwStrings.Columns.Add do
  begin
    Caption := 'Status';
    Width := 80;
  end;
end;

procedure TfrmTranslationEditor.btnLoadClick(Sender: TObject);
begin
  LoadTranslationFiles;
end;

procedure TfrmTranslationEditor.LoadTranslationFiles;
var
  SourceDir, TargetDir: string;
  SourceLang, TargetLang: string;
  SourceIni, TargetIni: TIniFile;
  Sections: TStringList;
  Keys: TStringList;
  I, J: Integer;
  Section, Key: string;
  SourceText, TargetText: string;
  Item: TListItem;
  Status: string;
begin
  // Déterminer les répertoires de langue
  SourceLang := LowerCase(cmbSourceLang.Text);
  TargetLang := LowerCase(cmbTargetLang.Text);

  SourceDir := ExtractFilePath(Application.ExeName) + 'Languages\' + SourceLang;
  TargetDir := ExtractFilePath(Application.ExeName) + 'Languages\' + TargetLang;

  // Vérifier si les répertoires existent
  if not DirectoryExists(SourceDir) then
  begin
    ShowMessage('Source language directory not found: ' + SourceDir);
    Exit;
  end;

  if not DirectoryExists(TargetDir) then
  begin
    ShowMessage('Target language directory not found: ' + TargetDir);
    Exit;
  end;

  // Fichiers de traduction
  FSourceFile := SourceDir + '\strings.ini';
  FTargetFile := TargetDir + '\strings.ini';

  // Vérifier si les fichiers existent
  if not FileExists(FSourceFile) then
  begin
    ShowMessage('Source language file not found: ' + FSourceFile);
    Exit;
  end;

  if not FileExists(FTargetFile) then
  begin
    ShowMessage('Target language file not found: ' + FTargetFile);
    if MessageDlg('Create target file?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      CopyFile(PChar(FSourceFile), PChar(FTargetFile), True)
    else
      Exit;
  end;

  // Charger les fichiers INI
  SourceIni := TIniFile.Create(FSourceFile);
  TargetIni := TIniFile.Create(FTargetFile);
  Sections := TStringList.Create;
  Keys := TStringList.Create;

  try
    // Effacer la liste existante
    lvwStrings.Items.Clear;

    // Lire les sections du fichier source
    SourceIni.ReadSections(Sections);

    // Pour chaque section, lire les clés
    for I := 0 to Sections.Count - 1 do
    begin
      Section := Sections[I];
      Keys.Clear;
      SourceIni.ReadSection(Section, Keys);

      // Pour chaque clé, ajouter une entrée à la liste
      for J := 0 to Keys.Count - 1 do
      begin
        Key := Keys[J];
        SourceText := SourceIni.ReadString(Section, Key, '');
        TargetText := TargetIni.ReadString(Section, Key, '');

        // Déterminer le statut
        if TargetText = '' then
          Status := 'Missing'
        else if TargetText = SourceText then
          Status := 'Identical'
        else
          Status := 'Translated';

        // Ajouter à la liste
        Item := lvwStrings.Items.Add;
        Item.Caption := Section + '.' + Key;
        Item.SubItems.Add(SourceText);
        Item.SubItems.Add(TargetText);
        Item.SubItems.Add(Status);
      end;
    end;

    // Mise à jour du statut
    stsBar.SimpleText := Format('Loaded: %d strings, %d sections',
      [lvwStrings.Items.Count, Sections.Count]);

  finally
    SourceIni.Free;
    TargetIni.Free;
    Sections.Free;
    Keys.Free;
  end;
end;

procedure TfrmTranslationEditor.lvwStringsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  FullKey: string;
  DotPos: Integer;
  Section, Key: string;
begin
  if Selected and Assigned(Item) then
  begin
    // Extraire la section et la clé
    FullKey := Item.Caption;
    DotPos := Pos('.', FullKey);
    if DotPos > 0 then
    begin
      Section := Copy(FullKey, 1, DotPos - 1);
      Key := Copy(FullKey, DotPos + 1, Length(FullKey) - DotPos);

      // Afficher les informations
      edtKey.Text := FullKey;
      memSource.Text := Item.SubItems[0];
      memTranslation.Text := Item.SubItems[1];
    end;
  end;
end;

procedure TfrmTranslationEditor.btnUpdateClick(Sender: TObject);
var
  Index: Integer;
begin
  // Mettre à jour la traduction sélectionnée
  Index := lvwStrings.ItemIndex;
  if Index >= 0 then
  begin
    lvwStrings.Items[Index].SubItems[1] := memTranslation.Text;

    // Mettre à jour le statut
    if memTranslation.Text = '' then
      lvwStrings.Items[Index].SubItems[2] := 'Missing'
    else if memTranslation.Text = memSource.Text then
      lvwStrings.Items[Index].SubItems[2] := 'Identical'
    else
      lvwStrings.Items[Index].SubItems[2] := 'Translated';
  end;
end;

procedure TfrmTranslationEditor.btnSaveClick(Sender: TObject);
var
  TargetIni: TIniFile;
  I: Integer;
  FullKey: string;
  DotPos: Integer;
  Section, Key: string;
  Translation: string;
begin
  if FTargetFile = '' then
  begin
    ShowMessage('No target file loaded');
    Exit;
  end;

  // Sauvegarder les traductions
  TargetIni := TIniFile.Create(FTargetFile);
  try
    for I := 0 to lvwStrings.Items.Count - 1 do
    begin
      // Extraire la section et la clé
      FullKey := lvwStrings.Items[I].Caption;
      DotPos := Pos('.', FullKey);
      if DotPos > 0 then
      begin
        Section := Copy(FullKey, 1, DotPos - 1);
        Key := Copy(FullKey, DotPos + 1, Length(FullKey) - DotPos);
        Translation := lvwStrings.Items[I].SubItems[1];

        // Enregistrer dans le fichier INI
        TargetIni.WriteString(Section, Key, Translation);
      end;
    end;

    ShowMessage('Translations saved successfully');

  finally
    TargetIni.Free;
  end;
end;

end.
```

## Utilisation des services de traduction en ligne

Pour accélérer le processus de traduction, vous pouvez intégrer des services de traduction en ligne :

```pascal
unit OnlineTranslationService;

interface

uses
  System.SysUtils, System.Classes, System.Net.HttpClient, System.JSON;

type
  TTranslationService = class
  private
    FApiKey: string;
    FHttpClient: THTTPClient;
  public
    constructor Create(const ApiKey: string);
    destructor Destroy; override;
    function Translate(const Text, SourceLang, TargetLang: string): string;
  end;

implementation

constructor TTranslationService.Create(const ApiKey: string);
begin
  inherited Create;
  FApiKey := ApiKey;
  FHttpClient := THTTPClient.Create;
end;

destructor TTranslationService.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TTranslationService.Translate(const Text, SourceLang, TargetLang: string): string;
var
  URL: string;
  Response: IHTTPResponse;
  RequestContent: TStringStream;
  ResponseJson: TJSONObject;
  JsonValue: TJSONValue;
begin
  Result := Text;  // Par défaut, retourner le texte d'origine en cas d'échec

  if Text = '' then
    Exit;

  try
    // Exemple avec l'API DeepL (à adapter selon le service utilisé)
    URL := 'https://api-free.deepl.com/v2/translate';

    // Préparer les données de la requête
    RequestContent := TStringStream.Create(
      Format('auth_key=%s&text=%s&source_lang=%s&target_lang=%s',
      [FApiKey,
       TNetEncoding.URL.Encode(Text),
       SourceLang,
       TargetLang]));
    try
      // Envoyer la requête POST
      Response := FHttpClient.Post(URL, RequestContent, nil,
                                  TNetEncoding.URL.EncodeQuery('application/x-www-form-urlencoded'));

      // Traiter la réponse
      if Response.StatusCode = 200 then
      begin
        ResponseJson := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          if ResponseJson <> nil then
          begin
            // Extraire la traduction (structure spécifique à DeepL)
            JsonValue := ResponseJson.GetValue('translations[0].text');
            if JsonValue <> nil then
              Result := JsonValue.Value;
          end;
        finally
          ResponseJson.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Translation service error: %d - %s',
                                [Response.StatusCode, Response.StatusText]);
    finally
      RequestContent.Free;
    end;
  except
    on E: Exception do
    begin
      // Journaliser l'erreur mais retourner le texte original
      // pour ne pas bloquer l'application
      OutputDebugString(PChar('Translation error: ' + E.Message));
    end;
  end;
end;

end.
```

### Intégration avec l'éditeur de traduction

Vous pouvez intégrer ce service à votre éditeur de traduction :

```pascal
procedure TfrmTranslationEditor.btnAutoTranslateClick(Sender: TObject);
var
  TranslationService: TTranslationService;
  SourceLang, TargetLang: string;
  Index: Integer;
  SourceText, TranslatedText: string;
begin
  // Obtenir les codes de langue
  SourceLang := GetLanguageCode(cmbSourceLang.Text);
  TargetLang := GetLanguageCode(cmbTargetLang.Text);

  // Créer le service de traduction
  TranslationService := TTranslationService.Create('YOUR_API_KEY_HERE');
  try
    // Traduire l'élément sélectionné ou tous les éléments manquants
    if cbxTranslateAll.Checked then
    begin
      // Pour chaque élément non traduit
      for Index := 0 to lvwStrings.Items.Count - 1 do
      begin
        if lvwStrings.Items[Index].SubItems[2] = 'Missing' then
        begin
          SourceText := lvwStrings.Items[Index].SubItems[0];

          // Traduire avec le service en ligne
          TranslatedText := TranslationService.Translate(
            SourceText, SourceLang, TargetLang);

          // Mettre à jour la liste
          lvwStrings.Items[Index].SubItems[1] := TranslatedText;
          lvwStrings.Items[Index].SubItems[2] :=
            IfThen(TranslatedText = '', 'Missing', 'Auto');

          // Afficher la progression
          stsBar.SimpleText := Format('Translating: %d/%d',
            [Index + 1, lvwStrings.Items.Count]);
          Application.ProcessMessages;
        end;
      end;
    end
    else
    begin
      // Traduire uniquement l'élément sélectionné
      Index := lvwStrings.ItemIndex;
      if Index >= 0 then
      begin
        SourceText := memSource.Text;

        // Traduire avec le service en ligne
        TranslatedText := TranslationService.Translate(
          SourceText, SourceLang, TargetLang);

        // Mettre à jour l'interface
        memTranslation.Text := TranslatedText;
        btnUpdateClick(Sender);  // Mettre à jour la liste
      end;
    end;

    ShowMessage('Translation completed.');

  finally
    TranslationService.Free;
  end;
end;

function TfrmTranslationEditor.GetLanguageCode(const Language: string): string;
begin
  // Convertir le nom de la langue en code ISO
  if Language = 'English' then
    Result := 'EN'
  else if Language = 'French' then
    Result := 'FR'
  else if Language = 'German' then
    Result := 'DE'
  else if Language = 'Spanish' then
    Result := 'ES'
  else if Language = 'Italian' then
    Result := 'IT'
  else if Language = 'Portuguese' then
    Result := 'PT'
  else if Language = 'Russian' then
    Result := 'RU'
  else if Language = 'Chinese' then
    Result := 'ZH'
  else if Language = 'Japanese' then
    Result := 'JA'
  else if Language = 'Arabic' then
    Result := 'AR'
  else
    Result := 'EN';  // Par défaut
end;
```

> ⚠️ La plupart des services de traduction en ligne sont payants et nécessitent une clé API. Lisez attentivement les conditions d'utilisation et les tarifs avant d'intégrer un service.

## Bonnes pratiques pour un processus de traduction efficace

### 1. Planifiez l'internationalisation dès le début

Ne considérez pas la traduction comme une fonctionnalité à ajouter à la fin du développement :

- Concevez votre architecture pour faciliter l'internationalisation
- Utilisez des identifiants cohérents pour les chaînes
- Documentez les contextes d'utilisation des chaînes

### 2. Utilisez une méthode cohérente pour les identifiants de chaînes

Adoptez une convention de nommage claire pour les identifiants de chaînes :

```
[Formulaire/Module].[Contexte].[Identifiant]
```

Par exemple :
- `MainForm.Title` - Titre du formulaire principal
- `MainForm.Welcome` - Message de bienvenue
- `Common.OK` - Texte du bouton OK
- `Messages.FileNotFound` - Message d'erreur

### 3. Fournissez du contexte aux traducteurs

Incluez des informations contextuelles pour aider les traducteurs :

```ini
[Common]
; Button labels
OK=OK
Cancel=Annuler
Save=Enregistrer
; %s = username
Welcome=Bienvenue, %s !
; %d = number of messages
MessageCount=Vous avez %d messages.
```

### 4. Utilisez des paramètres plutôt que des concaténations

```pascal
// MAUVAIS EXEMPLE
LabelMessage.Caption := 'Vous avez ' + IntToStr(Count) + ' messages non lus.';

// BON EXEMPLE
LabelMessage.Caption := Format(GetTranslatedString('Messages.Count'), [Count]);
```

### 5. Créez un glossaire technique

Pour les termes spécifiques à votre domaine, créez un glossaire pour assurer la cohérence des traductions :

```
Terme         | Description                                  | Traduction
--------------|----------------------------------------------|------------
Item          | Un élément individuel dans une liste         | Élément (fr), Elemento (es)
Batch process | Traitement automatique de plusieurs éléments | Traitement par lot (fr), Proceso por lotes (es)
```

### 6. Impliquez des traducteurs natifs

Les traductions automatiques ne sont jamais parfaites :

- Faites réviser les traductions par des locuteurs natifs
- Tenez compte des spécificités culturelles
- Vérifiez la cohérence terminologique

### 7. Mettez en place un système de validation

Implémentez une étape de validation des traductions :

```pascal
procedure ValidateTranslations(const LangFile: string);
var
  Ini: TIniFile;
  Sections, Keys: TStringList;
  I, J: Integer;
  Section, Key, Value: string;
  NumEmpty, NumWithPlaceholders, NumWithIssues: Integer;
  Report: TStringList;
begin
  Ini := TIniFile.Create(LangFile);
  Sections := TStringList.Create;
  Keys := TStringList.Create;
  Report := TStringList.Create;

  try
    NumEmpty := 0;
    NumWithPlaceholders := 0;
    NumWithIssues := 0;

    // Lire toutes les sections
    Ini.ReadSections(Sections);

    Report.Add('Validation report for: ' + LangFile);
    Report.Add('Generated: ' + DateTimeToStr(Now));
    Report.Add('');
    Report.Add('=== ISSUES ===');

    // Pour chaque section
    for I := 0 to Sections.Count - 1 do
    begin
      Section := Sections[I];
      Keys.Clear;
      Ini.ReadSection(Section, Keys);

      // Pour chaque clé
      for J := 0 to Keys.Count - 1 do
      begin
        Key := Keys[J];
        Value := Ini.ReadString(Section, Key, '');

        // Vérifier les valeurs vides
        if Value = '' then
        begin
          Inc(NumEmpty);
          Report.Add(Format('EMPTY: [%s] %s', [Section, Key]));
        end
        // Vérifier les placeholders
        else if (Pos('%s', Value) > 0) or (Pos('%d', Value) > 0) then
        begin
          Inc(NumWithPlaceholders);
          // Vérifier si le nombre de placeholders est correct
          if CountOccurrences('%s', Value) <> CountExpectedPlaceholders(Section, Key, '%s') then
          begin
            Inc(NumWithIssues);
            Report.Add(Format('PLACEHOLDER MISMATCH: [%s] %s has incorrect number of %%s',
              [Section, Key]));
          end;
          if CountOccurrences('%d', Value) <> CountExpectedPlaceholders(Section, Key, '%d') then
          begin
            Inc(NumWithIssues);
            Report.Add(Format('PLACEHOLDER MISMATCH: [%s] %s has incorrect number of %%d',
              [Section, Key]));
          end;
        end;
      end;
    end;

    // Résumé
    Report.Add('');
    Report.Add('=== SUMMARY ===');
    Report.Add(Format('Total strings: %d', [GetTotalStringCount(Sections, Keys, Ini)]));
    Report.Add(Format('Empty strings: %d', [NumEmpty]));
    Report.Add(Format('Strings with placeholders: %d', [NumWithPlaceholders]));
    Report.Add(Format('Strings with issues: %d', [NumWithIssues]));

    // Enregistrer le rapport
    Report.SaveToFile(ChangeFileExt(LangFile, '.validation.txt'));

  finally
    Ini.Free;
    Sections.Free;
    Keys.Free;
    Report.Free;
  end;
end;
```

## Exemples de flux de travail complets

### Petite équipe avec traduction manuelle

Pour une petite application ou une petite équipe :

1. **Préparation**
   - Créez une structure de répertoires pour les langues
   - Définissez un gestionnaire de traduction simple

2. **Développement**
   - Utilisez la fonction `GetTranslatedString()` dans tout le code
   - Ajoutez les chaînes au fichier de la langue principale au fur et à mesure

3. **Extraction et traduction**
   - À la fin de chaque itération, extrayez les nouvelles chaînes
   - Créez/mettez à jour les fichiers de traduction pour chaque langue
   - Traduisez manuellement ou avec l'aide d'un service en ligne

4. **Test et validation**
   - Testez l'application dans toutes les langues supportées
   - Vérifiez les problèmes d'interface (troncatures, alignements)

### Équipe moyenne avec traducteurs externes

Pour une application plus importante :

1. **Préparation**
   - Mettez en place un système de gestion des chaînes (ex: GNU gettext)
   - Documentez le processus de traduction

2. **Développement**
   - Utilisez les fonctions de marquage de chaînes (ex: `_()`)
   - Révisez régulièrement le code pour vérifier l'internationalisation

3. **Extraction et préparation des traductions**
   - Extrayez les chaînes avec l'outil approprié (ex: `xgettext`)
   - Mettez à jour les fichiers de traduction existants (ex: `msgmerge`)
   - Préparez des packages pour les traducteurs

4. **Traduction externe**
   - Envoyez les fichiers aux traducteurs
   - Fournissez un contexte et un glossaire
   - Définissez un calendrier de livraison

5. **Intégration et validation**
   - Intégrez les traductions reçues (ex: `msgfmt`)
   - Validez le formatage et les placeholders
   - Testez l'application dans toutes les langues

### Grande équipe avec gestion continue des traductions

Pour les grands projets avec de nombreuses langues :

1. **Mise en place d'une infrastructure**
   - Utilisez un système de gestion des traductions en ligne (ex: Crowdin, Lokalise)
   - Intégrez-le dans votre pipeline CI/CD

2. **Développement continu**
   - Les développeurs marquent les chaînes avec les fonctions appropriées
   - Le système détecte automatiquement les nouvelles chaînes

3. **Traduction en parallèle**
   - Les traducteurs travaillent en continu sur la plateforme
   - Les révisions sont effectuées par des réviseurs désignés

4. **Intégration et déploiement**
   - Les traductions validées sont automatiquement intégrées
   - Les builds quotidiens incluent les dernières traductions
   - Les tests automatisés vérifient l'affichage dans toutes les langues

## Outils populaires pour la traduction de logiciels

### Outils spécifiques à Delphi

1. **dxgettext** - Implémentation de GNU gettext pour Delphi
   - Extraction automatique des chaînes
   - Support des fichiers PO/MO
   - Documentation en anglais

2. **TsiLang** - Solution commerciale complète
   - Éditeur visuel intégré
   - Prévisualisation des traductions
   - Support multiformat

3. **ITE (Integrated Translation Environment)**
   - Environnement dédié pour la traduction
   - Support de plusieurs formats
   - Fonctionnalités avancées pour les traducteurs

### Outils génériques

1. **Poedit** - Éditeur de fichiers PO
   - Interface conviviale
   - Support des mémoires de traduction
   - Vérification orthographique

2. **OmegaT** - Outil de traduction assistée par ordinateur
   - Open source et gratuit
   - Mémoire de traduction puissante
   - Support de nombreux formats

3. **Crowdin** - Plateforme de traduction collaborative en ligne
   - Interface web intuitive
   - Gestion des versions
   - API pour l'intégration

4. **Lokalise** - Plateforme de gestion des traductions
   - Intégration CI/CD
   - Outils de collaboration
   - Support de screenshots pour le contexte

## Conclusion

La mise en place d'un bon flux de travail de traduction est essentielle pour créer des applications internationales de qualité. Voici les points clés à retenir :

1. **Planifiez l'internationalisation dès le début** du développement
2. **Choisissez des outils adaptés** à la taille de votre projet et à votre équipe
3. **Fournissez du contexte** pour aider les traducteurs
4. **Automatisez ce qui peut l'être** pour gagner du temps et réduire les erreurs
5. **Testez régulièrement** votre application dans toutes les langues supportées
6. **Impliquez des locuteurs natifs** pour garantir la qualité des traductions
7. **Maintenez la cohérence** terminologique tout au long du projet

En suivant ces conseils et en utilisant les outils appropriés, vous faciliterez grandement le processus de traduction de votre application Delphi, ce qui vous permettra d'atteindre un public international plus large avec une expérience utilisateur de qualité.

---

Avec cette section sur les outils de traduction et les flux de travail, vous avez maintenant tous les éléments nécessaires pour internationaliser efficacement vos applications Delphi. Dans le prochain chapitre, nous aborderons l'utilisation d'API et de bibliothèques externes pour étendre les fonctionnalités de vos applications.
