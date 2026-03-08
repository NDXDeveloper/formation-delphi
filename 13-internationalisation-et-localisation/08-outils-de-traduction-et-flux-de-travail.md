🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.8 Outils de traduction et flux de travail

## Introduction

Traduire une application en plusieurs langues est un processus complexe qui nécessite organisation, outils appropriés et collaboration avec des traducteurs. Cette section présente les outils disponibles pour Delphi et les meilleures pratiques pour gérer efficacement le processus de traduction.

## Vue d'ensemble du processus de traduction

### Les étapes du flux de traduction

```
1. Développement
   ↓
2. Extraction des textes à traduire
   ↓
3. Préparation des fichiers pour traducteurs
   ↓
4. Traduction
   ↓
5. Révision et validation
   ↓
6. Intégration dans l'application
   ↓
7. Tests
   ↓
8. Correction et mise à jour
```

### Acteurs du processus

| Rôle | Responsabilités | Compétences requises |
|------|----------------|---------------------|
| **Développeur** | Extraction, intégration, tests techniques | Delphi, outils de traduction |
| **Chef de projet** | Coordination, planification, budget | Gestion de projet |
| **Traducteur** | Traduction des textes | Langue source et cible, contexte métier |
| **Réviseur linguistique** | Vérification qualité, cohérence | Expertise linguistique |
| **Testeur** | Tests fonctionnels dans chaque langue | Langue cible, connaissance produit |

## Outils intégrés de Delphi

### Integrated Translation Manager (ITM)

Delphi inclut un gestionnaire de traduction intégré pour les fichiers DFM.

#### Accès à l'ITM

```
Menu : Tools → Translation Manager
```

#### Fonctionnalités

| Fonctionnalité | Description |
|----------------|-------------|
| **Extraction** | Extrait tous les textes des fichiers DFM |
| **Édition** | Interface pour traduire les textes |
| **Import/Export** | Support de fichiers externes |
| **Prévisualisation** | Voir les traductions dans l'IDE |

#### Utilisation de l'ITM

**Étape 1 : Marquer les formulaires comme localisables**

```pascal
// Dans le formulaire, définir la propriété
procedure TForm1.ConfigurerLocalisation;  
begin  
  Self.Localizable := True; // Active la localisation
end;
```

**Étape 2 : Créer les versions linguistiques**

1. Sélectionner le formulaire
2. Dans l'Inspecteur d'objets : `Language` → choisir la langue (ex: `English`)
3. Modifier les textes des composants dans cette langue
4. Delphi crée automatiquement un fichier `.dfm` pour chaque langue

**Structure de fichiers créée :**

```
MonFormulaire.pas           // Code source  
MonFormulaire.dfm          // Version française (défaut)  
MonFormulaire.en.dfm       // Version anglaise  
MonFormulaire.es.dfm       // Version espagnole  
MonFormulaire.de.dfm       // Version allemande  
```

**Étape 3 : Utiliser Translation Manager**

```
1. Ouvrir Translation Manager
2. Créer un nouveau projet de traduction
3. Ajouter les formulaires à traduire
4. Sélectionner les langues cibles
5. Traduire dans l'interface ou exporter
```

### Avantages et limitations de l'ITM

| Avantages | Limitations |
|-----------|-------------|
| ✅ Intégré à Delphi | ❌ Limité aux fichiers DFM |
| ✅ Gratuit | ❌ Interface basique |
| ✅ Prévisualisation directe | ❌ Pas de mémoire de traduction |
| ✅ Gestion des versions | ❌ Collaboration limitée |

## Outils tiers professionnels

### Comparaison des outils

| Outil | Prix | Langues | Formats | Points forts |
|-------|------|---------|---------|--------------|
| **Sisulizer** | Payant (€€€) | 100+ | DFM, RC, EXE | Très complet, mémoire de traduction |
| **SDL Passolo** | Payant (€€€€) | Toutes | Tous | Standard industrie, workflows avancés |
| **LocFactory** | Payant (€€) | 50+ | DFM, RC | Simple, intégration IDE |
| **GNU gettext** | Gratuit | Toutes | PO | Open source, communauté active |
| **Poedit** | Gratuit/Payant | Toutes | PO | Interface moderne, suggestions |

### Sisulizer

Sisulizer est l'un des outils les plus populaires pour Delphi.

#### Caractéristiques principales

```
✓ Scan automatique des ressources
✓ Mémoire de traduction
✓ Traduction automatique (Google, DeepL)
✓ Validation et vérification
✓ Support Delphi natif
✓ Gestion de projet
```

#### Workflow avec Sisulizer

**1. Créer un projet Sisulizer**

```
File → New Project
→ Sélectionner l'exécutable ou les sources
→ Choisir les langues cibles
→ Scanner le projet
```

**2. Traduire**

```
- Interface similaire à un tableur
- Colonne source | Colonne traduction
- Suggestions de la mémoire de traduction
- Validation en temps réel
```

**3. Générer les fichiers localisés**

```
Build → Build All Languages
→ Génère les fichiers DFM localisés
→ Ou génère un exécutable par langue
```

#### Exemple de script Sisulizer

```xml
<?xml version="1.0" encoding="utf-8"?>
<project>
  <settings>
    <sourceLanguage>fr-FR</sourceLanguage>
    <targetLanguages>
      <language>en-US</language>
      <language>es-ES</language>
      <language>de-DE</language>
    </targetLanguages>
  </settings>
  <scans>
    <scan type="Delphi">
      <path>C:\Projects\MonApp\Source\*.pas</path>
      <path>C:\Projects\MonApp\Source\*.dfm</path>
    </scan>
  </scans>
</project>
```

### GNU gettext et Poedit

GNU gettext est le système de traduction open source le plus répandu.

#### Intégration gettext dans Delphi

**1. Installer les composants gettext pour Delphi**

Plusieurs bibliothèques disponibles :
- dxgettext
- gnugettext for Delphi
- Composants tiers

**2. Utiliser les fonctions de traduction**

```pascal
uses
  gnugettext;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Initialiser gettext
  TranslateComponent(Self);

  // Utiliser les fonctions de traduction
  Label1.Caption := _('Hello World');
  Button1.Caption := _('Click me');
  ShowMessage(_('Welcome to the application'));
end;
```

**3. Extraire les chaînes**

```bash
# Extraire toutes les chaînes du code source
xgettext --language=Object-Pascal -o messages.pot *.pas

# Créer un fichier de traduction pour une langue
msginit --input=messages.pot --locale=fr_FR -o fr_FR.po
```

**4. Traduire avec Poedit**

```
1. Ouvrir le fichier .po avec Poedit
2. Traduire chaque chaîne
3. Sauvegarder (génère automatiquement le .mo)
4. Placer le fichier .mo dans le dossier de l'application
```

#### Structure d'un fichier PO

```po
# Fichier de traduction française
msgid ""  
msgstr ""  
"Project-Id-Version: MonApp 1.0\n"
"Language: fr\n"
"MIME-Version: 1.0\n"
"Content-Type: text/plain; charset=UTF-8\n"

#: MainForm.pas:45
msgid "Hello World"  
msgstr "Bonjour le monde"  

#: MainForm.pas:46
msgid "Click me"  
msgstr "Cliquez-moi"  

#: MainForm.pas:50
msgid "Welcome to the application"  
msgstr "Bienvenue dans l'application"  
```

## Formats de fichiers de traduction

### Formats courants

| Format | Extension | Usage | Outils |
|--------|-----------|-------|--------|
| **XLIFF** | .xlf, .xliff | Standard XML pour traduction | Sisulizer, SDL |
| **PO/POT** | .po, .pot | GNU gettext | Poedit, Lokalize |
| **RESX** | .resx | Ressources .NET | Visual Studio |
| **TMX** | .tmx | Mémoire de traduction | Tous |
| **JSON** | .json | Format moderne, léger | Tous |
| **CSV** | .csv | Tableur simple | Excel, LibreOffice |

### XLIFF (XML Localization Interchange File Format)

XLIFF est le standard industriel pour l'échange de fichiers de traduction.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<xliff version="1.2">
  <file source-language="fr-FR" target-language="en-US" datatype="plaintext">
    <body>
      <trans-unit id="1">
        <source>Bonjour le monde</source>
        <target>Hello World</target>
      </trans-unit>
      <trans-unit id="2">
        <source>Cliquez pour continuer</source>
        <target>Click to continue</target>
      </trans-unit>
    </body>
  </file>
</xliff>
```

### JSON pour traduction

Format moderne et lisible, facile à gérer :

```json
{
  "app": {
    "title": "Mon Application",
    "welcome": "Bienvenue"
  },
  "buttons": {
    "ok": "OK",
    "cancel": "Annuler",
    "save": "Enregistrer"
  },
  "messages": {
    "success": "Opération réussie",
    "error": "Une erreur est survenue"
  }
}
```

### CSV pour traduction

Format simple pour les traducteurs non techniques :

```csv
ID,Contexte,Français,Anglais,Espagnol,Allemand  
BTN_OK,Bouton,OK,OK,Aceptar,OK  
BTN_CANCEL,Bouton,Annuler,Cancel,Cancelar,Abbrechen  
MSG_WELCOME,Message,Bienvenue,Welcome,Bienvenido,Willkommen  
LBL_NAME,Libellé,Nom,Name,Nombre,Name  
```

## Création d'un système d'exportation/importation

### Exportateur de ressources

```pascal
unit ExportateurTraduction;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.Generics.Collections;

type
  TFormatExport = (feJSON, feCSV, feXLIFF, fePO);

  TExportateurTraduction = class
  private
    FLangueSource: string;
    FLanguesCibles: TArray<string>;
  public
    constructor Create(const LangueSource: string);

    procedure ExporterVersFichier(const CheminFichier: string; Format: TFormatExport);
    procedure ImporterDepuisFichier(const CheminFichier: string; Format: TFormatExport);

    // Méthodes spécifiques par format
    procedure ExporterJSON(const CheminFichier: string);
    procedure ExporterCSV(const CheminFichier: string);
    procedure ExporterXLIFF(const CheminFichier: string);

    property LanguesCibles: TArray<string> read FLanguesCibles write FLanguesCibles;
  end;

implementation

uses
  System.JSON, System.IOUtils;

constructor TExportateurTraduction.Create(const LangueSource: string);  
begin  
  inherited Create;
  FLangueSource := LangueSource;
end;

procedure TExportateurTraduction.ExporterJSON(const CheminFichier: string);  
var  
  JSONRoot, JSONSection: TJSONObject;
  IniFile: TIniFile;
  Sections, Cles: TStringList;
  i, j: Integer;
  Cle, Valeur: string;
begin
  JSONRoot := TJSONObject.Create;
  Sections := TStringList.Create;
  Cles := TStringList.Create;
  try
    // Charger depuis le fichier INI source
    IniFile := TIniFile.Create(Format('Lang\%s.ini', [FLangueSource]));
    try
      IniFile.ReadSections(Sections);

      for i := 0 to Sections.Count - 1 do
      begin
        JSONSection := TJSONObject.Create;
        Cles.Clear;
        IniFile.ReadSection(Sections[i], Cles);

        for j := 0 to Cles.Count - 1 do
        begin
          Cle := Cles[j];
          Valeur := IniFile.ReadString(Sections[i], Cle, '');
          JSONSection.AddPair(Cle, Valeur);
        end;

        JSONRoot.AddPair(Sections[i], JSONSection);
      end;
    finally
      IniFile.Free;
    end;

    // Sauvegarder le JSON
    TFile.WriteAllText(CheminFichier, JSONRoot.ToJSON, TEncoding.UTF8);
  finally
    Cles.Free;
    Sections.Free;
    JSONRoot.Free;
  end;
end;

procedure TExportateurTraduction.ExporterCSV(const CheminFichier: string);  
var  
  CSV: TStringList;
  IniFile: TIniFile;
  Sections, Cles: TStringList;
  i, j, k: Integer;
  Ligne: string;
  IniCible: TIniFile;
  ValeurSource, ValeurCible: string;
begin
  CSV := TStringList.Create;
  Sections := TStringList.Create;
  Cles := TStringList.Create;
  try
    // En-tête
    Ligne := 'ID;Contexte;' + FLangueSource;
    for i := 0 to High(FLanguesCibles) do
      Ligne := Ligne + ';' + FLanguesCibles[i];
    CSV.Add(Ligne);

    // Charger les données source
    IniFile := TIniFile.Create(Format('Lang\%s.ini', [FLangueSource]));
    try
      IniFile.ReadSections(Sections);

      for i := 0 to Sections.Count - 1 do
      begin
        Cles.Clear;
        IniFile.ReadSection(Sections[i], Cles);

        for j := 0 to Cles.Count - 1 do
        begin
          ValeurSource := IniFile.ReadString(Sections[i], Cles[j], '');
          Ligne := Format('%s.%s;%s;%s',
            [Sections[i], Cles[j], Sections[i], ValeurSource]);

          // Ajouter les traductions existantes
          for k := 0 to High(FLanguesCibles) do
          begin
            if FileExists(Format('Lang\%s.ini', [FLanguesCibles[k]])) then
            begin
              IniCible := TIniFile.Create(Format('Lang\%s.ini', [FLanguesCibles[k]]));
              try
                ValeurCible := IniCible.ReadString(Sections[i], Cles[j], '');
                Ligne := Ligne + ';' + ValeurCible;
              finally
                IniCible.Free;
              end;
            end
            else
              Ligne := Ligne + ';';
          end;

          CSV.Add(Ligne);
        end;
      end;
    finally
      IniFile.Free;
    end;

    // Sauvegarder le CSV
    CSV.SaveToFile(CheminFichier, TEncoding.UTF8);
  finally
    Cles.Free;
    Sections.Free;
    CSV.Free;
  end;
end;

procedure TExportateurTraduction.ExporterXLIFF(const CheminFichier: string);  
var  
  XML: TStringList;
  IniFile: TIniFile;
  Sections, Cles: TStringList;
  i, j: Integer;
  ID: Integer;
  Cle, Valeur: string;
begin
  XML := TStringList.Create;
  Sections := TStringList.Create;
  Cles := TStringList.Create;
  try
    // En-tête XLIFF
    XML.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XML.Add('<xliff version="1.2" xmlns="urn:oasis:names:tc:xliff:document:1.2">');
    XML.Add(Format('  <file source-language="%s" datatype="plaintext">', [FLangueSource]));
    XML.Add('    <body>');

    // Charger les données
    IniFile := TIniFile.Create(Format('Lang\%s.ini', [FLangueSource]));
    try
      IniFile.ReadSections(Sections);
      ID := 1;

      for i := 0 to Sections.Count - 1 do
      begin
        Cles.Clear;
        IniFile.ReadSection(Sections[i], Cles);

        for j := 0 to Cles.Count - 1 do
        begin
          Cle := Sections[i] + '.' + Cles[j];
          Valeur := IniFile.ReadString(Sections[i], Cles[j], '');

          XML.Add(Format('      <trans-unit id="%d">', [ID]));
          XML.Add(Format('        <source>%s</source>', [Valeur]));
          XML.Add('        <target></target>');
          XML.Add(Format('        <note>%s</note>', [Cle]));
          XML.Add('      </trans-unit>');

          Inc(ID);
        end;
      end;
    finally
      IniFile.Free;
    end;

    // Pied XLIFF
    XML.Add('    </body>');
    XML.Add('  </file>');
    XML.Add('</xliff>');

    // Sauvegarder
    XML.SaveToFile(CheminFichier, TEncoding.UTF8);
  finally
    Cles.Free;
    Sections.Free;
    XML.Free;
  end;
end;

procedure TExportateurTraduction.ExporterVersFichier(const CheminFichier: string;
  Format: TFormatExport);
begin
  case Format of
    feJSON: ExporterJSON(CheminFichier);
    feCSV: ExporterCSV(CheminFichier);
    feXLIFF: ExporterXLIFF(CheminFichier);
  end;
end;

procedure TExportateurTraduction.ImporterDepuisFichier(const CheminFichier: string;
  Format: TFormatExport);
begin
  // À implémenter selon le format
  case Format of
    feJSON: ; // ImporterJSON
    feCSV: ; // ImporterCSV
    feXLIFF: ; // ImporterXLIFF
  end;
end;

end.
```

## Flux de travail recommandé

### Workflow pour petits projets

```
1. Développeur : Développement en langue source
   ↓
2. Développeur : Extraction vers CSV
   ↓
3. Traducteur : Traduction dans Excel/LibreOffice
   ↓
4. Développeur : Import du CSV
   ↓
5. Développeur : Tests
```

### Workflow pour projets moyens

```
1. Développeur : Développement avec ResourceString
   ↓
2. Développeur : Export vers fichiers PO avec gettext
   ↓
3. Chef de projet : Distribution aux traducteurs
   ↓
4. Traducteurs : Traduction avec Poedit
   ↓
5. Réviseur : Validation des traductions
   ↓
6. Développeur : Import et compilation
   ↓
7. Testeurs : Tests dans chaque langue
   ↓
8. Équipe : Corrections si nécessaire
```

### Workflow pour grands projets

```
1. Équipe dev : Développement + Extraction automatique
   ↓
2. PM : Création de packages de traduction (XLIFF)
   ↓
3. Plateforme de traduction : Distribution (Crowdin, Lokalise)
   ↓
4. Traducteurs : Traduction en ligne avec mémoire
   ↓
5. Réviseurs : Validation et QA
   ↓
6. CI/CD : Import et build automatique
   ↓
7. QA : Tests automatisés + manuels
   ↓
8. Déploiement : Publication multi-langue
```

## Plateformes de traduction en ligne

### Services cloud de traduction

| Plateforme | Prix | Fonctionnalités | Best for |
|------------|------|----------------|----------|
| **Crowdin** | Gratuit/Payant | Collaboration, API, mémoire | Projets open source et entreprise |
| **Lokalise** | Payant | CI/CD, design tools | Équipes agiles |
| **Transifex** | Gratuit/Payant | Interface simple, TM | Projets communautaires |
| **Phrase** | Payant | Workflows avancés | Grandes entreprises |
| **POEditor** | Gratuit/Payant | Simple, abordable | Petites équipes |

### Intégration avec Crowdin

**1. Configuration du projet**

```yaml
# crowdin.yml
project_id: "123456"  
api_token: "votre_token_api"  
base_path: "."  

files:
  - source: /Lang/fr.ini
    translation: /Lang/%two_letters_code%.ini
```

**2. Script d'upload**

```pascal
// Appeler l'API Crowdin
procedure UploadVersCrowdin(const CheminFichier: string);  
var  
  Client: THTTPClient;
  Response: IHTTPResponse;
  FormData: TMultipartFormData;
begin
  Client := THTTPClient.Create;
  FormData := TMultipartFormData.Create;
  try
    FormData.AddFile('files[fr.ini]', CheminFichier);

    Response := Client.Post(
      'https://api.crowdin.com/api/v2/projects/123456/files',
      FormData
    );

    if Response.StatusCode = 200 then
      ShowMessage('Upload réussi')
    else
      ShowMessage('Erreur: ' + Response.StatusCode.ToString);
  finally
    FormData.Free;
    Client.Free;
  end;
end;
```

## Mémoire de traduction

### Qu'est-ce qu'une mémoire de traduction ?

Une mémoire de traduction (Translation Memory - TM) est une base de données qui stocke les segments traduits pour réutilisation.

**Avantages :**
- ✅ Cohérence des traductions
- ✅ Gain de temps (réutilisation)
- ✅ Réduction des coûts
- ✅ Qualité améliorée

### Format TMX (Translation Memory eXchange)

```xml
<?xml version="1.0" encoding="UTF-8"?>
<tmx version="1.4">
  <header creationtool="MonApp" creationtoolversion="1.0"
          srclang="fr-FR" datatype="plaintext"/>
  <body>
    <tu>
      <tuv xml:lang="fr-FR">
        <seg>Bienvenue dans l'application</seg>
      </tuv>
      <tuv xml:lang="en-US">
        <seg>Welcome to the application</seg>
      </tuv>
      <tuv xml:lang="es-ES">
        <seg>Bienvenido a la aplicación</seg>
      </tuv>
    </tu>
  </body>
</tmx>
```

### Gestionnaire de mémoire de traduction simple

```pascal
unit MemoireTraduction;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections;

type
  TSegmentTraduction = record
    Source: string;
    Cible: string;
    Langue: string;
  end;

  TMemoireTraduction = class
  private
    FSegments: TDictionary<string, TList<TSegmentTraduction>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Ajouter(const TexteSource: string; const TexteCible: string;
      const Langue: string);
    function Rechercher(const TexteSource: string; const Langue: string): string;
    function RechercherSimilaire(const TexteSource: string; const Langue: string;
      SeuilSimilarite: Integer = 80): TArray<TSegmentTraduction>;

    procedure ChargerDepuisTMX(const CheminFichier: string);
    procedure SauvegarderVersTMX(const CheminFichier: string);

    function NombreSegments: Integer;
  end;

implementation

uses
  System.Math;

constructor TMemoireTraduction.Create;  
begin  
  inherited;
  FSegments := TDictionary<string, TList<TSegmentTraduction>>.Create;
end;

destructor TMemoireTraduction.Destroy;  
var  
  Liste: TList<TSegmentTraduction>;
begin
  for Liste in FSegments.Values do
    Liste.Free;
  FSegments.Free;
  inherited;
end;

procedure TMemoireTraduction.Ajouter(const TexteSource, TexteCible, Langue: string);  
var  
  Segment: TSegmentTraduction;
  Liste: TList<TSegmentTraduction>;
begin
  Segment.Source := TexteSource;
  Segment.Cible := TexteCible;
  Segment.Langue := Langue;

  if not FSegments.TryGetValue(TexteSource, Liste) then
  begin
    Liste := TList<TSegmentTraduction>.Create;
    FSegments.Add(TexteSource, Liste);
  end;

  Liste.Add(Segment);
end;

function TMemoireTraduction.Rechercher(const TexteSource, Langue: string): string;  
var  
  Liste: TList<TSegmentTraduction>;
  Segment: TSegmentTraduction;
begin
  Result := '';

  if FSegments.TryGetValue(TexteSource, Liste) then
  begin
    for Segment in Liste do
    begin
      if Segment.Langue = Langue then
        Exit(Segment.Cible);
    end;
  end;
end;

function CalculerSimilarite(const S1, S2: string): Integer;  
var  
  Distance: Integer;
  MaxLen: Integer;
begin
  // Algorithme simple de distance de Levenshtein simplifiée
  // Dans un vrai système, utiliser un algorithme plus sophistiqué
  Distance := Abs(Length(S1) - Length(S2));
  MaxLen := Max(Length(S1), Length(S2));

  if MaxLen = 0 then
    Result := 100
  else
    Result := 100 - (Distance * 100 div MaxLen);
end;

function TMemoireTraduction.RechercherSimilaire(const TexteSource, Langue: string;
  SeuilSimilarite: Integer): TArray<TSegmentTraduction>;
var
  Resultats: TList<TSegmentTraduction>;
  Cle: string;
  Liste: TList<TSegmentTraduction>;
  Segment: TSegmentTraduction;
  Similarite: Integer;
begin
  Resultats := TList<TSegmentTraduction>.Create;
  try
    for Cle in FSegments.Keys do
    begin
      Similarite := CalculerSimilarite(TexteSource, Cle);

      if Similarite >= SeuilSimilarite then
      begin
        Liste := FSegments[Cle];
        for Segment in Liste do
        begin
          if Segment.Langue = Langue then
            Resultats.Add(Segment);
        end;
      end;
    end;

    Result := Resultats.ToArray;
  finally
    Resultats.Free;
  end;
end;

function TMemoireTraduction.NombreSegments: Integer;  
var  
  Liste: TList<TSegmentTraduction>;
begin
  Result := 0;
  for Liste in FSegments.Values do
    Inc(Result, Liste.Count);
end;

procedure TMemoireTraduction.ChargerDepuisTMX(const CheminFichier: string);  
begin  
  // À implémenter : parser le fichier TMX
end;

procedure TMemoireTraduction.SauvegarderVersTMX(const CheminFichier: string);  
begin  
  // À implémenter : générer le fichier TMX
end;

end.
```

## API de traduction automatique

### Services de traduction automatique

| Service | Prix | Qualité | API | Limites |
|---------|------|---------|-----|---------|
| **Google Translate** | Payant | Très bonne | ✅ | Quota |
| **DeepL** | Gratuit/Payant | Excellente | ✅ | 500k caractères/mois (gratuit) |
| **Microsoft Translator** | Payant | Bonne | ✅ | Quota |
| **Amazon Translate** | Payant | Bonne | ✅ | Quota |
| **LibreTranslate** | Gratuit | Moyenne | ✅ | Auto-hébergé |

### Utilisation de DeepL API

```pascal
uses
  System.Net.HttpClient, System.JSON;

function TraduireAvecDeepL(const Texte: string; LangueSource, LangueCible: string): string;  
var  
  Client: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
  JSON: TJSONObject;
  Traductions: TJSONArray;
const
  API_KEY = 'votre_cle_api_deepl';
begin
  Client := THTTPClient.Create;
  try
    URL := Format('https://api-free.deepl.com/v2/translate?auth_key=%s&text=%s&source_lang=%s&target_lang=%s',
      [API_KEY, TNetEncoding.URL.Encode(Texte), LangueSource, LangueCible]);

    Response := Client.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JSON := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Traductions := JSON.GetValue<TJSONArray>('translations');
        Result := Traductions.Items[0].GetValue<string>('text');
      finally
        JSON.Free;
      end;
    end
    else
      raise Exception.Create('Erreur API DeepL: ' + Response.StatusCode.ToString);
  finally
    Client.Free;
  end;
end;

// Utilisation
procedure TForm1.BtnTraduireClick(Sender: TObject);  
var  
  TexteTraduit: string;
begin
  try
    TexteTraduit := TraduireAvecDeepL(EditSource.Text, 'FR', 'EN');
    EditCible.Text := TexteTraduit;
  except
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
end;
```

> ⚠️ **Important** : La traduction automatique ne remplace PAS un traducteur humain. Utilisez-la pour :
> - Traductions provisoires
> - Suggestions aux traducteurs
> - Contenu non critique

## Automatisation et scripts

### Script de génération automatique

```pascal
program GenerateurTraductions;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Classes,
  ExportateurTraduction in 'ExportateurTraduction.pas';

procedure GenererFichiersTraduction;  
var  
  Exportateur: TExportateurTraduction;
  Langues: TArray<string>;
begin
  WriteLn('=== Générateur de fichiers de traduction ===');
  WriteLn;

  Exportateur := TExportateurTraduction.Create('fr');
  try
    Langues := ['en', 'es', 'de', 'it'];
    Exportateur.LanguesCibles := Langues;

    WriteLn('Export JSON...');
    Exportateur.ExporterJSON('Export\traductions.json');

    WriteLn('Export CSV...');
    Exportateur.ExporterCSV('Export\traductions.csv');

    WriteLn('Export XLIFF...');
    Exportateur.ExporterXLIFF('Export\traductions.xlf');

    WriteLn;
    WriteLn('Export terminé avec succès!');
  finally
    Exportateur.Free;
  end;
end;

begin
  try
    GenererFichiersTraduction;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ' + E.Message);
      ExitCode := 1;
    end;
  end;

  WriteLn;
  WriteLn('Appuyez sur Entrée pour quitter...');
  ReadLn;
end.
```

### Intégration CI/CD

**Script pour intégration continue (GitHub Actions) :**

```yaml
name: Translation Workflow

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  export-translations:
    runs-on: windows-latest
    steps:
      - uses: actions/checkout@v2

      - name: Setup Delphi
        uses: actions/setup-delphi@v1

      - name: Export translations
        run: |
          dcc32 GenerateurTraductions.dpr
          GenerateurTraductions.exe

      - name: Upload to Crowdin
        run: |
          crowdin upload sources

      - name: Archive exports
        uses: actions/upload-artifact@v2
        with:
          name: translation-files
          path: Export/
```

## Bonnes pratiques

### Recommandations pour la traduction

| Pratique | Description | Priorité |
|----------|-------------|----------|
| **Contexte** | Fournir du contexte aux traducteurs | ⭐⭐⭐ |
| **Glossaire** | Maintenir un glossaire de termes | ⭐⭐⭐ |
| **Screenshots** | Inclure des captures d'écran | ⭐⭐⭐ |
| **Mémoire TM** | Utiliser une mémoire de traduction | ⭐⭐⭐ |
| **Révision** | Faire réviser par un natif | ⭐⭐⭐ |
| **Longueur max** | Indiquer les limites de caractères | ⭐⭐ |
| **Format** | Expliquer les variables (%s, %d) | ⭐⭐ |
| **Testing** | Tester chaque langue | ⭐⭐⭐ |

### Checklist du processus

```
Préparation:
  □ Code source finalisé
  □ Extraction des chaînes complète
  □ Glossaire créé
  □ Captures d'écran préparées
  □ Contexte documenté

Distribution:
  □ Fichiers exportés au bon format
  □ Instructions pour les traducteurs
  □ Deadline communiquée
  □ Point de contact désigné

Traduction:
  □ Traducteurs qualifiés
  □ Outils appropriés fournis
  □ Questions répondues rapidement
  □ Progression suivie

Révision:
  □ Vérification par réviseur natif
  □ Cohérence terminologique
  □ Respect des longueurs
  □ Qualité linguistique

Intégration:
  □ Import dans l'application
  □ Compilation réussie
  □ Tests fonctionnels
  □ Tests visuels

Validation:
  □ Tous les textes traduits
  □ Pas d'erreurs d'affichage
  □ Formats corrects
  □ Validation utilisateur final
```

## Documentation pour traducteurs

### Package de traduction type

Un package complet pour traducteurs devrait contenir :

```
📦 Package_Traduction_v1.0/
├── 📄 README.md (Instructions)
├── 📄 GLOSSAIRE.md (Termes techniques)
├── 📁 Fichiers/
│   ├── 📄 traductions.csv (À traduire)
│   └── 📄 traductions_exemple.csv (Exemple)
├── 📁 Screenshots/
│   ├── 🖼 ecran_principal.png
│   ├── 🖼 dialogue_options.png
│   └── 🖼 formulaire_saisie.png
└── 📁 Contexte/
    └── 📄 notes_contexte.pdf
```

### Exemple de README pour traducteurs

```markdown
# Guide de traduction - MonApplication v1.0

## Instructions générales

1. Ouvrez le fichier `traductions.csv` avec Excel ou LibreOffice
2. Traduisez uniquement la colonne correspondant à votre langue
3. Ne modifiez PAS la colonne "ID" ni "Contexte"
4. Sauvegardez au format CSV UTF-8

## Règles de traduction

### Variables
- `%s` : Sera remplacé par du texte
- `%d` : Sera remplacé par un nombre
- Exemple: "Vous avez %d messages" → "You have %d messages"

### Longueur
- Respectez les limites indiquées dans "Contexte"
- Si trop long, trouvez une formulation plus courte

### Ton
- Tutoiement en français, vous en contexte formel
- Adapter selon les conventions de votre langue

### Termes techniques
- Consultez le GLOSSAIRE.md
- En cas de doute, demandez!

## Contact
- Email: traduction@monapp.com
- Deadline: 15/06/2024
```

## Conclusion

La gestion efficace des traductions nécessite les bons outils, un workflow bien défini et une bonne collaboration entre développeurs et traducteurs. En choisissant les outils adaptés à la taille de votre projet et en suivant les bonnes pratiques, vous pouvez créer et maintenir des applications multilingues de qualité.

**Points clés à retenir :**

- **Outils** : Choisir selon la taille du projet (ITM → Sisulizer → Plateforme cloud)
- **Formats** : Privilégier XLIFF ou PO pour l'interopérabilité
- **Workflow** : Définir un processus clair et le documenter
- **Mémoire TM** : Utiliser pour cohérence et efficacité
- **Automatisation** : Intégrer dans le CI/CD
- **Contexte** : Toujours fournir du contexte aux traducteurs
- **Révision** : Faire réviser par des locuteurs natifs
- **Tests** : Tester systématiquement chaque langue

Avec ces outils et méthodes, vous êtes équipé pour gérer professionnellement la traduction de vos applications Delphi vers de multiples langues.

⏭️ [Utilisation d'API et bibliothèques externes](/14-utilisation-dapi-et-bibliotheques-externes/README.md)
