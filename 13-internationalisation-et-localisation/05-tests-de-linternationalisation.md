🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.5 Tests de l'internationalisation

## Introduction

Tester l'internationalisation (i18n) d'une application est une étape cruciale souvent négligée. Une application peut fonctionner parfaitement dans sa langue d'origine, mais présenter de nombreux problèmes dans d'autres langues. Cette section vous guidera à travers les différentes méthodes et stratégies pour tester efficacement l'internationalisation de vos applications Delphi.

## Pourquoi tester l'internationalisation ?

Les problèmes d'internationalisation peuvent se manifester de nombreuses façons :

| Type de problème | Exemple | Impact |
|------------------|---------|--------|
| **Textes tronqués** | Bouton trop petit pour "Enregistrer sous..." en allemand | Interface illisible |
| **Mauvais encodage** | Caractères spéciaux affichés incorrectement (é → Ã©) | Confusion utilisateur |
| **Format incorrect** | Date affichée en MM/DD/YYYY pour un utilisateur français | Erreur d'interprétation |
| **Textes non traduits** | Messages en anglais dans une interface française | Expérience incohérente |
| **Débordement de mise en page** | Texte qui sort du cadre prévu | Interface cassée |
| **Caractères manquants** | Police ne supportant pas les caractères arabes ou chinois | Contenu illisible |

## Types de tests d'internationalisation

### 1. Tests visuels (manuels)

Les tests visuels consistent à vérifier l'apparence de l'interface dans chaque langue.

#### Checklist de test visuel

```pascal
type
  TTestVisuel = class
  private
    FLangueTeste: string;
    FProblemesTrouves: TStringList;
  public
    constructor Create(const Langue: string);
    destructor Destroy; override;

    procedure VerifierFormulaire(Form: TForm);
    procedure AjouterProbleme(const Description: string);
    function GenererRapport: string;
  end;

constructor TTestVisuel.Create(const Langue: string);
begin
  inherited Create;
  FLangueTeste := Langue;
  FProblemesTrouves := TStringList.Create;
end;

destructor TTestVisuel.Destroy;
begin
  FProblemesTrouves.Free;
  inherited;
end;

procedure TTestVisuel.VerifierFormulaire(Form: TForm);
var
  i: Integer;
  Composant: TComponent;
begin
  // Vérifier chaque composant du formulaire
  for i := 0 to Form.ComponentCount - 1 do
  begin
    Composant := Form.Components[i];

    // Vérifier les boutons
    if Composant is TButton then
      VerifierBouton(TButton(Composant));

    // Vérifier les labels
    if Composant is TLabel then
      VerifierLabel(TLabel(Composant));

    // Vérifier les menus
    if Composant is TMenuItem then
      VerifierMenuItem(TMenuItem(Composant));
  end;
end;

procedure TTestVisuel.AjouterProbleme(const Description: string);
begin
  FProblemesTrouves.Add(Format('[%s] %s', [FLangueTeste, Description]));
end;

function TTestVisuel.GenererRapport: string;
begin
  if FProblemesTrouves.Count = 0 then
    Result := Format('Aucun problème trouvé pour la langue %s', [FLangueTeste])
  else
    Result := Format('Problèmes trouvés pour %s :'#13#10'%s',
      [FLangueTeste, FProblemesTrouves.Text]);
end;
```

### 2. Tests fonctionnels

Vérifier que toutes les fonctionnalités marchent dans chaque langue.

#### Liste de vérification fonctionnelle

| Fonctionnalité | Points à vérifier |
|----------------|-------------------|
| **Saisie de données** | Les formats de date/nombre sont acceptés correctement |
| **Affichage** | Les données sont formatées selon la culture |
| **Validation** | Les messages d'erreur sont dans la bonne langue |
| **Navigation** | Les raccourcis clavier fonctionnent |
| **Export/Import** | Les fichiers conservent l'encodage correct |
| **Recherche** | La recherche fonctionne avec les caractères accentués |

### 3. Tests automatisés

Automatiser les tests permet de détecter rapidement les régressions.

```pascal
unit TestsInternationalisation;

interface

uses
  DUnitX.TestFramework, System.SysUtils, GestionnaireTraduction;

type
  [TestFixture]
  TTestsI18N = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestToutesLesLanguesDisponibles;
    [Test]
    procedure TestToutesLesClesTraduitesEnFrancais;
    [Test]
    procedure TestToutesLesClesTraduitesEnAnglais;
    [Test]
    procedure TestFormatDateFrancais;
    [Test]
    procedure TestFormatDateAmericain;
    [Test]
    procedure TestFormatNombreFrancais;
  end;

implementation

procedure TTestsI18N.Setup;
begin
  // Initialisation avant chaque test
end;

procedure TTestsI18N.TearDown;
begin
  // Nettoyage après chaque test
end;

procedure TTestsI18N.TestToutesLesLanguesDisponibles;
var
  Langues: TArray<string>;
begin
  Langues := ['fr', 'en', 'es', 'de', 'it'];

  // Vérifier que chaque fichier de langue existe
  for var Langue in Langues do
    Assert.IsTrue(FileExists('Lang\' + Langue + '.ini'),
      'Fichier de langue manquant : ' + Langue);
end;

procedure TTestsI18N.TestToutesLesClesTraduitesEnFrancais;
var
  ClesRequises: TArray<string>;
  Valeur: string;
begin
  GestionnaireTraduction.DefinirLangue('fr');

  // Liste des clés qui doivent exister
  ClesRequises := ['Boutons.Valider', 'Boutons.Annuler',
                   'Messages.Bienvenue', 'Libelles.Nom'];

  for var Cle in ClesRequises do
  begin
    Valeur := T(Cle);
    Assert.AreNotEqual(Cle, Valeur,
      'Traduction manquante pour : ' + Cle);
  end;
end;

procedure TTestsI18N.TestFormatDateFrancais;
var
  FormatFR: TFormatSettings;
  MaDate: TDateTime;
  DateStr: string;
begin
  FormatFR := TFormatSettings.Create('fr-FR');
  MaDate := EncodeDate(2024, 12, 25);

  DateStr := DateToStr(MaDate, FormatFR);
  Assert.AreEqual('25/12/2024', DateStr);
end;

procedure TTestsI18N.TestFormatNombreFrancais;
var
  FormatFR: TFormatSettings;
  Nombre: Double;
  NombreStr: string;
begin
  FormatFR := TFormatSettings.Create('fr-FR');
  Nombre := 1234.56;

  NombreStr := FormatFloat('#,##0.00', Nombre, FormatFR);
  Assert.IsTrue(Pos(',', NombreStr) > 0,
    'Le séparateur décimal devrait être une virgule');
end;

end.
```

## Stratégie de test par langue

### Phases de test

```
Phase 1: Test de la langue par défaut (français)
   ↓
Phase 2: Test d'une langue majeure (anglais)
   ↓
Phase 3: Test des autres langues européennes
   ↓
Phase 4: Test des langues avec caractères spéciaux (chinois, arabe)
   ↓
Phase 5: Test des langues RTL (arabe, hébreu)
```

### Priorisation des langues

| Priorité | Langues | Raison |
|----------|---------|--------|
| **Haute** | Langues cibles principales | Marché principal |
| **Moyenne** | Langues européennes courantes | Expansion internationale |
| **Basse** | Langues spécialisées | Marchés de niche |

## Outils de test

### 1. Outil de changement rapide de langue

Créez un outil pour tester rapidement toutes les langues :

```pascal
unit OutilTestLangue;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls,
  Vcl.ExtCtrls, GestionnaireTraduction;

type
  TFormTestLangue = class(TForm)
    PanelHaut: TPanel;
    ComboLangue: TComboBox;
    BtnAppliquer: TButton;
    BtnCaptureEcran: TButton;
    BtnSuivant: TButton;
    MemoRapport: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure BtnAppliquerClick(Sender: TObject);
    procedure BtnCaptureEcranClick(Sender: TObject);
    procedure BtnSuivantClick(Sender: TObject);
  private
    FIndexLangue: Integer;
    FLangues: TArray<string>;
    procedure ChargerLangues;
    procedure AppliquerLangue(const CodeLangue: string);
    procedure CaptureEcranFormulaire(Form: TForm);
    procedure TestLangueSuivante;
  end;

implementation

uses
  Vcl.Imaging.pngimage, Vcl.Graphics;

{$R *.dfm}

procedure TFormTestLangue.FormCreate(Sender: TObject);
begin
  ChargerLangues;
  FIndexLangue := 0;
end;

procedure TFormTestLangue.ChargerLangues;
begin
  FLangues := ['fr', 'en', 'es', 'de', 'it'];
  ComboLangue.Items.Clear;
  ComboLangue.Items.AddStrings(['Français', 'English', 'Español', 'Deutsch', 'Italiano']);
  ComboLangue.ItemIndex := 0;
end;

procedure TFormTestLangue.BtnAppliquerClick(Sender: TObject);
begin
  if ComboLangue.ItemIndex >= 0 then
    AppliquerLangue(FLangues[ComboLangue.ItemIndex]);
end;

procedure TFormTestLangue.AppliquerLangue(const CodeLangue: string);
begin
  GestionnaireTraduction.DefinirLangue(CodeLangue);

  // Recharger tous les formulaires ouverts
  for var i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] <> Self then
      (Screen.Forms[i] as TForm).Perform(WM_LANGUAGECHANGE, 0, 0);
  end;

  MemoRapport.Lines.Add(Format('Langue changée vers : %s', [CodeLangue]));
end;

procedure TFormTestLangue.BtnCaptureEcranClick(Sender: TObject);
var
  i: Integer;
begin
  // Capturer tous les formulaires
  for i := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[i] <> Self then
      CaptureEcranFormulaire(Screen.Forms[i]);
  end;

  MemoRapport.Lines.Add('Captures d''écran enregistrées');
end;

procedure TFormTestLangue.CaptureEcranFormulaire(Form: TForm);
var
  Bitmap: TBitmap;
  PNG: TPngImage;
  NomFichier: string;
begin
  Bitmap := TBitmap.Create;
  PNG := TPngImage.Create;
  try
    // Capturer le formulaire
    Bitmap.Width := Form.ClientWidth;
    Bitmap.Height := Form.ClientHeight;
    Form.PaintTo(Bitmap.Canvas, 0, 0);

    // Convertir en PNG et sauvegarder
    PNG.Assign(Bitmap);
    NomFichier := Format('Screenshots\%s_%s.png',
      [Form.Name, FLangues[ComboLangue.ItemIndex]]);
    PNG.SaveToFile(NomFichier);
  finally
    PNG.Free;
    Bitmap.Free;
  end;
end;

procedure TFormTestLangue.BtnSuivantClick(Sender: TObject);
begin
  TestLangueSuivante;
end;

procedure TFormTestLangue.TestLangueSuivante;
begin
  Inc(FIndexLangue);
  if FIndexLangue >= Length(FLangues) then
  begin
    ShowMessage('Test de toutes les langues terminé !');
    FIndexLangue := 0;
  end;

  ComboLangue.ItemIndex := FIndexLangue;
  AppliquerLangue(FLangues[FIndexLangue]);
end;

end.
```

### 2. Validateur de traductions

Vérifier que toutes les clés sont traduites :

```pascal
unit ValidateurTraductions;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.IniFiles;

type
  TProblemeTraduction = record
    Langue: string;
    Cle: string;
    TypeProbleme: string; // 'Manquant', 'Vide', 'Identique'
  end;

  TValidateurTraductions = class
  private
    FLangueReference: string;
    FProblemes: TList<TProblemeTraduction>;
    function ChargerCles(const Langue: string): TStringList;
  public
    constructor Create(const LangueReference: string = 'fr');
    destructor Destroy; override;

    procedure ValiderLangue(const CodeLangue: string);
    procedure ValiderToutesLesLangues;
    function GenererRapport: string;
    property Problemes: TList<TProblemeTraduction> read FProblemes;
  end;

implementation

constructor TValidateurTraductions.Create(const LangueReference: string);
begin
  inherited Create;
  FLangueReference := LangueReference;
  FProblemes := TList<TProblemeTraduction>.Create;
end;

destructor TValidateurTraductions.Destroy;
begin
  FProblemes.Free;
  inherited;
end;

function TValidateurTraductions.ChargerCles(const Langue: string): TStringList;
var
  IniFile: TIniFile;
  Sections, Cles: TStringList;
  i, j: Integer;
  Cle, Valeur: string;
begin
  Result := TStringList.Create;
  IniFile := TIniFile.Create(Format('Lang\%s.ini', [Langue]));
  Sections := TStringList.Create;
  Cles := TStringList.Create;
  try
    IniFile.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      Cles.Clear;
      IniFile.ReadSection(Sections[i], Cles);

      for j := 0 to Cles.Count - 1 do
      begin
        Cle := Sections[i] + '.' + Cles[j];
        Valeur := IniFile.ReadString(Sections[i], Cles[j], '');
        Result.AddObject(Cle, TObject(Pointer(Valeur)));
      end;
    end;
  finally
    Cles.Free;
    Sections.Free;
    IniFile.Free;
  end;
end;

procedure TValidateurTraductions.ValiderLangue(const CodeLangue: string);
var
  ClesReference, ClesLangue: TStringList;
  i: Integer;
  Probleme: TProblemeTraduction;
  CleRef, ValeurLangue: string;
begin
  ClesReference := ChargerCles(FLangueReference);
  ClesLangue := ChargerCles(CodeLangue);
  try
    // Vérifier que toutes les clés de référence existent
    for i := 0 to ClesReference.Count - 1 do
    begin
      CleRef := ClesReference[i];

      // Clé manquante ?
      if ClesLangue.IndexOf(CleRef) < 0 then
      begin
        Probleme.Langue := CodeLangue;
        Probleme.Cle := CleRef;
        Probleme.TypeProbleme := 'Manquant';
        FProblemes.Add(Probleme);
      end
      else
      begin
        ValeurLangue := string(Pointer(ClesLangue.Objects[ClesLangue.IndexOf(CleRef)]));

        // Valeur vide ?
        if Trim(ValeurLangue) = '' then
        begin
          Probleme.Langue := CodeLangue;
          Probleme.Cle := CleRef;
          Probleme.TypeProbleme := 'Vide';
          FProblemes.Add(Probleme);
        end;
      end;
    end;
  finally
    ClesLangue.Free;
    ClesReference.Free;
  end;
end;

procedure TValidateurTraductions.ValiderToutesLesLangues;
var
  Langues: TArray<string>;
  Langue: string;
begin
  FProblemes.Clear;
  Langues := ['en', 'es', 'de', 'it']; // Toutes sauf la référence

  for Langue in Langues do
  begin
    if FileExists(Format('Lang\%s.ini', [Langue])) then
      ValiderLangue(Langue);
  end;
end;

function TValidateurTraductions.GenererRapport: string;
var
  Rapport: TStringBuilder;
  Probleme: TProblemeTraduction;
  DernierLangue: string;
begin
  Rapport := TStringBuilder.Create;
  try
    if FProblemes.Count = 0 then
    begin
      Rapport.AppendLine('✓ Aucun problème de traduction détecté');
    end
    else
    begin
      Rapport.AppendLine(Format('⚠ %d problème(s) de traduction détecté(s) :'#13#10,
        [FProblemes.Count]));

      DernierLangue := '';
      for Probleme in FProblemes do
      begin
        if Probleme.Langue <> DernierLangue then
        begin
          Rapport.AppendLine(#13#10'--- ' + Probleme.Langue + ' ---');
          DernierLangue := Probleme.Langue;
        end;

        Rapport.AppendLine(Format('  [%s] %s',
          [Probleme.TypeProbleme, Probleme.Cle]));
      end;
    end;

    Result := Rapport.ToString;
  finally
    Rapport.Free;
  end;
end;

end.
```

### 3. Analyseur de mise en page

Détecte les problèmes de débordement de texte :

```pascal
unit AnalyseurMiseEnPage;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls;

type
  TProblemeMiseEnPage = record
    NomComposant: string;
    TypeProbleme: string;
    Description: string;
  end;

  TAnalyseurMiseEnPage = class
  private
    FProblemes: TList<TProblemeMiseEnPage>;
    procedure VerifierDebordementBouton(Bouton: TButton);
    procedure VerifierDebordementLabel(Lbl: TLabel);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AnalyserFormulaire(Form: TForm);
    function GenererRapport: string;
    property Problemes: TList<TProblemeMiseEnPage> read FProblemes;
  end;

implementation

constructor TAnalyseurMiseEnPage.Create;
begin
  inherited;
  FProblemes := TList<TProblemeMiseEnPage>.Create;
end;

destructor TAnalyseurMiseEnPage.Destroy;
begin
  FProblemes.Free;
  inherited;
end;

procedure TAnalyseurMiseEnPage.AnalyserFormulaire(Form: TForm);
var
  i: Integer;
  Composant: TComponent;
begin
  FProblemes.Clear;

  for i := 0 to Form.ComponentCount - 1 do
  begin
    Composant := Form.Components[i];

    if Composant is TButton then
      VerifierDebordementBouton(TButton(Composant))
    else if Composant is TLabel then
      VerifierDebordementLabel(TLabel(Composant));
  end;
end;

procedure TAnalyseurMiseEnPage.VerifierDebordementBouton(Bouton: TButton);
var
  LargeurTexte: Integer;
  Probleme: TProblemeMiseEnPage;
begin
  // Calculer la largeur nécessaire pour le texte
  LargeurTexte := Bouton.Canvas.TextWidth(Bouton.Caption) + 20; // Marge

  if LargeurTexte > Bouton.Width then
  begin
    Probleme.NomComposant := Bouton.Name;
    Probleme.TypeProbleme := 'Débordement de texte';
    Probleme.Description := Format(
      'Le bouton "%s" est trop petit. Largeur actuelle : %d, Nécessaire : %d',
      [Bouton.Caption, Bouton.Width, LargeurTexte]);
    FProblemes.Add(Probleme);
  end;
end;

procedure TAnalyseurMiseEnPage.VerifierDebordementLabel(Lbl: TLabel);
var
  LargeurTexte: Integer;
  Probleme: TProblemeMiseEnPage;
begin
  if not Lbl.AutoSize then
  begin
    LargeurTexte := Lbl.Canvas.TextWidth(Lbl.Caption);

    if LargeurTexte > Lbl.Width then
    begin
      Probleme.NomComposant := Lbl.Name;
      Probleme.TypeProbleme := 'Débordement de texte';
      Probleme.Description := Format(
        'Le label "%s" est trop petit. Activez AutoSize ou agrandissez le composant.',
        [Lbl.Caption]);
      FProblemes.Add(Probleme);
    end;
  end;
end;

function TAnalyseurMiseEnPage.GenererRapport: string;
var
  Rapport: TStringBuilder;
  Probleme: TProblemeMiseEnPage;
begin
  Rapport := TStringBuilder.Create;
  try
    if FProblemes.Count = 0 then
    begin
      Rapport.AppendLine('✓ Aucun problème de mise en page détecté');
    end
    else
    begin
      Rapport.AppendLine(Format('⚠ %d problème(s) de mise en page :'#13#10,
        [FProblemes.Count]));

      for Probleme in FProblemes do
      begin
        Rapport.AppendLine(Format('[%s] %s',
          [Probleme.NomComposant, Probleme.Description]));
      end;
    end;

    Result := Rapport.ToString;
  finally
    Rapport.Free;
  end;
end;

end.
```

## Checklist complète de test

### Tests de base (toutes langues)

| Test | Description | Critique |
|------|-------------|----------|
| ✓ Textes affichés | Tous les textes sont visibles et lisibles | ⭐⭐⭐ |
| ✓ Pas de débordement | Aucun texte ne dépasse de son conteneur | ⭐⭐⭐ |
| ✓ Encodage correct | Les caractères spéciaux s'affichent bien | ⭐⭐⭐ |
| ✓ Traductions complètes | Aucun texte dans la mauvaise langue | ⭐⭐⭐ |
| ✓ Formats de date | Les dates utilisent le format local | ⭐⭐ |
| ✓ Formats de nombre | Les nombres utilisent les bons séparateurs | ⭐⭐ |
| ✓ Symboles monétaires | La devise est correcte | ⭐⭐ |
| ✓ Raccourcis clavier | Les raccourcis sont cohérents | ⭐ |

### Tests spécifiques par type de langue

#### Langues européennes (FR, EN, ES, DE, IT)

```
□ Accents et caractères spéciaux (é, ñ, ü, etc.)
□ Guillemets typographiques (« », ", ")
□ Format de date approprié
□ Séparateurs de nombres corrects
□ Longueur des textes (allemand = textes longs)
□ Symboles monétaires (€, £, etc.)
```

#### Langues asiatiques (ZH, JA, KO)

```
□ Police supportant les caractères CJK
□ Largeur des caractères (caractères doubles)
□ Césure des mots (pas d'espaces en chinois/japonais)
□ Format de date asiatique (YYYY/MM/DD)
□ Taille de police appropriée
```

#### Langues RTL (AR, HE)

```
□ Interface miroir (droite à gauche)
□ Alignement du texte correct
□ Position des icônes inversée
□ Barres de défilement à gauche
□ Ordre de lecture des colonnes
□ Navigation au clavier inversée
```

## Scénarios de test

### Scénario 1 : Changement de langue dynamique

```pascal
procedure TesterChangementLangue;
var
  Form: TForm;
  LanguesATest: TArray<string>;
  Langue: string;
begin
  Form := TFormPrincipal.Create(nil);
  try
    LanguesATest := ['fr', 'en', 'es', 'de'];

    for Langue in LanguesATest do
    begin
      // Changer la langue
      GestionnaireTraduction.DefinirLangue(Langue);
      Form.AppliquerTraductions;

      // Vérifier l'affichage
      VerifierAffichage(Form, Langue);

      // Capturer une image
      CaptureEcran(Form, Langue);

      // Attendre un peu pour visualiser
      Sleep(1000);
      Application.ProcessMessages;
    end;
  finally
    Form.Free;
  end;
end;
```

### Scénario 2 : Saisie et validation

```pascal
procedure TesterSaisieValidation(const Langue: string);
var
  Form: TFormSaisie;
  DonneesTest: array of record
    DateStr: string;
    NombreStr: string;
    Valide: Boolean;
  end;
begin
  GestionnaireTraduction.DefinirLangue(Langue);
  Form := TFormSaisie.Create(nil);
  try
    // Définir les données de test selon la langue
    if Langue = 'fr' then
    begin
      SetLength(DonneesTest, 3);
      DonneesTest[0].DateStr := '25/12/2024';
      DonneesTest[0].NombreStr := '1234,56';
      DonneesTest[0].Valide := True;

      DonneesTest[1].DateStr := '12/25/2024'; // Format US - invalide en FR
      DonneesTest[1].NombreStr := '1234.56';  // Point - invalide en FR
      DonneesTest[1].Valide := False;
    end
    else if Langue = 'en' then
    begin
      SetLength(DonneesTest, 3);
      DonneesTest[0].DateStr := '12/25/2024';
      DonneesTest[0].NombreStr := '1234.56';
      DonneesTest[0].Valide := True;

      DonneesTest[1].DateStr := '25/12/2024'; // Format FR - invalide en US
      DonneesTest[1].NombreStr := '1234,56';  // Virgule - invalide en US
      DonneesTest[1].Valide := False;
    end;

    // Tester chaque cas
    for var i := 0 to High(DonneesTest) do
    begin
      Form.EditDate.Text := DonneesTest[i].DateStr;
      Form.EditNombre.Text := DonneesTest[i].NombreStr;

      var Resultat := Form.ValiderSaisie;

      if Resultat = DonneesTest[i].Valide then
        WriteLn(Format('[OK] Test %d réussi', [i]))
      else
        WriteLn(Format('[ERREUR] Test %d échoué', [i]));
    end;
  finally
    Form.Free;
  end;
end;
```

### Scénario 3 : Import/Export de données

```pascal
procedure TesterImportExport(const Langue: string);
var
  DonneesOriginales: TDataSet;
  CheminFichier: string;
begin
  GestionnaireTraduction.DefinirLangue(Langue);

  // Créer des données de test
  DonneesOriginales := CreerDonneesTest;
  try
    // Exporter
    CheminFichier := Format('Export_%s.csv', [Langue]);
    ExporterVersCsv(DonneesOriginales, CheminFichier);

    // Réimporter
    var DonneesReimportees := ImporterDepuisCsv(CheminFichier);
    try
      // Vérifier que les données sont identiques
      if ComparerDataSets(DonneesOriginales, DonneesReimportees) then
        WriteLn('[OK] Import/Export réussi pour ' + Langue)
      else
        WriteLn('[ERREUR] Données corrompues pour ' + Langue);
    finally
      DonneesReimportees.Free;
    end;
  finally
    DonneesOriginales.Free;
  end;
end;
```

## Détection des problèmes courants

### 1. Textes codés en dur

Créer un outil pour détecter les chaînes en dur dans le code :

```pascal
procedure RechercherTextesEnDur(const CheminSource: string);
var
  Fichiers: TStringDynArray;
  Fichier, Ligne: string;
  NumLigne: Integer;
  Lignes: TStringList;
begin
  // Trouver tous les fichiers .pas
  Fichiers := TDirectory.GetFiles(CheminSource, '*.pas', TSearchOption.soAllDirectories);

  Lignes := TStringList.Create;
  try
    for Fichier in Fichiers do
    begin
      Lignes.LoadFromFile(Fichier);

      for NumLigne := 0 to Lignes.Count - 1 do
      begin
        Ligne := Lignes[NumLigne];

        // Chercher des assignations de Caption, Text, Hint
        if ContientTexteEnDur(Ligne) then
        begin
          WriteLn(Format('%s (ligne %d): %s',
            [ExtractFileName(Fichier), NumLigne + 1, Trim(Ligne)]));
        end;
      end;
    end;
  finally
    Lignes.Free;
  end;
end;

function ContientTexteEnDur(const Ligne: string): Boolean;
var
  Motifs: TArray<string>;
  Motif: string;
begin
  Result := False;

  // Motifs à rechercher
  Motifs := ['.Caption := ''', '.Text := ''', '.Hint := ''',
             'ShowMessage(''', 'MessageDlg('''];

  for Motif in Motifs do
  begin
    if Pos(Motif, Ligne) > 0 then
    begin
      // Vérifier que ce n'est pas dans un commentaire
      if Pos('//', Ligne) = 0 then
        Exit(True);
    end;
  end;
end;
```

### 2. Traductions manquantes

```pascal
procedure DetecterTraductionsManquantes;
var
  ValidateurTrad: TValidateurTraductions;
  Rapport: string;
begin
  ValidateurTrad := TValidateurTraductions.Create('fr');
  try
    ValidateurTrad.ValiderToutesLesLangues;
    Rapport := ValidateurTrad.GenererRapport;

    WriteLn(Rapport);

    // Sauvegarder le rapport
    TFile.WriteAllText('RapportTraductions.txt', Rapport);
  finally
    ValidateurTrad.Free;
  end;
end;
```

### 3. Problèmes d'encodage

```pascal
procedure VerifierEncodageFichiers;
var
  FichiersLangue: TStringDynArray;
  Fichier: string;
  Contenu: TBytes;
begin
  FichiersLangue := TDirectory.GetFiles('Lang', '*.ini');

  for Fichier in FichiersLangue do
  begin
    Contenu := TFile.ReadAllBytes(Fichier);

    // Vérifier la présence du BOM UTF-8
    if (Length(Contenu) < 3) or
       (Contenu[0] <> $EF) or
       (Contenu[1] <> $BB) or
       (Contenu[2] <> $BF) then
    begin
      WriteLn('[ATTENTION] Fichier sans BOM UTF-8 : ' + Fichier);
    end;
  end;
end;
```

## Rapport de test

### Structure d'un rapport de test

```pascal
type
  TRapportTest = class
  private
    FLangueTeste: string;
    FDateTest: TDateTime;
    FTestsReussis: Integer;
    FTestsEchoues: Integer;
    FProblemesTrouves: TStringList;
  public
    constructor Create(const Langue: string);
    destructor Destroy; override;

    procedure AjouterReussite(const Description: string);
    procedure AjouterEchec(const Description: string);
    procedure AjouterProbleme(const Categorie, Description: string);
    function GenererRapportHTML: string;
    procedure SauvegarderRapport(const CheminFichier: string);
  end;

function TRapportTest.GenererRapportHTML: string;
var
  HTML: TStringBuilder;
begin
  HTML := TStringBuilder.Create;
  try
    HTML.AppendLine('<!DOCTYPE html>');
    HTML.AppendLine('<html>');
    HTML.AppendLine('<head>');
    HTML.AppendLine('  <meta charset="UTF-8">');
    HTML.AppendLine(Format('  <title>Rapport de test i18n - %s</title>', [FLangueTeste]));
    HTML.AppendLine('  <style>');
    HTML.AppendLine('    body { font-family: Arial, sans-serif; margin: 20px; }');
    HTML.AppendLine('    .success { color: green; }');
    HTML.AppendLine('    .error { color: red; }');
    HTML.AppendLine('    .warning { color: orange; }');
    HTML.AppendLine('  </style>');
    HTML.AppendLine('</head>');
    HTML.AppendLine('<body>');

    HTML.AppendLine(Format('<h1>Rapport de test - %s</h1>', [FLangueTeste]));
    HTML.AppendLine(Format('<p>Date : %s</p>',
      [FormatDateTime('dd/mm/yyyy hh:nn', FDateTest)]));

    HTML.AppendLine('<h2>Résumé</h2>');
    HTML.AppendLine(Format('<p class="success">Tests réussis : %d</p>', [FTestsReussis]));
    HTML.AppendLine(Format('<p class="error">Tests échoués : %d</p>', [FTestsEchoues]));

    if FProblemesTrouves.Count > 0 then
    begin
      HTML.AppendLine('<h2>Problèmes détectés</h2>');
      HTML.AppendLine('<ul>');
      for var i := 0 to FProblemesTrouves.Count - 1 do
        HTML.AppendLine(Format('<li class="warning">%s</li>', [FProblemesTrouves[i]]));
      HTML.AppendLine('</ul>');
    end;

    HTML.AppendLine('</body>');
    HTML.AppendLine('</html>');

    Result := HTML.ToString;
  finally
    HTML.Free;
  end;
end;
```

## Automatisation avec CI/CD

### Script de test pour intégration continue

```pascal
program TestsI18N_CI;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  ValidateurTraductions in 'ValidateurTraductions.pas',
  AnalyseurMiseEnPage in 'AnalyseurMiseEnPage.pas';

var
  CodeSortie: Integer;

procedure ExecuterTests;
var
  Validateur: TValidateurTraductions;
  NbProblemes: Integer;
begin
  WriteLn('=== Tests d''internationalisation ===');
  WriteLn;

  // Test des traductions
  WriteLn('Vérification des traductions...');
  Validateur := TValidateurTraductions.Create;
  try
    Validateur.ValiderToutesLesLangues;
    NbProblemes := Validateur.Problemes.Count;

    WriteLn(Validateur.GenererRapport);

    if NbProblemes > 0 then
      CodeSortie := 1; // Échec
  finally
    Validateur.Free;
  end;

  // Autres tests...

  WriteLn;
  if CodeSortie = 0 then
    WriteLn('✓ Tous les tests sont passés')
  else
    WriteLn('✗ Certains tests ont échoué');
end;

begin
  CodeSortie := 0;
  try
    ExecuterTests;
  except
    on E: Exception do
    begin
      WriteLn('ERREUR: ' + E.Message);
      CodeSortie := 2;
    end;
  end;

  ExitCode := CodeSortie;
end.
```

## Bonnes pratiques de test

### 1. Tester tôt et souvent

```
Ne pas attendre : Tester dès qu'une langue est ajoutée
Tester régulièrement : À chaque modification importante
Automatiser : Intégrer dans le processus de build
```

### 2. Impliquer des locuteurs natifs

Les meilleurs testeurs pour une langue sont les personnes qui la parlent nativement. Ils détecteront :
- Les traductions maladroites ou incorrectes
- Les formulations non naturelles
- Les erreurs culturelles
- Les problèmes de contexte

### 3. Documenter les problèmes

Pour chaque problème trouvé :
- Capturer une capture d'écran
- Noter les étapes pour reproduire
- Indiquer la langue concernée
- Évaluer la sévérité (critique, majeur, mineur)

### 4. Créer un environnement de test dédié

```pascal
procedure ConfigurerEnvironnementTest;
begin
  // Utiliser des données de test dans toutes les langues
  ChargerDonneesTest;

  // Activer les outils de développement
  ActiverModeDebug;

  // Logger tous les changements de langue
  ActiverJournalisationI18N;
end;
```

## Checklist finale avant release

```
Production:
  □ Toutes les langues ont été testées visuellement
  □ Tous les tests automatisés passent
  □ Les traductions ont été validées par des natifs
  □ Les captures d'écran dans chaque langue sont OK
  □ Les formats de date/nombre fonctionnent correctement
  □ L'export/import de données est testé dans chaque langue
  □ Les langues RTL sont testées (si applicable)
  □ La documentation utilisateur est traduite
  □ Les messages d'erreur sont tous traduits
  □ Le changement de langue fonctionne sans redémarrage
  □ Aucun texte codé en dur n'a été détecté
  □ Les fichiers de langue sont en UTF-8 avec BOM
  □ Les polices supportent tous les caractères
```

## Conclusion

Les tests d'internationalisation sont essentiels pour garantir une expérience utilisateur de qualité dans toutes les langues. En combinant tests manuels, automatisés et validation par des locuteurs natifs, vous pouvez vous assurer que votre application fonctionne parfaitement pour tous vos utilisateurs, quelle que soit leur langue.

**Points clés à retenir :**

- **Testez tôt** : Ne pas attendre la fin du développement
- **Automatisez** : Créer des outils et scripts de test
- **Documentez** : Garder une trace de tous les problèmes
- **Impliquez des natifs** : Essentiels pour la qualité des traductions
- **Utilisez des checklists** : Ne rien oublier
- **Intégrez au CI/CD** : Tests automatiques à chaque commit

Avec ces outils et méthodes, vous pouvez maintenir un haut niveau de qualité pour l'internationalisation de vos applications Delphi tout au long de leur cycle de vie.

⏭️ [Support Unicode et encodages](/13-internationalisation-et-localisation/06-support-unicode-et-encodages.md)
