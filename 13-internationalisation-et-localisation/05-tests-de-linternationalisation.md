# 13.5 Tests de l'internationalisation

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Après avoir implémenté l'internationalisation dans votre application Delphi, il est crucial de tester rigoureusement toutes les fonctionnalités liées aux différentes langues et cultures. Les tests d'internationalisation permettent de s'assurer que votre application fonctionnera correctement pour tous les utilisateurs, indépendamment de leur langue ou région.

## Pourquoi tester l'internationalisation ?

Les tests d'internationalisation sont essentiels car :

1. **Les bugs d'internationalisation sont souvent invisibles** dans la langue de développement
2. **Les problèmes peuvent varier selon les langues** (certaines langues posent des défis spécifiques)
3. **La correction tardive des problèmes d'internationalisation est coûteuse**
4. **Une mauvaise expérience linguistique peut compromettre l'adoption** de votre application

## Types de tests d'internationalisation

### 1. Tests fonctionnels de base

Commencez par vérifier que les fonctionnalités essentielles marchent dans toutes les langues supportées :

- **Affichage correct des menus et formulaires**
- **Fonctionnement des boutons et contrôles**
- **Passage d'une langue à l'autre**
- **Persistance des paramètres linguistiques**

### 2. Tests d'interface utilisateur

Vérifiez que l'interface s'adapte correctement aux différentes langues :

- **Vérification des troncatures de texte**
- **Alignement et espacement des contrôles**
- **Affichage des caractères spéciaux**
- **Comportement des langues de droite à gauche (RTL)**

### 3. Tests de saisie et validation

Assurez-vous que la saisie et la validation des données fonctionnent dans toutes les langues :

- **Saisie de caractères spéciaux**
- **Validation des formulaires**
- **Conversion des formats de date et nombres**
- **Gestion des erreurs multilingues**

### 4. Tests de performance

Vérifiez que les performances restent acceptables :

- **Temps de chargement des ressources linguistiques**
- **Réactivité de l'interface utilisateur**
- **Utilisation de la mémoire**

## Préparation de l'environnement de test

Avant de commencer les tests, assurez-vous que votre environnement est correctement configuré :

### Configuration du système pour différentes langues

```pascal
// Vérifier les langues disponibles sur le système
procedure TForm1.CheckAvailableLanguages;
var
  Locales: TStringList;
  I: Integer;
begin
  Locales := TStringList.Create;
  try
    // Récupérer toutes les locales disponibles
    for I := 1 to GetLocaleCount do
      Locales.Add(GetLocaleStr(I));

    // Afficher les locales disponibles
    ShowMessage('Locales disponibles : ' + Locales.Text);
  finally
    Locales.Free;
  end;
end;
```

> 💡 Vous pouvez également modifier temporairement les paramètres régionaux de Windows pour tester votre application dans des environnements localisés.

### Création d'un formulaire de test

Créez un formulaire dédié aux tests d'internationalisation :

```pascal
unit TestI18NForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmTestI18N = class(TForm)
    cmbLanguages: TComboBox;
    lblSelectLanguage: TLabel;
    btnApplyLanguage: TButton;
    mmoResults: TMemo;
    btnTestUI: TButton;
    btnTestDates: TButton;
    btnTestNumbers: TButton;
    btnTestBiDi: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnApplyLanguageClick(Sender: TObject);
    procedure btnTestUIClick(Sender: TObject);
    procedure btnTestDatesClick(Sender: TObject);
    procedure btnTestNumbersClick(Sender: TObject);
    procedure btnTestBiDiClick(Sender: TObject);
  private
    procedure LogResult(const Test, Result: string);
  end;

var
  frmTestI18N: TfrmTestI18N;

implementation

{$R *.dfm}

uses
  LocalizationManager; // Votre module de gestion de localisation

// Implémentation des méthodes...
```

## Méthodes de test et vérification

### 1. Test de l'interface utilisateur

Voici comment vérifier l'adaptation de l'interface utilisateur :

```pascal
procedure TfrmTestI18N.btnTestUIClick(Sender: TObject);
var
  Form: TForm;
  I: Integer;
  MaxWidth, MaxHeight: Integer;
  Control: TControl;
  OverflowFound: Boolean;
begin
  mmoResults.Clear;
  LogResult('Test UI', 'Démarrage des tests d''interface...');

  // Créer un formulaire temporaire pour les tests
  Form := TForm.Create(nil);
  try
    // Appliquer la localisation au formulaire
    LocalizationManager.ApplyTranslations(Form);

    // Vérifier les dimensions des contrôles
    MaxWidth := Form.ClientWidth;
    MaxHeight := Form.ClientHeight;
    OverflowFound := False;

    for I := 0 to Form.ComponentCount - 1 do
    begin
      if Form.Components[I] is TControl then
      begin
        Control := TControl(Form.Components[I]);

        // Vérifier si un contrôle dépasse les limites du formulaire
        if (Control.Left + Control.Width > MaxWidth) or
           (Control.Top + Control.Height > MaxHeight) then
        begin
          LogResult('Overflow', Format('Le contrôle %s dépasse les limites (%d,%d)',
            [Control.Name, Control.Left + Control.Width, Control.Top + Control.Height]));
          OverflowFound := True;
        end;

        // Vérifier si le texte est tronqué (pour les labels, boutons, etc.)
        if (Control is TLabel) or (Control is TButton) then
        begin
          // Cette vérification est simplifiée, à adapter selon vos besoins
          if Control.Width < Canvas.TextWidth(TLabel(Control).Caption) + 10 then
            LogResult('Troncature', Format('Le texte "%s" est probablement tronqué',
              [TLabel(Control).Caption]));
        end;
      end;
    end;

    if not OverflowFound then
      LogResult('Overflow', 'Aucun débordement détecté');

  finally
    Form.Free;
  end;
end;
```

### 2. Test des formats de date

```pascal
procedure TfrmTestI18N.btnTestDatesClick(Sender: TObject);
const
  TEST_DATES: array[0..2] of TDateTime = (
    42736.0,  // Une date arbitraire
    Now,      // Date et heure actuelles
    0.0       // 30/12/1899, date de référence Delphi
  );
var
  DateStr: string;
  ParsedDate: TDateTime;
  I: Integer;
  Success: Boolean;
begin
  mmoResults.Clear;
  LogResult('Test Dates', 'Démarrage des tests de format de date...');

  for I := 0 to High(TEST_DATES) do
  begin
    // Formater la date selon les paramètres régionaux actuels
    DateStr := DateToStr(TEST_DATES[I]);
    LogResult('Format', Format('Date %d formatée : %s', [I, DateStr]));

    // Essayer de reconvertir en TDateTime
    Success := True;
    try
      ParsedDate := StrToDate(DateStr);
      // Vérifier si la conversion est correcte (à la journée près)
      if Trunc(ParsedDate) = Trunc(TEST_DATES[I]) then
        LogResult('Parsing', Format('Date %d correctement analysée', [I]))
      else
        LogResult('Parsing', Format('Erreur d''analyse : attendu %s, obtenu %s',
          [DateToStr(TEST_DATES[I]), DateToStr(ParsedDate)]));
    except
      on E: Exception do
      begin
        LogResult('Parsing', Format('Exception lors de l''analyse de la date : %s',
          [E.Message]));
        Success := False;
      end;
    end;

    if Success then
      LogResult('Résultat', Format('Test de date %d : RÉUSSI', [I]))
    else
      LogResult('Résultat', Format('Test de date %d : ÉCHEC', [I]));
  end;
end;
```

### 3. Test des formats de nombres

```pascal
procedure TfrmTestI18N.btnTestNumbersClick(Sender: TObject);
const
  TEST_NUMBERS: array[0..3] of Double = (
    1234.56,      // Nombre décimal simple
    -9876.54,     // Nombre négatif
    0.123456789,  // Petit nombre décimal
    1000000.01    // Grand nombre avec décimales
  );
var
  NumStr: string;
  ParsedNum: Double;
  I: Integer;
  Success: Boolean;
  Epsilon: Double;
begin
  mmoResults.Clear;
  LogResult('Test Nombres', 'Démarrage des tests de format de nombre...');
  Epsilon := 0.0001; // Tolérance pour la comparaison de nombres à virgule flottante

  for I := 0 to High(TEST_NUMBERS) do
  begin
    // Formater le nombre selon les paramètres régionaux actuels
    NumStr := FloatToStr(TEST_NUMBERS[I]);
    LogResult('Format', Format('Nombre %d formaté : %s', [I, NumStr]));

    // Essayer de reconvertir en Double
    Success := True;
    try
      ParsedNum := StrToFloat(NumStr);
      // Vérifier si la conversion est correcte (à la tolérance près)
      if Abs(ParsedNum - TEST_NUMBERS[I]) < Epsilon then
        LogResult('Parsing', Format('Nombre %d correctement analysé', [I]))
      else
        LogResult('Parsing', Format('Erreur d''analyse : attendu %f, obtenu %f',
          [TEST_NUMBERS[I], ParsedNum]));
    except
      on E: Exception do
      begin
        LogResult('Parsing', Format('Exception lors de l''analyse du nombre : %s',
          [E.Message]));
        Success := False;
      end;
    end;

    if Success then
      LogResult('Résultat', Format('Test de nombre %d : RÉUSSI', [I]))
    else
      LogResult('Résultat', Format('Test de nombre %d : ÉCHEC', [I]));
  end;
end;
```

### 4. Test de la prise en charge RTL (droite à gauche)

```pascal
procedure TfrmTestI18N.btnTestBiDiClick(Sender: TObject);
var
  TestForm: TForm;
  Edit: TEdit;
  Memo: TMemo;
  RTLSupported: Boolean;
begin
  mmoResults.Clear;
  LogResult('Test BiDi', 'Démarrage des tests bidirectionnels...');

  // Créer un formulaire temporaire pour les tests
  TestForm := TForm.Create(nil);
  try
    TestForm.BiDiMode := bdRightToLeft;
    RTLSupported := (TestForm.BiDiMode = bdRightToLeft);

    if RTLSupported then
    begin
      LogResult('BiDi', 'Support RTL détecté dans les formulaires');

      // Tester l'alignement des contrôles
      Edit := TEdit.Create(TestForm);
      Edit.Parent := TestForm;
      Edit.ParentBiDiMode := True;

      if Edit.BiDiMode = bdRightToLeft then
        LogResult('BiDi', 'Les contrôles héritent correctement du mode RTL')
      else
        LogResult('BiDi', 'PROBLÈME : Les contrôles n''héritent pas du mode RTL');

      // Tester la saisie de texte RTL
      Memo := TMemo.Create(TestForm);
      Memo.Parent := TestForm;
      Memo.ParentBiDiMode := True;
      Memo.Text := 'مرحبا بالعالم'; // "Hello World" en arabe

      LogResult('BiDi', 'Test de texte arabe effectué');
    end
    else
      LogResult('BiDi', 'PROBLÈME : Le support RTL n''est pas activé correctement');

  finally
    TestForm.Free;
  end;
end;
```

### 5. Fonction utilitaire pour enregistrer les résultats

```pascal
procedure TfrmTestI18N.LogResult(const Test, Result: string);
begin
  mmoResults.Lines.Add(Format('[%s] %s: %s', [FormatDateTime('hh:nn:ss', Now), Test, Result]));
end;
```

## Liste de contrôle pour les tests manuels

Voici une liste de vérification que vous pouvez suivre pour tester manuellement l'internationalisation de votre application :

### Interface utilisateur
- [ ] Tous les textes sont-ils traduits ?
- [ ] Les textes sont-ils correctement alignés ?
- [ ] Les textes sont-ils lisibles (non tronqués) ?
- [ ] Les raccourcis clavier fonctionnent-ils ?
- [ ] Les images contenant du texte sont-elles localisées ?
- [ ] Les contrôles s'adaptent-ils aux différentes longueurs de texte ?

### Formats régionaux
- [ ] Les dates s'affichent-elles correctement ?
- [ ] Les nombres s'affichent-ils correctement ?
- [ ] Les devises s'affichent-elles correctement ?
- [ ] La saisie de dates/nombres avec les formats locaux fonctionne-t-elle ?
- [ ] Les tri et filtres fonctionnent-ils correctement avec les caractères accentués ?

### Bidirectionnalité (RTL)
- [ ] L'interface s'adapte-t-elle correctement en mode RTL ?
- [ ] Les menus et barres d'outils sont-ils correctement inversés ?
- [ ] Les contrôles personnalisés supportent-ils le mode RTL ?
- [ ] Le texte bidirectionnel (mélange RTL/LTR) s'affiche-t-il correctement ?

### Fonctionnalités générales
- [ ] Le changement de langue en cours d'exécution fonctionne-t-il ?
- [ ] Les messages d'erreur sont-ils traduits ?
- [ ] L'aide et la documentation sont-elles disponibles dans toutes les langues ?
- [ ] L'impression fonctionne-t-elle avec les différentes langues ?

## Automatisation des tests d'internationalisation

Pour les applications complexes, l'automatisation des tests peut être une bonne approche :

```pascal
unit AutomatedI18NTests;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TInternationalizationTests = class
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestAllTranslationsExist;
    [Test]
    procedure TestDateFormatting;
    [Test]
    procedure TestNumberFormatting;
    [Test]
    procedure TestRTLSupport;
    [Test]
    [TestCase('French', 'fr-FR')]
    [TestCase('English', 'en-US')]
    [TestCase('German', 'de-DE')]
    [TestCase('Arabic', 'ar-SA')]
    procedure TestSpecificLocale(const LocaleName: string);
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections, LocalizationManager;

// Implémentation des méthodes de test...
```

> 💡 L'utilisation de DUnitX ou d'un autre framework de test permet d'exécuter automatiquement des tests d'internationalisation à chaque compilation.

## Tester avec des scripts spécifiques

Pour les langues utilisant des scripts spéciaux, assurez-vous de tester avec des textes réels :

### Exemples de textes de test dans différents scripts

```pascal
procedure TfrmTestI18N.TestSpecialScripts;
const
  TEST_STRINGS: array[0..5] of record
    Language: string;
    Sample: string;
  end = (
    (Language: 'Arabe'; Sample: 'مرحبا بالعالم'),
    (Language: 'Chinois'; Sample: '你好，世界'),
    (Language: 'Japonais'; Sample: 'こんにちは世界'),
    (Language: 'Coréen'; Sample: '안녕하세요 세계'),
    (Language: 'Russe'; Sample: 'Привет, мир'),
    (Language: 'Thaï'; Sample: 'สวัสดีชาวโลก')
  );
var
  I: Integer;
begin
  mmoResults.Clear;
  LogResult('Scripts spéciaux', 'Test de rendu pour différents scripts...');

  for I := 0 to High(TEST_STRINGS) do
  begin
    // Tester l'affichage
    mmoResults.Lines.Add(Format('%s: %s', [TEST_STRINGS[I].Language, TEST_STRINGS[I].Sample]));

    // Vérifier que le nombre de caractères correspond
    LogResult(TEST_STRINGS[I].Language,
      Format('Longueur correcte : %d', [Length(TEST_STRINGS[I].Sample)]));
  end;
end;
```

## Détection des problèmes courants

Voici quelques problèmes courants à surveiller lors des tests d'internationalisation :

### 1. Problèmes de troncature de texte

```pascal
procedure DetectTruncationIssues(AForm: TForm);
var
  I: Integer;
  Control: TControl;
  TextWidth: Integer;
begin
  for I := 0 to AForm.ComponentCount - 1 do
  begin
    if AForm.Components[I] is TLabel then
    begin
      Control := TLabel(AForm.Components[I]);

      // Calculer la largeur du texte
      TextWidth := AForm.Canvas.TextWidth(TLabel(Control).Caption);

      // Si le texte est plus large que le contrôle
      if TextWidth > Control.Width then
        ShowMessage(Format('Texte tronqué détecté pour %s: "%s"',
          [Control.Name, TLabel(Control).Caption]));
    end;
  end;
end;
```

### 2. Problèmes de conversion de format

```pascal
procedure TestFormatConversions;
var
  DateStr, NumStr: string;
  TestDate: TDateTime;
  TestNumber: Double;
  Success: Boolean;
begin
  // Test avec différentes locales
  SetFormatsForLocale('fr-FR');

  // Tester une date
  TestDate := EncodeDate(2023, 4, 15);
  DateStr := DateToStr(TestDate);

  // Essayer de reconvertir
  Success := True;
  try
    TestDate := StrToDate(DateStr);
  except
    Success := False;
  end;

  if not Success then
    ShowMessage('Problème de conversion de date détecté pour fr-FR');

  // Tester un nombre
  TestNumber := 1234.56;
  NumStr := FloatToStr(TestNumber);

  // Essayer de reconvertir
  Success := True;
  try
    TestNumber := StrToFloat(NumStr);
  except
    Success := False;
  end;

  if not Success then
    ShowMessage('Problème de conversion de nombre détecté pour fr-FR');
end;
```

## Outils et ressources pour les tests d'internationalisation

### Création d'un outil de pseudo-localisation

La pseudo-localisation est une technique qui remplace les caractères par des équivalents accentués pour simuler une traduction, sans réellement traduire :

```pascal
function PseudoLocalize(const AString: string): string;
const
  // Tableau de remplacement des caractères
  CHAR_MAP: array[0..25] of record
    Original, Replacement: Char;
  end = (
    (Original: 'a'; Replacement: 'á'),
    (Original: 'b'; Replacement: 'ƀ'),
    (Original: 'c'; Replacement: 'ç'),
    (Original: 'd'; Replacement: 'ð'),
    (Original: 'e'; Replacement: 'é'),
    (Original: 'f'; Replacement: 'ƒ'),
    (Original: 'g'; Replacement: 'ğ'),
    (Original: 'h'; Replacement: 'ĥ'),
    (Original: 'i'; Replacement: 'í'),
    (Original: 'j'; Replacement: 'ĵ'),
    (Original: 'k'; Replacement: 'ķ'),
    (Original: 'l'; Replacement: 'ł'),
    (Original: 'm'; Replacement: 'ɱ'),
    (Original: 'n'; Replacement: 'ñ'),
    (Original: 'o'; Replacement: 'ô'),
    (Original: 'p'; Replacement: 'þ'),
    (Original: 'q'; Replacement: 'q'),
    (Original: 'r'; Replacement: 'ŕ'),
    (Original: 's'; Replacement: 'š'),
    (Original: 't'; Replacement: 'ŧ'),
    (Original: 'u'; Replacement: 'ú'),
    (Original: 'v'; Replacement: 'ṽ'),
    (Original: 'w'; Replacement: 'ŵ'),
    (Original: 'x'; Replacement: 'ẋ'),
    (Original: 'y'; Replacement: 'ý'),
    (Original: 'z'; Replacement: 'ž')
  );
var
  I, J: Integer;
  Ch: Char;
  Found: Boolean;
begin
  // Augmenter la longueur pour simuler l'expansion du texte
  Result := '[' + AString + ']';

  // Remplacer les caractères
  for I := 1 to Length(Result) do
  begin
    Ch := LowerCase(Result[I]);
    Found := False;

    for J := 0 to High(CHAR_MAP) do
    begin
      if Ch = CHAR_MAP[J].Original then
      begin
        if Result[I] = UpperCase(Result[I])[1] then
          Result[I] := UpperCase(CHAR_MAP[J].Replacement)[1]
        else
          Result[I] := CHAR_MAP[J].Replacement;

        Found := True;
        Break;
      end;
    end;
  end;
end;
```

### Création d'un fichier de traduction de test

```pascal
procedure CreateTestTranslationFile;
var
  StrList: TStringList;
  Sections: TStringList;
  Keys: TStringList;
  I, J: Integer;
begin
  StrList := TStringList.Create;
  Sections := TStringList.Create;
  Keys := TStringList.Create;

  try
    // Extraire les sections et clés du fichier de traduction existant
    // ...

    // Créer un fichier de pseudo-traduction
    for I := 0 to Sections.Count - 1 do
    begin
      StrList.Add('[' + Sections[I] + ']');

      for J := 0 to Keys.Count - 1 do
      begin
        if Keys.Names[J] = Sections[I] then
        begin
          // Pseudo-localiser la valeur
          StrList.Add(Format('%s=%s',
            [Keys.ValueFromIndex[J], PseudoLocalize(Keys.ValueFromIndex[J])]));
        end;
      end;

      StrList.Add('');
    end;

    // Sauvegarder le fichier de test
    StrList.SaveToFile('pseudo_translation.ini');
    ShowMessage('Fichier de pseudo-traduction créé avec succès');

  finally
    StrList.Free;
    Sections.Free;
    Keys.Free;
  end;
end;
```

## Bonnes pratiques pour les tests d'internationalisation

1. **Testez dès le début du développement**, pas uniquement à la fin

2. **Créez une suite de tests automatisés** pour vérifier régulièrement l'internationalisation

3. **Utilisez des données de test réelles** pour chaque langue

4. **Vérifiez les limites et cas particuliers**, comme les très longues chaînes ou les caractères spéciaux

5. **Testez sur différentes plateformes et configurations** si votre application est multi-plateforme

6. **Impliquez des utilisateurs natifs** si possible pour valider les traductions et l'expérience utilisateur

7. **Documentez les problèmes spécifiques à chaque langue** pour référence future

8. **Intégrez les tests d'internationalisation dans votre pipeline CI/CD** (Intégration Continue/Déploiement Continu)

## Exemple complet : Création d'un outil de test d'internationalisation

Pour faciliter les tests, vous pouvez créer un outil de test complet :

```pascal
unit I18NTestTool;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TfrmI18NTester = class(TForm)
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
    tabDateFormats: TTabSheet;
    tabNumberFormats: TTabSheet;
    tabUITests: TTabSheet;
    tabRTL: TTabSheet;
    cmbLanguages: TComboBox;
    lblSelectLanguage: TLabel;
    btnApplyLanguage: TButton;
    lvwResults: TListView;
    btnRunAllTests: TButton;
    edtTestDate: TEdit;
    btnTestDate: TButton;
    lblDateFormat: TLabel;
    edtTestNumber: TEdit;
    btnTestNumber: TButton;
    lblNumberFormat: TLabel;
    btnTestUI: TButton;
    chkAutomaticUICheck: TCheckBox;
    btnTestRTL: TButton;
    chkCreateScreenshots: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnApplyLanguageClick(Sender: TObject);
    procedure btnRunAllTestsClick(Sender: TObject);
    procedure btnTestDateClick(Sender: TObject);
    procedure btnTestNumberClick(Sender: TObject);
    procedure btnTestUIClick(Sender: TObject);
    procedure btnTestRTLClick(Sender: TObject);
  private
    procedure LogResult(const TestType, TestName, Result, Status: string);
    procedure RunDateTests;
    procedure RunNumberTests;
    procedure RunUITests;
    procedure RunRTLTests;
    procedure CreateScreenshot(const FormName, FileName: string);
  end;

var
  frmI18NTester: TfrmI18NTester;

implementation

{$R *.dfm}

uses
  LocalizationManager, System.DateUtils, System.IOUtils;

// Implémentation des méthodes...
```

## Conclusion

Les tests d'internationalisation sont une étape essentielle pour garantir la qualité et l'utilisabilité de votre application Delphi dans un contexte international. En suivant une approche méthodique et en utilisant les techniques présentées dans cette section, vous pouvez identifier et résoudre les problèmes d'internationalisation avant qu'ils n'atteignent vos utilisateurs.

Les principaux points à retenir sont :

1. **Testez toutes les langues supportées**, pas seulement la langue principale
2. **Vérifiez l'interface utilisateur, les formats de données et les fonctionnalités spécifiques** à chaque langue
3. **Automatisez autant que possible les tests** pour faciliter les vérifications régulières
4. **Utilisez des outils comme la pseudo-localisation** pour détecter les problèmes potentiels
5. **Documentez les résultats des tests** pour référence future

En investissant du temps dans les tests d'internationalisation, vous améliorerez considérablement l'expérience utilisateur de votre application pour tous vos utilisateurs, quelle que soit leur langue ou leur région.

---

Dans la prochaine section, nous verrons comment gérer les encodages et le support Unicode dans vos applications Delphi.

⏭️ [Support Unicode et encodages](/13-internationalisation-et-localisation/06-support-unicode-et-encodages.md)
