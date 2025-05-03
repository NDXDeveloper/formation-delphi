# 13.3 Adaptation à différentes langues

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La simple traduction des chaînes de caractères ne suffit pas pour créer une application véritablement internationale. L'adaptation à différentes langues, ou localisation, implique de prendre en compte plusieurs aspects culturels et linguistiques spécifiques. Dans cette section, nous aborderons les différentes techniques pour adapter votre application Delphi à plusieurs langues.

## Au-delà de la traduction de texte

Adapter une application à différentes langues va au-delà de la simple traduction :

1. **Différentes longueurs de texte** : Les traductions peuvent être plus longues ou plus courtes que le texte original
2. **Ordres de mots différents** : La structure grammaticale varie selon les langues
3. **Formatage spécifique** : Dates, nombres, monnaies, etc.
4. **Lecture de droite à gauche** : Certaines langues comme l'arabe et l'hébreu
5. **Considérations culturelles** : Couleurs, symboles, images qui peuvent avoir des significations différentes

## Gestion de l'interface utilisateur

### Adapter l'interface aux différentes longueurs de texte

Les textes traduits peuvent être plus longs que dans la langue d'origine, ce qui peut poser des problèmes de mise en page :

#### 1. Utilisation de l'ancrage et du dimensionnement automatique

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurer l'ancrage du bouton pour qu'il s'adapte
  Button1.Anchors := [akLeft, akRight, akBottom];

  // Adapter automatiquement la largeur d'un label
  Label1.AutoSize := True;
end;
```

#### 2. Utilisation des contraintes pour limiter le redimensionnement

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Définir des contraintes de taille minimale et maximale
  Button1.Constraints.MinWidth := 80;
  Button1.Constraints.MaxWidth := 200;
end;
```

#### 3. Ajustement dynamique de la taille des composants

```pascal
procedure TForm1.UpdateComponentSizes;
var
  ButtonWidth: Integer;
begin
  // Calculer la largeur idéale du bouton en fonction du texte
  ButtonWidth := Canvas.TextWidth(Button1.Caption) + 20;

  // Appliquer la nouvelle taille
  Button1.Width := ButtonWidth;
end;
```

### Gestion des langues de droite à gauche (RTL)

Pour les langues comme l'arabe ou l'hébreu qui se lisent de droite à gauche :

#### 1. Définition de la propriété BiDiMode

```pascal
procedure TForm1.SwitchToRTL;
begin
  // Appliquer le mode bidirectionnel au formulaire
  Self.BiDiMode := bdRightToLeft;

  // La propriété ParentBiDiMode permet aux composants enfants d'hériter
  // du BiDiMode du parent
  Label1.ParentBiDiMode := True;
  Edit1.ParentBiDiMode := True;

  // Ou définir explicitement pour certains composants
  Memo1.BiDiMode := bdRightToLeft;
end;
```

#### 2. Inverser l'alignement des composants

```pascal
procedure TForm1.UpdateAlignment;
begin
  if ApplicationIsRTL then
  begin
    Label1.Alignment := taRightJustify;
    Edit1.Alignment := taRightJustify;
  end
  else
  begin
    Label1.Alignment := taLeftJustify;
    Edit1.Alignment := taLeftJustify;
  end;
end;
```

#### 3. Activation du support RTL au niveau de l'application

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  if LanguageManager.CurrentLanguage = 'Arabic' then
  begin
    // Définir le mode RTL au niveau de l'application
    Application.BiDiMode := bdRightToLeft;
  end
  else
  begin
    Application.BiDiMode := bdLeftToRight;
  end;
end;
```

> ⚠️ Attention : Les changements de BiDiMode sont plus efficaces s'ils sont appliqués avant la création des formulaires. Idéalement, configurez-le dans le fichier .dpr du projet.

## Adaptation des formats régionaux

### Formats de date et heure

Les formats de date et heure varient considérablement selon les pays :

```pascal
uses
  System.SysUtils, System.DateUtils;

procedure TForm1.FormatDatesForLocale;
var
  CurrentDate: TDateTime;
  FormattedDate: string;
begin
  CurrentDate := Now;

  case LanguageManager.CurrentLanguage of
    'French':
      begin
        // Format français: jour/mois/année
        FormatSettings.ShortDateFormat := 'dd/mm/yyyy';
        FormatSettings.LongDateFormat := 'dddd d mmmm yyyy';
        FormatSettings.TimeFormat := 'hh:mm:ss';
      end;
    'English':
      begin
        // Format anglais: mois/jour/année
        FormatSettings.ShortDateFormat := 'mm/dd/yyyy';
        FormatSettings.LongDateFormat := 'dddd, mmmm d, yyyy';
        FormatSettings.TimeFormat := 'hh:mm:ss tt'; // Avec AM/PM
      end;
    'German':
      begin
        // Format allemand: jour.mois.année
        FormatSettings.ShortDateFormat := 'dd.mm.yyyy';
        FormatSettings.LongDateFormat := 'dddd, d. mmmm yyyy';
        FormatSettings.TimeFormat := 'hh:mm:ss';
      end;
  end;

  // Afficher la date selon le format local
  FormattedDate := DateTimeToStr(CurrentDate);
  ShowMessage(FormattedDate);
end;
```

### Formats numériques et monétaires

Les formats numériques et monétaires diffèrent également :

```pascal
procedure TForm1.FormatNumbersForLocale;
var
  Value: Double;
  FormattedValue: string;
begin
  Value := 1234567.89;

  case LanguageManager.CurrentLanguage of
    'French':
      begin
        // Format français: 1 234 567,89 €
        FormatSettings.DecimalSeparator := ',';
        FormatSettings.ThousandSeparator := ' ';
        FormatSettings.CurrencyString := '€';
        FormatSettings.CurrencyFormat := 1; // Symbole après le montant
      end;
    'English':
      begin
        // Format anglais: $1,234,567.89
        FormatSettings.DecimalSeparator := '.';
        FormatSettings.ThousandSeparator := ',';
        FormatSettings.CurrencyString := '$';
        FormatSettings.CurrencyFormat := 0; // Symbole avant le montant
      end;
    'German':
      begin
        // Format allemand: 1.234.567,89 €
        FormatSettings.DecimalSeparator := ',';
        FormatSettings.ThousandSeparator := '.';
        FormatSettings.CurrencyString := '€';
        FormatSettings.CurrencyFormat := 1; // Symbole après le montant
      end;
  end;

  // Afficher le nombre selon le format local
  FormattedValue := FormatFloat('#,##0.00', Value);
  ShowMessage(FormattedValue);

  // Afficher le montant en devise locale
  FormattedValue := FormatCurr('###,###,##0.00 €', Value);
  ShowMessage(FormattedValue);
end;
```

> 💡 À partir de Delphi XE3, vous pouvez utiliser `TFormatSettings.Create` pour créer des instances de formats spécifiques à chaque langue, ce qui évite de modifier les paramètres globaux.

```pascal
var
  FrenchFormat, EnglishFormat: TFormatSettings;
begin
  // Créer des formats régionaux spécifiques
  FrenchFormat := TFormatSettings.Create('fr-FR');
  EnglishFormat := TFormatSettings.Create('en-US');

  // Utiliser les formats spécifiques
  ShowMessage(DateTimeToStr(Now, FrenchFormat));
  ShowMessage(DateTimeToStr(Now, EnglishFormat));
end;
```

## Gestion de l'encodage des caractères

### Utilisation d'Unicode

Dans les versions modernes de Delphi, le type `string` est en fait un `UnicodeString`, ce qui facilite la gestion des caractères internationaux :

```pascal
var
  JapaneseText: string;
  RussianText: string;
  ArabicText: string;
begin
  // Toutes ces chaînes sont correctement gérées
  JapaneseText := 'こんにちは';
  RussianText := 'Привет';
  ArabicText := 'مرحبا';

  // Affichage dans un contrôle
  Memo1.Lines.Add(JapaneseText);
  Memo1.Lines.Add(RussianText);
  Memo1.Lines.Add(ArabicText);
end;
```

### Fichiers texte et encodage

Lorsque vous manipulez des fichiers texte, veillez à utiliser le bon encodage :

```pascal
procedure TForm1.SaveTextWithEncoding(const FileName, Text: string; Encoding: TEncoding);
var
  StreamWriter: TStreamWriter;
begin
  StreamWriter := TStreamWriter.Create(FileName, False, Encoding);
  try
    StreamWriter.Write(Text);
  finally
    StreamWriter.Free;
  end;
end;

procedure TForm1.LoadTextWithEncoding(const FileName: string; Encoding: TEncoding);
var
  StreamReader: TStreamReader;
  Text: string;
begin
  StreamReader := TStreamReader.Create(FileName, Encoding);
  try
    Text := StreamReader.ReadToEnd;
    Memo1.Text := Text;
  finally
    StreamReader.Free;
  end;
end;

procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  // Sauvegarder avec l'encodage UTF-8
  SaveTextWithEncoding('texte.txt', Memo1.Text, TEncoding.UTF8);

  // Ou avec d'autres encodages
  // SaveTextWithEncoding('texte.txt', Memo1.Text, TEncoding.Unicode);
  // SaveTextWithEncoding('texte.txt', Memo1.Text, TEncoding.ASCII);
end;
```

> 💡 L'encodage UTF-8 est généralement recommandé pour les fichiers texte internationaux car il est compact et compatible avec la plupart des systèmes.

## Adaptation de la disposition des contrôles

### Redimensionnement automatique des contrôles

Pour que votre interface s'adapte aux différentes longueurs de texte :

```pascal
procedure TForm1.LocalizeControls;
var
  MaxWidth: Integer;
  I: Integer;
begin
  // Mettre à jour les textes
  TranslateForm(Self);

  // Rechercher la largeur maximale nécessaire pour les labels
  MaxWidth := 0;
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TLabel then
    begin
      // Activer l'auto-dimensionnement
      TLabel(Components[I]).AutoSize := True;

      // Trouver la largeur maximale
      if TLabel(Components[I]).Width > MaxWidth then
        MaxWidth := TLabel(Components[I]).Width;
    end;
  end;

  // Ajuster les contrôles en conséquence
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TEdit then
    begin
      // Déplacer les contrôles de saisie pour s'aligner avec les labels
      TEdit(Components[I]).Left := MaxWidth + 10;
    end;
  end;
end;
```

### Utilisation des layouts pour une disposition flexible

Delphi offre des composants de mise en page qui facilitent l'adaptation de l'interface :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Utiliser TFlowPanel pour une disposition flexible
  FlowPanel1.AutoWrap := True;
  FlowPanel1.FlowDirection := fdLeftToRight;

  // Ajouter des contrôles au TFlowPanel
  Button1.Parent := FlowPanel1;
  Button2.Parent := FlowPanel1;
  Button3.Parent := FlowPanel1;
end;
```

## Exemple complet : Système de localisation avancé

Voici un exemple qui intègre la plupart des techniques vues précédemment :

```pascal
unit LocalizationManager;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, Vcl.Forms, Vcl.Controls;

type
  TLocalizationManager = class
  private
    FLanguageFile: string;
    FCurrentLanguage: string;
    FIniFile: TIniFile;
    FFormatSettings: TFormatSettings;
    function GetString(const Identifier: string): string;
  public
    constructor Create(const ALanguageFile: string);
    destructor Destroy; override;
    procedure SetLanguage(const Language: string);
    procedure ApplyLanguage(AForm: TForm);
    function FormatDate(const ADate: TDateTime): string;
    function FormatNumber(const AValue: Double): string;
    function FormatCurrency(const AValue: Double): string;
    property CurrentLanguage: string read FCurrentLanguage;
    property Strings[const Identifier: string]: string read GetString; default;
    property Format: TFormatSettings read FFormatSettings;
  end;

implementation

uses
  Vcl.StdCtrls;

constructor TLocalizationManager.Create(const ALanguageFile: string);
begin
  inherited Create;
  FLanguageFile := ALanguageFile;
  FIniFile := TIniFile.Create(FLanguageFile);
  // Langue par défaut
  SetLanguage('French');
end;

destructor TLocalizationManager.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TLocalizationManager.SetLanguage(const Language: string);
begin
  FCurrentLanguage := Language;

  // Configurer les formats régionaux selon la langue
  case Language of
    'French':
      begin
        FFormatSettings := TFormatSettings.Create('fr-FR');
      end;
    'English':
      begin
        FFormatSettings := TFormatSettings.Create('en-US');
      end;
    'German':
      begin
        FFormatSettings := TFormatSettings.Create('de-DE');
      end;
    'Arabic':
      begin
        FFormatSettings := TFormatSettings.Create('ar-SA');
        Application.BiDiMode := bdRightToLeft;
      end;
    else
      FFormatSettings := TFormatSettings.Create;
  end;
end;

function TLocalizationManager.GetString(const Identifier: string): string;
begin
  Result := FIniFile.ReadString(FCurrentLanguage, Identifier, Identifier);
end;

procedure TLocalizationManager.ApplyLanguage(AForm: TForm);
var
  I: Integer;
  Component: TComponent;
  PropertyName: string;
begin
  // Titre du formulaire
  AForm.Caption := GetString('form_' + AForm.Name + '_caption');

  // Parcourir tous les composants du formulaire
  for I := 0 to AForm.ComponentCount - 1 do
  begin
    Component := AForm.Components[I];

    // Déterminer la propriété à traduire selon le type de composant
    if Component is TButton then
      PropertyName := 'Caption'
    else if Component is TLabel then
      PropertyName := 'Caption'
    else if Component is TEdit then
      PropertyName := 'Text'
    else if Component is TMemo then
      PropertyName := 'Text'
    else
      Continue;

    // Identifier unique pour chaque composant
    try
      SetStrProp(Component, PropertyName,
        GetString('ctrl_' + Component.Name + '_' + PropertyName));
    except
      // Ignorer les erreurs
    end;

    // Activer l'autosize pour les labels
    if Component is TLabel then
      TLabel(Component).AutoSize := True;
  end;

  // Adapter l'interface selon la direction du texte
  if FCurrentLanguage = 'Arabic' then
    AForm.BiDiMode := bdRightToLeft
  else
    AForm.BiDiMode := bdLeftToRight;
end;

function TLocalizationManager.FormatDate(const ADate: TDateTime): string;
begin
  Result := DateToStr(ADate, FFormatSettings);
end;

function TLocalizationManager.FormatNumber(const AValue: Double): string;
begin
  Result := FormatFloat('#,##0.00', AValue, FFormatSettings);
end;

function TLocalizationManager.FormatCurrency(const AValue: Double): string;
begin
  Result := FormatCurr('#,##0.00', AValue, FFormatSettings);
end;

end.
```

### Utilisation du gestionnaire de localisation

```pascal
unit MainForm;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  LocalizationManager;

type
  TfrmMain = class(TForm)
    lblWelcome: TLabel;
    lblDate: TLabel;
    lblAmount: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    rgLanguage: TRadioGroup;
    edtAmount: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgLanguageClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    FLocManager: TLocalizationManager;
    procedure UpdateUI;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLocManager := TLocalizationManager.Create(
    ExtractFilePath(Application.ExeName) + 'languages.ini');

  // Configurer le RadioGroup
  rgLanguage.Items.Clear;
  rgLanguage.Items.Add('Français');
  rgLanguage.Items.Add('English');
  rgLanguage.Items.Add('Deutsch');
  rgLanguage.Items.Add('العربية');

  // Langue par défaut: Français
  rgLanguage.ItemIndex := 0;
  FLocManager.SetLanguage('French');

  UpdateUI;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLocManager.Free;
end;

procedure TfrmMain.rgLanguageClick(Sender: TObject);
begin
  case rgLanguage.ItemIndex of
    0: FLocManager.SetLanguage('French');
    1: FLocManager.SetLanguage('English');
    2: FLocManager.SetLanguage('German');
    3: FLocManager.SetLanguage('Arabic');
  end;

  UpdateUI;
end;

procedure TfrmMain.UpdateUI;
begin
  // Appliquer les traductions
  FLocManager.ApplyLanguage(Self);

  // Mettre à jour les formats
  lblDate.Caption := FLocManager['lbl_date'] + ': ' +
                     FLocManager.FormatDate(Date);

  // Formater le montant s'il est valide
  try
    lblAmount.Caption := FLocManager['lbl_amount'] + ': ' +
                         FLocManager.FormatCurrency(StrToFloat(edtAmount.Text));
  except
    lblAmount.Caption := FLocManager['lbl_amount'] + ': ' +
                         FLocManager.FormatCurrency(0);
  end;
end;

procedure TfrmMain.btnOKClick(Sender: TObject);
begin
  ShowMessage(FLocManager['msg_thank_you']);
end;

end.
```

## Bonnes pratiques pour l'adaptation multilingue

1. **Tester avec les langues cibles** : Ne vous contentez pas de traduire, testez réellement avec les langues cibles

2. **Prévoir de l'espace** : Le texte traduit peut être jusqu'à 30% plus long que l'original

3. **Utiliser des polices universelles** : Assurez-vous que les polices utilisées prennent en charge tous les caractères nécessaires

4. **Éviter les images contenant du texte** : Utilisez plutôt des chaînes localisables

5. **Tester avec différentes résolutions** : L'interface doit rester fonctionnelle sur différentes tailles d'écran

6. **Attention aux références culturelles** : Évitez les expressions idiomatiques ou les références trop spécifiques

7. **Noms de fichiers et chemins** : Utilisez des chemins qui fonctionnent avec des caractères non-ASCII

8. **Clavier et saisie** : Assurez-vous que les raccourcis clavier fonctionnent avec différentes dispositions de clavier

## Conclusion

L'adaptation à différentes langues va bien au-delà de la simple traduction des textes de votre application. Elle implique la prise en compte des spécificités culturelles, des formats de données, de la direction d'écriture et de l'ergonomie. En suivant les bonnes pratiques et en utilisant les outils appropriés, vous pouvez créer des applications Delphi véritablement internationales qui offriront une expérience utilisateur optimale, quelle que soit la langue ou la région de l'utilisateur.

Delphi offre un excellent support pour la localisation, tant au niveau de la VCL que de l'environnement de développement. En combinant ces capacités avec une bonne architecture logicielle et des processus de développement adaptés, vous pourrez facilement maintenir et étendre votre application pour répondre aux besoins d'un public international.

---

Dans la prochaine section, nous verrons comment gérer les formats de date, heure et nombres spécifiques à chaque culture.

⏭️ [Formats de date, heure et nombres](/13-internationalisation-et-localisation/04-formats-de-date-heure-et-nombres.md)
