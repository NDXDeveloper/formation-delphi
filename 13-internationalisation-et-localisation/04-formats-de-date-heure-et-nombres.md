# 13.4 Formats de date, heure et nombres

La gestion correcte des formats de date, heure et nombres est essentielle pour créer une application véritablement internationale. Ces formats varient considérablement d'un pays à l'autre, et leur bonne gestion contribue grandement à l'expérience utilisateur. Dans cette section, nous allons explorer en détail comment Delphi permet de gérer ces différents formats.

## L'importance des formats localisés

Pourquoi est-il important de respecter les formats locaux ?

- **Compréhension immédiate** : Les utilisateurs reconnaissent instantanément les formats qu'ils utilisent au quotidien
- **Éviter les confusions** : Par exemple, 04/07/2023 signifie 4 juillet 2023 aux États-Unis, mais 7 avril 2023 en France
- **Respect des conventions** : Montre que votre application est soignée et professionnelle

## La structure TFormatSettings

Delphi utilise la structure `TFormatSettings` pour gérer tous les paramètres régionaux liés au formatage. Cette structure contient de nombreux champs qui définissent comment les dates, heures et nombres doivent être affichés et interprétés.

### Création et utilisation de TFormatSettings

```pascal
var
  MyFormat: TFormatSettings;
begin
  // Obtenir les paramètres régionaux du système
  MyFormat := TFormatSettings.Create;

  // Ou créer des paramètres pour une région spécifique
  MyFormat := TFormatSettings.Create('fr-FR');  // Format français
  // MyFormat := TFormatSettings.Create('en-US');  // Format américain
  // MyFormat := TFormatSettings.Create('de-DE');  // Format allemand
end;
```

> 💡 La méthode `TFormatSettings.Create` avec un paramètre de langue est disponible depuis Delphi XE3. Sur les versions plus anciennes, utilisez `GetLocaleFormatSettings`.

### Principaux champs de TFormatSettings

Voici les principaux champs que vous pouvez personnaliser :

```pascal
// Pour les dates
MyFormat.DateSeparator := '/';             // Séparateur de date
MyFormat.ShortDateFormat := 'dd/mm/yyyy';  // Format de date court
MyFormat.LongDateFormat := 'dddd d mmmm yyyy'; // Format de date long

// Pour les heures
MyFormat.TimeSeparator := ':';         // Séparateur d'heure
MyFormat.TimeFormat := 'hh:nn:ss';     // Format d'heure (nn pour minutes)
MyFormat.TimeAMString := 'AM';         // Texte pour AM
MyFormat.TimePMString := 'PM';         // Texte pour PM

// Pour les nombres et devises
MyFormat.DecimalSeparator := ',';      // Séparateur décimal
MyFormat.ThousandSeparator := ' ';     // Séparateur des milliers
MyFormat.CurrencyString := '€';        // Symbole monétaire
MyFormat.CurrencyFormat := 1;          // Position du symbole (0=avant, 1=après)
MyFormat.NegCurrFormat := 5;           // Format pour les valeurs négatives
```

> ⚠️ Attention : Si vous modifiez la variable globale `FormatSettings`, cela affectera toutes les conversions dans votre application. Il est souvent préférable de créer des instances locales de `TFormatSettings` pour des usages spécifiques.

## Formatage des dates et heures

### Conversion entre dates et chaînes de caractères

Pour convertir une date en chaîne selon un format spécifique :

```pascal
var
  CurrentDate: TDateTime;
  FormattedDate: string;
  MyFormat: TFormatSettings;
begin
  CurrentDate := Now;  // Date et heure actuelles

  // Obtenir les paramètres régionaux
  MyFormat := TFormatSettings.Create('fr-FR');

  // Conversion simple avec les paramètres par défaut
  FormattedDate := DateToStr(CurrentDate);  // Utilise FormatSettings global
  ShowMessage('Format par défaut: ' + FormattedDate);

  // Conversion avec des paramètres personnalisés
  FormattedDate := DateToStr(CurrentDate, MyFormat);
  ShowMessage('Format français: ' + FormattedDate);

  // Utilisation de FormatDateTime pour un contrôle plus précis
  FormattedDate := FormatDateTime('dd/mm/yyyy', CurrentDate);
  ShowMessage('Format personnalisé: ' + FormattedDate);

  // Format long
  FormattedDate := FormatDateTime('dddd d mmmm yyyy à hh:nn:ss', CurrentDate, MyFormat);
  ShowMessage('Format long: ' + FormattedDate);
end;
```

### Spécificateurs de format pour les dates et heures

Delphi propose de nombreux spécificateurs pour personnaliser le format des dates et heures :

| Spécificateur | Description | Exemple |
|---------------|-------------|---------|
| `d` | Jour du mois sans zéro initial (1-31) | 5 |
| `dd` | Jour du mois avec zéro initial (01-31) | 05 |
| `ddd` | Abréviation du jour de la semaine | Lun |
| `dddd` | Nom complet du jour de la semaine | Lundi |
| `m` | Mois sans zéro initial (1-12) | 3 |
| `mm` | Mois avec zéro initial (01-12) | 03 |
| `mmm` | Abréviation du mois | Mar |
| `mmmm` | Nom complet du mois | Mars |
| `yy` | Année sur 2 chiffres | 23 |
| `yyyy` | Année sur 4 chiffres | 2023 |
| `h` | Heure sans zéro initial (0-23) | 9 |
| `hh` | Heure avec zéro initial (00-23) | 09 |
| `n` | Minute sans zéro initial (0-59) | 5 |
| `nn` | Minute avec zéro initial (00-59) | 05 |
| `s` | Seconde sans zéro initial (0-59) | 7 |
| `ss` | Seconde avec zéro initial (00-59) | 07 |
| `z` | Milliseconde sans zéros initiaux | 1 |
| `zzz` | Milliseconde avec zéros initiaux | 001 |
| `am/pm` | Indicateur AM/PM en minuscules | am |
| `a/p` | Indicateur A/P en minuscules | a |
| `AM/PM` | Indicateur AM/PM en majuscules | AM |
| `A/P` | Indicateur A/P en majuscules | A |

### Conversion de chaînes en dates

Pour convertir une chaîne en date selon un format spécifique :

```pascal
var
  DateStr: string;
  ADate: TDateTime;
  MyFormat: TFormatSettings;
begin
  // Chaîne à convertir
  DateStr := '15/03/2023';

  // Créer des paramètres de format français
  MyFormat := TFormatSettings.Create;
  MyFormat.DateSeparator := '/';
  MyFormat.ShortDateFormat := 'dd/mm/yyyy';

  try
    // Conversion en utilisant les paramètres spécifiés
    ADate := StrToDate(DateStr, MyFormat);

    // Afficher le résultat
    ShowMessage('La date a été convertie en: ' + DateToStr(ADate));

    // Obtenir les composants individuels
    ShowMessage(Format('Jour: %d, Mois: %d, Année: %d',
      [DayOf(ADate), MonthOf(ADate), YearOf(ADate)]));
  except
    on E: EConvertError do
      ShowMessage('Erreur de conversion: ' + E.Message);
  end;
end;
```

> 💡 Utilisez toujours un bloc `try..except` lorsque vous convertissez des chaînes en dates, car l'utilisateur peut saisir un format invalide.

### Fonctions utiles pour manipuler les dates

Delphi fournit de nombreuses fonctions pour manipuler les dates :

```pascal
var
  CurrentDate: TDateTime;
  NewDate: TDateTime;
begin
  CurrentDate := Now;

  // Extraire des parties de la date
  ShowMessage('Année: ' + IntToStr(YearOf(CurrentDate)));
  ShowMessage('Mois: ' + IntToStr(MonthOf(CurrentDate)));
  ShowMessage('Jour: ' + IntToStr(DayOf(CurrentDate)));

  // Extraire des parties de l'heure
  ShowMessage('Heure: ' + IntToStr(HourOf(CurrentDate)));
  ShowMessage('Minute: ' + IntToStr(MinuteOf(CurrentDate)));
  ShowMessage('Seconde: ' + IntToStr(SecondOf(CurrentDate)));

  // Ajouter des intervalles
  NewDate := IncDay(CurrentDate, 10);    // Ajouter 10 jours
  NewDate := IncMonth(CurrentDate, 3);   // Ajouter 3 mois
  NewDate := IncYear(CurrentDate, 1);    // Ajouter 1 an
  NewDate := IncHour(CurrentDate, 2);    // Ajouter 2 heures

  // Calculer des différences
  ShowMessage('Jours entre les dates: ' +
    IntToStr(DaysBetween(CurrentDate, NewDate)));
end;
```

> 💡 Ces fonctions sont définies dans l'unité `System.DateUtils`, n'oubliez pas de l'ajouter à votre clause `uses`.

## Formatage des nombres

### Conversion entre nombres et chaînes

```pascal
var
  Value: Double;
  FormattedValue: string;
  MyFormat: TFormatSettings;
begin
  Value := 1234567.89;

  // Obtenir les paramètres régionaux
  MyFormat := TFormatSettings.Create('fr-FR');

  // Format simple
  FormattedValue := FloatToStr(Value);  // Utilise FormatSettings global
  ShowMessage('Format par défaut: ' + FormattedValue);

  // Format avec paramètres personnalisés
  FormattedValue := FloatToStr(Value, MyFormat);
  ShowMessage('Format français: ' + FormattedValue);

  // Utilisation de FormatFloat pour un contrôle plus précis
  FormattedValue := FormatFloat('#,##0.00', Value, MyFormat);
  ShowMessage('Format personnalisé: ' + FormattedValue);
end;
```

### Spécificateurs de format pour les nombres

| Spécificateur | Description | Exemple (pour 1234.56) |
|---------------|-------------|------------------------|
| `0` | Chiffre (affiche 0 si absent) | 1234.56 → 1234.60 |
| `#` | Chiffre (rien si absent) | 1234.56 → 1234.6 |
| `.` | Emplacement du séparateur décimal | 1234.56 → 1234,56 (FR) |
| `,` | Séparateur des milliers et échelle | 1234.56 → 1 234,56 (FR) |
| `E+` | Notation scientifique | 1234.56 → 1.23E+03 |
| `%` | Multiplie par 100 et ajoute le symbole % | 0.5 → 50% |

### Conversion de chaînes en nombres

```pascal
var
  NumStr: string;
  Value: Double;
  MyFormat: TFormatSettings;
begin
  // Chaîne à convertir (format français)
  NumStr := '1 234,56';

  // Créer des paramètres de format français
  MyFormat := TFormatSettings.Create;
  MyFormat.DecimalSeparator := ',';
  MyFormat.ThousandSeparator := ' ';

  try
    // Conversion en utilisant les paramètres spécifiés
    Value := StrToFloat(NumStr, MyFormat);

    // Afficher le résultat
    ShowMessage('Le nombre a été converti en: ' + FloatToStr(Value));

    // Utiliser le nombre pour des calculs
    ShowMessage('Le double est: ' + FloatToStr(Value * 2));
  except
    on E: EConvertError do
      ShowMessage('Erreur de conversion: ' + E.Message);
  end;
end;
```

## Formatage des devises

Pour formater des valeurs monétaires :

```pascal
var
  Amount: Currency;
  FormattedAmount: string;
  MyFormat: TFormatSettings;
begin
  Amount := 1234.56;

  // Paramètres pour l'euro (format français)
  MyFormat := TFormatSettings.Create('fr-FR');
  MyFormat.CurrencyString := '€';
  MyFormat.CurrencyFormat := 1;  // 1 = symbole après le montant

  // Formatage simple
  FormattedAmount := CurrToStr(Amount, MyFormat);
  ShowMessage('Format par défaut: ' + FormattedAmount);

  // Formatage personnalisé
  FormattedAmount := FormatCurr('#,##0.00 €', Amount, MyFormat);
  ShowMessage('Format personnalisé: ' + FormattedAmount);

  // Paramètres pour le dollar (format US)
  MyFormat := TFormatSettings.Create('en-US');
  MyFormat.CurrencyString := '$';
  MyFormat.CurrencyFormat := 0;  // 0 = symbole avant le montant

  FormattedAmount := FormatCurr('$#,##0.00', Amount, MyFormat);
  ShowMessage('Format dollar: ' + FormattedAmount);
end;
```

## Adaptation dynamique des formats selon la langue

Pour adapter automatiquement les formats en fonction de la langue sélectionnée par l'utilisateur :

```pascal
procedure TForm1.ApplyFormatForLanguage(const Language: string);
begin
  case Language of
    'French':
      begin
        FormatSettings := TFormatSettings.Create('fr-FR');
        FormatSettings.CurrencyString := '€';
        FormatSettings.CurrencyFormat := 1;
      end;
    'English':
      begin
        FormatSettings := TFormatSettings.Create('en-US');
        FormatSettings.CurrencyString := '$';
        FormatSettings.CurrencyFormat := 0;
      end;
    'German':
      begin
        FormatSettings := TFormatSettings.Create('de-DE');
        FormatSettings.CurrencyString := '€';
        FormatSettings.CurrencyFormat := 1;
      end;
    'Spanish':
      begin
        FormatSettings := TFormatSettings.Create('es-ES');
        FormatSettings.CurrencyString := '€';
        FormatSettings.CurrencyFormat := 1;
      end;
  end;

  // Mettre à jour l'affichage
  UpdateFormattedDisplay;
end;

procedure TForm1.UpdateFormattedDisplay;
var
  CurrentDate: TDateTime;
  Amount: Currency;
begin
  CurrentDate := Now;
  Amount := 1234.56;

  lblDate.Caption := DateToStr(CurrentDate);
  lblTime.Caption := TimeToStr(CurrentDate);
  lblDateTime.Caption := DateTimeToStr(CurrentDate);

  lblNumber.Caption := FormatFloat('#,##0.00', Amount);
  lblCurrency.Caption := FormatCurr('#,##0.00 ' + FormatSettings.CurrencyString, Amount);
end;
```

## Création d'un composant d'entrée formatée

Voici un exemple de création d'un composant d'entrée qui s'adapte automatiquement au format régional :

```pascal
type
  TDateEditBox = class(TEdit)
  private
    FMyFormat: TFormatSettings;
    FDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    function GetDate: TDateTime;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    constructor Create(AOwner: TComponent); override;
    property Date: TDateTime read GetDate write SetDate;
  published
    // Propriétés publiées...
  end;

constructor TDateEditBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMyFormat := TFormatSettings.Create;
  FDate := Date;  // Date actuelle
  Text := DateToStr(FDate, FMyFormat);
end;

procedure TDateEditBox.SetDate(const Value: TDateTime);
begin
  FDate := Value;
  Text := DateToStr(FDate, FMyFormat);
end;

function TDateEditBox.GetDate: TDateTime;
begin
  Result := FDate;
end;

procedure TDateEditBox.CMExit(var Message: TCMExit);
begin
  inherited;
  try
    // Essayer de convertir la saisie utilisateur en date
    FDate := StrToDate(Text, FMyFormat);
    // Si réussi, formater proprement
    Text := DateToStr(FDate, FMyFormat);
  except
    on E: EConvertError do
    begin
      ShowMessage('Date invalide. Utilisez le format: ' +
                  FMyFormat.ShortDateFormat);
      SetFocus;
    end;
  end;
end;
```

## Exemple complet : Formulaire avec formats localisés

```pascal
unit LocalizedFormatForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TfrmFormats = class(TForm)
    lblTitle: TLabel;
    lblDateTitle: TLabel;
    lblTimeTitle: TLabel;
    lblNumberTitle: TLabel;
    lblCurrencyTitle: TLabel;
    lblDate: TLabel;
    lblTime: TLabel;
    lblNumber: TLabel;
    lblCurrency: TLabel;
    cmbLanguage: TComboBox;
    edtDate: TEdit;
    edtNumber: TEdit;
    btnApply: TButton;
    procedure FormCreate(Sender: TObject);
    procedure cmbLanguageChange(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
  private
    FFormatSettings: TFormatSettings;
    procedure ApplyFormatForLanguage(const Language: string);
    procedure UpdateFormattedDisplay;
  end;

var
  frmFormats: TfrmFormats;

implementation

{$R *.dfm}

procedure TfrmFormats.FormCreate(Sender: TObject);
begin
  // Initialiser la liste des langues
  cmbLanguage.Items.Clear;
  cmbLanguage.Items.Add('Français');
  cmbLanguage.Items.Add('English');
  cmbLanguage.Items.Add('Deutsch');
  cmbLanguage.Items.Add('Español');

  // Langue par défaut: Français
  cmbLanguage.ItemIndex := 0;
  ApplyFormatForLanguage('French');

  // Valeurs initiales
  edtDate.Text := DateToStr(Date, FFormatSettings);
  edtNumber.Text := '1234,56';
end;

procedure TfrmFormats.cmbLanguageChange(Sender: TObject);
begin
  case cmbLanguage.ItemIndex of
    0: ApplyFormatForLanguage('French');
    1: ApplyFormatForLanguage('English');
    2: ApplyFormatForLanguage('German');
    3: ApplyFormatForLanguage('Spanish');
  end;
end;

procedure TfrmFormats.ApplyFormatForLanguage(const Language: string);
begin
  case Language of
    'French':
      begin
        FFormatSettings := TFormatSettings.Create('fr-FR');
        lblTitle.Caption := 'Formats régionaux - Français';
      end;
    'English':
      begin
        FFormatSettings := TFormatSettings.Create('en-US');
        lblTitle.Caption := 'Regional Formats - English';
      end;
    'German':
      begin
        FFormatSettings := TFormatSettings.Create('de-DE');
        lblTitle.Caption := 'Regionale Formate - Deutsch';
      end;
    'Spanish':
      begin
        FFormatSettings := TFormatSettings.Create('es-ES');
        lblTitle.Caption := 'Formatos regionales - Español';
      end;
  end;

  // Afficher les formats pour référence
  lblDateTitle.Caption := 'Format de date: ' + FFormatSettings.ShortDateFormat;
  lblNumberTitle.Caption := 'Format numérique (séparateur: "' +
    FFormatSettings.DecimalSeparator + '")';

  // Mettre à jour l'affichage
  UpdateFormattedDisplay;
end;

procedure TfrmFormats.UpdateFormattedDisplay;
begin
  lblDate.Caption := DateToStr(Date, FFormatSettings);
  lblTime.Caption := TimeToStr(Time, FFormatSettings);
  lblNumber.Caption := FormatFloat('#,##0.00', 1234.56, FFormatSettings);
  lblCurrency.Caption := FormatCurr('#,##0.00', 1234.56, FFormatSettings);
end;

procedure TfrmFormats.btnApplyClick(Sender: TObject);
var
  InputDate: TDateTime;
  InputNumber: Double;
begin
  try
    // Essayer de convertir la date saisie
    InputDate := StrToDate(edtDate.Text, FFormatSettings);
    lblDate.Caption := DateToStr(InputDate, FFormatSettings);
  except
    on E: EConvertError do
      ShowMessage('Date invalide. Utilisez le format: ' +
                  FFormatSettings.ShortDateFormat);
  end;

  try
    // Essayer de convertir le nombre saisi
    InputNumber := StrToFloat(edtNumber.Text, FFormatSettings);
    lblNumber.Caption := FormatFloat('#,##0.00', InputNumber, FFormatSettings);
    lblCurrency.Caption := FormatCurr('#,##0.00', InputNumber, FFormatSettings);
  except
    on E: EConvertError do
      ShowMessage('Nombre invalide. Utilisez le séparateur: ' +
                  FFormatSettings.DecimalSeparator);
  end;
end;

end.
```

## Bonnes pratiques pour la gestion des formats

1. **Créez des instances spécifiques de TFormatSettings** plutôt que de modifier la variable globale `FormatSettings`

2. **Utilisez le type Currency** pour les valeurs monétaires afin d'éviter les erreurs d'arrondi

3. **Validez toujours les entrées utilisateur** avec un bloc `try..except` lors de la conversion

4. **Informez les utilisateurs du format attendu** si l'application n'accepte qu'un format spécifique

5. **Testez avec différentes locales** pour vous assurer que votre application se comporte correctement

6. **Évitez de coder en dur des formats** dans votre application, utilisez plutôt `TFormatSettings`

7. **N'oubliez pas les différences de fuseaux horaires** si votre application gère des dates internationales

## Chargement des formats régionaux depuis Windows

Pour utiliser les paramètres régionaux définis dans Windows :

```pascal
var
  WindowsSettings: TFormatSettings;
begin
  // Obtenir les paramètres régionaux actuels de Windows
  WindowsSettings := TFormatSettings.Create(LOCALE_USER_DEFAULT);

  // Les utiliser dans votre application
  ShowMessage('Format de date: ' + WindowsSettings.ShortDateFormat);
  ShowMessage('Séparateur décimal: ' + WindowsSettings.DecimalSeparator);
end;
```

## Conclusion

La gestion correcte des formats de date, heure et nombres est un aspect crucial de l'internationalisation de votre application. Delphi fournit tous les outils nécessaires pour adapter ces formats aux préférences linguistiques et régionales de vos utilisateurs.

En utilisant correctement `TFormatSettings` et les fonctions de formatage associées, vous pouvez créer une expérience utilisateur cohérente et professionnelle, quelle que soit la région du monde où votre application est utilisée.

---

Dans la prochaine section, nous verrons comment tester efficacement les aspects d'internationalisation de votre application pour vous assurer qu'elle fonctionne parfaitement pour tous vos utilisateurs.
