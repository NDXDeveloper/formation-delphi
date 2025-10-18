🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.4 Formats de date, heure et nombres

## Introduction

Au-delà de la traduction des textes, l'internationalisation d'une application nécessite l'adaptation des formats de date, d'heure et de nombres. Ces formats varient considérablement d'un pays à l'autre, et leur mauvaise gestion peut rendre votre application confuse ou inutilisable pour les utilisateurs internationaux.

## Pourquoi les formats varient-ils ?

Chaque culture a ses propres conventions pour représenter les dates, les heures et les nombres. Utiliser le mauvais format peut créer des ambiguïtés ou des erreurs d'interprétation.

### Exemple de confusion

La date **01/05/2024** signifie :
- **5 janvier 2024** aux États-Unis (MM/DD/YYYY)
- **1er mai 2024** en France (DD/MM/YYYY)

C'est une différence qui peut avoir des conséquences importantes !

## Formats de date selon les cultures

### Principaux formats de date

| Pays/Région | Format | Exemple | Séparateur |
|-------------|--------|---------|------------|
| France, Belgique | `DD/MM/YYYY` | 25/12/2024 | `/` |
| États-Unis | `MM/DD/YYYY` | 12/25/2024 | `/` |
| Royaume-Uni | `DD/MM/YYYY` | 25/12/2024 | `/` |
| Allemagne | `DD.MM.YYYY` | 25.12.2024 | `.` |
| Japon, Chine | `YYYY/MM/DD` | 2024/12/25 | `/` |
| ISO 8601 | `YYYY-MM-DD` | 2024-12-25 | `-` |
| Russie | `DD.MM.YYYY` | 25.12.2024 | `.` |

> 💡 **Format ISO 8601** : C'est le format international standard, sans ambiguïté, idéal pour les échanges de données.

### Formats longs et courts

Les dates peuvent être affichées sous forme courte ou longue :

| Langue | Format court | Format long |
|--------|--------------|-------------|
| Français | 25/12/2024 | mardi 25 décembre 2024 |
| Anglais (US) | 12/25/2024 | Tuesday, December 25, 2024 |
| Allemand | 25.12.2024 | Dienstag, 25. Dezember 2024 |
| Espagnol | 25/12/2024 | martes, 25 de diciembre de 2024 |

## Gestion des dates en Delphi

### Type TDateTime

Delphi utilise le type `TDateTime` pour stocker les dates et heures. Ce type est indépendant de la culture et représente un nombre décimal :

- **Partie entière** : nombre de jours depuis le 30 décembre 1899
- **Partie décimale** : fraction du jour (heure)

```pascal
var
  MaDate: TDateTime;
begin
  MaDate := Now; // Date et heure actuelles
  MaDate := Date; // Date actuelle (sans heure)
  MaDate := Time; // Heure actuelle (sans date)
end;
```

### Formatage des dates selon la culture

Delphi utilise les paramètres régionaux du système pour formater les dates.

#### Formatage automatique

```pascal
uses
  System.SysUtils;

var
  MaDate: TDateTime;
  DateStr: string;
begin
  MaDate := EncodeDate(2024, 12, 25); // 25 décembre 2024

  // Format court selon la culture du système
  DateStr := DateToStr(MaDate);
  // France : "25/12/2024"
  // USA    : "12/25/2024"

  // Format long selon la culture du système
  DateStr := FormatDateTime('dddddd', MaDate);
  // France : "mercredi 25 décembre 2024"
  // USA    : "Wednesday, December 25, 2024"
end;
```

#### Formatage personnalisé avec FormatDateTime

La fonction `FormatDateTime` permet de créer des formats personnalisés :

```pascal
var
  MaDate: TDateTime;
  DateStr: string;
begin
  MaDate := EncodeDate(2024, 12, 25);

  // Format personnalisé
  DateStr := FormatDateTime('dd/mm/yyyy', MaDate);  // "25/12/2024"
  DateStr := FormatDateTime('yyyy-mm-dd', MaDate);  // "2024-12-25" (ISO)
  DateStr := FormatDateTime('dd mmmm yyyy', MaDate); // "25 décembre 2024"
  DateStr := FormatDateTime('ddd dd mmm', MaDate);   // "mer 25 déc"
end;
```

### Spécificateurs de format pour les dates

| Spécificateur | Description | Exemple |
|---------------|-------------|---------|
| `d` | Jour sans zéro initial | 5 |
| `dd` | Jour avec zéro initial | 05 |
| `ddd` | Nom du jour abrégé | lun |
| `dddd` | Nom complet du jour | lundi |
| `m` | Mois sans zéro initial | 3 |
| `mm` | Mois avec zéro initial | 03 |
| `mmm` | Nom du mois abrégé | mar |
| `mmmm` | Nom complet du mois | mars |
| `yy` | Année sur 2 chiffres | 24 |
| `yyyy` | Année sur 4 chiffres | 2024 |
| `ddddd` | Date format court | 25/12/2024 |
| `dddddd` | Date format long | mardi 25 décembre 2024 |

### Création et décomposition de dates

```pascal
uses
  System.SysUtils, System.DateUtils;

var
  MaDate: TDateTime;
  Annee, Mois, Jour: Word;
begin
  // Créer une date
  MaDate := EncodeDate(2024, 12, 25); // 25 décembre 2024

  // Décomposer une date
  DecodeDate(MaDate, Annee, Mois, Jour);
  ShowMessage(Format('Année: %d, Mois: %d, Jour: %d', [Annee, Mois, Jour]));

  // Méthodes alternatives (plus modernes)
  Annee := YearOf(MaDate);
  Mois := MonthOf(MaDate);
  Jour := DayOf(MaDate);
end;
```

## Formats d'heure selon les cultures

### Formats 12h vs 24h

| Région | Format | Exemple |
|--------|--------|---------|
| France, Allemagne | 24 heures | 14:30:00 |
| États-Unis | 12 heures + AM/PM | 2:30:00 PM |
| Royaume-Uni | 12 ou 24 heures | 14:30 ou 2:30 PM |
| Espagne | 24 heures | 14:30:00 |

### Séparateurs d'heure

| Pays | Séparateur | Exemple |
|------|------------|---------|
| France | `:` | 14:30:45 |
| Allemagne | `:` ou `.` | 14:30:45 ou 14.30.45 |
| États-Unis | `:` | 2:30:45 PM |

### Formatage des heures en Delphi

```pascal
uses
  System.SysUtils;

var
  MonHeure: TDateTime;
  HeureStr: string;
begin
  MonHeure := EncodeTime(14, 30, 45, 0); // 14:30:45

  // Format court (selon culture système)
  HeureStr := TimeToStr(MonHeure);
  // France : "14:30:45"
  // USA    : "2:30:45 PM"

  // Format personnalisé
  HeureStr := FormatDateTime('hh:nn:ss', MonHeure);  // "14:30:45"
  HeureStr := FormatDateTime('hh:nn', MonHeure);     // "14:30"
  HeureStr := FormatDateTime('h:nn am/pm', MonHeure); // "2:30 pm"
end;
```

### Spécificateurs de format pour les heures

| Spécificateur | Description | Exemple |
|---------------|-------------|---------|
| `h` | Heure sans zéro initial | 2 |
| `hh` | Heure avec zéro initial | 02 ou 14 |
| `n` | Minute sans zéro initial | 5 |
| `nn` | Minute avec zéro initial | 05 |
| `s` | Seconde sans zéro initial | 7 |
| `ss` | Seconde avec zéro initial | 07 |
| `z` | Milliseconde sans zéros | 5 |
| `zzz` | Milliseconde avec zéros | 005 |
| `am/pm` | Indicateur AM/PM | am ou pm |
| `a/p` | Indicateur A/P | a ou p |
| `tt` | Heure format court | 14:30 |
| `tttttt` | Heure format long | 14:30:45 |

### Création et décomposition d'heures

```pascal
uses
  System.SysUtils, System.DateUtils;

var
  MonHeure: TDateTime;
  Heure, Minute, Seconde, Milliseconde: Word;
begin
  // Créer une heure
  MonHeure := EncodeTime(14, 30, 45, 500); // 14:30:45.500

  // Décomposer une heure
  DecodeTime(MonHeure, Heure, Minute, Seconde, Milliseconde);
  ShowMessage(Format('%d:%d:%d.%d', [Heure, Minute, Seconde, Milliseconde]));

  // Méthodes alternatives
  Heure := HourOf(MonHeure);
  Minute := MinuteOf(MonHeure);
  Seconde := SecondOf(MonHeure);
end;
```

## Formats de nombres selon les cultures

### Séparateurs décimaux et de milliers

| Pays | Séparateur décimal | Séparateur de milliers | Exemple |
|------|-------------------|------------------------|---------|
| France | `,` (virgule) | ` ` (espace) | 1 234,56 |
| États-Unis | `.` (point) | `,` (virgule) | 1,234.56 |
| Allemagne | `,` (virgule) | `.` (point) | 1.234,56 |
| Suisse | `.` (point) | `'` (apostrophe) | 1'234.56 |
| Inde | `.` (point) | `,` (virgule) | 1,23,456.78 |

> ⚠️ **Attention** : La même notation peut avoir des significations différentes !
> - **1.234** = mille deux cent trente-quatre aux USA
> - **1.234** = un virgule deux trois quatre en Allemagne

### Formatage des nombres en Delphi

```pascal
uses
  System.SysUtils;

var
  Nombre: Double;
  NombreStr: string;
begin
  Nombre := 1234.56;

  // Format selon la culture système
  NombreStr := FloatToStr(Nombre);
  // France : "1234,56"
  // USA    : "1234.56"

  // Format avec spécification de décimales
  NombreStr := FloatToStrF(Nombre, ffFixed, 15, 2);
  // "1234,56" (France) ou "1234.56" (USA)

  // Format avec séparateur de milliers
  NombreStr := FormatFloat('#,##0.00', Nombre);
  // France : "1 234,56"
  // USA    : "1,234.56"
end;
```

### Spécificateurs de format pour les nombres

| Spécificateur | Description | Exemple (1234.5) |
|---------------|-------------|------------------|
| `0` | Chiffre obligatoire | `0000.00` → 1234.50 |
| `#` | Chiffre optionnel | `####.##` → 1234.5 |
| `,` | Séparateur de milliers | `#,##0` → 1,234 |
| `.` | Séparateur décimal | `0.00` → 1234.50 |
| `;` | Séparateur de format | `#,##0;(#,##0)` |
| `E+` ou `E-` | Notation scientifique | `0.00E+00` → 1.23E+03 |

### Exemples de formatage

```pascal
var
  Valeur: Double;
  Resultat: string;
begin
  Valeur := 1234.567;

  // Différents formats
  Resultat := FormatFloat('0', Valeur);           // "1235" (arrondi)
  Resultat := FormatFloat('0.00', Valeur);        // "1234.57"
  Resultat := FormatFloat('#,##0.00', Valeur);    // "1,234.57" (USA)
  Resultat := FormatFloat('#,##0.##', Valeur);    // "1,234.57"
  Resultat := FormatFloat('0.00E+00', Valeur);    // "1.23E+03"

  // Format avec valeurs négatives
  Valeur := -1234.56;
  Resultat := FormatFloat('#,##0.00;(#,##0.00)', Valeur);
  // "(1,234.56)" - entre parenthèses pour les négatifs
end;
```

## Formats monétaires

### Symboles monétaires selon les cultures

| Pays | Devise | Symbole | Position | Exemple |
|------|--------|---------|----------|---------|
| France | Euro | € | Après, avec espace | 1 234,56 € |
| Allemagne | Euro | € | Après, avec espace | 1.234,56 € |
| États-Unis | Dollar | $ | Avant, sans espace | $1,234.56 |
| Royaume-Uni | Livre | £ | Avant, sans espace | £1,234.56 |
| Japon | Yen | ¥ | Avant, sans espace | ¥1,234 |
| Suisse | Franc | CHF | Avant, avec espace | CHF 1'234.56 |

### Formatage monétaire en Delphi

```pascal
uses
  System.SysUtils;

var
  Montant: Double;
  MontantStr: string;
begin
  Montant := 1234.56;

  // Format monétaire selon la culture système
  MontantStr := CurrToStr(Montant);
  // France : "1 234,56 €"
  // USA    : "$1,234.56"

  // Format monétaire avec devise spécifique
  MontantStr := FormatCurr('#,##0.00 €', Montant);
  // "1 234,56 €" (France) ou "1,234.56 €" (USA - avec séparateurs US)
end;
```

### Format avec gestion des valeurs négatives

```pascal
var
  Solde: Currency;
  SoldeStr: string;
begin
  Solde := -1234.56;

  // Différentes présentations des valeurs négatives
  SoldeStr := FormatCurr('#,##0.00 €;-#,##0.00 €', Solde);
  // "-1 234,56 €"

  SoldeStr := FormatCurr('#,##0.00 €;(#,##0.00 €)', Solde);
  // "(1 234,56 €)" - entre parenthèses

  SoldeStr := FormatCurr('#,##0.00 €;-#,##0.00 € DR', Solde);
  // "-1 234,56 € DR" - avec indicateur "DR" (débit)
end;
```

## FormatSettings : Paramètres régionaux

Delphi utilise la structure `TFormatSettings` pour gérer tous les paramètres de formatage.

### Structure TFormatSettings

```pascal
type
  TFormatSettings = record
    // Séparateurs
    DecimalSeparator: Char;        // ',' ou '.'
    ThousandSeparator: Char;       // ' ', ',', '.' ou '''
    DateSeparator: Char;           // '/', '-' ou '.'
    TimeSeparator: Char;           // ':' ou '.'

    // Formats de date
    ShortDateFormat: string;       // 'dd/mm/yyyy'
    LongDateFormat: string;        // 'dddd dd mmmm yyyy'

    // Formats d'heure
    ShortTimeFormat: string;       // 'hh:nn'
    LongTimeFormat: string;        // 'hh:nn:ss'
    TimeAMString: string;          // 'AM'
    TimePMString: string;          // 'PM'

    // Formats monétaires
    CurrencyString: string;        // '€' ou '$'
    CurrencyFormat: Byte;          // Position du symbole
    CurrencyDecimals: Byte;        // Nombre de décimales

    // Noms des jours et mois
    LongDayNames: array[1..7] of string;
    ShortDayNames: array[1..7] of string;
    LongMonthNames: array[1..12] of string;
    ShortMonthNames: array[1..12] of string;
  end;
```

### Obtenir les paramètres système

```pascal
uses
  System.SysUtils;

var
  Settings: TFormatSettings;
begin
  // Obtenir les paramètres du système
  Settings := TFormatSettings.Create;

  // Afficher les paramètres
  ShowMessage('Séparateur décimal : ' + Settings.DecimalSeparator);
  ShowMessage('Séparateur de milliers : ' + Settings.ThousandSeparator);
  ShowMessage('Format de date court : ' + Settings.ShortDateFormat);
  ShowMessage('Symbole monétaire : ' + Settings.CurrencyString);
end;
```

### Créer des paramètres personnalisés

```pascal
uses
  System.SysUtils;

function CreerFormatFrancais: TFormatSettings;
begin
  Result := TFormatSettings.Create;

  // Séparateurs
  Result.DecimalSeparator := ',';
  Result.ThousandSeparator := ' ';
  Result.DateSeparator := '/';
  Result.TimeSeparator := ':';

  // Formats
  Result.ShortDateFormat := 'dd/mm/yyyy';
  Result.LongDateFormat := 'dddd dd mmmm yyyy';
  Result.ShortTimeFormat := 'hh:nn';
  Result.LongTimeFormat := 'hh:nn:ss';

  // Monnaie
  Result.CurrencyString := '€';
  Result.CurrencyFormat := 3; // Après avec espace
  Result.CurrencyDecimals := 2;

  // Noms des jours
  Result.LongDayNames[1] := 'lundi';
  Result.LongDayNames[2] := 'mardi';
  Result.LongDayNames[3] := 'mercredi';
  Result.LongDayNames[4] := 'jeudi';
  Result.LongDayNames[5] := 'vendredi';
  Result.LongDayNames[6] := 'samedi';
  Result.LongDayNames[7] := 'dimanche';

  // Noms des mois
  Result.LongMonthNames[1] := 'janvier';
  Result.LongMonthNames[2] := 'février';
  // ... etc.
end;

// Utilisation
var
  FormatFR: TFormatSettings;
  DateStr: string;
begin
  FormatFR := CreerFormatFrancais;
  DateStr := FormatDateTime('dddd dd mmmm yyyy', Now, FormatFR);
  // "lundi 25 décembre 2024"
end;
```

### Utiliser des paramètres spécifiques

Toutes les fonctions de formatage acceptent un paramètre `TFormatSettings` optionnel :

```pascal
uses
  System.SysUtils;

var
  Nombre: Double;
  MaDate: TDateTime;
  FormatUS, FormatFR: TFormatSettings;
  ResultatUS, ResultatFR: string;
begin
  Nombre := 1234.56;
  MaDate := EncodeDate(2024, 12, 25);

  // Créer des formats pour différentes cultures
  FormatUS := TFormatSettings.Create('en-US');
  FormatFR := TFormatSettings.Create('fr-FR');

  // Formater le même nombre différemment
  ResultatUS := FloatToStr(Nombre, FormatUS); // "1234.56"
  ResultatFR := FloatToStr(Nombre, FormatFR); // "1234,56"

  // Formater la même date différemment
  ResultatUS := DateToStr(MaDate, FormatUS); // "12/25/2024"
  ResultatFR := DateToStr(MaDate, FormatFR); // "25/12/2024"
end;
```

## Conversion entre formats

### Conversion de chaînes en nombres

```pascal
uses
  System.SysUtils;

var
  TexteNombre: string;
  Nombre: Double;
  FormatFR: TFormatSettings;
begin
  FormatFR := TFormatSettings.Create('fr-FR');

  // Texte au format français
  TexteNombre := '1234,56';

  // Conversion avec format français
  Nombre := StrToFloat(TexteNombre, FormatFR); // 1234.56

  // Gestion sécurisée des erreurs
  if TryStrToFloat(TexteNombre, Nombre, FormatFR) then
    ShowMessage('Conversion réussie : ' + FloatToStr(Nombre))
  else
    ShowMessage('Erreur de conversion');
end;
```

### Conversion de chaînes en dates

```pascal
uses
  System.SysUtils;

var
  TexteDate: string;
  MaDate: TDateTime;
  FormatFR, FormatUS: TFormatSettings;
begin
  FormatFR := TFormatSettings.Create('fr-FR');
  FormatUS := TFormatSettings.Create('en-US');

  // Date au format français
  TexteDate := '25/12/2024';
  MaDate := StrToDate(TexteDate, FormatFR); // 25 décembre 2024

  // Date au format américain
  TexteDate := '12/25/2024';
  MaDate := StrToDate(TexteDate, FormatUS); // 25 décembre 2024

  // Gestion sécurisée
  if TryStrToDate(TexteDate, MaDate, FormatUS) then
    ShowMessage('Date valide')
  else
    ShowMessage('Format de date invalide');
end;
```

## Formats pour les échanges de données

### Format ISO 8601 pour les dates

Le format ISO 8601 est le standard international pour représenter les dates et heures de manière non ambiguë.

```pascal
uses
  System.SysUtils, System.DateUtils;

var
  MaDate: TDateTime;
  DateISO: string;
begin
  MaDate := EncodeDate(2024, 12, 25);

  // Format ISO : YYYY-MM-DD
  DateISO := FormatDateTime('yyyy-mm-dd', MaDate); // "2024-12-25"

  // Avec heure : YYYY-MM-DDTHH:NN:SS
  DateISO := FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now);
  // "2024-12-25T14:30:45"

  // Avec fuseau horaire
  DateISO := DateToISO8601(Now);
  // "2024-12-25T14:30:45+01:00"

  // Conversion inverse
  MaDate := ISO8601ToDate(DateISO);
end;
```

### Format invariant pour les nombres

Pour les échanges de données (fichiers, API, bases de données), utilisez toujours un format invariant :

```pascal
uses
  System.SysUtils;

var
  Nombre: Double;
  TexteInvariant: string;
  FormatInvariant: TFormatSettings;
begin
  Nombre := 1234.56;

  // Créer un format invariant (notation américaine)
  FormatInvariant := TFormatSettings.Create('en-US');
  FormatInvariant.DecimalSeparator := '.';
  FormatInvariant.ThousandSeparator := #0; // Pas de séparateur

  // Conversion en texte invariant
  TexteInvariant := FloatToStr(Nombre, FormatInvariant); // "1234.56"

  // Reconversion
  Nombre := StrToFloat(TexteInvariant, FormatInvariant);
end;
```

## Gestion pratique dans une application

### Classe de gestion des formats

```pascal
unit GestionnaireFormats;

interface

uses
  System.SysUtils;

type
  TGestionnaireFormats = class
  private
    FFormatActuel: TFormatSettings;
    FFormatInvariant: TFormatSettings;
  public
    constructor Create;

    // Formatage pour l'affichage
    function FormaterNombre(Valeur: Double; Decimales: Integer = 2): string;
    function FormaterMonnaie(Valeur: Currency): string;
    function FormaterDate(LaDate: TDateTime; FormatLong: Boolean = False): string;
    function FormaterHeure(LHeure: TDateTime; FormatLong: Boolean = False): string;

    // Conversion depuis l'entrée utilisateur
    function ConvertirEnNombre(const Texte: string): Double;
    function ConvertirEnDate(const Texte: string): TDateTime;

    // Pour les échanges de données
    function NombreVersInvariant(Valeur: Double): string;
    function InvariantVersNombre(const Texte: string): Double;
    function DateVersISO(LaDate: TDateTime): string;
    function ISOVersDate(const TexteISO: string): TDateTime;

    property FormatActuel: TFormatSettings read FFormatActuel write FFormatActuel;
  end;

implementation

uses
  System.DateUtils;

constructor TGestionnaireFormats.Create;
begin
  inherited;
  // Format selon la culture système
  FFormatActuel := TFormatSettings.Create;

  // Format invariant pour les échanges
  FFormatInvariant := TFormatSettings.Create('en-US');
  FFormatInvariant.DecimalSeparator := '.';
  FFormatInvariant.ThousandSeparator := #0;
end;

function TGestionnaireFormats.FormaterNombre(Valeur: Double; Decimales: Integer): string;
var
  Format: string;
begin
  Format := '#,##0.' + StringOfChar('0', Decimales);
  Result := FormatFloat(Format, Valeur, FFormatActuel);
end;

function TGestionnaireFormats.FormaterMonnaie(Valeur: Currency): string;
begin
  Result := CurrToStrF(Valeur, ffCurrency, 2, FFormatActuel);
end;

function TGestionnaireFormats.FormaterDate(LaDate: TDateTime; FormatLong: Boolean): string;
begin
  if FormatLong then
    Result := FormatDateTime(FFormatActuel.LongDateFormat, LaDate, FFormatActuel)
  else
    Result := FormatDateTime(FFormatActuel.ShortDateFormat, LaDate, FFormatActuel);
end;

function TGestionnaireFormats.FormaterHeure(LHeure: TDateTime; FormatLong: Boolean): string;
begin
  if FormatLong then
    Result := FormatDateTime(FFormatActuel.LongTimeFormat, LHeure, FFormatActuel)
  else
    Result := FormatDateTime(FFormatActuel.ShortTimeFormat, LHeure, FFormatActuel);
end;

function TGestionnaireFormats.ConvertirEnNombre(const Texte: string): Double;
begin
  if not TryStrToFloat(Texte, Result, FFormatActuel) then
    raise Exception.CreateFmt('"%s" n''est pas un nombre valide', [Texte]);
end;

function TGestionnaireFormats.ConvertirEnDate(const Texte: string): TDateTime;
begin
  if not TryStrToDate(Texte, Result, FFormatActuel) then
    raise Exception.CreateFmt('"%s" n''est pas une date valide', [Texte]);
end;

function TGestionnaireFormats.NombreVersInvariant(Valeur: Double): string;
begin
  Result := FloatToStr(Valeur, FFormatInvariant);
end;

function TGestionnaireFormats.InvariantVersNombre(const Texte: string): Double;
begin
  Result := StrToFloat(Texte, FFormatInvariant);
end;

function TGestionnaireFormats.DateVersISO(LaDate: TDateTime): string;
begin
  Result := DateToISO8601(LaDate);
end;

function TGestionnaireFormats.ISOVersDate(const TexteISO: string): TDateTime;
begin
  Result := ISO8601ToDate(TexteISO);
end;

end.
```

### Utilisation dans un formulaire

```pascal
uses
  GestionnaireFormats;

var
  Formats: TGestionnaireFormats;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Formats := TGestionnaireFormats.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Formats.Free;
end;

procedure TForm1.AfficherDonnees;
var
  Prix: Currency;
  DateCommande: TDateTime;
  Quantite: Double;
begin
  Prix := 1234.56;
  DateCommande := Now;
  Quantite := 15.5;

  // Afficher avec le format approprié
  LblPrix.Caption := Formats.FormaterMonnaie(Prix);
  LblDate.Caption := Formats.FormaterDate(DateCommande, True);
  LblQuantite.Caption := Formats.FormaterNombre(Quantite, 2);
end;

procedure TForm1.EnregistrerDonnees;
var
  PrixSaisi: Currency;
  DateSaisie: TDateTime;
begin
  try
    // Convertir depuis les saisies utilisateur
    PrixSaisi := Formats.ConvertirEnNombre(EditPrix.Text);
    DateSaisie := Formats.ConvertirEnDate(EditDate.Text);

    // Pour enregistrer en base de données, utiliser format invariant
    ExecuterSQL('INSERT INTO Commandes (Prix, DateCmd) VALUES (?, ?)',
      [Formats.NombreVersInvariant(PrixSaisi),
       Formats.DateVersISO(DateSaisie)]);

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Validation des saisies utilisateur

### Masque de saisie pour les dates

```pascal
uses
  Vcl.Mask;

procedure TForm1.ConfigurerMasqueDateFrancais;
begin
  // Format français : JJ/MM/AAAA
  MaskEditDate.EditMask := '!99/99/0000;1;_';
  MaskEditDate.Text := FormatDateTime('dd/mm/yyyy', Date);
end;

procedure TForm1.ConfigurerMasqueDateAmericain;
begin
  // Format américain : MM/JJ/AAAA
  MaskEditDate.EditMask := '!99/99/0000;1;_';
  MaskEditDate.Text := FormatDateTime('mm/dd/yyyy', Date);
end;
```

### Validation à la saisie

```pascal
procedure TForm1.EditNombreKeyPress(Sender: TObject; var Key: Char);
var
  SeparateurDecimal: Char;
begin
  SeparateurDecimal := FormatSettings.DecimalSeparator;

  // Autoriser uniquement les chiffres, le séparateur décimal et les touches de contrôle
  if not CharInSet(Key, ['0'..'9', SeparateurDecimal, #8, #13]) then
    Key := #0; // Ignorer la touche

  // Un seul séparateur décimal autorisé
  if (Key = SeparateurDecimal) and (Pos(SeparateurDecimal, EditNombre.Text) > 0) then
    Key := #0;
end;
```

## Bonnes pratiques

### Règles d'or pour les formats

| Règle | Description | Exemple |
|-------|-------------|---------|
| **Affichage** | Utiliser les formats de la culture utilisateur | `DateToStr(Date)` |
| **Saisie** | Accepter les formats de la culture utilisateur | `StrToDate(Edit.Text)` |
| **Stockage** | Toujours utiliser un format invariant | `DateToISO8601(Date)` |
| **Échange** | ISO 8601 pour dates, point pour décimales | `"2024-12-25"`, `"1234.56"` |
| **Validation** | Vérifier la validité avec TryStrTo... | `TryStrToFloat(s, v)` |

### Checklist de vérification

Avant de déployer votre application internationale :

- [ ] Les dates s'affichent correctement dans toutes les langues
- [ ] Les nombres utilisent les bons séparateurs
- [ ] Les montants monétaires ont le bon symbole et format
- [ ] Les heures respectent le format 12h/24h de la culture
- [ ] Les saisies utilisateur sont validées selon la culture
- [ ] Les données sont stockées en format invariant
- [ ] Les échanges utilisent ISO 8601 pour les dates
- [ ] Tous les TryStrTo... sont utilisés pour la conversion
- [ ] Les masques de saisie sont adaptés à chaque culture

### Pièges courants à éviter

#### 1. Confusion entre séparateurs

```pascal
// ❌ MAUVAIS : Format en dur
TexteNombre := '1234.56'; // Échouera en France
Nombre := StrToFloat(TexteNombre);

// ✅ BON : Utiliser le format approprié
Nombre := StrToFloat(TexteNombre, FormatSettings);
```

#### 2. Dates ambiguës

```pascal
// ❌ MAUVAIS : Format ambigu
TexteDate := '01/05/2024'; // 1er mai ou 5 janvier ?

// ✅ BON : Utiliser ISO 8601
TexteDate := '2024-05-01'; // Sans ambiguïté
```

#### 3. Stockage en format local

```pascal
// ❌ MAUVAIS : Stocker avec format local
SQL := 'INSERT INTO Table (Prix) VALUES (' + FloatToStr(Prix) + ')';
// Échouera si le serveur utilise un format différent

// ✅ BON : Format invariant avec paramètres
SQL := 'INSERT INTO Table (Prix) VALUES (:Prix)';
Query.ParamByName('Prix').AsFloat := Prix; // Delphi gère automatiquement
```

## Exemple complet d'application

Voici un exemple d'application gérant correctement les formats :

```pascal
unit FormulairePrincipal;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.StdCtrls, Vcl.Controls,
  GestionnaireFormats;

type
  TFormPrincipal = class(TForm)
    LblDate: TLabel;
    LblHeure: TLabel;
    LblPrix: TLabel;
    LblQuantite: TLabel;
    BtnAfficher: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnAfficherClick(Sender: TObject);
  private
    FFormats: TGestionnaireFormats;
    procedure AfficherInformations;
  end;

implementation

{$R *.dfm}

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FFormats := TGestionnaireFormats.Create;
  AfficherInformations;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FFormats.Free;
end;

procedure TFormPrincipal.BtnAfficherClick(Sender: TObject);
begin
  AfficherInformations;
end;

procedure TFormPrincipal.AfficherInformations;
var
  Maintenant: TDateTime;
  Prix: Currency;
  Quantite: Double;
begin
  Maintenant := Now;
  Prix := 1234.56;
  Quantite := 15.75;

  // Afficher avec les formats appropriés
  LblDate.Caption := 'Date : ' + FFormats.FormaterDate(Maintenant, True);
  LblHeure.Caption := 'Heure : ' + FFormats.FormaterHeure(Maintenant, True);
  LblPrix.Caption := 'Prix : ' + FFormats.FormaterMonnaie(Prix);
  LblQuantite.Caption := 'Quantité : ' + FFormats.FormaterNombre(Quantite, 2);

  // Afficher aussi les formats invariants (pour info)
  ShowMessage(Format(
    'Formats invariants :'#13#10 +
    'Date ISO : %s'#13#10 +
    'Prix : %s',
    [FFormats.DateVersISO(Maintenant),
     FFormats.NombreVersInvariant(Prix)]
  ));
end;

end.
```

## Conclusion

La gestion correcte des formats de date, heure et nombres est essentielle pour créer des applications vraiment internationales. Delphi offre tous les outils nécessaires via `TFormatSettings` et ses fonctions de formatage.

**Points clés à retenir :**

- Toujours utiliser les fonctions de formatage de Delphi, jamais de concaténations manuelles
- Différencier l'affichage (format utilisateur) du stockage (format invariant)
- Utiliser ISO 8601 pour les échanges de dates
- Valider les saisies avec les fonctions TryStrTo... appropriées
- Tester votre application dans différentes configurations régionales
- Créer une classe de gestion centralisée pour plus de cohérence

Dans la prochaine section, nous verrons comment tester efficacement l'internationalisation de votre application pour garantir une expérience utilisateur optimale dans toutes les langues et cultures.

⏭️ [Tests de l'internationalisation](/13-internationalisation-et-localisation/05-tests-de-linternationalisation.md)
