🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 13.3 Adaptation à différentes langues

## Introduction

Adapter une application à différentes langues va bien au-delà de la simple traduction des textes. Il s'agit de créer une expérience utilisateur cohérente et naturelle pour chaque culture. Dans cette section, nous allons découvrir comment mettre en pratique l'internationalisation (i18n) et la localisation (l10n) dans vos applications Delphi.

## Différence entre i18n et l10n

Avant de commencer, comprenons deux termes importants :

| Terme | Signification | Description | Exemple |
|-------|---------------|-------------|---------|
| **i18n** | Internationalization | Conception de l'application pour supporter plusieurs langues | Architecture avec ressources linguistiques |
| **l10n** | Localization | Adaptation à une langue/culture spécifique | Traduction en français |

> 💡 **i18n** = "i" + 18 lettres + "n" (internationalization)
> **l10n** = "l" + 10 lettres + "n" (localization)

## Codes de langue et de culture

Delphi utilise les codes ISO standard pour identifier les langues et les cultures.

### Codes de langue (ISO 639-1)

| Code | Langue |
|------|--------|
| `fr` | Français |
| `en` | Anglais |
| `es` | Espagnol |
| `de` | Allemand |
| `it` | Italien |
| `pt` | Portugais |
| `ru` | Russe |
| `zh` | Chinois |
| `ja` | Japonais |
| `ar` | Arabe |

### Codes de culture (ISO 3166-1 + langue)

Pour être plus précis, on combine langue et pays :

| Code | Description |
|------|-------------|
| `fr-FR` | Français (France) |
| `fr-CA` | Français (Canada) |
| `fr-BE` | Français (Belgique) |
| `en-US` | Anglais (États-Unis) |
| `en-GB` | Anglais (Royaume-Uni) |
| `es-ES` | Espagnol (Espagne) |
| `es-MX` | Espagnol (Mexique) |
| `pt-PT` | Portugais (Portugal) |
| `pt-BR` | Portugais (Brésil) |

## Détection de la langue du système

Delphi peut automatiquement détecter la langue utilisée par le système d'exploitation.

### Obtenir la langue système

```pascal
uses
  System.SysUtils, Winapi.Windows;

function ObtenirLangueSysteme: string;
var
  Buffer: array[0..255] of Char;
begin
  // Récupère le code de langue du système (ex: "fr-FR")
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SNAME, Buffer, SizeOf(Buffer));
  Result := Buffer;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  LangueSysteme: string;
begin
  LangueSysteme := ObtenirLangueSysteme;
  ShowMessage('Langue du système : ' + LangueSysteme);

  // Extraire juste le code de langue (2 premiers caractères)
  if Length(LangueSysteme) >= 2 then
    ShowMessage('Code langue : ' + Copy(LangueSysteme, 1, 2));
end;
```

### Utiliser SysLocale

Une méthode plus simple utilise la variable globale `SysLocale` :

```pascal
uses
  System.SysUtils;

function CodeLangueSysteme: string;
begin
  // Récupère les 2 premiers caractères du code de langue
  Result := Copy(SysLocale.PriLangID.ToString, 1, 2);
end;
```

## Mise en place d'un sélecteur de langue

Permettre à l'utilisateur de choisir sa langue est une bonne pratique.

### Interface de sélection

```pascal
unit SelecteurLangue;

interface

uses
  System.SysUtils, System.Classes, Vcl.StdCtrls, Vcl.Forms;

type
  TLangueDisponible = record
    Code: string;
    Nom: string;
    IconeCode: string; // Code emoji du drapeau
  end;

  TSelecteurLangue = class
  private
    FLangueActive: string;
    FLanguesDisponibles: array of TLangueDisponible;
    procedure InitialiserLangues;
  public
    constructor Create;
    function ObtenirLanguesDisponibles: TArray<TLangueDisponible>;
    procedure DefinirLangue(const CodeLangue: string);
    function LangueActive: string;
    function NomLangueActive: string;
  end;

implementation

constructor TSelecteurLangue.Create;
begin
  inherited;
  InitialiserLangues;
  FLangueActive := 'fr'; // Langue par défaut
end;

procedure TSelecteurLangue.InitialiserLangues;
begin
  SetLength(FLanguesDisponibles, 5);

  FLanguesDisponibles[0].Code := 'fr';
  FLanguesDisponibles[0].Nom := 'Français';
  FLanguesDisponibles[0].IconeCode := '🇫🇷';

  FLanguesDisponibles[1].Code := 'en';
  FLanguesDisponibles[1].Nom := 'English';
  FLanguesDisponibles[1].IconeCode := '🇬🇧';

  FLanguesDisponibles[2].Code := 'es';
  FLanguesDisponibles[2].Nom := 'Español';
  FLanguesDisponibles[2].IconeCode := '🇪🇸';

  FLanguesDisponibles[3].Code := 'de';
  FLanguesDisponibles[3].Nom := 'Deutsch';
  FLanguesDisponibles[3].IconeCode := '🇩🇪';

  FLanguesDisponibles[4].Code := 'it';
  FLanguesDisponibles[4].Nom := 'Italiano';
  FLanguesDisponibles[4].IconeCode := '🇮🇹';
end;

function TSelecteurLangue.ObtenirLanguesDisponibles: TArray<TLangueDisponible>;
begin
  Result := FLanguesDisponibles;
end;

procedure TSelecteurLangue.DefinirLangue(const CodeLangue: string);
begin
  FLangueActive := CodeLangue;
end;

function TSelecteurLangue.LangueActive: string;
begin
  Result := FLangueActive;
end;

function TSelecteurLangue.NomLangueActive: string;
var
  i: Integer;
begin
  Result := FLangueActive;
  for i := 0 to High(FLanguesDisponibles) do
  begin
    if FLanguesDisponibles[i].Code = FLangueActive then
    begin
      Result := FLanguesDisponibles[i].Nom;
      Break;
    end;
  end;
end;

end.
```

### Intégration dans un formulaire

```pascal
procedure TFormPrincipale.CreerMenuLangues;
var
  MenuItem: TMenuItem;
  Langues: TArray<TLangueDisponible>;
  Langue: TLangueDisponible;
begin
  Langues := SelecteurLangue.ObtenirLanguesDisponibles;

  for Langue in Langues do
  begin
    MenuItem := TMenuItem.Create(MenuLangue);
    MenuItem.Caption := Langue.IconeCode + ' ' + Langue.Nom;
    MenuItem.Tag := Integer(Langue.Code[1]); // Stocke le code
    MenuItem.OnClick := MenuLangueClick;
    MenuLangue.Add(MenuItem);
  end;
end;

procedure TFormPrincipale.MenuLangueClick(Sender: TObject);
var
  CodeLangue: string;
begin
  // Récupérer le code de langue depuis le tag
  CodeLangue := (Sender as TMenuItem).Caption;
  // Extraire juste le code (après le drapeau)
  CodeLangue := Copy(CodeLangue, Pos(' ', CodeLangue) + 1, 2);

  // Changer la langue
  ChangerLangue(CodeLangue);
end;
```

## Changement de langue à l'exécution

Voici comment permettre à l'utilisateur de changer de langue sans redémarrer l'application.

### Méthode 1 : Rechargement manuel des textes

```pascal
procedure TFormPrincipale.ChangerLangue(const CodeLangue: string);
begin
  // Définir la nouvelle langue
  GestionnaireTraduction.DefinirLangue(CodeLangue);

  // Recharger tous les textes de l'interface
  AppliquerTraductions;

  // Sauvegarder le choix de l'utilisateur
  SauvegarderPreferenceLangue(CodeLangue);
end;

procedure TFormPrincipale.AppliquerTraductions;
begin
  // Titre de la fenêtre
  Self.Caption := T('Titre.Principale');

  // Boutons
  BtnNouveau.Caption := T('Boutons.Nouveau');
  BtnOuvrir.Caption := T('Boutons.Ouvrir');
  BtnEnregistrer.Caption := T('Boutons.Enregistrer');
  BtnFermer.Caption := T('Boutons.Fermer');

  // Labels
  LblNom.Caption := T('Libelles.Nom');
  LblPrenom.Caption := T('Libelles.Prenom');
  LblEmail.Caption := T('Libelles.Email');

  // Menu
  MenuFichier.Caption := T('Menu.Fichier');
  MenuEdition.Caption := T('Menu.Edition');
  MenuAide.Caption := T('Menu.Aide');

  // Hints (bulles d'aide)
  BtnNouveau.Hint := T('Hints.Nouveau');
  BtnOuvrir.Hint := T('Hints.Ouvrir');

  // Messages de statut
  StatusBar1.Panels[0].Text := T('Status.Pret');
end;
```

### Méthode 2 : Rechargement automatique des formulaires

Pour les applications utilisant les fichiers `.dfm` localisés :

```pascal
procedure TFormPrincipale.ChangerLangueAvecDFM(const CodeLangue: string);
var
  FichiersOuverts: TList<TForm>;
  i: Integer;
begin
  // Sauvegarder les formulaires ouverts
  FichiersOuverts := TList<TForm>.Create;
  try
    for i := 0 to Screen.FormCount - 1 do
      FichiersOuverts.Add(Screen.Forms[i]);

    // Fermer tous les formulaires sauf le principal
    for i := Screen.FormCount - 1 downto 0 do
    begin
      if Screen.Forms[i] <> Self then
        Screen.Forms[i].Close;
    end;

    // Définir la nouvelle langue
    SetCurrentLanguage(CodeLangue);

    // Recharger le formulaire principal
    // Note : en pratique, il faut souvent redémarrer l'application
    ShowMessage(T('Messages.RedemarrerPourChangerLangue'));

  finally
    FichiersOuverts.Free;
  end;
end;
```

### Méthode 3 : Avec notification aux observateurs

Pour une architecture plus propre, utilisez le pattern Observer :

```pascal
type
  IObservateurLangue = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure LangueChangee(const NouveauCode: string);
  end;

  TGestionnaireLangue = class
  private
    FObservateurs: TList<IObservateurLangue>;
    FLangueActive: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AjouterObservateur(Observateur: IObservateurLangue);
    procedure RetirerObservateur(Observateur: IObservateurLangue);
    procedure ChangerLangue(const CodeLangue: string);
  end;

// Implémentation dans un formulaire
type
  TFormPrincipale = class(TForm, IObservateurLangue)
  private
    procedure LangueChangee(const NouveauCode: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TFormPrincipale.LangueChangee(const NouveauCode: string);
begin
  // Mettre à jour automatiquement l'interface
  AppliquerTraductions;
end;
```

## Sauvegarde des préférences utilisateur

Il est important de mémoriser le choix de langue de l'utilisateur.

### Utilisation d'un fichier INI

```pascal
uses
  System.IniFiles, System.IOUtils;

const
  FICHIER_CONFIG = 'config.ini';

procedure SauvegarderPreferenceLangue(const CodeLangue: string);
var
  IniFile: TIniFile;
  CheminConfig: string;
begin
  CheminConfig := TPath.Combine(TPath.GetHomePath, FICHIER_CONFIG);
  IniFile := TIniFile.Create(CheminConfig);
  try
    IniFile.WriteString('Preferences', 'Langue', CodeLangue);
  finally
    IniFile.Free;
  end;
end;

function ChargerPreferenceLangue: string;
var
  IniFile: TIniFile;
  CheminConfig: string;
begin
  CheminConfig := TPath.Combine(TPath.GetHomePath, FICHIER_CONFIG);

  if TFile.Exists(CheminConfig) then
  begin
    IniFile := TIniFile.Create(CheminConfig);
    try
      Result := IniFile.ReadString('Preferences', 'Langue', 'fr'); // 'fr' par défaut
    finally
      IniFile.Free;
    end;
  end
  else
    Result := 'fr'; // Langue par défaut
end;

// Utilisation au démarrage de l'application
procedure TFormPrincipale.FormCreate(Sender: TObject);
var
  LanguePreferee: string;
begin
  LanguePreferee := ChargerPreferenceLangue;
  ChangerLangue(LanguePreferee);
end;
```

### Utilisation du registre Windows (Windows uniquement)

```pascal
uses
  System.Win.Registry;

procedure SauvegarderLangueDansRegistre(const CodeLangue: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\MonApplication\Preferences', True) then
    begin
      Reg.WriteString('Langue', CodeLangue);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function ChargerLangueDepuisRegistre: string;
var
  Reg: TRegistry;
begin
  Result := 'fr'; // Valeur par défaut

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly('\Software\MonApplication\Preferences') then
    begin
      if Reg.ValueExists('Langue') then
        Result := Reg.ReadString('Langue');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;
```

## Gestion de la direction du texte (RTL/LTR)

Certaines langues comme l'arabe et l'hébreu s'écrivent de droite à gauche (RTL).

### Types de direction

| Type | Description | Langues |
|------|-------------|---------|
| **LTR** | Left-to-Right (Gauche à droite) | Français, anglais, espagnol, etc. |
| **RTL** | Right-to-Left (Droite à gauche) | Arabe, hébreu, persan, ourdou |

### Activation du mode RTL

```pascal
procedure TFormPrincipale.DefinirDirectionTexte(const CodeLangue: string);
begin
  // Langues RTL
  if (CodeLangue = 'ar') or (CodeLangue = 'he') or (CodeLangue = 'fa') then
  begin
    Self.BiDiMode := bdRightToLeft;

    // Inverser l'alignement des composants
    AlignementRTL;
  end
  else
  begin
    Self.BiDiMode := bdLeftToRight;
    AlignementLTR;
  end;
end;

procedure TFormPrincipale.AlignementRTL;
begin
  // Ajuster l'alignement des labels
  LblNom.Alignment := taRightJustify;
  LblPrenom.Alignment := taRightJustify;

  // Ajuster la position des boutons
  BtnValider.Left := 10;
  BtnAnnuler.Left := BtnValider.Left + BtnValider.Width + 10;
end;

procedure TFormPrincipale.AlignementLTR;
begin
  // Rétablir l'alignement normal
  LblNom.Alignment := taLeftJustify;
  LblPrenom.Alignment := taLeftJustify;

  // Position normale des boutons
  BtnAnnuler.Left := Self.ClientWidth - BtnAnnuler.Width - 10;
  BtnValider.Left := BtnAnnuler.Left - BtnValider.Width - 10;
end;
```

## Adaptation de la mise en page

Différentes langues nécessitent différentes tailles de composants.

### Textes de longueurs variables

```
Français : "Enregistrer sous..."   (20 caractères)
Anglais  : "Save as..."            (11 caractères)
Allemand : "Speichern unter..."    (19 caractères)
Russe    : "Сохранить как..."      (16 caractères)
```

### Stratégies d'adaptation

#### 1. Composants auto-dimensionnés

```pascal
procedure TFormPrincipale.ConfigurerBoutonAutoSize;
begin
  BtnEnregistrer.AutoSize := True;
  BtnEnregistrer.Caption := T('Boutons.Enregistrer');

  // Définir une largeur minimale
  if BtnEnregistrer.Width < 100 then
    BtnEnregistrer.Width := 100;
end;
```

#### 2. Anchors (ancrages)

```pascal
procedure TFormPrincipale.ConfigurerAncrages;
begin
  // Le bouton reste collé à droite même si sa taille change
  BtnValider.Anchors := [akRight, akBottom];
  BtnAnnuler.Anchors := [akRight, akBottom];

  // Le label s'étend horizontalement
  LblTitre.Anchors := [akLeft, akTop, akRight];
end;
```

#### 3. Taille dynamique des formulaires

```pascal
procedure TFormPrincipale.AjusterTailleFormulaire;
var
  LargeurMinimale: Integer;
begin
  // Calculer la largeur nécessaire en fonction des composants
  LargeurMinimale := 400; // Largeur de base

  // Ajouter de l'espace pour les textes longs
  if GestionnaireLangue.LangueActive = 'de' then
    LargeurMinimale := LargeurMinimale + 50; // Allemand = textes plus longs

  if Self.ClientWidth < LargeurMinimale then
    Self.ClientWidth := LargeurMinimale;
end;
```

### Tableau des coefficients d'expansion

Utilisez ces coefficients pour estimer l'espace nécessaire :

| Langue | Coefficient | Exemple (10 caractères) |
|--------|-------------|-------------------------|
| Anglais (référence) | 1.0 | 10 caractères |
| Français | 1.2 | 12 caractères |
| Allemand | 1.3 | 13 caractères |
| Espagnol | 1.15 | 11-12 caractères |
| Italien | 1.2 | 12 caractères |
| Russe | 1.15 | 11-12 caractères |
| Chinois | 0.7 | 7 caractères |
| Japonais | 0.8 | 8 caractères |

## Gestion des raccourcis clavier

Les raccourcis clavier doivent être adaptés à chaque langue.

### Définition des raccourcis

```pascal
resourcestring
  // Français
  MENU_FICHIER = '&Fichier';      // Alt+F
  MENU_NOUVEAU = '&Nouveau';      // Alt+N
  MENU_OUVRIR = '&Ouvrir';       // Alt+O
  MENU_ENREGISTRER = '&Enregistrer'; // Alt+E
  MENU_QUITTER = '&Quitter';     // Alt+Q

// En anglais (fichier séparé en.ini)
// MENU_FILE = "&File"          // Alt+F
// MENU_NEW = "&New"            // Alt+N
// MENU_OPEN = "&Open"          // Alt+O
// MENU_SAVE = "&Save"          // Alt+S
// MENU_QUIT = "&Quit"          // Alt+Q
```

### Vérification des conflits

```pascal
procedure TFormPrincipale.VerifierRaccourcisClavier;
var
  Raccourcis: TDictionary<Char, string>;
  i: Integer;
  Touche: Char;
begin
  Raccourcis := TDictionary<Char, string>.Create;
  try
    // Parcourir tous les éléments de menu
    for i := 0 to MainMenu1.Items.Count - 1 do
    begin
      Touche := ExtraireRaccourci(MainMenu1.Items[i].Caption);
      if Touche <> #0 then
      begin
        if Raccourcis.ContainsKey(Touche) then
          ShowMessage('Conflit de raccourci : ' + Touche)
        else
          Raccourcis.Add(Touche, MainMenu1.Items[i].Caption);
      end;
    end;
  finally
    Raccourcis.Free;
  end;
end;

function ExtraireRaccourci(const Caption: string): Char;
var
  Pos: Integer;
begin
  Result := #0;
  Pos := System.Pos('&', Caption);
  if (Pos > 0) and (Pos < Length(Caption)) then
    Result := UpCase(Caption[Pos + 1]);
end;
```

## Tests multi-langues

### Checklist de test par langue

Créez une liste de vérification pour chaque langue :

```pascal
type
  TElementTest = record
    Nom: string;
    Verifie: Boolean;
    Commentaire: string;
  end;

  TTestsLangue = class
  private
    FElements: TArray<TElementTest>;
  public
    procedure AjouterTest(const Nom: string);
    procedure MarquerVerifie(const Nom: string; const Commentaire: string = '');
    function TousVerifies: Boolean;
    function GenererRapport: string;
  end;

// Utilisation
procedure TesterLangue(const CodeLangue: string);
var
  Tests: TTestsLangue;
begin
  Tests := TTestsLangue.Create;
  try
    Tests.AjouterTest('Tous les textes traduits');
    Tests.AjouterTest('Pas de débordement de texte');
    Tests.AjouterTest('Raccourcis clavier fonctionnels');
    Tests.AjouterTest('Direction du texte correcte');
    Tests.AjouterTest('Formats de date corrects');
    Tests.AjouterTest('Symboles monétaires corrects');

    // Effectuer les tests...

    if not Tests.TousVerifies then
      ShowMessage('Tests incomplets pour ' + CodeLangue);
  finally
    Tests.Free;
  end;
end;
```

### Mode de test automatique

```pascal
procedure TFormPrincipale.ModeTestLangues;
var
  Langues: TArray<string>;
  Langue: string;
begin
  Langues := ['fr', 'en', 'es', 'de', 'it'];

  for Langue in Langues do
  begin
    ChangerLangue(Langue);
    Application.ProcessMessages;
    Sleep(2000); // Pause de 2 secondes pour visualiser

    // Capturer une image du formulaire
    CaptureEcran('Test_' + Langue + '.png');
  end;

  ShowMessage('Test terminé. Vérifiez les captures d''écran.');
end;
```

## Cas particuliers et pièges à éviter

### 1. Textes codés en dur

**❌ Mauvais :**
```pascal
procedure TFormPrincipale.BtnSaveClick(Sender: TObject);
begin
  ShowMessage('Fichier sauvegardé');
end;
```

**✅ Bon :**
```pascal
procedure TFormPrincipale.BtnSaveClick(Sender: TObject);
begin
  ShowMessage(T('Messages.FichierSauvegarde'));
end;
```

### 2. Concaténation de chaînes

**❌ Mauvais :**
```pascal
Message := 'Vous avez ' + IntToStr(NbMessages) + ' messages';
```

L'ordre des mots diffère selon les langues !

**✅ Bon :**
```pascal
// Français : "Vous avez %d messages"
// Anglais  : "You have %d messages"
// Allemand : "Sie haben %d Nachrichten"
Message := Format(T('Messages.NombreMessages'), [NbMessages]);
```

### 3. Images contenant du texte

Évitez les images avec du texte intégré. Préférez :

- Texte séparé superposé à l'image
- Images sans texte avec légendes
- Icônes universelles (symboles sans texte)

### 4. Formats spécifiques à une culture

```pascal
// ❌ Mauvais : format fixe
DateStr := '31/12/2024';

// ✅ Bon : format selon la culture
DateStr := FormatDateTime('ddddd', Date); // Utilise le format système
```

## Exemple complet : Application multi-langue

Voici un exemple complet d'application qui gère plusieurs langues :

```pascal
unit FormulairePrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.Menus, GestionnaireTraduction;

type
  TFormMain = class(TForm)
    MainMenu1: TMainMenu;
    MenuLangue: TMenuItem;
    BtnAction: TButton;
    LblInformation: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure CreerMenuLangues;
    procedure ChangerLangue(const CodeLangue: string);
    procedure AppliquerTraductions;
    procedure MenuLangueClick(Sender: TObject);
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
var
  LangueSauvegardee: string;
begin
  // Créer le menu de sélection des langues
  CreerMenuLangues;

  // Charger la langue préférée de l'utilisateur
  LangueSauvegardee := ChargerPreferenceLangue;

  // Si aucune préférence, utiliser la langue du système
  if LangueSauvegardee = '' then
    LangueSauvegardee := Copy(ObtenirLangueSysteme, 1, 2);

  // Appliquer la langue
  ChangerLangue(LangueSauvegardee);
end;

procedure TFormMain.CreerMenuLangues;
var
  MenuItem: TMenuItem;
  Langues: array[0..4] of record
    Code: string;
    Nom: string;
  end;
  i: Integer;
begin
  // Définir les langues disponibles
  Langues[0].Code := 'fr'; Langues[0].Nom := '🇫🇷 Français';
  Langues[1].Code := 'en'; Langues[1].Nom := '🇬🇧 English';
  Langues[2].Code := 'es'; Langues[2].Nom := '🇪🇸 Español';
  Langues[3].Code := 'de'; Langues[3].Nom := '🇩🇪 Deutsch';
  Langues[4].Code := 'it'; Langues[4].Nom := '🇮🇹 Italiano';

  // Créer les éléments de menu
  for i := 0 to High(Langues) do
  begin
    MenuItem := TMenuItem.Create(MenuLangue);
    MenuItem.Caption := Langues[i].Nom;
    MenuItem.Tag := i;
    MenuItem.OnClick := MenuLangueClick;
    MenuLangue.Add(MenuItem);
  end;
end;

procedure TFormMain.MenuLangueClick(Sender: TObject);
const
  CodesLangues: array[0..4] of string = ('fr', 'en', 'es', 'de', 'it');
var
  Index: Integer;
begin
  Index := (Sender as TMenuItem).Tag;
  if (Index >= 0) and (Index <= High(CodesLangues)) then
    ChangerLangue(CodesLangues[Index]);
end;

procedure TFormMain.ChangerLangue(const CodeLangue: string);
begin
  // Définir la nouvelle langue dans le gestionnaire
  GestionnaireTraduction.DefinirLangue(CodeLangue);

  // Appliquer les traductions à l'interface
  AppliquerTraductions;

  // Sauvegarder la préférence
  SauvegarderPreferenceLangue(CodeLangue);
end;

procedure TFormMain.AppliquerTraductions;
begin
  // Titre du formulaire
  Self.Caption := T('App.Titre');

  // Menu
  MenuLangue.Caption := T('Menu.Langue');

  // Composants
  BtnAction.Caption := T('Boutons.Executer');
  LblInformation.Caption := T('Libelles.Information');

  // Ajuster la taille si nécessaire
  BtnAction.AutoSize := True;
end;

end.
```

## Bonnes pratiques récapitulatives

| Bonne pratique | Description |
|----------------|-------------|
| **Tout externaliser** | Aucun texte visible en dur dans le code |
| **Tester tôt** | Vérifier l'affichage dans toutes les langues dès le début |
| **Format() au lieu de +** | Utiliser Format() pour assembler des chaînes |
| **Auto-size** | Activer AutoSize sur les composants quand c'est possible |
| **RTL/LTR** | Prévoir le support des langues RTL dès la conception |
| **Icônes universelles** | Privilégier les symboles aux textes dans les images |
| **Sauvegarder les préférences** | Mémoriser le choix de langue de l'utilisateur |
| **Langue système par défaut** | Détecter et utiliser la langue du système |

## Conclusion

L'adaptation à différentes langues est un processus qui demande de la planification et de l'attention aux détails. En suivant les bonnes pratiques présentées dans cette section, vous pourrez créer des applications véritablement internationales qui offrent une expérience utilisateur de qualité dans chaque langue.

**Points clés à retenir :**
- Détectez la langue du système par défaut
- Permettez à l'utilisateur de changer de langue facilement
- Sauvegardez les préférences utilisateur
- Adaptez la mise en page aux différentes longueurs de texte
- Testez systématiquement dans toutes les langues supportées
- Prévoyez le support des écritures RTL si nécessaire

Dans la prochaine section, nous verrons comment gérer les formats de date, heure et nombres qui varient également selon les cultures.

⏭️ [Formats de date, heure et nombres](/13-internationalisation-et-localisation/04-formats-de-date-heure-et-nombres.md)
