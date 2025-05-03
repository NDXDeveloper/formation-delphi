# 13.2 Ressources linguistiques

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Pour créer des applications multilingues, il est essentiel de séparer les chaînes de caractères de votre code source. Delphi propose plusieurs mécanismes pour gérer les ressources linguistiques, permettant ainsi une internationalisation efficace de vos applications.

## Pourquoi utiliser des ressources linguistiques ?

Avant de plonger dans les détails techniques, comprenons pourquoi les ressources linguistiques sont importantes :

1. **Séparation des préoccupations** : Le code et le texte affiché sont séparés, facilitant la maintenance
2. **Internationalisation simplifiée** : Ajout facile de nouvelles langues sans modifier le code
3. **Cohérence** : Gestion centralisée des textes pour une uniformité dans l'application
4. **Productivité** : Possibilité de confier la traduction à des spécialistes qui n'ont pas besoin de connaître le code

## Méthodes de gestion des ressources linguistiques

Delphi offre plusieurs approches pour gérer les ressources linguistiques :

### 1. Fichiers de ressources (.rc et .res)

Les fichiers de ressources sont la méthode traditionnelle pour stocker des chaînes et d'autres ressources :

#### Création d'un fichier de ressources (.rc)

Créez un fichier texte avec l'extension `.rc` contenant vos chaînes :

```
STRINGTABLE
BEGIN
  1, "Fichier"
  2, "Édition"
  3, "Affichage"
  4, "Outils"
  5, "Aide"
END
```

#### Compilation du fichier de ressources

Utilisez l'utilitaire BRCC32.EXE pour compiler votre fichier .rc en fichier .res :

```
brcc32 strings.rc
```

Vous pouvez également inclure cette commande dans votre projet pour que la compilation se fasse automatiquement.

#### Utilisation dans votre code

```pascal
{$R strings.res}  // Inclure le fichier de ressources

procedure TForm1.FormCreate(Sender: TObject);
var
  Buffer: array[0..255] of Char;
begin
  // Chargement d'une chaîne depuis les ressources
  LoadString(HInstance, 1, Buffer, SizeOf(Buffer));

  // Utilisation de la chaîne
  MenuItem1.Caption := Buffer;
end;
```

### 2. Fichiers DFM pour les formulaires

Les propriétés des composants VCL comme `Caption`, `Text`, etc. sont stockées dans les fichiers DFM, qui peuvent être localisés :

```pascal
object Form1: TForm1
  Caption = 'Mon application'
  ...
  object Label1: TLabel
    Caption = 'Bonjour !'
  end
end
```

### 3. Fichiers de ressources de chaînes (.dfm)

Delphi permet de créer des modules de données contenant uniquement des chaînes :

#### Création d'un module de données de chaînes

1. Choisissez **Fichier > Nouveau > Autre > Module de données**
2. Ajoutez des composants `TStringList` ou `TMemo` pour stocker vos chaînes
3. Définissez les propriétés `Name` et `Strings` pour chaque composant

```pascal
object StringRes: TStringRes
  object StringList1: TStringList
    Strings.Strings = (
      'Bonjour'
      'Au revoir'
      'Fichier'
      'Édition'
    )
  end
end
```

#### Utilisation dans votre code

```pascal
// Utilisation du module de données
uses StringRes;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Label1.Caption := StringRes.StringList1[0];  // "Bonjour"
end;
```

### 4. Fichiers de ressources externes (.txt ou .ini)

Pour une plus grande flexibilité, vous pouvez utiliser des fichiers externes :

#### Création d'un fichier de ressources .ini

```ini
[French]
Greeting=Bonjour
Farewell=Au revoir

[English]
Greeting=Hello
Farewell=Goodbye
```

#### Utilisation avec TIniFile

```pascal
uses IniFiles;

procedure TForm1.LoadLanguage(const Language: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.ExeName) + 'languages.ini');
  try
    Label1.Caption := IniFile.ReadString(Language, 'Greeting', 'Bonjour');
    Label2.Caption := IniFile.ReadString(Language, 'Farewell', 'Au revoir');
  finally
    IniFile.Free;
  end;
end;
```

## La classe TResourceStream

Pour des ressources plus complexes ou volumineuses, vous pouvez utiliser `TResourceStream` :

```pascal
uses Classes;

procedure TForm1.LoadResourceText;
var
  ResStream: TResourceStream;
  StrList: TStringList;
begin
  ResStream := TResourceStream.Create(HInstance, 'TEXTFILE', RT_RCDATA);
  try
    StrList := TStringList.Create;
    try
      StrList.LoadFromStream(ResStream);
      Memo1.Lines.Text := StrList.Text;
    finally
      StrList.Free;
    end;
  finally
    ResStream.Free;
  end;
end;
```

## Utilisation avancée : La bibliothèque GNU gettext

Pour des projets plus complexes, vous pouvez utiliser la bibliothèque GNU gettext, qui offre des fonctionnalités avancées pour l'internationalisation :

> 💡 Il existe plusieurs adaptations de GNU gettext pour Delphi, comme "dxgettext" ou "GnuGetText.pas".

### Installation de GNU gettext pour Delphi

1. Téléchargez une implémentation de GNU gettext pour Delphi
2. Ajoutez le répertoire d'installation à votre chemin de recherche de projet

### Utilisation de base

```pascal
uses
  gnugettext;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisation de gettext
  TranslateComponent(Self);

  // Traduction de chaînes individuelles
  Label1.Caption := _('Bonjour');  // La fonction _() recherche la traduction
end;
```

### Fichiers de traduction (.po et .mo)

GNU gettext utilise des fichiers .po (Portable Object) pour les traductions et des fichiers .mo (Machine Object) pour le runtime :

Exemple de fichier .po :
```
msgid "Bonjour"
msgstr "Hello"

msgid "Au revoir"
msgstr "Goodbye"
```

## Bibliothèques tierces pour la localisation

Plusieurs bibliothèques tierces offrent des solutions plus avancées pour la gestion des ressources linguistiques :

- **ITE (Integrated Translation Environment)**
- **TsiLang**
- **MultiLanguage Component**

Ces bibliothèques payantes offrent généralement des éditeurs visuels et des outils pour faciliter la gestion des traductions.

## Mise en œuvre pratique : Un gestionnaire de langues simple

Voici un exemple d'implémentation d'un gestionnaire de langues simple basé sur des fichiers INI :

```pascal
unit LanguageManager;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles;

type
  TLanguageManager = class
  private
    FLanguageFile: string;
    FCurrentLanguage: string;
    FIniFile: TIniFile;
    function GetString(const Identifier: string): string;
  public
    constructor Create(const ALanguageFile: string);
    destructor Destroy; override;
    procedure SetLanguage(const Language: string);
    property CurrentLanguage: string read FCurrentLanguage;
    property Strings[const Identifier: string]: string read GetString; default;
  end;

implementation

constructor TLanguageManager.Create(const ALanguageFile: string);
begin
  inherited Create;
  FLanguageFile := ALanguageFile;
  FIniFile := TIniFile.Create(FLanguageFile);
  // Langue par défaut
  FCurrentLanguage := 'French';
end;

destructor TLanguageManager.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TLanguageManager.SetLanguage(const Language: string);
begin
  FCurrentLanguage := Language;
end;

function TLanguageManager.GetString(const Identifier: string): string;
begin
  Result := FIniFile.ReadString(FCurrentLanguage, Identifier, Identifier);
end;

end.
```

### Utilisation du gestionnaire de langues

```pascal
var
  LangManager: TLanguageManager;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LangManager := TLanguageManager.Create(ExtractFilePath(Application.ExeName) + 'languages.ini');

  // Définir la langue
  LangManager.SetLanguage('French');

  // Utiliser les chaînes
  Button1.Caption := LangManager['BtnOK'];
  Label1.Caption := LangManager['Welcome'];
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LangManager.Free;
end;

procedure TForm1.cbLanguageChange(Sender: TObject);
begin
  case cbLanguage.ItemIndex of
    0: LangManager.SetLanguage('French');
    1: LangManager.SetLanguage('English');
    2: LangManager.SetLanguage('Spanish');
  end;

  // Mettre à jour les chaînes
  UpdateUILanguage;
end;

procedure TForm1.UpdateUILanguage;
begin
  // Mettre à jour toutes les chaînes de l'interface
  Button1.Caption := LangManager['BtnOK'];
  Label1.Caption := LangManager['Welcome'];
  // ... autres composants
end;
```

## Bonnes pratiques pour la gestion des ressources linguistiques

1. **Planifiez l'internationalisation dès le début** : Ne considérez pas la localisation comme une fonctionnalité à ajouter plus tard

2. **Utilisez des identifiants clairs** : Nommez vos chaînes de façon descriptive, par exemple `msg_welcome` plutôt que `str1`

3. **Évitez les chaînes concaténées** : Utilisez des paramètres pour les chaînes variables
   ```pascal
   // Mauvais
   Label1.Caption := 'Bonjour ' + UserName;

   // Bon
   Label1.Caption := Format(LangManager['msg_welcome'], [UserName]);
   ```

4. **Gérez les différences culturelles** : Pas uniquement les traductions, mais aussi les formats de date, heure, monnaie, etc.

5. **Testez avec différentes langues** : Certaines langues peuvent nécessiter plus d'espace ou un alignement différent

6. **Utilisez Unicode** : Assurez-vous que votre application prend en charge tous les caractères des langues cibles

7. **Créez des outils pour faciliter la traduction** : Un éditeur simple peut aider les traducteurs non-techniciens

## Exemple complet : Changement de langue à la volée

Voici un exemple complet montrant comment implémenter un changement de langue dynamique dans une application Delphi :

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, LanguageManager;

type
  TfrmMain = class(TForm)
    lblWelcome: TLabel;
    btnOK: TButton;
    btnCancel: TButton;
    rgLanguage: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure rgLanguageClick(Sender: TObject);
  private
    FLangManager: TLanguageManager;
    procedure UpdateUILanguage;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FLangManager := TLanguageManager.Create(
    ExtractFilePath(Application.ExeName) + 'languages.ini');

  // Configurer le RadioGroup
  rgLanguage.Items.Clear;
  rgLanguage.Items.Add('Français');
  rgLanguage.Items.Add('English');
  rgLanguage.Items.Add('Español');

  // Langue par défaut: Français
  rgLanguage.ItemIndex := 0;
  FLangManager.SetLanguage('French');

  UpdateUILanguage;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FLangManager.Free;
end;

procedure TfrmMain.rgLanguageClick(Sender: TObject);
begin
  case rgLanguage.ItemIndex of
    0: FLangManager.SetLanguage('French');
    1: FLangManager.SetLanguage('English');
    2: FLangManager.SetLanguage('Spanish');
  end;

  UpdateUILanguage;
end;

procedure TfrmMain.UpdateUILanguage;
begin
  lblWelcome.Caption := FLangManager['msg_welcome'];
  btnOK.Caption := FLangManager['btn_ok'];
  btnCancel.Caption := FLangManager['btn_cancel'];
  Caption := FLangManager['form_title'];
end;

end.
```

## Conclusion

La gestion des ressources linguistiques est un aspect essentiel du développement d'applications internationales. Delphi offre plusieurs méthodes pour gérer ces ressources, allant des simples fichiers de ressources aux solutions plus avancées comme GNU gettext.

En choisissant la bonne approche pour votre projet et en suivant les bonnes pratiques, vous pouvez créer des applications multilingues facilement maintenables et évolutives.

---

Dans la prochaine section, nous verrons comment adapter votre application à différentes langues au-delà de la simple traduction des chaînes de caractères.

⏭️ [Adaptation à différentes langues](/13-internationalisation-et-localisation/03-adaptation-a-differentes-langues.md)
