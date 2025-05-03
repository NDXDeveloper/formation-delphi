# 15.9 Partage de code entre applications mobile et desktop

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'un des grands avantages de Delphi est la possibilité de partager du code entre différentes plateformes. Cette fonctionnalité vous permet de développer une base de code commune qui peut être déployée sur des applications mobiles (Android et iOS) ainsi que sur des applications desktop (Windows, macOS et Linux). Dans cette section, nous explorerons les techniques et les meilleures pratiques pour maximiser la réutilisation du code tout en respectant les spécificités de chaque plateforme.

## Pourquoi partager du code ?

Avant de plonger dans les détails techniques, voyons les avantages du partage de code :

1. **Réduction du temps de développement** : Écrire le code une seule fois plutôt que de le réécrire pour chaque plateforme
2. **Maintenance simplifiée** : Les corrections de bugs et les améliorations s'appliquent à toutes les plateformes
3. **Cohérence fonctionnelle** : Assurer un comportement identique sur toutes les plateformes
4. **Rentabilité accrue** : Réduction des coûts de développement et de maintenance

## Les défis du développement multi-plateformes

Malgré ses avantages, le développement multi-plateformes présente certains défis :

1. **Différences d'interface utilisateur** : Les attentes des utilisateurs varient selon les plateformes
2. **Fonctionnalités spécifiques** : Certaines fonctionnalités existent sur une plateforme mais pas sur les autres
3. **Contraintes de performance** : Les appareils mobiles ont généralement moins de ressources
4. **Cycle de vie des applications** : Les applications mobiles et desktop ont des cycles de vie différents

Heureusement, Delphi offre des outils pour relever ces défis tout en maximisant le partage de code.

## Architecture pour le partage de code

La clé d'un partage de code réussi est une bonne architecture. Voici l'approche recommandée :

### Architecture en couches

Une architecture en couches bien définie facilite le partage de code :

1. **Couche métier** : Logique d'affaires, entités et règles métier (partageable à 100%)
2. **Couche d'accès aux données** : Interactions avec la base de données et les services (partageable à 90-100%)
3. **Couche présentation** : Interface utilisateur (partageable partiellement)
4. **Couche spécifique à la plateforme** : Fonctionnalités propres à chaque plateforme (non partageable)

![Architecture en couches](https://votre-serveur.com/images/layered-architecture.png)

### Modèle MVVM (Model-View-ViewModel)

Le modèle d'architecture MVVM est particulièrement adapté au partage de code :

- **Model** : Les données et la logique métier (partageable à 100%)
- **ViewModel** : La logique de présentation (partageable à 80-90%)
- **View** : L'interface utilisateur spécifique à chaque plateforme (moins partageable)

## Techniques de partage de code avec Delphi

Delphi offre plusieurs techniques pour partager du code entre les plateformes :

### 1. Projets multi-plateformes avec FireMonkey

FireMonkey (FMX) est le framework d'interface utilisateur multi-plateformes de Delphi qui permet de créer des interfaces qui fonctionnent sur Windows, macOS, iOS et Android.

Pour créer un projet multi-plateformes avec FireMonkey :

1. Sélectionnez **File > New > Multi-Device Application**
2. Choisissez un modèle de départ (Vide, Application principale, etc.)
3. Dans le **Project Manager**, vous verrez les plateformes cibles disponibles

![Projet Multi-Plateformes](https://votre-serveur.com/images/multi-platform-project.png)

### 2. Unités partagées

Les unités sont la base du partage de code en Delphi. Vous pouvez créer des unités qui contiennent du code commun à toutes les plateformes.

```pascal
unit BusinessLogic;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TCustomer = class
  private
    FID: Integer;
    FName: string;
    FEmail: string;
  public
    constructor Create(AID: Integer; const AName, AEmail: string);

    function ValidateEmail: Boolean;
    procedure SaveToDatabase;

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

  TCustomerManager = class
  private
    FCustomers: TObjectList<TCustomer>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadCustomers;
    function FindCustomerByEmail(const Email: string): TCustomer;
    procedure AddCustomer(Customer: TCustomer);

    property Customers: TObjectList<TCustomer> read FCustomers;
  end;

implementation

// Implémentations...

end.
```

Cette unité peut être utilisée à l'identique sur toutes les plateformes.

### 3. Compilation conditionnelle

Delphi permet d'utiliser des directives de compilation conditionnelle pour adapter le code aux différentes plateformes :

```pascal
{$IF DEFINED(MSWINDOWS)}
  // Code spécifique à Windows
{$ELSEIF DEFINED(MACOS)}
  // Code spécifique à macOS
{$ELSEIF DEFINED(IOS)}
  // Code spécifique à iOS
{$ELSEIF DEFINED(ANDROID)}
  // Code spécifique à Android
{$ELSE}
  // Code par défaut pour les autres plateformes
{$ENDIF}
```

Vous pouvez utiliser ces directives pour adapter certaines parties de votre code tout en conservant une base commune.

Exemple plus complet pour l'accès aux fichiers :

```pascal
function GetAppDataPath: string;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TPath.GetHomePath + '\AppData\Roaming\MyApp\';
{$ELSEIF DEFINED(MACOS)}
  Result := TPath.GetHomePath + '/Library/Application Support/MyApp/';
{$ELSEIF DEFINED(IOS) or DEFINED(ANDROID)}
  Result := TPath.GetDocumentsPath + '/';
{$ELSE}
  Result := './';
{$ENDIF}
end;
```

### 4. Packages d'exécution (Runtime Packages)

Les packages d'exécution permettent de partager du code sous forme de bibliothèques dynamiques (.bpl).

Avantages :
- Modularisation du code
- Mise à jour indépendante des modules
- Réduction de la taille de l'application

Pour créer un package :

1. Sélectionnez **File > New > Package**
2. Ajoutez les unités que vous souhaitez inclure
3. Compilez le package
4. Référencez-le dans vos projets d'application

### 5. Groupe de projets

Un groupe de projets permet de gérer plusieurs projets liés dans une structure unique :

1. Sélectionnez **File > New > Project Group**
2. Ajoutez vos projets (desktop, mobile, packages partagés, etc.)

Cette approche facilite la gestion de plusieurs applications qui partagent du code commun.

## Stratégies de partage de code

### Stratégie 1 : Séparation interface/implémentation

Une approche efficace consiste à séparer clairement l'interface (déclarations) de l'implémentation (code). Cette méthode utilise les interfaces Delphi pour définir des contrats que différentes plateformes peuvent implémenter :

```pascal
// Unit: DataModule.pas
unit DataModule;

interface

uses
  System.Classes, System.SysUtils;

type
  IDataStorage = interface
    ['{A1234567-1234-1234-1234-1234567890AB}']
    function SaveData(const Data: string): Boolean;
    function LoadData: string;
  end;

  TDataHelper = class
  private
    FStorage: IDataStorage;
  public
    constructor Create(AStorage: IDataStorage);

    function ProcessAndSaveData(const InputData: string): Boolean;
    function LoadAndProcessData: string;
  end;

implementation

constructor TDataHelper.Create(AStorage: IDataStorage);
begin
  inherited Create;
  FStorage := AStorage;
end;

function TDataHelper.ProcessAndSaveData(const InputData: string): Boolean;
var
  ProcessedData: string;
begin
  // Logique métier commune
  ProcessedData := InputData + ' (processed)';

  // Utiliser l'implémentation spécifique à la plateforme
  Result := FStorage.SaveData(ProcessedData);
end;

function TDataHelper.LoadAndProcessData: string;
var
  RawData: string;
begin
  // Utiliser l'implémentation spécifique à la plateforme
  RawData := FStorage.LoadData;

  // Logique métier commune
  Result := 'Processed: ' + RawData;
end;

end.
```

Pour chaque plateforme, vous implémentez l'interface :

```pascal
// Unit: DataModuleDesktop.pas
unit DataModuleDesktop;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles,
  DataModule;

type
  TDesktopDataStorage = class(TInterfacedObject, IDataStorage)
  private
    FIniFile: string;
  public
    constructor Create(const IniFileName: string);
    destructor Destroy; override;

    // Implémentation de l'interface IDataStorage
    function SaveData(const Data: string): Boolean;
    function LoadData: string;
  end;

implementation

constructor TDesktopDataStorage.Create(const IniFileName: string);
begin
  inherited Create;
  FIniFile := IniFileName;
end;

destructor TDesktopDataStorage.Destroy;
begin
  inherited;
end;

function TDesktopDataStorage.SaveData(const Data: string): Boolean;
var
  Ini: TIniFile;
begin
  Result := False;

  try
    Ini := TIniFile.Create(FIniFile);
    try
      Ini.WriteString('Data', 'Content', Data);
      Result := True;
    finally
      Ini.Free;
    end;
  except
    // Gérer les erreurs
  end;
end;

function TDesktopDataStorage.LoadData: string;
var
  Ini: TIniFile;
begin
  Result := '';

  try
    if FileExists(FIniFile) then
    begin
      Ini := TIniFile.Create(FIniFile);
      try
        Result := Ini.ReadString('Data', 'Content', '');
      finally
        Ini.Free;
      end;
    end;
  except
    // Gérer les erreurs
  end;
end;

end.
```

```pascal
// Unit: DataModuleMobile.pas
unit DataModuleMobile;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils,
  DataModule;

type
  TMobileDataStorage = class(TInterfacedObject, IDataStorage)
  private
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface IDataStorage
    function SaveData(const Data: string): Boolean;
    function LoadData: string;
  end;

implementation

constructor TMobileDataStorage.Create;
begin
  inherited Create;
  FFileName := TPath.Combine(TPath.GetDocumentsPath, 'appdata.txt');
end;

destructor TMobileDataStorage.Destroy;
begin
  inherited;
end;

function TMobileDataStorage.SaveData(const Data: string): Boolean;
begin
  Result := False;

  try
    TFile.WriteAllText(FFileName, Data);
    Result := True;
  except
    // Gérer les erreurs
  end;
end;

function TMobileDataStorage.LoadData: string;
begin
  Result := '';

  try
    if TFile.Exists(FFileName) then
      Result := TFile.ReadAllText(FFileName);
  except
    // Gérer les erreurs
  end;
end;

end.
```

### Stratégie 2 : Factory Method pour la création d'objets spécifiques

Le pattern Factory permet de créer des objets spécifiques à chaque plateforme tout en conservant une interface commune :

```pascal
// Unit: PlatformFactory.pas
unit PlatformFactory;

interface

uses
  System.Classes, System.SysUtils,
  DataModule;

type
  TPlatformFactory = class
  public
    class function CreateDataStorage: IDataStorage;
  end;

implementation

{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
uses
  DataModuleDesktop;
{$ELSE}
uses
  DataModuleMobile;
{$ENDIF}

class function TPlatformFactory.CreateDataStorage: IDataStorage;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TDesktopDataStorage.Create(TPath.Combine(
    TPath.GetHomePath, 'AppData\Roaming\MyApp\settings.ini'));
{$ELSEIF DEFINED(MACOS)}
  Result := TDesktopDataStorage.Create(TPath.Combine(
    TPath.GetHomePath, 'Library/Application Support/MyApp/settings.ini'));
{$ELSE}
  Result := TMobileDataStorage.Create;
{$ENDIF}
end;

end.
```

### Stratégie 3 : Utilisation des LiveBindings

Les LiveBindings de Delphi permettent de lier visuellement des données à l'interface utilisateur, ce qui facilite la séparation entre la logique métier et la présentation :

1. Créez un objet de données partageable
2. Utilisez l'éditeur LiveBindings dans Delphi pour lier les propriétés aux contrôles visuels
3. Le moteur LiveBindings s'occupe de la synchronisation

Cette approche aide à maintenir des interfaces cohérentes tout en partageant le modèle de données.

## Bonnes pratiques pour le partage de code

Pour maximiser l'efficacité du partage de code, suivez ces bonnes pratiques :

### 1. Planifiez votre architecture dès le début

Avant de commencer le développement, définissez clairement :
- Quelles parties du code seront partagées
- Comment gérer les différences entre plateformes
- Où placer le code spécifique à chaque plateforme

### 2. Utilisez des abstractions

- Créez des interfaces pour définir des comportements
- Utilisez des classes abstraites pour le code commun
- Implémentez des classes concrètes pour chaque plateforme

```pascal
type
  TAbstractPlatformServices = class abstract
  public
    class function GetDeviceID: string; virtual; abstract;
    class function GetOSVersion: string; virtual; abstract;
    class function IsTablet: Boolean; virtual; abstract;
  end;

  {$IF DEFINED(ANDROID)}
  TAndroidPlatformServices = class(TAbstractPlatformServices)
  public
    class function GetDeviceID: string; override;
    class function GetOSVersion: string; override;
    class function IsTablet: Boolean; override;
  end;
  {$ENDIF}

  {$IF DEFINED(IOS)}
  TiOSPlatformServices = class(TAbstractPlatformServices)
  public
    class function GetDeviceID: string; override;
    class function GetOSVersion: string; override;
    class function IsTablet: Boolean; override;
  end;
  {$ENDIF}

  // etc.
```

### 3. Organisez efficacement vos unités

- Regroupez le code commun dans des unités partagées
- Nommez clairement les unités spécifiques (ex: `FileUtils.Windows.pas`)
- Utilisez des alias d'unités pour simplifier les références

### 4. Minimisez les dépendances

- Évitez les dépendances inutiles entre les modules
- Privilégiez la composition plutôt que l'héritage
- Utilisez l'injection de dépendances pour plus de flexibilité

### 5. Testez sur toutes les plateformes

- Créez des tests unitaires pour le code partagé
- Vérifiez que le comportement est cohérent sur toutes les plateformes
- Automatisez les tests autant que possible

## Exemple complet : Application de gestion de notes multi-plateformes

Voici un exemple plus complet d'une application de notes qui partage du code entre desktop et mobile.

### 1. Structure du projet

- `NotesApp.groupproj` - Groupe de projets
  - `NotesApp.Desktop.dproj` - Application desktop
  - `NotesApp.Mobile.dproj` - Application mobile
  - `NotesApp.Core.dproj` - Package partagé contenant la logique métier

### 2. Modèle de données partagé

```pascal
// Unit: Notes.Model.pas (dans NotesApp.Core)
unit Notes.Model;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.JSON;

type
  TNoteCategory = (ncPersonal, ncWork, ncShopping, ncIdeas);

  TNote = class
  private
    FID: string;
    FTitle: string;
    FContent: string;
    FCreatedDate: TDateTime;
    FModifiedDate: TDateTime;
    FCategory: TNoteCategory;
  public
    constructor Create; overload;
    constructor Create(const ATitle, AContent: string;
                      ACategory: TNoteCategory); overload;

    function ToJSON: TJSONObject;
    procedure FromJSON(const JSONObj: TJSONObject);

    property ID: string read FID;
    property Title: string read FTitle write FTitle;
    property Content: string read FContent write FContent;
    property CreatedDate: TDateTime read FCreatedDate;
    property ModifiedDate: TDateTime read FModifiedDate write FModifiedDate;
    property Category: TNoteCategory read FCategory write FCategory;
  end;

  INoteRepository = interface
    ['{B1234567-1234-1234-1234-1234567890AB}']
    function GetNotes: TObjectList<TNote>;
    function GetNote(const ID: string): TNote;
    procedure SaveNote(Note: TNote);
    procedure DeleteNote(const ID: string);
    procedure SearchNotes(const SearchText: string;
                       Result: TObjectList<TNote>);
  end;

  TNoteManager = class
  private
    FRepository: INoteRepository;
  public
    constructor Create(ARepository: INoteRepository);
    destructor Destroy; override;

    function GetAllNotes: TObjectList<TNote>;
    function GetNotesByCategory(Category: TNoteCategory): TObjectList<TNote>;
    function CreateNewNote(const Title, Content: string;
                         Category: TNoteCategory): TNote;
    procedure UpdateNote(Note: TNote);
    procedure DeleteNote(const ID: string);
    function SearchNotes(const SearchText: string): TObjectList<TNote>;

    property Repository: INoteRepository read FRepository;
  end;

implementation

// Implémentations...

end.
```

### 3. Implémentations spécifiques aux plateformes

```pascal
// Unit: Notes.Repository.Desktop.pas
unit Notes.Repository.Desktop;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, Notes.Model;

type
  TDesktopNoteRepository = class(TInterfacedObject, INoteRepository)
  private
    FNotes: TObjectList<TNote>;
    FStoragePath: string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNotes: TObjectList<TNote>;
    function GetNote(const ID: string): TNote;
    procedure SaveNote(Note: TNote);
    procedure DeleteNote(const ID: string);
    procedure SearchNotes(const SearchText: string;
                       Result: TObjectList<TNote>);
  end;

implementation

// Implémentation pour desktop qui utilise un fichier JSON local

end.
```

```pascal
// Unit: Notes.Repository.Mobile.pas
unit Notes.Repository.Mobile;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Generics.Collections,
  System.IOUtils, Notes.Model;

type
  TMobileNoteRepository = class(TInterfacedObject, INoteRepository)
  private
    FNotes: TObjectList<TNote>;
    FStoragePath: string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor Create;
    destructor Destroy; override;

    function GetNotes: TObjectList<TNote>;
    function GetNote(const ID: string): TNote;
    procedure SaveNote(Note: TNote);
    procedure DeleteNote(const ID: string);
    procedure SearchNotes(const SearchText: string;
                       Result: TObjectList<TNote>);
  end;

implementation

// Implémentation pour mobile qui utilise un fichier dans le répertoire Documents

end.
```

### 4. Factory pour la création des objets spécifiques

```pascal
// Unit: Notes.Factory.pas
unit Notes.Factory;

interface

uses
  System.Classes, System.SysUtils, Notes.Model;

type
  TNotesFactory = class
  public
    class function CreateRepository: INoteRepository;
  end;

implementation

{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
uses
  Notes.Repository.Desktop;
{$ELSE}
uses
  Notes.Repository.Mobile;
{$ENDIF}

class function TNotesFactory.CreateRepository: INoteRepository;
begin
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  Result := TDesktopNoteRepository.Create;
{$ELSE}
  Result := TMobileNoteRepository.Create;
{$ENDIF}
end;

end.
```

### 5. Interface utilisateur spécifique à chaque plateforme

Pour l'application desktop (VCL ou FireMonkey) :

```pascal
// Unit: MainForm.Desktop.pas
unit MainForm.Desktop;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.StdCtrls, FMX.Edit, FMX.ComboEdit, FMX.Layouts,
  FMX.TabControl, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  Notes.Model, Notes.Factory;

type
  TMainFormDesktop = class(TForm)
    ToolBar1: TToolBar;
    btnNew: TButton;
    btnDelete: TButton;
    ListView1: TListView;
    Layout1: TLayout;
    edtTitle: TEdit;
    cmbCategory: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    memoContent: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure edtTitleChange(Sender: TObject);
    procedure memoContentChange(Sender: TObject);
    procedure cmbCategoryChange(Sender: TObject);
  private
    FNoteManager: TNoteManager;
    FCurrentNote: TNote;
    procedure LoadNotes;
    procedure UpdateCurrentNote;
    procedure ClearNoteForm;
  public
    { Public declarations }
  end;

var
  MainFormDesktop: TMainFormDesktop;

implementation

{$R *.fmx}

// Implémentations...

end.
```

Pour l'application mobile (FireMonkey) :

```pascal
// Unit: MainForm.Mobile.pas
unit MainForm.Mobile;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.StdCtrls, FMX.TabControl, FMX.Edit, FMX.Layouts,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  Notes.Model, Notes.Factory;

type
  TMainFormMobile = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    btnAdd: TSpeedButton;
    ListView1: TListView;
    btnBack: TSpeedButton;
    btnSave: TSpeedButton;
    lblTitle: TLabel;
    edtTitle: TEdit;
    Layout1: TLayout;
    Label1: TLabel;
    cmbCategory: TComboBox;
    memoContent: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure btnBackClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
  private
    FNoteManager: TNoteManager;
    FCurrentNote: TNote;
    FNewNote: Boolean;
    procedure LoadNotes;
    procedure SaveCurrentNote;
    procedure ClearNoteForm;
  public
    { Public declarations }
  end;

var
  MainFormMobile: TMainFormMobile;

implementation

{$R *.fmx}

// Implémentations...

end.
```

### 6. Programme principal

Pour l'application desktop :

```pascal
program NotesApp.Desktop;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm.Desktop in 'MainForm.Desktop.pas' {MainFormDesktop},
  Notes.Model in '..\Common\Notes.Model.pas',
  Notes.Repository.Desktop in '..\Common\Notes.Repository.Desktop.pas',
  Notes.Factory in '..\Common\Notes.Factory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFormDesktop, MainFormDesktop);
  Application.Run;
end.
```

Pour l'application mobile :

```pascal
program NotesApp.Mobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm.Mobile in 'MainForm.Mobile.pas' {MainFormMobile},
  Notes.Model in '..\Common\Notes.Model.pas',
  Notes.Repository.Mobile in '..\Common\Notes.Repository.Mobile.pas',
  Notes.Factory in '..\Common\Notes.Factory.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFormMobile, MainFormMobile);
  Application.Run;
end.
```

## Techniques avancées de partage de code

### Services locaux

Pour les fonctionnalités complexes, vous pouvez utiliser des services locaux :

```pascal
type
  IBackupService = interface
    ['{C1234567-1234-1234-1234-1234567890AB}']
    function BackupData(const DestinationPath: string): Boolean;
    function RestoreData(const SourcePath: string): Boolean;
  end;

  TCloudBackupService = class(TInterfacedObject, IBackupService)
  private
    // Implémentation commune
  end;

  {$IF DEFINED(MSWINDOWS)}
  TWindowsBackupService = class(TCloudBackupService)
  private
    // Implémentation spécifique à Windows
  end;
  {$ENDIF}
```

### Communication entre modules

Pour la communication entre modules, utilisez le pattern Observer ou un bus d'événements :

```pascal
type
  TNotificationCenter = class
  private
    class var FInstance: TNotificationCenter;
    FObservers: TDictionary<string, TList<TPair<TObject, TProc<TObject>>>>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddObserver(Observer: TObject; const NotificationName: string;
                        Handler: TProc<TObject>);
    procedure RemoveObserver(Observer: TObject; const NotificationName: string);
    procedure PostNotification(const NotificationName: string; Sender: TObject);

    class function Instance: TNotificationCenter;
    class procedure ReleaseInstance;
  end;
```

Cette approche permet une communication découplée entre les différentes parties de votre application.

## Conseils pour optimiser le partage de code

### 1. Adoptez une approche incrémentale

Ne cherchez pas à partager 100% du code dès le début :

1. Commencez par partager la logique métier fondamentale
2. Étendez progressivement le partage aux autres couches
3. Refactorisez régulièrement pour améliorer la structure

### 2. Comprenez les différences entre plateformes

Certaines fonctionnalités nécessitent des approches différentes selon la plateforme :

- **Stockage** : Les chemins d'accès et les permissions varient
- **Réseau** : La gestion de la connectivité diffère
- **Interface utilisateur** : Les paradigmes d'interaction sont différents
- **Performances** : Les optimisations peuvent être spécifiques

### 3. Documentez les interfaces partagées

Une bonne documentation facilite l'utilisation du code partagé :

- Documentez clairement les interfaces et les classes abstraites
- Spécifiez les comportements attendus des implémentations
- Indiquez les limitations spécifiques à chaque plateforme

### 4. Testez la compatibilité régulièrement

Les environnements de développement et les OS évoluent constamment :

- Testez fréquemment sur toutes les plateformes cibles
- Maintenez des appareils de test à jour (ou utilisez des services de test cloud)
- Automatisez les tests pour détecter les problèmes de compatibilité rapidement

### 5. Utilisez les gestionnaires de dépendances

Pour les projets complexes, structurez bien vos dépendances :

- Utilisez GetIt (le gestionnaire de packages Delphi) pour les composants
- Organisez clairement les dépendances de projets avec Project Manager
- Considérez l'utilisation d'outils de gestion de version comme Git avec une structure modulaire

## Cas d'usage courants du partage de code

### Modèles de données

Les modèles de données sont parfaits pour le partage de code car ils ne dépendent généralement pas de la plateforme :

```pascal
unit App.Models;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.JSON;

type
  TProduct = class
  private
    FID: Integer;
    FName: string;
    FPrice: Double;
    FDescription: string;
    FCategory: string;
    FImageURL: string;
    FRating: Double;
  public
    constructor Create; overload;
    constructor Create(JOBJ: TJSONObject); overload;

    function ToJSON: TJSONObject;

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Price: Double read FPrice write FPrice;
    property Description: string read FDescription write FDescription;
    property Category: string read FCategory write FCategory;
    property ImageURL: string read FImageURL write FImageURL;
    property Rating: Double read FRating write FRating;
  end;

  TProductList = class
  private
    FItems: TObjectList<TProduct>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddProduct(Product: TProduct);
    procedure LoadFromJSON(JSONArray: TJSONArray);
    function ToJSON: TJSONArray;

    property Items: TObjectList<TProduct> read FItems;
  end;

implementation

// Implémentations...

end.
```

Ce type de code peut être utilisé sans modification sur toutes les plateformes.

### Services d'accès aux données

Les services qui encapsulent l'accès aux API REST peuvent être largement partagés :

```pascal
unit App.Services;

interface

uses
  System.Classes, System.SysUtils, System.Net.HttpClient, System.Net.URLClient,
  System.JSON, App.Models;

type
  TProductService = class
  private
    FBaseURL: string;
    FAuthToken: string;
  public
    constructor Create(const BaseURL, AuthToken: string);

    function GetProducts: TProductList;
    function GetProductById(ID: Integer): TProduct;
    function SearchProducts(const Query: string): TProductList;
    function AddProduct(Product: TProduct): Boolean;
    function UpdateProduct(Product: TProduct): Boolean;
    function DeleteProduct(ID: Integer): Boolean;
  end;

implementation

constructor TProductService.Create(const BaseURL, AuthToken: string);
begin
  inherited Create;
  FBaseURL := BaseURL;
  FAuthToken := AuthToken;
end;

function TProductService.GetProducts: TProductList;
var
  Client: THTTPClient;
  Response: IHTTPResponse;
  URL: string;
  JSONArray: TJSONArray;
begin
  Result := TProductList.Create;
  Client := THTTPClient.Create;
  try
    // Configurer l'en-tête d'autorisation
    Client.CustomHeaders['Authorization'] := 'Bearer ' + FAuthToken;

    // Construire l'URL
    URL := FBaseURL + '/api/products';

    // Faire la requête
    Response := Client.Get(URL);

    // Vérifier la réponse
    if Response.StatusCode = 200 then
    begin
      JSONArray := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONArray;
      if JSONArray <> nil then
      try
        Result.LoadFromJSON(JSONArray);
      finally
        JSONArray.Free;
      end;
    end;
  finally
    Client.Free;
  end;
end;

// Autres implémentations...

end.
```

### Logique métier

La logique métier est idéale pour le partage de code, car elle encapsule les règles de votre application :

```pascal
unit App.BusinessLogic;

interface

uses
  System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  App.Models;

type
  TShoppingCart = class
  private
    FItems: TObjectList<TProduct>;
    FDiscountCode: string;
    FDiscountPercentage: Double;

    function CalculateSubtotal: Double;
    function CalculateDiscount: Double;
    function CalculateTax: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddProduct(Product: TProduct; Quantity: Integer = 1);
    procedure RemoveProduct(ProductID: Integer);
    procedure UpdateQuantity(ProductID: Integer; Quantity: Integer);
    procedure ApplyDiscountCode(const Code: string);
    procedure ClearCart;

    function GetItemCount: Integer;
    function GetTotal: Double;

    property Items: TObjectList<TProduct> read FItems;
    property DiscountCode: string read FDiscountCode;
    property DiscountPercentage: Double read FDiscountPercentage;
  end;

implementation

// Implémentations...

end.
```

### Utilitaires et helpers

Les fonctions utilitaires peuvent être partagées entre plateformes :

```pascal
unit App.Utils;

interface

uses
  System.Classes, System.SysUtils, System.DateUtils, System.NetEncoding;

type
  TDateUtils = class
  public
    class function FormatRelativeDate(ADate: TDateTime): string;
    class function IsToday(ADate: TDateTime): Boolean;
    class function IsYesterday(ADate: TDateTime): Boolean;
  end;

  TStringUtils = class
  public
    class function Truncate(const Text: string; MaxLength: Integer): string;
    class function StripHTML(const HTML: string): string;
    class function SanitizeFileName(const FileName: string): string;
  end;

  TCurrencyUtils = class
  public
    class function FormatCurrency(const Value: Double;
                                const CurrencyCode: string = 'EUR'): string;
    class function ConvertCurrency(const Value: Double;
                                 const FromCurrency, ToCurrency: string): Double;
  end;

implementation

// Implémentations...

end.
```

## Adaptation des interfaces utilisateur

Bien que le code d'interface ne soit pas totalement partageable, vous pouvez optimiser la réutilisation :

### Partager les ViewModels (MVVM)

Si vous utilisez le pattern MVVM, les ViewModels peuvent être largement partagés :

```pascal
unit App.ViewModels;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  App.Models, App.Services;

type
  TProductListViewModel = class
  private
    FProducts: TProductList;
    FService: TProductService;
    FIsLoading: Boolean;
    FSearchQuery: string;
    FOnProductsLoaded: TNotifyEvent;
    FOnError: TProc<string>;

    procedure SetSearchQuery(const Value: string);
  public
    constructor Create(AService: TProductService);
    destructor Destroy; override;

    procedure LoadProducts;
    procedure SearchProducts;
    procedure RefreshData;

    property Products: TProductList read FProducts;
    property IsLoading: Boolean read FIsLoading;
    property SearchQuery: string read FSearchQuery write SetSearchQuery;
    property OnProductsLoaded: TNotifyEvent read FOnProductsLoaded write FOnProductsLoaded;
    property OnError: TProc<string> read FOnError write FOnError;
  end;

implementation

// Implémentations...

end.
```

### Adaptateurs d'interface conditionnels

Utilisez des adaptateurs pour gérer les différences d'interface spécifiques à la plateforme :

```pascal
unit App.UIAdapters;

interface

uses
  System.SysUtils, System.Classes;

type
  IDialogService = interface
    ['{D1234567-1234-1234-1234-1234567890AB}']
    procedure ShowMessage(const Msg: string);
    function Confirm(const Msg: string): Boolean;
    function InputBox(const Title, Prompt: string; var Value: string): Boolean;
  end;

  INavigationService = interface
    ['{E1234567-1234-1234-1234-1234567890AB}']
    procedure NavigateTo(const ViewName: string);
    procedure NavigateBack;
    procedure ShowModal(const ViewName: string);
  end;

implementation

uses
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  App.Desktop.UIAdapters;
{$ELSE}
  App.Mobile.UIAdapters;
{$ENDIF}

function CreateDialogService: IDialogService;
begin
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  Result := TDesktopDialogService.Create;
{$ELSE}
  Result := TMobileDialogService.Create;
{$ENDIF}
end;

function CreateNavigationService: INavigationService;
begin
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  Result := TDesktopNavigationService.Create;
{$ELSE}
  Result := TMobileNavigationService.Create;
{$ENDIF}
end;

end.
```

### Ressources adaptatives

Gérez les différentes ressources (images, styles) selon la plateforme :

```pascal
function GetImageResourcePath(const ResourceName: string): string;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TPath.Combine('Resources\Images\Windows', ResourceName);
{$ELSEIF DEFINED(MACOS)}
  Result := TPath.Combine('Resources/Images/macOS', ResourceName);
{$ELSEIF DEFINED(IOS)}
  Result := TPath.Combine(TPath.GetDocumentsPath, 'Images/' + ResourceName);
{$ELSEIF DEFINED(ANDROID)}
  Result := TPath.Combine('assets/images', ResourceName);
{$ELSE}
  Result := 'images/' + ResourceName;
{$ENDIF}
end;
```

## Gestion des projets multi-plateformes

### Structure de répertoires recommandée

Une structure bien organisée facilite la maintenance :

```
ProjectRoot/
  ├── Source/
  │   ├── Common/            # Code partagé
  │   │   ├── Models/        # Modèles de données
  │   │   ├── Services/      # Services métier
  │   │   ├── Utils/         # Utilitaires
  │   │   └── ViewModels/    # ViewModels MVVM
  │   │
  │   ├── Desktop/           # Code spécifique Desktop
  │   │   ├── Views/         # Formulaires et vues
  │   │   ├── Adapters/      # Adaptateurs spécifiques
  │   │   └── Resources/     # Ressources desktop
  │   │
  │   └── Mobile/            # Code spécifique Mobile
  │       ├── Views/         # Formulaires et vues
  │       ├── Adapters/      # Adaptateurs spécifiques
  │       └── Resources/     # Ressources mobiles
  │
  ├── Projects/
  │   ├── Desktop/           # Projet Desktop
  │   │   ├── Win32/         # Fichiers de build Win32
  │   │   ├── Win64/         # Fichiers de build Win64
  │   │   └── macOS/         # Fichiers de build macOS
  │   │
  │   └── Mobile/            # Projet Mobile
  │       ├── Android/       # Fichiers de build Android
  │       └── iOS/           # Fichiers de build iOS
  │
  ├── Tests/                 # Tests unitaires et d'intégration
  ├── Docs/                  # Documentation
  └── Libs/                  # Bibliothèques tierces
```

### Configuration du groupe de projets

Dans Delphi, configurez correctement votre groupe de projets :

1. Créez un groupe de projets (**File > New > Project Group**)
2. Ajoutez vos projets (**Project > Add to Project**)
3. Configurez les dépendances si nécessaire (**Project > Project Dependencies**)
4. Configurez les chemins d'inclusion pour tous les projets

### Configuration des chemins d'inclusion

Pour chaque projet, configurez les chemins d'inclusion pour référencer les unités partagées :

1. Ouvrez les options du projet (**Project > Options**)
2. Accédez à **Delphi Compiler > Search path**
3. Ajoutez les chemins vers vos dossiers de code partagé :
   - `..\..\..\Source\Common\Models`
   - `..\..\..\Source\Common\Services`
   - `..\..\..\Source\Common\Utils`
   - `..\..\..\Source\Common\ViewModels`

## Défis courants et solutions

### Gestion des différences d'interface utilisateur

**Défi** : Les interfaces utilisateur desktop et mobile sont fondamentalement différentes.

**Solution** :
- Utilisez le pattern MVVM pour séparer la logique de présentation
- Créez des interfaces utilisateur distinctes mais connectées au même ViewModel
- Utilisez LiveBindings pour lier les données aux contrôles visuels

### Gestion des permissions spécifiques à la plateforme

**Défi** : Les plateformes mobiles ont des systèmes de permissions stricts.

**Solution** :
- Créez une interface pour les services nécessitant des permissions
- Implémentez cette interface différemment selon la plateforme
- Utilisez une Factory pour créer l'implémentation appropriée

```pascal
type
  ILocationService = interface
    ['{F1234567-1234-1234-1234-1234567890AB}']
    function GetCurrentLocation: TLocationCoord2D;
    procedure RequestLocationPermission;
    function HasLocationPermission: Boolean;
  end;

  TLocationServiceFactory = class
  public
    class function CreateLocationService: ILocationService;
  end;
```

### Gestion du cycle de vie de l'application

**Défi** : Les applications mobiles ont un cycle de vie différent des applications desktop.

**Solution** :
- Utilisez des gestionnaires d'événements standardisés
- Implémentez une interface commune pour les événements du cycle de vie

```pascal
type
  IApplicationLifecycle = interface
    ['{G1234567-1234-1234-1234-1234567890AB}']
    procedure OnApplicationStart;
    procedure OnApplicationResume;
    procedure OnApplicationPause;
    procedure OnApplicationStop;
    procedure OnApplicationMemoryWarning;

    procedure RegisterLifecycleObserver(Observer: TObject;
                                     EventHandler: TProc<string, TObject>);
    procedure UnregisterLifecycleObserver(Observer: TObject);
  end;
```

### Performances sur différentes plateformes

**Défi** : Les performances varient considérablement entre desktop et mobiles.

**Solution** :
- Optimisez les opérations lourdes pour les appareils mobiles
- Utilisez des stratégies adaptatives pour gérer les ressources

```pascal
procedure ProcessData(const Data: TStream);
{$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
// Version optimisée pour desktop avec plus de mémoire
var
  MemoryStream: TMemoryStream;
begin
  MemoryStream := TMemoryStream.Create;
  try
    MemoryStream.LoadFromStream(Data);
    // Traitement en mémoire
  finally
    MemoryStream.Free;
  end;
end;
{$ELSE}
// Version optimisée pour mobile avec moins de mémoire
var
  Buffer: array[0..4095] of Byte;
  BytesRead: Integer;
begin
  while True do
  begin
    BytesRead := Data.Read(Buffer, SizeOf(Buffer));
    if BytesRead = 0 then
      Break;

    // Traitement par petits blocs

    // Permettre au système de respirer entre les blocs
    TThread.Sleep(0);
  end;
end;
{$ENDIF}
```

## Études de cas

### Étude de cas 1 : Application de gestion d'événements

Une application de gestion d'événements avec versions desktop et mobile :

- **Code partagé (80%)** :
  - Modèles de données (événements, participants, lieux)
  - Services d'accès aux API
  - Logique métier (inscription, paiement, notifications)
  - ViewModels

- **Code spécifique desktop (20%)** :
  - Interface utilisateur multi-fenêtres
  - Rapports détaillés et impression
  - Outils d'administration avancés

- **Code spécifique mobile (20%)** :
  - Interface utilisateur optimisée pour le tactile
  - Intégration de la caméra pour scanner les QR codes
  - Fonctionnalités hors ligne

### Étude de cas 2 : Application de vente au détail

Une application de vente au détail avec point de vente desktop et catalogue mobile :

- **Code partagé (70%)** :
  - Catalogue de produits et gestion des stocks
  - Logique de tarification et promotions
  - Gestion des clients et profils
  - Synchronisation des données

- **Code spécifique desktop (30%)** :
  - Terminal de point de vente
  - Intégration du matériel (scanners, imprimantes)
  - Rapports de vente et analyses

- **Code spécifique mobile (30%)** :
  - Expérience de navigation dans le catalogue
  - Fonctionnalités sociales et partage
  - Paiement mobile et panier d'achat

## Conclusion

Le partage de code entre applications desktop et mobiles avec Delphi offre des avantages significatifs en termes de productivité et de maintenance. En suivant une architecture bien conçue et en appliquant les bonnes pratiques, vous pouvez atteindre un taux élevé de réutilisation de code tout en respectant les spécificités de chaque plateforme.

Les principales clés du succès sont :

1. **Architecture en couches** claire avec séparation des préoccupations
2. **Abstractions bien définies** pour isoler les différences entre plateformes
3. **Tests réguliers** sur toutes les plateformes cibles
4. **Approche incrémentale** qui commence par partager les éléments les plus universels

En combinant FireMonkey pour les interfaces utilisateur multi-plateformes et une architecture bien pensée pour le code partagé, vous pouvez créer des applications sophistiquées qui fonctionnent de manière native sur desktop et mobiles, tout en minimisant les efforts de développement et de maintenance.

Dans la prochaine section, nous explorerons les techniques pour optimiser les performances de vos applications mobiles Delphi, un aspect crucial pour offrir une expérience utilisateur fluide sur tous les appareils.

⏭️ [Permissions et confidentialité des données](/15-applications-mobiles-avec-delphi/10-permissions-et-confidentialite-des-donnees.md)
