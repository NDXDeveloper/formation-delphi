# 3.9 Organisation du code source et modularité

Une bonne organisation du code est essentielle pour développer des applications maintenables et évolutives. Dans cette section, nous allons explorer comment structurer efficacement votre code Delphi et comment utiliser la modularité pour créer des applications bien conçues.

## Pourquoi organiser son code ?

Un code bien organisé offre de nombreux avantages :

- **Lisibilité** : Le code est plus facile à comprendre
- **Maintenabilité** : Les modifications et corrections sont plus simples à réaliser
- **Réutilisabilité** : Les composants peuvent être réutilisés dans d'autres projets
- **Collaboration** : Plusieurs développeurs peuvent travailler ensemble plus efficacement
- **Testabilité** : Le code modulaire est plus facile à tester
- **Évolutivité** : L'application peut grandir sans devenir incontrôlable

## Les unités : le fondement de la modularité

En Delphi, l'unité (unit) est l'élément de base de la modularité. Une unité est un fichier `.pas` qui contient une portion isolée de code avec ses propres déclarations et implémentations.

### Structure d'une unité

Rappelons la structure d'une unité Delphi :

```pascal
unit MonUnite;

interface
  // Déclarations visibles par les autres unités
  // Uses, types, constantes, variables, procédures, fonctions...

implementation
  // Uses additionnels (visibles uniquement dans l'implémentation)
  // Code d'implémentation et déclarations privées

initialization
  // Code exécuté quand l'unité est chargée (optionnel)

finalization
  // Code exécuté quand l'unité est déchargée (optionnel)

end.
```

### Bonnes pratiques pour les unités

1. **Une responsabilité unique** : Chaque unité devrait avoir une responsabilité claire et unique
2. **Noms significatifs** : Donnez des noms qui reflètent le contenu ou l'objectif de l'unité
3. **Taille raisonnable** : Évitez les unités trop grandes (plus de 1000-2000 lignes)
4. **Minimiser les dépendances** : Limitez les clauses `uses` à ce qui est réellement nécessaire

## Organisation des unités

Voici différentes approches pour organiser vos unités :

### 1. Organisation par fonction

Regroupez le code selon sa fonction dans l'application :

- `MainForm.pas` : Formulaire principal
- `Database.pas` : Accès aux données
- `Configuration.pas` : Gestion des paramètres
- `Reporting.pas` : Génération de rapports
- `Utils.pas` : Fonctions utilitaires

### 2. Organisation par couche

Divisez votre application en couches :

- **Présentation** : Interfaces utilisateur
  - `MainForm.pas`, `CustomerForm.pas`, etc.
- **Logique métier** : Règles et traitements
  - `CustomerBusiness.pas`, `OrderBusiness.pas`, etc.
- **Accès aux données** : Communication avec les bases de données
  - `CustomerData.pas`, `OrderData.pas`, etc.
- **Infrastructure** : Services communs
  - `Logging.pas`, `Configuration.pas`, etc.

### 3. Organisation par domaine

Regroupez le code par domaine métier :

- **Clients**
  - `CustomerForm.pas`, `CustomerData.pas`, `CustomerBusiness.pas`
- **Commandes**
  - `OrderForm.pas`, `OrderData.pas`, `OrderBusiness.pas`
- **Produits**
  - `ProductForm.pas`, `ProductData.pas`, `ProductBusiness.pas`

## Unités spécialisées

Certains types d'unités ont des rôles spécifiques :

### Unités d'interface

Ces unités contiennent uniquement des déclarations d'interfaces (au sens POO) :

```pascal
unit Interfaces;

interface

type
  ILogger = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure Log(const Message: string);
  end;

  ICustomerRepository = interface
    ['{87654321-4321-4321-4321-CBA987654321}']
    function GetCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
  end;

implementation

end.
```

### Unités de constantes

Regroupent les constantes utilisées par l'application :

```pascal
unit Constants;

interface

const
  // Configuration
  DEFAULT_SERVER = 'localhost';
  DEFAULT_PORT = 3306;

  // Messages
  MSG_CONNECTION_ERROR = 'Erreur de connexion au serveur';

  // Valeurs métier
  TAX_RATE = 0.20;  // 20% de TVA

resourcestring  // Pour les chaînes traduisibles
  RS_WELCOME = 'Bienvenue dans notre application';
  RS_GOODBYE = 'Merci d''avoir utilisé notre application';

implementation

end.
```

### Unités de types

Contiennent les définitions de types partagés :

```pascal
unit Types;

interface

type
  TCustomerType = (ctRegular, ctVIP, ctReseller);

  TAddress = record
    Street: string;
    City: string;
    ZipCode: string;
    Country: string;
  end;

  TCustomer = class
    ID: Integer;
    Name: string;
    Email: string;
    CustomerType: TCustomerType;
    Address: TAddress;
  end;

implementation

end.
```

## Organisation des fichiers de projet

Au-delà des unités individuelles, organisez votre projet en dossiers logiques :

```
MonProjet/
  ├── src/                  # Code source
  │   ├── forms/            # Formulaires
  │   ├── datamodules/      # Modules de données
  │   ├── business/         # Logique métier
  │   ├── data/             # Accès aux données
  │   └── utils/            # Utilitaires
  ├── resources/            # Ressources
  │   ├── images/           # Images
  │   ├── strings/          # Fichiers de traduction
  │   └── reports/          # Modèles de rapports
  ├── libs/                 # Bibliothèques tierces
  ├── docs/                 # Documentation
  └── tests/                # Tests unitaires
```

Même si les dossiers sont utilisés dans l'IDE, assurez-vous que les chemins de recherche sont correctement configurés dans les options du projet.

## Namespaces (Espaces de noms)

Depuis Delphi 2009, vous pouvez utiliser des espaces de noms pour organiser votre code. Les espaces de noms aident à éviter les conflits de noms et à mieux structurer votre application.

### Utilisation des espaces de noms

Les espaces de noms sont simplement des préfixes pour vos unités :

```pascal
// Dans l'unité App.Database.Connection.pas
unit App.Database.Connection;

interface

type
  TDatabaseConnection = class
    // ...
  end;

implementation

end.
```

Vous pouvez ensuite utiliser cet espace de noms dans votre clause `uses` :

```pascal
uses
  System.SysUtils,
  System.Classes,
  App.Database.Connection;
```

### Organisation avec espaces de noms

Voici un exemple d'organisation d'unités avec des espaces de noms :

- `App.UI.MainForm`
- `App.UI.CustomerForm`
- `App.Business.Customer`
- `App.Business.Order`
- `App.Data.CustomerRepository`
- `App.Data.OrderRepository`
- `App.Utils.Logging`

Remarque : le nom de fichier doit correspondre au nom de l'unité, donc `App.UI.MainForm` serait sauvegardé dans `App.UI.MainForm.pas`.

## Packages

Pour les projets plus importants, les packages Delphi offrent un niveau supérieur de modularité.

### Qu'est-ce qu'un package ?

Un package est une bibliothèque de composants compilée séparément qui peut être utilisée par d'autres applications ou packages. Il existe deux types de packages :

- **Packages d'exécution** (.bpl) : Utilisés à l'exécution
- **Packages de conception** (.bpl) : Utilisés à la conception dans l'IDE

### Création d'un package

1. Choisissez `Fichier` > `Nouveau` > `Package` dans l'IDE
2. Ajoutez vos unités au package
3. Compilez le package

### Structure d'un projet à packages

Pour un grand projet, vous pourriez avoir :

- `MyApp.Core.bpl` : Fonctionnalités de base
- `MyApp.UI.bpl` : Interfaces utilisateur
- `MyApp.Reports.bpl` : Générateur de rapports
- `MyApp.exe` : Application principale qui utilise ces packages

Cette approche permet :
- Le développement parallèle
- Le chargement dynamique de fonctionnalités
- La mise à jour partielle de l'application

## Modèles architecturaux

Pour structurer votre application à un niveau plus élevé, considérez ces modèles d'architecture :

### MVC (Modèle-Vue-Contrôleur)

Divise l'application en trois parties :
- **Modèle** : Données et logique métier
- **Vue** : Interface utilisateur
- **Contrôleur** : Gère les entrées et coordonne le modèle et la vue

En Delphi, cela pourrait se traduire par :

```
App/
  ├── Models/
  │   ├── CustomerModel.pas
  │   └── OrderModel.pas
  ├── Views/
  │   ├── MainForm.pas
  │   └── CustomerForm.pas
  └── Controllers/
      ├── CustomerController.pas
      └── OrderController.pas
```

### MVVM (Modèle-Vue-VueModèle)

Une variante de MVC plus adaptée aux interfaces modernes :
- **Modèle** : Données et logique métier
- **Vue** : Interface utilisateur (passive)
- **VueModèle** : Médiateur entre le modèle et la vue

En Delphi, cela pourrait donner :

```
App/
  ├── Models/
  │   ├── CustomerModel.pas
  │   └── OrderModel.pas
  ├── Views/
  │   ├── MainForm.pas
  │   └── CustomerForm.pas
  └── ViewModels/
      ├── CustomerViewModel.pas
      └── OrderViewModel.pas
```

### DDD (Domain-Driven Design)

Pour les applications complexes, le DDD organise le code autour du domaine métier :
- **Couche domaine** : Entités, objets valeur, services de domaine
- **Couche application** : Orchestration, cas d'utilisation
- **Couche infrastructure** : Persistance, communication externe
- **Couche présentation** : Interfaces utilisateur

## Modularité au niveau du code

Au-delà de l'organisation des fichiers, la modularité se reflète dans la conception des classes et interfaces :

### Interfaces pour le découplage

Utilisez des interfaces pour réduire les dépendances entre les modules :

```pascal
// Dans Interfaces.pas
type
  ILogger = interface
    procedure Log(const Message: string);
  end;

// Dans FileLogger.pas
type
  TFileLogger = class(TInterfacedObject, ILogger)
    procedure Log(const Message: string); override;
  end;

// Dans BusinessLogic.pas
type
  TBusinessProcessor = class
  private
    FLogger: ILogger;
  public
    constructor Create(ALogger: ILogger);
    procedure ProcessData;
  end;
```

L'unité `BusinessLogic` dépend de l'interface `ILogger` mais pas de l'implémentation concrète `TFileLogger`.

### Injection de dépendances

Fournissez les dépendances de l'extérieur plutôt que de les créer à l'intérieur :

```pascal
// Au lieu de ceci :
constructor TCustomerService.Create;
begin
  FRepository := TCustomerRepository.Create;  // Dépendance forte
end;

// Préférez ceci :
constructor TCustomerService.Create(ARepository: ICustomerRepository);
begin
  FRepository := ARepository;  // Dépendance injectée
end;
```

Cela facilite les tests et permet de changer l'implémentation sans modifier le code.

## Exemple concret : Application de gestion de clients

Voici comment organiser une application simple de gestion de clients :

### 1. Les interfaces

```pascal
// App.Interfaces.pas
unit App.Interfaces;

interface

uses
  App.Types;

type
  ICustomerRepository = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function GetAll: TCustomerArray;
    function GetById(ID: Integer): TCustomer;
    procedure Save(Customer: TCustomer);
    procedure Delete(ID: Integer);
  end;

  ICustomerService = interface
    ['{87654321-4321-4321-4321-CBA987654321}']
    function GetCustomers: TCustomerArray;
    function GetCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
    procedure DeleteCustomer(ID: Integer);
    function GetVIPCustomers: TCustomerArray;
  end;

implementation

end.
```

### 2. Les types partagés

```pascal
// App.Types.pas
unit App.Types;

interface

type
  TCustomerType = (ctRegular, ctVIP, ctReseller);

  TCustomer = class
  public
    ID: Integer;
    Name: string;
    Email: string;
    CustomerType: TCustomerType;

    constructor Create;
  end;

  TCustomerArray = array of TCustomer;

implementation

constructor TCustomer.Create;
begin
  inherited;
  CustomerType := ctRegular;
end;

end.
```

### 3. L'implémentation du repository

```pascal
// App.Data.CustomerRepository.pas
unit App.Data.CustomerRepository;

interface

uses
  System.SysUtils, System.Classes,
  App.Interfaces, App.Types, FireDAC.Comp.Client;

type
  TCustomerRepository = class(TInterfacedObject, ICustomerRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(Connection: TFDConnection);
    destructor Destroy; override;

    // Implémentation de ICustomerRepository
    function GetAll: TCustomerArray;
    function GetById(ID: Integer): TCustomer;
    procedure Save(Customer: TCustomer);
    procedure Delete(ID: Integer);
  end;

implementation

// Implémentation...

end.
```

### 4. L'implémentation du service

```pascal
// App.Business.CustomerService.pas
unit App.Business.CustomerService;

interface

uses
  System.SysUtils,
  App.Interfaces, App.Types;

type
  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FRepository: ICustomerRepository;
  public
    constructor Create(Repository: ICustomerRepository);

    // Implémentation de ICustomerService
    function GetCustomers: TCustomerArray;
    function GetCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
    procedure DeleteCustomer(ID: Integer);
    function GetVIPCustomers: TCustomerArray;
  end;

implementation

constructor TCustomerService.Create(Repository: ICustomerRepository);
begin
  inherited Create;
  FRepository := Repository;
end;

// Reste de l'implémentation...

function TCustomerService.GetVIPCustomers: TCustomerArray;
var
  AllCustomers: TCustomerArray;
  Customer: TCustomer;
  I, Count: Integer;
begin
  AllCustomers := FRepository.GetAll;

  // Compter les clients VIP
  Count := 0;
  for Customer in AllCustomers do
    if Customer.CustomerType = ctVIP then
      Inc(Count);

  // Créer le tableau résultat
  SetLength(Result, Count);

  // Remplir le tableau
  Count := 0;
  for Customer in AllCustomers do
    if Customer.CustomerType = ctVIP then
    begin
      Result[Count] := Customer;
      Inc(Count);
    end;
end;

// Reste de l'implémentation...

end.
```

### 5. Le formulaire principal

```pascal
// App.UI.MainForm.pas
unit App.UI.MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  App.Interfaces, App.Types;

type
  TMainForm = class(TForm)
    StringGrid1: TStringGrid;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    btnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    FCustomerService: ICustomerService;
    procedure LoadCustomers;
  public
    constructor Create(AOwner: TComponent; CustomerService: ICustomerService); reintroduce;
  end;

implementation

{$R *.dfm}

// Implémentation...

end.
```

### 6. Le fichier projet (DPR)

```pascal
// Project1.dpr
program Project1;

uses
  Vcl.Forms,
  FireDAC.Comp.Client,
  App.UI.MainForm in 'App.UI.MainForm.pas' {MainForm},
  App.Interfaces in 'App.Interfaces.pas',
  App.Types in 'App.Types.pas',
  App.Data.CustomerRepository in 'App.Data.CustomerRepository.pas',
  App.Business.CustomerService in 'App.Business.CustomerService.pas';

{$R *.res}

var
  Connection: TFDConnection;
  Repository: ICustomerRepository;
  Service: ICustomerService;
  MainForm: TMainForm;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Configuration
  Connection := TFDConnection.Create(nil);
  try
    // Configuration de la connexion...
    Connection.ConnectionString := 'DriverID=SQLite;Database=customers.db';
    Connection.Connected := True;

    // Création du repository
    Repository := TCustomerRepository.Create(Connection);

    // Création du service
    Service := TCustomerService.Create(Repository);

    // Création du formulaire principal
    MainForm := TMainForm.Create(Application, Service);
    Application.MainForm := MainForm;
    Application.Run;
  finally
    Connection.Free;
  end;
end.
```

Cet exemple illustre :
- L'utilisation d'interfaces pour le découplage
- L'organisation du code en couches (UI, Business, Data)
- L'injection de dépendances
- La séparation des responsabilités

## Conseils pratiques

1. **Commencez simple** : Ne sur-ingénieriez pas les petits projets
2. **Réfactorisez progressivement** : Améliorez l'organisation au fur et à mesure
3. **Suivez un standard** : Adoptez des conventions de nommage et de structure cohérentes
4. **Documentez** : Expliquez l'architecture et l'organisation pour les nouveaux développeurs
5. **Utilisez des outils** : Des outils comme ModelMaker Code Explorer peuvent aider à gérer la structure
6. **Surveillez les dépendances** : Évitez les dépendances circulaires entre les unités

---

Une bonne organisation du code est un investissement qui porte ses fruits à long terme. En suivant ces principes de modularité, vous créerez des applications Delphi plus maintenables, plus évolutives et plus robustes. À mesure que vos projets grandiront, vous apprécierez de plus en plus l'importance d'une architecture bien conçue.

Dans la prochaine section, nous explorerons les nouveautés syntaxiques des dernières versions d'Object Pascal, qui peuvent vous aider à écrire un code encore plus propre et efficace.
