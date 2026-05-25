🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.1 Structuration d'un projet Delphi

## Introduction

La structuration d'un projet Delphi est un élément fondamental pour garantir la maintenabilité, l'évolutivité et la collaboration efficace au sein d'une équipe de développement. Une bonne organisation dès le départ vous fera gagner un temps considérable et évitera de nombreux problèmes à long terme.

Dans cette section, nous allons explorer les meilleures pratiques pour organiser vos projets Delphi de manière professionnelle et cohérente.

## Pourquoi structurer son projet ?

Avant d'entrer dans les détails techniques, comprenons pourquoi la structuration est importante :

- **Maintenabilité** : Un projet bien organisé est plus facile à comprendre et à modifier, même après plusieurs mois sans y toucher
- **Collaboration** : Une structure claire facilite le travail en équipe et l'intégration de nouveaux développeurs
- **Réutilisabilité** : Une bonne organisation permet d'identifier et de réutiliser facilement des portions de code
- **Évolutivité** : Un projet structuré peut grandir sans devenir un cauchemar à gérer
- **Débogage** : Retrouver la source d'un problème est beaucoup plus rapide dans un projet organisé

## Comprendre les fichiers d'un projet Delphi

Lorsque vous créez un projet Delphi, plusieurs types de fichiers sont générés automatiquement. Voici les principaux :

### Fichiers essentiels

**Le fichier projet (.dpr et .dproj)**

Le fichier `.dpr` (Delphi Project) est le point d'entrée de votre application. Il contient le code Object Pascal qui démarre l'application. Le fichier `.dproj` (compagnon obligatoire depuis Delphi 2007) est le fichier de configuration du projet au format XML : il contient les options de compilation, les chemins de recherche, les configurations Debug/Release, les plateformes cibles, les versions, etc. **Les deux fichiers (.dpr et .dproj) doivent être versionnés.**

```pascal
program MonApplication;

uses
  Vcl.Forms,
  UnitPrincipale in 'UnitPrincipale.pas' {FormPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.
```

**Le fichier groupe de projets (.groupproj)**

Lorsqu'une solution contient plusieurs projets liés (par exemple un exécutable et plusieurs DLL ou packages), Delphi les regroupe dans un fichier `.groupproj` au format XML. C'est l'équivalent d'une solution Visual Studio. Ce fichier se versionne également.

**Les unités (.pas)**

Les fichiers `.pas` contiennent le code source Object Pascal. Chaque formulaire, module ou classe a généralement sa propre unité.

**Les formulaires (.dfm / .fmx)**

Les fichiers `.dfm` (Delphi Form, pour VCL) et `.fmx` (FireMonkey, pour les applications multi-plateformes) stockent la description visuelle de vos formulaires. Ils sont généralement édités via le concepteur visuel de Delphi, mais peuvent aussi être modifiés en mode texte.

**Les fichiers ressources (.res et .rc)**

Le fichier `.res` est compilé (binaire) ; le `.rc` est sa source en texte. Ils contiennent les ressources de l'application : icône, manifeste Windows (Vista/7/8/10/11 styles), VERSIONINFO, chaînes localisées, etc.

### Fichiers générés (à ne pas versionner)

Certains fichiers sont générés lors de la compilation et ne doivent pas être inclus dans votre système de gestion de versions :

- `.dcu` : Unités compilées
- `.exe`, `.dll`, `.bpl`, `.bpi`, `.lib`, `.a`, `.so`, `.dylib` : Exécutables, bibliothèques et packages
- `.tds`, `.map`, `.drc`, `.dres`, `.rsm` : Symboles de débogage et fichiers intermédiaires
- `.~pas`, `.~dfm`, `.~fmx`, `.~dpr`, `.~dproj` : Sauvegardes automatiques de l'IDE
- `.identcache`, `.dsk`, `.stat`, `.local` : Caches et état de l'IDE (spécifiques à un poste)
- `__history/`, `__recovery/` : Historique local et sauvegardes de récupération
- Dossiers de sortie par plateforme :
  - `Win32/`, `Win64/`
  - `Linux64/`
  - `OSXARM64/` (macOS Apple Silicon, depuis Delphi 11)
  - `iOSDevice64/`
  - `Android64/`, `Android32/`

## Structure de dossiers recommandée

Voici une structure de dossiers professionnelle et évolutive pour un projet Delphi :

```
MonProjet/
│
├── Source/                    # Code source principal
│   ├── Forms/                # Formulaires de l'application
│   │   ├── Main/            # Formulaire principal
│   │   ├── Dialogs/         # Boîtes de dialogue
│   │   └── Reports/         # Formulaires de rapports
│   │
│   ├── Units/               # Unités de code (logique métier)
│   │   ├── Models/          # Modèles de données
│   │   ├── Controllers/     # Contrôleurs (logique)
│   │   └── Utils/           # Utilitaires et helpers
│   │
│   ├── DataModules/         # Modules de données
│   └── Resources/           # Ressources (images, icônes, fichiers)
│
├── Database/                # Scripts et schémas de base de données
│   ├── Scripts/            # Scripts SQL
│   └── Migrations/         # Scripts de migration
│
├── Docs/                   # Documentation
│   ├── API/               # Documentation API
│   ├── UserManual/        # Manuel utilisateur
│   └── Technical/         # Documentation technique
│
├── Tests/                  # Tests unitaires et d'intégration
│   ├── Unit/
│   └── Integration/
│
├── Lib/                    # Bibliothèques tierces
│   ├── Components/        # Composants externes
│   └── Packages/          # Packages installés
│
├── Build/                  # Fichiers de compilation
│   ├── Win32/
│   ├── Win64/
│   └── Release/
│
├── Config/                 # Fichiers de configuration
│   ├── Development/
│   ├── Production/
│   └── Test/
│
└── Deploy/                 # Scripts et fichiers de déploiement
    ├── Installers/
    └── Updates/
```

## Organisation du code source

### Principe de séparation des responsabilités

Une règle d'or en développement : **une unité = une responsabilité**. Évitez de créer des "super-unités" qui font tout. Préférez plusieurs petites unités spécialisées.

**Mauvaise pratique :**
```
Utils.pas (contenant 5000 lignes avec tout et n'importe quoi)
```

**Bonne pratique :**
```
AppStringHelper.pas   # Utilitaires pour les chaînes  
AppDateHelper.pas     # Utilitaires pour les dates  
AppFileHelper.pas     # Utilitaires pour les fichiers  
AppMathHelper.pas     # Utilitaires mathématiques  
```

> ⚠ Attention aux **collisions avec la RTL** : les noms `System.StrUtils`, `System.DateUtils`, `System.IOUtils`, `System.Math` existent déjà. Si vous nommez vos unités à l'identique (par exemple `DateUtils.pas`), le compilateur pourrait avoir des ambiguïtés selon l'ordre des `uses`. Préfixez vos unités d'un identifiant projet (`App`, `MyApp`, initiales de l'entreprise) ou utilisez une convention différenciante.

### Convention de nommage des unités

Deux écoles cohabitent dans la communauté Delphi :

**École 1 — Préfixe hongaroïsé (historique, Borland/RAD)**
- `frm` pour les formulaires : `frmMain.pas`, `frmCustomer.pas`
- `dm` pour les DataModules : `dmDatabase.pas`, `dmReports.pas`
- `u` pour les unités génériques : `uCustomer.pas`, `uOrder.pas`
- `intf` pour les interfaces : `intfDatabase.pas`

**École 2 — Suffixe descriptif (moderne, recommandée pour les projets neufs)**
- `MainForm.pas`, `CustomerForm.pas` (formulaires)
- `MainDataModule.pas`, `ReportsDataModule.pas` (DataModules)
- `Customer.pas`, `Order.pas` (modèles, sans préfixe)
- `IDatabaseConnection` déclarée dans `DatabaseConnection.pas` (pas dans le nom de fichier)

L'école 2 produit des chemins de namespace plus naturels (`MyApp.Forms.MainForm`) et s'aligne sur les conventions de la RTL Embarcadero moderne (`System.SysUtils`, `Vcl.Forms`, `FMX.Dialogs`).

**Règle universelle — noms explicites** : Le nom doit indiquer clairement le contenu
  - ✅ `CustomerManager.pas`
  - ❌ `Unit1.pas`

**Choisissez UNE convention par projet et tenez-vous-y** — mélanger les deux écoles dans un même projet produit un code visuellement chaotique.

### Structure d'une unité

Organisez systématiquement vos unités avec cette structure :

```pascal
unit CustomerManager;

interface

uses
  // 1. Unités système d'abord (System.*, Data.*, Winapi.*)
  System.SysUtils, System.Classes,
  // 2. Puis unités du framework UI (Vcl.* ou FMX.*)
  Vcl.Forms, Vcl.Dialogs,
  // 3. Enfin vos propres unités
  DatabaseModule;

type
  // Déclarations des types et classes
  TCustomer = class
  private
    FName: string;
    FEmail: string;
    procedure SetName(const Value: string);
  public
    property Name: string read FName write SetName;
    property Email: string read FEmail write FEmail;
  end;

  TCustomerManager = class
  public
    function LoadCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
  end;

implementation

{ TCustomer }

procedure TCustomer.SetName(const Value: string);  
begin  
  FName := Trim(Value);
end;

{ TCustomerManager }

function TCustomerManager.LoadCustomer(ID: Integer): TCustomer;  
begin  
  // Implémentation
end;

procedure TCustomerManager.SaveCustomer(Customer: TCustomer);  
begin  
  // Implémentation
end;

end.
```

> 📝 **Convention `uses`** : ce regroupement en 3 blocs ordonnés est une bonne pratique très courante. Il permet de voir immédiatement les dépendances système (immuables) avant les dépendances internes (volatiles). L'IDE Delphi propose un raccourci `Ctrl+Shift+I` pour ajouter automatiquement une unité dans le `uses` à la bonne position.

## Organisation par couches

Pour les projets de taille moyenne à grande, adoptez une architecture en couches :

> Dans les exemples qui suivent, nous adoptons l'**école 2 (suffixe descriptif)** pour rester cohérents. Adaptez à votre convention si vous avez choisi l'école 1.

### Couche Présentation (UI)

Cette couche contient tous les formulaires et l'interface utilisateur.

```
Source/Forms/
  ├── MainForm.pas          # Formulaire principal
  ├── CustomerListForm.pas  # Liste des clients
  └── CustomerEditForm.pas  # Édition d'un client
```

**Responsabilité** : Affichage et interaction avec l'utilisateur uniquement. Aucune logique métier ici.

### Couche Logique Métier (Business Logic)

Cette couche contient toute la logique de l'application.

```
Source/Business/
  ├── CustomerManager.pas   # Gestion des clients
  ├── OrderProcessor.pas    # Traitement des commandes
  └── ValidationRules.pas   # Règles de validation
```

**Responsabilité** : Traitement des données, validation, calculs, règles métier.

### Couche Accès aux Données (Data Access)

Cette couche gère la communication avec la base de données.

```
Source/DataAccess/
  ├── DatabaseModule.pas    # Module de données principal (TDataModule)
  ├── CustomerRepository.pas # Accès aux données clients
  └── OrderRepository.pas   # Accès aux données commandes
```

**Responsabilité** : Requêtes SQL, connexion à la base, CRUD.

### Couche Modèles (Models)

Cette couche définit les structures de données.

```
Source/Models/
  ├── Customer.pas          # Modèle Client
  ├── Order.pas            # Modèle Commande
  └── Product.pas          # Modèle Produit
```

**Responsabilité** : Définition des classes et structures de données uniquement.

## Gestion des dépendances

### Règle des dépendances unidirectionnelles

Les dépendances doivent toujours aller dans un seul sens :

```
Présentation → Logique Métier → Accès Données → Modèles
```

**Jamais l'inverse !** Un DataModule ne doit jamais référencer un formulaire.

### Utilisation des interfaces

Pour découpler vos couches, utilisez des interfaces :

```pascal
// Interface définie dans la couche Business
unit CustomerManagerIntf;

interface

type
  ICustomerManager = interface
    ['{A3F0B6E2-7D14-4F1A-9C5B-2E9D8F0A1234}']  // remplacer par un vrai GUID
    function LoadCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
  end;

implementation

end.
```

> 💡 **Astuce IDE** : pour générer un vrai GUID dans Delphi, placez votre curseur après le mot-clé `interface` et appuyez sur **Ctrl+Maj+G**. L'IDE insère un GUID frais.

Puis implémentez cette interface dans votre couche d'accès aux données.

## Gestion des ressources

### Images et icônes

Centralisez vos ressources visuelles :

```
Source/Resources/
  ├── Images/
  │   ├── Icons/        # Icônes 16x16, 32x32
  │   ├── Logos/        # Logos de l'application
  │   └── Backgrounds/  # Images de fond
  └── Styles/           # Feuilles de style VCL
```

### Utilisation d'ImageList

Créez un DataModule spécifique pour les ressources partagées :

```pascal
unit ResourcesModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList;

type
  TResourcesModule = class(TDataModule)
    ImageListIcons: TImageList;
    ImageListToolbar: TImageList;
  end;

var
  ResourcesModule: TResourcesModule;

implementation

end.
```

## Fichiers de configuration

### Séparez les configurations par environnement

Utilisez des fichiers de configuration distincts :

```
Config/
  ├── app.config.dev.ini      # Développement
  ├── app.config.test.ini     # Tests
  └── app.config.prod.ini     # Production
```

### Gestion dans le code

Créez une unité dédiée à la configuration :

```pascal
unit AppConfig;

interface

type
  TAppConfig = class
  private
    FDatabaseServer: string;
    FDatabaseName: string;
    procedure LoadFromFile(const FileName: string);
  public
    constructor Create;
    property DatabaseServer: string read FDatabaseServer;
    property DatabaseName: string read FDatabaseName;
  end;

implementation

uses
  System.IniFiles;

constructor TAppConfig.Create;  
begin  
  inherited;
  {$IFDEF DEBUG}
    LoadFromFile('Config\app.config.dev.ini');
  {$ELSE}
    LoadFromFile('Config\app.config.prod.ini');
  {$ENDIF}
end;

procedure TAppConfig.LoadFromFile(const FileName: string);  
var  
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FileName);
  try
    FDatabaseServer := IniFile.ReadString('Database', 'Server', 'localhost');
    FDatabaseName := IniFile.ReadString('Database', 'Name', 'mydb');
  finally
    IniFile.Free;
  end;
end;

end.
```

## Bonnes pratiques pour les projets en équipe

### Fichier README

Créez toujours un fichier `README.md` à la racine qui explique :
- Comment installer le projet
- Les prérequis
- Comment compiler
- La structure du projet
- Les conventions utilisées

### Fichier .gitignore

Si vous utilisez Git, créez un `.gitignore` adapté à Delphi :

```
# === Compilation : binaires et objets ===
*.dcu
*.dcp
*.exe
*.dll
*.bpl
*.bpi
*.lib
*.a
*.o
*.so
*.dylib
*.tds
*.map
*.drc
*.dres
*.rsm

# === Dossiers de build par plateforme ===
Win32/  
Win64/  
Linux64/  
OSXARM64/  
OSX64/            # ancienne plateforme x86_64 macOS (Delphi 10.x)  
iOSDevice64/  
iOSSimARM64/      # depuis Delphi 11.3  
Android/  
Android64/  
Android32/  

# === Caches et historique IDE (locaux à un poste) ===
__history/
__recovery/
*.~*              # sauvegardes automatiques (.~pas, .~dfm, .~dpr, etc.)
*.local
*.identcache
*.stat
*.dsk             # état du Desktop IDE (mise en page de l'éditeur)

# === Fichiers utilisateur à exclure (généralement) ===
# Le .cfg est régénéré depuis .dproj ; à exclure sauf cas spécifique.
*.cfg

# === Optionnel : packages installés ===
# Lib/Components/
```

**Remarques** :
- `.dproj.local` et `.dsk` contiennent des informations spécifiques à votre poste (chemins absolus, état des fenêtres de l'IDE) : ils ne doivent **jamais** être partagés.
- Vérifiez `git status` après le premier commit pour vous assurer qu'aucun fichier généré n'a été versionné par erreur.

### Documentation dans le code

Commentez votre code de manière utile :

```pascal
/// <summary>
///   Calcule le montant total TTC d'une commande
/// </summary>
/// <param name="OrderID">Identifiant de la commande</param>
/// <returns>Montant TTC en euros</returns>
/// <remarks>
///   Prend en compte les remises et le taux de TVA applicable
/// </remarks>
function CalculateTotalAmount(OrderID: Integer): Currency;
```

## Points clés à retenir

1. **Une unité = une responsabilité** : Ne créez pas de fichiers fourre-tout
2. **Organisez par couches** : Séparez UI, logique métier et accès données
3. **Nommez intelligemment** : Les noms doivent être explicites et cohérents
4. **Gérez les dépendances** : Les dépendances vont toujours du haut vers le bas
5. **Centralisez les ressources** : Images, icônes et configurations au même endroit
6. **Documentez** : README, commentaires et conventions claires
7. **Ne versionnez pas les fichiers générés** : Utilisez un .gitignore approprié

## Exemple de projet simple structuré

Voici à quoi pourrait ressembler un projet de gestion de clients simple mais bien structuré :

```
GestionClients/
│
├── Source/
│   ├── Forms/
│   │   ├── MainForm.pas              # Interface principale
│   │   └── CustomerEditForm.pas      # Édition client
│   │
│   ├── Business/
│   │   └── CustomerManager.pas       # Logique métier
│   │
│   ├── DataAccess/
│   │   └── DatabaseModule.pas        # Accès aux données
│   │
│   ├── Models/
│   │   └── Customer.pas              # Modèle Client
│   │
│   └── Utils/
│       └── AppStringHelper.pas       # Utilitaires (préfixe pour éviter
│                                     #  la collision avec System.StrUtils)
│
├── Config/
│   └── app.config.ini
│
├── GestionClients.dpr                # Projet principal
├── GestionClients.dproj
├── README.md
└── .gitignore
```

Cette structure simple mais efficace permet de faire évoluer facilement le projet en ajoutant de nouvelles fonctionnalités sans créer de désordre.

## Conclusion

La structuration d'un projet Delphi n'est pas une tâche complexe, mais elle demande de la réflexion et de la discipline. En suivant ces bonnes pratiques dès le début, vous construirez des applications plus maintenables, plus évolutives et plus faciles à comprendre pour vous-même et pour vos collègues.

N'oubliez pas : **le temps passé à bien organiser votre projet au début est du temps gagné tout au long du développement et de la maintenance de votre application.**

⏭️ [Patterns d'architecture (MVC, MVVM)](/18-architecture-et-bonnes-pratiques/02-patterns-darchitecture.md)
