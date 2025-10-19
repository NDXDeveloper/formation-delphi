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

**Le fichier projet (.dpr ou .dproj)**

Le fichier `.dpr` (Delphi Project) est le point d'entrée de votre application. Il contient le code principal qui démarre l'application. Le fichier `.dproj` est le fichier de configuration du projet au format XML.

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

**Les unités (.pas)**

Les fichiers `.pas` contiennent le code source Object Pascal. Chaque formulaire, module ou classe a généralement sa propre unité.

**Les formulaires (.dfm)**

Les fichiers `.dfm` (Delphi Form) stockent la description visuelle de vos formulaires. Ils sont généralement édités via le concepteur visuel de Delphi, mais peuvent aussi être modifiés en mode texte.

**Les fichiers ressources (.res)**

Ces fichiers contiennent les ressources de l'application comme l'icône, le manifeste Windows, etc.

### Fichiers générés (à ne pas versionner)

Certains fichiers sont générés lors de la compilation et ne doivent pas être inclus dans votre système de gestion de versions :

- `.dcu` : Unités compilées
- `.exe`, `.dll` : Exécutables et bibliothèques
- `.~pas`, `.~dfm` : Fichiers de sauvegarde automatique
- `__history` : Dossier contenant l'historique local des modifications
- `Win32`, `Win64`, `OSX64`, etc. : Dossiers de sortie de compilation

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
StringUtils.pas      # Utilitaires pour les chaînes
DateUtils.pas        # Utilitaires pour les dates
FileUtils.pas        # Utilitaires pour les fichiers
MathUtils.pas        # Utilitaires mathématiques
```

### Convention de nommage des unités

Adoptez une convention de nommage cohérente pour vos unités :

- **Préfixe descriptif** : Utilisez un préfixe pour identifier le type d'unité
  - `frm` pour les formulaires : `frmMain.pas`, `frmCustomer.pas`
  - `dm` pour les DataModules : `dmDatabase.pas`, `dmReports.pas`
  - `u` pour les unités génériques : `uCustomer.pas`, `uOrder.pas`
  - `intf` pour les interfaces : `intfDatabase.pas`

- **Noms explicites** : Le nom doit indiquer clairement le contenu
  - ✅ `CustomerManager.pas`
  - ❌ `Unit1.pas`

### Structure d'une unité

Organisez systématiquement vos unités avec cette structure :

```pascal
unit CustomerManager;

interface

uses
  // Unités système d'abord
  System.SysUtils, System.Classes,
  // Puis unités VCL/FMX
  Vcl.Forms, Vcl.Dialogs,
  // Enfin vos unités
  DataModuleMain;

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

## Organisation par couches

Pour les projets de taille moyenne à grande, adoptez une architecture en couches :

### Couche Présentation (UI)

Cette couche contient tous les formulaires et l'interface utilisateur.

```
Source/Forms/
  ├── frmMain.pas          # Formulaire principal
  ├── frmCustomerList.pas  # Liste des clients
  └── frmCustomerEdit.pas  # Édition d'un client
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
  ├── dmDatabase.pas        # Module de données principal
  ├── CustomerDAO.pas       # Accès aux données clients
  └── OrderDAO.pas          # Accès aux données commandes
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
unit intfCustomerManager;

interface

type
  ICustomerManager = interface
    ['{GUID-ICI}']
    function LoadCustomer(ID: Integer): TCustomer;
    procedure SaveCustomer(Customer: TCustomer);
  end;

implementation

end.
```

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
unit dmResources;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList;

type
  TdmResources = class(TDataModule)
    ImageListIcons: TImageList;
    ImageListToolbar: TImageList;
  end;

var
  dmResources: TdmResources;

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
# Fichiers compilés
*.dcu
*.exe
*.dll

# Dossiers de build
Win32/
Win64/
OSX64/
Android/
iOSDevice/

# Historique
__history/
*.~*

# Fichiers locaux
*.local
*.identcache
*.stat
```

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
│   │   ├── frmMain.pas              # Interface principale
│   │   └── frmCustomerEdit.pas      # Édition client
│   │
│   ├── Business/
│   │   └── CustomerManager.pas      # Logique métier
│   │
│   ├── DataAccess/
│   │   └── dmDatabase.pas           # Accès aux données
│   │
│   ├── Models/
│   │   └── Customer.pas             # Modèle Client
│   │
│   └── Utils/
│       └── StringHelper.pas         # Utilitaires
│
├── Config/
│   └── app.config.ini
│
├── GestionClients.dpr               # Projet principal
├── GestionClients.dproj
├── README.md
└── .gitignore
```

Cette structure simple mais efficace permet de faire évoluer facilement le projet en ajoutant de nouvelles fonctionnalités sans créer de désordre.

## Conclusion

La structuration d'un projet Delphi n'est pas une tâche complexe, mais elle demande de la réflexion et de la discipline. En suivant ces bonnes pratiques dès le début, vous construirez des applications plus maintenables, plus évolutives et plus faciles à comprendre pour vous-même et pour vos collègues.

N'oubliez pas : **le temps passé à bien organiser votre projet au début est du temps gagné tout au long du développement et de la maintenance de votre application.**

⏭️ [Patterns d'architecture (MVC, MVVM)](/18-architecture-et-bonnes-pratiques/02-patterns-darchitecture.md)
