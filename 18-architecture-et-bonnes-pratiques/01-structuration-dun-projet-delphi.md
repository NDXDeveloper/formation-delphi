# 18.1 Structuration d'un projet Delphi

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La structuration d'un projet est un aspect fondamental du développement logiciel, souvent négligé par les débutants mais essentiel pour créer des applications maintenables et évolutives. Une bonne organisation de votre projet Delphi dès le départ vous fera gagner un temps considérable lors des phases de développement, de maintenance et d'évolution. Cette section vous guidera à travers les principes et pratiques pour bien structurer vos projets Delphi.

## Pourquoi structurer son projet ?

Avant d'aborder le "comment", comprenons le "pourquoi" :

- **Maintenabilité** : Un code bien organisé est plus facile à comprendre et à modifier
- **Collaboration** : Permet à plusieurs développeurs de travailler efficacement sur le même projet
- **Évolutivité** : Facilite l'ajout de nouvelles fonctionnalités sans perturber l'existant
- **Réutilisabilité** : Les composants bien isolés peuvent être réutilisés dans d'autres projets
- **Testabilité** : Une structure claire facilite l'écriture de tests unitaires
- **Lisibilité** : Aide à comprendre rapidement l'architecture globale du projet

## Structure de base d'un projet Delphi

Un projet Delphi standard se compose des éléments suivants :

### Fichiers principaux

- **Fichier projet (.dproj)** : Fichier XML contenant les paramètres du projet
- **Fichier programme (.dpr)** : Point d'entrée de l'application, généralement court
- **Fichiers unités (.pas)** : Contiennent le code source de votre application
- **Fichiers de formulaires (.dfm)** : Décrivent la disposition des formulaires et composants visuels
- **Fichiers ressources (.res)** : Contiennent des ressources comme les icônes, images, etc.

### Exemple de structure minimale

```
MonProjet/
├── MonProjet.dpr      # Fichier programme principal
├── MonProjet.dproj    # Fichier projet
├── MainForm.pas       # Unité du formulaire principal
├── MainForm.dfm       # Description visuelle du formulaire principal
└── AboutForm.pas      # Unité d'un formulaire secondaire
└── AboutForm.dfm      # Description visuelle du formulaire secondaire
```

Cette structure basique fonctionne pour de petits projets, mais devient rapidement insuffisante pour des applications plus complexes.

## Organisation avancée des dossiers

Pour les projets de taille moyenne à grande, une organisation en dossiers est recommandée :

```
MonProjet/
├── src/                     # Code source
│   ├── forms/               # Formulaires
│   │   ├── MainForm.pas
│   │   ├── MainForm.dfm
│   │   ├── AboutForm.pas
│   │   └── AboutForm.dfm
│   ├── data/                # Accès aux données
│   │   ├── DataModule.pas
│   │   └── DataModule.dfm
│   ├── models/              # Classes de données et logique métier
│   │   ├── Customer.pas
│   │   └── Product.pas
│   └── utils/               # Fonctions et classes utilitaires
│       ├── StringUtils.pas
│       └── DateUtils.pas
├── res/                     # Ressources
│   ├── images/              # Images
│   └── strings/             # Fichiers de chaînes localisées
├── lib/                     # Bibliothèques tierces
├── bin/                     # Exécutables et DLL compilés
├── docs/                    # Documentation
└── MonProjet.dpr            # Fichier programme
└── MonProjet.dproj          # Fichier projet
```

### Avantages de cette structure

- **Séparation des préoccupations** : Chaque dossier a une responsabilité claire
- **Navigation facilitée** : Vous trouvez rapidement où se situe un élément spécifique
- **Isolation** : Les différentes parties de l'application sont bien séparées
- **Extensibilité** : Facile d'ajouter de nouveaux modules sans perturber l'existant

## Organisation par couches (architecture en oignons)

Pour les applications complexes, une organisation par couches est souvent recommandée :

```
MonProjet/
├── src/
│   ├── UI/                      # Interface utilisateur
│   │   ├── Forms/               # Formulaires standards
│   │   └── Dialogs/             # Boîtes de dialogue
│   ├── Presentation/            # Logique de présentation (MVVM/MVP)
│   │   ├── ViewModels/
│   │   └── Presenters/
│   ├── Domain/                  # Logique métier
│   │   ├── Entities/            # Objets métier
│   │   ├── Services/            # Services métier
│   │   └── Interfaces/          # Interfaces des services
│   ├── Infrastructure/          # Accès aux données externes
│   │   ├── Database/            # Accès aux bases de données
│   │   ├── Network/             # Communication réseau
│   │   └── Storage/             # Stockage local
│   └── Common/                  # Code partagé entre les couches
│       ├── Utils/               # Utilitaires génériques
│       └── Constants/           # Constantes globales
└── ...
```

Cette organisation reflète les principes d'architecture en couches où :

- L'**Interface Utilisateur** gère uniquement l'affichage et l'interaction
- La **Présentation** coordonne entre l'UI et le domaine
- Le **Domaine** contient la logique métier pure, indépendante des détails techniques
- L'**Infrastructure** gère les interactions avec le monde extérieur (bases de données, API...)

![Architecture en couches](https://placeholder.com/Layered_Architecture.png)

## Structure pour projets multi-plateformes

Si vous développez une application multi-plateformes avec FireMonkey, vous pourriez adopter cette structure :

```
MonProjet/
├── src/
│   ├── common/                  # Code partagé entre plateformes
│   │   ├── models/
│   │   ├── viewmodels/
│   │   └── services/
│   ├── platform/                # Code spécifique à chaque plateforme
│   │   ├── windows/
│   │   ├── macos/
│   │   ├── ios/
│   │   └── android/
│   └── ui/                      # Interface utilisateur
│       ├── forms/
│       ├── styles/              # Styles FireMonkey
│       └── assets/              # Ressources graphiques
└── ...
```

Cette approche permet de :
- Maximiser le partage de code entre plateformes
- Isoler clairement le code spécifique à chaque plateforme
- Faciliter la maintenance des fonctionnalités communes et spécifiques

## Structuration des unités Delphi

Au-delà de l'organisation des dossiers, la structure interne de vos unités (.pas) est également importante :

### Modèle d'unité bien structurée

```pascal
unit CustomerService;

interface

uses
  // Unités standard Delphi
  System.SysUtils, System.Classes,

  // Unités tierces
  SuperLogger,

  // Unités de votre projet
  Customer, Database;

type
  // Déclarations d'interfaces
  ICustomerService = interface
    ['{GUID-UNIQUE}']
    function GetCustomerById(const Id: Integer): TCustomer;
    // ...
  end;

  // Déclarations de classes
  TCustomerService = class(TInterfacedObject, ICustomerService)
  private
    FDatabase: TDatabase;
  public
    constructor Create(ADatabase: TDatabase);
    destructor Destroy; override;

    // Implémentation de ICustomerService
    function GetCustomerById(const Id: Integer): TCustomer;
  end;

implementation

{ TCustomerService }

constructor TCustomerService.Create(ADatabase: TDatabase);
begin
  inherited Create;
  FDatabase := ADatabase;
end;

destructor TCustomerService.Destroy;
begin
  // Nettoyage si nécessaire
  inherited;
end;

function TCustomerService.GetCustomerById(const Id: Integer): TCustomer;
begin
  // Implémentation
end;

end.
```

### Bonnes pratiques pour les unités

1. **Organiser les clauses `uses`** en groupes logiques (standard, tierces, projet)
2. **Déclarer toutes les interfaces dans la section `interface`**
3. **Implémenter le code dans la section `implementation`**
4. **Regrouper les méthodes par classe** plutôt que par fonctionnalité
5. **Conserver une seule classe principale par unité** quand c'est possible

## Gestion des dépendances

Un aspect crucial d'une bonne structure est la gestion des dépendances entre les différentes parties de votre application.

### Principes de base

1. **Évitez les dépendances circulaires** : Si l'unité A utilise l'unité B, alors B ne devrait pas utiliser A
2. **Dépendez des abstractions** : Utilisez des interfaces plutôt que des implémentations concrètes
3. **Injection de dépendances** : Transmettez les dépendances via le constructeur ou des propriétés

### Exemple d'injection de dépendances

```pascal
// Mauvaise approche (dépendance directe)
procedure TCustomerForm.LoadCustomer(const Id: Integer);
var
  CustomerService: TCustomerService;
begin
  CustomerService := TCustomerService.Create;
  try
    Customer := CustomerService.GetCustomerById(Id);
  finally
    CustomerService.Free;
  end;
end;

// Bonne approche (injection de dépendances)
procedure TCustomerForm.LoadCustomer(const Id: Integer;
                                    CustomerService: ICustomerService);
begin
  Customer := CustomerService.GetCustomerById(Id);
end;
```

## Gestion des fichiers dans l'IDE Delphi

Delphi offre plusieurs outils pour vous aider à organiser votre projet dans l'IDE :

### Groupes de projet

Pour les solutions comportant plusieurs projets liés :

1. Créez un groupe de projet via **Fichier > Nouveau > Groupe de projets**
2. Ajoutez vos projets existants au groupe
3. Organisez-les en dossiers virtuels selon leur fonction

![Groupe de projets Delphi](https://placeholder.com/Project_Group.png)

### Organisation des unités dans le Project Manager

Le Project Manager permet d'organiser visuellement vos fichiers :

1. Cliquez-droit sur le nom du projet
2. Sélectionnez **Ajouter > Nouveau dossier**
3. Glissez-déposez les unités dans les dossiers appropriés

Cette organisation n'affecte pas la structure réelle des fichiers sur le disque, mais facilite la navigation dans l'IDE.

## Modèles de structuration courants

Voici quelques modèles courants à considérer selon la taille et le type de votre application :

### 1. Modèle simple (petites applications)

- Un dossier principal contenant tous les fichiers
- Préfixes de nommage pour regrouper les fichiers liés (ex: `Cust_List.pas`, `Cust_Edit.pas`)
- Adapté aux applications de moins de 10-15 formulaires

### 2. Modèle MVC (Model-View-Controller)

- Séparation en trois types de composants :
  - **Modèles** : Représentation des données et logique métier
  - **Vues** : Formulaires et composants d'interface utilisateur
  - **Contrôleurs** : Coordination entre modèles et vues

### 3. Modèle modulaire

- Organisation par modules fonctionnels (Clients, Produits, Facturation...)
- Chaque module contient ses propres formes, classes et utilitaires
- Interfaces bien définies entre les modules

## Bonnes pratiques de nommage

Un bon système de nommage rend votre structure plus intuitive :

### Conventions pour les unités

- Préfixez par le nom du projet pour les unités principales : `MonProjet.MainForm.pas`
- Utilisez des noms complets et descriptifs : `CustomerManagement.pas` au lieu de `CustMgmt.pas`
- Pour les classes utilitaires, indiquez leur fonction : `StringUtils.pas`

### Conventions pour les classes

- Préfixez par `T` comme c'est la tradition en Delphi : `TCustomer`
- Pour les interfaces, préfixez par `I` : `ICustomerService`
- Pour les énumérations, préfixez par `E` : `ECustomerType`

### Conventions pour les fichiers de formulaires

- Suffixe `Form` pour les formulaires standards : `MainForm.pas`
- Suffixe `Dialog` pour les boîtes de dialogue : `SettingsDialog.pas`
- Suffixe `Frame` pour les cadres (TFrame) : `CustomerListFrame.pas`

## Considérations pratiques pour les débutants

Si vous débutez avec Delphi, voici quelques conseils simples :

1. **Commencez simple** : Ne surarchitecturez pas vos premiers projets
2. **Évoluez progressivement** : Améliorez la structure au fur et à mesure
3. **Réfléchissez avant d'ajouter** : Prenez l'habitude de vous demander "où ce fichier devrait-il aller ?"
4. **Observez des projets existants** : Examinez comment les projets open source sont structurés
5. **Soyez cohérent** : Une structure imparfaite mais cohérente est meilleure qu'une structure incohérente

## Structure pour les projets d'équipe

Si vous travaillez en équipe, considérez ces éléments supplémentaires :

1. **Document de conventions** : Créez un document définissant les règles de structure et de nommage
2. **Configuration d'IDE partagée** : Partagez les mêmes paramètres d'IDE entre les membres de l'équipe
3. **Contrôle de code source** : Utilisez Git ou SVN avec une structure de branches cohérente
4. **Revues de code** : Vérifiez que les nouvelles contributions respectent la structure convenue

## Évolution de la structure

Une bonne structure doit pouvoir évoluer avec votre application :

1. **Revues périodiques** : Examinez régulièrement si la structure reste adaptée
2. **Refactoring progressif** : Améliorez la structure par petites étapes
3. **Documentation** : Maintenez un document décrivant l'architecture et sa justification
4. **Mesure de la complexité** : Surveillez des métriques comme la complexité cyclomatique, la profondeur d'héritage, etc.

## Outils pour gérer la structure

Delphi et l'écosystème offrent plusieurs outils pour vous aider :

1. **ModelMaker Code Explorer** : Outil de refactoring et d'exploration de code
2. **GExperts** : Extensions pour Delphi avec des fonctionnalités d'organisation
3. **Delphi Code Formatter** : Pour maintenir un style de code cohérent
4. **Together** : Outil de modélisation UML intégré aux versions professionnelles de Delphi

## Exemple concret : Application de gestion de clients

Voyons comment structurer une petite application de gestion de clients :

```
GestionClients/
├── src/
│   ├── forms/
│   │   ├── Main.pas            # Formulaire principal
│   │   ├── CustomerList.pas    # Liste des clients
│   │   ├── CustomerEdit.pas    # Édition d'un client
│   │   └── About.pas           # À propos
│   ├── data/
│   │   ├── DataModule.pas      # Module de données
│   │   └── DatabaseConnection.pas # Connexion à la base
│   ├── models/
│   │   ├── Customer.pas        # Classe Client
│   │   └── Address.pas         # Classe Adresse
│   ├── services/
│   │   ├── CustomerService.pas # Gestion des clients
│   │   └── ReportService.pas   # Génération de rapports
│   └── utils/
│       ├── StringUtils.pas     # Utilitaires de chaînes
│       └── ValidationUtils.pas # Validation de données
├── res/
│   ├── icons/                  # Icônes de l'application
│   └── reports/                # Modèles de rapports
└── GestionClients.dpr          # Fichier projet
```

Cette structure est simple mais évolutive, adaptée à une petite application qui pourrait grandir avec le temps.

---

> **Astuce pour débutants** : Ne vous sentez pas obligé d'adopter immédiatement une structure complexe. Commencez avec une organisation simple, puis restructurez progressivement à mesure que votre projet grandit et que votre compréhension s'améliore. Une bonne structure est celle qui vous aide à naviguer et à comprendre votre propre code, même après plusieurs mois sans y avoir touché.

⏭️ [Patterns d'architecture (MVC, MVVM)](/18-architecture-et-bonnes-pratiques/02-patterns-darchitecture.md)
