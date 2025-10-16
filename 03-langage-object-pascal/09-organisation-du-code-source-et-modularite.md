🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 3.9 Organisation du code source et modularité

## Introduction

L'**organisation du code source** et la **modularité** sont essentielles pour créer des applications maintenables, évolutives et compréhensibles. Un code bien organisé est comme une maison bien rangée : on trouve facilement ce qu'on cherche et il est agréable d'y travailler.

## Pourquoi organiser son code ?

### Analogie du monde réel

Imaginez deux bibliothèques :

**Bibliothèque désorganisée** :
- Les livres sont empilés au hasard
- Pas de classification
- Impossible de trouver un livre spécifique
- Perte de temps constante

**Bibliothèque organisée** :
- Les livres sont classés par catégories
- Chaque section a sa place
- Étiquettes claires
- Facile de trouver et de ranger

Votre code doit être comme la bibliothèque organisée !

### Avantages d'un code bien organisé

1. **Lisibilité** : facile à comprendre
2. **Maintenabilité** : facile à modifier et corriger
3. **Réutilisabilité** : facile de réutiliser des parties du code
4. **Collaboration** : facile de travailler en équipe
5. **Évolutivité** : facile d'ajouter de nouvelles fonctionnalités
6. **Tests** : facile de tester chaque partie séparément
7. **Performance** : compilation plus rapide avec une bonne organisation

## Les unités (Units) en Delphi

### Qu'est-ce qu'une unité ?

Une **unité** (unit) est un fichier source Delphi qui contient du code réutilisable. C'est le bloc de construction fondamental pour organiser votre code en Delphi.

Chaque unité :
- Est un fichier `.pas`
- A un nom unique
- Contient des types, des classes, des fonctions, des procédures, des constantes
- Peut être réutilisée dans différents projets

### Structure d'une unité

```pascal
unit NomDeLUnite;

// Section INTERFACE : ce qui est visible de l'extérieur
interface

uses
  System.SysUtils, System.Classes;  // Unités utilisées

type
  // Déclaration des types, classes, etc.
  TMaClasse = class
  public
    procedure FaireQuelqueChose;
  end;

// Déclaration des fonctions et procédures publiques
function CalculerTotal(A, B: Integer): Integer;

// Section IMPLEMENTATION : le code interne
implementation

procedure TMaClasse.FaireQuelqueChose;
begin
  // Implémentation
end;

function CalculerTotal(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// Section INITIALIZATION (optionnelle)
initialization
  // Code exécuté au démarrage de l'application

// Section FINALIZATION (optionnelle)
finalization
  // Code exécuté à la fermeture de l'application

end.
```

### Les sections d'une unité

#### 1. Interface

La section **interface** définit ce qui est **visible et accessible** depuis l'extérieur de l'unité.

```pascal
interface

uses
  System.SysUtils;  // Unités nécessaires pour l'interface

type
  TCalculatrice = class
  public
    function Additionner(A, B: Integer): Integer;
  end;

// Cette fonction est accessible de l'extérieur
function CalculerMoyenne(Valeurs: array of Double): Double;
```

**Règle** : Tout ce qui est dans `interface` est public et utilisable par d'autres unités.

#### 2. Implementation

La section **implementation** contient le code réel des méthodes et peut contenir des éléments privés à l'unité.

```pascal
implementation

uses
  System.Math;  // Unité nécessaire uniquement pour l'implémentation

// Fonction privée, invisible de l'extérieur
function FonctionInterne: Integer;
begin
  Result := 42;
end;

function TCalculatrice.Additionner(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function CalculerMoyenne(Valeurs: array of Double): Double;
var
  I: Integer;
  Somme: Double;
begin
  Somme := 0;
  for I := Low(Valeurs) to High(Valeurs) do
    Somme := Somme + Valeurs[I];
  Result := Somme / Length(Valeurs);
end;
```

**Règle** : Ce qui est déclaré uniquement dans `implementation` est privé à l'unité.

#### 3. Initialization et Finalization (optionnelles)

```pascal
var
  CompteurGlobal: Integer;

initialization
  // Exécuté au démarrage de l'application
  CompteurGlobal := 0;
  WriteLn('Unité initialisée');

finalization
  // Exécuté à la fermeture de l'application
  WriteLn('Unité finalisée');
  // Libérer les ressources globales
```

## La clause Uses

### Qu'est-ce que la clause Uses ?

La clause `uses` indique quelles autres unités sont nécessaires pour votre code.

```pascal
unit MonUnite;

interface

uses
  System.SysUtils,    // Pour les fonctions de chaînes
  System.Classes,     // Pour TStringList, etc.
  Vcl.Dialogs;        // Pour ShowMessage

implementation

uses
  System.Math;        // Uniquement pour l'implémentation
```

### Uses dans interface vs implementation

```pascal
interface

uses
  System.SysUtils;  // Nécessaire pour l'interface publique
  // Les unités ici sont "propagées" aux utilisateurs de cette unité

implementation

uses
  System.Math;      // Nécessaire uniquement pour l'implémentation
  // Les unités ici restent privées à l'unité
```

**Règle d'or** : Mettez dans `interface/uses` uniquement ce qui est strictement nécessaire pour les déclarations publiques. Le reste va dans `implementation/uses`.

### Éviter les dépendances circulaires

**Problème** : Deux unités qui se référencent mutuellement.

```pascal
// Unit1.pas
unit Unit1;
interface
uses Unit2;  // Unit1 utilise Unit2

// Unit2.pas
unit Unit2;
interface
uses Unit1;  // Unit2 utilise Unit1 - ERREUR !
```

**Solution** : Déplacer l'une des références dans la section `implementation`.

```pascal
// Unit1.pas
unit Unit1;
interface
uses Unit2;

// Unit2.pas
unit Unit2;
interface
// Pas de uses Unit1 ici
implementation
uses Unit1;  // Référence déplacée ici
```

## Organisation par fonctionnalité

### Principe de séparation des responsabilités

Chaque unité doit avoir une **responsabilité unique et claire**.

```pascal
// ❌ Mauvais - une seule unité pour tout
unit ToutMonCode;
  // Classes de modèles
  // Classes de connexion DB
  // Classes d'interface
  // Fonctions utilitaires
  // Tout mélangé !
end.

// ✅ Bon - unités séparées par responsabilité
unit App.Modeles;      // Classes de données
unit App.Database;     // Accès aux données
unit App.Interface;    // Composants d'interface
unit App.Utilitaires;  // Fonctions utilitaires
```

### Exemple d'organisation par couches

```
Mon Application de Gestion
├── Modèles (données)
│   ├── App.Modeles.Client.pas
│   ├── App.Modeles.Commande.pas
│   └── App.Modeles.Produit.pas
│
├── Accès aux données
│   ├── App.Data.Connexion.pas
│   ├── App.Data.Client.pas
│   └── App.Data.Commande.pas
│
├── Logique métier
│   ├── App.Business.Vente.pas
│   └── App.Business.Stock.pas
│
└── Interface utilisateur
    ├── App.UI.MainForm.pas
    ├── App.UI.ClientForm.pas
    └── App.UI.CommandeForm.pas
```

## Structure des dossiers

### Organisation physique des fichiers

```
MonProjet/
├── Source/
│   ├── Modeles/
│   │   ├── Client.pas
│   │   ├── Commande.pas
│   │   └── Produit.pas
│   │
│   ├── Data/
│   │   ├── Connexion.pas
│   │   ├── ClientDAO.pas
│   │   └── CommandeDAO.pas
│   │
│   ├── Business/
│   │   ├── GestionVentes.pas
│   │   └── GestionStock.pas
│   │
│   ├── UI/
│   │   ├── MainForm.pas
│   │   ├── MainForm.dfm
│   │   ├── ClientForm.pas
│   │   └── ClientForm.dfm
│   │
│   └── Utils/
│       ├── StringUtils.pas
│       └── DateUtils.pas
│
├── Tests/
│   ├── TestClient.pas
│   └── TestCommande.pas
│
├── Resources/
│   ├── Images/
│   └── Icons/
│
└── Documentation/
    └── README.md
```

### Avantages de cette organisation

1. **Navigation facile** : on sait où chercher
2. **Compilation par modules** : changements localisés
3. **Travail en équipe** : moins de conflits
4. **Réutilisation** : modules indépendants
5. **Tests** : chaque module testable séparément

## Les Namespaces en Delphi

### Qu'est-ce qu'un namespace ?

Un **namespace** (espace de noms) est un préfixe qui permet d'organiser et de distinguer les unités.

```pascal
// Namespaces standards de Delphi
System.SysUtils      // Namespace System
System.Classes
System.Generics.Collections

Vcl.Forms            // Namespace Vcl (Visual Component Library)
Vcl.Dialogs
Vcl.Controls

FMX.Forms            // Namespace FMX (FireMonkey)
FMX.Types
```

### Créer vos propres namespaces

```pascal
// MonApp.Modeles.Client.pas
unit MonApp.Modeles.Client;

interface

type
  TClient = class
    // ...
  end;

implementation

end.
```

### Avantages des namespaces

1. **Organisation hiérarchique** : structure claire
2. **Évite les conflits de noms** : deux unités peuvent avoir le même nom dans des namespaces différents
3. **Indication de la fonction** : on comprend directement le rôle de l'unité

### Convention de nommage

```pascal
// ✅ Bon - structure claire
CompanyName.ProjectName.Category.Feature

// Exemples concrets
MonEntreprise.GestionStock.Modeles.Produit
MonEntreprise.GestionStock.Data.ProduitDAO
MonEntreprise.GestionStock.UI.ProduitForm

// ✅ Bon - simplifié
App.Models.Customer
App.Data.CustomerDAO
App.UI.CustomerForm
```

## Modularité et réutilisabilité

### Principe de modularité

La **modularité** consiste à diviser votre application en modules indépendants et réutilisables.

#### Exemple : Module de gestion des logs

```pascal
// App.Utils.Logger.pas
unit App.Utils.Logger;

interface

type
  TLogLevel = (llDebug, llInfo, llWarning, llError);

  TLogger = class
  private
    class var FInstance: TLogger;
    FLogFile: string;
  public
    class function Instance: TLogger;
    procedure Log(const Message: string; Level: TLogLevel = llInfo);
    procedure Debug(const Message: string);
    procedure Error(const Message: string);
  end;

implementation

uses
  System.SysUtils, System.IOUtils;

class function TLogger.Instance: TLogger;
begin
  if FInstance = nil then
    FInstance := TLogger.Create;
  Result := FInstance;
end;

procedure TLogger.Log(const Message: string; Level: TLogLevel);
var
  LogMessage: string;
begin
  LogMessage := Format('[%s] [%s] %s',
    [DateTimeToStr(Now), GetEnumName(TypeInfo(TLogLevel), Ord(Level)), Message]);
  // Écrire dans le fichier...
end;

procedure TLogger.Debug(const Message: string);
begin
  Log(Message, llDebug);
end;

procedure TLogger.Error(const Message: string);
begin
  Log(Message, llError);
end;

end.
```

Ce module peut être réutilisé dans n'importe quel projet !

### Créer des bibliothèques de code réutilisable

Regroupez les fonctionnalités communes dans des unités dédiées :

```pascal
// App.Utils.StringHelper.pas
unit App.Utils.StringHelper;

interface

type
  TStringHelper = class
  public
    class function EstEmail(const Email: string): Boolean;
    class function EstTelephone(const Tel: string): Boolean;
    class function CapitaliserMots(const Texte: string): string;
    class function RemplacerAccents(const Texte: string): string;
  end;

implementation

class function TStringHelper.EstEmail(const Email: string): Boolean;
begin
  // Validation d'email
  Result := Pos('@', Email) > 0;
end;

class function TStringHelper.EstTelephone(const Tel: string): Boolean;
begin
  // Validation de téléphone
  Result := Length(Tel) >= 10;
end;

class function TStringHelper.CapitaliserMots(const Texte: string): string;
begin
  // Mettre une majuscule au début de chaque mot
  Result := Texte;  // Implémentation simplifiée
end;

class function TStringHelper.RemplacerAccents(const Texte: string): string;
begin
  // Remplacer les accents
  Result := StringReplace(Texte, 'é', 'e', [rfReplaceAll]);
  // etc.
end;

end.
```

## Séparation des couches

### Architecture en couches

Organisez votre application en couches distinctes :

```
┌─────────────────────────────────┐
│   Couche Présentation (UI)      │  Formulaires, composants visuels
├─────────────────────────────────┤
│   Couche Logique Métier         │  Règles, calculs, validations
├─────────────────────────────────┤
│   Couche Accès aux Données      │  Connexion DB, requêtes
├─────────────────────────────────┤
│   Couche Données (Modèles)      │  Structures de données
└─────────────────────────────────┘
```

### Exemple concret

```pascal
// ===== COUCHE MODÈLE =====
// App.Models.Client.pas
unit App.Models.Client;

interface

type
  TClient = class
  private
    FID: Integer;
    FNom: string;
    FEmail: string;
  public
    property ID: Integer read FID write FID;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
  end;

implementation
end.

// ===== COUCHE DONNÉES =====
// App.Data.ClientDAO.pas
unit App.Data.ClientDAO;

interface

uses
  App.Models.Client, System.Generics.Collections;

type
  TClientDAO = class
  public
    function ObtenirTous: TList<TClient>;
    function ObtenirParID(ID: Integer): TClient;
    procedure Enregistrer(Client: TClient);
    procedure Supprimer(ID: Integer);
  end;

implementation

uses
  FireDAC.Comp.Client;  // Accès aux données

function TClientDAO.ObtenirTous: TList<TClient>;
begin
  Result := TList<TClient>.Create;
  // Code pour charger depuis la base de données
end;

function TClientDAO.ObtenirParID(ID: Integer): TClient;
begin
  Result := TClient.Create;
  // Code pour charger un client spécifique
end;

procedure TClientDAO.Enregistrer(Client: TClient);
begin
  // Code pour sauvegarder dans la base de données
end;

procedure TClientDAO.Supprimer(ID: Integer);
begin
  // Code pour supprimer de la base de données
end;

end.

// ===== COUCHE LOGIQUE MÉTIER =====
// App.Business.ClientService.pas
unit App.Business.ClientService;

interface

uses
  App.Models.Client, App.Data.ClientDAO;

type
  TClientService = class
  private
    FDAO: TClientDAO;
  public
    constructor Create;
    destructor Destroy; override;
    function ValiderEmail(const Email: string): Boolean;
    function CreerNouveauClient(const Nom, Email: string): TClient;
    procedure EnregistrerClient(Client: TClient);
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;

constructor TClientService.Create;
begin
  inherited Create;
  FDAO := TClientDAO.Create;
end;

destructor TClientService.Destroy;
begin
  FDAO.Free;
  inherited Destroy;
end;

function TClientService.ValiderEmail(const Email: string): Boolean;
begin
  // Règle métier : validation d'email
  Result := TRegEx.IsMatch(Email, '^[\w\.-]+@[\w\.-]+\.\w+$');
end;

function TClientService.CreerNouveauClient(const Nom, Email: string): TClient;
begin
  if not ValiderEmail(Email) then
    raise Exception.Create('Email invalide');

  Result := TClient.Create;
  Result.Nom := Nom;
  Result.Email := Email;
end;

procedure TClientService.EnregistrerClient(Client: TClient);
begin
  // Règles métier avant sauvegarde
  if Trim(Client.Nom) = '' then
    raise Exception.Create('Le nom est obligatoire');

  if not ValiderEmail(Client.Email) then
    raise Exception.Create('Email invalide');

  FDAO.Enregistrer(Client);
end;

end.

// ===== COUCHE PRÉSENTATION =====
// App.UI.ClientForm.pas
unit App.UI.ClientForm;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, App.Business.ClientService, App.Models.Client;

type
  TFormClient = class(TForm)
    EditNom: TEdit;
    EditEmail: TEdit;
    ButtonEnregistrer: TButton;
    procedure ButtonEnregistrerClick(Sender: TObject);
  private
    FService: TClientService;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Dialogs, System.SysUtils;

constructor TFormClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FService := TClientService.Create;
end;

destructor TFormClient.Destroy;
begin
  FService.Free;
  inherited Destroy;
end;

procedure TFormClient.ButtonEnregistrerClick(Sender: TObject);
var
  Client: TClient;
begin
  try
    Client := FService.CreerNouveauClient(EditNom.Text, EditEmail.Text);
    try
      FService.EnregistrerClient(Client);
      ShowMessage('Client enregistré avec succès');
    finally
      Client.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

end.
```

### Avantages de la séparation en couches

1. **Testabilité** : chaque couche peut être testée indépendamment
2. **Maintenance** : changements localisés
3. **Réutilisabilité** : les couches basses peuvent être réutilisées
4. **Clarté** : responsabilités bien définies
5. **Évolutivité** : facile d'ajouter des fonctionnalités

## Conventions de nommage

### Noms d'unités

```pascal
// ✅ Bon - descriptif et clair
App.Models.Customer.pas
App.Data.CustomerRepository.pas
App.Utils.StringHelper.pas

// ❌ Mauvais - trop vague
Unit1.pas
Utils.pas
Stuff.pas
```

### Structure de nommage recommandée

```
[Namespace].[Catégorie].[Fonctionnalité].pas

Exemples :
MonApp.Models.Product.pas
MonApp.Data.ProductDAO.pas
MonApp.Business.SalesService.pas
MonApp.UI.MainForm.pas
MonApp.Utils.Logger.pas
```

### Préfixes pour les types

| Type | Préfixe | Exemple |
|------|---------|---------|
| Classe | T | TClient, TCommande |
| Interface | I | IRepository, ILogger |
| Énumération | T | TStatus, TLogLevel |
| Record | T | TPoint, TRectangle |
| Formulaire | TForm | TFormClient, TFormMain |

## Documentation du code

### Commentaires XML

Delphi supporte les commentaires de documentation XML :

```pascal
/// <summary>
/// Calcule la moyenne d'un tableau de valeurs
/// </summary>
/// <param name="Valeurs">Tableau de valeurs à moyenner</param>
/// <returns>La moyenne des valeurs</returns>
/// <exception cref="Exception">Si le tableau est vide</exception>
function CalculerMoyenne(Valeurs: array of Double): Double;
```

### En-têtes d'unité

```pascal
{******************************************************************************}
{                                                                              }
{  Unité : App.Utils.StringHelper                                              }
{  Description : Fonctions utilitaires pour la manipulation de chaînes         }
{  Auteur : Votre Nom                                                          }
{  Date : 01/01/2025                                                           }
{  Version : 1.0                                                               }
{                                                                              }
{******************************************************************************}
unit App.Utils.StringHelper;
```

### Commentaires dans le code

```pascal
// ✅ Bon - explique le POURQUOI
// On utilise un timeout de 30s car certaines requêtes sont lentes
FDConnection.Timeout := 30000;

// ❌ Mauvais - dit ce que fait le code (déjà évident)
// Définit le timeout à 30000
FDConnection.Timeout := 30000;
```

## Gestion des dépendances

### Minimiser les dépendances

```pascal
// ❌ Mauvais - trop de dépendances
unit MonUnite;
interface
uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Grids, Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  System.Generics.Collections, System.JSON, System.RegularExpressions;

// ✅ Bon - dépendances minimales
unit MonUnite;
interface
uses
  System.SysUtils, System.Classes;  // Seulement ce qui est nécessaire

implementation
uses
  Vcl.Dialogs;  // Utilisé uniquement dans l'implémentation
```

### Inversion de dépendance

Utilisez des interfaces pour découpler les modules :

```pascal
// ❌ Mauvais - couplage fort
type
  TClientService = class
  private
    FMySQL: TMySQLConnection;  // Dépend d'une implémentation concrète
  end;

// ✅ Bon - couplage faible
type
  IDatabase = interface
    procedure Connect;
    procedure Execute(const SQL: string);
  end;

  TClientService = class
  private
    FDatabase: IDatabase;  // Dépend d'une abstraction
  end;
```

## Bonnes pratiques d'organisation

### 1. Une classe par unité (généralement)

```pascal
// ✅ Bon
// Client.pas - contient uniquement TClient
// Commande.pas - contient uniquement TCommande

// ⚠️ Acceptable si classes très liées
// Transaction.pas - contient TTransaction et TTransactionItem
```

### 2. Regrouper ce qui change ensemble

```pascal
// Classes qui changent ensemble → même unité
unit App.Models.Vente;

type
  TVente = class
    // ...
  end;

  TLigneVente = class  // Très lié à TVente
    // ...
  end;
```

### 3. Séparer interface et implémentation

```pascal
// IRepository.pas - interface uniquement
unit App.Interfaces.IRepository;

interface

type
  IRepository<T> = interface
    procedure Save(Entity: T);
    function GetById(ID: Integer): T;
  end;

implementation
end.

// ClientRepository.pas - implémentation
unit App.Data.ClientRepository;

interface

uses
  App.Interfaces.IRepository, App.Models.Client;

type
  TClientRepository = class(TInterfacedObject, IRepository<TClient>)
  public
    procedure Save(Entity: TClient);
    function GetById(ID: Integer): TClient;
  end;

implementation
// ...
end.
```

### 4. Utiliser des fichiers include avec parcimonie

```pascal
// Config.inc - constantes de configuration
const
  APP_VERSION = '1.0.0';
  MAX_CONNECTIONS = 100;

// Dans votre unité
unit MonUnite;

interface

{$I Config.inc}  // Inclure le fichier

implementation
end.
```

**Attention** : Ne pas abuser des includes, préférez les unités normales.

### 5. Organiser les uses alphabétiquement

```pascal
// ✅ Bon - facile de voir ce qui est utilisé
uses
  Data.DB,
  FireDAC.Comp.Client,
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  Vcl.Dialogs;

// ❌ Mauvais - désorganisé
uses
  Vcl.Dialogs, System.SysUtils, FireDAC.Comp.Client, Data.DB,
  System.Classes;
```

## Exemple d'application complète bien organisée

```
GestionCommerciale/
├── Source/
│   ├── Core/
│   │   ├── App.Core.Interfaces.pas      (Interfaces communes)
│   │   ├── App.Core.Types.pas           (Types communs)
│   │   └── App.Core.Constants.pas       (Constantes)
│   │
│   ├── Models/
│   │   ├── App.Models.Client.pas
│   │   ├── App.Models.Produit.pas
│   │   ├── App.Models.Commande.pas
│   │   └── App.Models.Facture.pas
│   │
│   ├── Data/
│   │   ├── App.Data.Connection.pas
│   │   ├── App.Data.ClientDAO.pas
│   │   ├── App.Data.ProduitDAO.pas
│   │   └── App.Data.CommandeDAO.pas
│   │
│   ├── Business/
│   │   ├── App.Business.ClientService.pas
│   │   ├── App.Business.VenteService.pas
│   │   └── App.Business.StockService.pas
│   │
│   ├── UI/
│   │   ├── Forms/
│   │   │   ├── App.UI.MainForm.pas/.dfm
│   │   │   ├── App.UI.ClientForm.pas/.dfm
│   │   │   └── App.UI.CommandeForm.pas/.dfm
│   │   │
│   │   └── Components/
│   │       ├── App.UI.ClientGrid.pas
│   │       └── App.UI.SearchPanel.pas
│   │
│   └── Utils/
│       ├── App.Utils.Logger.pas
│       ├── App.Utils.StringHelper.pas
│       └── App.Utils.Validator.pas
│
├── Tests/
│   ├── Tests.Models.pas
│   ├── Tests.Data.pas
│   └── Tests.Business.pas
│
└── Resources/
    ├── Images/
    ├── SQL/
    └── Config/
```

## Outils et fonctionnalités de l'IDE

### Navigation dans le code

- **Ctrl + Clic** : aller à la définition
- **Ctrl + Shift + Flèches** : naviguer entre déclaration/implémentation
- **Ctrl + G** : aller à la ligne
- **Ctrl + Q + Q** : marquer/revenir à une position

### Refactoring

- **Renommer** (Ctrl + Shift + E) : renommer un symbole partout
- **Extraire méthode** : créer une méthode depuis du code sélectionné
- **Déplacer** : déplacer des méthodes entre classes

### Gestionnaire de projet

Organisez vos fichiers en groupes logiques dans le gestionnaire de projet.

## Résumé

- **Unités** : blocs de construction fondamentaux
  - Section `interface` : ce qui est public
  - Section `implementation` : le code interne
  - Clause `uses` : gérer les dépendances

- **Organisation par fonctionnalité**
  - Une responsabilité par unité
  - Séparation en couches (Modèles, Data, Business, UI)
  - Structure de dossiers claire

- **Namespaces** : organiser hiérarchiquement
  - Format : `Company.Project.Category.Feature`
  - Évite les conflits de noms

- **Modularité**
  - Modules indépendants et réutilisables
  - Minimiser les dépendances
  - Utiliser l'inversion de dépendance

- **Bonnes pratiques**
  - Nommage cohérent et descriptif
  - Documentation du code
  - Une classe par unité (généralement)
  - Uses alphabétiques
  - Séparer interface et implémentation

- **Avantages**
  - Code lisible et maintenable
  - Facile à tester
  - Réutilisable
  - Évolutif
  - Travail en équipe facilité

Un code bien organisé est un investissement qui se rentabilise rapidement. Prenez le temps d'organiser correctement dès le début, vous gagnerez beaucoup de temps par la suite !

⏭️ [Nouveautés de la syntaxe Object Pascal (dernières versions)](/03-langage-object-pascal/10-nouveautes-de-la-syntaxe-object-pascal.md)
