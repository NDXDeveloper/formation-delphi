🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.7 Tests d'intégration

## Introduction

Imaginez que vous avez construit une voiture. Vous avez testé chaque pièce individuellement : le moteur fonctionne, les freins fonctionnent, les roues tournent. Mais est-ce que tout fonctionne ensemble quand vous assemblez la voiture complète ? C'est exactement la différence entre les tests unitaires et les tests d'intégration.

Les **tests d'intégration** vérifient que les différentes parties de votre application fonctionnent correctement ensemble. Ils testent les interactions entre vos modules, avec les bases de données, les services web, les fichiers, et d'autres systèmes externes.

Pour un débutant, comprendre cette distinction est crucial : les tests unitaires vérifient les pièces individuelles, les tests d'intégration vérifient que l'assemblage fonctionne.

## Qu'est-ce qu'un test d'intégration ?

### Définition simple

Un test d'intégration vérifie que plusieurs composants de votre application fonctionnent correctement ensemble. Il teste les **interactions** et les **interfaces** entre les différentes parties du système.

### Différence avec les tests unitaires

**Tests unitaires :**
- Testent une seule fonction ou méthode isolément
- Très rapides (millisecondes)
- N'utilisent pas de ressources externes réelles (mocks)
- Faciles à déboguer (scope limité)
- Nombreux (des centaines ou milliers)

**Tests d'intégration :**
- Testent plusieurs composants ensemble
- Plus lents (secondes ou minutes)
- Utilisent de vraies ressources (base de données de test, vraies APIs)
- Plus complexes à déboguer
- Moins nombreux mais plus complets

### Analogie pour comprendre

**Tests unitaires** = Tester chaque ingrédient d'une recette individuellement
- Le sucre est-il sucré ?
- La farine est-elle fine ?
- Les œufs sont-ils frais ?

**Tests d'intégration** = Tester la recette complète
- Est-ce que le gâteau a bon goût quand on mélange tous les ingrédients ?
- Est-ce qu'il monte correctement au four ?
- Est-ce que la texture est bonne ?

### Exemples de tests d'intégration

**Test base de données :**
```
1. Se connecter à une vraie base de données de test
2. Insérer un client
3. Vérifier qu'il apparaît dans la liste des clients
4. Modifier le client
5. Vérifier que les modifications sont sauvegardées
6. Supprimer le client
7. Vérifier qu'il n'apparaît plus
```

**Test API REST :**
```
1. Envoyer une requête HTTP à l'API
2. Vérifier le code de statut (200, 404, etc.)
3. Vérifier le format de la réponse (JSON valide)
4. Vérifier que les données sont correctes
5. Tester la gestion des erreurs
```

**Test interface utilisateur :**
```
1. Ouvrir un formulaire
2. Remplir les champs
3. Cliquer sur un bouton
4. Vérifier que l'action attendue se produit
5. Vérifier que l'interface se met à jour correctement
```

## Pourquoi les tests d'intégration sont importants

### Détecter les problèmes d'interaction

Même si chaque composant fonctionne isolément, ils peuvent ne pas fonctionner ensemble :

```pascal
// Ces deux fonctions fonctionnent parfaitement seules...
function ChargerClient(ID: Integer): TClient; // ✓ Tests unitaires OK  
function CalculerRemise(Client: TClient): Double; // ✓ Tests unitaires OK  

// ...mais ensemble, il y a un problème :
procedure AppliquerRemiseClient(ClientID: Integer);  
var  
  Client: TClient;
  Remise: Double;
begin
  Client := ChargerClient(ClientID);
  Remise := CalculerRemise(Client);  // ✗ Client pas initialisé si ID invalide !
  // Problème détecté uniquement par un test d'intégration
end;
```

### Tester les scénarios réels

Les tests d'intégration simulent ce que font vraiment vos utilisateurs :
- Connexion → Navigation → Action → Déconnexion
- Créer → Modifier → Consulter → Supprimer
- Entrée de données → Validation → Sauvegarde → Confirmation

### Vérifier la configuration

Les tests d'intégration détectent :
- Des problèmes de configuration de base de données
- Des erreurs de chaînes de connexion
- Des permissions manquantes
- Des dépendances non installées

### Confiance avant le déploiement

Avant de mettre en production, les tests d'intégration vous donnent confiance que le système fonctionne dans son ensemble.

## Pyramide des tests

### Le concept

La "pyramide des tests" est un modèle qui montre comment équilibrer les différents types de tests :

```
        /\
       /UI\         ← Peu de tests UI (lents, fragiles)
      /────\
     /Intég.\       ← Quelques tests d'intégration (moyennement rapides)
    /────────\
   / Unitaires\     ← Beaucoup de tests unitaires (rapides, ciblés)
  /────────────\
```

### Pourquoi cette forme ?

**Base large (tests unitaires) :**
- Nombreux
- Rapides
- Fiables
- Faciles à maintenir
- S'exécutent à chaque compilation

**Milieu (tests d'intégration) :**
- Quantité modérée
- Vitesse moyenne
- Testent les interactions critiques
- S'exécutent régulièrement

**Sommet (tests UI) :**
- Peu nombreux
- Lents
- Fragiles (se cassent facilement)
- Testent les parcours utilisateur critiques
- S'exécutent avant les releases

### Ratio recommandé

Pour une application typique :
- **70%** de tests unitaires
- **20%** de tests d'intégration
- **10%** de tests UI

## Types de tests d'intégration

### 1. Tests de base de données

Ces tests vérifient que votre application interagit correctement avec la base de données.

**Ce qu'on teste :**
- Les connexions
- Les requêtes SQL (SELECT, INSERT, UPDATE, DELETE)
- Les transactions
- Les contraintes d'intégrité
- Les procédures stockées
- Les performances

**Exemple conceptuel :**

```pascal
procedure TestAjouterClient;  
var  
  Client: TClient;
  ID: Integer;
begin
  // Arrange : Préparer les données
  Client := TClient.Create;
  Client.Nom := 'Dupont';
  Client.Prenom := 'Jean';
  Client.Email := 'jean.dupont@example.com';

  // Act : Exécuter l'action
  ID := GestionnaireClients.Ajouter(Client);

  // Assert : Vérifier le résultat
  Assert.IsTrue(ID > 0, 'ID doit être généré');

  // Vérifier que le client est bien en base
  Client := GestionnaireClients.ChargerParID(ID);
  Assert.IsNotNull(Client, 'Client doit exister en base');
  Assert.AreEqual('Dupont', Client.Nom);
  Assert.AreEqual('Jean', Client.Prenom);

  // Cleanup : Nettoyer
  GestionnaireClients.Supprimer(ID);
end;
```

### 2. Tests de services web (API REST)

Vérifient que votre application communique correctement avec des APIs externes ou vos propres services.

**Ce qu'on teste :**
- Les requêtes HTTP (GET, POST, PUT, DELETE)
- Les codes de statut
- Le format des réponses (JSON, XML)
- La gestion des erreurs
- Les timeouts
- L'authentification

**Exemple conceptuel :**

```pascal
procedure TestRecupererUtilisateur;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  JSONValue: TJSONValue;
  UserID: Integer;
begin
  // Arrange
  RESTClient := TRESTClient.Create('https://api.example.com');
  RESTRequest := TRESTRequest.Create(nil);
  RESTResponse := TRESTResponse.Create(nil);

  try
    RESTRequest.Client := RESTClient;
    RESTRequest.Response := RESTResponse;
    RESTRequest.Resource := 'users/123';
    RESTRequest.Method := rmGET;

    // Act : Exécuter la requête
    RESTRequest.Execute;

    // Assert : Vérifier la réponse
    Assert.AreEqual(200, RESTResponse.StatusCode, 'Status code doit être 200');

    JSONValue := RESTResponse.JSONValue;
    Assert.IsNotNull(JSONValue, 'La réponse doit contenir du JSON');

    UserID := JSONValue.GetValue<Integer>('id');
    Assert.AreEqual(123, UserID, 'L''ID utilisateur doit être 123');

  finally
    RESTClient.Free;
    RESTRequest.Free;
    RESTResponse.Free;
  end;
end;
```

### 3. Tests de fichiers et I/O

Vérifient les opérations de lecture/écriture de fichiers.

**Ce qu'on teste :**
- Lecture/écriture de fichiers
- Gestion des chemins
- Gestion des erreurs (fichier inexistant, permissions)
- Formats de fichiers (CSV, XML, JSON)
- Compression/décompression

**Exemple conceptuel :**

```pascal
procedure TestExporterClientsCSV;  
var  
  Clients: TList<TClient>;
  CheminFichier: string;
  Contenu: TStringList;
begin
  // Arrange
  Clients := TList<TClient>.Create;
  try
    Clients.Add(CreerClientTest('Dupont', 'Jean'));
    Clients.Add(CreerClientTest('Martin', 'Marie'));

    CheminFichier := TPath.GetTempPath + 'clients_test.csv';

    // Act : Exporter
    ExporteurCSV.ExporterClients(Clients, CheminFichier);

    // Assert : Vérifier que le fichier existe et contient les bonnes données
    Assert.IsTrue(TFile.Exists(CheminFichier), 'Le fichier doit exister');

    Contenu := TStringList.Create;
    try
      Contenu.LoadFromFile(CheminFichier);
      Assert.AreEqual(3, Contenu.Count, 'Header + 2 clients');
      Assert.Contains('Dupont', Contenu[1], 'Première ligne doit contenir Dupont');
      Assert.Contains('Martin', Contenu[2], 'Deuxième ligne doit contenir Martin');
    finally
      Contenu.Free;
    end;

    // Cleanup
    TFile.Delete(CheminFichier);
  finally
    Clients.Free;
  end;
end;
```

### 4. Tests de communication inter-processus

Pour les applications qui communiquent avec d'autres processus ou services.

**Ce qu'on teste :**
- Sockets TCP/IP
- Named pipes
- Message queues
- COM/DCOM
- WebSockets

### 5. Tests d'interface utilisateur

Les tests UI sont techniquement des tests d'intégration car ils testent l'interaction entre l'interface et la logique métier.

**Ce qu'on teste :**
- Ouverture de formulaires
- Saisie dans les contrôles
- Clics sur les boutons
- Validation des données
- Mise à jour de l'interface

**Note :** Les tests UI purs sont complexes en Delphi et nécessitent des outils spécialisés. On se concentre souvent sur les tests des couches métier et données.

## Configuration de l'environnement de test

### Environnements séparés

Il est crucial d'avoir des environnements distincts :

**Développement (Dev)**
- Base de données locale
- Services de test
- Données factices

**Test (Test/QA)**
- Base de données de test dédiée
- Services de test stables
- Données représentatives

**Production (Prod)**
- Base de données réelle
- Services réels
- Données réelles

**Règle d'or :** Ne JAMAIS tester sur la base de production !

### Base de données de test

**Approche 1 : Base de données dédiée**

Créez une base de données séparée pour les tests :

```sql
-- Création d'une base de test
CREATE DATABASE MyApp_Test;

-- Même structure que la base de prod
USE MyApp_Test;
-- ... créer les tables ...
```

**Approche 2 : Base en mémoire (SQLite)**

Pour des tests très rapides :

```pascal
procedure ConfigurerBaseDeDonneesTest;  
begin  
  FDConnection.DriverName := 'SQLite';
  FDConnection.Params.Database := ':memory:';  // Base en RAM
  FDConnection.Connected := True;

  // Créer la structure
  CreerTablesTest;
  // Insérer des données de test
  InsererDonneesTest;
end;
```

**Approche 3 : Conteneurs Docker**

Utilisez Docker pour créer des environnements de test isolés :

```yaml
# docker-compose.yml
version: '3'  
services:  
  mysql-test:
    image: mysql:8.0
    environment:
      MYSQL_DATABASE: myapp_test
      MYSQL_ROOT_PASSWORD: test123
    ports:
      - "3307:3306"
```

### Configuration dans le code

Utilisez des fichiers de configuration différents :

```pascal
type
  TEnvironment = (envDev, envTest, envProd);

function ChargerConfiguration(Env: TEnvironment): TConfiguration;  
begin  
  case Env of
    envDev:  Result := ChargerFichier('config.dev.ini');
    envTest: Result := ChargerFichier('config.test.ini');
    envProd: Result := ChargerFichier('config.prod.ini');
  end;
end;
```

**Fichier config.test.ini :**

```ini
[Database]
Server=localhost  
Port=3307  
Database=myapp_test  
Username=test_user  
Password=test_password  

[API]
BaseURL=https://api-test.example.com  
Timeout=5000  

[Logging]
Level=DEBUG  
FilePath=./logs/test.log  
```

## Stratégies de tests d'intégration

### 1. Tests top-down

Commencer par les niveaux supérieurs et descendre progressivement.

**Exemple :**
```
1. Tester le contrôleur principal
   ↓
2. Tester les services métier
   ↓
3. Tester les repositories de données
   ↓
4. Tester la base de données
```

**Avantages :**
- Teste rapidement les parcours utilisateur importants
- Trouve les problèmes architecturaux tôt

**Inconvénients :**
- Nécessite des mocks pour les couches basses non encore testées

### 2. Tests bottom-up

Commencer par les niveaux bas et monter progressivement.

**Exemple :**
```
1. Tester la base de données
   ↓
2. Tester les repositories
   ↓
3. Tester les services métier
   ↓
4. Tester les contrôleurs
```

**Avantages :**
- Les fondations sont solides avant de tester le reste
- Pas besoin de mocks pour les couches déjà testées

**Inconvénients :**
- Les problèmes de haut niveau sont détectés tard

### 3. Sandwich (hybride)

Combiner les deux approches : tester simultanément le haut et le bas, puis la couche intermédiaire.

```
Haut (UI/Controllers) ←→ [Couche intermédiaire] ←→ Bas (DB/Services)
         ↓                         ↓                        ↓
      Testés en                Testés en               Testés en
      parallèle                dernier                 parallèle
```

### 4. Big Bang

Assembler tous les modules et tout tester en une fois.

**⚠️ NON RECOMMANDÉ** : Difficile de trouver l'origine des bugs.

## Mocks et Stubs dans les tests d'intégration

### Quand utiliser des mocks

Dans les tests d'intégration, on utilise généralement de vraies ressources, mais parfois on a besoin de mocks pour :

**Services externes non fiables**
- Un service web qui est souvent en panne
- Une API avec un quota limité
- Un service payant qu'on ne veut pas solliciter constamment

**Ressources difficiles à configurer**
- Un système de paiement réel
- Un serveur email
- Un service SMS

**Scénarios d'erreur**
- Simuler une panne réseau
- Simuler un timeout
- Simuler une erreur serveur

### Créer un mock simple

**Interface :**

```pascal
type
  IServiceEmail = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;
  end;
```

**Implémentation réelle :**

```pascal
type
  TServiceEmailReel = class(TInterfacedObject, IServiceEmail)
  public
    function EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;
  end;

function TServiceEmailReel.EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;  
begin  
  // Envoie réellement l'email via SMTP
  Result := SMTPClient.Envoyer(Destinataire, Sujet, Corps);
end;
```

**Mock pour les tests :**

```pascal
type
  TServiceEmailMock = class(TInterfacedObject, IServiceEmail)
  private
    FEmailsEnvoyes: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    function EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;
    function VerifierEmailEnvoye(const Destinataire: string): Boolean;
    procedure Reinitialiser;
  end;

constructor TServiceEmailMock.Create;  
begin  
  FEmailsEnvoyes := TList<string>.Create;
end;

function TServiceEmailMock.EnvoyerEmail(const Destinataire, Sujet, Corps: string): Boolean;  
begin  
  // Ne fait qu'enregistrer qu'un email a été "envoyé"
  FEmailsEnvoyes.Add(Destinataire);
  Result := True;
end;

function TServiceEmailMock.VerifierEmailEnvoye(const Destinataire: string): Boolean;  
begin  
  Result := FEmailsEnvoyes.Contains(Destinataire);
end;
```

**Utilisation dans un test :**

```pascal
procedure TestEnvoiConfirmationCommande;  
var  
  ServiceEmail: IServiceEmail;
  GestionnaireCommandes: TGestionnaireCommandes;
begin
  // Arrange : Utiliser le mock au lieu du service réel
  ServiceEmail := TServiceEmailMock.Create;
  GestionnaireCommandes := TGestionnaireCommandes.Create(ServiceEmail);

  try
    // Act : Créer une commande
    GestionnaireCommandes.CreerCommande(ClientID, Produits);

    // Assert : Vérifier qu'un email a été "envoyé"
    Assert.IsTrue(
      (ServiceEmail as TServiceEmailMock).VerifierEmailEnvoye('client@example.com'),
      'Un email de confirmation doit être envoyé'
    );
  finally
    GestionnaireCommandes.Free;
  end;
end;
```

### Frameworks de mocking

Pour des mocks plus sophistiqués, utilisez des frameworks :

**Delphi Mocks**
- Création de mocks dynamiques
- Vérification des appels
- Configuration du comportement

**Spring4D Mocking**
- Partie du framework Spring4D
- Mocks basés sur des interfaces
- Syntaxe fluide

## Structure d'un projet de tests d'intégration

### Organisation des dossiers

```
MonProjet/
├── Source/
│   ├── Controllers/
│   ├── Services/
│   ├── Repositories/
│   └── Models/
├── Tests/
│   ├── Unit/
│   │   ├── TestControllers.pas
│   │   ├── TestServices.pas
│   │   └── TestModels.pas
│   └── Integration/
│       ├── TestDatabaseIntegration.pas
│       ├── TestAPIIntegration.pas
│       ├── TestFileOperations.pas
│       └── Helpers/
│           ├── TestDatabaseHelper.pas
│           └── TestDataBuilder.pas
└── Config/
    ├── config.dev.ini
    ├── config.test.ini
    └── config.prod.ini
```

### Classe de base pour tests d'intégration

```pascal
unit BaseIntegrationTest;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TBaseIntegrationTest = class
  protected
    procedure SetupDatabase; virtual;
    procedure CleanupDatabase; virtual;
    procedure SetupTestData; virtual;
  public
    [SetupFixture]
    procedure SetupFixture;

    [TearDownFixture]
    procedure TearDownFixture;

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;
  end;

implementation

procedure TBaseIntegrationTest.SetupFixture;  
begin  
  // Configuration une fois pour tous les tests de la fixture
  ChargerConfiguration(envTest);
end;

procedure TBaseIntegrationTest.TearDownFixture;  
begin  
  // Nettoyage final
end;

procedure TBaseIntegrationTest.Setup;  
begin  
  // Avant chaque test
  SetupDatabase;
  SetupTestData;
end;

procedure TBaseIntegrationTest.TearDown;  
begin  
  // Après chaque test
  CleanupDatabase;
end;

procedure TBaseIntegrationTest.SetupDatabase;  
begin  
  // Se connecter à la base de test
  // Créer les tables si nécessaire
end;

procedure TBaseIntegrationTest.CleanupDatabase;  
begin  
  // Supprimer les données de test
  // Réinitialiser les séquences
end;

procedure TBaseIntegrationTest.SetupTestData;  
begin  
  // Insérer des données de test communes
end;

end.
```

### Tests d'intégration de base de données

```pascal
unit TestDatabaseIntegration;

interface

uses
  DUnitX.TestFramework,
  BaseIntegrationTest,
  ClientRepository,
  ClientModel;

type
  [TestFixture]
  TTestClientRepository = class(TBaseIntegrationTest)
  private
    FRepository: TClientRepository;
  protected
    procedure SetupDatabase; override;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_AjouterClient_DoitCreerClientEnBase;

    [Test]
    procedure Test_ChargerClient_DoitRetournerClientExistant;

    [Test]
    procedure Test_ModifierClient_DoitMettreAJourDonnees;

    [Test]
    procedure Test_SupprimerClient_DoitRetirerClientDeBase;

    [Test]
    procedure Test_ListerClients_DoitRetournerTousLesClients;
  end;

implementation

uses
  System.SysUtils;

procedure TTestClientRepository.SetupDatabase;  
begin  
  inherited;
  // Configuration spécifique pour les tests de clients
end;

procedure TTestClientRepository.Setup;  
begin  
  inherited;
  FRepository := TClientRepository.Create;
end;

procedure TTestClientRepository.TearDown;  
begin  
  FRepository.Free;
  inherited;
end;

procedure TTestClientRepository.Test_AjouterClient_DoitCreerClientEnBase;  
var  
  Client: TClient;
  ID: Integer;
  ClientCharge: TClient;
begin
  // Arrange
  Client := TClient.Create;
  try
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean.dupont@example.com';
    Client.Telephone := '0123456789';

    // Act
    ID := FRepository.Ajouter(Client);

    // Assert
    Assert.IsTrue(ID > 0, 'ID doit être généré et positif');

    // Vérifier que le client existe vraiment en base
    ClientCharge := FRepository.ChargerParID(ID);
    try
      Assert.IsNotNull(ClientCharge, 'Le client doit exister en base');
      Assert.AreEqual('Dupont', ClientCharge.Nom, 'Le nom doit correspondre');
      Assert.AreEqual('Jean', ClientCharge.Prenom, 'Le prénom doit correspondre');
      Assert.AreEqual('jean.dupont@example.com', ClientCharge.Email, 'L''email doit correspondre');
    finally
      ClientCharge.Free;
    end;
  finally
    Client.Free;
  end;
end;

procedure TTestClientRepository.Test_ChargerClient_DoitRetournerClientExistant;  
var  
  Client: TClient;
  ID: Integer;
  ClientCharge: TClient;
begin
  // Arrange : Créer un client en base
  Client := TClient.Create;
  try
    Client.Nom := 'Martin';
    Client.Prenom := 'Marie';
    Client.Email := 'marie.martin@example.com';
    ID := FRepository.Ajouter(Client);
  finally
    Client.Free;
  end;

  // Act : Charger le client
  ClientCharge := FRepository.ChargerParID(ID);

  // Assert
  try
    Assert.IsNotNull(ClientCharge, 'Le client doit être chargé');
    Assert.AreEqual(ID, ClientCharge.ID, 'L''ID doit correspondre');
    Assert.AreEqual('Martin', ClientCharge.Nom);
    Assert.AreEqual('Marie', ClientCharge.Prenom);
  finally
    ClientCharge.Free;
  end;
end;

procedure TTestClientRepository.Test_ModifierClient_DoitMettreAJourDonnees;  
var  
  Client: TClient;
  ID: Integer;
  ClientModifie: TClient;
begin
  // Arrange : Créer un client
  Client := TClient.Create;
  try
    Client.Nom := 'Durand';
    Client.Prenom := 'Pierre';
    Client.Email := 'pierre.durand@example.com';
    ID := FRepository.Ajouter(Client);
  finally
    Client.Free;
  end;

  // Act : Modifier le client
  Client := FRepository.ChargerParID(ID);
  try
    Client.Email := 'nouveau.email@example.com';
    Client.Telephone := '9876543210';
    FRepository.Modifier(Client);
  finally
    Client.Free;
  end;

  // Assert : Vérifier les modifications
  ClientModifie := FRepository.ChargerParID(ID);
  try
    Assert.AreEqual('nouveau.email@example.com', ClientModifie.Email, 'L''email doit être modifié');
    Assert.AreEqual('9876543210', ClientModifie.Telephone, 'Le téléphone doit être modifié');
  finally
    ClientModifie.Free;
  end;
end;

procedure TTestClientRepository.Test_SupprimerClient_DoitRetirerClientDeBase;  
var  
  Client: TClient;
  ID: Integer;
  ClientCharge: TClient;
begin
  // Arrange : Créer un client
  Client := TClient.Create;
  try
    Client.Nom := 'Leroy';
    Client.Prenom := 'Sophie';
    Client.Email := 'sophie.leroy@example.com';
    ID := FRepository.Ajouter(Client);
  finally
    Client.Free;
  end;

  // Act : Supprimer le client
  FRepository.Supprimer(ID);

  // Assert : Vérifier qu'il n'existe plus
  ClientCharge := FRepository.ChargerParID(ID);
  Assert.IsNull(ClientCharge, 'Le client ne doit plus exister en base');
end;

procedure TTestClientRepository.Test_ListerClients_DoitRetournerTousLesClients;  
var  
  Client1, Client2: TClient;
  ID1, ID2: Integer;
  Clients: TList<TClient>;
begin
  // Arrange : Créer plusieurs clients
  Client1 := TClient.Create;
  try
    Client1.Nom := 'Dupont';
    Client1.Prenom := 'Jean';
    Client1.Email := 'jean@example.com';
    ID1 := FRepository.Ajouter(Client1);
  finally
    Client1.Free;
  end;

  Client2 := TClient.Create;
  try
    Client2.Nom := 'Martin';
    Client2.Prenom := 'Marie';
    Client2.Email := 'marie@example.com';
    ID2 := FRepository.Ajouter(Client2);
  finally
    Client2.Free;
  end;

  // Act : Lister tous les clients
  Clients := FRepository.ListerTous;

  // Assert
  try
    Assert.IsTrue(Clients.Count >= 2, 'Au moins 2 clients doivent être listés');

    // Vérifier que nos clients sont dans la liste
    Assert.IsTrue(
      Clients.Exists(function(C: TClient): Boolean
      begin
        Result := C.ID = ID1;
      end),
      'Le premier client doit être dans la liste'
    );

    Assert.IsTrue(
      Clients.Exists(function(C: TClient): Boolean
      begin
        Result := C.ID = ID2;
      end),
      'Le second client doit être dans la liste'
    );
  finally
    Clients.Free;
  end;
end;

end.
```

## Data builders pour les tests

Les **data builders** (ou **test data builders**) sont des classes helper qui créent des objets de test avec des valeurs par défaut raisonnables.

### Pourquoi utiliser des builders ?

**Sans builder :**

```pascal
procedure TestMethode1;  
var  
  Client: TClient;
begin
  Client := TClient.Create;
  Client.Nom := 'Dupont';
  Client.Prenom := 'Jean';
  Client.Email := 'jean@example.com';
  Client.Telephone := '0123456789';
  Client.Adresse := '1 rue de la Paix';
  Client.CodePostal := '75001';
  Client.Ville := 'Paris';
  Client.DateInscription := Now;
  // ...
end;
```

Beaucoup de code répétitif !

**Avec builder :**

```pascal
procedure TestMethode1;  
var  
  Client: TClient;
begin
  Client := TClientBuilder.UnClientParDefaut;
  // Ou personnaliser juste ce qui est nécessaire
  Client := TClientBuilder.UnClient
    .AvecNom('Dupont')
    .AvecEmail('jean@example.com')
    .Construire;
end;
```

### Implémentation d'un builder

```pascal
type
  TClientBuilder = class
  private
    FClient: TClient;
  public
    constructor Create;
    destructor Destroy; override;

    class function UnClient: TClientBuilder;
    class function UnClientParDefaut: TClient;

    function AvecNom(const Nom: string): TClientBuilder;
    function AvecPrenom(const Prenom: string): TClientBuilder;
    function AvecEmail(const Email: string): TClientBuilder;
    function AvecTelephone(const Telephone: string): TClientBuilder;
    function AvecAdresse(const Adresse, CodePostal, Ville: string): TClientBuilder;

    function Construire: TClient;
  end;

implementation

constructor TClientBuilder.Create;  
begin  
  FClient := TClient.Create;
  // Valeurs par défaut
  FClient.Nom := 'NomTest';
  FClient.Prenom := 'PrenomTest';
  FClient.Email := Format('test%d@example.com', [Random(10000)]);
  FClient.Telephone := '0123456789';
  FClient.Adresse := '1 rue Test';
  FClient.CodePostal := '75001';
  FClient.Ville := 'Paris';
  FClient.DateInscription := Now;
end;

class function TClientBuilder.UnClient: TClientBuilder;  
begin  
  Result := TClientBuilder.Create;
end;

class function TClientBuilder.UnClientParDefaut: TClient;  
begin  
  Result := TClientBuilder.Create.Construire;
end;

function TClientBuilder.AvecNom(const Nom: string): TClientBuilder;  
begin  
  FClient.Nom := Nom;
  Result := Self;
end;

function TClientBuilder.AvecPrenom(const Prenom: string): TClientBuilder;  
begin  
  FClient.Prenom := Prenom;
  Result := Self;
end;

function TClientBuilder.AvecEmail(const Email: string): TClientBuilder;  
begin  
  FClient.Email := Email;
  Result := Self;
end;

function TClientBuilder.AvecTelephone(const Telephone: string): TClientBuilder;  
begin  
  FClient.Telephone := Telephone;
  Result := Self;
end;

function TClientBuilder.AvecAdresse(const Adresse, CodePostal, Ville: string): TClientBuilder;  
begin  
  FClient.Adresse := Adresse;
  FClient.CodePostal := CodePostal;
  FClient.Ville := Ville;
  Result := Self;
end;

function TClientBuilder.Construire: TClient;  
begin  
  Result := FClient;
  FClient := nil;  // Transférer la propriété
end;

destructor TClientBuilder.Destroy;  
begin  
  FClient.Free;  // Au cas où Construire n'a pas été appelé
  inherited;
end;
```

**Utilisation :**

```pascal
procedure TestAjouterClientAvecAdresseParis;  
var  
  Client: TClient;
begin
  // Client avec juste le nom personnalisé, le reste par défaut
  Client := TClientBuilder.UnClient
    .AvecNom('Dupont')
    .Construire;

  // ...test...
end;

procedure TestAjouterClientComplet;  
var  
  Client: TClient;
begin
  // Client complètement personnalisé avec syntaxe fluide
  Client := TClientBuilder.UnClient
    .AvecNom('Martin')
    .AvecPrenom('Sophie')
    .AvecEmail('sophie.martin@example.com')
    .AvecTelephone('0987654321')
    .AvecAdresse('10 avenue des Champs', '75008', 'Paris')
    .Construire;

  // ...test...
end;
```

## Transactions dans les tests

### Principe du rollback

Pour que chaque test soit indépendant, on utilise des transactions qui sont annulées après chaque test.

```pascal
type
  TTestAvecTransaction = class
  private
    FConnection: TFDConnection;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure MonTest;
  end;

procedure TTestAvecTransaction.Setup;  
begin  
  FConnection := TFDConnection.Create(nil);
  // Configuration de la connexion...
  FConnection.Connected := True;

  // Démarrer une transaction
  FConnection.StartTransaction;
end;

procedure TTestAvecTransaction.TearDown;  
begin  
  // TOUJOURS faire un rollback, jamais de commit
  if FConnection.InTransaction then
    FConnection.Rollback;

  FConnection.Free;
end;

procedure TTestAvecTransaction.MonTest;  
begin  
  // Effectuer des opérations sur la base
  // INSERT, UPDATE, DELETE...

  // Les assertions...

  // Pas besoin de nettoyer manuellement,
  // le Rollback dans TearDown s'en charge
end;
```

**Avantages :**
- Chaque test part d'un état propre
- Pas besoin de supprimer manuellement les données de test
- Très rapide (pas d'I/O disque pour le rollback)

**Limitations :**
- Ne fonctionne pas avec les moteurs de BDD sans support transactionnel
- Certaines opérations DDL (CREATE TABLE) ne sont pas toujours rollback-able

## Tests d'API REST

### Structure d'un test d'API

```pascal
unit TestAPIIntegration;

interface

uses
  DUnitX.TestFramework,
  REST.Client,
  REST.Types,
  System.JSON;

type
  [TestFixture]
  TTestAPIREST = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FBaseURL: string;
  public
    [SetupFixture]
    procedure SetupFixture;

    [TearDownFixture]
    procedure TearDownFixture;

    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure Test_GET_Utilisateurs_RetourneListeUtilisateurs;

    [Test]
    procedure Test_GET_UtilisateurParID_RetourneUtilisateur;

    [Test]
    procedure Test_POST_CreerUtilisateur_RetourneUtilisateurCree;

    [Test]
    procedure Test_PUT_ModifierUtilisateur_MiseAJourReussie;

    [Test]
    procedure Test_DELETE_SupprimerUtilisateur_SuppressionReussie;

    [Test]
    procedure Test_GET_UtilisateurInexistant_Retourne404;
  end;

implementation

uses
  System.SysUtils;

procedure TTestAPIREST.SetupFixture;  
begin  
  // URL de l'API de test
  FBaseURL := 'https://api-test.example.com/v1';
end;

procedure TTestAPIREST.TearDownFixture;  
begin  
  // Nettoyage final
end;

procedure TTestAPIREST.Setup;  
begin  
  FRESTClient := TRESTClient.Create(FBaseURL);
  FRESTRequest := TRESTRequest.Create(nil);
  FRESTResponse := TRESTResponse.Create(nil);

  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;

  // Timeout de 30 secondes
  FRESTRequest.Timeout := 30000;

  // Headers communs
  FRESTRequest.AddAuthParameter('Authorization', 'Bearer TEST_TOKEN',
                               TRESTRequestParameterKind.pkHTTPHEADER);
end;

procedure TTestAPIREST.TearDown;  
begin  
  FRESTResponse.Free;
  FRESTRequest.Free;
  FRESTClient.Free;
end;

procedure TTestAPIREST.Test_GET_Utilisateurs_RetourneListeUtilisateurs;  
var  
  JSONArray: TJSONArray;
begin
  // Arrange
  FRESTRequest.Resource := 'users';
  FRESTRequest.Method := rmGET;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.AreEqual(200, FRESTResponse.StatusCode, 'Status code doit être 200 OK');
  Assert.IsNotNull(FRESTResponse.JSONValue, 'La réponse doit contenir du JSON');

  JSONArray := FRESTResponse.JSONValue as TJSONArray;
  Assert.IsNotNull(JSONArray, 'La réponse doit être un tableau JSON');
  Assert.IsTrue(JSONArray.Count > 0, 'Le tableau doit contenir au moins un utilisateur');
end;

procedure TTestAPIREST.Test_GET_UtilisateurParID_RetourneUtilisateur;  
var  
  JSONObject: TJSONObject;
  UserID: Integer;
begin
  // Arrange
  FRESTRequest.Resource := 'users/1';
  FRESTRequest.Method := rmGET;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.AreEqual(200, FRESTResponse.StatusCode);
  Assert.IsNotNull(FRESTResponse.JSONValue);

  JSONObject := FRESTResponse.JSONValue as TJSONObject;
  Assert.IsNotNull(JSONObject, 'La réponse doit être un objet JSON');

  UserID := JSONObject.GetValue<Integer>('id');
  Assert.AreEqual(1, UserID, 'L''ID utilisateur doit être 1');

  Assert.IsTrue(JSONObject.TryGetValue<string>('name') <> '', 'Le nom doit être présent');
  Assert.IsTrue(JSONObject.TryGetValue<string>('email') <> '', 'L''email doit être présent');
end;

procedure TTestAPIREST.Test_POST_CreerUtilisateur_RetourneUtilisateurCree;  
var  
  JSONObject, ResponseObject: TJSONObject;
  NewUserID: Integer;
begin
  // Arrange
  FRESTRequest.Resource := 'users';
  FRESTRequest.Method := rmPOST;

  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', 'Test User');
    JSONObject.AddPair('email', Format('test%d@example.com', [Random(10000)]));
    JSONObject.AddPair('role', 'user');

    FRESTRequest.AddBody(JSONObject.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
  finally
    JSONObject.Free;
  end;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.AreEqual(201, FRESTResponse.StatusCode, 'Status code doit être 201 Created');

  ResponseObject := FRESTResponse.JSONValue as TJSONObject;
  Assert.IsNotNull(ResponseObject);

  NewUserID := ResponseObject.GetValue<Integer>('id');
  Assert.IsTrue(NewUserID > 0, 'Un ID doit être généré');

  Assert.AreEqual('Test User', ResponseObject.GetValue<string>('name'));
end;

procedure TTestAPIREST.Test_PUT_ModifierUtilisateur_MiseAJourReussie;  
var  
  JSONObject: TJSONObject;
begin
  // Arrange
  FRESTRequest.Resource := 'users/1';
  FRESTRequest.Method := rmPUT;

  JSONObject := TJSONObject.Create;
  try
    JSONObject.AddPair('name', 'Updated Name');
    JSONObject.AddPair('email', 'updated@example.com');

    FRESTRequest.AddBody(JSONObject.ToJSON, TRESTContentType.ctAPPLICATION_JSON);
  finally
    JSONObject.Free;
  end;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.AreEqual(200, FRESTResponse.StatusCode, 'Status code doit être 200 OK');
end;

procedure TTestAPIREST.Test_DELETE_SupprimerUtilisateur_SuppressionReussie;  
begin  
  // Arrange
  FRESTRequest.Resource := 'users/999';  // ID de test à supprimer
  FRESTRequest.Method := rmDELETE;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.IsTrue(
    (FRESTResponse.StatusCode = 200) or (FRESTResponse.StatusCode = 204),
    'Status code doit être 200 OK ou 204 No Content'
  );
end;

procedure TTestAPIREST.Test_GET_UtilisateurInexistant_Retourne404;  
begin  
  // Arrange
  FRESTRequest.Resource := 'users/999999';  // ID qui n'existe pas
  FRESTRequest.Method := rmGET;

  // Act
  FRESTRequest.Execute;

  // Assert
  Assert.AreEqual(404, FRESTResponse.StatusCode, 'Status code doit être 404 Not Found');
end;

end.
```

## Gestion des données de test

### Fixtures (jeux de données)

Les **fixtures** sont des ensembles de données prédéfinies pour les tests.

**Approche 1 : Scripts SQL**

```sql
-- test_fixtures.sql
-- Nettoyer les données existantes
DELETE FROM Commandes;  
DELETE FROM Clients;  

-- Insérer des données de test
INSERT INTO Clients (ID, Nom, Prenom, Email) VALUES
(1, 'Dupont', 'Jean', 'jean.dupont@example.com'),
(2, 'Martin', 'Marie', 'marie.martin@example.com'),
(3, 'Durand', 'Pierre', 'pierre.durand@example.com');

INSERT INTO Commandes (ID, ClientID, DateCommande, MontantTotal) VALUES
(1, 1, '2025-01-15', 150.00),
(2, 1, '2025-02-20', 250.00),
(3, 2, '2025-03-10', 75.50);
```

**Chargement :**

```pascal
procedure ChargerFixtures;  
var  
  Script: TStringList;
begin
  Script := TStringList.Create;
  try
    Script.LoadFromFile('test_fixtures.sql');
    FDConnection.ExecSQL(Script.Text);
  finally
    Script.Free;
  end;
end;
```

**Approche 2 : Code Delphi**

```pascal
type
  TTestFixtures = class
  public
    class procedure InsererClientsTest;
    class procedure InsererCommandesTest;
    class procedure SupprimerToutesLesDonnees;
  end;

class procedure TTestFixtures.InsererClientsTest;  
begin  
  with FDQuery do
  begin
    SQL.Text := 'INSERT INTO Clients (Nom, Prenom, Email) VALUES (:Nom, :Prenom, :Email)';

    ParamByName('Nom').AsString := 'Dupont';
    ParamByName('Prenom').AsString := 'Jean';
    ParamByName('Email').AsString := 'jean.dupont@example.com';
    ExecSQL;

    ParamByName('Nom').AsString := 'Martin';
    ParamByName('Prenom').AsString := 'Marie';
    ParamByName('Email').AsString := 'marie.martin@example.com';
    ExecSQL;
  end;
end;

class procedure TTestFixtures.SupprimerToutesLesDonnees;  
begin  
  FDConnection.ExecSQL('DELETE FROM Commandes');
  FDConnection.ExecSQL('DELETE FROM Clients');
end;
```

**Approche 3 : Fichiers de données**

Stocker les données dans des fichiers JSON ou XML :

```json
// test_data.json
{
  "clients": [
    {
      "nom": "Dupont",
      "prenom": "Jean",
      "email": "jean.dupont@example.com"
    },
    {
      "nom": "Martin",
      "prenom": "Marie",
      "email": "marie.martin@example.com"
    }
  ]
}
```

## Bonnes pratiques des tests d'intégration

### 1. Isolation des tests

Chaque test doit être indépendant et ne pas dépendre de l'ordre d'exécution.

**Mauvais :**

```pascal
procedure Test1_CreerClient; // Crée un client avec ID=1  
procedure Test2_ModifierClient; // Suppose que le client ID=1 existe  
```

Si Test1 échoue, Test2 échouera aussi. C'est une **dépendance**.

**Bon :**

```pascal
procedure Test1_CreerClient;  
begin  
  // Crée son propre client
end;

procedure Test2_ModifierClient;  
begin  
  // Crée d'abord un client, puis le modifie
  CreerClientTest;
  // ... modification ...
end;
```

### 2. Nettoyage systématique

**TOUJOURS** nettoyer après vos tests, même en cas d'erreur.

```pascal
procedure MonTest;  
var  
  Client: TClient;
  ID: Integer;
begin
  // Créer
  Client := TClientBuilder.UnClientParDefaut;
  ID := Repository.Ajouter(Client);
  Client.Free;

  try
    // Test...
    Assert.AreEqual(...);
  finally
    // Nettoyer même si le test échoue
    Repository.Supprimer(ID);
  end;
end;
```

Ou utilisez des transactions avec rollback automatique.

### 3. Tests déterministes

Un test doit donner toujours le même résultat.

**Problèmes courants :**

```pascal
// MAUVAIS : Dépend de l'heure actuelle
Assert.AreEqual(Now, Client.DateInscription);

// MAUVAIS : Dépend de données aléatoires
Email := Format('test%d@example.com', [Random(1000)]);

// MAUVAIS : Dépend de l'ordre des résultats SQL
Premier := Clients[0];  // L'ordre peut varier
```

**Solutions :**

```pascal
// BON : Comparer avec une tolérance
Assert.IsTrue(Abs(Now - Client.DateInscription) < OneSecond);

// BON : Données prévisibles
Email := 'test_fixe@example.com';

// BON : Trier ou chercher spécifiquement
Premier := Clients.Find(function(C: TClient): Boolean  
begin  
  Result := C.ID = IDAttendu;
end);
```

### 4. Messages d'assertion clairs

```pascal
// MAUVAIS
Assert.IsTrue(Count > 0);

// BON
Assert.IsTrue(Count > 0,
  Format('Au moins un client doit exister, mais Count=%d', [Count]));
```

### 5. Un concept par test

Ne testez qu'une seule chose à la fois.

**Mauvais :**

```pascal
procedure Test_ToutesLesOperationsClient;  
begin  
  // Créer
  ID := Repository.Ajouter(Client);
  Assert...

  // Modifier
  Repository.Modifier(Client);
  Assert...

  // Supprimer
  Repository.Supprimer(ID);
  Assert...
end;
```

**Bon :**

```pascal
procedure Test_AjouterClient; // Un test  
procedure Test_ModifierClient; // Un autre test  
procedure Test_SupprimerClient; // Un troisième test  
```

### 6. Tests rapides

Les tests d'intégration sont plus lents que les tests unitaires, mais essayez de les garder raisonnablement rapides :
- Utilisez des bases de données en mémoire quand possible
- Limitez le volume de données de test
- Utilisez des transactions (rollback rapide)
- Exécutez en parallèle si possible

**Objectif :** Toute la suite de tests d'intégration en moins de 5 minutes.

### 7. Catégoriser les tests

Utilisez des attributs pour catégoriser :

```pascal
[Test]
[Category('Database')]
procedure Test_Database1;

[Test]
[Category('API')]
procedure Test_API1;

[Test]
[Category('Slow')]
procedure Test_LongRunning;
```

Vous pouvez ensuite exécuter seulement certaines catégories.

## Exécution des tests d'intégration

### En local pendant le développement

Exécutez fréquemment les tests d'intégration :
- Après chaque modification importante
- Avant de commiter
- Au moins une fois par jour

### Dans l'intégration continue (CI)

Configurez votre pipeline CI/CD pour exécuter automatiquement les tests :

```yaml
# Exemple avec GitLab CI
test_integration:
  stage: test
  script:
    - docker-compose up -d database-test
    - sleep 10  # Attendre que la base soit prête
    - ./MonAppTests.exe --category=Integration
    - docker-compose down
  artifacts:
    reports:
      junit: test-results.xml
```

### Tests de smoke

Les **smoke tests** sont un sous-ensemble minimal de tests d'intégration qui vérifient que les fonctionnalités critiques fonctionnent.

**Exemple :**

```pascal
[Test]
[Category('Smoke')]
procedure Test_ConnexionBaseDeDonnees;

[Test]
[Category('Smoke')]
procedure Test_APIDisponible;

[Test]
[Category('Smoke')]
procedure Test_CreerUtilisateurBasique;
```

Exécutez les smoke tests très fréquemment (à chaque build), et les tests d'intégration complets moins souvent (nightly).

## Outils et frameworks complémentaires

### TestInsight

Plugin pour Delphi qui permet d'exécuter et de visualiser les tests directement dans l'IDE.

**Avantages :**
- Exécution rapide depuis l'IDE
- Visualisation des résultats
- Navigation vers le code des tests
- Support de DUnit et DUnitX

### CI/CD

**Jenkins, GitLab CI, GitHub Actions, Azure DevOps**

Automatisez l'exécution des tests à chaque commit ou selon un planning.

### Coverage tools

**Delphi Code Coverage**, **AQtime**

Mesurent la couverture de code par les tests.

### Outils de monitoring

Pour les tests d'intégration d'APIs et de services :
- **Postman** : Tester manuellement les APIs
- **SoapUI** : Tests de services SOAP et REST
- **JMeter** : Tests de charge

## Conseils pour débutants

### 1. Commencez par les tests unitaires

Maîtrisez d'abord les tests unitaires avant de vous lancer dans les tests d'intégration. Les concepts sont similaires, mais les tests d'intégration ajoutent de la complexité.

### 2. Créez un environnement de test sûr

Ne testez JAMAIS sur la base de production. Créez une base de test dédiée ou utilisez Docker.

### 3. Documentez la configuration

Les tests d'intégration nécessitent souvent une configuration spécifique. Documentez :
- Comment configurer la base de test
- Les variables d'environnement nécessaires
- Les dépendances externes
- Comment lancer les tests

### 4. Utilisez des builders

Les data builders réduisent considérablement le code répétitif et rendent les tests plus lisibles.

### 5. Ne testez pas trop finement

Les tests d'intégration testent les interactions, pas les détails d'implémentation. Laissez les détails aux tests unitaires.

### 6. Automatisez le nettoyage

Utilisez des transactions avec rollback ou automatisez le nettoyage dans les méthodes TearDown.

### 7. Soyez patient

Les tests d'intégration sont plus lents. C'est normal. Ne vous attendez pas à la rapidité des tests unitaires.

### 8. Commencez petit

Ne tentez pas de tester toute l'application d'un coup. Commencez par :
1. Un test de connexion base de données
2. Un test CRUD simple (Create, Read, Update, Delete)
3. Un test d'API simple
4. Progressivement, ajoutez plus de tests

## Checklist des tests d'intégration

**□ Configuration**
- [ ] Environnement de test séparé configuré
- [ ] Base de données de test créée
- [ ] Fichiers de configuration de test en place
- [ ] Données de test (fixtures) préparées

**□ Structure**
- [ ] Projet de tests créé
- [ ] Classes de base pour tests d'intégration
- [ ] Helpers et builders de données de test
- [ ] Organisation des tests par catégorie

**□ Tests**
- [ ] Tests de base de données (CRUD complet)
- [ ] Tests d'APIs externes
- [ ] Tests de fichiers/I/O
- [ ] Tests de scénarios utilisateur importants

**□ Bonnes pratiques**
- [ ] Chaque test est indépendant
- [ ] Nettoyage systématique (TearDown ou transactions)
- [ ] Messages d'assertion clairs
- [ ] Pas de dépendances sur des données externes non contrôlées

**□ Exécution**
- [ ] Les tests s'exécutent en local
- [ ] Les tests s'exécutent dans le CI/CD
- [ ] Résultats de tests archivés
- [ ] Temps d'exécution acceptable (<5 minutes idéalement)

## Conclusion

Les tests d'intégration sont essentiels pour garantir que les différentes parties de votre application Delphi fonctionnent correctement ensemble. Bien qu'ils soient plus complexes à mettre en place et plus lents à exécuter que les tests unitaires, ils vous donnent une confiance précieuse avant de déployer en production.

**Points clés à retenir :**

**Complémentarité :** Les tests d'intégration complètent les tests unitaires, ils ne les remplacent pas. Utilisez les deux.

**Isolation :** Configurez un environnement de test séparé et sûr. Ne testez jamais sur les données de production.

**Indépendance :** Chaque test doit être indépendant et nettoyable. Utilisez des transactions ou un nettoyage systématique.

**Réalisme :** Les tests d'intégration utilisent de vraies ressources (bases de données, APIs) pour tester des scénarios réalistes.

**Pyramide :** Suivez la pyramide des tests : beaucoup de tests unitaires, quelques tests d'intégration, peu de tests UI.

**Automatisation :** Intégrez les tests dans votre pipeline CI/CD pour une exécution automatique.

**Data builders :** Utilisez des builders pour simplifier la création d'objets de test.

**Catégorisation :** Organisez vos tests par catégories (Database, API, Slow, Smoke) pour une exécution sélective.

En maîtrisant les tests d'intégration, vous rejoignez les rangs des développeurs qui livrent du code fiable et maintenable. C'est un investissement initial en temps qui rapporte énormément à long terme en termes de qualité, de confiance et de réduction des bugs en production. Commencez progressivement, apprenez de vos erreurs, et vous développerez rapidement l'expertise nécessaire pour tester efficacement vos applications Delphi dans leur ensemble.

⏭️ [Mocking et tests avec dépendances](/12-debogage-et-tests/08-mocking-et-tests-avec-dependances.md)
