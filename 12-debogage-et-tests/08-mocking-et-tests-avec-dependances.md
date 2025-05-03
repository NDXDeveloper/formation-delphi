# 12.8 Mocking et tests avec dépendances

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction au mocking

Le "mocking" est une technique essentielle dans le développement de tests efficaces. Elle consiste à créer des objets simulés (appelés "mocks") qui imitent le comportement de composants réels, mais de façon contrôlée et prévisible. Cette approche est particulièrement utile lorsque vous testez du code qui dépend d'autres composants.

## Pourquoi utiliser des mocks ?

Les mocks résolvent plusieurs problèmes courants dans les tests :

1. **Isolement** : Ils permettent de tester une unité de code en isolation, sans être affecté par ses dépendances.

2. **Rapidité** : Les tests utilisant des mocks s'exécutent généralement plus rapidement que ceux utilisant des composants réels (comme une base de données).

3. **Contrôle** : Vous pouvez programmer le comportement exact du mock, y compris les cas d'erreur difficiles à reproduire avec des composants réels.

4. **Indépendance** : Les tests ne dépendent pas de ressources externes qui pourraient être indisponibles ou instables.

5. **Vérification d'interactions** : Vous pouvez vérifier quand et comment vos dépendances sont appelées.

## Types de simulacres de test

Avant d'aller plus loin, clarifions quelques termes :

### 1. Dummy

Un objet simple qui est passé autour mais jamais réellement utilisé. Il sert uniquement à remplir des listes de paramètres.

```pascal
// Exemple de dummy
function CreateDummyLogger: ILogger;
begin
  Result := TInterfacedObject.Create as ILogger; // Implémentation vide
end;
```

### 2. Stub

Un objet qui fournit des réponses prédéfinies aux appels effectués pendant le test, sans répondre à autre chose.

```pascal
// Exemple de stub simple
TStubCalculateur = class(TInterfacedObject, ICalculateur)
public
  function Additionner(A, B: Integer): Integer;
end;

function TStubCalculateur.Additionner(A, B: Integer): Integer;
begin
  // Retourne toujours 42, quelle que soit l'entrée
  Result := 42;
end;
```

### 3. Spy

Un objet qui enregistre les appels qui lui sont faits, pour une vérification ultérieure.

```pascal
// Exemple de spy
TSpyLogger = class(TInterfacedObject, ILogger)
private
  FLogMessages: TStrings;
public
  constructor Create;
  destructor Destroy; override;
  procedure Log(const Message: string);
  function GetLoggedMessages: TStrings;
end;

constructor TSpyLogger.Create;
begin
  inherited Create;
  FLogMessages := TStringList.Create;
end;

destructor TSpyLogger.Destroy;
begin
  FLogMessages.Free;
  inherited;
end;

procedure TSpyLogger.Log(const Message: string);
begin
  FLogMessages.Add(Message);
end;

function TSpyLogger.GetLoggedMessages: TStrings;
begin
  Result := FLogMessages;
end;
```

### 4. Mock

Un objet programmé avec des attentes concernant les appels qu'il recevra et capable de vérifier que ces attentes sont satisfaites.

```pascal
// Conceptuellement, un mock vérifie les attentes
TMockDatabase = class(TInterfacedObject, IDatabase)
private
  FExpectedQuery: string;
  FWasCalled: Boolean;
public
  constructor Create(const ExpectedQuery: string);
  function ExecuteQuery(const Query: string): TDataSet;
  procedure VerifyExpectations;
end;

constructor TMockDatabase.Create(const ExpectedQuery: string);
begin
  inherited Create;
  FExpectedQuery := ExpectedQuery;
  FWasCalled := False;
end;

function TMockDatabase.ExecuteQuery(const Query: string): TDataSet;
begin
  FWasCalled := True;
  Assert(Query = FExpectedQuery, 'Query incorrecte');
  // Retourner un dataset factice
  Result := nil;
end;

procedure TMockDatabase.VerifyExpectations;
begin
  Assert(FWasCalled, 'La méthode ExecuteQuery n''a pas été appelée');
end;
```

### 5. Fake

Une implémentation qui a un comportement fonctionnel mais pas adapté à la production (comme une base de données en mémoire).

```pascal
// Exemple de fake pour une base de données
TFakeDatabase = class(TInterfacedObject, IDatabase)
private
  FTables: TDictionary<string, TList<TJSONObject>>;
public
  constructor Create;
  destructor Destroy; override;
  function ExecuteQuery(const Query: string): TDataSet;
  procedure Insert(const TableName: string; const Data: TJSONObject);
end;

// Implémentation simplifiée pour les tests
```

## Frameworks de mocking pour Delphi

Pour simplifier la création de mocks, plusieurs frameworks sont disponibles :

### 1. Delphi-Mocks

C'est l'un des frameworks les plus populaires, offrant une API fluide pour créer des mocks.

Installation :
1. Téléchargez le code depuis GitHub : [https://github.com/VSoftTechnologies/Delphi-Mocks](https://github.com/VSoftTechnologies/Delphi-Mocks)
2. Ajoutez le dossier `Source` au chemin de recherche de votre projet

Exemple d'utilisation :

```pascal
uses
  Delphi.Mocks;

// Dans votre test
procedure TTestService.TestMethodeAvecDependance;
var
  Mock: TMock<ILogger>;
  Service: TMonService;
begin
  // Création du mock
  Mock := TMock<ILogger>.Create;

  // Configuration du comportement attendu
  Mock.Setup.WillReturn(True).When.Log(It.IsAny<string>);

  // Injection du mock dans la classe à tester
  Service := TMonService.Create(Mock);
  try
    // Test de la méthode
    Service.ExecuterAction('test');

    // Vérification que le mock a été appelé correctement
    Mock.Verify.Once.When.Log('Action exécutée: test');
  finally
    Service.Free;
  end;
end;
```

### 2. DunitX Mocks

Plus récent, ce framework est intégré à DunitX et offre une syntaxe légèrement différente.

Exemple :

```pascal
uses
  DUnitX.Mocks;

// Dans votre test
procedure TTestService.TestAvecDUnitXMocks;
var
  MockLogger: TMock<ILogger>;
  Service: TMonService;
begin
  // Création du mock
  MockLogger := TMock<ILogger>.Create;

  // Configuration
  MockLogger.Setup.Returns(True).When.Log(Arg.IsAny<string>);

  // Utilisation
  Service := TMonService.Create(MockLogger.Instance);
  Service.ExecuterAction('test');

  // Vérification
  MockLogger.Verify.WasCalled(1).When.Log('Action exécutée: test');
end;
```

### 3. Implémentation manuelle

Si vous préférez éviter les frameworks externes ou si vous travaillez avec une version plus ancienne de Delphi, vous pouvez créer vos propres classes de mock manuellement.

## Techniques de conception pour faciliter le mocking

Pour que votre code soit facilement testable avec des mocks, certaines pratiques de conception sont recommandées :

### 1. Utiliser des interfaces

Les interfaces sont idéales pour le mocking car elles définissent clairement un contrat sans imposer d'implémentation.

```pascal
// Définition d'interface
IRepository = interface
  ['{8F3A4F7D-3B7C-4B41-9E68-6529CBD9A157}']
  function GetById(const Id: Integer): TCustomer;
  procedure Save(const Customer: TCustomer);
end;

// La classe réelle implémente l'interface
TDatabaseRepository = class(TInterfacedObject, IRepository)
  // Implémentation...
end;

// Le mock implémente la même interface
TMockRepository = class(TInterfacedObject, IRepository)
  // Implémentation pour les tests...
end;
```

### 2. Injection de dépendances

L'injection de dépendances consiste à fournir les dépendances d'une classe de l'extérieur plutôt que de les créer en interne.

```pascal
// Sans injection de dépendances (difficile à tester)
TServiceDifficile = class
private
  FRepository: TDatabaseRepository;
public
  constructor Create;
  destructor Destroy; override;
  // Méthodes...
end;

constructor TServiceDifficile.Create;
begin
  FRepository := TDatabaseRepository.Create;
end;

// Avec injection de dépendances (facile à tester)
TServiceTestable = class
private
  FRepository: IRepository;
public
  constructor Create(ARepository: IRepository);
  // Méthodes...
end;

constructor TServiceTestable.Create(ARepository: IRepository);
begin
  FRepository := ARepository;
end;
```

### 3. Principe de responsabilité unique

Chaque classe ne devrait avoir qu'une seule raison de changer. Les classes avec des responsabilités multiples sont plus difficiles à mocker.

```pascal
// Trop de responsabilités (difficile à tester)
TClasseComplexe = class
public
  procedure ChargerDonnees;
  procedure ValiderDonnees;
  procedure EnregistrerDonnees;
  procedure EnvoyerEmail;
end;

// Responsabilité unique (facile à tester)
TChargeurDonnees = class
public
  procedure Charger;
end;

TValidateurDonnees = class
public
  function Valider: Boolean;
end;

TEnregistreurDonnees = class
public
  procedure Enregistrer;
end;
```

## Exemple pratique complet

Voyons maintenant un exemple complet d'utilisation de mocks dans un test. Nous allons tester un service de gestion de commandes qui dépend d'un repository, d'un service de paiement et d'un service d'email.

### Étape 1 : Définir les interfaces

```pascal
// Unit1_Interfaces.pas
unit Unit1_Interfaces;

interface

uses
  System.Classes;

type
  TCommande = class
  public
    ID: Integer;
    ClientID: Integer;
    Montant: Currency;
    Statut: string;
  end;

  ICommandeRepository = interface
    ['{A73FC8B1-2C4C-4A1D-9F58-2847D848940A}']
    function ObtenirCommande(ID: Integer): TCommande;
    procedure EnregistrerCommande(Commande: TCommande);
  end;

  IServicePaiement = interface
    ['{06A3C7D2-9E8F-4A5B-8C6D-7F5E4D3B2A1F}']
    function TraiterPaiement(CommandeID: Integer; Montant: Currency): Boolean;
  end;

  IServiceEmail = interface
    ['{B2A1F3E4-D5C6-4B7A-8E9D-F0E1D2C3B4A5}']
    procedure EnvoyerConfirmation(Email: string; CommandeID: Integer);
  end;

implementation

end.
```

### Étape 2 : Implémenter le service à tester

```pascal
// Unit2_Service.pas
unit Unit2_Service;

interface

uses
  Unit1_Interfaces;

type
  TServiceCommande = class
  private
    FRepository: ICommandeRepository;
    FServicePaiement: IServicePaiement;
    FServiceEmail: IServiceEmail;
  public
    constructor Create(Repository: ICommandeRepository;
                      ServicePaiement: IServicePaiement;
                      ServiceEmail: IServiceEmail);
    function TraiterCommande(CommandeID: Integer; EmailClient: string): Boolean;
  end;

implementation

constructor TServiceCommande.Create(Repository: ICommandeRepository;
                                  ServicePaiement: IServicePaiement;
                                  ServiceEmail: IServiceEmail);
begin
  FRepository := Repository;
  FServicePaiement := ServicePaiement;
  FServiceEmail := ServiceEmail;
end;

function TServiceCommande.TraiterCommande(CommandeID: Integer; EmailClient: string): Boolean;
var
  Commande: TCommande;
begin
  Result := False;

  // Récupérer la commande
  Commande := FRepository.ObtenirCommande(CommandeID);
  if not Assigned(Commande) then
    Exit;

  try
    // Vérifier si la commande n'est pas déjà traitée
    if Commande.Statut <> 'En attente' then
      Exit;

    // Traiter le paiement
    if not FServicePaiement.TraiterPaiement(CommandeID, Commande.Montant) then
      Exit;

    // Mettre à jour le statut
    Commande.Statut := 'Payée';
    FRepository.EnregistrerCommande(Commande);

    // Envoyer confirmation par email
    FServiceEmail.EnvoyerConfirmation(EmailClient, CommandeID);

    Result := True;
  finally
    Commande.Free;
  end;
end;

end.
```

### Étape 3 : Créer le test avec mocks

```pascal
// Unit3_Tests.pas
unit Unit3_Tests;

interface

uses
  DUnitX.TestFramework,
  Unit1_Interfaces,
  Unit2_Service,
  Delphi.Mocks;

type
  [TestFixture]
  TServiceCommandeTests = class
  private
    FRepository: TMock<ICommandeRepository>;
    FServicePaiement: TMock<IServicePaiement>;
    FServiceEmail: TMock<IServiceEmail>;
    FService: TServiceCommande;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestTraiterCommandeSuccess;
    [Test]
    procedure TestTraiterCommandeCommandeNonTrouvee;
    [Test]
    procedure TestTraiterCommandeDejaTraitee;
    [Test]
    procedure TestTraiterCommandePaiementEchoue;
  end;

implementation

procedure TServiceCommandeTests.Setup;
begin
  // Créer les mocks
  FRepository := TMock<ICommandeRepository>.Create;
  FServicePaiement := TMock<IServicePaiement>.Create;
  FServiceEmail := TMock<IServiceEmail>.Create;

  // Créer le service avec les mocks
  FService := TServiceCommande.Create(
    FRepository.Instance,
    FServicePaiement.Instance,
    FServiceEmail.Instance
  );
end;

procedure TServiceCommandeTests.TearDown;
begin
  FService.Free;
end;

procedure TServiceCommandeTests.TestTraiterCommandeSuccess;
var
  Commande: TCommande;
  Resultat: Boolean;
begin
  // Préparer les données de test
  Commande := TCommande.Create;
  Commande.ID := 1;
  Commande.ClientID := 42;
  Commande.Montant := 100.0;
  Commande.Statut := 'En attente';

  // Configurer les mocks
  FRepository.Setup.WillReturn(Commande).When.ObtenirCommande(1);
  FServicePaiement.Setup.WillReturn(True).When.TraiterPaiement(1, 100.0);

  // Exécuter la méthode à tester
  Resultat := FService.TraiterCommande(1, 'client@exemple.com');

  // Vérifier les résultats
  Assert.IsTrue(Resultat, 'La commande aurait dû être traitée avec succès');

  // Vérifier les interactions avec les mocks
  FRepository.Verify.Once.ObtenirCommande(1);
  FServicePaiement.Verify.Once.TraiterPaiement(1, 100.0);
  FRepository.Verify.Once.EnregistrerCommande(It.IsAny<TCommande>);
  FServiceEmail.Verify.Once.EnvoyerConfirmation('client@exemple.com', 1);
end;

procedure TServiceCommandeTests.TestTraiterCommandeCommandeNonTrouvee;
var
  Resultat: Boolean;
begin
  // Configurer le mock pour simuler une commande non trouvée
  FRepository.Setup.WillReturn(nil).When.ObtenirCommande(999);

  // Exécuter la méthode à tester
  Resultat := FService.TraiterCommande(999, 'client@exemple.com');

  // Vérifier les résultats
  Assert.IsFalse(Resultat, 'La méthode devrait échouer si la commande n''existe pas');

  // Vérifier les interactions
  FRepository.Verify.Once.ObtenirCommande(999);
  FServicePaiement.Verify.Never.TraiterPaiement(It.IsAny<Integer>, It.IsAny<Currency>);
  FServiceEmail.Verify.Never.EnvoyerConfirmation(It.IsAny<string>, It.IsAny<Integer>);
end;

procedure TServiceCommandeTests.TestTraiterCommandeDejaTraitee;
var
  Commande: TCommande;
  Resultat: Boolean;
begin
  // Préparer une commande déjà traitée
  Commande := TCommande.Create;
  Commande.ID := 2;
  Commande.Statut := 'Payée'; // Statut déjà "Payée"

  // Configurer le mock
  FRepository.Setup.WillReturn(Commande).When.ObtenirCommande(2);

  // Exécuter la méthode à tester
  Resultat := FService.TraiterCommande(2, 'client@exemple.com');

  // Vérifier les résultats
  Assert.IsFalse(Resultat, 'La méthode devrait échouer pour une commande déjà traitée');

  // Vérifier les interactions
  FRepository.Verify.Once.ObtenirCommande(2);
  FServicePaiement.Verify.Never.TraiterPaiement(It.IsAny<Integer>, It.IsAny<Currency>);
end;

procedure TServiceCommandeTests.TestTraiterCommandePaiementEchoue;
var
  Commande: TCommande;
  Resultat: Boolean;
begin
  // Préparer la commande
  Commande := TCommande.Create;
  Commande.ID := 3;
  Commande.Montant := 50.0;
  Commande.Statut := 'En attente';

  // Configurer les mocks
  FRepository.Setup.WillReturn(Commande).When.ObtenirCommande(3);
  FServicePaiement.Setup.WillReturn(False).When.TraiterPaiement(3, 50.0); // Paiement échoue

  // Exécuter la méthode à tester
  Resultat := FService.TraiterCommande(3, 'client@exemple.com');

  // Vérifier les résultats
  Assert.IsFalse(Resultat, 'La méthode devrait échouer si le paiement échoue');

  // Vérifier les interactions
  FRepository.Verify.Once.ObtenirCommande(3);
  FServicePaiement.Verify.Once.TraiterPaiement(3, 50.0);
  FRepository.Verify.Never.EnregistrerCommande(It.IsAny<TCommande>);
  FServiceEmail.Verify.Never.EnvoyerConfirmation(It.IsAny<string>, It.IsAny<Integer>);
end;

initialization
  TDUnitX.RegisterTestFixture(TServiceCommandeTests);

end.
```

## Techniques avancées de mocking

### 1. Simuler des exceptions

Les mocks peuvent être configurés pour lever des exceptions, ce qui est utile pour tester la gestion d'erreurs :

```pascal
procedure TestGestionException;
var
  MockRepo: TMock<IRepository>;
  Service: TMonService;
begin
  MockRepo := TMock<IRepository>.Create;

  // Configurer le mock pour lever une exception
  MockRepo.Setup.WillRaise(EAccessViolation, 'Erreur simulée')
         .When.ObtenirDonnee(It.IsAny<Integer>);

  Service := TMonService.Create(MockRepo);
  try
    // L'appel devrait être entouré d'un try-except pour capturer l'exception
    Assert.WillRaise(
      procedure
      begin
        Service.TraiterDonnee(1);
      end,
      EAccessViolation
    );
  finally
    Service.Free;
  end;
end;
```

### 2. Mocks séquentiels

Vous pouvez configurer un mock pour retourner différentes valeurs lors d'appels successifs :

```pascal
procedure TestAppelsSequentiels;
var
  MockGenerator: TMock<INumberGenerator>;
  TestObject: TTestObject;
begin
  MockGenerator := TMock<INumberGenerator>.Create;

  // Configurer des retours séquentiels
  MockGenerator.Setup.WillReturnWhen(1).When.Generate; // Premier appel
  MockGenerator.Setup.WillReturnWhen(2).When.Generate; // Deuxième appel
  MockGenerator.Setup.WillReturnWhen(3).When.Generate; // Troisième appel

  TestObject := TTestObject.Create(MockGenerator);
  try
    Assert.AreEqual(1, TestObject.GetNextNumber);
    Assert.AreEqual(2, TestObject.GetNextNumber);
    Assert.AreEqual(3, TestObject.GetNextNumber);
  finally
    TestObject.Free;
  end;
end;
```

### 3. Mocks avec capture de paramètres

Pour des vérifications plus complexes, vous pouvez capturer les paramètres passés au mock :

```pascal
procedure TestCaptureParametres;
var
  MockProcessor: TMock<IDataProcessor>;
  CapturedData: TCustomData;
  Service: TDataService;
begin
  MockProcessor := TMock<IDataProcessor>.Create;

  // Configurer le mock pour capturer le paramètre
  MockProcessor.Setup.Captures(
    procedure(const Data: TCustomData)
    begin
      CapturedData := Data;
    end).When.Process(It.IsAny<TCustomData>);

  Service := TDataService.Create(MockProcessor);
  try
    Service.ProcessCustomData('Test', 42);

    // Vérifier les propriétés de l'objet capturé
    Assert.AreEqual('Test', CapturedData.Name);
    Assert.AreEqual(42, CapturedData.Value);
  finally
    Service.Free;
  end;
end;
```

## Bonnes pratiques pour les tests avec mocks

### 1. Ne mockez que ce qui est nécessaire

Ne créez pas de mocks pour tout. Limitez-vous aux dépendances externes ou complexes :

```pascal
// À éviter : trop de mocks
procedure TestAvecTropDeMocks;
var
  MockRepo: TMock<IRepository>;
  MockValidator: TMock<IValidator>;
  MockLogger: TMock<ILogger>;
  MockFormatter: TMock<IFormatter>;
  MockEmailSender: TMock<IEmailSender>;
  // ... et ainsi de suite
begin
  // Configuration excessive
end;

// Préférable : mocker seulement les dépendances nécessaires
procedure TestAvecMocksNecessaires;
var
  MockRepo: TMock<IRepository>; // Dépendance externe (base de données)
  MockEmailSender: TMock<IEmailSender>; // Dépendance externe (email)
  // Les autres dépendances pourraient utiliser les implémentations réelles
begin
  // Configuration raisonnable
end;
```

### 2. Évitez les tests fragiles

Les tests trop liés à l'implémentation interne sont fragiles et difficiles à maintenir :

```pascal
// Test fragile (trop d'attentes spécifiques)
procedure TestFragile;
begin
  // ...
  MockRepo.Verify.Exactly(3).ObtenirDonnee(It.IsAny<Integer>);
  MockRepo.Verify.Once.ObtenirDonnee(1);
  MockRepo.Verify.Once.ObtenirDonnee(2);
  MockRepo.Verify.Once.ObtenirDonnee(3);
  MockLogger.Verify.Exactly(6).Log(It.IsAny<string>);
  // ...
end;

// Test robuste (vérifications essentielles uniquement)
procedure TestRobuste;
begin
  // ...
  Assert.IsTrue(Resultat, 'L''opération devrait réussir');
  MockRepo.Verify.Once.EnregistrerResultat(It.IsAny<TResultat>);
  // ...
end;
```

### 3. Tests lisibles et maintenables

Structurez vos tests pour qu'ils soient faciles à comprendre :

```pascal
[Test]
procedure TestTraitementCommande;
begin
  // Arrange
  PreparerCommandeEtMocks(1, 100.0, 'En attente');

  // Act
  var Resultat := FService.TraiterCommande(1, 'client@exemple.com');

  // Assert
  Assert.IsTrue(Resultat);
  VerifierInteractionsSucces(1, 100.0, 'client@exemple.com');
end;

procedure PreparerCommandeEtMocks(ID: Integer; Montant: Currency; Statut: string);
var
  Commande: TCommande;
begin
  Commande := TCommande.Create;
  Commande.ID := ID;
  Commande.Montant := Montant;
  Commande.Statut := Statut;

  FRepository.Setup.WillReturn(Commande).When.ObtenirCommande(ID);
  FServicePaiement.Setup.WillReturn(True).When.TraiterPaiement(ID, Montant);
end;

procedure VerifierInteractionsSucces(ID: Integer; Montant: Currency; Email: string);
begin
  FRepository.Verify.Once.ObtenirCommande(ID);
  FServicePaiement.Verify.Once.TraiterPaiement(ID, Montant);
  FRepository.Verify.Once.EnregistrerCommande(It.IsAny<TCommande>);
  FServiceEmail.Verify.Once.EnvoyerConfirmation(Email, ID);
end;
```

## Gestion des dépendances avec l'injection de dépendances

Pour simplifier encore davantage les tests avec mocks, vous pouvez utiliser un conteneur d'injection de dépendances. Spring4D est une bibliothèque populaire qui offre cette fonctionnalité pour Delphi.

### Configuration avec Spring4D

```pascal
// Configuration du conteneur
procedure ConfigurerConteneur;
var
  Container: TContainer;
begin
  Container := TContainer.Create;

  // Enregistrement des interfaces
  Container.RegisterType<ICommandeRepository, TDBCommandeRepository>.AsSingleton;
  Container.RegisterType<IServicePaiement, TServicePaiementReel>;
  Container.RegisterType<IServiceEmail, TServiceEmailSMTP>;

  // Enregistrement du service principal
  Container.RegisterType<TServiceCommande>;

  Container.Build;

  // Stockage global du conteneur
  GlobalContainer := Container;
end;

// Utilisation dans l'application
procedure UtiliserService;
var
  Service: TServiceCommande;
begin
  Service := GlobalContainer.Resolve<TServiceCommande>;
  try
    // Utiliser le service
    Service.TraiterCommande(1, 'client@exemple.com');
  finally
    // Spring4D gère automatiquement la libération des objets
  end;
end;
```

### Remplacement pour les tests

```pascal
// Dans les tests
procedure ConfigurerConteneurTests;
var
  Container: TContainer;
  MockRepo: TMock<ICommandeRepository>;
  MockPaiement: TMock<IServicePaiement>;
  MockEmail: TMock<IServiceEmail>;
begin
  Container := TContainer.Create;

  // Créer les mocks
  MockRepo := TMock<ICommandeRepository>.Create;
  MockPaiement := TMock<IServicePaiement>.Create;
  MockEmail := TMock<IServiceEmail>.Create;

  // Configurer les mocks...

  // Enregistrer les mocks dans le conteneur
  Container.RegisterInstance<ICommandeRepository>(MockRepo.Instance);
  Container.RegisterInstance<IServicePaiement>(MockPaiement.Instance);
  Container.RegisterInstance<IServiceEmail>(MockEmail.Instance);

  // Enregistrer le service
  Container.RegisterType<TServiceCommande>;

  Container.Build;

  // Remplacer le conteneur global
  GlobalContainer := Container;
end;
```

### 2. Support des objets génériques et templates

```pascal
// Mocking avec génériques
[Test]
procedure TestAvecGeneriques;
var
  Mock: TMock<IRepository<TClient>>;
begin
  Mock := TMock<IRepository<TClient>>.Create;

  var Client := TClient.Create;
  Client.ID := 1;
  Client.Nom := 'Dupont';

  // Configuration pour les méthodes génériques
  Mock.When.GetById(1).Return(Client);

  // Utilisation
  var Service := TClientService.Create(Mock);
  var ResultClient := Service.ObtenirClient(1);

  // Vérification
  Assert.AreEqual('Dupont', ResultClient.Nom);
  Mock.Verify.Called.Once.With(1).On.GetById;
end;
```

### 3. Mocking de méthodes asynchrones

```pascal
// Mocking de méthodes asynchrones
[Test]
procedure TestMethodesAsynchrones;
var
  Mock: TMock<IAsyncService>;
begin
  Mock := TMock<IAsyncService>.Create;

  // Configuration d'une méthode asynchrone
  Mock.When.FetchDataAsync('users')
      .ReturnFuture(TArray<string>.Create('user1', 'user2'));

  // Test d'une méthode qui utilise ce service asynchrone
  var Processor := TDataProcessor.Create(Mock);
  var Task := Processor.ProcessUsersAsync;

  // Attendre la fin de la tâche
  Task.Wait;

  // Vérification
  Assert.AreEqual(2, Processor.UsersCount);
  Mock.Verify.Called.Once.On.FetchDataAsync;
end;
```

### 4. Mocking partiel (Partial Mocking)

Le mocking partiel permet de ne remplacer que certaines méthodes d'une classe tout en conservant le comportement réel des autres méthodes.

```pascal
// Mocking partiel
[Test]
procedure TestMockingPartiel;
var
  PartialMock: TPartialMock<TRealCalculator>;
begin
  PartialMock := TPartialMock<TRealCalculator>.Create;

  // Remplacer seulement la méthode GetCurrentDate
  PartialMock.When.GetCurrentDate.Return(EncodeDate(2025, 1, 1));

  // Les autres méthodes utilisent l'implémentation réelle
  var Result := PartialMock.CalculateInterest(1000, 0.05);

  // La méthode réelle utilise la date mockée pour les calculs
  Assert.AreEqual(50, Result);
end;
```

### 5. Mocking d'appels chaînés

```pascal
// Mocking d'appels chaînés
[Test]
procedure TestAppelsChainesExplicites;
var
  Mock: TMock<IQueryBuilder>;
  Query: IQuery;
begin
  Mock := TMock<IQueryBuilder>.Create;
  Query := TMock<IQuery>.Create;

  // Configuration d'appels chaînés
  Mock.When.Select('*').Return(Mock);
  Mock.When.From('Users').Return(Mock);
  Mock.When.Where('ID = 1').Return(Mock);
  Mock.When.Build.Return(Query);

  // Utilisation
  var Builder := Mock;
  var ResultQuery := Builder.Select('*').From('Users').Where('ID = 1').Build;

  // Vérification
  Assert.AreEqual(Query, ResultQuery);
  Mock.Verify.Called.Once.On.Select;
  Mock.Verify.Called.Once.On.From;
  Mock.Verify.Called.Once.On.Where;
  Mock.Verify.Called.Once.On.Build;
end;
```

### 6. Assertions améliorées pour la vérification des mocks

```pascal
// Assertions spécifiques aux mocks
[Test]
procedure TestAssertionsMocks;
var
  Mock: TMock<INotificationService>;
begin
  Mock := TMock<INotificationService>.Create;

  // Configuration
  Mock.When.NotifyUser(Any<Integer>, Any<string>).Return(True);

  // Utilisation
  var Service := TUserService.Create(Mock);
  Service.ProcessUserAction(10);

  // Vérifications améliorées
  Mock.Assert.WasCalled.WithExactArguments(10, 'Action traitée').On.NotifyUser;
  Mock.Assert.WasNotCalled.WithAnyArguments.On.NotifyAdmin;
  Mock.Assert.AllExpectationsMet;
end;
```

## Intégration du mocking avec d'autres techniques de test

### Combinaison avec les tests d'intégration

Vous pouvez combiner les mocks avec des tests d'intégration pour tester des composants spécifiques tout en utilisant des implémentations réelles pour d'autres :

```pascal
[Test]
procedure TestIntegrationAvecMocks;
var
  RealRepository: TClientRepository;
  MockEmailService: TMock<IEmailService>;
  MockLogger: TMock<ILogger>;
begin
  // Utiliser un repository réel pour tester l'intégration avec la base de données
  RealRepository := TClientRepository.Create(GetTestDatabaseConnection);

  // Mocker les services auxiliaires
  MockEmailService := TMock<IEmailService>.Create;
  MockLogger := TMock<ILogger>.Create;

  // Configurer les mocks
  MockEmailService.When.SendEmail(Any<string>, Any<string>).Return(True);

  // Créer le service à tester avec un mélange de composants réels et mockés
  var ClientService := TClientService.Create(
    RealRepository,
    MockEmailService,
    MockLogger
  );

  try
    // Test qui utilise la base de données réelle mais des services mockés
    var Result := ClientService.RegisterClient('Jean', 'jean@example.com');

    // Vérifications
    Assert.IsTrue(Result);
    Assert.IsTrue(RealRepository.ClientExists('jean@example.com'));
    MockEmailService.Verify.Called.Once.With(Any<string>, 'jean@example.com').On.SendEmail;
  finally
    RealRepository.Free;
  end;
end;
```

### Mocks dans les tests dirigés par les données

Les tests dirigés par les données (Data-Driven Tests) peuvent également utiliser des mocks :

```pascal
[Test]
[TestCase('Client valide', 'Jean,jean@example.com,true')]
[TestCase('Email invalide', 'Pierre,pierre@,false')]
[TestCase('Nom vide', ',marie@example.com,false')]
procedure TestValidationClientAvecMock(const Nom, Email: string; ResultatAttendu: Boolean);
var
  MockValidator: TMock<IValidator>;
begin
  MockValidator := TMock<IValidator>.Create;

  // Le mock retourne différentes valeurs selon les entrées
  if Email.Contains('@') and (Email.Contains('.')) and (Nom <> '') then
    MockValidator.When.Validate(Any<TClient>).Return(True)
  else
    MockValidator.When.Validate(Any<TClient>).Return(False);

  var Service := TClientService.Create(MockValidator);
  var Client := TClient.Create;
  try
    Client.Nom := Nom;
    Client.Email := Email;

    var Resultat := Service.EnregistrerClient(Client);

    Assert.AreEqual(ResultatAttendu, Resultat);
    MockValidator.Verify.Called.Once.With(Any<TClient>).On.Validate;
  finally
    Client.Free;
  end;
end;
```

## Challenges et solutions pour le mocking

### 1. Comment mocker les classes concrètes sans interfaces

Si vous devez travailler avec des classes qui n'implémentent pas d'interfaces, plusieurs solutions existent :

#### a. Utiliser l'héritage et la surcharge

```pascal
// Classe originale sans interface
TServiceSansInterface = class
public
  function GetData: TData; virtual;
  procedure ProcessData(Data: TData); virtual;
end;

// Mock par héritage
TMockService = class(TServiceSansInterface)
private
  FGetDataCalled: Boolean;
  FProcessDataCalled: Boolean;
  FMockData: TData;
public
  constructor Create;
  function GetData: TData; override;
  procedure ProcessData(Data: TData); override;
  property GetDataCalled: Boolean read FGetDataCalled;
  property ProcessDataCalled: Boolean read FProcessDataCalled;
end;

constructor TMockService.Create;
begin
  inherited;
  FMockData := TData.Create;
  // Initialiser FMockData avec des valeurs de test
end;

function TMockService.GetData: TData;
begin
  FGetDataCalled := True;
  Result := FMockData;
end;

procedure TMockService.ProcessData(Data: TData);
begin
  FProcessDataCalled := True;
  // Ne rien faire ou simuler le traitement
end;
```

#### b. Refactorisation pour utiliser des interfaces

La meilleure approche à long terme est de refactoriser pour introduire des interfaces :

```pascal
// Définir une interface
IDataService = interface
  ['{A1B2C3D4-E5F6-G7H8-I9J0-K1L2M3N4O5P6}']
  function GetData: TData;
  procedure ProcessData(Data: TData);
end;

// Adapter la classe existante
TServiceExistant = class(TInterfacedObject, IDataService)
public
  function GetData: TData;
  procedure ProcessData(Data: TData);
end;
```

### 2. Comment traiter les dépendances statiques

Les méthodes et classes statiques sont difficiles à mocker. Voici comment gérer ce problème :

#### a. Utiliser un wrapper

```pascal
// Classe statique difficile à mocker
TUtilities = class
public
  class function CalculateHash(const Text: string): string; static;
end;

// Interface pour le wrapper
IHashCalculator = interface
  function CalculateHash(const Text: string): string;
end;

// Wrapper réel
THashCalculator = class(TInterfacedObject, IHashCalculator)
public
  function CalculateHash(const Text: string): string;
end;

function THashCalculator.CalculateHash(const Text: string): string;
begin
  Result := TUtilities.CalculateHash(Text);
end;

// Usage avec injection de dépendances
TMyService = class
private
  FHashCalculator: IHashCalculator;
public
  constructor Create(HashCalculator: IHashCalculator);
  function ProcessText(const Text: string): string;
end;
```

#### b. Utiliser un localisateur de service

```pascal
// Localisateur de service global
TServiceLocator = class
private
  class var FHashCalculator: IHashCalculator;
public
  class procedure RegisterHashCalculator(HashCalculator: IHashCalculator);
  class function GetHashCalculator: IHashCalculator;
end;

// Implémentation
class procedure TServiceLocator.RegisterHashCalculator(HashCalculator: IHashCalculator);
begin
  FHashCalculator := HashCalculator;
end;

class function TServiceLocator.GetHashCalculator: IHashCalculator;
begin
  if not Assigned(FHashCalculator) then
    FHashCalculator := THashCalculator.Create;
  Result := FHashCalculator;
end;

// Utilisation
function TMyService.ProcessText(const Text: string): string;
var
  HashCalculator: IHashCalculator;
begin
  HashCalculator := TServiceLocator.GetHashCalculator;
  Result := HashCalculator.CalculateHash(Text);
end;

// Dans le test
procedure SetupTest;
var
  MockHashCalculator: TMock<IHashCalculator>;
begin
  MockHashCalculator := TMock<IHashCalculator>.Create;
  MockHashCalculator.Setup.WillReturn('MockedHash').When.CalculateHash('test');

  TServiceLocator.RegisterHashCalculator(MockHashCalculator);
end;
```

### 3. Comment gérer les dépendances complexes

Pour les systèmes avec de nombreuses dépendances interconnectées, utilisez un conteneur d'injection de dépendances comme Spring4D:

```pascal
// Configuration du conteneur pour les tests
procedure ConfigureContainer;
var
  Container: TContainer;
begin
  Container := TContainer.Create;

  // Enregistrer toutes les dépendances
  Container.RegisterType<IRepository1, TMockRepository1>.AsSingleton;
  Container.RegisterType<IRepository2, TMockRepository2>.AsSingleton;
  Container.RegisterType<IService1, TMockService1>.AsSingleton;
  Container.RegisterType<IService2, TMockService2>.AsSingleton;
  Container.RegisterType<ILogger, TMockLogger>.AsSingleton;

  // Enregistrer la classe à tester
  Container.RegisterType<TComplexSystem>.AsSingleton;

  Container.Build;

  GlobalContainer := Container;
end;

// Utilisation dans les tests
procedure TestComplexSystem;
var
  System: TComplexSystem;
  Repo1: IRepository1;
  Service1: IService1;
begin
  // Configurer le conteneur
  ConfigureContainer;

  // Obtenir les instances
  System := GlobalContainer.Resolve<TComplexSystem>;
  Repo1 := GlobalContainer.Resolve<IRepository1>;
  Service1 := GlobalContainer.Resolve<IService1>;

  // Configurer les mocks
  (Repo1 as TMockRepository1).Setup...
  (Service1 as TMockService1).Setup...

  // Tester
  System.DoSomethingComplex;

  // Vérifier
  (Repo1 as TMockRepository1).Verify...
  (Service1 as TMockService1).Verify...
end;
```

## Au-delà du mocking : autres techniques de tests avec dépendances

### 1. Tests avec des fakes

Les fakes sont des implémentations légères mais fonctionnelles qui remplacent les composants réels :

```pascal
// Interface
IDatabase = interface
  procedure SaveData(const Key, Value: string);
  function LoadData(const Key: string): string;
end;

// Implémentation de production avec SQLite
TSQLiteDatabase = class(TInterfacedObject, IDatabase)
  // Implémentation réelle
end;

// Fake pour les tests
TInMemoryDatabase = class(TInterfacedObject, IDatabase)
private
  FData: TDictionary<string, string>;
public
  constructor Create;
  destructor Destroy; override;
  procedure SaveData(const Key, Value: string);
  function LoadData(const Key: string): string;
end;

constructor TInMemoryDatabase.Create;
begin
  inherited Create;
  FData := TDictionary<string, string>.Create;
end;

destructor TInMemoryDatabase.Destroy;
begin
  FData.Free;
  inherited;
end;

procedure TInMemoryDatabase.SaveData(const Key, Value: string);
begin
  if FData.ContainsKey(Key) then
    FData[Key] := Value
  else
    FData.Add(Key, Value);
end;

function TInMemoryDatabase.LoadData(const Key: string): string;
begin
  if FData.ContainsKey(Key) then
    Result := FData[Key]
  else
    Result := '';
end;
```

### 2. Tests avec des doublures configurables

Une approche intermédiaire entre les mocks purs et les fakes :

```pascal
// Doublure configurable
TConfigurableEmailSender = class(TInterfacedObject, IEmailSender)
private
  FEmails: TList<TEmailMessage>;
  FShouldSucceed: Boolean;
public
  constructor Create(ShouldSucceed: Boolean = True);
  destructor Destroy; override;
  function SendEmail(const To, Subject, Body: string): Boolean;
  function GetSentEmails: TArray<TEmailMessage>;
  property ShouldSucceed: Boolean read FShouldSucceed write FShouldSucceed;
end;

constructor TConfigurableEmailSender.Create(ShouldSucceed: Boolean);
begin
  inherited Create;
  FEmails := TList<TEmailMessage>.Create;
  FShouldSucceed := ShouldSucceed;
end;

destructor TConfigurableEmailSender.Destroy;
begin
  for var Email in FEmails do
    Email.Free;
  FEmails.Free;
  inherited;
end;

function TConfigurableEmailSender.SendEmail(const To, Subject, Body: string): Boolean;
var
  Email: TEmailMessage;
begin
  Email := TEmailMessage.Create;
  Email.To := To;
  Email.Subject := Subject;
  Email.Body := Body;
  FEmails.Add(Email);

  Result := FShouldSucceed;
end;

function TConfigurableEmailSender.GetSentEmails: TArray<TEmailMessage>;
begin
  Result := FEmails.ToArray;
end;
```

## Conclusion

Le mocking et la gestion des dépendances sont des techniques essentielles pour créer des tests unitaires efficaces. En isolant le code testé de ses dépendances, vous pouvez écrire des tests plus rapides, plus fiables et plus ciblés.

En résumé :

1. **Utilisez des interfaces** pour rendre votre code facilement mockable.

2. **Appliquez l'injection de dépendances** pour découpler les composants.

3. **Choisissez le bon type de simulacre** (mock, stub, fake) selon le besoin du test.

4. **Concentrez-vous sur le comportement** plutôt que sur l'implémentation.

5. **Gardez vos tests lisibles** en structurant bien le code et en évitant les vérifications excessives.

6. **Profitez des frameworks de mocking** comme Delphi-Mocks pour simplifier la création et la configuration des mocks.

La maîtrise du mocking vous permettra d'écrire des tests plus robustes et de concevoir un code plus modulaire et maintenable. Combinée aux tests unitaires et aux tests d'intégration, cette technique vous aidera à construire des applications Delphi de haute qualité.

Dans la prochaine section, nous explorerons le débogage de code multi-thread, une compétence indispensable pour les applications modernes qui tirent parti de l'exécution parallèle.

⏭️ [Déboggage de code multi-thread](12-debogage-et-tests/09-deboggage-de-code-multi-thread.md)
