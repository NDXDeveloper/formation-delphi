🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.8 Mocking et tests avec dépendances

## Introduction

Imaginez que vous voulez tester un système d'alarme de voiture, mais que ce système dépend d'un moteur, d'une batterie, et de capteurs de portes. Devez-vous avoir une voiture complète et fonctionnelle juste pour tester l'alarme ? Non ! Vous pouvez créer des **simulations** (mocks) de ces composants pour tester l'alarme isolément.

C'est exactement le même principe en programmation. Lorsque vous testez une classe qui dépend d'autres classes (base de données, services web, système de fichiers), vous pouvez créer des **mocks** (des "fausses" versions) de ces dépendances pour tester votre code de manière isolée, rapide et fiable.

Pour un débutant, le mocking peut sembler complexe au premier abord, mais c'est en réalité un concept simple qui rend vos tests beaucoup plus faciles à écrire et à maintenir.

## Qu'est-ce qu'une dépendance ?

### Définition simple

Une **dépendance** est quelque chose dont votre code a besoin pour fonctionner. C'est une autre classe, un service, une ressource externe que votre code utilise.

### Exemples de dépendances

**Base de données :**

```pascal
type
  TGestionnaireClients = class
  private
    FDatabase: TDatabaseConnection;  // DÉPENDANCE
  public
    function ChargerClient(ID: Integer): TClient;
    procedure SauvegarderClient(Client: TClient);
  end;
```

`TGestionnaireClients` dépend de `TDatabaseConnection`.

**Service email :**

```pascal
type
  TServiceInscription = class
  private
    FServiceEmail: TEmailService;  // DÉPENDANCE
  public
    procedure InscrireUtilisateur(Nom, Email: string);
  end;
```

`TServiceInscription` dépend de `TEmailService`.

**API externe :**

```pascal
type
  TGestionnairePaiement = class
  private
    FAPIStripe: TStripeAPI;  // DÉPENDANCE
  public
    function ProcesserPaiement(Montant: Currency): Boolean;
  end;
```

`TGestionnairePaiement` dépend de `TStripeAPI`.

### Pourquoi les dépendances posent problème pour les tests

**1. Lenteur**

Accéder à une vraie base de données ou appeler une vraie API est lent. Si vous avez 100 tests qui utilisent la base de données, vos tests peuvent prendre plusieurs minutes.

**2. Imprévisibilité**

Les services externes peuvent être indisponibles, avoir des données qui changent, ou avoir des effets de bord non désirés.

**3. Coût**

Certaines APIs sont payantes. Vous ne voulez pas dépenser de l'argent à chaque fois que vous exécutez vos tests.

**4. Effets de bord**

Envoyer de vrais emails pendant les tests est problématique. Vous ne voulez pas spammer vos utilisateurs de test.

**5. Difficulté à tester les cas d'erreur**

Comment tester que votre code gère bien une panne de base de données si vous ne pouvez pas facilement simuler cette panne ?

## Qu'est-ce qu'un mock ?

### Définition

Un **mock** est une "fausse" implémentation d'une dépendance, créée spécifiquement pour les tests. Il simule le comportement de la vraie dépendance de manière contrôlée et prévisible.

### Analogie

Pensez aux simulateurs de vol :
- Un vrai avion = la vraie dépendance (base de données, API)
- Un simulateur de vol = un mock (simule l'avion pour l'entraînement)

Le simulateur :
- Est beaucoup moins cher que d'utiliser un vrai avion
- Permet de pratiquer sans danger
- Permet de simuler des situations difficiles (pannes, tempêtes)
- Donne des résultats prévisibles

C'est exactement ce que fait un mock !

### Exemple simple

**Vraie implémentation :**

```pascal
type
  IServiceEmail = interface
    ['{GUID}']
    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
  end;

  TServiceEmailReel = class(TInterfacedObject, IServiceEmail)
  public
    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
  end;

function TServiceEmailReel.EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;  
begin  
  // Code réel qui envoie vraiment un email via SMTP
  Result := SMTPClient.Send(Destinataire, Sujet, Message);
end;
```

**Mock pour les tests :**

```pascal
type
  TServiceEmailMock = class(TInterfacedObject, IServiceEmail)
  private
    FEmailsEnvoyes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
    function AEnvoyeEmailA(const Destinataire: string): Boolean;
    procedure Reinitialiser;
  end;

constructor TServiceEmailMock.Create;  
begin  
  FEmailsEnvoyes := TStringList.Create;
end;

destructor TServiceEmailMock.Destroy;  
begin  
  FEmailsEnvoyes.Free;
  inherited;
end;

function TServiceEmailMock.EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;  
begin  
  // Ne fait que noter qu'un email a été "envoyé"
  FEmailsEnvoyes.Add(Format('%s|%s|%s', [Destinataire, Sujet, Message]));
  Result := True;
end;

function TServiceEmailMock.AEnvoyeEmailA(const Destinataire: string): Boolean;  
var  
  i: Integer;
begin
  Result := False;
  for i := 0 to FEmailsEnvoyes.Count - 1 do
  begin
    if FEmailsEnvoyes[i].StartsWith(Destinataire + '|') then
    begin
      Result := True;
      Break;
    end;
  end;
end;
```

**Utilisation dans un test :**

```pascal
procedure TestInscriptionEnvoieEmail;  
var  
  ServiceEmail: IServiceEmail;
  ServiceInscription: TServiceInscription;
begin
  // Arrange : Utiliser le mock
  ServiceEmail := TServiceEmailMock.Create;
  ServiceInscription := TServiceInscription.Create(ServiceEmail);

  try
    // Act : Inscrire un utilisateur
    ServiceInscription.InscrireUtilisateur('Jean Dupont', 'jean@example.com');

    // Assert : Vérifier qu'un email a été "envoyé"
    Assert.IsTrue(
      (ServiceEmail as TServiceEmailMock).AEnvoyeEmailA('jean@example.com'),
      'Un email de confirmation doit être envoyé à l''utilisateur'
    );
  finally
    ServiceInscription.Free;
  end;
end;
```

**Avantages :**
- Le test s'exécute en quelques millisecondes (pas de vraie connexion SMTP)
- Aucun email n'est réellement envoyé
- Le test est fiable et reproductible
- On peut vérifier que l'email a bien été "envoyé"

## Types de test doubles

Le terme générique pour tous les "faux" objets est **test double**. Il existe plusieurs types :

### 1. Dummy

Un objet qui est passé mais jamais vraiment utilisé. Sert juste à remplir une liste de paramètres.

```pascal
type
  TLoggerDummy = class(TInterfacedObject, ILogger)
  public
    procedure Log(const Message: string);
  end;

procedure TLoggerDummy.Log(const Message: string);  
begin  
  // Ne fait rien du tout
end;
```

**Utilisation :** Quand vous devez passer un logger mais que le test ne s'intéresse pas au logging.

### 2. Stub

Un objet qui retourne des valeurs prédéfinies. Il simule des réponses.

```pascal
type
  TRepositoryClientStub = class(TInterfacedObject, IClientRepository)
  public
    function ChargerClient(ID: Integer): TClient;
  end;

function TRepositoryClientStub.ChargerClient(ID: Integer): TClient;  
begin  
  // Retourne toujours le même client de test
  Result := TClient.Create;
  Result.ID := ID;
  Result.Nom := 'Client Test';
  Result.Email := 'test@example.com';
end;
```

**Utilisation :** Quand vous voulez que la dépendance retourne des données spécifiques sans logique complexe.

### 3. Fake

Une implémentation simplifiée mais fonctionnelle. Elle a du comportement réel mais simplifié.

```pascal
type
  TDatabaseFake = class(TInterfacedObject, IDatabase)
  private
    FData: TDictionary<Integer, TClient>;
  public
    constructor Create;
    destructor Destroy; override;

    function ChargerClient(ID: Integer): TClient;
    procedure SauvegarderClient(Client: TClient);
  end;

constructor TDatabaseFake.Create;  
begin  
  // Base de données en mémoire
  FData := TDictionary<Integer, TClient>.Create;
end;

function TDatabaseFake.ChargerClient(ID: Integer): TClient;  
begin  
  if not FData.TryGetValue(ID, Result) then
    Result := nil;
end;

procedure TDatabaseFake.SauvegarderClient(Client: TClient);  
begin  
  FData.AddOrSetValue(Client.ID, Client);
end;
```

**Utilisation :** Quand vous voulez une version simplifiée mais réaliste (par exemple, une base en mémoire au lieu de SQL).

### 4. Spy

Un objet qui enregistre comment il a été utilisé pour vérification ultérieure.

```pascal
type
  TEmailServiceSpy = class(TInterfacedObject, IServiceEmail)
  private
    FNombreAppels: Integer;
    FDernierDestinataire: string;
  public
    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;

    property NombreAppels: Integer read FNombreAppels;
    property DernierDestinataire: string read FDernierDestinataire;
  end;

function TEmailServiceSpy.EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;  
begin  
  Inc(FNombreAppels);
  FDernierDestinataire := Destinataire;
  Result := True;
end;
```

**Utilisation :** Quand vous voulez vérifier qu'une méthode a été appelée, combien de fois, avec quels paramètres.

### 5. Mock (au sens strict)

Un objet configuré avec des attentes. Il vérifie lui-même si les attentes sont remplies.

```pascal
// Exemple conceptuel avec un framework de mocking
Mock := TMock<IServiceEmail>.Create;  
Mock.Setup.WillExecute(  
  function (const Args: TArray<TValue>): TValue
  begin
    Result := True;
  end
);

// Usage
ServiceEmail := Mock;  
ServiceInscription.InscrireUtilisateur('test@example.com');  

// Vérification
Mock.Verify('EnvoyerEmail', Times.Once);
```

**Utilisation :** Avec des frameworks de mocking pour des vérifications automatiques.

### Récapitulatif

| Type | Description | Complexité | Cas d'usage |
|------|-------------|------------|-------------|
| **Dummy** | Ne fait rien | Très simple | Paramètre obligatoire mais non utilisé |
| **Stub** | Retourne des valeurs fixes | Simple | Données de test prédéfinies |
| **Fake** | Implémentation simplifiée | Moyenne | Version allégée mais fonctionnelle |
| **Spy** | Enregistre les appels | Moyenne | Vérifier les interactions |
| **Mock** | Attentes configurables | Complexe | Vérifications sophistiquées |

## Injection de dépendances

Pour utiliser des mocks, vous devez d'abord rendre vos dépendances **injectables**.

### Le problème : Dépendances "en dur"

**Code non testable :**

```pascal
type
  TServiceInscription = class
  public
    procedure InscrireUtilisateur(const Nom, Email: string);
  end;

procedure TServiceInscription.InscrireUtilisateur(const Nom, Email: string);  
var  
  ServiceEmail: TServiceEmailReel;  // ❌ Dépendance créée en dur
begin
  ServiceEmail := TServiceEmailReel.Create;
  try
    // Créer l'utilisateur...

    // Envoyer l'email
    ServiceEmail.EnvoyerEmail(Email, 'Bienvenue', 'Votre inscription est confirmée');
  finally
    ServiceEmail.Free;
  end;
end;
```

**Problème :** Impossible de remplacer `TServiceEmailReel` par un mock pour les tests.

### La solution : Injection par le constructeur

**Code testable :**

```pascal
type
  TServiceInscription = class
  private
    FServiceEmail: IServiceEmail;  // Interface, pas classe concrète
  public
    constructor Create(ServiceEmail: IServiceEmail);
    procedure InscrireUtilisateur(const Nom, Email: string);
  end;

constructor TServiceInscription.Create(ServiceEmail: IServiceEmail);  
begin  
  FServiceEmail := ServiceEmail;
end;

procedure TServiceInscription.InscrireUtilisateur(const Nom, Email: string);  
begin  
  // Créer l'utilisateur...

  // Envoyer l'email via l'interface injectée
  FServiceEmail.EnvoyerEmail(Email, 'Bienvenue', 'Votre inscription est confirmée');
end;
```

**Avantages :**
- En production : `TServiceInscription.Create(TServiceEmailReel.Create)`
- En test : `TServiceInscription.Create(TServiceEmailMock.Create)`

### Autres types d'injection

**Injection par propriété :**

```pascal
type
  TServiceInscription = class
  private
    FServiceEmail: IServiceEmail;
  public
    property ServiceEmail: IServiceEmail read FServiceEmail write FServiceEmail;
    procedure InscrireUtilisateur(const Nom, Email: string);
  end;

// Usage
Service := TServiceInscription.Create;  
Service.ServiceEmail := TServiceEmailMock.Create;  // Injection  
```

**Injection par méthode :**

```pascal
type
  TServiceInscription = class
  public
    procedure InscrireUtilisateur(const Nom, Email: string;
                                   ServiceEmail: IServiceEmail);
  end;

procedure TServiceInscription.InscrireUtilisateur(const Nom, Email: string;
                                                   ServiceEmail: IServiceEmail);
begin
  // Utiliser ServiceEmail passé en paramètre
  ServiceEmail.EnvoyerEmail(Email, 'Bienvenue', 'Inscription confirmée');
end;
```

**Recommandation :** L'injection par constructeur est généralement la meilleure approche car elle rend les dépendances explicites et obligatoires.

## Interfaces : La clé du mocking

### Pourquoi utiliser des interfaces ?

Les interfaces permettent de :
- Définir un contrat sans imposer d'implémentation
- Remplacer facilement une implémentation par une autre
- Créer des mocks qui respectent le même contrat

### Définir une interface

```pascal
type
  IClientRepository = interface
    ['{12345678-1234-1234-1234-123456789012}']
    function ChargerClient(ID: Integer): TClient;
    procedure SauvegarderClient(Client: TClient);
    procedure SupprimerClient(ID: Integer);
    function ListerTous: TList<TClient>;
  end;
```

**Important :** Toujours mettre un GUID sur les interfaces en Delphi.

### Implémentation réelle

```pascal
type
  TClientRepositorySQL = class(TInterfacedObject, IClientRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(Connection: TFDConnection);

    function ChargerClient(ID: Integer): TClient;
    procedure SauvegarderClient(Client: TClient);
    procedure SupprimerClient(ID: Integer);
    function ListerTous: TList<TClient>;
  end;
```

### Mock pour les tests

```pascal
type
  TClientRepositoryMock = class(TInterfacedObject, IClientRepository)
  private
    FClients: TDictionary<Integer, TClient>;
  public
    constructor Create;
    destructor Destroy; override;

    function ChargerClient(ID: Integer): TClient;
    procedure SauvegarderClient(Client: TClient);
    procedure SupprimerClient(ID: Integer);
    function ListerTous: TList<TClient>;

    // Méthodes helper pour les tests
    procedure AjouterClientTest(Client: TClient);
    function ContientClient(ID: Integer): Boolean;
  end;
```

### Utilisation

```pascal
// Production
Repository := TClientRepositorySQL.Create(Connection);

// Tests
Repository := TClientRepositoryMock.Create;
```

Le code qui utilise `IClientRepository` ne voit pas la différence !

## Exemple complet : Tester une logique métier complexe

Imaginons un système de commande en ligne avec plusieurs dépendances.

### Définition des interfaces

```pascal
type
  // Interface pour le repository de clients
  IClientRepository = interface
    ['{A1B2C3D4-1234-1234-1234-123456789012}']
    function ChargerClient(ID: Integer): TClient;
  end;

  // Interface pour le repository de produits
  IProduitRepository = interface
    ['{B2C3D4E5-1234-1234-1234-123456789012}']
    function ChargerProduit(ID: Integer): TProduit;
  end;

  // Interface pour le service de paiement
  IServicePaiement = interface
    ['{C3D4E5F6-1234-1234-1234-123456789012}']
    function ProcesserPaiement(ClientID: Integer; Montant: Currency): Boolean;
  end;

  // Interface pour le service email
  IServiceEmail = interface
    ['{D4E5F6A7-1234-1234-1234-123456789012}']
    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
  end;
```

### Classe métier avec dépendances

```pascal
type
  TGestionnaireCommandes = class
  private
    FClientRepo: IClientRepository;
    FProduitRepo: IProduitRepository;
    FServicePaiement: IServicePaiement;
    FServiceEmail: IServiceEmail;
  public
    constructor Create(ClientRepo: IClientRepository;
                      ProduitRepo: IProduitRepository;
                      ServicePaiement: IServicePaiement;
                      ServiceEmail: IServiceEmail);

    function CreerCommande(ClientID: Integer;
                          ProduitsIDs: TArray<Integer>): TCommande;
  end;

constructor TGestionnaireCommandes.Create(ClientRepo: IClientRepository;
                                          ProduitRepo: IProduitRepository;
                                          ServicePaiement: IServicePaiement;
                                          ServiceEmail: IServiceEmail);
begin
  FClientRepo := ClientRepo;
  FProduitRepo := ProduitRepo;
  FServicePaiement := ServicePaiement;
  FServiceEmail := ServiceEmail;
end;

function TGestionnaireCommandes.CreerCommande(ClientID: Integer;
                                              ProduitsIDs: TArray<Integer>): TCommande;
var
  Client: TClient;
  Produit: TProduit;
  ProduitID: Integer;
  MontantTotal: Currency;
begin
  // 1. Charger le client
  Client := FClientRepo.ChargerClient(ClientID);
  if Client = nil then
    raise Exception.Create('Client introuvable');

  // 2. Créer la commande
  Result := TCommande.Create;
  Result.ClientID := ClientID;
  Result.DateCommande := Now;

  // 3. Ajouter les produits et calculer le total
  MontantTotal := 0;
  for ProduitID in ProduitsIDs do
  begin
    Produit := FProduitRepo.ChargerProduit(ProduitID);
    if Produit = nil then
      raise Exception.CreateFmt('Produit %d introuvable', [ProduitID]);

    Result.AjouterProduit(Produit);
    MontantTotal := MontantTotal + Produit.Prix;
  end;

  Result.MontantTotal := MontantTotal;

  // 4. Processer le paiement
  if not FServicePaiement.ProcesserPaiement(ClientID, MontantTotal) then
    raise Exception.Create('Paiement échoué');

  Result.Statut := stPayee;

  // 5. Envoyer email de confirmation
  FServiceEmail.EnvoyerEmail(
    Client.Email,
    'Confirmation de commande',
    Format('Votre commande de %.2f € a été confirmée', [MontantTotal])
  );
end;
```

### Mocks pour les tests

```pascal
type
  TClientRepositoryMock = class(TInterfacedObject, IClientRepository)
  private
    FClients: TDictionary<Integer, TClient>;
  public
    constructor Create;
    destructor Destroy; override;

    function ChargerClient(ID: Integer): TClient;
    procedure AjouterClient(Client: TClient);
  end;

  TProduitRepositoryMock = class(TInterfacedObject, IProduitRepository)
  private
    FProduits: TDictionary<Integer, TProduit>;
  public
    constructor Create;
    destructor Destroy; override;

    function ChargerProduit(ID: Integer): TProduit;
    procedure AjouterProduit(Produit: TProduit);
  end;

  TServicePaiementMock = class(TInterfacedObject, IServicePaiement)
  private
    FDoitReussir: Boolean;
  public
    constructor Create(DoitReussir: Boolean);
    function ProcesserPaiement(ClientID: Integer; Montant: Currency): Boolean;
  end;

  TServiceEmailMock = class(TInterfacedObject, IServiceEmail)
  private
    FEmailsEnvoyes: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
    function AEnvoyeEmailA(const Destinataire: string): Boolean;
  end;
```

### Tests unitaires avec mocks

```pascal
procedure TestCreerCommandeAvecSucces;  
var  
  ClientRepo: IClientRepository;
  ProduitRepo: IProduitRepository;
  ServicePaiement: IServicePaiement;
  ServiceEmail: IServiceEmail;
  Gestionnaire: TGestionnaireCommandes;
  Client: TClient;
  Produit1, Produit2: TProduit;
  Commande: TCommande;
begin
  // Arrange : Créer tous les mocks
  ClientRepo := TClientRepositoryMock.Create;
  ProduitRepo := TProduitRepositoryMock.Create;
  ServicePaiement := TServicePaiementMock.Create(True);  // Paiement réussira
  ServiceEmail := TServiceEmailMock.Create;

  // Préparer les données de test
  Client := TClient.Create;
  Client.ID := 1;
  Client.Nom := 'Dupont';
  Client.Email := 'dupont@example.com';
  (ClientRepo as TClientRepositoryMock).AjouterClient(Client);

  Produit1 := TProduit.Create;
  Produit1.ID := 10;
  Produit1.Nom := 'Livre';
  Produit1.Prix := 15.99;
  (ProduitRepo as TProduitRepositoryMock).AjouterProduit(Produit1);

  Produit2 := TProduit.Create;
  Produit2.ID := 20;
  Produit2.Nom := 'Stylo';
  Produit2.Prix := 2.50;
  (ProduitRepo as TProduitRepositoryMock).AjouterProduit(Produit2);

  // Créer le gestionnaire avec les mocks
  Gestionnaire := TGestionnaireCommandes.Create(
    ClientRepo, ProduitRepo, ServicePaiement, ServiceEmail
  );

  try
    // Act : Créer la commande
    Commande := Gestionnaire.CreerCommande(1, [10, 20]);

    try
      // Assert : Vérifier que la commande est correcte
      Assert.IsNotNull(Commande, 'La commande doit être créée');
      Assert.AreEqual(1, Commande.ClientID, 'Client ID doit correspondre');
      Assert.AreEqual(18.49, Commande.MontantTotal, 0.01, 'Montant total doit être correct');
      Assert.AreEqual(stPayee, Commande.Statut, 'La commande doit être payée');

      // Vérifier qu'un email a été envoyé
      Assert.IsTrue(
        (ServiceEmail as TServiceEmailMock).AEnvoyeEmailA('dupont@example.com'),
        'Un email de confirmation doit être envoyé'
      );
    finally
      Commande.Free;
    end;
  finally
    Gestionnaire.Free;
  end;
end;

procedure TestCreerCommandeAvecPaiementEchoue;  
var  
  ClientRepo: IClientRepository;
  ProduitRepo: IProduitRepository;
  ServicePaiement: IServicePaiement;
  ServiceEmail: IServiceEmail;
  Gestionnaire: TGestionnaireCommandes;
  ExceptionLevee: Boolean;
begin
  // Arrange
  ClientRepo := TClientRepositoryMock.Create;
  ProduitRepo := TProduitRepositoryMock.Create;
  ServicePaiement := TServicePaiementMock.Create(False);  // Paiement échouera
  ServiceEmail := TServiceEmailMock.Create;

  // Préparer les données...
  // (même code que précédemment)

  Gestionnaire := TGestionnaireCommandes.Create(
    ClientRepo, ProduitRepo, ServicePaiement, ServiceEmail
  );

  try
    // Act & Assert : Vérifier qu'une exception est levée
    ExceptionLevee := False;
    try
      Gestionnaire.CreerCommande(1, [10]);
    except
      on E: Exception do
      begin
        ExceptionLevee := True;
        Assert.Contains('Paiement échoué', E.Message,
                       'Le message d''erreur doit mentionner le paiement');
      end;
    end;

    Assert.IsTrue(ExceptionLevee, 'Une exception doit être levée en cas d''échec du paiement');
  finally
    Gestionnaire.Free;
  end;
end;
```

**Avantages de cette approche :**
- Tests ultra-rapides (pas d'accès BD, pas d'emails, pas d'API)
- Tests fiables (pas de dépendances externes)
- Facile de tester les cas d'erreur (paiement échoué, produit inexistant)
- Isolation complète de la logique métier

## Frameworks de mocking pour Delphi

Créer des mocks manuellement peut devenir répétitif. Les frameworks de mocking automatisent ce processus.

### Delphi Mocks

**Delphi Mocks** est un framework populaire pour créer des mocks dynamiques.

**Installation :**
- Télécharger depuis GitHub : https://github.com/VSoftTechnologies/Delphi-Mocks
- Via GetIt Package Manager dans Delphi

**Exemple d'utilisation :**

```pascal
uses
  Delphi.Mocks;

procedure TestAvecDelphiMocks;  
var  
  MockEmail: TMock<IServiceEmail>;
  ServiceInscription: TServiceInscription;
begin
  // Créer un mock
  MockEmail := TMock<IServiceEmail>.Create;

  // Configurer le comportement
  MockEmail.Setup.WillReturn(True).When.EnvoyerEmail('', '', '');

  // Utiliser le mock
  ServiceInscription := TServiceInscription.Create(MockEmail);
  try
    ServiceInscription.InscrireUtilisateur('Jean', 'jean@example.com');
  finally
    ServiceInscription.Free;
  end;

  // Vérifier que la méthode a été appelée
  MockEmail.Verify('EnvoyerEmail').Once;
end;
```

**Fonctionnalités :**
- Création automatique de mocks
- Configuration du comportement (WillReturn, WillRaise)
- Vérification des appels (Once, Never, AtLeast, etc.)
- Support des paramètres

### Spring4D

**Spring4D** inclut un système de mocking intégré.

**Exemple :**

```pascal
uses
  Spring.Mocking;

procedure TestAvecSpring4D;  
var  
  Mock: Mock<IServiceEmail>;
  ServiceInscription: TServiceInscription;
begin
  // Créer le mock
  Mock.Setup.Returns(True).When.EnvoyerEmail(Arg.IsAny<string>,
                                             Arg.IsAny<string>,
                                             Arg.IsAny<string>);

  ServiceInscription := TServiceInscription.Create(Mock);
  try
    ServiceInscription.InscrireUtilisateur('Jean', 'jean@example.com');
  finally
    ServiceInscription.Free;
  end;

  Mock.Verify.CalledOnce.When.EnvoyerEmail('jean@example.com',
                                           Arg.IsAny<string>,
                                           Arg.IsAny<string>);
end;
```

**Avantages de Spring4D :**
- Framework complet (IoC, DI, Collections)
- Mocking intégré
- Syntaxe fluide
- Bien maintenu

### Comparaison

| Framework | Avantages | Inconvénients |
|-----------|-----------|---------------|
| **Mocks manuels** | Contrôle total, simple à comprendre | Répétitif, plus de code |
| **Delphi Mocks** | Automatique, syntaxe claire | Dépendance externe |
| **Spring4D** | Complet, bien intégré | Courbe d'apprentissage |

**Recommandation pour débutants :** Commencez par des mocks manuels pour bien comprendre les concepts, puis passez à un framework.

## Quand utiliser des mocks ?

### Utilisez des mocks pour :

**1. Les dépendances externes**
- Bases de données
- Services web / APIs
- Systèmes de fichiers
- Services email / SMS
- Services de paiement

**2. Les opérations lentes**
- Tout ce qui prend plus d'une seconde
- Opérations réseau
- Calculs complexes

**3. Les opérations avec effets de bord**
- Envoi d'emails
- Modifications de fichiers
- Transactions financières
- Modifications de données externes

**4. Les cas difficiles à reproduire**
- Erreurs réseau
- Pannes de service
- Conditions de concurrence
- Limites système atteintes

### N'utilisez PAS de mocks pour :

**1. Les objets de valeur simples**

```pascal
// NE PAS mocker
Mock := TMock<TDateHelper>.Create;

// Utiliser la vraie classe
Helper := TDateHelper.Create;
```

**2. Les opérations très simples**

Si créer et utiliser le vrai objet est aussi simple que créer un mock, utilisez le vrai objet.

**3. Les dépendances que vous voulez vraiment tester**

Les tests d'intégration utilisent de vraies dépendances pour tester les interactions réelles.

**4. La logique que vous testez**

Ne moquez pas la classe que vous testez, seulement ses dépendances.

## Techniques avancées de mocking

### Mocks avec état

Un mock peut maintenir un état pour simuler un comportement réaliste.

```pascal
type
  TServicePaiementMockAvecSolde = class(TInterfacedObject, IServicePaiement)
  private
    FSoldes: TDictionary<Integer, Currency>;
  public
    constructor Create;
    destructor Destroy; override;

    function ProcesserPaiement(ClientID: Integer; Montant: Currency): Boolean;
    procedure DefinirSolde(ClientID: Integer; Solde: Currency);
  end;

function TServicePaiementMockAvecSolde.ProcesserPaiement(ClientID: Integer;
                                                          Montant: Currency): Boolean;
var
  Solde: Currency;
begin
  if FSoldes.TryGetValue(ClientID, Solde) then
  begin
    if Solde >= Montant then
    begin
      FSoldes[ClientID] := Solde - Montant;  // Déduire le montant
      Result := True;
    end
    else
      Result := False;  // Solde insuffisant
  end
  else
    Result := False;  // Client inconnu
end;
```

**Utilisation :**

```pascal
procedure TestPaiementAvecSoldeInsuffisant;  
var  
  ServicePaiement: IServicePaiement;
  Mock: TServicePaiementMockAvecSolde;
begin
  Mock := TServicePaiementMockAvecSolde.Create;
  ServicePaiement := Mock;

  // Définir un solde de 10€
  Mock.DefinirSolde(1, 10.00);

  // Tenter un paiement de 20€
  Assert.IsFalse(
    ServicePaiement.ProcesserPaiement(1, 20.00),
    'Le paiement doit échouer si le solde est insuffisant'
  );
end;
```

### Mocks avec séquences de réponses

Un mock peut retourner des réponses différentes à chaque appel.

```pascal
type
  TServiceAPIMockAvecSequence = class(TInterfacedObject, IServiceAPI)
  private
    FReponses: TQueue<string>;
  public
    constructor Create;
    destructor Destroy; override;

    function AppelerAPI(const Endpoint: string): string;
    procedure AjouterReponse(const Reponse: string);
  end;

function TServiceAPIMockAvecSequence.AppelerAPI(const Endpoint: string): string;  
begin  
  if FReponses.Count > 0 then
    Result := FReponses.Dequeue
  else
    raise Exception.Create('Plus de réponses configurées');
end;
```

**Utilisation :**

```pascal
procedure TestRetryAvecSequenceReponses;  
var  
  ServiceAPI: IServiceAPI;
  Mock: TServiceAPIMockAvecSequence;
begin
  Mock := TServiceAPIMockAvecSequence.Create;
  ServiceAPI := Mock;

  // Première fois : erreur
  Mock.AjouterReponse('{"error": "timeout"}');
  // Deuxième fois : succès
  Mock.AjouterReponse('{"result": "ok"}');

  // Tester que le retry fonctionne
  Result := ServiceAvecRetry.Executer(ServiceAPI);
  Assert.AreEqual('ok', Result);
end;
```

### Vérification d'ordre d'appels

Vérifier que les méthodes sont appelées dans le bon ordre.

```pascal
type
  TServiceTransactionMock = class(TInterfacedObject, IServiceTransaction)
  private
    FAppels: TList<string>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Commencer;
    procedure Valider;
    procedure Annuler;

    function VerifierOrdre(const OrdreAttendu: TArray<string>): Boolean;
  end;

function TServiceTransactionMock.VerifierOrdre(const OrdreAttendu: TArray<string>): Boolean;  
var  
  i: Integer;
begin
  Result := FAppels.Count = Length(OrdreAttendu);
  if Result then
  begin
    for i := 0 to FAppels.Count - 1 do
    begin
      if FAppels[i] <> OrdreAttendu[i] then
      begin
        Result := False;
        Break;
      end;
    end;
  end;
end;
```

**Test :**

```pascal
procedure TestTransactionOrdreCorrect;  
var  
  Mock: TServiceTransactionMock;
begin
  Mock := TServiceTransactionMock.Create;
  try
    // Exécuter des opérations
    Mock.Commencer;
    // ... opérations ...
    Mock.Valider;

    // Vérifier l'ordre
    Assert.IsTrue(
      Mock.VerifierOrdre(['Commencer', 'Valider']),
      'Les méthodes doivent être appelées dans le bon ordre'
    );
  finally
    Mock.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Utilisez des interfaces

Toujours définir des interfaces pour les dépendances que vous voulez mocker.

```pascal
// BON
type
  IServiceEmail = interface
    function EnvoyerEmail(...): Boolean;
  end;

// MAUVAIS - Difficile à mocker
type
  TServiceEmail = class
    function EnvoyerEmail(...): Boolean;
  end;
```

### 2. Nommage clair

Nommez vos mocks de manière explicite :

```pascal
// Bon nommage
TServiceEmailMock  
TClientRepositoryStub  
TDatabaseFake  

// Nommage confus
TServiceEmail2  
TTestClient  
TMyRepo  
```

### 3. Un mock par test (généralement)

Créez des mocks frais pour chaque test pour éviter les dépendances entre tests.

```pascal
[Setup]
procedure Setup;  
begin  
  // Créer des mocks frais avant chaque test
  FMockEmail := TServiceEmailMock.Create;
  FMockDatabase := TDatabaseMock.Create;
end;

[TearDown]
procedure TearDown;  
begin  
  // Nettoyer après chaque test
  FMockEmail := nil;  // Interface, libération automatique
  FMockDatabase := nil;
end;
```

### 4. Ne sur-spécifiez pas

Ne vérifiez que ce qui est important pour le test.

```pascal
// MAUVAIS - Trop de détails
Mock.Verify('EnvoyerEmail').WasCalledExactlyOnce
    .WithParameter('destinataire', 'exact@example.com')
    .WithParameter('sujet', 'Texte exact du sujet')
    .WithParameter('message', 'Texte exact du message');

// BON - Vérifier l'essentiel
Mock.Verify('EnvoyerEmail').WasCalledOnce;  
Assert.IsTrue(MockEmail.AEnvoyeEmailA('exact@example.com'));  
```

### 5. Documentation des mocks

Documentez le comportement de vos mocks :

```pascal
type
  /// <summary>
  /// Mock du service de paiement.
  /// Retourne toujours True sauf si DefinirEchec est appelé.
  /// Enregistre tous les paiements pour vérification.
  /// </summary>
  TServicePaiementMock = class(TInterfacedObject, IServicePaiement)
  ...
```

### 6. Mocks réutilisables

Créez des mocks réutilisables dans une unité dédiée :

```pascal
unit TestHelpers;

interface

type
  TServiceEmailMockBuilder = class
  public
    class function UnMockQuiReussit: IServiceEmail;
    class function UnMockQuiEchoue: IServiceEmail;
    class function UnMockEnregistreur: IServiceEmail;
  end;
```

### 7. Évitez les mocks trop complexes

Si votre mock devient très complexe, c'est peut-être un signe que :
- La classe testée fait trop de choses
- Le mock essaie de faire trop
- Vous devriez utiliser un fake ou un stub plus simple

```pascal
// Si votre mock ressemble à ça, c'est trop complexe
type
  TMonMockTropComplexe = class
  private
    FEtat1, FEtat2, FEtat3: Integer;
    FConfig1, FConfig2: Boolean;
    FReponses: TDictionary<string, TQueue<TValue>>;
    // ... 50 lignes de plus ...
  end;
```

### 8. Testez le comportement, pas l'implémentation

```pascal
// MAUVAIS - Teste l'implémentation
Mock.Verify('MethodeInterne').WasCalled;

// BON - Teste le comportement visible
Assert.IsTrue(ServiceEmail.AEnvoyeConfirmation);
```

## Tests avec dépendances multiples

### Organisation avec plusieurs mocks

```pascal
type
  [TestFixture]
  TTestServiceComplexe = class
  private
    FMockClient: IClientRepository;
    FMockProduit: IProduitRepository;
    FMockPaiement: IServicePaiement;
    FMockEmail: IServiceEmail;
    FService: TServiceComplexe;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestScenario1;

    [Test]
    procedure TestScenario2;
  private
    procedure ConfigurerMocksScenarioNormal;
    procedure ConfigurerMocksScenarioErreur;
  end;

procedure TTestServiceComplexe.Setup;  
begin  
  // Créer tous les mocks
  FMockClient := TClientRepositoryMock.Create;
  FMockProduit := TProduitRepositoryMock.Create;
  FMockPaiement := TServicePaiementMock.Create(True);
  FMockEmail := TServiceEmailMock.Create;

  // Créer le service avec toutes ses dépendances
  FService := TServiceComplexe.Create(
    FMockClient,
    FMockProduit,
    FMockPaiement,
    FMockEmail
  );
end;

procedure TTestServiceComplexe.TearDown;  
begin  
  FService.Free;
  // Les interfaces sont libérées automatiquement
end;

procedure TTestServiceComplexe.ConfigurerMocksScenarioNormal;  
var  
  Client: TClient;
  Produit: TProduit;
begin
  // Préparer les données pour un scénario normal
  Client := TClient.Create;
  Client.ID := 1;
  Client.Nom := 'Test';
  (FMockClient as TClientRepositoryMock).AjouterClient(Client);

  Produit := TProduit.Create;
  Produit.ID := 10;
  Produit.Prix := 50.00;
  (FMockProduit as TProduitRepositoryMock).AjouterProduit(Produit);
end;

procedure TTestServiceComplexe.TestScenario1;  
begin  
  // Arrange
  ConfigurerMocksScenarioNormal;

  // Act
  Resultat := FService.ExecuterOperation(1, 10);

  // Assert
  Assert.IsTrue(Resultat);
  Assert.IsTrue((FMockEmail as TServiceEmailMock).AEnvoyeEmail);
end;
```

## Exercice de refactoring vers la testabilité

### Code initial (non testable)

```pascal
type
  TServiceCommande = class
  public
    function CreerCommande(ClientID: Integer): Boolean;
  end;

function TServiceCommande.CreerCommande(ClientID: Integer): Boolean;  
var  
  Connection: TFDConnection;
  Query: TFDQuery;
  Client: TClient;
  EmailService: TEmailService;
begin
  // Création directe des dépendances - NON TESTABLE
  Connection := TFDConnection.Create(nil);
  try
    Connection.DriverName := 'MySQL';
    Connection.Params.Database := 'production_db';
    Connection.Connected := True;

    Query := TFDQuery.Create(nil);
    try
      Query.Connection := Connection;
      Query.SQL.Text := 'SELECT * FROM Clients WHERE ID = :ID';
      Query.ParamByName('ID').AsInteger := ClientID;
      Query.Open;

      if Query.RecordCount > 0 then
      begin
        Client := TClient.Create;
        Client.Nom := Query.FieldByName('Nom').AsString;

        // Autre dépendance en dur
        EmailService := TEmailService.Create;
        try
          EmailService.EnvoyerEmail(Client.Email, 'Commande', 'Confirmée');
        finally
          EmailService.Free;
        end;

        Result := True;
      end
      else
        Result := False;
    finally
      Query.Free;
    end;
  finally
    Connection.Free;
  end;
end;
```

**Problèmes :**
- Se connecte à la vraie base de données
- Envoie de vrais emails
- Impossible de tester sans infrastructure complète
- Difficile de tester les cas d'erreur

### Code refactoré (testable)

```pascal
// 1. Définir les interfaces
type
  IClientRepository = interface
    ['{GUID}']
    function ChargerClient(ID: Integer): TClient;
  end;

  IServiceEmail = interface
    ['{GUID}']
    function EnvoyerEmail(const Destinataire, Sujet, Message: string): Boolean;
  end;

// 2. Refactorer la classe avec injection de dépendances
type
  TServiceCommande = class
  private
    FClientRepo: IClientRepository;
    FServiceEmail: IServiceEmail;
  public
    constructor Create(ClientRepo: IClientRepository;
                      ServiceEmail: IServiceEmail);
    function CreerCommande(ClientID: Integer): Boolean;
  end;

constructor TServiceCommande.Create(ClientRepo: IClientRepository;
                                    ServiceEmail: IServiceEmail);
begin
  FClientRepo := ClientRepo;
  FServiceEmail := ServiceEmail;
end;

function TServiceCommande.CreerCommande(ClientID: Integer): Boolean;  
var  
  Client: TClient;
begin
  // Utiliser les interfaces injectées
  Client := FClientRepo.ChargerClient(ClientID);

  if Assigned(Client) then
  begin
    try
      FServiceEmail.EnvoyerEmail(
        Client.Email,
        'Commande',
        'Confirmée'
      );
      Result := True;
    finally
      Client.Free;
    end;
  end
  else
    Result := False;
end;
```

**Maintenant testable :**

```pascal
procedure TestCreerCommandeAvecClientExistant;  
var  
  MockRepo: IClientRepository;
  MockEmail: IServiceEmail;
  Service: TServiceCommande;
  Client: TClient;
begin
  // Arrange
  MockRepo := TClientRepositoryMock.Create;
  MockEmail := TServiceEmailMock.Create;

  Client := TClient.Create;
  Client.ID := 1;
  Client.Email := 'test@example.com';
  (MockRepo as TClientRepositoryMock).AjouterClient(Client);

  Service := TServiceCommande.Create(MockRepo, MockEmail);
  try
    // Act
    Result := Service.CreerCommande(1);

    // Assert
    Assert.IsTrue(Result);
    Assert.IsTrue((MockEmail as TServiceEmailMock).AEnvoyeEmail);
  finally
    Service.Free;
  end;
end;
```

## Conseils pour débutants

### 1. Commencez simple

Ne vous lancez pas immédiatement dans les frameworks de mocking. Créez d'abord quelques mocks manuels pour bien comprendre le concept.

### 2. Apprenez l'injection de dépendances

C'est la base du mocking. Sans injection de dépendances, vous ne pouvez pas utiliser de mocks efficacement.

### 3. Interfaces d'abord

Habituez-vous à définir des interfaces pour vos composants importants. C'est une bonne pratique même sans mocking.

### 4. Un pas à la fois

Si vous avez du code legacy non testable :
1. Identifiez les dépendances
2. Créez des interfaces
3. Refactorez pour utiliser l'injection de dépendances
4. Créez les mocks
5. Écrivez les tests

Ne tentez pas de tout refactorer d'un coup.

### 5. Acceptez la complexité initiale

Les premières fois que vous créez des mocks, cela peut sembler beaucoup de travail. Avec l'expérience, cela devient naturel et rapide.

### 6. Réutilisez vos mocks

Créez une bibliothèque de mocks réutilisables. Vous n'aurez pas à les recréer pour chaque test.

### 7. Ne moquez pas tout

Moquez seulement ce qui pose problème (lent, externe, avec effets de bord). Les objets simples n'ont pas besoin de mocks.

### 8. Testez d'abord sans mocks

Si possible, écrivez d'abord un test avec les vraies dépendances pour comprendre le comportement attendu. Ensuite, remplacez par des mocks.

## Checklist du mocking

**□ Design**
- [ ] Les dépendances sont définies comme des interfaces
- [ ] L'injection de dépendances est utilisée
- [ ] Les classes sont conçues pour être testables
- [ ] Les responsabilités sont bien séparées

**□ Mocks**
- [ ] Les mocks sont créés pour les dépendances externes
- [ ] Les mocks sont simples et ciblés
- [ ] Les mocks sont documentés
- [ ] Les mocks sont réutilisables

**□ Tests**
- [ ] Chaque test crée ses propres mocks
- [ ] Les tests ne dépendent pas les uns des autres
- [ ] Les tests vérifient le comportement, pas l'implémentation
- [ ] Les messages d'assertion sont clairs

**□ Maintenance**
- [ ] Les mocks évoluent avec les interfaces
- [ ] Les tests restent rapides
- [ ] Le code reste lisible
- [ ] La couverture de tests est bonne

## Pièges courants à éviter

### 1. Sur-mocking

**Problème :**

```pascal
// Tout mocker, même les objets simples
MockDateHelper := TMock<TDateHelper>.Create;  
MockStringHelper := TMock<TStringHelper>.Create;  
MockMathHelper := TMock<TMathHelper>.Create;  
// ... et 20 autres mocks ...
```

**Solution :** Moquez seulement ce qui est nécessaire (dépendances externes, lentes, ou avec effets de bord).

### 2. Tests fragiles

**Problème :**

```pascal
// Test trop couplé à l'implémentation
Mock.Verify('MethodeInterne1').WasCalled;  
Mock.Verify('MethodeInterne2').WasCalledAfter('MethodeInterne1');  
Mock.Verify('MethodeInterne3').WasCalledExactly(3).Times;  
```

Si l'implémentation change, le test casse même si le comportement est correct.

**Solution :** Testez le comportement observable, pas les détails d'implémentation.

### 3. Mocks qui cachent des bugs

**Problème :**

```pascal
// Mock qui retourne toujours True
MockPaiement.Setup.WillReturn(True);
```

Si le vrai service de paiement a un bug, vous ne le détecterez pas car le mock réussit toujours.

**Solution :** Complétez avec des tests d'intégration qui utilisent les vraies implémentations.

### 4. Duplication de logique

**Problème :**

```pascal
// Le mock réimplémente toute la logique métier
function TMockComplexe.Calculer: Currency;  
begin  
  // 100 lignes de logique...
end;
```

**Solution :** Les mocks doivent être simples. Si vous devez réimplémenter la logique, utilisez la vraie classe ou un fake.

### 5. Négliger le nettoyage

**Problème :**

```pascal
procedure Test1;  
var  
  Mock: TMonMock;
begin
  Mock := TMonMock.Create;
  // ... test ...
  // Oubli de Mock.Free; - fuite mémoire
end;
```

**Solution :** Utilisez des interfaces (libération automatique) ou des blocs try...finally.

## Conclusion

Le mocking est une technique puissante qui rend vos tests unitaires rapides, fiables et indépendants des ressources externes. Bien maîtrisé, il transforme complètement votre approche du testing et améliore la qualité de votre code Delphi.

**Points clés à retenir :**

**Dépendances :** Identifiez et isolez les dépendances de vos classes pour faciliter le testing.

**Interfaces :** Utilisez des interfaces pour définir les contrats et permettre l'injection de dépendances.

**Types de test doubles :** Comprenez les différences entre dummy, stub, fake, spy et mock, et utilisez le bon type pour chaque situation.

**Injection de dépendances :** Adoptez l'injection par constructeur pour rendre vos classes testables.

**Mocks manuels vs frameworks :** Commencez par des mocks manuels pour comprendre les concepts, puis utilisez des frameworks pour gagner du temps.

**Équilibre :** Ne moquez pas tout - utilisez les vraies implémentations quand c'est simple et approprié.

**Tests d'intégration :** Complétez les tests unitaires avec mocks par des tests d'intégration utilisant les vraies dépendances.

**Simplicité :** Gardez vos mocks simples et ciblés. Un mock complexe est souvent un signal d'un problème de design.

**Comportement vs implémentation :** Testez ce que fait votre code (comportement observable), pas comment il le fait (détails d'implémentation).

En maîtrisant le mocking et les tests avec dépendances, vous rejoignez les rangs des développeurs qui écrivent du code robuste, maintenable et bien testé. C'est un investissement en temps qui rapporte énormément en termes de qualité de code, de confiance lors des modifications, et de réduction des bugs. Pratiquez régulièrement ces techniques et elles deviendront une seconde nature dans votre développement quotidien avec Delphi.

⏭️ [Déboggage de code multi-thread](/12-debogage-et-tests/09-deboggage-de-code-multi-thread.md)
