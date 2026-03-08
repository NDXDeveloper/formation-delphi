🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.9 Clean Code et principes SOLID

## Introduction

Imaginez entrer dans deux cuisines différentes :

**Cuisine A :** Tout est en désordre. Les couteaux sont mélangés avec les fourchettes, les épices sont dans tous les placards, vous ne trouvez rien. Cuisiner devient un cauchemar.

**Cuisine B :** Tout est à sa place. Les ustensiles sont organisés, les épices sont étiquetées et rangées logiquement, vous trouvez ce dont vous avez besoin en quelques secondes. Cuisiner est un plaisir.

Le code, c'est pareil. Le **Clean Code** (code propre), c'est l'art d'écrire du code aussi organisé que la cuisine B : clair, compréhensible, maintenable.

Les **principes SOLID** sont cinq règles fondamentales qui vous guident vers ce code propre. Inventés par Robert C. Martin (Uncle Bob), ils sont devenus la référence en programmation orientée objet.

### Pourquoi le Clean Code ?

**Citation célèbre :**
> "N'importe quel idiot peut écrire du code qu'un ordinateur comprend. Les bons programmeurs écrivent du code que les humains comprennent."
> — Martin Fowler

**La réalité du développement :**
- Le code est lu 10 fois plus souvent qu'il n'est écrit
- 80% du coût d'un logiciel vient de la maintenance
- Le code "sale" ralentit progressivement le développement
- La dette technique s'accumule exponentiellement

**Avec du Clean Code :**
- ✅ Développement plus rapide à long terme
- ✅ Moins de bugs
- ✅ Onboarding facile des nouveaux développeurs
- ✅ Refactoring sans risque
- ✅ Plaisir de coder

## Les fondamentaux du Clean Code

Avant d'aborder SOLID, maîtrisons les principes de base du code propre.

### 1. Nommage expressif

Le nom d'une variable, fonction ou classe doit révéler son intention.

**❌ Mauvais nommage :**
```pascal
var
  d: Integer;  // d ? Quoi ?
  x: string;   // x ?

procedure p(a: Integer);  // Que fait p ?  
begin  
  // ...
end;

type
  TMgr = class  // Manager ? Quoi ?
```

**✅ Bon nommage :**
```pascal
var
  DaysUntilExpiration: Integer;
  CustomerEmail: string;

procedure CalculateTotalWithDiscount(DiscountPercentage: Integer);  
begin  
  // ...
end;

type
  TCustomerManager = class
```

**Règles du nommage :**

1. **Noms prononçables et recherchables**
```pascal
// ❌ Mauvais
var
  yyyymmdstr: string;  // Impossible à prononcer

// ✅ Bon
var
  CurrentDate: TDateTime;
```

2. **Éviter les abréviations obscures**
```pascal
// ❌ Mauvais
function CalcDiscntPct(amt: Currency): Double;

// ✅ Bon
function CalculateDiscountPercentage(Amount: Currency): Double;
```

3. **Utiliser des noms de domaine métier**
```pascal
// ✅ Bon - Vocabulaire métier clair
type
  TInvoice = class
    procedure ApplyLateFee;
    function CalculateTaxableAmount: Currency;
  end;
```

### 2. Fonctions courtes et focalisées

Une fonction doit faire **une seule chose**, et la faire bien.

**❌ Fonction trop longue :**
```pascal
procedure ProcessOrder(OrderID: Integer);  
var  
  Order: TOrder;
  Customer: TCustomer;
  Total: Currency;
begin
  // Charger la commande (20 lignes)
  Order := LoadOrder(OrderID);

  // Valider la commande (30 lignes)
  if not Order.IsValid then
    raise Exception.Create('Invalid order');

  // Calculer le total (40 lignes)
  Total := 0;
  for Item in Order.Items do
    Total := Total + Item.Price * Item.Quantity;

  // Appliquer les remises (50 lignes)
  if Customer.IsPremium then
    Total := Total * 0.9;

  // Générer la facture (60 lignes)
  // ...

  // Envoyer l'email (40 lignes)
  // ...

  // Mettre à jour le stock (30 lignes)
  // ...
end; // 270 lignes !
```

**✅ Fonctions courtes et focalisées :**
```pascal
procedure ProcessOrder(OrderID: Integer);  
var  
  Order: TOrder;
begin
  Order := LoadOrder(OrderID);
  ValidateOrder(Order);

  CalculateOrderTotal(Order);
  GenerateInvoice(Order);
  SendConfirmationEmail(Order);
  UpdateInventory(Order);
end;

function LoadOrder(OrderID: Integer): TOrder;  
begin  
  // Uniquement charger la commande
  Result := FOrderRepository.GetByID(OrderID);
end;

procedure ValidateOrder(Order: TOrder);  
begin  
  // Uniquement valider
  if not Order.IsValid then
    raise EOrderValidationError.Create('Invalid order');
end;

// Etc...
```

**Règle d'or :** Si vous devez utiliser "et" pour décrire ce que fait la fonction, elle fait probablement trop de choses.

### 3. Pas d'effets de bord

Une fonction ne doit pas modifier des choses inattendues.

**❌ Effets de bord cachés :**
```pascal
function CalculateTotal(Order: TOrder): Currency;  
begin  
  Result := Order.SubTotal + Order.Tax;

  // ⚠️ Effet de bord : modification de l'état global
  GlobalOrderCount := GlobalOrderCount + 1;
  LastCalculatedOrder := Order;

  // ⚠️ Effet de bord : modification de l'ordre
  Order.CalculatedAt := Now;
end;
```

**✅ Fonction pure (sans effets de bord) :**
```pascal
function CalculateTotal(SubTotal, Tax: Currency): Currency;  
begin  
  Result := SubTotal + Tax;
  // Aucun effet de bord, juste un calcul
end;
```

### 4. Limiter les paramètres

Plus une fonction a de paramètres, plus elle est difficile à comprendre et tester.

**❌ Trop de paramètres :**
```pascal
procedure CreateCustomer(
  FirstName, LastName, Email, Phone, Street, City,
  PostalCode, Country: string;
  IsPremium: Boolean;
  BirthDate: TDateTime;
  ReferralCode: string
);
```

**✅ Utiliser un objet paramètre :**
```pascal
type
  TCustomerData = record
    FirstName: string;
    LastName: string;
    Email: string;
    Phone: string;
    Address: TAddress;
    IsPremium: Boolean;
    BirthDate: TDateTime;
    ReferralCode: string;
  end;

procedure CreateCustomer(const Data: TCustomerData);
```

**Règle :** Maximum 3-4 paramètres. Au-delà, utilisez un objet.

### 5. Pas de "code mort"

Supprimez le code inutilisé.

**❌ Code mort conservé "au cas où" :**
```pascal
procedure Process;  
begin  
  // DoSomething;  ← Code commenté "au cas où"
  // OldMethod;    ← Ancienne version conservée

  NewMethod;

  // if UseOldAlgorithm then  ← Condition jamais vraie
  //   OldAlgorithm
  // else
    NewAlgorithm;
end;
```

**✅ Code propre :**
```pascal
procedure Process;  
begin  
  NewMethod;
  NewAlgorithm;
end;
```

**Pourquoi :** Git conserve l'historique. Pas besoin de garder du code mort.

### 6. Commentaires utiles uniquement

**❌ Commentaires inutiles :**
```pascal
// Incrémenter i
Inc(i);

// Créer un client
Client := TClient.Create;

// Boucle sur les items
for Item in Items do  
begin  
  // Traiter l'item
  ProcessItem(Item);
end;
```

**✅ Code auto-documenté :**
```pascal
procedure ProcessAllItems;  
var  
  Item: TOrderItem;
begin
  for Item in FOrderItems do
    ProcessItem(Item);
end;
```

**Commentaires utiles :**
```pascal
// WORKAROUND: Bug dans FireDAC 10.4 avec MySQL 8.0
// Ticket: #12345
// TODO: Retirer quand corrigé dans la version 11.0
Connection.Params.Add('CharacterSet=utf8mb4');

// Algorithme de Luhn pour validation carte bancaire
// Voir: https://en.wikipedia.org/wiki/Luhn_algorithm
function ValidateCreditCard(const Number: string): Boolean;
```

### 7. Gestion d'erreurs propre

**❌ Codes d'erreur :**
```pascal
function SaveCustomer(Customer: TCustomer): Integer;  
begin  
  if not Customer.IsValid then
    Exit(ERROR_INVALID_DATA);

  if not DatabaseConnected then
    Exit(ERROR_NO_CONNECTION);

  // Sauvegarde...
  Result := SUCCESS;
end;

// Appelant doit vérifier les codes
Code := SaveCustomer(Customer);  
if Code = ERROR_INVALID_DATA then  
  ShowMessage('Données invalides')
else if Code = ERROR_NO_CONNECTION then
  ShowMessage('Pas de connexion');
```

**✅ Exceptions typées :**
```pascal
procedure SaveCustomer(Customer: TCustomer);  
begin  
  if not Customer.IsValid then
    raise ECustomerValidationError.Create('Invalid customer data');

  if not DatabaseConnected then
    raise EDatabaseConnectionError.Create('No database connection');

  // Sauvegarde...
end;

// Appelant gère les exceptions
try
  SaveCustomer(Customer);
  ShowMessage('Sauvegardé !');
except
  on E: ECustomerValidationError do
    ShowMessage('Erreur de validation : ' + E.Message);
  on E: EDatabaseConnectionError do
    ShowMessage('Erreur de connexion : ' + E.Message);
end;
```

## Les principes SOLID

SOLID est un acronyme pour cinq principes de conception orientée objet. Appliqués ensemble, ils produisent du code flexible, maintenable et évolutif.

```
S - Single Responsibility Principle (Principe de responsabilité unique)  
O - Open/Closed Principle (Principe ouvert/fermé)  
L - Liskov Substitution Principle (Principe de substitution de Liskov)  
I - Interface Segregation Principle (Principe de ségrégation des interfaces)  
D - Dependency Inversion Principle (Principe d'inversion des dépendances)  
```

### S - Single Responsibility Principle (SRP)

**Principe :** Une classe ne doit avoir qu'une seule raison de changer. Autrement dit, une classe = une responsabilité.

**Pourquoi :** Si une classe fait trop de choses, modifier une fonctionnalité risque de casser les autres.

**❌ Violation du SRP :**
```pascal
type
  TCustomer = class
  private
    FName: string;
    FEmail: string;
  public
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;

    // Responsabilité 1 : Données client

    // Responsabilité 2 : Validation
    function IsValid: Boolean;

    // Responsabilité 3 : Accès base de données
    procedure SaveToDatabase;
    procedure LoadFromDatabase(ID: Integer);

    // Responsabilité 4 : Envoi d'emails
    procedure SendWelcomeEmail;

    // Responsabilité 5 : Génération de rapports
    function GenerateReport: string;
  end;
```

**Problèmes :**
- Changer le format d'email → modifier TCustomer
- Changer la base de données → modifier TCustomer
- Changer le format de rapport → modifier TCustomer
- Impossible de tester indépendamment
- Classe gigantesque et complexe

**✅ Respect du SRP :**
```pascal
// Responsabilité unique : Représenter les données client
type
  TCustomer = class
  private
    FID: Integer;
    FName: string;
    FEmail: string;
  public
    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

// Responsabilité unique : Valider les clients
type
  TCustomerValidator = class
  public
    function IsValid(Customer: TCustomer; out ErrorMessage: string): Boolean;
  end;

// Responsabilité unique : Accès base de données
type
  TCustomerRepository = class
  public
    procedure Save(Customer: TCustomer);
    function GetByID(ID: Integer): TCustomer;
    procedure Delete(ID: Integer);
  end;

// Responsabilité unique : Envoi d'emails
type
  TCustomerEmailService = class
  public
    procedure SendWelcomeEmail(Customer: TCustomer);
    procedure SendPasswordResetEmail(Customer: TCustomer);
  end;

// Responsabilité unique : Génération de rapports
type
  TCustomerReportGenerator = class
  public
    function GenerateReport(Customer: TCustomer): string;
  end;
```

**Bénéfices :**
- Chaque classe a une raison claire d'exister
- Facile à tester
- Facile à maintenir
- Facile à réutiliser

**Test de conformité :**
> "Si je décris cette classe à quelqu'un et que j'utilise le mot 'et', elle viole probablement le SRP"

### O - Open/Closed Principle (OCP)

**Principe :** Les entités logicielles doivent être ouvertes à l'extension mais fermées à la modification.

Autrement dit : vous devez pouvoir ajouter des fonctionnalités sans modifier le code existant.

**❌ Violation de l'OCP :**
```pascal
type
  TPaymentType = (ptCreditCard, ptPayPal, ptBitcoin);

  TPaymentProcessor = class
  public
    procedure ProcessPayment(Amount: Currency; PaymentType: TPaymentType);
  end;

implementation

procedure TPaymentProcessor.ProcessPayment(Amount: Currency; PaymentType: TPaymentType);  
begin  
  case PaymentType of
    ptCreditCard:
      begin
        // Logique carte de crédit
        ConnectToCreditCardGateway;
        // ...
      end;
    ptPayPal:
      begin
        // Logique PayPal
        ConnectToPayPalAPI;
        // ...
      end;
    ptBitcoin:
      begin
        // Logique Bitcoin
        ConnectToBlockchain;
        // ...
      end;
  end;
end;
```

**Problèmes :**
- Pour ajouter un nouveau moyen de paiement (Apple Pay), je dois :
  1. Modifier l'enum TPaymentType
  2. Modifier la fonction ProcessPayment
  3. Recompiler tout
  4. Risquer de casser les paiements existants

**✅ Respect de l'OCP avec polymorphisme :**
```pascal
// Interface de base
type
  IPaymentMethod = interface
    ['{GUID}']
    procedure ProcessPayment(Amount: Currency);
    function GetName: string;
  end;

// Implémentation carte de crédit
type
  TCreditCardPayment = class(TInterfacedObject, IPaymentMethod)
  private
    FCardNumber: string;
    FExpiryDate: string;
  public
    constructor Create(const CardNumber, ExpiryDate: string);
    procedure ProcessPayment(Amount: Currency);
    function GetName: string;
  end;

implementation

constructor TCreditCardPayment.Create(const CardNumber, ExpiryDate: string);  
begin  
  inherited Create;
  FCardNumber := CardNumber;
  FExpiryDate := ExpiryDate;
end;

procedure TCreditCardPayment.ProcessPayment(Amount: Currency);  
begin  
  // Logique spécifique carte de crédit
  ConnectToCreditCardGateway;
  AuthorizePayment(FCardNumber, FExpiryDate, Amount);
  // ...
end;

function TCreditCardPayment.GetName: string;  
begin  
  Result := 'Credit Card';
end;

// Implémentation PayPal
type
  TPayPalPayment = class(TInterfacedObject, IPaymentMethod)
  private
    FEmail: string;
  public
    constructor Create(const Email: string);
    procedure ProcessPayment(Amount: Currency);
    function GetName: string;
  end;

implementation

constructor TPayPalPayment.Create(const Email: string);  
begin  
  inherited Create;
  FEmail := Email;
end;

procedure TPayPalPayment.ProcessPayment(Amount: Currency);  
begin  
  // Logique spécifique PayPal
  ConnectToPayPalAPI;
  AuthorizePayment(FEmail, Amount);
  // ...
end;

function TPayPalPayment.GetName: string;  
begin  
  Result := 'PayPal';
end;

// Processeur de paiement (fermé à la modification)
type
  TPaymentProcessor = class
  public
    procedure ProcessPayment(PaymentMethod: IPaymentMethod; Amount: Currency);
  end;

implementation

procedure TPaymentProcessor.ProcessPayment(PaymentMethod: IPaymentMethod; Amount: Currency);  
begin  
  // Même code pour tous les moyens de paiement
  WriteLn('Processing payment with: ' + PaymentMethod.GetName);
  PaymentMethod.ProcessPayment(Amount);
  WriteLn('Payment processed successfully');
end;
```

**Pour ajouter Bitcoin maintenant :**
```pascal
// Nouvelle classe, AUCUNE modification du code existant !
type
  TBitcoinPayment = class(TInterfacedObject, IPaymentMethod)
  private
    FWalletAddress: string;
  public
    constructor Create(const WalletAddress: string);
    procedure ProcessPayment(Amount: Currency);
    function GetName: string;
  end;

implementation

constructor TBitcoinPayment.Create(const WalletAddress: string);  
begin  
  inherited Create;
  FWalletAddress := WalletAddress;
end;

procedure TBitcoinPayment.ProcessPayment(Amount: Currency);  
begin  
  ConnectToBlockchain;
  SendBitcoin(FWalletAddress, Amount);
end;

function TBitcoinPayment.GetName: string;  
begin  
  Result := 'Bitcoin';
end;
```

**Utilisation :**
```pascal
var
  Processor: TPaymentProcessor;
  Payment: IPaymentMethod;
begin
  Processor := TPaymentProcessor.Create;
  try
    // Paiement carte
    Payment := TCreditCardPayment.Create('1234-5678-9012-3456', '12/25');
    Processor.ProcessPayment(Payment, 100.00);

    // Paiement PayPal
    Payment := TPayPalPayment.Create('user@example.com');
    Processor.ProcessPayment(Payment, 50.00);

    // Paiement Bitcoin (nouveau, sans toucher au code existant !)
    Payment := TBitcoinPayment.Create('1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa');
    Processor.ProcessPayment(Payment, 75.00);
  finally
    Processor.Free;
  end;
end;
```

**Bénéfices :**
- Ajouter des fonctionnalités sans risque
- Code existant non touché = pas de régression
- Tests existants toujours valides

### L - Liskov Substitution Principle (LSP)

**Principe :** Les objets d'une classe dérivée doivent pouvoir remplacer les objets de la classe de base sans altérer le bon fonctionnement du programme.

Autrement dit : si B hérite de A, je dois pouvoir utiliser B partout où j'utilise A.

**❌ Violation du LSP :**
```pascal
type
  TRectangle = class
  protected
    FWidth: Integer;
    FHeight: Integer;
  public
    procedure SetWidth(Value: Integer); virtual;
    procedure SetHeight(Value: Integer); virtual;
    function GetArea: Integer;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TSquare = class(TRectangle)
  public
    procedure SetWidth(Value: Integer); override;
    procedure SetHeight(Value: Integer); override;
  end;

implementation

// Rectangle
procedure TRectangle.SetWidth(Value: Integer);  
begin  
  FWidth := Value;
end;

procedure TRectangle.SetHeight(Value: Integer);  
begin  
  FHeight := Value;
end;

function TRectangle.GetArea: Integer;  
begin  
  Result := FWidth * FHeight;
end;

// Carré : largeur et hauteur doivent être égales
procedure TSquare.SetWidth(Value: Integer);  
begin  
  FWidth := Value;
  FHeight := Value;  // ⚠️ Effet de bord inattendu
end;

procedure TSquare.SetHeight(Value: Integer);  
begin  
  FWidth := Value;   // ⚠️ Effet de bord inattendu
  FHeight := Value;
end;
```

**Pourquoi c'est un problème :**
```pascal
procedure TestRectangle(R: TRectangle);  
begin  
  R.Width := 5;
  R.Height := 10;

  // Pour un rectangle, on s'attend à Area = 50
  Assert(R.GetArea = 50);  // ✅ OK avec TRectangle
                           // ❌ ERREUR avec TSquare (Area = 100)
end;

var
  Rect: TRectangle;
  Square: TSquare;
begin
  Rect := TRectangle.Create;
  TestRectangle(Rect);  // OK
  Rect.Free;

  Square := TSquare.Create;
  TestRectangle(Square);  // ERREUR ! Violation du LSP
  Square.Free;
end;
```

**✅ Respect du LSP - Solution 1 : Pas d'héritage**
```pascal
type
  TShape = class
  public
    function GetArea: Integer; virtual; abstract;
  end;

  TRectangle = class(TShape)
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(Width, Height: Integer);
    function GetArea: Integer; override;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TSquare = class(TShape)
  private
    FSide: Integer;
  public
    constructor Create(Side: Integer);
    function GetArea: Integer; override;
    property Side: Integer read FSide;
  end;

implementation

constructor TRectangle.Create(Width, Height: Integer);  
begin  
  inherited Create;
  FWidth := Width;
  FHeight := Height;
end;

function TRectangle.GetArea: Integer;  
begin  
  Result := FWidth * FHeight;
end;

constructor TSquare.Create(Side: Integer);  
begin  
  inherited Create;
  FSide := Side;
end;

function TSquare.GetArea: Integer;  
begin  
  Result := FSide * FSide;
end;
```

**Autre exemple de violation :**
```pascal
type
  TBird = class
  public
    procedure Fly; virtual;
  end;

  TPenguin = class(TBird)
  public
    procedure Fly; override;  // ⚠️ Problème : les pingouins ne volent pas !
  end;

implementation

procedure TPenguin.Fly;  
begin  
  raise Exception.Create('Penguins cannot fly!');
end;
```

**Solution :**
```pascal
type
  TBird = class
  public
    procedure Eat; virtual;
  end;

  IFlyable = interface
    ['{GUID}']
    procedure Fly;
  end;

  TSparrow = class(TBird, IFlyable)
  public
    procedure Fly;
  end;

  TPenguin = class(TBird)
    // Ne peut pas voler, n'implémente pas IFlyable
  end;
```

**Règle pratique :** Si vous devez lever une exception dans une méthode héritée, vous violez probablement le LSP.

### I - Interface Segregation Principle (ISP)

**Principe :** Les clients ne doivent pas dépendre d'interfaces qu'ils n'utilisent pas. Mieux vaut plusieurs interfaces spécifiques qu'une interface générale.

**❌ Violation de l'ISP - Interface trop grosse :**
```pascal
type
  IWorker = interface
    ['{GUID}']
    procedure Work;
    procedure Eat;
    procedure Sleep;
    procedure GetPaid;
    procedure AttendMeeting;
    procedure TakeVacation;
  end;

// Problème : Un robot doit implémenter IWorker
type
  TRobotWorker = class(TInterfacedObject, IWorker)
  public
    procedure Work;
    procedure Eat;       // ⚠️ Un robot ne mange pas
    procedure Sleep;     // ⚠️ Un robot ne dort pas
    procedure GetPaid;   // ⚠️ Un robot n'est pas payé
    procedure AttendMeeting;  // ⚠️ Un robot ne va pas en réunion
    procedure TakeVacation;   // ⚠️ Un robot ne prend pas de vacances
  end;

implementation

procedure TRobotWorker.Work;  
begin  
  // OK
end;

procedure TRobotWorker.Eat;  
begin  
  raise Exception.Create('Robots do not eat!');
end;

// Etc...
```

**✅ Respect de l'ISP - Interfaces ségrégées :**
```pascal
type
  // Interface de base
  IWorkable = interface
    ['{GUID-1}']
    procedure Work;
  end;

  // Interfaces spécialisées
  IFeedable = interface
    ['{GUID-2}']
    procedure Eat;
  end;

  IRestable = interface
    ['{GUID-3}']
    procedure Sleep;
  end;

  IPayable = interface
    ['{GUID-4}']
    procedure GetPaid;
  end;

  ISocialWorker = interface
    ['{GUID-5}']
    procedure AttendMeeting;
    procedure TakeVacation;
  end;

// Robot : seulement IWorkable
type
  TRobotWorker = class(TInterfacedObject, IWorkable)
  public
    procedure Work;
  end;

// Humain : toutes les interfaces
type
  THumanWorker = class(TInterfacedObject, IWorkable, IFeedable, IRestable,
                       IPayable, ISocialWorker)
  public
    procedure Work;
    procedure Eat;
    procedure Sleep;
    procedure GetPaid;
    procedure AttendMeeting;
    procedure TakeVacation;
  end;

// Stagiaire : travaille et mange, mais pas payé
type
  TIntern = class(TInterfacedObject, IWorkable, IFeedable, IRestable)
  public
    procedure Work;
    procedure Eat;
    procedure Sleep;
  end;
```

**Utilisation :**
```pascal
procedure ManageWorkers(Workers: TArray<IWorkable>);  
var  
  Worker: IWorkable;
begin
  for Worker in Workers do
    Worker.Work;
end;

procedure PayDay(Workers: TArray<IPayable>);  
var  
  Worker: IPayable;
begin
  for Worker in Workers do
    Worker.GetPaid;
end;

var
  Workers: TArray<IWorkable>;
  PayableWorkers: TArray<IPayable>;
begin
  Workers := [
    THumanWorker.Create,
    TRobotWorker.Create,
    TIntern.Create
  ];

  ManageWorkers(Workers);  // Tous travaillent

  PayableWorkers := [
    THumanWorker.Create
    // Pas le robot ni le stagiaire
  ];

  PayDay(PayableWorkers);  // Seulement les humains payés
end;
```

**Bénéfices :**
- Pas d'implémentations vides ou qui lèvent des exceptions
- Contrats clairs et focalisés
- Flexibilité maximale

### D - Dependency Inversion Principle (DIP)

**Principe :**
1. Les modules de haut niveau ne doivent pas dépendre des modules de bas niveau. Les deux doivent dépendre d'abstractions.
2. Les abstractions ne doivent pas dépendre des détails. Les détails doivent dépendre des abstractions.

Autrement dit : **programmez vers des interfaces, pas vers des implémentations concrètes**.

**❌ Violation du DIP - Dépendance directe :**
```pascal
// Classe de bas niveau
type
  TMySQLDatabase = class
  public
    procedure Connect;
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Classe de haut niveau dépend directement de TMySQLDatabase
type
  TCustomerService = class
  private
    FDatabase: TMySQLDatabase;  // ⚠️ Dépendance directe
  public
    constructor Create;
    destructor Destroy; override;
    function GetCustomer(ID: Integer): TCustomer;
  end;

implementation

constructor TCustomerService.Create;  
begin  
  inherited;
  FDatabase := TMySQLDatabase.Create;  // ⚠️ Couplage fort
end;

destructor TCustomerService.Destroy;  
begin  
  FDatabase.Free;
  inherited;
end;

function TCustomerService.GetCustomer(ID: Integer): TCustomer;  
var  
  DS: TDataSet;
begin
  DS := FDatabase.ExecuteQuery('SELECT * FROM customers WHERE id = ' + IntToStr(ID));
  // ...
end;
```

**Problèmes :**
- Impossible de changer de base de données sans modifier TCustomerService
- Impossible de tester TCustomerService sans vraie base de données
- Couplage fort entre les couches

**✅ Respect du DIP - Inversion de dépendance :**
```pascal
// Abstraction (interface)
type
  IDatabase = interface
    ['{GUID}']
    procedure Connect;
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Implémentation MySQL
type
  TMySQLDatabase = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Implémentation PostgreSQL
type
  TPostgreSQLDatabase = class(TInterfacedObject, IDatabase)
  public
    procedure Connect;
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Implémentation Mock pour tests
type
  TMockDatabase = class(TInterfacedObject, IDatabase)
  private
    FTestData: TDataSet;
  public
    constructor Create(TestData: TDataSet);
    procedure Connect;
    function ExecuteQuery(const SQL: string): TDataSet;
  end;

// Classe de haut niveau dépend de l'abstraction
type
  TCustomerService = class
  private
    FDatabase: IDatabase;  // ✅ Dépend de l'interface
  public
    constructor Create(Database: IDatabase);  // ✅ Injection de dépendance
    function GetCustomer(ID: Integer): TCustomer;
  end;

implementation

constructor TCustomerService.Create(Database: IDatabase);  
begin  
  inherited Create;
  FDatabase := Database;  // ✅ La dépendance est injectée
end;

function TCustomerService.GetCustomer(ID: Integer): TCustomer;  
var  
  DS: TDataSet;
begin
  DS := FDatabase.ExecuteQuery('SELECT * FROM customers WHERE id = ' + IntToStr(ID));
  // Conversion DataSet → TCustomer
  Result := ConvertToCustomer(DS);
end;
```

**Utilisation :**
```pascal
var
  Database: IDatabase;
  Service: TCustomerService;
begin
  // Production : MySQL
  Database := TMySQLDatabase.Create;
  Service := TCustomerService.Create(Database);

  // Ou PostgreSQL (même code)
  Database := TPostgreSQLDatabase.Create;
  Service := TCustomerService.Create(Database);

  // Tests : Mock
  Database := TMockDatabase.Create(GetTestData);
  Service := TCustomerService.Create(Database);
end;
```

**Bénéfices :**
- Changement de base facile
- Tests unitaires simples
- Découplage des couches
- Flexibilité maximale

## Application pratique : Refactoring vers SOLID

Prenons un exemple complet de refactoring.

**Code initial (viole tous les principes) :**
```pascal
type
  TOrderProcessor = class
  private
    FConnection: TFDConnection;
  public
    constructor Create;
    destructor Destroy; override;
    function ProcessOrder(OrderID: Integer): Boolean;
  end;

implementation

constructor TOrderProcessor.Create;  
begin  
  inherited;
  FConnection := TFDConnection.Create(nil);
  FConnection.Params.Values['Server'] := 'localhost';
  FConnection.Params.Values['Database'] := 'orders';
  FConnection.Connected := True;
end;

destructor TOrderProcessor.Destroy;  
begin  
  FConnection.Free;
  inherited;
end;

function TOrderProcessor.ProcessOrder(OrderID: Integer): Boolean;  
var  
  Query: TFDQuery;
  Total, Discount: Currency;
  CustomerType: string;
begin
  Result := False;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Charger la commande
    Query.SQL.Text := 'SELECT * FROM orders WHERE id = ' + IntToStr(OrderID);
    Query.Open;

    if Query.IsEmpty then
      Exit;

    // Calculer le total
    Total := Query.FieldByName('total').AsCurrency;
    CustomerType := Query.FieldByName('customer_type').AsString;

    // Appliquer remise selon type client
    if CustomerType = 'premium' then
      Discount := Total * 0.10
    else if CustomerType = 'vip' then
      Discount := Total * 0.20
    else
      Discount := 0;

    Total := Total - Discount;

    // Mettre à jour
    Query.Close;
    Query.SQL.Text := 'UPDATE orders SET final_total = ' + FloatToStr(Total) +
                      ' WHERE id = ' + IntToStr(OrderID);
    Query.ExecSQL;

    // Envoyer email
    if CustomerType = 'premium' then
    begin
      // Logique email premium
    end
    else if CustomerType = 'vip' then
    begin
      // Logique email VIP
    end;

    Result := True;
  finally
    Query.Free;
  end;
end;
```

**Problèmes :**
- ❌ Viole SRP : fait trop de choses
- ❌ Viole OCP : pour ajouter un type client, modifier le code
- ❌ Viole ISP : pas d'interfaces
- ❌ Viole DIP : dépendance directe à FDConnection
- ❌ Code difficile à tester
- ❌ Injection SQL possible

**Code refactorisé avec SOLID :**

```pascal
// ============= ABSTRACTIONS (D - DIP) =============

type
  IOrderRepository = interface
    ['{GUID-1}']
    function GetOrder(ID: Integer): TOrder;
    procedure UpdateOrder(Order: TOrder);
  end;

  IDiscountStrategy = interface
    ['{GUID-2}']
    function CalculateDiscount(Total: Currency): Currency;
    function GetCustomerType: string;
  end;

  INotificationService = interface
    ['{GUID-3}']
    procedure SendOrderConfirmation(Order: TOrder);
  end;

// ============= MODÈLES (S - SRP) =============

type
  TOrder = class
  private
    FID: Integer;
    FTotal: Currency;
    FFinalTotal: Currency;
    FCustomerType: string;
  public
    property ID: Integer read FID write FID;
    property Total: Currency read FTotal write FTotal;
    property FinalTotal: Currency read FFinalTotal write FFinalTotal;
    property CustomerType: string read FCustomerType write FCustomerType;
  end;

// ============= STRATÉGIES DE REMISE (O - OCP) =============

type
  TStandardDiscount = class(TInterfacedObject, IDiscountStrategy)
  public
    function CalculateDiscount(Total: Currency): Currency;
    function GetCustomerType: string;
  end;

  TPremiumDiscount = class(TInterfacedObject, IDiscountStrategy)
  public
    function CalculateDiscount(Total: Currency): Currency;
    function GetCustomerType: string;
  end;

  TVIPDiscount = class(TInterfacedObject, IDiscountStrategy)
  public
    function CalculateDiscount(Total: Currency): Currency;
    function GetCustomerType: string;
  end;

implementation

function TStandardDiscount.CalculateDiscount(Total: Currency): Currency;  
begin  
  Result := 0;  // Pas de remise
end;

function TStandardDiscount.GetCustomerType: string;  
begin  
  Result := 'standard';
end;

function TPremiumDiscount.CalculateDiscount(Total: Currency): Currency;  
begin  
  Result := Total * 0.10;  // 10% de remise
end;

function TPremiumDiscount.GetCustomerType: string;  
begin  
  Result := 'premium';
end;

function TVIPDiscount.CalculateDiscount(Total: Currency): Currency;  
begin  
  Result := Total * 0.20;  // 20% de remise
end;

function TVIPDiscount.GetCustomerType: string;  
begin  
  Result := 'vip';
end;

// ============= REPOSITORY (S - SRP, D - DIP) =============

type
  TOrderRepository = class(TInterfacedObject, IOrderRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(Connection: TFDConnection);
    function GetOrder(ID: Integer): TOrder;
    procedure UpdateOrder(Order: TOrder);
  end;

implementation

constructor TOrderRepository.Create(Connection: TFDConnection);  
begin  
  inherited Create;
  FConnection := Connection;
end;

function TOrderRepository.GetOrder(ID: Integer): TOrder;  
var  
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM orders WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Result := TOrder.Create;
      Result.ID := Query.FieldByName('id').AsInteger;
      Result.Total := Query.FieldByName('total').AsCurrency;
      Result.CustomerType := Query.FieldByName('customer_type').AsString;
    end;
  finally
    Query.Free;
  end;
end;

procedure TOrderRepository.UpdateOrder(Order: TOrder);  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE orders SET final_total = :total WHERE id = :id';
    Query.ParamByName('total').AsCurrency := Order.FinalTotal;
    Query.ParamByName('id').AsInteger := Order.ID;
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

// ============= SERVICE DE NOTIFICATION (S - SRP) =============

type
  TEmailNotificationService = class(TInterfacedObject, INotificationService)
  public
    procedure SendOrderConfirmation(Order: TOrder);
  end;

implementation

procedure TEmailNotificationService.SendOrderConfirmation(Order: TOrder);  
begin  
  // Logique d'envoi d'email
  WriteLn('Sending confirmation email for order #', Order.ID);
end;

// ============= PROCESSEUR (S - SRP, O - OCP, D - DIP) =============

type
  TOrderProcessor = class
  private
    FRepository: IOrderRepository;
    FNotificationService: INotificationService;
    FDiscountStrategies: TDictionary<string, IDiscountStrategy>;
  public
    constructor Create(
      Repository: IOrderRepository;
      NotificationService: INotificationService
    );
    destructor Destroy; override;
    procedure RegisterDiscountStrategy(Strategy: IDiscountStrategy);
    function ProcessOrder(OrderID: Integer): Boolean;
  end;

implementation

constructor TOrderProcessor.Create(
  Repository: IOrderRepository;
  NotificationService: INotificationService
);
begin
  inherited Create;
  FRepository := Repository;
  FNotificationService := NotificationService;
  FDiscountStrategies := TDictionary<string, IDiscountStrategy>.Create;
end;

destructor TOrderProcessor.Destroy;  
begin  
  FDiscountStrategies.Free;
  inherited;
end;

procedure TOrderProcessor.RegisterDiscountStrategy(Strategy: IDiscountStrategy);  
begin  
  FDiscountStrategies.Add(Strategy.GetCustomerType, Strategy);
end;

function TOrderProcessor.ProcessOrder(OrderID: Integer): Boolean;  
var  
  Order: TOrder;
  Strategy: IDiscountStrategy;
  Discount: Currency;
begin
  Result := False;

  // Charger la commande
  Order := FRepository.GetOrder(OrderID);
  if Order = nil then
    Exit;

  try
    // Appliquer la stratégie de remise appropriée
    if FDiscountStrategies.TryGetValue(Order.CustomerType, Strategy) then
    begin
      Discount := Strategy.CalculateDiscount(Order.Total);
      Order.FinalTotal := Order.Total - Discount;
    end
    else
      Order.FinalTotal := Order.Total;

    // Sauvegarder
    FRepository.UpdateOrder(Order);

    // Notifier
    FNotificationService.SendOrderConfirmation(Order);

    Result := True;
  finally
    Order.Free;
  end;
end;

// ============= UTILISATION =============

var
  Connection: TFDConnection;
  Repository: IOrderRepository;
  NotificationService: INotificationService;
  Processor: TOrderProcessor;
begin
  // Configuration
  Connection := TFDConnection.Create(nil);
  // ... configurer la connexion

  Repository := TOrderRepository.Create(Connection);
  NotificationService := TEmailNotificationService.Create;

  Processor := TOrderProcessor.Create(Repository, NotificationService);
  try
    // Enregistrer les stratégies de remise (O - OCP)
    Processor.RegisterDiscountStrategy(TStandardDiscount.Create);
    Processor.RegisterDiscountStrategy(TPremiumDiscount.Create);
    Processor.RegisterDiscountStrategy(TVIPDiscount.Create);

    // Pour ajouter une nouvelle stratégie, aucune modification nécessaire !
    // Processor.RegisterDiscountStrategy(TGoldDiscount.Create);

    // Traiter une commande
    if Processor.ProcessOrder(123) then
      WriteLn('Order processed successfully');
  finally
    Processor.Free;
    Connection.Free;
  end;
end;
```

**Bénéfices du refactoring :**

✅ **SRP** : Chaque classe a une responsabilité unique
- TOrder : données
- TOrderRepository : accès base
- IDiscountStrategy : calcul de remise
- TEmailNotificationService : notifications
- TOrderProcessor : orchestration

✅ **OCP** : Pour ajouter un type de client, créez une nouvelle stratégie
```pascal
type
  TGoldDiscount = class(TInterfacedObject, IDiscountStrategy)
  public
    function CalculateDiscount(Total: Currency): Currency;
    function GetCustomerType: string;
  end;

// Pas besoin de modifier le code existant !
```

✅ **LSP** : Toutes les stratégies sont interchangeables

✅ **ISP** : Interfaces focalisées (IOrderRepository, IDiscountStrategy, INotificationService)

✅ **DIP** : TOrderProcessor dépend d'interfaces, pas d'implémentations

✅ **Testable** :
```pascal
// Mock pour les tests
type
  TMockRepository = class(TInterfacedObject, IOrderRepository)
  private
    FTestOrder: TOrder;
  public
    constructor Create(TestOrder: TOrder);
    function GetOrder(ID: Integer): TOrder;
    procedure UpdateOrder(Order: TOrder);
  end;

// Test
procedure TestOrderProcessing;  
var  
  TestOrder: TOrder;
  Repository: IOrderRepository;
  Notification: INotificationService;
  Processor: TOrderProcessor;
begin
  TestOrder := TOrder.Create;
  TestOrder.ID := 1;
  TestOrder.Total := 1000;
  TestOrder.CustomerType := 'premium';

  Repository := TMockRepository.Create(TestOrder);
  Notification := TMockNotificationService.Create;

  Processor := TOrderProcessor.Create(Repository, Notification);
  try
    Processor.RegisterDiscountStrategy(TPremiumDiscount.Create);
    Assert(Processor.ProcessOrder(1));
  finally
    Processor.Free;
    TestOrder.Free;
  end;
end;
```

## Checklist Clean Code et SOLID

Avant de considérer votre code comme "propre", vérifiez :

### Clean Code
- [ ] Les noms sont expressifs et révèlent l'intention
- [ ] Les fonctions sont courtes (< 20 lignes idéalement)
- [ ] Chaque fonction fait une seule chose
- [ ] Pas d'effets de bord cachés
- [ ] Maximum 3-4 paramètres par fonction
- [ ] Pas de code mort ou commenté
- [ ] Commentaires uniquement pour expliquer le "pourquoi"
- [ ] Gestion d'erreurs avec exceptions typées
- [ ] Pas de nombres magiques (remplacés par des constantes)
- [ ] Code formaté de manière cohérente

### SOLID
- [ ] **S**: Chaque classe a une seule responsabilité
- [ ] **O**: Nouvelles fonctionnalités par extension, pas modification
- [ ] **L**: Les classes dérivées sont substituables à leur classe de base
- [ ] **I**: Interfaces petites et focalisées
- [ ] **D**: Dépendances vers abstractions, pas implémentations

### Tests
- [ ] Code testable (pas de dépendances hardcodées)
- [ ] Tests unitaires pour la logique métier
- [ ] Coverage > 70%

## Conclusion

Le Clean Code et les principes SOLID ne sont pas des règles rigides, mais des guides pour écrire du code de qualité professionnelle.

**Points clés à retenir :**

1. **Clean Code = Code lisible et maintenable**
   - Nommage expressif
   - Fonctions courtes
   - Pas d'effets de bord

2. **SOLID = 5 principes pour un code flexible**
   - **S**: Une classe = une responsabilité
   - **O**: Ouvert à l'extension, fermé à la modification
   - **L**: Substituabilité des classes dérivées
   - **I**: Interfaces petites et focalisées
   - **D**: Dépendance vers les abstractions

3. **Les bénéfices sont réels**
   - Développement plus rapide à long terme
   - Moins de bugs
   - Code plus facile à comprendre et modifier
   - Tests plus simples

4. **Apprentissage progressif**
   - Commencez par le nommage et les fonctions courtes
   - Puis maîtrisez SRP
   - Progressivement, appliquez les autres principes

5. **Pragmatisme avant purisme**
   - Tous les principes ne s'appliquent pas partout
   - Trouvez le bon équilibre pour votre contexte
   - "Perfect is the enemy of good"

**Citation finale :**

> "Le seul moyen d'aller vite, c'est d'aller bien"
> — Robert C. Martin (Uncle Bob)

Le temps investi dans le Clean Code et SOLID est du temps gagné sur le long terme. Un code propre est un plaisir à maintenir. Un code sale devient rapidement un cauchemar.

**Commencez aujourd'hui :**
- Choisissez une fonction et refactorisez-la
- Appliquez SRP à une classe trop grosse
- Extrayez une interface pour découpler vos dépendances
- Relisez votre code comme si c'était celui d'un autre

Chaque petit pas vers le Clean Code est un pas vers un meilleur code, une meilleure application, et une meilleure expérience de développement.

---

**Ressources pour aller plus loin :**

- **Livre**: "Clean Code" de Robert C. Martin
- **Livre**: "Clean Architecture" de Robert C. Martin
- **Livre**: "Design Patterns" du Gang of Four
- **Site**: https://refactoring.guru (patterns et refactoring)
- **Communauté**: Stack Overflow, Reddit r/delphi

Bon coding propre ! 🎯

⏭️ [Domain-Driven Design (DDD) avec Delphi](/18-architecture-et-bonnes-pratiques/10-domain-driven-design-avec-delphi.md)
