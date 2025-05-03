# 18.9 Clean Code et principes SOLID

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Avez-vous déjà eu à travailler sur un code que vous avez écrit il y a six mois et vous êtes demandé : "Qui a écrit ce code confus et pourquoi ?" Ou peut-être avez-vous rejoint un projet existant et passé des jours à comprendre comment il fonctionne avant de pouvoir faire la moindre modification ? Ces situations illustrent parfaitement pourquoi le "Clean Code" (code propre) et les principes SOLID sont si importants.

Dans ce chapitre, nous explorerons comment écrire du code Delphi plus propre, plus maintenable et plus robuste. Que vous soyez un développeur débutant ou expérimenté, ces principes vous aideront à améliorer la qualité de votre code et à faciliter sa maintenance sur le long terme.

## Qu'est-ce que le Clean Code ?

Le Clean Code est une approche de programmation qui met l'accent sur la lisibilité et la maintenabilité du code. Comme l'a dit Robert C. Martin (Uncle Bob), auteur du livre "Clean Code" :

> *"Le code propre se lit comme une prose bien écrite."*

### Caractéristiques du code propre

Un code propre est :

1. **Lisible** : Facile à lire et à comprendre
2. **Simple** : Fait une chose et la fait bien
3. **Direct** : Résout le problème sans complexité inutile
4. **Expressif** : Les noms des variables, méthodes et classes révèlent leur intention
5. **Concis** : Ne contient pas de code inutile ou dupliqué
6. **Bien testé** : Couvert par des tests unitaires
7. **Bien organisé** : Structure cohérente et logique

## Principes du Clean Code en Delphi

### 1. Nommage significatif

Les noms des variables, méthodes et classes doivent clairement indiquer leur rôle ou responsabilité.

**Mauvais exemple :**
```pascal
var
  s: string;
  i: Integer;
  b: Boolean;

procedure P1;
```

**Bon exemple :**
```pascal
var
  ClientName: string;
  ProductCount: Integer;
  IsEligibleForDiscount: Boolean;

procedure ProcessClientOrder;
```

#### Conventions de nommage en Delphi

Delphi a ses propres conventions de nommage :

- **Unités** : Commencent généralement par 'U' (UCustomer) ou suivent une structure fonctionnelle (CustomerServices)
- **Classes** : Commencent généralement par 'T' (TCustomer)
- **Interfaces** : Commencent généralement par 'I' (ICustomerRepository)
- **Variables membres** : Souvent préfixées par 'F' (FCustomerName)
- **Paramètres** : Souvent préfixés par 'A' (ACustomerName)

### 2. Fonctions et méthodes courtes

Une méthode ne devrait faire qu'une seule chose et la faire bien. Idéalement, une méthode ne devrait pas dépasser 20-30 lignes.

**Mauvais exemple :**
```pascal
procedure TOrderProcessor.ProcessOrder(AOrder: TOrder);
var
  Customer: TCustomer;
  Product: TProduct;
  i: Integer;
  TotalAmount: Currency;
  InvoiceNumber: string;
begin
  // 1. Validation de la commande
  if AOrder = nil then
    raise EOrderException.Create('Order cannot be nil');
  if AOrder.Items.Count = 0 then
    raise EOrderException.Create('Order must have at least one item');

  // 2. Récupération du client
  Customer := CustomerRepository.FindById(AOrder.CustomerId);
  if Customer = nil then
    raise EOrderException.Create('Customer not found');

  // 3. Calcul du montant total
  TotalAmount := 0;
  for i := 0 to AOrder.Items.Count - 1 do
  begin
    Product := ProductRepository.FindById(AOrder.Items[i].ProductId);
    if Product = nil then
      raise EOrderException.Create('Product not found');
    TotalAmount := TotalAmount + (Product.Price * AOrder.Items[i].Quantity);
  end;

  // 4. Application de la remise
  if Customer.IsVIP then
    TotalAmount := TotalAmount * 0.9; // 10% de remise

  // 5. Création de la facture
  InvoiceNumber := 'INV-' + FormatDateTime('yyyymmddhhnnss', Now);
  Database.ExecuteSQL('INSERT INTO Invoices (Number, CustomerId, Amount, Date) VALUES (?, ?, ?, ?)',
    [InvoiceNumber, Customer.Id, TotalAmount, Now]);

  // 6. Mise à jour du stock
  for i := 0 to AOrder.Items.Count - 1 do
  begin
    Product := ProductRepository.FindById(AOrder.Items[i].ProductId);
    Product.Stock := Product.Stock - AOrder.Items[i].Quantity;
    ProductRepository.Update(Product);
  end;

  // 7. Envoi d'email de confirmation
  EmailService.SendOrderConfirmation(Customer.Email, InvoiceNumber, TotalAmount);
end;
```

**Bon exemple :**
```pascal
procedure TOrderProcessor.ProcessOrder(AOrder: TOrder);
begin
  ValidateOrder(AOrder);

  var Customer := GetCustomer(AOrder.CustomerId);
  var TotalAmount := CalculateOrderTotal(AOrder);

  if Customer.IsVIP then
    TotalAmount := ApplyVIPDiscount(TotalAmount);

  var InvoiceNumber := CreateInvoice(Customer, TotalAmount);
  UpdateInventory(AOrder);
  SendConfirmationEmail(Customer, InvoiceNumber, TotalAmount);
end;

procedure TOrderProcessor.ValidateOrder(AOrder: TOrder);
begin
  if AOrder = nil then
    raise EOrderException.Create('Order cannot be nil');
  if AOrder.Items.Count = 0 then
    raise EOrderException.Create('Order must have at least one item');
end;

function TOrderProcessor.GetCustomer(ACustomerId: Integer): TCustomer;
begin
  Result := CustomerRepository.FindById(ACustomerId);
  if Result = nil then
    raise EOrderException.Create('Customer not found');
end;

// ... autres méthodes extraites ...
```

### 3. DRY (Don't Repeat Yourself)

Évitez la duplication de code. Si vous écrivez le même code à plusieurs endroits, extrayez-le dans une méthode réutilisable.

**Mauvais exemple :**
```pascal
procedure TReportGenerator.GeneratePDFReport(AReportData: TReportData);
begin
  var PDFDoc := TPDFDocument.Create;
  try
    PDFDoc.Title := 'Monthly Report';
    PDFDoc.Author := 'System';
    PDFDoc.CreationDate := Now;

    // Configuration du PDF...
    PDFDoc.DefaultPageSetup.Margins.Left := 20;
    PDFDoc.DefaultPageSetup.Margins.Right := 20;
    PDFDoc.DefaultPageSetup.Margins.Top := 30;
    PDFDoc.DefaultPageSetup.Margins.Bottom := 30;

    // Génération du contenu...

    PDFDoc.SaveToFile('report.pdf');
  finally
    PDFDoc.Free;
  end;
end;

procedure TReportGenerator.GenerateInvoicePDF(AInvoice: TInvoice);
begin
  var PDFDoc := TPDFDocument.Create;
  try
    PDFDoc.Title := 'Invoice #' + AInvoice.Number;
    PDFDoc.Author := 'System';
    PDFDoc.CreationDate := Now;

    // Configuration du PDF... (dupliquée)
    PDFDoc.DefaultPageSetup.Margins.Left := 20;
    PDFDoc.DefaultPageSetup.Margins.Right := 20;
    PDFDoc.DefaultPageSetup.Margins.Top := 30;
    PDFDoc.DefaultPageSetup.Margins.Bottom := 30;

    // Génération du contenu de la facture...

    PDFDoc.SaveToFile('invoice_' + AInvoice.Number + '.pdf');
  finally
    PDFDoc.Free;
  end;
end;
```

**Bon exemple :**
```pascal
function TReportGenerator.CreateConfiguredPDFDocument(const ATitle: string): TPDFDocument;
begin
  Result := TPDFDocument.Create;
  Result.Title := ATitle;
  Result.Author := 'System';
  Result.CreationDate := Now;

  // Configuration standard du PDF
  Result.DefaultPageSetup.Margins.Left := 20;
  Result.DefaultPageSetup.Margins.Right := 20;
  Result.DefaultPageSetup.Margins.Top := 30;
  Result.DefaultPageSetup.Margins.Bottom := 30;
end;

procedure TReportGenerator.GeneratePDFReport(AReportData: TReportData);
begin
  var PDFDoc := CreateConfiguredPDFDocument('Monthly Report');
  try
    // Génération du contenu...

    PDFDoc.SaveToFile('report.pdf');
  finally
    PDFDoc.Free;
  end;
end;

procedure TReportGenerator.GenerateInvoicePDF(AInvoice: TInvoice);
begin
  var PDFDoc := CreateConfiguredPDFDocument('Invoice #' + AInvoice.Number);
  try
    // Génération du contenu de la facture...

    PDFDoc.SaveToFile('invoice_' + AInvoice.Number + '.pdf');
  finally
    PDFDoc.Free;
  end;
end;
```

### 4. Commentaires judicieux

Les commentaires doivent expliquer "pourquoi" plutôt que "quoi" ou "comment". Un bon code devrait être suffisamment expressif pour se passer d'explications sur ce qu'il fait.

**Mauvais exemple :**
```pascal
// Parcourt la liste des clients
for i := 0 to ClientList.Count - 1 do
begin
  // Obtient le client courant
  CurrentClient := ClientList[i];
  // Vérifie si le client est actif
  if CurrentClient.IsActive then
  begin
    // Ajoute le client à la liste des clients actifs
    ActiveClients.Add(CurrentClient);
  end;
end;
```

**Bon exemple :**
```pascal
// Nous filtrons les clients actifs pour l'envoi de la newsletter mensuelle
for i := 0 to ClientList.Count - 1 do
begin
  CurrentClient := ClientList[i];
  if CurrentClient.IsActive then
    ActiveClients.Add(CurrentClient);
end;
```

### 5. Gestion des erreurs propre

Gérez les erreurs de manière explicite et informative. Utilisez les blocs try-except et try-finally de manière appropriée.

**Mauvais exemple :**
```pascal
procedure TDataProcessor.ProcessFile(const AFileName: string);
var
  FileStream: TFileStream;
  DataReader: TDataReader;
begin
  FileStream := TFileStream.Create(AFileName, fmOpenRead);
  DataReader := TDataReader.Create(FileStream);

  // Si une exception se produit ici, FileStream et DataReader ne seront jamais libérés
  DataReader.ReadData;

  DataReader.Free;
  FileStream.Free;
end;
```

**Bon exemple :**
```pascal
procedure TDataProcessor.ProcessFile(const AFileName: string);
var
  FileStream: TFileStream;
  DataReader: TDataReader;
begin
  FileStream := nil;
  DataReader := nil;
  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead);
    DataReader := TDataReader.Create(FileStream);

    DataReader.ReadData;
  except
    on E: Exception do
    begin
      // Log de l'erreur ou gestion spécifique
      LogError('Error processing file: ' + E.Message);
      raise; // Relance l'exception pour informer l'appelant
    end;
  finally
    // Ces lignes s'exécuteront toujours, même en cas d'exception
    DataReader.Free;
    FileStream.Free;
  end;
end;
```

### 6. Éviter les nombres magiques et les chaînes codées en dur

Utilisez des constantes nommées pour les valeurs qui ont une signification particulière.

**Mauvais exemple :**
```pascal
if (Age >= 18) and (Income > 3000) then
  ApproveApplication
else
  RejectApplication;
```

**Bon exemple :**
```pascal
const
  MINIMUM_AGE_FOR_APPROVAL = 18;
  MINIMUM_INCOME_FOR_APPROVAL = 3000;

if (Age >= MINIMUM_AGE_FOR_APPROVAL) and
   (Income > MINIMUM_INCOME_FOR_APPROVAL) then
  ApproveApplication
else
  RejectApplication;
```

## Les principes SOLID

Les principes SOLID sont un ensemble de principes de conception orientée objet qui, lorsqu'ils sont appliqués ensemble, rendent le code plus maintenable, flexible et compréhensible.

L'acronyme SOLID représente :

- **S** : Single Responsibility Principle (Principe de Responsabilité Unique)
- **O** : Open/Closed Principle (Principe Ouvert/Fermé)
- **L** : Liskov Substitution Principle (Principe de Substitution de Liskov)
- **I** : Interface Segregation Principle (Principe de Ségrégation des Interfaces)
- **D** : Dependency Inversion Principle (Principe d'Inversion des Dépendances)

Voyons comment appliquer chacun de ces principes en Delphi.

### S - Principe de Responsabilité Unique (SRP)

Une classe ne devrait avoir qu'une seule raison de changer, c'est-à-dire qu'elle ne devrait avoir qu'une seule responsabilité.

**Mauvais exemple :**
```pascal
type
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FDatabase: TSQLConnection;
  public
    constructor Create;
    destructor Destroy; override;

    // Propriétés du client
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;

    // Méthodes de persistance
    procedure Save;
    procedure Load(AId: Integer);
    procedure Delete;

    // Méthodes de validation
    function ValidateEmail: Boolean;

    // Méthodes d'envoi d'emails
    procedure SendWelcomeEmail;
    procedure SendPasswordResetEmail;
  end;
```

**Bon exemple :**
```pascal
// Classe pour représenter un client (responsabilité : modèle de données)
type
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

// Classe pour gérer la persistance des clients (responsabilité : stockage)
type
  TCustomerRepository = class
  private
    FDatabase: TSQLConnection;
  public
    constructor Create(ADatabase: TSQLConnection);

    procedure Save(ACustomer: TCustomer);
    function Load(AId: Integer): TCustomer;
    procedure Delete(ACustomer: TCustomer);
  end;

// Classe pour valider les clients (responsabilité : validation)
type
  TCustomerValidator = class
  public
    class function ValidateEmail(const AEmail: string): Boolean;
  end;

// Classe pour envoyer des emails aux clients (responsabilité : communication)
type
  TCustomerEmailService = class
  private
    FEmailSender: TEmailSender;
  public
    constructor Create(AEmailSender: TEmailSender);

    procedure SendWelcomeEmail(ACustomer: TCustomer);
    procedure SendPasswordResetEmail(ACustomer: TCustomer);
  end;
```

### O - Principe Ouvert/Fermé (OCP)

Les entités logicielles (classes, modules, fonctions, etc.) doivent être ouvertes à l'extension mais fermées à la modification. En d'autres termes, vous devriez pouvoir étendre le comportement d'une entité sans modifier son code source.

**Mauvais exemple :**
```pascal
type
  TPaymentProcessor = class
  public
    procedure ProcessPayment(APaymentType: string; AAmount: Currency);
  end;

procedure TPaymentProcessor.ProcessPayment(APaymentType: string; AAmount: Currency);
begin
  if APaymentType = 'CreditCard' then
  begin
    // Logique pour traiter un paiement par carte de crédit
  end
  else if APaymentType = 'PayPal' then
  begin
    // Logique pour traiter un paiement PayPal
  end
  else if APaymentType = 'BankTransfer' then
  begin
    // Logique pour traiter un virement bancaire
  end;
  // Pour ajouter un nouveau type de paiement, il faut modifier cette méthode !
end;
```

**Bon exemple :**
```pascal
type
  IPaymentMethod = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    procedure ProcessPayment(AAmount: Currency);
  end;

  TCreditCardPayment = class(TInterfacedObject, IPaymentMethod)
  public
    procedure ProcessPayment(AAmount: Currency);
  end;

  TPayPalPayment = class(TInterfacedObject, IPaymentMethod)
  public
    procedure ProcessPayment(AAmount: Currency);
  end;

  TBankTransferPayment = class(TInterfacedObject, IPaymentMethod)
  public
    procedure ProcessPayment(AAmount: Currency);
  end;

  // Pour ajouter un nouveau type de paiement, il suffit d'ajouter une nouvelle classe
  // qui implémente IPaymentMethod, sans modifier le code existant :
  TApplePayPayment = class(TInterfacedObject, IPaymentMethod)
  public
    procedure ProcessPayment(AAmount: Currency);
  end;

  TPaymentProcessor = class
  public
    procedure ProcessPayment(APaymentMethod: IPaymentMethod; AAmount: Currency);
  end;

procedure TPaymentProcessor.ProcessPayment(APaymentMethod: IPaymentMethod; AAmount: Currency);
begin
  APaymentMethod.ProcessPayment(AAmount);
end;
```

### L - Principe de Substitution de Liskov (LSP)

Les objets d'une classe dérivée doivent pouvoir remplacer les objets de la classe de base sans affecter la correction du programme.

**Mauvais exemple :**
```pascal
type
  TRectangle = class
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    procedure SetWidth(AWidth: Integer); virtual;
    procedure SetHeight(AHeight: Integer); virtual;
    function GetArea: Integer;

    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

  TSquare = class(TRectangle)
  public
    // Un carré a des côtés égaux, donc SetWidth modifie aussi la hauteur
    procedure SetWidth(AWidth: Integer); override;
    // Et SetHeight modifie aussi la largeur
    procedure SetHeight(AHeight: Integer); override;
  end;

// Cette fonction suppose que modifier la largeur ne change pas la hauteur
procedure DoubleRectangleWidth(ARectangle: TRectangle);
var
  OriginalHeight: Integer;
begin
  OriginalHeight := ARectangle.Height;
  ARectangle.Width := ARectangle.Width * 2;

  // Avec TRectangle, cette assertion est vraie
  // Mais avec TSquare, elle échoue car la hauteur a aussi été modifiée !
  Assert(ARectangle.Height = OriginalHeight);
end;
```

**Bon exemple :**
```pascal
type
  // Interface commune pour les formes qui peuvent calculer leur aire
  IShape = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    function GetArea: Integer;
  end;

  // Rectangle implémente IShape
  TRectangle = class(TInterfacedObject, IShape)
  private
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(AWidth, AHeight: Integer);
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    function GetArea: Integer;
  end;

  // Carré implémente aussi IShape, mais n'hérite pas de TRectangle
  TSquare = class(TInterfacedObject, IShape)
  private
    FSideLength: Integer;
  public
    constructor Create(ASideLength: Integer);
    property SideLength: Integer read FSideLength write FSideLength;
    function GetArea: Integer;
  end;

// Cette fonction travaille avec n'importe quelle forme
procedure PrintArea(AShape: IShape);
begin
  Writeln('Area: ', AShape.GetArea);
end;
```

### I - Principe de Ségrégation des Interfaces (ISP)

Les clients ne doivent pas être forcés de dépendre d'interfaces qu'ils n'utilisent pas. Autrement dit, il vaut mieux avoir plusieurs interfaces spécifiques qu'une seule interface générale.

**Mauvais exemple :**
```pascal
type
  IWorker = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    procedure Work;
    procedure Eat;
    procedure Sleep;
  end;

  THuman = class(TInterfacedObject, IWorker)
  public
    procedure Work;
    procedure Eat;
    procedure Sleep;
  end;

  TRobot = class(TInterfacedObject, IWorker)
  public
    procedure Work;
    // Un robot n'a pas besoin de manger ni de dormir,
    // mais il est forcé d'implémenter ces méthodes
    procedure Eat; // Vide ou lance une exception
    procedure Sleep; // Vide ou lance une exception
  end;
```

**Bon exemple :**
```pascal
type
  IWorkable = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    procedure Work;
  end;

  IEatable = interface
    ['{B2C3D4E5-F6A1-8901-B2C3-D4E5F6A12345}']
    procedure Eat;
  end;

  ISleepable = interface
    ['{C3D4E5F6-A1B2-9012-C3D4-E5F6A1B23456}']
    procedure Sleep;
  end;

  THuman = class(TInterfacedObject, IWorkable, IEatable, ISleepable)
  public
    procedure Work;
    procedure Eat;
    procedure Sleep;
  end;

  TRobot = class(TInterfacedObject, IWorkable)
  public
    procedure Work;
    // Plus besoin d'implémenter Eat et Sleep
  end;
```

### D - Principe d'Inversion des Dépendances (DIP)

Les modules de haut niveau ne devraient pas dépendre des modules de bas niveau. Les deux devraient dépendre d'abstractions.
Les abstractions ne devraient pas dépendre des détails. Les détails devraient dépendre des abstractions.

**Mauvais exemple :**
```pascal
type
  TEmailSender = class
  public
    procedure SendEmail(ATo, ASubject, ABody: string);
  end;

  TNotificationService = class
  private
    FEmailSender: TEmailSender;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NotifyUser(AUserEmail, AMessage: string);
  end;

constructor TNotificationService.Create;
begin
  inherited;
  FEmailSender := TEmailSender.Create;
end;

destructor TNotificationService.Destroy;
begin
  FEmailSender.Free;
  inherited;
end;

procedure TNotificationService.NotifyUser(AUserEmail, AMessage: string);
begin
  FEmailSender.SendEmail(AUserEmail, 'Notification', AMessage);
end;
```

**Bon exemple :**
```pascal
type
  // Abstraction pour l'envoi de notifications
  INotificationSender = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    procedure SendNotification(ATo, AMessage: string);
  end;

  // Implémentation pour les emails
  TEmailSender = class(TInterfacedObject, INotificationSender)
  public
    procedure SendNotification(ATo, AMessage: string);
  end;

  // Implémentation pour les SMS
  TSMSSender = class(TInterfacedObject, INotificationSender)
  public
    procedure SendNotification(ATo, AMessage: string);
  end;

  // Service de haut niveau qui dépend de l'abstraction, pas de l'implémentation
  TNotificationService = class
  private
    FNotificationSender: INotificationSender;
  public
    constructor Create(ANotificationSender: INotificationSender);
    procedure NotifyUser(AUserEmail, AMessage: string);
  end;

constructor TNotificationService.Create(ANotificationSender: INotificationSender);
begin
  inherited Create;
  FNotificationSender := ANotificationSender;
end;

procedure TNotificationService.NotifyUser(AUserEmail, AMessage: string);
begin
  FNotificationSender.SendNotification(AUserEmail, AMessage);
end;
```

## Application pratique : Refactorisation avec SOLID et Clean Code

Voyons comment refactoriser un exemple de code Delphi en appliquant les principes SOLID et les pratiques de Clean Code.

### Avant refactorisation :

```pascal
type
  TCustomerManager = class
  private
    FDatabaseConnection: TSQLConnection;
    FDataset: TDataset;
    FEmailService: TIdSMTP;
  public
    constructor Create;
    destructor Destroy; override;

    // Méthodes pour les clients
    function AddCustomer(const Name, Email, Address: string; Age: Integer): Integer;
    procedure UpdateCustomer(Id: Integer; const Name, Email, Address: string; Age: Integer);
    procedure DeleteCustomer(Id: Integer);
    function GetCustomerById(Id: Integer): TDataset;

    // Méthodes pour les commandes
    function CreateOrder(CustomerId: Integer; Products: TStringList; TotalAmount: Currency): Integer;
    procedure ShipOrder(OrderId: Integer; ShippingMethod: string);
    function GetOrderStatus(OrderId: Integer): string;

    // Validation
    function ValidateCustomerData(const Name, Email: string; Age: Integer): Boolean;

    // Notifications
    procedure SendWelcomeEmail(const Email, Name: string);
    procedure SendOrderConfirmation(OrderId: Integer; const CustomerEmail: string);
  end;

constructor TCustomerManager.Create;
begin
  inherited;
  FDatabaseConnection := TSQLConnection.Create(nil);
  FDatabaseConnection.ConnectionName := 'MyConnection';
  FDatabaseConnection.DriverName := 'SQLite';
  FDatabaseConnection.Params.Values['Database'] := 'customers.db';
  FDatabaseConnection.Connected := True;

  FEmailService := TIdSMTP.Create(nil);
  FEmailService.Host := 'smtp.example.com';
  FEmailService.Port := 587;
  FEmailService.Username := 'user@example.com';
  FEmailService.Password := 'password123';
end;

destructor TCustomerManager.Destroy;
begin
  FEmailService.Free;
  FDatabaseConnection.Free;
  inherited;
end;

function TCustomerManager.AddCustomer(const Name, Email, Address: string; Age: Integer): Integer;
begin
  // Validation
  if not ValidateCustomerData(Name, Email, Age) then
    raise Exception.Create('Invalid customer data');

  // Insertion dans la base de données
  FDataset := FDatabaseConnection.CreateDataSet;
  try
    FDataset.Close;
    FDataset.SQL.Text := 'INSERT INTO Customers (Name, Email, Address, Age) VALUES (:Name, :Email, :Address, :Age) RETURNING ID';
    FDataset.ParamByName('Name').AsString := Name;
    FDataset.ParamByName('Email').AsString := Email;
    FDataset.ParamByName('Address').AsString := Address;
    FDataset.ParamByName('Age').AsInteger := Age;
    FDataset.Open;
    Result := FDataset.FieldByName('ID').AsInteger;
  finally
    FDataset.Free;
  end;

  // Envoi d'email de bienvenue
  SendWelcomeEmail(Email, Name);
end;

// ... autres méthodes ...

procedure TCustomerManager.SendWelcomeEmail(const Email, Name: string);
var
  Message: TIdMessage;
begin
  Message := TIdMessage.Create(nil);
  try
    Message.From.Address := 'noreply@example.com';
    Message.Recipients.Add.Address := Email;
    Message.Subject := 'Welcome to our service';
    Message.Body.Text := 'Hello ' + Name + ', welcome to our service!';

    FEmailService.Connect;
    try
      FEmailService.Send(Message);
    finally
      FEmailService.Disconnect;
    end;
  finally
    Message.Free;
  end;
end;
```

### Après refactorisation :

```pascal
// 1. Interfaces pour respecter les principes SOLID

// Interface pour l'accès aux données des clients (SRP, OCP, DIP)
type
  ICustomerRepository = interface
    ['{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}']
    function Add(const ACustomer: TCustomer): Integer;
    procedure Update(ACustomer: TCustomer);
    procedure Delete(ACustomerId: Integer);
    function GetById(ACustomerId: Integer): TCustomer;
  end;

// Interface pour l'accès aux données des commandes (SRP, ISP)
type
  IOrderRepository = interface
    ['{B2C3D4E5-F6A1-8901-B2C3-D4E5F6A12345}']
    function Create(AOrder: TOrder): Integer;
    procedure Update(AOrder: TOrder);
    function GetById(AOrderId: Integer): TOrder;
  end;

// Interface pour la validation (SRP, ISP)
type
  ICustomerValidator = interface
    ['{C3D4E5F6-A1B2-9012-C3D4-E5F6A1B23456}']
    function Validate(ACustomer: TCustomer): Boolean;
    function GetValidationErrors: TStringList;
  end;

// Interface pour l'envoi d'emails (SRP, ISP, DIP)
type
  IEmailService = interface
    ['{D4E5F6A1-B2C3-0123-D4E5-F6A1B2C34567}']
    procedure SendEmail(const ATo, ASubject, ABody: string);
  end;

// 2. Classes de modèle pour les entités métier

// Classe Customer (SRP)
type
  TCustomer = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FAddress: string;
    FAge: Integer;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Address: string read FAddress write FAddress;
    property Age: Integer read FAge write FAge;
  end;

// Classe Order (SRP)
type
  TOrderItem = class
  private
    FProductId: Integer;
    FQuantity: Integer;
    FUnitPrice: Currency;
  public
    property ProductId: Integer read FProductId write FProductId;
    property Quantity: Integer read FQuantity write FQuantity;
    property UnitPrice: Currency read FUnitPrice write FUnitPrice;
    function GetTotal: Currency;
  end;

  TOrder = class
  private
    FId: Integer;
    FCustomerId: Integer;
    FItems: TObjectList<TOrderItem>;
    FOrderDate: TDateTime;
    FStatus: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Integer read FId write FId;
    property CustomerId: Integer read FCustomerId write FCustomerId;
    property Items: TObjectList<TOrderItem> read FItems;
    property OrderDate: TDateTime read FOrderDate write FOrderDate;
    property Status: string read FStatus write FStatus;

    function GetTotalAmount: Currency;
  end;

// 3. Implémentations des interfaces

// Implémentation SQLite du repository client (SRP, OCP)
type
  TSQLiteCustomerRepository = class(TInterfacedObject, ICustomerRepository)
  private
    FConnection: TSQLConnection;
  public
    constructor Create(AConnection: TSQLConnection);

    function Add(const ACustomer: TCustomer): Integer;
    procedure Update(ACustomer: TCustomer);
    procedure Delete(ACustomerId: Integer);
    function GetById(ACustomerId: Integer): TCustomer;
  end;

// Implémentation du service d'email (SRP, OCP)
type
  TSmtpEmailService = class(TInterfacedObject, IEmailService)
  private
    FSmtpClient: TIdSMTP;
  public
    constructor Create(const AHost: string; APort: Integer;
                       const AUsername, APassword: string);
    destructor Destroy; override;

    procedure SendEmail(const ATo, ASubject, ABody: string);
  end;

// Implémentation du validateur client (SRP)
type
  TCustomerValidator = class(TInterfacedObject, ICustomerValidator)
  private
    FErrors: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    function Validate(ACustomer: TCustomer): Boolean;
    function GetValidationErrors: TStringList;
  end;

// 4. Service de gestion des clients (utilisant les interfaces pour respecter DIP)

type
  TCustomerService = class
  private
    FCustomerRepository: ICustomerRepository;
    FCustomerValidator: ICustomerValidator;
    FEmailService: IEmailService;
  public
    constructor Create(ACustomerRepository: ICustomerRepository;
                       ACustomerValidator: ICustomerValidator;
                       AEmailService: IEmailService);

    function RegisterCustomer(ACustomer: TCustomer): Integer;
    procedure UpdateCustomerDetails(ACustomer: TCustomer);
    procedure RemoveCustomer(ACustomerId: Integer);
    function GetCustomer(ACustomerId: Integer): TCustomer;

    procedure SendWelcomeEmail(ACustomer: TCustomer);
  end;

// 5. Service de gestion des commandes (utilisant les interfaces pour respecter DIP)

type
  TOrderService = class
  private
    FOrderRepository: IOrderRepository;
    FEmailService: IEmailService;
    FCustomerRepository: ICustomerRepository;
  public
    constructor Create(AOrderRepository: IOrderRepository;
                       AEmailService: IEmailService;
                       ACustomerRepository: ICustomerRepository);

    function PlaceOrder(AOrder: TOrder): Integer;
    procedure ShipOrder(AOrderId: Integer);
    function GetOrderStatus(AOrderId: Integer): string;

    procedure SendOrderConfirmation(AOrder: TOrder);
  end;

// 6. Implémentations des méthodes (exemples choisis)

constructor TCustomerService.Create(ACustomerRepository: ICustomerRepository;
                                   ACustomerValidator: ICustomerValidator;
                                   AEmailService: IEmailService);
begin
  inherited Create;
  FCustomerRepository := ACustomerRepository;
  FCustomerValidator := ACustomerValidator;
  FEmailService := AEmailService;
end;

function TCustomerService.RegisterCustomer(ACustomer: TCustomer): Integer;
begin
  // Validation du client
  if not FCustomerValidator.Validate(ACustomer) then
    raise EValidationException.Create('Invalid customer data: ' +
                                      FCustomerValidator.GetValidationErrors.Text);

  // Ajout du client dans le repository
  Result := FCustomerRepository.Add(ACustomer);

  // Envoi de l'email de bienvenue
  SendWelcomeEmail(ACustomer);
end;

procedure TCustomerService.SendWelcomeEmail(ACustomer: TCustomer);
var
  EmailBody: string;
begin
  EmailBody := 'Hello ' + ACustomer.Name + ', welcome to our service!';

  FEmailService.SendEmail(ACustomer.Email, 'Welcome to our service', EmailBody);
end;

constructor TSmtpEmailService.Create(const AHost: string; APort: Integer;
                                     const AUsername, APassword: string);
begin
  inherited Create;

  FSmtpClient := TIdSMTP.Create(nil);
  FSmtpClient.Host := AHost;
  FSmtpClient.Port := APort;
  FSmtpClient.Username := AUsername;
  FSmtpClient.Password := APassword;
end;

destructor TSmtpEmailService.Destroy;
begin
  FSmtpClient.Free;
  inherited;
end;

procedure TSmtpEmailService.SendEmail(const ATo, ASubject, ABody: string);
var
  Message: TIdMessage;
begin
  Message := TIdMessage.Create(nil);
  try
    Message.From.Address := 'noreply@example.com';
    Message.Recipients.Add.Address := ATo;
    Message.Subject := ASubject;
    Message.Body.Text := ABody;

    FSmtpClient.Connect;
    try
      FSmtpClient.Send(Message);
    finally
      FSmtpClient.Disconnect;
    end;
  finally
    Message.Free;
  end;
end;

// 7. Création et utilisation des objets (avec injection de dépendances)

procedure ConfigureAndUseServices;
var
  Connection: TSQLConnection;
  CustomerRepo: ICustomerRepository;
  OrderRepo: IOrderRepository;
  EmailService: IEmailService;
  Validator: ICustomerValidator;
  CustomerService: TCustomerService;
  OrderService: TOrderService;
  Customer: TCustomer;
begin
  // Configuration de la connexion à la base de données
  Connection := TSQLConnection.Create(nil);
  try
    Connection.ConnectionName := 'MyConnection';
    Connection.DriverName := 'SQLite';
    Connection.Params.Values['Database'] := 'customers.db';
    Connection.Connected := True;

    // Création des repositories
    CustomerRepo := TSQLiteCustomerRepository.Create(Connection);
    OrderRepo := TSQLiteOrderRepository.Create(Connection);

    // Création des autres services
    EmailService := TSmtpEmailService.Create('smtp.example.com', 587,
                                             'user@example.com', 'password123');
    Validator := TCustomerValidator.Create;

    // Création des services principaux avec injection de dépendances
    CustomerService := TCustomerService.Create(CustomerRepo, Validator, EmailService);
    OrderService := TOrderService.Create(OrderRepo, EmailService, CustomerRepo);

    // Utilisation des services
    Customer := TCustomer.Create;
    try
      Customer.Name := 'John Doe';
      Customer.Email := 'john@example.com';
      Customer.Address := '123 Main St';
      Customer.Age := 30;

      CustomerService.RegisterCustomer(Customer);
    finally
      Customer.Free;
    end;

  finally
    Connection.Free;
    // Note: Pas besoin de libérer les interfaces car elles sont gérées par le compteur de références
  end;
end;
```

## Avantages de l'approche refactorisée

La refactorisation ci-dessus a appliqué tous les principes SOLID et les pratiques de Clean Code, ce qui offre de nombreux avantages :

1. **Séparation des responsabilités (SRP)** :
   - Chaque classe a une seule responsabilité (gestion des clients, validation, emails, etc.)
   - Le code est plus facile à comprendre et à maintenir

2. **Extension sans modification (OCP)** :
   - De nouvelles implémentations de repository peuvent être ajoutées sans modifier le code existant
   - Par exemple, on pourrait ajouter une classe `TMongoDBCustomerRepository` sans toucher au reste du code

3. **Substitution de Liskov (LSP)** :
   - Toutes les implémentations d'interfaces peuvent être substituées sans affecter le comportement
   - Le code fonctionne avec n'importe quelle implémentation de `ICustomerRepository`

4. **Interfaces spécifiques (ISP)** :
   - Les interfaces sont petites et spécifiques (`ICustomerRepository`, `IEmailService`, etc.)
   - Les clients ne dépendent que des méthodes dont ils ont besoin

5. **Inversion des dépendances (DIP)** :
   - Les classes de haut niveau (`TCustomerService`) dépendent d'abstractions (interfaces)
   - Les détails d'implémentation sont injectés (injection de dépendances)

6. **Testabilité améliorée** :
   - Chaque composant peut être testé isolément avec des mocks
   - Par exemple, on peut tester `TCustomerService` avec un faux repository et un faux service d'email

7. **Flexibilité accrue** :
   - Facile de changer l'implémentation de n'importe quel composant
   - Par exemple, passer de SQLite à MySQL ou changer le service d'email

## Conseils pratiques pour appliquer Clean Code et SOLID en Delphi

### 1. Commencer petit

N'essayez pas de refactoriser toute votre application d'un coup. Commencez par identifier les parties les plus problématiques et appliquez les principes progressivement.

### 2. Utiliser les interfaces

Delphi supporte très bien les interfaces, qui sont essentielles pour l'application des principes OCP, LSP, ISP et DIP. Prenez l'habitude de définir des interfaces avant d'implémenter des classes.

### 3. Adopter l'injection de dépendances

Passez les dépendances via le constructeur plutôt que de les créer à l'intérieur des classes. Cela rend le code plus testable et plus flexible.

```pascal
// À éviter
constructor TCustomerService.Create;
begin
  inherited;
  FRepository := TSQLiteCustomerRepository.Create;
end;

// Préférer
constructor TCustomerService.Create(ARepository: ICustomerRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;
```

### 4. Limiter la taille des méthodes et des classes

- Visez des méthodes de moins de 20 lignes
- Limitez les classes à une seule responsabilité
- Si une classe devient trop grande, divisez-la en plusieurs classes plus petites

### 5. Utiliser des noms expressifs

Prenez le temps de choisir des noms qui révèlent l'intention :

```pascal
// Peu expressif
procedure P1(C: TCustomer);

// Expressif
procedure ValidateAndRegisterCustomer(ANewCustomer: TCustomer);
```

### 6. Éviter les abréviations obscures

```pascal
// Difficile à comprendre
var
  CntMgr: TCntMgr;

// Plus clair
var
  ContentManager: TContentManager;
```

### 7. Structurer les unités logiquement

- Regroupez les classes liées dans des unités cohérentes
- Utilisez des préfixes ou des suffixes pour indiquer le rôle des unités :
  - `Models.Customer.pas` pour les modèles
  - `Repositories.Customer.pas` pour les repositories
  - `Services.Customer.pas` pour les services

### 8. Documenter l'intention, pas l'évidence

```pascal
// Commentaire inutile qui répète le code
// Incrémente le compteur
Counter := Counter + 1;

// Commentaire utile qui explique l'intention
// Passe au client suivant dans la liste
Counter := Counter + 1;
```

### 9. Utiliser les assertions pour vérifier les préconditions

```pascal
procedure ProcessOrder(AOrder: TOrder);
begin
  Assert(Assigned(AOrder), 'Order cannot be nil');
  Assert(AOrder.Items.Count > 0, 'Order must have items');

  // Traitement de la commande...
end;
```

### 10. Appliquer le refactoring régulièrement

Ne considérez pas le refactoring comme une tâche exceptionnelle. Intégrez-le dans votre processus de développement quotidien selon le principe du "Boy Scout" : laissez le code plus propre que vous ne l'avez trouvé.

## Outils pour améliorer la qualité du code Delphi

Plusieurs outils peuvent vous aider à maintenir un code propre et à respecter les principes SOLID :

### 1. Analyseurs de code statiques

- [Pascal Analyzer](https://www.peganza.com/products_pal.html) : détecte les problèmes de qualité de code
- [DelphiAST](https://github.com/RomanYankovsky/DelphiAST) : analyse le code source Pascal
- [Sonar](https://www.sonarqube.org/) avec plugin Pascal : analyse complète de la qualité du code

### 2. Outils de refactoring dans l'IDE

- Utilisez les outils de refactoring intégrés à l'IDE Delphi (Ctrl+Shift+E pour renommer, etc.)
- [Castalia](http://www.twodesk.com/castalia/) : add-in pour améliorer l'expérience de développement et faciliter le refactoring

### 3. Frameworks de test unitaire

- [DUnit](http://dunit.sourceforge.net/) : framework de test unitaire classique
- [DUnitX](https://github.com/VSoftTechnologies/DUnitX) : version moderne de DUnit
- [TestInsight](https://bitbucket.org/sglienke/testinsight/wiki/Home) : extension IDE pour l'exécution de tests

### 4. Frameworks d'injection de dépendances

- [Spring4D](https://bitbucket.org/sglienke/spring4d) : framework complet avec conteneur IoC
- [DSharp](https://github.com/thomaserlang/dsharp) : bibliothèque avec support pour l'injection de dépendances

## Conclusion

L'adoption des principes SOLID et des pratiques de Clean Code en Delphi peut transformer radicalement la qualité de votre code. Le résultat sera un code plus lisible, plus maintenable, plus testable et plus évolutif.

Bien que l'application de ces principes puisse sembler exiger plus d'effort initial, cet investissement sera rapidement rentabilisé par la réduction du temps passé à déboguer, à comprendre le code existant et à l'adapter aux nouveaux besoins.

N'oubliez pas que l'amélioration de la qualité du code est un processus continu et progressif. Commencez par de petites améliorations, et avec le temps, ces bonnes pratiques deviendront naturelles et feront partie intégrante de votre façon de programmer.

Le code propre n'est pas seulement une question de technique, c'est aussi une question de respect : respect envers vos collègues qui devront lire et modifier votre code, respect envers vos futurs utilisateurs qui bénéficieront d'un logiciel plus fiable, et respect envers votre futur vous-même qui vous remerciera d'avoir écrit un code clair et bien structuré.

⏭️ [Domain-Driven Design (DDD) avec Delphi](/18-architecture-et-bonnes-pratiques/10-domain-driven-design-avec-delphi.md)
