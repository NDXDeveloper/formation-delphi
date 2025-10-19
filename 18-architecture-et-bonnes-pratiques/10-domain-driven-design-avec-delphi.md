🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.10 Domain-Driven Design (DDD) avec Delphi

## Introduction

Imaginez que vous devez construire une maison. Vous avez deux approches :

**Approche A - Technique d'abord :**
```
"J'ai besoin d'une base de données avec des tables : users, orders, products..."
"Ensuite je fais des formulaires pour créer des users..."
"Puis je code les CRUD..."
```

**Approche B - Métier d'abord (DDD) :**
```
"Dans notre boutique, un Client peut passer des Commandes"
"Une Commande contient des Produits"
"Quand un Client valide sa Commande, il devient un Acheteur"
"Si une Commande dépasse 500€, le Client obtient le statut Premium"
```

La différence ? Dans l'approche B, vous **parlez le langage du métier**, pas le langage technique. C'est l'essence du **Domain-Driven Design** (DDD).

### Qu'est-ce que le Domain-Driven Design ?

Le **Domain-Driven Design** (conception pilotée par le domaine) est une approche de développement logiciel créée par Eric Evans en 2003. L'idée centrale : **votre code doit refléter fidèlement le domaine métier**, pas la technique.

**Domaine** = Le secteur d'activité, le métier, les règles business

**Le problème classique :**
```pascal
// Code technique qui ne parle pas métier
type
  TUserData = class
    UserID: Integer;
    UserName: string;
    UserEmail: string;
    OrderCount: Integer;
  end;

procedure UpdateUser(UserID: Integer; NewData: TUserData);
```

**Approche DDD :**
```pascal
// Code qui parle métier
type
  TCustomer = class
  private
    FID: TCustomerID;
    FName: TCustomerName;
    FEmail: TEmailAddress;
    FOrderHistory: TOrderHistory;
  public
    procedure PlaceOrder(Order: TOrder);
    function IsEligibleForPremiumStatus: Boolean;
    property Name: TCustomerName read FName;
  end;
```

### Pourquoi DDD ?

**Les bénéfices :**

1. **Communication claire** : Développeurs et experts métier parlent le même langage
2. **Code qui vit longtemps** : Le métier évolue lentement, la technique change vite
3. **Complexité maîtrisée** : Les règles métier sont explicites dans le code
4. **Maintenance facilitée** : Le code raconte l'histoire du métier
5. **Moins de bugs métier** : Les règles sont dans le domaine, pas éparpillées

**Quand utiliser DDD ?**

✅ **Oui si :**
- Le domaine métier est complexe
- Les règles métier changent fréquemment
- Collaboration étroite avec des experts métier
- Projet à long terme

❌ **Non si :**
- Simple CRUD sans logique métier
- Projet très petit (< 10 écrans)
- Pas d'accès aux experts métier
- Prototype jetable

### DDD ≠ Architecture en couches

DDD peut être utilisé avec n'importe quelle architecture, mais il se marie bien avec :
- Hexagonal Architecture (Ports & Adapters)
- Clean Architecture
- Onion Architecture

## Les concepts fondamentaux du DDD

### 1. Ubiquitous Language (Langage Omniprésent)

Le **langage omniprésent** est le vocabulaire partagé par toute l'équipe : développeurs, experts métier, chefs de projet.

**Principe :** Les termes utilisés dans le code doivent être **exactement** les mêmes que ceux utilisés par le métier.

**❌ Mauvais exemple :**
```pascal
// L'expert métier dit : "Un client fidèle"
// Le code dit :
type
  TUser = class
    function GetLoyaltyLevel: Integer;  // ?
  end;
```

**✅ Bon exemple :**
```pascal
// L'expert métier dit : "Un client fidèle"
// Le code dit aussi :
type
  TCustomer = class
    function IsLoyalCustomer: Boolean;
  end;
```

**En pratique :**

```pascal
// ❌ Langage technique
procedure ProcessData(ID: Integer);
var
  Record: TDataRecord;
begin
  Record := LoadRecord(ID);
  UpdateField(Record, 'status', 'approved');
  SaveRecord(Record);
end;

// ✅ Langage métier (Ubiquitous Language)
procedure ApproveInvoice(InvoiceID: TInvoiceID);
var
  Invoice: TInvoice;
begin
  Invoice := FInvoiceRepository.GetByID(InvoiceID);
  Invoice.Approve;
  FInvoiceRepository.Save(Invoice);
end;
```

**Règle d'or :** Si un terme apparaît dans les discussions métier, il doit apparaître tel quel dans le code.

### 2. Bounded Context (Contexte Délimité)

Un **Bounded Context** est une frontière explicite dans laquelle un modèle de domaine est valide.

**Exemple concret : "Client" dans une entreprise**

Dans le contexte **Ventes** :
```pascal
type
  TCustomer = class
    // Un client = quelqu'un qui achète
    FOrderHistory: TList<TOrder>;
    FCreditLimit: Currency;
    function CanPlaceOrder(Amount: Currency): Boolean;
  end;
```

Dans le contexte **Support** :
```pascal
type
  TCustomer = class
    // Un client = quelqu'un qui a besoin d'aide
    FTickets: TList<TSupportTicket>;
    FPreferredContactMethod: TContactMethod;
    function HasOpenTickets: Boolean;
  end;
```

Dans le contexte **Comptabilité** :
```pascal
type
  TCustomer = class
    // Un client = un compte avec des paiements
    FInvoices: TList<TInvoice>;
    FPaymentHistory: TPaymentHistory;
    function GetOutstandingBalance: Currency;
  end;
```

**C'est le même client**, mais vu sous trois angles différents ! Chaque contexte a son propre modèle.

**En Delphi :**
```
MonProjet/
├── Source/
│   ├── Sales/                    ← Bounded Context "Ventes"
│   │   ├── Models/
│   │   │   ├── Customer.pas
│   │   │   └── Order.pas
│   │   └── Services/
│   │
│   ├── Support/                  ← Bounded Context "Support"
│   │   ├── Models/
│   │   │   ├── Customer.pas     (différent du Sales.Customer !)
│   │   │   └── Ticket.pas
│   │   └── Services/
│   │
│   └── Accounting/               ← Bounded Context "Comptabilité"
│       ├── Models/
│       │   ├── Customer.pas     (encore différent !)
│       │   └── Invoice.pas
│       └── Services/
```

### 3. Entities (Entités)

Une **Entité** est un objet qui a une **identité unique** qui persiste dans le temps, même si ses attributs changent.

**Caractéristiques :**
- A un identifiant unique (ID)
- Peut changer d'état
- Persiste dans le temps

**Exemple :**
```pascal
type
  TCustomerID = record
    Value: TGUID;
  end;

  TCustomer = class
  private
    FID: TCustomerID;        // Identité unique
    FName: string;           // Peut changer
    FEmail: string;          // Peut changer
    FCreatedAt: TDateTime;
  public
    constructor Create(ID: TCustomerID; const Name, Email: string);

    // L'égalité est basée sur l'ID, pas sur les attributs
    function Equals(Other: TCustomer): Boolean;

    property ID: TCustomerID read FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
  end;

implementation

constructor TCustomer.Create(ID: TCustomerID; const Name, Email: string);
begin
  inherited Create;
  FID := ID;
  FName := Name;
  FEmail := Email;
  FCreatedAt := Now;
end;

function TCustomer.Equals(Other: TCustomer): Boolean;
begin
  // Deux clients sont égaux s'ils ont le même ID
  // Même si leur nom ou email a changé
  Result := (Other <> nil) and (FID.Value = Other.ID.Value);
end;
```

**Exemple réel :**
Vous changez de nom, d'adresse, de téléphone, mais vous restez **vous**. Votre identité (numéro de sécurité sociale) ne change pas. Vous êtes une **Entité**.

### 4. Value Objects (Objets Valeur)

Un **Value Object** est un objet défini uniquement par ses attributs, sans identité propre. Deux objets valeur avec les mêmes attributs sont **identiques**.

**Caractéristiques :**
- Pas d'identité
- Immutable (ne change pas après création)
- L'égalité est basée sur les attributs

**❌ Mauvaise approche (types primitifs) :**
```pascal
type
  TCustomer = class
  private
    FEmail: string;  // Juste un string ?
  end;

// Problème : Pas de validation
Customer.Email := 'pas-un-email';  // Accepté !
```

**✅ Value Object :**
```pascal
type
  TEmailAddress = record
  private
    FValue: string;
    function GetValue: string;
  public
    constructor Create(const Value: string);
    class function TryCreate(const Value: string; out Email: TEmailAddress): Boolean; static;

    // Validation dans le constructeur
    function IsValid: Boolean;

    // Immutable
    property Value: string read GetValue;

    // Égalité basée sur la valeur
    class operator Equal(const A, B: TEmailAddress): Boolean;
  end;

implementation

constructor TEmailAddress.Create(const Value: string);
begin
  if not IsValidEmail(Value) then
    raise Exception.Create('Invalid email address');
  FValue := Value;
end;

class function TEmailAddress.TryCreate(const Value: string; out Email: TEmailAddress): Boolean;
begin
  Result := IsValidEmail(Value);
  if Result then
    Email.FValue := Value;
end;

function TEmailAddress.IsValid: Boolean;
begin
  Result := FValue.Contains('@') and (Length(FValue) > 3);
end;

function TEmailAddress.GetValue: string;
begin
  Result := FValue;
end;

class operator TEmailAddress.Equal(const A, B: TEmailAddress): Boolean;
begin
  Result := A.Value = B.Value;
end;
```

**Utilisation :**
```pascal
type
  TCustomer = class
  private
    FEmail: TEmailAddress;  // Plus de validation à faire ici !
  public
    property Email: TEmailAddress read FEmail write FEmail;
  end;

var
  Customer: TCustomer;
  Email: TEmailAddress;
begin
  Customer := TCustomer.Create;

  // ✅ Valide
  if TEmailAddress.TryCreate('john@example.com', Email) then
    Customer.Email := Email;

  // ❌ Exception levée
  Email := TEmailAddress.Create('invalid');  // Exception !
end;
```

**Autres exemples de Value Objects :**
```pascal
type
  TMoney = record
    Amount: Currency;
    Currency: TCurrencyCode;
  end;

  TAddress = record
    Street: string;
    City: string;
    PostalCode: string;
    Country: string;
  end;

  TDateRange = record
    StartDate: TDate;
    EndDate: TDate;
    function Contains(Date: TDate): Boolean;
  end;
```

### 5. Aggregates (Agrégats)

Un **Aggregate** est un groupe d'objets (Entités et Value Objects) traités comme une seule unité. Il a une **Entité Racine** (Aggregate Root) qui contrôle l'accès à tout l'agrégat.

**Règles :**
1. L'accès à l'agrégat passe toujours par la racine
2. Les objets internes ne peuvent pas être modifiés directement
3. Une transaction ne peut modifier qu'un seul agrégat à la fois

**❌ Sans Aggregate (anarchie) :**
```pascal
var
  Order: TOrder;
  OrderLine: TOrderLine;
begin
  Order := GetOrder(123);

  // Accès direct aux lignes, pas de contrôle !
  OrderLine := Order.Lines[0];
  OrderLine.Quantity := -10;  // ⚠️ Quantité négative !
  OrderLine.Price := 0;        // ⚠️ Prix à zéro !

  SaveOrderLine(OrderLine);    // ⚠️ Incohérence !
end;
```

**✅ Avec Aggregate (contrôlé) :**
```pascal
type
  TOrderID = record
    Value: TGUID;
  end;

  TOrderLine = class
  private
    FProductID: TProductID;
    FQuantity: Integer;
    FPrice: Currency;
  public
    constructor Create(ProductID: TProductID; Quantity: Integer; Price: Currency);
    property ProductID: TProductID read FProductID;
    property Quantity: Integer read FQuantity;
    property Price: Currency read FPrice;
    function GetTotal: Currency;
  end;

  // Aggregate Root
  TOrder = class
  private
    FID: TOrderID;
    FCustomerID: TCustomerID;
    FLines: TObjectList<TOrderLine>;
    FStatus: TOrderStatus;
    FTotal: Currency;

    // Méthodes privées pour maintenir la cohérence
    procedure RecalculateTotal;
    function ValidateOrderLine(ProductID: TProductID; Quantity: Integer): Boolean;
  public
    constructor Create(ID: TOrderID; CustomerID: TCustomerID);
    destructor Destroy; override;

    // Seule façon d'ajouter une ligne (via la racine)
    procedure AddLine(ProductID: TProductID; Quantity: Integer; Price: Currency);

    // Seule façon de modifier une ligne (via la racine)
    procedure UpdateLineQuantity(ProductID: TProductID; NewQuantity: Integer);

    // Seule façon de supprimer une ligne (via la racine)
    procedure RemoveLine(ProductID: TProductID);

    // Transitions d'état contrôlées
    procedure Submit;
    procedure Cancel;

    // Lecture seule de l'extérieur
    property ID: TOrderID read FID;
    property Status: TOrderStatus read FStatus;
    property Total: Currency read FTotal;
    property Lines: TObjectList<TOrderLine> read FLines;  // Read-only
  end;

implementation

constructor TOrder.Create(ID: TOrderID; CustomerID: TCustomerID);
begin
  inherited Create;
  FID := ID;
  FCustomerID := CustomerID;
  FLines := TObjectList<TOrderLine>.Create(True);
  FStatus := osCreated;
  FTotal := 0;
end;

destructor TOrder.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TOrder.AddLine(ProductID: TProductID; Quantity: Integer; Price: Currency);
var
  Line: TOrderLine;
begin
  // Validation
  if not ValidateOrderLine(ProductID, Quantity) then
    raise Exception.Create('Invalid order line');

  if FStatus <> osCreated then
    raise Exception.Create('Cannot modify submitted order');

  // Création
  Line := TOrderLine.Create(ProductID, Quantity, Price);
  FLines.Add(Line);

  // Maintien de la cohérence
  RecalculateTotal;
end;

procedure TOrder.UpdateLineQuantity(ProductID: TProductID; NewQuantity: Integer);
var
  Line: TOrderLine;
begin
  if FStatus <> osCreated then
    raise Exception.Create('Cannot modify submitted order');

  for Line in FLines do
  begin
    if Line.ProductID.Equals(ProductID) then
    begin
      if NewQuantity <= 0 then
        raise Exception.Create('Quantity must be positive');

      // On ne peut pas modifier directement Line.FQuantity car c'est privé
      // On doit recréer la ligne ou avoir une méthode SetQuantity protégée
      RemoveLine(ProductID);
      AddLine(ProductID, NewQuantity, Line.Price);
      Break;
    end;
  end;

  RecalculateTotal;
end;

procedure TOrder.RemoveLine(ProductID: TProductID);
var
  I: Integer;
begin
  if FStatus <> osCreated then
    raise Exception.Create('Cannot modify submitted order');

  for I := FLines.Count - 1 downto 0 do
  begin
    if FLines[I].ProductID.Equals(ProductID) then
    begin
      FLines.Delete(I);
      Break;
    end;
  end;

  RecalculateTotal;
end;

procedure TOrder.RecalculateTotal;
var
  Line: TOrderLine;
begin
  FTotal := 0;
  for Line in FLines do
    FTotal := FTotal + Line.GetTotal;
end;

function TOrder.ValidateOrderLine(ProductID: TProductID; Quantity: Integer): Boolean;
begin
  Result := (Quantity > 0) and (Quantity <= 1000);  // Règles métier
end;

procedure TOrder.Submit;
begin
  if FStatus <> osCreated then
    raise Exception.Create('Order already submitted');

  if FLines.Count = 0 then
    raise Exception.Create('Cannot submit empty order');

  FStatus := osSubmitted;
end;

procedure TOrder.Cancel;
begin
  if FStatus in [osShipped, osDelivered] then
    raise Exception.Create('Cannot cancel shipped/delivered order');

  FStatus := osCancelled;
end;
```

**Utilisation :**
```pascal
var
  Order: TOrder;
begin
  Order := TOrder.Create(NewOrderID, CustomerID);
  try
    // ✅ Contrôle total par la racine
    Order.AddLine(ProductID1, 2, 49.99);
    Order.AddLine(ProductID2, 1, 29.99);

    // ✅ Modifications contrôlées
    Order.UpdateLineQuantity(ProductID1, 3);

    // ✅ Validation automatique
    Order.Submit;

    // ❌ Impossible de modifier après soumission
    // Order.AddLine(...);  // Exception !

    FOrderRepository.Save(Order);
  finally
    Order.Free;
  end;
end;
```

**Bénéfices :**
- Cohérence garantie
- Règles métier centralisées
- Impossible d'avoir des états invalides
- Transactions simplifiées

### 6. Repositories (Dépôts)

Un **Repository** fait abstraction de la persistance. Il donne l'impression de travailler avec une collection d'objets en mémoire.

**Principe :** Le domaine ne doit pas connaître la base de données.

**Interface du Repository :**
```pascal
type
  IOrderRepository = interface
    ['{GUID}']
    function GetByID(ID: TOrderID): TOrder;
    function GetByCustomer(CustomerID: TCustomerID): TList<TOrder>;
    procedure Save(Order: TOrder);
    procedure Delete(Order: TOrder);
  end;
```

**Implémentation :**
```pascal
type
  TOrderRepository = class(TInterfacedObject, IOrderRepository)
  private
    FConnection: TFDConnection;
    function MapDataSetToOrder(DS: TDataSet): TOrder;
    procedure MapOrderToDataSet(Order: TOrder; DS: TDataSet);
  public
    constructor Create(Connection: TFDConnection);
    function GetByID(ID: TOrderID): TOrder;
    function GetByCustomer(CustomerID: TCustomerID): TList<TOrder>;
    procedure Save(Order: TOrder);
    procedure Delete(Order: TOrder);
  end;

implementation

constructor TOrderRepository.Create(Connection: TFDConnection);
begin
  inherited Create;
  FConnection := Connection;
end;

function TOrderRepository.GetByID(ID: TOrderID): TOrder;
var
  Query: TFDQuery;
begin
  Result := nil;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM orders WHERE id = :id';
    Query.ParamByName('id').AsString := GUIDToString(ID.Value);
    Query.Open;

    if not Query.IsEmpty then
      Result := MapDataSetToOrder(Query);
  finally
    Query.Free;
  end;
end;

function TOrderRepository.GetByCustomer(CustomerID: TCustomerID): TList<TOrder>;
var
  Query: TFDQuery;
  Order: TOrder;
begin
  Result := TList<TOrder>.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT * FROM orders WHERE customer_id = :customer_id';
    Query.ParamByName('customer_id').AsString := GUIDToString(CustomerID.Value);
    Query.Open;

    while not Query.Eof do
    begin
      Order := MapDataSetToOrder(Query);
      Result.Add(Order);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TOrderRepository.Save(Order: TOrder);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Vérifier si existe
    Query.SQL.Text := 'SELECT COUNT(*) FROM orders WHERE id = :id';
    Query.ParamByName('id').AsString := GUIDToString(Order.ID.Value);
    Query.Open;

    if Query.Fields[0].AsInteger > 0 then
    begin
      // UPDATE
      Query.Close;
      Query.SQL.Text := 'UPDATE orders SET status = :status, total = :total ' +
                        'WHERE id = :id';
    end
    else
    begin
      // INSERT
      Query.Close;
      Query.SQL.Text := 'INSERT INTO orders (id, customer_id, status, total) ' +
                        'VALUES (:id, :customer_id, :status, :total)';
    end;

    MapOrderToDataSet(Order, Query);
    Query.ExecSQL;

    // Sauvegarder les lignes aussi...
  finally
    Query.Free;
  end;
end;

procedure TOrderRepository.Delete(Order: TOrder);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'DELETE FROM orders WHERE id = :id';
    Query.ParamByName('id').AsString := GUIDToString(Order.ID.Value);
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TOrderRepository.MapDataSetToOrder(DS: TDataSet): TOrder;
var
  OrderID: TOrderID;
  CustomerID: TCustomerID;
begin
  OrderID.Value := StringToGUID(DS.FieldByName('id').AsString);
  CustomerID.Value := StringToGUID(DS.FieldByName('customer_id').AsString);

  Result := TOrder.Create(OrderID, CustomerID);
  // Charger les autres propriétés...
  // Charger les lignes...
end;

procedure TOrderRepository.MapOrderToDataSet(Order: TOrder; DS: TDataSet);
begin
  DS.ParamByName('id').AsString := GUIDToString(Order.ID.Value);
  DS.ParamByName('status').AsInteger := Ord(Order.Status);
  DS.ParamByName('total').AsCurrency := Order.Total;
  // etc.
end;
```

**Utilisation depuis le domaine :**
```pascal
var
  OrderRepo: IOrderRepository;
  Order: TOrder;
begin
  OrderRepo := TOrderRepository.Create(Connection);

  // Comme si c'était une collection en mémoire !
  Order := OrderRepo.GetByID(OrderID);
  Order.AddLine(ProductID, 2, 49.99);
  OrderRepo.Save(Order);

  // Le domaine ne sait pas que c'est une base de données
end;
```

### 7. Domain Services (Services du Domaine)

Un **Domain Service** contient de la logique métier qui ne rentre pas naturellement dans une Entité ou un Value Object.

**Quand utiliser un Domain Service ?**
- L'opération implique plusieurs Aggregates
- L'opération ne concerne pas naturellement une entité particulière
- C'est un processus métier, pas une propriété d'un objet

**Exemple : Transfert d'argent entre comptes**
```pascal
type
  // Entités
  TAccount = class
  private
    FID: TAccountID;
    FBalance: TMoney;
  public
    procedure Debit(Amount: TMoney);
    procedure Credit(Amount: TMoney);
    property ID: TAccountID read FID;
    property Balance: TMoney read FBalance;
  end;

  // Domain Service
  IMoneyTransferService = interface
    ['{GUID}']
    function Transfer(
      FromAccount: TAccount;
      ToAccount: TAccount;
      Amount: TMoney
    ): TTransferResult;
  end;

  TMoneyTransferService = class(TInterfacedObject, IMoneyTransferService)
  public
    function Transfer(
      FromAccount: TAccount;
      ToAccount: TAccount;
      Amount: TMoney
    ): TTransferResult;
  end;

implementation

// On ne peut pas mettre cette logique dans TAccount
// car elle concerne DEUX comptes à la fois !

function TMoneyTransferService.Transfer(
  FromAccount: TAccount;
  ToAccount: TAccount;
  Amount: TMoney
): TTransferResult;
begin
  // Validation
  if FromAccount.Balance.Amount < Amount.Amount then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Insufficient funds';
    Exit;
  end;

  if Amount.Amount <= 0 then
  begin
    Result.Success := False;
    Result.ErrorMessage := 'Amount must be positive';
    Exit;
  end;

  // Transaction atomique
  try
    FromAccount.Debit(Amount);
    ToAccount.Credit(Amount);

    Result.Success := True;
    Result.TransactionID := GenerateTransactionID;
  except
    on E: Exception do
    begin
      Result.Success := False;
      Result.ErrorMessage := 'Transfer failed: ' + E.Message;
    end;
  end;
end;
```

### 8. Domain Events (Événements du Domaine)

Un **Domain Event** représente quelque chose qui s'est passé dans le domaine et qui peut intéresser d'autres parties du système.

**Caractéristiques :**
- Immutable
- Nom au passé (OrderPlaced, CustomerRegistered)
- Contient toutes les informations nécessaires

**Exemple :**
```pascal
type
  TDomainEvent = class abstract
  private
    FOccurredAt: TDateTime;
  public
    constructor Create;
    property OccurredAt: TDateTime read FOccurredAt;
  end;

  TOrderPlacedEvent = class(TDomainEvent)
  private
    FOrderID: TOrderID;
    FCustomerID: TCustomerID;
    FTotalAmount: Currency;
  public
    constructor Create(OrderID: TOrderID; CustomerID: TCustomerID; TotalAmount: Currency);
    property OrderID: TOrderID read FOrderID;
    property CustomerID: TCustomerID read FCustomerID;
    property TotalAmount: Currency read FTotalAmount;
  end;

implementation

constructor TDomainEvent.Create;
begin
  inherited Create;
  FOccurredAt := Now;
end;

constructor TOrderPlacedEvent.Create(OrderID: TOrderID; CustomerID: TCustomerID; TotalAmount: Currency);
begin
  inherited Create;
  FOrderID := OrderID;
  FCustomerID := CustomerID;
  FTotalAmount := TotalAmount;
end;
```

**Dans l'Aggregate :**
```pascal
type
  TOrder = class
  private
    FDomainEvents: TList<TDomainEvent>;
  public
    constructor Create(ID: TOrderID; CustomerID: TCustomerID);
    destructor Destroy; override;

    procedure Submit;

    function GetDomainEvents: TArray<TDomainEvent>;
    procedure ClearDomainEvents;
  end;

implementation

procedure TOrder.Submit;
var
  Event: TOrderPlacedEvent;
begin
  if FStatus <> osCreated then
    raise Exception.Create('Order already submitted');

  FStatus := osSubmitted;

  // Lever l'événement
  Event := TOrderPlacedEvent.Create(FID, FCustomerID, FTotal);
  FDomainEvents.Add(Event);
end;

function TOrder.GetDomainEvents: TArray<TDomainEvent>;
begin
  Result := FDomainEvents.ToArray;
end;

procedure TOrder.ClearDomainEvents;
begin
  FDomainEvents.Clear;
end;
```

**Handlers d'événements :**
```pascal
type
  IEventHandler<T: TDomainEvent> = interface
    ['{GUID}']
    procedure Handle(Event: T);
  end;

  TOrderPlacedEventHandler = class(TInterfacedObject, IEventHandler<TOrderPlacedEvent>)
  public
    procedure Handle(Event: TOrderPlacedEvent);
  end;

implementation

procedure TOrderPlacedEventHandler.Handle(Event: TOrderPlacedEvent);
begin
  // Envoyer email de confirmation
  WriteLn('Sending confirmation email for order: ', GUIDToString(Event.OrderID.Value));

  // Mettre à jour les statistiques
  WriteLn('Updating sales statistics');

  // Notifier l'entrepôt
  WriteLn('Notifying warehouse');
end;
```

## Exemple complet : Système de réservation

Construisons un système de réservation d'hôtel avec DDD.

### Ubiquitous Language

Vocabulaire métier :
- **Guest** (Invité) : Personne qui réserve
- **Room** (Chambre) : Chambre d'hôtel
- **Booking** (Réservation) : Réservation d'une chambre
- **Check-in** (Enregistrement) : Arrivée du client
- **Check-out** (Départ) : Départ du client

### Value Objects

```pascal
unit Domain.ValueObjects;

interface

type
  TGuestName = record
  private
    FFirstName: string;
    FLastName: string;
  public
    constructor Create(const FirstName, LastName: string);
    function GetFullName: string;
    property FirstName: string read FFirstName;
    property LastName: string read FLastName;
  end;

  TEmailAddress = record
  private
    FValue: string;
  public
    constructor Create(const Value: string);
    class function TryCreate(const Value: string; out Email: TEmailAddress): Boolean; static;
    property Value: string read FValue;
  end;

  TDateRange = record
  private
    FCheckIn: TDate;
    FCheckOut: TDate;
  public
    constructor Create(CheckIn, CheckOut: TDate);
    function GetNights: Integer;
    function Contains(Date: TDate): Boolean;
    function Overlaps(Other: TDateRange): Boolean;
    property CheckIn: TDate read FCheckIn;
    property CheckOut: TDate read FCheckOut;
  end;

  TMoney = record
  private
    FAmount: Currency;
    FCurrencyCode: string;
  public
    constructor Create(Amount: Currency; const CurrencyCode: string);
    function Add(Other: TMoney): TMoney;
    function Multiply(Factor: Double): TMoney;
    property Amount: Currency read FAmount;
    property CurrencyCode: string read FCurrencyCode;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TGuestName }

constructor TGuestName.Create(const FirstName, LastName: string);
begin
  if Trim(FirstName) = '' then
    raise Exception.Create('First name is required');
  if Trim(LastName) = '' then
    raise Exception.Create('Last name is required');

  FFirstName := Trim(FirstName);
  FLastName := Trim(LastName);
end;

function TGuestName.GetFullName: string;
begin
  Result := FFirstName + ' ' + FLastName;
end;

{ TEmailAddress }

constructor TEmailAddress.Create(const Value: string);
begin
  if not TryCreate(Value, Self) then
    raise Exception.Create('Invalid email address');
end;

class function TEmailAddress.TryCreate(const Value: string; out Email: TEmailAddress): Boolean;
begin
  Result := (Trim(Value) <> '') and Value.Contains('@');
  if Result then
    Email.FValue := LowerCase(Trim(Value));
end;

{ TDateRange }

constructor TDateRange.Create(CheckIn, CheckOut: TDate);
begin
  if CheckIn >= CheckOut then
    raise Exception.Create('Check-out must be after check-in');
  if CheckIn < Date then
    raise Exception.Create('Check-in cannot be in the past');

  FCheckIn := CheckIn;
  FCheckOut := CheckOut;
end;

function TDateRange.GetNights: Integer;
begin
  Result := DaysBetween(FCheckOut, FCheckIn);
end;

function TDateRange.Contains(Date: TDate): Boolean;
begin
  Result := (Date >= FCheckIn) and (Date < FCheckOut);
end;

function TDateRange.Overlaps(Other: TDateRange): Boolean;
begin
  Result := (FCheckIn < Other.CheckOut) and (FCheckOut > Other.CheckIn);
end;

{ TMoney }

constructor TMoney.Create(Amount: Currency; const CurrencyCode: string);
begin
  if Amount < 0 then
    raise Exception.Create('Amount cannot be negative');
  FAmount := Amount;
  FCurrencyCode := UpperCase(CurrencyCode);
end;

function TMoney.Add(Other: TMoney): TMoney;
begin
  if FCurrencyCode <> Other.CurrencyCode then
    raise Exception.Create('Cannot add different currencies');
  Result := TMoney.Create(FAmount + Other.Amount, FCurrencyCode);
end;

function TMoney.Multiply(Factor: Double): TMoney;
begin
  Result := TMoney.Create(FAmount * Factor, FCurrencyCode);
end;

end.
```

### Entities

```pascal
unit Domain.Entities;

interface

uses
  Domain.ValueObjects, System.Generics.Collections;

type
  TGuestID = record
    Value: TGUID;
  end;

  TRoomID = record
    Value: Integer;
  end;

  TBookingID = record
    Value: TGUID;
  end;

  TRoomType = (rtStandard, rtDeluxe, rtSuite);

  TBookingStatus = (bsPending, bsConfirmed, bsCheckedIn, bsCheckedOut, bsCancelled);

  TGuest = class
  private
    FID: TGuestID;
    FName: TGuestName;
    FEmail: TEmailAddress;
    FPhone: string;
  public
    constructor Create(ID: TGuestID; Name: TGuestName; Email: TEmailAddress; const Phone: string);
    property ID: TGuestID read FID;
    property Name: TGuestName read FName;
    property Email: TEmailAddress read FEmail;
    property Phone: string read FPhone write FPhone;
  end;

  TRoom = class
  private
    FID: TRoomID;
    FNumber: string;
    FRoomType: TRoomType;
    FPricePerNight: TMoney;
    FMaxOccupancy: Integer;
  public
    constructor Create(ID: TRoomID; const Number: string; RoomType: TRoomType;
      PricePerNight: TMoney; MaxOccupancy: Integer);
    function CalculatePrice(Nights: Integer): TMoney;
    property ID: TRoomID read FID;
    property Number: string read FNumber;
    property RoomType: TRoomType read FRoomType;
    property PricePerNight: TMoney read FPricePerNight;
    property MaxOccupancy: Integer read FMaxOccupancy;
  end;

implementation

{ TGuest }

constructor TGuest.Create(ID: TGuestID; Name: TGuestName; Email: TEmailAddress; const Phone: string);
begin
  inherited Create;
  FID := ID;
  FName := Name;
  FEmail := Email;
  FPhone := Phone;
end;

{ TRoom }

constructor TRoom.Create(ID: TRoomID; const Number: string; RoomType: TRoomType;
  PricePerNight: TMoney; MaxOccupancy: Integer);
begin
  inherited Create;
  FID := ID;
  FNumber := Number;
  FRoomType := RoomType;
  FPricePerNight := PricePerNight;
  FMaxOccupancy := MaxOccupancy;
end;

function TRoom.CalculatePrice(Nights: Integer): TMoney;
begin
  Result := FPricePerNight.Multiply(Nights);
end;

end.
```

### Aggregate Root

```pascal
unit Domain.Aggregates;

interface

uses
  Domain.Entities, Domain.ValueObjects, Domain.Events,
  System.Generics.Collections;

type
  // Aggregate Root
  TBooking = class
  private
    FID: TBookingID;
    FGuestID: TGuestID;
    FRoomID: TRoomID;
    FDateRange: TDateRange;
    FStatus: TBookingStatus;
    FTotalPrice: TMoney;
    FCreatedAt: TDateTime;
    FDomainEvents: TList<TDomainEvent>;

    procedure AddDomainEvent(Event: TDomainEvent);
  public
    constructor Create(ID: TBookingID; GuestID: TGuestID; RoomID: TRoomID;
      DateRange: TDateRange; TotalPrice: TMoney);
    destructor Destroy; override;

    // Méthodes métier
    procedure Confirm;
    procedure CheckIn;
    procedure CheckOut;
    procedure Cancel(const Reason: string);

    // Validation métier
    function CanBeConfirmed: Boolean;
    function CanCheckIn: Boolean;
    function CanCheckOut: Boolean;
    function CanBeCancelled: Boolean;

    // Événements
    function GetDomainEvents: TArray<TDomainEvent>;
    procedure ClearDomainEvents;

    // Properties
    property ID: TBookingID read FID;
    property GuestID: TGuestID read FGuestID;
    property RoomID: TRoomID read FRoomID;
    property DateRange: TDateRange read FDateRange;
    property Status: TBookingStatus read FStatus;
    property TotalPrice: TMoney read FTotalPrice;
    property CreatedAt: TDateTime read FCreatedAt;
  end;

implementation

uses
  System.SysUtils;

constructor TBooking.Create(ID: TBookingID; GuestID: TGuestID; RoomID: TRoomID;
  DateRange: TDateRange; TotalPrice: TMoney);
begin
  inherited Create;
  FID := ID;
  FGuestID := GuestID;
  FRoomID := RoomID;
  FDateRange := DateRange;
  FTotalPrice := TotalPrice;
  FStatus := bsPending;
  FCreatedAt := Now;
  FDomainEvents := TList<TDomainEvent>.Create;
end;

destructor TBooking.Destroy;
begin
  FDomainEvents.Free;
  inherited;
end;

procedure TBooking.AddDomainEvent(Event: TDomainEvent);
begin
  FDomainEvents.Add(Event);
end;

function TBooking.CanBeConfirmed: Boolean;
begin
  Result := FStatus = bsPending;
end;

procedure TBooking.Confirm;
var
  Event: TBookingConfirmedEvent;
begin
  if not CanBeConfirmed then
    raise Exception.Create('Booking cannot be confirmed');

  FStatus := bsConfirmed;

  Event := TBookingConfirmedEvent.Create(FID, FGuestID, FRoomID, FDateRange);
  AddDomainEvent(Event);
end;

function TBooking.CanCheckIn: Boolean;
begin
  Result := (FStatus = bsConfirmed) and (Date >= FDateRange.CheckIn);
end;

procedure TBooking.CheckIn;
var
  Event: TGuestCheckedInEvent;
begin
  if not CanCheckIn then
    raise Exception.Create('Cannot check in');

  FStatus := bsCheckedIn;

  Event := TGuestCheckedInEvent.Create(FID, FGuestID, FRoomID);
  AddDomainEvent(Event);
end;

function TBooking.CanCheckOut: Boolean;
begin
  Result := FStatus = bsCheckedIn;
end;

procedure TBooking.CheckOut;
var
  Event: TGuestCheckedOutEvent;
begin
  if not CanCheckOut then
    raise Exception.Create('Cannot check out');

  FStatus := bsCheckedOut;

  Event := TGuestCheckedOutEvent.Create(FID, FGuestID, FRoomID);
  AddDomainEvent(Event);
end;

function TBooking.CanBeCancelled: Boolean;
begin
  Result := FStatus in [bsPending, bsConfirmed];
end;

procedure TBooking.Cancel(const Reason: string);
var
  Event: TBookingCancelledEvent;
begin
  if not CanBeCancelled then
    raise Exception.Create('Cannot cancel booking');

  FStatus := bsCancelled;

  Event := TBookingCancelledEvent.Create(FID, Reason);
  AddDomainEvent(Event);
end;

function TBooking.GetDomainEvents: TArray<TDomainEvent>;
begin
  Result := FDomainEvents.ToArray;
end;

procedure TBooking.ClearDomainEvents;
begin
  FDomainEvents.Clear;
end;

end.
```

### Domain Service

```pascal
unit Domain.Services;

interface

uses
  Domain.Aggregates, Domain.Entities, Domain.ValueObjects,
  System.Generics.Collections;

type
  IBookingService = interface
    ['{GUID}']
    function CreateBooking(Guest: TGuest; Room: TRoom; DateRange: TDateRange): TBooking;
    function IsRoomAvailable(RoomID: TRoomID; DateRange: TDateRange): Boolean;
  end;

  TBookingService = class(TInterfacedObject, IBookingService)
  private
    FBookingRepository: IBookingRepository;
  public
    constructor Create(BookingRepository: IBookingRepository);
    function CreateBooking(Guest: TGuest; Room: TRoom; DateRange: TDateRange): TBooking;
    function IsRoomAvailable(RoomID: TRoomID; DateRange: TDateRange): Boolean;
  end;

implementation

uses
  System.SysUtils;

constructor TBookingService.Create(BookingRepository: IBookingRepository);
begin
  inherited Create;
  FBookingRepository := BookingRepository;
end;

function TBookingService.IsRoomAvailable(RoomID: TRoomID; DateRange: TDateRange): Boolean;
var
  ExistingBookings: TList<TBooking>;
  Booking: TBooking;
begin
  Result := True;

  ExistingBookings := FBookingRepository.GetByRoomAndDateRange(RoomID, DateRange);
  try
    for Booking in ExistingBookings do
    begin
      if Booking.Status in [bsConfirmed, bsCheckedIn] then
      begin
        if Booking.DateRange.Overlaps(DateRange) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  finally
    ExistingBookings.Free;
  end;
end;

function TBookingService.CreateBooking(Guest: TGuest; Room: TRoom; DateRange: TDateRange): TBooking;
var
  BookingID: TBookingID;
  TotalPrice: TMoney;
begin
  // Vérifier disponibilité
  if not IsRoomAvailable(Room.ID, DateRange) then
    raise Exception.Create('Room is not available for selected dates');

  // Calculer le prix
  TotalPrice := Room.CalculatePrice(DateRange.GetNights);

  // Créer la réservation
  BookingID.Value := TGUID.NewGuid;
  Result := TBooking.Create(BookingID, Guest.ID, Room.ID, DateRange, TotalPrice);
end;

end.
```

### Repository

```pascal
unit Domain.Repositories;

interface

uses
  Domain.Aggregates, Domain.Entities, Domain.ValueObjects,
  System.Generics.Collections;

type
  IBookingRepository = interface
    ['{GUID}']
    function GetByID(ID: TBookingID): TBooking;
    function GetByGuest(GuestID: TGuestID): TList<TBooking>;
    function GetByRoomAndDateRange(RoomID: TRoomID; DateRange: TDateRange): TList<TBooking>;
    procedure Save(Booking: TBooking);
    procedure Delete(Booking: TBooking);
  end;

  // Implémentation avec FireDAC
  TBookingRepository = class(TInterfacedObject, IBookingRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(Connection: TFDConnection);
    function GetByID(ID: TBookingID): TBooking;
    function GetByGuest(GuestID: TGuestID): TList<TBooking>;
    function GetByRoomAndDateRange(RoomID: TRoomID; DateRange: TDateRange): TList<TBooking>;
    procedure Save(Booking: TBooking);
    procedure Delete(Booking: TBooking);
  end;

implementation

// Implémentation...

end.
```

### Utilisation

```pascal
program HotelBookingSystem;

uses
  Domain.Entities, Domain.Aggregates, Domain.ValueObjects,
  Domain.Services, Domain.Repositories;

var
  BookingService: IBookingService;
  BookingRepo: IBookingRepository;
  Guest: TGuest;
  Room: TRoom;
  Booking: TBooking;
  GuestName: TGuestName;
  Email: TEmailAddress;
  DateRange: TDateRange;
  Price: TMoney;
begin
  // Setup
  BookingRepo := TBookingRepository.Create(Connection);
  BookingService := TBookingService.Create(BookingRepo);

  // Créer un invité
  GuestName := TGuestName.Create('John', 'Doe');
  Email := TEmailAddress.Create('john.doe@example.com');
  Guest := TGuest.Create(NewGuestID, GuestName, Email, '+33612345678');

  // Créer une chambre
  Price := TMoney.Create(120.00, 'EUR');
  Room := TRoom.Create(NewRoomID, '101', rtDeluxe, Price, 2);

  // Créer une réservation
  DateRange := TDateRange.Create(Date + 7, Date + 10);  // 3 nuits

  try
    Booking := BookingService.CreateBooking(Guest, Room, DateRange);
    try
      // Confirmer
      Booking.Confirm;

      // Sauvegarder
      BookingRepo.Save(Booking);

      WriteLn('Booking created: ', GUIDToString(Booking.ID.Value));
      WriteLn('Total price: ', Booking.TotalPrice.Amount:0:2, ' ', Booking.TotalPrice.CurrencyCode);

      // Traiter les événements
      ProcessDomainEvents(Booking.GetDomainEvents);
      Booking.ClearDomainEvents;
    finally
      Booking.Free;
    end;
  except
    on E: Exception do
      WriteLn('Error: ', E.Message);
  end;
end.
```

## Avantages et inconvénients du DDD

### Avantages

✅ **Compréhension partagée** : Tout le monde parle le même langage

✅ **Code expressif** : Le code raconte l'histoire du métier

✅ **Flexibilité** : Facile d'adapter aux changements métier

✅ **Testabilité** : Logique métier isolée et testable

✅ **Maintenance** : Code organisé autour du métier, pas de la technique

✅ **Longévité** : Le métier évolue lentement, le code reste pertinent

### Inconvénients

❌ **Courbe d'apprentissage** : Concepts nouveaux à maîtriser

❌ **Plus de code** : Plus de classes que du CRUD simple

❌ **Collaboration nécessaire** : Besoin d'accès aux experts métier

❌ **Overkill pour le simple** : Trop pour un petit CRUD

❌ **Temps initial** : Setup plus long qu'une approche rapide

## Checklist DDD

### Ubiquitous Language
- [ ] Les termes du code correspondent aux termes métier
- [ ] Pas de traduction technique/métier
- [ ] Vocabulaire documenté et partagé

### Bounded Context
- [ ] Frontières de contexte clairement définies
- [ ] Modèles séparés par contexte
- [ ] Pas de fuite de concepts entre contextes

### Entities
- [ ] Ont une identité unique
- [ ] Égalité basée sur l'ID
- [ ] Comportement métier inclus

### Value Objects
- [ ] Pas d'identité
- [ ] Immutables
- [ ] Validation dans le constructeur
- [ ] Égalité basée sur les valeurs

### Aggregates
- [ ] Accès via la racine uniquement
- [ ] Cohérence maintenue par la racine
- [ ] Règles métier dans l'agrégat

### Repositories
- [ ] Un par Aggregate Root
- [ ] Interface dans le domaine
- [ ] Implémentation dans l'infrastructure
- [ ] API collection-like

### Domain Services
- [ ] Utilisés quand la logique ne rentre pas dans une entité
- [ ] Stateless
- [ ] Nom métier explicite

### Domain Events
- [ ] Nom au passé
- [ ] Immutables
- [ ] Contiennent toutes les infos nécessaires

## Conclusion

Le Domain-Driven Design n'est pas une silver bullet, mais une approche puissante pour les applications avec une complexité métier importante.

**Points clés à retenir :**

1. **Le domaine au centre** : Le code reflète le métier, pas la technique

2. **Ubiquitous Language** : Un vocabulaire partagé par toute l'équipe

3. **Building blocks** : Entities, Value Objects, Aggregates, Services, Repositories, Events

4. **Bounded Context** : Des frontières claires entre les modèles

5. **Collaboration** : DDD nécessite une étroite collaboration avec le métier

6. **Pas pour tout** : Utilisez DDD quand la complexité le justifie

**Quand appliquer DDD avec Delphi ?**

✅ Applications d'entreprise complexes
✅ Règles métier riches et changeantes
✅ Projets à long terme
✅ Équipe avec accès aux experts métier

❌ CRUD simples
❌ Prototypes rapides
❌ Applications sans logique métier
❌ Projets jetables

**Commencez petit :**
1. Identifiez votre domaine métier principal
2. Créez le vocabulaire (Ubiquitous Language)
3. Modélisez quelques Entities et Value Objects
4. Créez un Aggregate simple
5. Ajoutez un Repository
6. Itérez et affinez

**Citation finale :**

> "Le cœur du logiciel est sa capacité à résoudre des problèmes liés au domaine pour son utilisateur"
> — Eric Evans, Domain-Driven Design

Le DDD transforme votre façon de penser le développement logiciel : du technique vers le métier, de la base de données vers le domaine, de la solution technique vers la solution business.

---

**Ressources pour aller plus loin :**

- **Livre fondateur** : "Domain-Driven Design" d'Eric Evans
- **Livre pratique** : "Implementing Domain-Driven Design" de Vaughn Vernon
- **Site** : https://www.domainlanguage.com/
- **Communauté** : DDD Community (forums, meetups)

Bon DDD avec Delphi ! 🎯

⏭️ [Microservices et architecture distribuée](/18-architecture-et-bonnes-pratiques/11-microservices-et-architecture-distribuee.md)
