# 18.7 Revue de code et refactoring

## Introduction

Imaginez que vous venez de terminer d'écrire un magnifique morceau de code qui fonctionne parfaitement. Mission accomplie ? Pas tout à fait ! Même le code qui fonctionne correctement peut souvent être amélioré en termes de lisibilité, de performance ou de maintenabilité. C'est là qu'interviennent la revue de code et le refactoring.

Dans ce chapitre, nous allons explorer ces deux pratiques essentielles qui distinguent les développeurs professionnels des amateurs. Vous apprendrez comment évaluer votre code de manière critique et comment le transformer méthodiquement pour en faire un code plus propre et plus robuste, sans changer son comportement externe.

## Qu'est-ce que la revue de code ?

La revue de code (code review) est un processus systématique d'examen du code source par d'autres développeurs ou par vous-même. L'objectif n'est pas simplement de trouver des bugs, mais aussi d'améliorer la qualité globale du code, d'assurer la cohérence avec les standards du projet, et de partager les connaissances entre les membres de l'équipe.

### Pourquoi faire des revues de code ?

1. **Détection précoce des bugs** : Identifiez les problèmes avant qu'ils n'atteignent la production
2. **Amélioration de la qualité** : Assurez-vous que le code est lisible, maintenable et efficace
3. **Partage des connaissances** : Apprenez de nouvelles techniques et partagez votre expertise
4. **Cohérence du code** : Maintenez un style et une approche uniformes dans tout le projet
5. **Formation continue** : Améliorez constamment vos compétences en recevant des retours constructifs

## Qu'est-ce que le refactoring ?

Le refactoring est le processus de restructuration du code existant sans changer son comportement externe. En d'autres termes, le code fait toujours la même chose, mais il est organisé différemment, généralement pour être plus clair, plus simple, ou plus efficace.

### Pourquoi refactoriser le code ?

1. **Améliorer la lisibilité** : Rendre le code plus facile à comprendre
2. **Faciliter la maintenance** : Simplifier les modifications futures
3. **Réduire la complexité** : Décomposer des blocs de code complexes en parties plus simples
4. **Éliminer la duplication** : Appliquer le principe DRY (Don't Repeat Yourself)
5. **Optimiser les performances** : Rendre le code plus rapide ou moins gourmand en ressources
6. **Préparer l'évolution** : Faciliter l'ajout de nouvelles fonctionnalités

## Types de revues de code

### 1. Auto-revue

L'auto-revue consiste à relire et évaluer votre propre code. C'est une bonne pratique à adopter avant de soumettre votre code à d'autres personnes.

**Conseils pour l'auto-revue :**
- Prenez du recul : attendez quelques heures après avoir écrit le code avant de le relire
- Lisez le code ligne par ligne, comme si vous le voyiez pour la première fois
- Utilisez une checklist de vérification (nous en verrons une plus loin)
- Exécutez des tests unitaires pour vérifier la fonctionnalité

### 2. Revue par un pair (Peer Review)

La revue par un pair implique qu'un autre développeur examine votre code. C'est la forme la plus courante de revue de code dans les équipes.

**Processus typique :**
1. Le développeur termine une fonctionnalité et la soumet pour revue
2. Un ou plusieurs autres développeurs examinent le code
3. Les réviseurs laissent des commentaires et suggestions
4. Le développeur initial apporte les modifications nécessaires
5. Le cycle continue jusqu'à ce que le code soit approuvé

### 3. Revue d'équipe (Team Review)

Les revues d'équipe impliquent plusieurs développeurs examinant le code ensemble, souvent lors d'une réunion.

**Avantages :**
- Discussions en temps réel
- Partage de connaissances à grande échelle
- Opportunité d'apprentissage pour toute l'équipe

### 4. Revue assistée par outils

Les outils automatisés peuvent compléter les revues humaines en détectant automatiquement certains problèmes.

**Outils pour Delphi :**
- [Delphi Code Analyzer](https://sourceforge.net/projects/dca/)
- [Pascal Analyzer](https://www.peganza.com/products_pal.html)
- [CodeHealer](https://www.codehealer.com/)
- [SonarQube](https://www.sonarqube.org/) (avec plugin Pascal)

## Checklist pour la revue de code Delphi

Voici une checklist utile pour vos revues de code Delphi :

### Lisibilité et style
- [ ] Le code suit-il les conventions de nommage du projet ?
- [ ] Les noms sont-ils significatifs et descriptifs ?
- [ ] L'indentation et le formatage sont-ils cohérents ?
- [ ] Les commentaires sont-ils utiles et à jour ?
- [ ] Le code est-il facile à comprendre au premier coup d'œil ?

### Structure et conception
- [ ] Les fonctions et méthodes ont-elles une seule responsabilité ?
- [ ] La longueur des méthodes est-elle raisonnable (< 50 lignes) ?
- [ ] Les interfaces sont-elles bien définies et cohérentes ?
- [ ] Les principes SOLID sont-ils respectés ?
- [ ] Y a-t-il des duplications de code qui pourraient être factorisées ?

### Gestion des ressources
- [ ] Toutes les ressources allouées sont-elles libérées correctement ?
- [ ] Les objets sont-ils correctement créés et détruits ?
- [ ] Les blocs try-finally sont-ils utilisés pour protéger les ressources ?

### Gestion des erreurs
- [ ] Les exceptions sont-elles traitées de manière appropriée ?
- [ ] Les messages d'erreur sont-ils clairs et utiles ?
- [ ] Les cas limites et conditions d'erreur sont-ils gérés ?

### Performance
- [ ] Y a-t-il des optimisations évidentes qui pourraient être appliquées ?
- [ ] Les opérations coûteuses sont-elles effectuées efficacement ?
- [ ] Les boucles et les structures de données sont-elles optimisées ?

### Tests
- [ ] Le code est-il couvert par des tests unitaires ?
- [ ] Les tests couvrent-ils les cas normaux et exceptionnels ?
- [ ] Les tests sont-ils faciles à comprendre et à maintenir ?

## Comment mener efficacement une revue de code

### 1. Définir un objectif clair

Avant de commencer une revue, soyez clair sur ce que vous recherchez. Est-ce la sécurité, la performance, la lisibilité, ou tout à la fois ?

### 2. Se concentrer sur le code, pas sur la personne

Les commentaires devraient toujours porter sur le code, jamais sur le développeur. Utilisez "ce code pourrait être amélioré" plutôt que "tu as mal codé cette partie".

### 3. Être constructif et proposer des solutions

Ne vous contentez pas de pointer les problèmes, suggérez des améliorations :

❌ "Cette boucle est inefficace."
✅ "Cette boucle pourrait être plus efficace en utilisant un TDictionary au lieu de recherches linéaires répétées."

### 4. Hiérarchiser les commentaires

Tous les problèmes n'ont pas la même importance. Indiquez clairement ce qui est critique et ce qui est juste une suggestion d'amélioration.

### 5. Vérifier la compréhension

Assurez-vous que vos commentaires sont bien compris. Posez des questions plutôt que de faire des affirmations quand vous n'êtes pas sûr du contexte.

### 6. Limiter l'étendue de la revue

Une revue trop longue devient inefficace. Visez des sessions de 200-400 lignes de code maximum à la fois.

## Techniques de refactoring communes

Voyons maintenant quelques techniques de refactoring que vous pouvez appliquer à votre code Delphi.

### 1. Extraction de méthode

Cette technique consiste à prendre un fragment de code et à le transformer en méthode séparée.

**Avant :**
```pascal
procedure TCustomerManager.ProcessCustomer(Customer: TCustomer);
begin
  // Validation du client
  if Customer.Name = '' then
    raise EValidationError.Create('Le nom du client est requis');
  if not IsValidEmail(Customer.Email) then
    raise EValidationError.Create('Email client invalide');

  // Traitement du client
  Customer.LastProcessed := Now;
  if Customer.IsVIP then
    ApplyVIPDiscount(Customer)
  else
    ApplyStandardRates(Customer);

  // Enregistrement en base de données
  Database.BeginTransaction;
  try
    Database.UpdateCustomer(Customer);
    Database.UpdateLogs('Client traité: ' + Customer.Name);
    Database.Commit;
  except
    Database.Rollback;
    raise;
  end;
end;
```

**Après :**
```pascal
procedure TCustomerManager.ProcessCustomer(Customer: TCustomer);
begin
  ValidateCustomer(Customer);
  ApplyBusinessRules(Customer);
  SaveCustomerToDatabase(Customer);
end;

procedure TCustomerManager.ValidateCustomer(Customer: TCustomer);
begin
  if Customer.Name = '' then
    raise EValidationError.Create('Le nom du client est requis');
  if not IsValidEmail(Customer.Email) then
    raise EValidationError.Create('Email client invalide');
end;

procedure TCustomerManager.ApplyBusinessRules(Customer: TCustomer);
begin
  Customer.LastProcessed := Now;
  if Customer.IsVIP then
    ApplyVIPDiscount(Customer)
  else
    ApplyStandardRates(Customer);
end;

procedure TCustomerManager.SaveCustomerToDatabase(Customer: TCustomer);
begin
  Database.BeginTransaction;
  try
    Database.UpdateCustomer(Customer);
    Database.UpdateLogs('Client traité: ' + Customer.Name);
    Database.Commit;
  except
    Database.Rollback;
    raise;
  end;
end;
```

**Avantages :**
- Code plus lisible avec des blocs logiques clairement séparés
- Méthodes plus faciles à tester individuellement
- Réutilisation possible des méthodes extraites

### 2. Remplacement de condition par polymorphisme

Cette technique utilise le polymorphisme pour remplacer des structures conditionnelles complexes.

**Avant :**
```pascal
procedure ProcessPayment(Payment: TPayment);
begin
  case Payment.PaymentType of
    ptCreditCard:
      begin
        // Logique spécifique aux cartes de crédit
        ValidateCreditCardNumber(Payment.CardNumber);
        ChargeCard(Payment.CardNumber, Payment.Amount);
      end;
    ptBankTransfer:
      begin
        // Logique spécifique aux virements bancaires
        ValidateBankAccount(Payment.BankAccount);
        InitiateTransfer(Payment.BankAccount, Payment.Amount);
      end;
    ptPayPal:
      begin
        // Logique spécifique à PayPal
        ConnectToPayPal(Payment.PayPalEmail);
        RequestPayment(Payment.PayPalEmail, Payment.Amount);
      end;
  end;

  // Logique commune
  UpdateAccountBalance(Payment.CustomerID, Payment.Amount);
  SendConfirmationEmail(Payment.CustomerID, Payment.Amount);
end;
```

**Après :**
```pascal
// Dans l'unité d'interface
type
  TPayment = class
  protected
    FAmount: Currency;
    FCustomerID: Integer;
  public
    procedure Process; virtual; abstract;
    property Amount: Currency read FAmount write FAmount;
    property CustomerID: Integer read FCustomerID write FCustomerID;
  end;

  TCreditCardPayment = class(TPayment)
  private
    FCardNumber: string;
  public
    procedure Process; override;
    property CardNumber: string read FCardNumber write FCardNumber;
  end;

  TBankTransferPayment = class(TPayment)
  private
    FBankAccount: string;
  public
    procedure Process; override;
    property BankAccount: string read FBankAccount write FBankAccount;
  end;

  TPayPalPayment = class(TPayment)
  private
    FPayPalEmail: string;
  public
    procedure Process; override;
    property PayPalEmail: string read FPayPalEmail write FPayPalEmail;
  end;

// Dans l'unité d'implémentation
procedure TCreditCardPayment.Process;
begin
  // Logique spécifique aux cartes de crédit
  ValidateCreditCardNumber(CardNumber);
  ChargeCard(CardNumber, Amount);

  // Logique commune
  UpdateAccountBalance(CustomerID, Amount);
  SendConfirmationEmail(CustomerID, Amount);
end;

procedure TBankTransferPayment.Process;
begin
  // Logique spécifique aux virements bancaires
  ValidateBankAccount(BankAccount);
  InitiateTransfer(BankAccount, Amount);

  // Logique commune
  UpdateAccountBalance(CustomerID, Amount);
  SendConfirmationEmail(CustomerID, Amount);
end;

procedure TPayPalPayment.Process;
begin
  // Logique spécifique à PayPal
  ConnectToPayPal(PayPalEmail);
  RequestPayment(PayPalEmail, Amount);

  // Logique commune
  UpdateAccountBalance(CustomerID, Amount);
  SendConfirmationEmail(CustomerID, Amount);
end;

// Utilisation
procedure ProcessPayment(Payment: TPayment);
begin
  Payment.Process;
end;
```

**Avantages :**
- Élimine les structures conditionnelles complexes
- Facilite l'ajout de nouveaux types de paiement sans modifier le code existant
- Rend le code plus conforme au principe ouvert/fermé (Open/Closed Principle)

### 3. Introduction d'un objet paramètre

Cette technique consiste à regrouper plusieurs paramètres en un seul objet.

**Avant :**
```pascal
procedure CreateInvoice(CustomerID: Integer; ProductIDs: TArray<Integer>;
  Quantities: TArray<Integer>; Prices: TArray<Currency>; DiscountPercent: Double;
  InvoiceDate: TDateTime; DueDate: TDateTime; Notes: string);
begin
  // Création de facture avec de nombreux paramètres
end;
```

**Après :**
```pascal
type
  TInvoiceData = class
    CustomerID: Integer;
    ProductIDs: TArray<Integer>;
    Quantities: TArray<Integer>;
    Prices: TArray<Currency>;
    DiscountPercent: Double;
    InvoiceDate: TDateTime;
    DueDate: TDateTime;
    Notes: string;
  end;

procedure CreateInvoice(InvoiceData: TInvoiceData);
begin
  // Création de facture avec un seul objet paramètre
end;
```

**Avantages :**
- Signatures de méthodes plus propres et plus lisibles
- Facilite l'ajout de nouveaux paramètres sans casser l'interface
- Améliore la maintenabilité du code

### 4. Remplacement de code temporaire par requête

Cette technique consiste à remplacer des variables temporaires par des méthodes qui calculent la valeur.

**Avant :**
```pascal
function TOrder.CalculateTotal: Currency;
var
  Subtotal: Currency;
  TaxAmount: Currency;
  DiscountAmount: Currency;
begin
  Subtotal := 0;
  for var Item in Items do
    Subtotal := Subtotal + (Item.Price * Item.Quantity);

  TaxAmount := Subtotal * TAX_RATE;

  if IsEligibleForDiscount then
    DiscountAmount := Subtotal * DISCOUNT_RATE
  else
    DiscountAmount := 0;

  Result := Subtotal + TaxAmount - DiscountAmount;
end;
```

**Après :**
```pascal
function TOrder.CalculateSubtotal: Currency;
begin
  Result := 0;
  for var Item in Items do
    Result := Result + (Item.Price * Item.Quantity);
end;

function TOrder.CalculateTaxAmount: Currency;
begin
  Result := CalculateSubtotal * TAX_RATE;
end;

function TOrder.CalculateDiscountAmount: Currency;
begin
  if IsEligibleForDiscount then
    Result := CalculateSubtotal * DISCOUNT_RATE
  else
    Result := 0;
end;

function TOrder.CalculateTotal: Currency;
begin
  Result := CalculateSubtotal + CalculateTaxAmount - CalculateDiscountAmount;
end;
```

**Avantages :**
- Code plus expressif et auto-documenté
- Chaque méthode a une responsabilité unique
- Facilite les tests unitaires
- Permet la réutilisation des calculs intermédiaires

### 5. Déplacement de méthode

Cette technique consiste à déplacer une méthode vers la classe où elle est le plus utilisée ou logiquement la plus appropriée.

**Avant :**
```pascal
// Dans TOrderProcessor
procedure TOrderProcessor.ValidateCustomer(Customer: TCustomer);
begin
  if Customer.Name = '' then
    raise EValidationError.Create('Nom de client requis');
  if not IsValidEmail(Customer.Email) then
    raise EValidationError.Create('Email invalide');
  if Customer.CreditLimit < 0 then
    raise EValidationError.Create('Limite de crédit invalide');
end;

// Utilisation
procedure TOrderProcessor.ProcessOrder(Order: TOrder);
begin
  ValidateCustomer(Order.Customer);
  // Suite du traitement
end;
```

**Après :**
```pascal
// Dans TCustomer
procedure TCustomer.Validate;
begin
  if Name = '' then
    raise EValidationError.Create('Nom de client requis');
  if not IsValidEmail(Email) then
    raise EValidationError.Create('Email invalide');
  if CreditLimit < 0 then
    raise EValidationError.Create('Limite de crédit invalide');
end;

// Utilisation dans TOrderProcessor
procedure TOrderProcessor.ProcessOrder(Order: TOrder);
begin
  Order.Customer.Validate;
  // Suite du traitement
end;
```

**Avantages :**
- Encapsulation améliorée : la validation est maintenant une responsabilité du client
- Cohérence : les méthodes sont placées dans les classes les plus appropriées
- Réutilisation : la validation peut être appelée de n'importe où

## Le processus de refactoring

Voici les étapes à suivre pour un refactoring réussi :

### 1. Identifiez les "mauvaises odeurs" (code smells)

Les "mauvaises odeurs" sont des signes que le code pourrait bénéficier d'un refactoring :

- **Méthode trop longue** : Plus de 30-50 lignes
- **Classe trop grande** : Trop de responsabilités
- **Liste de paramètres longue** : Plus de 3-4 paramètres
- **Duplication de code** : Même code à plusieurs endroits
- **Instructions conditionnelles complexes** : Nombreux if/else ou case imbriqués
- **Commentaires excessifs** : Souvent un signe que le code n'est pas assez clair
- **Classes fortement couplées** : Trop de dépendances entre classes

### 2. Assurez-vous d'avoir des tests

Avant de refactoriser, assurez-vous d'avoir des tests unitaires qui vérifient le comportement actuel du code. Cela vous permettra de vérifier que le refactoring n'a pas introduit de régression.

### 3. Procédez par petits pas

Ne refactorisez pas tout d'un coup. Faites de petits changements, exécutez les tests, puis passez au changement suivant.

### 4. Utilisez les outils de l'IDE Delphi

Delphi offre plusieurs outils pour faciliter le refactoring :

- **Renommage** : Renomme une variable, classe ou méthode partout où elle est utilisée (Ctrl+Shift+E)
- **Extraction de méthode** : Crée une nouvelle méthode à partir du code sélectionné
- **Introduction de variable** : Crée une variable pour une expression sélectionnée
- **Déplacement dans l'unité** : Déplace du code vers une autre unité

### 5. Vérifiez après chaque étape

Après chaque modification, vérifiez que :
- Le code compile sans erreurs
- Tous les tests passent
- Le comportement externe reste identique

## Exemple concret : Refactoring d'une classe Form

Voyons un exemple concret de refactoring d'une classe de formulaire Delphi.

### Version initiale (avant refactoring)

```pascal
unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Data.DB, FireDAC.Comp.Client;

type
  TFormMain = class(TForm)
    edtName: TEdit;
    edtEmail: TEdit;
    edtPhone: TEdit;
    btnSave: TButton;
    btnCancel: TButton;
    lblStatus: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FConnection := TFDConnection.Create(nil);
  FConnection.Params.Database := 'C:\Data\customers.db';
  FConnection.Params.DriverID := 'SQLite';
  FConnection.Connected := True;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FQuery.Free;
  FConnection.Free;
end;

procedure TFormMain.btnSaveClick(Sender: TObject);
var
  ErrorMsg: string;
begin
  // Validation
  ErrorMsg := '';
  if edtName.Text = '' then
    ErrorMsg := 'Name is required';
  if (ErrorMsg = '') and (edtEmail.Text = '') then
    ErrorMsg := 'Email is required';
  if (ErrorMsg = '') and (Pos('@', edtEmail.Text) = 0) then
    ErrorMsg := 'Invalid email format';

  if ErrorMsg <> '' then
  begin
    lblStatus.Caption := ErrorMsg;
    Exit;
  end;

  // Sauvegarde en base de données
  try
    FQuery.Close;
    FQuery.SQL.Text := 'INSERT INTO Customers (Name, Email, Phone) VALUES (:Name, :Email, :Phone)';
    FQuery.ParamByName('Name').AsString := edtName.Text;
    FQuery.ParamByName('Email').AsString := edtEmail.Text;
    FQuery.ParamByName('Phone').AsString := edtPhone.Text;
    FQuery.ExecSQL;

    lblStatus.Caption := 'Customer saved successfully';
    edtName.Text := '';
    edtEmail.Text := '';
    edtPhone.Text := '';
  except
    on E: Exception do
      lblStatus.Caption := 'Error: ' + E.Message;
  end;
end;

procedure TFormMain.btnCancelClick(Sender: TObject);
begin
  edtName.Text := '';
  edtEmail.Text := '';
  edtPhone.Text := '';
  lblStatus.Caption := '';
end;

end.
```

### Version refactorisée

```pascal
unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, CustomerData, CustomerModel;

type
  TFormMain = class(TForm)
    edtName: TEdit;
    edtEmail: TEdit;
    edtPhone: TEdit;
    btnSave: TButton;
    btnCancel: TButton;
    lblStatus: TLabel;
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCustomerData: TCustomerData;
    FCustomer: TCustomer;

    procedure UpdateUIFromCustomer;
    procedure UpdateCustomerFromUI;
    procedure ClearFields;
    procedure DisplayStatus(const Message: string);
    function ValidateCustomer: Boolean;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FCustomerData := TCustomerData.Create;
  FCustomer := TCustomer.Create;

  // Initialisation de l'interface
  ClearFields;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FCustomer.Free;
  FCustomerData.Free;
end;

procedure TFormMain.UpdateUIFromCustomer;
begin
  edtName.Text := FCustomer.Name;
  edtEmail.Text := FCustomer.Email;
  edtPhone.Text := FCustomer.Phone;
end;

procedure TFormMain.UpdateCustomerFromUI;
begin
  FCustomer.Name := edtName.Text;
  FCustomer.Email := edtEmail.Text;
  FCustomer.Phone := edtPhone.Text;
end;

procedure TFormMain.ClearFields;
begin
  edtName.Text := '';
  edtEmail.Text := '';
  edtPhone.Text := '';
  lblStatus.Caption := '';

  // Réinitialiser l'objet client
  FCustomer.Clear;
end;

procedure TFormMain.DisplayStatus(const Message: string);
begin
  lblStatus.Caption := Message;
end;

function TFormMain.ValidateCustomer: Boolean;
var
  ValidationResult: TValidationResult;
begin
  UpdateCustomerFromUI;

  ValidationResult := FCustomer.Validate;
  if not ValidationResult.IsValid then
  begin
    DisplayStatus(ValidationResult.ErrorMessage);
    Result := False;
  end
  else
    Result := True;
end;

procedure TFormMain.btnSaveClick(Sender: TObject);
begin
  if not ValidateCustomer then
    Exit;

  try
    if FCustomerData.SaveCustomer(FCustomer) then
    begin
      DisplayStatus('Customer saved successfully');
      ClearFields;
    end;
  except
    on E: Exception do
      DisplayStatus('Error: ' + E.Message);
  end;
end;

procedure TFormMain.btnCancelClick(Sender: TObject);
begin
  ClearFields;
end;

end.
```

### Unités supplémentaires créées pendant le refactoring

```pascal
// CustomerModel.pas
unit CustomerModel;

interface

uses
  System.SysUtils;

type
  TValidationResult = record
    IsValid: Boolean;
    ErrorMessage: string;

    class function CreateValid: TValidationResult; static;
    class function CreateInvalid(const ErrorMsg: string): TValidationResult; static;
  end;

  TCustomer = class
  private
    FID: Integer;
    FName: string;
    FEmail: string;
    FPhone: string;
  public
    procedure Clear;
    function Validate: TValidationResult;

    property ID: Integer read FID write FID;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Phone: string read FPhone write FPhone;
  end;

implementation

class function TValidationResult.CreateValid: TValidationResult;
begin
  Result.IsValid := True;
  Result.ErrorMessage := '';
end;

class function TValidationResult.CreateInvalid(const ErrorMsg: string): TValidationResult;
begin
  Result.IsValid := False;
  Result.ErrorMessage := ErrorMsg;
end;

procedure TCustomer.Clear;
begin
  FID := 0;
  FName := '';
  FEmail := '';
  FPhone := '';
end;

function TCustomer.Validate: TValidationResult;
begin
  if FName = '' then
    Exit(TValidationResult.CreateInvalid('Name is required'));

  if FEmail = '' then
    Exit(TValidationResult.CreateInvalid('Email is required'));

  if Pos('@', FEmail) = 0 then
    Exit(TValidationResult.CreateInvalid('Invalid email format'));

  Result := TValidationResult.CreateValid;
end;

end.

// CustomerData.pas
unit CustomerData;

interface

uses
  System.SysUtils, FireDAC.Comp.Client, CustomerModel;

type
  TCustomerData = class
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;

    procedure InitDatabase;
  public
    constructor Create;
    destructor Destroy; override;

    function SaveCustomer(Customer: TCustomer): Boolean;
    function LoadCustomer(ID: Integer; Customer: TCustomer): Boolean;
  end;

implementation

constructor TCustomerData.Create;
begin
  inherited Create;

  FConnection := TFDConnection.Create(nil);
  FQuery := TFDQuery.Create(nil);

  InitDatabase;
end;

destructor TCustomerData.Destroy;
begin
  FQuery.Free;
  FConnection.Free;

  inherited;
end;

procedure TCustomerData.InitDatabase;
begin
  FConnection.Params.Database := 'C:\Data\customers.db';
  FConnection.Params.DriverID := 'SQLite';
  FConnection.Connected := True;

  FQuery.Connection := FConnection;

  // Vérifier si la table existe, sinon la créer
  FQuery.SQL.Text := 'CREATE TABLE IF NOT EXISTS Customers (' +
                     'ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                     'Name TEXT NOT NULL, ' +
                     'Email TEXT NOT NULL, ' +
                     'Phone TEXT)';
  FQuery.ExecSQL;
end;

function TCustomerData.SaveCustomer(Customer: TCustomer): Boolean;
begin
  FQuery.Close;

  if Customer.ID > 0 then
  begin
    // Mise à jour d'un client existant
    FQuery.SQL.Text := 'UPDATE Customers SET Name = :Name, Email = :Email, ' +
                       'Phone = :Phone WHERE ID = :ID';
    FQuery.ParamByName('ID').AsInteger := Customer.ID;
  end
  else
  begin
    // Insertion d'un nouveau client
    FQuery.SQL.Text := 'INSERT INTO Customers (Name, Email, Phone) ' +
                       'VALUES (:Name, :Email, :Phone)';
  end;

  FQuery.ParamByName('Name').AsString := Customer.Name;
  FQuery.ParamByName('Email').AsString := Customer.Email;
  FQuery.ParamByName('Phone').AsString := Customer.Phone;

  FQuery.ExecSQL;

  if Customer.ID = 0 then
  begin
    // Récupérer l'ID généré pour un nouveau client
    FQuery.SQL.Text := 'SELECT last_insert_rowid()';
    FQuery.Open;
    Customer.ID := FQuery.Fields[0].AsInteger;
  end;

  Result := True;
end;

function TCustomerData.LoadCustomer(ID: Integer; Customer: TCustomer): Boolean;
begin
  FQuery.Close;
  FQuery.SQL.Text := 'SELECT * FROM Customers WHERE ID = :ID';
  FQuery.ParamByName('ID').AsInteger := ID;
  FQuery.Open;

  if FQuery.Eof then
  begin
    Result := False;
    Exit;
  end;

  Customer.ID := FQuery.FieldByName('ID').AsInteger;
  Customer.Name := FQuery.FieldByName('Name').AsString;
  Customer.Email := FQuery.FieldByName('Email').AsString;
  Customer.Phone := FQuery.FieldByName('Phone').AsString;

  Result := True;
end;

end.
```

## Améliorations apportées par le refactoring

Analysons les principales améliorations obtenues grâce à ce refactoring :

### 1. Séparation des responsabilités

- **Avant** : Le formulaire gérait à la fois l'interface utilisateur, la validation des données et l'accès à la base de données.
- **Après** : Nous avons séparé le code en trois entités distinctes :
  - `TFormMain` : Responsable uniquement de l'interface utilisateur
  - `TCustomer` : Modèle de données avec validation intégrée
  - `TCustomerData` : Gestion de l'accès aux données

### 2. Code plus maintenable

- Les méthodes sont plus courtes et ont une seule responsabilité
- Le code est organisé de manière logique
- L'ajout de nouvelles fonctionnalités (comme modifier un client existant) sera beaucoup plus simple

### 3. Réutilisabilité accrue

- Les classes `TCustomer` et `TCustomerData` peuvent être réutilisées dans d'autres formulaires
- La logique de validation est centralisée dans la classe modèle

### 4. Testabilité améliorée

- La logique métier peut être testée indépendamment de l'interface utilisateur
- On peut facilement écrire des tests unitaires pour `TCustomer` et `TCustomerData`

### 5. Meilleure gestion des erreurs

- La validation est structurée avec un type dédié `TValidationResult`
- Les messages d'erreur sont plus clairs et centralisés

### 6. Flexibilité pour l'évolution

- Si nous voulons changer la méthode de stockage (par exemple, passer de SQLite à MySQL), seule la classe `TCustomerData` devra être modifiée

## Outils de refactoring dans Delphi

Delphi offre plusieurs outils intégrés pour faciliter le refactoring de votre code.

### Refactorings disponibles via le menu contextuel

Cliquez avec le bouton droit sur un identifiant, puis sélectionnez "Refactorings" pour accéder aux options suivantes :

1. **Rename** (Ctrl+Shift+E) : Renomme une variable, un champ, une méthode ou une classe partout où elle est utilisée.

2. **Extract Method** : Crée une nouvelle méthode à partir du code sélectionné.

3. **Extract Resource String** : Extrait une chaîne de caractères dans le fichier de ressources pour l'internationalisation.

4. **Introduce Variable** : Crée une variable locale pour une expression sélectionnée.

5. **Find Unit** (Ctrl+Shift+A) : Trouve et ajoute l'unité nécessaire pour un type ou une fonction non déclarée.

6. **Declare Variable** : Crée automatiquement une déclaration de variable pour un identifiant non déclaré.

7. **Sync Modified Files** : Met à jour les fichiers d'interface (.h) pour les modifications dans les fichiers d'implémentation (.cpp) - pour C++Builder.

### Code Formatter (Ctrl+D)

L'outil de formatage automatique de Delphi permet d'appliquer un style cohérent à votre code. Vous pouvez personnaliser les règles de formatage dans les options de l'IDE.

### Audits et Métriques

Delphi intègre des outils d'analyse statique du code qui peuvent vous aider à identifier les zones à refactoriser :

1. **Code Audits** : Détecte les problèmes potentiels comme les variables non utilisées, les paramètres qui pourraient être const, etc.

2. **Code Metrics** : Mesure la complexité du code (nombre de lignes, profondeur d'imbrication, etc.)

## Approche pratique pour intégrer la revue de code et le refactoring

Voici une approche par étapes pour intégrer efficacement la revue de code et le refactoring dans votre processus de développement :

### 1. Planifiez des sessions régulières

- Programmez des revues de code hebdomadaires ou bihebdomadaires
- Alternez entre revues individuelles et revues d'équipe

### 2. Commencez petit

- Ne tentez pas de refactoriser toute votre application d'un coup
- Choisissez les zones les plus problématiques ou celles que vous modifiez souvent

### 3. Élaborez une checklist de revue

- Créez une checklist adaptée à votre projet et à votre équipe
- Mettez-la à jour régulièrement en fonction de votre expérience

### 4. Établissez des standards de code

- Définissez des règles de nommage, de structuration et de formatage
- Documentez ces standards et assurez-vous qu'ils sont accessibles à tous

### 5. Automatisez ce qui peut l'être

- Utilisez des outils d'analyse statique et de formatage automatique
- Intégrez ces outils à votre processus de build ou de commit

### 6. Mesurez les résultats

- Suivez des métriques comme le nombre de bugs trouvés pendant les revues
- Évaluez l'impact du refactoring sur la maintenance et l'évolution du code

## Conseils pour un refactoring sans douleur

### 1. Assurez la rétrocompatibilité

Lorsque vous refactorisez du code qui est utilisé par d'autres composants ou applications, assurez-vous de maintenir les interfaces publiques inchangées.

### 2. Versionnez avant de refactoriser

Faites toujours un commit de votre code avant de commencer un refactoring important, pour pouvoir revenir en arrière si nécessaire.

### 3. Testez, testez, testez

Exécutez vos tests unitaires après chaque étape de refactoring pour vous assurer que vous n'avez pas introduit de régression.

### 4. Documentez vos décisions

Pour les refactorings majeurs, documentez les raisons de vos choix et les alternatives que vous avez envisagées.

### 5. Communiquez avec l'équipe

Assurez-vous que toute l'équipe est au courant des refactorings importants, surtout ceux qui pourraient affecter leur travail.

## Signes qu'un code a besoin de refactoring

### 1. Rigidité

Le code est difficile à modifier. Une simple modification entraîne une cascade de changements dans des modules non liés.

### 2. Fragilité

Le code a tendance à se casser à plusieurs endroits lorsque vous le modifiez.

### 3. Immobilité

Le code contient des parties qui pourraient être utiles ailleurs, mais l'effort pour les extraire et les rendre réutilisables est trop important.

### 4. Viscosité

Il est plus facile de faire les choses de manière incorrecte que de suivre le design prévu.

### 5. Complexité inutile

Le code contient des éléments qui n'apportent pas de valeur immédiate.

### 6. Répétition

Le même code ou une variation légèrement différente apparaît à plusieurs endroits.

### 7. Opacité

Le code est difficile à comprendre.

## Application pratique : Refactoring d'un projet Delphi existant

Voici une approche pratique pour refactoriser un projet Delphi existant :

### Étape 1 : Évaluation initiale

1. Identifiez les zones problématiques (à partir de rapports de bugs, de métriques de code, etc.)
2. Établissez une liste prioritaire des modules à refactoriser
3. Définissez des objectifs clairs pour chaque refactoring

### Étape 2 : Préparation

1. Assurez-vous d'avoir une suite de tests couvrant les fonctionnalités à refactoriser
2. Si vous n'avez pas de tests, créez-en avant de commencer
3. Créez une branche dédiée dans votre système de contrôle de version

### Étape 3 : Refactoring par étapes

1. Commencez par des refactorings simples (renommage, extraction de méthode, etc.)
2. Testez après chaque modification
3. Committez fréquemment avec des messages descriptifs

### Étape 4 : Validation

1. Exécutez tous les tests pour vérifier qu'il n'y a pas de régression
2. Demandez une revue de code à vos collègues
3. Vérifiez que les objectifs initiaux ont été atteints

### Étape 5 : Finalisation

1. Mettez à jour la documentation si nécessaire
2. Fusionnez la branche de refactoring avec la branche principale
3. Partagez les enseignements tirés avec l'équipe

## Conclusion

La revue de code et le refactoring sont deux pratiques essentielles qui, lorsqu'elles sont correctement intégrées dans votre processus de développement, peuvent considérablement améliorer la qualité de votre code Delphi.

La revue de code vous permet de détecter les problèmes tôt, de partager les connaissances et de maintenir des standards élevés au sein de votre équipe. Le refactoring, quant à lui, vous aide à maintenir votre code propre, flexible et maintenable sur le long terme.

N'oubliez pas que ces pratiques ne sont pas des activités ponctuelles, mais des habitudes à cultiver continuellement. Avec le temps, vous développerez un instinct pour détecter le code qui a besoin d'être amélioré et vous serez plus à l'aise pour appliquer les techniques de refactoring appropriées.

Enfin, rappelez-vous que le but ultime n'est pas d'avoir un code parfait, mais un code suffisamment bon pour répondre aux besoins actuels tout en restant adaptable aux exigences futures. Comme l'a dit Kent Beck, un des pionniers du refactoring : "Faites-le marcher, faites-le bien, puis faites-le vite."
