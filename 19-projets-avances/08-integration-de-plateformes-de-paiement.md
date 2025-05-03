# 19.8 Intégration de plateformes de paiement

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'intégration des paiements en ligne dans vos applications Delphi est devenue une fonctionnalité essentielle pour les développeurs qui créent des logiciels de commerce, de gestion ou de services. Ce chapitre vous guidera à travers les étapes nécessaires pour connecter votre application Delphi à différentes plateformes de paiement.

## Sommaire
- [Présentation des plateformes de paiement](#présentation-des-plateformes-de-paiement)
- [Prérequis pour l'intégration](#prérequis-pour-lintégration)
- [Intégration avec Stripe](#intégration-avec-stripe)
- [Intégration avec PayPal](#intégration-avec-paypal)
- [Sécurisation des transactions](#sécurisation-des-transactions)
- [Gestion des webhooks](#gestion-des-webhooks)
- [Tests et environnement sandbox](#tests-et-environnement-sandbox)
- [Bonnes pratiques](#bonnes-pratiques)
- [Exemple de projet complet](#exemple-de-projet-complet)

## Présentation des plateformes de paiement

Les plateformes de paiement sont des services tiers qui facilitent les transactions financières en ligne. Elles permettent à votre application d'accepter des paiements par carte de crédit, virement bancaire et autres méthodes, sans avoir à gérer directement les informations sensibles des cartes bancaires.

Les plateformes les plus populaires incluent:
- Stripe
- PayPal
- Adyen
- Square
- Braintree
- Worldpay

Dans ce tutoriel, nous nous concentrerons principalement sur Stripe et PayPal qui offrent des API REST complètes et bien documentées, idéales pour l'intégration avec Delphi.

## Prérequis pour l'intégration

Avant de commencer l'intégration, vous aurez besoin de:

1. Un compte développeur sur la plateforme de paiement choisie
2. Les clés d'API (publique et privée) fournies par la plateforme
3. Les composants Delphi pour les requêtes REST (TRESTClient, TRESTRequest, TRESTResponse)
4. Une bibliothèque pour manipuler le JSON (intégrée dans Delphi)
5. Une compréhension basique du protocole HTTPS

Pour installer les composants nécessaires:
1. Dans l'IDE Delphi, ouvrez la palette de composants
2. Assurez-vous que l'onglet "REST Client" est disponible
3. Si ce n'est pas le cas, cliquez-droit sur la palette et sélectionnez "Installer les composants"
4. Recherchez et installez les composants REST Client

## Intégration avec Stripe

### 1. Configuration initiale

Commençons par créer une unité dédiée à l'intégration de Stripe:

```pascal
unit StripePaymentService;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Client, REST.Types, REST.Response.Adapter;

type
  TStripePaymentService = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FSecretKey: string;
    FPublicKey: string;
  public
    constructor Create(const ASecretKey, APublicKey: string);
    destructor Destroy; override;

    function CreatePaymentIntent(Amount: Integer; Currency: string;
                                Description: string): TJSONObject;
    function RetrievePayment(PaymentIntentId: string): TJSONObject;
    // Autres méthodes...
  end;

implementation

// Implémentation à suivre...

end.
```

### 2. Implémentation de la classe Stripe

```pascal
constructor TStripePaymentService.Create(const ASecretKey, APublicKey: string);
begin
  inherited Create;

  FSecretKey := ASecretKey;
  FPublicKey := APublicKey;

  // Initialisation des composants REST
  FRESTClient := TRESTClient.Create(nil);
  FRESTClient.BaseURL := 'https://api.stripe.com/v1';

  FRESTResponse := TRESTResponse.Create(nil);

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;

  // Configuration de l'authentification
  FRESTClient.Authenticator := THTTPBasicAuthenticator.Create(FSecretKey, '');
end;

destructor TStripePaymentService.Destroy;
begin
  FRESTRequest.Free;
  FRESTResponse.Free;
  FRESTClient.Free;

  inherited;
end;

function TStripePaymentService.CreatePaymentIntent(Amount: Integer;
                        Currency: string; Description: string): TJSONObject;
var
  RequestBody: TStringList;
begin
  RequestBody := TStringList.Create;
  try
    // Préparer les paramètres pour Stripe
    RequestBody.Add('amount=' + IntToStr(Amount));
    RequestBody.Add('currency=' + Currency);
    RequestBody.Add('description=' + Description);

    // Configurer la requête
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'payment_intents';
    FRESTRequest.Body.Add(RequestBody.Text, TRESTContentType.ctURLENCODED);

    // Exécuter la requête
    FRESTRequest.Execute;

    // Analyser la réponse
    if FRESTResponse.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject
    else
      raise Exception.CreateFmt('Erreur Stripe: %s', [FRESTResponse.Content]);

  finally
    RequestBody.Free;
  end;
end;
```

### 3. Utilisation dans votre application

Voici comment utiliser cette classe dans un formulaire:

```pascal
procedure TMainForm.btnPayWithStripeClick(Sender: TObject);
var
  StripeService: TStripePaymentService;
  Response: TJSONObject;
  ClientSecret: string;
begin
  StripeService := TStripePaymentService.Create('sk_test_YourSecretKey',
                                             'pk_test_YourPublicKey');
  try
    // Créer une intention de paiement (en centimes)
    Response := StripeService.CreatePaymentIntent(2000, 'eur',
                                               'Achat de produit XYZ');

    // Récupérer le client_secret pour l'autorisation côté client
    ClientSecret := Response.GetValue<string>('client_secret');

    // À ce stade, vous pouvez:
    // 1. Rediriger vers une page web de paiement Stripe
    // 2. Utiliser le SDK Stripe.js dans un WebView
    // 3. Utiliser d'autres méthodes de finalisation du paiement

    // Exemple de redirection vers Stripe Checkout
    ShellExecute(0, 'open', PChar('https://yourwebsite.com/checkout.html?client_secret='
                + ClientSecret), nil, nil, SW_SHOWNORMAL);

  finally
    Response.Free;
    StripeService.Free;
  end;
end;
```

## Intégration avec PayPal

### 1. Configuration initiale

Créons une classe similaire pour PayPal:

```pascal
unit PayPalPaymentService;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  REST.Client, REST.Types, REST.Response.Adapter;

type
  TPayPalPaymentService = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FClientID: string;
    FClientSecret: string;
    FAccessToken: string;
    FTokenExpiry: TDateTime;

    function GetAccessToken: string;
  public
    constructor Create(const AClientID, AClientSecret: string);
    destructor Destroy; override;

    function CreateOrder(Amount: Double; Currency: string): TJSONObject;
    function CapturePayment(OrderID: string): TJSONObject;
  end;

implementation

// Implémentation à suivre...

end.
```

### 2. Implémentation de la classe PayPal

```pascal
constructor TPayPalPaymentService.Create(const AClientID, AClientSecret: string);
begin
  inherited Create;

  FClientID := AClientID;
  FClientSecret := AClientSecret;

  // Initialisation des composants REST
  FRESTClient := TRESTClient.Create(nil);
  // Utiliser l'environnement sandbox pour les tests
  FRESTClient.BaseURL := 'https://api-m.sandbox.paypal.com';

  FRESTResponse := TRESTResponse.Create(nil);

  FRESTRequest := TRESTRequest.Create(nil);
  FRESTRequest.Client := FRESTClient;
  FRESTRequest.Response := FRESTResponse;
end;

function TPayPalPaymentService.GetAccessToken: string;
var
  Auth, Params: TStringList;
begin
  // Vérifier si le token est toujours valide
  if (FAccessToken <> '') and (Now < FTokenExpiry) then
    Exit(FAccessToken);

  // Créer les informations d'authentification
  Auth := TStringList.Create;
  Params := TStringList.Create;

  try
    // Configuration de la requête d'authentification
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'v1/oauth2/token';

    // Ajouter les paramètres
    Params.Add('grant_type=client_credentials');
    FRESTRequest.Body.Add(Params.Text, TRESTContentType.ctURLENCODED);

    // Configurer l'authentification Basic
    FRESTRequest.Client.Authenticator := THTTPBasicAuthenticator.Create(FClientID, FClientSecret);

    // Exécuter la requête
    FRESTRequest.Execute;

    // Analyser la réponse
    if FRESTResponse.StatusCode = 200 then
    begin
      var JSONResponse := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject;
      try
        FAccessToken := JSONResponse.GetValue<string>('access_token');
        // Définir l'expiration (habituellement 3600 secondes)
        FTokenExpiry := Now + (JSONResponse.GetValue<Integer>('expires_in') / 86400);
        Result := FAccessToken;
      finally
        JSONResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur PayPal Auth: %s', [FRESTResponse.Content]);

  finally
    Auth.Free;
    Params.Free;
    // Réinitialiser l'authentificateur pour les prochaines requêtes
    FRESTRequest.Client.Authenticator := nil;
  end;
end;

function TPayPalPaymentService.CreateOrder(Amount: Double; Currency: string): TJSONObject;
var
  RequestBody: TJSONObject;
  PurchaseUnits: TJSONArray;
  UnitObject, AmountObject: TJSONObject;
begin
  // Obtenir le token d'accès
  var Token := GetAccessToken;

  // Créer le corps de la requête en JSON
  RequestBody := TJSONObject.Create;
  PurchaseUnits := TJSONArray.Create;
  UnitObject := TJSONObject.Create;
  AmountObject := TJSONObject.Create;

  try
    // Configuration de la requête
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'v2/checkout/orders';

    // Ajouter l'en-tête d'autorisation
    FRESTRequest.Params.Clear;
    FRESTRequest.Params.AddHeader('Authorization', 'Bearer ' + Token);
    FRESTRequest.Params.AddHeader('Content-Type', 'application/json');

    // Construire le JSON de la requête
    AmountObject.AddPair('currency_code', Currency);
    AmountObject.AddPair('value', TJSONString.Create(FormatFloat('0.00', Amount)));

    UnitObject.AddPair('amount', AmountObject);
    PurchaseUnits.Add(UnitObject);

    RequestBody.AddPair('intent', 'CAPTURE');
    RequestBody.AddPair('purchase_units', PurchaseUnits);

    // Définir le corps de la requête
    FRESTRequest.Body.Add(RequestBody.ToJSON, TRESTContentType.ctAPPLICATION_JSON);

    // Exécuter la requête
    FRESTRequest.Execute;

    // Analyser la réponse
    if (FRESTResponse.StatusCode = 200) or (FRESTResponse.StatusCode = 201) then
      Result := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject
    else
      raise Exception.CreateFmt('Erreur PayPal: %s', [FRESTResponse.Content]);

  except
    RequestBody.Free;
    raise;
  end;
end;
```

### 3. Utilisation dans votre application

```pascal
procedure TMainForm.btnPayWithPayPalClick(Sender: TObject);
var
  PayPalService: TPayPalPaymentService;
  Response: TJSONObject;
  OrderID, ApprovalURL: string;
  Links: TJSONArray;
  i: Integer;
begin
  PayPalService := TPayPalPaymentService.Create('YOUR_CLIENT_ID', 'YOUR_CLIENT_SECRET');
  try
    // Créer une commande
    Response := PayPalService.CreateOrder(19.99, 'EUR');

    // Récupérer l'ID de commande
    OrderID := Response.GetValue<string>('id');

    // Trouver l'URL d'approbation
    Links := Response.GetValue<TJSONArray>('links');
    ApprovalURL := '';

    for i := 0 to Links.Count - 1 do
    begin
      var Link := Links.Items[i] as TJSONObject;
      if Link.GetValue<string>('rel') = 'approve' then
      begin
        ApprovalURL := Link.GetValue<string>('href');
        Break;
      end;
    end;

    // Rediriger l'utilisateur vers PayPal pour approbation
    if ApprovalURL <> '' then
      ShellExecute(0, 'open', PChar(ApprovalURL), nil, nil, SW_SHOWNORMAL);

    // Stockez OrderID dans votre base de données pour référence future

  finally
    Response.Free;
    PayPalService.Free;
  end;
end;
```

## Sécurisation des transactions

La sécurité est primordiale lors de l'intégration des paiements. Voici quelques mesures essentielles:

### 1. Stockage sécurisé des clés API

Ne jamais inclure directement vos clés API dans votre code source. Utilisez plutôt:

```pascal
// Créer une fonction pour charger les clés depuis un fichier chiffré
function LoadAPIKeys: TAPIKeys;
var
  Cipher: TCipher;
  EncryptedFile: TFileStream;
begin
  Cipher := TCipher.Create;
  EncryptedFile := TFileStream.Create('api_keys.dat', fmOpenRead);
  try
    // Déchiffrer le contenu
    Result.StripeSecretKey := Cipher.Decrypt(ReadStringFromStream(EncryptedFile));
    Result.StripePublicKey := Cipher.Decrypt(ReadStringFromStream(EncryptedFile));
    // etc.
  finally
    EncryptedFile.Free;
    Cipher.Free;
  end;
end;
```

### 2. Validation côté serveur

Toujours vérifier les paiements sur votre serveur:

```pascal
function TPaymentProcessor.VerifyPaymentStatus(OrderID: string): Boolean;
var
  PaymentService: TStripePaymentService;
  Response: TJSONObject;
  Status: string;
begin
  PaymentService := TStripePaymentService.Create(LoadAPIKeys.StripeSecretKey, '');
  try
    Response := PaymentService.RetrievePayment(OrderID);
    Status := Response.GetValue<string>('status');

    // Vérifier que le statut est bien 'succeeded'
    Result := (Status = 'succeeded');

  finally
    Response.Free;
    PaymentService.Free;
  end;
end;
```

### 3. Utilisation de HTTPS

Assurez-vous que toutes les communications avec les API de paiement se font via HTTPS:

```pascal
// S'assurer que l'URL utilise HTTPS
procedure CheckSecureURL(const URL: string);
begin
  if not URL.StartsWith('https://') then
    raise Exception.Create('Erreur: Communication non sécurisée détectée. ' +
                          'Toutes les API de paiement doivent utiliser HTTPS.');
end;
```

## Gestion des webhooks

Les webhooks permettent aux plateformes de paiement de notifier votre application des événements importants (paiement réussi, échec, remboursement, etc.).

### 1. Création d'un endpoint webhook

Si votre application est connectée à un serveur web, vous devez créer un endpoint pour recevoir les notifications:

```pascal
procedure TWebModule1.WebModule1WebActionItem1Action(Sender: TObject;
                                                 Request: TWebRequest;
                                                 Response: TWebResponse;
                                                 var Handled: Boolean);
var
  WebhookData: TJSONObject;
  EventType: string;
  PaymentIntentID: string;
begin
  // Vérifier la signature webhook (important pour la sécurité)
  if not VerifyStripeSignature(Request.Content, Request.GetFieldByName('Stripe-Signature')) then
  begin
    Response.StatusCode := 401;
    Response.Content := 'Signature invalide';
    Exit;
  end;

  // Analyser le JSON reçu
  WebhookData := TJSONObject.ParseJSONValue(Request.Content) as TJSONObject;
  try
    EventType := WebhookData.GetValue<string>('type');

    // Traiter différents types d'événements
    if EventType = 'payment_intent.succeeded' then
    begin
      // Récupérer l'ID du paiement
      PaymentIntentID := WebhookData.GetValue<TJSONObject>('data')
                        .GetValue<TJSONObject>('object')
                        .GetValue<string>('id');

      // Mettre à jour la base de données, envoyer un e-mail de confirmation, etc.
      UpdateOrderStatus(PaymentIntentID, 'paid');
      SendConfirmationEmail(PaymentIntentID);
    end
    else if EventType = 'payment_intent.payment_failed' then
    begin
      // Gérer l'échec de paiement
      // ...
    end;

    // Répondre avec succès
    Response.StatusCode := 200;
    Response.Content := 'Webhook reçu';

  finally
    WebhookData.Free;
  end;

  Handled := True;
end;
```

### 2. Vérification de la signature (exemple pour Stripe)

```pascal
function VerifyStripeSignature(const Payload, Signature: string): Boolean;
var
  TimestampStr, ExpectedSignature, ActualSignature: string;
  Timestamp, ToleranceSeconds: Int64;
  Parts: TArray<string>;
  SecretKey: string;
begin
  // Clé secrète du webhook (configurée dans Stripe)
  SecretKey := LoadAPIKeys.StripeWebhookSecret;

  // Tolérance de 5 minutes
  ToleranceSeconds := 5 * 60;

  // Analyser la signature
  Parts := Signature.Split([',']);
  if Length(Parts) < 2 then
    Exit(False);

  // Extraire le timestamp
  TimestampStr := Parts[0].Replace('t=', '');
  if not TryStrToInt64(TimestampStr, Timestamp) then
    Exit(False);

  // Vérifier que le webhook n'est pas trop ancien
  if (DateTimeToUnix(Now) - Timestamp) > ToleranceSeconds then
    Exit(False);

  // Calculer la signature attendue
  ExpectedSignature := TimestampStr + '.' + Payload;
  ExpectedSignature := THashSHA256.GetHashString(ExpectedSignature, TEncoding.UTF8);

  // Comparer avec la signature reçue
  ActualSignature := Parts[1].Replace('v1=', '');

  Result := (ExpectedSignature = ActualSignature);
end;
```

## Tests et environnement sandbox

Avant de passer en production, testez toujours vos intégrations dans l'environnement sandbox:

### 1. Configuration des environnements

```pascal
type
  TPaymentEnvironment = (peTest, peProduction);

  TPaymentConfig = record
    Environment: TPaymentEnvironment;
    StripeSecretKey: string;
    StripePublicKey: string;
    PayPalClientID: string;
    PayPalClientSecret: string;
    procedure Initialize(AEnvironment: TPaymentEnvironment);
  end;

procedure TPaymentConfig.Initialize(AEnvironment: TPaymentEnvironment);
begin
  Environment := AEnvironment;

  case Environment of
    peTest:
      begin
        StripeSecretKey := 'sk_test_YourTestKey';
        StripePublicKey := 'pk_test_YourTestKey';
        PayPalClientID := 'test_client_id';
        PayPalClientSecret := 'test_client_secret';
      end;
    peProduction:
      begin
        StripeSecretKey := 'sk_live_YourLiveKey';
        StripePublicKey := 'pk_live_YourLiveKey';
        PayPalClientID := 'live_client_id';
        PayPalClientSecret := 'live_client_secret';
      end;
  end;
end;
```

### 2. Cartes de test

Pour Stripe, utilisez ces cartes de test:

```pascal
procedure TfrmPayment.ShowTestCardInfo;
begin
  mmTestCards.Lines.Clear;
  mmTestCards.Lines.Add('Cartes de test Stripe:');
  mmTestCards.Lines.Add('- Paiement réussi: 4242 4242 4242 4242');
  mmTestCards.Lines.Add('- Échec paiement: 4000 0000 0000 0002');
  mmTestCards.Lines.Add('- Paiement nécessitant 3D Secure: 4000 0000 0000 3220');
  mmTestCards.Lines.Add('Utiliser une date future et n''importe quel CVC');
end;
```

## Bonnes pratiques

### 1. Journalisation des erreurs

```pascal
procedure LogPaymentError(const Source, ErrorMsg: string);
var
  LogFile: TextFile;
  LogPath: string;
begin
  LogPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'payment_errors.log';

  AssignFile(LogFile, LogPath);
  try
    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, Format('[%s] %s: %s',
                            [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
                             Source, ErrorMsg]));
  finally
    CloseFile(LogFile);
  end;
end;
```

### 2. Structure de base de données pour les paiements

Voici un exemple de structure de table pour suivre les paiements:

```sql
CREATE TABLE payments (
  id INT AUTO_INCREMENT PRIMARY KEY,
  order_id VARCHAR(50) NOT NULL,
  provider VARCHAR(20) NOT NULL,
  provider_payment_id VARCHAR(100) NOT NULL,
  amount DECIMAL(10,2) NOT NULL,
  currency CHAR(3) NOT NULL,
  status VARCHAR(20) NOT NULL,
  created_at DATETIME DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);
```

### 3. Interface unifiée pour plusieurs fournisseurs

Créez une interface commune pour toutes vos plateformes de paiement:

```pascal
type
  IPaymentProvider = interface
    ['{A1B2C3D4-E5F6-G7H8-I9J0-K1L2M3N4O5P6}']
    function CreatePayment(Amount: Double; Currency: string;
                         Description: string): string; // Retourne ID
    function GetPaymentStatus(PaymentID: string): string;
    function RefundPayment(PaymentID: string; Amount: Double): Boolean;
  end;

  TStripeProvider = class(TInterfacedObject, IPaymentProvider)
  private
    FService: TStripePaymentService;
  public
    constructor Create(const SecretKey, PublicKey: string);
    destructor Destroy; override;

    // Implémentation de IPaymentProvider
    function CreatePayment(Amount: Double; Currency: string;
                         Description: string): string;
    function GetPaymentStatus(PaymentID: string): string;
    function RefundPayment(PaymentID: string; Amount: Double): Boolean;
  end;
```

## Exemple de projet complet

Voici un exemple simplifié d'un système de caisse avec intégration de paiement:

### 1. Interface utilisateur

```pascal
object frmCheckout: TfrmCheckout
  Left = 0
  Top = 0
  Caption = 'Finalisation de commande'
  ClientHeight = 450
  ClientWidth = 600
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  object pnlOrder: TPanel
    // Configuration du panneau...
    object lblTotal: TLabel
      Caption = 'Total :'
    end
    object lblAmount: TLabel
      Caption = '0.00 €'
    end
  end
  object rgPaymentMethod: TRadioGroup
    Caption = 'Méthode de paiement'
    Items.Strings = (
      'Carte bancaire (Stripe)'
      'PayPal'
    )
  end
  object btnPay: TButton
    Caption = 'Payer maintenant'
    OnClick = btnPayClick
  end
end
```

### 2. Code du formulaire

```pascal
unit CheckoutForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.JSON,
  StripePaymentService, PayPalPaymentService;

type
  TPaymentResult = (prSuccess, prFailed, prCancelled);

  TfrmCheckout = class(TForm)
    pnlOrder: TPanel;
    lblTotal: TLabel;
    lblAmount: TLabel;
    rgPaymentMethod: TRadioGroup;
    btnPay: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPayClick(Sender: TObject);
  private
    FStripeService: TStripePaymentService;
    FPayPalService: TPayPalPaymentService;
    FOrderID: string;
    FAmount: Double;

    function ProcessStripePayment: TPaymentResult;
    function ProcessPayPalPayment: TPaymentResult;
    procedure SavePaymentDetails(const ProviderName, PaymentID: string;
                              Result: TPaymentResult);
  public
    procedure SetOrderDetails(const OrderID: string; Amount: Double);
  end;

implementation

{$R *.dfm}

// Implémentation à suivre...

end.
```

### 3. Implémentation des paiements

```pascal
procedure TfrmCheckout.FormCreate(Sender: TObject);
var
  PaymentConfig: TPaymentConfig;
begin
  // Initialiser la configuration (en mode test par défaut)
  PaymentConfig.Initialize(peTest);

  // Créer les services de paiement
  FStripeService := TStripePaymentService.Create(PaymentConfig.StripeSecretKey,
                                              PaymentConfig.StripePublicKey);
  FPayPalService := TPayPalPaymentService.Create(PaymentConfig.PayPalClientID,
                                              PaymentConfig.PayPalClientSecret);
end;

procedure TfrmCheckout.FormDestroy(Sender: TObject);
begin
  FStripeService.Free;
  FPayPalService.Free;
end;

procedure TfrmCheckout.SetOrderDetails(const OrderID: string; Amount: Double);
begin
  FOrderID := OrderID;
  FAmount := Amount;

  lblAmount.Caption := FormatFloat('#,##0.00 €', FAmount);
end;

procedure TfrmCheckout.btnPayClick(Sender: TObject);
var
  PaymentResult: TPaymentResult;
  ProviderName, PaymentID: string;
begin
  if FAmount <= 0 then
  begin
    ShowMessage('Montant invalide');
    Exit;
  end;

  btnPay.Enabled := False;
  try
    case rgPaymentMethod.ItemIndex of
      0: // Stripe
      begin
        ProviderName := 'Stripe';
        PaymentResult := ProcessStripePayment;
      end;
      1: // PayPal
      begin
        ProviderName := 'PayPal';
        PaymentResult := ProcessPayPalPayment;
      end;
    else
      ShowMessage('Veuillez sélectionner une méthode de paiement');
      Exit;
    end;

    // Enregistrer le résultat du paiement
    SavePaymentDetails(ProviderName, FOrderID, PaymentResult);

    // Afficher le résultat
    case PaymentResult of
      prSuccess:
        begin
          ShowMessage('Paiement réussi !');
          ModalResult := mrOk; // Fermer la fenêtre avec succès
        end;
      prFailed:
        ShowMessage('Le paiement a échoué. Veuillez réessayer.');
      prCancelled:
        ShowMessage('Paiement annulé par l''utilisateur.');
    end;

  finally
    btnPay.Enabled := True;
  end;
end;

function TfrmCheckout.ProcessStripePayment: TPaymentResult;
var
  PaymentIntent: TJSONObject;
  ClientSecret, RedirectURL: string;
  PaymentForm: TfrmStripePayment;
begin
  Result := prCancelled; // Par défaut

  try
    // Créer une intention de paiement
    PaymentIntent := FStripeService.CreatePaymentIntent(
      Round(FAmount * 100), // Conversion en centimes
      'eur',
      Format('Commande #%s', [FOrderID])
    );

    // Récupérer le client_secret
    ClientSecret := PaymentIntent.GetValue<string>('client_secret');

    // Afficher le formulaire de paiement Stripe
    PaymentForm := TfrmStripePayment.Create(Self);
    try
      PaymentForm.SetPaymentDetails(ClientSecret, FAmount);

      if PaymentForm.ShowModal = mrOk then
        Result := prSuccess
      else
        Result := prFailed;
    finally
      PaymentForm.Free;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors du traitement du paiement Stripe: ' + E.Message);
      LogPaymentError('Stripe', E.Message);
      Result := prFailed;
    end;
  end;
end;

function TfrmCheckout.ProcessPayPalPayment: TPaymentResult;
var
  OrderResponse: TJSONObject;
  OrderID, ApprovalURL: string;
  Links: TJSONArray;
  i: Integer;
  ResultCode: Integer;
begin
  Result := prCancelled; // Par défaut

  try
    // Créer une commande PayPal
    OrderResponse := FPayPalService.CreateOrder(FAmount, 'EUR');

    // Récupérer l'ID de commande
    OrderID := OrderResponse.GetValue<string>('id');

    // Trouver l'URL d'approbation
    Links := OrderResponse.GetValue<TJSONArray>('links');
    ApprovalURL := '';

    for i := 0 to Links.Count - 1 do
    begin
      var Link := Links.Items[i] as TJSONObject;
      if Link.GetValue<string>('rel') = 'approve' then
      begin
        ApprovalURL := Link.GetValue<string>('href');
        Break;
      end;
    end;

    if ApprovalURL = '' then
      raise Exception.Create('URL d''approbation PayPal non trouvée');

    // Ouvrir le navigateur pour l'approbation PayPal
    ResultCode := ShellExecute(0, 'open', PChar(ApprovalURL), nil, nil, SW_SHOWNORMAL);

    if ResultCode <= 32 then // Erreur d'exécution
      raise Exception.CreateFmt('Impossible d''ouvrir le navigateur. Code: %d', [ResultCode]);

    // Attendre la confirmation de l'utilisateur
    if MessageDlg('Avez-vous complété le paiement sur PayPal?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Vérifier le statut du paiement
      var CaptureResponse := FPayPalService.CapturePayment(OrderID);
      var Status := CaptureResponse.GetValue<string>('status');

      if Status = 'COMPLETED' then
        Result := prSuccess
      else
        Result := prFailed;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors du traitement du paiement PayPal: ' + E.Message);
      LogPaymentError('PayPal', E.Message);
      Result := prFailed;
    end;
  end;
end;

procedure TfrmCheckout.SavePaymentDetails(const ProviderName, PaymentID: string;
                                        Result: TPaymentResult);
var
  Status: string;
begin
  // Définir le statut en fonction du résultat
  case Result of
    prSuccess: Status := 'completed';
    prFailed: Status := 'failed';
    prCancelled: Status := 'cancelled';
  end;

  // Exemple d'enregistrement dans une base de données
  with DataModule1.qryPayments do
  begin
    Close;
    SQL.Clear;
    SQL.Add('INSERT INTO payments (order_id, provider, amount, currency, status)');
    SQL.Add('VALUES (:order_id, :provider, :amount, :currency, :status)');

    ParamByName('order_id').AsString := FOrderID;
    ParamByName('provider').AsString := ProviderName;
    ParamByName('amount').AsFloat := FAmount;
    ParamByName('currency').AsString := 'EUR';
    ParamByName('status').AsString := Status;

    try
      ExecSQL;
    except
      on E: Exception do
        LogPaymentError('Database', E.Message);
    end;
  end;
end;
```

## Formulaire de paiement Stripe

Pour créer une expérience utilisateur optimale, nous allons implémenter un formulaire dédié au paiement par Stripe. Celui-ci utilisera un composant TWebBrowser pour charger une page HTML qui intègre Stripe.js.

### 1. Interface du formulaire

```pascal
unit StripePaymentForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.OleCtrls, SHDocVw;

type
  TfrmStripePayment = class(TForm)
    Panel1: TPanel;
    WebBrowser1: TWebBrowser;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure WebBrowser1DocumentComplete(ASender: TObject; const pDisp: IDispatch;
                                         const URL: OleVariant);
  private
    FClientSecret: string;
    FAmount: Double;
    FPaymentSuccessful: Boolean;
    procedure HandlePaymentResult(const Status: string);
  public
    procedure SetPaymentDetails(const AClientSecret: string; AAmount: Double);
  end;

implementation

{$R *.dfm}

// Implémentation à suivre...

end.
```

### 2. Implémentation du formulaire Stripe

```pascal
procedure TfrmStripePayment.FormCreate(Sender: TObject);
begin
  FPaymentSuccessful := False;
end;

procedure TfrmStripePayment.SetPaymentDetails(const AClientSecret: string; AAmount: Double);
begin
  FClientSecret := AClientSecret;
  FAmount := AAmount;
end;

procedure TfrmStripePayment.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmStripePayment.WebBrowser1DocumentComplete(ASender: TObject;
                                                     const pDisp: IDispatch;
                                                     const URL: OleVariant);
var
  Document: IHTMLDocument2;
  HTMLWindow: IHTMLWindow2;
  StripeHTML: string;
begin
  if WebBrowser1.ReadyState <> READYSTATE_COMPLETE then
    Exit;

  Document := WebBrowser1.Document as IHTMLDocument2;
  if not Assigned(Document) then
    Exit;

  // Construire la page HTML avec l'intégration Stripe
  StripeHTML :=
    '<!DOCTYPE html>' +
    '<html>' +
    '<head>' +
    '  <meta charset="utf-8">' +
    '  <title>Paiement Stripe</title>' +
    '  <script src="https://js.stripe.com/v3/"></script>' +
    '  <style>' +
    '    body { font-family: Arial, sans-serif; margin: 20px; }' +
    '    #payment-form { width: 100%; max-width: 500px; margin: 0 auto; }' +
    '    #card-element { margin: 20px 0; padding: 10px; border: 1px solid #ccc; border-radius: 4px; }' +
    '    #card-errors { color: #fa755a; margin-top: 10px; }' +
    '    .button { background: #5469d4; color: #ffffff; border-radius: 4px; border: 0;' +
    '              padding: 12px 16px; font-size: 16px; font-weight: 600; cursor: pointer; }' +
    '    .result-message { margin-top: 20px; }' +
    '  </style>' +
    '</head>' +
    '<body>' +
    '  <div id="payment-form">' +
    '    <h2>Paiement par carte bancaire</h2>' +
    '    <p>Montant: ' + FormatFloat('#,##0.00 €', FAmount) + '</p>' +
    '    <div id="card-element"></div>' +
    '    <div id="card-errors" role="alert"></div>' +
    '    <button id="submit-button" class="button">Payer</button>' +
    '    <div class="result-message" id="result-message"></div>' +
    '  </div>' +
    '  <script>' +
    '    var stripe = Stripe("' + GetStripePublicKey + '");' +
    '    var elements = stripe.elements();' +
    '    var cardElement = elements.create("card", {' +
    '      style: { base: { fontSize: "16px" } }' +
    '    });' +
    '    cardElement.mount("#card-element");' +
    '    ' +
    '    var form = document.getElementById("payment-form");' +
    '    var submitButton = document.getElementById("submit-button");' +
    '    var resultMessage = document.getElementById("result-message");' +
    '    ' +
    '    submitButton.addEventListener("click", function() {' +
    '      submitButton.disabled = true;' +
    '      stripe.confirmCardPayment("' + FClientSecret + '", {' +
    '        payment_method: { card: cardElement }' +
    '      })' +
    '      .then(function(result) {' +
    '        if (result.error) {' +
    '          document.getElementById("card-errors").textContent = result.error.message;' +
    '          submitButton.disabled = false;' +
    '          window.external.HandlePaymentResult("error");' +
    '        } else {' +
    '          if (result.paymentIntent.status === "succeeded") {' +
    '            resultMessage.textContent = "Paiement réussi !";' +
    '            window.external.HandlePaymentResult("succeeded");' +
    '          }' +
    '        }' +
    '      });' +
    '    });' +
    '  </script>' +
    '</body>' +
    '</html>';

  // Effacer le document actuel
  Document.clear;

  // Écrire notre HTML
  Document.write(StripeHTML);
  Document.close;

  // Exposer la méthode HandlePaymentResult au JavaScript
  HTMLWindow := (Document as IHTMLDocument2).parentWindow;
  HTMLWindow.execScript('function getWindowExternal() { return window.external; }', 'JavaScript');
end;

procedure TfrmStripePayment.HandlePaymentResult(const Status: string);
begin
  if Status = 'succeeded' then
  begin
    FPaymentSuccessful := True;
    ModalResult := mrOk;
  end
  else if Status = 'error' then
  begin
    FPaymentSuccessful := False;
    // Ne pas fermer la fenêtre, laisser l'utilisateur réessayer
  end;
end;

function GetStripePublicKey: string;
var
  PaymentConfig: TPaymentConfig;
begin
  PaymentConfig.Initialize(peTest); // ou peProduction selon le contexte
  Result := PaymentConfig.StripePublicKey;
end;
```

## Gestion des remboursements

Pour compléter votre intégration, il est important de pouvoir gérer les remboursements :

```pascal
function TStripePaymentService.RefundPayment(const PaymentIntentID: string;
                                          Amount: Integer = 0): TJSONObject;
var
  RequestBody: TStringList;
begin
  RequestBody := TStringList.Create;
  try
    // Configurer la requête
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'refunds';

    // Ajouter les paramètres
    RequestBody.Add('payment_intent=' + PaymentIntentID);
    if Amount > 0 then
      RequestBody.Add('amount=' + IntToStr(Amount));

    FRESTRequest.Body.Add(RequestBody.Text, TRESTContentType.ctURLENCODED);

    // Exécuter la requête
    FRESTRequest.Execute;

    // Analyser la réponse
    if FRESTResponse.StatusCode = 200 then
      Result := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject
    else
      raise Exception.CreateFmt('Erreur de remboursement Stripe: %s',
                              [FRESTResponse.Content]);
  finally
    RequestBody.Free;
  end;
end;
```

## Gestion des erreurs et messages à l'utilisateur

Il est crucial d'afficher des messages clairs en cas d'erreur lors du paiement :

```pascal
function GetReadablePaymentError(const ErrorCode: string): string;
begin
  // Codes d'erreur Stripe communs
  if ErrorCode = 'card_declined' then
    Result := 'Carte refusée. Veuillez essayer avec une autre carte.'
  else if ErrorCode = 'expired_card' then
    Result := 'Cette carte est expirée. Veuillez utiliser une autre carte.'
  else if ErrorCode = 'incorrect_cvc' then
    Result := 'Le code de sécurité (CVC) est incorrect.'
  else if ErrorCode = 'processing_error' then
    Result := 'Une erreur est survenue lors du traitement de la carte. Veuillez réessayer.'
  else if ErrorCode = 'insufficient_funds' then
    Result := 'Fonds insuffisants sur cette carte.'
  else
    Result := 'Erreur de paiement: ' + ErrorCode;
end;
```

## Intégrer d'autres fournisseurs de paiement

Pour intégrer d'autres fournisseurs comme Adyen ou WorldPay, suivez la même structure mais adaptez les API spécifiques :

```pascal
type
  TAdyenPaymentService = class
  private
    FRESTClient: TRESTClient;
    FRESTRequest: TRESTRequest;
    FRESTResponse: TRESTResponse;
    FApiKey: string;
    FMerchantAccount: string;
  public
    constructor Create(const AApiKey, AMerchantAccount: string);
    destructor Destroy; override;

    function CreatePayment(Amount: Integer; Currency, Reference: string): TJSONObject;
    // Autres méthodes...
  end;
```

## Exemple pratique complet : E-commerce avec panier

Voici un exemple plus complet d'intégration dans une application e-commerce :

```pascal
procedure TfrmCart.btnCheckoutClick(Sender: TObject);
var
  OrderID: string;
  TotalAmount: Double;
  CheckoutForm: TfrmCheckout;
begin
  // Créer un nouvel ID de commande
  OrderID := GenerateOrderID;

  // Calculer le montant total
  TotalAmount := CalculateCartTotal;

  // Enregistrer la commande dans la BD
  if not SaveOrderToDatabase(OrderID, TotalAmount) then
  begin
    ShowMessage('Erreur lors de l''enregistrement de la commande');
    Exit;
  end;

  // Afficher le formulaire de paiement
  CheckoutForm := TfrmCheckout.Create(Self);
  try
    CheckoutForm.SetOrderDetails(OrderID, TotalAmount);

    if CheckoutForm.ShowModal = mrOk then
    begin
      // Paiement réussi - finaliser la commande
      FinalizeOrder(OrderID);
      EmptyCart;
      ShowMessage('Merci pour votre commande! Un email de confirmation vous a été envoyé.');
    end
    else
    begin
      // Paiement échoué ou annulé
      UpdateOrderStatus(OrderID, 'payment_failed');
      ShowMessage('La commande n''a pas pu être finalisée.');
    end;

  finally
    CheckoutForm.Free;
  end;
end;
```

## Compléments avancés

### 1. Mise en œuvre d'un système de paiement récurrent (abonnements)

Pour gérer les abonnements avec Stripe :

```pascal
function TStripePaymentService.CreateSubscription(CustomerID, PriceID: string): TJSONObject;
var
  RequestBody: TStringList;
begin
  RequestBody := TStringList.Create;
  try
    // Configurer la requête
    FRESTRequest.Method := rmPOST;
    FRESTRequest.Resource := 'subscriptions';

    // Ajouter les paramètres
    RequestBody.Add('customer=' + CustomerID);
    RequestBody.Add('items[0][price]=' + PriceID);

    FRESTRequest.Body.Add(RequestBody.Text, TRESTContentType.ctURLENCODED);

    // Exécuter la requête
    FRESTRequest.Execute;

    // Analyser la réponse
    if (FRESTResponse.StatusCode = 200) or (FRESTResponse.StatusCode = 201) then
      Result := TJSONObject.ParseJSONValue(FRESTResponse.Content) as TJSONObject
    else
      raise Exception.CreateFmt('Erreur création abonnement: %s',
                              [FRESTResponse.Content]);
  finally
    RequestBody.Free;
  end;
end;
```

### 2. Implémentation de facturation automatique

```pascal
procedure GenerateInvoiceAfterPayment(const OrderID: string);
var
  Invoice: TInvoice;
  OrderDetails: TOrderDetails;
  PDFGenerator: TPDFInvoiceGenerator;
  EmailSender: TEmailSender;
begin
  // Récupérer les détails de la commande
  OrderDetails := GetOrderDetails(OrderID);

  // Créer une facture
  Invoice := TInvoice.Create;
  try
    Invoice.InvoiceNumber := GenerateInvoiceNumber;
    Invoice.OrderID := OrderID;
    Invoice.CustomerInfo := OrderDetails.CustomerInfo;
    Invoice.Items := OrderDetails.Items;
    Invoice.TotalAmount := OrderDetails.TotalAmount;
    Invoice.PaymentDate := Now;

    // Enregistrer la facture dans la base de données
    SaveInvoiceToDatabase(Invoice);

    // Générer un PDF
    PDFGenerator := TPDFInvoiceGenerator.Create;
    try
      PDFGenerator.GenerateInvoice(Invoice, GetInvoicePath(Invoice.InvoiceNumber));

      // Envoyer par email
      EmailSender := TEmailSender.Create;
      try
        EmailSender.SendInvoice(OrderDetails.CustomerEmail,
                              GetInvoicePath(Invoice.InvoiceNumber));
      finally
        EmailSender.Free;
      end;

    finally
      PDFGenerator.Free;
    end;

  finally
    Invoice.Free;
  end;
end;
```

## Ressources et documentation supplémentaire

Pour approfondir vos connaissances sur l'intégration des plateformes de paiement avec Delphi, consultez ces ressources :

1. Documentation officielle de Stripe : [https://stripe.com/docs](https://stripe.com/docs)
2. Documentation API de PayPal : [https://developer.paypal.com/docs/api/overview/](https://developer.paypal.com/docs/api/overview/)
3. Forums Delphi pour des exemples spécifiques
4. Blogs techniques sur les intégrations de paiement

## Conclusion

L'intégration des plateformes de paiement dans vos applications Delphi ouvre de nouvelles opportunités commerciales. Bien que cette intégration puisse sembler complexe au premier abord, l'utilisation des composants REST de Delphi et une bonne structure de code vous permettront de créer des solutions de paiement robustes et sécurisées.

Points clés à retenir :
1. Toujours utiliser les environnements sandbox pour les tests
2. Sécuriser toutes les communications et les données sensibles
3. Gérer correctement les erreurs et fournir des messages clairs aux utilisateurs
4. Implémenter des mécanismes de vérification et de réconciliation
5. Se tenir informé des mises à jour des API des plateformes de paiement

En suivant ce guide, vous disposez maintenant des bases nécessaires pour intégrer différentes solutions de paiement dans vos applications Delphi et proposer une expérience utilisateur fluide et professionnelle.

## Pratiques recommandées

- **Sécurité** : Ne stockez jamais les données de carte directement, utilisez les tokens fournis par les plateformes de paiement
- **Journalisation** : Enregistrez tous les événements de paiement pour le suivi et la résolution des problèmes
- **Conformité** : Assurez-vous que votre application respecte les normes PCI DSS si vous manipulez des données de paiement
- **Tests** : Testez rigoureusement tous les scénarios, y compris les échecs et les erreurs
- **Notifications** : Informez l'utilisateur de l'état de son paiement à chaque étape

---

**Note technique** : La plupart des exemples de ce tutoriel sont compatibles avec Delphi 11 Alexandria et supérieur. L'utilisation des expressions lambda dans certains exemples de code pourrait nécessiter Delphi 12 Athens ou supérieur.

⏭️ [Ressources et communauté](/20-ressources-et-communaute/README.md)
