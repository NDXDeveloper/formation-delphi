🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.8 Intégration de plateformes de paiement

## Introduction

Bienvenue dans le monde passionnant des paiements en ligne ! Dans ce chapitre, vous allez apprendre à intégrer des solutions de paiement dans vos applications Delphi pour permettre à vos utilisateurs de payer en toute sécurité.

### Qu'est-ce qu'une plateforme de paiement ?

Une **plateforme de paiement** est un service qui permet de traiter les transactions financières en ligne de manière sécurisée. Elle fait le lien entre votre application, la banque de votre client et votre compte bancaire.

**Analogie simple** : Imaginez un caissier dans un magasin. Il prend l'argent, vérifie qu'il est valide, fait la transaction, et vous donne un reçu. Une plateforme de paiement fait exactement ça, mais en ligne et de manière automatique !

#### Le parcours d'un paiement

```
[Client] → [Votre App] → [Plateforme de paiement] → [Banque Client]
                                    ↓
                           [Vérification]
                                    ↓
                            [Votre Banque] ← [Confirmation]
                                    ↓
[Client] ← [Confirmation] ← [Votre App]
```

### Pourquoi utiliser une plateforme de paiement ?

**Avantages** :
✅ **Sécurité** : Conformité PCI DSS automatique  
✅ **Simplicité** : Pas besoin d'être expert en finance  
✅ **Rapidité** : Intégration en quelques heures  
✅ **Confiance** : Logos reconnus (Stripe, PayPal, etc.)  
✅ **Support** : Cartes bancaires, virements, wallets  
✅ **International** : Paiements dans le monde entier

**Sans plateforme de paiement** :
❌ Conformité complexe (PCI DSS niveau 1)  
❌ Infrastructure coûteuse  
❌ Gestion du risque de fraude  
❌ Support technique bancaire  
❌ Certificats de sécurité

### Principales plateformes de paiement

| Plateforme | Popularité | Frais | Facilité |
|------------|-----------|-------|----------|
| **Stripe** | ⭐⭐⭐⭐⭐ | 1.4% + 0.25€ | Très facile |
| **PayPal** | ⭐⭐⭐⭐⭐ | 2.9% + 0.30€ | Facile |
| **Square** | ⭐⭐⭐⭐ | 1.75% | Facile |
| **Mollie** | ⭐⭐⭐⭐ | 1.8% + 0.25€ | Très facile |
| **Braintree** | ⭐⭐⭐ | 1.9% + 0.30€ | Moyen |

**Notre focus** : Stripe et PayPal (les plus populaires)

### Objectifs de ce chapitre

À la fin de ce tutoriel, vous serez capable de :

✅ Comprendre les concepts de paiement en ligne  
✅ Intégrer Stripe dans vos applications  
✅ Intégrer PayPal  
✅ Gérer les paiements uniques et récurrents  
✅ Implémenter les webhooks  
✅ Sécuriser les transactions  
✅ Gérer les remboursements  
✅ Tester en environnement sandbox  
✅ Déployer en production

### Prérequis

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Compréhension des API REST
- ✅ Notions de JSON
- ✅ Bases de sécurité web (HTTPS)

**Comptes nécessaires** :
- ✅ Compte Stripe (gratuit)
- ✅ Compte PayPal Business (gratuit)
- ✅ Compte bancaire ou carte de crédit pour tests

**Outils** :
- ✅ Delphi 13 Florence
- ✅ Postman (pour tester les APIs)
- ✅ Navigateur moderne

### Durée estimée

**10 à 15 heures** de travail, réparties ainsi :
- Compréhension des concepts : 2-3 heures
- Configuration des comptes : 1-2 heures
- Intégration Stripe : 3-4 heures
- Intégration PayPal : 2-3 heures
- Tests et sécurisation : 2-3 heures

---

## Partie 1 : Concepts fondamentaux

### 1.1 Sécurité et conformité PCI DSS

**PCI DSS** (Payment Card Industry Data Security Standard) est un ensemble de règles pour protéger les données de cartes bancaires.

#### Les 3 niveaux de conformité

**Niveau 1** : Vous gérez TOUT (très complexe)
- Stockage des numéros de carte
- Chiffrement
- Audits réguliers
- **À ÉVITER !**

**Niveau 2** : Plateforme tierce (recommandé)
- La plateforme stocke les données
- Vous n'avez jamais les numéros de carte
- Conformité automatique
- **C'EST CE QUE NOUS ALLONS FAIRE**

**Niveau 3** : Iframe/Redirect
- L'utilisateur saisit ailleurs
- Encore plus simple
- Moins de contrôle sur l'UX

#### Règle d'or : JAMAIS stocker les numéros de carte

```pascal
// ❌ JAMAIS FAIRE ÇA !
var
  CardNumber: string;
begin
  CardNumber := EditCardNumber.Text;
  SaveToDatabase(CardNumber); // INTERDIT !
end;

// ✅ À LA PLACE :
var
  Token: string;
begin
  // La plateforme crée un token sécurisé
  Token := StripeAPI.CreateCardToken(EditCardNumber.Text);
  SaveToDatabase(Token); // OK - c'est un token, pas le vrai numéro
end;
```

### 1.2 Types de paiements

#### Paiement unique (One-time)

Le client paie une fois.

**Exemples** :
- Achat d'un produit
- Réservation
- Don ponctuel

**Flux** :
```
[Client remplit formulaire] → [Validation] → [Paiement] → [Confirmation]
```

#### Paiement récurrent (Subscription)

Le client est débité automatiquement chaque mois/an.

**Exemples** :
- Abonnement SaaS
- Adhésion
- Service mensuel

**Flux** :
```
[Inscription] → [Premier paiement] → [Débits automatiques mensuels]
```

#### Paiement à la demande (On-demand)

Vous débitez quand vous voulez (avec autorisation du client).

**Exemples** :
- Uber (à la fin du trajet)
- Usage variable
- Consommation

### 1.3 Vocabulaire essentiel

**Token** : Représentation sécurisée d'une carte
- Exemple : `tok_1234abcd`
- Valide une seule fois ou pour un client

**Customer** : Client enregistré dans la plateforme
- Peut avoir plusieurs moyens de paiement
- Permet les paiements récurrents

**Charge** : Transaction de paiement
- Montant, devise, description
- Peut être capturée ou remboursée

**Webhook** : Notification automatique
- La plateforme vous prévient d'un événement
- Exemple : paiement réussi, échec, remboursement

**Sandbox** : Environnement de test
- Faux paiements pour tester
- Cartes de test : `4242 4242 4242 4242`

---

## Partie 2 : Intégration Stripe

### 2.1 Configuration du compte Stripe

**Étape 1 : Créer un compte**

1. Allez sur [stripe.com](https://stripe.com)
2. Cliquez "Start now" / "Commencer"
3. Créez votre compte
4. Validez votre email

**Étape 2 : Obtenir les clés API**

1. Tableau de bord → **Développeurs → Clés API**
2. Vous avez 4 clés :
   - **Clé publique test** : `pk_test_...` (pour le frontend)
   - **Clé secrète test** : `sk_test_...` (pour le backend)
   - **Clé publique live** : `pk_live_...` (production)
   - **Clé secrète live** : `sk_live_...` (production)

**⚠️ IMPORTANT** : La clé secrète doit rester SECRÈTE !

**Étape 3 : Mode test**

Par défaut, vous êtes en mode test. Parfait pour débuter !

**Cartes de test Stripe** :
- Succès : `4242 4242 4242 4242`
- Refusée : `4000 0000 0000 0002`
- 3D Secure : `4000 0027 6000 3184`

### 2.2 Classe Stripe pour Delphi

Créons une unité pour gérer Stripe :

```pascal
unit uStripe;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.NetEncoding;

type
  TStripePaymentStatus = (psSucceeded, psPending, psFailed, psCanceled);

  TStripePaymentIntent = record
    ID: string;
    Amount: Integer;          // En centimes (1000 = 10.00€)
    Currency: string;
    Status: TStripePaymentStatus;
    ClientSecret: string;
    Description: string;
  end;

  TStripeCustomer = record
    ID: string;
    Email: string;
    Name: string;
    Description: string;
  end;

  TStripe = class
  private
    FSecretKey: string;
    FPublishableKey: string;

    function BuildAuthHeader: string;
    function ParsePaymentIntent(const AJSON: TJSONObject): TStripePaymentIntent;
    function ParseCustomer(const AJSON: TJSONObject): TStripeCustomer;
  public
    constructor Create(const ASecretKey, APublishableKey: string);

    // Paiements
    function CreatePaymentIntent(AAmount: Integer; const ACurrency: string;
      const ADescription: string = ''): TStripePaymentIntent;
    function ConfirmPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;
    function CancelPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;
    function GetPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;

    // Clients
    function CreateCustomer(const AEmail, AName: string;
      const ADescription: string = ''): TStripeCustomer;
    function GetCustomer(const ACustomerID: string): TStripeCustomer;

    // Remboursements
    function RefundPayment(const APaymentIntentID: string;
      AAmount: Integer = 0): Boolean;

    property SecretKey: string read FSecretKey write FSecretKey;
    property PublishableKey: string read FPublishableKey write FPublishableKey;
  end;

implementation

const
  STRIPE_API_URL = 'https://api.stripe.com/v1';

{ TStripe }

constructor TStripe.Create(const ASecretKey, APublishableKey: string);  
begin  
  inherited Create;
  FSecretKey := ASecretKey;
  FPublishableKey := APublishableKey;
end;

function TStripe.BuildAuthHeader: string;  
var  
  Credentials: string;
begin
  Credentials := FSecretKey + ':';
  Result := 'Basic ' + TNetEncoding.Base64.Encode(Credentials);
end;

function TStripe.CreatePaymentIntent(AAmount: Integer; const ACurrency: string;
  const ADescription: string): TStripePaymentIntent;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    // Configuration
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    // Données du paiement
    FormData := TStringList.Create;
    try
      FormData.Add('amount=' + IntToStr(AAmount));
      FormData.Add('currency=' + ACurrency.ToLower);

      if not ADescription.IsEmpty then
        FormData.Add('description=' + TNetEncoding.URL.Encode(ADescription));

      FormData.Add('automatic_payment_methods[enabled]=true');

      // Appel API
      Response := HTTP.Post(
        STRIPE_API_URL + '/payment_intents',
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ParsePaymentIntent(JSONResponse);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur Stripe: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TStripe.ParsePaymentIntent(const AJSON: TJSONObject): TStripePaymentIntent;  
var  
  StatusStr: string;
begin
  Result.ID := AJSON.GetValue<string>('id');
  Result.Amount := AJSON.GetValue<Integer>('amount');
  Result.Currency := AJSON.GetValue<string>('currency');
  Result.ClientSecret := AJSON.GetValue<string>('client_secret');

  if AJSON.TryGetValue<string>('description', Result.Description) then;

  StatusStr := AJSON.GetValue<string>('status');
  if StatusStr = 'succeeded' then
    Result.Status := psSucceeded
  else if StatusStr = 'processing' then
    Result.Status := psPending
  else if StatusStr = 'canceled' then
    Result.Status := psCanceled
  else
    Result.Status := psFailed;
end;

function TStripe.ConfirmPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      FormData.Add('payment_method=pm_card_visa'); // Carte de test

      Response := HTTP.Post(
        Format('%s/payment_intents/%s/confirm', [STRIPE_API_URL, APaymentIntentID]),
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ParsePaymentIntent(JSONResponse);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur confirmation: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TStripe.GetPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    Response := HTTP.Get(
      Format('%s/payment_intents/%s', [STRIPE_API_URL, APaymentIntentID])
    );

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := ParsePaymentIntent(JSONResponse);
      finally
        JSONResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur récupération: %d - %s',
        [Response.StatusCode, Response.ContentAsString]);
  finally
    HTTP.Free;
  end;
end;

function TStripe.CancelPaymentIntent(const APaymentIntentID: string): TStripePaymentIntent;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      Response := HTTP.Post(
        Format('%s/payment_intents/%s/cancel', [STRIPE_API_URL, APaymentIntentID]),
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ParsePaymentIntent(JSONResponse);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur annulation: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TStripe.CreateCustomer(const AEmail, AName: string;
  const ADescription: string): TStripeCustomer;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      FormData.Add('email=' + TNetEncoding.URL.Encode(AEmail));
      FormData.Add('name=' + TNetEncoding.URL.Encode(AName));

      if not ADescription.IsEmpty then
        FormData.Add('description=' + TNetEncoding.URL.Encode(ADescription));

      Response := HTTP.Post(
        STRIPE_API_URL + '/customers',
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ParseCustomer(JSONResponse);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur création client: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TStripe.ParseCustomer(const AJSON: TJSONObject): TStripeCustomer;  
begin  
  Result.ID := AJSON.GetValue<string>('id');
  Result.Email := AJSON.GetValue<string>('email');
  Result.Name := AJSON.GetValue<string>('name');

  if AJSON.TryGetValue<string>('description', Result.Description) then;
end;

function TStripe.GetCustomer(const ACustomerID: string): TStripeCustomer;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    Response := HTTP.Get(
      Format('%s/customers/%s', [STRIPE_API_URL, ACustomerID])
    );

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := ParseCustomer(JSONResponse);
      finally
        JSONResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur récupération client: %d - %s',
        [Response.StatusCode, Response.ContentAsString]);
  finally
    HTTP.Free;
  end;
end;

function TStripe.RefundPayment(const APaymentIntentID: string;
  AAmount: Integer): Boolean;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
begin
  Result := False;
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      FormData.Add('payment_intent=' + APaymentIntentID);

      if AAmount > 0 then
        FormData.Add('amount=' + IntToStr(AAmount));

      Response := HTTP.Post(
        STRIPE_API_URL + '/refunds',
        FormData
      );

      Result := Response.StatusCode = 200;

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

end.
```

### 2.3 Interface de paiement Stripe

```pascal
unit uStripePaymentForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, uStripe;

type
  TStripePaymentForm = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    LabelSubtitle: TLabel;

    PanelAmount: TPanel;
    LabelAmount: TLabel;
    EditAmount: TEdit;
    ComboBoxCurrency: TComboBox;

    PanelDescription: TPanel;
    LabelDescription: TLabel;
    MemoDescription: TMemo;

    PanelActions: TPanel;
    ButtonPay: TButton;
    ButtonCancel: TButton;

    ProgressBar1: TProgressBar;
    LabelStatus: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonPayClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FStripe: TStripe;
    FPaymentIntentID: string;

    procedure ProcessPaymentAsync;
    procedure UpdateStatus(const AMessage: string; AColor: TColor);
  public
    property PaymentIntentID: string read FPaymentIntentID;
  end;

var
  StripePaymentForm: TStripePaymentForm;

implementation

{$R *.dfm}

uses
  System.Threading;

procedure TStripePaymentForm.FormCreate(Sender: TObject);  
begin  
  // Initialiser Stripe avec vos clés
  FStripe := TStripe.Create(
    'sk_test_votre_cle_secrete',  // Clé secrète test
    'pk_test_votre_cle_publique'  // Clé publique test
  );

  // Configurer l'interface
  ComboBoxCurrency.Items.Add('EUR - Euro');
  ComboBoxCurrency.Items.Add('USD - Dollar');
  ComboBoxCurrency.Items.Add('GBP - Livre');
  ComboBoxCurrency.ItemIndex := 0;

  EditAmount.Text := '10.00';
  MemoDescription.Text := 'Paiement test';

  ProgressBar1.Style := pbstNormal;
  ProgressBar1.Visible := False;
end;

procedure TStripePaymentForm.FormDestroy(Sender: TObject);  
begin  
  FStripe.Free;
end;

procedure TStripePaymentForm.UpdateStatus(const AMessage: string; AColor: TColor);  
begin  
  LabelStatus.Caption := AMessage;
  LabelStatus.Font.Color := AColor;
end;

procedure TStripePaymentForm.ProcessPaymentAsync;  
begin  
  // Désactiver l'interface
  ButtonPay.Enabled := False;
  ButtonCancel.Enabled := False;
  ProgressBar1.Visible := True;
  ProgressBar1.Style := pbstMarquee;

  UpdateStatus('Traitement en cours...', clBlue);

  TTask.Run(
    procedure
    var
      PaymentIntent: TStripePaymentIntent;
      Amount: Double;
      AmountCents: Integer;
      Currency: string;
      Description: string;
    begin
      try
        // Récupérer les données
        TThread.Synchronize(nil,
          procedure
          begin
            Amount := StrToFloatDef(EditAmount.Text, 0);
            AmountCents := Round(Amount * 100); // Convertir en centimes

            case ComboBoxCurrency.ItemIndex of
              0: Currency := 'eur';
              1: Currency := 'usd';
              2: Currency := 'gbp';
            else
              Currency := 'eur';
            end;

            Description := MemoDescription.Text;
          end);

        // Créer le PaymentIntent
        PaymentIntent := FStripe.CreatePaymentIntent(
          AmountCents,
          Currency,
          Description
        );

        FPaymentIntentID := PaymentIntent.ID;

        // Simuler la confirmation (en production, se fait côté client avec Stripe.js)
        Sleep(1000);
        PaymentIntent := FStripe.ConfirmPaymentIntent(PaymentIntent.ID);

        // Vérifier le statut
        if PaymentIntent.Status = psSucceeded then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              UpdateStatus('✓ Paiement réussi !', clGreen);
              ShowMessage(Format(
                'Paiement effectué avec succès !%s' +
                'ID: %s%s' +
                'Montant: %.2f %s',
                [#13#10, PaymentIntent.ID, #13#10, Amount, Currency.ToUpper]
              ));

              ModalResult := mrOk;
            end);
        end
        else
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              UpdateStatus('✗ Paiement échoué', clRed);
              ShowMessage('Le paiement a échoué. Veuillez réessayer.');
            end);
        end;

      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              UpdateStatus('✗ Erreur: ' + E.Message, clRed);
              ShowMessage('Erreur de paiement: ' + E.Message);
            end);
        end;
      end;

      // Réactiver l'interface
      TThread.Synchronize(nil,
        procedure
        begin
          ButtonPay.Enabled := True;
          ButtonCancel.Enabled := True;
          ProgressBar1.Visible := False;
        end);
    end);
end;

procedure TStripePaymentForm.ButtonPayClick(Sender: TObject);  
var  
  Amount: Double;
begin
  // Valider les données
  Amount := StrToFloatDef(EditAmount.Text, 0);

  if Amount <= 0 then
  begin
    ShowMessage('Veuillez entrer un montant valide');
    EditAmount.SetFocus;
    Exit;
  end;

  if MemoDescription.Text.Trim.IsEmpty then
  begin
    ShowMessage('Veuillez entrer une description');
    MemoDescription.SetFocus;
    Exit;
  end;

  // Confirmer
  if MessageDlg(
    Format('Confirmer le paiement de %.2f %s ?',
      [Amount, ComboBoxCurrency.Text.Substring(0, 3)]),
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes then
  begin
    ProcessPaymentAsync;
  end;
end;

procedure TStripePaymentForm.ButtonCancelClick(Sender: TObject);  
begin  
  if MessageDlg('Annuler le paiement ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ModalResult := mrCancel;
end;

end.
```

### 2.4 Gestion des abonnements

```pascal
// Extension de la classe TStripe pour les abonnements

type
  TStripeSubscription = record
    ID: string;
    CustomerID: string;
    Status: string;
    CurrentPeriodStart: TDateTime;
    CurrentPeriodEnd: TDateTime;
    CancelAtPeriodEnd: Boolean;
  end;

  TStripePrice = record
    ID: string;
    ProductID: string;
    Amount: Integer;
    Currency: string;
    Interval: string; // 'month', 'year'
  end;

// Ajouter à TStripe :
function TStripe.CreateSubscription(const ACustomerID, APriceID: string): TStripeSubscription;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      FormData.Add('customer=' + ACustomerID);
      FormData.Add('items[0][price]=' + APriceID);

      Response := HTTP.Post(
        STRIPE_API_URL + '/subscriptions',
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result.ID := JSONResponse.GetValue<string>('id');
          Result.CustomerID := JSONResponse.GetValue<string>('customer');
          Result.Status := JSONResponse.GetValue<string>('status');
          Result.CancelAtPeriodEnd := JSONResponse.GetValue<Boolean>('cancel_at_period_end');

          // Dates (Unix timestamp à convertir)
          var StartTimestamp := JSONResponse.GetValue<Int64>('current_period_start');
          var EndTimestamp := JSONResponse.GetValue<Int64>('current_period_end');

          Result.CurrentPeriodStart := UnixToDateTime(StartTimestamp);
          Result.CurrentPeriodEnd := UnixToDateTime(EndTimestamp);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur création abonnement: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TStripe.CancelSubscription(const ASubscriptionID: string;
  ACancelImmediately: Boolean = False): Boolean;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
begin
  Result := False;
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    FormData := TStringList.Create;
    try
      if not ACancelImmediately then
        FormData.Add('cancel_at_period_end=true');

      if ACancelImmediately then
        Response := HTTP.Delete(
          Format('%s/subscriptions/%s', [STRIPE_API_URL, ASubscriptionID])
        )
      else
        Response := HTTP.Post(
          Format('%s/subscriptions/%s', [STRIPE_API_URL, ASubscriptionID]),
          FormData
        );

      Result := Response.StatusCode = 200;

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;
```

---

## Partie 3 : Intégration PayPal

### 3.1 Configuration PayPal

**Étape 1 : Créer un compte Business**

1. Allez sur [developer.paypal.com](https://developer.paypal.com)
2. Créez un compte développeur
3. Créez une application Sandbox

**Étape 2 : Obtenir les credentials**

1. **Dashboard → My Apps & Credentials**
2. Créez une app
3. Notez vos credentials :
   - **Client ID** : Pour identifier votre app
   - **Secret** : Pour authentifier les requêtes

**Étape 3 : Comptes Sandbox**

PayPal fournit des comptes de test :
- **Vendeur** : business@example.com
- **Acheteur** : buyer@example.com
- Mot de passe : généré automatiquement

### 3.2 Classe PayPal pour Delphi

```pascal
unit uPayPal;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HttpClient,
  System.Net.URLClient, System.NetEncoding;

type
  TPayPalEnvironment = (ppeSandbox, ppeProduction);

  TPayPalOrder = record
    ID: string;
    Status: string; // CREATED, APPROVED, COMPLETED
    Amount: string;
    Currency: string;
    ApproveLink: string;
  end;

  TPayPal = class
  private
    FClientID: string;
    FSecret: string;
    FEnvironment: TPayPalEnvironment;
    FAccessToken: string;
    FTokenExpiry: TDateTime;

    function GetAPIURL: string;
    function GetAccessToken: string;
    function BuildAuthHeader: string;
    function ParseOrder(const AJSON: TJSONObject): TPayPalOrder;
  public
    constructor Create(const AClientID, ASecret: string;
      AEnvironment: TPayPalEnvironment = ppeSandbox);

    function CreateOrder(AAmount: Double; const ACurrency: string;
      const ADescription: string = ''): TPayPalOrder;
    function CaptureOrder(const AOrderID: string): TPayPalOrder;
    function GetOrder(const AOrderID: string): TPayPalOrder;

    property Environment: TPayPalEnvironment read FEnvironment write FEnvironment;
  end;

implementation

const
  PAYPAL_API_SANDBOX = 'https://api-m.sandbox.paypal.com';
  PAYPAL_API_PRODUCTION = 'https://api-m.paypal.com';

{ TPayPal }

constructor TPayPal.Create(const AClientID, ASecret: string;
  AEnvironment: TPayPalEnvironment);
begin
  inherited Create;
  FClientID := AClientID;
  FSecret := ASecret;
  FEnvironment := AEnvironment;
end;

function TPayPal.GetAPIURL: string;  
begin  
  if FEnvironment = ppeSandbox then
    Result := PAYPAL_API_SANDBOX
  else
    Result := PAYPAL_API_PRODUCTION;
end;

function TPayPal.GetAccessToken: string;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  FormData: TStringList;
  JSONResponse: TJSONObject;
  Credentials: string;
  ExpiresIn: Integer;
begin
  // Vérifier si le token est encore valide
  if (FAccessToken <> '') and (Now < FTokenExpiry) then
  begin
    Result := FAccessToken;
    Exit;
  end;

  HTTP := THTTPClient.Create;
  try
    // Basic Auth
    Credentials := TNetEncoding.Base64.Encode(FClientID + ':' + FSecret);
    HTTP.CustomHeaders['Authorization'] := 'Basic ' + Credentials;
    HTTP.CustomHeaders['Content-Type'] := 'application/x-www-form-urlencoded';

    FormData := TStringList.Create;
    try
      FormData.Add('grant_type=client_credentials');

      Response := HTTP.Post(
        GetAPIURL + '/v1/oauth2/token',
        FormData
      );

      if Response.StatusCode = 200 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          FAccessToken := JSONResponse.GetValue<string>('access_token');
          ExpiresIn := JSONResponse.GetValue<Integer>('expires_in');
          FTokenExpiry := IncSecond(Now, ExpiresIn - 60); // Marge de sécurité

          Result := FAccessToken;
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur authentification PayPal: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      FormData.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TPayPal.BuildAuthHeader: string;  
begin  
  Result := 'Bearer ' + GetAccessToken;
end;

function TPayPal.CreateOrder(AAmount: Double; const ACurrency: string;
  const ADescription: string): TPayPalOrder;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  RequestJSON: TJSONObject;
  PurchaseUnits: TJSONArray;
  PurchaseUnit: TJSONObject;
  AmountObj: TJSONObject;
  RequestBody: TStringStream;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;
    HTTP.CustomHeaders['Content-Type'] := 'application/json';

    // Construire le JSON de la commande
    RequestJSON := TJSONObject.Create;
    try
      RequestJSON.AddPair('intent', 'CAPTURE');

      PurchaseUnits := TJSONArray.Create;
      PurchaseUnit := TJSONObject.Create;

      AmountObj := TJSONObject.Create;
      AmountObj.AddPair('currency_code', ACurrency.ToUpper);
      AmountObj.AddPair('value', FormatFloat('0.00', AAmount));

      PurchaseUnit.AddPair('amount', AmountObj);

      if not ADescription.IsEmpty then
        PurchaseUnit.AddPair('description', ADescription);

      PurchaseUnits.AddElement(PurchaseUnit);
      RequestJSON.AddPair('purchase_units', PurchaseUnits);

      RequestBody := TStringStream.Create(RequestJSON.ToString, TEncoding.UTF8);
      try
        Response := HTTP.Post(
          GetAPIURL + '/v2/checkout/orders',
          RequestBody
        );

        if Response.StatusCode = 201 then
        begin
          JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
          try
            Result := ParseOrder(JSONResponse);
          finally
            JSONResponse.Free;
          end;
        end
        else
          raise Exception.CreateFmt('Erreur création commande PayPal: %d - %s',
            [Response.StatusCode, Response.ContentAsString]);

      finally
        RequestBody.Free;
      end;
    finally
      RequestJSON.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TPayPal.ParseOrder(const AJSON: TJSONObject): TPayPalOrder;  
var  
  Links: TJSONArray;
  Link: TJSONObject;
  I: Integer;
  PurchaseUnits: TJSONArray;
  FirstUnit: TJSONObject;
  Amount: TJSONObject;
begin
  Result.ID := AJSON.GetValue<string>('id');
  Result.Status := AJSON.GetValue<string>('status');

  // Récupérer le lien d'approbation
  Links := AJSON.GetValue<TJSONArray>('links');
  for I := 0 to Links.Count - 1 do
  begin
    Link := Links.Items[I] as TJSONObject;
    if Link.GetValue<string>('rel') = 'approve' then
    begin
      Result.ApproveLink := Link.GetValue<string>('href');
      Break;
    end;
  end;

  // Récupérer le montant
  if AJSON.TryGetValue<TJSONArray>('purchase_units', PurchaseUnits) then
  begin
    if PurchaseUnits.Count > 0 then
    begin
      FirstUnit := PurchaseUnits.Items[0] as TJSONObject;
      Amount := FirstUnit.GetValue<TJSONObject>('amount');
      Result.Amount := Amount.GetValue<string>('value');
      Result.Currency := Amount.GetValue<string>('currency_code');
    end;
  end;
end;

function TPayPal.CaptureOrder(const AOrderID: string): TPayPalOrder;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  EmptyBody: TStringStream;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;
    HTTP.CustomHeaders['Content-Type'] := 'application/json';

    EmptyBody := TStringStream.Create('', TEncoding.UTF8);
    try
      Response := HTTP.Post(
        Format('%s/v2/checkout/orders/%s/capture', [GetAPIURL, AOrderID]),
        EmptyBody
      );

      if Response.StatusCode = 201 then
      begin
        JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
        try
          Result := ParseOrder(JSONResponse);
        finally
          JSONResponse.Free;
        end;
      end
      else
        raise Exception.CreateFmt('Erreur capture commande PayPal: %d - %s',
          [Response.StatusCode, Response.ContentAsString]);

    finally
      EmptyBody.Free;
    end;
  finally
    HTTP.Free;
  end;
end;

function TPayPal.GetOrder(const AOrderID: string): TPayPalOrder;  
var  
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  JSONResponse: TJSONObject;
begin
  HTTP := THTTPClient.Create;
  try
    HTTP.CustomHeaders['Authorization'] := BuildAuthHeader;

    Response := HTTP.Get(
      Format('%s/v2/checkout/orders/%s', [GetAPIURL, AOrderID])
    );

    if Response.StatusCode = 200 then
    begin
      JSONResponse := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      try
        Result := ParseOrder(JSONResponse);
      finally
        JSONResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt('Erreur récupération commande PayPal: %d - %s',
        [Response.StatusCode, Response.ContentAsString]);
  finally
    HTTP.Free;
  end;
end;

end.
```

### 3.3 Interface PayPal avec WebBrowser

```pascal
unit uPayPalForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.OleCtrls, SHDocVw, uPayPal;

type
  TPayPalForm = class(TForm)
    PanelTop: TPanel;
    LabelInfo: TLabel;
    WebBrowser1: TWebBrowser;
    PanelBottom: TPanel;
    ButtonCancel: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure WebBrowser1NavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; const URL: OleVariant);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FPayPal: TPayPal;
    FOrderID: string;
    FCompleted: Boolean;

    procedure StartPayment(AAmount: Double; const ACurrency: string);
    procedure CheckOrderCompletion;
  public
    class function ExecutePayment(AAmount: Double; const ACurrency: string;
      const ADescription: string): Boolean;
  end;

implementation

{$R *.dfm}

uses
  System.Threading;

procedure TPayPalForm.FormCreate(Sender: TObject);  
begin  
  FPayPal := TPayPal.Create(
    'votre_client_id',
    'votre_secret',
    ppeSandbox
  );

  FCompleted := False;
end;

procedure TPayPalForm.FormDestroy(Sender: TObject);  
begin  
  FPayPal.Free;
end;

procedure TPayPalForm.StartPayment(AAmount: Double; const ACurrency: string);  
var  
  Order: TPayPalOrder;
begin
  try
    // Créer la commande
    Order := FPayPal.CreateOrder(AAmount, ACurrency, 'Paiement application');
    FOrderID := Order.ID;

    // Charger la page d'approbation PayPal
    WebBrowser1.Navigate(Order.ApproveLink);

    LabelInfo.Caption := Format('Commande créée: %s - Montant: %.2f %s',
      [Order.ID, AAmount, ACurrency]);

  except
    on E: Exception do
    begin
      ShowMessage('Erreur création commande: ' + E.Message);
      ModalResult := mrCancel;
    end;
  end;
end;

procedure TPayPalForm.WebBrowser1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; const URL: OleVariant);
var
  URLStr: string;
begin
  URLStr := VarToStr(URL);

  // Vérifier si l'utilisateur a approuvé
  if URLStr.Contains('success') or URLStr.Contains('approved') then
  begin
    LabelInfo.Caption := 'Paiement approuvé, finalisation...';
    CheckOrderCompletion;
  end
  else if URLStr.Contains('cancel') then
  begin
    ShowMessage('Paiement annulé par l''utilisateur');
    ModalResult := mrCancel;
  end;
end;

procedure TPayPalForm.CheckOrderCompletion;  
begin  
  TTask.Run(
    procedure
    var
      Order: TPayPalOrder;
    begin
      try
        Sleep(1000); // Petit délai

        // Capturer le paiement
        Order := FPayPal.CaptureOrder(FOrderID);

        if Order.Status = 'COMPLETED' then
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              FCompleted := True;
              ShowMessage('Paiement réussi !');
              ModalResult := mrOk;
            end);
        end
        else
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Le paiement n''a pas pu être complété');
              ModalResult := mrCancel;
            end);
        end;

      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur finalisation: ' + E.Message);
              ModalResult := mrCancel;
            end);
        end;
      end;
    end);
end;

procedure TPayPalForm.ButtonCancelClick(Sender: TObject);  
begin  
  if MessageDlg('Annuler le paiement ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    ModalResult := mrCancel;
end;

class function TPayPalForm.ExecutePayment(AAmount: Double; const ACurrency,
  ADescription: string): Boolean;
var
  Form: TPayPalForm;
begin
  Form := TPayPalForm.Create(nil);
  try
    Form.StartPayment(AAmount, ACurrency);
    Result := Form.ShowModal = mrOk;
  finally
    Form.Free;
  end;
end;

end.
```

---

## Partie 4 : Webhooks et événements

### 4.1 Qu'est-ce qu'un Webhook ?

Un **webhook** est une notification HTTP que la plateforme de paiement envoie à votre serveur quand un événement se produit.

**Événements courants** :
- `payment_intent.succeeded` : Paiement réussi
- `payment_intent.payment_failed` : Paiement échoué
- `customer.subscription.created` : Abonnement créé
- `customer.subscription.deleted` : Abonnement annulé
- `charge.refunded` : Remboursement effectué

### 4.2 Serveur Webhook avec Delphi

```pascal
unit uWebhookServer;

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  IdHTTPServer, IdContext, IdCustomHTTPServer;

type
  TWebhookServer = class
  private
    FHTTPServer: TIdHTTPServer;
    FPort: Integer;
    FStripeSigningSecret: string;

    procedure HandleRequest(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    function VerifyStripeSignature(const APayload, ASignature: string): Boolean;
    procedure ProcessStripeWebhook(const APayload: string);
  public
    constructor Create(APort: Integer; const AStripeSigningSecret: string);
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Port: Integer read FPort;
  end;

implementation

uses
  System.Hash, IdGlobal;

{ TWebhookServer }

constructor TWebhookServer.Create(APort: Integer; const AStripeSigningSecret: string);  
begin  
  inherited Create;
  FPort := APort;
  FStripeSigningSecret := AStripeSigningSecret;

  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.DefaultPort := FPort;
  FHTTPServer.OnCommandGet := HandleRequest;
end;

destructor TWebhookServer.Destroy;  
begin  
  Stop;
  FHTTPServer.Free;
  inherited;
end;

procedure TWebhookServer.Start;  
begin  
  FHTTPServer.Active := True;
end;

procedure TWebhookServer.Stop;  
begin  
  FHTTPServer.Active := False;
end;

procedure TWebhookServer.HandleRequest(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  Payload: string;
  Signature: string;
begin
  // Webhook Stripe sur /webhook
  if ARequestInfo.URI = '/webhook' then
  begin
    if ARequestInfo.CommandType = hcPOST then
    begin
      Payload := ARequestInfo.PostStream.DataString;
      Signature := ARequestInfo.RawHeaders.Values['Stripe-Signature'];

      // Vérifier la signature
      if VerifyStripeSignature(Payload, Signature) then
      begin
        ProcessStripeWebhook(Payload);

        AResponseInfo.ResponseNo := 200;
        AResponseInfo.ContentText := '{"received": true}';
      end
      else
      begin
        AResponseInfo.ResponseNo := 400;
        AResponseInfo.ContentText := '{"error": "Invalid signature"}';
      end;
    end
    else
    begin
      AResponseInfo.ResponseNo := 405;
      AResponseInfo.ContentText := '{"error": "Method not allowed"}';
    end;
  end
  else
  begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := '{"error": "Not found"}';
  end;
end;

function TWebhookServer.VerifyStripeSignature(const APayload, ASignature: string): Boolean;  
var  
  Parts: TArray<string>;
  Timestamp: string;
  Signatures: TArray<string>;
  ExpectedSignature: string;
  SignedPayload: string;
  I: Integer;
begin
  Result := False;

  // Format: t=timestamp,v1=signature1,v1=signature2
  Parts := ASignature.Split([',']);

  for I := 0 to High(Parts) do
  begin
    if Parts[I].StartsWith('t=') then
      Timestamp := Parts[I].Substring(2)
    else if Parts[I].StartsWith('v1=') then
    begin
      SetLength(Signatures, Length(Signatures) + 1);
      Signatures[High(Signatures)] := Parts[I].Substring(3);
    end;
  end;

  // Construire le payload signé
  SignedPayload := Timestamp + '.' + APayload;

  // Calculer la signature attendue (HMAC SHA256)
  ExpectedSignature := THashSHA2.GetHMACAsString(
    SignedPayload,
    FStripeSigningSecret,
    SHA256
  );

  // Vérifier
  for I := 0 to High(Signatures) do
  begin
    if Signatures[I] = ExpectedSignature then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TWebhookServer.ProcessStripeWebhook(const APayload: string);  
var  
  JSON: TJSONObject;
  EventType: string;
  DataObj: TJSONObject;
  ObjectObj: TJSONObject;
begin
  JSON := TJSONObject.ParseJSONValue(APayload) as TJSONObject;
  try
    EventType := JSON.GetValue<string>('type');
    DataObj := JSON.GetValue<TJSONObject>('data');
    ObjectObj := DataObj.GetValue<TJSONObject>('object');

    // Traiter selon le type d'événement
    if EventType = 'payment_intent.succeeded' then
    begin
      var PaymentIntentID := ObjectObj.GetValue<string>('id');
      var Amount := ObjectObj.GetValue<Integer>('amount');

      // Logique métier : marquer la commande comme payée
      // SavePaymentSuccess(PaymentIntentID, Amount);

    end
    else if EventType = 'payment_intent.payment_failed' then
    begin
      var PaymentIntentID := ObjectObj.GetValue<string>('id');

      // Logique métier : notifier l'échec
      // NotifyPaymentFailure(PaymentIntentID);

    end
    else if EventType = 'customer.subscription.deleted' then
    begin
      var SubscriptionID := ObjectObj.GetValue<string>('id');
      var CustomerID := ObjectObj.GetValue<string>('customer');

      // Logique métier : désactiver l'abonnement
      // DeactivateSubscription(CustomerID, SubscriptionID);
    end;

  finally
    JSON.Free;
  end;
end;

end.
```

### 4.3 Configuration des Webhooks

**Stripe** :
1. Dashboard → **Développeurs → Webhooks**
2. **Ajouter un endpoint**
3. URL : `https://votre-domaine.com/webhook`
4. Événements à écouter : Sélectionnez ceux qui vous intéressent
5. Notez le **Signing Secret** : `whsec_...`

**PayPal** :
1. Dashboard → **Webhooks**
2. **Créer un webhook**
3. URL : `https://votre-domaine.com/paypal-webhook`
4. Événements : `PAYMENT.CAPTURE.COMPLETED`, etc.

---

## Partie 5 : Sécurité et bonnes pratiques

### 5.1 Règles de sécurité

**❌ À NE JAMAIS FAIRE** :

```pascal
// NE JAMAIS stocker les numéros de carte
var CardNumber := '4242424242424242'; // INTERDIT !

// NE JAMAIS exposer la clé secrète
// var SecretKey := 'sk_live_...'; // dans le code frontend

// NE JAMAIS faire confiance aux montants côté client
// Si le client envoie le montant, il peut le modifier !
```

**✅ TOUJOURS FAIRE** :

```pascal
// Utiliser HTTPS
if not Request.IsSecure then
  raise Exception.Create('HTTPS requis');

// Valider côté serveur
function ValidateAmount(AAmount: Integer; AOrderID: string): Boolean;  
begin  
  // Récupérer le vrai montant depuis votre base
  var ExpectedAmount := GetOrderAmount(AOrderID);
  Result := AAmount = ExpectedAmount;
end;

// Logger tous les paiements
procedure LogPayment(const APaymentID: string; AAmount: Integer);  
begin  
  // Enregistrer dans la base
  // + Logger dans un fichier
  // + Alerter si montant suspect
end;
```

### 5.2 Gestion des erreurs

```pascal
function ProcessPaymentWithRetry(APaymentFunc: TFunc<Boolean>): Boolean;  
var  
  Attempt: Integer;
  MaxAttempts: Integer;
  WaitTime: Integer;
begin
  Result := False;
  MaxAttempts := 3;

  for Attempt := 1 to MaxAttempts do
  begin
    try
      Result := APaymentFunc();

      if Result then
        Exit; // Succès

    except
      on E: Exception do
      begin
        // Logger l'erreur
        LogError(Format('Tentative %d/%d échouée: %s',
          [Attempt, MaxAttempts, E.Message]));

        if Attempt < MaxAttempts then
        begin
          // Backoff exponentiel
          WaitTime := 1000 * (2 * Attempt);
          Sleep(WaitTime);
        end;
      end;
    end;
  end;

  // Échec après toutes les tentatives
  if not Result then
    raise Exception.Create('Paiement échoué après plusieurs tentatives');
end;
```

### 5.3 Tests de sécurité

```pascal
procedure TestPaymentSecurity;  
begin  
  // Test 1 : Montants négatifs
  try
    CreatePayment(-100, 'EUR');
    raise Exception.Create('ERREUR: Montant négatif accepté !');
  except
    on E: EArgumentException do
      WriteLn('✓ Montant négatif refusé');
  end;

  // Test 2 : Devises invalides
  try
    CreatePayment(1000, 'XXX');
    raise Exception.Create('ERREUR: Devise invalide acceptée !');
  except
    on E: Exception do
      WriteLn('✓ Devise invalide refusée');
  end;

  // Test 3 : Montants extrêmes
  try
    CreatePayment(999999999, 'EUR'); // 9,999,999.99 EUR
    raise Exception.Create('ERREUR: Montant extrême accepté !');
  except
    on E: Exception do
      WriteLn('✓ Montant extrême refusé');
  end;
end;
```

---

## Partie 6 : Tests et mise en production

### 6.1 Tests en Sandbox

**Cartes de test Stripe** :

| Carte | Usage |
|-------|-------|
| `4242 4242 4242 4242` | Succès |
| `4000 0000 0000 9995` | Fonds insuffisants |
| `4000 0000 0000 9987` | Carte perdue |
| `4000 0000 0000 0002` | Déclinée (raison générique) |
| `4000 0027 6000 3184` | 3D Secure requis |

**Codes de test** :
- CVV : N'importe quel 3 chiffres
- Date : N'importe quelle date future
- Code postal : N'importe lequel

### 6.2 Checklist avant production

**Configuration** :
- [ ] Clés de production obtenues
- [ ] Webhooks configurés avec URL HTTPS
- [ ] Certificat SSL valide
- [ ] Variables d'environnement sécurisées

**Sécurité** :
- [ ] Clés secrètes JAMAIS dans le code
- [ ] HTTPS sur toute l'application
- [ ] Validation côté serveur
- [ ] Logs de sécurité activés

**Tests** :
- [ ] Paiement réussi
- [ ] Paiement refusé
- [ ] Remboursement
- [ ] Webhooks reçus
- [ ] Abonnement créé/annulé

**Légal** :
- [ ] CGV/CGU à jour
- [ ] Politique de remboursement
- [ ] Mentions légales
- [ ] Conformité RGPD

### 6.3 Passage en production

```pascal
// Configuration environnement
type
  TPaymentEnvironment = (peTest, peProduction);

var
  Environment: TPaymentEnvironment;

procedure InitializePayment;  
begin  
  {$IFDEF DEBUG}
  Environment := peTest;
  {$ELSE}
  Environment := peProduction;
  {$ENDIF}

  case Environment of
    peTest:
      begin
        FStripe := TStripe.Create(
          GetEnvironmentVariable('STRIPE_TEST_SECRET'),
          GetEnvironmentVariable('STRIPE_TEST_PUBLIC')
        );
      end;
    peProduction:
      begin
        FStripe := TStripe.Create(
          GetEnvironmentVariable('STRIPE_LIVE_SECRET'),
          GetEnvironmentVariable('STRIPE_LIVE_PUBLIC')
        );

        // Activer les alertes
        EnablePaymentAlerts;
      end;
  end;
end;
```

### 6.4 Monitoring et alertes

```pascal
unit uPaymentMonitoring;

interface

uses
  System.SysUtils, System.Classes;

type
  TPaymentMonitor = class
  private
    FTotalPayments: Integer;
    FFailedPayments: Integer;
    FTotalAmount: Double;

    function GetSuccessRate: Double;
    procedure CheckAnomalies;
    procedure SendAlert(const AMessage: string);
  public
    procedure RecordPayment(ASuccess: Boolean; AAmount: Double);
    procedure GenerateDailyReport;

    property TotalPayments: Integer read FTotalPayments;
    property FailedPayments: Integer read FFailedPayments;
    property SuccessRate: Double read GetSuccessRate;
  end;

implementation

uses
  System.Net.HttpClient;

{ TPaymentMonitor }

function TPaymentMonitor.GetSuccessRate: Double;  
begin  
  if FTotalPayments = 0 then
    Result := 0
  else
    Result := (FTotalPayments - FFailedPayments) / FTotalPayments * 100;
end;

procedure TPaymentMonitor.RecordPayment(ASuccess: Boolean; AAmount: Double);  
begin  
  Inc(FTotalPayments);

  if ASuccess then
    FTotalAmount := FTotalAmount + AAmount
  else
    Inc(FFailedPayments);

  // Vérifier les anomalies
  CheckAnomalies;
end;

procedure TPaymentMonitor.CheckAnomalies;  
var  
  FailureRate: Double;
begin
  if FTotalPayments < 10 then
    Exit; // Pas assez de données

  FailureRate := FFailedPayments / FTotalPayments;

  // Alerter si > 10% d'échecs
  if FailureRate > 0.1 then
  begin
    SendAlert(Format(
      'Taux d''échec élevé: %.1f%% (%d/%d)',
      [FailureRate * 100, FFailedPayments, FTotalPayments]
    ));
  end;

  // Alerter si montant inhabituel
  if (FTotalAmount > 10000) and (FTotalPayments < 5) then
  begin
    SendAlert(Format(
      'Montant inhabituel: %.2f€ en %d paiements',
      [FTotalAmount, FTotalPayments]
    ));
  end;
end;

procedure TPaymentMonitor.SendAlert(const AMessage: string);  
begin  
  // Email
  // SendEmail('admin@votreapp.com', 'Alerte paiement', AMessage);

  // SMS
  // SendSMS('+33612345678', AMessage);

  // Slack
  // SendSlackMessage('#alerts', AMessage);

  // Log
  WriteLn('[ALERT] ' + AMessage);
end;

procedure TPaymentMonitor.GenerateDailyReport;  
var  
  Report: TStringList;
  SuccessRate: Double;
begin
  Report := TStringList.Create;
  try
    Report.Add('=== Rapport quotidien des paiements ===');
    Report.Add(Format('Total: %d paiements', [FTotalPayments]));
    Report.Add(Format('Réussis: %d', [FTotalPayments - FFailedPayments]));
    Report.Add(Format('Échoués: %d', [FFailedPayments]));

    if FTotalPayments > 0 then
    begin
      SuccessRate := (FTotalPayments - FFailedPayments) / FTotalPayments * 100;
      Report.Add(Format('Taux de succès: %.1f%%', [SuccessRate]));
    end;

    Report.Add(Format('Montant total: %.2f€', [FTotalAmount]));

    // Envoyer par email
    // SendEmail('admin@votreapp.com', 'Rapport quotidien', Report.Text);

  finally
    Report.Free;
  end;
end;

end.
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous savez maintenant intégrer des paiements en ligne dans vos applications Delphi. Vous maîtrisez :

✅ **Concepts de paiement** : Tokens, charges, webhooks  
✅ **Stripe** : Paiements uniques et abonnements  
✅ **PayPal** : Intégration complète  
✅ **Sécurité** : PCI DSS, HTTPS, validation  
✅ **Webhooks** : Événements en temps réel  
✅ **Tests** : Sandbox et cartes de test  
✅ **Production** : Déploiement sécurisé  
✅ **Monitoring** : Alertes et rapports

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Accepter des paiements par carte bancaire  
🎯 Gérer des abonnements récurrents  
🎯 Traiter les remboursements  
🎯 Sécuriser les transactions  
🎯 Tester en environnement sandbox  
🎯 Déployer en production  
🎯 Monitorer les paiements

### Coûts typiques

**Stripe** :
- 1.4% + 0.25€ par transaction (Europe)
- Pas de frais mensuels
- Remboursements : frais conservés

**PayPal** :
- 2.9% + 0.30€ par transaction
- Pas de frais mensuels
- Remboursements : frais remboursés

**Budget exemple** (100 ventes/mois à 50€) :
- Stripe : ~95€/mois
- PayPal : ~170€/mois

### Applications pratiques

**E-commerce** :
- Boutique en ligne
- Marketplace
- Services en ligne

**SaaS** :
- Abonnements mensuels
- Essais gratuits
- Facturation usage

**Services** :
- Réservations
- Formations
- Consultations

### Bonnes pratiques rappel

**Sécurité** :
- ✅ TOUJOURS utiliser HTTPS
- ✅ JAMAIS stocker les cartes
- ✅ Valider côté serveur
- ✅ Logger tout

**UX** :
- ✅ Messages clairs
- ✅ Loading states
- ✅ Gestion d'erreurs
- ✅ Confirmations

**Business** :
- ✅ Tester avant de lancer
- ✅ Monitorer les paiements
- ✅ CGV claires
- ✅ Support client

### Ressources complémentaires

**Documentation** :
- [Stripe Documentation](https://stripe.com/docs)
- [PayPal Developer](https://developer.paypal.com)
- [PCI Security Standards](https://www.pcisecuritystandards.org)

**Outils** :
- Stripe Dashboard pour monitoring
- PayPal Sandbox pour tests
- Postman pour tester les APIs

**Communautés** :
- Stripe Dev Discord
- PayPal Developer Forum
- Stack Overflow [stripe], [paypal]

### Évolutions possibles

**Fonctionnalités avancées** :
- Paiements fractionnés
- Marketplace (prendre une commission)
- Cryptomonnaies
- Wallets (Apple Pay, Google Pay)
- Paiements internationaux
- Facturation automatique

### Message final

Les paiements en ligne sont le cœur de nombreuses applications modernes. Avec Delphi, Stripe et PayPal, vous avez tous les outils pour créer des solutions de paiement professionnelles et sécurisées.

La sécurité est primordiale. Ne prenez jamais de raccourcis avec les données de paiement. Testez abondamment. Surveillez continuellement.

Vos utilisateurs vous font confiance avec leur argent. Honorez cette confiance en créant des systèmes fiables, transparents et sécurisés.

**Bon développement et bonnes ventes !** 💳💰🚀

---

⏭️ [Ressources et communauté](/20-ressources-et-communaute/README.md)
