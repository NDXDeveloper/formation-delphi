🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.2 Patterns d'architecture (MVC, MVVM)

## Introduction aux patterns d'architecture

Imaginez que vous deviez expliquer à quelqu'un comment cuisiner un plat complexe. Vous pourriez dire "mélangez tout et faites cuire", mais ce ne serait pas très utile. À la place, vous utiliseriez une **recette** : une série d'étapes éprouvées qui, si elles sont suivies, produisent un résultat délicieux.

Les patterns d'architecture sont exactement cela : des **recettes éprouvées** pour organiser votre code. Ils ne sont pas des règles absolues, mais des solutions qui ont fait leurs preuves face à des problèmes récurrents en développement logiciel.

### Qu'est-ce qu'un pattern d'architecture ?

Un pattern (ou patron en français) d'architecture est une solution réutilisable à un problème courant de conception logicielle. C'est une manière structurée d'organiser votre code pour qu'il soit :

- **Compréhensible** : Les autres développeurs (et vous-même plus tard) peuvent facilement comprendre la structure
- **Maintenable** : Modifier une partie n'affecte pas les autres
- **Testable** : Chaque partie peut être testée indépendamment
- **Évolutif** : Ajouter des fonctionnalités est simple

### Le problème que résolvent les patterns

Sans pattern, votre code ressemble souvent à cela :

```pascal
procedure TFormMain.ButtonSaveClick(Sender: TObject);
begin
  // Tout mélangé dans un même endroit !
  if EditName.Text = '' then
  begin
    ShowMessage('Nom requis !');
    Exit;
  end;

  // Calcul de la TVA
  TotalTTC := StrToFloat(EditPrix.Text) * 1.20;

  // Sauvegarde en base
  Query.SQL.Text := 'INSERT INTO clients (name, total) VALUES (:name, :total)';
  Query.ParamByName('name').AsString := EditName.Text;
  Query.ParamByName('total').AsFloat := TotalTTC;
  Query.ExecSQL;

  // Mise à jour de l'interface
  LabelStatut.Caption := 'Client sauvegardé !';
  LabelStatut.Font.Color := clGreen;
  ButtonSave.Enabled := False;
end;
```

**Les problèmes de cette approche :**

1. **Interface et logique mélangées** : Impossible de tester les calculs sans l'interface
2. **Pas réutilisable** : Si vous voulez sauvegarder depuis un autre formulaire, vous devez tout copier
3. **Difficile à maintenir** : Modifier le calcul de TVA nécessite de chercher dans tout le code des boutons
4. **Impossible à tester** : Comment tester cette fonction sans créer un formulaire complet ?

**Avec un pattern d'architecture, ce même code devient :**

- Une **Vue** qui affiche et réagit aux clics
- Un **Modèle** qui représente les données du client
- Un **Contrôleur** (ou ViewModel) qui orchestre la logique

Chaque partie a sa responsabilité et peut être modifiée indépendamment.

## Le pattern MVC (Model-View-Controller)

### Histoire et origines

Le pattern MVC a été inventé dans les années 1970 par Trygve Reenskaug pour le langage Smalltalk. C'est l'un des patterns les plus anciens et les plus utilisés en développement logiciel.

### Les trois composants

MVC divise votre application en trois parties distinctes :

```
    ┌─────────────┐
    │    VIEW     │  ← Interface utilisateur (ce que l'utilisateur voit)
    │ (Formulaire)│
    └──────┬──────┘
           │
           │ Événements (clics, saisie...)
           ↓
    ┌──────────────┐
    │ CONTROLLER   │  ← Logique de contrôle (chef d'orchestre)
    │  (Gérant)    │
    └──────┬───────┘
           │
           │ Manipulation
           ↓
    ┌──────────────┐
    │    MODEL     │  ← Données et logique métier
    │  (Données)   │
    └──────────────┘
```

#### 1. Le Model (Modèle)

Le Model représente **les données et la logique métier** de votre application. Il ne sait rien de l'interface utilisateur.

**Responsabilités :**
- Stocker les données
- Valider les données
- Effectuer les calculs métier
- Gérer la persistance (base de données)

**Ce qu'il ne fait PAS :**
- Afficher quoi que ce soit
- Gérer les événements utilisateur
- Connaître l'existence de l'interface

**Exemple concret - Modèle Client :**

```pascal
unit ModelClient;

interface

type
  TClient = class
  private
    FId: Integer;
    FNom: string;
    FEmail: string;
    FMontantTotal: Currency;
    procedure SetEmail(const Value: string);
    procedure SetNom(const Value: string);
  public
    constructor Create;

    // Validation métier
    function IsValid: Boolean;
    function GetErrorMessage: string;

    // Calculs métier
    function CalculerTotalTTC(TauxTVA: Double): Currency;

    // Propriétés
    property Id: Integer read FId write FId;
    property Nom: string read FNom write SetNom;
    property Email: string read FEmail write SetEmail;
    property MontantTotal: Currency read FMontantTotal write FMontantTotal;
  end;

implementation

uses
  System.SysUtils, System.RegularExpressions;

constructor TClient.Create;
begin
  inherited;
  FId := 0;
  FNom := '';
  FEmail := '';
  FMontantTotal := 0;
end;

procedure TClient.SetNom(const Value: string);
begin
  FNom := Trim(Value);
end;

procedure TClient.SetEmail(const Value: string);
begin
  FEmail := Trim(LowerCase(Value));
end;

function TClient.IsValid: Boolean;
var
  EmailRegex: TRegEx;
begin
  Result := False;

  // Validation du nom
  if FNom.Length < 2 then
    Exit;

  // Validation de l'email
  EmailRegex := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
  if not EmailRegex.IsMatch(FEmail) then
    Exit;

  Result := True;
end;

function TClient.GetErrorMessage: string;
var
  EmailRegex: TRegEx;
begin
  if FNom.Length < 2 then
    Exit('Le nom doit contenir au moins 2 caractères');

  EmailRegex := TRegEx.Create('^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
  if not EmailRegex.IsMatch(FEmail) then
    Exit('L''adresse email n''est pas valide');

  Result := '';
end;

function TClient.CalculerTotalTTC(TauxTVA: Double): Currency;
begin
  Result := FMontantTotal * (1 + TauxTVA / 100);
end;

end.
```

**Points clés du Model :**
- Aucune référence à l'interface (pas de `Vcl.Forms`, pas de `ShowMessage`)
- Logique métier pure (validation, calculs)
- Peut être testé indépendamment de l'interface

#### 2. La View (Vue)

La View est **l'interface utilisateur**. Elle affiche les données et capture les actions de l'utilisateur, mais ne contient aucune logique métier.

**Responsabilités :**
- Afficher les données à l'utilisateur
- Capturer les événements (clics, saisie)
- Transmettre les événements au Controller
- Mettre à jour l'affichage selon les données

**Ce qu'elle ne fait PAS :**
- Valider les données (logique métier)
- Calculer quoi que ce soit
- Accéder directement à la base de données
- Prendre des décisions métier

**Exemple concret - Formulaire Client :**

```pascal
unit ViewClientForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ControllerClient, ModelClient;

type
  TFormClient = class(TForm)
    EditNom: TEdit;
    EditEmail: TEdit;
    EditMontant: TEdit;
    ButtonSave: TButton;
    LabelNom: TLabel;
    LabelEmail: TLabel;
    LabelMontant: TLabel;
    LabelMessage: TLabel;
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FController: TClientController;
    procedure AfficherMessage(const Message: string; Erreur: Boolean);
  public
    procedure AfficherClient(Client: TClient);
    procedure ViderFormulaire;
  end;

var
  FormClient: TFormClient;

implementation

{$R *.dfm}

procedure TFormClient.FormCreate(Sender: TObject);
begin
  FController := TClientController.Create;
  ViderFormulaire;
end;

procedure TFormClient.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TFormClient.ButtonSaveClick(Sender: TObject);
var
  Nom, Email: string;
  Montant: Currency;
  Success: Boolean;
  ErrorMsg: string;
begin
  // La Vue récupère les données de l'interface
  Nom := EditNom.Text;
  Email := EditEmail.Text;

  // Tentative de conversion du montant
  if not TryStrToCurr(EditMontant.Text, Montant) then
  begin
    AfficherMessage('Le montant doit être un nombre valide', True);
    Exit;
  end;

  // La Vue délègue au Controller
  Success := FController.SauvegarderClient(Nom, Email, Montant, ErrorMsg);

  if Success then
  begin
    AfficherMessage('Client sauvegardé avec succès !', False);
    ViderFormulaire;
  end
  else
    AfficherMessage(ErrorMsg, True);
end;

procedure TFormClient.AfficherClient(Client: TClient);
begin
  EditNom.Text := Client.Nom;
  EditEmail.Text := Client.Email;
  EditMontant.Text := CurrToStr(Client.MontantTotal);
end;

procedure TFormClient.ViderFormulaire;
begin
  EditNom.Clear;
  EditEmail.Clear;
  EditMontant.Clear;
  LabelMessage.Caption := '';
  EditNom.SetFocus;
end;

procedure TFormClient.AfficherMessage(const Message: string; Erreur: Boolean);
begin
  LabelMessage.Caption := Message;
  if Erreur then
    LabelMessage.Font.Color := clRed
  else
    LabelMessage.Font.Color := clGreen;
end;

end.
```

**Points clés de la View :**
- Gère uniquement l'affichage et la capture des événements
- Délègue toute la logique au Controller
- Ne connaît pas les détails de la sauvegarde
- Peut être modifiée sans toucher au Model

#### 3. Le Controller (Contrôleur)

Le Controller est le **chef d'orchestre**. Il reçoit les événements de la View, manipule le Model, et met à jour la View.

**Responsabilités :**
- Recevoir les actions de la View
- Orchestrer les opérations
- Manipuler le Model
- Demander à la View de se mettre à jour
- Gérer le flux de l'application

**Ce qu'il ne fait PAS :**
- Afficher directement quelque chose
- Contenir de la logique métier complexe (c'est le rôle du Model)

**Exemple concret - Controller Client :**

```pascal
unit ControllerClient;

interface

uses
  ModelClient, System.SysUtils;

type
  TClientController = class
  private
    FModel: TClient;
    function SauvegarderEnBase(Client: TClient): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function SauvegarderClient(const Nom, Email: string;
      Montant: Currency; out ErrorMsg: string): Boolean;
    function ChargerClient(Id: Integer): TClient;
    function SupprimerClient(Id: Integer): Boolean;
  end;

implementation

uses
  DataModuleMain; // Module pour l'accès à la base

constructor TClientController.Create;
begin
  inherited;
  FModel := TClient.Create;
end;

destructor TClientController.Destroy;
begin
  FModel.Free;
  inherited;
end;

function TClientController.SauvegarderClient(const Nom, Email: string;
  Montant: Currency; out ErrorMsg: string): Boolean;
begin
  Result := False;
  ErrorMsg := '';

  // 1. Préparer le modèle avec les données
  FModel.Nom := Nom;
  FModel.Email := Email;
  FModel.MontantTotal := Montant;

  // 2. Valider via le modèle
  if not FModel.IsValid then
  begin
    ErrorMsg := FModel.GetErrorMessage;
    Exit;
  end;

  // 3. Sauvegarder
  try
    Result := SauvegarderEnBase(FModel);
    if not Result then
      ErrorMsg := 'Erreur lors de la sauvegarde en base de données';
  except
    on E: Exception do
    begin
      Result := False;
      ErrorMsg := 'Erreur technique : ' + E.Message;
    end;
  end;
end;

function TClientController.SauvegarderEnBase(Client: TClient): Boolean;
begin
  Result := False;

  // Utilisation du DataModule pour la persistance
  with dmMain.QueryClient do
  begin
    try
      Close;
      SQL.Text := 'INSERT INTO clients (nom, email, montant_total) ' +
                  'VALUES (:nom, :email, :montant)';
      ParamByName('nom').AsString := Client.Nom;
      ParamByName('email').AsString := Client.Email;
      ParamByName('montant').AsCurrency := Client.MontantTotal;
      ExecSQL;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

function TClientController.ChargerClient(Id: Integer): TClient;
begin
  Result := TClient.Create;

  with dmMain.QueryClient do
  begin
    Close;
    SQL.Text := 'SELECT * FROM clients WHERE id = :id';
    ParamByName('id').AsInteger := Id;
    Open;

    if not Eof then
    begin
      Result.Id := FieldByName('id').AsInteger;
      Result.Nom := FieldByName('nom').AsString;
      Result.Email := FieldByName('email').AsString;
      Result.MontantTotal := FieldByName('montant_total').AsCurrency;
    end;
  end;
end;

function TClientController.SupprimerClient(Id: Integer): Boolean;
begin
  Result := False;

  with dmMain.QueryClient do
  begin
    try
      Close;
      SQL.Text := 'DELETE FROM clients WHERE id = :id';
      ParamByName('id').AsInteger := Id;
      ExecSQL;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

end.
```

**Points clés du Controller :**
- Orchestre les opérations entre View et Model
- Gère les erreurs et les retours
- Isole la base de données du reste de l'application
- Peut être réutilisé avec différentes Views

### Flux de données en MVC

Voici comment les composants interagissent :

```
1. L'utilisateur clique sur "Sauvegarder"
   │
   ↓
2. La VIEW capture l'événement
   │
   ↓
3. La VIEW appelle le CONTROLLER
   │   (en lui passant les données saisies)
   ↓
4. Le CONTROLLER crée/met à jour le MODEL
   │
   ↓
5. Le MODEL valide les données
   │
   ↓
6. Le CONTROLLER sauvegarde via le MODEL
   │
   ↓
7. Le CONTROLLER retourne le résultat à la VIEW
   │
   ↓
8. La VIEW affiche le message de succès ou d'erreur
```

### Avantages du MVC

**1. Testabilité**
```pascal
// Test du modèle sans interface
procedure TestValidationClient;
var
  Client: TClient;
begin
  Client := TClient.Create;
  try
    Client.Nom := 'Jean';
    Client.Email := 'jean@example.com';
    Assert(Client.IsValid, 'Le client devrait être valide');

    Client.Email := 'email-invalide';
    Assert(not Client.IsValid, 'Le client ne devrait pas être valide');
  finally
    Client.Free;
  end;
end;
```

**2. Réutilisabilité**

Vous pouvez utiliser le même Model et Controller avec différentes Views :
- Une interface VCL pour Windows
- Une interface FMX pour mobile
- Une API REST
- Un service batch

**3. Maintenabilité**

Chaque composant peut être modifié indépendamment :
- Changer l'apparence ? Modifiez la View uniquement
- Nouvelle règle métier ? Modifiez le Model uniquement
- Nouvelle base de données ? Modifiez le Controller uniquement

### Inconvénients du MVC

**1. Complexité initiale**

Pour une petite application, MVC peut sembler "too much". Il faut créer plusieurs fichiers même pour une fonctionnalité simple.

**2. Courbe d'apprentissage**

Comprendre où mettre chaque morceau de code demande de l'expérience.

**3. Communication indirecte**

Le Controller peut devenir un goulot d'étranglement si mal conçu.

## Le pattern MVVM (Model-View-ViewModel)

### Histoire et origines

MVVM a été introduit par Microsoft en 2005 pour WPF (Windows Presentation Foundation). Il est une évolution du MVC, spécialement adapté aux frameworks avec du data binding (liaison de données).

### Les trois composants

MVVM est similaire à MVC, mais avec une différence importante dans le rôle du composant intermédiaire :

```
    ┌─────────────┐
    │    VIEW     │  ← Interface utilisateur
    │ (Formulaire)│
    └──────┬──────┘
           │
           │ Data Binding (liaison bidirectionnelle)
           ↓
    ┌──────────────┐
    │  VIEWMODEL   │  ← Représentation de la vue en données
    │ (Adaptateur) │
    └──────┬───────┘
           │
           │ Utilisation
           ↓
    ┌──────────────┐
    │    MODEL     │  ← Données et logique métier
    │  (Données)   │
    └──────────────┘
```

#### 1. Le Model

Le Model en MVVM est identique au Model en MVC : il représente les données et la logique métier.

```pascal
// Même code que pour MVC
unit ModelClient;

interface

type
  TClient = class
  private
    FId: Integer;
    FNom: string;
    FEmail: string;
    FMontantTotal: Currency;
  public
    function IsValid: Boolean;
    function CalculerTotalTTC(TauxTVA: Double): Currency;

    property Id: Integer read FId write FId;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
    property MontantTotal: Currency read FMontantTotal write FMontantTotal;
  end;

implementation

// ... même implémentation que précédemment

end.
```

#### 2. La View

La View en MVVM est encore plus passive qu'en MVC. Elle fait principalement du data binding avec le ViewModel.

```pascal
unit ViewClientForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Bind.DBScope,
  Data.Bind.EngExt, Vcl.Bind.DBEngExt, Data.Bind.Components,
  ViewModelClient;

type
  TFormClient = class(TForm)
    EditNom: TEdit;
    EditEmail: TEdit;
    EditMontant: TEdit;
    ButtonSave: TButton;
    LabelMessage: TLabel;
    // Composants de liaison de données
    BindingsList: TBindingsList;
    BindSourceClient: TBindSourceDB;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    FViewModel: TClientViewModel;
  public
  end;

var
  FormClient: TFormClient;

implementation

{$R *.dfm}

procedure TFormClient.FormCreate(Sender: TObject);
begin
  FViewModel := TClientViewModel.Create;

  // Configuration du data binding
  // (Dans une vraie application, ceci serait fait avec LiveBindings)
  FViewModel.OnPropertyChanged := procedure(const PropertyName: string)
  begin
    if PropertyName = 'Message' then
    begin
      LabelMessage.Caption := FViewModel.Message;
      if FViewModel.EstErreur then
        LabelMessage.Font.Color := clRed
      else
        LabelMessage.Font.Color := clGreen;
    end
    else if PropertyName = 'Nom' then
      EditNom.Text := FViewModel.Nom
    else if PropertyName = 'Email' then
      EditEmail.Text := FViewModel.Email
    else if PropertyName = 'MontantAffichage' then
      EditMontant.Text := FViewModel.MontantAffichage;
  end;
end;

procedure TFormClient.FormDestroy(Sender: TObject);
begin
  FViewModel.Free;
end;

procedure TFormClient.ButtonSaveClick(Sender: TObject);
begin
  // La View ne fait que transmettre les données au ViewModel
  FViewModel.Nom := EditNom.Text;
  FViewModel.Email := EditEmail.Text;
  FViewModel.MontantAffichage := EditMontant.Text;

  // Et déclencher l'action
  FViewModel.Sauvegarder;
end;

end.
```

#### 3. Le ViewModel

Le ViewModel est **une représentation de la View en termes de données**. C'est un adaptateur entre le Model (données métier) et la View (données d'affichage).

**Responsabilités :**
- Exposer les données dans un format adapté à la View
- Gérer les commandes (actions utilisateur)
- Implémenter la logique de présentation
- Notifier la View des changements (via des événements ou le binding)

```pascal
unit ViewModelClient;

interface

uses
  System.SysUtils, System.Classes, ModelClient;

type
  TPropertyChangedProc = reference to procedure(const PropertyName: string);

  TClientViewModel = class
  private
    FClient: TClient;
    FMessage: string;
    FEstErreur: Boolean;
    FOnPropertyChanged: TPropertyChangedProc;

    function GetNom: string;
    procedure SetNom(const Value: string);
    function GetEmail: string;
    procedure SetEmail(const Value: string);
    function GetMontantAffichage: string;
    procedure SetMontantAffichage(const Value: string);

    procedure NotifierChangement(const PropertyName: string);
    procedure DefinirMessage(const Msg: string; Erreur: Boolean);
    function SauvegarderEnBase: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // Propriétés liées à la vue
    property Nom: string read GetNom write SetNom;
    property Email: string read GetEmail write SetEmail;
    property MontantAffichage: string read GetMontantAffichage write SetMontantAffichage;
    property Message: string read FMessage;
    property EstErreur: Boolean read FEstErreur;

    // Commandes (actions)
    procedure Sauvegarder;
    procedure NouveauClient;

    // Notification de changement
    property OnPropertyChanged: TPropertyChangedProc read FOnPropertyChanged write FOnPropertyChanged;
  end;

implementation

uses
  DataModuleMain;

constructor TClientViewModel.Create;
begin
  inherited;
  FClient := TClient.Create;
  FMessage := '';
  FEstErreur := False;
end;

destructor TClientViewModel.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TClientViewModel.GetNom: string;
begin
  Result := FClient.Nom;
end;

procedure TClientViewModel.SetNom(const Value: string);
begin
  if FClient.Nom <> Value then
  begin
    FClient.Nom := Value;
    NotifierChangement('Nom');
  end;
end;

function TClientViewModel.GetEmail: string;
begin
  Result := FClient.Email;
end;

procedure TClientViewModel.SetEmail(const Value: string);
begin
  if FClient.Email <> Value then
  begin
    FClient.Email := Value;
    NotifierChangement('Email');
  end;
end;

function TClientViewModel.GetMontantAffichage: string;
begin
  // Conversion du modèle vers l'affichage
  if FClient.MontantTotal = 0 then
    Result := ''
  else
    Result := FormatFloat('#,##0.00', FClient.MontantTotal);
end;

procedure TClientViewModel.SetMontantAffichage(const Value: string);
var
  Montant: Currency;
begin
  // Conversion de l'affichage vers le modèle
  if Value = '' then
    FClient.MontantTotal := 0
  else if TryStrToCurr(StringReplace(Value, ' ', '', [rfReplaceAll]), Montant) then
  begin
    FClient.MontantTotal := Montant;
    NotifierChangement('MontantAffichage');
  end;
end;

procedure TClientViewModel.NotifierChangement(const PropertyName: string);
begin
  if Assigned(FOnPropertyChanged) then
    FOnPropertyChanged(PropertyName);
end;

procedure TClientViewModel.DefinirMessage(const Msg: string; Erreur: Boolean);
begin
  FMessage := Msg;
  FEstErreur := Erreur;
  NotifierChangement('Message');
end;

procedure TClientViewModel.Sauvegarder;
begin
  // 1. Validation
  if not FClient.IsValid then
  begin
    DefinirMessage(FClient.GetErrorMessage, True);
    Exit;
  end;

  // 2. Sauvegarde
  try
    if SauvegarderEnBase then
    begin
      DefinirMessage('Client sauvegardé avec succès !', False);
      NouveauClient;
    end
    else
      DefinirMessage('Erreur lors de la sauvegarde', True);
  except
    on E: Exception do
      DefinirMessage('Erreur : ' + E.Message, True);
  end;
end;

function TClientViewModel.SauvegarderEnBase: Boolean;
begin
  Result := False;

  with dmMain.QueryClient do
  begin
    try
      Close;
      SQL.Text := 'INSERT INTO clients (nom, email, montant_total) ' +
                  'VALUES (:nom, :email, :montant)';
      ParamByName('nom').AsString := FClient.Nom;
      ParamByName('email').AsString := FClient.Email;
      ParamByName('montant').AsCurrency := FClient.MontantTotal;
      ExecSQL;
      Result := True;
    except
      Result := False;
    end;
  end;
end;

procedure TClientViewModel.NouveauClient;
begin
  FClient.Free;
  FClient := TClient.Create;
  NotifierChangement('Nom');
  NotifierChangement('Email');
  NotifierChangement('MontantAffichage');
end;

end.
```

### Différences clés entre MVC et MVVM

| Aspect | MVC | MVVM |
|--------|-----|------|
| **Composant intermédiaire** | Controller (orchestrateur) | ViewModel (représentation) |
| **Communication View-Modèle** | Via le Controller | Via le ViewModel avec binding |
| **Logique de présentation** | Dans le Controller | Dans le ViewModel |
| **Couplage View-Modèle** | La View connaît le Controller | La View se lie au ViewModel |
| **Testabilité** | Très bonne | Excellente |
| **Data Binding** | Optionnel | Central au pattern |

### Avantages du MVVM

**1. Séparation ultra-claire**

Le ViewModel ne contient que de la logique de présentation. Aucune référence à l'UI :

```pascal
// Test du ViewModel sans interface graphique
procedure TestSauvegardeClient;
var
  ViewModel: TClientViewModel;
  MessageRecu: string;
begin
  ViewModel := TClientViewModel.Create;
  try
    ViewModel.OnPropertyChanged := procedure(const PropertyName: string)
    begin
      if PropertyName = 'Message' then
        MessageRecu := ViewModel.Message;
    end;

    ViewModel.Nom := 'Test';
    ViewModel.Email := 'test@example.com';
    ViewModel.MontantAffichage := '100.00';

    ViewModel.Sauvegarder;

    Assert(MessageRecu.Contains('succès'), 'La sauvegarde devrait réussir');
  finally
    ViewModel.Free;
  end;
end;
```

**2. Logique de présentation centralisée**

Toute la conversion entre données métier et affichage est dans le ViewModel :

```pascal
// Formatage pour l'affichage
function TClientViewModel.GetMontantAffichage: string;
begin
  Result := FormatFloat('#,##0.00 €', FClient.MontantTotal);
end;

// Parsing depuis l'affichage
procedure TClientViewModel.SetMontantAffichage(const Value: string);
var
  Montant: Currency;
  ValueNettoyee: string;
begin
  ValueNettoyee := StringReplace(Value, '€', '', [rfReplaceAll]);
  ValueNettoyee := StringReplace(ValueNettoyee, ' ', '', [rfReplaceAll]);

  if TryStrToCurr(ValueNettoyee, Montant) then
    FClient.MontantTotal := Montant;
end;
```

**3. Data Binding puissant**

Avec les LiveBindings de Delphi, la View se met à jour automatiquement :

```pascal
// La View n'a presque plus de code !
// Le binding fait tout automatiquement
```

### Inconvénients du MVVM

**1. Courbe d'apprentissage plus élevée**

Le concept de ViewModel et du binding n'est pas immédiat à comprendre.

**2. Plus de code initial**

Il faut créer le ViewModel en plus du Model et de la View.

**3. Complexité du binding**

Le système de binding peut être difficile à déboguer quand ça ne fonctionne pas.

## Autres patterns d'architecture pertinents

### MVP (Model-View-Presenter)

MVP est une variante de MVC où le Presenter a un contrôle total sur la View.

```
    ┌─────────────┐
    │    VIEW     │  ← Interface passive
    │ (Formulaire)│
    └──────┬──────┘
           ↕
    ┌──────────────┐
    │  PRESENTER   │  ← Contrôle total de la View
    │  (Contrôle)  │
    └──────┬───────┘
           │
           ↓
    ┌──────────────┐
    │    MODEL     │  ← Données
    └──────────────┘
```

**Différence principale** : Dans MVP, la View est totalement passive. Le Presenter lui dit exactement quoi afficher.

```pascal
// Interface de la View (contrat)
type
  IClientView = interface
    ['{GUID}']
    procedure AfficherNom(const Nom: string);
    procedure AfficherEmail(const Email: string);
    procedure AfficherMessage(const Message: string; Erreur: Boolean);
    procedure ViderFormulaire;
  end;

// Le Presenter manipule la View via l'interface
type
  TClientPresenter = class
  private
    FView: IClientView;
    FModel: TClient;
  public
    constructor Create(AView: IClientView);
    procedure SauvegarderClient(const Nom, Email: string; Montant: Currency);
    procedure ChargerClient(Id: Integer);
  end;
```

**Avantage** : La View est facilement testable avec un Mock.

### Clean Architecture

Clean Architecture (Architecture Propre) est une approche proposée par Robert C. Martin (Uncle Bob) qui met l'accent sur l'indépendance des frameworks.

```
┌─────────────────────────────────────┐
│        UI / Frameworks              │  ← VCL, FMX, Web
├─────────────────────────────────────┤
│     Interface Adapters              │  ← Controllers, Presenters
├─────────────────────────────────────┤
│      Business Logic                 │  ← Use Cases
├─────────────────────────────────────┤
│        Entities                     │  ← Models métier
└─────────────────────────────────────┘

Dépendances : Toujours de l'extérieur vers l'intérieur
```

**Principe** : Le cœur de l'application (Entities) ne dépend de rien. Tout dépend de lui.

### Quand utiliser quel pattern ?

| Pattern | Quand l'utiliser |
|---------|------------------|
| **MVC** | - Application de taille moyenne<br>- Logique métier complexe<br>- Pas de binding nécessaire<br>- Équipe habituée à MVC |
| **MVVM** | - Application avec beaucoup de binding<br>- UI complexe qui doit réagir aux données<br>- FMX ou applications modernes<br>- Tests unitaires prioritaires |
| **MVP** | - Tests ultra-importants<br>- View très passive souhaitée<br>- Contrôle total sur l'UI nécessaire |
| **Clean Architecture** | - Grandes applications d'entreprise<br>- Portabilité essentielle<br>- Changements de framework fréquents |

## Exemple complet : Comparaison MVC vs MVVM

Prenons un cas concret : une liste de clients avec recherche et édition.

### Version MVC

```pascal
// ========== MODEL ==========
unit ModelClient;

interface

type
  TClient = class
  private
    FId: Integer;
    FNom: string;
    FEmail: string;
  public
    property Id: Integer read FId write FId;
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
    function IsValid: Boolean;
  end;

  TClientList = class
  private
    FItems: TList<TClient>;
  public
    constructor Create;
    destructor Destroy; override;
    function Search(const Terme: string): TArray<TClient>;
    function GetAll: TArray<TClient>;
    property Items: TList<TClient> read FItems;
  end;

implementation

// ... implémentation

end.

// ========== CONTROLLER ==========
unit ControllerClient;

interface

uses
  ModelClient;

type
  TClientController = class
  private
    FClientList: TClientList;
  public
    constructor Create;
    destructor Destroy; override;
    function RechercherClients(const Terme: string): TArray<TClient>;
    function ChargerTous: TArray<TClient>;
    function SauvegarderClient(Client: TClient): Boolean;
  end;

implementation

// ... implémentation

end.

// ========== VIEW ==========
unit ViewClientList;

interface

uses
  Vcl.Forms, Vcl.Grids, Vcl.StdCtrls, ControllerClient, ModelClient;

type
  TFormClientList = class(TForm)
    EditRecherche: TEdit;
    GridClients: TStringGrid;
    ButtonRechercher: TButton;
    procedure ButtonRechercherClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FController: TClientController;
    procedure AfficherClients(Clients: TArray<TClient>);
  end;

implementation

procedure TFormClientList.FormCreate(Sender: TObject);
begin
  FController := TClientController.Create;
  AfficherClients(FController.ChargerTous);
end;

procedure TFormClientList.ButtonRechercherClick(Sender: TObject);
var
  Clients: TArray<TClient>;
begin
  Clients := FController.RechercherClients(EditRecherche.Text);
  AfficherClients(Clients);
end;

procedure TFormClientList.AfficherClients(Clients: TArray<TClient>);
var
  I: Integer;
begin
  GridClients.RowCount := Length(Clients) + 1;
  for I := 0 to High(Clients) do
  begin
    GridClients.Cells[0, I + 1] := IntToStr(Clients[I].Id);
    GridClients.Cells[1, I + 1] := Clients[I].Nom;
    GridClients.Cells[2, I + 1] := Clients[I].Email;
  end;
end;

end.
```

### Version MVVM

```pascal
// ========== MODEL ==========
// (identique à MVC)

// ========== VIEWMODEL ==========
unit ViewModelClientList;

interface

uses
  System.Classes, System.Generics.Collections, ModelClient;

type
  TClientViewModel = class
  private
    FNom: string;
    FEmail: string;
  public
    property Nom: string read FNom write FNom;
    property Email: string read FEmail write FEmail;
  end;

  TClientListViewModel = class
  private
    FClientList: TClientList;
    FClients: TObjectList<TClientViewModel>;
    FTermeRecherche: string;
    FOnClientsChanged: TNotifyEvent;
    procedure SetTermeRecherche(const Value: string);
    procedure ChargerClients(Clients: TArray<TClient>);
  public
    constructor Create;
    destructor Destroy; override;

    property TermeRecherche: string read FTermeRecherche write SetTermeRecherche;
    property Clients: TObjectList<TClientViewModel> read FClients;
    property OnClientsChanged: TNotifyEvent read FOnClientsChanged write FOnClientsChanged;

    procedure Rechercher;
    procedure ChargerTous;
  end;

implementation

constructor TClientListViewModel.Create;
begin
  inherited;
  FClientList := TClientList.Create;
  FClients := TObjectList<TClientViewModel>.Create(True);
  ChargerTous;
end;

destructor TClientListViewModel.Destroy;
begin
  FClients.Free;
  FClientList.Free;
  inherited;
end;

procedure TClientListViewModel.SetTermeRecherche(const Value: string);
begin
  if FTermeRecherche <> Value then
  begin
    FTermeRecherche := Value;
    Rechercher;
  end;
end;

procedure TClientListViewModel.Rechercher;
var
  Resultats: TArray<TClient>;
begin
  if FTermeRecherche = '' then
    Resultats := FClientList.GetAll
  else
    Resultats := FClientList.Search(FTermeRecherche);

  ChargerClients(Resultats);
end;

procedure TClientListViewModel.ChargerTous;
begin
  ChargerClients(FClientList.GetAll);
end;

procedure TClientListViewModel.ChargerClients(Clients: TArray<TClient>);
var
  I: Integer;
  VM: TClientViewModel;
begin
  FClients.Clear;

  for I := 0 to High(Clients) do
  begin
    VM := TClientViewModel.Create;
    VM.Nom := Clients[I].Nom;
    VM.Email := Clients[I].Email;
    FClients.Add(VM);
  end;

  if Assigned(FOnClientsChanged) then
    FOnClientsChanged(Self);
end;

end.

// ========== VIEW ==========
unit ViewClientList;

interface

uses
  Vcl.Forms, Vcl.Grids, Vcl.StdCtrls, ViewModelClientList;

type
  TFormClientList = class(TForm)
    EditRecherche: TEdit;
    GridClients: TStringGrid;
    procedure EditRechercheChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FViewModel: TClientListViewModel;
    procedure OnClientsChanged(Sender: TObject);
  end;

implementation

procedure TFormClientList.FormCreate(Sender: TObject);
begin
  FViewModel := TClientListViewModel.Create;
  FViewModel.OnClientsChanged := OnClientsChanged;
end;

procedure TFormClientList.EditRechercheChange(Sender: TObject);
begin
  // Le binding met à jour automatiquement
  FViewModel.TermeRecherche := EditRecherche.Text;
end;

procedure TFormClientList.OnClientsChanged(Sender: TObject);
var
  I: Integer;
begin
  GridClients.RowCount := FViewModel.Clients.Count + 1;

  for I := 0 to FViewModel.Clients.Count - 1 do
  begin
    GridClients.Cells[0, I + 1] := FViewModel.Clients[I].Nom;
    GridClients.Cells[1, I + 1] := FViewModel.Clients[I].Email;
  end;
end;

end.
```

**Observation** : Dans MVVM, la View est plus simple. Elle ne fait que du binding. Toute la logique est dans le ViewModel.

## Conseils pratiques

### Commencez simple

Ne vous lancez pas dans un pattern complexe pour une application de 3 formulaires. Commencez par :

1. Séparer les formulaires du code métier
2. Créer des unités distinctes pour différentes responsabilités
3. Progressivement adopter un pattern complet

### Choisissez selon votre contexte

**Utilisez MVC si :**
- Vous travaillez en VCL principalement
- Votre équipe connaît déjà MVC
- Le binding n'est pas crucial

**Utilisez MVVM si :**
- Vous travaillez en FMX
- Vous avez beaucoup de binding à faire
- La testabilité est critique

### Soyez cohérent

Une fois que vous avez choisi un pattern, **tenez-vous-y** dans tout le projet. Mélanger les patterns crée de la confusion.

### N'en faites pas trop

Les patterns sont des outils, pas des objectifs. Ne compliquez pas votre code pour "respecter le pattern". Parfois, une solution simple est meilleure qu'un pattern parfait.

### Documentez vos choix

Expliquez dans un README ou un document pourquoi vous avez choisi tel pattern, comment il est implémenté dans votre projet.

## Erreurs courantes à éviter

### 1. Mélanger les responsabilités

**Mauvais** :
```pascal
// Controller qui fait de l'affichage
procedure TClientController.Sauvegarder;
begin
  if Save then
    ShowMessage('Sauvegardé !'); // NON ! Le Controller ne doit pas afficher
end;
```

**Bon** :
```pascal
// Le Controller retourne le résultat, la View affiche
function TClientController.Sauvegarder: Boolean;
begin
  Result := Save;
end;
```

### 2. Model qui connaît la View

**Mauvais** :
```pascal
type
  TClient = class
  private
    FFormulaire: TForm; // NON ! Le Model ne doit pas connaître la View
  end;
```

### 3. Tout mettre dans le Controller/ViewModel

Le Controller/ViewModel n'est pas un fourre-tout. La logique métier va dans le Model !

### 4. View qui accède directement à la base

**Mauvais** :
```pascal
procedure TFormClient.ButtonSaveClick(Sender: TObject);
begin
  Query.SQL.Text := 'INSERT...'; // NON ! Passez par le Controller/ViewModel
  Query.ExecSQL;
end;
```

## Conclusion

Les patterns d'architecture comme MVC et MVVM sont des outils puissants pour créer des applications maintenables et évolutives. Ils peuvent sembler complexes au début, mais avec la pratique, ils deviennent naturels.

**Points clés à retenir :**

1. **MVC** sépare en Model (données), View (UI) et Controller (orchestrateur)
2. **MVVM** sépare en Model (données), View (UI) et ViewModel (représentation + binding)
3. Choisissez le pattern adapté à votre contexte
4. La séparation des responsabilités est l'objectif principal
5. Commencez simple et progressez graduellement
6. Restez cohérent dans votre application

Les patterns d'architecture ne sont pas une fin en soi, mais un moyen d'atteindre un code de meilleure qualité. Utilisez-les avec discernement et pragmatisme.

Dans la prochaine section, nous approfondirons la séparation entre l'interface utilisateur et la logique métier, en voyant des techniques concrètes pour y parvenir efficacement.

⏭️ [Séparation UI / logique métier](/18-architecture-et-bonnes-pratiques/03-separation-ui-logique-metier.md)
