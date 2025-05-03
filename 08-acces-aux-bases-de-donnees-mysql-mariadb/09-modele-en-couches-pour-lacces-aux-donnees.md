# 8.9 Modèle en couches pour l'accès aux données

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Jusqu'à présent, nous avons exploré comment connecter notre application à une base de données MySQL et manipuler les données avec différentes techniques. Cependant, dans les exemples précédents, nous avons souvent placé tout le code d'accès aux données directement dans les formulaires. Cette approche, bien que simple à comprendre pour les débutants, n'est pas idéale pour des applications professionnelles plus grandes.

Dans cette section, nous allons découvrir comment structurer notre code selon un **modèle en couches**, une approche qui sépare l'accès aux données de l'interface utilisateur et qui apporte de nombreux avantages pour le développement et la maintenance des applications.

## Pourquoi utiliser un modèle en couches ?

Avant de plonger dans l'implémentation, voyons les avantages d'un modèle en couches :

1. **Séparation des responsabilités** : Chaque partie du code a un rôle spécifique et bien défini
2. **Maintenabilité améliorée** : Il est plus facile de modifier une partie sans affecter les autres
3. **Réutilisabilité du code** : Les couches d'accès aux données peuvent être partagées entre plusieurs formulaires ou projets
4. **Testabilité** : Il est plus facile de tester chaque couche indépendamment
5. **Évolutivité** : Vous pouvez modifier une couche (par exemple, changer de base de données) sans impacter les autres

## Les différentes couches d'une application

Une architecture en couches typique pour une application Delphi comprend généralement les niveaux suivants :

![Architecture en couches](https://placeholder.pics/svg/650x400/DEDEDE/555555/Architecture%20en%20couches)

### 1. Interface Utilisateur (UI)

La couche Interface Utilisateur contient les formulaires, les contrôles visuels et la logique de présentation. C'est la partie que l'utilisateur voit et avec laquelle il interagit.

### 2. Logique Métier (Business Logic)

La couche Logique Métier contient les règles et processus de l'entreprise. Elle traite les données, effectue des calculs, applique des validations et exécute les opérations métier.

### 3. Accès aux Données (Data Access)

La couche Accès aux Données est responsable de toutes les interactions avec la base de données. Elle exécute les requêtes SQL, gère les connexions et convertit les données entre la base de données et les objets de l'application.

### 4. Modèle de Données (Data Model)

La couche Modèle de Données définit la structure des données manipulées par l'application, généralement sous forme de classes.

## Mise en œuvre du modèle en couches dans Delphi

Maintenant, voyons comment implémenter concrètement cette architecture dans une application Delphi.

### Structure du projet

D'abord, organisez votre projet en unités distinctes :

```
MonProjet/
  ├── UI/                  # Couche interface utilisateur
  │   ├── UMainForm.pas    # Formulaire principal
  │   └── UClientForm.pas  # Formulaire de gestion des clients
  ├── Business/            # Couche logique métier
  │   ├── UClientLogic.pas # Logique métier pour les clients
  │   └── ...
  ├── DataAccess/          # Couche accès aux données
  │   ├── UDataModule.pas  # Module de données pour les connexions
  │   ├── UClientDAO.pas   # Accès aux données des clients (DAO = Data Access Object)
  │   └── ...
  └── Model/               # Couche modèle de données
      ├── UClientModel.pas # Classe représentant un client
      └── ...
```

### Couche Modèle de Données

Commençons par définir notre modèle de données. Pour un client, cela pourrait ressembler à :

```delphi
// UClientModel.pas
unit UClientModel;

interface

uses
  System.SysUtils, System.Classes;

type
  TClient = class
  private
    FID: Integer;
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
    FAdresse: string;
    FVilleID: Integer;
    FDateNaissance: TDateTime;
    FActif: Boolean;
    FDateCreation: TDateTime;
  public
    constructor Create; overload;
    constructor Create(AID: Integer; ANom, APrenom, AEmail: string); overload;
    function EstValide: Boolean;
    function AgeEnAnnees: Integer;
  published  // Utilisé pour les propriétés accessibles par RTTI
    property ID: Integer read FID write FID;
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Email: string read FEmail write FEmail;
    property Telephone: string read FTelephone write FTelephone;
    property Adresse: string read FAdresse write FAdresse;
    property VilleID: Integer read FVilleID write FVilleID;
    property DateNaissance: TDateTime read FDateNaissance write FDateNaissance;
    property Actif: Boolean read FActif write FActif;
    property DateCreation: TDateTime read FDateCreation write FDateCreation;
  end;

implementation

constructor TClient.Create;
begin
  inherited Create;
  FID := 0;
  FNom := '';
  FPrenom := '';
  FEmail := '';
  FTelephone := '';
  FAdresse := '';
  FVilleID := 0;
  FDateNaissance := 0;
  FActif := True;
  FDateCreation := Now;
end;

constructor TClient.Create(AID: Integer; ANom, APrenom, AEmail: string);
begin
  Create;  // Appel au constructeur sans paramètres
  FID := AID;
  FNom := ANom;
  FPrenom := APrenom;
  FEmail := AEmail;
end;

function TClient.EstValide: Boolean;
begin
  // Vérifier que les champs obligatoires sont remplis
  Result := (FNom <> '') and (FEmail <> '');
end;

function TClient.AgeEnAnnees: Integer;
begin
  if FDateNaissance = 0 then
    Result := 0
  else
    Result := Trunc((Now - FDateNaissance) / 365.25);
end;

end.
```

### Couche Accès aux Données

Ensuite, créons un DataModule pour gérer la connexion à la base de données :

```delphi
// UDataModule.pas
unit UDataModule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, FireDAC.Comp.Client, Data.DB;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure ConfigurerConnexion;
  public
    function OuvrirConnexion: Boolean;
    procedure FermerConnexion;
    function ObtenirConnexion: TFDConnection;
  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  ConfigurerConnexion;
end;

procedure TDataModule1.ConfigurerConnexion;
begin
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=mon_utilisateur');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');
  FDConnection1.Params.Add('CharacterSet=utf8mb4');
  FDConnection1.LoginPrompt := False;
end;

function TDataModule1.OuvrirConnexion: Boolean;
begin
  Result := False;
  try
    if not FDConnection1.Connected then
      FDConnection1.Connected := True;
    Result := FDConnection1.Connected;
  except
    on E: Exception do
    begin
      // Gérer l'erreur de connexion
      // Dans une vraie application, utilisez un système de journalisation
      Result := False;
    end;
  end;
end;

procedure TDataModule1.FermerConnexion;
begin
  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;

function TDataModule1.ObtenirConnexion: TFDConnection;
begin
  Result := FDConnection1;
end;

end.
```

Puis, créons un DAO (Data Access Object) pour les clients :

```delphi
// UClientDAO.pas
unit UClientDAO;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, FireDAC.Stan.Async, FireDAC.DApt, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, UDataModule, UClientModel;

type
  TClientDAO = class
  private
    FDataModule: TDataModule1;
  public
    constructor Create(ADataModule: TDataModule1);
    destructor Destroy; override;

    // Méthodes CRUD (Create, Read, Update, Delete)
    function Ajouter(AClient: TClient): Boolean;
    function ObtenirParID(AID: Integer): TClient;
    function ObtenirTous: TFDQuery;
    function ObtenirParNom(ANom: string): TFDQuery;
    function Modifier(AClient: TClient): Boolean;
    function Supprimer(AID: Integer): Boolean;

    // Méthodes utilitaires
    function ClientExiste(AID: Integer): Boolean;
    function EmailUnique(AEmail: string; AExcludeID: Integer = 0): Boolean;
  end;

implementation

constructor TClientDAO.Create(ADataModule: TDataModule1);
begin
  inherited Create;
  FDataModule := ADataModule;
end;

destructor TClientDAO.Destroy;
begin
  // Nettoyage si nécessaire
  inherited;
end;

function TClientDAO.Ajouter(AClient: TClient): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  if not AClient.EstValide then
    Exit;

  if not EmailUnique(AClient.Email) then
    Exit;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      Query.SQL.Text := 'INSERT INTO clients (nom, prenom, email, telephone, ' +
                        'adresse, ville_id, date_naissance, actif, date_creation) ' +
                        'VALUES (:nom, :prenom, :email, :telephone, ' +
                        ':adresse, :ville_id, :date_naissance, :actif, :date_creation)';

      Query.ParamByName('nom').AsString := AClient.Nom;
      Query.ParamByName('prenom').AsString := AClient.Prenom;
      Query.ParamByName('email').AsString := AClient.Email;
      Query.ParamByName('telephone').AsString := AClient.Telephone;
      Query.ParamByName('adresse').AsString := AClient.Adresse;
      Query.ParamByName('ville_id').AsInteger := AClient.VilleID;
      Query.ParamByName('date_naissance').AsDateTime := AClient.DateNaissance;
      Query.ParamByName('actif').AsBoolean := AClient.Actif;
      Query.ParamByName('date_creation').AsDateTime := Now;

      Query.ExecSQL;

      // Récupérer l'ID généré
      AClient.ID := FDataModule.ObtenirConnexion.GetLastAutoGenValue('clients');

      Result := True;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Result := False;
    end;
  end;
end;

function TClientDAO.ObtenirParID(AID: Integer): TClient;
var
  Query: TFDQuery;
begin
  Result := nil;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      Query.SQL.Text := 'SELECT * FROM clients WHERE id = :id';
      Query.ParamByName('id').AsInteger := AID;
      Query.Open;

      if Query.RecordCount > 0 then
      begin
        Result := TClient.Create;
        Result.ID := Query.FieldByName('id').AsInteger;
        Result.Nom := Query.FieldByName('nom').AsString;
        Result.Prenom := Query.FieldByName('prenom').AsString;
        Result.Email := Query.FieldByName('email').AsString;
        Result.Telephone := Query.FieldByName('telephone').AsString;
        Result.Adresse := Query.FieldByName('adresse').AsString;
        Result.VilleID := Query.FieldByName('ville_id').AsInteger;
        Result.DateNaissance := Query.FieldByName('date_naissance').AsDateTime;
        Result.Actif := Query.FieldByName('actif').AsBoolean;
        Result.DateCreation := Query.FieldByName('date_creation').AsDateTime;
      end;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      if Assigned(Result) then
        Result.Free;
      Result := nil;
    end;
  end;
end;

function TClientDAO.ObtenirTous: TFDQuery;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);

  try
    Query.Connection := FDataModule.ObtenirConnexion;
    Query.SQL.Text := 'SELECT * FROM clients ORDER BY nom, prenom';
    Query.Open;

    Result := Query;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Query.Free;
      Result := nil;
    end;
  end;
end;

function TClientDAO.ObtenirParNom(ANom: string): TFDQuery;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);

  try
    Query.Connection := FDataModule.ObtenirConnexion;
    Query.SQL.Text := 'SELECT * FROM clients WHERE nom LIKE :nom OR prenom LIKE :nom ORDER BY nom, prenom';
    Query.ParamByName('nom').AsString := '%' + ANom + '%';
    Query.Open;

    Result := Query;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Query.Free;
      Result := nil;
    end;
  end;
end;

function TClientDAO.Modifier(AClient: TClient): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  if not AClient.EstValide then
    Exit;

  if not EmailUnique(AClient.Email, AClient.ID) then
    Exit;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      Query.SQL.Text := 'UPDATE clients SET ' +
                        'nom = :nom, ' +
                        'prenom = :prenom, ' +
                        'email = :email, ' +
                        'telephone = :telephone, ' +
                        'adresse = :adresse, ' +
                        'ville_id = :ville_id, ' +
                        'date_naissance = :date_naissance, ' +
                        'actif = :actif, ' +
                        'date_modification = :date_modification ' +
                        'WHERE id = :id';

      Query.ParamByName('nom').AsString := AClient.Nom;
      Query.ParamByName('prenom').AsString := AClient.Prenom;
      Query.ParamByName('email').AsString := AClient.Email;
      Query.ParamByName('telephone').AsString := AClient.Telephone;
      Query.ParamByName('adresse').AsString := AClient.Adresse;
      Query.ParamByName('ville_id').AsInteger := AClient.VilleID;
      Query.ParamByName('date_naissance').AsDateTime := AClient.DateNaissance;
      Query.ParamByName('actif').AsBoolean := AClient.Actif;
      Query.ParamByName('date_modification').AsDateTime := Now;
      Query.ParamByName('id').AsInteger := AClient.ID;

      Query.ExecSQL;

      Result := Query.RowsAffected > 0;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Result := False;
    end;
  end;
end;

function TClientDAO.Supprimer(AID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      Query.SQL.Text := 'DELETE FROM clients WHERE id = :id';
      Query.ParamByName('id').AsInteger := AID;
      Query.ExecSQL;

      Result := Query.RowsAffected > 0;
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      // Gérer l'erreur
      Result := False;
    end;
  end;
end;

function TClientDAO.ClientExiste(AID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      Query.SQL.Text := 'SELECT COUNT(*) FROM clients WHERE id = :id';
      Query.ParamByName('id').AsInteger := AID;
      Query.Open;

      Result := Query.Fields[0].AsInteger > 0;
    finally
      Query.Free;
    end;
  except
    // Gérer l'erreur
    Result := False;
  end;
end;

function TClientDAO.EmailUnique(AEmail: string; AExcludeID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;

  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDataModule.ObtenirConnexion;

      if AExcludeID > 0 then
      begin
        Query.SQL.Text := 'SELECT COUNT(*) FROM clients WHERE email = :email AND id <> :id';
        Query.ParamByName('id').AsInteger := AExcludeID;
      end
      else
        Query.SQL.Text := 'SELECT COUNT(*) FROM clients WHERE email = :email';

      Query.ParamByName('email').AsString := AEmail;
      Query.Open;

      Result := Query.Fields[0].AsInteger = 0;  // Aucun autre client avec cet email
    finally
      Query.Free;
    end;
  except
    // Gérer l'erreur
    Result := False;
  end;
end;

end.
```

### Couche Logique Métier

Maintenant, créons la couche logique métier qui encapsule les règles métier :

```delphi
// UClientLogic.pas
unit UClientLogic;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, Data.DB,
  UClientModel, UClientDAO, UDataModule;

type
  TClientLogic = class
  private
    FDataModule: TDataModule1;
    FClientDAO: TClientDAO;
  public
    constructor Create(ADataModule: TDataModule1);
    destructor Destroy; override;

    // Fonctionnalités métier
    function CreerClient(ANom, APrenom, AEmail, ATelephone: string;
                          AVilleID: Integer; ADateNaissance: TDateTime): Boolean;
    function MettreAJourClient(AID: Integer; ANom, APrenom, AEmail, ATelephone: string;
                          AVilleID: Integer; ADateNaissance: TDateTime; AActif: Boolean): Boolean;
    function DesactiverClient(AID: Integer): Boolean;
    function RechercherClients(ATerme: string): TFDQuery;
    function CalculerAgeClient(AID: Integer): Integer;
    function ObtenirTousLesClients: TFDQuery;
    function ObtenirClientParID(AID: Integer): TClient;
    function SupprimerClient(AID: Integer): Boolean;

    // Validations métier
    function ValiderEmail(AEmail: string): Boolean;
    function ValiderAge(ADateNaissance: TDateTime): Boolean;
  end;

implementation

constructor TClientLogic.Create(ADataModule: TDataModule1);
begin
  inherited Create;
  FDataModule := ADataModule;
  FClientDAO := TClientDAO.Create(FDataModule);
end;

destructor TClientLogic.Destroy;
begin
  FClientDAO.Free;
  inherited;
end;

function TClientLogic.CreerClient(ANom, APrenom, AEmail, ATelephone: string;
                          AVilleID: Integer; ADateNaissance: TDateTime): Boolean;
var
  Client: TClient;
begin
  Result := False;

  // Validation métier
  if (ANom = '') or (AEmail = '') then
    Exit;

  if not ValiderEmail(AEmail) then
    Exit;

  if not ValiderAge(ADateNaissance) then
    Exit;

  // Création du client
  Client := TClient.Create;
  try
    Client.Nom := ANom;
    Client.Prenom := APrenom;
    Client.Email := AEmail;
    Client.Telephone := ATelephone;
    Client.VilleID := AVilleID;
    Client.DateNaissance := ADateNaissance;
    Client.Actif := True;

    Result := FClientDAO.Ajouter(Client);
  finally
    Client.Free;
  end;
end;

function TClientLogic.MettreAJourClient(AID: Integer; ANom, APrenom, AEmail, ATelephone: string;
                          AVilleID: Integer; ADateNaissance: TDateTime; AActif: Boolean): Boolean;
var
  Client: TClient;
begin
  Result := False;

  // Validation métier
  if (ANom = '') or (AEmail = '') then
    Exit;

  if not ValiderEmail(AEmail) then
    Exit;

  if not ValiderAge(ADateNaissance) then
    Exit;

  // Récupération et mise à jour du client
  Client := FClientDAO.ObtenirParID(AID);
  if Client = nil then
    Exit;

  try
    Client.Nom := ANom;
    Client.Prenom := APrenom;
    Client.Email := AEmail;
    Client.Telephone := ATelephone;
    Client.VilleID := AVilleID;
    Client.DateNaissance := ADateNaissance;
    Client.Actif := AActif;

    Result := FClientDAO.Modifier(Client);
  finally
    Client.Free;
  end;
end;

function TClientLogic.DesactiverClient(AID: Integer): Boolean;
var
  Client: TClient;
begin
  Result := False;

  Client := FClientDAO.ObtenirParID(AID);
  if Client = nil then
    Exit;

  try
    Client.Actif := False;
    Result := FClientDAO.Modifier(Client);
  finally
    Client.Free;
  end;
end;

function TClientLogic.RechercherClients(ATerme: string): TFDQuery;
begin
  Result := FClientDAO.ObtenirParNom(ATerme);
end;

function TClientLogic.CalculerAgeClient(AID: Integer): Integer;
var
  Client: TClient;
begin
  Result := 0;

  Client := FClientDAO.ObtenirParID(AID);
  if Client <> nil then
  begin
    try
      Result := Client.AgeEnAnnees;
    finally
      Client.Free;
    end;
  end;
end;

function TClientLogic.ObtenirTousLesClients: TFDQuery;
begin
  Result := FClientDAO.ObtenirTous;
end;

function TClientLogic.ObtenirClientParID(AID: Integer): TClient;
begin
  Result := FClientDAO.ObtenirParID(AID);
end;

function TClientLogic.SupprimerClient(AID: Integer): Boolean;
begin
  Result := FClientDAO.Supprimer(AID);
end;

function TClientLogic.ValiderEmail(AEmail: string): Boolean;
begin
  // Vérification basique de format d'email
  Result := (Pos('@', AEmail) > 1) and (Pos('.', AEmail) > Pos('@', AEmail) + 1);
end;

function TClientLogic.ValiderAge(ADateNaissance: TDateTime): Boolean;
var
  Age: Integer;
begin
  if ADateNaissance = 0 then
    Result := True  // Date non spécifiée acceptée
  else
  begin
    Age := Trunc((Now - ADateNaissance) / 365.25);
    Result := (Age >= 0) and (Age <= 120);  // Vérification d'âge raisonnable
  end;
end;

end.
```

### Couche Interface Utilisateur

Enfin, créons un formulaire qui utilise notre architecture en couches :

```delphi
// UClientForm.pas
unit UClientForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Mask, Vcl.DBCtrls, FireDAC.Comp.Client,
  UDataModule, UClientLogic, UClientModel;

type
  TFormClient = class(TForm)
    PageControl1: TPageControl;
    TabSheetListe: TTabSheet;
    TabSheetDetail: TTabSheet;
    PanelRecherche: TPanel;
    DBGrid1: TDBGrid;
    LabelRecherche: TLabel;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;
    DataSource1: TDataSource;
    PanelActions: TPanel;
    ButtonNouveau: TButton;
    ButtonModifier: TButton;
    ButtonSupprimer: TButton;
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelPrenom: TLabel;
    EditPrenom: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    LabelTelephone: TLabel;
    EditTelephone: TEdit;
    LabelVille: TLabel;
    ComboBoxVille: TComboBox;
    LabelDateNaissance: TLabel;
    DateTimePickerNaissance: TDateTimePicker;
    CheckBoxActif: TCheckBox;
    ButtonEnregistrer: TButton;
    ButtonAnnuler: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure ButtonNouveauClick(Sender: TObject);
    procedure ButtonModifierClick(Sender: TObject);
    procedure ButtonSupprimerClick(Sender: TObject);
    procedure ButtonEnregistrerClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
  private
    FDataModule: TDataModule1;
    FClientLogic: TClientLogic;
    FClientQuery: TFDQuery;
    FVilleQuery: TFDQuery;
    FModeEdition: Boolean;
    FClientID: Integer;

    procedure ChargerVilles;
    procedure RafraichirListeClients;
    procedure AfficherClient(AID: Integer);
    procedure EffacerFormulaire;
    procedure ModeAffichage;
    procedure ModeEdition;
    function ValiderFormulaire: Boolean;
  public
    { Déclarations publiques }
  end;

var
  FormClient: TFormClient;

implementation

{$R *.dfm}

procedure TFormClient.FormCreate(Sender: TObject);
begin
  // Initialiser les modules et la logique
  FDataModule := TDataModule1.Create(Self);
  FClientLogic := TClientLogic.Create(FDataModule);

  // Ouvrir la connexion
  if not FDataModule.OuvrirConnexion then
  begin
    ShowMessage('Impossible de se connecter à la base de données.');
    StatusBar1.SimpleText := 'Non connecté';
    Exit;
  end;

  // Initialiser les composants
  PageControl1.ActivePage := TabSheetListe;
  FModeEdition := False;
  FClientID := 0;

  // Charger les données
  ChargerVilles;
  RafraichirListeClients;

  // Configuration de l'interface
  ModeAffichage;
  StatusBar1.SimpleText := 'Prêt';
end;

procedure TFormClient.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  if Assigned(FClientQuery) then
    FClientQuery.Free;

  if Assigned(FVilleQuery) then
    FVilleQuery.Free;

  FClientLogic.Free;

  // Fermer la connexion
  FDataModule.FermerConnexion;
end;

procedure TFormClient.ChargerVilles;
var
  Query: TFDQuery;
begin
  ComboBoxVille.Items.Clear;
  ComboBoxVille.Items.AddObject('(Aucune)', TObject(0));

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDataModule.ObtenirConnexion;
    Query.SQL.Text := 'SELECT id, nom FROM villes ORDER BY nom';
    Query.Open;

    while not Query.Eof do
    begin
      ComboBoxVille.Items.AddObject(
        Query.FieldByName('nom').AsString,
        TObject(Query.FieldByName('id').AsInteger)
      );
      Query.Next;
    end;

    ComboBoxVille.ItemIndex := 0;
  finally
    Query.Free;
  end;
end;

procedure TFormClient.RafraichirListeClients;
begin
  // Libérer la requête précédente si existante
  if Assigned(FClientQuery) then
    FreeAndNil(FClientQuery);

  // Obtenir la liste des clients via la couche logique
  FClientQuery := FClientLogic.ObtenirTousLesClients;

  if Assigned(FClientQuery) then
  begin
    // Configurer la source de données
    DataSource1.DataSet := FClientQuery;

    // Mettre à jour l'affichage
    StatusBar1.SimpleText := Format('%d clients trouvés', [FClientQuery.RecordCount]);
  end
  else
    StatusBar1.SimpleText := 'Erreur lors du chargement des clients';
end;

procedure TFormClient.ButtonRechercherClick(Sender: TObject);
var
  Terme: string;
begin
  Terme := Trim(EditRecherche.Text);

  // Libérer la requête précédente si existante
  if Assigned(FClientQuery) then
    FreeAndNil(FClientQuery);

  if Terme = '' then
    FClientQuery := FClientLogic.ObtenirTousLesClients
  else
    FClientQuery := FClientLogic.RechercherClients(Terme);

  if Assigned(FClientQuery) then
  begin
    // Configurer la source de données
    DataSource1.DataSet := FClientQuery;

    // Mettre à jour l'affichage
    StatusBar1.SimpleText := Format('%d clients trouvés', [FClientQuery.RecordCount]);
  end
  else
    StatusBar1.SimpleText := 'Erreur lors de la recherche';
end;

procedure TFormClient.ButtonNouveauClick(Sender: TObject);
begin
  EffacerFormulaire;
  FClientID := 0;
  FModeEdition := True;
  ModeEdition;
  PageControl1.ActivePage := TabSheetDetail;
  EditNom.SetFocus;
end;

procedure TFormClient.ButtonModifierClick(Sender: TObject);
begin
  if not Assigned(FClientQuery) or FClientQuery.IsEmpty then
  begin
    ShowMessage('Aucun client sélectionné.');
    Exit;
  end;

  // Charger le client sélectionné
  AfficherClient(FClientQuery.FieldByName('id').AsInteger);

  // Passer en mode édition
  FModeEdition := True;
  ModeEdition;
  PageControl1.ActivePage := TabSheetDetail;
  EditNom.SetFocus;
end;

procedure TFormClient.ButtonSupprimerClick(Sender: TObject);
begin
  if not Assigned(FClientQuery) or FClientQuery.IsEmpty then
  begin
    ShowMessage('Aucun client sélectionné.');
    Exit;
  end;

  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce client ?',
     mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if FClientLogic.SupprimerClient(FClientQuery.FieldByName('id').AsInteger) then
    begin
      ShowMessage('Client supprimé avec succès.');
      RafraichirListeClients;
    end
    else
      ShowMessage('Erreur lors de la suppression du client.');
  end;
end;

procedure TFormClient.ButtonEnregistrerClick(Sender: TObject);
var
  VilleID: Integer;
  Success: Boolean;
begin
  if not ValiderFormulaire then
    Exit;

  // Récupérer l'ID de la ville sélectionnée
  if ComboBoxVille.ItemIndex > 0 then
    VilleID := Integer(ComboBoxVille.Items.Objects[ComboBoxVille.ItemIndex])
  else
    VilleID := 0;

  // Création ou mise à jour selon le mode
  if FClientID = 0 then
  begin
    // Nouveau client
    Success := FClientLogic.CreerClient(
      EditNom.Text,
      EditPrenom.Text,
      EditEmail.Text,
      EditTelephone.Text,
      VilleID,
      DateTimePickerNaissance.Date
    );

    if Success then
      ShowMessage('Client créé avec succès.')
    else
      ShowMessage('Erreur lors de la création du client.');
  end
  else
  begin
    // Mise à jour d'un client existant
    Success := FClientLogic.MettreAJourClient(
      FClientID,
      EditNom.Text,
      EditPrenom.Text,
      EditEmail.Text,
      EditTelephone.Text,
      VilleID,
      DateTimePickerNaissance.Date,
      CheckBoxActif.Checked
    );

    if Success then
      ShowMessage('Client mis à jour avec succès.')
    else
      ShowMessage('Erreur lors de la mise à jour du client.');
  end;

  if Success then
  begin
    // Retour à la liste et rafraîchissement
    ModeAffichage;
    FModeEdition := False;
    PageControl1.ActivePage := TabSheetListe;
    RafraichirListeClients;
  end;
end;

procedure TFormClient.ButtonAnnulerClick(Sender: TObject);
begin
  // Annuler les modifications et revenir à la liste
  ModeAffichage;
  FModeEdition := False;
  PageControl1.ActivePage := TabSheetListe;
end;

procedure TFormClient.DBGrid1DblClick(Sender: TObject);
begin
  if not Assigned(FClientQuery) or FClientQuery.IsEmpty then
    Exit;

  // Afficher le client sélectionné
  AfficherClient(FClientQuery.FieldByName('id').AsInteger);
  PageControl1.ActivePage := TabSheetDetail;
end;

procedure TFormClient.AfficherClient(AID: Integer);
var
  Client: TClient;
  i: Integer;
begin
  // Récupérer le client via la couche logique
  Client := FClientLogic.ObtenirClientParID(AID);

  if not Assigned(Client) then
  begin
    ShowMessage('Client non trouvé.');
    Exit;
  end;

  try
    // Stocker l'ID pour les opérations futures
    FClientID := Client.ID;

    // Remplir les champs du formulaire
    EditNom.Text := Client.Nom;
    EditPrenom.Text := Client.Prenom;
    EditEmail.Text := Client.Email;
    EditTelephone.Text := Client.Telephone;

    // Sélectionner la ville dans la combobox
    ComboBoxVille.ItemIndex := 0;  // Par défaut "(Aucune)"
    for i := 0 to ComboBoxVille.Items.Count - 1 do
    begin
      if Integer(ComboBoxVille.Items.Objects[i]) = Client.VilleID then
      begin
        ComboBoxVille.ItemIndex := i;
        Break;
      end;
    end;

    // Autres champs
    if Client.DateNaissance > 0 then
      DateTimePickerNaissance.Date := Client.DateNaissance
    else
      DateTimePickerNaissance.Date := Date;

    CheckBoxActif.Checked := Client.Actif;

    // Afficher l'âge dans la barre d'état
    StatusBar1.SimpleText := Format('Client: %s %s, %d ans',
      [Client.Nom, Client.Prenom, Client.AgeEnAnnees]);
  finally
    Client.Free;
  end;
end;

procedure TFormClient.EffacerFormulaire;
begin
  // Réinitialiser les champs du formulaire
  EditNom.Text := '';
  EditPrenom.Text := '';
  EditEmail.Text := '';
  EditTelephone.Text := '';
  ComboBoxVille.ItemIndex := 0;
  DateTimePickerNaissance.Date := Date;
  CheckBoxActif.Checked := True;
end;

procedure TFormClient.ModeAffichage;
begin
  // Configurer l'interface en mode affichage
  EditNom.ReadOnly := True;
  EditPrenom.ReadOnly := True;
  EditEmail.ReadOnly := True;
  EditTelephone.ReadOnly := True;
  ComboBoxVille.Enabled := False;
  DateTimePickerNaissance.Enabled := False;
  CheckBoxActif.Enabled := False;

  ButtonEnregistrer.Visible := False;
  ButtonAnnuler.Visible := False;
end;

procedure TFormClient.ModeEdition;
begin
  // Configurer l'interface en mode édition
  EditNom.ReadOnly := False;
  EditPrenom.ReadOnly := False;
  EditEmail.ReadOnly := False;
  EditTelephone.ReadOnly := False;
  ComboBoxVille.Enabled := True;
  DateTimePickerNaissance.Enabled := True;
  CheckBoxActif.Enabled := True;

  ButtonEnregistrer.Visible := True;
  ButtonAnnuler.Visible := True;
end;

function TFormClient.ValiderFormulaire: Boolean;
begin
  Result := False;

  // Vérifier les champs obligatoires
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire.');
    EditNom.SetFocus;
    Exit;
  end;

  if Trim(EditEmail.Text) = '' then
  begin
    ShowMessage('L''email est obligatoire.');
    EditEmail.SetFocus;
    Exit;
  end;

  // Vérifier le format de l'email
  if not FClientLogic.ValiderEmail(EditEmail.Text) then
  begin
    ShowMessage('Format d''email invalide.');
    EditEmail.SetFocus;
    Exit;
  end;

  // Vérifier l'âge
  if not FClientLogic.ValiderAge(DateTimePickerNaissance.Date) then
  begin
    ShowMessage('Date de naissance invalide.');
    DateTimePickerNaissance.SetFocus;
    Exit;
  end;

  Result := True;
end;

end.
```

## Structure complète d'un projet en couches

Maintenant que nous avons vu comment mettre en œuvre chaque couche, voici comment organiser un projet Delphi complet en utilisant un modèle en couches :

```
MonProjet/
  ├── Sources/
  │   ├── UI/                  # Couche interface utilisateur
  │   │   ├── UMainForm.pas    # Formulaire principal
  │   │   ├── UClientForm.pas  # Formulaire de gestion des clients
  │   │   ├── UProduitForm.pas # Formulaire de gestion des produits
  │   │   └── ULoginForm.pas   # Formulaire de connexion
  │   │
  │   ├── Business/            # Couche logique métier
  │   │   ├── UClientLogic.pas # Logique métier pour les clients
  │   │   ├── UProduitLogic.pas # Logique métier pour les produits
  │   │   └── UAuthLogic.pas   # Logique d'authentification
  │   │
  │   ├── DataAccess/          # Couche accès aux données
  │   │   ├── UDataModule.pas  # Module de données principal
  │   │   ├── UClientDAO.pas   # Accès aux données des clients
  │   │   ├── UProduitDAO.pas  # Accès aux données des produits
  │   │   └── UUtilisateurDAO.pas # Accès aux données des utilisateurs
  │   │
  │   └── Model/               # Couche modèle de données
  │       ├── UClientModel.pas # Classe représentant un client
  │       ├── UProduitModel.pas # Classe représentant un produit
  │       └── UUtilisateurModel.pas # Classe représentant un utilisateur
  │
  ├── Assets/                  # Ressources (images, icônes, etc.)
  ├── Doc/                     # Documentation
  ├── Tests/                   # Tests unitaires
  └── MonProjet.dpr            # Fichier projet principal
```

Cette structure maintient une séparation claire entre les différentes responsabilités et facilite la maintenance du code.

## Avantages du modèle en couches pour les grands projets

### 1. Travail en équipe facilité

Avec une architecture en couches, plusieurs développeurs peuvent travailler sur différentes parties du projet sans se gêner mutuellement :
- Un développeur peut travailler sur l'interface utilisateur
- Un autre peut se concentrer sur la logique métier
- Un troisième peut s'occuper de l'accès aux données

### 2. Réutilisation du code

Les couches inférieures (Modèle, Accès aux Données) peuvent être réutilisées dans plusieurs projets ou dans différentes parties du même projet.

### 3. Évolutivité simplifiée

Si vous devez changer de base de données (par exemple, passer de MySQL à PostgreSQL), vous n'avez qu'à modifier la couche d'accès aux données, sans toucher à l'interface utilisateur ou à la logique métier.

### 4. Testabilité améliorée

Vous pouvez écrire des tests unitaires pour chaque couche indépendamment :
- Tests sur les modèles et leurs méthodes
- Tests sur la logique métier
- Tests simulés (mock tests) pour l'accès aux données

### 5. Maintenance simplifiée

Quand un bug apparaît, il est plus facile de le localiser dans une architecture bien structurée.

## Variations du modèle en couches

### Modèle MVC (Modèle-Vue-Contrôleur)

Le MVC est une variante populaire du modèle en couches :
- **Modèle** : Représente les données et la logique métier
- **Vue** : Gère l'interface utilisateur
- **Contrôleur** : Fait le lien entre le modèle et la vue, gère les interactions

### Modèle MVVM (Modèle-Vue-VueModèle)

Le MVVM est particulièrement adapté aux applications avec des interfaces riches :
- **Modèle** : Représente les données et la logique métier
- **Vue** : Interface utilisateur pure
- **VueModèle** : Expose les données du modèle sous une forme adaptée à la vue

### Modèle Repository

Ce modèle ajoute une abstraction supplémentaire entre la logique métier et l'accès aux données :
- **Repository** : Interface qui définit les méthodes d'accès aux données
- **DAO** : Implémentation concrète du repository

## Bonnes pratiques pour l'architecture en couches

1. **Dépendances à sens unique** : Les couches supérieures peuvent connaître les couches inférieures, mais pas l'inverse. Par exemple, la logique métier peut connaître le modèle, mais le modèle ne doit pas connaître la logique métier.

2. **Interfaces pour l'abstraction** : Utilisez des interfaces pour définir les contrats entre les couches, ce qui facilite les tests et le remplacement des implémentations.

   ```delphi
   // Définition de l'interface
   IClientDAO = interface
     ['{GUID}']
     function Ajouter(AClient: TClient): Boolean;
     function ObtenirParID(AID: Integer): TClient;
     // ...
   end;

   // Implémentation pour MySQL
   TClientDAOMySQL = class(TInterfacedObject, IClientDAO)
     // Implémentation des méthodes
   end;
   ```

3. **Injection de dépendances** : Passez les dépendances aux objets plutôt que de les créer en interne.

   ```delphi
   // Mauvaise pratique
   constructor TClientLogic.Create;
   begin
     FClientDAO := TClientDAO.Create;  // Dépendance directe
   end;

   // Bonne pratique
   constructor TClientLogic.Create(AClientDAO: IClientDAO);
   begin
     FClientDAO := AClientDAO;  // Injection de dépendance
   end;
   ```

4. **Unités séparées** : Chaque classe ou groupe de classes liées devrait être dans sa propre unité.

5. **Documentation des responsabilités** : Documentez clairement ce que chaque couche est censée faire.

## Migration vers une architecture en couches

Si vous avez déjà une application existante, voici comment migrer progressivement vers une architecture en couches :

1. **Identification des couches** : Identifiez les responsabilités dans votre code existant.

2. **Extraction du modèle** : Commencez par extraire les structures de données dans une couche modèle.

3. **Création de la couche d'accès aux données** : Déplacez progressivement le code d'accès à la base de données dans des classes DAO.

4. **Implémentation de la logique métier** : Créez des classes de logique métier qui utilisent les DAO.

5. **Refactorisation de l'interface utilisateur** : Finalement, adaptez vos formulaires pour utiliser la nouvelle logique métier au lieu d'accéder directement aux données.

## Conclusion

L'utilisation d'une architecture en couches dans vos applications Delphi avec MySQL apporte de nombreux avantages en termes de maintenabilité, d'évolutivité et de robustesse. Bien que cette approche nécessite plus de code et une réflexion plus approfondie sur la conception, elle s'avère payante sur le long terme, particulièrement pour les applications complexes ou destinées à évoluer.

Pour les débutants, il peut sembler plus simple de commencer avec une approche monolithique où tout le code est dans les formulaires. Cependant, au fur et à mesure que vous progressez, essayez d'adopter progressivement les principes de séparation des responsabilités pour améliorer la qualité de vos applications.

Dans la prochaine section, nous verrons comment gérer la migration et la synchronisation de bases de données, un aspect important pour les applications qui évoluent dans le temps.

---

**À suivre :** 8.10 Migration et synchronisation de bases de données

⏭️ [Migration et synchronisation de bases de données](08-acces-aux-bases-de-donnees-mysql-mariadb/10-migration-et-synchronisation-de-bases-de-donnees.md)
