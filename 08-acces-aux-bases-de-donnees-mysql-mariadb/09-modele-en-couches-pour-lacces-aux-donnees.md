🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.9 Modèle en couches pour l'accès aux données

## Introduction

Lorsque vous créez une application de base de données, il est tentant de tout mettre dans le formulaire : la connexion, les requêtes, la logique métier, et l'interface utilisateur. Cela fonctionne pour de petites applications, mais devient rapidement **ingérable** pour des projets plus importants.

Le **modèle en couches** (ou architecture en couches) est une approche qui **sépare** les différentes responsabilités de votre application en modules distincts. C'est une des meilleures pratiques du développement logiciel professionnel.

## Pourquoi séparer en couches ?

### Le problème de l'approche monolithique

Imaginez un formulaire qui fait tout :

```pascal
// ❌ TOUT dans le formulaire - Architecture monolithique
procedure TFormClients.btnSauvegarderClick(Sender: TObject);
begin
  // Connexion à la base
  FDConnection1.Params.Values['Server'] := 'localhost';
  FDConnection1.Params.Values['Database'] := 'ma_base';
  FDConnection1.Connected := True;

  // Validation métier
  if Trim(editNom.Text) = '' then
    raise Exception.Create('Nom obligatoire');
  if Pos('@', editEmail.Text) = 0 then
    raise Exception.Create('Email invalide');
  if CalculerAge(DateNaissance) < 18 then
    raise Exception.Create('Le client doit être majeur');

  // SQL
  FDQuery1.SQL.Text :=
    'INSERT INTO clients (nom, prenom, email, date_naissance) ' +
    'VALUES (:nom, :prenom, :email, :date)';
  FDQuery1.ParamByName('nom').AsString := editNom.Text;
  FDQuery1.ParamByName('prenom').AsString := editPrenom.Text;
  FDQuery1.ParamByName('email').AsString := editEmail.Text;
  FDQuery1.ParamByName('date').AsDate := DateNaissance;
  FDQuery1.ExecSQL;

  // Mise à jour de l'interface
  ChargerClients;
  ShowMessage('Client sauvegardé');
end;
```

**Problèmes :**
- 🔴 Code **dupliqué** dans chaque formulaire
- 🔴 **Difficile à tester** (tout est lié à l'interface)
- 🔴 **Impossible de réutiliser** la logique
- 🔴 **Modification complexe** : changer la base touche tous les formulaires
- 🔴 **Maintenance cauchemardesque** pour de grandes applications

### L'approche en couches

```
┌─────────────────────────────────────────┐
│     COUCHE PRÉSENTATION (UI)            │
│  FormClients, FormCommandes, etc.       │
│  - Affichage                            │
│  - Interaction utilisateur              │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│     COUCHE MÉTIER (Business Logic)      │
│  TClientManager, TCommandeManager       │
│  - Règles métier                        │
│  - Validation                           │
│  - Calculs                              │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│     COUCHE DONNÉES (Data Access)        │
│  TDataModule, TClientDAO                │
│  - Connexion base de données            │
│  - Requêtes SQL                         │
│  - Gestion CRUD                         │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│     BASE DE DONNÉES                     │
│  MySQL/MariaDB                          │
└─────────────────────────────────────────┘
```

**Avantages :**
- ✅ Code **organisé** et **maintenable**
- ✅ Facilement **testable** (chaque couche séparément)
- ✅ **Réutilisable** (même logique dans plusieurs formulaires)
- ✅ **Évolutif** (changer une couche sans toucher les autres)
- ✅ **Travail en équipe** facilité (chacun sur sa couche)

## Les trois couches principales

### 1. Couche Présentation (UI Layer)

**Responsabilités :**
- Afficher les données
- Gérer les interactions utilisateur (clics, saisies)
- Mettre à jour l'interface

**Ce qu'elle NE fait PAS :**
- ❌ Validation métier complexe
- ❌ Connexion directe à la base
- ❌ Logique de calcul

**Composants :**
- Formulaires (TForm)
- Contrôles visuels (Button, Edit, Grid)
- DataSources et contrôles DB

### 2. Couche Métier (Business Logic Layer)

**Responsabilités :**
- Implémenter les **règles métier**
- **Valider** les données
- Effectuer les **calculs** complexes
- Coordonner les opérations

**Ce qu'elle NE fait PAS :**
- ❌ Affichage (pas de ShowMessage)
- ❌ SQL direct
- ❌ Gestion de l'interface

**Composants :**
- Classes métier (TClientManager, TFactureManager)
- Objets métier (TClient, TCommande)
- Services (TEmailService, TReportService)

### 3. Couche Données (Data Access Layer)

**Responsabilités :**
- **Connexion** à la base de données
- Exécution des **requêtes SQL**
- Opérations **CRUD**
- Gestion des **transactions**

**Ce qu'elle NE fait PAS :**
- ❌ Validation métier
- ❌ Affichage
- ❌ Logique complexe

**Composants :**
- DataModule (TDataModule)
- Classes DAO (Data Access Object)
- Composants FireDAC

## Mise en pratique : Le DataModule

Le **DataModule** est le composant idéal pour implémenter la couche d'accès aux données.

### Qu'est-ce qu'un DataModule ?

Un **DataModule** est un conteneur **non-visuel** pour les composants de données. C'est comme un formulaire, mais sans interface graphique.

### Créer un DataModule

#### Étape 1 : Créer le DataModule

1. **Fichier** → **Nouveau** → **Autre...**
2. Dans la catégorie **Delphi Files**, sélectionnez **DataModule**
3. Nommez-le : `dmDatabase` (Data Module Database)
4. Sauvegardez : `uDmDatabase.pas`

#### Étape 2 : Ajouter les composants FireDAC

Sur le DataModule, placez :

- **TFDConnection** : la connexion principale
- **TFDPhysMySQLDriverLink** : le pilote MySQL
- **TFDQuery** pour chaque entité (Clients, Commandes, etc.)
- **TDataSource** correspondants

```pascal
unit uDmDatabase;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL, FireDAC.Phys.MySQLDef,
  FireDAC.VCLUI.Wait, FireDAC.Comp.Client, Data.DB, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet;

type
  TdmDatabase = class(TDataModule)
    FDConnection1: TFDConnection;
    FDPhysMySQLDriverLink1: TFDPhysMySQLDriverLink;

    // Queries
    FDQueryClients: TFDQuery;
    FDQueryCommandes: TFDQuery;
    FDQueryProduits: TFDQuery;

    // DataSources
    DataSourceClients: TDataSource;
    DataSourceCommandes: TDataSource;
    DataSourceProduits: TDataSource;

    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Déclarations privées }
    procedure ConfigurerConnexion;
  public
    { Déclarations publiques }
    // Méthodes d'accès aux données
    procedure ConnecterBase;
    procedure DeconnecterBase;

    // Clients
    procedure ChargerClients;
    function AjouterClient(const Nom, Prenom, Email: string): Integer;
    procedure ModifierClient(ID: Integer; const Nom, Prenom, Email: string);
    procedure SupprimerClient(ID: Integer);

    // Commandes
    procedure ChargerCommandes(ClientID: Integer);
    function CreerCommande(ClientID: Integer; const Articles: TArray<Integer>): Integer;
  end;

var
  dmDatabase: TdmDatabase;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TdmDatabase.DataModuleCreate(Sender: TObject);
begin
  ConfigurerConnexion;
  ConnecterBase;
end;

procedure TdmDatabase.DataModuleDestroy(Sender: TObject);
begin
  DeconnecterBase;
end;

procedure TdmDatabase.ConfigurerConnexion;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MySQL');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Port=3306');
  FDConnection1.Params.Add('Database=ma_gestion');
  FDConnection1.Params.Add('User_Name=delphi_user');
  FDConnection1.Params.Add('Password=MonMotDePasse');
  FDConnection1.Params.Add('CharacterSet=utf8mb4');
  FDConnection1.LoginPrompt := False;
end;

procedure TdmDatabase.ConnecterBase;
begin
  if not FDConnection1.Connected then
    FDConnection1.Connected := True;
end;

procedure TdmDatabase.DeconnecterBase;
begin
  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;

// ─── CLIENTS ───

procedure TdmDatabase.ChargerClients;
begin
  FDQueryClients.Close;
  FDQueryClients.SQL.Text :=
    'SELECT id, nom, prenom, email, telephone, actif ' +
    'FROM clients ' +
    'WHERE actif = TRUE ' +
    'ORDER BY nom, prenom';
  FDQueryClients.Open;
end;

function TdmDatabase.AjouterClient(const Nom, Prenom, Email: string): Integer;
begin
  FDQueryClients.SQL.Text :=
    'INSERT INTO clients (nom, prenom, email, date_creation) ' +
    'VALUES (:nom, :prenom, :email, NOW())';

  FDQueryClients.ParamByName('nom').AsString := Nom;
  FDQueryClients.ParamByName('prenom').AsString := Prenom;
  FDQueryClients.ParamByName('email').AsString := Email;

  FDQueryClients.ExecSQL;

  // Retourner l'ID auto-généré
  Result := FDConnection1.GetLastAutoGenValue('clients');
end;

procedure TdmDatabase.ModifierClient(ID: Integer; const Nom, Prenom, Email: string);
begin
  FDQueryClients.SQL.Text :=
    'UPDATE clients SET ' +
    '  nom = :nom, ' +
    '  prenom = :prenom, ' +
    '  email = :email ' +
    'WHERE id = :id';

  FDQueryClients.ParamByName('nom').AsString := Nom;
  FDQueryClients.ParamByName('prenom').AsString := Prenom;
  FDQueryClients.ParamByName('email').AsString := Email;
  FDQueryClients.ParamByName('id').AsInteger := ID;

  FDQueryClients.ExecSQL;
end;

procedure TdmDatabase.SupprimerClient(ID: Integer);
begin
  // Suppression logique
  FDQueryClients.SQL.Text := 'UPDATE clients SET actif = FALSE WHERE id = :id';
  FDQueryClients.ParamByName('id').AsInteger := ID;
  FDQueryClients.ExecSQL;
end;

// ─── COMMANDES ───

procedure TdmDatabase.ChargerCommandes(ClientID: Integer);
begin
  FDQueryCommandes.Close;
  FDQueryCommandes.SQL.Text :=
    'SELECT id, date_commande, total, statut ' +
    'FROM commandes ' +
    'WHERE client_id = :client_id ' +
    'ORDER BY date_commande DESC';

  FDQueryCommandes.ParamByName('client_id').AsInteger := ClientID;
  FDQueryCommandes.Open;
end;

function TdmDatabase.CreerCommande(ClientID: Integer;
  const Articles: TArray<Integer>): Integer;
var
  i: Integer;
  CommandeID: Integer;
begin
  FDConnection1.StartTransaction;
  try
    // Créer la commande
    FDQueryCommandes.SQL.Text :=
      'INSERT INTO commandes (client_id, date_commande, statut) ' +
      'VALUES (:client_id, NOW(), ''En cours'')';
    FDQueryCommandes.ParamByName('client_id').AsInteger := ClientID;
    FDQueryCommandes.ExecSQL;

    CommandeID := FDConnection1.GetLastAutoGenValue('commandes');

    // Ajouter les articles (code simplifié)
    // ... code pour ajouter les articles ...

    FDConnection1.Commit;
    Result := CommandeID;
  except
    FDConnection1.Rollback;
    raise;
  end;
end;

end.
```

### Utiliser le DataModule dans un formulaire

```pascal
unit uFormClients;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms,
  Vcl.Controls, Vcl.StdCtrls, Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids,
  uDmDatabase;  // ← Ajouter la référence au DataModule

type
  TFormClients = class(TForm)
    DBGrid1: TDBGrid;
    btnNouveau: TButton;
    btnModifier: TButton;
    btnSupprimer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnNouveauClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormClients: TFormClients;

implementation

{$R *.dfm}

procedure TFormClients.FormCreate(Sender: TObject);
begin
  // Lier au DataModule
  DBGrid1.DataSource := dmDatabase.DataSourceClients;

  // Charger les données
  dmDatabase.ChargerClients;
end;

procedure TFormClients.btnNouveauClick(Sender: TObject);
var
  NouveauID: Integer;
begin
  // Appeler le DataModule pour ajouter
  NouveauID := dmDatabase.AjouterClient('Nouveau', 'Client', 'nouveau@email.fr');

  ShowMessage('Client créé avec l''ID : ' + IntToStr(NouveauID));

  // Recharger
  dmDatabase.ChargerClients;
end;

end.
```

**Important :** Le DataModule doit être créé **en premier** dans le projet.

### Configuration dans le projet

Dans le fichier projet (`.dpr`), assurez-vous que le DataModule est créé avant les formulaires :

```pascal
program GestionClients;

uses
  Vcl.Forms,
  uDmDatabase in 'uDmDatabase.pas' {dmDatabase: TDataModule},  // ← En premier !
  uFormClients in 'uFormClients.pas' {FormClients};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Créer le DataModule en premier
  Application.CreateForm(TdmDatabase, dmDatabase);

  // Puis les formulaires
  Application.CreateForm(TFormClients, FormClients);

  Application.Run;
end.
```

## Couche Métier : Classes de gestion

La couche métier contient la **logique business** et orchestre les opérations.

### Créer une classe Manager

```pascal
unit uClientManager;

interface

uses
  System.SysUtils, System.Classes, uDmDatabase;

type
  TClientManager = class
  private
    FDataModule: TdmDatabase;
  public
    constructor Create(ADataModule: TdmDatabase);

    // Méthodes métier
    function CreerNouveauClient(const Nom, Prenom, Email, Telephone: string): Integer;
    procedure ModifierClient(ID: Integer; const Nom, Prenom, Email, Telephone: string);
    function SupprimerClient(ID: Integer): Boolean;
    function RechercherClients(const Critere: string): Boolean;

    // Validation
    function ValiderEmail(const Email: string): Boolean;
    function ValiderTelephone(const Telephone: string): Boolean;

    // Règles métier
    function ClientPeutEtreSupprimer(ClientID: Integer): Boolean;
    function CalculerNombreCommandes(ClientID: Integer): Integer;
  end;

implementation

{ TClientManager }

constructor TClientManager.Create(ADataModule: TdmDatabase);
begin
  inherited Create;
  FDataModule := ADataModule;
end;

function TClientManager.CreerNouveauClient(const Nom, Prenom, Email,
  Telephone: string): Integer;
begin
  // Validation métier
  if Trim(Nom) = '' then
    raise Exception.Create('Le nom est obligatoire');

  if Trim(Prenom) = '' then
    raise Exception.Create('Le prénom est obligatoire');

  if not ValiderEmail(Email) then
    raise Exception.Create('Email invalide');

  if (Telephone <> '') and (not ValiderTelephone(Telephone)) then
    raise Exception.Create('Numéro de téléphone invalide');

  // Vérifier que l'email n'existe pas déjà
  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT COUNT(*) AS nb FROM clients WHERE email = :email';
  FDataModule.FDQueryClients.ParamByName('email').AsString := Email;
  FDataModule.FDQueryClients.Open;

  if FDataModule.FDQueryClients.FieldByName('nb').AsInteger > 0 then
    raise Exception.Create('Cet email est déjà utilisé');

  // Appeler la couche données
  Result := FDataModule.AjouterClient(Nom, Prenom, Email);

  // Log ou notification (optionnel)
  // LogActivity('Client créé : ' + Nom + ' ' + Prenom);
end;

procedure TClientManager.ModifierClient(ID: Integer; const Nom, Prenom,
  Email, Telephone: string);
begin
  // Validation
  if Trim(Nom) = '' then
    raise Exception.Create('Le nom est obligatoire');

  if not ValiderEmail(Email) then
    raise Exception.Create('Email invalide');

  // Vérifier que l'email n'est pas utilisé par un autre client
  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT COUNT(*) AS nb FROM clients WHERE email = :email AND id <> :id';
  FDataModule.FDQueryClients.ParamByName('email').AsString := Email;
  FDataModule.FDQueryClients.ParamByName('id').AsInteger := ID;
  FDataModule.FDQueryClients.Open;

  if FDataModule.FDQueryClients.FieldByName('nb').AsInteger > 0 then
    raise Exception.Create('Cet email est déjà utilisé');

  // Appeler la couche données
  FDataModule.ModifierClient(ID, Nom, Prenom, Email);
end;

function TClientManager.SupprimerClient(ID: Integer): Boolean;
begin
  Result := False;

  // Règle métier : on ne peut pas supprimer un client avec des commandes
  if not ClientPeutEtreSupprimer(ID) then
  begin
    raise Exception.Create(
      'Impossible de supprimer : ce client a des commandes en cours');
  end;

  // Appeler la couche données
  FDataModule.SupprimerClient(ID);
  Result := True;
end;

function TClientManager.RechercherClients(const Critere: string): Boolean;
begin
  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT * FROM clients ' +
    'WHERE actif = TRUE ' +
    '  AND (nom LIKE :critere OR prenom LIKE :critere OR email LIKE :critere) ' +
    'ORDER BY nom, prenom';

  FDataModule.FDQueryClients.ParamByName('critere').AsString := '%' + Critere + '%';
  FDataModule.FDQueryClients.Open;

  Result := not FDataModule.FDQueryClients.IsEmpty;
end;

function TClientManager.ValiderEmail(const Email: string): Boolean;
begin
  Result := (Trim(Email) <> '') and
            (Pos('@', Email) > 0) and
            (Pos('.', Email) > Pos('@', Email));
end;

function TClientManager.ValiderTelephone(const Telephone: string): Boolean;
var
  TelNettoyé: string;
  i: Integer;
begin
  // Enlever les espaces et caractères spéciaux
  TelNettoyé := '';
  for i := 1 to Length(Telephone) do
    if CharInSet(Telephone[i], ['0'..'9']) then
      TelNettoyé := TelNettoyé + Telephone[i];

  // Vérifier la longueur (10 chiffres pour la France)
  Result := Length(TelNettoyé) = 10;
end;

function TClientManager.ClientPeutEtreSupprimer(ClientID: Integer): Boolean;
var
  NbCommandes: Integer;
begin
  NbCommandes := CalculerNombreCommandes(ClientID);
  Result := NbCommandes = 0;
end;

function TClientManager.CalculerNombreCommandes(ClientID: Integer): Integer;
begin
  FDataModule.FDQueryCommandes.Close;
  FDataModule.FDQueryCommandes.SQL.Text :=
    'SELECT COUNT(*) AS nb FROM commandes WHERE client_id = :id';
  FDataModule.FDQueryCommandes.ParamByName('id').AsInteger := ClientID;
  FDataModule.FDQueryCommandes.Open;

  Result := FDataModule.FDQueryCommandes.FieldByName('nb').AsInteger;
end;

end.
```

### Utiliser le Manager dans le formulaire

```pascal
unit uFormClients;

interface

uses
  System.SysUtils, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls,
  uDmDatabase, uClientManager;  // ← Ajouter les références

type
  TFormClients = class(TForm)
    btnNouveau: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNouveauClick(Sender: TObject);
  private
    FClientManager: TClientManager;
  end;

implementation

{$R *.dfm}

procedure TFormClients.FormCreate(Sender: TObject);
begin
  // Créer le manager
  FClientManager := TClientManager.Create(dmDatabase);
end;

procedure TFormClients.FormDestroy(Sender: TObject);
begin
  // Libérer le manager
  FClientManager.Free;
end;

procedure TFormClients.btnNouveauClick(Sender: TObject);
var
  NouveauID: Integer;
begin
  try
    // Utiliser le manager (pas le DataModule directement !)
    NouveauID := FClientManager.CreerNouveauClient(
      'Nouveau',
      'Client',
      'nouveau@email.fr',
      '0601020304'
    );

    ShowMessage('Client créé avec succès (ID: ' + IntToStr(NouveauID) + ')');
    dmDatabase.ChargerClients;

  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;

end.
```

**Avantage :** Toute la validation et la logique métier est **centralisée** dans le Manager, pas dispersée dans les formulaires !

## Objets métier (Domain Objects)

Pour une architecture encore plus propre, créez des **classes métier** qui représentent vos entités.

### Définir une classe métier

```pascal
unit uClient;

interface

uses
  System.SysUtils;

type
  TClient = class
  private
    FID: Integer;
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FTelephone: string;
    FActif: Boolean;
    FDateCreation: TDateTime;
  public
    constructor Create; overload;
    constructor Create(AID: Integer; const ANom, APrenom, AEmail: string); overload;

    // Propriétés
    property ID: Integer read FID write FID;
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Email: string read FEmail write FEmail;
    property Telephone: string read FTelephone write FTelephone;
    property Actif: Boolean read FActif write FActif;
    property DateCreation: TDateTime read FDateCreation write FDateCreation;

    // Méthodes métier
    function NomComplet: string;
    function EstValide: Boolean;
    function ToString: string; override;
  end;

implementation

{ TClient }

constructor TClient.Create;
begin
  inherited Create;
  FActif := True;
  FDateCreation := Now;
end;

constructor TClient.Create(AID: Integer; const ANom, APrenom, AEmail: string);
begin
  Create;
  FID := AID;
  FNom := ANom;
  FPrenom := APrenom;
  FEmail := AEmail;
end;

function TClient.NomComplet: string;
begin
  Result := FPrenom + ' ' + FNom;
end;

function TClient.EstValide: Boolean;
begin
  Result := (Trim(FNom) <> '') and
            (Trim(FPrenom) <> '') and
            (Pos('@', FEmail) > 0);
end;

function TClient.ToString: string;
begin
  Result := Format('%s (ID: %d)', [NomComplet, FID]);
end;

end.
```

### DAO (Data Access Object)

Le **DAO** est responsable de la conversion entre les objets métier et la base de données.

```pascal
unit uClientDAO;

interface

uses
  System.SysUtils, System.Generics.Collections,
  FireDAC.Comp.Client, uClient, uDmDatabase;

type
  TClientDAO = class
  private
    FDataModule: TdmDatabase;
    function DataSetToClient(DataSet: TFDQuery): TClient;
  public
    constructor Create(ADataModule: TdmDatabase);

    // CRUD
    function Lire(ID: Integer): TClient;
    function LireTous: TObjectList<TClient>;
    function Creer(Client: TClient): Integer;
    procedure Modifier(Client: TClient);
    procedure Supprimer(ID: Integer);

    // Recherches
    function RechercherParEmail(const Email: string): TClient;
    function RechercherParNom(const Nom: string): TObjectList<TClient>;
  end;

implementation

{ TClientDAO }

constructor TClientDAO.Create(ADataModule: TdmDatabase);
begin
  inherited Create;
  FDataModule := ADataModule;
end;

function TClientDAO.DataSetToClient(DataSet: TFDQuery): TClient;
begin
  Result := TClient.Create;
  Result.ID := DataSet.FieldByName('id').AsInteger;
  Result.Nom := DataSet.FieldByName('nom').AsString;
  Result.Prenom := DataSet.FieldByName('prenom').AsString;
  Result.Email := DataSet.FieldByName('email').AsString;
  Result.Telephone := DataSet.FieldByName('telephone').AsString;
  Result.Actif := DataSet.FieldByName('actif').AsBoolean;
  Result.DateCreation := DataSet.FieldByName('date_creation').AsDateTime;
end;

function TClientDAO.Lire(ID: Integer): TClient;
begin
  Result := nil;

  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT * FROM clients WHERE id = :id';
  FDataModule.FDQueryClients.ParamByName('id').AsInteger := ID;
  FDataModule.FDQueryClients.Open;

  if not FDataModule.FDQueryClients.IsEmpty then
    Result := DataSetToClient(FDataModule.FDQueryClients);
end;

function TClientDAO.LireTous: TObjectList<TClient>;
var
  Client: TClient;
begin
  Result := TObjectList<TClient>.Create(True);  // True = possède les objets

  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT * FROM clients WHERE actif = TRUE ORDER BY nom, prenom';
  FDataModule.FDQueryClients.Open;

  FDataModule.FDQueryClients.First;
  while not FDataModule.FDQueryClients.Eof do
  begin
    Client := DataSetToClient(FDataModule.FDQueryClients);
    Result.Add(Client);
    FDataModule.FDQueryClients.Next;
  end;
end;

function TClientDAO.Creer(Client: TClient): Integer;
begin
  FDataModule.FDQueryClients.SQL.Text :=
    'INSERT INTO clients (nom, prenom, email, telephone, date_creation) ' +
    'VALUES (:nom, :prenom, :email, :tel, :date)';

  FDataModule.FDQueryClients.ParamByName('nom').AsString := Client.Nom;
  FDataModule.FDQueryClients.ParamByName('prenom').AsString := Client.Prenom;
  FDataModule.FDQueryClients.ParamByName('email').AsString := Client.Email;
  FDataModule.FDQueryClients.ParamByName('tel').AsString := Client.Telephone;
  FDataModule.FDQueryClients.ParamByName('date').AsDateTime := Client.DateCreation;

  FDataModule.FDQueryClients.ExecSQL;

  Result := FDataModule.FDConnection1.GetLastAutoGenValue('clients');
  Client.ID := Result;
end;

procedure TClientDAO.Modifier(Client: TClient);
begin
  FDataModule.FDQueryClients.SQL.Text :=
    'UPDATE clients SET ' +
    '  nom = :nom, ' +
    '  prenom = :prenom, ' +
    '  email = :email, ' +
    '  telephone = :tel ' +
    'WHERE id = :id';

  FDataModule.FDQueryClients.ParamByName('nom').AsString := Client.Nom;
  FDataModule.FDQueryClients.ParamByName('prenom').AsString := Client.Prenom;
  FDataModule.FDQueryClients.ParamByName('email').AsString := Client.Email;
  FDataModule.FDQueryClients.ParamByName('tel').AsString := Client.Telephone;
  FDataModule.FDQueryClients.ParamByName('id').AsInteger := Client.ID;

  FDataModule.FDQueryClients.ExecSQL;
end;

procedure TClientDAO.Supprimer(ID: Integer);
begin
  FDataModule.FDQueryClients.SQL.Text :=
    'UPDATE clients SET actif = FALSE WHERE id = :id';
  FDataModule.FDQueryClients.ParamByName('id').AsInteger := ID;
  FDataModule.FDQueryClients.ExecSQL;
end;

function TClientDAO.RechercherParEmail(const Email: string): TClient;
begin
  Result := nil;

  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT * FROM clients WHERE email = :email';
  FDataModule.FDQueryClients.ParamByName('email').AsString := Email;
  FDataModule.FDQueryClients.Open;

  if not FDataModule.FDQueryClients.IsEmpty then
    Result := DataSetToClient(FDataModule.FDQueryClients);
end;

function TClientDAO.RechercherParNom(const Nom: string): TObjectList<TClient>;
var
  Client: TClient;
begin
  Result := TObjectList<TClient>.Create(True);

  FDataModule.FDQueryClients.Close;
  FDataModule.FDQueryClients.SQL.Text :=
    'SELECT * FROM clients WHERE nom LIKE :nom ORDER BY nom';
  FDataModule.FDQueryClients.ParamByName('nom').AsString := '%' + Nom + '%';
  FDataModule.FDQueryClients.Open;

  FDataModule.FDQueryClients.First;
  while not FDataModule.FDQueryClients.Eof do
  begin
    Client := DataSetToClient(FDataModule.FDQueryClients);
    Result.Add(Client);
    FDataModule.FDQueryClients.Next;
  end;
end;

end.
```

### Utiliser les objets métier

```pascal
procedure TFormClients.AfficherClients;
var
  DAO: TClientDAO;
  Clients: TObjectList<TClient>;
  Client: TClient;
begin
  DAO := TClientDAO.Create(dmDatabase);
  try
    Clients := DAO.LireTous;
    try
      Memo1.Clear;
      for Client in Clients do
      begin
        Memo1.Lines.Add(Client.ToString);
      end;
    finally
      Clients.Free;  // Libère automatiquement tous les clients
    end;
  finally
    DAO.Free;
  end;
end;

procedure TFormClients.CreerClient;
var
  DAO: TClientDAO;
  Client: TClient;
  NouveauID: Integer;
begin
  Client := TClient.Create;
  try
    // Remplir l'objet
    Client.Nom := 'Dupont';
    Client.Prenom := 'Jean';
    Client.Email := 'jean.dupont@email.fr';
    Client.Telephone := '0601020304';

    // Valider
    if not Client.EstValide then
      raise Exception.Create('Client invalide');

    // Sauvegarder
    DAO := TClientDAO.Create(dmDatabase);
    try
      NouveauID := DAO.Creer(Client);
      ShowMessage('Client créé avec l''ID : ' + IntToStr(NouveauID));
    finally
      DAO.Free;
    end;
  finally
    Client.Free;
  end;
end;
```

## Organisation des fichiers

### Structure recommandée

```
MonProjet/
│
├── Projet.dpr                  (Fichier projet)
│
├── Forms/                      (Couche Présentation)
│   ├── uFormMain.pas
│   ├── uFormClients.pas
│   ├── uFormCommandes.pas
│   └── uFormRapports.pas
│
├── DataModules/                (Couche Données)
│   ├── uDmDatabase.pas
│   └── uDmConfiguration.pas
│
├── Business/                   (Couche Métier)
│   ├── Managers/
│   │   ├── uClientManager.pas
│   │   ├── uCommandeManager.pas
│   │   └── uProduitManager.pas
│   │
│   └── Services/
│       ├── uEmailService.pas
│       └── uReportService.pas
│
├── DataAccess/                 (Accès données)
│   ├── uClientDAO.pas
│   ├── uCommandeDAO.pas
│   └── uProduitDAO.pas
│
├── Model/                      (Objets métier)
│   ├── uClient.pas
│   ├── uCommande.pas
│   └── uProduit.pas
│
└── Common/                     (Utilitaires communs)
    ├── uConstants.pas
    ├── uTypes.pas
    └── uUtils.pas
```

## Injection de dépendances (niveau intermédiaire)

Pour une architecture encore plus flexible, utilisez l'**injection de dépendances**.

### Principe

Au lieu de créer directement les dépendances, on les **injecte** via le constructeur ou des propriétés.

```pascal
// ❌ Sans injection : dépendance forte
type
  TClientManager = class
  private
    FDataModule: TdmDatabase;
  public
    constructor Create;  // Crée TdmDatabase à l'intérieur
  end;

// ✅ Avec injection : dépendance faible
type
  TClientManager = class
  private
    FDataModule: TdmDatabase;
  public
    constructor Create(ADataModule: TdmDatabase);  // Reçoit de l'extérieur
  end;
```

**Avantages :**
- Plus facile à **tester** (on peut injecter un mock)
- Plus **flexible** (on peut changer l'implémentation)
- Respect du principe de **responsabilité unique**

### Exemple avec interface

```pascal
// Interface pour l'accès données
type
  IClientRepository = interface
    ['{GUID-HERE}']
    function Lire(ID: Integer): TClient;
    function LireTous: TObjectList<TClient>;
    function Creer(Client: TClient): Integer;
    procedure Modifier(Client: TClient);
    procedure Supprimer(ID: Integer);
  end;

// Implémentation avec FireDAC
type
  TClientRepositoryFireDAC = class(TInterfacedObject, IClientRepository)
  private
    FConnection: TFDConnection;
  public
    constructor Create(AConnection: TFDConnection);
    // Implémenter les méthodes de l'interface
  end;

// Manager qui utilise l'interface
type
  TClientManager = class
  private
    FRepository: IClientRepository;
  public
    constructor Create(ARepository: IClientRepository);
    // Le manager ne connaît pas FireDAC, seulement l'interface !
  end;
```

**Résultat :** On peut facilement remplacer FireDAC par une autre technologie sans toucher au Manager !

## Tests unitaires

L'architecture en couches facilite énormément les **tests unitaires**.

### Tester la couche métier

```pascal
unit uClientManagerTests;

interface

uses
  DUnitX.TestFramework, uClientManager, uDmDatabase;

type
  [TestFixture]
  TClientManagerTests = class
  private
    FClientManager: TClientManager;
  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestCreerClient_Valide;

    [Test]
    procedure TestCreerClient_EmailInvalide;

    [Test]
    procedure TestValiderEmail;
  end;

implementation

procedure TClientManagerTests.Setup;
begin
  // Créer le manager avec une base de test
  FClientManager := TClientManager.Create(dmDatabase);
end;

procedure TClientManagerTests.TearDown;
begin
  FClientManager.Free;
end;

procedure TClientManagerTests.TestCreerClient_Valide;
var
  ID: Integer;
begin
  // Arrange
  // Act
  ID := FClientManager.CreerNouveauClient('Test', 'Client', 'test@email.fr', '');

  // Assert
  Assert.IsTrue(ID > 0, 'L''ID doit être supérieur à 0');
end;

procedure TClientManagerTests.TestCreerClient_EmailInvalide;
begin
  // Assert.WillRaise : on attend une exception
  Assert.WillRaise(
    procedure
    begin
      FClientManager.CreerNouveauClient('Test', 'Client', 'emailinvalide', '');
    end,
    Exception,
    'Devrait lever une exception pour email invalide'
  );
end;

procedure TClientManagerTests.TestValiderEmail;
begin
  Assert.IsTrue(FClientManager.ValiderEmail('test@example.com'));
  Assert.IsFalse(FClientManager.ValiderEmail('invalide'));
  Assert.IsFalse(FClientManager.ValiderEmail(''));
end;

end.
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Un DataModule par base de données**
   ```pascal
   dmPrincipale  // Base principale
   dmStatistiques  // Base de statistiques
   ```

2. **Séparer les responsabilités**
   ```
   Formulaires → Affichage uniquement
   Managers → Logique métier
   DAO → Accès données
   ```

3. **Valider dans la couche métier**
   ```pascal
   // ✅ Dans le Manager
   if not ValiderEmail(Email) then
     raise Exception.Create('Email invalide');
   ```

4. **Utiliser des interfaces**
   ```pascal
   IClientRepository, ICommandeRepository
   ```

5. **Créer des objets métier**
   ```pascal
   TClient, TCommande, TProduit
   ```

6. **Logger les opérations importantes**
   ```pascal
   Logger.Info('Client créé : ' + Client.NomComplet);
   ```

### ❌ À ÉVITER

1. **Mélanger les couches**
   ```pascal
   // ❌ SQL dans le formulaire
   FDQuery1.SQL.Text := 'SELECT...';

   // ✅ Appeler le DataModule ou Manager
   dmDatabase.ChargerClients;
   ```

2. **Dépendances circulaires**
   ```
   ❌ FormClients uses uClientManager
      uClientManager uses FormClients
   ```

3. **Logique dans les getters/setters**
   ```pascal
   // ❌ Logique métier dans la propriété
   procedure TClient.SetEmail(const Value: string);
   begin
     if Pos('@', Value) = 0 then
       raise Exception.Create('Email invalide');
     FEmail := Value;
   end;

   // ✅ Méthode de validation séparée
   function TClient.ValiderEmail: Boolean;
   ```

4. **Trop de paramètres**
   ```pascal
   // ❌ Trop de paramètres
   function Creer(N, P, E, T, A, V, C, D: string): Integer;

   // ✅ Utiliser un objet
   function Creer(Client: TClient): Integer;
   ```

## Architecture complète récapitulative

```
┌─────────────────────────────────────────────────────┐
│              COUCHE PRÉSENTATION                    │
│  FormMain, FormClients, FormCommandes               │
│  • Affichage                                        │
│  • Interaction utilisateur                          │
│  • Appelle → Managers                               │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│              COUCHE MÉTIER                          │
│  ClientManager, CommandeManager                     │
│  EmailService, ReportService                        │
│  • Règles métier                                    │
│  • Validation                                       │
│  • Orchestration                                    │
│  • Appelle → DAO                                    │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│         COUCHE ACCÈS DONNÉES (DAO)                  │
│  ClientDAO, CommandeDAO                             │
│  • Conversion objets ↔ base                         │
│  • Requêtes SQL                                     │
│  • Appelle → DataModule                             │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│              DATA MODULE                            │
│  TFDConnection, TFDQuery                            │
│  • Connexion physique                               │
│  • Gestion des composants FireDAC                   │
│  • Appelle → Base de données                        │
└──────────────────┬──────────────────────────────────┘
                   │
┌──────────────────▼──────────────────────────────────┐
│           BASE DE DONNÉES                           │
│  MySQL/MariaDB                                      │
└─────────────────────────────────────────────────────┘

     OBJETS MÉTIER (utilisés dans toutes les couches)
┌─────────────────────────────────────────────────────┐
│  TClient, TCommande, TProduit                       │
│  • Propriétés                                       │
│  • Méthodes métier simples                          │
└─────────────────────────────────────────────────────┘
```

## Résumé

### Points clés

✅ **Séparer en 3 couches** : Présentation, Métier, Données
✅ **DataModule** pour centraliser l'accès aux données
✅ **Managers** pour la logique métier
✅ **DAO** pour convertir objets ↔ base
✅ **Objets métier** pour représenter les entités
✅ **Injection de dépendances** pour la flexibilité

### Avantages de l'architecture en couches

| Avantage | Description |
|----------|-------------|
| **Maintenabilité** | Code organisé, facile à comprendre |
| **Réutilisabilité** | Même logique dans plusieurs formulaires |
| **Testabilité** | Chaque couche testable séparément |
| **Évolutivité** | Facile d'ajouter des fonctionnalités |
| **Travail d'équipe** | Chacun peut travailler sur sa couche |
| **Flexibilité** | Changer une couche sans toucher les autres |

### Progression recommandée

**Niveau débutant :**
1. Tout dans le formulaire (pour apprendre)
2. Passer à un DataModule simple

**Niveau intermédiaire :**
3. Ajouter des classes Manager
4. Créer des objets métier

**Niveau avancé :**
5. Implémenter des DAO
6. Utiliser des interfaces
7. Injection de dépendances

## Prochaines étapes

Vous maîtrisez maintenant l'architecture en couches ! Dans les sections suivantes, nous verrons :

1. **Migration de bases** de données
2. **Sécurisation** avancée
3. **Optimisation** des performances
4. **Patterns avancés** (Repository, Unit of Work)

Avec une architecture en couches bien conçue, votre application sera professionnelle, maintenable et évolutive !

⏭️ [Migration et synchronisation de bases de données](/08-acces-aux-bases-de-donnees-mysql-mariadb/10-migration-et-synchronisation-de-bases-de-donnees.md)
