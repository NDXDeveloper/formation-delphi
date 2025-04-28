# 8.12 Autres moteurs de bases de données (SQLite, PostgreSQL, SQL Server)

Jusqu'à présent, nous avons concentré notre attention sur MySQL/MariaDB, mais l'un des grands avantages de FireDAC est sa capacité à se connecter à de nombreux moteurs de bases de données différents. Dans cette section, nous explorerons trois alternatives populaires : SQLite, PostgreSQL et SQL Server, et verrons comment adapter votre code Delphi pour travailler avec ces systèmes.

## Pourquoi utiliser d'autres moteurs de bases de données ?

Chaque système de gestion de base de données (SGBD) a ses propres forces et faiblesses. Le choix dépend souvent de vos besoins spécifiques :

- **SQLite** : Léger, sans serveur, idéal pour les applications embarquées ou mobiles
- **PostgreSQL** : Robuste, open-source, excellentes performances pour les applications complexes
- **SQL Server** : Solution d'entreprise de Microsoft, bien intégrée avec les environnements Windows

## SQLite : Base de données légère et embarquée

### Qu'est-ce que SQLite ?

SQLite est une bibliothèque compacte qui implémente un moteur de base de données SQL auto-contenu, sans serveur et ne nécessitant aucune configuration. Contrairement à MySQL ou PostgreSQL, SQLite ne fonctionne pas selon un modèle client-serveur - la base de données entière est stockée dans un seul fichier.

### Avantages de SQLite

- **Aucune installation de serveur nécessaire**
- **Base de données contenue dans un seul fichier**
- **Très léger (~ 500 Ko)**
- **Idéal pour les applications mobiles, embarquées ou de bureau**
- **Zéro configuration**
- **Portable entre différentes plateformes**

### Limitations de SQLite

- **Moins adapté aux applications multi-utilisateurs**
- **Pas de gestion fine des droits utilisateurs**
- **Fonctionnalités SQL limitées par rapport aux SGBD complets**
- **Performances réduites pour les très grandes bases de données**

### Configuration de SQLite dans Delphi

Une des beautés de SQLite est sa simplicité de configuration. Vous n'avez pas besoin d'installer un serveur séparé - FireDAC inclut tout ce dont vous avez besoin.

#### 1. Création d'une connexion SQLite

```delphi
procedure TForm1.ConfigurerConnexionSQLite;
begin
  FDConnection1.Params.Clear;
  FDConnection1.DriverName := 'SQLite';

  // Chemin vers le fichier de base de données
  FDConnection1.Params.Database := ExtractFilePath(Application.ExeName) + 'mabase.db';

  // Options supplémentaires
  FDConnection1.Params.Add('DateTimeFormat=String');  // Format de date/heure
  FDConnection1.Params.Add('Encrypt=aes-256');        // Chiffrement (optionnel)
  FDConnection1.Params.Add('Password=mon_mot_de_passe');  // Mot de passe de chiffrement

  try
    FDConnection1.Connected := True;
    ShowMessage('Connexion à SQLite réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

#### 2. Création automatique de la base si elle n'existe pas

Un avantage de SQLite est que la base de données est créée automatiquement si elle n'existe pas déjà. Voici comment initialiser une nouvelle base de données :

```delphi
procedure TForm1.InitialiserBaseSQLite;
begin
  // Vérifier si le fichier existe déjà
  if not FileExists(FDConnection1.Params.Database) then
  begin
    // Le fichier sera créé automatiquement lors de la connexion
    FDConnection1.Connected := True;

    // Créer les tables
    FDConnection1.ExecSQL(
      'CREATE TABLE clients (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nom TEXT NOT NULL,' +
      '  prenom TEXT,' +
      '  email TEXT,' +
      '  telephone TEXT,' +
      '  date_creation TEXT DEFAULT CURRENT_TIMESTAMP' +
      ')'
    );

    FDConnection1.ExecSQL(
      'CREATE TABLE produits (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  nom TEXT NOT NULL,' +
      '  description TEXT,' +
      '  prix REAL NOT NULL,' +
      '  stock INTEGER DEFAULT 0' +
      ')'
    );

    ShowMessage('Base de données initialisée avec succès !');
  end
  else
  begin
    // Base de données existante, simplement se connecter
    FDConnection1.Connected := True;
  end;
end;
```

### Spécificités de SQLite à connaître

SQLite diffère des autres SGBD sur plusieurs points importants :

#### Types de données

SQLite utilise un typage dynamique, ce qui signifie qu'une colonne peut contenir différents types de données. Il n'y a que 5 classes de stockage :
- `INTEGER` : Nombres entiers
- `REAL` : Nombres à virgule flottante
- `TEXT` : Chaînes de caractères
- `BLOB` : Données binaires
- `NULL` : Absence de valeur

#### Transactions

SQLite est ACID (Atomicité, Cohérence, Isolation, Durabilité) et supporte les transactions :

```delphi
procedure TForm1.AjouterClientAvecTransaction;
begin
  FDConnection1.StartTransaction;
  try
    // Ajouter un client
    FDQuery1.SQL.Text := 'INSERT INTO clients (nom, prenom, email) VALUES (:nom, :prenom, :email)';
    FDQuery1.ParamByName('nom').AsString := 'Dupont';
    FDQuery1.ParamByName('prenom').AsString := 'Jean';
    FDQuery1.ParamByName('email').AsString := 'jean.dupont@exemple.com';
    FDQuery1.ExecSQL;

    // Si tout s'est bien passé, valider la transaction
    FDConnection1.Commit;
    ShowMessage('Client ajouté avec succès !');
  except
    on E: Exception do
    begin
      // En cas d'erreur, annuler la transaction
      FDConnection1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
    end;
  end;
end;
```

#### Dates et heures

SQLite n'a pas de type de données dédié aux dates et heures. Vous pouvez les stocker sous forme de :
- Texte (ISO8601 : 'YYYY-MM-DD HH:MM:SS.SSS')
- Timestamp Unix (INTEGER)
- Numérique Julian (REAL)

Pour une manipulation plus facile, utilisez le paramètre `DateTimeFormat` :

```delphi
// Configuration pour que SQLite gère les dates comme du texte ISO8601
FDConnection1.Params.Add('DateTimeFormat=String');

// Insertion d'une date
FDQuery1.SQL.Text := 'INSERT INTO evenements (titre, date_evt) VALUES (:titre, :date)';
FDQuery1.ParamByName('titre').AsString := 'Réunion';
FDQuery1.ParamByName('date').AsDateTime := Now;  // FireDAC convertira automatiquement
FDQuery1.ExecSQL;
```

### Exemple complet d'application SQLite

Voici un exemple simple d'application de gestion de contacts avec SQLite :

```delphi
unit UFormContacts;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Grids, Vcl.DBGrids, Data.DB, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet;

type
  TFormContacts = class(TForm)
    PanelTop: TPanel;
    LabelTitre: TLabel;
    PanelGauche: TPanel;
    PanelDroite: TPanel;
    DBGrid1: TDBGrid;
    EditNom: TEdit;
    EditPrenom: TEdit;
    EditEmail: TEdit;
    EditTelephone: TEdit;
    LabelNom: TLabel;
    LabelPrenom: TLabel;
    LabelEmail: TLabel;
    LabelTelephone: TLabel;
    ButtonAjouter: TButton;
    ButtonModifier: TButton;
    ButtonSupprimer: TButton;
    DataSource1: TDataSource;
    FDConnection1: TFDConnection;
    FDQueryContacts: TFDQuery;
    ButtonRechercher: TButton;
    EditRecherche: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure ButtonAjouterClick(Sender: TObject);
    procedure ButtonModifierClick(Sender: TObject);
    procedure ButtonSupprimerClick(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
  private
    procedure InitialiserBaseDeDonnees;
    procedure ChargerContacts;
    procedure EffacerChamps;
  public
    { Public declarations }
  end;

var
  FormContacts: TFormContacts;

implementation

{$R *.dfm}

procedure TFormContacts.FormCreate(Sender: TObject);
begin
  // Configurer la connexion SQLite
  FDConnection1.Params.Clear;
  FDConnection1.DriverName := 'SQLite';
  FDConnection1.Params.Database := ExtractFilePath(Application.ExeName) + 'contacts.db';
  FDConnection1.Params.Add('DateTimeFormat=String');

  // Initialiser la base de données si nécessaire
  InitialiserBaseDeDonnees;

  // Charger les contacts
  ChargerContacts;
end;

procedure TFormContacts.FormDestroy(Sender: TObject);
begin
  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;

procedure TFormContacts.InitialiserBaseDeDonnees;
begin
  try
    // Se connecter (crée le fichier s'il n'existe pas)
    FDConnection1.Connected := True;

    // Vérifier si la table contacts existe déjà
    FDQueryContacts.SQL.Text :=
      'SELECT name FROM sqlite_master WHERE type="table" AND name="contacts"';
    FDQueryContacts.Open;

    if FDQueryContacts.IsEmpty then
    begin
      // Créer la table contacts
      FDConnection1.ExecSQL(
        'CREATE TABLE contacts (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  nom TEXT NOT NULL,' +
        '  prenom TEXT,' +
        '  email TEXT,' +
        '  telephone TEXT,' +
        '  date_creation TEXT DEFAULT CURRENT_TIMESTAMP' +
        ')'
      );

      // Ajouter quelques exemples
      FDConnection1.ExecSQL(
        'INSERT INTO contacts (nom, prenom, email, telephone) VALUES ' +
        '("Dupont", "Jean", "jean.dupont@exemple.com", "01 23 45 67 89"),' +
        '("Martin", "Sophie", "sophie.martin@exemple.com", "01 98 76 54 32"),' +
        '("Durand", "Pierre", "pierre.durand@exemple.com", "07 12 34 56 78")'
      );
    end;
  except
    on E: Exception do
      ShowMessage('Erreur d''initialisation de la base de données : ' + E.Message);
  end;
end;

procedure TFormContacts.ChargerContacts;
begin
  // Charger tous les contacts
  FDQueryContacts.SQL.Text := 'SELECT * FROM contacts ORDER BY nom, prenom';
  FDQueryContacts.Open;

  // Configurer la source de données
  DataSource1.DataSet := FDQueryContacts;
end;

procedure TFormContacts.ButtonRechercherClick(Sender: TObject);
var
  Terme: string;
begin
  Terme := Trim(EditRecherche.Text);

  if Terme = '' then
    // Aucun terme de recherche, afficher tous les contacts
    FDQueryContacts.SQL.Text := 'SELECT * FROM contacts ORDER BY nom, prenom'
  else
    // Rechercher selon le terme
    FDQueryContacts.SQL.Text :=
      'SELECT * FROM contacts WHERE ' +
      'nom LIKE :terme OR prenom LIKE :terme OR email LIKE :terme OR telephone LIKE :terme ' +
      'ORDER BY nom, prenom';

  if Terme <> '' then
    FDQueryContacts.ParamByName('terme').AsString := '%' + Terme + '%';

  FDQueryContacts.Open;
end;

procedure TFormContacts.ButtonAjouterClick(Sender: TObject);
begin
  // Valider les champs
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    Exit;
  end;

  try
    // Ajouter le contact
    FDConnection1.StartTransaction;

    FDQueryContacts.SQL.Text :=
      'INSERT INTO contacts (nom, prenom, email, telephone) ' +
      'VALUES (:nom, :prenom, :email, :telephone)';
    FDQueryContacts.ParamByName('nom').AsString := Trim(EditNom.Text);
    FDQueryContacts.ParamByName('prenom').AsString := Trim(EditPrenom.Text);
    FDQueryContacts.ParamByName('email').AsString := Trim(EditEmail.Text);
    FDQueryContacts.ParamByName('telephone').AsString := Trim(EditTelephone.Text);
    FDQueryContacts.ExecSQL;

    FDConnection1.Commit;

    // Recharger les contacts
    ChargerContacts;

    // Effacer les champs
    EffacerChamps;

    ShowMessage('Contact ajouté avec succès !');
  except
    on E: Exception do
    begin
      FDConnection1.Rollback;
      ShowMessage('Erreur lors de l''ajout du contact : ' + E.Message);
    end;
  end;
end;

procedure TFormContacts.ButtonModifierClick(Sender: TObject);
var
  ID: Integer;
begin
  // Vérifier qu'un contact est sélectionné
  if FDQueryContacts.IsEmpty then
  begin
    ShowMessage('Aucun contact sélectionné');
    Exit;
  end;

  // Valider les champs
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    Exit;
  end;

  // Récupérer l'ID du contact sélectionné
  ID := FDQueryContacts.FieldByName('id').AsInteger;

  try
    // Modifier le contact
    FDConnection1.StartTransaction;

    FDQueryContacts.SQL.Text :=
      'UPDATE contacts SET ' +
      'nom = :nom, prenom = :prenom, email = :email, telephone = :telephone ' +
      'WHERE id = :id';
    FDQueryContacts.ParamByName('nom').AsString := Trim(EditNom.Text);
    FDQueryContacts.ParamByName('prenom').AsString := Trim(EditPrenom.Text);
    FDQueryContacts.ParamByName('email').AsString := Trim(EditEmail.Text);
    FDQueryContacts.ParamByName('telephone').AsString := Trim(EditTelephone.Text);
    FDQueryContacts.ParamByName('id').AsInteger := ID;
    FDQueryContacts.ExecSQL;

    FDConnection1.Commit;

    // Recharger les contacts
    ChargerContacts;

    ShowMessage('Contact modifié avec succès !');
  except
    on E: Exception do
    begin
      FDConnection1.Rollback;
      ShowMessage('Erreur lors de la modification du contact : ' + E.Message);
    end;
  end;
end;

procedure TFormContacts.ButtonSupprimerClick(Sender: TObject);
var
  ID: Integer;
begin
  // Vérifier qu'un contact est sélectionné
  if FDQueryContacts.IsEmpty then
  begin
    ShowMessage('Aucun contact sélectionné');
    Exit;
  end;

  // Demander confirmation
  if MessageDlg('Êtes-vous sûr de vouloir supprimer ce contact ?',
                mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Récupérer l'ID du contact sélectionné
  ID := FDQueryContacts.FieldByName('id').AsInteger;

  try
    // Supprimer le contact
    FDConnection1.StartTransaction;

    FDQueryContacts.SQL.Text := 'DELETE FROM contacts WHERE id = :id';
    FDQueryContacts.ParamByName('id').AsInteger := ID;
    FDQueryContacts.ExecSQL;

    FDConnection1.Commit;

    // Recharger les contacts
    ChargerContacts;

    // Effacer les champs
    EffacerChamps;

    ShowMessage('Contact supprimé avec succès !');
  except
    on E: Exception do
    begin
      FDConnection1.Rollback;
      ShowMessage('Erreur lors de la suppression du contact : ' + E.Message);
    end;
  end;
end;

procedure TFormContacts.DBGrid1CellClick(Column: TColumn);
begin
  // Afficher les détails du contact sélectionné
  if not FDQueryContacts.IsEmpty then
  begin
    EditNom.Text := FDQueryContacts.FieldByName('nom').AsString;
    EditPrenom.Text := FDQueryContacts.FieldByName('prenom').AsString;
    EditEmail.Text := FDQueryContacts.FieldByName('email').AsString;
    EditTelephone.Text := FDQueryContacts.FieldByName('telephone').AsString;
  end;
end;

procedure TFormContacts.EffacerChamps;
begin
  EditNom.Clear;
  EditPrenom.Clear;
  EditEmail.Clear;
  EditTelephone.Clear;
  EditNom.SetFocus;
end;

end.
```

## PostgreSQL : La base de données relationnelle avancée open-source

### Qu'est-ce que PostgreSQL ?

PostgreSQL (souvent appelé "Postgres") est un système de gestion de base de données relationnelle avancé, open-source et gratuit. Il est reconnu pour sa robustesse, sa conformité aux standards SQL et ses fonctionnalités avancées.

### Avantages de PostgreSQL

- **Haute conformité aux standards SQL**
- **Support avancé des transactions**
- **Extensibilité via des types de données personnalisés et des procédures stockées**
- **Excellent pour les grandes bases de données**
- **Support natif des données géospatiales**
- **Open-source et gratuité**

### Limitations de PostgreSQL

- **Configuration initiale plus complexe que SQLite**
- **Consommation de ressources plus importante**
- **Moins répandu dans les entreprises que SQL Server ou Oracle**

### Installation de PostgreSQL

Contrairement à SQLite, PostgreSQL nécessite l'installation d'un serveur :

1. Téléchargez PostgreSQL sur le [site officiel](https://www.postgresql.org/download/)
2. Suivez les instructions d'installation pour votre système d'exploitation
3. Lors de l'installation, notez le mot de passe de l'utilisateur `postgres` (administrateur)
4. Installez également pgAdmin, l'outil d'administration graphique

### Configuration de PostgreSQL dans Delphi

#### 1. Connexion à PostgreSQL

```delphi
procedure TForm1.ConfigurerConnexionPostgreSQL;
begin
  FDConnection1.Params.Clear;
  FDConnection1.DriverName := 'PG'; // PG est l'abréviation pour PostgreSQL dans FireDAC

  // Paramètres de connexion
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Port=5432');          // Port par défaut
  FDConnection1.Params.Add('Database=ma_base');   // Nom de la base
  FDConnection1.Params.Add('User_Name=postgres'); // Nom d'utilisateur
  FDConnection1.Params.Add('Password=mon_mot_de_passe'); // Mot de passe

  // Options supplémentaires
  FDConnection1.Params.Add('ApplicationName=Mon Application Delphi'); // Facilite l'identification dans les logs

  try
    FDConnection1.Connected := True;
    ShowMessage('Connexion à PostgreSQL réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

#### 2. Création de tables dans PostgreSQL

PostgreSQL offre des types de données plus riches que SQLite. Voici un exemple pour créer des tables :

```delphi
procedure TForm1.CreerTablesPostgreSQL;
begin
  // Créer une table clients avec des types PostgreSQL
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS clients (' +
    '  id SERIAL PRIMARY KEY,' +        // SERIAL = auto-incrémentation
    '  nom VARCHAR(100) NOT NULL,' +
    '  prenom VARCHAR(100),' +
    '  email VARCHAR(100) UNIQUE,' +    // Contrainte d'unicité
    '  telephone VARCHAR(20),' +
    '  date_naissance DATE,' +          // Type DATE natif
    '  solde DECIMAL(10,2) DEFAULT 0,' +// Précision décimale
    '  actif BOOLEAN DEFAULT TRUE,' +   // Type booléen natif
    '  date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
    ')'
  );

  // Créer une table avec une clé étrangère
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS commandes (' +
    '  id SERIAL PRIMARY KEY,' +
    '  client_id INTEGER NOT NULL,' +
    '  date_commande TIMESTAMP DEFAULT CURRENT_TIMESTAMP,' +
    '  montant DECIMAL(10,2) NOT NULL,' +
    '  statut VARCHAR(20) DEFAULT ''En attente'',' +
    '  CONSTRAINT fk_client FOREIGN KEY (client_id) REFERENCES clients(id)' +
    '    ON DELETE RESTRICT' +  // Empêche la suppression d'un client qui a des commandes
    ')'
  );
end;
```

### Spécificités de PostgreSQL à connaître

#### Types de données avancés

PostgreSQL offre des types de données très riches :

- `JSONB` : pour stocker des données JSON avec indexation
- `ARRAY` : pour stocker des tableaux
- `UUID` : pour les identifiants universels uniques
- `HSTORE` : pour stocker des paires clé-valeur
- Types géospatiaux : pour les coordonnées et formes géométriques

Exemple d'utilisation du type JSONB :

```delphi
// Insérer des données JSON
FDQuery1.SQL.Text :=
  'INSERT INTO produits (nom, prix, proprietes) VALUES (:nom, :prix, :props)';
FDQuery1.ParamByName('nom').AsString := 'Smartphone';
FDQuery1.ParamByName('prix').AsFloat := 599.99;
FDQuery1.ParamByName('props').AsWideString :=
  '{"couleur": "noir", "memoire": 128, "dimensions": {"hauteur": 15, "largeur": 7.5}}';
FDQuery1.ExecSQL;

// Rechercher dans les données JSON
FDQuery1.SQL.Text :=
  'SELECT * FROM produits WHERE proprietes->''couleur'' = ''noir'' AND ' +
  'CAST(proprietes->''memoire'' AS INTEGER) > 64';
FDQuery1.Open;
```

#### Schémas

PostgreSQL utilise un concept de "schéma" pour organiser les tables et autres objets. Par défaut, les objets sont créés dans le schéma "public" :

```delphi
// Créer un nouveau schéma
FDConnection1.ExecSQL('CREATE SCHEMA IF NOT EXISTS ventes');

// Créer une table dans un schéma spécifique
FDConnection1.ExecSQL(
  'CREATE TABLE ventes.clients (' +
  '  id SERIAL PRIMARY KEY,' +
  '  nom VARCHAR(100) NOT NULL' +
  ')'
);

// Interroger une table dans un schéma spécifique
FDQuery1.SQL.Text := 'SELECT * FROM ventes.clients';
FDQuery1.Open;
```

#### Fonctions et procédures stockées

PostgreSQL permet de créer des fonctions en plusieurs langages (SQL, PL/pgSQL, PL/Python, etc.) :

```delphi
// Créer une fonction simple
FDConnection1.ExecSQL(
  'CREATE OR REPLACE FUNCTION calculer_prix_total(quantite INTEGER, prix_unitaire DECIMAL) ' +
  'RETURNS DECIMAL AS $$ ' +
  'BEGIN ' +
  '  RETURN quantite * prix_unitaire; ' +
  'END; ' +
  '$$ LANGUAGE plpgsql'
);

// Utiliser la fonction
FDQuery1.SQL.Text := 'SELECT calculer_prix_total(5, 19.99) AS total';
FDQuery1.Open;
ShowMessage('Total : ' + FormatFloat('#,##0.00 €', FDQuery1.FieldByName('total').AsFloat));
```

### Différences notables entre MySQL et PostgreSQL

Si vous migrez de MySQL vers PostgreSQL, voici quelques différences syntaxiques à connaître :

| Fonctionnalité | MySQL | PostgreSQL |
|----------------|-------|------------|
| Auto-increment | `AUTO_INCREMENT` | `SERIAL` |
| Guillemets pour les identifiants | \`nom_table\` | "nom_table" |
| Concaténation de chaînes | `CONCAT(a, b)` | `a \|\| b` |
| LIMIT avec offset | `LIMIT 10, 5` | `LIMIT 5 OFFSET 10` |
| Sensibilité à la casse | Insensible sur Windows | Sensible (sauf configuration contraire) |
| Nom du type booléen | `TINYINT(1)` | `BOOLEAN` |

## SQL Server : La solution d'entreprise de Microsoft

### Qu'est-ce que SQL Server ?

Microsoft SQL Server est un système de gestion de base de données relationnelle développé par Microsoft. Il est particulièrement répandu dans les entreprises qui utilisent des technologies Microsoft.

### Avantages de SQL Server

- **Excellente intégration avec l'écosystème Microsoft**
- **Outils d'administration puissants (SQL Server Management Studio)**
- **Fonctionnalités avancées de Business Intelligence**
- **Haute disponibilité et récupération de données**
- **Support et documentation étendus**

### Limitations de SQL Server

- **Coût des licences pour les versions complètes**
- **Consommation de ressources importante**
- **Principalement conçu pour Windows**

### Installation de SQL Server

Pour utiliser SQL Server, vous avez plusieurs options :

1. **SQL Server Express** : Version gratuite, limitée mais suffisante pour débuter
2. **SQL Server Developer** : Version complète gratuite pour le développement non-production
3. **SQL Server Standard/Enterprise** : Versions payantes pour la production

Pour installer SQL Server Express :
1. Téléchargez-le sur le [site de Microsoft](https://www.microsoft.com/fr-fr/sql-server/sql-server-downloads)
2. Exécutez l'installateur et choisissez "Installation de base"
3. Notez le nom de l'instance (typiquement "SQLEXPRESS")
4. Installez également SQL Server Management Studio (SSMS)

### Configuration de SQL Server dans Delphi

#### 1. Connexion à SQL Server

```delphi
procedure TForm1.ConfigurerConnexionSQLServer;
begin
  FDConnection1.Params.Clear;
  FDConnection1.DriverName := 'MSSQL';

  // Paramètres de connexion
  FDConnection1.Params.Add('Server=localhost\SQLEXPRESS'); // Nom du serveur + instance
  FDConnection1.Params.Add('Database=ma_base');            // Nom de la base

  // Authentification Windows (recommandée)
  FDConnection1.Params.Add('OSAuthent=Yes');

  // OU Authentification SQL Server
  // FDConnection1.Params.Add('User_Name=sa');
  // FDConnection1.Params.Add('Password=mon_mot_de_passe');

  // Options supplémentaires
  FDConnection1.Params.Add('ApplicationName=Mon Application Delphi');

  try
    FDConnection1.Connected := True;
    ShowMessage('Connexion à SQL Server réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

#### 2. Création de tables dans SQL Server

```delphi
procedure TForm1.CreerTablesSQLServer;
begin
  // Créer une table clients avec des types SQL Server
  FDConnection1.ExecSQL(
    'CREATE TABLE clients (' +
    '  id INT IDENTITY(1,1) PRIMARY KEY,' +  // IDENTITY = auto-incrémentation
    '  nom NVARCHAR(100) NOT NULL,' +        // NVARCHAR = Unicode
    '  prenom NVARCHAR(100),' +
    '  email NVARCHAR(100),' +
    '  telephone NVARCHAR(20),' +
    '  date_naissance DATE,' +
    '  solde DECIMAL(10,2) DEFAULT 0,' +
    '  actif BIT DEFAULT 1,' +               // BIT = booléen (0 ou 1)
    '  date_creation DATETIME DEFAULT GETDATE()' + // Fonction SQL Server
    ')'
  );

  // Créer une table avec une clé étrangère
  FDConnection1.ExecSQL(
    'CREATE TABLE commandes (' +
    '  id INT IDENTITY(1,1) PRIMARY KEY,' +
    '  client_id INT NOT NULL,' +
    '  date_commande DATETIME DEFAULT GETDATE(),' +
    '  montant DECIMAL(10,2) NOT NULL,' +
    '  statut NVARCHAR(20) DEFAULT ''En attente'',' +
    '  CONSTRAINT fk_client FOREIGN KEY (client_id) REFERENCES clients(id)' +
    ')'
  );
end;
```

### Spécificités de SQL Server à connaître

#### Types de données

SQL Server possède des types de données spécifiques :
- `NVARCHAR` : chaînes Unicode (recommandé pour le multilinguisme)
- `VARCHAR` : chaînes non-Unicode (économise de l'espace si vous n'avez pas besoin d'Unicode)
- `DATETIME2` : plus précis que `DATETIME`
- `BIT` : pour les valeurs booléennes
- `UNIQUEIDENTIFIER` : pour les GUID/UUID

#### Schémas

Comme PostgreSQL, SQL Server utilise le concept de schémas :

```delphi
// Créer un nouveau schéma
FDConnection1.ExecSQL('CREATE SCHEMA ventes');

// Créer une table dans un schéma spécifique
FDConnection1.ExecSQL(
  'CREATE TABLE ventes.clients (' +
  '  id INT IDENTITY(1,1) PRIMARY KEY,' +
  '  nom NVARCHAR(100) NOT NULL' +
  ')'
);

// Interroger une table dans un schéma spécifique
FDQuery1.SQL.Text := 'SELECT * FROM ventes.clients';
FDQuery1.Open;
```

#### Fonctions et procédures stockées

SQL Server est très puissant pour les procédures stockées :

```delphi
// Créer une procédure stockée
FDConnection1.ExecSQL(
  'CREATE PROCEDURE sp_inserer_client ' +
  '  @nom NVARCHAR(100), ' +
  '  @prenom NVARCHAR(100), ' +
  '  @email NVARCHAR(100) ' +
  'AS ' +
  'BEGIN ' +
  '  INSERT INTO clients (nom, prenom, email) ' +
  '  VALUES (@nom, @prenom, @email); ' +
  '  SELECT SCOPE_IDENTITY() AS nouveau_id; ' + // Retourne l'ID généré
  'END'
);

// Appeler une procédure stockée
FDQuery1.SQL.Text := 'EXEC sp_inserer_client @nom, @prenom, @email';
FDQuery1.ParamByName('nom').AsString := 'Dupont';
FDQuery1.ParamByName('prenom').AsString := 'Jean';
FDQuery1.ParamByName('email').AsString := 'jean.dupont@exemple.com';
FDQuery1.Open;

// Récupérer l'ID généré
ShowMessage('Nouveau client créé avec ID : ' + FDQuery1.FieldByName('nouveau_id').AsString);
```

### Différences notables entre MySQL et SQL Server

Si vous migrez de MySQL vers SQL Server, voici quelques différences syntaxiques importantes :

| Fonctionnalité | MySQL | SQL Server |
|----------------|-------|------------|
| Auto-increment | `AUTO_INCREMENT` | `IDENTITY(1,1)` |
| Limite de résultats | `LIMIT 10` | `TOP 10` ou `OFFSET 0 ROWS FETCH NEXT 10 ROWS ONLY` |
| Concaténation | `CONCAT(a, b)` | `a + b` |
| Date actuelle | `NOW()` | `GETDATE()` |
| Guillemets | \`nom_table\` ou 'chaîne' | [nom_table] et 'chaîne' |
| Insensibilité à la casse | Par défaut | Configurable par collation |

## Adapter votre code pour plusieurs bases de données

L'un des grands avantages de FireDAC est sa capacité à travailler avec différents moteurs de bases de données de manière transparente. Voici comment concevoir votre application pour qu'elle fonctionne avec plusieurs moteurs.

### 1. Utiliser des requêtes compatibles

Certaines fonctions SQL sont spécifiques à chaque moteur. Pour une compatibilité maximale, utilisez uniquement les fonctionnalités communes :

```delphi
// NON PORTABLE (utilise des fonctions spécifiques)
case FDConnection1.DriverName of
  'SQLite': FDQuery1.SQL.Text := 'SELECT datetime(''now'') as date_actuelle';
  'MySQL': FDQuery1.SQL.Text := 'SELECT NOW() as date_actuelle';
  'PG': FDQuery1.SQL.Text := 'SELECT CURRENT_TIMESTAMP as date_actuelle';
  'MSSQL': FDQuery1.SQL.Text := 'SELECT GETDATE() as date_actuelle';
end;

// PORTABLE (laisse FireDAC gérer la date)
FDQuery1.SQL.Text := 'SELECT :date_actuelle as date_actuelle';
FDQuery1.ParamByName('date_actuelle').AsDateTime := Now;
```

### 2. Utiliser le générateur de SQL de FireDAC

FireDAC propose une fonctionnalité de génération SQL qui adapte les requêtes à chaque moteur :

```delphi
procedure TForm1.GenererRequetePortable;
var
  SQLText: string;
begin
  // Construire une requête abstraite
  SQLText := FDConnection1.SQLGenerator.GenerateSelectTable('clients', '*',
    'date_creation > :date AND actif = :actif', 'nom ASC', 10);

  FDQuery1.SQL.Text := SQLText;
  FDQuery1.ParamByName('date').AsDateTime := EncodeDate(2023, 1, 1);
  FDQuery1.ParamByName('actif').AsBoolean := True;
  FDQuery1.Open;
end;
```

### 3. Adapter les scripts de création de tables

Pour les scripts de création de tables, vous pouvez écrire une fonction qui génère le bon script selon la base de données :

```delphi
function TForm1.ObtenirScriptCreationTable: string;
begin
  case FDConnection1.DriverName of
    'SQLite':
      Result :=
        'CREATE TABLE IF NOT EXISTS clients (' +
        '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
        '  nom TEXT NOT NULL,' +
        '  date_creation TEXT DEFAULT CURRENT_TIMESTAMP' +
        ')';

    'MySQL':
      Result :=
        'CREATE TABLE IF NOT EXISTS clients (' +
        '  id INT AUTO_INCREMENT PRIMARY KEY,' +
        '  nom VARCHAR(100) NOT NULL,' +
        '  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP' +
        ')';

    'PG':
      Result :=
        'CREATE TABLE IF NOT EXISTS clients (' +
        '  id SERIAL PRIMARY KEY,' +
        '  nom VARCHAR(100) NOT NULL,' +
        '  date_creation TIMESTAMP DEFAULT CURRENT_TIMESTAMP' +
        ')';

    'MSSQL':
      Result :=
        'IF NOT EXISTS (SELECT * FROM sys.tables WHERE name = ''clients'') ' +
        'CREATE TABLE clients (' +
        '  id INT IDENTITY(1,1) PRIMARY KEY,' +
        '  nom NVARCHAR(100) NOT NULL,' +
        '  date_creation DATETIME DEFAULT GETDATE()' +
        ')';

    else
      raise Exception.Create('Moteur de base de données non pris en charge');
  end;
end;
```

### 4. Adapter les noms des pilotes FireDAC

Pour permettre à l'utilisateur de choisir la base de données, vous pouvez proposer une interface de sélection :

```delphi
procedure TForm1.ComboBoxSGBDChange(Sender: TObject);
begin
  // Définir le pilote en fonction du choix de l'utilisateur
  case ComboBoxSGBD.ItemIndex of
    0: // SQLite
      begin
        FDConnection1.DriverName := 'SQLite';
        FDConnection1.Params.Database := ExtractFilePath(Application.ExeName) + 'ma_base.db';
      end;

    1: // MySQL
      begin
        FDConnection1.DriverName := 'MySQL';
        FDConnection1.Params.Add('Server=localhost');
        FDConnection1.Params.Add('Database=ma_base');
        FDConnection1.Params.Add('User_Name=root');
        FDConnection1.Params.Add('Password=mot_de_passe');
      end;

    2: // PostgreSQL
      begin
        FDConnection1.DriverName := 'PG';
        FDConnection1.Params.Add('Server=localhost');
        FDConnection1.Params.Add('Database=ma_base');
        FDConnection1.Params.Add('User_Name=postgres');
        FDConnection1.Params.Add('Password=mot_de_passe');
      end;

    3: // SQL Server
      begin
        FDConnection1.DriverName := 'MSSQL';
        FDConnection1.Params.Add('Server=localhost\SQLEXPRESS');
        FDConnection1.Params.Add('Database=ma_base');
        FDConnection1.Params.Add('OSAuthent=Yes');
      end;
  end;

  try
    FDConnection1.Connected := True;
    ShowMessage('Connexion réussie !');
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;
```

### 5. Gérer les différences de séquences et identités

Pour récupérer l'ID généré après une insertion, chaque moteur a sa propre approche :

```delphi
function TForm1.InsererClientEtObtenirID(const Nom, Prenom: string): Integer;
begin
  Result := -1;

  FDConnection1.StartTransaction;
  try
    // Insérer le client
    FDQuery1.SQL.Text := 'INSERT INTO clients (nom, prenom) VALUES (:nom, :prenom)';
    FDQuery1.ParamByName('nom').AsString := Nom;
    FDQuery1.ParamByName('prenom').AsString := Prenom;
    FDQuery1.ExecSQL;

    // Récupérer l'ID selon le moteur
    case FDConnection1.DriverName of
      'SQLite':
        begin
          FDQuery1.SQL.Text := 'SELECT last_insert_rowid() AS id';
          FDQuery1.Open;
          Result := FDQuery1.FieldByName('id').AsInteger;
        end;

      'MySQL':
        begin
          FDQuery1.SQL.Text := 'SELECT LAST_INSERT_ID() AS id';
          FDQuery1.Open;
          Result := FDQuery1.FieldByName('id').AsInteger;
        end;

      'PG':
        begin
          FDQuery1.SQL.Text := 'SELECT currval(''clients_id_seq'') AS id';
          FDQuery1.Open;
          Result := FDQuery1.FieldByName('id').AsInteger;
        end;

      'MSSQL':
        begin
          FDQuery1.SQL.Text := 'SELECT SCOPE_IDENTITY() AS id';
          FDQuery1.Open;
          Result := FDQuery1.FieldByName('id').AsInteger;
        end;
    end;

    FDConnection1.Commit;
  except
    on E: Exception do
    begin
      FDConnection1.Rollback;
      ShowMessage('Erreur : ' + E.Message);
      Result := -1;
    end;
  end;
end;
```

### 6. Méthode simplifiée avec FireDAC

FireDAC propose une méthode plus simple pour récupérer l'ID généré, qui fonctionne avec tous les moteurs :

```delphi
function TForm1.InsererClientEtObtenirIDSimple(const Nom, Prenom: string): Integer;
var
  Command: TFDCommand;
begin
  Result := -1;

  Command := TFDCommand.Create(nil);
  try
    Command.Connection := FDConnection1;
    Command.CommandText.Text := 'INSERT INTO clients (nom, prenom) VALUES (:nom, :prenom)';
    Command.ParamByName('nom').AsString := Nom;
    Command.ParamByName('prenom').AsString := Prenom;
    Command.Prepare; // Important pour les performances

    // Configurer pour récupérer l'ID généré
    Command.FetchOptions.Items := [fiGenerated];
    Command.Execute;

    // Récupérer l'ID généré (fonctionne avec tous les moteurs supportés)
    Result := Command.GetLastAutoGenValue;
  finally
    Command.Free;
  end;
end;
```

## Requêtes spécifiques aux moteurs

Bien que la portabilité soit un objectif souhaitable, parfois vous aurez besoin d'utiliser des fonctionnalités spécifiques à un moteur de base de données. Voici quelques exemples :

### SQLite : Fonctions et PRAGMA

```delphi
// Activer les clés étrangères (désactivées par défaut dans SQLite)
FDConnection1.ExecSQL('PRAGMA foreign_keys = ON');

// Récupérer la version de SQLite
FDQuery1.SQL.Text := 'SELECT sqlite_version()';
FDQuery1.Open;
ShowMessage('Version SQLite : ' + FDQuery1.Fields[0].AsString);

// Utiliser des fonctions d'agrégation
FDQuery1.SQL.Text :=
  'SELECT ' +
  '  strftime(''%Y'', date_creation) AS annee, ' +
  '  COUNT(*) AS nombre ' +
  'FROM clients ' +
  'GROUP BY annee';
FDQuery1.Open;
```

### PostgreSQL : Types avancés et requêtes JSON

```delphi
// Utiliser un type Array
FDConnection1.ExecSQL(
  'CREATE TABLE produits (' +
  '  id SERIAL PRIMARY KEY,' +
  '  nom VARCHAR(100),' +
  '  tags TEXT[]' + // Type array
  ')'
);

// Insérer dans un array
FDQuery1.SQL.Text := 'INSERT INTO produits (nom, tags) VALUES (:nom, :tags)';
FDQuery1.ParamByName('nom').AsString := 'Smartphone';
FDQuery1.ParamByName('tags').AsString := '{électronique,mobile,hightech}';
FDQuery1.ExecSQL;

// Rechercher dans un array
FDQuery1.SQL.Text := 'SELECT * FROM produits WHERE ''mobile'' = ANY(tags)';
FDQuery1.Open;

// Utiliser des requêtes JSON
FDQuery1.SQL.Text :=
  'SELECT ' +
  '  id, ' +
  '  nom, ' +
  '  caracteristiques->''couleur'' AS couleur, ' +
  '  (caracteristiques->''dimensions''->>''hauteur'')::numeric AS hauteur ' +
  'FROM produits ' +
  'WHERE caracteristiques @> ''{"memoire": 128}''';
FDQuery1.Open;
```

### SQL Server : Fonctions de fenêtrage et partition

```delphi
// Utiliser des fonctions de fenêtrage (Window Functions)
FDQuery1.SQL.Text :=
  'SELECT ' +
  '  nom, ' +
  '  prenom, ' +
  '  ville, ' +
  '  solde, ' +
  '  RANK() OVER (PARTITION BY ville ORDER BY solde DESC) AS classement ' +
  'FROM clients';
FDQuery1.Open;

// Utiliser des CTE (Common Table Expressions)
FDQuery1.SQL.Text :=
  'WITH ClientsAvecSolde AS (' +
  '  SELECT ' +
  '    id, ' +
  '    nom, ' +
  '    solde, ' +
  '    CASE ' +
  '      WHEN solde < 0 THEN ''Négatif'' ' +
  '      WHEN solde = 0 THEN ''Zéro'' ' +
  '      ELSE ''Positif'' ' +
  '    END AS type_solde ' +
  '  FROM clients' +
  ') ' +
  'SELECT type_solde, COUNT(*) AS nombre ' +
  'FROM ClientsAvecSolde ' +
  'GROUP BY type_solde';
FDQuery1.Open;
```

## Maintenir une base de données pour un produit commercial

Si vous développez une application commerciale, vous pourriez avoir besoin de supporter plusieurs moteurs de bases de données. Voici quelques conseils pratiques :

### 1. Utiliser un fichier de configuration pour le type de base de données

```delphi
procedure TApp.InitialiserBaseDeDonnees;
var
  Config: TIniFile;
  DriverName: string;
  Server, Database, Username, Password: string;
begin
  Config := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    // Lire la configuration
    DriverName := Config.ReadString('Database', 'DriverName', 'SQLite');

    // Configurer la connexion
    FDConnection1.Params.Clear;
    FDConnection1.DriverName := DriverName;

    case DriverName of
      'SQLite':
        begin
          // Configurer SQLite
          FDConnection1.Params.Database := Config.ReadString('Database', 'DatabaseFile',
            ExtractFilePath(Application.ExeName) + 'database.db');
        end;

      'MySQL', 'PG', 'MSSQL':
        begin
          // Configurer un SGBD client-serveur
          Server := Config.ReadString('Database', 'Server', 'localhost');
          Database := Config.ReadString('Database', 'Database', 'ma_base');
          Username := Config.ReadString('Database', 'Username', '');
          Password := Config.ReadString('Database', 'Password', '');

          FDConnection1.Params.Add('Server=' + Server);
          FDConnection1.Params.Add('Database=' + Database);

          if Username <> '' then
            FDConnection1.Params.Add('User_Name=' + Username);

          if Password <> '' then
            FDConnection1.Params.Add('Password=' + Password);

          // Options spécifiques par moteur
          if DriverName = 'MSSQL' then
            FDConnection1.Params.Add('OSAuthent=' +
              Config.ReadString('Database', 'WindowsAuth', 'No'));
        end;
    end;
  finally
    Config.Free;
  end;
end;
```

### 2. Scripts de migration pour tous les moteurs

Lors des mises à jour de votre application, vous devrez peut-être migrer le schéma de la base de données. Préparez des scripts pour chaque moteur :

```delphi
procedure TApp.MigrerVers120;
var
  Script: string;
begin
  case FDConnection1.DriverName of
    'SQLite': Script :=
      'ALTER TABLE clients ADD COLUMN preference TEXT; ' +
      'CREATE INDEX idx_clients_preference ON clients(preference);';

    'MySQL': Script :=
      'ALTER TABLE clients ADD COLUMN preference VARCHAR(50); ' +
      'CREATE INDEX idx_clients_preference ON clients(preference);';

    'PG': Script :=
      'ALTER TABLE clients ADD COLUMN preference VARCHAR(50); ' +
      'CREATE INDEX idx_clients_preference ON clients(preference);';

    'MSSQL': Script :=
      'ALTER TABLE clients ADD preference NVARCHAR(50); ' +
      'CREATE INDEX idx_clients_preference ON clients(preference);';
  end;

  FDConnection1.ExecSQL(Script);
end;
```

### 3. Utiliser une couche d'abstraction

Pour les applications complexes, créez une couche d'abstraction qui masque les différences entre les moteurs :

```delphi
// UDatabaseLayer.pas
unit UDatabaseLayer;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, FireDAC.Stan.Param;

type
  TDatabaseLayer = class
  private
    FConnection: TFDConnection;
    function GetDriverName: string;
  public
    constructor Create(AConnection: TFDConnection);

    // Fonctions génériques
    function ExecSQL(const ASQL: string): Integer;
    function QuerySingle(const ASQL: string): Variant;

    // Fonctions spécifiques
    function GetCurrentDate: string;
    function GetLastInsertID(const TableName: string): Integer;
    function CreateLimitedQuery(const BaseQuery: string; Limit, Offset: Integer): string;

    property DriverName: string read GetDriverName;
  end;

implementation

constructor TDatabaseLayer.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

function TDatabaseLayer.GetDriverName: string;
begin
  Result := FConnection.DriverName;
end;

function TDatabaseLayer.ExecSQL(const ASQL: string): Integer;
begin
  Result := FConnection.ExecSQL(ASQL);
end;

function TDatabaseLayer.QuerySingle(const ASQL: string): Variant;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := ASQL;
    Query.Open;

    if Query.IsEmpty then
      Result := Null
    else
      Result := Query.Fields[0].Value;
  finally
    Query.Free;
  end;
end;

function TDatabaseLayer.GetCurrentDate: string;
begin
  case FConnection.DriverName of
    'SQLite': Result := "SELECT datetime('now')";
    'MySQL': Result := 'SELECT NOW()';
    'PG': Result := 'SELECT CURRENT_TIMESTAMP';
    'MSSQL': Result := 'SELECT GETDATE()';
    else Result := '';
  end;
end;

function TDatabaseLayer.GetLastInsertID(const TableName: string): Integer;
var
  SQL: string;
begin
  case FConnection.DriverName of
    'SQLite': SQL := 'SELECT last_insert_rowid()';
    'MySQL': SQL := 'SELECT LAST_INSERT_ID()';
    'PG': SQL := Format('SELECT currval(''%s_id_seq'')', [TableName]);
    'MSSQL': SQL := 'SELECT SCOPE_IDENTITY()';
    else SQL := '';
  end;

  Result := QuerySingle(SQL);
end;

function TDatabaseLayer.CreateLimitedQuery(const BaseQuery: string;
  Limit, Offset: Integer): string;
begin
  case FConnection.DriverName of
    'SQLite', 'MySQL', 'PG':
      Result := Format('%s LIMIT %d OFFSET %d', [BaseQuery, Limit, Offset]);

    'MSSQL':
      Result := Format('%s OFFSET %d ROWS FETCH NEXT %d ROWS ONLY',
                [BaseQuery, Offset, Limit]);

    else Result := BaseQuery;
  end;
end;

end.
```

## Conclusion

FireDAC offre une grande flexibilité pour travailler avec différents moteurs de bases de données. Chaque moteur a ses propres forces et faiblesses :

- **SQLite** est idéal pour les applications simples, mobiles ou embarquées
- **PostgreSQL** est excellent pour les applications avancées nécessitant de riches fonctionnalités
- **SQL Server** est parfait pour une intégration dans un environnement Microsoft existant
- **MySQL/MariaDB** (que nous avons vu précédemment) reste un bon choix polyvalent

En comprenant les spécificités de chaque moteur et en concevant votre application de manière appropriée, vous pouvez créer des solutions Delphi robustes adaptées à différents contextes.

Pour choisir le moteur de base de données le plus adapté à votre projet, posez-vous ces questions :
- Quel est le volume de données prévu ?
- Combien d'utilisateurs simultanés ?
- L'application est-elle déployée sur un seul poste ou en réseau ?
- Avez-vous besoin de fonctionnalités avancées (JSON, géospatial, etc.) ?
- Quelles sont les contraintes de coût et de licence ?

Quel que soit votre choix, FireDAC rend l'interaction avec la base de données simple et efficace, vous permettant de vous concentrer sur la logique métier de votre application.

---

**À suivre :** 8.13 NoSQL et bases de données documentaires
