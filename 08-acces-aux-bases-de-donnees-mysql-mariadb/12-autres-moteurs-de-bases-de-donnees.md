🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.12 Autres moteurs de bases de données (SQLite, PostgreSQL, SQL Server)

## Introduction

Jusqu'à présent, nous avons travaillé principalement avec MySQL/MariaDB. Mais le monde des bases de données est vaste, et chaque moteur de base de données a ses propres caractéristiques, avantages et cas d'usage optimaux.

FireDAC, la technologie d'accès aux données de Delphi, supporte plus de **20 moteurs de bases de données différents**. Dans ce chapitre, nous allons explorer trois alternatives majeures à MySQL/MariaDB : **SQLite**, **PostgreSQL** et **SQL Server**.

## Vue d'ensemble des moteurs de bases de données

### Les familles de bases de données

```
┌─────────────────────────────────────────────┐
│         BASES DE DONNÉES RELATIONNELLES     │
├─────────────────────────────────────────────┤
│  Embarquées        │  Client-Serveur        │
│  - SQLite          │  - MySQL/MariaDB       │
│  - Firebird        │  - PostgreSQL          │
│  - Access          │  - SQL Server          │
│  - LocalDB         │  - Oracle              │
│                    │  - DB2                 │
└─────────────────────────────────────────────┘
```

### Critères de choix

| Critère | Questions à se poser |
|---------|----------------------|
| **Architecture** | Application mono-poste ou client-serveur ? |
| **Volumétrie** | Combien de données ? Combien d'utilisateurs ? |
| **Performance** | Besoins en vitesse ? Transactions lourdes ? |
| **Coût** | Budget pour licences ? Open-source possible ? |
| **Plateforme** | Windows uniquement ? Multi-plateformes ? |
| **Fonctionnalités** | Besoin de fonctionnalités avancées ? |
| **Support** | Besoin d'un support commercial ? |
| **Compétences** | Expertise de l'équipe ? |

## SQLite : La base de données embarquée

### Qu'est-ce que SQLite ?

**SQLite** est une base de données **embarquée** (embedded), c'est-à-dire qu'elle ne nécessite pas de serveur séparé. Toute la base de données est contenue dans **un seul fichier**.

```
Application Delphi
    │
    ├─ Code de l'application
    └─ fichier_base.db  ← Toute la base dans un fichier
```

### Caractéristiques principales

| Caractéristique | Description |
|----------------|-------------|
| **Type** | Base de données embarquée |
| **Licence** | Domaine public (gratuit) |
| **Taille** | Très légère (~500 Ko) |
| **Plateformes** | Windows, macOS, Linux, iOS, Android |
| **Architecture** | Sans serveur |
| **Format** | Un seul fichier .db ou .sqlite |
| **Utilisateurs simultanés** | Lecture : illimitée, Écriture : un seul |

### Avantages de SQLite

✅ **Aucune installation requise** : pas de serveur à configurer
✅ **Ultra-portable** : copiez le fichier, c'est tout
✅ **Léger** : empreinte mémoire minimale
✅ **Rapide** : très performant pour lecture
✅ **Fiable** : transactions ACID complètes
✅ **Multiplateforme** : fonctionne partout
✅ **Gratuit** : domaine public, pas de licence

### Limitations de SQLite

⚠️ **Pas de concurrence en écriture** : un seul écrivain à la fois
⚠️ **Pas de gestion d'utilisateurs** : pas d'authentification intégrée
⚠️ **Pas de serveur distant** : accès fichier local uniquement
⚠️ **Fonctionnalités limitées** : moins riche que PostgreSQL/MySQL
⚠️ **Pas de procédures stockées**

### Quand utiliser SQLite ?

**✅ Idéal pour :**
- Applications **desktop** mono-poste
- Applications **mobiles** (iOS/Android)
- Applications **portables** sans installation
- **Prototypage** rapide
- Stockage de **configuration** ou **cache**
- Bases de données **embarquées** dans l'application
- **Petites** à moyennes volumétries (< 1 Go)

**❌ Éviter pour :**
- Applications avec **nombreux utilisateurs** simultanés en écriture
- Serveurs web avec **trafic élevé**
- Données **critiques** nécessitant haute disponibilité
- Besoin de **réplication** ou **clustering**

### Configuration de SQLite dans Delphi

#### Étape 1 : Ajouter les composants

```pascal
// Composants nécessaires sur le DataModule
TFDConnection: FDConnection1
TFDPhysSQLiteDriverLink: FDPhysSQLiteDriverLink1
TFDQuery: FDQueryClients
```

#### Étape 2 : Configurer la connexion

```pascal
procedure TdmDatabase.ConfigurerSQLite;
var
  CheminBase: string;
begin
  // Chemin du fichier de base de données
  CheminBase := ExtractFilePath(Application.ExeName) + 'data\mabase.db';

  // Configuration de la connexion
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=SQLite');
  FDConnection1.Params.Add('Database=' + CheminBase);

  // Options recommandées
  FDConnection1.Params.Add('LockingMode=Normal');
  FDConnection1.Params.Add('Synchronous=NORMAL');
  FDConnection1.Params.Add('JournalMode=WAL');  // Write-Ahead Logging

  FDConnection1.LoginPrompt := False;

  // Créer le fichier s'il n'existe pas
  if not FileExists(CheminBase) then
    ForceDirectories(ExtractFileDir(CheminBase));

  FDConnection1.Connected := True;
end;
```

#### Étape 3 : Créer les tables

```pascal
procedure CreerStructureSQLite;
begin
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS clients ( ' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  nom TEXT NOT NULL, ' +
    '  prenom TEXT NOT NULL, ' +
    '  email TEXT UNIQUE, ' +
    '  telephone TEXT, ' +
    '  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP ' +
    ')'
  );
end;
```

### Types de données SQLite

SQLite utilise un **typage dynamique** différent des autres bases :

| Type SQLite | Type Delphi | Exemple |
|-------------|-------------|---------|
| `INTEGER` | Integer, Int64 | `id INTEGER` |
| `REAL` | Double | `prix REAL` |
| `TEXT` | String | `nom TEXT` |
| `BLOB` | TBytes | `photo BLOB` |
| `NUMERIC` | Variant | `valeur NUMERIC` |

**Note :** SQLite stocke les dates/heures comme TEXT ou INTEGER, pas de type DATE natif.

### Particularités SQL de SQLite

```sql
-- Auto-incrément
CREATE TABLE clients (
    id INTEGER PRIMARY KEY AUTOINCREMENT,  -- Pas AUTO_INCREMENT !
    nom TEXT
);

-- Date et heure
INSERT INTO clients (nom, date_creation)
VALUES ('Dupont', datetime('now'));  -- datetime() au lieu de NOW()

-- Obtenir l'ID inséré
SELECT last_insert_rowid();  -- Au lieu de LAST_INSERT_ID()

-- Limite et offset
SELECT * FROM clients
LIMIT 10 OFFSET 20;  -- Syntaxe standard

-- Pas de BOOLEAN natif
CREATE TABLE clients (
    actif INTEGER DEFAULT 1  -- 0 = false, 1 = true
);
```

### Exemple complet avec SQLite

```pascal
unit uDatabaseSQLite;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Stan.Def;

type
  TDatabaseSQLite = class
  private
    FConnection: TFDConnection;
    FDriverLink: TFDPhysSQLiteDriverLink;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitialiserBase;
    procedure AjouterClient(const Nom, Prenom, Email: string);
    function ListerClients: TFDQuery;
  end;

implementation

constructor TDatabaseSQLite.Create;
var
  CheminBase: string;
begin
  inherited Create;

  // Créer les composants
  FDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
  FConnection := TFDConnection.Create(nil);

  // Configuration
  CheminBase := ExtractFilePath(ParamStr(0)) + 'clients.db';
  FConnection.Params.Add('DriverID=SQLite');
  FConnection.Params.Add('Database=' + CheminBase);
  FConnection.Params.Add('JournalMode=WAL');
  FConnection.LoginPrompt := False;

  FConnection.Connected := True;
end;

destructor TDatabaseSQLite.Destroy;
begin
  FConnection.Free;
  FDriverLink.Free;
  inherited;
end;

procedure TDatabaseSQLite.InitialiserBase;
begin
  FConnection.ExecSQL(
    'CREATE TABLE IF NOT EXISTS clients ( ' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT, ' +
    '  nom TEXT NOT NULL, ' +
    '  prenom TEXT NOT NULL, ' +
    '  email TEXT UNIQUE, ' +
    '  date_creation TEXT DEFAULT (datetime(''now'')) ' +
    ')'
  );
end;

procedure TDatabaseSQLite.AjouterClient(const Nom, Prenom, Email: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email)';

    Query.ParamByName('nom').AsString := Nom;
    Query.ParamByName('prenom').AsString := Prenom;
    Query.ParamByName('email').AsString := Email;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;

function TDatabaseSQLite.ListerClients: TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConnection;
  Result.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
  Result.Open;
end;

end.
```

## PostgreSQL : La base de données avancée

### Qu'est-ce que PostgreSQL ?

**PostgreSQL** (souvent abrégé "Postgres") est un système de gestion de base de données relationnel **open-source** très puissant, réputé pour sa **robustesse** et sa **conformité** aux standards SQL.

### Caractéristiques principales

| Caractéristique | Description |
|----------------|-------------|
| **Type** | Base de données client-serveur |
| **Licence** | PostgreSQL License (open-source, type BSD) |
| **Plateformes** | Windows, macOS, Linux, BSD |
| **Performance** | Excellente, optimisée pour lectures complexes |
| **Extensibilité** | Très extensible (extensions, types personnalisés) |
| **Conformité SQL** | Très stricte, respecte les standards |

### Avantages de PostgreSQL

✅ **Gratuit et open-source** : pas de coût de licence
✅ **Très performant** : excellentes performances en lecture
✅ **Fonctionnalités avancées** : JSON, XML, tableaux, types personnalisés
✅ **Robuste et fiable** : ACID complet, très stable
✅ **Extensible** : nombreuses extensions disponibles
✅ **Conformité SQL** : respecte strictement les standards
✅ **Support des données géospatiales** : PostGIS pour SIG
✅ **Multiplateforme** : fonctionne partout

### Comparaison PostgreSQL vs MySQL

| Aspect | PostgreSQL | MySQL/MariaDB |
|--------|-----------|---------------|
| **Performance lecture** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Performance écriture** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Conformité SQL** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |
| **Fonctionnalités** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Simplicité** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Popularité web** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Extensibilité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |

### Quand utiliser PostgreSQL ?

**✅ Idéal pour :**
- Applications **d'entreprise** complexes
- **Analytique** et reporting avancé
- Applications avec **transactions complexes**
- Données **géospatiales** (avec PostGIS)
- Besoin de **types de données avancés** (JSON, tableaux)
- **Intégrité des données** critique
- Requêtes **complexes** avec jointures multiples

**❌ Moins adapté pour :**
- Applications web simples (MySQL suffit)
- Besoin de simplicité maximale
- Équipe sans expertise PostgreSQL

### Installation de PostgreSQL

#### Windows
1. Téléchargez depuis [postgresql.org/download](https://www.postgresql.org/download/)
2. Exécutez l'installateur
3. Choisissez le mot de passe pour l'utilisateur `postgres`
4. Port par défaut : `5432`

#### Linux (Ubuntu/Debian)
```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
sudo systemctl start postgresql
sudo systemctl enable postgresql
```

### Configuration de PostgreSQL dans Delphi

```pascal
procedure TdmDatabase.ConfigurerPostgreSQL;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=PG');  // PG pour PostgreSQL
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Port=5432');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=postgres');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');
  FDConnection1.Params.Add('CharacterSet=UTF8');

  FDConnection1.LoginPrompt := False;
  FDConnection1.Connected := True;
end;
```

### Composants nécessaires

```pascal
// Sur le DataModule
TFDConnection: FDConnection1
TFDPhysPgDriverLink: FDPhysPgDriverLink1  // Pilote PostgreSQL
TFDQuery: FDQueryClients
```

### Particularités SQL de PostgreSQL

```sql
-- Séquences pour auto-incrément
CREATE SEQUENCE clients_id_seq;

CREATE TABLE clients (
    id INTEGER PRIMARY KEY DEFAULT nextval('clients_id_seq'),
    nom VARCHAR(100)
);

-- Ou plus simple avec SERIAL
CREATE TABLE clients (
    id SERIAL PRIMARY KEY,  -- Équivalent à INTEGER + séquence
    nom VARCHAR(100)
);

-- Typage strict
SELECT * FROM clients WHERE id = 1;  -- OK
SELECT * FROM clients WHERE id = '1';  -- Erreur ! Pas de conversion auto

-- Types avancés
CREATE TABLE produits (
    id SERIAL PRIMARY KEY,
    nom VARCHAR(100),
    tags TEXT[],  -- Tableau de textes
    attributs JSONB  -- JSON binaire
);

-- Insertion avec JSON
INSERT INTO produits (nom, tags, attributs)
VALUES ('Ordinateur', ARRAY['laptop', 'gaming'],
        '{"processeur": "i7", "ram": "16GB"}');

-- RETURNING pour récupérer l'ID
INSERT INTO clients (nom, prenom)
VALUES ('Dupont', 'Jean')
RETURNING id;  -- Retourne directement l'ID

-- Full-text search natif
CREATE INDEX idx_nom_fulltext ON clients USING GIN(to_tsvector('french', nom));

SELECT * FROM clients
WHERE to_tsvector('french', nom) @@ to_tsquery('french', 'dupont');
```

### Exemple avec PostgreSQL

```pascal
procedure TFormMain.UtiliserPostgreSQL;
var
  Query: TFDQuery;
  NouvelID: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Insertion avec RETURNING
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email) ' +
      'RETURNING id';

    Query.ParamByName('nom').AsString := 'Dupont';
    Query.ParamByName('prenom').AsString := 'Jean';
    Query.ParamByName('email').AsString := 'jean@email.fr';

    Query.Open;  // Open et non ExecSQL car RETURNING retourne des données
    NouvelID := Query.FieldByName('id').AsInteger;

    ShowMessage('Client créé avec l''ID : ' + IntToStr(NouvelID));

  finally
    Query.Free;
  end;
end;
```

### Utiliser JSON dans PostgreSQL

```pascal
procedure StockerDonneesJSON;
var
  Query: TFDQuery;
  JSONData: string;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;

    // Créer une table avec colonne JSON
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS produits ( ' +
      '  id SERIAL PRIMARY KEY, ' +
      '  nom VARCHAR(100), ' +
      '  specifications JSONB ' +  // JSONB = JSON binaire (plus performant)
      ')';
    Query.ExecSQL;

    // Insérer avec JSON
    JSONData := '{"couleur": "rouge", "taille": "M", "poids": 1.5}';

    Query.SQL.Text :=
      'INSERT INTO produits (nom, specifications) ' +
      'VALUES (:nom, :specs::jsonb)';

    Query.ParamByName('nom').AsString := 'T-Shirt';
    Query.ParamByName('specs').AsString := JSONData;
    Query.ExecSQL;

    // Requête sur JSON
    Query.SQL.Text :=
      'SELECT nom, specifications->>''couleur'' AS couleur ' +
      'FROM produits ' +
      'WHERE specifications->>''taille'' = ''M''';
    Query.Open;

    ShowMessage('Produit : ' + Query.FieldByName('nom').AsString +
                ', Couleur : ' + Query.FieldByName('couleur').AsString);

  finally
    Query.Free;
  end;
end;
```

## SQL Server : La solution Microsoft

### Qu'est-ce que SQL Server ?

**Microsoft SQL Server** est le système de gestion de base de données **commercial** de Microsoft, optimisé pour l'écosystème Windows et étroitement intégré avec les technologies Microsoft.

### Caractéristiques principales

| Caractéristique | Description |
|----------------|-------------|
| **Type** | Base de données client-serveur |
| **Licence** | Commercial (payant) + Express (gratuit limité) |
| **Plateformes** | Windows (principal), Linux (depuis 2017) |
| **Intégration** | Excellente avec .NET, Azure, Microsoft Stack |
| **Outils** | SQL Server Management Studio (SSMS) |
| **Versions** | Express (gratuit), Standard, Enterprise |

### Éditions de SQL Server

| Édition | Coût | Limites | Usage |
|---------|------|---------|-------|
| **Express** | Gratuit | 10 GB par base, 1 GB RAM | Développement, petites apps |
| **Standard** | ~$900/licence | Limites moyennes | PME |
| **Enterprise** | ~$7000/licence | Aucune limite | Grandes entreprises |
| **Developer** | Gratuit | Complet (non production) | Développement |

### Avantages de SQL Server

✅ **Intégration Microsoft** : parfait avec Windows, .NET, Azure
✅ **Outils graphiques** : SSMS très puissant
✅ **Performance** : très performant, optimisé pour Windows
✅ **Support commercial** : support Microsoft disponible
✅ **Sécurité avancée** : chiffrement, audit, masquage dynamique
✅ **Business Intelligence** : SSRS, SSIS, SSAS intégrés
✅ **Haute disponibilité** : clustering, mirroring, Always On

### Inconvénients

⚠️ **Coût** : licences coûteuses (sauf Express)
⚠️ **Windows** : principalement orienté Windows
⚠️ **Dépendance Microsoft** : lock-in écosystème
⚠️ **Complexité** : peut être complexe à administrer

### Quand utiliser SQL Server ?

**✅ Idéal pour :**
- Environnement **100% Microsoft** (Windows, .NET, Azure)
- **Entreprises** avec budget pour licences
- Besoin de **support commercial**
- **Business Intelligence** avancée
- Intégration avec **Active Directory**
- **Reporting** avec SSRS
- Applications **critiques** nécessitant haute disponibilité

**❌ Moins adapté pour :**
- Budget limité (sauf Express)
- Environnements multi-plateformes
- Open-source préféré
- Startups / petits projets

### Configuration de SQL Server dans Delphi

```pascal
procedure TdmDatabase.ConfigurerSQLServer;
begin
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=MSSQL');  // MSSQL pour SQL Server
  FDConnection1.Params.Add('Server=localhost\SQLEXPRESS');  // Instance SQL Server
  FDConnection1.Params.Add('Database=MaBase');

  // Authentification Windows (recommandé)
  FDConnection1.Params.Add('OSAuthent=Yes');

  // Ou authentification SQL Server
  // FDConnection1.Params.Add('User_Name=sa');
  // FDConnection1.Params.Add('Password=MonMotDePasse');

  FDConnection1.LoginPrompt := False;
  FDConnection1.Connected := True;
end;
```

### Composants nécessaires

```pascal
// Sur le DataModule
TFDConnection: FDConnection1
TFDPhysMSSQLDriverLink: FDPhysMSSQLDriverLink1  // Pilote SQL Server
TFDQuery: FDQueryClients
```

### Particularités SQL de SQL Server

```sql
-- Identity pour auto-incrément
CREATE TABLE clients (
    id INT IDENTITY(1,1) PRIMARY KEY,  -- IDENTITY au lieu de AUTO_INCREMENT
    nom VARCHAR(100)
);

-- Récupérer l'ID inséré
INSERT INTO clients (nom, prenom) VALUES ('Dupont', 'Jean');
SELECT SCOPE_IDENTITY() AS nouvel_id;  -- Au lieu de LAST_INSERT_ID()

-- TOP au lieu de LIMIT
SELECT TOP 10 * FROM clients;  -- Au lieu de LIMIT 10

-- Noms entre crochets
SELECT [nom], [prenom] FROM [clients];  -- Au lieu de backticks

-- Variables avec @
DECLARE @total INT;
SET @total = (SELECT COUNT(*) FROM clients);

-- Types de données spécifiques
CREATE TABLE donnees (
    id INT PRIMARY KEY,
    texte NVARCHAR(MAX),  -- Unicode, taille illimitée
    date_creation DATETIME2,  -- Plus précis que DATETIME
    donnees_xml XML,  -- Type XML natif
    guid UNIQUEIDENTIFIER  -- GUID natif
);

-- Procédures stockées
CREATE PROCEDURE sp_AjouterClient
    @nom VARCHAR(100),
    @prenom VARCHAR(100),
    @email VARCHAR(150)
AS
BEGIN
    INSERT INTO clients (nom, prenom, email)
    VALUES (@nom, @prenom, @email);

    SELECT SCOPE_IDENTITY() AS nouvel_id;
END;
```

### Exemple avec SQL Server

```pascal
procedure TFormMain.UtiliserSQLServer;
var
  Query: TFDQuery;
  StoredProc: TFDStoredProc;
  NouvelID: Integer;
begin
  // Méthode 1 : Requête SQL classique
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO clients (nom, prenom, email) ' +
      'VALUES (:nom, :prenom, :email); ' +
      'SELECT SCOPE_IDENTITY() AS id';

    Query.ParamByName('nom').AsString := 'Dupont';
    Query.ParamByName('prenom').AsString := 'Jean';
    Query.ParamByName('email').AsString := 'jean@email.fr';

    Query.Open;
    NouvelID := Query.FieldByName('id').AsInteger;

    ShowMessage('ID : ' + IntToStr(NouvelID));
  finally
    Query.Free;
  end;

  // Méthode 2 : Procédure stockée
  StoredProc := TFDStoredProc.Create(nil);
  try
    StoredProc.Connection := FDConnection1;
    StoredProc.StoredProcName := 'sp_AjouterClient';

    StoredProc.ParamByName('nom').AsString := 'Martin';
    StoredProc.ParamByName('prenom').AsString := 'Sophie';
    StoredProc.ParamByName('email').AsString := 'sophie@email.fr';

    StoredProc.ExecProc;

    ShowMessage('Procédure exécutée');
  finally
    StoredProc.Free;
  end;
end;
```

## Comparaison des moteurs

### Tableau récapitulatif

| Critère | SQLite | MySQL/MariaDB | PostgreSQL | SQL Server |
|---------|--------|---------------|------------|------------|
| **Architecture** | Embarquée | Client-serveur | Client-serveur | Client-serveur |
| **Coût** | Gratuit | Gratuit | Gratuit | Payant (Express gratuit) |
| **Plateforme** | Toutes | Toutes | Toutes | Windows (+ Linux) |
| **Concurrence** | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Performance lecture** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Performance écriture** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Simplicité** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |
| **Fonctionnalités** | ⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ |
| **Conformité SQL** | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Support** | Communauté | Communauté | Communauté | Commercial |

### Quel moteur choisir ?

```
┌──────────────────────────────────────────┐
│  Arbre de décision                       │
├──────────────────────────────────────────┤
│  Application mono-poste ?                │
│    Oui → SQLite                          │
│    Non ↓                                 │
│                                          │
│  Environnement Microsoft ?               │
│    Oui → SQL Server                      │
│    Non ↓                                 │
│                                          │
│  Besoin fonctionnalités avancées ?       │
│    Oui → PostgreSQL                      │
│    Non → MySQL/MariaDB                   │
└──────────────────────────────────────────┘
```

## Migration entre moteurs

### Différences de syntaxe SQL

#### Auto-incrément

```sql
-- MySQL
id INT AUTO_INCREMENT PRIMARY KEY

-- PostgreSQL
id SERIAL PRIMARY KEY

-- SQL Server
id INT IDENTITY(1,1) PRIMARY KEY

-- SQLite
id INTEGER PRIMARY KEY AUTOINCREMENT
```

#### Limite de résultats

```sql
-- MySQL, PostgreSQL, SQLite
SELECT * FROM clients LIMIT 10

-- SQL Server
SELECT TOP 10 * FROM clients
```

#### Date actuelle

```sql
-- MySQL
NOW() ou CURRENT_TIMESTAMP

-- PostgreSQL
NOW() ou CURRENT_TIMESTAMP

-- SQL Server
GETDATE() ou CURRENT_TIMESTAMP

-- SQLite
datetime('now')
```

#### Concaténation

```sql
-- MySQL
CONCAT(nom, ' ', prenom)

-- PostgreSQL
nom || ' ' || prenom

-- SQL Server
nom + ' ' + prenom  ou  CONCAT(nom, ' ', prenom)

-- SQLite
nom || ' ' || prenom
```

### Code portable entre moteurs

```pascal
function ObtenirRequeteClients: string;
begin
  case GetDriverID of
    'MySQL', 'MariaDB':
      Result := 'SELECT * FROM clients LIMIT 10';
    'PG':
      Result := 'SELECT * FROM clients LIMIT 10';
    'MSSQL':
      Result := 'SELECT TOP 10 * FROM clients';
    'SQLite':
      Result := 'SELECT * FROM clients LIMIT 10';
  else
    Result := 'SELECT * FROM clients';
  end;
end;

function GetDriverID: string;
begin
  Result := FDConnection1.Params.Values['DriverID'];
end;
```

### Abstraction avec FireDAC

FireDAC offre des mécanismes pour écrire du code portable :

```pascal
// FireDAC traduit automatiquement certaines fonctions
Query.SQL.Text :=
  'SELECT * FROM clients ' +
  'WHERE date_inscription > {fn CURDATE()}';  // Traduit selon le moteur

// Macro FireDAC pour portabilité
Query.SQL.Text :=
  'SELECT !LIMIT(10) * FROM clients';  // Traduit en TOP ou LIMIT
```

## Conseils pratiques

### 1. Tester avec plusieurs moteurs

```pascal
procedure TesterAvecDifferentsMoteurs;
begin
  // Test avec SQLite
  ConfigurerSQLite;
  ExecuterTests;

  // Test avec PostgreSQL
  ConfigurerPostgreSQL;
  ExecuterTests;

  // Test avec SQL Server
  ConfigurerSQLServer;
  ExecuterTests;
end;
```

### 2. Utiliser un gestionnaire de configuration

```pascal
type
  TDatabaseConfig = record
    MoteurBDD: string;  // 'SQLite', 'PostgreSQL', 'SQLServer', 'MySQL'
    Serveur: string;
    Port: Integer;
    NomBase: string;
    Utilisateur: string;
    MotDePasse: string;
  end;

procedure ConfigurerDepuisConfig(const Config: TDatabaseConfig);
begin
  case Config.MoteurBDD of
    'SQLite': ConfigurerSQLite(Config.NomBase);
    'PostgreSQL': ConfigurerPostgreSQL(Config);
    'SQLServer': ConfigurerSQLServer(Config);
    'MySQL': ConfigurerMySQL(Config);
  end;
end;
```

### 3. Documentation des différences

Documentez les particularités si vous supportez plusieurs moteurs :

```
README_DATABASE.md
==================

Moteurs supportés :
- SQLite (développement, application desktop)
- PostgreSQL (production Linux)
- SQL Server (production Windows)

Différences importantes :
- Auto-incrément : voir fichier migrations/
- Fonctions dates : utiliser les macros FireDAC
- Procédures stockées : uniquement PostgreSQL et SQL Server
```

## Ressources et outils

### Outils d'administration

| Base de données | Outils recommandés |
|-----------------|-------------------|
| **SQLite** | DB Browser for SQLite, SQLiteStudio |
| **PostgreSQL** | pgAdmin, DBeaver, DataGrip |
| **SQL Server** | SQL Server Management Studio (SSMS) |
| **MySQL** | MySQL Workbench, HeidiSQL, phpMyAdmin |

### Drivers et bibliothèques

FireDAC inclut les drivers pour tous ces moteurs. Mais vous pourriez avoir besoin de bibliothèques clientes :

- **SQLite** : Aucune (intégrée)
- **PostgreSQL** : libpq.dll
- **SQL Server** : ODBC ou OLE DB (Windows)
- **MySQL** : libmysql.dll ou libmariadb.dll

## Résumé

### Points clés

✅ **SQLite** : parfait pour applications desktop et mobiles
✅ **PostgreSQL** : excellent pour applications d'entreprise complexes
✅ **SQL Server** : idéal pour environnements Microsoft
✅ **FireDAC** : supporte tous ces moteurs avec la même API
✅ **Portabilité** : possible avec quelques adaptations

### Critères de choix

```
Application desktop mono-poste
  → SQLite

Application web ou multi-utilisateurs, open-source
  → PostgreSQL ou MySQL/MariaDB

Environnement Microsoft, budget disponible
  → SQL Server

Besoin de simplicité, popularité web
  → MySQL/MariaDB
```

### Tableau de décision

| Si vous avez besoin de... | Choisissez... |
|---------------------------|---------------|
| Base embarquée | SQLite |
| Conformité SQL stricte | PostgreSQL |
| Intégration Microsoft | SQL Server |
| Simplicité et popularité | MySQL/MariaDB |
| JSON natif | PostgreSQL ou SQL Server |
| Pas de serveur | SQLite |
| Support commercial | SQL Server |
| Performance lectures complexes | PostgreSQL |
| Performance écritures massives | MySQL/MariaDB |
| Budget zéro | SQLite, PostgreSQL, MySQL/MariaDB |

## Conclusion

Chaque moteur de base de données a ses forces et ses cas d'usage optimaux. **FireDAC** vous offre la flexibilité d'utiliser le moteur le plus adapté à votre projet, tout en conservant une API cohérente dans votre code Delphi.

- Pour débuter, **SQLite** est parfait : aucune configuration, simple, efficace
- Pour des applications professionnelles, **PostgreSQL** ou **MySQL/MariaDB** sont d'excellents choix
- Pour l'écosystème Microsoft, **SQL Server** s'intègre parfaitement

L'important est de choisir le moteur adapté à vos besoins, et FireDAC facilite grandement la tâche en vous permettant de changer de moteur avec un minimum de modifications de code !

⏭️ [NoSQL et bases de données documentaires](/08-acces-aux-bases-de-donnees-mysql-mariadb/13-nosql-et-bases-de-donnees-documentaires.md)
