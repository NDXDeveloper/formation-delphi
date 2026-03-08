🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.10 Migration et synchronisation de bases de données

## Introduction

Votre application évolue : vous ajoutez de nouvelles fonctionnalités, corrigez des bugs, améliorez les performances. Souvent, ces évolutions nécessitent des **modifications de la structure de votre base de données** : ajouter une colonne, créer une nouvelle table, modifier un index, etc.

La **migration de base de données** est le processus qui permet de faire évoluer votre schéma de base de données de manière contrôlée et reproductible. La **synchronisation** permet de s'assurer que tous les environnements (développement, test, production) sont à jour.

Dans ce chapitre, nous allons voir comment gérer professionnellement l'évolution de votre base de données.

## Qu'est-ce qu'une migration ?

### Définition

Une **migration** est une modification de la structure de la base de données :

```
Version 1.0                    Version 1.1
┌──────────────┐              ┌──────────────┐
│  clients     │              │  clients     │
│  - id        │   MIGRATION  │  - id        │
│  - nom       │   ═══════►   │  - nom       │
│  - email     │              │  - email     │
└──────────────┘              │  - telephone │ ← Nouveau champ
                              └──────────────┘
```

### Types de migrations

| Type | Description | Exemple |
|------|-------------|---------|
| **Additive** | Ajoute quelque chose | Nouvelle colonne, nouvelle table |
| **Destructive** | Supprime quelque chose | Suppression de colonne |
| **Transformative** | Modifie quelque chose | Renommage, changement de type |
| **Data** | Modifie les données | Mise à jour des valeurs |

## Pourquoi gérer les migrations ?

### Le problème sans gestion

Imaginez cette situation :

```
Développeur A : "J'ai ajouté la colonne 'telephone' dans ma base"  
Développeur B : "Ça ne marche pas chez moi, je n'ai pas cette colonne"  
Production : "L'application plante, il manque des colonnes !"  
```

**Problèmes :**
- ❌ Chaque environnement a une structure différente
- ❌ Impossible de savoir quelle est la version de la base
- ❌ Erreurs imprévisibles en production
- ❌ Perte de temps à synchroniser manuellement

### La solution : Migrations versionnées

```
Version de la base : 1.0 → 1.1 → 1.2 → 1.3
                     │      │      │      │
                     │      │      │      └─ Migration 003_ajout_table_commandes.sql
                     │      │      └──────── Migration 002_index_email.sql
                     │      └─────────────── Migration 001_ajout_telephone.sql
                     └────────────────────── Base initiale
```

**Avantages :**
- ✅ Historique complet des modifications
- ✅ Reproductible sur tous les environnements
- ✅ Retour en arrière possible (rollback)
- ✅ Collaboration facilitée

## Versionner sa base de données

### Principe du versioning

Chaque modification de la base reçoit un **numéro de version** unique et incrémental.

```
┌────────────────────────────────────┐
│  Table : schema_version            │
├────────────────────────────────────┤
│  version  │  nom_migration         │
│  1        │  initial_schema        │
│  2        │  ajout_telephone       │
│  3        │  ajout_table_commandes │
│  4        │  index_email           │
└────────────────────────────────────┘
```

### Créer la table de version

```sql
-- À exécuter une seule fois lors de la mise en place du système
CREATE TABLE IF NOT EXISTS schema_version (
    version INT PRIMARY KEY,
    nom_migration VARCHAR(100) NOT NULL,
    description TEXT,
    date_application DATETIME NOT NULL,
    duree_execution INT,  -- en millisecondes
    applique_par VARCHAR(100)
);
```

## Créer des scripts de migration

### Convention de nommage

Adoptez une convention claire pour nommer vos fichiers de migration :

```
migrations/
  001_initial_schema.sql
  002_ajout_telephone_clients.sql
  003_ajout_table_commandes.sql
  004_index_email_clients.sql
  005_ajout_statut_commandes.sql
```

**Format recommandé :** `NNN_description_courte.sql`
- `NNN` : Numéro séquentiel (001, 002, 003...)
- `description_courte` : Ce que fait la migration

### Structure d'un script de migration

```sql
-- ============================================
-- Migration : 002_ajout_telephone_clients.sql
-- Description : Ajoute le champ telephone à la table clients
-- Date : 2024-03-15
-- Auteur : Jean Dupont
-- ============================================

-- Vérification : ne pas exécuter si déjà appliqué
-- (Cette vérification sera gérée par le code Delphi)

-- Migration UP (appliquer)
ALTER TABLE clients  
ADD COLUMN telephone VARCHAR(20) AFTER email;  

-- Mettre à jour la version
INSERT INTO schema_version (version, nom_migration, description, date_application, applique_par)  
VALUES (2, '002_ajout_telephone_clients', 'Ajout du champ telephone', NOW(), 'migration_auto');  

-- ============================================
-- Migration DOWN (annuler - optionnel)
-- ============================================
-- ALTER TABLE clients DROP COLUMN telephone;
-- DELETE FROM schema_version WHERE version = 2;
```

### Types de migrations courantes

#### 1. Ajouter une colonne

```sql
-- Migration 002 : Ajouter telephone
ALTER TABLE clients  
ADD COLUMN telephone VARCHAR(20);  
```

#### 2. Modifier une colonne

```sql
-- Migration 003 : Agrandir le champ email
ALTER TABLE clients  
MODIFY COLUMN email VARCHAR(200);  
```

#### 3. Créer une table

```sql
-- Migration 004 : Créer table commandes
CREATE TABLE commandes (
    id INT AUTO_INCREMENT PRIMARY KEY,
    client_id INT NOT NULL,
    date_commande DATETIME NOT NULL,
    total DECIMAL(10,2) NOT NULL,
    statut ENUM('En cours', 'Validée', 'Livrée', 'Annulée') DEFAULT 'En cours',
    FOREIGN KEY (client_id) REFERENCES clients(id),
    INDEX idx_date_commande (date_commande),
    INDEX idx_client_statut (client_id, statut)
);
```

#### 4. Ajouter un index

```sql
-- Migration 005 : Index sur email pour recherches
CREATE INDEX idx_email ON clients(email);
```

#### 5. Migration de données

```sql
-- Migration 006 : Normaliser les emails en minuscules
UPDATE clients  
SET email = LOWER(email)  
WHERE email IS NOT NULL;  
```

## Implémenter les migrations dans Delphi

### Classe de gestion des migrations

```pascal
unit uMigrationManager;

interface

uses
  System.SysUtils, System.Classes, System.IOUtils,
  FireDAC.Comp.Client;

type
  TMigration = record
    Version: Integer;
    NomFichier: string;
    Description: string;
  end;

  TMigrationManager = class
  private
    FConnection: TFDConnection;
    FCheminMigrations: string;

    function ObtenirVersionActuelle: Integer;
    function ListerMigrations: TArray<TMigration>;
    function ChargerScriptMigration(const NomFichier: string): string;
    procedure AppliquerMigration(const Migration: TMigration);
    procedure EnregistrerMigration(const Migration: TMigration; DureeMs: Integer);
  public
    constructor Create(AConnection: TFDConnection; const ACheminMigrations: string);

    procedure VerifierEtCreerTableVersion;
    function NecessiteMigrations: Boolean;
    procedure MigrerVersVersion(VersionCible: Integer);
    procedure MigrerVersDerniere;
  end;

implementation

{ TMigrationManager }

constructor TMigrationManager.Create(AConnection: TFDConnection;
  const ACheminMigrations: string);
begin
  inherited Create;
  FConnection := AConnection;
  FCheminMigrations := ACheminMigrations;
end;

procedure TMigrationManager.VerifierEtCreerTableVersion;  
var  
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Créer la table si elle n'existe pas
    Query.SQL.Text :=
      'CREATE TABLE IF NOT EXISTS schema_version ( ' +
      '  version INT PRIMARY KEY, ' +
      '  nom_migration VARCHAR(100) NOT NULL, ' +
      '  description TEXT, ' +
      '  date_application DATETIME NOT NULL, ' +
      '  duree_execution INT, ' +
      '  applique_par VARCHAR(100) ' +
      ')';
    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

function TMigrationManager.ObtenirVersionActuelle: Integer;  
var  
  Query: TFDQuery;
begin
  Result := 0;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT MAX(version) AS version_max FROM schema_version';
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('version_max').AsInteger;

  finally
    Query.Free;
  end;
end;

function TMigrationManager.ListerMigrations: TArray<TMigration>;  
var  
  Fichiers: TStringDynArray;
  Chemin, NomFichier, Numero, Description: string;
  Migration: TMigration;
  Liste: TList<TMigration>;
begin
  Liste := TList<TMigration>.Create;
  try
    // Lister tous les fichiers .sql dans le dossier migrations
    Fichiers := TDirectory.GetFiles(FCheminMigrations, '*.sql');

    for Chemin in Fichiers do
    begin
      // Extraire le numéro et la description du nom de fichier
      // Format attendu : 001_description.sql
      // Note : la variable de boucle for..in est en lecture seule en Delphi
      NomFichier := ExtractFileName(Chemin);

      if Length(NomFichier) > 4 then
      begin
        Numero := Copy(NomFichier, 1, 3);
        Description := Copy(NomFichier, 5, Length(NomFichier) - 8); // Enlever .sql

        Migration.Version := StrToIntDef(Numero, 0);
        Migration.NomFichier := NomFichier;
        Migration.Description := StringReplace(Description, '_', ' ', [rfReplaceAll]);

        if Migration.Version > 0 then
          Liste.Add(Migration);
      end;
    end;

    // Trier par version
    Liste.Sort(TComparer<TMigration>.Construct(
      function(const A, B: TMigration): Integer
      begin
        Result := A.Version - B.Version;
      end
    ));

    Result := Liste.ToArray;

  finally
    Liste.Free;
  end;
end;

function TMigrationManager.ChargerScriptMigration(const NomFichier: string): string;  
var  
  CheminComplet: string;
begin
  CheminComplet := TPath.Combine(FCheminMigrations, NomFichier);
  Result := TFile.ReadAllText(CheminComplet, TEncoding.UTF8);
end;

procedure TMigrationManager.AppliquerMigration(const Migration: TMigration);  
var  
  Script: string;
  Query: TFDQuery;
  Debut: TDateTime;
  Duree: Integer;
begin
  // Charger le script
  Script := ChargerScriptMigration(Migration.NomFichier);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Mesurer le temps d'exécution
    Debut := Now;

    // Exécuter le script
    // Note : Pour des scripts complexes avec plusieurs commandes,
    // il faudrait les séparer et les exécuter une par une
    Query.SQL.Text := Script;
    Query.ExecSQL;

    // Calculer la durée
    Duree := MilliSecondsBetween(Now, Debut);

    // Enregistrer dans la table de version
    EnregistrerMigration(Migration, Duree);

  finally
    Query.Free;
  end;
end;

procedure TMigrationManager.EnregistrerMigration(const Migration: TMigration;
  DureeMs: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text :=
      'INSERT INTO schema_version (version, nom_migration, description, ' +
      '  date_application, duree_execution, applique_par) ' +
      'VALUES (:version, :nom, :desc, NOW(), :duree, :par)';

    Query.ParamByName('version').AsInteger := Migration.Version;
    Query.ParamByName('nom').AsString := Migration.NomFichier;
    Query.ParamByName('desc').AsString := Migration.Description;
    Query.ParamByName('duree').AsInteger := DureeMs;
    Query.ParamByName('par').AsString := 'Delphi Migration Manager';

    Query.ExecSQL;

  finally
    Query.Free;
  end;
end;

function TMigrationManager.NecessiteMigrations: Boolean;  
var  
  VersionActuelle: Integer;
  Migrations: TArray<TMigration>;
begin
  VersionActuelle := ObtenirVersionActuelle;
  Migrations := ListerMigrations;

  Result := (Length(Migrations) > 0) and
            (Migrations[High(Migrations)].Version > VersionActuelle);
end;

procedure TMigrationManager.MigrerVersVersion(VersionCible: Integer);  
var  
  VersionActuelle: Integer;
  Migrations: TArray<TMigration>;
  Migration: TMigration;
begin
  VersionActuelle := ObtenirVersionActuelle;

  if VersionCible <= VersionActuelle then
    Exit;  // Déjà à jour

  Migrations := ListerMigrations;

  FConnection.StartTransaction;
  try
    for Migration in Migrations do
    begin
      if (Migration.Version > VersionActuelle) and
         (Migration.Version <= VersionCible) then
      begin
        AppliquerMigration(Migration);
      end;
    end;

    FConnection.Commit;

  except
    FConnection.Rollback;
    raise;
  end;
end;

procedure TMigrationManager.MigrerVersDerniere;  
var  
  Migrations: TArray<TMigration>;
begin
  Migrations := ListerMigrations;

  if Length(Migrations) > 0 then
    MigrerVersVersion(Migrations[High(Migrations)].Version);
end;

end.
```

### Utiliser le gestionnaire de migrations

```pascal
unit uFormMain;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, uDmDatabase, uMigrationManager;

type
  TFormMain = class(TForm)
    btnVerifierMigrations: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnVerifierMigrationsClick(Sender: TObject);
  private
    FMigrationManager: TMigrationManager;
  end;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);  
var  
  CheminMigrations: string;
begin
  // Chemin vers le dossier migrations
  CheminMigrations := ExtractFilePath(Application.ExeName) + 'migrations\';

  // Créer le gestionnaire
  FMigrationManager := TMigrationManager.Create(
    dmDatabase.FDConnection1,
    CheminMigrations
  );

  // Vérifier/créer la table de version
  FMigrationManager.VerifierEtCreerTableVersion;

  // Vérifier s'il y a des migrations à appliquer
  if FMigrationManager.NecessiteMigrations then
  begin
    if MessageDlg('Des migrations de base de données sont nécessaires. ' +
                  'Voulez-vous les appliquer maintenant ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      try
        FMigrationManager.MigrerVersDerniere;
        ShowMessage('Migrations appliquées avec succès');
      except
        on E: Exception do
          ShowMessage('Erreur lors des migrations : ' + E.Message);
      end;
    end;
  end;
end;

procedure TFormMain.btnVerifierMigrationsClick(Sender: TObject);  
var  
  Query: TFDQuery;
begin
  Memo1.Clear;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := dmDatabase.FDConnection1;
    Query.SQL.Text :=
      'SELECT version, nom_migration, description, date_application ' +
      'FROM schema_version ORDER BY version';
    Query.Open;

    Memo1.Lines.Add('═══════════════════════════════════');
    Memo1.Lines.Add('    HISTORIQUE DES MIGRATIONS');
    Memo1.Lines.Add('═══════════════════════════════════');
    Memo1.Lines.Add('');

    while not Query.Eof do
    begin
      Memo1.Lines.Add(Format('Version %d : %s', [
        Query.FieldByName('version').AsInteger,
        Query.FieldByName('nom_migration').AsString
      ]));
      Memo1.Lines.Add('  ' + Query.FieldByName('description').AsString);
      Memo1.Lines.Add('  Appliquée le : ' +
        Query.FieldByName('date_application').AsString);
      Memo1.Lines.Add('');

      Query.Next;
    end;

  finally
    Query.Free;
  end;
end;

end.
```

## Migration au démarrage de l'application

### Vérification automatique

```pascal
program MonProjet;

uses
  Vcl.Forms,
  System.SysUtils,
  uDmDatabase,
  uMigrationManager,
  uFormMain;

{$R *.res}

var
  MigrationManager: TMigrationManager;
  CheminMigrations: string;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Créer le DataModule
  Application.CreateForm(TdmDatabase, dmDatabase);

  try
    // Initialiser les migrations
    CheminMigrations := ExtractFilePath(Application.ExeName) + 'migrations\';
    MigrationManager := TMigrationManager.Create(
      dmDatabase.FDConnection1,
      CheminMigrations
    );
    try
      // Créer la table de version si nécessaire
      MigrationManager.VerifierEtCreerTableVersion;

      // Appliquer automatiquement les migrations au démarrage
      if MigrationManager.NecessiteMigrations then
      begin
        MigrationManager.MigrerVersDerniere;
        ShowMessage('Base de données mise à jour avec succès');
      end;

    finally
      MigrationManager.Free;
    end;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de la migration : ' + E.Message);
      Application.Terminate;
      Exit;
    end;
  end;

  // Créer le formulaire principal
  Application.CreateForm(TFormMain, FormMain);

  Application.Run;
end.
```

## Synchronisation entre environnements

### Les environnements typiques

```
┌────────────────┐       ┌────────────────┐       ┌────────────────┐
│ DÉVELOPPEMENT  │       │      TEST      │       │   PRODUCTION   │
│                │       │                │       │                │
│ Version : 15   │  ───► │ Version : 12   │  ───► │ Version : 10   │
│ (en avance)    │       │ (à synchroniser)       │ (stable)       │
└────────────────┘       └────────────────┘       └────────────────┘
```

### Processus de synchronisation

1. **Développement** : Créer les migrations
2. **Test** : Appliquer et valider
3. **Production** : Déployer en production

### Script de synchronisation

```sql
-- sync_database.sql
-- Script pour synchroniser un environnement

-- 1. Sauvegarder d'abord !
-- mysqldump -u user -p ma_base > backup_avant_sync.sql

-- 2. Vérifier la version actuelle
SELECT MAX(version) AS version_actuelle FROM schema_version;

-- 3. Appliquer les migrations manquantes
-- (Exécuter chaque migration dans l'ordre)

SOURCE migrations/011_nouvelle_migration.sql;  
SOURCE migrations/012_autre_migration.sql;  
-- etc.

-- 4. Vérifier la nouvelle version
SELECT MAX(version) AS version_finale FROM schema_version;
```

## Gestion des données de référence

### Seed Data (données initiales)

Certaines migrations doivent inclure des **données de référence** :

```sql
-- Migration 010 : Ajouter les statuts de commande
CREATE TABLE statuts_commande (
    id INT PRIMARY KEY,
    code VARCHAR(20) UNIQUE NOT NULL,
    libelle VARCHAR(100) NOT NULL,
    ordre_affichage INT
);

-- Insérer les données de référence
INSERT INTO statuts_commande (id, code, libelle, ordre_affichage) VALUES
(1, 'EN_ATTENTE', 'En attente de validation', 1),
(2, 'VALIDEE', 'Validée', 2),
(3, 'EN_PREPARATION', 'En préparation', 3),
(4, 'EXPEDIE', 'Expédiée', 4),
(5, 'LIVREE', 'Livrée', 5),
(6, 'ANNULEE', 'Annulée', 99);
```

### Mise à jour des données existantes

```sql
-- Migration 011 : Normaliser les emails
UPDATE clients  
SET email = LOWER(TRIM(email))  
WHERE email IS NOT NULL;  

-- Vérification
SELECT COUNT(*) AS emails_invalides  
FROM clients  
WHERE email NOT LIKE '%@%.%';  
```

## Rollback (retour en arrière)

### Principe

Un **rollback** annule une migration et revient à la version précédente.

### Migration réversible

```sql
-- ============================================
-- Migration 012_ajout_colonne_notes.sql
-- ============================================

-- UP : Appliquer la migration
ALTER TABLE clients ADD COLUMN notes TEXT;

INSERT INTO schema_version (version, nom_migration, description, date_application)  
VALUES (12, '012_ajout_colonne_notes', 'Ajout colonne notes', NOW());  

-- ============================================
-- DOWN : Annuler la migration (rollback)
-- À exécuter manuellement si nécessaire
-- ============================================
/*
ALTER TABLE clients DROP COLUMN notes;  
DELETE FROM schema_version WHERE version = 12;  
*/
```

### Implémentation du rollback

```pascal
procedure TMigrationManager.RollbackVersVersion(VersionCible: Integer);  
var  
  VersionActuelle: Integer;
  Query: TFDQuery;
  Migrations: TArray<TMigration>;
  i: Integer;
begin
  VersionActuelle := ObtenirVersionActuelle;

  if VersionCible >= VersionActuelle then
    Exit;  // Pas de rollback nécessaire

  // ATTENTION : Le rollback doit être fait MANUELLEMENT
  // car il nécessite des scripts DOWN spécifiques
  raise Exception.Create(
    'Le rollback automatique n''est pas implémenté. ' +
    'Veuillez exécuter manuellement les scripts DOWN.'
  );
end;
```

**Note :** Le rollback automatique est complexe et risqué. Dans la pratique, on préfère souvent créer une **nouvelle migration corrective** plutôt que de revenir en arrière.

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours sauvegarder avant une migration**
   ```bash
   mysqldump -u user -p ma_base > backup_avant_v12.sql
   ```

2. **Tester d'abord en développement**
   ```
   DEV → TEST → PRODUCTION
   Jamais directement en production !
   ```

3. **Migrations incrémentielles**
   ```
   ✅ Migration 001 : Ajouter colonne
   ✅ Migration 002 : Ajouter index

   ❌ Une seule migration géante avec tout
   ```

4. **Documenter chaque migration**
   ```sql
   -- Description : Pourquoi cette migration ?
   -- Impact : Quelles tables sont affectées ?
   -- Durée estimée : ~30 secondes
   ```

5. **Utiliser des transactions**
   ```sql
   START TRANSACTION;
   -- Vos modifications
   COMMIT;
   ```

6. **Vérifier les contraintes**
   ```sql
   -- Avant d'ajouter une FK, vérifier l'intégrité
   SELECT COUNT(*) FROM commandes
   WHERE client_id NOT IN (SELECT id FROM clients);
   ```

7. **Versioning dans Git**
   ```
   git add migrations/012_nouvelle_migration.sql
   git commit -m "Migration 012: Ajout colonne notes"
   ```

### ❌ À ÉVITER

1. **Modifier une migration déjà appliquée**
   ```
   ❌ Éditer 005_ancienne_migration.sql après l'avoir appliquée
   ✅ Créer 013_correction_migration_005.sql
   ```

2. **Migrations non testées**
   ```
   ❌ Écrire et appliquer directement en production
   ✅ Tester en dev, puis test, puis production
   ```

3. **Suppressions sans vérification**
   ```sql
   -- ❌ Dangereux
   DROP TABLE old_table;

   -- ✅ Vérifié
   DROP TABLE IF EXISTS old_table;
   ```

4. **Migrations manuelles oubliées**
   ```
   ❌ Modifier la base directement sans créer de script
   ✅ Toujours passer par un script de migration
   ```

5. **Rollback automatique en production**
   ```
   ❌ Tenter de rollback automatiquement
   ✅ Créer une migration corrective
   ```

## Outils tiers pour les migrations

### 1. Liquibase

Outil Java open-source très puissant.

```xml
<!-- changeLog.xml -->
<databaseChangeLog>
  <changeSet id="1" author="jean">
    <createTable tableName="clients">
      <column name="id" type="int" autoIncrement="true">
        <constraints primaryKey="true"/>
      </column>
      <column name="nom" type="varchar(100)"/>
    </createTable>
  </changeSet>
</databaseChangeLog>
```

### 2. Flyway

Outil similaire, plus simple.

```
V1__Create_clients_table.sql  
V2__Add_telephone_column.sql  
V3__Create_commandes_table.sql  
```

### 3. DBDelta (pour Delphi)

Composant commercial pour Delphi qui gère les migrations automatiquement.

### 4. TMS XData Schema Manager

Pour les applications TMS XData (REST/ORM).

## Cas particuliers

### Migration avec données volumineuses

Pour des tables avec beaucoup de données :

```sql
-- Migration progressive par lots
SET @batch_size = 1000;  
SET @offset = 0;  

REPEAT
  UPDATE clients
  SET email = LOWER(email)
  WHERE id >= @offset AND id < @offset + @batch_size;

  SET @offset = @offset + @batch_size;

  -- Pause pour ne pas saturer
  SELECT SLEEP(0.1);

UNTIL @offset > (SELECT MAX(id) FROM clients)  
END REPEAT;  
```

### Migration avec downtime minimal

```sql
-- 1. Créer la nouvelle colonne
ALTER TABLE clients ADD COLUMN email_v2 VARCHAR(200);

-- 2. Copier les données progressivement (en arrière-plan)
UPDATE clients SET email_v2 = email WHERE id BETWEEN 1 AND 10000;
-- Continuer par lots...

-- 3. Une fois terminé, renommer
ALTER TABLE clients DROP COLUMN email;  
ALTER TABLE clients CHANGE COLUMN email_v2 email VARCHAR(200);  
```

### Migrations multi-bases

Si votre application utilise plusieurs bases :

```pascal
procedure TFormMain.MigrerToutesLesBases;  
var  
  MigrationPrincipale: TMigrationManager;
  MigrationStats: TMigrationManager;
begin
  // Base principale
  MigrationPrincipale := TMigrationManager.Create(
    dmDatabase.FDConnectionPrincipale,
    'migrations\principale\'
  );
  try
    MigrationPrincipale.MigrerVersDerniere;
  finally
    MigrationPrincipale.Free;
  end;

  // Base de statistiques
  MigrationStats := TMigrationManager.Create(
    dmDatabase.FDConnectionStats,
    'migrations\stats\'
  );
  try
    MigrationStats.MigrerVersDerniere;
  finally
    MigrationStats.Free;
  end;
end;
```

## Checklist de déploiement

### Avant le déploiement en production

- [ ] Sauvegarde complète de la base
- [ ] Migrations testées en environnement de test
- [ ] Scripts de rollback préparés (si nécessaire)
- [ ] Équipe technique prévenue
- [ ] Fenêtre de maintenance planifiée
- [ ] Monitoring activé
- [ ] Plan B en cas d'échec

### Pendant le déploiement

- [ ] Stopper l'application
- [ ] Appliquer les migrations
- [ ] Vérifier les logs
- [ ] Tester les fonctionnalités critiques
- [ ] Redémarrer l'application

### Après le déploiement

- [ ] Vérifier que tout fonctionne
- [ ] Surveiller les logs d'erreur
- [ ] Valider avec les utilisateurs
- [ ] Documenter les changements

## Exemple complet de workflow

### 1. Développement

```bash
# Créer une nouvelle migration
cd migrations  
touch 015_ajout_table_categories.sql  

# Éditer le fichier
nano 015_ajout_table_categories.sql
```

```sql
-- Contenu de 015_ajout_table_categories.sql
CREATE TABLE categories (
    id INT AUTO_INCREMENT PRIMARY KEY,
    nom VARCHAR(100) NOT NULL,
    description TEXT,
    actif BOOLEAN DEFAULT TRUE
);

INSERT INTO schema_version (version, nom_migration, description, date_application)  
VALUES (15, '015_ajout_table_categories', 'Création table categories', NOW());  
```

### 2. Test local

```pascal
// Dans Delphi
FMigrationManager.MigrerVersDerniere;
// Tester l'application
```

### 3. Commit dans Git

```bash
git add migrations/015_ajout_table_categories.sql  
git commit -m "Migration 015: Ajout table categories"  
git push  
```

### 4. Déploiement en test

```bash
# Sur le serveur de test
git pull
./appliquer_migrations.sh
# Tester
```

### 5. Déploiement en production

```bash
# Sauvegarde
mysqldump -u prod -p prod_db > backup_$(date +%Y%m%d_%H%M%S).sql

# Appliquer
mysql -u prod -p prod_db < migrations/015_ajout_table_categories.sql

# Vérifier
mysql -u prod -p prod_db -e "SELECT MAX(version) FROM schema_version;"
```

## Résumé

### Points clés

✅ **Versionner** toujours les modifications de base de données  
✅ **Table schema_version** pour tracker les migrations  
✅ **Scripts SQL** numérotés et documentés  
✅ **Tester** en dev avant de déployer  
✅ **Sauvegarder** avant chaque migration en production  
✅ **Automatiser** le processus avec du code Delphi  
✅ **Documenter** chaque migration

### Workflow recommandé

```
1. Créer script de migration → migrations/NNN_description.sql
2. Tester en développement → Appliquer et valider
3. Commit dans Git → git commit
4. Tester en environnement de test → Validation
5. Déployer en production → Avec sauvegarde
6. Vérifier le fonctionnement → Monitoring
```

### Architecture des migrations

```
MonProjet/
├── migrations/
│   ├── 001_initial_schema.sql
│   ├── 002_ajout_telephone.sql
│   ├── 003_ajout_table_commandes.sql
│   └── README.md (documentation)
│
├── code/
│   └── uMigrationManager.pas
│
└── docs/
    └── MIGRATIONS.md (guide)
```

## Prochaines étapes

Vous maîtrisez maintenant la gestion des migrations de base de données ! Dans les sections suivantes, nous verrons :

1. **Sécurisation des accès** et prévention des injections SQL
2. **Optimisation** : indexes, requêtes performantes
3. **Autres moteurs** de bases de données
4. **NoSQL** et bases documentaires

Avec un système de migration bien en place, vous pouvez faire évoluer votre application en toute confiance, sachant que la structure de votre base de données est toujours cohérente et reproductible !

⏭️ [Sécurisation des accès et prévention des injections SQL](/08-acces-aux-bases-de-donnees-mysql-mariadb/11-securisation-des-acces-et-prevention-des-injections-sql.md)
