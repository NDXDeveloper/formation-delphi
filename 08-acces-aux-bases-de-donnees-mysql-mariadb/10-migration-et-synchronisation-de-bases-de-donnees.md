# 8.10 Migration et synchronisation de bases de données

Une application de gestion évolue constamment au fil du temps. De nouvelles fonctionnalités sont ajoutées, des corrections sont apportées, et cela entraîne souvent des modifications dans la structure de la base de données. Comment gérer ces changements efficacement, surtout quand votre application est déjà déployée chez des clients ? Cette section vous guidera à travers les concepts et techniques de migration et de synchronisation de bases de données MySQL avec Delphi.

## Comprendre les enjeux de la migration de bases de données

Lorsque vous modifiez la structure d'une base de données (ajout de tables, modification de champs, etc.), vous devez vous assurer que :

1. Les données existantes ne sont pas perdues
2. La structure de la base de données reste cohérente
3. L'application peut fonctionner avec la nouvelle structure
4. La transition est aussi transparente que possible pour l'utilisateur

## Types de changements dans une base de données

Avant de plonger dans les solutions, identifions les principaux types de modifications que vous pourriez apporter à votre base de données :

### 1. Ajouts (les plus simples)
- Nouvelles tables
- Nouveaux champs dans des tables existantes
- Nouveaux index ou contraintes

### 2. Modifications
- Changement de type d'un champ
- Modification de la taille d'un champ
- Renommage d'une table ou d'un champ

### 3. Suppressions (les plus risquées)
- Suppression de tables
- Suppression de champs
- Suppression d'index ou de contraintes

## Approches pour la migration de bases de données

### Approche 1 : Script SQL manuel

La méthode la plus simple consiste à créer un script SQL qui effectue les modifications nécessaires.

#### Exemple de script de migration

```sql
-- Script de migration v1.0 à v1.1
-- Date: 2023-04-15

-- 1. Ajouter une nouvelle table pour les catégories
CREATE TABLE IF NOT EXISTS categories (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(100) NOT NULL,
  description TEXT,
  date_creation DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- 2. Ajouter un champ dans la table clients
ALTER TABLE clients
ADD COLUMN notes TEXT AFTER adresse;

-- 3. Modifier la taille d'un champ existant
ALTER TABLE produits
MODIFY COLUMN reference VARCHAR(20) NOT NULL;

-- 4. Ajouter une clé étrangère
ALTER TABLE produits
ADD COLUMN categorie_id INT AFTER description,
ADD CONSTRAINT fk_produit_categorie
FOREIGN KEY (categorie_id) REFERENCES categories(id);
```

#### Avantages et inconvénients de l'approche manuelle

**Avantages :**
- Contrôle total sur les modifications
- Simple à comprendre et à exécuter
- Facile à versionner (ex. dans Git)

**Inconvénients :**
- Risque d'erreurs humaines
- Difficulté à gérer les cas complexes (migration de données)
- Pas d'intégration directe avec l'application

### Approche 2 : Système de versionnage de schéma

Cette approche plus avancée consiste à implémenter un système qui :
1. Garde une trace de la version actuelle du schéma dans la base de données
2. Applique automatiquement les scripts de migration nécessaires

#### Implémentation d'un système simple de versionnage

Tout d'abord, créez une table pour stocker la version actuelle de la base de données :

```sql
CREATE TABLE schema_version (
  version VARCHAR(20) NOT NULL,
  date_mise_a_jour DATETIME DEFAULT CURRENT_TIMESTAMP,
  description VARCHAR(255)
);

-- Initialiser avec la version de base
INSERT INTO schema_version (version, description)
VALUES ('1.0', 'Version initiale');
```

Ensuite, dans votre application Delphi, implémentez un mécanisme pour vérifier et mettre à jour la version :

```delphi
// UDatabaseMigration.pas
unit UDatabaseMigration;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, System.Generics.Collections;

type
  TMigration = class
  private
    FVersion: string;
    FDescription: string;
    FScriptSQL: TStringList;
  public
    constructor Create(const AVersion, ADescription: string);
    destructor Destroy; override;
    procedure AddSQL(const ASQL: string);
    property Version: string read FVersion;
    property Description: string read FDescription;
    property ScriptSQL: TStringList read FScriptSQL;
  end;

  TDatabaseMigration = class
  private
    FConnection: TFDConnection;
    FMigrations: TObjectList<TMigration>;
    FCurrentVersion: string;
    function GetCurrentVersion: string;
    procedure ExecuteMigration(AMigration: TMigration);
    procedure UpdateVersionInfo(const AVersion, ADescription: string);
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
    procedure RegisterMigration(const AVersion, ADescription: string; AMigrationProc: TProc<TMigration>);
    function MigrateToLatest: Boolean;
  end;

implementation

{ TMigration }

constructor TMigration.Create(const AVersion, ADescription: string);
begin
  inherited Create;
  FVersion := AVersion;
  FDescription := ADescription;
  FScriptSQL := TStringList.Create;
end;

destructor TMigration.Destroy;
begin
  FScriptSQL.Free;
  inherited;
end;

procedure TMigration.AddSQL(const ASQL: string);
begin
  FScriptSQL.Add(ASQL);
end;

{ TDatabaseMigration }

constructor TDatabaseMigration.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
  FMigrations := TObjectList<TMigration>.Create(True);  // True = owns objects
  FCurrentVersion := GetCurrentVersion;
end;

destructor TDatabaseMigration.Destroy;
begin
  FMigrations.Free;
  inherited;
end;

function TDatabaseMigration.GetCurrentVersion: string;
var
  Query: TFDQuery;
begin
  Result := '0.0';  // Version par défaut si la table n'existe pas encore

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Vérifier si la table schema_version existe
    try
      Query.SQL.Text := 'SELECT version FROM schema_version ORDER BY date_mise_a_jour DESC LIMIT 1';
      Query.Open;

      if not Query.IsEmpty then
        Result := Query.FieldByName('version').AsString;
    except
      // Si la table n'existe pas, créer la structure initiale
      on E: Exception do
      begin
        try
          Query.SQL.Text := 'CREATE TABLE schema_version (' +
                            'version VARCHAR(20) NOT NULL, ' +
                            'date_mise_a_jour DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
                            'description VARCHAR(255))';
          Query.ExecSQL;

          Query.SQL.Text := 'INSERT INTO schema_version (version, description) ' +
                            'VALUES (:version, :description)';
          Query.ParamByName('version').AsString := Result;
          Query.ParamByName('description').AsString := 'Version initiale';
          Query.ExecSQL;
        except
          // Ignorer les erreurs ici, on suppose que l'utilisateur a les droits nécessaires
        end;
      end;
    end;
  finally
    Query.Free;
  end;
end;

procedure TDatabaseMigration.RegisterMigration(const AVersion, ADescription: string;
  AMigrationProc: TProc<TMigration>);
var
  Migration: TMigration;
begin
  // Vérifier que la version n'est pas déjà enregistrée
  for Migration in FMigrations do
    if CompareText(Migration.Version, AVersion) = 0 then
      Exit;  // Migration déjà enregistrée

  // Créer la nouvelle migration
  Migration := TMigration.Create(AVersion, ADescription);
  FMigrations.Add(Migration);

  // Appeler la procédure de configuration de la migration
  if Assigned(AMigrationProc) then
    AMigrationProc(Migration);
end;

procedure TDatabaseMigration.ExecuteMigration(AMigration: TMigration);
var
  SQL: string;
  Transaction: TFDTransaction;
begin
  Transaction := TFDTransaction.Create(nil);
  try
    Transaction.Connection := FConnection;
    Transaction.StartTransaction;

    try
      // Exécuter chaque instruction SQL de la migration
      for SQL in AMigration.ScriptSQL do
        FConnection.ExecSQL(SQL);

      // Mettre à jour la table de version
      UpdateVersionInfo(AMigration.Version, AMigration.Description);

      Transaction.Commit;
    except
      on E: Exception do
      begin
        Transaction.Rollback;
        raise;  // Relancer l'exception pour la gestion en amont
      end;
    end;
  finally
    Transaction.Free;
  end;
end;

procedure TDatabaseMigration.UpdateVersionInfo(const AVersion, ADescription: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'INSERT INTO schema_version (version, description) ' +
                      'VALUES (:version, :description)';
    Query.ParamByName('version').AsString := AVersion;
    Query.ParamByName('description').AsString := ADescription;
    Query.ExecSQL;

    FCurrentVersion := AVersion;
  finally
    Query.Free;
  end;
end;

function TDatabaseMigration.MigrateToLatest: Boolean;
var
  i: Integer;
  Migration: TMigration;
begin
  Result := True;

  try
    // Trier les migrations par version (ce code suppose que les versions sont au format x.y et comparables)
    FMigrations.Sort(TComparer<TMigration>.Construct(
      function(const Left, Right: TMigration): Integer
      begin
        Result := CompareText(Left.Version, Right.Version);
      end));

    // Exécuter chaque migration dont la version est supérieure à la version actuelle
    for i := 0 to FMigrations.Count - 1 do
    begin
      Migration := FMigrations[i];
      if CompareText(Migration.Version, FCurrentVersion) > 0 then
        ExecuteMigration(Migration);
    end;
  except
    on E: Exception do
    begin
      Result := False;
    end;
  end;
end;

end.
```

#### Utilisation du système de migration

Voici comment utiliser ce système dans votre application :

```delphi
procedure TForm1.MigrerBaseDeDonnees;
var
  Migration: TDatabaseMigration;
begin
  Migration := TDatabaseMigration.Create(FDConnection1);
  try
    // Enregistrer les migrations

    // Migration vers la version 1.1
    Migration.RegisterMigration('1.1', 'Ajout de la table catégories',
      procedure(M: TMigration)
      begin
        M.AddSQL('CREATE TABLE IF NOT EXISTS categories (' +
                 'id INT AUTO_INCREMENT PRIMARY KEY, ' +
                 'nom VARCHAR(100) NOT NULL, ' +
                 'description TEXT, ' +
                 'date_creation DATETIME DEFAULT CURRENT_TIMESTAMP)');
      end);

    // Migration vers la version 1.2
    Migration.RegisterMigration('1.2', 'Ajout du champ notes dans clients',
      procedure(M: TMigration)
      begin
        M.AddSQL('ALTER TABLE clients ADD COLUMN notes TEXT AFTER adresse');
      end);

    // Migration vers la version 1.3
    Migration.RegisterMigration('1.3', 'Relation produits-catégories',
      procedure(M: TMigration)
      begin
        M.AddSQL('ALTER TABLE produits ADD COLUMN categorie_id INT AFTER description');
        M.AddSQL('ALTER TABLE produits ADD CONSTRAINT fk_produit_categorie ' +
                 'FOREIGN KEY (categorie_id) REFERENCES categories(id)');
      end);

    // Exécuter les migrations nécessaires
    if Migration.MigrateToLatest then
      ShowMessage('Migration terminée avec succès')
    else
      ShowMessage('Erreur lors de la migration');
  finally
    Migration.Free;
  end;
end;
```

### Approche 3 : Outils tiers

Il existe plusieurs outils spécialisés pour la migration de bases de données MySQL :

1. **Flyway** - Outil Java populaire pour la migration de bases de données
2. **Liquibase** - Solution de gestion de schéma de base de données
3. **MySQL Workbench** - Outil officiel de MySQL avec fonctionnalités de synchronisation
4. **DBForge Studio** - IDE tiers pour MySQL avec outils de comparaison et synchronisation

Ces outils offrent des fonctionnalités avancées mais nécessitent une intégration séparée avec votre application Delphi.

## Synchronisation de bases de données

La synchronisation est un cas particulier de migration où l'on cherche à faire correspondre la structure d'une base de données avec une autre. Cela est particulièrement utile dans les scénarios suivants :

1. Synchronisation entre environnements (développement, test, production)
2. Mise à jour de bases de données clients
3. Réplication de structure pour des systèmes distribués

### Création d'un outil de synchronisation simple dans Delphi

Voici comment créer un outil qui compare et synchronise deux bases de données MySQL :

```delphi
// UDatabaseSync.pas
unit UDatabaseSync;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Comp.Client, System.Generics.Collections;

type
  TTableInfo = class
  public
    Name: string;
    Fields: TStringList;
    Indexes: TStringList;
    constructor Create;
    destructor Destroy; override;
  end;

  TDatabaseSync = class
  private
    FSourceConnection: TFDConnection;
    FTargetConnection: TFDConnection;
    FTables: TObjectDictionary<string, TTableInfo>;
    FTargetTables: TObjectDictionary<string, TTableInfo>;
    FSyncScript: TStringList;

    procedure CollectSourceSchema;
    procedure CollectTargetSchema;
    procedure CompareSchemas;
    function GetTableStructure(Connection: TFDConnection; const TableName: string): TTableInfo;
    function GetAllTables(Connection: TFDConnection): TStringList;
  public
    constructor Create(ASourceConnection, ATargetConnection: TFDConnection);
    destructor Destroy; override;
    procedure GenerateSyncScript;
    procedure ExecuteSyncScript;
    property SyncScript: TStringList read FSyncScript;
  end;

implementation

{ TTableInfo }

constructor TTableInfo.Create;
begin
  inherited;
  Fields := TStringList.Create;
  Indexes := TStringList.Create;
end;

destructor TTableInfo.Destroy;
begin
  Fields.Free;
  Indexes.Free;
  inherited;
end;

{ TDatabaseSync }

constructor TDatabaseSync.Create(ASourceConnection, ATargetConnection: TFDConnection);
begin
  inherited Create;
  FSourceConnection := ASourceConnection;
  FTargetConnection := ATargetConnection;
  FTables := TObjectDictionary<string, TTableInfo>.Create([doOwnsValues]);
  FTargetTables := TObjectDictionary<string, TTableInfo>.Create([doOwnsValues]);
  FSyncScript := TStringList.Create;
end;

destructor TDatabaseSync.Destroy;
begin
  FSyncScript.Free;
  FTables.Free;
  FTargetTables.Free;
  inherited;
end;

function TDatabaseSync.GetAllTables(Connection: TFDConnection): TStringList;
var
  Query: TFDQuery;
begin
  Result := TStringList.Create;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;
    Query.SQL.Text := 'SHOW TABLES';
    Query.Open;

    while not Query.Eof do
    begin
      Result.Add(Query.Fields[0].AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

function TDatabaseSync.GetTableStructure(Connection: TFDConnection; const TableName: string): TTableInfo;
var
  Query: TFDQuery;
begin
  Result := TTableInfo.Create;
  Result.Name := TableName;

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := Connection;

    // Récupérer la structure des champs
    Query.SQL.Text := 'DESCRIBE ' + TableName;
    Query.Open;

    while not Query.Eof do
    begin
      Result.Fields.Add(
        Format('%s %s %s', [
          Query.FieldByName('Field').AsString,
          Query.FieldByName('Type').AsString,
          IfThen(Query.FieldByName('Null').AsString = 'YES', 'NULL', 'NOT NULL')
        ])
      );
      Query.Next;
    end;

    // Récupérer les index
    Query.Close;
    Query.SQL.Text := 'SHOW INDEX FROM ' + TableName;
    Query.Open;

    while not Query.Eof do
    begin
      Result.Indexes.Add(
        Format('%s %s', [
          Query.FieldByName('Key_name').AsString,
          Query.FieldByName('Column_name').AsString
        ])
      );
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;

procedure TDatabaseSync.CollectSourceSchema;
var
  Tables: TStringList;
  TableName: string;
begin
  FTables.Clear;
  Tables := GetAllTables(FSourceConnection);
  try
    for TableName in Tables do
      FTables.Add(TableName, GetTableStructure(FSourceConnection, TableName));
  finally
    Tables.Free;
  end;
end;

procedure TDatabaseSync.CollectTargetSchema;
var
  Tables: TStringList;
  TableName: string;
begin
  FTargetTables.Clear;
  Tables := GetAllTables(FTargetConnection);
  try
    for TableName in Tables do
      FTargetTables.Add(TableName, GetTableStructure(FTargetConnection, TableName));
  finally
    Tables.Free;
  end;
end;

procedure TDatabaseSync.CompareSchemas;
var
  SourceTableName: string;
  SourceTable, TargetTable: TTableInfo;
  FieldExists: Boolean;
  i: Integer;
  FieldName, IndexName: string;
begin
  FSyncScript.Clear;
  FSyncScript.Add('-- Script de synchronisation généré le ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  FSyncScript.Add('');

  // Parcourir les tables sources
  for SourceTableName in FTables.Keys do
  begin
    SourceTable := FTables[SourceTableName];

    // Vérifier si la table existe dans la cible
    if not FTargetTables.ContainsKey(SourceTableName) then
    begin
      // Table à créer
      FSyncScript.Add(Format('-- Création de la table %s', [SourceTableName]));
      FSyncScript.Add('CREATE TABLE ' + SourceTableName + ' (');

      for i := 0 to SourceTable.Fields.Count - 1 do
      begin
        FSyncScript.Add('  ' + SourceTable.Fields[i] +
          IfThen(i < SourceTable.Fields.Count - 1, ',', ''));
      end;

      FSyncScript.Add(');');
      FSyncScript.Add('');
    end
    else
    begin
      // Table existe, comparer les champs
      TargetTable := FTargetTables[SourceTableName];

      for i := 0 to SourceTable.Fields.Count - 1 do
      begin
        FieldName := SourceTable.Fields[i].Split([' '])[0];
        FieldExists := False;

        // Vérifier si le champ existe
        for var j := 0 to TargetTable.Fields.Count - 1 do
        begin
          if TargetTable.Fields[j].StartsWith(FieldName + ' ') then
          begin
            FieldExists := True;

            // Vérifier si la définition est différente
            if SourceTable.Fields[i] <> TargetTable.Fields[j] then
            begin
              FSyncScript.Add(Format('-- Modification du champ %s dans la table %s',
                [FieldName, SourceTableName]));
              FSyncScript.Add(Format('ALTER TABLE %s MODIFY COLUMN %s;',
                [SourceTableName, SourceTable.Fields[i]]));
              FSyncScript.Add('');
            end;

            Break;
          end;
        end;

        // Champ manquant à ajouter
        if not FieldExists then
        begin
          FSyncScript.Add(Format('-- Ajout du champ %s dans la table %s',
            [FieldName, SourceTableName]));
          FSyncScript.Add(Format('ALTER TABLE %s ADD COLUMN %s;',
            [SourceTableName, SourceTable.Fields[i]]));
          FSyncScript.Add('');
        end;
      end;

      // Comparer les index
      for i := 0 to SourceTable.Indexes.Count - 1 do
      begin
        IndexName := SourceTable.Indexes[i].Split([' '])[0];

        if IndexName = 'PRIMARY' then
          Continue;  // Ne pas toucher à la clé primaire pour éviter les erreurs

        FieldExists := False;

        // Vérifier si l'index existe
        for var j := 0 to TargetTable.Indexes.Count - 1 do
        begin
          if TargetTable.Indexes[j].StartsWith(IndexName + ' ') then
          begin
            FieldExists := True;
            Break;
          end;
        end;

        // Index manquant à ajouter
        if not FieldExists then
        begin
          FSyncScript.Add(Format('-- Ajout de l''index %s dans la table %s',
            [IndexName, SourceTableName]));
          FSyncScript.Add(Format('CREATE INDEX %s ON %s(%s);',
            [IndexName, SourceTableName, SourceTable.Indexes[i].Split([' '])[1]]));
          FSyncScript.Add('');
        end;
      end;
    end;
  end;
end;

procedure TDatabaseSync.GenerateSyncScript;
begin
  // Collecter les informations de schéma
  CollectSourceSchema;
  CollectTargetSchema;

  // Comparer et générer le script
  CompareSchemas;
end;

procedure TDatabaseSync.ExecuteSyncScript;
var
  i: Integer;
  SQL: string;
  Transaction: TFDTransaction;
begin
  if FSyncScript.Count = 0 then
    Exit;

  Transaction := TFDTransaction.Create(nil);
  try
    Transaction.Connection := FTargetConnection;
    Transaction.StartTransaction;

    try
      for i := 0 to FSyncScript.Count - 1 do
      begin
        SQL := FSyncScript[i];

        // Ignorer les commentaires et lignes vides
        if (SQL = '') or SQL.StartsWith('--') then
          Continue;

        FTargetConnection.ExecSQL(SQL);
      end;

      Transaction.Commit;
    except
      on E: Exception do
      begin
        Transaction.Rollback;
        raise;  // Relancer l'exception pour la gestion en amont
      end;
    end;
  finally
    Transaction.Free;
  end;
end;

end.
```

#### Utilisation de l'outil de synchronisation

Voici comment utiliser cet outil :

```delphi
procedure TForm1.SynchroniserBasesDeDonnees;
var
  SourceConnection, TargetConnection: TFDConnection;
  DatabaseSync: TDatabaseSync;
begin
  SourceConnection := TFDConnection.Create(nil);
  TargetConnection := TFDConnection.Create(nil);
  try
    // Configurer la connexion source (base de référence)
    SourceConnection.DriverName := 'MySQL';
    SourceConnection.Params.Clear;
    SourceConnection.Params.Add('Server=localhost');
    SourceConnection.Params.Add('Database=ma_base_dev');
    SourceConnection.Params.Add('User_Name=root');
    SourceConnection.Params.Add('Password=password');
    SourceConnection.Connected := True;

    // Configurer la connexion cible (base à synchroniser)
    TargetConnection.DriverName := 'MySQL';
    TargetConnection.Params.Clear;
    TargetConnection.Params.Add('Server=192.168.1.10');
    TargetConnection.Params.Add('Database=ma_base_prod');
    TargetConnection.Params.Add('User_Name=admin');
    TargetConnection.Params.Add('Password=secure_password');
    TargetConnection.Connected := True;

    // Créer et exécuter la synchronisation
    DatabaseSync := TDatabaseSync.Create(SourceConnection, TargetConnection);
    try
      // Générer le script de synchronisation
      DatabaseSync.GenerateSyncScript;

      // Afficher le script pour vérification
      Memo1.Lines.Assign(DatabaseSync.SyncScript);

      // Exécuter si l'utilisateur confirme
      if MessageDlg('Voulez-vous exécuter ce script de synchronisation ?',
         mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        try
          DatabaseSync.ExecuteSyncScript;
          ShowMessage('Synchronisation terminée avec succès');
        except
          on E: Exception do
            ShowMessage('Erreur lors de la synchronisation : ' + E.Message);
        end;
      end;
    finally
      DatabaseSync.Free;
    end;
  finally
    SourceConnection.Free;
    TargetConnection.Free;
  end;
end;
```

## Migration de données

La migration de structure est une chose, mais que faire des données existantes ? Voici quelques techniques :

### 1. Préservation automatique des données

Pour les modifications simples (ajout de champ, changement de type compatible), MySQL préserve automatiquement les données. Exemple :

```sql
-- Les données sont conservées lors de cette opération
ALTER TABLE clients ADD COLUMN notes TEXT;
```

### 2. Migration manuelle de données

Pour des cas plus complexes, vous devrez écrire du code spécifique :

```sql
-- Exemple de migration de données entre tables
CREATE TABLE nouveaux_clients LIKE clients;
ALTER TABLE nouveaux_clients ADD COLUMN type_client ENUM('Particulier', 'Professionnel');

-- Copier les données avec logique métier
INSERT INTO nouveaux_clients (id, nom, prenom, email, type_client)
SELECT id, nom, prenom, email,
       CASE WHEN email LIKE '%@company.%' THEN 'Professionnel' ELSE 'Particulier' END
FROM clients;

-- Renommer les tables
RENAME TABLE clients TO clients_old, nouveaux_clients TO clients;
```

### 3. Utilisation de tables temporaires

Cette technique est utile pour des modifications complexes :

```sql
-- Créer une table temporaire avec la nouvelle structure
CREATE TABLE temp_clients (
  id INT AUTO_INCREMENT PRIMARY KEY,
  nom VARCHAR(100) NOT NULL,
  prenom VARCHAR(100),
  email VARCHAR(100) NOT NULL,
  type_client ENUM('Particulier', 'Professionnel')
);

-- Copier les données avec transformation
INSERT INTO temp_clients (id, nom, prenom, email, type_client)
SELECT id, nom, prenom, email,
       CASE WHEN email LIKE '%@company.%' THEN 'Professionnel' ELSE 'Particulier' END
FROM clients;

-- Remplacer l'ancienne table
DROP TABLE clients;
RENAME TABLE temp_clients TO clients;
```

## Meilleures pratiques pour la migration de bases de données

1. **Toujours sauvegarder avant une migration**
   ```sql
   -- Sauvegarde de la base de données
   mysqldump -u username -p database_name > backup_avant_migration.sql
   ```

2. **Tester les migrations dans un environnement de développement**
   - Ne jamais tester directement en production
   - Utiliser une copie de la base de production pour les tests

3. **Automatiser le processus**
   - Intégrer le système de migration dans votre application
   - Exécuter les migrations au démarrage de l'application

4. **Versionner les scripts de migration**
   - Garder tous les scripts dans un système de contrôle de version
   - Nommer clairement les versions (ex. "v1.0_to_v1.1_add_categories.sql")

5. **Documenter les changements**
   - Documenter pourquoi chaque changement a été fait
   - Tenir à jour un journal des migrations

6. **Planifier les migrations importantes**
   - Choisir des périodes de faible activité
   - Prévoir suffisamment de temps pour les tests et le rollback si nécessaire

## Intégration de la migration à votre application Delphi

### Vérification et migration au démarrage

Une approche courante consiste à vérifier la version de la base de données au démarrage de l'application :

```delphi
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Connexion à la base de données
  if not ConnecterBaseDeDonnees then
  begin
    ShowMessage('Impossible de se connecter à la base de données.');
    Exit;
  end;

  // Vérifier et mettre à jour la base de données si nécessaire
  if not MettreAJourBaseDeDonnees then
  begin
    ShowMessage('Erreur lors de la mise à jour de la base de données. ' +
                'L''application pourrait ne pas fonctionner correctement.');
  end;

  // Continuer l'initialisation de l'application
  ChargerDonnees;
end;

function TMainForm.MettreAJourBaseDeDonnees: Boolean;
var
  Migration: TDatabaseMigration;
begin
  Result := False;

  try
    // Créer le gestionnaire de migration
    Migration := TDatabaseMigration.Create(FDConnection1);
    try
      // Enregistrer toutes les migrations disponibles
      EnregistrerMigrations(Migration);

      // Exécuter les migrations nécessaires
      Result := Migration.MigrateToLatest;

      if Result then
        LogMessage('Base de données mise à jour avec succès')
      else
        LogErreur('Échec de la mise à jour de la base de données');
    finally
      Migration.Free;
    end;
  except
    on E: Exception do
    begin
      LogErreur('Exception lors de la mise à jour: ' + E.Message);
      Result := False;
    end;
  end;
end;

procedure TMainForm.EnregistrerMigrations(Migration: TDatabaseMigration);
begin
  // Version 1.1 - Structure initiale
  Migration.RegisterMigration('1.1', 'Structure initiale',
    procedure(M: TMigration)
    begin
      M.AddSQL('CREATE TABLE IF NOT EXISTS clients (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'nom VARCHAR(100) NOT NULL, ' +
               'prenom VARCHAR(100), ' +
               'email VARCHAR(100) NOT NULL, ' +
               'telephone VARCHAR(20), ' +
               'adresse TEXT, ' +
               'date_creation DATETIME DEFAULT CURRENT_TIMESTAMP)');

      M.AddSQL('CREATE TABLE IF NOT EXISTS produits (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'reference VARCHAR(20) NOT NULL, ' +
               'nom VARCHAR(100) NOT NULL, ' +
               'description TEXT, ' +
               'prix DECIMAL(10,2) NOT NULL, ' +
               'stock INT DEFAULT 0, ' +
               'date_creation DATETIME DEFAULT CURRENT_TIMESTAMP)');
    end);

  // Version 1.2 - Ajout table catégories
  Migration.RegisterMigration('1.2', 'Ajout table catégories',
    procedure(M: TMigration)
    begin
      M.AddSQL('CREATE TABLE IF NOT EXISTS categories (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'nom VARCHAR(100) NOT NULL, ' +
               'description TEXT, ' +
               'date_creation DATETIME DEFAULT CURRENT_TIMESTAMP)');

      M.AddSQL('ALTER TABLE produits ' +
               'ADD COLUMN categorie_id INT AFTER description, ' +
               'ADD CONSTRAINT fk_produit_categorie ' +
               'FOREIGN KEY (categorie_id) REFERENCES categories(id)');
    end);

  // Version 1.3 - Amélioration table clients
  Migration.RegisterMigration('1.3', 'Amélioration table clients',
    procedure(M: TMigration)
    begin
      M.AddSQL('ALTER TABLE clients ' +
               'ADD COLUMN date_naissance DATE AFTER adresse, ' +
               'ADD COLUMN ville_id INT AFTER adresse, ' +
               'ADD COLUMN notes TEXT AFTER adresse, ' +
               'ADD COLUMN actif BOOLEAN DEFAULT 1');

      M.AddSQL('CREATE TABLE IF NOT EXISTS villes (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'nom VARCHAR(100) NOT NULL, ' +
               'code_postal VARCHAR(10), ' +
               'pays VARCHAR(50) DEFAULT ''France'')');

      M.AddSQL('ALTER TABLE clients ' +
               'ADD CONSTRAINT fk_client_ville ' +
               'FOREIGN KEY (ville_id) REFERENCES villes(id)');
    end);

  // Version 1.4 - Ajout table commandes
  Migration.RegisterMigration('1.4', 'Ajout système de commandes',
    procedure(M: TMigration)
    begin
      M.AddSQL('CREATE TABLE IF NOT EXISTS commandes (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'client_id INT NOT NULL, ' +
               'date_commande DATETIME DEFAULT CURRENT_TIMESTAMP, ' +
               'statut ENUM(''En attente'', ''Validée'', ''Expédiée'', ''Livrée'', ''Annulée'') DEFAULT ''En attente'', ' +
               'total DECIMAL(10,2) DEFAULT 0, ' +
               'CONSTRAINT fk_commande_client FOREIGN KEY (client_id) REFERENCES clients(id))');

      M.AddSQL('CREATE TABLE IF NOT EXISTS commande_details (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'commande_id INT NOT NULL, ' +
               'produit_id INT NOT NULL, ' +
               'quantite INT NOT NULL DEFAULT 1, ' +
               'prix_unitaire DECIMAL(10,2) NOT NULL, ' +
               'CONSTRAINT fk_detail_commande FOREIGN KEY (commande_id) REFERENCES commandes(id), ' +
               'CONSTRAINT fk_detail_produit FOREIGN KEY (produit_id) REFERENCES produits(id))');
    end);
end;
```

### Interface utilisateur pour la migration

Pour les migrations plus importantes, vous pouvez créer une interface spécifique :

```delphi
procedure TMainForm.MettreAJourBaseDeDonneesAvecUI;
var
  FormMigration: TFormMigration;
begin
  FormMigration := TFormMigration.Create(Self);
  try
    FormMigration.Connection := FDConnection1;

    if FormMigration.ShowModal = mrOk then
      // Migration réussie, continuer
    else
      // Migration échouée ou annulée, prendre des mesures appropriées
  finally
    FormMigration.Free;
  end;
end;
```

Voici un exemple simple d'un formulaire de migration :

```delphi
// UFormMigration.pas
unit UFormMigration;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  FireDAC.Comp.Client, UDatabaseMigration;

type
  TFormMigration = class(TForm)
    LabelTitre: TLabel;
    ProgressBar: TProgressBar;
    MemoLog: TMemo;
    ButtonStart: TButton;
    ButtonClose: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
  private
    FConnection: TFDConnection;
    FMigration: TDatabaseMigration;
    FMigrationSuccess: Boolean;

    procedure LogMessage(const Msg: string);
    procedure EnregistrerMigrations;
    procedure ExecuterMigration;
  public
    property Connection: TFDConnection read FConnection write FConnection;
  end;

implementation

{$R *.dfm}

procedure TFormMigration.FormCreate(Sender: TObject);
begin
  FMigrationSuccess := False;
  ProgressBar.Position := 0;
  MemoLog.Clear;
  LogMessage('Prêt à démarrer la mise à jour de la base de données.');
  ButtonClose.Enabled := True;
end;

procedure TFormMigration.ButtonStartClick(Sender: TObject);
begin
  if not Assigned(FConnection) then
  begin
    LogMessage('Erreur: Connexion à la base de données non spécifiée.');
    Exit;
  end;

  // Désactiver les boutons pendant la migration
  ButtonStart.Enabled := False;
  ButtonClose.Enabled := False;

  try
    ExecuterMigration;
  finally
    // Réactiver le bouton de fermeture
    ButtonClose.Enabled := True;
  end;
end;

procedure TFormMigration.ButtonCloseClick(Sender: TObject);
begin
  if FMigrationSuccess then
    ModalResult := mrOk
  else
    ModalResult := mrCancel;
end;

procedure TFormMigration.LogMessage(const Msg: string);
begin
  MemoLog.Lines.Add(FormatDateTime('[hh:nn:ss] ', Now) + Msg);
  MemoLog.Perform(EM_SCROLLCARET, 0, 0);
  Application.ProcessMessages;
end;

procedure TFormMigration.EnregistrerMigrations;
begin
  // Enregistrer toutes les migrations disponibles
  // (même code que dans TMainForm.EnregistrerMigrations)

  // Version 1.1 - Structure initiale
  FMigration.RegisterMigration('1.1', 'Structure initiale',
    procedure(M: TMigration)
    begin
      M.AddSQL('CREATE TABLE IF NOT EXISTS clients (' +
               'id INT AUTO_INCREMENT PRIMARY KEY, ' +
               'nom VARCHAR(100) NOT NULL, ' +
               'prenom VARCHAR(100), ' +
               'email VARCHAR(100) NOT NULL, ' +
               'telephone VARCHAR(20), ' +
               'adresse TEXT, ' +
               'date_creation DATETIME DEFAULT CURRENT_TIMESTAMP)');

      // ... autres instructions SQL
    end);

  // ... autres migrations
end;

procedure TFormMigration.ExecuterMigration;
begin
  try
    LogMessage('Initialisation du processus de migration...');
    ProgressBar.Position := 10;

    // Créer le gestionnaire de migration
    FMigration := TDatabaseMigration.Create(FConnection);
    try
      // Enregistrer les migrations
      LogMessage('Chargement des scripts de migration...');
      EnregistrerMigrations;
      ProgressBar.Position := 30;

      // Vérification de la version actuelle
      LogMessage('Version actuelle de la base de données: ' + FMigration.CurrentVersion);
      ProgressBar.Position := 50;

      // Exécuter les migrations
      LogMessage('Exécution des migrations...');
      FMigrationSuccess := FMigration.MigrateToLatest;
      ProgressBar.Position := 90;

      if FMigrationSuccess then
      begin
        LogMessage('Migration terminée avec succès!');
        LogMessage('Nouvelle version de la base de données: ' + FMigration.CurrentVersion);
        ProgressBar.Position := 100;
      end
      else
      begin
        LogMessage('Erreur: La migration a échoué.');
        ProgressBar.Position := 0;
      end;
    finally
      FMigration.Free;
    end;
  except
    on E: Exception do
    begin
      LogMessage('Exception lors de la migration: ' + E.Message);
      FMigrationSuccess := False;
      ProgressBar.Position := 0;
    end;
  end;

  // Mettre à jour l'UI
  ButtonStart.Enabled := not FMigrationSuccess;
  if FMigrationSuccess then
    ButtonClose.Caption := 'Terminer'
  else
    ButtonClose.Caption := 'Annuler';
end;

end.
```

## Gérer les migrations dans les déploiements multi-utilisateurs

Lorsque vous déployez une application auprès de nombreux clients, la gestion des migrations devient plus complexe. Voici quelques stratégies :

### 1. Migrations automatiques intégrées

- L'application vérifie et met à jour automatiquement la base de données au démarrage
- Avantage : Transparent pour l'utilisateur
- Inconvénient : Peut causer des problèmes si l'utilisateur n'a pas les droits d'administration sur la base de données

### 2. Outil d'administration séparé

- Créez un outil d'administration distinct pour la mise à jour des bases de données
- Avantage : Peut être exécuté avec des privilèges administratifs
- Inconvénient : Nécessite une étape supplémentaire lors des mises à jour

### 3. Scripts de mise à jour fournis avec l'installateur

- Incluez les scripts SQL dans votre package d'installation
- Exécutez-les pendant le processus d'installation
- Avantage : Intégration avec le système d'installation
- Inconvénient : Moins flexible pour les mises à jour intermédiaires

## Gestion des erreurs de migration

Les erreurs de migration peuvent être catastrophiques si elles ne sont pas correctement gérées. Voici quelques techniques pour minimiser les risques :

### 1. Transactions

Utilisez des transactions pour garantir l'atomicité des migrations :

```delphi
// Dans la méthode ExecuteMigration de TDatabaseMigration
procedure TDatabaseMigration.ExecuteMigration(AMigration: TMigration);
var
  Transaction: TFDTransaction;
begin
  Transaction := TFDTransaction.Create(nil);
  try
    Transaction.Connection := FConnection;
    Transaction.StartTransaction;

    try
      // Exécuter toutes les instructions SQL
      // ...

      // Si tout va bien, valider la transaction
      Transaction.Commit;
    except
      on E: Exception do
      begin
        // En cas d'erreur, annuler toutes les modifications
        Transaction.Rollback;
        raise;  // Relancer l'exception pour la gestion en amont
      end;
    end;
  finally
    Transaction.Free;
  end;
end;
```

### 2. Sauvegardes automatiques

Effectuez une sauvegarde avant chaque migration importante :

```delphi
procedure TDatabaseMigration.BackupDatabase(const BackupPath: string);
var
  SaveDialog: TSaveDialog;
  BackupFile: string;
  Command: string;
begin
  if BackupPath = '' then
  begin
    SaveDialog := TSaveDialog.Create(nil);
    try
      SaveDialog.Title := 'Sauvegarde avant migration';
      SaveDialog.DefaultExt := 'sql';
      SaveDialog.Filter := 'Fichiers SQL (*.sql)|*.sql|Tous les fichiers (*.*)|*.*';
      SaveDialog.FileName := 'backup_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.sql';

      if SaveDialog.Execute then
        BackupFile := SaveDialog.FileName
      else
        Exit;  // L'utilisateur a annulé
    finally
      SaveDialog.Free;
    end;
  end
  else
    BackupFile := IncludeTrailingPathDelimiter(BackupPath) +
                  'backup_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.sql';

  // Exécuter mysqldump via la ligne de commande
  // Note: cela nécessite que mysqldump soit accessible dans le PATH
  Command := Format('mysqldump -h %s -u %s -p%s %s > "%s"',
    [FConnection.Params.Values['Server'],
     FConnection.Params.Values['User_Name'],
     FConnection.Params.Values['Password'],
     FConnection.Params.Values['Database'],
     BackupFile]);

  // Exécuter la commande
  // Note: Dans une application réelle, utilisez TProcess ou une autre méthode plus sécurisée
  if ShellExecute(0, 'open', 'cmd.exe', PChar('/c ' + Command), nil, SW_HIDE) <= 32 then
    raise Exception.Create('Erreur lors de la sauvegarde de la base de données');
end;
```

### 3. Système de journalisation détaillé

Implémentez un système de journalisation pour suivre chaque étape de la migration :

```delphi
procedure TDatabaseMigration.Log(const Msg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg);

  // Journaliser dans un fichier
  if FLogToFile and (FLogFileName <> '') then
  begin
    try
      with TStreamWriter.Create(FLogFileName, True, TEncoding.UTF8) do
      try
        WriteLine(FormatDateTime('[yyyy-mm-dd hh:nn:ss] ', Now) + Msg);
      finally
        Free;
      end;
    except
      // Ignorer les erreurs de journalisation
    end;
  end;
end;
```

## Migration de données complexes

Pour les migrations très complexes, vous pouvez avoir besoin d'outils plus puissants. Voici quelques techniques avancées :

### 1. Utilisation de procédures stockées

Pour des transformations de données complexes, les procédures stockées peuvent être plus efficaces :

```sql
-- Exemple de procédure stockée pour migrer des données
DELIMITER //
CREATE PROCEDURE migrate_client_data()
BEGIN
  DECLARE done INT DEFAULT FALSE;
  DECLARE client_id INT;
  DECLARE client_email VARCHAR(100);
  DECLARE cur CURSOR FOR SELECT id, email FROM clients_old;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET done = TRUE;

  OPEN cur;

  read_loop: LOOP
    FETCH cur INTO client_id, client_email;
    IF done THEN
      LEAVE read_loop;
    END IF;

    -- Logique complexe de migration
    IF client_email LIKE '%@company.%' THEN
      INSERT INTO clients_corporate (id, email, category)
      VALUES (client_id, client_email, 'Enterprise');
    ELSE
      INSERT INTO clients_individual (id, email, category)
      VALUES (client_id, client_email, 'Consumer');
    END IF;
  END LOOP;

  CLOSE cur;
END //
DELIMITER ;

-- Exécuter la procédure
CALL migrate_client_data();

-- Nettoyer
DROP PROCEDURE migrate_client_data;
```

### 2. Migration par lots

Pour les grandes tables, procédez par lots pour éviter les blocages :

```delphi
procedure TMigrationAvancee.MigrerDonneesParLots(
  const TableSource, TableDestination: string;
  const BatchSize: Integer = 1000);
var
  Query: TFDQuery;
  MaxID, MinID, CurrentID: Integer;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // Obtenir la plage d'IDs
    Query.SQL.Text := Format('SELECT MIN(id) as min_id, MAX(id) as max_id FROM %s', [TableSource]);
    Query.Open;
    MinID := Query.FieldByName('min_id').AsInteger;
    MaxID := Query.FieldByName('max_id').AsInteger;
    Query.Close;

    // Traiter par lots
    CurrentID := MinID;
    while CurrentID <= MaxID do
    begin
      // Commencer une transaction pour ce lot
      FConnection.StartTransaction;
      try
        // Migrer un lot de données
        Query.SQL.Text := Format(
          'INSERT INTO %s (id, nom, email, type) ' +
          'SELECT id, nom, email, ' +
          'CASE WHEN email LIKE ''%%@company.%%'' THEN ''Professionnel'' ELSE ''Particulier'' END ' +
          'FROM %s WHERE id >= %d AND id < %d',
          [TableDestination, TableSource, CurrentID, CurrentID + BatchSize]);
        Query.ExecSQL;

        // Valider ce lot
        FConnection.Commit;

        // Journaliser
        Log(Format('Lot migré: IDs %d à %d', [CurrentID, CurrentID + BatchSize - 1]));
      except
        on E: Exception do
        begin
          // Annuler en cas d'erreur
          FConnection.Rollback;
          Log(Format('Erreur lors de la migration du lot %d à %d: %s',
                     [CurrentID, CurrentID + BatchSize - 1, E.Message]));
          raise;
        end;
      end;

      // Passer au lot suivant
      CurrentID := CurrentID + BatchSize;

      // Mettre à jour la progression si nécessaire
      if Assigned(FOnProgress) then
        FOnProgress(Round(((CurrentID - MinID) / (MaxID - MinID + 1)) * 100));
    end;
  finally
    Query.Free;
  end;
end;
```

## Conclusion

La migration et la synchronisation de bases de données sont des aspects cruciaux du développement d'applications professionnelles avec Delphi et MySQL. Une bonne stratégie de migration vous permet de faire évoluer votre application tout en préservant les données existantes et en minimisant les perturbations pour les utilisateurs.

Les points clés à retenir :
- Planifiez vos migrations à l'avance
- Utilisez un système de versionnage pour suivre les changements
- Testez toujours les migrations dans un environnement de développement
- Sauvegardez les données avant toute migration importante
- Automatisez le processus autant que possible
- Journalisez toutes les opérations pour faciliter le dépannage

En suivant ces principes et en utilisant les techniques présentées dans cette section, vous pourrez gérer efficacement l'évolution de vos bases de données tout au long du cycle de vie de votre application.

---

**À suivre :** 8.11 Sécurisation des accès et prévention des injections SQL
