# 8.3 FireDAC : architecture et composants

Dans cette section, nous allons explorer l'architecture de FireDAC et ses principaux composants. Comprendre ces éléments vous permettra de tirer le meilleur parti de cette technologie d'accès aux données puissante et flexible.

## Qu'est-ce que FireDAC ?

FireDAC (Fast InProcess REtrieval of Data) est la bibliothèque d'accès aux données moderne de Delphi. Elle remplace les anciennes technologies comme BDE, dbExpress et ADO, en offrant de meilleures performances et davantage de fonctionnalités.

![Logo FireDAC](https://placeholder.pics/svg/300x100/DEDEDE/555555/FireDAC)

## L'architecture en couches de FireDAC

FireDAC utilise une architecture en couches qui sépare clairement les différentes responsabilités. Cette conception rend FireDAC à la fois puissant et flexible.

### Les couches principales de FireDAC

1. **Couche Application** : Interface avec votre code Delphi
2. **Couche Dataset** : Gestion des données en mémoire
3. **Couche SQL Command** : Traduction et exécution des requêtes SQL
4. **Couche Database** : Communication avec les bases de données spécifiques
5. **Couche Driver** : Interface avec les bibliothèques clientes des SGBD

Voici une représentation simplifiée de cette architecture :

```
+---------------------------+
|   Application Delphi      |
+---------------------------+
            ↕
+---------------------------+
|  Composants FireDAC       |
|  (TFDQuery, TFDTable...)  |
+---------------------------+
            ↕
+---------------------------+
|    Moteur FireDAC         |
+---------------------------+
            ↕
+---------------------------+
|   Pilotes spécifiques     |
|   (MySQL, SQLite...)      |
+---------------------------+
            ↕
+---------------------------+
|   Bases de données        |
+---------------------------+
```

Cette architecture en couches offre plusieurs avantages :

- **Uniformité** : La même interface est utilisée, quel que soit le SGBD
- **Flexibilité** : Vous pouvez changer de base de données sans modifier votre code
- **Performance** : Optimisation à chaque niveau de l'architecture
- **Extensibilité** : Possibilité d'ajouter de nouveaux pilotes

## Les composants essentiels de FireDAC

### TFDConnection

**TFDConnection** est le composant central qui gère la connexion à la base de données. C'est toujours le premier composant à configurer dans votre application.

#### Propriétés importantes de TFDConnection

- **DriverName** : Définit le type de base de données ('MySQL', 'SQLite', etc.)
- **Params** : Collection de paramètres pour configurer la connexion
- **Connected** : État de la connexion (True/False)
- **LoginPrompt** : Demande ou non les identifiants à l'utilisateur

#### Exemple de configuration pour MySQL

```delphi
FDConnection1.DriverName := 'MySQL';
FDConnection1.Params.Clear;
FDConnection1.Params.Add('Server=localhost');
FDConnection1.Params.Add('Database=ma_base');
FDConnection1.Params.Add('User_Name=mon_utilisateur');
FDConnection1.Params.Add('Password=mon_mot_de_passe');
FDConnection1.Params.Add('CharacterSet=utf8mb4');
FDConnection1.Connected := True;
```

### TFDQuery

**TFDQuery** est l'un des composants les plus utilisés. Il vous permet d'exécuter des requêtes SQL et de manipuler les résultats.

#### Propriétés importantes de TFDQuery

- **Connection** : Référence au composant TFDConnection
- **SQL** : Contient la requête SQL à exécuter
- **Active** : Indique si la requête est active
- **ParamByName** : Accède aux paramètres de la requête

#### Exemple d'utilisation de TFDQuery

```delphi
// Requête simple
FDQuery1.Connection := FDConnection1;
FDQuery1.SQL.Text := 'SELECT * FROM clients';
FDQuery1.Active := True;  // Exécute la requête et charge les résultats

// Requête avec paramètres
FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE ville = :ville';
FDQuery1.ParamByName('ville').AsString := 'Paris';
FDQuery1.Active := True;
```

### TFDTable

**TFDTable** représente une table complète de la base de données. Il est plus simple à utiliser que TFDQuery pour les opérations basiques.

#### Propriétés importantes de TFDTable

- **Connection** : Référence au composant TFDConnection
- **TableName** : Nom de la table à manipuler
- **Active** : Indique si la table est active

#### Exemple d'utilisation de TFDTable

```delphi
FDTable1.Connection := FDConnection1;
FDTable1.TableName := 'clients';
FDTable1.Active := True;  // Charge tous les enregistrements de la table
```

### TFDUpdateSQL

**TFDUpdateSQL** permet de personnaliser les opérations d'insertion, de mise à jour et de suppression associées à un TFDQuery.

#### Exemple d'utilisation de TFDUpdateSQL

```delphi
// Configuration du composant TFDUpdateSQL
FDUpdateSQL1.Connection := FDConnection1;
FDUpdateSQL1.InsertSQL.Text := 'INSERT INTO clients (nom, prenom) VALUES (:nom, :prenom)';
FDUpdateSQL1.ModifySQL.Text := 'UPDATE clients SET nom = :nom, prenom = :prenom WHERE id = :id';
FDUpdateSQL1.DeleteSQL.Text := 'DELETE FROM clients WHERE id = :id';

// Association avec un TFDQuery
FDQuery1.UpdateObject := FDUpdateSQL1;
```

### TFDTransaction

**TFDTransaction** gère les transactions pour assurer l'intégrité des données lors d'opérations multiples.

#### Exemple d'utilisation de TFDTransaction

```delphi
try
  // Démarrer la transaction
  FDTransaction1.Connection := FDConnection1;
  FDTransaction1.StartTransaction;

  // Exécuter des opérations
  FDQuery1.SQL.Text := 'INSERT INTO commandes (client_id, date) VALUES (1, NOW())';
  FDQuery1.ExecSQL;

  FDQuery1.SQL.Text := 'INSERT INTO details_commande (commande_id, produit_id, quantite) VALUES (LAST_INSERT_ID(), 5, 2)';
  FDQuery1.ExecSQL;

  // Valider les modifications
  FDTransaction1.Commit;
except
  on E: Exception do
  begin
    // En cas d'erreur, annuler toutes les modifications
    FDTransaction1.Rollback;
    ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### TDataSource

**TDataSource** fait le lien entre les données (TFDQuery, TFDTable) et les contrôles visuels. Ce n'est pas un composant FireDAC à proprement parler, mais il est essentiel pour l'affichage des données.

#### Exemple d'utilisation de TDataSource

```delphi
// Configuration de la source de données
DataSource1.DataSet := FDQuery1;

// Liaison avec des contrôles visuels
DBGrid1.DataSource := DataSource1;
DBEdit1.DataSource := DataSource1;
```

## Familles de composants FireDAC

FireDAC organise ses composants en plusieurs familles, chacune ayant un rôle spécifique :

### Composants de connexion et pilotes

- **TFDConnection** : Connexion à la base de données
- **TFDPhysMySQLDriverLink** : Lien explicite au pilote MySQL (optionnel)
- **TFDManagerLink** : Gestionnaire de connexions centralisé pour les applications multi-connexions

### Composants d'accès aux données

- **TFDQuery** : Exécution de requêtes SQL
- **TFDTable** : Accès direct aux tables
- **TFDStoredProc** : Exécution de procédures stockées
- **TFDCommand** : Commandes SQL sans jeu de résultats

### Composants de mise à jour

- **TFDUpdateSQL** : Personnalisation des opérations de mise à jour
- **TFDBatchMove** : Import/export de données en masse

### Composants de contrôle

- **TFDTransaction** : Gestion des transactions
- **TFDSchemaAdapter** : Coordination des mises à jour entre plusieurs DataSets

### Composants de mise en cache et de mémoire

- **TFDMemTable** : Table en mémoire
- **TFDLocalSQL** : Moteur SQL local pour les données en mémoire

## Emplacement des composants dans l'IDE

Dans Delphi, les composants FireDAC sont regroupés dans plusieurs onglets de la palette de composants :

- **FireDAC** : Composants principaux
- **FireDAC Comp** : Composants spécifiques aux bases de données
- **FireDAC UI** : Composants d'interface utilisateur pour FireDAC

![Palette FireDAC](https://placeholder.pics/svg/600x150/DEDEDE/555555/Palette%20FireDAC)

## L'architecture en action : Flux des données avec FireDAC

Voici comment les données circulent dans une application typique utilisant FireDAC :

1. **Connexion à la base de données** : Le composant TFDConnection établit la connexion
2. **Exécution de requêtes** : TFDQuery ou TFDTable envoie des commandes SQL
3. **Récupération des données** : Les résultats sont chargés dans un buffer local
4. **Liaison des données** : TDataSource relie les données aux contrôles visuels
5. **Modification des données** : Les changements sont d'abord stockés localement
6. **Validation des modifications** : TFDQuery.Post ou TFDTable.Post enregistre les changements
7. **Application des modifications** : Les modifications sont envoyées à la base de données
8. **Gestion des erreurs** : Les exceptions sont levées en cas de problème

## Démonstration pratique : Configuration complète

Voici un exemple complet qui montre comment configurer et utiliser les composants FireDAC pour afficher et modifier des données d'une table MySQL :

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  // 1. Configuration de la connexion
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=mon_utilisateur');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');

  try
    // 2. Établir la connexion
    FDConnection1.Connected := True;

    // 3. Configuration de la requête
    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom';

    // 4. Exécution de la requête
    FDQuery1.Open;

    StatusBar1.SimpleText := 'Connecté à la base de données. ' +
                             IntToStr(FDQuery1.RecordCount) + ' enregistrements chargés.';
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;

// Ajouter un nouvel enregistrement
procedure TForm1.btnAjouterClick(Sender: TObject);
begin
  FDQuery1.Append;  // Prépare un nouvel enregistrement
  // Les champs peuvent être remplis via DBEdit ou par code :
  // FDQuery1.FieldByName('nom').AsString := 'Nouveau nom';
end;

// Enregistrer les modifications
procedure TForm1.btnEnregistrerClick(Sender: TObject);
begin
  if FDQuery1.State in [dsEdit, dsInsert] then
    FDQuery1.Post;  // Valide les modifications
end;

// Supprimer un enregistrement
procedure TForm1.btnSupprimerClick(Sender: TObject);
begin
  if not FDQuery1.IsEmpty then
    if MessageDlg('Êtes-vous sûr de vouloir supprimer cet enregistrement ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FDQuery1.Delete;
end;
```

## Bonnes pratiques avec FireDAC

Pour terminer, voici quelques bonnes pratiques à suivre lorsque vous utilisez FireDAC :

1. **Gérez les connexions efficacement** : Fermez les connexions lorsqu'elles ne sont plus nécessaires.
2. **Utilisez les paramètres** : Préférez les requêtes paramétrées pour éviter les injections SQL.
3. **Gérez les transactions** : Utilisez des transactions pour les opérations multiples.
4. **Fermez les requêtes** : Fermez les TFDQuery lorsqu'elles ne sont plus utilisées.
5. **Contrôlez les exceptions** : Encadrez vos opérations de base de données dans des blocs try/except.
6. **Optimisez les requêtes** : Limitez les données récupérées à ce qui est nécessaire.
7. **Utilisez FetchOptions** : Configurez les options de récupération pour optimiser les performances.

## Conclusion

FireDAC est une technologie d'accès aux données puissante et flexible. Sa conception en couches et sa riche collection de composants vous permettent de créer des applications robustes qui interagissent efficacement avec diverses bases de données, dont MySQL/MariaDB.

Dans la prochaine section, nous mettrons en pratique ces connaissances en nous connectant spécifiquement à une base de données MySQL/MariaDB et en effectuant des opérations de base.

---

**À suivre :** 8.4 Connexion à une base MySQL/MariaDB
