# 8.13 NoSQL et bases de données documentaires

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Jusqu'à présent, nous avons exploré des bases de données relationnelles traditionnelles comme MySQL, SQLite, PostgreSQL et SQL Server. Mais il existe une autre catégorie de bases de données qui gagne en popularité : les bases de données NoSQL. Dans cette section, nous découvrirons ce qu'est le NoSQL, quand l'utiliser, et comment l'intégrer dans vos applications Delphi.

## Qu'est-ce que NoSQL ?

NoSQL (qui signifie "Not Only SQL") désigne une famille de bases de données conçues pour stocker, distribuer et accéder aux données d'une manière différente des bases de données relationnelles traditionnelles. Contrairement aux bases SQL qui organisent les données en tables avec des relations prédéfinies, les bases NoSQL offrent des modèles de données plus flexibles.

### Les principales caractéristiques des bases de données NoSQL

- **Schéma flexible** : Pas besoin de définir une structure rigide avant de stocker des données
- **Mise à l'échelle horizontale** : Facilité à distribuer les données sur plusieurs serveurs
- **Optimisées pour certains types d'opérations** : Souvent plus performantes pour des cas d'usage spécifiques
- **Moins d'accent sur les relations** : Les relations entre entités sont gérées différemment

## Types de bases de données NoSQL

Il existe quatre grands types de bases de données NoSQL :

### 1. Bases de données documentaires

Elles stockent des données sous forme de documents, généralement au format JSON ou BSON (Binary JSON). Chaque document peut avoir une structure différente et contenir des données complexes.

**Exemples** : MongoDB, Couchbase, RavenDB

### 2. Bases de données clé-valeur

Ce sont les plus simples des bases NoSQL. Elles associent chaque clé à une valeur, similaire à un dictionnaire ou une table de hachage.

**Exemples** : Redis, Amazon DynamoDB, Riak

### 3. Bases de données en colonnes

Elles stockent les données par colonnes plutôt que par lignes, ce qui est très efficace pour l'analyse de grandes quantités de données.

**Exemples** : Apache Cassandra, HBase, Google Bigtable

### 4. Bases de données graphes

Elles sont conçues pour représenter et stocker des relations complexes entre entités, sous forme de graphes.

**Exemples** : Neo4j, Amazon Neptune, ArangoDB

## Quand utiliser NoSQL plutôt que SQL ?

Les bases de données NoSQL sont particulièrement adaptées à certains scénarios :

- **Données semi-structurées ou non structurées** : Quand vos données n'ont pas de schéma fixe
- **Grands volumes de données** : Quand vous avez besoin de stocker des téraoctets de données
- **Mise à l'échelle horizontale** : Quand vous devez distribuer vos données sur plusieurs serveurs
- **Développement agile** : Quand le schéma de données évolue fréquemment
- **Haute disponibilité** : Quand votre application ne peut pas se permettre de temps d'arrêt

En revanche, les bases de données relationnelles (SQL) restent préférables pour :

- **Données fortement structurées** : Quand votre modèle de données est stable et bien défini
- **Transactions complexes** : Quand vous avez besoin de garanties ACID strictes
- **Requêtes complexes** : Quand vous devez effectuer des jointures et des agrégations sophistiquées
- **Intégrité référentielle** : Quand les relations entre vos données sont cruciales

## MongoDB : une base de données documentaire populaire

MongoDB est l'une des bases de données NoSQL les plus populaires. C'est une base documentaire qui stocke les données au format BSON (Binary JSON). Voyons comment l'utiliser avec Delphi.

### Installation et configuration de MongoDB

1. **Téléchargez MongoDB** depuis le [site officiel](https://www.mongodb.com/try/download/community)
2. **Installez MongoDB** en suivant les instructions
3. **Lancez le serveur MongoDB** (généralement via le service "MongoDB")
4. **Installez MongoDB Compass** (interface graphique officielle) pour explorer vos données plus facilement

### Connexion à MongoDB depuis Delphi

Delphi n'a pas de support natif pour MongoDB via FireDAC, mais il existe plusieurs bibliothèques tierces pour y accéder :

1. **DelphiMongoDB** : Une bibliothèque open-source simple
2. **MongoDB-Delphi-Driver** : Un driver plus complet
3. **TMS Aurelius** : Un ORM qui supporte MongoDB (solution commerciale)

Dans cet exemple, nous utiliserons la bibliothèque MongoDB-Delphi-Driver, qui est disponible sur GitHub.

#### Installation de MongoDB-Delphi-Driver

1. Clonez ou téléchargez le dépôt depuis [GitHub](https://github.com/mongodb-delphi/mongo-delphi-driver)
2. Ajoutez le chemin du dossier `source` à vos chemins de bibliothèques dans Delphi
3. Installez le package si nécessaire

#### Exemple basique de connexion à MongoDB

```delphi
uses
  MongoDB.Client, MongoDB.Connection, MongoDB.Protocol.Core;

procedure TForm1.ConnecterMongoDB;
var
  MongoClient: TMongoClient;
  Database: TMongoDatabase;
  Collection: TMongoCollection;
  Document: TMongoDocument;
begin
  try
    // Créer un client MongoDB
    MongoClient := TMongoClient.Create('mongodb://localhost:27017');
    try
      // Obtenir une référence à une base de données
      Database := MongoClient.GetDatabase('ma_base');

      // Obtenir une référence à une collection (équivalent d'une table)
      Collection := Database.GetCollection('clients');

      // Créer un document (équivalent d'un enregistrement)
      Document := TMongoDocument.Create;
      Document.Add('nom', 'Dupont');
      Document.Add('prenom', 'Jean');
      Document.Add('email', 'jean.dupont@exemple.com');
      Document.Add('age', 42);

      // Ajouter un sous-document pour l'adresse
      var Adresse := TMongoDocument.Create;
      Adresse.Add('rue', '123 Rue Principale');
      Adresse.Add('ville', 'Paris');
      Adresse.Add('code_postal', '75001');
      Document.Add('adresse', Adresse);

      // Insérer le document dans la collection
      Collection.InsertOne(Document);

      ShowMessage('Document inséré avec succès !');
    finally
      MongoClient.Free;
    end;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

### Opérations CRUD avec MongoDB

#### Créer (Create)

```delphi
procedure TForm1.AjouterClient(const Nom, Prenom, Email: string; Age: Integer);
var
  MongoClient: TMongoClient;
  Collection: TMongoCollection;
  Document: TMongoDocument;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Collection := MongoClient.GetDatabase('ma_base').GetCollection('clients');

    Document := TMongoDocument.Create;
    Document.Add('nom', Nom);
    Document.Add('prenom', Prenom);
    Document.Add('email', Email);
    Document.Add('age', Age);
    Document.Add('date_creation', Now);

    Collection.InsertOne(Document);
  finally
    MongoClient.Free;
  end;
end;
```

#### Lire (Read)

```delphi
procedure TForm1.RechercherParNom(const Nom: string);
var
  MongoClient: TMongoClient;
  Collection: TMongoCollection;
  Filter: TMongoDocument;
  Cursor: IMongoCursor;
  Document: TMongoDocument;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Collection := MongoClient.GetDatabase('ma_base').GetCollection('clients');

    // Créer un filtre (équivalent de la clause WHERE)
    Filter := TMongoDocument.Create;
    Filter.Add('nom', Nom);

    // Exécuter la requête
    Cursor := Collection.Find(Filter);

    // Parcourir les résultats
    Memo1.Clear;
    Memo1.Lines.Add('Résultats de la recherche :');

    while Cursor.Next do
    begin
      Document := Cursor.Doc;

      Memo1.Lines.Add(Format('%s %s (%s) - %d ans',
        [Document.Values['prenom'].AsString,
         Document.Values['nom'].AsString,
         Document.Values['email'].AsString,
         Document.Values['age'].AsInteger]));
    end;
  finally
    MongoClient.Free;
  end;
end;
```

#### Mettre à jour (Update)

```delphi
procedure TForm1.MettreAJourEmail(const Nom, Prenom, NouvelEmail: string);
var
  MongoClient: TMongoClient;
  Collection: TMongoCollection;
  Filter, Update: TMongoDocument;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Collection := MongoClient.GetDatabase('ma_base').GetCollection('clients');

    // Créer un filtre pour trouver le document à mettre à jour
    Filter := TMongoDocument.Create;
    Filter.Add('nom', Nom);
    Filter.Add('prenom', Prenom);

    // Créer le document de mise à jour
    Update := TMongoDocument.Create;
    var SetDoc := TMongoDocument.Create;
    SetDoc.Add('email', NouvelEmail);
    SetDoc.Add('date_modification', Now);
    Update.Add('$set', SetDoc);

    // Exécuter la mise à jour
    Collection.UpdateOne(Filter, Update);
  finally
    MongoClient.Free;
  end;
end;
```

#### Supprimer (Delete)

```delphi
procedure TForm1.SupprimerClient(const Nom, Prenom: string);
var
  MongoClient: TMongoClient;
  Collection: TMongoCollection;
  Filter: TMongoDocument;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Collection := MongoClient.GetDatabase('ma_base').GetCollection('clients');

    // Créer un filtre pour trouver le document à supprimer
    Filter := TMongoDocument.Create;
    Filter.Add('nom', Nom);
    Filter.Add('prenom', Prenom);

    // Exécuter la suppression
    Collection.DeleteOne(Filter);
  finally
    MongoClient.Free;
  end;
end;
```

### Requêtes avancées avec MongoDB

MongoDB propose un langage de requête riche. Voici quelques exemples :

#### Requête avec opérateurs de comparaison

```delphi
procedure TForm1.RechercherClientsAgés();
var
  MongoClient: TMongoClient;
  Collection: TMongoCollection;
  Filter: TMongoDocument;
  Cursor: IMongoCursor;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Collection := MongoClient.GetDatabase('ma_base').GetCollection('clients');

    // Créer un filtre avec un opérateur de comparaison
    Filter := TMongoDocument.Create;
    var AgeFilter := TMongoDocument.Create;
    AgeFilter.Add('$gt', 50);  // $gt = greater than (supérieur à)
    Filter.Add('age', AgeFilter);

    // Exécuter la requête et afficher les résultats
    Cursor := Collection.Find(Filter);

    Memo1.Clear;
    Memo1.Lines.Add('Clients de plus de 50 ans :');

    while Cursor.Next do
    begin
      var Document := Cursor.Doc;
      Memo1.Lines.Add(Format('%s %s - %d ans',
        [Document.Values['prenom'].AsString,
         Document.Values['nom'].AsString,
         Document.Values['age'].AsInteger]));
    end;
  finally
    MongoClient.Free;
  end;
end;
```

#### Agrégation de données

```delphi
procedure TForm1.AgesMoyensParVille;
var
  MongoClient: TMongoClient;
  Database: TMongoDatabase;
  Pipeline: TArray<TMongoDocument>;
  Cursor: IMongoCursor;
begin
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Database := MongoClient.GetDatabase('ma_base');

    // Créer un pipeline d'agrégation
    SetLength(Pipeline, 2);

    // Étape 1 : Grouper par ville
    Pipeline[0] := TMongoDocument.Create;
    var Group := TMongoDocument.Create;
    var ID := TMongoDocument.Create;
    ID.Add('ville', '$adresse.ville');
    Group.Add('_id', ID);
    Group.Add('age_moyen', TMongoDocument.Create.Add('$avg', '$age'));
    Group.Add('nombre', TMongoDocument.Create.Add('$sum', 1));
    Pipeline[0].Add('$group', Group);

    // Étape 2 : Trier par âge moyen décroissant
    Pipeline[1] := TMongoDocument.Create;
    Pipeline[1].Add('$sort', TMongoDocument.Create.Add('age_moyen', -1));

    // Exécuter l'agrégation
    Cursor := Database.RunCommand(TMongoDocument.Create.Add('aggregate', 'clients')
                                 .Add('pipeline', Pipeline)
                                 .Add('cursor', TMongoDocument.Create));

    // Afficher les résultats
    Memo1.Clear;
    Memo1.Lines.Add('Âge moyen par ville :');

    while Cursor.Next do
    begin
      var Document := Cursor.Doc;
      var Ville := Document.Values['_id'].AsDocument.Values['ville'].AsString;
      var AgeMoyen := Document.Values['age_moyen'].AsDouble;
      var Nombre := Document.Values['nombre'].AsInteger;

      Memo1.Lines.Add(Format('%s : %.1f ans (%d clients)', [Ville, AgeMoyen, Nombre]));
    end;
  finally
    MongoClient.Free;
  end;
end;
```

## Embarcadero InterBase avec JSON

Si vous préférez rester dans l'écosystème Embarcadero, InterBase (à partir de la version 2017) offre un support natif pour le stockage et la requête de données JSON. C'est une approche hybride qui combine les avantages des bases relationnelles et documentaires.

### Configuration d'InterBase avec JSON

```delphi
procedure TForm1.ConfigurerInterBaseJSON;
begin
  FDConnection1.DriverName := 'IB';
  FDConnection1.Params.Database := 'C:\Data\mabase.ib';
  FDConnection1.Params.UserName := 'sysdba';
  FDConnection1.Params.Password := 'masterkey';
  FDConnection1.Connected := True;

  // Créer une table avec une colonne JSON
  FDConnection1.ExecSQL(
    'CREATE TABLE produits (' +
    '  id INTEGER NOT NULL PRIMARY KEY,' +
    '  nom VARCHAR(100) NOT NULL,' +
    '  prix NUMERIC(10,2),' +
    '  caracteristiques BLOB SUB_TYPE JSON' +  // Colonne JSON
    ')'
  );
end;
```

### Utilisation des données JSON dans InterBase

```delphi
procedure TForm1.AjouterProduitAvecJSON;
var
  JsonData: string;
begin
  // Créer un objet JSON
  JsonData := '{"couleur": "noir", "dimensions": {"hauteur": 15, "largeur": 7.5, "profondeur": 0.8}, ' +
              '"fonctionnalites": ["wifi", "bluetooth", "nfc"]}';

  // Insérer dans la base de données
  FDQuery1.SQL.Text := 'INSERT INTO produits (id, nom, prix, caracteristiques) VALUES (:id, :nom, :prix, :json)';
  FDQuery1.ParamByName('id').AsInteger := 1;
  FDQuery1.ParamByName('nom').AsString := 'Smartphone X10';
  FDQuery1.ParamByName('prix').AsFloat := 599.99;
  FDQuery1.ParamByName('json').AsString := JsonData;
  FDQuery1.ExecSQL;
end;

procedure TForm1.RechercherSmartphonesNoirs;
begin
  // Rechercher avec une condition sur les données JSON
  FDQuery1.SQL.Text :=
    'SELECT id, nom, prix, caracteristiques ' +
    'FROM produits ' +
    'WHERE caracteristiques->>''$.couleur'' = ''noir''';
  FDQuery1.Open;

  // Afficher les résultats
  while not FDQuery1.Eof do
  begin
    Memo1.Lines.Add(Format('%d. %s - %.2f €',
      [FDQuery1.FieldByName('id').AsInteger,
       FDQuery1.FieldByName('nom').AsString,
       FDQuery1.FieldByName('prix').AsFloat]));

    // Extraire et afficher une propriété spécifique du JSON
    var CaracteristiquesJSON := FDQuery1.FieldByName('caracteristiques').AsString;
    var JSONObj := TJSONObject.ParseJSONValue(CaracteristiquesJSON) as TJSONObject;
    try
      if JSONObj.TryGetValue<string>('couleur', var Couleur) then
        Memo1.Lines.Add('  Couleur: ' + Couleur);

      // Accéder à un objet JSON imbriqué
      var DimensionsObj := JSONObj.GetValue<TJSONObject>('dimensions');
      if Assigned(DimensionsObj) then
      begin
        var Hauteur := DimensionsObj.GetValue<Double>('hauteur');
        var Largeur := DimensionsObj.GetValue<Double>('largeur');
        Memo1.Lines.Add(Format('  Dimensions: %.1f x %.1f cm', [Hauteur, Largeur]));
      end;
    finally
      JSONObj.Free;
    end;

    FDQuery1.Next;
  end;
end;
```

## Redis : Base de données clé-valeur

Redis est une base de données clé-valeur très rapide, qui stocke toutes les données en mémoire (avec persistance optionnelle sur disque).

### Installation et utilisation de Redis

1. Téléchargez et installez Redis depuis [redis.io](https://redis.io/download)
2. Utilisez la bibliothèque DelphiRedis ou TMS Redis pour vous connecter depuis Delphi

```delphi
// Exemple avec DelphiRedis
uses Redis.Client, Redis.Commons, Redis.NetLib.INDY;

procedure TForm1.UtiliserRedis;
var
  Redis: IRedisClient;
begin
  // Créer une connexion à Redis
  Redis := TRedisClient.Create('localhost', 6379);

  // Stocker une valeur
  Redis.SET('utilisateur:1:nom', 'Dupont');
  Redis.SET('utilisateur:1:prenom', 'Jean');
  Redis.SET('utilisateur:1:email', 'jean.dupont@exemple.com');

  // Stocker une valeur avec expiration (TTL)
  Redis.SETEX('session:123', 3600, 'utilisateur:1');  // Expire dans 1 heure

  // Récupérer une valeur
  var Nom := Redis.GET('utilisateur:1:nom');
  ShowMessage('Nom récupéré : ' + Nom);

  // Travailler avec des listes
  Redis.RPUSH('utilisateur:1:preferences', ['sport', 'lecture', 'cinema']);

  var Preferences := Redis.LRANGE('utilisateur:1:preferences', 0, -1);
  Memo1.Lines.Add('Préférences :');
  for var Pref in Preferences do
    Memo1.Lines.Add('- ' + Pref);

  // Incrémenter un compteur
  var NbVisites := Redis.INCR('compteur:visites');
  StatusBar1.SimpleText := 'Nombre de visites : ' + NbVisites.ToString;
end;
```

## Approche hybride : SQL + JSON

Une approche de plus en plus populaire consiste à utiliser une base de données SQL traditionnelle tout en stockant certaines données au format JSON. MySQL, PostgreSQL et SQL Server proposent tous un support natif du JSON.

### MySQL avec JSON

```delphi
procedure TForm1.UtiliserMySQLAvecJSON;
begin
  // Créer une table avec une colonne JSON
  FDConnection1.ExecSQL(
    'CREATE TABLE produits (' +
    '  id INT AUTO_INCREMENT PRIMARY KEY,' +
    '  nom VARCHAR(100) NOT NULL,' +
    '  prix DECIMAL(10,2) NOT NULL,' +
    '  specifications JSON' +
    ')'
  );

  // Insérer des données JSON
  FDQuery1.SQL.Text := 'INSERT INTO produits (nom, prix, specifications) VALUES (:nom, :prix, :specs)';
  FDQuery1.ParamByName('nom').AsString := 'Ordinateur portable';
  FDQuery1.ParamByName('prix').AsFloat := 999.99;
  FDQuery1.ParamByName('specs').AsString := '{"processeur": "Intel i7", "ram": 16, "stockage": {"type": "SSD", "capacite": 512}}';
  FDQuery1.ExecSQL;

  // Requête avec filtrage sur des données JSON
  FDQuery1.SQL.Text :=
    'SELECT id, nom, prix, specifications ' +
    'FROM produits ' +
    'WHERE JSON_EXTRACT(specifications, "$.processeur") = "Intel i7" ' +
    'AND JSON_EXTRACT(specifications, "$.ram") > 8';
  FDQuery1.Open;

  // Mise à jour d'une propriété JSON
  FDQuery1.SQL.Text :=
    'UPDATE produits ' +
    'SET specifications = JSON_SET(specifications, "$.ram", 32) ' +
    'WHERE id = :id';
  FDQuery1.ParamByName('id').AsInteger := 1;
  FDQuery1.ExecSQL;
end;
```

### PostgreSQL avec JSON

```delphi
procedure TForm1.UtiliserPostgreSQLAvecJSON;
begin
  // Créer une table avec une colonne JSONB
  FDConnection1.ExecSQL(
    'CREATE TABLE produits (' +
    '  id SERIAL PRIMARY KEY,' +
    '  nom VARCHAR(100) NOT NULL,' +
    '  prix DECIMAL(10,2) NOT NULL,' +
    '  specifications JSONB' +  // JSONB est plus efficace que JSON
    ')'
  );

  // Insérer des données JSON
  FDQuery1.SQL.Text := 'INSERT INTO produits (nom, prix, specifications) VALUES (:nom, :prix, :specs)';
  FDQuery1.ParamByName('nom').AsString := 'Smartphone';
  FDQuery1.ParamByName('prix').AsFloat := 699.99;
  FDQuery1.ParamByName('specs').AsString := '{"ecran": 6.5, "appareil_photo": {"principal": 48, "ultra_large": 12}, "couleurs": ["noir", "bleu", "rouge"]}';
  FDQuery1.ExecSQL;

  // Requête avec filtrage sur des données JSON
  FDQuery1.SQL.Text :=
    'SELECT id, nom, prix, specifications->''ecran'' AS taille_ecran ' +
    'FROM produits ' +
    'WHERE specifications->''ecran'' > ''6'' ' +
    'AND specifications@>''{"couleurs": ["noir"]}''';  // Contient la couleur noir
  FDQuery1.Open;

  // Mise à jour d'une propriété JSON
  FDQuery1.SQL.Text :=
    'UPDATE produits ' +
    'SET specifications = jsonb_set(specifications, ''{appareil_photo,principal}'', ''64'') ' +
    'WHERE id = :id';
  FDQuery1.ParamByName('id').AsInteger := 1;
  FDQuery1.ExecSQL;
end;
```

## Persistance d'objets avec SQL/JSON

Une approche pratique pour les applications Delphi consiste à utiliser la sérialisation JSON pour stocker et récupérer des objets directement.

```delphi
// Définition d'une classe avec sérialisation JSON
type
  TProduit = class
  private
    FID: Integer;
    FNom: string;
    FPrix: Double;
    FCouleur: string;
    FTaille: string;
    FCaracteristiques: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;

    // Propriétés publiées pour la sérialisation JSON
    property ID: Integer read FID write FID;
    property Nom: string read FNom write FNom;
    property Prix: Double read FPrix write FPrix;
    property Couleur: string read FCouleur write FCouleur;
    property Taille: string read FTaille write FTaille;
    property Caracteristiques: TDictionary<string, string> read FCaracteristiques;

    // Méthodes de sérialisation
    function ToJSON: string;
    procedure FromJSON(const JSONString: string);
  end;

// Implémentation
constructor TProduit.Create;
begin
  inherited;
  FCaracteristiques := TDictionary<string, string>.Create;
end;

destructor TProduit.Destroy;
begin
  FCaracteristiques.Free;
  inherited;
end;

function TProduit.ToJSON: string;
var
  JSONObj: TJSONObject;
  CaracObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    // Propriétés simples
    JSONObj.AddPair('id', TJSONNumber.Create(FID));
    JSONObj.AddPair('nom', FNom);
    JSONObj.AddPair('prix', TJSONNumber.Create(FPrix));
    JSONObj.AddPair('couleur', FCouleur);
    JSONObj.AddPair('taille', FTaille);

    // Dictionnaire des caractéristiques
    CaracObj := TJSONObject.Create;
    for var Pair in FCaracteristiques do
      CaracObj.AddPair(Pair.Key, Pair.Value);

    JSONObj.AddPair('caracteristiques', CaracObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TProduit.FromJSON(const JSONString: string);
var
  JSONObj: TJSONObject;
  CaracObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
  try
    // Propriétés simples
    if JSONObj.TryGetValue<Integer>('id', FID) then;
    if JSONObj.TryGetValue<string>('nom', FNom) then;
    if JSONObj.TryGetValue<Double>('prix', FPrix) then;
    if JSONObj.TryGetValue<string>('couleur', FCouleur) then;
    if JSONObj.TryGetValue<string>('taille', FTaille) then;

    // Dictionnaire des caractéristiques
    if JSONObj.TryGetValue<TJSONObject>('caracteristiques', CaracObj) then
    begin
      FCaracteristiques.Clear;
      for var Pair in CaracObj do
        FCaracteristiques.Add(Pair.JsonString.Value, Pair.JsonValue.Value);
    end;
  finally
    JSONObj.Free;
  end;
end;

// Utilisation avec une base de données relationnelle
procedure TForm1.EnregistrerProduit(Produit: TProduit);
begin
  FDQuery1.SQL.Text :=
    'INSERT INTO produits (nom, prix, donnees_json) VALUES (:nom, :prix, :json) ' +
    'RETURNING id';  // PostgreSQL syntax

  FDQuery1.ParamByName('nom').AsString := Produit.Nom;
  FDQuery1.ParamByName('prix').AsFloat := Produit.Prix;
  FDQuery1.ParamByName('json').AsString := Produit.ToJSON;

  FDQuery1.Open;
  Produit.ID := FDQuery1.FieldByName('id').AsInteger;
end;

procedure TForm1.ChargerProduit(ID: Integer; Produit: TProduit);
begin
  FDQuery1.SQL.Text := 'SELECT nom, prix, donnees_json FROM produits WHERE id = :id';
  FDQuery1.ParamByName('id').AsInteger := ID;
  FDQuery1.Open;

  if not FDQuery1.IsEmpty then
  begin
    Produit.ID := ID;
    Produit.Nom := FDQuery1.FieldByName('nom').AsString;
    Produit.Prix := FDQuery1.FieldByName('prix').AsFloat;
    Produit.FromJSON(FDQuery1.FieldByName('donnees_json').AsString);
  end;
end;
```

## Comparaison entre MongoDB et les solutions hybrides SQL+JSON

| Critère | MongoDB | SQL+JSON (MySQL, PostgreSQL, SQL Server) |
|---------|---------|------------------------------------------|
| Installation | Serveur séparé à installer | Souvent déjà disponible |
| Intégration avec Delphi | Via bibliothèques tierces | Native via FireDAC |
| Schéma | Totalement flexible | Semi-flexible |
| Requêtes | Syntaxe spécifique à MongoDB | SQL standard + extensions JSON |
| Transactions | Support limité dans les versions récentes | Support complet |
| Performance | Excellente pour les lectures/écritures simples | Bonne avec optimisations JSON |
| Mise à l'échelle | Conçu pour la distribution horizontale | Varie selon le SGBD |
| Maturité | Plus récent | Technologies bien établies |
| Support | Communauté active | Support commercial et communautaire |

## SQLite avec JSON : Une solution légère

SQLite (à partir de la version 3.9.0) offre également un support pour JSON à travers des fonctions spécifiques. C'est une solution idéale pour les applications qui ont besoin de flexibilité sans installer un serveur séparé.

```delphi
procedure TForm1.UtiliserSQLiteAvecJSON;
begin
  // Créer une table avec une colonne pour les données JSON
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS produits (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nom TEXT NOT NULL,' +
    '  prix REAL NOT NULL,' +
    '  caracteristiques TEXT' + // Stockera le JSON
    ')'
  );

  // Insérer des données avec JSON
  FDQuery1.SQL.Text := 'INSERT INTO produits (nom, prix, caracteristiques) VALUES (:nom, :prix, :caract)';
  FDQuery1.ParamByName('nom').AsString := 'Tablette';
  FDQuery1.ParamByName('prix').AsFloat := 349.99;
  FDQuery1.ParamByName('caract').AsString := '{"marque": "Tech+", "taille": 10.1, "os": "Android", "stockage": 64}';
  FDQuery1.ExecSQL;

  // Requête avec filtrage sur des données JSON
  FDQuery1.SQL.Text :=
    'SELECT id, nom, prix, json_extract(caracteristiques, ''$.taille'') AS taille_ecran ' +
    'FROM produits ' +
    'WHERE json_extract(caracteristiques, ''$.marque'') = ''Tech+'' ' +
    'AND json_extract(caracteristiques, ''$.stockage'') > 32';
  FDQuery1.Open;

  // Afficher les résultats
  while not FDQuery1.Eof do
  begin
    Memo1.Lines.Add(Format('%s - %.2f € (%.1f pouces)',
      [FDQuery1.FieldByName('nom').AsString,
       FDQuery1.FieldByName('prix').AsFloat,
       FDQuery1.FieldByName('taille_ecran').AsFloat]));
    FDQuery1.Next;
  end;
end;
```

## Quand utiliser quelle approche ?

Voici quelques conseils pour choisir la bonne solution en fonction de vos besoins :

### Utilisez MongoDB quand...

- Vous avez des données très hétérogènes (structure variable)
- Vous avez besoin d'une grande flexibilité de schéma
- Vous avez de gros volumes de données à distribuer
- Vos besoins en lecture/écriture sont simples (peu de jointures complexes)
- Vous pouvez installer et maintenir un serveur MongoDB

### Utilisez une solution SQL+JSON (MySQL, PostgreSQL, SQL Server) quand...

- Vous avez besoin de combiner données structurées et non structurées
- Vous voulez conserver les avantages des transactions ACID
- Vous avez déjà une infrastructure SQL en place
- Vous avez besoin de requêtes complexes (jointures, agrégations)
- Vous préférez rester dans l'écosystème FireDAC

### Utilisez SQLite+JSON quand...

- Vous développez une application locale ou mobile
- Vous n'avez pas besoin d'un serveur de base de données
- Vous voulez une solution légère et portable
- Vous avez des volumes de données modérés

### Utilisez Redis quand...

- Vous avez besoin de performances extrêmes
- Vous utilisez principalement des paires clé-valeur
- Vous avez besoin de fonctionnalités comme les files d'attente, pub/sub, etc.
- Vous pouvez tolérer une perte potentielle de données (selon configuration)

## Bonnes pratiques pour travailler avec des données JSON

Que vous utilisiez MongoDB ou une solution SQL+JSON, voici quelques bonnes pratiques :

### 1. Organisation du modèle de données

```delphi
// Définir des constantes pour les noms de champs JSON
const
  JSON_FIELD_ID = 'id';
  JSON_FIELD_NAME = 'nom';
  JSON_FIELD_PRICE = 'prix';
  JSON_FIELD_SPECS = 'specifications';
  JSON_FIELD_BRAND = 'marque';
  JSON_FIELD_COLOR = 'couleur';

// Créer un objet JSON avec des champs constants
function CreateProductJSON(const Name: string; Price: Double;
                           const Brand, Color: string): string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair(JSON_FIELD_NAME, Name);
    JSONObj.AddPair(JSON_FIELD_PRICE, TJSONNumber.Create(Price));

    var SpecsObj := TJSONObject.Create;
    SpecsObj.AddPair(JSON_FIELD_BRAND, Brand);
    SpecsObj.AddPair(JSON_FIELD_COLOR, Color);

    JSONObj.AddPair(JSON_FIELD_SPECS, SpecsObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;
```

### 2. Validation des données JSON

```delphi
function IsValidProductJSON(const JSONString: string): Boolean;
var
  JSONObj: TJSONObject;
  RequiredFields: TArray<string>;
  Field: string;
begin
  Result := False;

  try
    JSONObj := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
    if not Assigned(JSONObj) then
      Exit;

    try
      // Définir les champs requis
      RequiredFields := [JSON_FIELD_NAME, JSON_FIELD_PRICE, JSON_FIELD_SPECS];

      // Vérifier que tous les champs requis sont présents
      for Field in RequiredFields do
        if not JSONObj.TryGetValue<TJSONValue>(Field, var Value) then
          Exit;

      // Vérifier que le champ specifications est un objet JSON
      var SpecsObj: TJSONObject;
      if not (JSONObj.TryGetValue<TJSONObject>(JSON_FIELD_SPECS, SpecsObj) and
              Assigned(SpecsObj)) then
        Exit;

      // Vérifier que les champs spécifiques sont présents dans specifications
      if not (SpecsObj.TryGetValue<string>(JSON_FIELD_BRAND, var BrandValue) and
              SpecsObj.TryGetValue<string>(JSON_FIELD_COLOR, var ColorValue)) then
        Exit;

      Result := True;
    finally
      JSONObj.Free;
    end;
  except
    Result := False;
  end;
end;
```

### 3. Encapsulation des opérations JSON dans une classe

```delphi
// UJSONProductRepository.pas
unit UJSONProductRepository;

interface

uses
  System.SysUtils, System.Classes, System.JSON, FireDAC.Comp.Client;

type
  TProduct = class
  public
    ID: Integer;
    Name: string;
    Price: Double;
    Brand: string;
    Color: string;

    function ToJSON: string;
    procedure FromJSON(const JSONString: string);
  end;

  TJSONProductRepository = class
  private
    FConnection: TFDConnection;
    FTableName: string;
  public
    constructor Create(AConnection: TFDConnection; const ATableName: string = 'produits');

    function Add(Product: TProduct): Integer;
    function Update(Product: TProduct): Boolean;
    function Delete(ID: Integer): Boolean;
    function GetByID(ID: Integer; Product: TProduct): Boolean;
    function GetByBrand(const Brand: string): TArray<TProduct>;
    function GetAll: TArray<TProduct>;
  end;

implementation

// Implémentation de TProduct
function TProduct.ToJSON: string;
var
  JSONObj: TJSONObject;
begin
  JSONObj := TJSONObject.Create;
  try
    JSONObj.AddPair('nom', Name);
    JSONObj.AddPair('prix', TJSONNumber.Create(Price));

    var SpecsObj := TJSONObject.Create;
    SpecsObj.AddPair('marque', Brand);
    SpecsObj.AddPair('couleur', Color);

    JSONObj.AddPair('specifications', SpecsObj);

    Result := JSONObj.ToString;
  finally
    JSONObj.Free;
  end;
end;

procedure TProduct.FromJSON(const JSONString: string);
var
  JSONObj: TJSONObject;
  SpecsObj: TJSONObject;
begin
  JSONObj := TJSONObject.ParseJSONValue(JSONString) as TJSONObject;
  if not Assigned(JSONObj) then
    Exit;

  try
    if JSONObj.TryGetValue<string>('nom', Name) then;
    if JSONObj.TryGetValue<Double>('prix', Price) then;

    if JSONObj.TryGetValue<TJSONObject>('specifications', SpecsObj) then
    begin
      if SpecsObj.TryGetValue<string>('marque', Brand) then;
      if SpecsObj.TryGetValue<string>('couleur', Color) then;
    end;
  finally
    JSONObj.Free;
  end;
end;

// Implémentation de TJSONProductRepository
constructor TJSONProductRepository.Create(AConnection: TFDConnection; const ATableName: string);
begin
  inherited Create;
  FConnection := AConnection;
  FTableName := ATableName;

  // Créer la table si elle n'existe pas (pour SQLite)
  if FConnection.DriverName = 'SQLite' then
  begin
    FConnection.ExecSQL(
      'CREATE TABLE IF NOT EXISTS ' + FTableName + ' (' +
      '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '  donnees_json TEXT NOT NULL' +
      ')'
    );
  end;
end;

function TJSONProductRepository.Add(Product: TProduct): Integer;
var
  Query: TFDQuery;
begin
  Result := -1;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;

    // La syntaxe varie selon le SGBD
    case FConnection.DriverName of
      'SQLite':
        begin
          Query.SQL.Text := 'INSERT INTO ' + FTableName + ' (donnees_json) VALUES (:json); ' +
                            'SELECT last_insert_rowid() AS id';
        end;
      'MySQL':
        begin
          Query.SQL.Text := 'INSERT INTO ' + FTableName + ' (donnees_json) VALUES (:json); ' +
                            'SELECT LAST_INSERT_ID() AS id';
        end;
      'PG':
        begin
          Query.SQL.Text := 'INSERT INTO ' + FTableName + ' (donnees_json) VALUES (:json) ' +
                            'RETURNING id';
        end;
      'MSSQL':
        begin
          Query.SQL.Text := 'INSERT INTO ' + FTableName + ' (donnees_json) VALUES (:json); ' +
                            'SELECT SCOPE_IDENTITY() AS id';
        end;
    end;

    Query.ParamByName('json').AsString := Product.ToJSON;
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('id').AsInteger;

    Product.ID := Result;
  finally
    Query.Free;
  end;
end;

function TJSONProductRepository.Update(Product: TProduct): Boolean;
var
  Query: TFDQuery;
begin
  if Product.ID <= 0 then
    Exit(False);

  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'UPDATE ' + FTableName + ' SET donnees_json = :json WHERE id = :id';
    Query.ParamByName('json').AsString := Product.ToJSON;
    Query.ParamByName('id').AsInteger := Product.ID;
    Query.ExecSQL;

    Result := Query.RowsAffected > 0;
  finally
    Query.Free;
  end;
end;

function TJSONProductRepository.Delete(ID: Integer): Boolean;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'DELETE FROM ' + FTableName + ' WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.ExecSQL;

    Result := Query.RowsAffected > 0;
  finally
    Query.Free;
  end;
end;

function TJSONProductRepository.GetByID(ID: Integer; Product: TProduct): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT donnees_json FROM ' + FTableName + ' WHERE id = :id';
    Query.ParamByName('id').AsInteger := ID;
    Query.Open;

    if not Query.IsEmpty then
    begin
      Product.ID := ID;
      Product.FromJSON(Query.FieldByName('donnees_json').AsString);
      Result := True;
    end;
  finally
    Query.Free;
  end;
end;

function TJSONProductRepository.GetByBrand(const Brand: string): TArray<TProduct>;
var
  Query: TFDQuery;
  Products: TList<TProduct>;
  JsonPath: string;
begin
  Products := TList<TProduct>.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;

      // La syntaxe varie selon le SGBD
      case FConnection.DriverName of
        'SQLite':
          JsonPath := 'json_extract(donnees_json, ''$.specifications.marque'')';
        'MySQL':
          JsonPath := 'JSON_EXTRACT(donnees_json, ''$.specifications.marque'')';
        'PG':
          JsonPath := 'donnees_json->''specifications''->''marque''';
        'MSSQL':
          JsonPath := 'JSON_VALUE(donnees_json, ''$.specifications.marque'')';
      end;

      Query.SQL.Text := 'SELECT id, donnees_json FROM ' + FTableName +
                        ' WHERE ' + JsonPath + ' = :brand';
      Query.ParamByName('brand').AsString := Brand;
      Query.Open;

      while not Query.Eof do
      begin
        var Product := TProduct.Create;
        Product.ID := Query.FieldByName('id').AsInteger;
        Product.FromJSON(Query.FieldByName('donnees_json').AsString);
        Products.Add(Product);
        Query.Next;
      end;

      Result := Products.ToArray;
    finally
      Query.Free;
    end;
  finally
    // Ne pas libérer les produits individuels, c'est la responsabilité de l'appelant
    Products.Free;
  end;
end;

function TJSONProductRepository.GetAll: TArray<TProduct>;
var
  Query: TFDQuery;
  Products: TList<TProduct>;
begin
  Products := TList<TProduct>.Create;
  try
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FConnection;
      Query.SQL.Text := 'SELECT id, donnees_json FROM ' + FTableName;
      Query.Open;

      while not Query.Eof do
      begin
        var Product := TProduct.Create;
        Product.ID := Query.FieldByName('id').AsInteger;
        Product.FromJSON(Query.FieldByName('donnees_json').AsString);
        Products.Add(Product);
        Query.Next;
      end;

      Result := Products.ToArray;
    finally
      Query.Free;
    end;
  finally
    Products.Free;
  end;
end;

end.
```

### 4. Utilisation du repository

```delphi
procedure TForm1.ButtonAjouterClick(Sender: TObject);
var
  Repository: TJSONProductRepository;
  Product: TProduct;
begin
  Repository := TJSONProductRepository.Create(FDConnection1);
  try
    Product := TProduct.Create;
    try
      // Remplir les données du produit
      Product.Name := EditNom.Text;
      Product.Price := StrToFloatDef(EditPrix.Text, 0);
      Product.Brand := ComboBoxMarque.Text;
      Product.Color := ComboBoxCouleur.Text;

      // Ajouter le produit
      var NewID := Repository.Add(Product);
      if NewID > 0 then
      begin
        ShowMessage('Produit ajouté avec succès ! ID: ' + IntToStr(NewID));
        RafraichirListe;
      end
      else
        ShowMessage('Erreur lors de l''ajout du produit');
    finally
      Product.Free;
    end;
  finally
    Repository.Free;
  end;
end;

procedure TForm1.RafraichirListe;
var
  Repository: TJSONProductRepository;
  Products: TArray<TProduct>;
  I: Integer;
begin
  ListView1.Items.Clear;

  Repository := TJSONProductRepository.Create(FDConnection1);
  try
    Products := Repository.GetAll;
    try
      for I := 0 to High(Products) do
      begin
        var Item := ListView1.Items.Add;
        Item.Caption := Products[I].Name;
        Item.SubItems.Add(FormatFloat('#,##0.00 €', Products[I].Price));
        Item.SubItems.Add(Products[I].Brand);
        Item.SubItems.Add(Products[I].Color);
        Item.Data := Pointer(Products[I].ID); // Stocker l'ID pour référence ultérieure
      end;
    finally
      for I := 0 to High(Products) do
        Products[I].Free;
    end;
  finally
    Repository.Free;
  end;
end;
```

## Migration d'une base relationnelle vers NoSQL ou hybride

Si vous avez une application existante avec une base de données relationnelle et que vous souhaitez migrer vers une approche NoSQL ou hybride, voici quelques étapes à suivre :

### 1. Analysez votre modèle de données actuel

```delphi
procedure TForm1.AnalyserSchema;
var
  Tables: TFDMetaInfoQuery;
  Colonnes: TFDMetaInfoQuery;
begin
  // Récupérer la liste des tables
  Tables := TFDMetaInfoQuery.Create(nil);
  try
    Tables.Connection := FDConnection1;
    Tables.MetaInfoKind := mkTables;
    Tables.ObjectScopes := [osMy, osOther];
    Tables.Open;

    Memo1.Lines.Add('=== Tables trouvées dans la base ===');
    while not Tables.Eof do
    begin
      var TableName := Tables.FieldByName('TABLE_NAME').AsString;
      Memo1.Lines.Add('Table: ' + TableName);

      // Récupérer les colonnes de cette table
      Colonnes := TFDMetaInfoQuery.Create(nil);
      try
        Colonnes.Connection := FDConnection1;
        Colonnes.MetaInfoKind := mkTableFields;
        Colonnes.ObjectName := TableName;
        Colonnes.Open;

        while not Colonnes.Eof do
        begin
          Memo1.Lines.Add(Format('  - %s (%s)',
            [Colonnes.FieldByName('COLUMN_NAME').AsString,
             Colonnes.FieldByName('COLUMN_TYPENAME').AsString]));
          Colonnes.Next;
        end;
      finally
        Colonnes.Free;
      end;

      Memo1.Lines.Add('');
      Tables.Next;
    end;
  finally
    Tables.Free;
  end;
end;
```

### 2. Identifiez les candidats pour la conversion en JSON

Recherchez dans votre schéma relationnel :
- Tables avec beaucoup de colonnes optionnelles (NULL)
- Tables qui changent fréquemment de structure
- Tables avec des données semi-structurées
- Groupes de tables étroitement liées qui sont souvent consultées ensemble

### 3. Créez une stratégie de migration

Plusieurs approches sont possibles :

#### a. Migration complète vers MongoDB

```delphi
procedure TForm1.MigrerVersMongoDBClick(Sender: TObject);
var
  MongoClient: TMongoClient;
  Database: TMongoDatabase;
  Collection: TMongoCollection;
  Query: TFDQuery;
begin
  // Connexion à MongoDB
  MongoClient := TMongoClient.Create('mongodb://localhost:27017');
  try
    Database := MongoClient.GetDatabase('ma_nouvelle_base');
    Collection := Database.GetCollection('produits');

    // Récupérer les données depuis la base SQL
    Query := TFDQuery.Create(nil);
    try
      Query.Connection := FDConnection1;
      Query.SQL.Text := 'SELECT * FROM produits';
      Query.Open;

      ProgressBar1.Max := Query.RecordCount;
      ProgressBar1.Position := 0;

      while not Query.Eof do
      begin
        // Créer un document MongoDB pour chaque enregistrement SQL
        var Document := TMongoDocument.Create;

        // Ajouter tous les champs
        for var I := 0 to Query.FieldCount - 1 do
        begin
          var Field := Query.Fields[I];

          case Field.DataType of
            ftString, ftWideString, ftMemo, ftWideMemo:
              Document.Add(Field.FieldName, Field.AsString);

            ftInteger, ftSmallint, ftLargeint:
              Document.Add(Field.FieldName, Field.AsInteger);

            ftFloat, ftCurrency, ftBCD:
              Document.Add(Field.FieldName, Field.AsFloat);

            ftBoolean:
              Document.Add(Field.FieldName, Field.AsBoolean);

            ftDate, ftDateTime, ftTimeStamp:
              Document.Add(Field.FieldName, Field.AsDateTime);

            // Autres types...
          end;
        end;

        // Insérer dans MongoDB
        Collection.InsertOne(Document);

        ProgressBar1.Position := ProgressBar1.Position + 1;
        Application.ProcessMessages;

        Query.Next;
      end;

      ShowMessage('Migration terminée ! ' + IntToStr(ProgressBar1.Position) + ' enregistrements migrés.');
    finally
      Query.Free;
    end;
  finally
    MongoClient.Free;
  end;
end;
```

#### b. Conversion vers un modèle hybride SQL+JSON

```delphi
procedure TForm1.ConvertirVersHybrideClick(Sender: TObject);
var
  QuerySource, QueryDest: TFDQuery;
begin
  // Créer la nouvelle table avec colonne JSON
  FDConnection1.ExecSQL(
    'CREATE TABLE produits_new (' +
    '  id INT PRIMARY KEY,' +
    '  nom VARCHAR(100) NOT NULL,' +
    '  prix DECIMAL(10,2),' +
    '  details JSON' +  // Stockera les autres champs en JSON
    ')'
  );

  // Lire les données existantes
  QuerySource := TFDQuery.Create(nil);
  QueryDest := TFDQuery.Create(nil);
  try
    QuerySource.Connection := FDConnection1;
    QueryDest.Connection := FDConnection1;

    QuerySource.SQL.Text := 'SELECT * FROM produits';
    QuerySource.Open;

    QueryDest.SQL.Text := 'INSERT INTO produits_new (id, nom, prix, details) VALUES (:id, :nom, :prix, :details)';

    FDConnection1.StartTransaction;
    try
      while not QuerySource.Eof do
      begin
        // Créer un objet JSON pour les détails
        var JSONObj := TJSONObject.Create;
        try
          // Ajouter les champs non-primaires au JSON
          for var I := 0 to QuerySource.FieldCount - 1 do
          begin
            var Field := QuerySource.Fields[I];
            var FieldName := Field.FieldName.ToLower;

            // Exclure les champs qui sont déjà dans la structure principale
            if not (FieldName = 'id' or FieldName = 'nom' or FieldName = 'prix') then
            begin
              case Field.DataType of
                ftString, ftWideString, ftMemo, ftWideMemo:
                  if not Field.IsNull then
                    JSONObj.AddPair(Field.FieldName, Field.AsString);

                ftInteger, ftSmallint, ftLargeint:
                  if not Field.IsNull then
                    JSONObj.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsInteger));

                ftFloat, ftCurrency, ftBCD:
                  if not Field.IsNull then
                    JSONObj.AddPair(Field.FieldName, TJSONNumber.Create(Field.AsFloat));

                ftBoolean:
                  if not Field.IsNull then
                    JSONObj.AddPair(Field.FieldName, TJSONBool.Create(Field.AsBoolean));

                ftDate, ftDateTime, ftTimeStamp:
                  if not Field.IsNull then
                    JSONObj.AddPair(Field.FieldName, FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Field.AsDateTime));

                // Autres types...
              end;
            end;
          end;

          // Insérer dans la nouvelle table
          QueryDest.ParamByName('id').AsInteger := QuerySource.FieldByName('id').AsInteger;
          QueryDest.ParamByName('nom').AsString := QuerySource.FieldByName('nom').AsString;
          QueryDest.ParamByName('prix').AsFloat := QuerySource.FieldByName('prix').AsFloat;
          QueryDest.ParamByName('details').AsString := JSONObj.ToString;
          QueryDest.ExecSQL;
        finally
          JSONObj.Free;
        end;

        QuerySource.Next;
      end;

      FDConnection1.Commit;
      ShowMessage('Conversion terminée avec succès !');
    except
      on E: Exception do
      begin
        FDConnection1.Rollback;
        ShowMessage('Erreur lors de la conversion : ' + E.Message);
      end;
    end;
  finally
    QuerySource.Free;
    QueryDest.Free;
  end;
end;
```

## Conclusion

Les bases de données NoSQL et documentaires offrent une flexibilité précieuse pour de nombreux scénarios de développement. Que vous optiez pour une solution NoSQL pure comme MongoDB, une approche hybride SQL+JSON, ou une base de données embarquée comme SQLite avec JSON, Delphi vous permet de travailler efficacement avec ces technologies.

Principaux points à retenir :

1. **Les bases NoSQL** sont idéales pour les données non structurées ou semi-structurées et les applications nécessitant une grande évolutivité.

2. **Les approches hybrides SQL+JSON** offrent un bon compromis entre la flexibilité du NoSQL et les garanties transactionnelles des bases relationnelles.

3. **L'intégration avec Delphi** est plus directe pour les solutions SQL+JSON via FireDAC, tandis que les bases NoSQL pures nécessitent généralement des bibliothèques tierces.

4. **Le choix de la solution** dépend de vos besoins spécifiques : volume de données, structure, besoins en transactions, infrastructure existante, etc.

5. **La migration** d'une base relationnelle vers NoSQL ou hybride peut être progressive et ciblée sur les parties du modèle de données qui bénéficieront le plus de cette flexibilité.

En explorant ces différentes approches, vous pourrez choisir la solution la plus adaptée à vos besoins tout en profitant de la puissance et de la flexibilité de Delphi pour le développement d'applications de gestion de données.

---

**À suivre :** 9.1 Composants d'impression natifs

⏭️ [Rapports et impressions](09-rapports-et-impressions/README.md)
