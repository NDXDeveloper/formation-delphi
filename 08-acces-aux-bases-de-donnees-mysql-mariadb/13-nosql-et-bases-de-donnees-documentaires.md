🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.13 NoSQL et bases de données documentaires

## Introduction

Jusqu'à présent, nous avons travaillé exclusivement avec des bases de données **relationnelles** (MySQL, PostgreSQL, SQL Server, SQLite). Ces bases organisent les données en **tables** avec des **lignes** et des **colonnes**, et utilisent le langage **SQL** pour les manipuler.

Mais depuis les années 2000, un nouveau type de bases de données a émergé : les bases **NoSQL** (Not Only SQL). Elles adoptent une approche radicalement différente pour stocker et gérer les données, particulièrement adaptée aux besoins modernes du web et des applications distribuées.

Dans ce chapitre, nous allons découvrir le monde NoSQL et comprendre quand et comment l'utiliser avec Delphi.

## Qu'est-ce que le NoSQL ?

### Définition

**NoSQL** ne signifie pas "No SQL" (pas de SQL) mais plutôt **"Not Only SQL"** (pas seulement SQL). Ce sont des bases de données qui :
- N'utilisent **pas de tables** traditionnelles
- Ne nécessitent **pas de schéma fixe**
- Ne reposent **pas sur les jointures** SQL
- Privilégient la **scalabilité horizontale**

### Pourquoi le NoSQL est né ?

```
┌─────────────────────────────────────────┐
│  Problèmes des bases relationnelles     │
│  pour certains cas d'usage modernes     │
├─────────────────────────────────────────┤
│  • Schéma rigide et difficile à faire   │
│    évoluer                              │
│  • Difficile à distribuer sur plusieurs │
│    serveurs (scalabilité horizontale)   │
│  • Performances limitées pour volumes   │
│    massifs (Big Data)                   │
│  • Jointures coûteuses pour données     │
│    complexes imbriquées                 │
└─────────────────────────────────────────┘
                    ↓
         Émergence du NoSQL
```

**Contexte historique :**
- **2000s** : Explosion du web (Google, Facebook, Amazon)
- **Volumes massifs** : milliards d'utilisateurs, pétaoctets de données
- **Besoin de scalabilité** : distribuer sur des milliers de serveurs
- **Données variées** : documents, graphes, séries temporelles

## SQL vs NoSQL : Les différences fondamentales

### Modèle de données

#### Base SQL (Relationnelle)

```
┌──────────────────────────────────────────┐
│  Table : clients                         │
├────┬─────────┬──────────┬────────────────┤
│ id │ nom     │ prenom   │ email          │
├────┼─────────┼──────────┼────────────────┤
│ 1  │ Dupont  │ Jean     │ jean@email.fr  │
│ 2  │ Martin  │ Sophie   │ sophie@...     │
└────┴─────────┴──────────┴────────────────┘
           ↓ jointure
┌──────────────────────────────────────────┐
│  Table : commandes                       │
├────┬────────────┬──────────┬─────────────┤
│ id │ client_id  │ date     │ total       │
├────┼────────────┼──────────┼─────────────┤
│ 1  │ 1          │ 2024...  │ 150.00      │
└────┴────────────┴──────────┴─────────────┘
```

**Caractéristiques :**
- Données organisées en **tables**
- Schéma **fixe** (colonnes prédéfinies)
- Relations via **clés étrangères**
- Requêtes avec **jointures**

#### Base NoSQL (Documentaire)

```json
// Document : un client avec ses commandes
{
  "_id": "507f1f77bcf86cd799439011",
  "nom": "Dupont",
  "prenom": "Jean",
  "email": "jean@email.fr",
  "commandes": [
    {
      "date": "2024-03-15",
      "total": 150.00,
      "articles": [
        {"produit": "Livre", "prix": 20.00},
        {"produit": "Stylo", "prix": 2.50}
      ]
    }
  ],
  "adresses": [
    {
      "type": "livraison",
      "rue": "10 rue des Fleurs",
      "ville": "Paris"
    }
  ]
}
```

**Caractéristiques :**
- Données organisées en **documents** (JSON/BSON)
- Schéma **flexible** (chaque document peut être différent)
- Données **imbriquées** (pas de jointures)
- Requêtes sur les documents

### Tableau comparatif

| Aspect | SQL (Relationnel) | NoSQL (Documentaire) |
|--------|-------------------|----------------------|
| **Structure** | Tables, lignes, colonnes | Collections, documents |
| **Schéma** | Fixe, prédéfini | Flexible, dynamique |
| **Relations** | Clés étrangères, jointures | Imbrication, références |
| **Requêtes** | SQL | Opérations sur JSON |
| **Scalabilité** | Verticale (⬆️ serveur plus puissant) | Horizontale (➡️ plus de serveurs) |
| **Transactions** | ACID (strict) | BASE (éventuel) |
| **Cas d'usage** | Données structurées, relations | Données variables, hiérarchiques |

## Les 4 types de bases NoSQL

### 1. Bases documentaires

**Principe :** Stocker des documents (JSON, XML, BSON).

**Exemples :** MongoDB, CouchDB, Couchbase

```json
// Document MongoDB
{
  "_id": "123",
  "type": "client",
  "nom": "Dupont",
  "adresses": [
    {"type": "domicile", "ville": "Paris"},
    {"type": "travail", "ville": "Lyon"}
  ]
}
```

**Cas d'usage :**
- Applications web modernes
- Catalogues de produits
- Gestion de contenu (CMS)
- Profils utilisateurs

### 2. Bases clé-valeur

**Principe :** Stockage simple paire clé → valeur.

**Exemples :** Redis, Memcached, DynamoDB

```
clé: "session:user123"  
valeur: {nom: "Dupont", connecté: true}  

clé: "cache:page_accueil"  
valeur: "<html>...</html>"  
```

**Cas d'usage :**
- Cache applicatif
- Sessions utilisateur
- Files d'attente
- Compteurs temps réel

### 3. Bases en colonnes

**Principe :** Données organisées par colonnes plutôt que par lignes.

**Exemples :** Cassandra, HBase

```
Famille de colonnes : utilisateurs
┌──────────┬─────────────┬──────────────┐
│ Row Key  │ nom:prenom  │ contact:email│
├──────────┼─────────────┼──────────────┤
│ user1    │ Jean Dupont │ jean@...     │
│ user2    │ Sophie ...  │ sophie@...   │
└──────────┴─────────────┴──────────────┘
```

**Cas d'usage :**
- Big Data
- Séries temporelles
- Analytique massive
- IoT (Internet des Objets)

### 4. Bases orientées graphes

**Principe :** Données organisées en nœuds et relations.

**Exemples :** Neo4j, ArangoDB

```
(Jean)-[:AMI_DE]->(Sophie)
  │
  └─[:TRAVAILLE_POUR]->(Entreprise A)
                           │
                    (Sophie)-[:TRAVAILLE_POUR]
```

**Cas d'usage :**
- Réseaux sociaux
- Moteurs de recommandation
- Détection de fraudes
- Gestion de connaissances

## Focus : Bases de données documentaires

Les bases **documentaires** sont les plus populaires et les plus accessibles pour débuter avec NoSQL. Concentrons-nous sur **MongoDB**, le leader du marché.

### MongoDB : Vue d'ensemble

**MongoDB** est une base de données documentaire open-source qui stocke les données au format **BSON** (Binary JSON).

**Caractéristiques :**
- Documents JSON/BSON
- Schéma flexible
- Requêtes puissantes
- Indexation performante
- Réplication et sharding natifs
- Scalabilité horizontale

### Concepts de MongoDB

| Concept SQL | Équivalent MongoDB | Description |
|-------------|-------------------|-------------|
| **Base de données** | Base de données | Conteneur de collections |
| **Table** | Collection | Groupe de documents |
| **Ligne** | Document | Enregistrement JSON/BSON |
| **Colonne** | Champ | Propriété du document |
| **Index** | Index | Identique |
| **Jointure** | Lookup / Embedding | Référence ou imbrication |

### Structure d'un document MongoDB

```json
{
  "_id": ObjectId("507f1f77bcf86cd799439011"),  // ID unique généré auto
  "nom": "Dupont",
  "prenom": "Jean",
  "age": 35,
  "actif": true,
  "adresses": [                                  // Tableau imbriqué
    {
      "type": "domicile",
      "rue": "10 rue des Fleurs",
      "codePostal": "75001",
      "ville": "Paris"
    },
    {
      "type": "travail",
      "rue": "5 avenue de la République",
      "ville": "Lyon"
    }
  ],
  "commandes": [                                 // Références ou imbrication
    {
      "numero": "CMD001",
      "date": ISODate("2024-03-15T10:00:00Z"),
      "total": 150.00,
      "articles": [
        {"nom": "Livre", "quantite": 2, "prix": 20.00},
        {"nom": "Stylo", "quantite": 5, "prix": 2.00}
      ]
    }
  ],
  "preferences": {                               // Objet imbriqué
    "newsletter": true,
    "langue": "fr",
    "theme": "sombre"
  },
  "tags": ["premium", "actif", "verified"],     // Tableau simple
  "dateCreation": ISODate("2024-01-01T00:00:00Z"),
  "derniereConnexion": ISODate("2024-03-20T15:30:00Z")
}
```

**Avantages du document :**
- Toutes les données liées sont **au même endroit**
- Pas de jointure nécessaire
- Structure qui reflète les objets de l'application
- Flexible : chaque document peut avoir des champs différents

## MongoDB avec Delphi

### Installation de MongoDB

#### Windows

1. Téléchargez depuis [mongodb.com/try/download/community](https://www.mongodb.com/try/download/community)
2. Installez MongoDB Community Server
3. Démarrez le service MongoDB
4. Port par défaut : `27017`

#### Vérifier l'installation

```bash
# Ligne de commande MongoDB Shell
mongosh

# Dans le shell
> show dbs
admin   0.000GB  
config  0.000GB  
local   0.000GB  
```

### Connexion à MongoDB depuis Delphi

FireDAC supporte MongoDB natif. Voici comment configurer la connexion :

```pascal
uses
  FireDAC.Comp.Client, FireDAC.Phys.MongoDB, FireDAC.Phys.MongoDBDef;

procedure TdmDatabase.ConfigurerMongoDB;  
begin  
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('DriverID=Mongo');
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Port=27017');
  FDConnection1.Params.Add('Database=ma_base');

  // Optionnel : authentification
  // FDConnection1.Params.Add('User_Name=admin');
  // FDConnection1.Params.Add('Password=motdepasse');

  FDConnection1.LoginPrompt := False;
  FDConnection1.Connected := True;
end;
```

### Composants nécessaires

```pascal
// Sur le DataModule
TFDConnection: FDConnection1  
TFDPhysMongoDriverLink: FDPhysMongoDriverLink1  // Pilote MongoDB  
TFDMongoQuery: FDMongoQuery1  // Requête MongoDB (spécifique)  
```

### Opérations CRUD avec MongoDB

#### 1. Créer (Insert)

```pascal
procedure TdmDatabase.AjouterDocument;  
var  
  MongoQuery: TFDMongoQuery;
  JSONDoc: string;
begin
  MongoQuery := TFDMongoQuery.Create(nil);
  try
    MongoQuery.Connection := FDConnection1;

    // Créer un document JSON
    JSONDoc :=
      '{' +
      '  "nom": "Dupont",' +
      '  "prenom": "Jean",' +
      '  "email": "jean@email.fr",' +
      '  "age": 35,' +
      '  "actif": true,' +
      '  "adresses": [' +
      '    {"type": "domicile", "ville": "Paris"},' +
      '    {"type": "travail", "ville": "Lyon"}' +
      '  ]' +
      '}';

    // Insérer dans la collection "clients"
    MongoQuery.DatabaseName := 'ma_base';
    MongoQuery.CollectionName := 'clients';
    MongoQuery.QInsert := JSONDoc;
    MongoQuery.Execute;

    ShowMessage('Document inséré');

  finally
    MongoQuery.Free;
  end;
end;
```

#### 2. Lire (Find)

```pascal
procedure TdmDatabase.LireTousLesDocuments;  
var  
  MongoQuery: TFDMongoQuery;
begin
  MongoQuery := TFDMongoQuery.Create(nil);
  try
    MongoQuery.Connection := FDConnection1;
    MongoQuery.DatabaseName := 'ma_base';
    MongoQuery.CollectionName := 'clients';

    // Rechercher tous les documents
    MongoQuery.QFind := '{}';  // {} = tous les documents
    MongoQuery.Open;

    // Parcourir les résultats
    while not MongoQuery.Eof do
    begin
      ShowMessage(
        'Nom : ' + MongoQuery.FieldByName('nom').AsString + ' ' +
        MongoQuery.FieldByName('prenom').AsString
      );
      MongoQuery.Next;
    end;

  finally
    MongoQuery.Free;
  end;
end;

// Recherche avec filtre
procedure TdmDatabase.RechercherParNom(const Nom: string);  
begin  
  MongoQuery.QFind := Format('{"nom": "%s"}', [Nom]);
  MongoQuery.Open;
end;

// Recherche avec condition
procedure TdmDatabase.RechercherActifs;  
begin  
  MongoQuery.QFind := '{"actif": true}';
  MongoQuery.Open;
end;

// Recherche avec opérateur
procedure TdmDatabase.RechercherParAge;  
begin  
  // Âge >= 18
  MongoQuery.QFind := '{"age": {"$gte": 18}}';
  MongoQuery.Open;
end;
```

#### 3. Mettre à jour (Update)

```pascal
procedure TdmDatabase.ModifierDocument;  
var  
  MongoQuery: TFDMongoQuery;
begin
  MongoQuery := TFDMongoQuery.Create(nil);
  try
    MongoQuery.Connection := FDConnection1;
    MongoQuery.DatabaseName := 'ma_base';
    MongoQuery.CollectionName := 'clients';

    // Critère de recherche
    MongoQuery.QFind := '{"nom": "Dupont"}';

    // Modification
    MongoQuery.QUpdate := '{"$set": {"email": "nouveau@email.fr", "age": 36}}';

    MongoQuery.Execute;

    ShowMessage('Document(s) modifié(s)');

  finally
    MongoQuery.Free;
  end;
end;

// Ajouter un élément à un tableau
procedure TdmDatabase.AjouterAdresse;  
begin  
  MongoQuery.QFind := '{"nom": "Dupont"}';
  MongoQuery.QUpdate :=
    '{"$push": {' +
    '  "adresses": {' +
    '    "type": "vacances",' +
    '    "ville": "Nice"' +
    '  }' +
    '}}';
  MongoQuery.Execute;
end;
```

#### 4. Supprimer (Delete)

```pascal
procedure TdmDatabase.SupprimerDocument;  
var  
  MongoQuery: TFDMongoQuery;
begin
  MongoQuery := TFDMongoQuery.Create(nil);
  try
    MongoQuery.Connection := FDConnection1;
    MongoQuery.DatabaseName := 'ma_base';
    MongoQuery.CollectionName := 'clients';

    // Supprimer les documents correspondants
    MongoQuery.QDelete := '{"nom": "Dupont"}';
    MongoQuery.Execute;

    ShowMessage('Document(s) supprimé(s)');

  finally
    MongoQuery.Free;
  end;
end;
```

### Opérateurs MongoDB courants

| Opérateur | Signification | Exemple |
|-----------|---------------|---------|
| `$eq` | Égal | `{"age": {"$eq": 35}}` |
| `$ne` | Différent | `{"statut": {"$ne": "inactif"}}` |
| `$gt` | Supérieur | `{"age": {"$gt": 18}}` |
| `$gte` | Supérieur ou égal | `{"prix": {"$gte": 100}}` |
| `$lt` | Inférieur | `{"stock": {"$lt": 10}}` |
| `$lte` | Inférieur ou égal | `{"age": {"$lte": 65}}` |
| `$in` | Dans la liste | `{"ville": {"$in": ["Paris", "Lyon"]}}` |
| `$nin` | Pas dans la liste | `{"statut": {"$nin": ["supprimé"]}}` |
| `$and` | ET logique | `{"$and": [{"age": {"$gte": 18}}, {"actif": true}]}` |
| `$or` | OU logique | `{"$or": [{"ville": "Paris"}, {"ville": "Lyon"}]}` |
| `$exists` | Champ existe | `{"email": {"$exists": true}}` |
| `$regex` | Expression régulière | `{"nom": {"$regex": "^Dup"}}` |

### Opérateurs de mise à jour

| Opérateur | Action | Exemple |
|-----------|--------|---------|
| `$set` | Définir valeur | `{"$set": {"age": 36}}` |
| `$unset` | Supprimer champ | `{"$unset": {"temporaire": ""}}` |
| `$inc` | Incrémenter | `{"$inc": {"compteur": 1}}` |
| `$push` | Ajouter à tableau | `{"$push": {"tags": "nouveau"}}` |
| `$pull` | Retirer de tableau | `{"$pull": {"tags": "ancien"}}` |
| `$addToSet` | Ajouter si absent | `{"$addToSet": {"tags": "unique"}}` |

## Alternative : JSON dans PostgreSQL/MySQL

Si vous ne voulez pas adopter complètement NoSQL, les bases relationnelles modernes supportent le **JSON natif** :

### PostgreSQL avec JSON

```pascal
// Créer une table avec colonne JSON
Query.SQL.Text :=
  'CREATE TABLE clients ( ' +
  '  id SERIAL PRIMARY KEY, ' +
  '  nom VARCHAR(100), ' +
  '  donnees_json JSONB ' +  // JSONB = JSON binaire
  ')';
Query.ExecSQL;

// Insérer avec JSON
Query.SQL.Text :=
  'INSERT INTO clients (nom, donnees_json) ' +
  'VALUES (:nom, :json)';

Query.ParamByName('nom').AsString := 'Dupont';  
Query.ParamByName('json').AsString :=  
  '{"age": 35, "ville": "Paris", "hobbies": ["sport", "lecture"]}';
Query.ExecSQL;

// Requête sur JSON
Query.SQL.Text :=
  'SELECT nom, donnees_json->>''ville'' AS ville ' +
  'FROM clients ' +
  'WHERE donnees_json->>''age'' > ''18''';
Query.Open;
```

### MySQL avec JSON

```pascal
// MySQL 5.7+ supporte JSON
Query.SQL.Text :=
  'CREATE TABLE clients ( ' +
  '  id INT PRIMARY KEY AUTO_INCREMENT, ' +
  '  nom VARCHAR(100), ' +
  '  donnees JSON ' +
  ')';
Query.ExecSQL;

// Insérer
Query.SQL.Text :=
  'INSERT INTO clients (nom, donnees) ' +
  'VALUES (:nom, :json)';

Query.ParamByName('nom').AsString := 'Martin';  
Query.ParamByName('json').AsString :=  
  '{"age": 28, "ville": "Lyon"}';
Query.ExecSQL;

// Requête sur JSON
Query.SQL.Text :=
  'SELECT nom, JSON_EXTRACT(donnees, ''$.ville'') AS ville ' +
  'FROM clients ' +
  'WHERE JSON_EXTRACT(donnees, ''$.age'') > 18';
Query.Open;
```

**Avantages de JSON dans SQL :**
- ✅ Garde les avantages du relationnel (transactions ACID, jointures)
- ✅ Flexibilité pour certaines données
- ✅ Pas besoin d'apprendre un nouveau système

**Inconvénients :**
- ⚠️ Performances moindres que MongoDB pour requêtes JSON
- ⚠️ Pas de scalabilité horizontale native
- ⚠️ Fonctionnalités JSON moins riches

## Redis : Cache et données clé-valeur

**Redis** est une base de données en mémoire ultra-rapide, souvent utilisée comme **cache**.

### Cas d'usage Redis

```pascal
// Cache de session utilisateur
Redis.Set('session:user123', '{"nom": "Dupont", "role": "admin"}', 3600);  // Expire après 1h

// Cache de page
Redis.Set('cache:page_accueil', '<html>...</html>', 300);  // 5 minutes

// Compteur temps réel
Redis.Incr('visiteurs:aujourd''hui');

// File d'attente
Redis.LPush('queue:emails', 'email1@example.com');  
Redis.LPush('queue:emails', 'email2@example.com');  
Email := Redis.RPop('queue:emails');  // Traiter le plus ancien  
```

**Note :** FireDAC ne supporte pas Redis directement. Utilisez des bibliothèques tierces comme **Delphi Redis Client**.

## Quand utiliser NoSQL ?

### ✅ Utilisez NoSQL si...

**Données non structurées ou semi-structurées**
- Schéma flexible, évolutif
- Documents avec structures variables
- Exemple : profils utilisateurs, catalogues produits

**Scalabilité horizontale nécessaire**
- Millions d'utilisateurs
- Pétaoctets de données
- Distribution géographique

**Performance en lecture massive**
- Réseaux sociaux
- Flux d'actualités
- Analytique temps réel

**Développement agile**
- Prototypage rapide
- Itérations fréquentes
- Pas de migrations de schéma

**Données hiérarchiques/imbriquées**
- Arbres de catégories
- Commentaires imbriqués
- Structures JSON complexes

### ❌ Évitez NoSQL si...

**Relations complexes**
- Beaucoup de jointures
- Intégrité référentielle critique
- Exemple : comptabilité, ERP

**Transactions ACID strictes**
- Opérations bancaires
- Systèmes critiques
- Cohérence absolue requise

**Requêtes complexes ad-hoc**
- Rapports complexes
- Business Intelligence
- Analyses croisées

**Équipe habituée au SQL**
- Pas de compétences NoSQL
- Pas de temps pour apprendre

**Petit projet simple**
- SQLite ou MySQL suffisent
- Pas de volumétrie importante

## Avantages et inconvénients du NoSQL

### Avantages

| Avantage | Description |
|----------|-------------|
| **Flexibilité** | Schéma dynamique, évolution facile |
| **Scalabilité** | Distribution horizontale native |
| **Performance** | Optimisé pour lectures/écritures massives |
| **Développement rapide** | Pas de migrations complexes |
| **Données hiérarchiques** | Structure naturelle pour JSON |
| **Haute disponibilité** | Réplication et partitionnement intégrés |

### Inconvénients

| Inconvénient | Description |
|--------------|-------------|
| **Courbe d'apprentissage** | Nouveaux concepts à maîtriser |
| **Moins mature** | Outils et écosystème moins développés |
| **Transactions limitées** | ACID moins strict (BASE) |
| **Pas de jointures** | Dénormalisation nécessaire |
| **Redondance** | Duplication de données |
| **Requêtes complexes** | Plus difficiles qu'avec SQL |

## Modèle hybride : SQL + NoSQL

Dans la pratique, beaucoup d'applications utilisent les **deux** :

```
┌─────────────────────────────────────────┐
│         Application Moderne             │
├─────────────────────────────────────────┤
│  PostgreSQL/MySQL                       │
│  • Données transactionnelles            │
│  • Utilisateurs, commandes, paiements   │
│  • Intégrité critique                   │
├─────────────────────────────────────────┤
│  MongoDB                                │
│  • Catalogues produits                  │
│  • Profils utilisateurs enrichis        │
│  • Logs et événements                   │
├─────────────────────────────────────────┤
│  Redis                                  │
│  • Cache                                │
│  • Sessions                             │
│  • Compteurs temps réel                 │
└─────────────────────────────────────────┘
```

**Exemple concret : E-commerce**

```pascal
// Données transactionnelles → PostgreSQL
type
  TCommande = class
    ID: Integer;
    ClientID: Integer;
    DateCommande: TDateTime;
    Total: Currency;
  end;

// Catalogue produits → MongoDB
{
  "_id": "prod123",
  "nom": "Ordinateur portable",
  "description": "...",
  "caracteristiques": {
    "processeur": "Intel i7",
    "ram": "16GB",
    "stockage": "512GB SSD"
  },
  "images": ["img1.jpg", "img2.jpg"],
  "tags": ["informatique", "portable", "gaming"],
  "avis": [
    {"auteur": "Jean", "note": 5, "commentaire": "..."}
  ]
}

// Cache sessions → Redis
Redis.Set('session:user456', '{"panier": [1, 2, 3], "lang": "fr"}');
```

## Outils et bibliothèques

### Clients officiels

| Base de données | Outil d'administration |
|----------------|----------------------|
| **MongoDB** | MongoDB Compass, Studio 3T |
| **Redis** | RedisInsight, redis-cli |
| **Cassandra** | DataStax Studio |
| **CouchDB** | Fauxton (web) |

### Bibliothèques Delphi

- **FireDAC** : Support MongoDB intégré
- **Delphi Redis Client** : Client Redis open-source
- **mORMot** : Framework complet avec support NoSQL
- **TMS XData** : REST/ORM avec support MongoDB

## Bonnes pratiques NoSQL

### 1. Modélisation orientée requêtes

```
❌ Modélisation SQL (normalisée)
clients ──┬── commandes ──┬── articles
          │               └── produits
          └── adresses

✅ Modélisation NoSQL (dénormalisée)
{
  "client": {...},
  "commandes": [
    {
      "articles": [
        {"produit": {...}, "quantite": 2}
      ]
    }
  ],
  "adresses": [...]
}
```

**Principe :** Structurer les données selon comment elles seront **lues**, pas selon les règles de normalisation.

### 2. Accepter la redondance

```json
// OK en NoSQL : dupliquer les infos client
{
  "_id": "cmd123",
  "numero": "CMD-2024-001",
  "client": {                    // ← Duplication
    "id": "client456",
    "nom": "Dupont",
    "email": "dupont@email.fr"
  },
  "articles": [...]
}
```

**Avantage :** Pas de jointure, lecture ultra-rapide.

### 3. Indexer intelligemment

```javascript
// MongoDB : créer des index
db.clients.createIndex({"email": 1})  // Index sur email  
db.commandes.createIndex({"client.id": 1, "date": -1})  // Index composé  
```

### 4. Limiter la profondeur d'imbrication

```json
// ❌ Trop profond
{
  "niveau1": {
    "niveau2": {
      "niveau3": {
        "niveau4": {...}
      }
    }
  }
}

// ✅ Mieux : max 2-3 niveaux
{
  "client": {...},
  "adresses": [{...}],
  "commandes": [{...}]
}
```

## Résumé

### Points clés

✅ **NoSQL ≠ pas de SQL**, mais "Not Only SQL"  
✅ **4 types** : documentaire, clé-valeur, colonnes, graphes  
✅ **MongoDB** : leader des bases documentaires  
✅ **FireDAC** : supporte MongoDB nativement  
✅ **JSON** : disponible aussi dans PostgreSQL/MySQL  
✅ **Hybride** : souvent SQL + NoSQL dans une même application

### Quand choisir quoi ?

```
Données structurées + Relations + Transactions ACID
  → SQL (MySQL, PostgreSQL)

Données flexibles + Scalabilité horizontale + Lectures massives
  → NoSQL (MongoDB)

Cache + Sessions + Données temporaires
  → Redis

Analytique + Big Data + Séries temporelles
  → Cassandra

Réseaux sociaux + Recommandations + Relations complexes
  → Neo4j (Graphe)
```

### Tableau de décision

| Besoin | Solution recommandée |
|--------|---------------------|
| Application d'entreprise classique | PostgreSQL ou SQL Server |
| E-commerce (catalogue) | MongoDB + PostgreSQL |
| Application mobile avec sync | MongoDB ou CouchDB |
| Cache haute performance | Redis |
| IoT / Séries temporelles | InfluxDB ou Cassandra |
| Réseau social | Neo4j + MongoDB |
| Blog / CMS | MongoDB ou PostgreSQL (JSON) |

## Conclusion

Le monde des bases de données a évolué. **SQL** reste incontournable pour de nombreux cas d'usage, mais **NoSQL** apporte des solutions adaptées aux besoins modernes : scalabilité, flexibilité, performance sur données massives.

**L'approche pragmatique :**
1. Commencez avec **SQL** (PostgreSQL ou MySQL) pour la majorité des données
2. Ajoutez **MongoDB** pour les données flexibles (catalogues, profils)
3. Utilisez **Redis** pour le cache et les sessions
4. Évaluez d'autres solutions selon vos besoins spécifiques

Avec **FireDAC** et Delphi, vous avez la flexibilité d'utiliser le meilleur outil pour chaque besoin. N'ayez pas peur d'expérimenter et de mixer les approches !

Le futur des bases de données est **polyglotte** : utiliser plusieurs types de bases selon les besoins. Delphi vous permet de le faire facilement.

⏭️ [Améliorations FireDAC de Delphi 13](/08-acces-aux-bases-de-donnees-mysql-mariadb/14-ameliorations-firedac-delphi-13.md)
