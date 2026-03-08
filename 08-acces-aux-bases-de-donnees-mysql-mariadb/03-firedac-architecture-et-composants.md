🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.3 FireDAC : architecture et composants

## Introduction

FireDAC (Fire Data Access Components) est le framework moderne de Delphi pour accéder aux bases de données. Avant de commencer à l'utiliser, il est important de comprendre comment il est organisé et quels sont les composants principaux que vous utiliserez dans vos applications.

Dans ce chapitre, nous allons découvrir l'architecture de FireDAC de manière simple et progressive, sans rentrer dans des détails trop techniques.

## Qu'est-ce qu'un composant ?

Dans Delphi, un **composant** est un élément réutilisable que vous pouvez placer sur vos formulaires. Les composants FireDAC sont des éléments non-visuels (ils n'apparaissent pas à l'exécution) qui gèrent la communication avec la base de données.

**Analogie simple :**
Imaginez que vous voulez commander un repas dans un restaurant :
- Le **serveur** (composant de connexion) fait le lien entre vous et la cuisine
- Le **menu** (composant de requête) contient les plats disponibles
- Votre **commande** (les données) est transmise et préparée
- Le **plateau** (DataSource) apporte le plat à votre table

C'est exactement le même principe avec FireDAC : différents composants travaillent ensemble pour récupérer et afficher vos données.

## Architecture globale de FireDAC

FireDAC utilise une architecture en **couches** (layers) pour séparer les responsabilités :

```
┌─────────────────────────────────────────┐
│   Votre Application Delphi              │
│   (Interface utilisateur)               │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Composants de Liaison (DataSource)    │
│   - TDataSource                         │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Composants de Données (DataSets)      │
│   - TFDQuery, TFDTable, TFDStoredProc   │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Composant de Connexion                │
│   - TFDConnection                       │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Pilote FireDAC (FD.Phys.MySQL)        │
│   Gère la communication spécifique      │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Bibliothèque Client MySQL             │
│   (libmysql.dll / libmariadb.dll)       │
└──────────────────┬──────────────────────┘
                   │
┌──────────────────▼──────────────────────┐
│   Serveur MySQL/MariaDB                 │
│   (Base de données)                     │
└─────────────────────────────────────────┘
```

**Ne vous inquiétez pas** si ce schéma semble complexe ! Nous allons examiner chaque couche en détail.

## Les composants essentiels de FireDAC

### 1. TFDConnection - Le composant de connexion

**Rôle :** C'est le composant **central** qui établit et maintient la connexion avec votre base de données.

**Analogie :** C'est comme la ligne téléphonique entre vous et la base de données.

**Caractéristiques :**
- Un seul `TFDConnection` par base de données
- Tous les autres composants FireDAC doivent être reliés à cette connexion
- Gère l'ouverture et la fermeture de la connexion
- Contient tous les paramètres de connexion (serveur, utilisateur, mot de passe, etc.)

**Propriétés principales :**

| Propriété | Description | Exemple de valeur |
|-----------|-------------|-------------------|
| `DriverName` | Type de base de données | `'MySQL'` |
| `Params` | Paramètres de connexion | Server, Database, User_Name, Password |
| `Connected` | État de la connexion | `True` ou `False` |
| `LoginPrompt` | Demander les identifiants | `False` (généralement) |

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDConnection`

### 2. TFDQuery - Le composant de requête

**Rôle :** Permet d'exécuter des requêtes SQL et de manipuler les résultats.

**Analogie :** C'est comme un formulaire de recherche qui vous permet de demander exactement ce que vous voulez à la base de données.

**Caractéristiques :**
- Le composant le **plus utilisé** dans FireDAC
- Très flexible : peut faire des SELECT, INSERT, UPDATE, DELETE
- Supporte les paramètres SQL pour la sécurité
- Peut gérer des résultats de plusieurs milliers de lignes efficacement

**Propriétés principales :**

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `Connection` | Lien vers le TFDConnection | `FDConnection1` |
| `SQL` | La requête SQL à exécuter | `SELECT * FROM clients` |
| `Active` | Dataset ouvert ou fermé | `True` ou `False` |
| `Params` | Paramètres de la requête | `[ParamByName('id')]` |

**Méthodes importantes :**

| Méthode | Usage | Exemple |
|---------|-------|---------|
| `Open` | Exécute un SELECT | `FDQuery1.Open;` |
| `ExecSQL` | Exécute INSERT/UPDATE/DELETE | `FDQuery1.ExecSQL;` |
| `Close` | Ferme le dataset | `FDQuery1.Close;` |
| `First` | Va au premier enregistrement | `FDQuery1.First;` |
| `Next` | Va à l'enregistrement suivant | `FDQuery1.Next;` |
| `Eof` | Teste la fin du dataset | `while not FDQuery1.Eof do` |

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDQuery`

### 3. TFDTable - Le composant table

**Rôle :** Accède directement à une table de la base de données, sans écrire de SQL.

**Analogie :** C'est comme ouvrir un classeur et consulter directement une fiche.

**Caractéristiques :**
- Accès simple à une table complète
- Pas besoin d'écrire de SQL
- Moins flexible que TFDQuery
- Utile pour des opérations simples

**Propriétés principales :**

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `Connection` | Lien vers le TFDConnection | `FDConnection1` |
| `TableName` | Nom de la table | `'clients'` |
| `Active` | Table ouverte ou fermée | `True` ou `False` |

**Quand utiliser TFDTable vs TFDQuery ?**

**TFDTable :**
- ✅ Accès simple à toute une table
- ✅ Navigation rapide sans SQL
- ❌ Charge toute la table (peut être lourd)
- ❌ Pas de filtrage au niveau serveur

**TFDQuery :**
- ✅ Contrôle total avec SQL
- ✅ Peut filtrer côté serveur (plus efficace)
- ✅ Jointures et requêtes complexes
- ❌ Nécessite de connaître un peu de SQL

**Recommandation :** Utilisez principalement `TFDQuery` dans vos projets.

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDTable`

### 4. TFDStoredProc - Le composant procédure stockée

**Rôle :** Exécute des procédures stockées (stored procedures) dans la base de données.

**Qu'est-ce qu'une procédure stockée ?**
C'est un ensemble de commandes SQL enregistrées dans la base de données, que vous pouvez appeler par leur nom.

**Caractéristiques :**
- Exécute du code SQL complexe côté serveur
- Peut accepter des paramètres en entrée et en sortie
- Souvent plus performant pour des opérations complexes

**Propriétés principales :**

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `Connection` | Lien vers le TFDConnection | `FDConnection1` |
| `StoredProcName` | Nom de la procédure | `'calculer_total'` |
| `Params` | Paramètres de la procédure | `[Param1, Param2]` |

**Note pour débutants :** Les procédures stockées sont un sujet avancé. Pour commencer, concentrez-vous sur `TFDQuery`.

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDStoredProc`

### 5. TDataSource - Le composant de liaison

**Rôle :** Fait le lien entre les composants de données (Query, Table) et les composants visuels (Grids, Edit).

**Analogie :** C'est le câble qui relie votre lecteur DVD à votre télévision.

**Caractéristiques :**
- **Essentiel** pour afficher des données dans l'interface
- Un `TDataSource` par dataset que vous voulez afficher
- Ne fait pas partie de FireDAC à proprement parler (composant Delphi standard)

**Propriétés principales :**

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `DataSet` | Le dataset source | `FDQuery1` |
| `Enabled` | Active/désactive le lien | `True` ou `False` |

**Comment ça fonctionne ?**

```
TFDQuery1 ────► TDataSource1 ────► TDBGrid1
(Données)        (Liaison)         (Affichage)
```

**Dans la palette de composants :**
Onglet **Data Access** → `TDataSource`

## Composants complémentaires utiles

### TFDPhysMySQLDriverLink

**Rôle :** Spécifie explicitement l'utilisation du pilote MySQL pour FireDAC.

**Quand l'utiliser ?**
- Techniquement facultatif (FireDAC peut le détecter automatiquement)
- Utile pour spécifier le chemin de la bibliothèque client
- Recommandé pour éviter les ambiguïtés

**Propriétés principales :**

| Propriété | Description | Exemple |
|-----------|-------------|---------|
| `VendorLib` | Chemin vers libmysql.dll | `'C:\...\libmysql.dll'` |

**Dans la palette de composants :**
Onglet **FireDAC Links** → `TFDPhysMySQLDriverLink`

### TFDGUIxWaitCursor

**Rôle :** Affiche un curseur d'attente lors des opérations longues.

**Pourquoi c'est utile ?**
Quand une requête prend du temps, l'utilisateur voit que l'application travaille.

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDGUIxWaitCursor`

### TFDTransaction

**Rôle :** Gère les transactions pour assurer l'intégrité des données.

**Qu'est-ce qu'une transaction ?**
Un ensemble d'opérations qui doivent toutes réussir ou toutes échouer ensemble.

**Exemple :**
Transférer de l'argent entre deux comptes :
1. Débiter le compte A (-100€)
2. Créditer le compte B (+100€)

Si l'étape 2 échoue, l'étape 1 doit être annulée (rollback).

**Note pour débutants :** Les transactions sont importantes mais complexes. FireDAC gère automatiquement les transactions simples.

**Dans la palette de composants :**
Onglet **FireDAC** → `TFDTransaction`

## Organisation typique sur un formulaire

Voici comment vous organiserez généralement vos composants FireDAC :

```
Formulaire principal
├── FDConnection1 (TFDConnection)
│   └── Paramètres : MySQL, localhost, ma_base
│
├── FDPhysMySQLDriverLink1 (TFDPhysMySQLDriverLink)
│   └── VendorLib : chemin vers libmysql.dll
│
├── FDQuery1 (TFDQuery)
│   ├── Connection : FDConnection1
│   └── SQL : SELECT * FROM clients
│
├── DataSource1 (TDataSource)
│   └── DataSet : FDQuery1
│
└── DBGrid1 (TDBGrid)
    └── DataSource : DataSource1
```

**Flux de données :**
1. `FDConnection1` se connecte à MySQL
2. `FDQuery1` exécute une requête via cette connexion
3. `DataSource1` expose les données de la requête
4. `DBGrid1` affiche les données via le DataSource

## Les unités (units) FireDAC à inclure

Pour utiliser FireDAC, vous devez ajouter certaines unités dans la clause `uses` de votre code :

### Unités essentielles

```pascal
uses
  // Unités FireDAC de base
  FireDAC.Stan.Intf,      // Interfaces de base
  FireDAC.Stan.Option,    // Options FireDAC
  FireDAC.Stan.Error,     // Gestion des erreurs
  FireDAC.Stan.Def,       // Définitions
  FireDAC.Stan.Pool,      // Pool de connexions
  FireDAC.Stan.Async,     // Opérations asynchrones

  // Unités de connexion
  FireDAC.Phys.Intf,      // Interface physique
  FireDAC.Phys.MySQL,     // Pilote MySQL
  FireDAC.Phys.MySQLDef,  // Définitions MySQL

  // Unités d'interface utilisateur
  FireDAC.UI.Intf,        // Interface utilisateur
  FireDAC.VCLUI.Wait,     // Curseur d'attente VCL

  // Unités de composants
  FireDAC.Comp.Client,    // TFDConnection, TFDQuery, etc.
  FireDAC.Comp.DataSet,   // Support DataSet

  // Unités DAO (Data Access Objects)
  FireDAC.DApt.Intf,      // Interface adaptateur
  FireDAC.DApt;           // Adaptateur de données
```

**Bonne nouvelle :** Delphi ajoute automatiquement ces unités quand vous placez des composants FireDAC sur votre formulaire !

### Unités pour d'autres bases de données

Si vous utilisez d'autres bases de données :

```pascal
// Pour PostgreSQL
FireDAC.Phys.PG, FireDAC.Phys.PGDef,

// Pour SQLite
FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,

// Pour SQL Server
FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,

// Pour Oracle
FireDAC.Phys.Oracle, FireDAC.Phys.OracleDef,
```

## Propriétés communes aux datasets FireDAC

Tous les composants de données (Query, Table, StoredProc) partagent des propriétés communes car ils héritent de `TFDDataSet` :

### Navigation

| Propriété/Méthode | Description |
|-------------------|-------------|
| `Eof` | Fin du dataset (End Of File) |
| `Bof` | Début du dataset (Begin Of File) |
| `RecordCount` | Nombre total d'enregistrements |
| `RecNo` | Numéro de l'enregistrement courant |
| `First` | Aller au premier enregistrement |
| `Last` | Aller au dernier enregistrement |
| `Next` | Enregistrement suivant |
| `Prior` | Enregistrement précédent |

### Édition

| Méthode | Description |
|---------|-------------|
| `Edit` | Passe en mode édition |
| `Post` | Valide les modifications |
| `Cancel` | Annule les modifications |
| `Delete` | Supprime l'enregistrement courant |
| `Insert` | Insère un nouvel enregistrement |
| `Append` | Ajoute un enregistrement à la fin |

### État

| Propriété | Description | Valeurs |
|-----------|-------------|---------|
| `State` | État actuel du dataset | dsInactive, dsBrowse, dsEdit, dsInsert |
| `Modified` | Dataset modifié ? | True/False |
| `Active` | Dataset ouvert ? | True/False |

### Accès aux champs

```pascal
// Plusieurs façons d'accéder aux données
Value := FDQuery1.FieldByName('nom').AsString;  
Value := FDQuery1['nom'];  // Syntaxe courte  
Value := FDQuery1.Fields[0].AsString;  // Par index  
```

## Gestion de la mémoire et des ressources

### Principe important : Ouverture et Fermeture

**Règle d'or :** Ce que vous ouvrez, vous devez le fermer !

```pascal
// Bonne pratique
FDQuery1.Open;  
try  
  // Traiter les données
finally
  FDQuery1.Close;  // Toujours fermer
end;
```

### Libération automatique

Les composants placés sur un formulaire sont automatiquement libérés quand le formulaire est détruit. Vous n'avez pas besoin de les libérer manuellement.

### Connexion persistante ou à la demande ?

**Deux approches :**

**1. Connexion persistante** (recommandée pour débuter)
```pascal
// Ouverte au démarrage de l'application
FDConnection1.Connected := True;
```

**2. Connexion à la demande**
```pascal
// Ouverte uniquement quand nécessaire
FDConnection1.Open;  
try  
  FDQuery1.Open;
  // Traiter...
finally
  FDConnection1.Close;
end;
```

## Architecture en couches recommandée

Pour les applications professionnelles, séparez votre code en couches :

### Approche simple (débutants)

```
Formulaire (UI)
    ↓
Composants FireDAC directement sur le formulaire
    ↓
Base de données
```

### Approche professionnelle (avancé)

```
Formulaire (UI - Présentation)
    ↓
Module Métier (Business Logic)
    ↓
Module d'Accès aux Données (DataModule avec composants FireDAC)
    ↓
Base de données
```

**DataModule :** Un conteneur non-visuel pour regrouper tous vos composants FireDAC. Nous verrons cela plus tard.

## Résumé des composants essentiels

| Composant | Rôle | Indispensable ? |
|-----------|------|-----------------|
| **TFDConnection** | Connexion à la BD | ✅ Oui |
| **TFDQuery** | Requêtes SQL | ✅ Oui (principal) |
| **TDataSource** | Liaison UI | ✅ Oui (pour affichage) |
| TFDTable | Accès table directe | ⚠️ Optionnel |
| TFDStoredProc | Procédures stockées | ⚠️ Si nécessaire |
| TFDPhysMySQLDriverLink | Pilote MySQL | ⚠️ Recommandé |
| TFDGUIxWaitCursor | Curseur attente | ⚠️ Optionnel |
| TFDTransaction | Transactions | ⚠️ Avancé |

## Points clés à retenir

✅ **FireDAC utilise une architecture en couches** pour séparer les responsabilités

✅ **TFDConnection** est le composant central qui connecte à la base de données

✅ **TFDQuery** est le composant le plus utilisé pour exécuter des requêtes SQL

✅ **TDataSource** fait le lien entre les données et l'interface utilisateur

✅ **Tous les composants de données** doivent être reliés à un TFDConnection

✅ **N'oubliez pas de fermer** les datasets après utilisation

✅ **Les unités FireDAC** sont ajoutées automatiquement par Delphi

## Prochaines étapes

Maintenant que vous comprenez l'architecture et les composants de FireDAC, vous êtes prêt à :

1. Créer votre première connexion à MySQL/MariaDB
2. Exécuter vos premières requêtes SQL
3. Afficher des données dans votre interface
4. Manipuler les données (ajout, modification, suppression)

Dans la section suivante, nous allons mettre tout cela en pratique en créant notre première connexion fonctionnelle !

⏭️ [Connexion à une base MySQL/MariaDB](/08-acces-aux-bases-de-donnees-mysql-mariadb/04-connexion-a-une-base-mysql-mariadb.md)
