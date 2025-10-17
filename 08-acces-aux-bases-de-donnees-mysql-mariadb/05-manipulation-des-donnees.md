🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.5 Manipulation des données

## Introduction

Maintenant que vous savez vous connecter à une base de données MySQL/MariaDB avec FireDAC, il est temps d'apprendre à **manipuler les données** : les lire, les créer, les modifier et les supprimer. C'est le cœur même d'une application de base de données !

Dans ce chapitre, nous allons explorer toutes les techniques pour travailler efficacement avec vos données, de la simple lecture à la gestion avancée des transactions.

## Qu'est-ce que la manipulation des données ?

La **manipulation des données** (Data Manipulation) regroupe toutes les opérations qui permettent de :

- **Consulter** les données existantes (lire, rechercher, filtrer)
- **Ajouter** de nouvelles données (créer de nouveaux enregistrements)
- **Modifier** les données existantes (mettre à jour des informations)
- **Supprimer** des données (effacer des enregistrements)

### Analogie avec un classeur

Imaginez une base de données comme un classeur de fiches :

| Opération | Analogie physique | Opération base de données |
|-----------|-------------------|---------------------------|
| **Consulter** | Feuilleter le classeur, chercher une fiche | SELECT (Lecture) |
| **Ajouter** | Créer une nouvelle fiche et l'insérer | INSERT (Création) |
| **Modifier** | Prendre une fiche, la corriger, la remettre | UPDATE (Modification) |
| **Supprimer** | Retirer une fiche et la jeter | DELETE (Suppression) |

## Le modèle CRUD

Dans le monde des bases de données, on parle souvent de **CRUD**, un acronyme qui résume les quatre opérations fondamentales :

```
┌─────────────────────────────────────────┐
│            Opérations CRUD              │
├─────────────────────────────────────────┤
│  C - Create    (Créer)      → INSERT    │
│  R - Read      (Lire)       → SELECT    │
│  U - Update    (Mettre à jour) → UPDATE │
│  D - Delete    (Supprimer)  → DELETE    │
└─────────────────────────────────────────┘
```

**Toute application de gestion de données** utilise ce modèle CRUD. Que vous développiez :
- Une gestion de contacts
- Un système de facturation
- Un catalogue de produits
- Une application RH
- Un CRM

Vous manipulerez toujours les données selon ces 4 opérations de base.

## Vue d'ensemble du chapitre

Ce chapitre 8.5 est divisé en trois sous-sections qui vont vous guider progressivement dans la manipulation des données :

### 8.5.1 Requêtes SQL et paramétrées

**Ce que vous allez apprendre :**
- Les bases du langage SQL pour communiquer avec la base de données
- Comment exécuter des requêtes SELECT pour lire des données
- Comment utiliser INSERT, UPDATE et DELETE
- **L'importance critique des requêtes paramétrées** pour la sécurité
- Comment protéger votre application contre les injections SQL

**Pourquoi c'est important :**
Le SQL est le langage universel des bases de données. Les requêtes paramétrées sont **essentielles** pour sécuriser vos applications et éviter les failles de sécurité majeures.

### 8.5.2 CRUD : Create, Read, Update, Delete

**Ce que vous allez apprendre :**
- Comment construire une application CRUD complète
- Créer une interface utilisateur intuitive pour manipuler les données
- Implémenter chaque opération avec une interface graphique
- Valider les données avant de les enregistrer
- Gérer les erreurs et les cas particuliers
- Organiser votre code de manière professionnelle

**Pourquoi c'est important :**
C'est la mise en pratique concrète ! Vous allez créer une véritable application de gestion avec toutes les fonctionnalités qu'un utilisateur attend.

### 8.5.3 Transactions et intégrité des données

**Ce que vous allez apprendre :**
- Qu'est-ce qu'une transaction et pourquoi c'est crucial
- Comment garantir que vos données restent cohérentes
- Les propriétés ACID (Atomicité, Cohérence, Isolation, Durabilité)
- Comment gérer plusieurs opérations qui doivent réussir ou échouer ensemble
- Les contraintes d'intégrité référentielle
- Les bonnes pratiques pour des applications fiables

**Pourquoi c'est important :**
Les transactions garantissent que vos données ne sont jamais dans un état incohérent. C'est la différence entre une application amateur et une application professionnelle.

## Les outils à votre disposition

Pour manipuler les données avec Delphi et FireDAC, vous utiliserez principalement :

### Composants FireDAC

| Composant | Rôle | Usage principal |
|-----------|------|-----------------|
| **TFDConnection** | Connexion à la base | Établir le lien avec MySQL/MariaDB |
| **TFDQuery** | Exécuter des requêtes SQL | Lire et modifier les données |
| **TFDTable** | Accès direct à une table | Manipulation simple d'une table |
| **TDataSource** | Liaison avec l'interface | Connecter les données aux contrôles visuels |

### Méthodes principales

| Méthode | Composant | Usage |
|---------|-----------|-------|
| `Open` | TFDQuery | Exécuter une requête SELECT (lecture) |
| `ExecSQL` | TFDQuery | Exécuter INSERT/UPDATE/DELETE |
| `FieldByName` | TFDQuery | Accéder à un champ par son nom |
| `First`, `Next`, `Eof` | TFDQuery | Parcourir les enregistrements |
| `Post` | TFDQuery | Valider les modifications |
| `Cancel` | TFDQuery | Annuler les modifications |

## Le flux de manipulation des données

Voici comment se déroule typiquement la manipulation des données dans une application Delphi :

```
┌─────────────────────────────────────────────┐
│  1. L'utilisateur interagit avec l'interface│
│     (clique sur un bouton, saisit du texte) │
└──────────────────┬──────────────────────────┘
                   ↓
┌─────────────────────────────────────────────┐
│  2. Le code Delphi construit une requête SQL│
│     (avec des paramètres pour la sécurité)  │
└──────────────────┬──────────────────────────┘
                   ↓
┌─────────────────────────────────────────────┐
│  3. FireDAC envoie la requête à MySQL       │
│     (via la connexion TFDConnection)        │
└──────────────────┬──────────────────────────┘
                   ↓
┌─────────────────────────────────────────────┐
│  4. MySQL traite la requête                 │
│     (lecture, insertion, modification...)   │
└──────────────────┬──────────────────────────┘
                   ↓
┌─────────────────────────────────────────────┐
│  5. MySQL renvoie le résultat               │
│     (données, confirmation, ou erreur)      │
└──────────────────┬──────────────────────────┘
                   ↓
┌─────────────────────────────────────────────┐
│  6. FireDAC traite la réponse               │
│     (remplit le TFDQuery avec les données)  │
└──────────────────┬──────────────────────────┘
                   ↓
┌────────────────────────────────────────────────┐
│  7. L'interface se met à jour                  │
│     (grilles, champs, messages à l'utilisateur)│
└────────────────────────────────────────────────┘
```

## Exemple simple : Le cycle complet

Pour bien comprendre, voici un exemple simple montrant toutes les étapes :

### Scénario : Afficher la liste des clients

```pascal
// Étape 1 : L'utilisateur clique sur "Afficher les clients"
procedure TForm1.btnAfficherClick(Sender: TObject);
begin
  // Étape 2 : Construire la requête SQL
  FDQueryClients.SQL.Text := 'SELECT * FROM clients ORDER BY nom';

  // Étape 3-6 : FireDAC communique avec MySQL
  FDQueryClients.Open;  // Cette ligne fait tout le travail !

  // Étape 7 : Les données apparaissent dans la grille
  // (automatique si la grille est liée au TDataSource)
end;
```

C'est aussi simple que ça ! FireDAC gère toute la complexité de la communication avec MySQL.

## Les deux approches de manipulation

Il existe deux façons principales de manipuler les données avec FireDAC :

### Approche 1 : SQL direct (recommandée)

Vous écrivez explicitement les requêtes SQL :

```pascal
// Lecture
FDQuery.SQL.Text := 'SELECT * FROM clients WHERE actif = TRUE';
FDQuery.Open;

// Insertion
FDQuery.SQL.Text := 'INSERT INTO clients (nom, prenom) VALUES (:Nom, :Prenom)';
FDQuery.ParamByName('Nom').AsString := 'Dupont';
FDQuery.ParamByName('Prenom').AsString := 'Jean';
FDQuery.ExecSQL;
```

**Avantages :**
- Contrôle total sur les requêtes
- Plus efficace pour les requêtes complexes
- Meilleure compréhension de ce qui se passe

### Approche 2 : Manipulation directe du Dataset

Vous modifiez directement les enregistrements comme des objets :

```pascal
// Ajouter un enregistrement
FDQuery.Append;
FDQuery.FieldByName('nom').AsString := 'Dupont';
FDQuery.FieldByName('prenom').AsString := 'Jean';
FDQuery.Post;  // Enregistre dans la base

// Modifier un enregistrement
FDQuery.Edit;
FDQuery.FieldByName('email').AsString := 'nouveau@email.fr';
FDQuery.Post;  // Enregistre les modifications
```

**Avantages :**
- Plus intuitif pour les débutants
- Idéal pour la liaison avec des contrôles visuels (DBGrid, DBEdit)
- Moins de code SQL à écrire

**Dans ce chapitre**, nous verrons les deux approches, mais nous privilégierons le **SQL direct** car il offre plus de contrôle et de flexibilité.

## Concepts clés à maîtriser

Avant de plonger dans les détails, voici les concepts fondamentaux que vous devez comprendre :

### 1. Dataset (Ensemble de données)

Un **dataset** est un ensemble d'enregistrements chargés en mémoire. C'est comme une "photo" de votre table à un instant T.

```pascal
FDQuery.Open;  // Charge le dataset en mémoire
// Maintenant vous pouvez parcourir les enregistrements
```

### 2. État du Dataset

Un dataset peut être dans différents états :

| État | Description |
|------|-------------|
| `dsInactive` | Fermé, pas de données |
| `dsBrowse` | Ouvert, consultation |
| `dsEdit` | En cours de modification |
| `dsInsert` | En cours d'ajout |

### 3. Curseur (Enregistrement courant)

Le **curseur** pointe vers un enregistrement spécifique dans le dataset :

```pascal
FDQuery.First;  // Curseur sur le premier enregistrement
FDQuery.Next;   // Déplacer le curseur
FDQuery.Last;   // Curseur sur le dernier enregistrement
```

### 4. Paramètres SQL

Les **paramètres** sont des espaces réservés dans vos requêtes SQL, remplacés de manière sécurisée :

```pascal
// :Nom est un paramètre
SQL.Text := 'SELECT * FROM clients WHERE nom = :Nom';
ParamByName('Nom').AsString := 'Dupont';  // Remplace :Nom par 'Dupont'
```

**Crucial pour la sécurité !** Nous y reviendrons en détail dans la section 8.5.1.

## La sécurité avant tout

⚠️ **Point crucial à retenir dès maintenant :**

**JAMAIS** de concaténation de chaînes dans vos requêtes SQL !

```pascal
// ❌ DANGEREUX - Ne JAMAIS faire ça !
SQL.Text := 'SELECT * FROM clients WHERE nom = ''' + editNom.Text + '''';

// ✅ SÉCURISÉ - Toujours utiliser des paramètres
SQL.Text := 'SELECT * FROM clients WHERE nom = :Nom';
ParamByName('Nom').AsString := editNom.Text;
```

La première méthode est **vulnérable aux injections SQL**, une des failles de sécurité les plus dangereuses. Nous expliquerons pourquoi et comment l'éviter dans la section 8.5.1.

## Organisation de votre apprentissage

Pour tirer le meilleur parti de ce chapitre, nous vous recommandons de suivre cet ordre :

### Étape 1 : Requêtes SQL (8.5.1)
Apprenez les bases du SQL et comment utiliser les paramètres. C'est la **fondation** de tout le reste.

### Étape 2 : Application CRUD (8.5.2)
Mettez en pratique en créant une application complète. Vous verrez comment tout s'articule dans un projet réel.

### Étape 3 : Transactions (8.5.3)
Apprenez à garantir la cohérence de vos données. C'est ce qui fait la différence entre une application de qualité et une application professionnelle.

## Ressources pour aller plus loin

Une fois que vous aurez maîtrisé ce chapitre, vous pourrez explorer :

- **Requêtes complexes** : jointures, sous-requêtes, agrégations
- **Optimisation** : indexes, requêtes préparées, caching
- **Architecture en couches** : séparation de la logique métier et de l'accès aux données
- **ORM** : Object-Relational Mapping pour une approche plus orientée objet

## Ce que vous allez construire

À la fin de ce chapitre, vous serez capable de créer une application complète de gestion de clients incluant :

✅ Une liste de tous les clients avec recherche et filtres
✅ Un formulaire pour ajouter de nouveaux clients
✅ La possibilité de modifier les informations existantes
✅ La suppression sécurisée avec confirmation
✅ La validation des données (email, champs obligatoires)
✅ La gestion des erreurs (doublons, contraintes)
✅ Des transactions pour garantir la cohérence
✅ Une interface utilisateur intuitive et réactive

## Prérequis

Avant de commencer ce chapitre, assurez-vous de :

✓ Avoir installé et configuré MySQL/MariaDB (section 8.2)
✓ Comprendre l'architecture de FireDAC (section 8.3)
✓ Savoir établir une connexion à la base (section 8.4)
✓ Avoir créé au moins une table de test dans votre base

## Prêt à commencer ?

Vous avez maintenant une vue d'ensemble de ce qui vous attend dans ce chapitre. La manipulation des données est le cœur de toute application de base de données, et vous allez acquérir toutes les compétences nécessaires pour créer des applications robustes et sécurisées.

Dans la section suivante (8.5.1), nous plongerons dans le SQL et les requêtes paramétrées, la base de toute manipulation de données sécurisée.

**Allons-y !** 🚀

⏭️ [Requêtes SQL et paramétrées](/08-acces-aux-bases-de-donnees-mysql-mariadb/05.1-requetes-sql-parametrees.md)
