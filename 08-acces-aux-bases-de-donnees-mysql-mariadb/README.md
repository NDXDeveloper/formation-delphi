🔝 Retour au [Sommaire](/SOMMAIRE.md)

# Chapitre 8 : Accès aux bases de données MySQL/MariaDB

## Introduction

Jusqu'à présent, vous avez appris à créer des interfaces utilisateur sophistiquées et à gérer la navigation dans vos applications. Mais une application sans données est comme une bibliothèque sans livres : elle peut être belle, mais elle reste vide. La vraie puissance d'une application réside dans sa capacité à stocker, récupérer et manipuler des données de manière efficace et sécurisée.

Ce chapitre vous ouvre les portes du monde des bases de données avec Delphi, en se concentrant sur MySQL et MariaDB, deux des systèmes de gestion de bases de données les plus populaires au monde. Vous apprendrez à transformer vos applications en véritables systèmes d'information capables de gérer des milliers, voire des millions de données.

## Pourquoi les bases de données sont essentielles

### Les limites des fichiers texte

Vous avez peut-être déjà utilisé des fichiers texte, INI ou JSON pour stocker des données. C'est acceptable pour de petites quantités d'informations simples, mais imaginez :

- **Une application de gestion de stock** : 10 000 produits, 500 fournisseurs, 1000 clients
- **Un système de réservation** : Des centaines de réservations par jour
- **Une application de comptabilité** : Des milliers de transactions financières
- **Un réseau social** : Des millions d'utilisateurs et de messages

Avec des fichiers texte, vous seriez rapidement confronté à :
- ❌ Des performances catastrophiques (recherche lente)
- ❌ Des risques de corruption de données
- ❌ L'impossibilité d'accès concurrent (plusieurs utilisateurs en même temps)
- ❌ Aucune relation entre les données
- ❌ Pas de validation ou de contraintes
- ❌ Difficultés pour faire des recherches complexes

### Les avantages d'une base de données

Avec une base de données comme MySQL/MariaDB, vous obtenez :

✓ **Performance** : Recherche ultra-rapide sur des millions d'enregistrements
✓ **Intégrité** : Les données restent cohérentes et valides
✓ **Concurrence** : Des milliers d'utilisateurs simultanés
✓ **Relations** : Lier les données entre elles (clients ↔ commandes ↔ produits)
✓ **Sécurité** : Contrôle d'accès, authentification, chiffrement
✓ **Transactions** : Garantie que les opérations sont complètes ou annulées
✓ **Sauvegarde** : Outils professionnels de backup et restauration
✓ **Évolutivité** : Passer de 100 à 100 millions d'enregistrements

## Pourquoi MySQL et MariaDB ?

### MySQL : Le leader mondial

**MySQL** est l'une des bases de données les plus utilisées au monde :
- Utilisé par Facebook, Twitter, YouTube, Wikipedia
- Gratuit et open source (Community Edition)
- Existe depuis 1995 (maturité prouvée)
- Documentation abondante
- Grande communauté de développeurs

### MariaDB : L'alternative libre

**MariaDB** est un fork de MySQL créé par les fondateurs originaux de MySQL :
- 100% compatible avec MySQL
- Totalement open source (pas de version propriétaire)
- Souvent plus rapide que MySQL
- Innovations techniques supplémentaires
- Adopté par Linux, Google, Wikipedia

### Pourquoi ce chapitre se concentre sur MySQL/MariaDB ?

1. **Compatibilité parfaite** : Code identique pour les deux
2. **Popularité** : Compétence très demandée dans le monde professionnel
3. **Facilité d'apprentissage** : Excellente pour débuter
4. **Support Delphi** : FireDAC offre un support natif excellent
5. **Gratuité** : Aucun coût de licence
6. **Multi-plateforme** : Windows, Linux, macOS, Android, iOS

## Ce que vous allez apprendre

### 1. Vue d'ensemble des technologies d'accès aux données

Avant de plonger dans le code, vous comprendrez :
- L'évolution des technologies d'accès aux données dans Delphi
- Pourquoi FireDAC est la solution moderne recommandée
- Les alternatives et quand les utiliser
- L'architecture client-serveur

**Pourquoi c'est important** : Comprendre les options disponibles vous permet de faire les bons choix techniques.

### 2. Configuration de MySQL/MariaDB pour Delphi

Vous apprendrez à :
- Installer MySQL ou MariaDB sur votre machine
- Configurer le serveur pour le développement
- Créer votre première base de données
- Installer et configurer les drivers nécessaires
- Tester la connexion

**Résultat pratique** : Un environnement de développement prêt à l'emploi.

### 3. FireDAC : Architecture et composants

FireDAC est la bibliothèque moderne de Delphi pour accéder aux bases de données :
- Architecture multi-couches de FireDAC
- Les principaux composants (Connection, Query, Table, etc.)
- Le rôle de chaque composant
- Comment ils s'articulent ensemble

**Pourquoi c'est crucial** : FireDAC est puissant mais complexe. Comprendre son architecture évite bien des erreurs.

### 4. Connexion à une base MySQL/MariaDB

Votre première connexion concrète :
- Configurer un composant `TFDConnection`
- Paramètres de connexion (serveur, port, utilisateur, mot de passe)
- Tester la connexion
- Gérer les erreurs de connexion
- Connexion locale vs distante

**Premier succès** : Vous verrez "Connexion réussie !" s'afficher.

### 5. Manipulation des données (CRUD)

Le cœur de toute application de base de données :

**5.1 Requêtes SQL et paramétrées**
- Écrire des requêtes SELECT, INSERT, UPDATE, DELETE
- Utiliser des paramètres pour éviter les injections SQL
- Récupérer les résultats

**5.2 CRUD : Create, Read, Update, Delete**
- Créer de nouveaux enregistrements
- Lire et afficher les données
- Modifier des enregistrements existants
- Supprimer des données

**5.3 Transactions et intégrité**
- Comprendre les transactions ACID
- Commencer, valider ou annuler une transaction
- Garantir la cohérence des données

**Exemple concret** : Une application de gestion de contacts où vous pouvez ajouter, modifier, rechercher et supprimer des clients.

### 6. DataSets et DataSources

Les composants qui font le pont entre la base de données et l'interface :
- Comprendre le concept de DataSet
- Naviguer dans les enregistrements (First, Next, Prior, Last)
- Filtrer et trier les données
- Le rôle des DataSource
- Modes d'édition (Browse, Edit, Insert)

**Magie visuelle** : Vos données s'affichent automatiquement dans les composants visuels.

### 7. Contrôles liés aux données (Data-Aware Controls)

Afficher et éditer vos données sans écrire de code :
- `TDBGrid` : Grille de données
- `TDBEdit` : Champ de texte lié
- `TDBMemo` : Zone de texte multiligne
- `TDBComboBox` : Liste déroulante
- `TDBImage` : Images depuis la base
- `TDBNavigator` : Boutons de navigation

**Productivité** : Créer un écran de gestion complet en quelques minutes.

### 8. Live Bindings et liaison de données visuelle

L'approche moderne de Delphi pour lier les données :
- Comprendre les Live Bindings
- Créer des liaisons visuellement
- Utiliser avec FireMonkey (applications mobiles)
- Avantages par rapport aux contrôles data-aware classiques

**Innovation** : Une approche plus flexible et moderne.

### 9. Modèle en couches pour l'accès aux données

L'architecture professionnelle :
- Séparer l'interface, la logique métier et l'accès aux données
- Créer des DataModules
- Pattern Repository
- Avantages pour la maintenance et les tests
- Code réutilisable

**Application d'entreprise** : Structure votre code comme les pros.

### 10. Migration et synchronisation de bases de données

Gérer l'évolution de votre schéma de base de données :
- Scripts de migration
- Versioning du schéma
- Synchronisation entre environnements (dev, test, production)
- Outils et bonnes pratiques

**Monde réel** : Votre application évolue, votre base de données doit suivre.

### 11. Sécurisation des accès et prévention des injections SQL

La sécurité avant tout :
- Comprendre les injections SQL et comment les éviter
- Utiliser correctement les requêtes paramétrées
- Chiffrer les données sensibles
- Gérer les permissions et les rôles
- Protéger les mots de passe de connexion
- Audit et logging

**Sécurité** : Protégez vos données et celles de vos utilisateurs.

### 12. Autres moteurs de bases de données

Élargir vos horizons :
- **SQLite** : Base de données embarquée (mobile, desktop)
- **PostgreSQL** : Alternative puissante à MySQL
- **Microsoft SQL Server** : L'écosystème Microsoft
- **Oracle** : Pour les grandes entreprises
- Différences et similarités

**Flexibilité** : Le code FireDAC est facilement adaptable à d'autres bases.

### 13. NoSQL et bases de données documentaires

Au-delà du relationnel :
- Comprendre NoSQL (MongoDB, Redis, etc.)
- Quand choisir NoSQL vs SQL
- Utiliser MongoDB avec Delphi
- Bases de données en mémoire

**Tendances modernes** : Les nouvelles approches de stockage de données.

### 14. Améliorations FireDAC de Delphi 13

Les nouveautés :
- Nouvelles fonctionnalités de Delphi 13
- Améliorations de performance
- Nouveaux drivers et compatibilités
- Optimisations

**À jour** : Profiter des dernières innovations.

## Concepts fondamentaux à comprendre

### Qu'est-ce qu'une base de données relationnelle ?

Une base de données relationnelle organise les données en **tables** (comme des feuilles Excel) :

```
Table: Clients
┌────┬─────────────┬──────────────────┬───────────┐
│ ID │ Nom         │ Email            │ Ville     │
├────┼─────────────┼──────────────────┼───────────┤
│ 1  │ Dupont      │ dupont@mail.com  │ Paris     │
│ 2  │ Martin      │ martin@mail.com  │ Lyon      │
│ 3  │ Durand      │ durand@mail.com  │ Marseille │
└────┴─────────────┴──────────────────┴───────────┘

Table: Commandes
┌────┬────────────┬──────────┬─────────┐
│ ID │ ClientID   │ Date     │ Montant │
├────┼────────────┼──────────┼─────────┤
│ 1  │ 1          │ 2025-01  │ 150.00  │
│ 2  │ 1          │ 2025-02  │ 200.00  │
│ 3  │ 2          │ 2025-01  │ 75.50   │
└────┴────────────┴──────────┴─────────┘
```

Les tables sont **reliées** entre elles par des **clés** (ici, ClientID).

### SQL : Le langage des bases de données

**SQL (Structured Query Language)** est le langage universel pour communiquer avec les bases de données :

```sql
-- Lire des données
SELECT * FROM Clients WHERE Ville = 'Paris';

-- Ajouter des données
INSERT INTO Clients (Nom, Email, Ville)
VALUES ('Leroy', 'leroy@mail.com', 'Toulouse');

-- Modifier des données
UPDATE Clients SET Ville = 'Nice' WHERE ID = 2;

-- Supprimer des données
DELETE FROM Clients WHERE ID = 3;
```

**Rassurez-vous** : Vous n'avez pas besoin d'être un expert SQL pour commencer. Nous verrons les bases ensemble.

### Architecture Client-Serveur

```
┌─────────────────┐         ┌─────────────────┐
│  Application    │  ←───→  │  Serveur MySQL  │
│  Delphi         │         │  (Base de       │
│  (Client)       │         │   données)      │
└─────────────────┘         └─────────────────┘
```

- **Client (votre application Delphi)** : Envoie des requêtes SQL
- **Serveur MySQL** : Exécute les requêtes et retourne les résultats
- **Communication** : Via le réseau (même en local)

### FireDAC : La couche d'abstraction

FireDAC se place entre votre application et la base de données :

```
Application Delphi
       ↓
   FireDAC Components
       ↓
   FireDAC Drivers
       ↓
MySQL/MariaDB Server
```

**Avantage** : Vous écrivez le même code Delphi, FireDAC s'occupe de communiquer avec MySQL.

## Ce que vous devez savoir avant de commencer

### Prérequis techniques

✓ **Delphi installé** : Version Community ou supérieure
✓ **Bases de SQL** : Utile mais pas obligatoire (on apprendra ensemble)
✓ **Notions de programmation Delphi** : Variables, classes, événements
✓ **Compréhension des formulaires** : Savoir placer des composants

### Prérequis conceptuels

✓ **Comprendre les fichiers** : Vous savez ce qu'est un fichier sur disque
✓ **Notion de tableau** : Vous comprenez une liste d'éléments
✓ **Logique de base** : IF/THEN, boucles

### Ce que vous n'avez PAS besoin de savoir

✗ Administration de bases de données avancée
✗ Optimisation de requêtes complexes
✗ Architecture de serveurs en production
✗ Théorie mathématique des bases de données

## Installation nécessaire

Pour suivre ce chapitre, vous aurez besoin de :

### 1. Delphi avec FireDAC

FireDAC est inclus dans toutes les éditions de Delphi depuis XE5.
- Community Edition : ✓ Inclus
- Professional : ✓ Inclus
- Enterprise : ✓ Inclus

### 2. MySQL ou MariaDB

Vous devrez installer l'un de ces deux systèmes :

**Option 1 : MySQL Community Server**
- Téléchargement : https://dev.mysql.com/downloads/
- Gratuit et open source
- Version Windows, macOS, Linux

**Option 2 : MariaDB**
- Téléchargement : https://mariadb.org/download/
- Totalement gratuit
- 100% compatible MySQL

**Note** : Nous fournirons un guide d'installation détaillé dans la section 8.2.

### 3. Outil d'administration (optionnel mais recommandé)

Pour visualiser et manipuler vos bases de données :
- **MySQL Workbench** (officiel MySQL)
- **HeidiSQL** (gratuit, léger)
- **DBeaver** (multi-bases, gratuit)
- **phpMyAdmin** (via navigateur web)

## Comment aborder ce chapitre

### Pour les débutants complets en bases de données

**Votre parcours** :
1. Lisez l'introduction et les concepts de base attentivement
2. Installez MySQL/MariaDB en suivant le guide pas à pas
3. Suivez chaque section dans l'ordre, sans sauter d'étapes
4. Tapez TOUS les exemples de code (ne copiez-collez pas)
5. Expérimentez avec vos propres données
6. N'hésitez pas à relire les sections complexes

**Temps estimé** : 2-3 semaines en y consacrant quelques heures par jour

### Pour ceux qui connaissent SQL mais pas Delphi

**Votre avantage** : Vous comprenez déjà les concepts de base de données

**Concentrez-vous sur** :
- Comment FireDAC encapsule les opérations SQL
- Les composants visuels data-aware
- L'architecture en couches avec DataModules
- Les spécificités Delphi (TDataSet, TDataSource, etc.)

**Vous pouvez survoler** :
- Les bases du SQL
- Les concepts de transactions
- La normalisation

### Pour les développeurs Delphi qui ont utilisé d'autres technologies d'accès

**Si vous connaissez** BDE, dbExpress, ou ADO :

**Bonne nouvelle** : Les concepts sont similaires
**Attention** : FireDAC est plus moderne et plus puissant

**Focalisez-vous sur** :
- Les différences architecturales
- Les nouveaux composants FireDAC
- Les améliorations de performance
- Les fonctionnalités avancées

## Structure de l'apprentissage

### Phase 1 : Fondations (Sections 8.1 à 8.4)

**Objectif** : Connecter Delphi à MySQL
- Installation et configuration
- Comprendre FireDAC
- Première connexion réussie

**Résultat** : Vous pouvez vous connecter à une base de données.

### Phase 2 : Manipulation de base (Sections 8.5 à 8.7)

**Objectif** : CRUD (Create, Read, Update, Delete)
- Exécuter des requêtes SQL
- Afficher des données dans des grilles
- Modifier et sauvegarder

**Résultat** : Une application CRUD fonctionnelle simple.

### Phase 3 : Professionnalisation (Sections 8.8 à 8.11)

**Objectif** : Code de qualité professionnelle
- Architecture en couches
- Sécurité
- Bonnes pratiques

**Résultat** : Code maintenable et sécurisé.

### Phase 4 : Horizons élargis (Sections 8.12 à 8.14)

**Objectif** : Ouverture et modernité
- Autres bases de données
- NoSQL
- Nouveautés

**Résultat** : Vision complète de l'écosystème.

## Projet fil rouge : Application de gestion

Tout au long de ce chapitre, nous construirons progressivement une **application de gestion de bibliothèque** :

**Fonctionnalités** :
- Gestion des livres (titre, auteur, ISBN, disponibilité)
- Gestion des membres (nom, email, date d'inscription)
- Gestion des emprunts (qui a emprunté quel livre, dates)
- Recherche et filtres
- Statistiques

**Avantage** : Chaque section ajoutera une fonctionnalité, vous verrez votre application grandir.

## Ressources supplémentaires

### Documentation officielle

- **Delphi FireDAC** : https://docwiki.embarcadero.com/RADStudio/en/FireDAC
- **MySQL** : https://dev.mysql.com/doc/
- **MariaDB** : https://mariadb.com/kb/en/documentation/

### Communautés

- Forums Delphi-Praxis
- Stack Overflow (tag delphi + mysql)
- Reddit r/delphi
- Groupes Facebook Delphi

### Outils utiles

- **SQL Fiddle** : Tester des requêtes SQL en ligne
- **DB Diagram** : Concevoir visuellement vos schémas
- **Mockaroo** : Générer des données de test

## Ce que vous saurez faire après ce chapitre

À la fin de ce chapitre, vous serez capable de :

✓ Installer et configurer MySQL/MariaDB
✓ Connecter une application Delphi à MySQL
✓ Créer des tables et définir leur structure
✓ Exécuter toutes les opérations CRUD
✓ Afficher des données dans des grilles et formulaires
✓ Gérer les relations entre tables (1-N, N-N)
✓ Utiliser les transactions pour garantir l'intégrité
✓ Sécuriser votre application contre les injections SQL
✓ Structurer votre code en couches professionnelles
✓ Créer des applications de gestion complètes
✓ Adapter votre code à d'autres bases de données

## Points importants à retenir

### 1. La sécurité d'abord

**TOUJOURS** utiliser des requêtes paramétrées, **JAMAIS** de concaténation de chaînes dans les requêtes SQL.

```pascal
// ❌ DANGEREUX - Ne JAMAIS faire ça
Query.SQL.Text := 'SELECT * FROM Users WHERE Username = ''' + Edit1.Text + '''';

// ✓ CORRECT - Toujours paramétrer
Query.SQL.Text := 'SELECT * FROM Users WHERE Username = :Username';
Query.ParamByName('Username').AsString := Edit1.Text;
```

### 2. Toujours gérer les erreurs

Les bases de données peuvent échouer pour mille raisons (serveur down, réseau, permissions, etc.).

```pascal
try
  FDConnection1.Connected := True;
except
  on E: Exception do
    ShowMessage('Erreur de connexion : ' + E.Message);
end;
```

### 3. Fermer les connexions

N'oubliez jamais de fermer vos connexions et libérer les ressources.

### 4. Tester avec des données réelles

Ne testez pas qu'avec 10 enregistrements. Testez avec 10 000 pour voir les problèmes de performance.

## Un mot d'encouragement

Les bases de données peuvent sembler intimidantes au début. C'est normal ! Vous entrez dans un domaine qui combine plusieurs technologies (Delphi, SQL, MySQL).

**Mais rassurez-vous** : Des millions de développeurs avant vous ont appris ces concepts. Ils ne sont ni plus intelligents ni plus doués que vous. Ils ont simplement pris le temps de pratiquer.

**Mon conseil** :
- Allez-y étape par étape
- N'ayez pas peur des erreurs (elles sont vos meilleures professeurs)
- Expérimentez autant que possible
- Créez vos propres petits projets
- Amusez-vous !

Dans quelques semaines, vous créerez des applications de gestion de données comme un pro, et vous vous demanderez comment vous avez pu vous passer des bases de données avant.

---

Maintenant, prêt à plonger dans le monde fascinant des bases de données ? Commençons par une vue d'ensemble des technologies d'accès aux données disponibles dans Delphi.

⏭️ [Vue d'ensemble des technologies d'accès aux données](/08-acces-aux-bases-de-donnees-mysql-mariadb/01-vue-densemble-des-technologies-dacces-aux-donnees.md)
