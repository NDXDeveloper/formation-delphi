# 8.1 Vue d'ensemble des technologies d'accès aux données

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'accès aux bases de données est l'une des fonctionnalités les plus importantes dans le développement d'applications professionnelles. Delphi offre plusieurs technologies robustes pour se connecter et manipuler des bases de données, ce qui en fait un choix excellent pour développer des applications de gestion.

Dans cette section, nous allons explorer les différentes méthodes d'accès aux données disponibles dans Delphi, avec un focus particulier sur MySQL/MariaDB.

## Les technologies d'accès aux données dans Delphi

Delphi a connu plusieurs générations de technologies d'accès aux données. Voici les principales :

### 1. FireDAC (FireDAC Universal Data Access Components)

FireDAC est la technologie d'accès aux données la plus récente et la plus puissante de Delphi. C'est celle que nous recommandons pour tous les nouveaux projets.

**Avantages de FireDAC :**
- Support natif d'un grand nombre de bases de données (MySQL/MariaDB, SQLite, PostgreSQL, Oracle, SQL Server, etc.)
- Performances très élevées
- Fonctionnalités avancées (mise en cache, transactions, batches)
- Support complet du mode déconnecté
- Interface unifiée quelle que soit la base de données utilisée
- Support multi-plateforme (Windows, macOS, iOS, Android, Linux)

FireDAC est inclus dans toutes les éditions de Delphi, y compris la Community Edition.

### 2. dbExpress

dbExpress est une technologie plus légère qui était très utilisée avant FireDAC. Elle est toujours disponible mais moins recommandée pour les nouveaux projets.

**Caractéristiques de dbExpress :**
- Pilotes légers et rapides
- Principalement conçu pour les applications client-serveur
- Moins de fonctionnalités que FireDAC

### 3. ADO (ActiveX Data Objects)

ADO est une technologie Microsoft qui permet d'accéder à de nombreuses sources de données via ODBC ou OLE DB.

**Caractéristiques d'ADO :**
- Uniquement disponible sous Windows
- Utilise les pilotes ODBC ou OLE DB de Microsoft
- Bonne intégration avec les produits Microsoft

### 4. Interbase / IBX

Composants spécifiques pour accéder aux bases de données InterBase et Firebird (un fork open-source d'InterBase).

### 5. Autres technologies tierces

De nombreuses bibliothèques tierces existent pour l'accès aux données, comme UniDAC, AnyDAC (l'ancêtre de FireDAC), etc.

## Architecture générale de l'accès aux données dans Delphi

Quelle que soit la technologie utilisée, Delphi utilise une architecture en couches pour l'accès aux données :

1. **Couche de connexion** : Gère la connexion physique à la base de données (ex : `TFDConnection` pour FireDAC)
2. **Couche de commande** : Permet d'exécuter des requêtes SQL (ex : `TFDQuery`, `TFDCommand`)
3. **Couche de dataset** : Fournit une interface uniforme pour manipuler les données (tous les composants héritent de `TDataSet`)
4. **Couche de liaison** : Lie les contrôles visuels aux données (`TDataSource`)
5. **Contrôles liés aux données** : Composants visuels qui affichent et permettent d'éditer les données (`TDBGrid`, `TDBEdit`, etc.)

![Architecture d'accès aux données](https://placeholder.pics/svg/500x300/DEDEDE/555555/Architecture%20acc%C3%A8s%20donn%C3%A9es)

## Modes de fonctionnement

Delphi peut travailler avec les données selon deux modes principaux :

### Mode connecté
- Connexion permanente à la base de données
- Les modifications sont immédiatement envoyées à la base de données
- Idéal pour les applications avec peu d'utilisateurs ou un réseau rapide

### Mode déconnecté
- Les données sont chargées en mémoire
- L'utilisateur travaille sur une copie locale des données
- Les modifications sont validées en bloc (en batch)
- Idéal pour les applications mobiles ou avec de nombreux utilisateurs

## FireDAC et MySQL/MariaDB

Pour MySQL/MariaDB, FireDAC offre un support natif via le pilote `MySQL`. Ce pilote est inclus dans toutes les éditions de Delphi et ne nécessite pas de bibliothèques externes particulières, bien qu'il soit nécessaire d'avoir le client MySQL installé sur la machine.

Voici un exemple simple de connexion à MySQL avec FireDAC :

```delphi
// Création d'une connexion FireDAC à MySQL
FDConnection1.DriverName := 'MySQL';
FDConnection1.Params.Add('Server=localhost');
FDConnection1.Params.Add('Database=ma_base');
FDConnection1.Params.Add('User_Name=utilisateur');
FDConnection1.Params.Add('Password=mot_de_passe');
FDConnection1.Connected := True;
```

Dans les sections suivantes, nous explorerons en détail comment configurer et utiliser FireDAC avec MySQL/MariaDB.

## Les composants essentiels pour l'accès aux données

Voici les composants que vous utiliserez le plus souvent pour l'accès aux données avec FireDAC :

| Composant | Description |
|-----------|-------------|
| `TFDConnection` | Gère la connexion à la base de données |
| `TFDQuery` | Exécute des requêtes SQL et récupère les résultats |
| `TFDTable` | Accède directement à une table de la base de données |
| `TFDUpdateSQL` | Personnalise les opérations de mise à jour |
| `TFDStoredProc` | Exécute des procédures stockées |
| `TFDTransaction` | Gère les transactions |
| `TDataSource` | Fait le lien entre les données et les contrôles visuels |

## Conclusion

Delphi propose des solutions robustes et performantes pour l'accès aux données, avec FireDAC comme technologie phare actuelle. Dans les sections suivantes, nous verrons comment configurer MySQL/MariaDB pour l'utiliser avec Delphi, puis nous explorerons en détail les différentes fonctionnalités de FireDAC pour créer des applications de gestion complètes.

---

**À suivre :** 8.2 Configuration de MySQL/MariaDB pour Delphi

⏭️ [Configuration de MySQL/MariaDB pour Delphi](08-acces-aux-bases-de-donnees-mysql-mariadb/02-configuration-de-mysql-mariadb-pour-delphi.md)
