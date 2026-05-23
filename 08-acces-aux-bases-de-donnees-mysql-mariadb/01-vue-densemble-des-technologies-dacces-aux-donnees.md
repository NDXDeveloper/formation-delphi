🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.1 Vue d'ensemble des technologies d'accès aux données

## Introduction

Lorsque vous développez des applications avec Delphi, il est très fréquent d'avoir besoin de stocker et de récupérer des données. Que ce soit pour gérer une liste de clients, un inventaire de produits, ou des informations de facturation, vous aurez besoin de communiquer avec une **base de données**.

Delphi propose plusieurs technologies pour accéder aux bases de données. Chacune a ses avantages et son contexte d'utilisation. Comprendre ces différentes options vous aidera à choisir la meilleure solution pour vos projets.

## Pourquoi plusieurs technologies ?

Au fil des années, Embarcadero (l'éditeur de Delphi) a développé différentes technologies d'accès aux données pour répondre à l'évolution des besoins :
- Support de nouvelles bases de données
- Amélioration des performances
- Simplification du développement
- Compatibilité multi-plateformes

Certaines technologies sont anciennes mais encore utilisées dans des projets existants, tandis que d'autres sont modernes et recommandées pour de nouveaux développements.

## Les principales technologies d'accès aux données

### 1. FireDAC (Fire Data Access Components)

**FireDAC** est la technologie **moderne et recommandée** pour tous les nouveaux projets Delphi.

**Caractéristiques principales :**
- **Multi-plateformes** : fonctionne sur Windows, macOS, iOS, Android et Linux
- **Haute performance** : conçu pour être rapide et efficace
- **Support étendu** : compatible avec plus de 20 bases de données différentes (MySQL, PostgreSQL, SQLite, Oracle, SQL Server, MongoDB, etc.)
- **Architecture unifiée** : une seule API pour toutes les bases de données
- **Fonctionnalités avancées** : mise en cache locale, pooling de connexions, batch updates, etc.

**Quand l'utiliser ?**
- Pour tout nouveau projet Delphi
- Quand vous avez besoin de portabilité multi-plateformes
- Pour des applications mobiles
- Quand les performances sont critiques

**Disponibilité :**
FireDAC est inclus dans toutes les éditions de Delphi, y compris la Community Edition.

### 2. dbExpress (DBX)

**dbExpress** est une technologie plus ancienne, légère et rapide.

**Caractéristiques principales :**
- **Léger** : empreinte mémoire réduite
- **Accès direct** : communication directe avec la base de données
- **Unidirectionnel par défaut** : les curseurs parcourent les données dans un seul sens (améliore les performances)
- Support de plusieurs bases de données

**Limitations :**
- Moins de fonctionnalités que FireDAC
- **Officiellement déprécié** par Embarcadero depuis plusieurs versions (les composants restent disponibles mais ne sont plus activement développés)
- Support limité du multi-plateformes

**Quand l'utiliser ?**
- Pour maintenir des applications existantes utilisant dbExpress
- Dans des cas très spécifiques nécessitant une empreinte mémoire minimale

**Recommandation :** Pour les nouveaux projets, privilégiez FireDAC qui offre plus de fonctionnalités.

### 3. ADO (ActiveX Data Objects)

**ADO** est une technologie Microsoft pour accéder aux données sous Windows.

**Caractéristiques principales :**
- **Spécifique Windows** : ne fonctionne que sur cette plateforme
- Utilise les pilotes OLEDB de Windows
- Bien intégré avec les technologies Microsoft (SQL Server, Access, etc.)

**Limitations :**
- **Uniquement Windows** : impossible à utiliser sur d'autres plateformes
- Dépendance à Microsoft
- Performances parfois inférieures à FireDAC

**Quand l'utiliser ?**
- Pour maintenir des applications Windows existantes utilisant ADO
- Dans des environnements 100% Microsoft où ADO est déjà largement utilisé

**Recommandation :** Pour les nouveaux projets, même sous Windows uniquement, FireDAC est généralement préférable.

### 4. BDE (Borland Database Engine)

**BDE** est la technologie **historique** de Delphi, datant de l'époque Borland.

**Statut actuel :**
- **Obsolète et déprécié** : n'est plus maintenu ni développé
- Encore présent dans certaines très anciennes applications
- Nombreux problèmes de compatibilité avec les systèmes modernes
- Ne fonctionne pas bien avec Windows 10/11 en 64 bits

**Pourquoi vous en entendrez parler ?**
Vous pourriez rencontrer le BDE dans :
- Des applications Delphi très anciennes (années 90 - début 2000)
- Des projets de migration vers des technologies modernes
- De la documentation ancienne

**Recommandation forte :** **Ne jamais utiliser BDE pour de nouveaux projets.** Si vous maintenez une application avec BDE, envisagez une migration vers FireDAC.

### 5. Technologies spécifiques et alternatives

#### SQLite intégré
Delphi (via FireDAC) offre un excellent support pour **SQLite**, une base de données embarquée très populaire :
- Aucun serveur nécessaire
- Fichier de base de données unique
- Parfait pour les applications desktop légères ou mobiles
- Idéal pour le stockage local

#### DataSnap
**DataSnap** n'est pas vraiment une technologie d'accès aux données, mais plutôt un **framework multi-tiers** :
- Permet de créer des serveurs d'application
- Sépare la logique d'accès aux données du client
- Utile pour les architectures distribuées

#### REST et services Web
Pour des architectures modernes, vous pouvez également :
- Accéder aux données via des **API REST**
- Utiliser des **services Web** (SOAP, JSON)
- Delphi fournit d'excellents composants pour cela (TRESTClient, etc.)

## Tableau comparatif

| Technologie | Recommandée ? | Multi-plateformes | Performance | Facilité d'usage |
|-------------|---------------|-------------------|-------------|------------------|
| **FireDAC** | ✅ Oui | ✅ Oui | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **dbExpress** | ⚠️ Maintenance uniquement | ⚠️ Limité | ⭐⭐⭐⭐ | ⭐⭐⭐ |
| **ADO** | ⚠️ Windows uniquement | ❌ Non (Windows) | ⭐⭐⭐ | ⭐⭐⭐⭐ |
| **BDE** | ❌ Non (obsolète) | ❌ Non | ⭐⭐ | ⭐⭐ |

## Quelle technologie choisir ?

La réponse est simple pour la grande majorité des cas :

### 🎯 Utilisez FireDAC !

**FireDAC** est le choix par excellence pour :
- ✅ Tous les nouveaux projets
- ✅ Applications multi-plateformes (Windows, macOS, Linux, mobile)
- ✅ Applications nécessitant de hautes performances
- ✅ Projets avec différents types de bases de données
- ✅ Applications mobiles iOS et Android

### Cas particuliers

- **Maintenance d'anciennes applications** : conservez la technologie existante, mais envisagez une migration progressive vers FireDAC
- **Environnement Microsoft très spécifique** : ADO peut être acceptable, mais FireDAC reste recommandé
- **Applications BDE** : planifiez une migration vers FireDAC dès que possible

## Architecture typique avec FireDAC

Dans ce cours, nous nous concentrerons sur **FireDAC** car c'est la technologie moderne et polyvalente. Voici un aperçu de l'architecture typique :

```
Application Delphi
    ↓
Composants FireDAC
    ↓
Pilotes de base de données
    ↓
Base de données (MySQL, PostgreSQL, SQLite, etc.)
```

**Les composants principaux que vous utiliserez :**
- **TFDConnection** : établit la connexion à la base de données
- **TFDQuery** : exécute des requêtes SQL
- **TFDTable** : accède directement à une table
- **TFDStoredProc** : exécute des procédures stockées
- **TDataSource** : lie les données aux composants visuels

## Conclusion

L'écosystème Delphi offre plusieurs options pour l'accès aux données, mais **FireDAC** s'impose comme le standard moderne. Il combine performance, flexibilité et compatibilité multi-plateformes.

Dans les sections suivantes de ce chapitre, nous approfondirons FireDAC et apprendrons à l'utiliser pour créer des applications robustes connectées à MySQL/MariaDB.

**Points clés à retenir :**
- FireDAC est la technologie recommandée pour les nouveaux projets
- BDE est obsolète et à éviter absolument
- FireDAC supporte plus de 20 bases de données différentes
- Une seule API FireDAC fonctionne sur toutes les plateformes
- Les autres technologies (dbExpress, ADO) sont principalement pour la maintenance de code existant

⏭️ [Configuration de MySQL/MariaDB pour Delphi](/08-acces-aux-bases-de-donnees-mysql-mariadb/02-configuration-de-mysql-mariadb-pour-delphi.md)
