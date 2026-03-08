🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.2 Configuration de MySQL/MariaDB pour Delphi

## Introduction

Avant de pouvoir utiliser MySQL ou MariaDB dans vos applications Delphi, vous devez effectuer quelques étapes de configuration. Ce chapitre vous guide pas à pas dans ce processus, même si vous n'avez jamais installé de serveur de base de données auparavant.

## MySQL ou MariaDB : lequel choisir ?

**MariaDB** et **MySQL** sont deux systèmes de gestion de bases de données très similaires :

- **MySQL** : le système original, développé depuis 1995, maintenant propriété d'Oracle
- **MariaDB** : un fork (dérivé) de MySQL, créé en 2009, 100% compatible avec MySQL, maintenu par la communauté open source

**Pour Delphi, les deux fonctionnent exactement de la même manière.** Les étapes de configuration sont identiques, et vous pouvez utiliser les mêmes composants FireDAC pour les deux.

**Recommandation pour débutants :** Choisissez **MariaDB** car :
- Totalement gratuit et open source
- Installation simple
- Excellente documentation
- Très bien supporté par Delphi via FireDAC
- Compatibilité 100% avec MySQL

Dans ce tutoriel, nous utiliserons principalement le terme "MySQL" pour simplifier, mais tout s'applique également à MariaDB.

## Installation de MySQL/MariaDB

### Option 1 : Installation de MariaDB (recommandée pour débuter)

#### Sous Windows

1. **Téléchargement**
   - Rendez-vous sur [mariadb.org/download](https://mariadb.org/download/)
   - Téléchargez la version stable la plus récente pour Windows (fichier MSI)
   - Choisissez la version 64 bits si votre Windows est en 64 bits

2. **Installation**
   - Lancez le fichier d'installation téléchargé
   - Acceptez la licence
   - Choisissez l'installation "Complete" (complète)
   - **Important** : Notez bien le mot de passe root que vous définissez (vous en aurez besoin !)
   - Cochez "Enable networking" pour permettre les connexions réseau
   - Le port par défaut est **3306** (laissez-le tel quel sauf si vous savez pourquoi le changer)
   - Laissez les autres options par défaut
   - Terminez l'installation

3. **Vérification**
   - Ouvrez le menu Démarrer et cherchez "HeidiSQL" (installé avec MariaDB)
   - Lancez HeidiSQL
   - Une connexion à "localhost" devrait être configurée par défaut
   - Entrez le mot de passe root défini lors de l'installation
   - Cliquez sur "Ouvrir" : si vous voyez les bases de données système, tout fonctionne !

#### Sous macOS

1. **Installation via Homebrew** (méthode la plus simple)
   ```bash
   # Installez Homebrew si ce n'est pas déjà fait
   /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

   # Installez MariaDB
   brew install mariadb

   # Démarrez le service
   brew services start mariadb
   ```

2. **Configuration initiale**
   ```bash
   # Sécurisation de l'installation
   sudo mysql_secure_installation
   ```
   - Répondez aux questions de configuration
   - Définissez un mot de passe root sécurisé

#### Sous Linux (Ubuntu/Debian)

```bash
# Mise à jour des paquets
sudo apt update

# Installation de MariaDB
sudo apt install mariadb-server mariadb-client

# Démarrage du service
sudo systemctl start mariadb  
sudo systemctl enable mariadb  

# Configuration sécurisée
sudo mysql_secure_installation
```

### Option 2 : Installation de MySQL Community Server

Si vous préférez MySQL original :

1. Rendez-vous sur [dev.mysql.com/downloads/mysql](https://dev.mysql.com/downloads/mysql/)
2. Téléchargez MySQL Community Server
3. Suivez l'assistant d'installation (similaire à MariaDB)
4. Notez bien votre mot de passe root

## Configuration de base de données pour votre projet

### Connexion au serveur

Une fois MySQL/MariaDB installé, vous devez vous connecter pour créer votre base de données.

**Outils disponibles :**
- **HeidiSQL** (Windows, installé avec MariaDB) - recommandé pour débuter
- **MySQL Workbench** (multi-plateformes, officiel MySQL)
- **DBeaver** (gratuit, multi-plateformes, supporte de nombreuses bases)
- **Ligne de commande** (pour les utilisateurs avancés)

**Exemple avec HeidiSQL :**
1. Lancez HeidiSQL
2. Créez une nouvelle session avec ces paramètres :
   - Type de réseau : MySQL (TCP/IP)
   - Nom d'hôte : localhost (ou 127.0.0.1)
   - Utilisateur : root
   - Mot de passe : celui défini lors de l'installation
   - Port : 3306
3. Cliquez sur "Ouvrir"

### Création d'une base de données

**Important :** Pour chaque projet, créez une base de données dédiée.

**Méthode graphique (HeidiSQL) :**
1. Clic droit dans la liste des bases à gauche
2. Sélectionnez "Créer nouveau" → "Base de données"
3. Donnez un nom à votre base (par exemple : `ma_gestion` ou `projet_delphi`)
4. Choisissez l'encodage : **utf8mb4** (recommandé pour supporter tous les caractères Unicode)
5. Collation : **utf8mb4_general_ci**
6. Cliquez sur "OK"

**Méthode SQL :**
```sql
CREATE DATABASE ma_gestion  
CHARACTER SET utf8mb4  
COLLATE utf8mb4_general_ci;  
```

### Création d'un utilisateur dédié (recommandé)

**Pourquoi créer un utilisateur spécifique ?**
- **Sécurité** : ne pas utiliser le compte root dans votre application
- **Permissions limitées** : l'utilisateur n'a accès qu'à sa base de données
- **Bonnes pratiques** : chaque application a ses propres identifiants

**Création d'un utilisateur :**

```sql
-- Créer l'utilisateur
CREATE USER 'delphi_user'@'localhost'  
IDENTIFIED BY 'MotDePasseSecurise123!';  

-- Donner tous les droits sur la base de données spécifique
GRANT ALL PRIVILEGES ON ma_gestion.*  
TO 'delphi_user'@'localhost';  

-- Appliquer les changements
FLUSH PRIVILEGES;
```

**Explications :**
- `'delphi_user'@'localhost'` : nom d'utilisateur qui ne peut se connecter que depuis la machine locale
- `IDENTIFIED BY 'MotDePasseSecurise123!'` : le mot de passe (choisissez-en un fort !)
- `GRANT ALL PRIVILEGES ON ma_gestion.*` : droits complets sur la base `ma_gestion` uniquement
- `FLUSH PRIVILEGES` : applique immédiatement les changements

**Testez la connexion avec le nouvel utilisateur :**
- Créez une nouvelle session dans HeidiSQL avec l'utilisateur `delphi_user`
- Vous devriez voir uniquement la base de données `ma_gestion`

## Configuration des pilotes MySQL pour Delphi

FireDAC peut se connecter à MySQL de deux manières différentes :

### Option 1 : Bibliothèque client MySQL native (recommandée)

FireDAC utilise la bibliothèque client MySQL (`libmysql.dll` sous Windows) pour communiquer avec le serveur.

#### Sous Windows

1. **Localiser la DLL**
   - Si vous avez installé MariaDB : la DLL est dans `C:\Program Files\MariaDB XX.X\lib\`
   - Si vous avez installé MySQL : cherchez dans `C:\Program Files\MySQL\MySQL Server X.X\lib\`
   - Le fichier se nomme `libmysql.dll` (MySQL) ou `libmariadb.dll` (MariaDB)

2. **Configuration dans Delphi**

   Vous avez deux options :

   **Option A : Copier la DLL dans le dossier de votre application**
   - Copiez `libmysql.dll` dans le même dossier que votre fichier .exe
   - Avantage : simple, fonctionne immédiatement
   - Inconvénient : vous devez distribuer la DLL avec votre application

   **Option B : Spécifier le chemin dans votre code**
   - Indiquez à FireDAC où trouver la DLL
   - Nous verrons comment faire cela dans la section suivante

3. **Quelle DLL choisir ?**
   - Pour MariaDB : utilisez `libmariadb.dll`
   - Pour MySQL : utilisez `libmysql.dll`
   - Les deux sont compatibles avec Delphi via FireDAC
   - Prenez la version 64 bits si votre application est en 64 bits, 32 bits sinon

#### Sous macOS et Linux

La bibliothèque client est généralement installée automatiquement avec MySQL/MariaDB et accessible dans le système. FireDAC la trouvera automatiquement dans la plupart des cas.

### Option 2 : Pilote ODBC (alternative, moins performant)

FireDAC peut aussi utiliser ODBC, mais c'est moins performant et moins recommandé pour MySQL.

## Vérification de la configuration

Avant de commencer à coder dans Delphi, vérifiez que tout fonctionne :

### Checklist de vérification

✅ **Serveur MySQL/MariaDB installé et démarré**
   - Vérifiez dans les services Windows (services.msc) que le service "MySQL" ou "MariaDB" est démarré
   - Ou utilisez HeidiSQL pour vous connecter

✅ **Base de données créée**
   - Vous avez créé une base de données pour votre projet
   - Encodage : utf8mb4

✅ **Utilisateur dédié créé**
   - Un utilisateur avec mot de passe
   - Permissions accordées sur la base de données

✅ **Bibliothèque client disponible**
   - `libmysql.dll` ou `libmariadb.dll` localisée
   - Compatible avec l'architecture de votre application (32/64 bits)

✅ **Informations de connexion notées**
   - Hôte : localhost
   - Port : 3306
   - Nom de la base : ma_gestion (ou votre nom)
   - Utilisateur : delphi_user (ou votre nom)
   - Mot de passe : ***********

## Paramètres de connexion importants

Notez ces informations, vous en aurez besoin dans Delphi :

| Paramètre | Valeur typique | Description |
|-----------|----------------|-------------|
| **Server** | localhost ou 127.0.0.1 | Adresse du serveur |
| **Port** | 3306 | Port d'écoute MySQL/MariaDB |
| **Database** | ma_gestion | Nom de votre base |
| **User_Name** | delphi_user | Nom d'utilisateur |
| **Password** | ********** | Mot de passe |
| **CharacterSet** | utf8mb4 | Encodage des caractères |

## Configuration du pare-feu (si nécessaire)

Si vous prévoyez d'accéder à MySQL depuis une autre machine :

### Sous Windows
1. Ouvrez le Pare-feu Windows
2. Ajoutez une règle entrante pour le port 3306
3. Autorisez les connexions TCP

### Sous Linux
```bash
# UFW (Ubuntu)
sudo ufw allow 3306/tcp

# FirewallD (CentOS/RHEL)
sudo firewall-cmd --permanent --add-port=3306/tcp  
sudo firewall-cmd --reload  
```

**Note de sécurité :** N'ouvrez le port 3306 vers Internet que si absolument nécessaire et avec des mesures de sécurité appropriées (VPN, SSH tunnel, etc.).

## Conseils de sécurité

### Pour le développement

- Utilisez des mots de passe forts, même en développement (prenez de bonnes habitudes)
- Ne connectez jamais votre application avec le compte `root`
- Créez des utilisateurs avec des permissions minimales nécessaires

### Pour la production

- **Jamais** de mots de passe en clair dans le code source
- Utilisez des fichiers de configuration chiffrés
- Activez SSL/TLS pour les connexions distantes
- Limitez les connexions à des adresses IP spécifiques
- Effectuez des sauvegardes régulières
- Gardez MySQL/MariaDB à jour

## Problèmes courants et solutions

### Le service ne démarre pas
- Vérifiez les logs dans le dossier `data` de MySQL
- Assurez-vous qu'aucun autre programme n'utilise le port 3306
- Vérifiez les permissions sur les dossiers de données

### Impossible de se connecter depuis Delphi
- Vérifiez que le service est bien démarré
- Testez d'abord la connexion avec HeidiSQL
- Assurez-vous que l'utilisateur a les bonnes permissions
- Vérifiez que la bibliothèque client (`libmysql.dll`) est accessible

### Erreur "Access denied"
- Vérifiez le nom d'utilisateur et le mot de passe
- Vérifiez que l'utilisateur a les droits sur la base de données
- Assurez-vous d'utiliser le bon hôte (`localhost` vs `127.0.0.1`)

### Caractères accentués mal affichés
- Vérifiez l'encodage de la base de données (doit être utf8mb4)
- Spécifiez `CharacterSet=utf8mb4` dans les paramètres de connexion

## Outils utiles

### Gestionnaires graphiques recommandés

1. **HeidiSQL** (Windows) - gratuit
   - Simple et intuitif
   - Parfait pour débuter
   - Installé automatiquement avec MariaDB

2. **MySQL Workbench** (multi-plateformes) - gratuit
   - Officiel MySQL
   - Très complet
   - Design de schémas visuels

3. **DBeaver** (multi-plateformes) - gratuit
   - Support multi-bases de données
   - Interface moderne
   - Plugins disponibles

4. **phpMyAdmin** (web)
   - Interface web
   - Pratique pour administrer à distance
   - Nécessite un serveur web (Apache/Nginx + PHP)

## Prochaines étapes

Maintenant que MySQL/MariaDB est configuré et prêt à l'emploi, vous êtes prêt à :

1. Créer votre première connexion dans Delphi avec FireDAC
2. Exécuter vos premières requêtes SQL
3. Manipuler des données depuis votre application

Nous verrons tout cela dans les sections suivantes !

## Résumé

**Ce que vous devez avoir à ce stade :**

✓ MySQL ou MariaDB installé et fonctionnel  
✓ Une base de données créée avec encodage utf8mb4  
✓ Un utilisateur dédié avec permissions appropriées  
✓ La bibliothèque client MySQL/MariaDB accessible  
✓ Les paramètres de connexion notés et testés

**Configuration minimale pour Delphi :**
- Serveur : localhost
- Port : 3306
- Base : votre_base
- Utilisateur : votre_utilisateur
- Mot de passe : votre_mot_de_passe
- CharacterSet : utf8mb4

Vous êtes maintenant prêt à connecter Delphi à votre base de données MySQL/MariaDB !

⏭️ [FireDAC : architecture et composants](/08-acces-aux-bases-de-donnees-mysql-mariadb/03-firedac-architecture-et-composants.md)
