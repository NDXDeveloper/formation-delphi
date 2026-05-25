🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17. Distribution et déploiement

## Introduction

Félicitations ! Vous avez développé votre application Delphi, elle fonctionne correctement sur votre machine de développement, et vous êtes prêt à la partager avec vos utilisateurs. C'est maintenant que commence une nouvelle phase cruciale : la **distribution et le déploiement**.

Ce chapitre vous guidera à travers toutes les étapes nécessaires pour transformer votre projet Delphi en une application prête à être installée et utilisée par d'autres personnes, que ce soit sur un seul ordinateur ou sur des milliers de machines à travers le monde.

## Qu'est-ce que la distribution et le déploiement ?

### Distribution

La **distribution** désigne l'ensemble des processus qui permettent de rendre votre application accessible à vos utilisateurs finaux. Cela inclut :

- La préparation de votre application pour la production
- La création de packages d'installation
- La mise à disposition de l'application (téléchargement, magasins d'applications, médias physiques, etc.)
- La gestion des mises à jour

### Déploiement

Le **déploiement** fait référence à l'installation et à la configuration de votre application sur les machines des utilisateurs. Il englobe :

- L'installation de l'application et de ses dépendances
- La configuration initiale
- L'intégration avec le système d'exploitation
- La gestion des paramètres et des données utilisateur

## Pourquoi cette étape est-elle importante ?

Beaucoup de développeurs débutants négligent l'importance de la distribution et du déploiement. Pourtant, une application peut être techniquement excellente mais échouer simplement parce qu'elle est difficile à installer ou à mettre à jour.

### Première impression

L'installation est la première interaction de l'utilisateur avec votre application. Une installation simple, rapide et sans erreur crée une première impression positive.

### Professionnalisme

Un processus de distribution bien conçu démontre le sérieux et le professionnalisme de votre travail. Les utilisateurs font davantage confiance à une application qui s'installe proprement et respecte les conventions de leur système d'exploitation.

### Maintenance et support

Un bon système de distribution facilite grandement la maintenance de votre application. Les mises à jour peuvent être déployées facilement, les problèmes peuvent être corrigés rapidement, et vous gardez le contrôle sur les versions utilisées.

### Sécurité

La distribution et le déploiement incluent des aspects de sécurité importants : signature de code, vérification de l'intégrité, protection contre les modifications malveillantes, etc.

## Le cycle de vie d'une application

Pour bien comprendre la distribution et le déploiement, il est utile de visualiser le cycle de vie complet d'une application :

### 1. Développement

C'est la phase que vous connaissez bien : vous écrivez du code, testez, corrigez les bugs, ajoutez des fonctionnalités. Votre application fonctionne dans l'**environnement de développement**.

### 2. Préparation à la production

Avant de distribuer votre application, vous devez la préparer :
- Compiler en mode optimisé (Release)
- Supprimer les fonctionnalités de débogage
- Optimiser les performances
- Valider la sécurité

### 3. Packaging

Vous créez un package d'installation contenant :
- Votre exécutable
- Les bibliothèques nécessaires (DLL, etc.)
- Les fichiers de ressources (images, bases de données, etc.)
- La documentation utilisateur
- Les scripts d'installation

### 4. Distribution

Vous mettez l'application à disposition :
- Téléchargement sur un site web
- Publication sur des magasins d'applications
- Distribution sur médias physiques
- Déploiement sur réseau d'entreprise

### 5. Installation

L'utilisateur installe l'application sur sa machine. Le programme d'installation :
- Copie les fichiers aux bons emplacements
- Configure le système
- Crée des raccourcis
- Enregistre l'application

### 6. Utilisation

L'utilisateur travaille avec votre application. Pendant cette phase, vous pouvez :
- Collecter des données d'utilisation (télémétrie)
- Détecter les erreurs à distance
- Surveiller les performances

### 7. Mise à jour

Vous publiez de nouvelles versions pour :
- Corriger des bugs
- Ajouter des fonctionnalités
- Améliorer les performances
- Mettre à jour la sécurité

### 8. Désinstallation

Lorsque l'utilisateur n'a plus besoin de l'application, il peut la désinstaller proprement, en supprimant tous les fichiers et paramètres.

## Les défis de la distribution

Distribuer une application Delphi présente plusieurs défis qu'il faut anticiper :

### Diversité des environnements

Votre application devra fonctionner sur de nombreuses configurations différentes :
- **Systèmes d'exploitation Windows** : Windows 10 (fin de support gratuit le 14 octobre 2025), Windows 11 (24H2 actuel en 2026), Windows Server 2016/2019/2022/2025
- **Autres plateformes** (selon votre cible Delphi 13 Florence) : macOS 14+ (Sonoma minimum recommandé en 2026, macOS 26 Tahoe disponible), iOS 17+ (iOS 15 = floor Apple), Android 8+ (API 26), Linux (Ubuntu 22.04 LTS+, Debian 12+, RHEL 9/10) — voir section 17.6
- **Architectures** : x86 32 bits (Win32), x86_64 (Win64/Linux64), ARM64 (Windows on ARM, Apple Silicon, Android arm64-v8a, iOS arm64)
- **Langues** : différentes localisations et paramètres régionaux (formats de date, séparateurs décimaux, jeu de caractères)
- **Matériel** : processeurs variés, quantités de mémoire différentes, résolutions d'écran et facteurs DPI variables (du Full HD au 4K/8K)

### Dépendances

Votre application peut nécessiter des composants externes :
- **Bibliothèques système** : la plupart des DLL Windows sont déjà présentes sur les systèmes cibles (`kernel32.dll`, `user32.dll`, `gdi32.dll`, `ucrtbase.dll`…). À vérifier avec **Dependencies** (cf 17.6).
- **Runtime Delphi** : par défaut, Delphi compile en **statique** (tout est embarqué dans l'EXE) — aucun runtime à distribuer, contrairement à .NET ou Java. Sauf si vous utilisez explicitement des **runtime packages** (`.bpl`), ce qui est rare en production.
- **Bases de données** : drivers nécessaires si vous accédez à une BD externe (`libmysql.dll`, `sqlite3.dll`, `libpq.dll`, Oracle OCI, etc.). FireDAC charge ces drivers à l'exécution.
- **Frameworks tiers** : `.NET` (si vous appelez du code .NET via COM Interop ou Hydra), **Visual C++ Redistributable** (si vous utilisez des DLL C++ tierces), **WebView2 Runtime** (si vous utilisez `TEdgeBrowser`). **Pour du Delphi pur, ces frameworks ne sont pas nécessaires** — Delphi compile en code natif autonome.

### Sécurité et confiance

Les utilisateurs modernes sont (à juste titre) méfiants envers les logiciels inconnus :
- **Signature numérique** : Prouver l'authenticité de votre application
- **Certificats** : Établir la confiance
- **Réputation** : SmartScreen, antivirus, etc.

### Conformité légale

Selon votre type d'application et votre marché, vous devrez peut-être respecter :
- **RGPD** : Protection des données en Europe
- **Licences** : Conditions d'utilisation, licences open source
- **Accessibilité** : Normes d'accessibilité pour les personnes handicapées

## Stratégies de distribution

Il existe plusieurs approches pour distribuer votre application Delphi :

### Distribution directe

Vous fournissez directement l'application à vos utilisateurs :
- **Téléchargement depuis votre site web** : Simple et direct, vous gardez le contrôle
- **Livraison sur médias physiques** : CD, DVD, clés USB (de moins en moins courant)
- **Réseau d'entreprise** : Déploiement via des outils d'administration système

**Avantages** : Contrôle total, pas de commission  
**Inconvénients** : Vous gérez tout (hébergement, support, mises à jour)  

### Magasins d'applications

Publication sur des plateformes établies :
- **Microsoft Store** : pour les applications Windows
- **Mac App Store** : pour les applications macOS
- **App Store** et **Google Play** : pour les apps mobiles iOS/Android (cf section 17.6)
- **Steam** : pour les jeux et certaines applications
- **Setapp** (macOS) : abonnement à un ensemble d'applications

**Avantages** : visibilité, confiance des utilisateurs, gestion automatique des mises à jour  
**Inconvénients** : commission (12 % Microsoft Store non-jeu, 15-30 % Apple/Google selon revenus), processus de validation, règles strictes  

### Distribution d'entreprise

Pour les applications en entreprise :
- **Déploiement MSI** : Via Group Policy ou SCCM
- **Portails d'entreprise** : Catalogues d'applications internes
- **Conteneurs** : Docker pour certaines applications serveur

**Avantages** : Contrôle centralisé, déploiement à grande échelle  
**Inconvénients** : Complexité technique, infrastructure requise  

## Vue d'ensemble de ce chapitre

Dans les sections suivantes de ce chapitre, nous explorerons en détail chaque aspect de la distribution et du déploiement :

### 17.1 Compilation en mode release
Apprenez à préparer votre application pour la production en utilisant les bonnes options de compilation.

### 17.2 Optimisation du code final
Techniques pour améliorer les performances et réduire la taille de votre application.

### 17.3 Création d'installateurs
Comment créer des programmes d'installation professionnels avec Inno Setup ou InstallAware.

### 17.4 Signature de code
Sécurisez votre application avec des certificats numériques pour établir la confiance.

### 17.5 Mise à jour automatique
Implémentez un système de mise à jour automatique pour faciliter la maintenance.

### 17.6 Déploiement sur différentes plateformes
Adaptez votre stratégie de distribution pour Windows, macOS, iOS, Android et Linux.

### 17.7 Virtualisation et conteneurs Docker
Explorez les approches modernes de déploiement avec la conteneurisation.

### 17.8 MSI et Windows Store
Préparez votre application pour une distribution professionnelle sur Windows.

### 17.9 Déploiement continu (CI/CD)
Automatisez le processus de compilation, test et déploiement.

### 17.10 Télémétrie et analyse de crash
Surveillez votre application en production et identifiez rapidement les problèmes.

## Prérequis pour ce chapitre

Avant de commencer, assurez-vous d'avoir :

- Une application Delphi fonctionnelle et testée
- Une compréhension de base de votre public cible et de leurs besoins
- Accès à un environnement de test représentatif de celui de vos utilisateurs
- (Optionnel) Un site web ou une plateforme pour héberger votre application

## Conseils généraux

Tout au long de ce chapitre, gardez à l'esprit ces principes fondamentaux :

### Simplicité avant tout

L'installation doit être aussi simple que possible. Idéalement, l'utilisateur clique sur "Suivant" quelques fois et c'est terminé.

### Testez sur des machines vierges

Ne testez jamais uniquement sur votre machine de développement ! Utilisez des machines virtuelles ou des ordinateurs propres pour valider l'installation.

### Documentez le processus

Même si votre installation est simple, fournissez une documentation claire : configuration requise, étapes d'installation, résolution des problèmes courants.

### Prévoyez la désinstallation

Une bonne application doit pouvoir être désinstallée proprement, sans laisser de fichiers orphelins ou de clés de registre inutiles.

### Pensez aux mises à jour dès le début

Il est beaucoup plus facile d'implémenter un système de mise à jour dès la première version que de l'ajouter plus tard.

## Checklist avant distribution

Avant de distribuer votre application, vérifiez ces points essentiels :

- [ ] L'application a été testée en mode Release (cf 17.1).
- [ ] Toutes les fonctionnalités ont été validées (golden path + cas d'erreur).
- [ ] Les **informations de débogage embarquées** ont été retirées de l'EXE, mais le **fichier MAP est archivé** (indispensable pour décoder les crashes en production, cf 17.10).
- [ ] Les dépendances nécessaires sont identifiées (vérifiées avec **Dependencies** ou ProcessMonitor, cf 17.6).
- [ ] **L'installateur est signé numériquement** (cf 17.4 — obligatoire pour éviter le blocage SmartScreen).
- [ ] **Test sur la cible principale 2026** (Windows 11 24H2 minimum), idéalement aussi sur Windows 10 22H2 si vous gardez ce support.
- [ ] **Hash SHA-256 publié** à côté du téléchargement (pour permettre la vérification d'intégrité par l'utilisateur).
- [ ] La documentation utilisateur est prête (README, FAQ).
- [ ] Les informations de licence sont claires (LICENSE.txt + mention dans l'installateur).
- [ ] Un système de support utilisateur est en place (email, forum, ticketing).
- [ ] La politique de confidentialité est définie (obligatoire si vous collectez la moindre donnée — RGPD, cf 17.10).
- [ ] Les sauvegardes de votre code source sont sécurisées (Git distant, sauvegarde locale).
- [ ] **Plan de mise à jour** prévu (cf 17.5 — l'ajouter dans v2 est plus coûteux que dans v1).
- [ ] **Plan de rollback** documenté (que faire si la version livrée a un bug critique ?).

## Conclusion de l'introduction

La distribution et le déploiement représentent l'aboutissement de votre travail de développement. C'est le moment où votre application quitte l'environnement protégé de votre machine de développement pour entrer dans le monde réel, avec toute sa diversité et ses défis.

Une distribution bien planifiée et exécutée fait la différence entre une application qui reste confidentielle et une application qui rencontre son public. Prenez le temps de bien maîtriser ces concepts, et votre application aura toutes les chances de réussir.

Dans les sections suivantes, nous allons examiner en détail chaque étape du processus. Commençons par la compilation en mode Release, première étape essentielle pour préparer votre application à la distribution.

⏭️ [Compilation en mode release](/17-distribution-et-deploiement/01-compilation-en-mode-release.md)
