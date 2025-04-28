## Table des Matières

### 1. [Introduction à Delphi](#01-introduction-a-delphi)
- 1.1 Qu'est-ce que Delphi ?
- 1.2 Histoire et évolutions
- 1.3 Versions disponibles et éditions (Community Edition incluse)
- 1.4 Avantages et cas d'utilisation
- 1.5 Installation et configuration
- 1.6 Premier aperçu de l'environnement
- 1.7 Comparaison avec d'autres environnements de développement

### 2. [Découverte de l'IDE Delphi](#02-decouverte-de-lide-delphi)
- 2.1 Présentation de l'interface
- 2.2 Création d'un premier projet
- 2.3 La Palette d'outils et l'Inspecteur d'objets
- 2.4 Explorateur de projets et gestionnaire de code
- 2.5 Compilation et exécution
- 2.6 Personnalisation de l'IDE
- 2.7 Structure d'un projet Delphi
- 2.8 Introduction au cycle de développement avec Delphi
- 2.9 Utilisation du Gestionnaire de Packages (GetIt Package Manager)

### 3. [Langage Object Pascal](#03-langage-object-pascal)
- 3.1 Syntaxe fondamentale
- 3.2 Types de données et conversions
- 3.3 Variables, constantes et opérateurs
- 3.4 Structures de contrôle (conditions, boucles)
- 3.5 Procédures et fonctions
- 3.6 Gestion des exceptions
- 3.7 Programmation orientée objet
    - 3.7.1 Classes et objets
    - 3.7.2 Propriétés et méthodes
    - 3.7.3 Héritage et polymorphisme
    - 3.7.4 Constructeurs et destructeurs
    - 3.7.5 Interfaces
    - 3.7.6 Généricité
- 3.8 Modèles de conception (Design Patterns)
- 3.9 Organisation du code source et modularité
- 3.10 Nouveautés de la syntaxe Object Pascal (dernières versions)
- 3.11 Records avancés et opérateurs surchargés

### 4. [Conception d'Interfaces Utilisateur avec la VCL](#04-conception-dinterfaces-utilisateur-avec-la-vcl)
- 4.1 Qu'est-ce que la VCL (Visual Component Library) ?
- 4.2 Formulaires et fiches
- 4.3 Composants standard et leur utilisation
    - 4.3.1 Contrôles d'affichage (Labels, Images)
    - 4.3.2 Contrôles de saisie (Edit, Memo, ComboBox)
    - 4.3.3 Boutons et actions
    - 4.3.4 Listes et grilles
- 4.4 Contrôles avancés (PageControl, TreeView, ListView)
- 4.5 Menus et barres d'outils
- 4.6 Gestion des événements
- 4.7 Création de dialogues personnalisés
- 4.8 Développement de composants personnalisés
- 4.9 Migration depuis des versions précédentes de Delphi
- 4.10 Styles visuels et thèmes VCL
- 4.11 Accessibilité des interfaces utilisateur
- 4.12 Interfaces haute résolution et prise en charge du DPI

### 5. [Développement multi-plateforme avec FireMonkey (FMX)](#05-developpement-multi-plateforme-avec-firemonkey)
- 5.1 Introduction à FireMonkey
- 5.2 Différences entre VCL et FMX
- 5.3 Création d'interfaces multi-plateformes
- 5.4 Styles et apparence
- 5.5 Adaptation aux différentes tailles d'écran
- 5.6 Gestion du tactile et des gestes
- 5.7 Ciblage des plateformes : Windows, macOS, iOS, Android, Linux
- 5.8 Performances et optimisation mobile
- 5.9 Animations et effets visuels
- 5.10 FMXLinux : développement pour Linux

### 6. [Applications multi-fenêtres et navigation](#06-applications-multi-fenetres-et-navigation)
- 6.1 Gestion des formulaires multiples
- 6.2 Communication entre formulaires
- 6.3 Formulaires MDI (Multiple Document Interface)
- 6.4 Boîtes de dialogue standard et personnalisées
- 6.5 Assistants (Wizards)
- 6.6 Navigation dans les applications mobiles
- 6.7 Gestion de l'état de l'application

### 7. [Gestion des fichiers et flux de données](#07-gestion-des-fichiers-et-flux-de-donnees)
- 7.1 Lecture/écriture de fichiers texte
- 7.2 Manipulation de fichiers binaires
- 7.3 Utilisation des TStream et classes dérivées
- 7.4 Sérialisation et persistance d'objets
- 7.5 Compression et décompression
- 7.6 Traitement par lots (Batch)
- 7.7 Utilisation de formats modernes (JSON, XML, YAML)
- 7.8 Manipulation de fichiers CSV et Excel

### 8. [Accès aux bases de données MySQL/MariaDB](#08-acces-aux-bases-de-donnees-mysql-mariadb)
- 8.1 Vue d'ensemble des technologies d'accès aux données
- 8.2 Configuration de MySQL/MariaDB pour Delphi
- 8.3 FireDAC : architecture et composants
- 8.4 Connexion à une base MySQL/MariaDB
- 8.5 Manipulation des données
    - 8.5.1 Requêtes SQL et paramétrées
    - 8.5.2 CRUD : Create, Read, Update, Delete
    - 8.5.3 Transactions et intégrité des données
- 8.6 DataSets et DataSources
- 8.7 Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)
- 8.8 Live Bindings et liaison de données visuelle
- 8.9 Modèle en couches pour l'accès aux données
- 8.10 Migration et synchronisation de bases de données
- 8.11 Sécurisation des accès et prévention des injections SQL
- 8.12 Autres moteurs de bases de données (SQLite, PostgreSQL, SQL Server)
- 8.13 NoSQL et bases de données documentaires

### 9. [Rapports et impressions](#09-rapports-et-impressions)
- 9.1 Composants d'impression natifs
- 9.2 Aperçu avant impression
- 9.3 Générateurs de rapports (FastReport, QuickReport)
- 9.4 Création de rapports complexes
- 9.5 Graphiques et visualisations de données
- 9.6 Exportation vers différents formats (PDF, Excel, HTML...)
- 9.7 Rapports interactifs
- 9.8 Graphiques et tableaux de bord avec TeeChart

### 10. [Communication et services réseaux](#10-communication-et-services-reseaux)
- 10.1 Appels REST et API Web (TRESTClient)
- 10.2 Manipulation de JSON et XML
- 10.3 Socket et communications TCP/IP
- 10.4 Services SOAP et WebServices
- 10.5 Architecture client-serveur
- 10.6 Applications distribuées
- 10.7 OAuth2 et authentification moderne
- 10.8 GraphQL et nouvelles API
- 10.9 Intégration avec les services cloud (AWS, Azure, Google Cloud)
- 10.10 WebSockets et communications temps réel

### 11. [Multithreading et programmation asynchrone](#11-multithreading-et-programmation-asynchrone)
- 11.1 Principes du multithreading
- 11.2 Création et gestion de threads
- 11.3 Synchronisation et sections critiques
- 11.4 TTask et programmation parallèle
- 11.5 Tâches asynchrones et callbacks
- 11.6 Files d'attente et pools de threads
- 11.7 Interface utilisateur réactive
- 11.8 Cas d'usage concrets
- 11.9 Programmation réactive avec le pattern Observer
- 11.10 Performances et bonnes pratiques en multithreading

### 12. [Débogage et tests](#12-debogage-et-tests)
- 12.1 Utilisation des points d'arrêt
- 12.2 Inspection et modification des variables
- 12.3 Test unitaire avec DUnit/DUnitX
- 12.4 Profilage et optimisation des performances
- 12.5 Gestion des exceptions et journalisation
- 12.6 Débogage à distance
- 12.7 Tests d'intégration
- 12.8 Mocking et tests avec dépendances
- 12.9 Déboggage de code multi-thread
- 12.10 Couverture de code et qualité

### 13. [Internationalisation et localisation](#13-internationalisation-et-localisation)
- 13.1 Gestion des chaînes de caractères
- 13.2 Ressources linguistiques
- 13.3 Adaptation à différentes langues
- 13.4 Formats de date, heure et nombres
- 13.5 Tests de l'internationalisation
- 13.6 Support Unicode et encodages
- 13.7 Gestion des écritures bidirectionnelles (RTL)
- 13.8 Outils de traduction et flux de travail

### 14. [Utilisation d'API et bibliothèques externes](#14-utilisation-dapi-et-bibliotheques-externes)
- 14.1 Appels aux DLLs
- 14.2 Intégration de bibliothèques C/C++
- 14.3 API Windows natif
- 14.4 COM et ActiveX
- 14.5 Intégration avec des services tiers
- 14.6 Liaisons avec d'autres langages
- 14.7 Liaison avec des API REST tierces
- 14.8 Encapsulation d'API natives pour multi-plateformes
- 14.9 Intégration de bibliothèques JavaScript via WebView

### 15. [Applications mobiles avec Delphi](#15-applications-mobiles-avec-delphi)
- 15.1 Spécificités du développement mobile
- 15.2 Interface utilisateur tactile
- 15.3 Accès aux capteurs (GPS, accéléromètre...)
- 15.4 Utilisation de la caméra et des médias
- 15.5 Notifications
- 15.6 Stockage local et synchronisation
- 15.7 Publication sur App Store / Play Store
- 15.8 Mises à jour OTA (Over The Air)
- 15.9 Partage de code entre applications mobile et desktop
- 15.10 Permissions et confidentialité des données
- 15.11 Intégration des services Firebase

### 16. [Sécurité des applications](#16-securite-des-applications)
- 16.1 Authentification des utilisateurs
- 16.2 Autorisation et contrôle d'accès
- 16.3 Chiffrement des données
- 16.4 Sécurisation des connexions
- 16.5 Protection contre les vulnérabilités courantes
- 16.6 Audit de sécurité
- 16.7 Stockage sécurisé des identifiants
- 16.8 GDPR et confidentialité des données
- 16.9 Signature numérique et validation
- 16.10 Sécurité des applications mobiles

### 17. [Distribution et déploiement](#17-distribution-et-deploiement)
- 17.1 Compilation en mode release
- 17.2 Optimisation du code final
- 17.3 Création d'installateurs (Inno Setup, InstallAware)
- 17.4 Signature de code
- 17.5 Mise à jour automatique
- 17.6 Déploiement sur différentes plateformes
- 17.7 Virtualisation et conteneurs Docker
- 17.8 MSI et Windows Store
- 17.9 Déploiement continu (CI/CD)
- 17.10 Télémétrie et analyse de crash

### 18. [Architecture et bonnes pratiques](#18-architecture-et-bonnes-pratiques)
- 18.1 Structuration d'un projet Delphi
- 18.2 Patterns d'architecture (MVC, MVVM)
- 18.3 Séparation UI / logique métier
- 18.4 Gestion de la configuration
- 18.5 Versionnement et gestion de code source
- 18.6 Documentation du code
- 18.7 Revue de code et refactoring
- 18.8 Intégration avec Git et CI/CD
- 18.9 Clean Code et principes SOLID
- 18.10 Domain-Driven Design (DDD) avec Delphi
- 18.11 Microservices et architecture distribuée

### 19. [Projets avancés](#19-projets-avances)
- 19.1 Application de gestion complète avec MySQL/MariaDB
    - 19.1.1 Conception de la base de données
    - 19.1.2 Implémentation des couches d'accès
    - 19.1.3 Interface utilisateur évoluée
    - 19.1.4 Rapports et tableaux de bord
- 19.2 Application multi-plateformes avec FMX
- 19.3 Applications mobiles avec fonctionnalités avancées
- 19.4 Services Windows et applications de fond
- 19.5 Applications cloud et SaaS
- 19.6 Applications PWA (Progressive Web Apps) avec Delphi
- 19.7 Projets d'intelligence artificielle et machine learning
- 19.8 Intégration de plateformes de paiement

### 20. [Ressources et communauté](#20-ressources-et-communaute)
- 20.1 Documentation officielle
- 20.2 Forums et groupes d'entraide
- 20.3 Bibliothèques et composants tiers
- 20.4 Conférences et événements
- 20.5 Blogs et chaînes YouTube
- 20.6 Livres et formations
- 20.7 Rester à jour avec Delphi
- 20.8 Exemples de projets open source Delphi
- 20.9 Communauté francophone Delphi
- 20.10 Contribution aux projets Delphi

### 21. [Delphi et l'Internet des Objets (IoT)](#21-delphi-et-liot)
- 21.1 Introduction à l'IoT avec Delphi
- 21.2 Communication Bluetooth / série
- 21.3 Intégration avec Arduino / Raspberry Pi
- 21.4 Contrôle de périphériques externes
- 21.5 Protocoles IoT (MQTT, CoAP)
- 21.6 Gestion de dispositifs connectés
- 21.7 Traitement des données IoT en temps réel
- 21.8 Tableaux de bord pour solutions IoT

### 22. [Intelligence Artificielle et Machine Learning avec Delphi](#22-intelligence-artificielle-et-machine-learning-avec-delphi)
- 22.1 Introduction à l'IA et au ML dans les applications Delphi
- 22.2 Intégration avec TensorFlow et autres bibliothèques ML
- 22.3 Traitement du langage naturel (NLP)
- 22.4 Reconnaissance d'images et de formes
- 22.5 Développement de modèles prédictifs
- 22.6 Intégration avec des services d'IA cloud (Azure AI, Google AI, etc.)
- 22.7 Utilisation des grands modèles de langage (LLM) via API

### 23. [Conception d'applications Web avec Delphi](#23-conception-dapplications-web-avec-delphi)
- 23.1 Introduction à Intraweb et TMS Web Core
- 23.2 Applications Web basées sur VCL
- 23.3 Création de services REST avec Delphi
- 23.4 Utilisation de WebBroker et DataSnap
- 23.5 Développement de sites Web dynamiques
- 23.6 Intégration avec des frameworks JavaScript
- 23.7 Progressive Web Apps (PWA)
- 23.8 WebAssembly et Delphi

### 24. [Tendances et futur de Delphi](#24-tendances-et-futur-de-delphi)
- 24.1 Évolution récente de l'écosystème Delphi
- 24.2 Roadmap et orientations futures
- 24.3 Low-code et RAD moderne
- 24.4 Compétitivité dans le paysage technologique actuel
- 24.5 Migration et modernisation d'applications Delphi
- 24.6 Intégration avec les nouvelles technologies émergentes
