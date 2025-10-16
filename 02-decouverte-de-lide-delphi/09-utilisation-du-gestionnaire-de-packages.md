🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.9 Utilisation du Gestionnaire de Packages (GetIt Package Manager)

## Introduction

Delphi est livré avec une bibliothèque de composants très riche : VCL pour Windows, FireMonkey pour le multi-plateforme, et de nombreux composants pour l'accès aux données, les communications réseau, etc. Mais parfois, vous aurez besoin de fonctionnalités supplémentaires : graphiques avancés, composants d'interface moderne, bibliothèques spécialisées, etc.

C'est là qu'intervient **GetIt Package Manager**, le gestionnaire de packages intégré à Delphi. C'est un peu comme un "app store" pour développeurs : il vous permet de découvrir, installer et gérer facilement des milliers de composants et bibliothèques créés par Embarcadero et la communauté Delphi.

Dans cette section, nous allons découvrir comment utiliser GetIt pour enrichir votre environnement de développement et accélérer vos projets.

## Qu'est-ce que GetIt Package Manager ?

### Définition

GetIt Package Manager est un outil intégré à l'IDE Delphi qui vous permet de :

**Découvrir** des packages : parcourir un catalogue de composants et bibliothèques disponibles

**Installer** facilement : installer des packages en quelques clics, sans manipulation manuelle

**Mettre à jour** : garder vos packages à jour avec les dernières versions

**Gérer** : voir quels packages sont installés, les désinstaller si nécessaire

**Obtenir des exemples** : télécharger des exemples de code et des démos

### Qu'est-ce qu'un package ?

Un **package** (ou paquet) est un ensemble de composants, bibliothèques ou outils qui ajoutent des fonctionnalités à Delphi. Il peut contenir :

**Composants visuels** : nouveaux contrôles à placer sur vos fiches (boutons stylés, graphiques, grilles avancées, etc.)

**Bibliothèques de code** : fonctions et classes réutilisables (cryptographie, compression, analyse XML, etc.)

**Frameworks** : ensembles complets pour des tâches spécifiques (REST, IoT, machine learning, etc.)

**Outils** : utilitaires qui s'intègrent à l'IDE (assistants, générateurs de code, etc.)

**Exemples et démos** : projets d'exemple pour apprendre et s'inspirer

### Pourquoi utiliser des packages tiers ?

**Gagner du temps** : ne pas réinventer la roue. Si quelqu'un a déjà créé un composant de qualité, autant l'utiliser.

**Fonctionnalités avancées** : accéder à des fonctionnalités que vous ne pourriez pas (ou difficilement) développer vous-même.

**Qualité professionnelle** : beaucoup de packages sont développés par des professionnels et largement testés.

**Support et communauté** : les packages populaires ont souvent une communauté active et une documentation fournie.

**Mise à jour régulière** : les auteurs maintiennent et améliorent leurs packages.

### Types de packages dans GetIt

**Packages gratuits open source** : créés par la communauté, librement utilisables

**Packages gratuits propriétaires** : fournis gratuitement par des éditeurs, mais le code source n'est pas forcément disponible

**Packages commerciaux** : payants, souvent très professionnels avec support

**Packages d'Embarcadero** : composants et bibliothèques officiels additionnels

**Packages d'exemples** : projets de démonstration et tutoriels

## Accéder à GetIt Package Manager

### Ouvrir GetIt

Il existe plusieurs façons d'ouvrir GetIt Package Manager :

**Via le menu principal** : **Outils > GetIt Package Manager**

**Via l'écran d'accueil** : au démarrage de Delphi, cliquez sur "Obtenir plus" ou "GetIt" dans l'écran d'accueil

**Via la barre d'outils** : certaines versions affichent un bouton GetIt directement

**Raccourci clavier** : pas de raccourci par défaut, mais vous pouvez en créer un dans les options

### Interface de GetIt

Quand vous ouvrez GetIt, une fenêtre s'affiche avec plusieurs sections :

**Panneau de recherche** : en haut, pour rechercher des packages par nom ou mot-clé

**Catégories** : sur la gauche, pour filtrer par type de package

**Liste des packages** : au centre, affichant les packages disponibles avec des miniatures

**Détails du package** : en cliquant sur un package, vous voyez sa description, ses captures d'écran, et les options d'installation

### Première utilisation

Lors de votre première utilisation, GetIt peut avoir besoin de télécharger son catalogue. Cela peut prendre quelques instants selon votre connexion internet.

**Note** : GetIt nécessite une connexion internet pour fonctionner. Il télécharge les informations sur les packages et les packages eux-mêmes depuis les serveurs d'Embarcadero et des éditeurs tiers.

## Découvrir et explorer les packages

### Navigation par catégories

GetIt organise les packages en catégories. Sur le panneau gauche, vous trouverez :

**All** : tous les packages disponibles

**Recently Added** : les packages récemment ajoutés au catalogue

**Trial** : versions d'essai de packages commerciaux

**Commercial** : packages payants

**Samples** : exemples et démos

**VCL Components** : composants pour applications Windows VCL

**FireMonkey Components** : composants pour applications multi-plateformes

**Libraries** : bibliothèques de code sans composants visuels

**Styles** : thèmes visuels pour VCL et FireMonkey

**IDE Plugins** : extensions de l'IDE

**Mobile** : composants spécifiques au développement mobile

Et bien d'autres selon les versions de Delphi.

Cliquez sur une catégorie pour filtrer les packages affichés.

### Rechercher un package

Si vous cherchez quelque chose de précis, utilisez la barre de recherche en haut :

**Tapez un mot-clé** : par exemple "chart" pour trouver des composants de graphiques, "json" pour les bibliothèques JSON, "REST" pour les outils REST, etc.

**Les résultats s'affichent** en temps réel au fur et à mesure que vous tapez

**Cliquez sur un résultat** pour voir ses détails

### Consulter les détails d'un package

En cliquant sur un package, vous accédez à sa page de détails qui affiche :

**Description** : ce que fait le package, ses fonctionnalités principales

**Version** : numéro de version du package

**Éditeur/Auteur** : qui a créé le package

**Licence** : gratuit, commercial, open source, etc.

**Compatibilité** : versions de Delphi supportées, plateformes compatibles

**Captures d'écran** : aperçus visuels si c'est un composant graphique

**Taille** : espace disque nécessaire

**Dépendances** : autres packages requis, le cas échéant

**Liens** : site web, documentation, support

**Avis et notes** : si disponibles, évaluations d'autres développeurs

Prenez le temps de lire ces informations avant d'installer un package !

## Installer un package

### Processus d'installation

L'installation d'un package via GetIt est très simple :

1. **Trouvez le package** que vous voulez installer (via recherche ou navigation)

2. **Cliquez sur le package** pour afficher ses détails

3. **Cliquez sur le bouton "Install"** (ou "Installer")

4. **Acceptez la licence** si une fenêtre de licence apparaît (lisez-la !)

5. **GetIt télécharge le package** : une barre de progression s'affiche

6. **GetIt installe le package** : cela peut prendre de quelques secondes à quelques minutes selon la taille

7. **Confirmez** : un message vous informe que l'installation est terminée

8. **Redémarrez Delphi** si demandé (généralement nécessaire pour les composants visuels et plugins IDE)

### Ce qui se passe pendant l'installation

Derrière les scènes, GetIt :

**Télécharge** les fichiers du package depuis le serveur

**Décompresse** l'archive

**Copie** les fichiers aux bons emplacements

**Compile** le package (les fichiers .bpl)

**Enregistre** les composants dans la Palette d'outils

**Met à jour** les chemins de bibliothèques dans Delphi

Tout cela automatiquement, sans que vous ayez à manipuler des fichiers ou éditer des configurations !

### Installation de packages commerciaux

Pour les packages payants (marqués "Commercial" ou "Trial") :

**Version d'essai** : vous pouvez généralement installer une version d'essai limitée (durée limitée ou fonctionnalités restreintes)

**Achat** : cliquez sur "Buy" ou visitez le site de l'éditeur pour acheter une licence

**Activation** : après achat, vous recevez une clé de licence à saisir dans Delphi ou dans le package lui-même

Chaque éditeur a son propre système d'activation, suivez les instructions fournies.

### Vérifier l'installation

Après l'installation d'un package de composants, vérifiez qu'il est bien disponible :

**Ouvrez la Palette d'outils** : cherchez un nouvel onglet ou de nouveaux composants dans les onglets existants

**Créez un projet de test** : ajoutez le nouveau composant sur une fiche pour vérifier qu'il fonctionne

**Consultez la documentation** : beaucoup de packages incluent une documentation accessible via **Aide > Contenu tiers**

### Si l'installation échoue

Parfois, l'installation peut échouer. Causes possibles :

**Connexion internet** : problème de téléchargement

**Droits insuffisants** : vous devez peut-être exécuter Delphi en administrateur

**Conflit de versions** : le package n'est pas compatible avec votre version de Delphi

**Espace disque** : pas assez d'espace disponible

**Antivirus** : il bloque le téléchargement ou l'installation

Si l'installation échoue, GetIt affiche généralement un message d'erreur. Lisez-le attentivement pour comprendre le problème.

## Gérer les packages installés

### Voir les packages installés

Pour voir quels packages sont installés :

1. **Ouvrez GetIt Package Manager**

2. **Cliquez sur l'onglet "Installed"** (ou "Installés") en haut

Vous voyez la liste de tous les packages installés via GetIt, avec :
- Leur nom
- Leur version
- La date d'installation
- L'état (à jour ou mise à jour disponible)

### Mettre à jour un package

Quand une nouvelle version d'un package installé est disponible, GetIt vous le signale :

**Indicateur de mise à jour** : un badge ou une icône sur le package dans l'onglet "Installed"

**Notification** : parfois, Delphi vous notifie au démarrage qu'une mise à jour est disponible

Pour mettre à jour :

1. **Allez dans l'onglet "Installed"**

2. **Cliquez sur le package** à mettre à jour

3. **Cliquez sur "Update"** (ou "Mettre à jour")

4. **Attendez** le téléchargement et l'installation de la nouvelle version

5. **Redémarrez Delphi** si demandé

**Important** : avant de mettre à jour un package utilisé dans des projets en cours, vérifiez les notes de version (changelog) pour voir s'il y a des changements incompatibles.

### Désinstaller un package

Si vous n'utilisez plus un package ou si vous voulez libérer de l'espace :

1. **Ouvrez GetIt et allez dans "Installed"**

2. **Cliquez sur le package** à désinstaller

3. **Cliquez sur "Uninstall"** (ou "Désinstaller")

4. **Confirmez** la désinstallation

5. **Redémarrez Delphi**

GetIt supprime tous les fichiers du package et le retire de la Palette d'outils.

**Attention** : si vous avez des projets qui utilisent ce package, ils ne compileront plus après la désinstallation. Assurez-vous de ne plus en avoir besoin.

### Packages et versions de Delphi

Certains packages sont spécifiques à une version de Delphi. Si vous mettez à jour Delphi (par exemple de Delphi 12 à Delphi 13), vous devrez probablement réinstaller vos packages pour la nouvelle version.

GetIt gère les versions séparément : les packages installés pour Delphi 12 ne sont pas automatiquement disponibles dans Delphi 13.

## Packages populaires et recommandés

Voici quelques packages populaires disponibles via GetIt, utiles pour différents types de projets. Cette liste n'est pas exhaustive mais donne une idée de ce qui est disponible.

### Composants d'interface

**TMS VCL UI Pack** : collection de composants VCL modernes et stylés (commercial avec version d'essai)

**DevExpress VCL** : suite complète de composants professionnels pour VCL (commercial)

**AlphaControls** : composants VCL avec support de skinning avancé (commercial)

**Project Magenta Styles** : thèmes visuels modernes pour VCL (gratuit)

### Graphiques et visualisation

**TeeChart** : bibliothèque de graphiques puissante (incluse avec Delphi, versions avancées commerciales)

**ZXing Delphi** : génération et lecture de codes-barres et QR codes (open source, gratuit)

### Accès aux données

**FireDAC Extensions** : extensions supplémentaires pour FireDAC

**DevArt Data Access Components** : composants d'accès aux données professionnels (commercial)

### Web et REST

**Indy** : bibliothèque de communication réseau (incluse avec Delphi)

**mORMot** : framework complet pour services REST et ORM (open source, gratuit)

**REST Debugger** : outil pour tester les API REST (gratuit)

### PDF et rapports

**FastReport VCL** : générateur de rapports puissant (version Community gratuite, versions Pro commerciales)

**QuickPDF** : création et manipulation de PDF (commercial)

### Utilitaires et frameworks

**Spring4D** : framework moderne avec conteneurs, IoC, etc. (open source, gratuit)

**DUnitX** : framework de tests unitaires moderne (open source, gratuit)

**GExperts** : extensions de l'IDE très utiles (open source, gratuit)

### Mobile et multi-plateforme

**FMXLinux** : support Linux pour FireMonkey (inclus avec certaines éditions)

**Kastri** : bibliothèque complète pour le développement mobile (open source, gratuit)

### Exemples et learning

**RAD Studio Code Examples** : exemples officiels Embarcadero (gratuit)

**FireMonkey Component Samples** : exemples de composants FireMonkey (gratuit)

**VCL Styles Utils** : utilitaires et exemples pour les styles VCL (open source, gratuit)

**Note** : la disponibilité de ces packages peut varier selon votre édition de Delphi (Community, Professional, Enterprise, Architect) et votre version.

## Bonnes pratiques

### Avant d'installer un package

**Lisez la description** : assurez-vous que le package fait bien ce dont vous avez besoin

**Vérifiez la compatibilité** : avec votre version de Delphi et vos plateformes cibles

**Consultez la documentation** : le site web du package a souvent plus d'informations

**Cherchez des avis** : forums, blogs, vidéos YouTube peuvent donner des retours d'expérience

**Testez sur un projet de démonstration** : avant de l'utiliser dans un vrai projet

**Vérifiez la licence** : surtout pour les projets commerciaux

### Gestion des dépendances

**Documentez les packages utilisés** : dans un fichier README de votre projet, listez les packages nécessaires et leurs versions

**Attention aux dépendances** : certains packages dépendent d'autres packages. GetIt gère généralement cela automatiquement, mais soyez conscient des dépendances

**Version des packages** : notez quelle version vous utilisez. En cas de problème après une mise à jour, vous saurez où chercher

### Tester les packages

**Créez un projet de test** : avant d'intégrer un nouveau package dans votre application principale, testez-le dans un projet séparé

**Vérifiez les performances** : certains packages peuvent avoir un impact sur les performances

**Testez sur toutes vos plateformes** : si vous développez pour Windows et Android, testez le package sur les deux

### Mises à jour prudentes

**Ne mettez pas à jour pendant un développement critique** : attendez une période plus calme

**Lisez le changelog** : les notes de version indiquent les changements, y compris les changements incompatibles

**Testez après mise à jour** : recompilez et testez vos projets après avoir mis à jour un package

**Gardez une copie de l'ancienne version** : en cas de problème, vous pourrez revenir en arrière (le contrôle de version aide ici)

### Licence et légalité

**Respectez les licences** : ne redistribuez pas des packages commerciaux sans autorisation

**Licences open source** : même gratuits, ils ont des conditions (MIT, GPL, BSD, etc.). Lisez-les !

**Attribution** : certaines licences exigent que vous mentionniez l'auteur dans votre application

**Projets commerciaux** : vérifiez que la licence permet l'utilisation commerciale

## Alternatives à GetIt

Bien que GetIt soit l'outil officiel et le plus pratique, il existe d'autres façons d'obtenir des packages pour Delphi :

### Installation manuelle

Beaucoup de packages, surtout open source, peuvent être téléchargés et installés manuellement :

1. Téléchargez les sources depuis GitHub ou le site de l'auteur
2. Décompressez dans un dossier
3. Ouvrez le fichier .dpk (Delphi Package) dans Delphi
4. Compilez et installez

C'est plus complexe que GetIt, mais donne plus de contrôle (vous pouvez modifier le code source si nécessaire).

### Boss (Dependency Manager for Delphi)

Boss est un gestionnaire de dépendances en ligne de commande pour Delphi, inspiré de npm (Node.js) ou Maven (Java).

**Avantages** :
- Gère les dépendances de manière déclarative
- Idéal pour le travail en équipe
- Scriptable, intégrable dans CI/CD

**Inconvénients** :
- Moins intuitif pour les débutants
- Nécessite d'utiliser la ligne de commande

Pour débuter, GetIt est largement suffisant. Boss devient intéressant pour les projets plus avancés.

### GitHub et autres sources

Beaucoup de développeurs partagent leurs composants sur GitHub. Vous pouvez :

- Cloner le dépôt
- Lire le README pour les instructions d'installation
- Installer manuellement

C'est utile pour les packages très récents ou spécialisés qui ne sont pas encore dans GetIt.

## Résolution de problèmes courants

### GetIt ne se charge pas

**Causes possibles** :
- Pas de connexion internet
- Firewall ou proxy bloquant
- Serveurs d'Embarcadero temporairement indisponibles

**Solutions** :
- Vérifiez votre connexion internet
- Configurez les paramètres de proxy dans **Outils > Options > Connexion**
- Essayez plus tard

### Package ne s'installe pas

**Erreur "Incompatible with this version"** :
- Le package n'est pas compatible avec votre version de Delphi
- Cherchez une version compatible ou attendez une mise à jour

**Erreur "Not enough disk space"** :
- Libérez de l'espace disque
- Vérifiez que votre disque n'est pas plein

**Erreur "Compilation failed"** :
- Le package a un problème de code
- Vérifiez les forums ou contactez l'auteur
- Essayez une version différente

### Package installé mais composants invisibles

**Causes** :
- Delphi n'a pas été redémarré
- Package installé pour la mauvaise plateforme
- Problème de chemin de bibliothèque

**Solutions** :
- Redémarrez Delphi complètement
- Vérifiez dans **Composants > Installer des packages** que le package est bien listé
- Réinstallez le package

### Conflits entre packages

Parfois, deux packages peuvent entrer en conflit (même nom de composant, même unité, etc.).

**Solutions** :
- Désinstallez un des deux packages
- Contactez les auteurs pour signaler le conflit
- Utilisez l'installation manuelle pour plus de contrôle

### Ralentissement de l'IDE

Si vous installez beaucoup de packages, l'IDE peut devenir plus lent au démarrage et à l'utilisation.

**Solutions** :
- Désinstallez les packages que vous n'utilisez pas
- Utilisez des packages "runtime only" quand possible (pas de composants dans l'IDE, juste les bibliothèques)
- Envisagez une machine plus puissante (SSD, plus de RAM)

## GetIt et le travail en équipe

### Partager les packages entre développeurs

Quand vous travaillez en équipe, tous les développeurs doivent avoir les mêmes packages installés.

**Bonne pratique** :
1. Créez un fichier `PACKAGES.md` ou `DEPENDENCIES.md` dans votre projet
2. Listez tous les packages nécessaires avec leurs versions
3. Indiquez comment les installer (via GetIt ou installation manuelle)

Exemple de fichier PACKAGES.md :
```markdown
# Packages requis pour ce projet

## Via GetIt Package Manager
- FastReport VCL Community Edition (version 6.9)
- Spring4D (version 2.0)
- DUnitX (dernière version)

## Installation manuelle
- MyCustomLibrary : télécharger depuis https://github.com/...
```

### GetIt et le contrôle de version

**Ne committez PAS** les packages installés dans Git. Ils sont trop gros et spécifiques à chaque machine.

**Committez** la liste des dépendances et les instructions d'installation.

Chaque développeur installe les packages localement sur sa machine via GetIt.

## Sécurité et packages

### Packages de confiance

**Packages officiels Embarcadero** : sûrs, vérifiés

**Packages d'éditeurs connus** : généralement sûrs (DevExpress, TMS, DevArt, etc.)

**Packages open source populaires** : code source visible, communauté active = généralement sûrs

**Packages inconnus** : soyez prudent, vérifiez la réputation de l'auteur

### Précautions

**Lisez les commentaires** : si disponibles, les avis d'autres utilisateurs

**Vérifiez le site web** : un package sérieux a généralement un site web professionnel

**Scannez avec un antivirus** : après installation, vous pouvez scanner les fichiers

**Testez d'abord** : dans un projet test, pas directement dans votre projet principal

**Sandbox** : pour les packages douteux, testez dans une machine virtuelle

En général, les packages disponibles via GetIt sont vérifiés par Embarcadero, mais la prudence reste de mise.

## Nouveautés de GetIt dans Delphi 13

Delphi 13 Florence apporte des améliorations à GetIt :

**Interface améliorée** : plus moderne, plus rapide

**Gestion de versions** : meilleure gestion des versions de packages

**Recherche améliorée** : résultats plus pertinents, filtres avancés

**Notifications** : vous êtes notifié des mises à jour importantes

**Installation plus rapide** : optimisations du processus de téléchargement et installation

**Compatibilité** : meilleure détection de compatibilité avec votre configuration

Les fonctionnalités exactes peuvent varier selon votre édition (Community, Professional, Enterprise, Architect).

## Conseils pour débutants

### Commencez simple

Ne vous précipitez pas pour installer des dizaines de packages. Commencez par maîtriser les composants standards de Delphi.

Installez des packages uniquement quand vous avez un besoin réel.

### Packages recommandés pour débuter

**DUnitX** : pour apprendre les tests unitaires (gratuit, léger)

**GExperts** : améliore l'IDE avec plein de petits outils utiles (gratuit)

**Exemples RAD Studio** : apprenez des exemples officiels (gratuit)

**FastReport Community** : si vous avez besoin de rapports (gratuit)

Ces packages sont gratuits, stables, et largement utilisés.

### Lisez la documentation

Après avoir installé un package, prenez le temps de :
- Lire sa documentation
- Essayer les exemples fournis
- Expérimenter dans un projet de test

Ne vous contentez pas de l'installer et d'espérer que ça fonctionne magiquement !

### Posez des questions

Si vous avez des difficultés avec un package :
- Consultez sa documentation officielle
- Cherchez sur les forums Delphi
- Visitez Stack Overflow
- Contactez l'auteur si c'est un package open source

La communauté Delphi est généralement très aidante.

## Conclusion

GetIt Package Manager est un outil puissant qui ouvre les portes d'un vaste écosystème de composants et bibliothèques pour Delphi. Il transforme ce qui était autrefois une tâche fastidieuse (installer des composants tiers) en un processus simple et rapide.

Points essentiels à retenir :

- **GetIt** est le "app store" de Delphi pour composants et bibliothèques
- **Installation simple** : quelques clics suffisent pour installer un package
- **Gestion facilitée** : mises à jour et désinstallations faciles
- **Vaste catalogue** : gratuits et commerciaux, pour tous les besoins
- **Testez avant d'utiliser** : dans un projet de test
- **Documentez les dépendances** : pour vous et votre équipe
- **Restez prudent** : vérifiez les licences et la réputation

GetIt est l'un des grands atouts de Delphi moderne. Utilisé intelligemment, il peut considérablement accélérer votre développement et enrichir vos applications.

N'hésitez pas à explorer le catalogue, à tester différents packages, et à découvrir tout ce que la communauté Delphi a créé pour vous faciliter la vie !

Dans la section suivante, nous découvrirons la gestion des versions de packages avec GetIt, pour maintenir la cohérence et la compatibilité de vos projets.

⏭️ [Gestion des versions de packages avec GetIt](/02-decouverte-de-lide-delphi/10-gestion-des-versions-de-packages-avec-getit.md)
