🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.5 Installation et configuration

## Introduction

L'installation de Delphi est une étape cruciale pour commencer votre voyage dans le développement d'applications. Bien que le processus soit globalement simple, il est important de suivre certaines étapes pour garantir une installation correcte et optimale. Ce guide vous accompagne pas à pas dans l'installation et la configuration initiale de Delphi.

## Prérequis système

Avant d'installer Delphi, assurez-vous que votre ordinateur répond aux exigences minimales.

### Configuration matérielle minimale

**Processeur :**
- Intel Core i3 ou équivalent AMD
- 64 bits obligatoire (Delphi ne fonctionne plus sur les systèmes 32 bits)

**Mémoire RAM :**
- 4 Go minimum
- 8 Go recommandés
- 16 Go ou plus pour un confort optimal

**Espace disque :**
- 10 Go minimum pour l'installation de base
- 20 à 60 Go recommandés selon les plateformes ciblées
- SSD fortement recommandé pour de meilleures performances

**Écran :**
- Résolution minimum : 1366 x 768
- Recommandé : 1920 x 1080 ou supérieur
- Delphi supporte les écrans haute résolution (4K)

### Système d'exploitation

**Windows :**
- Windows 10 (version 1903 ou ultérieure)
- Windows 11 (recommandé)
- Windows Server 2016 ou ultérieur

**Important :** Delphi lui-même fonctionne uniquement sur Windows. Cependant, une fois installé, vous pourrez créer des applications pour Windows, macOS, iOS, Android et Linux.

### Logiciels prérequis

Delphi installera automatiquement certains composants nécessaires, mais vous devez avoir :

**Microsoft .NET Framework :**
- Version 4.7.2 ou ultérieure
- Généralement déjà installé sur Windows 10/11

**Visual C++ Redistributable :**
- Installé automatiquement par Delphi si nécessaire

**Droits d'administration :**
- Vous devez avoir des droits administrateur sur votre ordinateur pour installer Delphi

## Création d'un compte Embarcadero

Avant de télécharger Delphi, vous devez créer un compte gratuit sur le site d'Embarcadero.

### Étapes de création de compte

1. **Rendez-vous sur le site officiel**
   - Ouvrez votre navigateur et allez sur : www.embarcadero.com

2. **Cliquez sur "Sign In" ou "Register"**
   - Généralement situé en haut à droite du site

3. **Remplissez le formulaire d'inscription**
   - Nom et prénom
   - Adresse email (utilisez une adresse valide)
   - Créez un mot de passe sécurisé
   - Pays de résidence
   - Acceptez les conditions d'utilisation

4. **Confirmez votre email**
   - Consultez votre boîte email
   - Cliquez sur le lien de confirmation envoyé par Embarcadero

5. **Complétez votre profil**
   - Certaines informations supplémentaires peuvent être demandées
   - Indiquez votre situation (étudiant, développeur individuel, entreprise)

**Conseil :** Conservez précieusement vos identifiants. Vous en aurez besoin pour télécharger Delphi et activer votre licence.

## Téléchargement de Delphi

Une fois votre compte créé et confirmé, vous pouvez télécharger Delphi.

### Pour la Community Edition (gratuite)

1. **Connectez-vous à votre compte Embarcadero**

2. **Accédez à la page de téléchargement**
   - Cherchez "Delphi Community Edition" dans le menu ou la section produits
   - Ou allez directement sur la page dédiée à la Community Edition

3. **Acceptez les conditions**
   - Lisez et acceptez les termes de la licence Community
   - Confirmez que vous remplissez les conditions d'éligibilité

4. **Choisissez la version**
   - Sélectionnez "Delphi 13 Florence" (dernière version)
   - Choisissez la langue de l'interface (anglais recommandé, français disponible selon les versions)

5. **Lancez le téléchargement**
   - Le fichier d'installation fait généralement entre 2 et 6 Go
   - Le téléchargement peut prendre de quelques minutes à plusieurs heures selon votre connexion

### Pour les éditions payantes (Professional, Enterprise, Architect)

Le processus est similaire, mais vous devrez :
- Soit acheter une licence directement sur le site
- Soit demander une version d'essai de 30 jours
- Soit entrer un code de licence si vous en avez déjà un

### Téléchargement par le License Manager

Embarcadero propose également un gestionnaire de téléchargement appelé **License Manager** qui :
- Facilite le téléchargement des gros fichiers
- Permet de reprendre un téléchargement interrompu
- Gère vos licences et activations

**Recommandé** pour les connexions instables ou lentes.

## Installation de Delphi

Une fois le téléchargement terminé, vous pouvez procéder à l'installation.

### Préparation avant l'installation

**Fermez tous les programmes**
- Notamment les antivirus qui pourraient ralentir l'installation
- Fermez les applications qui utilisent beaucoup de mémoire

**Désactivez temporairement l'antivirus**
- Certains antivirus peuvent interférer avec l'installation
- Réactivez-le immédiatement après l'installation

**Assurez-vous d'avoir du temps**
- L'installation complète peut prendre de 30 minutes à 2 heures
- Ne l'interrompez pas une fois commencée

### Processus d'installation pas à pas

**Étape 1 : Lancement de l'installateur**
- Double-cliquez sur le fichier téléchargé (généralement nommé RADStudio_13_xxx.exe)
- Si Windows vous demande l'autorisation, cliquez sur "Oui"

**Étape 2 : Écran d'accueil**
- L'installateur de RAD Studio s'ouvre
- Cliquez sur "Next" pour continuer

**Étape 3 : Acceptation de la licence**
- Lisez (ou parcourez) les termes de la licence
- Cochez "I accept the agreement"
- Cliquez sur "Next"

**Étape 4 : Sélection du dossier d'installation**
- Le dossier par défaut est généralement : `C:\Program Files (x86)\Embarcadero\Studio\24.0`
- **Recommandation :** Gardez le dossier par défaut sauf raison spécifique
- Assurez-vous d'avoir suffisamment d'espace disque
- Cliquez sur "Next"

**Étape 5 : Choix des composants**
Vous devrez choisir quels composants installer :

**Pour les débutants, installez :**
- ✓ Delphi (obligatoire)
- ✓ RAD Studio IDE (obligatoire)
- ✓ Documentation et exemples
- ✓ Plateformes : Windows 32-bit et Windows 64-bit

**Vous pouvez décocher (pour économiser de l'espace) :**
- C++Builder (si vous ne faites que du Delphi)
- Plateformes mobiles (iOS, Android) si vous utilisez la Community Edition
- Plateformes macOS et Linux si vous n'en avez pas besoin immédiatement

**Important :** Vous pourrez toujours ajouter des composants plus tard via le programme d'installation.

**Étape 6 : Sélection des fonctionnalités additionnelles**
- GetIt Package Manager (recommandé : laissez coché)
- Support des styles visuels (recommandé)
- Exemples de code (très utile pour apprendre)

**Étape 7 : Configuration des raccourcis**
- Créer un raccourci sur le bureau (recommandé)
- Créer un raccourci dans le menu Démarrer (recommandé)

**Étape 8 : Résumé et confirmation**
- Vérifiez les options sélectionnées
- Cliquez sur "Install" pour commencer l'installation

**Étape 9 : Installation en cours**
- L'installateur copie les fichiers (cela peut prendre du temps)
- Une barre de progression indique l'avancement
- **Ne pas interrompre le processus**

**Étape 10 : Installation des prérequis**
- L'installateur peut installer automatiquement des composants nécessaires
- Comme Visual C++ Redistributable
- Laissez-le faire

**Étape 11 : Fin de l'installation**
- Une fois terminé, vous verrez un message de succès
- Cochez "Launch RAD Studio" pour lancer Delphi immédiatement
- Cliquez sur "Finish"

## Activation de la licence

Au premier lancement, Delphi vous demandera d'activer votre licence.

### Pour la Community Edition

1. **Écran d'activation**
   - Delphi affiche un écran vous demandant d'enregistrer le produit

2. **Connexion à votre compte**
   - Entrez vos identifiants Embarcadero (email et mot de passe)
   - Cliquez sur "Connect"

3. **Sélection de la licence**
   - Choisissez "Delphi Community Edition"
   - Confirmez que vous remplissez les conditions d'éligibilité

4. **Activation**
   - Cliquez sur "Activate"
   - L'activation se fait en ligne et prend quelques secondes

5. **Confirmation**
   - Vous recevrez un message confirmant l'activation réussie
   - Delphi est maintenant prêt à être utilisé

### Pour les éditions payantes

Le processus est similaire, mais vous devrez :
- Entrer votre clé de licence (serial number)
- Ou vous connecter avec un compte ayant une licence valide
- Ou utiliser la période d'essai de 30 jours

### Activation hors ligne

Si votre ordinateur n'a pas accès à Internet :
- Vous pouvez effectuer une activation hors ligne
- Contactez le support Embarcadero pour obtenir un fichier de licence
- Importez ce fichier dans Delphi

## Configuration initiale de l'IDE

Une fois Delphi installé et activé, quelques configurations initiales amélioreront votre expérience.

### Choix de la langue

Au premier lancement :
- Delphi peut vous demander de choisir la langue de l'interface
- Anglais est recommandé (documentation et ressources plus abondantes)
- Français est disponible mais certains termes techniques restent en anglais

### Configuration du thème visuel

Delphi propose plusieurs thèmes :
- **Light Theme :** Thème clair, traditionnel
- **Dark Theme :** Thème sombre, moins fatigant pour les yeux
- Vous pouvez changer cela dans : Tools > Options > IDE > Themes

### Configuration de l'éditeur de code

Personnalisez l'éditeur selon vos préférences :
- Tools > Options > Editor Options
- Taille de la police (recommandé : 10-12 points)
- Coloration syntaxique
- Indentation (recommandé : 2 espaces)
- Affichage des numéros de ligne (utile)

### Choix de la disposition (Layout)

Delphi permet de personnaliser la disposition des fenêtres :
- View > Desktops vous permet de choisir des dispositions prédéfinies
- "Classic Undocked" : fenêtres séparées (comme Delphi 7)
- "Default Layout" : tout dans une fenêtre principale (recommandé pour débuter)

### Configuration des chemins

Delphi doit connaître certains chemins système :
- Tools > Options > Environment Options > Delphi Options > Library
- Ces chemins sont normalement configurés automatiquement
- **Pour les débutants :** Ne modifiez rien ici pour l'instant

## Vérification de l'installation

Il est important de vérifier que tout fonctionne correctement.

### Test avec un projet simple

1. **Créez un nouveau projet**
   - File > New > VCL Forms Application - Delphi

2. **Projet créé automatiquement**
   - Delphi crée une fenêtre vide avec un formulaire

3. **Ajoutez un bouton**
   - Dans la palette d'outils (Tool Palette), trouvez "Button"
   - Cliquez sur le bouton puis sur le formulaire

4. **Compilez le projet**
   - Appuyez sur F9 ou cliquez sur Run (bouton vert "play")
   - Si tout va bien, votre application se compile et s'exécute

5. **Succès !**
   - Si une fenêtre avec votre bouton apparaît, l'installation est réussie

### Vérification des plateformes

Pour vérifier quelles plateformes sont installées :
- Cliquez droit sur votre projet dans le Project Manager
- Sélectionnez "Add Platform"
- Vous verrez la liste des plateformes disponibles

## Installation de composants additionnels

Delphi peut être étendu avec des composants tiers.

### GetIt Package Manager

**GetIt** est le gestionnaire de packages intégré :
- Accessible via Tools > GetIt Package Manager
- Permet d'installer facilement des bibliothèques et composants
- Gratuits et payants disponibles

**Composants populaires pour débuter :**
- TMS VCL UI Pack (composants visuels améliorés)
- Exemples de code supplémentaires
- Modèles de projets

### Installation manuelle de composants

Certains composants nécessitent une installation manuelle :
1. Téléchargez le composant
2. Décompressez-le dans un dossier
3. Ouvrez le package (.dpk) dans Delphi
4. Compilez et installez le package
5. Les nouveaux composants apparaissent dans la palette

**Pour les débutants :** Utilisez d'abord les composants standards avant d'installer des composants tiers.

## Problèmes courants et solutions

### L'installation échoue

**Solution :**
- Vérifiez l'espace disque disponible
- Désactivez temporairement l'antivirus
- Exécutez l'installateur en tant qu'administrateur
- Consultez les logs d'installation dans le dossier temporaire

### Delphi ne démarre pas

**Solution :**
- Vérifiez que tous les prérequis sont installés (.NET Framework)
- Essayez de lancer en mode administrateur
- Supprimez les fichiers de configuration (ils seront recréés)
- Réinstallez si nécessaire

### La licence ne s'active pas

**Solution :**
- Vérifiez votre connexion Internet
- Vérifiez vos identifiants Embarcadero
- Assurez-vous d'utiliser le bon type de licence
- Contactez le support Embarcadero si le problème persiste

### Delphi est très lent

**Solution :**
- Fermez les projets non utilisés
- Désactivez les fonctionnalités non essentielles
- Augmentez la RAM de votre ordinateur
- Installez Delphi sur un SSD

## Sauvegarde de votre configuration

Une fois Delphi configuré à votre goût :
- Exportez vos paramètres : Tools > Options > Environment Options > Export Settings
- Sauvegardez ce fichier
- Vous pourrez le réimporter après une réinstallation

## Mises à jour

Delphi reçoit régulièrement des mises à jour :
- **Correctifs de bugs :** Installez-les dès leur disponibilité
- **Mises à jour mineures :** Généralement sûres et recommandées
- **Nouvelles versions majeures :** Testez avant de migrer vos projets importants

Pour vérifier les mises à jour :
- Help > Check for Updates
- Ou via GetIt Package Manager

## Ressources post-installation

Après l'installation, explorez ces ressources :

**Documentation intégrée :**
- Help > RAD Studio Documentation (F1)
- Extrêmement complète et bien organisée

**Exemples de code :**
- Installés avec Delphi
- Généralement dans : `C:\Users\Public\Documents\Embarcadero\Studio\24.0\Samples`

**Tutoriels en ligne :**
- Site officiel Embarcadero
- DocWiki d'Embarcadero
- YouTube et blogs spécialisés

## Conseils pour bien démarrer

**Prenez le temps d'explorer l'IDE**
- Ne vous précipitez pas dans le code
- Familiarisez-vous avec les menus et fenêtres
- Regardez quelques vidéos de présentation

**Commencez simple**
- Créez des projets très simples au début
- Un bouton qui affiche un message
- Une calculatrice basique
- Progressez graduellement

**Sauvegardez régulièrement**
- Delphi sauvegarde automatiquement, mais prenez l'habitude de sauvegarder manuellement
- Utilisez un système de contrôle de version (Git) dès que possible

**Ne modifiez pas tout de suite**
- Gardez les paramètres par défaut au début
- Modifiez progressivement selon vos besoins
- Notez les changements que vous faites

## En résumé

L'installation de Delphi est un processus assez simple qui prend du temps mais qui, une fois terminé correctement, vous offre un environnement de développement complet et puissant. Prenez le temps de bien configurer votre environnement dès le début, cela vous fera gagner du temps par la suite.

Si vous rencontrez des problèmes, n'hésitez pas à consulter la documentation officielle ou à demander de l'aide sur les forums de la communauté Delphi. La communauté est généralement très réactive et prête à aider les débutants.

Maintenant que Delphi est installé et configuré, vous êtes prêt à découvrir l'environnement de développement et à créer votre première application !

⏭️ [Premier aperçu de l'environnement](/01-introduction-a-delphi/06-premier-apercu-de-lenvironnement.md)
