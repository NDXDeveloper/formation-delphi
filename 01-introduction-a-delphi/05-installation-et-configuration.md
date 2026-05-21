🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 1.5 Installation et configuration

## Introduction

L'installation de Delphi est une étape cruciale pour commencer votre voyage dans le développement d'applications. Bien que le processus soit globalement simple, il est important de suivre certaines étapes pour garantir une installation correcte et optimale. Ce guide vous accompagne pas à pas dans l'installation et la configuration initiale de Delphi.

## Prérequis système

Avant d'installer Delphi, assurez-vous que votre ordinateur répond aux exigences minimales.

### Configuration matérielle minimale (officielle Embarcadero pour Delphi 13)

**Processeur :**
- 1,8 GHz dual-core minimum
- Quad-core ou supérieur **recommandé**
- 64 bits obligatoire (les processeurs single-core ne sont pas supportés)

**Mémoire RAM :**
- 8 Go minimum (l'IDE Delphi 13 étant 64 bits, il consomme davantage de mémoire que les versions antérieures)
- 16 Go **recommandés**
- 32 Go ou plus pour les projets très volumineux ou les développeurs travaillant sur plusieurs plateformes simultanément

**Espace disque :**
- 6 Go minimum pour une installation très réduite
- 60 Go pour une installation complète avec toutes les plateformes (Windows 32/64, macOS, iOS, Android, Linux, exemples, documentation)
- **SSD fortement recommandé** : la compilation et le démarrage de l'IDE seront beaucoup plus rapides

**Carte graphique :**
- Compatible **DirectX 11**

**Écran :**
- Résolution minimum : 1440 x 900
- Recommandé : 1920 x 1080 ou supérieur
- Delphi 13 supporte parfaitement les écrans haute résolution (HiDPI, 4K) grâce à son IDE entièrement vectorisé

### Système d'exploitation

**Windows :**
- Windows 10 (version 1903 ou ultérieure, en 64 bits)
- Windows 11 (recommandé)
- Windows Server 2016, 2019 ou 2022

**Important :** Delphi lui-même fonctionne uniquement sur Windows. Cependant, une fois installé, vous pourrez créer des applications pour Windows, macOS, iOS, Android et Linux à partir de cette même installation Windows. Pour cibler macOS ou iOS et déployer/tester réellement, vous aurez besoin d'un Mac accessible sur le réseau (utilisé via le PAServer d'Embarcadero — voir plus bas).

### Configuration côté Mac (pour développement iOS / macOS)

Si vous comptez développer pour macOS ou iOS, vous aurez besoin d'un Mac en complément de votre PC Windows. C'est une exigence d'Apple qui s'applique à toutes les technologies (Delphi, Xamarin, React Native, Flutter…) : seul un Mac peut compiler/signer pour iOS et macOS.

**Sur le Mac, vous devrez installer :**

1. **Xcode** (gratuit sur le Mac App Store), qui fournit :
   - Le SDK iOS et macOS
   - Les outils de signature de code (codesign)
   - Le simulateur iOS
   - Les outils en ligne de commande

2. **PAServer (Platform Assistant Server)** d'Embarcadero, qui fait le pont entre votre Delphi sous Windows et le Mac :
   - PAServer est fourni avec Delphi (cherchez le dossier `PAServer` dans l'installation)
   - Copiez l'installateur sur votre Mac et exécutez-le
   - Lancez ensuite PAServer sur le Mac : il écoute sur un port réseau (par défaut 64211)
   - Dans Delphi sur Windows, configurez un **Connection Profile** pointant vers l'adresse IP du Mac

3. **Un compte développeur Apple** (gratuit pour les tests sur appareil personnel, payant à 99 $/an pour publier sur l'App Store)

**Configuration réseau :** Le Mac et le PC Windows doivent être sur le même réseau local (ou accessibles via VPN). Une fois configuré, vous pouvez compiler et lancer une application iOS/macOS depuis Windows comme si tout se faisait localement — Delphi envoie le code source au Mac via PAServer, qui compile et déploie.

> 💡 **Alternative cloud :** Si vous n'avez pas de Mac, certains services proposent des Mac en location à distance (MacInCloud, MacStadium, etc.). Vous pouvez aussi utiliser une **machine virtuelle macOS** sur un PC Intel ou sur un Mac Mini d'occasion comme machine de build dédiée.

### Logiciels prérequis

Delphi installera automatiquement certains composants nécessaires, mais vous devez avoir :

**Microsoft .NET Framework :**
- Version 4.5 ou ultérieure (Embarcadero peut vous proposer une version plus récente lors de l'installation)
- Généralement déjà installé sur Windows 10/11

**Visual C++ Redistributable :**
- Installé automatiquement par Delphi si nécessaire

**Windows SDK :**
- L'installateur vérifie le SDK Windows présent sur la machine et propose un lien vers la dernière version si besoin

**Android SDK / NDK (uniquement si vous ciblez Android) :**
- L'installateur propose d'installer automatiquement Android SDK 25.2.5 et le NDK r27b pour Delphi 13

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

### Web Installer vs ISO complet

Embarcadero propose **deux modes de téléchargement** pour l'installateur :

**Web Installer (recommandé) :**
- Un petit installateur (~100 Mo) qui télécharge les composants à la demande
- Avantage : vous pouvez ne sélectionner que les plateformes dont vous avez besoin
- Inconvénient : nécessite une connexion stable pendant toute l'installation

**ISO complet (Offline Installer) :**
- Un gros fichier ISO (5-10 Go selon la version)
- Avantage : pas besoin de connexion Internet pendant l'installation, idéal pour un poste hors-ligne ou plusieurs machines à installer
- Inconvénient : téléchargement initial long

Le **License Manager** d'Embarcadero (séparé) sert quant à lui à gérer vos licences et activations après installation. Vous le trouverez dans `bin\LicenseManager.exe` une fois Delphi installé.

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
- Pour Delphi 13 Florence, l'IDE est désormais 64 bits natif. Le dossier par défaut est : `C:\Program Files\Embarcadero\Studio\24.0`
- (Pour rappel : sur les versions antérieures à Delphi 13, le chemin était `C:\Program Files (x86)\Embarcadero\Studio\X.0` car l'IDE était une application 32 bits)
- **Recommandation :** Gardez le dossier par défaut sauf raison spécifique
- Assurez-vous d'avoir suffisamment d'espace disque
- Cliquez sur "Next"

**Étape 5 : Choix des composants**
Vous devrez choisir quels composants installer :

**Pour les débutants, installez :**
- ✓ Delphi (obligatoire)  
- ✓ RAD Studio IDE (obligatoire)  
- ✓ Documentation et exemples  
- ✓ Plateformes : Windows 32-bit et Windows 64-bit (les deux sont utiles pour cibler les vieux clients comme les nouveaux)  
- ✓ Windows on Arm (Arm64EC) si vous avez Delphi 13.1 et un PC Arm ou si vous prévoyez de cibler ce type de matériel

**Vous pouvez décocher (pour économiser de l'espace) :**
- C++Builder (si vous ne faites que du Delphi et n'avez pas de licence pour C++Builder)
- Plateformes mobiles (iOS, Android) si vous n'en avez pas besoin immédiatement (la Community Edition les supporte aussi, contrairement à une idée reçue)
- Plateformes macOS et Linux si vous n'en avez pas besoin immédiatement

**Important :** Vous pourrez toujours ajouter ou retirer des composants plus tard via la fonction "Modifier l'installation" (Modify) du programme d'installation.

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
- **Mountain Mist / Charcoal Dark Slate :** Variantes intermédiaires

Pour changer de thème : **Tools > Options > User Interface > IDE > Theme**. Le thème est appliqué à toute l'interface (menus, palette, éditeur).

### Configuration de l'éditeur de code

Personnalisez l'éditeur selon vos préférences via **Tools > Options > Editor > Display** (et sous-sections) :
- **Taille de la police** (recommandé : 11-12 points sur un écran Full HD, 14-16 sur 4K)
- **Coloration syntaxique** : couleurs personnalisables par type de jeton (Editor > Color)
- **Indentation** : par défaut 2 espaces (convention Delphi historique)
- **Numéros de ligne** : à activer ! Très utile pour les erreurs de compilation et le débogage
- **Block Indent / Tab Settings** : à laisser sur "Smart Tab" pour un comportement intuitif

### Choix du schéma de raccourcis clavier

Delphi propose plusieurs **schémas de raccourcis** (Key Mapping) pour s'adapter aux développeurs venant d'autres IDE :

- **Default** : Le schéma Delphi moderne (recommandé pour les nouveaux développeurs)
- **Classic** : Les raccourcis historiques de Turbo Pascal / Delphi 7 (pour les vétérans)
- **Visual Studio** : Pour les développeurs habitués à Visual Studio
- **Visual Basic** : Compatible avec les anciens habitués de VB

Pour changer : **Tools > Options > User Interface > Editor > Key Mappings**.

> 💡 **Astuce :** Si vous venez de Visual Studio ou que vous travaillez en parallèle avec, le schéma "Visual Studio" peut vous éviter beaucoup de confusion (notamment pour F5, F10, F11 qui n'ont pas le même comportement par défaut entre Delphi et VS).

### IDE Insight (recherche universelle)

À retenir absolument dès le démarrage : **Ctrl+.** (Control + point) ouvre l'**IDE Insight**, une barre de recherche universelle qui permet de :
- Trouver n'importe quelle commande de menu
- Ouvrir n'importe quel fichier du projet
- Lancer n'importe quelle fonction de l'IDE
- Configurer n'importe quelle option

C'est l'équivalent du "Quick Open" de VS Code ou du "Ctrl+Shift+P" de plein d'IDE modernes. **Indispensable** quand on ne se souvient plus dans quel menu se trouve une fonction.

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

## Structure des fichiers installés

Une fois Delphi 13 Florence installé, vous trouverez les fichiers répartis comme suit :

**Programme principal (binaires de l'IDE et compilateurs) :**
```
C:\Program Files\Embarcadero\Studio\24.0\
├── bin\                  ← Exécutables : IDE (bds.exe), compilateurs (dcc32.exe, dcc64.exe…), outils
├── source\               ← Code source de la RTL, VCL, FMX (très utile pour comprendre Delphi)
├── lib\                  ← Bibliothèques pré-compilées (*.dcu, *.bpl)
├── PAServer\             ← Installateur du Platform Assistant pour Mac/Linux
└── ...
```

**Données utilisateur (configurations, registre des composants) :**
```
C:\Users\<VotreNom>\AppData\Roaming\Embarcadero\BDS\24.0\
├── Bds.dproj            ← Configuration globale
├── Library.bdsproj      ← Liste des packages installés
└── ...
```

**Documents partagés (exemples, projets de démo) :**
```
C:\Users\Public\Documents\Embarcadero\Studio\24.0\
├── Samples\             ← Exemples de code Delphi
├── CatalogRepository\   ← Cache GetIt
└── Styles\              ← Fichiers de styles VCL/FMX (.vsf)
```

**Bon à savoir :**
- Le dossier `source\` contient le **code source intégral** de la RTL, VCL et FireMonkey. C'est une ressource d'apprentissage exceptionnelle : à chaque fois que vous voulez comprendre comment fonctionne un composant, vous pouvez aller lire son code source.
- Si vous avez un problème d'IDE qui plante au démarrage, **renommer le dossier `AppData\Roaming\Embarcadero\BDS\24.0\`** force Delphi à recréer une configuration neuve — souvent suffisant pour régler les problèmes de profil corrompu.

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

### L'antivirus signale RAD Studio comme suspect

**Solution :**
- C'est un faux positif courant avec les antivirus heuristiques (Windows Defender, Avast, McAfee…)
- Ajoutez le dossier `C:\Program Files\Embarcadero\` aux **exclusions de votre antivirus**
- Ajoutez aussi `C:\Users\<VotreNom>\AppData\Local\Embarcadero\` et `AppData\Roaming\Embarcadero\`
- En l'absence d'exclusions, l'antivirus peut considérablement ralentir la compilation (scan de chaque DCU généré)

### "Cannot find package" au chargement d'un projet

**Solution :**
- Un package requis par le projet n'est pas installé dans votre IDE
- Vérifiez dans GetIt si le package manque
- Ou installez-le manuellement depuis la source du composant
- Ce problème est fréquent quand on ouvre un projet écrit par quelqu'un d'autre

### Code Insight (autocomplétion) ne fonctionne pas

**Solution :**
- Tools > Options > Editor > Language Server : vérifier que **DelphiLSP** est activé
- Redémarrer Delphi
- Si le projet est très gros, l'indexation initiale peut prendre quelques minutes
- Vérifier dans le Messages Pane si DelphiLSP affiche des erreurs

## Désinstallation propre

Si vous devez désinstaller Delphi (pour réinstaller une version propre, pour libérer de l'espace, etc.) :

1. **Désinstallation normale :**
   - Panneau de configuration > Programmes et fonctionnalités
   - Sélectionner "Embarcadero RAD Studio 13" > Désinstaller
   - Suivre l'assistant

2. **Nettoyage des fichiers résiduels (recommandé pour une désinstallation complète) :**
   - Supprimer `C:\Program Files\Embarcadero\Studio\24.0\` (si encore présent)
   - Supprimer `C:\Users\<VotreNom>\AppData\Roaming\Embarcadero\BDS\24.0\`
   - Supprimer `C:\Users\<VotreNom>\AppData\Local\Embarcadero\BDS\24.0\`
   - Supprimer les exemples partagés : `C:\Users\Public\Documents\Embarcadero\Studio\24.0\` (attention : sauvegardez vos modifications si vous y avez touché)

3. **Outil officiel Embarcadero :**
   - Embarcadero distribue un outil "**ISO de désinstallation**" disponible sur leur site, qui nettoie tout proprement
   - Recommandé après plusieurs installations/désinstallations successives

## Sauvegarde de votre configuration

Une fois Delphi configuré à votre goût :
- Exportez vos paramètres : Tools > Options > Environment Options > Export Settings
- Sauvegardez ce fichier
- Vous pourrez le réimporter après une réinstallation

## Mises à jour

Delphi reçoit régulièrement des mises à jour, classées en plusieurs catégories :

- **Patches (correctifs de bugs)** — petits correctifs ciblés, publiés quelques fois par an. Installez-les dès leur disponibilité.
- **Updates intermédiaires** (ex : Delphi 13.1 par rapport à Delphi 13.0) — apportent des nouveautés mais respectent globalement la compatibilité avec la version majeure courante.
- **Nouvelles versions majeures** (ex : Delphi 13 vs Delphi 12) — publication annuelle environ, peuvent contenir des changements de compatibilité. **Testez avant de migrer vos projets importants.**

Pour vérifier les mises à jour :
- **Tools > Manage Platforms** : pour ajouter/retirer des plateformes ou mettre à jour des SDK mobiles
- **Help > Check for Updates** : pour les patches IDE
- **License Manager** (`bin\LicenseManager.exe`) : informations détaillées sur votre licence et les versions disponibles
- **GetIt Package Manager** : pour les composants et bibliothèques tiers
- Une fois inscrit, vous recevez aussi des notifications par email d'Embarcadero pour les nouvelles versions

## Ressources post-installation

Après l'installation, explorez ces ressources :

**Documentation intégrée :**
- Help > RAD Studio Documentation (F1 contextuel sur n'importe quel mot-clé)
- Extrêmement complète et bien organisée
- Disponible en ligne : [docwiki.embarcadero.com](https://docwiki.embarcadero.com/)

**Exemples de code :**
- Installés avec Delphi
- Pour Delphi 13 Florence : `C:\Users\Public\Documents\Embarcadero\Studio\24.0\Samples`
- Également accessibles via le menu Tools de l'IDE
- Source GitHub officielle d'Embarcadero avec des exemples additionnels : [github.com/Embarcadero](https://github.com/Embarcadero)

**Tutoriels en ligne :**
- Site officiel Embarcadero
- DocWiki d'Embarcadero
- YouTube (chaînes Embarcadero, Marco Cantù, Jim McKeeth)
- Blogs spécialisés (Marco Cantù, Andrea Magni, Olaf Monien, etc.)
- Site web companion IA (nouveau dans Delphi 13)

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
