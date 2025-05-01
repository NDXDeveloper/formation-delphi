# 1.5 Installation et configuration

🔝 Retour à la [Sommaire](/SOMMAIRE.md)

Cette section vous guidera pas à pas dans l'installation et la configuration initiale de votre environnement Delphi. Nous nous concentrerons sur l'installation de Delphi Community Edition, qui est gratuite pour un usage personnel et pour les petites entreprises.

## Prérequis système

Avant de commencer l'installation, assurez-vous que votre ordinateur répond aux exigences minimales :

### Configuration recommandée pour Delphi 12 Athens
- **Système d'exploitation** : Windows 10 ou Windows 11
- **Processeur** : Intel Core i5 ou supérieur (ou équivalent AMD)
- **Mémoire RAM** : 8 Go minimum, 16 Go recommandé
- **Espace disque** : Au moins 20 Go d'espace libre
- **Résolution d'écran** : 1280x800 minimum, Full HD recommandé
- **Droits administrateur** : Nécessaires pour l'installation

> **Note :** Pour Delphi 11 Alexandria, les exigences sont légèrement inférieures, mais il est recommandé de disposer d'une machine relativement récente pour une expérience fluide.

## Téléchargement de Delphi Community Edition

1. **Accédez au site officiel** d'Embarcadero : [www.embarcadero.com](https://www.embarcadero.com)

2. **Naviguez vers la section Téléchargements** ou recherchez directement "Delphi Community Edition"

3. **Créez un compte Embarcadero** si vous n'en avez pas déjà un :
   - Cliquez sur "Créer un compte"
   - Remplissez le formulaire avec vos informations
   - Vérifiez votre adresse email en cliquant sur le lien reçu

4. **Téléchargez l'installateur** :
   - Connectez-vous avec votre compte
   - Sélectionnez "Delphi Community Edition"
   - Cliquez sur le bouton de téléchargement
   - Sauvegardez le fichier d'installation (généralement un .exe de plusieurs Go)

> **Conseil :** Assurez-vous d'avoir une connexion internet stable pour le téléchargement, car le fichier est volumineux.

## Processus d'installation

### Phase 1 : Lancement de l'installation

1. **Exécutez le fichier téléchargé** avec les droits d'administrateur :
   - Faites un clic droit sur le fichier
   - Sélectionnez "Exécuter en tant qu'administrateur"

2. **Acceptez les autorisations UAC** si Windows vous le demande

3. **Patientez pendant l'extraction** des fichiers d'installation

### Phase 2 : Configuration de l'installation

1. **Sélectionnez la langue** d'installation et cliquez sur "Suivant"

2. **Acceptez le contrat de licence** après l'avoir lu :
   - Cochez la case "J'accepte les termes du contrat de licence"
   - Cliquez sur "Suivant"

3. **Choisissez le type d'installation** :
   - Pour les débutants, l'option "Installation typique" est recommandée
   - Cliquez sur "Suivant"

4. **Sélectionnez l'emplacement d'installation** :
   - Par défaut : `C:\Program Files (x86)\Embarcadero\Studio\xx.0` (où xx est le numéro de version)
   - Vous pouvez modifier cet emplacement si nécessaire
   - Assurez-vous d'avoir au moins 20 Go d'espace libre sur le disque choisi
   - Cliquez sur "Suivant"

5. **Configurez les options supplémentaires** :
   - Sélectionnez les composants à installer (pour débuter, les options par défaut sont appropriées)
   - Cliquez sur "Suivant"

6. **Vérifiez le récapitulatif** et cliquez sur "Installer"

### Phase 3 : Processus d'installation

1. **L'installation des fichiers commence**
   - Ce processus peut prendre 15 à 30 minutes selon votre système
   - Une barre de progression vous indique l'avancement

2. **Installation des composants supplémentaires**
   - Différents assistants peuvent s'ouvrir pour installer des composants spécifiques
   - Suivez les instructions à l'écran pour chacun d'eux

3. **Finalisation**
   - Une fois tous les composants installés, cliquez sur "Terminer"
   - Il vous sera peut-être demandé de redémarrer votre ordinateur

## Premier lancement et activation

1. **Lancez Delphi** depuis le menu Démarrer ou le raccourci sur le bureau

2. **Connexion à votre compte Embarcadero** :
   - Entrez vos identifiants de connexion (email et mot de passe)
   - Cliquez sur "Se connecter"

3. **Activation de la licence Community Edition** :
   - Sélectionnez "Delphi Community Edition"
   - Suivez les instructions pour activer votre licence gratuite
   - Confirmez que vous respectez les conditions d'utilisation pour la Community Edition

4. **Configuration initiale de l'IDE** :
   - Delphi vous propose de choisir votre configuration préférée
   - Pour les débutants, choisissez "Configuration standard"
   - Vous pouvez également sélectionner le thème visuel (clair ou sombre)

5. **Installation des packages supplémentaires** :
   - Delphi peut suggérer d'installer des packages additionnels
   - Pour commencer, vous pouvez accepter ceux qui sont proposés par défaut

## Configuration post-installation

### Configuration de l'environnement de développement

1. **Ajustez les options de l'IDE** :
   - Accédez au menu `Outils > Options`
   - Explorez les différentes catégories d'options
   - Pour débuter, les paramètres par défaut sont généralement appropriés

2. **Configurez les chemins de bibliothèques** (si nécessaire) :
   - Dans le menu `Outils > Options`, sélectionnez `Environnement > Delphi Options > Library`
   - Les chemins par défaut sont généralement corrects après une installation standard

### Configuration du GetIt Package Manager

Le GetIt Package Manager est un outil intégré qui vous permet d'installer facilement des composants supplémentaires :

1. **Accédez au GetIt Package Manager** :
   - Menu `Outils > GetIt Package Manager`
   - Ou cliquez sur l'icône correspondante dans la barre d'outils

2. **Parcourez les packages disponibles** :
   - Explorez les différentes catégories
   - Pour débuter, recherchez des packages marqués comme "populaires" ou "essentiels"

3. **Installez des packages utiles pour débutants** :
   - Composants de base de données
   - Utilitaires d'interface utilisateur
   - Composants de connexion réseau simples

> **Conseil :** N'installez pas trop de composants au début. Commencez avec les essentiels et ajoutez-en au fur et à mesure de vos besoins.

## Résolution des problèmes courants d'installation

### Problème : Échec d'installation avec message d'erreur

**Solution :**
1. Consultez le fichier journal d'installation (généralement dans le dossier Temp)
2. Vérifiez que vous avez les droits administrateur
3. Désactivez temporairement votre antivirus pendant l'installation
4. Assurez-vous d'avoir suffisamment d'espace disque

### Problème : Échec d'activation de la licence

**Solution :**
1. Vérifiez votre connexion internet
2. Assurez-vous que vos identifiants Embarcadero sont corrects
3. Contactez le support Embarcadero si le problème persiste

### Problème : IDE lent au démarrage

**Solution :**
1. Augmentez la mémoire allouée à l'IDE dans les options
2. Réduisez les packages chargés au démarrage
3. Désactivez les fonctionnalités non essentielles

## Vérification de l'installation

Pour vous assurer que tout fonctionne correctement, créez un simple projet "Hello World" :

1. Cliquez sur `Fichier > Nouveau > Application VCL`
2. Placez un bouton sur le formulaire
3. Double-cliquez sur le bouton pour créer un gestionnaire d'événement
4. Ajoutez la ligne : `ShowMessage('Hello World!');`
5. Appuyez sur F9 pour compiler et exécuter

Si votre application s'exécute et affiche "Hello World!" lorsque vous cliquez sur le bouton, félicitations ! Votre installation de Delphi est fonctionnelle.

## Maintenir Delphi à jour

Delphi publie régulièrement des mises à jour pour corriger des bugs et ajouter de nouvelles fonctionnalités :

1. **Vérifiez les mises à jour** :
   - Menu `Aide > Mises à jour Delphi`
   - Ou configurez les notifications automatiques

2. **Installez les mises à jour** :
   - Suivez les instructions à l'écran
   - Sauvegardez vos projets avant une mise à jour importante

> **Note :** Pour la Community Edition, vous devrez renouveler votre licence gratuitement chaque année. Delphi vous préviendra lorsque ce sera nécessaire.

---

Vous avez maintenant Delphi installé et configuré sur votre système ! Dans la prochaine section, nous explorerons l'environnement de développement et vous familiariserez avec son interface utilisateur.

⏭️ [Premier aperçu de l'environnement](/01-introduction-a-delphi/06-premier-apercu-de-lenvironnement.md)
