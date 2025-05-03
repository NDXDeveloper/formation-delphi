# 12.6 Débogage à distance

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction au débogage à distance

Le débogage à distance est une technique puissante qui vous permet d'exécuter et déboguer votre application sur une machine différente de celle où vous développez. Cette approche est particulièrement utile dans plusieurs scénarios :

- Déboguer des applications sur différentes configurations matérielles
- Résoudre des problèmes qui n'apparaissent que sur certains systèmes
- Tester des applications mobiles sans avoir à les exécuter dans un émulateur
- Déboguer des applications déployées dans leur environnement de production
- Résoudre des problèmes difficilement reproductibles sur votre machine de développement

Delphi offre un support intégré pour le débogage à distance, et cette section vous guidera pas à pas dans sa configuration et son utilisation.

## Principes de base du débogage à distance

Le débogage à distance avec Delphi repose sur deux composants principaux :

1. **L'IDE Delphi** sur votre machine de développement (machine hôte)
2. **Le serveur de débogage à distance** sur la machine cible où votre application sera exécutée

L'IDE se connecte au serveur de débogage à distance, qui contrôle l'exécution de votre application sur la machine cible. Cela vous permet d'utiliser toutes les fonctionnalités habituelles de débogage (points d'arrêt, exécution pas à pas, inspection des variables, etc.) même si l'application s'exécute sur une autre machine.

## Configuration du débogage à distance sur Windows

### Étape 1 : Préparer le serveur de débogage à distance

Le serveur de débogage à distance est inclus dans l'installation de Delphi. Pour le configurer sur la machine cible :

1. Copiez le fichier `rtl280.bpl` (le numéro peut varier selon votre version de Delphi) depuis le dossier `C:\Program Files (x86)\Embarcadero\Studio\XX.0\bin` de votre machine de développement vers un dossier sur la machine cible.

2. Copiez également le fichier `rmtdbg280.dll` depuis le même emplacement.

3. Copiez l'exécutable `rmtdbg.exe` depuis le dossier `C:\Program Files (x86)\Embarcadero\Studio\XX.0\bin`.

> 💡 **Remarque** : Les numéros dans les noms de fichiers (`280`, etc.) correspondent à la version de Delphi. Vérifiez les noms exacts dans votre installation.

### Étape 2 : Démarrer le serveur de débogage sur la machine cible

1. Sur la machine cible, ouvrez une invite de commandes avec des privilèges administrateur.

2. Naviguez jusqu'au dossier où vous avez copié les fichiers.

3. Exécutez la commande suivante :
   ```
   rmtdbg.exe -listen
   ```

4. Vous devriez voir un message indiquant que le serveur de débogage à distance est actif et en attente de connexions.

   ![Serveur de débogage à distance](https://via.placeholder.com/500x100)

5. Notez l'adresse IP de la machine cible, vous en aurez besoin pour configurer l'IDE.

### Étape 3 : Configurer l'IDE pour le débogage à distance

1. Dans l'IDE Delphi sur votre machine de développement, ouvrez votre projet.

2. Allez dans **Run > Parameters** (Exécuter > Paramètres) ou appuyez sur `Shift+Ctrl+F2`.

3. Dans la boîte de dialogue, sélectionnez l'onglet **Remote** (Distant).

4. Cochez la case **Remote Debug** (Débogage à distance).

5. Entrez l'adresse IP de la machine cible dans le champ **Remote Host** (Hôte distant).

6. Spécifiez le port dans le champ **Port** (par défaut : 64211).

7. Sélectionnez le mode de débogage à distance :
   - **Run** (Exécuter) : L'application est exécutée sur la machine cible
   - **Load** (Charger) : L'application est déjà en cours d'exécution sur la machine cible

8. Si nécessaire, configurez les chemins d'accès :
   - **Remote Path** (Chemin distant) : Emplacement où l'exécutable sera copié sur la machine cible
   - **Remote Source Path** (Chemin source distant) : Si le code source est différent entre les machines

   ![Configuration du débogage à distance](https://via.placeholder.com/500x300)

9. Cliquez sur **OK** pour enregistrer les paramètres.

### Étape 4 : Lancer une session de débogage à distance

1. Placez les points d'arrêt souhaités dans votre code.

2. Appuyez sur `F9` ou utilisez **Run > Run** (Exécuter > Exécuter) pour démarrer le débogage.

3. L'IDE se connectera au serveur de débogage à distance, copiera l'application sur la machine cible (si configuré en mode **Run**), puis démarrera l'exécution.

4. Lorsqu'un point d'arrêt est atteint, l'exécution s'arrête et l'IDE affiche le code source correspondant.

5. Vous pouvez maintenant utiliser toutes les fonctionnalités de débogage habituelles, comme si l'application s'exécutait localement.

## Débogage à distance sur les plateformes mobiles

Delphi permet également de déboguer à distance des applications sur iOS et Android, ce qui est essentiel pour le développement mobile.

### Débogage à distance sur Android

> 💡 **Nécessite Delphi 10.2 ou supérieur pour les fonctionnalités complètes**

#### Prérequis

1. Le SDK Android doit être correctement configuré dans Delphi
2. Un appareil Android physique ou un émulateur
3. Activation du mode développeur et du débogage USB sur l'appareil Android

#### Configuration

1. Connectez votre appareil Android via USB à votre ordinateur de développement.

2. Assurez-vous que les pilotes ADB (Android Debug Bridge) sont installés.

3. Dans l'IDE Delphi, allez dans **Tools > Options > Deployment** (Outils > Options > Déploiement).

4. Sélectionnez la plateforme Android et vérifiez que votre appareil est détecté.

5. Dans le projet, définissez la cible de déploiement sur Android :
   - Cliquez sur **Project > Deployment** (Projet > Déploiement)
   - Sélectionnez votre appareil dans la liste

6. Pour déboguer, il suffit d'appuyer sur `F9` comme pour une application normale.

### Débogage à distance sur iOS

> 💡 **Nécessite Delphi 10.2 ou supérieur et un Mac avec Xcode installé**

#### Prérequis

1. Un Mac exécutant Xcode et l'utilitaire PAServer (Platform Assistant Server)
2. Un appareil iOS physique ou un simulateur
3. Connexion réseau entre votre PC de développement et le Mac

#### Configuration du PAServer

1. Sur le Mac, installez PAServer depuis votre installation Delphi :
   - Copiez le dossier PAServer depuis `C:\Program Files (x86)\Embarcadero\Studio\XX.0\PAServer` de votre PC Windows vers votre Mac
   - Ou téléchargez-le depuis le site Embarcadero

2. Lancez PAServer sur le Mac :
   - Ouvrez le Terminal
   - Naviguez jusqu'au dossier PAServer
   - Exécutez `./PAServer -publichost [adresse_IP_du_Mac]`

3. Définissez un mot de passe lorsque demandé.

#### Configuration dans l'IDE Delphi

1. Dans l'IDE Delphi sur Windows, allez dans **Tools > Options > Connection Profile Manager** (Outils > Options > Gestionnaire de profil de connexion).

2. Ajoutez un nouveau profil pour votre Mac :
   - Nom du profil : Choisissez un nom descriptif
   - Adresse : Entrez l'adresse IP du Mac
   - Port : Laissez la valeur par défaut (64211)
   - Mot de passe : Entrez le mot de passe défini sur PAServer

3. Dans votre projet, définissez la cible sur iOS :
   - Dans **Project Manager** (Gestionnaire de projet), cliquez avec le bouton droit sur le nœud cible
   - Sélectionnez **Target Platforms** (Plateformes cibles)
   - Cochez iOS et définissez-le comme cible active

4. Pour déboguer, appuyez sur `F9` comme d'habitude.

## Techniques avancées de débogage à distance

### 1. Débogage à distance des services Windows

Les services Windows s'exécutent dans un contexte différent et peuvent être difficiles à déboguer. Le débogage à distance peut faciliter ce processus :

1. Dans le code de votre service, ajoutez une pause au démarrage :

```pascal
procedure TMonService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  // Ajouter un délai pour avoir le temps de se connecter
  Sleep(30000); // 30 secondes de pause

  // Code normal du service...
end;
```

2. Configurez le débogage à distance comme décrit précédemment.

3. Démarrez le service sur la machine cible.

4. Pendant la pause de 30 secondes, connectez-vous avec l'IDE en mode **Load** (Charger).

5. Placez des points d'arrêt et démarrez le débogage.

### 2. Utilisation de la fenêtre Évaluer/Modifier à distance

La fenêtre Évaluer/Modifier fonctionne aussi en débogage à distance :

1. Pendant une session de débogage à distance, lorsqu'un point d'arrêt est atteint, appuyez sur `Ctrl+F7`.

2. Entrez une expression à évaluer ou une variable à modifier.

3. Les résultats reflètent l'état actuel sur la machine distante.

### 3. Débogage avec des configurations réseau complexes

Dans certains environnements d'entreprise, les pare-feu ou autres configurations réseau peuvent compliquer le débogage à distance. Voici quelques techniques pour résoudre ces problèmes :

#### Spécifier un port différent

Si le port par défaut (64211) est bloqué :

1. Sur la machine cible, démarrez le serveur de débogage avec un port spécifique :
   ```
   rmtdbg.exe -listen:8000
   ```

2. Dans l'IDE, spécifiez le même port dans les paramètres de débogage à distance.

#### Utilisation de tunnels SSH

Si vous devez déboguer à travers des réseaux sécurisés :

1. Établissez un tunnel SSH entre votre machine de développement et la machine cible :
   ```
   ssh -L 64211:localhost:64211 utilisateur@machine_cible
   ```

2. Dans l'IDE, configurez le débogage à distance pour se connecter à `localhost`.

## Bonnes pratiques pour le débogage à distance

### 1. Sécurité

Le serveur de débogage à distance peut représenter un risque de sécurité s'il est mal configuré :

- Ne l'exécutez jamais sur des serveurs de production sans protection adéquate
- Utilisez des pare-feu pour restreindre l'accès aux ports de débogage
- Arrêtez le serveur de débogage à distance lorsqu'il n'est plus nécessaire
- Envisagez d'utiliser un VPN ou un tunnel SSH pour les connexions à travers Internet

### 2. Performance

Le débogage à distance peut être plus lent que le débogage local :

- Minimisez les données transférées (évitez d'inspecter de grandes structures de données)
- Utilisez des points d'arrêt ciblés plutôt que de nombreux pas à pas
- Préparez votre session de débogage à l'avance (identifiez les zones problématiques)

### 3. Organisation des fichiers

Pour un débogage efficace, les fichiers source doivent être identiques sur les deux machines :

- Utilisez un système de contrôle de version pour synchroniser les fichiers
- Configurez correctement les chemins d'accès aux sources dans l'IDE
- Vérifiez que les versions des bibliothèques sont identiques

## Dépannage du débogage à distance

### Problèmes de connexion

Si l'IDE ne peut pas se connecter au serveur de débogage à distance :

1. **Vérifiez le réseau** : Assurez-vous que les deux machines peuvent communiquer (utilisez `ping`).

2. **Vérifiez le pare-feu** : Assurez-vous que le port utilisé n'est pas bloqué.

3. **Vérifiez le serveur** : Assurez-vous que le serveur de débogage est en cours d'exécution.

4. **Vérifiez les adresses** : Assurez-vous que l'adresse IP et le port sont corrects.

### L'application se lance mais ne s'arrête pas aux points d'arrêt

1. **Vérifiez les symboles de débogage** : Assurez-vous que votre application est compilée avec les informations de débogage.

2. **Vérifiez les chemins d'accès aux sources** : L'IDE doit pouvoir localiser les fichiers source.

3. **Recompilez** : Essayez de recompiler l'application pour vous assurer que les points d'arrêt correspondent au code compilé.

## Améliorations du débogage à distance dans Delphi 12 Athens

> 💡 **Nécessite Delphi 12 ou supérieur**

Delphi 12 Athens apporte plusieurs améliorations au débogage à distance :

### 1. Débogage HTTPS sécurisé

```pascal
// Configuration du serveur de débogage avec SSL/TLS
rmtdbg.exe -listen -secure:cert.pem
```

### 2. Débogage à distance amélioré pour les applications multi-plateformes

Delphi 12 offre une expérience plus fluide pour le débogage des applications FireMonkey sur toutes les plateformes, avec une meilleure intégration pour Linux, iOS et Android.

### 3. Inspection améliorée des objets complexes

Le débogueur à distance dans Delphi 12 permet une meilleure visualisation des structures de données complexes, y compris les génériques et les collections.

## Conclusion

Le débogage à distance est un outil essentiel dans l'arsenal de tout développeur Delphi, particulièrement lorsqu'il s'agit de résoudre des problèmes spécifiques à certains environnements ou de travailler avec des applications multi-plateformes.

Bien que sa configuration initiale puisse sembler complexe, les avantages qu'il offre en termes de capacité à identifier et résoudre les problèmes dans leur environnement réel sont inestimables. Avec de la pratique, le débogage à distance deviendra une partie naturelle de votre flux de travail de développement.

Dans la prochaine section, nous explorerons les tests d'intégration, qui complètent les tests unitaires en vérifiant comment les différentes parties de votre application interagissent entre elles.

⏭️ [Tests d'intégration](/12-debogage-et-tests/07-tests-dintegration.md)
