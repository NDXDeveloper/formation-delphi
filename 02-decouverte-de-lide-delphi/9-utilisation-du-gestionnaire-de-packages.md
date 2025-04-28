# 2.9 Utilisation du Gestionnaire de Packages (GetIt Package Manager)

L'un des grands avantages de Delphi est sa capacité à être étendu avec des composants et bibliothèques supplémentaires. Le Gestionnaire de Packages GetIt (GetIt Package Manager) est l'outil intégré qui vous permet d'installer facilement ces extensions directement depuis l'IDE. Dans cette section, nous allons découvrir comment utiliser GetIt pour enrichir votre environnement de développement Delphi.

## Qu'est-ce que GetIt Package Manager ?

GetIt est un gestionnaire de packages intégré à Delphi qui vous permet de :
- Découvrir des composants et bibliothèques supplémentaires
- Installer facilement ces packages sans quitter l'IDE
- Gérer les mises à jour des packages installés
- Désinstaller les packages dont vous n'avez plus besoin

Ces packages peuvent être des composants visuels, des bibliothèques non visuelles, des styles, des exemples de code, ou même des outils de productivité.

## Accéder à GetIt Package Manager

Pour ouvrir le Gestionnaire de Packages GetIt :

1. Dans le menu principal, cliquez sur **Outils > GetIt Package Manager**
2. Ou utilisez le raccourci **Ctrl+Alt+G** (selon votre configuration)

![Accès à GetIt Package Manager](https://placeholder.com/delphi-getit-access)

> **Note :** La première utilisation de GetIt peut nécessiter une connexion avec votre compte Embarcadero Developer Network (EDN). Si vous n'en avez pas, vous serez invité à en créer un gratuitement.

## Interface de GetIt

L'interface de GetIt se compose de plusieurs zones principales :

### Barre de navigation

En haut de la fenêtre, vous trouverez les onglets principaux :
- **Découvrir** : Parcourir tous les packages disponibles
- **Installés** : Voir et gérer les packages déjà installés
- **Mises à jour** : Vérifier et installer les mises à jour disponibles
- **Paramètres** : Configurer les options de GetIt

### Zone de recherche

Une barre de recherche vous permet de trouver rapidement des packages par nom ou mot-clé.

### Filtres de catégories

Sur le côté gauche, vous pouvez filtrer les packages par catégorie :
- Composants
- Bibliothèques
- Styles
- Exemples
- Outils
- Et bien d'autres...

### Liste des packages

La zone principale affiche les packages disponibles ou installés selon l'onglet sélectionné. Pour chaque package, vous verrez :
- Nom et icône
- Description courte
- Version
- Éditeur
- Bouton d'installation/désinstallation

![Interface de GetIt](https://placeholder.com/delphi-getit-interface)

## Installer un package avec GetIt

L'installation d'un package avec GetIt est très simple :

1. **Recherchez le package** souhaité
   - Utilisez la barre de recherche ou parcourez les catégories
   - Lisez les descriptions pour trouver ce qui correspond à vos besoins

2. **Sélectionnez le package** pour voir plus de détails
   - Vous verrez une description complète
   - Les versions disponibles
   - Les dépendances éventuelles
   - Les plateformes supportées

3. **Cliquez sur le bouton "Installer"**
   - GetIt téléchargera automatiquement le package
   - Il résoudra et installera les dépendances si nécessaire
   - Une barre de progression vous indiquera l'avancement

4. **Redémarrez Delphi si demandé**
   - Certains packages nécessitent un redémarrage de l'IDE pour être activés
   - GetIt vous le signalera et vous proposera de redémarrer automatiquement

> **Astuce pour débutants :** Commencez par explorer les exemples disponibles dans GetIt. Ils vous aideront à comprendre comment utiliser diverses fonctionnalités de Delphi.

## Packages populaires pour débutants

Voici quelques packages recommandés pour les débutants :

### Composants visuels

- **DevExpress VCL Subscription** : Suite complète de composants visuels avancés
- **TMS Component Pack** : Collection de composants pour enrichir vos interfaces
- **Konopka Signature VCL Controls** : Composants visuels de haute qualité

### Bibliothèques utiles

- **REST Debugger** : Outil pour tester les API REST
- **DUnitX** : Framework de test unitaire pour Delphi
- **SQLite3 Components** : Accès facile aux bases de données SQLite

### Styles et apparence

- **VCL Styles** : Thèmes supplémentaires pour vos applications VCL
- **FMX Styles** : Thèmes pour les applications FireMonkey

### Exemples de code

- **Delphi Sample Projects** : Collection d'exemples officiels
- **Bookmarks** : Exemple de gestion de favoris
- **ClientDataSet Demo** : Exemple d'utilisation des ClientDataSets

![Packages populaires](https://placeholder.com/delphi-popular-packages)

## Gérer les packages installés

Pour gérer vos packages installés :

1. **Afficher les packages installés**
   - Allez dans l'onglet "Installés"
   - Vous verrez tous vos packages avec leur version

2. **Mettre à jour un package**
   - Allez dans l'onglet "Mises à jour"
   - Les packages qui peuvent être mis à jour seront listés
   - Cliquez sur "Mettre à jour" pour installer la nouvelle version

3. **Désinstaller un package**
   - Sélectionnez le package dans l'onglet "Installés"
   - Cliquez sur "Désinstaller"
   - Confirmez la désinstallation

> **Note :** La désinstallation d'un package peut affecter d'autres packages qui en dépendent. GetIt vous avertira de ces dépendances avant de procéder.

## Configurer GetIt

Vous pouvez configurer GetIt selon vos préférences :

1. Allez dans l'onglet **Paramètres**

2. Configurez les options suivantes :
   - **Proxy** : Si vous êtes derrière un proxy d'entreprise
   - **Téléchargement** : Dossier de téléchargement temporaire
   - **Notifications** : Préférences pour les alertes de mises à jour
   - **Dépôts** : Sources supplémentaires de packages

## GetIt et les licences

Les packages disponibles dans GetIt peuvent avoir différents types de licences :

1. **Packages gratuits** : Utilisables sans restriction
2. **Packages d'évaluation** : Utilisables pour une période limitée
3. **Packages commerciaux** : Nécessitent un achat ou une subscription

GetIt indique clairement le type de licence pour chaque package. Pour les packages commerciaux, vous devrez généralement activer une licence après l'installation.

> **Conseil :** Vérifiez toujours les conditions de licence avant d'utiliser un package dans un projet commercial.

## Utilisation des packages installés

Une fois un package installé, comment l'utiliser dans vos projets ?

### Composants visuels

Les composants visuels apparaîtront automatiquement dans la Palette d'outils, généralement dans un nouvel onglet portant le nom du package.

Pour les utiliser :
1. Sélectionnez l'onglet correspondant dans la Palette d'outils
2. Cliquez sur le composant souhaité
3. Cliquez sur votre formulaire pour l'ajouter
4. Configurez ses propriétés via l'Inspecteur d'objets

![Composants dans la Palette](https://placeholder.com/delphi-palette-components)

### Bibliothèques non visuelles

Pour les bibliothèques non visuelles :
1. Ajoutez l'unité correspondante à votre clause `uses`
2. Utilisez les classes, fonctions ou procédures fournies

Par exemple, si vous avez installé une bibliothèque JSON :

```pascal
uses
  System.SysUtils, System.Classes,
  SuperJSON; // Unité de la bibliothèque installée

procedure TForm1.ButtonParseClick(Sender: TObject);
var
  JSONObj: TSuperObject;
begin
  JSONObj := SO(Memo1.Text); // Utilisation de la bibliothèque
  // Suite du code...
end;
```

### Styles et thèmes

Pour appliquer un style installé à votre application :
1. Allez dans **Projet > Options**
2. Sélectionnez **Application > Apparence**
3. Choisissez le style dans la liste déroulante
4. Cliquez sur OK pour appliquer

## Résolution des problèmes courants

### GetIt ne se connecte pas

Si GetIt ne parvient pas à se connecter :
1. Vérifiez votre connexion Internet
2. Configurez les paramètres proxy si nécessaire
3. Assurez-vous que votre pare-feu n'en bloque pas l'accès

### L'installation échoue

Si l'installation d'un package échoue :
1. Vérifiez l'espace disque disponible
2. Fermez tous les projets ouverts et réessayez
3. Redémarrez Delphi et réessayez
4. Consultez les journaux d'installation (dans l'onglet Paramètres)

### Un composant installé n'apparaît pas

Si un composant installé n'apparaît pas dans la Palette d'outils :
1. Assurez-vous que Delphi a été redémarré après l'installation
2. Vérifiez que le package est bien installé (onglet "Installés")
3. Essayez de restaurer la Palette d'outils (clic droit > Réinitialiser)

## Exercice pratique

Pour vous familiariser avec GetIt, essayez cet exercice simple :

1. Ouvrez GetIt Package Manager
2. Recherchez et installez "REST Debugger"
3. Redémarrez Delphi si demandé
4. Explorez l'outil REST Debugger dans le menu Outils
5. Créez un nouveau projet et testez une API REST publique simple (comme https://jsonplaceholder.typicode.com/posts)

## Conseils pour bien utiliser GetIt

1. **Installez uniquement ce dont vous avez besoin** : Trop de packages peuvent ralentir l'IDE
2. **Consultez les évaluations et commentaires** avant d'installer un package
3. **Sauvegardez vos projets** avant d'installer de nouveaux packages
4. **Vérifiez régulièrement les mises à jour** pour profiter des dernières améliorations
5. **Explorez la documentation** fournie avec les packages installés

## Alternatives à GetIt

Bien que GetIt soit très pratique, il existe d'autres façons d'étendre Delphi :

1. **Installation manuelle de packages** (.bpl, .dpk)
2. **Gestionnaires de packages tiers** comme Delphinus ou Boss
3. **Code open-source** sur GitHub et autres plateformes

> **Pour les débutants :** Commencez par GetIt qui offre l'expérience la plus simple et intégrée. Vous pourrez explorer les autres options au fur et à mesure que vous gagnerez en expérience.

## Conclusion

Le Gestionnaire de Packages GetIt est un outil puissant qui vous permet d'étendre facilement les capacités de Delphi. En explorant et en installant judicieusement des packages, vous pouvez gagner un temps précieux en réutilisant des composants existants plutôt que de tout développer vous-même.

Dans le prochain chapitre, nous plongerons dans les fondamentaux du langage Object Pascal, la base de tout développement avec Delphi.
