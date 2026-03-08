🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 20.1 Documentation officielle

## Introduction

La documentation officielle est votre meilleure alliée dans votre parcours d'apprentissage de Delphi. Que vous soyez débutant ou développeur expérimenté, c'est une ressource incontournable qui vous accompagnera au quotidien. Dans cette section, nous allons découvrir ensemble comment accéder et utiliser efficacement toutes les ressources documentaires mises à disposition par Embarcadero.

## Pourquoi la documentation officielle est-elle importante ?

Avant de plonger dans les détails, comprenons pourquoi la documentation officielle devrait être votre premier réflexe :

- **Fiabilité** : L'information provient directement de l'éditeur du logiciel, garantissant son exactitude
- **Mise à jour régulière** : Elle suit les évolutions de Delphi et intègre les nouveautés de chaque version
- **Exhaustivité** : Tous les composants, toutes les classes et toutes les fonctionnalités y sont documentés
- **Exemples de code** : De nombreux exemples pratiques illustrent l'utilisation des différentes fonctionnalités
- **Gratuité** : Accessible à tous les utilisateurs de Delphi, y compris avec la Community Edition

## Le DocWiki : votre encyclopédie Delphi

### Qu'est-ce que le DocWiki ?

Le DocWiki est la documentation en ligne principale de Delphi. C'est un site web complet qui contient toute la documentation technique, des tutoriels, des guides et des références API.

**Adresse principale** : https://docwiki.embarcadero.com

### Comment naviguer dans le DocWiki

Le DocWiki est organisé en plusieurs grandes catégories :

#### RAD Studio (l'environnement global)
Cette section couvre l'IDE Delphi dans son ensemble, incluant :
- Les guides d'installation
- Les nouveautés de chaque version
- Les paramètres et configurations
- Les outils intégrés

#### Delphi (le langage et les bibliothèques)
Ici vous trouverez :
- La référence du langage Object Pascal
- La documentation des bibliothèques VCL et FireMonkey
- Les guides de développement par plateforme
- Les tutoriels pas à pas

#### Database (accès aux données)
Tout ce qui concerne FireDAC et l'accès aux bases de données :
- Configuration des connexions
- Utilisation des composants de données
- Optimisation des requêtes
- Exemples d'intégration

### Utiliser la recherche efficacement

Le DocWiki dispose d'un moteur de recherche puissant. Voici quelques astuces pour les débutants :

**Rechercher un composant** : Tapez simplement son nom, par exemple "TButton" ou "TStringList"

**Rechercher une fonctionnalité** : Utilisez des mots-clés en anglais, comme "file handling", "database connection", ou "json parsing"

**Filtrer par version** : La documentation indique souvent depuis quelle version une fonctionnalité est disponible

## L'aide intégrée dans l'IDE

### Accès direct depuis votre code

L'une des fonctionnalités les plus pratiques de Delphi est l'aide contextuelle directement dans l'IDE.

**Comment l'utiliser** :
1. Placez votre curseur sur un mot-clé, un nom de classe ou une fonction
2. Appuyez sur **F1**
3. La documentation s'ouvre automatiquement à la page correspondante

Cette méthode est particulièrement utile quand vous découvrez un nouveau composant ou que vous avez besoin d'un rappel rapide sur les paramètres d'une fonction.

### La fenêtre d'aide

L'aide peut s'ouvrir dans une fenêtre intégrée ou dans votre navigateur web, selon vos préférences. Pour modifier ce comportement :
- Menu **Tools** → **Options** → **Environment Options** → **Help**

## La référence API complète

### Comprendre la structure de la documentation API

Chaque classe, composant ou fonction dans Delphi est documenté de manière standardisée. Voici comment lire une page de documentation :

#### En-tête
- **Nom complet** : Le nom de la classe avec son namespace (exemple : `Vcl.StdCtrls.TButton`)
- **Unité** : L'unité à ajouter dans votre clause `uses` pour utiliser cet élément
- **Héritage** : La hiérarchie des classes parentes

#### Sections principales

**Description** : Une explication générale de ce que fait la classe ou la fonction

**Propriétés** : Liste toutes les propriétés disponibles avec leur type et leur description

**Méthodes** : Toutes les méthodes (fonctions et procédures) de la classe

**Événements** : Les événements que vous pouvez gérer (OnClick, OnChange, etc.)

**Exemples** : Des extraits de code montrant l'utilisation concrète

### Exemple de lecture d'une page de documentation

Prenons l'exemple de TStringList, une classe très utilisée :

```
Unit: System.Classes  
Hierarchy: TObject → TPersistent → TStrings → TStringList  
```

En lisant cette information, vous savez que :
- Vous devez ajouter `System.Classes` dans vos uses
- TStringList hérite de TStrings, donc possède toutes ses méthodes
- C'est une classe descendante de TObject (comme toutes les classes Delphi)

## Les guides de démarrage (Getting Started)

### Pour les nouveaux utilisateurs

Si vous débutez avec Delphi, commencez par ces guides :

**"Getting Started with RAD Studio"** : Introduction complète à l'environnement

**"Delphi Quick Start"** : Un tutoriel rapide pour créer votre première application

**"Mobile Tutorial Series"** : Si vous visez le développement mobile

### Parcours d'apprentissage recommandé

La documentation propose souvent des chemins d'apprentissage progressifs :

1. Installation et configuration
2. Création d'une application simple
3. Conception d'interface utilisateur
4. Connexion à une base de données
5. Déploiement de l'application

## Les notes de version (Release Notes)

### Pourquoi consulter les Release Notes ?

À chaque nouvelle version de Delphi, les Release Notes détaillent :
- **Les nouvelles fonctionnalités** : Ce qui a été ajouté
- **Les améliorations** : Ce qui a été optimisé
- **Les corrections de bugs** : Les problèmes résolus
- **Les changements incompatibles** : Ce qui peut affecter votre code existant

### Où les trouver ?

Dans le DocWiki, cherchez "What's New in RAD Studio" suivi du numéro de version. Pour Delphi 13 Florence, vous trouverez notamment :
- L'opérateur ternaire
- Les améliorations VCL et FireMonkey
- Le support LLDB v12
- Les nouveautés du site companion IA

## La documentation des bibliothèques tierces

### Composants inclus

Delphi inclut plusieurs bibliothèques documentées officiellement :
- **FireDAC** : Accès aux bases de données
- **TeeChart** : Création de graphiques
- **Indy** : Composants réseau et Internet
- **FireMonkey** : Framework multi-plateforme

Chacune possède sa propre section dans le DocWiki avec des guides détaillés.

## Documentation hors ligne

### Télécharger la documentation

Pour travailler sans connexion Internet, vous pouvez télécharger une version locale de la documentation :

1. Rendez-vous sur le site Embarcadero
2. Section "Documentation"
3. Téléchargez le package d'aide hors ligne correspondant à votre version

L'aide hors ligne s'intègre automatiquement dans l'IDE après installation.

## Conseils pour les débutants

### Comment tirer le meilleur parti de la documentation

**Commencez par les tutoriels** : Ne vous précipitez pas sur la référence API. Les tutoriels vous donnent un contexte et une progression logique.

**Utilisez F1 fréquemment** : N'hésitez pas à consulter l'aide pour chaque nouveau composant que vous découvrez. C'est en lisant la documentation qu'on apprend.

**Lisez les exemples de code** : Même si vous ne comprenez pas tout immédiatement, les exemples vous montrent les bonnes pratiques.

**Explorez les pages connexes** : En bas de chaque page, vous trouverez des liens vers des sujets similaires. Suivez-les pour approfondir vos connaissances.

**Marquez vos pages favorites** : Votre navigateur vous permet de sauvegarder les pages que vous consultez souvent.

### Que faire quand on ne trouve pas l'information ?

Si vous ne trouvez pas ce que vous cherchez dans la documentation :

1. **Reformulez votre recherche** : Essayez différents termes
2. **Consultez l'index alphabétique** : Parfois plus efficace que la recherche
3. **Vérifiez les forums officiels** : D'autres utilisateurs ont peut-être eu la même question
4. **Utilisez le companion IA** : Delphi 13 intègre une assistance IA pour le développement

### Comprendre les termes techniques

La documentation utilise parfois un vocabulaire technique. Ne vous découragez pas :

- **Property** (propriété) : Un attribut d'un composant que vous pouvez modifier
- **Method** (méthode) : Une fonction ou procédure d'une classe
- **Event** (événement) : Un déclencheur auquel vous pouvez réagir dans votre code
- **Unit** (unité) : Un fichier contenant du code Delphi
- **Component** (composant) : Un élément visuel ou non que vous ajoutez à votre application

## Ressources complémentaires dans la documentation officielle

### Code Examples Repository

Embarcadero maintient un dépôt d'exemples de code sur GitHub :  
https://github.com/Embarcadero  

Vous y trouverez des projets complets démontrant l'utilisation de fonctionnalités spécifiques.

### API Documentation Library

Pour une recherche très technique, la bibliothèque API documente chaque classe, fonction et constante disponible dans Delphi. C'est votre référence ultime quand vous maîtrisez les bases.

### Video Tutorials

La documentation officielle inclut aussi des liens vers des tutoriels vidéo créés par Embarcadero. Ces vidéos sont excellentes pour les apprenants visuels.

## Rester à jour avec la documentation

### S'abonner aux notifications

Le DocWiki permet de suivre les mises à jour :
- Nouvelles pages ajoutées
- Pages modifiées avec corrections
- Nouvelles versions de guides

### Consulter régulièrement

Même quand vous pensez maîtriser un sujet, revisitez la documentation périodiquement. Vous découvrirez souvent :
- Des fonctionnalités que vous aviez manquées
- De nouvelles façons de faire les choses
- Des optimisations et bonnes pratiques

## Conclusion

La documentation officielle de Delphi est une ressource extraordinaire qui mérite d'être explorée et consultée régulièrement. Ne la considérez pas comme une simple référence technique, mais comme votre guide d'apprentissage et votre compagnon de développement quotidien.

**Points clés à retenir** :
- Le DocWiki est accessible gratuitement en ligne
- Utilisez F1 dans l'IDE pour une aide contextuelle instantanée
- Commencez par les tutoriels avant de plonger dans la référence API
- Les Release Notes vous tiennent informé des nouveautés
- La documentation évolue et s'améliore constamment

Avec ces connaissances en main, vous êtes maintenant équipé pour exploiter pleinement la richesse de la documentation officielle de Delphi. N'oubliez pas : un bon développeur n'est pas celui qui connaît tout par cœur, mais celui qui sait où trouver l'information dont il a besoin !

⏭️ [Forums et groupes d'entraide](/20-ressources-et-communaute/02-forums-et-groupes-dentraide.md)
