🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.1 Présentation de l'interface

## Introduction

Lorsque vous lancez Delphi pour la première fois, vous découvrez un environnement de développement riche et complet. Ne vous laissez pas impressionner par le nombre de fenêtres et d'options : chaque élément a sa fonction, et vous allez rapidement vous familiariser avec cet espace de travail.

L'interface de Delphi 13 Florence est conçue pour vous permettre de développer des applications de manière visuelle et intuitive. Prenons le temps de découvrir ensemble les différentes zones qui composent cet environnement.

## Le tout premier écran : la Welcome Page

Quand vous lancez Delphi 13 pour la première fois, vous êtes accueilli par la **Welcome Page** (page d'accueil). C'est un point de départ pratique qui regroupe :

- **Vos projets récents** (la liste est vide au premier lancement)
- **Des modèles de projets** pour démarrer rapidement (VCL, FireMonkey, console, etc.)
- **Des liens vers la documentation** et les tutoriels
- **Les nouveautés de la version** que vous utilisez
- **Le site web companion IA** (nouveau dans Delphi 13)

Cette page reste accessible en permanence via un onglet en haut de l'IDE. Vous pouvez la fermer si vous préférez et la rouvrir via le menu **View > Welcome Page** (Affichage > Page d'accueil).

> 💡 **Astuce :** Au début, vous pouvez décocher l'option "Show on startup" si vous préférez démarrer directement sans la voir.

## Vue d'ensemble de l'interface

L'IDE (Integrated Development Environment) de Delphi est organisé en plusieurs zones principales qui travaillent ensemble pour faciliter votre développement. Voici un schéma simplifié de la disposition standard :

```
┌────────────────────────────────────────────────────────────────────────┐
│  Barre de menus : File  Edit  Search  View  Project  Run  Tools  Help  │
├────────────────────────────────────────────────────────────────────────┤
│  Barres d'outils : [New] [Open] [Save] | [Run▶] [Pause] [Stop] | ...   │
├──────────────┬──────────────────────────────────────┬──────────────────┤
│              │                                      │                  │
│              │                                      │  Project Manager │
│   Tool       │     Form Designer / Code Editor      │  (Explorateur de │
│   Palette    │                                      │   projets)       │
│   (Palette   │   - Vue "Design" : conception        │                  │
│    d'outils) │     visuelle de l'interface          ├──────────────────┤
│              │   - Vue "Code" (F12) : éditeur       │                  │
│   Composants │     de code Object Pascal            │ Object Inspector │
│   classés    │                                      │  (Inspecteur     │
│   par        │                                      │   d'objets)      │
│   catégorie  │                                      │                  │
│              │                                      │  - Properties    │
│              │                                      │  - Events        │
│              ├──────────────────────────────────────┤                  │
│              │ Messages : erreurs, warnings, debug  │ Structure View   │
└──────────────┴──────────────────────────────────────┴──────────────────┘
```

Cette disposition est **personnalisable** : vous pouvez déplacer chaque fenêtre, l'ancrer ailleurs, ou la faire flotter. Mais cette organisation par défaut est un bon point de départ.

Détaillons maintenant chaque zone :

### La barre de menus

Située tout en haut de l'écran, la barre de menus contient toutes les commandes principales de Delphi. Vous y trouverez les menus classiques :

**Fichier (File)** : pour créer, ouvrir, enregistrer vos projets et fichiers

**Édition (Edit)** : pour les opérations de copier-coller et de recherche dans le code

**Rechercher (Search)** : pour effectuer des recherches avancées dans vos fichiers

**Affichage (View)** : pour afficher ou masquer les différentes fenêtres de l'IDE

**Projet (Project)** : pour gérer les paramètres et les fichiers de votre projet

**Exécuter (Run)** : pour compiler et exécuter votre application

**Outils (Tools)** : pour accéder aux utilitaires et aux options de configuration

**Aide (Help)** : pour consulter la documentation et l'aide en ligne

### La barre d'outils

Juste en dessous de la barre de menus, vous trouvez une ou plusieurs barres d'outils. Ces barres contiennent des boutons pour accéder rapidement aux fonctions les plus utilisées, comme :

- Le bouton "Nouveau" pour créer un nouveau fichier
- Les boutons "Ouvrir" et "Enregistrer"
- Les boutons de compilation et d'exécution (avec l'icône de lecture verte)
- Les boutons pour déboguer votre application

Ces barres d'outils sont personnalisables : vous pouvez les déplacer, les masquer ou afficher uniquement celles dont vous avez besoin.

## Les fenêtres principales

### Le concepteur de fiches (Form Designer)

C'est la grande zone centrale où vous concevez visuellement l'interface de votre application. Quand vous créez un nouveau projet, une fiche vide (formulaire) apparaît ici. C'est sur cette fiche que vous allez placer vos boutons, zones de texte, images et tous les autres éléments visuels de votre application.

Pensez à cette zone comme à une toile vierge sur laquelle vous allez dessiner l'interface de votre programme. Vous pouvez y faire glisser des composants depuis la palette d'outils, les déplacer, les redimensionner, exactement comme vous le feriez dans un logiciel de dessin.

### L'éditeur de code (Code Editor)

Accessible via des onglets en bas du concepteur de fiches, l'éditeur de code est l'endroit où vous écrivez le code Object Pascal qui donnera vie à votre application. C'est ici que vous programmerez le comportement de vos boutons, que vous traiterez les données, et que vous implémenterez la logique de votre application.

L'éditeur de code de Delphi est très sophistiqué :

- Il colore automatiquement votre code pour le rendre plus lisible
- Il propose des suggestions pendant que vous tapez (auto-complétion)
- Il vous signale les erreurs de syntaxe en temps réel
- Il vous permet de naviguer facilement entre différentes parties de votre code

Vous pouvez basculer entre la vue concepteur et la vue code en utilisant la touche **F12**, ou en cliquant sur les onglets correspondants. La touche **F11** donne le focus à l'inspecteur d'objets (et permet aussi de basculer rapidement entre l'éditeur, le concepteur et l'inspecteur).

### L'inspecteur d'objets (Object Inspector)

Généralement situé sur la droite de l'écran, l'inspecteur d'objets est votre centre de contrôle pour tous les composants de votre application. Cette fenêtre se divise en deux onglets principaux :

**Propriétés (Properties)** : ici vous définissez les caractéristiques de vos composants. Par exemple, pour un bouton, vous pouvez modifier :
- Son texte (Caption)
- Sa taille (Width, Height)
- Sa position (Left, Top)
- Sa couleur
- Sa police de caractères
- Et bien d'autres paramètres

**Événements (Events)** : c'est ici que vous connectez vos composants au code. Par exemple, vous pouvez définir ce qui se passe quand l'utilisateur clique sur un bouton (événement OnClick), quand il survole un élément avec la souris, etc.

L'inspecteur d'objets change automatiquement pour afficher les propriétés du composant que vous avez sélectionné sur la fiche. C'est un outil que vous utiliserez constamment.

### La palette d'outils (Tool Palette)

La palette d'outils est votre boîte à outils pour construire l'interface de votre application. Elle contient tous les composants visuels et non-visuels que vous pouvez ajouter à votre fiche.

Les composants sont organisés par catégories :

**Standard** : les composants de base comme les boutons, les zones de texte, les étiquettes

**Additional** : des composants supplémentaires comme les grilles, les barres de progression

**Win32** : des composants spécifiques à Windows

**System** : des composants pour interagir avec le système

**Data Controls** : des composants pour afficher et modifier des données

Et bien d'autres catégories encore.

Pour utiliser un composant, il suffit de cliquer dessus dans la palette, puis de cliquer sur votre fiche à l'endroit où vous souhaitez le placer. Vous pouvez aussi double-cliquer sur un composant pour qu'il soit automatiquement ajouté à votre fiche.

### L'explorateur de projets (Project Manager)

Cette fenêtre, généralement située en haut à droite, vous montre la structure de votre projet. Un projet Delphi peut contenir plusieurs fichiers : des fiches, des unités de code, des ressources, etc. L'explorateur de projets vous permet de naviguer facilement entre tous ces éléments.

Vous pouvez :
- Voir tous les fichiers qui composent votre projet
- Ajouter ou supprimer des fichiers
- Accéder rapidement à n'importe quel fichier en double-cliquant dessus
- Gérer plusieurs projets en même temps (dans un groupe de projets)

### L'explorateur de structure (Structure)

Souvent situé sous l'explorateur de projets, l'explorateur de structure affiche l'organisation de votre code : les classes, les procédures, les fonctions, les variables. C'est un outil pratique pour naviguer rapidement dans du code complexe et comprendre comment il est organisé.

## Personnalisation de l'interface

L'une des forces de Delphi est que vous pouvez adapter l'interface à vos préférences. Vous pouvez :

**Déplacer les fenêtres** : toutes les fenêtres peuvent être détachées et repositionnées. Vous pouvez les ancrer à différents endroits de l'écran ou les laisser flottantes.

**Redimensionner les zones** : en faisant glisser les bordures entre les différentes zones, vous pouvez donner plus ou moins d'espace à chaque fenêtre.

**Masquer ou afficher des fenêtres** : via le menu **View** (Affichage), vous pouvez choisir quelles fenêtres afficher selon vos besoins du moment.

**Activer l'Auto-Hide (masquage automatique)** : pour les fenêtres ancrées, cliquez sur la petite icône punaise (📌) dans leur barre de titre. La fenêtre se replie en un onglet sur le bord de l'IDE et ne s'affiche qu'au survol. C'est très pratique pour gagner de l'espace tout en gardant les fenêtres accessibles.

**Enregistrer des dispositions** : une fois que vous avez organisé votre interface comme vous le souhaitez, vous pouvez enregistrer cette disposition pour la retrouver facilement. Allez dans **View > Desktops** (Affichage > Dispositions de bureau) pour gérer vos dispositions personnalisées.

> 📌 **Note sur la langue de l'IDE :** Cette formation utilise systématiquement les noms anglais des menus suivis de leur traduction française entre parenthèses. L'IDE Delphi n'est pas officiellement distribué en français (seuls l'anglais, l'allemand et le japonais sont disponibles selon les éditions et les régions). L'anglais est donc la langue à laquelle vous serez exposé dans l'IDE, la DocWiki et la plupart des ressources communautaires. La traduction française que nous fournissons sert uniquement à clarifier le sens des termes pendant l'apprentissage.

## Les nouveautés de l'interface dans Delphi 13 Florence

Delphi 13 apporte plusieurs améliorations significatives à l'interface :

**IDE 64 bits natif** : pour la première fois en près de 30 ans, l'IDE Delphi tourne comme une véritable application 64 bits (Delphi 2 était passé en 32 bits en 1996, et l'IDE l'est resté jusqu'à Delphi 12). Cela élimine l'ancienne limite mémoire (~4 Go par processus en 32 bits) et permet de travailler sur de très gros projets avec de nombreux packages chargés simultanément.

**Meilleure prise en charge des écrans haute résolution** : l'interface s'adapte mieux aux écrans 4K, 5K, 8K, et aux configurations multi-moniteurs, grâce à des icônes vectorielles et un rendu HiDPI optimisé.

**Thèmes visuels améliorés** : vous pouvez choisir entre un thème clair, un thème sombre, et plusieurs variantes intermédiaires (Mountain Mist, Charcoal Dark Slate), plus confortables pour de longues sessions.

**Amélioration du gestionnaire GetIt** : l'accès aux packages et composants tiers est plus intégré et fluide, avec une meilleure gestion des versions (voir section 2.10).

**Support LLDB v12** : pour un débogage modernisé sur macOS, iOS, Android et Linux (Windows conserve son débogueur natif).

**Site web companion IA** : un nouvel outil d'assistance pour répondre à vos questions et générer du code Object Pascal (voir section 2.11).

## Conseils pour bien débuter

**Ne cherchez pas à tout mémoriser** : au début, l'interface peut sembler complexe avec toutes ses fenêtres et options. C'est normal ! Concentrez-vous d'abord sur les zones principales : le concepteur de fiches, l'éditeur de code, l'inspecteur d'objets et la palette d'outils.

**Utilisez les raccourcis clavier** : quelques raccourcis essentiels à retenir dès le début :
- **F12** : basculer entre la fiche et le code
- **F9** : compiler et exécuter
- **Ctrl + Espace** : auto-complétion du code
- **Ctrl + S** : enregistrer
- **Ctrl + .** (Control + point) : ouvrir **IDE Insight**, la recherche universelle. Tapez n'importe quoi (nom de commande, de fichier, d'option) et l'IDE le trouve. À retenir absolument !

**N'hésitez pas à réorganiser** : testez différentes dispositions de fenêtres pour trouver celle qui vous convient le mieux. Chaque développeur a ses préférences !

**Explorez les menus** : prenez le temps de parcourir les différents menus pour découvrir les fonctionnalités disponibles. Vous ne les utiliserez pas toutes immédiatement, mais c'est utile de savoir ce qui existe.

## Conclusion

L'interface de Delphi est votre espace de travail principal pour les semaines et mois à venir. Au début, elle peut sembler intimidante avec toutes ses fenêtres et options, mais vous verrez qu'avec la pratique, vous vous y sentirez rapidement à l'aise.

Chaque zone de l'interface a un rôle précis : le concepteur pour dessiner votre interface, l'éditeur pour écrire votre code, l'inspecteur pour configurer vos composants, la palette pour ajouter des éléments. Une fois que vous aurez compris cette organisation logique, vous naviguerez naturellement d'une zone à l'autre selon vos besoins.

Dans la prochaine section, nous mettrons tout cela en pratique en créant votre premier projet Delphi !

⏭️ [Création d'un premier projet](/02-decouverte-de-lide-delphi/02-creation-dun-premier-projet.md)
