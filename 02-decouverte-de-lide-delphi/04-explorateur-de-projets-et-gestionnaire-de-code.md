# 2.4 Explorateur de projets et gestionnaire de code

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Après avoir découvert la Palette d'outils et l'Inspecteur d'objets, nous allons maintenant explorer deux autres outils essentiels de Delphi : l'**Explorateur de projets** et le **Gestionnaire de code**. Ces outils vous aideront à organiser et à naviguer efficacement dans les fichiers de votre projet, surtout lorsque ceux-ci deviennent plus complexes.

## L'Explorateur de projets

L'Explorateur de projets vous offre une vue organisée de tous les fichiers et ressources qui composent votre application Delphi. C'est comme une carte qui vous aide à naviguer dans la structure de votre projet.

### Emplacement et accès

Par défaut, l'Explorateur de projets se trouve généralement sur le côté droit de l'IDE Delphi, souvent dans le même groupe de fenêtres que la Palette d'outils, mais dans un onglet différent. Si vous ne le voyez pas :

1. Allez dans le menu **Affichage > Gestionnaire de projet**
2. Ou utilisez le raccourci clavier **Ctrl+Alt+F11**

![Explorateur de projets](https://placeholder.com/delphi-project-explorer)

### Structure de l'Explorateur de projets

L'Explorateur de projets affiche votre projet sous forme d'arborescence hiérarchique :

- **Groupe de projets** (si vous avez plusieurs projets liés)
  - **Projet** (votre application principale)
    - **Fichiers source** (.pas, .dfm)
    - **Fichiers ressources** (.res, .rc)
    - **Fichiers de configuration** (.ini, .conf)
    - **Autres fichiers** (images, etc.)

Dans un projet Delphi typique, vous verrez généralement :

1. **Le fichier projet** (.dproj) : Contient les informations de configuration du projet
2. **Le fichier de projet principal** (.dpr) : Point d'entrée de l'application
3. **Les unités** (.pas) : Fichiers contenant le code Object Pascal
4. **Les fichiers de formulaire** (.dfm) : Décrivent l'interface visuelle

### Fonctionnalités de l'Explorateur de projets

L'Explorateur de projets offre plusieurs fonctionnalités utiles :

#### Navigation rapide

- **Double-clic** sur un fichier pour l'ouvrir dans l'éditeur
- **Clic droit** sur un élément pour accéder à un menu contextuel avec diverses options

#### Menu contextuel

Un clic droit sur un élément de l'Explorateur de projets affiche un menu contextuel avec plusieurs options :

- **Ouvrir** : Ouvre le fichier sélectionné
- **Ajouter** : Ajoute un nouveau fichier au projet
- **Supprimer du projet** : Retire le fichier du projet (sans le supprimer du disque)
- **Renommer** : Renomme le fichier
- **Voir le code source** : Affiche le code (pour les unités)
- **Voir la forme** : Affiche le concepteur de formulaire (pour les fichiers .dfm)
- **Compiler** : Compile uniquement le fichier sélectionné
- **Propriétés** : Affiche les propriétés du fichier

#### Organisation des projets

Pour les projets plus complexes, l'Explorateur de projets permet de :

- **Créer des dossiers virtuels** pour organiser les fichiers logiquement (clic droit > Ajouter > Nouveau dossier)
- **Gérer les groupes de projets** pour travailler avec plusieurs projets liés
- **Définir l'ordre de compilation** des unités

> **Astuce pour débutants :** Pour commencer, concentrez-vous simplement sur la navigation entre les fichiers de votre projet en double-cliquant sur les éléments dans l'Explorateur de projets.

### Personnalisation de l'affichage

Vous pouvez personnaliser l'affichage de l'Explorateur de projets selon vos préférences :

- **Tri des fichiers** : Par nom, type ou date de modification
- **Filtrage des fichiers** : Pour n'afficher que certains types de fichiers
- **Mode d'affichage** : Vue en arborescence ou vue plate

## Le Gestionnaire de code

Le Gestionnaire de code (également appelé Navigateur de code ou Structure) est un outil qui vous permet de visualiser et de naviguer dans la structure interne de vos unités Pascal. Il vous aide à comprendre l'organisation de votre code et à naviguer rapidement entre les différentes sections.

### Emplacement et accès

Par défaut, le Gestionnaire de code se trouve souvent sur le côté gauche de l'IDE. Si vous ne le voyez pas :

1. Allez dans le menu **Affichage > Structure**
2. Ou utilisez le raccourci clavier **Alt+F12**

![Gestionnaire de code](https://placeholder.com/delphi-code-manager)

### Structure du Gestionnaire de code

Le Gestionnaire de code affiche la structure de l'unité actuellement ouverte sous forme d'arborescence, regroupant les éléments par catégories :

- **Uses** : Unités importées
- **Types** : Définitions de types (classes, records, etc.)
  - **Classes** : Avec leurs méthodes, propriétés et événements
  - **Interfaces** : Avec leurs méthodes
  - **Records** : Avec leurs champs
- **Variables** : Variables globales déclarées dans l'unité
- **Constantes** : Constantes déclarées dans l'unité
- **Fonctions/Procédures** : Routines globales

### Fonctionnalités du Gestionnaire de code

Le Gestionnaire de code offre plusieurs fonctionnalités utiles :

#### Navigation rapide dans le code

La fonctionnalité principale du Gestionnaire de code est de permettre une navigation rapide :

1. **Double-cliquez** sur un élément dans le Gestionnaire de code pour vous rendre directement à sa déclaration dans l'éditeur
2. **Clic droit** sur un élément pour accéder à un menu contextuel avec des options spécifiques

#### Visualisation de la structure

Le Gestionnaire de code vous aide à comprendre la structure de votre code :

- **Hiérarchie des classes** : Visualisez les relations d'héritage
- **Membres des classes** : Voyez tous les champs, méthodes et propriétés
- **Visibilité** : Les icônes indiquent la visibilité des membres (public, privé, protégé)

#### Filtrage et recherche

Pour les unités complexes avec beaucoup d'éléments, vous pouvez :

- **Filtrer par nom** : Utilisez la zone de recherche en haut du Gestionnaire de code
- **Filtrer par type** : Affichez uniquement certains types d'éléments
- **Trier** : Organisez les éléments par nom ou par ordre de déclaration

### Utilisation pratique du Gestionnaire de code

Voici quelques scénarios où le Gestionnaire de code est particulièrement utile :

#### Exploration d'une nouvelle unité

Lorsque vous travaillez avec une unité que vous n'avez pas créée ou que vous n'avez pas vue depuis longtemps, le Gestionnaire de code vous donne un aperçu rapide de sa structure sans avoir à parcourir tout le code.

#### Navigation dans les grands fichiers

Dans les unités contenant beaucoup de code, le Gestionnaire de code vous permet de sauter directement à la méthode ou à la propriété qui vous intéresse.

#### Comprendre les relations entre classes

Pour les architectures orientées objet complexes, le Gestionnaire de code vous aide à visualiser les hiérarchies de classes et leurs membres.

## Interactions entre l'Explorateur de projets et le Gestionnaire de code

Ces deux outils fonctionnent de manière complémentaire :

1. Utilisez l'**Explorateur de projets** pour naviguer entre les différents fichiers du projet
2. Une fois dans un fichier, utilisez le **Gestionnaire de code** pour naviguer entre les éléments au sein de ce fichier

## Quelques astuces pratiques

### Organisation efficace d'un projet

1. **Créez des unités thématiques** : Regroupez le code lié dans des unités dédiées (par exemple, UUtils pour les fonctions utilitaires, UDatabase pour l'accès aux données)

2. **Utilisez des dossiers virtuels** dans l'Explorateur de projets pour organiser logiquement vos fichiers :
   - Modèles (classes de données)
   - Vues (formulaires)
   - Utilitaires
   - Ressources

3. **Nommez clairement vos fichiers** pour faciliter la recherche dans l'Explorateur de projets :
   - Préfixez les noms d'unités (U pour les unités, F pour les frames, etc.)
   - Utilisez des noms descriptifs

### Raccourcis clavier utiles

| Raccourci | Action |
|-----------|--------|
| Ctrl+Alt+F11 | Afficher/masquer l'Explorateur de projets |
| Alt+F12 | Afficher/masquer le Gestionnaire de code |
| Ctrl+Tab | Basculer entre les fichiers ouverts |
| Ctrl+F12 | Afficher la liste des méthodes de la classe actuelle |
| F12 | Basculer entre l'interface et l'implémentation |

## Exercice pratique

Pour vous familiariser avec ces outils, essayez cet exercice simple :

1. Créez un nouveau projet avec plusieurs formulaires (au moins deux)
2. Ajoutez une unité sans formulaire pour contenir des fonctions utilitaires
3. Dans l'Explorateur de projets :
   - Créez un dossier virtuel "Forms" et déplacez-y vos fichiers de formulaire
   - Créez un dossier virtuel "Utils" et déplacez-y votre unité utilitaire
4. Utilisez le Gestionnaire de code pour naviguer entre les différentes procédures et méthodes

## Conclusion

L'Explorateur de projets et le Gestionnaire de code sont des outils essentiels qui vous aideront à travailler efficacement sur des projets Delphi, particulièrement lorsque ceux-ci deviennent plus complexes. En maîtrisant ces outils, vous gagnerez en productivité et aurez une meilleure compréhension de la structure de votre application.

Dans la prochaine section, nous verrons comment compiler et exécuter un projet Delphi, ainsi que les différentes options de compilation disponibles.

⏭️ [Compilation et exécution](/02-decouverte-de-lide-delphi/05-compilation-et-execution.md)
