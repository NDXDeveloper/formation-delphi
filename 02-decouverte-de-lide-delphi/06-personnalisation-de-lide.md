🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.6 Personnalisation de l'IDE

## Introduction

Chaque développeur a sa propre façon de travailler, ses préférences visuelles, ses habitudes. L'IDE Delphi est extrêmement flexible et vous permet de l'adapter à votre style personnel. Vous pouvez modifier presque tous les aspects de l'interface : les couleurs, la disposition des fenêtres, les polices, les raccourcis clavier, et bien plus encore.

Personnaliser votre environnement de travail n'est pas un luxe superficiel : un IDE bien configuré améliore votre productivité, réduit la fatigue visuelle, et rend le développement plus agréable. Dans cette section, nous allons explorer toutes les possibilités de personnalisation de Delphi.

## Accéder aux options de personnalisation

### Le menu Options

Le point central de la personnalisation est le menu **Tools > Options** (Outils > Options). Cette fenêtre de dialogue contient des dizaines de pages d'options, organisées en catégories dans une arborescence à gauche.

**Astuce :** vous pouvez aussi y accéder rapidement via **Ctrl + .** (IDE Insight) en tapant "options".

Au premier abord, cela peut sembler intimidant avec toutes ces options ! Mais ne vous inquiétez pas : les valeurs par défaut sont généralement bonnes, et vous n'avez pas besoin de tout modifier. Nous allons explorer les options les plus utiles pour débuter.

### Structure du dialogue Options

L'arborescence de gauche organise les options en grandes catégories (les noms anglais sont ceux affichés par l'IDE quand il est en langue anglaise, choix par défaut) :

**Environment Options** : options générales de l'IDE (dossiers de projets, proxy, options d'enregistrement, etc.)

**User Interface** : apparence de l'IDE (thème, éditeur, raccourcis clavier, Form Designer, IDE Insight)

**Editor** : tout ce qui concerne l'édition du code (police, couleurs, Code Insight, templates, sauvegarde)

**Language** : options spécifiques au langage (Delphi, C++) et chemins de bibliothèques

**Debugger Options** : configuration du débogage (exceptions, symboles, types de débogueurs)

**Type Library**, **Translation Tools**, **Resource Compiler** : outils spécifiques (avancé)

Chaque catégorie contient plusieurs sous-catégories. Explorez-les tranquillement pour découvrir ce qui est disponible. La structure exacte peut varier légèrement selon votre édition Delphi (Community / Pro / Enterprise / Architect).

## Personnaliser l'apparence générale

### Thèmes visuels

Delphi 13 Florence propose plusieurs thèmes visuels pour l'IDE. Le choix du thème est une question de préférence personnelle, mais peut avoir un impact important sur votre confort visuel.

Pour changer de thème :

1. Allez dans **Tools > Options > User Interface > IDE > Theme** (Outils > Options > Interface utilisateur > IDE > Thème)
2. Dans la liste déroulante, choisissez un thème

Les thèmes disponibles dans Delphi 13 :

**Light** : thème clair, classique, avec fond blanc. Idéal pour les environnements bien éclairés.

**Dark** : thème sombre avec fond noir ou gris foncé. Réduit la fatigue oculaire, particulièrement apprécié pour de longues sessions de programmation, surtout en soirée.

**Mountain Mist** : variante claire avec des tons gris doux

**Charcoal Dark Slate** : variante sombre encore plus sobre

Après avoir changé de thème, un redémarrage de l'IDE peut être nécessaire pour rafraîchir certains éléments (titre de fenêtre, certains panneaux).

**Conseil** : testez différents thèmes pour trouver celui qui vous convient. Le thème sombre est très populaire parmi les développeurs, mais certains préfèrent le thème clair. C'est une question de préférence personnelle.

### Langue de l'interface

L'IDE Delphi est principalement distribué en **anglais**. Selon les éditions et les régions, Embarcadero propose également des installeurs traduits en **allemand** et en **japonais** (il n'existe pas de version officielle en français de l'IDE lui-même). La documentation et la majorité des ressources communautaires sont en anglais.

Si une langue alternative est disponible sur votre installation, vous la trouverez dans :

1. **Tools > Options > Environment Options** (Outils > Options > Options d'environnement)
2. Section **Language** (si l'option apparaît)
3. Redémarrez Delphi après le changement

Pour ce tutoriel, nous mentionnons systématiquement le nom anglais des menus suivi de leur traduction française entre parenthèses, afin que vous puissiez vous repérer quelle que soit la configuration utilisée.

### Taille des polices de l'interface

Sur les écrans haute résolution (4K), l'interface peut sembler petite. Vous pouvez ajuster la taille :

1. **Tools > Options > User Interface > IDE > Fonts**
2. Ou ajustez la **mise à l'échelle Windows** dans les paramètres système (recommandé en premier)

Delphi 13 gère beaucoup mieux les écrans haute résolution que les versions précédentes grâce à son IDE 64 bits avec icônes vectorielles, mais vous devrez peut-être quand même ajuster certains paramètres.

## Personnaliser l'éditeur de code

L'éditeur de code est l'endroit où vous passerez le plus de temps. Le personnaliser correctement est donc crucial pour votre confort et votre productivité.

### Police et taille de caractères

La police de l'éditeur de code affecte directement la lisibilité et le confort de lecture.

Pour la configurer :

1. **Tools > Options > Editor > Display** (Outils > Options > Éditeur > Affichage)
2. Cherchez la section **Editor Font** (Police de l'éditeur)

**Police** : choisissez une police à chasse fixe (monospace). Les plus populaires :
- **Consolas** : la police par défaut de Delphi, excellente lisibilité
- **Courier New** : classique et universelle
- **Fira Code** : moderne, gérant les ligatures de code (si vous les aimez)
- **Source Code Pro** : très lisible, créée par Adobe
- **JetBrains Mono** : récente et très populaire

**Taille** : généralement entre 9 et 12 points. Choisissez selon votre écran et votre vue :
- 9-10 : pour voir plus de code à l'écran
- 11-12 : pour une meilleure lisibilité
- 13+ : si vous avez des problèmes de vue ou un grand écran

**Conseil** : utilisez une taille qui vous permet de lire confortablement sans vous pencher vers l'écran. Votre cou vous remerciera !

### Coloration syntaxique

La coloration syntaxique aide à distinguer les différents éléments du code. Delphi colore automatiquement :

- Les mots-clés du langage (begin, end, if, etc.)
- Les chaînes de caractères
- Les commentaires
- Les nombres
- Les identificateurs
- Les symboles

Pour personnaliser ces couleurs :

1. **Tools > Options > Editor > Color** (Outils > Options > Éditeur > Couleurs)
2. Sélectionnez un élément dans la liste (par exemple, "Reserved word")
3. Choisissez la couleur de texte et/ou de fond

**Attention** : la coloration par défaut est bien pensée. Ne modifiez les couleurs que si vous avez une bonne raison. Un excès de couleurs peut nuire à la lisibilité.

**Pour les thèmes sombres** : si vous utilisez un thème sombre, assurez-vous que les couleurs du code sont adaptées. Vous pouvez sélectionner des jeux de couleurs prédéfinis pour thème sombre.

### Options d'affichage

Dans **Tools > Options > Editor > Display** (Outils > Options > Éditeur > Affichage), vous trouverez de nombreuses options d'affichage :

**Numéros de ligne** : affiche les numéros de ligne dans la marge gauche. Très utile pour la navigation et le débogage. Recommandé : **activé**.

**Soulignement des URL** : souligne les URLs dans le code pour qu'elles ressemblent à des liens. Recommandé : selon préférence.

**Espaces de tabulation visibles** : affiche les tabulations sous forme de petits symboles. Utile pour détecter les problèmes d'indentation. Recommandé : **selon préférence** (peut être distrayant).

**Souligner la ligne actuelle** : met en évidence la ligne où se trouve votre curseur. Recommandé : **activé** (aide à se repérer).

**Mise en évidence des blocs** : affiche des guides verticaux pour les blocs begin/end. Très utile pour voir la structure du code. Recommandé : **activé**.

**Retour à la ligne** : fait revenir automatiquement le texte à la ligne au lieu d'avoir un défilement horizontal. Selon préférence, mais généralement **désactivé** pour du code.

### Options d'édition

Dans **Tools > Options > Editor > Editor** (Outils > Options > Éditeur > Éditeur), vous contrôlez le comportement de l'éditeur :

**Indentation automatique** : indente automatiquement le code au fur et à mesure que vous tapez. Recommandé : **activé**.

**Taille de tabulation** : généralement 2 ou 4 espaces. La convention Delphi est 2. Recommandé : **2**.

**Utiliser la touche Tab** : insère une tabulation ou des espaces. Recommandé : **espaces** pour éviter les problèmes de formatage entre différents éditeurs.

**Sauvegarde automatique** : sauvegarde automatiquement votre code à intervalles réguliers. Recommandé : **activé**, toutes les 5 minutes.

**Accolades correspondantes** : met en évidence l'accolade/parenthèse correspondante quand vous positionnez le curseur. Recommandé : **activé**.

**Complétion automatique des blocs** : ajoute automatiquement le `end` quand vous tapez `begin`. Recommandé : **activé**.

### Code Insight

Dans **Tools > Options > Editor > Code Insight** (Outils > Options > Éditeur > Code Insight), vous configurez l'auto-complétion et l'aide contextuelle :

**Délai d'invocation automatique** : temps d'attente avant que l'auto-complétion n'apparaisse automatiquement. Valeur recommandée : 1000 ms (1 seconde).

**Activer Code Insight** : active les fonctionnalités d'assistance au code. Recommandé : **activé** (sauf si vous trouvez cela trop intrusif).

**Info-bulles sur les symboles** : affiche des informations quand vous survolez un élément. Recommandé : **activé**.

**Navigation par Ctrl+clic** : permet de naviguer vers les déclarations avec Ctrl+clic. Recommandé : **activé**.

Si vous trouvez que Code Insight vous interrompt trop souvent, vous pouvez augmenter le délai ou le désactiver. Mais dans ce cas, vous devrez invoquer manuellement l'auto-complétion avec **Ctrl + Espace**.

### Templates de code

Les templates (ou modèles de code) sont des raccourcis pour insérer rapidement des structures courantes.

Pour les gérer :

1. **Tools > Options > Editor > Code Insight > Templates** (Outils > Options > Éditeur > Code Insight > Templates)
2. Vous voyez la liste des templates existants
3. Vous pouvez en ajouter, modifier ou supprimer

Exemples de templates utiles :

- `try` → bloc try-finally
- `tryf` → bloc try-finally
- `trye` → bloc try-except
- `ife` → structure if-then-else
- `fori` → boucle for avec indice

Vous pouvez créer vos propres templates pour les structures que vous utilisez souvent. Par exemple, si vous écrivez souvent des procédures de journalisation, créez un template `log` qui insère le code nécessaire.

## Organiser l'espace de travail

### Dispositions de bureau (Desktop Layouts)

Une disposition de bureau est une configuration sauvegardée de l'arrangement de toutes vos fenêtres : position, taille, visibilité, etc.

Delphi propose plusieurs dispositions par défaut, mais vous pouvez créer les vôtres.

#### Utiliser les dispositions

Pour changer de disposition :

**View > Desktops** (Affichage > Dispositions de bureau)

Vous verrez plusieurs options :
- **Default Layout** : la disposition par défaut
- **Debug Layout** : optimisée pour le débogage (avec les fenêtres de débogage visibles)
- **Classic Undocked** : fenêtres non ancrées, flottantes

Vous pouvez basculer entre ces dispositions selon votre activité du moment.

#### Créer votre propre disposition

1. Organisez vos fenêtres exactement comme vous le souhaitez
2. **View > Desktops > Save Desktop** (Affichage > Dispositions de bureau > Enregistrer la disposition)
3. Donnez-lui un nom significatif (par exemple, "Ma disposition VCL" ou "Ma disposition Édition de code")
4. Votre disposition personnalisée apparaît maintenant dans le menu

**Astuce** : créez différentes dispositions pour différentes tâches :
- Une pour le développement VCL (avec Inspecteur d'objets et Palette bien visibles)
- Une pour le développement FireMonkey
- Une pour le débogage (avec fenêtres de surveillance, pile d'appels, etc.)
- Une pour l'édition pure de code (éditeur maximisé, autres fenêtres cachées)

#### Gérer les dispositions

Pour supprimer ou renommer une disposition :

**View > Desktops > Manage Desktops** (Affichage > Dispositions de bureau > Gérer les dispositions)

### Ancrage et positionnement des fenêtres

Toutes les fenêtres de Delphi peuvent être :

**Ancrées** : attachées aux bords de l'IDE, intégrées dans l'interface

**Flottantes** : fenêtres indépendantes que vous pouvez déplacer librement, même sur un autre écran

**Onglets** : plusieurs fenêtres groupées en onglets dans une même zone

#### Ancrer une fenêtre

Pour ancrer une fenêtre flottante :

1. Faites glisser la fenêtre par sa barre de titre
2. Approchez-la d'un bord de l'IDE
3. Des zones bleues apparaissent pour indiquer où la fenêtre peut être ancrée
4. Relâchez la souris sur la zone souhaitée

#### Détacher une fenêtre

Pour rendre flottante une fenêtre ancrée :

1. Double-cliquez sur la barre de titre (ou sa poignée de déplacement)
2. Ou faites-la glisser hors de sa zone d'ancrage

**Pour les multi-écrans** : si vous avez plusieurs écrans, vous pouvez mettre certaines fenêtres (comme l'Inspecteur d'objets ou la Palette) sur un écran secondaire, et garder l'éditeur de code sur l'écran principal.

#### Redimensionner les zones

Les zones ancrées peuvent être redimensionnées en faisant glisser leurs bordures. C'est pratique pour donner plus d'espace à l'éditeur de code ou à l'Inspecteur d'objets selon vos besoins.

### Fenêtres automatiquement masquables

Certaines fenêtres peuvent être configurées pour se masquer automatiquement quand vous ne les utilisez pas, libérant ainsi de l'espace :

1. Cliquez sur l'icône **punaise** (📌) dans la barre de titre d'une fenêtre ancrée
2. Ou faites un clic droit > **Auto Hide** (Masquage automatique)

La fenêtre se réduit alors en un onglet sur le bord de l'IDE. Passez votre souris sur cet onglet pour que la fenêtre réapparaisse temporairement.

C'est utile pour les fenêtres que vous consultez occasionnellement (Explorateur de structure, Messages, etc.) mais qui prennent de la place quand vous n'en avez pas besoin.

### Barres d'outils

Vous pouvez personnaliser les barres d'outils : ajouter ou retirer des boutons, créer vos propres barres, etc.

Pour personnaliser :

1. **View > Toolbars > Customize** (Affichage > Barres d'outils > Personnaliser)
2. Ou clic droit sur une barre d'outils et choisir **Customize** (Personnaliser)

Dans la boîte de dialogue de personnalisation :

**Onglet Barres d'outils** : choisissez quelles barres afficher

**Onglet Commandes** : parcourez toutes les commandes disponibles et faites-les glisser vers les barres d'outils

**Onglet Options** : configurez l'apparence des barres (grandes icônes, afficher les étiquettes, etc.)

Vous pouvez créer votre propre barre d'outils avec uniquement les boutons que vous utilisez vraiment. Par exemple, une barre "Database" avec tous les outils de base de données, ou une barre "My Tools" avec vos fonctions favorites.

## Personnaliser les raccourcis clavier

Les raccourcis clavier sont essentiels pour la productivité. Delphi propose des centaines de raccourcis, et vous pouvez tous les personnaliser.

### Voir et modifier les raccourcis

1. **Tools > Options > User Interface > Editor > Key Mappings** (Outils > Options > Interface utilisateur > Éditeur > Raccourcis clavier)
2. Vous voyez la liste de toutes les commandes disponibles
3. Pour chaque commande, les raccourcis actuels sont affichés

### Modifier un raccourci

1. Sélectionnez une commande dans la liste
2. Dans la section du bas, vous voyez le raccourci actuel
3. Cliquez dans le champ **Press shortcut keys** (Nouveau raccourci)
4. Appuyez sur la combinaison de touches souhaitée (par exemple, **Ctrl + Shift + Z**)
5. Si le raccourci est déjà utilisé, Delphi vous le signale
6. Cliquez sur **Assign** (Affecter) pour valider

### Raccourcis à personnaliser pour les débutants

La plupart des raccourcis par défaut sont bien pensés, mais voici quelques suggestions de personnalisation courantes :

**Sauvegarder tout** : par défaut **Ctrl + Shift + S**. Certains préfèrent **Ctrl + Shift + A** pour l'uniformité avec d'autres logiciels.

**Commenter/Décommenter** : par défaut **Ctrl + /**. Vérifiez que ce raccourci vous convient.

**Formater le code** : par défaut **Ctrl + D**. Très utile, assurez-vous qu'il est bien configuré.

**Conseil** : ne modifiez pas trop de raccourcis au début. Apprenez d'abord les raccourcis par défaut, puis personnalisez uniquement ceux qui vous gênent vraiment. Trop de modifications peuvent vous désorienter, surtout si vous passez d'un ordinateur à un autre.

### Jeux de raccourcis

Delphi propose différents jeux de raccourcis (keymap) :

- **Delphi Classic** : raccourcis traditionnels de Delphi
- **Visual Studio** : pour ceux habitués à Visual Studio
- **Autres** : selon votre version

Vous pouvez changer de jeu dans **Tools > Options > User Interface > Editor > Key Mappings** (Outils > Options > Interface utilisateur > Éditeur > Raccourcis clavier), en haut de la fenêtre.

Si vous venez d'un autre environnement de développement, utiliser un jeu de raccourcis familier peut faciliter la transition.

## Personnaliser le concepteur de fiches

### Grille et guides

Quand vous placez des composants sur une fiche, une grille invisible vous aide à les aligner.

Pour configurer cette grille :

1. **Tools > Options > User Interface > Form Designer** (Outils > Options > Interface utilisateur > Concepteur de fiches)
2. Cherchez les options de **grille**

**Afficher la grille** : rend la grille visible (petits points sur la fiche). Selon préférence.

**Aligner sur la grille** : fait que les composants se "collent" automatiquement aux points de la grille. Recommandé : **activé** pour faciliter l'alignement.

**Espacement de la grille X et Y** : distance en pixels entre les points de la grille. Valeur courante : 8 pixels. Ajustez selon votre préférence.

**Guides d'alignement** : des lignes bleues qui apparaissent pour vous aider à aligner les composants entre eux. Recommandé : **activé**.

### Options de création de composants

**Créer avec les valeurs par défaut** : quand vous double-cliquez sur un composant dans la Palette, il est placé automatiquement au centre de la fiche avec sa taille par défaut.

**Afficher les noms de composants** : affiche le nom du composant sur le composant lui-même dans le concepteur. Utile pour s'y retrouver sur les fiches complexes. Selon préférence.

### Comportement de sélection

Vous pouvez configurer comment les composants sont sélectionnés et déplacés :

**Déplacement incrémental** : définit de combien de pixels les composants se déplacent quand vous utilisez les flèches du clavier. Valeur courante : 1 pixel (avec Shift pour déplacements plus grands).

## Personnaliser le débogueur

Le débogueur a aussi ses options de personnalisation. Nous verrons le débogage en détail dans un chapitre ultérieur, mais voici quelques options de base :

1. **Tools > Options > Debugger Options** (Outils > Options > Options du débogueur)

**Options de débogueur** :
- **Language Exceptions** : le programme s'arrête quand une exception se produit, même si elle est gérée. Utile pour déboguer. Recommandé : selon besoin.
- **Ignorer les points d'arrêt invalides** : évite les messages d'avertissement pour les points d'arrêt qui ne peuvent pas être définis.

**Colonnes de la fenêtre de débogage** : personnalisez quelles informations afficher dans les fenêtres de surveillance.

## Sauvegarde automatique et récupération

### Sauvegarde automatique

Pour éviter de perdre votre travail en cas de crash (rare, mais possible), activez la sauvegarde automatique :

1. **Tools > Options > Editor > Editor** (Outils > Options > Éditeur > Éditeur)
2. Cherchez **Auto save options** (Options de sauvegarde automatique)
3. **Activez** l'option (Editor files / Project desktop)
4. Configurez l'intervalle (recommandé : 5 minutes)

Delphi sauvegardera alors vos fichiers modifiés à intervalles réguliers.

### Fichiers de récupération

En cas de crash de Delphi, au prochain démarrage, l'IDE vous proposera de récupérer les fichiers sur lesquels vous travailliez. Ces fichiers sont stockés dans un dossier spécial (__recovery).

Ne désactivez pas cette fonctionnalité : elle peut vous sauver de la perte de travail.

### Fichiers de sauvegarde

Delphi crée des fichiers de sauvegarde avec l'extension .~pas, .~dfm, etc. Ce sont des copies de vos fichiers avant la dernière sauvegarde.

Vous pouvez configurer si vous voulez créer ces fichiers et combien en garder :

1. **Tools > Options > Environment Options** (Outils > Options > Options d'environnement)
2. Cherchez les options de **sauvegarde de fichier**

Recommandé : garder au moins 1 fichier de sauvegarde, pour pouvoir revenir en arrière en cas d'erreur.

## Gestion des chemins et répertoires

### Chemins de recherche

Delphi doit savoir où trouver les fichiers sources, les bibliothèques, etc.

Pour configurer les chemins :

1. **Tools > Options > Language > Delphi > Library** (Outils > Options > Langage > Delphi > Bibliothèque) — chemins globaux
2. Ou pour un projet spécifique : **Project > Options > Building > Delphi Compiler > Directories and Conditionals** (Projet > Options > Compilation > Compilateur Delphi > Répertoires et conditions)

Les chemins importants :

**Chemin de recherche** : où Delphi cherche les fichiers .pas quand vous faites un uses.

**Chemin de sortie** : où Delphi place les fichiers compilés (.dcu, .exe).

**Chemin des packages** : où se trouvent les packages installés.

En général, vous n'avez pas besoin de modifier ces chemins, sauf si vous utilisez des bibliothèques tierces qui ne sont pas dans les emplacements standard.

## Exporter et importer les paramètres

Une fois que vous avez configuré Delphi à votre goût, vous voudrez peut-être sauvegarder ces paramètres ou les transférer sur un autre ordinateur.

### Sauvegarder vos paramètres

Malheureusement, Delphi ne propose pas de fonction d'export intégrée simple pour tous les paramètres. Mais vous pouvez :

**Copier le dossier de configuration** : les paramètres de Delphi sont stockés dans :
```
C:\Users\[VotreNom]\AppData\Roaming\Embarcadero\BDS\[Version]
```

Pour Delphi 13 Florence, la version est **24.0**, donc le chemin complet est :
```
C:\Users\[VotreNom]\AppData\Roaming\Embarcadero\BDS\24.0
```

Sauvegardez ce dossier pour conserver vos paramètres.

**Dispositions de bureau** : les dispositions sont sauvegardées automatiquement dans ce même dossier.

**Raccourcis clavier** : aussi sauvegardés dans ce dossier.

### Transférer sur un autre ordinateur

Copiez le dossier de configuration d'un ordinateur à l'autre. Assurez-vous d'avoir la même version de Delphi sur les deux machines.

## Conseils et bonnes pratiques

### Commencez simple

Ne modifiez pas tout d'un coup ! Commencez avec les paramètres par défaut, et ne personnalisez que ce qui vous gêne vraiment. Au fil du temps, vous découvrirez naturellement les options qui amélioreront votre flux de travail.

### Documentez vos modifications

Si vous modifiez des paramètres importants (chemins, options de compilation spéciales, etc.), notez-les quelque part. Cela facilitera la configuration d'une nouvelle installation.

### Testez avant de valider

Certaines modifications (comme les raccourcis clavier) peuvent perturber vos habitudes. N'hésitez pas à revenir en arrière si quelque chose ne fonctionne pas comme prévu.

### Ergonomie avant esthétique

Un IDE joli mais inconfortable est contre-productif. Privilégiez votre confort visuel et la facilité d'utilisation par rapport à l'aspect purement esthétique.

### Sauvegardez votre configuration

Une fois que vous avez la configuration parfaite, sauvegardez le dossier de configuration. Cela vous permettra de restaurer rapidement en cas de problème ou de réinstallation.

### Inspirez-vous des autres

Regardez des vidéos de développeurs expérimentés utilisant Delphi. Vous découvrirez souvent de nouvelles façons d'organiser l'IDE ou des options que vous ne connaissiez pas.

### Adaptez selon le contexte

Utilisez différentes dispositions de bureau selon ce que vous faites :
- Conception d'interface : Inspecteur d'objets et Palette bien visibles
- Écriture de code : éditeur maximisé
- Débogage : fenêtres de débogage affichées

## Paramètres recommandés pour débuter

Voici une configuration recommandée pour bien démarrer avec Delphi :

**Apparence** :
- Thème : selon préférence (testez Dark et Light)
- Police de code : Consolas, taille 10-11

**Éditeur** :
- Numéros de ligne : activés
- Indentation : 2 espaces
- Sauvegarde automatique : activée, toutes les 5 minutes
- Mise en évidence des blocs : activée
- Code Insight : activé

**Concepteur** :
- Aligner sur la grille : activé
- Guides d'alignement : activés
- Espacement de grille : 8 pixels

**Général** :
- Une disposition par défaut confortable
- Barres d'outils : Standard, Debug, View visible
- Raccourcis : par défaut (Delphi Classic)

## Erreurs courantes à éviter

**Modifier trop de choses** : trop de modifications peuvent rendre l'IDE instable ou difficile à utiliser. Allez-y progressivement.

**Désactiver la sauvegarde automatique** : c'est une protection importante. Gardez-la activée.

**Masquer des fenêtres importantes** : si vous masquez accidentellement l'Inspecteur d'objets ou la Palette, réaffichez-les via le menu **View** (Affichage).

**Oublier de sauvegarder sa disposition** : si vous organisez vos fenêtres mais ne sauvegardez pas la disposition, vous perdrez tout si Delphi redémarre.

**Modifier les chemins sans savoir** : ne touchez pas aux chemins de répertoires si vous ne savez pas exactement ce que vous faites. Cela peut casser la compilation.

## Réinitialiser les paramètres

Si vous avez trop modifié et que vous voulez revenir aux paramètres par défaut :

**Fermer Delphi complètement**

**Supprimer (ou renommer) le dossier de configuration** :
```
C:\Users\[VotreNom]\AppData\Roaming\Embarcadero\BDS\24.0
```
(pour Delphi 13 Florence)

> 💡 **Astuce sans suppression :** plutôt que de supprimer, **renommez** le dossier en `24.0.backup`. Au prochain lancement, Delphi recréera un dossier `24.0` propre. Si vous voulez revenir à votre configuration précédente, supprimez le nouveau et renommez `24.0.backup` → `24.0`.

**Relancer Delphi** : il recréera le dossier avec les paramètres par défaut

**Attention** : cela supprime TOUTES vos personnalisations, y compris vos dispositions personnalisées, vos raccourcis, etc. Faites une sauvegarde avant si vous voulez pouvoir revenir en arrière.

## Conclusion

Personnaliser l'IDE Delphi vous permet de créer un environnement de travail qui vous correspond parfaitement. Un IDE bien configuré améliore votre productivité, réduit la fatigue, et rend le développement plus agréable.

Les points essentiels à retenir :

- Commencez avec les **paramètres par défaut** et personnalisez progressivement
- Le **thème** et la **police de code** ont un impact majeur sur le confort visuel
- Les **dispositions de bureau** vous permettent d'adapter l'interface selon vos tâches
- Les **raccourcis clavier** sont essentiels pour la productivité
- **Sauvegardez** votre configuration quand vous êtes satisfait

N'hésitez pas à expérimenter ! Toute modification peut être annulée, et dans le pire des cas, vous pouvez toujours réinitialiser aux paramètres par défaut.

L'important est de trouver une configuration qui vous permette de travailler confortablement et efficacement. Chaque développeur a ses préférences, et c'est tout à fait normal.

Dans la prochaine section, nous explorerons la structure d'un projet Delphi en détail, pour comprendre comment tous les fichiers s'organisent et interagissent !

⏭️ [Structure d'un projet Delphi](/02-decouverte-de-lide-delphi/07-structure-dun-projet-delphi.md)
