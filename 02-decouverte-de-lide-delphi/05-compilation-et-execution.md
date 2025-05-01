# 2.5 Compilation et exécution

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Après avoir conçu l'interface utilisateur et écrit le code de votre application, l'étape suivante consiste à compiler et exécuter votre projet. Dans cette section, nous allons explorer les différentes façons de transformer votre code source en une application fonctionnelle, ainsi que les options de compilation disponibles dans Delphi.

## Concepts fondamentaux

Avant de nous plonger dans les détails, clarifions quelques concepts de base :

### Qu'est-ce que la compilation ?

La **compilation** est le processus qui transforme votre code source Object Pascal en code machine que l'ordinateur peut exécuter. Le compilateur Delphi analyse votre code, vérifie sa syntaxe et le convertit en un fichier exécutable (.exe) ou une bibliothèque (.dll).

### Différence entre compilation et exécution

- **Compilation** : Transforme le code source en programme exécutable (sans l'exécuter)
- **Exécution** : Lance le programme compilé pour le tester

Dans Delphi, vous pouvez effectuer ces opérations séparément ou les combiner en une seule étape.

## Méthodes de compilation et d'exécution

### Exécution rapide (Compilation + Exécution)

La méthode la plus courante pendant le développement est la compilation suivie immédiatement de l'exécution :

1. Appuyez sur **F9** ou cliquez sur le bouton ![Run](https://placeholder.com/run-icon) (triangle vert) dans la barre d'outils
2. Delphi compilera automatiquement votre projet et lancera l'application si aucune erreur n'est détectée

> **Astuce pour débutants :** Utilisez F9 pendant le développement pour tester rapidement vos modifications.

### Compilation sans exécution

Si vous souhaitez uniquement compiler votre projet sans l'exécuter :

1. Appuyez sur **Ctrl+F9** ou allez dans le menu **Projet > Compiler**
2. Delphi compilera votre projet et signalera toute erreur ou avertissement

Cette option est utile lorsque vous souhaitez vérifier que votre code ne contient pas d'erreurs sans exécuter l'application.

### Compilation du projet entier

Pour compiler tous les fichiers de votre projet, même ceux qui n'ont pas été modifiés :

1. Allez dans le menu **Projet > Compiler tout**
2. Delphi recompilera tous les fichiers de votre projet

Cette option est utile après avoir modifié des fichiers d'en-tête ou des fichiers de ressources qui pourraient affecter plusieurs unités.

### Création d'un exécutable final (Build)

Pour préparer votre application pour la distribution :

1. Allez dans le menu **Projet > Build** ou appuyez sur **Shift+F9**
2. Delphi recompilera tous les fichiers et créera un exécutable optimisé

L'opération "Build" est similaire à "Compiler tout", mais elle produit généralement un exécutable plus optimisé, destiné à la distribution finale.

## La fenêtre des messages de compilation

Lorsque vous compilez votre projet, Delphi affiche les résultats dans la **fenêtre Messages** généralement située en bas de l'IDE :

![Fenêtre Messages](https://placeholder.com/delphi-messages-window)

Cette fenêtre affiche :

- **Erreurs** : Problèmes qui empêchent la compilation (en rouge)
- **Avertissements** : Problèmes potentiels qui n'empêchent pas la compilation (en jaune)
- **Informations** : Messages informatifs sur le processus de compilation

### Navigation dans les erreurs

Double-cliquez sur une erreur ou un avertissement dans la fenêtre Messages pour aller directement à la ligne de code concernée.

> **Conseil pratique :** Résolvez toujours les erreurs dans l'ordre où elles apparaissent. Parfois, la correction de la première erreur résout automatiquement plusieurs erreurs suivantes.

## Options de compilation

Delphi offre de nombreuses options pour configurer le processus de compilation selon vos besoins.

### Configurations de compilation (Build Configurations)

Delphi propose des configurations prédéfinies accessibles depuis la barre d'outils :

- **Debug** : Inclut des informations de débogage, optimisation minimale (pour le développement)
- **Release** : Optimisé pour la performance, sans informations de débogage (pour la distribution)

Pour changer de configuration :

1. Utilisez la liste déroulante dans la barre d'outils
2. Ou allez dans **Projet > Options > Compilateur**

![Configurations de compilation](https://placeholder.com/delphi-build-configurations)

### Options de projet détaillées

Pour accéder aux options détaillées de compilation :

1. Allez dans le menu **Projet > Options** ou appuyez sur **Shift+Ctrl+F11**
2. Sélectionnez la catégorie **Compilateur**

Les options les plus importantes pour les débutants sont :

#### Onglet "Compilateur"

- **Optimisation** : Améliore les performances mais peut rendre le débogage plus difficile
- **Déboggage** : Inclut des informations pour faciliter le débogage
- **Vérifications** : Active diverses vérifications pendant l'exécution (dépassement de tableau, débordement d'entier, etc.)

#### Onglet "Répertoires/Conditionnels"

- **Répertoires de sortie** : Où les fichiers compilés seront placés
- **Répertoires d'unités** : Où Delphi cherchera les unités référencées
- **Symboles conditionnels** : Définit des symboles pour la compilation conditionnelle

#### Onglet "Messages"

- **Avertissements** : Configure quels types d'avertissements seront affichés

> **Pour les débutants :** Au début, vous pouvez conserver les paramètres par défaut et explorer ces options au fur et à mesure que vous progressez.

## Résultats de la compilation

### Où trouver les fichiers générés

Par défaut, Delphi place les fichiers compilés dans un sous-dossier du répertoire de votre projet :

- **Debug** : Dans le dossier `Debug\Win32` (ou `Debug\Win64` pour les compilations 64 bits)
- **Release** : Dans le dossier `Release\Win32` (ou `Release\Win64` pour les compilations 64 bits)

Les principaux fichiers générés sont :

- **[NomProjet].exe** : L'exécutable principal
- **[NomProjet].drc** : Fichier de ressources compilées
- Divers fichiers temporaires (.dcu, .obj, etc.)

## Exécution de votre application

### Modes d'exécution

Delphi offre plusieurs façons d'exécuter votre application :

#### Exécution normale

- Appuyez sur **F9** ou cliquez sur le bouton Run
- L'application s'exécute normalement, avec le débogueur attaché

#### Exécution sans débogage

- Appuyez sur **Ctrl+Shift+F9** ou allez dans **Exécuter > Exécuter sans débogage**
- L'application s'exécute sans le débogueur, ce qui peut être plus rapide

#### Exécution jusqu'au curseur

- Placez le curseur à un endroit spécifique de votre code
- Appuyez sur **F4** ou utilisez **Exécuter > Exécuter jusqu'au curseur**
- L'application s'exécute jusqu'à ce que la ligne où se trouve le curseur soit atteinte

### Arrêt de l'exécution

Pour arrêter une application en cours d'exécution :

- Appuyez sur **Alt+F2** ou cliquez sur le bouton ![Stop](https://placeholder.com/stop-icon) (carré rouge) dans la barre d'outils
- Ou fermez simplement la fenêtre de l'application

## Compilation conditionnelle

La compilation conditionnelle vous permet d'inclure ou d'exclure certaines parties de code selon des conditions définies :

```pascal
{$IFDEF DEBUG}
  ShowMessage('Mode débogage activé');
{$ELSE}
  // Code pour la version release
{$ENDIF}
```

Les directives de compilation courantes sont :

- `{$IFDEF symbole}` : Inclut le code si le symbole est défini
- `{$IFNDEF symbole}` : Inclut le code si le symbole n'est pas défini
- `{$ENDIF}` : Termine un bloc conditionnel
- `{$DEFINE symbole}` : Définit un symbole

Les symboles prédéfinis utiles incluent :

- `DEBUG` : Défini en configuration Debug
- `RELEASE` : Défini en configuration Release
- `VER350` : Défini pour Delphi 12 Athens

> **Note :** Delphi 12 utilise VER350, Delphi 11 Alexandria utilise VER340.

## Problèmes courants et solutions

### Erreurs de compilation fréquentes

| Erreur | Cause probable | Solution |
|--------|----------------|----------|
| "Undeclared identifier" | Variable ou fonction non déclarée | Vérifiez l'orthographe ou déclarez-la |
| "Unit not found" | Unité manquante dans les uses | Ajoutez l'unité aux clauses uses |
| "Method not found" | Méthode appelée mais non implémentée | Implémentez la méthode manquante |
| "Incompatible types" | Types de données incompatibles | Utilisez une conversion explicite ou corrigez le type |

### Que faire si l'application ne s'exécute pas

Si votre application a été compilée avec succès mais ne s'exécute pas correctement :

1. Vérifiez la fenêtre Messages pour les avertissements
2. Utilisez le débogueur (couvert dans une section ultérieure) pour identifier le problème
3. Vérifiez les exceptions non gérées (particulièrement pour les accès à des fichiers ou des ressources)

## Exercice pratique

Pour vous familiariser avec la compilation et l'exécution :

1. Créez un projet simple avec un bouton
2. Ajoutez un gestionnaire d'événement OnClick avec ce code :
   ```pascal
   {$IFDEF DEBUG}
     ShowMessage('Exécution en mode DEBUG');
   {$ELSE}
     ShowMessage('Exécution en mode RELEASE');
   {$ENDIF}
   ```
3. Compilez et exécutez en configuration Debug (F9)
4. Changez pour la configuration Release et exécutez à nouveau
5. Observez la différence dans le message affiché

## Conclusion

La compilation et l'exécution sont des aspects fondamentaux du développement avec Delphi. En comprenant ces processus et en maîtrisant les différentes options disponibles, vous pourrez développer plus efficacement et créer des applications optimisées.

Avec ces connaissances, vous êtes maintenant prêt à explorer des fonctionnalités plus avancées de Delphi. Dans la prochaine section, nous verrons comment personnaliser l'IDE pour l'adapter à vos préférences et à votre flux de travail.

⏭️ [Personnalisation de l'IDE](/02-decouverte-de-lide-delphi/06-personnalisation-de-lide.md)
