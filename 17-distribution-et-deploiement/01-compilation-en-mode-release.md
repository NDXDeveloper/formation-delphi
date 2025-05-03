# 17.1 Compilation en mode release

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Lorsque vous développez une application Delphi, vous travaillez généralement en mode "Debug". Ce mode est idéal pendant le développement car il inclut des informations supplémentaires qui facilitent le débogage. Cependant, une fois votre application prête à être distribuée, vous devez la compiler en mode "Release". Cette section explique pourquoi et comment procéder.

## Différences entre les modes Debug et Release

Avant d'apprendre à compiler en mode Release, comprenons les différences principales entre les deux modes :

| Mode Debug | Mode Release |
|------------|--------------|
| Inclut des informations de débogage | Supprime les informations de débogage |
| Performances réduites | Performances optimisées |
| Taille de fichier plus grande | Taille de fichier réduite |
| Vérifications d'erreurs supplémentaires | Minimise les vérifications pour la performance |
| Idéal pour le développement | Idéal pour la distribution |

## Comment compiler en mode Release

### Étape 1 : Changer la configuration du projet

1. Dans l'IDE Delphi, allez dans le menu **Project** → **Options du projet** (ou appuyez sur `Shift+Ctrl+F11`)
2. Dans la boîte de dialogue qui s'ouvre, sélectionnez **Build Configuration** dans le panneau de gauche

![Configuration du projet](https://placeholder-image.com/delphi-project-config.png)

3. Dans la partie droite, vous verrez une liste déroulante nommée **Configuration**. Sélectionnez **Release** au lieu de **Debug**

### Étape 2 : Vérifier les paramètres de compilation

Toujours dans la boîte de dialogue des options du projet :

1. Sélectionnez **Delphi Compiler** dans le panneau de gauche
2. Vérifiez les options suivantes pour le mode Release :
   - Décochez **Debug information** (Informations de débogage)
   - Décochez **Use debug DCUs** (Utiliser les DCUs de débogage)
   - Assurez-vous que **Optimization** (Optimisation) est cochée

### Étape 3 : Compiler le projet

Une fois les paramètres configurés :

1. Cliquez sur **OK** pour fermer la boîte de dialogue des options
2. Pour compiler votre projet, allez dans le menu **Project** → **Compiler le projet** (ou appuyez sur `Ctrl+F9`)
3. Si vous souhaitez construire l'exécutable final, allez dans **Project** → **Construire le projet** (ou appuyez sur `Shift+F9`)

## Options d'optimisation avancées

Pour aller plus loin dans l'optimisation de votre application en mode Release, vous pouvez également ajuster ces paramètres :

### Optimisations du compilateur

Toujours dans **Project** → **Options du projet** → **Delphi Compiler** :

1. Sous l'onglet **Optimizations** :
   - Activez **Complete boolean evaluation** pour privilégier la vitesse
   - Activez **Pentium-safe FDIV** seulement si nécessaire pour des applications mathématiques précises

### Gestion des exceptions

Dans l'onglet **Compiling** :

- Vous pouvez décocher **Stack frames** pour gagner en performance, mais cela peut rendre le débogage des erreurs plus difficile en production
- L'option **Assertions** peut être désactivée en mode Release pour gagner en performance

## Différences visibles dans l'IDE

Quand vous êtes en mode Release, vous pouvez le constater facilement :

1. Dans la barre d'outils principale, le sélecteur de configuration affiche "Release"
2. Le titre de la fenêtre principale de l'IDE indique également "[Release]" à côté du nom du projet

![Mode Release dans l'IDE](https://placeholder-image.com/delphi-release-mode.png)

## Conseils pour le mode Release

- **Testez toujours votre application en mode Release** avant de la distribuer. Certains bugs peuvent apparaître uniquement en mode Release.
- **Conservez toujours une compilation Debug** pour faciliter le débogage si des problèmes sont signalés.
- **Ne distribuez jamais une version Debug** à vos clients finaux.
- **Vérifiez la taille de l'exécutable** : une réduction significative par rapport au mode Debug est normale.

## Conclusion

La compilation en mode Release est une étape essentielle avant de distribuer votre application Delphi. Elle permet d'optimiser les performances, de réduire la taille du fichier exécutable et de protéger votre code source. N'oubliez pas de tester rigoureusement votre application en mode Release avant la distribution finale.

## Exercice pratique

1. Ouvrez un de vos projets Delphi existants
2. Compilez-le en mode Debug et notez la taille du fichier exécutable
3. Suivez les étapes de ce tutoriel pour passer en mode Release
4. Recompilez le projet et comparez la taille du nouvel exécutable
5. Exécutez les deux versions et essayez de percevoir les différences de performance

Dans la prochaine section, nous verrons comment optimiser davantage le code final pour obtenir les meilleures performances possibles.

⏭️ [Optimisation du code final](17-distribution-et-deploiement/02-optimisation-du-code-final.md)
