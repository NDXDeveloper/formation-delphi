🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.1 Compilation en mode release

## Introduction

Lorsque vous développez une application avec Delphi, vous travaillez généralement en **mode Debug** (débogage). Ce mode facilite le développement en incluant des informations supplémentaires qui vous aident à corriger les erreurs. Cependant, lorsque votre application est prête à être distribuée aux utilisateurs finaux, vous devez la compiler en **mode Release** (publication).

## Comprendre les différences entre Debug et Release

### Mode Debug

Le mode Debug est optimisé pour le développement :

- **Informations de débogage** : Le compilateur inclut des informations détaillées qui permettent de suivre l'exécution du code ligne par ligne
- **Optimisations désactivées** : Le code est compilé de manière à faciliter le débogage, même si cela rend l'exécution plus lente
- **Fichier plus volumineux** : L'exécutable contient des données supplémentaires pour le débogage
- **Vérifications accrues** : Des contrôles supplémentaires sont effectués pendant l'exécution (débordements de tableau, vérifications de plage, etc.)

### Mode Release

Le mode Release est optimisé pour la production :

- **Pas d'informations de débogage** : L'exécutable ne contient que le code nécessaire à l'exécution
- **Optimisations activées** : Le compilateur applique diverses optimisations pour améliorer les performances
- **Fichier plus compact** : L'exécutable est plus petit et plus rapide
- **Vérifications réduites** : Certaines vérifications de développement sont désactivées pour gagner en performance

## Avantages de la compilation en mode Release

Compiler votre application en mode Release avant de la distribuer présente plusieurs avantages importants :

### 1. Performances optimales

Les optimisations du compilateur permettent à votre application de s'exécuter plus rapidement. Le code est restructuré pour une exécution plus efficace.

### 2. Taille réduite

L'absence d'informations de débogage réduit significativement la taille de l'exécutable, ce qui facilite sa distribution et son téléchargement.

### 3. Sécurité accrue

Sans les informations de débogage, il est plus difficile pour quelqu'un de rétro-ingénierie votre application pour comprendre son fonctionnement interne.

### 4. Professionnalisme

Une application compilée en Release donne une impression plus professionnelle et répond aux standards de distribution logicielle.

## Comment compiler en mode Release dans Delphi 13

### Méthode 1 : Via le menu de configuration

1. **Ouvrir le gestionnaire de configuration**
   - Allez dans le menu `Projet` → `Options`
   - Dans l'arborescence de gauche, sélectionnez `Compilation` sous votre plateforme cible (par exemple, Windows 32-bit ou Windows 64-bit)

2. **Sélectionner la configuration Release**
   - En haut de la fenêtre des options, vous verrez un menu déroulant `Configuration`
   - Sélectionnez `Release` dans ce menu

3. **Vérifier les paramètres**
   - Assurez-vous que les options suivantes sont configurées :
     - **Optimisation** : Activée
     - **Informations de débogage** : Désactivées
     - **Assertions** : Désactivées (optionnel)
     - **Vérifications de débordement** : Désactivées

4. **Appliquer et compiler**
   - Cliquez sur `OK` pour valider
   - Compilez votre projet avec `Projet` → `Compiler` ou en appuyant sur `Ctrl+F9`

### Méthode 2 : Via la barre d'outils

1. **Sélectionner la configuration depuis la barre d'outils**
   - Dans la barre d'outils principale, recherchez le menu déroulant de configuration (généralement à côté de la plateforme cible)
   - Sélectionnez `Release` dans la liste

2. **Compiler le projet**
   - Utilisez `Projet` → `Compiler` ou `Ctrl+F9`

## Paramètres importants de compilation Release

### Options de compilation

Dans `Projet` → `Options` → `Compilation`, voici les paramètres clés à vérifier :

#### Optimisation

- **Optimisation** : Cochez cette option pour activer les optimisations du compilateur
- **Alignement** : Laissez sur "Par défaut" ou "8 octets" pour des performances optimales

#### Informations de débogage

- **Informations de débogage** : Décochez toutes les options liées au débogage
- **Carte de débogage** : Désactivée (sauf si vous avez besoin d'analyser les crashes en production)

#### Vérifications à l'exécution

- **Vérification de débordement** : Désactivée (améliore les performances)
- **Vérification de plage** : Désactivée
- **Vérification des E/S** : Peut rester activée si votre application manipule beaucoup de fichiers

### Options de l'éditeur de liens

Dans `Projet` → `Options` → `Édition de liens` :

- **Informations de débogage détaillées** : Désactivées
- **Fichier MAP** : Désactivé (sauf besoin spécifique)
- **Numéros de ligne de débogage** : Désactivés

## Bonnes pratiques pour la compilation Release

### 1. Tester en mode Release avant la distribution

**Important** : Ne distribuez jamais une application sans l'avoir testée en mode Release. Certains bugs peuvent n'apparaître qu'en mode Release à cause des optimisations ou de l'absence de certaines vérifications.

### 2. Conserver une version Debug

Gardez toujours une copie de votre projet compilé en mode Debug. Si un utilisateur signale un bug, vous pourrez revenir au mode Debug pour le corriger plus facilement.

### 3. Nettoyer le projet avant compilation

Avant de compiler en Release, nettoyez votre projet :
- Menu `Projet` → `Nettoyer [NomDuProjet]`
- Cela supprime les fichiers temporaires et assure une compilation propre

### 4. Vérifier les chemins de sortie

Assurez-vous que vos fichiers Release sont générés dans un dossier distinct :
- Dans `Projet` → `Options` → `Répertoires et fichiers conditionnels`
- Configurez des chemins différents pour Debug et Release (par exemple : `.\Win32\Debug` et `.\Win32\Release`)

### 5. Attention aux assertions

Les assertions (`Assert()`) sont généralement désactivées en mode Release. Si votre code s'appuie sur des assertions pour fonctionner correctement, vous devez reconsiderer votre approche.

### 6. Gérer les messages de débogage

Si vous utilisez des fonctions comme `OutputDebugString()` ou `WriteLn()` pour afficher des informations de débogage, envisagez de les encadrer avec des directives de compilation conditionnelle :

```pascal
{$IFDEF DEBUG}
  OutputDebugString('Message de débogage');
{$ENDIF}
```

## Vérifier la compilation Release

Après avoir compilé en mode Release, vérifiez les points suivants :

### 1. Taille du fichier

L'exécutable Release devrait être notablement plus petit que la version Debug. Si ce n'est pas le cas, vérifiez vos paramètres de compilation.

### 2. Performance

Testez votre application et comparez les performances avec la version Debug. Vous devriez constater une amélioration, particulièrement sur les opérations intensives (calculs, traitement de données, etc.).

### 3. Fonctionnalité complète

Testez toutes les fonctionnalités de votre application pour vous assurer qu'aucune ne dépend de fonctionnalités spécifiques au mode Debug.

### 4. Gestion des erreurs

Vérifiez que la gestion des erreurs fonctionne correctement. Sans le débogueur, les exceptions non gérées peuvent faire planter l'application sans message explicite.

## Problèmes courants et solutions

### L'application plante en Release mais pas en Debug

Ce problème classique peut avoir plusieurs causes :

- **Variables non initialisées** : En mode Debug, Delphi initialise souvent automatiquement les variables. En Release, elles peuvent contenir des valeurs aléatoires
- **Optimisations trop agressives** : Essayez de désactiver temporairement certaines optimisations pour identifier le problème
- **Code dépendant du débogueur** : Vérifiez que votre code ne dépend pas de la présence du débogueur

### L'application est plus lente en Release

Bien que rare, cela peut arriver si :

- Les optimisations ne sont pas activées
- Votre code contient des boucles ou calculs qui bénéficient des vérifications Debug pour s'interrompre en cas d'erreur

### Impossible de déboguer une erreur signalée par un utilisateur

Si vous n'avez pas conservé de version Debug ou de fichiers MAP, il sera difficile de diagnostiquer les erreurs. Envisagez d'utiliser un système de journalisation (logging) dans vos applications Release pour capturer les informations importantes.

## Configuration pour différentes plateformes

Delphi 13 permet de compiler pour plusieurs plateformes. Pour chaque plateforme, vous pouvez avoir une configuration Release distincte :

### Windows 32-bit et 64-bit

- Configurez séparément les options Release pour Win32 et Win64
- Les optimisations peuvent différer légèrement entre les deux plateformes

### macOS, iOS, Android, Linux

- Chaque plateforme a ses propres paramètres Release
- Accédez-y via `Projet` → `Options`, puis sélectionnez la plateforme cible dans l'arborescence

## Conclusion

La compilation en mode Release est une étape cruciale avant la distribution de votre application. Elle garantit que vos utilisateurs bénéficieront de performances optimales et d'une expérience professionnelle.

**Points clés à retenir** :

- Toujours compiler en Release avant de distribuer
- Tester minutieusement la version Release
- Désactiver les informations de débogage et activer les optimisations
- Conserver une version Debug pour faciliter la maintenance
- Nettoyer le projet avant compilation
- Vérifier que tous les paramètres Release sont correctement configurés

En suivant ces recommandations, vous vous assurerez que votre application Delphi est prête pour une distribution professionnelle et offrira la meilleure expérience possible à vos utilisateurs.

⏭️ [Optimisation du code final](/17-distribution-et-deploiement/02-optimisation-du-code-final.md)
