🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.1 Compilation en mode release

## Introduction

Lorsque vous développez une application avec Delphi, vous travaillez généralement en **mode Debug** (débogage). Ce mode facilite le développement en incluant des informations supplémentaires qui vous aident à corriger les erreurs. Cependant, lorsque votre application est prête à être distribuée aux utilisateurs finaux, vous devez la compiler en **mode Release** (publication).

## Comprendre les différences entre Debug et Release

### Mode Debug

Le mode Debug est optimisé pour le développement :

- **Informations de débogage** : Le compilateur inclut des informations détaillées qui permettent de suivre l'exécution du code ligne par ligne.
- **Optimisations désactivées** : Le code est compilé de manière à faciliter le débogage, même si cela rend l'exécution plus lente.
- **Fichier plus volumineux** : L'exécutable contient des données supplémentaires pour le débogage.
- **Vérifications d'exécution activées par défaut** : `{$R+}` (vérification de plage d'indices), `{$Q+}` (débordement d'entier), `{$O-}` (optimisations désactivées). Ces contrôles interrompent l'application avec une exception si une erreur est détectée.

### Mode Release

Le mode Release est optimisé pour la production :

- **Pas d'informations de débogage embarquées** : L'exécutable ne contient que le code nécessaire à l'exécution (le `.map` reste un fichier séparé, à archiver).
- **Optimisations activées** : Le compilateur applique diverses optimisations pour améliorer les performances.
- **Fichier plus compact** : L'exécutable est plus petit et plus rapide.
- **Vérifications par défaut** : historiquement l'IDE désactive `{$R-}` et `{$Q-}` en Release pour gagner ~2 % de performance — mais en 2026, **la recommandation moderne est de les laisser activées** pour la sécurité. Voir la section *Vérifications à l'exécution* plus bas pour le détail.

## Avantages de la compilation en mode Release

Compiler votre application en mode Release avant de la distribuer présente plusieurs avantages importants :

### 1. Performances optimales

Les optimisations du compilateur permettent à votre application de s'exécuter plus rapidement. Le code est restructuré pour une exécution plus efficace.

### 2. Taille réduite

L'absence d'informations de débogage réduit significativement la taille de l'exécutable, ce qui facilite sa distribution et son téléchargement.

### 3. Légère gêne pour la rétro-ingénierie

Sans les informations de débogage embarquées, il faut un peu plus d'effort pour comprendre le binaire — mais **ne comptez pas dessus pour de la sécurité**. Les outils modernes (IDA Pro, Ghidra, Hopper, Binary Ninja) désassemblent un binaire Delphi sans difficulté ; les chaînes constantes restent visibles avec `strings`, et la **RTTI Delphi** expose même la liste des classes, méthodes et propriétés publiées. La compilation Release **ralentit** l'attaquant déterminé, elle ne le **bloque pas** (cf chapitre 16, README, section « Compilation native »).

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
     - **Informations de débogage embarquées dans l'EXE** : Désactivées
     - **Fichier MAP** : Activé (conservé hors-distribution, cf section *Options de l'éditeur de liens* plus bas)
     - **Assertions** : à votre discrétion (désactivées si pures aides debug)
     - **Vérifications de débordement / plage** : à laisser activées par défaut en 2026 pour la sécurité (cf note détaillée plus bas)

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

- **Informations de débogage embarquées** (directive `{$D+}` / `{$D-}` côté compilateur) : **décochez** dans l'IDE → équivaut à `{$D-}`. Pas d'embarquement dans l'EXE distribué.
- **Carte de débogage / Fichier MAP** (côté linker) : **ACTIVÉE** et conservée hors-distribution. C'est différent des info de debug embarquées : le `.map` est un fichier séparé, ne grossit pas l'EXE, et est essentiel pour décoder les crashes en production. À archiver avec chaque build (cf section 17.10).

#### Vérifications à l'exécution

> ⚠️ **Recommandation 2026** : la pratique historique « désactiver toutes les vérifications en Release pour gagner en performance » est aujourd'hui **fortement remise en question**. Les gains de performance sont marginaux sur les CPU modernes (<2 % en moyenne), alors que les conséquences peuvent être graves :  
> - Un **débordement d'entier** non détecté produit un résultat silencieusement faux (calcul de prix, indice de tableau, durée…).  
> - Un **dépassement d'indice de tableau** non détecté est une vulnérabilité de type *buffer overflow* — exactement le genre d'erreur que les attaquants exploitent.  
>  
> **Recommandation moderne** :  
> - **Vérification de débordement (`{$Q+}`)** : laisser **ACTIVÉE** en Release pour la sécurité. Désactiver uniquement les boucles intensives mesurées via `{$Q-}...{$Q+}` localement.  
> - **Vérification de plage (`{$R+}`)** : laisser **ACTIVÉE** en Release pour la sécurité (CWE-129).  
> - **Vérification des E/S (`{$I+}`)** : laisser activée (par défaut). Le coût est négligeable.  
> - **Assertions (`{$C+}` / `{$ASSERTIONS ON}`)** : à votre discrétion — les laisser activées si elles documentent des invariants critiques, les désactiver si elles servent juste de debug aid.

### Options de l'éditeur de liens

Dans `Projet` → `Options` → `Édition de liens` :

- **Informations de débogage détaillées** : Désactivées (ne pas embarquer dans l'EXE final)
- **Fichier MAP détaillé** : **ACTIVÉ et conservé hors-distribution**. Un fichier `.map` ne grossit pas l'EXE (il est séparé) et il est **indispensable** pour analyser les crashes en production (cf section 17.10 sur la télémétrie / madExcept / EurekaLog / Sentry). À convertir éventuellement en `.pdb` avec **`map2pdb`** d'Anders Melander pour utilisation avec WinDbg ou Sentry (lien dans la section 17.10).
- **Fichier RSM** (Remote Symbol Map) : optionnel, utile pour le debug à distance Linux.
- **Numéros de ligne de débogage** : Désactivés dans l'EXE distribué (mais conservés dans le fichier MAP correspondant).

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
- Dans `Projet` → `Options` → `Répertoires et fichiers conditionnels`.
- Configurez des chemins différents pour chaque combinaison plateforme/configuration. Par défaut, Delphi 13 utilise `.\$(Platform)\$(Config)`, ce qui donne :
  - `.\Win64\Release` et `.\Win64\Debug` (cible principale en 2026).
  - `.\Win32\Release` et `.\Win32\Debug` (si vous gardez une cible 32 bits).
  - `.\OSX64\Release`, `.\Linux64\Release`, `.\Android64\Release`, etc.
- Ne JAMAIS faire pointer Debug et Release vers le même dossier — les `.dcu` partagés entraînent des recompilations partielles imprévisibles.

### 5. Attention aux assertions

Les assertions (`Assert()`) sont généralement désactivées en mode Release (directive `{$C-}` ou `{$ASSERTIONS OFF}`). Si votre code s'appuie sur des assertions pour son **comportement métier** (pas juste pour des vérifications de debug), vous devez reconsidérer votre approche : une assertion est censée documenter une invariante, pas garantir une logique applicative. Mieux : utilisez `if not Condition then raise EInvariantViolated.Create(...)` pour les cas critiques.

### 6. Gérer les messages de débogage

Si vous utilisez des fonctions comme `OutputDebugString()` ou `WriteLn()` pour afficher des informations de débogage, encadrez-les avec des directives de compilation conditionnelle :

```pascal
{$IFDEF DEBUG}
  OutputDebugString('Message de débogage');
{$ENDIF}
```

> 💡 **Symbole `DEBUG` en Delphi** : par convention, la configuration *Debug* définit le symbole `DEBUG` dans `Projet` → `Options` → `Compilation` → `Définition de symboles conditionnels`. Vérifiez que ce symbole est bien présent en Debug et absent en Release. Vous pouvez aussi utiliser `{$IFOPT D+}` qui teste si les informations de débogage sont activées (équivalent symbolique sans définir de symbole personnalisé).

### 7. Builds en ligne de commande (pour CI/CD)

Pour automatiser les builds (cf section 17.9), utilisez les compilateurs de Delphi en CLI :

```batch
REM Compilation en Release Windows 64 bits via MSBuild + RAD Studio  
REM Studio\24.0 = Delphi 13 Florence (Studio\23.0 = Delphi 12 Athens).  
REM Préférer MSBuild : il honore la config Release du .dproj  
REM (optimisations, chemins de sortie, défines conditionnels, etc.).  
call "C:\Program Files (x86)\Embarcadero\Studio\24.0\bin\rsvars.bat"  
msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win64  

REM Alternative : dcc64 directement (plus rapide pour les petits projets).  
REM ⚠ dcc64 IGNORE la config du .dproj — il faut redéclarer les flags  
REM   à la main. Ici on active les optimisations (-$O+) et on définit le  
REM   symbole RELEASE. Ne PAS utiliser -CC : ce flag force une cible  
REM   console et casserait un projet VCL/FMX (utiliser -CG pour GUI si  
REM   nécessaire, mais le mieux est de laisser le .dpr décider via  
REM   {$APPTYPE ...}).  
dcc64 -B -$O+ -DRELEASE MonProjet.dpr  
```

Sous Linux/macOS, l'équivalent existe via `dcclinux64`, `dccosx64`, etc. Les CI modernes (GitHub Actions, GitLab CI) supportent les agents Windows pour exécuter ces builds.

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

- **Variables locales non initialisées** : contrairement à une idée reçue, Delphi **n'initialise PAS automatiquement les variables locales** non managées (`Integer`, `Boolean`, records simples, pointeurs). Leur valeur initiale est ce qui se trouve sur la pile à ce moment. La pile diffère entre Debug et Release, donc un bug latent peut se manifester uniquement en Release. (Les variables locales **managées** — `string`, tableaux dynamiques, interfaces — sont, elles, toujours initialisées à `''`/`nil` par le compilateur.)
- **Optimisations agressives** : la réorganisation des instructions par le compilateur peut exposer une dépendance d'ordre cachée (par ex. lire une variable avant qu'elle ne soit affectée par un autre chemin de code).
- **Vérifications désactivées** : si vous avez désactivé `{$R+}` ou `{$Q+}` en Release contre la recommandation de la section précédente, un débordement réel passe inaperçu jusqu'au crash.
- **Inlining d'effets de bord** : une fonction inlined peut être réordonnée par rapport à un effet de bord (UI, log, mutex) qui paraissait synchroniser le tout en Debug.

**Démarche de diagnostic** :
1. **Activez `{$R+}` et `{$Q+}` en Release** : transforme un débordement silencieux en exception localisable (au lieu d'un crash aléatoire plus tard).
2. **Compilez avec le fichier MAP** (cf section *Options de l'éditeur de liens*) et capturez la pile complète via madExcept ou JclDebug. Sans MAP, l'exception ne donne que des adresses brutes inexploitables.
3. **Détectez les variables locales non initialisées** via un analyseur statique : **Pascal Analyzer (Peganza)** ou **FixInsight** signalent l'usage avant affectation. `FastMM5` détecte les fuites et certains accès hors-limite, mais pas les variables non initialisées.
4. **Bisection** : si le bug n'est pas évident, désactivez les optimisations (`{$O-}`) en Release et recompilez. Si le bug disparaît, c'est un problème de réorganisation par le compilateur.

### L'application est plus lente en Release

Bien que rare, cela peut arriver si :

- Les optimisations ne sont pas activées dans la configuration Release.
- Vous avez activé d'office des outils de monitoring lourds (telemetry, logging verbeux) uniquement présents en Release.
- Un anti-virus / EDR sur la machine de test inspecte un binaire signé pour la première fois (effet « cold-cache » au premier lancement).
- Le binaire Release est dans un dossier indexé par Windows Search ou OneDrive (cas plus rare).

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
- Désactiver les informations de débogage **embarquées** dans l'EXE, mais **conserver le fichier `.map`** archivé (utile pour diagnostiquer les crashes en production)
- **Laisser activées** les vérifications de plage et de débordement par défaut (sécurité > gain de perf marginal)
- Conserver une version Debug pour faciliter la maintenance
- Nettoyer le projet avant compilation
- Vérifier que tous les paramètres Release sont correctement configurés

En suivant ces recommandations, vous vous assurerez que votre application Delphi est prête pour une distribution professionnelle et offrira la meilleure expérience possible à vos utilisateurs.

⏭️ [Optimisation du code final](/17-distribution-et-deploiement/02-optimisation-du-code-final.md)
