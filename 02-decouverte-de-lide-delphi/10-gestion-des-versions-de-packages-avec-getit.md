🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 2.10 Gestion des versions de packages avec GetIt

## Introduction

Dans la section précédente, nous avons découvert comment installer et utiliser des packages via GetIt Package Manager. Mais il y a un aspect crucial que nous n'avons pas encore exploré en détail : la **gestion des versions**.

Tout comme les applications que vous développez évoluent avec le temps (version 1.0, 1.1, 2.0, etc.), les packages que vous installez ont aussi des versions qui changent : nouvelles fonctionnalités, corrections de bugs, améliorations de performance. Savoir gérer ces versions est essentiel pour maintenir la stabilité de vos projets et éviter les surprises désagréables.

Dans cette section, nous allons apprendre à naviguer dans le monde des versions de packages, comprendre les numéros de version, gérer les mises à jour, et adopter les bonnes pratiques pour que vos projets restent stables et maintenables.

## Pourquoi la gestion des versions est-elle importante ?

### Les problèmes potentiels

Imaginez ce scénario : vous développez une application avec un package version 2.3. Tout fonctionne parfaitement. Six mois plus tard, vous installez une mise à jour vers la version 3.0. Soudain, votre application ne compile plus ! Certaines méthodes ont été renommées, d'autres supprimées, et le comportement de certains composants a changé.

C'est exactement le genre de problème que la gestion des versions vous aide à éviter.

**Stabilité** : vous voulez que votre application continue à fonctionner même quand les packages évoluent

**Reproductibilité** : vous devez pouvoir recompiler votre application dans 6 mois ou 2 ans avec les mêmes résultats

**Collaboration** : tous les membres de votre équipe doivent utiliser les mêmes versions pour éviter les conflits

**Débogage** : quand un bug apparaît, savoir quelle version vous utilisez aide à trouver la cause

**Documentation** : les fonctionnalités disponibles dépendent de la version

### Les avantages d'une bonne gestion

**Contrôle** : vous décidez quand mettre à jour, pas l'inverse

**Sécurité** : vous pouvez tester les nouvelles versions avant de les adopter

**Traçabilité** : vous savez toujours quelle version est utilisée dans quel projet

**Retour en arrière** : si une mise à jour pose problème, vous pouvez revenir à la version précédente

## Comprendre les numéros de version

### Format standard : Versionnage sémantique

La plupart des packages suivent une convention appelée **Versionnage Sémantique** (Semantic Versioning ou SemVer). Un numéro de version typique ressemble à ceci :

**2.3.5**

Ce nombre se décompose en trois parties :

**MAJEUR.MINEUR.PATCH** ou **MAJOR.MINOR.PATCH**

#### Version MAJEURE (Major)

Le premier chiffre (2 dans 2.3.5) représente la version **majeure**.

**Elle change quand** : il y a des modifications incompatibles avec les versions précédentes

**Exemples** :
- Suppression d'une méthode publique
- Changement du comportement d'une fonctionnalité existante
- Réorganisation complète de l'architecture

**Impact** : vos projets existants pourraient ne plus compiler ou ne plus fonctionner correctement

**Conseil** : prudence avant de mettre à jour une version majeure !

#### Version MINEURE (Minor)

Le deuxième chiffre (3 dans 2.3.5) représente la version **mineure**.

**Elle change quand** : de nouvelles fonctionnalités sont ajoutées de manière rétrocompatible

**Exemples** :
- Ajout d'une nouvelle méthode
- Ajout d'un nouveau composant
- Amélioration d'une fonctionnalité existante sans casser l'ancien comportement

**Impact** : vos projets existants devraient continuer à fonctionner normalement

**Conseil** : généralement sûr de mettre à jour, mais testez quand même

#### Version PATCH (Correctif)

Le troisième chiffre (5 dans 2.3.5) représente la version **patch** ou **correctif**.

**Elle change quand** : des bugs sont corrigés sans ajouter de nouvelles fonctionnalités

**Exemples** :
- Correction d'un bug
- Amélioration de la performance
- Correction d'une fuite mémoire
- Correction de fautes dans la documentation

**Impact** : aucun, que des améliorations

**Conseil** : mettez à jour dès que possible, c'est que du bénéfice !

### Exemples de progression de versions

**1.0.0** → **1.0.1** : correction de bugs, tout le reste identique

**1.0.1** → **1.1.0** : nouvelles fonctionnalités ajoutées, tout le reste compatible

**1.1.0** → **2.0.0** : changements majeurs, potentiellement incompatibles

**2.0.0** → **2.0.1** : corrections de bugs dans la version 2.0

**2.0.1** → **2.1.0** : nouvelles fonctionnalités dans la branche 2.x

### Versions spéciales

Vous rencontrerez parfois des versions avec des suffixes :

**1.0.0-beta** : version bêta, pas encore stable, pour les tests

**1.0.0-rc1** : Release Candidate, candidate à devenir la version finale

**1.0.0-alpha** : version alpha, très précoce, probablement instable

**1.0.0-dev** : version de développement, en cours de création

**2.0.0-preview** : aperçu de la future version 2.0

**Conseil** : n'utilisez ces versions que si vous êtes prêt à faire face à des bugs et des changements fréquents. Pour la production, restez sur des versions stables (sans suffixe).

### Versions incompatibles avec SemVer

Tous les packages ne suivent pas strictement le versionnage sémantique. Vous pourriez voir :

**Version datée** : 2024.12.15 (année.mois.jour)

**Version simple** : 1, 2, 3 (juste un numéro qui augmente)

**Version marketing** : XP, Vista, 10, 11 (comme Windows)

Dans ces cas, consultez la documentation du package pour comprendre leur système de versionnage.

## GetIt et la gestion des versions

### Comment GetIt affiche les versions

Dans l'interface GetIt, vous voyez les versions de plusieurs façons :

**Dans la liste des packages** : la version actuelle disponible est affichée

**Dans les détails du package** : numéro de version, date de sortie

**Dans l'onglet "Installed"** : la version installée sur votre machine

**Indicateur de mise à jour** : si une nouvelle version existe, GetIt vous le signale

### Installer une version spécifique

Par défaut, GetIt installe la **dernière version stable** d'un package. Mais vous pouvez parfois choisir une version spécifique :

**Pas toujours disponible** : malheureusement, GetIt ne propose pas toujours de choisir la version. Cela dépend de la façon dont le package a été publié.

**Pour les packages qui le proposent** :
1. Cliquez sur le package
2. Cherchez un menu déroulant "Version" dans les détails
3. Sélectionnez la version souhaitée
4. Installez

**Alternative** : pour les packages open source, vous pouvez installer manuellement une version spécifique en téléchargeant les sources d'une version précise depuis GitHub.

### Mettre à jour vers une nouvelle version

Quand une mise à jour est disponible, GetIt vous le signale. Pour mettre à jour :

1. **Ouvrez GetIt > onglet "Installed"**

2. **Identifiez le package à mettre à jour** : il a un indicateur de mise à jour

3. **Cliquez sur le package** pour voir les détails de la mise à jour

4. **Lisez les notes de version** (changelog) si disponibles : quoi de neuf ? Y a-t-il des changements incompatibles ?

5. **Décidez** : est-ce le bon moment pour mettre à jour ?

6. **Cliquez sur "Update"**

7. **Attendez** le téléchargement et l'installation

8. **Redémarrez Delphi**

9. **Testez** vos projets existants pour vérifier que tout fonctionne toujours

### Revenir à une version antérieure (downgrade)

Parfois, une mise à jour pose problème et vous voulez revenir à l'ancienne version. C'est plus compliqué avec GetIt :

**GetIt ne propose pas de downgrade direct** : vous ne pouvez pas simplement "désinstaller la mise à jour"

**Solution** :
1. **Désinstallez** la version actuelle via GetIt
2. **Si GetIt propose l'ancienne version** : réinstallez-la
3. **Sinon** : installation manuelle nécessaire
   - Trouvez les sources de l'ancienne version (GitHub, archive personnelle)
   - Installez manuellement le package

**Prévention** : sauvegardez les packages critiques avant de les mettre à jour, ou utilisez une machine de test.

## Compatibilité des versions avec Delphi

### Versions de Delphi

Chaque version de Delphi a son propre écosystème de packages. Un package compilé pour Delphi 12 ne fonctionnera pas nécessairement avec Delphi 13.

**Dans GetIt** : les packages affichent leurs compatibilités. Vous ne verrez que les packages compatibles avec votre version de Delphi.

**Mise à jour de Delphi** : si vous passez de Delphi 12 à Delphi 13, vous devrez réinstaller tous vos packages dans la nouvelle version. GetIt gère les deux versions séparément.

### Vérifier la compatibilité

Avant d'installer ou de mettre à jour un package, vérifiez :

**Version de Delphi** : "Compatible with Delphi 10.4 and later" signifie que votre Delphi doit être en version 10.4 minimum

**Plateformes** : Windows 32/64, macOS, iOS, Android, Linux ? Vérifiez que vos plateformes cibles sont supportées

**Dépendances** : certains packages nécessitent d'autres packages. GetIt gère généralement cela automatiquement

**Édition de Delphi** : certains packages ne sont disponibles que pour les éditions Professional, Enterprise ou Architect

### Matrice de compatibilité

Pour un projet multi-versions (supportant Delphi 11, 12 et 13 par exemple), créez une matrice de compatibilité :

| Package | Delphi 11 | Delphi 12 | Delphi 13 | Notes |
|---------|-----------|-----------|-----------|-------|
| FastReport | 6.8 | 6.9 | 6.9+ | Version 6.9+ requise pour D13 |
| Spring4D | 2.0 | 2.0 | 2.0 | Même version pour tous |
| MyCustomLib | 1.5 | 1.6 | 2.0 | Versions différentes |

Cette documentation est précieuse pour maintenir plusieurs versions en parallèle.

## Gérer les dépendances entre packages

### Qu'est-ce qu'une dépendance ?

Une dépendance se produit quand un package A nécessite un package B pour fonctionner.

**Exemple** : un package d'interface moderne pourrait dépendre d'un package de gestion d'images.

### Dépendances automatiques

GetIt gère souvent les dépendances automatiquement :

**Installation automatique** : quand vous installez un package qui a des dépendances, GetIt propose d'installer aussi les dépendances

**Vérification** : GetIt vérifie que toutes les dépendances sont satisfaites

**Avertissement** : si une dépendance manque, GetIt vous le signale

### Conflits de versions de dépendances

Le problème se corse quand deux packages dépendent de versions différentes du même package :

**Package A nécessite LibraryX version 2.x**  
**Package B nécessite LibraryX version 3.x**  

**Solutions possibles** :

1. **Mise à jour** : si Package A peut fonctionner avec LibraryX 3.x, mettez-le à jour

2. **Choix** : si les deux ne peuvent coexister, choisissez-en un seul

3. **Contact** : contactez les auteurs pour signaler l'incompatibilité

4. **Alternatives** : cherchez un autre package sans conflit

Ce genre de problème est rare mais peut arriver sur de gros projets.

## Stratégies de mise à jour

### Stratégie conservatrice : "If it ain't broke, don't fix it"

**Principe** : ne mettez à jour que si vous avez une bonne raison (bug critique, fonctionnalité nécessaire).

**Avantages** :
- Stabilité maximale
- Pas de surprises
- Moins de temps passé à tester

**Inconvénients** :
- Vous manquez les améliorations
- Accumulation de "dette technique"
- Risque de problèmes de sécurité non corrigés

**Recommandé pour** : applications en production critique, projets avec budget temps limité.

### Stratégie progressive : mises à jour régulières

**Principe** : mettez à jour régulièrement (tous les 3-6 mois), mais prudemment.

**Processus** :
1. Vérifiez les mises à jour disponibles
2. Lisez les changelogs
3. Testez sur une branche séparée ou un environnement de test
4. Si tout va bien, appliquez à la branche principale

**Avantages** :
- Vous bénéficiez des améliorations
- Les mises à jour sont moins "violentes" (petits incréments)
- Vous restez à jour

**Inconvénients** :
- Demande du temps de test
- Risque de petits problèmes occasionnels

**Recommandé pour** : la plupart des projets, bon équilibre entre stabilité et modernité.

### Stratégie agressive : toujours à jour

**Principe** : mettez à jour dès qu'une nouvelle version sort.

**Avantages** :
- Dernier cri en fonctionnalités
- Bugs corrigés immédiatement
- Vous aidez à identifier les problèmes tôt

**Inconvénients** :
- Risque élevé de casser quelque chose
- Beaucoup de temps passé à tester et adapter
- Peut être instable

**Recommandé pour** : projets expérimentaux, développeurs expérimentés, projets en phase de développement actif (pas en production).

### Quelle stratégie choisir ?

**Pour débuter** : stratégie conservatrice ou progressive. N'ajoutez pas la complexité des mises à jour fréquentes tant que vous apprenez.

**Pour la production** : stratégie progressive, avec tests rigoureux.

**Pour l'expérimentation** : stratégie agressive, pourquoi pas !

**L'important** : avoir une stratégie cohérente, pas mettre à jour au hasard.

## Documentation des versions utilisées

### Pourquoi documenter ?

Dans 6 mois, vous aurez oublié quelles versions vous utilisez. Un nouveau développeur rejoignant le projet aura besoin de savoir. Vous-même, sur un autre ordinateur, devrez recréer l'environnement.

La documentation des versions n'est pas optionnelle : elle est essentielle.

### Fichier PACKAGES.md ou DEPENDENCIES.md

Créez un fichier dans votre projet qui liste toutes les dépendances :

```markdown
# Dépendances du projet MonApplication

## Environnement
- Delphi 13 Florence Professional Edition
- Windows 10/11 64-bit

## Packages installés via GetIt

### Composants d'interface
- **FastReport VCL Community Edition**
  - Version : 6.9.12
  - Installé le : 2024-12-15
  - Usage : génération de rapports PDF
  - Notes : version Community gratuite suffisante

### Frameworks
- **Spring4D**
  - Version : 2.0.1
  - Installé le : 2024-11-20
  - Usage : conteneurs IoC, collections génériques
  - Notes : installation via GetIt, configuration par défaut

### Tests
- **DUnitX**
  - Version : dernière stable
  - Installé le : 2024-11-20
  - Usage : tests unitaires
  - Notes : framework de tests recommandé

## Packages installés manuellement

### ZXing Delphi
- Version : 3.5.0
- Source : https://github.com/Lakritz/ZXing.Delphi
- Installation : cloner le dépôt, compiler ZXing.dproj
- Usage : génération et lecture de QR codes
- Notes : version GetIt obsolète, installation manuelle recommandée

## Historique des versions

### 2024-12-15 : Mise à jour FastReport
- Ancienne version : 6.9.10
- Nouvelle version : 6.9.12
- Raison : correction bug impression PDF
- Impact : aucun, rétrocompatible

### 2024-11-20 : Installation initiale
- Tous les packages installés
```

### Fichier requirements.txt (style Python)

Pour un format plus compact, certains développeurs utilisent un style similaire à Python :

```
# Delphi Packages Requirements
# Format: PackageName==Version

FastReport-VCL-Community==6.9.12  
Spring4D==2.0.1  
DUnitX==latest  
ZXing-Delphi==3.5.0 # manual install  
```

### Dans le code source

Vous pouvez aussi documenter dans le code, dans une unité de constantes par exemple :

```pascal
unit ProjectInfo;

interface

const
  // Version de l'application
  APP_VERSION = '1.2.3';

  // Versions des packages utilisés
  FASTREPORT_VERSION = '6.9.12';
  SPRING4D_VERSION = '2.0.1';
  DUNITX_VERSION = 'latest';

  // Notes
  PACKAGE_NOTES =
    'FastReport : version Community gratuite' + #13#10 +
    'Spring4D : installation via GetIt' + #13#10 +
    'DUnitX : pour les tests unitaires';

implementation

end.
```

### Git et contrôle de version

**Committez** votre fichier de documentation des dépendances dans Git.

**Ne committez PAS** les packages eux-mêmes (trop volumineux, spécifiques à chaque machine).

**Taggez** les versions de votre application avec les versions des packages utilisés.

## Mises à jour et projets existants

### Tester avant d'appliquer

**Règle d'or** : ne mettez jamais à jour un package directement dans un projet en production sans tester.

**Processus recommandé** :

1. **Créez une branche Git** : `git checkout -b test-package-update`

2. **Mettez à jour le package** dans cette branche

3. **Compilez le projet** : des erreurs de compilation ?

4. **Testez toutes les fonctionnalités** : tout fonctionne comme avant ?

5. **Tests automatisés** : si vous en avez, lancez-les

6. **Tests manuels** : testez les parties qui utilisent le package mis à jour

7. **Si tout va bien** : mergez dans la branche principale

8. **Si problème** : revenez à la branche principale, le package n'a pas été mis à jour là

Cette approche vous protège contre les mauvaises surprises.

### Adapter le code après une mise à jour majeure

Parfois, une mise à jour majeure nécessite de modifier votre code. Voici comment procéder :

**Lisez le guide de migration** : les bons packages fournissent un guide expliquant les changements

**Compilez et notez les erreurs** : la compilation vous dira ce qui ne va plus

**Corrigez méthodiquement** : une erreur à la fois

**Utilisez la recherche** : si une méthode a été renommée, cherchez-la dans tout le projet et remplacez

**Testez au fur et à mesure** : après chaque correction, testez que ça fonctionne

**Documentez les changements** : notez ce que vous avez dû modifier, pour référence future

### Gestion de plusieurs projets

Si vous avez plusieurs projets utilisant le même package, les mises à jour deviennent plus complexes :

**Option 1 : synchroniser**
- Tous les projets utilisent la même version
- Plus simple à gérer
- Mais une mise à jour affecte tous les projets

**Option 2 : versions différentes**
- Chaque projet a ses versions
- Plus flexible
- Mais nécessite des environnements de développement séparés (machines virtuelles ou plusieurs installations de Delphi)

**Pour débuter** : option 1 est plus simple. Option 2 pour les besoins avancés.

## Packages et déploiement

### Runtime vs Design-Time

Certains packages ont deux composants :

**Design-Time Package** : utilisé uniquement dans l'IDE (composants dans la palette, éditeurs de propriétés, etc.)

**Runtime Package** : nécessaire pour exécuter l'application

**Implication** : lors du déploiement, vous devez parfois distribuer les runtime packages (.bpl) avec votre application.

### Distribuer avec ou sans packages

Vous avez deux options pour distribuer votre application :

**Option 1 : Lier statiquement** (Link with Runtime Packages = False)
- Tout le code est intégré dans votre .exe
- Fichier .exe plus gros
- Plus simple : un seul fichier à distribuer
- **Recommandé pour débuter**

**Option 2 : Utiliser des Runtime Packages** (Link with Runtime Packages = True)
- Le code des packages reste dans des .bpl séparés
- Fichier .exe plus petit
- Vous devez distribuer les .bpl avec le .exe
- Utile si plusieurs applications partagent les mêmes packages

**Configuration** : **Projet > Options > Packages > Link with Runtime Packages**

### Versions des packages et déploiement

**Attention** : si vous utilisez des Runtime Packages, vous devez distribuer la bonne version des .bpl.

**Problème potentiel** : si vous mettez à jour le package sur votre machine de développement, vous devez aussi mettre à jour les .bpl distribués avec votre application.

**Solution** : pour éviter ces complications en tant que débutant, utilisez le liage statique (Link with Runtime Packages = False).

## Outils complémentaires

### Boss Package Manager

Pour une gestion plus avancée des versions, certains développeurs utilisent **Boss** :

- Gestionnaire de dépendances en ligne de commande
- Fichier boss.json listant toutes les dépendances avec versions exactes
- Installation reproductible sur n'importe quelle machine
- Gestion automatique des versions

**Pour débuter** : GetIt suffit largement. Boss pour les projets avancés.

### Gestionnaires de versions système

Si vous installez des packages manuellement, utilisez Git pour gérer leurs sources :

```bash
# Dans votre dossier de bibliothèques Delphi
git clone https://github.com/auteur/package.git  
cd package  
git checkout v2.3.5  # Basculer vers une version spécifique  
```

Cela vous permet de changer facilement de version.

## Bonnes pratiques résumées

### Avant d'installer ou de mettre à jour

✅ **Lisez le changelog** : quoi de neuf ? Changements incompatibles ?

✅ **Vérifiez la compatibilité** : avec votre Delphi, vos plateformes

✅ **Sauvegardez** : commitez votre code actuel dans Git

✅ **Testez sur une branche** : ne mettez pas à jour directement dans master/main

✅ **Prévoyez du temps** : pour tester après la mise à jour

### Pendant l'utilisation

✅ **Documentez** : versions utilisées, date d'installation, raisons

✅ **Suivez une stratégie** : conservatrice, progressive ou agressive, mais cohérente

✅ **Vérifiez régulièrement** : les mises à jour disponibles (mais ne vous précipitez pas)

✅ **Lisez les notes de version** : avant de mettre à jour

### En équipe

✅ **Synchronisez** : toute l'équipe utilise les mêmes versions

✅ **Documentez dans le projet** : fichier PACKAGES.md partagé

✅ **Communiquez** : prévenez l'équipe avant une mise à jour

✅ **Testez ensemble** : chaque membre teste après une mise à jour

## Erreurs courantes à éviter

### Mettre à jour "pour mettre à jour"

**Erreur** : installer systématiquement chaque mise à jour sans raison

**Problème** : perte de temps, risque de casser quelque chose

**Solution** : mettez à jour uniquement si vous avez besoin d'une nouvelle fonctionnalité ou d'une correction de bug critique

### Ne jamais mettre à jour

**Erreur** : rester sur des versions obsolètes pendant des années

**Problème** : bugs non corrigés, failles de sécurité, fonctionnalités manquantes

**Solution** : stratégie progressive, mises à jour planifiées régulièrement

### Ne pas documenter

**Erreur** : ne pas noter quelles versions sont utilisées

**Problème** : impossible de reproduire l'environnement, confusion en équipe

**Solution** : fichier PACKAGES.md, mis à jour à chaque changement

### Ne pas tester après mise à jour

**Erreur** : mettre à jour et déployer immédiatement

**Problème** : bugs en production

**Solution** : tests systématiques après chaque mise à jour

### Versions différentes en développement et production

**Erreur** : développer avec une version, déployer avec une autre

**Problème** : comportement différent, bugs mystérieux

**Solution** : utilisez EXACTEMENT les mêmes versions partout

## Conclusion

La gestion des versions de packages peut sembler complexe au début, mais c'est une compétence essentielle pour tout développeur Delphi professionnel. Bien gérée, elle vous assure stabilité, reproductibilité et sérénité.

Points essentiels à retenir :

- **Comprenez le versionnage sémantique** : MAJEUR.MINEUR.PATCH
- **Documentez les versions** : fichier PACKAGES.md dans chaque projet
- **Adoptez une stratégie de mise à jour** : conservatrice ou progressive pour débuter
- **Testez avant d'appliquer** : branche Git dédiée aux tests de mise à jour
- **Synchronisez en équipe** : mêmes versions pour tous
- **Lisez les changelogs** : avant chaque mise à jour
- **Soyez prudent avec les versions majeures** : changements potentiellement incompatibles
- **Utilisez le contrôle de version** : Git pour sauvegarder avant les changements

Avec ces connaissances et ces bonnes pratiques, vous êtes maintenant équipé pour gérer efficacement les packages et leurs versions dans vos projets Delphi. La clé est la discipline : prendre le temps de bien faire les choses aujourd'hui vous évitera des problèmes demain.

Dans la prochaine section, nous découvrirons le site web companion IA et l'assistance au développement, des nouveautés de Delphi 13 qui vont révolutionner votre façon de développer !

⏭️ [Site web companion IA et assistance au développement](/02-decouverte-de-lide-delphi/11-site-web-companion-ia-et-assistance.md)
