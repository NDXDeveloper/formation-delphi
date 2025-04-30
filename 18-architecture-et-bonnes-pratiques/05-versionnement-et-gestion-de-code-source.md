# 18.5 Versionnement et gestion de code source

## Introduction

Imaginez que vous travaillez sur votre application Delphi depuis plusieurs semaines. Un jour, vous décidez d'ajouter une nouvelle fonctionnalité, mais après plusieurs heures de développement, vous réalisez que votre code ne fonctionne plus correctement. Vous aimeriez revenir à la version précédente, mais comment faire ?

C'est là qu'intervient le **versionnement de code source**. Il s'agit d'un système qui vous permet de suivre et de gérer les modifications apportées à votre code au fil du temps. Dans ce chapitre, nous allons explorer les principes fondamentaux du versionnement et comment l'intégrer efficacement dans vos projets Delphi.

## Pourquoi utiliser un système de gestion de versions ?

Le versionnement de code source offre de nombreux avantages :

1. **Historique des modifications** : Vous pouvez voir qui a modifié quoi, quand et pourquoi.
2. **Retour arrière** : Vous pouvez revenir à une version antérieure en cas de problème.
3. **Travail collaboratif** : Plusieurs développeurs peuvent travailler sur le même projet sans conflits majeurs.
4. **Branches parallèles** : Vous pouvez développer de nouvelles fonctionnalités sans affecter le code principal.
5. **Sauvegarde** : Votre code est sauvegardé sur un serveur distant, réduisant le risque de perte de données.
6. **Documentation** : Les messages de commit fournissent une documentation implicite des changements.

## Les systèmes de gestion de versions populaires

Plusieurs systèmes de gestion de versions existent, mais les plus utilisés aujourd'hui sont :

### Git

Git est devenu le standard de l'industrie pour la gestion de versions. Créé par Linus Torvalds (le créateur de Linux), Git est :
- Distribué (chaque développeur a une copie complète du dépôt)
- Rapide et efficace
- Excellent pour le travail hors ligne
- Supporté par de nombreuses plateformes comme GitHub, GitLab, et Bitbucket

### Subversion (SVN)

Bien que moins populaire aujourd'hui, SVN reste utilisé dans certaines entreprises :
- Système centralisé (un serveur central contient toutes les versions)
- Plus simple à apprendre que Git pour les débutants
- Bonne gestion des fichiers binaires

### Mercurial

Similaire à Git en termes de fonctionnalités, Mercurial est :
- Distribué comme Git
- Réputé pour sa simplicité et sa courbe d'apprentissage plus douce
- Moins répandu que Git dans l'écosystème de développement

## Git pour les projets Delphi

Pour la suite de ce chapitre, nous nous concentrerons sur Git, car c'est le système le plus utilisé aujourd'hui. Voyons comment l'utiliser efficacement avec Delphi.

### Installation de Git

1. Téléchargez Git depuis [git-scm.com](https://git-scm.com/)
2. Installez-le en suivant les instructions (les options par défaut conviennent généralement)
3. Après l'installation, ouvrez une invite de commande et vérifiez que Git est correctement installé :

```
git --version
```

### Configuration initiale de Git

Avant d'utiliser Git, vous devez configurer votre identité :

```
git config --global user.name "Votre Nom"
git config --global user.email "votre.email@exemple.com"
```

### Initialisation d'un dépôt Git pour un projet Delphi existant

Si vous avez déjà un projet Delphi et souhaitez commencer à utiliser Git :

1. Ouvrez une invite de commande dans le dossier de votre projet
2. Initialisez un nouveau dépôt Git :

```
git init
```

3. Ajoutez vos fichiers au suivi de Git :

```
git add .
```

4. Créez votre premier commit (sauvegarde de l'état actuel) :

```
git commit -m "Version initiale du projet"
```

### Création d'un fichier .gitignore pour Delphi

Git permet d'ignorer certains fichiers que vous ne souhaitez pas versionner (fichiers temporaires, binaires compilés, etc.). Pour Delphi, voici un exemple de fichier `.gitignore` à placer à la racine de votre projet :

```
# Fichiers de compilation Delphi
*.dcu
*.exe
*.dll
*.bpl
*.bpi
*.dcp
*.so
*.apk
*.drc
*.map
*.dres
*.rsm
*.tds
*.dcu
*.lib
*.a
*.o
*.ocx

# Dossiers de compilation
__history/
__recovery/
Win32/
Win64/
Android/
iOSDevice/
iOSSimulator/
OSX32/
Linux64/

# Fichiers de sauvegarde locaux
*.~*
*.local
*.identcache
*.projdata
*.tvsconfig
*.dsk

# Fichiers de configuration locale de l'IDE
*.stat

# Dossiers de bibliothèques (à adapter selon votre organisation)
/lib/

# Fichiers de base de données locaux
*.sdb
*.sqlite
```

Après avoir créé ce fichier, ajoutez-le à Git :

```
git add .gitignore
git commit -m "Ajout du fichier .gitignore pour Delphi"
```

### Opérations Git de base

#### Vérifier l'état actuel

Pour voir quels fichiers ont été modifiés :

```
git status
```

#### Voir les modifications

Pour voir le détail des modifications :

```
git diff
```

#### Ajouter des modifications

Pour ajouter un fichier modifié au prochain commit :

```
git add nom_du_fichier.pas
```

Pour ajouter tous les fichiers modifiés :

```
git add .
```

#### Créer un commit

Pour sauvegarder les modifications ajoutées :

```
git commit -m "Description concise des modifications"
```

#### Voir l'historique

Pour voir l'historique des commits :

```
git log
```

Pour un historique plus compact :

```
git log --oneline
```

### Travailler avec les branches

Les branches permettent de développer des fonctionnalités en parallèle sans affecter le code principal.

#### Créer une nouvelle branche

```
git branch nouvelle-fonctionnalite
```

#### Basculer vers une branche

```
git checkout nouvelle-fonctionnalite
```

Ou en une seule commande :

```
git checkout -b nouvelle-fonctionnalite
```

#### Fusionner une branche dans la branche principale

```
git checkout main       # Retour à la branche principale
git merge nouvelle-fonctionnalite
```

### Travailler avec un dépôt distant (GitHub, GitLab, etc.)

#### Lier votre dépôt local à un dépôt distant

```
git remote add origin https://github.com/votre-nom/votre-projet.git
```

#### Envoyer vos modifications vers le dépôt distant

```
git push -u origin main
```

#### Récupérer les modifications depuis le dépôt distant

```
git pull origin main
```

## Intégration de Git dans l'IDE Delphi

Delphi ne dispose pas d'une intégration Git native complète, mais vous pouvez utiliser des outils tiers :

### TortoiseGit

[TortoiseGit](https://tortoisegit.org/) est une extension Windows Explorer qui facilite l'utilisation de Git :
- Interface graphique intuitive
- Intégration dans le menu contextuel de l'explorateur Windows
- Visualisation des différences, historique, branches, etc.

### SourceTree

[SourceTree](https://www.sourcetreeapp.com/) d'Atlassian est un client Git gratuit avec une interface conviviale :
- Visualisation graphique de l'historique
- Gestion des branches et des fusions
- Support de GitHub, GitLab, BitBucket, etc.

### GitHub Desktop

[GitHub Desktop](https://desktop.github.com/) est un client Git simplifié développé par GitHub :
- Interface très simple pour les opérations de base
- Intégration directe avec GitHub
- Idéal pour les débutants

### Extensions Delphi

Quelques extensions pour améliorer l'intégration de Git dans Delphi :
- [DelphiGit](https://github.com/RRUZ/delphi-git) - Extension pour intégrer Git dans Delphi
- [Delphi IDE Explorer](https://github.com/RRUZ/delphi-ide-explorer) - Améliore l'expérience de développement avec Git

## Bonnes pratiques pour le versionnement de projets Delphi

### 1. Organisez votre structure de projet

Une bonne organisation facilite le versionnement :

```
MonProjet/
  ├── src/                 # Code source
  │   ├── forms/           # Fichiers de formulaires (.pas, .dfm)
  │   ├── units/           # Unités de code (.pas)
  │   ├── resources/       # Ressources (.rc, .res)
  │   └── packages/        # Packages (.dpk)
  ├── lib/                 # Bibliothèques externes
  ├── docs/                # Documentation
  ├── tests/               # Tests unitaires
  ├── bin/                 # Exécutables compilés (généralement ignorés par Git)
  ├── assets/              # Images, sons, etc.
  ├── scripts/             # Scripts d'automatisation
  ├── .gitignore           # Fichiers à ignorer par Git
  ├── README.md            # Documentation principale
  └── MonProjet.groupproj  # Projet Delphi principal
```

### 2. Créez des commits significatifs

Un bon commit :
- Est focalisé sur une modification ou une fonctionnalité spécifique
- A un message clair et descriptif
- Ne mélange pas différentes préoccupations

Format recommandé pour les messages de commit :

```
Type: Brève description (50 caractères max)

Description détaillée des modifications si nécessaire.
Expliquez pourquoi la modification a été faite, pas comment
(le code montre le comment).
```

Types courants :
- `Feature:` Nouvelle fonctionnalité
- `Fix:` Correction de bug
- `Refactor:` Amélioration du code sans changer son comportement
- `Docs:` Modification de la documentation
- `Test:` Ajout ou modification de tests
- `Chore:` Mises à jour de routine, maintenance

Exemple :
```
Fix: Correction de la fuite mémoire dans TClientDataSet

Libération correcte des ressources dans le destructeur pour
éviter les fuites mémoire lors de la fermeture de l'application.

Issue: #123
```

### 3. Utilisez les branches efficacement

Stratégie de branches recommandée :
- `main` ou `master` : Code stable et fonctionnel
- `develop` : Développement en cours
- `feature/nom-fonctionnalité` : Nouvelles fonctionnalités
- `bugfix/nom-bug` : Corrections de bugs
- `release/x.y.z` : Préparation d'une nouvelle version

### 4. Gérez correctement les fichiers binaires

Les fichiers binaires posent un défi particulier pour Git :
- Ils ne peuvent pas être fusionnés comme du texte
- Ils occupent beaucoup d'espace dans l'historique

Solutions :
- Utilisez [Git LFS](https://git-lfs.github.com/) (Large File Storage) pour les gros fichiers binaires
- Ne versionnez que les fichiers binaires essentiels et stables

### 5. Automatisez vos tests avant chaque commit

Utilisez des outils comme DUnit ou DUnitX pour tester votre code avant de le commiter :

```
# Script batch simple pour exécuter les tests avant un commit
@echo off
echo Exécution des tests...
TestRunner.exe
if errorlevel 1 (
  echo Les tests ont échoué, le commit est annulé
  exit /b 1
)
echo Tests réussis, poursuite du commit
```

## Flux de travail Git pour un projet Delphi

Voici un exemple de flux de travail Git adapté aux projets Delphi :

### 1. Démarrage d'une nouvelle fonctionnalité

```
# Assurez-vous d'être à jour
git checkout develop
git pull origin develop

# Créez une branche pour la nouvelle fonctionnalité
git checkout -b feature/nouvelle-fonctionnalite

# Travaillez sur votre code...
```

### 2. Pendant le développement

```
# Vérifiez régulièrement l'état de vos modifications
git status

# Ajoutez vos modifications par petits groupes logiques
git add src/units/UneUnite.pas
git commit -m "Feature: Implémentation de la validation des données"

# Continuez à travailler...
git add src/forms/FormPrincipale.pas src/forms/FormPrincipale.dfm
git commit -m "Feature: Mise à jour de l'interface utilisateur pour la validation"
```

### 3. Finalisation de la fonctionnalité

```
# Assurez-vous que tout fonctionne
# Exécutez vos tests unitaires

# Récupérez les dernières modifications de develop
git checkout develop
git pull origin develop

# Revenez à votre branche et intégrez les modifications de develop
git checkout feature/nouvelle-fonctionnalite
git merge develop

# Résolvez les conflits s'il y en a
# Testez à nouveau

# Fusionnez votre fonctionnalité dans develop
git checkout develop
git merge feature/nouvelle-fonctionnalite

# Envoyez les modifications au dépôt distant
git push origin develop
```

### 4. Préparation d'une version

```
# Créez une branche de release
git checkout develop
git checkout -b release/1.2.0

# Effectuez les dernières corrections et ajustements
git add src/Version.inc
git commit -m "Chore: Mise à jour du numéro de version pour 1.2.0"

# Fusionnez dans main et develop
git checkout main
git merge release/1.2.0
git tag -a v1.2.0 -m "Version 1.2.0"

git checkout develop
git merge release/1.2.0

# Supprimez la branche de release
git branch -d release/1.2.0

# Poussez tout vers le dépôt distant
git push origin main --tags
git push origin develop
```

## Gestion des versions dans le code Delphi

En plus du versionnement du code source, il est important de gérer la version de votre application elle-même.

### Création d'un fichier de version

Créez un fichier `Version.inc` que vous pourrez inclure dans vos unités :

```pascal
// Version.inc
// Format: Major.Minor.Patch.Build
{$DEFINE APP_VERSION_MAJOR := 1}
{$DEFINE APP_VERSION_MINOR := 2}
{$DEFINE APP_VERSION_PATCH := 0}
{$DEFINE APP_VERSION_BUILD := 123}

// Chaîne de version complète
{$DEFINE APP_VERSION := '1.2.0.123'}

// Date de build
{$DEFINE APP_BUILD_DATE := '30/04/2025'}
```

### Utilisation dans votre code

```pascal
unit MainForm;

interface

{$I Version.inc}

// Le reste du code...

implementation

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Format('Mon Application - v%s', [APP_VERSION]);
  // ...
end;

end.
```

### Mise à jour automatique du numéro de build

Vous pouvez créer un script pré-build pour incrémenter automatiquement le numéro de build :

```pascal
// IncrementBuild.pas
// Compilez ce programme et ajoutez-le comme action pré-build

program IncrementBuild;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

var
  VersionFile: TStringList;
  Line, BuildStr: string;
  BuildNum: Integer;
  i: Integer;
begin
  try
    VersionFile := TStringList.Create;
    try
      VersionFile.LoadFromFile('Version.inc');

      for i := 0 to VersionFile.Count - 1 do
      begin
        Line := VersionFile[i];
        if Pos('APP_VERSION_BUILD', Line) > 0 then
        begin
          BuildStr := Copy(Line, Pos(':=', Line) + 2, Length(Line));
          BuildStr := Trim(BuildStr);
          BuildStr := StringReplace(BuildStr, '}', '', [rfReplaceAll]);
          BuildNum := StrToIntDef(BuildStr, 0) + 1;
          VersionFile[i] := Format('{$DEFINE APP_VERSION_BUILD := %d}', [BuildNum]);
        end;

        if Pos('APP_VERSION :=', Line) > 0 then
        begin
          VersionFile[i] := Format('{$DEFINE APP_VERSION := ''%d.%d.%d.%d''}',
            [{$APP_VERSION_MAJOR}, {$APP_VERSION_MINOR},
             {$APP_VERSION_PATCH}, BuildNum]);
        end;

        if Pos('APP_BUILD_DATE', Line) > 0 then
        begin
          VersionFile[i] := Format('{$DEFINE APP_BUILD_DATE := ''%s''}',
            [FormatDateTime('dd/mm/yyyy', Now)]);
        end;
      end;

      VersionFile.SaveToFile('Version.inc');
      Writeln('Numéro de build incrémenté avec succès.');
    finally
      VersionFile.Free;
    end;
  except
    on E: Exception do
      Writeln('Erreur: ', E.Message);
  end;
end.
```

## Outils de versionnement avancés pour Delphi

### 1. Gestion sémantique des versions

La [versionnement sémantique](https://semver.org/lang/fr/) est une convention de numérotation qui donne du sens à chaque numéro :

- **X.Y.Z** où :
  - **X** = Version majeure (changements incompatibles)
  - **Y** = Version mineure (nouvelles fonctionnalités compatibles)
  - **Z** = Correctif (corrections de bugs compatibles)

### 2. Conventionnal Commits

La convention [Conventional Commits](https://www.conventionalcommits.org/fr/) standardise les messages de commit pour automatiser la génération de changelog et la gestion des versions.

Format : `type(portée): description`

Exemple :
```
feat(auth): ajout de l'authentification par Google
fix(database): correction de la fuite mémoire dans TDataModule
```

### 3. GitFlow et GitLab Flow

Ces flux de travail standardisés offrent des modèles clairs pour la gestion des branches :

- [GitFlow](https://nvie.com/posts/a-successful-git-branching-model/) : Adapté aux cycles de release réguliers
- [GitLab Flow](https://docs.gitlab.com/ee/topics/gitlab_flow.html) : Plus simple, avec déploiement continu

## Intégration continue pour les projets Delphi

L'intégration continue (CI) permet d'automatiser les tests et la compilation à chaque commit.

### Solutions populaires :

- **Jenkins** : Serveur d'intégration continue open-source très flexible
- **GitHub Actions** : Intégré à GitHub, facile à configurer
- **GitLab CI** : Intégré à GitLab, puissant et flexible
- **Azure DevOps** : Solution Microsoft complète (CI/CD, gestion de projet, etc.)

### Exemple de GitHub Action pour un projet Delphi

Créez un fichier `.github/workflows/build.yml` :

```yaml
name: Build Delphi Project

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build:
    runs-on: windows-latest

    steps:
    - uses: actions/checkout@v2

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1
      with:
        version: '12.0'  # Pour Delphi 12 Athens
        edition: 'community'

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Configuration=Release /p:Platform=Win32

    - name: Run Tests
      run: |
        .\bin\Release\Win32\TestRunner.exe

    - name: Upload Artifact
      uses: actions/upload-artifact@v2
      with:
        name: MonApplication
        path: bin\Release\Win32\MonApplication.exe
```

## Conclusion

La gestion de versions est un aspect fondamental du développement professionnel avec Delphi. Git est aujourd'hui l'outil standard pour cette tâche, offrant puissance et flexibilité.

En suivant les bonnes pratiques et en mettant en place des flux de travail adaptés, vous améliorerez considérablement la qualité de votre code et la productivité de votre équipe. N'oubliez pas que l'adoption d'un système de gestion de versions est un investissement qui porte ses fruits sur le long terme.

Les compétences en gestion de versions sont aujourd'hui indispensables pour tout développeur professionnel, et maîtriser Git vous donnera un avantage certain dans votre carrière, que vous travailliez seul ou en équipe.

## Ressources complémentaires

- [Pro Git](https://git-scm.com/book/fr/v2) - Livre complet sur Git (gratuit)
- [Learn Git Branching](https://learngitbranching.js.org/?locale=fr_FR) - Tutorial interactif pour apprendre Git
- [Oh Shit, Git!?!](https://ohshitgit.com/fr) - Comment se sortir des situations difficiles avec Git
- [Getting Started with Git and Delphi](https://www.embarcadero.com/starthere/xe5/mobdevsetup/ios/en/creating_a_git_repository.html) - Guide Embarcadero pour Git et Delphi
