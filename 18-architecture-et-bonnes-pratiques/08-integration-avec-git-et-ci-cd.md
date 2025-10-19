🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 18.8 Intégration avec Git et CI/CD

## Introduction

Imaginez une usine moderne : les produits avancent sur une chaîne de montage automatisée, chaque étape est vérifiée automatiquement, et à la fin, le produit est emballé et expédié sans intervention humaine. C'est efficace, rapide et fiable.

Le développement logiciel peut fonctionner de la même façon avec **CI/CD** (Continuous Integration / Continuous Deployment). Chaque fois que vous écrivez du code et le poussez sur Git, une "chaîne de montage automatisée" se met en marche :
- Compilation automatique
- Tests automatiques
- Vérifications de qualité
- Déploiement automatique

**Le résultat :** Moins d'erreurs, livraisons plus rapides, développeurs moins stressés.

### Qu'est-ce que CI/CD ?

**CI (Continuous Integration - Intégration Continue) :**
L'intégration fréquente du code de tous les développeurs dans une branche commune, avec vérification automatique que tout fonctionne.

**CD (Continuous Deployment/Delivery - Déploiement/Livraison Continue) :**
Le déploiement automatique du code validé vers les environnements de test ou de production.

### L'évolution du développement

**Années 2000 - Développement traditionnel :**
```
Développeur code pendant 2 semaines
→ Envoie le code
→ Quelqu'un compile manuellement
→ QA teste manuellement (1 semaine)
→ 50 bugs trouvés
→ Corrections (1 semaine)
→ Re-test
→ Déploiement manuel (souvent le vendredi soir = stress)
→ Bugs en production le weekend
```

**Aujourd'hui - Développement moderne avec CI/CD :**
```
Développeur code pendant 2 heures
→ Push sur Git
→ Compilation automatique (5 minutes)
→ Tests automatiques (10 minutes)
→ Checks qualité automatiques (2 minutes)
→ Si tout OK : déploiement automatique sur environnement de test
→ Si tests OK : déploiement automatique en production
→ Total : 20 minutes du code à la production
```

### Pourquoi CI/CD pour Delphi ?

**Objection courante :** "Delphi, c'est surtout pour le desktop Windows, pas besoin de CI/CD"

**Réalité :**
- ✅ Détection immédiate des erreurs de compilation
- ✅ Tests automatiques à chaque modification
- ✅ Builds reproductibles (toujours le même résultat)
- ✅ Historique complet des versions
- ✅ Déploiement fiable
- ✅ Gain de temps considérable

**Statistiques :**
- Les équipes avec CI/CD déploient 200 fois plus fréquemment
- 60% moins de bugs en production
- 50% de temps gagné sur les tâches répétitives

## Prérequis et concepts fondamentaux

### Ce que vous devez connaître

Avant d'aborder le CI/CD, assurez-vous de maîtriser :

1. **Git** (voir section 18.5)
   - Commits, branches, push/pull
   - Pull Requests
   - Gestion des conflits

2. **Tests automatisés**
   - Tests unitaires avec DUnitX
   - Comprendre l'importance des tests

3. **Ligne de commande**
   - Naviguer dans les dossiers
   - Exécuter des commandes
   - Comprendre les scripts

### Vocabulaire CI/CD

**Pipeline :** Une série d'étapes automatisées (build → test → deploy)

**Runner/Agent :** Machine qui exécute le pipeline

**Job :** Une tâche spécifique dans le pipeline (ex: compiler)

**Artifact :** Fichier produit par le pipeline (ex: .exe compilé)

**Environment :** Environnement cible (dev, test, production)

**Trigger :** Événement qui déclenche le pipeline (push, PR, schedule)

### Architecture CI/CD typique

```
┌──────────────────┐
│  Développeur     │
│  (Local)         │
└────────┬─────────┘
         │ git push
         ↓
┌──────────────────┐
│   GitHub/GitLab  │  ← Repository Git
└────────┬─────────┘
         │ webhook
         ↓
┌──────────────────┐
│   CI/CD Server   │  ← GitHub Actions, GitLab CI, etc.
│   (Runner)       │
└────────┬─────────┘
         │
         ├─→ 1. Checkout code
         ├─→ 2. Compile with Delphi
         ├─→ 3. Run tests
         ├─→ 4. Quality checks
         ├─→ 5. Create artifacts
         └─→ 6. Deploy
                ↓
         ┌──────────────────┐
         │  Environnement   │
         │  (Test/Prod)     │
         └──────────────────┘
```

## Workflow Git pour CI/CD

### Stratégie de branching

Pour que le CI/CD fonctionne bien, adoptez une stratégie de branching claire.

#### GitHub Flow (recommandé pour débuter)

```
main (toujours déployable)
  │
  ├─→ feature/login      ← Développement de fonctionnalité
  │    │ commits...
  │    └─→ Pull Request
  │         └─→ merge dans main
  │
  ├─→ fix/bug-urgent     ← Correction de bug
  │    │ commits...
  │    └─→ Pull Request
  │         └─→ merge dans main
  │
  └─→ Deploy automatique après chaque merge
```

**Règles :**
1. `main` est toujours stable et déployable
2. Chaque fonctionnalité = une branche
3. Pull Request obligatoire pour merger
4. CI/CD s'exécute sur chaque PR
5. Merge = déploiement automatique

#### GitFlow (pour projets plus complexes)

```
main (production)
  │
  └─→ develop (intégration)
       │
       ├─→ feature/xxx    ← Fonctionnalités
       ├─→ release/v2.0   ← Préparation release
       └─→ hotfix/urgent  ← Corrections urgentes
```

**Règles :**
1. `main` = production
2. `develop` = dernière version en développement
3. Features partent de `develop`
4. Releases testées avant merge dans `main`
5. CI/CD sur toutes les branches

### Protection des branches

Sur GitHub/GitLab, configurez la protection de `main` :

**GitHub : Settings → Branches → Branch protection rules**

```
✅ Require pull request reviews before merging
✅ Require status checks to pass before merging
   ✅ build
   ✅ test
   ✅ lint
✅ Require branches to be up to date before merging
✅ Include administrators
```

**Résultat :** Impossible de merger si le build ou les tests échouent !

### Commits et messages

Pour le CI/CD, les messages de commit peuvent déclencher des actions.

**Convention Conventional Commits :**

```bash
feat: Ajout du formulaire de connexion
fix: Correction du calcul de TVA
docs: Mise à jour du README
test: Ajout des tests pour TClient
refactor: Extraction de la logique de validation
chore: Mise à jour des dépendances
ci: Configuration du pipeline GitLab

# Avec breaking change
feat!: Changement de l'API de connexion

BREAKING CHANGE: L'ancien système d'auth est supprimé
```

**Avantages :**
- Génération automatique de CHANGELOG
- Versioning automatique (semantic versioning)
- Déclenchement conditionnel du pipeline

## Compilation en ligne de commande

Pour automatiser avec CI/CD, vous devez pouvoir compiler sans l'IDE.

### MSBuild (intégré à Delphi)

MSBuild est l'outil de compilation de Microsoft, utilisé par Delphi.

**Localisation :**
```
C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat
```

**Script de compilation basique :**

```batch
@echo off
REM build.bat - Script de compilation

echo ========================================
echo Compilation du projet Delphi
echo ========================================

REM Charger les variables d'environnement Delphi
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

REM Nettoyer les fichiers précédents
echo.
echo [1/4] Nettoyage...
del /Q Win32\Release\*.exe 2>nul
del /Q Win32\Release\*.dcu 2>nul

REM Compiler le projet
echo.
echo [2/4] Compilation...
msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32

REM Vérifier le résultat
if %ERRORLEVEL% NEQ 0 (
    echo.
    echo *** ERREUR: La compilation a echoue ***
    exit /b 1
)

echo.
echo [3/4] Compilation reussie !

REM Créer le dossier de distribution
echo.
echo [4/4] Creation du package...
if not exist "Deploy" mkdir Deploy
copy Win32\Release\*.exe Deploy\
copy Win32\Release\*.dll Deploy\ 2>nul

echo.
echo ========================================
echo Build termine avec succes !
echo Package disponible dans: Deploy\
echo ========================================

exit /b 0
```

**Utilisation :**
```bash
# Lancer le build
build.bat

# En cas de succès, code de retour = 0
# En cas d'erreur, code de retour = 1
```

### DCC32/DCC64 (compilateur Delphi)

Alternative à MSBuild, utilisation directe du compilateur.

```batch
@echo off
REM Compilation avec DCC32

call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

dcc32 -B ^
  -$D- ^
  -$L- ^
  -$Y- ^
  -DRELEASE ^
  -U"Win32\Release" ^
  -E"Win32\Release" ^
  -N"Win32\Release" ^
  MonProjet.dpr

if %ERRORLEVEL% NEQ 0 exit /b 1

echo Compilation reussie !
exit /b 0
```

**Options principales :**
- `-B` : Build all (recompiler tout)
- `-$D-` : Désactiver les assertions
- `-$L-` : Désactiver les symboles de debug
- `-DRELEASE` : Définir la constante RELEASE
- `-U` : Unit directories (chemins de recherche)
- `-E` : Output directory (dossier de sortie)

### Exemple de script avancé

```batch
@echo off
setlocal enabledelayedexpansion

REM ========================================
REM Script de build professionnel
REM ========================================

set PROJECT_NAME=GestionClients
set VERSION=1.0.0
set BUILD_CONFIG=Release
set BUILD_PLATFORM=Win32

echo.
echo ========================================
echo Build de %PROJECT_NAME% v%VERSION%
echo Configuration: %BUILD_CONFIG%
echo Plateforme: %BUILD_PLATFORM%
echo ========================================

REM Variables d'environnement Delphi
call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
if %ERRORLEVEL% NEQ 0 (
    echo ERREUR: Impossible de charger rsvars.bat
    exit /b 1
)

REM Nettoyage
echo.
echo [1/5] Nettoyage des fichiers precedents...
if exist "%BUILD_PLATFORM%\%BUILD_CONFIG%" (
    del /Q "%BUILD_PLATFORM%\%BUILD_CONFIG%\*.exe" 2>nul
    del /Q "%BUILD_PLATFORM%\%BUILD_CONFIG%\*.dcu" 2>nul
)

REM Compilation
echo.
echo [2/5] Compilation du projet...
msbuild %PROJECT_NAME%.dproj ^
    /t:Build ^
    /p:Config=%BUILD_CONFIG% ^
    /p:Platform=%BUILD_PLATFORM% ^
    /p:DCC_Define=RELEASE ^
    /verbosity:minimal

if %ERRORLEVEL% NEQ 0 (
    echo.
    echo *** ERREUR: La compilation a echoue ***
    exit /b 1
)

REM Vérification du résultat
echo.
echo [3/5] Verification du build...
set EXE_PATH=%BUILD_PLATFORM%\%BUILD_CONFIG%\%PROJECT_NAME%.exe
if not exist "%EXE_PATH%" (
    echo ERREUR: L'executable n'a pas ete genere
    exit /b 1
)

REM Création du package
echo.
echo [4/5] Creation du package de distribution...
set DEPLOY_DIR=Deploy\%PROJECT_NAME%_v%VERSION%_%BUILD_PLATFORM%
if exist "%DEPLOY_DIR%" rmdir /S /Q "%DEPLOY_DIR%"
mkdir "%DEPLOY_DIR%"

copy "%EXE_PATH%" "%DEPLOY_DIR%\" >nul
copy "%BUILD_PLATFORM%\%BUILD_CONFIG%\*.dll" "%DEPLOY_DIR%\" 2>nul
copy "README.md" "%DEPLOY_DIR%\" 2>nul
copy "LICENSE" "%DEPLOY_DIR%\" 2>nul

REM Génération d'un fichier version
echo Version=%VERSION% > "%DEPLOY_DIR%\version.txt"
echo Build Date=%DATE% %TIME% >> "%DEPLOY_DIR%\version.txt"
echo Platform=%BUILD_PLATFORM% >> "%DEPLOY_DIR%\version.txt"
echo Config=%BUILD_CONFIG% >> "%DEPLOY_DIR%\version.txt"

REM Compression (si 7-Zip est installé)
echo.
echo [5/5] Compression...
set ZIP_FILE=Deploy\%PROJECT_NAME%_v%VERSION%_%BUILD_PLATFORM%.zip
if exist "C:\Program Files\7-Zip\7z.exe" (
    "C:\Program Files\7-Zip\7z.exe" a -tzip "%ZIP_FILE%" "%DEPLOY_DIR%\*" >nul
    echo Archive creee: %ZIP_FILE%
) else (
    echo 7-Zip non trouve, archive non creee
)

echo.
echo ========================================
echo Build termine avec succes !
echo Executable: %EXE_PATH%
echo Package: %DEPLOY_DIR%
echo ========================================

exit /b 0
```

## GitHub Actions pour Delphi

GitHub Actions est la solution CI/CD intégrée à GitHub. C'est gratuit pour les projets publics et offre 2000 minutes/mois pour les projets privés.

### Structure d'un workflow

Les workflows GitHub Actions sont des fichiers YAML dans `.github/workflows/`.

**Structure de base :**

```yaml
name: Nom du workflow
on: [événements déclencheurs]
jobs:
  nom-du-job:
    runs-on: [type de machine]
    steps:
      - name: Étape 1
        uses: action@version
      - name: Étape 2
        run: commandes
```

### Premier workflow : Build simple

Créez `.github/workflows/build.yml` :

```yaml
name: Build Delphi Application

# Déclenchement
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    name: Compiler l'application
    runs-on: windows-latest

    steps:
    # 1. Checkout du code
    - name: Checkout repository
      uses: actions/checkout@v4

    # 2. Installer Delphi
    # Note: Vous devez avoir une licence et héberger l'installateur
    # Ou utiliser Delphi Community Edition
    - name: Setup Delphi
      run: |
        echo "Installation de Delphi..."
        # Commandes d'installation
        # Ceci est un placeholder - voir section suivante

    # 3. Compiler le projet
    - name: Build Project
      run: |
        call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
        msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32
      shell: cmd

    # 4. Uploader l'exécutable comme artifact
    - name: Upload Executable
      uses: actions/upload-artifact@v4
      with:
        name: application-windows
        path: Win32/Release/*.exe
```

### Le problème de la licence Delphi

**Défi :** Delphi nécessite une licence payante pour la compilation.

**Solutions :**

#### Solution 1 : Self-hosted Runner

Utilisez votre propre machine avec Delphi installé.

1. **Sur votre machine avec Delphi :**
```bash
# Télécharger le runner
# Settings → Actions → Runners → New self-hosted runner

# Suivre les instructions pour installer le runner
```

2. **Modifier le workflow :**
```yaml
jobs:
  build:
    runs-on: self-hosted  # Au lieu de windows-latest
```

**Avantages :**
- Utilise votre licence existante
- Contrôle total

**Inconvénients :**
- Votre machine doit rester allumée
- Consomme vos ressources

#### Solution 2 : Delphi Community Edition

Si votre projet est éligible (revenus < 5000$), utilisez Community Edition.

```yaml
- name: Install Delphi Community
  run: |
    # Script pour installer Delphi CE
    # (à adapter selon votre configuration)
```

#### Solution 3 : FPC/Lazarus (Free Pascal)

Alternative open source compatible avec beaucoup de code Delphi.

```yaml
- name: Setup Free Pascal
  run: |
    choco install freepascal -y

- name: Build with FPC
  run: fpc MonProjet.dpr
```

### Workflow complet avec tests

```yaml
name: CI Pipeline

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

env:
  DELPHI_PATH: "C:\\Program Files (x86)\\Embarcadero\\Studio\\22.0"
  PROJECT_NAME: GestionClients

jobs:
  build-and-test:
    name: Build et Tests
    runs-on: self-hosted  # Machine avec Delphi installé

    steps:
    # Checkout
    - name: Checkout code
      uses: actions/checkout@v4
      with:
        fetch-depth: 0  # Historique complet pour versioning

    # Informations
    - name: Display Environment
      run: |
        echo "Branch: ${{ github.ref }}"
        echo "Commit: ${{ github.sha }}"
        echo "Author: ${{ github.actor }}"
      shell: bash

    # Cache des DCU pour accélérer
    - name: Cache Delphi DCU
      uses: actions/cache@v4
      with:
        path: |
          Win32/Release/*.dcu
          Win64/Release/*.dcu
        key: dcu-${{ runner.os }}-${{ hashFiles('**/*.pas') }}
        restore-keys: |
          dcu-${{ runner.os }}-

    # Compilation Release
    - name: Build Release
      run: |
        call "${{ env.DELPHI_PATH }}\bin\rsvars.bat"
        msbuild ${{ env.PROJECT_NAME }}.dproj ^
          /t:Build ^
          /p:Config=Release ^
          /p:Platform=Win32 ^
          /p:DCC_Define=RELEASE
      shell: cmd

    # Compilation des tests
    - name: Build Tests
      run: |
        call "${{ env.DELPHI_PATH }}\bin\rsvars.bat"
        msbuild Tests\${{ env.PROJECT_NAME }}Tests.dproj ^
          /t:Build ^
          /p:Config=Debug ^
          /p:Platform=Win32
      shell: cmd

    # Exécution des tests
    - name: Run Tests
      run: |
        Tests\Win32\Debug\${{ env.PROJECT_NAME }}Tests.exe --format=junit --output=test-results.xml
      continue-on-error: true

    # Publication des résultats de tests
    - name: Publish Test Results
      uses: EnricoMi/publish-unit-test-result-action/composite@v2
      if: always()
      with:
        files: test-results.xml

    # Analyse de code statique (si Pascal Analyzer disponible)
    - name: Static Analysis
      run: |
        # Lancer Pascal Analyzer ou autre outil
        echo "Analyse statique..."
      continue-on-error: true

    # Archiver l'exécutable
    - name: Upload Artifact
      uses: actions/upload-artifact@v4
      with:
        name: ${{ env.PROJECT_NAME }}-Win32-Release
        path: |
          Win32/Release/*.exe
          Win32/Release/*.dll
        retention-days: 30

    # Créer une release si tag
    - name: Create Release
      if: startsWith(github.ref, 'refs/tags/v')
      uses: softprops/action-gh-release@v1
      with:
        files: Win32/Release/*.exe
        body: |
          Version ${{ github.ref_name }}

          Changements:
          ${{ github.event.head_commit.message }}
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

### Workflow multi-plateforme

Pour compiler Windows, macOS, iOS, Android :

```yaml
name: Multi-Platform Build

on:
  push:
    branches: [ main ]

jobs:
  build-windows:
    name: Build Windows
    runs-on: self-hosted-windows
    steps:
      - uses: actions/checkout@v4
      - name: Build Win32
        run: msbuild MyApp.dproj /p:Platform=Win32
      - name: Build Win64
        run: msbuild MyApp.dproj /p:Platform=Win64
      - uses: actions/upload-artifact@v4
        with:
          name: windows-builds
          path: |
            Win32/Release/*.exe
            Win64/Release/*.exe

  build-macos:
    name: Build macOS
    runs-on: self-hosted-macos
    steps:
      - uses: actions/checkout@v4
      - name: Build OSX64
        run: msbuild MyApp.dproj /p:Platform=OSX64
      - uses: actions/upload-artifact@v4
        with:
          name: macos-build
          path: OSX64/Release/*.app

  build-android:
    name: Build Android
    runs-on: self-hosted-windows
    steps:
      - uses: actions/checkout@v4
      - name: Build Android
        run: msbuild MyApp.dproj /p:Platform=Android
      - uses: actions/upload-artifact@v4
        with:
          name: android-build
          path: Android/Release/*.apk
```

## GitLab CI/CD pour Delphi

GitLab CI/CD est l'alternative intégrée à GitLab. Très puissant et flexible.

### Structure de base

Le fichier de configuration est `.gitlab-ci.yml` à la racine.

**Exemple simple :**

```yaml
# .gitlab-ci.yml

stages:
  - build
  - test
  - deploy

variables:
  PROJECT_NAME: "GestionClients"
  DELPHI_PATH: "C:\\Program Files (x86)\\Embarcadero\\Studio\\22.0"

# Job de compilation
build:
  stage: build
  tags:
    - windows
    - delphi
  script:
    - call "%DELPHI_PATH%\bin\rsvars.bat"
    - msbuild %PROJECT_NAME%.dproj /t:Build /p:Config=Release /p:Platform=Win32
  artifacts:
    paths:
      - Win32/Release/*.exe
    expire_in: 1 week

# Job de tests
test:
  stage: test
  tags:
    - windows
    - delphi
  script:
    - call "%DELPHI_PATH%\bin\rsvars.bat"
    - msbuild Tests\%PROJECT_NAME%Tests.dproj /t:Build /p:Config=Debug
    - Tests\Win32\Debug\%PROJECT_NAME%Tests.exe
  dependencies:
    - build

# Job de déploiement
deploy:
  stage: deploy
  tags:
    - windows
  script:
    - echo "Déploiement de l'application..."
    - copy Win32\Release\*.exe \\server\share\applications\
  only:
    - main
  dependencies:
    - build
```

### GitLab Runner sur Windows

Pour exécuter des builds Delphi, installez un GitLab Runner sur une machine Windows avec Delphi.

**Installation :**

```powershell
# Télécharger le runner
Invoke-WebRequest -Uri "https://gitlab-runner-downloads.s3.amazonaws.com/latest/binaries/gitlab-runner-windows-amd64.exe" -OutFile "C:\GitLab-Runner\gitlab-runner.exe"

# Installer
cd C:\GitLab-Runner
.\gitlab-runner.exe install
.\gitlab-runner.exe start

# Enregistrer le runner
.\gitlab-runner.exe register
```

**Configuration lors de l'enregistrement :**
```
GitLab URL: https://gitlab.com/
Token: [votre token depuis GitLab]
Description: Delphi Build Server
Tags: windows, delphi
Executor: shell
```

### Pipeline avancé avec cache

```yaml
# .gitlab-ci.yml

image: windows-servercore:latest

variables:
  PROJECT_NAME: "MonApplication"
  VERSION: "1.0.${CI_PIPELINE_ID}"

# Cache pour accélérer les builds
cache:
  key: ${CI_COMMIT_REF_SLUG}
  paths:
    - Win32/Release/*.dcu
    - Win64/Release/*.dcu

stages:
  - prepare
  - build
  - test
  - quality
  - package
  - deploy

# Préparation
prepare:
  stage: prepare
  tags: [windows, delphi]
  script:
    - echo "Preparation du build..."
    - echo "Version %VERSION%"
    - mkdir -p artifacts
  artifacts:
    paths:
      - artifacts/

# Build Win32
build-win32:
  stage: build
  tags: [windows, delphi]
  script:
    - call "%DELPHI_PATH%\bin\rsvars.bat"
    - echo Building Win32...
    - msbuild %PROJECT_NAME%.dproj /t:Build /p:Config=Release /p:Platform=Win32 /verbosity:minimal
  artifacts:
    paths:
      - Win32/Release/*.exe
      - Win32/Release/*.dll
    expire_in: 1 day

# Build Win64
build-win64:
  stage: build
  tags: [windows, delphi]
  script:
    - call "%DELPHI_PATH%\bin\rsvars.bat"
    - echo Building Win64...
    - msbuild %PROJECT_NAME%.dproj /t:Build /p:Config=Release /p:Platform=Win64 /verbosity:minimal
  artifacts:
    paths:
      - Win64/Release/*.exe
      - Win64/Release/*.dll
    expire_in: 1 day

# Tests unitaires
unit-tests:
  stage: test
  tags: [windows, delphi]
  script:
    - call "%DELPHI_PATH%\bin\rsvars.bat"
    - msbuild Tests\%PROJECT_NAME%Tests.dproj /t:Build /p:Config=Debug /p:Platform=Win32
    - Tests\Win32\Debug\%PROJECT_NAME%Tests.exe --format=junit --output=junit-results.xml
  artifacts:
    when: always
    reports:
      junit: junit-results.xml

# Analyse de code
code-quality:
  stage: quality
  tags: [windows]
  script:
    - echo "Analyse de la qualite du code..."
    # Lancer Pascal Analyzer ou FixInsight
    # paanalyzer MyProject.dpr -output quality-report.html
  artifacts:
    paths:
      - quality-report.html
  allow_failure: true

# Package
package:
  stage: package
  tags: [windows]
  dependencies:
    - build-win32
    - build-win64
  script:
    - echo "Creation du package de distribution..."
    - mkdir Deploy\%PROJECT_NAME%_v%VERSION%
    - copy Win32\Release\*.exe Deploy\%PROJECT_NAME%_v%VERSION%\
    - copy Win64\Release\*.exe Deploy\%PROJECT_NAME%_v%VERSION%\x64\
    - echo %VERSION% > Deploy\%PROJECT_NAME%_v%VERSION%\version.txt
    - 7z a Deploy\%PROJECT_NAME%_v%VERSION%.zip Deploy\%PROJECT_NAME%_v%VERSION%\*
  artifacts:
    paths:
      - Deploy/*.zip
    expire_in: 1 month

# Déploiement en environnement de test
deploy-test:
  stage: deploy
  tags: [windows, deploy]
  environment:
    name: test
    url: http://test.example.com
  script:
    - echo "Deploiement en environnement de test..."
    - copy Deploy\*.zip \\testserver\applications\
    # Décompresser et installer
  only:
    - develop

# Déploiement en production (manuel)
deploy-production:
  stage: deploy
  tags: [windows, deploy]
  environment:
    name: production
    url: http://www.example.com
  script:
    - echo "Deploiement en production..."
    - copy Deploy\*.zip \\prodserver\applications\
  when: manual
  only:
    - main
```

### Déclenchement conditionnel

```yaml
# Compiler seulement si des fichiers .pas ont changé
build:
  rules:
    - changes:
        - "**/*.pas"
        - "**/*.dfm"
        - "**/*.dpr"
  script:
    - msbuild MyProject.dproj

# Déployer seulement sur des tags
deploy:
  rules:
    - if: '$CI_COMMIT_TAG =~ /^v\d+\.\d+\.\d+$/'
  script:
    - echo "Deploying version $CI_COMMIT_TAG"

# Pipeline différent selon la branche
job-develop:
  rules:
    - if: '$CI_COMMIT_BRANCH == "develop"'
  script:
    - echo "Build rapide pour develop"
    - msbuild /p:Config=Debug

job-main:
  rules:
    - if: '$CI_COMMIT_BRANCH == "main"'
  script:
    - echo "Build complet pour main"
    - msbuild /p:Config=Release
```

## Azure DevOps pour Delphi

Azure DevOps (anciennement VSTS) est une solution Microsoft complète.

### Pipeline YAML

Créez `azure-pipelines.yml` :

```yaml
# azure-pipelines.yml

trigger:
  branches:
    include:
      - main
      - develop
  paths:
    include:
      - Source/*
      - Tests/*

pool:
  name: Default  # Pool avec agent Windows + Delphi
  demands:
    - Delphi
    - msbuild

variables:
  solution: 'MonProjet.dproj'
  buildPlatform: 'Win32'
  buildConfiguration: 'Release'

stages:
- stage: Build
  displayName: 'Build Application'
  jobs:
  - job: BuildJob
    displayName: 'Compile Project'
    steps:
    - checkout: self
      clean: true

    - task: PowerShell@2
      displayName: 'Setup Delphi Environment'
      inputs:
        targetType: 'inline'
        script: |
          $env:PATH = "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin;$env:PATH"
          & "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

    - task: MSBuild@1
      displayName: 'Build $(buildPlatform) $(buildConfiguration)'
      inputs:
        solution: '$(solution)'
        platform: '$(buildPlatform)'
        configuration: '$(buildConfiguration)'
        msbuildArguments: '/p:DCC_Define=RELEASE /verbosity:minimal'

    - task: CopyFiles@2
      displayName: 'Copy Build Output'
      inputs:
        SourceFolder: '$(buildPlatform)/$(buildConfiguration)'
        Contents: |
          **.exe
          **.dll
        TargetFolder: '$(Build.ArtifactStagingDirectory)'

    - task: PublishBuildArtifacts@1
      displayName: 'Publish Artifact'
      inputs:
        PathtoPublish: '$(Build.ArtifactStagingDirectory)'
        ArtifactName: 'drop'

- stage: Test
  displayName: 'Run Tests'
  dependsOn: Build
  jobs:
  - job: TestJob
    displayName: 'Execute Unit Tests'
    steps:
    - task: MSBuild@1
      displayName: 'Build Tests'
      inputs:
        solution: 'Tests/MonProjetTests.dproj'
        platform: '$(buildPlatform)'
        configuration: 'Debug'

    - task: VSTest@2
      displayName: 'Run Tests'
      inputs:
        testAssemblyVer2: |
          **\*Tests.exe
          !**\obj\**
        searchFolder: '$(System.DefaultWorkingDirectory)'
        runInParallel: false

- stage: Deploy
  displayName: 'Deploy to Test'
  dependsOn: Test
  condition: and(succeeded(), eq(variables['Build.SourceBranch'], 'refs/heads/develop'))
  jobs:
  - deployment: DeployJob
    displayName: 'Deploy to Test Environment'
    environment: 'Test'
    strategy:
      runOnce:
        deploy:
          steps:
          - task: DownloadBuildArtifacts@0
            inputs:
              artifactName: 'drop'

          - task: PowerShell@2
            displayName: 'Deploy Application'
            inputs:
              targetType: 'inline'
              script: |
                # Copier vers le serveur de test
                Copy-Item -Path "$(Pipeline.Workspace)/drop/*" -Destination "\\testserver\apps\" -Force
```

### Pipeline classique (visual)

Azure DevOps offre aussi un éditeur visuel :

1. Créer un nouveau pipeline
2. Choisir "Classic editor"
3. Ajouter des tâches :
   - **MSBuild** : Compiler le projet
   - **Copy Files** : Copier les exécutables
   - **Publish Artifacts** : Publier les artifacts
   - **Deploy** : Déployer sur serveur

## Tests automatisés dans le pipeline

### DUnitX en ligne de commande

Pour exécuter DUnitX dans un pipeline :

```pascal
program MonProjetTests;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  TestClient in 'TestClient.pas',
  TestCommande in 'TestCommande.pas';

var
  Runner: ITestRunner;
  Results: IRunResults;
  Logger: ITestLogger;
  NUnitLogger: ITestLogger;
begin
  try
    // Créer le runner
    Runner := TDUnitX.CreateRunner;

    // Logger console
    Logger := TDUnitXConsoleLogger.Create(true);
    Runner.AddLogger(Logger);

    // Logger XML pour CI/CD
    NUnitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    Runner.AddLogger(NUnitLogger);

    // Exécuter les tests
    Results := Runner.Execute;

    // Code de sortie
    if not Results.AllPassed then
      ExitCode := EXIT_ERRORS
    else
      ExitCode := EXIT_SUCCESS;

  except
    on E: Exception do
    begin
      WriteLn(E.ClassName, ': ', E.Message);
      ExitCode := EXIT_ERRORS;
    end;
  end;
end.
```

**Dans le pipeline :**

```yaml
- name: Run Tests
  run: |
    Tests\Win32\Debug\MonProjetTests.exe --output=test-results.xml
  continue-on-error: true

- name: Publish Test Results
  if: always()
  uses: EnricoMi/publish-unit-test-result-action@v2
  with:
    files: test-results.xml
```

### Tests avec couverture de code

Si vous utilisez un outil de couverture (comme TestInsight) :

```yaml
- name: Run Tests with Coverage
  run: |
    TestInsight\Coverage.exe MonProjetTests.exe --xml-coverage=coverage.xml

- name: Publish Coverage
  uses: codecov/codecov-action@v3
  with:
    files: coverage.xml
```

## Déploiement automatisé

### Déploiement sur serveur Windows

```yaml
deploy:
  stage: deploy
  environment: production
  script:
    # Arrêter l'application
    - psexec \\prodserver sc stop MonApplication

    # Copier les fichiers
    - robocopy Win32\Release \\prodserver\apps\MonApplication /MIR /R:3 /W:5

    # Redémarrer l'application
    - psexec \\prodserver sc start MonApplication

    # Vérifier le démarrage
    - ping -n 10 127.0.0.1 > nul
    - curl http://prodserver/health
  only:
    - tags
```

### Déploiement FTP

```yaml
deploy-ftp:
  stage: deploy
  script:
    - |
      ftp -inv prodserver.com <<EOF
      user $FTP_USER $FTP_PASSWORD
      binary
      cd /apps
      mput Win32/Release/*.exe
      bye
      EOF
```

### Création d'installateur

```yaml
build-installer:
  stage: package
  script:
    # Inno Setup
    - iscc installer-script.iss
  artifacts:
    paths:
      - Output\Setup_*.exe
```

**Script Inno Setup (installer-script.iss) :**

```ini
[Setup]
AppName=Mon Application
AppVersion={#VERSION}
DefaultDirName={pf}\MonApplication
OutputDir=Output
OutputBaseFilename=Setup_{#VERSION}

[Files]
Source: "Win32\Release\MonApplication.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "Win32\Release\*.dll"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\Mon Application"; Filename: "{app}\MonApplication.exe"
```

## Monitoring et notifications

### Notifications Slack

```yaml
# .gitlab-ci.yml
after_script:
  - |
    if [ "$CI_JOB_STATUS" == "success" ]; then
      curl -X POST -H 'Content-type: application/json' \
      --data "{\"text\":\"✅ Build réussi pour $CI_PROJECT_NAME ($CI_COMMIT_REF_NAME)\"}" \
      $SLACK_WEBHOOK_URL
    else
      curl -X POST -H 'Content-type: application/json' \
      --data "{\"text\":\"❌ Build échoué pour $CI_PROJECT_NAME ($CI_COMMIT_REF_NAME)\"}" \
      $SLACK_WEBHOOK_URL
    fi
```

### Notifications Email

GitHub Actions :
```yaml
- name: Send Email
  if: failure()
  uses: dawidd6/action-send-mail@v3
  with:
    server_address: smtp.gmail.com
    server_port: 465
    username: ${{ secrets.EMAIL_USERNAME }}
    password: ${{ secrets.EMAIL_PASSWORD }}
    subject: Build Failed - ${{ github.repository }}
    body: |
      Le build a échoué pour ${{ github.ref }}
      Commit: ${{ github.sha }}
      Auteur: ${{ github.actor }}
    to: dev-team@example.com
```

### Discord Webhook

```yaml
- name: Discord Notification
  if: always()
  run: |
    $status = if ($LASTEXITCODE -eq 0) { "✅ Succès" } else { "❌ Échec" }
    $json = @{
      content = "$status - Build $env:PROJECT_NAME"
      embeds = @(
        @{
          title = "Détails du build"
          fields = @(
            @{ name = "Branche"; value = "$env:BRANCH"; inline = $true }
            @{ name = "Commit"; value = "$env:COMMIT_SHA"; inline = $true }
          )
        }
      )
    } | ConvertTo-Json -Depth 4

    Invoke-WebRequest -Uri $env:DISCORD_WEBHOOK -Method Post -Body $json -ContentType "application/json"
  shell: powershell
```

## Versioning automatique

### Semantic Versioning avec commits

```yaml
versioning:
  stage: prepare
  script:
    - |
      # Analyser les commits pour déterminer la version
      if git log -1 --pretty=%B | grep -q "BREAKING CHANGE"; then
        # Version majeure
        VERSION=$(git describe --tags | awk -F. '{print $1+1".0.0"}')
      elif git log -1 --pretty=%B | grep -q "^feat"; then
        # Version mineure
        VERSION=$(git describe --tags | awk -F. '{print $1"."$2+1".0"}')
      else
        # Version patch
        VERSION=$(git describe --tags | awk -F. '{print $1"."$2"."$3+1"}')
      fi
      echo "VERSION=$VERSION" >> version.env
  artifacts:
    reports:
      dotenv: version.env
```

### Injection de version dans l'exécutable

Créez un fichier `Version.inc` automatiquement :

```yaml
- name: Generate Version File
  run: |
    $version = "${{ env.VERSION }}"
    $content = @"
const
  APP_VERSION = '$version';
  BUILD_DATE = '${{ github.event.head_commit.timestamp }}';
  BUILD_NUMBER = '${{ github.run_number }}';
  GIT_COMMIT = '${{ github.sha }}';
"@
    $content | Out-File -FilePath Source\Version.inc -Encoding UTF8
```

Puis dans votre code Delphi :

```pascal
unit AppVersion;

interface

const
  {$I Version.inc}

implementation

end.
```

## Bonnes pratiques CI/CD pour Delphi

### 1. Builds reproductibles

Assurez-vous que le build produit toujours le même résultat :

- ✅ Versions fixes des dépendances
- ✅ Environnement contrôlé
- ✅ Pas de chemins absolus
- ✅ Configuration explicite

### 2. Builds rapides

Optimisez le temps de build :

```yaml
# Cache des DCU
cache:
  paths:
    - "**/*.dcu"

# Compilation incrémentale
msbuild /t:Build  # Au lieu de /t:Rebuild

# Parallélisation
msbuild /m:4  # 4 processus parallèles
```

### 3. Fail fast

Arrêtez dès la première erreur :

```yaml
set -e  # Bash
$ErrorActionPreference = "Stop"  # PowerShell
```

### 4. Logs clairs

```yaml
- name: Build
  run: |
    echo "========================================="
    echo "Building $PROJECT_NAME v$VERSION"
    echo "Platform: $PLATFORM"
    echo "Configuration: $CONFIG"
    echo "========================================="
    msbuild /verbosity:minimal  # Pas trop verbeux
```

### 5. Artifacts organisés

```
artifacts/
├── windows/
│   ├── x86/
│   │   └── MonApp.exe
│   └── x64/
│       └── MonApp.exe
├── logs/
│   ├── build.log
│   └── test.log
└── reports/
    ├── test-results.xml
    └── coverage.html
```

### 6. Environnements isolés

```yaml
environments:
  - name: dev
    url: http://dev.example.com
  - name: test
    url: http://test.example.com
    protection: manual
  - name: production
    url: http://www.example.com
    protection: required_reviews
```

### 7. Rollback facile

Gardez les versions précédentes :

```yaml
artifacts:
  expire_in: 3 months  # Garder 3 mois

# Tag automatique
- git tag "build-$CI_PIPELINE_ID"
- git push --tags
```

### 8. Monitoring

```yaml
- name: Health Check
  if: success()
  run: |
    sleep 30  # Attendre le démarrage
    curl -f http://prodserver/health || exit 1
```

## Checklist CI/CD

Avant de mettre en production votre pipeline :

### Setup initial
- [ ] Git repository configuré
- [ ] .gitignore adapté à Delphi
- [ ] Runner installé et enregistré
- [ ] Delphi installé sur le runner
- [ ] Licence Delphi valide

### Pipeline
- [ ] Compilation automatique fonctionne
- [ ] Tests automatiques s'exécutent
- [ ] Artifacts sont générés
- [ ] Cache configuré pour accélérer
- [ ] Notifications configurées

### Qualité
- [ ] Tests unitaires présents
- [ ] Couverture de code > 70%
- [ ] Analyse statique configurée
- [ ] Pas de warnings de compilation
- [ ] Documentation à jour

### Déploiement
- [ ] Environnements définis (dev/test/prod)
- [ ] Déploiement automatique en test
- [ ] Déploiement manuel en prod
- [ ] Rollback possible
- [ ] Health checks configurés

### Sécurité
- [ ] Secrets stockés de manière sécurisée
- [ ] Pas de mots de passe dans le code
- [ ] Protection de la branche main
- [ ] Revue obligatoire avant merge

## Conclusion

L'intégration de Git avec CI/CD transforme radicalement votre façon de développer avec Delphi. Ce qui prenait des heures (compilation, tests, déploiement) prend maintenant quelques minutes, automatiquement.

**Bénéfices concrets :**

1. **Détection immédiate des problèmes** - Un commit qui casse le build ? Vous le savez en 5 minutes
2. **Confiance** - Les tests automatiques assurent que tout fonctionne
3. **Productivité** - Plus de temps perdu en tâches manuelles répétitives
4. **Qualité** - Le code est vérifié systématiquement
5. **Déploiements sereins** - Fini les vendredis soir stressants
6. **Traçabilité** - Historique complet de tous les builds et déploiements

**Par où commencer ?**

1. **Semaine 1** : Mettre en place la compilation automatique
2. **Semaine 2** : Ajouter les tests automatiques
3. **Semaine 3** : Configurer le déploiement automatique en test
4. **Semaine 4** : Peaufiner et optimiser

**Points clés à retenir :**

- ✅ Commencez simple, complexifiez progressivement
- ✅ La compilation en ligne de commande est la base
- ✅ Les tests automatiques sont essentiels
- ✅ Self-hosted runners pour utiliser votre licence Delphi
- ✅ GitHub Actions, GitLab CI ou Azure DevOps : tous sont adaptés
- ✅ Notifications pour rester informé
- ✅ Protection des branches pour garantir la qualité
- ✅ Automatisez tout ce qui est répétitif

**Citation finale :**

> "Si ça fait mal, faites-le plus souvent"
> — Martin Fowler (sur le déploiement)

L'idée : plus vous déployez fréquemment, plus ça devient facile et moins stressant. Le CI/CD rend cela possible.

Vous voilà prêt à moderniser votre workflow Delphi ! Commencez petit, itérez, et bientôt vous ne pourrez plus vous en passer. Votre équipe vous remerciera, et votre stress diminuera considérablement.

---

**Ressources complémentaires :**

- Documentation GitHub Actions : https://docs.github.com/actions
- Documentation GitLab CI : https://docs.gitlab.com/ee/ci/
- Azure DevOps : https://docs.microsoft.com/azure/devops/
- DUnitX : https://github.com/VSoftTechnologies/DUnitX
- Delphi MSBuild : https://docwiki.embarcadero.com/RADStudio/en/MSBuild

Bon build automatisé ! 🚀

⏭️ [Clean Code et principes SOLID](/18-architecture-et-bonnes-pratiques/09-clean-code-et-principes-solid.md)
