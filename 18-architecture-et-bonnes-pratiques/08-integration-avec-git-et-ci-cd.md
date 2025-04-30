# 18.8 Intégration avec Git et CI/CD

## Introduction

Dans ce chapitre, nous allons explorer comment intégrer votre développement Delphi avec deux pratiques essentielles du développement logiciel moderne : le contrôle de version avec Git et l'intégration/déploiement continus (CI/CD). Ces techniques, bien que parfois perçues comme complexes, peuvent considérablement améliorer votre flux de travail et la qualité de vos applications, même pour les projets de petite taille ou les développeurs individuels.

Nous aborderons ces concepts de manière progressive, en commençant par les bases et en avançant vers des configurations plus avancées. Que vous soyez un développeur solo ou que vous travailliez en équipe, ces outils et pratiques vous aideront à rendre votre développement Delphi plus efficace et plus fiable.

## Partie 1 : Intégration de Delphi avec Git

### Qu'est-ce que Git (rappel) ?

Git est un système de contrôle de version distribué qui vous permet de suivre les modifications de votre code au fil du temps. Contrairement aux anciens systèmes comme SVN, Git stocke une copie complète du dépôt sur chaque machine, ce qui facilite le travail hors ligne et améliore les performances.

Les avantages principaux de Git pour les développeurs Delphi sont :

- Suivi des modifications de code
- Possibilité de revenir à des versions antérieures
- Travail sur différentes fonctionnalités en parallèle via les branches
- Collaboration facilitée entre développeurs
- Sauvegarde décentralisée du code source

### Configuration de Git pour un projet Delphi

Nous avons vu les bases de Git dans le chapitre 18.5, mais voyons maintenant comment l'intégrer spécifiquement avec Delphi.

#### Création d'un fichier .gitignore adapté à Delphi

Pour éviter de versionner des fichiers inutiles, créez un fichier `.gitignore` à la racine de votre projet. Voici un exemple complet pour Delphi :

```
# Fichiers de compilation Delphi
*.dcu
*.o
*.obj
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

# Dossiers de compilation et temporaires
__history/
__recovery/
Win32/
Win64/
Android/
iOSDevice/
iOSSimulator/
OSX32/
Linux64/
Debug/
Release/

# Fichiers de sauvegarde locaux
*.~*
*.local
*.identcache
*.projdata
*.tvsconfig
*.dsk

# Fichiers de configuration locale
*.stat

# Dossiers de bibliothèques
/lib/

# Fichiers de projet spécifiques Delphi
ModelSupport*/
*.vlb
*.dproj.local
*.groupproj.local

# Fichiers de base de données locaux
*.sdb
*.sqlite
*.db
```

#### Organisation recommandée des fichiers

Une bonne organisation des fichiers facilite l'utilisation de Git avec Delphi :

```
MonProjet/
  ├── .git/                # Dossier Git (automatiquement créé)
  ├── .gitignore           # Fichier d'exclusion Git
  ├── README.md            # Description du projet
  ├── src/                 # Code source
  │   ├── units/           # Unités Pascal
  │   ├── forms/           # Formulaires
  │   └── resources/       # Ressources (images, icons, etc.)
  ├── libs/                # Bibliothèques tierces
  ├── tests/               # Tests unitaires
  ├── docs/                # Documentation
  ├── scripts/             # Scripts de build/déploiement
  └── MonProjet.dproj      # Projet Delphi principal
```

### Gestion des fichiers binaires

Les projets Delphi contiennent souvent des fichiers binaires comme des images et des ressources. Ces fichiers peuvent poser problème avec Git car ils ne peuvent pas être fusionnés comme du texte et occupent beaucoup d'espace dans l'historique.

#### Options pour gérer les fichiers binaires

1. **Git LFS (Large File Storage)** : Extension de Git pour gérer efficacement les gros fichiers binaires.

   Installation et configuration :
   ```bash
   # Installation de Git LFS
   git lfs install

   # Configuration pour suivre les fichiers image
   git lfs track "*.png" "*.jpg" "*.ico"

   # N'oubliez pas d'ajouter le fichier .gitattributes
   git add .gitattributes
   ```

2. **Externalisation des ressources** : Stockez les fichiers binaires volumineux en dehors du dépôt Git.

### Comment gérer les fichiers de projet Delphi (.dproj)

Les fichiers de projet Delphi (.dproj) sont des fichiers XML qui contiennent des informations sur votre projet. Ils incluent souvent des chemins absolus et des paramètres spécifiques à votre machine.

#### Approche 1 : Versionner les fichiers .dproj avec des directives

```xml
<!-- Dans le fichier .dproj -->
<!-- Modifiez les chemins pour utiliser des variables d'environnement -->
<DCC_UnitSearchPath>$(DELPHI_LIBS)\MyLibrary;$(DCC_UnitSearchPath)</DCC_UnitSearchPath>
```

Créez un fichier batch ou script qui définit ces variables avant d'ouvrir Delphi :

```bat
@echo off
SET DELPHI_LIBS=C:\Delphi\Libraries
START "" "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\bds.exe" -pDelphi
```

#### Approche 2 : Utiliser des fichiers modèles

1. Renommez votre fichier `.dproj` en `.dproj.template`
2. Ajoutez `.dproj` à votre `.gitignore`
3. Créez un script pour générer le `.dproj` à partir du template

### Stratégies de branches pour les projets Delphi

Une bonne stratégie de branches est essentielle pour un flux de travail efficace avec Git.

#### Stratégie de branches simple (GitFlow simplifié)

- **main** (ou **master**) : Code stable et prêt pour la production
- **develop** : Branche principale de développement
- **feature/xxx** : Branches pour les nouvelles fonctionnalités
- **bugfix/xxx** : Branches pour les corrections de bugs
- **release/x.x.x** : Branches pour la préparation des versions

#### Exemple de workflow typique

```bash
# Créer une nouvelle fonctionnalité
git checkout develop
git checkout -b feature/nouvelle-interface-client

# Faire des modifications et les committer
git add .
git commit -m "Ajout du formulaire de saisie client"

# Une fois la fonctionnalité terminée, fusion dans develop
git checkout develop
git merge feature/nouvelle-interface-client

# Préparation d'une release
git checkout -b release/1.2.0
# Effectuer les derniers ajustements...
git commit -m "Mise à jour du numéro de version"

# Finalisation de la release
git checkout main
git merge release/1.2.0
git tag -a v1.2.0 -m "Version 1.2.0"

git checkout develop
git merge release/1.2.0

# Suppression de la branche de release
git branch -d release/1.2.0
```

### Hooks Git pour automatiser les tâches Delphi

Les hooks Git sont des scripts qui s'exécutent automatiquement à certains moments, comme avant un commit ou après un push.

#### Hook de pré-commit pour vérifier le code Delphi

Créez un fichier `.git/hooks/pre-commit` (sans extension sur Linux/macOS, avec `.bat` ou `.cmd` sur Windows) :

```bat
@echo off
echo Vérification du code Delphi...

REM Exécuter les tests unitaires
"C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\dcc32.exe" -cc tests\TestRunner.dpr
if errorlevel 1 (
  echo Tests unitaires échoués, commit annulé!
  exit /b 1
)

REM Vérification réussie
echo Vérification réussie.
exit /b 0
```

N'oubliez pas de rendre le fichier exécutable sur Linux/macOS :
```bash
chmod +x .git/hooks/pre-commit
```

#### Hook de post-commit pour générer la documentation

```bat
@echo off
echo Génération de la documentation...

REM Chemin vers l'outil de documentation (exemple avec PasDoc)
pasdoc --title "Mon Projet Delphi" --output docs\api src\units\*.pas

echo Documentation générée.
```

## Partie 2 : Mise en place d'un système CI/CD pour Delphi

### Qu'est-ce que le CI/CD ?

CI/CD signifie Intégration Continue (Continuous Integration) et Déploiement Continu (Continuous Deployment/Delivery).

- **Intégration Continue (CI)** : Pratique consistant à intégrer fréquemment les modifications de code dans un dépôt partagé, avec vérification automatique (compilation, tests).

- **Déploiement Continu (CD)** : Extension de la CI qui automatise le processus de livraison et de déploiement des applications.

### Avantages du CI/CD pour les projets Delphi

1. **Détection précoce des problèmes** : Les erreurs de compilation et les bugs sont détectés rapidement
2. **Cohérence des builds** : Chaque build est effectué dans un environnement propre et contrôlé
3. **Automatisation des tests** : Les tests unitaires et d'intégration sont exécutés automatiquement
4. **Déploiement fiable** : Le processus de déploiement est standardisé et reproductible
5. **Feedback rapide** : Les développeurs reçoivent un retour immédiat sur leurs modifications

### Outils CI/CD compatibles avec Delphi

Plusieurs plateformes CI/CD peuvent être utilisées avec Delphi :

1. **Jenkins** : Serveur d'automatisation open-source hautement personnalisable
2. **GitHub Actions** : Service CI/CD intégré à GitHub
3. **GitLab CI/CD** : Intégré à GitLab
4. **Azure DevOps** : Solution Microsoft complète pour le CI/CD
5. **TeamCity** : Solution CI/CD commerciale de JetBrains

Pour ce chapitre, nous nous concentrerons sur GitHub Actions, car c'est une solution gratuite et facile à configurer.

### Mise en place de GitHub Actions pour un projet Delphi

GitHub Actions utilise des fichiers YAML pour définir les workflows d'automatisation.

#### Étape 1 : Créer la structure de répertoires

Dans votre dépôt GitHub, créez le répertoire `.github/workflows/`.

#### Étape 2 : Créer un workflow de base

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
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1.2.2
      with:
        version: '12.0'  # Pour Delphi 12 Athens (ajustez selon votre version)
        edition: 'community'

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32

    - name: Archive artifacts
      uses: actions/upload-artifact@v3
      with:
        name: MonProjet
        path: Win32/Release/MonProjet.exe
```

Ce workflow de base fait les choses suivantes :
1. Se déclenche lors des pushs sur les branches `main` et `develop`
2. Vérifie le code source
3. Configure Delphi (Community Edition)
4. Compile le projet en mode Release
5. Archive l'exécutable résultant

#### Étape 3 : Configuration avancée avec tests

Améliorons le workflow pour inclure des tests unitaires :

```yaml
name: Delphi CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]

jobs:
  build-and-test:
    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1.2.2
      with:
        version: '12.0'
        edition: 'community'

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32

    - name: Build Tests
      run: |
        msbuild tests/TestRunner.dproj /t:Build /p:Config=Release /p:Platform=Win32

    - name: Run Tests
      run: |
        tests/Win32/Release/TestRunner.exe --format=xml --output=test-results.xml

    - name: Publish Test Results
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: Delphi Tests
        path: test-results.xml
        reporter: java-junit

    - name: Archive application
      uses: actions/upload-artifact@v3
      with:
        name: MonProjet
        path: Win32/Release/MonProjet.exe
```

### Configuration d'un déploiement automatique

Ajoutons maintenant le déploiement automatique pour créer un installateur et le publier :

```yaml
name: Delphi CI/CD

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]
  release:
    types: [ published ]

jobs:
  build-and-test:
    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1.2.2
      with:
        version: '12.0'
        edition: 'community'

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32

    - name: Build Tests
      run: |
        msbuild tests/TestRunner.dproj /t:Build /p:Config=Release /p:Platform=Win32

    - name: Run Tests
      run: |
        tests/Win32/Release/TestRunner.exe --format=xml --output=test-results.xml

    - name: Publish Test Results
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: Delphi Tests
        path: test-results.xml
        reporter: java-junit

    - name: Archive application
      uses: actions/upload-artifact@v3
      with:
        name: MonProjet
        path: Win32/Release/MonProjet.exe

  deploy:
    needs: build-and-test
    if: github.event_name == 'release'
    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Download built application
      uses: actions/download-artifact@v3
      with:
        name: MonProjet
        path: installer/files

    - name: Setup Inno Setup
      run: |
        choco install innosetup -y

    - name: Build Installer
      run: |
        "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" /DMyAppVersion="${{ github.event.release.tag_name }}" installer/setup.iss

    - name: Upload Installer to Release
      uses: actions/upload-release-asset@v1
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
      with:
        upload_url: ${{ github.event.release.upload_url }}
        asset_path: installer/Output/MonProjetSetup.exe
        asset_name: MonProjetSetup-${{ github.event.release.tag_name }}.exe
        asset_content_type: application/octet-stream
```

### Création d'un script Inno Setup pour le déploiement

Pour que le workflow ci-dessus fonctionne, vous devez créer un script Inno Setup à l'emplacement `installer/setup.iss` :

```iss
; Script Inno Setup pour MonProjet

#define MyAppName "Mon Projet"
; MyAppVersion défini par le paramètre /D dans la ligne de commande

#define MyAppPublisher "Votre Entreprise"
#define MyAppURL "https://www.votreentreprise.com/"
#define MyAppExeName "MonProjet.exe"

[Setup]
AppId={{A1B2C3D4-E5F6-7890-A1B2-C3D4E5F67890}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputDir=Output
OutputBaseFilename=MonProjetSetup
Compression=lzma
SolidCompression=yes
WizardStyle=modern

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "files\MonProjet.exe"; DestDir: "{app}"; Flags: ignoreversion
; Ajoutez d'autres fichiers nécessaires ici
Source: "files\*.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs
Source: "..\assets\*"; DestDir: "{app}\assets"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
```

## Partie 3 : Configuration avancée et bonnes pratiques

### Utilisation des matrices de build pour plusieurs configurations

Les matrices de build permettent de tester plusieurs configurations en parallèle :

```yaml
jobs:
  build:
    strategy:
      matrix:
        platform: [Win32, Win64]
        config: [Debug, Release]

    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1.2.2
      with:
        version: '12.0'
        edition: 'community'

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Config=${{ matrix.config }} /p:Platform=${{ matrix.platform }}
```

### Gestion des secrets et des configurations sensibles

Pour les informations sensibles comme les clés d'API ou les mots de passe, utilisez les secrets GitHub :

1. Dans votre dépôt GitHub, allez dans Settings > Secrets and variables > Actions
2. Ajoutez vos secrets (par exemple, `CODE_SIGNING_CERTIFICATE`)
3. Utilisez-les dans votre workflow :

```yaml
steps:
  - name: Sign Application
    run: |
      echo "${{ secrets.CODE_SIGNING_CERTIFICATE }}" | base64 --decode > certificate.pfx
      signtool sign /f certificate.pfx /p "${{ secrets.CERTIFICATE_PASSWORD }}" /t http://timestamp.digicert.com Win32/Release/MonProjet.exe
      del certificate.pfx
```

### Mise en place de badges de statut dans votre README

Les badges de statut montrent l'état actuel de votre projet :

```markdown
# Mon Projet Delphi

![Build Status](https://github.com/votre-nom/votre-projet/actions/workflows/build.yml/badge.svg)
![Tests](https://github.com/votre-nom/votre-projet/actions/workflows/tests.yml/badge.svg)
![Release](https://img.shields.io/github/v/release/votre-nom/votre-projet)
```

### Automatisation des mises à jour de version

Vous pouvez automatiser les mises à jour de version dans votre projet Delphi :

```yaml
jobs:
  update-version:
    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Update version number
      run: |
        $versionFile = "src\Version.inc"
        $content = Get-Content $versionFile -Raw

        # Incrémenter le numéro de build
        $pattern = '(\{`\$DEFINE APP_VERSION_BUILD := )(\d+)(\})'
        $buildNumber = [int]$Matches[2] + 1
        $content = $content -replace $pattern, "`$1$buildNumber`$3"

        Set-Content -Path $versionFile -Value $content
      shell: pwsh

    - name: Commit changes
      run: |
        git config --local user.email "action@github.com"
        git config --local user.name "GitHub Action"
        git add src\Version.inc
        git commit -m "Mise à jour automatique du numéro de build"
        git push
```

## Partie 4 : Cas d'utilisation spécifiques à Delphi

### Configuration CI/CD pour les applications multi-plateformes FireMonkey

Pour les applications FireMonkey qui ciblent plusieurs plateformes :

```yaml
jobs:
  build:
    strategy:
      matrix:
        platform: [Win32, Win64, OSX64, Android, iOSDevice64]

    runs-on: windows-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Setup Delphi
      uses: gersonb/delphi-setup-action@v1.2.2
      with:
        version: '12.0'
        edition: 'community'

    - name: Setup Android SDK (if needed)
      if: matrix.platform == 'Android'
      run: |
        # Configuration Android SDK...

    - name: Setup iOS Certificates (if needed)
      if: matrix.platform == 'iOSDevice64'
      run: |
        # Configuration iOS certificates...

    - name: Build Project
      run: |
        msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=${{ matrix.platform }}
```

### Automatisation des mises à jour de packages GetIt

Si votre projet utilise des packages GetIt, vous pouvez automatiser leur installation :

```yaml
steps:
- name: Install GetIt Packages
  run: |
    # Créer un script d'installation des packages
    @echo SET BDS=C:\Program Files (x86)\Embarcadero\Studio\22.0 > install_packages.bat
    @echo "%%BDS%%\bin\rsvars.bat" >> install_packages.bat
    @echo "%%BDS%%\bin\bpm" install -g "REST Client Library" >> install_packages.bat
    @echo "%%BDS%%\bin\bpm" install -g "FMX Additional Components" >> install_packages.bat
    # Exécuter le script
    call install_packages.bat
```

### Génération automatique de documentation avec PasDoc

Pour générer automatiquement la documentation de votre code Delphi :

```yaml
jobs:
  documentation:
    runs-on: ubuntu-latest

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Install PasDoc
      run: |
        sudo apt-get update
        sudo apt-get install -y fpc
        git clone https://github.com/pasdoc/pasdoc.git
        cd pasdoc
        make

    - name: Generate Documentation
      run: |
        ./pasdoc/bin/pasdoc --title "Mon Projet Delphi" \
          --output docs/api \
          --format html \
          --source src/units/*.pas src/forms/*.pas

    - name: Deploy to GitHub Pages
      uses: JamesIves/github-pages-deploy-action@4.1.4
      with:
        branch: gh-pages
        folder: docs/api
```

## Bonnes pratiques CI/CD pour Delphi

1. **Gardez vos scripts de build simples et modulaires**
   - Créez des scripts batch ou PowerShell réutilisables pour les tâches communes

2. **Utilisez des environnements de build propres**
   - Assurez-vous que chaque build démarre dans un environnement frais sans dépendances cachées

3. **Automatisez les tests unitaires**
   - Utilisez DUnit ou DUnitX et configurez-les pour générer des rapports XML pour l'intégration CI

4. **Versionnez tout ce qui est nécessaire pour la build**
   - Y compris les scripts de build, les configurations, les ressources

5. **Testez vos workflows CI/CD localement**
   - Utilisez [act](https://github.com/nektos/act) pour tester les GitHub Actions localement

6. **Utilisez le cache pour accélérer les builds**
   - Mettez en cache les dépendances pour éviter de les télécharger à chaque fois

```yaml
steps:
- name: Cache Delphi packages
  uses: actions/cache@v3
  with:
    path: C:\Users\runneradmin\AppData\Roaming\Embarcadero\BDS\22.0\packages
    key: ${{ runner.os }}-delphi-packages
```

7. **Configurez des notifications**
   - Recevez des alertes en cas d'échec de build ou de tests

```yaml
- name: Send notification
  if: failure()
  uses: rtCamp/action-slack-notify@v2
  env:
    SLACK_WEBHOOK: ${{ secrets.SLACK_WEBHOOK }}
    SLACK_TITLE: "❌ Build Failed"
    SLACK_MESSAGE: "Le build a échoué. Consultez les logs pour plus de détails."
```

## Conclusion

L'intégration de Delphi avec Git et les systèmes CI/CD peut sembler complexe au premier abord, mais les avantages sont considérables, même pour les petits projets ou les développeurs individuels. Ces outils vous permettent d'automatiser les tâches répétitives, de détecter les problèmes tôt et d'améliorer la qualité globale de votre code.

Nous avons couvert les bases de l'intégration de Git avec Delphi, notamment la configuration d'un dépôt, la gestion des fichiers spécifiques à Delphi et l'utilisation des hooks Git pour automatiser certaines tâches. Nous avons également exploré la mise en place d'un système CI/CD avec GitHub Actions, qui peut automatiser la compilation, les tests et le déploiement de vos applications Delphi.

Commencez petit et ajoutez progressivement de l'automatisation à votre flux de travail. Même l'automatisation de quelques tâches de base comme la compilation et les tests peut faire une grande différence dans votre productivité et la qualité de votre code.

N'oubliez pas que l'objectif ultime est de vous libérer des tâches répétitives afin que vous puissiez vous concentrer sur ce qui est vraiment important : le développement de fonctionnalités et la résolution de problèmes métier pour vos utilisateurs.

## Ressources complémentaires

- [GitHub Actions pour Delphi](https://github.com/gersonb/delphi-setup-action) - Action pour configurer Delphi dans GitHub Actions
- [DelphiCI](https://github.com/project-como/delphi-ci) - Outils pour faciliter l'intégration continue avec Delphi
- [PasDoc](https://github.com/pasdoc/pasdoc) - Générateur de documentation pour Pascal/Delphi
- [Inno Setup](https://jrsoftware.org/isinfo.php) - Créateur d'installateurs pour Windows
- [GitHub Actions documentation](https://docs.github.com/en/actions) - Documentation officielle de GitHub Actions
