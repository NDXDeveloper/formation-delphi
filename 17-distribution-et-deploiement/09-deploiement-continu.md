🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.9 Déploiement continu (CI/CD)

## Introduction

Imaginez devoir effectuer manuellement ces tâches à chaque fois que vous modifiez votre code :

1. Compiler l'application pour Windows 32-bit
2. Compiler pour Windows 64-bit
3. Exécuter tous les tests unitaires
4. Créer l'installateur
5. Signer l'installateur numériquement
6. Télécharger sur votre serveur
7. Mettre à jour le fichier de version pour les mises à jour automatiques
8. Envoyer une notification à votre équipe

Si vous faites cela 5 fois par jour, vous perdez des heures de travail précieux. Et si vous oubliez une étape ? Un bug pourrait se glisser en production.

C'est là qu'intervient le **CI/CD** (Continuous Integration / Continuous Deployment), l'automatisation de tous ces processus. Le CI/CD transforme un processus manuel, lent et sujet aux erreurs en un flux automatique, rapide et fiable.

## Qu'est-ce que le CI/CD ?

### Définitions simples

**CI - Intégration Continue (Continuous Integration)** :
- Automatisation de la compilation et des tests
- Vérification automatique à chaque modification du code
- Détection rapide des problèmes

**CD - Déploiement Continu (Continuous Deployment)** :
- Automatisation de la livraison de l'application
- Déploiement automatique après validation
- Mise en production sans intervention manuelle

**Analogie** : C'est comme une usine automobile moderne. Au lieu de construire chaque voiture à la main (lent, erreurs possibles), une chaîne de montage automatisée construit rapidement et de manière identique chaque véhicule.

### Le flux CI/CD traditionnel

```
[1. Developer]
    ↓ (commit code)
[2. Version Control] (Git)
    ↓ (trigger)
[3. CI Server] (Compilation, Tests)
    ↓ (si succès)
[4. Build Artifacts] (Exécutables, Installateurs)
    ↓ (si succès)
[5. CD Pipeline] (Déploiement)
    ↓
[6. Production] (Serveurs, Store, Site Web)
    ↓
[7. Monitoring] (Surveillance)
```

### Avantages du CI/CD

#### 1. Gain de temps massif

**Sans CI/CD** : 30-60 minutes de travail manuel par déploiement

**Avec CI/CD** : 0 minutes de travail manuel, tout est automatique

Pour 10 déploiements par semaine, vous économisez **5-10 heures** !

#### 2. Réduction des erreurs

Les humains font des erreurs :
- Oublier de compiler pour une plateforme
- Oublier de lancer les tests
- Oublier de signer l'application
- Utiliser la mauvaise configuration

**CI/CD ne fait jamais d'erreur** : même processus à chaque fois.

#### 3. Détection précoce des bugs

Les tests automatiques détectent les problèmes **immédiatement** :
- Vous cassez quelque chose ? Vous le savez en 5 minutes
- Plus besoin d'attendre la fin du sprint pour tester
- Les bugs sont corrigés quand le code est frais dans votre esprit

#### 4. Déploiements fréquents et sûrs

Avec CI/CD, déployer devient **sans risque** :
- Déployez plusieurs fois par jour si nécessaire
- Chaque déploiement est testé automatiquement
- Retour en arrière facile si problème

#### 5. Collaboration améliorée

Toute l'équipe bénéficie :
- Tout le monde voit l'état du build en temps réel
- Pas de "ça marche sur ma machine" : même environnement pour tous
- Intégration continue des changements de chaque développeur

#### 6. Documentation automatique

Le pipeline CI/CD sert de **documentation vivante** :
- Comment compiler le projet ? Voir le pipeline
- Quelles dépendances ? Listées dans le pipeline
- Comment déployer ? Le pipeline le montre

## Concepts clés du CI/CD

### Build (Compilation)

Le **build** est la compilation de votre code source en exécutable.

**Build manuel** :
```
1. Ouvrir Delphi
2. Sélectionner la configuration Release
3. Compiler pour Win32
4. Compiler pour Win64
5. Vérifier les erreurs
```

**Build automatique** :
```yaml
# Le CI server fait tout automatiquement
build:
  - msbuild MonProjet.dproj /p:Config=Release /p:Platform=Win32
  - msbuild MonProjet.dproj /p:Config=Release /p:Platform=Win64
```

### Tests automatiques

Les **tests** vérifient que votre code fonctionne correctement.

Types de tests :
- **Tests unitaires** : Testent des fonctions individuelles
- **Tests d'intégration** : Testent que les modules fonctionnent ensemble
- **Tests de régression** : Vérifient qu'on n'a pas cassé quelque chose qui marchait

**Exemple de test unitaire Delphi avec DUnitX** :

```pascal
[Test]
procedure TCalculatorTests.TestAddition;  
begin  
  Assert.AreEqual(5, Calculator.Add(2, 3), 'Addition incorrecte');
end;
```

Dans le pipeline CI/CD, ces tests s'exécutent automatiquement.

### Artifacts (Artefacts)

Les **artifacts** sont les fichiers produits par le build :
- Exécutables (.exe)
- Installateurs (.msi, setup.exe)
- Fichiers de symboles pour le débogage
- Documentation générée

Le CI/CD **stocke ces artifacts** pour qu'ils soient disponibles au déploiement.

### Pipeline

Un **pipeline** est la séquence d'étapes automatisées :

```
Pipeline "Build et Test"
├── Stage 1: Compilation
│   ├── Job: Compiler Win32
│   └── Job: Compiler Win64
├── Stage 2: Tests
│   ├── Job: Tests unitaires
│   └── Job: Tests d'intégration
├── Stage 3: Package
│   └── Job: Créer installateur
└── Stage 4: Deploy
    └── Job: Uploader sur serveur
```

Chaque **stage** contient des **jobs** qui s'exécutent séquentiellement ou en parallèle.

### Environnements

Un **environnement** est une configuration de déploiement :

- **Development (Dev)** : Pour les développeurs, mis à jour constamment
- **Staging (Test)** : Environnement de pré-production, réplique de production
- **Production (Prod)** : L'application finale utilisée par les clients

Le pipeline peut déployer automatiquement vers différents environnements.

## Outils CI/CD populaires

### 1. GitLab CI/CD

**GitLab CI** est intégré dans GitLab (plateforme Git).

**Avantages** :
- Gratuit (version communautaire)
- Intégré dans GitLab
- Configuration simple (fichier YAML)
- Runners auto-hébergés ou cloud

**Inconvénients** :
- Nécessite GitLab (pas GitHub)
- Documentation parfois complexe

**Configuration** : Fichier `.gitlab-ci.yml` à la racine du projet

### 2. GitHub Actions

**GitHub Actions** est intégré dans GitHub.

**Avantages** :
- Intégré dans GitHub
- Marketplace d'actions réutilisables
- Gratuit pour les projets publics
- 2000 minutes/mois gratuites pour privés

**Inconvénients** :
- Coût potentiel pour gros projets privés
- Nécessite GitHub

**Configuration** : Fichiers YAML dans `.github/workflows/`

### 3. Azure DevOps (anciennement VSTS)

**Azure DevOps** de Microsoft offre une suite complète.

**Avantages** :
- Intégration Microsoft/Windows excellente
- Azure Pipelines très puissant
- Gratuit jusqu'à 5 utilisateurs
- Support natif MSBuild

**Inconvénients** :
- Interface parfois lourde
- Courbe d'apprentissage

**Configuration** : Fichier `azure-pipelines.yml` ou interface graphique

### 4. Jenkins

**Jenkins** est le système CI/CD open source historique.

**Avantages** :
- Totalement gratuit et open source
- Énormément de plugins
- Très flexible et personnalisable
- Auto-hébergé (contrôle total)

**Inconvénients** :
- Installation et maintenance nécessaires
- Interface ancienne
- Configuration parfois complexe

**Configuration** : Jenkinsfile ou interface web

### 5. TeamCity

**TeamCity** de JetBrains est un outil professionnel.

**Avantages** :
- Interface moderne et intuitive
- Excellent support Delphi possible
- Gratuit jusqu'à 3 agents
- Très stable

**Inconvénients** :
- Auto-hébergé uniquement
- Coût pour grande équipe

### Tableau comparatif

| Outil | Hébergement | Coût | Difficulté | Support Delphi |
|-------|-------------|------|------------|----------------|
| **GitLab CI** | Cloud/Self | Gratuit | Moyenne | Bon (via script) |
| **GitHub Actions** | Cloud | Gratuit* | Facile | Bon (via script) |
| **Azure DevOps** | Cloud | Gratuit* | Moyenne | Excellent (MSBuild natif) |
| **Jenkins** | Self | Gratuit | Difficile | Bon (plugins) |
| **TeamCity** | Self | Gratuit* | Facile | Bon |

*Gratuit avec limitations

**Recommandation pour débutants** :
- Si vous utilisez GitHub → **GitHub Actions**
- Si vous utilisez GitLab → **GitLab CI**
- Pour le meilleur support Windows → **Azure DevOps**

## Configuration CI/CD pour Delphi

### Prérequis

Pour automatiser la compilation Delphi, vous avez besoin de :

1. **Delphi installé** sur le serveur CI (ou runner)
2. **Licence ligne de commande** si utilisation en serveur
3. **Variables d'environnement** configurées correctement
4. **MSBuild** ou **rsvars.bat** accessible

### Compilation en ligne de commande

Delphi peut se compiler sans l'IDE :

```batch
REM Méthode 1 : Via rsvars.bat et msbuild  
call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"  
msbuild MonProjet.dproj /t:Build /p:Config=Release /p:Platform=Win32  

REM Méthode 2 : Via MSBuild directement
"C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\msbuild.exe" ^
  MonProjet.dproj ^
  /t:Build ^
  /p:Config=Release ^
  /p:Platform=Win64
```

**Options importantes** :

- `/t:Build` : Compiler le projet
- `/t:Clean` : Nettoyer avant compilation
- `/t:Rebuild` : Nettoyer puis compiler
- `/p:Config=Release` : Configuration Release
- `/p:Platform=Win32` : Plateforme cible
- `/verbosity:detailed` : Logs détaillés

## Exemple : Pipeline GitLab CI pour Delphi

### Fichier .gitlab-ci.yml

```yaml
# Définir les stages du pipeline
stages:
  - build
  - test
  - package
  - deploy

# Variables globales
variables:
  DELPHI_PATH: "C:\\Program Files (x86)\\Embarcadero\\Studio\\23.0"
  PROJECT_NAME: "MonApplication"

# Stage 1 : Compilation
build_win32:
  stage: build
  tags:
    - windows
    - delphi
  script:
    # Activer l'environnement Delphi
    - call "%DELPHI_PATH%\\bin\\rsvars.bat"

    # Compiler pour Win32
    - msbuild %PROJECT_NAME%.dproj /t:Rebuild /p:Config=Release /p:Platform=Win32

    # Vérifier que l'exe existe
    - if not exist "Win32\\Release\\%PROJECT_NAME%.exe" exit 1
  artifacts:
    paths:
      - Win32/Release/*.exe
      - Win32/Release/*.dll
    expire_in: 1 week

build_win64:
  stage: build
  tags:
    - windows
    - delphi
  script:
    - call "%DELPHI_PATH%\\bin\\rsvars.bat"
    - msbuild %PROJECT_NAME%.dproj /t:Rebuild /p:Config=Release /p:Platform=Win64
    - if not exist "Win64\\Release\\%PROJECT_NAME%.exe" exit 1
  artifacts:
    paths:
      - Win64/Release/*.exe
      - Win64/Release/*.dll
    expire_in: 1 week

# Stage 2 : Tests
test_unit:
  stage: test
  tags:
    - windows
    - delphi
  dependencies:
    - build_win64
  script:
    # Compiler les tests
    - call "%DELPHI_PATH%\\bin\\rsvars.bat"
    - msbuild Tests\\TestProject.dproj /t:Build /p:Config=Release

    # Exécuter les tests avec DUnitX
    - Tests\\Win64\\Release\\TestProject.exe -xml:test-results.xml

    # Vérifier le code de retour
    - if %errorlevel% neq 0 exit 1
  artifacts:
    reports:
      junit: test-results.xml
    when: always

# Stage 3 : Création du package
package_installer:
  stage: package
  tags:
    - windows
    - innosetup
  dependencies:
    - build_win32
    - build_win64
  script:
    # Compiler l'installateur avec Inno Setup
    - "C:\\Program Files (x86)\\Inno Setup 6\\ISCC.exe" Setup\\MonApp.iss

    # Signer l'installateur
    - signtool sign /f Certificate.pfx /p %CERT_PASSWORD% /t http://timestamp.digicert.com Output\\MonApp_Setup.exe
  artifacts:
    paths:
      - Output/MonApp_Setup.exe
    expire_in: 1 month

# Stage 4 : Déploiement
deploy_staging:
  stage: deploy
  tags:
    - windows
  dependencies:
    - package_installer
  environment:
    name: staging
    url: https://staging.monapp.com
  script:
    # Uploader vers le serveur staging
    - scp Output/MonApp_Setup.exe user@staging.monapp.com:/var/www/downloads/

    # Mettre à jour le fichier version.json
    - curl -X POST https://staging.monapp.com/api/update-version -d "version=1.0.%CI_PIPELINE_ID%"
  only:
    - develop

deploy_production:
  stage: deploy
  tags:
    - windows
  dependencies:
    - package_installer
  environment:
    name: production
    url: https://www.monapp.com
  script:
    # Uploader vers le serveur production
    - scp Output/MonApp_Setup.exe user@www.monapp.com:/var/www/downloads/
    - curl -X POST https://www.monapp.com/api/update-version -d "version=1.0.%CI_PIPELINE_ID%"
  only:
    - main
  when: manual  # Déploiement manuel en production
```

**Explication du pipeline** :

1. **build_win32 / build_win64** : Compile pour les deux plateformes en parallèle
2. **test_unit** : Exécute les tests unitaires (dépend du build Win64)
3. **package_installer** : Crée et signe l'installateur
4. **deploy_staging** : Déploie automatiquement sur staging (branche develop)
5. **deploy_production** : Déploie sur production (branche main, manuel)

### Configuration du Runner GitLab

Un **runner** est la machine qui exécute les jobs.

**Installation sur Windows** :

1. **Télécharger le runner** :
   - https://docs.gitlab.com/runner/install/windows.html

2. **Installer** :
   ```cmd
   gitlab-runner.exe install
   gitlab-runner.exe start
   ```

3. **Enregistrer avec GitLab** :
   ```cmd
   gitlab-runner.exe register
   ```
   - URL du GitLab
   - Token du projet (dans Settings → CI/CD → Runners)
   - Tags : `windows`, `delphi`
   - Executor : `shell`

4. **Configurer l'environnement** :
   - Installer Delphi sur la machine runner
   - Configurer les variables d'environnement
   - Installer les outils nécessaires (Inno Setup, signtool, etc.)

## Exemple : Pipeline GitHub Actions pour Delphi

### Fichier .github/workflows/build.yml

```yaml
name: Build and Deploy

# Déclencher sur push vers main ou develop
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  # Job 1 : Compilation
  build:
    name: Build Delphi Application
    runs-on: windows-latest

    steps:
    # Récupérer le code
    - name: Checkout code
      uses: actions/checkout@v3

    # Installer Delphi (via cache ou installation)
    # Note: Nécessite une licence
    - name: Setup Delphi
      run: |
        # Script d'installation/configuration de Delphi
        # À adapter selon votre méthode

    # Compiler Win32
    - name: Build Win32
      run: |
        call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
        msbuild MonApplication.dproj /t:Rebuild /p:Config=Release /p:Platform=Win32
      shell: cmd

    # Compiler Win64
    - name: Build Win64
      run: |
        call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
        msbuild MonApplication.dproj /t:Rebuild /p:Config=Release /p:Platform=Win64
      shell: cmd

    # Uploader les artifacts
    - name: Upload Win32 Artifact
      uses: actions/upload-artifact@v3
      with:
        name: MonApp-Win32
        path: Win32/Release/MonApplication.exe

    - name: Upload Win64 Artifact
      uses: actions/upload-artifact@v3
      with:
        name: MonApp-Win64
        path: Win64/Release/MonApplication.exe

  # Job 2 : Tests
  test:
    name: Run Tests
    runs-on: windows-latest
    needs: build

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Download artifacts
      uses: actions/download-artifact@v3
      with:
        name: MonApp-Win64

    - name: Run Unit Tests
      run: |
        # Compiler et exécuter les tests DUnitX
        call "C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat"
        msbuild Tests\TestProject.dproj /t:Build /p:Config=Release
        Tests\Win64\Release\TestProject.exe -xml:test-results.xml
      shell: cmd

    - name: Publish Test Results
      uses: EnricoMi/publish-unit-test-result-action/composite@v2
      if: always()
      with:
        files: test-results.xml

  # Job 3 : Création de l'installateur
  package:
    name: Create Installer
    runs-on: windows-latest
    needs: [build, test]

    steps:
    - name: Checkout code
      uses: actions/checkout@v3

    - name: Download artifacts
      uses: actions/download-artifact@v3

    - name: Install Inno Setup
      run: choco install innosetup -y

    - name: Build Installer
      run: |
        "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" Setup\MonApp.iss
      shell: cmd

    - name: Sign Installer
      env:
        CERT_PASSWORD: ${{ secrets.CERT_PASSWORD }}
      run: |
        signtool sign /f Certificate.pfx /p %CERT_PASSWORD% /t http://timestamp.digicert.com Output\MonApp_Setup.exe
      shell: cmd

    - name: Upload Installer
      uses: actions/upload-artifact@v3
      with:
        name: MonApp-Installer
        path: Output/MonApp_Setup.exe

  # Job 4 : Déploiement
  deploy:
    name: Deploy to Production
    runs-on: windows-latest
    needs: package
    if: github.ref == 'refs/heads/main'
    environment:
      name: production
      url: https://www.monapp.com

    steps:
    - name: Download Installer
      uses: actions/download-artifact@v3
      with:
        name: MonApp-Installer

    - name: Deploy to Server
      env:
        SSH_KEY: ${{ secrets.SSH_KEY }}
      run: |
        # Upload via SCP ou FTP
        scp -i ssh_key MonApp_Setup.exe user@www.monapp.com:/var/www/downloads/

    - name: Create GitHub Release
      uses: actions/create-release@v1
      env:
        GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
      with:
        tag_name: v${{ github.run_number }}
        release_name: Release v${{ github.run_number }}
        draft: false
        prerelease: false
```

### Secrets GitHub

Pour stocker les informations sensibles (mots de passe, clés SSH) :

1. **Repository → Settings → Secrets and variables → Actions**
2. **New repository secret**
3. Ajouter :
   - `CERT_PASSWORD` : Mot de passe du certificat
   - `SSH_KEY` : Clé SSH pour le déploiement
   - `DEPLOY_TOKEN` : Token d'authentification

Les secrets sont chiffrés et jamais affichés dans les logs.

## Exemple : Pipeline Azure DevOps pour Delphi

### Fichier azure-pipelines.yml

```yaml
# Pipeline Azure pour Delphi
trigger:
  branches:
    include:
      - main
      - develop

pool:
  vmImage: 'windows-latest'

variables:
  solution: '**/*.dproj'
  buildPlatform: 'Win32|Win64'
  buildConfiguration: 'Release'
  delphiPath: 'C:\Program Files (x86)\Embarcadero\Studio\23.0'

stages:
- stage: Build
  displayName: 'Build Application'
  jobs:
  - job: BuildJob
    displayName: 'Compile Delphi Project'
    steps:

    # Récupérer le code
    - checkout: self
      clean: true

    # Installer Delphi (ou utiliser agent pré-configuré)
    - task: PowerShell@2
      displayName: 'Setup Delphi Environment'
      inputs:
        targetType: 'inline'
        script: |
          & "$(delphiPath)\bin\rsvars.bat"

    # Compiler Win32
    - task: MSBuild@1
      displayName: 'Build Win32'
      inputs:
        solution: '$(solution)'
        platform: 'Win32'
        configuration: '$(buildConfiguration)'
        msbuildArguments: '/t:Rebuild'

    # Compiler Win64
    - task: MSBuild@1
      displayName: 'Build Win64'
      inputs:
        solution: '$(solution)'
        platform: 'Win64'
        configuration: '$(buildConfiguration)'
        msbuildArguments: '/t:Rebuild'

    # Publier les artifacts
    - task: PublishBuildArtifacts@1
      displayName: 'Publish Win32 Artifacts'
      inputs:
        PathtoPublish: 'Win32\Release'
        ArtifactName: 'Win32-Build'

    - task: PublishBuildArtifacts@1
      displayName: 'Publish Win64 Artifacts'
      inputs:
        PathtoPublish: 'Win64\Release'
        ArtifactName: 'Win64-Build'

- stage: Test
  displayName: 'Run Tests'
  dependsOn: Build
  jobs:
  - job: TestJob
    displayName: 'Execute Unit Tests'
    steps:

    - task: DownloadBuildArtifacts@0
      inputs:
        artifactName: 'Win64-Build'

    - task: PowerShell@2
      displayName: 'Run DUnitX Tests'
      inputs:
        targetType: 'inline'
        script: |
          # Exécuter les tests
          & "Tests\Win64\Release\TestProject.exe" -xml:test-results.xml

    - task: PublishTestResults@2
      displayName: 'Publish Test Results'
      inputs:
        testResultsFormat: 'JUnit'
        testResultsFiles: '**/test-results.xml'
        failTaskOnFailedTests: true

- stage: Package
  displayName: 'Create Installer'
  dependsOn: Test
  jobs:
  - job: PackageJob
    steps:

    - task: DownloadBuildArtifacts@0
      inputs:
        artifactName: 'Win32-Build'

    - task: DownloadBuildArtifacts@0
      inputs:
        artifactName: 'Win64-Build'

    # Créer l'installateur avec Inno Setup
    - task: PowerShell@2
      displayName: 'Build Installer'
      inputs:
        targetType: 'inline'
        script: |
          & "C:\Program Files (x86)\Inno Setup 6\ISCC.exe" Setup\MonApp.iss

    # Signer l'installateur
    - task: PowerShell@2
      displayName: 'Sign Installer'
      inputs:
        targetType: 'inline'
        script: |
          signtool sign /f Certificate.pfx /p $(CertPassword) /t http://timestamp.digicert.com Output\MonApp_Setup.exe

    - task: PublishBuildArtifacts@1
      inputs:
        PathtoPublish: 'Output'
        ArtifactName: 'Installer'

- stage: Deploy
  displayName: 'Deploy to Production'
  dependsOn: Package
  condition: and(succeeded(), eq(variables['Build.SourceBranch'], 'refs/heads/main'))
  jobs:
  - deployment: DeployJob
    displayName: 'Deploy Application'
    environment: 'Production'
    strategy:
      runOnce:
        deploy:
          steps:
          - task: DownloadBuildArtifacts@0
            inputs:
              artifactName: 'Installer'

          # Déployer sur le serveur
          - task: PowerShell@2
            displayName: 'Upload to Server'
            inputs:
              targetType: 'inline'
              script: |
                # Upload via SCP/SFTP
                scp $(System.ArtifactsDirectory)\Installer\MonApp_Setup.exe user@server:/path/
```

**Avantages Azure DevOps pour Delphi** :
- MSBuild natif et bien supporté
- Interface visuelle claire
- Gestion des releases sophistiquée
- Intégration Azure Cloud

## Bonnes pratiques CI/CD

### 1. Compiler sur une machine propre

❌ **Mauvais** : Compiler sur votre machine de développement
- Dépendances non documentées
- "Ça marche chez moi" mais pas ailleurs

✅ **Bon** : Utiliser un runner/agent dédié
- Environnement contrôlé et reproductible
- Détecte les dépendances manquantes

### 2. Versionner tout

Versionnez dans Git :
- ✅ Code source
- ✅ Scripts de build
- ✅ Configuration CI/CD
- ✅ Scripts de déploiement
- ✅ Documentation
- ❌ Pas les binaires compilés
- ❌ Pas les secrets (mots de passe, clés)

### 3. Builds rapides

Optimisez la vitesse du pipeline :
- **Utilisez le cache** : Dépendances, fichiers intermédiaires
- **Parallélisez** : Win32 et Win64 en même temps
- **Incrémental** : Ne recompilez que ce qui a changé
- **Machines rapides** : CPU puissant, SSD

**Objectif** : Build complet en moins de 10 minutes

### 4. Tests automatiques obligatoires

Le pipeline doit **échouer si les tests échouent** :

```yaml
test:
  script:
    - TestProject.exe -xml:results.xml
    - if %errorlevel% neq 0 exit 1  # Fail si tests ratés
```

Cela empêche le code buggé d'arriver en production.

### 5. Environnements multiples

Utilisez au minimum 3 environnements :
- **Dev** : Déploiement automatique à chaque commit
- **Staging** : Réplique de production pour tests finaux
- **Production** : L'application finale, déploiement contrôlé

### 6. Déploiement progressif

Ne déployez pas tout d'un coup en production :

```
Phase 1 : 10% des serveurs/utilisateurs
  ↓ (surveiller 24h)
Phase 2 : 50% des serveurs/utilisateurs
  ↓ (surveiller 24h)
Phase 3 : 100% des serveurs/utilisateurs
```

Cela limite l'impact d'un bug critique.

### 7. Rollback facile

Gardez toujours un moyen de revenir en arrière rapidement :
- Tags Git pour chaque version
- Artifacts conservés pendant 30 jours minimum
- Script de rollback testé

```yaml
rollback_production:
  when: manual
  script:
    - deploy_version.sh v1.2.0  # Version précédente stable
```

### 8. Notifications

Configurez des notifications pour :
- ✅ Build réussi (optionnel, pas trop verbeux)
- ❌ Build échoué (obligatoire, alerte l'équipe)
- 🚀 Déploiement production (obligatoire)

**Canaux** : Email, Slack, Microsoft Teams, Discord

### 9. Sécurité

- **Secrets** : Jamais dans le code, toujours dans des variables sécurisées
- **Scans** : Analyse de sécurité automatique (vulnérabilités, malware)
- **Accès** : Limiter qui peut déployer en production

### 10. Documentation

Documentez votre pipeline :
- README expliquant le processus
- Diagramme du flux CI/CD
- Procédure de rollback
- Contact en cas de problème

## Problèmes courants et solutions

### "Delphi not found" ou "rsvars.bat not found"

**Cause** : Delphi pas installé ou chemin incorrect

**Solution** :
```yaml
# Vérifier le chemin et le mettre en variable
variables:
  DELPHI_PATH: "C:\\Program Files (x86)\\Embarcadero\\Studio\\23.0"

script:
  - if not exist "%DELPHI_PATH%\\bin\\rsvars.bat" (
      echo Delphi not found! && exit 1
    )
```

### "License error" lors de la compilation

**Cause** : Pas de licence ligne de commande

**Solution** :
- Acheter une licence "Command Line" auprès d'Embarcadero
- Ou utiliser la licence desktop (selon les termes)
- Configurer la variable d'environnement de licence

### Tests échouent en CI mais passent localement

**Causes** :
- Différences d'environnement
- Chemins de fichiers absolus dans les tests
- Timing différent (tests trop rapides ou trop lents)

**Solutions** :
```pascal
// ✗ Mauvais : Chemin absolu
TestFile := 'C:\Users\moi\Documents\test.txt';

// ✓ Bon : Chemin relatif
TestFile := TPath.Combine(TPath.GetCurrentPath, 'TestData\test.txt');

// ✓ Bon : Attentes avec timeout
Assert.IsTrue(WaitForCondition(1000), 'Timeout après 1 seconde');
```

### Build trop lent (>30 minutes)

**Solutions** :
- **Cache** : Mettez en cache les dépendances
- **Parallélisation** : Compilez Win32 et Win64 simultanément
- **Compilation incrémentale** : Ne recompilez pas tout
- **Machine plus rapide** : Plus de CPU/RAM

```yaml
# Exemple de cache GitLab CI
cache:
  paths:
    - __history/
    - Win32/Release/*.dcu
    - Win64/Release/*.dcu
```

### Artifacts trop volumineux

**Problème** : Les artifacts occupent trop d'espace

**Solution** : Ne gardez que l'essentiel
```yaml
artifacts:
  paths:
    - Win64/Release/*.exe  # ✓ Exécutables
    - Win64/Release/*.dll  # ✓ DLL nécessaires
    # ✗ Pas les .dcu, .obj, etc.
  expire_in: 1 week  # ✓ Expiration automatique
```

### Pipeline échoue de manière intermittente

**Causes** :
- Tests non déterministes (dépendent du timing)
- Ressources externes indisponibles
- Problèmes réseau

**Solutions** :
- **Retry automatique** :
```yaml
retry:
  max: 2
  when: script_failure
```
- Tests déterministes uniquement
- Mocker les services externes

## Surveillance et monitoring après déploiement

Le CI/CD ne s'arrête pas au déploiement. Surveillez :

### Métriques à suivre

1. **Taux de succès des builds** : Doit être >90%
2. **Temps de build** : Surveiller les ralentissements
3. **Couverture de code** : Tests couvrent combien du code ?
4. **Fréquence de déploiement** : Combien par semaine ?
5. **MTTR** (Mean Time To Recovery) : Temps pour corriger un problème

### Outils de monitoring

- **Application Insights** (Azure) : Télémétrie d'application
- **Sentry** : Surveillance des erreurs
- **ELK Stack** : Logs centralisés
- **Grafana** : Dashboards de métriques

### Alertes

Configurez des alertes pour :
- 🚨 Taux d'erreur >5%
- 🚨 Build échoué sur branche main
- ⚠️ Temps de réponse >2 secondes
- ⚠️ Utilisation mémoire >80%

## Roadmap d'adoption CI/CD

Si vous débutez avec CI/CD, progressez par étapes :

### Phase 1 : Compilation automatique (Semaine 1-2)

```
Objectif : Build automatique à chaque commit
- Installer un runner/agent
- Créer pipeline minimal (build uniquement)
- Corriger les erreurs de compilation
```

### Phase 2 : Tests automatiques (Semaine 3-4)

```
Objectif : Tests unitaires automatiques
- Écrire premiers tests DUnitX
- Intégrer tests dans pipeline
- Configurer échec si tests ratés
```

### Phase 3 : Artifacts et packaging (Semaine 5-6)

```
Objectif : Créer installateurs automatiquement
- Automatiser Inno Setup dans pipeline
- Signature automatique
- Stocker artifacts
```

### Phase 4 : Déploiement automatique (Semaine 7-8)

```
Objectif : Déployer vers environnement de test
- Configurer environnement staging
- Automatiser upload
- Tester le déploiement
```

### Phase 5 : Production et monitoring (Semaine 9+)

```
Objectif : Déploiement production avec surveillance
- Déploiement production (manuel d'abord)
- Configurer monitoring
- Mettre en place rollback
- Progressivement automatiser production
```

## Coûts du CI/CD

### Coûts directs

| Service | Coût gratuit | Coût payant |
|---------|--------------|-------------|
| **GitLab CI** | Illimité (self-hosted) / 400 min/mois (cloud) | 10$/utilisateur/mois |
| **GitHub Actions** | 2000 min/mois | 0.008$/min au-delà |
| **Azure DevOps** | 1800 min/mois | 40$/agent parallèle/mois |
| **Jenkins** | Gratuit (self-hosted) | Coût serveur uniquement |

### Coûts indirects

- **Serveur/Agent** : ~50-200$/mois selon configuration
- **Stockage artifacts** : ~10-50$/mois
- **Temps d'apprentissage** : 20-40 heures initialement
- **Maintenance** : ~2-4 heures/mois

### ROI (Retour sur Investissement)

**Sans CI/CD** :
- 1h manuelle par déploiement
- 10 déploiements/semaine = 10h/semaine = 40h/mois
- À 50€/h = **2000€/mois de temps perdu**

**Avec CI/CD** :
- Coût : ~100€/mois
- Temps économisé : 40h/mois = 2000€
- **ROI : 1900€/mois = 22 800€/an**

Sans compter les avantages indirects (moins de bugs, déploiements plus fréquents, meilleure qualité).

## Checklist de mise en place CI/CD

Avant de lancer votre CI/CD :

- [ ] Code dans un système de contrôle de version (Git)
- [ ] Compilation en ligne de commande fonctionne
- [ ] Tests unitaires existent (DUnitX recommandé)
- [ ] Runner/Agent installé et configuré
- [ ] Delphi installé sur le runner
- [ ] Variables d'environnement configurées
- [ ] Pipeline de base créé (.gitlab-ci.yml, .yml GitHub, etc.)
- [ ] Build automatique fonctionne
- [ ] Tests automatiques fonctionnent
- [ ] Artifacts sauvegardés correctement
- [ ] Notifications configurées
- [ ] Secrets stockés de manière sécurisée
- [ ] Documentation du pipeline rédigée
- [ ] Procédure de rollback définie
- [ ] Équipe formée au nouveau processus

## Conclusion

Le CI/CD transforme radicalement la façon dont vous développez et déployez vos applications Delphi. Ce qui prenait des heures manuellement devient automatique et fiable.

**Points clés à retenir** :

1. **CI/CD = Automatisation** : Compilation, tests, packaging, déploiement
2. **Gains massifs** : Temps, qualité, fréquence de livraison
3. **Outils variés** : GitLab CI, GitHub Actions, Azure DevOps, Jenkins
4. **Progression par étapes** : Commencez simple, évoluez progressivement
5. **Tests automatiques** : Essentiels pour un CI/CD efficace
6. **Environnements multiples** : Dev, Staging, Production
7. **Sécurité** : Secrets protégés, accès contrôlés
8. **ROI élevé** : Investissement rentabilisé en quelques mois

Avec un bon pipeline CI/CD, vous pouvez :
- Déployer plusieurs fois par jour sans stress
- Détecter les bugs en quelques minutes
- Revenir en arrière en cas de problème
- Gagner des dizaines d'heures par mois
- Améliorer la qualité de vos applications

Le CI/CD n'est plus un luxe réservé aux grandes entreprises, c'est devenu un standard de l'industrie. Et avec Delphi, c'est tout à fait accessible ! Dans la dernière section de ce chapitre, nous explorerons la télémétrie et l'analyse de crash pour surveiller vos applications en production.

⏭️ [Télémétrie et analyse de crash](/17-distribution-et-deploiement/10-telemetrie-et-analyse-de-crash.md)
