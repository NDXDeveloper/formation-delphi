# 17.9 Déploiement continu (CI/CD)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Imaginez que vous venez d'ajouter une nouvelle fonctionnalité à votre application Delphi. Après avoir effectué vos tests locaux, vous devez maintenant compiler l'application, créer un installateur, effectuer des tests supplémentaires et finalement la déployer pour vos utilisateurs. Et si tout ce processus pouvait être automatisé ? C'est exactement ce que permet l'intégration continue et le déploiement continu (CI/CD).

Dans ce chapitre, nous allons découvrir comment mettre en place un pipeline CI/CD pour vos applications Delphi, afin d'automatiser la compilation, les tests et le déploiement de vos logiciels. Même si ces concepts peuvent sembler techniques au premier abord, nous les aborderons étape par étape, avec des exemples concrets adaptés aux débutants.

## Qu'est-ce que le CI/CD ?

Avant de plonger dans l'implémentation, clarifions ces termes :

### Intégration Continue (CI)

L'**Intégration Continue** consiste à fusionner fréquemment les modifications de code dans un dépôt central, puis à exécuter automatiquement des tests pour vérifier que ces modifications n'introduisent pas de problèmes. Pour une application Delphi, cela implique généralement :

- La compilation automatique du projet
- L'exécution de tests unitaires
- La vérification de la qualité du code

### Déploiement Continu (CD)

Le **Déploiement Continu** va plus loin en automatisant la livraison de l'application aux utilisateurs. Cela peut inclure :

- La création d'installateurs (EXE, MSI, MSIX)
- La publication sur des canaux de distribution (site web, Windows Store)
- La notification aux utilisateurs des nouvelles versions

![Schéma du processus CI/CD](https://placeholder-image.com/cicd-workflow.png)

## Avantages du CI/CD pour le développement Delphi

L'adoption d'un pipeline CI/CD pour vos projets Delphi offre de nombreux avantages :

1. **Détection précoce des problèmes** : Les erreurs sont identifiées rapidement après leur introduction
2. **Cohérence** : Chaque version est construite de la même manière, éliminant les problèmes liés à "l'environnement de développement"
3. **Gain de temps** : Les tâches répétitives sont automatisées
4. **Meilleure qualité** : Les tests systématiques améliorent la fiabilité du code
5. **Déploiement plus fréquent** : Possibilité de livrer des mises à jour plus régulièrement
6. **Feedback plus rapide** : Les utilisateurs peuvent tester les nouvelles fonctionnalités plus tôt

## Outils CI/CD compatibles avec Delphi

Plusieurs outils CI/CD peuvent être utilisés avec Delphi. Voici les plus populaires :

### 1. Jenkins

[Jenkins](https://jenkins.io/) est une plateforme d'automatisation open-source très flexible.

**Points forts** :
- Gratuit et open-source
- Hautement personnalisable
- Grande communauté et nombreux plugins
- Peut être hébergé sur votre propre serveur

**Points faibles** :
- Configuration initiale complexe
- Interface utilisateur moins moderne

### 2. GitHub Actions

[GitHub Actions](https://github.com/features/actions) est intégré directement à GitHub et permet d'automatiser des workflows directement depuis votre dépôt.

**Points forts** :
- Intégration parfaite avec GitHub
- Configuration par fichiers YAML simples
- Minutes gratuites généreuses pour les projets publics
- Interface moderne et intuitive

**Points faibles** :
- Limité aux dépôts GitHub
- Peut devenir coûteux pour les projets privés volumineux

### 3. Azure DevOps

[Azure DevOps](https://azure.microsoft.com/services/devops/) (anciennement VSTS) est la solution de Microsoft pour le CI/CD.

**Points forts** :
- Bonne intégration avec les produits Microsoft
- Interface utilisateur intuitive
- Offre gratuite généreuse (2000 minutes/mois)
- Fonctionnalités complètes de gestion de projet

**Points faibles** :
- Courbe d'apprentissage initiale
- Peut devenir complexe pour les grands projets

### 4. GitLab CI/CD

[GitLab CI/CD](https://docs.gitlab.com/ee/ci/) est intégré à la plateforme GitLab.

**Points forts** :
- Intégration native avec GitLab
- Configuration simple par fichier YAML
- Version communautaire gratuite disponible
- Documentation complète

**Points faibles** :
- Moins de runners (exécuteurs) disponibles pour Windows

### 5. TeamCity

[TeamCity](https://www.jetbrains.com/teamcity/) de JetBrains est particulièrement apprécié pour sa facilité d'utilisation.

**Points forts** :
- Interface utilisateur intuitive
- Bonne prise en charge de Delphi
- License gratuite pour les petits projets
- Détection intelligente des problèmes

**Points faibles** :
- Version complète payante
- Ressources système importantes requises

## Configuration d'un pipeline CI/CD pour Delphi

Dans ce tutoriel, nous allons utiliser **GitHub Actions** pour notre exemple, car :
- C'est accessible gratuitement
- La configuration est relativement simple
- Les concepts s'appliquent facilement à d'autres outils

### Prérequis

Pour suivre ce tutoriel, vous aurez besoin de :

1. Un compte [GitHub](https://github.com/)
2. Un projet Delphi versionné avec Git
3. Une licence Delphi valide (nous verrons comment l'utiliser dans un environnement CI/CD)

### Étape 1 : Préparation de votre projet Delphi

Avant de configurer le CI/CD, assurez-vous que votre projet Delphi peut être compilé en ligne de commande :

1. Ouvrez une invite de commande
2. Naviguez vers le dossier d'installation de Delphi
3. Testez la compilation en ligne de commande :

```batch
"C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
msbuild "C:\Chemin\Vers\Votre\Projet\MonProjet.dproj" /t:Build /p:Config=Release /p:Platform=Win32
```

Si cette commande fonctionne et compile votre projet, vous êtes prêt pour le CI/CD.

### Étape 2 : Configuration de GitHub Actions

1. Dans votre dépôt GitHub, cliquez sur l'onglet "Actions"
2. Cliquez sur "Set up a workflow yourself" (Configurer un workflow vous-même)
3. GitHub créera un fichier `.github/workflows/main.yml`
4. Remplacez le contenu par l'exemple de base suivant :

```yaml
name: Delphi CI/CD

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  workflow_dispatch:  # Permet de déclencher le workflow manuellement

jobs:
  build:
    runs-on: windows-latest

    steps:
    - name: Checkout du code
      uses: actions/checkout@v2

    - name: Configuration de Delphi
      run: |
        # Nous verrons cette partie dans l'étape suivante

    - name: Compilation du projet
      run: |
        # Nous verrons cette partie dans l'étape suivante

    - name: Tests unitaires
      run: |
        # Nous verrons cette partie dans l'étape suivante

    - name: Création de l'installateur
      run: |
        # Nous verrons cette partie dans l'étape suivante

    - name: Publication des artefacts
      uses: actions/upload-artifact@v2
      with:
        name: mon-application
        path: |
          # Nous verrons cette partie dans l'étape suivante
```

### Étape 3 : Installation automatique de Delphi

L'un des défis du CI/CD avec Delphi est l'installation du compilateur sur l'agent de build. Voici deux approches :

#### Option 1 : Utilisation d'une image Docker préinstallée

Cette option utilise une image Docker avec Delphi déjà installé :

```yaml
- name: Configuration de Delphi avec Docker
  run: |
    docker pull delphicontainers/delphi-xe10.4:latest
    docker run --name delphi-builder -v ${GITHUB_WORKSPACE}:/project delphicontainers/delphi-xe10.4:latest
```

#### Option 2 : Installation silencieuse de Delphi

Cette option télécharge et installe Delphi en mode silencieux (nécessite votre fichier de licence) :

```yaml
- name: Téléchargement et installation de Delphi
  run: |
    # Créer un dossier pour l'installateur
    mkdir C:\DelphiInstaller

    # Télécharger l'installateur (remplacez l'URL par un lien valide)
    curl -L "https://votre-url-de-stockage/Delphi_11_Alexandria_Setup.exe" -o "C:\DelphiInstaller\Setup.exe"

    # Créer un fichier de réponses pour l'installation silencieuse
    echo "[InstallParams]" > C:\DelphiInstaller\response.txt
    echo "LicenseFile=C:\DelphiInstaller\license.slip" >> C:\DelphiInstaller\response.txt
    echo "IgnoreLatestUpdate=1" >> C:\DelphiInstaller\response.txt
    echo "AutoSelectPlatforms=1" >> C:\DelphiInstaller\response.txt

    # Décoder votre fichier de licence depuis les secrets GitHub
    echo "${{ secrets.DELPHI_LICENSE }}" > C:\DelphiInstaller\license.slip

    # Exécuter l'installation silencieuse
    C:\DelphiInstaller\Setup.exe -q -I"C:\DelphiInstaller\response.txt"
```

Pour cette approche, vous devez ajouter votre fichier de licence comme secret GitHub :
1. Allez dans les paramètres de votre dépôt
2. Cliquez sur "Secrets and variables" > "Actions"
3. Ajoutez un nouveau secret nommé "DELPHI_LICENSE" avec le contenu de votre fichier de licence

### Étape 4 : Compilation de votre projet Delphi

Une fois Delphi configuré, ajoutez la compilation de votre projet :

```yaml
- name: Compilation du projet
  run: |
    # Initialiser les variables d'environnement Delphi
    call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"

    # Compiler le projet
    msbuild "MonProjet.dproj" /t:Build /p:Config=Release /p:Platform=Win32
```

### Étape 5 : Exécution des tests unitaires

Si votre projet inclut des tests unitaires (avec DUnit, DUnitX ou TestInsight), ajoutez leur exécution :

```yaml
- name: Tests unitaires
  run: |
    cd Win32\Release
    # Exécuter les tests unitaires
    MonProjetTests.exe -xml:tests-results.xml

    # Option : Publier les résultats des tests
    - name: Publier les résultats de test
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: Tests DUnit
        path: Win32\Release\tests-results.xml
        reporter: java-junit
```

### Étape 6 : Création d'un installateur

Automatisez la création de votre installateur (Inno Setup, MSI, etc.) :

```yaml
- name: Installation d'Inno Setup
  run: |
    choco install innosetup -y

- name: Création de l'installateur
  run: |
    cd Scripts
    "C:\Program Files (x86)\Inno Setup 6\iscc.exe" MonProjetSetup.iss
```

### Étape 7 : Publication des artefacts

Enfin, publiez les fichiers générés comme artefacts de build :

```yaml
- name: Publication des artefacts
  uses: actions/upload-artifact@v2
  with:
    name: mon-application
    path: |
      Output/MonProjet_Setup.exe
      Win32/Release/MonProjet.exe
```

### Workflow GitHub Actions complet

Voici à quoi ressemblerait un workflow complet :

```yaml
name: Delphi CI/CD

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]
  workflow_dispatch:

jobs:
  build:
    runs-on: windows-latest

    steps:
    - name: Checkout du code
      uses: actions/checkout@v2

    - name: Téléchargement et installation de Delphi
      run: |
        mkdir C:\DelphiInstaller
        curl -L "https://votre-url-de-stockage/Delphi_11_Alexandria_Setup.exe" -o "C:\DelphiInstaller\Setup.exe"
        echo "[InstallParams]" > C:\DelphiInstaller\response.txt
        echo "LicenseFile=C:\DelphiInstaller\license.slip" >> C:\DelphiInstaller\response.txt
        echo "IgnoreLatestUpdate=1" >> C:\DelphiInstaller\response.txt
        echo "AutoSelectPlatforms=1" >> C:\DelphiInstaller\response.txt
        echo "${{ secrets.DELPHI_LICENSE }}" > C:\DelphiInstaller\license.slip
        C:\DelphiInstaller\Setup.exe -q -I"C:\DelphiInstaller\response.txt"

    - name: Compilation du projet
      run: |
        call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
        msbuild "MonProjet.dproj" /t:Build /p:Config=Release /p:Platform=Win32

    - name: Tests unitaires
      run: |
        cd Win32\Release
        MonProjetTests.exe -xml:tests-results.xml

    - name: Publier les résultats de test
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: Tests DUnit
        path: Win32\Release\tests-results.xml
        reporter: java-junit

    - name: Installation d'Inno Setup
      run: |
        choco install innosetup -y

    - name: Création de l'installateur
      run: |
        cd Scripts
        "C:\Program Files (x86)\Inno Setup 6\iscc.exe" MonProjetSetup.iss

    - name: Publication des artefacts
      uses: actions/upload-artifact@v2
      with:
        name: mon-application
        path: |
          Output/MonProjet_Setup.exe
          Win32/Release/MonProjet.exe
```

## Mise en place du Déploiement Continu

Maintenant que nous avons configuré l'Intégration Continue, passons au Déploiement Continu.

### Déploiement sur un site web

Pour publier automatiquement votre installateur sur votre site web :

```yaml
- name: Déploiement sur le site web
  uses: SamKirkland/FTP-Deploy-Action@4.3.0
  with:
    server: ${{ secrets.FTP_SERVER }}
    username: ${{ secrets.FTP_USERNAME }}
    password: ${{ secrets.FTP_PASSWORD }}
    local-dir: Output/
    server-dir: /public_html/downloads/
```

N'oubliez pas d'ajouter les secrets FTP_SERVER, FTP_USERNAME et FTP_PASSWORD dans votre dépôt GitHub.

### Déploiement sur le Windows Store

Pour automatiser la soumission au Windows Store :

```yaml
- name: Installation de Windows Store Uploader
  run: |
    dotnet tool install -g WindowsStoreUploader

- name: Soumission au Windows Store
  run: |
    wsuploader submit -p "Output/MonProjet.msixupload" -id ${{ secrets.STORE_ID }} -t ${{ secrets.STORE_TOKEN }}
```

### Création de releases GitHub

Pour créer automatiquement une release GitHub lorsqu'un tag est poussé :

```yaml
name: Delphi Release

on:
  push:
    tags:
      - 'v*'

jobs:
  build:
    # ... étapes de build comme avant ...

  release:
    needs: build
    runs-on: windows-latest
    steps:
      - name: Télécharger les artefacts
        uses: actions/download-artifact@v2
        with:
          name: mon-application
          path: release-files

      - name: Créer la release
        uses: softprops/action-gh-release@v1
        with:
          files: release-files/*
          body: |
            ## Nouvelle version ${{ github.ref_name }}

            Changelog:
            - Fonctionnalité 1
            - Correction de bug 2
            - Amélioration 3
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

## Bonnes pratiques pour le CI/CD avec Delphi

Pour tirer le meilleur parti de votre pipeline CI/CD avec Delphi, suivez ces bonnes pratiques :

### 1. Automatisez les tests

Les tests automatisés sont essentiels pour un pipeline CI/CD efficace :

```pascal
// Exemple de test unitaire avec DUnitX
procedure TMyTests.TestAddition;
begin
  Assert.AreEqual(5, Calculator.Add(2, 3), 'Addition should work');
end;
```

### 2. Versionning sémantique

Adoptez le versionnement sémantique (MAJEUR.MINEUR.CORRECTIF) et synchronisez-le avec votre système de build :

```pascal
// Dans un fichier VersionInfo.pas
const
  VERSION_MAJOR = 1;
  VERSION_MINOR = 2;
  VERSION_PATCH = 3;
  VERSION_BUILD = {$IFDEF DEFINE_BUILD_NUMBER} {$DEFINE_BUILD_NUMBER} {$ELSE} 0 {$ENDIF};

  VERSION_STRING = FORMAT('%d.%d.%d.%d', [VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH, VERSION_BUILD]);
```

Dans votre workflow CI/CD, définissez le numéro de build :

```yaml
- name: Set Build Number
  run: |
    echo "#define DEFINE_BUILD_NUMBER ${{ github.run_number }}" > buildnumber.inc
```

### 3. Gestion des secrets

Ne stockez jamais les informations sensibles directement dans vos workflows, utilisez des secrets :

- Fichiers de licence Delphi
- Clés de signature de code
- Identifiants de déploiement
- Clés API

### 4. Matrices de build

Pour les applications multi-plateformes, utilisez des matrices pour compiler pour différentes plateformes :

```yaml
jobs:
  build:
    strategy:
      matrix:
        platform: [Win32, Win64, Android, iOS]
        config: [Release, Debug]

    steps:
      # ...
      - name: Compilation du projet
        run: |
          msbuild "MonProjet.dproj" /t:Build /p:Config=${{ matrix.config }} /p:Platform=${{ matrix.platform }}
```

### 5. Optimisation de la vitesse de build

Accélérez vos builds en :

- Utilisant la mise en cache pour les dépendances
- Limitant les tests en fonction des fichiers modifiés
- Exécutant des jobs en parallèle quand c'est possible

```yaml
- name: Cache des packages Delphi
  uses: actions/cache@v2
  with:
    path: C:\Users\runneradmin\Documents\Embarcadero\Studio\22.0\Bpl
    key: ${{ runner.os }}-delphi-packages-${{ hashFiles('**/*.dpk') }}
```

## Cas d'usage avancés

### Intégration du contrôle qualité

Intégrez SonarQube ou une solution similaire pour analyser la qualité du code :

```yaml
- name: Analyse SonarQube
  uses: SonarSource/sonarqube-scan-action@master
  env:
    SONAR_TOKEN: ${{ secrets.SONAR_TOKEN }}
    SONAR_HOST_URL: ${{ secrets.SONAR_HOST_URL }}
  with:
    args: >
      -Dsonar.projectKey=mon-projet-delphi
      -Dsonar.sources=.
      -Dsonar.exclusions=Win32/**/*,Win64/**/*
```

### Automatisation des changelogs

Générez automatiquement des changelogs à partir des messages de commit :

```yaml
- name: Générer le changelog
  id: changelog
  uses: metcalfc/changelog-generator@v0.4.4
  with:
    myToken: ${{ secrets.GITHUB_TOKEN }}
    head-ref: ${{ github.ref }}
    base-ref: ${{ github.event.before }}
```

### Notifications de build

Ajoutez des notifications par e-mail ou sur des plateformes comme Slack ou Discord :

```yaml
- name: Envoyer une notification Slack
  uses: 8398a7/action-slack@v3
  with:
    status: ${{ job.status }}
    fields: repo,message,commit,author,action,eventName,ref,workflow
  env:
    SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
  if: always()
```

## Exemple concret : Pipeline CI/CD complet pour une application Delphi

Voici un exemple plus complet et réaliste pour une application Delphi professionnelle :

```yaml
name: Delphi CI/CD Pipeline

on:
  push:
    branches: [ main, develop ]
    tags: [ 'v*' ]
  pull_request:
    branches: [ main, develop ]
  workflow_dispatch:

jobs:
  build:
    runs-on: windows-latest
    strategy:
      matrix:
        config: [Release]
        platform: [Win32, Win64]

    steps:
    - name: Checkout du code
      uses: actions/checkout@v2
      with:
        fetch-depth: 0  # Récupérer tout l'historique pour les numéros de version

    - name: Configuration de l'environnement Delphi
      run: |
        # Installation de Delphi (ici via image Docker ou installation silencieuse)
        # ...

    - name: Configuration des numéros de version
      run: |
        $version = "1.0.0"
        # Si c'est un tag, extraire la version du tag
        if ("${{ github.ref }}".StartsWith("refs/tags/v")) {
          $version = "${{ github.ref }}".Substring(11)
        }
        # Définir le numéro de build
        echo "VERSION=$version" | Out-File -FilePath $env:GITHUB_ENV -Append
        echo "#define VERSION_STRING ""$version.${{ github.run_number }}""" > version.inc

    - name: Restauration des dépendances
      run: |
        # Installez les packages Delphi nécessaires
        call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
        dpm install

    - name: Compilation du projet
      run: |
        call "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\rsvars.bat"
        msbuild "MaApplication.dproj" /t:Build /p:Config=${{ matrix.config }} /p:Platform=${{ matrix.platform }}

    - name: Exécution des tests unitaires
      run: |
        cd ${{ matrix.platform }}\${{ matrix.config }}
        MaApplicationTests.exe -xml:test-results.xml

    - name: Publication des résultats de test
      uses: dorny/test-reporter@v1
      if: always()
      with:
        name: Tests ${{ matrix.platform }}-${{ matrix.config }}
        path: ${{ matrix.platform }}\${{ matrix.config }}\test-results.xml
        reporter: java-junit

    - name: Création de l'installateur
      if: matrix.platform == 'Win64' && matrix.config == 'Release'
      run: |
        choco install innosetup -y
        copy ${{ matrix.platform }}\${{ matrix.config }}\MaApplication.exe Setup\
        # Remplacer la version dans le script InnoSetup
        (Get-Content Setup\Setup.iss) -replace '#define AppVersion ".*"', '#define AppVersion "${{ env.VERSION }}.${{ github.run_number }}"' | Set-Content Setup\Setup.iss
        "C:\Program Files (x86)\Inno Setup 6\iscc.exe" Setup\Setup.iss

    - name: Signature du code
      if: matrix.platform == 'Win64' && matrix.config == 'Release'
      run: |
        # Décoder le certificat à partir des secrets
        echo "${{ secrets.CODE_SIGNING_CERT }}" | base64 -d > certificate.pfx
        # Signer l'installateur
        signtool sign /f certificate.pfx /p ${{ secrets.CERT_PASSWORD }} /tr http://timestamp.digicert.com /td sha256 /fd sha256 Setup\Output\MaApplicationSetup.exe

    - name: Publication des artefacts
      uses: actions/upload-artifact@v2
      with:
        name: ma-application-${{ matrix.platform }}-${{ matrix.config }}
        path: |
          ${{ matrix.platform }}\${{ matrix.config }}\MaApplication.exe
          Setup\Output\MaApplicationSetup.exe

  release:
    needs: build
    if: startsWith(github.ref, 'refs/tags/v')
    runs-on: windows-latest
    steps:
      - name: Télécharger les artefacts
        uses: actions/download-artifact@v2

      - name: Préparer les fichiers pour la release
        run: |
          mkdir release-files
          copy ma-application-Win64-Release\Setup\Output\MaApplicationSetup.exe release-files\MaApplicationSetup_${{ github.ref_name }}.exe
          copy ma-application-Win32-Release\Win32\Release\MaApplication.exe release-files\MaApplication_32bit_${{ github.ref_name }}.exe
          copy ma-application-Win64-Release\Win64\Release\MaApplication.exe release-files\MaApplication_64bit_${{ github.ref_name }}.exe

      - name: Générer le changelog
        id: changelog
        uses: metcalfc/changelog-generator@v0.4.4
        with:
          myToken: ${{ secrets.GITHUB_TOKEN }}

      - name: Créer la release GitHub
        uses: softprops/action-gh-release@v1
        with:
          files: release-files/*
          body: |
            ## Version ${{ github.ref_name }}

            ${{ steps.changelog.outputs.changelog }}

      - name: Déploiement sur le site web
        uses: SamKirkland/FTP-Deploy-Action@4.3.0
        with:
          server: ${{ secrets.FTP_SERVER }}
          username: ${{ secrets.FTP_USERNAME }}
          password: ${{ secrets.FTP_PASSWORD }}
          local-dir: release-files/
          server-dir: /public_html/downloads/

      - name: Notification de la nouvelle version
        uses: 8398a7/action-slack@v3
        with:
          status: custom
          fields: repo,message,commit,author
          custom_payload: |
            {
              "attachments": [{
                "color": "good",
                "text": "Nouvelle version ${{ github.ref_name }} de MaApplication déployée!"
              }]
            }
        env:
          SLACK_WEBHOOK_URL: ${{ secrets.SLACK_WEBHOOK_URL }}
```

## Exercice pratique : Création d'un workflow CI simple

Pour mettre en pratique ce que vous avez appris, suivez cet exercice :

1. Prenez un projet Delphi simple existant
2. Créez un dépôt GitHub et poussez votre code
3. Créez un fichier `.github/workflows/ci.yml` avec une configuration de base :
   - Checkout du code
   - (Simulez) l'installation de Delphi avec un simple message
   - (Simulez) la compilation avec un autre message
4. Activez GitHub Actions et vérifiez que le workflow s'exécute
5. Progressivement, améliorez le workflow en ajoutant des étapes plus avancées

## Conclusion

La mise en place d'un pipeline CI/CD pour vos applications Delphi peut sembler complexe au départ, mais les avantages sont considérables. En automatisant la compilation, les tests et le déploiement, vous améliorez la qualité de votre code, réduisez les erreurs humaines et accélérez la livraison de nouvelles fonctionnalités à vos utilisateurs.

Commencez simplement, avec un pipeline basique, puis enrichissez-le progressivement. N'oubliez pas que le CI/CD est un processus d'amélioration continue, tout comme le développement logiciel lui-même.

Dans la prochaine section, nous explorerons les techniques de télémétrie et d'analyse de crash pour suivre la santé de vos applications en production et identifier rapidement les problèmes rencontrés par vos utilisateurs.

⏭️ [Télémétrie et analyse de crash](17-distribution-et-deploiement/10-telemetrie-et-analyse-de-crash.md)
