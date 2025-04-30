# 17.6 Déploiement sur différentes plateformes

## Introduction

Une des grandes forces de Delphi est sa capacité à créer des applications pour différentes plateformes à partir d'une seule base de code. Grâce à FireMonkey (FMX) et aux capacités multi-plateformes de Delphi, vous pouvez déployer vos applications sur :

- Windows (32 et 64 bits)
- macOS
- iOS (iPhone et iPad)
- Android
- Linux (depuis Delphi 10.4 via FMXLinux)

Dans ce chapitre, nous allons explorer le processus de déploiement pour chacune de ces plateformes, en commençant par les bases et en abordant progressivement les spécificités de chaque environnement. Nous nous concentrerons sur la préparation, la compilation et la distribution de votre application pour chaque plateforme cible.

## Configuration des plateformes cibles

Avant de commencer à déployer sur une plateforme, vous devez vous assurer que l'environnement de développement est correctement configuré. Dans Delphi, cela implique l'installation des SDK (Software Development Kits) nécessaires.

### Vérification des plateformes installées

1. Ouvrez Delphi et créez un nouveau projet multi-plateforme FireMonkey (Fichier → Nouveau → FireMonkey → Application multiplateforme)
2. Dans la barre d'outils, examinez le sélecteur de plateforme pour voir quelles plateformes sont disponibles

![Sélecteur de plateforme](https://placeholder-image.com/platform_selector.png)

Si certaines plateformes sont manquantes ou apparaissent en grisé, vous devrez peut-être installer les SDK correspondants.

### Installation des SDK manquants

Pour installer les SDK manquants, utilisez le Gestionnaire de SDK intégré à Delphi :

1. Allez dans **Outils** → **Options**
2. Dans l'arborescence à gauche, naviguez jusqu'à **Deployment** → **SDK Manager**
3. Dans la liste des SDK, cochez ceux que vous souhaitez installer
4. Cliquez sur **Installer les SDK sélectionnés**

![SDK Manager](https://placeholder-image.com/sdk_manager.png)

## Déploiement sur Windows

Windows est la plateforme native pour Delphi, ce qui rend le déploiement relativement simple.

### Compilation pour Windows 32-bit

1. Sélectionnez **Windows 32-bit** dans le sélecteur de plateforme
2. Choisissez **Release** dans le sélecteur de configuration (à côté du sélecteur de plateforme)
3. Allez dans **Projet** → **Compiler le projet** (ou appuyez sur `Shift+F9`)
4. Votre exécutable sera généré dans le sous-dossier `Win32\Release` de votre projet

### Compilation pour Windows 64-bit

1. Sélectionnez **Windows 64-bit** dans le sélecteur de plateforme
2. Choisissez **Release** dans le sélecteur de configuration
3. Allez dans **Projet** → **Compiler le projet**
4. Votre exécutable sera généré dans le sous-dossier `Win64\Release` de votre projet

### Fichiers à distribuer

Pour une application Windows complète, vous devrez inclure :

- Votre fichier exécutable (.exe)
- Toutes les DLL utilisées par votre application
- Fichiers de données (si nécessaire)
- Fichiers de configuration

### Création d'un installateur

Comme nous l'avons vu dans la section [17.3 Création d'installateurs](lien-vers-section-17.3), vous pouvez utiliser Inno Setup ou InstallAware pour créer un installateur professionnel pour Windows.

### Considérations pour Windows 10/11

Pour offrir une meilleure intégration avec Windows 10/11 :

1. Ajoutez un manifeste d'application pour la prise en charge du DPI (haute résolution)
2. Intégrez des icônes au format PNG en haute résolution
3. Utilisez les styles visuels Windows 10/11 disponibles dans Delphi

## Déploiement sur macOS

Le déploiement sur macOS est un peu plus complexe et nécessite quelques étapes supplémentaires.

### Prérequis

Pour compiler pour macOS, vous aurez besoin :

1. D'un Mac physique ou d'une machine virtuelle exécutant macOS
2. De Xcode installé sur cette machine macOS
3. Du SDK macOS configuré dans Delphi
4. De PAServer (Platform Assistant Server) en cours d'exécution sur la machine macOS

### Configuration de PAServer

1. Installez PAServer sur votre Mac (disponible dans le dossier d'installation de Delphi, sous-dossier `PAServer`)
2. Lancez PAServer sur votre Mac et notez le mot de passe affiché
3. Dans Delphi, configurez la connexion à PAServer :
   - Allez dans **Outils** → **Options**
   - Naviguez vers **Connection Profile Manager**
   - Ajoutez une nouvelle connexion avec l'adresse IP de votre Mac et le mot de passe de PAServer

### Compilation pour macOS

1. Sélectionnez **macOS 64-bit** dans le sélecteur de plateforme
2. Choisissez **Release** dans le sélecteur de configuration
3. Allez dans **Projet** → **Compiler le projet**
4. Delphi se connectera à PAServer sur votre Mac et générera le binaire macOS

### Création d'un bundle d'application macOS

Sur macOS, les applications sont généralement distribuées sous forme de bundles `.app`. Delphi peut créer automatiquement ce format :

1. Assurez-vous que l'option **Build application bundle** est activée dans les options du projet (onglet macOS)
2. Compilez votre application
3. Le bundle `.app` sera créé dans le dossier de sortie sur votre Mac

### Signature et notarisation (obligatoire pour la distribution)

Pour distribuer une application macOS, elle doit être signée et notarisée par Apple :

1. Obtenez un certificat de développeur Apple (via le Apple Developer Program)
2. Signez votre application avec ce certificat :
   ```bash
   codesign --force --sign "Developer ID Application: Votre Nom" VotreApp.app
   ```
3. Soumettez votre application à Apple pour notarisation :
   ```bash
   xcrun altool --notarize-app --primary-bundle-id "com.votreentreprise.votreapp" --username "email@apple.com" --password "app-specific-password" --file VotreApp.zip
   ```

## Déploiement sur iOS (iPhone et iPad)

Le déploiement sur iOS est similaire à macOS, mais avec des étapes supplémentaires liées à l'App Store.

### Prérequis

Vous aurez besoin :

1. D'un Mac avec Xcode installé
2. D'un compte Apple Developer (99$/an)
3. De PAServer en cours d'exécution sur le Mac
4. Du SDK iOS configuré dans Delphi

### Préparation de votre application pour iOS

1. Configurez les icônes et écrans de démarrage dans le projet :
   - Allez dans **Projet** → **Options**
   - Naviguez vers l'onglet **Application**
   - Configurez les icônes et les écrans de démarrage pour les différentes tailles d'écran

2. Configurez les informations de version et le bundle ID :
   - Dans les options du projet, onglet **Version Info**
   - Définissez un Bundle ID unique (ex: com.votreentreprise.votreapp)
   - Configurez les numéros de version (CFBundleVersion et CFBundleShortVersionString)

### Compilation pour iOS

1. Sélectionnez **iOS Device 64-bit** dans le sélecteur de plateforme (ou **iOS Simulator** pour les tests)
2. Choisissez **Release** dans le sélecteur de configuration
3. Allez dans **Projet** → **Compiler le projet**

### Création d'un certificat et d'un profil de provisionnement

Pour déployer sur des appareils iOS ou sur l'App Store :

1. Créez un certificat de distribution dans l'Apple Developer Portal
2. Créez un identifiant d'application (App ID) correspondant à votre Bundle ID
3. Créez un profil de provisionnement pour cet App ID
4. Téléchargez et installez le certificat et le profil sur votre Mac

### Génération du package IPA pour l'App Store

1. Dans Delphi, allez dans **Projet** → **Options de déploiement**
2. Configurez les paramètres de déploiement iOS :
   - Sélectionnez votre profil de provisionnement
   - Activez l'option **Build for App Store**
3. Compilez le projet
4. Utilisez Xcode ou l'Application Loader pour soumettre le package IPA à l'App Store Connect

## Déploiement sur Android

Android est une plateforme importante pour les applications mobiles et Delphi offre un excellent support pour celle-ci.

### Prérequis

Pour compiler pour Android, vous aurez besoin :

1. Du SDK Android (installable via le Gestionnaire de SDK de Delphi)
2. Du JDK (Java Development Kit)
3. Des outils Android SDK (adb, etc.)

### Configuration de votre projet Android

1. Configurez les icônes et écrans de démarrage :
   - Dans les options du projet, onglet **Application**
   - Définissez des icônes pour les différentes densités d'écran (ldpi, mdpi, hdpi, xhdpi, xxhdpi)

2. Configurez le manifeste Android :
   - Dans les options du projet, onglet **Version Info**
   - Définissez Package Name (ex: com.votreentreprise.votreapp)
   - Configurez les versions (version code et version name)
   - Spécifiez les permissions requises

### Compilation pour Android

1. Sélectionnez **Android 32-bit** ou **Android 64-bit** dans le sélecteur de plateforme
2. Choisissez **Release** dans le sélecteur de configuration
3. Allez dans **Projet** → **Compiler le projet**
4. Votre package APK sera généré dans le dossier de sortie du projet

### Signature du package APK

Pour distribuer une application Android, vous devez la signer avec votre propre clé :

1. Créez une clé de signature (si vous n'en avez pas déjà une) :
   ```bash
   keytool -genkey -v -keystore votreapp.keystore -alias votreapp -keyalg RSA -keysize 2048 -validity 10000
   ```

2. Configurez la signature dans les options de déploiement Android de Delphi :
   - Allez dans **Projet** → **Options de déploiement**
   - Onglet **Signing**
   - Spécifiez le keystore, l'alias et les mots de passe

3. Recompilez le projet pour générer un APK signé

### Publication sur Google Play Store

1. Créez un compte développeur Google Play (frais uniques de 25$)
2. Accédez à la Google Play Console
3. Créez une nouvelle application
4. Téléversez votre APK signé
5. Fournissez tous les assets marketing nécessaires (captures d'écran, description, etc.)
6. Soumettez pour examen

## Déploiement sur Linux

Depuis Delphi 10.4, il est possible de déployer des applications sur Linux en utilisant FMXLinux.

### Prérequis

Pour compiler pour Linux, vous aurez besoin :

1. D'une machine Linux (physique ou virtuelle)
2. De PAServer installé et en cours d'exécution sur cette machine
3. Des bibliothèques de développement GTK3 installées sur la machine Linux

### Installation de PAServer sur Linux

1. Copiez le dossier PAServer de votre installation Delphi vers votre machine Linux
2. Installez les dépendances requises :
   ```bash
   sudo apt-get install libgtk-3-dev
   sudo apt-get install libcanberra-gtk3-module
   ```
3. Lancez PAServer :
   ```bash
   ./paserver
   ```

### Compilation pour Linux

1. Sélectionnez **Linux 64-bit** dans le sélecteur de plateforme
2. Choisissez **Release** dans le sélecteur de configuration
3. Configurez la connexion PAServer (comme pour macOS)
4. Allez dans **Projet** → **Compiler le projet**

### Distribution de l'application Linux

Pour distribuer votre application Linux :

1. Copiez l'exécutable et tous les fichiers nécessaires (bibliothèques partagées, ressources, etc.)
2. Créez un script d'installation ou utilisez un format de package comme .deb ou .rpm
3. Assurez-vous que toutes les dépendances sont satisfaites sur les systèmes cibles

## Conseil important : Conception multi-plateformes dès le départ

Pour réussir le déploiement sur plusieurs plateformes, il est essentiel de penser "multi-plateformes" dès le début de votre projet :

1. **Utilisez FireMonkey (FMX)** pour l'interface utilisateur si vous ciblez plusieurs plateformes
2. **Isolez le code spécifique à la plateforme** :
   ```pascal
   {$IFDEF MSWINDOWS}
     // Code spécifique à Windows
   {$ENDIF}

   {$IFDEF MACOS}
     // Code spécifique à macOS
   {$ENDIF}

   {$IFDEF IOS}
     // Code spécifique à iOS
   {$ENDIF}

   {$IFDEF ANDROID}
     // Code spécifique à Android
   {$ENDIF}

   {$IFDEF LINUX}
     // Code spécifique à Linux
   {$ENDIF}
   ```

3. **Créez des abstractions** pour les fonctionnalités spécifiques à la plateforme
4. **Testez régulièrement** sur toutes les plateformes cibles

## Solution pratique : Utilisation d'un script de déploiement multi-plateformes

Pour automatiser le déploiement sur plusieurs plateformes, vous pouvez créer un script batch ou PowerShell :

```powershell
# Script PowerShell pour compiler pour plusieurs plateformes
param (
    [string]$ProjectFile,
    [switch]$Windows32 = $false,
    [switch]$Windows64 = $false,
    [switch]$Android = $false,
    [switch]$All = $false
)

$DelphiPath = "C:\Program Files (x86)\Embarcadero\Studio\22.0\bin\bds.exe"

# Fonction pour compiler pour une plateforme spécifique
function CompilePlatform {
    param (
        [string]$Platform,
        [string]$Config = "Release"
    )

    Write-Host "Compilation pour $Platform en mode $Config..."
    & "$DelphiPath" -build -config=$Config -platform=$Platform "$ProjectFile"

    if ($LASTEXITCODE -eq 0) {
        Write-Host "Compilation réussie pour $Platform" -ForegroundColor Green
    } else {
        Write-Host "Échec de la compilation pour $Platform" -ForegroundColor Red
    }
}

# Compiler pour les plateformes sélectionnées
if ($Windows32 -or $All) {
    CompilePlatform "Win32"
}

if ($Windows64 -or $All) {
    CompilePlatform "Win64"
}

if ($Android -or $All) {
    CompilePlatform "Android"
}

Write-Host "Processus de compilation terminé."
```

## Déploiement de mises à jour multi-plateformes

Le système de mise à jour automatique que nous avons vu dans la section précédente peut être adapté pour fonctionner sur différentes plateformes. Voici quelques ajustements à apporter :

### Fichier de version multi-plateformes

```xml
<?xml version="1.0" encoding="UTF-8"?>
<updates>
  <platform name="Windows">
    <version>2.1.5</version>
    <url>https://www.monsite.com/downloads/win/MonApp_Setup_2.1.5.exe</url>
    <notes>Notes de version pour Windows...</notes>
  </platform>

  <platform name="macOS">
    <version>2.1.5</version>
    <url>https://www.monsite.com/downloads/mac/MonApp_2.1.5.dmg</url>
    <notes>Notes de version pour macOS...</notes>
  </platform>

  <platform name="Android">
    <version>2.1.5</version>
    <url>https://www.monsite.com/downloads/android/MonApp_2.1.5.apk</url>
    <notes>Notes de version pour Android...</notes>
  </platform>
</updates>
```

### Code de vérification multi-plateformes

```pascal
function TAutoUpdater.CheckForUpdates: TUpdateInfo;
var
  HTTP: THTTPClient;
  Response: IHTTPResponse;
  XMLDoc: IXMLDocument;
  RootNode, PlatformNode: IXMLNode;
  PlatformName: string;
begin
  // Initialiser le résultat
  Result.CurrentVersion := FCurrentVersion;
  Result.UpdateAvailable := False;

  // Déterminer la plateforme actuelle
  {$IFDEF MSWINDOWS}
    PlatformName := 'Windows';
  {$ENDIF}
  {$IFDEF MACOS}
    PlatformName := 'macOS';
  {$ENDIF}
  {$IFDEF IOS}
    PlatformName := 'iOS';
  {$ENDIF}
  {$IFDEF ANDROID}
    PlatformName := 'Android';
  {$ENDIF}
  {$IFDEF LINUX}
    PlatformName := 'Linux';
  {$ENDIF}

  try
    HTTP := THTTPClient.Create;
    try
      Response := HTTP.Get(FUpdateURL);

      if Response.StatusCode = 200 then
      begin
        XMLDoc := TXMLDocument.Create(nil);
        XMLDoc.LoadFromXML(Response.ContentAsString);
        XMLDoc.Active := True;

        RootNode := XMLDoc.DocumentElement;

        // Parcourir les plateformes pour trouver la nôtre
        for var i := 0 to RootNode.ChildNodes.Count - 1 do
        begin
          PlatformNode := RootNode.ChildNodes[i];

          if (PlatformNode.NodeName = 'platform') and
             (PlatformNode.Attributes['name'] = PlatformName) then
          begin
            Result.NewVersion := PlatformNode.ChildValues['version'];
            Result.DownloadURL := PlatformNode.ChildValues['url'];
            Result.ReleaseNotes := PlatformNode.ChildValues['notes'];

            // Comparer les versions
            Result.UpdateAvailable := CompareVersions(FCurrentVersion, Result.NewVersion) < 0;

            if Result.UpdateAvailable and Assigned(FOnUpdateAvailable) then
              FOnUpdateAvailable(Result)
            else if not Result.UpdateAvailable and Assigned(FOnNoUpdateAvailable) then
              FOnNoUpdateAvailable(Self);

            Break;
          end;
        end;
      end
      else if Assigned(FOnError) then
        FOnError('Erreur HTTP: ' + Response.StatusCode.ToString);
    finally
      HTTP.Free;
    end;
  except
    on E: Exception do
      if Assigned(FOnError) then
        FOnError('Erreur: ' + E.Message);
  end;
end;
```

## Exercice pratique : Déploiement sur deux plateformes

Pour mettre en pratique ce que vous avez appris, suivez ces étapes :

1. Créez une application FireMonkey simple (par exemple, une calculatrice ou un bloc-notes)
2. Assurez-vous qu'elle fonctionne correctement sur Windows
3. Configurez votre environnement pour une deuxième plateforme (Android est généralement la plus facile à configurer après Windows)
4. Adaptez l'interface utilisateur si nécessaire pour qu'elle soit ergonomique sur les deux plateformes
5. Compilez l'application pour les deux plateformes
6. Testez le déploiement sur les deux plateformes

## Conclusion

Le déploiement multi-plateformes avec Delphi est un atout majeur qui vous permet d'atteindre un public plus large avec un seul projet. Bien que chaque plateforme ait ses spécificités et ses défis, Delphi et FireMonkey offrent les outils nécessaires pour créer des applications professionnelles sur Windows, macOS, iOS, Android et Linux.

En suivant les étapes décrites dans ce chapitre et en planifiant votre développement de manière multi-plateforme dès le départ, vous pourrez tirer pleinement parti de cette capacité et offrir une expérience cohérente à vos utilisateurs, quelle que soit la plateforme qu'ils utilisent.

Dans la prochaine section, nous explorerons la virtualisation et les conteneurs Docker, qui offrent encore plus de flexibilité pour le déploiement de vos applications Delphi.
