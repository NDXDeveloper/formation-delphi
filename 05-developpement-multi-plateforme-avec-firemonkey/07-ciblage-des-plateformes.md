🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.7 Ciblage des plateformes : Windows, macOS, iOS, Android, Linux

## Introduction

L'un des atouts majeurs de FireMonkey est sa capacité à compiler une seule application vers plusieurs systèmes d'exploitation. Mais cibler une plateforme ne se limite pas à cliquer sur un bouton "compiler pour iOS" : chaque système a ses spécificités, ses exigences, et ses conventions. Dans cette section, nous allons explorer comment configurer, compiler, et déployer votre application FireMonkey sur Windows, macOS, iOS, Android et Linux.

## 1. Vue d'ensemble des plateformes supportées

### Plateformes disponibles avec FireMonkey

**Desktop** :
- **Windows** : 32 bits et 64 bits (Windows 10, 11)
- **macOS** : Intel et Apple Silicon (macOS 10.14+)
- **Linux** : 64 bits (principales distributions)

**Mobile** :
- **iOS** : iPhone et iPad (iOS 12+)
- **Android** : Smartphones et tablettes (Android 6.0+)

### Éditions de Delphi et plateformes

Les plateformes disponibles dépendent de votre édition de Delphi :

**Community Edition** :
- Windows 32 bits et 64 bits
- Android

**Professional Edition** :
- Windows 32 bits et 64 bits
- macOS (avec connexion à un Mac)
- iOS (avec connexion à un Mac)
- Android
- Linux (avec FMXLinux)

**Enterprise/Architect Edition** :
- Toutes les plateformes ci-dessus
- Fonctionnalités avancées de déploiement

### Un code source, plusieurs cibles

Le principe de FireMonkey :

```pascal
// Un seul code source
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  ShowMessage('Hello World!');
end;

// Compile vers :
// - Windows 32 bits → .exe
// - Windows 64 bits → .exe
// - macOS → .app
// - iOS → .app
// - Android → .apk
// - Linux → binaire ELF
```

## 2. Configuration de l'environnement multi-plateforme

### Configuration pour Windows

**Aucune configuration spéciale nécessaire** : Delphi s'installe sur Windows et peut compiler pour Windows immédiatement.

**Compilation** :
- **32 bits** : Cible "Windows 32-bit"
- **64 bits** : Cible "Windows 64-bit"

**Déploiement** :
- Un simple fichier .exe
- Peut nécessiter des DLLs (runtime Visual C++)

### Configuration pour macOS

Pour compiler vers macOS, vous avez besoin :

**Matériel** :
- Un Mac physique ou virtuel
- Connexion réseau entre votre PC Windows et le Mac

**Logiciel sur le Mac** :
- macOS 10.14 ou supérieur
- Xcode installé (gratuit depuis l'App Store)
- PAServer (Platform Assistant Server) - fourni avec Delphi

**Configuration** :

1. **Installer PAServer sur le Mac** :
   ```bash
   # Copier PAServer depuis l'installation Delphi
   # Dossier : C:\Program Files\Embarcadero\Studio\XX.0\PAServer\
   # Copier vers le Mac et exécuter
   ./paserver
   ```

2. **Dans Delphi, configurer la connexion** :
   - Tools → Options → Connection Profile Manager
   - Ajouter un nouveau profil macOS
   - Entrer l'IP du Mac et le mot de passe PAServer
   - Tester la connexion

3. **Sélectionner la plateforme macOS** :
   - Project → Target Platforms
   - Ajouter "macOS 64-bit"
   - Double-cliquer pour l'activer

### Configuration pour iOS

iOS nécessite un Mac, car Apple impose l'utilisation de Xcode.

**Prérequis** :
- Un Mac avec macOS et Xcode
- PAServer installé sur le Mac
- Compte développeur Apple (99$/an pour publier)
- Profil de provisionnement et certificat de développement

**Configuration** :

1. **Créer un App ID** :
   - Se connecter à developer.apple.com
   - Certificates, Identifiers & Profiles
   - Créer un App ID pour votre application

2. **Créer un profil de provisionnement** :
   - Development ou Distribution
   - Lier aux appareils de test (pour développement)

3. **Configurer dans Delphi** :
   - Tools → Options → SDK Manager
   - Ajouter le SDK iOS
   - Project → Options → Provisioning
   - Sélectionner le profil de provisionnement

4. **Déployer sur appareil** :
   - Connecter l'iPhone/iPad au Mac (USB)
   - Project → Deploy
   - L'application s'installe sur l'appareil

### Configuration pour Android

Android est plus simple car ne nécessite pas de Mac.

**Prérequis** :
- Android SDK (téléchargeable via Delphi)
- JDK (Java Development Kit)
- Pour tester : Un appareil Android ou un émulateur

**Configuration** :

1. **Installer Android SDK** :
   - Tools → Options → Deployment → SDK Manager
   - Télécharger Android SDK
   - Installer les API levels nécessaires (minimum API 23)

2. **Installer JDK** :
   - Télécharger OpenJDK ou Oracle JDK
   - Configurer le chemin dans Delphi
   - Tools → Options → Environment Options → SDK Manager

3. **Configurer un appareil de test** :
   - Activer "Mode développeur" sur l'appareil Android
   - Activer "Débogage USB"
   - Connecter via USB

4. **Compiler et déployer** :
   - Sélectionner plateforme Android
   - Run → Run (F9)
   - L'APK est généré et installé sur l'appareil

### Configuration pour Linux

Linux nécessite FMXLinux, une extension de FireMonkey.

**Prérequis** :
- Une machine Linux (physique ou virtuelle)
- Distribution compatible (Ubuntu, Debian, Fedora)
- PAServer Linux

**Configuration** :

1. **Installer PAServer sur Linux** :
   ```bash
   # Installer les dépendances
   sudo apt-get install libgtk-3-0

   # Copier et exécuter PAServer
   ./paserver-linux64
   ```

2. **Dans Delphi** :
   - Configurer la connexion vers la machine Linux
   - Sélectionner plateforme Linux64

3. **Compiler** :
   - Le binaire est généré sur la machine Linux
   - Peut être exécuté directement

## 3. Gestion des plateformes dans l'IDE

### Le gestionnaire de plateformes

**Accès** : Project → Target Platforms

**Interface** :
```
Target Platforms
├── Windows 32-bit  [Active]
├── Windows 64-bit
├── macOS 64-bit
├── iOS Device
├── iOS Simulator
├── Android 32-bit
├── Android 64-bit
└── Linux 64-bit
```

**Actions** :
- **Ajouter** : Ajouter une nouvelle plateforme
- **Activer** : Double-clic pour activer une plateforme
- **Supprimer** : Retirer une plateforme du projet

### Compilation par plateforme

**Méthode 1 : Via le gestionnaire de projets**
```
1. Activer la plateforme cible (double-clic)
2. Project → Build [Nom du projet]
3. Le binaire est créé pour cette plateforme
```

**Méthode 2 : Via la ligne de commande**
```bash
# Compiler pour Windows 64-bit
msbuild MonProjet.dproj /p:Platform=Win64

# Compiler pour Android
msbuild MonProjet.dproj /p:Platform=Android

# Compiler pour iOS
msbuild MonProjet.dproj /p:Platform=iOSDevice64
```

**Méthode 3 : Compilation batch**
```pascal
// Script pour compiler toutes les plateformes
for %%p in (Win32 Win64 Android iOSDevice64 OSX64) do (
    msbuild MonProjet.dproj /p:Platform=%%p /p:Config=Release
)
```

### Fichiers générés par plateforme

Chaque plateforme génère des fichiers différents :

**Windows** :
```
Win32\Debug\MonApp.exe  
Win64\Release\MonApp.exe  
```

**macOS** :
```
OSX64\Debug\MonApp.app/
  └── Contents/
      └── MacOS/
          └── MonApp
```

**iOS** :
```
iOSDevice64\Debug\MonApp.app/
  └── MonApp (binaire ARM64)
```

**Android** :
```
Android\Debug\MonApp.apk
```

**Linux** :
```
Linux64\Debug\MonApp (binaire ELF)
```

## 4. Code conditionnel par plateforme

### Directives de compilation

Pour écrire du code spécifique à une plateforme, utilisez les directives de compilation :

**Syntaxe de base** :
```pascal
{$IFDEF PLATEFORME}
  // Code spécifique
{$ENDIF}
```

**Directives disponibles** :

```pascal
{$IFDEF MSWINDOWS}
  // Code Windows uniquement
{$ENDIF}

{$IFDEF MACOS}
  // Code macOS uniquement (inclut iOS et macOS)
{$ENDIF}

{$IFDEF ANDROID}
  // Code Android uniquement
{$ENDIF}

{$IFDEF IOS}
  // Code iOS uniquement
{$ENDIF}

{$IFDEF LINUX}
  // Code Linux uniquement
{$ENDIF}
```

### Directives combinées

```pascal
{$IFDEF ANDROID OR IOS}
  // Code mobile (Android et iOS)
  TaillePolice := 16;
{$ELSE}
  // Code desktop
  TaillePolice := 11;
{$ENDIF}
```

```pascal
{$IFDEF MSWINDOWS OR MACOS OR LINUX}
  // Code desktop seulement
  AfficherBarreMenu;
{$ELSE}
  // Code mobile
  AfficherMenuHamburger;
{$ENDIF}
```

### Exemple pratique : Adapter l'interface

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  {$IFDEF ANDROID OR IOS}
    // Mobile : Boutons plus grands
    Button1.Height := 50;
    Button1.Width := 200;

    // Navigation en bas
    TabControl1.TabPosition := TTabPosition.Bottom;

    // Pas de barre de menus
    MainMenu1.Visible := False;
  {$ELSE}
    // Desktop : Interface traditionnelle
    Button1.Height := 30;
    Button1.Width := 100;

    // Navigation en haut
    TabControl1.TabPosition := TTabPosition.Top;

    // Barre de menus visible
    MainMenu1.Visible := True;
  {$ENDIF}
end;
```

### Appels aux API natives

Chaque plateforme a ses propres API natives :

```pascal
{$IFDEF MSWINDOWS}
uses
  WinAPI.Windows, WinAPI.Messages;

procedure AfficherNotificationWindows;  
begin  
  // Utiliser l'API Windows
  MessageBox(0, 'Notification', 'Application', MB_OK);
end;
{$ENDIF}

{$IFDEF ANDROID}
uses
  Androidapi.JNI.JavaTypes, Androidapi.JNI.Toast;

procedure AfficherNotificationAndroid;  
var  
  Toast: JToast;
begin
  // Utiliser l'API Android
  Toast := TJToast.JavaClass.makeText(
    TAndroidHelper.Context,
    StrToJCharSequence('Notification'),
    TJToast.JavaClass.LENGTH_SHORT);
  Toast.show;
end;
{$ENDIF}

{$IFDEF IOS}
uses
  iOSapi.Foundation, iOSapi.UIKit;

procedure AfficherNotificationiOS;  
var  
  Alert: UIAlertView;
begin
  // Utiliser l'API iOS
  Alert := TUIAlertView.Create;
  Alert.initWithTitle(
    StrToNSStr('Application'),
    StrToNSStr('Notification'),
    nil,
    StrToNSStr('OK'),
    nil);
  Alert.show;
end;
{$ENDIF}
```

### Fonction multi-plateforme unifiée

Créer des fonctions qui fonctionnent sur toutes les plateformes :

```pascal
procedure AfficherNotification(const Titre, Message: string);  
begin  
  {$IFDEF MSWINDOWS}
    AfficherNotificationWindows;
  {$ENDIF}

  {$IFDEF ANDROID}
    AfficherNotificationAndroid;
  {$ENDIF}

  {$IFDEF IOS}
    AfficherNotificationiOS;
  {$ENDIF}

  {$IFDEF MACOS}
    // Notification macOS
    ShowMessage(Message);
  {$ENDIF}

  {$IFDEF LINUX}
    // Notification Linux
    ShowMessage(Message);
  {$ENDIF}
end;

// Utilisation simple partout
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  AfficherNotification('Info', 'Action terminée');
end;
```

## 5. Spécificités par plateforme

### Windows : Spécificités

**Points forts** :
- Intégration parfaite avec l'OS
- Accès à toute l'API Windows
- Performance optimale
- Aucune restriction

**Considérations** :
```pascal
// Chemins de fichiers
{$IFDEF MSWINDOWS}
  CheminApp := ExtractFilePath(ParamStr(0));
  CheminDocs := GetEnvironmentVariable('USERPROFILE') + '\Documents\';
{$ENDIF}

// Séparateur de chemin
{$IFDEF MSWINDOWS}
  Separateur := '\';
{$ELSE}
  Separateur := '/';
{$ENDIF}
```

**Permissions** :
- Aucune restriction particulière
- Peut nécessiter élévation UAC pour certaines opérations

### macOS : Spécificités

**Points forts** :
- Look natif macOS
- Intégration avec l'écosystème Apple
- Accès aux API Cocoa

**Considérations** :

```pascal
{$IFDEF MACOS}
  // Chemins macOS
  CheminApp := ExtractFilePath(ParamStr(0));
  CheminDocs := GetHomePath + '/Documents/';

  // Sandbox pour App Store
  // Limitations d'accès aux fichiers
{$ENDIF}
```

**Structure de l'application** :
```
MonApp.app/
├── Contents/
│   ├── Info.plist (métadonnées)
│   ├── MacOS/
│   │   └── MonApp (exécutable)
│   └── Resources/
│       └── (icônes, ressources)
```

**Signature de code** :
- Obligatoire pour distribuer
- Certificat développeur Apple nécessaire

### iOS : Spécificités

**Points forts** :
- Accès à tous les capteurs iPhone/iPad
- Intégration avec les services Apple
- Performance excellente

**Contraintes fortes** :

```pascal
{$IFDEF IOS}
  // Sandbox strict
  // Accès limité au système de fichiers
  CheminDocs := TPath.GetDocumentsPath;

  // Pas d'accès au système de fichiers général
  // Pas d'exécution de code externe
  // Toutes les ressources doivent être embarquées
{$ENDIF}
```

**Permissions à demander** :
```pascal
// Exemple : Demander accès à la caméra
uses
  iOSapi.AVFoundation;

procedure DemanderAccesCamera;  
begin  
  TAVCaptureDevice.OCClass.requestAccessForMediaType(
    AVMediaTypeVideo,
    procedure(granted: Boolean)
    begin
      if granted then
        // Autorisation accordée
      else
        // Autorisation refusée
    end);
end;
```

**Orientations** :
- Configurer dans Project → Options → Version Info
- Cocher les orientations supportées

**Distribution** :
- Obligatoire via App Store (ou TestFlight pour test)
- Processus de révision Apple

### Android : Spécificités

**Points forts** :
- Fragmentation mais grande flexibilité
- Distribution libre (APK direct ou Play Store)
- Accès complet au système

**Permissions dans AndroidManifest.xml** :

```xml
<!-- Accès Internet -->
<uses-permission android:name="android.permission.INTERNET"/>

<!-- Accès à la caméra -->
<uses-permission android:name="android.permission.CAMERA"/>

<!-- Accès au stockage -->
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE"/>
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>

<!-- Localisation -->
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>
```

**Dans Delphi** :
```pascal
{$IFDEF ANDROID}
uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  FMX.Platform.Android;

// Demander une permission à l'exécution (Android 6.0+)
procedure DemanderPermission(const Permission: string);  
begin  
  PermissionsService.RequestPermissions(
    [Permission],
    procedure(const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
        // Permission accordée
      else
        // Permission refusée
    end);
end;
{$ENDIF}
```

**Chemins Android** :
```pascal
{$IFDEF ANDROID}
  // Stockage interne (privé à l'app)
  CheminInterne := TPath.GetDocumentsPath;

  // Stockage externe (partagé)
  CheminExterne := TPath.GetSharedDocumentsPath;
{$ENDIF}
```

**Fragmentation** :
- Tester sur plusieurs versions Android
- Tester différentes résolutions d'écran
- Gérer les notchs et écrans pliables

### Linux : Spécificités

**Points forts** :
- Open source
- Grande communauté
- Pas de restrictions

**Considérations** :

```pascal
{$IFDEF LINUX}
  // Chemins Linux standard
  CheminApp := ExtractFilePath(ParamStr(0));
  CheminHome := GetEnvironmentVariable('HOME');
  CheminConfig := CheminHome + '/.config/MonApp/';
{$ENDIF}
```

**Dépendances** :
- GTK3 pour l'interface graphique
- Bibliothèques système

**Distribution** :
```bash
# Empaqueter pour Debian/Ubuntu
# Créer un package .deb

# Empaqueter pour Red Hat/Fedora
# Créer un package .rpm

# AppImage (portable)
# Créer un .AppImage
```

## 6. Ressources et assets par plateforme

### Images et icônes

Chaque plateforme a ses exigences d'icônes :

**Windows** :
- Icône .ico (multi-résolutions : 16x16, 32x32, 48x48, 256x256)

**macOS** :
- Icône .icns (multiples résolutions)

**iOS** :
- Multiples tailles : 20x20, 29x29, 40x40, 60x60, 76x76, 83.5x83.5, 1024x1024
- Format PNG
- Retina (@2x, @3x)

**Android** :
- Plusieurs densités : ldpi, mdpi, hdpi, xhdpi, xxhdpi, xxxhdpi
- Icône adaptative (Android 8+)

**Configuration dans Delphi** :
- Project → Options → Application → Icons
- Charger les icônes pour chaque plateforme

### Déploiement des fichiers

Le gestionnaire de déploiement indique quels fichiers copier :

**Accès** : Project → Deployment

```
Deployment Manager
├── Windows
│   ├── MonApp.exe
│   └── (DLLs si nécessaire)
├── macOS
│   ├── MonApp (binaire)
│   └── Resources/
├── iOS
│   ├── MonApp (binaire)
│   └── Assets/
└── Android
    ├── classes.dex
    └── res/
```

**Ajouter un fichier** :
```
1. Deployment Manager
2. Add Files
3. Sélectionner le fichier
4. Choisir les plateformes cibles
5. Spécifier le chemin de destination
```

### Chemins de déploiement

```pascal
// Chemin du fichier déployé
function ObtenirCheminRessource(const NomFichier: string): string;  
begin  
  {$IFDEF MSWINDOWS}
    Result := ExtractFilePath(ParamStr(0)) + NomFichier;
  {$ENDIF}

  {$IFDEF MACOS}
    Result := TPath.Combine(TPath.GetDocumentsPath, NomFichier);
  {$ENDIF}

  {$IFDEF IOS}
    // Ressources embarquées dans le bundle
    Result := TPath.Combine(TPath.GetDocumentsPath, NomFichier);
  {$ENDIF}

  {$IFDEF ANDROID}
    // Assets internes
    Result := TPath.Combine(TPath.GetDocumentsPath, NomFichier);
  {$ENDIF}

  {$IFDEF LINUX}
    Result := TPath.Combine(TPath.GetHomePath, '.monapp', NomFichier);
  {$ENDIF}
end;
```

## 7. Tests multi-plateformes

### Stratégie de test

**Phase 1 : Développement**
- Développer principalement sur une plateforme (généralement Windows)
- Compiler régulièrement vers les autres plateformes

**Phase 2 : Tests fonctionnels**
- Tester sur chaque plateforme cible
- Vérifier le comportement spécifique

**Phase 3 : Tests sur appareils réels**
- Desktop : Différentes versions d'OS
- Mobile : Différents modèles et versions d'OS

### Configuration de test

**Pour iOS** :
```
Option 1 : Simulateur iOS (sur Mac)
- Rapide, gratuit
- Ne teste pas le matériel réel
- Performances différentes

Option 2 : Appareil physique
- Test réel
- Nécessite profil de développement
- Meilleur pour les tests finaux
```

**Pour Android** :
```
Option 1 : Émulateur Android
- Android Studio AVD Manager
- Lent au démarrage
- Gratuit

Option 2 : Appareil physique
- Plus rapide
- Test réel des capteurs
- Recommandé
```

**Pour macOS** :
```
Option 1 : Mac physique
- Idéal pour développement et tests
- Coûteux

Option 2 : Machine virtuelle
- VMware, VirtualBox (gris légalement)
- Performances réduites
```

**Pour Linux** :
```
Option 1 : Linux natif
- Performance complète
- Dual boot ou machine dédiée

Option 2 : Machine virtuelle
- VirtualBox, VMware
- Facile à configurer
- Bon pour les tests
```

### Checklist de tests par plateforme

**Windows** :
- [ ] Windows 10 (64-bit)
- [ ] Windows 11
- [ ] Différentes résolutions d'écran
- [ ] Mode DPI élevé
- [ ] UAC activé

**macOS** :
- [ ] macOS sur Intel
- [ ] macOS sur Apple Silicon (M1/M2)
- [ ] Différentes versions (10.14+)
- [ ] Retina display

**iOS** :
- [ ] iPhone (petit écran)
- [ ] iPhone Pro Max (grand écran)
- [ ] iPad
- [ ] Portrait et paysage
- [ ] iOS 15, 16, 17

**Android** :
- [ ] Smartphone petit écran (5")
- [ ] Smartphone grand écran (6.5")
- [ ] Tablette
- [ ] Android 8, 10, 12, 13
- [ ] Différents fabricants (Samsung, Google, Xiaomi)

**Linux** :
- [ ] Ubuntu 22.04 LTS
- [ ] Debian
- [ ] Fedora
- [ ] Environnements : GNOME, KDE, XFCE

## 8. Optimisation par plateforme

### Performances mobile vs desktop

```pascal
procedure TForm1.OptimiserSelonPlateforme;  
begin  
  {$IFDEF ANDROID OR IOS}
    // Mobile : Limiter les effets visuels
    EffetOmbre1.Enabled := False;
    EffetFlou1.Enabled := False;

    // Réduire la fréquence de mise à jour
    Timer1.Interval := 100;  // 10 FPS

    // Images basse résolution
    ChargerImagesMobile;
  {$ELSE}
    // Desktop : Activer tous les effets
    EffetOmbre1.Enabled := True;
    EffetFlou1.Enabled := True;

    // Haute fréquence
    Timer1.Interval := 16;  // ~60 FPS

    // Images haute résolution
    ChargerImagesHD;
  {$ENDIF}
end;
```

### Gestion de la mémoire

```pascal
procedure TForm1.GererMemoireSelonPlateforme;  
begin  
  {$IFDEF ANDROID OR IOS}
    // Mobile : Mémoire limitée
    // Libérer agressivement les ressources
    Image1.Bitmap.Clear;

    // Charger les images à la demande
    ChargerImageLazyLoading;
  {$ELSE}
    // Desktop : Mémoire abondante
    // Précharger pour la performance
    PrechargerImages;
  {$ENDIF}
end;
```

### Économie de batterie (mobile)

```pascal
procedure TForm1.ModeEconomieBatterie;  
begin  
  {$IFDEF ANDROID OR IOS}
    // Réduire la fréquence GPS
    LocationSensor1.Distance := 100;  // 100m au lieu de 10m

    // Désactiver les animations inutiles
    DesactiverAnimations;

    // Réduire la luminosité d'arrière-plan
    BackgroundProcess.Interval := 5000;  // 5 sec au lieu de 1 sec
  {$ENDIF}
end;
```

## 9. Déploiement et distribution

### Windows : Distribution

**Options** :
1. **Fichier EXE simple**
   - Copier l'exe et les DLLs
   - Distribution par téléchargement direct

2. **Installateur**
   - Inno Setup (gratuit)
   - InstallAware (commercial)
   - Advanced Installer

3. **Microsoft Store**
   - Empaqueter en MSIX
   - Processus de soumission Microsoft

### macOS : Distribution

**Options** :
1. **Package DMG**
   - Créer une image disque
   - Drag & Drop installation

2. **Mac App Store**
   - Compte développeur Apple requis (99$/an)
   - Processus de révision strict
   - Sandboxing obligatoire

**Signature de code** :
```bash
# Signer l'application
codesign --deep --force --verify --verbose \
  --sign "Developer ID Application: Votre Nom" \
  MonApp.app

# Notariser pour macOS 10.15+
xcrun notarytool submit MonApp.app \
  --apple-id votre@email.com \
  --password motdepasse \
  --team-id TEAMID
```

### iOS : Distribution

**Via TestFlight (test)** :
1. Archive l'application dans Xcode
2. Upload vers App Store Connect
3. Inviter les testeurs
4. Distribution automatique

**Via App Store (production)** :
1. Créer la fiche App Store
2. Préparer les captures d'écran (tous les formats)
3. Soumettre pour révision
4. Attendre approbation (1-7 jours)
5. Publication

### Android : Distribution

**Options** :
1. **APK direct**
   - Distribution hors store
   - Utilisateurs doivent autoriser "Sources inconnues"

2. **Google Play Store**
   - Compte développeur (25$ une fois)
   - Processus de révision automatisé
   - Publication en quelques heures

3. **Stores alternatifs**
   - Amazon Appstore
   - Samsung Galaxy Store
   - F-Droid (open source)

**Signature de l'APK** :
```pascal
// Configuration dans Delphi
Project → Options → Provisioning → Android
- Créer un keystore
- Signer l'APK

// Ou via ligne de commande
jarsigner -verbose -sigalg SHA1withRSA -digestalg SHA1 \
  -keystore MonKeystore.keystore MonApp.apk mon_alias
```

### Linux : Distribution

**Options** :
1. **Binaire simple**
   - Distribuer l'exécutable avec dépendances
   - Script d'installation shell

2. **Package .deb (Debian/Ubuntu)**
   ```bash
   dpkg-deb --build monapp
   ```

3. **Package .rpm (Red Hat/Fedora)**
   ```bash
   rpmbuild -ba monapp.spec
   ```

4. **AppImage (portable)**
   - Un seul fichier pour toutes les distributions
   - Pas d'installation nécessaire

5. **Snap / Flatpak**
   - Formats modernes
   - Sandboxing

## 10. Bonnes pratiques multi-plateformes

### ✅ À FAIRE

**1. Tester régulièrement sur toutes les plateformes cibles**
```pascal
// Ne pas attendre la fin du projet
// Compiler pour toutes les plateformes chaque semaine
```

**2. Isoler le code spécifique aux plateformes**
```pascal
// Créer des unités séparées par plateforme
unit PlatformUtils;

interface
  procedure InitPlateforme;
  function ObtenirInfosPlateforme: string;

implementation

{$IFDEF MSWINDOWS}
  // Implémentation Windows
{$ENDIF}

{$IFDEF ANDROID}
  // Implémentation Android
{$ENDIF}
```

**3. Utiliser les services FMX quand disponibles**
```pascal
// Préférer les services FMX multi-plateformes
uses FMX.DialogService;

TDialogService.ShowMessage('Message');
// Fonctionne partout automatiquement
```

**4. Respecter les conventions de chaque plateforme**
```pascal
{$IFDEF IOS}
  // Boutons de navigation iOS à gauche
  ButtonRetour.Align := TAlignLayout.Left;
{$ENDIF}

{$IFDEF ANDROID}
  // Bouton retour système Android géré automatiquement
{$ENDIF}
```

**5. Prévoir des ressources adaptées**
```pascal
// Images en plusieurs résolutions
// Icônes pour chaque plateforme
// Textes localisés
```

### ❌ À ÉVITER

**1. Code plateforme sans directives**
```pascal
// ❌ MAUVAIS : Ne compile pas sur toutes les plateformes
uses WinAPI.Windows;  // Erreur sur macOS, iOS, Android, Linux
```

**2. Chemins de fichiers en dur**
```pascal
// ❌ MAUVAIS
Fichier := 'C:\Temp\data.txt';  // Ne fonctionne que sur Windows

// ✅ BON
Fichier := TPath.Combine(TPath.GetTempPath, 'data.txt');
```

**3. Supposer les capacités du système**
```pascal
// ❌ MAUVAIS : Supposer que le GPS est disponible
// Toujours vérifier les capacités
```

**4. Ignorer les permissions mobiles**
```pascal
// ❌ MAUVAIS : Accéder à la caméra sans permission
// Toujours demander les permissions sur mobile
```

**5. Interface identique partout**
```pascal
// ❌ MAUVAIS : Forcer le même look sur toutes les plateformes
// Adapter l'interface aux conventions de chaque système
```

## 11. Dépannage par plateforme

### Problèmes Windows

**Problème** : DLL manquantes
```
Solution : Déployer les DLLs nécessaires avec l'exe  
ou utiliser un installateur qui les installe  
```

**Problème** : Permissions insuffisantes
```
Solution : Demander l'élévation UAC si nécessaire  
ou éviter d'écrire dans Program Files  
```

### Problèmes macOS

**Problème** : "App endommagée" au lancement
```
Solution : Signer correctement l'application  
ou demander à l'utilisateur d'autoriser dans Sécurité & Confidentialité  
```

**Problème** : Connexion PAServer échoue
```
Solution : Vérifier le firewall sur le Mac  
S'assurer que PAServer est en cours d'exécution  
Vérifier l'IP et le mot de passe  
```

### Problèmes iOS

**Problème** : Profil de provisionnement invalide
```
Solution : Régénérer le profil sur developer.apple.com  
Télécharger et réinstaller dans Xcode  
```

**Problème** : App crash au lancement
```
Solution : Vérifier les logs dans Xcode  
Souvent lié aux permissions ou ressources manquantes  
```

### Problèmes Android

**Problème** : Permission refusée
```
Solution : Déclarer dans AndroidManifest.xml  
Demander à l'exécution pour Android 6.0+  
```

**Problème** : APK ne s'installe pas
```
Solution : Vérifier la signature  
Vérifier la version d'Android cible  
Autoriser sources inconnues  
```

### Problèmes Linux

**Problème** : Bibliothèques manquantes
```
Solution : Installer les dépendances  
sudo apt-get install libgtk-3-0  
```

**Problème** : Connexion PAServer échoue
```
Solution : Vérifier le firewall Linux  
S'assurer que le port est ouvert  
```

## Conclusion

Le ciblage multi-plateforme est l'un des atouts majeurs de FireMonkey, mais nécessite de comprendre les spécificités de chaque système. Les points clés à retenir :

🎯 **Configuration** : Chaque plateforme nécessite une configuration spécifique

🎯 **Code conditionnel** : Utilisez les directives de compilation pour le code spécifique

🎯 **Tests** : Testez régulièrement sur toutes les plateformes cibles

🎯 **Conventions** : Respectez les standards de chaque système

🎯 **Ressources** : Préparez des assets adaptés à chaque plateforme

🎯 **Distribution** : Chaque plateforme a son propre processus de déploiement

🎯 **Permissions** : Gérez correctement les permissions sur mobile

Avec une bonne compréhension de ces aspects, vous pourrez créer des applications qui fonctionnent parfaitement sur Windows, macOS, iOS, Android et Linux, en tirant parti des forces de chaque plateforme tout en maintenant un code source unique et maintenable.

⏭️ [Performances et optimisation mobile](/05-developpement-multi-plateforme-avec-firemonkey/08-performances-et-optimisation-mobile.md)
