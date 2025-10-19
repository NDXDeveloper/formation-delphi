🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.6 Déploiement sur différentes plateformes

## Introduction

L'un des grands atouts de Delphi est sa capacité à créer des applications multi-plateformes à partir d'un seul code source. Avec Delphi 13, vous pouvez développer une application qui fonctionnera sur Windows, macOS, iOS, Android et même Linux, tout en maintenant une base de code largement commune.

Cependant, déployer sur différentes plateformes présente des défis spécifiques : chaque système d'exploitation a ses propres conventions, exigences et processus de distribution. Cette section vous guidera à travers les particularités de chaque plateforme et vous aidera à distribuer votre application Delphi efficacement, quelle que soit la cible.

## Vue d'ensemble des plateformes supportées

### Plateformes disponibles avec Delphi

Delphi 13 Florence supporte les plateformes suivantes :

| Plateforme | Framework | Architectures | Niveau de support |
|------------|-----------|---------------|-------------------|
| **Windows** | VCL / FMX | 32-bit, 64-bit | Excellent (natif) |
| **macOS** | FMX | 64-bit (Intel), ARM64 (M1/M2/M3) | Excellent |
| **iOS** | FMX | ARM64 (iPhone/iPad) | Très bon |
| **Android** | FMX | ARM, ARM64, x86 | Très bon |
| **Linux** | FMX | 64-bit | Bon (FMXLinux) |

### Choisir entre VCL et FireMonkey (FMX)

**VCL (Visual Component Library)** :
- Exclusivement pour Windows
- Interface native Windows
- Performances excellentes
- Composants riches et matures
- Idéal pour applications desktop Windows uniquement

**FireMonkey (FMX)** :
- Multi-plateforme (Windows, macOS, iOS, Android, Linux)
- Interface personnalisée (non native)
- Apparence moderne
- Un seul code pour toutes les plateformes
- Idéal pour applications devant tourner sur plusieurs systèmes

**Conseil pour débutants** : Si votre application doit fonctionner uniquement sur Windows, utilisez VCL. Si vous visez plusieurs plateformes, utilisez FMX dès le début.

## Déploiement sur Windows

Windows reste la plateforme principale pour Delphi, avec deux architectures à considérer.

### Windows 32-bit vs 64-bit

#### Quand choisir 32-bit ?

**Avantages** :
- Compatible avec Windows 32-bit et 64-bit
- Taille de l'exécutable légèrement plus petite
- Certains composants tiers n'existent qu'en 32-bit

**Inconvénients** :
- Limité à ~2 Go de mémoire par processus
- Moins de performances pour les calculs intensifs
- De plus en plus obsolète

#### Quand choisir 64-bit ?

**Avantages** :
- Accès à toute la mémoire disponible
- Meilleures performances
- Standard moderne
- Requis pour certaines API Windows modernes

**Inconvénients** :
- Ne fonctionne pas sur Windows 32-bit (rare aujourd'hui)
- Taille de l'exécutable légèrement plus grande

**Recommandation 2025** : Privilégiez le 64-bit. Windows 32-bit est quasiment obsolète.

### Configuration du déploiement Windows

#### Étape 1 : Sélectionner la plateforme cible

Dans Delphi :

1. **Gestionnaire de projets** (View → Project Manager)
2. Clic droit sur **Target Platforms**
3. **Add Platform** → Sélectionnez :
   - `Win32` pour Windows 32-bit
   - `Win64` pour Windows 64-bit

Vous pouvez avoir les deux plateformes dans le même projet.

#### Étape 2 : Compiler pour chaque plateforme

Dans la barre d'outils :
1. Sélectionnez la plateforme cible (`Win32` ou `Win64`)
2. Compilez : `Projet` → `Compiler` (Ctrl+F9)

Les exécutables seront créés dans :
- `Win32\Release\MonApp.exe` (32-bit)
- `Win64\Release\MonApp.exe` (64-bit)

#### Étape 3 : Gérer les dépendances

**Fichiers à distribuer avec votre application Windows** :

**Pour VCL** :
- Votre `.exe`
- Éventuelles DLL tierces
- Fichiers de ressources (images, bases de données, etc.)

**Pour FMX** :
- Votre `.exe`
- Fichiers de style (.style) si personnalisés
- Éventuelles DLL tierces
- Fichiers de ressources

**DLL système courantes** :
- `midas.dll` (si vous utilisez DBExpress)
- Drivers de base de données (MySQL, SQLite, etc.)
- Runtime Visual C++ si nécessaire

#### Vérifier les dépendances

Utilisez **Dependency Walker** (depends.exe) :

1. Téléchargez depuis : http://www.dependencywalker.com/
2. Ouvrez votre `.exe` dans Dependency Walker
3. Identifiez les DLL manquantes (affichées en rouge)
4. Incluez ces DLL dans votre installation

### Conventions Windows

**Emplacements d'installation** :
```
C:\Program Files\VotreApp\              (64-bit)
C:\Program Files (x86)\VotreApp\        (32-bit)
```

**Données utilisateur** :
```
%APPDATA%\VotreApp\                     (données roaming)
%LOCALAPPDATA%\VotreApp\                (données locales)
%PROGRAMDATA%\VotreApp\                 (données communes)
```

**Clés de registre** :
```
HKEY_LOCAL_MACHINE\Software\VotreApp\   (configuration système)
HKEY_CURRENT_USER\Software\VotreApp\    (configuration utilisateur)
```

### Déploiement via Microsoft Store

Pour distribuer sur le **Microsoft Store** :

#### Prérequis

- Compte développeur Microsoft (19$ par an ou gratuit pour certaines organisations)
- Application packagée en MSIX (nouveau format)
- Conformité aux exigences du Store

#### Créer un package MSIX

1. **Installer le SDK Windows** avec les outils de packaging
2. **Utiliser l'outil MSIX Packaging Tool** :
   - Disponible gratuitement sur le Microsoft Store
   - Permet de convertir votre installateur en MSIX

3. **Configuration du manifeste** :
   - Nom de l'application
   - Éditeur (certificat)
   - Icônes et captures d'écran
   - Capacités requises

4. **Soumettre au Microsoft Store** :
   - Via le Partner Center
   - Processus de certification (1-3 jours)

**Avantages du Microsoft Store** :
- Visibilité accrue
- Mises à jour automatiques gérées par Windows
- Système de paiement intégré

**Inconvénients** :
- Processus de validation long
- Restrictions sur certaines fonctionnalités
- Commission de 15-30%

## Déploiement sur macOS

### Prérequis

Pour déployer sur macOS, vous avez besoin de :

1. **Un Mac** avec macOS 10.14 ou supérieur
2. **Xcode** installé (gratuit depuis l'App Store)
3. **Delphi** avec la licence FMX
4. **Connexion réseau** entre votre PC Windows et le Mac (PAServer)

### Configuration de PAServer

**PAServer** (Platform Assistant Server) est un serveur qui s'exécute sur le Mac et permet à Delphi (sur Windows) de compiler et déployer des applications macOS.

#### Installation sur Mac

1. **Copier PAServer** depuis votre installation Delphi :
   - Emplacement : `C:\Program Files (x86)\Embarcadero\Studio\23.0\PAServer\PAServer-23.0.pkg`
   - Transférez ce fichier sur votre Mac (USB, réseau, etc.)

2. **Installer PAServer** :
   - Double-cliquez sur le `.pkg`
   - Suivez l'assistant d'installation

3. **Lancer PAServer** :
   - Ouvrez le Terminal
   - Tapez : `paserver` puis Entrée
   - Notez le mot de passe affiché

#### Configuration dans Delphi

1. **Ajouter la plateforme macOS** :
   - Project Manager → Target Platforms
   - Add Platform → `macOS 64-bit`

2. **Configurer la connexion** :
   - Clic droit sur `macOS 64-bit` → Properties
   - **Profile Name** : Donnez un nom (ex: "Mon Mac")
   - **Host Name** : Adresse IP de votre Mac (ex: 192.168.1.100)
   - **Port** : 64211 (par défaut)
   - **Password** : Celui affiché par PAServer
   - Testez la connexion

3. **Premier déploiement** :
   - Sélectionnez `macOS 64-bit` comme plateforme
   - Compilez et exécutez (F9)
   - L'application se lance sur le Mac !

### Structure d'une application macOS

Les applications macOS sont des **bundles** (paquets) avec l'extension `.app`. Exemple :

```
MonApp.app/
├── Contents/
    ├── Info.plist              (métadonnées)
    ├── MacOS/
    │   └── MonApp              (exécutable)
    ├── Resources/
    │   ├── MonApp.icns         (icône)
    │   └── ...                 (ressources)
    └── Frameworks/             (bibliothèques)
```

### Signature et notarisation macOS

Depuis macOS Catalina (10.15), Apple exige :

1. **Signature avec certificat Developer ID**
2. **Notarisation** par Apple

#### Obtenir un certificat Apple

1. **Inscrivez-vous au Apple Developer Program** :
   - Coût : 99$ par an
   - Site : https://developer.apple.com/programs/

2. **Créer un certificat** :
   - Apple Developer Portal → Certificates
   - Créez un "Developer ID Application"
   - Téléchargez et installez le certificat sur votre Mac

#### Signer l'application

```bash
# Sur le Mac, dans le Terminal
codesign --deep --force --verify --verbose \
  --sign "Developer ID Application: Votre Nom (TEAM_ID)" \
  MonApp.app
```

#### Notariser l'application

```bash
# Créer une archive
ditto -c -k --keepParent MonApp.app MonApp.zip

# Soumettre pour notarisation
xcrun notarytool submit MonApp.zip \
  --apple-id "votre@email.com" \
  --password "mot-de-passe-app-specific" \
  --team-id "TEAM_ID" \
  --wait

# Une fois approuvé, agrafer le ticket
xcrun stapler staple MonApp.app
```

**Important** : Sans notarisation, macOS affichera un message d'avertissement et l'utilisateur devra passer par les préférences système pour autoriser l'application.

### Distribution sur macOS

#### Distribution directe

**Option 1 : Image disque (.dmg)**

Créez une image disque avec votre application :

```bash
# Créer un dossier temporaire
mkdir MonApp_Package
cp -R MonApp.app MonApp_Package/

# Créer le DMG
hdiutil create -volname "Mon Application" \
  -srcfolder MonApp_Package \
  -ov -format UDZO MonApp.dmg
```

L'utilisateur monte le DMG et glisse l'application dans son dossier Applications.

**Option 2 : Package Installer (.pkg)**

Créez un installateur macOS classique avec **Packages** (outil gratuit).

#### Mac App Store

Pour distribuer sur le **Mac App Store** :

1. **Certificat différent** : "Mac App Store Application"
2. **Sandbox obligatoire** : L'application doit respecter le sandboxing
3. **Pas de notarisation nécessaire** : Gérée par le Store
4. **Processus de soumission** via App Store Connect

**Avantages** :
- Visibilité
- Mises à jour automatiques
- Confiance des utilisateurs

**Inconvénients** :
- Restrictions du sandbox (accès limité au système)
- Commission 15-30%
- Processus de validation (1-7 jours)

### Spécificités macOS

#### Différences d'interface

- **Menu dans la barre supérieure**, pas dans la fenêtre
- **Boutons de fermeture à gauche** (rouge, jaune, vert)
- **Pas de menu "Fichier → Quitter"** : utilisez le menu de l'application

**Adaptation dans le code** :

```pascal
{$IFDEF MACOS}
  // macOS : Menu dans la barre système
  MainMenu.SystemMenu := TSystemMenu.MacSystemMenu;
{$ENDIF}
```

#### Gestion des préférences

Sur macOS, utilisez `NSUserDefaults` :

```pascal
uses
  Macapi.Foundation, Macapi.Helpers;

procedure SavePreference(const Key, Value: string);
var
  Defaults: NSUserDefaults;
begin
  Defaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  Defaults.setObject(
    TNSString.OCClass.stringWithString(StrToNSStr(Value)),
    StrToNSStr(Key)
  );
  Defaults.synchronize;
end;
```

## Déploiement sur iOS (iPhone / iPad)

### Prérequis

1. **Mac** avec Xcode
2. **Apple Developer Program** (99$/an)
3. **Device physique iOS** (pour tests sur appareil réel)
4. **Certificats et profils de provisioning**

### Configuration iOS

#### Étape 1 : Certificats de développement

1. **Apple Developer Portal** :
   - Certificates → iOS Development
   - Créez un certificat de développement
   - Téléchargez et installez sur votre Mac

2. **Profil de provisioning** :
   - Profiles → iOS Development
   - Créez un profil incluant votre appareil
   - Téléchargez

#### Étape 2 : Configuration dans Delphi

1. **Ajouter la plateforme iOS** :
   - Project Manager → Add Platform → iOS Device 64-bit

2. **Configurer le SDK** :
   - Tools → Options → SDK Manager
   - Ajoutez le SDK iOS depuis votre Mac (via PAServer)

3. **Options de projet iOS** :
   - Project → Options → Provisioning (iOS)
   - Sélectionnez votre profil de provisioning

#### Étape 3 : Déployer sur l'appareil

1. **Connectez votre iPhone/iPad** au Mac via USB
2. Dans Delphi, sélectionnez **iOS Device 64-bit**
3. Appuyez sur **F9** (Run)
4. L'application se déploie sur votre appareil !

### Structure de l'application iOS

Une application iOS est un bundle `.app` similaire à macOS :

```
MonApp.app/
├── Info.plist
├── MonApp                  (exécutable ARM64)
├── Assets.car              (ressources graphiques)
├── Default.png             (splash screen)
└── ...
```

### Distribution iOS

#### Distribution Ad-Hoc (test)

Pour distribuer à un groupe restreint de testeurs :

1. **Créer un profil Ad-Hoc** :
   - Incluez les UDID des appareils testeurs
   - Maximum 100 appareils par an

2. **Distribuer le fichier IPA** :
   - Build → Archive
   - Export → Ad-Hoc
   - Partagez le `.ipa` avec vos testeurs

#### TestFlight (test beta)

**TestFlight** est la plateforme de test d'Apple :

1. **Créer une archive** :
   - Project → Deployment → Archive

2. **Uploader vers App Store Connect** :
   - Via Application Loader ou Xcode

3. **Inviter des testeurs** :
   - Jusqu'à 10 000 testeurs externes
   - Pas de limite pour les testeurs internes

4. **Distribuer** :
   - Les testeurs téléchargent TestFlight
   - Reçoivent une invitation
   - Installent votre application

#### App Store

**Processus de soumission** :

1. **Préparer l'application** :
   - Icônes (plusieurs tailles requises)
   - Captures d'écran (iPhone, iPad, différentes tailles)
   - Description, mots-clés
   - Politique de confidentialité

2. **Créer l'application** dans App Store Connect

3. **Uploader le build** :
   - Via Delphi (Deployment → Upload to App Store)
   - Ou via Xcode/Application Loader

4. **Soumettre à la revue** :
   - Processus de validation : 1-7 jours
   - Apple teste l'application

5. **Publication** :
   - Automatique ou planifiée après approbation

**Exigences App Store** :
- Respect des Human Interface Guidelines
- Pas de code privé ou non documenté
- Politique de confidentialité obligatoire
- Fonctionnalités complètes (pas de démo)

### Spécificités iOS

#### Gestion de la mémoire

iOS a une **gestion stricte de la mémoire** :

```pascal
procedure TForm1.FormActivate(Sender: TObject);
begin
  // iOS peut fermer l'app en arrière-plan
  // Sauvegardez les données importantes
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  // L'app passe en arrière-plan
  SaveData; // Sauvegarde automatique
end;
```

#### Permissions

Demandez les permissions nécessaires dans `Info.plist` :

```xml
<key>NSCameraUsageDescription</key>
<string>Nous avons besoin d'accéder à votre caméra pour scanner des codes-barres</string>

<key>NSLocationWhenInUseUsageDescription</key>
<string>Nous utilisons votre localisation pour vous montrer les magasins à proximité</string>
```

#### Orientations

Définissez les orientations supportées :

```pascal
// Project → Options → Application → Orientation
// Portrait, Landscape Left, Landscape Right, Portrait Upside Down
```

## Déploiement sur Android

### Prérequis

1. **Android SDK** (installé avec Delphi ou séparément)
2. **JDK** (Java Development Kit)
3. **Appareil Android** ou émulateur
4. Éventuellement : **Compte Google Play Developer** (25$ unique)

### Configuration Android

#### Étape 1 : SDK Manager

Dans Delphi :

1. **Tools → Options → SDK Manager**
2. Vérifiez que le **Android SDK** est correctement configuré
3. Installez les composants nécessaires :
   - Android SDK Platform-Tools
   - Android SDK Build-Tools
   - Android API Level 33+ (recommandé)

#### Étape 2 : Ajouter la plateforme

1. **Project Manager** → Add Platform
2. Sélectionnez :
   - `Android (32-bit ARM)`
   - `Android (64-bit ARM)` - **Requis par Google Play depuis 2019**

**Important** : Google Play exige le support 64-bit. Compilez toujours pour les deux architectures.

#### Étape 3 : Configuration du projet

**Options importantes** :

1. **Project → Options → Application (Android)** :
   - **Version Code** : Numéro interne (incrémenté à chaque version)
   - **Version Name** : Version visible (ex: 1.0.0)
   - **Package Name** : Identifiant unique (ex: com.monentreprise.monapp)
   - **Min SDK Version** : API minimum supportée (21 = Android 5.0)
   - **Target SDK Version** : API ciblée (33+ recommandé)

2. **Permissions** :
   - Cochez les permissions nécessaires
   - Internet, caméra, stockage, etc.

### Signature de l'application Android

**Toutes les applications Android doivent être signées**.

#### Créer un keystore

```bash
# Via la ligne de commande
keytool -genkey -v -keystore MonApp.keystore \
  -alias MonAppKey -keyalg RSA -keysize 2048 -validity 10000

# Vous serez invité à entrer :
# - Un mot de passe du keystore
# - Des informations sur vous/votre organisation
# - Un mot de passe pour la clé
```

**Important** : **Ne perdez jamais ce keystore !** Sans lui, vous ne pourrez plus mettre à jour votre application sur Google Play.

#### Configurer la signature dans Delphi

1. **Project → Options → Provisioning (Android)**
2. **Key Configuration** :
   - **Keystore File** : Sélectionnez votre `.keystore`
   - **Keystore Password** : Mot de passe du keystore
   - **Key Alias** : L'alias de la clé (ex: MonAppKey)
   - **Key Password** : Mot de passe de la clé

### Création de l'APK/AAB

#### APK (Android Package)

Format traditionnel pour distribuer des applications Android.

**Créer un APK** :
1. Sélectionnez `Android (64-bit ARM)` (ou les deux architectures)
2. **Project → Build Configuration → Release**
3. **Compile** (Ctrl+F9)
4. L'APK se trouve dans : `Android64\Release\MonApp.apk`

#### AAB (Android App Bundle)

Format moderne requis par Google Play depuis 2021.

**Créer un AAB** :
1. **Project → Deployment**
2. Sélectionnez **Application Store** (au lieu de Debug)
3. Cochez **Generate Android App Bundle (.aab)**
4. Compilez
5. L'AAB se trouve dans : `Android64\Release\MonApp.aab`

**Avantages de l'AAB** :
- Google Play génère des APK optimisés pour chaque appareil
- Taille de téléchargement réduite pour les utilisateurs
- Requis pour les nouvelles applications sur Play Store

### Distribution Android

#### Distribution directe (APK)

Vous pouvez distribuer l'APK directement :

- Via votre site web
- Par email
- Magasins alternatifs (Amazon Appstore, Samsung Galaxy Store, etc.)

**L'utilisateur doit autoriser les sources inconnues** sur son appareil.

#### Google Play Store

**Processus de publication** :

1. **Créer un compte Google Play Developer** :
   - Coût unique : 25$
   - Via https://play.google.com/console/

2. **Créer une application** :
   - Play Console → Créer une application
   - Nom, catégorie, contact

3. **Préparer la fiche** :
   - **Description** : Courte et longue
   - **Icône** : 512×512 px
   - **Captures d'écran** : Plusieurs tailles (téléphone, tablette)
   - **Vidéo** (optionnel mais recommandé)
   - **Catégorie et tags**

4. **Questionnaire de contenu** :
   - Classification du contenu
   - Public cible
   - Politique de confidentialité (obligatoire)

5. **Uploader l'AAB** :
   - Production → Versions → Créer une version
   - Uploadez le fichier `.aab`
   - Notes de version

6. **Soumettre à la revue** :
   - Validation : quelques heures à quelques jours
   - Google teste automatiquement l'application

7. **Publication** :
   - Déploiement progressif possible (5%, 10%, 50%, 100%)
   - Disponible dans ~24h

### Spécificités Android

#### Fragmentation des appareils

Android tourne sur des **milliers d'appareils différents** :

- Différentes tailles d'écran
- Différentes densités (DPI)
- Différentes versions d'Android

**Testez sur plusieurs appareils** ou utilisez un service comme **Firebase Test Lab**.

#### Permissions runtime

Depuis Android 6.0, certaines permissions doivent être demandées à l'exécution :

```pascal
uses
  FMX.Types, Androidapi.Helpers, Androidapi.JNI.Os;

procedure TForm1.RequestCameraPermission;
begin
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions(
    ['android.permission.CAMERA'],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        // Permission accordée
        OpenCamera;
      end
      else
        ShowMessage('Permission caméra refusée');
    end
  );
  {$ENDIF}
end;
```

#### Gestion du bouton Retour

Sur Android, le bouton **Retour** doit être géré :

```pascal
procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  {$IFDEF ANDROID}
  if Key = vkHardwareBack then
  begin
    Key := 0; // Empêcher le comportement par défaut

    // Votre logique
    if CanGoBack then
      GoBack
    else
      // Demander confirmation de fermeture
      if MessageDlg('Quitter l''application ?', TMsgDlgType.mtConfirmation,
                    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
        Close;
  end;
  {$ENDIF}
end;
```

#### Icônes adaptatives

Android 8.0+ utilise des **icônes adaptatives** :

- **Foreground** : Élément principal (logo)
- **Background** : Arrière-plan uni ou dégradé

Fournissez les deux couches dans vos ressources.

## Déploiement sur Linux

Avec Delphi 13 et **FMXLinux**, vous pouvez créer des applications Linux graphiques.

### Prérequis

1. **Machine Linux** (physique ou virtuelle)
   - Ubuntu 20.04+ recommandé
   - Debian, CentOS aussi supportés

2. **PAServer pour Linux** installé sur la machine Linux

### Configuration Linux

#### Installation de PAServer sur Linux

1. **Transférez PAServer** sur votre machine Linux :
   ```bash
   scp PAServer-23.0.tar.gz user@linux-machine:/tmp/
   ```

2. **Installez** :
   ```bash
   cd /tmp
   tar -xzf PAServer-23.0.tar.gz
   cd PAServer-23.0
   ./setup.sh
   ```

3. **Lancez PAServer** :
   ```bash
   paserver
   ```

#### Configuration dans Delphi

1. **Ajoutez la plateforme** :
   - Project Manager → Add Platform → Linux 64-bit

2. **Configurez la connexion** :
   - Similaire à macOS
   - Adresse IP de votre machine Linux
   - Mot de passe PAServer

3. **Compilez et exécutez** :
   - Sélectionnez `Linux 64-bit`
   - F9 pour compiler et exécuter

### Distribution Linux

#### Formats de packages

**1. Archive TAR.GZ**

Le plus simple : une archive avec votre exécutable et ses dépendances.

```bash
tar -czf MonApp-1.0.0.tar.gz MonApp libs/ resources/
```

L'utilisateur extrait et exécute.

**2. DEB (Debian/Ubuntu)**

Créez un package `.deb` pour Debian et Ubuntu :

Structure :
```
MonApp_1.0.0_amd64/
├── DEBIAN/
│   └── control          (métadonnées)
├── usr/
    ├── bin/
    │   └── MonApp       (exécutable)
    └── share/
        └── applications/
            └── MonApp.desktop  (lanceur)
```

Créez le package :
```bash
dpkg-deb --build MonApp_1.0.0_amd64
```

**3. RPM (RedHat/CentOS/Fedora)**

Utilisez `rpmbuild` pour créer un package `.rpm`.

**4. AppImage**

Format universel fonctionnant sur toutes les distributions :

- Un seul fichier
- Pas d'installation nécessaire
- Inclut toutes les dépendances

**5. Flatpak / Snap**

Formats modernes pour la distribution d'applications Linux :

- **Flatpak** : Support large, sandboxing
- **Snap** : Créé par Canonical (Ubuntu)

### Spécificités Linux

#### Dépendances système

Votre application peut nécessiter des bibliothèques système :

```bash
# Identifier les dépendances
ldd MonApp

# Résultat exemple :
#   libQt5Core.so.5 => /lib/x86_64-linux-gnu/libQt5Core.so.5
#   libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0
```

**Incluez les dépendances** ou documentez-les dans le README.

#### Permissions d'exécution

Sur Linux, les fichiers ne sont pas exécutables par défaut :

```bash
chmod +x MonApp
```

Votre installateur doit gérer cela.

#### Intégration de bureau

Créez un fichier `.desktop` pour intégrer votre application au menu :

```ini
[Desktop Entry]
Type=Application
Name=Mon Application
Comment=Description de mon application
Exec=/usr/bin/MonApp
Icon=/usr/share/icons/MonApp.png
Categories=Utility;
Terminal=false
```

## Gestion multi-plateforme du code

### Directives de compilation conditionnelle

Utilisez les directives pour gérer les spécificités de chaque plateforme :

```pascal
uses
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF MACOS}
  Macapi.Foundation,
  {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.Foundation,
  {$ENDIF}
  {$IFDEF LINUX}
  Posix.Stdlib,
  {$ENDIF}
  System.SysUtils;

procedure ShowPlatformInfo;
begin
  {$IFDEF MSWINDOWS}
  ShowMessage('Vous êtes sur Windows');
  {$ENDIF}

  {$IFDEF MACOS}
  ShowMessage('Vous êtes sur macOS');
  {$ENDIF}

  {$IFDEF ANDROID}
  ShowMessage('Vous êtes sur Android');
  {$ENDIF}

  {$IFDEF IOS}
  ShowMessage('Vous êtes sur iOS');
  {$ENDIF}

  {$IFDEF LINUX}
  ShowMessage('Vous êtes sur Linux');
  {$ENDIF}
end;
```

### Chemins de fichiers multi-plateformes

Utilisez `TPath` pour gérer les chemins :

```pascal
uses
  System.IOUtils;

var
  DocumentsPath: string;
  TempPath: string;
begin
  // Dossier documents
  DocumentsPath := TPath.GetDocumentsPath;

  // Dossier temporaire
  TempPath := TPath.GetTempPath;

  // Combiner des chemins
  ConfigFile := TPath.Combine(DocumentsPath, 'config.ini');
end;
```

**Résultats selon la plateforme** :

| Plateforme | Documents | Temp |
|------------|-----------|------|
| Windows | `C:\Users\...\Documents` | `C:\Users\...\AppData\Local\Temp` |
| macOS | `/Users/.../Documents` | `/var/folders/.../T` |
| iOS | `/var/mobile/.../Documents` | `/var/mobile/.../tmp` |
| Android | `/storage/emulated/0/Documents` | `/data/data/.../cache` |
| Linux | `/home/.../Documents` | `/tmp` |

### Différences d'interface

#### Boutons et contrôles

Sur mobile, adaptez la taille :

```pascal
{$IF DEFINED(IOS) OR DEFINED(ANDROID)}
  Button1.Height := 50; // Plus grand pour le tactile
{$ELSE}
  Button1.Height := 25; // Standard desktop
{$ENDIF}
```

#### Menus

Sur mobile, remplacez les menus par :
- **Hamburger menu** (tiroir latéral)
- **Tab bar** (barre d'onglets)
- **Action sheets** (feuilles d'actions)

## Tableau comparatif des plateformes

| Aspect | Windows | macOS | iOS | Android | Linux |
|--------|---------|-------|-----|---------|-------|
| **Framework** | VCL/FMX | FMX | FMX | FMX | FMX |
| **Signature requise** | Non (recommandé) | Oui | Oui | Oui | Non |
| **Certification requise** | Non | Oui (99$/an) | Oui (99$/an) | Non | Non |
| **Store principal** | Microsoft Store | Mac App Store | App Store | Play Store | - |
| **Frais Store** | 15-30% | 15-30% | 15-30% | 15-30% | - |
| **Distribution directe** | Facile | Possible | Difficile | Facile | Facile |
| **Validation** | 1-3 jours | 1-7 jours | 1-7 jours | Heures-jours | - |
| **Mises à jour** | Manuel/Auto | Manuel/Auto | Auto (Store) | Auto (Store) | Manuel |
| **Sandboxing** | Non | Store:Oui | Oui | Oui | Non |

## Bonnes pratiques multi-plateformes

### 1. Concevoir pour le multi-plateforme dès le début

Si vous savez que vous ciblerez plusieurs plateformes :

✅ **Utilisez FMX dès le départ**
✅ **Testez régulièrement sur toutes les plateformes**
✅ **Évitez les API spécifiques à une plateforme dans le code commun**

### 2. Séparer la logique métier de l'interface

```pascal
// Unité multi-plateforme (logique)
unit BusinessLogic;

// Unité spécifique (interface)
unit MainForm; // Windows
unit MainForm.iOS; // iOS
```

### 3. Adapter l'interface aux conventions

- **Windows** : Menu dans la fenêtre, clavier+souris
- **macOS** : Menu en haut, trackpad/souris
- **iOS/Android** : Gestes tactiles, pas de menu
- **Linux** : Similaire à Windows

### 4. Gérer les différentes résolutions

Utilisez des layouts adaptatifs :

```pascal
// Utiliser TLayout et anchors
Layout.Align := TAlignLayout.Client;
Button.Anchors := [TAnchorKind.akLeft, TAnchorKind.akBottom];

// Layouts différents selon orientation
if Form.Width > Form.Height then
  // Paysage
else
  // Portrait
```

### 5. Tester les performances

Les performances varient selon la plateforme :

- Desktop (Windows/macOS/Linux) : Généralement rapides
- Mobile (iOS/Android) : Plus limités, optimisez !

### 6. Gérer les ressources

Utilisez le **Deployment Manager** pour gérer les ressources par plateforme :

- Icônes spécifiques (tailles différentes)
- Images adaptées aux résolutions
- Fichiers de configuration

### 7. Documenter les spécificités

Documentez dans votre code :

```pascal
// NOTE: Sur iOS, cette fonctionnalité nécessite iOS 13+
// NOTE: Sur Android, demander permission STORAGE
```

## Checklist de déploiement multi-plateforme

Avant de distribuer sur chaque plateforme :

### Windows
- [ ] Compilé en 64-bit (et 32-bit si nécessaire)
- [ ] Testé sur Windows 10 et 11
- [ ] Toutes les DLL incluses
- [ ] Installateur créé et signé
- [ ] Testé sans droits admin

### macOS
- [ ] Application signée avec Developer ID
- [ ] Notarisation effectuée
- [ ] Bundle .app correct
- [ ] DMG ou PKG créé
- [ ] Testé sur Intel et Apple Silicon (M1/M2/M3)

### iOS
- [ ] Compilé pour ARM64
- [ ] Profil de provisioning correct
- [ ] Icônes toutes tailles fournies
- [ ] Captures d'écran préparées
- [ ] Politique de confidentialité en ligne
- [ ] TestFlight testé

### Android
- [ ] AAB créé et signé
- [ ] Support 32-bit et 64-bit
- [ ] Permissions déclarées
- [ ] Icône adaptative fournie
- [ ] Keystore sauvegardé en lieu sûr
- [ ] Testé sur plusieurs appareils/versions

### Linux
- [ ] Dépendances documentées
- [ ] Package créé (.deb, .rpm, ou .tar.gz)
- [ ] Fichier .desktop créé
- [ ] Permissions d'exécution correctes
- [ ] Testé sur Ubuntu et Debian

## Problèmes courants

### "Platform not found" ou "SDK not configured"

**Solution** : Vérifiez le SDK Manager et assurez-vous que tous les SDK sont installés.

### "Certificate not found" (macOS/iOS)

**Solution** : Vérifiez que vos certificats sont installés sur le Mac, pas sur le PC Windows.

### "App crashes on device but works in simulator"

**Solution** :
- Compilez en mode Release
- Vérifiez les autorisations
- Testez avec logs activés

### "Play Store reject: Missing 64-bit support"

**Solution** : Compilez pour `Android 64-bit ARM` en plus du 32-bit.

### Application signée mais SmartScreen/Gatekeeper bloque quand même

**Solution** :
- **Windows** : Construisez une réputation progressivement
- **macOS** : Vérifiez la notarisation avec `spctl --assess -vv MonApp.app`

## Conclusion

Le déploiement multi-plateforme avec Delphi offre une opportunité exceptionnelle de toucher un large public avec un seul code source. Chaque plateforme a ses spécificités, mais avec les bons outils et pratiques :

**Points clés à retenir** :

1. **Windows** : Privilégiez le 64-bit, signez votre code
2. **macOS** : Signature et notarisation obligatoires
3. **iOS** : Inscription Developer, processus App Store
4. **Android** : AAB requis pour Play Store, ne perdez pas votre keystore
5. **Linux** : Gérez les dépendances, plusieurs formats de distribution
6. **Multi-plateforme** : Utilisez FMX, testez régulièrement partout, adaptez les interfaces

Avec Delphi 13 Florence et ses capacités multi-plateformes, vous pouvez créer des applications qui touchent des milliards d'utilisateurs sur tous les systèmes d'exploitation majeurs. Dans la prochaine section, nous explorerons la virtualisation et les conteneurs Docker pour des déploiements modernes.

⏭️ [Virtualisation et conteneurs Docker](/17-distribution-et-deploiement/07-virtualisation-et-conteneurs-docker.md)
