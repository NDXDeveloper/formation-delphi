🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.8 MSI et Windows Store

## Introduction

Lorsque vous distribuez une application Windows, vous avez le choix entre plusieurs formats et canaux de distribution. Deux options professionnelles se distinguent particulièrement : les **packages MSI** (Microsoft Installer) et le **Microsoft Store** (anciennement Windows Store).

Ces deux approches offrent des avantages différents et répondent à des besoins spécifiques. Le format MSI est le standard professionnel pour les entreprises et l'administration système, tandis que le Microsoft Store s'adresse au grand public et offre une distribution simplifiée.

Dans cette section, nous explorerons ces deux méthodes en détail, leurs avantages, leurs inconvénients, et comment préparer votre application Delphi pour chacune d'elles.

## MSI : Microsoft Installer

### Qu'est-ce qu'un fichier MSI ?

**MSI** (Microsoft Installer) est le format d'installation standard de Microsoft pour Windows. C'est un fichier avec l'extension `.msi` qui contient :

- Les fichiers de votre application
- Les instructions d'installation
- Les informations de configuration du système
- Les données pour la désinstallation

**Différence avec un EXE** :

| Fichier EXE | Fichier MSI |
|-------------|-------------|
| Programme exécutable autonome | Package géré par Windows Installer |
| Installation personnalisée | Installation standardisée |
| Peu de contrôle système | Intégration complète avec Windows |
| Désinstallation parfois incomplète | Désinstallation propre garantie |

### Pourquoi utiliser le format MSI ?

#### 1. Déploiement en entreprise

Les entreprises préfèrent MSI car :
- **Déploiement centralisé** via Active Directory ou Group Policy
- **Installation silencieuse** en masse sur des centaines de postes
- **Gestion des versions** : mise à jour, réparation, désinstallation
- **Journalisation** : Logs détaillés de chaque installation

#### 2. Standardisation Windows

MSI est le format natif de Windows :
- **Intégré dans Windows** : Pas de logiciel supplémentaire nécessaire
- **Réparation automatique** : Détecte et répare les fichiers corrompus
- **Désinstallation propre** : Supprime tous les fichiers installés
- **Compatibilité** : Fonctionne sur toutes les versions de Windows

#### 3. Installation transactionnelle

L'installation MSI est **transactionnelle** :
- Si l'installation échoue, tout est annulé (rollback)
- Le système revient à son état initial
- Pas de fichiers orphelins en cas d'erreur

#### 4. Contrôle administrateur

Les administrateurs système apprécient :
- **Paramètres de ligne de commande** : Contrôle total sur l'installation
- **Transformations (MST)** : Personnalisation sans modifier le MSI
- **Patches (MSP)** : Mises à jour ciblées

### Structure d'un fichier MSI

Un MSI est une base de données structurée contenant :

```
[Base de données MSI]
├── Tables
│   ├── File (fichiers à installer)
│   ├── Directory (arborescence des dossiers)
│   ├── Registry (clés de registre)
│   ├── Shortcut (raccourcis)
│   ├── Component (composants)
│   ├── Feature (fonctionnalités)
│   └── ... (50+ tables)
├── Flux (Streams)
│   ├── Fichiers binaires (images, DLL, etc.)
│   ├── Scripts personnalisés
│   └── Cabinets (archives compressées)
└── Propriétés
    ├── ProductCode (GUID unique)
    ├── UpgradeCode (GUID pour les mises à jour)
    └── Version, Fabricant, etc.
```

### Créer un MSI avec WiX Toolset

**WiX** (Windows Installer XML) est l'outil gratuit et open source de Microsoft pour créer des MSI.

#### Installation de WiX

1. **Télécharger WiX** :
   - Site : https://wixtoolset.org/
   - Version recommandée : WiX v4 (ou v3 pour stabilité)

2. **Installer** :
   - Exécutez le programme d'installation
   - Suivez l'assistant

3. **Vérifier l'installation** :
   ```cmd
   candle -?
   light -?
   ```
   Si ces commandes fonctionnent, WiX est installé correctement.

#### Créer un MSI simple avec WiX

WiX utilise des fichiers XML pour définir l'installation.

**Étape 1 : Créer le fichier source (Product.wxs)**

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">

  <!-- Produit principal -->
  <Product Id="*"
           Name="Mon Application Delphi"
           Language="1036"
           Version="1.0.0"
           Manufacturer="Votre Entreprise"
           UpgradeCode="12345678-1234-1234-1234-123456789ABC">

    <!-- Package MSI -->
    <Package InstallerVersion="200"
             Compressed="yes"
             InstallScope="perMachine"
             Description="Mon Application Delphi v1.0" />

    <!-- Média (fichiers) -->
    <MediaTemplate EmbedCab="yes" />

    <!-- Fonctionnalité principale -->
    <Feature Id="ProductFeature"
             Title="Application principale"
             Level="1">
      <ComponentGroupRef Id="ProductComponents" />
    </Feature>
  </Product>

  <!-- Structure des dossiers -->
  <Fragment>
    <Directory Id="TARGETDIR" Name="SourceDir">
      <Directory Id="ProgramFilesFolder">
        <Directory Id="INSTALLFOLDER" Name="MonApplication" />
      </Directory>
      <Directory Id="ProgramMenuFolder">
        <Directory Id="ApplicationProgramsFolder" Name="Mon Application"/>
      </Directory>
    </Directory>
  </Fragment>

  <!-- Composants (fichiers à installer) -->
  <Fragment>
    <ComponentGroup Id="ProductComponents" Directory="INSTALLFOLDER">

      <!-- Exécutable principal -->
      <Component Id="MainExecutable" Guid="11111111-1111-1111-1111-111111111111">
        <File Id="MonAppEXE"
              Source="MonApp.exe"
              KeyPath="yes">

          <!-- Raccourci dans le menu Démarrer -->
          <Shortcut Id="StartMenuShortcut"
                    Directory="ApplicationProgramsFolder"
                    Name="Mon Application"
                    WorkingDirectory="INSTALLFOLDER"
                    Icon="AppIcon.exe"
                    IconIndex="0"
                    Advertise="yes" />
        </File>
      </Component>

      <!-- Fichiers additionnels -->
      <Component Id="ConfigFile" Guid="22222222-2222-2222-2222-222222222222">
        <File Id="ConfigINI" Source="config.ini" KeyPath="yes" />
      </Component>

    </ComponentGroup>
  </Fragment>

  <!-- Icône pour le raccourci -->
  <Icon Id="AppIcon.exe" SourceFile="MonApp.exe" />

</Wix>
```

**Explication des éléments clés** :

- **Product** : Définit le produit (nom, version, fabricant)
  - `Id="*"` : GUID généré automatiquement à chaque build
  - `UpgradeCode` : GUID fixe pour identifier les mises à jour

- **Package** : Configuration du package MSI
  - `InstallerVersion="200"` : Version 2.0 de Windows Installer (Windows 2000+)
  - `InstallScope="perMachine"` : Installation pour tous les utilisateurs

- **Feature** : Fonctionnalités installables
  - Permet aux utilisateurs de choisir quoi installer

- **Directory** : Structure des dossiers
  - `ProgramFilesFolder` : C:\Program Files\
  - `INSTALLFOLDER` : Dossier de votre application

- **Component** : Unité d'installation (fichiers, clés de registre, etc.)
  - Chaque composant a un GUID unique
  - `KeyPath` : Fichier représentant le composant

**Étape 2 : Compiler le MSI**

```cmd
REM 1. Compiler le fichier WXS en WIXOBJ  
candle Product.wxs  

REM 2. Lier pour créer le MSI  
light Product.wixobj -out MonApplication.msi  
```

**Étape 3 : Tester l'installation**

```cmd
REM Installation normale  
msiexec /i MonApplication.msi  

REM Installation silencieuse  
msiexec /i MonApplication.msi /quiet /norestart  

REM Désinstallation  
msiexec /x MonApplication.msi  
```

#### Fonctionnalités avancées de WiX

##### 1. Installation conditionnelle

Installez uniquement si certaines conditions sont remplies :

```xml
<Feature Id="OptionalFeature" Title="Fonctionnalité optionnelle" Level="1">
  <Condition Level="0">
    <!-- N'installe pas si .NET 6 n'est pas présent -->
    <![CDATA[NETFRAMEWORK60 <> "#1"]]>
  </Condition>
  <ComponentGroupRef Id="OptionalComponents" />
</Feature>
```

##### 2. Interface utilisateur personnalisée

WiX propose des interfaces prédéfinies :

```xml
<!-- Interface minimale -->
<UIRef Id="WixUI_Minimal" />

<!-- Interface avec choix de dossier -->
<UIRef Id="WixUI_InstallDir" />
<Property Id="WIXUI_INSTALLDIR" Value="INSTALLFOLDER" />

<!-- Interface complète avec features -->
<UIRef Id="WixUI_FeatureTree" />
```

##### 3. Détection de versions existantes

Empêchez l'installation de versions plus anciennes :

```xml
<MajorUpgrade
  DowngradeErrorMessage="Une version plus récente est déjà installée."
  AllowSameVersionUpgrades="yes" />
```

##### 4. Lancement d'application après installation

```xml
<CustomAction Id="LaunchApplication"
              FileKey="MonAppEXE"
              ExeCommand=""
              Execute="immediate"
              Impersonate="yes"
              Return="asyncNoWait" />

<InstallExecuteSequence>
  <Custom Action="LaunchApplication" After="InstallFinalize">
    NOT Installed AND NOT REMOVE
  </Custom>
</InstallExecuteSequence>
```

##### 5. Clés de registre

```xml
<Component Id="RegistryEntries" Guid="33333333-3333-3333-3333-333333333333">
  <RegistryKey Root="HKCU"
               Key="Software\MonEntreprise\MonApp"
               Action="createAndRemoveOnUninstall">
    <RegistryValue Type="string"
                   Name="Version"
                   Value="1.0.0"
                   KeyPath="yes" />
    <RegistryValue Type="string"
                   Name="InstallPath"
                   Value="[INSTALLFOLDER]" />
  </RegistryKey>
</Component>
```

### Créer un MSI avec des outils visuels

Si vous trouvez WiX trop complexe, utilisez des outils graphiques :

#### 1. InstallShield (Commercial)

**Avantages** :
- Interface graphique complète
- Assistant puissant
- Support professionnel
- Intégration Visual Studio/Delphi

**Inconvénients** :
- Payant (plusieurs centaines de dollars)
- Complexe pour débuter

**Site** : https://www.revenera.com/install/products/installshield

#### 2. Advanced Installer (Commercial avec version gratuite)

**Avantages** :
- Interface moderne et intuitive
- Version gratuite pour usage personnel
- Fonctionnalités professionnelles
- Excellent support MSI

**Inconvénients** :
- Version gratuite limitée
- Version professionnelle payante (~500$)

**Site** : https://www.advancedinstaller.com/

**Créer un MSI avec Advanced Installer (version gratuite)** :

1. **Installer Advanced Installer**
2. **Nouveau projet** : Fichier → Nouveau → MSI
3. **Product Details** : Nom, version, éditeur
4. **Files and Folders** : Ajoutez votre exécutable et fichiers
5. **Shortcuts** : Créez des raccourcis (menu Démarrer, bureau)
6. **Build** : Générez le MSI

C'est beaucoup plus visuel et intuitif que WiX !

### Déploiement MSI en entreprise

#### Installation silencieuse

Les administrateurs système utilisent souvent l'installation silencieuse :

```cmd
REM Installation silencieuse de base  
msiexec /i MonApp.msi /quiet  

REM Avec log détaillé  
msiexec /i MonApp.msi /quiet /l*v install.log  

REM Avec paramètres personnalisés  
msiexec /i MonApp.msi /quiet INSTALLFOLDER="C:\MonDossier" ADDLOCAL=ALL  

REM Désinstallation silencieuse  
msiexec /x MonApp.msi /quiet  
```

#### Déploiement via Group Policy (Active Directory)

Dans un domaine Windows, les administrateurs peuvent :

1. **Placer le MSI** sur un partage réseau
2. **Créer une GPO** (Group Policy Object)
3. **Configurer le déploiement** :
   - Installation assignée (automatique)
   - Installation publiée (disponible dans "Programmes et fonctionnalités")
4. **Appliquer aux ordinateurs/utilisateurs**

Le MSI s'installe automatiquement sur tous les postes du domaine.

#### Transformations (MST)

Les fichiers **MST** (Microsoft Transform) permettent de personnaliser un MSI sans le modifier :

```cmd
REM Créer une transformation  
msitran.exe -g MonApp.msi MonApp.mst  

REM Appliquer une transformation  
msiexec /i MonApp.msi TRANSFORMS=MonApp.mst  
```

**Utilité** : Personnaliser l'installation pour différents services/sites.

### Mises à jour et patches MSI

#### Mises à jour majeures

Pour une nouvelle version :

```xml
<Product Id="*"
         Version="2.0.0"
         UpgradeCode="12345678-1234-1234-1234-123456789ABC">

  <!-- Même UpgradeCode, nouveau ProductCode automatique -->
  <MajorUpgrade DowngradeErrorMessage="Version plus récente installée." />

</Product>
```

Lors de l'installation du nouveau MSI, l'ancien est automatiquement désinstallé.

#### Patches (MSP)

Pour des corrections mineures sans réinstallation complète :

```cmd
REM Créer un patch MSP avec WiX  
torch.exe v1.0.wixpdb v1.1.wixpdb -out diff.wixmst  
pyro.exe diff.wixmst -out patch.msp  
```

L'utilisateur applique le patch qui met à jour uniquement les fichiers modifiés.

## Microsoft Store (Windows Store)

### Qu'est-ce que le Microsoft Store ?

Le **Microsoft Store** est la plateforme officielle de distribution d'applications pour Windows. C'est l'équivalent de l'App Store d'Apple ou du Play Store de Google.

**Avantages pour les développeurs** :

1. **Visibilité** : Des millions d'utilisateurs Windows parcourent le Store
2. **Confiance** : Les utilisateurs font confiance aux applications du Store
3. **Mises à jour automatiques** : Gérées par Windows
4. **Paiement intégré** : Microsoft gère les transactions
5. **Pas de signature à gérer** : Microsoft signe votre application

**Avantages pour les utilisateurs** :

1. **Installation simple** : Un clic, pas d'assistant complexe
2. **Sécurité** : Applications validées par Microsoft
3. **Mises à jour automatiques** : Toujours la dernière version
4. **Désinstallation propre** : Suppression complète garantie
5. **Sandboxing** : Applications isolées du système

### Types d'applications acceptées

Le Microsoft Store accepte :

- **Applications UWP** (Universal Windows Platform)
- **Applications Win32 packagées** (applications desktop classiques dont Delphi)
- **Applications Web progressives** (PWA)
- **Jeux** (y compris Xbox)

Pour Delphi, vous distribuerez une **application Win32 packagée**.

### Prérequis pour publier sur le Microsoft Store

#### 1. Compte développeur Microsoft

**Inscription** :
- Site : https://developer.microsoft.com/store/register
- **Coût** : 19$ par an (individuel) ou 99$ par an (entreprise)
- **Processus** : Vérification d'identité (1-3 jours)

#### 2. Application conforme

Votre application doit respecter les **politiques du Store** :

**✓ Autorisé** :
- Applications utiles et fonctionnelles
- Jeux
- Applications professionnelles
- Applications gratuites ou payantes

**✗ Interdit** :
- Applications incomplètes ou démo uniquement
- Contenu offensant, illégal ou dangereux
- Applications qui violent la vie privée
- Malwares, virus, etc.
- Applications qui ne fonctionnent pas

#### 3. Package MSIX

Le Microsoft Store nécessite le format **MSIX** (évolution de l'ancien APPX).

### Qu'est-ce que MSIX ?

**MSIX** est le format de package moderne de Microsoft pour Windows 10/11. C'est l'équivalent des fichiers `.app` sur macOS ou `.apk` sur Android.

**Avantages de MSIX** :

1. **Installation propre** : Tous les fichiers dans un conteneur isolé
2. **Désinstallation complète** : Aucun fichier résiduel
3. **Mises à jour différentielles** : Téléchargement uniquement des changements
4. **Sandboxing** : Isolation de l'application
5. **Portable** : Peut être distribué hors Store

**Structure d'un MSIX** :

```
MonApp.msix
├── AppxManifest.xml        (métadonnées)
├── Assets/                 (icônes, images)
│   ├── Square150x150Logo.png
│   ├── Square44x44Logo.png
│   └── ...
├── MonApp.exe              (votre application)
├── *.dll                   (dépendances)
└── AppxSignature.p7x       (signature)
```

### Créer un package MSIX pour votre application Delphi

#### Méthode 1 : MSIX Packaging Tool (Recommandé pour débutants)

**MSIX Packaging Tool** est un outil gratuit de Microsoft qui convertit votre installateur en MSIX.

**Installation** :
1. Ouvrez le **Microsoft Store**
2. Recherchez "MSIX Packaging Tool"
3. Installez (gratuit)

**Utilisation** :

1. **Lancer MSIX Packaging Tool**

2. **Application package** → **Create package on this computer**

3. **Choisir un environnement** :
   - Recommandé : Machine virtuelle propre
   - Possible : Machine actuelle (moins fiable)

4. **Préparer l'ordinateur** :
   - Fermer les applications en arrière-plan
   - Désactiver l'antivirus temporairement
   - L'outil vérifie que l'environnement est prêt

5. **Sélectionner l'installateur** :
   - Parcourir vers votre `setup.exe` ou `.msi`
   - **Signature** : Choisir votre certificat

6. **Informations sur le package** :
   - **Nom du package** : Identifiant unique (ex: VotreEntreprise.MonApp)
   - **Nom d'affichage** : Nom visible pour l'utilisateur
   - **Nom de l'éditeur** : Doit correspondre à votre certificat
   - **Version** : 1.0.0.0 (format quatre nombres)

7. **Installation** :
   - L'outil lance votre installateur
   - **Suivez l'installation normalement**
   - Cliquez sur "Next" quand terminé

8. **Première exécution** :
   - Lancez votre application
   - Testez les fonctionnalités principales
   - L'outil enregistre les modifications

9. **Créer le package** :
   - Vérifiez les fichiers détectés
   - Ajoutez des fichiers manquants si nécessaire
   - Cliquez sur "Create"

10. **Package créé** :
    - Le fichier `.msix` est généré
    - Testez-le en l'installant

#### Méthode 2 : Créer manuellement avec makeappx

Pour plus de contrôle, créez le package manuellement.

**Étape 1 : Créer le manifeste AppxManifest.xml**

```xml
<?xml version="1.0" encoding="utf-8"?>
<Package xmlns="http://schemas.microsoft.com/appx/manifest/foundation/windows10"
         xmlns:uap="http://schemas.microsoft.com/appx/manifest/uap/windows10"
         xmlns:rescap="http://schemas.microsoft.com/appx/manifest/foundation/windows10/restrictedcapabilities">

  <Identity Name="VotreEntreprise.MonApp"
            Version="1.0.0.0"
            Publisher="CN=Votre Nom"
            ProcessorArchitecture="x64" />

  <Properties>
    <DisplayName>Mon Application Delphi</DisplayName>
    <PublisherDisplayName>Votre Entreprise</PublisherDisplayName>
    <Description>Une application géniale développée avec Delphi</Description>
    <Logo>Assets\StoreLogo.png</Logo>
  </Properties>

  <Dependencies>
    <TargetDeviceFamily Name="Windows.Desktop"
                        MinVersion="10.0.17763.0"
                        MaxVersionTested="10.0.22621.0" />
  </Dependencies>

  <Resources>
    <Resource Language="fr-FR" />
    <Resource Language="en-US" />
  </Resources>

  <Applications>
    <Application Id="MonApp"
                 Executable="MonApp.exe"
                 EntryPoint="Windows.FullTrustApplication">

      <uap:VisualElements DisplayName="Mon Application"
                          Description="Application Delphi"
                          BackgroundColor="#777777"
                          Square150x150Logo="Assets\Square150x150Logo.png"
                          Square44x44Logo="Assets\Square44x44Logo.png">
      </uap:VisualElements>

    </Application>
  </Applications>

  <Capabilities>
    <rescap:Capability Name="runFullTrust" />
  </Capabilities>

</Package>
```

**Points importants** :

- **Identity → Name** : Identifiant unique, recommandé : `Entreprise.NomApp`
- **Identity → Publisher** : Doit correspondre au sujet de votre certificat
- **Identity → Version** : Format obligatoire : X.Y.Z.W
- **runFullTrust** : Capacité requise pour les applications Win32

**Étape 2 : Préparer les assets (icônes)**

Le Store nécessite plusieurs tailles d'icônes :

```
Assets/
├── Square44x44Logo.png      (44×44 px)
├── Square150x150Logo.png    (150×150 px)
├── StoreLogo.png            (50×50 px)
├── Wide310x150Logo.png      (310×150 px - optionnel)
└── Square310x310Logo.png    (310×310 px - optionnel)
```

**Étape 3 : Organiser les fichiers**

```
MonAppPackage/
├── AppxManifest.xml
├── Assets/
│   ├── Square44x44Logo.png
│   └── Square150x150Logo.png
├── MonApp.exe
└── (autres DLL et fichiers)
```

**Étape 4 : Créer le package**

```cmd
REM Créer le package MSIX  
makeappx pack /d MonAppPackage /p MonApp.msix  

REM Signer le package  
signtool sign /fd SHA256 /a /f CertificatCodeSigning.pfx /p MotDePasse MonApp.msix  
```

**Étape 5 : Tester le package**

```powershell
# Installer le package localement
Add-AppxPackage -Path MonApp.msix

# Lancer l'application depuis le menu Démarrer

# Désinstaller
Get-AppxPackage *MonApp* | Remove-AppxPackage
```

### Soumettre au Microsoft Store

#### Étape 1 : Préparer la soumission

**Informations nécessaires** :

1. **Nom de l'application** : Unique sur le Store
2. **Description** :
   - Courte (moins de 200 caractères)
   - Longue (jusqu'à 10 000 caractères)
3. **Captures d'écran** :
   - Au moins 1, recommandé 3-5
   - Tailles : 1366×768 ou 1920×1080
4. **Icône du Store** : 300×300 px minimum
5. **Classification** :
   - Âge (3+, 7+, 12+, 16+, 18+)
   - Catégorie (Productivité, Utilitaires, Jeux, etc.)
6. **Prix** : Gratuit ou payant
7. **Marchés** : Pays où distribuer
8. **Politique de confidentialité** : URL obligatoire

#### Étape 2 : Partner Center

1. **Accéder au Partner Center** :
   - https://partner.microsoft.com/dashboard

2. **Créer une nouvelle application** :
   - Apps and games → New product → MSIX or PWA app
   - Réserver le nom de l'application

3. **Remplir les sections** :

   **Product identity** :
   - Informations générées automatiquement
   - Notez le `Package/Identity/Name` pour votre manifeste

   **Pricing and availability** :
   - Gratuit ou prix fixe
   - Marchés (pays)
   - Date de publication

   **Properties** :
   - Catégorie et sous-catégorie
   - Déclarations de contenu
   - Fonctionnalités système requises

   **Age ratings** :
   - Questionnaire sur le contenu
   - Génère automatiquement les classifications d'âge

   **Packages** :
   - **Upload** votre fichier `.msix` ou `.msixbundle`
   - Le système valide le package

   **Store listings** :
   - Description de l'application
   - Captures d'écran
   - Bande-annonce (optionnel)
   - Notes de version

   **Submission options** :
   - Options de publication
   - Notes pour les testeurs

4. **Soumettre pour certification** :
   - Vérifiez que tout est complet
   - Cliquez sur "Submit for certification"

#### Étape 3 : Processus de certification

**Durée** : 1-3 jours en moyenne

**Étapes** :

1. **Validation du package** : Vérifications automatiques (quelques minutes)
2. **Conformité des métadonnées** : Description, images, catégorie
3. **Tests techniques** : L'application s'installe et fonctionne
4. **Examen manuel** : Un testeur humain vérifie la conformité
5. **Tests de sécurité** : Scan antimalware, etc.

**Résultats possibles** :

- ✅ **Certifié** : Application publiée automatiquement
- ⚠️ **Corrections nécessaires** : Liste de problèmes à corriger
- ❌ **Rejeté** : Non conforme aux politiques

#### Étape 4 : Publication

Une fois certifiée :
- L'application est **publiée sur le Store**
- Apparaît dans les recherches
- Disponible au téléchargement
- Vous recevez une notification

**Délai de propagation** : 1-24 heures pour être visible partout

### Mises à jour sur le Microsoft Store

Pour publier une mise à jour :

1. **Préparer la nouvelle version** :
   - Augmentez le numéro de version dans le manifeste
   - Recompilez le package MSIX

2. **Créer une nouvelle soumission** :
   - Partner Center → Votre app → Update
   - Uploadez le nouveau package

3. **Notes de version** :
   - Décrivez les changements ("Quoi de neuf")

4. **Soumettre** :
   - Même processus de certification
   - Plus rapide si déjà certifié une fois

Les utilisateurs reçoivent automatiquement la mise à jour via Windows Update.

### Mises à jour graduelles

Le Store permet des **déploiements progressifs** :

```
Jour 1 : 10% des utilisateurs  
Jour 2 : 25% des utilisateurs  
Jour 3 : 50% des utilisateurs  
Jour 4 : 100% des utilisateurs  
```

**Avantage** : Détecter les bugs sur un petit groupe avant diffusion massive.

## Comparaison MSI vs Microsoft Store

| Critère | MSI | Microsoft Store |
|---------|-----|-----------------|
| **Coût** | Gratuit | 19-99$/an |
| **Distribution** | Manuelle | Automatique via Store |
| **Visibilité** | Dépend de vous | Store = millions d'utilisateurs |
| **Installation** | Assistant classique | Un clic |
| **Mises à jour** | Manuel (vous gérez) | Automatique (Windows Update) |
| **Entreprises** | Excellent (GPO, SCCM) | Moyen (possible mais moins) |
| **Paiements** | Vous gérez | Microsoft gère (commission 15-30%) |
| **Contrôle** | Total | Limité (soumis aux règles Store) |
| **Sandboxing** | Non | Oui (isolation) |
| **Certification** | Non requise | Obligatoire (1-3 jours) |
| **Format** | .msi | .msix |
| **Outils** | WiX, InstallShield, Advanced Installer | MSIX Packaging Tool |

### Quand utiliser MSI ?

✅ **Utilisez MSI si** :
- Vous ciblez principalement les entreprises
- Vous voulez un contrôle total sur la distribution
- Vous avez besoin de déploiement via Group Policy
- Vous ne voulez pas payer d'abonnement annuel
- Votre application nécessite des configurations complexes

### Quand utiliser le Microsoft Store ?

✅ **Utilisez le Store si** :
- Vous ciblez le grand public
- Vous voulez une distribution simplifiée
- Vous souhaitez des mises à jour automatiques gérées par Windows
- Vous voulez bénéficier de la visibilité du Store
- Vous voulez que Microsoft gère les paiements
- Vous vendez votre application (gestion de licence intégrée)

### Peut-on faire les deux ?

**Oui !** Vous pouvez :
- Distribuer un **MSI pour les entreprises** (site web, revendeurs)
- Publier sur le **Store pour le grand public**

C'est ce que font de nombreux éditeurs de logiciels.

## Applications Delphi et sandboxing

### Le défi du sandboxing

Le Microsoft Store impose le **sandboxing** (isolation) pour les applications MSIX. Cela signifie que l'application tourne dans un conteneur restreint.

**Limitations du sandbox** :

- ❌ Accès restreint au système de fichiers
- ❌ Pas d'accès direct au registre système
- ❌ Certaines API Windows restreintes
- ❌ Pas de pilotes en mode noyau

**Solutions** :

1. **runFullTrust** : Capacité qui donne plus de permissions
   ```xml
   <Capabilities>
     <rescap:Capability Name="runFullTrust" />
   </Capabilities>
   ```
   Votre application Delphi Win32 nécessite cette capacité.

2. **broadFileSystemAccess** : Accès complet aux fichiers
   ```xml
   <Capabilities>
     <rescap:Capability Name="broadFileSystemAccess" />
   </Capabilities>
   ```
   L'utilisateur doit autoriser cette permission.

### Adapter votre application Delphi

Pour le Store, votre application doit :

1. **Utiliser les chemins appropriés** :
   ```pascal
   // ✗ Mauvais : Écrire dans Program Files
   ConfigFile := ExtractFilePath(Application.ExeName) + 'config.ini';

   // ✓ Bon : Utiliser le dossier de données d'application
   ConfigFile := TPath.Combine(
     TPath.GetHomePath,
     'AppData\Local\MonApp\config.ini'
   );
   ```

2. **Demander les permissions nécessaires** dans le manifeste

3. **Gérer gracieusement les refus de permissions**

4. **Respecter la vie privée** : Politique de confidentialité claire

## Monétisation sur le Microsoft Store

### Options de tarification

#### 1. Application gratuite

- Pas de frais pour l'utilisateur
- Pas de commission Microsoft
- Revenus possibles via :
  - Publicité (dans l'app)
  - Achats in-app
  - Version premium séparée

#### 2. Application payante

- Prix fixe (minimum 0,99$, max 999,99$)
- Microsoft prend **15-30% de commission** :
  - 15% si < 25 000$ de revenus
  - 30% au-delà
- Paiement mensuel des revenus
- Support multi-devises automatique

#### 3. Abonnement

- Paiement récurrent (mensuel, annuel)
- Commission identique (15-30%)
- Renouvellement automatique
- Gestion des annulations par Microsoft

#### 4. Essai gratuit

- Période d'essai avant paiement
- Durées : 7, 15, 30 jours ou 1 mois
- Conversion automatique en version payante

#### 5. Achats in-app

- Fonctionnalités ou contenu payant dans l'app
- Consommables ou permanents
- API Microsoft pour gérer les achats

### Statistiques et analyses

Le Partner Center fournit :
- **Téléchargements** : Nombre et tendances
- **Notes et avis** : Feedback utilisateurs
- **Revenus** : Si application payante
- **Démographie** : Âge, pays, appareil
- **Crashs et erreurs** : Rapports automatiques

## Checklist de publication

### Pour MSI

- [ ] Application compilée en Release
- [ ] Toutes les dépendances identifiées et incluses
- [ ] WiX ou outil MSI installé
- [ ] Fichier .wxs créé et configuré
- [ ] ProductCode et UpgradeCode définis
- [ ] Raccourcis configurés (menu Démarrer, bureau)
- [ ] Interface utilisateur choisie (UIRef)
- [ ] MSI compilé et testé
- [ ] Installation testée sur machine propre
- [ ] Installation silencieuse testée
- [ ] Désinstallation propre vérifiée
- [ ] Version et métadonnées correctes
- [ ] MSI signé numériquement (recommandé)

### Pour Microsoft Store

- [ ] Compte développeur Microsoft créé et validé
- [ ] Application conforme aux politiques du Store
- [ ] Package MSIX créé
- [ ] Manifeste AppxManifest.xml configuré
- [ ] Icônes de toutes tailles créées
- [ ] Captures d'écran préparées (1366×768 min)
- [ ] Description courte et longue rédigées
- [ ] Politique de confidentialité en ligne
- [ ] Classification d'âge déterminée
- [ ] Prix et marchés définis
- [ ] Package testé localement
- [ ] Package signé
- [ ] Soumission créée dans Partner Center
- [ ] Tous les champs requis remplis
- [ ] Package uploadé
- [ ] Soumission envoyée pour certification

## Problèmes courants et solutions

### MSI : "Erreur 2502" ou "Erreur 2503" lors de l'installation

**Cause** : Permissions insuffisantes dans le dossier Temp

**Solution** :
```cmd
REM Lancer l'installation en admin  
msiexec /i MonApp.msi /a  
```

### MSI : Installation échoue silencieusement

**Cause** : Erreur pendant l'installation

**Solution** : Créer un log détaillé
```cmd
msiexec /i MonApp.msi /l*v install.log  
REM Examiner install.log pour trouver l'erreur  
```

### Store : "Package validation failed"

**Causes courantes** :
- Manifeste incorrect (version, publisher)
- Icônes manquantes ou mauvaise taille
- Capacités non déclarées

**Solution** : Utiliser l'outil de validation
```cmd
makeappx.exe validate /p MonApp.msix
```

### Store : "Publisher name doesn't match certificate"

**Cause** : Le Publisher dans le manifeste ne correspond pas au certificat

**Solution** :
```cmd
REM Voir le sujet du certificat  
certutil -dump CertificatCodeSigning.pfx  

REM Copier exactement le CN= dans le manifeste
```

### Store : Application rejetée pour "Functionality"

**Cause** : L'application ne fonctionne pas correctement ou crash

**Solution** :
- Testez abondamment avant soumission
- Vérifiez les logs de crash
- Corrigez les bugs identifiés

### Store : Mises à jour pas visible sur tous les appareils

**Cause** : Propagation graduelle

**Solution** : Patience, peut prendre jusqu'à 24h. Vérifiez aussi les paramètres de déploiement progressif.

## Ressources et outils

### Documentation officielle

- **WiX Toolset** : https://wixtoolset.org/documentation/
- **MSI Reference** : https://docs.microsoft.com/windows/win32/msi/
- **Partner Center** : https://docs.microsoft.com/windows/uwp/publish/
- **MSIX** : https://docs.microsoft.com/windows/msix/

### Outils recommandés

- **WiX Toolset** : Créer des MSI (gratuit)
- **Advanced Installer** : Alternative visuelle à WiX
- **MSIX Packaging Tool** : Convertir en MSIX (gratuit)
- **Orca** : Éditeur de tables MSI (gratuit, dans Windows SDK)

### Communautés

- **WiX Users Google Group** : Support WiX
- **Stack Overflow** : Tags [wix], [windows-installer], [msix]

## Conclusion

MSI et le Microsoft Store représentent deux approches complémentaires pour distribuer vos applications Delphi sur Windows :

**MSI** est idéal pour :
- Les déploiements en entreprise
- Le contrôle total de la distribution
- Les installations complexes nécessitant des configurations

**Microsoft Store** est idéal pour :
- Toucher le grand public
- Simplifier la distribution et les mises à jour
- Bénéficier de la visibilité du Store
- Monétiser facilement votre application

**Points clés à retenir** :

1. **MSI** : Standard professionnel, WiX gratuit, déploiement entreprise
2. **MSIX** : Format moderne, requis pour le Store, sandboxing
3. **Store** : Visibilité, mises à jour auto, certification obligatoire (1-3 jours)
4. **Coûts** : MSI gratuit, Store 19-99$/an + commission 15-30%
5. **Les deux** : Vous pouvez distribuer via MSI ET Store simultanément

Avec ces deux options, vous pouvez choisir la meilleure stratégie de distribution pour votre application Delphi en fonction de votre public cible et de vos objectifs commerciaux. Dans la prochaine section, nous explorerons le déploiement continu (CI/CD) pour automatiser vos processus de build et de distribution.

⏭️ [Déploiement continu (CI/CD)](/17-distribution-et-deploiement/09-deploiement-continu.md)
