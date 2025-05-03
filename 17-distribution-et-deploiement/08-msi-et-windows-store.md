# 17.8 MSI et Windows Store

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Vous avez développé et optimisé votre application Delphi, et maintenant vous cherchez le meilleur moyen de la distribuer aux utilisateurs de Windows ? Ce chapitre vous guidera à travers deux méthodes de distribution professionnelles : les packages d'installation MSI et le Microsoft Store (Windows Store). Ces méthodes offrent à vos utilisateurs une expérience d'installation fluide et sécurisée, et peuvent aider votre application à atteindre un public plus large.

## Partie 1 : Création de packages MSI

### Qu'est-ce qu'un package MSI ?

MSI (Microsoft Installer) est un format de package d'installation standard pour Windows. Comparé aux installateurs exécutables traditionnels (.exe), les packages MSI offrent plusieurs avantages :

- **Installation et désinstallation silencieuses** (sans interface)
- **Installation administrative** (déploiement sur plusieurs machines)
- **Réparation automatique** (auto-correction des problèmes)
- **Déploiement via Group Policy** dans les environnements d'entreprise
- **Meilleure compatibilité** avec les systèmes de gestion des applications d'entreprise

![Exemple d'installation MSI](https://placeholder-image.com/msi-installation.png)

### Outils pour créer des packages MSI

Plusieurs outils permettent de créer des packages MSI pour vos applications Delphi :

1. **WiX Toolset** (gratuit, open-source)
2. **Advanced Installer** (commercial, avec version gratuite limitée)
3. **InstallShield** (commercial)
4. **NSIS** avec extension MSI (gratuit)

Dans ce tutoriel, nous verrons deux approches : WiX Toolset (gratuit) et Advanced Installer (version communautaire).

### Création d'un package MSI avec WiX Toolset

[WiX Toolset](https://wixtoolset.org/) (Windows Installer XML) est un ensemble d'outils gratuits qui permet de créer des packages MSI à partir de fichiers XML.

#### Étape 1 : Installation de WiX Toolset

1. Téléchargez la dernière version stable de WiX Toolset sur [wixtoolset.org](https://wixtoolset.org/releases/)
2. Exécutez l'installateur et suivez les instructions
3. Installez également l'extension Visual Studio si vous utilisez Visual Studio

#### Étape 2 : Création du fichier source WiX

Créez un fichier XML nommé `MonApplication.wxs` avec le contenu de base suivant :

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">
  <!-- Définition du produit -->
  <Product Id="*"
           Name="Mon Application Delphi"
           Language="1036"
           Version="1.0.0.0"
           Manufacturer="Votre Entreprise"
           UpgradeCode="PUT-GUID-HERE">

    <!-- Package MSI -->
    <Package InstallerVersion="200"
             Compressed="yes"
             InstallScope="perMachine"
             Description="Installateur de Mon Application Delphi" />

    <!-- Média -->
    <Media Id="1" Cabinet="MonApplication.cab" EmbedCab="yes" />

    <!-- Condition d'installation : Windows 7 ou plus récent -->
    <Condition Message="Cette application nécessite Windows 7 ou plus récent.">
      <![CDATA[Installed OR (VersionNT >= 601)]]>
    </Condition>

    <!-- Fonctionnalités -->
    <Feature Id="ProductFeature" Title="Mon Application" Level="1">
      <ComponentGroupRef Id="ProductComponents" />
      <ComponentGroupRef Id="ProductShortcuts" />
    </Feature>

    <!-- Interface utilisateur -->
    <UIRef Id="WixUI_InstallDir" />
    <Property Id="WIXUI_INSTALLDIR" Value="INSTALLFOLDER" />

    <!-- Licence (remplacez par votre fichier de licence) -->
    <WixVariable Id="WixUILicenseRtf" Value="License.rtf" />

  </Product>

  <!-- Structure des dossiers -->
  <Fragment>
    <Directory Id="TARGETDIR" Name="SourceDir">
      <Directory Id="ProgramFilesFolder">
        <Directory Id="INSTALLFOLDER" Name="Mon Application Delphi" />
      </Directory>
      <Directory Id="ProgramMenuFolder">
        <Directory Id="ApplicationProgramsFolder" Name="Mon Application Delphi" />
      </Directory>
      <Directory Id="DesktopFolder" Name="Desktop" />
    </Directory>
  </Fragment>

  <!-- Composants de l'application -->
  <Fragment>
    <ComponentGroup Id="ProductComponents" Directory="INSTALLFOLDER">
      <!-- Fichier principal de l'application -->
      <Component Id="MainExecutable" Guid="*">
        <File Id="AppEXE" Source="$(var.SourceDir)\MonApplication.exe" KeyPath="yes">
          <Shortcut Id="ExeShortcut" Directory="ApplicationProgramsFolder"
                    Name="Mon Application" WorkingDirectory="INSTALLFOLDER"
                    Icon="AppIcon.ico" IconIndex="0" Advertise="yes" />
        </File>
      </Component>

      <!-- Autres fichiers (ajoutez tous vos fichiers ici) -->
      <Component Id="Documentation" Guid="*">
        <File Id="DocFile" Source="$(var.SourceDir)\Manual.pdf" KeyPath="yes" />
      </Component>
    </ComponentGroup>

    <!-- Raccourcis -->
    <ComponentGroup Id="ProductShortcuts" Directory="ApplicationProgramsFolder">
      <Component Id="ApplicationShortcuts" Guid="*">
        <Shortcut Id="UninstallShortcut" Name="Désinstaller Mon Application"
                  Target="[SystemFolder]msiexec.exe" Arguments="/x [ProductCode]" />
        <RemoveFolder Id="CleanupShortcuts" On="uninstall" />
        <RegistryValue Root="HKCU" Key="Software\VotreEntreprise\MonApplication"
                       Name="installed" Type="integer" Value="1" KeyPath="yes" />
      </Component>
    </ComponentGroup>
  </Fragment>

  <!-- Icônes -->
  <Fragment>
    <Icon Id="AppIcon.ico" SourceFile="$(var.SourceDir)\MonApplication.ico" />
  </Fragment>
</Wix>
```

#### Étape 3 : Préparation des fichiers

1. Créez un fichier de licence au format RTF nommé `License.rtf`
2. Organisez vos fichiers d'application dans un dossier source
3. Assurez-vous d'avoir une icône pour votre application (`MonApplication.ico`)

#### Étape 4 : Compilation du package MSI

1. Ouvrez une invite de commande
2. Naviguez vers le dossier où se trouve votre fichier .wxs
3. Exécutez les commandes suivantes pour compiler le MSI :

```batch
set SourceDir=C:\Chemin\Vers\VosFichiers
candle MonApplication.wxs -dSourceDir=%SourceDir%
light -ext WixUIExtension MonApplication.wixobj -out MonApplication.msi
```

Le fichier MSI sera créé dans le même dossier. Vous pouvez maintenant le distribuer à vos utilisateurs.

### Création d'un package MSI avec Advanced Installer

[Advanced Installer](https://www.advancedinstaller.com/) offre une interface graphique plus intuitive pour créer des packages MSI.

#### Étape 1 : Installation d'Advanced Installer

1. Téléchargez et installez Advanced Installer Community Edition (version gratuite)
2. Lancez Advanced Installer

#### Étape 2 : Création d'un nouveau projet

1. Choisissez "Nouveau projet" → "Simple" (ou "Professional" si vous avez une licence)
2. Remplissez les informations de base (nom du produit, version, etc.)

![Advanced Installer Nouveau Projet](https://placeholder-image.com/advanced-installer-new.png)

#### Étape 3 : Configuration du projet

1. Dans la section "Information Produit", remplissez tous les détails de votre application
2. Dans "Fichiers et Dossiers", ajoutez votre exécutable Delphi et tous les fichiers associés
3. Dans "Raccourcis", configurez les raccourcis pour le menu Démarrer et le Bureau
4. Dans "Registre", ajoutez les clés de registre nécessaires (si votre application en utilise)
5. Dans "Prérequis", ajoutez les dépendances nécessaires (comme les packages redistributables Visual C++)

#### Étape 4 : Personnalisation de l'interface utilisateur

1. Dans la section "Thèmes", choisissez un thème pour votre installateur
2. Ajoutez votre logo et personnalisez les couleurs

#### Étape 5 : Construction du package MSI

1. Cliquez sur "Construire" dans la barre d'outils
2. Choisissez l'emplacement de sortie pour votre fichier MSI
3. Attendez la fin de la construction

Vous obtiendrez un fichier MSI professionnel avec toutes les fonctionnalités configurées.

### Bonnes pratiques pour les packages MSI

Pour créer un package MSI professionnel et fiable, suivez ces bonnes pratiques :

1. **Utilisez un GUID unique** pour UpgradeCode (générez-en un avec des outils en ligne)
2. **Incluez toutes les dépendances** ou vérifiez qu'elles sont installées
3. **Testez l'installation et la désinstallation** sur différentes versions de Windows
4. **Signez numériquement votre MSI** pour la sécurité (voir le chapitre [17.4 Signature de code](lien-vers-section-17.4))
5. **Créez des journaux d'installation** pour faciliter le débogage (`msiexec /i MonApplication.msi /l*v install.log`)

## Partie 2 : Publication sur le Windows Store

Le Microsoft Store (anciennement Windows Store) est la boutique d'applications officielle de Microsoft, intégrée dans Windows 10 et 11. Publier votre application Delphi sur le Store offre plusieurs avantages :

- **Visibilité accrue** auprès des utilisateurs de Windows
- **Installation et mises à jour simplifiées**
- **Confiance** des utilisateurs envers les applications vérifiées
- **Monétisation** via achats in-app ou abonnements

### Prérequis pour publier sur le Windows Store

Avant de commencer, vous aurez besoin de :

1. **Un compte développeur Microsoft Store** (frais d'inscription uniques de 19$ pour les particuliers, 99$ pour les entreprises)
2. **Une application Delphi compatible** avec Windows 10/11
3. **Visual Studio** (la version Community gratuite suffit)
4. **Desktop App Converter** ou **MSIX Packaging Tool** pour convertir votre application

### Méthodes pour publier une application Delphi sur le Store

Il existe trois principales méthodes pour publier une application Delphi sur le Windows Store :

1. **Package MSIX** (recommandé pour les nouvelles applications)
2. **Desktop Bridge** (pour convertir des applications existantes)
3. **Conteneurisation** (pour les applications plus complexes)

### Méthode 1 : Création d'un package MSIX

MSIX est le format de package d'application moderne de Microsoft, qui remplace progressivement les anciens formats (MSI, AppX).

#### Étape 1 : Installation de l'outil MSIX Packaging

1. Recherchez "MSIX Packaging Tool" dans le Microsoft Store et installez-le
2. Ou téléchargez-le depuis le [site Microsoft](https://docs.microsoft.com/fr-fr/windows/msix/packaging-tool/tool-overview)

#### Étape 2 : Préparation de votre application

1. Assurez-vous que votre application Delphi fonctionne correctement sur Windows 10/11
2. Préparez tous les fichiers nécessaires (exécutable, DLL, ressources, etc.)
3. Créez des icônes haute résolution (44x44, 150x150, 300x300, 620x300, etc.)

#### Étape 3 : Création du package MSIX

1. Lancez l'outil MSIX Packaging Tool
2. Choisissez "Créer un package d'application à partir d'une application installée (.msi ou setup.exe)"
3. Suivez l'assistant :
   - Sélectionnez votre installateur (.exe ou .msi)
   - Spécifiez les informations du package (nom, version, éditeur)
   - Observez l'installation et validez les modifications

![MSIX Packaging Tool](https://placeholder-image.com/msix-tool.png)

4. Finalisez le package :
   - Ajoutez les visuels (icônes, images promotionnelles)
   - Configurez les capacités requises par votre application
   - Générez le package MSIX

#### Étape 4 : Test du package MSIX

1. Installez le package sur votre machine en double-cliquant dessus
2. Testez toutes les fonctionnalités de votre application
3. Vérifiez l'installateur et le désinstallateur

### Méthode 2 : Utilisation du Desktop Bridge

Le Desktop Bridge (également appelé Centennial) est une technologie qui permet de convertir des applications desktop traditionnelles en applications Windows Store.

#### Étape 1 : Installation des outils

1. Installez Visual Studio (avec la charge de travail "Développement pour la plateforme Windows universelle")
2. Installez le Windows SDK pour Windows 10

#### Étape 2 : Création d'un projet d'empaquetage

1. Dans Visual Studio, créez un nouveau projet "Package d'application Windows (Desktop Bridge)"
2. Configurez le projet :
   - Choisissez le mode d'installation "Installer sur ce système"
   - Spécifiez le chemin vers votre application Delphi compilée

![Projet Desktop Bridge](https://placeholder-image.com/desktop-bridge.png)

3. Modifiez le fichier `Package.appxmanifest` pour configurer votre application :
   - Identité (nom, éditeur, version)
   - Propriétés visuelles (nom affiché, description)
   - Capacités requises (accès fichiers, réseau, etc.)

#### Étape 3 : Génération du package

1. Générez la solution dans Visual Studio (mode Release)
2. Le package AppX/MSIX sera créé dans le dossier de sortie

### Publication de votre application sur le Store

Une fois que vous avez créé votre package MSIX, vous pouvez le publier sur le Microsoft Store.

#### Étape 1 : Création d'une réservation de nom

1. Connectez-vous au [Tableau de bord des partenaires](https://partner.microsoft.com/dashboard)
2. Naviguez vers "Windows et Xbox" → "Vue d'ensemble" → "Créer une application"
3. Réservez un nom unique pour votre application

#### Étape 2 : Préparation de la soumission

1. Dans la page de votre application, cliquez sur "Démarrer votre soumission"
2. Remplissez toutes les sections obligatoires :
   - **Prix et disponibilité** : marchés, prix, date de publication
   - **Propriétés** : catégorie, tranche d'âge, mots-clés
   - **Destinataires** : restrictions d'âge et de pays
   - **Paquets** : téléversez votre fichier MSIX/AppX

![Soumission au Store](https://placeholder-image.com/store-submission.png)

#### Étape 3 : Ajout des assets marketing

1. Téléversez les captures d'écran (au moins une, idéalement 4-5)
2. Ajoutez une icône de magasin (300x300 pixels minimum)
3. Fournissez les images promotionnelles (si nécessaire)
4. Rédigez une description attrayante et détaillée
5. Ajoutez des notes de version

#### Étape 4 : Soumission et certification

1. Passez en revue toutes les informations
2. Soumettez votre application
3. Attendez le processus de certification (généralement 1-3 jours ouvrables)
4. Corrigez les problèmes si votre application est rejetée

### Considérations spécifiques pour les applications Delphi

Lorsque vous publiez une application Delphi sur le Windows Store, gardez à l'esprit ces considérations spécifiques :

1. **Isolation du stockage** : Les applications du Store sont conteneurisées, vous devez utiliser les dossiers appropriés pour stocker vos données :
   ```pascal
   function GetAppDataFolder: string;
   begin
     Result := IncludeTrailingPathDelimiter(
       GetEnvironmentVariable('LOCALAPPDATA')) +
       'Packages\' +
       GetEnvironmentVariable('PACKAGE_FAMILY_NAME') +
       '\LocalState';
   end;
   ```

2. **Permissions et capacités** : Déclarez toutes les fonctionnalités que votre application utilise (accès aux fichiers, caméra, etc.) dans le manifeste

3. **Adaptabilité DPI** : Assurez-vous que votre application est compatible avec différentes résolutions d'écran

4. **Modèle de version** : Utilisez un système de versionnage cohérent (comme le versionnage sémantique)

### Mises à jour d'applications sur le Store

L'un des grands avantages du Store est la gestion automatique des mises à jour :

1. Créez une nouvelle version de votre application avec un numéro de version supérieur
2. Générez un nouveau package MSIX
3. Créez une nouvelle soumission dans le Tableau de bord des partenaires
4. Téléversez le nouveau package et décrivez les changements
5. Les utilisateurs recevront automatiquement la mise à jour

## Comparaison : MSI vs Windows Store

Pour vous aider à choisir la méthode de distribution la plus adaptée à votre application Delphi, voici une comparaison :

| Critère | MSI | Windows Store |
|---------|-----|---------------|
| Coût | Gratuit à créer | Frais d'inscription (19$ ou 99$) |
| Public cible | Tous utilisateurs Windows, entreprises | Principalement utilisateurs Windows 10/11 |
| Installation | Téléchargement manuel | Intégrée au système |
| Mises à jour | Gestion manuelle | Automatiques |
| Sécurité | Variable (selon signature) | Vérifiée par Microsoft |
| Restrictions | Minimales | Nombreuses (sandbox, capacités) |
| Possibilités de monétisation | Limitées | Intégrées (achats, abonnements) |
| Complexité de publication | Modérée | Élevée |
| Visibilité | À gérer soi-même | Aide à la découvrabilité |

## Distribution hybride

Vous n'avez pas à choisir exclusivement entre MSI et Windows Store. De nombreux développeurs adoptent une approche hybride :

1. **Package MSI** pour les environnements d'entreprise et les utilisateurs d'anciennes versions de Windows
2. **Windows Store** pour les utilisateurs de Windows 10/11 qui préfèrent cette méthode d'installation

Cette stratégie maximise votre audience tout en offrant la meilleure expérience à chaque segment d'utilisateurs.

## Exercice pratique : création d'un package MSI simple

Pour mettre en pratique ce que vous avez appris, suivez cet exercice :

1. Prenez une application Delphi simple que vous avez développée
2. Téléchargez et installez Advanced Installer Community Edition
3. Créez un projet MSI de base pour votre application
4. Ajoutez l'exécutable et les fichiers nécessaires
5. Configurez les raccourcis pour le menu Démarrer et le Bureau
6. Construisez le package MSI
7. Testez l'installation sur un autre ordinateur ou une machine virtuelle

## Conclusion

La distribution professionnelle de vos applications Delphi via des packages MSI ou le Windows Store améliore considérablement l'expérience utilisateur et la perception de votre logiciel. Les packages MSI offrent une solution robuste pour tous les environnements Windows, tandis que le Store apporte visibilité et simplicité pour les utilisateurs modernes.

Chaque méthode a ses avantages et ses défis, mais avec les connaissances acquises dans ce chapitre, vous êtes maintenant en mesure de choisir la solution qui convient le mieux à votre application et à votre public cible.

Dans la prochaine section, nous explorerons l'intégration continue et le déploiement continu (CI/CD) pour automatiser le processus de compilation, de test et de déploiement de vos applications Delphi.

⏭️ [Déploiement continu (CI/CD)](17-distribution-et-deploiement/09-deploiement-continu.md)
