# 17.3 Création d'installateurs (Inno Setup, InstallAware)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Une application Delphi bien conçue et optimisée mérite un installateur professionnel pour faciliter son déploiement chez les utilisateurs finaux. Ce chapitre vous guide à travers la création d'installateurs pour vos applications Delphi, en vous présentant deux outils populaires : **Inno Setup** (gratuit) et **InstallAware** (commercial).

Un bon installateur permet de :
- Copier les fichiers de votre application au bon endroit
- Créer des raccourcis dans le menu Démarrer et sur le Bureau
- Enregistrer les bibliothèques et composants nécessaires
- Configurer les paramètres initiaux
- Offrir une désinstallation propre

## Inno Setup : L'outil gratuit et puissant

### Présentation d'Inno Setup

[Inno Setup](https://jrsoftware.org/isinfo.php) est un créateur d'installateurs gratuit, open-source et très populaire. Malgré sa gratuité, il offre des fonctionnalités avancées et est largement utilisé dans l'écosystème Delphi.

**Avantages :**
- Totalement gratuit et open-source
- Léger et rapide
- Facilement scriptable
- Génère des installateurs compacts
- Prise en charge multilingue
- Excellente compatibilité avec Delphi

### Installation d'Inno Setup

1. Téléchargez Inno Setup depuis le site officiel : [https://jrsoftware.org/isdl.php](https://jrsoftware.org/isdl.php)
2. Lancez l'installateur et suivez les instructions
3. Lors de l'installation, vous pouvez choisir d'inclure les exemples (recommandé pour les débutants)

### Création d'un installateur basique avec Inno Setup

Après avoir installé Inno Setup, suivez ces étapes pour créer votre premier installateur :

1. Lancez Inno Setup
2. Sélectionnez "Create a new script file using the Script Wizard" (Créer un nouveau fichier script avec l'assistant)

![Assistant Inno Setup](https://placeholder-image.com/inno-setup-wizard.png)

3. Dans l'assistant, renseignez les informations de base :
   - **Nom de l'application** : Le nom de votre application Delphi
   - **Version** : La version actuelle (ex: 1.0.0)
   - **Éditeur** : Votre nom ou celui de votre entreprise
   - **Site web** : L'URL de votre site (optionnel)

4. Sur l'écran suivant, indiquez le dossier d'installation par défaut :
   - Généralement `{pf}\VotreApplication` (qui sera remplacé par le chemin vers Program Files)

5. Sur l'écran "Application Files", ajoutez vos fichiers :
   - Cliquez sur "Add file(s)" et sélectionnez l'exécutable de votre application
   - Ajoutez également les DLL et autres ressources nécessaires

6. Configurez les raccourcis :
   - Cochez les options pour créer des raccourcis dans le menu Démarrer et/ou sur le Bureau

7. Pour la documentation :
   - Ajoutez un fichier Lisez-moi ou manuel si disponible

8. Configurez les options d'installation :
   - Choisissez les privilèges nécessaires (administrateur ou utilisateur standard)
   - Définissez les paramètres de langue

9. Terminez l'assistant et enregistrez le script généré avec l'extension `.iss`

10. Compilez le script en cliquant sur le bouton "Compile" dans la barre d'outils

Une fois compilé, votre installateur sera créé dans le dossier de sortie spécifié (généralement `Output`).

### Personnalisation du script Inno Setup

Le script généré par l'assistant peut être personnalisé pour ajouter des fonctionnalités avancées. Voici un exemple de script commenté :

```pascal
; Script généré par l'assistant Inno Setup.
; Pour plus d'informations, voir la documentation d'Inno Setup.

#define MyAppName "Mon Application Delphi"
#define MyAppVersion "1.0"
#define MyAppPublisher "Votre Nom"
#define MyAppURL "https://www.votresite.com"
#define MyAppExeName "MonApp.exe"

[Setup]
; Identifiant unique pour cette installation (utilisez un nouveau GUID pour chaque application)
AppId={{YOUR-GUID-HERE}
; Informations de base sur l'application
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
; Dossier d'installation par défaut
DefaultDirName={autopf}\{#MyAppName}
; Désactive la demande de dossier d'installation
DisableDirPage=no
; Nom du groupe dans le menu Démarrer
DefaultGroupName={#MyAppName}
; Désactive la demande de groupe dans le menu Démarrer
DisableProgramGroupPage=yes
; Fichier de licence à afficher (optionnel)
LicenseFile=C:\MesProjects\Licence.txt
; Fichier d'informations à afficher (optionnel)
InfoBeforeFile=C:\MesProjects\Avant.txt
; Fichier d'informations post-installation (optionnel)
InfoAfterFile=C:\MesProjects\Apres.txt
; Icône de l'installateur
SetupIconFile=C:\MesProjects\Icone.ico
; Compression (recommandé: LZMA2/max)
Compression=lzma2
SolidCompression=yes
; Version Windows minimale requise
MinVersion=6.0

[Languages]
; Langues disponibles dans l'installateur
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
; Tâches optionnelles que l'utilisateur peut choisir
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; OnlyBelowVersion: 6.1

[Files]
; Liste des fichiers à installer
Source: "C:\MesProjects\MonApp.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\MesProjects\Data\*"; DestDir: "{app}\Data"; Flags: ignoreversion recursesubdirs
; NOTE: Ne pas utiliser "Flags: ignoreversion" pour les DLLs système ou partagées

[Icons]
; Raccourcis à créer
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
; Actions à effectuer après l'installation
; Propose d'exécuter l'application à la fin de l'installation
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent
```

### Fonctionnalités avancées d'Inno Setup

Voici quelques fonctionnalités avancées que vous pouvez ajouter à votre script :

#### Vérification des prérequis

```pascal
[Code]
function IsDotNetInstalled(): Boolean;
begin
  // Vérifier si .NET est installé
  Result := RegKeyExists(HKLM, 'SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full');
end;

function InitializeSetup(): Boolean;
begin
  Result := True;

  if not IsDotNetInstalled then begin
    MsgBox('Cette application nécessite .NET Framework 4.0 ou supérieur. ' +
           'Veuillez l''installer avant de continuer.', mbError, MB_OK);
    Result := False;
  end;
end;
```

#### Exécution de commandes pendant l'installation

```pascal
[Run]
; Enregistre une DLL COM
Filename: "{sys}\regsvr32.exe"; Parameters: "/s ""{app}\MaLibrairie.dll"""; Flags: runhidden
```

#### Création d'un fichier de configuration

```pascal
[INI]
Filename: "{app}\config.ini"; Section: "Settings"; Key: "UserName"; String: "{code:GetUserName}"
```

## InstallAware : Solution commerciale avancée

### Présentation d'InstallAware

[InstallAware](https://www.installaware.com/) est une solution commerciale plus avancée, offrant une interface graphique plus riche et des fonctionnalités supplémentaires. Elle est particulièrement intégrée avec Delphi.

**Avantages :**
- Interface visuelle complète
- Intégration directe avec Delphi
- Compression avancée
- Prise en charge native des patches et mises à jour
- Nombreux assistants prédéfinis
- Virtualisation d'applications

**Inconvénients :**
- Solution payante
- Courbe d'apprentissage plus importante

### Installation d'InstallAware

1. Téléchargez une version d'essai ou achetez une licence sur le site officiel
2. Suivez les instructions d'installation
3. Pour l'intégration avec Delphi, InstallAware installe généralement un menu dans l'IDE

### Création d'un installateur avec InstallAware

1. Lancez InstallAware ou accédez-y depuis le menu Delphi
2. Choisissez "Nouveau projet" ou "Nouveau à partir d'un modèle"
3. Sélectionnez un modèle adapté à votre type d'application

![InstallAware Template](https://placeholder-image.com/installaware-template.png)

4. Configurez les informations de base :
   - Nom de l'application
   - Version
   - Éditeur
   - Etc.

5. Ajoutez vos fichiers d'application :
   - Vous pouvez directement ajouter la sortie de votre projet Delphi
   - InstallAware détectera automatiquement les dépendances

6. Configurez les raccourcis et associations de fichiers

7. Personnalisez l'interface utilisateur :
   - Logo
   - Couleurs
   - Thème

8. Ajoutez des prérequis si nécessaire :
   - .NET Framework
   - Visual C++ Redistributable
   - Autres dépendances

9. Configurez les options de mise à jour en ligne (si disponible dans votre édition)

10. Compilez votre installateur

### Intégration d'InstallAware avec Delphi

Si vous utilisez une version d'InstallAware intégrée à Delphi, vous pouvez configurer votre projet pour générer automatiquement un installateur après une compilation réussie :

1. Dans Delphi, allez dans **Project** → **Options du projet**
2. Naviguez jusqu'à l'onglet **InstallAware**
3. Activez la case à cocher "Générer un installateur après une compilation réussie"
4. Configurez les paramètres selon vos besoins

## Comparaison entre Inno Setup et InstallAware

| Fonctionnalité | Inno Setup | InstallAware |
|----------------|------------|--------------|
| Prix | Gratuit | Commercial |
| Facilité d'utilisation | Modérée (script) | Élevée (visuel) |
| Taille de l'installateur | Très compacte | Variable |
| Intégration Delphi | Manuelle | Native |
| Mises à jour | Limitées | Avancées |
| Patches | À coder manuellement | Intégrés |
| Complexité | Simple à moyenne | Simple à avancée |
| Temps d'apprentissage | Court | Moyen |

## Bonnes pratiques pour les installateurs

Quel que soit l'outil choisi, suivez ces conseils pour créer des installateurs professionnels :

1. **Testez sur plusieurs versions de Windows** :
   - Windows 10, Windows 11
   - Versions 32 et 64 bits si pertinent

2. **Vérifiez les droits d'administration** :
   - Déterminez si votre application nécessite des droits d'administrateur
   - Utilisez le niveau de privilège minimum nécessaire

3. **Incluez tous les composants requis** :
   - DLLs
   - Fichiers de données
   - Configurations par défaut

4. **Proposez une désinstallation propre** :
   - Supprimez tous les fichiers créés par l'installation
   - Optionnellement, offrez de conserver les données utilisateur

5. **Personnalisez l'apparence** :
   - Ajoutez votre logo
   - Utilisez des couleurs cohérentes avec votre marque

6. **Fournissez des options d'installation** :
   - Installation minimale ou complète
   - Choix des composants à installer

## Exemple pratique : Création d'un installateur pour une application de gestion

Voici un exemple concret d'utilisation d'Inno Setup pour une application de gestion Delphi :

```pascal
; Script pour une application de gestion d'inventaire
#define AppName "GestionStock"
#define AppVersion "2.1"
#define AppPublisher "Votre Entreprise"
#define AppExeName "GestionStock.exe"

[Setup]
AppId={{F5E7C221-9C44-4D5B-8654-A3E24F3B9E1D}
AppName={#AppName}
AppVersion={#AppVersion}
AppPublisher={#AppPublisher}
DefaultDirName={autopf}\{#AppName}
DefaultGroupName={#AppName}
OutputDir=Installers
OutputBaseFilename=GestionStock_Setup_{#AppVersion}
Compression=lzma2/ultra
SolidCompression=yes
; Nécessite Windows 7 ou plus récent
MinVersion=6.1
; Installez pour tous les utilisateurs
PrivilegesRequired=admin
; Vérifiez les versions précédentes
AppMutex=GestionStockAppMutex

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"

[Tasks]
Name: "desktopicon"; Description: "Créer un raccourci sur le bureau"; GroupDescription: "Raccourcis:"
Name: "startupicon"; Description: "Lancer au démarrage de Windows"; GroupDescription: "Raccourcis:"

[Files]
; Fichier principal
Source: "C:\Projects\GestionStock\Win64\Release\GestionStock.exe"; DestDir: "{app}"; Flags: ignoreversion
; Fichiers de base
Source: "C:\Projects\GestionStock\Win64\Release\*.dll"; DestDir: "{app}"; Flags: ignoreversion
; Base de données par défaut
Source: "C:\Projects\GestionStock\Database\*"; DestDir: "{app}\Database"; Flags: ignoreversion recursesubdirs createallsubdirs
; Manuel utilisateur
Source: "C:\Projects\GestionStock\Docs\Manuel.pdf"; DestDir: "{app}\Docs"; Flags: ignoreversion

[Icons]
Name: "{group}\{#AppName}"; Filename: "{app}\{#AppExeName}"
Name: "{group}\Manuel utilisateur"; Filename: "{app}\Docs\Manuel.pdf"
Name: "{group}\{cm:UninstallProgram,{#AppName}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{#AppName}"; Filename: "{app}\{#AppExeName}"; Tasks: desktopicon
Name: "{commonstartup}\{#AppName}"; Filename: "{app}\{#AppExeName}"; Tasks: startupicon

[Registry]
; Enregistre l'application pour qu'elle s'ouvre avec les fichiers .gst
Root: HKCR; Subkey: ".gst"; ValueType: string; ValueName: ""; ValueData: "GestionStockFile"; Flags: uninsdeletevalue
Root: HKCR; Subkey: "GestionStockFile"; ValueType: string; ValueName: ""; ValueData: "Fichier GestionStock"; Flags: uninsdeletekey
Root: HKCR; Subkey: "GestionStockFile\DefaultIcon"; ValueType: string; ValueName: ""; ValueData: "{app}\{#AppExeName},0"
Root: HKCR; Subkey: "GestionStockFile\shell\open\command"; ValueType: string; ValueName: ""; ValueData: """{app}\{#AppExeName}"" ""%1"""

[Run]
; Exécute la configuration de la base de données si c'est une première installation
Filename: "{app}\{#AppExeName}"; Parameters: "/setup"; Flags: runhidden; Check: IsFirstInstall
; Propose de lancer l'application après l'installation
Filename: "{app}\{#AppExeName}"; Description: "Lancer {#AppName} maintenant"; Flags: nowait postinstall skipifsilent

[Code]
function IsFirstInstall: Boolean;
begin
  Result := not RegKeyExists(HKLM, 'Software\{#AppPublisher}\{#AppName}');
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    // Enregistre l'installation dans le registre
    RegWriteStringValue(HKLM, 'Software\{#AppPublisher}\{#AppName}', 'Version', '{#AppVersion}');
    RegWriteStringValue(HKLM, 'Software\{#AppPublisher}\{#AppName}', 'InstallPath', ExpandConstant('{app}'));
  end;
end;
```

## Conclusion

La création d'un installateur professionnel est une étape essentielle pour distribuer votre application Delphi. Inno Setup offre une solution gratuite et puissante pour la plupart des besoins, tandis qu'InstallAware propose des fonctionnalités avancées pour les applications commerciales complexes.

Quel que soit votre choix, assurez-vous de tester votre installateur sur différentes versions de Windows et dans différentes conditions pour garantir une expérience utilisateur optimale.

Dans la prochaine section, nous aborderons la signature de code, une étape importante pour renforcer la confiance des utilisateurs et éviter les avertissements de sécurité lors de l'installation.

## Exercice pratique

1. Téléchargez et installez Inno Setup
2. Créez un installateur basique pour l'une de vos applications Delphi
3. Ajoutez au moins une fonctionnalité avancée (vérification des prérequis, création d'un fichier de configuration, etc.)
4. Testez l'installateur sur un autre ordinateur ou une machine virtuelle

⏭️ [Signature de code](/17-distribution-et-deploiement/04-signature-de-code.md)
