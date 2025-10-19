🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.3 Création d'installateurs (Inno Setup, InstallAware)

## Introduction

Un installateur professionnel est la carte de visite de votre application. C'est la première chose que voient vos utilisateurs, et une installation simple et fluide crée immédiatement une impression positive. À l'inverse, une installation compliquée ou qui échoue peut conduire l'utilisateur à abandonner avant même d'avoir essayé votre application.

Dans cette section, nous allons explorer deux solutions populaires pour créer des installateurs professionnels pour vos applications Delphi : **Inno Setup** (gratuit et open source) et **InstallAware** (commercial avec de nombreuses fonctionnalités avancées).

## Qu'est-ce qu'un installateur ?

Un installateur est un programme qui :

1. **Copie les fichiers** de votre application aux bons emplacements
2. **Configure le système** (registre Windows, variables d'environnement, etc.)
3. **Crée des raccourcis** (menu Démarrer, bureau)
4. **Vérifie les prérequis** (système d'exploitation, frameworks nécessaires)
5. **Enregistre l'application** pour permettre sa désinstallation
6. **Gère les mises à jour** (détection de versions existantes)

### Pourquoi ne pas simplement copier les fichiers ?

Vous pourriez être tenté de simplement copier votre exécutable et demander aux utilisateurs de l'exécuter. C'est une mauvaise idée pour plusieurs raisons :

- **Professionnalisme** : Les utilisateurs s'attendent à un vrai installateur
- **Emplacement approprié** : Les applications doivent être installées dans `Program Files` ou `Program Files (x86)`
- **Permissions** : L'installateur gère les droits d'accès correctement
- **Désinstallation** : Sans installateur, difficile de supprimer proprement l'application
- **Intégration système** : Raccourcis, associations de fichiers, etc.

## Inno Setup : La solution gratuite et populaire

### Présentation

**Inno Setup** est un créateur d'installateurs gratuit, open source et très populaire dans l'écosystème Delphi. Il existe depuis 1997 et est utilisé par de nombreuses applications célèbres.

**Avantages** :
- Totalement gratuit et open source
- Très léger (installateurs de quelques MB)
- Excellente documentation
- Langage de script puissant (Pascal Script)
- Grande communauté et nombreux exemples
- Intégration possible dans l'IDE Delphi

**Inconvénients** :
- Interface moins moderne qu'InstallAware
- Courbe d'apprentissage pour les fonctionnalités avancées
- Moins de templates visuels prédéfinis

### Installation d'Inno Setup

1. **Téléchargement**
   - Rendez-vous sur : https://jrsoftware.org/isinfo.php
   - Téléchargez la dernière version (actuellement Inno Setup 6.x)
   - Téléchargez aussi **ISTool** si vous préférez une interface graphique

2. **Installation**
   - Exécutez le programme d'installation
   - Suivez les étapes (installation standard)
   - L'installateur lui-même est créé avec Inno Setup !

3. **Premier lancement**
   - Lancez **Inno Setup Compiler** depuis le menu Démarrer
   - Vous verrez l'éditeur de scripts

### Créer votre premier installateur avec Inno Setup

#### Étape 1 : Utiliser l'assistant

Inno Setup propose un assistant qui facilite la création d'un installateur de base.

1. **Lancer l'assistant**
   - Dans Inno Setup Compiler : `Fichier` → `Nouveau`
   - Sélectionnez "Créer un nouveau fichier de script à l'aide de l'assistant Script"
   - Cliquez sur `OK`

2. **Informations sur l'application**
   - **Nom de l'application** : "Mon Application Delphi"
   - **Version** : "1.0"
   - **Éditeur** : Votre nom ou nom de société
   - **Site web** : Votre site web
   - Cliquez sur `Suivant`

3. **Dossier de destination**
   - **Dossier par défaut** : Laissez `{autopf}\Mon Application Delphi`
     - `{autopf}` signifie "Program Files" approprié (32 ou 64 bits)
   - **Permettre de changer le dossier** : Cochez (recommandé)
   - Cliquez sur `Suivant`

4. **Fichiers de l'application**
   - **Fichier principal** : Cliquez sur `Parcourir` et sélectionnez votre `.exe`
   - **Autres fichiers** : Ajoutez les DLL, fichiers de données, etc.
   - **Autoriser l'utilisateur à lancer l'application** : Cochez
   - Cliquez sur `Suivant`

5. **Raccourcis**
   - **Menu Démarrer** : Cochez pour créer un groupe dans le menu Démarrer
   - **Nom du groupe** : "Mon Application Delphi"
   - **Bureau** : Cochez si vous voulez un raccourci sur le bureau
   - **Raccourci de désinstallation** : Cochez (recommandé)
   - Cliquez sur `Suivant`

6. **Documentation**
   - **Fichier Lisez-moi** : Ajoutez un README.txt si vous en avez un
   - **Licence** : Ajoutez votre fichier LICENSE.txt
   - Cliquez sur `Suivant`

7. **Langues**
   - Sélectionnez les langues que vous souhaitez supporter
   - Au minimum : Français et Anglais
   - Cliquez sur `Suivant`

8. **Options de compilation**
   - **Nom du fichier de sortie** : "setup" (donnera setup.exe)
   - **Icône personnalisée** : Choisissez une icône pour l'installateur
   - **Mot de passe** : Laissez vide (sauf besoin spécifique)
   - Cliquez sur `Suivant`

9. **Préprocesseur Inno Setup**
   - Laissez les options par défaut pour commencer
   - Cliquez sur `Suivant`

10. **Terminer l'assistant**
    - Cliquez sur `Terminer`
    - L'assistant génère un script `.iss`

#### Étape 2 : Comprendre le script généré

Inno Setup utilise des scripts texte avec l'extension `.iss`. Voici un exemple de script de base :

```ini
[Setup]
AppName=Mon Application Delphi
AppVersion=1.0
DefaultDirName={autopf}\Mon Application Delphi
DefaultGroupName=Mon Application Delphi
OutputDir=Output
OutputBaseFilename=setup
Compression=lzma2
SolidCompression=yes

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "Créer un raccourci sur le bureau"; GroupDescription: "Raccourcis supplémentaires:"

[Files]
Source: "C:\MonProjet\Win32\Release\MonApp.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "C:\MonProjet\Win32\Release\*.dll"; DestDir: "{app}"; Flags: ignoreversion

[Icons]
Name: "{group}\Mon Application"; Filename: "{app}\MonApp.exe"
Name: "{group}\Désinstaller Mon Application"; Filename: "{uninstallexe}"
Name: "{autodesktop}\Mon Application"; Filename: "{app}\MonApp.exe"; Tasks: desktopicon

[Run]
Filename: "{app}\MonApp.exe"; Description: "Lancer Mon Application"; Flags: nowait postinstall skipifsilent
```

**Explication des sections** :

- **[Setup]** : Configuration générale de l'installateur
- **[Languages]** : Langues supportées
- **[Tasks]** : Options que l'utilisateur peut choisir pendant l'installation
- **[Files]** : Fichiers à copier
- **[Icons]** : Raccourcis à créer
- **[Run]** : Programmes à exécuter après l'installation

#### Étape 3 : Personnaliser le script

Vous pouvez modifier le script pour ajouter des fonctionnalités :

**Ajouter des prérequis**

```ini
[Setup]
; Nécessite Windows 10 ou supérieur
MinVersion=10.0

[Code]
function InitializeSetup(): Boolean;
begin
  Result := True;
  if not IsDotNetInstalled(net462, 0) then
  begin
    MsgBox('Cette application nécessite .NET Framework 4.6.2.', mbError, MB_OK);
    Result := False;
  end;
end;
```

**Créer des associations de fichiers**

```ini
[Registry]
Root: HKCR; Subkey: ".monext"; ValueType: string; ValueData: "MonAppFile"; Flags: uninsdeletekey
Root: HKCR; Subkey: "MonAppFile"; ValueType: string; ValueData: "Fichier Mon Application"; Flags: uninsdeletekey
Root: HKCR; Subkey: "MonAppFile\DefaultIcon"; ValueType: string; ValueData: "{app}\MonApp.exe,0"; Flags: uninsdeletekey
Root: HKCR; Subkey: "MonAppFile\shell\open\command"; ValueType: string; ValueData: """{app}\MonApp.exe"" ""%1"""; Flags: uninsdeletekey
```

**Ajouter des composants optionnels**

```ini
[Components]
Name: "main"; Description: "Fichiers principaux"; Types: full compact custom; Flags: fixed
Name: "help"; Description: "Fichiers d'aide"; Types: full
Name: "samples"; Description: "Exemples"; Types: full

[Files]
Source: "MonApp.exe"; DestDir: "{app}"; Components: main
Source: "Aide\*"; DestDir: "{app}\Aide"; Components: help
Source: "Exemples\*"; DestDir: "{app}\Exemples"; Components: samples; Flags: recursesubdirs
```

#### Étape 4 : Compiler l'installateur

1. **Sauvegarder le script**
   - `Fichier` → `Enregistrer sous`
   - Donnez un nom : `MonApp.iss`

2. **Compiler**
   - Cliquez sur `Compiler` dans la barre d'outils (icône d'engrenage)
   - Ou appuyez sur `Ctrl+F9`
   - Ou menu `Compiler` → `Compiler`

3. **Vérifier la compilation**
   - La fenêtre de sortie affiche la progression
   - Si tout va bien : "Compilation réussie"
   - Cherchez les avertissements (warnings) et erreurs

4. **Tester l'installateur**
   - Le fichier `setup.exe` est créé dans le dossier `Output`
   - Testez-le sur une machine propre (idéalement une VM)

### Fonctionnalités avancées d'Inno Setup

#### Pascal Script

Inno Setup supporte un langage de script basé sur Pascal pour des logiques complexes :

```pascal
[Code]
var
  DataDirPage: TInputDirWizardPage;

procedure InitializeWizard;
begin
  // Créer une page personnalisée pour choisir le dossier de données
  DataDirPage := CreateInputDirPage(wpSelectDir,
    'Sélectionner le dossier des données',
    'Où voulez-vous stocker les données de l''application ?',
    'Les données de l''application seront stockées dans le dossier suivant.',
    False, '');
  DataDirPage.Add('');
  DataDirPage.Values[0] := ExpandConstant('{userdocs}\MonApp');
end;

function GetDataDir(Param: String): String;
begin
  Result := DataDirPage.Values[0];
end;
```

#### Vérification de versions existantes

```pascal
[Code]
function InitializeSetup(): Boolean;
var
  OldVersion: String;
  UninstallString: String;
begin
  Result := True;

  // Chercher une installation existante
  if RegQueryStringValue(HKLM, 'Software\Microsoft\Windows\CurrentVersion\Uninstall\MonApp_is1',
     'UninstallString', UninstallString) then
  begin
    if MsgBox('Une version de Mon Application est déjà installée. Voulez-vous la désinstaller ?',
              mbConfirmation, MB_YESNO) = IDYES then
    begin
      // Lancer la désinstallation
      Exec(RemoveQuotes(UninstallString), '/SILENT', '', SW_HIDE, ewWaitUntilTerminated, ResultCode);
    end
    else
      Result := False;
  end;
end;
```

## InstallAware : La solution professionnelle

### Présentation

**InstallAware** est une solution commerciale très complète pour créer des installateurs Windows professionnels. Elle est particulièrement appréciée pour sa facilité d'utilisation et ses nombreux templates.

**Avantages** :
- Interface graphique moderne et intuitive
- Templates professionnels nombreux
- Support des technologies modernes (Windows Store, AppX, MSIX)
- Intégration complète avec Delphi
- Support technique professionnel
- Fonctionnalités avancées (installations réseaux, déploiement silencieux, etc.)
- Conformité avec les standards Microsoft

**Inconvénients** :
- Payant (plusieurs centaines de dollars)
- Installateurs plus volumineux qu'avec Inno Setup
- Courbe d'apprentissage pour maîtriser toutes les fonctionnalités

### Versions et tarifs

InstallAware propose plusieurs éditions :

- **Express** : Version d'entrée de gamme (~300$)
- **Studio** : Version complète pour développeurs (~700$)
- **Developer** : Pour les grandes équipes (~1200$)

*Note : Les prix sont indicatifs et peuvent varier*

### Installation d'InstallAware

1. **Achat et téléchargement**
   - Visitez https://www.installaware.com
   - Achetez la licence appropriée
   - Téléchargez l'installateur

2. **Installation**
   - Exécutez l'installateur
   - Entrez votre clé de licence
   - Suivez l'assistant d'installation

3. **Intégration avec Delphi**
   - InstallAware peut s'intégrer directement dans l'IDE Delphi
   - Permet de créer des installateurs depuis Delphi

### Créer un installateur avec InstallAware

#### Étape 1 : Nouveau projet

1. **Lancer InstallAware Studio**
   - Démarrez InstallAware depuis le menu Démarrer

2. **Créer un nouveau projet**
   - Cliquez sur `File` → `New Project`
   - Choisissez un template (par exemple : "Windows Application")
   - Donnez un nom au projet

3. **Assistant de configuration**
   - InstallAware lance un assistant similaire à Inno Setup
   - Remplissez les informations de base

#### Étape 2 : Configuration du projet

**Informations générales**

Dans l'onglet `General` :
- **Product Name** : Nom de votre application
- **Product Version** : Version (ex: 1.0.0.0)
- **Company Name** : Votre société
- **Support Website** : Votre site web

**Fichiers à installer**

Dans l'onglet `Files` :
1. Cliquez sur `Add Files`
2. Naviguez vers votre dossier `Release`
3. Sélectionnez votre `.exe` et les fichiers nécessaires
4. InstallAware détecte automatiquement les dépendances

**Destination**

- **Installation Folder** : `$PROGRAMFILES$\[ProductName]`
- Les variables sont entourées de `$` au lieu de `{}`

#### Étape 3 : Configuration de l'interface

**Pages de l'installateur**

InstallAware utilise un système visuel de pages :

1. **Welcome Page** : Page d'accueil
   - Personnalisez le texte de bienvenue
   - Ajoutez votre logo

2. **License Agreement** : Accord de licence
   - Ajoutez votre fichier de licence (RTF ou TXT)

3. **Installation Folder** : Choix du dossier
   - Configuré automatiquement
   - Permettez ou non la personnalisation

4. **Ready to Install** : Confirmation
   - Résumé avant installation

5. **Progress** : Barre de progression
   - Affichage automatique

6. **Finish** : Page finale
   - Option pour lancer l'application
   - Option pour afficher le fichier README

**Personnalisation visuelle**

1. Dans l'onglet `Dialogs`
2. Double-cliquez sur une page pour la personnaliser
3. Modifiez :
   - Images de fond
   - Logo de votre application
   - Couleurs et polices
   - Textes et messages

#### Étape 4 : Fonctionnalités avancées

**Prérequis et redistributables**

InstallAware gère automatiquement de nombreux prérequis :

1. Allez dans l'onglet `Prerequisites`
2. Cochez les composants nécessaires :
   - Visual C++ Redistributable
   - .NET Framework
   - DirectX
   - SQL Server Express
   - Etc.

InstallAware téléchargera et installera automatiquement ces composants si nécessaires.

**Raccourcis et associations**

Dans l'onglet `Shortcuts` :
- **Start Menu** : Créez des raccourcis dans le menu Démarrer
- **Desktop** : Raccourci sur le bureau
- **Quick Launch** : Barre de lancement rapide

Dans l'onglet `File Associations` :
- Associez des extensions de fichiers à votre application
- Définissez les icônes et actions

**Registre Windows**

Dans l'onglet `Registry` :
- Ajoutez des clés de registre nécessaires
- Configurez les paramètres de l'application
- Gérez les licences

#### Étape 5 : Compilation

1. **Configurer les options de build**
   - `Build` → `Build Settings`
   - Choisissez le niveau de compression
   - Définissez le nom du fichier de sortie

2. **Compiler l'installateur**
   - Cliquez sur `Build` → `Build Setup`
   - Ou appuyez sur `F7`

3. **Résultat**
   - Un fichier `.exe` est créé dans le dossier de sortie
   - Testez-le sur une machine propre

### InstallAware vs Inno Setup : Tableau comparatif

| Critère | Inno Setup | InstallAware |
|---------|------------|--------------|
| **Prix** | Gratuit | Payant (300$+) |
| **Interface** | Éditeur de texte | Interface graphique moderne |
| **Courbe d'apprentissage** | Moyenne | Facile |
| **Taille des installateurs** | Petite | Moyenne |
| **Templates** | Peu | Nombreux |
| **Support** | Communauté | Support commercial |
| **Intégration IDE** | Manuelle | Native Delphi |
| **Prérequis** | Script manuel | Gestion automatique |
| **MSI/MSIX** | Non natif | Support complet |
| **Personnalisation** | Très flexible (script) | Très flexible (visuel) |

## Bonnes pratiques pour les installateurs

### 1. Tester, tester, tester

**Testez sur différentes configurations** :
- Windows 10 version 21H2, 22H2
- Windows 11
- Machines 32 bits et 64 bits (si applicable)
- Avec et sans droits administrateur
- Sur des machines "propres" (machines virtuelles)

**Testez différents scénarios** :
- Installation standard
- Installation personnalisée
- Installation silencieuse (`/SILENT` ou `/VERYSILENT`)
- Mise à jour d'une version existante
- Désinstallation complète

### 2. Respecter les conventions Windows

**Emplacements standards** :
- Programme : `C:\Program Files\VotreApp` (64-bit) ou `C:\Program Files (x86)\VotreApp` (32-bit)
- Données utilisateur : `%APPDATA%\VotreApp` ou `%LOCALAPPDATA%\VotreApp`
- Données communes : `%PROGRAMDATA%\VotreApp`
- Documents : `%USERPROFILE%\Documents\VotreApp`

**Ne jamais écrire dans** :
- Le dossier Program Files pendant l'exécution normale
- Le dossier Windows
- Le dossier System32

### 3. Gérer les permissions correctement

**Demander les droits administrateur** uniquement si nécessaire :
```ini
; Inno Setup
[Setup]
PrivilegesRequired=admin
```

**Pour InstallAware** :
- Définissez dans `General` → `Privileges Required`

Si votre application peut fonctionner sans droits admin, privilégiez une installation utilisateur.

### 4. Fournir des options d'installation

**Installation typique** :
- Tous les composants essentiels
- Configuration par défaut
- Pour 90% des utilisateurs

**Installation personnalisée** :
- Permet de choisir les composants
- Choisir l'emplacement
- Pour les utilisateurs avancés

**Installation minimale** :
- Seulement les fichiers essentiels
- Pour économiser l'espace disque

### 5. Gérer les mises à jour intelligemment

**Détecter les versions existantes** :
```pascal
; Inno Setup - Code Pascal
[Code]
function InitializeSetup(): Boolean;
var
  Version: String;
begin
  if RegQueryStringValue(HKLM, 'Software\MonApp', 'Version', Version) then
  begin
    // Une version existe
    if Version < '2.0' then
      MsgBox('Mise à jour depuis la version ' + Version, mbInformation, MB_OK);
  end;
  Result := True;
end;
```

**Préserver les données utilisateur** :
- Ne supprimez pas les fichiers de configuration
- Ne réinitialisez pas les préférences
- Sauvegardez les données avant mise à jour si nécessaire

### 6. Créer une désinstallation propre

**Supprimer tous les fichiers installés** :
- L'exécutable et les DLL
- Les fichiers de ressources
- Les raccourcis créés

**Ne pas supprimer** :
- Les fichiers de données utilisateur (proposer l'option)
- Les fichiers créés par l'utilisateur
- Les documents

**Nettoyer le registre** :
- Supprimer les clés créées par l'installateur
- Supprimer les associations de fichiers

### 7. Signature numérique

Signez votre installateur avec un certificat de signature de code :
- Établit la confiance
- Évite les avertissements Windows SmartScreen
- Requis pour le Microsoft Store

**Pour signer avec Inno Setup** :
```ini
[Setup]
SignTool=signtool sign /f "MonCertificat.pfx" /p "MotDePasse" /t http://timestamp.digicert.com $f
SignedUninstaller=yes
```

**InstallAware** :
- Configuration dans `Build Settings` → `Code Signing`

### 8. Informations de version et propriétés

**Assurez-vous que votre exécutable contient** :
- Numéro de version
- Nom du produit
- Copyright
- Description
- Icône

Dans Delphi : `Projet` → `Options` → `Version Info`

### 9. Documentation et support

**Inclure dans l'installateur** :
- Fichier README avec instructions de base
- Fichier de licence (LICENSE.txt)
- Documentation utilisateur (optionnel)
- Lien vers le support en ligne

**Page de fin d'installation** :
- Option "Afficher le fichier README"
- Option "Visiter le site web"
- Option "Lancer l'application"

### 10. Installation silencieuse

Permettez l'installation silencieuse pour les déploiements automatisés :

**Inno Setup** :
```
setup.exe /SILENT      ; Installation sans interface
setup.exe /VERYSILENT  ; Installation complètement invisible
setup.exe /DIR="C:\MonApp"  ; Spécifier le dossier
```

**InstallAware** :
- Support natif des installations silencieuses via MSI
- Paramètres : `/quiet`, `/passive`

## Checklist pour un installateur professionnel

Avant de distribuer votre installateur, vérifiez :

- [ ] L'installateur se lance sans erreur
- [ ] Toutes les dépendances sont incluses
- [ ] Les raccourcis sont créés correctement
- [ ] L'application se lance après installation
- [ ] La désinstallation supprime tout proprement
- [ ] L'installateur est signé numériquement
- [ ] Les informations de version sont correctes
- [ ] La licence est incluse et affichée
- [ ] Les prérequis sont détectés et installés
- [ ] L'interface est traduite dans les bonnes langues
- [ ] Les messages d'erreur sont clairs
- [ ] La taille de l'installateur est raisonnable
- [ ] Testé sur plusieurs configurations Windows
- [ ] Testé avec et sans droits administrateur
- [ ] Les mises à jour fonctionnent correctement

## Problèmes courants et solutions

### L'application ne se lance pas après installation

**Causes possibles** :
- DLL manquantes : Vérifiez les dépendances
- Permissions insuffisantes : Testez l'emplacement d'installation
- Chemins incorrects : Vérifiez les chemins relatifs/absolus

**Solution** : Utilisez des outils comme **Dependency Walker** pour identifier les DLL manquantes.

### Erreur "Accès refusé" pendant l'installation

**Cause** : Droits insuffisants

**Solution** :
- Demandez les droits administrateur
- Ou installez dans un dossier utilisateur

### L'installateur est bloqué par Windows SmartScreen

**Cause** : Installateur non signé ou nouvelle application

**Solution** :
- Signez votre installateur avec un certificat valide
- Construisez une réputation au fil du temps

### La désinstallation laisse des fichiers

**Cause** : Fichiers créés après installation non gérés

**Solution** :
```ini
; Inno Setup
[UninstallDelete]
Type: files; Name: "{app}\config.ini"
Type: filesandordirs; Name: "{app}\logs"
```

## Conclusion

Créer un installateur professionnel est une étape essentielle de la distribution de votre application Delphi. Que vous choisissiez **Inno Setup** pour sa simplicité et sa gratuité, ou **InstallAware** pour ses fonctionnalités avancées et son interface moderne, l'important est de créer une expérience d'installation fluide et professionnelle.

**Points clés à retenir** :

- Un bon installateur crée une première impression positive
- Testez toujours sur des machines propres
- Respectez les conventions et emplacements Windows
- Gérez proprement les mises à jour et la désinstallation
- Signez votre installateur pour établir la confiance
- Fournissez des options adaptées à différents types d'utilisateurs

Avec les outils et techniques présentés dans ce chapitre, vous êtes maintenant capable de créer des installateurs professionnels pour vos applications Delphi. Dans la section suivante, nous verrons comment renforcer encore la confiance en signant numériquement votre code.

⏭️ [Signature de code](/17-distribution-et-deploiement/04-signature-de-code.md)
