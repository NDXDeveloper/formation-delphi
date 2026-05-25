🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 17.3 Création d'installateurs (Inno Setup, InstallAware)

## Introduction

Un installateur professionnel est la carte de visite de votre application. C'est la première chose que voient vos utilisateurs, et une installation simple et fluide crée immédiatement une impression positive. À l'inverse, une installation compliquée ou qui échoue peut conduire l'utilisateur à abandonner avant même d'avoir essayé votre application.

Dans cette section, nous allons explorer deux solutions populaires pour créer des installateurs professionnels pour vos applications Delphi : **Inno Setup** (gratuit et open source) et **InstallAware** (commercial avec de nombreuses fonctionnalités avancées).

> 💡 **Le paysage des installateurs Windows en 2026** : au-delà des deux outils couverts ici en détail, il existe d'autres options à connaître :  
> - **WiX Toolset v6** (gratuit, open source, sortie 2025) — génère des packages MSI et MSIX. Référence pour les builds CI/CD professionnels, mais courbe d'apprentissage importante (XML déclaratif). Très utilisé par l'industrie. Voir section 17.8 pour le détail.  
> - **NSIS** (gratuit, open source) — alternative historique à Inno Setup, syntaxe différente. Plus utilisé dans le monde Windows général que dans la communauté Delphi.  
> - **Advanced Installer** (commercial, ~500-3000 €/an selon édition) — devenu en 2025-2026 l'alternative commerciale la plus populaire à InstallAware. Excellente interface visuelle, génération native MSI et MSIX, intégration Visual Studio et CI.  
> - **MSIX Packaging Tool** (gratuit, Microsoft) — pour convertir un installateur classique en package MSIX moderne (voir section 17.8).  
>  
> Le choix dépend de votre cible : grand public Windows → **Inno Setup** suffit largement. Distribution d'entreprise / GPO / SCCM → **WiX** ou **Advanced Installer** (MSI natif). Microsoft Store → **MSIX** (voir 17.8).

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
   - Téléchargez la dernière version (Inno Setup 6.x — la 6.4 sortie en 2024 est la version stable en 2026).
   - ⚠ **ISTool** (IDE alternatif souvent recommandé dans d'anciens tutoriels) **n'est plus maintenu depuis 2011**. L'IDE intégré à Inno Setup 6 est désormais largement suffisant. Pour une expérience plus moderne, l'extension **Inno Setup pour VS Code** (Marketplace) offre coloration syntaxique, snippets et compilation depuis VS Code.

2. **Installation**
   - Exécutez le programme d'installation.
   - Suivez les étapes (installation standard).
   - L'installateur lui-même est créé avec Inno Setup !

3. **Premier lancement**
   - Lancez **Inno Setup Compiler** depuis le menu Démarrer.
   - Vous verrez l'éditeur de scripts.

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
AppPublisher=Votre Société  
AppPublisherURL=https://www.votre-site.com  
DefaultDirName={autopf}\Mon Application Delphi  
DefaultGroupName=Mon Application Delphi  
OutputDir=Output  
OutputBaseFilename=setup  
Compression=lzma2  
SolidCompression=yes  
; ⚠ En 2026, privilégier le 64 bits. Forcer une installation 64 bits
;   si la build est compilée pour Win64 :
; ⚠ Depuis Inno Setup 6.3 (2024), `x64` est DÉPRÉCIÉ. Utiliser `x64compatible`
;   qui couvre à la fois Windows x64 ET Windows on ARM (émulation x64).
;   Le compilateur affiche un warning si vous gardez `x64`.
ArchitecturesInstallIn64BitMode=x64compatible  
ArchitecturesAllowed=x64compatible  

[Languages]
Name: "french"; MessagesFile: "compiler:Languages\French.isl"  
Name: "english"; MessagesFile: "compiler:Default.isl"  

[Tasks]
Name: "desktopicon"; Description: "Créer un raccourci sur le bureau"; GroupDescription: "Raccourcis supplémentaires:"

[Files]
; ⚠ Adaptez le chemin source à votre dossier de build Delphi (Win64\Release
;   pour Delphi 13 Florence en 64 bits, ou Win32\Release pour le 32 bits).
Source: "Win64\Release\MonApp.exe"; DestDir: "{app}"; Flags: ignoreversion  
Source: "Win64\Release\*.dll"; DestDir: "{app}"; Flags: ignoreversion  

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
; Nécessite Windows 10 1607 (build 14393) ou supérieur.
; Format : Major.Minor.Build. Pour Windows 11 24H2 : 10.0.26100.
; ⚠ Windows 10 atteint sa fin de support gratuit le 14 octobre 2025 ;
;   privilégier Windows 11 comme cible principale en 2026.
MinVersion=10.0.14393

[Code]
function InitializeSetup(): Boolean;  
begin  
  Result := True;
  // ⚠ Pour Delphi pur, cette vérification n'est généralement PAS
  //   nécessaire — Delphi compile en code natif sans dépendance .NET.
  //   À vérifier uniquement si vous appelez du code .NET depuis Delphi
  //   (COM Interop, Hydra, etc.).
  //
  // ⚠ Repères de support .NET en 2026 :
  //   - .NET Framework 4.6.2  : fin de support prévue 12 janvier 2027.
  //   - .NET Framework 4.7 / 4.7.1 : fin de support prévue 12 janvier 2027.
  //   - .NET Framework 4.7.2 / 4.8 / 4.8.1 : supportées (cycle lié à
  //     Windows ; pas de date de fin annoncée).
  //   - .NET 8 (LTS) : supporté jusqu'en novembre 2026.
  //   - .NET 10 (LTS, novembre 2025) : supporté jusqu'en novembre 2028.
  //
  // ⚠ `IsDotNetInstalled(...)` N'EST PAS native d'Inno Setup. Elle vient
  //   d'un script helper de Stein Åsmul couramment réutilisé. Pour l'avoir,
  //   téléchargez `isxdl.iss` / `donetfx_install.iss` ou écrivez votre
  //   propre helper via `RegQueryDWordValue` sur la clé :
  //     HKLM\SOFTWARE\Microsoft\NET Framework Setup\NDP\v4\Full → Release
  //   (numéro >= 528040 pour .NET Framework 4.8, >= 533320 pour 4.8.1).
  if not IsDotNetInstalled(net48, 0) then
  begin
    MsgBox('Cette application nécessite .NET Framework 4.8.', mbError, MB_OK);
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
  UninstallString: String;
  ResultCode: Integer;  // ⚠ Doit être déclaré (out param d'Exec).
begin
  Result := True;

  // Chercher une installation existante.
  // ⚠ Sur une cible 64 bits, l'entrée d'uninstall est généralement
  //   sous HKLM64 (pas HKLM 32 bits). Utiliser RegQueryStringValue
  //   avec la clé appropriée selon `IsWin64`.
  if RegQueryStringValue(HKLM, 'Software\Microsoft\Windows\CurrentVersion\Uninstall\MonApp_is1',
     'UninstallString', UninstallString) then
  begin
    if MsgBox('Une version de Mon Application est déjà installée. Voulez-vous la désinstaller ?',
              mbConfirmation, MB_YESNO) = IDYES then
    begin
      // Lancer la désinstallation
      Exec(RemoveQuotes(UninstallString), '/SILENT', '', SW_HIDE,
           ewWaitUntilTerminated, ResultCode);
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

InstallAware gère automatiquement de nombreux prérequis. À utiliser **seulement si nécessaire** — Delphi compile en code natif et n'a généralement aucune de ces dépendances :

1. Allez dans l'onglet `Prerequisites`.
2. Cochez les composants nécessaires :
   - **Visual C++ Redistributable** : uniquement si vous utilisez des composants tiers C++ ou des DLL liées à MSVCRT (rare pour du Delphi pur).
   - **.NET Framework / .NET (Core)** : uniquement si vous appelez du code .NET (COM Interop, Hydra). Pour 2026, viser .NET 8 LTS ou supérieur.
   - **DirectX** : si vous utilisez DirectX directement (FMX utilise déjà DirectX en interne sur Windows mais n'a pas besoin du redistribuable séparé).
   - **SQL Server Express** : pour les apps qui embarquent une base SQL Server locale (LocalDB ou Express).
   - **WebView2 Runtime** : si vous utilisez `TEdgeBrowser` (le composant Edge dans FMX/VCL). Pré-installé sur Windows 11, mais à vérifier sur Windows 10.

InstallAware téléchargera et installera automatiquement ces composants si nécessaires.

**Raccourcis et associations**

Dans l'onglet `Shortcuts` :
- **Start Menu** : créez des raccourcis dans le menu Démarrer.
- **Desktop** : raccourci sur le bureau (à laisser optionnel, beaucoup d'utilisateurs préfèrent un bureau dégagé).
- ~~**Quick Launch**~~ : barre supprimée par Microsoft depuis Windows 7 (ne plus utiliser). Sur Windows 10/11, préférer l'épinglage à la barre des tâches.

Dans l'onglet `File Associations` :
- Associez des extensions de fichiers à votre application.
- Définissez les icônes et actions.
- ⚠ Sur Windows 10+, les associations sont **proposées** à l'utilisateur via le panneau « Applications par défaut » — l'installateur ne peut plus forcer l'association comme avant Windows 8.

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
- Windows 11 23H2 et 24H2 (cible principale en 2026)
- Windows 10 22H2 (fin de support gratuit oct. 2025 — encore présent chez beaucoup d'utilisateurs)
- Windows Server 2019, 2022, 2025 (si votre app a des usages serveur)
- Machines 32 bits (Win32) et 64 bits (Win64), ARM64 si vous ciblez Windows on ARM
- Avec et sans droits administrateur
- Sur des machines "propres" (machines virtuelles ou conteneurs Windows Sandbox)

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
// ⚠ NE PAS comparer des versions comme des STRINGS : `'1.10' < '2.0'` est
//   vrai (caractère par caractère, '1' < '2') MAIS `'1.10' < '1.9'` est
//   également vrai par défaut, car '.' = '.' puis '1' < '9' — ce qui est
//   faux numériquement (1.10 > 1.9). Toujours comparer en parsant les
//   composantes. Inno Setup fournit `CompareVersion` (depuis 6.4) ou
//   on peut utiliser un helper StrToInt sur chaque morceau.

function ParseVersionMajorMinor(const V: string; out Major, Minor: Integer): Boolean;  
var  
  DotPos: Integer;
begin
  Major := 0; Minor := 0;
  DotPos := Pos('.', V);
  if DotPos = 0 then
  begin
    Result := TryStrToInt(V, Major);
    Exit;
  end;
  Result := TryStrToInt(Copy(V, 1, DotPos - 1), Major) and
            TryStrToInt(Copy(V, DotPos + 1, MaxInt), Minor);
end;

function InitializeSetup(): Boolean;  
var  
  VersionStr: String;
  Major, Minor: Integer;
begin
  Result := True;
  // ⚠ Sur Windows 64 bits : un Inno Setup compilé en 32 bits lit par défaut
  //   HKLM\Software\WOW6432Node\... (redirection automatique). Pour lire
  //   la branche 64 bits, utilisez la constante HKLM64 (Inno Setup 5.x+).
  if RegQueryStringValue(HKLM, 'Software\MonApp', 'Version', VersionStr) then
  begin
    if ParseVersionMajorMinor(VersionStr, Major, Minor) then
    begin
      // Comparaison numérique correcte : "1.10" est bien > "1.9".
      if (Major < 2) then
        MsgBox('Mise à jour depuis la version ' + VersionStr,
               mbInformation, MB_OK);
    end;
  end;
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

> ⚠️ **Depuis juin 2023, les certificats OV et EV Code Signing doivent obligatoirement être stockés sur un module matériel** (HSM, token USB type SafeNet eToken, YubiKey 5 FIPS) selon les *CA/Browser Forum Baseline Requirements*. Vous ne recevrez plus de `.pfx` exportable par email — le token vous est envoyé par courrier physique. Voir section 17.4 pour le détail.

**Pour signer avec Inno Setup** (commande moderne 2026) :
```ini
; ⚠ Inno Setup ne fait PAS d'expansion `$VAR` style shell sur la
;   ligne SignTool. Pour injecter une variable d'environnement, deux
;   approches :
;     1. Syntaxe Windows `%VAR%` — interprétée par le shell appelé
;        par Inno Setup au moment de la compilation.
;     2. Préprocesseur Inno Setup : `#define MyPwd GetEnv("SIGNPWD")`
;        puis `{#MyPwd}` dans la commande.
;   ⚠ Un mot de passe en clair sur la ligne de commande reste visible
;   dans `tasklist` et l'historique. Préférer un token hardware avec
;   PIN (cf bloc suivant), ou stocker le PIN dans un coffre (Vault,
;   GitHub Secrets, etc.).
;
; Commande moderne (RFC 3161, SHA-256) :
;   /fd sha256        : algorithme de digest (PAS sha1, déprécié)
;   /tr <url> /td sha256 : timestamp RFC 3161 SHA-256 (PAS /t legacy)
;   $f                : placeholder Inno Setup pour le fichier à signer
[Setup]
SignTool=signtool sign /f "MonCertificat.pfx" /p "%SIGNPWD%" /fd sha256 /tr http://timestamp.digicert.com /td sha256 $f  
SignedUninstaller=yes  
```

**Avec un token hardware** (EV ou OV moderne, obligatoire depuis juin 2023), la commande devient :
```ini
; /sha1 cible le certificat par empreinte SHA-1 du sujet (pas du digest
; de signature). /a laisse signtool choisir automatiquement le meilleur
; certificat disponible sur le store si /sha1 est omis.
[Setup]
SignTool=signtool sign /sha1 "EMPREINTE_DU_CERTIFICAT" /fd sha256 /tr http://timestamp.digicert.com /td sha256 $f  
SignedUninstaller=yes  
```

**InstallAware / Advanced Installer / WiX** :
- Tous trois intègrent la signature en post-build, avec des options équivalentes pour préciser l'algorithme et le serveur de timestamp.

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

**Inno Setup** — switches courants :
```
setup.exe /SILENT              ; Installation sans interface (erreurs visibles)  
setup.exe /VERYSILENT          ; Installation complètement invisible  
setup.exe /SUPPRESSMSGBOXES    ; Supprime aussi les MsgBox (à combiner avec /SILENT)  
setup.exe /NORESTART           ; Empêche le redémarrage automatique éventuel  
setup.exe /DIR="C:\MonApp"     ; Spécifier le dossier d'installation  
setup.exe /TASKS="desktopicon" ; Activer/désactiver des tâches optionnelles  
setup.exe /LOG="install.log"   ; Tracer toute l'installation dans un fichier  
setup.exe /LANG=french         ; Forcer la langue (si plusieurs définies)  
```

> 💡 **Combo recommandé pour le déploiement GPO/SCCM** :  
> `setup.exe /VERYSILENT /SUPPRESSMSGBOXES /NORESTART /LOG="%TEMP%\monapp_install.log"`

**InstallAware** :
- Support natif des installations silencieuses via MSI (puisque InstallAware peut générer un MSI sous-jacent).
- Paramètres MSI standards : `msiexec /i monapp.msi /quiet /norestart /l*v install.log`
- Pour les setup.exe InstallAware bootstrappers : `setup.exe /s` (silencieux) ou `setup.exe /a` (mode admin install).

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

**Solution** : Utilisez des outils modernes pour identifier les DLL manquantes :
- **Dependencies** (https://github.com/lucasg/Dependencies) — successeur moderne et open source de Dependency Walker. ⚠ Dependency Walker (depends.exe) historique de 2006 est désormais obsolète et produit beaucoup de faux positifs sur les *API Sets* de Windows 10/11.
- **Process Monitor (Sysinternals)** — pour observer en temps réel les DLL recherchées au lancement.
- **dumpbin /dependents** (Visual Studio Build Tools, gratuit) — listing CLI des dépendances directes d'un PE.

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
