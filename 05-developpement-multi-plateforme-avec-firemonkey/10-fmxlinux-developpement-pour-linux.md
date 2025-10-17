🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.10 FMXLinux : développement pour Linux

## Introduction

Linux représente un écosystème important dans le monde du développement logiciel, particulièrement pour les serveurs, les environnements scientifiques, et les postes de travail de développeurs. Avec FMXLinux, Embarcadero a étendu FireMonkey pour permettre le développement d'applications graphiques Linux natives. Dans cette section, nous allons découvrir comment créer des applications Delphi qui fonctionnent sur Linux, complétant ainsi la couverture multi-plateforme de FireMonkey.

## 1. Qu'est-ce que FMXLinux ?

### Définition

**FMXLinux** est l'extension de FireMonkey qui permet de compiler et exécuter des applications FireMonkey sur les systèmes Linux 64 bits. C'est le même framework FireMonkey que vous utilisez pour Windows, macOS, iOS et Android, mais ciblant maintenant Linux.

### Un peu d'histoire

**2017** : Première version de FMXLinux avec Delphi 10.2 Tokyo
**Aujourd'hui** : Support mature et stable dans Delphi

FMXLinux comble un vide important : avant son existence, il n'y avait pas de solution RAD (Rapid Application Development) simple pour créer des applications graphiques Linux avec Delphi.

### Que peut-on faire avec FMXLinux ?

**Applications desktop Linux** :
- Outils de gestion et d'administration
- Applications métier
- Outils de développement
- Dashboards et monitoring
- Applications scientifiques
- Clients d'API et services

**Avantages** :
- ✅ Même code que Windows/macOS/mobile
- ✅ Interface graphique native
- ✅ Performance native (compilé, pas interprété)
- ✅ Accès aux API Linux
- ✅ Pas de dépendances Java ou .NET

## 2. Distributions Linux supportées

### Distributions principales

FMXLinux fonctionne sur les principales distributions Linux 64 bits :

**Famille Debian** :
- Ubuntu 18.04 LTS, 20.04 LTS, 22.04 LTS
- Debian 10, 11, 12
- Linux Mint
- Elementary OS

**Famille Red Hat** :
- Fedora 36, 37, 38
- Red Hat Enterprise Linux (RHEL) 8, 9
- CentOS Stream
- Rocky Linux

**Autres** :
- openSUSE Leap, Tumbleweed
- Arch Linux
- Manjaro

### Environnements de bureau

FMXLinux fonctionne avec tous les environnements de bureau populaires :
- **GNOME** (Ubuntu, Fedora par défaut)
- **KDE Plasma** (Kubuntu, openSUSE)
- **XFCE** (Xubuntu, léger)
- **Cinnamon** (Linux Mint)
- **MATE** (léger et traditionnel)

**Important** : Votre application fonctionnera de la même manière quel que soit l'environnement de bureau.

## 3. Configuration de l'environnement

### Prérequis

**Sur votre PC Windows (développement)** :
- Delphi 10.2 Tokyo ou supérieur (Professional, Enterprise ou Architect)
- Connexion réseau vers une machine Linux

**Sur la machine Linux (cible)** :
- Distribution Linux 64 bits
- GTK 3 installé (généralement préinstallé)
- PAServer Linux (fourni avec Delphi)
- Connexion réseau vers votre PC Windows

### Architecture de développement

```
┌─────────────────────┐         Réseau           ┌──────────────────────┐
│   PC Windows        │ ◄──────────────────────► │  Machine Linux       │
│                     │                          │                      │
│  - Delphi IDE       │   Compilation à distance │  - PAServer          │
│  - Code source      │   Déploiement            │  - Application       │
│  - Débogage         │   Exécution              │  - Bibliothèques     │
└─────────────────────┘                          └──────────────────────┘
```

Vous développez sur Windows, mais la compilation et l'exécution se font sur Linux via PAServer.

### Installation de PAServer sur Linux

**Étape 1 : Copier PAServer**

PAServer pour Linux se trouve dans votre installation Delphi :
```
C:\Program Files (x86)\Embarcadero\Studio\XX.0\PAServer\LinuxPAServer23.0.tar.gz
```

Copiez ce fichier sur votre machine Linux (via SFTP, clé USB, etc.)

**Étape 2 : Extraire et installer**

Sur Linux, dans un terminal :
```bash
# Créer un répertoire pour PAServer
mkdir ~/PAServer
cd ~/PAServer

# Extraire l'archive
tar -xzf LinuxPAServer23.0.tar.gz

# Rendre le script exécutable
chmod +x paserver
```

**Étape 3 : Installer les dépendances**

Ubuntu/Debian :
```bash
sudo apt-get update
sudo apt-get install libgtk-3-0 libgtk-3-dev
sudo apt-get install joe wget p7zip-full curl openssh-server
sudo apt-get install build-essential
sudo apt-get install zlib1g-dev libcurl4-gnutls-dev
```

Fedora/Red Hat :
```bash
sudo dnf install gtk3 gtk3-devel
sudo dnf install joe wget p7zip curl openssh-server
sudo dnf install gcc gcc-c++ make
sudo dnf install zlib-devel libcurl-devel
```

**Étape 4 : Lancer PAServer**

```bash
cd ~/PAServer
./paserver
```

Première exécution : PAServer vous demandera de créer un mot de passe.

```
PAServer version 23.0
Password:
Confirm password:

PAServer started on port 64211
```

**Astuce** : Noter le port (64211 par défaut) et le mot de passe.

### Configuration dans Delphi

**Étape 1 : Créer un profil de connexion**

1. Dans Delphi : **Tools → Options**
2. Aller dans **Connection Profile Manager**
3. Cliquer **Add**
4. Configurer :
   - **Profile name** : Linux64 (ou un nom de votre choix)
   - **Platform** : Linux64
   - **Host name** : Adresse IP de votre machine Linux
   - **Port number** : 64211 (par défaut)
   - **Password** : Le mot de passe PAServer

**Étape 2 : Tester la connexion**

1. Cliquer **Test Connection**
2. Si succès : "Connection established successfully"
3. Cliquer **OK** pour sauvegarder

### Ajouter la plateforme Linux64 au projet

**Méthode 1 : Nouveau projet**

1. **File → New → Multi-Device Application - Delphi**
2. Choisir un template (Blank Application)
3. Dans **Project Manager**, clic droit sur **Target Platforms**
4. **Add Platform → Linux 64-bit**
5. Double-cliquer sur **Linux64** pour l'activer

**Méthode 2 : Projet existant**

Si vous avez déjà un projet FireMonkey :
1. **Project → Target Platforms**
2. **Add Platform → Linux 64-bit**
3. Sélectionner le profil de connexion créé
4. OK

## 4. Premier projet FMXLinux

### Créer une application simple

**Étape 1 : Nouveau projet**
```
File → New → Multi-Device Application
Template : Blank Application
```

**Étape 2 : Ajouter des composants**

Sur le formulaire :
- 1 TLabel (renommer en LabelTitre)
- 1 TEdit (renommer en EditNom)
- 1 TButton (renommer en ButtonBonjour)
- 1 TMemo (renommer en MemoResultat)

**Étape 3 : Code**

```pascal
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    LabelTitre: TLabel;
    EditNom: TEdit;
    ButtonBonjour: TButton;
    MemoResultat: TMemo;
    procedure ButtonBonjourClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  LabelTitre.Text := 'Application Linux avec FMXLinux';
  EditNom.TextPrompt := 'Entrez votre nom';
  ButtonBonjour.Text := 'Dire Bonjour';

  {$IFDEF LINUX}
  Caption := 'FMXLinux Demo - Linux';
  {$ELSE}
  Caption := 'FMXLinux Demo - ' + TOSVersion.ToString;
  {$ENDIF}
end;

procedure TForm1.ButtonBonjourClick(Sender: TObject);
var
  Message: string;
begin
  if EditNom.Text.Trim.IsEmpty then
  begin
    ShowMessage('Veuillez entrer votre nom');
    Exit;
  end;

  Message := Format('Bonjour %s !', [EditNom.Text]);

  {$IFDEF LINUX}
  Message := Message + #13#10 + 'Vous utilisez Linux !';
  {$ELSE}
  Message := Message + #13#10 + 'Vous utilisez : ' + TOSVersion.ToString;
  {$ENDIF}

  MemoResultat.Lines.Add(TimeToStr(Now) + ' - ' + Message);
end;

end.
```

**Étape 4 : Compiler et exécuter**

1. Sélectionner la plateforme **Linux64**
2. **Run → Run (F9)**
3. Delphi compile, déploie sur Linux via PAServer
4. L'application s'exécute sur Linux !

### Observer le déploiement

Dans la fenêtre Messages de Delphi, vous verrez :
```
[PAClient] Connecting to 192.168.1.100:64211...
[PAClient] Connected
[Deploying] Project1 (Linux64)
[Deploying] Transferring files...
[Deploying] Complete
[Running] /home/user/PAServer/scratch-dir/Project1
```

## 5. Spécificités Linux

### Système de fichiers

**Chemins Linux** :
```pascal
// Séparateur : / (pas \)
var
  CheminLinux: string;
begin
  {$IFDEF LINUX}
  CheminLinux := '/home/user/documents/fichier.txt';
  {$ELSE}
  CheminLinux := 'C:\Users\user\documents\fichier.txt';
  {$ENDIF}
end;
```

**Utiliser TPath pour la portabilité** :
```pascal
uses
  System.IOUtils;

var
  CheminPortable: string;
begin
  // S'adapte automatiquement au système
  CheminPortable := TPath.Combine(
    TPath.GetDocumentsPath,
    'MonFichier.txt'
  );

  // Linux : /home/user/Documents/MonFichier.txt
  // Windows : C:\Users\user\Documents\MonFichier.txt
end;
```

### Chemins standards Linux

```pascal
{$IFDEF LINUX}
uses
  Posix.Stdlib;

function ObtenirCheminHome: string;
begin
  Result := string(getenv('HOME'));
  // Exemple : /home/username
end;

function ObtenirCheminConfig: string;
begin
  Result := TPath.Combine(ObtenirCheminHome, '.config', 'MonApp');
  // Exemple : /home/username/.config/MonApp
end;

function ObtenirCheminDonnees: string;
begin
  Result := TPath.Combine(ObtenirCheminHome, '.local', 'share', 'MonApp');
  // Exemple : /home/username/.local/share/MonApp
end;
{$ENDIF}
```

### Permissions fichiers

Sur Linux, les permissions sont importantes :

```pascal
{$IFDEF LINUX}
uses
  Posix.SysStat;

procedure RendreExecutable(const Fichier: string);
var
  StatBuf: _stat;
begin
  // Obtenir les permissions actuelles
  if stat(PAnsiChar(AnsiString(Fichier)), StatBuf) = 0 then
  begin
    // Ajouter permission d'exécution
    chmod(PAnsiChar(AnsiString(Fichier)),
          StatBuf.st_mode or S_IXUSR or S_IXGRP or S_IXOTH);
  end;
end;
{$ENDIF}
```

### Variables d'environnement

```pascal
{$IFDEF LINUX}
uses
  Posix.Stdlib;

function ObtenirVariableEnv(const NomVar: string): string;
var
  P: PAnsiChar;
begin
  P := getenv(PAnsiChar(AnsiString(NomVar)));
  if P <> nil then
    Result := string(P)
  else
    Result := '';
end;

// Exemples
procedure AfficherInfosSysteme;
begin
  ShowMessage('User : ' + ObtenirVariableEnv('USER'));
  ShowMessage('Home : ' + ObtenirVariableEnv('HOME'));
  ShowMessage('Shell : ' + ObtenirVariableEnv('SHELL'));
  ShowMessage('Desktop : ' + ObtenirVariableEnv('XDG_CURRENT_DESKTOP'));
end;
{$ENDIF}
```

### Exécuter des commandes système

```pascal
{$IFDEF LINUX}
uses
  Posix.Stdlib, Posix.Unistd;

function ExecuterCommande(const Commande: string): Integer;
begin
  Result := system(PAnsiChar(AnsiString(Commande)));
end;

// Exemples d'utilisation
procedure ExemplesCommandes;
begin
  // Ouvrir un fichier avec l'application par défaut
  ExecuterCommande('xdg-open /home/user/document.pdf');

  // Ouvrir une URL dans le navigateur
  ExecuterCommande('xdg-open https://www.example.com');

  // Notification desktop
  ExecuterCommande('notify-send "Titre" "Message de notification"');
end;
{$ENDIF}
```

### Détecter l'environnement de bureau

```pascal
{$IFDEF LINUX}
function ObtenirEnvironnementBureau: string;
begin
  Result := ObtenirVariableEnv('XDG_CURRENT_DESKTOP');
  // Retourne : GNOME, KDE, XFCE, etc.
end;

function EstGNOME: Boolean;
begin
  Result := ObtenirEnvironnementBureau.Contains('GNOME');
end;

function EstKDE: Boolean;
begin
  Result := ObtenirEnvironnementBureau.Contains('KDE');
end;
{$ENDIF}
```

## 6. Interface utilisateur sur Linux

### Apparence native

FireMonkey sur Linux utilise GTK3, ce qui donne une apparence cohérente avec le système :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF LINUX}
  // L'apparence est automatiquement GTK3
  // Les boutons, menus, dialogues ressemblent à des apps Linux natives
  {$ENDIF}
end;
```

### Dialogues système

Les dialogues FireMonkey s'affichent comme des dialogues GTK natifs :

```pascal
procedure TForm1.AfficherDialogues;
begin
  // Dialogue message - apparence native GTK
  ShowMessage('Message Linux natif');

  // Dialogue confirmation
  if MessageDlg('Voulez-vous continuer ?',
                TMsgDlgType.mtConfirmation,
                [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    ShowMessage('Vous avez cliqué Oui');

  // Dialogue ouverture de fichier
  var OpenDialog := TOpenDialog.Create(nil);
  try
    OpenDialog.Filter := 'Fichiers texte|*.txt|Tous les fichiers|*.*';
    if OpenDialog.Execute then
      ShowMessage('Fichier sélectionné : ' + OpenDialog.FileName);
  finally
    OpenDialog.Free;
  end;
end;
```

### Icônes et thème système

```pascal
procedure TForm1.AdapterAuTheme;
begin
  {$IFDEF LINUX}
  // FireMonkey s'adapte au thème clair/sombre du système
  // Pas de code spécial nécessaire

  // Pour une icône d'application
  if FileExists('/usr/share/icons/hicolor/48x48/apps/monapp.png') then
    Application.Icon.LoadFromFile(
      '/usr/share/icons/hicolor/48x48/apps/monapp.png');
  {$ENDIF}
end;
```

## 7. Déploiement d'applications Linux

### Structure de déploiement

Après compilation, votre application Linux contient :
```
MonApp/
├── MonApp (exécutable)
├── libcgsqlite3.so (si base de données)
├── libmidas.so (si DataSnap)
└── (autres dépendances si nécessaires)
```

### Méthode 1 : Binaire simple

**Pour distribution simple** :

1. Compiler en mode Release
2. Récupérer le binaire depuis PAServer :
   ```
   /home/user/PAServer/scratch-dir/MonApp/Linux64/Release/MonApp
   ```
3. Distribuer avec un script d'installation

**Script install.sh** :
```bash
#!/bin/bash
# Installation de MonApp

APP_NAME="MonApp"
INSTALL_DIR="/opt/$APP_NAME"
DESKTOP_FILE="/usr/share/applications/$APP_NAME.desktop"

echo "Installation de $APP_NAME..."

# Créer le répertoire d'installation
sudo mkdir -p $INSTALL_DIR
sudo cp $APP_NAME $INSTALL_DIR/
sudo chmod +x $INSTALL_DIR/$APP_NAME

# Créer le raccourci desktop
sudo cat > $DESKTOP_FILE << EOF
[Desktop Entry]
Version=1.0
Type=Application
Name=$APP_NAME
Comment=Mon Application Linux
Exec=$INSTALL_DIR/$APP_NAME
Icon=$INSTALL_DIR/icon.png
Terminal=false
Categories=Utility;
EOF

echo "Installation terminée !"
echo "Lancez l'application depuis le menu Applications."
```

### Méthode 2 : Package .deb (Debian/Ubuntu)

**Structure du package** :
```
monapp_1.0-1_amd64/
├── DEBIAN/
│   └── control
├── opt/
│   └── monapp/
│       ├── MonApp
│       └── icon.png
└── usr/
    └── share/
        └── applications/
            └── monapp.desktop
```

**Fichier control** :
```
Package: monapp
Version: 1.0-1
Section: utils
Priority: optional
Architecture: amd64
Depends: libgtk-3-0
Maintainer: Votre Nom <email@exemple.com>
Description: Mon Application Linux
 Description détaillée de votre application
 sur plusieurs lignes si nécessaire.
```

**Créer le package** :
```bash
# Construire le package
dpkg-deb --build monapp_1.0-1_amd64

# Installer localement pour tester
sudo dpkg -i monapp_1.0-1_amd64.deb
```

### Méthode 3 : AppImage (Portable)

**AppImage** = fichier unique exécutable sur toutes les distributions

**Avantages** :
- Un seul fichier
- Pas d'installation nécessaire
- Fonctionne partout
- Inclut toutes les dépendances

**Création d'AppImage** :

1. Télécharger **appimagetool**
2. Créer la structure :
```
MonApp.AppDir/
├── AppRun (script de lancement)
├── MonApp (exécutable)
├── monapp.desktop
├── icon.png
└── usr/
    └── lib/ (bibliothèques)
```

3. Créer l'AppImage :
```bash
./appimagetool-x86_64.AppImage MonApp.AppDir
```

Résultat : `MonApp-x86_64.AppImage` (fichier unique distributable)

### Méthode 4 : Flatpak

**Flatpak** = système de packaging moderne pour Linux

**Avantages** :
- Sandboxing (sécurité)
- Distribution via Flathub
- Mises à jour automatiques
- Isolation des dépendances

**Fichier manifest (org.exemple.MonApp.yaml)** :
```yaml
app-id: org.exemple.MonApp
runtime: org.freedesktop.Platform
runtime-version: '22.08'
sdk: org.freedesktop.Sdk
command: MonApp

finish-args:
  - --share=ipc
  - --socket=x11
  - --socket=wayland
  - --device=dri
  - --filesystem=home

modules:
  - name: MonApp
    buildsystem: simple
    build-commands:
      - install -D MonApp /app/bin/MonApp
      - install -D monapp.desktop /app/share/applications/org.exemple.MonApp.desktop
      - install -D icon.png /app/share/icons/hicolor/256x256/apps/org.exemple.MonApp.png
    sources:
      - type: file
        path: MonApp
      - type: file
        path: monapp.desktop
      - type: file
        path: icon.png
```

**Construire le Flatpak** :
```bash
flatpak-builder --force-clean build-dir org.exemple.MonApp.yaml
flatpak-builder --repo=repo --force-clean build-dir org.exemple.MonApp.yaml
```

## 8. Intégration système Linux

### Fichier .desktop

Pour que votre application apparaisse dans le menu :

**monapp.desktop** :
```ini
[Desktop Entry]
Version=1.0
Type=Application
Name=Mon Application
Name[fr]=Mon Application
Comment=Description courte
Comment[fr]=Description courte en français
Exec=/opt/monapp/MonApp
Icon=/opt/monapp/icon.png
Terminal=false
Categories=Utility;Office;
Keywords=gestion;outil;
```

**Installation** :
```bash
# Système (tous les utilisateurs)
sudo cp monapp.desktop /usr/share/applications/

# Utilisateur local
cp monapp.desktop ~/.local/share/applications/
```

### Icône d'application

**Formats recommandés** :
- PNG : 16x16, 32x32, 48x48, 128x128, 256x256
- SVG : vectoriel (idéal)

**Installation des icônes** :
```bash
# Icônes système
sudo cp icon-16.png /usr/share/icons/hicolor/16x16/apps/monapp.png
sudo cp icon-32.png /usr/share/icons/hicolor/32x32/apps/monapp.png
sudo cp icon-48.png /usr/share/icons/hicolor/48x48/apps/monapp.png
sudo cp icon-256.png /usr/share/icons/hicolor/256x256/apps/monapp.png

# Mettre à jour le cache d'icônes
sudo gtk-update-icon-cache /usr/share/icons/hicolor/
```

### Notifications desktop

```pascal
{$IFDEF LINUX}
procedure EnvoyerNotification(const Titre, Message: string);
var
  Commande: string;
begin
  // Utiliser notify-send pour les notifications
  Commande := Format('notify-send "%s" "%s"', [Titre, Message]);
  system(PAnsiChar(AnsiString(Commande)));
end;

// Notification avec icône
procedure NotificationAvecIcone(const Titre, Message, Icone: string);
var
  Commande: string;
begin
  Commande := Format('notify-send -i "%s" "%s" "%s"', [Icone, Titre, Message]);
  system(PAnsiChar(AnsiString(Commande)));
end;

// Exemples
procedure TForm1.EnvoyerNotifications;
begin
  EnvoyerNotification('Information', 'Traitement terminé avec succès');
  NotificationAvecIcone('Erreur', 'Une erreur est survenue', 'dialog-error');
end;
{$ENDIF}
```

## 9. Débogage sur Linux

### Déboguer à distance

Delphi permet de déboguer votre application Linux directement depuis l'IDE Windows :

**Configurer le débogage** :
1. Dans Delphi, placer des points d'arrêt dans le code
2. **Run → Run (F9)**
3. L'application démarre sur Linux
4. Le débogueur se connecte via PAServer

**Fonctionnalités disponibles** :
- Points d'arrêt
- Pas à pas (F7, F8)
- Inspection des variables
- Pile d'appels
- Évaluation d'expressions

### Logs et diagnostics

```pascal
{$IFDEF LINUX}
procedure EcrireLog(const Message: string);
var
  Fichier: TextFile;
  CheminLog: string;
begin
  CheminLog := TPath.Combine(
    TPath.GetHomePath,
    '.local', 'share', 'MonApp', 'app.log'
  );

  // Créer le répertoire si nécessaire
  ForceDirectories(TPath.GetDirectoryName(CheminLog));

  AssignFile(Fichier, CheminLog);
  if FileExists(CheminLog) then
    Append(Fichier)
  else
    Rewrite(Fichier);

  try
    WriteLn(Fichier, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) +
                     ' - ' + Message);
  finally
    CloseFile(Fichier);
  end;
end;
{$ENDIF}
```

### Informations système

```pascal
{$IFDEF LINUX}
uses
  Posix.SysUtsname;

function ObtenirInfosSysteme: string;
var
  UtsName: utsname;
begin
  if uname(UtsName) = 0 then
  begin
    Result := Format(
      'Système : %s'#13#10 +
      'Node : %s'#13#10 +
      'Release : %s'#13#10 +
      'Version : %s'#13#10 +
      'Machine : %s',
      [
        string(UtsName.sysname),
        string(UtsName.nodename),
        string(UtsName.release),
        string(UtsName.version),
        string(UtsName.machine)
      ]
    );
  end
  else
    Result := 'Impossible d''obtenir les infos système';
end;

procedure TForm1.AfficherInfosSysteme;
begin
  ShowMessage(ObtenirInfosSysteme);
end;
{$ENDIF}
```

## 10. Limitations et considérations

### Ce qui fonctionne parfaitement

✅ **Composants FireMonkey standards**
✅ **FireDAC** (accès bases de données)
✅ **REST/HTTP** (communication réseau)
✅ **Fichiers et flux**
✅ **Threads et parallélisme**
✅ **Animations et effets**
✅ **JSON, XML**

### Limitations connues

⚠️ **Pas de WebBrowser natif** : TWebBrowser n'est pas disponible sur Linux
- Solution : Utiliser un composant tiers ou appeler un navigateur externe

⚠️ **Services système** : Créer des daemons Linux nécessite du code natif spécifique

⚠️ **Certaines API Windows** : Évidemment, les API Windows spécifiques ne fonctionnent pas
- Solution : Code conditionnel avec {$IFDEF LINUX}

### Alternatives et solutions

**Pour WebBrowser** :
```pascal
{$IFDEF LINUX}
procedure OuvrirURL(const URL: string);
begin
  // Ouvrir dans le navigateur par défaut
  system(PAnsiChar(AnsiString('xdg-open ' + URL)));
end;
{$ELSE}
procedure OuvrirURL(const URL: string);
begin
  WebBrowser1.Navigate(URL);
end;
{$ENDIF}
```

## 11. Bonnes pratiques FMXLinux

### ✅ À FAIRE

**1. Tester sur plusieurs distributions**
```pascal
// Testez au minimum sur :
// - Ubuntu (Debian)
// - Fedora (Red Hat)
// Pour couvrir les deux grandes familles
```

**2. Utiliser TPath pour les chemins**
```pascal
// ✅ BON : Portable
CheminFichier := TPath.Combine(TPath.GetDocumentsPath, 'data.txt');

// ❌ MAUVAIS : Spécifique Windows
CheminFichier := 'C:\Users\user\Documents\data.txt';
```

**3. Gérer les permissions**
```pascal
{$IFDEF LINUX}
  // Vérifier les permissions avant d'écrire
  if DirectoryExists(CheminDonnees) then
    SauvegarderDonnees
  else
    ForceDirectories(CheminDonnees);
{$ENDIF}
```

**4. Fournir plusieurs formats de distribution**
```pascal
// Distribuer en :
// - .deb (Ubuntu/Debian)
// - .rpm (Fedora/RHEL)
// - AppImage (universel)
// - Flatpak (moderne)
```

**5. Respecter les conventions Linux**
```pascal
{$IFDEF LINUX}
  // Configuration : ~/.config/MonApp/
  // Données : ~/.local/share/MonApp/
  // Cache : ~/.cache/MonApp/
  // Logs : ~/.local/share/MonApp/logs/
{$ENDIF}
```

**6. Prévoir une interface en ligne de commande**
```pascal
{$IFDEF LINUX}
// Les utilisateurs Linux apprécient les options CLI
if ParamCount > 0 then
begin
  if ParamStr(1) = '--version' then
    WriteLn('MonApp version 1.0')
  else if ParamStr(1) = '--help' then
    AfficherAide;
end;
{$ENDIF}
```

### ❌ À ÉVITER

**1. Chemins en dur Windows**
```pascal
// ❌ MAUVAIS
Fichier := 'C:\Program Files\MonApp\config.ini';
```

**2. Supposer l'utilisateur root**
```pascal
// ❌ MAUVAIS : Écrire dans /etc/ ou /usr/
// Les apps utilisateur ne doivent PAS nécessiter root
```

**3. Interface uniquement graphique**
```pascal
// ❌ MAUVAIS : Pas d'option ligne de commande
// Sur Linux, beaucoup d'utilisateurs utilisent le terminal
```

**4. Ignorer les fichiers cachés**
```pascal
// ❌ MAUVAIS : Oublier que .config est un répertoire caché
// Les utilisateurs ne le verront pas facilement
```

## 12. Ressources et communauté

### Documentation officielle

**Embarcadero DocWiki** :
- FMXLinux Guide
- Linux Application Development
- PAServer Documentation

### Communauté

**Forums** :
- Embarcadero Forums (section FMXLinux)
- Delphi-PRAXiS
- Stack Overflow (tag: delphi-fmx)

**Groupes** :
- LinkedIn Delphi Groups
- Reddit r/delphi
- Telegram Delphi channels

### Exemples de projets

**Sur GitHub** :
- Rechercher "fmxlinux"
- Exemples officiels Embarcadero
- Projets open source utilisant FMXLinux

## Conclusion

FMXLinux complète l'écosystème multi-plateforme de FireMonkey en apportant Linux dans l'équation. Les points clés à retenir :

🐧 **Même code** : Votre application FireMonkey fonctionne sur Linux sans réécriture

🐧 **GTK3 natif** : Apparence cohérente avec le système Linux

🐧 **PAServer** : Permet développement depuis Windows

🐧 **Multiple distributions** : Fonctionne sur Ubuntu, Fedora, Debian, etc.

🐧 **Déploiement flexible** : .deb, .rpm, AppImage, Flatpak

🐧 **Performance native** : Compilé, pas interprété

🐧 **Intégration système** : Notifications, menus, icônes

🐧 **Débogage distant** : Debug depuis Delphi sur Windows

Avec FMXLinux, vous pouvez maintenant créer des applications qui fonctionnent sur Windows, macOS, iOS, Android **ET** Linux, couvrant ainsi pratiquement tous les systèmes d'exploitation modernes avec un seul code source Delphi. C'est le véritable "Write Once, Compile Anywhere" pour les développeurs Delphi.

⏭️ [Applications graphiques Linux avec FireMonkey](/05-developpement-multi-plateforme-avec-firemonkey/10.1-applications-graphiques-linux-avec-firemonkey.md)
