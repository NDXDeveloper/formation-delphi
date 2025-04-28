# 5.10 FMXLinux : développement pour Linux

Le support Linux pour FireMonkey, souvent appelé FMXLinux, est une fonctionnalité introduite dans Delphi 11 Alexandria qui permet aux développeurs d'étendre leurs applications FireMonkey au monde Linux. Cette section vous guidera à travers les bases du développement d'applications FireMonkey pour Linux, depuis la configuration jusqu'au déploiement.

> **Note :** FMXLinux nécessite Delphi 11 Alexandria ou supérieur.

## Introduction à FMXLinux

FMXLinux permet de porter vos applications FireMonkey existantes vers Linux avec un minimum de modifications. Grâce à cette technologie, vous pouvez maintenant cibler les principales plateformes desktop avec une base de code commune :
- Windows
- macOS
- Linux

FMXLinux repose sur GTK3 (GIMP Toolkit version 3), une bibliothèque graphique multiplateforme populaire sous Linux, pour afficher l'interface utilisateur de vos applications.

## Prérequis pour le développement Linux

Avant de commencer le développement pour Linux, assurez-vous de disposer des éléments suivants :

1. **Delphi 11 Alexandria ou supérieur** : Version minimale requise pour le support FMXLinux

2. **Système Linux pour tester** : Bien que vous puissiez développer sous Windows, vous aurez besoin d'un système Linux pour tester vos applications. Options possibles :
   - Machine Linux physique
   - Machine virtuelle (comme VirtualBox ou VMware)
   - Sous-système Windows pour Linux (WSL2) avec environnement graphique

3. **Dépendances Linux requises** : Sur votre système Linux de test, installez les dépendances nécessaires :

```bash
# Pour Ubuntu/Debian
sudo apt-get update
sudo apt-get install libgtk-3-0 libgtk-3-dev

# Pour Fedora/Red Hat
sudo dnf install gtk3 gtk3-devel
```

## Configuration d'un projet FMXLinux

### Créer un nouveau projet Linux

Pour créer un nouveau projet compatible Linux :

1. Lancez Delphi et sélectionnez **Fichier > Nouveau > Application multi-périphériques**
2. Vous obtiendrez un formulaire FireMonkey vide
3. Allez dans **Projet > Options du Projet > Plateformes cibles**
4. Activez la plateforme **Linux 64 bits**

![Options de plateforme](https://placehold.co/400x300)

### Configurer un projet existant pour Linux

Pour ajouter le support Linux à un projet FireMonkey existant :

1. Ouvrez votre projet FireMonkey
2. Allez dans **Projet > Options du Projet > Plateformes cibles**
3. Activez la plateforme **Linux 64 bits**
4. Vérifiez votre code pour les problèmes de compatibilité (voir plus bas)

## Spécificités du développement pour Linux

### Détection conditionnelle de la plateforme

Pour adapter votre code aux spécificités de Linux, utilisez les directives de compilation :

```pascal
{$IFDEF LINUX}
  // Code spécifique à Linux
{$ENDIF}

{$IF Defined(MSWINDOWS)}
  // Code pour Windows
{$ELSEIF Defined(MACOS)}
  // Code pour macOS
{$ELSEIF Defined(LINUX)}
  // Code pour Linux
{$ENDIF}
```

### Accès aux fonctionnalités Linux spécifiques

Pour accéder aux fonctionnalités spécifiques de Linux, vous pouvez utiliser l'unité `Posix.Unistd` :

```pascal
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  {$IFDEF LINUX}
  Posix.Unistd, Posix.Stdlib,
  {$ENDIF}
  // Autres unités...
  ;

procedure TForm1.ExecuterCommandeLinux;
{$IFDEF LINUX}
var
  ResultCode: Integer;
begin
  // Exécuter une commande shell Linux
  ResultCode := System(_PAnsiChar('xdg-open https://www.embarcadero.com'));
  if ResultCode <> 0 then
    ShowMessage('Erreur lors de l''exécution de la commande');
end;
{$ELSE}
begin
  ShowMessage('Cette fonctionnalité est disponible uniquement sous Linux');
end;
{$ENDIF}
```

### Chemins de fichiers

Les chemins de fichiers sous Linux utilisent des barres obliques (`/`) au lieu des barres obliques inversées (`\`) utilisées sous Windows. Utilisez les fonctions de l'unité `System.IOUtils` pour gérer les chemins de manière multiplateforme :

```pascal
uses
  System.IOUtils;

procedure TForm1.GestionCheminsFichiers;
var
  CheminDocument: string;
begin
  // Obtenir un chemin compatible avec la plateforme actuelle
  CheminDocument := TPath.Combine(TPath.GetDocumentsPath, 'monfichier.txt');

  // Afficher le chemin
  Memo1.Lines.Add('Chemin du document : ' + CheminDocument);

  // Vérifier si le chemin existe
  if TFile.Exists(CheminDocument) then
    Memo1.Lines.Add('Le fichier existe');
end;
```

## Interface utilisateur sous Linux

### Styles et thèmes GTK

Sous Linux, FireMonkey utilise GTK3 comme backend d'interface utilisateur. Par défaut, votre application adoptera le thème GTK du système, ce qui lui donnera un aspect natif.

Vous pouvez également appliquer un style FireMonkey spécifique :

```pascal
procedure TForm1.AppliquerStyleLinux;
begin
  {$IFDEF LINUX}
  // Utiliser un style spécifique sous Linux
  TStyleManager.TrySetStyleFromResource('Windows10');
  {$ENDIF}
end;
```

### Adaptations spécifiques pour Linux

Certains éléments d'interface peuvent nécessiter des ajustements pour offrir une meilleure expérience sous Linux :

```pascal
procedure TForm1.AdapterInterfaceLinux;
begin
  {$IFDEF LINUX}
  // Augmenter légèrement la taille des contrôles pour GTK
  Button1.Height := Button1.Height + 4;
  Edit1.Height := Edit1.Height + 4;

  // Ajuster les marges pour un meilleur rendu sous GTK
  Panel1.Margins.Top := 8;
  Panel1.Margins.Bottom := 8;
  {$ENDIF}
end;
```

### Menus et raccourcis clavier

Les conventions de menu diffèrent légèrement sous Linux. Pour une meilleure expérience utilisateur, adaptez vos menus selon la plateforme :

```pascal
procedure TForm1.ConfigurerMenus;
begin
  // Menu commun à toutes les plateformes
  MainMenu1.Items[0].Text := 'Fichier';

  {$IFDEF LINUX}
  // Sous Linux, "Préférences" est plus courant que "Options"
  MenuItemOptions.Text := 'Préférences';

  // Raccourcis clavier Linux style (utilisant Ctrl)
  MenuItemCouper.ShortCut := TextToShortCut('Ctrl+X');
  MenuItemCopier.ShortCut := TextToShortCut('Ctrl+C');
  MenuItemColler.ShortCut := TextToShortCut('Ctrl+V');
  {$ENDIF}
end;
```

## Gestion des différences entre les plateformes

### Boîtes de dialogue de fichiers

Les boîtes de dialogue de fichiers sont gérées différemment selon la plateforme. FireMonkey s'occupe de ces différences, mais vous devrez peut-être adapter certains paramètres :

```pascal
procedure TForm1.OuvrirFichier;
var
  Dialog: TOpenDialog;
begin
  Dialog := TOpenDialog.Create(nil);
  try
    Dialog.Title := 'Sélectionner un fichier';
    Dialog.Filter := 'Tous les fichiers|*.*|Documents texte|*.txt';

    {$IFDEF LINUX}
    // Sous Linux, définir le répertoire initial vers le dossier personnel
    Dialog.InitialDir := GetEnvironmentVariable('HOME');
    {$ELSE}
    Dialog.InitialDir := TPath.GetDocumentsPath;
    {$ENDIF}

    if Dialog.Execute then
      TraiterFichier(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;
```

### Fonctionnalités du presse-papiers

Le presse-papiers fonctionne généralement de manière cohérente entre les plateformes, mais certaines fonctionnalités avancées peuvent nécessiter des adaptations :

```pascal
procedure TForm1.CopierTexteDansPressepaiers;
begin
  Clipboard.AsText := Edit1.Text;

  {$IFDEF LINUX}
  // Sur certaines distributions Linux, il peut être nécessaire
  // d'attendre un court instant pour que le presse-papiers soit mis à jour
  Sleep(50);
  {$ENDIF}
end;
```

## Déploiement sous Linux

### Compilation pour Linux

Pour compiler votre application pour Linux :

1. Sélectionnez **Linux 64 bits** comme plateforme cible dans la barre d'outils
2. Compilez votre projet (**Maj+F9** ou **Projet > Compiler**)

Le fichier exécutable résultant sera créé dans le dossier de sortie Linux de votre projet.

### Fichiers requis pour l'exécution

Pour que votre application s'exécute sous Linux, vous devrez déployer les fichiers suivants :

1. Votre fichier exécutable principal
2. Les bibliothèques RTL et FireMonkey (généralement situées dans le dossier d'installation de Delphi sous `bin/linuxx64/`)

Voici un exemple de structure de déploiement :

```
MonApplication/
  ├── MonApplication          # Votre exécutable
  ├── libfmx.so               # Bibliothèque FireMonkey
  ├── librtl.so               # Bibliothèque RTL
  └── [autres bibliothèques .so requises]
```

### Script de déploiement

Voici un exemple de script shell que vous pouvez utiliser pour déployer votre application Linux :

```bash
#!/bin/bash

# Chemin vers votre application compilée
APP_NAME="MonApplication"
DELPHI_PATH="/chemin/vers/delphi"
LIBS_PATH="$DELPHI_PATH/bin/linuxx64"
OUTPUT_DIR="./deploy"

# Créer le répertoire de déploiement
mkdir -p $OUTPUT_DIR

# Copier l'exécutable
cp bin/linux64/Release/$APP_NAME $OUTPUT_DIR/

# Copier les bibliothèques requises
cp $LIBS_PATH/librtl.so $OUTPUT_DIR/
cp $LIBS_PATH/libfmx.so $OUTPUT_DIR/

# Ajouter d'autres bibliothèques selon les besoins
# cp $LIBS_PATH/libfmxlinux.so $OUTPUT_DIR/
# cp $LIBS_PATH/libRESTComponents.so $OUTPUT_DIR/ # Si vous utilisez REST

echo "Déploiement terminé dans $OUTPUT_DIR"
```

### Création d'un paquet Debian (.deb)

Pour une distribution plus professionnelle, vous pouvez créer un paquet Debian :

1. Créez un répertoire de structure Debian :

```bash
mkdir -p myapp/DEBIAN
mkdir -p myapp/usr/bin
mkdir -p myapp/usr/share/applications
mkdir -p myapp/usr/share/pixmaps
```

2. Créez un fichier de contrôle `myapp/DEBIAN/control` :

```
Package: monapplication
Version: 1.0
Section: utils
Priority: optional
Architecture: amd64
Depends: libgtk-3-0 (>= 3.20.0)
Maintainer: Votre Nom <votre.email@exemple.com>
Description: Mon Application Delphi
 Une application développée avec Delphi et FireMonkey.
 Cette application offre les fonctionnalités suivantes...
```

3. Copiez vos fichiers dans la structure :

```bash
cp MonApplication myapp/usr/bin/
cp libfmx.so librtl.so myapp/usr/bin/
cp icon.png myapp/usr/share/pixmaps/monapplication.png
```

4. Créez un fichier `.desktop` pour l'intégration au menu :

```
[Desktop Entry]
Name=Mon Application
Comment=Description de mon application
Exec=/usr/bin/MonApplication
Icon=monapplication
Terminal=false
Type=Application
Categories=Utility;
```

5. Créez le paquet Debian :

```bash
dpkg-deb --build myapp
```

Le résultat sera un fichier `myapp.deb` que les utilisateurs pourront installer avec la commande `sudo dpkg -i myapp.deb`.

## Dépannage des problèmes courants sous Linux

### Problèmes de bibliothèques manquantes

Si votre application ne démarre pas en raison de bibliothèques manquantes, utilisez la commande `ldd` pour identifier les dépendances :

```bash
ldd ./MonApplication
```

Installez ensuite les bibliothèques manquantes :

```bash
sudo apt-get install libgtk-3-0 libgtk-3-dev
```

### Problèmes de permissions

Si vous rencontrez des problèmes de permissions :

```bash
# Rendre votre application exécutable
chmod +x ./MonApplication

# Rendre les bibliothèques accessibles
chmod +x *.so
```

### Problèmes d'affichage

Si l'interface utilisateur s'affiche incorrectement :

1. Vérifiez que GTK3 est correctement installé
2. Testez avec différents thèmes GTK
3. Vérifiez les variables d'environnement d'affichage :

```bash
export GTK_DEBUG=interactive
./MonApplication
```

## Exemple complet : Application multi-plateforme

Voici un exemple d'application simple qui s'adapte aux différentes plateformes, y compris Linux :

```pascal
unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.ListBox, FMX.Memo,
  FMX.Memo.Types, FMX.ScrollBox,
  {$IFDEF LINUX}
  Posix.Unistd, Posix.Stdlib,
  {$ENDIF}
  System.IOUtils;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Button1: TButton;
    Edit1: TEdit;
    Memo1: TMemo;
    ListBox1: TListBox;
    StatusBar1: TStatusBar;
    StatusLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure DetecterSystemeExploitation;
    procedure AjusterInterfaceSelonPlateforme;
    function ObtenirInfoSysteme: string;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DetecterSystemeExploitation;
  AjusterInterfaceSelonPlateforme;

  // Remplir la liste avec des éléments de test
  ListBox1.Items.Add('Élément 1');
  ListBox1.Items.Add('Élément 2');
  ListBox1.Items.Add('Élément 3');

  // Afficher le chemin du répertoire utilisateur
  Memo1.Lines.Add('Répertoire documents : ' + TPath.GetDocumentsPath);
  Memo1.Lines.Add('Répertoire temporaire : ' + TPath.GetTempPath);
end;

procedure TMainForm.DetecterSystemeExploitation;
begin
  {$IF Defined(MSWINDOWS)}
    Caption := 'Application Windows';
    StatusLabel.Text := 'Exécution sous Windows';
  {$ELSEIF Defined(MACOS)}
    Caption := 'Application macOS';
    StatusLabel.Text := 'Exécution sous macOS';
  {$ELSEIF Defined(LINUX)}
    Caption := 'Application Linux';
    StatusLabel.Text := 'Exécution sous Linux';
  {$ELSE}
    Caption := 'Application multi-plateforme';
    StatusLabel.Text := 'Plateforme inconnue';
  {$ENDIF}
end;

procedure TMainForm.AjusterInterfaceSelonPlateforme;
begin
  {$IFDEF MSWINDOWS}
    // Ajustements pour Windows
    Button1.TextSettings.Font.Size := 10;
  {$ENDIF}

  {$IFDEF MACOS}
    // Ajustements pour macOS
    Button1.TextSettings.Font.Size := 12;
    ToolBar1.Height := 40;
  {$ENDIF}

  {$IFDEF LINUX}
    // Ajustements pour Linux
    Button1.TextSettings.Font.Size := 11;
    Button1.Height := Button1.Height + 4;
    Edit1.Height := Edit1.Height + 4;
    Label1.TextSettings.Font.Size := 12;
  {$ENDIF}
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Texte saisi : ' + Edit1.Text);
  Memo1.Lines.Add('Informations système : ');
  Memo1.Lines.Add(ObtenirInfoSysteme);
  Edit1.Text := '';
end;

function TMainForm.ObtenirInfoSysteme: string;
var
  ResultText: string;
begin
  ResultText := 'OS : ';

  {$IF Defined(MSWINDOWS)}
    ResultText := ResultText + 'Windows ' + TOSVersion.Major.ToString + '.' +
                  TOSVersion.Minor.ToString;
  {$ELSEIF Defined(MACOS)}
    ResultText := ResultText + 'macOS ' + TOSVersion.Major.ToString + '.' +
                  TOSVersion.Minor.ToString;
  {$ELSEIF Defined(LINUX)}
    // Sous Linux, on peut obtenir plus d'informations sur la distribution
    {$IFDEF LINUX}
    var FileName := '/etc/os-release';
    if TFile.Exists(FileName) then
    begin
      var Content := TFile.ReadAllText(FileName);
      var Lines := Content.Split([#10]);
      for var Line in Lines do
      begin
        if Line.StartsWith('PRETTY_NAME=') then
        begin
          var Name := Line.Replace('PRETTY_NAME=', '').Replace('"', '');
          ResultText := ResultText + Name;
          Break;
        end;
      end;
    end
    else
      ResultText := ResultText + 'Linux (distribution inconnue)';
    {$ENDIF}
  {$ELSE}
    ResultText := ResultText + 'Inconnu';
  {$ENDIF}

  // Ajouter des informations sur le système
  ResultText := ResultText + #13#10 + 'Architecture : ' + TOSVersion.Architecture;
  ResultText := ResultText + #13#10 + 'Version : ' + TOSVersion.ToString;

  Result := ResultText;
end;

end.
```

## Recommandations pour le développement Linux

1. **Testez régulièrement** : Ne développez pas toute votre application sous Windows pour ensuite découvrir des problèmes sous Linux. Testez fréquemment.

2. **Utiliser des chemins relatifs** : Évitez les chemins codés en dur spécifiques à une plateforme.

3. **Respectez les conventions Linux** : Les utilisateurs Linux s'attendent à certaines conventions d'interface (menus, raccourcis clavier, etc.).

4. **Gestion des permissions** : Sous Linux, la gestion des permissions de fichiers est plus stricte. Assurez-vous que votre application demande les permissions appropriées.

5. **Préférez les API multiplateformes** : Utilisez autant que possible les API FireMonkey multiplateformes plutôt que des APIs spécifiques à une plateforme.

6. **Documentation sur le déploiement** : Fournissez des instructions claires pour l'installation sous Linux, y compris les dépendances requises.

## Conclusion

FMXLinux permet d'étendre la portée de vos applications FireMonkey au monde Linux avec relativement peu d'efforts. En comprenant les différences entre les plateformes et en adaptant votre code et votre interface utilisateur en conséquence, vous pouvez offrir une expérience utilisateur cohérente et native sur toutes les plateformes desktop.

L'ajout du support Linux à vos applications FireMonkey existantes ouvre de nouvelles possibilités et marchés, particulièrement dans les environnements professionnels et éducatifs où Linux est largement utilisé.

En suivant les bonnes pratiques présentées dans cette section, vous serez bien équipé pour développer, déployer et maintenir des applications FireMonkey multi-plateformes qui fonctionnent parfaitement sous Linux, Windows et macOS.
