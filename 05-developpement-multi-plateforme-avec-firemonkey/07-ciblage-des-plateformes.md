# 5.7 Ciblage des plateformes : Windows, macOS, iOS, Android, Linux

L'un des principaux avantages de FireMonkey est sa capacité à cibler plusieurs plateformes à partir d'une base de code unique. Dans cette section, nous allons explorer comment configurer, adapter et déployer votre application pour chaque plateforme prise en charge : Windows, macOS, iOS, Android et Linux.

## Configuration du projet multi-plateforme

Pour commencer, vous devez configurer votre projet pour cibler les plateformes souhaitées.

### Activer les plateformes cibles

1. Ouvrez ou créez un projet FireMonkey
2. Allez dans **Projet > Options du Projet**
3. Sélectionnez **Plateformes cibles** dans le menu de gauche
4. Cochez les plateformes que vous souhaitez cibler

![Options de plateforme](https://placehold.co/400x300)

Par défaut, seule la plateforme Windows est activée. Vous devez activer manuellement les autres plateformes.

### Configuration requise pour chaque plateforme

Chaque plateforme nécessite certains outils et configurations :

#### Windows
- Aucune configuration supplémentaire requise
- Prise en charge native dans Delphi

#### macOS
- Un Mac physique ou une machine virtuelle macOS
- Xcode installé sur le Mac
- Delphi installé sous Windows avec connexion au Mac via "PAServer"

#### iOS
- Un Mac avec Xcode
- Compte développeur Apple (payant) pour déployer sur des appareils réels
- Certificats de développement configurés

#### Android
- Android SDK
- Java Development Kit (JDK)
- Configurer les chemins dans Delphi via **Outils > Options > SDK Manager**

#### Linux
> **Note :** Nécessite Delphi 11 Alexandria ou supérieur.
- Bibliothèques GTK3 sur le système Linux cible
- Aucun SDK supplémentaire n'est requis pour la compilation

## Détection de la plateforme dans le code

Pour adapter votre application à chaque plateforme, vous pouvez détecter la plateforme actuelle à la compilation ou à l'exécution.

### Détection à la compilation avec des directives conditionnelles

Utilisez des directives de compilation pour inclure ou exclure du code selon la plateforme :

```pascal
{$IFDEF MSWINDOWS}
  // Code spécifique à Windows
{$ENDIF}

{$IFDEF MACOS}
  // Code spécifique à macOS
  {$IFDEF IOS}
    // Code spécifique à iOS (sous-ensemble de MACOS)
  {$ELSE}
    // Code spécifique à macOS (desktop)
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  // Code spécifique à Android
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique à Linux
{$ENDIF}
```

Exemple concret d'utilisation :

```pascal
procedure TForm1.ConfigurerBoutonRetour;
begin
{$IFDEF ANDROID}
  // Sur Android, utiliser le bouton hardware Back
  KeyboardAddEventListener(HandleBackButton);
{$ENDIF}

{$IFDEF IOS}
  // Sur iOS, ajouter un bouton "Retour" explicite
  BackButton.Visible := True;
{$ENDIF}

{$IF DEFINED(MSWINDOWS) OR DEFINED(MACOS) OR DEFINED(LINUX)}
  // Sur desktop, ajouter un bouton standard
  BackButton.Visible := True;
  BackButton.Width := 100;  // Plus large pour la souris
{$ENDIF}
end;
```

### Détection à l'exécution

Pour certains cas, vous préférerez peut-être détecter la plateforme à l'exécution :

```pascal
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Platform;

procedure TForm1.FormCreate(Sender: TObject);
begin
  if TOSVersion.Platform = TOSVersion.TPlatform.pfWindows then
    // Code spécifique à Windows
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfMacOS then
    // Code spécifique à macOS
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfiOS then
    // Code spécifique à iOS
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfAndroid then
    // Code spécifique à Android
  else if TOSVersion.Platform = TOSVersion.TPlatform.pfLinux then
    // Code spécifique à Linux
end;
```

## Adaptations spécifiques à chaque plateforme

Voici les principales adaptations à considérer pour chaque plateforme :

### Windows

Windows est généralement la plateforme la plus simple à cibler, car Delphi y est natif.

```pascal
{$IFDEF MSWINDOWS}
// Accéder à certaines fonctionnalités Windows spécifiques
uses
  Winapi.Windows, Winapi.Messages;

procedure TForm1.UtiliserFonctionnalitesWindows;
var
  WindowHandle: HWND;
begin
  // Exemple : accéder au handle de la fenêtre
  WindowHandle := FmxHandleToHWND(Self.Handle);

  // Utiliser des API Windows
  SetWindowText(WindowHandle, 'Titre modifié via API Windows');
end;
{$ENDIF}
```

#### Particularités Windows :
- Supporte à la fois la souris et le tactile
- Adapte automatiquement l'affichage selon la résolution
- Peut utiliser les API Windows natives si nécessaire

### macOS

Pour macOS, vous devez tenir compte de certaines spécificités :

```pascal
{$IFDEF MACOS}
{$IF not defined(IOS)} // Vérifier qu'on est bien sur macOS (desktop)
uses
  Macapi.Foundation, Macapi.AppKit;

procedure TForm1.UtiliserFonctionnalitesMacOS;
var
  Pool: NSAutoreleasePool;
  FileDialog: NSOpenPanel;
begin
  // Exemple : utiliser un dialogue de fichier natif macOS
  Pool := TNSAutoreleasePool.Create;
  try
    FileDialog := TNSOpenPanel.Create;
    FileDialog.setCanChooseFiles(True);
    FileDialog.setAllowsMultipleSelection(False);

    if FileDialog.runModal = NSModalResponseOK then
    begin
      var URL := TNSUrl.Wrap(FileDialog.URLs.objectAtIndex(0));
      ShowMessage('Fichier sélectionné : ' + URL.path.UTF8String);
    end;
  finally
    Pool.release;
  end;
end;
{$ENDIF}
{$ENDIF}
```

#### Particularités macOS :
- Interface utilisateur qui respecte les conventions macOS
- Support de la barre de menu en haut de l'écran
- Support du trackpad et des gestes

### iOS

iOS a des contraintes strictes en matière d'interface et d'interaction :

```pascal
{$IFDEF IOS}
uses
  iOSapi.Foundation, iOSapi.UIKit;

procedure TForm1.UtiliserFonctionnalitesiOS;
begin
  // Exemple : accéder au statut de la batterie
  UIDevice.currentDevice.setBatteryMonitoringEnabled(True);
  var BatteryLevel := UIDevice.currentDevice.batteryLevel;
  ShowMessage('Niveau de batterie : ' + FloatToStr(BatteryLevel * 100) + '%');

  // Exemple : vibration
  AudioServicesPlaySystemSound(kSystemSoundID_Vibrate);
end;
{$ENDIF}
```

#### Particularités iOS :
- Doit respecter les directives d'interface Apple
- Pas de menus contextuels classiques
- Gestion des permissions stricte
- Navigation par gestes et boutons spécifiques

### Android

Android a sa propre approche de l'interface et des interactions :

```pascal
{$IFDEF ANDROID}
uses
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Toast;

procedure TForm1.UtiliserFonctionnalitesAndroid;
begin
  // Exemple : afficher un toast Android
  ToastLength := TJToast.JavaClass.LENGTH_SHORT;
  CallInUIThread(
    procedure
    begin
      TJToast.JavaClass.makeText(
        SharedActivityContext,
        StrToJCharSequence('Message Android natif'),
        ToastLength
      ).show;
    end
  );
end;
{$ENDIF}
```

#### Particularités Android :
- Navigation avec bouton "Back" hardware
- Menus options en bas de l'écran
- Notifications spécifiques
- Permissions à demander explicitement à l'exécution

### Linux

Le support Linux est plus récent dans FireMonkey et nécessite Delphi 11 ou supérieur :

```pascal
{$IFDEF LINUX}
uses
  Posix.Stdlib;

procedure TForm1.UtiliserFonctionnalitesLinux;
begin
  // Exemple : exécuter une commande shell Linux
  system(PAnsiChar('xdg-open https://www.embarcadero.com'));
end;
{$ENDIF}
```

#### Particularités Linux :
- Interface basée sur GTK3
- Tendance à suivre les conventions de bureau GNOME
- Moins optimisé que les autres plateformes (support plus récent)

## Fonctionnalités communes avec implémentations spécifiques

Certaines fonctionnalités nécessitent une implémentation différente selon la plateforme, même si le comportement final est similaire.

### Partage de contenu

```pascal
procedure TForm1.PartagerTexte(const Texte, Sujet: string);
begin
{$IFDEF MSWINDOWS}
  // Sous Windows, copier dans le presse-papiers est souvent la méthode de partage la plus simple
  Clipboard.SetTextBuf(PChar(Texte));
  ShowMessage('Texte copié dans le presse-papiers.');
{$ENDIF}

{$IFDEF ANDROID}
  // Utiliser l'Intent de partage Android
  var Intent := TJIntent.Create;
  Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
  Intent.putExtra(TJIntent.JavaClass.EXTRA_SUBJECT, StringToJString(Sujet));
  Intent.putExtra(TJIntent.JavaClass.EXTRA_TEXT, StringToJString(Texte));
  Intent.setType(StringToJString('text/plain'));
  SharedActivity.startActivity(TJIntent.JavaClass.createChooser(Intent, StringToJString('Partager via')));
{$ENDIF}

{$IFDEF IOS}
  // Utiliser UIActivityViewController pour le partage iOS
  var ActivityItems := TNSArray.Create;
  ActivityItems.addObject((TNSString.OCClass.stringWithString(StrToNSStr(Texte))));

  var ActivityController := TUIActivityViewController.Alloc;
  ActivityController.initWithActivityItems(ActivityItems, nil);

  var RootView := SharedApplication.keyWindow.rootViewController;
  RootView.presentViewController(ActivityController, True, nil);
{$ENDIF}

{$IF DEFINED(MACOS) and not DEFINED(IOS)}
  // Sous macOS, un dialogue de partage natif
  var Service := TNSService.Create;
  Service.initWithName(NSStr('com.apple.share'));
  Service.performWithItems(TNSArray.OCClass.arrayWithObject(NSStr(Texte)));
{$ENDIF}

{$IFDEF LINUX}
  // Sous Linux, via presse-papiers ou commande externe
  Clipboard.SetTextBuf(PChar(Texte));
  ShowMessage('Texte copié dans le presse-papiers.');
{$ENDIF}
end;
```

### Stockage de données locales

```pascal
function TForm1.ObtenirCheminStockageLocal: string;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) +
            'MonApplication';
{$ELSEIF DEFINED(MACOS) and not DEFINED(IOS)}
  Result := IncludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    'Library/Application Support/MonApplication');
{$ELSEIF DEFINED(IOS)}
  Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath);
{$ELSEIF DEFINED(ANDROID)}
  Result := IncludeTrailingPathDelimiter(TPath.GetPublicPath);
{$ELSEIF DEFINED(LINUX)}
  Result := IncludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    '.config/MonApplication');
{$ENDIF}

  // Créer le répertoire s'il n'existe pas
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;
```

## Configuration des permissions

Les permissions sont gérées différemment selon les plateformes :

### Android

Pour Android, les permissions doivent être déclarées dans le fichier `AndroidManifest.template.xml` :

1. Ouvrez le projet dans Delphi
2. Allez dans **Projet > Options du Projet > Application**
3. Sélectionnez la plateforme **Android**
4. Cliquez sur **Personnaliser AndroidManifest.xml...**
5. Ajoutez les permissions nécessaires dans la section `<manifest>` :

```xml
<uses-permission android:name="android.permission.INTERNET" />
<uses-permission android:name="android.permission.CAMERA" />
<uses-permission android:name="android.permission.READ_EXTERNAL_STORAGE" />
<uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE" />
```

De plus, sous Android 6.0+, vous devez aussi demander les permissions à l'exécution :

```pascal
{$IFDEF ANDROID}
uses
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Os;

procedure TForm1.DemanderPermissionCamera;
begin
  var Permission := JStringToString(TJManifest_permission.JavaClass.CAMERA);

  // Vérifier si la permission est déjà accordée
  if ContextCompat.checkSelfPermission(TAndroidHelper.Context,
     StringToJString(Permission)) <> TJPackageManager.JavaClass.PERMISSION_GRANTED then
  begin
    // Demander la permission
    var Permissions: TJavaObjectArray<JString>;
    Permissions := TJavaObjectArray<JString>.Create(1);
    Permissions.Items[0] := StringToJString(Permission);

    TAndroidHelper.Activity.requestPermissions(Permissions, 1);
  end
  else
  begin
    // Permission déjà accordée
    OuvrirCamera;
  end;
end;
{$ENDIF}
```

### iOS

Pour iOS, les permissions doivent être déclarées dans le fichier `info.plist` :

1. Ouvrez le projet dans Delphi
2. Allez dans **Projet > Options du Projet > Application**
3. Sélectionnez la plateforme **iOS Device**
4. Cliquez sur **Infos version**
5. Ajoutez les clés de permissions nécessaires, par exemple :

```
NSCameraUsageDescription = Nous avons besoin d'accéder à votre caméra pour scanner des codes QR.
NSPhotoLibraryUsageDescription = Nous avons besoin d'accéder à vos photos pour sauvegarder les images.
```

## Packager et déployer pour chaque plateforme

### Windows

La compilation et le déploiement pour Windows sont les plus simples :

1. Sélectionnez la plateforme **Windows 32-bit** ou **Windows 64-bit**
2. Compilez le projet (**Shift+F9**) ou créez un package (**Projet > Compiler**)
3. Vous pouvez créer un installateur avec des outils comme InnoSetup

### macOS

Pour macOS, vous aurez besoin d'un Mac connecté :

1. Configurez PAServer sur votre Mac
2. Dans Delphi, configurez la connexion au PAServer
3. Sélectionnez la plateforme **macOS**
4. Compilez le projet
5. Pour la distribution, créez un package DMG ou soumettez à l'App Store

### iOS

Pour iOS, le processus est similaire à macOS mais avec des étapes supplémentaires :

1. Configurez PAServer sur votre Mac
2. Configurez votre certificat de développeur et profil de provisionnement
3. Dans Delphi, sélectionnez **iOS Device** ou **iOS Simulator**
4. Compilez et déployez
5. Pour l'App Store, préparez toutes les ressources nécessaires et utilisez Xcode pour la soumission finale

### Android

Pour Android, vous pouvez déployer directement depuis Delphi :

1. Sélectionnez la plateforme **Android**
2. Connectez un appareil Android en mode débogage USB ou configurez un émulateur
3. Compilez et déployez directement sur l'appareil

Pour la publication :
1. Générez un APK signé : **Projet > Déploiement > Créer package d'application...**
2. Créez ou sélectionnez une clé de signature
3. Soumettez l'APK au Google Play Store

### Linux

Pour Linux :

1. Sélectionnez la plateforme **Linux 64-bit**
2. Compilez le projet
3. Transférez l'exécutable et les bibliothèques requises sur un système Linux
4. Assurez-vous que les bibliothèques GTK3 sont installées sur le système cible

## Meilleures pratiques pour le développement multi-plateforme

1. **Commencez par une conception commune** : Concevez d'abord l'architecture et l'interface de base commune à toutes les plateformes

2. **Factoriser le code spécifique** : Isolez le code spécifique à une plateforme dans des unités ou des méthodes séparées

3. **Utilisez des interfaces** : Définissez des interfaces pour les fonctionnalités qui nécessitent une implémentation spécifique à chaque plateforme

4. **Testez régulièrement sur toutes les plateformes** : Ne laissez pas une plateforme sans test trop longtemps pour éviter des surprises

5. **Respectez les conventions de chaque plateforme** : Les utilisateurs s'attendent à une expérience cohérente avec les autres applications de leur plateforme

6. **Utilisez le style adapté à chaque plateforme** : Tirez parti des styles prédéfinis de FireMonkey pour adopter l'apparence native

7. **Évitez les dépendances externes non portables** : Préférez les bibliothèques multi-plateformes ou implémentez des alternatives pour chaque plateforme

## Exemple complet : Application multi-plateforme adaptative

Voici un exemple de structure de base pour une application qui s'adapte à chaque plateforme :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.MultiView, FMX.Edit,
  // Inclure les unités spécifiques à chaque plateforme
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF}
  {$IFDEF ANDROID}
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.Foundation, iOSapi.UIKit,
  {$ENDIF}
  // Autres imports selon les plateformes
  ;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    TitleLabel: TLabel;
    ContentLayout: TLayout;
    MultiView1: TMultiView;
    Button1: TButton;
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure ConfigurerPourPlateforme;
    procedure ConfigurerWindows;
    procedure ConfigurerMacOS;
    procedure ConfigureriOS;
    procedure ConfigurerAndroid;
    procedure ConfigurerLinux;
    function ObtenirCheminStockage: string;
    procedure PartagerTexte(const Texte: string);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Configuration commune
  ToolBar1.Align := TAlignLayout.Top;
  ContentLayout.Align := TAlignLayout.Client;

  // Configuration spécifique à la plateforme
  ConfigurerPourPlateforme;
end;

procedure TMainForm.ConfigurerPourPlateforme;
begin
  {$IFDEF MSWINDOWS}
  ConfigurerWindows;
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
    ConfigureriOS;
    {$ELSE}
    ConfigurerMacOS;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  ConfigurerAndroid;
  {$ENDIF}

  {$IFDEF LINUX}
  ConfigurerLinux;
  {$ENDIF}
end;

procedure TMainForm.ConfigurerWindows;
begin
  // Configuration spécifique à Windows
  Caption := 'Application Windows';

  // Ajuster l'interface pour la souris
  Button1.Height := 30;

  // Utiliser le style Windows
  TStyleManager.TrySetStyleFromResource('Windows');

  // Menu classique au lieu de hamburger
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.MasterButton := nil; // Pas de bouton hamburger

  // Ajouter une barre de menu (exemple simplifié)
  var MenuBar := TToolBar.Create(Self);
  MenuBar.Parent := Self;
  MenuBar.Height := 25;
  MenuBar.Align := TAlignLayout.MostTop;

  var MenuButton := TButton.Create(Self);
  MenuButton.Parent := MenuBar;
  MenuButton.Text := 'Fichier';
  MenuButton.Width := 80;
  MenuButton.Align := TAlignLayout.Left;
end;

procedure TMainForm.ConfigurerMacOS;
begin
  // Configuration spécifique à macOS
  Caption := 'Application macOS';

  // Utiliser le style macOS
  TStyleManager.TrySetStyleFromResource('Aqua Light');

  // Interface optimisée pour trackpad
  Button1.Height := 28;
end;

procedure TMainForm.ConfigureriOS;
begin
  // Configuration spécifique à iOS
  TitleLabel.Text := 'Application iOS';

  // Utiliser le style iOS
  TStyleManager.TrySetStyleFromResource('iOS');

  // Interface tactile
  Button1.Height := 44;

  // Navigation iOS style
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.Placement := TPlacement.Left;
end;

procedure TMainForm.ConfigurerAndroid;
begin
  // Configuration spécifique à Android
  TitleLabel.Text := 'Application Android';

  // Utiliser le style Material Design
  TStyleManager.TrySetStyleFromResource('Android');

  // Interface tactile optimisée
  Button1.Height := 50;

  // Menu hamburger style Android
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.Placement := TPlacement.Left;

  // Gérer le bouton Back d'Android
  // (Code simplifié, normalement dans une unité séparée)
  {$IFDEF ANDROID}
  var KeyboardService: IFMXVirtualKeyboardService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, KeyboardService) then
    KeyboardService.SetShowVirtualKeyboardButtonVisibility(True);
  {$ENDIF}
end;

procedure TMainForm.ConfigurerLinux;
begin
  // Configuration spécifique à Linux
  Caption := 'Application Linux';

  // Utiliser un style compatible
  TStyleManager.TrySetStyleFromResource('Windows');

  // Interface standard
  Button1.Height := 30;
end;

function TMainForm.ObtenirCheminStockage: string;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := IncludeTrailingPathDelimiter(GetEnvironmentVariable('APPDATA')) + 'MonApplication';
  {$ELSEIF DEFINED(MACOS) and not DEFINED(IOS)}
  Result := IncludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    'Library/Application Support/MonApplication');
  {$ELSEIF DEFINED(IOS)}
  Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath);
  {$ELSEIF DEFINED(ANDROID)}
  Result := IncludeTrailingPathDelimiter(TPath.GetPublicPath);
  {$ELSEIF DEFINED(LINUX)}
  Result := IncludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(GetEnvironmentVariable('HOME')) +
    '.config/MonApplication');
  {$ENDIF}

  // Créer le répertoire si nécessaire
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

procedure TMainForm.PartagerTexte(const Texte: string);
begin
  {$IFDEF MSWINDOWS}
  Clipboard.AsText := Texte;
  ShowMessage('Texte copié dans le presse-papiers');
  {$ENDIF}

  {$IFDEF ANDROID}
  // Code Android pour partager du texte (simplifié)
  ShowMessage('Fonctionnalité de partage Android');
  {$ENDIF}

  {$IFDEF IOS}
  // Code iOS pour partager du texte (simplifié)
  ShowMessage('Fonctionnalité de partage iOS');
  {$ENDIF}

  {$IF DEFINED(MACOS) and not DEFINED(IOS)}
  Clipboard.AsText := Texte;
  ShowMessage('Texte copié dans le presse-papiers');
  {$ENDIF}

  {$IFDEF LINUX}
  Clipboard.AsText := Texte;
  ShowMessage('Texte copié dans le presse-papiers');
  {$ENDIF}
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  PartagerTexte('Texte à partager depuis l''application multi-plateforme');
end;

end.
```

## Conclusion

FireMonkey offre une puissante solution pour le développement multi-plateforme, permettant de cibler Windows, macOS, iOS, Android et Linux à partir d'une base de code unique. En comprenant les spécificités de chaque plateforme et en utilisant les directives conditionnelles et les APIs appropriées, vous pouvez créer des applications qui offrent une expérience utilisateur native sur chaque système d'exploitation.

L'approche recommandée consiste à développer d'abord les fonctionnalités communes, puis à adapter l'interface et le comportement pour chaque plateforme. En suivant les bonnes pratiques présentées dans cette section, vous pourrez maximiser la réutilisation du code tout en respectant les conventions et attentes des utilisateurs sur chaque plateforme.

Dans la section suivante, nous explorerons comment optimiser les performances de vos applications FireMonkey sur les appareils mobiles.
