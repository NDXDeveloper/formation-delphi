🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.3 Applications mobiles avec fonctionnalités avancées

## Introduction

Bienvenue dans le monde passionnant du développement mobile avec Delphi ! Dans ce chapitre, vous allez apprendre à créer de véritables applications iOS et Android qui exploitent pleinement les capacités des smartphones modernes.

### Pourquoi développer des applications mobiles ?

Le marché mobile est gigantesque et en constante croissance :

📱 **Plus de 6 milliards** d'utilisateurs de smartphones dans le monde  
📱 **Des millions d'applications** téléchargées chaque jour  
📱 **Opportunités professionnelles** nombreuses et variées  
📱 **Accès à un marché global** via les stores

Avec Delphi, vous pouvez créer des applications natives pour iOS et Android **avec un seul code source**, ce qui représente un avantage énorme en termes de temps et de coûts de développement.

### Ce que vous allez apprendre

À la fin de ce chapitre, vous serez capable de :

✅ Créer des applications mobiles natives iOS et Android  
✅ Concevoir des interfaces tactiles optimisées  
✅ Accéder aux capteurs (GPS, caméra, accéléromètre)  
✅ Gérer les permissions de manière appropriée  
✅ Implémenter des notifications push  
✅ Stocker des données localement  
✅ Optimiser les performances mobiles  
✅ Publier sur l'App Store et le Play Store  
✅ Gérer les différentes tailles d'écran  
✅ Intégrer des services natifs (partage, contacts, etc.)

### Prérequis

Avant de commencer, assurez-vous d'avoir :

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Compréhension de FireMonkey (FMX)
- ✅ Notions d'interfaces utilisateur

**Matériel et logiciels** :
- ✅ Delphi 13 Florence installé avec les plateformes mobiles
- ✅ Pour iOS : Un Mac avec Xcode
- ✅ Pour Android : Android SDK installé
- ✅ Un smartphone pour tester (iOS ou Android)
- ✅ Comptes développeur (Apple et/ou Google)

**Configuration** :
- ✅ PAServer configuré (pour iOS)
- ✅ SDK Android installé et configuré
- ✅ Émulateurs ou appareils de test

### Durée estimée

**20 à 25 heures** de travail, réparties ainsi :
- Compréhension des concepts mobiles : 3-4 heures
- Configuration de l'environnement : 2-3 heures
- Développement de l'interface : 5-7 heures
- Intégration des fonctionnalités : 6-8 heures
- Tests et optimisation : 3-4 heures
- Publication : 1-2 heures

---

## Partie 1 : Comprendre le développement mobile

### 1.1 Mobile vs Desktop : Les différences fondamentales

Le développement mobile présente des particularités importantes par rapport au desktop :

#### Taille d'écran

**Desktop** :
- Écrans larges (généralement 1920x1080 ou plus)
- Orientation paysage principalement
- Espace abondant pour les contrôles

**Mobile** :
- Écrans petits (5-7 pouces en général)
- Portrait et paysage
- Espace limité, nécessite priorisation

#### Interaction utilisateur

**Desktop** :
- Souris et clavier
- Précision au pixel
- Survol (hover)
- Clic droit

**Mobile** :
- Tactile uniquement
- Zone de toucher ~44x44 points
- Pas de survol
- Gestes (swipe, pinch, etc.)

#### Ressources

**Desktop** :
- CPU puissant
- Mémoire abondante
- Stockage important
- Connexion stable

**Mobile** :
- CPU limité (économie d'énergie)
- Mémoire restreinte (2-8 GB)
- Stockage variable
- Connexion intermittente

#### Système

**Desktop** :
- Applications toujours actives
- Multitâche complet
- Permissions limitées

**Mobile** :
- Cycle de vie strict
- Multitâche contrôlé
- Permissions granulaires

### 1.2 iOS vs Android

Bien que Delphi permette d'utiliser un seul code source, il est important de comprendre les différences entre les deux plateformes :

#### Tableau comparatif

| Caractéristique | iOS | Android |
|-----------------|-----|---------|
| Langage natif | Swift/Objective-C | Java/Kotlin |
| Design | HIG (Human Interface Guidelines) | Material Design |
| Fragmentation | Faible (quelques modèles) | Élevée (milliers de modèles) |
| Permissions | Plus strictes | Plus flexibles |
| Store | App Store (Apple) | Play Store (Google) |
| Validation | Revue stricte (1-7 jours) | Validation rapide (<24h) |
| Distribution | Uniquement via App Store* | Play Store + autres |
| Coût développeur | 99 USD/an | 25 USD (une fois) |

*Sauf avec certificat entreprise

#### Philosophies de design

**iOS (Human Interface Guidelines)** :
- Interface épurée et minimaliste
- Navigation par onglets (bottom tabs)
- Bouton retour en haut à gauche
- Gestes de bord d'écran
- Transparence et flou

**Android (Material Design)** :
- Design coloré avec effets d'élévation
- Navigation par tiroir (drawer)
- Bouton retour système
- FAB (Floating Action Button)
- Ombres et élévations

### 1.3 Cycle de vie d'une application mobile

Contrairement aux applications desktop, les applications mobiles ont un cycle de vie strict géré par le système d'exploitation :

```
┌─────────────┐
│   Lancée    │ ← Application démarre
└──────┬──────┘
       ↓
┌─────────────┐
│   Active    │ ← Utilisateur interagit
└──────┬──────┘
       ↓
┌─────────────┐
│  Inactive   │ ← Interruption (appel)
└──────┬──────┘
       ↓
┌─────────────┐
│ Arrière-plan│ ← Application en background
└──────┬──────┘
       ↓
┌─────────────┐
│  Suspendue  │ ← OS peut tuer l'app
└─────────────┘
```

**Événements importants** :

```pascal
procedure TFormMain.FormActivate(Sender: TObject);  
begin  
  // L'application devient active
  // Reprendre les tâches, rafraîchir les données
end;

procedure TFormMain.FormDeactivate(Sender: TObject);  
begin  
  // L'application passe en arrière-plan
  // Sauvegarder l'état, arrêter les tâches gourmandes
end;
```

---

## Partie 2 : Configuration de l'environnement

### 2.1 Configuration pour iOS

#### Prérequis

1. **Un Mac** avec macOS 11 ou supérieur
2. **Xcode** installé depuis l'App Store
3. **Compte développeur Apple** ($99/an)
4. **Certificats et profils** de provisioning

#### Installation de PAServer

PAServer (Platform Assistant Server) permet à Delphi de communiquer avec votre Mac.

**Sur le Mac** :

1. Copiez PAServer depuis le dossier d'installation Delphi
2. Lancez PAServer :
```bash
cd /Applications/PAServer
./paserver
```

3. Notez le mot de passe affiché

**Dans Delphi** :

1. Menu **Outils → Options → Serveur de plate-forme**
2. Ajoutez un nouveau serveur :
   - Nom : "Mon Mac"
   - Adresse IP : Adresse de votre Mac
   - Port : 64211
   - Mot de passe : Celui de PAServer
3. Testez la connexion

#### Configuration des certificats iOS

**Étapes** :

1. Connectez-vous à [developer.apple.com](https://developer.apple.com)
2. Créez un **App ID** pour votre application
3. Créez un **Certificat de développement**
4. Créez un **Profil de provisioning**
5. Téléchargez et installez sur le Mac

**Dans Delphi** :

1. **Projet → Options → Provisionnement**
2. Sélectionnez votre profil de provisioning
3. Sélectionnez votre certificat

### 2.2 Configuration pour Android

#### Installation du SDK Android

**Méthode 1 : Via Android Studio (recommandé)**

1. Téléchargez [Android Studio](https://developer.android.com/studio)
2. Installez Android Studio
3. Lancez le SDK Manager
4. Installez :
   - Android SDK Platform (API 34 ou supérieure — Google Play exige API 34+ depuis août 2025 pour les nouvelles apps et mises à jour)
   - Android SDK Build-Tools
   - Android SDK Platform-Tools
   - Google Play Services

**Méthode 2 : Via Delphi**

1. Menu **Outils → Options → SDK Manager**
2. Téléchargez le SDK Android
3. Installez les packages nécessaires

#### Configuration dans Delphi

1. **Outils → Options → SDK Manager**
2. Onglet **Android**
3. Vérifiez les chemins :
   - SDK Path : Chemin vers le SDK Android
   - NDK Path : Chemin vers le NDK
   - JDK Path : Chemin vers Java

#### Connexion d'un appareil Android

**Mode développeur** :

1. Sur le téléphone : **Paramètres → À propos**
2. Appuyez 7 fois sur "Numéro de build"
3. **Paramètres → Options développeur**
4. Activez "Débogage USB"

**Test de connexion** :

```bash
adb devices
```

Vous devriez voir votre appareil listé.

### 2.3 Création du projet mobile

**Étape 1 : Nouveau projet**

1. **Fichier → Nouveau → Application multi-plateforme**
2. Choisissez **Application vide**
3. Sauvegardez le projet

**Étape 2 : Ajout des plateformes**

1. Dans le **Gestionnaire de projets**
2. Clic droit sur le projet
3. **Ajouter une plateforme**
4. Sélectionnez **iOS Device 64-bit** et **Android 64-bit**

**Étape 3 : Configuration de base**

```pascal
program MobileApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

---

## Partie 3 : Interface utilisateur mobile

### 3.1 Principes de conception mobile

#### Règles d'or

1. **Simplicité** : Chaque écran = une tâche principale
2. **Accessibilité** : Zones tactiles minimum 44x44 points
3. **Visibilité** : Contrastes élevés, textes lisibles
4. **Feedback** : Réponse immédiate aux actions
5. **Navigation** : Claire et intuitive

#### Zones de confort tactile

```
┌─────────────────────┐
│     Difficile       │  Zone supérieure
├─────────────────────┤
│                     │
│      Facile         │  Zone centrale (pouce)
│                     │
├─────────────────────┤
│     Facile          │  Zone inférieure
└─────────────────────┘
```

**Conseil** : Placez les actions importantes dans les zones faciles d'accès.

### 3.2 Conception du formulaire principal

Créons une application de **suivi de fitness** avec fonctionnalités avancées.

#### Structure de l'interface

```
┌───────────────────────────────┐
│  ← Fitness Tracker            │  Barre de titre
├───────────────────────────────┤
│                               │
│   [Icône]  12,453 pas         │  Carte d'activité
│            8.2 km parcourus   │
│                               │
├───────────────────────────────┤
│                               │
│   [Graphique d'activité]      │  Visualisation
│                               │
├───────────────────────────────┤
│  [Démarrer] [Historique]      │  Actions
└───────────────────────────────┘
│ [Activité][Stats][Profil]     │  Navigation
└───────────────────────────────┘
```

#### Composants principaux

**Pour l'interface mobile** :

1. **TToolBar** : Barre de titre en haut
2. **TTabControl** : Navigation par onglets
3. **TListView** : Listes optimisées
4. **TLayout** : Conteneurs pour organisation
5. **TRectangle** : Cartes et zones visuelles
6. **TButton** : Boutons avec zones tactiles adéquates
7. **TLabel** : Textes et titres

### 3.3 Code du formulaire principal

```pascal
unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TabControl, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  FMX.Controls.Presentation, FMX.Objects;

type
  TFormMain = class(TForm)
    TabControl1: TTabControl;
    TabItemActivity: TTabItem;
    TabItemStats: TTabItem;
    TabItemProfile: TTabItem;
    ToolBar1: TToolBar;
    LabelTitle: TLabel;
    LayoutActivity: TLayout;
    RectangleSteps: TRectangle;
    LabelSteps: TLabel;
    LabelDistance: TLabel;
    ButtonStart: TButton;
    ButtonHistory: TButton;

    procedure FormCreate(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FIsTracking: Boolean;
    procedure UpdateStepCount(ASteps: Integer);
    procedure AdaptToOrientation;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  FMX.Platform;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  FIsTracking := False;

  // Adapter l'interface à l'écran
  AdaptToOrientation;

  // Initialiser l'affichage
  UpdateStepCount(0);
end;

procedure TFormMain.AdaptToOrientation;  
begin  
  // Adapter selon l'orientation
  if Width > Height then
  begin
    // Mode paysage
    LayoutActivity.Align := TAlignLayout.Left;
    LayoutActivity.Width := Width / 2;
  end
  else
  begin
    // Mode portrait
    LayoutActivity.Align := TAlignLayout.Top;
    LayoutActivity.Height := Height * 0.4;
  end;
end;

procedure TFormMain.FormResize(Sender: TObject);  
begin  
  AdaptToOrientation;
end;

procedure TFormMain.ButtonStartClick(Sender: TObject);  
begin  
  FIsTracking := not FIsTracking;

  if FIsTracking then
  begin
    ButtonStart.Text := 'Arrêter';
    // Démarrer le suivi
  end
  else
  begin
    ButtonStart.Text := 'Démarrer';
    // Arrêter le suivi
  end;
end;

procedure TFormMain.UpdateStepCount(ASteps: Integer);  
var  
  Distance: Double;
begin
  LabelSteps.Text := Format('%s pas', [FormatFloat('#,##0', ASteps)]);

  // Estimation : 1 pas ≈ 0.75m
  Distance := ASteps * 0.75 / 1000;
  LabelDistance.Text := Format('%.1f km', [Distance]);
end;

end.
```

### 3.4 Navigation par onglets

La navigation par onglets est idéale pour mobile :

```pascal
procedure TFormMain.TabControl1Change(Sender: TObject);  
begin  
  case TabControl1.ActiveTab.Index of
    0: // Onglet Activité
      begin
        LabelTitle.Text := 'Activité';
      end;
    1: // Onglet Statistiques
      begin
        LabelTitle.Text := 'Statistiques';
        LoadStatistics;
      end;
    2: // Onglet Profil
      begin
        LabelTitle.Text := 'Profil';
        LoadProfile;
      end;
  end;
end;
```

### 3.5 Adaptation aux différentes tailles d'écran

```pascal
procedure TFormMain.AdaptToScreenSize;  
var  
  ScreenWidth: Single;
  Scale: Single;
begin
  // Obtenir la largeur de l'écran (FMX : TScreen expose Width/Height en Single,
  // il n'y a pas de propriété Size groupée comme en VCL).
  ScreenWidth := Screen.Width;

  // Calculer le facteur d'échelle
  if ScreenWidth < 768 then
  begin
    // Petit écran (iPhone SE, etc.)
    Scale := 0.8;
    LabelTitle.TextSettings.Font.Size := 18;
  end
  else if ScreenWidth < 1024 then
  begin
    // Écran moyen (iPhone standard)
    Scale := 1.0;
    LabelTitle.TextSettings.Font.Size := 20;
  end
  else
  begin
    // Grand écran (iPad, tablettes)
    Scale := 1.2;
    LabelTitle.TextSettings.Font.Size := 24;
  end;

  // Appliquer l'échelle aux composants
  ButtonStart.Height := 50 * Scale;
  ButtonHistory.Height := 50 * Scale;
end;
```

---

## Partie 4 : Fonctionnalités natives

### 4.1 Accès aux services de localisation (GPS)

#### Demande de permission

**Android (AndroidManifest.xml)** :

```xml
<uses-permission android:name="android.permission.ACCESS_FINE_LOCATION"/>
<uses-permission android:name="android.permission.ACCESS_COARSE_LOCATION"/>
```

**iOS (Info.plist)** :

```xml
<key>NSLocationWhenInUseUsageDescription</key>
<string>Cette app utilise votre position pour suivre vos activités</string>
```

#### Code Delphi

```pascal
uses
  System.Math, System.Sensors, System.Sensors.Components, System.Permissions;

type
  TFormMain = class(TForm)
    LocationSensor1: TLocationSensor;
    procedure LocationSensor1LocationChanged(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
  private
    procedure RequestLocationPermission;
    procedure OnPermissionRequestResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
    function HaversineDistance(const A, B: TLocationCoord2D): Double;
  end;

procedure TFormMain.RequestLocationPermission;  
begin  
  PermissionsService.RequestPermissions(
    ['android.permission.ACCESS_FINE_LOCATION',
     'android.permission.ACCESS_COARSE_LOCATION'],
    OnPermissionRequestResult
  );
end;

procedure TFormMain.OnPermissionRequestResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
begin
  if (Length(AGrantResults) > 0) and
     (AGrantResults[0] = TPermissionStatus.Granted) then
  begin
    // Permission accordée, activer le GPS
    LocationSensor1.Active := True;
  end
  else
  begin
    ShowMessage('Permission GPS refusée');
  end;
end;

// Distance haversine en mètres entre deux coordonnées GPS.
// TLocationCoord2D (System.Sensors) est un simple record (Latitude, Longitude) :
// il n'expose pas de méthode Distance, il faut donc calculer soi-même.
function TFormMain.HaversineDistance(const A, B: TLocationCoord2D): Double;  
const  
  EarthRadiusMeters = 6371000.0;
var
  Lat1, Lat2, DLat, DLon, H: Double;
begin
  Lat1 := DegToRad(A.Latitude);
  Lat2 := DegToRad(B.Latitude);
  DLat := DegToRad(B.Latitude - A.Latitude);
  DLon := DegToRad(B.Longitude - A.Longitude);

  H := Sin(DLat / 2) * Sin(DLat / 2) +
       Cos(Lat1) * Cos(Lat2) * Sin(DLon / 2) * Sin(DLon / 2);

  Result := 2 * EarthRadiusMeters * ArcSin(Sqrt(H));
end;

procedure TFormMain.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  Distance: Double;
begin
  // Calculer la distance parcourue (formule haversine en mètres)
  Distance := HaversineDistance(OldLocation, NewLocation);

  // Mettre à jour l'affichage
  LabelLocation.Text := Format('Lat: %.6f, Lon: %.6f',
    [NewLocation.Latitude, NewLocation.Longitude]);
  LabelDistance.Text := Format('Distance: %.2f m', [Distance]);
end;
```

#### Optimisation de la batterie

```pascal
procedure TFormMain.ConfigureLocationSensor;  
begin  
  // Précision vs batterie
  LocationSensor1.Accuracy := 10; // 10 mètres

  // Mise à jour minimum
  LocationSensor1.Distance := 5; // Tous les 5 mètres

  // Désactiver quand inutile
  if not FIsTracking then
    LocationSensor1.Active := False;
end;
```

### 4.2 Utilisation de la caméra

#### Permissions

**Android** :
```xml
<uses-permission android:name="android.permission.CAMERA"/>
<uses-feature android:name="android.hardware.camera" android:required="false"/>
```

**iOS** :
```xml
<key>NSCameraUsageDescription</key>
<string>Cette app utilise la caméra pour prendre des photos</string>
```

#### Code de capture

L'approche recommandée en FMX est d'utiliser **`TTakePhotoFromCameraAction`** (issue de `FMX.MediaLibrary.Actions`) plutôt que d'appeler manuellement le service de caméra :

```pascal
uses
  FMX.MediaLibrary, FMX.MediaLibrary.Actions, FMX.Platform, FMX.Graphics,
  System.IOUtils;

type
  TFormMain = class(TForm)
    ButtonTakePhoto: TButton;
    ImagePhoto: TImage;
    TakePhotoFromCameraAction1: TTakePhotoFromCameraAction;
    procedure ButtonTakePhotoClick(Sender: TObject);
    procedure TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);
  private
    procedure SavePhotoToFile(ABitmap: TBitmap);
  end;

procedure TFormMain.ButtonTakePhotoClick(Sender: TObject);  
begin  
  // Déclenche l'ouverture de la caméra ; le callback DidFinishTaking
  // est appelé avec le bitmap capturé.
  TakePhotoFromCameraAction1.Execute;
end;

procedure TFormMain.TakePhotoFromCameraAction1DidFinishTaking(Image: TBitmap);  
begin  
  if Assigned(Image) then
  begin
    ImagePhoto.Bitmap.Assign(Image);
    SavePhotoToFile(Image);
  end;
end;

procedure TFormMain.SavePhotoToFile(ABitmap: TBitmap);  
var  
  FileName: string;
begin
  FileName := TPath.Combine(
    TPath.GetDocumentsPath,
    Format('photo_%s.jpg', [FormatDateTime('yyyymmdd_hhnnss', Now)])
  );

  ABitmap.SaveToFile(FileName);
end;
```

### 4.3 Accès à l'accéléromètre

```pascal
uses
  System.Sensors, System.Sensors.Components;

type
  // Record local : System.Sensors n'expose pas de type "TAcceleration" prêt
  // à l'emploi, on compose donc nous-mêmes les trois axes dans un record.
  TAcceleration = record
    X, Y, Z: Double;
  end;

  TFormMain = class(TForm)
    MotionSensor1: TMotionSensor;
    procedure MotionSensor1DataChanged(Sender: TObject);
  private
    FStepCount: Integer;
    FLastAcceleration: TAcceleration;
    procedure DetectStep(const AAccel: TAcceleration);
  end;

procedure TFormMain.MotionSensor1DataChanged(Sender: TObject);  
var  
  Accel: TAcceleration;
begin
  // AccelerationX/Y/Z renvoient chacun un Double (un axe) ; on les agrège
  // dans le record TAcceleration déclaré ci-dessus.
  Accel.X := MotionSensor1.Sensor.AccelerationX;
  Accel.Y := MotionSensor1.Sensor.AccelerationY;
  Accel.Z := MotionSensor1.Sensor.AccelerationZ;
  DetectStep(Accel);
  FLastAcceleration := Accel;
end;

procedure TFormMain.DetectStep(const AAccel: TAcceleration);  
var  
  Magnitude: Double;
  Threshold: Double;
begin
  // Calculer l'intensité du mouvement.
  // ⚠️ AccelerationX/Y/Z sont exprimés en **g** (unités de gravité) sous Delphi
  // FMX. Au repos, téléphone à plat sur une table : X≈0, Y≈0, Z≈1 → Magnitude≈1.
  // Tout mouvement ajoute à cette valeur de base.
  Magnitude := Sqrt(
    Sqr(AAccel.X) +
    Sqr(AAccel.Y) +
    Sqr(AAccel.Z)
  );

  Threshold := 1.2; // Seuil de détection (1.2 g = gravité + 0.2 g supplémentaire)

  // Détecter un pas
  if Magnitude > Threshold then
  begin
    Inc(FStepCount);
    UpdateStepCount(FStepCount);
  end;
end;
```

> ⚠️ **Limite de cet exemple** : ce code est volontairement très simplifié pour illustrer la lecture du capteur. Un seul vrai pas génère typiquement plusieurs pics d'accélération (impact, rebond, décélération) qui seront tous comptés ici — vous obtiendrez 3 à 10 fois trop de pas. Pour un véritable podomètre, utilisez plutôt :  
> - **iOS** : `CMPedometer` (framework CoreMotion) — disponible via interop Objective-C  
> - **Android** : `Sensor.TYPE_STEP_COUNTER` (capteur matériel calibré) — disponible via interop Java  
> - Ou un algorithme avec **moyenne mobile + détection de pic + période de réfraction** (≥ 250 ms entre deux pas) sur la composante haute fréquence de la magnitude.

### 4.4 Notifications locales

```pascal
uses
  FMX.Notification;

type
  TFormMain = class(TForm)
    NotificationCenter1: TNotificationCenter;
  private
    procedure ScheduleNotification;
    procedure SendNotificationNow;
  end;

procedure TFormMain.ScheduleNotification;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter1.CreateNotification;
  try
    Notification.Name := 'ReminderNotification';
    Notification.Title := 'Rappel d''activité';
    Notification.AlertBody := 'Il est temps de bouger !';
    Notification.FireDate := Now + (1/24); // Dans 1 heure
    Notification.EnableSound := True;

    // Planifier la notification
    NotificationCenter1.ScheduleNotification(Notification);
  finally
    Notification.Free;
  end;
end;

procedure TFormMain.SendNotificationNow;  
var  
  Notification: TNotification;
begin
  Notification := NotificationCenter1.CreateNotification;
  try
    Notification.Name := 'InstantNotification';
    Notification.Title := 'Objectif atteint !';
    Notification.AlertBody := 'Vous avez atteint vos 10,000 pas !';

    // Envoyer immédiatement
    NotificationCenter1.PresentNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

**Permissions requises** :

**iOS** : Demandée automatiquement au premier usage

**Android (API 33+)** :
```xml
<uses-permission android:name="android.permission.POST_NOTIFICATIONS"/>
```

### 4.5 Partage de contenu

```pascal
uses
  FMX.Platform;

procedure ShareText(const AText: string);  
var  
  ShareService: IFMXShareService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXShareService, ShareService) then
  begin
    ShareService.Share(nil, AText);
  end;
end;

procedure ShareImage(AImage: TBitmap; const AMessage: string);  
var  
  ShareService: IFMXShareService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXShareService, ShareService) then
  begin
    ShareService.Share(AImage, AMessage);
  end;
end;

// Utilisation
procedure TFormMain.ButtonShareClick(Sender: TObject);  
begin  
  ShareText('J''ai parcouru 10,000 pas aujourd''hui avec FitnessTracker !');
end;
```

---

## Partie 5 : Stockage de données

### 5.1 Stockage local avec SQLite

SQLite est idéal pour les applications mobiles car il est léger et intégré.

#### Configuration de FireDAC pour SQLite

```pascal
uses
  FireDAC.Stan.Def, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Comp.Client, FireDAC.Phys.SQLite, FireDAC.Stan.ExprFuncs;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure InitializeDatabase;
    function GetDatabasePath: string;
    procedure CreateTables;  // Doit être déclarée pour que l'implémentation
                             // `procedure TDataModule1.CreateTables;` ci-dessous compile.
  end;

procedure TDataModule1.InitializeDatabase;  
var  
  DBPath: string;
begin
  DBPath := GetDatabasePath;

  // Configuration de la connexion
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Database=' + DBPath);
  FDConnection1.Params.Add('DriverID=SQLite');
  FDConnection1.Params.Add('LockingMode=Normal');

  // Créer le fichier s'il n'existe pas
  if not TFile.Exists(DBPath) then
  begin
    FDConnection1.Connected := True;
    CreateTables;
  end
  else
    FDConnection1.Connected := True;
end;

function TDataModule1.GetDatabasePath: string;  
begin  
  {$IF DEFINED(IOS)}
  Result := TPath.Combine(TPath.GetDocumentsPath, 'fitness.db');
  {$ELSEIF DEFINED(ANDROID)}
  Result := TPath.Combine(TPath.GetDocumentsPath, 'fitness.db');
  {$ELSE}
  Result := TPath.Combine(TPath.GetHomePath, 'fitness.db');
  {$ENDIF}
end;

procedure TDataModule1.CreateTables;  
begin  
  // Créer la table des activités
  FDConnection1.ExecSQL(
    'CREATE TABLE IF NOT EXISTS activities (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  date TEXT NOT NULL,' +
    '  steps INTEGER,' +
    '  distance REAL,' +
    '  calories INTEGER,' +
    '  duration INTEGER' +
    ')'
  );
end;
```

#### Sauvegarde de données

```pascal
procedure SaveActivity(ADate: TDateTime; ASteps, ACalories, ADuration: Integer;
  ADistance: Double);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO activities (date, steps, distance, calories, duration) ' +
      'VALUES (:date, :steps, :distance, :calories, :duration)';

    Query.ParamByName('date').AsDateTime := ADate;
    Query.ParamByName('steps').AsInteger := ASteps;
    Query.ParamByName('distance').AsFloat := ADistance;
    Query.ParamByName('calories').AsInteger := ACalories;
    Query.ParamByName('duration').AsInteger := ADuration;

    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

#### Lecture de données

```pascal
procedure LoadActivities(AListBox: TListBox);  
var  
  Query: TFDQuery;
  Item: TListBoxItem;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text :=
      'SELECT * FROM activities ORDER BY date DESC LIMIT 30';
    Query.Open;

    AListBox.Clear;
    AListBox.BeginUpdate;
    try
      while not Query.Eof do
      begin
        Item := TListBoxItem.Create(AListBox);
        Item.Parent := AListBox;
        Item.Text := Format('%s - %d pas', [
          FormatDateTime('dd/mm/yyyy', Query.FieldByName('date').AsDateTime),
          Query.FieldByName('steps').AsInteger
        ]);
        Item.Height := 60;

        Query.Next;
      end;
    finally
      AListBox.EndUpdate;
    end;
  finally
    Query.Free;
  end;
end;
```

### 5.2 Préférences utilisateur

```pascal
uses
  System.IOUtils, System.JSON;

type
  TUserPreferences = class
  private
    FDailyGoal: Integer;
    FNotificationsEnabled: Boolean;
    FUnitSystem: string; // 'metric' ou 'imperial'
    function GetPreferencesFile: string;
  public
    constructor Create;
    procedure Load;
    procedure Save;

    property DailyGoal: Integer read FDailyGoal write FDailyGoal;
    property NotificationsEnabled: Boolean read FNotificationsEnabled
      write FNotificationsEnabled;
    property UnitSystem: string read FUnitSystem write FUnitSystem;
  end;

function TUserPreferences.GetPreferencesFile: string;  
begin  
  Result := TPath.Combine(TPath.GetDocumentsPath, 'preferences.json');
end;

procedure TUserPreferences.Save;  
var  
  JSON: TJSONObject;
begin
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('dailyGoal', TJSONNumber.Create(FDailyGoal));
    JSON.AddPair('notificationsEnabled', TJSONBool.Create(FNotificationsEnabled));
    JSON.AddPair('unitSystem', FUnitSystem);

    TFile.WriteAllText(GetPreferencesFile, JSON.ToString);
  finally
    JSON.Free;
  end;
end;

procedure TUserPreferences.Load;  
var  
  JSON: TJSONObject;
  JSONText: string;
begin
  // ⚠️ On initialise TOUJOURS les valeurs par défaut AVANT de tenter le parsing,
  // pour couvrir les trois cas qui font échouer la lecture :
  //   - le fichier n'existe pas (première installation),
  //   - le fichier est corrompu (crash en cours d'écriture, édition manuelle),
  //   - une clé a été renommée/supprimée entre deux versions de l'app.
  // Sans ces valeurs par défaut, le démarrage de l'app planterait sur
  // EAccessViolation (JSON nil) ou EJSONException (clé manquante).
  FDailyGoal := 10000;
  FNotificationsEnabled := True;
  FUnitSystem := 'metric';

  if not TFile.Exists(GetPreferencesFile) then
    Exit;

  JSONText := TFile.ReadAllText(GetPreferencesFile);
  JSON := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
  if not Assigned(JSON) then
    Exit; // Fichier corrompu : on garde les valeurs par défaut

  try
    // TryGetValue : une clé absente ne casse pas tout, on conserve la valeur
    // par défaut initialisée plus haut.
    JSON.TryGetValue<Integer>('dailyGoal', FDailyGoal);
    JSON.TryGetValue<Boolean>('notificationsEnabled', FNotificationsEnabled);
    JSON.TryGetValue<string>('unitSystem', FUnitSystem);
  finally
    JSON.Free;
  end;
end;
```

---

## Partie 6 : Optimisation des performances mobiles

### 6.1 Gestion de la mémoire

La mémoire est limitée sur mobile, il faut l'utiliser judicieusement.

#### Libération des ressources

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImageList: TObjectList<TBitmap>;
  end;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  FImageList := TObjectList<TBitmap>.Create(True);
end;

procedure TFormMain.FormDestroy(Sender: TObject);  
begin  
  // Libérer les ressources
  FImageList.Free;

  // Arrêter les capteurs
  LocationSensor1.Active := False;
  MotionSensor1.Active := False;
end;
```

#### Chargement d'images optimisé

```pascal
procedure LoadImageOptimized(const AFileName: string; AImage: TImage);  
var  
  Bitmap: TBitmap;
  MaxSize: Integer;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile(AFileName);

    // Redimensionner si trop grande
    MaxSize := 1024;
    if (Bitmap.Width > MaxSize) or (Bitmap.Height > MaxSize) then
    begin
      if Bitmap.Width > Bitmap.Height then
        Bitmap.Resize(MaxSize, Trunc(Bitmap.Height * MaxSize / Bitmap.Width))
      else
        Bitmap.Resize(Trunc(Bitmap.Width * MaxSize / Bitmap.Height), MaxSize);
    end;

    AImage.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
```

### 6.2 Threading pour les opérations longues

```pascal
procedure TFormMain.LoadDataAsync;  
begin  
  // Afficher un indicateur de chargement
  ShowLoadingIndicator;

  TTask.Run(
    procedure
    var
      Data: TArray<TActivity>;
    begin
      // Opération longue dans un thread séparé
      Data := LoadActivitiesFromDatabase;

      // Retour au thread principal pour l'UI
      TThread.Synchronize(nil,
        procedure
        begin
          DisplayActivities(Data);
          HideLoadingIndicator;
        end);
    end);
end;
```

### 6.3 Économie de batterie

#### Désactiver les capteurs inutiles

```pascal
procedure TFormMain.FormDeactivate(Sender: TObject);  
begin  
  // L'app passe en arrière-plan
  // Désactiver les capteurs pour économiser la batterie
  if not FIsTracking then
  begin
    LocationSensor1.Active := False;
    MotionSensor1.Active := False;
  end;

  // Sauvegarder l'état
  SaveCurrentState;
end;

procedure TFormMain.FormActivate(Sender: TObject);  
begin  
  // L'app redevient active
  // Réactiver si nécessaire
  if FIsTracking then
  begin
    LocationSensor1.Active := True;
    MotionSensor1.Active := True;
  end;
end;
```

#### Optimiser les mises à jour

```pascal
// ❌ Mauvais : Mise à jour à chaque changement
procedure TFormMain.MotionSensor1DataChanged(Sender: TObject);  
begin  
  UpdateUI; // Trop fréquent !
end;

// ✅ Bon : Mise à jour temporisée
type
  TFormMain = class(TForm)
    TimerUpdate: TTimer;
  private
    FPendingUpdate: Boolean;
  end;

procedure TFormMain.MotionSensor1DataChanged(Sender: TObject);  
begin  
  FPendingUpdate := True;
  // Le timer mettra à jour l'UI toutes les 500ms
end;

procedure TFormMain.TimerUpdateTimer(Sender: TObject);  
begin  
  if FPendingUpdate then
  begin
    UpdateUI;
    FPendingUpdate := False;
  end;
end;
```

### 6.4 Virtualisation des listes

Pour de grandes listes, utilisez la virtualisation :

```pascal
procedure TFormMain.SetupListView;  
begin  
  // ListView gère automatiquement la virtualisation
  ListView1.ItemAppearance.ItemHeight := 60;

  // Charger par lots
  LoadItemsInBatches;
end;

procedure LoadItemsInBatches;  
const  
  BATCH_SIZE = 50;
var
  I: Integer;
begin
  // Charger les 50 premiers
  for I := 0 to BATCH_SIZE - 1 do
  begin
    if I < TotalItems then
      AddItemToListView(I);
  end;

  // Charger le reste à la demande
end;
```

---

## Partie 7 : Gestion des permissions

### 7.1 Système de permissions moderne

Les systèmes mobiles modernes exigent des permissions explicites pour accéder aux fonctionnalités sensibles.

#### Permissions courantes

| Permission | iOS | Android |
|------------|-----|---------|
| Localisation | NSLocationWhenInUseUsageDescription | ACCESS_FINE_LOCATION |
| Caméra | NSCameraUsageDescription | CAMERA |
| Photos | NSPhotoLibraryUsageDescription | READ_MEDIA_IMAGES (API 33+) / READ_EXTERNAL_STORAGE (≤ API 32) |
| Notifications | Automatique | POST_NOTIFICATIONS (API 33+) |
| Contacts | NSContactsUsageDescription | READ_CONTACTS |

> ℹ️ **Note Android 13+** : depuis API 33, `READ_EXTERNAL_STORAGE` est **déprécié** au profit des permissions granulaires `READ_MEDIA_IMAGES`, `READ_MEDIA_VIDEO`, `READ_MEDIA_AUDIO`. Sur Android 13+, déclarer uniquement `READ_EXTERNAL_STORAGE` ne permet plus d'accéder aux photos. Google Play exigeant API 34+ pour les nouvelles apps depuis août 2025, vous devez désormais utiliser les permissions `READ_MEDIA_*`.

#### Code de demande de permission

```pascal
uses
  System.Permissions;

type
  TFormMain = class(TForm)
  private
    procedure RequestPermissions;
    procedure OnPermissionsResult(Sender: TObject;
      const APermissions: TArray<string>;
      const AGrantResults: TArray<TPermissionStatus>);
  end;

procedure TFormMain.RequestPermissions;  
var  
  Permissions: TArray<string>;
begin
  {$IF DEFINED(ANDROID)}
  SetLength(Permissions, 3);
  Permissions[0] := 'android.permission.ACCESS_FINE_LOCATION';
  Permissions[1] := 'android.permission.CAMERA';
  Permissions[2] := 'android.permission.POST_NOTIFICATIONS';
  {$ELSE}
  // iOS demande les permissions automatiquement
  SetLength(Permissions, 0);
  {$ENDIF}

  if Length(Permissions) > 0 then
  begin
    PermissionsService.RequestPermissions(
      Permissions,
      OnPermissionsResult
    );
  end;
end;

procedure TFormMain.OnPermissionsResult(Sender: TObject;
  const APermissions: TArray<string>;
  const AGrantResults: TArray<TPermissionStatus>);
var
  I: Integer;
  AllGranted: Boolean;
begin
  AllGranted := True;

  for I := 0 to High(AGrantResults) do
  begin
    if AGrantResults[I] <> TPermissionStatus.Granted then
    begin
      AllGranted := False;
      Break;
    end;
  end;

  if AllGranted then
    InitializeFeatures
  else
    ShowPermissionDeniedMessage;
end;
```

### 7.2 Gestion élégante des refus

```pascal
procedure TFormMain.CheckAndRequestPermission(const APermission: string;
  AOnGranted: TProc);
begin
  if PermissionsService.IsPermissionGranted(APermission) then
  begin
    // Permission déjà accordée
    if Assigned(AOnGranted) then
      AOnGranted;
  end
  else
  begin
    // Demander la permission
    PermissionsService.RequestPermissions(
      [APermission],
      procedure(const APermissions: TArray<string>;
        const AGrantResults: TArray<TPermissionStatus>)
      begin
        if (Length(AGrantResults) > 0) and
           (AGrantResults[0] = TPermissionStatus.Granted) then
        begin
          if Assigned(AOnGranted) then
            AOnGranted;
        end
        else
        begin
          ShowMessage('Permission nécessaire pour cette fonctionnalité');
        end;
      end);
  end;
end;

// Utilisation
procedure TFormMain.ButtonLocationClick(Sender: TObject);  
begin  
  CheckAndRequestPermission(
    'android.permission.ACCESS_FINE_LOCATION',
    procedure
    begin
      StartLocationTracking;
    end);
end;
```

---

## Partie 8 : Tests sur appareils réels

### 8.1 Déploiement sur iOS

#### Étapes de déploiement

1. **Connecter l'iPhone** au Mac via USB
2. **Dans Delphi**, sélectionner la plateforme **iOS Device - 64 bit**
3. **Compiler** (Ctrl+F9)
4. **Exécuter** (F9) - l'app est transférée et lancée

#### Debug sur iPhone

```pascal
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  {$IFDEF DEBUG}
  // Afficher des informations de debug
  LabelDebug.Visible := True;
  LabelDebug.Text := 'Mode DEBUG actif';
  {$ENDIF}
end;

procedure LogDebug(const AMessage: string);  
begin  
  {$IFDEF DEBUG}
  // Écrire dans les logs
  FMX.Types.Log.d(AMessage);
  {$ENDIF}
end;
```

#### Console de debug iOS

Utilisez **Console.app** sur Mac pour voir les logs :
1. Ouvrez **Console.app**
2. Sélectionnez votre iPhone
3. Filtrez par le nom de votre app

### 8.2 Déploiement sur Android

#### Via USB

1. **Activer le débogage USB** sur le téléphone
2. **Connecter** via USB
3. **Dans Delphi**, sélectionner **Android 64-bit**
4. **Compiler et exécuter**

#### Logs Android

```bash
# Voir les logs en temps réel
adb logcat | grep "YourAppName"

# Logs spécifiques
adb logcat *:E  # Erreurs uniquement
```

#### Debug dans le code

```pascal
uses
  {$IFDEF ANDROID}
  Androidapi.Log,
  {$ENDIF}
  FMX.Types;

procedure LogMessage(const AMessage: string);  
begin  
  {$IFDEF ANDROID}
  // LOGI attend un MarshaledAString (PAnsiChar). On encode donc le message en UTF-8.
  LOGI(MarshaledAString(UTF8Encode(AMessage)));
  {$ENDIF}

  {$IFDEF IOS}
  FMX.Types.Log.d(AMessage);
  {$ENDIF}
end;
```

### 8.3 Tests essentiels

#### Checklist de test

**Fonctionnalités de base** :
- [ ] Lancement de l'app
- [ ] Navigation entre écrans
- [ ] Saisie de données
- [ ] Sauvegarde/chargement
- [ ] Rotation de l'écran

**Permissions** :
- [ ] Demande de permissions appropriée
- [ ] Gestion du refus de permission
- [ ] Fonctionnement avec permissions accordées

**Cycle de vie** :
- [ ] Mise en arrière-plan
- [ ] Retour au premier plan
- [ ] Interruption (appel entrant)
- [ ] Fermeture propre

**Performance** :
- [ ] Fluidité de l'interface
- [ ] Temps de chargement
- [ ] Consommation batterie
- [ ] Utilisation mémoire

**Réseau** :
- [ ] Avec connexion WiFi
- [ ] Avec données mobiles
- [ ] Sans connexion
- [ ] Perte de connexion

**Différentes résolutions** :
- [ ] Petit écran (5")
- [ ] Moyen (6")
- [ ] Grand (6.7"+)
- [ ] Tablette

---

## Partie 9 : Publication sur les stores

### 9.1 Préparation de l'application

#### Checklist avant publication

**Code** :
- [ ] Version Release compilée
- [ ] Debug désactivé
- [ ] Optimisations activées
- [ ] Pas de code de test
- [ ] Gestion d'erreurs complète

**Ressources** :
- [ ] Icônes pour toutes les tailles
- [ ] Écran de lancement (Splash)
- [ ] Images de prévisualisation
- [ ] Captures d'écran

**Légal** :
- [ ] Politique de confidentialité
- [ ] Conditions d'utilisation
- [ ] Licences des bibliothèques tierces
- [ ] Conformité RGPD/GDPR

**Tests** :
- [ ] Tests sur plusieurs appareils
- [ ] Test de toutes les fonctionnalités
- [ ] Test des cas d'erreur
- [ ] Test hors ligne

### 9.2 Publication sur l'App Store (iOS)

#### Configuration dans Delphi

**Options du projet** :

1. **Version info** :
   - CFBundleVersion : `1.0.0`
   - CFBundleShortVersionString : `1.0`

2. **Icônes** :
   - Ajoutez toutes les tailles d'icônes requises
   - 1024x1024 pour l'App Store

3. **Orientation** :
   - Portrait, Landscape, ou les deux

#### App Store Connect

1. **Créer l'app** sur [App Store Connect](https://appstoreconnect.apple.com)
2. **Remplir les informations** :
   - Nom de l'app
   - Description
   - Mots-clés
   - Catégorie
   - Captures d'écran (iPhone et iPad)
   - Icône 1024x1024

3. **Créer l'archive** dans Delphi
4. **Uploader via Transporter** (app Mac)
5. **Soumettre pour révision**

#### Temps de validation

- Révision : 1-7 jours généralement
- Première soumission souvent plus longue
- Rejets possibles (suivre les guidelines Apple)

### 9.3 Publication sur le Play Store (Android)

#### Signature de l'APK

Créez un keystore :

```bash
keytool -genkey -v -keystore my-app.keystore -alias my-app -keyalg RSA -keysize 2048 -validity 10000
```

**Dans Delphi** :

1. **Projet → Options → Provisionnement (Android)**
2. **Keystore** : Sélectionnez votre fichier `.keystore`
3. **Alias** : Votre alias
4. **Mots de passe** : Keystore et alias

#### Configuration du Play Store

1. **Créer l'app** sur [Google Play Console](https://play.google.com/console)
2. **Fiche du Store** :
   - Titre
   - Description courte (80 caractères)
   - Description complète
   - Captures d'écran (téléphone, tablette 7", tablette 10")
   - Icône 512x512
   - Bannière 1024x500

3. **Classification du contenu**
4. **Prix et distribution**
5. **Publication** :
   - Production : Publication complète
   - Test interne : Pour les testeurs
   - Test fermé : Pour un groupe limité
   - Test ouvert : Pour le grand public

#### Génération de l'APK/AAB

**AAB (Android App Bundle)** recommandé :

1. Compilez en mode Release
2. Générez le fichier AAB
3. Uploadez sur Play Console

**Temps de validation** :
- Quelques heures généralement
- Moins strict qu'Apple
- Révisions automatiques et manuelles

### 9.4 Mises à jour

#### Gestion des versions

```pascal
const
  APP_VERSION = '1.0.0';
  BUILD_NUMBER = 1;

procedure CheckForUpdates;  
var  
  RESTClient: TRESTClient;
  RESTRequest: TRESTRequest;
  RESTResponse: TRESTResponse;
  LatestVersion: string;
begin
  // Vérifier la dernière version depuis votre serveur
  RESTClient := TRESTClient.Create('https://api.votreapp.com');
  try
    RESTRequest := TRESTRequest.Create(nil);
    RESTResponse := TRESTResponse.Create(nil);
    try
      RESTRequest.Client := RESTClient;
      RESTRequest.Response := RESTResponse;
      RESTRequest.Resource := 'version';
      RESTRequest.Execute;

      LatestVersion := RESTResponse.JSONValue.GetValue<string>('version');

      if LatestVersion <> APP_VERSION then
        PromptUpdate;
    finally
      RESTRequest.Free;
      RESTResponse.Free;
    end;
  finally
    RESTClient.Free;
  end;
end;

procedure PromptUpdate;  
begin  
  MessageDlg('Une nouvelle version est disponible. Voulez-vous mettre à jour ?',
    TMsgDlgType.mtInformation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
        OpenAppStore;
    end);
end;

procedure OpenAppStore;  
{$IF DEFINED(IOS) or DEFINED(ANDROID)}
var
  URL: string;
{$ENDIF}
begin
  {$IFDEF IOS}
  // Format moderne (apps.apple.com remplace itunes.apple.com depuis iOS 13)
  URL := 'itms-apps://apps.apple.com/app/idVOTRE_APP_ID';
  // Sur iOS, on ouvre l'URL via SharedApplication.openURL (à compléter selon
  // les unités iOS importées : iOSapi.UIKit, Macapi.Helpers, etc.).
  // SharedApplication.openURL(TNSURL.Wrap(...));
  {$ENDIF}

  {$IFDEF ANDROID}
  URL := 'market://details?id=com.votrecompagnie.votreapp';
  // L'appel à TAndroidHelper / TJIntent doit être encadré par {$IFDEF ANDROID},
  // sinon le code ne compile pas sur iOS (les unités Androidapi.* n'y existent pas).
  TAndroidHelper.Activity.startActivity(
    TJIntent.JavaClass.init(
      TJIntent.JavaClass.ACTION_VIEW,
      TJnet_Uri.JavaClass.parse(StringToJString(URL))
    )
  );
  {$ENDIF}
end;
```

---

## Partie 10 : Fonctionnalités avancées

### 10.1 Intégration Firebase

Firebase offre de nombreux services pour les apps mobiles.

#### Analytics

```pascal
uses
  FMX.Analytics, FMX.Analytics.AppAnalytics;

procedure LogScreenView(const AScreenName: string);  
begin  
  TAppAnalytics.ScreenView(AScreenName);
end;

procedure LogEvent(const AEventName: string; const AParams: TArray<string>);  
begin  
  TAppAnalytics.LogEvent(AEventName, AParams);
end;

// Utilisation
procedure TFormMain.ButtonStartClick(Sender: TObject);  
begin  
  LogEvent('activity_started', ['type', 'running']);
  StartActivity;
end;
```

#### Push Notifications

```pascal
uses
  FMX.PushNotification.FCM;

type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FPushService: TPushService;
    procedure OnReceiveNotification(Sender: TObject;
      const ANotification: TPushServiceNotification);
  end;

procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Initialiser le service
  FPushService := TPushServiceManager.Instance.GetServiceByName(
    TPushService.TServiceNames.FCM);

  if Assigned(FPushService) then
  begin
    FPushService.OnReceiveNotification := OnReceiveNotification;
    FPushService.Active := True;
  end;
end;

procedure TFormMain.OnReceiveNotification(Sender: TObject;
  const ANotification: TPushServiceNotification);
begin
  // Traiter la notification.
  // ⚠️ DataObject.GetValue(name) renvoie un TJSONValue : il faut utiliser la
  // surcharge générique GetValue<string>(name) pour obtenir directement la
  // string ; sinon la concaténation `string + TJSONValue` ne compile pas.
  ShowMessage('Notification reçue: ' +
    ANotification.DataObject.GetValue<string>('message'));
end;
```

### 10.2 Authentification biométrique

> 💡 **Note** : Depuis Delphi 10.4, le framework propose l'unité **`FMX.BiometricAuth`** avec la classe `TBiometricAuth` qui encapsule TouchID, FaceID (iOS) et BiometricPrompt (Android) sans passer par un service custom. L'exemple ci-dessous reste valide pour illustrer le pattern « service de plateforme » utilisé sur les anciennes versions ou pour vos propres extensions.

```pascal
uses
  FMX.Platform;

type
  // Interface custom pour illustrer le pattern Service de Plateforme.
  // Pensez à générer un vrai GUID via Ctrl+Maj+G dans l'IDE.
  TBiometricAuthService = interface(IInterface)
    ['{VOTRE-GUID}']
    function IsBiometricAvailable: Boolean;
    procedure Authenticate(const APrompt: string;
      ACallback: TProc<Boolean>);
  end;

procedure AuthenticateWithBiometric;  
var  
  BiometricService: TBiometricAuthService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
    TBiometricAuthService, BiometricService) then
  begin
    if BiometricService.IsBiometricAvailable then
    begin
      BiometricService.Authenticate('Authentification requise',
        procedure(ASuccess: Boolean)
        begin
          if ASuccess then
            ShowMessage('Authentification réussie')
          else
            ShowMessage('Authentification échouée');
        end);
    end;
  end;
end;
```

### 10.3 Paiements in-app

```pascal
uses
  FMX.InAppPurchase;

type
  TFormMain = class(TForm)
    InAppPurchase1: TInAppPurchase;
    procedure InAppPurchase1ProductsRequestResponse(Sender: TObject;
      const Products: TIAPProductList; const InvalidProductIDs: TStrings);
    procedure InAppPurchase1PurchaseCompleted(Sender: TObject;
      const ProductID: string; NewTransaction: Boolean);
  private
    procedure QueryProducts;
    procedure PurchaseProduct(const AProductID: string);
  end;

procedure TFormMain.QueryProducts;  
begin  
  InAppPurchase1.QueryProducts(['premium_version', 'remove_ads']);
end;

procedure TFormMain.InAppPurchase1ProductsRequestResponse(Sender: TObject;
  const Products: TIAPProductList; const InvalidProductIDs: TStrings);
var
  Product: TProduct;
begin
  for Product in Products do
  begin
    // Afficher les produits disponibles
    ShowMessage(Format('%s: %s', [Product.Title, Product.Price]));
  end;
end;

procedure TFormMain.PurchaseProduct(const AProductID: string);  
begin  
  InAppPurchase1.PurchaseProduct(AProductID);
end;

procedure TFormMain.InAppPurchase1PurchaseCompleted(Sender: TObject;
  const ProductID: string; NewTransaction: Boolean);
begin
  // Achat complété
  if ProductID = 'premium_version' then
    ActivatePremiumFeatures;
end;
```

### 10.4 Deep Linking

Permettre l'ouverture de votre app via des liens.

**Configuration iOS** — à ajouter dans le fichier `Info.plist` (XML) :

```xml
<key>CFBundleURLTypes</key>
<array>
  <dict>
    <key>CFBundleURLSchemes</key>
    <array>
      <string>votreapp</string>
    </array>
  </dict>
</array>
```

**Configuration Android** — à ajouter dans `AndroidManifest.xml`, à l'intérieur de la balise `<activity>` correspondante :

```xml
<intent-filter>
  <action android:name="android.intent.action.VIEW" />
  <category android:name="android.intent.category.DEFAULT" />
  <category android:name="android.intent.category.BROWSABLE" />
  <data android:scheme="votreapp" android:host="activity" />
</intent-filter>
```

**Gestion dans le code Delphi** :

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    procedure HandleURL(const AURL: string);
  end;

procedure TFormMain.HandleURL(const AURL: string);  
begin  
  // Exemple: votreapp://activity/123
  if AURL.StartsWith('votreapp://activity/') then
  begin
    var ActivityID := AURL.Replace('votreapp://activity/', '');
    LoadActivity(ActivityID);
  end;
end;
```

---

## Partie 11 : Débogage et maintenance

### 11.1 Crash Reporting

```pascal
uses
  System.SysUtils;

type
  TCrashReporter = class
  public
    // HandleException doit être une méthode de classe (et non une anonymous method)
    // pour pouvoir être assignée à TExceptionEvent (= procedure of object).
    class procedure HandleException(Sender: TObject; E: Exception);
    class procedure Initialize;
    class procedure LogCrash(const AException: Exception);
    class procedure SendCrashReport;
  end;

class procedure TCrashReporter.HandleException(Sender: TObject; E: Exception);  
begin  
  LogCrash(E);
end;

class procedure TCrashReporter.Initialize;  
begin  
  // Application.OnException est de type TExceptionEvent (procedure of object) :
  // on lui passe une méthode de classe, pas une anonymous method.
  Application.OnException := HandleException;
end;

class procedure TCrashReporter.LogCrash(const AException: Exception);  
var  
  LogFile: string;
  LogContent: string;
begin
  LogFile := TPath.Combine(TPath.GetDocumentsPath, 'crashes.log');

  LogContent := Format(
    '[%s] %s: %s' + sLineBreak +
    'Stack Trace:' + sLineBreak + '%s' + sLineBreak,
    [
      FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
      AException.ClassName,
      AException.Message,
      AException.StackTrace
    ]);

  TFile.AppendAllText(LogFile, LogContent);

  // Optionnel : envoyer au serveur
  SendCrashReport;
end;
```

### 11.2 Analytics et métriques

```pascal
procedure TrackUserAction(const AAction: string);  
begin  
  TAppAnalytics.LogEvent('user_action', ['action', AAction]);
end;

procedure TrackScreenTime(const AScreen: string; ADuration: Integer);  
begin  
  TAppAnalytics.LogEvent('screen_time', [
    'screen', AScreen,
    'duration', ADuration.ToString
  ]);
end;

procedure TrackError(const AError: string);  
begin  
  TAppAnalytics.LogEvent('error', ['message', AError]);
end;
```

### 11.3 Maintenance et mises à jour

#### Système de feature flags

```pascal
type
  TFeatureFlags = class
  private
    class var FFlags: TDictionary<string, Boolean>;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure LoadFromServer;
    class function IsEnabled(const AFeature: string): Boolean;
  end;

// ⚠️ Implémentation OBLIGATOIRE du class constructor : sans elle, FFlags reste
// nil et le premier appel à IsEnabled lèverait EAccessViolation (FFlags.ContainsKey
// sur un dictionnaire non initialisé). Le class constructor s'exécute
// automatiquement au chargement de l'unité, avant tout code utilisateur.
class constructor TFeatureFlags.Create;  
begin  
  FFlags := TDictionary<string, Boolean>.Create;
end;

class destructor TFeatureFlags.Destroy;  
begin  
  FFlags.Free;
end;

class procedure TFeatureFlags.LoadFromServer;  
begin  
  // Exemple : appel REST au backend pour récupérer les flags et remplir FFlags.
  // Implémentation à compléter selon votre API (TRESTClient, TNetHTTPClient…).
end;

class function TFeatureFlags.IsEnabled(const AFeature: string): Boolean;  
begin  
  if FFlags.ContainsKey(AFeature) then
    Result := FFlags[AFeature]
  else
    Result := False; // Désactivé par défaut
end;

// Utilisation
procedure TFormMain.ShowNewFeature;  
begin  
  if TFeatureFlags.IsEnabled('new_dashboard') then
    ShowNewDashboard
  else
    ShowOldDashboard;
end;
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous avez parcouru un chemin complet dans le développement d'applications mobiles avec Delphi. Vous maîtrisez maintenant :

✅ **Développement mobile natif** pour iOS et Android  
✅ **Interfaces tactiles** optimisées et adaptatives  
✅ **Accès aux capteurs** : GPS, caméra, accéléromètre  
✅ **Permissions** : Gestion appropriée et élégante  
✅ **Notifications** : Locales et push  
✅ **Stockage de données** : SQLite et préférences  
✅ **Optimisation** : Performance et batterie  
✅ **Publication** : App Store et Play Store  
✅ **Fonctionnalités avancées** : Firebase, paiements, biométrie

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Créer des applications mobiles professionnelles  
🎯 Exploiter les fonctionnalités natives des smartphones  
🎯 Gérer le cycle de vie des applications mobiles  
🎯 Optimiser pour la performance et la batterie  
🎯 Publier sur les stores officiels  
🎯 Maintenir et mettre à jour vos applications

### Ressources complémentaires

**Documentation** :
- [Delphi Mobile Development Guide](https://docwiki.embarcadero.com/RADStudio/en/Mobile_Tutorials)
- [Apple Human Interface Guidelines](https://developer.apple.com/design/human-interface-guidelines/)
- [Android Material Design](https://material.io/design)

**Communautés** :
- Stack Overflow [tag: delphi-fmx]
- Reddit r/delphi
- Forums Embarcadero

**Outils** :
- Firebase Console
- App Store Connect
- Google Play Console
- TestFlight (iOS)
- Google Play Internal Testing

### Projets suggérés pour pratiquer

1. **App de santé** : Suivi d'activité avec graphiques
2. **App de notes** : Avec synchronisation cloud
3. **App météo** : API + localisation
4. **App de photos** : Galerie avec filtres
5. **App de chat** : Messaging en temps réel
6. **App de shopping** : E-commerce avec paiements

### Prochaines étapes

Maintenant que vous maîtrisez le développement mobile, explorez :

- **19.5 Applications cloud et SaaS** : Backend pour vos apps mobiles
- **19.7 Projets IA et Machine Learning** : Intelligence artificielle mobile
- **Wearables** : Apple Watch et Wear OS

### Message final

Le développement mobile est un domaine passionnant et en constante évolution. Avec Delphi, vous disposez d'un outil puissant qui vous permet de créer des applications natives de haute qualité pour iOS et Android avec un seul code source.

**Conseils pour réussir** :
- Testez toujours sur de vrais appareils
- Suivez les guidelines de chaque plateforme
- Optimisez pour la batterie et la performance
- Écoutez les retours des utilisateurs
- Restez à jour avec les nouvelles versions d'iOS et Android

**Bon développement mobile avec Delphi !** 📱✨

---

⏭️ [Services Windows et applications de fond](/19-projets-avances/04-services-windows-et-applications-de-fond.md)
