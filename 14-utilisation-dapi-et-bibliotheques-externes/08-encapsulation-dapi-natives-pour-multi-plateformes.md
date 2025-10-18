🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 14.8 Encapsulation d'API natives pour multi-plateformes

## Introduction à l'encapsulation multi-plateformes

### Qu'est-ce que l'encapsulation multi-plateformes ?

L'**encapsulation multi-plateformes** consiste à créer une interface unique et commune pour accéder à des fonctionnalités qui sont implémentées différemment sur chaque système d'exploitation.

**Analogie :** Imaginez une télécommande universelle. Peu importe la marque de votre télévision (Samsung, LG, Sony), vous utilisez les mêmes boutons (Power, Volume, Chaîne). L'encapsulation fonctionne de la même manière : une interface unique pour différentes implémentations.

### Pourquoi encapsuler les API natives ?

**Code réutilisable** : Écrire le code une seule fois et le faire fonctionner sur toutes les plateformes.

**Maintenance simplifiée** : Les changements d'une plateforme n'affectent pas les autres.

**Abstraction** : Le code métier ne doit pas connaître les détails d'implémentation de chaque OS.

**Évolutivité** : Ajouter facilement une nouvelle plateforme sans modifier le code existant.

**Testabilité** : Plus facile de tester le code avec des implémentations mockées.

### Les plateformes supportées par Delphi

- **Windows** (32-bit et 64-bit)
- **macOS** (Intel et Apple Silicon)
- **iOS** (iPhone et iPad)
- **Android** (smartphones et tablettes)
- **Linux** (64-bit)

Chaque plateforme a ses propres API natives avec des noms, syntaxes et comportements différents.

## Directives de compilation conditionnelle

### Les directives de base

Les directives de compilation permettent d'inclure ou d'exclure du code selon la plateforme cible.

```pascal
{$IFDEF MSWINDOWS}
  // Code spécifique à Windows
{$ENDIF}

{$IFDEF MACOS}
  // Code spécifique à macOS/iOS
{$ENDIF}

{$IFDEF ANDROID}
  // Code spécifique à Android
{$ENDIF}

{$IFDEF LINUX}
  // Code spécifique à Linux
{$ENDIF}
```

### Directives détaillées

```pascal
{$IFDEF MSWINDOWS}
  // Windows (32 ou 64 bit)
  {$IFDEF WIN32}
    // Windows 32-bit uniquement
  {$ENDIF}
  {$IFDEF WIN64}
    // Windows 64-bit uniquement
  {$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  {$IFDEF IOS}
    // iOS uniquement
    {$IFDEF CPUARM}
      // iOS sur ARM (appareils réels)
    {$ENDIF}
    {$IFDEF CPUARM64}
      // iOS sur ARM64
    {$ENDIF}
  {$ELSE}
    // macOS uniquement
  {$ENDIF}
{$ENDIF}

{$IFDEF ANDROID}
  {$IFDEF ANDROID32}
    // Android 32-bit
  {$ENDIF}
  {$IFDEF ANDROID64}
    // Android 64-bit
  {$ENDIF}
{$ENDIF}
```

### Exemple pratique de directives

```pascal
function ObtenirNomPlateforme: string;
begin
  {$IFDEF MSWINDOWS}
  Result := 'Windows';
  {$ENDIF}

  {$IFDEF MACOS}
    {$IFDEF IOS}
    Result := 'iOS';
    {$ELSE}
    Result := 'macOS';
    {$ENDIF}
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := 'Android';
  {$ENDIF}

  {$IFDEF LINUX}
  Result := 'Linux';
  {$ENDIF}
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Plateforme actuelle: ' + ObtenirNomPlateforme);
end;
```

## Pattern d'encapsulation : Interface + Implémentation

### Définir une interface commune

La première étape consiste à définir une interface qui sera la même sur toutes les plateformes.

```pascal
unit Notification.Interfaces;

interface

type
  // Interface commune pour toutes les plateformes
  INotificationService = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    procedure AfficherNotification(const Titre, Message: string);
    function NotificationsAutorisees: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

end.
```

### Implémentations spécifiques par plateforme

#### Windows

```pascal
unit Notification.Windows;

interface

uses
  Notification.Interfaces, Winapi.Windows;

type
  TNotificationServiceWindows = class(TInterfacedObject, INotificationService)
  public
    procedure AfficherNotification(const Titre, Message: string);
    function NotificationsAutorisees: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

procedure TNotificationServiceWindows.AfficherNotification(
  const Titre, Message: string);
begin
  // Utiliser l'API Windows pour les notifications toast
  // Code simplifié pour l'exemple
  MessageBox(0, PChar(Message), PChar(Titre), MB_OK or MB_ICONINFORMATION);
end;

function TNotificationServiceWindows.NotificationsAutorisees: Boolean;
begin
  // Sur Windows, les notifications sont toujours autorisées
  Result := True;
end;

procedure TNotificationServiceWindows.DemanderAutorisation;
begin
  // Pas nécessaire sur Windows
end;

end.
```

#### Android

```pascal
unit Notification.Android;

interface

uses
  Notification.Interfaces,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.Helpers;

type
  TNotificationServiceAndroid = class(TInterfacedObject, INotificationService)
  public
    procedure AfficherNotification(const Titre, Message: string);
    function NotificationsAutorisees: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

procedure TNotificationServiceAndroid.AfficherNotification(
  const Titre, Message: string);
var
  NotificationBuilder: JNotificationCompat_Builder;
  NotificationManager: JNotificationManager;
begin
  // Utiliser l'API Android pour les notifications
  NotificationBuilder := TJNotificationCompat_Builder.JavaClass.init(
    TAndroidHelper.Context);

  NotificationBuilder
    .setContentTitle(StrToJCharSequence(Titre))
    .setContentText(StrToJCharSequence(Message))
    .setSmallIcon(TAndroidHelper.Context.getApplicationInfo.icon)
    .setAutoCancel(True);

  NotificationManager := TJNotificationManager.Wrap(
    TAndroidHelper.Context.getSystemService(
      TJContext.JavaClass.NOTIFICATION_SERVICE));

  NotificationManager.notify(1, NotificationBuilder.build);
end;

function TNotificationServiceAndroid.NotificationsAutorisees: Boolean;
begin
  // Vérifier les permissions Android
  Result := TAndroidHelper.Context.checkSelfPermission(
    StringToJString('android.permission.POST_NOTIFICATIONS')) =
    TJPackageManager.JavaClass.PERMISSION_GRANTED;
end;

procedure TNotificationServiceAndroid.DemanderAutorisation;
begin
  // Demander la permission à l'utilisateur
  TAndroidHelper.Activity.requestPermissions(
    TJavaObjectArray<JString>.Create(
      StringToJString('android.permission.POST_NOTIFICATIONS')), 1);
end;

end.
```

#### iOS

```pascal
unit Notification.iOS;

interface

uses
  Notification.Interfaces,
  iOSapi.UserNotifications,
  iOSapi.Foundation;

type
  TNotificationServiceiOS = class(TInterfacedObject, INotificationService)
  public
    procedure AfficherNotification(const Titre, Message: string);
    function NotificationsAutorisees: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

procedure TNotificationServiceiOS.AfficherNotification(
  const Titre, Message: string);
var
  Center: UNUserNotificationCenter;
  Content: UNMutableNotificationContent;
  Request: UNNotificationRequest;
begin
  Center := TUNUserNotificationCenter.Wrap(
    TUNUserNotificationCenter.OCClass.currentNotificationCenter);

  Content := TUNMutableNotificationContent.Create;
  Content.setTitle(StrToNSStr(Titre));
  Content.setBody(StrToNSStr(Message));

  Request := TUNNotificationRequest.Wrap(
    TUNNotificationRequest.OCClass.requestWithIdentifier(
      StrToNSStr('notification1'), Content, nil));

  Center.addNotificationRequest(Request, nil);
end;

function TNotificationServiceiOS.NotificationsAutorisees: Boolean;
var
  Center: UNUserNotificationCenter;
  Settings: UNNotificationSettings;
begin
  Center := TUNUserNotificationCenter.Wrap(
    TUNUserNotificationCenter.OCClass.currentNotificationCenter);

  // Récupérer les paramètres de manière synchrone (simplifié)
  Result := True; // Dans la vraie vie, utiliser un callback
end;

procedure TNotificationServiceiOS.DemanderAutorisation;
var
  Center: UNUserNotificationCenter;
begin
  Center := TUNUserNotificationCenter.Wrap(
    TUNUserNotificationCenter.OCClass.currentNotificationCenter);

  Center.requestAuthorizationWithOptions(
    UNAuthorizationOptionAlert or UNAuthorizationOptionSound, nil);
end;

end.
```

### Factory pour créer l'implémentation appropriée

```pascal
unit Notification.Factory;

interface

uses
  Notification.Interfaces;

type
  TNotificationFactory = class
  public
    class function Create: INotificationService;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Notification.Windows;
  {$ENDIF}
  {$IFDEF ANDROID}
  Notification.Android;
  {$ENDIF}
  {$IFDEF IOS}
  Notification.iOS;
  {$ENDIF}
  {$IFDEF MACOS}
  Notification.macOS;
  {$ENDIF}
  {$IFDEF LINUX}
  Notification.Linux;
  {$ENDIF}

class function TNotificationFactory.Create: INotificationService;
begin
  {$IFDEF MSWINDOWS}
  Result := TNotificationServiceWindows.Create;
  {$ENDIF}

  {$IFDEF ANDROID}
  Result := TNotificationServiceAndroid.Create;
  {$ENDIF}

  {$IFDEF IOS}
  Result := TNotificationServiceiOS.Create;
  {$ENDIF}

  {$IFDEF MACOS}
  Result := TNotificationServicemacOS.Create;
  {$ENDIF}

  {$IFDEF LINUX}
  Result := TNotificationServiceLinux.Create;
  {$ENDIF}
end;

end.
```

### Utilisation dans l'application

```pascal
unit MainForm;

interface

uses
  System.SysUtils, FMX.Forms, FMX.Controls, FMX.StdCtrls,
  Notification.Interfaces, Notification.Factory;

type
  TForm1 = class(TForm)
    ButtonNotifier: TButton;
    procedure ButtonNotifierClick(Sender: TObject);
  private
    FNotificationService: INotificationService;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // Créer le service approprié pour la plateforme
  FNotificationService := TNotificationFactory.Create;
end;

procedure TForm1.ButtonNotifierClick(Sender: TObject);
begin
  // Demander l'autorisation si nécessaire
  if not FNotificationService.NotificationsAutorisees then
    FNotificationService.DemanderAutorisation;

  // Afficher la notification
  FNotificationService.AfficherNotification(
    'Mon Application',
    'Ceci est une notification multi-plateformes !'
  );
end;

end.
```

## Exemple complet : Service de géolocalisation

### Interface commune

```pascal
unit Location.Interfaces;

interface

type
  TCoordonnees = record
    Latitude: Double;
    Longitude: Double;
    Altitude: Double;
    Precision: Double;
  end;

  TLocationCallback = reference to procedure(const Coordonnees: TCoordonnees);
  TLocationErrorCallback = reference to procedure(const Erreur: string);

  ILocationService = interface
    ['{87654321-4321-4321-4321-210987654321}']
    procedure DemanderPosition(OnSuccess: TLocationCallback;
      OnError: TLocationErrorCallback);
    procedure DemarrerSuiviPosition(OnUpdate: TLocationCallback);
    procedure ArreterSuiviPosition;
    function AutorisationObtenue: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

end.
```

### Implémentation Windows

```pascal
unit Location.Windows;

interface

uses
  Location.Interfaces, Winapi.Windows;

type
  TLocationServiceWindows = class(TInterfacedObject, ILocationService)
  public
    procedure DemanderPosition(OnSuccess: TLocationCallback;
      OnError: TLocationErrorCallback);
    procedure DemarrerSuiviPosition(OnUpdate: TLocationCallback);
    procedure ArreterSuiviPosition;
    function AutorisationObtenue: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

uses
  System.Sensors;

procedure TLocationServiceWindows.DemanderPosition(
  OnSuccess: TLocationCallback; OnError: TLocationErrorCallback);
var
  LocationSensor: TLocationSensor;
  Coord: TCoordonnees;
begin
  LocationSensor := TLocationSensor.Create(nil);
  try
    if LocationSensor.Authorized then
    begin
      LocationSensor.Active := True;

      Coord.Latitude := LocationSensor.Latitude;
      Coord.Longitude := LocationSensor.Longitude;
      Coord.Altitude := 0;
      Coord.Precision := 10;

      OnSuccess(Coord);
    end
    else
      OnError('Autorisation refusée');
  finally
    LocationSensor.Free;
  end;
end;

procedure TLocationServiceWindows.DemarrerSuiviPosition(
  OnUpdate: TLocationCallback);
begin
  // Implémentation du suivi continu
end;

procedure TLocationServiceWindows.ArreterSuiviPosition;
begin
  // Arrêter le suivi
end;

function TLocationServiceWindows.AutorisationObtenue: Boolean;
begin
  Result := True; // Windows ne nécessite pas d'autorisation spéciale
end;

procedure TLocationServiceWindows.DemanderAutorisation;
begin
  // Pas nécessaire sur Windows
end;

end.
```

### Implémentation Android

```pascal
unit Location.Android;

interface

uses
  Location.Interfaces,
  Androidapi.JNI.Location,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers;

type
  TLocationServiceAndroid = class(TInterfacedObject, ILocationService)
  private
    FLocationManager: JLocationManager;
    FLocationListener: JLocationListener;
    FOnUpdate: TLocationCallback;
  public
    constructor Create;
    procedure DemanderPosition(OnSuccess: TLocationCallback;
      OnError: TLocationErrorCallback);
    procedure DemarrerSuiviPosition(OnUpdate: TLocationCallback);
    procedure ArreterSuiviPosition;
    function AutorisationObtenue: Boolean;
    procedure DemanderAutorisation;
  end;

implementation

constructor TLocationServiceAndroid.Create;
begin
  inherited;
  FLocationManager := TJLocationManager.Wrap(
    TAndroidHelper.Context.getSystemService(
      TJContext.JavaClass.LOCATION_SERVICE));
end;

procedure TLocationServiceAndroid.DemanderPosition(
  OnSuccess: TLocationCallback; OnError: TLocationErrorCallback);
var
  LastKnownLocation: JLocation;
  Coord: TCoordonnees;
begin
  if not AutorisationObtenue then
  begin
    OnError('Permission de localisation non accordée');
    Exit;
  end;

  LastKnownLocation := FLocationManager.getLastKnownLocation(
    TJLocationManager.JavaClass.GPS_PROVIDER);

  if LastKnownLocation <> nil then
  begin
    Coord.Latitude := LastKnownLocation.getLatitude;
    Coord.Longitude := LastKnownLocation.getLongitude;
    Coord.Altitude := LastKnownLocation.getAltitude;
    Coord.Precision := LastKnownLocation.getAccuracy;

    OnSuccess(Coord);
  end
  else
    OnError('Position non disponible');
end;

procedure TLocationServiceAndroid.DemarrerSuiviPosition(
  OnUpdate: TLocationCallback);
begin
  FOnUpdate := OnUpdate;

  // Créer un listener et commencer les mises à jour
  // Code simplifié
  FLocationManager.requestLocationUpdates(
    TJLocationManager.JavaClass.GPS_PROVIDER,
    1000,  // Intervalle en ms
    10,    // Distance minimale en mètres
    FLocationListener);
end;

procedure TLocationServiceAndroid.ArreterSuiviPosition;
begin
  if FLocationListener <> nil then
    FLocationManager.removeUpdates(FLocationListener);
end;

function TLocationServiceAndroid.AutorisationObtenue: Boolean;
begin
  Result := TAndroidHelper.Context.checkSelfPermission(
    StringToJString('android.permission.ACCESS_FINE_LOCATION')) =
    TJPackageManager.JavaClass.PERMISSION_GRANTED;
end;

procedure TLocationServiceAndroid.DemanderAutorisation;
begin
  TAndroidHelper.Activity.requestPermissions(
    TJavaObjectArray<JString>.Create(
      StringToJString('android.permission.ACCESS_FINE_LOCATION')), 100);
end;

end.
```

## Gestion des fonctionnalités manquantes

### Vérifier la disponibilité d'une fonctionnalité

```pascal
unit Feature.Checker;

interface

type
  TFeatureChecker = class
  public
    class function BluetoothDisponible: Boolean;
    class function NfcDisponible: Boolean;
    class function BiometrieDisponible: Boolean;
    class function CameraDisponible: Boolean;
  end;

implementation

class function TFeatureChecker.BluetoothDisponible: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := True; // À implémenter correctement
  {$ENDIF}

  {$IFDEF ANDROID}
  // Vérifier si l'appareil a Bluetooth
  Result := TAndroidHelper.Context.getPackageManager.hasSystemFeature(
    StringToJString('android.hardware.bluetooth'));
  {$ENDIF}

  {$IFDEF IOS}
  Result := True; // Tous les appareils iOS ont Bluetooth
  {$ENDIF}
end;

class function TFeatureChecker.CameraDisponible: Boolean;
begin
  {$IFDEF ANDROID}
  Result := TAndroidHelper.Context.getPackageManager.hasSystemFeature(
    StringToJString('android.hardware.camera'));
  {$ENDIF}

  {$IFDEF IOS}
  Result := True; // Tous les appareils iOS ont une caméra
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Result := False; // Dépend du matériel
  {$ENDIF}
end;

end.
```

### Interface avec fonctionnalités optionnelles

```pascal
unit Camera.Interfaces;

interface

type
  ICameraService = interface
    ['{11111111-2222-3333-4444-555555555555}']
    function Disponible: Boolean;
    procedure PrendrePhoto(OnSuccess: TProc<TBitmap>; OnError: TProc<string>);
    function FlashDisponible: Boolean;
    procedure ActiverFlash(Activer: Boolean);
  end;

implementation

end.
```

### Implémentation avec gestion élégante des limitations

```pascal
unit Camera.Windows;

interface

uses
  Camera.Interfaces, FMX.Graphics;

type
  TCameraServiceWindows = class(TInterfacedObject, ICameraService)
  public
    function Disponible: Boolean;
    procedure PrendrePhoto(OnSuccess: TProc<TBitmap>; OnError: TProc<string>);
    function FlashDisponible: Boolean;
    procedure ActiverFlash(Activer: Boolean);
  end;

implementation

function TCameraServiceWindows.Disponible: Boolean;
begin
  // Sur Windows, la caméra n'est pas toujours disponible
  Result := False; // Vérification simplifiée
end;

procedure TCameraServiceWindows.PrendrePhoto(OnSuccess: TProc<TBitmap>;
  OnError: TProc<string>);
begin
  if not Disponible then
  begin
    OnError('Caméra non disponible sur cette plateforme');
    Exit;
  end;

  // Implémentation pour Windows
end;

function TCameraServiceWindows.FlashDisponible: Boolean;
begin
  Result := False; // Rarement disponible sur PC
end;

procedure TCameraServiceWindows.ActiverFlash(Activer: Boolean);
begin
  // Ne fait rien sur Windows
end;

end.
```

## Pattern adaptateur pour API existantes

### Adapter l'API Delphi aux besoins métier

```pascal
unit Storage.Interfaces;

interface

type
  IStorageService = interface
    ['{22222222-3333-4444-5555-666666666666}']
    procedure Sauvegarder(const Cle, Valeur: string);
    function Charger(const Cle: string): string;
    procedure Supprimer(const Cle: string);
    function Existe(const Cle: string): Boolean;
  end;

implementation

end.
```

### Implémentation Windows (Registry)

```pascal
unit Storage.Windows;

interface

uses
  Storage.Interfaces, System.Win.Registry;

type
  TStorageServiceWindows = class(TInterfacedObject, IStorageService)
  private
    const REG_PATH = 'Software\MonApplication';
  public
    procedure Sauvegarder(const Cle, Valeur: string);
    function Charger(const Cle: string): string;
    procedure Supprimer(const Cle: string);
    function Existe(const Cle: string): Boolean;
  end;

implementation

procedure TStorageServiceWindows.Sauvegarder(const Cle, Valeur: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_PATH, True) then
    begin
      Reg.WriteString(Cle, Valeur);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TStorageServiceWindows.Charger(const Cle: string): string;
var
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(REG_PATH) then
    begin
      if Reg.ValueExists(Cle) then
        Result := Reg.ReadString(Cle);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TStorageServiceWindows.Supprimer(const Cle: string);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(REG_PATH, False) then
    begin
      Reg.DeleteValue(Cle);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TStorageServiceWindows.Existe(const Cle: string): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKeyReadOnly(REG_PATH) then
    begin
      Result := Reg.ValueExists(Cle);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

end.
```

### Implémentation mobile (Fichiers)

```pascal
unit Storage.Mobile;

interface

uses
  Storage.Interfaces, System.SysUtils, System.IOUtils, System.IniFiles;

type
  TStorageServiceMobile = class(TInterfacedObject, IStorageService)
  private
    function ObtenirCheminFichier: string;
  public
    procedure Sauvegarder(const Cle, Valeur: string);
    function Charger(const Cle: string): string;
    procedure Supprimer(const Cle: string);
    function Existe(const Cle: string): Boolean;
  end;

implementation

function TStorageServiceMobile.ObtenirCheminFichier: string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, 'app_settings.ini');
end;

procedure TStorageServiceMobile.Sauvegarder(const Cle, Valeur: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ObtenirCheminFichier);
  try
    IniFile.WriteString('Settings', Cle, Valeur);
  finally
    IniFile.Free;
  end;
end;

function TStorageServiceMobile.Charger(const Cle: string): string;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ObtenirCheminFichier);
  try
    Result := IniFile.ReadString('Settings', Cle, '');
  finally
    IniFile.Free;
  end;
end;

procedure TStorageServiceMobile.Supprimer(const Cle: string);
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ObtenirCheminFichier);
  try
    IniFile.DeleteKey('Settings', Cle);
  finally
    IniFile.Free;
  end;
end;

function TStorageServiceMobile.Existe(const Cle: string): Boolean;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(ObtenirCheminFichier);
  try
    Result := IniFile.ValueExists('Settings', Cle);
  finally
    IniFile.Free;
  end;
end;

end.
```

## Service d'authentification biométrique

### Interface commune

```pascal
unit Biometric.Interfaces;

interface

type
  TBiometricType = (btFingerprint, btFaceID, btNone);

  TBiometricCallback = reference to procedure(Success: Boolean; const Erreur: string);

  IBiometricService = interface
    ['{33333333-4444-5555-6666-777777777777}']
    function Disponible: Boolean;
    function TypeBiometrique: TBiometricType;
    procedure Authentifier(const Raison: string; Callback: TBiometricCallback);
  end;

implementation

end.
```

### Implémentation Android

```pascal
unit Biometric.Android;

interface

uses
  Biometric.Interfaces,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Hardware,
  Androidapi.Helpers;

type
  TBiometricServiceAndroid = class(TInterfacedObject, IBiometricService)
  private
    FFingerprintManager: JFingerprintManager;
  public
    constructor Create;
    function Disponible: Boolean;
    function TypeBiometrique: TBiometricType;
    procedure Authentifier(const Raison: string; Callback: TBiometricCallback);
  end;

implementation

constructor TBiometricServiceAndroid.Create;
begin
  inherited;
  FFingerprintManager := TJFingerprintManager.Wrap(
    TAndroidHelper.Context.getSystemService(
      TJContext.JavaClass.FINGERPRINT_SERVICE));
end;

function TBiometricServiceAndroid.Disponible: Boolean;
begin
  Result := (FFingerprintManager <> nil) and
            FFingerprintManager.isHardwareDetected and
            FFingerprintManager.hasEnrolledFingerprints;
end;

function TBiometricServiceAndroid.TypeBiometrique: TBiometricType;
begin
  if Disponible then
    Result := btFingerprint
  else
    Result := btNone;
end;

procedure TBiometricServiceAndroid.Authentifier(const Raison: string;
  Callback: TBiometricCallback);
begin
  if not Disponible then
  begin
    Callback(False, 'Biométrie non disponible');
    Exit;
  end;

  // Implémenter l'authentification
  // Code simplifié pour l'exemple
end;

end.
```

### Implémentation iOS

```pascal
unit Biometric.iOS;

interface

uses
  Biometric.Interfaces,
  iOSapi.LocalAuthentication,
  iOSapi.Foundation;

type
  TBiometricServiceiOS = class(TInterfacedObject, IBiometricService)
  private
    FContext: LAContext;
  public
    constructor Create;
    destructor Destroy; override;
    function Disponible: Boolean;
    function TypeBiometrique: TBiometricType;
    procedure Authentifier(const Raison: string; Callback: TBiometricCallback);
  end;

implementation

constructor TBiometricServiceiOS.Create;
begin
  inherited;
  FContext := TLAContext.Create;
end;

destructor TBiometricServiceiOS.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TBiometricServiceiOS.Disponible: Boolean;
var
  Error: NSError;
begin
  Result := FContext.canEvaluatePolicy(
    LAPolicyDeviceOwnerAuthenticationWithBiometrics, @Error);
end;

function TBiometricServiceiOS.TypeBiometrique: TBiometricType;
begin
  if not Disponible then
    Exit(btNone);

  case FContext.biometryType of
    LABiometryTypeFaceID: Result := btFaceID;
    LABiometryTypeTouchID: Result := btFingerprint;
  else
    Result := btNone;
  end;
end;

procedure TBiometricServiceiOS.Authentifier(const Raison: string;
  Callback: TBiometricCallback);
begin
  if not Disponible then
  begin
    Callback(False, 'Biométrie non disponible');
    Exit;
  end;

  FContext.evaluatePolicy(
    LAPolicyDeviceOwnerAuthenticationWithBiometrics,
    StrToNSStr(Raison),
    procedure(success: Boolean; error: NSError)
    begin
      if success then
        Callback(True, '')
      else
        Callback(False, NSStrToStr(error.localizedDescription));
    end);
end;

end.
```

## Tests de compatibilité

### Classe de test multi-plateformes

```pascal
unit Platform.Tests;

interface

type
  TPlatformTests = class
  public
    class procedure TesterNotifications;
    class procedure TesterLocalisation;
    class procedure TesterStockage;
    class procedure TesterBiometrie;
    class procedure TesterToutesLesFonctionnalites;
  end;

implementation

uses
  Notification.Factory, Location.Factory, Storage.Factory, Biometric.Factory;

class procedure TPlatformTests.TesterNotifications;
var
  Service: INotificationService;
begin
  Service := TNotificationFactory.Create;

  if Service.NotificationsAutorisees then
    Service.AfficherNotification('Test', 'Les notifications fonctionnent!')
  else
  begin
    Service.DemanderAutorisation;
    ShowMessage('Autorisation demandée pour les notifications');
  end;
end;

class procedure TPlatformTests.TesterLocalisation;
var
  Service: ILocationService;
begin
  Service := TLocationFactory.Create;

  if Service.AutorisationObtenue then
  begin
    Service.DemanderPosition(
      procedure(const Coord: TCoordonnees)
      begin
        ShowMessage(Format('Position: %.6f, %.6f',
          [Coord.Latitude, Coord.Longitude]));
      end,
      procedure(const Erreur: string)
      begin
        ShowMessage('Erreur: ' + Erreur);
      end);
  end
  else
    Service.DemanderAutorisation;
end;

class procedure TPlatformTests.TesterStockage;
var
  Service: IStorageService;
  Valeur: string;
begin
  Service := TStorageFactory.Create;

  Service.Sauvegarder('test_key', 'test_value');
  Valeur := Service.Charger('test_key');

  if Valeur = 'test_value' then
    ShowMessage('Stockage fonctionne!')
  else
    ShowMessage('Problème avec le stockage');
end;

class procedure TPlatformTests.TesterBiometrie;
var
  Service: IBiometricService;
begin
  Service := TBiometricFactory.Create;

  if Service.Disponible then
  begin
    ShowMessage('Type: ' + GetEnumName(TypeInfo(TBiometricType),
      Ord(Service.TypeBiometrique)));

    Service.Authentifier('Test d''authentification',
      procedure(Success: Boolean; const Erreur: string)
      begin
        if Success then
          ShowMessage('Authentification réussie!')
        else
          ShowMessage('Échec: ' + Erreur);
      end);
  end
  else
    ShowMessage('Biométrie non disponible');
end;

class procedure TPlatformTests.TesterToutesLesFonctionnalites;
begin
  TesterNotifications;
  TesterLocalisation;
  TesterStockage;
  TesterBiometrie;
end;

end.
```

## Bonnes pratiques

### Organisation du code

```
MonProjet/
├── Interfaces/
│   ├── Notification.Interfaces.pas
│   ├── Location.Interfaces.pas
│   ├── Storage.Interfaces.pas
│   └── Biometric.Interfaces.pas
├── Implementations/
│   ├── Windows/
│   │   ├── Notification.Windows.pas
│   │   ├── Location.Windows.pas
│   │   └── Storage.Windows.pas
│   ├── Android/
│   │   ├── Notification.Android.pas
│   │   ├── Location.Android.pas
│   │   └── Biometric.Android.pas
│   ├── iOS/
│   │   ├── Notification.iOS.pas
│   │   ├── Location.iOS.pas
│   │   └── Biometric.iOS.pas
│   └── macOS/
│       └── ...
├── Factories/
│   ├── Notification.Factory.pas
│   ├── Location.Factory.pas
│   └── Storage.Factory.pas
└── Tests/
    └── Platform.Tests.pas
```

### Principes de conception

**1. Séparation des responsabilités** : Une interface = une responsabilité claire.

**2. Dépendance aux abstractions** : Le code métier dépend des interfaces, pas des implémentations.

**3. Remplacement de Liskov** : Toutes les implémentations doivent être interchangeables.

**4. Interface Segregation** : Préférer plusieurs petites interfaces à une grande.

**5. Inversion de dépendance** : Utiliser des factories pour créer les instances.

### Documentation

```pascal
/// <summary>
/// Service de notifications multi-plateformes
/// </summary>
/// <remarks>
/// Plateformes supportées:
/// - Windows: Notifications toast via API Windows 10+
/// - Android: Notifications via NotificationManager (API 23+)
/// - iOS: Notifications locales via UserNotifications (iOS 10+)
/// - macOS: Notifications via NSUserNotificationCenter
/// </remarks>
type
  INotificationService = interface
    /// <summary>
    /// Affiche une notification à l'utilisateur
    /// </summary>
    /// <param name="Titre">Titre de la notification</param>
    /// <param name="Message">Corps de la notification</param>
    procedure AfficherNotification(const Titre, Message: string);
  end;
```

### Gestion des versions d'API

```pascal
unit Platform.Version;

interface

type
  TPlatformVersion = record
    Major: Integer;
    Minor: Integer;
    Build: Integer;
    function EstCompatible(MajorMin, MinorMin: Integer): Boolean;
  end;

function ObtenirVersionPlateforme: TPlatformVersion;

implementation

uses
  {$IFDEF ANDROID}
  Androidapi.Helpers;
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.Foundation;
  {$ENDIF}

function TPlatformVersion.EstCompatible(MajorMin, MinorMin: Integer): Boolean;
begin
  Result := (Major > MajorMin) or
            ((Major = MajorMin) and (Minor >= MinorMin));
end;

function ObtenirVersionPlateforme: TPlatformVersion;
begin
  {$IFDEF ANDROID}
  Result.Major := TAndroidHelper.Context.getApplicationInfo.targetSdkVersion;
  Result.Minor := 0;
  Result.Build := 0;
  {$ENDIF}

  {$IFDEF IOS}
  // Récupérer la version iOS
  Result.Major := 14; // Exemple
  Result.Minor := 0;
  Result.Build := 0;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  Result.Major := 10;
  Result.Minor := 0;
  Result.Build := 0;
  {$ENDIF}
end;

// Utilisation
if ObtenirVersionPlateforme.EstCompatible(10, 0) then
  // Utiliser une fonctionnalité nécessitant version 10.0+
end.
```

## Résumé

L'encapsulation d'API natives permet de créer des applications vraiment multi-plateformes.

**Points clés :**

1. **Interface commune** : Définir une abstraction indépendante de la plateforme
2. **Implémentations spécifiques** : Une classe par plateforme avec le code natif
3. **Factory Pattern** : Créer automatiquement la bonne implémentation
4. **Directives conditionnelles** : {$IFDEF} pour inclure le code spécifique
5. **Fonctionnalités optionnelles** : Gérer élégamment les API manquantes
6. **Tests** : Tester sur chaque plateforme cible
7. **Organisation** : Structure de dossiers claire par plateforme
8. **Documentation** : Documenter les différences et limitations
9. **Versions** : Gérer les différentes versions d'API
10. **Maintenance** : Faciliter l'ajout de nouvelles plateformes

L'encapsulation demande plus de travail initial mais offre :
- Code métier propre et lisible
- Maintenance simplifiée
- Tests plus faciles
- Évolution facilitée
- Ajout de plateformes sans refonte

C'est un investissement qui se rentabilise rapidement sur des projets multi-plateformes sérieux.

⏭️ [Intégration de bibliothèques JavaScript via WebView](/14-utilisation-dapi-et-bibliotheques-externes/09-integration-de-bibliotheques-javascript-via-webview.md)
