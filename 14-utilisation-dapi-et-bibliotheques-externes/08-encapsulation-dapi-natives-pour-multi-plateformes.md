# 14.8 Encapsulation d'API natives pour multi-plateformes

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Avec Delphi, il est possible de créer des applications qui fonctionnent sur plusieurs plateformes comme Windows, macOS, iOS, Android et Linux. Cependant, chaque système d'exploitation dispose de ses propres API natives qui offrent des fonctionnalités spécifiques à la plateforme.

L'encapsulation d'API natives consiste à créer une interface commune qui masque les différences entre les plateformes, permettant ainsi d'écrire du code qui fonctionne partout, tout en exploitant les fonctionnalités spécifiques de chaque système.

Dans ce chapitre, nous allons explorer comment encapsuler des API natives pour créer des applications véritablement multi-plateformes avec Delphi.

## Pourquoi encapsuler les API natives ?

Voici quelques raisons pour lesquelles vous pourriez avoir besoin d'encapsuler des API natives :

1. **Accès à des fonctionnalités non disponibles dans la FMX** : Bien que FireMonkey (FMX) offre de nombreuses fonctionnalités multi-plateformes, certaines capacités avancées des systèmes d'exploitation ne sont pas directement accessibles.

2. **Performances optimales** : Les API natives sont généralement plus performantes que les couches d'abstraction.

3. **Intégration avec les services du système** : Pour une intégration fluide avec les notifications, les contacts, la géolocalisation, etc.

4. **Expérience utilisateur cohérente** : Suivre les conventions propres à chaque plateforme tout en maintenant une logique d'application unifiée.

## Principes de base de l'encapsulation

### 1. L'approche d'interface commune

Le principe fondamental est de définir une interface commune qui sera implémentée différemment pour chaque plateforme :

```pascal
type
  IDeviceService = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function GetDeviceID: string;
    function GetOSVersion: string;
    procedure Vibrate(DurationMS: Integer);
    function HasFlash: Boolean;
    procedure TurnFlashlight(On: Boolean);
  end;
```

### 2. Implémentations spécifiques à la plateforme

Ensuite, vous créez des implémentations spécifiques à chaque plateforme :

```pascal
// Fichier conditionnel pour Windows
{$IF DEFINED(MSWINDOWS)}
type
  TWindowsDeviceService = class(TInterfacedObject, IDeviceService)
  public
    function GetDeviceID: string;
    function GetOSVersion: string;
    procedure Vibrate(DurationMS: Integer);
    function HasFlash: Boolean;
    procedure TurnFlashlight(On: Boolean);
  end;
{$ENDIF}

// Fichier conditionnel pour Android
{$IF DEFINED(ANDROID)}
type
  TAndroidDeviceService = class(TInterfacedObject, IDeviceService)
  public
    function GetDeviceID: string;
    function GetOSVersion: string;
    procedure Vibrate(DurationMS: Integer);
    function HasFlash: Boolean;
    procedure TurnFlashlight(On: Boolean);
  end;
{$ENDIF}

// Fichier conditionnel pour iOS
{$IF DEFINED(IOS)}
type
  TiOSDeviceService = class(TInterfacedObject, IDeviceService)
  public
    function GetDeviceID: string;
    function GetOSVersion: string;
    procedure Vibrate(DurationMS: Integer);
    function HasFlash: Boolean;
    procedure TurnFlashlight(On: Boolean);
  end;
{$ENDIF}
```

### 3. Factory pour créer l'implémentation appropriée

Un "factory" (fabrique) détermine quelle implémentation utiliser selon la plateforme :

```pascal
function GetDeviceService: IDeviceService;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TWindowsDeviceService.Create;
{$ELSEIF DEFINED(ANDROID)}
  Result := TAndroidDeviceService.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSDeviceService.Create;
{$ELSE}
  // Implémentation par défaut ou génération d'erreur
  raise Exception.Create('Plateforme non supportée');
{$ENDIF}
end;
```

### 4. Utilisation dans le code partagé

Dans votre code commun, vous utilisez l'interface sans vous soucier de la plateforme :

```pascal
procedure TForm1.ButtonVibrateClick(Sender: TObject);
var
  DeviceService: IDeviceService;
begin
  DeviceService := GetDeviceService;
  DeviceService.Vibrate(500); // Vibre pendant 500ms
end;

procedure TForm1.ButtonFlashlightClick(Sender: TObject);
var
  DeviceService: IDeviceService;
begin
  DeviceService := GetDeviceService;

  if DeviceService.HasFlash then
    DeviceService.TurnFlashlight(not FlashlightOn)
  else
    ShowMessage('Pas de flash disponible sur cet appareil');
end;
```

## Exemple 1 : Encapsulation des notifications push

Voici un exemple plus complet d'encapsulation des notifications push :

### 1. Interface commune pour les notifications

```pascal
type
  TPushNotificationData = record
    Title: string;
    Message: string;
    Data: string; // Données JSON supplémentaires
  end;

  TPushNotificationEvent = procedure(const Data: TPushNotificationData) of object;

  IPushNotificationService = interface
    ['{98765432-1234-5678-9012-123456789ABC}']
    // Initialisation du service
    function Initialize: Boolean;

    // Enregistrement pour recevoir les notifications
    function RegisterForPushNotifications: Boolean;

    // Désenregistrement
    procedure UnregisterForPushNotifications;

    // Vérifier si les notifications sont autorisées
    function AreNotificationsEnabled: Boolean;

    // Ouvrir les paramètres de l'application
    procedure OpenNotificationSettings;

    // Événements
    procedure SetOnNotificationReceived(const Handler: TPushNotificationEvent);
    procedure SetOnTokenReceived(const Handler: TProc<string>);
  end;
```

### 2. Implémentation pour Android

```pascal
{$IF DEFINED(ANDROID)}
uses
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Androidapi.Helpers,
  Androidapi.JNI.Os;

type
  TAndroidPushNotificationService = class(TInterfacedObject, IPushNotificationService)
  private
    FOnNotificationReceived: TPushNotificationEvent;
    FOnTokenReceived: TProc<string>;
    FFirebaseMessaging: JFirebaseMessaging;
    FNotificationManager: JNotificationManager;

    // Gestionnaire de notifications Android
    procedure HandleNotificationReceived(const Intent: JIntent);
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface
    function Initialize: Boolean;
    function RegisterForPushNotifications: Boolean;
    procedure UnregisterForPushNotifications;
    function AreNotificationsEnabled: Boolean;
    procedure OpenNotificationSettings;

    procedure SetOnNotificationReceived(const Handler: TPushNotificationEvent);
    procedure SetOnTokenReceived(const Handler: TProc<string>);
  end;

constructor TAndroidPushNotificationService.Create;
begin
  inherited;

  // Obtenir le gestionnaire de notifications Android
  FNotificationManager := TJNotificationManager.Wrap(
    TAndroidHelper.Context.getSystemService(TJContext.JavaClass.NOTIFICATION_SERVICE));
end;

function TAndroidPushNotificationService.Initialize: Boolean;
var
  Intent: JIntent;
begin
  Result := False;

  try
    // Initialiser Firebase Messaging
    FFirebaseMessaging := TJFirebaseMessaging.JavaClass.getInstance;

    // Enregistrer le receveur de notifications
    Intent := TJIntent.JavaClass.init(StringToJString('com.yourapp.NOTIFICATION_RECEIVED'));
    TAndroidHelper.Context.registerReceiver(
      TJBroadcastReceiver.Create(HandleNotificationReceived),
      TJIntentFilter.JavaClass.init(Intent.getAction));

    Result := True;
  except
    on E: Exception do
      Log.d('PushNotification', 'Erreur d''initialisation: ' + E.Message);
  end;
end;

function TAndroidPushNotificationService.RegisterForPushNotifications: Boolean;
begin
  Result := False;

  if not Assigned(FFirebaseMessaging) then
    Exit;

  try
    // Demander le jeton FCM
    FFirebaseMessaging.getToken.addOnCompleteListener(
      procedure(Task: JTask)
      begin
        if Task.isSuccessful then
        begin
          var Token := JString(Task.getResult).toString;

          if Assigned(FOnTokenReceived) then
            FOnTokenReceived(JStringToString(Token));

          Result := True;
        end;
      end);
  except
    on E: Exception do
      Log.d('PushNotification', 'Erreur d''enregistrement: ' + E.Message);
  end;
end;

function TAndroidPushNotificationService.AreNotificationsEnabled: Boolean;
begin
  Result := FNotificationManager.areNotificationsEnabled;
end;

procedure TAndroidPushNotificationService.OpenNotificationSettings;
var
  Intent: JIntent;
begin
  // Ouvrir les paramètres de notification de l'application
  Intent := TJIntent.JavaClass.init(StringToJString('android.settings.APP_NOTIFICATION_SETTINGS'));
  Intent.putExtra(StringToJString('android.provider.extra.APP_PACKAGE'),
                 StringToJString(TAndroidHelper.Context.getPackageName));
  Intent.setFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK);

  TAndroidHelper.Context.startActivity(Intent);
end;

procedure TAndroidPushNotificationService.HandleNotificationReceived(const Intent: JIntent);
var
  NotificationData: TPushNotificationData;
  Bundle: JBundle;
begin
  if not Assigned(FOnNotificationReceived) then
    Exit;

  Bundle := Intent.getExtras;
  if Bundle <> nil then
  begin
    NotificationData.Title := JStringToString(Bundle.getString(StringToJString('title')));
    NotificationData.Message := JStringToString(Bundle.getString(StringToJString('message')));
    NotificationData.Data := JStringToString(Bundle.getString(StringToJString('data')));

    FOnNotificationReceived(NotificationData);
  end;
end;
{$ENDIF}
```

### 3. Implémentation pour iOS

```pascal
{$IF DEFINED(IOS)}
uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit,
  iOSapi.UserNotifications;

type
  TiOSPushNotificationService = class(TInterfacedObject, IPushNotificationService)
  private
    FOnNotificationReceived: TPushNotificationEvent;
    FOnTokenReceived: TProc<string>;
    FUserNotificationCenter: UNUserNotificationCenter;

    // Délégué Objective-C pour les notifications
    procedure HandleNotificationReceived(notification: UNNotification);
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface
    function Initialize: Boolean;
    function RegisterForPushNotifications: Boolean;
    procedure UnregisterForPushNotifications;
    function AreNotificationsEnabled: Boolean;
    procedure OpenNotificationSettings;

    procedure SetOnNotificationReceived(const Handler: TPushNotificationEvent);
    procedure SetOnTokenReceived(const Handler: TProc<string>);
  end;

constructor TiOSPushNotificationService.Create;
begin
  inherited;

  // Obtenir le centre de notifications
  FUserNotificationCenter := TUNUserNotificationCenter.OCClass.currentNotificationCenter;
end;

function TiOSPushNotificationService.Initialize: Boolean;
begin
  Result := False;

  if not Assigned(FUserNotificationCenter) then
    Exit;

  // Configurer le délégué pour les notifications
  FUserNotificationCenter.setDelegate(Self);

  Result := True;
end;

function TiOSPushNotificationService.RegisterForPushNotifications: Boolean;
var
  Options: UNAuthorizationOptions;
begin
  Result := False;

  if not Assigned(FUserNotificationCenter) then
    Exit;

  // Demander l'autorisation pour les notifications
  Options := UNAuthorizationOptionAlert or
             UNAuthorizationOptionSound or
             UNAuthorizationOptionBadge;

  FUserNotificationCenter.requestAuthorizationWithOptions(Options,
    procedure(granted: Boolean; error: NSError)
    begin
      if granted then
      begin
        // Enregistrer pour les notifications distantes
        TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication).
          registerForRemoteNotifications;

        Result := True;
      end;
    end);
end;

function TiOSPushNotificationService.AreNotificationsEnabled: Boolean;
var
  Settings: UNNotificationSettings;
  Semaphore: dispatch_semaphore_t;
  Result: Boolean;
begin
  Result := False;
  Semaphore := dispatch_semaphore_create(0);

  FUserNotificationCenter.getNotificationSettingsWithCompletionHandler(
    procedure(settings: UNNotificationSettings)
    begin
      Result := (settings.authorizationStatus = UNAuthorizationStatusAuthorized);
      dispatch_semaphore_signal(Semaphore);
    end);

  dispatch_semaphore_wait(Semaphore, DISPATCH_TIME_FOREVER);
  dispatch_release(Semaphore);
end;

procedure TiOSPushNotificationService.OpenNotificationSettings;
var
  URL: NSURL;
begin
  // Ouvrir les paramètres de l'application
  URL := TNSURL.Wrap(TNSURL.OCClass.URLWithString(
    StrToNSStr('App-Prefs:root=NOTIFICATIONS_ID&path=YOUR_APP_BUNDLE_ID')));

  TUIApplication.Wrap(TUIApplication.OCClass.sharedApplication).
    openURL(URL);
end;

procedure TiOSPushNotificationService.HandleNotificationReceived(notification: UNNotification);
var
  NotificationData: TPushNotificationData;
  UserInfo: NSDictionary;
begin
  if not Assigned(FOnNotificationReceived) then
    Exit;

  UserInfo := notification.request.content.userInfo;

  NotificationData.Title := NSStrToStr(TNSString.Wrap(UserInfo.objectForKey(StrToNSStr('title'))));
  NotificationData.Message := NSStrToStr(TNSString.Wrap(UserInfo.objectForKey(StrToNSStr('message'))));
  NotificationData.Data := NSStrToStr(TNSString.Wrap(UserInfo.objectForKey(StrToNSStr('data'))));

  FOnNotificationReceived(NotificationData);
end;
{$ENDIF}
```

### 4. Factory pour les notifications

```pascal
unit PushNotificationService;

interface

uses
  System.SysUtils, System.Classes, System.JSON;

type
  TPushNotificationData = record
    Title: string;
    Message: string;
    Data: string;
  end;

  TPushNotificationEvent = procedure(const Data: TPushNotificationData) of object;

  IPushNotificationService = interface
    ['{98765432-1234-5678-9012-123456789ABC}']
    function Initialize: Boolean;
    function RegisterForPushNotifications: Boolean;
    procedure UnregisterForPushNotifications;
    function AreNotificationsEnabled: Boolean;
    procedure OpenNotificationSettings;

    procedure SetOnNotificationReceived(const Handler: TPushNotificationEvent);
    procedure SetOnTokenReceived(const Handler: TProc<string>);
  end;

// Factory method
function GetPushNotificationService: IPushNotificationService;

implementation

{$IF DEFINED(ANDROID)}
uses
  PushNotification.Android;
{$ELSEIF DEFINED(IOS)}
uses
  PushNotification.iOS;
{$ENDIF}

function GetPushNotificationService: IPushNotificationService;
begin
{$IF DEFINED(ANDROID)}
  Result := TAndroidPushNotificationService.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSPushNotificationService.Create;
{$ELSE}
  // Implémentation factice pour les plateformes non supportées
  Result := TNullPushNotificationService.Create;
{$ENDIF}
end;

end.
```

### 5. Utilisation dans le code commun

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Obtenir le service de notifications
  FPushNotificationService := GetPushNotificationService;

  // Définir les gestionnaires d'événements
  FPushNotificationService.SetOnNotificationReceived(HandleNotificationReceived);
  FPushNotificationService.SetOnTokenReceived(HandleTokenReceived);

  // Initialiser le service
  if FPushNotificationService.Initialize then
  begin
    // Enregistrer pour les notifications
    if not FPushNotificationService.RegisterForPushNotifications then
      ShowMessage('Impossible de s''enregistrer pour les notifications');
  end
  else
    ShowMessage('Impossible d''initialiser le service de notifications');
end;

procedure TForm1.HandleNotificationReceived(const Data: TPushNotificationData);
begin
  // Traiter la notification reçue
  TThread.Synchronize(nil,
    procedure
    begin
      ShowMessage('Notification reçue: ' + Data.Title + #13#10 + Data.Message);

      // Traiter les données supplémentaires si nécessaire
      if Data.Data <> '' then
      begin
        var JsonObj := TJSONObject.ParseJSONValue(Data.Data) as TJSONObject;
        if Assigned(JsonObj) then
        try
          // Utiliser les données JSON
        finally
          JsonObj.Free;
        end;
      end;
    end);
end;

procedure TForm1.HandleTokenReceived(const Token: string);
begin
  // Envoyer le jeton au serveur back-end
  Log.d('PushNotification', 'Token reçu: ' + Token);

  // À implémenter: envoi du jeton à votre serveur
end;

procedure TForm1.ButtonSettingsClick(Sender: TObject);
begin
  // Ouvrir les paramètres de notification
  FPushNotificationService.OpenNotificationSettings;
end;
```

## Exemple 2 : Encapsulation de l'accès à la caméra

Voici un autre exemple qui encapsule l'accès à la caméra sur différentes plateformes :

### 1. Interface commune

```pascal
type
  TCameraResolution = (crLow, crMedium, crHigh);
  TCameraPosition = (cpBack, cpFront);

  TOnImageCapturedEvent = procedure(const Bitmap: TBitmap) of object;

  ICameraService = interface
    ['{ABCDEF12-3456-7890-ABCD-1234567890AB}']
    // Vérifier si la caméra est disponible
    function IsCameraAvailable(Position: TCameraPosition = cpBack): Boolean;

    // Démarrer la prévisualisation dans un contrôle
    function StartPreview(Control: TControl; Position: TCameraPosition = cpBack): Boolean;

    // Arrêter la prévisualisation
    procedure StopPreview;

    // Prendre une photo
    procedure TakePicture(Resolution: TCameraResolution = crHigh);

    // Activer/désactiver le flash
    procedure SetFlashMode(Enabled: Boolean);

    // Basculer entre les caméras avant/arrière
    procedure SwitchCamera;

    // Événement pour la capture d'image
    property OnImageCaptured: TOnImageCapturedEvent
      read GetOnImageCaptured write SetOnImageCaptured;
  end;
```

### 2. Implémentation Android

```pascal
{$IF DEFINED(ANDROID)}
uses
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Hardware;

type
  TAndroidCameraService = class(TInterfacedObject, ICameraService)
  private
    FCamera: JCamera;
    FPreviewControl: TControl;
    FCameraPosition: TCameraPosition;
    FSurfaceView: JSurfaceView;
    FSurfaceHolder: JSurfaceHolder;
    FOnImageCaptured: TOnImageCapturedEvent;

    procedure HandlePictureTaken(data: TJavaArray<Byte>; camera: JCamera);
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface
    function IsCameraAvailable(Position: TCameraPosition = cpBack): Boolean;
    function StartPreview(Control: TControl; Position: TCameraPosition = cpBack): Boolean;
    procedure StopPreview;
    procedure TakePicture(Resolution: TCameraResolution = crHigh);
    procedure SetFlashMode(Enabled: Boolean);
    procedure SwitchCamera;

    function GetOnImageCaptured: TOnImageCapturedEvent;
    procedure SetOnImageCaptured(const Value: TOnImageCapturedEvent);
  end;

// Détails d'implémentation...
{$ENDIF}
```

### 3. Implémentation iOS

```pascal
{$IF DEFINED(IOS)}
uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit,
  iOSapi.AVFoundation;

type
  TiOSCameraService = class(TInterfacedObject, ICameraService)
  private
    FSession: AVCaptureSession;
    FDevice: AVCaptureDevice;
    FPreviewLayer: AVCaptureVideoPreviewLayer;
    FPhotoOutput: AVCapturePhotoOutput;
    FPreviewControl: TControl;
    FCameraPosition: TCameraPosition;
    FOnImageCaptured: TOnImageCapturedEvent;

    procedure HandleCaptureOutput(output: AVCaptureOutput;
                                 sampleBuffer: CMSampleBufferRef;
                                 connection: AVCaptureConnection);
  public
    constructor Create;
    destructor Destroy; override;

    // Implémentation de l'interface
    function IsCameraAvailable(Position: TCameraPosition = cpBack): Boolean;
    function StartPreview(Control: TControl; Position: TCameraPosition = cpBack): Boolean;
    procedure StopPreview;
    procedure TakePicture(Resolution: TCameraResolution = crHigh);
    procedure SetFlashMode(Enabled: Boolean);
    procedure SwitchCamera;

    function GetOnImageCaptured: TOnImageCapturedEvent;
    procedure SetOnImageCaptured(const Value: TOnImageCapturedEvent);
  end;

// Détails d'implémentation...
{$ENDIF}
```

### 4. Factory pour la caméra

```pascal
unit CameraService;

interface

uses
  System.SysUtils, System.Classes, FMX.Controls, FMX.Graphics;

type
  TCameraResolution = (crLow, crMedium, crHigh);
  TCameraPosition = (cpBack, cpFront);

  TOnImageCapturedEvent = procedure(const Bitmap: TBitmap) of object;

  ICameraService = interface
    ['{ABCDEF12-3456-7890-ABCD-1234567890AB}']
    function IsCameraAvailable(Position: TCameraPosition = cpBack): Boolean;
    function StartPreview(Control: TControl; Position: TCameraPosition = cpBack): Boolean;
    procedure StopPreview;
    procedure TakePicture(Resolution: TCameraResolution = crHigh);
    procedure SetFlashMode(Enabled: Boolean);
    procedure SwitchCamera;

    function GetOnImageCaptured: TOnImageCapturedEvent;
    procedure SetOnImageCaptured(const Value: TOnImageCapturedEvent);

    property OnImageCaptured: TOnImageCapturedEvent
      read GetOnImageCaptured write SetOnImageCaptured;
  end;

// Factory method
function GetCameraService: ICameraService;

implementation

{$IF DEFINED(ANDROID)}
uses
  Camera.Android;
{$ELSEIF DEFINED(IOS)}
uses
  Camera.iOS;
{$ELSE}
uses
  Camera.Default;
{$ENDIF}

function GetCameraService: ICameraService;
begin
{$IF DEFINED(ANDROID)}
  Result := TAndroidCameraService.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSCameraService.Create;
{$ELSE}
  // Implémentation factice pour les plateformes non supportées
  Result := TDummyCameraService.Create;
{$ENDIF}
end;

end.
```

### 5. Utilisation dans une application

```pascal
procedure TForm1.ButtonStartCameraClick(Sender: TObject);
begin
  if not Assigned(FCameraService) then
    FCameraService := GetCameraService;

  FCameraService.OnImageCaptured := HandleImageCaptured;

  if FCameraService.IsCameraAvailable then
  begin
    if FCameraService.StartPreview(CameraPreviewLayout, cpBack) then
      ButtonTakePicture.Enabled := True
    else
      ShowMessage('Impossible de démarrer la prévisualisation de la caméra');
  end
  else
    ShowMessage('Aucune caméra disponible sur cet appareil');
end;

procedure TForm1.ButtonTakePictureClick(Sender: TObject);
begin
  if Assigned(FCameraService) then
    FCameraService.TakePicture(crHigh);
end;

procedure TForm1.HandleImageCaptured(const Bitmap: TBitmap);
begin
  // Afficher l'image capturée
  ImagePhoto.Bitmap.Assign(Bitmap);

  // Sauvegarder l'image si nécessaire
  // Bitmap.SaveToFile(...);
end;

procedure TForm1.ButtonSwitchCameraClick(Sender: TObject);
begin
  if Assigned(FCameraService) then
    FCameraService.SwitchCamera;
end;

procedure TForm1.ButtonFlashClick(Sender: TObject);
begin
  if Assigned(FCameraService) then
  begin
    // Basculer l'état du flash
    ButtonFlash.Tag := 1 - ButtonFlash.Tag;
    FCameraService.SetFlashMode(ButtonFlash.Tag = 1);

    // Mettre à jour l'icône
    if ButtonFlash.Tag = 1 then
      ButtonFlash.Text := 'Flash: ON'
    else
      ButtonFlash.Text := 'Flash: OFF';
  end;
end;
```

## Bonnes pratiques pour l'encapsulation d'API natives

### 1. La règle du plus petit dénominateur commun

Essayez d'identifier les fonctionnalités communes à toutes les plateformes pour créer une interface de base solide. Si une fonction n'est disponible que sur certaines plateformes, vous pouvez :

- Simuler la fonctionnalité sur les plateformes non prises en charge
- Fournir une méthode `IsSupported` pour vérifier la disponibilité
- Lancer une exception claire sur les plateformes non prises en charge

```pascal
procedure TWindowsDeviceService.Vibrate(DurationMS: Integer);
begin
  // Windows n'a généralement pas de fonction de vibration sur les ordinateurs de bureau
  // Vous pouvez jouer un son à la place ou ne rien faire
  Log.d('DeviceService', 'La vibration n''est pas prise en charge sur Windows desktop');
end;

function TWindowsDeviceService.HasVibration: Boolean;
begin
  // Indiquer que la vibration n'est pas disponible
  Result := False;
end;
```

### 2. Configuration par directive de compilation

Utilisez des directives de compilation pour inclure ou exclure du code spécifique à une plateforme :

```pascal
{$IF DEFINED(ANDROID) or DEFINED(IOS)}
// Code spécifique aux appareils mobiles
{$ELSE}
// Code spécifique aux ordinateurs de bureau
{$ENDIF}
```

Vous pouvez également utiliser `{$IF RTLVersion > 32}` pour cibler certaines versions de Delphi.

### 3. Gestion des permissions

Les différentes plateformes ont des approches différentes pour les permissions. Créez une couche d'abstraction pour gérer cela :

```pascal
type
  TPermissionType = (ptCamera, ptLocation, ptStorage, ptContacts);
  TPermissionStatus = (psUnknown, psGranted, psDenied, psRestricted);

  TPermissionRequestResult = procedure(PermissionType: TPermissionType;
                                     Status: TPermissionStatus) of object;

  IPermissionService = interface
    ['{FEDCBA98-7654-3210-FEDC-123456789ABC}']
    function CheckPermission(PermissionType: TPermissionType): TPermissionStatus;
    procedure RequestPermission(PermissionType: TPermissionType;
                              const OnResult: TPermissionRequestResult);
    function ShouldShowRequestRationale(PermissionType: TPermissionType): Boolean;
    procedure OpenAppSettings;
  end;
```

### 4. Initialisation paresseuse (Lazy initialization)

Si l'initialisation de l'API native est coûteuse, utilisez l'initialisation paresseuse :

```pascal
function TAndroidLocationService.GetLocationManager: JLocationManager;
begin
  if FLocationManager = nil then
  begin
    FLocationManager := TJLocationManager.Wrap(
      TAndroidHelper.Context.getSystemService(TJContext.JavaClass.LOCATION_SERVICE));
  end;
  Result := FLocationManager;
end;
```

Cette approche garantit que l'API n'est initialisée que lorsqu'elle est réellement nécessaire, améliorant ainsi les performances au démarrage de l'application.

### 5. Gestion des cycles de vie

Assurez-vous de gérer correctement le cycle de vie des ressources natives :

```pascal
destructor TiOSCameraService.Destroy;
begin
  // Arrêter et libérer la session de capture
  if Assigned(FSession) and FSession.running then
  begin
    FSession.stopRunning;
    // Supprimer les entrées et sorties
    for var Input in FSession.inputs do
      FSession.removeInput(Input);
    for var Output in FSession.outputs do
      FSession.removeOutput(Output);
  end;

  // Libérer les références
  FDevice := nil;
  FPreviewLayer := nil;
  FPhotoOutput := nil;
  FSession := nil;

  inherited;
end;
```

### 6. Documentation détaillée des limites

Documentez clairement les différences de comportement entre les plateformes :

```pascal
/// <summary>
///   Démarre la localisation en temps réel.
/// </summary>
/// <param name="Accuracy">Précision désirée</param>
/// <remarks>
///   Sur Android: Nécessite les permissions ACCESS_FINE_LOCATION et ACCESS_COARSE_LOCATION.
///   Sur iOS: Nécessite l'ajout de NSLocationWhenInUseUsageDescription dans Info.plist.
///   Sur Windows: Utilise l'API Geolocation avec une précision réduite.
/// </remarks>
function StartLocationUpdates(Accuracy: TLocationAccuracy): Boolean;
```

## Exemple 3 : Encapsulation de l'accès aux contacts

Voici un exemple d'encapsulation de l'accès aux contacts du téléphone :

### 1. Interface commune pour les contacts

```pascal
type
  TContactField = (cfName, cfPhone, cfEmail, cfPhoto, cfAddress);
  TContactFields = set of TContactField;

  TContact = record
    ID: string;
    DisplayName: string;
    FirstName: string;
    LastName: string;
    Phones: TArray<string>;
    Emails: TArray<string>;
    PhotoStream: TStream;
    Addresses: TArray<string>;
  end;

  TContactsResult = TArray<TContact>;
  TContactsCallback = procedure(const Contacts: TContactsResult) of object;

  IContactsService = interface
    ['{01234567-89AB-CDEF-0123-456789ABCDEF}']
    // Vérifier les permissions
    function HasContactsPermission: Boolean;
    function RequestContactsPermission: Boolean;

    // Récupérer tous les contacts
    procedure GetAllContacts(Fields: TContactFields; const Callback: TContactsCallback);

    // Rechercher des contacts
    procedure FindContacts(const Query: string; Fields: TContactFields;
                          const Callback: TContactsCallback);

    // Récupérer un contact par ID
    function GetContactByID(const ContactID: string; Fields: TContactFields): TContact;

    // Créer ou modifier un contact
    function SaveContact(const Contact: TContact): Boolean;

    // Supprimer un contact
    function DeleteContact(const ContactID: string): Boolean;

    // Ouvrir l'interface native de contacts
    procedure ShowContactsUI;
  end;
```

### 2. Implémentation Android

```pascal
{$IF DEFINED(ANDROID)}
uses
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.Provider;

type
  TAndroidContactsService = class(TInterfacedObject, IContactsService)
  private
    FContentResolver: JContentResolver;

    function GetContentResolver: JContentResolver;
    function CreateContactFromCursor(Cursor: JCursor; Fields: TContactFields): TContact;
  public
    function HasContactsPermission: Boolean;
    function RequestContactsPermission: Boolean;
    procedure GetAllContacts(Fields: TContactFields; const Callback: TContactsCallback);
    procedure FindContacts(const Query: string; Fields: TContactFields;
                          const Callback: TContactsCallback);
    function GetContactByID(const ContactID: string; Fields: TContactFields): TContact;
    function SaveContact(const Contact: TContact): Boolean;
    function DeleteContact(const ContactID: string): Boolean;
    procedure ShowContactsUI;
  end;

function TAndroidContactsService.GetContentResolver: JContentResolver;
begin
  if FContentResolver = nil then
    FContentResolver := TAndroidHelper.Context.getContentResolver;
  Result := FContentResolver;
end;

function TAndroidContactsService.HasContactsPermission: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(
    JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS));
end;

function TAndroidContactsService.RequestContactsPermission: Boolean;
begin
  // Utiliser le service de permissions pour demander l'accès aux contacts
  Result := False;

  PermissionsService.RequestPermissions(
    [JStringToString(TJManifest_permission.JavaClass.READ_CONTACTS)],
    procedure(const APermissions: TArray<string>; const AGrantResults: TArray<Boolean>)
    begin
      if Length(AGrantResults) > 0 then
        Result := AGrantResults[0];
    end);
end;

procedure TAndroidContactsService.GetAllContacts(Fields: TContactFields;
                                               const Callback: TContactsCallback);
var
  Cursor: JCursor;
  Contacts: TContactsResult;
  Contact: TContact;
  Projection: TJavaObjectArray<JString>;
  Uri: Jnet_Uri;
begin
  if not HasContactsPermission then
  begin
    if Assigned(Callback) then
      Callback(nil);
    Exit;
  end;

  // Préparer les colonnes à récupérer
  Projection := CreateJavaStringArray([
    StringToJString(TJContactsContract_ContactsColumns.JavaClass.DISPLAY_NAME),
    StringToJString(TJContactsContract_ContactsColumns.JavaClass._ID)
    // Ajouter d'autres colonnes selon Fields
  ]);

  // Uri pour les contacts
  Uri := TJContactsContract_Contacts.JavaClass.CONTENT_URI;

  // Exécuter la requête
  Cursor := GetContentResolver.query(Uri, Projection, nil, nil, nil);

  if Cursor <> nil then
  begin
    try
      // Allouer le tableau de résultats
      SetLength(Contacts, Cursor.getCount);

      // Parcourir les résultats
      if Cursor.moveToFirst then
      begin
        var Index := 0;
        repeat
          Contact := CreateContactFromCursor(Cursor, Fields);
          Contacts[Index] := Contact;
          Inc(Index);
        until not Cursor.moveToNext;
      end;

      // Appeler le callback avec les résultats
      if Assigned(Callback) then
        Callback(Contacts);
    finally
      Cursor.close;
    end;
  end
  else if Assigned(Callback) then
    Callback(nil);
end;

// Autres méthodes de l'implémentation...
{$ENDIF}
```

### 3. Implémentation iOS

```pascal
{$IF DEFINED(IOS)}
uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.UIKit,
  iOSapi.Contacts;

type
  TiOSContactsService = class(TInterfacedObject, IContactsService)
  private
    FContactStore: CNContactStore;

    function GetContactStore: CNContactStore;
    function CreateContactFromCNContact(CNContact: CNContact; Fields: TContactFields): TContact;
  public
    function HasContactsPermission: Boolean;
    function RequestContactsPermission: Boolean;
    procedure GetAllContacts(Fields: TContactFields; const Callback: TContactsCallback);
    procedure FindContacts(const Query: string; Fields: TContactFields;
                          const Callback: TContactsCallback);
    function GetContactByID(const ContactID: string; Fields: TContactFields): TContact;
    function SaveContact(const Contact: TContact): Boolean;
    function DeleteContact(const ContactID: string): Boolean;
    procedure ShowContactsUI;
  end;

function TiOSContactsService.GetContactStore: CNContactStore;
begin
  if FContactStore = nil then
    FContactStore := TCNContactStore.Create;
  Result := FContactStore;
end;

function TiOSContactsService.HasContactsPermission: Boolean;
var
  AuthStatus: CNAuthorizationStatus;
begin
  AuthStatus := TCNContactStore.OCClass.authorizationStatusForEntityType(
    CNEntityTypeContacts);
  Result := (AuthStatus = CNAuthorizationStatusAuthorized);
end;

function TiOSContactsService.RequestContactsPermission: Boolean;
var
  Semaphore: dispatch_semaphore_t;
begin
  Result := False;
  Semaphore := dispatch_semaphore_create(0);

  GetContactStore.requestAccessForEntityType(CNEntityTypeContacts,
    procedure(granted: Boolean; error: NSError)
    begin
      Result := granted;
      dispatch_semaphore_signal(Semaphore);
    end);

  dispatch_semaphore_wait(Semaphore, DISPATCH_TIME_FOREVER);
  dispatch_release(Semaphore);
end;

procedure TiOSContactsService.GetAllContacts(Fields: TContactFields;
                                           const Callback: TContactsCallback);
var
  Error: NSError;
  FetchRequest: CNContactFetchRequest;
  Contacts: TContactsResult;
  ContactCount: Integer;
begin
  if not HasContactsPermission then
  begin
    if Assigned(Callback) then
      Callback(nil);
    Exit;
  end;

  // Préparer les clés à récupérer
  var Keys := TNSArray.Create;
  Keys.addObject((CNContactGivenNameKey as ILocalObject).GetObjectID);
  Keys.addObject((CNContactFamilyNameKey as ILocalObject).GetObjectID);
  Keys.addObject((CNContactIdentifierKey as ILocalObject).GetObjectID);

  if cfPhone in Fields then
    Keys.addObject((CNContactPhoneNumbersKey as ILocalObject).GetObjectID);
  if cfEmail in Fields then
    Keys.addObject((CNContactEmailAddressesKey as ILocalObject).GetObjectID);
  if cfPhoto in Fields then
    Keys.addObject((CNContactImageDataKey as ILocalObject).GetObjectID);
  if cfAddress in Fields then
    Keys.addObject((CNContactPostalAddressesKey as ILocalObject).GetObjectID);

  // Créer la requête
  FetchRequest := TCNContactFetchRequest.OCClass.alloc;
  FetchRequest := TCNContactFetchRequest.Wrap(
    FetchRequest.initWithKeysToFetch(Keys));

  // Récupérer les contacts
  ContactCount := 0;
  SetLength(Contacts, 100); // Taille initiale

  GetContactStore.enumerateContactsWithFetchRequest(FetchRequest, error,
    procedure(contact: CNContact; stop: PBoolean)
    begin
      // Redimensionner le tableau si nécessaire
      if ContactCount >= Length(Contacts) then
        SetLength(Contacts, Length(Contacts) * 2);

      // Ajouter le contact
      Contacts[ContactCount] := CreateContactFromCNContact(contact, Fields);
      Inc(ContactCount);
    end);

  // Redimensionner à la taille finale
  SetLength(Contacts, ContactCount);

  // Appeler le callback avec les résultats
  if Assigned(Callback) then
    Callback(Contacts);
end;

// Autres méthodes de l'implémentation...
{$ENDIF}
```

### 4. Factory pour les contacts

```pascal
unit ContactsService;

interface

uses
  System.SysUtils, System.Classes;

type
  TContactField = (cfName, cfPhone, cfEmail, cfPhoto, cfAddress);
  TContactFields = set of TContactField;

  TContact = record
    ID: string;
    DisplayName: string;
    FirstName: string;
    LastName: string;
    Phones: TArray<string>;
    Emails: TArray<string>;
    PhotoStream: TStream;
    Addresses: TArray<string>;
  end;

  TContactsResult = TArray<TContact>;
  TContactsCallback = procedure(const Contacts: TContactsResult) of object;

  IContactsService = interface
    ['{01234567-89AB-CDEF-0123-456789ABCDEF}']
    function HasContactsPermission: Boolean;
    function RequestContactsPermission: Boolean;
    procedure GetAllContacts(Fields: TContactFields; const Callback: TContactsCallback);
    procedure FindContacts(const Query: string; Fields: TContactFields;
                          const Callback: TContactsCallback);
    function GetContactByID(const ContactID: string; Fields: TContactFields): TContact;
    function SaveContact(const Contact: TContact): Boolean;
    function DeleteContact(const ContactID: string): Boolean;
    procedure ShowContactsUI;
  end;

// Factory method
function GetContactsService: IContactsService;

implementation

{$IF DEFINED(ANDROID)}
uses
  Contacts.Android;
{$ELSEIF DEFINED(IOS)}
uses
  Contacts.iOS;
{$ELSE}
uses
  Contacts.Default;
{$ENDIF}

function GetContactsService: IContactsService;
begin
{$IF DEFINED(ANDROID)}
  Result := TAndroidContactsService.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSContactsService.Create;
{$ELSE}
  // Implémentation factice pour les plateformes non supportées
  Result := TNullContactsService.Create;
{$ENDIF}
end;

end.
```

### 5. Utilisation du service de contacts

```pascal
procedure TForm1.ButtonLoadContactsClick(Sender: TObject);
begin
  // Vérifier que nous avons les permissions
  if not FContactsService.HasContactsPermission then
  begin
    FContactsService.RequestContactsPermission;
    Exit;
  end;

  // Afficher un indicateur de progression
  ActivityIndicator1.Visible := True;
  ActivityIndicator1.Enabled := True;

  // Charger les contacts (opération potentiellement longue)
  FContactsService.GetAllContacts([cfName, cfPhone, cfEmail],
    procedure(const Contacts: TContactsResult)
    begin
      // S'assurer que nous sommes sur le thread principal
      TThread.Synchronize(nil,
        procedure
        begin
          try
            // Cacher l'indicateur de progression
            ActivityIndicator1.Visible := False;
            ActivityIndicator1.Enabled := False;

            // Afficher les contacts
            DisplayContacts(Contacts);
          except
            on E: Exception do
              ShowMessage('Erreur: ' + E.Message);
          end;
        end);
    end);
end;

procedure TForm1.DisplayContacts(const Contacts: TContactsResult);
var
  Contact: TContact;
  Item: TListViewItem;
begin
  ListView1.Items.Clear;

  for Contact in Contacts do
  begin
    Item := ListView1.Items.Add;
    Item.Text := Contact.DisplayName;

    if Length(Contact.Phones) > 0 then
      Item.Detail := Contact.Phones[0]
    else if Length(Contact.Emails) > 0 then
      Item.Detail := Contact.Emails[0];

    // Stocker l'ID du contact pour référence future
    Item.TagString := Contact.ID;
  end;
end;

procedure TForm1.ListView1ItemClick(const Sender: TObject;
                                  const AItem: TListViewItem);
var
  ContactID: string;
  Contact: TContact;
begin
  // Récupérer l'ID du contact
  ContactID := AItem.TagString;

  // Charger les détails complets
  Contact := FContactsService.GetContactByID(ContactID,
                [cfName, cfPhone, cfEmail, cfPhoto, cfAddress]);

  // Afficher les détails du contact
  ShowContactDetails(Contact);
end;
```

## Gérer les différences de fonctionnalités

Parfois, il n'est pas possible d'harmoniser complètement les fonctionnalités entre plateformes. Dans ces cas, vous pouvez :

### 1. Utiliser des interfaces spécifiques à la plateforme

```pascal
type
  // Interface de base commune à toutes les plateformes
  IDeviceService = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
    function GetDeviceID: string;
    function GetOSVersion: string;
  end;

  // Interface spécifique à Android
  IAndroidDeviceService = interface(IDeviceService)
    ['{87654321-1234-1234-1234-123456789ABC}']
    function GetAndroidID: string;
    function IsRooted: Boolean;
  end;

  // Interface spécifique à iOS
  IiOSDeviceService = interface(IDeviceService)
    ['{ABCDEF12-1234-1234-1234-123456789ABC}']
    function GetIOSIdentifierForVendor: string;
    function IsJailbroken: Boolean;
  end;
```

### 2. Vérifier la plateforme à l'exécution

```pascal
procedure TForm1.ButtonSpecificFeatureClick(Sender: TObject);
begin
  var DeviceService := GetDeviceService;

  // Vérifier si le service supporte les fonctionnalités spécifiques à Android
  if Supports(DeviceService, IAndroidDeviceService) then
  begin
    var AndroidService := DeviceService as IAndroidDeviceService;
    ShowMessage('Android ID: ' + AndroidService.GetAndroidID);
  end
  // Vérifier si le service supporte les fonctionnalités spécifiques à iOS
  else if Supports(DeviceService, IiOSDeviceService) then
  begin
    var iOSService := DeviceService as IiOSDeviceService;
    ShowMessage('iOS Identifier: ' + iOSService.GetIOSIdentifierForVendor);
  end
  else
  begin
    ShowMessage('Fonctionnalité non disponible sur cette plateforme');
  end;
end;
```

### 3. Fournir des alternatives dégradées

```pascal
// Implémentation Android qui utilise les notifications natives
procedure TAndroidNotificationService.ShowNotification(const Title, Message: string);
var
  NotificationBuilder: JNotificationCompat_Builder;
  NotificationManager: JNotificationManager;
begin
  // Code pour afficher une notification Android...
end;

// Implémentation Windows qui simule une notification
procedure TWindowsNotificationService.ShowNotification(const Title, Message: string);
begin
  // Sur Windows Desktop, utiliser une fenêtre popup si les notifications ne sont pas disponibles
  TSimplePopupForm.ShowPopup(Title, Message);
end;
```

## Exemple 4 : Service abstrait pour le stockage local

Le stockage des données peut varier significativement entre les plateformes. Voici comment créer une abstraction:

### 1. Interface commune pour le stockage

```pascal
type
  TStorageType = (stSecure, stStandard, stTemporary);

  ILocalStorage = interface
    ['{FEDCBA98-7654-3210-FEDC-BA9876543210}']
    // Opérations basiques
    function StoreValue(const Key, Value: string; StorageType: TStorageType = stStandard): Boolean;
    function RetrieveValue(const Key: string; out Value: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function RemoveValue(const Key: string; StorageType: TStorageType = stStandard): Boolean;

    // Gestion des blobs
    function StoreBlob(const Key: string; const Blob: TBytes;
                      StorageType: TStorageType = stStandard): Boolean;
    function RetrieveBlob(const Key: string; out Blob: TBytes;
                         StorageType: TStorageType = stStandard): Boolean;

    // Fichiers
    function GetStoragePath(StorageType: TStorageType): string;
    function FileExists(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
    function CopyToStorage(const SourceFilename, DestFilename: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function DeleteFile(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
  end;
```

### 2. Implémentation pour Android

```pascal
{$IF DEFINED(ANDROID)}
uses
  System.IOUtils,
  Androidapi.JNI.JavaTypes,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText;

type
  TAndroidLocalStorage = class(TInterfacedObject, ILocalStorage)
  private
    FContext: JContext;
    FSharedPreferences: JSharedPreferences;
    FSecurePreferences: JSharedPreferences;

    function GetContext: JContext;
    function GetSharedPreferences: JSharedPreferences;
    function GetSecurePreferences: JSharedPreferences;
  public
    function StoreValue(const Key, Value: string; StorageType: TStorageType = stStandard): Boolean;
    function RetrieveValue(const Key: string; out Value: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function RemoveValue(const Key: string; StorageType: TStorageType = stStandard): Boolean;

    function StoreBlob(const Key: string; const Blob: TBytes;
                      StorageType: TStorageType = stStandard): Boolean;
    function RetrieveBlob(const Key: string; out Blob: TBytes;
                         StorageType: TStorageType = stStandard): Boolean;

    function GetStoragePath(StorageType: TStorageType): string;
    function FileExists(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
    function CopyToStorage(const SourceFilename, DestFilename: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function DeleteFile(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
  end;

function TAndroidLocalStorage.GetContext: JContext;
begin
  if FContext = nil then
    FContext := TAndroidHelper.Context;
  Result := FContext;
end;

function TAndroidLocalStorage.GetSharedPreferences: JSharedPreferences;
begin
  if FSharedPreferences = nil then
    FSharedPreferences := GetContext.getSharedPreferences(
      StringToJString('app_preferences'), TJContext.JavaClass.MODE_PRIVATE);
  Result := FSharedPreferences;
end;

function TAndroidLocalStorage.StoreValue(const Key, Value: string;
                                       StorageType: TStorageType): Boolean;
var
  Editor: JSharedPreferences_Editor;
  Prefs: JSharedPreferences;
begin
  try
    // Choisir le stockage approprié
    case StorageType of
      stSecure: Prefs := GetSecurePreferences;
      else Prefs := GetSharedPreferences;
    end;

    // Obtenir l'éditeur et stocker la valeur
    Editor := Prefs.edit;
    Editor.putString(StringToJString(Key), StringToJString(Value));

    // Appliquer les changements
    Result := Editor.commit;
  except
    Result := False;
  end;
end;

function TAndroidLocalStorage.GetStoragePath(StorageType: TStorageType): string;
begin
  case StorageType of
    stSecure:
      Result := TPath.Combine(TPath.GetDocumentsPath, 'secure');
    stStandard:
      Result := TPath.GetDocumentsPath;
    stTemporary:
      Result := TPath.GetTempPath;
  end;

  // S'assurer que le dossier existe
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

// Autres méthodes de l'implémentation...
{$ENDIF}
```

### 3. Implémentation pour iOS

```pascal
{$IF DEFINED(IOS)}
uses
  System.IOUtils,
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.Security;

type
  TiOSLocalStorage = class(TInterfacedObject, ILocalStorage)
  private
    FUserDefaults: NSUserDefaults;
    FKeychainQuery: NSMutableDictionary;

    function GetUserDefaults: NSUserDefaults;
    function CreateKeychainQuery(const Key: string): NSMutableDictionary;
  public
    function StoreValue(const Key, Value: string; StorageType: TStorageType = stStandard): Boolean;
    function RetrieveValue(const Key: string; out Value: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function RemoveValue(const Key: string; StorageType: TStorageType = stStandard): Boolean;

    function StoreBlob(const Key: string; const Blob: TBytes;
                      StorageType: TStorageType = stStandard): Boolean;
    function RetrieveBlob(const Key: string; out Blob: TBytes;
                         StorageType: TStorageType = stStandard): Boolean;

    function GetStoragePath(StorageType: TStorageType): string;
    function FileExists(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
    function CopyToStorage(const SourceFilename, DestFilename: string;
                          StorageType: TStorageType = stStandard): Boolean;
    function DeleteFile(const Filename: string; StorageType: TStorageType = stStandard): Boolean;
  end;

function TiOSLocalStorage.GetUserDefaults: NSUserDefaults;
begin
  if FUserDefaults = nil then
    FUserDefaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  Result := FUserDefaults;
end;

function TiOSLocalStorage.StoreValue(const Key, Value: string;
                                   StorageType: TStorageType): Boolean;
var
  Dict: NSMutableDictionary;
  Status: OSStatus;
  ValueData: NSData;
begin
  Result := False;

  case StorageType of
    stSecure:
      begin
        // Utiliser le trousseau iOS (Keychain)
        Dict := CreateKeychainQuery(Key);

        // Préparer les données
        ValueData := TNSData.Wrap(
          TNSData.OCClass.dataWithBytes(
            PByte(UTF8String(Value)),
            Length(UTF8String(Value))
          )
        );

        // Ajouter les données au dictionnaire
        Dict.setObject(ValueData, CFSTR('v_Data'));

        // Supprimer toute entrée existante
        SecItemDelete(Dict);

        // Ajouter le nouvel élément
        Status := SecItemAdd(Dict, nil);
        Result := (Status = errSecSuccess);
      end;
    else
      begin
        // Utiliser NSUserDefaults pour le stockage standard
        GetUserDefaults.setObject(NSStr(Value), NSStr(Key));
        Result := GetUserDefaults.synchronize;
      end;
  end;
end;

function TiOSLocalStorage.GetStoragePath(StorageType: TStorageType): string;
begin
  case StorageType of
    stSecure:
      Result := TPath.Combine(TPath.GetDocumentsPath, 'secure');
    stStandard:
      Result := TPath.GetDocumentsPath;
    stTemporary:
      Result := TPath.GetTempPath;
  end;

  // S'assurer que le dossier existe
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

// Autres méthodes de l'implémentation...
{$ENDIF}
```

### 4. Factory et utilisation

```pascal
// Factory
function GetLocalStorage: ILocalStorage;
begin
{$IF DEFINED(ANDROID)}
  Result := TAndroidLocalStorage.Create;
{$ELSEIF DEFINED(IOS)}
  Result := TiOSLocalStorage.Create;
{$ELSE}
  Result := TStandardLocalStorage.Create;
{$ENDIF}
end;

// Utilisation
procedure TForm1.ButtonSaveClick(Sender: TObject);
var
  Storage: ILocalStorage;
begin
  Storage := GetLocalStorage;

  // Stocker une valeur
  if Storage.StoreValue('user_name', EditName.Text) then
    ShowMessage('Nom sauvegardé !')
  else
    ShowMessage('Erreur lors de la sauvegarde du nom');

  // Stocker une valeur de manière sécurisée
  if Storage.StoreValue('password', EditPassword.Text, stSecure) then
    ShowMessage('Mot de passe sauvegardé en sécurité !')
  else
    ShowMessage('Erreur lors de la sauvegarde du mot de passe');
end;

procedure TForm1.ButtonLoadClick(Sender: TObject);
var
  Storage: ILocalStorage;
  Value: string;
begin
  Storage := GetLocalStorage;

  // Récupérer une valeur normale
  if Storage.RetrieveValue('user_name', Value) then
    EditName.Text := Value
  else
    ShowMessage('Nom d''utilisateur non trouvé');

  // Récupérer une valeur sécurisée
  if Storage.RetrieveValue('password', Value, stSecure) then
    EditPassword.Text := Value
  else
    ShowMessage('Mot de passe non trouvé');
end;

procedure TForm1.ButtonSaveImageClick(Sender: TObject);
var
  Storage: ILocalStorage;
  SourcePath: string;
  DestPath: string;
begin
  if OpenDialog1.Execute then
  begin
    Storage := GetLocalStorage;
    SourcePath := OpenDialog1.FileName;

    // Obtenir le chemin de stockage et préparer le nom de fichier de destination
    DestPath := TPath.Combine(Storage.GetStoragePath(stStandard),
                             TPath.GetFileName(SourcePath));

    // Copier le fichier vers le stockage local
    if Storage.CopyToStorage(SourcePath, DestPath) then
      ShowMessage('Image sauvegardée: ' + DestPath)
    else
      ShowMessage('Erreur lors de la sauvegarde de l''image');
  end;
end;
```

## Techniques avancées d'encapsulation

Pour des applications plus complexes, vous pouvez utiliser des techniques avancées pour améliorer votre encapsulation d'API natives.

### 1. Modèle de conception Bridge

Le modèle Bridge sépare l'abstraction de son implémentation, permettant aux deux de varier indépendamment.

```pascal
// Abstraction : Interface pour la fonctionnalité
type
  IMediaPlayer = interface
    ['{12345678-ABCD-1234-ABCD-1234567890AB}']
    procedure Play(const URL: string);
    procedure Pause;
    procedure Stop;
    procedure SetVolume(Value: Single);
    function GetDuration: Integer;
    function GetPosition: Integer;
    procedure SeekTo(Position: Integer);
  end;

// Implémentation : Interface pour les implémentations spécifiques à la plateforme
type
  IMediaPlayerImpl = interface
    ['{ABCDEF12-3456-7890-ABCD-98765432DCBA}']
    procedure NativePlay(const URL: string);
    procedure NativePause;
    procedure NativeStop;
    procedure NativeSetVolume(Value: Single);
    function NativeGetDuration: Integer;
    function NativeGetPosition: Integer;
    procedure NativeSeekTo(Position: Integer);
  end;

// Classe abstraite qui fait le pont entre l'abstraction et l'implémentation
type
  TMediaPlayerBridge = class(TInterfacedObject, IMediaPlayer)
  private
    FImpl: IMediaPlayerImpl;
  public
    constructor Create(AImpl: IMediaPlayerImpl);

    // Implémentation de IMediaPlayer en déléguant à l'implémentation native
    procedure Play(const URL: string);
    procedure Pause;
    procedure Stop;
    procedure SetVolume(Value: Single);
    function GetDuration: Integer;
    function GetPosition: Integer;
    procedure SeekTo(Position: Integer);
  end;

constructor TMediaPlayerBridge.Create(AImpl: IMediaPlayerImpl);
begin
  inherited Create;
  FImpl := AImpl;
end;

procedure TMediaPlayerBridge.Play(const URL: string);
begin
  if Assigned(FImpl) then
    FImpl.NativePlay(URL);
end;

// Autres méthodes de délégation...

// Factory qui crée l'abstraction avec l'implémentation appropriée
function CreateMediaPlayer: IMediaPlayer;
var
  Impl: IMediaPlayerImpl;
begin
{$IF DEFINED(ANDROID)}
  Impl := TAndroidMediaPlayerImpl.Create;
{$ELSEIF DEFINED(IOS)}
  Impl := TiOSMediaPlayerImpl.Create;
{$ELSE}
  Impl := TDefaultMediaPlayerImpl.Create;
{$ENDIF}
  Result := TMediaPlayerBridge.Create(Impl);
end;
```

### 2. Utilisation de classes partielles

Dans certains cas, vous pouvez utiliser des classes partielles pour mieux organiser votre code:

```pascal
// Déclaration dans un fichier commun (MediaService.pas)
type
  TMediaService = class(TInterfacedObject, IMediaService)
  private
    // Champs communs
  public
    // Méthodes communes
    function Play(const URL: string): Boolean;
    procedure Stop;

    // Méthodes abstraites qui seront implémentées par plateforme
    function NativePlay(const URL: string): Boolean; virtual; abstract;
    procedure NativeStop; virtual; abstract;
  end;

// Implémentation Android (MediaService.Android.pas)
type
  TMediaService = class(TMediaService)
  private
    FMediaPlayer: JMediaPlayer;
  public
    // Implémentation des méthodes abstraites pour Android
    function NativePlay(const URL: string): Boolean; override;
    procedure NativeStop; override;
  end;

// Implémentation iOS (MediaService.iOS.pas)
type
  TMediaService = class(TMediaService)
  private
    FPlayer: AVPlayer;
  public
    // Implémentation des méthodes abstraites pour iOS
    function NativePlay(const URL: string): Boolean; override;
    procedure NativeStop; override;
  end;
```

### 3. Utilisation d'attributs pour les permissions

Vous pouvez utiliser des attributs pour gérer les permissions nécessaires par plateforme:

```pascal
type
  [Android('android.permission.CAMERA')]
  [iOS('NSCameraUsageDescription')]
  TCameraService = class(TInterfacedObject, ICameraService)
    // Implementation...
  end;

  [Android('android.permission.ACCESS_FINE_LOCATION')]
  [iOS('NSLocationWhenInUseUsageDescription')]
  TLocationService = class(TInterfacedObject, ILocationService)
    // Implementation...
  end;
```

Puis, à l'initialisation:

```pascal
procedure InitializeService<T: TInterfacedObject>(var Service: IInterface);
var
  ServiceClass: TClass;
  AndroidAttrs: TArray<AndroidAttribute>;
  iOSAttrs: TArray<iOSAttribute>;
begin
  ServiceClass := T;

{$IF DEFINED(ANDROID)}
  // Vérifier et demander les permissions Android
  AndroidAttrs := ServiceClass.GetAttributes<AndroidAttribute>;
  for var Attr in AndroidAttrs do
    PermissionService.RequestPermission(Attr.Permission);
{$ELSEIF DEFINED(IOS)}
  // Vérifier les définitions de Info.plist pour iOS
  iOSAttrs := ServiceClass.GetAttributes<iOSAttribute>;
  for var Attr in iOSAttrs do
    if not IsPlistKeyDefined(Attr.Key) then
      Log.w('Missing Info.plist key: ' + Attr.Key);
{$ENDIF}

  // Créer l'instance du service
  Service := T.Create;
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeService<TCameraService>(FCameraService);
  InitializeService<TLocationService>(FLocationService);
end;
```

## Organisation du code pour les projets multiplateforme

### 1. Structure des fichiers

Pour maintenir une organisation claire, utilisez cette structure de fichiers :

```
└── MyApp/
    ├── Common/                         # Code commun à toutes les plateformes
    │   ├── Interfaces/                 # Interfaces de service
    │   │   ├── DeviceService.pas       # Interface pour les fonctionnalités d'appareil
    │   │   ├── StorageService.pas      # Interface pour le stockage
    │   │   └── ...
    │   ├── Models/                     # Modèles de données partagés
    │   └── Utils/                      # Utilitaires génériques
    │
    ├── Platforms/                      # Implémentations spécifiques à la plateforme
    │   ├── Android/                    # Code spécifique à Android
    │   │   ├── Services/               # Implémentations de services
    │   │   │   ├── AndroidDeviceService.pas
    │   │   │   ├── AndroidStorageService.pas
    │   │   │   └── ...
    │   │   └── Utils/                  # Utilitaires spécifiques à Android
    │   │
    │   ├── iOS/                        # Code spécifique à iOS
    │   │   ├── Services/               # Implémentations de services
    │   │   │   ├── iOSDeviceService.pas
    │   │   │   ├── iOSStorageService.pas
    │   │   │   └── ...
    │   │   └── Utils/                  # Utilitaires spécifiques à iOS
    │   │
    │   └── Default/                    # Implémentations par défaut/factices
    │       ├── MockDeviceService.pas
    │       ├── MockStorageService.pas
    │       └── ...
    │
    ├── Features/                       # Fonctionnalités de l'application
    │   ├── Auth/                       # Fonctionnalité d'authentification
    │   ├── Profile/                    # Fonctionnalité de profil utilisateur
    │   └── ...
    │
    └── UI/                             # Interface utilisateur
        ├── Styles/                     # Styles et thèmes
        ├── Components/                 # Composants réutilisables
        └── Screens/                    # Écrans/formulaires de l'application
```

### 2. Utilisation des directives de compilation

Les directives de compilation vous aident à organiser votre code par plateforme :

```pascal
unit PlatformUtils;

interface

// Fonctions communes disponibles sur toutes les plateformes
function GetPlatformName: string;
function GetScreenScale: Single;

{$IF DEFINED(ANDROID)}
// Fonctions spécifiques à Android
function GetAndroidAPILevel: Integer;
function IsAndroidEmulator: Boolean;
{$ENDIF}

{$IF DEFINED(IOS)}
// Fonctions spécifiques à iOS
function GetIOSVersion: string;
function IsIPad: Boolean;
{$ENDIF}

implementation

function GetPlatformName: string;
begin
{$IF DEFINED(ANDROID)}
  Result := 'Android';
{$ELSEIF DEFINED(IOS)}
  Result := 'iOS';
{$ELSEIF DEFINED(MACOS)}
  Result := 'macOS';
{$ELSEIF DEFINED(MSWINDOWS)}
  Result := 'Windows';
{$ELSEIF DEFINED(LINUX)}
  Result := 'Linux';
{$ELSE}
  Result := 'Unknown';
{$ENDIF}
end;

function GetScreenScale: Single;
begin
{$IF DEFINED(ANDROID)}
  // Implémentation Android
{$ELSEIF DEFINED(IOS)}
  // Implémentation iOS
{$ELSE}
  // Implémentation par défaut
{$ENDIF}
end;

{$IF DEFINED(ANDROID)}
function GetAndroidAPILevel: Integer;
begin
  Result := TJBuild_VERSION.JavaClass.SDK_INT;
end;

function IsAndroidEmulator: Boolean;
var
  Build: JBuild;
begin
  Build := TJBuild.JavaClass;
  Result := (JStringToString(Build.FINGERPRINT) = 'generic') or
           (JStringToString(Build.FINGERPRINT).StartsWith('generic/sdk')) or
           (JStringToString(Build.MODEL).Contains('google_sdk')) or
           (JStringToString(Build.MODEL).Contains('Emulator')) or
           (JStringToString(Build.MODEL).Contains('Android SDK'));
end;
{$ENDIF}

{$IF DEFINED(IOS)}
function GetIOSVersion: string;
var
  SystemVersion: NSString;
begin
  SystemVersion := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).systemVersion;
  Result := NSStrToStr(SystemVersion);
end;

function IsIPad: Boolean;
begin
  Result := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;
{$ENDIF}

end.
```

### 3. Registre de services (Service Locator)

Pour gérer efficacement vos services encapsulés, utilisez un registre de services :

```pascal
unit ServiceLocator;

interface

uses
  System.SysUtils, System.Generics.Collections, System.Rtti;

type
  TServiceLocator = class
  private
    class var FInstance: TServiceLocator;
    FServices: TDictionary<TGUID, IInterface>;

    constructor Create;
    destructor Destroy; override;
  public
    class function GetInstance: TServiceLocator;
    class procedure ReleaseInstance;

    // Enregistrer un service
    procedure RegisterService<I: IInterface>(const Service: I);

    // Résoudre un service
    function ResolveService<I: IInterface>: I;

    // Vérifier si un service est enregistré
    function IsServiceRegistered<I: IInterface>: Boolean;

    // Supprimer un service
    procedure UnregisterService<I: IInterface>;
  end;

implementation

constructor TServiceLocator.Create;
begin
  inherited Create;
  FServices := TDictionary<TGUID, IInterface>.Create;
end;

destructor TServiceLocator.Destroy;
begin
  FServices.Free;
  inherited;
end;

class function TServiceLocator.GetInstance: TServiceLocator;
begin
  if FInstance = nil then
    FInstance := TServiceLocator.Create;
  Result := FInstance;
end;

class procedure TServiceLocator.ReleaseInstance;
begin
  if FInstance <> nil then
  begin
    FInstance.Free;
    FInstance := nil;
  end;
end;

procedure TServiceLocator.RegisterService<I>(const Service: I);
var
  TypeInfo: PTypeInfo;
  GUID: TGUID;
begin
  TypeInfo := TypeInfo(I);
  GUID := GetTypeData(TypeInfo).GUID;

  FServices.AddOrSetValue(GUID, IInterface(Service));
end;

function TServiceLocator.ResolveService<I>: I;
var
  TypeInfo: PTypeInfo;
  GUID: TGUID;
  Service: IInterface;
begin
  TypeInfo := TypeInfo(I);
  GUID := GetTypeData(TypeInfo).GUID;

  if FServices.TryGetValue(GUID, Service) then
    Result := I(Service)
  else
    raise Exception.CreateFmt('Service %s not registered', [GetTypeName(TypeInfo)]);
end;

function TServiceLocator.IsServiceRegistered<I>: Boolean;
var
  TypeInfo: PTypeInfo;
  GUID: TGUID;
begin
  TypeInfo := TypeInfo(I);
  GUID := GetTypeData(TypeInfo).GUID;

  Result := FServices.ContainsKey(GUID);
end;

procedure TServiceLocator.UnregisterService<I>;
var
  TypeInfo: PTypeInfo;
  GUID: TGUID;
begin
  TypeInfo := TypeInfo(I);
  GUID := GetTypeData(TypeInfo).GUID;

  if FServices.ContainsKey(GUID) then
    FServices.Remove(GUID);
end;

end.
```

### 4. Initialisation au démarrage de l'application

Enregistrez tous vos services au démarrage de l'application :

```pascal
procedure TMyApplication.Initialize;
begin
  inherited;

  // Initialiser le registre de services
  InitializeServices;
end;

procedure TMyApplication.InitializeServices;
var
  ServiceLocator: TServiceLocator;
begin
  ServiceLocator := TServiceLocator.GetInstance;

  // Enregistrer les services multiplateforme
  ServiceLocator.RegisterService<IDeviceService>(GetDeviceService);
  ServiceLocator.RegisterService<ICameraService>(GetCameraService);
  ServiceLocator.RegisterService<ILocationService>(GetLocationService);
  ServiceLocator.RegisterService<ILocalStorage>(GetLocalStorage);
  ServiceLocator.RegisterService<IPermissionService>(GetPermissionService);

  // Enregistrer d'autres services...
end;
```

### 5. Utilisation des services

Utilisez les services dans votre code :

```pascal
procedure TMyForm.ButtonTakePhotoClick(Sender: TObject);
var
  CameraService: ICameraService;
begin
  // Résoudre le service
  CameraService := TServiceLocator.GetInstance.ResolveService<ICameraService>;

  // Vérifier si la caméra est disponible
  if CameraService.IsCameraAvailable then
  begin
    // Configurer l'événement de capture
    CameraService.OnImageCaptured := HandleImageCaptured;

    // Démarrer la prévisualisation
    if CameraService.StartPreview(CameraPreviewControl) then
    begin
      // Afficher les contrôles de caméra
      PanelCameraControls.Visible := True;
    end
    else
      ShowMessage('Impossible de démarrer la caméra');
  end
  else
    ShowMessage('Aucune caméra disponible sur cet appareil');
end;

procedure TMyForm.HandleImageCaptured(const Bitmap: TBitmap);
var
  Storage: ILocalStorage;
  FileName: string;
  Stream: TMemoryStream;
begin
  // Résoudre le service de stockage
  Storage := TServiceLocator.GetInstance.ResolveService<ILocalStorage>;

  // Générer un nom de fichier unique
  FileName := TPath.Combine(
    Storage.GetStoragePath(stStandard),
    'photo_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.jpg'
  );

  // Sauvegarder l'image
  Stream := TMemoryStream.Create;
  try
    Bitmap.SaveToStream(Stream);
    Stream.Position := 0;

    if TFile.WriteAllBytes(FileName, Stream.Memory, Stream.Size) then
      ShowMessage('Photo sauvegardée: ' + FileName)
    else
      ShowMessage('Erreur lors de la sauvegarde de la photo');
  finally
    Stream.Free;
  end;
end;
```

## Tester les API natives encapsulées

### 1. Création de simulations (mocks) pour les tests

```pascal
type
  TMockCameraService = class(TInterfacedObject, ICameraService)
  private
    FOnImageCaptured: TOnImageCapturedEvent;
    FCameraAvailable: Boolean;
    FPreviewActive: Boolean;
  public
    constructor Create(CameraAvailable: Boolean = True);

    function IsCameraAvailable(Position: TCameraPosition = cpBack): Boolean;
    function StartPreview(Control: TControl; Position: TCameraPosition = cpBack): Boolean;
    procedure StopPreview;
    procedure TakePicture(Resolution: TCameraResolution = crHigh);
    procedure SetFlashMode(Enabled: Boolean);
    procedure SwitchCamera;

    property OnImageCaptured: TOnImageCapturedEvent
      read FOnImageCaptured write FOnImageCaptured;

    // Méthodes spécifiques aux tests
    procedure SimulatePictureTaken(const TestImage: string);
  end;

constructor TMockCameraService.Create(CameraAvailable: Boolean = True);
begin
  inherited Create;
  FCameraAvailable := CameraAvailable;
  FPreviewActive := False;
end;

function TMockCameraService.IsCameraAvailable(Position: TCameraPosition): Boolean;
begin
  Result := FCameraAvailable;
end;

function TMockCameraService.StartPreview(Control: TControl; Position: TCameraPosition): Boolean;
begin
  FPreviewActive := FCameraAvailable;
  Result := FPreviewActive;
end;

procedure TMockCameraService.StopPreview;
begin
  FPreviewActive := False;
end;

procedure TMockCameraService.TakePicture(Resolution: TCameraResolution);
begin
  // La résolution est ignorée dans cette simulation
  if not FPreviewActive then
    Exit;

  // Simuler un délai de prise de photo
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(500); // Délai simulé de 500ms

      // Simuler la prise de photo avec une image de test
      SimulatePictureTaken('test_image.jpg');
    end
  ).Start;
end;

procedure TMockCameraService.SimulatePictureTaken(const TestImage: string);
var
  Bitmap: TBitmap;
begin
  if not Assigned(FOnImageCaptured) then
    Exit;

  Bitmap := TBitmap.Create;
  try
    // Charger une image de test
    if FileExists(TestImage) then
      Bitmap.LoadFromFile(TestImage)
    else
    begin
      // Créer une image de test simple
      Bitmap.SetSize(640, 480);
      Bitmap.Canvas.Brush.Color := TAlphaColors.White;
      Bitmap.Canvas.FillRect(TRectF.Create(0, 0, 640, 480));
      Bitmap.Canvas.Fill.Color := TAlphaColors.Black;
      Bitmap.Canvas.Font.Size := 20;
      Bitmap.Canvas.FillText(
        TRectF.Create(0, 0, 640, 480),
        'TEST IMAGE',
        False, 1.0, [], TTextAlign.Center, TTextAlign.Center
      );
    end;

    // Notifier l'événement sur le thread principal
    TThread.Synchronize(nil,
      procedure
      begin
        FOnImageCaptured(Bitmap);
      end
    );
  finally
    Bitmap.Free;
  end;
end;
```

### 2. Tests unitaires

```pascal
procedure TestCameraService;
var
  MockCamera: TMockCameraService;
  ImageReceived: Boolean;
  TestControl: TControl;
begin
  // Configurer
  MockCamera := TMockCameraService.Create(True);
  ImageReceived := False;
  TestControl := TControl.Create(nil);

  try
    // Configurer l'événement
    MockCamera.OnImageCaptured :=
      procedure(const Bitmap: TBitmap)
      begin
        // Vérifier que l'image n'est pas nil
        Assert(Bitmap <> nil);
        Assert(Bitmap.Width > 0);
        Assert(Bitmap.Height > 0);

        ImageReceived := True;
      end;

    // Tester la disponibilité
    Assert(MockCamera.IsCameraAvailable);

    // Tester le démarrage de la prévisualisation
    Assert(MockCamera.StartPreview(TestControl));

    // Tester la prise de photo
    MockCamera.TakePicture;

    // Attendre que l'événement soit déclenché
    while not ImageReceived do
      Application.ProcessMessages;

    // Vérifier que l'image a été reçue
    Assert(ImageReceived);

  finally
    TestControl.Free;
    MockCamera.Free;
  end;
end;
```

### 3. Configuration pour les tests

```pascal
// Dans ServiceLocator
procedure TServiceLocator.RegisterTestServices;
begin
  // Remplacer les services par des mocks pour les tests
  RegisterService<ICameraService>(TMockCameraService.Create);
  RegisterService<ILocationService>(TMockLocationService.Create);
  RegisterService<IDeviceService>(TMockDeviceService.Create);
  RegisterService<ILocalStorage>(TMockLocalStorage.Create);
end;

// Dans le code d'initialisation des tests
procedure InitializeTestEnvironment;
begin
  // Configurer le registre de services pour les tests
  TServiceLocator.GetInstance.RegisterTestServices;
end;
```

## Conclusion et bonnes pratiques

L'encapsulation des API natives pour les applications multi-plateformes est une technique essentielle pour créer des applications de qualité qui fonctionnent sur différents systèmes. Voici un récapitulatif des meilleures pratiques à suivre :

1. **Concevoir des interfaces claires et complètes** :
   - Définissez des interfaces qui représentent les fonctionnalités, pas les implémentations
   - Incluez des méthodes pour vérifier la disponibilité des fonctionnalités
   - Prévoyez des mécanismes de retour d'information (callbacks, événements)

2. **Gérer les différences entre plateformes** :
   - Utilisez le plus petit dénominateur commun pour les interfaces de base
   - Étendez avec des interfaces spécifiques à la plateforme pour les fonctionnalités avancées
   - Documentez clairement les limites et différences

3. **Organiser le code** :
   - Séparez les interfaces des implémentations
   - Utilisez des directives de compilation pour le code spécifique à la plateforme
   - Implémentez un registre de services pour une gestion centralisée

4. **Tester efficacement** :
   - Créez des simulations (mocks) pour les tests unitaires
   - Testez les cas limites et les situations d'erreur
   - Vérifiez sur chaque plateforme cible

5. **Optimiser les performances** :
   - Utilisez l'initialisation paresseuse pour les ressources coûteuses
   - Gérez correctement le cycle de vie des ressources natives
   - Évitez les conversions de données inutiles entre Delphi et les API natives

6. **Gestion des permissions** :
   - Encapsulez la logique de demande de permissions
   - Fournissez des mécanismes de vérification préalable
   - Gérez gracieusement les refus de permission

En suivant ces principes, vous créerez des applications Delphi multi-plateformes robustes qui exploitent pleinement les capacités de chaque système d'exploitation tout en maintenant une base de code claire et maintenable.

N'oubliez pas que le but principal de l'encapsulation d'API natives est de simplifier votre code d'application en masquant les complexités et les différences des plateformes sous-jacentes, vous permettant de vous concentrer sur les fonctionnalités et l'expérience utilisateur de votre application.

⏭️ [Intégration de bibliothèques JavaScript via WebView](/14-utilisation-dapi-et-bibliotheques-externes/09-integration-de-bibliotheques-javascript-via-webview.md)
