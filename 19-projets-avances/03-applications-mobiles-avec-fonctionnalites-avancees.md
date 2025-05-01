# 19.3 Applications mobiles avec fonctionnalités avancées

## Introduction

Les appareils mobiles offrent un large éventail de fonctionnalités matérielles et logicielles que nous pouvons exploiter pour créer des applications riches et interactives. Dans ce chapitre, nous allons développer une application de "Carnet de Voyage" qui utilisera plusieurs fonctionnalités avancées disponibles sur les appareils mobiles.

Notre application permettra aux utilisateurs de :
- Prendre des photos et les intégrer à leurs souvenirs de voyage
- Utiliser le GPS pour géolocaliser les lieux visités
- Utiliser des capteurs comme l'accéléromètre et le gyroscope
- Accéder aux contacts de l'appareil
- Partager des contenus sur les réseaux sociaux
- Utiliser des notifications locales
- Intégrer des cartes interactives
- Stocker des données localement avec chiffrement

Ce tutoriel est conçu pour fonctionner avec Delphi 11 Alexandria ou Delphi 12 Athens.

## Prérequis

- Delphi 11 Alexandria ou Delphi 12 Athens
- Connaissance de base de FireMonkey (FMX)
- SDK Android et/ou iOS configuré dans votre environnement Delphi
- Un appareil mobile physique pour tester (certaines fonctionnalités ne fonctionnent pas correctement sur émulateur)

## 1. Création du projet

Commençons par créer notre application mobile :

1. Lancez Delphi et sélectionnez **Fichier > Nouveau > Application Multi-périphériques**.
2. Choisissez **Formulaire vide** et cliquez sur **OK**.
3. Sauvegardez le projet en lui donnant un nom significatif (par exemple, "TravelJournal").

## 2. Configuration du projet pour les fonctionnalités avancées

Avant de commencer à coder, nous devons configurer notre projet pour qu'il puisse accéder aux différentes fonctionnalités matérielles :

1. Dans le Project Manager, cliquez-droit sur le projet et sélectionnez **Options du projet**.
2. Naviguez vers **Utilisation** sous la plateforme cible (Android/iOS).
3. Activez les permissions suivantes :
   - **Caméra**
   - **Localisation précise (GPS)**
   - **Stockage externe** (pour Android)
   - **Contacts**
   - **Photos/Multimédia** (pour iOS)
   - **Capteurs de mouvement**
   - **Notifications**

4. Pour iOS, vous devrez également ajouter des descriptions d'utilisation dans la section **Version Info > Keys** :
   - `NSCameraUsageDescription` : "Cette application utilise l'appareil photo pour capturer vos souvenirs de voyage."
   - `NSLocationWhenInUseUsageDescription` : "Cette application utilise votre position pour marquer les lieux que vous visitez."
   - `NSPhotoLibraryUsageDescription` : "Cette application accède à votre galerie pour sauvegarder et charger des images."
   - `NSContactsUsageDescription` : "Cette application accède à vos contacts pour partager vos voyages."

## 3. Structure de l'application

Notre application sera organisée en plusieurs écrans :

1. **Écran principal** : Liste des voyages
2. **Détails du voyage** : Informations détaillées avec liste des souvenirs
3. **Ajout/édition de voyage** : Formulaire pour créer ou modifier un voyage
4. **Ajout/édition de souvenir** : Formulaire pour ajouter une photo, une note ou une position
5. **Carte** : Vue des lieux visités sur une carte
6. **Paramètres** : Configuration de l'application

### 3.1 Création des formulaires

Commençons par créer les unités pour chaque écran :

1. Créez un nouveau formulaire (Fichier > Nouveau > Multi-périphériques > Formulaire vide) pour chacun des écrans mentionnés ci-dessus.
2. Nommez et sauvegardez chaque unité avec un nom significatif (par exemple, `MainFormUnit.pas`, `TripDetailsFormUnit.pas`, etc.).

## 4. Modèle de données

Définissons les classes qui représenteront nos données :

Créez une nouvelle unité (`DataModelUnit.pas`) et implémentez les classes suivantes :

```pascal
unit DataModelUnit;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.JSON,
  System.IOUtils;

type
  TGeoLocation = record
    Latitude: Double;
    Longitude: Double;
    Altitude: Double;
    Timestamp: TDateTime;
    function ToJSON: TJSONObject;
    procedure FromJSON(const JSON: TJSONObject);
  end;

  TTripMemoryType = (mtPhoto, mtNote, mtLocation, mtAudio);

  TTripMemory = class
  private
    FID: TGUID;
    FTitle: string;
    FDescription: string;
    FType: TTripMemoryType;
    FLocation: TGeoLocation;
    FDateTime: TDateTime;
    FFilePath: string;
    FTags: TArray<string>;
    FWeather: string;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject;
    procedure FromJSON(const JSON: TJSONObject);

    property ID: TGUID read FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property MemoryType: TTripMemoryType read FType write FType;
    property Location: TGeoLocation read FLocation write FLocation;
    property DateTime: TDateTime read FDateTime write FDateTime;
    property FilePath: string read FFilePath write FFilePath;
    property Tags: TArray<string> read FTags write FTags;
    property Weather: string read FWeather write FWeather;
  end;

  TTrip = class
  private
    FID: TGUID;
    FTitle: string;
    FDescription: string;
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FDestination: string;
    FMemories: TObjectList<TTripMemory>;
    FCoverPhotoPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function ToJSON: TJSONObject;
    procedure FromJSON(const JSON: TJSONObject);
    function AddMemory(const Title, Description: string; MemoryType: TTripMemoryType): TTripMemory;

    property ID: TGUID read FID;
    property Title: string read FTitle write FTitle;
    property Description: string read FDescription write FDescription;
    property StartDate: TDateTime read FStartDate write FStartDate;
    property EndDate: TDateTime read FEndDate write FEndDate;
    property Destination: string read FDestination write FDestination;
    property Memories: TObjectList<TTripMemory> read FMemories;
    property CoverPhotoPath: string read FCoverPhotoPath write FCoverPhotoPath;
  end;

  TTripManager = class
  private
    FTrips: TObjectList<TTrip>;
    FStoragePath: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadTrips;
    procedure SaveTrips;
    function AddTrip(const Title, Description, Destination: string;
                    StartDate, EndDate: TDateTime): TTrip;
    procedure DeleteTrip(const ID: TGUID);
    function FindTripByID(const ID: TGUID): TTrip;

    property Trips: TObjectList<TTrip> read FTrips;
    property StoragePath: string read FStoragePath;
  end;

implementation

// Implémentation des méthodes...
// Le code est simplifié pour le tutoriel. Une implémentation complète
// inclurait la sérialisation/désérialisation JSON et la gestion des fichiers.

end.
```

## 5. Interface utilisateur principale

Maintenant, créons l'interface de l'écran principal qui affichera la liste des voyages :

1. Ouvrez le fichier `MainFormUnit.pas`.
2. Ajoutez les composants suivants depuis la palette :

   - Un `TToolBar` en haut avec :
     - Un `TLabel` pour le titre "Mes Voyages"
     - Un `TButton` pour ajouter un nouveau voyage (texte "+")

   - Un `TListView` qui occupera la majorité de l'écran pour afficher les voyages

   - Un `TRectangle` qui s'affichera quand la liste est vide avec un message

3. Configurez l'apparence de la ListView pour afficher des vignettes et du texte.

Voici le code de base pour l'unité principale :

```pascal
unit MainFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Objects, FMX.Controls.Presentation, FMX.Layouts,
  DataModelUnit;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    TitleLabel: TLabel;
    AddButton: TButton;
    TripListView: TListView;
    EmptyMessage: TRectangle;
    EmptyText: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure TripListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    FTripManager: TTripManager;
    procedure RefreshTripList;
    procedure UpdateEmptyState;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  TripEditFormUnit, TripDetailsFormUnit;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FTripManager := TTripManager.Create;
  FTripManager.LoadTrips;
  RefreshTripList;
  UpdateEmptyState;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FTripManager.Free;
end;

procedure TMainForm.RefreshTripList;
var
  Trip: TTrip;
  Item: TListViewItem;
  Bitmap: TBitmap;
begin
  TripListView.BeginUpdate;
  try
    TripListView.Items.Clear;

    for Trip in FTripManager.Trips do
    begin
      Item := TripListView.Items.Add;
      Item.Text := Trip.Title;
      Item.Detail := Trip.Destination + ' - ' +
                    FormatDateTime('dd/mm/yyyy', Trip.StartDate);

      // Chargement de l'image de couverture si elle existe
      if (Trip.CoverPhotoPath <> '') and FileExists(Trip.CoverPhotoPath) then
      begin
        Bitmap := TBitmap.Create;
        try
          Bitmap.LoadFromFile(Trip.CoverPhotoPath);
          Item.Bitmap := Bitmap;
        finally
          Bitmap.Free;
        end;
      end;

      Item.TagObject := Trip; // Pour retrouver l'objet associé au clic
    end;
  finally
    TripListView.EndUpdate;
  end;

  UpdateEmptyState;
end;

procedure TMainForm.UpdateEmptyState;
begin
  EmptyMessage.Visible := TripListView.Items.Count = 0;
end;

procedure TMainForm.AddButtonClick(Sender: TObject);
begin
  TripEditForm.ShowForNewTrip;
  if TripEditForm.ModalResult = mrOK then
    RefreshTripList;
end;

procedure TMainForm.TripListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  Trip: TTrip;
begin
  if AItem = nil then
    Exit;

  Trip := AItem.TagObject as TTrip;
  TripDetailsForm.ShowTripDetails(Trip);

  // Rafraîchir la liste au cas où des modifications ont été apportées
  RefreshTripList;
end;

end.
```

## 6. Fonctionnalités avancées sur mobile

Maintenant, implémentons les fonctionnalités avancées spécifiques aux mobiles :

### 6.1 Utilisation de l'appareil photo

Pour permettre aux utilisateurs de prendre des photos pour leurs souvenirs de voyage, créons une méthode dédiée dans le formulaire d'ajout de souvenirs :

```pascal
// Dans les uses, ajoutez :
FMX.Media, System.Permissions, FMX.DialogService;

// Dans la classe TMemoryEditForm, ajoutez :
private
  FPhotoFileName: string;
  procedure TakePhotoFromCamera;
  procedure PermissionRequestResult(Sender: TObject;
    const APermissions: TClassicStringDynArray;
    const AGrantResults: TClassicPermissionStatusDynArray);

// Implémentation :
procedure TMemoryEditForm.TakePhotoFromCamera;
var
  PhotoFile: string;
begin
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions([JStringToString(
    TJManifest_permission.JavaClass.CAMERA)], PermissionRequestResult);
  Exit;
  {$ENDIF}

  {$IFDEF IOS}
  // Sur iOS, les permissions sont gérées différemment
  {$ENDIF}

  // Code qui sera exécuté après que les permissions sont accordées
  TakePhotoInternal;
end;

procedure TMemoryEditForm.TakePhotoInternal;
var
  CameraComponent: TCameraComponent;
begin
  CameraComponent := TCameraComponent.Create(Self);
  try
    CameraComponent.Kind := TCameraKind.FrontCamera; // ou BackCamera

    if CameraComponent.Available then
    begin
      // Créer un nom de fichier unique
      FPhotoFileName := TPath.Combine(
        TPath.GetDocumentsPath,
        'Photo_' + FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');

      // Capturer la photo
      CameraComponent.SampleBufferToBitmap(PhotoImage.Bitmap, True);

      // Enregistrer l'image
      PhotoImage.Bitmap.SaveToFile(FPhotoFileName);
    end
    else
      ShowMessage('Appareil photo non disponible');
  finally
    CameraComponent.Free;
  end;
end;

procedure TMemoryEditForm.PermissionRequestResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
begin
  // Vérifier si la permission camera a été accordée
  if (Length(AGrantResults) = 1) and
     (AGrantResults[0] = TPermissionStatus.Granted) then
    TakePhotoInternal
  else
    ShowMessage('La permission d''utiliser l''appareil photo est nécessaire');
end;
```

### 6.2 Géolocalisation GPS

Pour enregistrer la position actuelle, implémentons le code suivant :

```pascal
// Dans les uses, ajoutez :
System.Sensors, System.Sensors.Components;

// Dans la classe, ajoutez :
private
  FLocationSensor: TLocationSensor;
  procedure GetCurrentLocation;
  procedure LocationSensorLocationChanged(Sender: TObject; const OldLocation,
    NewLocation: TLocationCoord2D);

// Implémentation :
procedure TMemoryEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  // Initialiser le capteur de localisation
  FLocationSensor := TLocationSensor.Create(Self);
  FLocationSensor.OnLocationChanged := LocationSensorLocationChanged;
  FLocationSensor.Active := False; // L'activer uniquement quand nécessaire
end;

procedure TMemoryEditForm.GetCurrentLocation;
begin
  // Vérifier et demander les permissions
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions([
    JStringToString(TJManifest_permission.JavaClass.ACCESS_FINE_LOCATION)],
    LocationPermissionRequestResult);
  Exit;
  {$ENDIF}

  // Activer le capteur après avoir obtenu les permissions
  StartLocationSensor;
end;

procedure TMemoryEditForm.StartLocationSensor;
begin
  if FLocationSensor.Available then
  begin
    LocationStatusLabel.Text := 'Recherche de position...';
    FLocationSensor.Active := True;
  end
  else
    ShowMessage('GPS non disponible sur cet appareil');
end;

procedure TMemoryEditForm.LocationSensorLocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
var
  MemoryLocation: TGeoLocation;
begin
  // Enregistrer la nouvelle position
  MemoryLocation.Latitude := NewLocation.Latitude;
  MemoryLocation.Longitude := NewLocation.Longitude;
  MemoryLocation.Altitude := FLocationSensor.Sensor.Altitude;
  MemoryLocation.Timestamp := Now;

  // Mettre à jour l'interface
  LocationStatusLabel.Text := Format('Position: %.6f, %.6f',
    [MemoryLocation.Latitude, MemoryLocation.Longitude]);

  // Stocker la position dans l'objet mémoire
  FMemory.Location := MemoryLocation;

  // Désactiver le capteur pour économiser la batterie
  FLocationSensor.Active := False;
end;
```

### 6.3 Intégration d'une carte interactive

Pour afficher les souvenirs géolocalisés sur une carte, créons un formulaire dédié :

```pascal
// Dans TripMapFormUnit.pas, ajoutez :
uses
  FMX.Maps; // Pour accéder aux composants de carte

type
  TTripMapForm = class(TForm)
    ToolBar1: TToolBar;
    BackButton: TButton;
    TitleLabel: TLabel;
    MapView: TMapView;
    procedure FormCreate(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
  private
    FTrip: TTrip;
    procedure AddMemoriesToMap;
  public
    procedure ShowMap(Trip: TTrip);
  end;

// Implémentation :
procedure TTripMapForm.FormCreate(Sender: TObject);
begin
  MapView.MapType := TMapType.Normal;
  MapView.ZoomLevel := 13; // Niveau de zoom initial
end;

procedure TTripMapForm.ShowMap(Trip: TTrip);
begin
  FTrip := Trip;
  TitleLabel.Text := 'Carte : ' + Trip.Title;
  AddMemoriesToMap;
  ShowModal;
end;

procedure TTripMapForm.AddMemoriesToMap;
var
  Memory: TTripMemory;
  MapMarker: TMapMarkerDescriptor;
  Location: TMapCoordinate;
begin
  MapView.Clear; // Effacer les marqueurs existants

  for Memory in FTrip.Memories do
  begin
    // Ignorer les souvenirs sans coordonnées GPS
    if (Memory.Location.Latitude = 0) and (Memory.Location.Longitude = 0) then
      Continue;

    // Créer un marqueur pour chaque souvenir
    Location.Latitude := Memory.Location.Latitude;
    Location.Longitude := Memory.Location.Longitude;

    MapMarker := TMapMarkerDescriptor.Create(Location, Memory.Title);
    MapMarker.Snippet := FormatDateTime('dd/mm/yyyy hh:nn', Memory.DateTime);

    // Personnaliser l'icône selon le type de souvenir
    case Memory.MemoryType of
      mtPhoto: MapMarker.IconFile := 'camera.png';
      mtNote: MapMarker.IconFile := 'note.png';
      mtLocation: MapMarker.IconFile := 'pin.png';
      mtAudio: MapMarker.IconFile := 'audio.png';
    end;

    // Ajouter le marqueur à la carte
    MapView.AddMarker(MapMarker);
  end;

  // Si au moins un marqueur a été ajouté, centrer la carte sur le premier
  if FTrip.Memories.Count > 0 then
  begin
    for Memory in FTrip.Memories do
    begin
      if (Memory.Location.Latitude <> 0) or (Memory.Location.Longitude <> 0) then
      begin
        Location.Latitude := Memory.Location.Latitude;
        Location.Longitude := Memory.Location.Longitude;
        MapView.Location := Location;
        Break;
      end;
    end;
  end;
end;
```

### 6.4 Capteurs de mouvement (accéléromètre)

Implémentons une fonction qui utilisera l'accéléromètre pour détecter les secousses de l'appareil :

```pascal
// Dans les uses, ajoutez :
System.Sensors, System.Sensors.Components;

// Dans la classe TTripDetailsForm, ajoutez :
private
  FMotionSensor: TMotionSensor;
  FLastShakeTime: TDateTime;
  procedure MotionSensorReadingChanged(Sender: TObject);
  procedure HandleShake;

// Implémentation :
procedure TTripDetailsForm.FormCreate(Sender: TObject);
begin
  // Initialiser le capteur de mouvement
  FMotionSensor := TMotionSensor.Create(Self);
  FMotionSensor.OnReadingChanged := MotionSensorReadingChanged;
  FMotionSensor.Active := True;
  FLastShakeTime := 0;
end;

procedure TTripDetailsForm.MotionSensorReadingChanged(Sender: TObject);
const
  ShakeThreshold = 2.0; // Seuil de détection des secousses
  MinTimeBetweenShakes = 2; // Secondes entre les détections
var
  Acceleration: TMotionSensorData;
  MagnitudeSquared: Single;
begin
  // Lecture des données de l'accéléromètre
  Acceleration := FMotionSensor.Sensor.CurrentReading;

  // Calculer la magnitude de l'accélération
  MagnitudeSquared := Sqr(Acceleration.AccelerationX) +
                    Sqr(Acceleration.AccelerationY) +
                    Sqr(Acceleration.AccelerationZ);

  // Détecter une secousse
  if (MagnitudeSquared > Sqr(ShakeThreshold)) and
     (SecondsBetween(Now, FLastShakeTime) > MinTimeBetweenShakes) then
  begin
    FLastShakeTime := Now;
    HandleShake;
  end;
end;

procedure TTripDetailsForm.HandleShake;
begin
  // Action à exécuter en cas de secousse (par exemple, afficher un souvenir aléatoire)
  TThread.Synchronize(nil,
    procedure
    var
      RandomIndex: Integer;
    begin
      if FTrip.Memories.Count > 0 then
      begin
        RandomIndex := Random(FTrip.Memories.Count);
        ShowMemoryDetails(FTrip.Memories[RandomIndex]);
      end;
    end);
end;
```

### 6.5 Notifications locales

Implémentons des notifications pour rappeler à l'utilisateur d'ajouter des souvenirs :

```pascal
// Dans les uses, ajoutez :
FMX.PushNotification;

// Dans la classe TTripDetailsForm, ajoutez :
private
  FNotificationCenter: TPushNotificationCenter;
  procedure SetupNotifications;
  procedure ScheduleReminderNotification;

// Implémentation :
procedure TTripDetailsForm.SetupNotifications;
begin
  FNotificationCenter := TPushNotificationCenter.Create(nil);
end;

procedure TTripDetailsForm.ScheduleReminderNotification;
var
  Notification: TPushNotification;
begin
  // Créer une notification pour le soir
  Notification := TPushNotification.Create;
  try
    Notification.Title := 'Carnet de Voyage';
    Notification.AlertBody := 'N''oubliez pas d''ajouter vos souvenirs de la journée !';
    Notification.FireDate := EncodeTime(21, 0, 0, 0); // 21:00
    Notification.RepeatInterval := TRepeatInterval.Day;

    // Ajouter la notification au centre de notification
    FNotificationCenter.ScheduleNotification(Notification);
  finally
    Notification.Free;
  end;
end;
```

### 6.6 Partage sur les réseaux sociaux et via d'autres applications

Implémentons une fonction de partage :

```pascal
// Dans les uses, ajoutez :
FMX.ShareSheet;

procedure TMemoryDetailsForm.ShareButtonClick(Sender: TObject);
var
  ShareService: IFMXShareSheetService;
  SharedItems: TShareItems;
  SharedItem: TShareItem;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetService, ShareService) then
  begin
    // Créer les éléments à partager
    SharedItems := TShareItems.Create;
    try
      // Ajouter du texte
      SharedItem := TShareItem.Create;
      SharedItem.Text := Format('Souvenir du voyage à %s: %s - %s',
        [FTrip.Destination, FMemory.Title, FMemory.Description]);
      SharedItems.Add(SharedItem);

      // Ajouter une image si disponible
      if (FMemory.FilePath <> '') and FileExists(FMemory.FilePath) and
         (FMemory.MemoryType = mtPhoto) then
      begin
        SharedItem := TShareItem.Create;
        SharedItem.FilePath := FMemory.FilePath;
        SharedItems.Add(SharedItem);
      end;

      // Afficher le dialogue de partage
      ShareService.Share(SharedItems,
        procedure(const AResult: TShareResult)
        begin
          case AResult of
            TShareResult.Completed: ShowMessage('Partagé avec succès');
            TShareResult.Cancelled: {Rien à faire};
            TShareResult.Failed: ShowMessage('Échec du partage');
          end;
        end);
    finally
      SharedItems.Free;
    end;
  end
  else
    ShowMessage('Le partage n''est pas disponible sur cet appareil');
end;
```

### 6.7 Accès aux contacts de l'appareil

Permettons à l'utilisateur de sélectionner un contact pour partager son voyage :

```pascal
// Dans les uses, ajoutez :
FMX.AddressBook, FMX.AddressBook.Types;

// Dans la classe TTripDetailsForm, ajoutez :
private
  FAddressBook: TAddressBook;
  procedure LoadContacts;
  procedure AddressBookPermissionRequest(Sender: TObject; const AMessage: string;
    const AAccessGranted: Boolean);

// Implémentation :
procedure TTripDetailsForm.LoadContacts;
begin
  // Créer et initialiser le carnet d'adresses
  FAddressBook := TAddressBook.Create(Self);
  FAddressBook.RequestPermission(AddressBookPermissionRequest);
end;

procedure TTripDetailsForm.AddressBookPermissionRequest(Sender: TObject;
  const AMessage: string; const AAccessGranted: Boolean);
var
  Contact: TAddressBookContact;
  ContactItem: TListViewItem;
begin
  if AAccessGranted then
  begin
    FAddressBook.AllContacts; // Charger tous les contacts

    // Afficher les contacts dans une ListView
    ContactsListView.BeginUpdate;
    try
      ContactsListView.Items.Clear;

      for Contact in FAddressBook.Contacts do
      begin
        ContactItem := ContactsListView.Items.Add;
        ContactItem.Text := Contact.DisplayName;

        // Afficher l'email ou le téléphone si disponible
        if Contact.EMails.Count > 0 then
          ContactItem.Detail := Contact.EMails[0].Address
        else if Contact.PhoneNumbers.Count > 0 then
          ContactItem.Detail := Contact.PhoneNumbers[0].Number;

        ContactItem.TagObject := Contact; // Pour retrouver le contact lors du clic
      end;
    finally
      ContactsListView.EndUpdate;
    end;
  end
  else
    ShowMessage('L''accès aux contacts est nécessaire pour cette fonctionnalité');
end;
```

### 6.8 Stockage sécurisé

Implémentons un système de stockage chiffré pour les données sensibles :

```pascal
// Dans les uses, ajoutez :
System.IOUtils, IdGlobal, IdCoder, IdCoderMIME, IdHashSHA;

// Créez une unité dédiée à la sécurité :
unit SecurityUtils;

interface

uses
  System.SysUtils, System.Classes, IdGlobal, IdCoder, IdCoderMIME, IdHashSHA;

type
  TSecurityUtils = class
  public
    class function EncryptString(const AString, APassword: string): string;
    class function DecryptString(const AEncryptedString, APassword: string): string;
    class function HashPassword(const APassword: string): string;
    class function VerifyPassword(const APassword, AHash: string): Boolean;
  end;

implementation

class function TSecurityUtils.EncryptString(const AString, APassword: string): string;
var
  Cipher, Key: TIdBytes;
  Encoder: TIdEncoderMIME;
begin
  // Version simplifiée - dans une application réelle, utilisez un algorithme de chiffrement robuste
  Key := TIdHashSHA256.Create.HashString(APassword);
  Cipher := TIdBytes(AString);
  // Appliquer un XOR simple avec la clé
  for var I := 0 to Length(Cipher) - 1 do
    Cipher[I] := Cipher[I] xor Key[I mod Length(Key)];

  // Encoder en Base64 pour stockage
  Encoder := TIdEncoderMIME.Create;
  try
    Result := Encoder.EncodeBytes(Cipher);
  finally
    Encoder.Free;
  end;
end;

class function TSecurityUtils.DecryptString(const AEncryptedString, APassword: string): string;
var
  Cipher, Key: TIdBytes;
  Decoder: TIdDecoderMIME;
begin
  // Décoder le Base64
  Decoder := TIdDecoderMIME.Create;
  try
    Cipher := Decoder.DecodeBytes(AEncryptedString);
  finally
    Decoder.Free;
  end;

  // Récréer la clé
  Key := TIdHashSHA256.Create.HashString(APassword);

  // Appliquer le XOR inverse
  for var I := 0 to Length(Cipher) - 1 do
    Cipher[I] := Cipher[I] xor Key[I mod Length(Key)];

  // Convertir en string
  Result := TEncoding.UTF8.GetString(Cipher);
end;

class function TSecurityUtils.HashPassword(const APassword: string): string;
var
  SHA: TIdHashSHA256;
  Hash: TIdBytes;
  Encoder: TIdEncoderMIME;
begin
  // Créer un hash du mot de passe pour stockage sécurisé
  SHA := TIdHashSHA256.Create;
  try
    Hash := SHA.HashString(APassword);

    Encoder := TIdEncoderMIME.Create;
    try
      Result := Encoder.EncodeBytes(Hash);
    finally
      Encoder.Free;
    end;
  finally
    SHA.Free;
  end;
end;

class function TSecurityUtils.VerifyPassword(const APassword, AHash: string): Boolean;
begin
  // Vérifier si le mot de passe correspond au hash stocké
  Result := HashPassword(APassword) = AHash;
end;

end.
```

## 7. Intégration de l'enregistrement audio

Ajoutons la capacité d'enregistrer des notes audio :

```pascal
// Dans les uses, ajoutez :
FMX.Media;

// Dans la classe TMemoryEditForm, ajoutez :
private
  FAudioRecorder: TAudioRecorder;
  FIsRecording: Boolean;
  FAudioFileName: string;
  procedure StartAudioRecording;
  procedure StopAudioRecording;
  procedure OnAudioRecorderFinished(const Sender: TObject; const ASuccess: Boolean);

// Implémentation :
procedure TMemoryEditForm.FormCreate(Sender: TObject);
begin
  // Initialiser l'enregistreur audio
  FAudioRecorder := TAudioRecorder.Create;
  FAudioRecorder.OnFinish := OnAudioRecorderFinished;
  FIsRecording := False;
end;

procedure TMemoryEditForm.StartAudioRecording;
begin
  // Vérifier et demander les permissions
  {$IFDEF ANDROID}
  PermissionsService.RequestPermissions([
    JStringToString(TJManifest_permission.JavaClass.RECORD_AUDIO)],
    AudioPermissionRequestResult);
  Exit;
  {$ENDIF}

  // Démarrer l'enregistrement après avoir obtenu les permissions
  StartAudioRecordingInternal;
end;

procedure TMemoryEditForm.StartAudioRecordingInternal;
begin
  if FAudioRecorder.State = TAudioRecorderState.Stopped then
  begin
    // Créer un nom de fichier unique
    FAudioFileName := TPath.Combine(
      TPath.GetDocumentsPath,
      'Audio_' + FormatDateTime('yyyymmddhhnnss', Now) + '.aac');

    // Démarrer l'enregistrement
    FAudioRecorder.FileName := FAudioFileName;
    FAudioRecorder.AudioSettings.SampleRate := 44100;
    FAudioRecorder.AudioSettings.ChannelCount := 2;

    if FAudioRecorder.Start then
    begin
      RecordButton.Text := 'Arrêter';
      FIsRecording := True;
      RecordingStatusLabel.Text := 'Enregistrement en cours...';
    end
    else
      ShowMessage('Impossible de démarrer l''enregistrement audio');
  end;
end;

procedure TMemoryEditForm.StopAudioRecording;
begin
  if FIsRecording then
  begin
    FAudioRecorder.Stop;
    RecordButton.Text := 'Enregistrer';
    FIsRecording := False;
    RecordingStatusLabel.Text := 'Enregistrement terminé';
  end;
end;

procedure TMemoryEditForm.OnAudioRecorderFinished(const Sender: TObject;
  const ASuccess: Boolean);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FIsRecording := False;
      RecordButton.Text := 'Enregistrer';

      if ASuccess then
      begin
        RecordingStatusLabel.Text := 'Enregistrement terminé avec succès';

        // Mettre à jour la mémoire avec le fichier audio
        FMemory.FilePath := FAudioFileName;
        FMemory.MemoryType := mtAudio;
      end
      else
      begin
        RecordingStatusLabel.Text := 'Erreur lors de l''enregistrement';
        ShowMessage('L''enregistrement audio a échoué');
      end;
    end);
end;
```

## 8. Mise en œuvre de la météo en temps réel

Ajoutons une fonctionnalité pour récupérer les informations météo basées sur la localisation :

```pascal
// Dans les uses, ajoutez :
System.Net.HttpClient, System.JSON;

// Dans la classe TMemoryEditForm, ajoutez :
private
  procedure GetWeatherForLocation(const Latitude, Longitude: Double);
  procedure OnWeatherRequestCompleted(const Sender: TObject;
    const AResponse: IHTTPResponse);

// Implémentation :
procedure TMemoryEditForm.GetWeatherForLocation(const Latitude, Longitude: Double);
var
  HttpClient: THTTPClient;
  URL: string;
begin
  HttpClient := THTTPClient.Create;
  try
    // Remplacez YOUR_API_KEY par votre clé API météo réelle
    URL := Format('https://api.openweathermap.org/data/2.5/weather?lat=%.6f&lon=%.6f&units=metric&appid=YOUR_API_KEY',
      [Latitude, Longitude]);

    // Note : dans une application réelle, n'exposez jamais votre clé API dans le code

    HttpClient.BeginGet(URL, nil, nil, nil, OnWeatherRequestCompleted);
  finally
    HttpClient.Free;
  end;
end;

procedure TMemoryEditForm.OnWeatherRequestCompleted(const Sender: TObject;
  const AResponse: IHTTPResponse);
var
  JsonObj: TJSONObject;
  WeatherObj: TJSONObject;
  WeatherArray: TJSONArray;
  Temperature: Double;
  WeatherDesc: string;
begin
  if AResponse.StatusCode = 200 then
  begin
    try
      JsonObj := TJSONObject.ParseJSONValue(AResponse.ContentAsString) as TJSONObject;
      if JsonObj <> nil then
      begin
        try
          // Extraire les informations météo
          WeatherArray := JsonObj.GetValue<TJSONArray>('weather');
          if (WeatherArray <> nil) and (WeatherArray.Count > 0) then
          begin
            WeatherObj := WeatherArray.Items[0] as TJSONObject;
            WeatherDesc := WeatherObj.GetValue<string>('description');
          end;

          Temperature := JsonObj.GetValue<TJSONObject>('main').GetValue<Double>('temp');

          // Formater et stocker les informations météo
          FMemory.Weather := Format('%.1f°C, %s', [Temperature, WeatherDesc]);

          // Mettre à jour l'interface
          TThread.Synchronize(nil,
            procedure
            begin
              WeatherLabel.Text := FMemory.Weather;
            end);
        finally
          JsonObj.Free;
        end;
      end;
    except
      on E: Exception do
        TThread.Synchronize(nil,
          procedure
          begin
            WeatherLabel.Text := 'Erreur: ' + E.Message;
          end);
    end;
  end
  else
    TThread.Synchronize(nil,
      procedure
      begin
        WeatherLabel.Text := 'Erreur: ' + AResponse.StatusText;
      end);
end;
```

## 9. Utilisation des Tags NFC

Implémentons la lecture de tags NFC pour associer des souvenirs à des objets physiques :

```pascal
// Dans les uses, ajoutez :
FMX.Platform.Android, Androidapi.JNI.App, Androidapi.JNI.GraphicsContentViewText,
Androidapi.JNI.Nfc, Androidapi.Helpers, Androidapi.JNI.JavaTypes;

// Ajoutez une unité dédiée au NFC :
unit NFCUtils;

interface

uses
  System.SysUtils, System.Classes, FMX.Platform.Android, Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Nfc, Androidapi.Helpers,
  Androidapi.JNI.JavaTypes;

type
  TNFCCallback = reference to procedure(const TagID: string; const TagData: string);

  TNFC = class
  private
    FCallback: TNFCCallback;
    FEnabled: Boolean;
    procedure ProcessIntent(const Intent: JIntent);
  public
    constructor Create;
    procedure EnableReading(const Callback: TNFCCallback);
    procedure DisableReading;
    property Enabled: Boolean read FEnabled;
  end;

implementation

// Implémentation simplifiée pour le tutoriel
constructor TNFC.Create;
begin
  inherited Create;
  FEnabled := False;
end;

procedure TNFC.EnableReading(const Callback: TNFCCallback);
var
  CurrentIntent: JIntent;
  NfcAdapter: JNfcAdapter;
begin
  FCallback := Callback;

  // Vérifier si NFC est disponible
  NfcAdapter := TJNfcAdapter.JavaClass.getDefaultAdapter(MainActivity);
  if NfcAdapter = nil then
    raise Exception.Create('NFC non disponible sur cet appareil');

  if not NfcAdapter.isEnabled then
    raise Exception.Create('NFC est désactivé. Veuillez l''activer dans les paramètres');

  // Traiter l'intent courant si c'est un tag NFC
  CurrentIntent := MainActivity.getIntent;
  if (CurrentIntent <> nil) and
     (JStringToString(CurrentIntent.getAction) = TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED) then
    ProcessIntent(CurrentIntent);

  FEnabled := True;
end;

procedure TNFC.DisableReading;
begin
  FCallback := nil;
  FEnabled := False;
end;

procedure TNFC.ProcessIntent(const Intent: JIntent);
var
  Action: string;
  Tag: JTag;
  Id: TJavaArray<Byte>;
  TagId: string;
  NdefMessages: TJavaObjectArray<JNdefMessage>;
  NdefRecords: TJavaObjectArray<JNdefRecord>;
  Record: JNdefRecord;
  Payload: TJavaArray<Byte>;
  TagData: string;
  I, J: Integer;
begin
  if Intent = nil then
    Exit;

  Action := JStringToString(Intent.getAction);

  // Vérifier si c'est une action NFC
  if (Action = TJNfcAdapter.JavaClass.ACTION_NDEF_DISCOVERED) or
     (Action = TJNfcAdapter.JavaClass.ACTION_TAG_DISCOVERED) or
     (Action = TJNfcAdapter.JavaClass.ACTION_TECH_DISCOVERED) then
  begin
    // Récupérer le tag
    Tag := Intent.getParcelableExtra(TJNfcAdapter.JavaClass.EXTRA_TAG);
    if Tag <> nil then
    begin
      // Récupérer l'ID du tag
      Id := Tag.getId;
      TagId := '';
      if Id <> nil then
      begin
        for I := 0 to Id.Length - 1 do
          TagId := TagId + IntToHex(Byte(Id.Items[I]), 2);
      end;

      // Récupérer les données NDEF si disponibles
      TagData := '';
      NdefMessages := Intent.getParcelableArrayExtra(TJNfcAdapter.JavaClass.EXTRA_NDEF_MESSAGES)
        as TJavaObjectArray<JNdefMessage>;

      if (NdefMessages <> nil) and (NdefMessages.Length > 0) then
      begin
        NdefRecords := NdefMessages.Items[0].getRecords;

        for J := 0 to NdefRecords.Length - 1 do
        begin
          Record := NdefRecords.Items[J];
          Payload := Record.getPayload;

          // Le premier octet est le code de format, on le saute
          if Payload.Length > 1 then
          begin
            TagData := TagData + System.SysUtils.TEncoding.UTF8.GetString(
              Payload.ToBytes, 1, Payload.Length - 1) + ' ';
          end;
        end;
      end;

      // Appeler le callback avec les infos du tag
      if Assigned(FCallback) then
        FCallback(TagId, TagData);
    end;
  end;
end;

end.

// Dans TMemoryEditForm, ajoutez :
private
  FNFC: TNFC;
  procedure EnableNFCReading;
  procedure OnNFCTagRead(const TagID, TagData: string);

// Implémentation :
procedure TMemoryEditForm.EnableNFCReading;
begin
  {$IFDEF ANDROID}
  if FNFC = nil then
    FNFC := TNFC.Create;

  try
    FNFC.EnableReading(OnNFCTagRead);
    NFCStatusLabel.Text := 'NFC activé. Approchez un tag NFC...';
  except
    on E: Exception do
      ShowMessage('Erreur NFC: ' + E.Message);
  end;
  {$ELSE}
  ShowMessage('NFC n''est disponible que sur Android');
  {$ENDIF}
end;

procedure TMemoryEditForm.OnNFCTagRead(const TagID, TagData: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      NFCStatusLabel.Text := 'Tag lu: ' + TagID;

      // Ajouter les données du tag à la description
      if Trim(TagData) <> '' then
      begin
        if Trim(DescriptionMemo.Text) <> '' then
          DescriptionMemo.Text := DescriptionMemo.Text + sLineBreak + sLineBreak;

        DescriptionMemo.Text := DescriptionMemo.Text + 'Données du tag NFC: ' + TagData;
      end;

      // Associer l'ID du tag au souvenir
      NFCTagEdit.Text := TagID;
    end);
end;
```

## 10. Reconnaissance de texte (OCR) dans les images

Implémentons la reconnaissance de texte dans les photos :

```pascal
// Cette fonctionnalité nécessite l'utilisation de la Firebase ML Kit pour Android/iOS
// Dans les uses, ajoutez pour Android :
{$IFDEF ANDROID}
Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
Androidapi.JNI.Os, FMX.Helpers.Android, Androidapi.JNI.Media;
{$ENDIF}

// Dans la classe TMemoryDetailsForm, ajoutez :
private
  procedure RecognizeTextInImage(const ImagePath: string);
  {$IFDEF ANDROID}
  procedure ProcessTextRecognition(const Bitmap: JBitmap);
  {$ENDIF}

// Implémentation :
procedure TMemoryDetailsForm.RecognizeTextButton(Sender: TObject);
begin
  if (FMemory <> nil) and (FMemory.MemoryType = mtPhoto) and
     (FMemory.FilePath <> '') and FileExists(FMemory.FilePath) then
  begin
    RecognizeTextInImage(FMemory.FilePath);
  end
  else
    ShowMessage('Aucune image disponible pour la reconnaissance de texte');
end;

procedure TMemoryDetailsForm.RecognizeTextInImage(const ImagePath: string);
{$IFDEF ANDROID}
var
  Bitmap: JBitmap;
  Options: JBitmapFactory_Options;
begin
  // Charger l'image
  Options := TJBitmapFactory_Options.JavaClass.init;
  Bitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(ImagePath), Options);

  if Bitmap <> nil then
  begin
    try
      ProcessTextRecognition(Bitmap);
    finally
      // Le garbage collector Java s'occupera de libérer le bitmap
    end;
  end
  else
    ShowMessage('Impossible de charger l''image');
{$ELSE}
begin
  ShowMessage('La reconnaissance de texte est disponible uniquement sur Android');
{$ENDIF}
end;

{$IFDEF ANDROID}
procedure TMemoryDetailsForm.ProcessTextRecognition(const Bitmap: JBitmap);
var
  Image: JFirebaseVisionImage;
  TextRecognizer: JFirebaseVisionTextRecognizer;
  Task: JTask;
begin
  // Note : Ce code est un exemple et nécessite l'intégration de Firebase ML Kit
  // que nous ne pouvons pas détailler complètement dans ce tutoriel

  // Créer une image FirebaseVision
  Image := TJFirebaseVisionImage.JavaClass.fromBitmap(Bitmap);

  // Obtenir le détecteur de texte
  TextRecognizer := TJFirebaseVision.JavaClass.getInstance.getOnDeviceTextRecognizer;

  // Processus asynchrone de reconnaissance
  Task := TextRecognizer.processImage(Image);

  // Ajouter un listener pour traiter le résultat
  Task.addOnSuccessListener(TJOnSuccessListener.JavaClass.init(
    // Implémentation simplifiée, il faudrait créer une classe Java pour l'interface OnSuccessListener
    procedure(Text: JFirebaseVisionText)
    var
      ResultText: string;
      TextBlocks: JList;
      I: Integer;
      Block: JFirebaseVisionText_TextBlock;
    begin
      ResultText := '';

      // Extraire le texte des blocs
      TextBlocks := Text.getTextBlocks;
      for I := 0 to TextBlocks.size - 1 do
      begin
        Block := TextBlocks.get(I) as JFirebaseVisionText_TextBlock;
        ResultText := ResultText + JStringToString(Block.getText) + sLineBreak;
      end;

      // Afficher le résultat
      TThread.Synchronize(nil,
        procedure
        begin
          if ResultText <> '' then
            ShowMessage('Texte détecté:' + sLineBreak + ResultText)
          else
            ShowMessage('Aucun texte détecté dans l''image');
        end);
    end));

  Task.addOnFailureListener(TJOnFailureListener.JavaClass.init(
    procedure(E: JException)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          ShowMessage('Erreur lors de la reconnaissance de texte: ' +
            JStringToString(E.getMessage));
        end);
    end));
end;
{$ENDIF}
```

## 11. Reconnaissance faciale

Ajoutons la détection et reconnaissance de visages dans les photos :

```pascal
// Cette fonctionnalité utilise également Firebase ML Kit
// Méthode similaire à la reconnaissance de texte, mais avec le détecteur de visages

{$IFDEF ANDROID}
procedure TMemoryDetailsForm.DetectFacesInImage(const ImagePath: string);
var
  Bitmap: JBitmap;
  Options: JBitmapFactory_Options;
  Image: JFirebaseVisionImage;
  FaceDetector: JFirebaseVisionFaceDetector;
  DetectorOptions: JFirebaseVisionFaceDetectorOptions;
  Task: JTask;
begin
  // Charger l'image
  Options := TJBitmapFactory_Options.JavaClass.init;
  Bitmap := TJBitmapFactory.JavaClass.decodeFile(StringToJString(ImagePath), Options);

  if Bitmap <> nil then
  begin
    try
      // Créer une image FirebaseVision
      Image := TJFirebaseVisionImage.JavaClass.fromBitmap(Bitmap);

      // Configurer le détecteur de visages
      DetectorOptions := TJFirebaseVisionFaceDetectorOptions.JavaClass.Builder
        .setPerformanceMode(TJFirebaseVisionFaceDetectorOptions.JavaClass.ACCURATE)
        .setLandmarkMode(TJFirebaseVisionFaceDetectorOptions.JavaClass.ALL_LANDMARKS)
        .setClassificationMode(TJFirebaseVisionFaceDetectorOptions.JavaClass.ALL_CLASSIFICATIONS)
        .build;

      // Obtenir le détecteur
      FaceDetector := TJFirebaseVision.JavaClass.getInstance.getVisionFaceDetector(DetectorOptions);

      // Processus asynchrone de détection
      Task := FaceDetector.detectInImage(Image);

      // Ajouter un listener pour traiter le résultat
      Task.addOnSuccessListener(TJOnSuccessListener.JavaClass.init(
        procedure(Faces: JList)
        var
          ResultText: string;
          I: Integer;
          Face: JFirebaseVisionFace;
        begin
          ResultText := Format('Nombre de visages détectés: %d', [Faces.size]);

          for I := 0 to Faces.size - 1 do
          begin
            Face := Faces.get(I) as JFirebaseVisionFace;
            ResultText := ResultText + sLineBreak +
              Format('Visage %d - Sourire: %.1f%%, Œil gauche: %.1f%%, Œil droit: %.1f%%',
                [I + 1,
                 Face.getSmilingProbability * 100,
                 Face.getLeftEyeOpenProbability * 100,
                 Face.getRightEyeOpenProbability * 100]);
          end;

          // Afficher le résultat
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage(ResultText);
            end);
        end));

      Task.addOnFailureListener(TJOnFailureListener.JavaClass.init(
        procedure(E: JException)
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur lors de la détection de visages: ' +
                JStringToString(E.getMessage));
            end);
        end));
    finally
      // Le garbage collector Java s'occupera de libérer le bitmap
    end;
  end
  else
    ShowMessage('Impossible de charger l''image');
end;
{$ENDIF}
```

## 12. Réalité augmentée simple

Créons une fonctionnalité de réalité augmentée pour visualiser des souvenirs virtuels dans l'environnement réel :

```pascal
// Cette fonctionnalité est très spécifique à la plateforme et nécessite
// l'intégration d'ARCore (Android) ou ARKit (iOS)
// Voici un exemple simplifié pour Android avec ARCore

// Dans un nouveau fichier ARViewFormUnit.pas :
unit ARViewFormUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation, FMX.Media, DataModelUnit
  {$IFDEF ANDROID}
  , Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText,
  FMX.Platform.Android
  {$ENDIF};

type
  TARViewForm = class(TForm)
    ToolBar1: TToolBar;
    BackButton: TButton;
    TitleLabel: TLabel;
    CameraComponent: TCameraComponent;
    ARViewport: TViewport3D;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
  private
    FTrip: TTrip;
    procedure InitializeAR;
    procedure PlaceMemoriesInAR;
  public
    procedure ShowARView(Trip: TTrip);
  end;

implementation

{$R *.fmx}

procedure TARViewForm.FormCreate(Sender: TObject);
begin
  // Initialisation de base
  CameraComponent.Active := False;
end;

procedure TARViewForm.FormDestroy(Sender: TObject);
begin
  // Nettoyage des ressources
  CameraComponent.Active := False;
  {$IFDEF ANDROID}
  // Nettoyage spécifique ARCore ici
  {$ENDIF}
end;

procedure TARViewForm.ShowARView(Trip: TTrip);
begin
  FTrip := Trip;
  TitleLabel.Text := 'AR : ' + Trip.Title;

  // Initialiser la réalité augmentée
  InitializeAR;

  // Placer les souvenirs virtuellement
  PlaceMemoriesInAR;

  // Afficher le formulaire
  ShowModal;
end;

procedure TARViewForm.InitializeAR;
begin
  // Initialiser la caméra du périphérique
  CameraComponent.Active := True;

  {$IFDEF ANDROID}
  // Initialisation d'ARCore - code simplifié
  // Dans une application réelle, vous devriez utiliser un wrapper Delphi pour ARCore
  // ou créer une interface Java Native (JNI) pour ARCore
  {$ENDIF}

  {$IFDEF IOS}
  // Initialisation d'ARKit - code simplifié
  // Dans une application réelle, vous devriez utiliser un wrapper Delphi pour ARKit
  {$ENDIF}
end;

procedure TARViewForm.PlaceMemoriesInAR;
var
  Memory: TTripMemory;
begin
  // Pour chaque souvenir avec géolocalisation, placer un marqueur virtuel
  for Memory in FTrip.Memories do
  begin
    if (Memory.Location.Latitude <> 0) or (Memory.Location.Longitude <> 0) then
    begin
      // Code simplifié - dans une application réelle, vous utiliseriez la position réelle
      // de l'utilisateur et calculeriez la position relative des souvenirs

      // Créer un objet 3D pour représenter le souvenir
      {$IFDEF ANDROID}
      // Utilisation d'ARCore pour placer des ancres et des objets 3D
      {$ENDIF}

      {$IFDEF IOS}
      // Utilisation d'ARKit pour placer des ancres et des objets 3D
      {$ENDIF}
    end;
  end;
end;

procedure TARViewForm.BackButtonClick(Sender: TObject);
begin
  Close;
end;

end.
```

## 13. Sauvegarde et synchronisation cloud

Implémentons une solution de sauvegarde des données vers un service cloud comme Firebase :

```pascal
// Créez une unité dédiée à la synchronisation cloud :
unit CloudSyncUnit;

interface

uses
  System.SysUtils, System.Classes, System.JSON, System.Net.HTTPClient,
  System.Net.URLClient, DataModelUnit;

type
  TCloudSyncStatus = (cssNone, cssSyncing, cssSynced, cssError);

  TCloudSyncStatusEvent = procedure(const Status: TCloudSyncStatus;
    const Message: string) of object;

  TCloudSync = class
  private
    FAPIKey: string;
    FUserID: string;
    FBaseURL: string;
    FOnSyncStatus: TCloudSyncStatusEvent;

    procedure DoSyncStatus(const Status: TCloudSyncStatus; const Msg: string);
    function GetAuthToken: string;
  public
    constructor Create(const APIKey, UserID: string);

    procedure SyncTrip(Trip: TTrip);
    procedure SyncAllTrips(Trips: TObjectList<TTrip>);
    procedure DownloadTrips(const OnComplete: TProc<TObjectList<TTrip>>);

    property OnSyncStatus: TCloudSyncStatusEvent read FOnSyncStatus write FOnSyncStatus;
  end;

implementation

constructor TCloudSync.Create(const APIKey, UserID: string);
begin
  inherited Create;
  FAPIKey := APIKey;
  FUserID := UserID;
  FBaseURL := 'https://your-firebase-project.firebaseio.com/'; // Remplacez par votre URL Firebase
end;

procedure TCloudSync.DoSyncStatus(const Status: TCloudSyncStatus; const Msg: string);
begin
  if Assigned(FOnSyncStatus) then
    FOnSyncStatus(Status, Msg);
end;

function TCloudSync.GetAuthToken: string;
var
  HttpClient: THTTPClient;
  URL: string;
  RequestContent: TStringStream;
  Response: IHTTPResponse;
  JsonObj: TJSONObject;
begin
  Result := '';

  HttpClient := THTTPClient.Create;
  RequestContent := TStringStream.Create('', TEncoding.UTF8);
  try
    // Construire la requête d'authentification Firebase
    // Note: Ceci est une version simplifiée, dans une application réelle
    // vous devriez implémenter une authentification complète avec Firebase
    JsonObj := TJSONObject.Create;
    try
      JsonObj.AddPair('key', FAPIKey);
      JsonObj.AddPair('uid', FUserID);
      RequestContent.WriteString(JsonObj.ToJSON);
    finally
      JsonObj.Free;
    end;

    URL := 'https://identitytoolkit.googleapis.com/v1/accounts:signInWithCustomToken?key=' + FAPIKey;

    // Envoyer la requête
    Response := HttpClient.Post(URL, RequestContent, nil, TNetHeaders.Create);

    if Response.StatusCode = 200 then
    begin
      // Extraire le token d'authentification
      JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      if JsonObj <> nil then
      begin
        try
          Result := JsonObj.GetValue<string>('idToken');
        finally
          JsonObj.Free;
        end;
      end;
    end
    else
      DoSyncStatus(cssError, 'Erreur d''authentification: ' + Response.StatusText);
  finally
    RequestContent.Free;
    HttpClient.Free;
  end;
end;

procedure TCloudSync.SyncTrip(Trip: TTrip);
var
  HttpClient: THTTPClient;
  URL: string;
  RequestContent: TStringStream;
  Response: IHTTPResponse;
  AuthToken: string;
  TripJson: TJSONObject;
begin
  DoSyncStatus(cssSyncing, 'Synchronisation en cours...');

  // Obtenir un token d'authentification
  AuthToken := GetAuthToken;
  if AuthToken = '' then
  begin
    DoSyncStatus(cssError, 'Échec d''authentification');
    Exit;
  end;

  HttpClient := THTTPClient.Create;
  try
    // Convertir le voyage en JSON
    TripJson := Trip.ToJSON;
    RequestContent := TStringStream.Create(TripJson.ToJSON, TEncoding.UTF8);
    TripJson.Free;

    try
      // Construire l'URL
      URL := FBaseURL + 'users/' + FUserID + '/trips/' + GUIDToString(Trip.ID).Replace('{', '').Replace('}', '') + '.json';
      URL := URL + '?auth=' + AuthToken;

      // Envoyer les données
      Response := HttpClient.Put(URL, RequestContent, nil, TNetHeaders.Create);

      if Response.StatusCode = 200 then
        DoSyncStatus(cssSynced, 'Synchronisation réussie')
      else
        DoSyncStatus(cssError, 'Erreur de synchronisation: ' + Response.StatusText);
    finally
      RequestContent.Free;
    end;
  finally
    HttpClient.Free;
  end;
end;

procedure TCloudSync.SyncAllTrips(Trips: TObjectList<TTrip>);
var
  Trip: TTrip;
begin
  for Trip in Trips do
  begin
    SyncTrip(Trip);
    // Ajouter un délai pour ne pas saturer l'API
    Sleep(500);
  end;
end;

procedure TCloudSync.DownloadTrips(const OnComplete: TProc<TObjectList<TTrip>>);
var
  HttpClient: THTTPClient;
  URL: string;
  Response: IHTTPResponse;
  AuthToken: string;
  JsonObj, TripObj: TJSONObject;
  JsonPair: TJSONPair;
  Trips: TObjectList<TTrip>;
  Trip: TTrip;
begin
  DoSyncStatus(cssSyncing, 'Téléchargement des données...');

  // Obtenir un token d'authentification
  AuthToken := GetAuthToken;
  if AuthToken = '' then
  begin
    DoSyncStatus(cssError, 'Échec d''authentification');
    if Assigned(OnComplete) then
      OnComplete(nil);
    Exit;
  end;

  Trips := TObjectList<TTrip>.Create(True);
  HttpClient := THTTPClient.Create;
  try
    // Construire l'URL
    URL := FBaseURL + 'users/' + FUserID + '/trips.json';
    URL := URL + '?auth=' + AuthToken;

    // Récupérer les données
    Response := HttpClient.Get(URL);

    if Response.StatusCode = 200 then
    begin
      JsonObj := TJSONObject.ParseJSONValue(Response.ContentAsString) as TJSONObject;
      if JsonObj <> nil then
      begin
        try
          // Parcourir tous les voyages
          for JsonPair in JsonObj do
          begin
            TripObj := JsonPair.JsonValue as TJSONObject;

            Trip := TTrip.Create;
            Trip.FromJSON(TripObj);
            Trips.Add(Trip);
          end;

          DoSyncStatus(cssSynced, 'Téléchargement terminé');
        finally
          JsonObj.Free;
        end;
      end;
    end
    else
    begin
      DoSyncStatus(cssError, 'Erreur de téléchargement: ' + Response.StatusText);
      FreeAndNil(Trips);
    end;
  finally
    HttpClient.Free;

    if Assigned(OnComplete) then
      OnComplete(Trips);
  end;
end;

end.
```

## 14. Utilisation de Notifications Push

Implémentons des notifications push pour informer les utilisateurs de nouvelles fonctionnalités ou rappels importants :

```pascal
// Intégration de Firebase Cloud Messaging (FCM) pour les notifications push
// Cette unité fournie est simplifiée et nécessite une configuration complète de FCM

unit PushNotificationManager;

interface

uses
  System.SysUtils, System.Classes, FMX.PushNotification, System.Messaging
  {$IFDEF ANDROID}
  , Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.App, Androidapi.Helpers,
  Androidapi.JNI.Os, FMX.Helpers.Android
  {$ENDIF};

type
  TPushNotificationManager = class
  private
    FPushService: TPushService;
    FDeviceToken: string;
    FIsRegistered: Boolean;

    procedure RegisterRemoteNotifications;
    procedure ReceiveNotificationEvent(Sender: TObject; const ServiceNotification: TPushServiceNotification);
    procedure ReceiveDeviceTokenEvent(Sender: TObject; const DeviceToken: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure SubscribeToTopic(const Topic: string);

    property DeviceToken: string read FDeviceToken;
    property IsRegistered: Boolean read FIsRegistered;
  end;

implementation

constructor TPushNotificationManager.Create;
begin
  inherited Create;
  FIsRegistered := False;
  FDeviceToken := '';
end;

destructor TPushNotificationManager.Destroy;
begin
  if Assigned(FPushService) then
  begin
    FPushService.OnNotification := nil;
    FPushService.OnDeviceTokenReceived := nil;
    FPushService.OnServiceDestroy := nil;
    FreeAndNil(FPushService);
  end;

  inherited;
end;

procedure TPushNotificationManager.Initialize;
begin
  // Créer le service de notification
  FPushService := TPushServiceManager.Instance.GetServiceByName(TPushService.TServiceNames.FCM);

  if FPushService <> nil then
  begin
    // Définir les gestionnaires d'événements
    FPushService.OnNotification := ReceiveNotificationEvent;
    FPushService.OnDeviceTokenReceived := ReceiveDeviceTokenEvent;

    // Démarrer le service
    RegisterRemoteNotifications;
  end
  else
    raise Exception.Create('Service de notification Firebase non disponible');
end;

procedure TPushNotificationManager.RegisterRemoteNotifications;
begin
  if FPushService <> nil then
  begin
    FPushService.AppProps[TPushService.TAppPropNames.GCMAppID] := 'YOUR_FCM_SENDER_ID'; // Remplacez par votre ID Firebase

    // Demander l'autorisation et s'enregistrer pour les notifications push
    {$IFDEF IOS}
    // Sur iOS, nous devons demander l'autorisation à l'utilisateur
    FPushService.AppProps[TPushService.TAppPropNames.RequestPermission] := 'true';
    {$ENDIF}

    // S'enregistrer
    FPushService.StartService;
  end;
end;

procedure TPushNotificationManager.ReceiveNotificationEvent(Sender: TObject;
  const ServiceNotification: TPushServiceNotification);
var
  Notification: TPushServiceNotification;
  NotificationCenter: TNotificationCenter;
  LocalNotification: TNotification;
begin
  Notification := ServiceNotification;

  // Traiter la notification push reçue
  TThread.Synchronize(nil,
    procedure
    begin
      // Si l'application est en premier plan, afficher une notification locale
      NotificationCenter := TNotificationCenter.Create(nil);
      try
        LocalNotification := NotificationCenter.CreateNotification;
        try
          LocalNotification.Title := Notification.DataObject.GetValue('title', 'Notification');
          LocalNotification.AlertBody := Notification.DataObject.GetValue('body', '');

          // Présenter la notification
          NotificationCenter.PresentNotification(LocalNotification);
        finally
          LocalNotification.Free;
        end;
      finally
        NotificationCenter.Free;
      end;

      // Déclencher un événement système que d'autres parties de l'application peuvent écouter
      TMessageManager.DefaultManager.SendMessage(nil,
        TMessage<TPushServiceNotification>.Create(Notification));
    end);
end;

procedure TPushNotificationManager.ReceiveDeviceTokenEvent(Sender: TObject;
  const DeviceToken: string);
begin
  FDeviceToken := DeviceToken;
  FIsRegistered := True;

  // Enregistrer le token sur votre serveur backend
  // Ce code dépend de votre implémentation spécifique

  // Souscrire aux sujets par défaut
  SubscribeToTopic('general');
end;

procedure TPushNotificationManager.SubscribeToTopic(const Topic: string);
begin
  if (FPushService <> nil) and FIsRegistered then
  begin
    {$IFDEF ANDROID}
    // Sur Android, nous pouvons utiliser la méthode native de Firebase
    // Ceci est une simplification, vous devriez implémenter correctement l'accès JNI
    var FirebaseMessaging := TJFirebaseMessaging.JavaClass.getInstance;
    FirebaseMessaging.subscribeToTopic(StringToJString(Topic));
    {$ENDIF}

    {$IFDEF IOS}
    // Sur iOS, les sujets doivent être gérés côté serveur
    // Vous devez envoyer le DeviceToken à votre backend et l'associer au sujet
    {$ENDIF}
  end;
end;

end.
```

## 15. Authentification biométrique

Implémentons l'authentification par empreinte digitale ou reconnaissance faciale pour sécuriser l'accès à l'application :

```pascal
// Créez une unité dédiée à l'authentification biométrique
unit BiometricAuth;

interface

uses
  System.SysUtils, System.Classes, FMX.DialogService,
  {$IFDEF ANDROID}
  Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.Support, Androidapi.JNI.Hardware.Fingerprint,
  FMX.Platform.Android, Androidapi.JNI.Biometric, Androidapi.JNI.Core
  {$ENDIF}
  {$IFDEF IOS}
  iOSapi.Foundation, FMX.Platform.iOS, Macapi.ObjectiveC, iOSapi.UIKit, iOSapi.LocalAuthentication
  {$ENDIF};

type
  TBiometricAuthResult = (barSuccess, barError, barCancelled, barNotAvailable);
  TBiometricAuthCallback = reference to procedure(const Result: TBiometricAuthResult);

  TBiometricAuth = class
  private
    FCallback: TBiometricAuthCallback;
    {$IFDEF ANDROID}
    FBiometricPrompt: JBiometricPrompt;
    FAuthCallback: JBiometricPrompt_AuthenticationCallback;
    procedure SetupAndroidBiometric;
    function GetMainActivity: JActivity;
    {$ENDIF}
    {$IFDEF IOS}
    FContext: LAContext;
    function CanAuthenticateWithBiometrics: Boolean;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function IsBiometricAvailable: Boolean;
    procedure Authenticate(const Reason: string; const Callback: TBiometricAuthCallback);
  end;

implementation

constructor TBiometricAuth.Create;
begin
  inherited Create;

  {$IFDEF ANDROID}
  SetupAndroidBiometric;
  {$ENDIF}

  {$IFDEF IOS}
  FContext := TLAContext.Create;
  {$ENDIF}
end;

destructor TBiometricAuth.Destroy;
begin
  {$IFDEF IOS}
  FContext.release;
  {$ENDIF}

  inherited;
end;

function TBiometricAuth.IsBiometricAvailable: Boolean;
begin
  {$IFDEF ANDROID}
  // Vérifier si l'authentification biométrique est disponible sur Android
  var BiometricManager := TJBiometricManager.JavaClass.from(GetMainActivity);
  Result := BiometricManager.canAuthenticate(TJBiometricManager.JavaClass.BIOMETRIC_STRONG) =
    TJBiometricManager.JavaClass.BIOMETRIC_SUCCESS;
  {$ENDIF}

  {$IFDEF IOS}
  // Vérifier si l'authentification biométrique est disponible sur iOS
  Result := CanAuthenticateWithBiometrics;
  {$ENDIF}

  {$IF not Defined(ANDROID) and not Defined(IOS)}
  Result := False;
  {$ENDIF}
end;

procedure TBiometricAuth.Authenticate(const Reason: string; const Callback: TBiometricAuthCallback);
begin
  FCallback := Callback;

  if not IsBiometricAvailable then
  begin
    if Assigned(FCallback) then
      FCallback(barNotAvailable);
    Exit;
  end;

  {$IFDEF ANDROID}
  // Créer et afficher le prompt biométrique Android
  var Executor := TJExecutors.JavaClass.newSingleThreadExecutor;
  var PromptInfo := TJBiometricPrompt_PromptInfo.JavaClass.Builder
    .setTitle(StringToJString('Authentification requise'))
    .setSubtitle(StringToJString(Reason))
    .setNegativeButtonText(StringToJString('Annuler'))
    .build;

  TJActivity(GetMainActivity).runOnUiThread(TJRunnable.JavaClass.init(
    procedure
    begin
      FBiometricPrompt.authenticate(PromptInfo);
    end));
  {$ENDIF}

  {$IFDEF IOS}
  // Authentification biométrique iOS
  var ErrorRef: Pointer;
  if FContext.canEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, @ErrorRef) then
  begin
    FContext.evaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics,
      StringToNSString(Reason),
      procedure(Success: Boolean; Error: NSError)
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            if Success then
            begin
              if Assigned(FCallback) then
                FCallback(barSuccess);
            end
            else
            begin
              if Error <> nil then
              begin
                if Error.code = LAError.UserCancel then
                begin
                  if Assigned(FCallback) then
                    FCallback(barCancelled);
                end
                else
                begin
                  if Assigned(FCallback) then
                    FCallback(barError);
                end;
              end;
            end;
          end);
      end);
  end
  else
  begin
    if Assigned(FCallback) then
      FCallback(barNotAvailable);
  end;
  {$ENDIF}

  {$IF not Defined(ANDROID) and not Defined(IOS)}
  if Assigned(FCallback) then
    FCallback(barNotAvailable);
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TBiometricAuth.SetupAndroidBiometric;
begin
  // Créer le callback d'authentification
  FAuthCallback := TJBiometricPrompt_AuthenticationCallback.JavaClass.init(
    procedure(result: JBiometricPrompt_AuthenticationResult)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FCallback) then
            FCallback(barSuccess);
        end);
    end,

    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FCallback) then
            FCallback(barError);
        end);
    end,

    procedure
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if Assigned(FCallback) then
            FCallback(barCancelled);
        end);
    end);

  // Créer le prompt biométrique
  var Executor := TJExecutors.JavaClass.newSingleThreadExecutor;
  FBiometricPrompt := TJBiometricPrompt.JavaClass.init(GetMainActivity, Executor, FAuthCallback);
end;

function TBiometricAuth.GetMainActivity: JActivity;
begin
  Result := TAndroidHelper.Activity;
end;
{$ENDIF}

{$IFDEF IOS}
function TBiometricAuth.CanAuthenticateWithBiometrics: Boolean;
var
  ErrorRef: Pointer;
begin
  Result := FContext.canEvaluatePolicy(LAPolicy.DeviceOwnerAuthenticationWithBiometrics, @ErrorRef);
end;
{$ENDIF}

end.
```

## 16. Intégration des services de paiement in-app

Implémentons l'achat in-app pour débloquer des fonctionnalités premium :

```pascal
// Cette fonctionnalité nécessite l'intégration des services de paiement spécifiques à la plateforme
// Note: Cet exemple est simplifié et nécessite une implémentation complète
// des services de facturation Google/Apple

unit InAppPurchaseManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections
  {$IFDEF ANDROID}
  , Androidapi.JNI.JavaTypes, Androidapi.Helpers, Androidapi.JNI.App,
  Androidapi.JNI.Billing, FMX.Helpers.Android
  {$ENDIF}
  {$IFDEF IOS}
  , Macapi.ObjectiveC, iOSapi.Foundation, iOSapi.StoreKit, FMX.Helpers.iOS
  {$ENDIF};

type
  TPurchaseResult = (prSuccess, prError, prCancelled);
  TPurchaseCallback = reference to procedure(const Result: TPurchaseResult; const ProductID: string);

  TProductInfo = record
    ID: string;
    Title: string;
    Description: string;
    Price: Currency;
    CurrencyCode: string;
  end;

  TInAppPurchaseManager = class
  private
    FProductIDs: TStringList;
    FProductInfos: TDictionary<string, TProductInfo>;
    FCallback: TPurchaseCallback;
    FInitialized: Boolean;

    {$IFDEF ANDROID}
    FBillingClient: JBillingClient;
    FPurchasesUpdatedListener: JBillingClient_PurchasesUpdatedListener;
    procedure SetupAndroidBilling;
    procedure QueryAndroidProducts;
    {$ENDIF}

    {$IFDEF IOS}
    FProductsRequest: SKProductsRequest;
    FStoreObserver: id;
    procedure SetupiOSStore;
    procedure QueryiOSProducts;
    function ProductRequestDelegate: NSObject;
    function PaymentTransactionObserver: NSObject;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize;
    procedure AddProduct(const ProductID: string);
    procedure PurchaseProduct(const ProductID: string; const Callback: TPurchaseCallback);
    function GetProductInfo(const ProductID: string): TProductInfo;
    function IsProductPurchased(const ProductID: string): Boolean;
    procedure RestorePurchases;

    property Initialized: Boolean read FInitialized;
  end;

implementation

constructor TInAppPurchaseManager.Create;
begin
  inherited Create;
  FInitialized := False;
  FProductIDs := TStringList.Create;
  FProductInfos := TDictionary<string, TProductInfo>.Create;
end;

destructor TInAppPurchaseManager.Destroy;
begin
  FProductIDs.Free;
  FProductInfos.Free;

  inherited;
end;

procedure TInAppPurchaseManager.Initialize;
begin
  {$IFDEF ANDROID}
  SetupAndroidBilling;
  {$ENDIF}

  {$IFDEF IOS}
  SetupiOSStore;
  {$ENDIF}

  FInitialized := True;
end;

procedure TInAppPurchaseManager.AddProduct(const ProductID: string);
begin
  if not FProductIDs.Contains(ProductID) then
  begin
    FProductIDs.Add(ProductID);

    // Requête des informations de produit une fois que nous avons des IDs
    if FInitialized and (FProductIDs.Count > 0) then
    begin
      {$IFDEF ANDROID}
      QueryAndroidProducts;
      {$ENDIF}

      {$IFDEF IOS}
      QueryiOSProducts;
      {$ENDIF}
    end;
  end;
end;

function TInAppPurchaseManager.GetProductInfo(const ProductID: string): TProductInfo;
begin
  if FProductInfos.ContainsKey(ProductID) then
    Result := FProductInfos[ProductID]
  else
  begin
    Result.ID := ProductID;
    Result.Title := 'Produit inconnu';
    Result.Description := '';
    Result.Price := 0;
    Result.CurrencyCode := '';
  end;
end;

procedure TInAppPurchaseManager.PurchaseProduct(const ProductID: string; const Callback: TPurchaseCallback);
begin
  if not FInitialized then
  begin
    if Assigned(Callback) then
      Callback(prError, ProductID);
    Exit;
  end;

  FCallback := Callback;

  {$IFDEF ANDROID}
  // Lancer l'achat sur Android
  var FlowParams := TJBillingFlowParams.JavaClass.newBuilder
    .setSkuDetails(FSkuDetailsMap.get(StringToJString(ProductID)) as JSkuDetails)
    .build;

  var ResponseCode := FBillingClient.launchBillingFlow(TAndroidHelper.Activity, FlowParams);

  if ResponseCode <> TJBillingClient.JavaClass.BillingResponseCode.OK then
  begin
    if Assigned(FCallback) then
      FCallback(prError, ProductID);
  end;
  {$ENDIF}

  {$IFDEF IOS}
  // Lancer l'achat sur iOS
  var Product := FProductMap.objectForKey(StringToID(ProductID));
  if (Product <> nil) and (Product is SKProduct) then
  begin
    var Payment := TSKPayment.Wrap(TSKPayment.OCClass.paymentWithProduct(SKProduct(Product)));
    TSKPaymentQueue.OCClass.defaultQueue.addPayment(Payment);
  end
  else
  begin
    if Assigned(FCallback) then
      FCallback(prError, ProductID);
  end;
  {$ENDIF}
end;

function TInAppPurchaseManager.IsProductPurchased(const ProductID: string): Boolean;
begin
  Result := False;

  if not FInitialized then
    Exit;

  {$IFDEF ANDROID}
  // Vérifier si le produit est acheté sur Android
  var PurchasesResult := FBillingClient.queryPurchases(TJBillingClient.JavaClass.SkuType.INAPP);

  if PurchasesResult.getResponseCode = TJBillingClient.JavaClass.BillingResponseCode.OK then
  begin
    var Purchases := PurchasesResult.getPurchasesList;
    if (Purchases <> nil) and (Purchases.size > 0) then
    begin
      for var I := 0 to Purchases.size - 1 do
      begin
        var Purchase := Purchases.get(I) as JPurchase;
        if JStringToString(Purchase.getSku) = ProductID then
        begin
          Result := Purchase.getPurchaseState = TJPurchase.JavaClass.PurchaseState.PURCHASED;
          Break;
        end;
      end;
    end;
  end;
  {$ENDIF}

  {$IFDEF IOS}
  // Vérifier si le produit est acheté sur iOS
  // Généralement, vous stockez cette information localement après un achat réussi
  // ou utilisez StoreKit pour vérifier les reçus
  var Defaults := TNSUserDefaults.Wrap(TNSUserDefaults.OCClass.standardUserDefaults);
  var PurchasedProducts := TNSArray.Wrap(Defaults.arrayForKey(NSStr('PurchasedProducts')));

  if PurchasedProducts <> nil then
  begin
    for var I := 0 to PurchasedProducts.count - 1 do
    begin
      var ProductIdObj := TNSString.Wrap(PurchasedProducts.objectAtIndex(I));
      if NSStrToStr(ProductIdObj) = ProductID then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
  {$ENDIF}
end;

procedure TInAppPurchaseManager.RestorePurchases;
begin
  if not FInitialized then
    Exit;

  {$IFDEF ANDROID}
  // Restaurer les achats sur Android
  // Les achats sont généralement déjà disponibles via queryPurchases
  {$ENDIF}

  {$IFDEF IOS}
  // Restaurer les achats sur iOS
  TSKPaymentQueue.OCClass.defaultQueue.restoreCompletedTransactions;
  {$ENDIF}
end;

// Les implémentations spécifiques à la plateforme sont simplifiées
// Une implémentation complète nécessiterait beaucoup plus de code et d'interaction
// avec les API natives des plateformes

end.
```

## 17. Gestion avancée de l'appareil photo

Implémentons des fonctionnalités avancées pour l'appareil photo comme les filtres et les modes de capture :

```pascal
// Ajoutons une unité de gestion avancée de l'appareil photo

unit AdvancedCameraManager;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.Graphics, FMX.Types, FMX.Media, FMX.Platform, System.Permissions;

type
  TCameraFilterType = (cftNone, cftGrayscale, cftSepia, cftNegative, cftVintage);
  TCameraFlashMode = (cfmAuto, cfmOn, cfmOff);

  TAdvancedCameraManager = class
  private
    FCameraComponent: TCameraComponent;
    FCurrentFilter: TCameraFilterType;
    FFlashMode: TCameraFlashMode;
    FAvailableCameras: TArray<TCameraDescription>;
    FCurrentCameraIndex: Integer;

    function ApplyFilter(const Bitmap: TBitmap; FilterType: TCameraFilterType): TBitmap;
    function ApplyGrayscaleFilter(const Bitmap: TBitmap): TBitmap;
    function ApplySepiaFilter(const Bitmap: TBitmap): TBitmap;
    function ApplyNegativeFilter(const Bitmap: TBitmap): TBitmap;
    function ApplyVintageFilter(const Bitmap: TBitmap): TBitmap;
    procedure UpdateFlashMode;
  public
    constructor Create(ACameraComponent: TCameraComponent);
    destructor Destroy; override;

    procedure StartCamera;
    procedure StopCamera;
    procedure TakePhoto(const FileName: string);
    procedure SwitchCamera;
    procedure SetFilter(FilterType: TCameraFilterType);
    procedure SetFlashMode(FlashMode: TCameraFlashMode);

    property CurrentFilter: TCameraFilterType read FCurrentFilter write SetFilter;
    property FlashMode: TCameraFlashMode read FFlashMode write SetFlashMode;
  end;

implementation

constructor TAdvancedCameraManager.Create(ACameraComponent: TCameraComponent);
begin
  inherited Create;
  FCameraComponent := ACameraComponent;
  FCurrentFilter := cftNone;
  FFlashMode := cfmAuto;

  // Obtenir la liste des caméras disponibles
  FAvailableCameras := TCameraManager.Current.GetCameraDescriptions;
  FCurrentCameraIndex := 0;

  // Configurer la caméra initiale
  if Length(FAvailableCameras) > 0 then
  begin
    FCameraComponent.CameraDescription := FAvailableCameras[FCurrentCameraIndex];
  end;
end;

destructor TAdvancedCameraManager.Destroy;
begin
  StopCamera;
  inherited;
end;

procedure TAdvancedCameraManager.StartCamera;
begin
  // Démarrer la caméra si elle n'est pas déjà active
  if not FCameraComponent.Active then
  begin
    // Vérifier et demander les permissions nécessaires
    {$IFDEF ANDROID}
    PermissionsService.RequestPermissions([JStringToString(
      TJManifest_permission.JavaClass.CAMERA)],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(AGrantResults) = 1) and
           (AGrantResults[0] = TPermissionStatus.Granted) then
        begin
          FCameraComponent.Active := True;
          UpdateFlashMode;
        end;
      end);
    {$ELSE}
    FCameraComponent.Active := True;
    UpdateFlashMode;
    {$ENDIF}
  end;
end;

procedure TAdvancedCameraManager.StopCamera;
begin
  // Arrêter la caméra si elle est active
  if FCameraComponent.Active then
    FCameraComponent.Active := False;
end;

procedure TAdvancedCameraManager.TakePhoto(const FileName: string);
var
  Bitmap: TBitmap;
  FilteredBitmap: TBitmap;
begin
  // Vérifier si la caméra est active
  if not FCameraComponent.Active then
    Exit;

  // Capturer l'image
  Bitmap := TBitmap.Create;
  try
    FCameraComponent.SampleBufferToBitmap(Bitmap, True);

    // Appliquer un filtre si nécessaire
    if FCurrentFilter <> cftNone then
    begin
      FilteredBitmap := ApplyFilter(Bitmap, FCurrentFilter);
      try
        // Sauvegarder l'image filtrée
        FilteredBitmap.SaveToFile(FileName);
      finally
        FilteredBitmap.Free;
      end;
    end
    else
      // Sauvegarder l'image originale
      Bitmap.SaveToFile(FileName);
  finally
    Bitmap.Free;
  end;
end;

procedure TAdvancedCameraManager.SwitchCamera;
begin
  // Vérifier s'il y a plusieurs caméras disponibles
  if Length(FAvailableCameras) <= 1 then
    Exit;

  // Passer à la caméra suivante
  FCurrentCameraIndex := (FCurrentCameraIndex + 1) mod Length(FAvailableCameras);

  // Appliquer la nouvelle caméra
  if FCameraComponent.Active then
    FCameraComponent.Active := False;

  FCameraComponent.CameraDescription := FAvailableCameras[FCurrentCameraIndex];

  FCameraComponent.Active := True;
  UpdateFlashMode;
end;

procedure TAdvancedCameraManager.SetFilter(FilterType: TCameraFilterType);
begin
  FCurrentFilter := FilterType;
end;

procedure TAdvancedCameraManager.SetFlashMode(FlashMode: TCameraFlashMode);
begin
  FFlashMode := FlashMode;
  UpdateFlashMode;
end;

procedure TAdvancedCameraManager.UpdateFlashMode;
begin
  if not FCameraComponent.Active then
    Exit;

  // Vérifier si le flash est disponible
  if not FCameraComponent.HasFlash then
    Exit;

  // Appliquer le mode flash
  case FFlashMode of
    cfmAuto: FCameraComponent.FlashMode := TFlashMode.Auto;
    cfmOn: FCameraComponent.FlashMode := TFlashMode.On;
    cfmOff: FCameraComponent.FlashMode := TFlashMode.Off;
  end;
end;

function TAdvancedCameraManager.ApplyFilter(const Bitmap: TBitmap;
  FilterType: TCameraFilterType): TBitmap;
begin
  case FilterType of
    cftGrayscale: Result := ApplyGrayscaleFilter(Bitmap);
    cftSepia: Result := ApplySepiaFilter(Bitmap);
    cftNegative: Result := ApplyNegativeFilter(Bitmap);
    cftVintage: Result := ApplyVintageFilter(Bitmap);
    else Result := TBitmap.Create(Bitmap.Width, Bitmap.Height);
  end;

  if FilterType = cftNone then
    Result.Assign(Bitmap);
end;

function TAdvancedCameraManager.ApplyGrayscaleFilter(const Bitmap: TBitmap): TBitmap;
var
  DestBitmap: TBitmap;
  SrcData, DestData: TBitmapData;
  X, Y: Integer;
  SrcPixel: TAlphaColorRec;
  Luminance: Byte;
begin
  DestBitmap := TBitmap.Create(Bitmap.Width, Bitmap.Height);

  if Bitmap.Map(TMapAccess.Read, SrcData) and
     DestBitmap.Map(TMapAccess.Write, DestData) then
  begin
    try
      for Y := 0 to Bitmap.Height - 1 do
      begin
        for X := 0 to Bitmap.Width - 1 do
        begin
          SrcPixel := TAlphaColorRec(SrcData.GetPixel(X, Y));

          // Calculer la luminance (formule standard: 0.299*R + 0.587*G + 0.114*B)
          Luminance := Round(0.299 * SrcPixel.R + 0.587 * SrcPixel.G + 0.114 * SrcPixel.B);

          // Appliquer la même valeur à tous les canaux pour obtenir du gris
          DestData.SetPixel(X, Y, TAlphaColorRec.Create(SrcPixel.A, Luminance, Luminance, Luminance).Color);
        end;
      end;
    finally
      Bitmap.Unmap(SrcData);
      DestBitmap.Unmap(DestData);
    end;
  end;

  Result := DestBitmap;
end;

function TAdvancedCameraManager.ApplySepiaFilter(const Bitmap: TBitmap): TBitmap;
var
  DestBitmap: TBitmap;
  SrcData, DestData: TBitmapData;
  X, Y: Integer;
  SrcPixel: TAlphaColorRec;
  R, G, B: Byte;
begin
  DestBitmap := TBitmap.Create(Bitmap.Width, Bitmap.Height);

  if Bitmap.Map(TMapAccess.Read, SrcData) and
     DestBitmap.Map(TMapAccess.Write, DestData) then
  begin
    try
      for Y := 0 to Bitmap.Height - 1 do
      begin
        for X := 0 to Bitmap.Width - 1 do
        begin
          SrcPixel := TAlphaColorRec(SrcData.GetPixel(X, Y));

          // Appliquer l'effet sépia
          R := Min(Round(0.393 * SrcPixel.R + 0.769 * SrcPixel.G + 0.189 * SrcPixel.B), 255);
          G := Min(Round(0.349 * SrcPixel.R + 0.686 * SrcPixel.G + 0.168 * SrcPixel.B), 255);
          B := Min(Round(0.272 * SrcPixel.R + 0.534 * SrcPixel.G + 0.131 * SrcPixel.B), 255);

          DestData.SetPixel(X, Y, TAlphaColorRec.Create(SrcPixel.A, R, G, B).Color);
        end;
      end;
    finally
      Bitmap.Unmap(SrcData);
      DestBitmap.Unmap(DestData);
    end;
  end;

  Result := DestBitmap;
end;

function TAdvancedCameraManager.ApplyNegativeFilter(const Bitmap: TBitmap): TBitmap;
var
  DestBitmap: TBitmap;
  SrcData, DestData: TBitmapData;
  X, Y: Integer;
  SrcPixel: TAlphaColorRec;
begin
  DestBitmap := TBitmap.Create(Bitmap.Width, Bitmap.Height);

  if Bitmap.Map(TMapAccess.Read, SrcData) and
     DestBitmap.Map(TMapAccess.Write, DestData) then
  begin
    try
      for Y := 0 to Bitmap.Height - 1 do
      begin
        for X := 0 to Bitmap.Width - 1 do
        begin
          SrcPixel := TAlphaColorRec(SrcData.GetPixel(X, Y));

          // Inverser les couleurs pour créer un négatif
          DestData.SetPixel(X, Y, TAlphaColorRec.Create(
            SrcPixel.A,
            255 - SrcPixel.R,
            255 - SrcPixel.G,
            255 - SrcPixel.B).Color);
        end;
      end;
    finally
      Bitmap.Unmap(SrcData);
      DestBitmap.Unmap(DestData);
    end;
  end;

  Result := DestBitmap;
end;

function TAdvancedCameraManager.ApplyVintageFilter(const Bitmap: TBitmap): TBitmap;
var
  DestBitmap: TBitmap;
  SrcData, DestData: TBitmapData;
  X, Y: Integer;
  SrcPixel: TAlphaColorRec;
  R, G, B: Byte;
  Luminance: Single;
begin
  DestBitmap := TBitmap.Create(Bitmap.Width, Bitmap.Height);

  if Bitmap.Map(TMapAccess.Read, SrcData) and
     DestBitmap.Map(TMapAccess.Write, DestData) then
  begin
    try
      for Y := 0 to Bitmap.Height - 1 do
      begin
        for X := 0 to Bitmap.Width - 1 do
        begin
          SrcPixel := TAlphaColorRec(SrcData.GetPixel(X, Y));

          // Calculer la luminance
          Luminance := 0.299 * SrcPixel.R + 0.587 * SrcPixel.G + 0.114 * SrcPixel.B;

          // Appliquer un effet vintage (teintes jaunies/brunes avec contraste réduit)
          R := Min(Round(Luminance * 0.9 + 40), 255);
          G := Min(Round(Luminance * 0.7 + 20), 255);
          B := Min(Round(Luminance * 0.5 + 10), 255);

          DestData.SetPixel(X, Y, TAlphaColorRec.Create(SrcPixel.A, R, G, B).Color);
        end;
      end;
    finally
      Bitmap.Unmap(SrcData);
      DestBitmap.Unmap(DestData);
    end;
  end;

  Result := DestBitmap;
end;

end.
```

## 18. Utilisation des gestes tactiles avancés

Implémentons la détection et la gestion de gestes tactiles complexes pour une expérience utilisateur plus riche :

```pascal
// Créons une unité pour gérer les gestes avancés
unit GestureManager;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Gestures;

type
  TGestureEvent = procedure(Sender: TObject; const EventInfo: TGestureEventInfo;
    var Handled: Boolean) of object;

  TGestureManager = class
  private
    FForm: TForm;
    FGestureManager: TGestureManager;
    FOnTap: TGestureEvent;
    FOnDoubleTap: TGestureEvent;
    FOnLongTap: TGestureEvent;
    FOnPan: TGestureEvent;
    FOnPinch: TGestureEvent;
    FOnRotate: TGestureEvent;
    FOnTwoFingerTap: TGestureEvent;
    procedure DoGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
  public
    constructor Create(AForm: TForm);
    destructor Destroy; override;

    procedure EnableGestures(const Gestures: TGestureTypes = []);
    procedure DisableGestures;

    property OnTap: TGestureEvent read FOnTap write FOnTap;
    property OnDoubleTap: TGestureEvent read FOnDoubleTap write FOnDoubleTap;
    property OnLongTap: TGestureEvent read FOnLongTap write FOnLongTap;
    property OnPan: TGestureEvent read FOnPan write FOnPan;
    property OnPinch: TGestureEvent read FOnPinch write FOnPinch;
    property OnRotate: TGestureEvent read FOnRotate write FOnRotate;
    property OnTwoFingerTap: TGestureEvent read FOnTwoFingerTap write FOnTwoFingerTap;
  end;

implementation

constructor TGestureManager.Create(AForm: TForm);
begin
  inherited Create;
  FForm := AForm;

  // Créer le gestionnaire de gestes
  FGestureManager := TGestureManager.Create(AForm);

  // Associer l'événement
  FForm.Touch.GestureManager := FGestureManager;
  FForm.Touch.OnGesture := DoGesture;
end;

destructor TGestureManager.Destroy;
begin
  if Assigned(FForm) then
    FForm.Touch.GestureManager := nil;

  FGestureManager.Free;
  inherited;
end;

procedure TGestureManager.EnableGestures(const Gestures: TGestureTypes);
var
  GestureList: TGestureTypes;
begin
  // Si aucun geste n'est spécifié, activer tous les gestes supportés
  if Gestures = [] then
    GestureList := [TGestureType.Standard, TGestureType.RecognizedGesture,
                   TGestureType.Tap, TGestureType.DoubleTap, TGestureType.LongTap,
                   TGestureType.Pan, TGestureType.PressAndTap, TGestureType.Pinch,
                   TGestureType.Rotate, TGestureType.TwoFingerTap]
  else
    GestureList := Gestures;

  // Activer les gestes
  FForm.Touch.StandardGestures := GestureList;
  FForm.Touch.InteractiveGestures := GestureList;
end;

procedure TGestureManager.DisableGestures;
begin
  FForm.Touch.StandardGestures := [];
  FForm.Touch.InteractiveGestures := [];
end;

procedure TGestureManager.DoGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
  var Handled: Boolean);
begin
  Handled := False;

  // Rediriger l'événement vers le gestionnaire approprié en fonction du type de geste
  case EventInfo.GestureID of
    igiTap:
      if Assigned(FOnTap) then
        FOnTap(Sender, EventInfo, Handled);

    igiDoubleTap:
      if Assigned(FOnDoubleTap) then
        FOnDoubleTap(Sender, EventInfo, Handled);

    igiLongTap:
      if Assigned(FOnLongTap) then
        FOnLongTap(Sender, EventInfo, Handled);

    igiPan:
      if Assigned(FOnPan) then
        FOnPan(Sender, EventInfo, Handled);

    igiPinch:
      if Assigned(FOnPinch) then
        FOnPinch(Sender, EventInfo, Handled);

    igiRotate:
      if Assigned(FOnRotate) then
        FOnRotate(Sender, EventInfo, Handled);

    igiTwoFingerTap:
      if Assigned(FOnTwoFingerTap) then
        FOnTwoFingerTap(Sender, EventInfo, Handled);
  end;
end;

end.
```

## 19. Intégration avec les réseaux sociaux

Implémentons l'authentification et le partage via les réseaux sociaux :

```pascal
// Cette unité simplifie l'intégration avec les réseaux sociaux
unit SocialMediaIntegration;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Platform, FMX.ShareSheet,
  System.Net.HttpClient, System.JSON
  {$IFDEF ANDROID}
  , Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Net, Androidapi.JNI.App,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes, FMX.Helpers.Android
  {$ENDIF}
  {$IFDEF IOS}
  , iOSapi.Foundation, iOSapi.UIKit, Macapi.Helpers, FMX.Helpers.iOS
  {$ENDIF};

type
  TSocialNetwork = (snFacebook, snTwitter, snInstagram);
  TAuthCallback = reference to procedure(const Success: Boolean; const UserID, UserName: string);
  TShareCallback = reference to procedure(const Success: Boolean);

  TSocialMediaIntegration = class
  private
    // Clés d'API pour chaque réseau social - à remplacer par vos propres clés
    FFacebookAppID: string;
    FTwitterApiKey: string;
    FInstagramClientID: string;

    function IsNetworkAvailable: Boolean;
    procedure DoAuthenticate(Network: TSocialNetwork; const Callback: TAuthCallback);
    procedure DoShare(Network: TSocialNetwork; const Text: string; const ImagePath: string;
      const Callback: TShareCallback);
  public
    constructor Create;

    procedure Initialize(const FacebookAppID, TwitterApiKey, InstagramClientID: string);
    procedure Authenticate(Network: TSocialNetwork; const Callback: TAuthCallback);
    procedure ShareText(Network: TSocialNetwork; const Text: string; const Callback: TShareCallback = nil);
    procedure ShareImage(Network: TSocialNetwork; const Text: string; const ImagePath: string;
      const Callback: TShareCallback = nil);
    procedure ShareToAll(const Text: string; const ImagePath: string = '');
    function IsAuthenticated(Network: TSocialNetwork): Boolean;
    procedure Logout(Network: TSocialNetwork);
  end;

implementation

constructor TSocialMediaIntegration.Create;
begin
  inherited Create;
end;

procedure TSocialMediaIntegration.Initialize(const FacebookAppID, TwitterApiKey, InstagramClientID: string);
begin
  FFacebookAppID := FacebookAppID;
  FTwitterApiKey := TwitterApiKey;
  FInstagramClientID := InstagramClientID;

  // Note: Dans une application réelle, vous devriez initialiser les SDK de chaque réseau social
  // Ce code est simplifié pour le tutoriel
end;

function TSocialMediaIntegration.IsNetworkAvailable: Boolean;
begin
  // Vérifier si une connexion réseau est disponible
  {$IFDEF ANDROID}
  var ConnectivityManager := TJConnectivityManager.Wrap(
    TAndroidHelper.Activity.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE));
  var NetworkInfo := ConnectivityManager.getActiveNetworkInfo;

  Result := (NetworkInfo <> nil) and NetworkInfo.isConnected;
  {$ENDIF}

  {$IFDEF IOS}
  // Sur iOS, cette vérification est généralement gérée par les SDK spécifiques
  Result := True;
  {$ENDIF}

  {$IF not Defined(ANDROID) and not Defined(IOS)}
  Result := True;
  {$ENDIF}
end;

procedure TSocialMediaIntegration.Authenticate(Network: TSocialNetwork; const Callback: TAuthCallback);
begin
  if not IsNetworkAvailable then
  begin
    if Assigned(Callback) then
      Callback(False, '', 'Pas de connexion réseau');
    Exit;
  end;

  DoAuthenticate(Network, Callback);
end;

procedure TSocialMediaIntegration.DoAuthenticate(Network: TSocialNetwork; const Callback: TAuthCallback);
begin
  // Dans une application réelle, vous devriez implémenter l'authentification
  // pour chaque réseau social en utilisant leur SDK spécifique

  // Ce code est un exemple simplifié
  case Network of
    snFacebook:
      begin
        // Authentification Facebook
        {$IFDEF ANDROID}
        // Utiliser le SDK Facebook pour Android
        {$ENDIF}

        {$IFDEF IOS}
        // Utiliser le SDK Facebook pour iOS
        {$ENDIF}

        // Simuler une authentification réussie pour le tutoriel
        if Assigned(Callback) then
          Callback(True, '1234567890', 'John Doe');
      end;

    snTwitter:
      begin
        // Authentification Twitter
        {$IFDEF ANDROID}
        // Utiliser le SDK Twitter pour Android
        {$ENDIF}

        {$IFDEF IOS}
        // Utiliser le SDK Twitter pour iOS
        {$ENDIF}

        // Simuler une authentification réussie pour le tutoriel
        if Assigned(Callback) then
          Callback(True, '0987654321', '@johndoe');
      end;

    snInstagram:
      begin
        // Authentification Instagram
        {$IFDEF ANDROID}
        // Utiliser le SDK Instagram pour Android
        {$ENDIF}

        {$IFDEF IOS}
        // Utiliser le SDK Instagram pour iOS
        {$ENDIF}

        // Simuler une authentification réussie pour le tutoriel
        if Assigned(Callback) then
          Callback(True, 'insta12345', 'johndoe');
      end;
  end;
end;

procedure TSocialMediaIntegration.ShareText(Network: TSocialNetwork; const Text: string;
  const Callback: TShareCallback);
begin
  DoShare(Network, Text, '', Callback);
end;

procedure TSocialMediaIntegration.ShareImage(Network: TSocialNetwork; const Text: string;
  const ImagePath: string; const Callback: TShareCallback);
begin
  DoShare(Network, Text, ImagePath, Callback);
end;

procedure TSocialMediaIntegration.DoShare(Network: TSocialNetwork; const Text: string;
  const ImagePath: string; const Callback: TShareCallback);
begin
  // Dans une application réelle, vous devriez implémenter le partage
  // pour chaque réseau social en utilisant leur SDK spécifique

  // Utiliser le service de partage natif pour une méthode simplifiée
  var ShareService: IFMXShareSheetService;
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetService, ShareService) then
  begin
    var SharedItems := TShareItems.Create;
    try
      // Ajouter le texte
      if Text <> '' then
      begin
        var TextItem := TShareItem.Create;
        TextItem.Text := Text;
        SharedItems.Add(TextItem);
      end;

      // Ajouter l'image si spécifiée
      if (ImagePath <> '') and System.IOUtils.TFile.Exists(ImagePath) then
      begin
        var ImageItem := TShareItem.Create;
        ImageItem.FilePath := ImagePath;
        SharedItems.Add(ImageItem);
      end;

      // Afficher la feuille de partage
      ShareService.Share(SharedItems,
        procedure(const AResult: TShareResult)
        begin
          if Assigned(Callback) then
          begin
            case AResult of
              TShareResult.Completed: Callback(True);
              else Callback(False);
            end;
          end;
        end);
    finally
      SharedItems.Free;
    end;
  end
  else if Assigned(Callback) then
    Callback(False);
end;

procedure TSocialMediaIntegration.ShareToAll(const Text: string; const ImagePath: string);
var
  ShareService: IFMXShareSheetService;
  SharedItems: TShareItems;
begin
  // Partager via la feuille de partage native (dialogue de partage du système)
  if TPlatformServices.Current.SupportsPlatformService(IFMXShareSheetService, ShareService) then
  begin
    SharedItems := TShareItems.Create;
    try
      // Ajouter le texte
      if Text <> '' then
      begin
        var TextItem := TShareItem.Create;
        TextItem.Text := Text;
        SharedItems.Add(TextItem);
      end;

      // Ajouter l'image si spécifiée
      if (ImagePath <> '') and System.IOUtils.TFile.Exists(ImagePath) then
      begin
        var ImageItem := TShareItem.Create;
        ImageItem.FilePath := ImagePath;
        SharedItems.Add(ImageItem);
      end;

      // Afficher la feuille de partage
      ShareService.Share(SharedItems, nil);
    finally
      SharedItems.Free;
    end;
  end;
end;

function TSocialMediaIntegration.IsAuthenticated(Network: TSocialNetwork): Boolean;
begin
  // Dans une application réelle, vous devriez vérifier si l'utilisateur est authentifié
  // en utilisant les SDK spécifiques de chaque réseau social

  // Ce code est un exemple simplifié qui simule toujours une authentification
  Result := True;
end;

procedure TSocialMediaIntegration.Logout(Network: TSocialNetwork);
begin
  // Dans une application réelle, vous devriez implémenter la déconnexion
  // pour chaque réseau social en utilisant leur SDK spécifique

  // Ce code est un exemple simplifié sans implémentation réelle
end;

end.
```

## 20. Conclusion et intégration dans notre application

Maintenant que nous avons créé plusieurs fonctionnalités avancées, intégrons-les dans notre application de Carnet de Voyage :

```pascal
// Dans l'unité principale, ajoutons un code d'initialisation pour activer toutes ces fonctionnalités

procedure TMainForm.InitializeAdvancedFeatures;
begin
  // Initialiser la gestion avancée de l'appareil photo
  FCameraManager := TAdvancedCameraManager.Create(CameraComponent);

  // Initialiser la gestion des gestes
  FGestureManager := TGestureManager.Create(Self);
  FGestureManager.EnableGestures();
  FGestureManager.OnPinch := HandlePinchGesture;
  FGestureManager.OnRotate := HandleRotateGesture;

  // Initialiser l'intégration avec les réseaux sociaux
  FSocialMedia := TSocialMediaIntegration.Create;
  FSocialMedia.Initialize(
    'votre_facebook_app_id',
    'votre_twitter_api_key',
    'votre_instagram_client_id');

  // Initialiser la synchronisation cloud
  FCloudSync := TCloudSync.Create('votre_api_key', 'votre_user_id');
  FCloudSync.OnSyncStatus := HandleSyncStatus;

  // Initialiser l'authentification biométrique
  FBioAuth := TBiometricAuth.Create;

  // Initialiser les notifications push
  FPushNotifications := TPushNotificationManager.Create;
  try
    FPushNotifications.Initialize;
  except
    on E: Exception do
      ShowMessage('Erreur d''initialisation des notifications: ' + E.Message);
  end;
end;
```

## Résumé des fonctionnalités

Notre application mobile avancée "Carnet de Voyage" offre maintenant :

1. **Capture multimédia** : photos avec filtres, enregistrements audio
2. **Géolocalisation** : enregistrement de position GPS pour chaque souvenir
3. **Carte interactive** : visualisation des souvenirs sur une carte
4. **Capteurs** : utilisation de l'accéléromètre pour naviguer entre les souvenirs
5. **Notifications locales et push** : rappels pour documenter le voyage
6. **Partage social** : intégration avec les réseaux sociaux
7. **Sécurité** : stockage chiffré et authentification biométrique
8. **Synchronisation cloud** : sauvegarde et restauration des données
9. **Vision par ordinateur** : reconnaissance de texte et de visages dans les photos
10. **Expérience utilisateur tactile** : gestion avancée des gestes
11. **NFC** : association de souvenirs à des objets physiques
12. **Réalité augmentée** : visualisation immersive des souvenirs
13. **Achats in-app** : fonctionnalités premium débloquables

## Conseils pour le développement mobile avancé

1. **Testez sur des appareils physiques** : Les émulateurs ne peuvent pas reproduire fidèlement toutes les fonctionnalités matérielles (caméra, GPS, capteurs). Privilégiez les tests sur des appareils réels.

2. **Adoptez une approche progressive** : N'intégrez pas toutes les fonctionnalités avancées d'un coup. Commencez par l'essentiel puis enrichissez progressivement votre application.

3. **Respectez les règles des plateformes** : Android et iOS ont des directives de conception et de développement spécifiques. Respectez-les pour éviter les rejets lors de la publication.

4. **Gérez la batterie avec soin** : Les fonctionnalités avancées (GPS, caméra, réseaux) consomment beaucoup d'énergie. Activez-les uniquement lorsque nécessaire.

5. **Offrez des alternatives** : Tous les appareils ne disposent pas des mêmes capacités. Prévoyez des chemins alternatifs lorsqu'une fonctionnalité n'est pas disponible.

6. **Considérez la taille des applications** : Les bibliothèques pour les fonctionnalités avancées peuvent alourdir considérablement votre application. Utilisez des techniques comme le téléchargement à la demande quand c'est possible.

7. **Gérez les permissions intelligemment** : Ne demandez pas toutes les permissions au démarrage. Sollicitez-les au moment opportun et expliquez pourquoi vous en avez besoin.

8. **Testez la compatibilité** : Certaines fonctionnalités peuvent ne pas fonctionner de la même manière sur tous les appareils ou versions d'OS. Établissez une matrice de compatibilité.

9. **Optimisez les performances** : Les fonctionnalités avancées peuvent ralentir l'application. Optimisez le code et envisagez d'utiliser des threads secondaires pour les opérations lourdes.

10. **Respectez la vie privée** : Les fonctionnalités avancées collectent souvent des données sensibles. Soyez transparent sur leur utilisation et respectez les réglementations (RGPD, CCPA).

## Défis courants et solutions

### Défi 1 : Fragmentation des appareils Android

**Problème** : Les appareils Android présentent une grande diversité de tailles d'écran, versions d'OS et capacités matérielles.

**Solution** :
- Utilisez des mises en page adaptatives (via les ancrages et les contraintes)
- Définissez des conditions minimales réalistes pour votre application
- Implémentez des détections de fonctionnalités et proposez des alternatives
- Testez sur un panel représentatif d'appareils

### Défi 2 : Permissions utilisateur

**Problème** : Les utilisateurs peuvent refuser les permissions essentielles au fonctionnement de votre application.

**Solution** :
- Expliquez clairement pourquoi vous avez besoin de chaque permission
- Demandez les permissions au moment où elles sont nécessaires, pas toutes au démarrage
- Proposez des fonctionnalités alternatives lorsqu'une permission est refusée
- Ne redemandez pas constamment une permission refusée

```pascal
procedure TMyForm.RequestCameraPermissionWithExplanation;
begin
  // Explique d'abord pourquoi la permission est nécessaire
  TDialogService.ShowMessage(
    'Nous avons besoin d'accéder à votre appareil photo pour capturer vos souvenirs de voyage.',
    procedure(const AResult: TModalResult)
    begin
      // Demande la permission après l'explication
      PermissionsService.RequestPermissions([JStringToString(
        TJManifest_permission.JavaClass.CAMERA)],
        CameraPermissionCallback);
    end);
end;
```

### Défi 3 : Consommation de batterie

**Problème** : Les fonctionnalités avancées peuvent vider rapidement la batterie de l'appareil.

**Solution** :
- Activez les capteurs et services uniquement lorsqu'ils sont nécessaires
- Réduisez la fréquence des mises à jour GPS lorsque possible
- Implémentez des mécanismes de mise en veille pour les processus intensifs
- Optimisez les algorithmes de traitement d'image et de son

```pascal
procedure TLocationTracker.StartTracking(const HighAccuracy: Boolean);
begin
  if HighAccuracy then
  begin
    // Haute précision mais consommation élevée
    LocationSensor.Distance := 5; // mètres
    LocationSensor.UpdateInterval := 5000; // 5 secondes
  end
  else
  begin
    // Précision modérée, économie de batterie
    LocationSensor.Distance := 50; // mètres
    LocationSensor.UpdateInterval := 60000; // 1 minute
  end;

  LocationSensor.Active := True;
end;
```

### Défi 4 : Gestion de la connectivité réseau

**Problème** : La connectivité réseau peut être instable, surtout en déplacement.

**Solution** :
- Concevez votre application pour fonctionner hors ligne autant que possible
- Implémentez une file d'attente pour les opérations réseau
- Ajoutez des mécanismes de reprise sur erreur et de synchronisation différée
- Informez l'utilisateur sur l'état de la connectivité

```pascal
procedure TCloudSync.AddToSyncQueue(const ItemID: string; const Data: TStream);
begin
  FSyncQueue.Add(TSyncQueueItem.Create(ItemID, Data));

  // Si en ligne, démarrer la synchronisation immédiatement
  if IsNetworkAvailable then
    ProcessSyncQueue
  else
    SaveSyncQueueToDisk; // Sauvegarder pour synchronisation ultérieure
end;

procedure TCloudSync.ProcessSyncQueue;
begin
  if (FSyncQueue.Count = 0) or not IsNetworkAvailable then
    Exit;

  // Traiter les éléments de la file d'attente
  // ...
end;

// Réagir aux changements de connectivité
procedure TMainForm.NetworkConnectivityChanged(Sender: TObject);
begin
  if IsNetworkAvailable then
  begin
    StatusLabel.Text := 'En ligne';
    CloudSync.ProcessSyncQueue; // Traiter les éléments en attente
  end
  else
    StatusLabel.Text := 'Hors ligne - Les données seront synchronisées plus tard';
end;
```

## Application en action : exemple concret

Voyons comment ces fonctionnalités peuvent être intégrées dans un scénario utilisateur typique :

### Scénario : Capture d'un souvenir de voyage

1. L'utilisateur ouvre l'application "Carnet de Voyage"
2. L'application détecte sa position actuelle via GPS
3. L'utilisateur appuie sur le bouton "Nouveau souvenir"
4. L'application propose différentes options : Photo, Note, Audio
5. L'utilisateur choisit "Photo" et prend un cliché d'un monument
6. L'application applique automatiquement un filtre "Vintage" à la photo
7. La reconnaissance d'image identifie le monument et suggère son nom
8. L'application récupère les données météo actuelles et les associe au souvenir
9. L'utilisateur peut ajouter une note vocale via l'enregistreur audio
10. Le souvenir est sauvegardé localement avec chiffrement
11. L'application lance la synchronisation cloud en arrière-plan
12. L'utilisateur peut partager ce souvenir sur les réseaux sociaux d'un simple geste

### Code pour ce scénario :

```pascal
procedure TMemoryFormUnit.CaptureMemoryWithContext;
var
  Memory: TTripMemory;
begin
  // 1-2. Créer un nouveau souvenir avec position actuelle
  Memory := FTrip.AddMemory('', '', mtPhoto);

  // Obtenir la position actuelle
  GetCurrentLocation(
    procedure(const Location: TGeoLocation)
    begin
      Memory.Location := Location;

      // 3-4. Ouvrir l'interface de capture photo
      TThread.Synchronize(nil,
        procedure
        begin
          OpenCameraInterface(Memory);
        end);
    end);
end;

procedure TMemoryFormUnit.OpenCameraInterface(Memory: TTripMemory);
begin
  // 5. Prendre une photo
  FCameraManager.SetFilter(cftVintage); // 6. Appliquer un filtre vintage
  FCameraManager.StartCamera;

  // Configurer le bouton de capture
  CaptureButton.OnClick :=
    procedure(Sender: TObject)
    begin
      var PhotoFileName := TPath.Combine(
        TPath.GetDocumentsPath,
        'Photo_' + FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');

      FCameraManager.TakePhoto(PhotoFileName);
      Memory.FilePath := PhotoFileName;

      // 7. Reconnaissance d'image
      RecognizeImageContent(PhotoFileName,
        procedure(const RecognizedName: string)
        begin
          if RecognizedName <> '' then
            Memory.Title := RecognizedName
          else
            Memory.Title := 'Photo de ' + FormatDateTime('dd/mm/yyyy hh:nn', Now);

          // 8. Obtenir la météo actuelle
          GetWeatherForLocation(Memory.Location.Latitude, Memory.Location.Longitude);
        end);

      // 9. Proposer d'ajouter une note audio
      TDialogService.MessageDialog('Souhaitez-vous ajouter un commentaire audio ?',
        TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
        procedure(const AResult: TModalResult)
        begin
          if AResult = mrYes then
            StartAudioRecording(Memory);
          else
            FinalizeMemorySaving(Memory);
        end);
    end;
end;

procedure TMemoryFormUnit.FinalizeMemorySaving(Memory: TTripMemory);
begin
  // 10. Sauvegarder en local avec chiffrement
  var EncryptedData := TSecurityUtils.EncryptString(
    Memory.ToJSON.ToString, FAppSettings.EncryptionKey);
  TFile.WriteAllText(
    TPath.Combine(FTripManager.StoragePath, GUIDToString(Memory.ID) + '.emem'),
    EncryptedData);

  // 11. Synchroniser avec le cloud
  TThread.CreateAnonymousThread(
    procedure
    begin
      FCloudSync.SyncTrip(FTrip);
    end).Start;

  // 12. Proposer le partage
  TDialogService.MessageDialog('Souvenir enregistré. Voulez-vous le partager ?',
    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbNo, 0,
    procedure(const AResult: TModalResult)
    begin
      if AResult = mrYes then
        FSocialMedia.ShareToAll(
          'Mon voyage à ' + FTrip.Destination + ': ' + Memory.Title,
          Memory.FilePath);
    end);
end;
```

## Extensions et améliorations possibles

Notre application est déjà très riche, mais voici quelques pistes d'amélioration pour aller encore plus loin :

### 1. Intelligence artificielle avancée

Intégrez des modèles d'IA pour analyser les photos et suggérer des descriptions, identifier des points d'intérêt, ou même créer automatiquement des récits de voyage basés sur les souvenirs collectés.

```pascal
procedure TMemoryAnalyzer.GenerateTripStory(Trip: TTrip; const Callback: TProc<string>);
var
  Memories: TStringList;
  Story: string;
begin
  // Collecte des informations sur tous les souvenirs
  Memories := TStringList.Create;
  try
    for var Memory in Trip.Memories do
    begin
      Memories.Add(Format('%s: %s à %s (Météo: %s)',
        [FormatDateTime('dd/mm/yyyy', Memory.DateTime),
         Memory.Title,
         Memory.Location.Name,
         Memory.Weather]));
    end;

    // Appel à un service d'IA pour générer une histoire
    var AIRequest := TJSONObject.Create;
    try
      AIRequest.AddPair('destination', Trip.Destination);
      AIRequest.AddPair('startDate', FormatDateTime('yyyy-mm-dd', Trip.StartDate));
      AIRequest.AddPair('endDate', FormatDateTime('yyyy-mm-dd', Trip.EndDate));
      AIRequest.AddPair('memories', TJSONString.Create(Memories.Text));

      CallAIService(AIRequest.ToString,
        procedure(const Response: string)
        begin
          Story := Response;
          if Assigned(Callback) then
            Callback(Story);
        end);
    finally
      AIRequest.Free;
    end;
  finally
    Memories.Free;
  end;
end;
```

### 2. Communauté et social

Créez une plateforme communautaire où les utilisateurs peuvent partager leurs voyages, découvrir des itinéraires populaires et se connecter avec d'autres voyageurs.

```pascal
procedure TSocialFeedForm.LoadPopularDestinations;
begin
  FCloudService.GetPopularDestinations(10,
    procedure(const Destinations: TArray<TDestinationInfo>)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          PopularDestinationsListView.BeginUpdate;
          try
            PopularDestinationsListView.Items.Clear;

            for var Destination in Destinations do
            begin
              var Item := PopularDestinationsListView.Items.Add;
              Item.Text := Destination.Name;
              Item.Detail := Format('%d voyageurs - Note moyenne: %.1f/5',
                [Destination.VisitorCount, Destination.Rating]);

              if Destination.ImageURL <> '' then
                LoadImageFromURL(Destination.ImageURL, Item.Bitmap);
            end;
          finally
            PopularDestinationsListView.EndUpdate;
          end;
        end);
    end);
end;
```

### 3. Réalité augmentée avancée

Développez l'expérience de réalité augmentée pour permettre aux utilisateurs de "voir" les souvenirs d'autres voyageurs superposés sur le monde réel lorsqu'ils visitent les mêmes lieux.

```pascal
procedure TARWorldView.ShowNearbySouvenirs(const CurrentLocation: TGeoLocation);
begin
  // Rechercher les souvenirs à proximité (rayon de 500m)
  FCloudService.GetNearbySouvenirs(CurrentLocation, 500,
    procedure(const SouvenirList: TArray<TSharedSouvenir>)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Effacer les marqueurs AR existants
          ClearARAnchors;

          // Créer un marqueur AR pour chaque souvenir à proximité
          for var Souvenir in SouvenirList do
          begin
            // Calcule la position relative par rapport à l'utilisateur
            var RelativePosition := CalculateRelativeARPosition(
              CurrentLocation, Souvenir.Location);

            // Ajouter le marqueur AR
            AddARAnchor(RelativePosition, Souvenir.Title, Souvenir.ImageURL);
          end;
        end);
    end);
end;
```

### 4. Assistant de voyage personnel

Créez un assistant intelligent qui suggère des activités, des lieux à visiter, et des rappels basés sur la localisation, la météo et les préférences de l'utilisateur.

```pascal
procedure TTravelAssistant.GenerateDailyRecommendations(Trip: TTrip);
var
  CurrentDate: TDateTime;
  CurrentLocation: TGeoLocation;
  Weather: string;
begin
  // Obtenir la date et position actuelles
  CurrentDate := Date;
  LocationSensor.Active := True;
  CurrentLocation.Latitude := LocationSensor.Sensor.Latitude;
  CurrentLocation.Longitude := LocationSensor.Sensor.Longitude;
  LocationSensor.Active := False;

  // Obtenir la météo du jour
  GetWeatherForLocation(CurrentLocation.Latitude, CurrentLocation.Longitude,
    procedure(const WeatherInfo: string)
    begin
      Weather := WeatherInfo;

      // Générer des recommandations selon le contexte
      var IsRainy := Weather.Contains('pluie') or Weather.Contains('rain');
      var IsSunny := Weather.Contains('soleil') or Weather.Contains('sunny');

      var Recommendations := TStringList.Create;
      try
        if IsRainy then
        begin
          Recommendations.Add('Il pleut aujourd''hui. Voici des suggestions:');
          Recommendations.Add('- Visitez le musée local');
          Recommendations.Add('- Découvrez la gastronomie locale dans un restaurant');
          Recommendations.Add('- Profitez d''un spa ou d''une activité intérieure');
        end
        else if IsSunny then
        begin
          Recommendations.Add('Belle journée ensoleillée! Voici des suggestions:');
          Recommendations.Add('- Explorez le parc principal');
          Recommendations.Add('- Faites une promenade le long de la côte');
          Recommendations.Add('- Visitez les monuments historiques');
        end;

        // Ajouter des suggestions basées sur les points d'intérêt proches
        FindNearbyPointsOfInterest(CurrentLocation,
          procedure(const POIList: TArray<TPointOfInterest>)
          begin
            if Length(POIList) > 0 then
            begin
              Recommendations.Add('');
              Recommendations.Add('Points d''intérêt à proximité:');

              for var I := 0 to Min(2, Length(POIList) - 1) do
                Recommendations.Add(Format('- %s (%.1f km)',
                  [POIList[I].Name, POIList[I].Distance]));
            end;

            // Afficher les recommandations
            TThread.Synchronize(nil,
              procedure
              begin
                ShowNotification('Recommandations du jour', Recommendations.Text);
              end);
          end);
      finally
        Recommendations.Free;
      end;
    end);
end;
```

## Conclusion

Le développement d'applications mobiles avec des fonctionnalités avancées offre des possibilités infinies pour créer des expériences riches et immersives. Avec Delphi et FireMonkey, vous disposez de tous les outils nécessaires pour tirer parti des capacités matérielles et logicielles des appareils modernes.

Les exemples présentés dans ce tutoriel ne sont que la pointe de l'iceberg. N'hésitez pas à explorer davantage, à combiner ces fonctionnalités de manière créative, et à imaginer de nouvelles façons d'enrichir l'expérience utilisateur.

Rappelez-vous que la meilleure application mobile n'est pas nécessairement celle qui utilise le plus de fonctionnalités avancées, mais celle qui les utilise judicieusement pour résoudre des problèmes réels et apporter une valeur ajoutée à ses utilisateurs.

Alors lancez-vous, expérimentez, et créez des applications mobiles qui se démarquent !

---

> **Note** : Ce tutoriel utilise Delphi 12 Athens. La plupart des exemples sont compatibles avec Delphi 11 Alexandria. Les fonctionnalités spécifiques à Delphi 12 sont marquées comme telles.
