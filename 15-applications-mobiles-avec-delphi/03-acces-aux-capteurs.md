# 15.3 Accès aux capteurs (GPS, accéléromètre...)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'un des grands avantages du développement mobile est la possibilité d'exploiter les nombreux capteurs intégrés dans les appareils modernes. Ces capteurs permettent à vos applications d'interagir avec le monde physique et d'offrir des fonctionnalités enrichies. Avec Delphi et FireMonkey, vous pouvez facilement accéder à ces capteurs sur Android et iOS à partir d'une base de code commune.

## Les capteurs disponibles

Les appareils mobiles modernes intègrent généralement une variété de capteurs, dont les plus courants sont :

| Capteur | Description | Utilisations typiques |
|---------|-------------|----------------------|
| GPS/Localisation | Détermine la position géographique | Navigation, géolocalisation, météo locale |
| Accéléromètre | Mesure l'accélération linéaire | Détection de mouvement, orientation, podomètre |
| Gyroscope | Mesure la rotation | Jeux, réalité augmentée, contrôles basés sur la rotation |
| Magnétomètre (boussole) | Détecte le champ magnétique | Applications de boussole, navigation |
| Capteur de proximité | Détecte la présence d'objets proches | Désactivation de l'écran pendant les appels |
| Capteur de luminosité | Mesure l'intensité lumineuse ambiante | Ajustement automatique de la luminosité |
| Baromètre | Mesure la pression atmosphérique | Altimètre, prévisions météo |

## Accès aux capteurs avec FireMonkey

FireMonkey fournit une API unifiée pour accéder aux différents capteurs, ce qui vous permet d'écrire du code fonctionnant sur les deux plateformes principales (Android et iOS).

### Configuration et permissions

Avant d'utiliser les capteurs, vous devez configurer les permissions appropriées dans votre projet :

#### Pour Android

Dans le Project Manager, accédez à :
- **Project > Options > Uses Permissions**

Cochez les permissions nécessaires comme :
- `ACCESS_FINE_LOCATION` (pour le GPS)
- `ACCESS_COARSE_LOCATION` (pour la localisation approximative)

#### Pour iOS

Éditez le fichier `Info.plist` en ajoutant les descriptions de permissions :
- `NSLocationWhenInUseUsageDescription` (pour l'utilisation du GPS uniquement lorsque l'app est active)
- `NSLocationAlwaysUsageDescription` (pour l'utilisation du GPS en arrière-plan)

Ces descriptions seront affichées à l'utilisateur lors de la demande d'autorisation.

### Exemple d'utilisation du GPS/Localisation

Le service de localisation est l'un des plus utilisés. Voici comment l'implémenter :

```pascal
uses
  System.Sensors, System.Sensors.Components;

type
  TLocationForm = class(TForm)
    LocationSensor1: TLocationSensor;
    btnStartGPS: TButton;
    btnStopGPS: TButton;
    lblLatitude: TLabel;
    lblLongitude: TLabel;
    lblAltitude: TLabel;
    procedure btnStartGPSClick(Sender: TObject);
    procedure btnStopGPSClick(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TLocationForm.btnStartGPSClick(Sender: TObject);
begin
  if LocationSensor1.Available then
  begin
    LocationSensor1.Active := True;
    ShowMessage('GPS activé, en attente de signal...');
  end
  else
    ShowMessage('GPS non disponible sur cet appareil');
end;

procedure TLocationForm.btnStopGPSClick(Sender: TObject);
begin
  LocationSensor1.Active := False;
  ShowMessage('GPS désactivé');
end;

procedure TLocationForm.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  // Mise à jour de l'interface avec les nouvelles coordonnées
  lblLatitude.Text := 'Latitude: ' + NewLocation.Latitude.ToString;
  lblLongitude.Text := 'Longitude: ' + NewLocation.Longitude.ToString;
  lblAltitude.Text := 'Altitude: ' + LocationSensor1.Sensor.Altitude.ToString + ' m';
end;
```

#### Explication pas à pas

1. **Placement du composant** : Déposez un composant `TLocationSensor` depuis la palette sur votre formulaire
2. **Configuration des événements** : Double-cliquez sur l'événement `OnLocationChanged` pour créer le gestionnaire
3. **Activation du capteur** : Réglez la propriété `Active` sur `True` pour démarrer la détection
4. **Traitement des données** : Dans l'événement `OnLocationChanged`, récupérez et utilisez les nouvelles coordonnées

#### Bonnes pratiques pour l'utilisation du GPS

- **Économie d'énergie** : Désactivez le GPS (en mettant `Active := False`) lorsqu'il n'est pas nécessaire
- **Indicateur de chargement** : Affichez un indicateur pendant l'acquisition du signal GPS
- **Gestion des erreurs** : Prévoyez un comportement alternatif si le GPS n'est pas disponible
- **Précision variable** : Gardez à l'esprit que la précision peut varier (de quelques mètres à plusieurs dizaines de mètres)

### Utilisation de l'accéléromètre

L'accéléromètre permet de détecter les mouvements et l'orientation de l'appareil :

```pascal
uses
  System.Sensors, System.Sensors.Components;

type
  TAccelForm = class(TForm)
    MotionSensor1: TMotionSensor;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    Switch1: TSwitch;
    procedure Switch1Switch(Sender: TObject);
    procedure MotionSensor1SensorChoosing(Sender: TObject);
    procedure MotionSensor1AccelerationChanged(Sender: TObject;
      const AX, AY, AZ: Single);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TAccelForm.Switch1Switch(Sender: TObject);
begin
  // Activer/désactiver le capteur selon l'état du Switch
  MotionSensor1.Active := Switch1.IsChecked;
end;

procedure TAccelForm.MotionSensor1SensorChoosing(Sender: TObject);
begin
  // Sélectionner le bon type de capteur
  MotionSensor1.SensorType := TSensorType.Accelerometer;
end;

procedure TAccelForm.MotionSensor1AccelerationChanged(Sender: TObject;
  const AX, AY, AZ: Single);
begin
  // Mettre à jour l'interface avec les valeurs d'accélération
  lblX.Text := 'X: ' + FormatFloat('0.00', AX) + ' m/s²';
  lblY.Text := 'Y: ' + FormatFloat('0.00', AY) + ' m/s²';
  lblZ.Text := 'Z: ' + FormatFloat('0.00', AZ) + ' m/s²';

  // Exemple : détecter une secousse (valeurs simplifiées)
  if (Abs(AX) > 15) or (Abs(AY) > 15) or (Abs(AZ) > 15) then
    ShowMessage('Secousse détectée!');
end;
```

#### Applications de l'accéléromètre

- **Détection d'orientation** : Portrait/paysage automatique
- **Détection de secousse** : Pour actualiser des données ou annuler une action
- **Podomètre** : Comptage de pas (avec des algorithmes supplémentaires)
- **Jeux** : Contrôles basés sur le mouvement

### Utilisation du capteur de proximité

Le capteur de proximité détecte la présence d'objets à proximité du téléphone :

```pascal
uses
  System.Sensors, System.Sensors.Components;

type
  TProximityForm = class(TForm)
    ProximitySensor1: TCustomSensor;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
    procedure ProximitySensor1SensorChanged(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TProximityForm.FormCreate(Sender: TObject);
var
  SensorManager: TSensorManager;
  SensorList: TSensorArray;
  i: Integer;
begin
  // Recherche du capteur de proximité
  SensorManager := TSensorManager.Current;
  SensorList := SensorManager.GetSensorsByCategory(TSensorCategory.Proximity);

  if Length(SensorList) > 0 then
  begin
    ProximitySensor1.Sensor := SensorList[0];
    ProximitySensor1.Active := True;
  end
  else
    ShowMessage('Pas de capteur de proximité disponible');
end;

procedure TProximityForm.ProximitySensor1SensorChanged(Sender: TObject);
var
  ProximityValue: Double;
begin
  // Récupération de la valeur du capteur
  ProximityValue := ProximitySensor1.Data.Value;

  // Si l'objet est proche
  if ProximityValue < 5 then
  begin
    Rectangle1.Fill.Color := TAlphaColors.Red;
  end
  else
  begin
    Rectangle1.Fill.Color := TAlphaColors.Green;
  end;
end;
```

## Combinaison de capteurs pour des fonctionnalités avancées

Les applications les plus innovantes combinent souvent plusieurs capteurs pour créer des expériences enrichies :

### Exemple : Boussole avec compensation d'inclinaison

```pascal
uses
  System.Sensors, System.Sensors.Components;

type
  TCompassForm = class(TForm)
    CompassSensor1: TSensor;
    MotionSensor1: TMotionSensor;
    imgNeedle: TImage;
    procedure FormCreate(Sender: TObject);
    procedure CompassSensor1SensorChanged(Sender: TObject);
    procedure MotionSensor1AccelerationChanged(Sender: TObject;
      const AX, AY, AZ: Single);
  private
    FTiltCompensation: Single;
  public
    { Public declarations }
  end;

implementation

procedure TCompassForm.FormCreate(Sender: TObject);
var
  SensorManager: TSensorManager;
  SensorList: TSensorArray;
begin
  // Configuration du capteur de boussole
  SensorManager := TSensorManager.Current;
  SensorList := SensorManager.GetSensorsByCategory(TSensorCategory.Orientation);

  if Length(SensorList) > 0 then
  begin
    CompassSensor1.Sensor := SensorList[0];
    CompassSensor1.Active := True;
  end;

  // Configuration du capteur de mouvement
  MotionSensor1.SensorType := TSensorType.Accelerometer;
  MotionSensor1.Active := True;

  FTiltCompensation := 0;
end;

procedure TCompassForm.MotionSensor1AccelerationChanged(Sender: TObject;
  const AX, AY, AZ: Single);
begin
  // Calculer la compensation d'inclinaison basée sur l'accéléromètre
  FTiltCompensation := ArcTan2(AY, AZ) * (180 / Pi);
end;

procedure TCompassForm.CompassSensor1SensorChanged(Sender: TObject);
var
  HeadingValue, AdjustedHeading: Single;
begin
  // Obtenir le cap magnétique
  HeadingValue := CompassSensor1.Data.Value;

  // Appliquer la compensation d'inclinaison
  AdjustedHeading := HeadingValue + FTiltCompensation;

  // Normaliser l'angle entre 0 et 360 degrés
  while AdjustedHeading < 0 do
    AdjustedHeading := AdjustedHeading + 360;
  while AdjustedHeading >= 360 do
    AdjustedHeading := AdjustedHeading - 360;

  // Faire pivoter l'image de l'aiguille
  imgNeedle.RotationAngle := AdjustedHeading;
end;
```

## Bonnes pratiques pour l'utilisation des capteurs

### 1. Gestion de l'énergie

Les capteurs, en particulier le GPS, peuvent consommer beaucoup d'énergie :

```pascal
procedure TMyForm.FormActivate(Sender: TObject);
begin
  // Activer les capteurs seulement quand l'application est au premier plan
  if LocationSensor1.Available then
    LocationSensor1.Active := True;
end;

procedure TMyForm.FormDeactivate(Sender: TObject);
begin
  // Désactiver les capteurs quand l'application passe en arrière-plan
  LocationSensor1.Active := False;
end;
```

### 2. Gestion de la disponibilité des capteurs

Tous les appareils ne disposent pas des mêmes capteurs :

```pascal
procedure TMyForm.CheckSensors;
begin
  // Vérifier la disponibilité du capteur et adapter l'interface
  if not LocationSensor1.Available then
  begin
    btnStartGPS.Enabled := False;
    lblStatus.Text := 'GPS non disponible sur cet appareil';
  end;

  if not MotionSensor1.Available then
  begin
    tabAccelerometer.Visible := False;
    lblStatus.Text := lblStatus.Text + #13#10 + 'Accéléromètre non disponible';
  end;
end;
```

### 3. Fréquence d'échantillonnage

Adaptez la fréquence d'échantillonnage à vos besoins réels :

```pascal
// Pour les applications qui ne nécessitent pas de mises à jour fréquentes
LocationSensor1.UpdateInterval := 10000;  // En millisecondes (10 secondes)

// Pour les applications nécessitant des mises à jour rapides
MotionSensor1.UpdateInterval := 50;  // 50ms = 20 mises à jour par seconde
```

### 4. Filtrage des données

Les données brutes des capteurs peuvent contenir du bruit. Utilisez des techniques de filtrage :

```pascal
// Exemple simple de filtre passe-bas pour l'accéléromètre
const
  FILTER_FACTOR = 0.1;
var
  FilteredX, FilteredY, FilteredZ: Single;

procedure TMyForm.MotionSensor1AccelerationChanged(Sender: TObject;
  const AX, AY, AZ: Single);
begin
  // Appliquer un filtre passe-bas
  FilteredX := (AX * FILTER_FACTOR) + (FilteredX * (1.0 - FILTER_FACTOR));
  FilteredY := (AY * FILTER_FACTOR) + (FilteredY * (1.0 - FILTER_FACTOR));
  FilteredZ := (AZ * FILTER_FACTOR) + (FilteredZ * (1.0 - FILTER_FACTOR));

  // Utiliser les valeurs filtrées
  UpdateDisplay(FilteredX, FilteredY, FilteredZ);
end;
```

## Débogage des applications utilisant des capteurs

Le débogage d'applications utilisant des capteurs présente des défis particuliers :

1. **Émulateurs limités** : Les émulateurs ne simulent pas toujours correctement les capteurs
2. **Tests sur appareils réels** : Essentiels pour valider le comportement
3. **Journalisation** : Implémentez une journalisation détaillée des valeurs des capteurs

```pascal
// Exemple de journalisation pour le débogage
procedure TMyForm.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  // Journalisation des coordonnées
  Log('GPS', Format('Lat: %f, Lon: %f, Alt: %f, Précision: %f m',
    [NewLocation.Latitude, NewLocation.Longitude,
     LocationSensor1.Sensor.Altitude,
     LocationSensor1.Sensor.Accuracy]));

  // Mise à jour de l'interface
  UpdateLocationDisplay(NewLocation);
end;

procedure TMyForm.Log(const Category, Message: string);
var
  LogFile: TextFile;
  LogFileName: string;
begin
  LogFileName := TPath.Combine(TPath.GetDocumentsPath, 'SensorLog.txt');
  AssignFile(LogFile, LogFileName);

  if FileExists(LogFileName) then
    Append(LogFile)
  else
    Rewrite(LogFile);

  WriteLn(LogFile, FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) +
                   ' [' + Category + '] ' + Message);

  CloseFile(LogFile);
end;
```

## Conclusion

L'accès aux capteurs ouvre un monde de possibilités pour vos applications mobiles, permettant de créer des expériences contextuelles et interactives. Avec Delphi et FireMonkey, vous disposez d'une API unifiée qui simplifie grandement le développement multi-plateformes.

En suivant les bonnes pratiques présentées dans ce chapitre, vous pourrez intégrer efficacement les capteurs dans vos applications tout en préservant l'autonomie de la batterie et en offrant une expérience utilisateur fluide.

## À retenir

- Vérifiez toujours la disponibilité d'un capteur avant de tenter de l'utiliser
- Configurez correctement les permissions dans votre projet
- Gérez soigneusement l'activation/désactivation des capteurs selon le cycle de vie de l'application
- Adaptez la fréquence d'échantillonnage en fonction de vos besoins réels
- Testez toujours sur des appareils réels pour valider le comportement des capteurs

Dans la prochaine section, nous aborderons l'utilisation de la caméra et des fonctionnalités multimédias dans vos applications mobiles.

⏭️ [Utilisation de la caméra et des médias](15-applications-mobiles-avec-delphi/04-utilisation-de-la-camera-et-des-medias.md)
