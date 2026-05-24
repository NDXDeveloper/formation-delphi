🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.3 Accès aux capteurs (GPS, accéléromètre...)

## Introduction

Les smartphones et tablettes modernes sont de véritables concentrés de technologie. Ils embarquent une multitude de capteurs qui permettent à vos applications de percevoir le monde physique : la position géographique, les mouvements, l'orientation, la luminosité ambiante, et bien plus encore.

Avec Delphi et FireMonkey, accéder à ces capteurs est étonnamment simple grâce à des composants et des API unifiées qui fonctionnent de manière identique sur iOS et Android. Dans cette section, nous allons explorer comment exploiter ces capteurs pour créer des applications mobiles riches et interactives.

## Vue d'ensemble des capteurs disponibles

Les appareils mobiles disposent généralement des capteurs suivants :

**Capteurs de position** :
- **GPS/GNSS** : Détermine la position géographique précise
- **Réseau cellulaire et WiFi** : Localisation approximative mais rapide

**Capteurs de mouvement** :
- **Accéléromètre** : Mesure l'accélération et l'inclinaison
- **Gyroscope** : Mesure la rotation et l'orientation
- **Magnétomètre (Boussole)** : Détermine l'orientation par rapport au nord magnétique

**Capteurs environnementaux** :
- **Capteur de luminosité** : Mesure la lumière ambiante
- **Capteur de proximité** : Détecte la présence d'un objet proche
- **Baromètre** : Mesure la pression atmosphérique (altitude)
- **Capteur de température** : Sur certains appareils

**Autres capteurs** :
- **Podomètre** : Compte les pas
- **Reconnaissance d'activité** : Détecte si l'utilisateur marche, court, est en voiture, etc.

## Localisation GPS

Le GPS est probablement le capteur le plus utilisé dans les applications mobiles. Il permet de créer des applications de navigation, de suivi d'activités sportives, de recherche de lieux à proximité, et bien plus encore.

### Configuration du composant TLocationSensor

Delphi fournit le composant `TLocationSensor` qui unifie l'accès au GPS sur toutes les plateformes.

> ⚠ **Permission requise avant d'activer le GPS.** Sur Android 6+ et iOS 8+, mettre `Active := True` sans permission accordée ne déclenchera **aucune lecture** — le sensor restera inactif silencieusement. Demandez d'abord la permission `ACCESS_FINE_LOCATION` (Android) ou laissez iOS afficher son dialogue système grâce à la clé `NSLocationWhenInUseUsageDescription` déclarée dans Info.plist. Voir la section *Gestion des permissions* plus bas.

```pascal
uses
  System.Sensors, System.Sensors.Components;

// Créer et configurer le capteur de localisation
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  LocationSensor1 := TLocationSensor.Create(Self);
  LocationSensor1.OnLocationChanged := LocationChange;
  // Active := True seulement APRÈS la demande de permission réussie
  // (voir DemanderPermissionLocalisation plus bas).
end;
```

### Obtenir la position actuelle

```pascal
// Gérer les changements de position
procedure TFormMain.LocationChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  // Afficher les coordonnées
  LabelLatitude.Text := 'Latitude: ' + NewLocation.Latitude.ToString;
  LabelLongitude.Text := 'Longitude: ' + NewLocation.Longitude.ToString;

  // Afficher l'adresse approximative
  AfficherAdresse(NewLocation.Latitude, NewLocation.Longitude);
end;
```

### Précision et économie d'énergie

La précision du GPS a un impact direct sur la consommation de batterie. Vous pouvez ajuster la précision selon vos besoins :

```pascal
// Configurer la précision de la localisation
procedure TFormMain.ConfigurerPrecisionGPS;  
begin  
  // Haute précision (GPS) - consomme plus d'énergie
  LocationSensor1.Accuracy := TLocationAccuracy.High;
  LocationSensor1.Distance := 10; // Mise à jour tous les 10 mètres

  // Ou précision moyenne (WiFi/réseau) - économise la batterie
  // LocationSensor1.Accuracy := TLocationAccuracy.Medium;
  // LocationSensor1.Distance := 100; // Mise à jour tous les 100 mètres
end;
```

### Calculer la distance entre deux points

```pascal
uses
  System.Math;

// Calculer la distance entre deux coordonnées GPS (en kilomètres)
function TFormMain.CalculerDistance(Lat1, Lon1, Lat2, Lon2: Double): Double;  
const  
  RayonTerre = 6371; // Rayon de la Terre en kilomètres
var
  dLat, dLon, a, c: Double;
begin
  // Formule de Haversine
  dLat := DegToRad(Lat2 - Lat1);
  dLon := DegToRad(Lon2 - Lon1);

  a := Sin(dLat / 2) * Sin(dLat / 2) +
       Cos(DegToRad(Lat1)) * Cos(DegToRad(Lat2)) *
       Sin(dLon / 2) * Sin(dLon / 2);

  c := 2 * ArcTan2(Sqrt(a), Sqrt(1 - a));
  Result := RayonTerre * c;
end;

// Utilisation
procedure TFormMain.BtnCalculerDistanceClick(Sender: TObject);  
var  
  Distance: Double;
begin
  // Distance entre Paris et Lyon par exemple
  Distance := CalculerDistance(48.8566, 2.3522, 45.7640, 4.8357);
  ShowMessage('Distance: ' + FormatFloat('0.00', Distance) + ' km');
end;
```

### Géocodage : obtenir une adresse depuis des coordonnées

> ⚠ **Politique d'usage de Nominatim (OpenStreetMap).** L'API publique de Nominatim impose :  
> - un **`User-Agent` identifiant votre application** (avec un email ou une URL de contact),  
> - un **maximum d'une requête par seconde** sur l'instance publique,  
> - pas d'usage commercial à fort volume — pour cela, hébergez votre propre instance ou utilisez un service payant (Mapbox, Google Geocoding, etc.).  
>  
> Sans ces précautions, votre IP sera bloquée. Le code ci-dessous montre comment fixer le `User-Agent`.

```pascal
uses
  System.Net.HttpClient, System.Net.URLClient, System.JSON;

// Convertir des coordonnées GPS en adresse lisible (géocodage inversé)
procedure TFormMain.AfficherAdresse(Latitude, Longitude: Double);  
var  
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JsonValue: TJSONValue;
  URL: string;
begin
  URL := Format(
    'https://nominatim.openstreetmap.org/reverse?format=json&lat=%f&lon=%f',
    [Latitude, Longitude]);

  HttpClient := THTTPClient.Create;
  try
    // Indispensable pour respecter la politique Nominatim
    HttpClient.UserAgent := 'MonApp/1.0 (contact@exemple.fr)';

    Response := HttpClient.Get(URL);
    if Response.StatusCode = 200 then
    begin
      JsonValue := TJSONObject.ParseJSONValue(Response.ContentAsString);
      try
        if Assigned(JsonValue) then
          LabelAdresse.Text := JsonValue.GetValue<string>('display_name');
      finally
        JsonValue.Free;
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;
```

### Cas d'usage pratiques du GPS

**Application de tracking sportif** :
```pascal
// Enregistrer un parcours de course ou vélo
var
  ListeParcours: TList<TLocationCoord2D>;
  DistanceTotale: Double;

procedure TFormMain.DemarrerEnregistrement;  
begin  
  ListeParcours := TList<TLocationCoord2D>.Create;
  DistanceTotale := 0;
  LocationSensor1.Active := True;
  TimerDuree.Enabled := True;
end;

procedure TFormMain.LocationChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  // Ajouter le point au parcours
  ListeParcours.Add(NewLocation);

  // Calculer la distance supplémentaire
  if ListeParcours.Count > 1 then
  begin
    DistanceTotale := DistanceTotale +
      CalculerDistance(OldLocation.Latitude, OldLocation.Longitude,
                      NewLocation.Latitude, NewLocation.Longitude);
    LabelDistance.Text := FormatFloat('0.00', DistanceTotale) + ' km';
  end;
end;
```

**Recherche de lieux à proximité** :
```pascal
// Trouver les restaurants dans un rayon de 1 km
procedure TFormMain.ChercherRestaurantsProches;  
var  
  Position: TLocationCoord2D;
begin
  Position.Latitude := LocationSensor1.Sensor.Latitude;
  Position.Longitude := LocationSensor1.Sensor.Longitude;

  // Rechercher dans la base de données ou via une API
  for var Restaurant in ListeRestaurants do
  begin
    var Distance := CalculerDistance(
      Position.Latitude, Position.Longitude,
      Restaurant.Latitude, Restaurant.Longitude
    );

    if Distance <= 1.0 then // Dans un rayon de 1 km
      AfficherRestaurant(Restaurant);
  end;
end;
```

## Accéléromètre

L'accéléromètre mesure l'accélération de l'appareil selon trois axes (X, Y, Z). Il permet de détecter les mouvements, l'inclinaison, et les secousses.

### Configuration du TMotionSensor

```pascal
uses
  System.Sensors, System.Sensors.Components;

// Créer et configurer le capteur de mouvement
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  MotionSensor1 := TMotionSensor.Create(Self);
  MotionSensor1.Active := True;
  MotionSensor1.OnDataChanged := MotionSensorChange;
end;
```

### Lire les données de l'accéléromètre

```pascal
// Gérer les changements de données du capteur de mouvement
procedure TFormMain.MotionSensorChange(Sender: TObject);  
var  
  AccelX, AccelY, AccelZ: Double;
begin
  // Lire l'accélération sur chaque axe
  AccelX := MotionSensor1.Sensor.AccelerationX;
  AccelY := MotionSensor1.Sensor.AccelerationY;
  AccelZ := MotionSensor1.Sensor.AccelerationZ;

  // Afficher les valeurs
  LabelX.Text := 'X: ' + FormatFloat('0.00', AccelX);
  LabelY.Text := 'Y: ' + FormatFloat('0.00', AccelY);
  LabelZ.Text := 'Z: ' + FormatFloat('0.00', AccelZ);
end;
```

### Détection de l'inclinaison

```pascal
// Détecter l'inclinaison de l'appareil
procedure TFormMain.DetecterInclinaison;  
var  
  AccelX, AccelY: Double;
  Angle: Double;
begin
  AccelX := MotionSensor1.Sensor.AccelerationX;
  AccelY := MotionSensor1.Sensor.AccelerationY;

  // Calculer l'angle d'inclinaison
  Angle := ArcTan2(AccelY, AccelX) * 180 / Pi;

  // Ajuster un élément visuel selon l'inclinaison
  ImageBulle.RotationAngle := Angle;
end;
```

### Détection de secousse (shake)

> ⚠ **Attention à la gravité.** `AccelerationX/Y/Z` (en m/s²) inclut la **gravité terrestre** (~9,81 m/s² sur l'axe vertical). Au repos, la norme `√(X² + Y² + Z²)` vaut donc déjà ≈ 9,81. Pour détecter une vraie secousse, on compare la norme à un seuil **supérieur à la gravité** (typiquement 12 à 15 m/s², soit ~3-5 m/s² au-dessus du repos), ou on soustrait la gravité estimée via un filtre passe-haut.

```pascal
uses
  System.DateUtils;

// Détecter si l'utilisateur secoue l'appareil
var
  DerniereSecousse: TDateTime;  // 0 initialement = jamais détectée

procedure TFormMain.DetecterSecousse;  
const  
  GRAVITE             = 9.81;   // m/s² au repos
  SEUIL_AU_DESSUS     = 5.0;    // tolérance au-dessus de la gravité
  DELAI_ENTRE_SECOUSSES = 1.0;  // secondes
var
  AccelX, AccelY, AccelZ, Norme: Double;
begin
  AccelX := MotionSensor1.Sensor.AccelerationX;
  AccelY := MotionSensor1.Sensor.AccelerationY;
  AccelZ := MotionSensor1.Sensor.AccelerationZ;

  // Norme du vecteur d'accélération
  Norme := Sqrt(AccelX * AccelX + AccelY * AccelY + AccelZ * AccelZ);

  // Une secousse = accélération nettement supérieure à la gravité,
  // et il faut au moins DELAI_ENTRE_SECOUSSES depuis la précédente
  // pour éviter de déclencher plusieurs fois sur un seul geste.
  if (Norme > GRAVITE + SEUIL_AU_DESSUS) and
     ((DerniereSecousse = 0) or
      (SecondsBetween(Now, DerniereSecousse) > DELAI_ENTRE_SECOUSSES)) then
  begin
    DerniereSecousse := Now;
    OnSecousseDetectee;
  end;
end;

procedure TFormMain.OnSecousseDetectee;  
begin  
  ShowMessage('Appareil secoué !');
  // Action typique : réinitialiser un compteur, rafraîchir des données,
  // annuler la dernière action (« shake to undo » sur iOS), etc.
end;
```

### Cas d'usage de l'accéléromètre

**Jeu utilisant l'inclinaison** :
```pascal
// Déplacer un personnage en inclinant l'appareil
procedure TFormMain.DeplacerPersonnage;  
var  
  AccelX, AccelY: Double;
begin
  AccelX := MotionSensor1.Sensor.AccelerationX;
  AccelY := MotionSensor1.Sensor.AccelerationY;

  // Déplacer le personnage selon l'inclinaison
  Personnage.Position.X := Personnage.Position.X + (AccelX * 5);
  Personnage.Position.Y := Personnage.Position.Y + (AccelY * 5);

  // Limiter aux bords de l'écran
  if Personnage.Position.X < 0 then Personnage.Position.X := 0;
  if Personnage.Position.X > ClientWidth - Personnage.Width then
    Personnage.Position.X := ClientWidth - Personnage.Width;
end;
```

**Niveau à bulle (spirit level)** :
```pascal
// Créer un niveau à bulle numérique
procedure TFormMain.AfficherNiveau;  
var  
  AccelX, AccelY: Double;
  AngleX, AngleY: Double;
begin
  AccelX := MotionSensor1.Sensor.AccelerationX;
  AccelY := MotionSensor1.Sensor.AccelerationY;

  // Calculer les angles
  AngleX := ArcTan2(AccelX, AccelY) * 180 / Pi;
  AngleY := ArcTan2(AccelY, AccelX) * 180 / Pi;

  // Positionner la bulle
  Bulle.Position.X := ClientWidth / 2 + (AccelX * 50);
  Bulle.Position.Y := ClientHeight / 2 + (AccelY * 50);

  // Indiquer si l'appareil est de niveau
  if (Abs(AccelX) < 0.1) and (Abs(AccelY) < 0.1) then
  begin
    Bulle.Fill.Color := TAlphaColors.Green;
    LabelStatus.Text := 'Niveau !';
  end
  else
  begin
    Bulle.Fill.Color := TAlphaColors.Red;
    LabelStatus.Text := 'Pas de niveau';
  end;
end;
```

## Gyroscope

Le gyroscope mesure la vitesse de rotation de l'appareil autour de ses trois axes. Il est particulièrement utile pour les applications de réalité augmentée et les jeux.

### Lecture des données du gyroscope

```pascal
// Lire les données de rotation
procedure TFormMain.MotionSensorChange(Sender: TObject);  
var  
  RotationX, RotationY, RotationZ: Double;
begin
  // Vitesse de rotation autour de chaque axe (en radians par seconde)
  RotationX := MotionSensor1.Sensor.AngularVelocityX;
  RotationY := MotionSensor1.Sensor.AngularVelocityY;
  RotationZ := MotionSensor1.Sensor.AngularVelocityZ;

  LabelRotX.Text := 'Rotation X: ' + FormatFloat('0.00', RotationX);
  LabelRotY.Text := 'Rotation Y: ' + FormatFloat('0.00', RotationY);
  LabelRotZ.Text := 'Rotation Z: ' + FormatFloat('0.00', RotationZ);
end;
```

### Application : vue panoramique 360°

```pascal
// Faire pivoter une image panoramique selon l'orientation de l'appareil
var
  AngleTotal: Double = 0;
  DerniereMAJ: TDateTime = 0;  // 0 = première mesure ignorée

procedure TFormMain.MotionSensorChange(Sender: TObject);  
var  
  Maintenant: TDateTime;
  DeltaTemps: Double;
  RotationY: Double;
begin
  Maintenant := Now;

  // Premier appel : on ne peut pas calculer de delta, on s'initialise
  if DerniereMAJ = 0 then
  begin
    DerniereMAJ := Maintenant;
    Exit;
  end;

  // Temps écoulé en secondes depuis la dernière lecture
  DeltaTemps := MilliSecondsBetween(Maintenant, DerniereMAJ) / 1000;
  DerniereMAJ := Maintenant;

  // Vitesse angulaire verticale (en radians par seconde)
  RotationY := MotionSensor1.Sensor.AngularVelocityY;

  // Intégrer pour obtenir l'angle cumulé (en degrés)
  AngleTotal := AngleTotal + (RotationY * DeltaTemps * 180 / Pi);

  // Faire défiler l'image panoramique horizontalement
  ImagePanorama.Position.X := -AngleTotal * 10;
end;
```

## Magnétomètre (Boussole)

Le magnétomètre détecte le champ magnétique terrestre et permet de déterminer l'orientation de l'appareil par rapport au nord magnétique.

### Obtenir le cap (direction)

> ⚠ **Attention au piège des propriétés `HeadingX` / `HeadingY`.** Sur `TCustomOrientationSensor`, **`HeadingX` et `HeadingY` ne sont pas le cap en degrés** : ce sont les composantes X et Y (en microTeslas) du vecteur pointant vers le nord magnétique. Le **cap magnétique** en degrés (0 = Nord, 90 = Est, 180 = Sud, 270 = Ouest) s'obtient via la propriété `MagHeading` quand elle est exposée par la plateforme, ou en le recalculant à partir des composantes.

```pascal
uses
  System.Sensors, System.Sensors.Components, System.Math;

// Lire le cap de la boussole
procedure TFormMain.OrientationSensorChange(Sender: TObject);  
var  
  Cap: Double;
begin
  if not Assigned(OrientationSensor1.Sensor) then
    Exit;

  // Approche 1 : propriété MagHeading (cap magnétique en degrés)
  Cap := OrientationSensor1.Sensor.MagHeading;

  // Approche 2 (fallback) : calcul à partir des composantes du vecteur
  // if IsNan(Cap) then
  //   Cap := RadToDeg(ArcTan2(OrientationSensor1.Sensor.HeadingY,
  //                           OrientationSensor1.Sensor.HeadingX));

  // Normaliser dans [0 ; 360[
  while Cap < 0   do Cap := Cap + 360;
  while Cap > 360 do Cap := Cap - 360;

  LabelCap.Text := 'Cap: ' + FormatFloat('0', Cap) + '°';

  // Orienter une image de boussole (l'aiguille pointe vers le nord)
  ImageAiguille.RotationAngle := -Cap;

  AfficherDirectionCardinale(Cap);
end;

procedure TFormMain.AfficherDirectionCardinale(Cap: Double);  
var  
  Direction: string;
begin
  // Déterminer la direction cardinale
  if (Cap >= 337.5) or (Cap < 22.5) then
    Direction := 'Nord'
  else if (Cap >= 22.5) and (Cap < 67.5) then
    Direction := 'Nord-Est'
  else if (Cap >= 67.5) and (Cap < 112.5) then
    Direction := 'Est'
  else if (Cap >= 112.5) and (Cap < 157.5) then
    Direction := 'Sud-Est'
  else if (Cap >= 157.5) and (Cap < 202.5) then
    Direction := 'Sud'
  else if (Cap >= 202.5) and (Cap < 247.5) then
    Direction := 'Sud-Ouest'
  else if (Cap >= 247.5) and (Cap < 292.5) then
    Direction := 'Ouest'
  else
    Direction := 'Nord-Ouest';

  LabelDirection.Text := Direction;
end;
```

### Application : réalité augmentée

```pascal
// Afficher des points d'intérêt superposés sur la caméra selon l'orientation
procedure TFormMain.AfficherPOIEnRA;  
var  
  Cap: Double;
  POI: TPointInteret;
  AngleVersPOI: Double;
  DistanceAngulaire: Double;
begin
  // Cap magnétique en degrés (voir la mise en garde plus haut)
  Cap := OrientationSensor1.Sensor.MagHeading;

  for POI in ListePointsInteret do
  begin
    // Calculer l'angle vers le POI depuis notre position
    AngleVersPOI := CalculerAngleVers(POI.Latitude, POI.Longitude);

    // Calculer la différence angulaire
    DistanceAngulaire := AngleVersPOI - Cap;

    // Normaliser entre -180 et 180
    if DistanceAngulaire > 180 then DistanceAngulaire := DistanceAngulaire - 360;
    if DistanceAngulaire < -180 then DistanceAngulaire := DistanceAngulaire + 360;

    // Afficher le POI s'il est dans le champ de vision (±45°)
    if Abs(DistanceAngulaire) < 45 then
    begin
      POI.Label.Visible := True;
      // Positionner le label selon l'angle
      POI.Label.Position.X := ClientWidth / 2 + (DistanceAngulaire * 10);
    end
    else
      POI.Label.Visible := False;
  end;
end;
```

## Podomètre et compteur de pas

Sur les appareils récents, vous pouvez accéder au compteur de pas intégré.

> ⚠ **`TPedometerSensor` n'existe pas en standard dans la RTL Delphi.** Le pseudo-code ci-dessous illustre le **principe** d'utilisation. En pratique, sur Android, il faut interroger les capteurs natifs `TYPE_STEP_COUNTER` ou `TYPE_STEP_DETECTOR` via JNI ; sur iOS, on utilise `CMPedometer` du framework *CoreMotion* via les bindings Objective-C. Des bibliothèques tierces (Kastri Free, par exemple) encapsulent ce travail.

### Utilisation du podomètre (exemple conceptuel)

```pascal
// Pseudo-code : adapter selon le wrapper natif que vous intégrez
procedure TFormMain.InitialiserPodometre;  
var  
  StepCounter: TPedometerSensor;  // ⚠ classe fictive — wrapper tiers
begin
  StepCounter := TPedometerSensor.Create(Self);
  StepCounter.OnStepCountChanged := PasComptabilises;
  StepCounter.Active := True;
end;

procedure TFormMain.PasComptabilises(Sender: TObject; NumberOfSteps: Integer);  
begin  
  LabelPas.Text := NumberOfSteps.ToString + ' pas';

  // Calculer la distance approximative (moyenne de 0.75m par pas)
  var DistanceKm := (NumberOfSteps * 0.75) / 1000;
  LabelDistance.Text := FormatFloat('0.00', DistanceKm) + ' km';

  // Calculer les calories brûlées (approximation)
  var Calories := NumberOfSteps * 0.04; // ~0.04 cal par pas
  LabelCalories.Text := FormatFloat('0', Calories) + ' cal';
end;
```

## Capteur de luminosité

Le capteur de luminosité ambiante peut être utilisé pour adapter l'interface à l'environnement.

### Adapter l'interface selon la luminosité

```pascal
uses
  System.Sensors, System.Sensors.Components;

// Réagir aux changements de luminosité
procedure TFormMain.InitialiserCapteurLuminosite;  
begin  
  // LightSensor1 est un TLightSensor posé sur la fiche.
  // L'événement standard est OnSensorChanged (et non OnIlluminanceChanged).
  LightSensor1.OnSensorChanged := LightSensorChange;
  LightSensor1.Active := True;
end;

procedure TFormMain.LightSensorChange(Sender: TObject);  
var  
  Illuminance: Double;
begin
  if not Assigned(LightSensor1.Sensor) then
    Exit;

  Illuminance := LightSensor1.Sensor.Illuminance;  // en lux
  LabelLux.Text := FormatFloat('0', Illuminance) + ' lux';

  // Adapter l'interface selon la luminosité
  if Illuminance < 10 then
  begin
    // Environnement très sombre - mode nuit
    ActiverModeNuit;
  end
  else if Illuminance < 100 then
  begin
    // Environnement sombre - réduire la luminosité
    Rectangle1.Opacity := 0.8;
  end
  else if Illuminance > 1000 then
  begin
    // Plein soleil - augmenter le contraste
    AugmenterContraste;
  end;
end;

procedure TFormMain.ActiverModeNuit;  
begin  
  // Basculer vers un thème sombre
  StyleBook1.Style := 'Dark';
  Rectangle1.Fill.Color := TAlphaColors.Black;
  Label1.TextSettings.FontColor := TAlphaColors.White;
end;
```

## Gestion des permissions

L'accès aux capteurs nécessite souvent des permissions spécifiques, particulièrement pour le GPS.

### Demander la permission de localisation

> 💡 `PermissionsService` (unité `System.Permissions`) prend des **chaînes** correspondant aux noms Android (`android.permission.…`). Il n'y a **pas** d'unité `FMX.Permissions` exposant des constantes nommées dans la RTL standard — la convention est d'utiliser les chaînes directement, éventuellement regroupées dans une constante locale pour la lisibilité.

```pascal
uses
  FMX.DialogService, System.Permissions;

const
  PERM_FINE_LOCATION   = 'android.permission.ACCESS_FINE_LOCATION';
  PERM_COARSE_LOCATION = 'android.permission.ACCESS_COARSE_LOCATION';

procedure TFormMain.DemanderPermissionLocalisation;  
begin  
  PermissionsService.RequestPermissions(
    [PERM_FINE_LOCATION, PERM_COARSE_LOCATION],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        LocationSensor1.Active := True;
        ShowMessage('GPS activé');
      end
      else
        TDialogService.ShowMessage(
          'L''accès à la localisation est nécessaire pour cette fonctionnalité.');
    end);
end;
```

### Vérifier si une permission est accordée

```pascal
function TFormMain.PermissionLocalisationAccordee: Boolean;  
begin  
  Result := PermissionsService.IsPermissionGranted(PERM_FINE_LOCATION);
end;

procedure TFormMain.FormShow(Sender: TObject);  
begin  
  if not PermissionLocalisationAccordee then
    DemanderPermissionLocalisation
  else
    LocationSensor1.Active := True;
end;
```

## Bonnes pratiques

### Économiser la batterie

Les capteurs consomment de l'énergie. Voici quelques conseils pour optimiser la consommation :

> ⚠ **Rappel cycle de vie mobile.** `OnDeactivate` / `OnActivate` d'un `TForm` réagissent au changement de focus **entre formulaires de l'app**, pas aux passages arrière-plan / premier plan déclenchés par l'OS. Pour vraiment désactiver les capteurs quand l'application passe en arrière-plan, il faut s'abonner à `IFMXApplicationEventService` (voir le chapitre 15.1) et réagir à `EnteredBackground` / `BecameActive`.

```pascal
// Pseudo-code à brancher dans le handler IFMXApplicationEventService
procedure TFormMain.HandlerEvenementApp(AEvent: TApplicationEvent);  
begin  
  case AEvent of
    TApplicationEvent.EnteredBackground:
      begin
        // L'app est en arrière-plan : couper tout ce qui draine la batterie
        LocationSensor1.Active := False;
        MotionSensor1.Active   := False;
      end;
    TApplicationEvent.BecameActive:
      begin
        // L'app revient au premier plan : on relance ce qui est utile
        LocationSensor1.Active := True;
        MotionSensor1.Active   := True;
      end;
  end;
end;

// Ajuster la fréquence de mise à jour
procedure TFormMain.OptimiserPrecision;  
begin  
  // Pour le tracking : haute précision nécessaire
  LocationSensor1.Accuracy := TLocationAccuracy.High;
  LocationSensor1.Distance := 10; // Tous les 10m

  // Pour une simple localisation : précision moyenne suffit
  // LocationSensor1.Accuracy := TLocationAccuracy.Medium;
  // LocationSensor1.Distance := 100; // Tous les 100m
end;
```

### Gérer les erreurs et cas limites

```pascal
// Vérifier si le capteur est disponible
procedure TFormMain.VerifierDisponibiliteCapteur;  
begin  
  if not Assigned(LocationSensor1.Sensor) then
  begin
    ShowMessage('Le GPS n''est pas disponible sur cet appareil');
    Exit;
  end;

  if not LocationSensor1.Sensor.Available then
  begin
    ShowMessage('Le GPS est désactivé. Veuillez l''activer dans les paramètres.');
    Exit;
  end;
end;

// Gérer un timeout de localisation — version asynchrone avec un timer.
// ⚠ NE PAS utiliser de boucle « while + Sleep + ProcessMessages » : sur
//   mobile cela gèle l'interface, consomme de la batterie et empêche
//   le système de notifier la position quand elle arrive enfin.
procedure TFormMain.DemarrerGPSAvecTimeout;  
begin  
  LocationObtenue := False;
  LocationSensor1.Active := True;

  // Timer one-shot qui annule la demande au bout de 30 s
  TimerTimeoutGPS.Interval := 30000;
  TimerTimeoutGPS.OnTimer := TimerTimeoutGPSTimer;
  TimerTimeoutGPS.Enabled := True;
end;

procedure TFormMain.TimerTimeoutGPSTimer(Sender: TObject);  
begin  
  TimerTimeoutGPS.Enabled := False;
  if not LocationObtenue then
  begin
    LocationSensor1.Active := False;
    ShowMessage('Impossible d''obtenir la localisation. ' +
                'Vérifiez que le GPS est activé.');
  end;
end;

procedure TFormMain.LocationChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  LocationObtenue := True;
  TimerTimeoutGPS.Enabled := False;  // on a notre position, on annule le timeout
  // … traiter la position …
end;
```

### Informer l'utilisateur

```pascal
// Afficher un indicateur pendant l'acquisition GPS
procedure TFormMain.DemarrerLocalisationAvecIndicateur;  
begin  
  AniIndicator1.Enabled := True;
  AniIndicator1.Visible := True;
  LabelStatus.Text := 'Localisation en cours...';

  LocationSensor1.Active := True;
end;

procedure TFormMain.LocationChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  // Masquer l'indicateur une fois la position obtenue
  AniIndicator1.Enabled := False;
  AniIndicator1.Visible := False;
  LabelStatus.Text := 'Position obtenue';
end;
```

## Combinaison de plusieurs capteurs

Les applications les plus intéressantes combinent souvent plusieurs capteurs pour créer des expériences riches.

### Exemple : application de randonnée

```pascal
// Combiner GPS, accéléromètre et altimètre
type
  TDonneesRandonnee = record
    Position: TLocationCoord2D;
    Altitude: Double;
    Vitesse: Double;
    NombrePas: Integer;
    DistanceParcourue: Double;
  end;

var
  DonneesActuelles: TDonneesRandonnee;

procedure TFormMain.MettreAJourDonnees;  
begin  
  // Position GPS
  DonneesActuelles.Position.Latitude := LocationSensor1.Sensor.Latitude;
  DonneesActuelles.Position.Longitude := LocationSensor1.Sensor.Longitude;
  DonneesActuelles.Altitude := LocationSensor1.Sensor.Altitude;

  // Vitesse de déplacement
  DonneesActuelles.Vitesse := LocationSensor1.Sensor.Speed * 3.6; // m/s en km/h

  // Affichage
  LabelAltitude.Text := FormatFloat('0', DonneesActuelles.Altitude) + ' m';
  LabelVitesse.Text := FormatFloat('0.0', DonneesActuelles.Vitesse) + ' km/h';
  LabelDistance.Text := FormatFloat('0.00', DonneesActuelles.DistanceParcourue) + ' km';
end;
```

## Conclusion

Les capteurs des appareils mobiles ouvrent un monde de possibilités pour créer des applications innovantes et interactives. Avec Delphi, accéder à ces capteurs est simple et unifié sur toutes les plateformes.

Les points clés à retenir :

1. **GPS** : Essentiel pour les applications de navigation et de localisation
2. **Accéléromètre** : Détecte les mouvements et l'inclinaison
3. **Gyroscope** : Mesure les rotations pour la réalité augmentée et les jeux
4. **Magnétomètre** : Fournit l'orientation pour les boussoles et la navigation
5. **Permissions** : Toujours demander et gérer correctement les permissions
6. **Batterie** : Désactiver les capteurs quand ils ne sont pas nécessaires
7. **Combinaison** : Les meilleures applications combinent plusieurs capteurs

Dans la section suivante, nous verrons comment utiliser la caméra et gérer les médias pour enrichir encore davantage vos applications mobiles.

⏭️ [Utilisation de la caméra et des médias](/15-applications-mobiles-avec-delphi/04-utilisation-de-la-camera-et-des-medias.md)
