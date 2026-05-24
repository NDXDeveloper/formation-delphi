🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.4 Utilisation de la caméra et des médias

## Introduction

Les capacités multimédia des smartphones modernes ont transformé ces appareils en véritables studios de création portables. Appareil photo, caméra vidéo, enregistreur audio, lecteur multimédia : tout est intégré dans un seul appareil. Pour vos applications mobiles, exploiter ces capacités peut créer des expériences utilisateur riches et engageantes.

Avec Delphi et FireMonkey, accéder à la caméra et gérer les médias est simple grâce à des composants dédiés qui fonctionnent de manière unifiée sur iOS et Android. Dans cette section, nous allons explorer comment capturer des photos et vidéos, accéder à la bibliothèque multimédia, lire et enregistrer du son, et manipuler ces médias dans vos applications.

## Vue d'ensemble des capacités multimédia

Les appareils mobiles offrent plusieurs fonctionnalités multimédia que vous pouvez intégrer dans vos applications :

**Capture d'images** :
- Prendre des photos avec la caméra
- Accéder aux photos existantes dans la galerie
- Éditer et manipuler les images

**Vidéo** :
- Enregistrer des vidéos
- Lire des vidéos locales ou en streaming
- Capturer des images depuis un flux vidéo

**Audio** :
- Enregistrer du son avec le microphone
- Lire des fichiers audio
- Contrôler la lecture (pause, volume, etc.)

**Partage** :
- Partager des médias vers d'autres applications
- Recevoir des médias depuis d'autres applications

## Capture de photos avec la caméra

La capture de photos est l'une des fonctionnalités multimédia les plus demandées dans les applications mobiles.

### Configuration du composant TCameraComponent

Delphi fournit le composant `TCameraComponent` pour accéder à la caméra de l'appareil.

```pascal
uses
  FMX.Media;

// Configurer la caméra dans le FormCreate
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  // Créer et configurer le composant caméra
  Camera1 := TCameraComponent.Create(Self);
  Camera1.Kind := TCameraKind.FrontCamera; // ou BackCamera
  Camera1.Quality := TVideoCaptureQuality.MediumQuality;
  Camera1.OnSampleBufferReady := CameraSampleBufferReady;
end;
```

### Prendre une photo simple

```pascal
uses
  FMX.MediaLibrary, FMX.Graphics;

// Démarrer l'aperçu de la caméra
procedure TFormMain.BtnDemarrerCameraClick(Sender: TObject);  
begin  
  // Vérifier si la caméra est disponible
  if not Camera1.HasCamera then
  begin
    ShowMessage('Aucune caméra disponible sur cet appareil');
    Exit;
  end;

  // Activer la caméra
  Camera1.Active := True;
end;

// Capturer le frame courant en photo.
// `SampleBufferToBitmap` est SYNCHRONE : il copie le dernier frame
// disponible dans le bitmap. Ce qui est asynchrone, c'est le flux
// vidéo continu de la caméra, signalé par OnSampleBufferReady à
// chaque nouvelle image.
procedure TFormMain.BtnPrendrePhotoClick(Sender: TObject);  
begin  
  if Camera1.Active then
    Camera1.SampleBufferToBitmap(Image1.Bitmap, True);
end;

// Pour un aperçu vidéo « live » dans Image1, on copie chaque frame
// au fur et à mesure. ⚠ OnSampleBufferReady est appelé depuis un
// thread d'arrière-plan, il faut donc marshaler vers l'UI.
procedure TFormMain.CameraSampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Camera1.SampleBufferToBitmap(Image1.Bitmap, True);
    end);
end;
```

### Utiliser l'interface système de prise de photo

Une approche plus simple consiste à utiliser l'interface photo native du système :

```pascal
uses
  FMX.MediaLibrary, FMX.Platform;

// Prendre une photo avec l'interface native
procedure TFormMain.BtnPhotoNativeClick(Sender: TObject);  
var  
  MediaLibrary: IFMXCameraService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, MediaLibrary) then
  begin
    MediaLibrary.TakePhotoFromCamera(Image1,
      procedure(Image: TBitmap)
      begin
        if Assigned(Image) then
        begin
          // La photo a été prise avec succès
          Image1.Bitmap.Assign(Image);
          ShowMessage('Photo capturée !');

          // Sauvegarder si nécessaire
          SauvegarderPhoto(Image);
        end
        else
        begin
          // L'utilisateur a annulé
          ShowMessage('Capture annulée');
        end;
      end);
  end
  else
    ShowMessage('Service caméra non disponible');
end;
```

### Choisir entre caméra avant et arrière

```pascal
// Basculer entre les caméras
procedure TFormMain.BtnBasculerCameraClick(Sender: TObject);  
begin  
  Camera1.Active := False;

  if Camera1.Kind = TCameraKind.BackCamera then
    Camera1.Kind := TCameraKind.FrontCamera
  else
    Camera1.Kind := TCameraKind.BackCamera;

  Camera1.Active := True;
end;
```

### Sauvegarder une photo dans la galerie

```pascal
uses
  System.IOUtils, FMX.Graphics;

// Sauvegarder une photo dans la galerie de l'appareil
procedure TFormMain.SauvegarderPhoto(Photo: TBitmap);  
var  
  MediaLibrary: IFMXPhotoLibrary;
  CheminPhoto: string;
begin
  // Sauvegarder d'abord localement
  CheminPhoto := TPath.Combine(TPath.GetDocumentsPath,
    'photo_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.jpg');
  Photo.SaveToFile(CheminPhoto);

  // Ajouter à la galerie
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, MediaLibrary) then
  begin
    MediaLibrary.AddImageToSavedPhotosAlbum(CheminPhoto,
      procedure(Success: Boolean)
      begin
        if Success then
          ShowMessage('Photo enregistrée dans la galerie')
        else
          ShowMessage('Erreur lors de l''enregistrement');
      end);
  end;
end;
```

## Accès à la bibliothèque photo

Permettre à l'utilisateur de choisir une photo existante dans sa galerie est tout aussi important que de prendre une nouvelle photo.

### Sélectionner une photo depuis la galerie

```pascal
uses
  FMX.MediaLibrary;

// Choisir une photo depuis la galerie
procedure TFormMain.BtnChoisirPhotoClick(Sender: TObject);  
var  
  MediaLibrary: IFMXPhotoLibrary;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhotoLibrary, MediaLibrary) then
  begin
    MediaLibrary.RequestPermission(
      procedure(AGranted: Boolean)
      begin
        if AGranted then
        begin
          // Permission accordée, ouvrir la galerie
          var TakeImageDelegate: TOnDidFinishTaking;
          TakeImageDelegate := procedure(Image: TBitmap)
          begin
            if Assigned(Image) then
            begin
              Image1.Bitmap.Assign(Image);
              ShowMessage('Photo chargée !');
            end;
          end;

          MediaLibrary.TakeImageFromLibrary(Image1, TakeImageDelegate);
        end
        else
          ShowMessage('Permission refusée pour accéder aux photos');
      end);
  end;
end;
```

### Choisir entre caméra et galerie

```pascal
// Donner le choix à l'utilisateur
procedure TFormMain.BtnAjouterPhotoClick(Sender: TObject);  
begin  
  // Afficher un dialogue de choix
  TDialogService.MessageDialog('Choisir une source',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
    TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult)
    begin
      case AResult of
        mrYes: PrendrePhotoAvecCamera;
        mrNo: ChoisirPhotoGalerie;
      end;
    end,
    'Caméra', 'Galerie');
end;
```

## Manipulation d'images

Une fois qu'une image est capturée ou chargée, vous pouvez la manipuler de diverses manières.

### Redimensionner une image

```pascal
uses
  FMX.Graphics;

// Redimensionner une image pour optimiser la mémoire
procedure TFormMain.RedimensionnerImage(Source: TBitmap; NouvelleWidth,
  NouvelleHeight: Integer);
var
  ImageRedim: TBitmap;
  Ratio: Single;
begin
  ImageRedim := TBitmap.Create;
  try
    // Calculer le ratio pour conserver les proportions
    Ratio := Min(NouvelleWidth / Source.Width,
                 NouvelleHeight / Source.Height);

    // Définir les nouvelles dimensions
    ImageRedim.Width := Round(Source.Width * Ratio);
    ImageRedim.Height := Round(Source.Height * Ratio);

    // Redimensionner avec interpolation
    if ImageRedim.Canvas.BeginScene then
    try
      ImageRedim.Canvas.DrawBitmap(Source,
        RectF(0, 0, Source.Width, Source.Height),
        RectF(0, 0, ImageRedim.Width, ImageRedim.Height),
        1.0, True);
    finally
      ImageRedim.Canvas.EndScene;
    end;

    // Copier le résultat
    Source.Assign(ImageRedim);
  finally
    ImageRedim.Free;
  end;
end;

// Utilisation
procedure TFormMain.OptimiserPhoto;  
begin  
  // Redimensionner à maximum 1024x1024 pour économiser la mémoire
  RedimensionnerImage(Image1.Bitmap, 1024, 1024);
end;
```

### Faire pivoter une image

```pascal
// Pivoter une image de 90 degrés
procedure TFormMain.PivoterImage90(Image: TBitmap);  
var  
  ImagePivotee: TBitmap;
begin
  ImagePivotee := TBitmap.Create;
  try
    ImagePivotee.Width := Image.Height;
    ImagePivotee.Height := Image.Width;

    if ImagePivotee.Canvas.BeginScene then
    try
      ImagePivotee.Canvas.SetMatrix(
        TMatrix.CreateRotation(DegToRad(90)) *
        TMatrix.CreateTranslation(ImagePivotee.Width, 0));
      ImagePivotee.Canvas.DrawBitmap(Image,
        RectF(0, 0, Image.Width, Image.Height),
        RectF(0, 0, Image.Width, Image.Height),
        1.0);
    finally
      ImagePivotee.Canvas.EndScene;
    end;

    Image.Assign(ImagePivotee);
  finally
    ImagePivotee.Free;
  end;
end;

// Boutons de rotation
procedure TFormMain.BtnPivoterDroiteClick(Sender: TObject);  
begin  
  PivoterImage90(Image1.Bitmap);
end;
```

### Appliquer des filtres

```pascal
uses
  FMX.Filter.Effects;

// Appliquer un filtre noir et blanc.
// Note : TMonochromeEffect est un TFmxObject ; en passant Image1 comme
// propriétaire (paramètre du constructeur) ET en mettant Parent à
// Image1, le filtre sera automatiquement libéré quand Image1 le sera.
// Pas besoin de Free explicite — n'essayez surtout pas avec un
// try..finally Free : cela retirerait l'effet immédiatement.
procedure TFormMain.AppliquerNoirEtBlanc;  
var  
  Filtre: TMonochromeEffect;
begin
  Filtre := TMonochromeEffect.Create(Image1);
  Filtre.Parent := Image1;
end;

// Appliquer un filtre sépia
procedure TFormMain.AppliquerSepia;  
var  
  Filtre: TSepiaEffect;
begin
  Filtre := TSepiaEffect.Create(Image1);
  Filtre.Parent := Image1;
  Filtre.Amount := 0.8; // Intensité du filtre
end;

// Flouter une image
procedure TFormMain.AppliquerFlou;  
var  
  Filtre: TGaussianBlurEffect;
begin
  Filtre := TGaussianBlurEffect.Create(Image1);
  Filtre.Parent := Image1;
  Filtre.BlurAmount := 3.0; // Intensité du flou
end;
```

### Recadrer une image

```pascal
// Recadrer une zone de l'image
procedure TFormMain.RecadrerImage(Image: TBitmap; X, Y, Width, Height: Integer);  
var  
  ImageRecadree: TBitmap;
begin
  ImageRecadree := TBitmap.Create;
  try
    ImageRecadree.Width := Width;
    ImageRecadree.Height := Height;

    if ImageRecadree.Canvas.BeginScene then
    try
      ImageRecadree.Canvas.DrawBitmap(Image,
        RectF(X, Y, X + Width, Y + Height),
        RectF(0, 0, Width, Height),
        1.0);
    finally
      ImageRecadree.Canvas.EndScene;
    end;

    Image.Assign(ImageRecadree);
  finally
    ImageRecadree.Free;
  end;
end;
```

## Enregistrement et lecture vidéo

### Enregistrer une vidéo

```pascal
uses
  FMX.Media;

// Enregistrer une vidéo avec l'interface native
procedure TFormMain.BtnEnregistrerVideoClick(Sender: TObject);  
var  
  MediaService: IFMXCameraService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, MediaService) then
  begin
    MediaService.TakeVideoFromCamera(Image1,
      procedure(VideoPath: string)
      begin
        if not VideoPath.IsEmpty then
        begin
          // Vidéo enregistrée avec succès
          ShowMessage('Vidéo enregistrée : ' + VideoPath);
          LireVideo(VideoPath);
        end
        else
          ShowMessage('Enregistrement annulé');
      end);
  end;
end;
```

### Lire une vidéo

```pascal
uses
  FMX.Media;

// Configurer et lire une vidéo
procedure TFormMain.LireVideo(CheminVideo: string);  
begin  
  // Créer le lecteur média si nécessaire
  if not Assigned(MediaPlayer1) then
  begin
    MediaPlayer1 := TMediaPlayer.Create(Self);
    MediaPlayer1.Parent := Self;
  end;

  // Configurer le lecteur
  MediaPlayer1.FileName := CheminVideo;
  MediaPlayer1.Align := TAlignLayout.Client;

  // Lancer la lecture
  MediaPlayer1.Play;
end;

// Contrôles de lecture.
// ⚠ TMediaPlayer n'expose pas de méthode Pause. Pour mettre en pause,
//   on appelle Stop : la lecture s'arrête mais CurrentTime conserve sa
//   valeur, ce qui permet de reprendre via Play depuis cet instant.
//   Pour un vrai « stop » (retour au début), on remet CurrentTime à 0.
procedure TFormMain.BtnPlayClick(Sender: TObject);  
begin  
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Play;  // reprend là où on s'est arrêté
end;

procedure TFormMain.BtnPauseClick(Sender: TObject);  
begin  
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Stop;  // sans toucher à CurrentTime = pause
end;

procedure TFormMain.BtnStopClick(Sender: TObject);  
begin  
  if Assigned(MediaPlayer1) then
  begin
    MediaPlayer1.Stop;
    MediaPlayer1.CurrentTime := 0;  // retour au début
  end;
end;
```

### Contrôles avancés de lecture vidéo

> 💡 **Unité de `TMediaPlayer.CurrentTime` et `Duration`.** Ces propriétés sont de type `TMediaTime` (alias d'`Int64`) exprimé en **unités de 100 nanosecondes** (« ticks ») depuis le début du média. La constante `MediaTimeScale = 10000000` représente le nombre de ticks par seconde — il faut donc diviser par `MediaTimeScale` pour obtenir des secondes avant tout `FormatDateTime`.

```pascal
uses
  FMX.Media;  // pour MediaTimeScale

// Barre de progression de la vidéo
procedure TFormMain.TimerVideoTimer(Sender: TObject);  
var  
  SecondesActuelles, SecondesTotales: Double;
begin
  if Assigned(MediaPlayer1) and (MediaPlayer1.Duration > 0) then
  begin
    // Le ratio CurrentTime / Duration est sans unité : OK directement
    TrackBar1.Value := (MediaPlayer1.CurrentTime / MediaPlayer1.Duration) * 100;

    // Conversion ticks → secondes → TDateTime fractionnel pour FormatDateTime
    SecondesActuelles := MediaPlayer1.CurrentTime / MediaTimeScale;
    SecondesTotales   := MediaPlayer1.Duration   / MediaTimeScale;

    LabelTemps.Text :=
      FormatDateTime('nn:ss', SecondesActuelles / SecsPerDay) + ' / ' +
      FormatDateTime('nn:ss', SecondesTotales   / SecsPerDay);
  end;
end;

// Permettre à l'utilisateur de naviguer dans la vidéo
// (CurrentTime et Duration sont dans la même unité, donc le ratio
//  reste correct sans conversion explicite)
procedure TFormMain.TrackBar1Change(Sender: TObject);  
begin  
  if Assigned(MediaPlayer1) and (MediaPlayer1.Duration > 0) then
    MediaPlayer1.CurrentTime :=
      Round((TrackBar1.Value / 100) * MediaPlayer1.Duration);
end;

// Contrôle du volume (Volume va de 0.0 à 1.0)
procedure TFormMain.TrackBarVolumeChange(Sender: TObject);  
begin  
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Volume := TrackBarVolume.Value / 100;
end;
```

## Enregistrement audio

L'enregistrement audio permet de créer des applications comme des dictaphones, des notes vocales, ou des applications de messagerie vocale.

### Configuration de l'enregistreur audio

> 💡 **`TMicrophone` n'existe pas en standard dans FMX.** L'enregistrement audio passe par la classe abstraite `TAudioCaptureDevice` que l'on récupère via `TCaptureDeviceManager`. Le code ci-dessous montre la **bonne** approche.

```pascal
uses
  FMX.Media;

procedure TFormMain.FormCreate(Sender: TObject);  
var  
  Devices: TArray<TCaptureDevice>;
begin
  // On récupère le premier périphérique audio disponible (typiquement
  // le micro intégré ; sur un appareil avec plusieurs micros, on peut
  // itérer pour choisir).
  Devices := TCaptureDeviceManager.Current.GetDevicesByMediaType(TMediaType.Audio);
  if Length(Devices) > 0 then
    FAudioCapture := TAudioCaptureDevice(Devices[0])
  else
    ShowMessage('Aucun microphone disponible');
end;
```

> Pas de `Free` explicite : `TCaptureDeviceManager` possède les périphériques retournés et s'occupe de leur libération.

### Enregistrer un fichier audio

```pascal
// Démarrer l'enregistrement
procedure TFormMain.BtnDemarrerEnregistrementClick(Sender: TObject);  
var  
  CheminFichier: string;
begin
  if not Assigned(FAudioCapture) then
    Exit;

  CheminFichier := TPath.Combine(TPath.GetDocumentsPath,
    'enregistrement_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.wav');

  FAudioCapture.FileName := CheminFichier;
  FAudioCapture.StartCapture;

  BtnDemarrerEnregistrement.Enabled := False;
  BtnArreterEnregistrement.Enabled := True;
  LabelStatus.Text := 'Enregistrement en cours...';
end;

// Arrêter l'enregistrement
procedure TFormMain.BtnArreterEnregistrementClick(Sender: TObject);  
begin  
  if not Assigned(FAudioCapture) then
    Exit;

  FAudioCapture.StopCapture;

  BtnDemarrerEnregistrement.Enabled := True;
  BtnArreterEnregistrement.Enabled := False;
  LabelStatus.Text := 'Enregistrement terminé';

  ShowMessage('Audio enregistré : ' + FAudioCapture.FileName);
end;
```

### Visualisation du niveau audio

> 💡 `TAudioCaptureDevice` n'expose **pas** de propriété `AudioLevel` standard : le niveau RMS doit être calculé à partir des échantillons audio. L'exemple ci-dessous est conceptuel ; pour une vraie barre de niveau, branchez-vous sur `OnSampleBufferReady` et calculez la RMS du buffer.

```pascal
// Pseudo-code : calcul de la RMS sur les échantillons audio
procedure TFormMain.TimerNiveauAudioTimer(Sender: TObject);  
var  
  Niveau: Single;
begin
  if not Assigned(FAudioCapture) or
     (FAudioCapture.State <> TCaptureDeviceState.Capturing) then
    Exit;

  Niveau := CalculerNiveauRMS;  // 0.0 à 1.0, à implémenter

  ProgressBar1.Value := Niveau * 100;
  if Niveau > 0.8 then
    ProgressBar1.Foreground.Color := TAlphaColors.Red
  else if Niveau > 0.5 then
    ProgressBar1.Foreground.Color := TAlphaColors.Orange
  else
    ProgressBar1.Foreground.Color := TAlphaColors.Green;
end;
```

## Lecture audio

### Lire un fichier audio

```pascal
uses
  FMX.Media;

// Lire un fichier audio
procedure TFormMain.LireAudio(CheminFichier: string);  
begin  
  if not Assigned(MediaPlayer1) then
    MediaPlayer1 := TMediaPlayer.Create(Self);

  MediaPlayer1.FileName := CheminFichier;
  MediaPlayer1.Play;
end;

// Liste de lecture simple
var
  ListeLecture: TStringList;
  IndexActuel: Integer = 0;

procedure TFormMain.LirePlaylist;  
begin  
  if (IndexActuel >= 0) and (IndexActuel < ListeLecture.Count) then
  begin
    LireAudio(ListeLecture[IndexActuel]);
    LabelPiste.Text := 'Piste ' + (IndexActuel + 1).ToString +
      ' / ' + ListeLecture.Count.ToString;
  end;
end;

procedure TFormMain.BtnSuivantClick(Sender: TObject);  
begin  
  IndexActuel := (IndexActuel + 1) mod ListeLecture.Count;
  LirePlaylist;
end;

procedure TFormMain.BtnPrecedentClick(Sender: TObject);  
begin  
  IndexActuel := (IndexActuel - 1 + ListeLecture.Count) mod ListeLecture.Count;
  LirePlaylist;
end;
```

## Permissions pour les médias

L'accès à la caméra, au microphone et à la bibliothèque photo nécessite des permissions.

### Demander les permissions nécessaires

> 💡 `PermissionsService` accepte des **chaînes** (les noms Android). Les constantes nommées du type `TPermissions.CAMERA` n'existent pas dans la RTL standard — on déclare donc les chaînes utiles en haut du fichier.

```pascal
uses
  System.Permissions, FMX.DialogService;

const
  PERM_CAMERA              = 'android.permission.CAMERA';
  PERM_RECORD_AUDIO        = 'android.permission.RECORD_AUDIO';
  PERM_READ_EXT_STORAGE    = 'android.permission.READ_EXTERNAL_STORAGE';   // ⚠ obsolète Android 13+
  PERM_WRITE_EXT_STORAGE   = 'android.permission.WRITE_EXTERNAL_STORAGE';  // ⚠ obsolète Android 13+
  PERM_READ_MEDIA_IMAGES   = 'android.permission.READ_MEDIA_IMAGES';       // Android 13+
  PERM_READ_MEDIA_VIDEO    = 'android.permission.READ_MEDIA_VIDEO';        // Android 13+
  PERM_READ_MEDIA_AUDIO    = 'android.permission.READ_MEDIA_AUDIO';        // Android 13+
  PERM_READ_MEDIA_VISUAL_USER_SELECTED =
    'android.permission.READ_MEDIA_VISUAL_USER_SELECTED';                  // Android 14+

procedure TFormMain.DemanderPermissionCamera;  
begin  
  PermissionsService.RequestPermissions(
    [PERM_CAMERA],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
        ActiverCamera
      else
        TDialogService.ShowMessage(
          'L''accès à la caméra est nécessaire pour cette fonctionnalité.');
    end);
end;

procedure TFormMain.DemanderPermissionMicrophone;  
begin  
  PermissionsService.RequestPermissions(
    [PERM_RECORD_AUDIO],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
        DemarrerEnregistrement
      else
        ShowMessage('Permission microphone refusée');
    end);
end;

// Photos : on demande les permissions ciblées sur Android 13+
// et les anciennes sur les versions antérieures. À adapter selon
// votre `targetSdkVersion`.
procedure TFormMain.DemanderPermissionPhotos;  
var  
  Perms: TArray<string>;
begin
  Perms := [PERM_READ_MEDIA_IMAGES, PERM_READ_MEDIA_VIDEO,
            PERM_READ_EXT_STORAGE];  // les deux pour couvrir toutes les versions

  PermissionsService.RequestPermissions(Perms,
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    var
      Accordee: Boolean;
      i: Integer;
    begin
      Accordee := False;
      for i := 0 to High(AGrantResults) do
        if AGrantResults[i] = TPermissionStatus.Granted then
        begin
          Accordee := True;
          Break;
        end;

      if Accordee then
        OuvrirGalerie
      else
        ShowMessage('Permission refusée pour accéder aux photos');
    end);
end;
```

### Vérifier les permissions avant utilisation

```pascal
function TFormMain.APermissionCamera: Boolean;  
begin  
  Result := PermissionsService.IsPermissionGranted(PERM_CAMERA);
end;

procedure TFormMain.BtnPhotoClick(Sender: TObject);  
begin  
  if APermissionCamera then
    PrendrePhoto
  else
    DemanderPermissionCamera;
end;
```

## Partage de médias

Permettre aux utilisateurs de partager des photos ou vidéos vers d'autres applications.

### Partager une image

```pascal
uses
  FMX.MediaLibrary, System.IOUtils;

// Partager une image vers d'autres applications
procedure TFormMain.PartagerImage(Image: TBitmap);  
var  
  CheminTemp: string;
  SharingService: IFMXShareSheetActionsService;
begin
  // Sauvegarder temporairement l'image
  CheminTemp := TPath.Combine(TPath.GetTempPath, 'partage.jpg');
  Image.SaveToFile(CheminTemp);

  // Utiliser le service de partage natif
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXShareSheetActionsService, SharingService) then
  begin
    SharingService.Share(Self,
      'Partagez cette image',
      CheminTemp);
  end;
end;

procedure TFormMain.BtnPartagerClick(Sender: TObject);  
begin  
  PartagerImage(Image1.Bitmap);
end;
```

## Optimisation et bonnes pratiques

### Gestion de la mémoire

> ⚠ **Cycle de vie mobile (rappel).** `FormDeactivate` ne se déclenche **pas** quand l'utilisateur quitte l'application pour répondre à un appel ou consulter une autre app. Pour vraiment libérer la caméra et le média quand l'OS met votre application en arrière-plan, branchez-vous sur `IFMXApplicationEventService` (chapitre 15.1) et réagissez à `EnteredBackground` / `BecameActive`.

```pascal
// À appeler depuis le handler IFMXApplicationEventService quand
// l'événement TApplicationEvent.EnteredBackground se produit.
procedure TFormMain.LibererRessourcesMultimedia;  
begin  
  // Arrêter la caméra (libère le hardware pour les autres apps)
  if Assigned(Camera1) and Camera1.Active then
    Camera1.Active := False;

  // Arrêter la lecture média (évite que l'audio continue en sourdine)
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Stop;

  // Libérer les grandes images (très important : un bitmap 12 MP peut
  // peser 50+ Mo en mémoire vive)
  Image1.Bitmap.SetSize(0, 0);
end;
```

### Compression d'images

```pascal
uses
  FMX.Surfaces, FMX.Graphics;

// Compresser une image en JPEG avant l'envoi.
// Qualite : 1..100 (90 = bonne qualité, 70 = compromis classique).
procedure TFormMain.CompresserImage(Image: TBitmap; Qualite: Integer);  
var  
  Stream: TMemoryStream;
  Surface: TBitmapSurface;
  Params: TBitmapCodecSaveParams;
begin
  Stream := TMemoryStream.Create;
  Surface := TBitmapSurface.Create;
  try
    Surface.Assign(Image);

    // Le paramètre de qualité est passé via TBitmapCodecSaveParams
    Params.Quality := Qualite;
    TBitmapCodecManager.SaveToStream(Stream, Surface, '.jpg', @Params);

    // Recharger l'image compressée pour remplacer l'originale
    Stream.Position := 0;
    Image.LoadFromStream(Stream);
  finally
    Surface.Free;
    Stream.Free;
  end;
end;
```

### Traitement asynchrone

```pascal
// Traiter les images en arrière-plan pour ne pas bloquer l'UI
procedure TFormMain.TraiterImageAsync(CheminImage: string);  
begin  
  TTask.Run(
    procedure
    var
      Image: TBitmap;
    begin
      Image := TBitmap.Create;
      try
        // Charger l'image
        Image.LoadFromFile(CheminImage);

        // Traiter (redimensionner, appliquer filtres, etc.)
        RedimensionnerImage(Image, 800, 800);

        // Mettre à jour l'UI dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            Image1.Bitmap.Assign(Image);
            ShowMessage('Image traitée !');
          end);
      finally
        Image.Free;
      end;
    end);
end;
```

### Gestion des erreurs

```pascal
// Gérer les erreurs d'accès aux médias
procedure TFormMain.PrendrePhotoSecurisee;  
begin  
  try
    if not APermissionCamera then
    begin
      DemanderPermissionCamera;
      Exit;
    end;

    if not Camera1.HasCamera then
    begin
      ShowMessage('Aucune caméra disponible');
      Exit;
    end;

    Camera1.Active := True;
    PrendrePhoto;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de l''accès à la caméra : ' + E.Message);
      Camera1.Active := False;
    end;
  end;
end;
```

## Cas d'usage pratiques

### Application de notes avec photos

```pascal
// Structure pour une note avec photo
type
  TNote = class
    Texte: string;
    CheminPhoto: string;
    DateCreation: TDateTime;
  end;

// Créer une nouvelle note avec photo
procedure TFormMain.CreerNoteAvecPhoto;  
var  
  Note: TNote;
begin
  Note := TNote.Create;
  Note.Texte := MemoNote.Text;
  Note.DateCreation := Now;

  // Sauvegarder la photo
  var CheminPhoto := TPath.Combine(TPath.GetDocumentsPath,
    'note_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.jpg');
  Image1.Bitmap.SaveToFile(CheminPhoto);
  Note.CheminPhoto := CheminPhoto;

  ListeNotes.Add(Note);
  ShowMessage('Note créée avec succès !');
end;
```

### Application de reconnaissance de texte (OCR basique)

```pascal
// Prendre une photo d'un document et extraire le texte
procedure TFormMain.ScannerDocument;  
begin  
  PrendrePhotoAvecCamera(
    procedure(Image: TBitmap)
    begin
      // En production, utiliser une API OCR
      // Ici, exemple simplifié
      ExtraireTexte(Image);
    end);
end;
```

### Application de filtres photo

```pascal
// Appliquer différents filtres photo
procedure TFormMain.CreerAppliFiltre;  
begin  
  // Liste de filtres disponibles
  ComboBoxFiltres.Items.Clear;
  ComboBoxFiltres.Items.Add('Aucun');
  ComboBoxFiltres.Items.Add('Noir et Blanc');
  ComboBoxFiltres.Items.Add('Sépia');
  ComboBoxFiltres.Items.Add('Flou');
  ComboBoxFiltres.Items.Add('Accentuer');
  ComboBoxFiltres.ItemIndex := 0;
end;

procedure TFormMain.ComboBoxFiltresChange(Sender: TObject);  
begin  
  // Supprimer les effets précédents
  SupprimerTousLesEffets(Image1);

  // Appliquer le nouveau filtre
  case ComboBoxFiltres.ItemIndex of
    1: AppliquerNoirEtBlanc;
    2: AppliquerSepia;
    3: AppliquerFlou;
    4: AppliquerAccentuation;
  end;
end;
```

## Conclusion

Les capacités multimédia des appareils mobiles offrent d'innombrables possibilités pour créer des applications riches et engageantes. Avec Delphi et FireMonkey, l'accès à ces fonctionnalités est simplifié grâce à des composants unifiés qui fonctionnent de manière identique sur iOS et Android.

Les points clés à retenir :

1. **Caméra** : Utilisez TCameraComponent ou l'interface native pour capturer des photos
2. **Galerie** : Accédez facilement aux photos existantes de l'utilisateur
3. **Manipulation d'images** : Redimensionnez, pivotez et appliquez des filtres
4. **Vidéo** : Enregistrez et lisez des vidéos avec TMediaPlayer
5. **Audio** : Enregistrez avec `TAudioCaptureDevice` (via `TCaptureDeviceManager`) et lisez avec `TMediaPlayer`
6. **Permissions** : Demandez toujours les permissions nécessaires
7. **Optimisation** : Gérez la mémoire et traitez les médias de façon asynchrone
8. **Partage** : Permettez aux utilisateurs de partager leurs créations

Dans la section suivante, nous verrons comment utiliser les notifications pour maintenir l'engagement de vos utilisateurs avec votre application.

⏭️ [Notifications](/15-applications-mobiles-avec-delphi/05-notifications.md)
