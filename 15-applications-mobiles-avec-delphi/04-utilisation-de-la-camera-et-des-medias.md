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

// Capturer une photo
procedure TFormMain.BtnPrendrePhotoClick(Sender: TObject);
begin
  if Camera1.Active then
  begin
    // La capture est asynchrone, l'événement OnSampleBufferReady sera déclenché
    Camera1.SampleBufferToBitmap(Image1.Bitmap, True);
  end;
end;

// Gérer l'image capturée
procedure TFormMain.CameraSampleBufferReady(Sender: TObject;
  const ATime: TMediaTime);
begin
  // Synchroniser avec le thread principal
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

// Appliquer un filtre noir et blanc
procedure TFormMain.AppliquerNoirEtBlanc;
var
  Filtre: TMonochromeEffect;
begin
  Filtre := TMonochromeEffect.Create(Image1);
  try
    Filtre.Parent := Image1;
    // Le filtre est automatiquement appliqué visuellement
  finally
    // Ne pas libérer si on veut garder l'effet
    // Filtre.Free;
  end;
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

// Contrôles de lecture
procedure TFormMain.BtnPlayClick(Sender: TObject);
begin
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Play;
end;

procedure TFormMain.BtnPauseClick(Sender: TObject);
begin
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Stop;
end;

procedure TFormMain.BtnStopClick(Sender: TObject);
begin
  if Assigned(MediaPlayer1) then
  begin
    MediaPlayer1.Stop;
    MediaPlayer1.CurrentTime := 0;
  end;
end;
```

### Contrôles avancés de lecture vidéo

```pascal
// Barre de progression de la vidéo
procedure TFormMain.TimerVideoTimer(Sender: TObject);
begin
  if Assigned(MediaPlayer1) and (MediaPlayer1.Duration > 0) then
  begin
    TrackBar1.Value := (MediaPlayer1.CurrentTime / MediaPlayer1.Duration) * 100;
    LabelTemps.Text := FormatDateTime('nn:ss', MediaPlayer1.CurrentTime / SecsPerDay) +
      ' / ' + FormatDateTime('nn:ss', MediaPlayer1.Duration / SecsPerDay);
  end;
end;

// Permettre à l'utilisateur de naviguer dans la vidéo
procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  if Assigned(MediaPlayer1) and (MediaPlayer1.Duration > 0) then
  begin
    MediaPlayer1.CurrentTime := Round((TrackBar1.Value / 100) * MediaPlayer1.Duration);
  end;
end;

// Contrôle du volume
procedure TFormMain.TrackBarVolumeChange(Sender: TObject);
begin
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Volume := TrackBarVolume.Value / 100;
end;
```

## Enregistrement audio

L'enregistrement audio permet de créer des applications comme des dictaphones, des notes vocales, ou des applications de messagerie vocale.

### Configuration de l'enregistreur audio

```pascal
uses
  FMX.Media;

// Configurer l'enregistreur audio
procedure TFormMain.FormCreate(Sender: TObject);
begin
  MicrophoneAudioCaptureDevice1 := TMicrophone.Create;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  MicrophoneAudioCaptureDevice1.Free;
end;
```

### Enregistrer un fichier audio

```pascal
// Démarrer l'enregistrement
procedure TFormMain.BtnDemarrerEnregistrementClick(Sender: TObject);
var
  CheminFichier: string;
begin
  CheminFichier := TPath.Combine(TPath.GetDocumentsPath,
    'enregistrement_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.wav');

  if Assigned(MicrophoneAudioCaptureDevice1) then
  begin
    MicrophoneAudioCaptureDevice1.FileName := CheminFichier;
    MicrophoneAudioCaptureDevice1.StartCapture;

    BtnDemarrerEnregistrement.Enabled := False;
    BtnArreterEnregistrement.Enabled := True;
    LabelStatus.Text := 'Enregistrement en cours...';
  end;
end;

// Arrêter l'enregistrement
procedure TFormMain.BtnArreterEnregistrementClick(Sender: TObject);
begin
  if Assigned(MicrophoneAudioCaptureDevice1) then
  begin
    MicrophoneAudioCaptureDevice1.StopCapture;

    BtnDemarrerEnregistrement.Enabled := True;
    BtnArreterEnregistrement.Enabled := False;
    LabelStatus.Text := 'Enregistrement terminé';

    ShowMessage('Audio enregistré : ' + MicrophoneAudioCaptureDevice1.FileName);
  end;
end;
```

### Visualisation du niveau audio

```pascal
// Afficher le niveau audio pendant l'enregistrement
procedure TFormMain.TimerNiveauAudioTimer(Sender: TObject);
var
  Niveau: Single;
begin
  if Assigned(MicrophoneAudioCaptureDevice1) and
     MicrophoneAudioCaptureDevice1.State = TCaptureDeviceState.Capturing then
  begin
    // Obtenir le niveau audio (0.0 à 1.0)
    Niveau := MicrophoneAudioCaptureDevice1.AudioLevel;

    // Afficher visuellement
    ProgressBar1.Value := Niveau * 100;

    // Changer la couleur selon le niveau
    if Niveau > 0.8 then
      ProgressBar1.Foreground.Color := TAlphaColors.Red
    else if Niveau > 0.5 then
      ProgressBar1.Foreground.Color := TAlphaColors.Orange
    else
      ProgressBar1.Foreground.Color := TAlphaColors.Green;
  end;
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

```pascal
uses
  System.Permissions, FMX.DialogService;

// Demander la permission pour la caméra
procedure TFormMain.DemanderPermissionCamera;
begin
  PermissionsService.RequestPermissions(
    [TPermissions.CAMERA],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
      begin
        // Permission accordée
        ActiverCamera;
      end
      else
      begin
        TDialogService.ShowMessage(
          'L''accès à la caméra est nécessaire pour cette fonctionnalité.');
      end;
    end
  );
end;

// Demander la permission pour le microphone
procedure TFormMain.DemanderPermissionMicrophone;
begin
  PermissionsService.RequestPermissions(
    [TPermissions.RECORD_AUDIO],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
        DemarrerEnregistrement
      else
        ShowMessage('Permission microphone refusée');
    end
  );
end;

// Demander la permission pour accéder aux photos
procedure TFormMain.DemanderPermissionPhotos;
begin
  PermissionsService.RequestPermissions(
    [TPermissions.READ_EXTERNAL_STORAGE, TPermissions.WRITE_EXTERNAL_STORAGE],
    procedure(const APermissions: TArray<string>;
              const AGrantResults: TArray<TPermissionStatus>)
    begin
      if (Length(AGrantResults) > 0) and
         (AGrantResults[0] = TPermissionStatus.Granted) then
        OuvrirGalerie
      else
        ShowMessage('Permission refusée pour accéder aux photos');
    end
  );
end;
```

### Vérifier les permissions avant utilisation

```pascal
// Vérifier si on a déjà les permissions
function TFormMain.APermissionCamera: Boolean;
begin
  Result := PermissionsService.IsPermissionGranted(TPermissions.CAMERA);
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

```pascal
// Libérer les ressources quand on ne les utilise plus
procedure TFormMain.FormDeactivate(Sender: TObject);
begin
  // Arrêter la caméra
  if Assigned(Camera1) and Camera1.Active then
    Camera1.Active := False;

  // Arrêter la lecture média
  if Assigned(MediaPlayer1) then
    MediaPlayer1.Stop;

  // Libérer les grandes images
  Image1.Bitmap.SetSize(0, 0);
end;
```

### Compression d'images

```pascal
// Compresser une image avant l'envoi
procedure TFormMain.CompresserImage(Image: TBitmap; Qualite: Integer);
var
  Stream: TMemoryStream;
  Surface: TBitmapSurface;
begin
  Stream := TMemoryStream.Create;
  Surface := TBitmapSurface.Create;
  try
    Surface.Assign(Image);

    // Sauvegarder avec compression JPEG
    TBitmapCodecManager.SaveToStream(Stream, Surface, '.jpg');

    // Recharger l'image compressée
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
5. **Audio** : Enregistrez et lisez de l'audio avec TMicrophone et TMediaPlayer
6. **Permissions** : Demandez toujours les permissions nécessaires
7. **Optimisation** : Gérez la mémoire et traitez les médias de façon asynchrone
8. **Partage** : Permettez aux utilisateurs de partager leurs créations

Dans la section suivante, nous verrons comment utiliser les notifications pour maintenir l'engagement de vos utilisateurs avec votre application.

⏭️ [Notifications](/15-applications-mobiles-avec-delphi/05-notifications.md)
