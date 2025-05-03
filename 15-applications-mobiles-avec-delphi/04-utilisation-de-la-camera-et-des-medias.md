# 15.4 Utilisation de la caméra et des médias

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Les appareils mobiles modernes sont équipés de caméras de plus en plus performantes et offrent de nombreuses fonctionnalités multimédias. Intégrer ces capacités à vos applications Delphi peut considérablement enrichir l'expérience utilisateur. Dans cette section, nous allons explorer comment utiliser la caméra et manipuler différents types de médias (images, audio, vidéo) dans vos applications mobiles.

## 1. Utilisation de la caméra

Delphi offre plusieurs façons d'intégrer la caméra dans vos applications mobiles. Nous allons examiner les approches les plus courantes.

### Configuration des permissions

Avant de pouvoir utiliser la caméra, vous devez configurer les permissions appropriées :

#### Pour Android

Dans le Project Manager, accédez à :
- **Project > Options > Uses Permissions**

Cochez les permissions nécessaires :
- `CAMERA` (pour l'accès à la caméra)
- `WRITE_EXTERNAL_STORAGE` (pour enregistrer les photos prises)

#### Pour iOS

Éditez le fichier `Info.plist` en ajoutant les descriptions de permissions :
- `NSCameraUsageDescription` (justification pour l'utilisation de la caméra)
- `NSPhotoLibraryUsageDescription` (justification pour l'accès à la photothèque)

### Méthode 1 : Utilisation du composant `TCameraComponent`

Le composant `TCameraComponent` est la manière la plus simple d'intégrer la caméra :

```pascal
uses
  FMX.Media;

type
  TCameraForm = class(TForm)
    CameraComponent1: TCameraComponent;
    ImageControl1: TImageControl;
    btnStartCamera: TButton;
    btnTakePhoto: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnStartCameraClick(Sender: TObject);
    procedure btnTakePhotoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPhotoTaken: Boolean;
  public
    { Public declarations }
  end;

implementation

procedure TCameraForm.FormCreate(Sender: TObject);
begin
  FPhotoTaken := False;

  // Vérification de la disponibilité de la caméra
  if CameraComponent1.Available then
    btnStartCamera.Enabled := True
  else
  begin
    btnStartCamera.Enabled := False;
    ShowMessage('Aucune caméra disponible sur cet appareil');
  end;
end;

procedure TCameraForm.btnStartCameraClick(Sender: TObject);
begin
  // Démarrage de la caméra
  if not CameraComponent1.Active then
  begin
    CameraComponent1.Active := True;
    btnStartCamera.Text := 'Arrêter la caméra';
    btnTakePhoto.Enabled := True;
  end
  else
  begin
    CameraComponent1.Active := False;
    btnStartCamera.Text := 'Démarrer la caméra';
    btnTakePhoto.Enabled := False;
  end;
end;

procedure TCameraForm.btnTakePhotoClick(Sender: TObject);
begin
  // Prise de photo
  if CameraComponent1.Active then
  begin
    CameraComponent1.SampleBufferToBitmap(ImageControl1.Bitmap, True);
    FPhotoTaken := True;

    // Optionnel : Arrêter la caméra après la prise de photo
    // CameraComponent1.Active := False;
    // btnStartCamera.Text := 'Démarrer la caméra';
  end;
end;

procedure TCameraForm.FormDestroy(Sender: TObject);
begin
  // S'assurer que la caméra est désactivée
  if CameraComponent1.Active then
    CameraComponent1.Active := False;
end;
```

Pour ajouter ce code à votre application :

1. Créez un nouveau formulaire
2. Depuis la palette de composants, faites glisser :
   - un `TCameraComponent` (onglet System)
   - un `TImageControl` (onglet Controls)
   - deux `TButton` (onglet Standard)
3. Disposez-les selon vos préférences et configurez leurs propriétés
4. Implémentez les gestionnaires d'événements comme indiqué ci-dessus

### Méthode 2 : Utilisation de `TakePhotoFromCamera`

Pour les cas où vous avez juste besoin de prendre une photo rapidement sans afficher de prévisualisation :

```pascal
uses
  FMX.Media;

type
  TQuickPhotoForm = class(TForm)
    ImageControl1: TImageControl;
    btnQuickPhoto: TButton;
    procedure btnQuickPhotoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TQuickPhotoForm.btnQuickPhotoClick(Sender: TObject);
begin
  TakePhotoFromCamera(ImageControl1.Bitmap,
    procedure(const AResult: TTakePhotoResult)
    begin
      case AResult of
        TTakePhotoResult.Success:
          ShowMessage('Photo prise avec succès');
        TTakePhotoResult.Canceled:
          ShowMessage('Prise de photo annulée');
        TTakePhotoResult.Error:
          ShowMessage('Erreur lors de la prise de photo');
      end;
    end);
end;
```

Cette méthode est très simple car elle utilise l'interface de caméra native du système d'exploitation.

### Enregistrement des photos prises

Pour enregistrer une photo après l'avoir prise :

```pascal
procedure TSavePhotoForm.SavePhoto(const ABitmap: TBitmap);
var
  SavePath: string;
  ImageFileName: string;
begin
  {$IFDEF ANDROID}
  SavePath := TPath.GetSharedDownloadsPath;
  {$ELSE}
  {$IFDEF IOS}
  SavePath := TPath.GetDocumentsPath;
  {$ELSE}
  SavePath := TPath.GetPicturesPath;
  {$ENDIF}
  {$ENDIF}

  // Créer un nom de fichier unique basé sur la date et l'heure
  ImageFileName := 'Photo_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.jpg';
  SavePath := TPath.Combine(SavePath, ImageFileName);

  // Enregistrer l'image
  try
    ABitmap.SaveToFile(SavePath);
    ShowMessage('Photo enregistrée : ' + SavePath);
  except
    on E: Exception do
      ShowMessage('Erreur lors de l'enregistrement : ' + E.Message);
  end;
end;
```

### Choisir entre caméra avant et arrière

La plupart des appareils mobiles disposent de plusieurs caméras. Voici comment basculer entre elles :

```pascal
procedure TCameraForm.btnSwitchCameraClick(Sender: TObject);
var
  NewKind: TCameraKind;
begin
  // Désactiver la caméra actuelle
  if CameraComponent1.Active then
    CameraComponent1.Active := False;

  // Basculer entre les caméras avant et arrière
  if CameraComponent1.Kind = TCameraKind.FrontCamera then
    NewKind := TCameraKind.BackCamera
  else
    NewKind := TCameraKind.FrontCamera;

  // Essayer d'appliquer le changement
  try
    CameraComponent1.Kind := NewKind;
    // Réactiver la caméra
    CameraComponent1.Active := True;
  except
    on E: Exception do
    begin
      ShowMessage('Impossible de changer de caméra : ' + E.Message);
      // Essayer de réactiver la caméra précédente
      CameraComponent1.Active := True;
    end;
  end;
end;
```

## 2. Gestion des images

### Sélection d'images depuis la galerie

Permettre à l'utilisateur de choisir une image depuis la galerie de photos :

```pascal
uses
  FMX.MediaLibrary, FMX.Graphics;

procedure TImageForm.btnSelectImageClick(Sender: TObject);
begin
  TakePhotoFromLibrary(ImageControl1.Bitmap,
    procedure(const AResult: TTakePhotoResult)
    begin
      case AResult of
        TTakePhotoResult.Success:
          lblStatus.Text := 'Image chargée avec succès';
        TTakePhotoResult.Canceled:
          lblStatus.Text := 'Sélection annulée';
        TTakePhotoResult.Error:
          lblStatus.Text := 'Erreur lors du chargement de l'image';
      end;
    end);
end;
```

### Manipulation d'images

Delphi offre plusieurs possibilités pour manipuler des images :

```pascal
procedure TImageForm.ApplyGrayscaleFilter;
var
  W, H: Integer;
  X, Y: Integer;
  P: TAlphaColorRec;
  Gray: Byte;
  Bitmap: TBitmap;
begin
  // Créer une copie de l'image originale
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(ImageControl1.Bitmap);

    // Parcourir chaque pixel
    for Y := 0 to Bitmap.Height - 1 do
    begin
      for X := 0 to Bitmap.Width - 1 do
      begin
        // Obtenir la couleur du pixel
        P := TAlphaColorRec(Bitmap.Canvas.Pixels[X, Y]);

        // Calculer la valeur de gris (formule standard)
        Gray := Round(0.299 * P.R + 0.587 * P.G + 0.114 * P.B);

        // Appliquer la nouvelle couleur
        P.R := Gray;
        P.G := Gray;
        P.B := Gray;

        // Mettre à jour le pixel
        Bitmap.Canvas.Pixels[X, Y] := P.Color;
      end;
    end;

    // Afficher l'image modifiée
    ImageControl1.Bitmap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;
```

> **Note** : Ce traitement peut être lent pour des images volumineuses. Pour de meilleures performances sur des images de grande taille, utilisez des threads ou des bibliothèques optimisées.

### Partage d'images

Partager une image avec d'autres applications :

```pascal
uses
  FMX.MediaLibrary, FMX.Graphics, System.SysUtils, System.IOUtils;

procedure TImageForm.ShareImage(const ABitmap: TBitmap);
var
  TempPath, TempFile: string;
begin
  // Créer un fichier temporaire
  TempPath := TPath.GetTempPath;
  TempFile := TPath.Combine(TempPath, 'share_image_' +
                            FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');

  // Enregistrer l'image dans le fichier temporaire
  try
    ABitmap.SaveToFile(TempFile);

    // Partager le fichier
    {$IF DEFINED(ANDROID) or DEFINED(IOS)}
    ShareImage(TempFile, 'Partager cette image');
    {$ELSE}
    ShowMessage('Partage non supporté sur cette plateforme');
    {$ENDIF}
  except
    on E: Exception do
      ShowMessage('Erreur de partage : ' + E.Message);
  end;
end;
```

## 3. Lecture et enregistrement audio

### Lecture de fichiers audio

Delphi fournit le composant `TMediaPlayer` pour la lecture de fichiers audio :

```pascal
uses
  FMX.Media;

type
  TAudioForm = class(TForm)
    MediaPlayer1: TMediaPlayer;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    TrackBar1: TTrackBar;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FTrackBarChanging: Boolean;
  public
    { Public declarations }
  end;

implementation

procedure TAudioForm.FormCreate(Sender: TObject);
begin
  FTrackBarChanging := False;

  // Charger un fichier audio (à adapter selon l'emplacement de vos fichiers)
  {$IF DEFINED(ANDROID)}
  MediaPlayer1.FileName := TPath.Combine(TPath.GetDocumentsPath, 'sample.mp3');
  {$ELSEIF DEFINED(IOS)}
  MediaPlayer1.FileName := TPath.Combine(TPath.GetDocumentsPath, 'sample.mp3');
  {$ELSE}
  MediaPlayer1.FileName := 'C:\Samples\sample.mp3';
  {$ENDIF}

  // Configurer la TrackBar
  TrackBar1.Min := 0;
  Timer1.Enabled := True;
end;

procedure TAudioForm.btnPlayClick(Sender: TObject);
begin
  if MediaPlayer1.Media <> nil then
  begin
    MediaPlayer1.Play;
    TrackBar1.Max := MediaPlayer1.Duration;
  end;
end;

procedure TAudioForm.btnPauseClick(Sender: TObject);
begin
  if MediaPlayer1.Media <> nil then
    MediaPlayer1.Pause;
end;

procedure TAudioForm.btnStopClick(Sender: TObject);
begin
  if MediaPlayer1.Media <> nil then
  begin
    MediaPlayer1.Stop;
    TrackBar1.Value := 0;
  end;
end;

procedure TAudioForm.Timer1Timer(Sender: TObject);
begin
  // Mettre à jour la position de la TrackBar
  if (not FTrackBarChanging) and (MediaPlayer1.Media <> nil) and
     (MediaPlayer1.State = TMediaState.Playing) then
  begin
    TrackBar1.Value := MediaPlayer1.CurrentTime;
  end;
end;

procedure TAudioForm.TrackBar1Change(Sender: TObject);
begin
  if MediaPlayer1.Media <> nil then
  begin
    FTrackBarChanging := True;
    try
      MediaPlayer1.CurrentTime := Round(TrackBar1.Value);
    finally
      FTrackBarChanging := False;
    end;
  end;
end;
```

### Enregistrement audio

Pour enregistrer de l'audio depuis le microphone :

```pascal
uses
  FMX.Media;

type
  TRecordForm = class(TForm)
    MicrophoneComponent1: TAudioCaptureDevice;
    btnStartRecording: TButton;
    btnStopRecording: TButton;
    lblStatus: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStartRecordingClick(Sender: TObject);
    procedure btnStopRecordingClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FAudioFileName: string;
  public
    { Public declarations }
  end;

implementation

procedure TRecordForm.FormCreate(Sender: TObject);
begin
  // Vérifier la disponibilité du microphone
  if MicrophoneComponent1.Available then
    btnStartRecording.Enabled := True
  else
  begin
    btnStartRecording.Enabled := False;
    lblStatus.Text := 'Microphone non disponible';
  end;

  // Définir le nom du fichier d'enregistrement
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  FAudioFileName := TPath.Combine(TPath.GetDocumentsPath,
                              'Recording_' + FormatDateTime('yyyymmddhhnnss', Now) + '.wav');
  {$ELSE}
  FAudioFileName := TPath.Combine(TPath.GetMusicPath,
                              'Recording_' + FormatDateTime('yyyymmddhhnnss', Now) + '.wav');
  {$ENDIF}
end;

procedure TRecordForm.btnStartRecordingClick(Sender: TObject);
begin
  // Commencer l'enregistrement
  if not MicrophoneComponent1.Active then
  begin
    try
      // Configurer l'enregistrement
      MicrophoneComponent1.FileName := FAudioFileName;
      MicrophoneComponent1.Active := True;

      // Mettre à jour l'interface
      btnStartRecording.Enabled := False;
      btnStopRecording.Enabled := True;
      lblStatus.Text := 'Enregistrement en cours...';
    except
      on E: Exception do
        ShowMessage('Erreur au démarrage de l''enregistrement : ' + E.Message);
    end;
  end;
end;

procedure TRecordForm.btnStopRecordingClick(Sender: TObject);
begin
  // Arrêter l'enregistrement
  if MicrophoneComponent1.Active then
  begin
    MicrophoneComponent1.Active := False;

    // Mettre à jour l'interface
    btnStartRecording.Enabled := True;
    btnStopRecording.Enabled := False;
    lblStatus.Text := 'Enregistrement terminé : ' + FAudioFileName;

    // Créer un nouveau nom de fichier pour le prochain enregistrement
    {$IF DEFINED(ANDROID) or DEFINED(IOS)}
    FAudioFileName := TPath.Combine(TPath.GetDocumentsPath,
                              'Recording_' + FormatDateTime('yyyymmddhhnnss', Now) + '.wav');
    {$ELSE}
    FAudioFileName := TPath.Combine(TPath.GetMusicPath,
                              'Recording_' + FormatDateTime('yyyymmddhhnnss', Now) + '.wav');
    {$ENDIF}
  end;
end;

procedure TRecordForm.FormDestroy(Sender: TObject);
begin
  // S'assurer que l'enregistrement est arrêté
  if MicrophoneComponent1.Active then
    MicrophoneComponent1.Active := False;
end;
```

> **Note importante** : N'oubliez pas de configurer les permissions appropriées dans votre projet pour l'accès au microphone :
> - `RECORD_AUDIO` pour Android
> - `NSMicrophoneUsageDescription` dans Info.plist pour iOS

## 4. Lecture et enregistrement vidéo

### Lecture de vidéos

La lecture de vidéos utilise également le composant `TMediaPlayer` :

```pascal
uses
  FMX.Media, FMX.Types;

type
  TVideoForm = class(TForm)
    MediaPlayer1: TMediaPlayer;
    MediaPlayerControl1: TMediaPlayerControl;
    btnOpenVideo: TButton;
    procedure btnOpenVideoClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

procedure TVideoForm.btnOpenVideoClick(Sender: TObject);
begin
  // Cette fonction demandera à l'utilisateur de sélectionner une vidéo
  // depuis sa bibliothèque (utilise les API natives du système)
  {$IF DEFINED(ANDROID) or DEFINED(IOS)}
  SelectVideo(
    procedure(const AFileName: string)
    begin
      if AFileName <> '' then
      begin
        MediaPlayer1.FileName := AFileName;
        MediaPlayer1.Play;
      end;
    end);
  {$ELSE}
  ShowMessage('Sélection de vidéo non supportée sur cette plateforme');
  {$ENDIF}
end;

procedure TVideoForm.FormResize(Sender: TObject);
begin
  // Ajuster la taille du contrôle vidéo lors du redimensionnement
  MediaPlayerControl1.Size.Width := Self.Width - 20;
  MediaPlayerControl1.Size.Height := (MediaPlayerControl1.Size.Width * 9) / 16; // Ratio 16:9
end;
```

### Enregistrement vidéo

Pour lancer un enregistrement vidéo :

```pascal
uses
  FMX.MediaLibrary, System.Permissions;

type
  TVideoRecordForm = class(TForm)
    btnRecordVideo: TButton;
    procedure btnRecordVideoClick(Sender: TObject);
    procedure RecordVideoPermissionsResult(Sender: TObject;
      const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$IF DEFINED(ANDROID)}
procedure TVideoRecordForm.btnRecordVideoClick(Sender: TObject);
var
  PermissionsService: IFMXPermissionsService;
begin
  // Vérifier et demander les permissions nécessaires sur Android
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXPermissionsService, IInterface(PermissionsService)) then
  begin
    // Demander les permissions nécessaires
    PermissionsService.RequestPermissions(
      ['android.permission.CAMERA', 'android.permission.RECORD_AUDIO'],
      RecordVideoPermissionsResult, nil);
  end
  else
  begin
    // Continuer si le service de permissions n'est pas disponible
    TakeVideoFromCamera(
      procedure(const AVideo: string; ASuccess: Boolean)
      begin
        if ASuccess then
          ShowMessage('Vidéo enregistrée : ' + AVideo)
        else
          ShowMessage('Erreur ou annulation de l''enregistrement vidéo');
      end);
  end;
end;

procedure TVideoRecordForm.RecordVideoPermissionsResult(Sender: TObject;
  const APermissions: TClassicStringDynArray;
  const AGrantResults: TClassicPermissionStatusDynArray);
var
  AllGranted: Boolean;
  I: Integer;
begin
  // Vérifier si toutes les permissions ont été accordées
  AllGranted := True;
  for I := 0 to High(AGrantResults) do
    if AGrantResults[I] <> TPermissionStatus.Granted then
    begin
      AllGranted := False;
      Break;
    end;

  if AllGranted then
  begin
    // Lancer l'enregistrement vidéo
    TakeVideoFromCamera(
      procedure(const AVideo: string; ASuccess: Boolean)
      begin
        if ASuccess then
          ShowMessage('Vidéo enregistrée : ' + AVideo)
        else
          ShowMessage('Erreur ou annulation de l''enregistrement vidéo');
      end);
  end
  else
    ShowMessage('Permissions nécessaires non accordées');
end;
{$ELSE}
procedure TVideoRecordForm.btnRecordVideoClick(Sender: TObject);
begin
  // Pour iOS et autres plateformes
  TakeVideoFromCamera(
    procedure(const AVideo: string; ASuccess: Boolean)
    begin
      if ASuccess then
        ShowMessage('Vidéo enregistrée : ' + AVideo)
      else
        ShowMessage('Erreur ou annulation de l''enregistrement vidéo');
    end);
end;
{$ENDIF}
```

## 5. Considérations et bonnes pratiques

### Gestion de la mémoire

Les opérations multimédias peuvent consommer beaucoup de mémoire :

1. **Libérez les ressources** dès qu'elles ne sont plus nécessaires
2. **Redimensionnez les images** à une taille adaptée avant traitement
3. **Utilisez des streams** pour gérer les fichiers volumineux

```pascal
// Exemple de redimensionnement d'image pour économiser de la mémoire
procedure ResizeImageForDisplay(const SourceBitmap, DestBitmap: TBitmap;
                                MaxWidth, MaxHeight: Integer);
var
  AspectRatio: Single;
  NewWidth, NewHeight: Integer;
begin
  AspectRatio := SourceBitmap.Width / SourceBitmap.Height;

  if SourceBitmap.Width > SourceBitmap.Height then
  begin
    NewWidth := Min(SourceBitmap.Width, MaxWidth);
    NewHeight := Round(NewWidth / AspectRatio);

    if NewHeight > MaxHeight then
    begin
      NewHeight := MaxHeight;
      NewWidth := Round(NewHeight * AspectRatio);
    end;
  end
  else
  begin
    NewHeight := Min(SourceBitmap.Height, MaxHeight);
    NewWidth := Round(NewHeight * AspectRatio);

    if NewWidth > MaxWidth then
    begin
      NewWidth := MaxWidth;
      NewHeight := Round(NewWidth / AspectRatio);
    end;
  end;

  DestBitmap.SetSize(NewWidth, NewHeight);
  DestBitmap.Canvas.BeginScene;
  try
    DestBitmap.Canvas.DrawBitmap(SourceBitmap,
                                RectF(0, 0, SourceBitmap.Width, SourceBitmap.Height),
                                RectF(0, 0, NewWidth, NewHeight),
                                1);
  finally
    DestBitmap.Canvas.EndScene;
  end;
end;
```

### Adaptation aux spécificités des plateformes

Les comportements peuvent différer entre Android et iOS :

1. **Testez sur les deux plateformes** systématiquement
2. **Utilisez des directives de compilation conditionnelle** pour gérer les différences
3. **Prévoyez des chemins alternatifs** en cas de fonctionnalité non supportée

### Performances et économie d'énergie

Les opérations multimédias sont gourmandes en ressources :

1. **Désactivez la caméra** quand elle n'est pas visible
2. **Libérez les lecteurs multimédias** quand ils ne sont pas utilisés
3. **Optimisez la qualité des médias** selon les besoins réels de l'application
4. **Utilisez des threads séparés** pour les opérations de traitement d'images

```pascal
// Exemple d'utilisation d'un thread pour le traitement d'image
procedure TImageProcessingForm.ProcessImageInBackground(const SourceBitmap: TBitmap);
begin
  // Afficher un indicateur de chargement
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  // Lancer le traitement en arrière-plan
  TTask.Run(procedure
  var
    ProcessedBitmap: TBitmap;
  begin
    ProcessedBitmap := TBitmap.Create;
    try
      // Appliquer les filtres et traitements
      ApplyImageFilter(SourceBitmap, ProcessedBitmap);

      // Revenir au thread principal pour mettre à jour l'interface
      TThread.Synchronize(nil, procedure
      begin
        ImageControl1.Bitmap.Assign(ProcessedBitmap);

        // Masquer l'indicateur de chargement
        AniIndicator1.Enabled := False;
        AniIndicator1.Visible := False;
      end);
    finally
      ProcessedBitmap.Free;
    end;
  end);
end;
```

## 6. Exemple complet : Application de prise de photos avec filtres

Voici un exemple plus complet d'une application simple de prise de photos avec application de filtres :

```pascal
// Ce code est un exemple conceptuel qui montre comment structurer une telle application
// Certaines parties sont simplifiées pour la clarté

unit PhotoFilterUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Media,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, System.Threading,
  System.Permissions, FMX.MediaLibrary;

type
  TFilterType = (ftNone, ftGrayscale, ftSepia, ftNegative, ftBlur);

  TPhotoFilterForm = class(TForm)
    CameraComponent1: TCameraComponent;
    ImageControl1: TImageControl;
    ButtonPanel: TPanel;
    btnTakePhoto: TButton;
    btnApplyFilter: TButton;
    btnSavePhoto: TButton;
    btnSharePhoto: TButton;
    FilterPanel: TPanel;
    rbNone: TRadioButton;
    rbGrayscale: TRadioButton;
    rbSepia: TRadioButton;
    rbNegative: TRadioButton;
    rbBlur: TRadioButton;
    StatusBar: TStatusBar;
    lblStatus: TLabel;
    AniIndicator1: TAniIndicator;
    btnToggleCamera: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnTakePhotoClick(Sender: TObject);
    procedure btnApplyFilterClick(Sender: TObject);
    procedure btnSavePhotoClick(Sender: TObject);
    procedure btnSharePhotoClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnToggleCameraClick(Sender: TObject);
  private
    FOriginalBitmap: TBitmap;
    FFilteredBitmap: TBitmap;
    FHasPhoto: Boolean;

    procedure ShowStatus(const Msg: string);
    function GetSelectedFilter: TFilterType;
    procedure ApplyFilter(FilterType: TFilterType;
                         const SourceBitmap, DestBitmap: TBitmap);
    procedure SavePhotoToGallery(const Bitmap: TBitmap);
    procedure SharePhoto(const Bitmap: TBitmap);
    procedure CheckPermissions;
  public
    { Public declarations }
  end;

var
  PhotoFilterForm: TPhotoFilterForm;

implementation

{$R *.fmx}

procedure TPhotoFilterForm.FormCreate(Sender: TObject);
begin
  FOriginalBitmap := TBitmap.Create;
  FFilteredBitmap := TBitmap.Create;
  FHasPhoto := False;

  // Vérifier les permissions
  CheckPermissions;

  // Initialisation de l'interface
  AniIndicator1.Visible := False;
  rbNone.IsChecked := True;

  btnApplyFilter.Enabled := False;
  btnSavePhoto.Enabled := False;
  btnSharePhoto.Enabled := False;

  // Vérifier la disponibilité de la caméra
  if CameraComponent1.Available then
  begin
    btnTakePhoto.Enabled := True;
    btnToggleCamera.Enabled := True;

    // Démarrer automatiquement la caméra
    try
      CameraComponent1.Active := True;
      ShowStatus('Caméra activée');
    except
      on E: Exception do
        ShowStatus('Erreur d''activation de la caméra: ' + E.Message);
    end;
  end
  else
  begin
    btnTakePhoto.Enabled := False;
    btnToggleCamera.Enabled := False;
    ShowStatus('Pas de caméra disponible');
  end;
end;

procedure TPhotoFilterForm.CheckPermissions;
{$IF DEFINED(ANDROID)}
var
  PermissionsService: IFMXPermissionsService;
begin
  if TPlatformServices.Current.SupportsPlatformService(
       IFMXPermissionsService, IInterface(PermissionsService)) then
  begin
    // Demander les permissions nécessaires
    PermissionsService.RequestPermissions(
      ['android.permission.CAMERA',
       'android.permission.WRITE_EXTERNAL_STORAGE',
       'android.permission.READ_EXTERNAL_STORAGE'],
      nil, nil);
  end;
end;
{$ELSE}
begin
  // Sur iOS, les permissions sont gérées via Info.plist
end;
{$ENDIF}

procedure TPhotoFilterForm.btnTakePhotoClick(Sender: TObject);
begin
  if CameraComponent1.Active then
  begin
    // Capturer l'image depuis la caméra
    try
      CameraComponent1.SampleBufferToBitmap(FOriginalBitmap, True);

      // Copier l'image originale vers l'image affichée
      ImageControl1.Bitmap.Assign(FOriginalBitmap);

      FHasPhoto := True;
      btnApplyFilter.Enabled := True;
      btnSavePhoto.Enabled := True;
      btnSharePhoto.Enabled := True;

      ShowStatus('Photo prise');
    except
      on E: Exception do
        ShowStatus('Erreur lors de la prise de photo: ' + E.Message);
    end;
  end
  else
    ShowStatus('La caméra n''est pas active');
end;

procedure TPhotoFilterForm.btnToggleCameraClick(Sender: TObject);
var
  NewKind: TCameraKind;
begin
  // Désactiver temporairement la caméra
  if CameraComponent1.Active then
    CameraComponent1.Active := False;

  // Changer de caméra
  if CameraComponent1.Kind = TCameraKind.BackCamera then
    NewKind := TCameraKind.FrontCamera
  else
    NewKind := TCameraKind.BackCamera;

  try
    CameraComponent1.Kind := NewKind;
    // Réactiver la caméra
    CameraComponent1.Active := True;

    if NewKind = TCameraKind.BackCamera then
      ShowStatus('Caméra arrière activée')
    else
      ShowStatus('Caméra avant activée');
  except
    on E: Exception do
    begin
      ShowStatus('Erreur lors du changement de caméra: ' + E.Message);
      // Essayer de réactiver la caméra précédente
      CameraComponent1.Active := True;
    end;
  end;
end;

function TPhotoFilterForm.GetSelectedFilter: TFilterType;
begin
  if rbGrayscale.IsChecked then
    Result := ftGrayscale
  else if rbSepia.IsChecked then
    Result := ftSepia
  else if rbNegative.IsChecked then
    Result := ftNegative
  else if rbBlur.IsChecked then
    Result := ftBlur
  else
    Result := ftNone;
end;

procedure TPhotoFilterForm.btnApplyFilterClick(Sender: TObject);
var
  FilterType: TFilterType;
begin
  if not FHasPhoto then
  begin
    ShowStatus('Prenez d''abord une photo');
    Exit;
  end;

  // Désactiver les boutons pendant le traitement
  btnApplyFilter.Enabled := False;
  btnSavePhoto.Enabled := False;
  btnSharePhoto.Enabled := False;

  // Afficher l'indicateur d'activité
  AniIndicator1.Visible := True;
  AniIndicator1.Enabled := True;

  FilterType := GetSelectedFilter;
  ShowStatus('Application du filtre...');

  // Traiter l'image dans un thread séparé
  TTask.Run(procedure
  begin
    try
      // Créer une copie de l'image originale
      FFilteredBitmap.Assign(FOriginalBitmap);

      // Appliquer le filtre sélectionné
      ApplyFilter(FilterType, FOriginalBitmap, FFilteredBitmap);

      // Revenir au thread principal pour mettre à jour l'interface
      TThread.Synchronize(nil, procedure
      begin
        // Afficher l'image filtrée
        ImageControl1.Bitmap.Assign(FFilteredBitmap);

        // Réactiver les boutons
        btnApplyFilter.Enabled := True;
        btnSavePhoto.Enabled := True;
        btnSharePhoto.Enabled := True;

        // Masquer l'indicateur d'activité
        AniIndicator1.Enabled := False;
        AniIndicator1.Visible := False;

        ShowStatus('Filtre appliqué');
      end);
    except
      on E: Exception do
        TThread.Synchronize(nil, procedure
        begin
          // Gérer les erreurs
          ShowStatus('Erreur: ' + E.Message);

          // Réactiver les boutons
          btnApplyFilter.Enabled := True;
          btnSavePhoto.Enabled := True;
          btnSharePhoto.Enabled := True;

          // Masquer l'indicateur d'activité
          AniIndicator1.Enabled := False;
          AniIndicator1.Visible := False;
        end);
    end;
  end);
end;

procedure TPhotoFilterForm.ApplyFilter(FilterType: TFilterType;
                                     const SourceBitmap, DestBitmap: TBitmap);
var
  X, Y: Integer;
  SrcPixel, DstPixel: TAlphaColorRec;
  Gray: Byte;
begin
  // Pas de filtre - copie simple
  if FilterType = ftNone then
  begin
    DestBitmap.Assign(SourceBitmap);
    Exit;
  end;

  // Appliquer le filtre pixel par pixel
  for Y := 0 to SourceBitmap.Height - 1 do
  begin
    for X := 0 to SourceBitmap.Width - 1 do
    begin
      // Obtenir la couleur du pixel source
      SrcPixel := TAlphaColorRec(SourceBitmap.Canvas.Pixels[X, Y]);

      case FilterType of
        ftGrayscale:
        begin
          // Filtre noir et blanc
          Gray := Round(0.299 * SrcPixel.R + 0.587 * SrcPixel.G + 0.114 * SrcPixel.B);
          DstPixel.A := SrcPixel.A;
          DstPixel.R := Gray;
          DstPixel.G := Gray;
          DstPixel.B := Gray;
        end;

        ftSepia:
        begin
          // Filtre sépia
          DstPixel.A := SrcPixel.A;
          DstPixel.R := Min(255, Round(SrcPixel.R * 0.393 + SrcPixel.G * 0.769 + SrcPixel.B * 0.189));
          DstPixel.G := Min(255, Round(SrcPixel.R * 0.349 + SrcPixel.G * 0.686 + SrcPixel.B * 0.168));
          DstPixel.B := Min(255, Round(SrcPixel.R * 0.272 + SrcPixel.G * 0.534 + SrcPixel.B * 0.131));
        end;

        ftNegative:
        begin
          // Filtre négatif
          DstPixel.A := SrcPixel.A;
          DstPixel.R := 255 - SrcPixel.R;
          DstPixel.G := 255 - SrcPixel.G;
          DstPixel.B := 255 - SrcPixel.B;
        end;

        ftBlur:
        begin
          // Filtre flou simplifié (moyenne des pixels voisins)
          // Note: Un vrai filtre de flou utiliserait une matrice de convolution
          // Ceci est une implémentation simplifiée pour l'exemple
          DstPixel := SrcPixel; // Par défaut, garder le pixel original

          // Pour les pixels non situés au bord, appliquer un flou simple
          if (X > 0) and (X < SourceBitmap.Width - 1) and
             (Y > 0) and (Y < SourceBitmap.Height - 1) then
          begin
            // Collecter les pixels voisins et calculer la moyenne
            var SumR, SumG, SumB: Integer;
            SumR := 0;
            SumG := 0;
            SumB := 0;

            for var OffY := -1 to 1 do
            begin
              for var OffX := -1 to 1 do
              begin
                var NPixel := TAlphaColorRec(SourceBitmap.Canvas.Pixels[X + OffX, Y + OffY]);
                SumR := SumR + NPixel.R;
                SumG := SumG + NPixel.G;
                SumB := SumB + NPixel.B;
              end;
            end;

            // Calculer la moyenne (9 pixels)
            DstPixel.A := SrcPixel.A;
            DstPixel.R := SumR div 9;
            DstPixel.G := SumG div 9;
            DstPixel.B := SumB div 9;
          end;
        end;
      end;

      // Mettre à jour le pixel de destination
      DestBitmap.Canvas.Pixels[X, Y] := DstPixel.Color;
    end;
  end;
end;

procedure TPhotoFilterForm.btnSavePhotoClick(Sender: TObject);
begin
  if not FHasPhoto then
  begin
    ShowStatus('Prenez d''abord une photo');
    Exit;
  end;

  SavePhotoToGallery(ImageControl1.Bitmap);
end;

procedure TPhotoFilterForm.SavePhotoToGallery(const Bitmap: TBitmap);
var
  TempPath, TempFile: string;
begin
  TempPath := TPath.GetTempPath;
  TempFile := TPath.Combine(TempPath, 'photo_' +
                            FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');

  // Désactiver les boutons pendant l'opération
  btnSavePhoto.Enabled := False;

  ShowStatus('Enregistrement de l''image...');

  // Enregistrer dans un thread séparé
  TTask.Run(procedure
  begin
    try
      // Enregistrer l'image dans un fichier temporaire
      Bitmap.SaveToFile(TempFile);

      // Ajouter le fichier à la galerie
      {$IF DEFINED(ANDROID) or DEFINED(IOS)}
      TThread.Synchronize(nil, procedure
      begin
        // Ajouter à la galerie de photos
        TMediaLibrary.AddImageToSavedPhotosAlbum(TempFile,
          procedure(const AAdded: Boolean)
          begin
            if AAdded then
              ShowStatus('Photo enregistrée dans la galerie')
            else
              ShowStatus('Erreur lors de l''enregistrement dans la galerie');

            btnSavePhoto.Enabled := True;

            // Supprimer le fichier temporaire
            TFile.Delete(TempFile);
          end);
      end);
      {$ELSE}
      // Sur les autres plateformes, copier vers le dossier Images
      var DestFile := TPath.Combine(TPath.GetPicturesPath,
                                   'photo_' + FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');
      TFile.Copy(TempFile, DestFile);
      TFile.Delete(TempFile);

      TThread.Synchronize(nil, procedure
      begin
        ShowStatus('Photo enregistrée: ' + DestFile);
        btnSavePhoto.Enabled := True;
      end);
      {$ENDIF}
    except
      on E: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          ShowStatus('Erreur d''enregistrement: ' + E.Message);
          btnSavePhoto.Enabled := True;
        end);
      end;
    end;
  end);
end;

procedure TPhotoFilterForm.btnSharePhotoClick(Sender: TObject);
begin
  if not FHasPhoto then
  begin
    ShowStatus('Prenez d''abord une photo');
    Exit;
  end;

  SharePhoto(ImageControl1.Bitmap);
end;

procedure TPhotoFilterForm.SharePhoto(const Bitmap: TBitmap);
var
  TempPath, TempFile: string;
begin
  TempPath := TPath.GetTempPath;
  TempFile := TPath.Combine(TempPath, 'share_' +
                            FormatDateTime('yyyymmddhhnnss', Now) + '.jpg');

  // Désactiver les boutons pendant l'opération
  btnSharePhoto.Enabled := False;

  ShowStatus('Préparation du partage...');

  // Préparer le partage dans un thread séparé
  TTask.Run(procedure
  begin
    try
      // Enregistrer l'image dans un fichier temporaire
      Bitmap.SaveToFile(TempFile);

      // Partager le fichier
      TThread.Synchronize(nil, procedure
      begin
        {$IF DEFINED(ANDROID) or DEFINED(IOS)}
        var Intent := TJIntent.Create;
        Intent.setAction(TJIntent.JavaClass.ACTION_SEND);
        Intent.setType(StringToJString('image/jpeg'));

        var Uri := TJnet_Uri.JavaClass.fromFile(TJFile.JavaClass.init(StringToJString(TempFile)));
        Intent.putExtra(TJIntent.JavaClass.EXTRA_STREAM, TJParcelable.Wrap(Uri));

        SharedActivity.startActivity(TJIntent.JavaClass.createChooser(Intent,
                                     StringToJString('Partager l''image via')));
        {$ELSE}
        // Sur les autres plateformes, afficher un message
        ShowMessage('Le partage n''est pas disponible sur cette plateforme.');
        {$ENDIF}

        ShowStatus('Image prête pour le partage');
        btnSharePhoto.Enabled := True;
      end);
    except
      on E: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          ShowStatus('Erreur de partage: ' + E.Message);
          btnSharePhoto.Enabled := True;
        end);
      end;
    end;
  end);
end;

procedure TPhotoFilterForm.ShowStatus(const Msg: string);
begin
  lblStatus.Text := Msg;
end;

procedure TPhotoFilterForm.FormDestroy(Sender: TObject);
begin
  // Libérer les ressources
  if CameraComponent1.Active then
    CameraComponent1.Active := False;

  FOriginalBitmap.Free;
  FFilteredBitmap.Free;
end;
```

## 7. Gestion des médias sur les différentes plateformes

### Différences entre Android et iOS

Bien que Delphi offre une abstraction qui permet d'écrire du code multi-plateformes, certaines différences subsistent entre Android et iOS en matière de gestion des médias :

| Fonctionnalité | Android | iOS |
|----------------|---------|-----|
| Accès à la galerie | Nécessite les permissions READ_EXTERNAL_STORAGE | Nécessite NSPhotoLibraryUsageDescription |
| Enregistrement dans la galerie | Nécessite WRITE_EXTERNAL_STORAGE | Accès via MediaLibrary API |
| Formats de média | Plus permissif avec divers formats | Plus restrictif, préfère les formats Apple |
| Accès aux fichiers | Accès direct au système de fichiers | Environnement plus sandboxé |
| Performances | Variable selon les appareils | Plus homogène |

### Adaptation aux contraintes de chaque plateforme

Pour assurer une expérience optimale sur toutes les plateformes :

1. **Testez sur des appareils réels** de chaque plateforme
2. **Utilisez des directives conditionnelles** pour gérer les différences
3. **Définissez des chemins d'accès appropriés** pour chaque plateforme
4. **Adaptez la qualité des médias** selon les performances des appareils cibles

## 8. Bonnes pratiques pour utiliser la caméra et les médias

### Performance et réactivité

1. **Utilisez des threads pour les opérations intensives**
   - Traitement d'images
   - Encodage/décodage
   - Opérations d'E/S

2. **Surveillez l'utilisation de la mémoire**
   - Les bitmaps de grande taille peuvent rapidement consommer toute la mémoire
   - Redimensionnez les images quand c'est possible
   - Libérez les ressources non utilisées

3. **Fournissez un retour visuel**
   - Indicateurs de chargement
   - Barres de progression
   - Messages d'état

### Expérience utilisateur

1. **Respectez les conventions de la plateforme**
   - Sur iOS, utilisez des contrôles qui correspondent au design d'Apple
   - Sur Android, suivez les directives Material Design

2. **Gérez correctement les orientations**
   - Adaptez l'interface aux orientations portrait et paysage
   - Ajustez la prévisualisation de la caméra en fonction de l'orientation

3. **Optimisez les demandes de permission**
   - Expliquez pourquoi votre app a besoin d'accéder à la caméra ou aux médias
   - Demandez les permissions au moment approprié (pas toutes au démarrage)
   - Prévoyez un comportement dégradé si l'accès est refusé

## 9. Applications pratiques

Voici quelques idées d'applications que vous pouvez réaliser avec les connaissances acquises dans ce chapitre :

1. **Application de scanner de documents**
   - Prise de photo de documents
   - Recadrage automatique
   - Correction de perspective
   - Export en PDF

2. **Application de retouche photo**
   - Filtres personnalisés
   - Ajustements (luminosité, contraste, etc.)
   - Collages et effets

3. **Application de reconnaissance**
   - Reconnaissance de codes QR/codes-barres
   - Reconnaissance de texte (OCR)
   - Identification d'objets

4. **Application d'enregistrement vocal**
   - Enregistrement de mémos
   - Organisation par catégories
   - Transposition texte/vocal

## Conclusion

L'intégration de la caméra et des fonctionnalités multimédias dans vos applications Delphi ouvre un vaste champ de possibilités. En appliquant les techniques présentées dans ce chapitre, vous pourrez créer des applications mobiles riches qui tirent pleinement parti des capacités des appareils modernes.

Les points clés à retenir sont :
- La configuration correcte des permissions est essentielle
- Le cycle de vie des composants multimédias doit être géré soigneusement
- Les opérations intensives doivent être exécutées dans des threads séparés
- Les ressources doivent être libérées dès qu'elles ne sont plus nécessaires
- L'adaptation aux spécificités de chaque plateforme améliore l'expérience utilisateur

Dans la prochaine section, nous explorerons les notifications push et locales pour maintenir l'engagement des utilisateurs avec votre application, même lorsqu'elle n'est pas au premier plan.

⏭️ [Notifications](/15-applications-mobiles-avec-delphi/05-notifications.md)
