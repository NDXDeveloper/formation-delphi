# 5.6 Gestion du tactile et des gestes

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'un des grands avantages du développement multi-plateforme avec FireMonkey est la possibilité de créer des applications qui fonctionnent aussi bien avec une souris qu'avec des écrans tactiles. Dans cette section, nous allons explorer comment implémenter et gérer les interactions tactiles et les gestes pour rendre vos applications plus intuitives et agréables à utiliser sur les appareils mobiles et les écrans tactiles.

## Principes fondamentaux du tactile

Avant de plonger dans le code, voici quelques principes à comprendre:

- **Événements de base**: tap (toucher), pan (glisser), pinch (pincer)
- **Interactions directes**: l'utilisateur interagit directement avec les éléments visuels
- **Retour visuel**: les utilisateurs ont besoin d'un retour visuel pour confirmer leurs actions
- **Taille des cibles**: les éléments tactiles doivent être suffisamment grands (au moins 44×44 points)

## Événements tactiles standard

FireMonkey gère automatiquement les événements tactiles de base et les convertit en événements souris équivalents. Ainsi, un bouton réagira naturellement aux touchers tactiles sans code supplémentaire.

Les événements tactiles standard incluent:

```pascal
// Événements équivalents à la souris, fonctionnent aussi en tactile
Button1.OnClick := ButtonClickHandler;      // Clic / Tap simple
Button1.OnMouseDown := MouseDownHandler;    // Appui / Toucher
Button1.OnMouseUp := MouseUpHandler;        // Relâchement / Fin de toucher
Button1.OnMouseMove := MouseMoveHandler;    // Déplacement
```

## Gestion des événements tactiles spécifiques

Pour des interactions plus avancées, FireMonkey propose des événements tactiles spécifiques:

```pascal
// Gestionnaires d'événements tactiles spécifiques
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer le tactile sur le formulaire et ses enfants
  Touch.EnableGestures := True;

  // Configurer les événements tactiles
  OnTap := HandleTap;
  OnTapHold := HandleTapHold;
  OnDoubleTap := HandleDoubleTap;
  OnTouch := HandleTouch;
end;

procedure TForm1.HandleTap(Sender: TObject; const Point: TPointF);
begin
  ShowMessage('Tap à la position: ' + FloatToStr(Point.X) + ',' + FloatToStr(Point.Y));
end;

procedure TForm1.HandleDoubleTap(Sender: TObject; const Point: TPointF);
begin
  ShowMessage('Double tap détecté!');
end;

procedure TForm1.HandleTapHold(Sender: TObject; const Point: TPointF);
begin
  ShowMessage('Tap maintenu (équivalent clic droit)');
  // Souvent utilisé pour afficher un menu contextuel
  PopupMenu1.Popup(Round(Point.X), Round(Point.Y));
end;

procedure TForm1.HandleTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
begin
  // Accès à toutes les informations de toucher
  if Length(Touches) > 0 then
  begin
    Label1.Text := 'Nombre de points de contact: ' + IntToStr(Length(Touches));

    // Informations sur le premier point de contact
    Label2.Text := Format('Position: (%.1f, %.1f)', [Touches[0].Location.X, Touches[0].Location.Y]);

    // Action tactile (début, mouvement, fin)
    case Action of
      TTouchAction.Down: Label3.Text := 'Action: Contact initial';
      TTouchAction.Move: Label3.Text := 'Action: Mouvement';
      TTouchAction.Up: Label3.Text := 'Action: Fin de contact';
    end;
  end;
end;
```

## Gestion des gestes

Les gestes sont des interactions tactiles plus complexes comme le pincement, la rotation ou le balayage. FireMonkey inclut un système complet de gestion des gestes:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activation des gestes sur le formulaire
  Touch.GestureManager.Enabled := True;

  // Spécifier les gestes reconnus
  Touch.GestureManager.RecognizedGestures := [
    TGestureType.Standard, // Gestes standards (tap, double tap...)
    TGestureType.Swipe,    // Balayage
    TGestureType.Zoom,     // Pincement pour zoom
    TGestureType.Pan,      // Déplacement/Panoramique
    TGestureType.Rotate,   // Rotation
    TGestureType.LongTap,  // Appui long
    TGestureType.TwoFingerTap // Appui à deux doigts
  ];

  // Configurer le gestionnaire d'événements pour les gestes
  OnGesture := HandleGesture;
end;

procedure TForm1.HandleGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Déterminer le type de geste
  case EventInfo.GestureID of
    igiZoom:
      begin
        // Geste de pincement pour zoom
        // EventInfo.Distance contient la distance entre les doigts
        // Plus grand que 1 = écartement (zoom avant)
        // Plus petit que 1 = rapprochement (zoom arrière)
        if EventInfo.Distance > 1 then
          Label1.Text := 'Zoom avant: ' + FormatFloat('0.00', EventInfo.Distance)
        else
          Label1.Text := 'Zoom arrière: ' + FormatFloat('0.00', EventInfo.Distance);

        // Exemple: redimensionner une image
        Image1.Width := Image1.Width * EventInfo.Distance;
        Image1.Height := Image1.Height * EventInfo.Distance;

        Handled := True;
      end;

    igiPan:
      begin
        // Geste de déplacement
        // EventInfo.Location = position actuelle
        // EventInfo.Distance = distance depuis le début

        // Exemple: déplacer un élément
        Rectangle1.Position.X := Rectangle1.Position.X + EventInfo.Location.X - FLastX;
        Rectangle1.Position.Y := Rectangle1.Position.Y + EventInfo.Location.Y - FLastY;

        FLastX := EventInfo.Location.X;
        FLastY := EventInfo.Location.Y;

        Handled := True;
      end;

    igiRotate:
      begin
        // Geste de rotation
        // EventInfo.Angle = angle de rotation en radians

        // Exemple: faire pivoter une image
        Image1.RotationAngle := Image1.RotationAngle + (EventInfo.Angle * 180 / PI);

        Handled := True;
      end;

    igiSwipe:
      begin
        // Geste de balayage
        case EventInfo.GestureParameter of
          0: Label1.Text := 'Balayage vers la droite';
          1: Label1.Text := 'Balayage vers la gauche';
          2: Label1.Text := 'Balayage vers le haut';
          3: Label1.Text := 'Balayage vers le bas';
        end;

        Handled := True;
      end;

    igiLongTap:
      begin
        // Appui long
        Label1.Text := 'Appui long détecté';

        // Souvent utilisé pour afficher un menu contextuel
        PopupMenu1.Popup(Round(EventInfo.Location.X), Round(EventInfo.Location.Y));

        Handled := True;
      end;
  end;
end;
```

## Cas d'usage pratiques

### 1. Liste défilante avec inertie

Les listes défilantes sont une interface tactile courante. Utilisons un `TListBox` avec inertie:

```pascal
procedure TForm1.ConfigurerListeDefilante;
begin
  ListBox1.AllowDrawing := True;           // Activer le dessin personnalisé si nécessaire
  ListBox1.AniCalculations.Animation := True;  // Activer l'animation
  ListBox1.AniCalculations.BoundsAnimation := True; // Effet de rebond aux limites
  ListBox1.AniCalculations.TouchTracking := [ttVertical]; // Défilement vertical uniquement

  // Remplir la liste avec des éléments de test
  for var i := 1 to 50 do
    ListBox1.Items.Add('Élément ' + i.ToString);
end;
```

### 2. Zoom et panoramique sur une image

Cette fonctionnalité est courante dans les visionneuses de photos et les applications de cartographie:

```pascal
procedure TForm1.ConfigurerImageZoomable;
begin
  // Créer un conteneur pour l'image
  ZoomContainer := TLayout.Create(Self);
  ZoomContainer.Parent := Self;
  ZoomContainer.Align := TAlignLayout.Client;

  // Ajouter l'image
  Image1 := TImage.Create(Self);
  Image1.Parent := ZoomContainer;
  Image1.WrapMode := TImageWrapMode.Original; // Ne pas redimensionner automatiquement
  Image1.Position.X := 0;
  Image1.Position.Y := 0;
  Image1.Bitmap.LoadFromFile('large_image.jpg');

  // Configurer les gestes sur le conteneur
  ZoomContainer.Touch.GestureManager.Enabled := True;
  ZoomContainer.Touch.GestureManager.RecognizedGestures :=
    [TGestureType.Zoom, TGestureType.Pan];
  ZoomContainer.OnGesture := HandleImageGesture;

  // Variables pour suivre l'état de zoom
  FCurrentScale := 1.0;
  FMinScale := 0.5;
  FMaxScale := 3.0;
end;

procedure TForm1.HandleImageGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiZoom:
      begin
        // Calculer la nouvelle échelle
        var NewScale := FCurrentScale * EventInfo.Distance;

        // Limiter le zoom min/max
        if (NewScale >= FMinScale) and (NewScale <= FMaxScale) then
        begin
          // Appliquer le nouveau zoom en conservant le point central
          var CenterX := EventInfo.Location.X;
          var CenterY := EventInfo.Location.Y;

          // Calculer la nouvelle position pour maintenir le point de zoom centré
          Image1.Scale.X := NewScale;
          Image1.Scale.Y := NewScale;

          FCurrentScale := NewScale;
        end;

        Handled := True;
      end;

    igiPan:
      begin
        // Déplacer l'image (panoramique)
        if FCurrentScale > 1.0 then
        begin
          Image1.Position.X := Image1.Position.X + (EventInfo.Location.X - FLastX);
          Image1.Position.Y := Image1.Position.Y + (EventInfo.Location.Y - FLastY);

          // Limiter le déplacement pour éviter les espaces vides
          LimiterDeplacementImage;
        end;

        FLastX := EventInfo.Location.X;
        FLastY := EventInfo.Location.Y;

        Handled := True;
      end;
  end;
end;

procedure TForm1.LimiterDeplacementImage;
var
  MinX, MinY, MaxX, MaxY: Single;
begin
  // Calculer les limites de déplacement pour éviter les bords vides
  MinX := ZoomContainer.Width - (Image1.Width * FCurrentScale);
  MinY := ZoomContainer.Height - (Image1.Height * FCurrentScale);
  MaxX := 0;
  MaxY := 0;

  // Si l'image zoomée est plus petite que le conteneur, la centrer
  if MinX > 0 then
  begin
    MinX := -MinX / 2;
    MaxX := MinX;
  end;

  if MinY > 0 then
  begin
    MinY := -MinY / 2;
    MaxY := MinY;
  end;

  // Appliquer les limites
  if Image1.Position.X < MinX then Image1.Position.X := MinX;
  if Image1.Position.Y < MinY then Image1.Position.Y := MinY;
  if Image1.Position.X > MaxX then Image1.Position.X := MaxX;
  if Image1.Position.Y > MaxY then Image1.Position.Y := MaxY;
end;
```

### 3. Galerie d'images avec balayage

Création d'une galerie d'images simple où l'utilisateur peut naviguer par balayage:

```pascal
procedure TForm1.ConfigurerGalerieImages;
begin
  // Configuration du conteneur
  GalerieContainer := TLayout.Create(Self);
  GalerieContainer.Parent := Self;
  GalerieContainer.Align := TAlignLayout.Client;

  // Activer les gestes
  GalerieContainer.Touch.GestureManager.Enabled := True;
  GalerieContainer.Touch.GestureManager.RecognizedGestures := [TGestureType.Swipe];
  GalerieContainer.OnGesture := HandleGalerieGesture;

  // Charger les images
  ChargerImages;

  // Afficher la première image
  AfficherImage(0);
end;

procedure TForm1.ChargerImages;
begin
  // Initialiser la liste d'images
  FImages := TList<TBitmap>.Create;

  // Charger les images depuis un dossier
  var Files := TDirectory.GetFiles('Images', '*.jpg');
  for var FileName in Files do
  begin
    var Bitmap := TBitmap.Create;
    Bitmap.LoadFromFile(FileName);
    FImages.Add(Bitmap);
  end;

  FCurrentImageIndex := 0;
end;

procedure TForm1.AfficherImage(Index: Integer);
begin
  // Vérifier l'index
  if (Index < 0) or (Index >= FImages.Count) then
    Exit;

  // Mettre à jour l'index courant
  FCurrentImageIndex := Index;

  // Créer ou réutiliser l'image
  if not Assigned(FImageView) then
  begin
    FImageView := TImage.Create(Self);
    FImageView.Parent := GalerieContainer;
    FImageView.Align := TAlignLayout.Client;
    FImageView.WrapMode := TImageWrapMode.Fit;
  end;

  // Afficher l'image
  FImageView.Bitmap.Assign(FImages[Index]);

  // Mettre à jour l'indicateur de position
  LabelPosition.Text := Format('%d/%d', [Index + 1, FImages.Count]);
end;

procedure TForm1.HandleGalerieGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiSwipe then
  begin
    case EventInfo.GestureParameter of
      // Balayage vers la droite (image précédente)
      0: AfficherImage(FCurrentImageIndex - 1);

      // Balayage vers la gauche (image suivante)
      1: AfficherImage(FCurrentImageIndex + 1);
    end;

    Handled := True;
  end;
end;
```

## Boutons et contrôles adaptés au tactile

Pour une meilleure expérience tactile, les contrôles doivent être suffisamment grands. Voici quelques conseils:

```pascal
procedure TForm1.ConfigurerControlsTactiles;
begin
  // Boutons plus grands pour le tactile
  Button1.Height := 50;
  Button1.Margins.Top := 10;
  Button1.Margins.Bottom := 10;

  // Espacement plus grand pour les CheckBox et RadioButton
  CheckBox1.Height := 40;
  RadioButton1.Height := 40;

  // Plus grand espacement entre les éléments de liste
  ListBox1.ItemHeight := 44;
end;
```

## Retour visuel pour les interactions tactiles

Les utilisateurs ont besoin de feedback visuel lorsqu'ils interagissent avec l'écran:

```pascal
procedure TForm1.ConfigurerRetourVisuel;
begin
  // Ajouter un effet d'animation au clic sur un bouton
  Button1.OnClick := ButtonClickWithFeedback;

  // Configurer l'animation
  FButtonAnimation := TColorAnimation.Create(Self);
  FButtonAnimation.Parent := Button1;
  FButtonAnimation.Duration := 0.2;
  FButtonAnimation.StartValue := TAlphaColors.Blue;
  FButtonAnimation.StopValue := TAlphaColors.Skyblue;
  FButtonAnimation.PropertyName := 'Fill.Color';
  FButtonAnimation.Trigger := 'IsMouseOver=true';
  FButtonAnimation.TriggerInverse := 'IsMouseOver=false';
  FButtonAnimation.Enabled := True;
end;

procedure TForm1.ButtonClickWithFeedback(Sender: TObject);
var
  Button: TButton;
  OriginalColor: TAlphaColor;
begin
  Button := Sender as TButton;
  OriginalColor := Button.Fill.Color;

  // Changer la couleur au clic
  Button.Fill.Color := TAlphaColors.Red;

  // Planifier le retour à la couleur d'origine
  TThread.CreateAnonymousThread(procedure
  begin
    Sleep(100);
    TThread.Synchronize(nil, procedure
    begin
      Button.Fill.Color := OriginalColor;
      // Exécuter l'action normale du bouton ici
      ExecuterActionBouton(Button);
    end);
  end).Start;
end;
```

## Adaptation à différents types d'appareils

Les interactions tactiles diffèrent selon le type d'appareil:

```pascal
procedure TForm1.AdapterInteractionsPourAppareil;
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    // Configuration pour appareils mobiles (tactile natif)
    ConfigurerPourMobile;
  {$ELSEIF DEFINED(MSWINDOWS)}
    // Configuration hybride pour Windows (souris + tactile possible)
    if TOSVersion.Check(10) then
      ConfigurerPourWindowsTactile
    else
      ConfigurerPourWindowsClassique;
  {$ENDIF}
end;

procedure TForm1.ConfigurerPourMobile;
begin
  // Contrôles plus grands
  AjusterTailleBoutons(1.5);  // Multiplier la taille par 1.5

  // Activer tous les gestes
  Touch.GestureManager.RecognizedGestures := [
    TGestureType.Standard, TGestureType.Swipe, TGestureType.Zoom,
    TGestureType.Pan, TGestureType.Rotate, TGestureType.LongTap,
    TGestureType.TwoFingerTap
  ];

  // Optimiser la liste pour le tactile
  ListBox1.AniCalculations.TouchTracking := [ttVertical];
  ListBox1.AniCalculations.Animation := True;
  ListBox1.AniCalculations.BoundsAnimation := True;
  ListBox1.AniCalculations.Averaging := True;
  ListBox1.AniCalculations.DecelerationRate := 0.7;  // Inertie plus importante

  // Ajouter des effets visuels tactiles
  ConfigurerRetourVisuel;
end;
```

## Meilleures pratiques pour les interfaces tactiles

1. **Taille des éléments tactiles**
   - Utilisez des boutons d'au moins 44×44 points
   - Prévoyez un espacement suffisant entre les éléments tactiles

2. **Retour visuel**
   - Fournissez toujours un feedback visuel pour les interactions
   - Utilisez des animations et des changements d'état visuels

3. **Conception intuitive**
   - Utilisez des gestes standard que les utilisateurs connaissent déjà
   - Évitez les gestes complexes ou non évidents

4. **Performance**
   - Les interfaces tactiles doivent être réactives et fluides
   - Optimisez vos animations pour maintenir 60 FPS

5. **Test sur des appareils réels**
   - Ne vous fiez pas uniquement aux simulateurs
   - Testez sur différents appareils avec différentes tailles d'écran

## Débogage des interactions tactiles

Voici une technique simple pour déboguer les interactions tactiles:

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer un label pour afficher les informations de débogage
  FDebugLabel := TLabel.Create(Self);
  FDebugLabel.Parent := Self;
  FDebugLabel.Align := TAlignLayout.Bottom;
  FDebugLabel.Height := 100;
  FDebugLabel.TextSettings.WordWrap := True;
  FDebugLabel.TextSettings.HorzAlign := TTextAlign.Leading;

  // Activer le débogage tactile
  OnTouch := DebugTouch;
  Touch.GestureManager.Enabled := True;
  OnGesture := DebugGesture;
end;

procedure TForm1.DebugTouch(Sender: TObject; const Touches: TTouches; const Action: TTouchAction);
var
  Info: string;
begin
  Info := Format('Points: %d Action: %s', [Length(Touches), GetEnumName(TypeInfo(TTouchAction), Ord(Action))]);

  if Length(Touches) > 0 then
    Info := Info + Format(' Pos: (%.1f, %.1f)', [Touches[0].Location.X, Touches[0].Location.Y]);

  FDebugLabel.Text := Info;
end;

procedure TForm1.DebugGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  GestureName: string;
begin
  case EventInfo.GestureID of
    igiZoom: GestureName := 'Zoom';
    igiPan: GestureName := 'Pan';
    igiRotate: GestureName := 'Rotate';
    igiSwipe: GestureName := 'Swipe';
    igiLongTap: GestureName := 'LongTap';
    igiDoubleTap: GestureName := 'DoubleTap';
    else GestureName := 'Unknown';
  end;

  FDebugLabel.Text := Format('Geste: %s Pos: (%.1f, %.1f) Param: %d',
    [GestureName, EventInfo.Location.X, EventInfo.Location.Y, EventInfo.GestureParameter]);

  // Ne pas marquer comme géré pour permettre le traitement normal
  Handled := False;
end;
```

## Conclusion

La gestion du tactile et des gestes est essentielle pour créer des applications multi-plateformes modernes et intuitives. FireMonkey fournit un système puissant et flexible qui vous permet d'implémenter facilement des interactions tactiles sophistiquées.

En utilisant les événements tactiles standards, la reconnaissance de gestes et les techniques présentées dans cette section, vous pouvez créer des applications qui offrent une expérience utilisateur naturelle et agréable sur tous les appareils, des smartphones aux ordinateurs portables avec écran tactile.

Dans la section suivante, nous explorerons comment cibler spécifiquement différentes plateformes comme Windows, macOS, iOS, Android et Linux avec FireMonkey.

⏭️ [Ciblage des plateformes : Windows, macOS, iOS, Android, Linux](/05-developpement-multi-plateforme-avec-firemonkey/07-ciblage-des-plateformes.md)
