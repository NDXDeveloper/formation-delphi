🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.6 Gestion du tactile et des gestes

## Introduction

Les interfaces tactiles ont révolutionné notre façon d'interagir avec les appareils. Là où la souris nécessite un pointeur précis et des clics, le tactile utilise des gestes naturels : taper, glisser, pincer, faire tourner. Dans cette section, nous allons explorer comment FireMonkey gère ces interactions tactiles et comment créer des interfaces qui répondent intuitivement aux gestes des utilisateurs.

## 1. Tactile vs Souris : comprendre les différences

### Interaction à la souris (Desktop)

**Caractéristiques** :
- **Pointeur précis** : Curseur de 1 pixel
- **Survol** : Détection du passage sans clic
- **Boutons multiples** : Clic gauche, droit, molette
- **Clic** : Action précise et rapide
- **Double-clic** : Action rapide répétée

**Événements typiques** :
```pascal
procedure TForm1.ButtonMouseEnter(Sender: TObject);  
begin  
  // Souris entre dans la zone du bouton
  Button1.Opacity := 0.8;
end;

procedure TForm1.ButtonMouseLeave(Sender: TObject);  
begin  
  // Souris sort de la zone du bouton
  Button1.Opacity := 1.0;
end;

procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  // Clic sur le bouton
  ShowMessage('Clic !');
end;
```

### Interaction tactile (Mobile/Tablette)

**Caractéristiques** :
- **Contact large** : Doigt de 8-10mm de diamètre
- **Pas de survol** : Pas de détection avant le contact
- **Gestes riches** : Tap, swipe, pinch, rotate, long press
- **Multi-touch** : Plusieurs doigts simultanément
- **Pression** : Certains écrans détectent la force du contact

**Événements typiques** :
```pascal
procedure TForm1.ButtonTap(Sender: TObject; const Point: TPointF);  
begin  
  // Tap (équivalent du clic) sur le bouton
  ShowMessage('Tap !');
end;

procedure TForm1.ImageSwipe(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Geste de balayage sur l'image
  if EventInfo.GestureID = igiRight then
    ImageSuivante;
end;
```

### Implications pour le développement

**Sur desktop (souris)** :
- Zones cliquables peuvent être petites (10x10 pixels)
- Effets de survol possibles
- Menus contextuels (clic droit)
- Double-clic fréquent

**Sur mobile (tactile)** :
- Zones tactiles doivent être grandes (minimum 44x44 pixels)
- Pas d'effet de survol
- Gestes pour naviguer et manipuler
- Long press pour les actions secondaires

**FireMonkey unifie les deux** :
- Les événements souris fonctionnent sur tactile
- Les événements tactiles fonctionnent avec la souris (simulés)
- Un seul code pour les deux interactions

## 2. Les événements tactiles de base

### OnTap - Le "clic" tactile

`OnTap` est l'équivalent tactile du `OnClick`. C'est un contact bref et direct.

```pascal
procedure TForm1.Button1Tap(Sender: TObject; const Point: TPointF);  
begin  
  // Point contient les coordonnées exactes du tap
  ShowMessage('Tap à la position : ' +
              Point.X.ToString + ', ' + Point.Y.ToString);
end;
```

**Quand l'utiliser** :
- Actions principales (boutons, liens)
- Sélection d'éléments
- Déclenchement d'actions simples

### OnTouch - Contact brut

`OnTouch` donne accès aux événements tactiles bruts : début, mouvement, fin du contact.

```pascal
procedure TForm1.Panel1Touch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  Point: TPointF;
begin
  case Action of
    TTouchAction.Down:
    begin
      // Doigt touche l'écran
      Point := Touches[0].Location;
      Label1.Text := 'Touch Down';
    end;

    TTouchAction.Move:
    begin
      // Doigt se déplace
      Point := Touches[0].Location;
      Label1.Text := 'Touch Move : ' + Point.X.ToString;
    end;

    TTouchAction.Up:
    begin
      // Doigt quitte l'écran
      Label1.Text := 'Touch Up';
    end;
  end;
end;
```

**Quand l'utiliser** :
- Dessin libre
- Contrôles personnalisés
- Suivi précis du doigt
- Interactions complexes

### OnMouseDown, OnMouseMove, OnMouseUp

Ces événements "souris" fonctionnent aussi pour le tactile :

```pascal
procedure TForm1.Canvas1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  // Fonctionne pour clic souris ET contact tactile
  FDernierPoint := PointF(X, Y);
  FEnTrainDeDessiner := True;
end;

procedure TForm1.Canvas1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  if FEnTrainDeDessiner then
  begin
    // Dessiner une ligne
    Canvas1.Canvas.BeginScene;
    try
      Canvas1.Canvas.DrawLine(FDernierPoint, PointF(X, Y), 1.0);
      FDernierPoint := PointF(X, Y);
    finally
      Canvas1.Canvas.EndScene;
    end;
  end;
end;

procedure TForm1.Canvas1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FEnTrainDeDessiner := False;
end;
```

**Avantage** : Code compatible souris ET tactile automatiquement.

## 3. Les gestes prédéfinis

### Qu'est-ce qu'un geste ?

Un **geste** est une séquence de mouvements tactiles qui forme un pattern reconnaissable. FireMonkey reconnaît automatiquement plusieurs gestes standards.

### Gestes standards disponibles

**Gestes de base** :

**igiLeft** - Balayage vers la gauche (Swipe Left)
```pascal
// Utilisateur fait glisser son doigt de droite à gauche
// Typique pour : page suivante, supprimer un élément
```

**igiRight** - Balayage vers la droite (Swipe Right)
```pascal
// Utilisateur fait glisser son doigt de gauche à droite
// Typique pour : page précédente, annuler
```

**igiUp** - Balayage vers le haut (Swipe Up)
```pascal
// Utilisateur fait glisser son doigt de bas en haut
// Typique pour : faire défiler, révéler plus d'infos
```

**igiDown** - Balayage vers le bas (Swipe Down)
```pascal
// Utilisateur fait glisser son doigt de haut en bas
// Typique pour : actualiser, faire défiler vers le bas
```

**Gestes multi-touch** :

**igiZoom** - Pincement (Zoom in/out)
```pascal
// Deux doigts qui s'écartent (zoom in) ou se rapprochent (zoom out)
// Typique pour : zoomer dans une image, une carte
```

**igiRotate** - Rotation
```pascal
// Deux doigts qui tournent autour d'un point central
// Typique pour : faire pivoter une image, un objet
```

**Gestes avancés** :

**igiLongTap** - Appui long
```pascal
// Doigt maintenu sur un point pendant environ 1 seconde
// Typique pour : menu contextuel, sélection multiple
```

**igiDoubleTap** - Double tap
```pascal
// Deux taps rapides au même endroit
// Typique pour : zoomer rapidement, éditer
```

### Activer les gestes sur un composant

Pour qu'un composant reconnaisse les gestes, vous devez :

**Étape 1** : Activer Touch dans les options
```pascal
// En code
Image1.Touch.GestureManager := GestureManager1;  
Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom,  
                                     TInteractiveGesture.Pan,
                                     TInteractiveGesture.Rotate];

// Ou dans l'inspecteur d'objets :
// Touch → InteractiveGestures → Cocher les gestes souhaités
```

**Étape 2** : Gérer l'événement OnGesture
```pascal
procedure TForm1.Image1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiZoom:
    begin
      // Zoom détecté
      AjusterZoom(EventInfo);
      Handled := True;
    end;

    igiRotate:
    begin
      // Rotation détectée
      FaireRotation(EventInfo);
      Handled := True;
    end;
  end;
end;
```

## 4. Le composant TGestureManager

### Qu'est-ce que TGestureManager ?

`TGestureManager` est un composant qui centralise la gestion des gestes pour votre application. Il reconnaît les patterns tactiles et les transforme en événements gestes.

### Utilisation basique

**Étape 1** : Ajouter un TGestureManager
```pascal
// Glisser-déposer un TGestureManager sur le formulaire
// Il apparaît dans la zone des composants non visuels
```

**Étape 2** : Configurer les gestes reconnus
```pascal
// Dans l'inspecteur d'objets du GestureManager
// Ou en code :
GestureManager1.StandardGestures := [sgLeft, sgRight, sgUp, sgDown];
```

**Étape 3** : Lier les composants
```pascal
// Chaque composant utilise le GestureManager
Image1.Touch.GestureManager := GestureManager1;  
Panel1.Touch.GestureManager := GestureManager1;  
```

### Gestes standards (Standard Gestures)

Les gestes standards sont les patterns prédéfinis :

```pascal
type
  TStandardGesture = (
    sgLeft,      // Balayage gauche
    sgRight,     // Balayage droite
    sgUp,        // Balayage haut
    sgDown       // Balayage bas
  );

// Activer plusieurs gestes
GestureManager1.StandardGestures := [sgLeft, sgRight];
```

### Exemple complet : Galerie d'images avec gestes

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Configuration du GestureManager
  GestureManager1.StandardGestures := [sgLeft, sgRight];

  // Configuration de l'image
  ImageViewer.Touch.GestureManager := GestureManager1;
  ImageViewer.Touch.InteractiveGestures := [TInteractiveGesture.Zoom,
                                            TInteractiveGesture.Pan];

  // Charger la première image
  FIndexImage := 0;
  AfficherImage(FIndexImage);
end;

procedure TForm1.ImageViewerGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiLeft then
  begin
    // Swipe gauche : image suivante
    ImageSuivante;
    Handled := True;
  end
  else if EventInfo.GestureID = igiRight then
  begin
    // Swipe droite : image précédente
    ImagePrecedente;
    Handled := True;
  end
  else if EventInfo.GestureID = igiZoom then
  begin
    // Pincement : zoomer
    ZoomerImage(EventInfo.Distance);
    Handled := True;
  end;
end;

procedure TForm1.ImageSuivante;  
begin  
  Inc(FIndexImage);
  if FIndexImage >= FListeImages.Count then
    FIndexImage := 0;

  AfficherImage(FIndexImage);
end;

procedure TForm1.ImagePrecedente;  
begin  
  Dec(FIndexImage);
  if FIndexImage < 0 then
    FIndexImage := FListeImages.Count - 1;

  AfficherImage(FIndexImage);
end;

procedure TForm1.ZoomerImage(Distance: Single);  
var  
  FacteurZoom: Single;
begin
  // Distance positif = écarter les doigts (zoom in)
  // Distance négatif = rapprocher les doigts (zoom out)
  FacteurZoom := 1 + (Distance / 1000);

  ImageViewer.Scale.X := ImageViewer.Scale.X * FacteurZoom;
  ImageViewer.Scale.Y := ImageViewer.Scale.Y * FacteurZoom;

  // Limiter le zoom
  if ImageViewer.Scale.X < 0.5 then
  begin
    ImageViewer.Scale.X := 0.5;
    ImageViewer.Scale.Y := 0.5;
  end
  else if ImageViewer.Scale.X > 3.0 then
  begin
    ImageViewer.Scale.X := 3.0;
    ImageViewer.Scale.Y := 3.0;
  end;
end;
```

## 5. Gestes interactifs (Interactive Gestures)

### Différence avec les Standard Gestures

**Standard Gestures** :
- Détectés à la fin du mouvement
- Actions discrètes (swipe détecté une fois)
- Événement OnGesture avec GestureID

**Interactive Gestures** :
- Détectés en continu pendant le mouvement
- Actions progressives (zoom qui change pendant le pincement)
- Événement OnGesture avec informations en temps réel

### Types de gestes interactifs

```pascal
type
  TInteractiveGesture = (
    Zoom,          // Pincement (deux doigts)
    Pan,           // Déplacement (un ou deux doigts)
    Rotate,        // Rotation (deux doigts)
    TwoFingerTap,  // Tap avec deux doigts
    PressAndTap,   // Maintenir un doigt et taper avec un autre
    LongTap,       // Appui long
    DoubleTap      // Double tap
  );
```

### Implémenter le zoom interactif

```pascal
procedure TForm1.ImageZoomGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Zoom interactif : l'image change de taille pendant le geste
  if EventInfo.GestureID = igiZoom then
  begin
    // Vérifier les phases du geste
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin
      // Début du geste : sauvegarder l'échelle initiale
      FEchelleInitiale := Image1.Scale.X;
    end
    else if (TInteractiveGestureFlag.gfInertia in EventInfo.Flags) or
            (TInteractiveGestureFlag.gfEnd in EventInfo.Flags) then
    begin
      // Fin du geste ou inertie
      // Rien de spécial à faire
    end
    else
    begin
      // Pendant le geste : ajuster l'échelle en temps réel
      Image1.Scale.X := FEchelleInitiale * EventInfo.Distance / 100;
      Image1.Scale.Y := FEchelleInitiale * EventInfo.Distance / 100;
    end;

    Handled := True;
  end;
end;
```

### Implémenter la rotation interactive

```pascal
procedure TForm1.ImageRotateGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiRotate then
  begin
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin
      // Sauvegarder l'angle initial
      FAngleInitial := Image1.RotationAngle;
    end
    else
    begin
      // Appliquer la rotation en temps réel
      Image1.RotationAngle := FAngleInitial + RadToDeg(EventInfo.Angle);
    end;

    Handled := True;
  end;
end;
```

### Implémenter le déplacement (Pan)

```pascal
procedure TForm1.ImagePanGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  DeltaX, DeltaY: Single;
begin
  if EventInfo.GestureID = igiPan then
  begin
    // Calculer le déplacement
    DeltaX := EventInfo.Location.X - FDernierPoint.X;
    DeltaY := EventInfo.Location.Y - FDernierPoint.Y;

    // Déplacer l'image
    Image1.Position.X := Image1.Position.X + DeltaX;
    Image1.Position.Y := Image1.Position.Y + DeltaY;

    FDernierPoint := EventInfo.Location;
    Handled := True;
  end;
end;
```

## 6. Zones tactiles et ergonomie

### Taille minimale des zones tactiles

**Règle d'or** : Minimum 44x44 pixels (environ 7mm)

Pourquoi ?
- Un doigt adulte fait environ 8-10mm de diamètre
- La précision tactile est limitée
- Les utilisateurs ont besoin de confort

```pascal
// ❌ MAUVAIS : Bouton trop petit
Button1.Width := 30;  
Button1.Height := 20;  

// ✅ BON : Taille tactile confortable
Button1.Width := 120;  
Button1.Height := 50;  

// Sur mobile spécifiquement
{$IFDEF ANDROID OR IOS}
  Button1.Height := 50;  // Minimum recommandé
{$ELSE}
  Button1.Height := 30;  // Desktop : plus petit acceptable
{$ENDIF}
```

### Espacement entre zones tactiles

**Règle** : Minimum 8 pixels d'espacement entre zones tactiles

```pascal
// Configuration d'une barre de boutons tactiles
ButtonsLayout.ItemSpacing := 12;  // 12 pixels entre chaque bouton  
ButtonsLayout.Padding.Rect := TRectF.Create(10, 10, 10, 10);  
```

### Zones de confort sur mobile

Sur smartphone, certaines zones sont plus faciles à atteindre :

**Zone verte (facile)** :
- Centre de l'écran
- Bas de l'écran (pouces)

**Zone orange (moyenne)** :
- Haut de l'écran
- Bords gauche et droit

**Zone rouge (difficile)** :
- Coins supérieurs
- Tout en haut de l'écran

**Implications** :
```pascal
// Placer les actions principales dans la zone accessible
ButtonConfirmer.Align := TAlignLayout.Bottom;  
ButtonConfirmer.Height := 60;  
ButtonConfirmer.Margins.Bottom := 20;  

// Actions secondaires peuvent être plus haut
ButtonParametres.Align := TAlignLayout.Top;
```

## 7. Feedback visuel et retour tactile

### Importance du feedback

Sans feedback visuel, l'utilisateur ne sait pas si son geste a été détecté. Le feedback doit être :
- **Immédiat** : Moins de 100ms
- **Visible** : Changement clair
- **Approprié** : Correspond à l'action

### Feedback visuel sur tap

```pascal
procedure TForm1.ButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  // Feedback visuel immédiat
  (Sender as TButton).Opacity := 0.7;
end;

procedure TForm1.ButtonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  // Retour à la normale
  (Sender as TButton).Opacity := 1.0;
end;
```

### Animation de feedback

```pascal
procedure TForm1.ButtonTapAnimation(Sender: TObject);  
var  
  ScaleAnim: TFloatAnimation;
begin
  // Créer une animation de "pression"
  ScaleAnim := TFloatAnimation.Create(Sender as TFmxObject);
  ScaleAnim.Parent := Sender as TFmxObject;
  ScaleAnim.PropertyName := 'Scale.X';
  ScaleAnim.StartValue := 1.0;
  ScaleAnim.StopValue := 0.95;
  ScaleAnim.Duration := 0.1;
  ScaleAnim.AutoReverse := True;
  ScaleAnim.Start;

  // Même animation pour Scale.Y
  ScaleAnim := TFloatAnimation.Create(Sender as TFmxObject);
  ScaleAnim.Parent := Sender as TFmxObject;
  ScaleAnim.PropertyName := 'Scale.Y';
  ScaleAnim.StartValue := 1.0;
  ScaleAnim.StopValue := 0.95;
  ScaleAnim.Duration := 0.1;
  ScaleAnim.AutoReverse := True;
  ScaleAnim.Start;
end;
```

### Feedback sur swipe

```pascal
procedure TForm1.ListSwipeIndicateur(Direction: TSwipeDirection);  
var  
  Indicator: TRectangle;
begin
  // Créer un indicateur visuel du swipe
  Indicator := TRectangle.Create(Self);
  Indicator.Parent := Self;
  Indicator.Width := 100;
  Indicator.Height := 50;

  case Direction of
    TSwipeDirection.Left:
    begin
      Indicator.Position.X := Width;
      Indicator.Fill.Color := TAlphaColors.Red;
      // Animer vers la gauche
      AnimerVersGauche(Indicator);
    end;

    TSwipeDirection.Right:
    begin
      Indicator.Position.X := -100;
      Indicator.Fill.Color := TAlphaColors.Green;
      // Animer vers la droite
      AnimerVersDroite(Indicator);
    end;
  end;
end;
```

### Vibration tactile (Haptic Feedback)

Sur appareils mobiles, vous pouvez déclencher une vibration :

```pascal
uses
  FMX.VirtualKeyboard, FMX.Platform;

procedure TForm1.DeclencherVibration;  
var  
  VKService: IFMXVirtualKeyboardService;
begin
  {$IFDEF ANDROID OR IOS}
  if TPlatformServices.Current.SupportsPlatformService(
    IFMXVirtualKeyboardService, IInterface(VKService)) then
  begin
    // Vibration courte
    VKService.HideVirtualKeyboard;
    // Note : API limitée, pour vibration complète utiliser services natifs
  end;
  {$ENDIF}
end;
```

## 8. Gestes en conflit

### Problème des gestes conflictuels

Certains gestes peuvent entrer en conflit :
- Swipe horizontal vs Swipe vertical
- Pan vs Swipe
- Zoom vs Rotation

### Prioriser les gestes

```pascal
procedure TForm1.GererConflitsGestes(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  // Priorité 1 : Zoom
  if EventInfo.GestureID = igiZoom then
  begin
    GererZoom(EventInfo);
    Handled := True;  // Bloquer les autres gestionnaires
    Exit;
  end;

  // Priorité 2 : Rotation
  if EventInfo.GestureID = igiRotate then
  begin
    GererRotation(EventInfo);
    Handled := True;
    Exit;
  end;

  // Priorité 3 : Pan
  if EventInfo.GestureID = igiPan then
  begin
    GererPan(EventInfo);
    Handled := True;
  end;
end;
```

### Désactiver temporairement des gestes

```pascal
procedure TForm1.ActiverModeZoom;  
begin  
  // En mode zoom, désactiver le pan
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom];
end;

procedure TForm1.ActiverModePan;  
begin  
  // En mode navigation, désactiver le zoom
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Pan];
end;

procedure TForm1.ActiverTousLesGestes;  
begin  
  // Réactiver tous les gestes
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom,
                                       TInteractiveGesture.Pan,
                                       TInteractiveGesture.Rotate];
end;
```

### Gestes contextuels

```pascal
procedure TForm1.AjusterGestesSelonContexte;  
begin  
  if ModeEdition then
  begin
    // En édition : permettre rotation et zoom
    Canvas.Touch.InteractiveGestures := [TInteractiveGesture.Zoom,
                                         TInteractiveGesture.Rotate];
  end
  else if ModeNavigation then
  begin
    // En navigation : permettre pan et swipe
    Canvas.Touch.InteractiveGestures := [TInteractiveGesture.Pan];
    Canvas.Touch.GestureManager.StandardGestures := [sgLeft, sgRight];
  end
  else if ModeVisualisation then
  begin
    // En visualisation : tout est permis
    Canvas.Touch.InteractiveGestures := [TInteractiveGesture.Zoom,
                                         TInteractiveGesture.Pan,
                                         TInteractiveGesture.Rotate];
  end;
end;
```

## 9. Multi-touch : plusieurs doigts simultanés

### Détecter plusieurs points de contact

```pascal
procedure TForm1.CanvasTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  I: Integer;
  Point: TPointF;
begin
  // Touches est un tableau de tous les points de contact actifs
  Label1.Text := 'Nombre de doigts : ' + Length(Touches).ToString;

  for I := 0 to Length(Touches) - 1 do
  begin
    Point := Touches[I].Location;

    // Dessiner un cercle à chaque point de contact
    Canvas.Canvas.BeginScene;
    try
      Canvas.Canvas.Fill.Color := TAlphaColors.Blue;
      Canvas.Canvas.FillEllipse(
        RectF(Point.X - 25, Point.Y - 25, Point.X + 25, Point.Y + 25), 0.5);
    finally
      Canvas.Canvas.EndScene;
    end;
  end;
end;
```

### Application : Dessin multi-doigts

```pascal
type
  TDoigt = record
    ID: Integer;
    DernierPoint: TPointF;
    Couleur: TAlphaColor;
  end;

var
  FDoigtsActifs: TArray<TDoigt>;

procedure TForm1.CanvasMultiTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  I, Index: Integer;
  Point: TPointF;
  Doigt: TDoigt;
begin
  case Action of
    TTouchAction.Down:
    begin
      // Nouveau doigt
      for I := 0 to Length(Touches) - 1 do
      begin
        Doigt.ID := I;
        Doigt.DernierPoint := Touches[I].Location;
        Doigt.Couleur := CouleurAleatoire;

        SetLength(FDoigtsActifs, Length(FDoigtsActifs) + 1);
        FDoigtsActifs[Length(FDoigtsActifs) - 1] := Doigt;
      end;
    end;

    TTouchAction.Move:
    begin
      // Dessiner avec chaque doigt
      Canvas.Canvas.BeginScene;
      try
        for I := 0 to Length(Touches) - 1 do
        begin
          Point := Touches[I].Location;

          // Trouver le doigt correspondant
          Index := TrouverDoigt(I);
          if Index >= 0 then
          begin
            Canvas.Canvas.Stroke.Color := FDoigtsActifs[Index].Couleur;
            Canvas.Canvas.DrawLine(
              FDoigtsActifs[Index].DernierPoint, Point, 1.0);
            FDoigtsActifs[Index].DernierPoint := Point;
          end;
        end;
      finally
        Canvas.Canvas.EndScene;
      end;
    end;

    TTouchAction.Up:
    begin
      // Retirer les doigts levés
      SetLength(FDoigtsActifs, 0);
    end;
  end;
end;
```

## 10. Exemples pratiques

### Exemple 1 : Liste avec swipe pour supprimer

```pascal
procedure TForm1.ListBoxItemSwipe(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  Item: TListBoxItem;
begin
  if EventInfo.GestureID = igiLeft then
  begin
    // Swipe gauche : révéler le bouton supprimer
    Item := (Sender as TListBoxItem);
    RevelerBoutonSupprimer(Item);
    Handled := True;
  end
  else if EventInfo.GestureID = igiRight then
  begin
    // Swipe droite : masquer le bouton
    Item := (Sender as TListBoxItem);
    MasquerBoutonSupprimer(Item);
    Handled := True;
  end;
end;

procedure TForm1.RevelerBoutonSupprimer(Item: TListBoxItem);  
var  
  Anim: TFloatAnimation;
begin
  // Animer l'item vers la gauche
  Anim := TFloatAnimation.Create(Item);
  Anim.Parent := Item;
  Anim.PropertyName := 'Position.X';
  Anim.StartValue := 0;
  Anim.StopValue := -80;  // Décalage de 80 pixels
  Anim.Duration := 0.3;
  Anim.Start;

  // Le bouton supprimer en dessous devient visible
end;
```

### Exemple 2 : Zoom et pan sur une carte

```pascal
procedure TForm1.CarteGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiZoom:
    begin
      // Zoom sur la carte
      ZoomerCarte(EventInfo.Distance);
      Handled := True;
    end;

    igiPan:
    begin
      // Déplacer la carte
      DeplacerCarte(EventInfo.Location);
      Handled := True;
    end;

    igiDoubleTap:
    begin
      // Double tap : zoom rapide centré
      ZoomRapide(EventInfo.Location);
      Handled := True;
    end;
  end;
end;

procedure TForm1.ZoomerCarte(Distance: Single);  
var  
  NouveauZoom: Single;
begin
  NouveauZoom := FZoomActuel * (1 + Distance / 200);

  // Limiter le zoom entre 0.5x et 10x
  if (NouveauZoom >= 0.5) and (NouveauZoom <= 10.0) then
  begin
    FZoomActuel := NouveauZoom;
    ImageCarte.Scale.X := FZoomActuel;
    ImageCarte.Scale.Y := FZoomActuel;
  end;
end;

procedure TForm1.ZoomRapide(Centre: TPointF);  
begin  
  // Doubler le zoom avec animation
  TAnimator.AnimateFloat(ImageCarte, 'Scale.X', FZoomActuel * 2, 0.3);
  TAnimator.AnimateFloat(ImageCarte, 'Scale.Y', FZoomActuel * 2, 0.3);
  FZoomActuel := FZoomActuel * 2;
end;
```

### Exemple 3 : Pull-to-refresh (tirer pour actualiser)

```pascal
procedure TForm1.ListViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  DeltaY: Single;
begin
  if FEnTrainDeTirer then
  begin
    DeltaY := Y - FPointDepart.Y;

    if DeltaY > 0 then
    begin
      // Tirer vers le bas
      if DeltaY < 100 then
      begin
        // Zone élastique
        IndicateurActualisation.Height := DeltaY;
        IndicateurActualisation.Opacity := DeltaY / 100;
      end
      else
      begin
        // Seuil atteint : actualiser
        if not FActualisationDeclenchee then
        begin
          FActualisationDeclenchee := True;
          LancerActualisation;
        end;
      end;
    end;
  end;
end;

procedure TForm1.ListViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  // Vérifier si on est en haut de la liste
  if ListView1.ScrollViewPos = 0 then
  begin
    FEnTrainDeTirer := True;
    FPointDepart := PointF(X, Y);
    FActualisationDeclenchee := False;
  end;
end;

procedure TForm1.ListViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FEnTrainDeTirer := False;

  if not FActualisationDeclenchee then
  begin
    // Animation de retour
    TAnimator.AnimateFloat(IndicateurActualisation, 'Height', 0, 0.3);
  end;
end;
```

## 11. Bonnes pratiques

### ✅ À FAIRE

**1. Respecter les tailles minimales**
```pascal
// Zones tactiles : minimum 44x44 pixels
Button1.Width := 120;  
Button1.Height := 50;  
```

**2. Donner un feedback immédiat**
```pascal
procedure ButtonTap(Sender: TObject);  
begin  
  // Feedback visuel instantané
  (Sender as TButton).Opacity := 0.7;
  // Puis action
  ExecuterAction;
  // Puis restaurer
  (Sender as TButton).Opacity := 1.0;
end;
```

**3. Utiliser les gestes standards**
```pascal
// Les utilisateurs connaissent déjà ces gestes
// - Swipe pour naviguer
// - Pinch pour zoomer
// - Long press pour menu contextuel
// - Double tap pour zoom rapide
```

**4. Tester sur appareil réel**
```pascal
// L'émulateur ne remplace pas un test sur appareil physique
// Les gestes ont une sensation différente
```

**5. Gérer le paramètre Handled**
```pascal
procedure OnGesture(...; var Handled: Boolean);  
begin  
  if GestionOK then
    Handled := True;  // Empêche la propagation
end;
```

**6. Prévoir des alternatives**
```pascal
// Ne pas forcer les gestes complexes
// Offrir des boutons pour les mêmes actions
```

### ❌ À ÉVITER

**1. Zones tactiles trop petites**
```pascal
// ❌ MAUVAIS
Button1.Width := 30;  
Button1.Height := 20;  
```

**2. Gestes non-standard sans explication**
```pascal
// ❌ MAUVAIS : Inventer des gestes bizarres
// Les utilisateurs ne les découvriront jamais
```

**3. Pas de feedback visuel**
```pascal
// ❌ MAUVAIS : Bouton qui ne montre pas qu'il est tapé
// L'utilisateur tape plusieurs fois par frustration
```

**4. Conflits de gestes non gérés**
```pascal
// ❌ MAUVAIS : Swipe et Pan en même temps
// Comportement imprévisible
```

**5. Oublier le mode paysage**
```pascal
// ❌ MAUVAIS : Gestes qui ne fonctionnent qu'en portrait
```

**6. Dépendre uniquement des gestes**
```pascal
// ❌ MAUVAIS : Fonctionnalités accessibles uniquement par geste caché
// Toujours offrir une alternative visible (bouton, menu)
```

## 12. Débogage et tests

### Visualiser les touches

Pour déboguer les gestes, affichez les informations tactiles :

```pascal
procedure TForm1.FormTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
var
  I: Integer;
  Info: string;
begin
  Info := 'Action: ';
  case Action of
    TTouchAction.Down: Info := Info + 'Down';
    TTouchAction.Move: Info := Info + 'Move';
    TTouchAction.Up: Info := Info + 'Up';
  end;

  Info := Info + #13#10 + 'Touches: ' + Length(Touches).ToString;

  for I := 0 to Length(Touches) - 1 do
  begin
    Info := Info + #13#10 + Format('  [%d] X:%.0f Y:%.0f',
      [I, Touches[I].Location.X, Touches[I].Location.Y]);
  end;

  MemoDebug.Text := Info;
end;
```

### Logger les gestes détectés

```pascal
procedure TForm1.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  GestureNom: string;
begin
  case EventInfo.GestureID of
    igiLeft: GestureNom := 'Swipe Left';
    igiRight: GestureNom := 'Swipe Right';
    igiUp: GestureNom := 'Swipe Up';
    igiDown: GestureNom := 'Swipe Down';
    igiZoom: GestureNom := 'Zoom';
    igiPan: GestureNom := 'Pan';
    igiRotate: GestureNom := 'Rotate';
    igiDoubleTap: GestureNom := 'Double Tap';
    igiLongTap: GestureNom := 'Long Tap';
  else
    GestureNom := 'Unknown';
  end;

  MemoLog.Lines.Add(FormatDateTime('hh:nn:ss', Now) + ' - ' + GestureNom);
end;
```

### Tester avec la souris sur desktop

Sur desktop, la souris simule le tactile :
- Clic = Tap
- Clic + glisser = Swipe/Pan
- Ctrl + Molette = Zoom (dans certains cas)

Mais testez toujours sur appareil réel pour l'expérience finale.

## Conclusion

La gestion du tactile et des gestes est essentielle pour créer des applications mobiles et tactiles intuitives. Les points clés à retenir :

👆 **Zones tactiles** : Minimum 44x44 pixels avec espacement suffisant

👆 **Feedback** : Toujours donner un retour visuel immédiat

👆 **Gestes standards** : Utiliser les gestes que les utilisateurs connaissent

👆 **TGestureManager** : Composant central pour la reconnaissance de gestes

👆 **Gestes interactifs** : Pour les actions progressives (zoom, rotation)

👆 **Multi-touch** : Support de plusieurs doigts simultanés

👆 **Tests** : Toujours tester sur appareil réel

👆 **Alternatives** : Offrir des boutons en plus des gestes

Avec ces techniques, vos applications offriront une expérience tactile fluide et naturelle, que ce soit sur smartphone, tablette, ou même sur des écrans tactiles desktop. Dans la section suivante, nous verrons comment cibler spécifiquement chaque plateforme et gérer leurs particularités.

⏭️ [Ciblage des plateformes : Windows, macOS, iOS, Android, Linux](/05-developpement-multi-plateforme-avec-firemonkey/07-ciblage-des-plateformes.md)
