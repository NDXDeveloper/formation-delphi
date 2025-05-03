# 15.2 Interface utilisateur tactile

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La création d'interfaces tactiles efficaces constitue l'un des défis majeurs du développement mobile. Avec Delphi et FireMonkey (FMX), vous disposez des outils nécessaires pour concevoir des interfaces adaptées au toucher qui offrent une expérience utilisateur intuitive et agréable.

## Principes fondamentaux des interfaces tactiles

### Différences avec les interfaces desktop traditionnelles

Avant de plonger dans les aspects techniques, comprenons les différences essentielles entre les interfaces tactiles et les interfaces traditionnelles contrôlées par souris/clavier :

| Interface desktop | Interface tactile |
|-------------------|-------------------|
| Pointeur précis (souris) | Toucher moins précis (doigt) |
| Survol possible (hover) | Pas d'état de survol |
| Clic droit disponible | Actions secondaires différentes |
| Taille des contrôles variable | Taille minimale nécessaire pour les contrôles |
| Clavier physique | Clavier virtuel occupant l'écran |

### Taille et espacement des contrôles

L'un des principes les plus importants pour les interfaces tactiles est d'offrir des cibles suffisamment grandes :

- **Taille minimale recommandée** : 44×44 points pour tous les éléments interactifs
- **Espacement minimal** : 8-10 points entre les éléments interactifs
- **Zone de toucher** : Peut être plus grande que l'élément visuel (utilisez `HitTest` pour cela)

```pascal
// Exemple : Augmenter la zone de toucher d'un bouton
procedure TMyForm.Button1ApplyStyleLookup(Sender: TObject);
var
  HitTestObj: TControl;
begin
  // Récupérer l'objet de HitTest du bouton
  HitTestObj := TControl(Button1.FindStyleResource('hitTest'));

  if Assigned(HitTestObj) then
  begin
    // Agrandir la zone de toucher de 10 pixels dans chaque direction
    HitTestObj.Margins.Left := -10;
    HitTestObj.Margins.Top := -10;
    HitTestObj.Margins.Right := -10;
    HitTestObj.Margins.Bottom := -10;
  end;
end;
```

### Feedback visuel et haptique

Les utilisateurs s'attendent à une réponse immédiate à leurs interactions :

- **Animation de pression** : Toujours montrer visuellement qu'un élément a été touché
- **Transitions** : Utiliser des animations pour indiquer les changements d'état
- **Retour haptique** : Sur les plateformes qui le supportent, utiliser les vibrations pour confirmer les actions importantes

```pascal
// Exemple : Ajouter un feedback visuel simple à un contrôle personnalisé
procedure TMyTouchControl.DoTap(Sender: TObject; const Point: TPointF);
begin
  // Démarrer une animation d'opacité (flash)
  FadeAnimation.StartValue := 1;
  FadeAnimation.StopValue := 0.7;
  FadeAnimation.Duration := 0.1;
  FadeAnimation.Start;

  // Appeler une vibration (Android)
  {$IFDEF ANDROID}
  CallInUIThread(procedure
  begin
    MainActivity.getSystemService(TJContext.JavaClass.VIBRATOR_SERVICE).vibrate(50);
  end);
  {$ENDIF}

  // Exécuter l'action principale
  if Assigned(FOnTap) then
    FOnTap(Self, Point);
end;
```

## Gestes tactiles dans FireMonkey

FireMonkey inclut un système complet de gestion des gestes tactiles qui vous permet de capturer et de réagir à différentes interactions.

### Gestes standard supportés

- **Tap** (toucher simple)
- **Double Tap** (double toucher)
- **Long Tap** (toucher prolongé)
- **Pan** (glissement)
- **Swipe** (balayage)
- **Pinch** (pincement à deux doigts)
- **Rotation** (rotation à deux doigts)

### Configuration des gestes dans l'IDE

La façon la plus simple d'activer les gestes est d'utiliser l'Inspecteur d'objets :

1. Sélectionnez un contrôle dans le concepteur de formulaire
2. Localisez la section **Touch** dans l'Inspecteur d'objets
3. Cochez les gestes que vous souhaitez activer
4. Configurez les paramètres spécifiques (comme la distance minimale pour un swipe)
5. Implémentez les gestionnaires d'événements correspondants

![Configuration des gestes dans l'IDE](https://i.imgur.com/placeholder.png)

### Programmation des gestes

Voici comment traiter les gestes courants :

```pascal
// Gestionnaire de toucher simple (Tap)
procedure TMyForm.Rectangle1Tap(Sender: TObject; const Point: TPointF);
begin
  ShowMessage('Toucher à la position X: ' + Point.X.ToString + ', Y: ' + Point.Y.ToString);
end;

// Gestionnaire de glissement (Pan)
procedure TMyForm.Rectangle1Pan(Sender: TObject; const Point: TPointF;
  var Handled: Boolean);
begin
  // Déplacer le contrôle avec le doigt
  Rectangle1.Position.X := Rectangle1.Position.X + Point.X;
  Rectangle1.Position.Y := Rectangle1.Position.Y + Point.Y;
  Handled := True; // Marquer l'événement comme traité
end;

// Gestionnaire de pincement (Zoom)
procedure TMyForm.Image1Pinch(Sender: TObject; const Point: TPointF;
  const Distance: Integer; var Handled: Boolean);
begin
  // Changer l'échelle de l'image pendant le pincement
  Image1.Scale.X := Image1.Scale.X * (1 + (Distance / 1000));
  Image1.Scale.Y := Image1.Scale.Y * (1 + (Distance / 1000));
  Handled := True;
end;
```

### Gestes personnalisés

Pour les besoins plus spécifiques, vous pouvez créer des gestes personnalisés :

```pascal
// Exemple simple de détection d'un geste en Z
procedure TMyForm.FormCreate(Sender: TObject);
begin
  // Créer un gestionnaire de gestes personnalisé
  MyGestureManager := TGestureManager.Create(Self);

  // Définir les points du geste en Z
  SetLength(FZGesturePoints, 4);
  FZGesturePoints[0] := TPointF.Create(0, 0);
  FZGesturePoints[1] := TPointF.Create(100, 0);
  FZGesturePoints[2] := TPointF.Create(0, 100);
  FZGesturePoints[3] := TPointF.Create(100, 100);

  // Enregistrer le geste
  FZGestureID := MyGestureManager.AddRecordedGesture('ZGesture', FZGesturePoints);

  // Attacher le gestionnaire au contrôle
  Rectangle1.Touch.GestureManager := MyGestureManager;
  Rectangle1.Touch.StandardGestures := Rectangle1.Touch.StandardGestures + [TStandardGesture.sgCustom];
  Rectangle1.OnGesture := HandleCustomGesture;
end;

procedure TMyForm.HandleCustomGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = FZGestureID then
  begin
    ShowMessage('Geste en Z détecté!');
    Handled := True;
  end;
end;
```

## Composants adaptés aux interfaces tactiles

FireMonkey inclut plusieurs contrôles spécifiquement conçus ou adaptés pour les interfaces tactiles :

### 1. TSpeedButton vs TButton

Les `TSpeedButton` sont généralement préférables aux `TButton` standard pour les interfaces tactiles car :
- Ils sont plus légers visuellement
- Ils s'adaptent mieux aux différents styles mobiles
- Ils offrent un rendu plus proche des conventions des plateformes mobiles

### 2. TListView optimisé pour mobile

Le `TListView` de FireMonkey est spécialement conçu pour les performances mobiles :

```pascal
// Configuration basique d'une ListView mobile
procedure TMyForm.SetupMobileListView;
begin
  ListView1.ItemAppearance.ItemAppearance := 'ImageListItem';
  ListView1.ItemAppearanceObjects.ItemObjects.Image.Visible := True;
  ListView1.SearchVisible := True; // Ajoute une barre de recherche

  // Ajouter quelques éléments de test
  for var i := 1 to 50 do
  begin
    var Item := ListView1.Items.Add;
    Item.Text := 'Item ' + i.ToString;
    Item.Detail := 'Description pour item ' + i.ToString;
    // L'image serait définie ici également
  end;
end;
```

### 3. Actions tactiles avec ActionList

La combinaison de `TActionList` avec des contrôles tactiles permet de centraliser la logique :

```pascal
// Dans FormCreate
procedure TMyForm.FormCreate(Sender: TObject);
begin
  // Configurer l'action
  SaveAction.Caption := 'Enregistrer';
  SaveAction.OnExecute := SaveActionExecute;

  // Assigner l'action à un SpeedButton
  SpeedButton1.Action := SaveAction;

  // Et à un élément de menu
  MenuItem1.Action := SaveAction;
end;

// Logique centralisée
procedure TMyForm.SaveActionExecute(Sender: TObject);
begin
  // Code pour sauvegarder les données
  SaveData;
  ShowMessage('Données enregistrées');
end;
```

## Patron de conception pour interfaces tactiles

### Navigation par glissement (Swipe Navigation)

Un modèle courant dans les applications mobiles est la navigation par glissement :

```pascal
procedure TMainForm.SetupSwipeNavigation;
begin
  // Activer le geste de balayage horizontal
  Layout1.Touch.StandardGestures := Layout1.Touch.StandardGestures + [TStandardGesture.sgLeft, TStandardGesture.sgRight];
  Layout1.OnGesture := HandleNavigationGesture;
end;

procedure TMainForm.HandleNavigationGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        // Navigation vers la page suivante
        TabControl1.GotoNextTab;
        Handled := True;
      end;
    sgiRight:
      begin
        // Navigation vers la page précédente
        TabControl1.GotoPriorTab;
        Handled := True;
      end;
  end;
end;
```

### Menu "hamburger" et tiroirs de navigation

Le menu hamburger est un modèle d'interface tactile extrêmement répandu :

```pascal
// Exemple simplifié de menu hamburger
procedure TMainForm.SetupHamburgerMenu;
begin
  // Configurer le panneau de menu pour qu'il soit initialement hors écran
  MenuPanel.Position.X := -MenuPanel.Width;
  MenuPanel.Visible := True;

  // Gestionnaire pour le bouton hamburger
  HamburgerButton.OnClick := ToggleMenu;

  // Gestionnaire pour fermer le menu en touchant en dehors
  OverlayRectangle.OnClick := CloseMenu;
  OverlayRectangle.Visible := False;
end;

procedure TMainForm.ToggleMenu(Sender: TObject);
begin
  if MenuPanel.Position.X < 0 then
    OpenMenu
  else
    CloseMenu(nil);
end;

procedure TMainForm.OpenMenu;
begin
  // Afficher l'overlay semi-transparent
  OverlayRectangle.Opacity := 0;
  OverlayRectangle.Visible := True;

  // Animations
  MenuAnimation.StartValue := -MenuPanel.Width;
  MenuAnimation.StopValue := 0;
  OverlayAnimation.StartValue := 0;
  OverlayAnimation.StopValue := 0.5;

  MenuAnimation.Start;
  OverlayAnimation.Start;
end;

procedure TMainForm.CloseMenu(Sender: TObject);
begin
  // Animations inverses
  MenuAnimation.StartValue := 0;
  MenuAnimation.StopValue := -MenuPanel.Width;
  OverlayAnimation.StartValue := 0.5;
  OverlayAnimation.StopValue := 0;

  MenuAnimation.Start;
  OverlayAnimation.Start;

  // Cacher l'overlay quand l'animation est terminée
  OverlayAnimation.OnFinish := procedure(Sender: TObject)
  begin
    OverlayRectangle.Visible := False;
  end;
end;
```

## Adaptation aux différentes orientations

Les applications mobiles doivent souvent prendre en charge à la fois les orientations portrait et paysage :

```pascal
// Réagir aux changements d'orientation
procedure TMyForm.FormResize(Sender: TObject);
begin
  // Déterminer l'orientation actuelle
  FIsPortrait := Height > Width;

  if FIsPortrait then
    AdaptLayoutToPortrait
  else
    AdaptLayoutToLandscape;
end;

procedure TMyForm.AdaptLayoutToPortrait;
begin
  // Réorganiser les contrôles pour l'orientation portrait
  Panel1.Align := TAlignLayout.Top;
  Panel1.Height := 120;
  Panel2.Align := TAlignLayout.Client;
  // etc.
end;

procedure TMyForm.AdaptLayoutToLandscape;
begin
  // Réorganiser les contrôles pour l'orientation paysage
  Panel1.Align := TAlignLayout.Left;
  Panel1.Width := 200;
  Panel2.Align := TAlignLayout.Client;
  // etc.
end;
```

## Test et optimisation des interfaces tactiles

### Conseils pour les tests

1. **Testez sur de vrais appareils** - Les émulateurs ne reproduisent pas parfaitement l'expérience tactile
2. **Testez avec différentes tailles de doigts** - Les utilisateurs ont des doigts de différentes tailles
3. **Testez avec une main** - De nombreux utilisateurs tiennent leur téléphone et l'utilisent d'une seule main
4. **Vérifiez la lisibilité en extérieur** - Les écrans sont souvent difficiles à lire en plein soleil

### Optimisation des performances

Les interfaces tactiles doivent être particulièrement fluides pour offrir une bonne expérience :

1. Réduisez le nombre de contrôles visibles simultanément
2. Utilisez `TListView` au lieu de `TScrollBox` avec de nombreux contrôles
3. Activez le double buffering pour éviter les scintillements
4. Utilisez des images de taille optimisée

```pascal
// Exemple : Optimisation d'une liste avec virtualisation
procedure TMyForm.SetupOptimizedList;
begin
  // Configuration d'un ListView virtuel (données chargées à la demande)
  ListView1.BeginUpdate;
  try
    ListView1.ItemCount := FTotalItemCount; // Peut être des milliers
    ListView1.OnItemAppearance := OnListItemAppearance;
  finally
    ListView1.EndUpdate;
  end;
end;

procedure TMyForm.OnListItemAppearance(const Sender: TObject; const AItem: TListViewItem);
begin
  // Cet événement est appelé uniquement pour les éléments visibles
  // Charger les données uniquement pour les éléments visibles
  AItem.Text := GetItemText(AItem.Index);
  AItem.Detail := GetItemDetail(AItem.Index);

  // Charger l'image en arrière-plan si nécessaire
  TThread.CreateAnonymousThread(procedure
  begin
    var Bitmap := LoadImageForItem(AItem.Index);
    TThread.Synchronize(nil, procedure
    begin
      if ListView1.Items[AItem.Index] = AItem then // Vérifier si l'élément est encore visible
        AItem.Bitmap := Bitmap;
    end);
  end).Start;
end;
```

## Directives spécifiques aux plateformes

### iOS

- Suivez les [Human Interface Guidelines d'Apple](https://developer.apple.com/design/human-interface-guidelines/)
- Utilisez les gestes standard d'iOS (ex: retour par balayage de gauche à droite)
- Respectez les zones de sécurité (safe areas) pour éviter les encoches et les coins arrondis

### Android

- Suivez les [Material Design Guidelines de Google](https://material.io/design)
- Utilisez le bouton de retour physique/virtuel d'Android
- Supportez les différentes densités d'écran avec des ressources adaptées

```pascal
// Exemple : Adapter le code selon la plateforme
procedure TMyForm.SetupBackNavigation;
begin
  {$IFDEF IOS}
  // Pour iOS, configurer le geste de balayage
  EnableBackSwipeGesture;
  {$ENDIF}

  {$IFDEF ANDROID}
  // Pour Android, intercepter le bouton retour physique
  FKeyboardService := TPlatformServices.Current.GetPlatformService(IFMXKeyboardService) as IFMXKeyboardService;
  FKeyboardService.SetKeyboardToolbarMode(TKeyboardToolbarMode.ToolbarMode);
  FKeyboardService.SetKeyboardServiceMode(TKeyboardServiceMode.Manual);
  KeyboardShownHandler := TMessageManager.DefaultManager.SubscribeToMessage(TFormKeyDown, HandleKeyDown);
  {$ENDIF}
end;

{$IFDEF ANDROID}
procedure TMyForm.HandleKeyDown(const Sender: TObject; const M: TMessage);
begin
  if (M is TFormKeyDown) and (TFormKeyDown(M).Key = vkHardwareBack) then
  begin
    TFormKeyDown(M).Key := 0; // Consommer l'événement
    DoBackNavigation;
  end;
end;
{$ENDIF}
```

## Conclusion

La conception d'interfaces tactiles efficaces avec Delphi nécessite une compréhension approfondie des principes d'interaction mobile et des outils disponibles dans FireMonkey. En suivant les directives présentées dans ce chapitre, vous pourrez créer des applications mobiles intuitives et agréables à utiliser.

Rappelez-vous que la meilleure façon d'améliorer vos compétences en conception d'interfaces tactiles est de tester régulièrement sur de vrais appareils et de collecter les retours des utilisateurs.

Dans le prochain chapitre, nous explorerons comment accéder aux capteurs spécifiques des appareils mobiles pour enrichir vos applications avec des fonctionnalités comme la géolocalisation et l'accéléromètre.

⏭️ [Accès aux capteurs (GPS, accéléromètre...)](/15-applications-mobiles-avec-delphi/03-acces-aux-capteurs.md)
