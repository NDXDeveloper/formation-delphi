🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 15.2 Interface utilisateur tactile

## Introduction

L'interface tactile a révolutionné la façon dont les utilisateurs interagissent avec les applications. Contrairement à une souris qui permet une précision au pixel près, un doigt est moins précis mais offre des interactions plus naturelles et intuitives. Concevoir une interface utilisateur tactile efficace nécessite de comprendre ces différences et d'adapter votre approche de conception en conséquence.

Dans cette section, nous allons explorer comment créer des interfaces utilisateur adaptées au tactile avec Delphi et FireMonkey, en respectant les bonnes pratiques et en offrant une expérience utilisateur optimale.

## Différences entre interaction souris et tactile

### Précision et taille des cibles

**Avec une souris**, le curseur est représenté par un point précis à l'écran. L'utilisateur peut cliquer sur de très petits éléments (quelques pixels) sans difficulté.

**Avec le doigt**, la zone de contact est beaucoup plus large (environ 8-10mm de diamètre). De plus, le doigt cache partiellement l'élément sur lequel l'utilisateur appuie, ce qui rend l'interaction moins précise.

**Règle d'or** : Les éléments interactifs (boutons, liens, contrôles) doivent mesurer au minimum **44x44 points** (environ 7-9mm), et idéalement **48x48 points** pour être confortables à utiliser.

```pascal
// Dans Delphi, définir une taille minimale pour un bouton
procedure TFormMain.ConfigurerBouton(Bouton: TButton);  
begin  
  Bouton.Width := 48;  // En points (pixels indépendants)
  Bouton.Height := 48;
  Bouton.Text := 'OK';
end;
```

### Espacement entre les éléments

Sur mobile, il est crucial de laisser suffisamment d'espace entre les éléments interactifs pour éviter les clics accidentels. Un espacement de **8 à 16 points** minimum entre les boutons est recommandé.

```pascal
// Positionner des boutons avec un espacement approprié
procedure TFormMain.CreerBoutonsEspaces;  
begin  
  Bouton1.Position.Y := 100;
  Bouton2.Position.Y := Bouton1.Position.Y + Bouton1.Height + 12; // Espacement de 12 points
  Bouton3.Position.Y := Bouton2.Position.Y + Bouton2.Height + 12;
end;
```

### Feedback visuel immédiat

L'utilisateur doit recevoir un retour visuel instantané lorsqu'il touche un élément. Sans le survol de la souris, ce feedback est encore plus important sur mobile.

```pascal
// Changer l'apparence d'un bouton lors de l'appui
procedure TFormMain.BoutonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TButton(Sender).Opacity := 0.7; // Réduire l'opacité
end;

procedure TFormMain.BoutonMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  TButton(Sender).Opacity := 1.0; // Restaurer l'opacité
end;
```

## Gestes tactiles fondamentaux

Les utilisateurs mobiles s'attendent à pouvoir utiliser des gestes naturels dans vos applications.

### Le Tap (Toucher simple)

C'est l'équivalent du clic de souris. L'utilisateur touche brièvement un élément pour l'activer.

```pascal
// Gérer un simple toucher
procedure TFormMain.ImageTap(Sender: TObject; const Point: TPointF);  
begin  
  ShowMessage('Image touchée à la position: ' +
    Point.X.ToString + ', ' + Point.Y.ToString);
end;
```

### Le Long Press (Appui long)

L'utilisateur maintient son doigt sur un élément pendant une seconde ou plus. Cela sert souvent à afficher un menu contextuel ou des options supplémentaires.

```pascal
uses
  FMX.Gestures;

// Configurer la détection d'appui long
procedure TFormMain.FormCreate(Sender: TObject);  
begin  
  Image1.Touch.GestureManager := GestureManager1;
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.LongTap];
end;

// Gérer l'appui long
procedure TFormMain.Image1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  if EventInfo.GestureID = igiLongTap then
  begin
    ShowMessage('Appui long détecté !');
    Handled := True;
  end;
end;
```

### Le Swipe (Glissement)

L'utilisateur glisse rapidement son doigt dans une direction (gauche, droite, haut, bas). Ce geste est couramment utilisé pour naviguer entre des écrans ou supprimer des éléments.

```pascal
// Activer la détection de swipe
procedure TFormMain.ConfigurerSwipe;  
begin  
  Panel1.Touch.GestureManager := GestureManager1;
  Panel1.Touch.InteractiveGestures := [TInteractiveGesture.Left,
                                        TInteractiveGesture.Right];
end;

// Gérer le swipe
procedure TFormMain.Panel1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    igiLeft:
      begin
        // Swipe vers la gauche - aller à l'écran suivant
        AfficherEcranSuivant;
        Handled := True;
      end;
    igiRight:
      begin
        // Swipe vers la droite - retourner à l'écran précédent
        AfficherEcranPrecedent;
        Handled := True;
      end;
  end;
end;
```

### Le Pinch (Pincement)

L'utilisateur utilise deux doigts pour zoomer (écarter) ou dézoomer (pincer). Ce geste est essentiel pour les images, les cartes et les documents.

```pascal
// Configurer le zoom par pincement
procedure TFormMain.ConfigurerZoom;  
begin  
  Image1.Touch.GestureManager := GestureManager1;
  Image1.Touch.InteractiveGestures := [TInteractiveGesture.Zoom];
end;

// Gérer le zoom
// On stocke la distance précédente entre les deux doigts dans un champ
// privé du formulaire (FDistancePrecedente: Single) pour pouvoir
// calculer un facteur d'échelle relatif à chaque mouvement.
procedure TFormMain.Image1Gesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
var
  Facteur: Single;
begin
  if EventInfo.GestureID = igiZoom then
  begin
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin
      // Début du geste : on mémorise la distance initiale
      FDistancePrecedente := EventInfo.Distance;
    end
    else if FDistancePrecedente > 0 then
    begin
      // Mouvement : on applique un facteur proportionnel
      Facteur := EventInfo.Distance / FDistancePrecedente;
      Image1.Scale.X := Image1.Scale.X * Facteur;
      Image1.Scale.Y := Image1.Scale.Y * Facteur;
      FDistancePrecedente := EventInfo.Distance;
    end;
    Handled := True;
  end;
end;
```

### Le Scroll (Défilement)

L'utilisateur fait défiler le contenu en glissant son doigt verticalement ou horizontalement. FireMonkey gère automatiquement le scroll avec les composants `TVertScrollBox` et `THorzScrollBox`.

```pascal
// Créer une zone de défilement vertical
procedure TFormMain.CreerZoneDefilement;  
var  
  ScrollBox: TVertScrollBox;
  i: Integer;
begin
  ScrollBox := TVertScrollBox.Create(Self);
  ScrollBox.Parent := Self;
  ScrollBox.Align := TAlignLayout.Client;

  // Ajouter du contenu qui dépasse la hauteur visible
  for i := 1 to 20 do
  begin
    var Btn := TButton.Create(ScrollBox);
    Btn.Parent := ScrollBox;
    Btn.Text := 'Bouton ' + i.ToString;
    Btn.Position.Y := (i - 1) * 60;
    Btn.Width := 200;
    Btn.Height := 48;
  end;
end;
```

## Adaptation aux différentes tailles d'écran

Les appareils mobiles existent dans une multitude de tailles et de résolutions. Votre interface doit s'adapter élégamment à toutes ces variations.

### Layouts adaptatifs avec Anchors

Les **Anchors** (ancres) permettent à vos contrôles de s'adapter automatiquement lorsque la taille du formulaire change.

```pascal
// Ancrer un bouton en bas à droite
procedure TFormMain.ConfigurerAnchors;  
begin  
  BoutonValider.Anchors := [TAnchorKind.akRight, TAnchorKind.akBottom];
  // Le bouton restera toujours en bas à droite, quelle que soit la taille de l'écran
end;
```

### Alignement automatique

La propriété `Align` est votre meilleure amie pour créer des layouts qui s'adaptent automatiquement.

```pascal
// Créer une interface avec en-tête, contenu et pied de page
procedure TFormMain.CreerLayoutAdaptatif;  
begin  
  // En-tête fixe en haut
  PanelEntete.Align := TAlignLayout.Top;
  PanelEntete.Height := 60;

  // Pied de page fixe en bas
  PanelPied.Align := TAlignLayout.Bottom;
  PanelPied.Height := 60;

  // Le contenu occupe tout l'espace restant
  PanelContenu.Align := TAlignLayout.Client;
end;
```

### Layouts pour différentes orientations

Votre application doit gérer à la fois le mode portrait (vertical) et paysage (horizontal).

> 💡 `TLayout` est un simple conteneur, il n'a **pas** de propriété `Orientation`. Pour changer la disposition selon l'orientation, on rejoue les propriétés `Align`, `Size` et `Margins` des contrôles enfants — ou on utilise un conteneur orientable comme `TFlowLayout` (avec `Justify` / `JustifyTrailing`) ou `TGridLayout` (avec `ColumnCount`).

```pascal
uses
  FMX.Types;

// Détecter le changement d'orientation
procedure TFormMain.FormResize(Sender: TObject);  
begin  
  if Width > Height then
    AdapterInterfacePaysage
  else
    AdapterInterfacePortrait;
end;

procedure TFormMain.AdapterInterfacePortrait;  
begin  
  // En portrait : image en haut, texte juste en dessous
  Image1.Align := TAlignLayout.Top;
  Image1.Height := 200;
  Image1.Margins.Rect := RectF(10, 10, 10, 10);

  MemoDescription.Align := TAlignLayout.Client;
end;

procedure TFormMain.AdapterInterfacePaysage;  
begin  
  // En paysage : image à gauche, texte à droite (côte à côte)
  Image1.Align := TAlignLayout.Left;
  Image1.Width  := ClientWidth / 2;
  Image1.Height := 0;  // ignoré quand Align = Left
  Image1.Margins.Rect := RectF(10, 10, 10, 10);

  MemoDescription.Align := TAlignLayout.Client;
end;
```

> 💡 Sur les vrais appareils, l'orientation est aussi accessible via `Screen.Orientation` (énumération `TScreenOrientation`). `FormResize` reste le déclencheur le plus simple et fonctionne dès qu'on tourne l'écran.

### Responsive Design avec TLayout

Le composant `TLayout` est un conteneur invisible qui aide à organiser vos contrôles de manière flexible.

```pascal
// Créer une grille responsive de cartes
procedure TFormMain.CreerGrilleResponsive;  
var  
  Layout: TLayout;
  i, ColCount: Integer;
begin
  // Déterminer le nombre de colonnes selon la largeur
  if Width < 600 then
    ColCount := 1  // 1 colonne sur petit écran
  else if Width < 900 then
    ColCount := 2  // 2 colonnes sur écran moyen
  else
    ColCount := 3; // 3 colonnes sur grand écran

  // Créer les cartes avec le bon nombre de colonnes
  for i := 0 to 11 do
  begin
    Layout := TLayout.Create(Self);
    Layout.Parent := FlowLayout1;
    Layout.Width := (ClientWidth / ColCount) - 10;
    Layout.Height := 150;
    // Ajouter le contenu de la carte
  end;
end;
```

## Composants tactiles essentiels de FireMonkey

FireMonkey offre de nombreux composants spécialement conçus pour le tactile.

### TButton - Le bouton standard

Le bouton est l'élément interactif le plus courant. Assurez-vous qu'il soit suffisamment grand et visible.

```pascal
// Créer un bouton tactile optimal
procedure TFormMain.CreerBoutonTactile;  
begin  
  var Btn := TButton.Create(Self);
  Btn.Parent := Self;
  Btn.Width := 150;
  Btn.Height := 48;
  Btn.Text := 'Valider';
  Btn.Position.X := 100;
  Btn.Position.Y := 200;
  Btn.OnClick := BoutonClick;
end;
```

### TSpeedButton - Bouton avec icône

Idéal pour les barres d'outils tactiles.

```pascal
// Créer une barre d'outils tactile
procedure TFormMain.CreerBarreOutils;  
var  
  BtnNouveau, BtnEditer, BtnSupprimer: TSpeedButton;
begin
  // Bouton Nouveau
  BtnNouveau := TSpeedButton.Create(Self);
  BtnNouveau.Parent := ToolBar1;
  BtnNouveau.Width := 48;
  BtnNouveau.Height := 48;
  BtnNouveau.StyleLookup := 'addtoolbutton'; // Style avec icône +

  // Bouton Éditer
  BtnEditer := TSpeedButton.Create(Self);
  BtnEditer.Parent := ToolBar1;
  BtnEditer.Width := 48;
  BtnEditer.Height := 48;
  BtnEditer.StyleLookup := 'composetoolbutton'; // Style avec icône édition
end;
```

### TSwitch - Interrupteur

Parfait pour les options on/off, plus tactile qu'une case à cocher.

```pascal
// Utiliser un switch pour une option
procedure TFormMain.CreerSwitch;  
begin  
  var Switch := TSwitch.Create(Self);
  Switch.Parent := Self;
  Switch.Position.X := 100;
  Switch.Position.Y := 150;
  Switch.OnSwitch := SwitchChange;
end;

procedure TFormMain.SwitchChange(Sender: TObject);  
begin  
  if TSwitch(Sender).IsChecked then
    ShowMessage('Option activée')
  else
    ShowMessage('Option désactivée');
end;
```

### TListView - Liste tactile

Pour afficher des listes d'éléments avec des interactions tactiles (swipe pour supprimer, etc.). FireMonkey propose deux mécanismes complémentaires :

**1. Activer le mode édition standard (le plus simple).** Quand `EditMode = True`, FMX affiche automatiquement un bouton « rouge » de suppression sur chaque ligne et déclenche `OnDeletingItem` / `OnDeleteItem` à la confirmation. C'est l'approche recommandée pour 90 % des cas.

```pascal
procedure TFormMain.CreerListeTactile;  
var  
  Item: TListViewItem;
  i: Integer;
begin
  ListView1.ItemAppearance.ItemAppearance := 'ListItemRightDetail';
  ListView1.OnDeletingItem := ListView1DeletingItem;

  // Ajouter quelques éléments
  for i := 1 to 5 do
  begin
    Item := ListView1.Items.Add;
    Item.Text   := 'Élément ' + i.ToString;
    Item.Detail := 'Description ' + i.ToString;
  end;
end;

procedure TFormMain.BtnModeEditionClick(Sender: TObject);  
begin  
  // Active/désactive l'apparition du bouton « rouge » de suppression
  ListView1.EditMode := not ListView1.EditMode;
end;

procedure TFormMain.ListView1DeletingItem(Sender: TObject; AIndex: Integer;
  var ACanDelete: Boolean);
begin
  // Permet d'annuler la suppression (par exemple après confirmation)
  ACanDelete := True;
end;
```

**2. Ajouter un bouton personnalisé via une apparence dynamique.** Pour un bouton « Supprimer » toujours visible à droite de la ligne, on utilise une apparence comme `ListItemRightDetail` enrichie d'un `TListItemTextButton` ajouté au runtime. **Important** : `TListItemTextButton` est un *drawable* enfant de l'item — on l'instancie via son constructeur en lui passant l'item comme parent, on ne fait jamais un simple `DeleteButton.Text := …` sur une variable non initialisée.

```pascal
uses
  FMX.ListView.Appearances;

procedure TFormMain.AjouterBoutonSupprimerAItem(Item: TListViewItem;
  TagID: Integer);
var
  Btn: TListItemTextButton;
begin
  // Le bouton se rattache aux Objects de l'item via son constructeur
  Btn := TListItemTextButton.Create(Item);
  Btn.Name        := 'btnDelete';
  Btn.Text        := 'Supprimer';
  // Énumération imbriquée — pas TListItemButtonType !
  Btn.ButtonType  := TListItemTextButton.TButtonType.Delete;
  Btn.TagString   := TagID.ToString;
  Btn.PlaceOffset.X := -10;
  Btn.Align         := TListItemAlign.Trailing;
  Btn.TextAlign     := TTextAlign.Trailing;
end;

procedure TFormMain.ListView1ItemClickEx(const Sender: TObject;
  ItemIndex: Integer; const LocalClickPos: TPointF;
  const ItemObject: TListItemDrawable);
begin
  // Détecter le clic sur le bouton personnalisé
  if (ItemObject <> nil) and (ItemObject.Name = 'btnDelete') then
    SupprimerElementParTag(ItemObject.TagString);
end;
```

> 💡 La signature de l'événement de clic « étendu » est bien `OnItemClickEx`, pas `OnClick` sur le bouton lui-même : `TListItemDrawable` n'expose pas d'événement `OnClick` propre.

### TTabControl - Navigation par onglets

Excellent pour organiser différentes sections de votre application.

```pascal
// Créer une navigation par onglets
procedure TFormMain.CreerOnglets;  
var  
  TabItem1, TabItem2: TTabItem;
begin
  TabControl1.Align := TAlignLayout.Client;

  // Premier onglet
  TabItem1 := TTabItem.Create(TabControl1);
  TabItem1.Parent := TabControl1;
  TabItem1.Text := 'Accueil';

  // Deuxième onglet
  TabItem2 := TTabItem.Create(TabControl1);
  TabItem2.Parent := TabControl1;
  TabItem2.Text := 'Paramètres';
end;
```

## Principes de conception tactile

### Hiérarchie visuelle claire

Sur un petit écran, l'espace est précieux. Établissez une hiérarchie claire avec :
- Des titres plus grands et en gras
- Des espacements généreux entre les sections
- Des groupements logiques d'informations

### Zones de confort du pouce

Sur mobile, les utilisateurs tiennent souvent leur téléphone d'une main et interagissent avec le pouce. Les zones faciles à atteindre se situent :
- **Facile** : Bas et milieu de l'écran
- **Difficile** : Haut et coins opposés

Placez les actions principales dans les zones faciles d'accès.

```pascal
// Placer un bouton d'action principal en bas
procedure TFormMain.PlacerBoutonPrincipal;  
begin  
  BoutonPrincipal.Align := TAlignLayout.Bottom;
  BoutonPrincipal.Height := 60;
  BoutonPrincipal.Margins.Rect := RectF(10, 10, 10, 10);
end;
```

### Contrastes et lisibilité

Les écrans mobiles sont souvent utilisés en extérieur avec beaucoup de lumière. Assurez-vous d'avoir :
- Un contraste élevé entre le texte et le fond
- Une taille de police suffisante (minimum 14-16 points pour le corps de texte)
- Des couleurs distinctes pour les éléments interactifs

```pascal
// Configurer un style avec bon contraste
procedure TFormMain.ConfigurerStyle;  
begin  
  Label1.TextSettings.Font.Size := 16;
  Label1.TextSettings.FontColor := TAlphaColors.Black;
  Label1.Fill.Color := TAlphaColors.White;
end;
```

### Minimalisme et focus

Ne surchargez pas l'écran. Chaque écran devrait avoir :
- Un objectif principal clair
- Peu d'options (5-7 maximum)
- Des espaces blancs pour respirer

## Animations et transitions

Les animations rendent l'interface plus fluide et aident l'utilisateur à comprendre les changements d'état.

### Animation de transition entre écrans

```pascal
uses
  FMX.Ani;

// Faire glisser un nouvel écran depuis la droite
procedure TFormMain.AnimerTransition;  
begin  
  // Positionner le nouvel écran hors de vue à droite
  NouvelEcran.Position.X := ClientWidth;
  NouvelEcran.Visible := True;

  // Animer le glissement
  TAnimator.AnimateFloat(NouvelEcran, 'Position.X', 0, 0.3,
    TAnimationType.In, TInterpolationType.Cubic);
end;
```

### Animation de bouton au clic

```pascal
// Créer un effet "rebond" lors du clic
procedure TFormMain.AnimerBoutonClic(Sender: TObject);  
begin  
  // Réduire légèrement
  TAnimator.AnimateFloat(Sender, 'Scale.X', 0.95, 0.1);
  TAnimator.AnimateFloat(Sender, 'Scale.Y', 0.95, 0.1);

  // Puis revenir à la normale
  TAnimator.AnimateFloatDelay(Sender, 'Scale.X', 1.0, 0.1, 0.1);
  TAnimator.AnimateFloatDelay(Sender, 'Scale.Y', 1.0, 0.1, 0.1);
end;
```

### Animation de feedback visuel

```pascal
// Animer un changement de couleur pour indiquer une action
procedure TFormMain.AnimerFeedback;  
begin  
  Rectangle1.Fill.Color := TAlphaColors.Green;

  // Revenir à la couleur normale après 0.5 secondes
  TAnimator.AnimateColorDelay(Rectangle1, 'Fill.Color',
    TAlphaColors.Lightgray, 0.3, 0.5);
end;
```

## Clavier virtuel et saisie de texte

Sur mobile, le clavier occupe une grande partie de l'écran lorsqu'il apparaît.

### Gérer l'apparition du clavier

L'événement `OnVirtualKeyboardShown` est déclenché à chaque apparition/disparition du clavier virtuel. `Bounds` donne le rectangle (en pixels écran) occupé par le clavier — il faut convertir `Bounds.Top` en coordonnées formulaire avec `ScreenToClient` pour calculer la hauteur disponible restante :

```pascal
procedure TFormMain.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
var
  ClientTopDuClavier: Single;
begin
  if KeyboardVisible then
  begin
    // Bounds est en coordonnées écran ; on le ramène au formulaire
    ClientTopDuClavier := ScreenToClient(PointF(Bounds.Left, Bounds.Top)).Y;
    // La zone de contenu s'arrête juste au-dessus du clavier
    ScrollBox1.Height := ClientTopDuClavier - ScrollBox1.Position.Y;
  end
  else
  begin
    // Le clavier disparaît : on rétablit la hauteur initiale
    ScrollBox1.Height := ClientHeight - ScrollBox1.Position.Y;
  end;
end;
```

> 💡 Si vous avez un `TVertScrollBox` aligné `Client`, FMX gère partiellement ce décalage tout seul (le champ qui a le focus reste visible). N'intervenez à la main que pour les mises en page non triviales.

### Types de clavier appropriés

Utilisez le bon type de clavier selon le contenu attendu :

```pascal
// Configurer le type de clavier pour chaque champ
procedure TFormMain.ConfigurerClaviers;  
begin  
  // Clavier numérique pour les nombres
  EditAge.KeyboardType := TVirtualKeyboardType.NumberPad;

  // Clavier email pour les adresses email
  EditEmail.KeyboardType := TVirtualKeyboardType.EmailAddress;

  // Clavier téléphone
  EditTelephone.KeyboardType := TVirtualKeyboardType.PhonePad;

  // Clavier URL
  EditSiteWeb.KeyboardType := TVirtualKeyboardType.URL;
end;
```

### Bouton "Suivant" et "Terminé"

```pascal
// Configurer les actions de retour du clavier
procedure TFormMain.ConfigurerRetourClavier;  
begin  
  // "Suivant" pour passer au champ suivant
  EditNom.ReturnKeyType := TReturnKeyType.Next;
  EditNom.OnKeyDown := PasserChampSuivant;

  // "Terminé" pour le dernier champ
  EditEmail.ReturnKeyType := TReturnKeyType.Done;
  EditEmail.OnKeyDown := FermerClavier;
end;

procedure TFormMain.PasserChampSuivant(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkReturn then
  begin
    EditPrenom.SetFocus;
    Key := 0; // Consommer l'événement
  end;
end;
```

## Conclusion

Créer une interface utilisateur tactile efficace demande de repenser complètement votre approche de conception par rapport aux applications desktop traditionnelles. Les principes clés à retenir sont :

1. **Taille des cibles** : Minimum 44x44 points pour tous les éléments interactifs
2. **Espacement** : Laisser suffisamment d'espace entre les contrôles
3. **Feedback visuel** : Toujours indiquer clairement les interactions
4. **Gestes naturels** : Supporter tap, swipe, pinch selon le contexte
5. **Adaptation** : Votre interface doit s'adapter à toutes les tailles et orientations
6. **Simplicité** : Privilégier la clarté et le minimalisme
7. **Performance** : Des animations fluides à 60 FPS

FireMonkey offre tous les outils nécessaires pour créer des interfaces tactiles modernes et réactives. En appliquant ces principes et en testant régulièrement sur de vrais appareils, vous créerez des applications mobiles agréables et intuitives à utiliser.

Dans la section suivante, nous verrons comment exploiter les capteurs des appareils mobiles pour créer des expériences encore plus riches et interactives.

⏭️ [Accès aux capteurs (GPS, accéléromètre...)](/15-applications-mobiles-avec-delphi/03-acces-aux-capteurs.md)
