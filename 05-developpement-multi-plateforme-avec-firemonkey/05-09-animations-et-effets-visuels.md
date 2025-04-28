# 5.9 Animations et effets visuels

Les animations et effets visuels sont des éléments essentiels pour créer des interfaces utilisateur modernes et attrayantes. FireMonkey offre un système d'animation puissant et flexible qui vous permet d'ajouter facilement du mouvement et des effets à vos applications. Dans cette section, nous explorerons comment utiliser ces fonctionnalités pour rendre vos applications plus dynamiques et engageantes.

## Comprendre les animations dans FireMonkey

FireMonkey propose un système d'animation déclaratif, ce qui signifie que vous pouvez créer des animations sans écrire beaucoup de code. Le concept de base est simple : vous définissez une valeur de départ, une valeur d'arrivée et la durée pendant laquelle l'animation doit s'exécuter. FireMonkey se charge automatiquement de calculer et d'appliquer toutes les valeurs intermédiaires.

### Types d'animations disponibles

FireMonkey propose plusieurs types d'animations prédéfinis :

| Type d'animation | Description | Propriétés ciblées |
|------------------|-------------|-------------------|
| TFloatAnimation | Anime des valeurs numériques simples | Position, taille, opacité, etc. |
| TColorAnimation | Anime des transitions de couleur | Fill.Color, Stroke.Color, etc. |
| TBitmapAnimation | Anime entre différentes images | Bitmap d'un TImage |
| TGradientAnimation | Anime des transitions de dégradés | Fill.Gradient |
| TRectAnimation | Anime des rectangles (position et taille) | Position, taille |
| TPathAnimation | Anime le déplacement d'un objet le long d'un chemin | Position |

## Créer une animation simple

### Animation via l'interface graphique

La façon la plus simple de créer une animation est d'utiliser l'interface graphique de Delphi :

1. Placez un composant (par exemple, un `TRectangle`) sur votre formulaire
2. Dans la palette d'outils, cherchez la section **Animations**
3. Glissez-déposez un composant `TFloatAnimation` sur votre `TRectangle`
4. Dans l'inspecteur d'objets, configurez l'animation :
   - **PropertyName** : propriété à animer (par exemple, `Position.X`)
   - **StartValue** : valeur initiale (par exemple, `0`)
   - **StopValue** : valeur finale (par exemple, `200`)
   - **Duration** : durée en secondes (par exemple, `1.5`)
   - **Delay** : délai avant le démarrage en secondes
   - **Enabled** : activer/désactiver l'animation
   - **Loop** : répéter l'animation indéfiniment
   - **Trigger** : condition de déclenchement

### Animation par code

Vous pouvez également créer des animations entièrement par code :

```pascal
procedure TForm1.CreateAnimation;
var
  Animation: TFloatAnimation;
begin
  // Créer une animation pour déplacer horizontalement un rectangle
  Animation := TFloatAnimation.Create(Rectangle1);
  Animation.Parent := Rectangle1;  // L'animation doit être enfant du composant à animer
  Animation.PropertyName := 'Position.X';
  Animation.StartValue := 10;
  Animation.StopValue := 200;
  Animation.Duration := 2;  // 2 secondes
  Animation.Delay := 0.5;   // Attendre 0.5 seconde avant de démarrer
  Animation.Interpolation := TInterpolationType.Quadratic;
  Animation.AnimationType := TAnimationType.InOut;
  Animation.Loop := True;   // Répéter indéfiniment
  Animation.Enabled := True; // Démarrer l'animation
end;
```

## Contrôler les animations

### Démarrer et arrêter une animation

Pour contrôler une animation à partir d'un événement, comme un clic de bouton :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Activer ou désactiver l'animation
  if FloatAnimation1.Enabled then
  begin
    FloatAnimation1.Enabled := False;
    Button1.Text := 'Démarrer l''animation';
  end
  else
  begin
    FloatAnimation1.Enabled := True;
    Button1.Text := 'Arrêter l''animation';
  end;
end;
```

### Détecter la fin d'une animation

Vous pouvez réagir à la fin d'une animation en utilisant l'événement `OnFinish` :

```pascal
procedure TForm1.AnimationFinished(Sender: TObject);
begin
  ShowMessage('Animation terminée !');

  // Lancer une autre animation à la suite
  SecondAnimation.Enabled := True;
end;

procedure TForm1.ConfigureAnimations;
begin
  FloatAnimation1.OnFinish := AnimationFinished;
end;
```

## Types d'interpolation

L'interpolation détermine comment les valeurs intermédiaires sont calculées entre le début et la fin de l'animation. FireMonkey offre plusieurs types d'interpolation :

| Type d'interpolation | Description | Effet visuel |
|----------------------|-------------|-------------|
| Linear | Progression constante | Mouvement régulier et mécanique |
| Quadratic | Progression selon une courbe quadratique | Mouvement naturel avec légère accélération/décélération |
| Cubic | Progression selon une courbe cubique | Mouvement plus fluide avec accélération/décélération prononcée |
| Quartic | Progression selon une courbe à la puissance 4 | Mouvement très accentué |
| Quintic | Progression selon une courbe à la puissance 5 | Effet très prononcé avec forte accélération/décélération |
| Sinusoidal | Progression selon une courbe sinusoïdale | Mouvement ondulant |
| Exponential | Progression exponentielle | Accélération ou décélération très rapide |
| Circular | Progression circulaire | Mouvement avec une transition douce |
| Elastic | Effet élastique avec rebond | Effet de ressort |
| Back | Légèrement dépasse la cible puis revient | Effet de "trop loin puis retour" |
| Bounce | Effet de rebond à la fin | Rebondissement comme une balle |

Pour configurer l'interpolation :

```pascal
procedure TForm1.ConfigurerInterpolation;
begin
  FloatAnimation1.Interpolation := TInterpolationType.Elastic;
  FloatAnimation1.AnimationType := TAnimationType.Out; // In, Out, ou InOut
end;
```

## Animations courantes

### Animation de fondu (Fade)

```pascal
procedure TForm1.CreerAnimationFondu;
var
  Animation: TFloatAnimation;
begin
  Animation := TFloatAnimation.Create(Rectangle1);
  Animation.Parent := Rectangle1;
  Animation.PropertyName := 'Opacity';
  Animation.StartValue := 1;   // Totalement visible
  Animation.StopValue := 0;    // Totalement transparent
  Animation.Duration := 1;     // 1 seconde
  Animation.Interpolation := TInterpolationType.Linear;
  Animation.Enabled := True;
end;
```

### Animation de redimensionnement

```pascal
procedure TForm1.CreerAnimationRedimensionnement;
var
  Animation: TFloatAnimation;
begin
  Animation := TFloatAnimation.Create(Rectangle1);
  Animation.Parent := Rectangle1;
  Animation.PropertyName := 'Scale.X';  // Échelle horizontale
  Animation.StartValue := 1;
  Animation.StopValue := 1.5;
  Animation.Duration := 0.8;
  Animation.Interpolation := TInterpolationType.Elastic;
  Animation.AnimationType := TAnimationType.InOut;
  Animation.Enabled := True;

  // Créer une deuxième animation pour l'échelle verticale
  var Animation2 := TFloatAnimation.Create(Rectangle1);
  Animation2.Parent := Rectangle1;
  Animation2.PropertyName := 'Scale.Y';  // Échelle verticale
  Animation2.StartValue := 1;
  Animation2.StopValue := 1.5;
  Animation2.Duration := 0.8;
  Animation2.Interpolation := TInterpolationType.Elastic;
  Animation2.AnimationType := TAnimationType.InOut;
  Animation2.Enabled := True;
end;
```

### Animation de rotation

```pascal
procedure TForm1.CreerAnimationRotation;
var
  Animation: TFloatAnimation;
begin
  Animation := TFloatAnimation.Create(Rectangle1);
  Animation.Parent := Rectangle1;
  Animation.PropertyName := 'RotationAngle';
  Animation.StartValue := 0;
  Animation.StopValue := 360;  // Rotation complète de 360 degrés
  Animation.Duration := 2;
  Animation.Loop := True;
  Animation.Enabled := True;
end;
```

### Animation de couleur

```pascal
procedure TForm1.CreerAnimationCouleur;
var
  Animation: TColorAnimation;
begin
  Animation := TColorAnimation.Create(Rectangle1);
  Animation.Parent := Rectangle1;
  Animation.PropertyName := 'Fill.Color';
  Animation.StartValue := TAlphaColors.Blue;
  Animation.StopValue := TAlphaColors.Red;
  Animation.Duration := 1.5;
  Animation.Loop := True;
  Animation.Interpolation := TInterpolationType.Sinusoidal;
  Animation.AnimationType := TAnimationType.InOut;
  Animation.Enabled := True;
end;
```

## Animations déclenchées (Trigger)

FireMonkey permet de déclencher des animations automatiquement lorsque certaines conditions sont remplies, comme lorsque la souris passe sur un composant.

### Animation au survol de la souris

```pascal
procedure TForm1.ConfigurerAnimationSurvol;
begin
  // Créer une animation qui s'exécute quand la souris survole le bouton
  var Animation := TColorAnimation.Create(Button1);
  Animation.Parent := Button1;
  Animation.PropertyName := 'Fill.Color';
  Animation.StartValue := TAlphaColors.Blue;
  Animation.StopValue := TAlphaColors.Crimson;
  Animation.Duration := 0.2;
  Animation.Trigger := 'IsMouseOver=true';      // Déclenché quand la souris est sur le bouton
  Animation.TriggerInverse := 'IsMouseOver=false'; // Animation inverse quand la souris quitte le bouton
  Animation.Enabled := True;
end;
```

### Animation au clic

```pascal
procedure TForm1.ConfigurerAnimationClic;
begin
  // Créer une animation de "pression" quand on clique sur le bouton
  var Animation := TFloatAnimation.Create(Button1);
  Animation.Parent := Button1;
  Animation.PropertyName := 'Scale.X';
  Animation.StartValue := 1;
  Animation.StopValue := 0.95;  // Légèrement plus petit
  Animation.Duration := 0.1;
  Animation.AutoReverse := True; // Revenir à la taille normale
  Animation.Trigger := 'IsPressed=true';
  Animation.Enabled := True;

  // Faire la même chose pour l'échelle verticale
  var Animation2 := TFloatAnimation.Create(Button1);
  Animation2.Parent := Button1;
  Animation2.PropertyName := 'Scale.Y';
  Animation2.StartValue := 1;
  Animation2.StopValue := 0.95;
  Animation2.Duration := 0.1;
  Animation2.AutoReverse := True;
  Animation2.Trigger := 'IsPressed=true';
  Animation2.Enabled := True;
end;
```

## Effets visuels

Outre les animations, FireMonkey propose plusieurs effets visuels que vous pouvez appliquer à vos composants pour améliorer leur apparence.

### Types d'effets disponibles

| Effet | Description |
|-------|-------------|
| TBlurEffect | Ajoute un flou à un composant |
| TShadowEffect | Ajoute une ombre portée |
| TGlowEffect | Ajoute une lueur autour d'un composant |
| TReflectionEffect | Ajoute un reflet sous un composant |
| TInnerGlowEffect | Ajoute une lueur à l'intérieur d'un composant |
| TBevelEffect | Ajoute un effet de biseautage |
| TMonochromeEffect | Transforme un composant en monochrome |
| TSepiaEffect | Applique un effet sépia (photo ancienne) |
| TInvertEffect | Inverse les couleurs d'un composant |

### Ajouter un effet d'ombre

Par l'interface graphique :
1. Sélectionnez un composant dans votre formulaire
2. Dans la palette d'outils, cherchez la section **Effets**
3. Faites glisser un composant `TShadowEffect` sur votre composant
4. Configurez les propriétés dans l'inspecteur d'objets

Par code :

```pascal
procedure TForm1.AjouterEffetOmbre;
var
  Effet: TShadowEffect;
begin
  Effet := TShadowEffect.Create(Rectangle1);
  Effet.Parent := Rectangle1;  // L'effet doit être enfant du composant cible
  Effet.Distance := 3;  // Distance de l'ombre
  Effet.Direction := 45;  // Direction de l'ombre (en degrés)
  Effet.Softness := 0.4;  // Adoucissement (0.0 à 1.0)
  Effet.Opacity := 0.7;  // Opacité (0.0 à 1.0)
  Effet.ShadowColor := TAlphaColors.Black;
  Effet.Enabled := True;
end;
```

### Combiner plusieurs effets

Vous pouvez ajouter plusieurs effets à un même composant :

```pascal
procedure TForm1.CombinerEffets;
var
  OmbreEffet: TShadowEffect;
  LueurEffet: TGlowEffect;
begin
  // Ajouter une ombre
  OmbreEffet := TShadowEffect.Create(Rectangle1);
  OmbreEffet.Parent := Rectangle1;
  OmbreEffet.Distance := 3;
  OmbreEffet.Direction := 45;
  OmbreEffet.Softness := 0.4;
  OmbreEffet.Opacity := 0.6;

  // Ajouter une lueur
  LueurEffet := TGlowEffect.Create(Rectangle1);
  LueurEffet.Parent := Rectangle1;
  LueurEffet.GlowColor := TAlphaColors.Yellow;
  LueurEffet.Opacity := 0.7;
  LueurEffet.Softness := 0.5;
  LueurEffet.Enabled := True;
end;
```

### Animer un effet

Vous pouvez animer les propriétés d'un effet pour créer des effets dynamiques :

```pascal
procedure TForm1.AnimerEffet;
var
  Animation: TFloatAnimation;
begin
  // Animer la propriété Softness de l'effet d'ombre
  Animation := TFloatAnimation.Create(ShadowEffect1);
  Animation.Parent := ShadowEffect1;
  Animation.PropertyName := 'Softness';
  Animation.StartValue := 0.1;
  Animation.StopValue := 0.8;
  Animation.Duration := 2;
  Animation.Loop := True;
  Animation.Enabled := True;
end;
```

## Exemples pratiques

### Exemple 1 : Bouton avec animation de survol et clic

```pascal
procedure TForm1.CreerBoutonAnime;
var
  Bouton: TRectangle;
  Texte: TText;
  AnimationSurvol: TColorAnimation;
  AnimationClic: TFloatAnimation;
  EffetOmbre: TShadowEffect;
begin
  // Créer un bouton personnalisé
  Bouton := TRectangle.Create(Self);
  Bouton.Parent := Self;
  Bouton.Position.X := 50;
  Bouton.Position.Y := 50;
  Bouton.Width := 150;
  Bouton.Height := 50;
  Bouton.Fill.Color := TAlphaColors.Skyblue;
  Bouton.Stroke.Color := TAlphaColors.Darkblue;
  Bouton.Stroke.Thickness := 2;
  Bouton.XRadius := 10;
  Bouton.YRadius := 10;
  Bouton.Cursor := crHandPoint;

  // Ajouter le texte du bouton
  Texte := TText.Create(Self);
  Texte.Parent := Bouton;
  Texte.Align := TAlignLayout.Client;
  Texte.TextSettings.FontColor := TAlphaColors.Black;
  Texte.Text := 'Cliquez-moi';
  Texte.TextSettings.HorzAlign := TTextAlign.Center;
  Texte.TextSettings.VertAlign := TTextAlign.Center;
  Texte.HitTest := False; // Permettre aux clics de passer à travers le texte

  // Ajouter un effet d'ombre
  EffetOmbre := TShadowEffect.Create(Self);
  EffetOmbre.Parent := Bouton;
  EffetOmbre.Distance := 3;
  EffetOmbre.Direction := 45;
  EffetOmbre.Softness := 0.3;
  EffetOmbre.Opacity := 0.6;

  // Animation au survol (changement de couleur)
  AnimationSurvol := TColorAnimation.Create(Self);
  AnimationSurvol.Parent := Bouton;
  AnimationSurvol.PropertyName := 'Fill.Color';
  AnimationSurvol.StartValue := TAlphaColors.Skyblue;
  AnimationSurvol.StopValue := TAlphaColors.Dodgerblue;
  AnimationSurvol.Duration := 0.2;
  AnimationSurvol.Trigger := 'IsMouseOver=true';
  AnimationSurvol.TriggerInverse := 'IsMouseOver=false';
  AnimationSurvol.Enabled := True;

  // Animation au clic (rétrécissement)
  AnimationClic := TFloatAnimation.Create(Self);
  AnimationClic.Parent := Bouton;
  AnimationClic.PropertyName := 'Scale.X';
  AnimationClic.StartValue := 1;
  AnimationClic.StopValue := 0.95;
  AnimationClic.Duration := 0.1;
  AnimationClic.AutoReverse := True;
  AnimationClic.Trigger := 'IsPressed=true';
  AnimationClic.Enabled := True;

  // Animation au clic (rétrécissement vertical)
  var AnimationClic2 := TFloatAnimation.Create(Self);
  AnimationClic2.Parent := Bouton;
  AnimationClic2.PropertyName := 'Scale.Y';
  AnimationClic2.StartValue := 1;
  AnimationClic2.StopValue := 0.95;
  AnimationClic2.Duration := 0.1;
  AnimationClic2.AutoReverse := True;
  AnimationClic2.Trigger := 'IsPressed=true';
  AnimationClic2.Enabled := True;

  // Gestionnaire d'événement pour le clic
  Bouton.OnClick :=
    procedure(Sender: TObject)
    begin
      ShowMessage('Bouton cliqué !');
    end;
end;
```

### Exemple 2 : Animation de chargement (spinner)

```pascal
procedure TForm1.CreerAnimationChargement;
var
  Cercle: TCircle;
  Animation: TFloatAnimation;
begin
  // Créer un cercle pour l'animation de chargement
  Cercle := TCircle.Create(Self);
  Cercle.Parent := Self;
  Cercle.Position.X := 100;
  Cercle.Position.Y := 100;
  Cercle.Width := 40;
  Cercle.Height := 40;
  Cercle.Fill.Kind := TBrushKind.None;  // Transparent à l'intérieur
  Cercle.Stroke.Color := TAlphaColors.Dodgerblue;
  Cercle.Stroke.Thickness := 4;
  Cercle.Stroke.Dash := TStrokeDash.Dash; // Style pointillé

  // Créer l'animation de rotation
  Animation := TFloatAnimation.Create(Self);
  Animation.Parent := Cercle;
  Animation.PropertyName := 'RotationAngle';
  Animation.StartValue := 0;
  Animation.StopValue := 360;
  Animation.Duration := 1.5;  // Rotation en 1.5 secondes
  Animation.Loop := True;     // Boucle infinie
  Animation.Enabled := True;  // Démarrer immédiatement
end;
```

### Exemple 3 : Menu avec animation d'ouverture/fermeture

```pascal
procedure TForm1.CreerMenuAnime;
var
  MenuFond: TRectangle;
  BoutonMenu: TSpeedButton;
  Animation: TFloatAnimation;
  EstOuvert: Boolean;
begin
  // Variables
  EstOuvert := False;

  // Créer le fond du menu
  MenuFond := TRectangle.Create(Self);
  MenuFond.Parent := Self;
  MenuFond.Position.X := -200; // Commencer hors écran
  MenuFond.Position.Y := 0;
  MenuFond.Width := 200;
  MenuFond.Height := Self.Height;
  MenuFond.Fill.Color := TAlphaColors.Lightgray;

  // Ajouter des éléments au menu
  for var i := 0 to 4 do
  begin
    var Item := TRectangle.Create(Self);
    Item.Parent := MenuFond;
    Item.Align := TAlignLayout.Top;
    Item.Margins.Top := 5;
    Item.Margins.Left := 5;
    Item.Margins.Right := 5;
    Item.Height := 40;
    Item.Fill.Color := TAlphaColors.White;
    Item.XRadius := 5;
    Item.YRadius := 5;

    var Texte := TText.Create(Self);
    Texte.Parent := Item;
    Texte.Align := TAlignLayout.Client;
    Texte.Text := 'Option ' + (i+1).ToString;
    Texte.TextSettings.HorzAlign := TTextAlign.Center;
    Texte.TextSettings.VertAlign := TTextAlign.Center;
  end;

  // Bouton pour ouvrir/fermer le menu
  BoutonMenu := TSpeedButton.Create(Self);
  BoutonMenu.Parent := Self;
  BoutonMenu.Position.X := 10;
  BoutonMenu.Position.Y := 10;
  BoutonMenu.Width := 40;
  BoutonMenu.Height := 40;
  BoutonMenu.Text := '☰'; // Symbole hamburger
  BoutonMenu.TextSettings.Font.Size := 20;

  // Créer l'animation (mais ne pas la démarrer encore)
  Animation := TFloatAnimation.Create(Self);
  Animation.Parent := MenuFond;
  Animation.PropertyName := 'Position.X';
  Animation.Duration := 0.3;
  Animation.Interpolation := TInterpolationType.Cubic;
  Animation.AnimationType := TAnimationType.InOut;

  // Gérer le clic sur le bouton
  BoutonMenu.OnClick :=
    procedure(Sender: TObject)
    begin
      if EstOuvert then
      begin
        // Fermer le menu
        Animation.StopValue := -200;
        Animation.StartValue := 0;
      end
      else
      begin
        // Ouvrir le menu
        Animation.StartValue := -200;
        Animation.StopValue := 0;
      end;

      Animation.Enabled := True; // Démarrer l'animation
      EstOuvert := not EstOuvert; // Inverser l'état
    end;
end;
```

## Bonnes pratiques pour les animations

1. **Modération** : Utilisez les animations avec parcimonie pour ne pas surcharger l'interface utilisateur
2. **Performances** : Sur les appareils mobiles, limitez le nombre d'animations simultanées
3. **Cohérence** : Utilisez des animations similaires pour des actions similaires
4. **Durée** : Préférez des animations courtes (0.2-0.5s) pour les interactions fréquentes
5. **But** : Les animations doivent avoir un but (guider l'attention, montrer une relation, etc.)
6. **Désactivation** : Offrez une option pour désactiver les animations aux utilisateurs qui préfèrent une interface plus sobre

## Animation vs Performance

Les animations peuvent affecter les performances, surtout sur les appareils mobiles. Voici quelques techniques pour optimiser vos animations :

```pascal
procedure TForm1.OptimiserAnimations;
begin
  // Réduire la complexité des animations
  Animation1.Interpolation := TInterpolationType.Linear; // L'interpolation linéaire est la moins coûteuse

  // Limiter le nombre d'images par seconde pour les appareils moins puissants
  if EstAppareilBasDeGamme then
    Animation1.FrameRate := 30
  else
    Animation1.FrameRate := 60;

  // Éviter d'animer trop de propriétés en même temps
  Animation2.Enabled := False; // Désactiver les animations non essentielles

  // Éviter les effets coûteux lors des animations
  GlowEffect1.Enabled := False; // Désactiver les effets pendant l'animation

  // Utiliser l'accélération matérielle quand c'est possible
  Rectangle1.DisableInterpolation := False;
end;
```

## Conclusion

Les animations et effets visuels de FireMonkey vous permettent de créer des interfaces utilisateur dynamiques et engageantes avec un minimum d'effort. En comprenant les différents types d'animations et d'effets disponibles, ainsi que leurs propriétés et options, vous pouvez améliorer considérablement l'expérience utilisateur de vos applications multi-plateformes.

N'oubliez pas que la clé d'une bonne animation est qu'elle soit subtile et qu'elle serve un objectif précis dans l'interface utilisateur. Avec de la pratique et en suivant les bonnes pratiques présentées dans cette section, vous pourrez intégrer des animations qui améliorent réellement l'utilisabilité de vos applications plutôt que de simplement les décorer.

Dans la section suivante, nous explorerons le développement pour Linux avec FireMonkey, ouvrant ainsi une plateforme supplémentaire pour vos applications multi-plateformes.
