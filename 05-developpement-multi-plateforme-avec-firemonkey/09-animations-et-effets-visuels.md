🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.9 Animations et effets visuels

## Introduction

Les animations et effets visuels sont ce qui donne vie à une interface utilisateur. Une application statique peut être fonctionnelle, mais une application avec des animations bien pensées devient intuitive, agréable et moderne. FireMonkey intègre un système d'animation puissant et des effets visuels prêts à l'emploi qui vous permettent de créer des interfaces dynamiques et attrayantes sans être un expert en graphisme. Dans cette section, nous allons explorer comment utiliser ces outils pour enrichir vos applications.

## 1. Pourquoi animer ?

### Les bénéfices des animations

**Guidage de l'attention** :
- Une animation attire naturellement le regard
- Guide l'utilisateur vers les éléments importants
- Aide à comprendre les transitions

**Feedback visuel** :
- Confirme qu'une action a été prise en compte
- Rend l'interface plus réactive et vivante
- Améliore la perception de rapidité

**Continuité** :
- Les transitions douces sont plus naturelles que les changements brusques
- Aide à comprendre la relation entre les écrans
- Réduit la désorientation de l'utilisateur

**Modernité** :
- Les interfaces modernes sont dynamiques
- Crée une impression de qualité
- Se distingue des applications basiques

### Quand utiliser les animations ?

**✅ Utiliser des animations pour** :
- Transitions entre écrans
- Apparition/disparition d'éléments
- Feedback sur les boutons (effet de pression)
- Mise en évidence d'informations importantes
- Chargement et attente
- Révéler/cacher des menus

**❌ Éviter les animations pour** :
- Chaque petit changement (trop = fatiguant)
- Actions critiques nécessitant rapidité
- Contenus textuels longs (lecture difficile)
- Sur mobile bas de gamme (performance)

## 2. Le système d'animation FireMonkey

### Principe général

FireMonkey utilise un système d'animation **déclaratif** et **automatique** :

**Déclaratif** : Vous dites "ce que vous voulez" pas "comment le faire"
```pascal
// Vous dites : "Je veux que Position.X passe de 0 à 300"
// FireMonkey calcule toutes les étapes intermédiaires
```

**Automatique** : FireMonkey gère le timing et les calculs
```pascal
// Pas besoin de Timer ou de boucle
// FireMonkey utilise le moteur de rendu pour animer
```

### Architecture des animations

```
TAnimation (classe de base abstraite)
├── TFloatAnimation (animer un nombre)
├── TColorAnimation (animer une couleur)
├── TGradientAnimation (animer un dégradé)
├── TBitmapAnimation (animer entre images)
├── TBitmapListAnimation (animer une séquence)
└── TPathAnimation (animer le long d'un chemin)
```

Chaque type d'animation hérite de `TAnimation` et ajoute des fonctionnalités spécifiques.

## 3. TFloatAnimation - Animations de nombres

### Qu'est-ce que TFloatAnimation ?

`TFloatAnimation` permet d'animer n'importe quelle propriété numérique (Single, Float) d'un composant.

**Propriétés animables** :
- Position (X, Y)
- Taille (Width, Height)
- Opacité (Opacity)
- Rotation (RotationAngle)
- Échelle (Scale.X, Scale.Y)
- Et toute autre propriété de type Single ou Float

### Créer une animation simple

**Au moment de la conception (Design Time)** :

1. Sélectionner le composant à animer
2. Dans l'inspecteur d'objets : clic droit → "Add Animation"
3. Choisir TFloatAnimation
4. Configurer les propriétés :
   - PropertyName : Nom de la propriété à animer
   - StartValue : Valeur de départ
   - StopValue : Valeur d'arrivée
   - Duration : Durée en secondes
   - AutoReverse : Retour automatique
   - Loop : Répétition en boucle

**En code (Run Time)** :

```pascal
procedure TForm1.AnimerBouton;  
var  
  Anim: TFloatAnimation;
begin
  // Créer l'animation
  Anim := TFloatAnimation.Create(Button1);
  Anim.Parent := Button1;

  // Configurer
  Anim.PropertyName := 'Position.X';  // Propriété à animer
  Anim.StartValue := 0;                // Position de départ
  Anim.StopValue := 300;               // Position d'arrivée
  Anim.Duration := 1.0;                // 1 seconde

  // Lancer
  Anim.Start;
end;
```

### Exemples d'animations courantes

#### Faire apparaître (Fade In)

```pascal
procedure TForm1.FadeIn(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  // Partir de transparent
  Composant.Opacity := 0;
  Composant.Visible := True;

  // Animer vers opaque
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Opacity';
  Anim.StartValue := 0;
  Anim.StopValue := 1;
  Anim.Duration := 0.5;
  Anim.Start;
end;
```

#### Faire disparaître (Fade Out)

```pascal
procedure TForm1.FadeOut(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  // Animer vers transparent
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Opacity';
  Anim.StartValue := 1;
  Anim.StopValue := 0;
  Anim.Duration := 0.5;

  // Masquer à la fin
  Anim.OnFinish := procedure(Sender: TObject)
  begin
    Composant.Visible := False;
  end;

  Anim.Start;
end;
```

#### Déplacement horizontal

```pascal
procedure TForm1.DeplacerVersLaDroite(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Position.X';
  Anim.StartValue := Composant.Position.X;
  Anim.StopValue := Composant.Position.X + 200;
  Anim.Duration := 0.8;
  Anim.Start;
end;
```

#### Rotation

```pascal
procedure TForm1.FaireTourner(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'RotationAngle';
  Anim.StartValue := 0;
  Anim.StopValue := 360;  // Tour complet
  Anim.Duration := 2.0;
  Anim.Loop := True;      // En boucle infinie
  Anim.Start;
end;
```

#### Zoom (Échelle)

```pascal
procedure TForm1.Zoomer(Composant: TControl);  
var  
  AnimX, AnimY: TFloatAnimation;
begin
  // Animer Scale.X
  AnimX := TFloatAnimation.Create(Composant);
  AnimX.Parent := Composant;
  AnimX.PropertyName := 'Scale.X';
  AnimX.StartValue := 1;
  AnimX.StopValue := 1.5;  // 150% de la taille
  AnimX.Duration := 0.3;
  AnimX.AutoReverse := True;
  AnimX.Start;

  // Animer Scale.Y (en même temps)
  AnimY := TFloatAnimation.Create(Composant);
  AnimY.Parent := Composant;
  AnimY.PropertyName := 'Scale.Y';
  AnimY.StartValue := 1;
  AnimY.StopValue := 1.5;
  AnimY.Duration := 0.3;
  AnimY.AutoReverse := True;
  AnimY.Start;
end;
```

### Propriétés importantes de TAnimation

**Duration** : Durée de l'animation en secondes
```pascal
Anim.Duration := 0.5;  // 0.5 seconde  
Anim.Duration := 2.0;  // 2 secondes  
```

**Delay** : Délai avant le début
```pascal
Anim.Delay := 1.0;  // Attendre 1 seconde avant de démarrer
```

**AutoReverse** : Retour automatique
```pascal
Anim.AutoReverse := True;  // Va de Start à Stop puis revient
```

**Loop** : Répétition
```pascal
Anim.Loop := True;  // Boucle infinie
```

**Enabled** : Activer/désactiver
```pascal
Anim.Enabled := True;   // L'animation peut démarrer  
Anim.Enabled := False;  // L'animation est désactivée  
```

**Inverse** : Inverser le sens
```pascal
Anim.Inverse := True;  // Va de Stop vers Start au lieu de Start vers Stop
```

## 4. Interpolations (Easing)

### Qu'est-ce que l'interpolation ?

L'**interpolation** (ou **easing**) détermine la courbe de progression de l'animation :
- **Linéaire** : Vitesse constante
- **Ease In** : Démarre lentement, accélère
- **Ease Out** : Démarre vite, ralentit
- **Ease In Out** : Démarre lent, accélère au milieu, ralentit à la fin

### Types d'interpolation disponibles

```pascal
type
  TInterpolationType = (
    Linear,        // Linéaire (vitesse constante)
    Quadratic,     // Quadratique (accélération douce)
    Cubic,         // Cubique (accélération moyenne)
    Quartic,       // Quartique (accélération forte)
    Quintic,       // Quintique (accélération très forte)
    Sinusoidal,    // Sinusoïdale (naturelle)
    Exponential,   // Exponentielle (très rapide)
    Circular,      // Circulaire (effet de cercle)
    Elastic,       // Élastique (rebond)
    Back,          // Recul puis avance
    Bounce         // Rebondit à la fin
  );
```

### Utilisation des interpolations

```pascal
procedure TForm1.AnimerAvecInterpolation;  
var  
  Anim: TFloatAnimation;
begin
  Anim := TFloatAnimation.Create(Button1);
  Anim.Parent := Button1;
  Anim.PropertyName := 'Position.Y';
  Anim.StartValue := 0;
  Anim.StopValue := 300;
  Anim.Duration := 1.0;

  // Choisir le type d'interpolation
  Anim.Interpolation := TInterpolationType.Elastic;

  // Choisir le mode (In, Out, InOut)
  Anim.AnimationType := TAnimationType.InOut;

  Anim.Start;
end;
```

### Exemples d'effets selon l'interpolation

**Linear** : Mouvement de robot (mécanique)
```pascal
Anim.Interpolation := TInterpolationType.Linear;
// Mouvement constant, sans accélération
```

**Quadratic Out** : Mouvement naturel (recommandé)
```pascal
Anim.Interpolation := TInterpolationType.Quadratic;  
Anim.AnimationType := TAnimationType.Out;  
// Démarre vite, ralentit naturellement
```

**Elastic** : Effet ressort
```pascal
Anim.Interpolation := TInterpolationType.Elastic;  
Anim.AnimationType := TAnimationType.Out;  
// Arrive à destination et rebondit légèrement
```

**Bounce** : Effet rebond
```pascal
Anim.Interpolation := TInterpolationType.Bounce;  
Anim.AnimationType := TAnimationType.Out;  
// Rebondit plusieurs fois à l'arrivée
```

**Back** : Effet recul
```pascal
Anim.Interpolation := TInterpolationType.Back;  
Anim.AnimationType := TAnimationType.Out;  
// Dépasse légèrement la cible puis revient
```

### Recommandations par cas d'usage

**Apparition/disparition** : Quadratic ou Cubic Out  
**Déplacement d'éléments** : Quadratic InOut  
**Feedback bouton** : Back ou Elastic (subtil)  
**Animation ludique** : Bounce  
**Animation mécanique** : Linear  
**Attention utilisateur** : Elastic (modéré)  

## 5. TColorAnimation - Animations de couleur

### Animer les couleurs

`TColorAnimation` permet d'animer la transition entre deux couleurs.

```pascal
procedure TForm1.AnimerCouleurFond;  
var  
  Anim: TColorAnimation;
begin
  Anim := TColorAnimation.Create(Rectangle1);
  Anim.Parent := Rectangle1;
  Anim.PropertyName := 'Fill.Color';
  Anim.StartValue := TAlphaColors.Blue;
  Anim.StopValue := TAlphaColors.Red;
  Anim.Duration := 2.0;
  Anim.Start;
end;
```

### Effet de pulsation (couleur)

```pascal
procedure TForm1.PulsationCouleur(Composant: TRectangle);  
var  
  Anim: TColorAnimation;
begin
  Anim := TColorAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Fill.Color';
  Anim.StartValue := TAlphaColors.White;
  Anim.StopValue := TAlphaColors.Yellow;
  Anim.Duration := 1.0;
  Anim.AutoReverse := True;
  Anim.Loop := True;
  Anim.Start;
end;
```

### Animer la couleur du texte

```pascal
procedure TForm1.AnimerCouleurTexte;  
var  
  Anim: TColorAnimation;
begin
  Anim := TColorAnimation.Create(Label1);
  Anim.Parent := Label1;
  Anim.PropertyName := 'TextSettings.FontColor';
  Anim.StartValue := TAlphaColors.Black;
  Anim.StopValue := TAlphaColors.Red;
  Anim.Duration := 0.5;
  Anim.Start;
end;
```

## 6. Animations combinées

### Plusieurs animations simultanées

```pascal
procedure TForm1.AnimationComplete(Composant: TControl);  
var  
  AnimOpacity: TFloatAnimation;
  AnimScale: TFloatAnimation;
  AnimRotation: TFloatAnimation;
begin
  // Apparition progressive
  AnimOpacity := TFloatAnimation.Create(Composant);
  AnimOpacity.Parent := Composant;
  AnimOpacity.PropertyName := 'Opacity';
  AnimOpacity.StartValue := 0;
  AnimOpacity.StopValue := 1;
  AnimOpacity.Duration := 0.5;
  AnimOpacity.Start;

  // Zoom simultané
  AnimScale := TFloatAnimation.Create(Composant);
  AnimScale.Parent := Composant;
  AnimScale.PropertyName := 'Scale.X';
  AnimScale.StartValue := 0.5;
  AnimScale.StopValue := 1;
  AnimScale.Duration := 0.5;
  AnimScale.Start;

  // Rotation simultanée
  AnimRotation := TFloatAnimation.Create(Composant);
  AnimRotation.Parent := Composant;
  AnimRotation.PropertyName := 'RotationAngle';
  AnimRotation.StartValue := -180;
  AnimRotation.StopValue := 0;
  AnimRotation.Duration := 0.5;
  AnimRotation.Start;
end;
```

### Animations séquentielles

```pascal
procedure TForm1.AnimationSequence(Composant: TControl);  
var  
  Anim1, Anim2, Anim3: TFloatAnimation;
begin
  // Animation 1 : Déplacement
  Anim1 := TFloatAnimation.Create(Composant);
  Anim1.Parent := Composant;
  Anim1.PropertyName := 'Position.X';
  Anim1.StartValue := 0;
  Anim1.StopValue := 200;
  Anim1.Duration := 0.5;

  // Animation 2 : Commence après la 1
  Anim2 := TFloatAnimation.Create(Composant);
  Anim2.Parent := Composant;
  Anim2.PropertyName := 'Position.Y';
  Anim2.StartValue := Composant.Position.Y;
  Anim2.StopValue := Composant.Position.Y + 100;
  Anim2.Duration := 0.5;
  Anim2.Delay := 0.5;  // Commence après 0.5 sec

  // Animation 3 : Commence après la 2
  Anim3 := TFloatAnimation.Create(Composant);
  Anim3.Parent := Composant;
  Anim3.PropertyName := 'RotationAngle';
  Anim3.StartValue := 0;
  Anim3.StopValue := 360;
  Anim3.Duration := 0.5;
  Anim3.Delay := 1.0;  // Commence après 1 sec

  // Lancer toutes les animations
  Anim1.Start;
  Anim2.Start;
  Anim3.Start;
end;
```

### Chaînage avec OnFinish

```pascal
procedure TForm1.AnimationChainee(Composant: TControl);  
var  
  Anim1: TFloatAnimation;
begin
  // Première animation
  Anim1 := TFloatAnimation.Create(Composant);
  Anim1.Parent := Composant;
  Anim1.PropertyName := 'Position.X';
  Anim1.StartValue := Composant.Position.X;
  Anim1.StopValue := Composant.Position.X + 200;
  Anim1.Duration := 0.5;

  // Quand terminée, lancer la suivante
  Anim1.OnFinish := procedure(Sender: TObject)
  var
    Anim2: TFloatAnimation;
  begin
    Anim2 := TFloatAnimation.Create(Composant);
    Anim2.Parent := Composant;
    Anim2.PropertyName := 'Position.Y';
    Anim2.StartValue := Composant.Position.Y;
    Anim2.StopValue := Composant.Position.Y + 200;
    Anim2.Duration := 0.5;
    Anim2.Start;
  end;

  Anim1.Start;
end;
```

## 7. Effets visuels

### Qu'est-ce qu'un effet ?

Les **effets** sont des filtres visuels appliqués aux composants :
- Ombres
- Flous
- Lueurs
- Reflets
- Et bien d'autres

**Important** : Les effets utilisent le GPU et peuvent impacter les performances sur mobile.

### TShadowEffect - Ombre portée

```pascal
procedure TForm1.AjouterOmbre(Composant: TControl);  
var  
  Shadow: TShadowEffect;
begin
  Shadow := TShadowEffect.Create(Composant);
  Shadow.Parent := Composant;

  // Configuration
  Shadow.Distance := 5;              // Distance de l'ombre
  Shadow.Direction := 45;            // Angle (en degrés)
  Shadow.Softness := 0.4;            // Douceur (0-1)
  Shadow.Opacity := 0.6;             // Opacité (0-1)
  Shadow.ShadowColor := TAlphaColors.Black;
end;
```

### TGlowEffect - Effet de lueur

```pascal
procedure TForm1.AjouterLueur(Composant: TControl);  
var  
  Glow: TGlowEffect;
begin
  Glow := TGlowEffect.Create(Composant);
  Glow.Parent := Composant;

  // Configuration
  Glow.Softness := 0.4;              // Douceur
  Glow.Opacity := 0.9;               // Opacité
  Glow.GlowColor := TAlphaColors.Cyan;  // Couleur de la lueur
end;
```

### TBlurEffect - Effet de flou

```pascal
procedure TForm1.AjouterFlou(Composant: TControl);  
var  
  Blur: TBlurEffect;
begin
  Blur := TBlurEffect.Create(Composant);
  Blur.Parent := Composant;

  // Configuration
  Blur.Softness := 0.3;  // Intensité du flou (0-1)
end;
```

### TReflectionEffect - Effet miroir

```pascal
procedure TForm1.AjouterReflet(Composant: TControl);  
var  
  Reflection: TReflectionEffect;
begin
  Reflection := TReflectionEffect.Create(Composant);
  Reflection.Parent := Composant;

  // Configuration
  Reflection.Length := 0.5;    // Longueur du reflet (0-1)
  Reflection.Opacity := 0.5;   // Opacité du reflet
  Reflection.Offset := 0;      // Décalage vertical
end;
```

### TInnerGlowEffect - Lueur intérieure

```pascal
procedure TForm1.AjouterLueurInterieure(Composant: TControl);  
var  
  InnerGlow: TInnerGlowEffect;
begin
  InnerGlow := TInnerGlowEffect.Create(Composant);
  InnerGlow.Parent := Composant;

  // Configuration
  InnerGlow.Softness := 0.5;
  InnerGlow.Opacity := 0.7;
  InnerGlow.GlowColor := TAlphaColors.White;
end;
```

### Animer les effets

```pascal
procedure TForm1.AnimerOmbre(Composant: TControl);  
var  
  Shadow: TShadowEffect;
  Anim: TFloatAnimation;
begin
  // Créer l'ombre
  Shadow := TShadowEffect.Create(Composant);
  Shadow.Parent := Composant;
  Shadow.Distance := 2;
  Shadow.Softness := 0.3;

  // Animer la distance de l'ombre
  Anim := TFloatAnimation.Create(Shadow);
  Anim.Parent := Shadow;
  Anim.PropertyName := 'Distance';
  Anim.StartValue := 2;
  Anim.StopValue := 10;
  Anim.Duration := 0.3;
  Anim.AutoReverse := True;
  Anim.Start;
end;
```

## 8. Patterns d'animation courants

### Shake (Secousse)

```pascal
procedure TForm1.Shake(Composant: TControl);  
var  
  Anim: TFloatAnimation;
  PosInitiale: Single;
begin
  PosInitiale := Composant.Position.X;

  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Position.X';
  Anim.StartValue := PosInitiale - 10;
  Anim.StopValue := PosInitiale + 10;
  Anim.Duration := 0.1;
  Anim.AutoReverse := True;
  Anim.Loop := True;

  Anim.OnFinish := procedure(Sender: TObject)
  begin
    // Arrêter après 3 oscillations
    if Anim.Tag >= 3 then
    begin
      Anim.Stop;
      Composant.Position.X := PosInitiale;
    end
    else
      Anim.Tag := Anim.Tag + 1;
  end;

  Anim.Start;
end;
```

### Pulse (Pulsation)

```pascal
procedure TForm1.Pulse(Composant: TControl);  
var  
  AnimX, AnimY: TFloatAnimation;
begin
  // Pulser en X
  AnimX := TFloatAnimation.Create(Composant);
  AnimX.Parent := Composant;
  AnimX.PropertyName := 'Scale.X';
  AnimX.StartValue := 1;
  AnimX.StopValue := 1.1;
  AnimX.Duration := 0.5;
  AnimX.AutoReverse := True;
  AnimX.Loop := True;
  AnimX.Start;

  // Pulser en Y
  AnimY := TFloatAnimation.Create(Composant);
  AnimY.Parent := Composant;
  AnimY.PropertyName := 'Scale.Y';
  AnimY.StartValue := 1;
  AnimY.StopValue := 1.1;
  AnimY.Duration := 0.5;
  AnimY.AutoReverse := True;
  AnimY.Loop := True;
  AnimY.Start;
end;
```

### Slide In (Glissement vers l'intérieur)

```pascal
procedure TForm1.SlideInFromLeft(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  // Positionner hors écran à gauche
  Composant.Position.X := -Composant.Width;
  Composant.Visible := True;

  // Animer vers la position finale
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Position.X';
  Anim.StartValue := -Composant.Width;
  Anim.StopValue := 0;
  Anim.Duration := 0.5;
  Anim.Interpolation := TInterpolationType.Quadratic;
  Anim.AnimationType := TAnimationType.Out;
  Anim.Start;
end;
```

### Bounce (Rebond)

```pascal
procedure TForm1.Bounce(Composant: TControl);  
var  
  Anim: TFloatAnimation;
  PosY: Single;
begin
  PosY := Composant.Position.Y;

  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'Position.Y';
  Anim.StartValue := PosY;
  Anim.StopValue := PosY + 50;  // Descendre de 50 pixels
  Anim.Duration := 0.6;
  Anim.Interpolation := TInterpolationType.Bounce;
  Anim.AnimationType := TAnimationType.Out;
  Anim.AutoReverse := True;
  Anim.Start;
end;
```

### Flip (Retournement)

```pascal
procedure TForm1.Flip(Composant: TControl);  
var  
  Anim: TFloatAnimation;
begin
  Anim := TFloatAnimation.Create(Composant);
  Anim.Parent := Composant;
  Anim.PropertyName := 'RotationAngle';
  Anim.StartValue := 0;
  Anim.StopValue := 180;
  Anim.Duration := 0.6;
  Anim.Interpolation := TInterpolationType.Cubic;
  Anim.AnimationType := TAnimationType.InOut;
  Anim.Start;
end;
```

## 9. Indicateurs de chargement animés

### Spinner rotatif

```pascal
procedure TForm1.CreerSpinner;  
var  
  Circle: TCircle;
  Anim: TFloatAnimation;
begin
  // Créer un cercle incomplet
  Circle := TCircle.Create(Self);
  Circle.Parent := Self;
  Circle.Width := 50;
  Circle.Height := 50;
  Circle.Position.X := 100;
  Circle.Position.Y := 100;

  // Configurer l'apparence
  Circle.Stroke.Color := TAlphaColors.Blue;
  Circle.Stroke.Thickness := 3;
  Circle.Fill.Kind := TBrushKind.None;

  // Animation de rotation
  Anim := TFloatAnimation.Create(Circle);
  Anim.Parent := Circle;
  Anim.PropertyName := 'RotationAngle';
  Anim.StartValue := 0;
  Anim.StopValue := 360;
  Anim.Duration := 1.0;
  Anim.Loop := True;
  Anim.Start;
end;
```

### Points de chargement

```pascal
procedure TForm1.CreerDotsLoader;  
var  
  i: Integer;
  Dot: TCircle;
  Anim: TFloatAnimation;
begin
  for i := 0 to 2 do
  begin
    // Créer un point
    Dot := TCircle.Create(Self);
    Dot.Parent := Self;
    Dot.Width := 15;
    Dot.Height := 15;
    Dot.Position.X := 100 + (i * 25);
    Dot.Position.Y := 100;
    Dot.Fill.Color := TAlphaColors.Blue;

    // Animation d'opacité avec délai
    Anim := TFloatAnimation.Create(Dot);
    Anim.Parent := Dot;
    Anim.PropertyName := 'Opacity';
    Anim.StartValue := 0.3;
    Anim.StopValue := 1;
    Anim.Duration := 0.6;
    Anim.Delay := i * 0.2;  // Délai progressif
    Anim.AutoReverse := True;
    Anim.Loop := True;
    Anim.Start;
  end;
end;
```

### Barre de progression animée

```pascal
procedure TForm1.AnimerProgressBar;  
var  
  Anim: TFloatAnimation;
begin
  Anim := TFloatAnimation.Create(ProgressBar1);
  Anim.Parent := ProgressBar1;
  Anim.PropertyName := 'Value';
  Anim.StartValue := 0;
  Anim.StopValue := 100;
  Anim.Duration := 3.0;
  Anim.Interpolation := TInterpolationType.Linear;
  Anim.Start;
end;
```

## 10. Transitions entre écrans

### Transition simple

```pascal
procedure TForm1.TransitionVersEcran2;  
var  
  AnimOut, AnimIn: TFloatAnimation;
begin
  // Faire sortir l'écran actuel
  AnimOut := TFloatAnimation.Create(Panel1);
  AnimOut.Parent := Panel1;
  AnimOut.PropertyName := 'Position.X';
  AnimOut.StartValue := 0;
  AnimOut.StopValue := -Self.Width;
  AnimOut.Duration := 0.3;

  AnimOut.OnFinish := procedure(Sender: TObject)
  begin
    Panel1.Visible := False;
    Panel2.Visible := True;

    // Faire entrer le nouvel écran
    Panel2.Position.X := Self.Width;
    AnimIn := TFloatAnimation.Create(Panel2);
    AnimIn.Parent := Panel2;
    AnimIn.PropertyName := 'Position.X';
    AnimIn.StartValue := Self.Width;
    AnimIn.StopValue := 0;
    AnimIn.Duration := 0.3;
    AnimIn.Start;
  end;

  AnimOut.Start;
end;
```

### Transition avec fondu croisé

```pascal
procedure TForm1.TransitionFondu;  
var  
  AnimOut, AnimIn: TFloatAnimation;
begin
  // Préparer le nouvel écran
  Panel2.Position.X := 0;
  Panel2.Position.Y := 0;
  Panel2.Opacity := 0;
  Panel2.Visible := True;

  // Faire disparaître l'ancien
  AnimOut := TFloatAnimation.Create(Panel1);
  AnimOut.Parent := Panel1;
  AnimOut.PropertyName := 'Opacity';
  AnimOut.StartValue := 1;
  AnimOut.StopValue := 0;
  AnimOut.Duration := 0.5;
  AnimOut.Start;

  // Faire apparaître le nouveau
  AnimIn := TFloatAnimation.Create(Panel2);
  AnimIn.Parent := Panel2;
  AnimIn.PropertyName := 'Opacity';
  AnimIn.StartValue := 0;
  AnimIn.StopValue := 1;
  AnimIn.Duration := 0.5;
  AnimIn.Start;
end;
```

## 11. Animations au tap/clic

### Effet de pression sur bouton

```pascal
procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  AnimScale: TFloatAnimation;
begin
  // Réduire légèrement le bouton
  AnimScale := TFloatAnimation.Create(Button1);
  AnimScale.Parent := Button1;
  AnimScale.PropertyName := 'Scale.X';
  AnimScale.StartValue := 1;
  AnimScale.StopValue := 0.95;
  AnimScale.Duration := 0.1;
  AnimScale.Start;

  // Même chose pour Y
  AnimScale := TFloatAnimation.Create(Button1);
  AnimScale.Parent := Button1;
  AnimScale.PropertyName := 'Scale.Y';
  AnimScale.StartValue := 1;
  AnimScale.StopValue := 0.95;
  AnimScale.Duration := 0.1;
  AnimScale.Start;
end;

procedure TForm1.Button1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  AnimScale: TFloatAnimation;
begin
  // Revenir à la taille normale
  AnimScale := TFloatAnimation.Create(Button1);
  AnimScale.Parent := Button1;
  AnimScale.PropertyName := 'Scale.X';
  AnimScale.StartValue := 0.95;
  AnimScale.StopValue := 1;
  AnimScale.Duration := 0.1;
  AnimScale.Interpolation := TInterpolationType.Back;
  AnimScale.AnimationType := TAnimationType.Out;
  AnimScale.Start;

  AnimScale := TFloatAnimation.Create(Button1);
  AnimScale.Parent := Button1;
  AnimScale.PropertyName := 'Scale.Y';
  AnimScale.StartValue := 0.95;
  AnimScale.StopValue := 1;
  AnimScale.Duration := 0.1;
  AnimScale.Interpolation := TInterpolationType.Back;
  AnimScale.AnimationType := TAnimationType.Out;
  AnimScale.Start;
end;
```

### Effet ondulation (Ripple - Material Design)

```pascal
procedure TForm1.CreerEffetRipple(X, Y: Single);  
var  
  Circle: TCircle;
  AnimScale, AnimOpacity: TFloatAnimation;
begin
  // Créer un cercle à la position du tap
  Circle := TCircle.Create(Self);
  Circle.Parent := Self;
  Circle.Width := 10;
  Circle.Height := 10;
  Circle.Position.X := X - 5;
  Circle.Position.Y := Y - 5;
  Circle.Fill.Color := TAlphaColors.White;
  Circle.Opacity := 0.5;
  Circle.HitTest := False;  // Ne pas intercepter les clics

  // Animer l'expansion
  AnimScale := TFloatAnimation.Create(Circle);
  AnimScale.Parent := Circle;
  AnimScale.PropertyName := 'Scale.X';
  AnimScale.StartValue := 1;
  AnimScale.StopValue := 20;
  AnimScale.Duration := 0.6;

  AnimScale.OnFinish := procedure(Sender: TObject)
  begin
    Circle.Free;  // Libérer après l'animation
  end;

  AnimScale.Start;

  // Animer la disparition
  AnimOpacity := TFloatAnimation.Create(Circle);
  AnimOpacity.Parent := Circle;
  AnimOpacity.PropertyName := 'Opacity';
  AnimOpacity.StartValue := 0.5;
  AnimOpacity.StopValue := 0;
  AnimOpacity.Duration := 0.6;
  AnimOpacity.Start;
end;
```

## 12. Bonnes pratiques

### ✅ À FAIRE

**1. Animations courtes et douces**
```pascal
// Durée idéale : 0.2 à 0.5 secondes
Anim.Duration := 0.3;  // ✅ BON

// Trop long = utilisateur attend
Anim.Duration := 3.0;  // ❌ MAUVAIS
```

**2. Utiliser des interpolations naturelles**
```pascal
// Quadratic Out = naturel
Anim.Interpolation := TInterpolationType.Quadratic;  
Anim.AnimationType := TAnimationType.Out;  
```

**3. Animer les états, pas les actions**
```pascal
// ✅ BON : Animer l'apparition d'un panneau
FadeIn(PanelOptions);

// ❌ MAUVAIS : Animer chaque lettre d'un texte qu'on tape
// (sauf si c'est l'effet recherché)
```

**4. Limiter sur mobile**
```pascal
{$IFDEF ANDROID OR IOS}
  // Mobile : animations simples
  Anim.Duration := 0.2;
  // Pas d'effets visuels
{$ELSE}
  // Desktop : animations plus riches
  Anim.Duration := 0.5;
  AjouterOmbre(Panel1);
{$ENDIF}
```

**5. Libérer les animations terminées**
```pascal
Anim.OnFinish := procedure(Sender: TObject)  
begin  
  TAnimation(Sender).Free;  // Libérer la mémoire
end;
```

**6. Donner du feedback**
```pascal
// Toujours confirmer une action par une animation
procedure TForm1.SauvegarderClick(Sender: TObject);  
begin  
  // Sauvegarder
  Sauvegarder;

  // Feedback visuel
  Pulse(IconeConfirmation);
  FadeIn(LabelConfirmation);
end;
```

### ❌ À ÉVITER

**1. Animations trop nombreuses**
```pascal
// ❌ MAUVAIS : Tout animer
for i := 0 to 100 do
  AnimerElement(Elements[i]);
// = Framerate catastrophique
```

**2. Animations trop longues**
```pascal
// ❌ MAUVAIS : L'utilisateur attend
Anim.Duration := 5.0;
```

**3. Animations bloquantes**
```pascal
// ❌ MAUVAIS : Empêcher l'interaction pendant l'animation
// Laissez l'utilisateur interagir si possible
```

**4. Effets sur tous les composants**
```pascal
// ❌ MAUVAIS : Ombre sur chaque bouton
for var Button in ListeBoutons do
  AjouterOmbre(Button);
// = Performances dégradées sur mobile
```

**5. Oublier de libérer**
```pascal
// ❌ MAUVAIS : Créer sans libérer
procedure AnimerSansLiberer;  
begin  
  var Anim := TFloatAnimation.Create(nil);  // Fuite mémoire !
  Anim.Start;
end;
```

## 13. Performances des animations

### Optimiser pour mobile

```pascal
procedure TForm1.ConfigurerAnimationsSelonPlateforme;  
begin  
  {$IFDEF ANDROID OR IOS}
    // Mobile : Simplifier
    NombreAnimationsSimultanees := 3;
    DureeAnimationDefaut := 0.2;
    EffetsVisuelActifs := False;
  {$ELSE}
    // Desktop : Mode complet
    NombreAnimationsSimultanees := 10;
    DureeAnimationDefaut := 0.5;
    EffetsVisuelActifs := True;
  {$ENDIF}
end;
```

### Arrêter les animations inutiles

```pascal
procedure TForm1.FormDeactivate(Sender: TObject);  
begin  
  // Arrêter les animations quand l'app passe en arrière-plan
  for var i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TAnimation then
      TAnimation(Components[i]).Stop;
  end;
end;
```

### Réutiliser les animations

```pascal
var
  FAnimationFadeIn: TFloatAnimation;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Créer une fois
  FAnimationFadeIn := TFloatAnimation.Create(Self);
  FAnimationFadeIn.PropertyName := 'Opacity';
  FAnimationFadeIn.StartValue := 0;
  FAnimationFadeIn.StopValue := 1;
  FAnimationFadeIn.Duration := 0.3;
end;

procedure TForm1.FadeInComposant(Composant: TControl);  
begin  
  // Réutiliser
  FAnimationFadeIn.Parent := Composant;
  FAnimationFadeIn.Start;
end;
```

## Conclusion

Les animations et effets visuels sont des outils puissants pour créer des interfaces modernes et engageantes. Les points clés à retenir :

🎬 **Système intégré** : FireMonkey offre un système d'animation complet et facile

🎬 **Types d'animations** : Float, Color, Bitmap, Path pour tous les besoins

🎬 **Interpolations** : Donnent du naturel et de la personnalité

🎬 **Effets visuels** : Ombres, flous, lueurs enrichissent l'interface

🎬 **Combinaisons** : Animations simultanées et séquentielles pour effets complexes

🎬 **Patterns** : Shake, Pulse, Slide, Bounce sont réutilisables

🎬 **Modération** : Animations courtes, douces, et limitées sur mobile

🎬 **Performance** : Attention aux effets sur mobile, libérer les ressources

Avec ces techniques, vous pouvez transformer une interface statique en une expérience dynamique et professionnelle qui guide et ravit vos utilisateurs. L'animation n'est pas de la décoration : c'est un outil de communication qui améliore la compréhension et l'utilisabilité de votre application.

⏭️ [FMXLinux : développement pour Linux](/05-developpement-multi-plateforme-avec-firemonkey/10-fmxlinux-developpement-pour-linux.md)
