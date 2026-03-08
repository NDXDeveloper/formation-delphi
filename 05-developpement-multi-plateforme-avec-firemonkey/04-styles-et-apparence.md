🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.4 Styles et apparence

## Introduction

L'un des aspects les plus puissants et distinctifs de FireMonkey est son système de styles. Contrairement à la VCL où l'apparence des composants est largement déterminée par Windows, FireMonkey vous donne un contrôle total sur l'aspect visuel de votre application. Dans cette section, nous allons explorer comment fonctionnent les styles FMX et comment les utiliser pour créer des interfaces modernes et attrayantes.

## 1. Qu'est-ce qu'un style en FireMonkey ?

### Définition simple

Un **style** en FireMonkey est comme une "feuille de style" qui définit l'apparence visuelle de tous les composants de votre application. Pensez-y comme à un thème qui transforme l'apparence sans changer le code.

**Analogie** : Si votre application était une maison, le style serait la décoration intérieure (peinture, meubles, textures). Vous pouvez changer complètement la décoration sans modifier la structure de la maison.

### Comment ça fonctionne

En FireMonkey, chaque composant (bouton, champ de texte, etc.) n'est pas un contrôle système natif, mais un dessin vectoriel. Le style définit :

- **Les formes** : Rectangles arrondis, cercles, formes personnalisées
- **Les couleurs** : Fond, bordures, texte, dégradés
- **Les états** : Apparence normale, survolée, pressée, désactivée
- **Les effets** : Ombres, lueurs, transparence
- **Les animations** : Transitions entre les états

### Un style, plusieurs composants

Le grand avantage : un seul style s'applique à TOUS les composants de votre application. Changez le style, et toute l'interface change instantanément.

```pascal
// Sans changer une ligne de code de l'interface, vous pouvez :
StyleBook1.LoadFromFile('StyleModerne.style');    // Look moderne  
StyleBook1.LoadFromFile('StyleiOS.style');        // Look iOS  
StyleBook1.LoadFromFile('StyleAndroid.style');    // Look Android  
StyleBook1.LoadFromFile('StyleSombre.style');     // Mode sombre  
```

## 2. Les styles prédéfinis

### Styles natifs par plateforme

FireMonkey inclut des styles qui imitent l'apparence native de chaque plateforme :

**Windows.style** :
- Look Windows moderne
- Boutons rectangulaires avec légères ombres
- Couleurs système Windows

**macOS.style** :
- Style macOS élégant
- Boutons arrondis subtils
- Police et espacements macOS

**iOS.style** :
- Interface iOS épurée
- Boutons minimalistes
- Typographie iOS

**Android.style** :
- Material Design
- Effets d'ondulation (ripple)
- Couleurs vives Android

### Style par défaut

Lorsque vous créez un nouveau projet FireMonkey sans spécifier de style, FireMonkey utilise automatiquement le style correspondant à la plateforme cible :

- Compilation pour Windows → Style Windows
- Compilation pour iOS → Style iOS
- Compilation pour Android → Style Android
- Et ainsi de suite

**Résultat** : Votre application a automatiquement un look natif sur chaque plateforme, sans effort supplémentaire.

### Avantages des styles natifs

✅ Les utilisateurs reconnaissent les contrôles familiers de leur système

✅ Respect des conventions de chaque plateforme

✅ Aucune configuration nécessaire

### Quand utiliser un style personnalisé ?

Utilisez un style personnalisé quand :

- Vous voulez une **identité visuelle unique** pour votre marque
- Vous souhaitez une **apparence identique** sur toutes les plateformes
- Vous créez une **application créative** qui sort des standards
- Vous implémentez un **mode sombre** ou des thèmes alternatifs

## 3. Le composant TStyleBook

### Qu'est-ce que TStyleBook ?

`TStyleBook` est le composant qui charge et applique un style à votre formulaire et ses composants enfants.

**Placement** :
```pascal
// Au moment de la conception (design time)
// 1. Glissez un TStyleBook de la palette sur votre formulaire
// 2. Il apparaît dans la zone des composants non visuels
```

### Propriétés importantes

**FileName** : Chemin vers le fichier de style (.style)
```pascal
StyleBook1.FileName := 'C:\Styles\MonStyle.style';
```

**Resource** : Style embarqué dans l'application
```pascal
// Le style est intégré à l'exécutable, pas de fichier externe nécessaire
```

### Appliquer un StyleBook à un formulaire

```pascal
// Lier le StyleBook au formulaire
Form1.StyleBook := StyleBook1;

// Tous les composants du formulaire utilisent maintenant ce style
```

### Charger un style dynamiquement

```pascal
procedure TForm1.ChargerStyleModerne;  
begin  
  StyleBook1.LoadFromFile('Styles\Moderne.style');
  Form1.StyleBook := StyleBook1;
end;

procedure TForm1.ChargerStyleSombre;  
begin  
  StyleBook1.LoadFromFile('Styles\Sombre.style');
  Form1.StyleBook := StyleBook1;
end;

procedure TForm1.ButtonThemeClick(Sender: TObject);  
begin  
  if ModeActuel = 'Clair' then
  begin
    ChargerStyleSombre;
    ModeActuel := 'Sombre';
  end
  else
  begin
    ChargerStyleModerne;
    ModeActuel := 'Clair';
  end;
end;
```

**Résultat** : L'utilisateur peut basculer entre thème clair et sombre d'un simple clic !

### StyleBook et hiérarchie

Un StyleBook s'applique à son formulaire et **tous les composants enfants**, mais vous pouvez avoir plusieurs StyleBooks dans une application :

```pascal
// Form1 utilise un style
Form1.StyleBook := StyleBookPrincipal;

// Form2 utilise un style différent
Form2.StyleBook := StyleBookSecondaire;
```

## 4. La propriété StyleLookup

### Qu'est-ce que StyleLookup ?

Chaque composant FireMonkey a une propriété `StyleLookup` qui détermine quelle partie du style utiliser.

**Valeurs par défaut** :
- TButton → StyleLookup = 'buttonstyle'
- TEdit → StyleLookup = 'editstyle'
- TLabel → StyleLookup = 'labelstyle'
- TPanel → StyleLookup = 'panelstyle'

### Personnaliser StyleLookup

Vous pouvez changer le StyleLookup pour appliquer un style différent à un composant spécifique.

**Exemple** : Créer un bouton avec l'apparence d'un bouton de barre d'outils
```pascal
Button1.StyleLookup := 'toolbuttonstyle';
```

**Exemple** : Créer un bouton circulaire
```pascal
ButtonCirculaire.StyleLookup := 'orbbuttonstyle';
```

### Styles prédéfinis disponibles

FireMonkey inclut de nombreux StyleLookup prédéfinis :

**Pour les boutons** :
- `buttonstyle` : Bouton standard
- `toolbuttonstyle` : Bouton de barre d'outils
- `priortoolbuttonstyle` : Bouton avec indicateur de priorité
- `segmentedbuttonstyle` : Bouton segmenté
- `orbbuttonstyle` : Bouton circulaire

**Pour les panneaux** :
- `panelstyle` : Panneau standard
- `transparentcircle` : Panneau circulaire transparent
- `groupboxstyle` : Boîte de groupe

**Pour les champs** :
- `editstyle` : Champ de texte standard
- `searcheditbutton` : Champ de recherche avec icône
- `clearingeditbutton` : Champ avec bouton de suppression

### Créer des variantes visuelles

En changeant simplement le StyleLookup, vous créez des variantes sans dupliquer le code :

```pascal
// Bouton d'action principale
ButtonPrimary.Text := 'Sauvegarder';  
ButtonPrimary.StyleLookup := 'buttonstyle';  

// Bouton secondaire avec style différent
ButtonSecondary.Text := 'Annuler';  
ButtonSecondary.StyleLookup := 'toolbuttonstyle';  
```

## 5. Personnalisation des couleurs et propriétés

### Modifier les couleurs de base

Même en utilisant un style prédéfini, vous pouvez modifier certaines propriétés visuelles :

#### Propriété Fill (Remplissage)

```pascal
// Changer la couleur de fond d'un rectangle
Rectangle1.Fill.Color := TAlphaColors.Blue;

// Utiliser un dégradé
Rectangle1.Fill.Kind := TBrushKind.Gradient;  
Rectangle1.Fill.Gradient.Color := TAlphaColors.Blue;  
Rectangle1.Fill.Gradient.Color1 := TAlphaColors.Lightblue;  
```

#### Propriété Stroke (Bordure)

```pascal
// Couleur de bordure
Rectangle1.Stroke.Color := TAlphaColors.Red;

// Épaisseur de bordure
Rectangle1.Stroke.Thickness := 2;

// Type de bordure
Rectangle1.Stroke.Kind := TBrushKind.Solid;
```

#### Transparence (Opacity)

```pascal
// Opacité complète
Panel1.Opacity := 1.0;

// Semi-transparent
Panel1.Opacity := 0.5;

// Presque invisible
Panel1.Opacity := 0.1;
```

### Personnalisation du texte

#### TextSettings pour les composants de texte

```pascal
// Taille de police
Label1.TextSettings.Font.Size := 18;

// Famille de police
Label1.TextSettings.Font.Family := 'Segoe UI';

// Style de police
Label1.TextSettings.FontColor := TAlphaColors.Navy;  
Label1.TextSettings.Font.Style := [TFontStyle.fsBold];  

// Alignement horizontal
Label1.TextSettings.HorzAlign := TTextAlign.Center;

// Alignement vertical
Label1.TextSettings.VertAlign := TTextAlign.Center;
```

### Couleurs avec canal alpha

FireMonkey utilise des couleurs ARGB (Alpha, Rouge, Vert, Bleu) permettant la transparence :

```pascal
// Couleurs prédéfinies
Rectangle1.Fill.Color := TAlphaColors.Red;  
Rectangle1.Fill.Color := TAlphaColors.Blue;  

// Couleur personnalisée avec alpha
// Format : $AARRGGBB (AA = alpha, RR = rouge, GG = vert, BB = bleu)
Rectangle1.Fill.Color := $80FF0000;  // Rouge semi-transparent

// Utiliser TAlphaColorRec pour plus de clarté
Rectangle1.Fill.Color := TAlphaColorRec.Create(255, 0, 0, 128); // Rouge, alpha 128
```

## 6. Création de styles personnalisés

### L'éditeur de styles

Delphi inclut un **éditeur de styles graphique** pour créer et modifier des styles.

**Accès** :
1. Menu : **Outils → Éditeur de style de bitmap**
2. Ou clic droit sur StyleBook → **Éditer un style personnalisé**

### Anatomie d'un style

Un style FireMonkey est composé de :

**Objects** : Éléments visuels (rectangles, textes, images)
```
buttonstyle
├── background (rectangle de fond)
├── text (label du texte)
└── glyph (icône optionnelle)
```

**États** : Variations selon l'interaction
- Normal
- Hovered (survolé)
- Pressed (pressé)
- Focused (focus)
- Disabled (désactivé)

### Processus de création simplifié

**Étape 1** : Partir d'un style existant
```pascal
// Dupliquer un style existant comme base
// File → New → Other → Delphi Files → FireMonkey Style
```

**Étape 2** : Modifier les éléments visuels
- Changer les couleurs
- Modifier les formes
- Ajouter des effets

**Étape 3** : Définir les états
- Créer des variantes pour chaque état (normal, hover, pressed)
- Ajouter des animations de transition

**Étape 4** : Sauvegarder
```pascal
// Sauvegarder en .style
// File → Save As → MonStylePerso.style
```

**Étape 5** : Utiliser dans l'application
```pascal
StyleBook1.LoadFromFile('MonStylePerso.style');
```

### Styles basés sur des images

Vous pouvez créer des styles entièrement basés sur des images (bitmaps) :

**Avantages** :
- Design très personnalisé
- Aspect unique
- Contrôle artistique total

**Inconvénients** :
- Taille de fichier plus importante
- Moins flexible pour les changements de taille
- Nécessite des images pour chaque état

## 7. Thèmes clair et sombre

### Implémenter un mode sombre

Le mode sombre est devenu un standard sur toutes les plateformes. Voici comment l'implémenter :

#### Méthode 1 : Deux StyleBooks

```pascal
var
  StyleClair: TStyleBook;
  StyleSombre: TStyleBook;
  ModeSombre: Boolean;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  ModeSombre := False;
  StyleClair := TStyleBook.Create(Self);
  StyleClair.LoadFromFile('Styles\Clair.style');

  StyleSombre := TStyleBook.Create(Self);
  StyleSombre.LoadFromFile('Styles\Sombre.style');

  // Appliquer le style clair par défaut
  Self.StyleBook := StyleClair;
end;

procedure TForm1.BasculerTheme;  
begin  
  if ModeSombre then
  begin
    Self.StyleBook := StyleClair;
    ModeSombre := False;
  end
  else
  begin
    Self.StyleBook := StyleSombre;
    ModeSombre := True;
  end;
end;
```

#### Méthode 2 : Couleurs adaptatives

```pascal
procedure TForm1.AppliquerCouleurs(ModeSombre: Boolean);  
begin  
  if ModeSombre then
  begin
    Form1.Fill.Color := TAlphaColors.Black;
    Label1.TextSettings.FontColor := TAlphaColors.White;
    Panel1.Fill.Color := $FF1E1E1E;  // Gris foncé
  end
  else
  begin
    Form1.Fill.Color := TAlphaColors.White;
    Label1.TextSettings.FontColor := TAlphaColors.Black;
    Panel1.Fill.Color := TAlphaColors.Whitesmoke;
  end;
end;
```

### Détecter la préférence système

Sur les plateformes modernes, vous pouvez détecter si l'utilisateur préfère un thème sombre :

```pascal
uses
  FMX.Platform;

function TForm1.SystemeEnModeSombre: Boolean;  
var  
  ThemeService: IFMXThemeService;
begin
  Result := False;
  if TPlatformServices.Current.SupportsPlatformService(IFMXThemeService) then
  begin
    ThemeService := TPlatformServices.Current.GetPlatformService(IFMXThemeService) as IFMXThemeService;
    // Vérifier le thème système
    Result := ThemeService.DefaultTheme = 'Dark';
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  if SystemeEnModeSombre then
    AppliquerThemeSombre
  else
    AppliquerThemeClair;
end;
```

## 8. Effets visuels sur les styles

### Ajouter des effets aux composants

FireMonkey permet d'ajouter des effets visuels qui s'intègrent au système de styles :

#### Ombre portée (Shadow)

```pascal
var
  Shadow: TShadowEffect;
begin
  Shadow := TShadowEffect.Create(Panel1);
  Shadow.Parent := Panel1;
  Shadow.Distance := 5;
  Shadow.Direction := 45;
  Shadow.Softness := 0.4;
  Shadow.Opacity := 0.5;
  Shadow.ShadowColor := TAlphaColors.Black;
end;
```

#### Effet de lueur (Glow)

```pascal
var
  Glow: TGlowEffect;
begin
  Glow := TGlowEffect.Create(Button1);
  Glow.Parent := Button1;
  Glow.GlowColor := TAlphaColors.Cyan;
  Glow.Softness := 0.3;
  Glow.Opacity := 0.8;
end;
```

#### Effet de flou (Blur)

```pascal
var
  Blur: TBlurEffect;
begin
  Blur := TBlurEffect.Create(Image1);
  Blur.Parent := Image1;
  Blur.Softness := 0.5;
end;
```

#### Reflet (Reflection)

```pascal
var
  Reflection: TReflectionEffect;
begin
  Reflection := TReflectionEffect.Create(Label1);
  Reflection.Parent := Label1;
  Reflection.Opacity := 0.5;
  Reflection.Length := 0.5;
end;
```

### Combiner plusieurs effets

Vous pouvez appliquer plusieurs effets à un même composant :

```pascal
procedure TForm1.StyliserBoutonPremium(Button: TButton);  
var  
  Shadow: TShadowEffect;
  Glow: TGlowEffect;
begin
  // Ombre
  Shadow := TShadowEffect.Create(Button);
  Shadow.Parent := Button;
  Shadow.Distance := 3;
  Shadow.Softness := 0.3;

  // Lueur
  Glow := TGlowEffect.Create(Button);
  Glow.Parent := Button;
  Glow.GlowColor := TAlphaColors.Gold;
  Glow.Softness := 0.2;
end;
```

## 9. Styles et performances

### Impact des styles sur les performances

**Styles vectoriels** :
- ✅ Légers en mémoire
- ✅ S'adaptent à toutes les résolutions
- ✅ Rapides à rendre

**Styles basés sur images** :
- ⚠️ Plus lourds en mémoire
- ⚠️ Nécessitent plusieurs résolutions pour être nets
- ⚠️ Temps de chargement plus long

**Effets visuels** :
- ⚠️ Consomment du GPU
- ⚠️ À utiliser avec modération sur mobile
- ⚠️ Peuvent ralentir l'interface si trop nombreux

### Optimisation

**Sur mobile** :
```pascal
{$IFDEF ANDROID OR IOS}
  // Limiter les effets sur mobile
  Shadow1.Enabled := False;
  Blur1.Enabled := False;
{$ELSE}
  // Activer tous les effets sur desktop
  Shadow1.Enabled := True;
  Blur1.Enabled := True;
{$ENDIF}
```

**Chargement asynchrone** :
```pascal
procedure TForm1.ChargerStyleAsync;  
begin  
  TTask.Run(procedure
  begin
    StyleBook1.LoadFromFile('StyleLourd.style');
    TThread.Synchronize(nil, procedure
    begin
      Self.StyleBook := StyleBook1;
    end);
  end);
end;
```

## 10. Styles par composant individuel

### Personnaliser un composant spécifique

Parfois, vous voulez qu'un seul composant ait un style différent sans changer tout le formulaire.

#### Méthode 1 : Propriétés directes

```pascal
// Personnaliser directement
Button1.StyledSettings := [];  // Désactiver le style hérité  
Button1.Fill.Color := TAlphaColors.Red;  
Button1.TextSettings.FontColor := TAlphaColors.White;  
```

#### Méthode 2 : StyleLookup personnalisé

```pascal
// Créer un style unique dans l'éditeur de styles
// et lui donner un nom personnalisé
Button1.StyleLookup := 'MyCoolButtonStyle';
```

### La propriété StyledSettings

`StyledSettings` contrôle quelles propriétés sont influencées par le style :

```pascal
// Hériter de toutes les propriétés de style (défaut)
Button1.StyledSettings := [TStyledSetting.Family,
                           TStyledSetting.Size,
                           TStyledSetting.FontColor,
                           TStyledSetting.Style];

// Ne pas hériter de la couleur de police
Button1.StyledSettings := Button1.StyledSettings - [TStyledSetting.FontColor];  
Button1.TextSettings.FontColor := TAlphaColors.Red;  // Couleur personnalisée  

// Ignorer complètement le style
Button1.StyledSettings := [];
```

## 11. Bonnes pratiques pour les styles

### ✅ À FAIRE

**1. Utiliser les styles natifs par défaut**
```pascal
// Laissez FireMonkey choisir le style adapté à la plateforme
// Pas besoin de code spécial
```

**2. Créer un style personnalisé pour l'identité de marque**
```pascal
// Si vous avez une charte graphique spécifique
StyleBook1.LoadFromFile('StyleEntreprise.style');
```

**3. Prévoir thème clair ET sombre**
```pascal
// Les utilisateurs apprécient d'avoir le choix
if PrefereSombre then
  ChargerStyleSombre
else
  ChargerStyleClair;
```

**4. Tester sur toutes les plateformes cibles**
```pascal
// Un style peut apparaître différemment selon la plateforme
// Testez iOS, Android, Windows, macOS
```

**5. Utiliser des couleurs de la charte graphique**
```pascal
// Définir des constantes pour les couleurs de la marque
const
  COULEUR_PRINCIPALE = $FF0066CC;
  COULEUR_SECONDAIRE = $FF00CC66;
  COULEUR_TEXTE = $FF333333;
```

**6. Maintenir la cohérence**
```pascal
// Tous les boutons principaux doivent avoir le même style
// Tous les boutons secondaires aussi
```

### ❌ À ÉVITER

**1. Abuser des effets visuels**
```pascal
// ❌ Trop d'ombres, de lueurs, de flous
// Résultat : Interface surchargée et lente
```

**2. Ignorer les standards de la plateforme**
```pascal
// ❌ Interface iOS sur Android ou vice-versa
// Les utilisateurs sont déroutés
```

**3. Texte illisible**
```pascal
// ❌ Contraste insuffisant
Label1.TextSettings.FontColor := TAlphaColors.Lightgray;  // Sur fond blanc
```

**4. Styles trop complexes**
```pascal
// ❌ Styles avec des dizaines d'états et d'animations
// Difficile à maintenir
```

**5. Négliger l'accessibilité**
```pascal
// ❌ Couleurs seules pour transmettre l'information
// ❌ Texte trop petit
// ❌ Zones tactiles trop petites
```

## 12. Styles et accessibilité

### Contraste suffisant

Assurez-vous d'avoir un contraste suffisant entre le texte et l'arrière-plan :

**Ratios recommandés** :
- Texte normal : Au moins 4.5:1
- Texte large (18pt+) : Au moins 3:1

```pascal
// BON : Contraste élevé
Panel1.Fill.Color := TAlphaColors.White;  
Label1.TextSettings.FontColor := TAlphaColors.Black;  

// MAUVAIS : Contraste faible
Panel1.Fill.Color := TAlphaColors.Lightgray;  
Label1.TextSettings.FontColor := TAlphaColors.White;  
```

### Taille de texte adaptée

```pascal
{$IFDEF ANDROID OR IOS}
  // Minimum 14-16pt sur mobile
  Label1.TextSettings.Font.Size := 16;
{$ELSE}
  // Minimum 10-12pt sur desktop
  Label1.TextSettings.Font.Size := 11;
{$ENDIF}
```

### Ne pas se fier uniquement à la couleur

```pascal
// ❌ MAUVAIS : Couleur seule
ButtonDanger.Fill.Color := TAlphaColors.Red;

// ✅ BON : Couleur + icône + texte
ButtonDanger.Fill.Color := TAlphaColors.Red;  
ButtonDanger.Text := '⚠ Supprimer';  // Icône + texte explicite  
```

## 13. Exemples de palettes de couleurs

### Palette moderne

```pascal
const
  // Couleurs principales
  PRIMARY = $FF2196F3;      // Bleu
  SECONDARY = $FFFF9800;    // Orange

  // États
  SUCCESS = $FF4CAF50;      // Vert
  WARNING = $FFFFC107;      // Jaune/Orange
  DANGER = $FFF44336;       // Rouge
  INFO = $FF00BCD4;         // Cyan

  // Neutres
  GRIS_FONCE = $FF212121;
  GRIS_MOYEN = $FF757575;
  GRIS_CLAIR = $FFBDBDBD;
  GRIS_TRES_CLAIR = $FFF5F5F5;
```

### Palette mode sombre

```pascal
const
  // Mode sombre
  DARK_BG = $FF121212;         // Fond principal
  DARK_SURFACE = $FF1E1E1E;    // Surfaces (cards, panels)
  DARK_PRIMARY = $FFBB86FC;    // Couleur primaire
  DARK_SECONDARY = $FF03DAC6;  // Couleur secondaire
  DARK_TEXT = $FFFFFFFF;       // Texte principal
  DARK_TEXT_SECONDARY = $FFB3B3B3;  // Texte secondaire
```

## 14. Ressources et outils

### Générateurs de palettes

Pour créer des palettes harmonieuses :
- Adobe Color
- Coolors.co
- Material Design Color Tool

### Styles prêts à l'emploi

De nombreux styles FireMonkey sont disponibles en ligne :
- Styles sur le forum Delphi
- Styles commerciaux (TMS, DevExpress)
- Styles open source sur GitHub

### Documentation officielle

```
Embarcadero DocWiki :
- FireMonkey Styles
- Style Designer
- Creating Custom Styles
```

## Conclusion

Le système de styles de FireMonkey est l'un de ses atouts majeurs. Il vous permet de :

🎨 **Créer des interfaces modernes** et attrayantes

🎨 **Maintenir une cohérence visuelle** sur toutes les plateformes

🎨 **Changer l'apparence facilement** sans modifier le code

🎨 **S'adapter aux préférences utilisateur** (thème clair/sombre)

🎨 **Différencier votre application** avec une identité visuelle unique

Les points clés à retenir :

✅ Les styles définissent l'apparence de tous les composants

✅ TStyleBook charge et applique les styles

✅ StyleLookup permet de varier l'apparence de composants individuels

✅ Vous pouvez créer des styles personnalisés avec l'éditeur de styles

✅ Les effets visuels enrichissent l'interface (avec modération)

✅ Toujours penser accessibilité (contraste, taille de texte)

Dans la section suivante, nous verrons comment adapter votre interface aux différentes tailles d'écran et résolutions, en complément de ce que vous avez appris sur les styles.

⏭️ [Adaptation aux différentes tailles d'écran](/05-developpement-multi-plateforme-avec-firemonkey/05-adaptation-aux-differentes-tailles-decran.md)
