# 5.2 Différences entre VCL et FMX

Si vous avez déjà travaillé avec Delphi, vous êtes probablement familier avec la VCL (Visual Component Library). Maintenant que vous découvrez FireMonkey (FMX), il est important de comprendre les principales différences entre ces deux bibliothèques d'interface utilisateur. Cette section vous aidera à faire la transition en douceur de l'une à l'autre.

## Origine et philosophie

**VCL (Visual Component Library)** :
- Créée avec les premières versions de Delphi dans les années 1990
- Conçue spécifiquement pour Windows
- Utilise les contrôles natifs Windows (boutons, listes, etc.)
- Optimisée pour les applications de bureau Windows

**FireMonkey (FMX)** :
- Introduite avec Delphi XE2 en 2011
- Conçue dès le départ pour être multi-plateforme
- Dessine ses propres contrôles (ne dépend pas des contrôles natifs)
- Optimisée pour la portabilité et les interfaces visuelles modernes

## Principales différences techniques

### 1. Structure des unités et espaces de noms

**VCL** utilise des unités préfixées par `Vcl.` :
```pascal
uses
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Dialogs;
```

**FMX** utilise des unités préfixées par `FMX.` :
```pascal
uses
  FMX.Forms, FMX.Controls, FMX.StdCtrls, FMX.Dialogs;
```

### 2. Extensions de fichiers

**VCL** :
- Fichiers projet : `.dpr`
- Fichiers formulaire : `.dfm`

**FMX** :
- Fichiers projet : `.dpr` (identique à VCL)
- Fichiers formulaire : `.fmx`

### 3. Système de coordonnées et positionnement

**VCL** :
- Utilise les pixels comme unité de base
- Positionnement basé sur des coordonnées absolues en pixels
- Mise à l'échelle manuelle pour les écrans haute résolution

**FMX** :
- Utilise un système de coordonnées indépendant de la résolution
- Mise à l'échelle automatique pour différentes résolutions d'écran
- Propriétés `Position.X` et `Position.Y` au lieu de `Left` et `Top`
- Propriétés `Size.Width` et `Size.Height` au lieu de `Width` et `Height`

Exemple VCL :
```pascal
Button1.Left := 20;
Button1.Top := 30;
Button1.Width := 100;
Button1.Height := 30;
```

Exemple FMX équivalent :
```pascal
Button1.Position.X := 20;
Button1.Position.Y := 30;
Button1.Size.Width := 100;
Button1.Size.Height := 30;
```

### 4. Propriétés et événements

**VCL** :
- Propriétés directes pour la plupart des attributs
- Événements similaires dans toute la bibliothèque

**FMX** :
- Organisation hiérarchique des propriétés (par exemple, `TextSettings.Font.Size` au lieu de `Font.Size`)
- Certains événements sont différents ou organisés différemment

Exemple de propriété de texte :

VCL :
```pascal
Label1.Font.Size := 14;
Label1.Font.Color := clRed;
```

FMX :
```pascal
Label1.TextSettings.Font.Size := 14;
Label1.TextSettings.FontColor := TAlphaColors.Red;
```

### 5. Couleurs et transparence

**VCL** :
- Utilise le type `TColor` et les constantes comme `clRed`, `clBlue`
- Support limité de la transparence

**FMX** :
- Utilise le type `TAlphaColor` qui inclut le canal alpha (transparence)
- Constantes de couleur comme `TAlphaColors.Red`, `TAlphaColors.Blue`
- Support complet de la transparence et des dégradés

Exemple :

VCL :
```pascal
Shape1.Brush.Color := clRed;
```

FMX :
```pascal
Rectangle1.Fill.Color := TAlphaColors.Red;
Rectangle1.Opacity := 0.7; // 70% d'opacité
```

### 6. Styles et thèmes

**VCL** :
- Utilise les styles Windows ou des styles personnalisés
- Appliqués au niveau de l'application

**FMX** :
- Système de styles beaucoup plus flexible
- Styles par plateforme (Windows, macOS, iOS, Android)
- Peut être appliqué au niveau de l'application, du formulaire ou du composant
- Utilise des StyleBooks pour organiser les styles

### 7. Conteneurs et alignement

**VCL** :
- Alignement basique (alTop, alBottom, alClient, etc.)
- Ancrage pour le redimensionnement

**FMX** :
- Alignement avancé avec marges et remplissage
- Conteneurs spécialisés comme TLayout, TGridLayout, TFlowLayout
- Adaptatif pour différentes tailles d'écran

Exemple FMX :
```pascal
Button1.Align := TAlignLayout.Top;
Button1.Margins.Left := 10;
Button1.Margins.Top := 5;
```

### 8. Composants et contrôles spécifiques

**VCL** :
- TButton, TEdit, TListBox, etc.
- Composants spécifiques à Windows comme TRichEdit

**FMX** :
- TButton, TEdit, TListBox (noms similaires mais fonctionnement différent)
- Composants multi-plateformes comme TMultiView pour les menus de navigation mobile
- Composants spécifiques à la plateforme comme TPresentedScrollBox

### 9. Support graphique et animation

**VCL** :
- Support graphique 2D basique via TCanvas
- Animations limitées

**FMX** :
- Moteur graphique avancé avec support 2D et 3D
- Animations intégrées et transitions fluides
- Effets visuels comme les ombres, flous, réflexions

## Composants équivalents courants

| Fonction | VCL | FireMonkey |
|----------|-----|------------|
| Formulaire | TForm | TForm |
| Bouton | TButton | TButton |
| Champ de texte | TEdit | TEdit |
| Zone de texte multi-lignes | TMemo | TMemo |
| Liste déroulante | TComboBox | TComboBox |
| Case à cocher | TCheckBox | TCheckBox |
| Bouton radio | TRadioButton | TRadioButton |
| Étiquette | TLabel | TLabel |
| Boîte de dialogue | MessageDlg() | MessageDlg() |
| Image | TImage | TImage |
| Forme | TShape | TRectangle, TCircle, etc. |
| Panneau | TPanel | TPanel ou TRectangle |
| Barre de progression | TProgressBar | TProgressBar |

## Conseils pour la transition de VCL vers FMX

1. **Commencez petit** : Créez d'abord des applications simples pour vous familiariser avec les différences.

2. **Attention aux noms similaires** : Bien que de nombreux composants portent le même nom, leurs propriétés et comportements peuvent être différents.

3. **Utilisez l'IDE pour explorer** : L'inspecteur d'objets de Delphi est votre ami pour découvrir les propriétés disponibles.

4. **Pensez "multi-plateforme"** : Évitez d'utiliser des fonctionnalités spécifiques à Windows, sauf si vous utilisez une vérification de plateforme.

5. **Adaptez votre code** : Utilisez des instructions conditionnelles pour gérer les différences entre plateformes :

```pascal
{$IFDEF MSWINDOWS}
  // Code spécifique à Windows
{$ELSE}
  {$IFDEF ANDROID}
    // Code spécifique à Android
  {$ELSE}
    // Code pour les autres plateformes
  {$ENDIF}
{$ENDIF}
```

6. **Apprenez le système de styles** : Les styles FMX sont puissants mais différents du système VCL.

7. **N'essayez pas de convertir directement** : La conversion directe d'une application VCL complexe vers FMX peut être difficile. Envisagez plutôt une refonte progressive.

## Quand choisir VCL ou FMX ?

**Choisissez VCL si** :
- Vous développez exclusivement pour Windows
- Vous avez besoin des contrôles Windows natifs spécifiques
- La performance sur Windows est critique
- Vous maintenez une application VCL existante
- Vous avez besoin d'intégrations système Windows profondes

**Choisissez FMX si** :
- Vous devez cibler plusieurs plateformes
- Vous développez des applications mobiles (iOS/Android)
- Vous voulez une interface utilisateur moderne avec animations
- Vous préférez un aspect visuel cohérent sur toutes les plateformes
- Vous débutez un nouveau projet sans héritage existant

## Conclusion

Comprendre les différences entre VCL et FireMonkey est essentiel pour tirer le meilleur parti de ces deux bibliothèques. FMX n'est pas simplement une version multi-plateforme de la VCL, mais une approche différente de la création d'interfaces utilisateur. Avec cette connaissance, vous pourrez choisir la bibliothèque la plus adaptée à votre projet et éviter les pièges courants lors de la transition de l'une à l'autre.

Dans les sections suivantes, nous explorerons en détail comment créer des interfaces utilisateur attrayantes et fonctionnelles avec FireMonkey.
