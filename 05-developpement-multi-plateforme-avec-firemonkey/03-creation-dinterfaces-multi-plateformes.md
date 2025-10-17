🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 5.3 Création d'interfaces multi-plateformes

## Introduction

Créer une interface qui fonctionne parfaitement sur Windows, macOS, iOS, Android et Linux n'est pas simplement une question de compilation. C'est une approche de conception spécifique qui nécessite de penser différemment dès le départ. Dans cette section, nous allons découvrir les principes, les techniques et les bonnes pratiques pour créer des interfaces véritablement multi-plateformes avec FireMonkey.

## 1. Les principes fondamentaux du design multi-plateforme

### Penser "adaptatif" plutôt que "fixe"

**Ancienne approche (VCL classique)** :
- Concevoir pour une résolution fixe (par exemple 1024x768)
- Placer les composants à des positions absolues
- Espérer que l'utilisateur ne change pas la taille de la fenêtre

**Approche multi-plateforme (FMX)** :
- Concevoir pour des tailles d'écran variables (de 4 pouces à 27 pouces)
- Utiliser des dispositions relatives et des layouts
- Prévoir que l'interface s'adaptera automatiquement

### Le concept de "responsive design"

Le "responsive design" (design adaptatif) signifie que votre interface :

✅ **S'adapte à la taille de l'écran** : Fonctionne sur un smartphone 5 pouces comme sur un écran desktop 24 pouces

✅ **Gère les orientations** : Portrait et paysage sans problème

✅ **Reste utilisable** : Les boutons restent accessibles, le texte reste lisible

✅ **Optimise l'espace** : Utilise intelligemment l'espace disponible

**Exemple concret** :
Imaginez une application de prise de notes :
- Sur smartphone : Une seule colonne, liste des notes en plein écran
- Sur tablette : Deux colonnes, liste à gauche, contenu à droite
- Sur desktop : Trois colonnes, catégories / liste / contenu

C'est le même code, mais l'interface s'adapte !

### Les trois règles d'or

**Règle 1 : Ne jamais utiliser de positions absolues**
```pascal
// ❌ MAUVAIS : Position fixe
Button1.Position.X := 100;
Button1.Position.Y := 200;

// ✅ BON : Alignement relatif
Button1.Align := TAlignLayout.Bottom;
Button1.Margins.Bottom := 10;
```

**Règle 2 : Toujours tester sur plusieurs tailles d'écran**
Ne vous contentez pas de tester sur votre écran de développement. Utilisez les outils de prévisualisation de l'IDE pour voir comment votre interface se comporte.

**Règle 3 : Privilégier les layouts aux positions manuelles**
Les layouts (TLayout, TGridLayout, TFlowLayout) sont vos meilleurs amis pour créer des interfaces adaptatives.

## 2. Les composants de mise en page (Layouts)

### TLayout - Le conteneur universel

**TLayout** est un conteneur invisible qui sert à organiser d'autres composants.

**Caractéristiques** :
- Invisible à l'exécution (ne se dessine pas)
- Peut contenir d'autres composants
- Peut avoir des propriétés d'alignement
- Peut être imbriqué dans d'autres layouts

**Utilisation typique** :
```pascal
// Créer une zone d'en-tête
HeaderLayout := TLayout.Create(Form1);
HeaderLayout.Parent := Form1;
HeaderLayout.Align := TAlignLayout.Top;
HeaderLayout.Height := 60;

// Créer une zone de contenu
ContentLayout := TLayout.Create(Form1);
ContentLayout.Parent := Form1;
ContentLayout.Align := TAlignLayout.Client; // Prend tout l'espace restant

// Créer une zone de pied de page
FooterLayout := TLayout.Create(Form1);
FooterLayout.Parent := Form1;
FooterLayout.Align := TAlignLayout.Bottom;
FooterLayout.Height := 40;
```

**Résultat** : Une structure classique en trois parties qui s'adapte à toutes les tailles d'écran.

### TGridLayout - Disposition en grille

**TGridLayout** organise les composants en lignes et colonnes, comme un tableau.

**Propriétés importantes** :
- `ColumnCount` : Nombre de colonnes
- `RowCount` : Nombre de lignes
- `ItemWidth` : Largeur des cellules
- `ItemHeight` : Hauteur des cellules
- `Orientation` : Horizontal ou Vertical (ordre de remplissage)

**Exemple d'utilisation** :
Créer une grille de 3x3 boutons :
```pascal
GridLayout1.ColumnCount := 3;
GridLayout1.RowCount := 3;
GridLayout1.ItemWidth := 100;
GridLayout1.ItemHeight := 100;
GridLayout1.Orientation := TOrientation.Horizontal;

// Ajouter 9 boutons - ils se placeront automatiquement dans la grille
for i := 1 to 9 do
begin
  Button := TButton.Create(GridLayout1);
  Button.Parent := GridLayout1;
  Button.Text := IntToStr(i);
end;
```

**Cas d'usage** :
- Calculatrices
- Claviers virtuels
- Galeries d'images
- Tableaux de bord avec tuiles

### TFlowLayout - Disposition fluide

**TFlowLayout** dispose les composants les uns après les autres, comme du texte qui se "wrappe" (retour à la ligne automatique).

**Comportement** :
- Ajoute les composants horizontalement jusqu'à manquer d'espace
- Passe à la ligne suivante automatiquement
- S'adapte à la largeur disponible

**Utilisation typique** :
```pascal
FlowLayout1.Justify := TFlowJustify.Left;
FlowLayout1.HorizontalGap := 10;
FlowLayout1.VerticalGap := 10;

// Ajouter plusieurs tags/badges
for i := 1 to 20 do
begin
  Tag := TButton.Create(FlowLayout1);
  Tag.Parent := FlowLayout1;
  Tag.Text := 'Tag ' + IntToStr(i);
  Tag.Width := 80;
  Tag.Height := 30;
end;
```

**Cas d'usage** :
- Tags ou badges
- Barres d'outils adaptatives
- Listes de chips (comme les destinataires d'un email)
- Menus qui s'adaptent à la largeur

### TScrollBox - Zone défilante

**TScrollBox** crée une zone avec barres de défilement automatiques si le contenu déborde.

**Propriétés importantes** :
- `ShowScrollBars` : Toujours visible, automatique, ou jamais
- `BounceAnimation` : Effet de rebond (mobile)
- `AniCalculations` : Animations de défilement

**Utilisation** :
```pascal
ScrollBox1.Align := TAlignLayout.Client;
ScrollBox1.ShowScrollBars := False; // Masqué sur mobile, gestuel

// Ajouter du contenu qui peut dépasser
ContentLayout := TLayout.Create(ScrollBox1);
ContentLayout.Parent := ScrollBox1;
ContentLayout.Height := 2000; // Plus haut que l'écran
```

**Important sur mobile** : Sur iOS et Android, il est recommandé de masquer les barres de défilement et de laisser l'utilisateur faire défiler avec ses doigts.

### TVertScrollBox - Défilement vertical optimisé

Version optimisée de TScrollBox pour le défilement vertical uniquement (plus performant sur mobile).

```pascal
VertScrollBox1.Align := TAlignLayout.Client;
// Le contenu s'étend automatiquement en largeur
// Défile verticalement si nécessaire
```

## 3. Propriétés d'alignement (Align)

La propriété `Align` est cruciale pour créer des interfaces adaptatives.

### Valeurs principales

**TAlignLayout.None** : Pas d'alignement automatique (position manuelle)

**TAlignLayout.Top** : S'aligne en haut, s'étend sur toute la largeur
```pascal
Toolbar.Align := TAlignLayout.Top;
Toolbar.Height := 50; // Définir la hauteur
```

**TAlignLayout.Bottom** : S'aligne en bas, s'étend sur toute la largeur
```pascal
StatusBar.Align := TAlignLayout.Bottom;
StatusBar.Height := 30;
```

**TAlignLayout.Left** : S'aligne à gauche, s'étend sur toute la hauteur
```pascal
SidePanel.Align := TAlignLayout.Left;
SidePanel.Width := 200;
```

**TAlignLayout.Right** : S'aligne à droite, s'étend sur toute la hauteur
```pascal
PropertiesPanel.Align := TAlignLayout.Right;
PropertiesPanel.Width := 250;
```

**TAlignLayout.Client** : Occupe tout l'espace restant
```pascal
MainContent.Align := TAlignLayout.Client;
// Pas besoin de spécifier Width/Height
```

**TAlignLayout.Contents** : S'adapte à son contenu
```pascal
Panel1.Align := TAlignLayout.Contents;
// La taille s'ajuste automatiquement
```

### Ordre d'alignement

L'ordre dans lequel vous créez les composants alignés est important :

```pascal
// 1. D'abord la barre d'outils en haut
ToolBar.Align := TAlignLayout.Top;

// 2. Puis le panneau de gauche
LeftPanel.Align := TAlignLayout.Left;

// 3. Puis le panneau de droite
RightPanel.Align := TAlignLayout.Right;

// 4. Enfin le contenu central
MainArea.Align := TAlignLayout.Client;
```

**Résultat** : La toolbar occupe toute la largeur en haut, puis les panneaux latéraux se positionnent dans l'espace restant, et enfin le contenu central occupe ce qui reste.

## 4. Marges et espacement (Margins et Padding)

### Margins - Marges externes

Les marges créent un espace **autour** du composant, entre lui et son conteneur ou ses voisins.

```pascal
Button1.Margins.Left := 10;
Button1.Margins.Top := 10;
Button1.Margins.Right := 10;
Button1.Margins.Bottom := 10;

// Ou en une seule fois
Button1.Margins.Rect := TRectF.Create(10, 10, 10, 10);
```

**Utilisation typique** :
```pascal
Panel1.Align := TAlignLayout.Client;
Panel1.Margins.Left := 20;
Panel1.Margins.Top := 20;
Panel1.Margins.Right := 20;
Panel1.Margins.Bottom := 20;
// Le panel occupe tout l'espace client mais avec 20 pixels de marge
```

### Padding - Marges internes

Le padding crée un espace **à l'intérieur** du composant, entre ses bords et son contenu.

```pascal
Panel1.Padding.Left := 15;
Panel1.Padding.Top := 15;
Panel1.Padding.Right := 15;
Panel1.Padding.Bottom := 15;
// Les composants enfants auront 15 pixels d'espace par rapport aux bords
```

### Différence Margins vs Padding

**Margins** (marges externes) :
- Espace entre le composant et l'extérieur
- Transparent (laisse voir le fond)
- N'affecte pas le contenu du composant

**Padding** (marges internes) :
- Espace entre le bord du composant et son contenu
- À l'intérieur de la zone du composant
- Affecte la position du contenu

**Exemple visuel** :
```
┌─────────────────────────────────┐
│        Margin (externe)         │
│  ┌───────────────────────────┐  │
│  │    Padding (interne)      │  │
│  │  ┌─────────────────────┐  │  │
│  │  │     Contenu         │  │  │
│  │  └─────────────────────┘  │  │
│  └───────────────────────────┘  │
└─────────────────────────────────┘
```

## 5. Ancrage (Anchors)

Les ancrages permettent de définir comment un composant se comporte quand son conteneur change de taille.

### Principe

La propriété `Anchors` contient un ensemble de valeurs :
- `akLeft` : Ancré au bord gauche
- `akTop` : Ancré au bord haut
- `akRight` : Ancré au bord droit
- `akBottom` : Ancré au bord bas

### Comportements typiques

**Par défaut** : `[akLeft, akTop]`
```pascal
Button1.Anchors := [akLeft, akTop];
// Le bouton reste à distance fixe du coin supérieur gauche
// Sa taille ne change pas
```

**S'étirer horizontalement** : `[akLeft, akTop, akRight]`
```pascal
Edit1.Anchors := [akLeft, akTop, akRight];
// Le champ s'étire en largeur quand la fenêtre s'élargit
```

**S'étirer verticalement** : `[akLeft, akTop, akBottom]`
```pascal
Memo1.Anchors := [akLeft, akTop, akBottom];
// Le memo s'étire en hauteur quand la fenêtre s'agrandit
```

**S'étirer dans les deux sens** : `[akLeft, akTop, akRight, akBottom]`
```pascal
Panel1.Anchors := [akLeft, akTop, akRight, akBottom];
// Le panel s'étire dans les deux dimensions
```

**Rester en bas à droite** : `[akRight, akBottom]`
```pascal
ButtonOK.Anchors := [akRight, akBottom];
ButtonOK.Position.X := Form1.Width - ButtonOK.Width - 10;
ButtonOK.Position.Y := Form1.Height - ButtonOK.Height - 10;
// Le bouton reste toujours en bas à droite
```

### Combinaison Align et Anchors

Vous pouvez combiner `Align` et `Anchors` pour des effets sophistiqués, mais dans la plupart des cas, utilisez soit l'un soit l'autre pour éviter les conflits.

## 6. Gestion des différentes tailles d'écran

### Comprendre les résolutions et DPI

**DPI (Dots Per Inch)** : Nombre de pixels par pouce

Les écrans modernes ont des DPI très variés :
- Écran PC standard : ~96 DPI
- Écran Mac Retina : ~220 DPI
- iPhone : ~326 DPI (Retina)
- iPad : ~264 DPI
- Écran 4K : ~160 DPI

**Important** : FireMonkey gère automatiquement les DPI ! Un bouton de 100 pixels de large occupera la même taille physique sur tous les écrans, même si le nombre de pixels réels utilisés diffère.

### Tailles d'écran typiques

**Smartphones** :
- Petit : 4-5 pouces (320x568 à 375x667 pixels)
- Standard : 5-6 pouces (375x667 à 414x896 pixels)
- Grand : 6+ pouces (414x896+ pixels)

**Tablettes** :
- iPad : 768x1024 (portrait) ou 1024x768 (paysage)
- Tablettes Android : Très variable

**Desktop** :
- Minimum : 1280x720
- Standard : 1920x1080
- Large : 2560x1440+

### Stratégies d'adaptation

#### Stratégie 1 : Layout adaptatif unique

Créer une interface qui s'adapte à toutes les tailles avec des layouts intelligents.

**Exemple** :
```pascal
// Structure qui fonctionne sur toutes les tailles
MainLayout.Align := TAlignLayout.Client;
MainLayout.Padding.Rect := TRectF.Create(10, 10, 10, 10);

// Utiliser TFlowLayout pour que les éléments s'adaptent
FlowLayout1.Parent := MainLayout;
FlowLayout1.Align := TAlignLayout.Top;
```

**Avantages** :
- Un seul design à maintenir
- Fonctionne automatiquement sur toutes les plateformes

**Inconvénients** :
- Compromis nécessaires
- Peut ne pas être optimal sur toutes les tailles

#### Stratégie 2 : Vues multiples avec MultiView

Créer différentes dispositions selon la taille d'écran disponible.

**Principe** :
```pascal
if Form1.Width < 600 then
begin
  // Disposition mobile : une colonne
  SidePanel.Visible := False;
  MainContent.Align := TAlignLayout.Client;
end
else
begin
  // Disposition desktop : deux colonnes
  SidePanel.Visible := True;
  SidePanel.Width := 250;
  MainContent.Align := TAlignLayout.Client;
end;
```

**Avantages** :
- Interface optimale pour chaque taille
- Meilleure expérience utilisateur

**Inconvénients** :
- Plus de code à maintenir
- Gestion de plusieurs layouts

#### Stratégie 3 : Composants différents par plateforme

Utiliser des vues complètement différentes pour mobile et desktop.

```pascal
{$IFDEF ANDROID OR IOS}
  // Interface mobile avec TTabControl
  TabControl1.Visible := True;
  DesktopLayout.Visible := False;
{$ELSE}
  // Interface desktop classique
  TabControl1.Visible := False;
  DesktopLayout.Visible := True;
{$ENDIF}
```

## 7. Gestion des orientations (Portrait / Paysage)

Sur mobile et tablette, l'utilisateur peut faire pivoter l'appareil.

### Détecter le changement d'orientation

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  if Width > Height then
  begin
    // Mode paysage (Landscape)
    ShowMessage('Mode paysage');
  end
  else
  begin
    // Mode portrait
    ShowMessage('Mode portrait');
  end;
end;
```

### Adapter l'interface selon l'orientation

**Exemple pratique** : Application de galerie photos

```pascal
procedure TForm1.AdaptToOrientation;
begin
  if Width > Height then
  begin
    // Paysage : Plus d'images par ligne
    GridLayout1.ColumnCount := 4;
  end
  else
  begin
    // Portrait : Moins d'images par ligne
    GridLayout1.ColumnCount := 2;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  AdaptToOrientation;
end;
```

### Verrouiller l'orientation

Parfois, vous voulez forcer une orientation :

```pascal
// Portrait uniquement
ScreenService := TScreenOrientations.Portrait;

// Paysage uniquement
ScreenService := TScreenOrientations.Landscape;

// Permettre les deux
ScreenService := TScreenOrientations.Portrait + TScreenOrientations.Landscape;
```

## 8. Composant TMultiView - Menus latéraux

Le composant `TMultiView` est essentiel pour créer des interfaces modernes avec menus latéraux (drawer, hamburger menu).

### Principe

**TMultiView** crée un panneau qui peut :
- Se cacher/afficher à la demande
- Coulisser depuis un bord de l'écran
- Se superposer au contenu ou le pousser

### Configuration basique

```pascal
MultiView1.Mode := TMultiViewMode.Drawer; // Menu type "drawer"
MultiView1.DrawerOptions := [TDrawerOption.ShowAnimation];
MultiView1.MasterButton := ButtonMenu; // Bouton qui ouvre/ferme le menu

// Contenu du menu
ListBox1.Parent := MultiView1;
// ... ajouter des items
```

### Modes disponibles

**Drawer** : Menu qui coulisse depuis le côté
- Typique sur Android
- Se superpose au contenu

**PlatformBehaviour** : S'adapte à la plateforme
- Drawer sur mobile
- Panel fixe sur desktop

**Panel** : Panneau classique
- Toujours visible sur desktop
- Peut se cacher sur mobile

**Popover** : Fenêtre flottante
- Style iOS
- Se positionne près du bouton

### Exemple d'utilisation

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du MultiView
  MultiView1.Width := 250;
  MultiView1.Mode := TMultiViewMode.Drawer;
  MultiView1.MasterButton := ButtonMenu;

  // Ajouter des éléments au menu
  ListBox1.Parent := MultiView1;
  ListBox1.Align := TAlignLayout.Client;

  // Items de menu
  ListBox1.Items.Add('Accueil');
  ListBox1.Items.Add('Paramètres');
  ListBox1.Items.Add('À propos');
end;

procedure TForm1.ListBox1ItemClick(Sender: TCustomListBox; const Item: TListBoxItem);
begin
  // Gérer le clic sur un item
  case ListBox1.ItemIndex of
    0: ShowHome;
    1: ShowSettings;
    2: ShowAbout;
  end;

  // Fermer le menu
  MultiView1.HideMaster;
end;
```

## 9. Contrôles adaptatifs

### Tailles de boutons adaptées au tactile

Sur mobile, les zones tactiles doivent être suffisamment grandes (minimum 44x44 pixels).

```pascal
{$IFDEF ANDROID OR IOS}
  Button1.Height := 44; // Taille minimale tactile
  Button1.Width := 120;
{$ELSE}
  Button1.Height := 25; // Taille desktop classique
  Button1.Width := 80;
{$ENDIF}
```

### Espacement adapté

```pascal
{$IFDEF ANDROID OR IOS}
  FlowLayout1.HorizontalGap := 15; // Plus d'espace sur mobile
  FlowLayout1.VerticalGap := 15;
{$ELSE}
  FlowLayout1.HorizontalGap := 5;  // Moins d'espace sur desktop
  FlowLayout1.VerticalGap := 5;
{$ENDIF}
```

### Taille de police adaptée

```pascal
{$IFDEF ANDROID OR IOS}
  Label1.TextSettings.Font.Size := 16; // Police plus grande sur mobile
{$ELSE}
  Label1.TextSettings.Font.Size := 11; // Police standard desktop
{$ENDIF}
```

## 10. Patterns d'interface multi-plateforme

### Pattern 1 : Master-Detail adaptatif

**Sur tablette/desktop** : Deux panneaux côte à côte
**Sur smartphone** : Navigation entre deux écrans

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  if Width >= 768 then
  begin
    // Mode tablette/desktop : Vue côte à côte
    MasterPanel.Visible := True;
    MasterPanel.Width := 300;
    DetailPanel.Visible := True;
  end
  else
  begin
    // Mode smartphone : Navigation
    if ShowingDetail then
    begin
      MasterPanel.Visible := False;
      DetailPanel.Visible := True;
    end
    else
    begin
      MasterPanel.Visible := True;
      DetailPanel.Visible := False;
    end;
  end;
end;
```

### Pattern 2 : Tabs vs Menu

**Sur mobile** : Onglets en bas (style iOS) ou menu hamburger
**Sur desktop** : Onglets en haut ou menu latéral permanent

```pascal
{$IFDEF ANDROID OR IOS}
  TabControl1.TabPosition := TTabPosition.Bottom;
  TabControl1.Visible := True;
  SideMenu.Visible := False;
{$ELSE}
  TabControl1.Visible := False;
  SideMenu.Visible := True;
  SideMenu.Width := 200;
{$ENDIF}
```

### Pattern 3 : Formulaires adaptatifs

Adapter le nombre de colonnes selon la largeur disponible.

```pascal
procedure TForm1.ArrangeFormFields;
begin
  if Width < 600 then
  begin
    // Mobile : Une colonne
    FieldsLayout.ColumnCount := 1;
  end
  else if Width < 1200 then
  begin
    // Tablette : Deux colonnes
    FieldsLayout.ColumnCount := 2;
  end
  else
  begin
    // Desktop : Trois colonnes
    FieldsLayout.ColumnCount := 3;
  end;
end;
```

## 11. Bonnes pratiques

### ✅ À FAIRE

1. **Tester régulièrement sur plusieurs tailles**
   - Utilisez les outils de prévisualisation de l'IDE
   - Testez sur des appareils réels

2. **Utiliser des layouts plutôt que des positions fixes**
   - TLayout, TGridLayout, TFlowLayout sont vos amis
   - Évitez Position.X et Position.Y autant que possible

3. **Penser "mobile d'abord"**
   - Concevez pour la contrainte la plus forte (petit écran)
   - Enrichissez ensuite pour les grands écrans

4. **Respecter les standards de chaque plateforme**
   - Boutons en bas sur iOS
   - Menus hamburger sur Android
   - Barres de menu classiques sur desktop

5. **Prévoir des marges suffisantes**
   - Ne collez pas les composants aux bords
   - Laissez respirer l'interface

6. **Utiliser des tailles relatives quand possible**
   - Pourcentages plutôt que pixels fixes
   - Proportions plutôt que valeurs absolues

### ❌ À ÉVITER

1. **Positions absolues en pixels**
   - Fragile face aux changements de taille
   - Ne s'adapte pas

2. **Ignorer les orientations**
   - Toujours penser portrait ET paysage
   - Tester les deux

3. **Textes trop petits sur mobile**
   - Minimum 14-16px pour la lisibilité
   - Pensez aux utilisateurs avec des difficultés visuelles

4. **Boutons trop petits pour le tactile**
   - Minimum 44x44 pixels
   - Laissez de l'espace entre les zones tactiles

5. **Trop de contenu sur petit écran**
   - Simplifiez l'interface mobile
   - Hiérarchisez l'information

6. **Négliger les performances sur mobile**
   - Évitez trop d'animations simultanées
   - Optimisez les images

## 12. Outils de l'IDE pour le multi-plateforme

### Prévisualisation des plateformes

Dans l'IDE Delphi, vous pouvez :
- Changer la taille de prévisualisation
- Simuler différents appareils (iPhone, iPad, Android)
- Tester les orientations

**Utilisation** :
1. Dans le concepteur de formulaires, regardez en haut
2. Sélectionnez une plateforme cible (iPhone, Android, etc.)
3. L'aperçu s'adapte automatiquement

### Vues par plateforme

Vous pouvez créer des vues spécifiques par plateforme :

1. Clic droit sur un formulaire
2. "Add View" → Choisir la plateforme
3. Personnaliser pour cette plateforme

**Important** : À utiliser avec parcimonie, car cela multiplie le travail de maintenance.

### Débogage multi-plateforme

L'IDE permet de déboguer sur :
- Windows local
- macOS (via connexion réseau)
- iOS (via Mac et câble)
- Android (via USB ou émulateur)

## Conclusion

La création d'interfaces multi-plateformes demande une approche différente de la conception traditionnelle desktop. Les principes clés à retenir sont :

🎯 **Flexibilité** : Utilisez des layouts et des alignements plutôt que des positions fixes

🎯 **Adaptation** : Testez sur différentes tailles et orientations dès le début

🎯 **Simplicité** : Commencez simple, enrichissez progressivement

🎯 **Standards** : Respectez les conventions de chaque plateforme

🎯 **Tests** : Testez sur des appareils réels, pas seulement en émulation

Avec ces techniques et bonnes pratiques, vous êtes équipé pour créer des interfaces qui fonctionneront magnifiquement sur tous les appareils, de la plus petite montre connectée au plus grand écran d'ordinateur.

Dans les sections suivantes, nous explorerons comment personnaliser l'apparence avec les styles, gérer les spécificités de chaque plateforme, et optimiser les performances de vos applications multi-plateformes.

⏭️ [Styles et apparence](/05-developpement-multi-plateforme-avec-firemonkey/04-styles-et-apparence.md)
