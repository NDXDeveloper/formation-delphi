# 5.3 Création d'interfaces multi-plateformes

L'un des grands avantages de FireMonkey est la possibilité de créer des interfaces utilisateur qui fonctionnent sur plusieurs plateformes à partir d'une seule base de code. Cette section vous guidera pas à pas dans la création d'interfaces efficaces et adaptatives pour différents systèmes d'exploitation et tailles d'écran.

## Principes fondamentaux

Avant de commencer à concevoir votre interface multi-plateforme, gardez à l'esprit ces principes essentiels :

1. **Penser "adaptatif" plutôt que "fixe"** : Créez des interfaces qui s'adaptent à différentes tailles et orientations d'écran.
2. **Respecter les conventions de chaque plateforme** : Les utilisateurs s'attendent à certains comportements spécifiques à leur plateforme.
3. **Tester sur toutes les plateformes cibles** : Une interface qui semble parfaite sur Windows peut nécessiter des ajustements sur iOS ou Android.

## Configuration d'un projet multi-plateforme

Pour commencer un nouveau projet multi-plateforme :

1. Dans Delphi, sélectionnez **Fichier > Nouveau > Application multi-périphériques**
2. L'assistant de création vous présentera un formulaire vide compatible avec toutes les plateformes
3. Par défaut, les plateformes Windows (32/64 bits) sont activées
4. Pour ajouter d'autres plateformes, allez dans **Projet > Options du Projet > Plateformes cibles**

![Options de plateformes](https://placehold.co/400x300)

## Les conteneurs : la base d'une interface adaptative

Les conteneurs sont essentiels pour créer des interfaces adaptatives. Voici les principaux types :

### TLayout

Un conteneur invisible qui permet d'organiser vos contrôles :

```pascal
procedure TForm1.CreateLayout;
var
  MyLayout: TLayout;
begin
  MyLayout := TLayout.Create(Self);
  MyLayout.Parent := Self;
  MyLayout.Align := TAlignLayout.Top;
  MyLayout.Height := 50;

  // Ajouter des contrôles à l'intérieur du layout
  // ...
end;
```

### TGridLayout

Organise les contrôles en grille, idéal pour les interfaces de type galerie :

```pascal
GridLayout1.ColumnSpan := 2;  // Nombre de colonnes
GridLayout1.ItemHeight := 100; // Hauteur de chaque élément
GridLayout1.ItemWidth := 100;  // Largeur de chaque élément
```

### TFlowLayout

Dispose les contrôles automatiquement en fonction de l'espace disponible :

```pascal
FlowLayout1.HorizontalFlow := True; // Flux horizontal
FlowLayout1.WrapMode := TFlowLayoutWrapMode.Wrap; // Retour à la ligne
```

## Système d'alignement et d'ancrage

FireMonkey offre un système d'alignement puissant pour positionner des contrôles par rapport à leur parent :

### Propriété Align

```pascal
Button1.Align := TAlignLayout.Top;     // Aligné en haut
Button2.Align := TAlignLayout.Bottom;  // Aligné en bas
Button3.Align := TAlignLayout.Left;    // Aligné à gauche
Button4.Align := TAlignLayout.Right;   // Aligné à droite
Button5.Align := TAlignLayout.Client;  // Remplit l'espace restant
Button6.Align := TAlignLayout.Center;  // Centré
```

### Marges et espacement

Les propriétés `Margins` et `Padding` permettent d'affiner le positionnement :

```pascal
Button1.Margins.Left := 10;
Button1.Margins.Top := 5;
Button1.Margins.Right := 10;
Button1.Margins.Bottom := 5;

Panel1.Padding.Left := 10;  // Espace intérieur gauche
```

## Adaptation aux différentes résolutions d'écran

### Unités indépendantes de la résolution

FireMonkey utilise par défaut des unités indépendantes de la résolution. Les tailles sont automatiquement ajustées en fonction de la résolution de l'écran.

### Propriété Scale

Vous pouvez contrôler l'échelle d'un contrôle avec la propriété `Scale` :

```pascal
Image1.Scale.X := 1.5; // Agrandit l'image horizontalement
Image1.Scale.Y := 1.5; // Agrandit l'image verticalement
```

### Adaptation à l'orientation

Pour adapter votre interface à l'orientation (portrait/paysage) :

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  if Width > Height then
    // Mode paysage
    SetupLandscapeLayout
  else
    // Mode portrait
    SetupPortraitLayout;
end;
```

## Adaptation spécifique à la plateforme

### Utilisation de la directive de compilation

Vous pouvez adapter votre code selon la plateforme cible :

```pascal
{$IF DEFINED(MSWINDOWS)}
  // Code spécifique à Windows
{$ELSEIF DEFINED(ANDROID)}
  // Code spécifique à Android
{$ELSEIF DEFINED(IOS)}
  // Code spécifique à iOS
{$ELSEIF DEFINED(MACOS)}
  // Code spécifique à macOS
{$ELSE}
  // Code pour les autres plateformes
{$ENDIF}
```

### Détection de la plateforme à l'exécution

Alternativement, vous pouvez détecter la plateforme pendant l'exécution :

```pascal
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  {$IFDEF ANDROID} Androidapi.Helpers, {$ENDIF}
  FMX.Platform;

procedure AjusterInterfaceSelonPlateforme;
begin
  {$IFDEF MSWINDOWS}
  if TOSVersion.Platform = pfWindows then
  begin
    // Ajustements pour Windows
    Button1.Height := 30;
    Label1.Font.Size := 12;
  end;
  {$ENDIF}

  {$IFDEF ANDROID}
  if TOSVersion.Platform = pfAndroid then
  begin
    // Ajustements pour Android
    Button1.Height := 50; // Boutons plus grands pour écran tactile
    Label1.Font.Size := 16;
  end;
  {$ENDIF}
end;
```

## Exemple pratique : formulaire adaptatif multi-plateforme

Voici un exemple complet d'un formulaire qui s'adapte à différentes plateformes et orientations :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Edit;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    HeaderLabel: TLabel;
    ContentLayout: TLayout;
    FooterLayout: TLayout;
    ActionButton: TButton;
    InputEdit: TEdit;
    ItemsList: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure AjusterInterfacePourPlateforme;
    procedure ConfigurerModePortrait;
    procedure ConfigurerModePaysage;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  ToolBar1.Align := TAlignLayout.Top;
  HeaderLabel.Align := TAlignLayout.Client;
  HeaderLabel.TextSettings.HorzAlign := TTextAlign.Center;

  ContentLayout.Align := TAlignLayout.Client;
  FooterLayout.Align := TAlignLayout.Bottom;
  ActionButton.Align := TAlignLayout.Right;

  // Ajustements spécifiques à la plateforme
  AjusterInterfacePourPlateforme;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // Adapter l'interface selon l'orientation
  if Width > Height then
    ConfigurerModePaysage
  else
    ConfigurerModePortrait;
end;

procedure TForm1.AjusterInterfacePourPlateforme;
begin
  {$IF DEFINED(MSWINDOWS)}
    HeaderLabel.TextSettings.Font.Size := 16;
    ActionButton.Height := 30;
    FooterLayout.Height := 40;
  {$ELSEIF DEFINED(ANDROID) or DEFINED(IOS)}
    // Interfaces tactiles : éléments plus grands
    HeaderLabel.TextSettings.Font.Size := 18;
    ActionButton.Height := 50;
    FooterLayout.Height := 60;
    InputEdit.Height := 40;
  {$ENDIF}
end;

procedure TForm1.ConfigurerModePortrait;
begin
  // Configuration pour orientation portrait
  ItemsList.Align := TAlignLayout.Client;
  InputEdit.Align := TAlignLayout.Top;
  InputEdit.Width := FooterLayout.Width - ActionButton.Width;
end;

procedure TForm1.ConfigurerModePaysage;
begin
  // Configuration pour orientation paysage
  ItemsList.Align := TAlignLayout.Left;
  ItemsList.Width := Width / 2;
  InputEdit.Align := TAlignLayout.Top;
end;

end.
```

## Contrôles qui s'adaptent automatiquement

FireMonkey inclut plusieurs contrôles qui s'adaptent naturellement aux différentes plateformes :

### TMultiView

Un contrôle parfait pour créer des menus de navigation qui s'adapte automatiquement :
- Menu hamburger sur mobile
- Panneau latéral sur desktop

```pascal
MultiView1.Mode := TMultiViewMode.Drawer; // Menu glissant
MultiView1.MasterButton := MenuButton;    // Bouton qui ouvre le menu
```

### TabControl

Permet de créer des interfaces à onglets, s'adaptant automatiquement au style de la plateforme :

```pascal
TabControl1.TabPosition := TTabPosition.Bottom; // Onglets en bas (style mobile)
```

### TListView

Une liste optimisée pour chaque plateforme avec des styles natifs :

```pascal
ListView1.ItemAppearance.ItemAppearance := 'ListItem'; // Style standard
ListView1.ItemAppearance.ItemHeight := 44; // Hauteur standard pour tactile
```

## Meilleures pratiques

1. **Commencez simple** : Créez d'abord une interface basique qui fonctionne partout

2. **Testez fréquemment** : Vérifiez régulièrement sur différentes plateformes et orientations

3. **Utilisez les conteneurs** : Organisez votre interface avec des TLayout pour faciliter l'adaptation

4. **Respectez les directives de conception** :
   - Boutons plus grands sur les appareils tactiles
   - Espacement adapté à chaque plateforme
   - Menus adaptés au contexte (hamburger sur mobile, barre de menu sur desktop)

5. **Évitez les positions absolues** : Préférez l'alignement relatif et les ancrages

6. **Utilisez les styles natifs** quand c'est possible pour donner un aspect familier aux utilisateurs

7. **Considérez chaque plateforme** lors de la conception :
   - Mobile : écran tactile, espace limité
   - Desktop : souris/clavier, écran plus grand
   - Tablette : combinaison des deux approches

## Conclusion

La création d'interfaces multi-plateformes avec FireMonkey demande une approche différente de la programmation traditionnelle pour Windows. En suivant les principes adaptatifs et en utilisant les conteneurs et alignements de manière judicieuse, vous pouvez créer des applications qui offrent une expérience utilisateur de qualité sur toutes les plateformes.

Dans la section suivante, nous explorerons comment personnaliser l'apparence de vos applications avec les styles et thèmes FireMonkey.

## Astuce bonus : Prévisualisation multi-plateforme

Delphi offre un outil de prévisualisation multi-plateforme qui vous permet de voir comment votre interface s'affichera sur différents appareils sans avoir à compiler pour chaque plateforme :

1. Ouvrez votre formulaire FMX
2. Cliquez avec le bouton droit sur le fond du formulaire
3. Sélectionnez "Prévisualisations multi-plateformes"
4. Choisissez la plateforme et l'appareil à prévisualiser

Cette fonctionnalité vous aide à identifier rapidement les problèmes d'adaptation avant même de compiler votre application.
