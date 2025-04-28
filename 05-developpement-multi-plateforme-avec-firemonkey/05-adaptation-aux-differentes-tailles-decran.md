# 5.5 Adaptation aux différentes tailles d'écran

L'un des défis majeurs du développement multi-plateforme est de créer des interfaces qui s'adaptent harmonieusement à toutes les tailles d'écran - des smartphones aux grands moniteurs de bureau. FireMonkey offre plusieurs outils puissants pour relever ce défi. Dans cette section, nous verrons comment créer des interfaces adaptatives qui offrent une expérience utilisateur optimale quelle que soit la taille de l'écran.

## Comprendre le défi des différentes résolutions

Les applications modernes doivent fonctionner sur de nombreux appareils aux caractéristiques très variées :

- Smartphones : écrans de 4 à 7 pouces, en mode portrait ou paysage
- Tablettes : écrans de 7 à 13 pouces, souvent utilisés dans les deux orientations
- Ordinateurs portables : écrans de 13 à 17 pouces, principalement en mode paysage
- Ordinateurs de bureau : moniteurs de 19 à 34 pouces ou plus, en mode paysage
- Appareils haute résolution : densités de pixels variables (DPI différents)

## Concepts de base pour les interfaces adaptatives

### 1. Unités indépendantes de la résolution

Par défaut, FireMonkey utilise des unités logiques plutôt que des pixels physiques. Cela signifie que l'interface s'adapte automatiquement aux différentes densités d'écran. Ces unités logiques sont converties en pixels physiques en fonction de la résolution de l'écran.

### 2. Approche "fluide" plutôt que "fixe"

Une interface adaptative ne devrait pas avoir des dimensions fixes en pixels, mais plutôt s'adapter en fonction de l'espace disponible.

## Outils de base pour l'adaptation aux écrans

### Propriété Align

La propriété `Align` est l'outil le plus fondamental pour créer des interfaces adaptatives :

```pascal
// Contrôle qui s'étire horizontalement en haut de l'écran
Panel1.Align := TAlignLayout.Top;

// Contrôle qui remplit tout l'espace disponible au centre
Panel2.Align := TAlignLayout.Client;

// Contrôle qui s'étire verticalement sur le côté gauche
Panel3.Align := TAlignLayout.Left;

// Contrôle qui s'étire horizontalement en bas de l'écran
Panel4.Align := TAlignLayout.Bottom;
```

### Propriété Margins

Les marges permettent d'affiner le positionnement des contrôles alignés :

```pascal
// Ajoute des marges autour d'un contrôle aligné
Panel1.Align := TAlignLayout.Client;
Panel1.Margins.Left := 10;
Panel1.Margins.Top := 10;
Panel1.Margins.Right := 10;
Panel1.Margins.Bottom := 10;
```

### Propriété Padding

Le rembourrage définit l'espace entre le bord d'un conteneur et ses contrôles enfants :

```pascal
// Ajoute un espace intérieur au panel
Panel1.Padding.Left := 10;
Panel1.Padding.Top := 10;
Panel1.Padding.Right := 10;
Panel1.Padding.Bottom := 10;
```

### Propriétés Anchors

Les ancres permettent de fixer un contrôle à certains bords de son parent, et de le redimensionner en conséquence :

```pascal
// Le contrôle reste à distance fixe des bords droit et bas
Button1.Anchors := [TAnchorKind.akRight, TAnchorKind.akBottom];

// Le contrôle s'étire horizontalement mais garde une position fixe verticalement
Edit1.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight];
```

## Conteneurs adaptatifs

FireMonkey offre plusieurs types de conteneurs spécialement conçus pour créer des interfaces adaptatives :

### TLayout

Un conteneur invisible qui permet d'organiser vos contrôles. C'est la base de nombreuses interfaces adaptatives :

```pascal
// Création d'un layout qui prend tout l'espace disponible
Layout1.Align := TAlignLayout.Client;

// Ajout d'un contrôle dans le layout qui s'adapte à sa taille
Button1.Parent := Layout1;
Button1.Align := TAlignLayout.Center;
```

### TScaledLayout

Un conteneur qui s'adapte automatiquement à son parent tout en maintenant les proportions de ses enfants :

```pascal
ScaledLayout1.Align := TAlignLayout.Client;
ScaledLayout1.OriginalWidth := 320;  // Largeur de conception
ScaledLayout1.OriginalHeight := 480; // Hauteur de conception
ScaledLayout1.Scale.X := 1.0;        // Échelle horizontale initiale
ScaledLayout1.Scale.Y := 1.0;        // Échelle verticale initiale
```

### TGridLayout

Organise les contrôles en grille et s'adapte automatiquement :

```pascal
GridLayout1.Align := TAlignLayout.Client;
GridLayout1.ItemHeight := 100;       // Hauteur des cellules
GridLayout1.ItemWidth := 100;        // Largeur des cellules
GridLayout1.Orientation := TOrientation.Horizontal; // Direction de remplissage
```

### TFlowLayout

Dispose automatiquement les contrôles en fonction de l'espace disponible :

```pascal
FlowLayout1.Align := TAlignLayout.Client;
FlowLayout1.WrapMode := TFlowLayoutWrapMode.Wrap; // Retour à la ligne automatique
FlowLayout1.HorizontalFlow := True;              // Flux horizontal
FlowLayout1.HorizontalGap := 5;                  // Espace horizontal entre éléments
FlowLayout1.VerticalGap := 5;                    // Espace vertical entre éléments
```

## Détection et adaptation à l'orientation

Un aspect essentiel de l'adaptation aux écrans est la gestion de l'orientation (portrait/paysage) :

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  if Width > Height then
    AdapterInterfaceModePaysage
  else
    AdapterInterfaceModePortrait;
end;

procedure TForm1.AdapterInterfaceModePaysage;
begin
  // Réorganiser l'interface pour le mode paysage
  Panel1.Align := TAlignLayout.Left;
  Panel1.Width := Width * 0.3; // 30% de la largeur
  Panel2.Align := TAlignLayout.Client;
end;

procedure TForm1.AdapterInterfaceModePortrait;
begin
  // Réorganiser l'interface pour le mode portrait
  Panel1.Align := TAlignLayout.Top;
  Panel1.Height := Height * 0.3; // 30% de la hauteur
  Panel2.Align := TAlignLayout.Client;
end;
```

## Adaptation aux différentes catégories d'appareils

Vous pouvez aussi adapter votre interface en fonction du type d'appareil :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  DeviceType: TDeviceDisplayMetrics;
begin
  DeviceType := GetDeviceDisplayMetrics;

  case DeviceType.DeviceClass of
    TDeviceClass.Phone:
      ConfigurerInterfacePourTelephone;
    TDeviceClass.Tablet:
      ConfigurerInterfacePourTablette;
    TDeviceClass.Desktop:
      ConfigurerInterfacePourBureau;
  end;
end;

function TForm1.GetDeviceDisplayMetrics: TDeviceDisplayMetrics;
var
  ScreenService: IFMXScreenService;
begin
  Result.DeviceClass := TDeviceClass.Desktop; // Valeur par défaut

  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
  begin
    ScreenService := TPlatformServices.Current.GetPlatformService(IFMXScreenService) as IFMXScreenService;
    Result.Width := ScreenService.GetScreenSize.X;
    Result.Height := ScreenService.GetScreenSize.Y;
    Result.PhysicalSize := ScreenService.GetScreenSize.Z; // Taille en pouces

    // Déterminer la catégorie d'appareil en fonction de la taille physique
    if Result.PhysicalSize < 6.5 then
      Result.DeviceClass := TDeviceClass.Phone
    else if Result.PhysicalSize < 11 then
      Result.DeviceClass := TDeviceClass.Tablet
    else
      Result.DeviceClass := TDeviceClass.Desktop;
  end;
end;
```

> **Note :** Cette fonction `GetDeviceDisplayMetrics` est un exemple simplifié. En pratique, vous pourriez avoir besoin d'affiner la détection selon votre cas d'utilisation.

## Adaptation au DPI (densité de pixels)

Les écrans modernes ont des densités de pixels très variables. FireMonkey gère automatiquement la mise à l'échelle, mais vous pouvez personnaliser ce comportement :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  ScreenService: IFMXScreenService;
  Scale: Single;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService) then
  begin
    ScreenService := TPlatformServices.Current.GetPlatformService(IFMXScreenService) as IFMXScreenService;
    Scale := ScreenService.GetScreenScale;

    // Ajuster les tailles en fonction de la densité de l'écran
    if Scale > 1.5 then
    begin
      // Écran haute résolution
      Label1.TextSettings.Font.Size := 18;
      Button1.Height := 50;
    end
    else
    begin
      // Écran résolution standard
      Label1.TextSettings.Font.Size := 14;
      Button1.Height := 32;
    end;
  end;
end;
```

## Exemple pratique : formulaire adaptatif complet

Voici un exemple complet d'un formulaire qui s'adapte à différentes tailles d'écran et orientations :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox, FMX.Objects, FMX.Edit;

type
  TDeviceClass = (Phone, Tablet, Desktop);

  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    HeaderLabel: TLabel;
    MainLayout: TLayout;
    LeftPanel: TLayout;
    ContentPanel: TLayout;
    DetailPanel: TLayout;
    ListView1: TListBox;
    DetailLabel: TLabel;
    DetailContent: TMemo;
    SearchBox: TEdit;
    MenuButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuButtonClick(Sender: TObject);
  private
    FDeviceClass: TDeviceClass;
    FIsPortrait: Boolean;
    procedure DetectDeviceClass;
    procedure AdapterInterfaceAuFormat;
    procedure ConfigurerModeTelephone;
    procedure ConfigurerModeTablette;
    procedure ConfigurerModeBureau;
    procedure AfficherPanneauGauche(Visible: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration initiale des composants
  ToolBar1.Align := TAlignLayout.Top;
  MainLayout.Align := TAlignLayout.Client;

  LeftPanel.Align := TAlignLayout.Left;
  ListView1.Align := TAlignLayout.Client;
  SearchBox.Align := TAlignLayout.Top;

  ContentPanel.Align := TAlignLayout.Client;
  DetailPanel.Align := TAlignLayout.Client;
  DetailLabel.Align := TAlignLayout.Top;
  DetailContent.Align := TAlignLayout.Client;

  // Détection initiale de l'appareil et adaptation
  DetectDeviceClass;
  AdapterInterfaceAuFormat;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // Vérifier si l'orientation a changé
  FIsPortrait := Height > Width;

  // Adapter l'interface à la nouvelle taille/orientation
  AdapterInterfaceAuFormat;
end;

procedure TForm1.DetectDeviceClass;
begin
  // Logique simplifiée de détection du type d'appareil
  // En pratique, vous utiliseriez des APIs de plateforme pour une détection plus précise

  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
    if (Screen.Width < 600) and (Screen.Height < 1000) then
      FDeviceClass := TDeviceClass.Phone
    else
      FDeviceClass := TDeviceClass.Tablet;
  {$ELSE}
    FDeviceClass := TDeviceClass.Desktop;
  {$ENDIF}
end;

procedure TForm1.AdapterInterfaceAuFormat;
begin
  // Adapter l'interface selon le type d'appareil et l'orientation
  case FDeviceClass of
    TDeviceClass.Phone:
      ConfigurerModeTelephone;
    TDeviceClass.Tablet:
      ConfigurerModeTablette;
    TDeviceClass.Desktop:
      ConfigurerModeBureau;
  end;
end;

procedure TForm1.ConfigurerModeTelephone;
begin
  // Sur téléphone, interface simplifiée
  HeaderLabel.TextSettings.Font.Size := 16;
  MenuButton.Visible := True;

  // En mode portrait : uniquement liste OU détail
  if FIsPortrait then
  begin
    LeftPanel.Width := MainLayout.Width;
    AfficherPanneauGauche(True);
    DetailPanel.Visible := False;
  end
  else // En mode paysage : liste ET détail côte à côte
  begin
    LeftPanel.Width := MainLayout.Width * 0.4;
    AfficherPanneauGauche(True);
    DetailPanel.Visible := True;
  end;
end;

procedure TForm1.ConfigurerModeTablette;
begin
  // Sur tablette, interface hybride
  HeaderLabel.TextSettings.Font.Size := 18;
  MenuButton.Visible := not FIsPortrait;

  if FIsPortrait then
  begin
    // En portrait : le panneau gauche prend toute la largeur quand visible
    LeftPanel.Width := MainLayout.Width;
    AfficherPanneauGauche(False); // Caché par défaut, accessible via menu
  end
  else
  begin
    // En paysage : panneau gauche toujours visible, 30% de la largeur
    LeftPanel.Width := MainLayout.Width * 0.3;
    AfficherPanneauGauche(True);
  end;

  DetailPanel.Visible := True;
end;

procedure TForm1.ConfigurerModeBureau;
begin
  // Sur bureau, interface complète
  HeaderLabel.TextSettings.Font.Size := 20;
  MenuButton.Visible := False;

  // Panneau gauche toujours visible
  LeftPanel.Width := MainLayout.Width * 0.25;
  AfficherPanneauGauche(True);
  DetailPanel.Visible := True;
end;

procedure TForm1.AfficherPanneauGauche(Visible: Boolean);
begin
  // Afficher ou cacher le panneau gauche
  LeftPanel.Visible := Visible;

  // Si le panneau gauche est masqué, le panneau de contenu prend toute la place
  if not Visible then
    ContentPanel.Align := TAlignLayout.Client
  else
    ContentPanel.Align := TAlignLayout.Client;
end;

procedure TForm1.MenuButtonClick(Sender: TObject);
begin
  // Basculer la visibilité du panneau gauche quand on clique sur le bouton de menu
  AfficherPanneauGauche(not LeftPanel.Visible);
end;

end.
```

## Techniques avancées

### 1. Mise à l'échelle des images

Pour les images, utilisez la propriété `WrapMode` avec `TImageWrapMode.Fit` pour qu'elles s'adaptent à leur conteneur :

```pascal
Image1.WrapMode := TImageWrapMode.Fit;
```

### 2. Polices adaptatives

Ajustez la taille des polices en fonction de la taille de l'écran :

```pascal
procedure AjusterTaillesPolices(Scale: Single);
begin
  LabelTitre.TextSettings.Font.Size := Round(20 * Scale);
  LabelSousTitre.TextSettings.Font.Size := Round(16 * Scale);
  TexteNormal.TextSettings.Font.Size := Round(14 * Scale);
end;
```

### 3. Utilisation des pourcentages

Pour un positionnement vraiment adaptatif, utilisez des pourcentages plutôt que des valeurs fixes :

```pascal
// Un panel qui occupe 40% de la largeur du formulaire
procedure TForm1.FormResize(Sender: TObject);
begin
  Panel1.Width := Width * 0.4;
end;
```

## Recommandations et bonnes pratiques

1. **Toujours utiliser l'alignement** : Préférez TAlignLayout aux positions fixes

2. **Tester sur plusieurs appareils** : Ne vous fiez pas uniquement au simulateur

3. **Penser mobile d'abord** : Concevez d'abord pour les petits écrans, puis adaptez pour les plus grands

4. **Utiliser des conteneurs hiérarchiques** : Organisez votre interface en plusieurs niveaux de conteneurs

5. **Éviter les dimensions fixes** : Utilisez des dimensions relatives (pourcentages) autant que possible

6. **Adapter les contrôles tactiles** : Sur mobile, les boutons et zones tactiles doivent être plus grands

7. **Prévoir différentes densités d'écran** : Préparez des assets (images, icônes) pour différentes résolutions

8. **Vérifier l'orientation** : Testez votre application en mode portrait et paysage

## Conclusion

L'adaptation aux différentes tailles d'écran est un aspect fondamental du développement multi-plateforme. FireMonkey fournit tous les outils nécessaires pour créer des interfaces fluides et adaptatives qui offrent une excellente expérience utilisateur sur n'importe quel appareil.

En utilisant les conteneurs adaptatifs, les alignements et les techniques présentées dans cette section, vous pouvez créer des applications qui s'adaptent automatiquement à toutes les tailles d'écran, des smartphones aux grands moniteurs de bureau.

La section suivante explorera comment gérer les interactions tactiles et les gestes pour rendre vos applications encore plus intuitives sur les appareils mobiles.
