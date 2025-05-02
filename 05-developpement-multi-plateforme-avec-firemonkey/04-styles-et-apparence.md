# 5.4 Styles et apparence

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

L'un des grands avantages de FireMonkey est son système de styles puissant et flexible. Les styles vous permettent de personnaliser l'apparence de votre application et de maintenir une cohérence visuelle sur toutes les plateformes ou, au contraire, d'adopter l'apparence native de chaque système. Dans cette section, nous allons explorer comment utiliser et personnaliser les styles dans vos applications FireMonkey.

## Comprendre le système de styles FireMonkey

Contrairement à la VCL qui utilise les contrôles natifs de Windows, FireMonkey dessine ses propres contrôles. Cela permet une personnalisation beaucoup plus poussée de l'apparence visuelle. Un style FireMonkey définit :

- Les couleurs
- Les polices
- Les formes des contrôles
- Les animations et effets visuels
- Les transitions

## Styles prédéfinis

Delphi inclut plusieurs styles prédéfinis que vous pouvez utiliser immédiatement :

| Style | Description |
|-------|-------------|
| Windows | Aspect similaire à Windows 10/11 |
| Metallic | Aspect métallique avec dégradés |
| Light | Design moderne et minimaliste clair |
| Dark | Thème sombre élégant |
| Aqua | Style inspiré de macOS |
| iOS | Apparence des applications iOS |
| Android | Apparence des applications Android Material Design |

## Appliquer un style prédéfini

### 1. Utiliser un style au niveau de l'application

La façon la plus simple d'appliquer un style est au niveau de l'application entière :

```pascal
// À placer dans la méthode FormCreate
TStyleManager.TrySetStyleFromResource('Windows');
```

ou

```pascal
// À placer dans FormCreate
TStyleManager.SetStyle('Light');
```

### 2. Utiliser un StyleBook

Pour une gestion plus flexible, vous pouvez utiliser un composant TStyleBook :

1. Depuis la palette d'outils, faites glisser un composant **TStyleBook** sur votre formulaire
2. Dans l'Inspecteur d'objets, cliquez sur la propriété **StyleResource**
3. Cliquez sur le bouton [...] pour ouvrir l'éditeur de ressources de style
4. Choisissez un style prédéfini dans la liste déroulante
5. Associez le StyleBook à votre formulaire en définissant la propriété `StyleBook` du formulaire

```pascal
Form1.StyleBook := StyleBook1;
```

## Changer de style en cours d'exécution

Voici comment permettre à l'utilisateur de changer de style pendant l'exécution de l'application :

```pascal
procedure TForm1.ComboBoxStyleChange(Sender: TObject);
var
  StyleName: string;
begin
  StyleName := ComboBox1.Items[ComboBox1.ItemIndex];
  TStyleManager.TrySetStyleFromResource(StyleName);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Remplir la liste déroulante avec les styles disponibles
  for StyleName in TStyleManager.StyleNames do
    ComboBox1.Items.Add(StyleName);

  // Sélectionner le style actuel
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(TStyleManager.ActiveStyleName);
end;
```

## Différents styles selon la plateforme

FireMonkey permet d'appliquer automatiquement un style approprié à la plateforme d'exécution :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
  TStyleManager.TrySetStyleFromResource('Windows');
  {$ENDIF}

  {$IFDEF IOS}
  TStyleManager.TrySetStyleFromResource('iOS');
  {$ENDIF}

  {$IFDEF ANDROID}
  TStyleManager.TrySetStyleFromResource('Android');
  {$ENDIF}

  {$IFDEF MACOS}
  TStyleManager.TrySetStyleFromResource('Aqua Light');
  {$ENDIF}
end;
```

## Personnaliser l'apparence des contrôles individuels

### Propriétés de style de base

Vous pouvez personnaliser l'apparence de chaque contrôle individuellement :

```pascal
// Personnaliser un bouton
Button1.StyledSettings := []; // Désactiver les paramètres hérités du style
Button1.TextSettings.Font.Family := 'Segoe UI';
Button1.TextSettings.Font.Size := 14;
Button1.TextSettings.FontColor := TAlphaColors.White;
Button1.Fill.Color := TAlphaColors.Red;
Button1.Stroke.Color := TAlphaColors.Darkred;
Button1.Stroke.Thickness := 2;
```

### TRectangle comme base personnalisée

Un TRectangle peut servir de base pour créer des contrôles personnalisés :

```pascal
Rectangle1.Fill.Color := TAlphaColors.Blue;
Rectangle1.Fill.Kind := TBrushKind.Gradient;
Rectangle1.Fill.Gradient.Color := TAlphaColors.Lightblue;
Rectangle1.Fill.Gradient.Color1 := TAlphaColors.Darkblue;
Rectangle1.Fill.Gradient.StartPosition.X := 0;
Rectangle1.Fill.Gradient.StartPosition.Y := 0;
Rectangle1.Fill.Gradient.StopPosition.X := 1;
Rectangle1.Fill.Gradient.StopPosition.Y := 1;
Rectangle1.XRadius := 10; // Coins arrondis
Rectangle1.YRadius := 10; // Coins arrondis
```

## Types de remplissage (Fill)

FireMonkey propose plusieurs types de remplissage pour vos contrôles :

| Type | Description | Exemple |
|------|-------------|---------|
| Solid | Couleur unie | `Rectangle1.Fill.Kind := TBrushKind.Solid;` |
| Gradient | Dégradé de couleurs | `Rectangle1.Fill.Kind := TBrushKind.Gradient;` |
| Bitmap | Image comme remplissage | `Rectangle1.Fill.Kind := TBrushKind.Bitmap;` |
| Resource | Ressource externe | `Rectangle1.Fill.Kind := TBrushKind.Resource;` |

### Exemple de dégradé linéaire

```pascal
Rectangle1.Fill.Kind := TBrushKind.Gradient;
Rectangle1.Fill.Gradient.Style := TGradientStyle.Linear;
Rectangle1.Fill.Gradient.Color := TAlphaColors.Skyblue;
Rectangle1.Fill.Gradient.Color1 := TAlphaColors.Navy;
```

### Exemple de dégradé radial

```pascal
Rectangle1.Fill.Kind := TBrushKind.Gradient;
Rectangle1.Fill.Gradient.Style := TGradientStyle.Radial;
Rectangle1.Fill.Gradient.Color := TAlphaColors.White;
Rectangle1.Fill.Gradient.Color1 := TAlphaColors.Lime;
```

## Effets visuels

FireMonkey prend en charge divers effets visuels qui peuvent être appliqués à vos contrôles :

### Ombre portée

```pascal
// Ajouter un effet d'ombre à un rectangle
ShadowEffect1 := TShadowEffect.Create(Rectangle1);
ShadowEffect1.Parent := Rectangle1;
ShadowEffect1.Distance := 3;
ShadowEffect1.Direction := 45;
ShadowEffect1.Softness := 0.3;
ShadowEffect1.Opacity := 0.6;
ShadowEffect1.ShadowColor := TAlphaColors.Black;
```

### Effet de flou

```pascal
// Ajouter un effet de flou à une image
BlurEffect1 := TBlurEffect.Create(Image1);
BlurEffect1.Parent := Image1;
BlurEffect1.Softness := 0.5;
```

### Effet de reflet

```pascal
// Ajouter un effet de reflet à un bouton
ReflectionEffect1 := TReflectionEffect.Create(Button1);
ReflectionEffect1.Parent := Button1;
ReflectionEffect1.Opacity := 0.5;
```

## Création d'un style personnalisé

Pour des applications professionnelles, vous voudrez peut-être créer votre propre style. Voici les étapes de base :

1. **Créez un nouveau StyleBook** : Ajoutez un TStyleBook à votre formulaire

2. **Ouvrez l'éditeur de style** : Double-cliquez sur le StyleBook ou ouvrez la propriété StyleResource

3. **Copiez un style existant** : Commencez avec un style prédéfini comme base

4. **Modifiez les composants du style** : L'éditeur ressemble à celui de Photoshop ou d'Illustrator

5. **Enregistrez votre style** : Enregistrez-le sous un nouveau nom

6. **Appliquez votre style** : Assignez votre StyleBook au formulaire

## Les thèmes clair/sombre

Une tendance moderne consiste à proposer des thèmes clairs et sombres. FireMonkey facilite l'implémentation de cette fonctionnalité :

```pascal
procedure TForm1.SwitchThemeClick(Sender: TObject);
begin
  if TStyleManager.ActiveStyleName.Contains('Dark') then
    TStyleManager.TrySetStyleFromResource('Windows')
  else
    TStyleManager.TrySetStyleFromResource('Windows Dark');

  // Mise à jour des éléments qui ne suivent pas automatiquement le style
  UpdateCustomElements;
end;

procedure TForm1.UpdateCustomElements;
var
  IsDarkTheme: Boolean;
begin
  IsDarkTheme := TStyleManager.ActiveStyleName.Contains('Dark');

  if IsDarkTheme then
  begin
    // Couleurs pour thème sombre
    Label1.TextSettings.FontColor := TAlphaColors.White;
    Image1.Opacity := 0.8;
  end
  else
  begin
    // Couleurs pour thème clair
    Label1.TextSettings.FontColor := TAlphaColors.Black;
    Image1.Opacity := 1.0;
  end;
end;
```

## Adaptation de style selon la plateforme et le thème système

Avec Delphi 12 Athens, vous pouvez détecter et adapter automatiquement vos styles selon le thème du système d'exploitation :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  DarkTheme: Boolean;
begin
  {$IF DEFINED(MSWINDOWS) or DEFINED(MACOS)}
  // Détecter si le système utilise un thème sombre
  DarkTheme := TOSVersion.IsDarkModeEnabled;

  if DarkTheme then
    TStyleManager.TrySetStyleFromResource('Windows Dark')
  else
    TStyleManager.TrySetStyleFromResource('Windows');
  {$ENDIF}
end;
```

> **Astuce :** La fonction `TOSVersion.IsDarkModeEnabled` nécessite Delphi 12 ou supérieur.

## Meilleures pratiques pour les styles

1. **Cohérence** : Maintenez une apparence cohérente dans toute votre application

2. **Adaptabilité** : Assurez-vous que votre application s'adapte bien aux changements de style

3. **Respect des directives de plateforme** : Sur mobile, respectez les directives de conception d'iOS et d'Android

4. **Accessibilité** : Vérifiez que les contrastes sont suffisants pour tous les utilisateurs

5. **Testez sur toutes les plateformes** : Certains styles peuvent rendre différemment selon la plateforme

6. **Évitez de modifier des contrôles individuels** : Utilisez des styles globaux autant que possible

7. **Créez des composants de style réutilisables** : Pour les éléments personnalisés qui reviennent souvent

## Exemple complet : Application avec sélecteur de style

Voici un exemple complet d'application permettant à l'utilisateur de choisir entre différents styles :

```pascal
unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListBox, FMX.Layouts, FMX.Objects;

type
  TForm1 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    Layout1: TLayout;
    ComboBox1: TComboBox;
    Label2: TLabel;
    Button1: TButton;
    Rectangle1: TRectangle;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure UpdatePreviewElements;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Remplir le ComboBox avec les styles disponibles
  for StyleName in TStyleManager.StyleNames do
    ComboBox1.Items.Add(StyleName);

  // Sélectionner le style actuel
  ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(TStyleManager.ActiveStyleName);

  // Initialiser les éléments de prévisualisation
  UpdatePreviewElements;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  StyleName: string;
begin
  StyleName := ComboBox1.Items[ComboBox1.ItemIndex];
  TStyleManager.TrySetStyleFromResource(StyleName);

  // Mettre à jour les éléments de prévisualisation
  UpdatePreviewElements;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Vous utilisez le style : ' + TStyleManager.ActiveStyleName);
end;

procedure TForm1.UpdatePreviewElements;
var
  IsDarkTheme: Boolean;
begin
  // Déterminer si c'est un thème sombre
  IsDarkTheme := TStyleManager.ActiveStyleName.Contains('Dark');

  // Ajuster les éléments personnalisés qui ne suivent pas automatiquement le style
  if IsDarkTheme then
  begin
    Rectangle1.Fill.Color := TAlphaColors.Navy;
    Rectangle1.Stroke.Color := TAlphaColors.Skyblue;
  end
  else
  begin
    Rectangle1.Fill.Color := TAlphaColors.Lightblue;
    Rectangle1.Stroke.Color := TAlphaColors.Darkblue;
  end;
end;

end.
```

## Conclusion

Le système de styles de FireMonkey est l'un de ses atouts majeurs pour le développement multi-plateforme. Il vous permet de créer des applications visuellement attractives tout en maintenant une cohérence sur toutes les plateformes. Que vous choisissiez d'utiliser les styles prédéfinis qui imitent les interfaces natives ou de créer votre propre identité visuelle unique, FireMonkey vous offre tous les outils nécessaires.

Dans la section suivante, nous verrons comment adapter votre application aux différentes tailles d'écran pour offrir une expérience utilisateur optimale sur tous les appareils.

⏭️ [Adaptation aux différentes tailles d'écran](/05-developpement-multi-plateforme-avec-firemonkey/05-adaptation-aux-differentes-tailles-decran.md)
