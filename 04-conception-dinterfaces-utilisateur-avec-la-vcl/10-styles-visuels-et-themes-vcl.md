# 4.10 Styles visuels et thèmes VCL

Les styles visuels et thèmes VCL permettent de transformer complètement l'apparence de votre application Delphi sans modifier votre code. Cette fonctionnalité puissante vous permet de donner un aspect moderne à vos applications et d'offrir à vos utilisateurs la possibilité de personnaliser l'interface selon leurs préférences. Dans cette section, nous allons découvrir comment implémenter et gérer les styles visuels dans vos applications VCL.

## Qu'est-ce que les styles visuels VCL ?

Les styles visuels VCL (aussi appelés thèmes VCL) sont une technologie qui permet de modifier l'apparence des contrôles standard de la VCL. Introduits dans Delphi XE2 et considérablement améliorés dans les versions ultérieures, ils offrent plusieurs avantages :

- Modernisation de l'interface utilisateur
- Cohérence visuelle sur toutes les plateformes
- Personnalisation selon l'identité visuelle de votre entreprise
- Adaptation aux préférences des utilisateurs (mode clair/sombre)
- Amélioration de l'expérience utilisateur

## Styles intégrés disponibles

Delphi 12 Athens (et Delphi 11 Alexandria) propose de nombreux styles prédéfinis, notamment :

- Windows11 Modern Light/Dark
- Windows11 Classic Light/Dark
- Windows10 Blue/SlateGray/Black
- Aqua Light/Dark
- Glow
- Sky
- Ruby Graphite
- Emerald Light/Dark
- Amethyst Kamri
- Et bien d'autres...

Ces styles couvrent une large gamme d'esthétiques, des interfaces professionnelles aux designs modernes et colorés.

## Activer un style visuel

### Étape 1 : Ajouter les unités nécessaires

Pour utiliser les styles VCL, vous devez d'abord ajouter les unités requises :

```pascal
uses
  Vcl.Themes, Vcl.Styles;
```

### Étape 2 : Choisir et activer un style

Pour appliquer un style, utilisez la méthode `TStyleManager.TrySetStyle` :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  TStyleManager.TrySetStyle('Windows11 Modern Light');
end;
```

Vous pouvez également ajouter cette ligne dans le bloc d'initialisation de votre projet (fichier .dpr) :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  UnitPrincipale in 'UnitPrincipale.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Activer le style avant de créer les formulaires
  TStyleManager.TrySetStyle('Windows11 Modern Light');

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end;
```

## Lister les styles disponibles

Pour permettre à l'utilisateur de choisir un style, vous pouvez lister tous les styles disponibles :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Remplir une ComboBox avec les styles disponibles
  ComboBoxStyles.Items.Clear;

  for StyleName in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(StyleName);

  // Sélectionner le style actuel
  ComboBoxStyles.ItemIndex := ComboBoxStyles.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;
```

## Changer de style à la volée

L'un des grands avantages des styles VCL est que vous pouvez les changer dynamiquement pendant l'exécution de l'application :

```pascal
procedure TForm1.ComboBoxStylesChange(Sender: TObject);
begin
  if ComboBoxStyles.ItemIndex >= 0 then
    TStyleManager.TrySetStyle(ComboBoxStyles.Text);
end;
```

## Personnaliser l'apparence des contrôles avec les styles

Les styles VCL modifient automatiquement l'apparence de presque tous les contrôles standard. Cependant, vous pouvez aussi ajuster certaines propriétés pour mieux s'intégrer avec les styles :

### Boutons plats et modernes

```pascal
Button1.StyleElements := [seFont, seClient, seBorder];
Button1.Flat := True;
```

### Panneaux transparents

```pascal
Panel1.StyleElements := [seFont];
Panel1.ParentBackground := True;
```

### Adaptation aux styles sombres

```pascal
// Vérifier si le style actuel est sombre
function IsCurrentStyleDark: Boolean;
var
  RGB: Integer;
  R, G, B: Byte;
  Color: TColor;
begin
  // Obtenir la couleur de fond du formulaire avec le style appliqué
  Color := StyleServices.GetStyleColor(scWindow);

  // Extraire les composants rouge, vert et bleu
  RGB := ColorToRGB(Color);
  R := GetRValue(RGB);
  G := GetGValue(RGB);
  B := GetBValue(RGB);

  // Calculer la luminosité approximative
  // Si la somme R+G+B est inférieure à 384 (moyenne de 128 par canal),
  // considérer que c'est un thème sombre
  Result := (R + G + B) < 384;
end;

// Utilisation
procedure TForm1.ApplyStyleSpecificSettings;
begin
  if IsCurrentStyleDark then
  begin
    // Ajustements pour styles sombres
    Chart1.BackWall.Brush.Color := clGray;
    Chart1.Title.Font.Color := clWhite;
  end
  else
  begin
    // Ajustements pour styles clairs
    Chart1.BackWall.Brush.Color := clWhite;
    Chart1.Title.Font.Color := clBlack;
  end;
end;
```

## Gérer le changement de style

Lorsqu'un style est modifié, vous pourriez avoir besoin de mettre à jour certains éléments de votre application. Vous pouvez intercepter cet événement :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // S'abonner à l'événement de changement de style
  TStyleManager.OnStyleChanged := StyleChangedHandler;
end;

procedure TForm1.StyleChangedHandler(Sender: TObject);
begin
  // Mettre à jour les éléments qui ont besoin d'être ajustés
  ApplyStyleSpecificSettings;

  // Si vous avez des images qui doivent s'adapter au style
  UpdateImagesForStyle;
end;

procedure TForm1.UpdateImagesForStyle;
begin
  // Exemple : changer les icônes selon le style
  if IsCurrentStyleDark then
  begin
    Image1.Picture.LoadFromFile('icons_dark/save.png');
    Image2.Picture.LoadFromFile('icons_dark/open.png');
  end
  else
  begin
    Image1.Picture.LoadFromFile('icons_light/save.png');
    Image2.Picture.LoadFromFile('icons_light/open.png');
  end;
end;
```

## Exclure certains contrôles des styles

Dans certains cas, vous pourriez vouloir qu'un contrôle conserve son apparence standard, indépendamment du style actif :

```pascal
// Exclure complètement un contrôle du système de styles
Panel1.StyleElements := [];

// Inclure seulement certains éléments
Edit1.StyleElements := [seFont, seBorder]; // Pas de couleur de fond stylisée
```

Les valeurs possibles pour `StyleElements` sont :
- `seFont` : Police et couleur du texte
- `seClient` : Couleur de fond
- `seBorder` : Bordures
- `seAll` : Tous les éléments (valeur par défaut)

## Créer un sélecteur de styles interactif

Voici un exemple plus complet d'un sélecteur de styles avec aperçu :

```pascal
unit UnitStyleSelector;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Themes, Vcl.Styles;

type
  TFormStyleSelector = class(TForm)
    ListBoxStyles: TListBox;
    PanelPreview: TPanel;
    ButtonApply: TButton;
    ButtonCancel: TButton;
    LabelStyleName: TLabel;
    PanelPreviewContent: TPanel;
    ButtonPreview: TButton;
    EditPreview: TEdit;
    CheckBoxPreview: TCheckBox;
    RadioButtonPreview: TRadioButton;
    TrackBarPreview: TTrackBar;
    ComboBoxPreview: TComboBox;
    ProgressBarPreview: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure ListBoxStylesClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    FOriginalStyle: string;
    procedure UpdatePreview;
  public
    class function Execute: Boolean;
  end;

implementation

{$R *.dfm}

// Fonction d'exécution statique
class function TFormStyleSelector.Execute: Boolean;
var
  Dialog: TFormStyleSelector;
begin
  Dialog := TFormStyleSelector.Create(Application);
  try
    Result := Dialog.ShowModal = mrOk;
  finally
    Dialog.Free;
  end;
end;

procedure TFormStyleSelector.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Mémoriser le style original au cas où l'utilisateur annule
  FOriginalStyle := TStyleManager.ActiveStyle.Name;

  // Remplir la liste des styles
  ListBoxStyles.Items.Clear;
  for StyleName in TStyleManager.StyleNames do
    ListBoxStyles.Items.Add(StyleName);

  // Sélectionner le style actuel
  ListBoxStyles.ItemIndex := ListBoxStyles.Items.IndexOf(FOriginalStyle);

  // Initialiser les contrôles de démonstration
  ComboBoxPreview.Items.Add('Option 1');
  ComboBoxPreview.Items.Add('Option 2');
  ComboBoxPreview.Items.Add('Option 3');
  ComboBoxPreview.ItemIndex := 0;

  CheckBoxPreview.Checked := True;
  RadioButtonPreview.Checked := True;
  TrackBarPreview.Position := 50;
  ProgressBarPreview.Position := 75;

  // Mettre à jour l'aperçu
  UpdatePreview;
end;

procedure TFormStyleSelector.ListBoxStylesClick(Sender: TObject);
begin
  // Changer le style pour l'aperçu
  if ListBoxStyles.ItemIndex >= 0 then
  begin
    TStyleManager.TrySetStyle(ListBoxStyles.Items[ListBoxStyles.ItemIndex]);
    UpdatePreview;
  end;
end;

procedure TFormStyleSelector.UpdatePreview;
begin
  // Mettre à jour le libellé avec le nom du style
  LabelStyleName.Caption := 'Style : ' + TStyleManager.ActiveStyle.Name;
end;

procedure TFormStyleSelector.ButtonApplyClick(Sender: TObject);
begin
  // L'utilisateur confirme le choix, fermer avec OK
  ModalResult := mrOk;
end;

procedure TFormStyleSelector.ButtonCancelClick(Sender: TObject);
begin
  // L'utilisateur annule, restaurer le style original
  TStyleManager.TrySetStyle(FOriginalStyle);
  ModalResult := mrCancel;
end;
```

Usage dans le formulaire principal :

```pascal
procedure TForm1.ButtonSelectStyleClick(Sender: TObject);
begin
  if TFormStyleSelector.Execute then
    // Le style a été changé et confirmé par l'utilisateur
    SaveStylePreference(TStyleManager.ActiveStyle.Name);
end;
```

## Sauvegarder et restaurer le style préféré

Pour offrir une meilleure expérience utilisateur, vous pouvez sauvegarder la préférence de style et la restaurer au prochain démarrage :

```pascal
procedure SaveStylePreference(const StyleName: string);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApp', True) then
    begin
      Registry.WriteString('StyleName', StyleName);
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

function LoadStylePreference: string;
var
  Registry: TRegistry;
begin
  Result := ''; // Style par défaut si rien n'est trouvé

  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    if Registry.OpenKey('Software\MyCompany\MyApp', False) then
    begin
      if Registry.ValueExists('StyleName') then
        Result := Registry.ReadString('StyleName');
      Registry.CloseKey;
    end;
  finally
    Registry.Free;
  end;
end;

// Dans le projet principal ou le formulaire principal
procedure TForm1.FormCreate(Sender: TObject);
var
  StyleName: string;
begin
  // Charger et appliquer le style préféré
  StyleName := LoadStylePreference;
  if (StyleName <> '') and (TStyleManager.StyleNames.IndexOf(StyleName) >= 0) then
    TStyleManager.TrySetStyle(StyleName)
  else
    TStyleManager.TrySetStyle('Windows11 Modern Light'); // Style par défaut
end;
```

## Mode clair/sombre automatique

Une fonctionnalité moderne très appréciée est la détection automatique du mode clair/sombre du système d'exploitation :

```pascal
// Nécessite Delphi 10.4 ou supérieur et Windows 10/11
uses
  Winapi.Windows, Vcl.Themes, Vcl.Styles;

// Constantes pour l'API Windows
const
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19;

// Détecte si Windows utilise le mode sombre
function IsWindowsInDarkMode: Boolean;
var
  Module: HMODULE;
  ShouldAppsUseDarkMode: function: Boolean; stdcall;
begin
  Result := False;

  // Charger la DLL UxTheme
  Module := LoadLibrary('UxTheme.dll');
  if Module <> 0 then
  begin
    try
      // Obtenir la fonction ShouldAppsUseDarkMode
      @ShouldAppsUseDarkMode := GetProcAddress(Module, MakeIntResource(132));

      if Assigned(ShouldAppsUseDarkMode) then
        Result := ShouldAppsUseDarkMode();
    finally
      FreeLibrary(Module);
    end;
  end;
end;

// Applique le style approprié selon le mode du système
procedure ApplySystemTheme;
begin
  if IsWindowsInDarkMode then
    TStyleManager.TrySetStyle('Windows11 Modern Dark')
  else
    TStyleManager.TrySetStyle('Windows11 Modern Light');
end;

// Utilisation dans le formulaire principal
procedure TForm1.FormCreate(Sender: TObject);
begin
  ApplySystemTheme;
end;
```

## Conseils avancés pour les styles VCL

### 1. Performances

L'utilisation des styles peut avoir un impact sur les performances, surtout avec de nombreux contrôles. Quelques conseils :

- Utilisez `DoubleBuffered := True` sur les formulaires pour réduire le scintillement
- N'appliquez pas de styles à des contrôles critiques en termes de performance
- Testez votre application avec différents styles pour identifier les problèmes éventuels

### 2. Compatibilité avec les composants tiers

Certains composants tiers peuvent ne pas bien fonctionner avec les styles VCL :

- Vérifiez si le composant supporte les styles VCL
- Utilisez `StyleElements := []` pour les composants problématiques
- Contactez le fournisseur pour des versions compatibles

### 3. Création de styles personnalisés

Vous pouvez créer vos propres styles avec l'outil Bitmap Style Designer inclus dans Delphi :

1. Ouvrez l'outil via le menu **Outils** > **Bitmap Style Designer**
2. Créez un nouveau style ou modifiez un style existant
3. Personnalisez les couleurs, textures et formes selon vos besoins
4. Enregistrez le style en tant que fichier .vsf
5. Chargez-le dans votre application :

```pascal
TStyleManager.LoadFromFile('MonStylePersonnalise.vsf');
TStyleManager.TrySetStyle('MonStylePersonnalise');
```

## Bonnes pratiques pour l'utilisation des styles

1. **Testez avec plusieurs styles** : Assurez-vous que votre application est attrayante et fonctionnelle avec différents styles.

2. **Adaptez-vous aux styles sombres** : Vérifiez que vos graphiques, images et couleurs personnalisées s'adaptent bien aux styles sombres.

3. **Offrez du choix** : Permettez aux utilisateurs de choisir leur style préféré et mémorisez ce choix.

4. **Cohérence** : Gardez une cohérence visuelle dans toute l'application, même avec les composants qui ne supportent pas les styles.

5. **Respect des préférences système** : Si possible, détectez et respectez le mode clair/sombre du système d'exploitation.

## Conclusion

Les styles visuels VCL sont un moyen puissant de moderniser vos applications Delphi et d'améliorer l'expérience utilisateur. Ils vous permettent de transformer complètement l'apparence de votre application avec un minimum d'effort de codage. En offrant à vos utilisateurs la possibilité de personnaliser l'interface, vous rendez votre application plus agréable et plus accessible.

Avec Delphi 12 Athens et Delphi 11 Alexandria, les styles VCL sont plus riches et plus flexibles que jamais, vous permettant de créer des applications à l'aspect professionnel et moderne, tout en maintenant la facilité d'utilisation et la productivité qui caractérisent le développement Delphi.

---

*Exercice pratique : Créez une application simple avec plusieurs contrôles VCL standards et ajoutez un sélecteur de styles permettant à l'utilisateur de choisir parmi au moins 5 styles différents. Ajoutez également un mode "Auto" qui détecte et applique automatiquement le thème clair ou sombre en fonction des paramètres du système d'exploitation.*
