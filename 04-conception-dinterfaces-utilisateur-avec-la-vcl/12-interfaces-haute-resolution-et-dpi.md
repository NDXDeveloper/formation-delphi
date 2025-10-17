🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.12 Interfaces haute résolution et prise en charge du DPI

## Introduction

Avec la popularisation des écrans haute résolution (4K, 5K, Retina), la gestion du DPI (Dots Per Inch) est devenue un aspect crucial du développement d'applications modernes. Une application qui ne gère pas correctement le DPI apparaîtra floue ou avec des éléments trop petits sur les écrans haute résolution, nuisant gravement à l'expérience utilisateur.

## 4.12.1 Comprendre le DPI

### Qu'est-ce que le DPI ?

**DPI (Dots Per Inch)** ou **PPI (Pixels Per Inch)** mesure la densité de pixels d'un écran. Plus le DPI est élevé, plus les pixels sont petits et plus l'image est détaillée.

### Évolution des écrans

**Écrans traditionnels (96 DPI) :**
```
Résolution standard : 1920×1080 (Full HD)
Taille écran : 24 pouces
DPI : 96
Pixels par pouce : 92

Apparence : Pixels visibles de près
Texte : Légèrement pixelisé
```

**Écrans haute résolution (192+ DPI) :**
```
Résolution 4K : 3840×2160
Taille écran : 24 pouces
DPI : 192 (200% scaling)
Pixels par pouce : 184

Apparence : Pixels invisibles
Texte : Parfaitement lisse
```

### Le problème avec les anciennes applications

**Sans gestion du DPI :**
```
Écran 96 DPI (standard)          Écran 192 DPI (4K)
┌─────────────────────┐          ┌──────────┐
│                     │          │          │
│   [  Bouton  ]      │    →     │  [Btn]   │
│                     │          │          │
│   Texte lisible     │          │  Txt     │
└─────────────────────┘          └──────────┘
     Taille normale                Trop petit !
```

**Avec gestion du DPI :**
```
Écran 96 DPI (standard)          Écran 192 DPI (4K)
┌─────────────────────┐          ┌─────────────────────┐
│                     │          │                     │
│   [  Bouton  ]      │    →     │   [  Bouton  ]      │
│                     │          │                     │
│   Texte lisible     │          │   Texte lisible     │
└─────────────────────┘          └─────────────────────┘
     Taille normale                Taille adaptée
```

### Pourquoi c'est important

**Problèmes sans gestion DPI :**
- Texte illisible (trop petit)
- Boutons difficiles à cliquer
- Interface désordonnée
- Images pixelisées ou trop petites
- Expérience utilisateur dégradée

**Avantages d'une bonne gestion DPI :**
- Interface nette sur tous les écrans
- Texte toujours lisible
- Éléments cliquables de taille appropriée
- Application professionnelle
- Meilleure satisfaction utilisateur

---

## 4.12.2 Configuration de base pour le DPI

### Le manifeste d'application

Le manifeste indique à Windows comment gérer le DPI de votre application.

**Créer ou modifier le manifeste :**

1. **Créer un fichier de manifeste**
   ```
   Nom du fichier : MonApplication.manifest
   Emplacement : Même dossier que le .dpr
   ```

2. **Contenu du manifeste pour DPI moderne :**

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0">
  <assemblyIdentity
    version="1.0.0.0"
    processorArchitecture="*"
    name="MonApplication"
    type="win32"/>

  <description>Mon Application avec support DPI</description>

  <!-- Support DPI Per-Monitor V2 (Windows 10 1703+) -->
  <application xmlns="urn:schemas-microsoft-com:asm.v3">
    <windowsSettings>
      <dpiAware xmlns="http://schemas.microsoft.com/SMI/2005/WindowsSettings">true</dpiAware>
      <dpiAwareness xmlns="http://schemas.microsoft.com/SMI/2016/WindowsSettings">PerMonitorV2</dpiAwareness>
    </windowsSettings>
  </application>

  <!-- Compatibilité Windows -->
  <compatibility xmlns="urn:schemas-microsoft-com:compatibility.v1">
    <application>
      <!-- Windows 10 et 11 -->
      <supportedOS Id="{8e0f7a12-bfb3-4fe8-b9a5-48fd50a15a9a}"/>
      <!-- Windows 8.1 -->
      <supportedOS Id="{1f676c76-80e1-4239-95bb-83d0f6d0da78}"/>
      <!-- Windows 8 -->
      <supportedOS Id="{4a2f28e3-53b9-4441-ba9c-d69d4a4a6e38}"/>
      <!-- Windows 7 -->
      <supportedOS Id="{35138b9a-5d96-4fbd-8e2d-a2440225f93a}"/>
    </application>
  </compatibility>
</assembly>
```

3. **Lier le manifeste au projet**

```pascal
// Dans le fichier .dpr, ajouter :
{$R *.res}
{$R 'MonApplication.manifest.res' 'MonApplication.manifest'}

// Ou via les options du projet :
// Projet → Options → Application → Manifest
// Sélectionner "Incorporer un manifeste personnalisé"
```

### Modes de conscience DPI

**Trois modes principaux :**

| Mode | Description | Usage |
|------|-------------|-------|
| **DPI Unaware** | Windows scale l'application | Ancien code, rendu flou |
| **System DPI Aware** | Scale au DPI du démarrage | Applications simples |
| **Per-Monitor V2** | S'adapte à chaque écran | Recommandé pour Delphi 13 |

**Comparaison visuelle :**

```
DPI Unaware (pas recommandé)
┌────────────────────────┐
│  Application floue     │  ← Windows agrandit l'image
│  [Texte pixelisé]      │
└────────────────────────┘

System DPI Aware (acceptable)
┌────────────────────────┐
│  Application nette     │  ← DPI fixe au lancement
│  [Texte net]           │
└────────────────────────┘
Mais problème si on change d'écran

Per-Monitor V2 (recommandé)
┌────────────────────────┐
│  Application nette     │  ← S'adapte dynamiquement
│  [Texte net]           │
└────────────────────────┘
Fonctionne sur tous les écrans
```

---

## 4.12.3 Propriétés essentielles du formulaire

### Scaled : Activer le scaling automatique

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // TOUJOURS activer Scaled pour le support DPI
  Scaled := True;

  // Cette propriété indique à Delphi d'ajuster automatiquement
  // la taille de tous les composants selon le DPI
end;
```

**Dans l'Inspecteur d'objets :**
```
Propriété : Scaled
Valeur : True (IMPORTANT !)
```

### PixelsPerInch : DPI de conception

```pascal
// Cette propriété indique le DPI utilisé lors de la conception
// Valeur standard : 96 (écran 100%)
// Delphi utilisera cette valeur comme référence pour le scaling

// Lecture seule en mode design
// Vous pouvez le lire en runtime :
procedure TForm1.FormCreate(Sender: TObject);
begin
  ShowMessage(Format('DPI de conception : %d', [PixelsPerInch]));
  ShowMessage(Format('DPI actuel : %d', [Screen.PixelsPerInch]));
end;
```

### Font.PixelsPerInch : DPI de la police

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // La police s'adapte automatiquement si Scaled = True
  Font.Name := 'Segoe UI';
  Font.Size := 9; // Taille logique (points)

  // La taille réelle en pixels sera ajustée selon le DPI
  // À 96 DPI : 9pt = 12 pixels
  // À 192 DPI : 9pt = 24 pixels
end;
```

### AutoScroll : Barres de défilement automatiques

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Permettre le défilement si le contenu dépasse la taille
  AutoScroll := True;

  // Utile sur petits écrans avec DPI élevé
end;
```

---

## 4.12.4 Scaling automatique dans Delphi

### Comment fonctionne le scaling automatique

Delphi ajuste automatiquement la taille des composants selon la formule :

```
Taille réelle = Taille de conception × (DPI actuel / DPI de conception)

Exemple :
Bouton conçu à 96 DPI, largeur 100 pixels
Sur écran 192 DPI : 100 × (192 / 96) = 200 pixels
Sur écran 144 DPI : 100 × (144 / 96) = 150 pixels
```

### Exemple de formulaire auto-scalable

```pascal
unit FormPrincipal;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigurerDPI;
    procedure AfficherInfosDPI;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ConfigurerDPI;
  AfficherInfosDPI;
end;

procedure TFormMain.ConfigurerDPI;
begin
  // Activer le scaling automatique
  Scaled := True;

  // S'assurer que la police principale est scalable
  Font.Name := 'Segoe UI';
  Font.Size := 9;

  // Permettre le scroll si nécessaire
  AutoScroll := True;

  // Les composants hériteront automatiquement
  // du scaling du formulaire
end;

procedure TFormMain.AfficherInfosDPI;
begin
  Caption := Format('Mon Application - DPI: %d (Scale: %d%%)',
    [Screen.PixelsPerInch,
     Round(Screen.PixelsPerInch / 96 * 100)]);

  Label1.Caption := Format(
    'DPI de conception : %d' + #13#10 +
    'DPI actuel : %d' + #13#10 +
    'Facteur d''échelle : %.2f',
    [PixelsPerInch,
     Screen.PixelsPerInch,
     Screen.PixelsPerInch / PixelsPerInch]);
end;

end.
```

### Propriétés qui se scalent automatiquement

**Se scalent automatiquement (si Scaled = True) :**
```pascal
- Width, Height
- Left, Top
- Font.Size
- BorderWidth
- Margins
- Padding
```

**Ne se scalent PAS automatiquement :**
```pascal
- Images bitmap (nécessitent gestion spéciale)
- Tailles codées en dur dans le code
- Coordonnées de dessin (Canvas)
```

---

## 4.12.5 Gestion manuelle du DPI

### Quand utiliser le scaling manuel

Parfois, vous devez gérer le DPI manuellement :
- Dessin sur Canvas
- Chargement d'images
- Positionnement dynamique de composants
- Calculs de taille personnalisés

### Fonction de scaling manuel

```pascal
// Fonction utilitaire pour scaler une valeur
function ScaleValue(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

// Ou pour scaler depuis le DPI de conception du formulaire
function TForm1.ScaleValueFromDesign(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, Self.PixelsPerInch);
end;

// Utilisation :
procedure TForm1.CreerBoutonDynamique;
var
  Bouton: TButton;
begin
  Bouton := TButton.Create(Self);
  Bouton.Parent := Self;

  // Utiliser des valeurs scalées
  Bouton.Width := ScaleValue(100);   // 100 pixels à 96 DPI
  Bouton.Height := ScaleValue(25);   // 25 pixels à 96 DPI
  Bouton.Left := ScaleValue(10);
  Bouton.Top := ScaleValue(10);

  Bouton.Caption := 'Bouton scalé';
end;
```

### Scaler le dessin sur Canvas

```pascal
procedure TForm1.Image1Paint(Sender: TObject);
var
  ScaleFactor: Double;
  RayonScaled, XScaled, YScaled: Integer;
begin
  // Calculer le facteur d'échelle
  ScaleFactor := Screen.PixelsPerInch / 96;

  // Scaler les dimensions
  RayonScaled := Round(50 * ScaleFactor);  // Rayon de 50px à 96 DPI
  XScaled := Round(100 * ScaleFactor);
  YScaled := Round(100 * ScaleFactor);

  // Dessiner avec les valeurs scalées
  Image1.Canvas.Brush.Color := clBlue;
  Image1.Canvas.Ellipse(
    XScaled - RayonScaled,
    YScaled - RayonScaled,
    XScaled + RayonScaled,
    YScaled + RayonScaled
  );
end;
```

### Classe helper pour DPI

```pascal
type
  TDPIHelper = class
  private
    class var FCurrentDPI: Integer;
  public
    class procedure Initialize;
    class function Scale(Value: Integer): Integer;
    class function ScaleFloat(Value: Double): Double;
    class function ScaleRect(const R: TRect): TRect;
    class function Unscale(Value: Integer): Integer;
    class property CurrentDPI: Integer read FCurrentDPI;
  end;

implementation

class procedure TDPIHelper.Initialize;
begin
  FCurrentDPI := Screen.PixelsPerInch;
end;

class function TDPIHelper.Scale(Value: Integer): Integer;
begin
  Result := MulDiv(Value, FCurrentDPI, 96);
end;

class function TDPIHelper.ScaleFloat(Value: Double): Double;
begin
  Result := Value * FCurrentDPI / 96;
end;

class function TDPIHelper.ScaleRect(const R: TRect): TRect;
begin
  Result.Left := Scale(R.Left);
  Result.Top := Scale(R.Top);
  Result.Right := Scale(R.Right);
  Result.Bottom := Scale(R.Bottom);
end;

class function TDPIHelper.Unscale(Value: Integer): Integer;
begin
  Result := MulDiv(Value, 96, FCurrentDPI);
end;

// Utilisation :
procedure TForm1.FormCreate(Sender: TObject);
begin
  TDPIHelper.Initialize;

  Panel1.Width := TDPIHelper.Scale(200);
  Panel1.Height := TDPIHelper.Scale(100);
end;
```

---

## 4.12.6 Gestion des images haute résolution

### Problème avec les images bitmap

```pascal
// Problème : Image 100×100 pixels sur écran 192 DPI
Image1.Picture.LoadFromFile('icone.png');
// → Apparaît trop petite (50×50 en taille logique)

// Ou si étirée :
Image1.Stretch := True;
// → Apparaît floue/pixelisée
```

### Solution 1 : Images multiples selon DPI

```pascal
procedure TForm1.ChargerImageSelon DPI;
var
  NomFichier: string;
begin
  // Choisir l'image selon le DPI
  if Screen.PixelsPerInch >= 192 then
    NomFichier := 'icone@2x.png'    // 200×200 pixels
  else if Screen.PixelsPerInch >= 144 then
    NomFichier := 'icone@1.5x.png'  // 150×150 pixels
  else
    NomFichier := 'icone.png';      // 100×100 pixels

  if FileExists(NomFichier) then
    Image1.Picture.LoadFromFile(NomFichier)
  else
    Image1.Picture.LoadFromFile('icone.png'); // Fallback
end;
```

### Solution 2 : ImageList avec multi-résolution

```pascal
procedure TForm1.ConfigurerImageListDPI;
begin
  // Delphi supporte les ImageList multi-résolution
  ImageList1.Width := ScaleValue(16);
  ImageList1.Height := ScaleValue(16);

  // Charger différentes tailles
  // ImageList ajustera automatiquement
end;
```

### Solution 3 : Images vectorielles (SVG)

```pascal
// Utiliser des SVG pour une qualité parfaite à toute taille
// Nécessite un composant SVG (TMS, etc.)
procedure TForm1.ChargerSVG;
begin
  // Les SVG s'adaptent parfaitement à n'importe quel DPI
  SVGImage1.LoadFromFile('icone.svg');
  SVGImage1.Width := ScaleValue(100);
  SVGImage1.Height := ScaleValue(100);
  // Rendu parfait quelle que soit la taille
end;
```

### ImageList scalable

```pascal
type
  TFormMain = class(TForm)
    ImageList16: TImageList;   // Images 16×16
    ImageList32: TImageList;   // Images 32×32
    ImageList48: TImageList;   // Images 48×48
  private
    procedure ChoisirImageListSelon DPI;
  end;

procedure TFormMain.ChoisirImageListSelon DPI;
begin
  // Choisir l'ImageList appropriée
  if Screen.PixelsPerInch >= 192 then
  begin
    ToolBar1.Images := ImageList48;  // 200% = 48×48
    MainMenu1.Images := ImageList48;
  end
  else if Screen.PixelsPerInch >= 144 then
  begin
    ToolBar1.Images := ImageList32;  // 150% = 32×32
    MainMenu1.Images := ImageList32;
  end
  else
  begin
    ToolBar1.Images := ImageList16;  // 100% = 16×16
    MainMenu1.Images := ImageList16;
  end;
end;
```

---

## 4.12.7 Changement de DPI à l'exécution

### Détecter le changement de DPI

Windows 10 et supérieur peuvent changer le DPI dynamiquement lorsque vous déplacez une fenêtre entre écrans de DPI différents.

```pascal
type
  TFormMain = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    FDPIActuel: Integer;
  protected
    procedure ChangeScale(M, D: Integer{$IF CompilerVersion >= 31};
      isDpiChange: Boolean{$IFEND}); override;
  public
    procedure AjusterPourNouveauDPI;
  end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FDPIActuel := Screen.PixelsPerInch;
end;

procedure TFormMain.ChangeScale(M, D: Integer{$IF CompilerVersion >= 31};
  isDpiChange: Boolean{$IFEND});
begin
  inherited;

  {$IF CompilerVersion >= 31}
  if isDpiChange then
  {$IFEND}
  begin
    // Le DPI a changé
    AjusterPourNouveauDPI;
  end;
end;

procedure TFormMain.AjusterPourNouveauDPI;
begin
  // Recalculer les éléments qui ne se scalent pas automatiquement

  // Recharger les images appropriées
  ChargerImageSelonDPI;

  // Ajuster les ImageList
  ChoisirImageListSelonDPI;

  // Redessiner les éléments personnalisés
  Invalidate;

  // Mettre à jour le titre
  Caption := Format('Mon Application - DPI: %d', [Screen.PixelsPerInch]);

  FDPIActuel := Screen.PixelsPerInch;
end;
```

### Message Windows WM_DPICHANGED

```pascal
type
  TFormMain = class(TForm)
  private
    procedure WMDpiChanged(var Message: TMessage); message WM_DPICHANGED;
  end;

procedure TFormMain.WMDpiChanged(var Message: TMessage);
var
  NouveauDPI: Integer;
  SuggestedRect: PRect;
begin
  // Nouveau DPI
  NouveauDPI := HiWord(Message.WParam);

  // Rectangle suggéré par Windows
  SuggestedRect := PRect(Message.LParam);

  // Ajuster la fenêtre
  if Assigned(SuggestedRect) then
  begin
    SetBounds(
      SuggestedRect^.Left,
      SuggestedRect^.Top,
      SuggestedRect^.Right - SuggestedRect^.Left,
      SuggestedRect^.Bottom - SuggestedRect^.Top
    );
  end;

  // Laisser Delphi gérer le reste
  inherited;
end;
```

---

## 4.12.8 Tests sur différents DPI

### Simuler différents DPI

**Méthode 1 : Paramètres Windows**

```
Windows 10/11 :
1. Paramètres → Système → Affichage
2. "Mise à l'échelle" : Changer le pourcentage
   - 100% = 96 DPI
   - 125% = 120 DPI
   - 150% = 144 DPI
   - 200% = 192 DPI
3. Se déconnecter/reconnecter (ou redémarrer)
4. Tester votre application
```

**Méthode 2 : Propriétés de compatibilité**

```
1. Clic droit sur l'exécutable → Propriétés
2. Onglet Compatibilité
3. "Remplacer le comportement de mise à l'échelle haute résolution"
4. Tester différentes options :
   - Application
   - Système
   - Système (amélioré)
```

**Méthode 3 : Écrans virtuels**

```
Utiliser une machine virtuelle avec différentes résolutions :
- VM à 1920×1080 avec 100% scaling
- VM à 3840×2160 avec 200% scaling
```

### Checklist de test DPI

```
Tests à effectuer :

□ 96 DPI (100% - Standard)
  □ Interface complète visible
  □ Texte lisible
  □ Boutons cliquables
  □ Images nettes

□ 120 DPI (125% - Courant)
  □ Mise à l'échelle correcte
  □ Pas de texte coupé
  □ Boutons bien dimensionnés
  □ Images adaptées

□ 144 DPI (150% - Laptops)
  □ Interface proportionnelle
  □ Texte bien dimensionné
  □ Marges correctes
  □ Icônes appropriées

□ 192 DPI (200% - 4K)
  □ Tout bien scalé
  □ Aucun flou
  □ Images haute qualité
  □ Performance acceptable

□ Changement dynamique
  □ Déplacer entre écrans différents
  □ Pas de crash
  □ Ajustement correct
  □ Images mises à jour
```

### Outil de test DPI

```pascal
unit FormTestDPI;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFormTestDPI = class(TForm)
    MemoInfo: TMemo;
    ButtonRafraichir: TButton;
    PanelTest: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRafraichirClick(Sender: TObject);
  private
    procedure AfficherInfosDPI;
    procedure DessinerRegle;
  end;

var
  FormTestDPI: TFormTestDPI;

implementation

{$R *.dfm}

procedure TFormTestDPI.FormCreate(Sender: TObject);
begin
  Scaled := True;
  AfficherInfosDPI;
  DessinerRegle;
end;

procedure TFormTestDPI.AfficherInfosDPI;
begin
  MemoInfo.Lines.Clear;
  MemoInfo.Lines.Add('=== Informations DPI ===');
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add(Format('DPI actuel : %d', [Screen.PixelsPerInch]));
  MemoInfo.Lines.Add(Format('Scaling : %d%%',
    [Round(Screen.PixelsPerInch / 96 * 100)]));
  MemoInfo.Lines.Add(Format('DPI de conception : %d', [PixelsPerInch]));
  MemoInfo.Lines.Add(Format('Facteur d''échelle : %.2fx',
    [Screen.PixelsPerInch / PixelsPerInch]));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('=== Écran ===');
  MemoInfo.Lines.Add(Format('Largeur : %d pixels', [Screen.Width]));
  MemoInfo.Lines.Add(Format('Hauteur : %d pixels', [Screen.Height]));
  MemoInfo.Lines.Add('');
  MemoInfo.Lines.Add('=== Formulaire ===');
  MemoInfo.Lines.Add(Format('Largeur : %d pixels', [Width]));
  MemoInfo.Lines.Add(Format('Hauteur : %d pixels', [Height]));
  MemoInfo.Lines.Add(Format('Scaled : %s', [BoolToStr(Scaled, True)]));
end;

procedure TFormTestDPI.DessinerRegle;
var
  i, Largeur: Integer;
  ScaleFactor: Double;
begin
  ScaleFactor := Screen.PixelsPerInch / 96;

  with PanelTest.Canvas do
  begin
    Brush.Color := clWhite;
    FillRect(PanelTest.ClientRect);

    Pen.Color := clBlack;

    // Dessiner une règle graduée
    for i := 0 to 10 do
    begin
      Largeur := Round(i * 50 * ScaleFactor);

      // Trait
      MoveTo(Largeur, 0);
      LineTo(Largeur, 20);

      // Texte
      TextOut(Largeur + 2, 5, Format('%d cm', [i * 5]));
    end;
  end;
end;

procedure TFormTestDPI.ButtonRafraichirClick(Sender: TObject);
begin
  AfficherInfosDPI;
  DessinerRegle;
end;

end.
```

---

## 4.12.9 Problèmes courants et solutions

### Problème 1 : Interface floue

**Symptôme :**
L'application apparaît floue sur un écran haute résolution.

**Cause :**
Le manifeste n'est pas configuré ou Scaled = False.

**Solution :**
```pascal
1. Vérifier le manifeste :
   - dpiAware doit être true
   - dpiAwareness doit être PerMonitorV2

2. Vérifier Scaled :
   Self.Scaled := True;

3. Recompiler complètement l'application
```

### Problème 2 : Texte coupé

**Symptôme :**
Le texte des labels est coupé sur certains DPI.

**Cause :**
Propriété AutoSize = False ou composants trop petits.

**Solution :**
```pascal
// Pour les labels
Label1.AutoSize := True;
Label1.WordWrap := True; // Si multiligne

// Pour les boutons
Button1.AutoSize := True;

// Ou définir des tailles minimales
procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Constraints.MinWidth := ScaleValue(100);
  Button1.Constraints.MinHeight := ScaleValue(25);
end;
```

### Problème 3 : Boutons trop petits

**Symptôme :**
Les boutons sont difficiles à cliquer sur écran tactile/haute résolution.

**Cause :**
Tailles codées en dur trop petites.

**Solution :**
```pascal
// Tailles minimales recommandées (à 96 DPI)
const
  HAUTEUR_BOUTON_MIN = 32;  // Minimum pour toucher
  LARGEUR_BOUTON_MIN = 80;
  TAILLE_ICONE_MIN = 24;

procedure TForm1.ConfigurerBoutons;
begin
  Button1.Height := ScaleValue(HAUTEUR_BOUTON_MIN);
  Button1.Width := ScaleValue(LARGEUR_BOUTON_MIN);
end;
```

### Problème 4 : Images pixelisées

**Symptôme :**
Les icônes et images apparaissent floues.

**Cause :**
Images en résolution unique étirées.

**Solution :**
```pascal
// Utiliser plusieurs résolutions
procedure ChargerIconeAdaptee;
begin
  if Screen.PixelsPerInch >= 192 then
    ImageList1.LoadFromFile('icones@2x.png')
  else if Screen.PixelsPerInch >= 144 then
    ImageList1.LoadFromFile('icones@1.5x.png')
  else
    ImageList1.LoadFromFile('icones.png');
end;

// Ou utiliser des SVG
```

### Problème 5 : Layout cassé lors du changement de DPI

**Symptôme :**
Les composants se chevauchent ou sont mal alignés.

**Cause :**
Utilisation de positions absolues au lieu d'alignements.

**Solution :**
```pascal
// Préférer les propriétés d'alignement
Panel1.Align := alTop;
Panel2.Align := alClient;
Button1.Align := alBottom;

// Ou utiliser les ancres
Button1.Anchors := [akRight, akBottom];

// Au lieu de
Button1.Left := 500;  // Position absolue = problème
Button1.Top := 400;
```

### Problème 6 : Police trop petite/grande

**Symptôme :**
Les polices ne s'adaptent pas correctement.

**Cause :**
Font.PixelsPerInch non correctement défini.

**Solution :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Laisser Delphi gérer la police
  Font.Name := 'Segoe UI';
  Font.Size := 9;  // Taille en points, pas en pixels

  // Ne PAS utiliser Font.Height directement
  // Font.Height := 12;  // À éviter
end;
```

---

## 4.12.10 Bonnes pratiques

### 1. Toujours activer Scaled

```pascal
// Pour TOUS les formulaires
procedure TFormBase.FormCreate(Sender: TObject);
begin
  Scaled := True;
  AutoScroll := True;
end;
```

### 2. Utiliser des tailles relatives

```pascal
// Bon : Tailles proportionnelles
Panel1.Width := ClientWidth div 2;
Panel2.Height := ClientHeight - Panel1.Height;

// Mauvais : Tailles absolues
Panel1.Width := 400;  // Ne s'adapte pas
```

### 3. Privilégier Align et Anchors

```pascal
// Bon : Utilisation d'Align
PanelGauche.Align := alLeft;
PanelGauche.Width := ScaleValue(200);
PanelDroite.Align := alClient;

// Bon : Utilisation d'Anchors
ButtonOK.Anchors := [akRight, akBottom];
ButtonAnnuler.Anchors := [akRight, akBottom];
```

### 4. Tester régulièrement

```
Workflow recommandé :
1. Développer à 100% (96 DPI)
2. Tester à 125% toutes les semaines
3. Tester à 150% avant chaque release
4. Tester à 200% sur les fonctionnalités critiques
```

### 5. Utiliser AutoSize pour les labels

```pascal
procedure ConfigurerLabels;
begin
  Label1.AutoSize := True;
  Label1.WordWrap := True;  // Si texte long

  // Le label s'adaptera automatiquement
end;
```

### 6. Définir des contraintes minimales

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Empêcher le formulaire d'être trop petit
  Constraints.MinWidth := ScaleValue(800);
  Constraints.MinHeight := ScaleValue(600);

  // Pour les composants individuels
  Edit1.Constraints.MinHeight := ScaleValue(24);
  Button1.Constraints.MinWidth := ScaleValue(80);
  Button1.Constraints.MinHeight := ScaleValue(32);
end;
```

### 7. Documenter les tailles de référence

```pascal
const
  // Toutes les tailles sont définies pour 96 DPI
  // et seront automatiquement scalées

  LARGEUR_PANNEAU_GAUCHE = 250;
  HAUTEUR_BARRE_OUTILS = 40;
  HAUTEUR_BOUTON_STANDARD = 32;
  LARGEUR_BOUTON_STANDARD = 100;
  MARGE_STANDARD = 8;
  ESPACEMENT_STANDARD = 4;
```

### 8. Créer des composants réutilisables

```pascal
type
  TBoutonScalable = class(TButton)
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TBoutonScalable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Configuration par défaut scalable
  Height := MulDiv(32, Screen.PixelsPerInch, 96);
  Constraints.MinHeight := Height;
  Constraints.MinWidth := MulDiv(80, Screen.PixelsPerInch, 96);
end;
```

---

## 4.12.11 Exemple complet : Application DPI-aware

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.Imaging.pngimage;

type
  TFormPrincipal = class(TForm)
    PanelHaut: TPanel;
    PanelGauche: TPanel;
    PanelCentre: TPanel;
    StatusBar1: TStatusBar;
    LabelTitre: TLabel;
    ButtonNouveau: TButton;
    ButtonOuvrir: TButton;
    ButtonEnregistrer: TButton;
    ImageLogo: TImage;
    MemoContenu: TMemo;
    ListBoxNavigation: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  protected
    procedure ChangeScale(M, D: Integer{$IF CompilerVersion >= 31};
      isDpiChange: Boolean{$IFEND}); override;
  private
    FDPIInitial: Integer;
    procedure ConfigurerDPI;
    procedure AjusterLayout;
    procedure ChargerImagesSelonDPI;
    procedure MettreAJourStatusBar;
  public
    { Déclarations publiques }
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

{$R *.dfm}

// Fonction helper
function ScaleValue(Value: Integer): Integer;
begin
  Result := MulDiv(Value, Screen.PixelsPerInch, 96);
end;

procedure TFormPrincipal.FormCreate(Sender: TObject);
begin
  FDPIInitial := Screen.PixelsPerInch;
  ConfigurerDPI;
  AjusterLayout;
  ChargerImagesSelonDPI;
  MettreAJourStatusBar;
end;

procedure TFormPrincipal.ConfigurerDPI;
begin
  // Activer le scaling automatique
  Scaled := True;
  AutoScroll := True;

  // Police principale
  Font.Name := 'Segoe UI';
  Font.Size := 9;

  // Contraintes de taille minimale
  Constraints.MinWidth := ScaleValue(800);
  Constraints.MinHeight := ScaleValue(600);

  // Configuration des panneaux avec tailles scalées
  PanelHaut.Height := ScaleValue(60);
  PanelGauche.Width := ScaleValue(200);

  // Configuration des boutons
  ButtonNouveau.Height := ScaleValue(32);
  ButtonOuvrir.Height := ScaleValue(32);
  ButtonEnregistrer.Height := ScaleValue(32);

  // Marges et espacements
  PanelHaut.Padding.SetBounds(
    ScaleValue(8),
    ScaleValue(8),
    ScaleValue(8),
    ScaleValue(8)
  );

  PanelGauche.Padding.SetBounds(
    ScaleValue(4),
    ScaleValue(4),
    ScaleValue(4),
    ScaleValue(4)
  );
end;

procedure TFormPrincipal.AjusterLayout;
begin
  // Layout réactif avec Align
  PanelHaut.Align := alTop;
  PanelGauche.Align := alLeft;
  PanelCentre.Align := alClient;
  StatusBar1.Align := alBottom;

  // Alignements internes
  MemoContenu.Align := alClient;
  ListBoxNavigation.Align := alClient;

  // Titre centré
  LabelTitre.Align := alClient;
  LabelTitre.Alignment := taCenter;
  LabelTitre.Layout := tlCenter;
  LabelTitre.Font.Size := 14;
  LabelTitre.Font.Style := [fsBold];
end;

procedure TFormPrincipal.ChargerImagesSelonDPI;
var
  CheminImage: string;
begin
  // Choisir l'image selon le DPI
  if Screen.PixelsPerInch >= 192 then
    CheminImage := 'logo@2x.png'
  else if Screen.PixelsPerInch >= 144 then
    CheminImage := 'logo@1.5x.png'
  else
    CheminImage := 'logo.png';

  // Charger si le fichier existe
  if FileExists(CheminImage) then
  begin
    try
      ImageLogo.Picture.LoadFromFile(CheminImage);
      ImageLogo.Proportional := True;
      ImageLogo.Width := ScaleValue(48);
      ImageLogo.Height := ScaleValue(48);
    except
      // Gérer l'erreur silencieusement
    end;
  end;
end;

procedure TFormPrincipal.MettreAJourStatusBar;
begin
  StatusBar1.Panels[0].Text := Format('DPI: %d (%d%%)',
    [Screen.PixelsPerInch,
     Round(Screen.PixelsPerInch / 96 * 100)]);

  StatusBar1.Panels[1].Text := Format('Résolution: %d×%d',
    [Screen.Width, Screen.Height]);
end;

procedure TFormPrincipal.FormResize(Sender: TObject);
begin
  // Ajustements lors du redimensionnement
  MettreAJourStatusBar;
end;

procedure TFormPrincipal.ChangeScale(M, D: Integer{$IF CompilerVersion >= 31};
  isDpiChange: Boolean{$IFEND});
begin
  inherited;

  {$IF CompilerVersion >= 31}
  if isDpiChange then
  {$IFEND}
  begin
    // Le DPI a changé (changement d'écran)
    ChargerImagesSelonDPI;
    MettreAJourStatusBar;

    // Forcer le redessin
    Invalidate;
  end;
end;

end.
```

---

## Conclusion

La gestion correcte du DPI est essentielle pour créer des applications modernes qui fonctionnent parfaitement sur tous les types d'écrans, des anciens moniteurs 1080p aux nouveaux écrans 4K et 8K.

### Points clés à retenir

✅ **Toujours activer Scaled = True** sur tous les formulaires
✅ **Configurer le manifeste** avec PerMonitorV2
✅ **Utiliser des fonctions de scaling** pour les valeurs dynamiques
✅ **Prévoir plusieurs résolutions d'images** (1x, 1.5x, 2x)
✅ **Privilégier Align et Anchors** plutôt que des positions fixes
✅ **Tester régulièrement** à différents DPI
✅ **Définir des contraintes minimales** pour éviter les interfaces cassées
✅ **Utiliser des polices scalables** (taille en points, pas en pixels)

### Avantages d'une application DPI-aware

- ✨ Interface nette sur tous les écrans
- 📱 Fonctionne sur laptops haute résolution
- 🖥️ Parfait sur écrans 4K/5K
- 👥 Meilleure satisfaction utilisateur
- 🏆 Application professionnelle
- 🔮 Prête pour l'avenir

Avec une bonne gestion du DPI, votre application Delphi sera magnifique sur n'importe quel écran, du Full HD au 8K ! 🎨

⏭️ [Modernisation des applications VCL pour Windows 11](/04-conception-dinterfaces-utilisateur-avec-la-vcl/13-modernisation-vcl-pour-windows-11.md)
