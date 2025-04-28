# 4.12 Interfaces haute résolution et prise en charge du DPI

Avec la multiplication des écrans haute résolution (HD, 4K, etc.) et des affichages à haute densité de pixels sur les ordinateurs portables et les tablettes, il devient essentiel de s'assurer que vos applications Delphi s'affichent correctement sur tous les types d'écrans. Cette section vous guidera à travers les concepts de base de la prise en charge du DPI (Dots Per Inch - Points Par Pouce) et vous montrera comment adapter vos applications VCL aux interfaces haute résolution.

## Comprendre les concepts de base

### Qu'est-ce que le DPI ?

Le DPI (Dots Per Inch) mesure la densité de pixels sur un écran. Plus la valeur DPI est élevée, plus les éléments à l'écran apparaissent petits à taille physique égale.

- **DPI standard** : 96 DPI (échelle 100%)
- **Écrans haute résolution** : 120 DPI (125%), 144 DPI (150%), 192 DPI (200%), etc.

Windows permet aux utilisateurs de modifier l'échelle d'affichage pour améliorer la lisibilité sur les écrans haute résolution. Sans prise en charge appropriée du DPI, votre application peut apparaître floue ou trop petite.

### Problèmes courants sur les écrans haute résolution

Sans adaptation spécifique, les applications Delphi peuvent rencontrer plusieurs problèmes sur les écrans haute résolution :

1. **Texte et contrôles trop petits** : difficiles à lire et à utiliser
2. **Images pixelisées** : les icônes et autres éléments graphiques apparaissent flous
3. **Problèmes de mise en page** : éléments mal alignés ou se chevauchant
4. **Zones cliquables inexactes** : décalage entre la position visuelle et la zone interactive

## Activer la prise en charge du DPI dans votre application

Delphi offre plusieurs mécanismes pour gérer les écrans haute résolution. Voici comment les configurer :

### Configuration de base

Modifiez votre fichier de projet (.dpr) pour activer la prise en charge du DPI :

```pascal
program MonApplication;

uses
  Vcl.Forms,
  UnitPrincipale in 'UnitPrincipale.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;

  // Activer la mise à l'échelle DPI
  Application.MainFormOnTaskbar := True;
  Application.ScaleForCurrentDpi := True;

  Application.CreateForm(TForm1, Form1);
  Application.Run;
end;
```

### Propriétés de mise à l'échelle pour les formulaires

Pour chaque formulaire, vous pouvez configurer les propriétés suivantes :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer la mise à l'échelle
  Scaled := True;

  // Vous pouvez également définir manuellement le PixelsPerInch
  // PixelsPerInch := 96; // Valeur par défaut
end;
```

## Techniques d'adaptation pour différentes résolutions

### 1. Approche réactive avec Align et Anchors

Utilisez les propriétés `Align` et `Anchors` pour que vos contrôles s'adaptent automatiquement au redimensionnement du formulaire :

```pascal
// Exemple d'utilisation d'Align
Panel1.Align := alTop;        // Aligné en haut, largeur complète
Panel2.Align := alBottom;     // Aligné en bas, largeur complète
Panel3.Align := alLeft;       // Aligné à gauche, hauteur complète
Panel4.Align := alRight;      // Aligné à droite, hauteur complète
Panel5.Align := alClient;     // Remplit l'espace restant

// Exemple d'utilisation d'Anchors
Button1.Anchors := [akRight, akBottom];  // Reste ancré en bas à droite
Edit1.Anchors := [akLeft, akTop, akRight]; // S'étire horizontalement
Memo1.Anchors := [akLeft, akTop, akRight, akBottom]; // S'étire dans toutes les directions
```

### 2. Gestion des images et icônes

Pour les images et icônes, vous devez fournir des versions haute résolution :

#### Utilisation de TImageCollection et TVirtualImageList (Delphi 10.3 et supérieur)

```pascal
procedure ConfigurerImages;
var
  ImageCollection: TImageCollection;
  VirtualImageList: TVirtualImageList;
begin
  // Créer la collection d'images
  ImageCollection := TImageCollection.Create(Self);
  ImageCollection.Name := 'ImageCollection1';

  // Ajouter des images à différentes résolutions
  with ImageCollection.Images.Add do
  begin
    Name := 'Save';
    SourceImages.Add.Scale := 1.0;  // 100% (96 DPI)
    SourceImages.Add.Scale := 1.5;  // 150% (144 DPI)
    SourceImages.Add.Scale := 2.0;  // 200% (192 DPI)

    SourceImages[0].ImageSource := TSvgImageSource.Create;
    TSvgImageSource(SourceImages[0].ImageSource).SVG.Text := LoadSVG('save_96dpi.svg');

    SourceImages[1].ImageSource := TSvgImageSource.Create;
    TSvgImageSource(SourceImages[1].ImageSource).SVG.Text := LoadSVG('save_144dpi.svg');

    SourceImages[2].ImageSource := TSvgImageSource.Create;
    TSvgImageSource(SourceImages[2].ImageSource).SVG.Text := LoadSVG('save_192dpi.svg');
  end;

  // Créer la liste d'images virtuelle
  VirtualImageList := TVirtualImageList.Create(Self);
  VirtualImageList.Name := 'VirtualImageList1';
  VirtualImageList.ImageCollection := ImageCollection;
  VirtualImageList.DPI := Screen.PixelsPerInch;

  // Utiliser la liste d'images
  Button1.Images := VirtualImageList;
  Button1.ImageIndex := 0;
end;
```

#### Utilisation des SVG (recommandé pour Delphi 11 et supérieur)

Les images SVG (Scalable Vector Graphics) sont idéales pour les interfaces haute résolution car elles s'adaptent automatiquement à n'importe quelle taille sans perte de qualité.

```pascal
uses
  Vcl.VirtualImage, Vcl.SVGIconImageList;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer une liste d'icônes SVG
  SVGIconImageList1 := TSVGIconImageList.Create(Self);
  SVGIconImageList1.Name := 'SVGIconImageList1';
  SVGIconImageList1.Size := Round(16 * Screen.PixelsPerInch / 96);  // Adapter la taille au DPI

  // Ajouter une icône SVG
  SVGIconImageList1.SVGIconItems.Add.SVG.Text := LoadSVG('save.svg');

  // Utiliser l'icône
  Button1.Images := SVGIconImageList1;
  Button1.ImageIndex := 0;
end;
```

### 3. Adaptation de la taille des polices

Les polices doivent également être mises à l'échelle correctement :

```pascal
procedure AjusterTaillesPolice(Control: TWinControl; Facteur: Double);
var
  i: Integer;
begin
  // Ajuster la taille de police du contrôle actuel
  Control.Font.Size := Round(Control.Font.Size * Facteur);

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TControl then
      TControl(Control.Controls[i]).Font.Size := Round(TControl(Control.Controls[i]).Font.Size * Facteur);

    // Récursion pour les contrôles conteneurs
    if Control.Controls[i] is TWinControl then
      AjusterTaillesPolice(TWinControl(Control.Controls[i]), Facteur);
  end;
end;
```

## Détection et gestion des changements de DPI

### Détection du DPI actuel

```pascal
function GetCurrentDPI: Integer;
begin
  Result := Screen.PixelsPerInch;
end;

function GetScaleFactor: Double;
begin
  Result := Screen.PixelsPerInch / 96;  // 96 DPI est la valeur de référence
end;
```

### Réaction aux changements de DPI

À partir de Delphi 10.3, vous pouvez réagir aux changements de DPI à l'exécution (par exemple, lorsque l'utilisateur déplace l'application sur un autre écran) :

```pascal
procedure TForm1.AfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  // Code à exécuter après un changement de DPI
  StatusBar1.SimpleText := Format('DPI changé de %d à %d', [OldDPI, NewDPI]);

  // Vous pouvez ajuster manuellement certains éléments si nécessaire
  AjusterElementsSpecifiques(NewDPI / OldDPI);
end;

procedure TForm1.BeforeMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  // Code à exécuter avant un changement de DPI
  // Utile pour sauvegarder des états ou préparer la transition
end;
```

## Exemple complet d'une application compatible haute résolution

Voici un exemple complet montrant comment créer un formulaire qui s'adapte bien aux différentes résolutions :

```pascal
unit FormPrincipal;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.VirtualImageList,
  Vcl.SVGIconImageList;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    SplitterVertical: TSplitter;
    TreeView1: TTreeView;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    SVGIconImageList1: TSVGIconImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
  private
    { Déclarations privées }
    procedure ChargerIcônes;
    procedure AfficherInfosDPI;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := 'Application compatible haute résolution';

  // Configuration du layout responsive
  Panel1.Align := alLeft;
  Panel1.Width := Round(250 * Screen.PixelsPerInch / 96);

  Panel2.Align := alClient;

  SplitterVertical.Align := alLeft;
  SplitterVertical.Left := Panel1.Width;

  TreeView1.Align := alClient;
  ListView1.Align := alClient;

  // Configurer les événements DPI
  OnAfterMonitorDpiChanged := FormAfterMonitorDpiChanged;

  // Charger les icônes adaptées au DPI
  ChargerIcônes;

  // Afficher les informations DPI actuelles
  AfficherInfosDPI;
end;

procedure TForm1.ChargerIcônes;
begin
  // Configuration de la liste d'icônes SVG
  SVGIconImageList1.Scaled := True;
  SVGIconImageList1.Size := Round(16 * Screen.PixelsPerInch / 96);

  // Ajouter des icônes (dans un projet réel, chargez de vrais fichiers SVG)
  with SVGIconImageList1.SVGIconItems.Add do
  begin
    Name := 'Save';
    SVG.Text :=
      '<svg viewBox="0 0 24 24">' +
      '<path d="M19 12v7H5v-7H3v7c0 1.1.9 2 2 2h14c1.1 0 2-.9 2-2v-7h-2zm-6 ' +
      '.67l2.59-2.58L17 11.5l-5 5-5-5 1.41-1.41L11 12.67V3h2v9.67z"/>' +
      '</svg>';
  end;

  with SVGIconImageList1.SVGIconItems.Add do
  begin
    Name := 'Open';
    SVG.Text :=
      '<svg viewBox="0 0 24 24">' +
      '<path d="M20 6h-8l-2-2H4c-1.1 0-1.99.9-1.99 2L2 18c0 1.1.9 2 2 2h16c1.1 0 2-.9 ' +
      '2-2V8c0-1.1-.9-2-2-2zm0 12H4V8h16v10z"/>' +
      '</svg>';
  end;

  // Associer les icônes aux boutons de la barre d'outils
  ToolButton1.ImageIndex := 0;
  ToolButton2.ImageIndex := 1;
  ToolBar1.Images := SVGIconImageList1;
end;

procedure TForm1.AfficherInfosDPI;
begin
  StatusBar1.SimpleText := Format('DPI actuel: %d (Échelle: %.0f%%)',
                          [Screen.PixelsPerInch, Screen.PixelsPerInch / 96 * 100]);
end;

procedure TForm1.FormAfterMonitorDpiChanged(Sender: TObject; OldDPI, NewDPI: Integer);
begin
  // Mettre à jour les informations DPI
  AfficherInfosDPI;

  // Redimensionner le panneau de gauche proportionnellement
  Panel1.Width := Round(Panel1.Width * NewDPI / OldDPI);

  // Ajuster la taille des icônes
  SVGIconImageList1.Size := Round(16 * NewDPI / 96);
  SVGIconImageList1.Change;
end;

end.
```

## Tester votre application à différentes résolutions

Pour vous assurer que votre application fonctionne correctement à différentes résolutions, voici quelques techniques de test :

### 1. Changer l'échelle d'affichage Windows

1. Cliquez avec le bouton droit sur le bureau
2. Sélectionnez "Paramètres d'affichage"
3. Modifiez l'échelle et la disposition
4. Testez votre application à différentes échelles (100%, 125%, 150%, 200%, etc.)

### 2. Tester dans une VM avec différentes configurations

Utilisez des machines virtuelles avec différentes configurations d'affichage pour tester votre application.

### 3. Utiliser l'émulation de DPI dans Delphi

À partir de Delphi 10.3, vous pouvez émuler différentes résolutions DPI directement dans l'IDE :

1. Ouvrez le formulaire en mode conception
2. Cliquez sur le bouton "DPI Emulation" dans la barre d'outils
3. Sélectionnez différentes échelles pour voir comment votre formulaire s'affichera

## Bonnes pratiques pour les interfaces haute résolution

### 1. Conception depuis le début

- Pensez à la haute résolution dès le début de la conception
- Utilisez les propriétés `Align` et `Anchors` systématiquement
- Organisez votre interface avec des panneaux et des séparateurs

### 2. Utilisation des images

- Préférez les formats vectoriels (SVG) aux formats bitmap
- Si vous utilisez des bitmaps, préparez-les en plusieurs résolutions
- Utilisez `TVirtualImageList` pour gérer automatiquement les différentes échelles

### 3. Polices et tailles

- Évitez de coder en dur les tailles de police
- Utilisez des tailles relatives plutôt que des valeurs fixes
- Testez régulièrement à différentes échelles

### 4. Marges et espacements

- Calculez les marges et espacements en fonction du DPI actuel
- Exemple : `Margin := Round(8 * Screen.PixelsPerInch / 96);`

## Résolution des problèmes courants

### 1. Images floues

**Problème** : Les images apparaissent floues sur les écrans haute résolution.

**Solution** :
- Utilisez des SVG pour les icônes et les images simples
- Fournissez des versions haute résolution pour les images bitmap
- Utilisez `TVirtualImageList` et `TImageCollection`

### 2. Texte trop petit

**Problème** : Le texte est difficile à lire à haute résolution.

**Solution** :
- Activez `ScaleForCurrentDpi` dans votre application
- Assurez-vous que `Scaled := True` est défini pour vos formulaires
- Évitez d'utiliser des polices de taille fixe codées en dur

### 3. Éléments d'interface mal positionnés

**Problème** : Les contrôles se chevauchent ou laissent des espaces indésirables.

**Solution** :
- Utilisez les propriétés `Align` et `Anchors` pour positionner les contrôles
- Organisez l'interface avec des panneaux (`TPanel`) et des séparateurs (`TSplitter`)
- Gérez les événements `OnAfterMonitorDpiChanged` pour ajuster les éléments personnalisés

## Conclusion

La prise en charge des écrans haute résolution est devenue essentielle dans le développement d'applications modernes. Delphi offre les outils nécessaires pour créer des interfaces qui s'adaptent parfaitement à toutes les résolutions d'écran.

En suivant les principes et techniques présentés dans cette section, vous pourrez vous assurer que vos applications VCL offrent une expérience utilisateur optimale sur tous les types d'écrans, des moniteurs standard aux écrans 4K et au-delà.

N'oubliez pas que la clé d'une bonne prise en charge DPI est de l'intégrer dès le début de votre projet, en suivant les bonnes pratiques de conception réactive et en testant régulièrement sur différentes configurations d'affichage.

---

*Exercice pratique : Prenez une application Delphi existante et améliorez sa prise en charge des écrans haute résolution. Activez les options DPI appropriées, remplacez les icônes bitmap par des SVG, et assurez-vous que la mise en page s'adapte correctement au redimensionnement. Testez l'application à différentes échelles (100%, 125%, 150%, 200%) et corrigez les problèmes que vous observez.*
