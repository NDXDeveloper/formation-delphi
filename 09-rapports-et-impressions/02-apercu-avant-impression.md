🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.2 Aperçu avant impression

## Introduction

L'aperçu avant impression est une fonctionnalité essentielle dans toute application professionnelle. Elle permet aux utilisateurs de visualiser le document avant de l'imprimer réellement, ce qui évite le gaspillage de papier et d'encre, et permet de vérifier la mise en page.

Contrairement à de nombreux environnements de développement, Delphi ne propose pas de composant natif d'aperçu avant impression prêt à l'emploi. Cependant, il existe plusieurs approches pour créer cette fonctionnalité, allant de solutions personnalisées à des composants tiers.

## Pourquoi un aperçu avant impression ?

Les avantages d'un aperçu avant impression sont nombreux :

- **Économie** : réduction du gaspillage de papier et d'encre
- **Contrôle** : vérification de la mise en page avant impression
- **Professionnalisme** : expérience utilisateur améliorée
- **Correction** : possibilité de modifier le document avant impression
- **Navigation** : consultation de documents multi-pages

## Les différentes approches

Il existe trois approches principales pour créer un aperçu avant impression dans Delphi :

1. **Approche manuelle** : créer son propre système d'aperçu en dessinant sur un formulaire
2. **Composant TPrintPreviewDialog** : disponible dans certaines versions de Delphi
3. **Bibliothèques tierces** : utiliser des composants spécialisés

Nous allons explorer chacune de ces approches.

## Approche 1 : Aperçu manuel avec TImage

### Principe de base

L'idée est de dessiner le contenu sur un composant `TImage` au lieu de l'envoyer directement à l'imprimante. Cela permet de prévisualiser le résultat à l'écran.

### Création d'un formulaire d'aperçu

Commençons par créer un formulaire dédié à l'aperçu :

**Étapes :**

1. Créez un nouveau formulaire (File → New → Form)
2. Nommez-le `FormApercu`
3. Ajoutez les composants suivants :
   - Un `TScrollBox` nommé `ScrollBox1` (propriété Align = alClient)
   - Un `TImage` nommé `ImageApercu` à l'intérieur du ScrollBox
   - Un `TPanel` en haut (Align = alTop) pour les boutons
   - Un `TButton` nommé `btnImprimer` avec Caption = 'Imprimer'
   - Un `TButton` nommé `btnFermer` avec Caption = 'Fermer'
   - Un `TButton` nommé `btnZoomPlus` avec Caption = 'Zoom +'
   - Un `TButton` nommé `btnZoomMoins` avec Caption = 'Zoom -'

### Code de base pour l'aperçu

```pascal
unit UApercu;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Printers;

type
  TFormApercu = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    ImageApercu: TImage;
    btnImprimer: TButton;
    btnFermer: TButton;
    btnZoomPlus: TButton;
    btnZoomMoins: TButton;
    procedure btnImprimerClick(Sender: TObject);
    procedure btnFermerClick(Sender: TObject);
    procedure btnZoomPlusClick(Sender: TObject);
    procedure btnZoomMoinsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FZoom: Integer;
    procedure AjusterZoom;
  public
    procedure GenererApercu;
  end;

var
  FormApercu: TFormApercu;

implementation

{$R *.dfm}

procedure TFormApercu.FormCreate(Sender: TObject);
begin
  FZoom := 100;  // Zoom à 100% par défaut
  ImageApercu.Picture.Bitmap := TBitmap.Create;
end;

procedure TFormApercu.GenererApercu;
var
  FacteurEchelle: Double;
begin
  // Créer un bitmap aux dimensions de la page d'impression
  ImageApercu.Picture.Bitmap.Width := Printer.PageWidth;
  ImageApercu.Picture.Bitmap.Height := Printer.PageHeight;

  // Remplir le fond en blanc
  ImageApercu.Picture.Bitmap.Canvas.Brush.Color := clWhite;
  ImageApercu.Picture.Bitmap.Canvas.FillRect(
    Rect(0, 0, ImageApercu.Picture.Bitmap.Width, ImageApercu.Picture.Bitmap.Height)
  );

  // Calculer le facteur d'échelle pour adapter l'impression au bitmap
  FacteurEchelle := ImageApercu.Picture.Bitmap.Width / Screen.PixelsPerInch;

  // Dessiner le contenu (exemple simple)
  ImageApercu.Picture.Bitmap.Canvas.Font.Size := 12;
  ImageApercu.Picture.Bitmap.Canvas.TextOut(100, 100, 'Aperçu du document');
  ImageApercu.Picture.Bitmap.Canvas.TextOut(100, 200, 'Ce texte sera imprimé');

  // Ajuster la taille de l'image selon le zoom
  AjusterZoom;
end;

procedure TFormApercu.AjusterZoom;
begin
  ImageApercu.Width := Round(ImageApercu.Picture.Bitmap.Width * FZoom / 100);
  ImageApercu.Height := Round(ImageApercu.Picture.Bitmap.Height * FZoom / 100);
  ImageApercu.Stretch := True;
  ImageApercu.Proportional := True;
end;

procedure TFormApercu.btnZoomPlusClick(Sender: TObject);
begin
  if FZoom < 200 then
  begin
    FZoom := FZoom + 10;
    AjusterZoom;
  end;
end;

procedure TFormApercu.btnZoomMoinsClick(Sender: TObject);
begin
  if FZoom > 20 then
  begin
    FZoom := FZoom - 10;
    AjusterZoom;
  end;
end;

procedure TFormApercu.btnImprimerClick(Sender: TObject);
begin
  // Imprimer réellement le document
  Printer.BeginDoc;
  try
    // Copier le contenu du bitmap vers l'imprimante
    Printer.Canvas.Draw(0, 0, ImageApercu.Picture.Bitmap);
  finally
    Printer.EndDoc;
  end;
  Close;
end;

procedure TFormApercu.btnFermerClick(Sender: TObject);
begin
  Close;
end;

end.
```

### Utilisation depuis le formulaire principal

Depuis votre formulaire principal, vous pouvez afficher l'aperçu ainsi :

```pascal
procedure TFormPrincipal.btnApercuClick(Sender: TObject);
begin
  FormApercu := TFormApercu.Create(Self);
  try
    FormApercu.GenererApercu;
    FormApercu.ShowModal;
  finally
    FormApercu.Free;
  end;
end;
```

## Amélioration : Utiliser un TMetafile

L'utilisation d'un `TMetafile` au lieu d'un `TBitmap` offre plusieurs avantages :

- **Qualité** : les métafichiers sont vectoriels, donc sans perte de qualité
- **Taille** : fichiers plus légers
- **Compatibilité** : format standard Windows

### Code avec TMetafile

```pascal
procedure TFormApercu.GenererApercuMetafile;
var
  Metafile: TMetafile;
  MetafileCanvas: TMetafileCanvas;
begin
  // Créer un métafichier
  Metafile := TMetafile.Create;
  try
    // Définir les dimensions du métafichier
    Metafile.Width := Printer.PageWidth;
    Metafile.Height := Printer.PageHeight;

    // Créer un canvas pour dessiner dans le métafichier
    MetafileCanvas := TMetafileCanvas.Create(Metafile, 0);
    try
      // Dessiner le contenu
      MetafileCanvas.Font.Size := 12;
      MetafileCanvas.TextOut(100, 100, 'Aperçu avec métafichier');
      MetafileCanvas.TextOut(100, 200, 'Qualité vectorielle préservée');

      // Dessiner une ligne
      MetafileCanvas.Pen.Width := 2;
      MetafileCanvas.MoveTo(100, 300);
      MetafileCanvas.LineTo(500, 300);
    finally
      MetafileCanvas.Free;
    end;

    // Affecter le métafichier à l'image
    ImageApercu.Picture.Assign(Metafile);

    // Ajuster la taille
    AjusterZoom;
  finally
    Metafile.Free;
  end;
end;
```

## Approche 2 : Classe réutilisable pour l'aperçu

Pour rendre le système plus flexible, créons une classe qui peut générer l'aperçu de n'importe quel contenu.

### Définition de la classe

```pascal
type
  TTypeProcedureDessin = procedure(Canvas: TCanvas; PageWidth, PageHeight: Integer) of object;

  TGestionnaireApercu = class
  private
    FProcedureDessin: TTypeProcedureDessin;
    FNombrePages: Integer;
    FPageCourante: Integer;
  public
    constructor Create;
    procedure DefinirProcedureDessin(AProcedure: TTypeProcedureDessin; ANombrePages: Integer = 1);
    procedure GenererPage(ACanvas: TCanvas; APageWidth, APageHeight: Integer; ANumeroPage: Integer);
    property PageCourante: Integer read FPageCourante write FPageCourante;
    property NombrePages: Integer read FNombrePages;
  end;

implementation

constructor TGestionnaireApercu.Create;
begin
  inherited;
  FPageCourante := 1;
  FNombrePages := 1;
end;

procedure TGestionnaireApercu.DefinirProcedureDessin(AProcedure: TTypeProcedureDessin; ANombrePages: Integer);
begin
  FProcedureDessin := AProcedure;
  FNombrePages := ANombrePages;
  FPageCourante := 1;
end;

procedure TGestionnaireApercu.GenererPage(ACanvas: TCanvas; APageWidth, APageHeight: Integer; ANumeroPage: Integer);
begin
  if Assigned(FProcedureDessin) then
  begin
    FPageCourante := ANumeroPage;
    FProcedureDessin(ACanvas, APageWidth, APageHeight);
  end;
end;
```

### Utilisation de la classe

```pascal
procedure TFormPrincipal.DessinerMonDocument(Canvas: TCanvas; PageWidth, PageHeight: Integer);
begin
  // Dessiner le contenu du document
  Canvas.Font.Size := 14;
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(100, 100, 'Mon document');

  Canvas.Font.Size := 12;
  Canvas.Font.Style := [];
  Canvas.TextOut(100, 200, 'Contenu de la page ' + IntToStr(GestionnaireApercu.PageCourante));
end;

procedure TFormPrincipal.btnApercuAvecClasseClick(Sender: TObject);
begin
  // Créer le gestionnaire
  GestionnaireApercu := TGestionnaireApercu.Create;
  try
    // Définir la procédure de dessin
    GestionnaireApercu.DefinirProcedureDessin(DessinerMonDocument, 3);

    // Afficher l'aperçu
    FormApercu := TFormApercu.Create(Self);
    try
      FormApercu.Gestionnaire := GestionnaireApercu;
      FormApercu.GenererApercu;
      FormApercu.ShowModal;
    finally
      FormApercu.Free;
    end;
  finally
    GestionnaireApercu.Free;
  end;
end;
```

## Gestion de plusieurs pages

Pour gérer plusieurs pages dans l'aperçu, ajoutez des boutons de navigation.

### Ajout de boutons de navigation

Ajoutez ces composants dans le Panel de votre formulaire d'aperçu :

- `TButton` nommé `btnPagePrecedente` avec Caption = '< Précédent'
- `TLabel` nommé `lblNumeroPage` avec Caption = 'Page 1 / 1'
- `TButton` nommé `btnPageSuivante` avec Caption = 'Suivant >'

### Code de navigation

```pascal
procedure TFormApercu.btnPagePrecedenteClick(Sender: TObject);
begin
  if Assigned(FGestionnaire) and (FGestionnaire.PageCourante > 1) then
  begin
    FGestionnaire.PageCourante := FGestionnaire.PageCourante - 1;
    GenererApercu;
    MettreAJourAffichagePage;
  end;
end;

procedure TFormApercu.btnPageSuivanteClick(Sender: TObject);
begin
  if Assigned(FGestionnaire) and (FGestionnaire.PageCourante < FGestionnaire.NombrePages) then
  begin
    FGestionnaire.PageCourante := FGestionnaire.PageCourante + 1;
    GenererApercu;
    MettreAJourAffichagePage;
  end;
end;

procedure TFormApercu.MettreAJourAffichagePage;
begin
  lblNumeroPage.Caption := Format('Page %d / %d',
    [FGestionnaire.PageCourante, FGestionnaire.NombrePages]);

  btnPagePrecedente.Enabled := FGestionnaire.PageCourante > 1;
  btnPageSuivante.Enabled := FGestionnaire.PageCourante < FGestionnaire.NombrePages;
end;
```

## Approche 3 : Composants tiers

### TPrintPreview de RxLib

RxLib est une bibliothèque gratuite qui propose un composant `TPrintPreview` très performant.

**Installation :**

1. Téléchargez RxLib depuis GitHub
2. Installez les packages dans Delphi
3. Le composant apparaît dans la palette "Rx Controls"

**Utilisation basique :**

```pascal
uses
  RxRichEd;

procedure TForm1.btnApercuRxClick(Sender: TObject);
begin
  PrintPreview1.BeginDoc;
  try
    PrintPreview1.Canvas.Font.Size := 12;
    PrintPreview1.Canvas.TextOut(100, 100, 'Aperçu avec RxLib');
  finally
    PrintPreview1.EndDoc;
  end;

  PrintPreview1.ShowModal;
end;
```

### Autres bibliothèques

Il existe d'autres bibliothèques offrant des aperçus avant impression :

- **FastReport** : générateur de rapports complet avec aperçu intégré
- **QuickReport** : inclus dans certaines versions de Delphi
- **ReportBuilder** : solution professionnelle payante
- **VCL-Extensions** : composants additionnels pour Delphi

## Fonctionnalités avancées d'aperçu

### Zoom intelligent

Implémentez différents modes de zoom :

```pascal
type
  TModeZoom = (zmAjusterLargeur, zmAjusterHauteur, zmPageEntiere, zmPersonnalise);

procedure TFormApercu.AppliquerModeZoom(Mode: TModeZoom);
begin
  case Mode of
    zmAjusterLargeur:
      begin
        FZoom := Round((ScrollBox1.Width - 20) * 100 / ImageApercu.Picture.Width);
        AjusterZoom;
      end;

    zmAjusterHauteur:
      begin
        FZoom := Round((ScrollBox1.Height - 20) * 100 / ImageApercu.Picture.Height);
        AjusterZoom;
      end;

    zmPageEntiere:
      begin
        // Calculer le zoom pour que toute la page soit visible
        var ZoomLargeur := Round((ScrollBox1.Width - 20) * 100 / ImageApercu.Picture.Width);
        var ZoomHauteur := Round((ScrollBox1.Height - 20) * 100 / ImageApercu.Picture.Height);
        FZoom := Min(ZoomLargeur, ZoomHauteur);
        AjusterZoom;
      end;
  end;
end;
```

### Affichage de plusieurs pages côte à côte

Pour afficher plusieurs pages simultanément :

```pascal
procedure TFormApercu.AfficherDeuxPages;
var
  i: Integer;
  Image: TImage;
  PosX: Integer;
begin
  PosX := 10;

  for i := 0 to 1 do
  begin
    Image := TImage.Create(ScrollBox1);
    Image.Parent := ScrollBox1;
    Image.Left := PosX;
    Image.Top := 10;
    Image.Width := 400;
    Image.Height := 600;

    // Générer l'aperçu de la page i
    GenererPage(Image, FGestionnaire.PageCourante + i);

    PosX := PosX + Image.Width + 10;
  end;
end;
```

### Impression directe depuis l'aperçu

Permettez à l'utilisateur d'imprimer directement depuis l'aperçu :

```pascal
procedure TFormApercu.btnImprimerDepuisApercuClick(Sender: TObject);
var
  PrintDialog: TPrintDialog;
begin
  PrintDialog := TPrintDialog.Create(Self);
  try
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := FGestionnaire.NombrePages;
    PrintDialog.Options := [poPageNums];

    if PrintDialog.Execute then
    begin
      ImprimerPages(PrintDialog.FromPage, PrintDialog.ToPage);
      Close;
    end;
  finally
    PrintDialog.Free;
  end;
end;

procedure TFormApercu.ImprimerPages(DebutPage, FinPage: Integer);
var
  i: Integer;
begin
  Printer.BeginDoc;
  try
    for i := DebutPage to FinPage do
    begin
      FGestionnaire.PageCourante := i;
      FGestionnaire.GenererPage(Printer.Canvas, Printer.PageWidth, Printer.PageHeight, i);

      if i < FinPage then
        Printer.NewPage;
    end;
  finally
    Printer.EndDoc;
  end;
end;
```

## Exportation depuis l'aperçu

Ajoutez la possibilité d'exporter le document vers différents formats.

### Exportation en PDF

Utilisez une bibliothèque comme Gnostice ou Synopse PDF :

```pascal
uses
  SynPdf;

procedure TFormApercu.ExporterEnPDF(const NomFichier: string);
var
  PDF: TPdfDocument;
  Page: TPdfPage;
  i: Integer;
begin
  PDF := TPdfDocument.Create;
  try
    for i := 1 to FGestionnaire.NombrePages do
    begin
      Page := PDF.AddPage;
      FGestionnaire.PageCourante := i;

      // Dessiner sur la page PDF
      FGestionnaire.GenererPage(Page.Canvas, Round(595.28), Round(841.89), i);
    end;

    PDF.SaveToFile(NomFichier);
    ShowMessage('Document exporté en PDF : ' + NomFichier);
  finally
    PDF.Free;
  end;
end;
```

### Exportation en image

Exportez l'aperçu comme image :

```pascal
procedure TFormApercu.ExporterEnImage(const NomFichier: string);
begin
  case ExtractFileExt(NomFichier).ToLower of
    '.bmp': ImageApercu.Picture.Bitmap.SaveToFile(NomFichier);
    '.jpg', '.jpeg':
      begin
        var JPEG := TJPEGImage.Create;
        try
          JPEG.Assign(ImageApercu.Picture.Bitmap);
          JPEG.CompressionQuality := 90;
          JPEG.SaveToFile(NomFichier);
        finally
          JPEG.Free;
        end;
      end;
    '.png':
      begin
        var PNG := TPngImage.Create;
        try
          PNG.Assign(ImageApercu.Picture.Bitmap);
          PNG.SaveToFile(NomFichier);
        finally
          PNG.Free;
        end;
      end;
  end;

  ShowMessage('Image exportée : ' + NomFichier);
end;
```

## Optimisation des performances

### Mise en cache des pages

Pour améliorer les performances avec des documents multi-pages :

```pascal
type
  TCachePages = class
  private
    FPages: TObjectList<TBitmap>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterPage(Bitmap: TBitmap);
    function ObtenirPage(Index: Integer): TBitmap;
    procedure Vider;
  end;

constructor TCachePages.Create;
begin
  inherited;
  FPages := TObjectList<TBitmap>.Create(True);
end;

destructor TCachePages.Destroy;
begin
  FPages.Free;
  inherited;
end;

procedure TCachePages.AjouterPage(Bitmap: TBitmap);
var
  NouvellePage: TBitmap;
begin
  NouvellePage := TBitmap.Create;
  NouvellePage.Assign(Bitmap);
  FPages.Add(NouvellePage);
end;

function TCachePages.ObtenirPage(Index: Integer): TBitmap;
begin
  if (Index >= 0) and (Index < FPages.Count) then
    Result := FPages[Index]
  else
    Result := nil;
end;

procedure TCachePages.Vider;
begin
  FPages.Clear;
end;
```

### Génération asynchrone

Pour ne pas bloquer l'interface utilisateur :

```pascal
procedure TFormApercu.GenererApercuAsyncrone;
begin
  TTask.Run(
    procedure
    var
      Metafile: TMetafile;
      MetafileCanvas: TMetafileCanvas;
    begin
      Metafile := TMetafile.Create;
      try
        Metafile.Width := Printer.PageWidth;
        Metafile.Height := Printer.PageHeight;

        MetafileCanvas := TMetafileCanvas.Create(Metafile, 0);
        try
          FGestionnaire.GenererPage(MetafileCanvas,
            Printer.PageWidth, Printer.PageHeight, FGestionnaire.PageCourante);
        finally
          MetafileCanvas.Free;
        end;

        // Mise à jour de l'interface dans le thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            ImageApercu.Picture.Assign(Metafile);
            AjusterZoom;
          end
        );
      finally
        Metafile.Free;
      end;
    end
  );
end;
```

## Conseils et bonnes pratiques

### Interface utilisateur

- **Boutons clairs** : utilisez des icônes reconnaissables (imprimante, loupe, flèches)
- **Raccourcis clavier** : ajoutez des raccourcis (Ctrl+P pour imprimer, Échap pour fermer)
- **Indicateur de progression** : pour les documents volumineux
- **État sauvegardé** : mémorisez le niveau de zoom de l'utilisateur

### Performances

- **Génération à la demande** : ne générez que les pages visibles
- **Utilisation de métafichiers** : pour une meilleure qualité et moins de mémoire
- **Cache intelligent** : gardez en cache les pages récemment consultées
- **Threads** : déportez la génération dans un thread séparé

### Compatibilité

- **Test multi-résolutions** : testez sur différentes résolutions d'écran
- **Respect du DPI** : adaptez l'affichage selon le DPI de l'écran
- **Gestion des erreurs** : prévoyez les cas où l'imprimante n'est pas disponible

## Exemple complet : Aperçu professionnel

Voici un exemple combinant plusieurs fonctionnalités :

```pascal
unit UApercuProfessionnel;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Printers;

type
  TFormApercuPro = class(TForm)
    PanelHaut: TPanel;
    ScrollBox1: TScrollBox;
    ImageApercu: TImage;
    btnImprimer: TButton;
    btnExporter: TButton;
    btnFermer: TButton;
    TrackBarZoom: TTrackBar;
    lblZoom: TLabel;
    btnPagePrec: TButton;
    btnPageSuiv: TButton;
    lblPage: TLabel;
    ComboModeAffichage: TComboBox;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnImprimerClick(Sender: TObject);
    procedure btnExporterClick(Sender: TObject);
    procedure btnFermerClick(Sender: TObject);
    procedure TrackBarZoomChange(Sender: TObject);
    procedure btnPagePrecClick(Sender: TObject);
    procedure btnPageSuivClick(Sender: TObject);
    procedure ComboModeAffichageChange(Sender: TObject);
  private
    FPageCourante: Integer;
    FNombrePages: Integer;
    procedure GenererApercu;
    procedure MettreAJourAffichage;
    procedure AppliquerZoom;
  end;

var
  FormApercuPro: TFormApercuPro;

implementation

{$R *.dfm}

procedure TFormApercuPro.FormCreate(Sender: TObject);
begin
  FPageCourante := 1;
  FNombrePages := 5;  // Exemple avec 5 pages

  TrackBarZoom.Min := 20;
  TrackBarZoom.Max := 200;
  TrackBarZoom.Position := 100;

  ComboModeAffichage.Items.Add('Page entière');
  ComboModeAffichage.Items.Add('Largeur de page');
  ComboModeAffichage.Items.Add('Deux pages');
  ComboModeAffichage.ItemIndex := 0;

  GenererApercu;
  MettreAJourAffichage;
end;

procedure TFormApercuPro.GenererApercu;
var
  Metafile: TMetafile;
  MetafileCanvas: TMetafileCanvas;
begin
  Metafile := TMetafile.Create;
  try
    Metafile.Width := Printer.PageWidth;
    Metafile.Height := Printer.PageHeight;

    MetafileCanvas := TMetafileCanvas.Create(Metafile, 0);
    try
      // Fond blanc
      MetafileCanvas.Brush.Color := clWhite;
      MetafileCanvas.FillRect(Rect(0, 0, Metafile.Width, Metafile.Height));

      // Contenu de la page
      MetafileCanvas.Font.Size := 16;
      MetafileCanvas.Font.Style := [fsBold];
      MetafileCanvas.TextOut(100, 100, 'Document d''exemple');

      MetafileCanvas.Font.Size := 12;
      MetafileCanvas.Font.Style := [];
      MetafileCanvas.TextOut(100, 200, 'Page ' + IntToStr(FPageCourante) + ' sur ' + IntToStr(FNombrePages));
      MetafileCanvas.TextOut(100, 300, 'Ceci est un aperçu professionnel');
    finally
      MetafileCanvas.Free;
    end;

    ImageApercu.Picture.Assign(Metafile);
  finally
    Metafile.Free;
  end;

  AppliquerZoom;
end;

procedure TFormApercuPro.AppliquerZoom;
var
  ZoomPourcent: Integer;
begin
  ZoomPourcent := TrackBarZoom.Position;
  ImageApercu.Width := Round(ImageApercu.Picture.Width * ZoomPourcent / 100);
  ImageApercu.Height := Round(ImageApercu.Picture.Height * ZoomPourcent / 100);
  ImageApercu.Stretch := True;
  ImageApercu.Proportional := True;
  lblZoom.Caption := Format('Zoom : %d%%', [ZoomPourcent]);
end;

procedure TFormApercuPro.MettreAJourAffichage;
begin
  lblPage.Caption := Format('Page %d / %d', [FPageCourante, FNombrePages]);
  btnPagePrec.Enabled := FPageCourante > 1;
  btnPageSuiv.Enabled := FPageCourante < FNombrePages;
end;

procedure TFormApercuPro.TrackBarZoomChange(Sender: TObject);
begin
  AppliquerZoom;
end;

procedure TFormApercuPro.btnPagePrecClick(Sender: TObject);
begin
  if FPageCourante > 1 then
  begin
    Dec(FPageCourante);
    GenererApercu;
    MettreAJourAffichage;
  end;
end;

procedure TFormApercuPro.btnPageSuivClick(Sender: TObject);
begin
  if FPageCourante < FNombrePages then
  begin
    Inc(FPageCourante);
    GenererApercu;
    MettreAJourAffichage;
  end;
end;

procedure TFormApercuPro.btnImprimerClick(Sender: TObject);
var
  i: Integer;
begin
  if MessageDlg('Imprimer toutes les pages ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Printer.BeginDoc;
    try
      for i := 1 to FNombrePages do
      begin
        FPageCourante := i;
        GenererApercu;
        Printer.Canvas.Draw(0, 0, ImageApercu.Picture.Graphic);
        if i < FNombrePages then
          Printer.NewPage;
      end;
    finally
      Printer.EndDoc;
    end;
    ShowMessage('Impression terminée');
  end;
end;

procedure TFormApercuPro.btnExporterClick(Sender: TObject);
begin
  SaveDialog1.Filter := 'Image PNG|*.png|Image JPEG|*.jpg|Bitmap|*.bmp';
  if SaveDialog1.Execute then
  begin
    ImageApercu.Picture.SaveToFile(SaveDialog1.FileName);
    ShowMessage('Document exporté : ' + SaveDialog1.FileName);
  end;
end;

procedure TFormApercuPro.btnFermerClick(Sender: TObject);
begin
  Close;
end;

procedure TFormApercuPro.ComboModeAffichageChange(Sender: TObject);
begin
  case ComboModeAffichage.ItemIndex of
    0: TrackBarZoom.Position := 100;  // Page entière
    1: TrackBarZoom.Position := Round((ScrollBox1.Width - 20) * 100 / ImageApercu.Picture.Width);
    2: TrackBarZoom.Position := 50;   // Deux pages (simulation)
  end;
end;

end.
```

## Résumé

L'aperçu avant impression est une fonctionnalité essentielle qui améliore considérablement l'expérience utilisateur. Les points clés à retenir :

- **Plusieurs approches possibles** : manuelle avec TImage, métafichiers, ou composants tiers
- **TMetafile recommandé** : qualité vectorielle et performances optimales
- **Fonctionnalités essentielles** : zoom, navigation multi-pages, impression et export
- **Performances** : utilisez le cache et la génération asynchrone pour les gros documents
- **Composants tiers** : pour des fonctionnalités avancées sans réinventer la roue

Dans la prochaine section, nous découvrirons les générateurs de rapports comme FastReport et QuickReport qui offrent des aperçus encore plus sophistiqués avec des fonctionnalités de conception visuelle.

⏭️ [Générateurs de rapports (FastReport, QuickReport)](/09-rapports-et-impressions/03-generateurs-de-rapports.md)
