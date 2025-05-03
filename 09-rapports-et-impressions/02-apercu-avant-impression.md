# 9.2 Aperçu avant impression

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

L'aperçu avant impression est une fonctionnalité essentielle pour les applications qui génèrent des documents. Elle permet aux utilisateurs de visualiser ce qui sera imprimé avant d'envoyer le travail à l'imprimante, économisant ainsi du papier et du temps. Dans cette section, nous allons découvrir comment implémenter cette fonctionnalité dans vos applications Delphi.

## Pourquoi utiliser un aperçu avant impression ?

L'aperçu avant impression offre plusieurs avantages :

- Vérifier la mise en page avant d'imprimer physiquement le document
- Réduire le gaspillage de papier et d'encre
- Permettre à l'utilisateur d'ajuster les paramètres d'impression si nécessaire
- Offrir une meilleure expérience utilisateur

## Implémentation de base d'un aperçu avant impression

Delphi ne fournit pas de composant natif dédié à l'aperçu avant impression, mais nous pouvons en créer un en utilisant les composants existants. Voici une approche simple :

### Méthode 1 : Utilisation d'un TImage pour l'aperçu

Cette approche consiste à dessiner le contenu sur un composant TImage pour le visualiser avant impression.

```pascal
procedure TForm1.CreerApercuSurImage;
var
  ScaleFactor: Double;
begin
  // Calculer le facteur d'échelle pour adapter la page à l'image
  ScaleFactor := Image1.Width / Printer.PageWidth;

  // Effacer l'image
  Image1.Picture.Bitmap.Width := Image1.Width;
  Image1.Picture.Bitmap.Height := Image1.Height;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Rect(0, 0, Image1.Width, Image1.Height));

  // Dessiner le contenu mis à l'échelle
  Image1.Canvas.Font.Name := 'Arial';
  Image1.Canvas.Font.Size := Round(12 * ScaleFactor);
  Image1.Canvas.TextOut(
    Round(100 * ScaleFactor),
    Round(100 * ScaleFactor),
    'Exemple de texte dans l''aperçu'
  );

  // Dessiner un cadre pour simuler les limites de la page
  Image1.Canvas.Pen.Style := psDot;
  Image1.Canvas.Pen.Color := clGray;
  Image1.Canvas.Rectangle(0, 0, Image1.Width - 1, Image1.Height - 1);
end;
```

Cette méthode est simple mais a des limitations : elle n'offre pas de défilement pour les documents multi-pages et l'échelle peut ne pas être précise.

### Méthode 2 : Création d'un formulaire d'aperçu dédié

Une meilleure approche consiste à créer un formulaire dédié à l'aperçu avant impression :

1. Créez un nouveau formulaire (`TPreviewForm`) dans votre projet
2. Ajoutez un `TScrollBox` pour permettre le défilement
3. Ajoutez un `TImage` dans le `TScrollBox` pour afficher chaque page

Voici un exemple d'implémentation :

```pascal
unit PreviewForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ComCtrls, Printers;

type
  TfrmPreview = class(TForm)
    ScrollBox1: TScrollBox;
    Image1: TImage;
    ToolBar1: TToolBar;
    btnPrint: TButton;
    btnClose: TButton;
    btnPrevPage: TButton;
    btnNextPage: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnPrevPageClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
  private
    FPageCount: Integer;
    FCurrentPage: Integer;
    FPages: array of TBitmap;
    procedure CreatePreviewPages;
    procedure DisplayPage(PageNum: Integer);
    procedure UpdateStatus;
  public
    procedure PreparePreview;
    procedure DrawPage(Canvas: TCanvas; PageNum: Integer; ScaleFactor: Double);
  end;

var
  frmPreview: TfrmPreview;

implementation

{$R *.dfm}

procedure TfrmPreview.FormCreate(Sender: TObject);
begin
  FCurrentPage := 1;
  FPageCount := 0;
end;

procedure TfrmPreview.PreparePreview;
begin
  CreatePreviewPages;
  DisplayPage(1);
  UpdateStatus;
end;

procedure TfrmPreview.CreatePreviewPages;
var
  i: Integer;
  Bitmap: TBitmap;
  ScaleFactor: Double;
begin
  // Libérer les anciennes pages
  for i := 0 to Length(FPages) - 1 do
    if Assigned(FPages[i]) then
      FPages[i].Free;

  // Définir le nombre de pages (pour cet exemple: 3 pages)
  FPageCount := 3;
  SetLength(FPages, FPageCount);

  // Calculer l'échelle pour adapter à la zone d'affichage
  ScaleFactor := (ScrollBox1.Width - 20) / Printer.PageWidth;
  if ScaleFactor > 1 then ScaleFactor := 1; // Ne pas agrandir

  // Créer l'aperçu de chaque page
  for i := 0 to FPageCount - 1 do
  begin
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Round(Printer.PageWidth * ScaleFactor);
    Bitmap.Height := Round(Printer.PageHeight * ScaleFactor);

    // Fond blanc
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    // Dessiner le contenu
    DrawPage(Bitmap.Canvas, i + 1, ScaleFactor);

    FPages[i] := Bitmap;
  end;

  FCurrentPage := 1;
end;

procedure TfrmPreview.DisplayPage(PageNum: Integer);
begin
  if (PageNum < 1) or (PageNum > FPageCount) then Exit;

  FCurrentPage := PageNum;

  // Redimensionner l'image pour correspondre à la page
  Image1.Picture.Assign(FPages[PageNum - 1]);
  Image1.Width := FPages[PageNum - 1].Width;
  Image1.Height := FPages[PageNum - 1].Height;

  // Centrer l'image dans le ScrollBox
  Image1.Left := (ScrollBox1.Width - Image1.Width) div 2;
  if Image1.Left < 0 then Image1.Left := 0;

  UpdateStatus;
end;

procedure TfrmPreview.UpdateStatus;
begin
  StatusBar1.SimpleText := Format('Page %d de %d', [FCurrentPage, FPageCount]);
  btnPrevPage.Enabled := FCurrentPage > 1;
  btnNextPage.Enabled := FCurrentPage < FPageCount;
end;

procedure TfrmPreview.btnPrevPageClick(Sender: TObject);
begin
  DisplayPage(FCurrentPage - 1);
end;

procedure TfrmPreview.btnNextPageClick(Sender: TObject);
begin
  DisplayPage(FCurrentPage + 1);
end;

procedure TfrmPreview.btnPrintClick(Sender: TObject);
var
  i: Integer;
begin
  // Imprimer toutes les pages
  Printer.BeginDoc;
  try
    for i := 1 to FPageCount do
    begin
      if i > 1 then Printer.NewPage;
      DrawPage(Printer.Canvas, i, 1.0); // Échelle 1:1 pour l'impression
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TfrmPreview.btnCloseClick(Sender: TObject);
begin
  Close;
end;

// Cette méthode doit être remplacée dans l'application réelle
procedure TfrmPreview.DrawPage(Canvas: TCanvas; PageNum: Integer; ScaleFactor: Double);
var
  Y: Integer;
begin
  // Exemple de dessin pour les différentes pages
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Round(14 * ScaleFactor);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Round(100 * ScaleFactor), Round(100 * ScaleFactor),
    Format('Page exemple %d', [PageNum]));

  Canvas.Font.Style := [];
  Canvas.Font.Size := Round(10 * ScaleFactor);
  Y := Round(150 * ScaleFactor);

  case PageNum of
    1: begin
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Contenu de la première page');
      Y := Y + Round(20 * ScaleFactor);
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Introduction au document');
    end;

    2: begin
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Contenu de la deuxième page');
      Y := Y + Round(20 * ScaleFactor);
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Corps du document');
    end;

    3: begin
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Contenu de la troisième page');
      Y := Y + Round(20 * ScaleFactor);
      Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Conclusion du document');
    end;
  end;

  // Dessiner un cadre autour de la page
  Canvas.Pen.Color := clBlack;
  Canvas.Pen.Style := psSolid;
  Canvas.Rectangle(0, 0, Canvas.ClipRect.Right - 1, Canvas.ClipRect.Bottom - 1);
end;

end.
```

Pour utiliser ce formulaire d'aperçu, voici comment procéder dans votre application principale :

```pascal
procedure TMainForm.btnPreviewClick(Sender: TObject);
begin
  // Créer et configurer le formulaire d'aperçu
  if not Assigned(frmPreview) then
    Application.CreateForm(TfrmPreview, frmPreview);

  // Préparer l'aperçu (crée les pages et affiche la première)
  frmPreview.PreparePreview;

  // Afficher le formulaire d'aperçu
  frmPreview.ShowModal;
end;
```

## Amélioration de l'aperçu avant impression

Pour rendre l'aperçu plus utile et convivial, vous pouvez ajouter les fonctionnalités suivantes :

### 1. Zoom avant/arrière

```pascal
procedure TfrmPreview.ZoomIn;
begin
  CurrentZoom := CurrentZoom * 1.25;
  RefreshPreview;
end;

procedure TfrmPreview.ZoomOut;
begin
  CurrentZoom := CurrentZoom / 1.25;
  RefreshPreview;
end;

procedure TfrmPreview.RefreshPreview;
begin
  // Redessiner avec le nouveau niveau de zoom
  DisplayPage(FCurrentPage);
end;
```

### 2. Aperçu multi-pages (vignettes)

Vous pouvez ajouter un panneau latéral montrant des miniatures de toutes les pages :

```pascal
procedure TfrmPreview.CreateThumbnails;
var
  i: Integer;
  ThumbSize: Integer;
  Thumb: TImage;
begin
  // Vider le panneau des miniatures
  PanelThumbnails.DestroyComponents;

  ThumbSize := 100; // Taille des miniatures

  for i := 0 to FPageCount - 1 do
  begin
    Thumb := TImage.Create(PanelThumbnails);
    Thumb.Parent := PanelThumbnails;
    Thumb.Width := ThumbSize;
    Thumb.Height := Round(ThumbSize * (Printer.PageHeight / Printer.PageWidth));
    Thumb.Left := 5;
    Thumb.Top := 5 + i * (Thumb.Height + 10);
    Thumb.Stretch := True;
    Thumb.Tag := i + 1; // Stocker le numéro de page
    Thumb.OnClick := ThumbClick; // Gestionnaire de clic

    // Créer une miniature à partir de la page
    Thumb.Picture.Assign(ScaleBitmap(FPages[i], ThumbSize));
  end;
end;

procedure TfrmPreview.ThumbClick(Sender: TObject);
begin
  // Afficher la page correspondant à la miniature cliquée
  if Sender is TImage then
    DisplayPage(TImage(Sender).Tag);
end;
```

### 3. Impression des pages sélectionnées

```pascal
procedure TfrmPreview.btnPrintSelectionClick(Sender: TObject);
var
  PrintDialog: TPrintDialog;
begin
  PrintDialog := TPrintDialog.Create(nil);
  try
    PrintDialog.MinPage := 1;
    PrintDialog.MaxPage := FPageCount;
    PrintDialog.FromPage := FCurrentPage;
    PrintDialog.ToPage := FCurrentPage;
    PrintDialog.Options := [poPageNums];

    if PrintDialog.Execute then
    begin
      Printer.BeginDoc;
      try
        // Imprimer les pages sélectionnées
        if PrintDialog.PrintRange = prPageNums then
        begin
          // Imprimer les pages spécifiées
          for var i := PrintDialog.FromPage to PrintDialog.ToPage do
          begin
            if i > PrintDialog.FromPage then Printer.NewPage;
            DrawPage(Printer.Canvas, i, 1.0);
          end;
        end
        else
        begin
          // Imprimer toutes les pages
          for var i := 1 to FPageCount do
          begin
            if i > 1 then Printer.NewPage;
            DrawPage(Printer.Canvas, i, 1.0);
          end;
        end;
      finally
        Printer.EndDoc;
      end;
    end;
  finally
    PrintDialog.Free;
  end;
end;
```

## Exporter l'aperçu en PDF

Une fonctionnalité très appréciée des utilisateurs est l'exportation en PDF. Delphi ne dispose pas de cette fonctionnalité en natif, mais vous pouvez :

1. Utiliser des bibliothèques tierces comme Synopse PDF Engine ou PowerPDF
2. Utiliser l'imprimante PDF de Windows 10/11

Voici un exemple utilisant l'imprimante PDF de Windows :

```pascal
procedure TfrmPreview.btnExportPDFClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  OldPrinterIndex: Integer;
  PDFPrinterIndex: Integer;
  i: Integer;
begin
  // Rechercher l'imprimante PDF de Windows
  PDFPrinterIndex := -1;
  for i := 0 to Printer.Printers.Count - 1 do
    if Pos('PDF', Printer.Printers[i]) > 0 then
    begin
      PDFPrinterIndex := i;
      Break;
    end;

  if PDFPrinterIndex = -1 then
  begin
    ShowMessage('Aucune imprimante PDF n''a été trouvée sur votre système.');
    Exit;
  end;

  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Enregistrer en PDF';
    SaveDialog.DefaultExt := 'pdf';
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';

    if SaveDialog.Execute then
    begin
      // Sauvegarder l'imprimante actuelle
      OldPrinterIndex := Printer.PrinterIndex;

      try
        // Basculer vers l'imprimante PDF
        Printer.PrinterIndex := PDFPrinterIndex;

        // Imprimer toutes les pages vers le PDF
        Printer.BeginDoc;
        try
          for i := 1 to FPageCount do
          begin
            if i > 1 then Printer.NewPage;
            DrawPage(Printer.Canvas, i, 1.0);
          end;
        finally
          Printer.EndDoc;
        end;

        ShowMessage('Le document a été exporté en PDF.');
      finally
        // Restaurer l'imprimante précédente
        Printer.PrinterIndex := OldPrinterIndex;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;
```

Cette approche nécessite une interaction utilisateur pour sélectionner le nom du fichier PDF dans la boîte de dialogue de l'imprimante PDF.

## Exemple pratique : Rapport client avec aperçu

Voici un exemple complet d'un rapport client avec prévisualisation :

```pascal
unit ClientReport;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Printers;

type
  TClientInfo = record
    Name: string;
    Address: string;
    City: string;
    Phone: string;
    Email: string;
    ClientSince: TDateTime;
    Balance: Currency;
  end;

  TPurchase = record
    Date: TDateTime;
    Product: string;
    Quantity: Integer;
    UnitPrice: Currency;
  end;

  TClientReport = class
  private
    FClient: TClientInfo;
    FPurchases: array of TPurchase;
    FCurrentPage: Integer;
    FTotalPages: Integer;

    procedure DrawHeader(Canvas: TCanvas; ScaleFactor: Double);
    procedure DrawFooter(Canvas: TCanvas; ScaleFactor: Double; PageNum: Integer);
    procedure DrawClientInfo(Canvas: TCanvas; ScaleFactor: Double);
    procedure DrawPurchaseTable(Canvas: TCanvas; ScaleFactor: Double; var ItemsDrawn: Integer; StartItem: Integer);
    function GetTotalAmount: Currency;
  public
    constructor Create;
    procedure SetClientData(const Client: TClientInfo);
    procedure AddPurchase(const Purchase: TPurchase);
    procedure ClearPurchases;

    procedure Print;
    procedure DrawPreview(Canvas: TCanvas; PageNum: Integer; ScaleFactor: Double);
    function GetPageCount: Integer;
  end;

implementation

constructor TClientReport.Create;
begin
  inherited;
  FCurrentPage := 1;
  FTotalPages := 1;
  SetLength(FPurchases, 0);
end;

procedure TClientReport.SetClientData(const Client: TClientInfo);
begin
  FClient := Client;
end;

procedure TClientReport.AddPurchase(const Purchase: TPurchase);
var
  Len: Integer;
begin
  Len := Length(FPurchases);
  SetLength(FPurchases, Len + 1);
  FPurchases[Len] := Purchase;
end;

procedure TClientReport.ClearPurchases;
begin
  SetLength(FPurchases, 0);
end;

function TClientReport.GetTotalAmount: Currency;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Length(FPurchases) - 1 do
    Result := Result + FPurchases[i].Quantity * FPurchases[i].UnitPrice;
end;

procedure TClientReport.DrawHeader(Canvas: TCanvas; ScaleFactor: Double);
var
  Y: Integer;
begin
  Y := Round(50 * ScaleFactor);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Round(18 * ScaleFactor);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'RAPPORT CLIENT');

  Y := Y + Round(30 * ScaleFactor);
  Canvas.Font.Size := Round(10 * ScaleFactor);
  Canvas.Font.Style := [];
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Date: ' + FormatDateTime('dd/mm/yyyy', Date));
end;

procedure TClientReport.DrawFooter(Canvas: TCanvas; ScaleFactor: Double; PageNum: Integer);
var
  Y: Integer;
  PageText: string;
begin
  Y := Round((Printer.PageHeight - 100) * ScaleFactor);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Round(8 * ScaleFactor);
  Canvas.Font.Style := [];

  PageText := Format('Page %d sur %d', [PageNum, FTotalPages]);
  Canvas.TextOut(
    Round((Printer.PageWidth - Canvas.TextWidth(PageText) - 50) * ScaleFactor),
    Y,
    PageText
  );

  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Confidentiel - Document généré par Mon Application Delphi');
end;

procedure TClientReport.DrawClientInfo(Canvas: TCanvas; ScaleFactor: Double);
var
  Y: Integer;
begin
  Y := Round(150 * ScaleFactor);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Name := 'Arial';
  Canvas.Font.Size := Round(12 * ScaleFactor);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Informations client:');

  Y := Y + Round(25 * ScaleFactor);
  Canvas.Font.Size := Round(10 * ScaleFactor);
  Canvas.Font.Style := [];
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Nom: ' + FClient.Name);

  Y := Y + Round(20 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Adresse: ' + FClient.Address);

  Y := Y + Round(20 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Ville: ' + FClient.City);

  Y := Y + Round(20 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Téléphone: ' + FClient.Phone);

  Y := Y + Round(20 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Email: ' + FClient.Email);

  Y := Y + Round(20 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Client depuis: ' + FormatDateTime('dd/mm/yyyy', FClient.ClientSince));

  Y := Y + Round(20 * ScaleFactor);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Solde actuel: ' + FormatFloat('#,##0.00 €', FClient.Balance));

  Y := Y + Round(40 * ScaleFactor);
  Canvas.TextOut(Round(100 * ScaleFactor), Y, 'Historique des achats:');
end;

procedure TClientReport.DrawPurchaseTable(Canvas: TCanvas; ScaleFactor: Double; var ItemsDrawn: Integer; StartItem: Integer);
var
  Y, i, MaxItems: Integer;
  ColWidth: array[0..3] of Integer;
  TotalWidth: Integer;
  X: Integer;
begin
  Y := Round(355 * ScaleFactor);

  // Définir la largeur des colonnes
  ColWidth[0] := Round(120 * ScaleFactor); // Date
  ColWidth[1] := Round(250 * ScaleFactor); // Produit
  ColWidth[2] := Round(80 * ScaleFactor);  // Quantité
  ColWidth[3] := Round(100 * ScaleFactor); // Prix unitaire

  TotalWidth := ColWidth[0] + ColWidth[1] + ColWidth[2] + ColWidth[3];
  X := Round(100 * ScaleFactor);

  // En-têtes de colonnes
  Canvas.Font.Size := Round(10 * ScaleFactor);
  Canvas.Font.Style := [fsBold];
  Canvas.TextOut(X, Y, 'Date');
  Inc(X, ColWidth[0]);
  Canvas.TextOut(X, Y, 'Produit');
  Inc(X, ColWidth[1]);
  Canvas.TextOut(X, Y, 'Quantité');
  Inc(X, ColWidth[2]);
  Canvas.TextOut(X, Y, 'Prix unitaire');

  // Ligne de séparation
  Y := Y + Round(20 * ScaleFactor);
  Canvas.Pen.Width := Round(1 * ScaleFactor);
  Canvas.MoveTo(Round(100 * ScaleFactor), Y);
  Canvas.LineTo(Round(100 * ScaleFactor) + TotalWidth, Y);

  // Calculer combien d'éléments peuvent tenir sur cette page
  MaxItems := (Round(Printer.PageHeight * ScaleFactor) - Y - Round(120 * ScaleFactor)) div Round(25 * ScaleFactor);
  if MaxItems > Length(FPurchases) - StartItem then
    MaxItems := Length(FPurchases) - StartItem;

  ItemsDrawn := MaxItems;

  // Lignes de données
  Y := Y + Round(10 * ScaleFactor);
  Canvas.Font.Style := [];

  for i := StartItem to StartItem + MaxItems - 1 do
  begin
    X := Round(100 * ScaleFactor);

    Canvas.TextOut(X, Y, FormatDateTime('dd/mm/yyyy', FPurchases[i].Date));
    Inc(X, ColWidth[0]);

    Canvas.TextOut(X, Y, FPurchases[i].Product);
    Inc(X, ColWidth[1]);

    Canvas.TextOut(X, Y, IntToStr(FPurchases[i].Quantity));
    Inc(X, ColWidth[2]);

    Canvas.TextOut(X, Y, FormatFloat('#,##0.00 €', FPurchases[i].UnitPrice));

    Y := Y + Round(25 * ScaleFactor);
  end;

  // Si c'est la dernière page, afficher le total
  if StartItem + MaxItems >= Length(FPurchases) then
  begin
    Y := Y + Round(10 * ScaleFactor);
    Canvas.Pen.Width := Round(1 * ScaleFactor);
    Canvas.MoveTo(Round(100 * ScaleFactor), Y);
    Canvas.LineTo(Round(100 * ScaleFactor) + TotalWidth, Y);

    Y := Y + Round(20 * ScaleFactor);
    Canvas.Font.Style := [fsBold];
    X := Round(100 * ScaleFactor) + ColWidth[0] + ColWidth[1];
    Canvas.TextOut(X, Y, 'TOTAL:');

    X := X + ColWidth[2];
    Canvas.TextOut(X, Y, FormatFloat('#,##0.00 €', GetTotalAmount));
  end;
end;

function TClientReport.GetPageCount: Integer;
var
  ItemsPerPage, FirstPageItems: Integer;
begin
  // Items sur la première page (en tenant compte de l'en-tête client)
  FirstPageItems := (Printer.PageHeight - 500) div 25;
  if FirstPageItems > Length(FPurchases) then
    FirstPageItems := Length(FPurchases);

  // Si tout tient sur la première page
  if FirstPageItems = Length(FPurchases) then
    Result := 1
  else
  begin
    // Items sur les pages suivantes
    ItemsPerPage := (Printer.PageHeight - 300) div 25;
    Result := 1 + Ceil((Length(FPurchases) - FirstPageItems) / ItemsPerPage);
  end;

  FTotalPages := Result;
end;

procedure TClientReport.Print;
var
  i, ItemsDrawn, StartItem: Integer;
begin
  GetPageCount; // Calculer le nombre total de pages

  Printer.BeginDoc;
  try
    // Première page avec les infos client
    DrawHeader(Printer.Canvas, 1.0);
    DrawClientInfo(Printer.Canvas, 1.0);
    StartItem := 0;
    DrawPurchaseTable(Printer.Canvas, 1.0, ItemsDrawn, StartItem);
    DrawFooter(Printer.Canvas, 1.0, 1);

    // Pages suivantes si nécessaire
    StartItem := ItemsDrawn;
    for i := 2 to FTotalPages do
    begin
      Printer.NewPage;
      DrawHeader(Printer.Canvas, 1.0);
      DrawPurchaseTable(Printer.Canvas, 1.0, ItemsDrawn, StartItem);
      DrawFooter(Printer.Canvas, 1.0, i);
      StartItem := StartItem + ItemsDrawn;
    end;
  finally
    Printer.EndDoc;
  end;
end;

procedure TClientReport.DrawPreview(Canvas: TCanvas; PageNum: Integer; ScaleFactor: Double);
var
  i, ItemsDrawn, StartItem: Integer;
begin
  if PageNum < 1 then Exit;
  GetPageCount; // Calculer le nombre total de pages

  if PageNum > FTotalPages then Exit;

  // Dessiner la page demandée
  DrawHeader(Canvas, ScaleFactor);

  if PageNum = 1 then
  begin
    // Première page avec les infos client
    DrawClientInfo(Canvas, ScaleFactor);
    StartItem := 0;
  end
  else
  begin
    // Pages suivantes : calculer l'élément de départ
    StartItem := 0;
    DrawPurchaseTable(Canvas, 0, ItemsDrawn, StartItem); // Appel temporaire pour calculer les éléments de la 1ère page

    // Calculer l'élément de départ pour les pages suivantes
    for i := 2 to PageNum do
    begin
      StartItem := StartItem + ItemsDrawn;
      if i < PageNum then
        DrawPurchaseTable(Canvas, 0, ItemsDrawn, StartItem); // Appel temporaire pour calculer les éléments
    end;
  end;

  // Dessiner le tableau des achats pour cette page
  DrawPurchaseTable(Canvas, ScaleFactor, ItemsDrawn, StartItem);

  // Dessiner le pied de page
  DrawFooter(Canvas, ScaleFactor, PageNum);
end;

end.
```

## Utilisation du rapport client avec aperçu

Voici comment utiliser cette classe de rapport dans votre application :

```pascal
procedure TMainForm.btnPreviewClientReportClick(Sender: TObject);
var
  ClientReport: TClientReport;
  Client: TClientInfo;
  Purchase: TPurchase;
  i: Integer;
begin
  // Créer le rapport
  ClientReport := TClientReport.Create;
  try
    // Définir les informations du client
    Client.Name := 'Jean Dupont';
    Client.Address := '123 Rue de la Programmation';
    Client.City := '75000 Paris';
    Client.Phone := '01 23 45 67 89';
    Client.Email := 'jean.dupont@example.com';
    Client.ClientSince := EncodeDate(2018, 3, 15);
    Client.Balance := 1250.75;

    ClientReport.SetClientData(Client);

    // Ajouter des achats de test
    for i := 1 to 20 do // Créer assez d'achats pour plusieurs pages
    begin
      Purchase.Date := EncodeDate(2023, Random(12) + 1, Random(28) + 1);
      Purchase.Product := Format('Produit test #%d', [i]);
      Purchase.Quantity := Random(10) + 1;
      Purchase.UnitPrice := (Random(1000) + 1) / 10;

      ClientReport.AddPurchase(Purchase);
    end;

    // Créer et afficher le formulaire d'aperçu
    if not Assigned(frmPreview) then
      Application.CreateForm(TfrmPreview, frmPreview);

    // Configurer le formulaire d'aperçu pour utiliser notre rapport
    frmPreview.SetReportObject(ClientReport);
    frmPreview.PreparePreview;
    frmPreview.ShowModal;
  finally
    ClientReport.Free;
  end;
end;
```

Et pour que cela fonctionne, il faut modifier le formulaire d'aperçu pour qu'il accepte un objet rapport :

```pascal
// Dans l'interface du formulaire d'aperçu
type
  TfrmPreview = class(TForm)
    // composants existants...
  private
    FReport: TClientReport;
    // autres variables privées...
  public
    procedure SetReportObject(Report: TClientReport);
    // autres méthodes publiques...
  end;

// Dans l'implémentation
procedure TfrmPreview.SetReportObject(Report: TClientReport);
begin
  FReport := Report;
end;

procedure TfrmPreview.PreparePreview;
begin
  if not Assigned(FReport) then Exit;

  // Calculer le nombre de pages
  FPageCount := FReport.GetPageCount;
  SetLength(FPages, FPageCount);

  // Créer l'aperçu de chaque page
  CreatePreviewPages;

  // Afficher la première page
  DisplayPage(1);
  UpdateStatus;
end;

procedure TfrmPreview.CreatePreviewPages;
var
  i: Integer;
  Bitmap: TBitmap;
  ScaleFactor: Double;
begin
  // Libérer les anciennes pages
  for i := 0 to Length(FPages) - 1 do
    if Assigned(FPages[i]) then
      FPages[i].Free;

  // Calculer l'échelle
  ScaleFactor := (ScrollBox1.Width - 20) / Printer.PageWidth;
  if ScaleFactor > 1 then ScaleFactor := 1; // Ne pas agrandir

  // Créer l'aperçu de chaque page
  for i := 0 to FPageCount - 1 do
  begin
    Bitmap := TBitmap.Create;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Width := Round(Printer.PageWidth * ScaleFactor);
    Bitmap.Height := Round(Printer.PageHeight * ScaleFactor);

    // Fond blanc
    Bitmap.Canvas.Brush.Color := clWhite;
    Bitmap.Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));

    // Dessiner le contenu
    FReport.DrawPreview(Bitmap.Canvas, i + 1, ScaleFactor);

    FPages[i] := Bitmap;
  end;

  FCurrentPage := 1;
end;

procedure TfrmPreview.btnPrintClick(Sender: TObject);
begin
  if Assigned(FReport) then
    FReport.Print;
end;
```

## Amélioration avancée : Impression directe vers PDF

Pour améliorer l'exportation PDF, vous pouvez utiliser une bibliothèque comme **Synopse PDF Engine**, qui est gratuite et open source. Voici un exemple d'implémentation :

```pascal
// Ajoutez Synopse PDF Engine à vos uses
uses
  SynPdf, SynGdiPlus;

procedure TClientReport.ExportToPdf(const FileName: string);
var
  Pdf: TPdfDocument;
  Page: TPdfPage;
  i, ItemsDrawn, StartItem: Integer;
  ScaleFactor: Double;
begin
  Pdf := TPdfDocument.Create;
  try
    Pdf.DefaultPageWidth := Printer.PageWidth * 72 / 254; // Convertir mm en points
    Pdf.DefaultPageHeight := Printer.PageHeight * 72 / 254;

    // Calculer le nombre de pages
    GetPageCount;

    // Première page avec les infos client
    Page := Pdf.AddPage;

    // Note: L'échelle est différente pour PDF
    ScaleFactor := 1.0;

    // Dessiner directement sur le PDF en utilisant GDI+
    with Pdf.NewContent do
    begin
      DrawHeader(Page.Canvas, ScaleFactor);
      DrawClientInfo(Page.Canvas, ScaleFactor);
      StartItem := 0;
      DrawPurchaseTable(Page.Canvas, ScaleFactor, ItemsDrawn, StartItem);
      DrawFooter(Page.Canvas, ScaleFactor, 1);
    end;

    // Pages suivantes si nécessaire
    StartItem := ItemsDrawn;
    for i := 2 to FTotalPages do
    begin
      Page := Pdf.AddPage;

      with Pdf.NewContent do
      begin
        DrawHeader(Page.Canvas, ScaleFactor);
        DrawPurchaseTable(Page.Canvas, ScaleFactor, ItemsDrawn, StartItem);
        DrawFooter(Page.Canvas, ScaleFactor, i);
      end;

      StartItem := StartItem + ItemsDrawn;
    end;

    // Enregistrer le PDF
    Pdf.SaveToFile(FileName);
  finally
    Pdf.Free;
  end;
end;
```

## Conclusion et bonnes pratiques

L'aperçu avant impression est une fonctionnalité essentielle pour les applications professionnelles. Bien que Delphi ne fournisse pas de composant natif pour cette fonctionnalité, nous avons vu comment en créer un efficacement.

### Résumé des bonnes pratiques :

1. **Séparation des responsabilités** : Séparez le code de génération du contenu (rapport) du code d'affichage (aperçu).

2. **Facteur d'échelle** : Utilisez toujours un facteur d'échelle pour adapter le contenu à la résolution d'affichage.

3. **Réutilisation du code** : Utilisez le même code pour l'aperçu et l'impression finale pour garantir la cohérence.

4. **Gestion de la mémoire** : Libérez correctement les bitmaps et autres ressources graphiques.

5. **Interface utilisateur intuitive** : Proposez des contrôles de navigation, zoom et impression clairs.

6. **Exportation PDF** : Ajoutez la possibilité d'exporter en PDF pour une meilleure expérience utilisateur.

7. **Performances** : Pour les documents volumineux, générez les pages à la demande plutôt que toutes à la fois.

8. **Compatibilité avec les imprimantes** : Testez avec différentes imprimantes et résolutions.

### Alternatives pour les projets complexes :

Si vos besoins en rapports sont complexes, envisagez d'utiliser des composants spécialisés comme :

- **FastReport** : Solution complète de rapports avec éditeur visuel
- **QuickReport** : Composant de rapports populaire pour Delphi
- **Report Builder** : Solution puissante avec support pour les sous-rapports

Ces solutions offrent des fonctionnalités avancées comme les tableaux croisés dynamiques, les graphiques, les codes-barres, et bien plus encore.

Dans la prochaine section, nous explorerons ces générateurs de rapports tiers et leurs fonctionnalités avancées.

⏭️ [Générateurs de rapports (FastReport, QuickReport)](/09-rapports-et-impressions/03-generateurs-de-rapports.md)


