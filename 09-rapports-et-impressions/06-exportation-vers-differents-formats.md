🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.6 Exportation vers différents formats (PDF, Excel, HTML...)

## Introduction

L'exportation de données vers différents formats est une fonctionnalité cruciale dans les applications professionnelles. Elle permet aux utilisateurs de partager, analyser et archiver les informations dans le format le plus adapté à leurs besoins. Que ce soit un rapport PDF pour impression, un fichier Excel pour analyse, ou du HTML pour publication web, Delphi offre de nombreuses solutions pour exporter vos données.

## Pourquoi exporter vers différents formats ?

Les différents formats répondent à des besoins variés :

- **PDF** : archivage, impression, partage officiel, documents immuables
- **Excel** : analyse de données, calculs, tableaux croisés dynamiques
- **HTML** : publication web, emails, documentation en ligne
- **CSV** : interopérabilité, import dans d'autres systèmes
- **Word** : rapports éditables, documentation
- **XML/JSON** : échange de données, APIs, intégration
- **Images** : présentations, captures, documentation visuelle

## Vue d'ensemble des solutions

### Solutions natives Delphi

- **Manipulation de fichiers** : création manuelle de CSV, TXT
- **Composants VCL** : TWebBrowser pour HTML
- **RTL** : classes pour JSON, XML

### Bibliothèques tierces

- **FastReport** : export PDF, Excel, Word, HTML et plus
- **DevExpress** : suite complète d'export
- **Gnostice** : solutions PDF professionnelles
- **TMS Software** : composants d'export spécialisés
- **Synopse PDF** : bibliothèque PDF open source
- **SheetJS** : manipulation Excel avancée

### Approches par format

Chaque format peut être généré de plusieurs façons. Nous allons explorer les méthodes les plus courantes et pratiques.

## Export PDF

Le PDF est le format universel pour les documents finaux. Il préserve la mise en forme et est lisible sur toutes les plateformes.

### Méthode 1 : Avec FastReport

C'est la méthode la plus simple et professionnelle.

```pascal
uses
  frxClass, frxExportPDF;

procedure TForm1.ExporterEnPDF_FastReport(const NomFichier: string);  
var  
  PDFExport: TfrxPDFExport;
begin
  PDFExport := TfrxPDFExport.Create(nil);
  try
    // Configuration de base
    PDFExport.FileName := NomFichier;
    PDFExport.ShowDialog := False;
    PDFExport.ShowProgress := True;
    PDFExport.OverwritePrompt := True;

    // Qualité
    PDFExport.Quality := 95;
    PDFExport.Compressed := True;
    PDFExport.EmbeddedFonts := True;
    PDFExport.Background := True;

    // Métadonnées
    PDFExport.Title := 'Rapport des ventes';
    PDFExport.Author := 'MonEntreprise';
    PDFExport.Subject := 'Statistiques mensuelles';
    PDFExport.Keywords := 'ventes, rapport, statistiques';
    PDFExport.Creator := 'Application Gestion v1.0';

    // Préparer et exporter le rapport
    frxReport1.LoadFromFile('MonRapport.fr3');
    frxReport1.PrepareReport;
    frxReport1.Export(PDFExport);

    ShowMessage('PDF créé avec succès : ' + NomFichier);
  finally
    PDFExport.Free;
  end;
end;
```

### Méthode 2 : Avec Synopse PDF

Bibliothèque PDF open source très performante.

```pascal
uses
  SynPdf, SynGdiPlus;

procedure TForm1.ExporterEnPDF_Synopse(const NomFichier: string);  
var  
  PDF: TPdfDocumentGDI;
  Page: TPdfPage;
  Y: Integer;
begin
  PDF := TPdfDocumentGDI.Create;
  try
    // Métadonnées
    PDF.Info.Title := 'Rapport des ventes';
    PDF.Info.Author := 'MonEntreprise';
    PDF.Info.Subject := 'Statistiques';
    PDF.Info.CreationDate := Now;

    // Ajouter une page
    PDF.AddPage;
    Page := PDF.CurrentPage;

    // Configuration de la police
    PDF.Canvas.SetFont('Arial', 12);
    Y := 50;

    // Ajouter du contenu
    PDF.Canvas.TextOut(50, Y, 'RAPPORT DES VENTES');
    Inc(Y, 30);

    PDF.Canvas.SetFont('Arial', 10);
    PDF.Canvas.TextOut(50, Y, 'Date : ' + DateToStr(Date));
    Inc(Y, 20);

    // Parcourir les données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      PDF.Canvas.TextOut(50, Y,
        Format('%s : %.2f €',
          [FDQueryVentes.FieldByName('produit').AsString,
           FDQueryVentes.FieldByName('montant').AsFloat]));
      Inc(Y, 20);

      // Nouvelle page si nécessaire
      if Y > 750 then
      begin
        PDF.AddPage;
        Y := 50;
      end;

      FDQueryVentes.Next;
    end;

    // Sauvegarder
    PDF.SaveToFile(NomFichier);
    ShowMessage('PDF créé avec succès');
  finally
    PDF.Free;
  end;
end;
```

### Export PDF avec sécurité

Protégez vos PDF avec des mots de passe.

```pascal
procedure TForm1.ExporterPDFProtege(const NomFichier: string);  
var  
  PDFExport: TfrxPDFExport;
begin
  PDFExport := TfrxPDFExport.Create(nil);
  try
    PDFExport.FileName := NomFichier;

    // Mots de passe
    PDFExport.UserPassword := 'motdepasse_lecture';
    PDFExport.OwnerPassword := 'motdepasse_admin';

    // Permissions
    PDFExport.ProtectionFlags := [ePrint, eCopy]; // Autoriser impression et copie
    // ou
    // PDFExport.ProtectionFlags := []; // Tout interdire

    // Options disponibles :
    // ePrint - autoriser l'impression
    // eModify - autoriser la modification
    // eCopy - autoriser la copie de texte
    // eAnnot - autoriser les annotations

    frxReport1.PrepareReport;
    frxReport1.Export(PDFExport);
  finally
    PDFExport.Free;
  end;
end;
```

### PDF avec signets et hyperliens

Créez des PDF interactifs avec navigation.

```pascal
procedure TForm1.CreerPDFInteractif;  
var  
  PDFExport: TfrxPDFExport;
begin
  PDFExport := TfrxPDFExport.Create(nil);
  try
    PDFExport.FileName := 'RapportInteractif.pdf';

    // Activer les signets
    PDFExport.Outline := True;

    // Activer les hyperliens
    PDFExport.HideToolbar := False;
    PDFExport.HideMenubar := False;
    PDFExport.PrintScaling := psNone;

    // Dans le rapport FastReport, configurez les signets :
    // - Propriété Bookmark sur les objets pour créer des signets
    // - Propriété Hyperlink pour ajouter des liens

    frxReport1.PrepareReport;
    frxReport1.Export(PDFExport);
  finally
    PDFExport.Free;
  end;
end;
```

## Export Excel

Excel est le format de prédilection pour l'analyse de données et les calculs.

### Méthode 1 : Avec FastReport

```pascal
uses
  frxClass, frxExportXLSX;

procedure TForm1.ExporterEnExcel_FastReport(const NomFichier: string);  
var  
  ExcelExport: TfrxXLSXExport;
begin
  ExcelExport := TfrxXLSXExport.Create(nil);
  try
    ExcelExport.FileName := NomFichier;
    ExcelExport.ShowDialog := False;

    // Options d'export
    ExcelExport.Wysiwyg := True; // Reproduire la mise en forme
    ExcelExport.PageBreaks := True; // Respecter les sauts de page
    ExcelExport.ChunkSize := 50; // Taille des chunks (performance)
    ExcelExport.ExportPictures := True; // Exporter les images
    ExcelExport.ExportFormulas := False; // Formules ou valeurs
    ExcelExport.OpenAfterExport := False; // Ouvrir automatiquement

    // Préparer et exporter
    frxReport1.PrepareReport;
    frxReport1.Export(ExcelExport);

    ShowMessage('Excel créé avec succès');
  finally
    ExcelExport.Free;
  end;
end;
```

### Méthode 2 : Avec OLE Automation

Contrôle direct d'Excel via COM.

```pascal
uses
  ComObj, Variants;

procedure TForm1.ExporterEnExcel_OLE(const NomFichier: string);  
var  
  ExcelApp, Workbook, Worksheet: Variant;
  Ligne: Integer;
begin
  try
    // Créer l'application Excel
    ExcelApp := CreateOleObject('Excel.Application');
    ExcelApp.Visible := False; // Masquer Excel pendant le traitement

    // Créer un nouveau classeur
    Workbook := ExcelApp.Workbooks.Add;
    Worksheet := Workbook.Worksheets[1];

    // Titre
    Worksheet.Cells[1, 1] := 'RAPPORT DES VENTES';
    Worksheet.Range['A1:D1'].Merge;
    Worksheet.Range['A1'].Font.Size := 16;
    Worksheet.Range['A1'].Font.Bold := True;
    Worksheet.Range['A1'].HorizontalAlignment := -4108; // xlCenter

    // En-têtes de colonnes
    Ligne := 3;
    Worksheet.Cells[Ligne, 1] := 'Date';
    Worksheet.Cells[Ligne, 2] := 'Produit';
    Worksheet.Cells[Ligne, 3] := 'Quantité';
    Worksheet.Cells[Ligne, 4] := 'Montant';

    // Mise en forme des en-têtes
    Worksheet.Range['A3:D3'].Font.Bold := True;
    Worksheet.Range['A3:D3'].Interior.Color := RGB(200, 200, 200);

    // Remplir les données
    FDQueryVentes.First;
    Inc(Ligne);

    while not FDQueryVentes.Eof do
    begin
      Worksheet.Cells[Ligne, 1] := FDQueryVentes.FieldByName('date_vente').AsString;
      Worksheet.Cells[Ligne, 2] := FDQueryVentes.FieldByName('produit').AsString;
      Worksheet.Cells[Ligne, 3] := FDQueryVentes.FieldByName('quantite').AsInteger;
      Worksheet.Cells[Ligne, 4] := FDQueryVentes.FieldByName('montant').AsFloat;

      Inc(Ligne);
      FDQueryVentes.Next;
    end;

    // Formule de total
    Inc(Ligne);
    Worksheet.Cells[Ligne, 3] := 'TOTAL :';
    Worksheet.Cells[Ligne, 3].Font.Bold := True;
    Worksheet.Cells[Ligne, 4] := Format('=SUM(D4:D%d)', [Ligne - 1]);
    Worksheet.Cells[Ligne, 4].Font.Bold := True;

    // Formatage des nombres
    Worksheet.Range[Format('D4:D%d', [Ligne])].NumberFormat := '#,##0.00 €';

    // Ajuster la largeur des colonnes
    Worksheet.Columns.AutoFit;

    // Enregistrer
    Workbook.SaveAs(NomFichier);
    Workbook.Close;

    ShowMessage('Fichier Excel créé avec succès');
  finally
    ExcelApp.Quit;
    ExcelApp := Unassigned;
  end;
end;
```

### Méthode 3 : Export direct vers XLSX avec bibliothèque

Utilisation de la bibliothèque XLSX (SheetJS via FireDAC ou composants tiers).

```pascal
uses
  FireDAC.Comp.BatchMove,
  FireDAC.Comp.BatchMove.Dataset,
  FireDAC.Comp.BatchMove.XLSX;

procedure TForm1.ExporterEnXLSX_FireDAC(const NomFichier: string);  
var  
  BatchMove: TFDBatchMove;
  Reader: TFDBatchMoveDataSetReader;
  Writer: TFDBatchMoveXLSXWriter;
begin
  BatchMove := TFDBatchMove.Create(nil);
  Reader := TFDBatchMoveDataSetReader.Create(BatchMove);
  Writer := TFDBatchMoveXLSXWriter.Create(BatchMove);

  try
    // Configuration du lecteur
    Reader.DataSet := FDQueryVentes;

    // Configuration de l'écriture
    Writer.FileName := NomFichier;
    Writer.SheetName := 'Ventes';

    // Options
    Writer.DataDefs.Add.DestName := 'Date';
    Writer.DataDefs.Add.DestName := 'Produit';
    Writer.DataDefs.Add.DestName := 'Quantité';
    Writer.DataDefs.Add.DestName := 'Montant';

    // Associer lecteur et écrivain
    BatchMove.Reader := Reader;
    BatchMove.Writer := Writer;

    // Exécuter
    BatchMove.Execute;

    ShowMessage('Export Excel terminé');
  finally
    Writer.Free;
    Reader.Free;
    BatchMove.Free;
  end;
end;
```

### Excel avec plusieurs feuilles

```pascal
procedure TForm1.ExporterExcelMultiFeuilles(const NomFichier: string);  
var  
  ExcelApp, Workbook: Variant;
begin
  ExcelApp := CreateOleObject('Excel.Application');
  try
    Workbook := ExcelApp.Workbooks.Add;

    // Feuille 1 : Ventes
    RemplirFeuilleVentes(Workbook.Worksheets[1]);
    Workbook.Worksheets[1].Name := 'Ventes';

    // Feuille 2 : Statistiques
    Workbook.Worksheets.Add;
    RemplirFeuilleStatistiques(Workbook.Worksheets[2]);
    Workbook.Worksheets[2].Name := 'Statistiques';

    // Feuille 3 : Graphique
    Workbook.Worksheets.Add;
    CreerGraphiqueExcel(Workbook.Worksheets[3]);
    Workbook.Worksheets[3].Name := 'Graphique';

    Workbook.SaveAs(NomFichier);
    Workbook.Close;
  finally
    ExcelApp.Quit;
  end;
end;

procedure TForm1.RemplirFeuilleVentes(Worksheet: Variant);  
var  
  Ligne: Integer;
begin
  // En-têtes
  Worksheet.Cells[1, 1] := 'Date';
  Worksheet.Cells[1, 2] := 'Montant';

  // Données
  FDQueryVentes.First;
  Ligne := 2;
  while not FDQueryVentes.Eof do
  begin
    Worksheet.Cells[Ligne, 1] := FDQueryVentes.FieldByName('date_vente').AsString;
    Worksheet.Cells[Ligne, 2] := FDQueryVentes.FieldByName('montant').AsFloat;
    Inc(Ligne);
    FDQueryVentes.Next;
  end;
end;
```

### Excel avec mise en forme conditionnelle

```pascal
procedure TForm1.AjouterMiseEnFormeConditionnelle(Worksheet: Variant; Plage: string);  
var  
  FormatCondition: Variant;
begin
  // Ajouter une règle de mise en forme conditionnelle
  FormatCondition := Worksheet.Range[Plage].FormatConditions.Add(
    1, // xlCellValue
    3, // xlGreater
    '=1000'
  );

  // Configurer le format
  FormatCondition.Interior.Color := RGB(255, 200, 200); // Fond rouge clair
  FormatCondition.Font.Bold := True;
  FormatCondition.Font.Color := RGB(200, 0, 0); // Texte rouge
end;
```

## Export CSV

Le CSV est le format le plus simple pour l'échange de données.

### Export CSV basique

```pascal
procedure TForm1.ExporterEnCSV(const NomFichier: string);  
var  
  Fichier: TextFile;
  i: Integer;
  Ligne: string;
begin
  AssignFile(Fichier, NomFichier);
  try
    Rewrite(Fichier);

    // En-têtes (noms des colonnes)
    Ligne := '';
    for i := 0 to FDQueryVentes.FieldCount - 1 do
    begin
      if i > 0 then
        Ligne := Ligne + ';';
      Ligne := Ligne + FDQueryVentes.Fields[i].FieldName;
    end;
    WriteLn(Fichier, Ligne);

    // Données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      Ligne := '';
      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        if i > 0 then
          Ligne := Ligne + ';';

        // Gérer les guillemets et points-virgules dans les données
        var Valeur := FDQueryVentes.Fields[i].AsString;
        if (Pos(';', Valeur) > 0) or (Pos('"', Valeur) > 0) then
        begin
          Valeur := StringReplace(Valeur, '"', '""', [rfReplaceAll]);
          Valeur := '"' + Valeur + '"';
        end;

        Ligne := Ligne + Valeur;
      end;
      WriteLn(Fichier, Ligne);
      FDQueryVentes.Next;
    end;

    ShowMessage('Export CSV terminé');
  finally
    CloseFile(Fichier);
  end;
end;
```

### Export CSV avec encodage UTF-8

```pascal
uses
  System.IOUtils;

procedure TForm1.ExporterEnCSV_UTF8(const NomFichier: string);  
var  
  Lignes: TStringList;
  i: Integer;
  Ligne: string;
begin
  Lignes := TStringList.Create;
  try
    // En-têtes
    Ligne := '';
    for i := 0 to FDQueryVentes.FieldCount - 1 do
    begin
      if i > 0 then Ligne := Ligne + ';';
      Ligne := Ligne + FDQueryVentes.Fields[i].FieldName;
    end;
    Lignes.Add(Ligne);

    // Données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      Ligne := '';
      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        if i > 0 then Ligne := Ligne + ';';
        Ligne := Ligne + FDQueryVentes.Fields[i].AsString;
      end;
      Lignes.Add(Ligne);
      FDQueryVentes.Next;
    end;

    // Sauvegarder en UTF-8 avec BOM
    Lignes.SaveToFile(NomFichier, TEncoding.UTF8);

    ShowMessage('Export CSV UTF-8 terminé');
  finally
    Lignes.Free;
  end;
end;
```

### Export CSV avec configuration

```pascal
type
  TCSVExportOptions = record
    Separateur: Char;
    DelimiteurTexte: Char;
    IncluireEnTetes: Boolean;
    Encodage: TEncoding;
  end;

procedure TForm1.ExporterCSVAvecOptions(const NomFichier: string; Options: TCSVExportOptions);  
var  
  Lignes: TStringList;
  i: Integer;
  Ligne: string;

  function FormatValeur(const Valeur: string): string;
  begin
    Result := Valeur;
    // Si la valeur contient le séparateur ou le délimiteur
    if (Pos(Options.Separateur, Result) > 0) or
       (Pos(Options.DelimiteurTexte, Result) > 0) then
    begin
      // Doubler les délimiteurs de texte
      Result := StringReplace(Result, Options.DelimiteurTexte,
        Options.DelimiteurTexte + Options.DelimiteurTexte, [rfReplaceAll]);
      // Entourer de délimiteurs
      Result := Options.DelimiteurTexte + Result + Options.DelimiteurTexte;
    end;
  end;

begin
  Lignes := TStringList.Create;
  try
    // En-têtes
    if Options.IncluireEnTetes then
    begin
      Ligne := '';
      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        if i > 0 then Ligne := Ligne + Options.Separateur;
        Ligne := Ligne + FormatValeur(FDQueryVentes.Fields[i].FieldName);
      end;
      Lignes.Add(Ligne);
    end;

    // Données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      Ligne := '';
      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        if i > 0 then Ligne := Ligne + Options.Separateur;
        Ligne := Ligne + FormatValeur(FDQueryVentes.Fields[i].AsString);
      end;
      Lignes.Add(Ligne);
      FDQueryVentes.Next;
    end;

    Lignes.SaveToFile(NomFichier, Options.Encodage);
  finally
    Lignes.Free;
  end;
end;

// Utilisation
procedure TForm1.btnExporterCSVClick(Sender: TObject);  
var  
  Options: TCSVExportOptions;
begin
  Options.Separateur := ';';
  Options.DelimiteurTexte := '"';
  Options.IncluireEnTetes := True;
  Options.Encodage := TEncoding.UTF8;

  ExporterCSVAvecOptions('export.csv', Options);
end;
```

## Export HTML

HTML est parfait pour la publication web et les emails.

### Export HTML simple

```pascal
procedure TForm1.ExporterEnHTML(const NomFichier: string);  
var  
  HTML: TStringList;
  i: Integer;
begin
  HTML := TStringList.Create;
  try
    // Structure HTML
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('  <meta charset="UTF-8">');
    HTML.Add('  <title>Rapport des Ventes</title>');
    HTML.Add('  <style>');
    HTML.Add('    body { font-family: Arial, sans-serif; margin: 20px; }');
    HTML.Add('    h1 { color: #333; }');
    HTML.Add('    table { border-collapse: collapse; width: 100%; }');
    HTML.Add('    th { background-color: #4CAF50; color: white; padding: 10px; text-align: left; }');
    HTML.Add('    td { border: 1px solid #ddd; padding: 8px; }');
    HTML.Add('    tr:nth-child(even) { background-color: #f2f2f2; }');
    HTML.Add('    tr:hover { background-color: #ddd; }');
    HTML.Add('  </style>');
    HTML.Add('</head>');
    HTML.Add('<body>');

    // Titre
    HTML.Add('  <h1>Rapport des Ventes</h1>');
    HTML.Add('  <p>Généré le : ' + DateTimeToStr(Now) + '</p>');

    // Tableau
    HTML.Add('  <table>');

    // En-têtes
    HTML.Add('    <tr>');
    for i := 0 to FDQueryVentes.FieldCount - 1 do
      HTML.Add('      <th>' + FDQueryVentes.Fields[i].FieldName + '</th>');
    HTML.Add('    </tr>');

    // Données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      HTML.Add('    <tr>');
      for i := 0 to FDQueryVentes.FieldCount - 1 do
        HTML.Add('      <td>' + FDQueryVentes.Fields[i].AsString + '</td>');
      HTML.Add('    </tr>');
      FDQueryVentes.Next;
    end;

    HTML.Add('  </table>');
    HTML.Add('</body>');
    HTML.Add('</html>');

    // Sauvegarder
    HTML.SaveToFile(NomFichier, TEncoding.UTF8);

    ShowMessage('Export HTML terminé');
  finally
    HTML.Free;
  end;
end;
```

### Export HTML avec graphique

```pascal
procedure TForm1.ExporterHTMLAvecGraphique(const NomFichier: string);  
var  
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('  <meta charset="UTF-8">');
    HTML.Add('  <title>Rapport avec Graphique</title>');
    HTML.Add('  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>');
    HTML.Add('  <style>');
    HTML.Add('    body { font-family: Arial; margin: 20px; }');
    HTML.Add('    .chart-container { width: 80%; margin: 20px auto; }');
    HTML.Add('  </style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('  <h1>Rapport des Ventes</h1>');

    // Conteneur du graphique
    HTML.Add('  <div class="chart-container">');
    HTML.Add('    <canvas id="myChart"></canvas>');
    HTML.Add('  </div>');

    // Script pour le graphique
    HTML.Add('  <script>');
    HTML.Add('    const ctx = document.getElementById("myChart");');
    HTML.Add('    new Chart(ctx, {');
    HTML.Add('      type: "bar",');
    HTML.Add('      data: {');
    HTML.Add('        labels: ["Jan", "Fév", "Mar", "Avr", "Mai"],');
    HTML.Add('        datasets: [{');
    HTML.Add('          label: "Ventes",');
    HTML.Add('          data: [120, 150, 135, 180, 165],');
    HTML.Add('          backgroundColor: "rgba(54, 162, 235, 0.5)",');
    HTML.Add('          borderColor: "rgba(54, 162, 235, 1)",');
    HTML.Add('          borderWidth: 1');
    HTML.Add('        }]');
    HTML.Add('      }');
    HTML.Add('    });');
    HTML.Add('  </script>');

    HTML.Add('</body>');
    HTML.Add('</html>');

    HTML.SaveToFile(NomFichier, TEncoding.UTF8);
  finally
    HTML.Free;
  end;
end;
```

### Export HTML responsive

```pascal
procedure TForm1.ExporterHTMLResponsive(const NomFichier: string);  
var  
  HTML: TStringList;
begin
  HTML := TStringList.Create;
  try
    HTML.Add('<!DOCTYPE html>');
    HTML.Add('<html>');
    HTML.Add('<head>');
    HTML.Add('  <meta charset="UTF-8">');
    HTML.Add('  <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    HTML.Add('  <title>Rapport Responsive</title>');
    HTML.Add('  <style>');
    HTML.Add('    * { box-sizing: border-box; }');
    HTML.Add('    body { font-family: Arial; margin: 0; padding: 20px; }');
    HTML.Add('    .container { max-width: 1200px; margin: 0 auto; }');
    HTML.Add('    table { width: 100%; border-collapse: collapse; }');
    HTML.Add('    th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }');
    HTML.Add('    @media screen and (max-width: 600px) {');
    HTML.Add('      table { font-size: 12px; }');
    HTML.Add('      th, td { padding: 6px; }');
    HTML.Add('    }');
    HTML.Add('  </style>');
    HTML.Add('</head>');
    HTML.Add('<body>');
    HTML.Add('  <div class="container">');

    // Contenu...

    HTML.Add('  </div>');
    HTML.Add('</body>');
    HTML.Add('</html>');

    HTML.SaveToFile(NomFichier, TEncoding.UTF8);
  finally
    HTML.Free;
  end;
end;
```

## Export XML

XML est un format structuré idéal pour l'échange de données.

### Export XML simple

```pascal
uses
  Xml.XMLDoc, Xml.XMLIntf;

procedure TForm1.ExporterEnXML(const NomFichier: string);  
var  
  XMLDoc: IXMLDocument;
  RootNode, RecordNode, FieldNode: IXMLNode;
  i: Integer;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;
    XMLDoc.Version := '1.0';
    XMLDoc.Encoding := 'UTF-8';

    // Nœud racine
    RootNode := XMLDoc.AddChild('Ventes');
    RootNode.Attributes['date_export'] := DateTimeToStr(Now);

    // Parcourir les données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      RecordNode := RootNode.AddChild('Vente');

      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        FieldNode := RecordNode.AddChild(FDQueryVentes.Fields[i].FieldName);
        FieldNode.Text := FDQueryVentes.Fields[i].AsString;
      end;

      FDQueryVentes.Next;
    end;

    // Sauvegarder
    XMLDoc.SaveToFile(NomFichier);

    ShowMessage('Export XML terminé');
  finally
    XMLDoc := nil;
  end;
end;
```

### Export XML avec attributs

```pascal
procedure TForm1.ExporterXMLAvecAttributs(const NomFichier: string);  
var  
  XMLDoc: IXMLDocument;
  RootNode, RecordNode: IXMLNode;
begin
  XMLDoc := TXMLDocument.Create(nil);
  try
    XMLDoc.Active := True;

    RootNode := XMLDoc.AddChild('Ventes');

    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      RecordNode := RootNode.AddChild('Vente');

      // Utiliser des attributs au lieu de nœuds enfants
      RecordNode.Attributes['id'] := FDQueryVentes.FieldByName('id').AsString;
      RecordNode.Attributes['date'] := FDQueryVentes.FieldByName('date_vente').AsString;
      RecordNode.Attributes['montant'] := FDQueryVentes.FieldByName('montant').AsString;
      RecordNode.Attributes['produit'] := FDQueryVentes.FieldByName('produit').AsString;

      FDQueryVentes.Next;
    end;

    XMLDoc.SaveToFile(NomFichier);
  finally
    XMLDoc := nil;
  end;
end;
```

## Export JSON

JSON est le format moderne pour les APIs et applications web.

### Export JSON simple

```pascal
uses
  System.JSON;

procedure TForm1.ExporterEnJSON(const NomFichier: string);  
var  
  JSONArray: TJSONArray;
  JSONObject: TJSONObject;
  i: Integer;
  JSONString: string;
begin
  JSONArray := TJSONArray.Create;
  try
    // Parcourir les données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      JSONObject := TJSONObject.Create;

      for i := 0 to FDQueryVentes.FieldCount - 1 do
      begin
        case FDQueryVentes.Fields[i].DataType of
          ftInteger, ftSmallint, ftWord:
            JSONObject.AddPair(FDQueryVentes.Fields[i].FieldName,
              TJSONNumber.Create(FDQueryVentes.Fields[i].AsInteger));

          ftFloat, ftCurrency, ftBCD:
            JSONObject.AddPair(FDQueryVentes.Fields[i].FieldName,
              TJSONNumber.Create(FDQueryVentes.Fields[i].AsFloat));

          ftBoolean:
            JSONObject.AddPair(FDQueryVentes.Fields[i].FieldName,
              TJSONBool.Create(FDQueryVentes.Fields[i].AsBoolean));

          ftDate, ftDateTime, ftTime:
            JSONObject.AddPair(FDQueryVentes.Fields[i].FieldName,
              DateTimeToStr(FDQueryVentes.Fields[i].AsDateTime));
        else
          JSONObject.AddPair(FDQueryVentes.Fields[i].FieldName,
            FDQueryVentes.Fields[i].AsString);
        end;
      end;

      JSONArray.AddElement(JSONObject);
      FDQueryVentes.Next;
    end;

    // Sauvegarder avec indentation
    JSONString := JSONArray.Format(2); // 2 espaces d'indentation
    TFile.WriteAllText(NomFichier, JSONString, TEncoding.UTF8);

    ShowMessage('Export JSON terminé');
  finally
    JSONArray.Free;
  end;
end;
```

### Export JSON structuré

```pascal
procedure TForm1.ExporterJSONStructure(const NomFichier: string);  
var  
  RootObject: TJSONObject;
  MetaData: TJSONObject;
  DataArray: TJSONArray;
  RecordObject: TJSONObject;
begin
  RootObject := TJSONObject.Create;
  try
    // Métadonnées
    MetaData := TJSONObject.Create;
    MetaData.AddPair('date_export', DateTimeToStr(Now));
    MetaData.AddPair('version', '1.0');
    MetaData.AddPair('nombre_enregistrements', TJSONNumber.Create(FDQueryVentes.RecordCount));
    RootObject.AddPair('metadata', MetaData);

    // Données
    DataArray := TJSONArray.Create;
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      RecordObject := TJSONObject.Create;
      RecordObject.AddPair('id', TJSONNumber.Create(FDQueryVentes.FieldByName('id').AsInteger));
      RecordObject.AddPair('produit', FDQueryVentes.FieldByName('produit').AsString);
      RecordObject.AddPair('montant', TJSONNumber.Create(FDQueryVentes.FieldByName('montant').AsFloat));

      DataArray.AddElement(RecordObject);
      FDQueryVentes.Next;
    end;
    RootObject.AddPair('data', DataArray);

    // Sauvegarder
    TFile.WriteAllText(NomFichier, RootObject.Format(2), TEncoding.UTF8);
  finally
    RootObject.Free;
  end;
end;
```

## Export Word

Pour créer des documents Word éditables.

### Export vers Word via OLE

```pascal
uses
  ComObj;

procedure TForm1.ExporterEnWord(const NomFichier: string);  
var  
  WordApp, Document, Range: Variant;
begin
  WordApp := CreateOleObject('Word.Application');
  try
    WordApp.Visible := False;

    // Nouveau document
    Document := WordApp.Documents.Add;

    // Titre
    Range := Document.Range;
    Range.Text := 'RAPPORT DES VENTES'#13#10;
    Range.Font.Size := 18;
    Range.Font.Bold := True;
    Range.ParagraphFormat.Alignment := 1; // wdAlignParagraphCenter

    // Date
    Range := Document.Range;
    Range.Start := Range.End;
    Range.Text := 'Date : ' + DateToStr(Date) + #13#10#13#10;

    // Tableau
    var Table := Document.Tables.Add(
      Document.Range(Range.End, Range.End),
      FDQueryVentes.RecordCount + 1,  // lignes (+ en-tête)
      FDQueryVentes.FieldCount        // colonnes
    );

    // En-têtes
    for var i := 0 to FDQueryVentes.FieldCount - 1 do
    begin
      Table.Cell(1, i + 1).Range.Text := FDQueryVentes.Fields[i].FieldName;
      Table.Cell(1, i + 1).Range.Font.Bold := True;
      Table.Cell(1, i + 1).Shading.BackgroundPatternColor := RGB(200, 200, 200);
    end;

    // Données
    FDQueryVentes.First;
    var Ligne := 2;
    while not FDQueryVentes.Eof do
    begin
      for var i := 0 to FDQueryVentes.FieldCount - 1 do
        Table.Cell(Ligne, i + 1).Range.Text := FDQueryVentes.Fields[i].AsString;

      Inc(Ligne);
      FDQueryVentes.Next;
    end;

    // Mise en forme du tableau
    Table.AutoFitBehavior(2); // wdAutoFitContent
    Table.Borders.Enable := True;

    // Sauvegarder
    Document.SaveAs2(NomFichier);
    Document.Close;

    ShowMessage('Document Word créé');
  finally
    WordApp.Quit;
  end;
end;
```

## Export texte formaté (RTF)

RTF est un format texte enrichi compatible avec Word et autres traitements de texte.

```pascal
procedure TForm1.ExporterEnRTF(const NomFichier: string);  
var  
  RTF: TStringList;
begin
  RTF := TStringList.Create;
  try
    // En-tête RTF
    RTF.Add('{\rtf1\ansi\deff0');
    RTF.Add('{\fonttbl{\f0 Arial;}}');
    RTF.Add('{\colortbl;\red0\green0\blue0;\red255\green0\blue0;}');

    // Titre
    RTF.Add('\f0\fs32\b RAPPORT DES VENTES\b0\fs20\par');
    RTF.Add('\par');

    // Date
    RTF.Add('Date : ' + DateToStr(Date) + '\par');
    RTF.Add('\par');

    // Données
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      RTF.Add(Format('%s : %s\par',
        [FDQueryVentes.FieldByName('produit').AsString,
         FDQueryVentes.FieldByName('montant').AsString]));
      FDQueryVentes.Next;
    end;

    RTF.Add('}');

    RTF.SaveToFile(NomFichier);
  finally
    RTF.Free;
  end;
end;
```

## Export d'images

Exportez vos graphiques et visualisations en images.

### Export graphique TeeChart

```pascal
uses
  VCLTee.TeeProcs, VCLTee.TeePNG, VCLTee.TeeJPEG;

procedure TForm1.ExporterGraphiqueEnImage(const NomFichier: string);  
var  
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(NomFichier));

  if Extension = '.png' then
  begin
    var PNG := TTeePNGExport.Create;
    try
      PNG.Panel := Chart1;
      PNG.Width := Chart1.Width;
      PNG.Height := Chart1.Height;
      PNG.SaveToFile(NomFichier);
    finally
      PNG.Free;
    end;
  end
  else if (Extension = '.jpg') or (Extension = '.jpeg') then
  begin
    var JPEG := TTeeJPEGExport.Create;
    try
      JPEG.Panel := Chart1;
      JPEG.Width := Chart1.Width;
      JPEG.Height := Chart1.Height;
      JPEG.SaveToFile(NomFichier);
    finally
      JPEG.Free;
    end;
  end
  else if Extension = '.bmp' then
    Chart1.SaveToBitmapFile(NomFichier);
end;
```

### Capture d'écran d'un formulaire

```pascal
procedure TForm1.CapturerFormulaire(const NomFichier: string);  
var  
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := Width;
    Bitmap.Height := Height;

    // Capturer le formulaire
    var DC := GetDC(Handle);
    try
      BitBlt(Bitmap.Canvas.Handle, 0, 0, Width, Height, DC, 0, 0, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;

    // Sauvegarder
    Bitmap.SaveToFile(NomFichier);
  finally
    Bitmap.Free;
  end;
end;
```

## Interface utilisateur pour l'export

### Dialogue de sélection de format

```pascal
procedure TForm1.btnExporterClick(Sender: TObject);  
var  
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Title := 'Exporter les données';
    SaveDialog.Filter :=
      'Fichiers PDF (*.pdf)|*.pdf|' +
      'Fichiers Excel (*.xlsx)|*.xlsx|' +
      'Fichiers CSV (*.csv)|*.csv|' +
      'Fichiers HTML (*.html)|*.html|' +
      'Fichiers XML (*.xml)|*.xml|' +
      'Fichiers JSON (*.json)|*.json|' +
      'Tous les fichiers (*.*)|*.*';
    SaveDialog.DefaultExt := 'pdf';

    if SaveDialog.Execute then
    begin
      var Extension := LowerCase(ExtractFileExt(SaveDialog.FileName));

      Screen.Cursor := crHourGlass;
      try
        if Extension = '.pdf' then
          ExporterEnPDF_FastReport(SaveDialog.FileName)
        else if Extension = '.xlsx' then
          ExporterEnExcel_FastReport(SaveDialog.FileName)
        else if Extension = '.csv' then
          ExporterEnCSV(SaveDialog.FileName)
        else if Extension = '.html' then
          ExporterEnHTML(SaveDialog.FileName)
        else if Extension = '.xml' then
          ExporterEnXML(SaveDialog.FileName)
        else if Extension = '.json' then
          ExporterEnJSON(SaveDialog.FileName)
        else
          ShowMessage('Format non supporté');
      finally
        Screen.Cursor := crDefault;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;
```

### Formulaire d'options d'export

```pascal
type
  TFormOptionsExport = class(TForm)
    RadioGroupFormat: TRadioGroup;
    CheckBoxOuvrirApres: TCheckBox;
    CheckBoxInclureEnTetes: TCheckBox;
    ComboBoxEncodage: TComboBox;
    btnExporter: TButton;
    btnAnnuler: TButton;
  end;

procedure TFormOptionsExport.btnExporterClick(Sender: TObject);  
begin  
  var Format := '';
  case RadioGroupFormat.ItemIndex of
    0: Format := 'PDF';
    1: Format := 'Excel';
    2: Format := 'CSV';
    3: Format := 'HTML';
  end;

  // Appeler la fonction d'export appropriée avec les options
  ModalResult := mrOk;
end;
```

### Barre de progression pour export

```pascal
procedure TForm1.ExporterAvecProgression(const NomFichier: string);  
var  
  TotalLignes, LigneActuelle: Integer;
begin
  TotalLignes := FDQueryVentes.RecordCount;
  LigneActuelle := 0;

  ProgressBar1.Max := TotalLignes;
  ProgressBar1.Position := 0;
  ProgressBar1.Visible := True;

  try
    FDQueryVentes.First;
    while not FDQueryVentes.Eof do
    begin
      // Traiter la ligne...

      Inc(LigneActuelle);
      ProgressBar1.Position := LigneActuelle;
      Application.ProcessMessages; // Rafraîchir l'interface

      FDQueryVentes.Next;
    end;

    ShowMessage('Export terminé');
  finally
    ProgressBar1.Visible := False;
  end;
end;
```

## Gestion des erreurs et validation

### Gestion robuste des erreurs

```pascal
procedure TForm1.ExporterAvecGestionErreurs(const NomFichier: string);  
begin  
  try
    // Vérifier que le dataset contient des données
    if FDQueryVentes.IsEmpty then
    begin
      ShowMessage('Aucune donnée à exporter');
      Exit;
    end;

    // Vérifier que le chemin est accessible
    var Repertoire := ExtractFilePath(NomFichier);
    if not DirectoryExists(Repertoire) then
      ForceDirectories(Repertoire);

    // Vérifier que le fichier n'est pas en lecture seule
    if FileExists(NomFichier) and FileIsReadOnly(NomFichier) then
    begin
      ShowMessage('Le fichier est en lecture seule');
      Exit;
    end;

    // Effectuer l'export
    ExporterEnPDF_FastReport(NomFichier);

  except
    on E: EFOpenError do
      ShowMessage('Impossible d''ouvrir le fichier : ' + E.Message);
    on E: EInOutError do
      ShowMessage('Erreur d''entrée/sortie : ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur lors de l''export : ' + E.Message);
  end;
end;
```

### Validation avant export

```pascal
function TForm1.ValiderAvantExport: Boolean;  
begin  
  Result := True;

  // Vérifier la connexion
  if not FDConnection1.Connected then
  begin
    ShowMessage('Base de données non connectée');
    Exit(False);
  end;

  // Vérifier les données
  if FDQueryVentes.IsEmpty then
  begin
    if MessageDlg('Aucune donnée à exporter. Continuer quand même ?',
       mtWarning, [mbYes, mbNo], 0) = mrNo then
      Exit(False);
  end;

  // Vérifier l'espace disque (exemple simplifié)
  var DisponibleMo := DiskFree(0) div (1024 * 1024);
  if DisponibleMo < 10 then
  begin
    ShowMessage('Espace disque insuffisant');
    Exit(False);
  end;
end;
```

## Optimisation et performance

### Export asynchrone

```pascal
uses
  System.Threading;

procedure TForm1.ExporterAsync(const NomFichier: string);  
begin  
  btnExporter.Enabled := False;
  ProgressBar1.Visible := True;

  TTask.Run(
    procedure
    begin
      try
        // Export dans le thread
        ExporterEnPDF_FastReport(NomFichier);

        // Retour au thread principal
        TThread.Synchronize(nil,
          procedure
          begin
            ShowMessage('Export terminé');
            btnExporter.Enabled := True;
            ProgressBar1.Visible := False;
          end
        );
      except
        on E: Exception do
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Erreur : ' + E.Message);
              btnExporter.Enabled := True;
              ProgressBar1.Visible := False;
            end
          );
      end;
    end
  );
end;
```

### Export par lots (batch)

```pascal
procedure TForm1.ExporterParLots(const CheminBase: string);  
const  
  TAILLE_LOT = 1000;
var
  NumeroLot: Integer;
begin
  NumeroLot := 0;
  FDQueryVentes.First;

  while not FDQueryVentes.Eof do
  begin
    Inc(NumeroLot);
    var NomFichier := Format('%s_lot_%d.csv', [CheminBase, NumeroLot]);

    // Exporter un lot
    var LignesExportees := 0;
    // ... code d'export ...

    // Avancer jusqu'au prochain lot
    while (LignesExportees < TAILLE_LOT) and not FDQueryVentes.Eof do
    begin
      FDQueryVentes.Next;
      Inc(LignesExportees);
    end;
  end;
end;
```

## Conseils et bonnes pratiques

### Choix du format

- **PDF** : documents finaux, archivage, partage officiel
- **Excel** : analyse de données, calculs, rapports interactifs
- **CSV** : simplicité, universalité, import dans d'autres systèmes
- **HTML** : publication web, emails, documentation
- **XML/JSON** : échange de données structurées, APIs
- **Word** : documents éditables, modèles personnalisables

### Qualité des exports

- **Encodage** : utilisez UTF-8 pour la compatibilité internationale
- **Métadonnées** : incluez date, version, source des données
- **Validation** : vérifiez les données avant export
- **Formatage** : respectez les conventions du format cible
- **Taille** : compressez ou divisez les gros fichiers

### Performance

- **Asynchrone** : ne bloquez pas l'interface utilisateur
- **Progression** : informez l'utilisateur de l'avancement
- **Mémoire** : libérez les ressources après usage
- **Lots** : divisez les gros volumes en plusieurs fichiers
- **Cache** : réutilisez les connexions et ressources

### Sécurité

- **Validation** : vérifiez les chemins et noms de fichiers
- **Permissions** : contrôlez l'accès aux exports
- **Chiffrement** : protégez les exports sensibles (PDF avec mot de passe)
- **Logs** : journalisez les exports pour audit
- **Nettoyage** : supprimez les fichiers temporaires

### Expérience utilisateur

- **Dialogue clair** : interface simple et intuitive
- **Options** : offrez des choix pertinents sans surcharger
- **Feedback** : informez sur la réussite ou l'échec
- **Raccourcis** : mémorisez les préférences utilisateur
- **Aide** : expliquez les formats et leurs usages

## Résumé

L'exportation de données vers différents formats est une fonctionnalité essentielle des applications professionnelles. Les points clés :

- **Multiples formats** : PDF, Excel, CSV, HTML, XML, JSON, Word et images
- **FastReport** : solution complète pour les exports professionnels
- **OLE Automation** : contrôle direct d'Excel et Word
- **Formats texte** : CSV, XML, JSON pour l'interopérabilité
- **Gestion d'erreurs** : validation et récupération robustes
- **Performance** : export asynchrone pour les gros volumes
- **Interface utilisateur** : dialogue intuitif avec progression
- **Bonnes pratiques** : encodage UTF-8, métadonnées, validation

Maîtriser l'exportation vers différents formats permet de créer des applications flexibles qui s'intègrent parfaitement dans l'écosystème informatique de l'entreprise.

⏭️ [Rapports interactifs](/09-rapports-et-impressions/07-rapports-interactifs.md)
