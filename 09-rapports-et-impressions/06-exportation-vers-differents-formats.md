# 9.6 Exportation vers différents formats (PDF, Excel, HTML...)

## Introduction

L'une des fonctionnalités les plus appréciées des systèmes de rapports est la possibilité d'exporter les documents générés vers différents formats. Cette exportation permet de partager facilement les rapports avec d'autres personnes, de les archiver ou de les traiter ultérieurement dans d'autres applications.

Dans cette section, nous allons découvrir comment exporter vos rapports Delphi vers les formats les plus courants comme PDF, Excel, Word, HTML et d'autres formats utiles. Nous nous concentrerons principalement sur FastReport, car c'est la solution la plus complète, mais les concepts sont similaires pour QuickReport et d'autres générateurs de rapports.

## Exportation avec FastReport

FastReport propose de nombreux formats d'exportation grâce à des composants spécialisés. Pour chaque format d'exportation souhaité, vous devez ajouter le composant correspondant à votre formulaire.

### Configuration de base pour l'exportation

Voici les étapes générales pour exporter un rapport vers n'importe quel format :

1. Ajouter le composant d'exportation approprié au formulaire
2. Configurer les options d'exportation spécifiques au format
3. Préparer le rapport (données, mise en page, etc.)
4. Exécuter l'exportation

```pascal
procedure TForm1.ExportReport;
begin
  // 1. Préparer le rapport
  frxReport1.LoadFromFile('MonRapport.fr3');
  frxReport1.PrepareReport;  // Prépare le rapport avec les données actuelles

  // 2. Exporter (exemple avec PDF)
  frxPDFExport1.FileName := 'MonRapport.pdf';
  frxPDFExport1.ShowDialog := True;  // Affiche une boîte de dialogue pour confirmer
  frxReport1.Export(frxPDFExport1);
end;
```

### Exportation vers PDF

Le format PDF (Portable Document Format) est idéal pour préserver la mise en page exacte de vos rapports. Il est parfait pour l'archivage, l'impression et le partage de documents.

```pascal
procedure TForm1.ExportToPDF;
begin
  // Vérifier que le composant d'exportation est présent
  if not Assigned(frxPDFExport1) then
    frxPDFExport1 := TfrxPDFExport.Create(Self);

  // Configurer les options PDF
  frxPDFExport1.FileName := 'MonRapport.pdf';  // Nom du fichier de sortie
  frxPDFExport1.ShowDialog := True;            // Afficher le dialogue de sauvegarde
  frxPDFExport1.Subject := 'Mon rapport';      // Métadonnées PDF
  frxPDFExport1.Author := 'Mon Application';
  frxPDFExport1.Creator := 'Mon Application Delphi';

  // Options avancées
  frxPDFExport1.Background := False;           // Pas de fond de page
  frxPDFExport1.EmbeddedFonts := False;        // Ne pas incorporer les polices
  frxPDFExport1.PrintOptimized := True;        // Optimiser pour l'impression
  frxPDFExport1.Quality := 90;                 // Qualité des images (0-100)

  // Sécurité (optionnel)
  frxPDFExport1.ProtectionFlags := []; // Aucune restriction
  //frxPDFExport1.ProtectionFlags := [pfPrint, pfModify]; // Restreindre certaines actions
  //frxPDFExport1.UserPassword := 'motdepasse';  // Mot de passe utilisateur
  //frxPDFExport1.OwnerPassword := 'admin';      // Mot de passe propriétaire

  // Exporter
  frxReport1.Export(frxPDFExport1);
end;
```

Options importantes pour l'exportation PDF :
- `PrintOptimized` : optimise le PDF pour l'impression
- `EmbeddedFonts` : incorpore les polices dans le fichier
- `ProtectionFlags` : définit les restrictions sur le PDF
- `UserPassword` / `OwnerPassword` : protège le document par mot de passe

### Exportation vers Excel

L'exportation vers Excel est particulièrement utile lorsque les utilisateurs doivent manipuler ou analyser les données du rapport.

```pascal
procedure TForm1.ExportToExcel;
begin
  // Vérifier que le composant d'exportation est présent
  if not Assigned(frxXLSExport1) then
    frxXLSExport1 := TfrxXLSExport.Create(Self);

  // Configurer les options Excel
  frxXLSExport1.FileName := 'MonRapport.xls';  // Nom du fichier de sortie
  frxXLSExport1.ShowDialog := True;            // Afficher le dialogue de sauvegarde

  // Options spécifiques à Excel
  frxXLSExport1.ExportPageBreaks := True;      // Conserve les sauts de page
  frxXLSExport1.MinRowHeight := 0;             // Hauteur minimale des lignes
  frxXLSExport1.MaxRowHeight := 0;             // Hauteur maximale des lignes
  frxXLSExport1.WorksheetName := 'Données';    // Nom de la feuille de calcul
  frxXLSExport1.Wysiwyg := True;               // "What You See Is What You Get"

  // Exporter
  frxReport1.Export(frxXLSExport1);
end;
```

FastReport prend en charge deux formats Excel :
- `.xls` - Format Excel classique
- `.xlsx` - Format Excel moderne (Open XML), via le composant `TfrxXLSXExport`

La différence principale d'utilisation est le nom du composant et le format de fichier par défaut.

### Exportation vers Word (RTF/DOCX)

Exporter vers Word permet de modifier le rapport dans un traitement de texte.

```pascal
procedure TForm1.ExportToWord;
begin
  // Pour le format RTF
  if not Assigned(frxRTFExport1) then
    frxRTFExport1 := TfrxRTFExport.Create(Self);

  frxRTFExport1.FileName := 'MonRapport.rtf';
  frxRTFExport1.ShowDialog := True;

  // Quelques options RTF
  frxRTFExport1.ExportPageBreaks := True;    // Conserver les sauts de page
  frxRTFExport1.Wysiwyg := True;             // Préserver la mise en page

  // Exporter en RTF
  frxReport1.Export(frxRTFExport1);

  // Pour le format DOCX (Word moderne)
  if not Assigned(frxDOCXExport1) then
    frxDOCXExport1 := TfrxDOCXExport.Create(Self);

  frxDOCXExport1.FileName := 'MonRapport.docx';
  frxDOCXExport1.ShowDialog := True;

  // Exporter en DOCX
  frxReport1.Export(frxDOCXExport1);
end;
```

Le format RTF (Rich Text Format) est plus ancien mais largement compatible, tandis que DOCX est le format moderne de Microsoft Word.

### Exportation vers HTML

L'exportation HTML est parfaite pour publier des rapports sur le web ou les intégrer dans des applications web.

```pascal
procedure TForm1.ExportToHTML;
begin
  if not Assigned(frxHTMLExport1) then
    frxHTMLExport1 := TfrxHTMLExport.Create(Self);

  frxHTMLExport1.FileName := 'MonRapport.html';
  frxHTMLExport1.ShowDialog := True;

  // Options HTML
  frxHTMLExport1.FixedWidth := True;         // Utiliser une largeur fixe
  frxHTMLExport1.Background := True;         // Exporter la couleur de fond
  frxHTMLExport1.Pictures := True;           // Inclure les images
  frxHTMLExport1.EmbeddedPictures := False;  // Images externes (non intégrées)
  frxHTMLExport1.Navigator := False;         // Ne pas inclure les boutons de navigation
  frxHTMLExport1.MultiPage := True;          // Générer plusieurs pages HTML

  // Styles HTML
  frxHTMLExport1.UseCSS := True;             // Utiliser des feuilles de style CSS

  // Exporter
  frxReport1.Export(frxHTMLExport1);
end;
```

Options importantes pour l'exportation HTML :
- `MultiPage` : crée un fichier par page ou un seul fichier
- `Pictures` : inclut ou non les images
- `EmbeddedPictures` : intègre les images en base64 ou crée des fichiers séparés
- `UseCSS` : utilise les feuilles de style CSS pour la mise en page

### Autres formats d'exportation

FastReport prend en charge de nombreux autres formats utiles :

#### CSV (données séparées par des virgules)
```pascal
procedure TForm1.ExportToCSV;
begin
  if not Assigned(frxCSVExport1) then
    frxCSVExport1 := TfrxCSVExport.Create(Self);

  frxCSVExport1.FileName := 'MonRapport.csv';
  frxCSVExport1.ShowDialog := True;

  // Options CSV
  frxCSVExport1.Separator := ';';            // Séparateur (virgule, point-virgule, etc.)
  frxCSVExport1.OEMCodepage := False;        // Utiliser l'encodage OEM

  // Exporter
  frxReport1.Export(frxCSVExport1);
end;
```

#### Image (PNG, JPEG, BMP, TIFF)
```pascal
procedure TForm1.ExportToPNG;
begin
  if not Assigned(frxPNGExport1) then
    frxPNGExport1 := TfrxPNGExport.Create(Self);

  frxPNGExport1.FileName := 'MonRapport.png';
  frxPNGExport1.ShowDialog := True;

  // Options d'image
  frxPNGExport1.Resolution := 300;           // Résolution en DPI
  frxPNGExport1.JpegQuality := 90;           // Qualité JPEG (0-100)
  frxPNGExport1.MonochromeBmpCompression := True; // Compression pour BMP monochrome

  // Autres formats disponibles : JPEG, BMP, TIFF...

  // Exporter
  frxReport1.Export(frxPNGExport1);
end;
```

#### XML (données structurées)
```pascal
procedure TForm1.ExportToXML;
begin
  if not Assigned(frxXMLExport1) then
    frxXMLExport1 := TfrxXMLExport.Create(Self);

  frxXMLExport1.FileName := 'MonRapport.xml';
  frxXMLExport1.ShowDialog := True;

  // Exporter
  frxReport1.Export(frxXMLExport1);
end;
```

## Dialogue d'exportation simplifié

FastReport offre également une approche simplifiée avec un dialogue d'exportation tout-en-un :

```pascal
procedure TForm1.ShowExportDialog;
begin
  // Préparer le rapport
  frxReport1.PrepareReport;

  // Afficher le dialogue d'exportation
  frxReport1.ShowExportDialog;
end;
```

Ce dialogue présente à l'utilisateur tous les formats d'exportation disponibles dans une seule interface.

## Exportation par lots

Pour exporter un rapport vers plusieurs formats en une seule opération :

```pascal
procedure TForm1.BatchExport;
begin
  // Préparer le rapport
  frxReport1.LoadFromFile('MonRapport.fr3');
  frxReport1.PrepareReport;

  // Exporter vers PDF
  frxPDFExport1.FileName := 'MonRapport.pdf';
  frxPDFExport1.ShowDialog := False; // Pas de dialogue
  frxReport1.Export(frxPDFExport1);

  // Exporter vers Excel
  frxXLSExport1.FileName := 'MonRapport.xlsx';
  frxXLSExport1.ShowDialog := False;
  frxReport1.Export(frxXLSExport1);

  // Exporter vers HTML
  frxHTMLExport1.FileName := 'MonRapport.html';
  frxHTMLExport1.ShowDialog := False;
  frxReport1.Export(frxHTMLExport1);

  ShowMessage('Exportation terminée vers PDF, Excel et HTML');
end;
```

## Envoi par email

Vous pouvez facilement exporter un rapport et l'envoyer directement par email :

```pascal
procedure TForm1.EmailPDFReport;
begin
  // Préparer le rapport
  frxReport1.PrepareReport;

  // Exporter vers PDF dans un fichier temporaire
  frxPDFExport1.FileName := GetTempDir + 'RapportTemporaire.pdf';
  frxPDFExport1.ShowDialog := False;
  frxReport1.Export(frxPDFExport1);

  // Envoyer par email
  if FileExists(frxPDFExport1.FileName) then
  begin
    // Utiliser l'API de messagerie de Windows
    ShellExecute(0, 'open',
      PChar('mailto:destinataire@example.com?subject=Mon Rapport&body=Veuillez trouver ci-joint le rapport demandé.&attachment=' +
      frxPDFExport1.FileName),
      nil, nil, SW_SHOW);

    // Note: cette méthode ouvre le client email par défaut de l'utilisateur
    // Pour des solutions plus avancées, utilisez Indy ou autre composant email
  end;
end;
```

## Exportation à partir de QuickReport

Si vous utilisez QuickReport au lieu de FastReport, le principe est similaire mais avec des composants différents :

```pascal
procedure TForm1.ExportQuickReportToPDF;
var
  QRExportFilter: TQRPDFFilter;
begin
  QRExportFilter := TQRPDFFilter.Create(Self);
  try
    QuickRep1.ExportToFilter(QRExportFilter);
    QRExportFilter.Filename := 'MonRapport.pdf';
    QuickRep1.Prepare;
    QuickRep1.Print;
  finally
    QRExportFilter.Free;
  end;
end;
```

Pour d'autres formats avec QuickReport, utilisez les filtres correspondants :
- `TQRCSVFilter` pour CSV
- `TQRXLSFilter` pour Excel
- `TQRHTMLFilter` pour HTML
- `TQRRTFFilter` pour RTF

## Exportation programmatique sans dialogue

Pour automatiser l'exportation sans interaction de l'utilisateur (par exemple dans un traitement par lots) :

```pascal
procedure TForm1.BatchExportReports;
var
  i: Integer;
  ReportFiles: TStringList;
  OutputDir: string;
begin
  ReportFiles := TStringList.Create;
  try
    // Liste des rapports à traiter
    ReportFiles.Add('Rapport1.fr3');
    ReportFiles.Add('Rapport2.fr3');
    ReportFiles.Add('Rapport3.fr3');

    // Répertoire de sortie
    OutputDir := ExtractFilePath(Application.ExeName) + 'Exports\';
    if not DirectoryExists(OutputDir) then
      ForceDirectories(OutputDir);

    // Désactiver les dialogues
    frxPDFExport1.ShowDialog := False;

    // Traiter chaque rapport
    for i := 0 to ReportFiles.Count - 1 do
    begin
      // Charger et préparer le rapport
      frxReport1.Clear;
      frxReport1.LoadFromFile(ReportFiles[i]);
      frxReport1.PrepareReport;

      // Exporter en PDF
      frxPDFExport1.FileName := OutputDir + ChangeFileExt(ExtractFileName(ReportFiles[i]), '.pdf');
      frxReport1.Export(frxPDFExport1);
    end;

    ShowMessage('Exportation par lots terminée!');
  finally
    ReportFiles.Free;
  end;
end;
```

## Personnalisation des noms de fichiers exportés

Pour une meilleure organisation, vous pouvez générer des noms de fichiers dynamiques :

```pascal
procedure TForm1.ExportWithDynamicFilename;
var
  Filename: string;
begin
  // Générer un nom de fichier basé sur la date et l'heure
  Filename := 'Rapport_' + FormatDateTime('yyyymmdd_hhnnss', Now) + '.pdf';

  // Ou basé sur des données du rapport
  // Filename := 'Facture_' + ClientDataSet1.FieldByName('NumFacture').AsString + '.pdf';

  // Exporter
  frxPDFExport1.FileName := Filename;
  frxPDFExport1.ShowDialog := True;
  frxReport1.Export(frxPDFExport1);
end;
```

## Options d'exportation avancées

### Gestion des pages à exporter

Vous pouvez choisir quelles pages du rapport exporter :

```pascal
procedure TForm1.ExportSelectedPages;
begin
  // Exporter uniquement les pages 2 à 5
  frxPDFExport1.FileName := 'PagesSelectionnees.pdf';
  frxPDFExport1.ShowDialog := True;

  // Définir les pages à exporter
  frxPDFExport1.PageNumbers := '2-5';
  // Autres exemples: '1,3,5' ou '1-3,5,7-9'

  frxReport1.Export(frxPDFExport1);
end;
```

### Exportation de rapports avec graphiques

Lorsque vous exportez des rapports contenant des graphiques, certains formats préservent mieux les graphiques que d'autres :

- **PDF** : Préserve parfaitement les graphiques
- **HTML** : Convertit les graphiques en images
- **Excel** : Selon la complexité, peut perdre certains détails
- **Word** : Préserve généralement bien les graphiques

Pour une meilleure qualité des graphiques exportés :

```pascal
procedure TForm1.ExportWithCharts;
begin
  // Pour les graphiques, augmenter la résolution
  frxPDFExport1.Resolution := 300; // 300 DPI pour une bonne qualité

  // Si vous utilisez l'export HTML, assurez-vous que les images sont activées
  frxHTMLExport1.Pictures := True;

  // Exporter
  frxReport1.Export(frxPDFExport1);
end;
```

## Intégration dans une application complète

Pour intégrer les fonctionnalités d'exportation dans une application réelle, voici un exemple plus complet :

```pascal
procedure TForm1.btnExportClick(Sender: TObject);
var
  ExportPath: string;
begin
  // Vérifier que le rapport est prêt
  if not frxReport1.PreviewPages.Count > 0 then
  begin
    ShowMessage('Veuillez d''abord générer le rapport!');
    Exit;
  end;

  // Déterminer le chemin d'exportation
  ExportPath := ExtractFilePath(Application.ExeName) + 'Exports\';
  if not DirectoryExists(ExportPath) then
    ForceDirectories(ExportPath);

  // Configurer l'exportation selon le format choisi
  case RadioGroup1.ItemIndex of
    0: // PDF
      begin
        frxPDFExport1.FileName := ExportPath + 'Rapport.pdf';
        frxPDFExport1.Description := 'Fichier PDF';
        frxPDFExport1.ShowDialog := True;
        frxReport1.Export(frxPDFExport1);
      end;

    1: // Excel
      begin
        frxXLSExport1.FileName := ExportPath + 'Rapport.xlsx';
        frxXLSExport1.ShowDialog := True;
        frxReport1.Export(frxXLSExport1);
      end;

    2: // Word
      begin
        frxDOCXExport1.FileName := ExportPath + 'Rapport.docx';
        frxDOCXExport1.ShowDialog := True;
        frxReport1.Export(frxDOCXExport1);
      end;

    3: // HTML
      begin
        frxHTMLExport1.FileName := ExportPath + 'Rapport.html';
        frxHTMLExport1.ShowDialog := True;
        frxReport1.Export(frxHTMLExport1);
      end;

    4: // Image PNG
      begin
        frxPNGExport1.FileName := ExportPath + 'Rapport.png';
        frxPNGExport1.ShowDialog := True;
        frxReport1.Export(frxPNGExport1);
      end;

    5: // CSV
      begin
        frxCSVExport1.FileName := ExportPath + 'Rapport.csv';
        frxCSVExport1.ShowDialog := True;
        frxReport1.Export(frxCSVExport1);
      end;
  end;
end;
```

## Astuces et bonnes pratiques

### 1. Prévisualisation avant exportation

Permettez aux utilisateurs de prévisualiser le rapport avant de l'exporter :

```pascal
procedure TForm1.GenerateAndExport;
begin
  // Préparer le rapport
  frxReport1.LoadFromFile('MonRapport.fr3');
  frxReport1.PrepareReport;

  // Afficher la prévisualisation
  if frxReport1.ShowReport then
  begin
    // Après la fermeture de la prévisualisation, proposer l'exportation
    if MessageDlg('Voulez-vous exporter ce rapport?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      frxReport1.ShowExportDialog;
    end;
  end;
end;
```

### 2. Gestion des erreurs

```pascal
procedure TForm1.ExportWithErrorHandling;
begin
  try
    // Préparer le rapport
    frxReport1.PrepareReport;

    // Exporter
    frxPDFExport1.FileName := 'MonRapport.pdf';
    frxPDFExport1.ShowDialog := True;
    frxReport1.Export(frxPDFExport1);
  except
    on E: Exception do
    begin
      ShowMessage('Erreur lors de l''exportation: ' + E.Message);
      // Journaliser l'erreur, etc.
    end;
  end;
end;
```

### 3. Configuration des composants au design-time

Au lieu de créer les composants d'exportation par code, vous pouvez les ajouter à votre formulaire en design-time :

1. Cliquez avec le bouton droit dans l'éditeur de formulaire
2. Sélectionnez "Ajouter un composant..."
3. Recherchez et ajoutez les composants d'exportation (TfrxPDFExport, etc.)
4. Configurez leurs propriétés dans l'Object Inspector

### 4. Conservation des métadonnées

Pour les formats qui supportent les métadonnées (comme PDF) :

```pascal
procedure TForm1.ExportWithMetadata;
begin
  // Configurer les métadonnées
  frxPDFExport1.Author := 'Mon Application';
  frxPDFExport1.Creator := 'FastReport';
  frxPDFExport1.Subject := 'Rapport mensuel';
  frxPDFExport1.Title := 'Rapport de ventes - ' + FormatDateTime('mmmm yyyy', Date);
  frxPDFExport1.Keywords := 'rapport, ventes, mensuel';

  // Exporter
  frxReport1.Export(frxPDFExport1);
end;
```

### 5. Optimisation des performances

Pour les grands rapports, optimisez l'exportation :

```pascal
procedure TForm1.OptimizedExport;
begin
  // Activer le cache de fichier pour réduire l'utilisation de la mémoire
  frxReport1.ReportOptions.UseFileCache := True;
  frxReport1.ReportOptions.MaxMemSize := 10; // MB

  // Désactiver les éléments inutiles selon le format
  if ExportFormat = 'CSV' then
  begin
    // Pour CSV, pas besoin d'images
    frxReport1.ReportOptions.EnableImages := False;
  end;

  // Exporter
  frxReport1.Export(frxPDFExport1);
end;
```

## Conclusion

L'exportation de rapports vers différents formats est une fonctionnalité essentielle qui ajoute une grande valeur à vos applications Delphi. Les formats les plus utilisés sont :

- **PDF** : Pour l'archivage et l'impression fidèle
- **Excel** : Pour l'analyse ultérieure des données
- **Word** : Pour la modification et personnalisation
- **HTML** : Pour la publication web
- **CSV** : Pour l'importation dans d'autres systèmes
- **Images** : Pour l'inclusion dans d'autres documents

Avec les composants d'exportation de FastReport, vous pouvez facilement proposer ces options à vos utilisateurs, soit individuellement, soit via un dialogue unifié.

Dans la prochaine section, nous verrons comment créer des rapports interactifs qui offrent encore plus de flexibilité à vos utilisateurs.
