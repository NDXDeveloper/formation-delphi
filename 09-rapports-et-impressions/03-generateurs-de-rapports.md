# 9.3 Générateurs de rapports (FastReport, QuickReport)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Bien que Delphi offre des composants natifs pour l'impression, la création de rapports complexes nécessite souvent des outils plus spécialisés. Les générateurs de rapports sont des composants tiers qui simplifient considérablement la création, la conception et l'impression de rapports professionnels.

Dans cette section, nous allons explorer deux des générateurs de rapports les plus populaires pour Delphi : FastReport et QuickReport.

## Pourquoi utiliser un générateur de rapports ?

Avant d'entrer dans les détails, voici pourquoi vous devriez considérer l'utilisation d'un générateur de rapports professionnel :

- **Conception visuelle** : Interface de design WYSIWYG (What You See Is What You Get)
- **Flexibilité** : Adapté aux rapports simples comme aux documents complexes multi-pages
- **Prévisualisation intégrée** : Aperçu avant impression prêt à l'emploi
- **Export multi-formats** : PDF, Excel, Word, HTML, XML, etc.
- **Accès aux données** : Connexion facile aux bases de données et ensembles de données
- **Bandes (bands)** : Organisation du rapport en sections logiques (en-tête, détails, pied de page...)
- **Calculs automatiques** : Totaux, moyennes, compteurs et autres agrégations
- **Graphiques et diagrammes** : Visualisation des données intégrée

## FastReport

FastReport est l'un des générateurs de rapports les plus complets pour Delphi. Il offre un environnement de conception visuel puissant et un large éventail de fonctionnalités.

### Installation de FastReport

FastReport n'est pas inclus dans Delphi et doit être acheté séparément. Une version d'évaluation est généralement disponible sur le site officiel [fast-report.com](https://www.fast-report.com/).

Une fois téléchargé et installé, vous devez l'ajouter à votre environnement Delphi :

1. Dans Delphi, sélectionnez **Composants > Installer des packages...**
2. Cliquez sur **Ajouter...**
3. Naviguez jusqu'au dossier d'installation de FastReport et sélectionnez le fichier package approprié (par exemple, `FR*.bpl`)
4. Suivez les instructions à l'écran pour terminer l'installation

Après l'installation, une nouvelle palette de composants FastReport devrait apparaître dans l'environnement Delphi.

### Composants principaux de FastReport

Voici les composants principaux que vous utiliserez avec FastReport :

- **TfrxReport** : Composant principal qui gère le rapport
- **TfrxDesigner** : Designer visuel pour créer et modifier les rapports
- **TfrxDBDataset** : Connexion aux sources de données Delphi (TDataSet)
- **TfrxPreview** : Composant d'aperçu avant impression

### Création d'un rapport simple avec FastReport

Voici comment créer un rapport basique :

1. **Préparation du projet** :

```pascal
// Assurez-vous d'inclure les unités nécessaires
uses
  frxClass, frxDBSet, frxPreview;
```

2. **Ajout des composants** :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Créer les composants FastReport
  frxReport1 := TfrxReport.Create(Self);
  frxDBDataset1 := TfrxDBDataset.Create(Self);

  // Configurer la source de données (par exemple, si vous avez un ClientDataSet)
  frxDBDataset1.DataSet := ClientDataSet1;
  frxDBDataset1.UserName := 'Clients'; // Nom utilisé dans le designer
end;
```

3. **Création et exécution du rapport** :

```pascal
procedure TMainForm.btnCreateReportClick(Sender: TObject);
var
  Page: TfrxReportPage;
  Band: TfrxBand;
  Memo: TfrxMemoView;
begin
  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Configurer la page
  Page.Orientation := poPortrait;
  Page.PaperWidth := 210;  // A4
  Page.PaperHeight := 297;
  Page.LeftMargin := 10;
  Page.RightMargin := 10;
  Page.TopMargin := 10;
  Page.BottomMargin := 10;

  // Créer une bande d'en-tête de rapport (Report Title)
  Band := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 20;

  // Ajouter un titre
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 20;
  Memo.Text := 'Liste des Clients';
  Memo.HAlign := haCenter;
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];

  // Créer une bande d'en-tête de page (Page Header)
  Band := TfrxPageHeader.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 30;
  Band.Height := 20;

  // Ajouter les en-têtes de colonnes
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'Nom';
  Memo.Font.Style := [fsBold];

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 110;
  Memo.Top := 0;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := 'Adresse';
  Memo.Font.Style := [fsBold];

  // Créer une bande de détails (Master Data)
  Band := TfrxMasterData.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 60;
  Band.Height := 20;
  Band.DataSet := frxDBDataset1; // Lier à notre dataset

  // Ajouter les champs de données
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := '[Clients."NomClient"]'; // Nom du champ dans la table

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 110;
  Memo.Top := 0;
  Memo.Width := 150;
  Memo.Height := 20;
  Memo.Text := '[Clients."Adresse"]'; // Nom du champ dans la table

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Utilisation du designer visuel de FastReport

La création manuelle par code comme ci-dessus est rarement utilisée en pratique. FastReport brille par son designer visuel :

```pascal
procedure TMainForm.btnDesignReportClick(Sender: TObject);
begin
  // Créer un rapport vide si nécessaire
  if frxReport1.IsEmpty then
    frxReport1.LoadFromFile('RapportVide.fr3');

  // Ouvrir le designer
  frxReport1.DesignReport;
end;
```

Le designer visuel offre une interface intuitive avec :
- Une barre d'outils pour ajouter des objets (texte, images, lignes, etc.)
- Un explorateur de données montrant les sources disponibles
- Un inspecteur de propriétés pour configurer les objets
- Une règle et une grille pour un alignement précis
- Des options de zoom pour une mise en page détaillée

### Organisation en bandes (bands)

FastReport organise les rapports en bandes horizontales, chacune ayant un rôle spécifique :

| Bande | Description |
|-------|-------------|
| Report Title | Apparaît une fois au début du rapport |
| Report Summary | Apparaît une fois à la fin du rapport |
| Page Header | Apparaît en haut de chaque page |
| Page Footer | Apparaît en bas de chaque page |
| Master Data | Se répète pour chaque enregistrement dans le dataset principal |
| Detail | Se répète pour les enregistrements détaillés (relations maître-détail) |
| Group Header | Apparaît au début de chaque groupe |
| Group Footer | Apparaît à la fin de chaque groupe |

### Expressions et calculs

FastReport permet d'utiliser des expressions pour les calculs dynamiques :

```
// Exemples d'expressions
[Clients."Prix"] * [Clients."Quantite"]  // Multiplication
[Page#]  // Numéro de page
[Date]   // Date actuelle
[Sum(<expression>)]  // Somme d'une expression
```

### Exportation vers différents formats

FastReport excelle dans l'exportation vers différents formats :

```pascal
procedure TMainForm.btnExportToPdfClick(Sender: TObject);
begin
  // Préparer le rapport
  frxReport1.PrepareReport;

  // Exporter en PDF
  frxReport1.Export(frxPDFExport1);
end;
```

Vous devez ajouter les composants d'exportation correspondants (TfrxPDFExport, TfrxXLSExport, etc.) à votre formulaire.

## QuickReport

QuickReport est un autre générateur de rapports populaire pour Delphi. Il est plus léger que FastReport mais offre néanmoins des fonctionnalités essentielles pour la création de rapports professionnels.

### Installation de QuickReport

QuickReport était historiquement inclus dans certaines éditions de Delphi, mais les versions récentes nécessitent généralement un achat séparé. L'installation est similaire à celle de FastReport :

1. Achetez et téléchargez QuickReport depuis [quickreport.co.uk](https://www.quickreport.co.uk/)
2. Installez-le en suivant les instructions fournies
3. Dans Delphi, utilisez le menu **Composants > Installer des packages...** pour intégrer QuickReport

### Composants principaux de QuickReport

Les composants essentiels de QuickReport sont :

- **TQuickRep** : Composant principal du rapport
- **TQRBand** : Bande horizontale (en-tête, détail, pied de page...)
- **TQRLabel** : Étiquette de texte statique
- **TQRDBText** : Affiche le contenu d'un champ de base de données
- **TQRExpr** : Affiche le résultat d'une expression

### Création d'un rapport simple avec QuickReport

Voici un exemple de création d'un rapport basique avec QuickReport :

```pascal
procedure TMainForm.CreateQuickReport;
var
  Report: TQuickRep;
  TitleBand: TQRBand;
  DetailBand: TQRBand;
  HeaderBand: TQRBand;
  FooterBand: TQRBand;
  Label: TQRLabel;
  DBText: TQRDBText;
begin
  // Créer le rapport
  Report := TQuickRep.Create(Self);
  Report.Parent := Self;
  Report.Visible := False;

  // Configurer le rapport
  Report.DataSet := ClientDataSet1; // Votre source de données
  Report.Font.Name := 'Arial';
  Report.Font.Size := 10;

  // Créer la bande de titre
  TitleBand := TQRBand.Create(Report);
  TitleBand.Parent := Report;
  TitleBand.BandType := rbTitle;
  TitleBand.Height := 50;

  // Ajouter un titre
  Label := TQRLabel.Create(TitleBand);
  Label.Parent := TitleBand;
  Label.Caption := 'Liste des Clients';
  Label.Font.Size := 18;
  Label.Font.Style := [fsBold];
  Label.Alignment := taCenter;
  Label.Width := Report.Width;
  Label.Top := 10;

  // Créer une bande d'en-tête
  HeaderBand := TQRBand.Create(Report);
  HeaderBand.Parent := Report;
  HeaderBand.BandType := rbColumnHeader;
  HeaderBand.Height := 30;

  // Ajouter des en-têtes de colonnes
  Label := TQRLabel.Create(HeaderBand);
  Label.Parent := HeaderBand;
  Label.Caption := 'Nom';
  Label.Font.Style := [fsBold];
  Label.Left := 10;
  Label.Top := 10;

  Label := TQRLabel.Create(HeaderBand);
  Label.Parent := HeaderBand;
  Label.Caption := 'Adresse';
  Label.Font.Style := [fsBold];
  Label.Left := 150;
  Label.Top := 10;

  // Créer une bande de détail
  DetailBand := TQRBand.Create(Report);
  DetailBand.Parent := Report;
  DetailBand.BandType := rbDetail;
  DetailBand.Height := 20;

  // Ajouter des champs de données
  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'NomClient';
  DBText.Left := 10;
  DBText.Top := 0;

  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'Adresse';
  DBText.Left := 150;
  DBText.Top := 0;

  // Créer une bande de pied de page
  FooterBand := TQRBand.Create(Report);
  FooterBand.Parent := Report;
  FooterBand.BandType := rbPageFooter;
  FooterBand.Height := 30;

  // Ajouter un numéro de page
  Label := TQRLabel.Create(FooterBand);
  Label.Parent := FooterBand;
  Label.Caption := 'Page ';
  Label.Left := Report.Width - 100;
  Label.Top := 10;

  // Numéro de page automatique
  Label := TQRExpr.Create(FooterBand);
  Label.Parent := FooterBand;
  Label.Expression := 'PAGENUMBER';
  Label.Left := Report.Width - 50;
  Label.Top := 10;

  // Prévisualiser le rapport
  Report.Preview;
end;
```

### Designer visuel de QuickReport

Comme FastReport, QuickReport propose aussi un designer visuel qui facilite la création de rapports :

```pascal
procedure TMainForm.btnDesignQuickReportClick(Sender: TObject);
begin
  // Vérifier si le rapport existe déjà
  if not FileExists('ClientReport.qrp') then
  begin
    // Créer un rapport basique
    CreateQuickReport;
    QuickRep1.SaveToFile('ClientReport.qrp');
  end;

  // Ouvrir le designer
  QRDesign1.Report := QuickRep1;
  QRDesign1.Execute;
end;
```

## Comparaison : FastReport vs QuickReport

Voici une comparaison des deux générateurs de rapports pour vous aider à choisir :

| Fonctionnalité | FastReport | QuickReport |
|----------------|------------|-------------|
| Complexité | Plus complexe, mais très puissant | Plus simple, plus facile à apprendre |
| Interface | Designer moderne et riche | Designer plus basique |
| Formats d'export | Plus nombreux (PDF, Excel, Word, HTML, etc.) | Limité aux formats standard |
| Graphiques | Outils de graphiques avancés | Support basique des graphiques |
| Sous-rapports | Support complet | Support limité |
| Prix | Généralement plus élevé | Généralement moins cher |
| Documentation | Très complète | Correcte |

## Exemple pratique : Rapport de facture avec FastReport

Voici un exemple plus complet pour créer un rapport de facture avec FastReport :

```pascal
procedure TMainForm.CreateInvoiceReport;
begin
  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter les sources de données
  frxReport1.DataSets.Clear;
  frxReport1.DataSets.Add(frxDBDataset1); // Clients
  frxReport1.DataSets.Add(frxDBDataset2); // Factures
  frxReport1.DataSets.Add(frxDBDataset3); // Détails des factures

  // Charger le modèle de rapport depuis un fichier
  if FileExists('Modeles/Facture.fr3') then
    frxReport1.LoadFromFile('Modeles/Facture.fr3')
  else
  begin
    // Créer un rapport de facture par défaut
    CreateDefaultInvoiceTemplate;
    // Sauvegarder pour utilisation future
    frxReport1.SaveToFile('Modeles/Facture.fr3');
  end;

  // Définir des variables globales
  frxReport1.Variables['NumeroFacture'] := QuotedStr('FA-' + FormatDateTime('yyyymmdd', Date) + '-001');
  frxReport1.Variables['DateFacture'] := QuotedStr(FormatDateTime('dd/mm/yyyy', Date));
  frxReport1.Variables['TauxTVA'] := '20'; // 20%

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;

procedure TMainForm.CreateDefaultInvoiceTemplate;
var
  Page: TfrxReportPage;
  Band: TfrxBand;
  Memo: TfrxMemoView;
  Picture: TfrxPictureView;
  Line: TfrxLineView;
begin
  // Créer une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);
  Page.Orientation := poPortrait;
  Page.PaperWidth := 210;  // A4
  Page.PaperHeight := 297;
  Page.LeftMargin := 10;
  Page.RightMargin := 10;
  Page.TopMargin := 10;
  Page.BottomMargin := 10;

  // En-tête du rapport
  Band := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 90;

  // Logo de l'entreprise
  Picture := TfrxPictureView.Create(frxReport1);
  Band.Objects.Add(Picture);
  Picture.Left := 0;
  Picture.Top := 0;
  Picture.Width := 100;
  Picture.Height := 50;
  Picture.HightQuality := True;
  if FileExists('logo.png') then
    Picture.LoadFromFile('logo.png');

  // Titre "FACTURE"
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 100;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 30;
  Memo.Text := 'FACTURE';
  Memo.Font.Size := 16;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  // Numéro de facture
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 100;
  Memo.Top := 30;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'N° [<NumeroFacture>]';
  Memo.HAlign := haRight;

  // Date de facture
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 100;
  Memo.Top := 50;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'Date: [<DateFacture>]';
  Memo.HAlign := haRight;

  // Coordonnées de l'entreprise
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 50;
  Memo.Width := 200;
  Memo.Height := 40;
  Memo.Text := 'VOTRE ENTREPRISE' + #13#10 +
               '123 Rue du Commerce' + #13#10 +
               '75000 Paris, France' + #13#10 +
               'Tél: 01 23 45 67 89';

  // Informations client (Master Data)
  Band := TfrxMasterData.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 100;
  Band.Height := 60;
  Band.DataSet := frxDBDataset1; // Dataset client

  // Titre "FACTURÉ À"
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'FACTURÉ À:';
  Memo.Font.Style := [fsBold];

  // Coordonnées du client
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 20;
  Memo.Width := 200;
  Memo.Height := 40;
  Memo.Text := '[Clients."NomClient"]' + #13#10 +
               '[Clients."Adresse"]' + #13#10 +
               '[Clients."CodePostal"] [Clients."Ville"]' + #13#10 +
               'Tél: [Clients."Telephone"]';

  // En-tête des lignes de facture
  Band := TfrxHeader.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 170;
  Band.Height := 25;

  // Ligne horizontale avant l'en-tête
  Line := TfrxLineView.Create(frxReport1);
  Band.Objects.Add(Line);
  Line.Left := 0;
  Line.Top := 0;
  Line.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Line.Height := 0;
  Line.Frame.Typ := [ftTop];

  // En-têtes des colonnes
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 5;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'QUANTITÉ';
  Memo.Font.Style := [fsBold];

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 70;
  Memo.Top := 5;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := 'DESCRIPTION';
  Memo.Font.Style := [fsBold];

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 280;
  Memo.Top := 5;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'PRIX UNIT.';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 60;
  Memo.Top := 5;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'TOTAL HT';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  // Ligne horizontale après l'en-tête
  Line := TfrxLineView.Create(frxReport1);
  Band.Objects.Add(Line);
  Line.Left := 0;
  Line.Top := 24;
  Line.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Line.Height := 0;
  Line.Frame.Typ := [ftTop];

  // Détails des lignes de facture
  Band := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 200;
  Band.Height := 20;
  Band.DataSet := frxDBDataset3; // Dataset des lignes de facture

  // Colonnes de détail
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := '[DetailsFacture."Quantite"]';

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 70;
  Memo.Top := 0;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := '[DetailsFacture."Description"]';

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 280;
  Memo.Top := 0;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := '[DetailsFacture."PrixUnitaire"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 60;
  Memo.Top := 0;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := '[DetailsFacture."Quantite"] * [DetailsFacture."PrixUnitaire"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // Pied de facture (totaux)
  Band := TfrxReportSummary.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 240;
  Band.Height := 100;

  // Ligne horizontale avant le total
  Line := TfrxLineView.Create(frxReport1);
  Band.Objects.Add(Line);
  Line.Left := 0;
  Line.Top := 0;
  Line.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Line.Height := 0;
  Line.Frame.Typ := [ftTop];

  // Total HT
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 160;
  Memo.Top := 10;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'Total HT:';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 60;
  Memo.Top := 10;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'SUM([DetailsFacture."Quantite"] * [DetailsFacture."PrixUnitaire"])';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // TVA
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 160;
  Memo.Top := 30;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'TVA ([<TauxTVA>]%):';
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 60;
  Memo.Top := 30;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'SUM([DetailsFacture."Quantite"] * [DetailsFacture."PrixUnitaire"]) * [<TauxTVA>] / 100';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // Total TTC
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 160;
  Memo.Top := 50;
  Memo.Width := 100;
  Memo.Height := 20;
  Memo.Text := 'Total TTC:';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := Page.Width - Page.LeftMargin - Page.RightMargin - 60;
  Memo.Top := 50;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'SUM([DetailsFacture."Quantite"] * [DetailsFacture."PrixUnitaire"]) * (1 + [<TauxTVA>] / 100)';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  // Conditions de paiement
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 70;
  Memo.Width := 350;
  Memo.Height := 30;
  Memo.Text := 'CONDITIONS DE PAIEMENT: Paiement à 30 jours à compter de la date de facture.' + #13#10 +
               'Merci pour votre confiance!';

  // Pied de page
  Band := TfrxPageFooter.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Top := 350;
  Band.Height := 20;

  // Ligne horizontale avant le pied de page
  Line := TfrxLineView.Create(frxReport1);
  Band.Objects.Add(Line);
  Line.Left := 0;
  Line.Top := 0;
  Line.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Line.Height := 0;
  Line.Frame.Typ := [ftTop];

  // Numéro de page
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 5;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 15;
  Memo.Text := 'Page [Page#] sur [TotalPages#]';
  Memo.HAlign := haCenter;
end;
```

### Conseils pour utiliser efficacement FastReport

Pour tirer le meilleur parti de FastReport, voici quelques conseils pratiques :

1. **Utiliser les modèles** : Créez des modèles réutilisables pour vos rapports fréquents.

```pascal
// Charger un modèle
frxReport1.LoadFromFile('Modeles/Facture.fr3');

// Sauvegarder un modèle
frxReport1.SaveToFile('Modeles/Facture.fr3');
```

2. **Exploiter les variables** : Utilisez des variables pour personnaliser vos rapports sans modifier le design.

```pascal
// Définir des variables
frxReport1.Variables['NomClient'] := QuotedStr('Dupont SA');
frxReport1.Variables['DateFacture'] := QuotedStr(FormatDateTime('dd/mm/yyyy', Date));

// Utiliser dans le rapport: [<NomClient>], [<DateFacture>]
```

3. **Créer des groupes** : Organisez vos données en groupes pour des rapports plus lisibles.

```
// Dans le designer, ajoutez une bande Group Header et définissez la condition de groupe
// Par exemple, pour grouper par catégorie : [Produits."Categorie"]
```

4. **Utiliser des scripts** : FastReport prend en charge les scripts pour ajouter une logique complexe.

```pascal
// Exemple de script dans le rapport (en Pascal)
procedure OnBeforePrint;
begin
  if <Produits."Prix"> > 1000 then
    Font.Color := clRed
  else
    Font.Color := clBlack;
end;
```

5. **Créer des graphiques** : Ajoutez des visualisations à vos rapports.

```pascal
// Dans le code, après avoir créé un rapport avec TfrxReport
var
  Chart: TfrxChartView;
begin
  Chart := TfrxChartView.Create(frxReport1);
  // Configuration du graphique
  Chart.Left := 10;
  Chart.Top := 10;
  Chart.Width := 400;
  Chart.Height := 300;
  Chart.Chart.Title.Text.Add('Ventes mensuelles');
  // Ajouter séries, données, etc.
end;
```

## Exemple pratique : Génération d'étiquettes avec QuickReport

Voici un exemple de génération d'étiquettes d'adresses avec QuickReport :

```pascal
procedure TMainForm.CreateAddressLabels;
var
  Report: TQuickRep;
  DetailBand: TQRBand;
  Label: TQRLabel;
  DBText: TQRDBText;
begin
  // Créer le rapport
  Report := TQuickRep.Create(Self);
  Report.Parent := Self;
  Report.Visible := False;

  // Configuration pour étiquettes 3x8 (24 par page)
  Report.Page.Orientation := poPortrait;
  Report.Page.PaperSize := Custom;
  Report.Page.Width := 210; // A4 width in mm
  Report.Page.Height := 297; // A4 height in mm

  // Définir les marges
  Report.Page.LeftMargin := 5;
  Report.Page.TopMargin := 10;
  Report.Page.RightMargin := 5;
  Report.Page.BottomMargin := 5;

  // Lier aux données
  Report.DataSet := ClientDataSet1; // Table de clients

  // Créer une bande de détail pour chaque étiquette
  DetailBand := TQRBand.Create(Report);
  DetailBand.Parent := Report;
  DetailBand.BandType := rbDetail;

  // Taille de l'étiquette (3 colonnes, 8 lignes)
  DetailBand.Size.Width := (Report.Page.Width - Report.Page.LeftMargin - Report.Page.RightMargin) / 3;
  DetailBand.Size.Height := (Report.Page.Height - Report.Page.TopMargin - Report.Page.BottomMargin) / 8;

  // Options d'impression en colonnes
  Report.Columns := 3; // 3 colonnes

  // Ajouter les champs d'adresse
  // Nom
  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.Left := 5;
  DBText.Top := 5;
  DBText.Width := DetailBand.Width - 10;
  DBText.Height := 20;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'NomClient';
  DBText.Font.Style := [fsBold];

  // Adresse ligne 1
  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.Left := 5;
  DBText.Top := 25;
  DBText.Width := DetailBand.Width - 10;
  DBText.Height := 20;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'Adresse';

  // Code postal et ville
  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.Left := 5;
  DBText.Top := 45;
  DBText.Width := DetailBand.Width - 10;
  DBText.Height := 20;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'CodePostal';

  DBText := TQRDBText.Create(DetailBand);
  DBText.Parent := DetailBand;
  DBText.Left := 40;
  DBText.Top := 45;
  DBText.Width := DetailBand.Width - 45;
  DBText.Height := 20;
  DBText.DataSet := ClientDataSet1;
  DBText.DataField := 'Ville';

  // Prévisualiser
  Report.Preview;
end;
```

## Rapport interactif avec FastReport

FastReport permet de créer des rapports interactifs avec des liens et des actions :

```pascal
procedure TMainForm.CreateInteractiveReport;
var
  Page: TfrxReportPage;
  Band: TfrxBand;
  Memo: TfrxMemoView;
begin
  // Créer un rapport de base
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Bande de titre
  Band := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 50;

  // Titre interactif qui ouvre une URL quand on clique dessus
  Memo := TfrxMemoView.Create(frxReport1);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'Rapport interactif - Cliquez pour plus d''informations';
  Memo.Font.Size := 14;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;
  Memo.Font.Color := clBlue;
  Memo.Cursor := crHandPoint;
  Memo.Underlines := True;

  // Définir l'action pour ouvrir une URL
  Memo.URL := 'http://www.example.com';

  // Le reste du rapport...

  // Prévisualiser
  frxReport1.ShowReport;
end;
```

## Intégration de rapports dans votre application

Pour une meilleure intégration des rapports dans votre application, suivez ces conseils :

### 1. Organisation des rapports

Créez une structure de répertoires pour vos modèles de rapports :

```
MonApplication/
  |- Rapports/
      |- Modeles/
          |- Factures.fr3
          |- Clients.fr3
          |- Etiquettes.qrp
      |- Sorties/
          |- (fichiers PDF générés)
```

### 2. Gestionnaire de rapports

Créez une classe dédiée à la gestion des rapports :

```pascal
unit ReportManager;

interface

uses
  System.SysUtils, System.Classes, frxClass, frxDBSet, frxExportPDF;

type
  TReportType = (rtInvoice, rtCustomerList, rtProductCatalog, rtLabels);

  TReportManager = class
  private
    FReport: TfrxReport;
    FPDFExport: TfrxPDFExport;
    procedure ConfigureDataSets;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowReport(ReportType: TReportType);
    procedure ExportToPDF(ReportType: TReportType; const FileName: string);
    procedure SetReportParameter(const ParamName: string; const ParamValue: Variant);
  end;

implementation

constructor TReportManager.Create;
begin
  inherited Create;

  FReport := TfrxReport.Create(nil);
  FPDFExport := TfrxPDFExport.Create(nil);

  // Configuration des exports
  FPDFExport.ShowProgress := False;
  FPDFExport.ShowDialog := False;
  FPDFExport.Creator := 'Mon Application Delphi';
  FPDFExport.Subject := 'Rapport généré par Mon Application';
end;

destructor TReportManager.Destroy;
begin
  FReport.Free;
  FPDFExport.Free;

  inherited;
end;

procedure TReportManager.ConfigureDataSets;
var
  ClientDataSet: TfrxDBDataset;
begin
  // Nettoyer les datasets existants
  FReport.DataSets.Clear;

  // Ajouter les datasets nécessaires
  ClientDataSet := TfrxDBDataset.Create(FReport);
  ClientDataSet.DataSet := DataModule1.ClientDataSet1; // Votre source de données
  ClientDataSet.UserName := 'Clients';
  FReport.DataSets.Add(ClientDataSet);

  // Ajouter d'autres datasets selon vos besoins
end;

procedure TReportManager.ShowReport(ReportType: TReportType);
var
  TemplateFile: string;
begin
  // Sélectionner le bon template selon le type de rapport
  case ReportType of
    rtInvoice: TemplateFile := 'Rapports\Modeles\Facture.fr3';
    rtCustomerList: TemplateFile := 'Rapports\Modeles\Clients.fr3';
    rtProductCatalog: TemplateFile := 'Rapports\Modeles\Catalogue.fr3';
    rtLabels: TemplateFile := 'Rapports\Modeles\Etiquettes.fr3';
  else
    raise Exception.Create('Type de rapport non reconnu');
  end;

  // Vérifier si le fichier existe
  if not FileExists(TemplateFile) then
    raise Exception.CreateFmt('Modèle de rapport introuvable: %s', [TemplateFile]);

  // Configurer les datasets
  ConfigureDataSets;

  // Charger et afficher le rapport
  FReport.LoadFromFile(TemplateFile);
  FReport.ShowReport;
end;

procedure TReportManager.ExportToPDF(ReportType: TReportType; const FileName: string);
begin
  // Préparer le rapport
  ShowReport(ReportType);

  // Configurer l'export PDF
  FPDFExport.FileName := FileName;

  // Exporter
  FReport.PrepareReport;
  FReport.Export(FPDFExport);
end;

procedure TReportManager.SetReportParameter(const ParamName: string; const ParamValue: Variant);
begin
  if VarIsStr(ParamValue) then
    FReport.Variables[ParamName] := QuotedStr(ParamValue)
  else
    FReport.Variables[ParamName] := ParamValue;
end;

end.
```

### 3. Utilisation dans votre application

```pascal
procedure TMainForm.btnInvoiceClick(Sender: TObject);
var
  ReportMgr: TReportManager;
begin
  ReportMgr := TReportManager.Create;
  try
    // Définir des paramètres si nécessaire
    ReportMgr.SetReportParameter('NumeroFacture', 'FA-' + FormatDateTime('yyyymmdd', Date) + '-001');
    ReportMgr.SetReportParameter('DateFacture', FormatDateTime('dd/mm/yyyy', Date));

    // Afficher le rapport
    ReportMgr.ShowReport(rtInvoice);
  finally
    ReportMgr.Free;
  end;
end;

procedure TMainForm.btnExportPdfClick(Sender: TObject);
var
  ReportMgr: TReportManager;
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Enregistrer la facture en PDF';
    SaveDialog.DefaultExt := 'pdf';
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';
    SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'Rapports\Sorties';

    if SaveDialog.Execute then
    begin
      ReportMgr := TReportManager.Create;
      try
        // Exporter en PDF
        ReportMgr.ExportToPDF(rtInvoice, SaveDialog.FileName);

        // Ouvrir le PDF si l'utilisateur le souhaite
        if MessageDlg('Le PDF a été généré avec succès. Voulez-vous l''ouvrir?',
                      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
        begin
          ShellExecute(0, 'open', PChar(SaveDialog.FileName), nil, nil, SW_SHOWNORMAL);
        end;
      finally
        ReportMgr.Free;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;
```

## Dépannage et astuces

### Problèmes courants et solutions

#### 1. "Impossible de trouver le fichier modèle de rapport"

```pascal
// Vérifier le chemin complet du fichier
var
  FullPath: string;
begin
  FullPath := ExtractFilePath(Application.ExeName) + 'Rapports\Modeles\Facture.fr3';
  if not FileExists(FullPath) then
    ShowMessage('Fichier introuvable: ' + FullPath)
  else
    frxReport1.LoadFromFile(FullPath);
end;
```

#### 2. "Le rapport ne s'affiche pas correctement"

```pascal
// Activer le mode de débogage dans FastReport
frxReport1.ReportOptions.ShowProgress := True;
frxReport1.ReportOptions.ShowPreparedReport := True;
```

#### 3. "Les données ne s'affichent pas dans le rapport"

```pascal
// Vérifier que les datasets sont correctement assignés
if not Assigned(frxDBDataset1.DataSet) then
  ShowMessage('Dataset non assigné')
else if frxDBDataset1.DataSet.IsEmpty then
  ShowMessage('Dataset vide')
else
  ShowMessage('Dataset OK: ' + IntToStr(frxDBDataset1.DataSet.RecordCount) + ' enregistrements');
```

### Astuces pour optimiser les performances

1. **Précharger les modèles** : Chargez les modèles de rapports au démarrage de l'application.

```pascal
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Précharger les modèles fréquemment utilisés
  frxReport1.LoadFromFile('Rapports\Modeles\Facture.fr3');
  frxReport2.LoadFromFile('Rapports\Modeles\Clients.fr3');
end;
```

2. **Limiter les données** : Ne chargez que les données nécessaires.

```pascal
// Exemple avec une requête SQL filtrée
ADOQuery1.SQL.Text := 'SELECT * FROM Clients WHERE DateCommande >= :Date';
ADOQuery1.Parameters.ParamByName('Date').Value := StartOfTheMonth(Date);
ADOQuery1.Open;
```

3. **Utiliser des pages virtuelles** pour les grands rapports.

```pascal
frxReport1.ReportOptions.UseFileCache := True;
frxReport1.ReportOptions.MaxMemSize := 10; // MB
```

## Conclusion

Les générateurs de rapports comme FastReport et QuickReport sont des outils puissants qui simplifient considérablement la création de rapports professionnels dans vos applications Delphi. Ils offrent :

- Une interface de conception visuelle intuitive
- Des fonctionnalités d'impression et de prévisualisation avancées
- L'exportation vers différents formats
- L'intégration facile avec les sources de données

Bien que FastReport soit généralement plus riche en fonctionnalités, QuickReport reste une option viable pour des besoins plus simples. Le choix entre les deux dépendra de la complexité de vos rapports, de votre budget et de vos préférences personnelles.

En pratique, il est recommandé de :
1. Structurer vos rapports en modèles réutilisables
2. Créer une classe de gestion des rapports pour centraliser la logique
3. Utiliser des variables pour personnaliser les rapports sans modifier le design
4. Profiter des fonctionnalités interactives pour une meilleure expérience utilisateur

Dans la prochaine section, nous aborderons la création de rapports complexes avec des fonctionnalités avancées comme les sous-rapports, les graphiques et les tableaux croisés dynamiques.

⏭️ [Création de rapports complexes](09-rapports-et-impressions/04-creation-de-rapports-complexes.md)
