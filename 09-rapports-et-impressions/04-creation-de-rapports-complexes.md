# 9.4 Création de rapports complexes

## Introduction

Après avoir découvert les bases des générateurs de rapports dans les sections précédentes, nous allons maintenant nous plonger dans la création de rapports plus complexes. Ces rapports avancés vous permettront de présenter des informations de manière plus détaillée et structurée, de répondre aux besoins métier sophistiqués et d'offrir une meilleure expérience à vos utilisateurs.

Dans cette section, nous utiliserons principalement FastReport pour nos exemples, car il offre davantage de fonctionnalités avancées, mais les concepts généraux sont applicables à d'autres générateurs de rapports comme QuickReport.

## Rapports maître-détail

Les rapports maître-détail vous permettent d'afficher des données hiérarchiques, comme une facture avec ses lignes de détail ou un client avec ses commandes.

### Configuration des relations entre données

Pour créer un rapport maître-détail, vous devez d'abord établir les relations entre vos ensembles de données :

```pascal
procedure TMainForm.ConfigureDataSets;
begin
  // Dataset principal (maître)
  frxDBDatasetClients.DataSet := ClientDataSet1;
  frxDBDatasetClients.UserName := 'Clients';

  // Dataset détail
  frxDBDatasetCommandes.DataSet := ClientDataSet2;
  frxDBDatasetCommandes.UserName := 'Commandes';

  // Dataset pour les lignes de commandes
  frxDBDatasetLignesCommandes.DataSet := ClientDataSet3;
  frxDBDatasetLignesCommandes.UserName := 'LignesCommandes';

  // Ajout des datasets au rapport
  frxReport1.DataSets.Clear;
  frxReport1.DataSets.Add(frxDBDatasetClients);
  frxReport1.DataSets.Add(frxDBDatasetCommandes);
  frxReport1.DataSets.Add(frxDBDatasetLignesCommandes);
end;
```

### Création d'un rapport maître-détail dans FastReport

Voici comment structurer un rapport maître-détail avec FastReport :

1. **Créez une bande MasterData pour les données maîtres**
   - Par exemple, les informations du client

2. **Créez une bande DetailData pour les détails**
   - Par exemple, les commandes du client
   - Configurez la propriété `DataSet` pour qu'elle pointe vers votre dataset détail
   - Configurez la propriété `MasterSource` pour établir la relation

Par exemple, dans le designer FastReport :

```
- ReportTitle (Titre du rapport)
- PageHeader (En-tête de page)
- MasterData [Clients] (Informations client)
  - DetailData [Commandes] (Commandes du client)
    - DetailData [LignesCommandes] (Lignes de la commande)
- PageFooter (Pied de page)
```

### Exemple de code pour un rapport à trois niveaux

Voici comment créer un rapport client-commandes-lignes programmatiquement :

```pascal
procedure TMainForm.CreateMasterDetailReport;
var
  Page: TfrxReportPage;
  BandTitle, BandPageHeader, BandPageFooter: TfrxBand;
  BandMasterClient, BandDetailCommande, BandDetailLigne: TfrxBand;
  Memo: TfrxMemoView;
  Line: TfrxLineView;
begin
  // Configurer les datasets
  ConfigureDataSets;

  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // 1. Bande de titre du rapport
  BandTitle := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(BandTitle);
  BandTitle.Height := 40;

  Memo := TfrxMemoView.Create(frxReport1);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'LISTE DES CLIENTS ET COMMANDES';
  Memo.Font.Size := 16;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // 2. En-tête de page
  BandPageHeader := TfrxPageHeader.Create(frxReport1);
  Page.Bands.Add(BandPageHeader);
  BandPageHeader.Height := 20;

  // 3. Bande maître (Clients)
  BandMasterClient := TfrxMasterData.Create(frxReport1);
  Page.Bands.Add(BandMasterClient);
  BandMasterClient.Height := 60;
  BandMasterClient.DataSet := frxDBDatasetClients;

  // Ajout d'informations client
  Memo := TfrxMemoView.Create(frxReport1);
  BandMasterClient.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 10;
  Memo.Width := 400;
  Memo.Height := 20;
  Memo.Text := 'Client: [Clients."NomClient"] - Code: [Clients."CodeClient"]';
  Memo.Font.Style := [fsBold];

  Memo := TfrxMemoView.Create(frxReport1);
  BandMasterClient.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 30;
  Memo.Width := 400;
  Memo.Height := 20;
  Memo.Text := 'Adresse: [Clients."Adresse"], [Clients."Ville"] [Clients."CodePostal"]';

  // 4. Bande détail pour les commandes
  BandDetailCommande := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(BandDetailCommande);
  BandDetailCommande.Height := 50;
  BandDetailCommande.DataSet := frxDBDatasetCommandes;

  // En-tête des commandes
  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailCommande.Objects.Add(Memo);
  Memo.Left := 20; // Indentation pour montrer la hiérarchie
  Memo.Top := 5;
  Memo.Width := 400;
  Memo.Height := 20;
  Memo.Text := 'Commande N° [Commandes."NumeroCommande"] du [Commandes."DateCommande"]';
  Memo.Font.Style := [fsBold];

  // Titre des colonnes de lignes de commande
  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailCommande.Objects.Add(Memo);
  Memo.Left := 40;
  Memo.Top := 30;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := 'Produit';
  Memo.Font.Style := [fsBold];

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailCommande.Objects.Add(Memo);
  Memo.Left := 250;
  Memo.Top := 30;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := 'Quantité';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailCommande.Objects.Add(Memo);
  Memo.Left := 320;
  Memo.Top := 30;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'Prix Unitaire';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailCommande.Objects.Add(Memo);
  Memo.Left := 410;
  Memo.Top := 30;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'Total';
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haRight;

  // 5. Bande détail pour les lignes de commande
  BandDetailLigne := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(BandDetailLigne);
  BandDetailLigne.Height := 20;
  BandDetailLigne.DataSet := frxDBDatasetLignesCommandes;

  // Données des lignes de commande
  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailLigne.Objects.Add(Memo);
  Memo.Left := 40;
  Memo.Top := 0;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := '[LignesCommandes."DesignationProduit"]';

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailLigne.Objects.Add(Memo);
  Memo.Left := 250;
  Memo.Top := 0;
  Memo.Width := 60;
  Memo.Height := 20;
  Memo.Text := '[LignesCommandes."Quantite"]';
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailLigne.Objects.Add(Memo);
  Memo.Left := 320;
  Memo.Top := 0;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := '[LignesCommandes."PrixUnitaire"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetailLigne.Objects.Add(Memo);
  Memo.Left := 410;
  Memo.Top := 0;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := '[LignesCommandes."Quantite"] * [LignesCommandes."PrixUnitaire"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // 6. Pied de page
  BandPageFooter := TfrxPageFooter.Create(frxReport1);
  Page.Bands.Add(BandPageFooter);
  BandPageFooter.Height := 20;

  // Numéro de page
  Memo := TfrxMemoView.Create(frxReport1);
  BandPageFooter.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 20;
  Memo.Text := 'Page [Page#] sur [TotalPages#]';
  Memo.HAlign := haCenter;

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

## Utilisation des groupes

Les groupes permettent d'organiser les données par catégories, comme regrouper les clients par région ou les produits par catégorie.

### Création de groupes dans FastReport

Pour créer un groupe dans FastReport :

1. Ajoutez une bande **GroupHeader** pour l'en-tête du groupe
2. Ajoutez une bande **GroupFooter** pour le pied du groupe (optionnel)
3. Définissez la condition de groupe dans la propriété `Condition` de l'en-tête

```pascal
procedure TMainForm.CreateGroupedReport;
var
  Page: TfrxReportPage;
  BandTitle, BandPageHeader, BandPageFooter: TfrxBand;
  BandGroupHeader, BandGroupFooter, BandDetail: TfrxBand;
  Memo: TfrxMemoView;
  Line: TfrxLineView;
begin
  // Configurer les datasets
  frxDBDataset1.DataSet := ClientDataSet1; // Table de produits
  frxDBDataset1.UserName := 'Produits';
  frxReport1.DataSets.Clear;
  frxReport1.DataSets.Add(frxDBDataset1);

  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // 1. Bande de titre
  BandTitle := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(BandTitle);
  BandTitle.Height := 40;

  Memo := TfrxMemoView.Create(frxReport1);
  BandTitle.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'CATALOGUE DES PRODUITS PAR CATÉGORIE';
  Memo.Font.Size := 16;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // 2. En-tête de page
  BandPageHeader := TfrxPageHeader.Create(frxReport1);
  Page.Bands.Add(BandPageHeader);
  BandPageHeader.Height := 20;

  // 3. En-tête de groupe (par catégorie)
  BandGroupHeader := TfrxGroupHeader.Create(frxReport1);
  Page.Bands.Add(BandGroupHeader);
  BandGroupHeader.Height := 30;
  BandGroupHeader.Condition := '[Produits."Categorie"]';

  Memo := TfrxMemoView.Create(frxReport1);
  BandGroupHeader.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 5;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 25;
  Memo.Text := 'Catégorie: [Produits."Categorie"]';
  Memo.Font.Size := 12;
  Memo.Font.Style := [fsBold];

  // 4. Détails des produits
  BandDetail := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 20;
  BandDetail.DataSet := frxDBDataset1;

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetail.Objects.Add(Memo);
  Memo.Left := 20; // Indentation pour montrer la hiérarchie
  Memo.Top := 0;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := '[Produits."Reference"] - [Produits."Designation"]';

  Memo := TfrxMemoView.Create(frxReport1);
  BandDetail.Objects.Add(Memo);
  Memo.Left := 250;
  Memo.Top := 0;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := '[Produits."PrixHT"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // 5. Pied de groupe
  BandGroupFooter := TfrxGroupFooter.Create(frxReport1);
  Page.Bands.Add(BandGroupFooter);
  BandGroupFooter.Height := 30;

  // Ligne de séparation
  Line := TfrxLineView.Create(frxReport1);
  BandGroupFooter.Objects.Add(Line);
  Line.Left := 0;
  Line.Top := 5;
  Line.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Line.Height := 0;
  Line.Frame.Typ := [ftTop];

  // Total par catégorie
  Memo := TfrxMemoView.Create(frxReport1);
  BandGroupFooter.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 10;
  Memo.Width := 200;
  Memo.Height := 20;
  Memo.Text := 'Nombre de produits dans cette catégorie:';

  Memo := TfrxMemoView.Create(frxReport1);
  BandGroupFooter.Objects.Add(Memo);
  Memo.Left := 250;
  Memo.Top := 10;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := 'COUNT(Produits."Reference")';
  Memo.HAlign := haRight;

  // 6. Pied de page
  BandPageFooter := TfrxPageFooter.Create(frxReport1);
  Page.Bands.Add(BandPageFooter);
  BandPageFooter.Height := 20;

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Agrégations dans les groupes

Les groupes sont particulièrement utiles pour calculer des totaux et des statistiques :

- **SUM()** : Somme des valeurs
- **AVG()** : Moyenne des valeurs
- **MIN()** / **MAX()** : Valeur minimale/maximale
- **COUNT()** : Nombre d'éléments

Exemple d'utilisation dans un pied de groupe :

```pascal
// Dans la propriété Text d'un TfrxMemoView dans un pied de groupe
'Total des ventes: [SUM(<Ventes."Montant">,MasterData1)]'
'Prix moyen: [AVG(<Produits."PrixHT">,MasterData1)]'
'Produit le plus cher: [MAX(<Produits."PrixHT">,MasterData1)]'
```

## Sous-rapports

Les sous-rapports permettent d'inclure un rapport dans un autre, idéal pour des informations complémentaires ou des structures complexes.

### Création d'un sous-rapport

```pascal
procedure TMainForm.CreateReportWithSubreport;
var
  MainReport, SubReport: TfrxReport;
  Page: TfrxReportPage;
  BandDetail: TfrxBand;
  SubBand: TfrxSubreport;
begin
  // Rapport principal
  MainReport := frxReport1;
  MainReport.Clear;

  // Créer la page du rapport principal
  Page := TfrxReportPage.Create(MainReport);
  MainReport.Pages.Add(Page);

  // Ajouter une bande de détail
  BandDetail := TfrxDetailData.Create(MainReport);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 100;

  // Ajouter un sous-rapport
  SubBand := TfrxSubreport.Create(MainReport);
  BandDetail.Objects.Add(SubBand);
  SubBand.Left := 0;
  SubBand.Top := 30;
  SubBand.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  SubBand.Height := 20;

  // Créer le sous-rapport
  SubReport := TfrxReport.Create(Self);
  SubBand.Report := SubReport;

  // Configurer le sous-rapport
  // (Code pour créer le contenu du sous-rapport)

  // Prévisualiser le rapport principal
  MainReport.ShowReport;
end;
```

### Utilisation pratique des sous-rapports

Les sous-rapports sont utiles pour :

1. **Rapports avec des sections conditionnelles**
   - Par exemple, afficher différentes informations selon le type de client

2. **Données non liées directement**
   - Par exemple, afficher des statistiques globales dans un rapport client

3. **Mise en page complexe**
   - Par exemple, afficher un graphique à côté d'un tableau

## Tableaux croisés dynamiques

Les tableaux croisés dynamiques (cross-tab) permettent d'afficher des données sous forme de matrice, avec des totaux en ligne et en colonne.

### Création d'un tableau croisé dans FastReport

```pascal
procedure TMainForm.CreateCrossTabReport;
var
  Page: TfrxReportPage;
  Band: TfrxBand;
  CrossTab: TfrxCrossView;
begin
  // Configurer les datasets
  frxDBDataset1.DataSet := ClientDataSet1; // Table des ventes
  frxDBDataset1.UserName := 'Ventes';
  frxReport1.DataSets.Clear;
  frxReport1.DataSets.Add(frxDBDataset1);

  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Ajouter une bande de rapport
  Band := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 100;

  // Ajouter un tableau croisé
  CrossTab := TfrxCrossView.Create(frxReport1);
  Band.Objects.Add(CrossTab);
  CrossTab.Left := 0;
  CrossTab.Top := 30;
  CrossTab.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  CrossTab.Height := 200;
  CrossTab.DataSet := frxDBDataset1;

  // Configurer les dimensions du tableau croisé
  CrossTab.RowFields.Add('Categorie');         // Lignes: Catégories de produits
  CrossTab.ColumnFields.Add('MONTHNAME(DateVente)'); // Colonnes: Mois de vente
  CrossTab.CellFields.Add('SUM(Montant)');     // Valeurs: Somme des montants

  // Format d'affichage pour les cellules
  CrossTab.CellFields[0].DisplayFormat.FormatStr := '%2.2f €';
  CrossTab.CellFields[0].DisplayFormat.Kind := fkNumeric;

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Personnalisation des tableaux croisés

Vous pouvez personnaliser l'apparence et le comportement des tableaux croisés :

```pascal
// Style et apparence
CrossTab.RowLevels[0].ShowHeader := True;
CrossTab.ColumnLevels[0].ShowHeader := True;
CrossTab.CellFields[0].Alignment := taRightJustify;
CrossTab.ColumnGap := 4;
CrossTab.RowGap := 4;
CrossTab.ShowColumnTotal := True;
CrossTab.ShowRowTotal := True;
CrossTab.ShowCornerTotal := True;

// Textes personnalisés pour les totaux
CrossTab.ColumnTotalCaption := 'Total mensuel';
CrossTab.RowTotalCaption := 'Total par catégorie';
CrossTab.CornerTotalCaption := 'Total général';
```

## Graphiques et visualisations

Les graphiques permettent de visualiser les données et de faire ressortir les tendances.

### Création d'un graphique dans FastReport

```pascal
procedure TMainForm.CreateChartReport;
var
  Page: TfrxReportPage;
  Band: TfrxReportTitle;
  Chart: TfrxChartView;
begin
  // Configurer les datasets
  frxDBDataset1.DataSet := ClientDataSet1; // Table des ventes
  frxDBDataset1.UserName := 'Ventes';
  frxReport1.DataSets.Clear;
  frxReport1.DataSets.Add(frxDBDataset1);

  // Créer un nouveau rapport
  frxReport1.Clear;

  // Ajouter une page
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Ajouter une bande de titre
  Band := TfrxReportTitle.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 400; // Assez grand pour contenir le graphique

  // Ajouter un graphique
  Chart := TfrxChartView.Create(frxReport1);
  Band.Objects.Add(Chart);
  Chart.Left := 0;
  Chart.Top := 40;
  Chart.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Chart.Height := 300;
  Chart.Chart.Title.Text.Add('Évolution des ventes par mois');

  // Configurer le type de graphique
  Chart.Chart.SeriesType := stLine; // Graphique en ligne

  // Définir les séries de données
  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := False;
    DataSource := frxDBDataset1;
    XSource := 'MONTHNAME(DateVente)'; // Axe X : mois
    YSource := 'SUM(Montant)';    // Axe Y : montant des ventes
    Name := 'Ventes';
  end;

  // Ajouter une deuxième série (exemple)
  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := False;
    DataSource := frxDBDataset1;
    XSource := 'MONTHNAME(DateVente)'; // Axe X : mois
    YSource := 'SUM(Quantite)';    // Axe Y : quantité vendue
    Name := 'Quantités';
  end;

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Types de graphiques disponibles

FastReport prend en charge de nombreux types de graphiques :

- Histogrammes (Bar)
- Lignes (Line)
- Secteurs (Pie)
- Aires (Area)
- Points (Points)
- Combinés (plusieurs types)

```pascal
// Changer le type de graphique
Chart.Chart.SeriesType := stBar;   // Histogramme
Chart.Chart.SeriesType := stLine;  // Ligne
Chart.Chart.SeriesType := stPie;   // Secteur (camembert)
Chart.Chart.SeriesType := stArea;  // Aire
Chart.Chart.SeriesType := stPoint; // Points
```

## Formulaires et rapports interactifs

Les formulaires permettent de demander des paramètres à l'utilisateur avant l'exécution du rapport.

### Création d'un dialogue de paramètres

```pascal
procedure TMainForm.CreateParametrizedReport;
var
  Report: TfrxReport;
  Dialog: TfrxDialogPage;
  Page: TfrxReportPage;
  Edit: TfrxEditControl;
  DateEdit: TfrxDateEditControl;
  Button: TfrxButtonControl;
  Label: TfrxLabelControl;
  Band: TfrxBand;
  Memo: TfrxMemoView;
begin
  Report := frxReport1;
  Report.Clear;

  // 1. Créer une page de dialogue
  Dialog := TfrxDialogPage.Create(Report);
  Report.Pages.Add(Dialog);
  Dialog.Width := 300;
  Dialog.Height := 200;
  Dialog.Caption := 'Paramètres du rapport';

  // Ajouter des contrôles au dialogue
  Label := TfrxLabelControl.Create(Dialog);
  Label.Parent := Dialog;
  Label.Left := 12;
  Label.Top := 20;
  Label.Width := 100;
  Label.Height := 20;
  Label.Caption := 'Date de début:';

  DateEdit := TfrxDateEditControl.Create(Dialog);
  DateEdit.Parent := Dialog;
  DateEdit.Left := 120;
  DateEdit.Top := 20;
  DateEdit.Width := 150;
  DateEdit.Height := 24;
  DateEdit.Date := Date - 30; // Par défaut: 30 jours avant aujourd'hui
  DateEdit.Name := 'edDateDebut';

  Label := TfrxLabelControl.Create(Dialog);
  Label.Parent := Dialog;
  Label.Left := 12;
  Label.Top := 50;
  Label.Width := 100;
  Label.Height := 20;
  Label.Caption := 'Date de fin:';

  DateEdit := TfrxDateEditControl.Create(Dialog);
  DateEdit.Parent := Dialog;
  DateEdit.Left := 120;
  DateEdit.Top := 50;
  DateEdit.Width := 150;
  DateEdit.Height := 24;
  DateEdit.Date := Date; // Par défaut: aujourd'hui
  DateEdit.Name := 'edDateFin';

  Label := TfrxLabelControl.Create(Dialog);
  Label.Parent := Dialog;
  Label.Left := 12;
  Label.Top := 80;
  Label.Width := 100;
  Label.Height := 20;
  Label.Caption := 'Client:';

  Edit := TfrxEditControl.Create(Dialog);
  Edit.Parent := Dialog;
  Edit.Left := 120;
  Edit.Top := 80;
  Edit.Width := 150;
  Edit.Height := 24;
  Edit.Text := 'Tous';
  Edit.Name := 'edClient';

  Button := TfrxButtonControl.Create(Dialog);
  Button.Parent := Dialog;
  Button.Left := 120;
  Button.Top := 120;
  Button.Width := 80;
  Button.Height := 30;
  Button.Caption := 'OK';
  Button.ModalResult := mrOk;
  Button.Default := True;

  Button := TfrxButtonControl.Create(Dialog);
  Button.Parent := Dialog;
  Button.Left := 210;
  Button.Top := 120;
  Button.Width := 80;
  Button.Height := 30;
  Button.Caption := 'Annuler';
  Button.ModalResult := mrCancel;
  Button.Cancel := True;

  // 2. Créer la page du rapport
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Ajouter une bande de titre
  Band := TfrxReportTitle.Create(Report);
  Page.Bands.Add(Band);
  Band.Height := 60;

  // Titre du rapport avec paramètres
  Memo := TfrxMemoView.Create(Report);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 30;
  Memo.Text := 'RAPPORT DES VENTES';
  Memo.Font.Size := 16;
  Memo.Font.Style := [fsBold];
  Memo.HAlign := haCenter;

  // Sous-titre avec période
  Memo := TfrxMemoView.Create(Report);
  Band.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 30;
  Memo.Width := Page.Width - Page.LeftMargin - Page.RightMargin;
  Memo.Height := 20;
  Memo.Text := 'Période du [<DateDebut>] au [<DateFin>]';
  Memo.HAlign := haCenter;

  // Ajouter le script pour récupérer les valeurs du dialogue
  Report.Script.Text :=
    'var DateDebut, DateFin, Client: Variant;' + #13#10 +
    'procedure DialogPage1OnShow(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Code exécuté à l''affichage du dialogue' + #13#10 +
    'end;' + #13#10 +
    'procedure DialogPage1OnHide(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Récupérer les valeurs saisies' + #13#10 +
    '  DateDebut := edDateDebut.Date;' + #13#10 +
    '  DateFin := edDateFin.Date;' + #13#10 +
    '  Client := edClient.Text;' + #13#10 +
    '  // Les rendre disponibles comme variables du rapport' + #13#10 +
    '  Report.Variables["DateDebut"] := DateToStr(DateDebut);' + #13#10 +
    '  Report.Variables["DateFin"] := DateToStr(DateFin);' + #13#10 +
    '  Report.Variables["Client"] := Client;' + #13#10 +
    'end;' + #13#10;

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

### Filtrage des données selon les paramètres

Une fois les paramètres récupérés, vous pouvez les utiliser pour filtrer les données :

```pascal
procedure TMainForm.ApplyReportParams;
begin
  // Récupérer les valeurs du dialogue
  DateDebut := frxReport1.Variables['DateDebut'];
  DateFin := frxReport1.Variables['DateFin'];
  Client := frxReport1.Variables['Client'];

  // Filtrer le dataset
  if Client <> 'Tous' then
  begin
    ClientDataSet1.Filter := Format('DateVente >= ''%s'' AND DateVente <= ''%s'' AND Client = ''%s''',
      [DateDebut, DateFin, Client]);
  end
  else
  begin
    ClientDataSet1.Filter := Format('DateVente >= ''%s'' AND DateVente <= ''%s''',
      [DateDebut, DateFin]);
  end;

  ClientDataSet1.Filtered := True;
end;
```

## Rapports conditionnels

Les rapports conditionnels permettent de modifier l'apparence ou le contenu du rapport en fonction des données.

### Mise en forme conditionnelle

```pascal
procedure TMainForm.CreateConditionalFormattingReport;
var
  Page: TfrxReportPage;
  BandDetail: TfrxDetailData;
  Memo: TfrxMemoView;
begin
  // Configuration de base du rapport
  frxReport1.Clear;
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Bande de détail
  BandDetail := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 20;
  BandDetail.DataSet := frxDBDataset1;

  // Champ avec mise en forme conditionnelle
  Memo := TfrxMemoView.Create(frxReport1);
  BandDetail.Objects.Add(Memo);
  Memo.Left := 0;
  Memo.Top := 0;
  Memo.Width := 300;
  Memo.Height := 20;
  Memo.Text := '[Produits."Designation"]';

  // Prix avec mise en forme conditionnelle
  Memo := TfrxMemoView.Create(frxReport1);
  BandDetail.Objects.Add(Memo);
  Memo.Left := 310;
  Memo.Top := 0;
  Memo.Width := 80;
  Memo.Height := 20;
  Memo.Text := '[Produits."Prix"]';
  Memo.DisplayFormat.FormatStr := '%2.2f €';
  Memo.DisplayFormat.Kind := fkNumeric;
  Memo.HAlign := haRight;

  // Script pour la mise en forme conditionnelle
  Memo.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  // Prix en rouge si > 1000 €' + #13#10 +
    '  if <Produits."Prix"> > 1000 then' + #13#10 +
    '  begin' + #13#10 +
    '    Font.Color := clRed;' + #13#10 +
    '    Font.Style := [fsBold];' + #13#10 +
    '  end' + #13#10 +
    '  // Prix en vert si < 50 €' + #13#10 +
    '  else if <Produits."Prix"> < 50 then' + #13#10 +
    '  begin' + #13#10 +
    '    Font.Color := clGreen;' + #13#10 +
    '  end' + #13#10 +
    '  else' + #13#10 +
    '  begin' + #13#10 +
    '    Font.Color := clBlack;' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Affichage conditionnel d'objets

```pascal
// Dans le script d'un objet TfrxMemoView
'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
'begin' + #13#10 +
'  // Masquer l''objet si le stock est à zéro' + #13#10 +
'  if <Produits."Stock"> = 0 then' + #13#10 +
'    Visible := False' + #13#10 +
'  else' + #13#10 +
'    Visible := True;' + #13#10 +
'end;';
```

## Personnalisation avancée avec des scripts

Les scripts permettent d'ajouter une logique complexe à vos rapports.

### Utilisation du langage script de FastReport

FastReport utilise Pascal Script comme langage de script par défaut.

```pascal
// Ajouter un script global au rapport
frxReport1.Script.Text :=
  'var TotalGeneral: Double;' + #13#10 +
  '' + #13#10 +
  'procedure ReportTitle1OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
  'begin' + #13#10 +
  '  // Initialiser le total général' + #13#10 +
  '  TotalGeneral := 0;' + #13#10 +
  'end;' + #13#10 +
  '' + #13#10 +
  'procedure DetailData1OnAfterPrint(Sender: TfrxComponent);' + #13#10 +
  'begin' + #13#10 +
  '  // Additionner au total général' + #13#10 +
  '  TotalGeneral := TotalGeneral + <LignesCommandes."PrixUnitaire"> * <LignesCommandes."Quantite">;' + #13#10 +
  'end;' + #13#10 +
  '' + #13#10 +
  'function FormatMontant(Montant: Double): String;' + #13#10 +
  'begin' + #13#10 +
  '  // Fonction personnalisée pour formater un montant' + #13#10 +
  '  Result := FormatFloat(''#,##0.00 €'', Montant);' + #13#10 +
  'end;';
```

### Événements disponibles dans les scripts

Chaque bande et objet peut réagir à différents événements :

```
OnBeforePrint : Avant l'impression de l'objet/bande
OnAfterPrint : Après l'impression de l'objet/bande
OnBeforeData : Avant le chargement des données
OnAfterData : Après le chargement des données
```

### Exemple de calcul complexe avec script

```pascal
// Exemple : calcul de la marge brute et du pourcentage de marge
Memo := TfrxMemoView.Create(frxReport1);
BandDetail.Objects.Add(Memo);
Memo.Left := 400;
Memo.Top := 0;
Memo.Width := 80;
Memo.Height := 20;
Memo.Text := '[Marge]';
Memo.DisplayFormat.FormatStr := '%2.2f €';
Memo.DisplayFormat.Kind := fkNumeric;
Memo.HAlign := haRight;

Memo.Script :=
  'var PrixVente, PrixAchat, Marge, PourcentageMarge: Double;' + #13#10 +
  '' + #13#10 +
  'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
  'begin' + #13#10 +
  '  // Récupérer les prix' + #13#10 +
  '  PrixVente := <Produits."PrixVente">;' + #13#10 +
  '  PrixAchat := <Produits."PrixAchat">;' + #13#10 +
  '  ' + #13#10 +
  '  // Calculer la marge' + #13#10 +
  '  Marge := PrixVente - PrixAchat;' + #13#10 +
  '  ' + #13#10 +
  '  // Calculer le pourcentage de marge' + #13#10 +
  '  if PrixAchat > 0 then' + #13#10 +
  '    PourcentageMarge := (Marge / PrixAchat) * 100' + #13#10 +
  '  else' + #13#10 +
  '    PourcentageMarge := 0;' + #13#10 +
  '  ' + #13#10 +
  '  // Stocker dans une variable pour l''affichage' + #13#10 +
  '  <Marge> := Marge;' + #13#10 +
  '  ' + #13#10 +
  '  // Colorer selon la marge' + #13#10 +
  '  if PourcentageMarge < 15 then' + #13#10 +
  '    Font.Color := clRed' + #13#10 +
  '  else if PourcentageMarge > 50 then' + #13#10 +
  '    Font.Color := clGreen' + #13#10 +
  '  else' + #13#10 +
  '    Font.Color := clBlack;' + #13#10 +
  'end;';
```

## Création de codes-barres et QR codes

FastReport permet d'intégrer facilement des codes-barres et QR codes dans vos rapports.

### Ajout d'un code-barres

```pascal
procedure TMainForm.AddBarcodesToReport;
var
  Page: TfrxReportPage;
  Band: TfrxDetailData;
  Barcode: TfrxBarCodeView;
  QRCode: TfrxBarCodeView;
begin
  // Configuration de base du rapport
  frxReport1.Clear;
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Bande de détail
  Band := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(Band);
  Band.Height := 120;
  Band.DataSet := frxDBDataset1;

  // Ajouter un code-barres EAN-13
  Barcode := TfrxBarCodeView.Create(frxReport1);
  Band.Objects.Add(Barcode);
  Barcode.Left := 0;
  Barcode.Top := 10;
  Barcode.Width := 200;
  Barcode.Height := 50;
  Barcode.BarType := bcEAN13;
  Barcode.Expression := '[Produits."CodeEAN"]';
  Barcode.Text := ''; // Sera remplacé par l'expression
  Barcode.Rotation := 0;
  Barcode.ShowText := True;

  // Ajouter un QR Code
  QRCode := TfrxBarCodeView.Create(frxReport1);
  Band.Objects.Add(QRCode);
  QRCode.Left := 250;
  QRCode.Top := 10;
  QRCode.Width := 100;
  QRCode.Height := 100;
  QRCode.BarType := bcQR;
  QRCode.Expression := '[Produits."Reference"]';
  QRCode.Text := ''; // Sera remplacé par l'expression
  QRCode.Rotation := 0;

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Types de codes-barres disponibles

FastReport prend en charge de nombreux types de codes-barres :

```pascal
// Types courants
bcEAN13      // Code EAN-13 (utilisé pour les produits de grande consommation)
bcEAN8       // Version courte de l'EAN-13
bcUPC_A      // Universal Product Code (utilisé aux États-Unis)
bcCode39     // Code 39 (alphanumédique)
bcCode128    // Code 128 (très polyvalent)
bcQR         // QR Code (matrice 2D à haute capacité)
bcDataMatrix // DataMatrix (matrice 2D dense)
bcPDF417     // PDF417 (code-barres 2D haute capacité)
```

## Rapports multi-langues

Pour créer des rapports disponibles en plusieurs langues, vous pouvez utiliser la fonctionnalité de dictionnaire de FastReport.

### Configuration d'un rapport multilingue

```pascal
procedure TMainForm.CreateMultiLanguageReport;
var
  Report: TfrxReport;
  Dict: TfrxCustomLanguageDict;
begin
  Report := frxReport1;

  // Créer un dictionnaire
  Dict := TfrxCustomLanguageDict.Create;

  // Ajouter des traductions
  Dict.StrL.Add('Titre');
  Dict.StrR.Add('Title');
  Dict.StrL.Add('Montant');
  Dict.StrR.Add('Amount');
  Dict.StrL.Add('Client');
  Dict.StrR.Add('Customer');
  Dict.StrL.Add('Date');
  Dict.StrR.Add('Date');
  Dict.StrL.Add('Page');
  Dict.StrR.Add('Page');

  // Assigner le dictionnaire au rapport
  Report.Dictionary := Dict;

  // Par défaut, utiliser la langue française (Left)
  Report.UseLanguage := True;
  Report.SelectedLanguage := 0; // 0 pour langue Left, 1 pour langue Right

  // Charger le modèle de rapport
  Report.LoadFromFile('Rapports\Modeles\RapportBilingue.fr3');

  // Prévisualiser
  Report.ShowReport;
end;
```

### Changement de langue à l'exécution

```pascal
procedure TMainForm.cbLangueChange(Sender: TObject);
begin
  case cbLangue.ItemIndex of
    0: // Français
      begin
        frxReport1.SelectedLanguage := 0;
      end;
    1: // Anglais
      begin
        frxReport1.SelectedLanguage := 1;
      end;
  end;

  // Recharger le rapport avec la nouvelle langue
  frxReport1.PrepareReport;
  frxPreview1.Refresh;
end;
```

## Rapports avec images dynamiques

Vous pouvez intégrer des images dynamiques dans vos rapports, comme des logos de clients ou des photos de produits.

### Chargement d'images depuis une base de données

```pascal
procedure TMainForm.CreateReportWithImages;
var
  Page: TfrxReportPage;
  BandDetail: TfrxDetailData;
  Picture: TfrxPictureView;
begin
  // Configuration de base du rapport
  frxReport1.Clear;
  Page := TfrxReportPage.Create(frxReport1);
  frxReport1.Pages.Add(Page);

  // Bande de détail
  BandDetail := TfrxDetailData.Create(frxReport1);
  Page.Bands.Add(BandDetail);
  BandDetail.Height := 120;
  BandDetail.DataSet := frxDBDataset1;

  // Ajouter une image
  Picture := TfrxPictureView.Create(frxReport1);
  BandDetail.Objects.Add(Picture);
  Picture.Left := 10;
  Picture.Top := 10;
  Picture.Width := 100;
  Picture.Height := 100;
  Picture.HightQuality := True;
  Picture.DataField := 'Photo'; // Champ BLOB de la base de données
  Picture.DataSet := frxDBDataset1;

  // Script pour gérer les images manquantes
  Picture.Script :=
    'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
    'begin' + #13#10 +
    '  if Picture.IsNull then' + #13#10 +
    '  begin' + #13#10 +
    '    // Charger une image par défaut' + #13#10 +
    '    Picture.LoadFromFile(''Images\NoImage.jpg'');' + #13#10 +
    '  end;' + #13#10 +
    'end;';

  // Prévisualiser le rapport
  frxReport1.ShowReport;
end;
```

### Chargement d'images depuis des fichiers

```pascal
// Dans le script d'un objet TfrxPictureView
'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
'var' + #13#10 +
'  CheminImage: String;' + #13#10 +
'begin' + #13#10 +
'  // Construire le chemin de l''image à partir du code produit' + #13#10 +
'  CheminImage := ''Images\Produits\'' + <Produits."Reference"> + ''.jpg'';' + #13#10 +
'  ' + #13#10 +
'  // Vérifier si le fichier existe' + #13#10 +
'  if FileExists(CheminImage) then' + #13#10 +
'    Picture.LoadFromFile(CheminImage)' + #13#10 +
'  else' + #13#10 +
'    Picture.LoadFromFile(''Images\NoImage.jpg'');' + #13#10 +
'end;';
```

## Rapports avec signatures et annotations

Vous pouvez ajouter des annotations et des zones de signature à vos rapports.

### Ajout d'une zone de signature

```pascal
procedure TMainForm.AddSignatureArea;
var
  Page: TfrxReportPage;
  BandFooter: TfrxReportSummary;
  Shape: TfrxShapeView;
  Memo: TfrxMemoView;
begin
  // Configuration de base du rapport
  // ...

  // Pied de rapport
  BandFooter := TfrxReportSummary.Create(frxReport1);
  Page.Bands.Add(BandFooter);
  BandFooter.Height := 100;

  // Zone de signature (rectangle)
  Shape := TfrxShapeView.Create(frxReport1);
  BandFooter.Objects.Add(Shape);
  Shape.Left := 350;
  Shape.Top := 20;
  Shape.Width := 200;
  Shape.Height := 60;
  Shape.Shape := skRectangle;
  Shape.Frame.Typ := [ftLeft, ftRight, ftTop, ftBottom];
  Shape.Frame.Width := 0.5;

  // Texte de signature
  Memo := TfrxMemoView.Create(frxReport1);
  BandFooter.Objects.Add(Memo);
  Memo.Left := 350;
  Memo.Top := 5;
  Memo.Width := 200;
  Memo.Height := 15;
  Memo.Text := 'Signature du client:';
  Memo.Font.Style := [fsBold];

  // Date
  Memo := TfrxMemoView.Create(frxReport1);
  BandFooter.Objects.Add(Memo);
  Memo.Left := 350;
  Memo.Top := 85;
  Memo.Width := 200;
  Memo.Height := 15;
  Memo.Text := 'Date: ___/___/______';
end;
```

## Impression par lots et fusion de rapports

Pour des besoins avancés, vous pouvez fusionner plusieurs rapports ou imprimer par lots.

### Fusion de rapports

```pascal
procedure TMainForm.MergeReports;
var
  Report1, Report2, FinalReport: TfrxReport;
  Page: TfrxReportPage;
begin
  // Créer les rapports
  Report1 := TfrxReport.Create(nil);
  Report2 := TfrxReport.Create(nil);
  FinalReport := TfrxReport.Create(nil);

  try
    // Charger les rapports à fusionner
    Report1.LoadFromFile('Rapports\Modeles\Rapport1.fr3');
    Report2.LoadFromFile('Rapports\Modeles\Rapport2.fr3');

    // Préparer les rapports
    Report1.PrepareReport;
    Report2.PrepareReport;

    // Fusionner dans le rapport final
    FinalReport.Clear;

    // Copier les pages du premier rapport
    for var i := 0 to Report1.PreviewPages.Count - 1 do
      FinalReport.PreviewPages.Add(Report1.PreviewPages[i]);

    // Copier les pages du second rapport
    for var i := 0 to Report2.PreviewPages.Count - 1 do
      FinalReport.PreviewPages.Add(Report2.PreviewPages[i]);

    // Afficher le rapport fusionné
    FinalReport.ShowPreparedReport;
  finally
    Report1.Free;
    Report2.Free;
    FinalReport.Free;
  end;
end;
```

### Impression par lots

```pascal
procedure TMainForm.BatchPrintReports;
var
  Reports: TStringList;
  i: Integer;
  Report: TfrxReport;
begin
  // Liste des rapports à imprimer
  Reports := TStringList.Create;
  try
    Reports.Add('Rapports\Modeles\Facture.fr3');
    Reports.Add('Rapports\Modeles\BonLivraison.fr3');
    Reports.Add('Rapports\Modeles\Etiquettes.fr3');

    // Créer un rapport temporaire
    Report := TfrxReport.Create(nil);
    try
      // Parcourir la liste et imprimer chaque rapport
      for i := 0 to Reports.Count - 1 do
      begin
        Report.Clear;
        Report.LoadFromFile(Reports[i]);

        // Configurer les données
        ConfigureReportData(Report, i);

        // Imprimer directement sans aperçu
        Report.PrepareReport;
        Report.Print;
      end;
    finally
      Report.Free;
    end;
  finally
    Reports.Free;
  end;
end;

procedure TMainForm.ConfigureReportData(Report: TfrxReport; Index: Integer);
begin
  // Configuration spécifique selon le type de rapport
  case Index of
    0: // Facture
      begin
        Report.Variables['NumeroFacture'] := QuotedStr('F2023-001');
        // Autres configurations...
      end;
    1: // Bon de livraison
      begin
        Report.Variables['NumeroLivraison'] := QuotedStr('BL2023-001');
        // Autres configurations...
      end;
    2: // Étiquettes
      begin
        // Configurations pour les étiquettes...
      end;
  end;
end;
```

## Architecture d'une solution de rapports évoluée

Pour les projets d'envergure, il est conseillé de créer une architecture modulaire pour vos rapports.

### Création d'un gestionnaire de rapports

```pascal
unit ReportManager;

interface

uses
  System.SysUtils, System.Classes, frxClass, frxDBSet, frxExportPDF,
  frxExportXLS, frxExportDOCX, Data.DB;

type
  TReportType = (rtInvoice, rtDelivery, rtCatalog, rtStatistics);

  TReportFormat = (rfNone, rfPDF, rfExcel, rfWord);

  TReportParameters = class
  public
    DateDebut: TDateTime;
    DateFin: TDateTime;
    Client: string;
    Categorie: string;
    // Autres paramètres...

    constructor Create;
  end;

  TReportManager = class
  private
    FReport: TfrxReport;
    FPDFExport: TfrxPDFExport;
    FXLSExport: TfrxXLSExport;
    FDOCXExport: TfrxDOCXExport;
    FReportPath: string;
    FTemplateDirectory: string;
    FOutputDirectory: string;
    FShowDialog: Boolean;

    procedure ConfigureDataSets(ReportType: TReportType);
    procedure ApplyParameters(Params: TReportParameters);
    procedure ExportToFormat(Format: TReportFormat; const FileName: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowReport(ReportType: TReportType; Params: TReportParameters = nil);
    procedure PrintReport(ReportType: TReportType; Params: TReportParameters = nil);
    function  ExportReport(ReportType: TReportType; Format: TReportFormat;
                          const FileName: string = ''; Params: TReportParameters = nil): string;

    property ReportPath: string read FReportPath write FReportPath;
    property TemplateDirectory: string read FTemplateDirectory write FTemplateDirectory;
    property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
    property ShowDialog: Boolean read FShowDialog write FShowDialog;
  end;

implementation

uses
  DataModule; // Module contenant vos données

constructor TReportParameters.Create;
begin
  inherited;
  DateDebut := Date - 30; // Par défaut: 30 jours avant aujourd'hui
  DateFin := Date;
  Client := '';
  Categorie := '';
end;

constructor TReportManager.Create;
begin
  inherited;

  FReport := TfrxReport.Create(nil);
  FPDFExport := TfrxPDFExport.Create(nil);
  FXLSExport := TfrxXLSExport.Create(nil);
  FDOCXExport := TfrxDOCXExport.Create(nil);

  // Configurer les exports
  FPDFExport.ShowProgress := False;
  FPDFExport.Creator := 'Mon Application Delphi';
  FPDFExport.Subject := 'Rapport généré';

  FXLSExport.ShowProgress := False;

  FDOCXExport.ShowProgress := False;

  // Configurer les chemins
  FReportPath := ExtractFilePath(ParamStr(0));
  FTemplateDirectory := FReportPath + 'Rapports\Modeles\';
  FOutputDirectory := FReportPath + 'Rapports\Sorties\';

  // Options par défaut
  FShowDialog := True;
end;

destructor TReportManager.Destroy;
begin
  FReport.Free;
  FPDFExport.Free;
  FXLSExport.Free;
  FDOCXExport.Free;

  inherited;
end;

procedure TReportManager.ConfigureDataSets(ReportType: TReportType);
var
  frxDBDataset: TfrxDBDataset;
begin
  // Nettoyer les datasets existants
  FReport.DataSets.Clear;

  // Configurer les datasets selon le type de rapport
  case ReportType of
    rtInvoice:
      begin
        // Dataset pour les clients
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblClients;
        frxDBDataset.UserName := 'Clients';
        FReport.DataSets.Add(frxDBDataset);

        // Dataset pour les factures
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblFactures;
        frxDBDataset.UserName := 'Factures';
        FReport.DataSets.Add(frxDBDataset);

        // Dataset pour les détails de factures
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblDetailsFacture;
        frxDBDataset.UserName := 'DetailsFacture';
        FReport.DataSets.Add(frxDBDataset);
      end;

    rtDelivery:
      begin
        // Dataset pour les livraisons
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblLivraisons;
        frxDBDataset.UserName := 'Livraisons';
        FReport.DataSets.Add(frxDBDataset);

        // Autres datasets pour le bon de livraison...
      end;

    rtCatalog:
      begin
        // Dataset pour les produits
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblProduits;
        frxDBDataset.UserName := 'Produits';
        FReport.DataSets.Add(frxDBDataset);

        // Dataset pour les catégories
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.tblCategories;
        frxDBDataset.UserName := 'Categories';
        FReport.DataSets.Add(frxDBDataset);
      end;

    rtStatistics:
      begin
        // Dataset pour les statistiques
        frxDBDataset := TfrxDBDataset.Create(FReport);
        frxDBDataset.DataSet := DataModule1.qryStatistiques;
        frxDBDataset.UserName := 'Statistiques';
        FReport.DataSets.Add(frxDBDataset);
      end;
  end;
end;

procedure TReportManager.ApplyParameters(Params: TReportParameters);
begin
  if not Assigned(Params) then
    Exit;

  // Appliquer les paramètres au rapport
  FReport.Variables['DateDebut'] := QuotedStr(FormatDateTime('dd/mm/yyyy', Params.DateDebut));
  FReport.Variables['DateFin'] := QuotedStr(FormatDateTime('dd/mm/yyyy', Params.DateFin));

  if Params.Client <> '' then
    FReport.Variables['Client'] := QuotedStr(Params.Client);

  if Params.Categorie <> '' then
    FReport.Variables['Categorie'] := QuotedStr(Params.Categorie);

  // Filtrer les datasets en fonction des paramètres
  if (Params.DateDebut > 0) and (Params.DateFin > 0) then
  begin
    with DataModule1 do
    begin
      // Exemple pour les factures
      tblFactures.Filter := Format('DateFacture >= ''%s'' AND DateFacture <= ''%s''',
        [FormatDateTime('yyyy-mm-dd', Params.DateDebut),
         FormatDateTime('yyyy-mm-dd', Params.DateFin)]);
      tblFactures.Filtered := True;

      // Autres filtres selon les besoins...
    end;
  end;

  if Params.Client <> '' then
  begin
    with DataModule1 do
    begin
      // Filtrer par client
      tblFactures.Filter := tblFactures.Filter + ' AND CodeClient = ''' + Params.Client + '''';
      tblFactures.Filtered := True;
    end;
  end;
end;

procedure TReportManager.ExportToFormat(Format: TReportFormat; const FileName: string);
var
  OutputFileName: string;
begin
  if FileName = '' then
  begin
    // Générer un nom de fichier par défaut
    OutputFileName := FOutputDirectory + FormatDateTime('yyyymmdd_hhnnss', Now);

    case Format of
      rfPDF: OutputFileName := OutputFileName + '.pdf';
      rfExcel: OutputFileName := OutputFileName + '.xlsx';
      rfWord: OutputFileName := OutputFileName + '.docx';
    end;
  end
  else
    OutputFileName := FileName;

  // Créer le répertoire de sortie s'il n'existe pas
  if not DirectoryExists(ExtractFilePath(OutputFileName)) then
    ForceDirectories(ExtractFilePath(OutputFileName));

  // Exporter selon le format demandé
  case Format of
    rfPDF:
      begin
        FPDFExport.FileName := OutputFileName;
        FPDFExport.ShowDialog := FShowDialog;
        FReport.Export(FPDFExport);
      end;

    rfExcel:
      begin
        FXLSExport.FileName := OutputFileName;
        FXLSExport.ShowDialog := FShowDialog;
        FReport.Export(FXLSExport);
      end;

    rfWord:
      begin
        FDOCXExport.FileName := OutputFileName;
        FDOCXExport.ShowDialog := FShowDialog;
        FReport.Export(FDOCXExport);
      end;
  end;
end;

procedure TReportManager.ShowReport(ReportType: TReportType; Params: TReportParameters);
var
  TemplateFile: string;
begin
  // Sélectionner le bon template selon le type de rapport
  case ReportType of
    rtInvoice: TemplateFile := FTemplateDirectory + 'Facture.fr3';
    rtDelivery: TemplateFile := FTemplateDirectory + 'BonLivraison.fr3';
    rtCatalog: TemplateFile := FTemplateDirectory + 'Catalogue.fr3';
    rtStatistics: TemplateFile := FTemplateDirectory + 'Statistiques.fr3';
  else
    raise Exception.Create('Type de rapport non reconnu');
  end;

  // Vérifier si le fichier existe
  if not FileExists(TemplateFile) then
    raise Exception.CreateFmt('Modèle de rapport introuvable: %s', [TemplateFile]);

  // Configurer les datasets
  ConfigureDataSets(ReportType);

  // Appliquer les paramètres
  if Assigned(Params) then
    ApplyParameters(Params);

  // Charger et afficher le rapport
  FReport.LoadFromFile(TemplateFile);
  FReport.ShowReport;
end;

procedure TReportManager.PrintReport(ReportType: TReportType; Params: TReportParameters);
var
  TemplateFile: string;
begin
  // Sélectionner le bon template selon le type de rapport
  case ReportType of
    rtInvoice: TemplateFile := FTemplateDirectory + 'Facture.fr3';
    rtDelivery: TemplateFile := FTemplateDirectory + 'BonLivraison.fr3';
    rtCatalog: TemplateFile := FTemplateDirectory + 'Catalogue.fr3';
    rtStatistics: TemplateFile := FTemplateDirectory + 'Statistiques.fr3';
  else
    raise Exception.Create('Type de rapport non reconnu');
  end;

  // Vérifier si le fichier existe
  if not FileExists(TemplateFile) then
    raise Exception.CreateFmt('Modèle de rapport introuvable: %s', [TemplateFile]);

  // Configurer les datasets
  ConfigureDataSets(ReportType);

  // Appliquer les paramètres
  if Assigned(Params) then
    ApplyParameters(Params);

  // Charger, préparer et imprimer le rapport
  FReport.LoadFromFile(TemplateFile);
  FReport.PrepareReport;
  FReport.Print;
end;

function TReportManager.ExportReport(ReportType: TReportType; Format: TReportFormat;
                                    const FileName: string; Params: TReportParameters): string;
var
  TemplateFile, OutputFileName: string;
begin
  // Sélectionner le bon template selon le type de rapport
  case ReportType of
    rtInvoice: TemplateFile := FTemplateDirectory + 'Facture.fr3';
    rtDelivery: TemplateFile := FTemplateDirectory + 'BonLivraison.fr3';
    rtCatalog: TemplateFile := FTemplateDirectory + 'Catalogue.fr3';
    rtStatistics: TemplateFile := FTemplateDirectory + 'Statistiques.fr3';
  else
    raise Exception.Create('Type de rapport non reconnu');
  end;

  // Vérifier si le fichier existe
  if not FileExists(TemplateFile) then
    raise Exception.CreateFmt('Modèle de rapport introuvable: %s', [TemplateFile]);

  // Configurer les datasets
  ConfigureDataSets(ReportType);

  // Appliquer les paramètres
  if Assigned(Params) then
    ApplyParameters(Params);

  // Déterminer le nom du fichier de sortie
  if FileName = '' then
  begin
    // Générer un nom de fichier par défaut
    OutputFileName := FOutputDirectory;

    case ReportType of
      rtInvoice: OutputFileName := OutputFileName + 'Facture_';
      rtDelivery: OutputFileName := OutputFileName + 'BonLivraison_';
      rtCatalog: OutputFileName := OutputFileName + 'Catalogue_';
      rtStatistics: OutputFileName := OutputFileName + 'Statistiques_';
    end;

    OutputFileName := OutputFileName + FormatDateTime('yyyymmdd_hhnnss', Now);

    case Format of
      rfPDF: OutputFileName := OutputFileName + '.pdf';
      rfExcel: OutputFileName := OutputFileName + '.xlsx';
      rfWord: OutputFileName := OutputFileName + '.docx';
      else OutputFileName := '';
    end;
  end
  else
    OutputFileName := FileName;

  // Charger et préparer le rapport
  FReport.LoadFromFile(TemplateFile);
  FReport.PrepareReport;

  // Exporter si un format est spécifié
  if (Format <> rfNone) and (OutputFileName <> '') then
    ExportToFormat(Format, OutputFileName);

  // Retourner le nom du fichier généré
  Result := OutputFileName;
end;

end.
```

### Utilisation du gestionnaire de rapports

```pascal
procedure TMainForm.GenerateInvoiceReport;
var
  ReportMgr: TReportManager;
  Params: TReportParameters;
begin
  ReportMgr := TReportManager.Create;
  try
    // Configurer le gestionnaire
    ReportMgr.TemplateDirectory := ExtractFilePath(Application.ExeName) + 'Rapports\Modeles\';
    ReportMgr.OutputDirectory := ExtractFilePath(Application.ExeName) + 'Rapports\Sorties\';
    ReportMgr.ShowDialog := True;

    // Créer et configurer les paramètres
    Params := TReportParameters.Create;
    try
      Params.DateDebut := DateTimePicker1.Date;
      Params.DateFin := DateTimePicker2.Date;
      Params.Client := edtClient.Text;

      // Afficher le rapport
      ReportMgr.ShowReport(rtInvoice, Params);
    finally
      Params.Free;
    end;
  finally
    ReportMgr.Free;
  end;
end;

procedure TMainForm.ExportCatalogToPDF;
var
  ReportMgr: TReportManager;
  Params: TReportParameters;
  SaveDialog: TSaveDialog;
  FileName: string;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Enregistrer le catalogue en PDF';
    SaveDialog.DefaultExt := 'pdf';
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';
    SaveDialog.InitialDir := ExtractFilePath(Application.ExeName) + 'Rapports\Sorties\';

    if SaveDialog.Execute then
    begin
      FileName := SaveDialog.FileName;

      ReportMgr := TReportManager.Create;
      try
        Params := TReportParameters.Create;
        try
          Params.Categorie := cbCategorie.Text;

          // Exporter en PDF sans afficher de dialogue
          ReportMgr.ShowDialog := False;
          FileName := ReportMgr.ExportReport(rtCatalog, rfPDF, FileName, Params);

          // Afficher un message de confirmation
          if FileExists(FileName) then
          begin
            if MessageDlg('Le PDF a été généré avec succès. Voulez-vous l''ouvrir?',
                          mtConfirmation, [mbYes, mbNo], 0) = mrYes then
            begin
              ShellExecute(0, 'open', PChar(FileName), nil, nil, SW_SHOWNORMAL);
            end;
          end;
        finally
          Params.Free;
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

## Bonnes pratiques et optimisation

Pour créer des rapports complexes efficaces et maintenables, voici quelques bonnes pratiques :

### 1. Organisation des rapports

```
MonApplication/
  |- Rapports/
      |- Modeles/
          |- Commun/
              |- Entetes.fr3
              |- PiedPage.fr3
          |- Factures/
              |- FactureStandard.fr3
              |- FactureDetaillée.fr3
          |- Livraisons/
              |- BonLivraison.fr3
      |- Sorties/
          |- (fichiers exportés)
      |- Images/
          |- Logos/
          |- Produits/
```

### 2. Optimisation des performances

- **Limiter les données** : Filtrez vos données avant de générer le rapport
- **Réutiliser les objets** : Créez et configurez les rapports une seule fois
- **Compression des images** : Optimisez les images pour réduire la taille des fichiers
- **Cache des pages** : Activez l'option de cache pour les grands rapports

```pascal
// Activer le cache pour les grands rapports
frxReport1.ReportOptions.UseFileCache := True;
frxReport1.ReportOptions.MaxMemSize := 10; // MB
```

### 3. Tests et débogage

```pascal
// Activer le mode debug
frxReport1.ReportOptions.ShowProgress := True;
frxReport1.ReportOptions.ShowPreparedReport := True;

// Ajouter des messages de débogage dans les scripts
'procedure OnBeforePrint(Sender: TfrxComponent);' + #13#10 +
'begin' + #13#10 +
'  DebugMessage(''Variable X = '' + IntToStr(X));' + #13#10 +
'end;';
```

### 4. Sécurité des données

- **Filtrage des données sensibles** : Ne montrez que les données nécessaires
- **Protection des PDF** : Ajoutez des mots de passe aux documents sensibles
- **Limitations de droits** : Contrôlez qui peut accéder à quels rapports

```pascal
// Protéger un PDF exporté
FPDFExport.OwnerPassword := 'MotDePasseAdmin';
FPDFExport.UserPassword := 'MotDePasseUtilisateur';
FPDFExport.Permissions := [ppPrint, ppModify]; // Autorisations
```

### 5. Standardisation

- **Définir des styles communs** : Créez des styles cohérents pour tous vos rapports
- **Utiliser des sous-rapports partagés** : Créez des éléments réutilisables
- **Documentation** : Documentez vos rapports et leurs paramètres

## Conclusion

La création de rapports complexes avec FastReport (ou QuickReport) offre de nombreuses possibilités pour présenter vos données de manière professionnelle et interactive. En combinant les différentes fonctionnalités comme les groupes, les sous-rapports, les tableaux croisés, les graphiques et les scripts, vous pouvez créer des rapports sophistiqués qui répondent précisément à vos besoins métier.

Pour une gestion efficace des rapports dans une application d'entreprise, il est recommandé de mettre en place une architecture modulaire avec un gestionnaire de rapports centralisé, comme celui présenté dans cette section. Cette approche facilite la maintenance, l'évolution et l'utilisation des rapports tout au long du cycle de vie de votre application.

Dans la prochaine section, nous explorerons comment créer des rapports interactifs avec des fonctionnalités avancées comme les tableaux de bord, les rapports à la demande et les analyses ad hoc.
