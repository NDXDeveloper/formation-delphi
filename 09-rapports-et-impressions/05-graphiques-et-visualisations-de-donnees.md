# 9.5 Graphiques et visualisations de données

## Introduction

La visualisation des données est un aspect essentiel des applications professionnelles. Présenter des informations sous forme de graphiques, diagrammes ou tableaux de bord permet aux utilisateurs de comprendre rapidement des tendances, des comparaisons ou des répartitions qui seraient difficiles à percevoir dans des données brutes.

Dans cette section, nous allons explorer comment intégrer différents types de graphiques et visualisations dans vos applications Delphi, à la fois dans vos rapports et directement dans vos interfaces utilisateur.

## Graphiques natifs dans les rapports FastReport

### Types de graphiques disponibles

FastReport intègre un composant de graphique puissant qui prend en charge de nombreux formats de visualisation :

- **Histogrammes** : Comparer des valeurs entre différentes catégories
- **Courbes** : Montrer l'évolution de données au fil du temps
- **Secteurs (camemberts)** : Illustrer des proportions d'un tout
- **Aires** : Similaire aux courbes, mais avec une zone colorée sous la ligne
- **Nuages de points** : Visualiser des corrélations entre deux variables
- **Graphiques en radar** : Comparer plusieurs variables quantitatives
- **Jauges** : Afficher une mesure par rapport à une échelle
- **Pyramides** : Montrer des proportions hiérarchiques

### Création d'un graphique simple

Voici comment ajouter un graphique à votre rapport FastReport :

1. Dans le designer FastReport, sélectionnez l'outil "Chart" (Graphique) dans la barre d'outils
2. Dessinez le graphique sur votre rapport
3. Double-cliquez sur le graphique pour ouvrir l'éditeur de graphique
4. Configurez le type de graphique et les séries de données

Par code, voici comment créer un graphique dans un rapport :

```pascal
procedure TForm1.CreateChartReport;
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  Band: TfrxBand;
  Chart: TfrxChartView;
begin
  Report := TfrxReport.Create(Self);

  // Créer une page
  Page := TfrxReportPage.Create(Report);
  Report.Pages.Add(Page);

  // Ajouter une bande
  Band := TfrxReportTitle.Create(Report);
  Page.Bands.Add(Band);
  Band.Height := 300;

  // Ajouter un graphique
  Chart := TfrxChartView.Create(Report);
  Band.Objects.Add(Chart);
  Chart.Left := 20;
  Chart.Top := 20;
  Chart.Width := 600;
  Chart.Height := 250;

  // Configurer le graphique
  Chart.Chart.Title.Text.Add('Ventes mensuelles 2023');
  Chart.Chart.SeriesType := stBar; // Histogramme

  // Configurer la série de données
  with Chart.Chart.Series.Add do
  begin
    ColorEachPoint := True; // Couleur différente pour chaque barre
    XSource := 'frxDBDataset1."Mois"'; // Axe X (mois)
    YSource := 'frxDBDataset1."Montant"'; // Axe Y (ventes)
    Name := 'Ventes';
  end;

  // Prévisualiser le rapport
  Report.ShowReport;
end;
```

### Graphique comparatif multi-séries

Pour comparer plusieurs ensembles de données, vous pouvez ajouter plusieurs séries à votre graphique :

```pascal
// Ajouter des séries multiples
with Chart.Chart.Series.Add do
begin
  ColorEachPoint := False;
  Color := clRed;
  XSource := 'frxDBDataset1."Mois"';
  YSource := 'frxDBDataset1."Montant"';
  Name := 'Ventes 2023';
end;

with Chart.Chart.Series.Add do
begin
  ColorEachPoint := False;
  Color := clBlue;
  XSource := 'frxDBDataset2."Mois"';
  YSource := 'frxDBDataset2."Montant"';
  Name := 'Ventes 2022';
end;
```

### Personnalisation des graphiques

Vous pouvez personnaliser de nombreux aspects de vos graphiques :

```pascal
// Style du graphique
Chart.Chart.View3D := True; // Graphique en 3D
Chart.Chart.BackWall.Color := clCream; // Couleur de fond
Chart.Chart.Gradient.Visible := True; // Dégradé de couleur

// Légende
Chart.Chart.Legend.Visible := True;
Chart.Chart.Legend.Alignment := laBottom; // Position en bas

// Titre et police
Chart.Chart.Title.Font.Size := 14;
Chart.Chart.Title.Font.Style := [fsBold];
Chart.Chart.Title.Alignment := taCenter;

// Axes
Chart.Chart.BottomAxis.Title.Caption := 'Mois';
Chart.Chart.LeftAxis.Title.Caption := 'Ventes (€)';
Chart.Chart.BottomAxis.Grid.Visible := True;
```

### Graphique avec données calculées

Vous pouvez combiner les fonctions d'agrégation de FastReport avec les graphiques :

```pascal
// Pour créer un graphique qui montre les ventes totales par catégorie
with Chart.Chart.Series.Add do
begin
  ColorEachPoint := True;
  XSource := 'frxDBDataset1."Categorie"';
  YSource := 'SUM(frxDBDataset1."Montant")';
  Name := 'Ventes par catégorie';
end;
```

## Composants TeeChart dans Delphi

TeeChart est une bibliothèque de graphiques puissante incluse dans certaines éditions de Delphi. Elle offre des fonctionnalités avancées pour les visualisations interactives directement dans vos formulaires.

### Ajout d'un graphique TeeChart à un formulaire

1. Placez un composant `TChart` sur votre formulaire depuis la palette "TeeChart"
2. Utilisez l'éditeur de graphique en double-cliquant sur le composant
3. Ajoutez des séries de données et configurez l'apparence

Voici comment ajouter un graphique par code :

```pascal
procedure TForm1.CreateTeeChart;
var
  Chart: TChart;
  Series: TBarSeries;
begin
  // Créer le composant Chart
  Chart := TChart.Create(Self);
  Chart.Parent := Self;
  Chart.Left := 20;
  Chart.Top := 20;
  Chart.Width := 500;
  Chart.Height := 300;

  // Configurer le titre
  Chart.Title.Text.Add('Ventes mensuelles');
  Chart.Title.Font.Size := 12;
  Chart.Title.Font.Style := [fsBold];

  // Ajouter une série d'histogramme
  Series := TBarSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.Title := 'Ventes 2023';

  // Ajouter des données
  Series.Add(1000, 'Jan', clRed);
  Series.Add(1200, 'Fév', clRed);
  Series.Add(900, 'Mar', clRed);
  Series.Add(1500, 'Avr', clRed);
  Series.Add(1800, 'Mai', clRed);
  Series.Add(2200, 'Juin', clRed);

  // Personnaliser les axes
  Chart.BottomAxis.Title.Caption := 'Mois';
  Chart.LeftAxis.Title.Caption := 'Montant (€)';

  // Activer les fonctionnalités interactives
  Chart.AllowPanning := pmNone;
  Chart.AllowZoom := False;

  // Personnaliser l'apparence
  Chart.View3D := True;
  Chart.View3DOptions.Elevation := 315;
  Chart.View3DOptions.Perspective := 100;
  Chart.View3DOptions.Rotation := 360;
end;
```

### Remplir un graphique avec des données depuis une base de données

Pour les applications réelles, vous voudrez remplir vos graphiques avec des données provenant d'une base de données :

```pascal
procedure TForm1.LoadChartFromDatabase;
var
  Series: TBarSeries;
begin
  // Accéder à une série existante
  Series := Chart1.Series[0] as TBarSeries;
  Series.Clear; // Effacer les données existantes

  // Exécuter une requête
  DataModule1.qryVentesMensuelles.Close;
  DataModule1.qryVentesMensuelles.Open;

  // Parcourir les enregistrements et ajouter au graphique
  DataModule1.qryVentesMensuelles.First;
  while not DataModule1.qryVentesMensuelles.Eof do
  begin
    Series.Add(
      DataModule1.qryVentesMensuelles.FieldByName('Montant').AsFloat,
      DataModule1.qryVentesMensuelles.FieldByName('Mois').AsString,
      clRed
    );
    DataModule1.qryVentesMensuelles.Next;
  end;
end;
```

### Types de séries disponibles dans TeeChart

TeeChart propose de nombreux types de visualisations :

- `TLineSeries` : Graphique en ligne (courbe)
- `TBarSeries` : Histogramme (barres verticales)
- `THorizBarSeries` : Histogramme horizontal
- `TAreaSeries` : Graphique en aire
- `TPieSeries` : Graphique en secteurs (camembert)
- `TPointSeries` : Nuage de points
- `TFastLineSeries` : Courbes optimisées pour les grands volumes de données
- `TBubbleSeries` : Nuage de bulles (trois dimensions de données)
- `TRadarSeries` : Graphique en radar

Exemple d'utilisation d'un camembert :

```pascal
procedure TForm1.CreatePieChart;
var
  Series: TPieSeries;
begin
  Series := TPieSeries.Create(Chart1);
  Chart1.AddSeries(Series);

  // Configurer le camembert
  Series.Marks.Style := smsLabelPercent; // Afficher libellé et pourcentage
  Series.ExplodeBiggest := 20; // Détacher la plus grande part

  // Ajouter des données
  Series.Add(35, 'Hardware', clRed);
  Series.Add(45, 'Software', clGreen);
  Series.Add(15, 'Services', clBlue);
  Series.Add(5, 'Formation', clYellow);

  // Rotation du camembert
  Series.RotationAngle := 90;
end;
```

### Graphiques interactifs avec TeeChart

TeeChart permet de créer des graphiques interactifs que vos utilisateurs peuvent explorer :

```pascal
procedure TForm1.ConfigureInteractiveChart;
begin
  // Activer le zoom
  Chart1.AllowZoom := True;

  // Activer le déplacement
  Chart1.AllowPanning := pmBoth; // Déplacement dans les deux axes

  // Permettre la rotation 3D avec la souris
  Chart1.View3D := True;
  Chart1.View3DOptions.Orthogonal := False;
  Chart1.View3DOptions.Perspective := 15;
  Chart1.View3DOptions.Zoom := 90;

  // Ajouter un menu contextuel
  Chart1.PopupMenu := PopupMenu1;

  // Afficher des informations au survol
  Chart1.ShowHint := True;
end;
```

Pour gérer les événements de clic sur un graphique :

```pascal
procedure TForm1.Chart1ClickSeries(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Afficher les détails de l'élément cliqué
  ShowMessage(Format('Vous avez cliqué sur %s: %g',
    [Series.XLabel[ValueIndex], Series.YValue[ValueIndex]]));

  // Vous pouvez ici ouvrir un autre formulaire, filtrer des données, etc.
end;
```

## Créer des tableaux de bord avec plusieurs graphiques

Les tableaux de bord combinent plusieurs visualisations pour offrir une vue complète des données.

### Organisation des graphiques sur un formulaire

```pascal
procedure TForm1.CreateDashboard;
var
  PanelVentes, PanelClients, PanelProduits, PanelGeographie: TPanel;
  ChartVentes, ChartClients, ChartProduits, ChartGeographie: TChart;
begin
  // Créer des panneaux pour organiser les graphiques
  PanelVentes := TPanel.Create(Self);
  PanelVentes.Parent := Self;
  PanelVentes.Left := 10;
  PanelVentes.Top := 10;
  PanelVentes.Width := 400;
  PanelVentes.Height := 300;
  PanelVentes.Caption := '';

  PanelClients := TPanel.Create(Self);
  PanelClients.Parent := Self;
  PanelClients.Left := 420;
  PanelClients.Top := 10;
  PanelClients.Width := 400;
  PanelClients.Height := 300;
  PanelClients.Caption := '';

  PanelProduits := TPanel.Create(Self);
  PanelProduits.Parent := Self;
  PanelProduits.Left := 10;
  PanelProduits.Top := 320;
  PanelProduits.Width := 400;
  PanelProduits.Height := 300;
  PanelProduits.Caption := '';

  PanelGeographie := TPanel.Create(Self);
  PanelGeographie.Parent := Self;
  PanelGeographie.Left := 420;
  PanelGeographie.Top := 320;
  PanelGeographie.Width := 400;
  PanelGeographie.Height := 300;
  PanelGeographie.Caption := '';

  // Créer les graphiques dans chaque panneau
  ChartVentes := TChart.Create(Self);
  ChartVentes.Parent := PanelVentes;
  ChartVentes.Align := alClient;
  ChartVentes.Title.Text.Add('Ventes mensuelles');

  ChartClients := TChart.Create(Self);
  ChartClients.Parent := PanelClients;
  ChartClients.Align := alClient;
  ChartClients.Title.Text.Add('Répartition des clients');

  ChartProduits := TChart.Create(Self);
  ChartProduits.Parent := PanelProduits;
  ChartProduits.Align := alClient;
  ChartProduits.Title.Text.Add('Top 5 des produits');

  ChartGeographie := TChart.Create(Self);
  ChartGeographie.Parent := PanelGeographie;
  ChartGeographie.Align := alClient;
  ChartGeographie.Title.Text.Add('Ventes par région');

  // Ajouter des séries et des données à chaque graphique
  // (Code similaire aux exemples précédents)
end;
```

### Interaction entre les graphiques

Un tableau de bord efficace permet aux utilisateurs d'interagir avec les données et de voir les relations entre différentes visualisations :

```pascal
procedure TForm1.ChartVentesClickSeries(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MoisSelectionne: string;
begin
  // Récupérer le mois sélectionné
  MoisSelectionne := Series.XLabel[ValueIndex];

  // Mettre à jour les autres graphiques pour ce mois
  UpdateClientChartForMonth(MoisSelectionne);
  UpdateProductChartForMonth(MoisSelectionne);
  UpdateGeographyChartForMonth(MoisSelectionne);

  // Mettre à jour le titre principal
  LabelPeriode.Caption := 'Période: ' + MoisSelectionne + ' 2023';
end;

procedure TForm1.UpdateClientChartForMonth(const Month: string);
begin
  // Exécuter une requête filtrée par mois
  DataModule1.qryClientsParMois.Close;
  DataModule1.qryClientsParMois.Parameters.ParamByName('Mois').Value := Month;
  DataModule1.qryClientsParMois.Open;

  // Mettre à jour le graphique des clients
  // ...
end;
```

## Visualisations avancées avec des bibliothèques tierces

### Intégration de bibliothèques JavaScript pour des visualisations web

Pour des visualisations plus modernes et interactives, vous pouvez intégrer des bibliothèques JavaScript comme Chart.js ou D3.js via un composant `TWebBrowser` :

```pascal
procedure TForm1.CreateHTMLChart;
var
  HTMLContent: TStringList;
  TempFile: string;
begin
  HTMLContent := TStringList.Create;
  try
    HTMLContent.Add('<!DOCTYPE html>');
    HTMLContent.Add('<html>');
    HTMLContent.Add('<head>');
    HTMLContent.Add('<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>');
    HTMLContent.Add('<style>body { margin: 0; }</style>');
    HTMLContent.Add('</head>');
    HTMLContent.Add('<body>');
    HTMLContent.Add('<canvas id="myChart" width="100%" height="100%"></canvas>');
    HTMLContent.Add('<script>');
    HTMLContent.Add('const ctx = document.getElementById("myChart");');
    HTMLContent.Add('new Chart(ctx, {');
    HTMLContent.Add('  type: "bar",');
    HTMLContent.Add('  data: {');
    HTMLContent.Add('    labels: ["Jan", "Fév", "Mar", "Avr", "Mai", "Juin"],');
    HTMLContent.Add('    datasets: [{');
    HTMLContent.Add('      label: "Ventes 2023",');
    HTMLContent.Add('      data: [1000, 1200, 900, 1500, 1800, 2200],');
    HTMLContent.Add('      backgroundColor: ["rgba(255, 99, 132, 0.6)", "rgba(54, 162, 235, 0.6)",');
    HTMLContent.Add('                       "rgba(255, 206, 86, 0.6)", "rgba(75, 192, 192, 0.6)",');
    HTMLContent.Add('                       "rgba(153, 102, 255, 0.6)", "rgba(255, 159, 64, 0.6)"]');
    HTMLContent.Add('    }]');
    HTMLContent.Add('  },');
    HTMLContent.Add('  options: {');
    HTMLContent.Add('    responsive: true,');
    HTMLContent.Add('    plugins: {');
    HTMLContent.Add('      title: {');
    HTMLContent.Add('        display: true,');
    HTMLContent.Add('        text: "Ventes mensuelles 2023"');
    HTMLContent.Add('      }');
    HTMLContent.Add('    }');
    HTMLContent.Add('  }');
    HTMLContent.Add('});');
    HTMLContent.Add('</script>');
    HTMLContent.Add('</body>');
    HTMLContent.Add('</html>');

    // Enregistrer dans un fichier temporaire
    TempFile := ExtractFilePath(Application.ExeName) + 'chart_temp.html';
    HTMLContent.SaveToFile(TempFile);

    // Charger dans le navigateur web
    WebBrowser1.Navigate('file://' + TempFile);
  finally
    HTMLContent.Free;
  end;
end;
```

### Autres bibliothèques de visualisation

Il existe plusieurs bibliothèques tierces pour Delphi qui offrent des capacités de visualisation avancées :

- **VCL Styles Charts** : graphiques modernes compatibles avec les styles VCL
- **SVG Components** : dessiner des graphiques vectoriels SVG
- **Steema TeeChart Pro** : version avancée de TeeChart avec plus de types de graphiques
- **DXChart** : composant de DevExpress pour les visualisations interactives
- **GDI+ Charts** : graphiques utilisant l'API GDI+ de Windows

## Conseils pour des visualisations efficaces

### Choix du bon type de graphique

Le choix du type de graphique dépend de ce que vous voulez montrer :

- **Histogramme** : Comparaison entre catégories
- **Ligne/Courbe** : Tendance ou évolution dans le temps
- **Camembert** : Proportions d'un tout (moins de 7 segments pour être lisible)
- **Radar** : Comparaison multi-paramètres
- **Nuage de points** : Corrélations entre deux variables
- **Carte de chaleur** : Distribution de densité
- **Jauge** : Progression vers un objectif

### Bonnes pratiques en visualisation de données

Pour créer des graphiques clairs et informatifs :

1. **Simplicité** : Ne surchargez pas vos graphiques
2. **Titres clairs** : Indiquez précisément ce que montre le graphique
3. **Échelles appropriées** : Utilisez des échelles qui ne déforment pas la perception
4. **Légendes** : Expliquez clairement ce que représente chaque série
5. **Couleurs** : Utilisez des couleurs contrastées mais harmonieuses
6. **Accessibilité** : Pensez aux utilisateurs daltoniens (évitez rouge-vert ensemble)
7. **Interactivité** : Permettez aux utilisateurs d'explorer les données
8. **Cohérence** : Utilisez les mêmes styles et couleurs pour les mêmes types de données

### Exemple de tableau de bord bien conçu

```pascal
procedure TForm1.CreateProfessionalDashboard;
begin
  // Titre principal avec période
  LabelTitre := TLabel.Create(Self);
  LabelTitre.Parent := Self;
  LabelTitre.Left := 10;
  LabelTitre.Top := 10;
  LabelTitre.Font.Size := 16;
  LabelTitre.Font.Style := [fsBold];
  LabelTitre.Caption := 'Tableau de bord des ventes';

  // Panneau de filtres
  PanelFiltres := TPanel.Create(Self);
  PanelFiltres.Parent := Self;
  PanelFiltres.Left := 10;
  PanelFiltres.Top := 40;
  PanelFiltres.Width := ClientWidth - 20;
  PanelFiltres.Height := 50;

  // Ajout des contrôles de filtre
  DateDebut := TDateTimePicker.Create(Self);
  DateDebut.Parent := PanelFiltres;
  DateDebut.Left := 100;
  DateDebut.Top := 15;

  DateFin := TDateTimePicker.Create(Self);
  DateFin.Parent := PanelFiltres;
  DateFin.Left := 250;
  DateFin.Top := 15;

  BtnAppliquer := TButton.Create(Self);
  BtnAppliquer.Parent := PanelFiltres;
  BtnAppliquer.Left := 400;
  BtnAppliquer.Top := 13;
  BtnAppliquer.Caption := 'Appliquer';
  BtnAppliquer.OnClick := BtnAppliquerClick;

  // Création du tableau de bord
  CreateDashboardPanels;
  LoadDashboardData;
end;

procedure TForm1.CreateDashboardPanels;
begin
  // Création des panneaux et graphiques
  // (similar to previous example)
end;

procedure TForm1.LoadDashboardData;
begin
  // Chargement des données dans les graphiques
  // depuis la base de données
end;

procedure TForm1.BtnAppliquerClick(Sender: TObject);
begin
  // Mise à jour des graphiques selon la période sélectionnée
  LoadDashboardData;
end;
```

## Conclusion

Les graphiques et visualisations de données transforment les chiffres bruts en informations compréhensibles et actionnables. Dans Delphi, vous disposez de plusieurs options pour intégrer des visualisations dans vos applications :

- FastReport pour les graphiques dans vos rapports
- TeeChart pour les graphiques intégrés à votre interface utilisateur
- Bibliothèques tierces pour des visualisations avancées
- Intégration web pour les visualisations JavaScript modernes

En suivant les bonnes pratiques de visualisation et en choisissant les bons types de graphiques pour vos données, vous pouvez créer des tableaux de bord informatifs qui aideront vos utilisateurs à prendre de meilleures décisions.

Dans la section suivante, nous verrons comment exporter vos rapports et visualisations vers différents formats comme PDF, Excel ou HTML.
