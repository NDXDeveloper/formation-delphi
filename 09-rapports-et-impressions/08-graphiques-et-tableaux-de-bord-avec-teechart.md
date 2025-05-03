# 9.8 Graphiques et tableaux de bord avec TeeChart

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction à TeeChart

TeeChart est une bibliothèque puissante de composants graphiques pour Delphi qui permet de créer des visualisations de données avancées. Que vous ayez besoin de graphiques simples ou de tableaux de bord complexes, TeeChart offre tous les outils nécessaires pour transformer vos données brutes en informations visuelles compréhensibles.

Dans cette section, nous allons explorer comment utiliser TeeChart pour créer des graphiques professionnels et des tableaux de bord interactifs dans vos applications Delphi.

## Installation et configuration de TeeChart

### Versions de TeeChart

TeeChart est disponible en plusieurs versions :

1. **TeeChart Standard** : Version de base incluse dans certaines éditions de Delphi
2. **TeeChart Pro** : Version professionnelle avec plus de fonctionnalités (vendue séparément)
3. **TeeChart Lite** : Version gratuite avec des fonctionnalités limitées

### Vérification de la disponibilité de TeeChart

Pour vérifier si TeeChart est disponible dans votre installation de Delphi :

1. Lancez Delphi et créez un nouveau projet VCL
2. Ouvrez l'onglet "Palette de composants"
3. Recherchez la catégorie "TeeChart" ou "Charting"

Si vous ne trouvez pas ces composants, vous devrez peut-être installer TeeChart séparément.

### Installation des composants TeeChart

Si TeeChart n'est pas installé dans votre Delphi :

1. Téléchargez TeeChart depuis le site officiel (version appropriée)
2. Suivez les instructions d'installation fournies
3. Redémarrez Delphi
4. Vérifiez que les composants TeeChart sont maintenant visibles dans la palette

## Les composants de base de TeeChart

### TChart : le composant principal

Le composant principal de TeeChart est `TChart`. C'est le conteneur qui hébergera vos graphiques.

Pour ajouter un graphique à votre formulaire :

1. Placez un composant `TChart` sur votre formulaire depuis la palette
2. Redimensionnez-le selon vos besoins
3. Double-cliquez sur le composant pour ouvrir l'éditeur de graphique

### L'éditeur de graphique TeeChart

L'éditeur de graphique est un outil puissant qui vous permet de configurer tous les aspects de votre graphique sans écrire de code :

![Éditeur TeeChart](/images/teechart-editor.png)

Les principales fonctionnalités de l'éditeur sont :

- **Ajout/suppression de séries** : Différents types de graphiques
- **Configuration des axes** : Titres, échelles, graduations
- **Personnalisation des légendes** : Position, style, police
- **Configuration des titres** : Texte, police, alignement
- **Options diverses** : 3D, zoom, panoramique, etc.

## Création d'un graphique simple

Voici comment créer un graphique simple à l'aide de TeeChart :

```pascal
procedure TForm1.CreateSimpleChart;
var
  Chart: TChart;
  Series: TBarSeries;
  i: Integer;
begin
  // Créer le composant Chart dynamiquement
  Chart := TChart.Create(Self);
  Chart.Parent := Self;
  Chart.Left := 20;
  Chart.Top := 20;
  Chart.Width := 500;
  Chart.Height := 300;

  // Configurer le titre
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add('Ventes par mois');
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

  // Activer/désactiver certaines fonctionnalités
  Chart.View3D := True;  // Graphique en 3D
  Chart.Legend.Visible := True;  // Afficher la légende
end;
```

Ou plus simplement, en utilisant un composant placé sur le formulaire depuis la palette :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurer le titre
  Chart1.Title.Text.Clear;
  Chart1.Title.Text.Add('Ventes par mois');

  // Ajouter des données à une série existante
  if Chart1.SeriesCount > 0 then
  begin
    (Chart1.Series[0] as TBarSeries).Clear;  // Effacer les données existantes

    // Ajouter des données
    Chart1.Series[0].Add(1000, 'Jan');
    Chart1.Series[0].Add(1200, 'Fév');
    Chart1.Series[0].Add(900, 'Mar');
    Chart1.Series[0].Add(1500, 'Avr');
    Chart1.Series[0].Add(1800, 'Mai');
    Chart1.Series[0].Add(2200, 'Juin');
  end;
end;
```

## Types de graphiques disponibles

TeeChart propose une grande variété de types de graphiques. Voici les plus courants :

### Graphiques standard

1. **TLineSeries** - Graphique en ligne (courbe)
   ```pascal
   Series := TLineSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

2. **TBarSeries** - Histogramme vertical
   ```pascal
   Series := TBarSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

3. **THorizBarSeries** - Histogramme horizontal
   ```pascal
   Series := THorizBarSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

4. **TPieSeries** - Graphique en secteurs (camembert)
   ```pascal
   Series := TPieSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

5. **TAreaSeries** - Graphique en aire
   ```pascal
   Series := TAreaSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

### Graphiques avancés

1. **TBubbleSeries** - Graphique à bulles (3 dimensions de données)
   ```pascal
   Series := TBubbleSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   // Ajout de données : X, Y et taille de la bulle
   Series.AddBubble(10, 20, 5, 'Point 1');
   ```

2. **TCandleSeries** - Graphique en chandelier (boursier)
   ```pascal
   Series := TCandleSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   // Ajout de données : X, Open, High, Low, Close
   Series.AddCandle(0, 10, 15, 8, 12, 'Jour 1');
   ```

3. **TFastLineSeries** - Ligne optimisée pour les grandes quantités de données
   ```pascal
   Series := TFastLineSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

4. **TPolarSeries** - Graphique polaire
   ```pascal
   Series := TPolarSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

5. **TRadarSeries** - Graphique en radar
   ```pascal
   Series := TRadarSeries.Create(Chart1);
   Chart1.AddSeries(Series);
   ```

## Personnalisation des graphiques

### Personnalisation des couleurs et du style

```pascal
procedure TForm1.CustomizeChartAppearance;
begin
  // Couleurs des séries
  Chart1.Series[0].Color := clRed;

  // Utiliser des couleurs différentes pour chaque point
  if Chart1.Series[0] is TBarSeries then
    TBarSeries(Chart1.Series[0]).ColorEachPoint := True;

  // Personnaliser les couleurs de fond
  Chart1.Color := clWhite;  // Couleur de fond du graphique
  Chart1.BackWall.Color := clCream;  // Couleur du mur arrière (en 3D)

  // Ajouter un dégradé de couleur
  Chart1.Gradient.Visible := True;
  Chart1.Gradient.StartColor := clWhite;
  Chart1.Gradient.EndColor := clSilver;
  Chart1.Gradient.Direction := gdBottomTop;

  // Personnaliser les bordures
  Chart1.BorderStyle := bsSingle;
  Chart1.BevelInner := bvRaised;
  Chart1.BevelOuter := bvLowered;

  // Style 3D
  Chart1.View3D := True;
  Chart1.View3DOptions.Elevation := 315;  // Angle d'élévation
  Chart1.View3DOptions.Perspective := 50;  // Perspective (0-100)
  Chart1.View3DOptions.Rotation := 360;  // Rotation horizontale
  Chart1.View3DOptions.Orthogonal := False;  // True pour une vue orthogonale
end;
```

### Personnalisation des axes

```pascal
procedure TForm1.CustomizeChartAxes;
begin
  // Titres des axes
  Chart1.BottomAxis.Title.Caption := 'Catégories';
  Chart1.LeftAxis.Title.Caption := 'Valeurs (€)';

  // Styles des titres
  Chart1.BottomAxis.Title.Font.Style := [fsBold];
  Chart1.LeftAxis.Title.Font.Color := clBlue;

  // Grilles
  Chart1.BottomAxis.Grid.Visible := True;
  Chart1.LeftAxis.Grid.Visible := True;
  Chart1.BottomAxis.Grid.Style := psDot;  // Style pointillé

  // Échelle automatique ou personnalisée
  Chart1.LeftAxis.Automatic := False;  // Désactiver l'échelle automatique
  Chart1.LeftAxis.Minimum := 0;
  Chart1.LeftAxis.Maximum := 3000;
  Chart1.LeftAxis.Increment := 500;  // Incrément entre les graduations

  // Format des graduations
  Chart1.LeftAxis.LabelsFormat.Precision := 0;  // Nombre de décimales
  Chart1.LeftAxis.LabelsFormat.Thousands := True;  // Séparateur de milliers

  // Rotation des libellés
  Chart1.BottomAxis.LabelsAngle := 45;  // Rotation de 45 degrés
end;
```

### Personnalisation des légendes et étiquettes

```pascal
procedure TForm1.CustomizeChartLegend;
begin
  // Configuration de la légende
  Chart1.Legend.Visible := True;
  Chart1.Legend.Alignment := laBottom;  // En bas du graphique
  Chart1.Legend.Transparent := False;
  Chart1.Legend.Color := clInfoBk;
  Chart1.Legend.Font.Style := [fsBold];
  Chart1.Legend.Frame.Visible := True;

  // Étiquettes des points (Marks)
  if Chart1.Series[0] is TBarSeries then
  begin
    TBarSeries(Chart1.Series[0]).Marks.Visible := True;
    TBarSeries(Chart1.Series[0]).Marks.Arrow.Visible := True;
    TBarSeries(Chart1.Series[0]).Marks.Style := smsValue;  // Afficher les valeurs

    // Styles disponibles pour les marks:
    // smsValue - Valeur seule
    // smsPercent - Pourcentage
    // smsLabel - Libellé seul
    // smsLabelPercent - Libellé et pourcentage
    // smsLabelValue - Libellé et valeur
    // smsLegend - Texte de la légende
    // smsPointIndex - Index du point
    // smsXValue - Valeur X
  end;
end;
```

## Connexion des graphiques aux données

### Chargement depuis une base de données

```pascal
procedure TForm1.LoadChartFromDatabase;
var
  Series: TBarSeries;
begin
  // Supposons que nous avons un ADOQuery lié à une table de ventes
  ADOQuery1.Close;
  ADOQuery1.SQL.Text := 'SELECT Mois, Montant FROM Ventes WHERE Annee = 2023';
  ADOQuery1.Open;

  // Créer une série pour les données
  Series := TBarSeries.Create(Chart1);
  Chart1.AddSeries(Series);
  Series.Title := 'Ventes 2023';

  // Charger les données depuis la requête
  ADOQuery1.First;
  while not ADOQuery1.Eof do
  begin
    Series.Add(
      ADOQuery1.FieldByName('Montant').AsFloat,
      ADOQuery1.FieldByName('Mois').AsString
    );
    ADOQuery1.Next;
  end;
end;
```

### Utilisation de DBChart pour une liaison automatique

Au lieu de charger manuellement les données, vous pouvez utiliser le composant `TDBChart` qui se lie directement à une source de données :

```pascal
procedure TForm1.SetupDBChart;
var
  Series: TBarSeries;
begin
  // Supposons que DBChart1 est déjà placé sur le formulaire

  // Configurer la source de données
  DBChart1.DataSource := DataSource1;  // Doit pointer vers une source de données

  // Créer une série
  Series := TBarSeries.Create(DBChart1);
  DBChart1.AddSeries(Series);
  Series.Title := 'Ventes 2023';

  // Configurer les champs pour la série
  Series.XLabelsSource := 'Mois';  // Champ pour les libellés X
  Series.DataSource := DataSource1;
  Series.YValues.ValueSource := 'Montant';  // Champ pour les valeurs Y

  // Le graphique se mettra à jour automatiquement quand les données changent
end;
```

## Graphiques interactifs

### Zoom et déplacement

TeeChart permet aux utilisateurs d'interagir avec les graphiques via le zoom et le déplacement :

```pascal
procedure TForm1.EnableChartInteractivity;
begin
  // Activer le zoom
  Chart1.AllowZoom := True;  // L'utilisateur peut zoomer avec la souris

  // Activer le déplacement (pan)
  Chart1.AllowPanning := pmBoth;  // Déplacement dans les deux directions
  // Autres options : pmNone, pmHorizontal, pmVertical

  // Personnaliser les fonctionnalités de zoom
  Chart1.ZoomStyle := zsAnimated;  // Zoom animé
  Chart1.ZoomButton := mbRight;  // Utiliser le bouton droit pour zoomer

  // Ajouter un bouton de reset
  Button1.Caption := 'Réinitialiser zoom';
  Button1.OnClick := ChartResetZoom;
end;

procedure TForm1.ChartResetZoom(Sender: TObject);
begin
  Chart1.ZoomPrecentage(100);  // Réinitialiser au niveau de zoom 100%
  // ou
  Chart1.UndoZoom;  // Annuler le dernier zoom
end;
```

### Événements de clic sur les points

Vous pouvez également détecter les clics sur les points du graphique :

```pascal
procedure TForm1.SetupClickEvents;
begin
  // Associer l'événement de clic
  Chart1.OnClickSeries := ChartClickSeries;
end;

procedure TForm1.ChartClickSeries(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // Afficher les informations du point cliqué
  ShowMessage(Format('Vous avez cliqué sur %s: %g',
    [Series.XLabel[ValueIndex], Series.YValue[ValueIndex]]));

  // Vous pouvez ici ouvrir un autre formulaire, filtrer des données, etc.
end;
```

### Infobulles personnalisées

```pascal
procedure TForm1.EnableCustomTooltips;
begin
  // Activer les infobulles
  Chart1.ShowHint := True;

  // Configurer l'événement pour personnaliser les infobulles
  Chart1.OnGetLegendText := ChartGetLegendText;
end;

procedure TForm1.ChartGetLegendText(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; var LegendText: string);
begin
  // Personnaliser le texte de l'infobulle
  LegendText := Format('%s - %s : %g€ (%.1f%%)',
    [Series.Title, Series.XLabel[ValueIndex], Series.YValue[ValueIndex],
     Series.PercentValue[ValueIndex]]);
end;
```

## Création d'un tableau de bord

Un tableau de bord (dashboard) combine plusieurs graphiques et indicateurs pour offrir une vue d'ensemble des données. Voici comment créer un tableau de bord simple avec TeeChart :

```pascal
procedure TForm1.CreateDashboard;
begin
  // Cette procédure configure le formulaire comme un tableau de bord

  // Configurer le formulaire
  Caption := 'Tableau de bord des ventes';
  Width := 800;
  Height := 600;

  // Créer les panneaux pour organiser les éléments
  CreateDashboardPanels;

  // Créer les graphiques
  CreateSalesChart;
  CreateCategoryChart;
  CreateRegionChart;

  // Créer les indicateurs
  CreateKPIIndicators;

  // Charger les données
  LoadDashboardData;
end;

procedure TForm1.CreateDashboardPanels;
begin
  // Panneau supérieur pour les indicateurs
  PanelTop := TPanel.Create(Self);
  PanelTop.Parent := Self;
  PanelTop.Align := alTop;
  PanelTop.Height := 100;
  PanelTop.Caption := '';

  // Panneau gauche pour le graphique des ventes
  PanelLeft := TPanel.Create(Self);
  PanelLeft.Parent := Self;
  PanelLeft.Align := alLeft;
  PanelLeft.Width := 400;
  PanelLeft.Caption := '';

  // Panneau droit pour les autres graphiques
  PanelRight := TPanel.Create(Self);
  PanelRight.Parent := Self;
  PanelRight.Align := alClient;
  PanelRight.Caption := '';

  // Sous-panneaux dans le panneau droit
  PanelRightTop := TPanel.Create(Self);
  PanelRightTop.Parent := PanelRight;
  PanelRightTop.Align := alTop;
  PanelRightTop.Height := 250;
  PanelRightTop.Caption := '';

  PanelRightBottom := TPanel.Create(Self);
  PanelRightBottom.Parent := PanelRight;
  PanelRightBottom.Align := alClient;
  PanelRightBottom.Caption := '';
end;

procedure TForm1.CreateSalesChart;
var
  Chart: TChart;
  Series: TLineSeries;
begin
  // Créer le graphique d'évolution des ventes
  Chart := TChart.Create(Self);
  Chart.Parent := PanelLeft;
  Chart.Align := alClient;

  // Configurer le titre
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add('Évolution des ventes');

  // Créer une série de ligne
  Series := TLineSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.Title := 'Ventes 2023';
  Series.Pointer.Visible := True;  // Afficher les points

  // Personnaliser l'apparence
  Chart.View3D := False;
  Chart.Legend.Visible := True;
  Chart.Legend.Alignment := laBottom;

  // Sauvegarder une référence pour pouvoir le mettre à jour
  ChartSales := Chart;
end;

procedure TForm1.CreateCategoryChart;
var
  Chart: TChart;
  Series: TPieSeries;
begin
  // Créer le graphique de répartition par catégorie
  Chart := TChart.Create(Self);
  Chart.Parent := PanelRightTop;
  Chart.Align := alClient;

  // Configurer le titre
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add('Ventes par catégorie');

  // Créer une série en camembert
  Series := TPieSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.Marks.Visible := True;
  Series.Marks.Style := smsLabelPercent;  // Afficher libellé et pourcentage

  // Personnaliser l'apparence
  Chart.View3D := True;
  Series.ExplodeBiggest := 20;  // Détacher la plus grande part

  // Sauvegarder une référence
  ChartCategories := Chart;
end;

procedure TForm1.CreateRegionChart;
var
  Chart: TChart;
  Series: TBarSeries;
begin
  // Créer le graphique de ventes par région
  Chart := TChart.Create(Self);
  Chart.Parent := PanelRightBottom;
  Chart.Align := alClient;

  // Configurer le titre
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add('Ventes par région');

  // Créer une série d'histogramme
  Series := TBarSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.ColorEachPoint := True;  // Couleur différente pour chaque barre
  Series.Marks.Visible := True;
  Series.Marks.Style := smsValue;  // Afficher les valeurs

  // Personnaliser l'apparence
  Chart.View3D := True;
  Chart.BottomAxis.LabelsAngle := 45;  // Rotation des libellés

  // Sauvegarder une référence
  ChartRegions := Chart;
end;

procedure TForm1.CreateKPIIndicators;
begin
  // Créer les indicateurs de performance (KPI)
  // Ce sont des étiquettes qui affichent des valeurs clés

  // KPI 1: Total des ventes
  CreateKPI(PanelTop, 20, 'Total des ventes', 'lblTotalSales');

  // KPI 2: Nombre de clients
  CreateKPI(PanelTop, 170, 'Nombre de clients', 'lblClientCount');

  // KPI 3: Ticket moyen
  CreateKPI(PanelTop, 320, 'Ticket moyen', 'lblAvgTicket');

  // KPI 4: Croissance
  CreateKPI(PanelTop, 470, 'Croissance', 'lblGrowth');
end;

procedure TForm1.CreateKPI(Parent: TWinControl; Left: Integer;
  Title: string; LabelName: string);
var
  PanelKPI: TPanel;
  LabelTitle, LabelValue: TLabel;
begin
  // Créer un panneau pour l'indicateur
  PanelKPI := TPanel.Create(Self);
  PanelKPI.Parent := Parent;
  PanelKPI.Left := Left;
  PanelKPI.Top := 20;
  PanelKPI.Width := 130;
  PanelKPI.Height := 60;
  PanelKPI.BevelOuter := bvNone;
  PanelKPI.Color := clWhite;

  // Étiquette pour le titre
  LabelTitle := TLabel.Create(Self);
  LabelTitle.Parent := PanelKPI;
  LabelTitle.Left := 5;
  LabelTitle.Top := 5;
  LabelTitle.Caption := Title;
  LabelTitle.Font.Style := [fsBold];

  // Étiquette pour la valeur
  LabelValue := TLabel.Create(Self);
  LabelValue.Parent := PanelKPI;
  LabelValue.Left := 5;
  LabelValue.Top := 25;
  LabelValue.Width := 120;
  LabelValue.Height := 30;
  LabelValue.Font.Size := 14;
  LabelValue.Font.Color := clNavy;
  LabelValue.Caption := '0';

  // Stocker une référence à l'étiquette par son nom
  LabelValue.Name := LabelName;

  // Mémoriser l'étiquette pour pouvoir la mettre à jour
  if LabelName = 'lblTotalSales' then
    lblTotalSales := LabelValue
  else if LabelName = 'lblClientCount' then
    lblClientCount := LabelValue
  else if LabelName = 'lblAvgTicket' then
    lblAvgTicket := LabelValue
  else if LabelName = 'lblGrowth' then
    lblGrowth := LabelValue;
end;

procedure TForm1.LoadDashboardData;
begin
  // Charger les données depuis la base de données

  // 1. Évolution des ventes
  qrySalesEvolution.Close;
  qrySalesEvolution.Open;

  TLineSeries(ChartSales.Series[0]).Clear;
  qrySalesEvolution.First;
  while not qrySalesEvolution.Eof do
  begin
    TLineSeries(ChartSales.Series[0]).AddXY(
      qrySalesEvolution.FieldByName('MoisNum').AsInteger,
      qrySalesEvolution.FieldByName('Montant').AsFloat,
      qrySalesEvolution.FieldByName('Mois').AsString
    );
    qrySalesEvolution.Next;
  end;

  // 2. Ventes par catégorie
  qrySalesByCategory.Close;
  qrySalesByCategory.Open;

  TPieSeries(ChartCategories.Series[0]).Clear;
  qrySalesByCategory.First;
  while not qrySalesByCategory.Eof do
  begin
    TPieSeries(ChartCategories.Series[0]).Add(
      qrySalesByCategory.FieldByName('Montant').AsFloat,
      qrySalesByCategory.FieldByName('Categorie').AsString,
      clTeeColor  // Couleur automatique
    );
    qrySalesByCategory.Next;
  end;

  // 3. Ventes par région
  qrySalesByRegion.Close;
  qrySalesByRegion.Open;

  TBarSeries(ChartRegions.Series[0]).Clear;
  qrySalesByRegion.First;
  while not qrySalesByRegion.Eof do
  begin
    TBarSeries(ChartRegions.Series[0]).Add(
      qrySalesByRegion.FieldByName('Montant').AsFloat,
      qrySalesByRegion.FieldByName('Region').AsString,
      clTeeColor  // Couleur automatique
    );
    qrySalesByRegion.Next;
  end;

  // 4. Indicateurs de performance (KPI)
  qryKPIs.Close;
  qryKPIs.Open;

  if not qryKPIs.IsEmpty then
  begin
    lblTotalSales.Caption := FormatFloat('#,##0.00 €', qryKPIs.FieldByName('TotalVentes').AsFloat);
    lblClientCount.Caption := FormatFloat('#,##0', qryKPIs.FieldByName('NbClients').AsFloat);
    lblAvgTicket.Caption := FormatFloat('#,##0.00 €', qryKPIs.FieldByName('TicketMoyen').AsFloat);

    // Afficher la croissance avec une couleur différente selon le signe
    if qryKPIs.FieldByName('Croissance').AsFloat >= 0 then
    begin
      lblGrowth.Caption := '+' + FormatFloat('#,##0.0 %', qryKPIs.FieldByName('Croissance').AsFloat);
      lblGrowth.Font.Color := clGreen;
    end
    else
    begin
      lblGrowth.Caption := FormatFloat('#,##0.0 %', qryKPIs.FieldByName('Croissance').AsFloat);
      lblGrowth.Font.Color := clRed;
    end;
  end;
end;
```

## Ajout de filtres et d'interactivité au tableau de bord

Pour rendre votre tableau de bord plus utile, vous pouvez ajouter des filtres et de l'interactivité :

```pascal
procedure TForm1.AddDashboardFilters;
var
  PanelFilters: TPanel;
  LabelPeriod, LabelRegion: TLabel;
begin
  // Créer un panneau pour les filtres
  PanelFilters := TPanel.Create(Self);
  PanelFilters.Parent := Self;
  PanelFilters.Align := alTop;
  PanelFilters.Height := 50;
  PanelFilters.Top := 0;
  PanelFilters.Caption := '';

  // Filtre de période
  LabelPeriod := TLabel.Create(Self);
  LabelPeriod.Parent := PanelFilters;
  LabelPeriod.Left := 20;
  LabelPeriod.Top := 15;
  LabelPeriod.Caption := 'Période:';

  cbPeriod := TComboBox.Create(Self);
  cbPeriod.Parent := PanelFilters;
  cbPeriod.Left := 80;
  cbPeriod.Top := 12;
  cbPeriod.Width := 120;
  cbPeriod.Style := csDropDownList;
  cbPeriod.Items.Add('Cette année');
  cbPeriod.Items.Add('12 derniers mois');
  cbPeriod.Items.Add('Ce trimestre');
  cbPeriod.Items.Add('Ce mois');
  cbPeriod.ItemIndex := 0;
  cbPeriod.OnChange := FilterChanged;

  // Filtre de région
  LabelRegion := TLabel.Create(Self);
  LabelRegion.Parent := PanelFilters;
  LabelRegion.Left := 220;
  LabelRegion.Top := 15;
  LabelRegion.Caption := 'Région:';

  cbRegion := TComboBox.Create(Self);
  cbRegion.Parent := PanelFilters;
  cbRegion.Left := 270;
  cbRegion.Top := 12;
  cbRegion.Width := 120;
  cbRegion.Style := csDropDownList;
  cbRegion.Items.Add('Toutes');
  // Charger les régions depuis la base de données
  LoadRegions;
  cbRegion.ItemIndex := 0;
  cbRegion.OnChange := FilterChanged;

  // Bouton de rafraîchissement
  btnRefresh := TButton.Create(Self);
  btnRefresh.Parent := PanelFilters;
  btnRefresh.Left := 410;
  btnRefresh.Top := 10;
  btnRefresh.Width := 100;
  btnRefresh.Height := 30;
  btnRefresh.Caption := 'Actualiser';
  btnRefresh.OnClick := RefreshDashboard;
end;

procedure TForm1.LoadRegions;
begin
  // Charger la liste des régions depuis la base de données
  qryRegions.Close;
  qryRegions.Open;

  qryRegions.First;
  while not qryRegions.Eof do
  begin
    cbRegion.Items.Add(qryRegions.FieldByName('Region').AsString);
    qryRegions.Next;
  end;
end;

procedure TForm1.FilterChanged(Sender: TObject);
begin
  // Cette méthode est appelée quand l'utilisateur change un filtre
  // Rien à faire ici, le bouton Actualiser sera utilisé pour appliquer les filtres
end;

procedure TForm1.RefreshDashboard(Sender: TObject);
begin
  // Appliquer les filtres et mettre à jour le tableau de bord

  // 1. Déterminer la période
  case cbPeriod.ItemIndex of
    0: // Cette année
      begin
        StartDate := EncodeDate(YearOf(Date), 1, 1);
        EndDate := Date;
      end;
    1: // 12 derniers mois
      begin
        StartDate := IncMonth(Date, -12);
        EndDate := Date;
      end;
    2: // Ce trimestre
      begin
        case MonthOf(Date) of
          1..3: StartDate := EncodeDate(YearOf(Date), 1, 1);
          4..6: StartDate := EncodeDate(YearOf(Date), 4, 1);
          7..9: StartDate := EncodeDate(YearOf(Date), 7, 1);
          else  StartDate := EncodeDate(YearOf(Date), 10, 1);
        end;
        EndDate := Date;
      end;
    3: // Ce mois
      begin
        StartDate := EncodeDate(YearOf(Date), MonthOf(Date), 1);
        EndDate := Date;
      end;
  end;

  // 2. Déterminer la région
  if cbRegion.ItemIndex = 0 then
    SelectedRegion := ''  // Toutes les régions
  else
    SelectedRegion := cbRegion.Text;

  // 3. Mettre à jour les requêtes et les graphiques
  UpdateDashboardData;
end;

procedure TForm1.UpdateDashboardData;
begin
  // Mettre à jour les requêtes avec les filtres

  // 1. Évolution des ventes
  qrySalesEvolution.Close;
  qrySalesEvolution.Parameters.ParamByName('StartDate').Value := StartDate;
  qrySalesEvolution.Parameters.ParamByName('EndDate').Value := EndDate;

  if SelectedRegion <> '' then
    qrySalesEvolution.Parameters.ParamByName('Region').Value := SelectedRegion
  else
    qrySalesEvolution.Parameters.ParamByName('Region').Value := Null;

  qrySalesEvolution.Open;

  // 2. Ventes par catégorie
  qrySalesByCategory.Close;
  qrySalesByCategory.Parameters.ParamByName('StartDate').Value := StartDate;
  qrySalesByCategory.Parameters.ParamByName('EndDate').Value := EndDate;

  if SelectedRegion <> '' then
    qrySalesByCategory.Parameters.ParamByName('Region').Value := SelectedRegion
  else
    qrySalesByCategory.Parameters.ParamByName('Region').Value := Null;

  qrySalesByCategory.Open;

  // 3. Ventes par région
  qrySalesByRegion.Close;
  qrySalesByRegion.Parameters.ParamByName('StartDate').Value := StartDate;
  qrySalesByRegion.Parameters.ParamByName('EndDate').Value := EndDate;
  qrySalesByRegion.Open;

  // 4. Indicateurs de performance (KPI)
  qryKPIs.Close;
  qryKPIs.Parameters.ParamByName('StartDate').Value := StartDate;
  qryKPIs.Parameters.ParamByName('EndDate').Value := EndDate;

  if SelectedRegion <> '' then
    qryKPIs.Parameters.ParamByName('Region').Value := SelectedRegion
  else
    qryKPIs.Parameters.ParamByName('Region').Value := Null;

  qryKPIs.Open;

  // Mettre à jour les graphiques
  LoadDashboardData;

  // Mettre à jour le titre pour refléter les filtres
  PanelTop.Caption := Format('Période: %s au %s | Région: %s',
    [FormatDateTime('dd/mm/yyyy', StartDate),
     FormatDateTime('dd/mm/yyyy', EndDate),
     IfThen(SelectedRegion = '', 'Toutes', SelectedRegion)]);
end;
```

## Ajout d'interactivité entre les graphiques

Une fonctionnalité avancée des tableaux de bord est l'interactivité entre les graphiques, où un clic sur un élément d'un graphique met à jour les autres éléments :

```pascal
procedure TForm1.SetupChartInteractivity;
begin
  // Configurer les événements de clic pour les graphiques
  ChartRegions.OnClickSeries := ChartRegionsClick;
  ChartCategories.OnClickSeries := ChartCategoriesClick;
end;

procedure TForm1.ChartRegionsClick(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Region: string;
begin
  // Récupérer la région cliquée
  Region := Series.XLabel[ValueIndex];

  // Mettre à jour le filtre de région
  cbRegion.ItemIndex := cbRegion.Items.IndexOf(Region);

  // Si Ctrl est maintenu, ne pas actualiser pour permettre la sélection multiple
  if not (ssCtrl in Shift) then
    RefreshDashboard(nil);
end;

procedure TForm1.ChartCategoriesClick(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Category: string;
begin
  // Récupérer la catégorie cliquée
  Category := Series.XLabel[ValueIndex];

  // Ouvrir un rapport détaillé pour cette catégorie
  OpenCategoryReport(Category);
end;

procedure TForm1.OpenCategoryReport(const Category: string);
begin
  // Créer un formulaire pour afficher les détails de la catégorie
  with TfrmCategoryDetail.Create(Self) do
  try
    // Configurer le formulaire
    Caption := 'Détails de la catégorie: ' + Category;

    // Définir la période et la catégorie
    SetParams(StartDate, EndDate, Category, SelectedRegion);

    // Afficher le formulaire
    ShowModal;
  finally
    Free;
  end;
end;
```

## Exportation et partage des graphiques et tableaux de bord

TeeChart offre des fonctionnalités d'exportation pour partager vos visualisations :

```pascal
procedure TForm1.AddExportButtons;
var
  PanelExport: TPanel;
  btnExportImage, btnExportPDF, btnExportExcel, btnPrint: TButton;
begin
  // Créer un panneau pour les boutons d'exportation
  PanelExport := TPanel.Create(Self);
  PanelExport.Parent := Self;
  PanelExport.Align := alBottom;
  PanelExport.Height := 40;
  PanelExport.Caption := '';

  // Bouton d'exportation en image
  btnExportImage := TButton.Create(Self);
  btnExportImage.Parent := PanelExport;
  btnExportImage.Left := 20;
  btnExportImage.Top := 8;
  btnExportImage.Width := 100;
  btnExportImage.Height := 25;
  btnExportImage.Caption := 'Export Image';
  btnExportImage.OnClick := ExportToImage;

  // Bouton d'exportation en PDF
  btnExportPDF := TButton.Create(Self);
  btnExportPDF.Parent := PanelExport;
  btnExportPDF.Left := 130;
  btnExportPDF.Top := 8;
  btnExportPDF.Width := 100;
  btnExportPDF.Height := 25;
  btnExportPDF.Caption := 'Export PDF';
  btnExportPDF.OnClick := ExportToPDF;

  // Bouton d'exportation en Excel
  btnExportExcel := TButton.Create(Self);
  btnExportExcel.Parent := PanelExport;
  btnExportExcel.Left := 240;
  btnExportExcel.Top := 8;
  btnExportExcel.Width := 100;
  btnExportExcel.Height := 25;
  btnExportExcel.Caption := 'Export Excel';
  btnExportExcel.OnClick := ExportToExcel;

  // Bouton d'impression
  btnPrint := TButton.Create(Self);
  btnPrint.Parent := PanelExport;
  btnPrint.Left := 350;
  btnPrint.Top := 8;
  btnPrint.Width := 100;
  btnPrint.Height := 25;
  btnPrint.Caption := 'Imprimer';
  btnPrint.OnClick := PrintDashboard;
end;

procedure TForm1.ExportToImage(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter en image';
    SaveDialog.Filter := 'Images PNG (*.png)|*.png|Images JPEG (*.jpg)|*.jpg|Images BMP (*.bmp)|*.bmp';
    SaveDialog.DefaultExt := 'png';

    if SaveDialog.Execute then
    begin
      case ExtractFileExt(SaveDialog.FileName) of
        '.png': ChartSales.SaveToPNGFile(SaveDialog.FileName);
        '.jpg': ChartSales.SaveToJPEGFile(SaveDialog.FileName);
        '.bmp': ChartSales.SaveToBitmapFile(SaveDialog.FileName);
      end;

      ShowMessage('Graphique exporté avec succès!');
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.ExportToPDF(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  PDF: TCanvas;
  PDFPage: TMetaFile;
  PDFPageCanvas: TMetafileCanvas;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter en PDF';
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';
    SaveDialog.DefaultExt := 'pdf';

    if SaveDialog.Execute then
    begin
      // Note: Pour TeeChart Pro, il existe une méthode directe ExportToPDF
      // Pour TeeChart Standard, il faut utiliser une bibliothèque tierce ou TfrxReport

      // Exemple avec TfrxReport
      with TfrxReport.Create(nil) do
      try
        // Créer une page vide
        with TfrxReportPage.Create(TfrxReport) do
        begin
          CreateUniqueName;
          Width := 210;
          Height := 297;
        end;

        // Préparer le rapport
        PrepareReport;

        // Exporter en PDF
        ExportToPDF(SaveDialog.FileName);

        ShowMessage('Tableau de bord exporté en PDF avec succès!');
      finally
        Free;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.ExportToExcel(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  Excel: Variant;
  Workbook, Sheet: Variant;
  i: Integer;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter vers Excel';
    SaveDialog.Filter := 'Fichiers Excel (*.xlsx)|*.xlsx';
    SaveDialog.DefaultExt := 'xlsx';

    if SaveDialog.Execute then
    begin
      // Utiliser Automation pour exporter vers Excel
      try
        Excel := CreateOleObject('Excel.Application');
        Excel.Visible := False;
        Workbook := Excel.Workbooks.Add;
        Sheet := Workbook.Worksheets[1];

        // Titre
        Sheet.Cells[1, 1] := 'Tableau de bord des ventes';
        Sheet.Cells[1, 1].Font.Bold := True;
        Sheet.Cells[1, 1].Font.Size := 14;

        // Période
        Sheet.Cells[2, 1] := Format('Période: %s au %s | Région: %s',
          [FormatDateTime('dd/mm/yyyy', StartDate),
           FormatDateTime('dd/mm/yyyy', EndDate),
           IfThen(SelectedRegion = '', 'Toutes', SelectedRegion)]);

        // Exporter les données des KPIs
        Sheet.Cells[4, 1] := 'Indicateurs clés';
        Sheet.Cells[4, 1].Font.Bold := True;

        Sheet.Cells[5, 1] := 'Total des ventes:';
        Sheet.Cells[5, 2] := qryKPIs.FieldByName('TotalVentes').AsFloat;
        Sheet.Cells[5, 2].NumberFormat := '#,##0.00 €';

        Sheet.Cells[6, 1] := 'Nombre de clients:';
        Sheet.Cells[6, 2] := qryKPIs.FieldByName('NbClients').AsInteger;
        Sheet.Cells[6, 2].NumberFormat := '#,##0';

        Sheet.Cells[7, 1] := 'Ticket moyen:';
        Sheet.Cells[7, 2] := qryKPIs.FieldByName('TicketMoyen').AsFloat;
        Sheet.Cells[7, 2].NumberFormat := '#,##0.00 €';

        Sheet.Cells[8, 1] := 'Croissance:';
        Sheet.Cells[8, 2] := qryKPIs.FieldByName('Croissance').AsFloat / 100; // En pourcentage
        Sheet.Cells[8, 2].NumberFormat := '+0.0%;-0.0%;0%';

        // Exporter les données des graphiques (ventes par région)
        Sheet.Cells[10, 1] := 'Ventes par région';
        Sheet.Cells[10, 1].Font.Bold := True;

        Sheet.Cells[11, 1] := 'Région';
        Sheet.Cells[11, 2] := 'Montant';

        qrySalesByRegion.First;
        i := 12;
        while not qrySalesByRegion.Eof do
        begin
          Sheet.Cells[i, 1] := qrySalesByRegion.FieldByName('Region').AsString;
          Sheet.Cells[i, 2] := qrySalesByRegion.FieldByName('Montant').AsFloat;
          Sheet.Cells[i, 2].NumberFormat := '#,##0.00 €';
          Inc(i);
          qrySalesByRegion.Next;
        end;

        // Créer un graphique Excel
        var Chart := Sheet.ChartObjects.Add(400, 75, 300, 200);
        Chart.Chart.ChartType := 51; // xlColumnClustered
        Chart.Chart.SetSourceData(Sheet.Range['A11:B' + IntToStr(i-1)]);
        Chart.Chart.HasTitle := True;
        Chart.Chart.ChartTitle.Text := 'Ventes par région';

        // Enregistrer le fichier
        Workbook.SaveAs(SaveDialog.FileName);

        ShowMessage('Tableau de bord exporté en Excel avec succès!');
      finally
        Excel.Quit;
        Excel := Unassigned;
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TForm1.PrintDashboard(Sender: TObject);
var
  PrintDialog: TPrintDialog;
begin
  PrintDialog := TPrintDialog.Create(nil);
  try
    if PrintDialog.Execute then
    begin
      // Imprimer les graphiques
      ChartSales.Print;
      ChartCategories.Print;
      ChartRegions.Print;

      ShowMessage('Tableau de bord imprimé avec succès!');
    end;
  finally
    PrintDialog.Free;
  end;
end;
```

## Animations et effets visuels

TeeChart permet d'ajouter des animations pour rendre vos graphiques plus attrayants :

```pascal
procedure TForm1.AddChartAnimations;
begin
  // Configurer les animations pour le graphique de ventes
  with ChartSales.SeriesAnimation[0] do
  begin
    Style := anSpeed;  // Style d'animation
    StartValue := 0;   // Valeur de départ
    Steps := 20;       // Nombre d'étapes
    Delay := 0;        // Délai initial
    Enabled := True;   // Activer l'animation
  end;

  // Configurer les animations pour le graphique en camembert
  with ChartCategories.SeriesAnimation[0] do
  begin
    Style := anPie;    // Style spécifique aux camemberts
    Steps := 20;
    Enabled := True;
  end;

  // Configurer les animations pour le graphique de régions
  with ChartRegions.SeriesAnimation[0] do
  begin
    Style := anLeftRight;
    Steps := 20;
    Enabled := True;
  end;
end;

procedure TForm1.RefreshWithAnimation;
begin
  // Désactiver temporairement les animations
  ChartSales.SeriesAnimation[0].Enabled := False;
  ChartCategories.SeriesAnimation[0].Enabled := False;
  ChartRegions.SeriesAnimation[0].Enabled := False;

  // Mettre à jour les données
  UpdateDashboardData;

  // Réactiver et lancer les animations
  ChartSales.SeriesAnimation[0].Enabled := True;
  ChartCategories.SeriesAnimation[0].Enabled := True;
  ChartRegions.SeriesAnimation[0].Enabled := True;

  // Lancer les animations
  ChartSales.AnimateSeries(0);
  ChartCategories.AnimateSeries(0);
  ChartRegions.AnimateSeries(0);
end;
```

## Graphiques avancés et fonctionnalités spéciales

TeeChart offre de nombreuses fonctionnalités avancées pour des visualisations spécifiques. Voici quelques exemples :

### Graphique à bulles (3 dimensions de données)

```pascal
procedure TForm1.CreateBubbleChart;
var
  Series: TBubbleSeries;
begin
  // Créer une série à bulles
  Series := TBubbleSeries.Create(Chart1);
  Chart1.AddSeries(Series);
  Series.Title := 'Performance produits';

  // La taille de la bulle représente une troisième dimension
  // Paramètres : X, Y, Taille, Étiquette
  Series.AddBubble(10, 20, 5, 'Produit A');  // X=10, Y=20, Taille=5
  Series.AddBubble(15, 30, 8, 'Produit B');  // X=15, Y=30, Taille=8
  Series.AddBubble(20, 15, 12, 'Produit C'); // X=20, Y=15, Taille=12

  // Personnaliser l'apparence
  Series.Pointer.InflateMargins := True;
  Series.Pointer.Brush.Color := clRed;
  Series.Pointer.Gradient.Visible := True;

  // Configurer les axes
  Chart1.BottomAxis.Title.Caption := 'Prix';
  Chart1.LeftAxis.Title.Caption := 'Ventes';

  // Ajouter une légende pour expliquer la taille
  Chart1.SubTitle.Text.Clear;
  Chart1.SubTitle.Text.Add('Taille des bulles = Marge');
  Chart1.SubTitle.Font.Style := [fsItalic];
end;
```

### Graphique boursier (chandelier)

```pascal
procedure TForm1.CreateCandleChart;
var
  Series: TCandleSeries;
begin
  // Créer une série de chandeliers (boursier)
  Series := TCandleSeries.Create(Chart1);
  Chart1.AddSeries(Series);
  Series.Title := 'Cours de l''action';

  // Ajouter des données boursières
  // Paramètres : Date, Ouverture, Haut, Bas, Clôture
  Series.AddCandle(EncodeDate(2023, 1, 1), 100, 110, 95, 105);
  Series.AddCandle(EncodeDate(2023, 1, 2), 105, 115, 100, 110);
  Series.AddCandle(EncodeDate(2023, 1, 3), 110, 120, 105, 115);

  // Personnaliser l'apparence
  Series.CandleWidth := 0.8;
  Series.UpCloseColor := clGreen;  // Couleur hausse
  Series.DownCloseColor := clRed;  // Couleur baisse

  // Configurer les axes
  Chart1.BottomAxis.Title.Caption := 'Date';
  Chart1.BottomAxis.DateTimeFormat := 'dd/mm/yyyy';
  Chart1.LeftAxis.Title.Caption := 'Prix';
end;
```

### Graphique jauge

```pascal
procedure TForm1.CreateGaugeChart;
var
  Series: TCircularGauge;
begin
  // Créer une jauge circulaire
  Series := TCircularGauge.Create(Chart1);
  Chart1.AddSeries(Series);
  Series.Title := 'Progression';

  // Configurer la jauge
  Series.SetGaugeValue(70);  // Valeur à 70%
  Series.GreenLine.Value := 100;  // Valeur maximale
  Series.RedLine.Value := 0;      // Valeur minimale
  Series.YellowLine.Value := 50;  // Seuil intermédiaire

  // Personnaliser l'apparence
  Series.RotationAngle := 135;  // Angle de départ
  Series.TotalAngle := 270;     // Angle total
  Series.MinValue := 0;         // Valeur minimale
  Series.MaxValue := 100;       // Valeur maximale

  // Afficher la valeur
  Series.ValueFormat := '0%';
  Series.ShowValue := True;
end;
```

## Optimisation des performances

Pour les graphiques avec beaucoup de données, vous pouvez optimiser les performances :

```pascal
procedure TForm1.OptimizeChartPerformance;
begin
  // Désactiver certaines fonctionnalités gourmandes en ressources
  Chart1.View3D := False;  // La 3D consomme plus de ressources

  // Utiliser des séries optimisées pour les grandes quantités de données
  // TFastLineSeries au lieu de TLineSeries
  // TPointSeries au lieu de TLineSeries avec points

  // Réduire le nombre de points affichés
  TFastLineSeries(Chart1.Series[0]).MaxPointsPerPage := 1000;

  // Optimiser le dessin
  Chart1.BufferedDisplay := True;  // Dessiner en mémoire avant d'afficher

  // Désactiver les fonctionnalités non utilisées
  Chart1.Legend.Visible := False;  // Si la légende n'est pas nécessaire
  Chart1.MarginLeft := 3;  // Réduire les marges
  Chart1.MarginRight := 3;
  Chart1.MarginTop := 3;
  Chart1.MarginBottom := 3;
end;
```

## Conseils et bonnes pratiques

### Choisir le bon type de graphique

- **Histogrammes** : Comparer des valeurs entre différentes catégories
- **Courbes** : Montrer l'évolution des données dans le temps
- **Camemberts** : Montrer des proportions d'un tout (limité à 5-7 segments pour la lisibilité)
- **Aires** : Montrer l'évolution et l'accumulation de valeurs dans le temps
- **Nuages de points** : Montrer les corrélations entre deux variables
- **Bulles** : Ajouter une troisième dimension aux nuages de points
- **Jauges** : Montrer la progression vers un objectif

### Conception efficace de tableaux de bord

1. **Simplicité** : Ne pas surcharger le tableau de bord avec trop d'informations
2. **Organisation** : Placer les indicateurs les plus importants en haut à gauche
3. **Cohérence** : Utiliser les mêmes couleurs pour les mêmes concepts
4. **Interactivité** : Permettre à l'utilisateur d'explorer les données
5. **Contexte** : Toujours fournir le contexte (période, filtres appliqués)
6. **Comparaison** : Inclure des références pour comparer (année précédente, objectifs)

### Personnalisation pour votre marque

```pascal
procedure TForm1.ApplyBrandingToCharts;
begin
  // Appliquer les couleurs de la marque
  Chart1.Color := RGB(240, 240, 240);  // Couleur de fond

  // Palette de couleurs personnalisée
  Chart1.SeriesList.ColorPalette := TColorPalette.Create(Chart1);
  Chart1.SeriesList.ColorPalette.Colors[0] := RGB(0, 120, 200);    // Bleu primaire
  Chart1.SeriesList.ColorPalette.Colors[1] := RGB(240, 170, 0);    // Orange secondaire
  Chart1.SeriesList.ColorPalette.Colors[2] := RGB(0, 160, 100);    // Vert tertiaire
  Chart1.SeriesList.ColorPalette.Colors[3] := RGB(200, 60, 60);    // Rouge accent

  // Polices de caractères
  Chart1.Title.Font.Name := 'Segoe UI';
  Chart1.Legend.Font.Name := 'Segoe UI';
  Chart1.BottomAxis.Title.Font.Name := 'Segoe UI';
  Chart1.LeftAxis.Title.Font.Name := 'Segoe UI';

  // Ajouter un logo (si TeeChart Pro)
  Chart1.BackImage.Assign(imgLogo.Picture);
  Chart1.BackImageInside := False;
  Chart1.BackImageMode := pbCenter;
  Chart1.BackImageTransp := 70;  // Transparence à 70%
end;
```

## Intégration de TeeChart dans vos applications réelles

### Création d'une unité réutilisable pour les graphiques

Pour mieux organiser votre code et réutiliser vos composants de graphiques, vous pouvez créer une unité spécifique :

```pascal
unit ChartUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls,
  VCLTee.Chart, VCLTee.Series, VCLTee.TeEngine, VCLTee.TeeProcs;

type
  // Structure pour stocker les paramètres du graphique
  TChartSettings = record
    Title: string;
    XAxisTitle: string;
    YAxisTitle: string;
    Use3D: Boolean;
    ShowLegend: Boolean;
  end;

  // Classe d'aide pour les graphiques
  TChartHelper = class
  public
    // Créer un graphique en barres
    class function CreateBarChart(AOwner: TComponent; Parent: TWinControl;
      Settings: TChartSettings): TChart;

    // Créer un graphique en courbe
    class function CreateLineChart(AOwner: TComponent; Parent: TWinControl;
      Settings: TChartSettings): TChart;

    // Créer un graphique en camembert
    class function CreatePieChart(AOwner: TComponent; Parent: TWinControl;
      Settings: TChartSettings): TChart;

    // Charger les données dans un graphique à partir d'une requête
    class procedure LoadChartFromQuery(Chart: TChart; Query: TDataSet;
      XField, YField: string; SeriesTitle: string = '');

    // Appliquer un style cohérent à tous les graphiques
    class procedure ApplyStandardStyle(Chart: TChart);
  end;

implementation

class function TChartHelper.CreateBarChart(AOwner: TComponent; Parent: TWinControl;
  Settings: TChartSettings): TChart;
var
  Chart: TChart;
  Series: TBarSeries;
begin
  // Créer le graphique
  Chart := TChart.Create(AOwner);
  Chart.Parent := Parent;
  Chart.Align := alClient;

  // Appliquer les paramètres
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add(Settings.Title);
  Chart.BottomAxis.Title.Caption := Settings.XAxisTitle;
  Chart.LeftAxis.Title.Caption := Settings.YAxisTitle;
  Chart.View3D := Settings.Use3D;
  Chart.Legend.Visible := Settings.ShowLegend;

  // Créer une série
  Series := TBarSeries.Create(Chart);
  Chart.AddSeries(Series);

  // Appliquer le style standard
  ApplyStandardStyle(Chart);

  Result := Chart;
end;

class function TChartHelper.CreateLineChart(AOwner: TComponent; Parent: TWinControl;
  Settings: TChartSettings): TChart;
var
  Chart: TChart;
  Series: TLineSeries;
begin
  // Créer le graphique
  Chart := TChart.Create(AOwner);
  Chart.Parent := Parent;
  Chart.Align := alClient;

  // Appliquer les paramètres
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add(Settings.Title);
  Chart.BottomAxis.Title.Caption := Settings.XAxisTitle;
  Chart.LeftAxis.Title.Caption := Settings.YAxisTitle;
  Chart.View3D := Settings.Use3D;
  Chart.Legend.Visible := Settings.ShowLegend;

  // Créer une série
  Series := TLineSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.Pointer.Visible := True;

  // Appliquer le style standard
  ApplyStandardStyle(Chart);

  Result := Chart;
end;

class function TChartHelper.CreatePieChart(AOwner: TComponent; Parent: TWinControl;
  Settings: TChartSettings): TChart;
var
  Chart: TChart;
  Series: TPieSeries;
begin
  // Créer le graphique
  Chart := TChart.Create(AOwner);
  Chart.Parent := Parent;
  Chart.Align := alClient;

  // Appliquer les paramètres
  Chart.Title.Text.Clear;
  Chart.Title.Text.Add(Settings.Title);
  Chart.View3D := Settings.Use3D;
  Chart.Legend.Visible := Settings.ShowLegend;

  // Créer une série
  Series := TPieSeries.Create(Chart);
  Chart.AddSeries(Series);
  Series.Marks.Visible := True;
  Series.Marks.Style := smsLabelPercent;

  // Appliquer le style standard
  ApplyStandardStyle(Chart);

  Result := Chart;
end;

class procedure TChartHelper.LoadChartFromQuery(Chart: TChart; Query: TDataSet;
  XField, YField: string; SeriesTitle: string = '');
begin
  // Vérifier que le graphique a au moins une série
  if Chart.SeriesCount = 0 then
    Exit;

  // Effacer les données existantes
  Chart.Series[0].Clear;

  // Définir le titre de la série
  if SeriesTitle <> '' then
    Chart.Series[0].Title := SeriesTitle;

  // Charger les données depuis la requête
  Query.First;
  while not Query.Eof do
  begin
    if Chart.Series[0] is TPieSeries then
    begin
      // Pour les camemberts
      TPieSeries(Chart.Series[0]).Add(
        Query.FieldByName(YField).AsFloat,
        Query.FieldByName(XField).AsString,
        clTeeColor
      );
    end
    else if Chart.Series[0] is TBarSeries then
    begin
      // Pour les histogrammes
      TBarSeries(Chart.Series[0]).Add(
        Query.FieldByName(YField).AsFloat,
        Query.FieldByName(XField).AsString,
        clTeeColor
      );
    end
    else if Chart.Series[0] is TLineSeries then
    begin
      // Pour les courbes
      TLineSeries(Chart.Series[0]).AddXY(
        Query.RecNo,
        Query.FieldByName(YField).AsFloat,
        Query.FieldByName(XField).AsString
      );
    end;

    Query.Next;
  end;
end;

class procedure TChartHelper.ApplyStandardStyle(Chart: TChart);
begin
  // Appliquer un style standard et cohérent

  // Couleurs et fond
  Chart.Color := clWhite;
  Chart.BackWall.Color := clWhite;
  Chart.Gradient.Visible := False;

  // Police
  Chart.Title.Font.Name := 'Segoe UI';
  Chart.Title.Font.Size := 12;
  Chart.Title.Font.Style := [fsBold];

  Chart.Legend.Font.Name := 'Segoe UI';
  Chart.Legend.Font.Size := 9;

  Chart.BottomAxis.Title.Font.Name := 'Segoe UI';
  Chart.BottomAxis.Title.Font.Size := 9;
  Chart.LeftAxis.Title.Font.Name := 'Segoe UI';
  Chart.LeftAxis.Title.Font.Size := 9;

  // Grille
  Chart.BottomAxis.Grid.Visible := True;
  Chart.BottomAxis.Grid.Style := psDot;
  Chart.LeftAxis.Grid.Visible := True;
  Chart.LeftAxis.Grid.Style := psDot;

  // Marges
  Chart.MarginLeft := 5;
  Chart.MarginRight := 5;
  Chart.MarginTop := 5;
  Chart.MarginBottom := 5;
end;

end.
```

### Utilisation de l'unité réutilisable

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param,
  VCLTee.Chart, VCLTee.Series, VCLTee.TeEngine, VCLTee.TeeProcs,
  ChartUtils;  // Notre unité d'aide pour les graphiques

type
  TForm1 = class(TForm)
    PanelTop: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    ButtonRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonRefreshClick(Sender: TObject);
  private
    FSalesChart: TChart;
    FCategoryChart: TChart;
    FRegionChart: TChart;
    FQuery1, FQuery2, FQuery3: TFDQuery;

    procedure CreateCharts;
    procedure LoadChartData;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialiser les requêtes
  FQuery1 := TFDQuery.Create(Self);
  FQuery1.Connection := DataModule1.FDConnection1;
  FQuery1.SQL.Text := 'SELECT Mois, SUM(Montant) AS Total FROM Ventes GROUP BY Mois ORDER BY MoisNum';

  FQuery2 := TFDQuery.Create(Self);
  FQuery2.Connection := DataModule1.FDConnection1;
  FQuery2.SQL.Text := 'SELECT Categorie, SUM(Montant) AS Total FROM Ventes GROUP BY Categorie';

  FQuery3 := TFDQuery.Create(Self);
  FQuery3.Connection := DataModule1.FDConnection1;
  FQuery3.SQL.Text := 'SELECT Region, SUM(Montant) AS Total FROM Ventes GROUP BY Region';

  // Créer les graphiques
  CreateCharts;

  // Charger les données
  LoadChartData;
end;

procedure TForm1.CreateCharts;
var
  Settings: TChartSettings;
begin
  // Paramètres pour le graphique des ventes
  Settings.Title := 'Évolution des ventes';
  Settings.XAxisTitle := 'Mois';
  Settings.YAxisTitle := 'Montant (€)';
  Settings.Use3D := False;
  Settings.ShowLegend := True;

  // Créer le graphique d'évolution des ventes
  FSalesChart := TChartHelper.CreateLineChart(Self, PanelLeft, Settings);

  // Paramètres pour le graphique des catégories
  Settings.Title := 'Répartition par catégorie';
  Settings.Use3D := True;
  Settings.ShowLegend := True;

  // Créer le graphique de répartition par catégorie
  FCategoryChart := TChartHelper.CreatePieChart(Self, PanelRight, Settings);

  // Paramètres pour le graphique des régions
  Settings.Title := 'Ventes par région';
  Settings.XAxisTitle := 'Région';
  Settings.YAxisTitle := 'Montant (€)';
  Settings.Use3D := True;
  Settings.ShowLegend := False;

  // Créer un panneau supplémentaire pour le troisième graphique
  var Panel := TPanel.Create(Self);
  Panel.Parent := PanelTop;
  Panel.Align := alClient;

  // Créer le graphique des ventes par région
  FRegionChart := TChartHelper.CreateBarChart(Self, Panel, Settings);
end;

procedure TForm1.LoadChartData;
begin
  // Charger les données dans les graphiques
  FQuery1.Open;
  TChartHelper.LoadChartFromQuery(FSalesChart, FQuery1, 'Mois', 'Total', 'Ventes mensuelles');

  FQuery2.Open;
  TChartHelper.LoadChartFromQuery(FCategoryChart, FQuery2, 'Categorie', 'Total');

  FQuery3.Open;
  TChartHelper.LoadChartFromQuery(FRegionChart, FQuery3, 'Region', 'Total', 'Ventes par région');
end;

procedure TForm1.ButtonRefreshClick(Sender: TObject);
begin
  // Actualiser les données
  LoadChartData;
end;

end.
```

## Exemple complet : Tableau de bord de ventes

Voici un exemple complet de tableau de bord de ventes utilisant les concepts abordés dans cette section :

```pascal
unit SalesDashboard;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, Vcl.Imaging.pngimage,
  VCLTee.Chart, VCLTee.Series, VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.TeeGDIPlus,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param;

type
  TfrmSalesDashboard = class(TForm)
    PanelTop: TPanel;
    PanelFilters: TPanel;
    PanelContent: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PanelExport: TPanel;
    lblTitle: TLabel;
    lblPeriod: TLabel;
    cbPeriod: TComboBox;
    lblRegion: TLabel;
    cbRegion: TComboBox;
    btnRefresh: TSpeedButton;
    ChartSales: TChart;
    ChartCategories: TChart;
    PanelRightTop: TPanel;
    PanelRightBottom: TPanel;
    ChartRegions: TChart;
    PanelKPI: TPanel;
    PanelTotalSales: TPanel;
    lblTotalSalesTitle: TLabel;
    lblTotalSales: TLabel;
    PanelNbClients: TPanel;
    lblNbClientsTitle: TLabel;
    lblNbClients: TLabel;
    PanelAvgTicket: TPanel;
    lblAvgTicketTitle: TLabel;
    lblAvgTicket: TLabel;
    PanelGrowth: TPanel;
    lblGrowthTitle: TLabel;
    lblGrowth: TLabel;
    btnExportPDF: TButton;
    btnExportExcel: TButton;
    btnPrint: TButton;
    imgLogo: TImage;
    DateFrom: TDateTimePicker;
    DateTo: TDateTimePicker;
    lblDateRange: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cbPeriodChange(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cbRegionChange(Sender: TObject);
    procedure ChartSalesClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChartCategoriesClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChartRegionsClickSeries(Sender: TCustomChart; Series: TChartSeries;
      ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnExportPDFClick(Sender: TObject);
    procedure btnExportExcelClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure DateFromChange(Sender: TObject);
    procedure DateToChange(Sender: TObject);
  private
    FQuery1, FQuery2, FQuery3, FQuery4: TFDQuery;

    procedure InitializeQueries;
    procedure InitializeCharts;
    procedure SetupFilters;
    procedure LoadChartData;
    procedure UpdateKPIs;
    procedure ShowDetailForm(const Category: string);
    procedure ApplyBranding;
  end;

var
  frmSalesDashboard: TfrmSalesDashboard;

implementation

{$R *.dfm}

uses DataModule, DetailForm;

procedure TfrmSalesDashboard.FormCreate(Sender: TObject);
begin
  // Initialiser les composants
  InitializeQueries;
  InitializeCharts;
  SetupFilters;
  ApplyBranding;

  // Charger les données initiales
  LoadChartData;
  UpdateKPIs;
end;

procedure TfrmSalesDashboard.InitializeQueries;
begin
  // Créer et configurer les requêtes
  FQuery1 := TFDQuery.Create(Self);
  FQuery1.Connection := DataModule1.FDConnection1;
  FQuery1.SQL.Text :=
    'SELECT Mois, MoisNum, SUM(Montant) AS Total ' +
    'FROM Ventes ' +
    'WHERE DateVente BETWEEN :StartDate AND :EndDate ' +
    'AND (:Region IS NULL OR Region = :Region) ' +
    'GROUP BY Mois, MoisNum ' +
    'ORDER BY MoisNum';

  FQuery2 := TFDQuery.Create(Self);
  FQuery2.Connection := DataModule1.FDConnection1;
  FQuery2.SQL.Text :=
    'SELECT Categorie, SUM(Montant) AS Total ' +
    'FROM Ventes ' +
    'WHERE DateVente BETWEEN :StartDate AND :EndDate ' +
    'AND (:Region IS NULL OR Region = :Region) ' +
    'GROUP BY Categorie';

  FQuery3 := TFDQuery.Create(Self);
  FQuery3.Connection := DataModule1.FDConnection1;
  FQuery3.SQL.Text :=
    'SELECT Region, SUM(Montant) AS Total ' +
    'FROM Ventes ' +
    'WHERE DateVente BETWEEN :StartDate AND :EndDate ' +
    'GROUP BY Region';

  FQuery4 := TFDQuery.Create(Self);
  FQuery4.Connection := DataModule1.FDConnection1;
  FQuery4.SQL.Text :=
    'SELECT ' +
    '  SUM(Montant) AS TotalVentes, ' +
    '  COUNT(DISTINCT ClientID) AS NbClients, ' +
    '  SUM(Montant) / COUNT(DISTINCT FactureID) AS TicketMoyen, ' +
    '  (SUM(Montant) - :PrevTotal) / :PrevTotal * 100 AS Croissance ' +
    'FROM Ventes ' +
    'WHERE DateVente BETWEEN :StartDate AND :EndDate ' +
    'AND (:Region IS NULL OR Region = :Region)';
end;

procedure TfrmSalesDashboard.InitializeCharts;
begin
  // Configurer le graphique des ventes
  with ChartSales do
  begin
    Title.Text.Clear;
    Title.Text.Add('Évolution des ventes');
    BottomAxis.Title.Caption := 'Mois';
    LeftAxis.Title.Caption := 'Montant (€)';

    // Créer une série de ligne
    var Series := TLineSeries.Create(ChartSales);
    AddSeries(Series);
    Series.Title := 'Ventes';
    Series.Pointer.Visible := True;
    Series.Marks.Visible := True;
    Series.Marks.Style := smsValue;
    Series.Marks.Format := '%2.0n €';

    OnClickSeries := ChartSalesClickSeries;
  end;

  // Configurer le graphique des catégories
  with ChartCategories do
  begin
    Title.Text.Clear;
    Title.Text.Add('Répartition par catégorie');

    // Créer une série en camembert
    var Series := TPieSeries.Create(ChartCategories);
    AddSeries(Series);
    Series.Marks.Visible := True;
    Series.Marks.Style := smsLabelPercent;

    OnClickSeries := ChartCategoriesClickSeries;
  end;

  // Configurer le graphique des régions
  with ChartRegions do
  begin
    Title.Text.Clear;
    Title.Text.Add('Ventes par région');
    BottomAxis.Title.Caption := 'Région';
    LeftAxis.Title.Caption := 'Montant (€)';

    // Créer une série d'histogramme
    var Series := TBarSeries.Create(ChartRegions);
    AddSeries(Series);
    Series.ColorEachPoint := True;
    Series.Marks.Visible := True;
    Series.Marks.Style := smsValue;
    Series.Marks.Format := '%2.0n €';

    OnClickSeries := ChartRegionsClickSeries;
  end;
end;

procedure TfrmSalesDashboard.SetupFilters;
begin
  // Configurer le filtre de période
  cbPeriod.Items.Clear;
  cbPeriod.Items.Add('Année en cours');
  cbPeriod.Items.Add('12 derniers mois');
  cbPeriod.Items.Add('Trimestre en cours');
  cbPeriod.Items.Add('Mois en cours');
  cbPeriod.Items.Add('Personnalisée');
  cbPeriod.ItemIndex := 0;

  // Configurer le filtre de région
  cbRegion.Items.Clear;
  cbRegion.Items.Add('Toutes les régions');

  // Charger les régions depuis la base de données
  var Query := TFDQuery.Create(nil);
  try
    Query.Connection := DataModule1.FDConnection1;
    Query.SQL.Text := 'SELECT DISTINCT Region FROM Ventes ORDER BY Region';
    Query.Open;

    while not Query.Eof do
    begin
      cbRegion.Items.Add(Query.FieldByName('Region').AsString);
      Query.Next;
    end;
  finally
    Query.Free;
  end;

  cbRegion.ItemIndex := 0;

  // Initialiser les dates pour la période par défaut (année en cours)
  DateFrom.Date := EncodeDate(YearOf(Now), 1, 1);
  DateTo.Date := Now;
end;

procedure TfrmSalesDashboard.cbPeriodChange(Sender: TObject);
begin
  // Ajuster les dates selon la période sélectionnée
  case cbPeriod.ItemIndex of
    0: // Année en cours
      begin
        DateFrom.Date := EncodeDate(YearOf(Now), 1, 1);
        DateTo.Date := Now;
      end;
    1: // 12 derniers mois
      begin
        DateFrom.Date := IncMonth(Now, -12);
        DateTo.Date := Now;
      end;
    2: // Trimestre en cours
      begin
        case MonthOf(Now) of
          1..3: DateFrom.Date := EncodeDate(YearOf(Now), 1, 1);
          4..6: DateFrom.Date := EncodeDate(YearOf(Now), 4, 1);
          7..9: DateFrom.Date := EncodeDate(YearOf(Now), 7, 1);
          else  DateFrom.Date := EncodeDate(YearOf(Now), 10, 1);
        end;
        DateTo.Date := Now;
      end;
    3: // Mois en cours
      begin
        DateFrom.Date := EncodeDate(YearOf(Now), MonthOf(Now), 1);
        DateTo.Date := Now;
      end;
    4: // Personnalisée
      begin
        // Ne rien faire, l'utilisateur définira les dates manuellement
      end;
  end;

  // Activer/désactiver les contrôles de date selon la période
  DateFrom.Enabled := cbPeriod.ItemIndex = 4;
  DateTo.Enabled := cbPeriod.ItemIndex = 4;

  // Mettre à jour l'affichage si ce n'est pas une période personnalisée
  if cbPeriod.ItemIndex <> 4 then
  begin
    LoadChartData;
    UpdateKPIs;
  end;
end;

procedure TfrmSalesDashboard.DateFromChange(Sender: TObject);
begin
  // Si la période est "Personnalisée", mettre à jour lors du changement de date
  if cbPeriod.ItemIndex = 4 then
  begin
    LoadChartData;
    UpdateKPIs;
  end;
end;

procedure TfrmSalesDashboard.DateToChange(Sender: TObject);
begin
  // Si la période est "Personnalisée", mettre à jour lors du changement de date
  if cbPeriod.ItemIndex = 4 then
  begin
    LoadChartData;
    UpdateKPIs;
  end;
end;

procedure TfrmSalesDashboard.cbRegionChange(Sender: TObject);
begin
  // Mettre à jour les données lors du changement de région
  LoadChartData;
  UpdateKPIs;
end;

procedure TfrmSalesDashboard.btnRefreshClick(Sender: TObject);
begin
  // Actualiser les données
  LoadChartData;
  UpdateKPIs;
end;

procedure TfrmSalesDashboard.LoadChartData;
var
  StartDate, EndDate: TDate;
  Region: string;
begin
  // Récupérer les paramètres de filtrage
  StartDate := DateFrom.Date;
  EndDate := DateTo.Date;

  if cbRegion.ItemIndex = 0 then
    Region := ''
  else
    Region := cbRegion.Text;

  // Mettre à jour le graphique des ventes
  FQuery1.Close;
  FQuery1.ParamByName('StartDate').AsDate := StartDate;
  FQuery1.ParamByName('EndDate').AsDate := EndDate;

  if Region = '' then
    FQuery1.ParamByName('Region').Clear
  else
    FQuery1.ParamByName('Region').AsString := Region;

  FQuery1.Open;

  TLineSeries(ChartSales.Series[0]).Clear;
  FQuery1.First;
  while not FQuery1.Eof do
  begin
    TLineSeries(ChartSales.Series[0]).AddXY(
      FQuery1.FieldByName('MoisNum').AsInteger,
      FQuery1.FieldByName('Total').AsFloat,
      FQuery1.FieldByName('Mois').AsString
    );
    FQuery1.Next;
  end;

  // Mettre à jour le graphique des catégories
  FQuery2.Close;
  FQuery2.ParamByName('StartDate').AsDate := StartDate;
  FQuery2.ParamByName('EndDate').AsDate := EndDate;

  if Region = '' then
    FQuery2.ParamByName('Region').Clear
  else
    FQuery2.ParamByName('Region').AsString := Region;

  FQuery2.Open;

  TPieSeries(ChartCategories.Series[0]).Clear;
  FQuery2.First;
  while not FQuery2.Eof do
  begin
    TPieSeries(ChartCategories.Series[0]).Add(
      FQuery2.FieldByName('Total').AsFloat,
      FQuery2.FieldByName('Categorie').AsString,
      clTeeColor
    );
    FQuery2.Next;
  end;

  // Mettre à jour le graphique des régions
  FQuery3.Close;
  FQuery3.ParamByName('StartDate').AsDate := StartDate;
  FQuery3.ParamByName('EndDate').AsDate := EndDate;
  FQuery3.Open;

  TBarSeries(ChartRegions.Series[0]).Clear;
  FQuery3.First;
  while not FQuery3.Eof do
  begin
    TBarSeries(ChartRegions.Series[0]).Add(
      FQuery3.FieldByName('Total').AsFloat,
      FQuery3.FieldByName('Region').AsString,
      clTeeColor
    );
    FQuery3.Next;
  end;

  // Mettre à jour le titre du tableau de bord pour refléter les filtres
  lblTitle.Caption := Format('Tableau de bord des ventes - %s au %s',
    [FormatDateTime('dd/mm/yyyy', StartDate),
     FormatDateTime('dd/mm/yyyy', EndDate)]);

  if Region <> '' then
    lblTitle.Caption := lblTitle.Caption + ' - Région: ' + Region;
end;

procedure TfrmSalesDashboard.UpdateKPIs;
var
  StartDate, EndDate, PrevStartDate, PrevEndDate: TDate;
  Region: string;
  PrevTotal: Double;
begin
  // Récupérer les paramètres de filtrage
  StartDate := DateFrom.Date;
  EndDate := DateTo.Date;

  if cbRegion.ItemIndex = 0 then
    Region := ''
  else
    Region := cbRegion.Text;

  // Calculer la période précédente (pour la croissance)
  // Si la période est d'un an, comparer à l'année précédente
  // Sinon, utiliser une période de même durée juste avant
  if YearOf(StartDate) = YearOf(EndDate) and MonthOf(StartDate) = 1 and DayOf(StartDate) = 1 then
  begin
    // Année complète
    PrevStartDate := EncodeDate(YearOf(StartDate) - 1, 1, 1);
    PrevEndDate := EncodeDate(YearOf(EndDate) - 1, 12, 31);
  end
  else
  begin
    // Période quelconque
    var Duration := EndDate - StartDate + 1;
    PrevEndDate := StartDate - 1;
    PrevStartDate := PrevEndDate - Duration + 1;
  end;

  // Calculer le total des ventes de la période précédente
  var QueryPrev := TFDQuery.Create(nil);
  try
    QueryPrev.Connection := DataModule1.FDConnection1;
    QueryPrev.SQL.Text :=
      'SELECT SUM(Montant) AS Total ' +
      'FROM Ventes ' +
      'WHERE DateVente BETWEEN :StartDate AND :EndDate ' +
      'AND (:Region IS NULL OR Region = :Region)';

    QueryPrev.ParamByName('StartDate').AsDate := PrevStartDate;
    QueryPrev.ParamByName('EndDate').AsDate := PrevEndDate;

    if Region = '' then
      QueryPrev.ParamByName('Region').Clear
    else
      QueryPrev.ParamByName('Region').AsString := Region;

    QueryPrev.Open;
    PrevTotal := QueryPrev.FieldByName('Total').AsFloat;

    // Si pas de ventes sur la période précédente, utiliser 1 pour éviter division par zéro
    if PrevTotal = 0 then
      PrevTotal := 1;
  finally
    QueryPrev.Free;
  end;

  // Mettre à jour les KPI
  FQuery4.Close;
  FQuery4.ParamByName('StartDate').AsDate := StartDate;
  FQuery4.ParamByName('EndDate').AsDate := EndDate;
  FQuery4.ParamByName('PrevTotal').AsFloat := PrevTotal;

  if Region = '' then
    FQuery4.ParamByName('Region').Clear
  else
    FQuery4.ParamByName('Region').AsString := Region;

  FQuery4.Open;

  if not FQuery4.IsEmpty then
  begin
    // Total des ventes
    lblTotalSales.Caption := FormatFloat('#,##0.00 €', FQuery4.FieldByName('TotalVentes').AsFloat);

    // Nombre de clients
    lblNbClients.Caption := FormatFloat('#,##0', FQuery4.FieldByName('NbClients').AsFloat);

    // Ticket moyen
    lblAvgTicket.Caption := FormatFloat('#,##0.00 €', FQuery4.FieldByName('TicketMoyen').AsFloat);

    // Croissance (avec code couleur)
    var Growth := FQuery4.FieldByName('Croissance').AsFloat;

    if Growth >= 0 then
    begin
      lblGrowth.Caption := '+' + FormatFloat('#,##0.0 %', Growth);
      lblGrowth.Font.Color := clGreen;
    end
    else
    begin
      lblGrowth.Caption := FormatFloat('#,##0.0 %', Growth);
      lblGrowth.Font.Color := clRed;
    end;
  end;
end;

procedure TfrmSalesDashboard.ChartSalesClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Month: string;
  MonthNum: Integer;
begin
  // Récupérer le mois cliqué
  Month := Series.XLabel[ValueIndex];
  MonthNum := ValueIndex + 1; // Supposons que les index commencent à 0 pour janvier

  // Afficher un message avec les détails
  ShowMessage(Format('Ventes pour %s: %m',
    [Month, Series.YValue[ValueIndex]]));

  // Ici, vous pourriez ouvrir un formulaire détaillé pour ce mois
  // ou filtrer davantage les données
end;

procedure TfrmSalesDashboard.ChartCategoriesClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Category: string;
begin
  // Récupérer la catégorie cliquée
  Category := Series.XLabel[ValueIndex];

  // Ouvrir un formulaire de détail pour cette catégorie
  ShowDetailForm(Category);
end;

procedure TfrmSalesDashboard.ChartRegionsClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Region: string;
begin
  // Récupérer la région cliquée
  Region := Series.XLabel[ValueIndex];

  // Mettre à jour le filtre de région
  cbRegion.ItemIndex := cbRegion.Items.IndexOf(Region);

  // Actualiser les données
  LoadChartData;
  UpdateKPIs;
end;

procedure TfrmSalesDashboard.ShowDetailForm(const Category: string);
begin
  // Créer et afficher un formulaire de détail
  with TfrmDetail.Create(Self) do
  try
    Caption := 'Détails de la catégorie ' + Category;

    // Passer les paramètres
    FilterCategory := Category;
    FilterStartDate := DateFrom.Date;
    FilterEndDate := DateTo.Date;

    if cbRegion.ItemIndex = 0 then
      FilterRegion := ''
    else
      FilterRegion := cbRegion.Text;

    // Initialiser et afficher
    Initialize;
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmSalesDashboard.btnExportPDFClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter en PDF';
    SaveDialog.Filter := 'Fichiers PDF (*.pdf)|*.pdf';
    SaveDialog.DefaultExt := 'pdf';

    if SaveDialog.Execute then
    begin
      // Note: Cette fonctionnalité nécessite souvent des composants tiers
      // comme FastReport ou une bibliothèque PDF

      // Exemple simple avec TeeChart Pro
      // ChartSales.ExportToPDF(SaveDialog.FileName);

      // Exemple alternatif avec imprimante PDF de Windows
      try
        // Sélectionner l'imprimante PDF
        var OldPrinter := Printer.PrinterIndex;
        var PDFPrinterIndex := -1;

        for var i := 0 to Printer.Printers.Count - 1 do
          if Pos('PDF', Printer.Printers[i]) > 0 then
          begin
            PDFPrinterIndex := i;
            Break;
          end;

        if PDFPrinterIndex = -1 then
        begin
          ShowMessage('Aucune imprimante PDF trouvée.');
          Exit;
        end;

        Printer.PrinterIndex := PDFPrinterIndex;

        // Imprimer les graphiques vers PDF
        ChartSales.Print;
        ChartCategories.Print;
        ChartRegions.Print;

        // Restaurer l'imprimante par défaut
        Printer.PrinterIndex := OldPrinter;

        ShowMessage('Exportation en PDF réussie!');
      except
        on E: Exception do
          ShowMessage('Erreur lors de l''exportation en PDF: ' + E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TfrmSalesDashboard.btnExportExcelClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Title := 'Exporter vers Excel';
    SaveDialog.Filter := 'Fichiers Excel (*.xlsx)|*.xlsx';
    SaveDialog.DefaultExt := 'xlsx';

    if SaveDialog.Execute then
    begin
      // Utiliser Excel Automation pour exporter
      try
        var Excel := CreateOleObject('Excel.Application');
        Excel.Visible := False;
        var Workbook := Excel.Workbooks.Add;
        var Sheet := Workbook.Worksheets[1];

        // Titre
        Sheet.Cells[1, 1] := lblTitle.Caption;
        Sheet.Cells[1, 1].Font.Bold := True;
        Sheet.Cells[1, 1].Font.Size := 14;

        // KPIs
        Sheet.Cells[3, 1] := 'Total des ventes:';
        Sheet.Cells[3, 2] := StrToFloat(StringReplace(lblTotalSales.Caption, ' €', '', [rfReplaceAll]));
        Sheet.Cells[3, 2].NumberFormat := '#,##0.00 €';

        Sheet.Cells[4, 1] := 'Nombre de clients:';
        Sheet.Cells[4, 2] := StrToInt(lblNbClients.Caption);

        Sheet.Cells[5, 1] := 'Ticket moyen:';
        Sheet.Cells[5, 2] := StrToFloat(StringReplace(lblAvgTicket.Caption, ' €', '', [rfReplaceAll]));
        Sheet.Cells[5, 2].NumberFormat := '#,##0.00 €';

        Sheet.Cells[6, 1] := 'Croissance:';
        Sheet.Cells[6, 2] := StrToFloat(StringReplace(lblGrowth.Caption, ' %', '', [rfReplaceAll])) / 100;
        Sheet.Cells[6, 2].NumberFormat := '+0.0%;-0.0%;0%';

        // Données des ventes par mois
        Sheet.Cells[8, 1] := 'VENTES PAR MOIS';
        Sheet.Cells[8, 1].Font.Bold := True;

        Sheet.Cells[9, 1] := 'Mois';
        Sheet.Cells[9, 2] := 'Montant';

        FQuery1.First;
        var Row := 10;
        while not FQuery1.Eof do
        begin
          Sheet.Cells[Row, 1] := FQuery1.FieldByName('Mois').AsString;
          Sheet.Cells[Row, 2] := FQuery1.FieldByName('Total').AsFloat;
          Sheet.Cells[Row, 2].NumberFormat := '#,##0.00 €';
          Inc(Row);
          FQuery1.Next;
        end;

        // Créer un graphique
        var ChartObject := Sheet.ChartObjects.Add(400, 50, 300, 200);
        var Chart := ChartObject.Chart;
        Chart.ChartType := 4; // xlLine
        Chart.SetSourceData(Sheet.Range['A9:B' + IntToStr(Row-1)]);
        Chart.HasTitle := True;
        Chart.ChartTitle.Text := 'Évolution des ventes';

        // Données des ventes par catégorie
        Sheet.Cells[8, 4] := 'VENTES PAR CATÉGORIE';
        Sheet.Cells[8, 4].Font.Bold := True;

        Sheet.Cells[9, 4] := 'Catégorie';
        Sheet.Cells[9, 5] := 'Montant';

        FQuery2.First;
        Row := 10;
        while not FQuery2.Eof do
        begin
          Sheet.Cells[Row, 4] := FQuery2.FieldByName('Categorie').AsString;
          Sheet.Cells[Row, 5] := FQuery2.FieldByName('Total').AsFloat;
          Sheet.Cells[Row, 5].NumberFormat := '#,##0.00 €';
          Inc(Row);
          FQuery2.Next;
        end;

        // Créer un graphique en camembert
        ChartObject := Sheet.ChartObjects.Add(400, 300, 300, 200);
        Chart := ChartObject.Chart;
        Chart.ChartType := 5; // xlPie
        Chart.SetSourceData(Sheet.Range['D9:E' + IntToStr(Row-1)]);
        Chart.HasTitle := True;
        Chart.ChartTitle.Text := 'Répartition par catégorie';

        // Enregistrer le fichier
        Workbook.SaveAs(SaveDialog.FileName);

        ShowMessage('Exportation vers Excel réussie!');
      except
        on E: Exception do
          ShowMessage('Erreur lors de l''exportation vers Excel: ' + E.Message);
      end;
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TfrmSalesDashboard.btnPrintClick(Sender: TObject);
var
  PrintDialog: TPrintDialog;
begin
  PrintDialog := TPrintDialog.Create(nil);
  try
    if PrintDialog.Execute then
    begin
      // Imprimer chaque graphique
      ChartSales.Print;
      ChartCategories.Print;
      ChartRegions.Print;

      ShowMessage('Impression terminée!');
    end;
  finally
    PrintDialog.Free;
  end;
end;

procedure TfrmSalesDashboard.ApplyBranding;
begin
  // Appliquer l'image de marque à tous les graphiques

  // Couleurs et styles
  var Charts: array[0..2] of TChart;
  Charts[0] := ChartSales;
  Charts[1] := ChartCategories;
  Charts[2] := ChartRegions;

  for var i := 0 to 2 do
  begin
    // Couleurs de fond
    Charts[i].Color := clWhite;
    Charts[i].BackWall.Color := clWhite;
    Charts[i].Gradient.Visible := False;

    // Police
    Charts[i].Title.Font.Name := 'Segoe UI';
    Charts[i].Title.Font.Size := 11;
    Charts[i].Title.Font.Style := [fsBold];

    Charts[i].Legend.Font.Name := 'Segoe UI';
    Charts[i].Legend.Font.Size := 9;

    // Grille
    if Charts[i] <> ChartCategories then // Pas de grille pour le camembert
    begin
      Charts[i].BottomAxis.Grid.Color := $00EEEEEE;
      Charts[i].LeftAxis.Grid.Color := $00EEEEEE;
    end;

    // Logo (version TeeChart Pro)
    // Charts[i].BackImage.Assign(imgLogo.Picture);
    // Charts[i].BackImageInside := False;
    // Charts[i].BackImageMode := pbCenter;
    // Charts[i].BackImageTransp := 70;
  end;

  // Palette de couleurs personnalisée pour les séries
  var Colors: array[0..5] of TColor;
  Colors[0] := RGB(0, 120, 200);    // Bleu primaire
  Colors[1] := RGB(240, 170, 0);    // Orange secondaire
  Colors[2] := RGB(0, 160, 100);    // Vert tertiaire
  Colors[3] := RGB(200, 60, 60);    // Rouge accent
  Colors[4] := RGB(120, 90, 170);   // Violet
  Colors[5] := RGB(80, 180, 160);   // Turquoise

  // Appliquer les couleurs aux séries
  if ChartSales.SeriesCount > 0 then
    ChartSales.Series[0].Color := Colors[0];

  // Pour les graphiques où chaque point a sa propre couleur
  if ChartRegions.SeriesCount > 0 then
  begin
    TBarSeries(ChartRegions.Series[0]).ColorEachPoint := False; // Pour appliquer notre palette
    ChartRegions.Series[0].Color := Colors[1];
  end;

  // Pour le camembert, définir explicitement les couleurs
  if ChartCategories.SeriesCount > 0 then
  begin
    for var i := 0 to Min(TPieSeries(ChartCategories.Series[0]).Count - 1, High(Colors)) do
      TPieSeries(ChartCategories.Series[0]).ValueColor[i] := Colors[i];
  end;

  // Styliser les panneaux KPI
  PanelTotalSales.BevelOuter := bvNone;
  PanelTotalSales.Color := RGB(240, 250, 255); // Bleu très clair
  PanelTotalSales.Border.Color := Colors[0];
  PanelTotalSales.Border.Width := 1;

  PanelNbClients.BevelOuter := bvNone;
  PanelNbClients.Color := RGB(255, 250, 240); // Orange très clair
  PanelNbClients.Border.Color := Colors[1];
  PanelNbClients.Border.Width := 1;

  PanelAvgTicket.BevelOuter := bvNone;
  PanelAvgTicket.Color := RGB(240, 255, 245); // Vert très clair
  PanelAvgTicket.Border.Color := Colors[2];
  PanelAvgTicket.Border.Width := 1;

  PanelGrowth.BevelOuter := bvNone;
  PanelGrowth.Color := RGB(255, 245, 245); // Rouge très clair
  PanelGrowth.Border.Color := Colors[3];
  PanelGrowth.Border.Width := 1;

  // Styliser les étiquettes KPI
  lblTotalSalesTitle.Font.Style := [fsBold];
  lblNbClientsTitle.Font.Style := [fsBold];
  lblAvgTicketTitle.Font.Style := [fsBold];
  lblGrowthTitle.Font.Style := [fsBold];

  lblTotalSales.Font.Size := 12;
  lblTotalSales.Font.Color := Colors[0];
  lblNbClients.Font.Size := 12;
  lblNbClients.Font.Color := Colors[1];
  lblAvgTicket.Font.Size := 12;
  lblAvgTicket.Font.Color := Colors[2];
  lblGrowth.Font.Size := 12;
  // lblGrowth.Font.Color sera défini dynamiquement selon la valeur
end;
```

## Conclusion

TeeChart est une bibliothèque de graphiques puissante qui vous permet de créer des visualisations professionnelles dans vos applications Delphi. Dans cette section, nous avons exploré :

1. **Les bases de TeeChart** : installation, types de graphiques, personnalisation.
2. **La connexion aux données** : chargement manuel et liaison à des bases de données.
3. **L'interactivité** : zoom, déplacement, gestion des clics sur les éléments.
4. **Les tableaux de bord** : combinaison de plusieurs graphiques et indicateurs.
5. **L'exportation et le partage** : vers PDF, Excel et l'impression.
6. **Les techniques avancées** : animations, optimisation des performances.
7. **L'organisation du code** : création d'unités réutilisables, intégration dans une application.

Pour créer des visualisations efficaces avec TeeChart, gardez à l'esprit ces principes :

### Choisir le bon type de graphique

- **Histogrammes** : pour comparer des valeurs entre différentes catégories
- **Courbes** : pour montrer des tendances ou des évolutions dans le temps
- **Camemberts** : pour illustrer des proportions d'un tout (limités à 5-7 segments)
- **Aires** : pour montrer l'accumulation de valeurs dans le temps
- **Nuages de points** : pour visualiser des corrélations entre deux variables

### Concevoir des tableaux de bord efficaces

1. **Clarté** : un tableau de bord doit communiquer clairement les informations clés
2. **Simplicité** : évitez de surcharger avec trop de graphiques ou de données
3. **Cohérence** : utilisez les mêmes styles et couleurs pour des concepts similaires
4. **Interactivité** : permettez aux utilisateurs d'explorer les données
5. **Filtrages** : offrez des options pour affiner les données selon différents critères

### Performances et optimisation

Pour les graphiques avec de grandes quantités de données, pensez à :
- Utiliser `TFastLineSeries` au lieu de `TLineSeries`
- Activer `BufferedDisplay` pour un rendu plus rapide
- Limiter le nombre de points avec `MaxPointsPerPage`
- Désactiver les fonctionnalités inutiles comme la 3D ou certaines étiquettes

Avec les techniques présentées dans cette section, vous disposez maintenant des connaissances nécessaires pour créer des visualisations de données professionnelles et interactives qui aideront vos utilisateurs à mieux comprendre et analyser leurs données, renforçant ainsi considérablement la valeur de vos applications Delphi.

⏭️ [Communication et services réseaux](/10-communication-et-services-reseaux/README.md)
