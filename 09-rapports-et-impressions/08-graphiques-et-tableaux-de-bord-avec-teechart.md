🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.8 Graphiques et tableaux de bord avec TeeChart

## Introduction

TeeChart est la solution de référence pour créer des tableaux de bord professionnels dans Delphi. Au-delà de simples graphiques isolés, cette section explore la création de tableaux de bord complets qui combinent plusieurs visualisations, offrent des interactions riches et présentent les données de manière cohérente et intuitive. Les tableaux de bord transforment les données brutes en insights actionnables pour la prise de décision.

## Qu'est-ce qu'un tableau de bord ?

Un tableau de bord (dashboard) est une interface visuelle qui présente les indicateurs clés de performance (KPI) et les métriques importantes d'une organisation ou d'un processus. Il se caractérise par :

- **Vue d'ensemble** : présentation synthétique des informations essentielles
- **Visualisations multiples** : combinaison de graphiques, jauges, indicateurs
- **Temps réel** : mise à jour régulière ou continue des données
- **Interactivité** : filtres, drill-down, navigation entre vues
- **Cohérence visuelle** : design harmonieux et professionnel
- **Accessibilité** : information claire et immédiatement compréhensible

## Architecture d'un tableau de bord

### Structure typique

Un tableau de bord se compose généralement de :

```
┌────────────────────────────────────────────────────────┐
│  En-tête : Titre, Date, Filtres                        │
├──────────────┬──────────────┬──────────────┬───────────┤
│  KPI 1       │  KPI 2       │  KPI 3       │  KPI 4    │
│  € 125 450   │  +15.3%      │  87 clients  │  ★★★★☆ │
├──────────────┴──────────────┴──────────────┴───────────┤
│  Graphique principal (évolution temporelle)            │
│  [Graphique en ligne]                                  │
├──────────────────────────┬─────────────────────────────┤
│  Répartition (camembert) │  Comparaison (barres)       │
│  [Pie chart]             │  [Bar chart]                │
├──────────────────────────┴─────────────────────────────┤
│  Tableau détaillé des données                          │
└────────────────────────────────────────────────────────┘
```

### Composants principaux

```pascal
type
  TFormTableauDeBord = class(TForm)
    // Zone d'en-tête
    PanelHeader: TPanel;
    LabelTitre: TLabel;
    DateEdit1: TDateTimePicker;
    DateEdit2: TDateTimePicker;
    ComboBoxFiltre: TComboBox;
    btnActualiser: TButton;

    // KPIs (Indicateurs)
    PanelKPIs: TPanel;
    PanelKPI1: TPanel;
    PanelKPI2: TPanel;
    PanelKPI3: TPanel;
    PanelKPI4: TPanel;

    // Graphiques
    ChartPrincipal: TChart;
    ChartRepartition: TChart;
    ChartComparaison: TChart;
    ChartTendance: TChart;

    // Tableau de données
    DBGrid1: TDBGrid;

    // Base de données
    FDConnection1: TFDConnection;
    FDQueryPrincipale: TFDQuery;
    DataSource1: TDataSource;

    // Timer pour actualisation
    Timer1: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure btnActualiserClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure ChargerDonnees;
    procedure MettreAJourKPIs;
    procedure MettreAJourGraphiques;
    procedure ConfigurerInterface;
  end;
```

## Configuration initiale du tableau de bord

### Mise en page du formulaire

```pascal
procedure TFormTableauDeBord.FormCreate(Sender: TObject);
begin
  // Configuration du formulaire
  Caption := 'Tableau de bord - Ventes';
  WindowState := wsMaximized;
  Position := poScreenCenter;

  // Configuration de l'interface
  ConfigurerInterface;

  // Connexion à la base de données
  FDConnection1.Params.Values['Database'] := 'ma_base';
  FDConnection1.Connected := True;

  // Chargement initial
  ChargerDonnees;
  MettreAJourKPIs;
  MettreAJourGraphiques;

  // Actualisation automatique toutes les 30 secondes
  Timer1.Interval := 30000;
  Timer1.Enabled := True;
end;

procedure TFormTableauDeBord.ConfigurerInterface;
begin
  // En-tête
  PanelHeader.Height := 60;
  PanelHeader.Align := alTop;
  PanelHeader.Color := $00404040;

  LabelTitre.Font.Size := 16;
  LabelTitre.Font.Style := [fsBold];
  LabelTitre.Font.Color := clWhite;
  LabelTitre.Caption := 'Tableau de Bord des Ventes';

  // KPIs
  PanelKPIs.Height := 100;
  PanelKPIs.Align := alTop;
  ConfigurerPanelKPI(PanelKPI1, 'Chiffre d''Affaires');
  ConfigurerPanelKPI(PanelKPI2, 'Évolution');
  ConfigurerPanelKPI(PanelKPI3, 'Clients');
  ConfigurerPanelKPI(PanelKPI4, 'Satisfaction');

  // Configuration des graphiques
  ConfigurerGraphiquePrincipal;
  ConfigurerGraphiqueRepartition;
  ConfigurerGraphiqueComparaison;
end;

procedure TFormTableauDeBord.ConfigurerPanelKPI(Panel: TPanel; const Titre: string);
begin
  Panel.BevelOuter := bvNone;
  Panel.Color := clWhite;
  Panel.BorderStyle := bsSingle;

  // Créer un label pour le titre
  var LabelTitre := TLabel.Create(Panel);
  LabelTitre.Parent := Panel;
  LabelTitre.Caption := Titre;
  LabelTitre.Font.Size := 9;
  LabelTitre.Font.Color := clGray;
  LabelTitre.Align := alTop;
  LabelTitre.AlignWithMargins := True;
  LabelTitre.Margins.SetBounds(10, 10, 10, 5);

  // Créer un label pour la valeur
  var LabelValeur := TLabel.Create(Panel);
  LabelValeur.Parent := Panel;
  LabelValeur.Name := 'LabelValeur' + Panel.Name;
  LabelValeur.Caption := '---';
  LabelValeur.Font.Size := 24;
  LabelValeur.Font.Style := [fsBold];
  LabelValeur.Align := alClient;
  LabelValeur.AlignWithMargins := True;
  LabelValeur.Margins.SetBounds(10, 5, 10, 10);
  LabelValeur.Alignment := taCenter;
  LabelValeur.Layout := tlCenter;
end;
```

## Création des KPIs (Indicateurs)

### Indicateurs numériques

```pascal
procedure TFormTableauDeBord.MettreAJourKPIs;
var
  CA, CAMoisPrecedent: Double;
  NombreClients: Integer;
  TauxSatisfaction: Double;
  Evolution: Double;
begin
  // Calculer le chiffre d'affaires
  FDConnection1.ExecSQL(
    'SELECT SUM(montant) as total FROM ventes ' +
    'WHERE MONTH(date_vente) = MONTH(CURDATE()) ' +
    'AND YEAR(date_vente) = YEAR(CURDATE())',
    procedure(DataSet: TDataSet)
    begin
      CA := DataSet.FieldByName('total').AsFloat;
    end
  );

  // CA du mois précédent pour comparaison
  FDConnection1.ExecSQL(
    'SELECT SUM(montant) as total FROM ventes ' +
    'WHERE MONTH(date_vente) = MONTH(CURDATE() - INTERVAL 1 MONTH)',
    procedure(DataSet: TDataSet)
    begin
      CAMoisPrecedent := DataSet.FieldByName('total').AsFloat;
    end
  );

  // Calculer l'évolution
  if CAMoisPrecedent > 0 then
    Evolution := ((CA - CAMoisPrecedent) / CAMoisPrecedent) * 100
  else
    Evolution := 0;

  // Nombre de clients actifs
  FDConnection1.ExecSQL(
    'SELECT COUNT(DISTINCT id_client) as total FROM ventes ' +
    'WHERE MONTH(date_vente) = MONTH(CURDATE())',
    procedure(DataSet: TDataSet)
    begin
      NombreClients := DataSet.FieldByName('total').AsInteger;
    end
  );

  // Taux de satisfaction (exemple)
  TauxSatisfaction := 4.2; // Récupéré d'une enquête

  // Mettre à jour l'affichage
  AfficherKPI(PanelKPI1, FormatFloat('#,##0 €', CA), clGreen);

  var CouleurEvolution := IfThen(Evolution >= 0, clGreen, clRed);
  var SigneEvolution := IfThen(Evolution >= 0, '+', '');
  AfficherKPI(PanelKPI2, SigneEvolution + FormatFloat('0.0', Evolution) + '%', CouleurEvolution);

  AfficherKPI(PanelKPI3, IntToStr(NombreClients), clBlue);
  AfficherKPI(PanelKPI4, FormatFloat('0.0', TauxSatisfaction) + '/5 ★', clOrange);
end;

procedure TFormTableauDeBord.AfficherKPI(Panel: TPanel; const Valeur: string; Couleur: TColor);
var
  LabelValeur: TLabel;
begin
  LabelValeur := Panel.FindComponent('LabelValeur' + Panel.Name) as TLabel;
  if Assigned(LabelValeur) then
  begin
    LabelValeur.Caption := Valeur;
    LabelValeur.Font.Color := Couleur;
  end;
end;
```

### Indicateurs avec icônes et tendances

```pascal
type
  TKPIAvecTendance = class(TPanel)
  private
    FValeur: Double;
    FValeurPrecedente: Double;
    FTitre: string;
    FImage: TImage;
    FLabelTitre: TLabel;
    FLabelValeur: TLabel;
    FLabelTendance: TLabel;
    FShapeTendance: TShape;
    procedure MettreAJour;
    function CalculerTendance: Double;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Initialiser(const Titre: string; Valeur, ValeurPrecedente: Double);
  end;

constructor TKPIAvecTendance.Create(AOwner: TComponent);
begin
  inherited;

  // Image/Icône
  FImage := TImage.Create(Self);
  FImage.Parent := Self;
  FImage.Align := alLeft;
  FImage.Width := 48;
  FImage.Center := True;
  FImage.Proportional := True;

  // Titre
  FLabelTitre := TLabel.Create(Self);
  FLabelTitre.Parent := Self;
  FLabelTitre.Align := alTop;
  FLabelTitre.Font.Size := 9;
  FLabelTitre.Font.Color := clGray;
  FLabelTitre.Margins.SetBounds(5, 5, 5, 0);

  // Valeur
  FLabelValeur := TLabel.Create(Self);
  FLabelValeur.Parent := Self;
  FLabelValeur.Align := alClient;
  FLabelValeur.Font.Size := 20;
  FLabelValeur.Font.Style := [fsBold];
  FLabelValeur.Alignment := taCenter;
  FLabelValeur.Layout := tlCenter;

  // Indicateur de tendance
  FShapeTendance := TShape.Create(Self);
  FShapeTendance.Parent := Self;
  FShapeTendance.Align := alBottom;
  FShapeTendance.Height := 8;

  // Label tendance
  FLabelTendance := TLabel.Create(Self);
  FLabelTendance.Parent := Self;
  FLabelTendance.Align := alBottom;
  FLabelTendance.Font.Size := 8;
  FLabelTendance.Alignment := taCenter;
end;

procedure TKPIAvecTendance.Initialiser(const Titre: string; Valeur, ValeurPrecedente: Double);
begin
  FTitre := Titre;
  FValeur := Valeur;
  FValeurPrecedente := ValeurPrecedente;
  MettreAJour;
end;

procedure TKPIAvecTendance.MettreAJour;
var
  Tendance: Double;
begin
  FLabelTitre.Caption := FTitre;
  FLabelValeur.Caption := FormatFloat('#,##0.00', FValeur);

  Tendance := CalculerTendance;

  if Tendance > 0 then
  begin
    FShapeTendance.Brush.Color := $0000C000; // Vert
    FLabelTendance.Caption := '▲ +' + FormatFloat('0.0', Tendance) + '%';
    FLabelTendance.Font.Color := clGreen;
  end
  else if Tendance < 0 then
  begin
    FShapeTendance.Brush.Color := $000000C0; // Rouge
    FLabelTendance.Caption := '▼ ' + FormatFloat('0.0', Tendance) + '%';
    FLabelTendance.Font.Color := clRed;
  end
  else
  begin
    FShapeTendance.Brush.Color := clGray;
    FLabelTendance.Caption := '= Stable';
    FLabelTendance.Font.Color := clGray;
  end;
end;

function TKPIAvecTendance.CalculerTendance: Double;
begin
  if FValeurPrecedente <> 0 then
    Result := ((FValeur - FValeurPrecedente) / FValeurPrecedente) * 100
  else
    Result := 0;
end;
```

## Graphiques intégrés au tableau de bord

### Graphique principal : Évolution temporelle

```pascal
uses
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs;

procedure TFormTableauDeBord.ConfigurerGraphiquePrincipal;
begin
  // Configuration générale
  ChartPrincipal.Title.Text.Text := 'Évolution des Ventes Mensuelles';
  ChartPrincipal.Title.Font.Size := 12;
  ChartPrincipal.Title.Font.Style := [fsBold];

  // Axes
  ChartPrincipal.LeftAxis.Title.Caption := 'Montant (€)';
  ChartPrincipal.BottomAxis.Title.Caption := 'Mois';

  // Légende
  ChartPrincipal.Legend.Visible := True;
  ChartPrincipal.Legend.Alignment := laBottom;
  ChartPrincipal.Legend.LegendStyle := lsSeries;

  // Apparence
  ChartPrincipal.View3D := False;
  ChartPrincipal.BackColor := clWhite;
  ChartPrincipal.Color := clWhite;
  ChartPrincipal.Gradient.Visible := False;

  // Grille
  ChartPrincipal.LeftAxis.Grid.Visible := True;
  ChartPrincipal.LeftAxis.Grid.Color := $00F0F0F0;
  ChartPrincipal.BottomAxis.Grid.Visible := False;
end;

procedure TFormTableauDeBord.ChargerGraphiquePrincipal;
var
  Serie2024, Serie2023: TLineSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  // Série année en cours
  Serie2024 := TLineSeries.Create(ChartPrincipal);
  Serie2024.ParentChart := ChartPrincipal;
  Serie2024.Title := '2024';
  Serie2024.Color := $00FF8040; // Bleu-orange
  Serie2024.LinePen.Width := 3;
  Serie2024.Pointer.Visible := True;
  Serie2024.Pointer.Style := psCircle;
  Serie2024.Pointer.HorizSize := 5;
  Serie2024.Pointer.VertSize := 5;

  // Série année précédente (comparaison)
  Serie2023 := TLineSeries.Create(ChartPrincipal);
  Serie2023.ParentChart := ChartPrincipal;
  Serie2023.Title := '2023';
  Serie2023.Color := clSilver;
  Serie2023.LinePen.Width := 2;
  Serie2023.LinePen.Style := psDash;

  // Charger les données
  FDQueryPrincipale.Close;
  FDQueryPrincipale.SQL.Text :=
    'SELECT MONTH(date_vente) as mois, YEAR(date_vente) as annee, ' +
    'SUM(montant) as total FROM ventes ' +
    'WHERE YEAR(date_vente) IN (2023, 2024) ' +
    'GROUP BY YEAR(date_vente), MONTH(date_vente) ' +
    'ORDER BY annee, mois';
  FDQueryPrincipale.Open;

  while not FDQueryPrincipale.Eof do
  begin
    var Mois := FDQueryPrincipale.FieldByName('mois').AsInteger;
    var Annee := FDQueryPrincipale.FieldByName('annee').AsInteger;
    var Total := FDQueryPrincipale.FieldByName('total').AsFloat;

    if Annee = 2024 then
      Serie2024.AddXY(Mois, Total, FormatSettings.ShortMonthNames[Mois])
    else
      Serie2023.AddXY(Mois, Total, FormatSettings.ShortMonthNames[Mois]);

    FDQueryPrincipale.Next;
  end;
end;
```

### Graphique de répartition (Camembert)

```pascal
procedure TFormTableauDeBord.ConfigurerGraphiqueRepartition;
begin
  ChartRepartition.Title.Text.Text := 'Répartition par Catégorie';
  ChartRepartition.Title.Font.Size := 11;
  ChartRepartition.Title.Font.Style := [fsBold];

  ChartRepartition.View3D := True;
  ChartRepartition.Chart3DPercent := 25;

  ChartRepartition.Legend.Visible := True;
  ChartRepartition.Legend.Alignment := laRight;
end;

procedure TFormTableauDeBord.ChargerGraphiqueRepartition;
var
  Serie: TPieSeries;
begin
  ChartRepartition.RemoveAllSeries;

  Serie := TPieSeries.Create(ChartRepartition);
  Serie.ParentChart := ChartRepartition;

  // Configuration
  Serie.Marks.Style := smsLabelPercent;
  Serie.Marks.Visible := True;
  Serie.CircleBackColor := clWhite;
  Serie.Circled := True;

  // Détacher la plus grande part
  Serie.ExplodeBiggest := 10;

  // Charger les données
  FDQueryPrincipale.Close;
  FDQueryPrincipale.SQL.Text :=
    'SELECT c.nom as categorie, SUM(v.montant) as total ' +
    'FROM ventes v ' +
    'INNER JOIN produits p ON v.id_produit = p.id ' +
    'INNER JOIN categories c ON p.id_categorie = c.id ' +
    'WHERE MONTH(v.date_vente) = MONTH(CURDATE()) ' +
    'GROUP BY c.nom ' +
    'ORDER BY total DESC';
  FDQueryPrincipale.Open;

  // Palette de couleurs personnalisée
  var Couleurs: array[0..6] of TColor := [
    $00FF8040, // Bleu-orange
    $004080FF, // Orange
    $0080FF40, // Vert-bleu
    $00FF4080, // Violet
    $0040FF80, // Vert
    $008040FF, // Rouge-violet
    $00FFFF40  // Cyan
  ];

  var Index := 0;
  while not FDQueryPrincipale.Eof do
  begin
    Serie.AddPie(
      FDQueryPrincipale.FieldByName('total').AsFloat,
      FDQueryPrincipale.FieldByName('categorie').AsString,
      Couleurs[Index mod Length(Couleurs)]
    );

    Inc(Index);
    FDQueryPrincipale.Next;
  end;
end;
```

### Graphique de comparaison (Barres)

```pascal
procedure TFormTableauDeBord.ConfigurerGraphiqueComparaison;
begin
  ChartComparaison.Title.Text.Text := 'Top 10 Produits';
  ChartComparaison.Title.Font.Size := 11;
  ChartComparaison.Title.Font.Style := [fsBold];

  ChartComparaison.View3D := False;
  ChartComparaison.Legend.Visible := False;

  ChartComparaison.BottomAxis.LabelStyle := talPointValue;
  ChartComparaison.LeftAxis.Title.Caption := 'Ventes';
end;

procedure TFormTableauDeBord.ChargerGraphiqueComparaison;
var
  Serie: THorizBarSeries;
begin
  ChartComparaison.RemoveAllSeries;

  Serie := THorizBarSeries.Create(ChartComparaison);
  Serie.ParentChart := ChartComparaison;

  // Configuration
  Serie.BarPen.Visible := True;
  Serie.BarPen.Color := clGray;
  Serie.ColorEachPoint := True;
  Serie.Gradient.Visible := True;
  Serie.Gradient.Direction := gdLeftRight;

  // Marques (valeurs sur les barres)
  Serie.Marks.Visible := True;
  Serie.Marks.Style := smsValue;

  // Charger les données
  FDQueryPrincipale.Close;
  FDQueryPrincipale.SQL.Text :=
    'SELECT p.nom as produit, SUM(v.quantite) as total ' +
    'FROM ventes v ' +
    'INNER JOIN produits p ON v.id_produit = p.id ' +
    'WHERE MONTH(v.date_vente) = MONTH(CURDATE()) ' +
    'GROUP BY p.nom ' +
    'ORDER BY total DESC ' +
    'LIMIT 10';
  FDQueryPrincipale.Open;

  while not FDQueryPrincipale.Eof do
  begin
    Serie.AddXY(
      FDQueryPrincipale.FieldByName('total').AsFloat,
      FDQueryPrincipale.FieldByName('produit').AsString
    );
    FDQueryPrincipale.Next;
  end;
end;
```

## Graphiques avancés pour tableaux de bord

### Graphique de jauge

```pascal
uses
  VCLTee.TeeGauge;

procedure TFormTableauDeBord.CreerJaugeObjectif;
var
  ChartJauge: TChart;
  Jauge: TGaugeSeries;
  ObjectifAtteint: Double;
begin
  // Créer le chart
  ChartJauge := TChart.Create(PanelKPIs);
  ChartJauge.Parent := PanelKPIs;
  ChartJauge.Width := 200;
  ChartJauge.Height := 100;
  ChartJauge.View3D := False;
  ChartJauge.Legend.Visible := False;
  ChartJauge.Title.Visible := False;

  // Créer la jauge
  Jauge := TGaugeSeries.Create(ChartJauge);
  Jauge.ParentChart := ChartJauge;

  // Configuration
  Jauge.MinValue := 0;
  Jauge.MaxValue := 100;

  // Calculer le pourcentage d'objectif atteint
  ObjectifAtteint := CalculerPourcentageObjectif;
  Jauge.Value := ObjectifAtteint;

  // Apparence
  Jauge.TotalAngle := 180; // Demi-cercle
  Jauge.RotationAngle := 90;

  // Couleur selon la performance
  if ObjectifAtteint >= 100 then
    Jauge.Hand.Color := clGreen
  else if ObjectifAtteint >= 80 then
    Jauge.Hand.Color := clYellow
  else
    Jauge.Hand.Color := clRed;

  // Style de l'aiguille
  Jauge.Hand.Style := hsDiamond;
  Jauge.Hand.InflateMargins := True;

  // Cadre et fond
  Jauge.Frame.Visible := True;
  Jauge.Frame.Color := clGray;
end;

function TFormTableauDeBord.CalculerPourcentageObjectif: Double;
var
  CAActuel, CAObjectif: Double;
begin
  // CA actuel
  FDConnection1.ExecSQL(
    'SELECT SUM(montant) as total FROM ventes WHERE MONTH(date_vente) = MONTH(CURDATE())',
    procedure(DataSet: TDataSet)
    begin
      CAActuel := DataSet.FieldByName('total').AsFloat;
    end
  );

  // CA objectif
  FDConnection1.ExecSQL(
    'SELECT objectif FROM objectifs_mensuels WHERE mois = MONTH(CURDATE())',
    procedure(DataSet: TDataSet)
    begin
      CAObjectif := DataSet.FieldByName('objectif').AsFloat;
    end
  );

  if CAObjectif > 0 then
    Result := (CAActuel / CAObjectif) * 100
  else
    Result := 0;
end;
```

### Graphique sparkline (mini-graphique)

```pascal
type
  TSparkline = class(TChart)
  private
    FSerie: TFastLineSeries;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AjouterValeurs(const Valeurs: array of Double);
    procedure Effacer;
  end;

constructor TSparkline.Create(AOwner: TComponent);
begin
  inherited;

  // Configuration minimaliste
  Self.View3D := False;
  Self.Legend.Visible := False;
  Self.Title.Visible := False;
  Self.LeftAxis.Visible := False;
  Self.BottomAxis.Visible := False;
  Self.BackColor := clWhite;
  Self.Color := clWhite;
  Self.BevelOuter := bvNone;
  Self.BorderStyle := bsNone;

  // Créer la série
  FSerie := TFastLineSeries.Create(Self);
  FSerie.ParentChart := Self;
  FSerie.LinePen.Width := 2;
  FSerie.LinePen.Color := $00FF8040;
  FSerie.Pointer.Visible := False;
end;

procedure TSparkline.AjouterValeurs(const Valeurs: array of Double);
var
  i: Integer;
begin
  FSerie.Clear;
  for i := Low(Valeurs) to High(Valeurs) do
    FSerie.AddY(Valeurs[i]);
end;

procedure TSparkline.Effacer;
begin
  FSerie.Clear;
end;

// Utilisation dans un KPI
procedure TFormTableauDeBord.AjouterSparklineAuKPI(Panel: TPanel);
var
  Sparkline: TSparkline;
  Valeurs: array[0..6] of Double;
begin
  // Créer le sparkline
  Sparkline := TSparkline.Create(Panel);
  Sparkline.Parent := Panel;
  Sparkline.Align := alBottom;
  Sparkline.Height := 30;

  // Charger les 7 derniers jours
  ChargerHistoriqueVentes7Jours(Valeurs);
  Sparkline.AjouterValeurs(Valeurs);
end;
```

### Graphique de tendance avec zone de confiance

```pascal
procedure TFormTableauDeBord.CreerGraphiqueTendance;
var
  ChartTendance: TChart;
  SerieDonnees: TLineSeries;
  SerieTendance: TLineSeries;
  SerieZoneConfiance: TAreaSeries;
begin
  ChartTendance := TChart.Create(Self);
  ChartTendance.Parent := PanelGraphiques;
  ChartTendance.Title.Text.Text := 'Tendance et Prévision';

  // Zone de confiance (aire)
  SerieZoneConfiance := TAreaSeries.Create(ChartTendance);
  SerieZoneConfiance.ParentChart := ChartTendance;
  SerieZoneConfiance.AreaColor := $00FFE0C0;
  SerieZoneConfiance.Transparency := 70;
  SerieZoneConfiance.AreaLinesPen.Visible := False;

  // Ligne de tendance
  SerieTendance := TLineSeries.Create(ChartTendance);
  SerieTendance.ParentChart := ChartTendance;
  SerieTendance.Title := 'Tendance';
  SerieTendance.Color := clRed;
  SerieTendance.LinePen.Style := psDash;
  SerieTendance.LinePen.Width := 2;

  // Données réelles
  SerieDonnees := TLineSeries.Create(ChartTendance);
  SerieDonnees.ParentChart := ChartTendance;
  SerieDonnees.Title := 'Données réelles';
  SerieDonnees.Color := clBlue;
  SerieDonnees.LinePen.Width := 3;
  SerieDonnees.Pointer.Visible := True;

  // Charger et calculer
  ChargerDonneesAvecTendance(SerieDonnees, SerieTendance, SerieZoneConfiance);
end;
```

## Interactivité dans les tableaux de bord

### Filtres globaux

```pascal
procedure TFormTableauDeBord.AppliquerFiltresGlobaux;
var
  DateDebut, DateFin: TDate;
  Categorie: string;
begin
  DateDebut := DateEdit1.Date;
  DateFin := DateEdit2.Date;
  Categorie := ComboBoxFiltre.Text;

  // Stocker les filtres globalement
  FFiltres.DateDebut := DateDebut;
  FFiltres.DateFin := DateFin;
  FFiltres.Categorie := Categorie;

  // Actualiser tous les composants
  Screen.Cursor := crHourGlass;
  try
    ChargerDonnees;
    MettreAJourKPIs;
    MettreAJourGraphiques;

    StatusBar1.SimpleText := Format('Période : %s - %s | Dernière màj : %s',
      [DateToStr(DateDebut), DateToStr(DateFin), TimeToStr(Now)]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormTableauDeBord.btnActualiserClick(Sender: TObject);
begin
  AppliquerFiltresGlobaux;
end;
```

### Drill-down depuis un graphique

```pascal
procedure TFormTableauDeBord.ChartRepartitionClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Categorie: string;
begin
  if Button = mbLeft then
  begin
    // Récupérer la catégorie cliquée
    Categorie := Series.Labels[ValueIndex];

    // Ouvrir une vue détaillée
    AfficherDetailCategorie(Categorie);
  end;
end;

procedure TFormTableauDeBord.AfficherDetailCategorie(const Categorie: string);
var
  FormDetail: TFormDetailCategorie;
begin
  FormDetail := TFormDetailCategorie.Create(Self);
  try
    FormDetail.Categorie := Categorie;
    FormDetail.DateDebut := FFiltres.DateDebut;
    FormDetail.DateFin := FFiltres.DateFin;
    FormDetail.ChargerDonnees;
    FormDetail.ShowModal;
  finally
    FormDetail.Free;
  end;
end;
```

### Synchronisation entre graphiques

```pascal
procedure TFormTableauDeBord.SynchroniserGraphiques;
var
  MoisSelectionne: Integer;
begin
  // Quand on clique sur un mois dans le graphique principal
  MoisSelectionne := ChartPrincipal.Series[0].XValues.Locate(PointClique);

  // Mettre à jour les autres graphiques pour ce mois
  ChargerGraphiqueRepartition(MoisSelectionne);
  ChargerGraphiqueComparaison(MoisSelectionne);
  ChargerTableauDetails(MoisSelectionne);
end;

procedure TFormTableauDeBord.ChartPrincipalClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    var Mois := Round(Series.XValue[ValueIndex]);
    SynchroniserGraphiques(Mois);

    // Indiquer visuellement la sélection
    MarquerPointSelectionne(Series, ValueIndex);
  end;
end;

procedure TFormTableauDeBord.MarquerPointSelectionne(Series: TChartSeries; Index: Integer);
var
  i: Integer;
begin
  // Réinitialiser tous les points
  for i := 0 to Series.Count - 1 do
  begin
    if Series is TLineSeries then
    begin
      TLineSeries(Series).Pointer.HorizSize := 5;
      TLineSeries(Series).Pointer.VertSize := 5;
    end;
  end;

  // Agrandir le point sélectionné
  if Series is TLineSeries then
  begin
    TLineSeries(Series).Pointer.HorizSize := 8;
    TLineSeries(Series).Pointer.VertSize := 8;
  end;
end;
```

### Tooltip personnalisé

```pascal
uses
  VCLTee.TeeTools;

procedure TFormTableauDeBord.ConfigurerTooltips;
var
  Tool: TMarksTipTool;
begin
  // ChartPrincipal
  Tool := TMarksTipTool.Create(ChartPrincipal);
  Tool.ParentChart := ChartPrincipal;
  Tool.MouseAction := mtmMove;
  Tool.MouseDelay := 100;
  Tool.Style := smsXY;

  // Personnaliser le format
  Tool.OnGetText := procedure(Sender: TMarksTipTool; var Text: string)
  begin
    var Serie := Sender.Series;
    var Index := Sender.ValueIndex;

    if Assigned(Serie) and (Index >= 0) then
    begin
      Text := Format('%s'#13#10'Montant : %s €'#13#10'Évolution : %s',
        [Serie.Labels[Index],
         FormatFloat('#,##0.00', Serie.YValue[Index]),
         CalculerEvolutionPoint(Serie, Index)]);
    end;
  end;
end;
```

## Actualisation en temps réel

### Actualisation automatique

```pascal
procedure TFormTableauDeBord.Timer1Timer(Sender: TObject);
begin
  if FActualisationAutomatique then
  begin
    ChargerDonnees;
    MettreAJourKPIs;
    MettreAJourGraphiques;

    LabelDerniereMiseAJour.Caption := 'Dernière màj : ' + TimeToStr(Now);
  end;
end;

procedure TFormTableauDeBord.CheckBoxAutoRefreshClick(Sender: TObject);
begin
  FActualisationAutomatique := CheckBoxAutoRefresh.Checked;
  Timer1.Enabled := FActualisationAutomatique;

  if FActualisationAutomatique then
    StatusBar1.SimpleText := 'Actualisation automatique activée (30s)'
  else
    StatusBar1.SimpleText := 'Actualisation manuelle';
end;
```

### Actualisation progressive

```pascal
procedure TFormTableauDeBord.ActualiserProgressivement;
begin
  // Actualiser les KPIs d'abord (plus rapide)
  TTask.Run(
    procedure
    begin
      var KPIs := CalculerKPIs;

      TThread.Synchronize(nil,
        procedure
        begin
          AfficherKPIs(KPIs);
        end
      );
    end
  );

  // Puis les graphiques
  TTask.Run(
    procedure
    begin
      var DonneesGraphiques := ChargerDonneesGraphiques;

      TThread.Synchronize(nil,
        procedure
        begin
          MettreAJourGraphiques(DonneesGraphiques);
        end
      );
    end
  );
end;
```

### Animation lors de l'actualisation

```pascal
uses
  VCLTee.TeeAnimations;

procedure TFormTableauDeBord.AnimerActualisation;
var
  Animation: TTeeAnimation;
begin
  // Charger les nouvelles données
  ChargerDonnees;

  // Animer les changements
  Animation := TTeeAnimation.Create(ChartPrincipal);
  try
    Animation.Mode := amScroll;
    Animation.Steps := 20;
    Animation.Execute;
  finally
    Animation.Free;
  end;
end;

procedure TFormTableauDeBord.AnimerKPI(Panel: TPanel; NouvelleValeur: Double);
var
  LabelValeur: TLabel;
  ValeurActuelle: Double;
  Timer: TTimer;
begin
  LabelValeur := Panel.FindComponent('LabelValeur' + Panel.Name) as TLabel;
  if not Assigned(LabelValeur) then Exit;

  // Animer la transition de valeur
  ValeurActuelle := StrToFloatDef(StringReplace(LabelValeur.Caption, ' ', '', [rfReplaceAll]), 0);

  Timer := TTimer.Create(Panel);
  Timer.Interval := 50;
  Timer.Tag := 0;
  Timer.OnTimer := procedure(Sender: TObject)
  var
    T: TTimer;
    Progress: Double;
    ValeurIntermediaire: Double;
  begin
    T := Sender as TTimer;
    Inc(T.Tag);

    Progress := T.Tag / 20; // 20 étapes

    if Progress >= 1 then
    begin
      LabelValeur.Caption := FormatFloat('#,##0', NouvelleValeur);
      T.Enabled := False;
      T.Free;
    end
    else
    begin
      ValeurIntermediaire := ValeurActuelle + (NouvelleValeur - ValeurActuelle) * Progress;
      LabelValeur.Caption := FormatFloat('#,##0', ValeurIntermediaire);
    end;
  end;
  Timer.Enabled := True;
end;
```

## Export du tableau de bord

### Export en image

```pascal
uses
  VCLTee.TeePNG;

procedure TFormTableauDeBord.ExporterTableauDeBordImage(const NomFichier: string);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    // Capturer tout le formulaire
    Bitmap.Width := ClientWidth;
    Bitmap.Height := ClientHeight;

    // Dessiner le contenu
    PaintTo(Bitmap.Canvas, 0, 0);

    // Sauvegarder
    Bitmap.SaveToFile(ChangeFileExt(NomFichier, '.bmp'));

    // Ou en PNG pour meilleure qualité
    var PNG := TTeePNGExport.Create;
    try
      PNG.SaveToFile(NomFichier);
    finally
      PNG.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;
```

### Export en PDF avec tous les graphiques

```pascal
uses
  frxClass, frxExportPDF;

procedure TFormTableauDeBord.ExporterTableauDeBordPDF(const NomFichier: string);
var
  Report: TfrxReport;
  Page: TfrxReportPage;
  PDFExport: TfrxPDFExport;

  procedure AjouterKPI(Y: Integer; const Titre, Valeur: string);
  var
    Memo: TfrxMemoView;
  begin
    Memo := TfrxMemoView.Create(Page);
    Memo.Left := 20;
    Memo.Top := Y;
    Memo.Width := 200;
    Memo.Height := 40;
    Memo.Font.Size := 12;
    Memo.Font.Style := [fsBold];
    Memo.Memo.Text := Format('%s: %s', [Titre, Valeur]);
  end;

  procedure AjouterGraphique(Chart: TChart; X, Y: Integer);
  var
    Picture: TfrxPictureView;
    Bitmap: TBitmap;
  begin
    Bitmap := Chart.TeeCreateBitmap(Chart.Color, Chart.ClientRect);
    try
      Picture := TfrxPictureView.Create(Page);
      Picture.Left := X;
      Picture.Top := Y;
      Picture.Width := 300;
      Picture.Height := 200;
      Picture.Picture.Assign(Bitmap);
    finally
      Bitmap.Free;
    end;
  end;

begin
  Report := TfrxReport.Create(nil);
  try
    // Créer une page
    Page := TfrxReportPage.Create(Report);
    Page.Orientation := poLandscape;

    // Ajouter titre
    var Titre := TfrxMemoView.Create(Page);
    Titre.Left := 20;
    Titre.Top := 20;
    Titre.Width := 700;
    Titre.Height := 30;
    Titre.Font.Size := 18;
    Titre.Font.Style := [fsBold];
    Titre.Memo.Text := 'Tableau de Bord - ' + DateToStr(Date);

    // Ajouter KPIs
    var Y := 70;
    AjouterKPI(Y, 'CA', GetKPIValue(PanelKPI1));
    Inc(Y, 50);
    AjouterKPI(Y, 'Évolution', GetKPIValue(PanelKPI2));

    // Ajouter graphiques
    AjouterGraphique(ChartPrincipal, 20, 200);
    AjouterGraphique(ChartRepartition, 340, 200);

    // Exporter
    PDFExport := TfrxPDFExport.Create(nil);
    try
      PDFExport.FileName := NomFichier;
      PDFExport.ShowDialog := False;
      Report.PrepareReport;
      Report.Export(PDFExport);
    finally
      PDFExport.Free;
    end;
  finally
    Report.Free;
  end;
end;
```

## Tableau de bord responsive

### Adaptation à la taille de fenêtre

```pascal
procedure TFormTableauDeBord.FormResize(Sender: TObject);
begin
  AdapterDisposition;
end;

procedure TFormTableauDeBord.AdapterDisposition;
begin
  // Largeur suffisante : disposition en colonnes
  if ClientWidth >= 1200 then
  begin
    DispositionLarge;
  end
  // Largeur moyenne : disposition compacte
  else if ClientWidth >= 800 then
  begin
    DispositionMoyenne;
  end
  // Petite largeur : disposition verticale
  else
  begin
    DispositionEtroite;
  end;
end;

procedure TFormTableauDeBord.DispositionLarge;
begin
  // KPIs en ligne
  PanelKPI1.Width := ClientWidth div 4;
  PanelKPI2.Width := ClientWidth div 4;
  PanelKPI3.Width := ClientWidth div 4;
  PanelKPI4.Width := ClientWidth div 4;

  // Graphiques en grille 2x2
  ChartPrincipal.Width := ClientWidth div 2;
  ChartPrincipal.Height := (ClientHeight - PanelHeader.Height - PanelKPIs.Height) div 2;

  ChartRepartition.Width := ClientWidth div 2;
  ChartRepartition.Height := ChartPrincipal.Height;

  // Positionner
  ChartRepartition.Left := ChartPrincipal.Width;
  ChartComparaison.Top := ChartPrincipal.Height;
end;

procedure TFormTableauDeBord.DispositionMoyenne;
begin
  // Disposition ajustée pour écran moyen
  // ...
end;

procedure TFormTableauDeBord.DispositionEtroite;
begin
  // Tout en vertical pour petit écran
  PanelKPI1.Width := ClientWidth;
  PanelKPI2.Width := ClientWidth;

  ChartPrincipal.Width := ClientWidth;
  ChartPrincipal.Align := alTop;
  // ...
end;
```

## Thèmes et personnalisation

### Thème sombre

```pascal
procedure TFormTableauDeBord.AppliquerThemeSombre;
begin
  // Formulaire
  Color := $00202020;

  // Panneaux
  PanelHeader.Color := $00101010;
  PanelKPIs.Color := $00202020;

  // KPIs
  PanelKPI1.Color := $00303030;
  PanelKPI2.Color := $00303030;

  // Labels
  LabelTitre.Font.Color := clWhite;

  // Graphiques
  ConfigurerGraphiqueThemeSombre(ChartPrincipal);
  ConfigurerGraphiqueThemeSombre(ChartRepartition);
  ConfigurerGraphiqueThemeSombre(ChartComparaison);
end;

procedure TFormTableauDeBord.ConfigurerGraphiqueThemeSombre(Chart: TChart);
begin
  Chart.Color := $00303030;
  Chart.BackColor := $00303030;
  Chart.Title.Font.Color := clWhite;
  Chart.LeftAxis.Title.Font.Color := clWhite;
  Chart.BottomAxis.Title.Font.Color := clWhite;
  Chart.LeftAxis.LabelsFont.Color := clWhite;
  Chart.BottomAxis.LabelsFont.Color := clWhite;
  Chart.Legend.Font.Color := clWhite;
  Chart.Legend.Color := $00404040;
  Chart.LeftAxis.Grid.Color := $00404040;
  Chart.BottomAxis.Grid.Color := $00404040;
end;
```

### Palette de couleurs personnalisée

```pascal
type
  TPaletteTableauBord = class
  public
    class function Couleur1: TColor;
    class function Couleur2: TColor;
    class function Couleur3: TColor;
    class function Couleur4: TColor;
    class function Couleur5: TColor;
    class function ObtenirCouleur(Index: Integer): TColor;
  end;

class function TPaletteTableauBord.Couleur1: TColor;
begin
  Result := $00FF8040; // Bleu-orange principal
end;

class function TPaletteTableauBord.Couleur2: TColor;
begin
  Result := $004080FF; // Orange complémentaire
end;

class function TPaletteTableauBord.ObtenirCouleur(Index: Integer): TColor;
const
  Palette: array[0..6] of TColor = (
    $00FF8040, $004080FF, $0080FF40, $00FF4080,
    $0040FF80, $008040FF, $00FFFF40
  );
begin
  Result := Palette[Index mod Length(Palette)];
end;

// Utilisation
procedure TFormTableauBord.AppliquerPalette;
var
  i: Integer;
begin
  for i := 0 to ChartRepartition.SeriesCount - 1 do
  begin
    if ChartRepartition.Series[i] is TPieSeries then
    begin
      // La palette sera appliquée automatiquement
      TPieSeries(ChartRepartition.Series[i]).ColorEachSlice := True;
    end;
  end;
end;
```

## Conseils et bonnes pratiques

### Design de tableaux de bord

- **Hiérarchie visuelle** : les informations les plus importantes en haut et en grand
- **Espace blanc** : aérez pour améliorer la lisibilité
- **Couleurs cohérentes** : utilisez une palette limitée (5-7 couleurs max)
- **Typo lisible** : polices claires, tailles suffisantes
- **Alignement** : grille régulière pour un aspect professionnel

### Performance

- **Optimisation SQL** : requêtes efficaces avec index appropriés
- **Cache** : mémorisez les calculs coûteux
- **Actualisation intelligente** : ne rafraîchissez que ce qui change
- **Chargement asynchrone** : ne bloquez pas l'interface
- **Pagination** : limitez les données affichées

### Utilisabilité

- **Clarté** : chaque métrique doit être immédiatement compréhensible
- **Contexte** : toujours donner une référence (objectif, période précédente)
- **Interactivité** : permettez l'exploration des données
- **Export** : possibilité de sauvegarder et partager
- **Aide** : explications des indicateurs disponibles

### Maintenance

- **Modularité** : composants réutilisables
- **Configuration** : paramètres externalisés
- **Logging** : journalisez les erreurs et performances
- **Tests** : validez avec différents jeux de données
- **Documentation** : expliquez les calculs et sources

## Résumé

Les tableaux de bord avec TeeChart offrent une solution complète pour visualiser et analyser les données. Les points clés :

- **Architecture structurée** : KPIs, graphiques multiples, filtres, navigation
- **TeeChart** : bibliothèque puissante avec 60+ types de graphiques
- **Interactivité** : drill-down, filtres, synchronisation entre vues
- **Temps réel** : actualisation automatique et progressive
- **Personnalisation** : thèmes, couleurs, disposition adaptative
- **Export** : PDF, images pour partage et archivage
- **Performance** : optimisations pour réactivité
- **Design professionnel** : apparence soignée et intuitive

Maîtriser la création de tableaux de bord avec TeeChart permet de transformer les données brutes en outils d'aide à la décision puissants et visuellement attractifs, essentiels pour le pilotage d'activité moderne.

⏭️ [Communication et services réseaux](/10-communication-et-services-reseaux/README.md)
