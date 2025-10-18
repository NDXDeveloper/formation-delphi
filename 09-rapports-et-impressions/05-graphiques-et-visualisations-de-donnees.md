🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.5 Graphiques et visualisations de données

## Introduction

Les graphiques et visualisations sont essentiels pour transformer des données brutes en informations compréhensibles. Ils permettent de révéler des tendances, des patterns et des insights qui seraient difficiles à percevoir dans des tableaux de chiffres. Delphi offre plusieurs solutions puissantes pour créer des visualisations de données professionnelles.

## Pourquoi visualiser les données ?

Les graphiques offrent de nombreux avantages :

- **Compréhension rapide** : une image vaut mille mots (et mille chiffres)
- **Identification de tendances** : visualisation immédiate des évolutions
- **Comparaisons** : facilite la comparaison entre différentes données
- **Communication** : transmet efficacement l'information aux décideurs
- **Détection d'anomalies** : repère les valeurs inhabituelles
- **Engagement** : rend les rapports plus attractifs et professionnels

## Solutions disponibles dans Delphi

### TChart (VCL natif)

**TChart** est le composant graphique inclus dans Delphi VCL.

**Avantages :**
- Gratuit et inclus dans Delphi
- Facile à utiliser
- Suffisant pour des graphiques simples
- Bonne intégration VCL

**Limitations :**
- Fonctionnalités limitées
- Apparence moins moderne
- Peu d'options d'interactivité

**Emplacement :** Onglet **Additional** de la palette de composants

### TeeChart

**TeeChart** est la bibliothèque graphique professionnelle de référence pour Delphi.

**Avantages :**
- Très complet : 60+ types de graphiques
- Apparence professionnelle
- Hautement personnalisable
- Interactivité avancée
- 3D et animations
- Export vers de nombreux formats
- Excellente documentation

**Éditions :**
- **Standard** : incluse dans certaines éditions de Delphi
- **Pro** : version complète avec toutes les fonctionnalités
- **Source Code** : code source complet

**Site officiel :** https://www.steema.com

### Autres alternatives

- **DevExpress Charts** : si vous utilisez la suite DevExpress
- **FastReport Charts** : intégrés dans FastReport
- **TMS Charts** : composants TMS Software
- **Google Charts / Chart.js** : via composants WebBrowser

## Installation et configuration de TeeChart

### Vérification de l'installation

TeeChart Standard est normalement inclus dans Delphi. Vérifiez :

1. Ouvrez la palette de composants
2. Cherchez l'onglet **TeeChart**
3. Vous devriez voir le composant **TChart**

### Mise à jour vers TeeChart Pro

Si vous souhaitez la version Pro :

1. Téléchargez depuis www.steema.com
2. Exécutez l'installeur
3. Sélectionnez votre version de Delphi
4. Les packages sont automatiquement compilés et installés

## Premier graphique avec TChart

### Préparation du formulaire

1. Créez un nouveau projet VCL
2. Sur le formulaire, placez un composant **TChart** (onglet TeeChart)
3. Redimensionnez-le pour occuper une bonne partie du formulaire
4. Nommez-le `ChartPrincipal`

### Ajout d'une série de données

#### Méthode visuelle

1. Double-cliquez sur le composant `TChart`
2. L'éditeur de graphique s'ouvre
3. Onglet **Series** → cliquez sur **Add**
4. Choisissez un type de série (Line, Bar, Pie, etc.)
5. Cliquez sur **OK**

#### Méthode par code

```pascal
uses
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs;

procedure TForm1.FormCreate(Sender: TObject);
var
  Serie: TLineSeries;
begin
  // Créer une série de type ligne
  Serie := TLineSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Ajouter des points
  Serie.AddXY(1, 10, 'Janvier');
  Serie.AddXY(2, 15, 'Février');
  Serie.AddXY(3, 12, 'Mars');
  Serie.AddXY(4, 18, 'Avril');
  Serie.AddXY(5, 22, 'Mai');

  // Personnalisation
  Serie.Title := 'Ventes 2024';
  Serie.Color := clBlue;
  Serie.Pointer.Visible := True;
end;
```

### Configuration de base

```pascal
procedure TForm1.ConfigurerGraphique;
begin
  // Titre du graphique
  ChartPrincipal.Title.Text.Text := 'Évolution des ventes';
  ChartPrincipal.Title.Font.Size := 14;
  ChartPrincipal.Title.Font.Style := [fsBold];

  // Légende
  ChartPrincipal.Legend.Visible := True;
  ChartPrincipal.Legend.Alignment := laBottom;

  // Axes
  ChartPrincipal.LeftAxis.Title.Caption := 'Montant (€)';
  ChartPrincipal.BottomAxis.Title.Caption := 'Mois';

  // Apparence générale
  ChartPrincipal.BackColor := clWhite;
  ChartPrincipal.Color := clWhite;
  ChartPrincipal.Gradient.Visible := False;
end;
```

## Types de graphiques

### Graphiques en ligne (Line Chart)

Parfaits pour visualiser des évolutions dans le temps.

```pascal
procedure TForm1.CreerGraphiqueLigne;
var
  Serie: TLineSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TLineSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Style de ligne
  Serie.LinePen.Width := 2;
  Serie.LinePen.Color := clBlue;

  // Points visibles
  Serie.Pointer.Visible := True;
  Serie.Pointer.Style := psCircle;
  Serie.Pointer.HorizSize := 4;
  Serie.Pointer.VertSize := 4;

  // Ajouter des données
  Serie.AddXY(1, 120);
  Serie.AddXY(2, 145);
  Serie.AddXY(3, 132);
  Serie.AddXY(4, 178);
  Serie.AddXY(5, 195);
  Serie.AddXY(6, 210);
end;
```

### Graphiques en barres (Bar Chart)

Idéaux pour comparer des valeurs entre différentes catégories.

```pascal
procedure TForm1.CreerGraphiqueBarres;
var
  Serie: TBarSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TBarSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Orientation horizontale ou verticale
  Serie.BarStyle := bsRectangle;
  Serie.MultiBar := mbNone;

  // Couleurs des barres
  Serie.ColorEachPoint := True;

  // Ajouter des données
  Serie.AddXY(0, 150, 'Paris', clRed);
  Serie.AddXY(1, 120, 'Lyon', clBlue);
  Serie.AddXY(2, 180, 'Marseille', clGreen);
  Serie.AddXY(3, 95, 'Toulouse', clYellow);

  // Afficher les valeurs sur les barres
  Serie.Marks.Visible := True;
  Serie.Marks.Style := smsValue;
end;
```

### Graphiques en camembert (Pie Chart)

Parfaits pour montrer des proportions ou des parts.

```pascal
procedure TForm1.CreerGraphiqueCamembert;
var
  Serie: TPieSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TPieSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Style 3D
  Serie.Circled := True;
  Serie.RotationAngle := 45;

  // Détacher une tranche
  Serie.ExplodeBiggest := 15; // Pourcentage de séparation

  // Ajouter des données
  Serie.AddXY(35, 'Électronique', clBlue);
  Serie.AddXY(25, 'Vêtements', clRed);
  Serie.AddXY(20, 'Alimentation', clGreen);
  Serie.AddXY(15, 'Maison', clYellow);
  Serie.AddXY(5, 'Autres', clGray);

  // Afficher les pourcentages
  Serie.Marks.Visible := True;
  Serie.Marks.Style := smsLabelPercent;
end;
```

### Graphiques en aires (Area Chart)

Visualisent les volumes cumulés dans le temps.

```pascal
procedure TForm1.CreerGraphiqueAire;
var
  Serie: TAreaSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TAreaSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Couleur et transparence
  Serie.AreaColor := clSkyBlue;
  Serie.AreaBrush.Style := bsSolid;
  Serie.Transparency := 50; // 0-100

  // Bordure
  Serie.AreaLinesPen.Visible := True;
  Serie.AreaLinesPen.Color := clBlue;
  Serie.AreaLinesPen.Width := 2;

  // Données
  Serie.AddXY(1, 10);
  Serie.AddXY(2, 15);
  Serie.AddXY(3, 13);
  Serie.AddXY(4, 18);
  Serie.AddXY(5, 22);
end;
```

### Graphiques en points (Point/Scatter)

Parfaits pour visualiser la corrélation entre deux variables.

```pascal
procedure TForm1.CreerGraphiquePoints;
var
  Serie: TPointSeries;
  i: Integer;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TPointSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Style des points
  Serie.Pointer.Style := psCircle;
  Serie.Pointer.HorizSize := 5;
  Serie.Pointer.VertSize := 5;
  Serie.Pointer.Brush.Color := clRed;
  Serie.Pointer.Pen.Color := clBlack;

  // Générer des points aléatoires
  Randomize;
  for i := 1 to 50 do
    Serie.AddXY(Random(100), Random(100));
end;
```

### Graphiques en bulles (Bubble Chart)

Affichent trois dimensions de données.

```pascal
procedure TForm1.CreerGraphiqueBulles;
var
  Serie: TBubbleSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TBubbleSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Ajouter des bulles (X, Y, Rayon)
  Serie.AddBubble(10, 20, 5, 'Produit A', clRed);
  Serie.AddBubble(30, 45, 10, 'Produit B', clBlue);
  Serie.AddBubble(50, 30, 15, 'Produit C', clGreen);
  Serie.AddBubble(70, 60, 8, 'Produit D', clYellow);

  // Afficher les étiquettes
  Serie.Marks.Visible := True;
end;
```

## Liaison aux données de base de données

### Liaison directe avec TDBChart

**TDBChart** est un composant qui se lie directement à un dataset.

```pascal
procedure TForm1.ConfigurerGraphiqueDB;
var
  Serie: TLineSeries;
begin
  // Préparer la requête
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT mois, SUM(montant) as total ' +
    'FROM ventes ' +
    'GROUP BY mois ' +
    'ORDER BY mois';
  FDQueryVentes.Open;

  // Créer la série
  Serie := TLineSeries.Create(DBChart1);
  Serie.ParentChart := DBChart1;

  // Lier aux champs de la base
  Serie.DataSource := FDQueryVentes;
  Serie.XLabelsSource := 'mois';
  Serie.YValues.ValueSource := 'total';
end;
```

### Remplissage manuel depuis une requête

Pour plus de contrôle, remplissez manuellement :

```pascal
procedure TForm1.RemplirGraphiqueDepuisDB;
var
  Serie: TBarSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  Serie := TBarSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;
  Serie.ColorEachPoint := True;

  // Ouvrir la requête
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT categorie, SUM(montant) as total ' +
    'FROM ventes ' +
    'GROUP BY categorie ' +
    'ORDER BY total DESC';
  FDQueryVentes.Open;

  // Parcourir les résultats
  while not FDQueryVentes.Eof do
  begin
    Serie.AddXY(
      Serie.Count,
      FDQueryVentes.FieldByName('total').AsFloat,
      FDQueryVentes.FieldByName('categorie').AsString
    );
    FDQueryVentes.Next;
  end;

  // Titre
  ChartPrincipal.Title.Text.Text := 'Ventes par catégorie';
end;
```

### Actualisation automatique

Actualisez le graphique lors des changements de données :

```pascal
procedure TForm1.FDQueryVentesAfterScroll(DataSet: TDataSet);
begin
  RemplirGraphiqueDepuisDB;
end;

procedure TForm1.btnActualiserClick(Sender: TObject);
begin
  FDQueryVentes.Refresh;
  RemplirGraphiqueDepuisDB;
end;
```

## Graphiques avec plusieurs séries

### Comparaison de plusieurs datasets

```pascal
procedure TForm1.CreerGraphiqueComparatif;
var
  Serie2023, Serie2024: TLineSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  // Série 2023
  Serie2023 := TLineSeries.Create(ChartPrincipal);
  Serie2023.ParentChart := ChartPrincipal;
  Serie2023.Title := 'Ventes 2023';
  Serie2023.Color := clBlue;
  Serie2023.AddXY(1, 120, 'Jan');
  Serie2023.AddXY(2, 145, 'Fév');
  Serie2023.AddXY(3, 132, 'Mar');
  Serie2023.AddXY(4, 178, 'Avr');

  // Série 2024
  Serie2024 := TLineSeries.Create(ChartPrincipal);
  Serie2024.ParentChart := ChartPrincipal;
  Serie2024.Title := 'Ventes 2024';
  Serie2024.Color := clRed;
  Serie2024.AddXY(1, 135, 'Jan');
  Serie2024.AddXY(2, 162, 'Fév');
  Serie2024.AddXY(3, 148, 'Mar');
  Serie2024.AddXY(4, 195, 'Avr');

  // Configuration
  ChartPrincipal.Title.Text.Text := 'Comparaison 2023 vs 2024';
  ChartPrincipal.Legend.Visible := True;
end;
```

### Types de séries mixtes

Combinez différents types de graphiques :

```pascal
procedure TForm1.CreerGraphiqueMixte;
var
  SerieBarres: TBarSeries;
  SerieLigne: TLineSeries;
begin
  ChartPrincipal.RemoveAllSeries;

  // Barres pour les ventes
  SerieBarres := TBarSeries.Create(ChartPrincipal);
  SerieBarres.ParentChart := ChartPrincipal;
  SerieBarres.Title := 'Ventes';
  SerieBarres.Color := clSkyBlue;
  SerieBarres.AddXY(1, 150, 'T1');
  SerieBarres.AddXY(2, 180, 'T2');
  SerieBarres.AddXY(3, 165, 'T3');
  SerieBarres.AddXY(4, 195, 'T4');

  // Ligne pour l'objectif
  SerieLigne := TLineSeries.Create(ChartPrincipal);
  SerieLigne.ParentChart := ChartPrincipal;
  SerieLigne.Title := 'Objectif';
  SerieLigne.Color := clRed;
  SerieLigne.LinePen.Width := 3;
  SerieLigne.LinePen.Style := psDash;
  SerieLigne.AddXY(1, 160);
  SerieLigne.AddXY(2, 160);
  SerieLigne.AddXY(3, 160);
  SerieLigne.AddXY(4, 160);

  ChartPrincipal.Title.Text.Text := 'Ventes vs Objectif';
end;
```

## Personnalisation avancée

### Axes personnalisés

```pascal
procedure TForm1.PersonnaliserAxes;
begin
  // Axe des ordonnées (gauche)
  ChartPrincipal.LeftAxis.Title.Caption := 'Montant (K€)';
  ChartPrincipal.LeftAxis.Title.Font.Style := [fsBold];
  ChartPrincipal.LeftAxis.Minimum := 0;
  ChartPrincipal.LeftAxis.Maximum := 200;
  ChartPrincipal.LeftAxis.Increment := 20;
  ChartPrincipal.LeftAxis.Grid.Visible := True;
  ChartPrincipal.LeftAxis.Grid.Color := clSilver;
  ChartPrincipal.LeftAxis.Grid.Style := psDot;

  // Axe des abscisses (bas)
  ChartPrincipal.BottomAxis.Title.Caption := 'Trimestre';
  ChartPrincipal.BottomAxis.LabelsAngle := 45; // Rotation des étiquettes
  ChartPrincipal.BottomAxis.LabelStyle := talMark;

  // Axe secondaire (droite)
  ChartPrincipal.RightAxis.Visible := True;
  ChartPrincipal.RightAxis.Title.Caption := 'Pourcentage (%)';
end;
```

### Couleurs et dégradés

```pascal
procedure TForm1.PersonnaliserCouleurs;
var
  Serie: TBarSeries;
begin
  Serie := TBarSeries(ChartPrincipal.Series[0]);

  // Dégradé sur les barres
  Serie.Gradient.Visible := True;
  Serie.Gradient.StartColor := clSkyBlue;
  Serie.Gradient.EndColor := clBlue;
  Serie.Gradient.Direction := gdTopBottom;

  // Fond du graphique avec dégradé
  ChartPrincipal.Gradient.Visible := True;
  ChartPrincipal.Gradient.StartColor := clWhite;
  ChartPrincipal.Gradient.EndColor := $00F0F0F0;
  ChartPrincipal.Gradient.Direction := gdTopBottom;

  // Bordures
  Serie.BarPen.Visible := True;
  Serie.BarPen.Color := clGray;
  Serie.BarPen.Width := 1;
end;
```

### Annotations et marqueurs

```pascal
uses
  VCLTee.TeeTools;

procedure TForm1.AjouterAnnotations;
var
  Annotation: TAnnotationTool;
  Ligne: TColorLineTool;
begin
  // Ajouter une annotation
  Annotation := TAnnotationTool.Create(ChartPrincipal);
  Annotation.ParentChart := ChartPrincipal;
  Annotation.Text := 'Point important';
  Annotation.Left := 150;
  Annotation.Top := 100;
  Annotation.Shape.Font.Color := clRed;

  // Ajouter une ligne de référence
  Ligne := TColorLineTool.Create(ChartPrincipal);
  Ligne.ParentChart := ChartPrincipal;
  Ligne.Axis := ChartPrincipal.LeftAxis;
  Ligne.Value := 150; // Valeur Y de la ligne
  Ligne.Pen.Color := clRed;
  Ligne.Pen.Style := psDash;
  Ligne.Pen.Width := 2;
end;
```

## Graphiques interactifs

### Zoom et navigation

```pascal
procedure TForm1.ActiverZoom;
begin
  // Zoom avec la souris
  ChartPrincipal.AllowZoom := True;
  ChartPrincipal.Zoom.Allow := True;
  ChartPrincipal.Zoom.Direction := tzdHorizontal; // ou tzdVertical, tzdBoth

  // Animation lors du zoom
  ChartPrincipal.Zoom.Animated := True;
  ChartPrincipal.Zoom.AnimatedSteps := 10;

  // Défilement (pan)
  ChartPrincipal.AllowPanning := pmBoth;
end;

procedure TForm1.btnResetZoomClick(Sender: TObject);
begin
  ChartPrincipal.UndoZoom;
end;
```

### Survol de la souris (Hover)

```pascal
uses
  VCLTee.TeeTools;

procedure TForm1.ActiverSurvol;
var
  Hint: TMarksTipTool;
begin
  // Afficher les valeurs au survol
  Hint := TMarksTipTool.Create(ChartPrincipal);
  Hint.ParentChart := ChartPrincipal;
  Hint.MouseAction := mtmMove; // Afficher au passage de la souris
  Hint.MouseDelay := 100; // Délai en millisecondes
end;

procedure TForm1.ChartPrincipalMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Clicked: Integer;
  Serie: TChartSeries;
begin
  // Détecter sur quel point la souris se trouve
  Serie := ChartPrincipal.Series[0];
  Clicked := Serie.Clicked(X, Y);

  if Clicked <> -1 then
  begin
    // Afficher l'info du point
    StatusBar1.SimpleText := Format('Point %d : %s = %.2f',
      [Clicked, Serie.Labels[Clicked], Serie.YValue[Clicked]]);
  end;
end;
```

### Clic sur les éléments

```pascal
procedure TForm1.ChartPrincipalClickSeries(Sender: TCustomChart; Series: TChartSeries;
  ValueIndex: Integer; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    ShowMessage(Format('Vous avez cliqué sur : %s' + #13#10 + 'Valeur : %.2f',
      [Series.Labels[ValueIndex], Series.YValue[ValueIndex]]));
  end;
end;
```

### Légende interactive

```pascal
procedure TForm1.ChartPrincipalClickLegend(Sender: TCustomChart; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  // Trouver quelle série a été cliquée dans la légende
  for i := 0 to ChartPrincipal.SeriesCount - 1 do
  begin
    if ChartPrincipal.Legend.Clicked(X, Y) = i then
    begin
      // Afficher/cacher la série
      ChartPrincipal.Series[i].Active := not ChartPrincipal.Series[i].Active;
      Break;
    end;
  end;
end;
```

## Graphiques 3D

### Activation de la 3D

```pascal
procedure TForm1.Activer3D;
begin
  // Activer le mode 3D
  ChartPrincipal.View3D := True;

  // Configuration 3D
  ChartPrincipal.Chart3DPercent := 50; // Profondeur (0-100)
  ChartPrincipal.Rotation := 345; // Rotation horizontale
  ChartPrincipal.Elevation := 345; // Élévation
  ChartPrincipal.Perspective := 15; // Perspective (0-100)
  ChartPrincipal.Orthogonal := False; // Projection orthogonale ou perspective

  // Effets visuels
  ChartPrincipal.Shadow.Visible := True;
  ChartPrincipal.Shadow.Color := clGray;
  ChartPrincipal.Shadow.HorizSize := 5;
  ChartPrincipal.Shadow.VertSize := 5;
end;
```

### Graphiques 3D de surface

```pascal
uses
  VCLTee.TeeSurfa;

procedure TForm1.CreerGraphiqueSurface;
var
  Serie: TSurfaceSeries;
  x, z: Integer;
begin
  ChartPrincipal.RemoveAllSeries;
  ChartPrincipal.View3D := True;

  Serie := TSurfaceSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Générer des données de surface
  for x := 0 to 10 do
    for z := 0 to 10 do
      Serie.AddXYZ(x, Sin(x) * Cos(z) * 10, z);

  // Style de la surface
  Serie.UseColorRange := True;
  Serie.UsePalette := True;
  Serie.PaletteStyle := psStrong;
end;
```

### Contrôle de rotation 3D

```pascal
procedure TForm1.TrackBarRotationChange(Sender: TObject);
begin
  ChartPrincipal.Rotation := TrackBarRotation.Position;
end;

procedure TForm1.TrackBarElevationChange(Sender: TObject);
begin
  ChartPrincipal.Elevation := TrackBarElevation.Position;
end;

procedure TForm1.CheckBox3DClick(Sender: TObject);
begin
  ChartPrincipal.View3D := CheckBox3D.Checked;
end;
```

## Graphiques animés

### Animation au chargement

```pascal
uses
  VCLTee.TeeAnimations;

procedure TForm1.AnimerGraphique;
var
  Animation: TTeeAnimation;
begin
  Animation := TTeeAnimation.Create(ChartPrincipal);
  Animation.Mode := amScroll; // ou amAll, amFade
  Animation.Steps := 30;
  Animation.Execute;
end;
```

### Mise à jour dynamique

```pascal
procedure TForm1.Timer1Timer(Sender: TObject);
var
  Serie: TLineSeries;
  NouvelleValeur: Double;
begin
  Serie := TLineSeries(ChartPrincipal.Series[0]);

  // Nouvelle valeur aléatoire
  NouvelleValeur := Random(50) + 50;

  // Ajouter le nouveau point
  Serie.AddXY(Serie.Count, NouvelleValeur);

  // Limiter le nombre de points affichés (fenêtre glissante)
  if Serie.Count > 20 then
    Serie.Delete(0);

  // Ajuster l'échelle
  ChartPrincipal.BottomAxis.Automatic := True;
end;
```

### Transition entre graphiques

```pascal
procedure TForm1.TransitionVersNouveauGraphique;
var
  OldChart: TBitmap;
begin
  // Capturer l'état actuel
  OldChart := TBitmap.Create;
  try
    OldChart.Width := ChartPrincipal.Width;
    OldChart.Height := ChartPrincipal.Height;
    ChartPrincipal.Draw(OldChart.Canvas, ChartPrincipal.ClientRect);

    // Modifier le graphique
    CreerNouveauGraphique;

    // Animer la transition (fade)
    // (nécessite une bibliothèque d'animation ou code personnalisé)
  finally
    OldChart.Free;
  end;
end;
```

## Tableaux de bord (Dashboards)

### Disposition multi-graphiques

```pascal
procedure TForm1.CreerTableauDeBord;
begin
  // Graphique 1 : Évolution mensuelle (en haut)
  CreerGraphiqueEvolution(Chart1);

  // Graphique 2 : Répartition par catégorie (en bas à gauche)
  CreerGraphiqueCategoriesGauche(Chart2);

  // Graphique 3 : Top 5 produits (en bas à droite)
  CreerGraphiqueTop5(Chart3);

  // Indicateurs numériques
  AfficherIndicateurs;
end;

procedure TForm1.CreerGraphiqueEvolution(Chart: TChart);
var
  Serie: TLineSeries;
begin
  Chart.RemoveAllSeries;
  Serie := TLineSeries.Create(Chart);
  Serie.ParentChart := Chart;
  // ... configuration
end;
```

### Indicateurs KPI

Créez des indicateurs clés de performance visuels :

```pascal
procedure TForm1.AfficherKPI(const Nom: string; Valeur, Objectif: Double);
var
  Pourcentage: Double;
  Couleur: TColor;
begin
  Pourcentage := (Valeur / Objectif) * 100;

  // Déterminer la couleur selon la performance
  if Pourcentage >= 100 then
    Couleur := clGreen
  else if Pourcentage >= 80 then
    Couleur := clYellow
  else
    Couleur := clRed;

  // Afficher sur un panel
  with TPanelKPI.Create(Self) do
  begin
    Parent := PanelKPIs;
    Caption := Format('%s'#13#10'%.0f / %.0f'#13#10'%.1f%%',
      [Nom, Valeur, Objectif, Pourcentage]);
    Color := Couleur;
    Font.Size := 12;
    Font.Style := [fsBold];
  end;
end;
```

### Graphiques de jauge

```pascal
uses
  VCLTee.TeeGauge;

procedure TForm1.CreerJauge;
var
  Jauge: TGaugeSeries;
begin
  ChartJauge.RemoveAllSeries;
  ChartJauge.View3D := False;
  ChartJauge.Legend.Visible := False;

  Jauge := TGaugeSeries.Create(ChartJauge);
  Jauge.ParentChart := ChartJauge;

  // Configuration
  Jauge.MinValue := 0;
  Jauge.MaxValue := 100;
  Jauge.Value := 75;

  // Zones colorées
  Jauge.TotalAngle := 180; // Demi-cercle
  Jauge.RotationAngle := 90;

  // Apparence
  Jauge.Frame.Visible := True;
  Jauge.Hand.Color := clRed;
  Jauge.Hand.Style := hsDiamond;
end;
```

## Export de graphiques

### Export en image

```pascal
uses
  VCLTee.TeePNG, VCLTee.TeeJPEG;

procedure TForm1.ExporterGraphiqueImage(const NomFichier: string);
var
  Extension: string;
begin
  Extension := LowerCase(ExtractFileExt(NomFichier));

  case Extension of
    '.bmp': ChartPrincipal.SaveToBitmapFile(NomFichier);

    '.png':
      begin
        var PNG := TTeePNGExport.Create;
        try
          PNG.Width := ChartPrincipal.Width;
          PNG.Height := ChartPrincipal.Height;
          PNG.Panel := ChartPrincipal;
          PNG.SaveToFile(NomFichier);
        finally
          PNG.Free;
        end;
      end;

    '.jpg', '.jpeg':
      begin
        var JPEG := TTeeJPEGExport.Create;
        try
          JPEG.Width := ChartPrincipal.Width;
          JPEG.Height := ChartPrincipal.Height;
          JPEG.Panel := ChartPrincipal;
          JPEG.SaveToFile(NomFichier);
        finally
          JPEG.Free;
        end;
      end;
  end;

  ShowMessage('Graphique exporté : ' + NomFichier);
end;
```

### Export vers le presse-papiers

```pascal
uses
  Vcl.Clipbrd;

procedure TForm1.CopierGraphiqueVersPressePapiers;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ChartPrincipal.Width;
    Bitmap.Height := ChartPrincipal.Height;
    ChartPrincipal.Draw(Bitmap.Canvas, ChartPrincipal.ClientRect);
    Clipboard.Assign(Bitmap);
    ShowMessage('Graphique copié dans le presse-papiers');
  finally
    Bitmap.Free;
  end;
end;
```

### Export en PDF

```pascal
uses
  VCLTee.TeePDFCanvas;

procedure TForm1.ExporterGraphiquePDF(const NomFichier: string);
var
  PDF: TTeePDFCanvas;
begin
  PDF := TTeePDFCanvas.Create;
  try
    PDF.Title := 'Graphique des ventes';
    ChartPrincipal.Draw(PDF, ChartPrincipal.ClientRect);
    PDF.SaveToFile(NomFichier);
    ShowMessage('Graphique exporté en PDF');
  finally
    PDF.Free;
  end;
end;
```

## Graphiques spécialisés

### Graphique de Gantt

Pour visualiser des plannings et chronologies.

```pascal
uses
  VCLTee.TeeGantt;

procedure TForm1.CreerGraphiqueGantt;
var
  Gantt: TGanttSeries;
begin
  ChartGantt.RemoveAllSeries;

  Gantt := TGanttSeries.Create(ChartGantt);
  Gantt.ParentChart := ChartGantt;

  // Ajouter des tâches (StartDate, EndDate, Y, Label)
  Gantt.AddGantt(EncodeDate(2024, 1, 1), EncodeDate(2024, 2, 15), 0, 'Conception');
  Gantt.AddGantt(EncodeDate(2024, 2, 1), EncodeDate(2024, 4, 30), 1, 'Développement');
  Gantt.AddGantt(EncodeDate(2024, 4, 15), EncodeDate(2024, 5, 31), 2, 'Tests');
  Gantt.AddGantt(EncodeDate(2024, 6, 1), EncodeDate(2024, 6, 15), 3, 'Déploiement');

  // Configuration
  ChartGantt.BottomAxis.DateTimeFormat := 'mmm yyyy';
  ChartGantt.LeftAxis.Inverted := True;
end;
```

### Graphique boursier (Candlestick)

```pascal
uses
  VCLTee.TeeCandl;

procedure TForm1.CreerGraphiqueBoursier;
var
  Candle: TCandleSeries;
begin
  ChartBourse.RemoveAllSeries;

  Candle := TCandleSeries.Create(ChartBourse);
  Candle.ParentChart := ChartBourse;

  // Ajouter des données (Date, Open, High, Low, Close)
  Candle.AddCandle(Date, 100, 105, 98, 103);
  Candle.AddCandle(Date + 1, 103, 108, 101, 107);
  Candle.AddCandle(Date + 2, 107, 110, 105, 106);

  // Style
  Candle.UpCloseColor := clGreen;
  Candle.DownCloseColor := clRed;
  Candle.CandleWidth := 5;
end;
```

### Carte thermique (Heatmap)

```pascal
procedure TForm1.CreerCarteThermique;
var
  i, j: Integer;
  Serie: TPointSeries;
  Valeur: Double;
begin
  ChartHeatmap.RemoveAllSeries;
  ChartHeatmap.View3D := False;

  Serie := TPointSeries.Create(ChartHeatmap);
  Serie.ParentChart := ChartHeatmap;

  // Grille de données avec couleurs selon la valeur
  for i := 0 to 10 do
    for j := 0 to 10 do
    begin
      Valeur := Random(100);
      Serie.AddXY(i, j);

      // Couleur selon la valeur
      if Valeur > 75 then
        Serie.ValueColor[Serie.Count - 1] := clRed
      else if Valeur > 50 then
        Serie.ValueColor[Serie.Count - 1] := clYellow
      else if Valeur > 25 then
        Serie.ValueColor[Serie.Count - 1] := clGreen
      else
        Serie.ValueColor[Serie.Count - 1] := clBlue;
    end;

  // Style des points (carrés)
  Serie.Pointer.Style := psRectangle;
  Serie.Pointer.HorizSize := 10;
  Serie.Pointer.VertSize := 10;
end;
```

## Graphiques temps réel

### Graphique de monitoring

```pascal
type
  TFormMonitoring = class(TForm)
    Chart1: TChart;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FSerieCPU: TFastLineSeries;
    FSerieMemoire: TFastLineSeries;
    FMaxPoints: Integer;
  end;

procedure TFormMonitoring.FormCreate(Sender: TObject);
begin
  FMaxPoints := 60; // 60 secondes d'historique

  // Série pour le CPU
  FSerieCPU := TFastLineSeries.Create(Chart1);
  FSerieCPU.ParentChart := Chart1;
  FSerieCPU.Title := 'CPU (%)';
  FSerieCPU.Color := clRed;
  FSerieCPU.LinePen.Width := 2;

  // Série pour la mémoire
  FSerieMemoire := TFastLineSeries.Create(Chart1);
  FSerieMemoire.ParentChart := Chart1;
  FSerieMemoire.Title := 'Mémoire (%)';
  FSerieMemoire.Color := clBlue;
  FSerieMemoire.LinePen.Width := 2;

  // Configuration du graphique
  Chart1.LeftAxis.Minimum := 0;
  Chart1.LeftAxis.Maximum := 100;
  Chart1.BottomAxis.Automatic := True;
  Chart1.Legend.Visible := True;

  // Démarrer le timer (intervalle 1 seconde)
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

procedure TFormMonitoring.Timer1Timer(Sender: TObject);
var
  CPU, Memoire: Double;
begin
  // Obtenir les valeurs (simulées ici)
  CPU := Random(30) + 20;
  Memoire := Random(20) + 40;

  // Ajouter les nouveaux points
  FSerieCPU.AddXY(FSerieCPU.Count, CPU);
  FSerieMemoire.AddXY(FSerieMemoire.Count, Memoire);

  // Limiter le nombre de points (fenêtre glissante)
  if FSerieCPU.Count > FMaxPoints then
  begin
    FSerieCPU.Delete(0);
    FSerieMemoire.Delete(0);
  end;

  // Mettre à jour l'affichage
  Chart1.Repaint;
end;
```

## Optimisation des performances

### Utilisation de TFastLineSeries

Pour de grandes quantités de données, utilisez les séries "Fast" :

```pascal
procedure TForm1.CreerGraphiquePerformant;
var
  Serie: TFastLineSeries;
  i: Integer;
begin
  ChartPrincipal.RemoveAllSeries;

  // TFastLineSeries est optimisée pour beaucoup de points
  Serie := TFastLineSeries.Create(ChartPrincipal);
  Serie.ParentChart := ChartPrincipal;

  // Désactiver les labels pour plus de performance
  ChartPrincipal.BottomAxis.Labels := False;

  // Ajouter beaucoup de points
  for i := 0 to 10000 do
    Serie.AddXY(i, Random(100));
end;
```

### Suspension du redessin

```pascal
procedure TForm1.AjouterBeaucoupDePoints;
var
  i: Integer;
begin
  // Suspendre le redessin pendant l'ajout
  ChartPrincipal.AutoRepaint := False;
  try
    for i := 0 to 1000 do
      Serie.AddXY(i, Random(100));
  finally
    ChartPrincipal.AutoRepaint := True;
    ChartPrincipal.Repaint; // Redessiner une seule fois
  end;
end;
```

### Simplification visuelle

```pascal
procedure TForm1.OptimiserAffichage;
begin
  // Désactiver les effets coûteux
  ChartPrincipal.View3D := False;
  ChartPrincipal.Gradient.Visible := False;
  ChartPrincipal.Shadow.Visible := False;

  // Réduire la qualité d'antialiasing
  ChartPrincipal.BufferedDisplay := False;
end;
```

## Accessibilité et responsive design

### Adaptation à la taille de la fenêtre

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  // Ajuster la taille des polices selon la taille de la fenêtre
  if Width < 800 then
  begin
    ChartPrincipal.Title.Font.Size := 10;
    ChartPrincipal.Legend.Font.Size := 8;
  end
  else
  begin
    ChartPrincipal.Title.Font.Size := 14;
    ChartPrincipal.Legend.Font.Size := 10;
  end;

  // Ajuster la position de la légende
  if Height < 400 then
    ChartPrincipal.Legend.Visible := False
  else
    ChartPrincipal.Legend.Visible := True;
end;
```

### Mode sombre

```pascal
procedure TForm1.AppliquerModeSombre;
begin
  ChartPrincipal.Color := $00202020; // Gris foncé
  ChartPrincipal.BackColor := $00202020;
  ChartPrincipal.Title.Font.Color := clWhite;
  ChartPrincipal.LeftAxis.Title.Font.Color := clWhite;
  ChartPrincipal.BottomAxis.Title.Font.Color := clWhite;
  ChartPrincipal.LeftAxis.LabelsFont.Color := clWhite;
  ChartPrincipal.BottomAxis.LabelsFont.Color := clWhite;
  ChartPrincipal.Legend.Font.Color := clWhite;
  ChartPrincipal.Legend.Color := $00303030;
  ChartPrincipal.LeftAxis.Grid.Color := $00404040;
  ChartPrincipal.BottomAxis.Grid.Color := $00404040;
end;
```

## Conseils et bonnes pratiques

### Choix du type de graphique

- **Lignes** : évolutions temporelles, tendances
- **Barres** : comparaisons entre catégories
- **Camemberts** : proportions (max 5-7 parts)
- **Aires** : volumes cumulés
- **Points** : corrélations, dispersions
- **Bulles** : 3 dimensions de données

### Design efficace

- **Clarté** : privilégiez la simplicité
- **Couleurs** : utilisez une palette cohérente (5-7 couleurs max)
- **Contraste** : assurez un bon contraste entre les éléments
- **Étiquettes** : titres et légendes explicites
- **Échelles** : commencez à zéro pour les barres
- **Animations** : utilisez-les avec parcimonie

### Performance

- **Limitation** : n'affichez pas plus de points que nécessaire
- **Échantillonnage** : réduisez le nombre de points pour les gros datasets
- **Séries Fast** : utilisez TFastLineSeries pour beaucoup de données
- **Actualisation** : ne rafraîchissez que quand nécessaire
- **Simplification** : désactivez la 3D et les effets pour les graphiques complexes

### Accessibilité

- **Couleurs** : ne comptez pas uniquement sur les couleurs (ajoutez des motifs, textures)
- **Contraste** : respectez les ratios de contraste WCAG
- **Taille** : polices suffisamment grandes (min 10pt)
- **Alternative** : fournissez les données sous forme de tableau également
- **Navigation** : permettez la navigation au clavier si possible

## Exemple complet : Tableau de bord analytique

```pascal
unit UTableauBord;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  FireDAC.Comp.Client, Data.DB;

type
  TFormTableauBord = class(TForm)
    ChartEvolution: TChart;
    ChartRepartition: TChart;
    ChartComparaison: TChart;
    ChartTop10: TChart;
    FDConnection1: TFDConnection;
    FDQueryVentes: TFDQuery;

    procedure FormCreate(Sender: TObject);
    procedure ChargerDonnees;
  private
    procedure CreerGraphiqueEvolution;
    procedure CreerGraphiqueRepartition;
    procedure CreerGraphiqueComparaison;
    procedure CreerGraphiqueTop10;
  end;

implementation

{$R *.dfm}

procedure TFormTableauBord.FormCreate(Sender: TObject);
begin
  // Configurer la connexion
  FDConnection1.Params.Values['Database'] := 'ma_base';
  FDConnection1.Connected := True;

  // Charger et afficher les données
  ChargerDonnees;
end;

procedure TFormTableauBord.ChargerDonnees;
begin
  CreerGraphiqueEvolution;
  CreerGraphiqueRepartition;
  CreerGraphiqueComparaison;
  CreerGraphiqueTop10;
end;

procedure TFormTableauBord.CreerGraphiqueEvolution;
var
  Serie: TLineSeries;
begin
  ChartEvolution.RemoveAllSeries;
  ChartEvolution.Title.Text.Text := 'Évolution du CA mensuel';

  Serie := TLineSeries.Create(ChartEvolution);
  Serie.ParentChart := ChartEvolution;
  Serie.Color := clBlue;
  Serie.LinePen.Width := 3;
  Serie.Pointer.Visible := True;

  // Charger les données
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT MONTH(date_vente) as mois, SUM(montant) as total ' +
    'FROM ventes ' +
    'WHERE YEAR(date_vente) = 2024 ' +
    'GROUP BY MONTH(date_vente) ' +
    'ORDER BY mois';
  FDQueryVentes.Open;

  while not FDQueryVentes.Eof do
  begin
    Serie.AddXY(
      FDQueryVentes.FieldByName('mois').AsInteger,
      FDQueryVentes.FieldByName('total').AsFloat,
      FormatSettings.ShortMonthNames[FDQueryVentes.FieldByName('mois').AsInteger]
    );
    FDQueryVentes.Next;
  end;
end;

procedure TFormTableauBord.CreerGraphiqueRepartition;
var
  Serie: TPieSeries;
begin
  ChartRepartition.RemoveAllSeries;
  ChartRepartition.Title.Text.Text := 'Répartition par catégorie';
  ChartRepartition.View3D := False;

  Serie := TPieSeries.Create(ChartRepartition);
  Serie.ParentChart := ChartRepartition;
  Serie.Marks.Style := smsLabelPercent;

  // Charger les données
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT categorie, SUM(montant) as total ' +
    'FROM ventes ' +
    'GROUP BY categorie ' +
    'ORDER BY total DESC';
  FDQueryVentes.Open;

  while not FDQueryVentes.Eof do
  begin
    Serie.AddXY(
      FDQueryVentes.FieldByName('total').AsFloat,
      FDQueryVentes.FieldByName('categorie').AsString
    );
    FDQueryVentes.Next;
  end;
end;

procedure TFormTableauBord.CreerGraphiqueComparaison;
var
  Serie2023, Serie2024: TBarSeries;
begin
  ChartComparaison.RemoveAllSeries;
  ChartComparaison.Title.Text.Text := '2023 vs 2024';

  // Série 2023
  Serie2023 := TBarSeries.Create(ChartComparaison);
  Serie2023.ParentChart := ChartComparaison;
  Serie2023.Title := '2023';
  Serie2023.Color := clSkyBlue;
  Serie2023.MultiBar := mbSide;

  // Série 2024
  Serie2024 := TBarSeries.Create(ChartComparaison);
  Serie2024.ParentChart := ChartComparaison;
  Serie2024.Title := '2024';
  Serie2024.Color := clNavy;
  Serie2024.MultiBar := mbSide;

  // Charger les données (exemple simplifié)
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT QUARTER(date_vente) as trimestre, ' +
    '       YEAR(date_vente) as annee, ' +
    '       SUM(montant) as total ' +
    'FROM ventes ' +
    'WHERE YEAR(date_vente) IN (2023, 2024) ' +
    'GROUP BY YEAR(date_vente), QUARTER(date_vente) ' +
    'ORDER BY trimestre, annee';
  FDQueryVentes.Open;

  // (logique de remplissage des deux séries)
end;

procedure TFormTableauBord.CreerGraphiqueTop10;
var
  Serie: THorizBarSeries;
begin
  ChartTop10.RemoveAllSeries;
  ChartTop10.Title.Text.Text := 'Top 10 produits';

  Serie := THorizBarSeries.Create(ChartTop10);
  Serie.ParentChart := ChartTop10;
  Serie.ColorEachPoint := True;
  Serie.Marks.Visible := True;

  // Charger les données
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text :=
    'SELECT nom_produit, SUM(quantite) as total ' +
    'FROM ventes v ' +
    'INNER JOIN produits p ON v.id_produit = p.id ' +
    'GROUP BY nom_produit ' +
    'ORDER BY total DESC ' +
    'LIMIT 10';
  FDQueryVentes.Open;

  while not FDQueryVentes.Eof do
  begin
    Serie.AddXY(
      FDQueryVentes.FieldByName('total').AsFloat,
      FDQueryVentes.FieldByName('nom_produit').AsString
    );
    FDQueryVentes.Next;
  end;
end;

end.
```

## Résumé

Les graphiques et visualisations de données sont essentiels pour transformer l'information brute en insights exploitables. Les points clés :

- **TeeChart** est la solution de référence pour Delphi avec plus de 60 types de graphiques
- **Types variés** : lignes, barres, camemberts, aires, points, bulles et bien d'autres
- **Liaison aux données** : intégration directe avec les datasets Delphi
- **Personnalisation complète** : couleurs, styles, axes, légendes entièrement configurables
- **Interactivité** : zoom, survol, clics et navigation améliorent l'expérience utilisateur
- **3D et animations** : ajoutent un aspect visuel professionnel
- **Export multiple** : PNG, JPEG, PDF, presse-papiers
- **Performance** : optimisations pour les grandes quantités de données
- **Tableaux de bord** : combinaison de plusieurs graphiques pour une vision globale

Maîtriser les graphiques dans Delphi permet de créer des applications analytiques puissantes qui facilitent la prise de décision basée sur les données.

⏭️ [Exportation vers différents formats (PDF, Excel, HTML...)](/09-rapports-et-impressions/06-exportation-vers-differents-formats.md)
