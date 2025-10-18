🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.7 Rapports interactifs

## Introduction

Les rapports interactifs transforment des documents statiques en outils d'exploration de données dynamiques. Contrairement aux rapports traditionnels qui présentent simplement l'information, les rapports interactifs permettent aux utilisateurs de naviguer, filtrer, approfondir et personnaliser l'affichage des données en temps réel. Cette approche améliore considérablement l'expérience utilisateur et la valeur analytique des rapports.

## Qu'est-ce qu'un rapport interactif ?

Un rapport interactif est un rapport qui offre des fonctionnalités permettant à l'utilisateur d'interagir avec le contenu :

- **Navigation** : liens cliquables, signets, table des matières
- **Drill-down** : exploration progressive des données (du général au détail)
- **Drill-through** : passage d'un rapport à un autre
- **Filtres dynamiques** : sélection des données à afficher
- **Tri interactif** : réorganisation des données par l'utilisateur
- **Expansion/Réduction** : affichage/masquage de sections
- **Paramètres** : personnalisation du rapport avant affichage
- **Recherche** : localisation rapide d'informations
- **Export sélectif** : choix des données à exporter

## Avantages des rapports interactifs

### Pour les utilisateurs

- **Exploration autonome** : trouvent leurs propres réponses
- **Gain de temps** : pas besoin de générer plusieurs rapports
- **Personnalisation** : adaptent le rapport à leurs besoins
- **Découverte** : révèlent des insights cachés
- **Efficacité** : accès direct aux informations pertinentes

### Pour les développeurs

- **Moins de rapports à créer** : un rapport interactif remplace plusieurs rapports statiques
- **Maintenance simplifiée** : modifications centralisées
- **Satisfaction utilisateur** : fonctionnalités appréciées
- **Valeur ajoutée** : différenciation de l'application

## Configuration de base dans FastReport

### Activation de l'interactivité

FastReport offre un excellent support pour les rapports interactifs.

```pascal
procedure TForm1.ConfigurerRapportInteractif;
begin
  // Charger le rapport
  frxReport1.LoadFromFile('RapportInteractif.fr3');

  // Options d'aperçu pour l'interactivité
  frxReport1.PreviewOptions.AllowEdit := False; // Désactiver l'édition
  frxReport1.PreviewOptions.Buttons := [pbPrint, pbSave, pbZoom, pbFind, pbOutline];

  // Activer la barre de navigation
  frxReport1.PreviewOptions.ShowOutline := True; // Table des matières

  // Options de recherche
  frxReport1.PreviewOptions.ShowCaptions := True;

  // Mode plein écran disponible
  frxReport1.PreviewOptions.ShowInTaskbar := True;

  // Afficher le rapport
  frxReport1.ShowReport;
end;
```

### Aperçu personnalisé

Créez un formulaire d'aperçu personnalisé pour plus de contrôle.

```pascal
type
  TFormApercuInteractif = class(TForm)
    frxPreview: TfrxPreview;
    PanelTop: TPanel;
    btnNavigationPrecedent: TButton;
    btnNavigationSuivant: TButton;
    ComboBoxZoom: TComboBox;
    EditRecherche: TEdit;
    btnRechercher: TButton;
    TreeViewSignets: TTreeView;

    procedure btnNavigationPrecedentClick(Sender: TObject);
    procedure btnNavigationSuivantClick(Sender: TObject);
    procedure ComboBoxZoomChange(Sender: TObject);
    procedure btnRechercherClick(Sender: TObject);
  end;

procedure TFormApercuInteractif.FormCreate(Sender: TObject);
begin
  // Lier le preview au rapport
  frxReport1.Preview := frxPreview;
  frxReport1.ShowReport(True); // True = mode modal

  // Remplir les options de zoom
  ComboBoxZoom.Items.AddStrings(['50%', '75%', '100%', '125%', '150%', '200%']);
  ComboBoxZoom.ItemIndex := 2; // 100%
end;

procedure TFormApercuInteractif.ComboBoxZoomChange(Sender: TObject);
begin
  case ComboBoxZoom.ItemIndex of
    0: frxPreview.Zoom := 0.5;
    1: frxPreview.Zoom := 0.75;
    2: frxPreview.Zoom := 1.0;
    3: frxPreview.Zoom := 1.25;
    4: frxPreview.Zoom := 1.5;
    5: frxPreview.Zoom := 2.0;
  end;
end;

procedure TFormApercuInteractif.btnRechercherClick(Sender: TObject);
begin
  frxPreview.Find(EditRecherche.Text);
end;
```

## Navigation et signets

### Création de signets

Les signets permettent une navigation rapide dans le rapport.

**Dans le designer FastReport :**

1. Sélectionnez un objet (titre de section, par exemple)
2. Dans l'Inspecteur d'objets, propriété **Bookmark**
3. Entrez une expression : `<frxDBDataset."nom_categorie">`

**Par code :**

```pascal
// Dans l'événement OnBeforePrint du band ou de l'objet
begin
  // Créer un signet avec le nom de la catégorie
  Engine.AddBookmark(<frxDBDataset."nom_categorie">);
end;
```

### Table des matières automatique

FastReport génère automatiquement une table des matières basée sur les signets.

```pascal
procedure TForm1.CreerRapportAvecTDM;
begin
  // Le rapport doit avoir des signets configurés
  frxReport1.LoadFromFile('RapportAvecSignets.fr3');

  // Activer l'affichage de la table des matières
  frxReport1.PreviewOptions.ShowOutline := True;
  frxReport1.PreviewOptions.OutlineVisible := True;
  frxReport1.PreviewOptions.OutlineWidth := 200; // Largeur en pixels

  frxReport1.ShowReport;
end;
```

### Navigation hiérarchique

Créez une structure de navigation à plusieurs niveaux.

**Exemple : Structure Région → Ville → Magasin**

```pascal
// Dans le Group Header Région
procedure RegionHeaderOnBeforePrint;
begin
  Engine.AddBookmark(<frxDBDataset."nom_region">, 0); // Niveau 0
end;

// Dans le Group Header Ville
procedure VilleHeaderOnBeforePrint;
begin
  Engine.AddBookmark(<frxDBDataset."nom_ville">, 1); // Niveau 1
end;

// Dans le Group Header Magasin
procedure MagasinHeaderOnBeforePrint;
begin
  Engine.AddBookmark(<frxDBDataset."nom_magasin">, 2); // Niveau 2
end;
```

### Hyperliens internes

Créez des liens vers d'autres parties du rapport.

**Dans le designer :**

1. Sélectionnez un objet Memo
2. Propriété **Hyperlink.Kind** = `hkURL` ou `hkBookmark`
3. Propriété **Hyperlink.Value** :
   - Pour une URL : `'http://www.exemple.com'`
   - Pour un signet : `'#NomDuSignet'`
   - Pour un signet dynamique : `'#' + <frxDBDataset."nom_categorie">`

**Exemple de navigation :**

```
┌─ Page 1 : Sommaire ─────────────────────┐
│ Cliquez sur une catégorie :             │
│ • [Électronique] (lien vers page 2)     │
│ • [Vêtements] (lien vers page 5)        │
│ • [Alimentation] (lien vers page 8)     │
└─────────────────────────────────────────┘
```

## Drill-down : exploration progressive

Le drill-down permet d'explorer les données du général au détail.

### Drill-down avec groupes extensibles

**Configuration dans FastReport :**

1. Sélectionnez un Group Header
2. Propriété **Collapsed** = `True` (démarrer réduit)
3. Propriété **AllowCollapse** = `True` (permettre expansion/réduction)
4. Ajoutez un indicateur visuel (▶ ou ▼)

**Exemple visuel :**

```
▶ Électronique (cliquer pour développer)

▼ Vêtements (développé)
  ├─ T-shirts : 150 ventes
  ├─ Pantalons : 120 ventes
  └─ Chaussures : 180 ventes

▶ Alimentation (cliquer pour développer)
```

### Drill-down par code

```pascal
procedure TForm1.CreerDrillDown;
begin
  // Dans le designer, configurez les groupes avec Collapsed = True
  frxReport1.LoadFromFile('RapportDrillDown.fr3');

  // L'utilisateur peut cliquer sur les + pour développer
  frxReport1.ShowReport;
end;
```

### Drill-down avec niveaux de détail

Montrez différents niveaux de détail selon les besoins.

```pascal
// Variable globale dans le rapport : NiveauDetail (Integer)

// Dans le designer, événement OnBeforePrint de la bande de détail
procedure DetailBandOnBeforePrint;
begin
  // Afficher plus ou moins de détails selon le niveau
  case <NiveauDetail> of
    1: // Niveau résumé
      begin
        MemoDetail1.Visible := False;
        MemoDetail2.Visible := False;
      end;
    2: // Niveau standard
      begin
        MemoDetail1.Visible := True;
        MemoDetail2.Visible := False;
      end;
    3: // Niveau complet
      begin
        MemoDetail1.Visible := True;
        MemoDetail2.Visible := True;
      end;
  end;
end;
```

### Indicateurs visuels pour drill-down

Ajoutez des symboles pour indiquer la possibilité d'expansion.

```pascal
// Dans l'événement OnBeforePrint du Group Header
procedure GroupHeaderOnBeforePrint;
begin
  if <GroupHeader1.Collapsed> then
    MemoIndicateur.Text := '▶'
  else
    MemoIndicateur.Text := '▼';
end;
```

## Drill-through : navigation entre rapports

Le drill-through permet de passer d'un rapport à un rapport détaillé.

### Implémentation de base

```pascal
type
  TGestionnaireRapports = class
  private
    FRapportPrincipal: TfrxReport;
    FRapportDetail: TfrxReport;
  public
    procedure AfficherRapportPrincipal;
    procedure AfficherRapportDetail(ID: Integer);
  end;

procedure TGestionnaireRapports.AfficherRapportPrincipal;
begin
  FRapportPrincipal.LoadFromFile('RapportVentes.fr3');
  FRapportPrincipal.ShowReport;
end;

procedure TGestionnaireRapports.AfficherRapportDetail(ID: Integer);
begin
  // Charger le rapport détaillé
  FRapportDetail.LoadFromFile('RapportDetailVente.fr3');

  // Passer le paramètre
  FRapportDetail.Variables['ID_Vente'] := QuotedStr(IntToStr(ID));

  // Afficher
  FRapportDetail.ShowReport;
end;
```

### Liens drill-through dans les rapports

**Dans le rapport principal :**

1. Sélectionnez un objet (numéro de commande, par exemple)
2. Propriété **Hyperlink.Kind** = `hkCustom`
3. Événement **OnClick** de l'objet :

```pascal
procedure MemoNumeroCommandeOnClick;
begin
  // Appeler une procédure Delphi
  frxReport1.DoScript('AfficherDetailCommande', <frxDBDataset."id_commande">);
end;
```

**Dans le code Delphi :**

```pascal
procedure TForm1.frxReport1UserFunction(const MethodName: String;
  var Params: Variant);
begin
  if MethodName = 'AFFICHERDETAILCOMMANDE' then
  begin
    var ID := Integer(Params[0]);
    GestionnaireRapports.AfficherRapportDetail(ID);
  end;
end;
```

### Navigation avec historique

Implémentez un système de navigation avec retour arrière.

```pascal
type
  THistoriqueNavigation = class
  private
    FHistorique: TList<TRapportInfo>;
    FPosition: Integer;
  public
    procedure AjouterRapport(const NomRapport: string; Params: TDictionary<string, string>);
    function PeutRetourner: Boolean;
    function PeutAvancer: Boolean;
    procedure Retourner;
    procedure Avancer;
  end;

procedure TForm1.btnRetourClick(Sender: TObject);
begin
  if HistoriqueNavigation.PeutRetourner then
  begin
    HistoriqueNavigation.Retourner;
    // Recharger le rapport précédent
    ChargerRapport(HistoriqueNavigation.RapportActuel);
  end;
end;
```

## Filtres et paramètres interactifs

### Panneau de filtres

Créez une interface pour que l'utilisateur filtre les données.

```pascal
type
  TFormRapportAvecFiltres = class(TForm)
    PanelFiltres: TPanel;
    DateEdit1: TDateTimePicker;
    DateEdit2: TDateTimePicker;
    ComboCategorie: TComboBox;
    CheckBoxInclureClotures: TCheckBox;
    btnAppliquer: TButton;
    frxReport1: TfrxReport;

    procedure btnAppliquerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

procedure TFormRapportAvecFiltres.FormCreate(Sender: TObject);
begin
  // Initialiser les filtres
  DateEdit1.Date := StartOfTheMonth(Date);
  DateEdit2.Date := EndOfTheMonth(Date);

  // Remplir le combo des catégories
  ChargerCategories;
end;

procedure TFormRapportAvecFiltres.btnAppliquerClick(Sender: TObject);
begin
  // Construire la requête avec les filtres
  var SQL := 'SELECT * FROM ventes WHERE 1=1';

  // Filtre par date
  SQL := SQL + Format(' AND date_vente BETWEEN %s AND %s',
    [QuotedStr(DateToStr(DateEdit1.Date)), QuotedStr(DateToStr(DateEdit2.Date))]);

  // Filtre par catégorie
  if ComboCategorie.ItemIndex > 0 then
    SQL := SQL + Format(' AND categorie = %s', [QuotedStr(ComboCategorie.Text)]);

  // Filtre par statut
  if not CheckBoxInclureClotures.Checked then
    SQL := SQL + ' AND statut <> ''Cloturé''';

  // Appliquer les filtres
  FDQueryVentes.SQL.Text := SQL;
  FDQueryVentes.Open;

  // Passer les informations au rapport pour affichage
  frxReport1.Variables['DateDebut'] := QuotedStr(DateToStr(DateEdit1.Date));
  frxReport1.Variables['DateFin'] := QuotedStr(DateToStr(DateEdit2.Date));
  frxReport1.Variables['Categorie'] := QuotedStr(ComboCategorie.Text);

  // Générer le rapport
  frxReport1.ShowReport;
end;
```

### Filtres dans le rapport FastReport

FastReport permet d'intégrer des filtres directement dans l'aperçu.

**Utilisation de variables interactives :**

1. Menu **Report → Variables**
2. Créez des variables (ex: `DateDebut`, `DateFin`)
3. Dans l'aperçu, l'utilisateur peut modifier les valeurs
4. Le rapport se régénère automatiquement

### Filtres en cascade

Les filtres en cascade se mettent à jour selon les sélections précédentes.

```pascal
procedure TFormRapportFiltres.ComboCategorieChange(Sender: TObject);
begin
  // Mettre à jour les sous-catégories selon la catégorie
  ChargerSousCategories(ComboCategorie.Text);

  // Réinitialiser les produits
  ComboBoxProduit.Items.Clear;
  ComboBoxProduit.Enabled := False;
end;

procedure TFormRapportFiltres.ComboSousCategorieChange(Sender: TObject);
begin
  // Mettre à jour les produits selon la sous-catégorie
  ChargerProduits(ComboCategorie.Text, ComboSousCategorie.Text);
  ComboBoxProduit.Enabled := True;
end;
```

### Sauvegarde des filtres

Permettez aux utilisateurs de sauvegarder leurs filtres favoris.

```pascal
type
  TFiltreSauvegarde = record
    Nom: string;
    DateDebut: TDate;
    DateFin: TDate;
    Categorie: string;
    Parametres: TDictionary<string, string>;
  end;

procedure TFormRapportFiltres.btnSauvegarderFiltreClick(Sender: TObject);
var
  Filtre: TFiltreSauvegarde;
  NomFiltre: string;
begin
  if InputQuery('Sauvegarder le filtre', 'Nom du filtre :', NomFiltre) then
  begin
    Filtre.Nom := NomFiltre;
    Filtre.DateDebut := DateEdit1.Date;
    Filtre.DateFin := DateEdit2.Date;
    Filtre.Categorie := ComboCategorie.Text;

    SauvegarderFiltre(Filtre);
    ChargerListeFiltres;

    ShowMessage('Filtre sauvegardé');
  end;
end;

procedure TFormRapportFiltres.ChargerFiltre(const Filtre: TFiltreSauvegarde);
begin
  DateEdit1.Date := Filtre.DateDebut;
  DateEdit2.Date := Filtre.DateFin;
  ComboCategorie.Text := Filtre.Categorie;

  btnAppliquerClick(nil);
end;
```

## Tri interactif

Permettez à l'utilisateur de réorganiser les données.

### Tri par clic sur les colonnes

```pascal
type
  TFormRapportTriable = class(TForm)
    frxReport1: TfrxReport;
  private
    FColonneTri: string;
    FOrdreTri: string; // 'ASC' ou 'DESC'
  public
    procedure TrierPar(const Colonne: string);
  end;

procedure TFormRapportTriable.TrierPar(const Colonne: string);
begin
  // Inverser l'ordre si on clique sur la même colonne
  if FColonneTri = Colonne then
  begin
    if FOrdreTri = 'ASC' then
      FOrdreTri := 'DESC'
    else
      FOrdreTri := 'ASC';
  end
  else
  begin
    FColonneTri := Colonne;
    FOrdreTri := 'ASC';
  end;

  // Reconstruire la requête avec le nouveau tri
  var SQL := 'SELECT * FROM ventes ORDER BY ' + FColonneTri + ' ' + FOrdreTri;
  FDQueryVentes.SQL.Text := SQL;
  FDQueryVentes.Open;

  // Régénérer le rapport
  frxReport1.ShowReport;
end;
```

### Interface de tri

Créez un panneau permettant de configurer le tri.

```pascal
procedure TFormRapportTriable.CreerPanneauTri;
var
  Panel: TPanel;
  ComboColonne: TComboBox;
  ComboOrdre: TComboBox;
  btnAppliquer: TButton;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.Height := 50;

  // Sélection de colonne
  ComboColonne := TComboBox.Create(Panel);
  ComboColonne.Parent := Panel;
  ComboColonne.Left := 10;
  ComboColonne.Top := 10;
  ComboColonne.Items.AddStrings(['Date', 'Produit', 'Montant', 'Quantité']);
  ComboColonne.ItemIndex := 0;

  // Ordre de tri
  ComboOrdre := TComboBox.Create(Panel);
  ComboOrdre.Parent := Panel;
  ComboOrdre.Left := 150;
  ComboOrdre.Top := 10;
  ComboOrdre.Items.AddStrings(['Croissant', 'Décroissant']);
  ComboOrdre.ItemIndex := 0;

  // Bouton appliquer
  btnAppliquer := TButton.Create(Panel);
  btnAppliquer.Parent := Panel;
  btnAppliquer.Left := 290;
  btnAppliquer.Top := 10;
  btnAppliquer.Caption := 'Trier';
  btnAppliquer.OnClick := procedure(Sender: TObject)
  begin
    var Colonne := ComboColonne.Text;
    var Ordre := IfThen(ComboOrdre.ItemIndex = 0, 'ASC', 'DESC');
    TrierPar(Colonne);
  end;
end;
```

### Tri multiple

Permettez le tri sur plusieurs colonnes.

```pascal
type
  TCriterieTri = record
    Colonne: string;
    Ordre: string;
  end;

var
  CriteresTri: TList<TCriterieTri>;

procedure TFormRapportTriable.AjouterCriterieTri(const Colonne, Ordre: string);
var
  Critere: TCriterieTri;
begin
  Critere.Colonne := Colonne;
  Critere.Ordre := Ordre;
  CriteresTri.Add(Critere);

  AppliquerTri;
end;

procedure TFormRapportTriable.AppliquerTri;
var
  SQL, ClauseOrderBy: string;
  i: Integer;
begin
  SQL := 'SELECT * FROM ventes';

  if CriteresTri.Count > 0 then
  begin
    ClauseOrderBy := ' ORDER BY ';
    for i := 0 to CriteresTri.Count - 1 do
    begin
      if i > 0 then
        ClauseOrderBy := ClauseOrderBy + ', ';
      ClauseOrderBy := ClauseOrderBy + CriteresTri[i].Colonne + ' ' + CriteresTri[i].Ordre;
    end;
    SQL := SQL + ClauseOrderBy;
  end;

  FDQueryVentes.SQL.Text := SQL;
  FDQueryVentes.Open;
  frxReport1.ShowReport;
end;
```

## Recherche dans les rapports

### Fonction de recherche intégrée

FastReport offre une fonction de recherche native.

```pascal
procedure TForm1.btnRechercherClick(Sender: TObject);
begin
  // Ouvrir la boîte de dialogue de recherche
  frxPreview.Find;

  // Ou rechercher directement un texte
  // frxPreview.Find('mot_à_rechercher');
end;

procedure TForm1.EditRechercheKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then // Entrée
  begin
    frxPreview.Find(EditRecherche.Text);
    Key := #0;
  end;
end;
```

### Mise en évidence des résultats

```pascal
procedure TForm1.RechercherEtSurligner(const Texte: string);
begin
  // Rechercher
  var Trouve := frxPreview.Find(Texte, []);

  if not Trouve then
    ShowMessage('Aucun résultat trouvé pour : ' + Texte)
  else
    // Le texte trouvé est automatiquement surligné par FastReport
    StatusBar1.SimpleText := Format('"%s" trouvé', [Texte]);
end;
```

### Recherche avancée avec filtres

```pascal
type
  TOptionsRecherche = record
    Texte: string;
    SensibleCasse: Boolean;
    MotEntier: Boolean;
    RechercheDans: string; // 'Tout', 'Colonnes', 'Titres'
  end;

procedure TForm1.RechercheAvancee(Options: TOptionsRecherche);
var
  Flags: TfrxSearchFlags;
begin
  Flags := [];

  if Options.SensibleCasse then
    Include(Flags, sfMatchCase);

  if Options.MotEntier then
    Include(Flags, sfWholeWord);

  var Trouve := frxPreview.Find(Options.Texte, Flags);

  if Trouve then
    LabelResultat.Caption := 'Résultat trouvé'
  else
    LabelResultat.Caption := 'Aucun résultat';
end;
```

## Rapports avec actions personnalisées

### Boutons d'action dans le rapport

Ajoutez des boutons cliquables dans le rapport.

**Dans le designer FastReport :**

1. Ajoutez un objet Shape (rectangle) pour simuler un bouton
2. Ajoutez un Memo dessus avec le texte du bouton
3. Dans l'événement OnClick du Shape :

```pascal
procedure ShapeBoutonOnClick;
begin
  // Appeler une fonction Delphi
  frxReport1.DoScript('ExporterSelectionPDF', <frxDBDataset."id">);
end;
```

**Dans Delphi :**

```pascal
procedure TForm1.frxReport1UserFunction(const MethodName: String; var Params: Variant);
begin
  if MethodName = 'EXPORTERSELECTIONPDF' then
  begin
    var ID := Integer(Params[0]);
    ExporterEnregistrementPDF(ID);
  end
  else if MethodName = 'ENVOYEREMAIL' then
  begin
    var Email := string(Params[0]);
    EnvoyerEmail(Email);
  end;
end;
```

### Actions contextuelles

Proposez des actions différentes selon le contexte.

```pascal
procedure TForm1.frxReport1ClickObject(Sender: TView; Button: TMouseButton;
  Shift: TShiftState; var Modified: Boolean);
var
  Menu: TPopupMenu;
  MenuItem: TMenuItem;
begin
  if Button = mbRight then
  begin
    // Créer un menu contextuel
    Menu := TPopupMenu.Create(Self);
    try
      // Action 1 : Voir les détails
      MenuItem := TMenuItem.Create(Menu);
      MenuItem.Caption := 'Voir les détails';
      MenuItem.OnClick := procedure(Sender: TObject)
      begin
        AfficherDetails;
      end;
      Menu.Items.Add(MenuItem);

      // Action 2 : Exporter cette ligne
      MenuItem := TMenuItem.Create(Menu);
      MenuItem.Caption := 'Exporter en PDF';
      MenuItem.OnClick := procedure(Sender: TObject)
      begin
        ExporterLignePDF;
      end;
      Menu.Items.Add(MenuItem);

      // Afficher le menu
      Menu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
    finally
      Menu.Free;
    end;
  end;
end;
```

### Validation et actions conditionnelles

```pascal
// Dans l'événement OnClick d'un objet du rapport
procedure BoutonValiderOnClick;
begin
  if <frxDBDataset."statut"> = 'En attente' then
  begin
    // Valider l'enregistrement
    frxReport1.DoScript('ValiderEnregistrement', <frxDBDataset."id">);
  end
  else
  begin
    ShowMessage('Cet enregistrement ne peut pas être validé');
  end;
end;
```

## Tableaux de bord interactifs

### Tableau de bord avec filtres globaux

```pascal
type
  TFormTableauBordInteractif = class(TForm)
    PanelFiltres: TPanel;
    PanelGraphiques: TPanel;
    Chart1: TChart;
    Chart2: TChart;
    Chart3: TChart;
    Grid1: TDBGrid;
    DateEdit1: TDateTimePicker;
    DateEdit2: TDateTimePicker;
    ComboFiltre: TComboBox;

    procedure AppliquerFiltres;
    procedure MettreAJourTableauBord;
  end;

procedure TFormTableauBordInteractif.AppliquerFiltres;
begin
  // Appliquer les filtres à toutes les requêtes
  var DateDebut := DateEdit1.Date;
  var DateFin := DateEdit2.Date;
  var Filtre := ComboFiltre.Text;

  // Mettre à jour tous les composants
  MettreAJourGraphique1(DateDebut, DateFin, Filtre);
  MettreAJourGraphique2(DateDebut, DateFin, Filtre);
  MettreAJourGraphique3(DateDebut, DateFin, Filtre);
  MettreAJourGrille(DateDebut, DateFin, Filtre);
end;

procedure TFormTableauBordInteractif.MettreAJourTableauBord;
begin
  Screen.Cursor := crHourGlass;
  try
    AppliquerFiltres;
    StatusBar1.SimpleText := 'Tableau de bord mis à jour : ' + TimeToStr(Now);
  finally
    Screen.Cursor := crDefault;
  end;
end;
```

### Graphiques cliquables

Rendez les graphiques interactifs avec drill-down.

```pascal
procedure TFormTableauBordInteractif.Chart1ClickSeries(Sender: TCustomChart;
  Series: TChartSeries; ValueIndex: Integer; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Categorie: string;
begin
  // Obtenir la catégorie cliquée
  Categorie := Series.Labels[ValueIndex];

  // Ouvrir un rapport détaillé pour cette catégorie
  AfficherRapportDetailCategorie(Categorie);
end;

procedure TFormTableauBordInteractif.AfficherRapportDetailCategorie(const Categorie: string);
begin
  frxReportDetail.Variables['Categorie'] := QuotedStr(Categorie);
  frxReportDetail.Variables['DateDebut'] := QuotedStr(DateToStr(DateEdit1.Date));
  frxReportDetail.Variables['DateFin'] := QuotedStr(DateToStr(DateEdit2.Date));

  frxReportDetail.ShowReport;
end;
```

### Actualisation automatique

Mettez à jour le tableau de bord périodiquement.

```pascal
procedure TFormTableauBordInteractif.FormCreate(Sender: TObject);
begin
  // Activer le timer pour actualisation automatique
  Timer1.Interval := 60000; // 60 secondes
  Timer1.Enabled := True;

  // Première mise à jour
  MettreAJourTableauBord;
end;

procedure TFormTableauBordInteractif.Timer1Timer(Sender: TObject);
begin
  if CheckBoxActualisationAuto.Checked then
    MettreAJourTableauBord;
end;
```

## Export interactif

### Sélection du contenu à exporter

Permettez à l'utilisateur de choisir ce qu'il veut exporter.

```pascal
type
  TFormExportInteractif = class(TForm)
    CheckListBoxSections: TCheckListBox;
    RadioGroupFormat: TRadioGroup;
    CheckBoxInclureGraphiques: TCheckBox;
    btnExporter: TButton;
  end;

procedure TFormExportInteractif.btnExporterClick(Sender: TObject);
var
  SectionsAExporter: TStringList;
  i: Integer;
begin
  SectionsAExporter := TStringList.Create;
  try
    // Collecter les sections sélectionnées
    for i := 0 to CheckListBoxSections.Count - 1 do
    begin
      if CheckListBoxSections.Checked[i] then
        SectionsAExporter.Add(CheckListBoxSections.Items[i]);
    end;

    if SectionsAExporter.Count = 0 then
    begin
      ShowMessage('Veuillez sélectionner au moins une section');
      Exit;
    end;

    // Configurer le rapport pour n'exporter que les sections sélectionnées
    ConfigurerSectionsVisibles(SectionsAExporter);

    // Exporter selon le format choisi
    case RadioGroupFormat.ItemIndex of
      0: ExporterPDF;
      1: ExporterExcel;
      2: ExporterHTML;
    end;
  finally
    SectionsAExporter.Free;
  end;
end;

procedure TFormExportInteractif.ConfigurerSectionsVisibles(Sections: TStringList);
begin
  // Masquer toutes les sections
  frxReport1.FindObject('SectionVentes').Visible := False;
  frxReport1.FindObject('SectionStatistiques').Visible := False;
  frxReport1.FindObject('SectionGraphiques').Visible := False;

  // Afficher uniquement les sections sélectionnées
  if Sections.IndexOf('Ventes') >= 0 then
    frxReport1.FindObject('SectionVentes').Visible := True;

  if Sections.IndexOf('Statistiques') >= 0 then
    frxReport1.FindObject('SectionStatistiques').Visible := True;

  if Sections.IndexOf('Graphiques') >= 0 then
    frxReport1.FindObject('SectionGraphiques').Visible := True;
end;
```

### Aperçu avant export

Montrez un aperçu de ce qui sera exporté.

```pascal
procedure TFormExportInteractif.btnApercuClick(Sender: TObject);
begin
  // Configurer le rapport selon les sélections
  ConfigurerSectionsVisibles(GetSectionsSelectionnees);

  // Afficher l'aperçu
  frxReport1.ShowReport;
end;

procedure TFormExportInteractif.btnExporterDepuisApercuClick(Sender: TObject);
begin
  // L'utilisateur a vu l'aperçu et veut exporter
  if frxReport1.PreparedPages.Count = 0 then
  begin
    ShowMessage('Veuillez d''abord afficher l''aperçu');
    Exit;
  end;

  // Exporter directement le rapport préparé
  ExporterRapportPrepare;
end;
```

## Personnalisation de l'aperçu

### Aperçu avec fonctionnalités personnalisées

```pascal
type
  TFormApercuPersonnalise = class(TForm)
    frxPreview: TfrxPreview;
    ToolBar1: TToolBar;
    btnImprimer: TToolButton;
    btnExporter: TToolButton;
    btnEmail: TToolButton;
    btnPartager: TToolButton;
    Separator1: TToolButton;
    btnZoomPlus: TToolButton;
    btnZoomMoins: TToolButton;
    btnPagePrec: TToolButton;
    btnPageSuiv: TToolButton;
    ComboBoxPages: TComboBox;
    btnRechercher: TToolButton;
    EditRecherche: TEdit;

    procedure btnImprimerClick(Sender: TObject);
    procedure btnExporterClick(Sender: TObject);
    procedure btnEmailClick(Sender: TObject);
    procedure btnPartagerClick(Sender: TObject);
    procedure btnRechercherClick(Sender: TObject);
  end;

procedure TFormApercuPersonnalise.btnEmailClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  FichierPDF: string;
begin
  // Générer un PDF temporaire
  FichierPDF := TPath.GetTempFileName + '.pdf';
  ExporterPDF(FichierPDF);

  // Ouvrir le client email avec le PDF en pièce jointe
  EnvoyerEmailAvecPieceJointe(FichierPDF);
end;

procedure TFormApercuPersonnalise.btnPartagerClick(Sender: TObject);
var
  Menu: TPopupMenu;
begin
  // Créer un menu avec différentes options de partage
  Menu := TPopupMenu.Create(Self);
  try
    AjouterMenuItemPartage(Menu, 'Envoyer par email', @PartagerEmail);
    AjouterMenuItemPartage(Menu, 'Sauvegarder dans le cloud', @PartagerCloud);
    AjouterMenuItemPartage(Menu, 'Copier le lien', @PartagerLien);

    Menu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  finally
    Menu.Free;
  end;
end;
```

### Barre d'outils personnalisée

```pascal
procedure TFormApercuPersonnalise.ConfigurerBarreOutils;
begin
  // Configurer les boutons de la barre d'outils
  with ToolBar1 do
  begin
    ShowCaptions := True;
    ButtonHeight := 32;
    ButtonWidth := 32;
  end;

  // Ajouter des icônes
  btnImprimer.ImageIndex := 0;
  btnExporter.ImageIndex := 1;
  btnEmail.ImageIndex := 2;

  // Configurer les raccourcis
  btnImprimer.ShortCut := ShortCut(Ord('P'), [ssCtrl]);
  btnExporter.ShortCut := ShortCut(Ord('S'), [ssCtrl]);
  btnRechercher.ShortCut := ShortCut(Ord('F'), [ssCtrl]);
end;
```

## Rapports collaboratifs

### Annotations et commentaires

Permettez aux utilisateurs d'ajouter des notes sur les rapports.

```pascal
type
  TAnnotation = record
    ID: Integer;
    Page: Integer;
    X, Y: Integer;
    Texte: string;
    Auteur: string;
    DateCreation: TDateTime;
  end;

procedure TFormRapportCollaboratif.AjouterAnnotation(Page, X, Y: Integer; const Texte: string);
var
  Annotation: TAnnotation;
begin
  Annotation.ID := GenererID;
  Annotation.Page := Page;
  Annotation.X := X;
  Annotation.Y := Y;
  Annotation.Texte := Texte;
  Annotation.Auteur := GetNomUtilisateur;
  Annotation.DateCreation := Now;

  SauvegarderAnnotation(Annotation);
  AfficherAnnotations;
end;

procedure TFormRapportCollaboratif.frxPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbRight) and (ssCtrl in Shift) then
  begin
    // Ctrl + Clic droit = ajouter une annotation
    var Texte: string;
    if InputQuery('Nouvelle annotation', 'Commentaire :', Texte) then
    begin
      var Page := frxPreview.PageNo;
      AjouterAnnotation(Page, X, Y, Texte);
    end;
  end;
end;
```

### Partage de rapports

```pascal
procedure TFormRapportCollaboratif.PartagerRapport;
var
  Destinataires: TStringList;
  i: Integer;
begin
  Destinataires := TStringList.Create;
  try
    // Sélectionner les destinataires
    if SelectionnerDestinataires(Destinataires) then
    begin
      // Générer le rapport
      var CheminPDF := GenererPDFTemporaire;

      // Envoyer à chaque destinataire
      for i := 0 to Destinataires.Count - 1 do
      begin
        EnvoyerRapportParEmail(Destinataires[i], CheminPDF);
      end;

      // Logger le partage
      LoggerPartage(Destinataires, CheminPDF);

      ShowMessage(Format('Rapport partagé avec %d personne(s)', [Destinataires.Count]));
    end;
  finally
    Destinataires.Free;
  end;
end;
```

## Performance et optimisation

### Chargement progressif

Pour les gros rapports, chargez les données progressivement.

```pascal
procedure TForm1.GenererRapportProgressif;
const
  TAILLE_BLOC = 1000;
var
  Offset: Integer;
begin
  Offset := 0;

  frxReport1.PrepareReport(False); // Ne pas tout charger

  repeat
    // Charger un bloc de données
    ChargerBlocDonnees(Offset, TAILLE_BLOC);

    // Ajouter au rapport
    frxReport1.PreparePage(frxReport1.PreviewPages.Count);

    Offset := Offset + TAILLE_BLOC;

    // Mise à jour de la progression
    ProgressBar1.Position := Offset;
    Application.ProcessMessages;

  until FDQueryVentes.Eof;

  frxReport1.ShowPreparedReport;
end;
```

### Cache des rapports

Mettez en cache les rapports fréquemment consultés.

```pascal
type
  TCacheRapport = class
  private
    FCacheDir: string;
    FDureeValidite: Integer; // en heures
  public
    function ObtenirRapport(const CleRapport: string): Boolean;
    procedure SauvegarderRapport(const CleRapport: string);
    function EstValide(const CleRapport: string): Boolean;
  end;

procedure TForm1.AfficherRapportAvecCache(const Parametres: string);
var
  CleCache: string;
begin
  CleCache := GenererCleCacheRapport(Parametres);

  if CacheRapport.EstValide(CleCache) and CacheRapport.ObtenirRapport(CleCache) then
  begin
    // Charger depuis le cache
    frxReport1.LoadPreparedReportFromFile(CacheRapport.CheminCache(CleCache));
    frxReport1.ShowPreparedReport;
  end
  else
  begin
    // Générer le rapport
    frxReport1.PrepareReport;
    frxReport1.ShowPreparedReport;

    // Sauvegarder dans le cache
    CacheRapport.SauvegarderRapport(CleCache);
  end;
end;
```

## Conseils et bonnes pratiques

### Design d'interface

- **Intuitivité** : les fonctionnalités interactives doivent être évidentes
- **Feedback visuel** : montrez ce qui est cliquable (curseur, survol)
- **Cohérence** : utilisez les mêmes conventions partout
- **Progression** : indiquez l'avancement des opérations longues
- **Aide contextuelle** : expliquez les fonctionnalités complexes

### Performance

- **Chargement différé** : ne chargez que ce qui est visible
- **Cache intelligent** : réutilisez les résultats calculés
- **Optimisation SQL** : filtrez au niveau de la base
- **Pagination** : limitez les données affichées simultanément
- **Asynchrone** : évitez de bloquer l'interface

### Accessibilité

- **Navigation clavier** : toutes les fonctions accessibles au clavier
- **Raccourcis** : F3 pour rechercher, Ctrl+P pour imprimer, etc.
- **Contraste** : éléments interactifs bien visibles
- **Tooltips** : aide sur les boutons et fonctions
- **Alternative texte** : décrivez les actions disponibles

### Sécurité

- **Validation** : vérifiez les entrées utilisateur
- **Permissions** : contrôlez l'accès aux fonctionnalités
- **Audit** : journalisez les actions importantes
- **Export sécurisé** : protégez les données sensibles
- **Sessions** : gérez les droits utilisateur

### Expérience utilisateur

- **Cohérence** : comportements prévisibles
- **Réactivité** : réponse immédiate aux actions
- **Annulation** : possibilité de revenir en arrière
- **Préférences** : sauvegardez les choix utilisateur
- **Documentation** : aide intégrée et exemples

## Exemple complet : Rapport de ventes interactif

```pascal
unit URapportVentesInteractif;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls, FireDAC.Comp.Client,
  frxClass, frxDBSet, frxPreview;

type
  TFormRapportVentesInteractif = class(TForm)
    // Composants visuels
    PanelFiltres: TPanel;
    PanelApercu: TPanel;
    DateEdit1: TDateTimePicker;
    DateEdit2: TDateTimePicker;
    ComboCategorie: TComboBox;
    ComboVendeur: TComboBox;
    CheckBoxDetails: TCheckBox;
    btnAppliquer: TButton;
    btnReset: TButton;
    btnExporter: TButton;
    StatusBar1: TStatusBar;

    // Composants FastReport
    frxReport1: TfrxReport;
    frxPreview1: TfrxPreview;
    frxDBDataset1: TfrxDBDataset;

    // Base de données
    FDConnection1: TFDConnection;
    FDQueryVentes: TFDQuery;

    procedure FormCreate(Sender: TObject);
    procedure btnAppliquerClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnExporterClick(Sender: TObject);
    procedure frxReport1UserFunction(const MethodName: String; var Params: Variant);
  private
    FHistorique: TList<string>;
    procedure InitialiserFiltres;
    procedure AppliquerFiltres;
    procedure GenererRapport;
    procedure AjouterHistorique(const Action: string);
    procedure AfficherDetailVente(ID: Integer);
  end;

implementation

{$R *.dfm}

procedure TFormRapportVentesInteractif.FormCreate(Sender: TObject);
begin
  FHistorique := TList<string>.Create;

  // Configuration de la connexion
  FDConnection1.Params.Values['Database'] := 'ma_base';
  FDConnection1.Connected := True;

  // Configuration du preview
  frxPreview1.Parent := PanelApercu;
  frxPreview1.Align := alClient;
  frxReport1.Preview := frxPreview1;

  // Initialisation
  InitialiserFiltres;
  GenererRapport;
end;

procedure TFormRapportVentesInteractif.InitialiserFiltres;
begin
  // Dates par défaut : mois en cours
  DateEdit1.Date := StartOfTheMonth(Date);
  DateEdit2.Date := EndOfTheMonth(Date);

  // Charger les catégories
  ComboCategorie.Items.Clear;
  ComboCategorie.Items.Add('Toutes');
  FDConnection1.ExecSQL('SELECT DISTINCT categorie FROM produits ORDER BY categorie',
    procedure(DataSet: TDataSet)
    begin
      while not DataSet.Eof do
      begin
        ComboCategorie.Items.Add(DataSet.FieldByName('categorie').AsString);
        DataSet.Next;
      end;
    end);
  ComboCategorie.ItemIndex := 0;

  // Charger les vendeurs
  ComboVendeur.Items.Clear;
  ComboVendeur.Items.Add('Tous');
  // ... similaire pour les vendeurs
  ComboVendeur.ItemIndex := 0;
end;

procedure TFormRapportVentesInteractif.AppliquerFiltres;
var
  SQL: string;
begin
  SQL := 'SELECT v.*, p.nom as produit, p.categorie, u.nom as vendeur ' +
         'FROM ventes v ' +
         'INNER JOIN produits p ON v.id_produit = p.id ' +
         'INNER JOIN utilisateurs u ON v.id_vendeur = u.id ' +
         'WHERE v.date_vente BETWEEN :date_debut AND :date_fin';

  // Filtre catégorie
  if ComboCategorie.ItemIndex > 0 then
    SQL := SQL + ' AND p.categorie = :categorie';

  // Filtre vendeur
  if ComboVendeur.ItemIndex > 0 then
    SQL := SQL + ' AND u.nom = :vendeur';

  SQL := SQL + ' ORDER BY v.date_vente DESC';

  // Appliquer la requête
  FDQueryVentes.Close;
  FDQueryVentes.SQL.Text := SQL;
  FDQueryVentes.ParamByName('date_debut').AsDate := DateEdit1.Date;
  FDQueryVentes.ParamByName('date_fin').AsDate := DateEdit2.Date;

  if ComboCategorie.ItemIndex > 0 then
    FDQueryVentes.ParamByName('categorie').AsString := ComboCategorie.Text;

  if ComboVendeur.ItemIndex > 0 then
    FDQueryVentes.ParamByName('vendeur').AsString := ComboVendeur.Text;

  FDQueryVentes.Open;

  AjouterHistorique(Format('Filtres appliqués : %s à %s, Cat: %s, Vendeur: %s',
    [DateToStr(DateEdit1.Date), DateToStr(DateEdit2.Date),
     ComboCategorie.Text, ComboVendeur.Text]));
end;

procedure TFormRapportVentesInteractif.btnAppliquerClick(Sender: TObject);
begin
  Screen.Cursor := crHourGlass;
  try
    AppliquerFiltres;
    GenererRapport;
    StatusBar1.SimpleText := Format('%d ventes trouvées', [FDQueryVentes.RecordCount]);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormRapportVentesInteractif.GenererRapport;
begin
  // Passer les paramètres au rapport
  frxReport1.Variables['DateDebut'] := QuotedStr(DateToStr(DateEdit1.Date));
  frxReport1.Variables['DateFin'] := QuotedStr(DateToStr(DateEdit2.Date));
  frxReport1.Variables['AfficherDetails'] := BoolToStr(CheckBoxDetails.Checked, True);

  // Générer
  frxReport1.LoadFromFile('RapportVentesInteractif.fr3');
  frxReport1.ShowReport(False); // False = non modal
end;

procedure TFormRapportVentesInteractif.frxReport1UserFunction(
  const MethodName: String; var Params: Variant);
begin
  if MethodName = 'AFFICHERDETAILVENTE' then
  begin
    var ID := Integer(Params[0]);
    AfficherDetailVente(ID);
  end
  else if MethodName = 'EXPORTERLIGNE' then
  begin
    var ID := Integer(Params[0]);
    ExporterLigneEnPDF(ID);
  end;
end;

procedure TFormRapportVentesInteractif.AfficherDetailVente(ID: Integer);
begin
  // Créer un nouveau formulaire pour afficher les détails
  var FormDetail := TFormDetailVente.Create(Self);
  try
    FormDetail.ChargerVente(ID);
    FormDetail.ShowModal;
  finally
    FormDetail.Free;
  end;

  AjouterHistorique('Détail vente #' + IntToStr(ID));
end;

procedure TFormRapportVentesInteractif.btnResetClick(Sender: TObject);
begin
  InitialiserFiltres;
  CheckBoxDetails.Checked := False;
  btnAppliquerClick(nil);
end;

procedure TFormRapportVentesInteractif.btnExporterClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Filter := 'PDF|*.pdf|Excel|*.xlsx|CSV|*.csv';
    SaveDialog.DefaultExt := 'pdf';

    if SaveDialog.Execute then
    begin
      case SaveDialog.FilterIndex of
        1: ExporterPDF(SaveDialog.FileName);
        2: ExporterExcel(SaveDialog.FileName);
        3: ExporterCSV(SaveDialog.FileName);
      end;

      AjouterHistorique('Export : ' + SaveDialog.FileName);
    end;
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormRapportVentesInteractif.AjouterHistorique(const Action: string);
begin
  FHistorique.Add(Format('[%s] %s', [TimeToStr(Now), Action]));
end;

end.
```

## Résumé

Les rapports interactifs transforment les rapports statiques en outils d'exploration puissants. Les points clés :

- **Navigation** : signets, table des matières, hyperliens pour se déplacer facilement
- **Drill-down** : exploration progressive du général au détail
- **Drill-through** : passage fluide entre rapports liés
- **Filtres dynamiques** : personnalisation des données affichées
- **Tri interactif** : réorganisation selon les besoins
- **Recherche** : localisation rapide d'informations
- **Actions personnalisées** : boutons et menus contextuels
- **Tableaux de bord** : vues multiples avec interactions
- **Export sélectif** : choix précis du contenu à exporter
- **Performance** : optimisations pour la réactivité

Maîtriser les rapports interactifs permet de créer des applications analytiques modernes qui donnent aux utilisateurs le pouvoir d'explorer et de comprendre leurs données de manière autonome et efficace.

⏭️ [Graphiques et tableaux de bord avec TeeChart](/09-rapports-et-impressions/08-graphiques-et-tableaux-de-bord-avec-teechart.md)
