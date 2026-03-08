🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.4 Création de rapports complexes

## Introduction

Après avoir découvert les bases des générateurs de rapports, il est temps d'explorer la création de rapports plus sophistiqués. Les rapports complexes combinent plusieurs techniques avancées pour produire des documents professionnels adaptés aux besoins métier spécifiques : rapports financiers détaillés, analyses multi-niveaux, documents avec calculs élaborés, etc.

Dans cette section, nous allons découvrir comment créer des rapports qui vont au-delà des simples listes de données.

## Qu'est-ce qu'un rapport complexe ?

Un rapport complexe se caractérise par une ou plusieurs de ces caractéristiques :

- **Plusieurs sources de données** : combinaison de différentes tables ou requêtes
- **Groupements imbriqués** : plusieurs niveaux de regroupement (catégorie → sous-catégorie → produit)
- **Calculs sophistiqués** : totaux, sous-totaux, pourcentages, moyennes à différents niveaux
- **Mise en page élaborée** : colonnes multiples, positionnement conditionnel, sauts de page intelligents
- **Données dynamiques** : structure du rapport qui s'adapte aux données
- **Éléments interactifs** : liens, signets, table des matières
- **Graphiques et visualisations** : intégration de multiples graphiques
- **Formats spéciaux** : codes-barres, QR codes, images dynamiques

## Rapport maître-détail multiniveau

### Concept

Un rapport multiniveau affiche des données hiérarchiques sur plusieurs niveaux. Par exemple :

**Niveau 1** : Région  
**Niveau 2** : Ville  
**Niveau 3** : Magasin  
**Niveau 4** : Ventes du magasin  

### Structure de données

Pour créer ce type de rapport, vous aurez besoin de plusieurs datasets ou d'une requête avec jointures.

#### Approche 1 : Plusieurs datasets

```pascal
// Dataset des régions
FDQueryRegions.SQL.Text := 'SELECT id_region, nom_region FROM regions ORDER BY nom_region';

// Dataset des villes (lié aux régions)
FDQueryVilles.SQL.Text := 'SELECT id_ville, nom_ville, id_region FROM villes ORDER BY nom_ville';  
FDQueryVilles.MasterSource := DataSourceRegions;  
FDQueryVilles.MasterFields := 'id_region';  

// Dataset des magasins (lié aux villes)
FDQueryMagasins.SQL.Text := 'SELECT id_magasin, nom_magasin, id_ville FROM magasins ORDER BY nom_magasin';  
FDQueryMagasins.MasterSource := DataSourceVilles;  
FDQueryMagasins.MasterFields := 'id_ville';  

// Dataset des ventes (lié aux magasins)
FDQueryVentes.SQL.Text := 'SELECT date_vente, montant, id_magasin FROM ventes ORDER BY date_vente';  
FDQueryVentes.MasterSource := DataSourceMagasins;  
FDQueryVentes.MasterFields := 'id_magasin';  
```

#### Approche 2 : Requête unique avec jointures

```sql
SELECT
    r.nom_region,
    v.nom_ville,
    m.nom_magasin,
    vt.date_vente,
    vt.montant
FROM regions r  
INNER JOIN villes v ON r.id_region = v.id_region  
INNER JOIN magasins m ON v.id_ville = m.id_ville  
INNER JOIN ventes vt ON m.id_magasin = vt.id_magasin  
ORDER BY r.nom_region, v.nom_ville, m.nom_magasin, vt.date_vente  
```

### Configuration du rapport

Dans FastReport, vous devez configurer plusieurs niveaux de groupement :

**Étape 1 : Créer les groupes**

1. Ajoutez un **Group Header** pour les régions
   - Condition de groupe : `<frxDBDataset."nom_region">`
2. Ajoutez un **Group Header** pour les villes
   - Condition de groupe : `<frxDBDataset."nom_ville">`
3. Ajoutez un **Group Header** pour les magasins
   - Condition de groupe : `<frxDBDataset."nom_magasin">`
4. Ajoutez la bande **Master Data** pour les ventes

**Étape 2 : Organiser visuellement**

- **Region Header** : en gras, grande taille, fond coloré
- **Ville Header** : en gras, indentation de 1 cm
- **Magasin Header** : indentation de 2 cm
- **Master Data** : indentation de 3 cm, lignes de détail

**Étape 3 : Ajouter les pieds de groupe**

Chaque niveau peut avoir un pied avec des sous-totaux :

- **Magasin Footer** : total par magasin
- **Ville Footer** : total par ville
- **Region Footer** : total par région

### Calculs à plusieurs niveaux

#### Total par magasin

Dans le pied du groupe Magasin :

```
Total magasin : [SUM(<frxDBDataset."montant">, MasterData1)]
```

#### Total par ville

Dans le pied du groupe Ville :

```
Total ville : [SUM(<frxDBDataset."montant">, GroupHeader2)]
```

Le deuxième paramètre indique jusqu'où remonter pour le calcul.

#### Total général

Dans la bande **Report Summary** :

```
Total général : [SUM(<frxDBDataset."montant">)]
```

### Exemple complet de structure

```
┌─ Report Title ────────────────────────────┐
│  RAPPORT DES VENTES PAR RÉGION            │
└───────────────────────────────────────────┘

┌─ Page Header ─────────────────────────────┐
│  Région | Ville | Magasin | Date | Montant│
└───────────────────────────────────────────┘

┌─ Region Header ───────────────────────────┐
│  RÉGION : Île-de-France                   │
└───────────────────────────────────────────┘

  ┌─ Ville Header ─────────────────────────┐
  │    Ville : Paris                       │
  └────────────────────────────────────────┘

    ┌─ Magasin Header ───────────────────┐
    │      Magasin : Paris Centre        │
    └────────────────────────────────────┘

      ┌─ Master Data ───────────────────┐
      │        01/10/2024    150,00 €   │
      │        02/10/2024    230,00 €   │
      └─────────────────────────────────┘

    ┌─ Magasin Footer ───────────────────┐
    │      Total magasin : 380,00 €      │
    └────────────────────────────────────┘

  ┌─ Ville Footer ───────────────────────┐
  │    Total ville : 380,00 €            │
  └──────────────────────────────────────┘

┌─ Region Footer ───────────────────────────┐
│  TOTAL RÉGION : 380,00 €                  │
└───────────────────────────────────────────┘

┌─ Report Summary ──────────────────────────┐
│  TOTAL GÉNÉRAL : 380,00 €                 │
└───────────────────────────────────────────┘
```

## Rapports avec données de plusieurs sources

### Utilisation de plusieurs datasets

Parfois, vous devez combiner des données provenant de sources complètement différentes.

**Exemple :** Rapport de gestion combinant :
- Données de ventes (base MySQL)
- Données comptables (base SQL Server)
- Données analytiques (fichier Excel)

### Configuration dans FastReport

**Étape 1 : Préparer les composants**

```pascal
// Connexion MySQL pour les ventes
FDConnection1.DriverName := 'MySQL';  
FDQueryVentes.Connection := FDConnection1;  
frxDBDatasetVentes.DataSet := FDQueryVentes;  

// Connexion SQL Server pour la comptabilité
FDConnection2.DriverName := 'MSSQL';  
FDQueryCompta.Connection := FDConnection2;  
frxDBDatasetCompta.DataSet := FDQueryCompta;  

// Dataset pour fichier Excel
FDQueryExcel.Connection := FDConnectionExcel;  
frxDBDatasetAnalytics.DataSet := FDQueryExcel;  
```

**Étape 2 : Organisation du rapport**

Créez des sections distinctes dans le rapport :

1. **Section Ventes** (utilise frxDBDatasetVentes)
2. **Section Comptabilité** (utilise frxDBDatasetCompta)
3. **Section Analytique** (utilise frxDBDatasetAnalytics)

**Étape 3 : Synchronisation**

Si les données doivent être synchronisées (même période, etc.), passez des paramètres :

```pascal
frxReport1.Variables['DateDebut'] := QuotedStr(DateToStr(DateDebut));  
frxReport1.Variables['DateFin'] := QuotedStr(DateToStr(DateFin));  
```

Puis dans les requêtes :

```sql
SELECT * FROM ventes  
WHERE date_vente BETWEEN :DateDebut AND :DateFin  
```

### Utilisation de sous-rapports

Pour combiner élégamment plusieurs sources, utilisez des sous-rapports :

**Rapport principal** : vue d'ensemble et totaux généraux  
**Sous-rapport 1** : détails des ventes  
**Sous-rapport 2** : détails comptables  
**Sous-rapport 3** : analyses graphiques  

## Calculs avancés et agrégations

### Calculs personnalisés

FastReport permet des calculs complexes via des expressions.

#### Pourcentage du total

```
[(<frxDBDataset."montant"> / [SUM(<frxDBDataset."montant">)]) * 100]%
```

#### Variation par rapport à la moyenne

```
[<frxDBDataset."valeur"> - [AVG(<frxDBDataset."valeur">)]]
```

#### Cumul progressif

Utilisez une variable :

```pascal
// Dans l'événement OnBeforePrint de la bande Master Data
begin
  Cumul := Cumul + <frxDBDataset."montant">;
  Memo1.Text := FormatFloat('#,##0.00', Cumul);
end;
```

### Variables système et personnalisées

#### Déclaration de variables

Dans le rapport, créez des variables :

1. Menu **Report → Variables**
2. Créez une catégorie "Calculs"
3. Ajoutez vos variables :
   - `Cumul` : Double
   - `Compteur` : Integer
   - `Maximum` : Double

#### Initialisation

Dans l'événement **OnStartReport** du rapport :

```pascal
begin
  Cumul := 0;
  Compteur := 0;
  Maximum := 0;
end;
```

#### Utilisation

Dans les événements des bandes :

```pascal
// Dans Master Data OnBeforePrint
begin
  Inc(Compteur);
  Cumul := Cumul + <frxDBDataset."montant">;

  if <frxDBDataset."montant"> > Maximum then
    Maximum := <frxDBDataset."montant">;
end;
```

### Tableaux de synthèse

Créez des tableaux récapitulatifs avec des calculs agrégés.

**Exemple : Synthèse des ventes par catégorie**

```
┌────────────────────┬───────────┬──────────┬─────────┐
│ Catégorie          │ Quantité  │ Montant  │ % Total │
├────────────────────┼───────────┼──────────┼─────────┤
│ Électronique       │     245   │ 15 650 € │   35%   │
│ Vêtements          │     789   │ 23 400 € │   52%   │
│ Maison             │     156   │  5 850 € │   13%   │
├────────────────────┼───────────┼──────────┼─────────┤
│ TOTAL              │   1 190   │ 44 900 € │  100%   │
└────────────────────┴───────────┴──────────┴─────────┘
```

## Mise en page sophistiquée

### Colonnes multiples

Pour afficher des données sur plusieurs colonnes (étiquettes, listes, etc.).

**Configuration :**

1. Sélectionnez la page du rapport
2. Dans l'Inspecteur d'objets : **Columns** → **Count** = 2 (ou plus)
3. Configurez **Width** et **Gap** (espacement entre colonnes)

**Exemple d'utilisation :** Liste de produits affichée sur 3 colonnes pour économiser du papier.

### Positionnement conditionnel

Déplacez ou cachez des éléments selon les données.

#### Cacher un élément conditionnel

```pascal
// Dans l'événement OnBeforePrint d'un Memo
begin
  if <frxDBDataset."montant"> = 0 then
    Memo1.Visible := False
  else
    Memo1.Visible := True;
end;
```

#### Ajuster la position

```pascal
// Décaler un élément selon une condition
begin
  if <frxDBDataset."type"> = 'Premium' then
    Memo1.Left := 50
  else
    Memo1.Left := 100;
end;
```

### Sauts de page intelligents

Contrôlez les sauts de page pour améliorer la lisibilité.

#### Forcer un saut de page

Configurez la propriété **StartNewPage** d'une bande à `True`.

**Exemple :** Chaque nouvelle région commence sur une nouvelle page.

#### Éviter de couper un groupe

Configurez **KeepTogether** à `True` sur un Group Header pour garder tout le groupe sur la même page.

#### Saut de page conditionnel

```pascal
// Dans l'événement OnBeforePrint du Group Header
begin
  if Engine.CurY > (Engine.PageHeight - 2000) then
    Engine.NewPage;
end;
```

### En-têtes et pieds de page dynamiques

Modifiez les en-têtes selon le contenu de la page.

```pascal
// Dans l'événement OnBeforePrint du Page Header
begin
  if <frxDBDataset."categorie"> <> OldCategorie then
  begin
    MemoCategorie.Text := 'Catégorie : ' + <frxDBDataset."categorie">;
    OldCategorie := <frxDBDataset."categorie">;
  end;
end;
```

## Rapports avec graphiques multiples

### Intégration de plusieurs graphiques

Un rapport peut contenir plusieurs graphiques pour visualiser différents aspects des données.

**Exemple : Tableau de bord financier**

- **Graphique 1** : Évolution mensuelle du chiffre d'affaires (courbes)
- **Graphique 2** : Répartition par catégorie (camembert)
- **Graphique 3** : Comparaison par région (barres)
- **Graphique 4** : Top 10 des produits (barres horizontales)

### Configuration de graphiques complexes

#### Graphique avec plusieurs séries

1. Ajoutez un graphique dans le rapport
2. Double-cliquez pour ouvrir l'éditeur
3. Onglet **Series** → **Add** pour ajouter plusieurs séries
4. Pour chaque série :
   - Sélectionnez le dataset
   - Définissez les champs Value et Argument
   - Choisissez le type (ligne, barre, etc.)

**Exemple : Ventes et objectifs sur le même graphique**

- **Série 1** : Ventes réelles (barres bleues)
- **Série 2** : Objectifs (ligne rouge)

#### Graphique avec axes secondaires

Pour comparer des données d'échelles différentes :

1. Série 1 : Quantités (axe gauche)
2. Série 2 : Pourcentages (axe droit)

Configurez **SecondaryAxisY** sur la deuxième série.

### Graphiques dynamiques

Générez des graphiques basés sur des calculs faits dans le rapport.

```pascal
// Dans l'événement OnBeforePrint du graphique
begin
  Chart1.Series[0].Clear;

  // Ajouter les données calculées
  Chart1.Series[0].Add(<Total1>, 'Janvier');
  Chart1.Series[0].Add(<Total2>, 'Février');
  Chart1.Series[0].Add(<Total3>, 'Mars');
end;
```

## Rapports avec données hiérarchiques

### Données en arborescence

Pour afficher des structures hiérarchiques (organigrammes, nomenclatures, etc.).

**Exemple : Nomenclature de produits**

```
Catégorie 1
  ├── Sous-catégorie 1.1
  │   ├── Produit A
  │   └── Produit B
  └── Sous-catégorie 1.2
      └── Produit C
Catégorie 2
  └── Sous-catégorie 2.1
      ├── Produit D
      └── Produit E
```

### Implémentation avec indentation

Utilisez une variable pour gérer le niveau d'indentation :

```pascal
// Variable : NiveauIndentation (Integer)

// Dans Master Data OnBeforePrint
begin
  MemoNom.Left := 100 + (NiveauIndentation * 200); // 200 pixels par niveau

  // Symbole selon le niveau
  case NiveauIndentation of
    0: MemoSymbole.Text := '■';
    1: MemoSymbole.Text := '├──';
    2: MemoSymbole.Text := '   ├──';
  end;
end;
```

### Requête récursive

Utilisez des CTE (Common Table Expressions) pour extraire les données hiérarchiques :

```sql
WITH RECURSIVE Hierarchie AS (
  SELECT id, nom, id_parent, 0 AS niveau
  FROM categories
  WHERE id_parent IS NULL

  UNION ALL

  SELECT c.id, c.nom, c.id_parent, h.niveau + 1
  FROM categories c
  INNER JOIN Hierarchie h ON c.id_parent = h.id
)
SELECT * FROM Hierarchie ORDER BY niveau, nom
```

## Rapports interactifs

### Signets (Bookmarks)

Créez une navigation interne dans le rapport PDF.

**Configuration :**

1. Sélectionnez un objet (titre de section, par exemple)
2. Dans l'Inspecteur d'objets : **Bookmark** = `<frxDBDataset."nom_section">`

Le PDF généré contiendra des signets cliquables.

### Hyperliens

Ajoutez des liens cliquables dans le rapport.

**Lien externe :**

1. Sélectionnez un Memo
2. Propriété **Hyperlink.Value** = `'http://www.exemple.com'`

**Lien dynamique :**

```
[<frxDBDataset."url_site_web">]
```

**Lien vers une autre section du rapport :**

```
#[<frxDBDataset."nom_section">]
```

### Table des matières

Générez automatiquement une table des matières.

**Étape 1 : Créer une page de table des matières**

1. Ajoutez une nouvelle page au rapport
2. Nommez-la "Table des matières"
3. Placez-la en première position

**Étape 2 : Configurer les entrées**

1. Dans les sections du rapport, marquez les éléments avec des signets
2. Configurez la propriété **TOC** (Table Of Contents)

**Étape 3 : Afficher la table**

FastReport génère automatiquement la table des matières avec les numéros de page.

## Formatage conditionnel avancé

### Mise en forme par seuils

Appliquez différents formats selon des seuils multiples.

```pascal
// Dans OnBeforePrint du Memo affichant le montant
begin
  if <frxDBDataset."montant"> >= 10000 then
  begin
    Memo1.Font.Color := clGreen;
    Memo1.Font.Style := [fsBold];
    Memo1.Text := '★★★ ' + FormatFloat('#,##0.00', <frxDBDataset."montant">);
  end
  else if <frxDBDataset."montant"> >= 5000 then
  begin
    Memo1.Font.Color := clBlue;
    Memo1.Font.Style := [fsBold];
    Memo1.Text := '★★ ' + FormatFloat('#,##0.00', <frxDBDataset."montant">);
  end
  else if <frxDBDataset."montant"> >= 1000 then
  begin
    Memo1.Font.Color := clBlack;
    Memo1.Text := '★ ' + FormatFloat('#,##0.00', <frxDBDataset."montant">);
  end
  else
  begin
    Memo1.Font.Color := clGray;
    Memo1.Text := FormatFloat('#,##0.00', <frxDBDataset."montant">);
  end;
end;
```

### Barres de données (Data Bars)

Créez des barres visuelles proportionnelles aux valeurs.

```pascal
// Dans OnBeforePrint d'un Shape (rectangle)
begin
  var Largeur := Round((<frxDBDataset."valeur"> / <Maximum>) * 200);
  Shape1.Width := Largeur;

  // Couleur selon la valeur
  if <frxDBDataset."valeur"> > 80 then
    Shape1.Brush.Color := clGreen
  else if <frxDBDataset."valeur"> > 50 then
    Shape1.Brush.Color := clYellow
  else
    Shape1.Brush.Color := clRed;
end;
```

### Icônes conditionnelles

Ajoutez des icônes visuelles selon les valeurs.

```pascal
// Dans OnBeforePrint d'un Picture
begin
  if <frxDBDataset."statut"> = 'OK' then
    Picture1.LoadFromFile('icone_ok.png')
  else if <frxDBDataset."statut"> = 'Attention' then
    Picture1.LoadFromFile('icone_warning.png')
  else
    Picture1.LoadFromFile('icone_erreur.png');
end;
```

## Rapports avec codes-barres et QR codes

### Génération de codes-barres

Les codes-barres sont utiles pour l'identification, la traçabilité et la logistique.

**Types supportés :**
- EAN13, EAN8 : produits de consommation
- Code128 : usage général, alphanumériques
- Code39 : logistique, industrie
- QR Code : liens, informations complexes
- DataMatrix : petits espaces, traçabilité

### Configuration d'un code-barres

1. Ajoutez un objet **Barcode** dans le rapport
2. Double-cliquez pour configurer :
   - **Type** : choisissez le type de code
   - **Text** : `[<frxDBDataset."code_produit">]`
   - **Options** : ShowText (afficher le texte sous le code)
   - **Rotation** : orientation si nécessaire

### QR Code avec informations multiples

Encodez plusieurs informations dans un QR Code :

```pascal
// Dans OnBeforePrint du Barcode
begin
  var InfoQR := 'Produit:' + <frxDBDataset."nom"> + #13#10 +
                'Ref:' + <frxDBDataset."reference"> + #13#10 +
                'Prix:' + FormatFloat('0.00', <frxDBDataset."prix">) + #13#10 +
                'URL:http://monsite.com/produit/' + <frxDBDataset."id">;

  Barcode1.Text := InfoQR;
end;
```

### Étiquettes avec codes-barres

Créez des feuilles d'étiquettes :

1. Définissez la taille de la page selon votre format d'étiquettes
2. Configurez les colonnes (nombre d'étiquettes par ligne)
3. Ajoutez le code-barres et les informations complémentaires
4. Configurez la hauteur de la bande Master Data

## Rapports avec images dynamiques

### Chargement d'images depuis la base

Affichez des images stockées dans la base de données.

**Si stockées en BLOB :**

```pascal
// Dans OnBeforePrint du Picture
begin
  if not <frxDBDataset."photo">.IsNull then
  begin
    var Stream := TMemoryStream.Create;
    try
      <frxDBDataset."photo">.SaveToStream(Stream);
      Stream.Position := 0;
      Picture1.Picture.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end
  else
    Picture1.Picture := nil; // Pas d'image
end;
```

**Si stockées comme chemin de fichier :**

```pascal
// Dans OnBeforePrint du Picture
begin
  var CheminImage := <frxDBDataset."chemin_photo">;
  if FileExists(CheminImage) then
    Picture1.LoadFromFile(CheminImage)
  else
    Picture1.LoadFromFile('image_par_defaut.jpg');
end;
```

### Redimensionnement automatique

Adaptez les images à un cadre prédéfini :

```pascal
// Propriétés du Picture
Picture1.Stretch := True;  
Picture1.KeepAspectRatio := True;  
Picture1.Center := True;  
```

### Images conditionnelles

Affichez différentes images selon les données :

```pascal
// Dans OnBeforePrint
begin
  case <frxDBDataset."niveau_priorite"> of
    1: Picture1.LoadFromFile('priorite_haute.png');
    2: Picture1.LoadFromFile('priorite_moyenne.png');
    3: Picture1.LoadFromFile('priorite_basse.png');
  end;
end;
```

## Rapports croisés (Crosstabs)

Les rapports croisés affichent des données agrégées en tableau à double entrée.

### Création d'un rapport croisé

**Exemple : Ventes par mois et par vendeur**

```
┌──────────────┬─────┬─────┬─────┬─────┬────────┐
│ Vendeur      │ Jan │ Fév │ Mar │ Avr │ Total  │
├──────────────┼─────┼─────┼─────┼─────┼────────┤
│ Dupont       │ 1500│ 1800│ 2100│ 1900│  7 300 │
│ Martin       │ 2200│ 1900│ 2300│ 2400│  8 800 │
│ Durand       │ 1700│ 2100│ 1850│ 2050│  7 700 │
├──────────────┼─────┼─────┼─────┼─────┼────────┤
│ Total        │ 5400│ 5800│ 6250│ 6350│ 23 800 │
└──────────────┴─────┴─────┴─────┴─────┴────────┘
```

### Configuration

1. Menu **Insert → Cross-table**
2. Configuration :
   - **Dataset** : votre source de données
   - **Row Field** : champ pour les lignes (vendeur)
   - **Column Field** : champ pour les colonnes (mois)
   - **Cell Field** : champ pour les valeurs (montant)
   - **Function** : Sum, Count, Avg, etc.
3. Options :
   - **Show Totals** : afficher les totaux
   - **Show Row/Column Headers** : afficher les en-têtes
   - **Auto Size** : ajustement automatique

### Formatage du crosstab

```pascal
// Dans l'événement OnBeforePrint des cellules
begin
  // Cellule de total en gras
  if Cell.IsTotalCell then
  begin
    Cell.Font.Style := [fsBold];
    Cell.Color := clYellow;
  end;

  // Mise en forme des valeurs
  Cell.Text := FormatFloat('#,##0', Cell.Value);
end;
```

### Crosstab avec plusieurs niveaux

Créez des crosstabs complexes avec sous-totaux :

- **Lignes** : Région → Vendeur
- **Colonnes** : Année → Mois
- **Valeurs** : Montant des ventes

## Rapports avec données en temps réel

### Actualisation automatique

Pour des rapports qui doivent refléter les données les plus récentes.

```pascal
procedure TForm1.ActualiserEtAfficherRapport;  
begin  
  // Rafraîchir les données
  FDQueryVentes.Close;
  FDQueryVentes.Open;

  // Afficher le rapport
  frxReport1.ShowReport;
end;

// Timer pour actualisation périodique
procedure TForm1.Timer1Timer(Sender: TObject);  
begin  
  ActualiserEtAfficherRapport;
end;
```

### Horodatage et traçabilité

Ajoutez des informations de génération :

```pascal
// Passer les informations au rapport
frxReport1.Variables['HeureGeneration'] := QuotedStr(TimeToStr(Time));  
frxReport1.Variables['UtilisateurGeneration'] := QuotedStr(GetCurrentUser);  
frxReport1.Variables['VersionRapport'] := QuotedStr('1.2.5');  
```

Dans le rapport (pied de page) :

```
Généré le [Date] à [HeureGeneration] par [UtilisateurGeneration]  
Version du rapport : [VersionRapport]  
```

## Optimisation des rapports complexes

### Performance

Pour les rapports volumineux (milliers de lignes) :

#### Utiliser le cache fichier

```pascal
frxReport1.EngineOptions.UseFileCache := True;  
frxReport1.EngineOptions.TempDir := GetTempDirectory;  
```

#### Limiter les sous-rapports

Évitez les sous-rapports dans des boucles de données importantes.

#### Optimiser les requêtes SQL

```sql
-- Mauvais : récupérer toutes les données puis filtrer
SELECT * FROM ventes

-- Bon : filtrer au niveau de la base
SELECT * FROM ventes  
WHERE date_vente >= '2024-01-01'  
  AND date_vente <= '2024-12-31'
ORDER BY date_vente
```

#### Désactiver les fonctionnalités inutiles

```pascal
frxReport1.EngineOptions.SilentMode := True;  // Pas de dialogues  
frxReport1.PreviewOptions.AllowEdit := False;  // Pas d'édition  
```

### Gestion de la mémoire

Pour les très gros rapports :

```pascal
// Libérer la mémoire après génération
frxReport1.PrepareReport(True);  // True = libérer les datasets après  
frxReport1.ShowPreparedReport;  

// Ou exporter directement
frxReport1.PrepareReport;  
frxReport1.Export(PDFExport);  
frxReport1.Clear;  // Libérer la mémoire  
```

### Génération asynchrone

Pour ne pas bloquer l'interface :

```pascal
procedure TForm1.GenererRapportAsync;  
begin  
  ProgressBar1.Visible := True;
  btnGenerer.Enabled := False;

  TTask.Run(
    procedure
    begin
      frxReport1.PrepareReport;

      TThread.Synchronize(nil,
        procedure
        begin
          frxReport1.ShowPreparedReport;
          ProgressBar1.Visible := False;
          btnGenerer.Enabled := True;
        end
      );
    end
  );
end;
```

## Modèles de rapports réutilisables

### Création d'un modèle

Un modèle de rapport contient la structure sans les données spécifiques.

**Exemple : Modèle de facture**

1. Créez le design complet de la facture
2. Utilisez des variables pour toutes les données spécifiques
3. Enregistrez comme `Modele_Facture.fr3`

### Utilisation du modèle

```pascal
procedure TForm1.GenererFacture(NumeroFacture: Integer);  
begin  
  // Charger le modèle
  frxReport1.LoadFromFile('Modele_Facture.fr3');

  // Récupérer les données de cette facture
  FDQueryFacture.ParamByName('numero').AsInteger := NumeroFacture;
  FDQueryFacture.Open;

  // Passer les variables
  frxReport1.Variables['NumeroFacture'] := QuotedStr(IntToStr(NumeroFacture));
  frxReport1.Variables['DateFacture'] := QuotedStr(DateToStr(Date));

  // Générer
  frxReport1.ShowReport;
end;
```

### Bibliothèque de modèles

Organisez vos modèles :

```
Modeles/
├── Factures/
│   ├── Facture_Standard.fr3
│   ├── Facture_Export.fr3
│   └── Avoir.fr3
├── Rapports/
│   ├── Ventes_Mensuelles.fr3
│   ├── Stock_Complet.fr3
│   └── Analyse_Client.fr3
└── Etiquettes/
    ├── Etiquette_Produit.fr3
    └── Badge_Personnel.fr3
```

## Rapports paramétrables

### Interface de sélection

Créez un formulaire pour configurer le rapport avant génération.

```pascal
type
  TFormParametresRapport = class(TForm)
    DateEdit1: TDateTimePicker;
    DateEdit2: TDateTimePicker;
    ComboCategorie: TComboBox;
    CheckDetaille: TCheckBox;
    CheckGraphiques: TCheckBox;
    btnGenerer: TButton;
  end;

procedure TFormParametresRapport.btnGenererClick(Sender: TObject);  
begin  
  // Passer les paramètres au rapport
  frxReport1.Variables['DateDebut'] := QuotedStr(DateToStr(DateEdit1.Date));
  frxReport1.Variables['DateFin'] := QuotedStr(DateToStr(DateEdit2.Date));
  frxReport1.Variables['Categorie'] := QuotedStr(ComboCategorie.Text);
  frxReport1.Variables['ModeDetaille'] := BoolToStr(CheckDetaille.Checked, True);
  frxReport1.Variables['AfficherGraphiques'] := BoolToStr(CheckGraphiques.Checked, True);

  // Configurer le rapport selon les options
  if not CheckGraphiques.Checked then
  begin
    // Cacher la section graphiques
    frxReport1.FindObject('BandGraphiques').Visible := False;
  end;

  // Générer le rapport
  frxReport1.ShowReport;
  Close;
end;
```

### Rapport adaptatif

Le rapport s'adapte selon les paramètres :

```pascal
// Dans l'événement OnStartReport du rapport
begin
  // Afficher ou cacher des sections selon les paramètres
  if <ModeDetaille> = 'False' then
  begin
    DetailBand.Visible := False;
    SyntheseBand.Visible := True;
  end;

  // Modifier le titre selon les paramètres
  if <Categorie> <> 'Toutes' then
    TitreRapport := 'Rapport pour la catégorie : ' + <Categorie>
  else
    TitreRapport := 'Rapport toutes catégories';
end;
```

## Export avancé

### Export avec options personnalisées

#### PDF avec options

```pascal
procedure TForm1.ExporterPDFAvecOptions;  
begin  
  PDFExport := TfrxPDFExport.Create(nil);
  try
    // Options de qualité
    PDFExport.Quality := 95;
    PDFExport.EmbeddedFonts := True;
    PDFExport.Compressed := True;

    // Métadonnées
    PDFExport.Title := 'Rapport des ventes';
    PDFExport.Author := 'MonEntreprise';
    PDFExport.Subject := 'Analyse mensuelle';
    PDFExport.Keywords := 'ventes, statistiques, 2024';
    PDFExport.Creator := 'Application Gestion v2.0';

    // Sécurité
    PDFExport.UserPassword := 'lecture';
    PDFExport.OwnerPassword := 'admin';
    PDFExport.PrintAllowed := True;
    PDFExport.ModifyAllowed := False;

    // Export
    frxReport1.PrepareReport;
    PDFExport.FileName := 'Rapport_Ventes.pdf';
    frxReport1.Export(PDFExport);
  finally
    PDFExport.Free;
  end;
end;
```

#### Excel avec formatage

```pascal
procedure TForm1.ExporterExcelAvecFormat;  
begin  
  ExcelExport := TfrxXLSXExport.Create(nil);
  try
    // Options
    ExcelExport.Wysiwyg := True;  // Reproduire la mise en forme
    ExcelExport.PageBreaks := True;  // Respecter les sauts de page
    ExcelExport.ChunkSize := 50;  // Performance
    ExcelExport.ExportPictures := True;
    ExcelExport.ExportFormulas := False;

    // Export
    frxReport1.PrepareReport;
    ExcelExport.FileName := 'Rapport_Ventes.xlsx';
    frxReport1.Export(ExcelExport);
  finally
    ExcelExport.Free;
  end;
end;
```

### Export vers plusieurs formats simultanément

```pascal
procedure TForm1.ExporterMultiFormat(const CheminBase: string);  
begin  
  frxReport1.PrepareReport;

  // PDF
  PDFExport.FileName := CheminBase + '.pdf';
  frxReport1.Export(PDFExport);

  // Excel
  ExcelExport.FileName := CheminBase + '.xlsx';
  frxReport1.Export(ExcelExport);

  // HTML
  HTMLExport.FileName := CheminBase + '.html';
  frxReport1.Export(HTMLExport);

  ShowMessage('Rapport exporté en 3 formats');
end;
```

## Conseils et bonnes pratiques

### Conception

- **Planification** : dessinez d'abord le rapport sur papier
- **Simplicité** : ne surchargez pas la mise en page
- **Cohérence** : utilisez une charte graphique uniforme
- **Hiérarchie visuelle** : guidez l'œil avec la taille, les couleurs
- **Espacement** : aérez pour améliorer la lisibilité

### Performance

- **Requêtes optimisées** : filtrez au niveau SQL
- **Pagination** : limitez le nombre de lignes si possible
- **Index** : assurez-vous que les tables sont bien indexées
- **Cache** : utilisez le cache fichier pour les gros rapports
- **Libération mémoire** : nettoyez après génération

### Maintenance

- **Versionnage** : gardez l'historique des versions
- **Documentation** : documentez les variables et paramètres
- **Tests** : testez avec différents jeux de données
- **Nommage** : noms explicites pour les objets du rapport
- **Modularité** : réutilisez les modèles et sous-rapports

### Sécurité

- **Validation** : vérifiez les paramètres avant génération
- **Droits** : contrôlez l'accès aux rapports sensibles
- **Logs** : journalisez les générations de rapports
- **Protection PDF** : utilisez les mots de passe si nécessaire
- **Données sensibles** : masquez les informations confidentielles

## Exemple complet : Rapport de gestion mensuel

Voici un exemple qui combine plusieurs techniques :

```pascal
unit URapportGestion;

interface

uses
  System.SysUtils, System.Classes, Data.DB, FireDAC.Comp.Client,
  frxClass, frxDBSet, frxExportPDF;

type
  TFormRapportGestion = class(TForm)
    frxReport: TfrxReport;
    frxDBVentes: TfrxDBDataset;
    frxDBDepenses: TfrxDBDataset;
    frxDBObjectifs: TfrxDBDataset;
    FDQueryVentes: TFDQuery;
    FDQueryDepenses: TFDQuery;
    FDQueryObjectifs: TFDQuery;

    procedure GenererRapport(Mois, Annee: Integer);
  end;

implementation

procedure TFormRapportGestion.GenererRapport(Mois, Annee: Integer);  
var  
  DateDebut, DateFin: TDate;
  CheminRapport: string;
begin
  // Calculer les dates
  DateDebut := EncodeDate(Annee, Mois, 1);
  DateFin := EndOfTheMonth(DateDebut);

  // Préparer les requêtes
  FDQueryVentes.Close;
  FDQueryVentes.ParamByName('date_debut').AsDate := DateDebut;
  FDQueryVentes.ParamByName('date_fin').AsDate := DateFin;
  FDQueryVentes.Open;

  FDQueryDepenses.Close;
  FDQueryDepenses.ParamByName('date_debut').AsDate := DateDebut;
  FDQueryDepenses.ParamByName('date_fin').AsDate := DateFin;
  FDQueryDepenses.Open;

  FDQueryObjectifs.Close;
  FDQueryObjectifs.ParamByName('mois').AsInteger := Mois;
  FDQueryObjectifs.ParamByName('annee').AsInteger := Annee;
  FDQueryObjectifs.Open;

  // Calculer les indicateurs
  var TotalVentes := FDQueryVentes.FieldByName('total').AsFloat;
  var TotalDepenses := FDQueryDepenses.FieldByName('total').AsFloat;
  var Benefice := TotalVentes - TotalDepenses;
  var ObjectifMois := FDQueryObjectifs.FieldByName('objectif').AsFloat;
  var TauxReussite := (TotalVentes / ObjectifMois) * 100;

  // Charger et configurer le rapport
  frxReport.LoadFromFile('RapportGestion.fr3');

  // Passer les variables
  frxReport.Variables['Mois'] := QuotedStr(FormatSettings.LongMonthNames[Mois]);
  frxReport.Variables['Annee'] := IntToStr(Annee);
  frxReport.Variables['TotalVentes'] := FloatToStr(TotalVentes);
  frxReport.Variables['TotalDepenses'] := FloatToStr(TotalDepenses);
  frxReport.Variables['Benefice'] := FloatToStr(Benefice);
  frxReport.Variables['TauxReussite'] := FloatToStr(TauxReussite);

  // Générer et afficher
  frxReport.ShowReport;

  // Exporter automatiquement en PDF
  CheminRapport := Format('Rapports\Gestion_%d_%d.pdf', [Annee, Mois]);
  var PDFExport := TfrxPDFExport.Create(nil);
  try
    PDFExport.FileName := CheminRapport;
    PDFExport.ShowDialog := False;
    frxReport.Export(PDFExport);
  finally
    PDFExport.Free;
  end;
end;

end.
```

## Résumé

La création de rapports complexes demande de maîtriser plusieurs techniques avancées. Les points clés :

- **Structuration multiniveau** : groupes imbriqués avec sous-totaux à chaque niveau
- **Sources multiples** : combinaison de différentes sources de données
- **Calculs avancés** : agrégations, pourcentages, cumuls et variables personnalisées
- **Mise en page sophistiquée** : colonnes, sauts de page intelligents, positionnement dynamique
- **Éléments visuels** : graphiques multiples, codes-barres, images dynamiques
- **Interactivité** : signets, hyperliens, table des matières
- **Performance** : optimisation pour les gros volumes de données
- **Réutilisabilité** : modèles paramétrables et adaptables

Les rapports complexes sont essentiels pour les applications professionnelles. En maîtrisant ces techniques, vous pouvez créer des documents sophistiqués qui répondent aux besoins métier les plus exigeants.

Dans la prochaine section, nous découvrirons comment créer des graphiques et visualisations de données encore plus avancés pour enrichir vos rapports.

⏭️ [Graphiques et visualisations de données](/09-rapports-et-impressions/05-graphiques-et-visualisations-de-donnees.md)
