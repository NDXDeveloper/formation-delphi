🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 9.3 Générateurs de rapports (FastReport, QuickReport)

## Introduction

Les générateurs de rapports sont des outils spécialisés qui facilitent considérablement la création de documents imprimés complexes. Alors que les composants d'impression natifs de Delphi conviennent pour des impressions simples, les générateurs de rapports offrent des fonctionnalités avancées pour créer des rapports professionnels avec mise en page sophistiquée, graphiques, sous-rapports et bien plus encore.

## Pourquoi utiliser un générateur de rapports ?

### Avantages par rapport à l'impression manuelle

Les générateurs de rapports offrent de nombreux avantages :

- **Conception visuelle** : interface graphique pour créer la mise en page sans coder
- **Aperçu intégré** : prévisualisation professionnelle avec zoom, navigation, etc.
- **Bandes de données** : gestion automatique des en-têtes, détails, pieds de page
- **Liaison aux données** : connexion directe aux bases de données et datasets
- **Export multiple** : PDF, Excel, Word, HTML, XML et autres formats
- **Sous-rapports** : inclusion de rapports dans d'autres rapports
- **Graphiques** : intégration de graphiques et visualisations
- **Calculs automatiques** : sommes, moyennes, compteurs automatiques
- **Formatage conditionnel** : mise en forme basée sur les données
- **Réutilisabilité** : les rapports sont stockés dans des fichiers séparés

### Cas d'utilisation typiques

Les générateurs de rapports sont parfaits pour :

- Factures et devis
- Listes et tableaux de données
- Rapports financiers
- Bulletins et relevés
- Étiquettes et badges
- Documents complexes multi-pages
- Rapports avec graphiques et statistiques

## Vue d'ensemble des solutions disponibles

### FastReport

**FastReport** est actuellement le générateur de rapports le plus populaire pour Delphi.

**Points forts :**
- Interface moderne et intuitive
- Excellent support et documentation
- Mise à jour régulière
- Large gamme de formats d'export
- Support multi-plateforme (VCL et FMX)
- Communauté active

**Éditions :**
- **FastReport VCL** : pour applications Windows VCL
- **FastReport FMX** : pour applications multi-plateformes FireMonkey
- **FastReport Server** : serveur de rapports centralisé
- **Version d'évaluation** : disponible gratuitement pour tester

**Site officiel :** https://www.fast-report.com

### QuickReport

**QuickReport** est un générateur de rapports qui a été inclus gratuitement dans certaines versions de Delphi.

**Points forts :**
- Gratuit avec Delphi (versions anciennes)
- Simple à apprendre
- Bien intégré à l'IDE Delphi
- Léger et rapide

**Points faibles :**
- Plus maintenu activement
- Fonctionnalités limitées par rapport à FastReport
- Export limité (principalement PDF)
- Interface vieillissante

**Disponibilité :**
QuickReport était inclus dans Delphi jusqu'à la version XE, puis retiré. Il existe des versions tierces comme QR5 disponibles séparément.

### Autres alternatives

D'autres solutions existent également :

- **ReportBuilder** : solution professionnelle très complète
- **Rave Reports** : inclus dans certaines versions de Delphi
- **DevExpress Reports** : si vous utilisez la suite DevExpress
- **FreeReport** : version open source de FastReport (ancienne)

## Installation de FastReport

### Téléchargement

1. Rendez-vous sur le site de FastReport : https://www.fast-report.com
2. Téléchargez la version d'évaluation ou achetez une licence
3. Exécutez l'installeur téléchargé

### Installation

L'installation de FastReport est simple :

1. Lancez l'installeur
2. Choisissez le répertoire d'installation
3. Sélectionnez votre version de Delphi
4. L'installeur compile et installe les packages automatiquement
5. Redémarrez Delphi

### Vérification de l'installation

Après installation, vous devriez voir :

- Un nouvel onglet **FastReport 6** dans la palette de composants
- Les composants principaux : `TfrxReport`, `TfrxDBDataset`, `TfrxDesigner`, etc.
- Le menu **Tools → FastReport** dans l'IDE

## Concepts fondamentaux

### Architecture d'un rapport

Un rapport FastReport est composé de plusieurs éléments :

#### 1. Le rapport (Report)

C'est le conteneur principal représenté par le composant `TfrxReport`. Il contient toutes les pages et éléments du rapport.

#### 2. Les pages (Pages)

Un rapport peut contenir plusieurs pages. Chaque page a :
- Des dimensions (A4, Letter, etc.)
- Une orientation (portrait ou paysage)
- Des marges

#### 3. Les bandes (Bands)

Les bandes définissent les zones du rapport :

- **Report Title** : titre du rapport (une seule fois au début)
- **Page Header** : en-tête répété sur chaque page
- **Master Data** : bande de détails (répétée pour chaque enregistrement)
- **Page Footer** : pied de page répété sur chaque page
- **Report Summary** : résumé final du rapport

#### 4. Les objets (Objects)

Les objets sont les éléments visuels placés dans les bandes :

- **Memo** : texte statique ou lié aux données
- **Picture** : images
- **Line** : lignes de séparation
- **Shape** : formes géométriques
- **Barcode** : codes-barres
- **Chart** : graphiques

### Sources de données

FastReport peut se connecter à plusieurs types de sources :

- **Datasets Delphi** : via `TfrxDBDataset`
- **Variables** : données passées par code
- **Requêtes internes** : requêtes SQL dans le rapport
- **Listes et collections** : données en mémoire

## Premier rapport avec FastReport

### Préparation du formulaire

1. Créez un nouveau projet VCL
2. Ajoutez un formulaire principal
3. Placez les composants suivants :
   - `TfrxReport` (onglet FastReport 6) nommé `frxReport1`
   - `TButton` nommé `btnAfficherRapport` avec Caption = 'Afficher le rapport'

### Conception du rapport

Double-cliquez sur le composant `frxReport1` pour ouvrir le designer.

#### Interface du designer

Le designer FastReport se compose de :

- **Barre d'outils** : outils de conception
- **Palette d'objets** : objets à placer dans le rapport
- **Zone de conception** : représentation visuelle du rapport
- **Inspecteur d'objets** : propriétés des objets sélectionnés
- **Arbre des objets** : structure hiérarchique du rapport

#### Création d'un rapport simple

**Étape 1 : Ajouter une bande Report Title**

1. Cliquez sur le bouton **Insert Band** (icône avec bandes)
2. Sélectionnez **Report Title**
3. Une bande bleue apparaît en haut de la page

**Étape 2 : Ajouter du texte**

1. Cliquez sur l'icône **Text** (A) dans la palette
2. Cliquez dans la bande Report Title
3. Un objet Memo apparaît
4. Double-cliquez dessus pour éditer
5. Tapez : "Mon premier rapport FastReport"
6. Fermez l'éditeur de texte

**Étape 3 : Formater le titre**

1. Sélectionnez l'objet Memo
2. Dans la barre d'outils, changez :
   - Police : Arial
   - Taille : 18
   - Style : Gras
3. Alignez au centre avec le bouton d'alignement

**Étape 4 : Enregistrer le rapport**

1. Cliquez sur **File → Save**
2. Enregistrez le rapport : `MonPremierRapport.fr3`

### Affichage du rapport

Retournez dans Delphi et écrivez le code pour afficher le rapport :

```pascal
procedure TForm1.btnAfficherRapportClick(Sender: TObject);
begin
  // Charger le fichier rapport
  frxReport1.LoadFromFile('MonPremierRapport.fr3');

  // Afficher l'aperçu
  frxReport1.ShowReport;
end;
```

**Alternative sans fichier externe :**

Vous pouvez aussi concevoir le rapport directement dans le composant :

```pascal
procedure TForm1.btnAfficherRapportClick(Sender: TObject);
begin
  // Le rapport est déjà dans le composant
  frxReport1.ShowReport;
end;
```

## Rapport avec données de base de données

### Configuration de la source de données

**Étape 1 : Préparer les composants**

Ajoutez sur votre formulaire :
- `TFDConnection` pour la connexion
- `TFDQuery` nommé `FDQueryClients` avec une requête SQL
- `TDataSource` nommé `DataSourceClients`
- `TfrxDBDataset` nommé `frxDBDatasetClients`

**Configuration :**

```pascal
// Configuration de la connexion (déjà vue dans le chapitre 8)
FDConnection1.DriverName := 'MySQL';
// ... autres paramètres de connexion

// Requête
FDQueryClients.Connection := FDConnection1;
FDQueryClients.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
FDQueryClients.Open;

// DataSource
DataSourceClients.DataSet := FDQueryClients;

// FastReport Dataset
frxDBDatasetClients.DataSet := FDQueryClients;
```

**Étape 2 : Configurer le rapport**

1. Double-cliquez sur `frxReport1` pour ouvrir le designer
2. Cliquez sur **Report → Data** dans le menu
3. La fenêtre des sources de données s'ouvre
4. Vous devriez voir `frxDBDatasetClients` dans la liste
5. Cochez la case pour l'activer
6. Cliquez sur **OK**

### Création d'un rapport maître-détail

**Étape 1 : Ajouter les bandes**

1. Ajoutez une bande **Report Title** pour le titre
2. Ajoutez une bande **Page Header** pour les en-têtes de colonnes
3. Ajoutez une bande **Master Data** pour les données
4. Ajoutez une bande **Page Footer** pour la numérotation

**Étape 2 : Configurer la bande Master Data**

1. Sélectionnez la bande **Master Data**
2. Dans l'Inspecteur d'objets, propriété **DataSet**, sélectionnez `frxDBDatasetClients`

**Étape 3 : Ajouter les champs**

1. Dans la fenêtre **Data Tree** (arbre des données), développez `frxDBDatasetClients`
2. Vous voyez tous les champs de la table
3. Glissez-déposez les champs dans la bande Master Data :
   - `nom` dans la première colonne
   - `prenom` dans la deuxième colonne
   - `email` dans la troisième colonne

**Étape 4 : En-têtes de colonnes**

Dans la bande **Page Header**, ajoutez des objets Memo avec :
- "Nom" au-dessus de la colonne nom
- "Prénom" au-dessus de la colonne prénom
- "Email" au-dessus de la colonne email

Mettez ces en-têtes en gras.

**Étape 5 : Numérotation des pages**

Dans la bande **Page Footer**, ajoutez un Memo avec :
```
Page [Page] sur [TotalPages]
```

Les expressions entre crochets sont automatiquement remplacées par FastReport.

### Code complet

```pascal
procedure TForm1.btnRapportClientsClick(Sender: TObject);
begin
  try
    // Ouvrir la connexion et la requête
    FDConnection1.Connected := True;
    FDQueryClients.Open;

    // Charger et afficher le rapport
    frxReport1.LoadFromFile('RapportClients.fr3');
    frxReport1.ShowReport;
  except
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Formatage et expressions

### Expressions FastReport

FastReport utilise un langage de script (PascalScript) pour les expressions :

#### Champs de données

```
[frxDBDatasetClients."nom"]
```

#### Variables système

```
[Page]           // Numéro de page actuelle
[TotalPages]     // Nombre total de pages
[Date]           // Date du jour
[Time]           // Heure actuelle
[Line]           // Numéro de ligne
```

#### Opérations

```
[<Total>]        // Somme d'un champ
[<Count>]        // Nombre d'enregistrements
[<Avg>]          // Moyenne
[<Min>]          // Minimum
[<Max>]          // Maximum
```

### Formatage des nombres

Pour formater un nombre avec 2 décimales :

```
[FormatFloat('#,##0.00', <frxDBDatasetFactures."montant">)]
```

### Formatage des dates

Pour formater une date :

```
[FormatDateTime('dd/mm/yyyy', <frxDBDatasetFactures."date">)]
```

### Expressions conditionnelles

Vous pouvez utiliser des conditions :

```
[IIF(<frxDBDatasetFactures."montant"> > 1000, 'Élevé', 'Normal')]
```

## Mise en forme conditionnelle

### Changer la couleur selon une condition

**Exemple : colorer en rouge les montants supérieurs à 1000 €**

1. Sélectionnez l'objet Memo contenant le montant
2. Cliquez sur **Highlight** dans la barre d'outils
3. Cliquez sur **Add** pour ajouter une condition
4. Configurez :
   - **Condition** : `<frxDBDatasetFactures."montant"> > 1000`
   - **Font Color** : clRed
5. Cliquez sur **OK**

### Bandes alternées

Pour alterner les couleurs de fond des lignes :

1. Sélectionnez la bande **Master Data**
2. Dans l'Inspecteur d'objets, trouvez **OnBeforePrint**
3. Double-cliquez pour créer l'événement
4. Ajoutez le code :

```pascal
begin
  if <Line> mod 2 = 0 then
    MasterData1.Color := clWhite
  else
    MasterData1.Color := $00F0F0F0;  // Gris clair
end;
```

## Groupes et sous-totaux

### Créer un groupe

Pour grouper par catégorie par exemple :

**Étape 1 : Ajouter les bandes de groupe**

1. Cliquez sur **Insert Band**
2. Sélectionnez **Group Header**
3. Dans la fenêtre qui s'ouvre :
   - **Dataset** : sélectionnez votre dataset
   - **Group Condition** : `<frxDBDataset."categorie">`
4. Cochez **Add Group Footer** pour avoir un pied de groupe

**Étape 2 : Configurer l'en-tête de groupe**

1. Dans la bande **Group Header**, ajoutez un Memo
2. Contenu : `Catégorie : [frxDBDataset."categorie"]`
3. Mettez-le en gras

**Étape 3 : Ajouter des sous-totaux**

1. Dans la bande **Group Footer**, ajoutez un Memo
2. Contenu : `Total : [SUM(<frxDBDataset."montant">,MasterData1)]`

La fonction `SUM` calcule automatiquement le total pour chaque groupe.

## Sous-rapports

Les sous-rapports permettent d'inclure un rapport dans un autre.

### Utilisation typique

Exemple : une facture avec les détails des lignes

**Rapport principal :** informations de la facture (client, date, numéro)
**Sous-rapport :** lignes de la facture (articles, quantités, prix)

### Création d'un sous-rapport

**Étape 1 : Ajouter l'objet Subreport**

1. Dans la palette, cliquez sur **Subreport**
2. Placez-le dans votre rapport principal
3. Double-cliquez dessus pour ouvrir son designer

**Étape 2 : Concevoir le sous-rapport**

1. Le sous-rapport a sa propre structure avec ses bandes
2. Ajoutez une bande **Master Data**
3. Liez-la à votre dataset de détails
4. Ajoutez les champs nécessaires

**Étape 3 : Liaison maître-détail**

```pascal
// Dans le code
frxDBDatasetFactures.DataSet := FDQueryFactures;
frxDBDatasetLignes.DataSet := FDQueryLignes;

// Définir la relation
FDQueryLignes.MasterSource := DataSourceFactures;
FDQueryLignes.MasterFields := 'id_facture';
```

## Graphiques dans les rapports

FastReport permet d'intégrer des graphiques facilement.

### Ajout d'un graphique

**Étape 1 : Insérer un graphique**

1. Cliquez sur l'icône **Chart** dans la palette
2. Placez-le dans une bande (Report Summary par exemple)
3. Redimensionnez-le

**Étape 2 : Configurer le graphique**

1. Double-cliquez sur le graphique
2. L'éditeur de graphique s'ouvre
3. Onglet **Data** :
   - **Dataset** : sélectionnez votre source
   - **Value** : champ pour les valeurs
   - **Argument** : champ pour les étiquettes
4. Onglet **Series** :
   - Choisissez le type (camembert, barres, courbes, etc.)
5. Onglet **Options** :
   - Personnalisez l'apparence

### Exemple de graphique

Graphique des ventes par mois :

- **Dataset** : requête avec les ventes mensuelles
- **Value** : `total_ventes`
- **Argument** : `mois`
- **Type** : Bar Chart (histogramme)

## Export de rapports

FastReport offre de nombreux formats d'export.

### Export via l'aperçu

Lorsque l'aperçu est affiché, l'utilisateur peut :

1. Cliquer sur le bouton **Export**
2. Choisir le format (PDF, Excel, Word, etc.)
3. Sélectionner l'emplacement
4. Le rapport est exporté

### Export par code

Vous pouvez aussi exporter directement par code :

#### Export en PDF

```pascal
uses
  frxClass, frxExportPDF;

procedure TForm1.ExporterEnPDF;
var
  PDFExport: TfrxPDFExport;
begin
  PDFExport := TfrxPDFExport.Create(nil);
  try
    frxReport1.LoadFromFile('MonRapport.fr3');
    frxReport1.PrepareReport;  // Préparer le rapport

    PDFExport.FileName := 'C:\Rapports\MonRapport.pdf';
    PDFExport.ShowDialog := False;  // Pas de dialogue
    PDFExport.DefaultPath := 'C:\Rapports';

    frxReport1.Export(PDFExport);

    ShowMessage('Rapport exporté en PDF');
  finally
    PDFExport.Free;
  end;
end;
```

#### Export en Excel

```pascal
uses
  frxClass, frxExportXLS;

procedure TForm1.ExporterEnExcel;
var
  ExcelExport: TfrxXLSExport;
begin
  ExcelExport := TfrxXLSExport.Create(nil);
  try
    frxReport1.LoadFromFile('MonRapport.fr3');
    frxReport1.PrepareReport;

    ExcelExport.FileName := 'C:\Rapports\MonRapport.xls';
    ExcelExport.ShowDialog := False;
    ExcelExport.OpenAfterExport := True;  // Ouvrir automatiquement

    frxReport1.Export(ExcelExport);
  finally
    ExcelExport.Free;
  end;
end;
```

#### Autres formats disponibles

- **HTML** : `TfrxHTMLExport`
- **RTF (Word)** : `TfrxRTFExport`
- **XML** : `TfrxXMLExport`
- **CSV** : `TfrxCSVExport`
- **Image** : `TfrxBMPExport`, `TfrxJPEGExport`, `TfrxPNGExport`

## Passage de paramètres au rapport

Vous pouvez passer des variables au rapport depuis votre code.

### Déclaration de variables

**Dans le designer FastReport :**

1. Cliquez sur **Report → Variables**
2. Cliquez sur **Add Category** : nommez-la "Parametres"
3. Cliquez sur **Add** pour ajouter une variable
4. Nom : `NomClient`, Type : `String`

### Affectation par code

```pascal
procedure TForm1.AfficherRapportAvecParametre;
begin
  frxReport1.LoadFromFile('MonRapport.fr3');

  // Affecter la valeur de la variable
  frxReport1.Variables['NomClient'] := QuotedStr('Dupont');
  frxReport1.Variables['DateRapport'] := QuotedStr(DateToStr(Date));

  frxReport1.ShowReport;
end;
```

### Utilisation dans le rapport

Dans un Memo du rapport, utilisez :

```
Client : [NomClient]
Date : [DateRapport]
```

## QuickReport : présentation

Bien que moins utilisé aujourd'hui, QuickReport mérite d'être mentionné car il est encore présent dans des applications existantes.

### Composants principaux

- **TQuickRep** : composant principal du rapport
- **TQRBand** : bandes du rapport
- **TQRLabel** : texte statique
- **TQRDBText** : texte lié aux données
- **TQRImage** : images
- **TQRShape** : formes
- **TQRChart** : graphiques

### Structure d'un rapport QuickReport

Un rapport QuickReport se construit directement sur un formulaire Delphi :

1. Créez un nouveau formulaire
2. Placez un composant `TQuickRep`
3. Ajoutez des bandes (`TQRBand`)
4. Placez des composants dans les bandes

### Exemple simple avec QuickReport

```pascal
// Sur le formulaire principal
procedure TForm1.btnRapportQRClick(Sender: TObject);
begin
  FormRapportQR := TFormRapportQR.Create(Self);
  try
    FormRapportQR.QuickRep1.Preview;
  finally
    FormRapportQR.Free;
  end;
end;

// Sur le formulaire du rapport
procedure TFormRapportQR.QuickRep1BeforePrint(Sender: TCustomQuickRep; var PrintReport: Boolean);
begin
  // Préparation du rapport
  QRLabel1.Caption := 'Rapport du ' + DateToStr(Date);
end;
```

### Liaison aux données

```pascal
// Configuration
QuickRep1.DataSet := FDQueryClients;
QRDBText1.DataSet := FDQueryClients;
QRDBText1.DataField := 'nom';
```

### Limitations de QuickReport

- Interface moins moderne que FastReport
- Fonctionnalités d'export limitées
- Plus maintenu activement
- Moins flexible pour les mises en page complexes

## Comparaison FastReport vs QuickReport

| Critère | FastReport | QuickReport |
|---------|-----------|-------------|
| **Interface** | Moderne, designer séparé | Intégrée à l'IDE |
| **Facilité** | Courbe d'apprentissage moyenne | Plus simple pour débuter |
| **Exports** | Nombreux formats (PDF, Excel, etc.) | Limité (principalement PDF) |
| **Prix** | Payant (version d'évaluation) | Gratuit (si inclus dans Delphi) |
| **Maintenance** | Mise à jour régulière | Plus maintenu |
| **Fonctionnalités** | Très complet | Basique |
| **Performance** | Excellente | Bonne |
| **Support** | Actif, documentation complète | Limité |

### Recommandation

- **Pour nouveaux projets** : FastReport
- **Pour maintenance d'anciens projets** : conserver QuickReport si déjà utilisé
- **Pour rapports simples** : composants natifs Delphi suffisent
- **Pour rapports complexes** : FastReport est le meilleur choix

## Fonctionnalités avancées de FastReport

### Scripts dans les rapports

FastReport intègre un moteur de script PascalScript qui permet d'ajouter de la logique.

**Exemple : calcul personnalisé**

1. Sélectionnez un objet Memo
2. Dans l'Inspecteur d'objets, trouvez l'événement **OnBeforePrint**
3. Cliquez sur le bouton [...] pour ouvrir l'éditeur de script
4. Écrivez votre code :

```pascal
begin
  if <frxDBDataset."montant"> > 1000 then
    Memo1.Text := 'Montant élevé : ' + FormatFloat('#,##0.00', <frxDBDataset."montant">)
  else
    Memo1.Text := FormatFloat('#,##0.00', <frxDBDataset."montant">);
end;
```

### Rapports maître-détail-détail

Vous pouvez créer des structures complexes :

- **Niveau 1** : Catégories
- **Niveau 2** : Produits par catégorie
- **Niveau 3** : Ventes par produit

Chaque niveau a ses propres bandes Header/Footer avec sous-totaux.

### Codes-barres

FastReport supporte de nombreux types de codes-barres :

1. Cliquez sur l'icône **Barcode**
2. Placez-le dans le rapport
3. Double-cliquez pour configurer :
   - **Type** : EAN13, Code128, QR Code, etc.
   - **Text** : `[frxDBDataset."code_article"]`
   - **Options** : taille, bordures, etc.

### Rapports croisés (crosstabs)

Les rapports croisés affichent des données en tableau à double entrée.

**Exemple :** Ventes par mois et par vendeur

1. Cliquez sur **Insert → Cross-table**
2. Configurez :
   - **Rows** : vendeur
   - **Columns** : mois
   - **Cells** : total des ventes
3. FastReport génère automatiquement le tableau

### Watermark (filigrane)

Ajoutez un filigrane à vos pages :

1. Sélectionnez la page
2. Dans l'Inspecteur d'objets, trouvez **Watermark**
3. Configurez :
   - **Enabled** : True
   - **Text** : "CONFIDENTIEL"
   - **Font** : taille, couleur, angle

## Optimisation des performances

### Rapports volumineux

Pour les rapports avec beaucoup de données :

**Mode Double-Pass**

```pascal
frxReport1.EngineOptions.DoublePass := True;
```

Permet de faire deux passes : une pour compter, une pour imprimer.

**Gestion de la mémoire**

```pascal
frxReport1.EngineOptions.UseFileCache := True;
frxReport1.EngineOptions.TempDir := 'C:\Temp';
```

Utilise le disque plutôt que la mémoire pour les gros rapports.

### Préparation en arrière-plan

```pascal
procedure TForm1.PreparerRapportAsync;
begin
  TTask.Run(
    procedure
    begin
      frxReport1.PrepareReport;

      TThread.Synchronize(nil,
        procedure
        begin
          frxReport1.ShowPreparedReport;
        end
      );
    end
  );
end;
```

## Sécurité des rapports

### Protection par mot de passe

Protégez vos fichiers de rapport :

```pascal
frxReport1.Password := 'MonMotDePasse';
frxReport1.SaveToFile('RapportProtege.fr3');
```

Pour charger :

```pascal
frxReport1.Password := 'MonMotDePasse';
frxReport1.LoadFromFile('RapportProtege.fr3');
```

### PDF avec restrictions

Créez des PDF protégés :

```pascal
PDFExport.UserPassword := 'lecture';
PDFExport.OwnerPassword := 'admin';
PDFExport.ProtectionFlags := [ePrint, eModify];
```

## Internationalisation

### Rapports multilingues

**Approche 1 : Variables**

```pascal
frxReport1.Variables['TitreRapport'] := QuotedStr(GetTranslation('titre'));
```

**Approche 2 : Fichiers séparés**

Créez un rapport par langue :
- `Rapport_FR.fr3`
- `Rapport_EN.fr3`
- `Rapport_ES.fr3`

Chargez le bon fichier selon la langue :

```pascal
var
  LangueCode: string;
begin
  LangueCode := 'FR';  // Déterminé selon les préférences
  frxReport1.LoadFromFile('Rapport_' + LangueCode + '.fr3');
end;
```

## Déploiement

### Fichiers nécessaires

Pour distribuer votre application avec FastReport :

**DLLs requises :**
- `fs26.bpl` : moteur de script
- `fsDB26.bpl` : accès aux données
- `frx26.bpl` : moteur FastReport
- `frxDB26.bpl` : composants base de données
- `frxe26.bpl` : composants export

**Remarque :** Le nombre (26) correspond à la version de Delphi.

### Installation minimale

Pour une distribution sans installation :
1. Copiez les BPL dans le dossier de l'application
2. Incluez les fichiers `.fr3` des rapports
3. Vérifiez les licences FastReport pour la redistribution

## Conseils et bonnes pratiques

### Conception de rapports

- **Simplicité** : ne surchargez pas la mise en page
- **Cohérence** : utilisez les mêmes polices et couleurs
- **Lisibilité** : marges suffisantes, texte bien espacé
- **Tests** : testez avec différents volumes de données
- **Performance** : optimisez les requêtes SQL

### Organisation

- **Fichiers séparés** : stockez les rapports dans des fichiers `.fr3`
- **Nomenclature** : nommez clairement vos rapports
- **Versions** : utilisez un système de contrôle de version
- **Documentation** : documentez les paramètres et variables

### Développement

- **Réutilisabilité** : créez des modèles de rapports
- **Modularité** : utilisez des sous-rapports pour les parties communes
- **Variables** : utilisez des variables plutôt que du texte codé en dur
- **Tests** : testez tous les cas (données vides, beaucoup de données, etc.)

## Résumé

Les générateurs de rapports sont des outils indispensables pour créer des documents professionnels. Les points clés :

- **FastReport** est le choix privilégié pour les nouveaux projets Delphi
- **Designer visuel** facilite grandement la création de rapports
- **Liaison aux données** automatique avec les datasets Delphi
- **Export multiple** vers PDF, Excel, Word et autres formats
- **Fonctionnalités avancées** : graphiques, sous-rapports, codes-barres
- **Scripts intégrés** pour la logique personnalisée
- **Performance optimisée** pour les gros volumes de données

Dans la prochaine section, nous verrons comment créer des rapports encore plus complexes avec des fonctionnalités avancées comme les tableaux de bord et les visualisations de données interactives.

⏭️ [Création de rapports complexes](/09-rapports-et-impressions/04-creation-de-rapports-complexes.md)
