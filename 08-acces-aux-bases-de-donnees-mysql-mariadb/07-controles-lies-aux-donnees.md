🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.7 Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)

## Introduction

Les **contrôles liés aux données** (Data-Aware Controls) sont des composants visuels spécialement conçus pour afficher et modifier automatiquement les données d'un DataSet. Ils rendent la création d'interfaces de bases de données incroyablement simple et rapide.

Dans ce chapitre, nous allons explorer en détail tous les contrôles DB disponibles dans Delphi et apprendre à les utiliser efficacement.

## Qu'est-ce qu'un contrôle lié aux données ?

### Définition

Un **contrôle lié aux données** (ou Data-Aware Control) est un composant visuel qui :
- Se connecte automatiquement à un DataSource
- Affiche les données de l'enregistrement courant
- Permet la modification directe des données
- Se met à jour automatiquement lors de la navigation

### Contrôle standard vs contrôle DB

| Contrôle standard | Contrôle DB | Différence |
|-------------------|-------------|------------|
| `TEdit` | `TDBEdit` | Le DBEdit se lie à un champ de la base |
| `TMemo` | `TDBMemo` | Le DBMemo affiche automatiquement le contenu |
| `TCheckBox` | `TDBCheckBox` | Le DBCheckBox reflète la valeur booléenne |
| `TImage` | `TDBImage` | Le DBImage affiche une image stockée |

### Propriétés communes

Tous les contrôles DB partagent ces deux propriétés essentielles :

| Propriété | Type | Description |
|-----------|------|-------------|
| `DataSource` | TDataSource | Le DataSource auquel se connecter |
| `DataField` | String | Le nom du champ à afficher/modifier |

## Palette de composants Data Controls

Dans l'IDE Delphi, les contrôles DB se trouvent dans l'onglet **Data Controls** :

```
┌─────────────────────────────────────┐
│  Data Controls                      │
├─────────────────────────────────────┤
│  DBGrid  DBNavigator  DBText        │
│  DBEdit  DBMemo  DBImage  DBCombo   │
│  DBCheckBox  DBRadioGroup           │
│  DBLookupComboBox  DBLookupListBox  │
│  DBRichEdit  DBCtrlGrid             │
└─────────────────────────────────────┘
```

## TDBGrid : La grille de données

**TDBGrid** est probablement le contrôle DB le plus utilisé. Il affiche les données sous forme de tableau avec des lignes et des colonnes.

### Configuration de base

```pascal
// Au design time (dans l'inspecteur d'objets)
DBGrid1.DataSource := DataSource1;

// Ou par code
DBGrid1.DataSource := DataSource1;
```

C'est tout ! Le DBGrid affiche automatiquement toutes les colonnes du DataSet.

### Propriétés importantes

#### Options d'affichage

```pascal
// Afficher les titres de colonnes
DBGrid1.Options := DBGrid1.Options + [dgTitles];

// Permettre la sélection de lignes entières
DBGrid1.Options := DBGrid1.Options + [dgRowSelect];

// Afficher les indicateurs (flèche sur la ligne courante)
DBGrid1.Options := DBGrid1.Options + [dgIndicator];

// Permettre l'édition directe
DBGrid1.Options := DBGrid1.Options + [dgEditing];

// Afficher les lignes alternées colorées
DBGrid1.Options := DBGrid1.Options + [dgRowLines];
```

#### Configuration complète des options

```pascal
// Configurer toutes les options ensemble
DBGrid1.Options := [
  dgTitles,          // Afficher les en-têtes
  dgIndicator,       // Afficher l'indicateur de ligne
  dgColumnResize,    // Permettre le redimensionnement des colonnes
  dgColLines,        // Lignes verticales entre colonnes
  dgRowLines,        // Lignes horizontales entre lignes
  dgTabs,            // Navigation avec Tab
  dgRowSelect,       // Sélection de lignes entières
  dgAlwaysShowSelection, // Garder la sélection visible
  dgConfirmDelete,   // Confirmer avant suppression
  dgCancelOnExit     // Annuler les modifications si on quitte
];
```

### Personnalisation des colonnes

#### Masquer/afficher des colonnes

```pascal
// Masquer la colonne "id"
DBGrid1.Columns[0].Visible := False;

// Ou par le nom du champ
var
  i: Integer;
begin
  for i := 0 to DBGrid1.Columns.Count - 1 do
  begin
    if DBGrid1.Columns[i].FieldName = 'id' then
      DBGrid1.Columns[i].Visible := False;
  end;
end;
```

#### Définir la largeur des colonnes

```pascal
// Définir la largeur en pixels
DBGrid1.Columns[0].Width := 50;   // ID : 50 pixels
DBGrid1.Columns[1].Width := 150;  // Nom : 150 pixels
DBGrid1.Columns[2].Width := 150;  // Prénom : 150 pixels
DBGrid1.Columns[3].Width := 200;  // Email : 200 pixels

// Largeur automatique
DBGrid1.Columns[0].Width := -1;  // Auto-ajustement
```

#### Changer les titres

```pascal
// Changer le titre d'une colonne
DBGrid1.Columns[0].Title.Caption := 'Identifiant';
DBGrid1.Columns[1].Title.Caption := 'Nom complet';
DBGrid1.Columns[2].Title.Caption := 'Adresse e-mail';

// Alignement du titre
DBGrid1.Columns[0].Title.Alignment := taCenter;

// Police du titre
DBGrid1.Columns[0].Title.Font.Style := [fsBold];
DBGrid1.Columns[0].Title.Font.Color := clNavy;
```

#### Alignement et format des cellules

```pascal
// Aligner le contenu
DBGrid1.Columns[0].Alignment := taCenter;      // ID centré
DBGrid1.Columns[3].Alignment := taRightJustify; // Prix aligné à droite

// Format d'affichage pour les nombres
DBGrid1.Columns[3].DisplayFormat := '#,##0.00 €';  // Pour les prix

// Couleur de fond
DBGrid1.Columns[0].Color := clInfoBk;
```

### Éditeur de colonnes (au design time)

**Méthode recommandée** : Utiliser l'éditeur de colonnes

1. Clic droit sur le DBGrid → **Columns Editor**
2. Cliquez sur **Add All Fields** pour ajouter toutes les colonnes
3. Sélectionnez chaque colonne et modifiez ses propriétés :
   - `FieldName` : Champ à afficher
   - `Title.Caption` : En-tête de colonne
   - `Width` : Largeur
   - `Visible` : Afficher ou masquer
   - `ReadOnly` : Lecture seule
   - `Color` : Couleur de fond

### Coloration conditionnelle

```pascal
// Événement OnDrawColumnCell
procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  // Colorer les lignes selon une condition
  if FDQuery1.FieldByName('actif').AsBoolean = False then
  begin
    // Client inactif : fond gris clair
    DBGrid1.Canvas.Brush.Color := clSilver;
    DBGrid1.Canvas.FillRect(Rect);
  end;

  // Colorer une colonne spécifique
  if Column.FieldName = 'solde' then
  begin
    if FDQuery1.FieldByName('solde').AsCurrency < 0 then
      DBGrid1.Canvas.Font.Color := clRed  // Solde négatif en rouge
    else
      DBGrid1.Canvas.Font.Color := clGreen; // Solde positif en vert
  end;

  // Dessiner le texte par défaut
  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;
```

### Événements utiles du DBGrid

```pascal
// Double-clic sur une cellule
procedure TForm1.DBGrid1DblClick(Sender: TObject);
begin
  // Ouvrir un formulaire de détail, par exemple
  ShowMessage('ID : ' + FDQuery1.FieldByName('id').AsString);
end;

// Clic sur un titre de colonne (pour trier)
procedure TForm1.DBGrid1TitleClick(Column: TColumn);
begin
  // Trier par cette colonne
  FDQuery1.Close;
  FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY ' + Column.FieldName;
  FDQuery1.Open;
end;

// Changement de cellule
procedure TForm1.DBGrid1CellClick(Column: TColumn);
begin
  // Réagir au clic sur une cellule
  StatusBar1.SimpleText := 'Colonne : ' + Column.Title.Caption;
end;
```

### Exporter le contenu du DBGrid

```pascal
procedure ExporterVersCsv(Grid: TDBGrid; const NomFichier: string);
var
  F: TextFile;
  i: Integer;
  Ligne: string;
begin
  AssignFile(F, NomFichier);
  Rewrite(F);
  try
    // En-têtes
    Ligne := '';
    for i := 0 to Grid.Columns.Count - 1 do
    begin
      if Grid.Columns[i].Visible then
      begin
        if Ligne <> '' then Ligne := Ligne + ';';
        Ligne := Ligne + Grid.Columns[i].Title.Caption;
      end;
    end;
    WriteLn(F, Ligne);

    // Données
    Grid.DataSource.DataSet.DisableControls;
    try
      Grid.DataSource.DataSet.First;
      while not Grid.DataSource.DataSet.Eof do
      begin
        Ligne := '';
        for i := 0 to Grid.Columns.Count - 1 do
        begin
          if Grid.Columns[i].Visible then
          begin
            if Ligne <> '' then Ligne := Ligne + ';';
            Ligne := Ligne + Grid.Columns[i].Field.AsString;
          end;
        end;
        WriteLn(F, Ligne);
        Grid.DataSource.DataSet.Next;
      end;
    finally
      Grid.DataSource.DataSet.EnableControls;
    end;

    ShowMessage('Export réussi');
  finally
    CloseFile(F);
  end;
end;
```

## Contrôles de saisie

### TDBEdit : Champ de saisie

**Usage :** Éditer une valeur texte ou numérique sur une ligne.

```pascal
// Configuration
DBEdit1.DataSource := DataSource1;
DBEdit1.DataField := 'nom';

// Propriétés utiles
DBEdit1.ReadOnly := False;     // Autoriser la modification
DBEdit1.MaxLength := 50;       // Limiter à 50 caractères
DBEdit1.CharCase := ecUpperCase; // Tout en majuscules
DBEdit1.Color := clWindow;     // Couleur de fond
```

**Validation :**

```pascal
// Événement OnExit : validation à la sortie du champ
procedure TForm1.DBEdit1Exit(Sender: TObject);
begin
  if Trim(DBEdit1.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    DBEdit1.SetFocus;  // Remettre le focus
  end;
end;
```

### TDBMemo : Zone de texte multiligne

**Usage :** Éditer un texte long sur plusieurs lignes (commentaires, description).

```pascal
// Configuration
DBMemo1.DataSource := DataSource1;
DBMemo1.DataField := 'commentaires';

// Propriétés
DBMemo1.ScrollBars := ssVertical;  // Barre de défilement
DBMemo1.WordWrap := True;          // Retour à la ligne automatique
DBMemo1.MaxLength := 1000;         // Limite de caractères
```

### TDBRichEdit : Texte enrichi

**Usage :** Texte avec mise en forme (gras, italique, couleurs).

```pascal
DBRichEdit1.DataSource := DataSource1;
DBRichEdit1.DataField := 'description_riche';
```

## Contrôles de sélection

### TDBCheckBox : Case à cocher

**Usage :** Valeurs booléennes (Oui/Non, Vrai/Faux, Actif/Inactif).

```pascal
// Configuration
DBCheckBox1.DataSource := DataSource1;
DBCheckBox1.DataField := 'actif';

// Personnalisation
DBCheckBox1.Caption := 'Client actif';
DBCheckBox1.ValueChecked := '1';     // Valeur si coché
DBCheckBox1.ValueUnchecked := '0';   // Valeur si décoché
```

### TDBComboBox : Liste déroulante

**Usage :** Sélectionner une valeur parmi une liste prédéfinie.

```pascal
// Configuration
DBComboBox1.DataSource := DataSource1;
DBComboBox1.DataField := 'civilite';

// Ajouter les valeurs possibles
DBComboBox1.Items.Clear;
DBComboBox1.Items.Add('M.');
DBComboBox1.Items.Add('Mme');
DBComboBox1.Items.Add('Mlle');

// Style
DBComboBox1.Style := csDropDownList;  // Pas d'édition libre
```

### TDBRadioGroup : Groupe de boutons radio

**Usage :** Choix exclusif parmi plusieurs options.

```pascal
// Configuration
DBRadioGroup1.DataSource := DataSource1;
DBRadioGroup1.DataField := 'statut';

// Définir les valeurs
DBRadioGroup1.Items.Clear;
DBRadioGroup1.Items.Add('Actif');
DBRadioGroup1.Items.Add('Inactif');
DBRadioGroup1.Items.Add('Suspendu');

// Valeurs correspondantes dans la base
DBRadioGroup1.Values.Clear;
DBRadioGroup1.Values.Add('A');
DBRadioGroup1.Values.Add('I');
DBRadioGroup1.Values.Add('S');

// Affichage
DBRadioGroup1.Caption := 'Statut du client';
DBRadioGroup1.Columns := 3;  // 3 colonnes
```

## Contrôles de Lookup (listes de choix)

Les contrôles **Lookup** permettent de sélectionner une valeur à partir d'une **autre table**.

### Concept du Lookup

**Scénario typique :**
- Table `commandes` avec un champ `client_id`
- Table `clients` avec `id` et `nom`
- Vous voulez afficher le **nom du client** au lieu de l'ID numérique

### TDBLookupComboBox : Liste déroulante de lookup

```pascal
// DataSource principal (commandes)
DataSourceCommandes.DataSet := FDQueryCommandes;

// DataSource de lookup (clients)
DataSourceClients.DataSet := FDQueryClients;

// Configuration du DBLookupComboBox
DBLookupComboBox1.DataSource := DataSourceCommandes;  // Source principale
DBLookupComboBox1.DataField := 'client_id';           // Champ à stocker

DBLookupComboBox1.ListSource := DataSourceClients;    // Source de la liste
DBLookupComboBox1.KeyField := 'id';                   // Champ clé dans clients
DBLookupComboBox1.ListField := 'nom';                 // Champ à afficher

// Charger les clients
FDQueryClients.SQL.Text := 'SELECT id, nom FROM clients ORDER BY nom';
FDQueryClients.Open;
```

**Fonctionnement :**
1. L'utilisateur sélectionne "Dupont Jean" dans la liste
2. Le DBLookupComboBox stocke l'`id` (par exemple 5) dans `commandes.client_id`
3. Il affiche "Dupont Jean" à l'écran

### Afficher plusieurs champs dans le Lookup

```pascal
// Afficher nom et prénom
DBLookupComboBox1.ListField := 'nom;prenom';

// Ou concaténer dans la requête
FDQueryClients.SQL.Text :=
  'SELECT id, CONCAT(nom, '' '', prenom) AS nom_complet FROM clients';
DBLookupComboBox1.ListField := 'nom_complet';
```

### TDBLookupListBox : Liste de choix

Similaire à DBLookupComboBox mais affiche une liste permanente au lieu d'une liste déroulante.

```pascal
DBLookupListBox1.DataSource := DataSourceCommandes;
DBLookupListBox1.DataField := 'client_id';
DBLookupListBox1.ListSource := DataSourceClients;
DBLookupListBox1.KeyField := 'id';
DBLookupListBox1.ListField := 'nom';
DBLookupListBox1.Height := 150;  // Afficher plusieurs lignes
```

## Contrôles d'affichage (lecture seule)

### TDBText : Texte en lecture seule

**Usage :** Afficher une valeur sans possibilité de modification.

```pascal
DBText1.DataSource := DataSource1;
DBText1.DataField := 'total';

// Mise en forme
DBText1.Font.Size := 14;
DBText1.Font.Style := [fsBold];
DBText1.Font.Color := clRed;
```

**Avantage sur TDBEdit :** Plus léger, optimisé pour l'affichage uniquement.

### TDBImage : Affichage d'images

**Usage :** Afficher des images stockées dans la base (champs BLOB).

```pascal
DBImage1.DataSource := DataSource1;
DBImage1.DataField := 'photo';

// Options d'affichage
DBImage1.Stretch := True;       // Étirer l'image
DBImage1.Proportional := True;  // Garder les proportions
DBImage1.Center := True;        // Centrer l'image
```

**Charger une image :**

```pascal
procedure ChargerImage(const CheminFichier: string);
begin
  if FDQuery1.State <> dsEdit then
    FDQuery1.Edit;

  TBlobField(FDQuery1.FieldByName('photo')).LoadFromFile(CheminFichier);
  FDQuery1.Post;
end;
```

## TDBNavigator : Barre de navigation

Le **DBNavigator** fournit des boutons pour naviguer et manipuler les données.

### Configuration complète

```pascal
DBNavigator1.DataSource := DataSource1;

// Personnaliser les boutons visibles
DBNavigator1.VisibleButtons := [
  nbFirst,    // Premier
  nbPrior,    // Précédent
  nbNext,     // Suivant
  nbLast,     // Dernier
  nbInsert,   // Nouveau
  nbDelete,   // Supprimer
  nbEdit,     // Éditer
  nbPost,     // Valider
  nbCancel,   // Annuler
  nbRefresh   // Rafraîchir
];

// Confirmer avant suppression
DBNavigator1.ConfirmDelete := True;

// Afficher les hints (bulles d'aide)
DBNavigator1.ShowHint := True;
DBNavigator1.Hints.Clear;
DBNavigator1.Hints.Add('Premier enregistrement');
DBNavigator1.Hints.Add('Enregistrement précédent');
DBNavigator1.Hints.Add('Enregistrement suivant');
DBNavigator1.Hints.Add('Dernier enregistrement');
DBNavigator1.Hints.Add('Insérer un enregistrement');
DBNavigator1.Hints.Add('Supprimer cet enregistrement');
DBNavigator1.Hints.Add('Modifier cet enregistrement');
DBNavigator1.Hints.Add('Valider les modifications');
DBNavigator1.Hints.Add('Annuler les modifications');
DBNavigator1.Hints.Add('Rafraîchir les données');
```

### Événements du DBNavigator

```pascal
// Avant de cliquer sur un bouton
procedure TForm1.DBNavigator1BeforeAction(Sender: TObject;
  Button: TNavigateBtn);
begin
  case Button of
    nbDelete:
      begin
        // Vérification personnalisée avant suppression
        if MessageDlg('Vraiment supprimer ?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
          Abort;  // Annule l'action
      end;
    nbPost:
      begin
        // Validation avant enregistrement
        if Trim(FDQuery1.FieldByName('nom').AsString) = '' then
        begin
          ShowMessage('Le nom est obligatoire');
          Abort;
        end;
      end;
  end;
end;
```

## Contrôles spécialisés

### TDBCtrlGrid : Grille de contrôles

Affiche plusieurs enregistrements avec des contrôles personnalisés pour chacun (comme une "grille de fiches").

```pascal
// Configuration
DBCtrlGrid1.DataSource := DataSource1;
DBCtrlGrid1.RowCount := 5;  // 5 enregistrements visibles

// Placer des contrôles à l'intérieur
// (DBEdit, DBImage, etc. positionnés sur le DBCtrlGrid)
```

**Usage :** Créer des listes personnalisées de type "catalogue" ou "galerie".

### TDBListBox : Liste avec données

```pascal
DBListBox1.DataSource := DataSource1;
DBListBox1.DataField := 'categorie';
```

## Mise en page et organisation

### Exemple d'interface complète

```
┌────────────────────────────────────────────┐
│  Fiche Client                          [X] │
├────────────────────────────────────────────┤
│  [|◄] [◄] [►] [►|] [+] [🗑] [✓] [X]  [↻]   │ ← DBNavigator
├────────────────────────────────────────────┤
│  ┌──────────────────────────────────────┐  │
│  │ Informations générales               │  │
│  ├──────────────────────────────────────┤  │
│  │ Civilité : [M.    ▼]  ← DBComboBox   │  │
│  │ Nom :      [_____________] ← DBEdit  │  │
│  │ Prénom :   [_____________] ← DBEdit  │  │
│  │ Email :    [_____________] ← DBEdit  │  │
│  │ Tél :      [_____________] ← DBEdit  │  │
│  │ ☑ Actif    ← DBCheckBox             │  │
│  └──────────────────────────────────────┘  │
│  ┌──────────────────────────────────────┐  │
│  │ Commentaires      ← DBMemo           │  │
│  │                                      │  │
│  │                                      │  │
│  └──────────────────────────────────────┘  │
└────────────────────────────────────────────┘
```

### Utiliser des panels pour organiser

```pascal
// Panel pour les informations de base
PanelInfos.Align := alTop;
PanelInfos.Height := 200;
PanelInfos.Caption := 'Informations générales';

// Panel pour les commentaires
PanelCommentaires.Align := alClient;
```

## Validation des données

### Validation au niveau des contrôles

```pascal
// Dans l'événement OnExit du DBEdit
procedure TForm1.DBEditEmailExit(Sender: TObject);
begin
  // Valider l'email
  if (Trim(DBEditEmail.Text) <> '') and
     (Pos('@', DBEditEmail.Text) = 0) then
  begin
    ShowMessage('Adresse email invalide');
    DBEditEmail.SetFocus;
  end;
end;
```

### Validation centralisée

```pascal
// Dans l'événement BeforePost du DataSet
procedure TForm1.FDQuery1BeforePost(DataSet: TDataSet);
begin
  // Nom obligatoire
  if Trim(DataSet.FieldByName('nom').AsString) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    DBEditNom.SetFocus;
    Abort;  // Empêche le Post
  end;

  // Email valide
  if Pos('@', DataSet.FieldByName('email').AsString) = 0 then
  begin
    ShowMessage('Email invalide');
    DBEditEmail.SetFocus;
    Abort;
  end;

  // Téléphone avec format
  if not ValidatePhone(DataSet.FieldByName('telephone').AsString) then
  begin
    ShowMessage('Numéro de téléphone invalide');
    DBEditTel.SetFocus;
    Abort;
  end;
end;
```

## Formatage des données

### Masques de saisie

```pascal
// Téléphone français
DBEditTel.EditMask := '00 00 00 00 00;1;_';

// Code postal
DBEditCP.EditMask := '00000;1;_';

// Date
DBEditDate.EditMask := '00/00/0000;1;_';

// NIR (Sécurité sociale)
DBEditNIR.EditMask := '0 00 00 00 000 000 00;1;_';
```

**Format du masque :**
- `0` : Chiffre obligatoire
- `9` : Chiffre optionnel
- `L` : Lettre obligatoire
- `l` : Lettre optionnelle
- `A` : Lettre ou chiffre obligatoire
- `a` : Lettre ou chiffre optionnel
- `;1` : Enregistrer avec le masque
- `;_` : Caractère d'espace réservé

### DisplayFormat pour l'affichage

```pascal
// Dans l'éditeur de champs du DataSet
// Pour un champ monétaire
TFloatField(FDQuery1prix).DisplayFormat := '#,##0.00 €';

// Pour un champ pourcentage
TFloatField(FDQuery1taux).DisplayFormat := '0.00 %';

// Pour les grands nombres
TIntegerField(FDQuery1population).DisplayFormat := '#,###';
```

## Gestion des couleurs

### Coloration selon l'état

```pascal
procedure TForm1.DataSource1StateChange(Sender: TObject);
begin
  case TDataSource(Sender).State of
    dsEdit, dsInsert:
      begin
        // Mode édition : fond jaune clair
        DBEditNom.Color := $00FFFFCC;
        DBEditPrenom.Color := $00FFFFCC;
        DBEditEmail.Color := $00FFFFCC;
      end;
    dsBrowse:
      begin
        // Mode navigation : fond blanc
        DBEditNom.Color := clWindow;
        DBEditPrenom.Color := clWindow;
        DBEditEmail.Color := clWindow;
      end;
  end;
end;
```

### Coloration selon les valeurs

```pascal
procedure TForm1.FDQuery1AfterScroll(DataSet: TDataSet);
begin
  // Client inactif : grisé
  if not DataSet.FieldByName('actif').AsBoolean then
  begin
    DBEditNom.Color := clSilver;
    DBEditNom.Font.Color := clGray;
  end
  else
  begin
    DBEditNom.Color := clWindow;
    DBEditNom.Font.Color := clBlack;
  end;
end;
```

## Contrôles en lecture seule

### Rendre un contrôle ReadOnly

```pascal
// Désactiver l'édition
DBEditID.ReadOnly := True;
DBEditDateCreation.ReadOnly := True;

// Visuellement différent
DBEditID.Color := clBtnFace;
DBEditID.Font.Color := clGrayText;
```

### Désactiver selon les droits

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Si l'utilisateur n'est pas administrateur
  if not EstAdministrateur then
  begin
    DBEditSalaire.ReadOnly := True;
    DBEditSalaire.Color := clBtnFace;
    DBNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast];
  end;
end;
```

## Raccourcis clavier et navigation

### Navigation au clavier dans les contrôles

```pascal
// Ordre de tabulation
DBEditNom.TabOrder := 0;
DBEditPrenom.TabOrder := 1;
DBEditEmail.TabOrder := 2;
DBEditTel.TabOrder := 3;

// Passer directement au suivant avec Enter
procedure TForm1.DBEditNomKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then  // Touche Enter
  begin
    Key := #0;  // Annuler le beep
    SelectNext(Sender as TWinControl, True, True);  // Aller au suivant
  end;
end;
```

### Raccourcis pour le DBNavigator

```pascal
// Gérer les touches F2, F3, etc.
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F2: FDQuery1.Insert;     // F2 : Nouveau
    VK_F3: FDQuery1.Edit;       // F3 : Modifier
    VK_F4: FDQuery1.Delete;     // F4 : Supprimer
    VK_F5: FDQuery1.Refresh;    // F5 : Rafraîchir
  end;
end;
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Utiliser la validation dans BeforePost**
   ```pascal
   procedure FDQuery1BeforePost(DataSet: TDataSet);
   begin
     // Validation centralisée
   end;
   ```

2. **Fournir un retour visuel**
   ```pascal
   // Changer la couleur en mode édition
   if DataSource1.State in [dsEdit, dsInsert] then
     Panel1.Color := clYellow;
   ```

3. **Désactiver les contrôles pendant le chargement**
   ```pascal
   DBEdit1.Enabled := False;
   try
     FDQuery1.Open;
   finally
     DBEdit1.Enabled := True;
   end;
   ```

4. **Utiliser DBLookupComboBox pour les clés étrangères**
   ```pascal
   // Plutôt que de saisir un ID manuellement
   DBLookupComboBox1.ListField := 'nom_client';
   ```

5. **Gérer les valeurs NULL**
   ```pascal
   if FDQuery1.FieldByName('telephone').IsNull then
     DBTextTel.Caption := '(non renseigné)';
   ```

### ❌ À ÉVITER

1. **Modifier directement sans vérifier l'état**
   ```pascal
   // ❌ ERREUR
   DBEdit1.Text := 'Nouvelle valeur';  // Ne modifie pas la base !

   // ✅ CORRECT
   FDQuery1.Edit;
   FDQuery1.FieldByName('nom').AsString := 'Nouvelle valeur';
   FDQuery1.Post;
   ```

2. **Oublier de lier DataSource ET DataField**
   ```pascal
   // ❌ Incomplet
   DBEdit1.DataSource := DataSource1;
   // Manque DataField !
   ```

3. **Trop de contrôles sur un formulaire**
   ```
   ❌ Un formulaire avec 50 DBEdit
   ✅ Organiser en onglets ou sous-formulaires
   ```

4. **Ne pas gérer les erreurs de contraintes**
   ```pascal
   // ✅ Gérer les doublons
   try
     FDQuery1.Post;
   except
     on E: Exception do
       if Pos('Duplicate', E.Message) > 0 then
         ShowMessage('Cette valeur existe déjà');
   end;
   ```

## Tableau récapitulatif des contrôles DB

| Contrôle | Usage | Propriétés clés |
|----------|-------|-----------------|
| `TDBGrid` | Grille de données | `DataSource`, `Options`, `Columns` |
| `TDBEdit` | Saisie texte | `DataSource`, `DataField`, `MaxLength` |
| `TDBMemo` | Texte multiligne | `DataSource`, `DataField`, `ScrollBars` |
| `TDBText` | Affichage texte | `DataSource`, `DataField` |
| `TDBCheckBox` | Case à cocher | `DataSource`, `DataField`, `Caption` |
| `TDBComboBox` | Liste déroulante | `DataSource`, `DataField`, `Items` |
| `TDBLookupComboBox` | Lookup | `DataSource`, `DataField`, `ListSource`, `KeyField`, `ListField` |
| `TDBImage` | Image | `DataSource`, `DataField`, `Stretch` |
| `TDBNavigator` | Navigation | `DataSource`, `VisibleButtons` |
| `TDBRadioGroup` | Choix exclusif | `DataSource`, `DataField`, `Items`, `Values` |

## Résumé

### Points clés

✅ Les contrôles DB se lient automatiquement aux données via **DataSource** et **DataField**
✅ **TDBGrid** est le composant le plus polyvalent pour afficher des données
✅ **TDBLookupComboBox** est essentiel pour les relations entre tables
✅ La **validation** se fait idéalement dans l'événement **BeforePost**
✅ Les contrôles DB se mettent à jour **automatiquement** lors de la navigation
✅ Utilisez **DBNavigator** pour fournir une interface de navigation standard

### Workflow typique

```
1. Placer un TDataSource sur le formulaire
2. Le lier à un TFDQuery
3. Placer des contrôles DB (DBEdit, DBGrid, etc.)
4. Configurer DataSource et DataField pour chaque contrôle
5. Ouvrir le Query → Les données s'affichent automatiquement !
```

## Prochaines étapes

Vous maîtrisez maintenant tous les contrôles liés aux données ! Dans les sections suivantes, nous verrons :

1. **Live Bindings** : Une approche moderne et flexible pour lier les données
2. **Modèle en couches** : Séparer la logique d'accès aux données de l'interface
3. **Migration et synchronisation** : Gérer l'évolution de vos bases de données

Avec ces connaissances, vous pouvez créer des interfaces riches et intuitives pour manipuler vos données MySQL/MariaDB !

⏭️ [Live Bindings et liaison de données visuelle](/08-acces-aux-bases-de-donnees-mysql-mariadb/08-live-bindings-et-liaison-de-donnees-visuelle.md)
