# 8.7 Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)

Dans les sections précédentes, nous avons appris à connecter notre application à une base de données MySQL et à manipuler les données avec les DataSets et DataSources. Maintenant, il est temps d'explorer comment afficher et modifier ces données à travers l'interface utilisateur grâce aux contrôles liés aux données.

## Qu'est-ce qu'un contrôle lié aux données ?

Un **contrôle lié aux données** (ou contrôle DB-aware) est un composant visuel spécialement conçu pour afficher et/ou modifier les données provenant d'un DataSet. Ces contrôles se connectent à un DataSource et permettent une interaction automatique avec les données sans avoir à écrire beaucoup de code.

![Architecture des contrôles DB](https://placeholder.pics/svg/600x250/DEDEDE/555555/Architecture%20Contrôles%20DB)

## Trouver les contrôles liés aux données dans l'IDE

Dans l'IDE de Delphi, les contrôles liés aux données sont regroupés principalement dans la palette de composants sous deux onglets :

- **Data Controls** : Contient les contrôles de base liés aux données
- **Data Access** : Contient les composants d'accès aux données (DataSources, etc.)

![Contrôles dans l'IDE](https://placeholder.pics/svg/500x200/DEDEDE/555555/Data%20Controls%20dans%20l'IDE)

## Les contrôles liés aux données les plus courants

### TDBGrid

Le `TDBGrid` est sans doute le contrôle le plus utilisé pour afficher des données. Il présente les données sous forme de tableau avec des lignes (enregistrements) et des colonnes (champs).

#### Propriétés importantes du TDBGrid

- **DataSource** : La source de données à afficher
- **Columns** : Collection des colonnes et leur configuration
- **Options** : Diverses options de comportement et d'apparence
- **ReadOnly** : Si True, les données ne peuvent pas être modifiées

#### Configuration de base d'un TDBGrid

```delphi
procedure TForm1.ConfigurerDBGrid;
begin
  // Connecter le DBGrid à la source de données
  DBGrid1.DataSource := DataSource1;

  // Optionnel : Configurer les options
  DBGrid1.Options := DBGrid1.Options + [dgRowSelect, dgAlwaysShowSelection]
                   - [dgEditing];  // Désactiver l'édition

  // Définir la hauteur des lignes
  DBGrid1.DefaultRowHeight := 22;

  // Ajuster automatiquement la largeur des colonnes
  DBGrid1.Columns.BeginUpdate;
  try
    // Configuration des colonnes (si nécessaire)
  finally
    DBGrid1.Columns.EndUpdate;
  end;
end;
```

#### Personnalisation des colonnes

Pour une meilleure présentation des données, vous pouvez personnaliser les colonnes :

1. Sélectionnez le `TDBGrid` dans le formulaire
2. Cliquez-droit et choisissez "Columns Editor"
3. Cliquez sur le bouton "+" pour ajouter les colonnes souhaitées
4. Configurez chaque colonne (titre, largeur, alignement, etc.)

Ou par code :

```delphi
procedure TForm1.PersonnaliserColonnesDBGrid;
var
  Colonne: TColumn;
begin
  // Supprimer les colonnes existantes
  DBGrid1.Columns.Clear;

  // Ajouter et configurer des colonnes personnalisées
  // Colonne ID
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'id';
  Colonne.Title.Caption := 'ID';
  Colonne.Width := 50;
  Colonne.Alignment := taCenter;

  // Colonne Nom
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'nom';
  Colonne.Title.Caption := 'Nom';
  Colonne.Width := 150;

  // Colonne Prénom
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'prenom';
  Colonne.Title.Caption := 'Prénom';
  Colonne.Width := 150;

  // Colonne Date de naissance avec format personnalisé
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'date_naissance';
  Colonne.Title.Caption := 'Date de naissance';
  Colonne.Width := 120;
  Colonne.ButtonStyle := cbsNone;  // Pas de bouton calendrier

  // Colonne Email
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'email';
  Colonne.Title.Caption := 'Adresse e-mail';
  Colonne.Width := 200;

  // Masquer certaines colonnes
  // Colonne.Visible := False;
end;
```

#### Événements utiles du TDBGrid

- **OnDrawColumnCell** : Permet de personnaliser l'affichage des cellules
- **OnDblClick** : Déclenché lors d'un double-clic sur une cellule
- **OnTitleClick** : Déclenché lors d'un clic sur un titre de colonne (utile pour le tri)

#### Exemple de personnalisation de l'affichage

```delphi
procedure TForm1.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  // Personnaliser l'affichage des cellules
  if not (gdSelected in State) then
  begin
    // Colorer en alternance les lignes
    if Odd(DBGrid1.DataSource.DataSet.RecNo) then
      DBGrid1.Canvas.Brush.Color := $F0F0F0
    else
      DBGrid1.Canvas.Brush.Color := clWhite;

    // Colorer différemment certaines valeurs
    if (Column.FieldName = 'statut') and
       (Column.Field.AsString = 'Inactif') then
      DBGrid1.Canvas.Font.Color := clRed;
  end;

  // Dessiner la cellule
  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;
```

#### Tri des données en cliquant sur les en-têtes

```delphi
procedure TForm1.DBGrid1TitleClick(Column: TColumn);
begin
  // Trier par la colonne cliquée
  if FDQuery1.IndexFieldNames = Column.FieldName then
    // Inverser l'ordre si on clique sur la même colonne
    FDQuery1.IndexFieldNames := Column.FieldName + ':D'
  else
    FDQuery1.IndexFieldNames := Column.FieldName;

  // Indication visuelle de la colonne de tri (optionnel)
  Column.Title.Font.Style := [fsBold];
end;
```

### TDBNavigator

Le `TDBNavigator` est un contrôle qui fournit des boutons pour naviguer et manipuler les enregistrements d'un DataSet. Il est très utile pour les interfaces de gestion de données.

#### Propriétés du TDBNavigator

- **DataSource** : La source de données à contrôler
- **VisibleButtons** : Détermine quels boutons sont visibles
- **Hints** : Infobulles pour chaque bouton
- **ConfirmDelete** : Demande une confirmation avant la suppression

#### Fonctions des boutons du TDBNavigator

| Bouton | Fonction |
|--------|----------|
| Premier | Se déplace au premier enregistrement |
| Précédent | Se déplace à l'enregistrement précédent |
| Suivant | Se déplace à l'enregistrement suivant |
| Dernier | Se déplace au dernier enregistrement |
| Insérer | Ajoute un nouvel enregistrement |
| Supprimer | Supprime l'enregistrement actuel |
| Éditer | Passe en mode édition |
| Valider | Enregistre les modifications |
| Annuler | Annule les modifications |
| Rafraîchir | Recharge les données du DataSet |

#### Configuration du TDBNavigator

```delphi
procedure TForm1.ConfigurerDBNavigator;
begin
  // Associer le navigateur à la source de données
  DBNavigator1.DataSource := DataSource1;

  // Personnaliser les boutons visibles (optionnel)
  DBNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast,
                                  nbInsert, nbDelete, nbEdit,
                                  nbPost, nbCancel, nbRefresh];

  // Définir des infobulles personnalisées
  DBNavigator1.Hints.Clear;
  DBNavigator1.Hints.Add('Premier');
  DBNavigator1.Hints.Add('Précédent');
  DBNavigator1.Hints.Add('Suivant');
  DBNavigator1.Hints.Add('Dernier');
  DBNavigator1.Hints.Add('Ajouter');
  DBNavigator1.Hints.Add('Supprimer');
  DBNavigator1.Hints.Add('Modifier');
  DBNavigator1.Hints.Add('Enregistrer');
  DBNavigator1.Hints.Add('Annuler');
  DBNavigator1.Hints.Add('Actualiser');

  // Activer les infobulles
  DBNavigator1.ShowHint := True;

  // Demander confirmation avant suppression
  DBNavigator1.ConfirmDelete := True;
end;
```

### Contrôles d'édition de texte

#### TDBEdit

Le `TDBEdit` est un champ de texte simple lié à un champ de la base de données.

```delphi
// Configuration de base
DBEdit1.DataSource := DataSource1;
DBEdit1.DataField := 'nom';  // Nom du champ à éditer
```

#### TDBMemo

Le `TDBMemo` est utilisé pour éditer des textes longs sur plusieurs lignes.

```delphi
// Configuration de base
DBMemo1.DataSource := DataSource1;
DBMemo1.DataField := 'description';
DBMemo1.ScrollBars := ssVertical;  // Ajouter une barre de défilement verticale
```

#### TDBRichEdit

Le `TDBRichEdit` permet d'éditer du texte avec mise en forme (gras, italique, etc.).

```delphi
// Configuration de base
DBRichEdit1.DataSource := DataSource1;
DBRichEdit1.DataField := 'contenu_formate';
```

### Contrôles de sélection

#### TDBComboBox

Le `TDBComboBox` permet de sélectionner une valeur parmi une liste prédéfinie.

```delphi
// Configuration de base
DBComboBox1.DataSource := DataSource1;
DBComboBox1.DataField := 'categorie';

// Ajouter des éléments à la liste
DBComboBox1.Items.Clear;
DBComboBox1.Items.Add('Catégorie A');
DBComboBox1.Items.Add('Catégorie B');
DBComboBox1.Items.Add('Catégorie C');
```

#### TDBLookupComboBox

Le `TDBLookupComboBox` est l'un des contrôles les plus puissants et utiles. Il permet de sélectionner une valeur à partir d'une autre table (par exemple, choisir un client dans une liste de clients).

##### Configuration du TDBLookupComboBox

Pour utiliser ce contrôle, nous avons besoin :
1. D'un DataSet principal (celui que nous éditons)
2. D'un DataSet de recherche (contenant les valeurs à afficher dans la liste)
3. D'un DataSource pour le DataSet de recherche

```delphi
procedure TForm1.ConfigurerDBLookupComboBox;
begin
  // Configurons un lookup pour sélectionner une ville

  // 1. Configurer le DataSet de recherche
  FDQueryVilles.Connection := FDConnection1;
  FDQueryVilles.SQL.Text := 'SELECT id, nom FROM villes ORDER BY nom';
  FDQueryVilles.Open;

  // 2. Configurer le DataSource de recherche
  DataSourceVilles.DataSet := FDQueryVilles;

  // 3. Configurer le DBLookupComboBox
  DBLookupComboBoxVille.DataSource := DataSourceClients;  // DataSource principal
  DBLookupComboBoxVille.DataField := 'ville_id';          // Champ à éditer
  DBLookupComboBoxVille.ListSource := DataSourceVilles;   // Source de la liste
  DBLookupComboBoxVille.KeyField := 'id';                 // Champ de clé dans la liste
  DBLookupComboBoxVille.ListField := 'nom';               // Champ à afficher dans la liste
end;
```

Avec cette configuration, lorsque l'utilisateur sélectionne une ville dans la liste, c'est l'ID de la ville qui sera enregistré dans le champ `ville_id` de la table clients, mais l'utilisateur verra le nom de la ville.

#### TDBRadioGroup

Le `TDBRadioGroup` permet de sélectionner une option parmi plusieurs via des boutons radio.

```delphi
// Configuration de base
DBRadioGroup1.DataSource := DataSource1;
DBRadioGroup1.DataField := 'statut';

// Définir les options
DBRadioGroup1.Items.Clear;
DBRadioGroup1.Items.Add('Actif');
DBRadioGroup1.Items.Add('En attente');
DBRadioGroup1.Items.Add('Inactif');

// Définir les valeurs correspondantes
DBRadioGroup1.Values.Clear;
DBRadioGroup1.Values.Add('A');
DBRadioGroup1.Values.Add('P');
DBRadioGroup1.Values.Add('I');
```

#### TDBCheckBox

Le `TDBCheckBox` est utilisé pour les champs booléens (vrai/faux).

```delphi
// Configuration de base
DBCheckBox1.DataSource := DataSource1;
DBCheckBox1.DataField := 'est_actif';
DBCheckBox1.Caption := 'Client actif';

// Personnalisation des valeurs (optionnel)
DBCheckBox1.ValueChecked := 'Oui';
DBCheckBox1.ValueUnchecked := 'Non';
```

### Contrôles pour les dates

#### TDBDateTimePicker

Le `TDBDateTimePicker` permet de sélectionner une date et/ou une heure facilement.

```delphi
// Configuration de base
DBDateTimePicker1.DataSource := DataSource1;
DBDateTimePicker1.DataField := 'date_naissance';

// Format de la date
DBDateTimePicker1.Format := 'dd/MM/yyyy';
```

### Contrôles d'affichage uniquement

#### TDBText

Le `TDBText` affiche le contenu d'un champ sans permettre l'édition.

```delphi
// Configuration de base
DBText1.DataSource := DataSource1;
DBText1.DataField := 'nom_complet';
```

#### TDBImage

Le `TDBImage` affiche une image stockée dans un champ BLOB.

```delphi
// Configuration de base
DBImage1.DataSource := DataSource1;
DBImage1.DataField := 'photo';
DBImage1.Stretch := True;  // Redimensionner l'image pour qu'elle s'adapte
DBImage1.Proportional := True;  // Conserver les proportions
```

## Gestion des événements des contrôles

Les contrôles liés aux données génèrent des événements qui peuvent être utilisés pour personnaliser leur comportement :

```delphi
procedure TForm1.DBEdit1Change(Sender: TObject);
begin
  // Code à exécuter lorsque le contenu du DBEdit change
  Label1.Caption := 'Modifié : ' + DBEdit1.Text;
end;

procedure TForm1.DBLookupComboBoxVilleCloseUp(Sender: TObject);
begin
  // Code à exécuter après la sélection d'une ville
  if not VarIsNull(DBLookupComboBoxVille.KeyValue) then
    Label2.Caption := 'Ville sélectionnée : ' + DBLookupComboBoxVille.Text;
end;
```

## Contrôler l'état d'activation des contrôles

Il est souvent nécessaire d'activer ou désactiver des contrôles en fonction de l'état du DataSet :

```delphi
procedure TForm1.DataSource1StateChange(Sender: TObject);
begin
  // Activer l'édition uniquement quand on est en mode insertion ou édition
  DBEdit1.Enabled := DataSource1.State in [dsEdit, dsInsert];
  DBEdit2.Enabled := DataSource1.State in [dsEdit, dsInsert];
  DBLookupComboBox1.Enabled := DataSource1.State in [dsEdit, dsInsert];

  // Activer les boutons en fonction de l'état
  ButtonEnregistrer.Enabled := DataSource1.State in [dsEdit, dsInsert];
  ButtonAnnuler.Enabled := DataSource1.State in [dsEdit, dsInsert];
  ButtonModifier.Enabled := (DataSource1.State = dsBrowse) and
                            not DataSource1.DataSet.IsEmpty;
end;
```

## Validation des données saisies

Vous pouvez valider les données avant qu'elles ne soient enregistrées dans le DataSet :

```delphi
procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Vérifier si le champ modifié est le champ 'email'
  if (Field <> nil) and (Field.FieldName = 'email') then
  begin
    // Vérifier que l'email contient un '@'
    if (Field.AsString <> '') and (Pos('@', Field.AsString) = 0) then
    begin
      ShowMessage('Adresse e-mail invalide !');
      DBEdit3.SetFocus;  // Revenir au champ email
    end;
  end;
end;

procedure TForm1.FDQuery1BeforePost(DataSet: TDataSet);
begin
  // Vérifier que les champs obligatoires sont remplis
  if DataSet.FieldByName('nom').AsString = '' then
  begin
    ShowMessage('Le nom est obligatoire !');
    DBEdit1.SetFocus;
    Abort;  // Annuler l'enregistrement
  end;

  if DataSet.FieldByName('email').AsString = '' then
  begin
    ShowMessage('L''email est obligatoire !');
    DBEdit3.SetFocus;
    Abort;
  end;
end;
```

## Exemple complet : Formulaire de gestion des clients

Voici un exemple qui illustre l'utilisation de plusieurs contrôles liés aux données dans une application réelle :

```delphi
unit UnitGestionClients;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.ExtCtrls, Vcl.DBCtrls, Vcl.Mask,
  Vcl.ComCtrls;

type
  TFormGestionClients = class(TForm)
    // Composants de base de données
    FDConnection1: TFDConnection;
    FDQueryClients: TFDQuery;
    DataSourceClients: TDataSource;
    FDQueryVilles: TFDQuery;
    DataSourceVilles: TDataSource;

    // Contrôles de navigation et d'affichage des données
    PanelHaut: TPanel;
    DBNavigator1: TDBNavigator;
    PageControl1: TPageControl;

    // Page "Liste des clients"
    TabSheetListe: TTabSheet;
    DBGrid1: TDBGrid;
    PanelRecherche: TPanel;
    LabelRechercher: TLabel;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;

    // Page "Détails du client"
    TabSheetDetails: TTabSheet;
    LabelNom: TLabel;
    DBEditNom: TDBEdit;
    LabelPrenom: TLabel;
    DBEditPrenom: TDBEdit;
    LabelEmail: TLabel;
    DBEditEmail: TDBEdit;
    LabelTelephone: TLabel;
    DBEditTelephone: TDBEdit;
    LabelAdresse: TLabel;
    DBMemoAdresse: TDBMemo;
    LabelVille: TLabel;
    DBLookupComboBoxVille: TDBLookupComboBox;
    LabelDateNaissance: TLabel;
    DBDateTimePickerNaissance: TDBDateTimePicker;
    DBCheckBoxActif: TDBCheckBox;
    GroupBoxGenre: TDBRadioGroup;
    LabelNotes: TLabel;
    DBRichEditNotes: TDBRichEdit;
    LabelDateCreation: TLabel;
    DBTextDateCreation: TDBText;

    // Boutons d'action
    PanelBoutons: TPanel;
    ButtonNouveau: TButton;
    ButtonModifier: TButton;
    ButtonSupprimer: TButton;
    ButtonEnregistrer: TButton;
    ButtonAnnuler: TButton;

    StatusBar1: TStatusBar;

    // Événements
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure ButtonNouveauClick(Sender: TObject);
    procedure ButtonModifierClick(Sender: TObject);
    procedure ButtonSupprimerClick(Sender: TObject);
    procedure ButtonEnregistrerClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure DataSourceClientsStateChange(Sender: TObject);
    procedure FDQueryClientsBeforePost(DataSet: TDataSet);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
      DataCol: Integer; Column: TColumn; State: TGridDrawState);

  private
    procedure ConnecterBaseDeDonnees;
    procedure ConfigurerControles;
    procedure ConfigurerDBGrid;
    procedure MettreAJourStatut;
  public
    { Déclarations publiques }
  end;

var
  FormGestionClients: TFormGestionClients;

implementation

{$R *.dfm}

procedure TFormGestionClients.FormCreate(Sender: TObject);
begin
  ConnecterBaseDeDonnees;
  ConfigurerControles;

  // Initialiser les contrôles
  PageControl1.ActivePage := TabSheetListe;
  EditRecherche.Clear;

  MettreAJourStatut;
end;

procedure TFormGestionClients.ConnecterBaseDeDonnees;
begin
  try
    FDConnection1.Connected := True;

    // Configurer et ouvrir les DataSets
    FDQueryClients.Connection := FDConnection1;
    FDQueryClients.SQL.Text := 'SELECT * FROM clients ORDER BY nom, prenom';
    FDQueryClients.Open;

    FDQueryVilles.Connection := FDConnection1;
    FDQueryVilles.SQL.Text := 'SELECT id, nom FROM villes ORDER BY nom';
    FDQueryVilles.Open;

  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;

procedure TFormGestionClients.ConfigurerControles;
begin
  // Configurer le navigateur
  DBNavigator1.DataSource := DataSourceClients;

  // Configurer le DBGrid
  ConfigurerDBGrid;

  // Configurer le DBLookupComboBox
  DBLookupComboBoxVille.ListSource := DataSourceVilles;
  DBLookupComboBoxVille.KeyField := 'id';
  DBLookupComboBoxVille.ListField := 'nom';

  // Configurer le DBRadioGroup
  GroupBoxGenre.Items.Clear;
  GroupBoxGenre.Items.Add('Homme');
  GroupBoxGenre.Items.Add('Femme');
  GroupBoxGenre.Items.Add('Autre');
  GroupBoxGenre.Values.Clear;
  GroupBoxGenre.Values.Add('H');
  GroupBoxGenre.Values.Add('F');
  GroupBoxGenre.Values.Add('A');

  // Configurer le DBDateTimePicker
  DBDateTimePickerNaissance.Format := 'dd/MM/yyyy';

  // Configurer les contrôles en fonction de l'état du DataSet
  DataSourceClientsStateChange(nil);
end;

procedure TFormGestionClients.ConfigurerDBGrid;
var
  Colonne: TColumn;
begin
  DBGrid1.DataSource := DataSourceClients;

  // Personnaliser les colonnes
  DBGrid1.Columns.Clear;

  // Colonne ID
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'id';
  Colonne.Title.Caption := 'ID';
  Colonne.Width := 50;
  Colonne.Alignment := taCenter;

  // Colonne Nom
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'nom';
  Colonne.Title.Caption := 'Nom';
  Colonne.Width := 150;

  // Colonne Prénom
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'prenom';
  Colonne.Title.Caption := 'Prénom';
  Colonne.Width := 150;

  // Colonne Email
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'email';
  Colonne.Title.Caption := 'Email';
  Colonne.Width := 200;

  // Colonne Téléphone
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'telephone';
  Colonne.Title.Caption := 'Téléphone';
  Colonne.Width := 120;

  // Colonne Actif
  Colonne := DBGrid1.Columns.Add;
  Colonne.FieldName := 'actif';
  Colonne.Title.Caption := 'Actif';
  Colonne.Width := 60;
  Colonne.Alignment := taCenter;

  // Options du DBGrid
  DBGrid1.Options := DBGrid1.Options + [dgRowSelect, dgAlwaysShowSelection]
                   - [dgEditing];
end;

procedure TFormGestionClients.FormDestroy(Sender: TObject);
begin
  // Fermer proprement les DataSets et la connexion
  if FDQueryVilles.Active then
    FDQueryVilles.Close;

  if FDQueryClients.Active then
    FDQueryClients.Close;

  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;

procedure TFormGestionClients.ButtonRechercherClick(Sender: TObject);
var
  TermeRecherche: string;
begin
  TermeRecherche := Trim(EditRecherche.Text);

  if TermeRecherche = '' then
  begin
    // Réinitialiser le filtre
    FDQueryClients.Filtered := False;
    FDQueryClients.Filter := '';
  end
  else
  begin
    // Appliquer un filtre
    FDQueryClients.Filtered := False;  // Désactiver d'abord
    FDQueryClients.Filter := Format('(nom LIKE ''%%%s%%'') OR (prenom LIKE ''%%%s%%'') OR (email LIKE ''%%%s%%'')',
                               [TermeRecherche, TermeRecherche, TermeRecherche]);
    FDQueryClients.Filtered := True;
  end;

  MettreAJourStatut;
end;

procedure TFormGestionClients.ButtonNouveauClick(Sender: TObject);
begin
  FDQueryClients.Append;
  PageControl1.ActivePage := TabSheetDetails;
  DBEditNom.SetFocus;
end;

procedure TFormGestionClients.ButtonModifierClick(Sender: TObject);
begin
  if not FDQueryClients.IsEmpty then
  begin
    FDQueryClients.Edit;
    PageControl1.ActivePage := TabSheetDetails;
    DBEditNom.SetFocus;
  end;
end;

procedure TFormGestionClients.ButtonSupprimerClick(Sender: TObject);
begin
  if not FDQueryClients.IsEmpty then
    if MessageDlg('Êtes-vous sûr de vouloir supprimer ce client ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FDQueryClients.Delete;
end;

procedure TFormGestionClients.ButtonEnregistrerClick(Sender: TObject);
begin
  if FDQueryClients.State in [dsEdit, dsInsert] then
  begin
    try
      FDQueryClients.Post;
      PageControl1.ActivePage := TabSheetListe;
    except
      on E: Exception do
        ShowMessage('Erreur lors de l''enregistrement : ' + E.Message);
    end;
  end;
end;

procedure TFormGestionClients.ButtonAnnulerClick(Sender: TObject);
begin
  if FDQueryClients.State in [dsEdit, dsInsert] then
  begin
    FDQueryClients.Cancel;
    PageControl1.ActivePage := TabSheetListe;
  end;
end;

procedure TFormGestionClients.DBGrid1DblClick(Sender: TObject);
begin
  if not FDQueryClients.IsEmpty then
  begin
    PageControl1.ActivePage := TabSheetDetails;
  end;
end;

procedure TFormGestionClients.DBGrid1TitleClick(Column: TColumn);
begin
  // Trier par la colonne cliquée
  if FDQueryClients.IndexFieldNames = Column.FieldName then
    // Inverser l'ordre si on clique sur la même colonne
    FDQueryClients.IndexFieldNames := Column.FieldName + ':D'
  else
    FDQueryClients.IndexFieldNames := Column.FieldName;
end;

procedure TFormGestionClients.DataSourceClientsStateChange(Sender: TObject);
begin
  // Adapter l'interface en fonction de l'état du DataSet
  case FDQueryClients.State of
    dsEdit, dsInsert:
      begin
        // En mode édition ou insertion
        ButtonNouveau.Enabled := False;
        ButtonModifier.Enabled := False;
        ButtonSupprimer.Enabled := False;
        ButtonEnregistrer.Enabled := True;
        ButtonAnnuler.Enabled := True;
        DBNavigator1.Enabled := False;
        DBGrid1.Enabled := False;
        EditRecherche.Enabled := False;
        ButtonRechercher.Enabled := False;

        StatusBar1.SimpleText := 'Édition en cours...';
      end;
    dsBrowse:
      begin
        // En mode navigation
        ButtonNouveau.Enabled := True;
        ButtonModifier.Enabled := not FDQueryClients.IsEmpty;
        ButtonSupprimer.Enabled := not FDQueryClients.IsEmpty;
        ButtonEnregistrer.Enabled := False;
        ButtonAnnuler.Enabled := False;
        DBNavigator1.Enabled := True;
        DBGrid1.Enabled := True;
        EditRecherche.Enabled := True;
        ButtonRechercher.Enabled := True;

        MettreAJourStatut;
      end;
  end;
end;

procedure TFormGestionClients.FDQueryClientsBeforePost(DataSet: TDataSet);
begin
  // Valider les données avant enregistrement
  if DataSet.FieldByName('nom').AsString = '' then
  begin
    ShowMessage('Le nom est obligatoire !');
    DBEditNom.SetFocus;
    Abort;
  end;

  if DataSet.FieldByName('email').AsString = '' then
  begin
    ShowMessage('L''email est obligatoire !');
    DBEditEmail.SetFocus;
    Abort;
  end;

  // Ajouter des champs de suivi
  if DataSet.State = dsInsert then
    DataSet.FieldByName('date_creation').AsDateTime := Now
  else if DataSet.State = dsEdit then
    DataSet.FieldByName('date_modification').AsDateTime := Now;
end;

procedure TFormGestionClients.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect;
  DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  // Personnaliser l'affichage des cellules
  if not (gdSelected in State) then
  begin
    // Colorer en alternance les lignes
    if Odd(FDQueryClients.RecNo) then
      DBGrid1.Canvas.Brush.Color := $F0F0F0
    else
      DBGrid1.Canvas.Brush.Color := clWhite;

    // Colorer différemment certaines valeurs
    if (Column.FieldName = 'actif') then
    begin
      if Column.Field.AsString = 'Oui' then
        DBGrid1.Canvas.Font.Color := clGreen
      else
        DBGrid1.Canvas.Font.Color := clRed;
    end;
  end;

  // Dessiner la cellule
  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TFormGestionClients.MettreAJourStatut;
begin
  if FDQueryClients.Active then
  begin
    if FDQueryClients.Filtered then
      StatusBar1.SimpleText := Format('%d clients trouvés (filtrés)', [FDQueryClients.RecordCount])
    else
      StatusBar1.SimpleText := Format('%d clients au total', [FDQueryClients.RecordCount]);
  end
  else
    StatusBar1.SimpleText := 'Base de données non connectée';
end;

end.
```

## Bonnes pratiques pour l'utilisation des contrôles liés aux données

Pour tirer le meilleur parti des contrôles liés aux données dans vos applications Delphi :

1. **Organisation de l'interface** : Regroupez logiquement les contrôles liés (par exemple, utilisez des `TGroupBox` ou des `TPanel`).

2. **Validation des données** :
   - Utilisez les événements `BeforePost` du DataSet pour valider les données
   - Ajoutez des validations au niveau des contrôles pour un retour immédiat

3. **Expérience utilisateur** :
   - Désactivez les contrôles qui ne doivent pas être modifiés dans certains états
   - Utilisez des raccourcis clavier (propriété `ShortCut`)
   - Ajoutez des infobulles explicatives (propriété `Hint` avec `ShowHint := True`)

4. **Performance** :
   - Ne chargez que les données nécessaires dans le DataSet
   - Utilisez des objets `TField` persistants pour un accès rapide aux champs
   - Pour les grandes listes, utilisez des techniques de chargement progressif

5. **Facilité de maintenance** :
   - Nommez clairement vos contrôles (par exemple, `DBEditNomClient`)
   - Commentez le code, particulièrement pour les validations complexes
   - Centralisez la logique de validation dans des méthodes dédiées

## Personnalisation avancée des contrôles

### Créer un contrôle personnalisé

Si les contrôles standard ne répondent pas à vos besoins, vous pouvez créer vos propres contrôles liés aux données :

```delphi
unit DBAdvancedEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.StdCtrls, Vcl.DBCtrls, Data.DB;

type
  TDBAdvancedEdit = class(TDBEdit)
  private
    FMandatory: Boolean;
    FMandatoryColor: TColor;
    procedure SetMandatory(const Value: Boolean);
    procedure UpdateBackground;
  protected
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Mandatory: Boolean read FMandatory write SetMandatory default False;
    property MandatoryColor: TColor read FMandatoryColor write FMandatoryColor default clYellow;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls', [TDBAdvancedEdit]);
end;

constructor TDBAdvancedEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMandatory := False;
  FMandatoryColor := clYellow;
end;

procedure TDBAdvancedEdit.SetMandatory(const Value: Boolean);
begin
  if FMandatory <> Value then
  begin
    FMandatory := Value;
    UpdateBackground;
  end;
end;

procedure TDBAdvancedEdit.Change;
begin
  inherited Change;
  UpdateBackground;
end;

procedure TDBAdvancedEdit.UpdateBackground;
begin
  if FMandatory and (Text = '') then
    Color := FMandatoryColor
  else
    Color := clWindow;
end;

end.
```

### Utiliser des styles pour personnaliser l'apparence

Delphi permet de personnaliser l'apparence des contrôles via les styles VCL :

```delphi
procedure TForm1.ApplyCustomStyle;
begin
  // Charger un style personnalisé
  TStyleManager.TrySetStyle('Aqua Light Slate');

  // Ou appliquer des modifications spécifiques
  with DBGrid1.TitleFont do
  begin
    Name := 'Segoe UI';
    Size := 10;
    Style := [fsBold];
    Color := clNavy;
  end;
end;
```

## Conclusion

Les contrôles liés aux données sont l'un des grands avantages de Delphi pour le développement d'applications de gestion. Ils vous permettent de créer rapidement des interfaces utilisateur qui interagissent avec vos bases de données sans avoir à écrire beaucoup de code.

Pour créer des applications professionnelles avec Delphi et MySQL/MariaDB :
- Utilisez les DataSets et DataSources pour accéder aux données
- Choisissez les contrôles liés aux données appropriés pour chaque type de champ
- Personnalisez l'apparence et le comportement des contrôles pour une meilleure expérience utilisateur
- Validez les données pour assurer l'intégrité de votre base de données
- Structurez votre interface de manière logique et intuitive

Dans la prochaine section, nous explorerons "Live Bindings et liaison de données visuelle", une autre approche puissante pour connecter vos données à votre interface utilisateur.

---

**À suivre :** 8.8 Live Bindings et liaison de données visuelle
