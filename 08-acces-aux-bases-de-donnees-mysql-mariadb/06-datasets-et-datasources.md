# 8.6 DataSets et DataSources

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans cette section, nous allons explorer deux concepts fondamentaux pour la gestion des données dans Delphi : les DataSets et les DataSources. Ces composants forment le pont entre votre base de données et l'interface utilisateur de votre application.

## Qu'est-ce qu'un DataSet ?

Un **DataSet** est un composant qui représente un ensemble de données sous forme de tableau. Il peut s'agir des résultats d'une requête SQL, d'une table entière, ou même d'une table en mémoire. Les DataSets vous permettent de :

- Parcourir les enregistrements
- Ajouter, modifier ou supprimer des données
- Filtrer ou trier les données
- Rechercher des informations spécifiques

Dans FireDAC, les composants comme `TFDQuery`, `TFDTable` et `TFDMemTable` sont tous des DataSets - ils héritent tous de la classe de base `TDataSet`.

![Structure hiérarchique des DataSets](https://placeholder.pics/svg/550x250/DEDEDE/555555/Hiérarchie%20DataSets)

## Qu'est-ce qu'un DataSource ?

Un **DataSource** (source de données) est un composant qui sert d'intermédiaire entre un DataSet et les contrôles visuels. Il transmet les données du DataSet aux contrôles d'affichage et renvoie les modifications effectuées par l'utilisateur au DataSet.

En résumé :
- Le **DataSet** contient et gère les données
- Le **DataSource** fait le lien entre le DataSet et l'interface utilisateur
- Les **contrôles liés aux données** (DB-aware controls) affichent et permettent la modification des données

![Architecture DataSet-DataSource-Contrôles](https://placeholder.pics/svg/650x200/DEDEDE/555555/Architecture%20DataSet-DataSource-Contrôles)

## Configuration de base

Voici comment configurer ces éléments ensemble :

### 1. Placer les composants

1. Ajoutez un composant `TFDConnection` configuré pour votre base de données MySQL
2. Ajoutez un composant `TFDQuery` ou `TFDTable` (ce sont des DataSets)
3. Ajoutez un composant `TDataSource`
4. Ajoutez des contrôles liés aux données (`TDBGrid`, `TDBEdit`, etc.)

![Composants dans l'IDE](https://placeholder.pics/svg/500x250/DEDEDE/555555/Composants%20dans%20l'IDE)

### 2. Configurer les liaisons

1. Reliez le `TFDQuery` à votre `TFDConnection` :
   ```
   FDQuery1.Connection := FDConnection1;
   ```

2. Définissez la requête SQL :
   ```
   FDQuery1.SQL.Text := 'SELECT * FROM clients';
   ```

3. Reliez le `TDataSource` au `TFDQuery` :
   ```
   DataSource1.DataSet := FDQuery1;
   ```

4. Reliez les contrôles au `TDataSource` :
   ```
   DBGrid1.DataSource := DataSource1;
   DBEdit1.DataSource := DataSource1;
   ```

5. Activez le DataSet :
   ```
   FDQuery1.Active := True;
   ```

## Propriétés essentielles des DataSets

Tous les DataSets partagent un ensemble de propriétés et méthodes communes. Voici les plus importantes :

### Propriétés d'état

- **Active** : Détermine si le DataSet contient des données (True) ou non (False).
- **State** : Indique l'état actuel du DataSet (`dsBrowse`, `dsEdit`, `dsInsert`, etc.).
- **RecordCount** : Nombre d'enregistrements dans le DataSet.
- **RecNo** : Position actuelle dans le DataSet.
- **EOF** et **BOF** : Indiquent si le curseur est à la fin ou au début du DataSet.
- **Fields** : Collection des champs disponibles dans le DataSet.

### Exemple pour vérifier l'état d'un DataSet

```delphi
procedure TForm1.AfficherEtatDataSet;
var
  EtatTexte: string;
begin
  // Vérifier si le DataSet est actif
  if not FDQuery1.Active then
  begin
    Memo1.Lines.Add('Le DataSet n''est pas actif');
    Exit;
  end;

  // Afficher l'état actuel
  case FDQuery1.State of
    dsBrowse: EtatTexte := 'Navigation';
    dsEdit: EtatTexte := 'Édition';
    dsInsert: EtatTexte := 'Insertion';
    dsSetKey: EtatTexte := 'Définition de clé';
    else EtatTexte := 'Autre état';
  end;

  Memo1.Lines.Add('État actuel : ' + EtatTexte);
  Memo1.Lines.Add('Nombre d''enregistrements : ' + IntToStr(FDQuery1.RecordCount));
  Memo1.Lines.Add('Position actuelle : ' + IntToStr(FDQuery1.RecNo));

  // Vérifier si nous sommes au début ou à la fin
  if FDQuery1.BOF then
    Memo1.Lines.Add('Au début du DataSet');
  if FDQuery1.EOF then
    Memo1.Lines.Add('À la fin du DataSet');
end;
```

## Navigation dans un DataSet

Les DataSets offrent plusieurs méthodes pour naviguer entre les enregistrements :

| Méthode | Description |
|---------|-------------|
| `First` | Se déplace au premier enregistrement |
| `Last` | Se déplace au dernier enregistrement |
| `Next` | Se déplace à l'enregistrement suivant |
| `Prior` | Se déplace à l'enregistrement précédent |
| `MoveBy(n)` | Se déplace de n enregistrements (avant ou arrière) |

### Exemple de navigation

```delphi
procedure TForm1.btnPremierClick(Sender: TObject);
begin
  FDQuery1.First;
  MettreAJourStatut;
end;

procedure TForm1.btnPrecedentClick(Sender: TObject);
begin
  FDQuery1.Prior;
  MettreAJourStatut;
end;

procedure TForm1.btnSuivantClick(Sender: TObject);
begin
  FDQuery1.Next;
  MettreAJourStatut;
end;

procedure TForm1.btnDernierClick(Sender: TObject);
begin
  FDQuery1.Last;
  MettreAJourStatut;
end;

procedure TForm1.MettreAJourStatut;
begin
  StatusBar1.SimpleText := Format('Enregistrement %d sur %d',
    [FDQuery1.RecNo, FDQuery1.RecordCount]);
end;
```

## Modification des données

Les DataSets permettent également de modifier, ajouter ou supprimer des enregistrements :

### Ajouter un nouvel enregistrement

```delphi
procedure TForm1.btnAjouterClick(Sender: TObject);
begin
  FDQuery1.Append;  // Le DataSet passe en état dsInsert
  // À ce stade, l'utilisateur peut remplir les champs via les contrôles DB-aware
  // ou vous pouvez définir des valeurs par défaut :
  FDQuery1.FieldByName('date_creation').AsDateTime := Now;
  FDQuery1.FieldByName('actif').AsBoolean := True;
end;
```

### Modifier un enregistrement existant

```delphi
procedure TForm1.btnModifierClick(Sender: TObject);
begin
  if not FDQuery1.IsEmpty then
  begin
    FDQuery1.Edit;  // Le DataSet passe en état dsEdit
    // L'utilisateur peut maintenant modifier les champs
  end;
end;
```

### Enregistrer les modifications

```delphi
procedure TForm1.btnEnregistrerClick(Sender: TObject);
begin
  if FDQuery1.State in [dsEdit, dsInsert] then
    FDQuery1.Post;  // Valide les modifications
end;
```

### Annuler les modifications

```delphi
procedure TForm1.btnAnnulerClick(Sender: TObject);
begin
  if FDQuery1.State in [dsEdit, dsInsert] then
    FDQuery1.Cancel;  // Annule les modifications
end;
```

### Supprimer un enregistrement

```delphi
procedure TForm1.btnSupprimerClick(Sender: TObject);
begin
  if not FDQuery1.IsEmpty then
    if MessageDlg('Êtes-vous sûr de vouloir supprimer cet enregistrement ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      FDQuery1.Delete;
end;
```

## Accès aux champs d'un DataSet

Il existe plusieurs façons d'accéder aux valeurs des champs :

### 1. Via FieldByName

C'est la méthode la plus simple mais pas la plus efficace :

```delphi
// Lire une valeur
NomClient := FDQuery1.FieldByName('nom').AsString;
DateNaissance := FDQuery1.FieldByName('date_naissance').AsDateTime;
SoldeCompte := FDQuery1.FieldByName('solde').AsFloat;

// Modifier une valeur
FDQuery1.Edit;
FDQuery1.FieldByName('adresse').AsString := 'Nouvelle adresse';
FDQuery1.Post;
```

### 2. Via des objets TField persistants

Cette méthode est plus efficace pour les accès fréquents :

1. Sélectionnez votre DataSet dans l'IDE
2. Cliquez-droit et choisissez "Fields Editor"
3. Cliquez-droit dans l'éditeur et choisissez "Add all fields"

Delphi va créer des objets `TField` pour chaque colonne. Vous pouvez alors y accéder directement :

```delphi
// Supposons que vous avez ajouté les champs "nom", "date_naissance" et "solde"
NomClient := FDQuery1nom.AsString;  // Notez l'absence de FieldByName
DateNaissance := FDQuery1date_naissance.AsDateTime;
SoldeCompte := FDQuery1solde.AsFloat;
```

### 3. Via une variable de champ

Pour un accès encore plus rapide dans une boucle :

```delphi
procedure TForm1.CalculerTotal;
var
  ChampPrix: TField;
  Total: Double;
begin
  Total := 0;

  ChampPrix := FDQuery1.FieldByName('prix');  // Récupérer le champ une seule fois

  FDQuery1.First;
  while not FDQuery1.EOF do
  begin
    Total := Total + ChampPrix.AsFloat;  // Utiliser la référence au champ
    FDQuery1.Next;
  end;

  LabelTotal.Caption := 'Total : ' + FormatFloat('#,##0.00 €', Total);
end;
```

## Filtrage des données

Les DataSets permettent de filtrer les données sans avoir à exécuter une nouvelle requête SQL :

```delphi
procedure TForm1.btnFiltrerClick(Sender: TObject);
begin
  // Activer le filtre
  FDQuery1.Filtered := False;  // Désactiver d'abord pour éviter les problèmes
  FDQuery1.Filter := 'ville = ''Paris'' AND age > 18';
  FDQuery1.Filtered := True;

  StatusBar1.SimpleText := Format('%d enregistrements après filtrage',
    [FDQuery1.RecordCount]);
end;

procedure TForm1.btnTousClick(Sender: TObject);
begin
  // Désactiver le filtre
  FDQuery1.Filtered := False;
  FDQuery1.Filter := '';

  StatusBar1.SimpleText := Format('%d enregistrements au total',
    [FDQuery1.RecordCount]);
end;
```

## Recherche dans un DataSet

Vous pouvez rechercher des enregistrements spécifiques :

```delphi
procedure TForm1.btnRechercherClick(Sender: TObject);
var
  ValeurRecherchee: string;
begin
  ValeurRecherchee := EditRecherche.Text;

  if ValeurRecherchee = '' then Exit;

  // Recherche à partir de la position actuelle
  FDQuery1.FindNearest([ValeurRecherchee]);

  // Ou recherche exacte (doit être au début du DataSet)
  // FDQuery1.First;
  // if not FDQuery1.Locate('nom', ValeurRecherchee, []) then
  //   ShowMessage('Valeur non trouvée');
end;
```

## Tri des données

Vous pouvez également trier les données sans nouvelle requête SQL :

```delphi
procedure TForm1.btnTrierNomClick(Sender: TObject);
begin
  FDQuery1.IndexFieldNames := 'nom';  // Tri par le champ "nom"
end;

procedure TForm1.btnTrierDateClick(Sender: TObject);
begin
  FDQuery1.IndexFieldNames := 'date_creation DESC';  // Tri descendant
end;
```

## Événements importants des DataSets

Les DataSets fournissent plusieurs événements qui vous permettent de contrôler et de réagir aux changements :

| Événement | Description |
|-----------|-------------|
| `BeforeOpen` | Avant l'ouverture du DataSet |
| `AfterOpen` | Après l'ouverture du DataSet |
| `BeforeClose` | Avant la fermeture du DataSet |
| `AfterClose` | Après la fermeture du DataSet |
| `BeforeInsert` | Avant l'insertion d'un nouvel enregistrement |
| `AfterInsert` | Après l'insertion d'un nouvel enregistrement |
| `BeforeEdit` | Avant la modification d'un enregistrement |
| `AfterEdit` | Après le passage en mode édition |
| `BeforePost` | Avant l'enregistrement des modifications |
| `AfterPost` | Après l'enregistrement des modifications |
| `BeforeDelete` | Avant la suppression d'un enregistrement |
| `AfterDelete` | Après la suppression d'un enregistrement |
| `OnNewRecord` | Lors de la création d'un nouvel enregistrement |

### Exemple d'utilisation des événements

```delphi
procedure TForm1.FDQuery1BeforePost(DataSet: TDataSet);
begin
  // Vérification avant enregistrement
  if DataSet.FieldByName('email').AsString = '' then
  begin
    ShowMessage('L''email est obligatoire !');
    Abort;  // Annule l'opération Post
  end;

  // Mise à jour automatique de certains champs
  if DataSet.State = dsInsert then
    DataSet.FieldByName('date_creation').AsDateTime := Now
  else if DataSet.State = dsEdit then
    DataSet.FieldByName('date_modification').AsDateTime := Now;
end;

procedure TForm1.FDQuery1AfterPost(DataSet: TDataSet);
begin
  // Actions après enregistrement
  StatusBar1.SimpleText := 'Enregistrement sauvegardé à ' +
                           TimeToStr(Now);

  // Rafraîchir d'autres parties de l'application si nécessaire
  MettreAJourStatistiques;
end;
```

## Le composant TDataSource en détail

Le `TDataSource` joue un rôle crucial en transmettant les données et les événements entre le DataSet et les contrôles visuels.

### Propriétés importantes de TDataSource

- **DataSet** : Le DataSet auquel ce DataSource est associé
- **Enabled** : Active ou désactive la source de données
- **AutoEdit** : Détermine si les contrôles peuvent automatiquement mettre le DataSet en mode édition

### Événements utiles de TDataSource

- **OnDataChange** : Déclenché lorsque les données changent
- **OnStateChange** : Déclenché lorsque l'état du DataSet change
- **OnUpdateData** : Déclenché lorsque les données sont mises à jour

### Exemple d'utilisation des événements TDataSource

```delphi
procedure TForm1.DataSource1StateChange(Sender: TObject);
begin
  // Activer/désactiver les boutons en fonction de l'état
  btnAjouter.Enabled := not (DataSource1.State in [dsEdit, dsInsert]);
  btnModifier.Enabled := (DataSource1.State = dsBrowse) and not FDQuery1.IsEmpty;
  btnSupprimer.Enabled := (DataSource1.State = dsBrowse) and not FDQuery1.IsEmpty;
  btnEnregistrer.Enabled := DataSource1.State in [dsEdit, dsInsert];
  btnAnnuler.Enabled := DataSource1.State in [dsEdit, dsInsert];
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
  // Mise à jour de l'interface en fonction de l'enregistrement actuel
  if not FDQuery1.IsEmpty then
    StatusBar1.SimpleText := 'Client : ' + FDQuery1.FieldByName('nom').AsString
  else
    StatusBar1.SimpleText := 'Aucun client';
end;
```

## Utilisation de plusieurs DataSources

Vous pouvez avoir plusieurs DataSources dans une application, chacun connecté à un DataSet différent. Cela est particulièrement utile pour gérer des relations maître-détail.

### Exemple de relation maître-détail

```delphi
// Configuration des DataSets
FDQueryClients.Connection := FDConnection1;
FDQueryClients.SQL.Text := 'SELECT * FROM clients';

FDQueryCommandes.Connection := FDConnection1;
FDQueryCommandes.SQL.Text := 'SELECT * FROM commandes WHERE client_id = :client_id';
FDQueryCommandes.ParamByName('client_id').DataType := ftInteger;

// Configuration des DataSources
DataSourceClients.DataSet := FDQueryClients;
DataSourceCommandes.DataSet := FDQueryCommandes;

// Liaison maître-détail
FDQueryCommandes.MasterSource := DataSourceClients;
FDQueryCommandes.MasterFields := 'id';  // Champ dans la table clients
FDQueryCommandes.IndexFieldNames := 'client_id';  // Champ correspondant dans la table commandes

// Activation des DataSets
FDQueryClients.Active := True;
FDQueryCommandes.Active := True;
```

Dans cet exemple, lorsque l'utilisateur navigue entre les clients, la liste des commandes est automatiquement filtrée pour n'afficher que les commandes du client sélectionné.

## Bonnes pratiques

Pour une utilisation efficace des DataSets et DataSources :

1. **Nommez clairement vos composants** : Utilisez des noms descriptifs comme `dsClients` pour un DataSource lié aux clients.

2. **Limitez les données chargées** : Évitez de charger des tables entières si vous n'en avez pas besoin. Utilisez des requêtes SQL avec des clauses WHERE appropriées.

3. **Utilisez le mode déconnecté** : Chargez les données, déconnectez-vous de la base, puis reconnectez-vous uniquement pour valider les modifications.

4. **Créez des objets TField persistants** pour les champs fréquemment utilisés.

5. **Désactivez AutoEdit** si vous souhaitez contrôler précisément quand les utilisateurs peuvent éditer les données.

6. **Activez CachedUpdates** pour les opérations en lot :
   ```delphi
   FDQuery1.CachedUpdates := True;
   // ... modifications multiples ...
   FDQuery1.ApplyUpdates;  // Envoie toutes les modifications à la base
   ```

7. **Utilisez des événements** pour valider les données et maintenir l'intégrité.

## Exemple complet : Formulaire de gestion des clients

Voici un exemple qui rassemble plusieurs concepts vus dans cette section :

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
    FDConnection1: TFDConnection;
    FDQueryClients: TFDQuery;
    DataSourceClients: TDataSource;
    PanelHaut: TPanel;
    DBNavigator1: TDBNavigator;
    PanelCentre: TPanel;
    DBGrid1: TDBGrid;
    PanelDroite: TPanel;
    LabelNom: TLabel;
    DBEditNom: TDBEdit;
    LabelPrenom: TLabel;
    DBEditPrenom: TDBEdit;
    LabelEmail: TLabel;
    DBEditEmail: TDBEdit;
    LabelTelephone: TLabel;
    DBEditTelephone: TDBEdit;
    GroupBoxRecherche: TGroupBox;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;
    StatusBar1: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure DataSourceClientsStateChange(Sender: TObject);
    procedure DataSourceClientsDataChange(Sender: TObject; Field: TField);
    procedure FDQueryClientsBeforePost(DataSet: TDataSet);
  private
    procedure ConnecterBaseDeDonnees;
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

  // Initialiser les contrôles
  EditRecherche.Clear;

  // Configurer les événements du DataSource et DataSet
  DataSourceClients.OnStateChange := DataSourceClientsStateChange;
  DataSourceClients.OnDataChange := DataSourceClientsDataChange;
  FDQueryClients.BeforePost := FDQueryClientsBeforePost;
end;

procedure TFormGestionClients.ConnecterBaseDeDonnees;
begin
  try
    FDConnection1.Connected := True;

    // Configurer la requête
    FDQueryClients.Connection := FDConnection1;
    FDQueryClients.SQL.Text := 'SELECT * FROM clients ORDER BY nom, prenom';
    FDQueryClients.Open;

    MettreAJourStatut;
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;

procedure TFormGestionClients.FormDestroy(Sender: TObject);
begin
  // Fermer proprement
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

procedure TFormGestionClients.DataSourceClientsStateChange(Sender: TObject);
begin
  // Adapter l'interface en fonction de l'état du DataSet
  case FDQueryClients.State of
    dsEdit, dsInsert:
      StatusBar1.SimpleText := 'Édition en cours...';
    dsBrowse:
      MettreAJourStatut;
  end;

  // Activer/désactiver la recherche pendant l'édition
  GroupBoxRecherche.Enabled := (FDQueryClients.State = dsBrowse);
end;

procedure TFormGestionClients.DataSourceClientsDataChange(Sender: TObject; Field: TField);
begin
  // Réagir au changement d'enregistrement courant
  if not FDQueryClients.IsEmpty then
    Caption := 'Gestion des clients - ' + FDQueryClients.FieldByName('nom').AsString +
               ' ' + FDQueryClients.FieldByName('prenom').AsString
  else
    Caption := 'Gestion des clients';
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

## Conclusion

Les DataSets et DataSources sont des concepts fondamentaux pour travailler avec des données dans Delphi. Ils fournissent une abstraction puissante qui vous permet de manipuler des données sans vous soucier des détails de la base de données sous-jacente.

Avec ces composants, vous pouvez :
- Afficher et modifier des données facilement dans votre interface utilisateur
- Naviguer, filtrer et trier les enregistrements
- Valider les données avant qu'elles ne soient enregistrées
- Créer des relations maître-détail pour gérer des données hiérarchiques

Dans la prochaine section, nous explorerons les contrôles liés aux données qui permettent d'afficher et de modifier les informations de vos DataSets.

---

**À suivre :** 8.7 Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)

⏭️ [Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)](/08-acces-aux-bases-de-donnees-mysql-mariadb/07-controles-lies-aux-donnees.md)
