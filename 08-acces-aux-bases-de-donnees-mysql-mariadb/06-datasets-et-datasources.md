🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.6 DataSets et DataSources

## Introduction

Dans les sections précédentes, vous avez appris à exécuter des requêtes SQL pour manipuler les données. Maintenant, nous allons approfondir le concept de **DataSet** et comprendre comment **DataSource** fait le lien entre vos données et l'interface utilisateur.

Ces composants sont au cœur de l'architecture de données de Delphi et vous permettent de créer des interfaces riches et interactives avec un minimum de code.

## Qu'est-ce qu'un DataSet ?

### Définition

Un **DataSet** est un conteneur de données en mémoire qui représente un ensemble d'enregistrements provenant d'une base de données. C'est comme une "table virtuelle" dans votre application.

### Analogie : Le classeur de fiches

Imaginez un **DataSet** comme un classeur de fiches posé sur votre bureau :

```
┌─────────────────────────────────────┐
│  DATASET (Classeur en mémoire)      │
├─────────────────────────────────────┤
│  Fiche 1 : Dupont Jean              │ ← Premier enregistrement
│  Fiche 2 : Martin Sophie            │
│  Fiche 3 : Bernard Luc              │ ← Enregistrement courant (curseur)
│  Fiche 4 : Durand Marie             │
│  Fiche 5 : Petit Claire             │ ← Dernier enregistrement
└─────────────────────────────────────┘
```

- Vous pouvez **parcourir** les fiches (First, Next, Prior, Last)
- Vous pouvez **lire** le contenu de la fiche courante
- Vous pouvez **modifier** une fiche (Edit)
- Vous pouvez **ajouter** une nouvelle fiche (Insert, Append)
- Vous pouvez **supprimer** une fiche (Delete)

### En Delphi

Dans Delphi, plusieurs composants héritent de `TDataSet` :

| Composant | Type | Usage |
|-----------|------|-------|
| **TFDQuery** | DataSet FireDAC | Le plus utilisé, pour les requêtes SQL |
| **TFDTable** | DataSet FireDAC | Accès direct à une table |
| **TFDStoredProc** | DataSet FireDAC | Exécution de procédures stockées |
| **TClientDataSet** | DataSet en mémoire | Données sans connexion à la base |

**Dans ce chapitre**, nous nous concentrerons principalement sur **TFDQuery** car c'est le composant le plus polyvalent et le plus utilisé.

## Le cycle de vie d'un DataSet

Un DataSet passe par différentes étapes dans son cycle de vie :

```
┌──────────────┐
│   INACTIVE   │ ← Dataset fermé, pas de données
└──────┬───────┘
       │ Open
       ▼
┌──────────────┐
│    BROWSE    │ ← Navigation et consultation
└──────┬───────┘
       │ Edit/Insert
       ▼
┌──────────────┐
│ EDIT/INSERT  │ ← Modification en cours
└──────┬───────┘
       │ Post → retour à BROWSE
       │ Cancel → retour à BROWSE
       ▼
┌──────────────┐
│    BROWSE    │
└──────┬───────┘
       │ Close
       ▼
┌──────────────┐
│   INACTIVE   │
└──────────────┘
```

## États d'un DataSet

### Les états possibles

Un DataSet est toujours dans l'un de ces états :

| État | Constante | Description |
|------|-----------|-------------|
| **Inactif** | `dsInactive` | Dataset fermé, aucune donnée accessible |
| **Navigation** | `dsBrowse` | Dataset ouvert, consultation des données |
| **Édition** | `dsEdit` | Modification d'un enregistrement existant |
| **Insertion** | `dsInsert` | Création d'un nouvel enregistrement |
| **Calcul** | `dsCalcFields` | Calcul de champs calculés (usage avancé) |

### Vérifier l'état courant

```pascal
// Obtenir l'état actuel
case FDQuery1.State of
  dsInactive: ShowMessage('Dataset fermé');
  dsBrowse:   ShowMessage('Consultation');
  dsEdit:     ShowMessage('Modification en cours');
  dsInsert:   ShowMessage('Ajout en cours');
end;

// Vérifier un état spécifique
if FDQuery1.State = dsEdit then
  ShowMessage('Modification en cours');

// Vérifier si le dataset est actif
if FDQuery1.Active then
  ShowMessage('Dataset ouvert')
else
  ShowMessage('Dataset fermé');
```

## Navigation dans un DataSet

### Le curseur d'enregistrement

Le **curseur** est un pointeur qui indique quel enregistrement est actuellement actif dans le DataSet.

### Méthodes de navigation

```pascal
// ─── Positionnement absolu ───
FDQuery1.First;    // Aller au premier enregistrement
FDQuery1.Last;     // Aller au dernier enregistrement

// ─── Déplacement relatif ───
FDQuery1.Next;     // Enregistrement suivant
FDQuery1.Prior;    // Enregistrement précédent

// ─── Déplacement par index ───
FDQuery1.RecNo := 5;  // Aller au 5ème enregistrement (si disponible)

// ─── Déplacement par recherche ───
FDQuery1.Locate('nom', 'Dupont', []);  // Chercher et positionner
```

### Propriétés de position

```pascal
// Vérifier la position
if FDQuery1.Bof then
  ShowMessage('Début du dataset');

if FDQuery1.Eof then
  ShowMessage('Fin du dataset');

// Numéro de l'enregistrement courant
ShowMessage('Enregistrement n°' + IntToStr(FDQuery1.RecNo));

// Nombre total d'enregistrements
ShowMessage('Total : ' + IntToStr(FDQuery1.RecordCount) + ' enregistrements');

// Vérifier si le dataset est vide
if FDQuery1.IsEmpty then
  ShowMessage('Aucun enregistrement')
else
  ShowMessage('Dataset contient des données');
```

### Parcourir tous les enregistrements

```pascal
procedure ParcourrirTous;
begin
  FDQuery1.First;
  while not FDQuery1.Eof do
  begin
    // Traiter l'enregistrement courant
    ShowMessage(FDQuery1.FieldByName('nom').AsString);

    // Passer au suivant
    FDQuery1.Next;
  end;
end;
```

**Bonne pratique :** Toujours vérifier `Eof` pour éviter de dépasser la fin du DataSet.

## Accès aux champs (Fields)

### Les différentes méthodes

Il existe plusieurs façons d'accéder aux valeurs des champs :

#### Méthode 1 : FieldByName (recommandée)

```pascal
// Lecture
Nom := FDQuery1.FieldByName('nom').AsString;
Age := FDQuery1.FieldByName('age').AsInteger;
Salaire := FDQuery1.FieldByName('salaire').AsCurrency;
DateNaissance := FDQuery1.FieldByName('date_naissance').AsDateTime;

// Écriture (en mode Edit ou Insert)
FDQuery1.Edit;
FDQuery1.FieldByName('nom').AsString := 'Nouveau nom';
FDQuery1.Post;
```

**Avantages :**
- Lisible et explicite
- Indépendant de l'ordre des colonnes
- Génère une erreur si le champ n'existe pas

#### Méthode 2 : Notation crochets (syntaxe courte)

```pascal
// Lecture
Nom := FDQuery1['nom'];
Email := FDQuery1['email'];

// Écriture
FDQuery1.Edit;
FDQuery1['nom'] := 'Nouveau nom';
FDQuery1.Post;
```

**Avantages :**
- Syntaxe plus courte
- Pratique pour du code rapide

#### Méthode 3 : Par index

```pascal
// Accès par position (0, 1, 2...)
Nom := FDQuery1.Fields[0].AsString;
Prenom := FDQuery1.Fields[1].AsString;
```

**Inconvénients :**
- Fragile : si l'ordre des colonnes change, le code casse
- Moins lisible
- **À éviter** sauf cas spécifiques

#### Méthode 4 : Champs persistants (avancé)

Dans l'éditeur de champs du dataset, vous pouvez créer des propriétés persistantes :

```pascal
// Après avoir créé les champs persistants dans l'IDE
Nom := FDQuery1nom.AsString;  // Plus rapide, type-safe
Age := FDQuery1age.AsInteger;
```

### Conversions de types

Chaque champ propose différentes conversions :

| Méthode | Type retourné | Usage |
|---------|---------------|-------|
| `AsString` | String | Texte |
| `AsInteger` | Integer | Nombres entiers |
| `AsInt64` | Int64 | Grands entiers |
| `AsFloat` | Double | Nombres décimaux |
| `AsCurrency` | Currency | Montants monétaires |
| `AsBoolean` | Boolean | Vrai/Faux |
| `AsDateTime` | TDateTime | Date et heure |
| `AsDate` | TDate | Date seulement |
| `AsTime` | TTime | Heure seulement |

### Gestion des valeurs NULL

```pascal
// Vérifier si un champ est NULL
if FDQuery1.FieldByName('telephone').IsNull then
  ShowMessage('Pas de téléphone renseigné')
else
  Telephone := FDQuery1.FieldByName('telephone').AsString;

// Assigner NULL à un champ
FDQuery1.Edit;
FDQuery1.FieldByName('telephone').Clear;  // Définit à NULL
FDQuery1.Post;

// Valeur par défaut si NULL
Telephone := FDQuery1.FieldByName('telephone').AsString;
if Telephone = '' then
  Telephone := 'Non renseigné';
```

## Modification des données avec un DataSet

### Ajouter un enregistrement

#### Méthode Append

```pascal
// Append : Ajouter à la fin
FDQuery1.Append;  // Passe en mode dsInsert
FDQuery1.FieldByName('nom').AsString := 'Dupont';
FDQuery1.FieldByName('prenom').AsString := 'Jean';
FDQuery1.FieldByName('email').AsString := 'jean.dupont@email.fr';
FDQuery1.Post;  // Valider et enregistrer dans la base
```

#### Méthode Insert

```pascal
// Insert : Insérer à la position courante
FDQuery1.Insert;  // Passe en mode dsInsert
FDQuery1.FieldByName('nom').AsString := 'Martin';
FDQuery1.FieldByName('prenom').AsString := 'Sophie';
FDQuery1.Post;
```

**Note :** Pour la plupart des bases de données, `Insert` et `Append` ont le même effet. `Append` est plus couramment utilisé.

### Modifier un enregistrement

```pascal
// Passer en mode édition
FDQuery1.Edit;  // Passe en mode dsEdit

// Modifier les champs
FDQuery1.FieldByName('email').AsString := 'nouveau@email.fr';
FDQuery1.FieldByName('telephone').AsString := '0612345678';

// Valider les modifications
FDQuery1.Post;  // Enregistre dans la base
```

### Annuler les modifications

```pascal
// Annuler les modifications en cours
FDQuery1.Cancel;  // Retourne en mode dsBrowse sans enregistrer
```

### Supprimer un enregistrement

```pascal
// Supprimer l'enregistrement courant
if MessageDlg('Confirmer la suppression ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
begin
  FDQuery1.Delete;  // Supprime immédiatement de la base
end;
```

## Qu'est-ce qu'un DataSource ?

### Définition

Un **DataSource** (TDataSource) est un composant qui fait le **lien** entre un DataSet et les composants visuels (contrôles liés aux données).

### Analogie : Le câble de connexion

```
┌──────────────┐       ┌──────────────┐       ┌──────────────┐
│   TFDQuery   │──────►│ TDataSource  │──────►│   DBGrid     │
│  (Données)   │       │   (Câble)    │       │ (Affichage)  │
└──────────────┘       └──────────────┘       └──────────────┘
```

Le DataSource est comme un **câble** qui :
- Transmet les données du DataSet vers les contrôles visuels
- Notifie les contrôles quand les données changent
- Synchronise plusieurs contrôles sur la même source

### Configuration d'un DataSource

```pascal
// Au design time (dans l'inspecteur d'objets)
DataSource1.DataSet := FDQuery1;

// Ou par code
DataSource1.DataSet := FDQuery1;
```

## Composants liés aux données (Data-Aware Controls)

Les composants visuels qui peuvent être liés à un DataSource sont appelés **Data-Aware Controls** (contrôles sensibles aux données).

### Les principaux composants

| Composant | Usage | Propriété DataSource |
|-----------|-------|---------------------|
| **TDBGrid** | Grille de données (tableau) | `DataSource` |
| **TDBEdit** | Champ de saisie | `DataSource` + `DataField` |
| **TDBMemo** | Zone de texte multiligne | `DataSource` + `DataField` |
| **TDBText** | Texte en lecture seule | `DataSource` + `DataField` |
| **TDBComboBox** | Liste déroulante | `DataSource` + `DataField` |
| **TDBCheckBox** | Case à cocher | `DataSource` + `DataField` |
| **TDBImage** | Image | `DataSource` + `DataField` |
| **TDBLookupComboBox** | Liste de choix depuis une autre table | `DataSource` + `DataField` |
| **TDBNavigator** | Boutons de navigation | `DataSource` |

### Configuration d'un contrôle lié

```pascal
// DBEdit pour afficher le nom
DBEdit1.DataSource := DataSource1;
DBEdit1.DataField := 'nom';

// DBEdit pour afficher le prénom
DBEdit2.DataSource := DataSource1;
DBEdit2.DataField := 'prenom';

// DBGrid pour afficher tout
DBGrid1.DataSource := DataSource1;
```

**Automatisme :** Quand vous changez d'enregistrement dans le DataSet, **tous** les contrôles liés se mettent à jour automatiquement !

## Exemple complet : Formulaire maître-détail

Créons une interface complète avec navigation et édition :

### Composants sur le formulaire

```
┌─────────────────────────────────────────────┐
│  Gestion des Clients                    [X] │
├─────────────────────────────────────────────┤
│  [|◄] [◄] [►] [►|] [+] [✓] [X] [🗑]         │ ← DBNavigator
├─────────────────────────────────────────────┤
│  ┌───────────────────────────────────────┐  │
│  │ ID | Nom    | Prénom | Email          │  │ ← DBGrid
│  │ 1  | Dupont | Jean   | jean@...       │  │
│  │ 2  | Martin | Sophie | sophie@...     │  │
│  └───────────────────────────────────────┘  │
├─────────────────────────────────────────────┤
│  Détails de l'enregistrement courant        │
│  ┌─────────────────────────────────────┐    │
│  │ Nom :     [Dupont              ]    │    │ ← DBEdit
│  │ Prénom :  [Jean                ]    │    │ ← DBEdit
│  │ Email :   [jean@email.fr       ]    │    │ ← DBEdit
│  │ Tél :     [0601020304          ]    │    │ ← DBEdit
│  └─────────────────────────────────────┘    │
└─────────────────────────────────────────────┘
```

### Code de mise en place

```pascal
unit uFormClients;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Forms,
  Vcl.DBCtrls, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, Vcl.Controls,
  FireDAC.Comp.Client, Data.DB, Vcl.ExtCtrls;

type
  TFormClients = class(TForm)
    // Composants FireDAC
    FDConnection1: TFDConnection;
    FDQueryClients: TFDQuery;
    DataSourceClients: TDataSource;

    // Composants visuels
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    DBEditNom: TDBEdit;
    DBEditPrenom: TDBEdit;
    DBEditEmail: TDBEdit;
    DBEditTel: TDBEdit;

    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FormClients: TFormClients;

implementation

{$R *.dfm}

procedure TFormClients.FormCreate(Sender: TObject);
begin
  // Connecter à la base
  FDConnection1.Connected := True;

  // Configurer le DataSource
  DataSourceClients.DataSet := FDQueryClients;

  // Configurer les DBEdit
  DBEditNom.DataSource := DataSourceClients;
  DBEditNom.DataField := 'nom';

  DBEditPrenom.DataSource := DataSourceClients;
  DBEditPrenom.DataField := 'prenom';

  DBEditEmail.DataSource := DataSourceClients;
  DBEditEmail.DataField := 'email';

  DBEditTel.DataSource := DataSourceClients;
  DBEditTel.DataField := 'telephone';

  // Configurer le DBGrid
  DBGrid1.DataSource := DataSourceClients;

  // Configurer le DBNavigator
  DBNavigator1.DataSource := DataSourceClients;

  // Charger les données
  FDQueryClients.SQL.Text := 'SELECT * FROM clients ORDER BY nom';
  FDQueryClients.Open;
end;

end.
```

**Magie de Delphi :** Avec ce code, vous avez :
- Navigation automatique (via DBNavigator)
- Affichage dans la grille
- Édition directe dans les DBEdit
- Synchronisation automatique de tous les contrôles !

## Le composant DBNavigator

**TDBNavigator** est un composant très pratique qui fournit des boutons de navigation et d'édition.

### Les boutons du DBNavigator

| Bouton | Action | Méthode équivalente |
|--------|--------|---------------------|
| `|◄` | Premier enregistrement | `First` |
| `◄` | Enregistrement précédent | `Prior` |
| `►` | Enregistrement suivant | `Next` |
| `►|` | Dernier enregistrement | `Last` |
| `+` | Ajouter un enregistrement | `Insert` |
| `🗑` | Supprimer l'enregistrement | `Delete` |
| `✓` | Valider les modifications | `Post` |
| `X` | Annuler les modifications | `Cancel` |
| `↻` | Rafraîchir les données | `Refresh` |

### Personnaliser le DBNavigator

```pascal
// Masquer certains boutons
DBNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbRefresh];

// Confirmer avant suppression
DBNavigator1.ConfirmDelete := True;

// Changer les hints (bulles d'aide)
DBNavigator1.Hints.Clear;
DBNavigator1.Hints.Add('Premier');
DBNavigator1.Hints.Add('Précédent');
// etc.
DBNavigator1.ShowHint := True;
```

## Événements importants d'un DataSet

Les DataSets génèrent des événements que vous pouvez intercepter pour ajouter votre logique.

### Événements de navigation

```pascal
// Avant de changer d'enregistrement
procedure TForm1.FDQuery1BeforeScroll(DataSet: TDataSet);
begin
  // Vérifier si des modifications sont en cours
  if DataSet.State in [dsEdit, dsInsert] then
  begin
    if MessageDlg('Enregistrer les modifications ?',
       mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      DataSet.Post
    else
      DataSet.Cancel;
  end;
end;

// Après avoir changé d'enregistrement
procedure TForm1.FDQuery1AfterScroll(DataSet: TDataSet);
begin
  // Mettre à jour l'interface
  StatusBar1.SimpleText := 'Enregistrement ' +
    IntToStr(DataSet.RecNo) + ' sur ' + IntToStr(DataSet.RecordCount);
end;
```

### Événements de modification

```pascal
// Avant d'insérer un enregistrement
procedure TForm1.FDQuery1BeforeInsert(DataSet: TDataSet);
begin
  // Vérifier les permissions
  if not UtilisateurPeutAjouter then
  begin
    ShowMessage('Vous n''avez pas les droits pour ajouter');
    Abort;  // Annule l'opération
  end;
end;

// Avant de valider les modifications
procedure TForm1.FDQuery1BeforePost(DataSet: TDataSet);
begin
  // Validation personnalisée
  if Trim(DataSet.FieldByName('nom').AsString) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Abort;  // Empêche le Post
  end;

  // Valider l'email
  if Pos('@', DataSet.FieldByName('email').AsString) = 0 then
  begin
    ShowMessage('Email invalide');
    Abort;
  end;
end;

// Après avoir validé les modifications
procedure TForm1.FDQuery1AfterPost(DataSet: TDataSet);
begin
  ShowMessage('Enregistrement sauvegardé');
end;

// Avant de supprimer
procedure TForm1.FDQuery1BeforeDelete(DataSet: TDataSet);
begin
  if MessageDlg('Confirmer la suppression ?',
     mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Abort;  // Annule la suppression
end;
```

### Événements de champs

```pascal
// Quand un champ change
procedure TForm1.FDQuery1nomChange(Sender: TField);
begin
  // Le champ "nom" a été modifié
  lblNomModifie.Visible := True;
end;

// Validation d'un champ
procedure TForm1.FDQuery1emailValidate(Sender: TField);
begin
  if Pos('@', Sender.AsString) = 0 then
    raise Exception.Create('Email invalide');
end;
```

## Filtrage et tri d'un DataSet

### Filtrer les données

#### Filtre côté serveur (recommandé)

```pascal
// Filtrer avec WHERE dans la requête SQL
FDQuery1.Close;
FDQuery1.SQL.Text := 'SELECT * FROM clients WHERE actif = TRUE';
FDQuery1.Open;
```

#### Filtre côté client

```pascal
// Filtrer après avoir chargé les données
FDQuery1.Filter := 'nom LIKE ''D%''';  // Noms commençant par D
FDQuery1.Filtered := True;

// Désactiver le filtre
FDQuery1.Filtered := False;

// Filtres multiples
FDQuery1.Filter := 'actif = TRUE AND nom LIKE ''D%''';
FDQuery1.Filtered := True;
```

**Important :** Le filtrage côté client charge toutes les données puis filtre en mémoire. Pour de grandes tables, préférez le filtrage côté serveur (WHERE dans SQL).

### Trier les données

#### Tri côté serveur (recommandé)

```pascal
// Trier avec ORDER BY dans la requête
FDQuery1.Close;
FDQuery1.SQL.Text := 'SELECT * FROM clients ORDER BY nom, prenom';
FDQuery1.Open;
```

#### Tri côté client

```pascal
// Trier après chargement (FireDAC)
FDQuery1.IndexFieldNames := 'nom;prenom';  // Tri sur nom puis prénom

// Ordre décroissant
FDQuery1.IndexFieldNames := 'nom:D';  // D pour Descending
```

## Recherche dans un DataSet

### Méthode Locate

```pascal
// Chercher un enregistrement
if FDQuery1.Locate('nom', 'Dupont', []) then
  ShowMessage('Client trouvé')
else
  ShowMessage('Client non trouvé');

// Recherche insensible à la casse
if FDQuery1.Locate('nom', 'dupont', [loCaseInsensitive]) then
  ShowMessage('Trouvé');

// Recherche partielle
if FDQuery1.Locate('nom', 'Dup', [loPartialKey]) then
  ShowMessage('Trouvé');

// Recherche sur plusieurs champs
if FDQuery1.Locate('nom;prenom', VarArrayOf(['Dupont', 'Jean']), []) then
  ShowMessage('Trouvé');
```

### Méthode Lookup

```pascal
// Récupérer une valeur sans déplacer le curseur
var
  Email: Variant;
begin
  Email := FDQuery1.Lookup('id', 5, 'email');
  if not VarIsNull(Email) then
    ShowMessage('Email : ' + VarToStr(Email));
end;
```

## Rafraîchir les données

### Méthode Refresh

```pascal
// Recharger les données depuis la base
FDQuery1.Refresh;

// Ou fermer et rouvrir
FDQuery1.Close;
FDQuery1.Open;
```

### Rafraîchissement automatique

```pascal
// Événement de formulaire avec un Timer
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  FDQuery1.Refresh;  // Recharge toutes les 30 secondes
end;
```

## Propriétés utiles d'un DataSet

| Propriété | Type | Description |
|-----------|------|-------------|
| `Active` | Boolean | Dataset ouvert ou fermé |
| `State` | TDataSetState | État courant (Browse, Edit, Insert...) |
| `RecordCount` | Integer | Nombre d'enregistrements |
| `RecNo` | Integer | Numéro de l'enregistrement courant |
| `Bof` | Boolean | True si au début |
| `Eof` | Boolean | True si à la fin |
| `IsEmpty` | Boolean | True si aucun enregistrement |
| `Modified` | Boolean | True si modifications non sauvegardées |
| `FieldCount` | Integer | Nombre de champs |
| `Fields[]` | TField | Accès aux champs par index |

## Méthodes utiles d'un DataSet

| Méthode | Description |
|---------|-------------|
| `Open` | Ouvre le dataset |
| `Close` | Ferme le dataset |
| `Refresh` | Recharge les données |
| `First` | Premier enregistrement |
| `Last` | Dernier enregistrement |
| `Next` | Enregistrement suivant |
| `Prior` | Enregistrement précédent |
| `Append` | Ajouter à la fin |
| `Insert` | Insérer à la position courante |
| `Edit` | Passer en mode édition |
| `Post` | Valider les modifications |
| `Cancel` | Annuler les modifications |
| `Delete` | Supprimer l'enregistrement |
| `Locate` | Rechercher un enregistrement |
| `DisableControls` | Désactiver les contrôles liés |
| `EnableControls` | Réactiver les contrôles liés |

## Optimisation : DisableControls / EnableControls

Quand vous parcourez de nombreux enregistrements, désactivez les contrôles visuels pour améliorer les performances :

```pascal
procedure TraiterTous;
begin
  FDQuery1.DisableControls;  // Désactive les mises à jour visuelles
  try
    FDQuery1.First;
    while not FDQuery1.Eof do
    begin
      // Traiter l'enregistrement
      // ...

      FDQuery1.Next;
    end;
  finally
    FDQuery1.EnableControls;  // Réactive les mises à jour
  end;
end;
```

**Gain :** Peut être **10 à 100 fois plus rapide** pour les traitements en masse !

## Bonnes pratiques

### ✅ À FAIRE

1. **Toujours utiliser un DataSource** pour lier les données aux contrôles
   ```pascal
   DBEdit1.DataSource := DataSource1;
   DBEdit1.DataField := 'nom';
   ```

2. **Valider dans BeforePost** plutôt que dans le bouton
   ```pascal
   procedure FDQuery1BeforePost(DataSet: TDataSet);
   begin
     // Validation ici
   end;
   ```

3. **Utiliser DisableControls** pour les traitements en masse
   ```pascal
   FDQuery1.DisableControls;
   try
     // Traitement
   finally
     FDQuery1.EnableControls;
   end;
   ```

4. **Vérifier l'état avant modification**
   ```pascal
   if not (FDQuery1.State in [dsEdit, dsInsert]) then
     FDQuery1.Edit;
   ```

### ❌ À ÉVITER

1. **Modifier directement sans Edit**
   ```pascal
   // ❌ ERREUR
   FDQuery1.FieldByName('nom').AsString := 'Nouveau';

   // ✅ CORRECT
   FDQuery1.Edit;
   FDQuery1.FieldByName('nom').AsString := 'Nouveau';
   FDQuery1.Post;
   ```

2. **Oublier le Post**
   ```pascal
   // ❌ Modifications perdues
   FDQuery1.Edit;
   FDQuery1.FieldByName('nom').AsString := 'Nouveau';
   // Pas de Post !
   ```

3. **Accéder aux champs d'un dataset fermé**
   ```pascal
   // ❌ ERREUR
   if not FDQuery1.Active then
     FDQuery1.FieldByName('nom').AsString;  // Exception !
   ```

## Résumé

### DataSet : Les points clés

✅ Un DataSet est un conteneur de données en mémoire
✅ Il a un état (Inactive, Browse, Edit, Insert)
✅ On navigue avec First, Next, Prior, Last
✅ On modifie avec Edit, Post, Cancel
✅ On accède aux champs avec FieldByName

### DataSource : Les points clés

✅ Le DataSource fait le lien entre DataSet et contrôles visuels
✅ Tous les contrôles liés sont synchronisés automatiquement
✅ Un seul DataSource peut servir plusieurs contrôles
✅ Les contrôles DB (DBEdit, DBGrid, etc.) utilisent un DataSource

### Schéma récapitulatif

```
┌──────────────┐       ┌──────────────┐       ┌──────────────┐
│  TFDQuery    │──────►│ TDataSource  │──────►│   DBGrid     │
│              │       │              │       │   DBEdit     │
│  - Open      │       │  - DataSet   │       │   DBText     │
│  - Edit      │       │              │       │   etc.       │
│  - Post      │       │              │       │              │
│  - Next      │       │              │       │  Tous        │
│  - Fields    │       │              │       │  synchronisés│
└──────────────┘       └──────────────┘       └──────────────┘
```

## Prochaines étapes

Vous maîtrisez maintenant les DataSets et DataSources ! Dans les sections suivantes, nous verrons :

1. **Contrôles liés aux données** (DBGrid avancé, DBLookup, etc.)
2. **Live Bindings** pour des liaisons encore plus flexibles
3. **Modèle en couches** pour une meilleure architecture
4. **Optimisation** des performances avec les DataSets

Avec ces connaissances, vous pouvez créer des interfaces riches et interactives qui manipulent les données de manière intuitive et efficace !

⏭️ [Contrôles liés aux données (DBGrid, DBEdit, DBLookupComboBox...)](/08-acces-aux-bases-de-donnees-mysql-mariadb/07-controles-lies-aux-donnees.md)
