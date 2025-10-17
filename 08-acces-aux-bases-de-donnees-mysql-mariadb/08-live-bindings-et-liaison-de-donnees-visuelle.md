🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 8.8 Live Bindings et liaison de données visuelle

## Introduction

Les **Live Bindings** (liaisons dynamiques) sont une technologie moderne de Delphi qui permet de lier des données à des contrôles visuels de manière **flexible et visuelle**, sans nécessairement utiliser les composants DB traditionnels (DBEdit, DBGrid, etc.).

Cette approche offre plus de liberté et de puissance tout en restant simple à mettre en œuvre grâce à un éditeur visuel intégré.

## Pourquoi les Live Bindings ?

### Approche traditionnelle vs Live Bindings

#### Approche traditionnelle (Data-Aware Controls)

```
┌──────────────┐       ┌──────────────┐       ┌──────────────┐
│  TFDQuery    │──────►│ TDataSource  │──────►│   DBEdit     │
│              │       │              │       │   DBGrid     │
└──────────────┘       └──────────────┘       └──────────────┘
```

**Limitations :**
- Nécessite des composants DB spécifiques (DBEdit, DBGrid)
- Moins flexible pour les contrôles non-DB
- Architecture rigide

#### Approche Live Bindings

```
┌──────────────┐       ┌──────────────┐       ┌──────────────┐
│  TFDQuery    │──────►│ BindSource   │──────►│    Edit      │
│  ou Objet    │       │              │       │    ListView  │
│  ou Liste    │       │              │       │  N'importe   │
└──────────────┘       └──────────────┘       │   quel       │
                                              │  contrôle    │
                                              └──────────────┘
```

**Avantages :**
- Fonctionne avec **n'importe quel** contrôle visuel
- Peut lier des **objets** directement (pas seulement des DataSets)
- Liaison **bidirectionnelle** automatique
- Expressions et formatage puissants
- Éditeur visuel pour créer les liaisons

## Quand utiliser les Live Bindings ?

### ✅ Live Bindings sont idéaux pour :

- Applications **multi-plateformes** (FireMonkey)
- Liaison avec des **objets métier** (pas de DataSet)
- Contrôles **non-DB** (ListView, TreeView modernes)
- **Formatage** complexe des données
- Prototypage **rapide** d'interfaces
- Architecture **MVVM** (Model-View-ViewModel)

### ⚠️ Data-Aware Controls traditionnels sont mieux pour :

- Applications **VCL** Windows pures
- Équipes habituées à l'approche traditionnelle
- Besoins simples avec DataSets standards
- Maintenance de code existant

**Note :** Les deux approches peuvent coexister dans une même application !

## Composants des Live Bindings

### Palette LiveBindings

Dans l'IDE, vous trouverez ces composants dans l'onglet **LiveBindings** :

| Composant | Description |
|-----------|-------------|
| `TBindSourceDB` | Source de données depuis un DataSet |
| `TBindingsList` | Conteneur de toutes les liaisons |
| `TPrototypeBindSource` | Source de données de prototypage |
| `TAdapterBindSource` | Adaptateur pour objets personnalisés |
| `TBindNavigator` | Navigateur pour Live Bindings |

### TBindSourceDB : Lier un DataSet

**Rôle :** Exposer un DataSet (TFDQuery, TFDTable) aux Live Bindings.

```pascal
// Au design time
BindSourceDB1.DataSet := FDQueryClients;

// Ou par code
BindSourceDB1.DataSet := FDQueryClients;
BindSourceDB1.AutoEdit := True;  // Passer automatiquement en mode Edit
```

### TBindingsList : Le gestionnaire

**Rôle :** Conteneur invisible qui gère toutes les liaisons de votre formulaire.

```pascal
// Automatiquement placé sur le formulaire quand vous créez une liaison
// Vous n'avez généralement pas besoin d'y toucher
```

## Créer une liaison simple

### Méthode 1 : L'éditeur visuel (recommandé)

C'est la méthode la plus simple et intuitive pour les débutants.

#### Étape 1 : Préparer les composants

1. Placez un **TFDQuery** et configurez-le
2. Placez un **TBindSourceDB** et liez-le au Query
3. Placez un **TEdit** (standard, pas DBEdit !)
4. Placez un **TBindingsList** (si pas déjà présent)

#### Étape 2 : Ouvrir l'éditeur de liaisons

1. Clic droit sur le formulaire
2. Sélectionnez **Bind Visually...**
3. L'éditeur visuel s'ouvre

#### Étape 3 : Créer la liaison

Dans l'éditeur visuel :

```
┌─────────────────────────────────────────────┐
│  LiveBindings Designer                      │
├─────────────────────────────────────────────┤
│  Sources:          Contrôles:               │
│  ┌──────────────┐  ┌──────────────┐         │
│  │BindSourceDB1 │  │   Edit1      │         │
│  │  - nom       │  │   - Text     │         │
│  │  - prenom    │  │              │         │
│  │  - email     │  └──────────────┘         │
│  └──────────────┘                           │
└─────────────────────────────────────────────┘
```

1. Sélectionnez **nom** dans BindSourceDB1
2. Faites-le **glisser** vers **Text** de Edit1
3. Une liaison est créée automatiquement !

**Résultat :** Edit1 affiche maintenant le champ "nom" et se met à jour automatiquement !

### Méthode 2 : Assistant de liaison rapide

1. Clic droit sur le contrôle (Edit1)
2. Sélectionnez **Bind to...**
3. Choisissez la source (BindSourceDB1)
4. Choisissez le champ (nom)
5. Cliquez OK

### Méthode 3 : Par code (avancé)

```pascal
uses
  Data.Bind.Components, Data.Bind.Controls, Data.Bind.EngExt;

procedure TForm1.CreerLiaisonParCode;
var
  Binding: TLinkControlToField;
begin
  Binding := TLinkControlToField.Create(Self);
  Binding.DataSource := BindSourceDB1;
  Binding.FieldName := 'nom';
  Binding.Control := Edit1;
  Binding.Track := True;  // Mise à jour automatique
  Binding.Active := True;
end;
```

## Types de liaisons

### Liaison simple (champ → contrôle)

**Usage :** Afficher/éditer une valeur unique.

```pascal
// Edit pour le nom
Link: BindSourceDB1.nom → Edit1.Text

// Label pour affichage seul
Link: BindSourceDB1.email → Label1.Caption

// CheckBox pour booléen
Link: BindSourceDB1.actif → CheckBox1.Checked
```

### Liaison de liste (DataSet → ListView/Grid)

**Usage :** Afficher plusieurs enregistrements.

#### Avec TListView

```pascal
// Configuration du BindSourceDB
BindSourceDB1.DataSet := FDQueryClients;

// Dans l'éditeur visuel :
// 1. Glisser BindSourceDB1 vers ListView1
// 2. Choisir "Link to ListView"
// 3. Sélectionner les champs à afficher

// Résultat : ListView affiche tous les clients !
```

**Par code :**

```pascal
var
  LinkListView: TLinkListControlToField;
begin
  LinkListView := TLinkListControlToField.Create(Self);
  LinkListView.DataSource := BindSourceDB1;
  LinkListView.FieldName := 'nom';
  LinkListView.Control := ListView1;
  LinkListView.Active := True;
end;
```

#### Avec TStringGrid

```pascal
// Glisser BindSourceDB1 vers StringGrid1
// Choisir "Link to StringGrid"
// Les colonnes sont créées automatiquement pour chaque champ
```

## Liaison bidirectionnelle

Une des forces des Live Bindings : la **synchronisation bidirectionnelle** automatique.

### Fonctionnement

```
┌───────────────┐  ←───────────→  ┌───────────────┐
│  TFDQuery     │                 │   Edit1       │
│  nom = "..."  │                 │  Text = "..." │
└───────────────┘                 └───────────────┘
```

**Changement dans Edit1** → Automatiquement mis à jour dans le DataSet
**Changement dans le DataSet** → Automatiquement mis à jour dans Edit1

### Configuration

```pascal
// Propriété Track = True (par défaut)
LinkControlToField1.Track := True;  // Synchronisation automatique

// AutoActivate = True
LinkControlToField1.AutoActivate := True;  // Active automatiquement

// Notify = True
LinkControlToField1.NotifyOutputs := True;  // Notifie les changements
```

## Expressions et formatage

Les Live Bindings permettent d'utiliser des **expressions** pour transformer les données.

### Formatage simple

```pascal
// Dans l'éditeur de liaisons, onglet "Format"
// Expression : Format('%s %s', [prenom, nom])

// Résultat : "Jean Dupont"
```

### Expressions courantes

#### Concaténation de champs

```pascal
// Expression de sortie (Output)
Expression: Format('%s %s', [nom, prenom])
Direction: Unidirectional (Source → Control)

// Edit1 affiche : "Dupont Jean"
```

#### Formatage de dates

```pascal
// Expression
Expression: FormatDateTime('dd/mm/yyyy', date_inscription)

// Label1 affiche : "15/03/2024"
```

#### Formatage monétaire

```pascal
// Expression
Expression: FormatCurrency(prix)

// Label affiche : "1 234,56 €"
```

#### Conditions

```pascal
// Expression
Expression: if(actif, 'Actif', 'Inactif')

// Label affiche : "Actif" ou "Inactif"
```

### Expressions personnalisées complexes

```pascal
// Dans l'événement OnEvalExpression
procedure TForm1.BindingsList1EvalExpression(Sender: TObject;
  const AExpression: string; out AResult: TValue);
begin
  if AExpression = 'NomComplet' then
  begin
    AResult := FDQueryClients.FieldByName('prenom').AsString + ' ' +
               FDQueryClients.FieldByName('nom').AsString;
  end;
end;

// Utiliser dans l'éditeur :
// Expression: NomComplet
```

## Navigation avec TBindNavigator

**TBindNavigator** est l'équivalent de TDBNavigator pour les Live Bindings.

### Configuration

```pascal
// Lier au BindSourceDB
BindNavigator1.DataSource := BindSourceDB1;

// Personnaliser les boutons
BindNavigator1.Orientation := orHorizontal;  // ou orVertical
BindNavigator1.VisibleButtons := [nbFirst, nbPrior, nbNext, nbLast,
                                   nbInsert, nbDelete, nbEdit,
                                   nbPost, nbCancel, nbRefresh];
```

### Différences avec DBNavigator

- Fonctionne avec **n'importe quelle** source de données
- Design plus **moderne** et personnalisable
- Peut être lié à des **listes d'objets**

## Liaison avec des objets (sans DataSet)

Une des fonctionnalités les plus puissantes : lier directement des **objets Delphi**.

### Définir une classe

```pascal
type
  TClient = class
  private
    FNom: string;
    FPrenom: string;
    FEmail: string;
    FActif: Boolean;
  published
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
    property Email: string read FEmail write FEmail;
    property Actif: Boolean read FActif write FActif;
  end;
```

**Important :** Les propriétés doivent être **published** pour être accessibles aux Live Bindings.

### Utiliser TAdapterBindSource

```pascal
var
  MonClient: TClient;
begin
  // Créer l'objet
  MonClient := TClient.Create;
  MonClient.Nom := 'Dupont';
  MonClient.Prenom := 'Jean';
  MonClient.Email := 'jean@email.fr';
  MonClient.Actif := True;

  // Lier au BindSource
  AdapterBindSource1.Adapter.SetDataObject(MonClient);
  AdapterBindSource1.Active := True;
end;
```

### Lier les contrôles

Dans l'éditeur visuel, liez AdapterBindSource1 aux contrôles comme avec un DataSet.

```pascal
// Edit1 → Nom
// Edit2 → Prenom
// Edit3 → Email
// CheckBox1 → Actif
```

**Résultat :** Les modifications dans les contrôles se reflètent **directement** dans l'objet !

## Liaison avec des listes d'objets

### TObjectBindSourceAdapter

Pour lier une **liste d'objets** (TObjectList, TList<T>) :

```pascal
type
  TClientList = TObjectList<TClient>;

var
  Clients: TClientList;
begin
  // Créer la liste
  Clients := TClientList.Create(True);  // True = possède les objets

  // Ajouter des clients
  Clients.Add(TClient.Create);
  Clients[0].Nom := 'Dupont';
  Clients[0].Prenom := 'Jean';

  Clients.Add(TClient.Create);
  Clients[1].Nom := 'Martin';
  Clients[1].Prenom := 'Sophie';

  // Lier à l'adapter
  AdapterBindSource1.Adapter := TObjectBindSourceAdapter.Create(Self, Clients, True);
  AdapterBindSource1.Active := True;
end;
```

**Lier à un ListView :**

```pascal
// Dans l'éditeur visuel :
// Glisser AdapterBindSource1 vers ListView1
// Configurer les champs à afficher
```

**Résultat :** Le ListView affiche tous les objets de la liste !

## Prototypage avec TPrototypeBindSource

**TPrototypeBindSource** permet de créer rapidement des données de test.

### Utilisation

```pascal
// Au design time
PrototypeBindSource1.Active := True;

// Ajouter des champs
PrototypeBindSource1.AddField('nom', ftString);
PrototypeBindSource1.AddField('prenom', ftString);
PrototypeBindSource1.AddField('email', ftString);

// Générer des données automatiquement
PrototypeBindSource1.RecordCount := 10;  // 10 enregistrements de test
```

**Usage :** Parfait pour **prototyper** une interface avant d'avoir la vraie base de données.

## Interface Master-Detail avec Live Bindings

Créer une relation maître-détail entre deux sources.

### Scénario : Clients et Commandes

```pascal
// Source maître (Clients)
BindSourceDB_Clients.DataSet := FDQueryClients;

// Source détail (Commandes)
BindSourceDB_Commandes.DataSet := FDQueryCommandes;

// Configurer la relation
// Dans FDQueryCommandes :
FDQueryCommandes.MasterSource := DataSourceClients;  // DataSource traditionnel
FDQueryCommandes.MasterFields := 'id=client_id';

// Ou créer un lien dans l'éditeur LiveBindings
// Lier BindSourceDB_Clients.id → BindSourceDB_Commandes.client_id
```

**Résultat :** Quand vous changez de client, les commandes se mettent à jour automatiquement.

## Liaison de validation

Valider les données avec des expressions.

### Expression de validation

```pascal
// Dans l'éditeur de liaisons
// Onglet "Validation"

// Valider que l'email contient @
Expression: Pos('@', email) > 0
Message d'erreur: "Email invalide"

// Valider que le nom n'est pas vide
Expression: Length(Trim(nom)) > 0
Message d'erreur: "Le nom est obligatoire"
```

### Par code

```pascal
procedure TForm1.BindingsList1EvalExpression(Sender: TObject;
  const AExpression: string; out AResult: TValue);
begin
  if AExpression = 'EmailValide' then
  begin
    AResult := Pos('@', Edit1.Text) > 0;
  end;
end;

// Utiliser dans une liaison avec condition
```

## Liaison de calculs

Créer des champs calculés avec Live Bindings.

### Exemple : Prix TTC

```pascal
// Expression dans un Label
Expression: prix_ht * 1.20

// Ou plus complexe :
Expression: if(tva_reduite, prix_ht * 1.055, prix_ht * 1.20)
```

### Champ calculé personnalisé

```pascal
// Dans un TBindExpression
BindExpression1.SourceComponent := BindSourceDB1;
BindExpression1.SourceExpression := 'prix_ht * 1.20';
BindExpression1.ControlComponent := LabelPrixTTC;
BindExpression1.ControlExpression := 'Text';
BindExpression1.Direction := dirSourceToControl;
BindExpression1.Active := True;
```

## Styles visuels avec Live Bindings

Changer l'apparence selon les données.

### Exemple : Colorer selon le statut

```pascal
// Événement OnLocationChanged du BindSourceDB
procedure TForm1.BindSourceDB1LocationChanged(Sender: TObject);
begin
  // Changer la couleur selon l'état
  if FDQueryClients.FieldByName('actif').AsBoolean then
  begin
    Panel1.Color := clLime;
    Label1.Caption := 'ACTIF';
  end
  else
  begin
    Panel1.Color := clSilver;
    Label1.Caption := 'INACTIF';
  end;
end;
```

### Liaison conditionnelle de propriétés

```pascal
// TBindExpression pour changer la couleur
BindExpression1.SourceExpression := 'if(actif, $00FF00, $C0C0C0)';
BindExpression1.ControlExpression := 'Color';
```

## Live Bindings avec FireMonkey (FMX)

Les Live Bindings brillent particulièrement avec **FireMonkey** pour les applications multi-plateformes.

### Exemple : ListView FMX

```pascal
// Configuration identique à VCL
BindSourceDB1.DataSet := FDQueryClients;

// Lier au ListView FireMonkey
// Le reste fonctionne exactement pareil !
```

**Avantage :** Le même code fonctionne sur Windows, macOS, iOS, Android !

## Débogage des Live Bindings

### Afficher les liaisons actives

```pascal
// Lister toutes les liaisons
procedure AfficherLiaisons;
var
  i: Integer;
  Binding: TContainedBindComponent;
begin
  for i := 0 to BindingsList1.ComponentCount - 1 do
  begin
    if BindingsList1.Components[i] is TContainedBindComponent then
    begin
      Binding := TContainedBindComponent(BindingsList1.Components[i]);
      ShowMessage(Binding.Name + ': ' +
                  BoolToStr(Binding.Active, True));
    end;
  end;
end;
```

### Activer/désactiver une liaison

```pascal
// Trouver et désactiver une liaison
LinkControlToField1.Active := False;

// Réactiver
LinkControlToField1.Active := True;
```

## Performances et optimisation

### Désactiver temporairement les liaisons

```pascal
// Pendant un traitement massif
BindingsList1.Notify(Self, 'BeginUpdate');
try
  // Modifications multiples
  FDQueryClients.First;
  while not FDQueryClients.Eof do
  begin
    // Traiter...
    FDQueryClients.Next;
  end;
finally
  BindingsList1.Notify(Self, 'EndUpdate');
end;
```

### Liaisons unidirectionnelles

```pascal
// Si pas besoin de mise à jour bidirectionnelle
LinkControlToField1.Direction := dirSourceToControl;  // Unidirectionnel

// Au lieu de
LinkControlToField1.Direction := dirBidirectional;  // Bidirectionnel (défaut)
```

## Comparaison : Data-Aware vs Live Bindings

| Aspect | Data-Aware Controls | Live Bindings |
|--------|-------------------|---------------|
| **Complexité** | Simple | Modérée |
| **Flexibilité** | Limitée | Très flexible |
| **Contrôles supportés** | Composants DB uniquement | Tous les contrôles |
| **Objets métier** | Non (nécessite DataSet) | Oui (direct) |
| **Multi-plateformes** | VCL uniquement | VCL + FMX |
| **Expressions** | Limitées | Puissantes |
| **Courbe d'apprentissage** | Douce | Moyenne |
| **Performance** | Excellente | Bonne |
| **Maturité** | Très mature | Mature |

## Architecture MVVM avec Live Bindings

Les Live Bindings facilitent l'architecture **MVVM** (Model-View-ViewModel).

### Structure MVVM

```
┌─────────────┐       ┌─────────────┐       ┌─────────────┐
│   Model     │──────►│  ViewModel  │──────►│    View     │
│  (Objets)   │       │  (Adapter)  │       │ (Formulaire)│
└─────────────┘       └─────────────┘       └─────────────┘
                             │
                             │ Live Bindings
                             ▼
                      Synchronisation
                       automatique
```

### Exemple simplifié

```pascal
// Model
type
  TClientModel = class
  private
    FNom: string;
    FPrenom: string;
  published
    property Nom: string read FNom write FNom;
    property Prenom: string read FPrenom write FPrenom;
  end;

// ViewModel
type
  TClientViewModel = class
  private
    FClient: TClientModel;
    FAdapter: TAdapterBindSource;
  public
    constructor Create(AOwner: TComponent);
    property Client: TClientModel read FClient;
    property Adapter: TAdapterBindSource read FAdapter;
  end;

// View (Formulaire)
procedure TFormClient.FormCreate(Sender: TObject);
begin
  FViewModel := TClientViewModel.Create(Self);
  AdapterBindSource1 := FViewModel.Adapter;
  // Les liaisons dans l'éditeur visuel font le reste !
end;
```

## Bonnes pratiques

### ✅ À FAIRE

1. **Utiliser l'éditeur visuel** pour créer les liaisons
   ```
   Plus rapide et moins d'erreurs que le code
   ```

2. **Nommer les liaisons de manière explicite**
   ```pascal
   LinkNomToEdit.Name := 'LinkNomToEdit';
   ```

3. **Grouper les liaisons logiquement**
   ```pascal
   // Un BindingsList par formulaire ou par section
   ```

4. **Utiliser les expressions** pour le formatage
   ```pascal
   Expression: FormatDateTime('dd/mm/yyyy', date)
   ```

5. **Tester sur toutes les plateformes cibles**
   ```
   Surtout si vous utilisez FMX
   ```

### ❌ À ÉVITER

1. **Mélanger Data-Aware et Live Bindings sans raison**
   ```pascal
   // ❌ Incohérent
   DBEdit1 (Data-Aware) + Edit2 avec LiveBinding

   // ✅ Cohérent : tout en Live Bindings OU tout en Data-Aware
   ```

2. **Liaisons trop complexes**
   ```pascal
   // ❌ Expression illisible
   Expression: if(x > 10, if(y < 5, 'A', 'B'), if(z = 3, 'C', 'D'))

   // ✅ Simplifier avec une méthode
   Expression: CalculerCategorie(x, y, z)
   ```

3. **Oublier d'activer les liaisons**
   ```pascal
   LinkControlToField1.Active := True;  // Important !
   ```

4. **Performances : trop de liaisons actives**
   ```
   ❌ 100+ liaisons sur un formulaire = lenteur
   ✅ Désactiver les liaisons non visibles
   ```

## Exemples complets

### Exemple 1 : Formulaire simple avec Live Bindings

```pascal
unit uFormClientLiveBindings;

interface

uses
  System.SysUtils, System.Classes, Vcl.Forms, Vcl.Controls,
  Vcl.StdCtrls, Vcl.ExtCtrls,
  FireDAC.Comp.Client, Data.Bind.Components, Data.Bind.Controls,
  Data.Bind.EngExt, Data.Bind.DBScope;

type
  TFormClientLiveBindings = class(TForm)
    // Composants FireDAC
    FDConnection1: TFDConnection;
    FDQueryClients: TFDQuery;

    // Live Bindings
    BindSourceDB1: TBindSourceDB;
    BindingsList1: TBindingsList;
    BindNavigator1: TBindNavigator;

    // Contrôles (standards, pas DB !)
    EditNom: TEdit;
    EditPrenom: TEdit;
    EditEmail: TEdit;
    CheckBoxActif: TCheckBox;
    LabelNomComplet: TLabel;

    procedure FormCreate(Sender: TObject);
  end;

implementation

procedure TFormClientLiveBindings.FormCreate(Sender: TObject);
begin
  // Connexion et chargement
  FDConnection1.Connected := True;
  FDQueryClients.SQL.Text := 'SELECT * FROM clients';
  FDQueryClients.Open;

  // Configurer le BindSource
  BindSourceDB1.DataSet := FDQueryClients;

  // Les liaisons sont créées dans l'éditeur visuel :
  // EditNom → nom
  // EditPrenom → prenom
  // EditEmail → email
  // CheckBoxActif → actif
  // LabelNomComplet → Expression: nom + ' ' + prenom

  // Le BindNavigator est déjà lié au BindSourceDB1
end;

end.
```

### Exemple 2 : ListView avec objets

```pascal
procedure TForm1.ChargerClientsEnObjets;
var
  Client: TClient;
  Liste: TObjectList<TClient>;
begin
  Liste := TObjectList<TClient>.Create(True);

  // Charger depuis la base
  FDQueryClients.First;
  while not FDQueryClients.Eof do
  begin
    Client := TClient.Create;
    Client.Nom := FDQueryClients.FieldByName('nom').AsString;
    Client.Prenom := FDQueryClients.FieldByName('prenom').AsString;
    Client.Email := FDQueryClients.FieldByName('email').AsString;
    Liste.Add(Client);
    FDQueryClients.Next;
  end;

  // Lier au BindSource
  AdapterBindSource1.Adapter := TObjectBindSourceAdapter.Create(
    Self, Liste, True);
  AdapterBindSource1.Active := True;

  // Le ListView affiche automatiquement la liste !
end;
```

## Résumé

### Points clés

✅ Les **Live Bindings** offrent une alternative moderne aux contrôles DB
✅ Ils fonctionnent avec **n'importe quel** contrôle visuel
✅ Liaison possible avec **DataSets** ET **objets** directement
✅ **Éditeur visuel** puissant pour créer les liaisons
✅ **Expressions** pour formatage et calculs avancés
✅ Idéal pour **FireMonkey** (multi-plateformes)
✅ Facilite l'architecture **MVVM**

### Quand choisir quoi ?

**Choisissez Data-Aware Controls si :**
- Application VCL Windows pure
- Équipe habituée à cette approche
- Besoin de simplicité maximale
- Maintenance de code existant

**Choisissez Live Bindings si :**
- Application multi-plateformes (FMX)
- Architecture MVVM ou similaire
- Liaison avec des objets métier
- Besoin de flexibilité et d'expressions
- Nouveau projet moderne

### Comparaison visuelle

```
Data-Aware Controls:
  TFDQuery → TDataSource → DBEdit ✓ Simple, éprouvé

Live Bindings:
  TFDQuery → TBindSourceDB → Edit ✓ Flexible, moderne
  TObjet → TAdapter → Edit ✓ Pas de DataSet nécessaire
```

## Prochaines étapes

Vous maîtrisez maintenant les Live Bindings ! Dans les sections suivantes, nous verrons :

1. **Modèle en couches** : Organiser votre code en architecture professionnelle
2. **Migration de bases** : Gérer l'évolution de vos structures
3. **Optimisation** : Améliorer les performances

Avec les Live Bindings, vous pouvez créer des applications modernes, flexibles et maintenables qui fonctionnent sur toutes les plateformes supportées par Delphi !

⏭️ [Modèle en couches pour l'accès aux données](/08-acces-aux-bases-de-donnees-mysql-mariadb/09-modele-en-couches-pour-lacces-aux-donnees.md)
