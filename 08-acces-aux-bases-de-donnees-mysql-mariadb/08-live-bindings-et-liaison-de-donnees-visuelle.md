# 8.8 Live Bindings et liaison de données visuelle

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Dans les sections précédentes, nous avons exploré les méthodes traditionnelles de liaison de données dans Delphi, qui utilisent les contrôles "DB-aware" et les DataSources. Maintenant, découvrons une approche plus moderne et flexible : les **Live Bindings**.

## Qu'est-ce que Live Bindings ?

**Live Bindings** est une technologie introduite dans Delphi XE2 qui permet de connecter visuellement presque n'importe quel type d'objet à un autre. Contrairement aux contrôles DB-aware traditionnels qui ne peuvent se connecter qu'à des DataSources, Live Bindings peut lier :

- Des contrôles visuels à des sources de données
- Des contrôles visuels à d'autres contrôles visuels
- Des objets non visuels entre eux
- Des propriétés spécifiques d'objets

![Concept Live Bindings](https://placeholder.pics/svg/650x250/DEDEDE/555555/Concept%20Live%20Bindings)

## Avantages de Live Bindings par rapport aux contrôles DB-aware

- **Universalité** : Fonctionne avec tous types de contrôles, pas seulement les contrôles DB-aware
- **Flexibilité** : Permet de lier des propriétés spécifiques, pas seulement la valeur principale
- **Bidirectionnalité** : Peut synchroniser dans les deux sens (aller-retour)
- **Transformations** : Permet d'appliquer des transformations aux données lors du transfert
- **Multi-plateforme** : Essentiel pour FireMonkey qui n'a pas de contrôles DB-aware traditionnels
- **Visuel** : La conception des liaisons peut se faire visuellement dans l'IDE

## Quand utiliser Live Bindings ?

Live Bindings est particulièrement utile dans les cas suivants :
- Applications multi-plateformes avec FireMonkey
- Interfaces complexes nécessitant des liaisons personnalisées
- Liaison avec des sources de données non-traditionnelles (JSON, XML, services web)
- Besoin de transformations de données à la volée

## Les composants clés de Live Bindings

### 1. TBindSourceDB

Le composant `TBindSourceDB` est l'équivalent du `TDataSource` pour Live Bindings. Il fait le lien entre un DataSet (comme `TFDQuery`) et les contrôles visuels.

![TBindSourceDB](https://placeholder.pics/svg/400x150/DEDEDE/555555/TBindSourceDB)

### 2. TBindingsList

Le composant `TBindingsList` est le gestionnaire central des liaisons. Il stocke et gère toutes les liaisons de votre formulaire.

### 3. TLinkXXXToYYY

Ces composants représentent les liaisons spécifiques entre différents types d'objets :
- `TLinkControlToField` : Lie un contrôle à un champ de la base de données
- `TLinkPropertyToField` : Lie une propriété spécifique à un champ
- `TLinkControlToProperty` : Lie un contrôle à une propriété d'un autre objet

## Configuration de base avec Live Bindings

### Méthode 1 : Configuration visuelle (recommandée pour les débutants)

1. **Préparation du DataSet** :
   - Placez un composant `TFDConnection` et configurez-le pour MySQL
   - Placez un composant `TFDQuery` et définissez sa requête SQL
   - Activez le `TFDQuery` (Active = True)

2. **Ajout des composants Live Bindings** :
   - Placez un composant `TBindSourceDB` sur le formulaire
   - Définissez sa propriété `DataSet` pour pointer vers votre `TFDQuery`
   - Placez un composant `TBindingsList` sur le formulaire

3. **Création visuelle des liaisons** :
   - Sélectionnez un contrôle visuel (ex: `TEdit`)
   - Cliquez-droit et choisissez "Bind Visually..."
   - L'éditeur LiveBindings Designer s'ouvre
   - Faites glisser une ligne depuis le contrôle vers la source de données
   - Sélectionnez le champ à lier et les propriétés à synchroniser

![Live Bindings Designer](https://placeholder.pics/svg/600x350/DEDEDE/555555/Live%20Bindings%20Designer)

### Méthode 2 : Configuration par code

Voici comment créer des liaisons par code :

```delphi
procedure TForm1.SetupLiveBindings;
var
  BindSourceDB: TBindSourceDB;
  BindingsList: TBindingsList;
  LinkControlToField: TLinkControlToField;
begin
  // Créer les composants de base
  BindSourceDB := TBindSourceDB.Create(Self);
  BindSourceDB.DataSet := FDQuery1;
  BindSourceDB.Name := 'BindSourceDB1';

  BindingsList := TBindingsList.Create(Self);
  BindingsList.Name := 'BindingsList1';

  // Créer une liaison entre Edit1 et le champ 'nom'
  LinkControlToField := TLinkControlToField.Create(Self);
  LinkControlToField.Category := 'Quick Bindings';
  LinkControlToField.DataSource := BindSourceDB;
  LinkControlToField.FieldName := 'nom';
  LinkControlToField.Control := Edit1;
  LinkControlToField.Track := True;  // Mettre à jour en temps réel
  LinkControlToField.Active := True;

  // Ajouter la liaison à la liste
  BindingsList.Add(LinkControlToField);
end;
```

## Exemple pas à pas : Créer un formulaire avec Live Bindings

Suivons un exemple concret pour mieux comprendre comment utiliser Live Bindings :

### Étape 1 : Créer le formulaire et les composants de base

1. Créez un nouveau projet VCL Application
2. Placez les composants suivants sur le formulaire :
   - `TFDConnection` (nommez-le `FDConnection1`)
   - `TFDQuery` (nommez-le `FDQuery1`)
   - `TBindSourceDB` (nommez-le `BindSourceDB1`)
   - `TBindingsList` (nommez-le `BindingsList1`)
   - Des contrôles standard : `TEdit`, `TMemo`, `TCheckBox`, `TDateTimePicker`, `TComboBox`
   - Un `TButton` pour naviguer (Suivant, Précédent)
   - Un `TLabel` pour afficher la position actuelle

### Étape 2 : Configurer la connexion et la requête

```delphi
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configurer la connexion MySQL
  FDConnection1.DriverName := 'MySQL';
  FDConnection1.Params.Clear;
  FDConnection1.Params.Add('Server=localhost');
  FDConnection1.Params.Add('Database=ma_base');
  FDConnection1.Params.Add('User_Name=mon_utilisateur');
  FDConnection1.Params.Add('Password=mon_mot_de_passe');

  try
    // Configurer et ouvrir le DataSet
    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := 'SELECT * FROM clients';
    FDQuery1.Open;

    // Configurer le BindSourceDB
    BindSourceDB1.DataSet := FDQuery1;

    // Mettre à jour l'affichage
    UpdatePositionLabel;
  except
    on E: Exception do
      ShowMessage('Erreur de connexion : ' + E.Message);
  end;
end;

procedure TForm1.UpdatePositionLabel;
begin
  if FDQuery1.Active then
    LabelPosition.Caption := Format('Enregistrement %d sur %d',
      [FDQuery1.RecNo, FDQuery1.RecordCount])
  else
    LabelPosition.Caption := 'Non connecté';
end;
```

### Étape 3 : Créer les liaisons visuellement

1. Sélectionnez l'`TEdit` (nommez-le `EditNom`)
2. Cliquez-droit et choisissez "Bind Visually..."
3. Dans le LiveBindings Designer, faites glisser une ligne depuis `EditNom` vers `BindSourceDB1`
4. Dans la boîte de dialogue :
   - Choisissez le champ "nom" dans la source de données
   - Dans l'onglet "Bindings" :
     - Source Component : EditNom
     - Source Property : Text
     - Control Component : BindSourceDB1
     - Control Property : nom
     - Direction : Bidirectional
   - Activez "Track changes" pour une mise à jour en temps réel
   - Cliquez sur "OK"

5. Répétez cette procédure pour les autres contrôles :
   - Liez `Memo1` au champ "adresse"
   - Liez `CheckBox1` au champ "actif"
   - Liez `DateTimePicker1` au champ "date_naissance"
   - Liez `ComboBox1` au champ "categorie"

### Étape 4 : Ajouter la navigation

```delphi
procedure TForm1.ButtonPrecedentClick(Sender: TObject);
begin
  if not FDQuery1.BOF then
  begin
    FDQuery1.Prior;
    UpdatePositionLabel;
  end;
end;

procedure TForm1.ButtonSuivantClick(Sender: TObject);
begin
  if not FDQuery1.EOF then
  begin
    FDQuery1.Next;
    UpdatePositionLabel;
  end;
end;
```

### Étape 5 : Ajouter les validations et les mises à jour

```delphi
procedure TForm1.ButtonEnregistrerClick(Sender: TObject);
begin
  // Appliquer les modifications en cours
  BindingsList1.ApplyUpdates;

  // Enregistrer dans la base de données
  if FDQuery1.State in [dsEdit, dsInsert] then
    FDQuery1.Post;

  ShowMessage('Modifications enregistrées !');
end;
```

## Fonctionnalités avancées de Live Bindings

### 1. Expressions et transformations

Live Bindings permet d'appliquer des transformations aux données grâce à des expressions :

```delphi
// Dans l'éditeur de liaison ou par code :
LinkControlToField1.SourceExpression := 'UpperCase(%s)';  // Convertit en majuscules
```

### 2. Formatage personnalisé

Vous pouvez formater les données affichées :

```delphi
// Dans l'éditeur de liaison ou par code :
LinkControlToField1.FormatLink.FormatExpression := '%f €';  // Pour un montant
```

### 3. Gestion des événements de mise à jour

```delphi
procedure TForm1.LinkControlToField1AssigningValue(Sender: TObject;
  AssignValueRec: TBindingAssignValueRec; var Value: TValue; var Handled: Boolean);
begin
  // Code exécuté avant l'assignation de la valeur
  if Value.AsString = '' then
  begin
    ShowMessage('Le champ ne peut pas être vide !');
    Value := 'Valeur requise';
    Handled := True;  // Indiquer que nous avons traité la valeur
  end;
end;
```

### 4. Liaison de contrôles à contrôles

Vous pouvez lier des contrôles entre eux, sans passer par une base de données :

1. Sélectionnez le premier contrôle (ex: `TrackBar1`)
2. Cliquez-droit et choisissez "Bind Visually..."
3. Faites glisser une ligne vers le deuxième contrôle (ex: `ProgressBar1`)
4. Configurez la liaison (ex: Position → Position)

```delphi
// Par code :
LinkControlToProperty := TLinkControlToProperty.Create(Self);
LinkControlToProperty.Control := TrackBar1;
LinkControlToProperty.Component := ProgressBar1;
LinkControlToProperty.SourceProperty := 'Position';
LinkControlToProperty.DestProperty := 'Position';
LinkControlToProperty.Active := True;
BindingsList1.Add(LinkControlToProperty);
```

### 5. Liaison à des objets personnalisés

Live Bindings peut également se lier à des objets qui ne sont pas des DataSets :

```delphi
type
  TMonObjet = class
  private
    FNom: string;
    FAge: Integer;
    procedure SetNom(const Value: string);
    procedure SetAge(const Value: Integer);
  published  // Les propriétés doivent être published
    property Nom: string read FNom write SetNom;
    property Age: Integer read FAge write SetAge;
  end;

// Dans le formulaire :
var
  MonObjet: TMonObjet;
  BindSource: TBindSource;  // Au lieu de TBindSourceDB

// Dans FormCreate :
MonObjet := TMonObjet.Create;
MonObjet.Nom := 'Jean Dupont';
MonObjet.Age := 30;

BindSource := TBindSource.Create(Self);
BindSource.DataObject := MonObjet;

// Ensuite, créez les liaisons comme d'habitude
```

## Comparaison : Contrôles DB-aware vs Live Bindings

| Aspect | Contrôles DB-aware | Live Bindings |
|--------|-------------------|--------------|
| Facilité d'utilisation | Très simple, drag & drop | Plus complexe, nécessite configuration |
| Flexibilité | Limitée, uniquement avec DataSources | Très flexible, tout type d'objet |
| Performance | Très rapide | Un peu plus lent (surcharge due à la réflexion) |
| Multi-plateforme | VCL uniquement | VCL et FireMonkey |
| Personnalisation | Limitée | Très personnalisable (expressions, formats) |
| Interface visuelle | Directe | Nécessite LiveBindings Designer |
| Courbe d'apprentissage | Faible | Moyenne |

## Bonnes pratiques avec Live Bindings

1. **Nommer clairement les composants** : Les noms par défaut comme "LinkControlToField1" ne sont pas parlants.

2. **Utiliser l'éditeur visuel** : C'est plus simple et moins sujet aux erreurs pour les débutants.

3. **Tester les liaisons** : Vérifiez régulièrement que les liaisons fonctionnent dans les deux sens (si bidirectionnelles).

4. **Attention aux performances** : Les liaisons multiples peuvent affecter les performances sur des formulaires complexes.

5. **Gérer les erreurs** : Utilisez les événements comme `OnAssigningValue` pour valider les données et gérer les erreurs.

6. **Organisation** : Gardez vos liaisons organisées dans le LiveBindings Designer pour faciliter la maintenance.

7. **Documentation** : Documenter les transformations et expressions complexes pour la maintenance future.

## Limitations et pièges courants

- **Performance** : Live Bindings peut être plus lent que les contrôles DB-aware traditionnels.

- **Débogage** : Les erreurs dans les expressions peuvent être difficiles à déboguer.

- **Compatibilité** : Certaines fonctionnalités avancées ne sont pas disponibles dans toutes les versions de Delphi.

- **Liaison circulaire** : Évitez de créer des liaisons circulaires qui se mettent à jour mutuellement.

## Exemple complet : Formulaire de consultation de produits

```delphi
unit FormProduits;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Data.Bind.Components, Data.Bind.DBScope, System.Rtti, System.Bindings.Outputs,
  Vcl.Bind.Editors, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.Bind.Navigator;

type
  TFormConsultationProduits = class(TForm)
    FDConnection1: TFDConnection;
    FDQueryProduits: TFDQuery;
    BindSourceDBProduits: TBindSourceDB;
    BindingsList1: TBindingsList;
    PanelHaut: TPanel;
    PanelNavigation: TPanel;
    PanelContenu: TPanel;
    EditReference: TEdit;
    EditNom: TEdit;
    EditPrix: TEdit;
    MemoDescription: TMemo;
    LabelReference: TLabel;
    LabelNom: TLabel;
    LabelPrix: TLabel;
    LabelDescription: TLabel;
    LabelStock: TLabel;
    TrackBarStock: TTrackBar;
    ButtonPremier: TButton;
    ButtonPrecedent: TButton;
    ButtonSuivant: TButton;
    ButtonDernier: TButton;
    BindNavigator1: TBindNavigator;
    EditRecherche: TEdit;
    ButtonRechercher: TButton;
    LabelPosition: TLabel;
    ComboBoxCategorie: TComboBox;
    LabelCategorie: TLabel;
    CheckBoxActif: TCheckBox;
    ImageProduit: TImage;
    ProgressBarStock: TProgressBar;
    FDQueryCategories: TFDQuery;
    BindSourceDBCategories: TBindSourceDB;
    StatusBar1: TStatusBar;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkControlToField6: TLinkControlToField;
    LinkControlToProperty1: TLinkControlToProperty;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonPremierClick(Sender: TObject);
    procedure ButtonPrecedentClick(Sender: TObject);
    procedure ButtonSuivantClick(Sender: TObject);
    procedure ButtonDernierClick(Sender: TObject);
    procedure ButtonRechercherClick(Sender: TObject);
    procedure FDQueryProduitsAfterScroll(DataSet: TDataSet);
    procedure TrackBarStockChange(Sender: TObject);
    procedure LinkControlToField3AssigningValue(Sender: TObject;
      AssignValueRec: TBindingAssignValueRec; var Value: TValue; var Handled: Boolean);
  private
    procedure ConnecterBaseDeDonnees;
    procedure UpdatePositionLabel;
  public
    { Déclarations publiques }
  end;

var
  FormConsultationProduits: TFormConsultationProduits;

implementation

{$R *.dfm}

procedure TFormConsultationProduits.FormCreate(Sender: TObject);
begin
  ConnecterBaseDeDonnees;

  // Configurer les contrôles
  TrackBarStock.Min := 0;
  TrackBarStock.Max := 100;
  ProgressBarStock.Min := 0;
  ProgressBarStock.Max := 100;

  // Remplir ComboBox des catégories
  ComboBoxCategorie.Items.Clear;
  FDQueryCategories.First;
  while not FDQueryCategories.EOF do
  begin
    ComboBoxCategorie.Items.Add(FDQueryCategories.FieldByName('nom').AsString);
    FDQueryCategories.Next;
  end;

  // Mettre à jour l'affichage
  UpdatePositionLabel;
end;

procedure TFormConsultationProduits.ConnecterBaseDeDonnees;
begin
  try
    // Configurer et ouvrir la connexion
    FDConnection1.Connected := True;

    // Ouvrir les DataSets
    FDQueryCategories.Open;
    FDQueryProduits.Open;

    // Configurer les BindSourceDB
    BindSourceDBCategories.DataSet := FDQueryCategories;
    BindSourceDBProduits.DataSet := FDQueryProduits;

    StatusBar1.SimpleText := 'Connecté à la base de données';
  except
    on E: Exception do
    begin
      ShowMessage('Erreur de connexion : ' + E.Message);
      StatusBar1.SimpleText := 'Non connecté';
    end;
  end;
end;

procedure TFormConsultationProduits.FormDestroy(Sender: TObject);
begin
  // Fermer proprement
  if FDQueryProduits.Active then
    FDQueryProduits.Close;

  if FDQueryCategories.Active then
    FDQueryCategories.Close;

  if FDConnection1.Connected then
    FDConnection1.Connected := False;
end;

procedure TFormConsultationProduits.ButtonPremierClick(Sender: TObject);
begin
  FDQueryProduits.First;
  UpdatePositionLabel;
end;

procedure TFormConsultationProduits.ButtonPrecedentClick(Sender: TObject);
begin
  if not FDQueryProduits.BOF then
  begin
    FDQueryProduits.Prior;
    UpdatePositionLabel;
  end;
end;

procedure TFormConsultationProduits.ButtonSuivantClick(Sender: TObject);
begin
  if not FDQueryProduits.EOF then
  begin
    FDQueryProduits.Next;
    UpdatePositionLabel;
  end;
end;

procedure TFormConsultationProduits.ButtonDernierClick(Sender: TObject);
begin
  FDQueryProduits.Last;
  UpdatePositionLabel;
end;

procedure TFormConsultationProduits.ButtonRechercherClick(Sender: TObject);
var
  SearchTerm: string;
begin
  SearchTerm := Trim(EditRecherche.Text);

  if SearchTerm = '' then
  begin
    FDQueryProduits.Filtered := False;
    FDQueryProduits.Filter := '';
  end
  else
  begin
    FDQueryProduits.Filtered := False;
    FDQueryProduits.Filter := Format('(reference LIKE ''%%%s%%'') OR (nom LIKE ''%%%s%%'')',
                             [SearchTerm, SearchTerm]);
    FDQueryProduits.Filtered := True;
  end;

  UpdatePositionLabel;
end;

procedure TFormConsultationProduits.FDQueryProduitsAfterScroll(DataSet: TDataSet);
begin
  UpdatePositionLabel;
end;

procedure TFormConsultationProduits.TrackBarStockChange(Sender: TObject);
begin
  // Cette mise à jour est automatique grâce à TLinkControlToProperty
  // Nous n'avons pas besoin de code supplémentaire ici
end;

procedure TFormConsultationProduits.LinkControlToField3AssigningValue(
  Sender: TObject; AssignValueRec: TBindingAssignValueRec; var Value: TValue;
  var Handled: Boolean);
var
  PrixStr: string;
  Prix: Double;
begin
  // Validation du prix avant assignation
  PrixStr := Value.AsString;

  if TryStrToFloat(PrixStr, Prix) then
  begin
    if Prix < 0 then
    begin
      ShowMessage('Le prix ne peut pas être négatif !');
      Value := 0;
      Handled := True;
    end;
  end
  else if PrixStr <> '' then
  begin
    ShowMessage('Format de prix invalide !');
    Value := 0;
    Handled := True;
  end;
end;

procedure TFormConsultationProduits.UpdatePositionLabel;
begin
  if FDQueryProduits.Active then
  begin
    if FDQueryProduits.Filtered then
      LabelPosition.Caption := Format('Produit %d sur %d (filtrés)',
        [FDQueryProduits.RecNo, FDQueryProduits.RecordCount])
    else
      LabelPosition.Caption := Format('Produit %d sur %d',
        [FDQueryProduits.RecNo, FDQueryProduits.RecordCount]);
  end
  else
    LabelPosition.Caption := 'Non connecté';
end;

end.
```

## Conclusion

Live Bindings offre une approche moderne et flexible pour la liaison de données dans Delphi. Bien que légèrement plus complexe que les contrôles DB-aware traditionnels, cette technologie apporte une flexibilité et des possibilités de personnalisation qui peuvent être précieuses, particulièrement pour :

- Le développement multi-plateforme avec FireMonkey
- Les interfaces utilisateur complexes
- Les liaisons avec des sources de données non traditionnelles
- L'application de transformations aux données

Pour les débutants, nous recommandons :
1. Commencer par explorer l'éditeur visuel (LiveBindings Designer)
2. Expérimenter avec des liaisons simples contrôle-à-champ
3. Progressivement découvrir les fonctionnalités plus avancées

À mesure que vous vous familiariserez avec Live Bindings, vous découvrirez que cette technologie peut considérablement simplifier votre code et rendre vos interfaces plus réactives et plus robustes.

---

**À suivre :** 8.9 Modèle en couches pour l'accès aux données

⏭️ [Modèle en couches pour l'accès aux données](08-acces-aux-bases-de-donnees-mysql-mariadb/09-modele-en-couches-pour-lacces-aux-donnees.md)
