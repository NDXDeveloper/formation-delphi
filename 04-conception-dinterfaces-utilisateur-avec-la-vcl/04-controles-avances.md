🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.4 Contrôles avancés (PageControl, TreeView, ListView)

## Introduction

Dans ce chapitre, nous allons découvrir trois contrôles VCL essentiels qui vous permettront de créer des interfaces utilisateur professionnelles et structurées. Ces contrôles sont largement utilisés dans les applications Windows modernes et vous les avez probablement déjà rencontrés dans de nombreux logiciels.

## 4.4.1 Le PageControl (Contrôle à onglets)

### Qu'est-ce qu'un PageControl ?

Le **PageControl** est un contrôle qui permet d'organiser votre interface en plusieurs onglets (tabs), comme dans un navigateur web ou un éditeur de texte. Chaque onglet peut contenir différents composants et représente une "page" distincte de votre application.

### Quand utiliser un PageControl ?

- Lorsque vous avez beaucoup d'informations à afficher et que vous souhaitez les organiser par catégories
- Pour créer des boîtes de dialogue de paramètres ou de configuration
- Pour séparer différentes vues d'une même application
- Pour économiser de l'espace à l'écran en regroupant les contrôles par thème

### Mise en place d'un PageControl

#### Étape 1 : Ajouter le composant

1. Dans la palette d'outils, recherchez **PageControl** (catégorie "Win32")
2. Glissez-déposez le composant sur votre formulaire
3. Redimensionnez-le selon vos besoins

#### Étape 2 : Ajouter des onglets (TabSheets)

Un PageControl sans onglets est inutile. Voici comment en ajouter :

**Méthode visuelle :**
- Faites un clic droit sur le PageControl
- Sélectionnez "Nouvelle page" (ou "New Page")
- Répétez l'opération pour chaque onglet souhaité

**Par code :**
```pascal
var
  NewTab: TTabSheet;
begin
  NewTab := TTabSheet.Create(PageControl1);
  NewTab.PageControl := PageControl1;
  NewTab.Caption := 'Mon nouvel onglet';
end;
```

#### Étape 3 : Personnaliser les onglets

Pour chaque onglet (TabSheet), vous pouvez modifier :

- **Caption** : Le texte affiché sur l'onglet
- **ImageIndex** : L'index d'une image (si vous utilisez un ImageList)
- **TabVisible** : Afficher ou masquer l'onglet

### Propriétés importantes du PageControl

| Propriété | Description |
|-----------|-------------|
| **ActivePage** | L'onglet actuellement sélectionné |
| **ActivePageIndex** | L'index (numéro) de l'onglet actif |
| **TabPosition** | Position des onglets (haut, bas, gauche, droite) |
| **MultiLine** | Permet d'afficher les onglets sur plusieurs lignes |
| **Style** | Style d'affichage des onglets |
| **HotTrack** | Active l'effet de survol sur les onglets |

### Exemple de code pratique

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Configurer le PageControl
  PageControl1.TabPosition := tpTop;
  PageControl1.MultiLine := False;

  // Créer 3 onglets dynamiquement
  for i := 1 to 3 do
  begin
    with TTabSheet.Create(PageControl1) do
    begin
      PageControl := PageControl1;
      Caption := 'Onglet ' + IntToStr(i);
    end;
  end;

  // Sélectionner le premier onglet
  PageControl1.ActivePageIndex := 0;
end;

// Événement de changement d'onglet
procedure TForm1.PageControl1Change(Sender: TObject);
begin
  ShowMessage('Vous êtes maintenant sur : ' +
              PageControl1.ActivePage.Caption);
end;
```

### Conseils pratiques

- Limitez le nombre d'onglets pour ne pas surcharger l'interface (idéalement 5-7 maximum)
- Utilisez des noms d'onglets clairs et concis
- Placez les onglets les plus utilisés en premier
- Vous pouvez désactiver un onglet en mettant `TabSheet.Enabled := False`

---

## 4.4.2 Le TreeView (Arborescence)

### Qu'est-ce qu'un TreeView ?

Le **TreeView** est un contrôle qui affiche des données sous forme d'arborescence hiérarchique, comme l'Explorateur de fichiers Windows. Il permet de représenter des relations parent-enfant entre les éléments.

### Quand utiliser un TreeView ?

- Pour afficher une structure de répertoires ou de fichiers
- Pour créer un menu de navigation hiérarchique
- Pour représenter une structure organisationnelle
- Pour afficher des données avec plusieurs niveaux de catégories

### Structure du TreeView

Un TreeView est composé de **nœuds** (nodes) :
- **Nœud racine** : Le niveau le plus haut
- **Nœud parent** : Un nœud qui contient d'autres nœuds
- **Nœud enfant** : Un nœud contenu dans un autre nœud
- **Nœud feuille** : Un nœud sans enfants

### Mise en place d'un TreeView

#### Ajouter le composant

1. Recherchez **TreeView** dans la palette d'outils (catégorie "Win32")
2. Placez-le sur votre formulaire
3. Redimensionnez-le selon vos besoins

#### Propriétés importantes

| Propriété | Description |
|-----------|-------------|
| **Items** | Collection de tous les nœuds |
| **Selected** | Le nœud actuellement sélectionné |
| **ShowLines** | Affiche les lignes de connexion |
| **ShowRoot** | Affiche les lignes au niveau racine |
| **ShowButtons** | Affiche les boutons +/- pour développer/réduire |
| **ReadOnly** | Empêche l'édition des nœuds |
| **HideSelection** | Masque la sélection quand le contrôle perd le focus |
| **MultiSelect** | Permet la sélection multiple |

### Ajouter des nœuds

#### Méthode 1 : En mode conception

1. Double-cliquez sur le TreeView (ou clic droit → "Items Editor")
2. Cliquez sur "Nouvel élément" pour ajouter un nœud racine
3. Cliquez sur "Nouveau sous-élément" pour ajouter un enfant
4. Modifiez la propriété **Text** de chaque nœud

#### Méthode 2 : Par code

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  RootNode, ChildNode, SubChildNode: TTreeNode;
begin
  TreeView1.Items.Clear; // Vider l'arborescence

  // Ajouter un nœud racine
  RootNode := TreeView1.Items.Add(nil, 'Racine');

  // Ajouter des nœuds enfants
  ChildNode := TreeView1.Items.AddChild(RootNode, 'Enfant 1');
  TreeView1.Items.AddChild(RootNode, 'Enfant 2');

  // Ajouter un sous-enfant
  SubChildNode := TreeView1.Items.AddChild(ChildNode, 'Sous-enfant 1.1');

  // Développer tous les nœuds
  TreeView1.FullExpand;
end;
```

### Exemple complet : Arborescence de pays et villes

```pascal
procedure TForm1.CreerArborescenceVilles;
var
  France, Paris, Lyon: TTreeNode;
  Espagne, Madrid, Barcelone: TTreeNode;
begin
  TreeView1.Items.Clear;

  // France et ses villes
  France := TreeView1.Items.Add(nil, 'France');
  Paris := TreeView1.Items.AddChild(France, 'Paris');
  Lyon := TreeView1.Items.AddChild(France, 'Lyon');

  // Espagne et ses villes
  Espagne := TreeView1.Items.Add(nil, 'Espagne');
  Madrid := TreeView1.Items.AddChild(Espagne, 'Madrid');
  Barcelone := TreeView1.Items.AddChild(Espagne, 'Barcelone');

  TreeView1.FullExpand;
end;

// Gérer la sélection d'un nœud
procedure TForm1.TreeView1Click(Sender: TObject);
var
  SelectedNode: TTreeNode;
begin
  SelectedNode := TreeView1.Selected;
  if Assigned(SelectedNode) then
  begin
    ShowMessage('Vous avez sélectionné : ' + SelectedNode.Text);
  end;
end;
```

### Manipuler les nœuds

```pascal
// Développer un nœud
Node.Expand(False); // False = développer seulement ce nœud
Node.Expand(True);  // True = développer aussi les enfants

// Réduire un nœud
Node.Collapse(True);

// Supprimer un nœud
Node.Delete;

// Vérifier si un nœud a des enfants
if Node.HasChildren then
  ShowMessage('Ce nœud a des enfants');

// Parcourir les enfants d'un nœud
ChildNode := Node.GetFirstChild;
while Assigned(ChildNode) do
begin
  // Faire quelque chose avec ChildNode
  ShowMessage(ChildNode.Text);
  ChildNode := Node.GetNextChild(ChildNode);
end;

// Obtenir le parent d'un nœud
ParentNode := Node.Parent;
```

### Utiliser des images avec ImageList

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  FolderNode, FileNode: TTreeNode;
begin
  // Associer un ImageList au TreeView
  TreeView1.Images := ImageList1;

  // Ajouter des nœuds avec des images
  FolderNode := TreeView1.Items.Add(nil, 'Dossier');
  FolderNode.ImageIndex := 0; // Image fermée
  FolderNode.SelectedIndex := 1; // Image ouverte

  FileNode := TreeView1.Items.AddChild(FolderNode, 'Fichier.txt');
  FileNode.ImageIndex := 2;
  FileNode.SelectedIndex := 2;
end;
```

### Conseils pratiques

- Utilisez `BeginUpdate` et `EndUpdate` lors de l'ajout de nombreux nœuds pour améliorer les performances
- Stockez des données supplémentaires dans la propriété `Data` du nœud
- Implémentez la recherche dans l'arborescence pour les grandes structures
- Permettez le glisser-déposer pour réorganiser les nœuds si nécessaire

---

## 4.4.3 Le ListView (Liste détaillée)

### Qu'est-ce qu'un ListView ?

Le **ListView** est un contrôle polyvalent qui peut afficher des données sous plusieurs formes : liste simple, liste détaillée avec colonnes, grandes icônes, petites icônes, etc. C'est le contrôle utilisé dans l'Explorateur Windows pour afficher les fichiers.

### Quand utiliser un ListView ?

- Pour afficher des listes de données avec plusieurs colonnes
- Pour créer des catalogues de produits avec images
- Pour afficher des résultats de recherche
- Pour présenter des données tabulaires avec plus de flexibilité qu'une grille

### Les différents modes d'affichage

Le ListView propose plusieurs modes via la propriété **ViewStyle** :

| Mode | Description |
|------|-------------|
| **vsIcon** | Grandes icônes avec texte en dessous |
| **vsSmallIcon** | Petites icônes avec texte à côté |
| **vsList** | Liste simple sur une colonne |
| **vsReport** | Vue détaillée avec colonnes (la plus utilisée) |

### Mise en place d'un ListView

#### Étape 1 : Ajouter le composant

1. Recherchez **ListView** dans la palette d'outils (catégorie "Win32")
2. Placez-le sur votre formulaire
3. Redimensionnez-le

#### Étape 2 : Configurer les colonnes (mode Report)

**Méthode visuelle :**
1. Cliquez droit sur le ListView → "Columns Editor"
2. Cliquez sur "Ajouter" pour chaque colonne
3. Définissez la propriété **Caption** de chaque colonne
4. Ajustez la propriété **Width** pour la largeur

**Par code :**
```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  ListView1.ViewStyle := vsReport;
  ListView1.Columns.Clear;

  // Ajouter des colonnes
  with ListView1.Columns.Add do
  begin
    Caption := 'Nom';
    Width := 150;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'Prénom';
    Width := 150;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'Âge';
    Width := 50;
  end;
end;
```

### Propriétés importantes

| Propriété | Description |
|-----------|-------------|
| **ViewStyle** | Mode d'affichage |
| **Columns** | Collection des colonnes |
| **Items** | Collection des éléments |
| **Selected** | L'élément sélectionné |
| **MultiSelect** | Permet la sélection multiple |
| **GridLines** | Affiche des lignes de grille |
| **RowSelect** | Sélectionne toute la ligne |
| **ReadOnly** | Empêche l'édition |
| **SortType** | Type de tri automatique |
| **LargeImages** | ImageList pour grandes icônes |
| **SmallImages** | ImageList pour petites icônes |

### Ajouter des éléments

```pascal
procedure TForm1.AjouterPersonne(Nom, Prenom: string; Age: Integer);
var
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Caption := Nom; // Première colonne
  Item.SubItems.Add(Prenom); // Deuxième colonne
  Item.SubItems.Add(IntToStr(Age)); // Troisième colonne
end;

// Exemple d'utilisation
procedure TForm1.Button1Click(Sender: TObject);
begin
  ListView1.Items.Clear;

  AjouterPersonne('Dupont', 'Jean', 35);
  AjouterPersonne('Martin', 'Sophie', 28);
  AjouterPersonne('Bernard', 'Paul', 42);
end;
```

### Exemple complet avec gestion

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du ListView
  ListView1.ViewStyle := vsReport;
  ListView1.GridLines := True;
  ListView1.RowSelect := True;
  ListView1.ReadOnly := True;

  // Créer les colonnes
  ListView1.Columns.Clear;
  with ListView1.Columns.Add do
  begin
    Caption := 'Produit';
    Width := 200;
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Prix';
    Width := 80;
    Alignment := taRightJustify; // Aligner à droite
  end;
  with ListView1.Columns.Add do
  begin
    Caption := 'Stock';
    Width := 80;
    Alignment := taCenter; // Centrer
  end;

  // Remplir avec des données
  RemplirListeProduits;
end;

procedure TForm1.RemplirListeProduits;
var
  Item: TListItem;
begin
  ListView1.Items.BeginUpdate; // Optimisation
  try
    ListView1.Items.Clear;

    // Produit 1
    Item := ListView1.Items.Add;
    Item.Caption := 'Ordinateur portable';
    Item.SubItems.Add('899.00 €');
    Item.SubItems.Add('15');

    // Produit 2
    Item := ListView1.Items.Add;
    Item.Caption := 'Souris sans fil';
    Item.SubItems.Add('24.99 €');
    Item.SubItems.Add('47');

    // Produit 3
    Item := ListView1.Items.Add;
    Item.Caption := 'Clavier mécanique';
    Item.SubItems.Add('89.90 €');
    Item.SubItems.Add('23');
  finally
    ListView1.Items.EndUpdate; // Rafraîchir l'affichage
  end;
end;
```

### Gérer les événements

```pascal
// Événement de sélection
procedure TForm1.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    Label1.Caption := 'Sélectionné : ' + Item.Caption;
  end;
end;

// Double-clic sur un élément
procedure TForm1.ListView1DblClick(Sender: TObject);
var
  Item: TListItem;
begin
  Item := ListView1.Selected;
  if Assigned(Item) then
  begin
    ShowMessage('Produit : ' + Item.Caption + #13#10 +
                'Prix : ' + Item.SubItems[0] + #13#10 +
                'Stock : ' + Item.SubItems[1]);
  end;
end;

// Clic sur l'en-tête d'une colonne (pour le tri)
procedure TForm1.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  // Implémenter le tri par colonne
  ShowMessage('Tri par : ' + Column.Caption);
end;
```

### Utiliser des images

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  Item: TListItem;
begin
  // Associer les ImageList
  ListView1.SmallImages := ImageList1;
  ListView1.LargeImages := ImageList2;

  // Ajouter un élément avec une image
  Item := ListView1.Items.Add;
  Item.Caption := 'Document.txt';
  Item.ImageIndex := 0; // Index de l'image dans l'ImageList
  Item.SubItems.Add('10 Ko');
end;
```

### Recherche dans le ListView

```pascal
procedure TForm1.RechercherDansListView(const Texte: string);
var
  i: Integer;
  Item: TListItem;
  Trouve: Boolean;
begin
  Trouve := False;

  for i := 0 to ListView1.Items.Count - 1 do
  begin
    Item := ListView1.Items[i];
    // Recherche dans la première colonne
    if Pos(LowerCase(Texte), LowerCase(Item.Caption)) > 0 then
    begin
      Item.Selected := True;
      Item.MakeVisible(False);
      Trouve := True;
      Break;
    end;
  end;

  if not Trouve then
    ShowMessage('Aucun élément trouvé');
end;
```

### Supprimer des éléments

```pascal
// Supprimer l'élément sélectionné
procedure TForm1.SupprimerSelection;
begin
  if Assigned(ListView1.Selected) then
  begin
    if MessageDlg('Voulez-vous vraiment supprimer cet élément ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      ListView1.Selected.Delete;
    end;
  end;
end;

// Supprimer tous les éléments sélectionnés (MultiSelect = True)
procedure TForm1.SupprimerToutesSelections;
var
  i: Integer;
begin
  if ListView1.SelCount > 0 then
  begin
    if MessageDlg('Supprimer ' + IntToStr(ListView1.SelCount) + ' élément(s) ?',
                  mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      // Parcourir de la fin vers le début pour éviter les problèmes d'index
      for i := ListView1.Items.Count - 1 downto 0 do
      begin
        if ListView1.Items[i].Selected then
          ListView1.Items[i].Delete;
      end;
    end;
  end;
end;
```

### Conseils pratiques

- Utilisez `BeginUpdate` / `EndUpdate` lors de l'ajout ou suppression de nombreux éléments
- Activez `RowSelect` pour une meilleure expérience utilisateur en mode Report
- Implémentez le tri par colonnes pour faciliter la navigation
- Utilisez des couleurs alternées pour améliorer la lisibilité (voir propriété `AlternateRowColor` dans certaines versions)
- Stockez des données supplémentaires dans la propriété `Data` de chaque item

---

## Comparaison des trois contrôles

| Critère | PageControl | TreeView | ListView |
|---------|-------------|----------|----------|
| **Usage principal** | Organisation par onglets | Structure hiérarchique | Liste de données |
| **Nombre de niveaux** | 1 seul niveau | Illimité | 1 seul niveau |
| **Affichage d'images** | Possible (sur onglets) | Oui | Oui (plusieurs modes) |
| **Colonnes** | Non | Non | Oui (mode Report) |
| **Complexité** | Simple | Moyenne | Moyenne à élevée |
| **Cas d'usage typique** | Paramètres | Navigation | Catalogue, résultats |

## Bonnes pratiques générales

1. **Cohérence** : Utilisez le même type de contrôle pour des fonctionnalités similaires dans votre application
2. **Performance** : Utilisez `BeginUpdate` / `EndUpdate` lors de modifications multiples
3. **Accessibilité** : Assurez-vous que les contrôles sont navigables au clavier
4. **Feedback utilisateur** : Fournissez un retour visuel lors des interactions
5. **Documentation** : Commentez votre code, surtout lors de manipulations complexes

## Conclusion

Ces trois contrôles avancés sont des outils puissants pour créer des interfaces utilisateur riches et professionnelles :

- **PageControl** : Parfait pour organiser de nombreux contrôles et informations
- **TreeView** : Idéal pour représenter des structures hiérarchiques
- **ListView** : Excellent pour afficher des listes de données détaillées

Maîtriser ces composants vous permettra de créer des applications Delphi plus sophistiquées et plus agréables à utiliser. N'hésitez pas à expérimenter avec les différentes propriétés et événements pour découvrir toutes leurs possibilités !

⏭️ [Menus et barres d'outils](/04-conception-dinterfaces-utilisateur-avec-la-vcl/05-menus-et-barres-doutils.md)
