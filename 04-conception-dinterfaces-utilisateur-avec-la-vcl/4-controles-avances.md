# 4.4 Contrôles avancés (PageControl, TreeView, ListView)

Les contrôles avancés de la VCL vous permettent de créer des interfaces utilisateur plus riches et plus interactives. Dans cette section, nous explorerons trois contrôles particulièrement puissants : les contrôles à onglets (`TPageControl`), les vues en arbre (`TTreeView`) et les vues en liste (`TListView`). Bien que nous ayons déjà abordé brièvement `TTreeView` et `TListView` dans la section précédente, nous les examinerons ici plus en profondeur.

## Contrôle à onglets (TPageControl et TTabSheet)

Le contrôle à onglets est l'un des moyens les plus efficaces d'organiser une interface utilisateur complexe en plusieurs pages accessibles facilement. Il est composé de deux éléments principaux :

- `TPageControl` : le conteneur principal qui gère les onglets
- `TTabSheet` : chaque onglet individuel qui contient des composants

### Création d'un PageControl

Pour ajouter un contrôle à onglets à votre formulaire :

1. Cliquez sur le composant `PageControl` dans l'onglet **Win32** de la palette de composants
2. Dessinez-le à la taille souhaitée sur votre formulaire
3. Par défaut, un premier onglet (`TabSheet1`) est créé automatiquement

### Propriétés importantes de TPageControl

- **ActivePage** : l'onglet actuellement sélectionné
- **ActivePageIndex** : l'indice de l'onglet actif (commence à 0)
- **MultiLine** : permet d'afficher les onglets sur plusieurs lignes
- **TabPosition** : position des onglets (haut, bas, gauche, droite)
- **Style** : style des onglets (standard, boutons, plats)
- **HotTrack** : surligne l'onglet sous le curseur
- **Images** : permet d'associer une ImageList pour afficher des icônes
- **TabHeight** et **TabWidth** : hauteur et largeur des onglets

### Propriétés importantes de TTabSheet

- **Caption** : texte affiché sur l'onglet
- **ImageIndex** : indice de l'image à afficher
- **TabVisible** : rend l'onglet visible ou invisible
- **Enabled** : active ou désactive l'onglet
- **PageIndex** : position de l'onglet dans le PageControl

### Ajouter et supprimer des onglets

#### À la conception

1. Cliquez avec le bouton droit sur le `PageControl` et sélectionnez "Nouveau TabSheet"
2. Utilisez l'Inspecteur d'objets pour modifier ses propriétés
3. Placez des composants sur chaque onglet comme vous le feriez sur un formulaire

#### Par code à l'exécution

```pascal
// Ajouter un nouvel onglet
procedure TForm1.ButtonAjouterClick(Sender: TObject);
var
  NouvelOnglet: TTabSheet;
  Compteur: Integer;
begin
  // Obtenir le nombre actuel d'onglets pour nommer le nouvel onglet
  Compteur := PageControl1.PageCount + 1;

  // Créer un nouvel onglet
  NouvelOnglet := TTabSheet.Create(PageControl1);
  NouvelOnglet.PageControl := PageControl1;  // Associer au PageControl
  NouvelOnglet.Caption := 'Onglet ' + IntToStr(Compteur);

  // Optionnel : ajouter des composants au nouvel onglet
  with TButton.Create(NouvelOnglet) do
  begin
    Parent := NouvelOnglet;
    Caption := 'Bouton dans onglet ' + IntToStr(Compteur);
    Left := 20;
    Top := 20;
  end;

  // Activer le nouvel onglet
  PageControl1.ActivePage := NouvelOnglet;
end;

// Supprimer l'onglet actif
procedure TForm1.ButtonSupprimerClick(Sender: TObject);
begin
  if PageControl1.PageCount > 1 then
    PageControl1.ActivePage.Free;
  // Note : la destruction d'un TabSheet le retire automatiquement du PageControl
end;
```

### Événements importants

- **OnChange** : déclenché lorsque l'onglet actif change
- **OnChanging** : déclenché juste avant le changement d'onglet (peut être annulé)
- **OnMouseLeave** : déclenché lorsque le curseur quitte la zone des onglets

### Exemple : Formulaire de configuration simple

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration des onglets
  TabSheetGeneral.Caption := 'Général';
  TabSheetAffichage.Caption := 'Affichage';
  TabSheetAvance.Caption := 'Avancé';

  // Configuration du contenu de l'onglet Général
  with TCheckBox.Create(TabSheetGeneral) do
  begin
    Parent := TabSheetGeneral;
    Caption := 'Démarrer avec Windows';
    Left := 20;
    Top := 20;
  end;

  with TCheckBox.Create(TabSheetGeneral) do
  begin
    Parent := TabSheetGeneral;
    Caption := 'Afficher les notifications';
    Left := 20;
    Top := 50;
    Checked := True;
  end;

  // Configuration similaire pour les autres onglets...
end;
```

### Astuces pour TPageControl

1. **Pages invisibles** : Vous pouvez cacher certains onglets avec `TabSheet.TabVisible := False` tout en gardant leur contenu accessible par code.

2. **Onglets dynamiques** : Créez des onglets en fonction des besoins de l'utilisateur, par exemple un onglet par document ouvert.

3. **Validations avant changement** : Utilisez l'événement `OnChanging` pour valider les données avant de changer d'onglet :

```pascal
procedure TForm1.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  // Vérifier si les données de l'onglet actuel sont valides
  if (PageControl1.ActivePage = TabSheetDonnees) and not ValiderDonnees then
  begin
    ShowMessage('Veuillez corriger les erreurs avant de changer d''onglet.');
    AllowChange := False;  // Empêcher le changement d'onglet
  end;
end;
```

## Vue en arbre (TTreeView) - Présentation avancée

Nous avons déjà introduit le composant `TTreeView` dans la section précédente, mais explorons maintenant des fonctionnalités plus avancées.

### Personnalisation avancée

#### Déployer/replier tous les nœuds

```pascal
// Déployer tous les nœuds
procedure TForm1.ButtonDevelopperClick(Sender: TObject);
begin
  TreeView1.FullExpand;
end;

// Replier tous les nœuds
procedure TForm1.ButtonReplierClick(Sender: TObject);
begin
  TreeView1.FullCollapse;
end;

// Déployer jusqu'à un certain niveau
procedure TForm1.DeployerJusquAuNiveau(Niveau: Integer);
var
  i: Integer;
  Node: TTreeNode;
begin
  for i := 0 to TreeView1.Items.Count - 1 do
  begin
    Node := TreeView1.Items[i];
    if Node.Level < Niveau then
      Node.Expand(False);
  end;
end;
```

#### Cases à cocher dans un TreeView

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer les cases à cocher
  TreeView1.StateImages := ImageListStateIcons;  // ImageList avec cases à cocher
  TreeView1.CheckBoxes := True;
end;

// Obtenir l'état coché/non coché
function TForm1.EstCoche(Node: TTreeNode): Boolean;
begin
  Result := Node.StateIndex = 1;  // Supposant que 1 = coché, 0 = non coché
end;
```

#### Drag & Drop dans un TreeView

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer le drag & drop
  TreeView1.DragMode := dmAutomatic;
end;

procedure TForm1.TreeView1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  DestNode: TTreeNode;
begin
  // Accepter uniquement le drop si c'est sur un nœud valide
  DestNode := TreeView1.GetNodeAt(X, Y);
  Accept := (Source = TreeView1) and (DestNode <> nil);
end;

procedure TForm1.TreeView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  SourceNode, DestNode: TTreeNode;
begin
  if Source = TreeView1 then
  begin
    SourceNode := TreeView1.Selected;
    DestNode := TreeView1.GetNodeAt(X, Y);

    if (SourceNode <> nil) and (DestNode <> nil) then
    begin
      // Éviter de déplacer un nœud sur lui-même ou sur un de ses enfants
      if not SourceNode.HasAsParent(DestNode) then
      begin
        // Déplacer le nœud
        SourceNode.MoveTo(DestNode, naAddChild);
      end;
    end;
  end;
end;
```

### Exemple avancé : Explorer les dossiers avec taille

```pascal
type
  // Structure pour stocker les informations de taille
  TDossierInfo = class
    Taille: Int64;
    Fichiers: Integer;
  end;

procedure TForm1.ButtonAnalyseClick(Sender: TObject);
var
  RootNode: TTreeNode;
begin
  if SelectDirectory('Sélectionnez un dossier à analyser', '', DossierSelectionne) then
  begin
    // Réinitialiser l'arbre
    TreeView1.Items.Clear;

    // Créer le nœud racine
    RootNode := TreeView1.Items.Add(nil, ExtractFileName(DossierSelectionne));
    RootNode.Data := TDossierInfo.Create;

    // Analyser le dossier
    Screen.Cursor := crHourGlass;
    try
      AnalyserDossier(DossierSelectionne, RootNode);

      // Mettre à jour l'affichage avec les tailles
      MettreAJourAffichageTailles(RootNode);

      // Développer le premier niveau
      RootNode.Expand(False);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.AnalyserDossier(const Chemin: string; Node: TTreeNode);
var
  SearchRec: TSearchRec;
  Result: Integer;
  NewNode: TTreeNode;
  Info: TDossierInfo;
  SousDossierChemin: string;
  Taille: Int64;
begin
  // Obtenir l'objet info du nœud parent
  Info := TDossierInfo(Node.Data);

  // Analyser les fichiers
  Result := FindFirst(Chemin + '\*.*', faAnyFile, SearchRec);
  try
    while Result = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        if (SearchRec.Attr and faDirectory) = faDirectory then
        begin
          // C'est un dossier
          SousDossierChemin := Chemin + '\' + SearchRec.Name;

          // Créer un nœud pour le sous-dossier
          NewNode := TreeView1.Items.AddChild(Node, SearchRec.Name);
          NewNode.Data := TDossierInfo.Create;

          // Analyser récursivement le sous-dossier
          AnalyserDossier(SousDossierChemin, NewNode);

          // Ajouter la taille du sous-dossier au dossier parent
          Info.Taille := Info.Taille + TDossierInfo(NewNode.Data).Taille;
          Info.Fichiers := Info.Fichiers + TDossierInfo(NewNode.Data).Fichiers;
        end
        else
        begin
          // C'est un fichier
          Taille := SearchRec.Size;
          Info.Taille := Info.Taille + Taille;
          Inc(Info.Fichiers);
        end;
      end;

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TForm1.MettreAJourAffichageTailles(Node: TTreeNode);
var
  i: Integer;
  Info: TDossierInfo;
begin
  // Mettre à jour le texte avec la taille
  Info := TDossierInfo(Node.Data);
  Node.Text := Node.Text + Format(' (%s, %d fichiers)',
                            [FormatTaille(Info.Taille), Info.Fichiers]);

  // Mettre à jour les nœuds enfants
  for i := 0 to Node.Count - 1 do
    MettreAJourAffichageTailles(Node.Item[i]);
end;

// Fonction pour formater une taille en Ko, Mo, Go
function TForm1.FormatTaille(Taille: Int64): string;
begin
  if Taille < 1024 then
    Result := Format('%d octets', [Taille])
  else if Taille < 1024*1024 then
    Result := Format('%.2f Ko', [Taille / 1024])
  else if Taille < 1024*1024*1024 then
    Result := Format('%.2f Mo', [Taille / (1024*1024)])
  else
    Result := Format('%.2f Go', [Taille / (1024*1024*1024)]);
end;

// Libérer les objets TDossierInfo lors de la destruction de l'arbre
procedure TForm1.TreeView1Deletion(Sender: TObject; Node: TTreeNode);
begin
  if Node.Data <> nil then
    TDossierInfo(Node.Data).Free;
end;
```

## Vue en liste (TListView) - Présentation avancée

Le composant `TListView` offre une grande flexibilité pour afficher des données de différentes manières. Voyons quelques techniques avancées pour l'exploiter pleinement.

### Styles d'affichage avancés

#### Personnalisation des éléments (OwnerDraw)

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer le dessin personnalisé
  ListView1.OwnerDraw := True;
  ListView1.OnDrawItem := ListView1DrawItem;
  ListView1.OnDrawSubItem := ListView1DrawSubItem;
end;

// Dessiner un élément entier (en mode vsIcon, vsSmallIcon ou vsList)
procedure TForm1.ListView1DrawItem(Sender: TCustomListView; Item: TListItem;
  Rect: TRect; State: TOwnerDrawState);
begin
  with Sender.Canvas do
  begin
    // Fond différent selon l'état
    if odSelected in State then
      Brush.Color := clHighlight
    else if Odd(Item.Index) then
      Brush.Color := clMoneyGreen  // Effet "zébré"
    else
      Brush.Color := clWhite;

    FillRect(Rect);

    // Dessiner le texte
    if odSelected in State then
      Font.Color := clHighlightText
    else
      Font.Color := clWindowText;

    TextOut(Rect.Left + 4, Rect.Top + 2, Item.Caption);
  end;
end;

// Dessiner une sous-colonne (en mode vsReport)
procedure TForm1.ListView1DrawSubItem(Sender: TCustomListView; Item: TListItem;
  SubItem: Integer; Rect: TRect; State: TOwnerDrawState);
var
  S: string;
begin
  with Sender.Canvas do
  begin
    // Fond différent selon l'état
    if odSelected in State then
      Brush.Color := clHighlight
    else if Odd(Item.Index) then
      Brush.Color := clMoneyGreen  // Effet "zébré"
    else
      Brush.Color := clWhite;

    FillRect(Rect);

    // Dessiner le texte
    if odSelected in State then
      Font.Color := clHighlightText
    else
      Font.Color := clWindowText;

    // Dessiner différemment selon la colonne
    if SubItem = 0 then  // Première colonne (Caption)
      TextOut(Rect.Left + 4, Rect.Top + 2, Item.Caption)
    else
    begin
      S := Item.SubItems[SubItem - 1];

      // Alignement à droite pour les nombres
      if (SubItem = 2) and (S <> '') then // Colonne de taille
        TextOut(Rect.Right - TextWidth(S) - 4, Rect.Top + 2, S)
      else
        TextOut(Rect.Left + 4, Rect.Top + 2, S);
    end;
  end;
end;
```

#### Groupes dans un ListView

La fonctionnalité de groupes (disponible depuis Windows XP) permet d'organiser les éléments en catégories visuelles.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer les groupes
  ListView1.GroupView := True;

  // Créer quelques groupes
  with ListView1.Groups.Add do
  begin
    Header := 'Documents';
    ID := 1;
    State := [lgsNormal, lgsCollapsible];
  end;

  with ListView1.Groups.Add do
  begin
    Header := 'Images';
    ID := 2;
    State := [lgsNormal, lgsCollapsible];
  end;

  with ListView1.Groups.Add do
  begin
    Header := 'Autres';
    ID := 3;
    State := [lgsNormal, lgsCollapsible];
  end;
end;

procedure TForm1.AjouterFichierAuBonGroupe(const Chemin: string);
var
  Ext: string;
  GroupID: Integer;
  ListItem: TListItem;
begin
  Ext := LowerCase(ExtractFileExt(Chemin));

  // Déterminer le groupe selon l'extension
  if Ext = '.doc' then
    GroupID := 1
  else if (Ext = '.jpg') or (Ext = '.png') or (Ext = '.bmp') then
    GroupID := 2
  else
    GroupID := 3;

  // Ajouter l'élément au bon groupe
  ListItem := ListView1.Items.Add;
  ListItem.Caption := ExtractFileName(Chemin);
  ListItem.SubItems.Add(Ext);
  ListItem.GroupID := GroupID;
end;
```

### Tri et filtrage avancés

#### Tri personnalisé

```pascal
procedure TForm1.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin
  // Stocker la colonne de tri
  if FTriColonne = Column.Index then
    // Même colonne : inverser l'ordre
    FTriAscendant := not FTriAscendant
  else
  begin
    // Nouvelle colonne : trier en ordre ascendant
    FTriColonne := Column.Index;
    FTriAscendant := True;
  end;

  // Trier la liste
  (Sender as TListView).CustomSort(@ComparerElements, FTriColonne);
end;

// Fonction de comparaison appelée par CustomSort
function ComparerElements(Item1, Item2: TListItem; Data: Integer): Integer; stdcall;
var
  S1, S2: string;
  N1, N2: Int64;
  Form: TForm1;
begin
  Form := Application.MainForm as TForm1;

  // Data contient l'indice de la colonne de tri
  if Data = 0 then
  begin
    // Tri sur la première colonne (Caption)
    S1 := Item1.Caption;
    S2 := Item2.Caption;
  end
  else
  begin
    // Tri sur une sous-colonne
    S1 := Item1.SubItems[Data - 1];
    S2 := Item2.SubItems[Data - 1];
  end;

  // Comparer selon le type de données
  if Data = 2 then
  begin
    // Colonne de taille (numérique)
    N1 := StrToInt64Def(S1, 0);
    N2 := StrToInt64Def(S2, 0);

    if N1 < N2 then
      Result := -1
    else if N1 > N2 then
      Result := 1
    else
      Result := 0;
  end
  else
  begin
    // Colonnes textuelles
    Result := CompareText(S1, S2);
  end;

  // Inverser si tri descendant
  if not Form.FTriAscendant then
    Result := -Result;
end;
```

#### Recherche incrémentale

```pascal
procedure TForm1.EditRechercheChange(Sender: TObject);
var
  i: Integer;
  Recherche: string;
  Trouve: Boolean;
begin
  Recherche := LowerCase(EditRecherche.Text);

  if Recherche = '' then
  begin
    // Réafficher tous les éléments
    for i := 0 to ListView1.Items.Count - 1 do
      ListView1.Items[i].Visible := True;

    Exit;
  end;

  // Masquer les éléments qui ne correspondent pas
  Trouve := False;
  for i := 0 to ListView1.Items.Count - 1 do
  begin
    if Pos(Recherche, LowerCase(ListView1.Items[i].Caption)) > 0 then
    begin
      ListView1.Items[i].Visible := True;

      if not Trouve then
      begin
        // Sélectionner le premier élément trouvé
        ListView1.Selected := ListView1.Items[i];
        ListView1.Items[i].MakeVisible(False);
        Trouve := True;
      end;
    end
    else
      ListView1.Items[i].Visible := False;
  end;
end;
```

### Exemple intégré : Explorateur de fichiers avancé

Voici un exemple qui combine `TTreeView` (pour la navigation dans les dossiers) et `TListView` (pour afficher le contenu du dossier sélectionné) :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration du TreeView pour les dossiers
  TreeView1.Images := ImageListDossiers;
  ChargerLecteurs;

  // Configuration du ListView pour les fichiers
  ListView1.ViewStyle := vsReport;
  ListView1.SmallImages := ImageListFichiers;

  with ListView1.Columns.Add do
  begin
    Caption := 'Nom';
    Width := 200;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'Taille';
    Width := 100;
    Alignment := taRightJustify;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'Type';
    Width := 120;
  end;

  with ListView1.Columns.Add do
  begin
    Caption := 'Date de modification';
    Width := 150;
  end;
end;

procedure TForm1.ChargerLecteurs;
// Code pour charger les lecteurs dans le TreeView
// Similaire à l'exemple précédent
// ...

procedure TForm1.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  // Quand un nœud est sélectionné, afficher son contenu
  if Node <> nil then
    ChargerContenuDossier(ExtraireCheminComplet(Node));
end;

procedure TForm1.ChargerContenuDossier(const Chemin: string);
var
  SearchRec: TSearchRec;
  Result: Integer;
  ListItem: TListItem;
  S: string;
  FileInfo: TSHFileInfo;
begin
  ListView1.Items.Clear;

  if Chemin = '' then Exit;

  Result := FindFirst(Chemin + '\*.*', faAnyFile, SearchRec);
  try
    while Result = 0 do
    begin
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        ListItem := ListView1.Items.Add;
        ListItem.Caption := SearchRec.Name;

        // Déterminer l'icône appropriée pour le fichier
        SHGetFileInfo(PChar(Chemin + '\' + SearchRec.Name), 0, FileInfo,
                      SizeOf(FileInfo), SHGFI_ICON or SHGFI_SMALLICON or SHGFI_TYPENAME);

        // Taille
        if (SearchRec.Attr and faDirectory) = faDirectory then
          S := '<Dossier>'
        else
          S := FormatTaille(SearchRec.Size);

        ListItem.SubItems.Add(S);

        // Type
        ListItem.SubItems.Add(FileInfo.szTypeName);

        // Date de modification
        ListItem.SubItems.Add(DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

        // Stocker l'attribut pour différencier dossiers et fichiers
        ListItem.Data := Pointer(SearchRec.Attr);

        // Icône
        if (SearchRec.Attr and faDirectory) = faDirectory then
          ListItem.ImageIndex := 0  // Icône de dossier
        else
          ListItem.ImageIndex := DeterminerIconeFichier(SearchRec.Name);
      end;

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

// Double-clic sur un élément de la liste
procedure TForm1.ListView1DblClick(Sender: TObject);
var
  Chemin: string;
  Item: TListItem;
begin
  Item := ListView1.Selected;

  if Item = nil then Exit;

  // Si c'est un dossier, naviguer vers ce dossier
  if (Integer(Item.Data) and faDirectory) = faDirectory then
  begin
    Chemin := ExtraireCheminComplet(TreeView1.Selected) + '\' + Item.Caption;

    // Chercher si le nœud existe déjà
    var Node := TrouverOuCreerNoeud(TreeView1.Selected, Item.Caption);

    // Sélectionner le nœud (ce qui chargera son contenu via l'événement OnChange)
    if Node <> nil then
      TreeView1.Selected := Node;
  end
  else
  begin
    // Si c'est un fichier, l'ouvrir avec l'application associée
    Chemin := ExtraireCheminComplet(TreeView1.Selected) + '\' + Item.Caption;
    ShellExecute(0, 'open', PChar(Chemin), nil, nil, SW_SHOWNORMAL);
  end;
end;

// Recherche ou création d'un nœud dans le TreeView
function TForm1.TrouverOuCreerNoeud(ParentNode: TTreeNode;
  const NomDossier: string): TTreeNode;
var
  i: Integer;
  Node: TTreeNode;
begin
  // Chercher si le nœud existe déjà
  for i := 0 to ParentNode.Count - 1 do
  begin
    if CompareText(ParentNode.Item[i].Text, NomDossier) = 0 then
    begin
      Result := ParentNode.Item[i];
      Exit;
    end;
  end;

  // Sinon, créer un nouveau nœud
  Result := TreeView1.Items.AddChild(ParentNode, NomDossier);
  Result.ImageIndex := 0;  // Icône de dossier
  Result.SelectedIndex := 0;

  // Ajouter un nœud factice pour permettre l'expansion
  TreeView1.Items.AddChild(Result, 'Chargement...');
end;
```

## Conclusion et bonnes pratiques

Les contrôles avancés comme `TPageControl`, `TTreeView` et `TListView` offrent une grande richesse fonctionnelle pour créer des interfaces utilisateur sophistiquées. Voici quelques conseils pour les utiliser efficacement :

### Pour PageControl

- Utilisez-le pour organiser logiquement les fonctionnalités par catégories
- Limitez le nombre d'onglets (5-7 maximum) pour éviter de surcharger l'interface
- Considérez l'utilisation d'icônes pour rendre les onglets plus reconnaissables

### Pour TreeView

- Chargez les données à la demande pour les grandes structures
- Utilisez des icônes pour différencier les types de nœuds
- Pensez à personnaliser le menu contextuel (clic droit) pour des actions spécifiques aux nœuds

### Pour ListView

- Choisissez le style d'affichage le plus adapté à vos données
- Pensez à utiliser les groupes pour mieux organiser les informations
- Implémentez des fonctions de tri et de filtrage pour faciliter la navigation

Ces contrôles avancés demandent un peu plus d'efforts de programmation mais offrent une expérience utilisateur beaucoup plus riche et professionnelle.

Dans la prochaine section, nous explorerons les menus et barres d'outils, qui compléteront votre boîte à outils pour créer des interfaces utilisateur professionnelles.

---

*Exercice pratique : Créez une application simple de gestion de tâches en utilisant un PageControl avec deux onglets : "Tâches" et "Statistiques". Dans l'onglet "Tâches", placez un ListView pour afficher la liste des tâches avec leur statut et leur priorité. Dans l'onglet "Statistiques", affichez un TreeView montrant les tâches organisées par catégories et par priorité.*
