# 4.3 Composants standard et leur utilisation

Les composants standard sont les éléments de base de toute interface utilisateur développée avec Delphi. Ils permettent aux utilisateurs d'interagir avec votre application et d'afficher des informations. Ces composants sont regroupés principalement dans les onglets **Standard** et **Additional** de la palette de composants.

## Qu'est-ce qu'un composant VCL ?

Avant de plonger dans les composants spécifiques, comprenons ce qu'est un composant VCL :

Un **composant** est un élément réutilisable de l'interface utilisateur ou du code qui encapsule des fonctionnalités spécifiques. Dans Delphi, les composants VCL sont des classes Object Pascal qui dérivent généralement de `TComponent` ou de l'une de ses classes dérivées.

## La palette de composants

La palette de composants est votre "boîte à outils" principale dans Delphi. Elle est généralement située en haut de l'IDE et organisée en onglets thématiques.

Pour utiliser un composant :
1. Cliquez sur le composant dans la palette
2. Cliquez à l'endroit souhaité sur votre formulaire ou dessinez-le en maintenant le bouton de la souris enfoncé
3. Ajustez ses propriétés à l'aide de l'Inspecteur d'objets

![Palette de composants](https://via.placeholder.com/600x100)

## La hiérarchie des composants

Les composants VCL sont organisés en une hiérarchie d'héritage bien définie :

```
TObject
  └── TPersistent
       └── TComponent
            └── TControl
                 ├── TGraphicControl (contrôles légers, sans fenêtre Windows)
                 └── TWinControl (contrôles avec fenêtre Windows native)
                      └── Divers contrôles spécialisés...
```

Cette hiérarchie est importante à comprendre car elle détermine le comportement de base et les capacités des différents composants.

## Nommer les composants

Lors de la création d'un composant, Delphi lui attribue automatiquement un nom par défaut (par exemple, `Button1`, `Edit1`, etc.). Il est fortement recommandé de renommer vos composants pour leur donner des noms significatifs qui reflètent leur fonction.

Pour renommer un composant :
1. Sélectionnez le composant sur le formulaire
2. Dans l'Inspecteur d'objets, modifiez la propriété `Name`
3. Utilisez une convention de nommage cohérente (par exemple `btnSave`, `edtName`, etc.)

Une convention de nommage courante consiste à utiliser un préfixe indiquant le type de composant :

| Préfixe | Type de composant |
|---------|------------------|
| btn     | Button (bouton)  |
| lbl     | Label (étiquette) |
| edt     | Edit (champ de texte) |
| cbb     | ComboBox (liste déroulante) |
| lst     | ListBox (liste) |
| chk     | CheckBox (case à cocher) |
| rad     | RadioButton (bouton radio) |
| pnl     | Panel (panneau) |
| img     | Image (image) |
| mmo     | Memo (zone de texte multi-lignes) |

## Propriétés, méthodes et événements communs

La plupart des composants VCL partagent un ensemble de propriétés, méthodes et événements de base.

### Propriétés communes

- **Name** : nom du composant (utilisé dans le code)
- **Caption** ou **Text** : texte affiché par le composant
- **Enabled** : détermine si le composant peut répondre aux actions de l'utilisateur
- **Visible** : détermine si le composant est visible à l'exécution
- **Hint** : texte de l'info-bulle qui apparaît quand on survole le composant
- **ShowHint** : active/désactive l'affichage de l'info-bulle
- **Font** : définit la police utilisée pour afficher le texte
- **Color** : couleur de fond du composant
- **Cursor** : forme du curseur lorsqu'il survole le composant
- **Left, Top, Width, Height** : position et taille du composant
- **Align** : mode d'alignement automatique du composant
- **Anchors** : points d'ancrage pour le redimensionnement
- **TabOrder** : ordre de tabulation pour la navigation au clavier
- **TabStop** : détermine si le composant reçoit le focus lors de la navigation par Tab

### Méthodes communes

- **SetFocus** : donne le focus au contrôle
- **Hide** / **Show** : masque ou affiche le contrôle
- **BringToFront** / **SendToBack** : gère l'ordre de superposition
- **Update** : force la mise à jour visuelle du contrôle
- **Refresh** : redessine complètement le contrôle

### Événements communs

- **OnClick** : déclenché quand l'utilisateur clique sur le composant
- **OnDblClick** : déclenché lors d'un double-clic
- **OnMouseDown, OnMouseMove, OnMouseUp** : événements liés à la souris
- **OnKeyDown, OnKeyPress, OnKeyUp** : événements liés au clavier
- **OnEnter, OnExit** : déclenché quand le composant reçoit ou perd le focus
- **OnChange** : déclenché lorsque le contenu du composant change
- **OnDragDrop, OnDragOver** : pour gérer le glisser-déposer
- **OnResize** : déclenché lorsque le composant est redimensionné

## L'Inspecteur d'objets

L'Inspecteur d'objets est votre principal outil pour interagir avec les composants. Il permet de :
- Consulter et modifier les propriétés des composants
- Créer des gestionnaires d'événements
- Explorer les composants dans une hiérarchie

![Inspecteur d'objets](https://via.placeholder.com/300x400)

### Utilisation efficace de l'Inspecteur d'objets

- Utilisez la zone de recherche en haut pour trouver rapidement une propriété
- Basculez entre l'affichage par catégories et l'affichage alphabétique
- Pour les propriétés complexes, cliquez sur les boutons `...` ou `+` pour accéder à des éditeurs spécialisés
- Double-cliquez sur un événement pour créer automatiquement un gestionnaire

## Positionnement des composants

Delphi offre plusieurs outils pour positionner précisément vos composants :

### Alignement manuel

- Utilisez la grille de conception comme guide
- Maintenez la touche `Shift` enfoncée pour aligner sur la grille
- Utilisez les touches fléchées pour des ajustements précis

### Outils d'alignement

Le menu **Edition > Aligner** propose plusieurs options :
- Aligner les bords (gauche, droite, haut, bas)
- Centrer (horizontalement, verticalement)
- Uniformiser les tailles
- Répartir les espaces entre composants

### Alignement automatique (propriété Align)

La propriété `Align` permet de positionner automatiquement un composant par rapport à son parent :
- `alNone` : pas d'alignement automatique
- `alTop` : s'étend en haut
- `alBottom` : s'étend en bas
- `alLeft` : s'étend à gauche
- `alRight` : s'étend à droite
- `alClient` : remplit tout l'espace restant
- `alCustom` : alignement personnalisé (Delphi 10.4 et supérieur)

### Ancrages (propriété Anchors)

Les ancrages définissent comment un composant se redimensionne lorsque son parent est redimensionné :
- `akLeft` : ancré au bord gauche
- `akTop` : ancré au bord supérieur
- `akRight` : ancré au bord droit
- `akBottom` : ancré au bord inférieur

Par exemple, un composant avec `[akLeft, akTop, akRight]` s'étirera horizontalement mais gardera sa position verticale.

## Conseils pour les débutants

1. **Explorez la palette** : Prenez le temps de découvrir tous les composants disponibles
2. **Utilisez l'aide contextuelle** : F1 sur un composant sélectionné affiche sa documentation
3. **Commencez simple** : Maîtrisez d'abord les composants de base avant d'aborder les plus complexes
4. **Nommez correctement** : De bons noms facilitent grandement la maintenance du code
5. **Créez des prototypes** : Expérimentez avec différentes dispositions et composants

## Conclusion

Les composants standard sont les briques fondamentales de toute application Delphi. Une bonne compréhension de leurs propriétés, méthodes et événements vous permettra de créer des interfaces utilisateur riches et fonctionnelles.

Dans les sections suivantes, nous explorerons en détail les différentes catégories de composants standard, en commençant par les contrôles d'affichage.

---

*Astuce pratique : Pour comprendre rapidement un composant, créez un nouveau projet test, placez le composant sur un formulaire, et explorez ses propriétés et événements. Cette approche pratique est souvent plus efficace que la simple lecture de documentation.*

# 4.3.1 Contrôles d'affichage (Labels, Images)

Les contrôles d'affichage sont des composants VCL qui servent principalement à présenter des informations à l'utilisateur, sans nécessairement permettre une interaction directe. Ces composants sont essentiels pour construire une interface claire et informative. Dans cette section, nous allons explorer les deux types principaux de contrôles d'affichage : les étiquettes (Labels) et les images (Images).

## Les étiquettes (TLabel)

L'étiquette, représentée par le composant `TLabel`, est sans doute le composant d'affichage le plus simple et le plus utilisé. Son rôle principal est d'afficher du texte statique sur un formulaire.

### Création d'une étiquette

Pour ajouter une étiquette à votre formulaire :
1. Cliquez sur le composant `Label` dans l'onglet **Standard** de la palette de composants
2. Cliquez à l'endroit souhaité sur votre formulaire

### Propriétés importantes

- **Caption** : le texte affiché par l'étiquette
- **Alignment** : l'alignement du texte (gauche, droite, centré)
- **AutoSize** : si activé, l'étiquette s'ajuste automatiquement à la taille du texte
- **WordWrap** : si activé, le texte se répartit sur plusieurs lignes
- **FocusControl** : spécifie le contrôle qui recevra le focus quand on appuie sur le raccourci clavier
- **Font** : définit la police, le style, la taille et la couleur du texte
- **Color** : couleur de fond (souvent définie comme `clBtnFace` ou `clNone` pour être transparent)
- **Transparent** : si activé, le fond de l'étiquette est transparent

### Exemple d'utilisation basique

```pascal
// Création d'une étiquette en code
procedure TForm1.FormCreate(Sender: TObject);
var
  MyLabel: TLabel;
begin
  MyLabel := TLabel.Create(Self);
  MyLabel.Parent := Self;  // Le formulaire est le parent
  MyLabel.Left := 20;
  MyLabel.Top := 20;
  MyLabel.Caption := 'Bonjour Delphi !';
  MyLabel.Font.Size := 14;
  MyLabel.Font.Style := [fsBold];
  MyLabel.Font.Color := clBlue;
end;
```

### Astuces pour les étiquettes

1. **Raccourcis clavier** : Incluez un caractère '&' dans le Caption pour définir une touche d'accès rapide. Par exemple, `&Nom :` affichera "Nom :" et Alt+N activera le contrôle associé via FocusControl.

2. **Étiquettes comme séparateurs** : Créez des séparateurs visuels en utilisant un Label avec un seul caractère underscore ('_') étiré horizontalement.

3. **Text vs Caption** : Les Labels utilisent Caption, alors que d'autres contrôles utilisent souvent Text. C'est une subtilité importante à connaître.

### Labels avec liens (TLinkLabel)

Delphi propose également le composant `TLinkLabel` (dans l'onglet **Additional**) qui permet d'afficher des liens hypertextes cliquables.

```pascal
procedure TForm1.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, 'open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;
```

## Les images (TImage)

Le composant `TImage` permet d'afficher des images dans votre application. C'est un contrôle essentiel pour enrichir visuellement votre interface utilisateur.

### Création d'un composant Image

Pour ajouter une image à votre formulaire :
1. Cliquez sur le composant `Image` dans l'onglet **Additional** de la palette
2. Dessinez-le sur votre formulaire à la taille souhaitée

### Propriétés importantes

- **Picture** : contient l'image à afficher
- **Stretch** : si activé, l'image s'étire pour remplir tout le contrôle
- **Proportional** : conserve les proportions de l'image lors du redimensionnement
- **Center** : centre l'image dans le contrôle
- **AutoSize** : ajuste automatiquement la taille du contrôle à celle de l'image
- **Transparent** : rend transparent le fond de l'image (pour les formats qui supportent la transparence)

### Formats d'image supportés

Le composant `TImage` prend en charge plusieurs formats d'image courants :
- Bitmap (.bmp)
- JPEG (.jpg, .jpeg)
- PNG (.png)
- GIF (.gif)
- Icon (.ico)
- Windows Metafile (.wmf)

### Charger une image

Il existe plusieurs façons de charger une image :

#### 1. À la conception via l'Inspecteur d'objets

1. Sélectionnez le composant Image sur votre formulaire
2. Dans l'Inspecteur d'objets, cliquez sur les points de suspension (...) à côté de la propriété `Picture`
3. Sélectionnez l'image souhaitée dans la boîte de dialogue

#### 2. À l'exécution par code

```pascal
// Charger une image depuis un fichier
procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
end;

// Charger une image depuis les ressources
procedure TForm1.FormCreate(Sender: TObject);
begin
  Image1.Picture.Bitmap.LoadFromResourceName(HInstance, 'LOGO_RESOURCE');
end;
```

### Manipuler les images par code

```pascal
// Effacer une image
procedure TForm1.ButtonClearClick(Sender: TObject);
begin
  Image1.Picture := nil;  // ou Image1.Picture.Graphic := nil;
end;

// Redimensionner une image
procedure TForm1.ButtonResizeClick(Sender: TObject);
var
  Bitmap: TBitmap;
begin
  // Crée une copie redimensionnée
  Bitmap := TBitmap.Create;
  try
    Bitmap.SetSize(200, 150);  // Nouvelle taille
    Bitmap.Canvas.StretchDraw(Rect(0, 0, 200, 150), Image1.Picture.Graphic);
    Image1.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;  // Toujours libérer les ressources
  end;
end;
```

### Astuces pour les images

1. **Performances** : Les grandes images peuvent consommer beaucoup de mémoire. Redimensionnez-les avant de les charger si possible.

2. **Transparence** : Pour les images PNG avec transparence, assurez-vous que la propriété `Transparent` est activée.

3. **Double buffering** : Activez la propriété `DoubleBuffered` du formulaire pour éviter le scintillement lors du redessin des images.

## Autres contrôles d'affichage importants

### TShape

Le composant `TShape` permet d'afficher des formes géométriques simples.

Propriétés principales :
- **Shape** : type de forme (rectangle, cercle, ellipse, arrondi, carré)
- **Brush** : définit le remplissage (couleur, motif)
- **Pen** : définit le contour (couleur, style, épaisseur)

### TPaintBox

Le composant `TPaintBox` offre une zone où vous pouvez dessiner librement en utilisant le canevas (Canvas).

```pascal
procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  // Dessiner directement sur le canevas
  with PaintBox1.Canvas do
  begin
    Pen.Color := clRed;
    Pen.Width := 2;
    MoveTo(0, 0);
    LineTo(PaintBox1.Width, PaintBox1.Height);

    Brush.Color := clBlue;
    Ellipse(10, 10, 50, 50);
  end;
end;
```

### TPanel comme contrôle d'affichage

Bien que `TPanel` soit principalement un conteneur, il est souvent utilisé comme contrôle d'affichage pour :
- Créer des zones colorées ou avec bordures
- Regrouper visuellement des éléments liés
- Afficher des informations avec sa propriété `Caption`

## Cas d'utilisation pratiques

### Création d'une visionneuse d'images simple

```pascal
procedure TForm1.ButtonBrowseClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
    // Ajuster la taille de l'image si nécessaire
    Image1.Proportional := True;
    Image1.Stretch := True;
    // Afficher le nom du fichier
    lblFileName.Caption := ExtractFileName(OpenPictureDialog1.FileName);
  end;
end;
```

### Affichage d'un texte formaté avec TLabel

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer un texte enrichi
  lblTitle.Font.Size := 18;
  lblTitle.Font.Style := [fsBold];
  lblTitle.Caption := 'Bienvenue dans notre application';

  // Créer un texte explicatif sur plusieurs lignes
  lblDescription.WordWrap := True;
  lblDescription.Width := 300;
  lblDescription.Caption := 'Cette application vous permet de visualiser ' +
    'et de modifier des images. Utilisez les boutons ci-dessous pour ' +
    'charger, enregistrer ou manipuler vos images.';
end;
```

## Conclusion

Les contrôles d'affichage comme `TLabel` et `TImage` sont des composants fondamentaux pour créer des interfaces utilisateur informatives. Ils permettent d'afficher clairement des textes et des contenus visuels à vos utilisateurs.

Dans la prochaine section, nous explorerons les contrôles de saisie qui permettent aux utilisateurs d'interagir avec votre application et d'entrer des données.

---

*Exercice pratique : Créez un petit visualiseur d'images avec un TImage et plusieurs TLabels pour afficher des informations sur l'image (nom, dimensions, format). Ajoutez un bouton pour charger une nouvelle image et essayez d'implémenter un zoom simple.*

# 4.3.2 Contrôles de saisie (Edit, Memo, ComboBox)

Les contrôles de saisie sont des composants VCL essentiels qui permettent aux utilisateurs d'interagir avec votre application en entrant ou en sélectionnant des données. Dans cette section, nous explorerons les trois types fondamentaux de contrôles de saisie : les champs de texte (`TEdit`), les zones de texte multi-lignes (`TMemo`) et les listes déroulantes (`TComboBox`).

## Le champ de texte (TEdit)

Le composant `TEdit` est le contrôle de saisie le plus élémentaire. Il permet à l'utilisateur d'entrer une ligne de texte simple.

### Création d'un champ TEdit

Pour ajouter un champ de texte à votre formulaire :
1. Cliquez sur le composant `Edit` dans l'onglet **Standard** de la palette de composants
2. Cliquez à l'endroit souhaité sur votre formulaire

### Propriétés importantes

- **Text** : contient le texte affiché et saisi dans le champ
- **TextHint** : texte d'indication qui s'affiche quand le champ est vide
- **MaxLength** : nombre maximum de caractères autorisés (0 = illimité)
- **PasswordChar** : caractère à afficher pour masquer le texte (pour les mots de passe)
- **ReadOnly** : si activé, le texte ne peut pas être modifié
- **CharCase** : force la casse du texte (normale, majuscules, minuscules)
- **NumbersOnly** : si activé, seuls les chiffres sont acceptés
- **TextHint** : texte indicatif qui s'affiche quand le champ est vide

### Événements importants

- **OnChange** : déclenché chaque fois que le contenu du champ change
- **OnKeyPress** : déclenché lorsqu'une touche est pressée
- **OnKeyDown, OnKeyUp** : déclenchés au début et à la fin de la pression d'une touche
- **OnEnter, OnExit** : déclenchés lorsque le champ reçoit ou perd le focus

### Exemples d'utilisation

#### Validation de saisie

```pascal
procedure TForm1.EditAgeKeyPress(Sender: TObject; var Key: Char);
begin
  // Accepter uniquement les chiffres et la touche Backspace
  if not (Key in ['0'..'9', #8]) then
    Key := #0;
end;
```

#### Réaction au changement de texte

```pascal
procedure TForm1.EditSearchChange(Sender: TObject);
begin
  // Mettre à jour la liste filtrée lorsque le texte change
  ListBox1.Items.Clear;

  // Recherche simple
  for var i := 0 to MasterList.Count - 1 do
    if Pos(LowerCase(EditSearch.Text), LowerCase(MasterList[i])) > 0 then
      ListBox1.Items.Add(MasterList[i]);
end;
```

### Astuces pour TEdit

1. **Contrôle d'erreur** : Utilisez la propriété `Color` pour signaler visuellement une erreur de saisie (par exemple, en passant à clRed).

2. **Effacement automatique** : Utilisez l'événement `OnEnter` pour effacer automatiquement un champ lorsqu'il reçoit le focus.

   ```pascal
   procedure TForm1.EditCodeEnter(Sender: TObject);
   begin
     EditCode.SelectAll;  // Sélectionne tout le texte pour un remplacement facile
   end;
   ```

3. **TextHint moderne** : La propriété `TextHint` affiche un texte explicatif quand le champ est vide, très utile pour guider l'utilisateur.

## La zone de texte multi-lignes (TMemo)

Le composant `TMemo` est similaire à `TEdit`, mais il permet la saisie et l'affichage de texte sur plusieurs lignes.

### Création d'un composant TMemo

Pour ajouter un Memo à votre formulaire :
1. Cliquez sur le composant `Memo` dans l'onglet **Standard** de la palette
2. Dessinez-le à la taille souhaitée sur votre formulaire

### Propriétés importantes

- **Lines** : contient les lignes de texte (c'est un `TStrings`)
- **Text** : tout le texte sous forme d'une seule chaîne (avec des sauts de ligne)
- **ScrollBars** : barres de défilement (aucune, horizontale, verticale, les deux)
- **WordWrap** : si activé, le texte passe automatiquement à la ligne
- **WantReturns** : si activé, la touche Entrée ajoute un saut de ligne
- **WantTabs** : si activé, la touche Tab insère une tabulation
- **ReadOnly** : si activé, le texte ne peut pas être modifié

### Manipulation du texte dans un Memo

```pascal
// Ajouter du texte à la fin
Memo1.Lines.Add('Nouvelle ligne de texte');

// Insérer du texte à une position spécifique
Memo1.Lines.Insert(2, 'Ligne insérée en position 3');

// Charger le contenu depuis un fichier
Memo1.Lines.LoadFromFile('C:\chemin\vers\fichier.txt');

// Enregistrer le contenu dans un fichier
Memo1.Lines.SaveToFile('C:\chemin\vers\nouveau_fichier.txt');

// Effacer tout le contenu
Memo1.Clear;

// Accéder à une ligne spécifique
var troisièmeLigne := Memo1.Lines[2];  // Les indices commencent à 0

// Obtenir le nombre de lignes
var nombreLignes := Memo1.Lines.Count;
```

### Cas d'utilisation courants

- Saisie de commentaires ou de notes
- Affichage de logs ou de résultats détaillés
- Édition simple de texte ou de code
- Affichage de fichiers texte

### Astuces pour TMemo

1. **Performances** : Pour les grands volumes de texte, désactivez temporairement la mise à jour de l'affichage pendant l'ajout massif de texte :

   ```pascal
   Memo1.Lines.BeginUpdate;
   try
     for var i := 1 to 10000 do
       Memo1.Lines.Add('Ligne ' + i.ToString);
   finally
     Memo1.Lines.EndUpdate;
   end;
   ```

2. **Recherche de texte** : Implémentez une recherche simple dans un Memo :

   ```pascal
   function FindInMemo(Memo: TMemo; SearchText: string; StartPos: Integer): Integer;
   var
     FoundPos: Integer;
     MemText: string;
   begin
     MemText := Copy(Memo.Text, StartPos, Length(Memo.Text));
     FoundPos := Pos(LowerCase(SearchText), LowerCase(MemText));

     if FoundPos > 0 then
       Result := StartPos + FoundPos - 1
     else
       Result := 0;
   end;
   ```

## La liste déroulante (TComboBox)

Le composant `TComboBox` combine un champ de texte et une liste déroulante, permettant à l'utilisateur soit de saisir du texte, soit de sélectionner une valeur prédéfinie.

### Création d'une ComboBox

Pour ajouter une liste déroulante à votre formulaire :
1. Cliquez sur le composant `ComboBox` dans l'onglet **Standard** de la palette
2. Cliquez à l'endroit souhaité sur votre formulaire

### Propriétés importantes

- **Text** : texte affiché dans la partie éditable
- **Items** : liste des éléments disponibles
- **ItemIndex** : indice de l'élément sélectionné (-1 si aucun)
- **Style** : définit le comportement de la liste (csDropDown, csDropDownList, etc.)
- **DropDownCount** : nombre maximum d'éléments visibles quand la liste est déroulée
- **AutoComplete** : complète automatiquement le texte lors de la saisie
- **Sorted** : si activé, trie les éléments par ordre alphabétique

### Les différents styles de ComboBox

- **csDropDown** : permet la saisie libre et la sélection dans la liste
- **csDropDownList** : permet uniquement la sélection dans la liste (pas de saisie libre)
- **csSimple** : affiche toujours la liste (sans la dérouler)
- **csOwnerDrawFixed** : éléments de hauteur fixe avec dessin personnalisé
- **csOwnerDrawVariable** : éléments de hauteur variable avec dessin personnalisé

### Manipulation de la liste

```pascal
// Ajouter des éléments
ComboBox1.Items.Add('Élément 1');
ComboBox1.Items.Add('Élément 2');

// Ajouter plusieurs éléments d'un coup
ComboBox1.Items.AddStrings(['Option A', 'Option B', 'Option C']);

// Insérer un élément à une position spécifique
ComboBox1.Items.Insert(1, 'Nouvel élément');

// Supprimer un élément
ComboBox1.Items.Delete(2);

// Vider la liste
ComboBox1.Items.Clear;

// Sélectionner un élément par son indice
ComboBox1.ItemIndex := 0;  // Sélectionne le premier élément

// Sélectionner un élément par son texte
var idx := ComboBox1.Items.IndexOf('Option B');
if idx >= 0 then
  ComboBox1.ItemIndex := idx;
```

### Événements importants

- **OnChange** : déclenché lorsque la sélection ou le texte change
- **OnSelect** : déclenché uniquement lorsqu'un élément est sélectionné dans la liste
- **OnDropDown** : déclenché lorsque la liste se déroule
- **OnCloseUp** : déclenché lorsque la liste se referme

### Cas d'utilisation courants

- Sélection de catégories ou d'options prédéfinies
- Saisie avec suggestions (historique, valeurs fréquentes)
- Filtres dans une interface de recherche
- Sélection d'unités de mesure, de devises, etc.

### Astuces pour TComboBox

1. **Chargement dynamique** : Remplir la ComboBox à partir d'une table de base de données :

   ```pascal
   procedure TForm1.FormCreate(Sender: TObject);
   begin
     // Supposons que Query1 est un composant TFDQuery déjà configuré
     Query1.SQL.Text := 'SELECT NomPays FROM Pays ORDER BY NomPays';
     Query1.Open;

     ComboBoxPays.Items.Clear;
     while not Query1.Eof do
     begin
       ComboBoxPays.Items.Add(Query1.FieldByName('NomPays').AsString);
       Query1.Next;
     end;

     Query1.Close;
   end;
   ```

2. **ComboBox avec images** : Pour une ComboBox avec des images à côté du texte, utilisez `csOwnerDrawFixed` et un ImageList :

   ```pascal
   procedure TForm1.ComboBox1DrawItem(Control: TWinControl; Index: Integer;
     Rect: TRect; State: TOwnerDrawState);
   var
     ComboBox: TComboBox;
   begin
     ComboBox := Control as TComboBox;

     // Dessiner le fond
     if odSelected in State then
       ComboBox.Canvas.Brush.Color := clHighlight
     else
       ComboBox.Canvas.Brush.Color := ComboBox.Color;

     ComboBox.Canvas.FillRect(Rect);

     // Dessiner l'image
     ImageList1.Draw(ComboBox.Canvas, Rect.Left + 2, Rect.Top + 1, Index);

     // Dessiner le texte
     ComboBox.Canvas.TextOut(Rect.Left + ImageList1.Width + 6,
         Rect.Top + 2, ComboBox.Items[Index]);
   end;
   ```

## TCheckBox et TRadioButton - Les contrôles de choix

Bien que non mentionnés dans le titre, il est important de présenter brièvement deux autres contrôles de saisie très courants :

### Case à cocher (TCheckBox)

Le composant `TCheckBox` permet à l'utilisateur d'activer ou désactiver une option.

Propriétés importantes :
- **Checked** : état de la case (cochée ou non)
- **State** : état plus détaillé (cbUnchecked, cbChecked, cbGrayed)
- **AllowGrayed** : si activé, permet un état intermédiaire (grisé)
- **Caption** : texte affiché à côté de la case

```pascal
// Vérifier l'état d'une case à cocher
if CheckBoxEnvoiEmail.Checked then
  EnvoyerConfirmationEmail;
```

### Bouton radio (TRadioButton)

Le composant `TRadioButton` permet de sélectionner une option parmi plusieurs options mutuellement exclusives. Les boutons radio d'un même groupe s'excluent automatiquement.

Propriétés importantes :
- **Checked** : état du bouton (sélectionné ou non)
- **Caption** : texte affiché à côté du bouton

Pour créer un groupe de boutons radio :
1. Placez-les sur le même parent (formulaire ou panel)
2. Seul un bouton peut être sélectionné à la fois dans ce groupe
3. Pour avoir plusieurs groupes, utilisez des conteneurs différents (panels)

```pascal
// Déterminer quel bouton radio est sélectionné
var typeEnvoi: string;
if RadioButtonEmail.Checked then
  typeEnvoi := 'Email'
else if RadioButtonSMS.Checked then
  typeEnvoi := 'SMS'
else if RadioButtonCourrier.Checked then
  typeEnvoi := 'Courrier';
```

## Exemple concret : Formulaire de contact

Voici un exemple qui combine plusieurs contrôles de saisie pour créer un formulaire de contact :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisation de la ComboBox
  ComboBoxCivilite.Items.Clear;
  ComboBoxCivilite.Items.AddStrings(['M.', 'Mme', 'Dr.', 'Prof.']);
  ComboBoxCivilite.ItemIndex := 0;

  // Configuration du champ de saisie du téléphone
  EditTelephone.TextHint := 'Ex: 01 23 45 67 89';

  // Configuration du mémo
  MemoMessage.Lines.Clear;
  MemoMessage.TextHint := 'Entrez votre message ici...';
end;

procedure TForm1.ButtonEnvoyerClick(Sender: TObject);
begin
  // Validation basique
  if EditNom.Text = '' then
  begin
    ShowMessage('Veuillez entrer votre nom.');
    EditNom.SetFocus;
    Exit;
  end;

  if EditEmail.Text = '' then
  begin
    ShowMessage('Veuillez entrer votre email.');
    EditEmail.SetFocus;
    Exit;
  end;

  if MemoMessage.Text = '' then
  begin
    ShowMessage('Veuillez entrer un message.');
    MemoMessage.SetFocus;
    Exit;
  end;

  // Traitement du formulaire
  EnvoyerFormulaire(
    ComboBoxCivilite.Text,
    EditNom.Text,
    EditEmail.Text,
    MemoMessage.Text,
    CheckBoxCopie.Checked
  );

  // Confirmation
  ShowMessage('Votre message a bien été envoyé.');

  // Réinitialisation du formulaire
  EditNom.Clear;
  EditEmail.Clear;
  MemoMessage.Clear;
  CheckBoxCopie.Checked := False;
end;
```

## Conclusion

Les contrôles de saisie sont essentiels pour permettre aux utilisateurs d'interagir avec votre application. Chaque type de contrôle a ses propres caractéristiques et cas d'utilisation spécifiques :

- `TEdit` pour la saisie de texte simple sur une ligne
- `TMemo` pour la saisie ou l'affichage de texte multi-lignes
- `TComboBox` pour la sélection dans une liste avec option de saisie libre
- `TCheckBox` pour les options à activer/désactiver
- `TRadioButton` pour la sélection exclusive parmi plusieurs options

En maîtrisant ces contrôles fondamentaux, vous serez en mesure de créer des interfaces utilisateur interactives et conviviales pour vos applications Delphi.

Dans la prochaine section, nous explorerons les boutons et les actions, qui permettent aux utilisateurs de déclencher des opérations dans votre application.

---

*Exercice pratique : Créez un petit formulaire d'inscription avec des champs pour le nom, l'email, un mot de passe (avec confirmation), une liste déroulante pour le pays, et une zone de texte pour les commentaires. Ajoutez une validation basique pour vérifier que les champs obligatoires sont remplis et que les deux mots de passe correspondent.*

# 4.3.3 Boutons et actions

Les boutons sont parmi les composants les plus fondamentaux et les plus utilisés dans les interfaces utilisateur. Ils permettent aux utilisateurs de déclencher des actions dans votre application. Dans cette section, nous explorerons les différents types de boutons disponibles dans Delphi et le système d'actions qui vous permet d'organiser et de centraliser les fonctionnalités de votre application.

## Les boutons standard (TButton)

Le composant `TButton` est le type de bouton le plus courant et le plus simple. Il s'agit d'un bouton rectangulaire standard avec un texte.

### Création d'un bouton standard

Pour ajouter un bouton à votre formulaire :
1. Cliquez sur le composant `Button` dans l'onglet **Standard** de la palette de composants
2. Cliquez à l'endroit souhaité sur votre formulaire

### Propriétés importantes

- **Caption** : texte affiché sur le bouton
- **Enabled** : active ou désactive le bouton
- **Default** : si `True`, ce bouton sera activé lorsque l'utilisateur appuie sur la touche Entrée
- **Cancel** : si `True`, ce bouton sera activé lorsque l'utilisateur appuie sur la touche Échap
- **ModalResult** : valeur retournée lorsque le bouton est cliqué dans une boîte de dialogue modale
- **ImageIndex** et **Images** : permettent d'associer une image au bouton (via un ImageList)

### Événements principaux

- **OnClick** : déclenché lorsque l'utilisateur clique sur le bouton
- **OnMouseEnter**, **OnMouseLeave** : déclenchés lorsque la souris entre ou quitte la zone du bouton

### Exemple de code simple

```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);
var
  Nombre1, Nombre2, Resultat: Integer;
begin
  // Conversion des valeurs entrées
  Nombre1 := StrToIntDef(EditNombre1.Text, 0);
  Nombre2 := StrToIntDef(EditNombre2.Text, 0);

  // Calcul et affichage du résultat
  Resultat := Nombre1 + Nombre2;
  LabelResultat.Caption := 'Résultat: ' + IntToStr(Resultat);
end;
```

### Utilisation avec ModalResult

Le `ModalResult` est très utile pour les formulaires modaux (boîtes de dialogue) :

```pascal
// Configuration des boutons dans le formulaire de dialogue
ButtonOK.ModalResult := mrOk;
ButtonAnnuler.ModalResult := mrCancel;

// Dans le formulaire appelant
procedure TForm1.ButtonModifierClick(Sender: TObject);
var
  Formulaire: TFormEdition;
begin
  Formulaire := TFormEdition.Create(Self);
  try
    // Initialisation du formulaire
    Formulaire.EditNom.Text := NomActuel;

    // Affichage du formulaire et attente de la réponse
    if Formulaire.ShowModal = mrOk then
    begin
      // L'utilisateur a validé, traitement des données
      NomActuel := Formulaire.EditNom.Text;
      // Autres traitements...
    end;
    // Sinon, l'utilisateur a annulé, rien à faire
  finally
    Formulaire.Free;  // Libération de la mémoire
  end;
end;
```

## Autres types de boutons

### Bouton à bascule (TSpeedButton)

Le `TSpeedButton` est un bouton léger (sans fenêtre Windows) qui peut rester enfoncé. Il est idéal pour les barres d'outils.

Propriétés spécifiques :
- **GroupIndex** : permet de créer des groupes mutuellement exclusifs (comme des boutons radio)
- **AllowAllUp** : si `True`, tous les boutons du groupe peuvent être relevés
- **Down** : état enfoncé/relevé du bouton
- **Flat** : style plat (moderne) ou en relief (classique)

```pascal
// Création d'un groupe de SpeedButtons
SpeedButtonGras.GroupIndex := 1;
SpeedButtonItalique.GroupIndex := 2;
SpeedButtonSouligne.GroupIndex := 3;

// Gestion de l'état enfoncé/relevé
procedure TForm1.SpeedButtonGrasClick(Sender: TObject);
begin
  if SpeedButtonGras.Down then
    Memo1.SelAttributes.Style := Memo1.SelAttributes.Style + [fsBold]
  else
    Memo1.SelAttributes.Style := Memo1.SelAttributes.Style - [fsBold];
end;
```

### Bouton bitmap (TBitBtn)

Le `TBitBtn` est similaire au `TButton` mais avec des capacités d'affichage d'image intégrées, sans nécessiter d'ImageList.

Propriétés spécifiques :
- **Kind** : types prédéfinis (bkOK, bkCancel, bkHelp, etc.)
- **Glyph** : image personnalisée à afficher
- **Layout** : position de l'image par rapport au texte

```pascal
// Configuration d'un BitBtn
BitBtnValider.Kind := bkOK;  // Utilise l'icône OK prédéfinie
BitBtnValider.Caption := 'Valider';
```

### Bouton avec menu (TButtonMenu)

Disponible dans les versions récentes, ce bouton peut afficher un menu déroulant associé.

## Le système d'actions

Le système d'actions est l'une des fonctionnalités les plus puissantes de Delphi pour organiser le code d'une application. Il permet de :
- Centraliser le code associé aux commandes utilisateur
- Réutiliser les mêmes fonctionnalités à plusieurs endroits
- Synchroniser automatiquement l'état des contrôles

### Composants du système d'actions

#### TActionList

C'est le conteneur qui stocke toutes les actions de votre application.

Pour ajouter une liste d'actions :
1. Cliquez sur le composant `ActionList` dans l'onglet **Standard** de la palette
2. Placez-le sur votre formulaire (il sera invisible à l'exécution)
3. Double-cliquez sur le composant pour ouvrir l'éditeur d'actions

#### TAction

Une action représente une opération spécifique que l'utilisateur peut exécuter (comme Ouvrir, Enregistrer, Couper, Copier, etc.).

Propriétés importantes :
- **Caption** : texte affiché sur les contrôles associés
- **Hint** : info-bulle affichée
- **ShortCut** : raccourci clavier
- **ImageIndex** : indice de l'image dans une ImageList
- **Enabled** : active ou désactive l'action (et tous les contrôles associés)
- **Visible** : rend visible ou invisible tous les contrôles associés
- **Checked** : état coché/non coché (pour les menus et boutons à bascule)

Événements principaux :
- **OnExecute** : code exécuté lorsque l'action est déclenchée
- **OnUpdate** : code pour mettre à jour l'état de l'action (enabled, checked, etc.)

### Création et utilisation d'une action

#### 1. Créer une action

1. Double-cliquez sur votre `ActionList` pour ouvrir l'éditeur
2. Cliquez sur le bouton + pour ajouter une nouvelle action
3. Définissez ses propriétés (Caption, Hint, ShortCut, etc.)
4. Double-cliquez sur l'action pour créer son gestionnaire `OnExecute`

```pascal
procedure TForm1.ActionNouveauExecute(Sender: TObject);
begin
  // Code pour créer un nouveau document
  MemoTexte.Clear;
  NomFichierActuel := '';
  StatusBar1.Panels[0].Text := 'Nouveau document';
end;
```

#### 2. Créer un gestionnaire OnUpdate

```pascal
procedure TForm1.ActionEnregistrerUpdate(Sender: TObject);
begin
  // L'action Enregistrer est disponible uniquement si le document a été modifié
  ActionEnregistrer.Enabled := DocumentModifie;
end;
```

#### 3. Associer l'action à des contrôles

Pour associer une action à un bouton, menu ou autre contrôle :
1. Sélectionnez le contrôle (par exemple, un bouton)
2. Dans l'Inspecteur d'objets, définissez la propriété `Action` avec l'action souhaitée

Le bouton héritera automatiquement de toutes les propriétés de l'action (Caption, Enabled, Hint, etc.).

### Exemple complet : Éditeur de texte simple

Voici un exemple d'utilisation des actions pour créer un éditeur de texte simple :

```pascal
// Dans la section privée de la classe
private
  NomFichierActuel: string;
  DocumentModifie: Boolean;
  procedure MajTitreFenetre;
  procedure SetDocumentModifie(const Value: Boolean);

// Procédure pour mettre à jour le titre de la fenêtre
procedure TForm1.MajTitreFenetre;
begin
  if NomFichierActuel = '' then
    Caption := 'Document sans titre - Éditeur'
  else
    Caption := ExtractFileName(NomFichierActuel) + ' - Éditeur';

  if DocumentModifie then
    Caption := '* ' + Caption;
end;

// Procédure pour définir l'état de modification
procedure TForm1.SetDocumentModifie(const Value: Boolean);
begin
  if DocumentModifie <> Value then
  begin
    DocumentModifie := Value;
    MajTitreFenetre;
  end;
end;

// Action Nouveau
procedure TForm1.ActionNouveauExecute(Sender: TObject);
begin
  if DocumentModifie then
  begin
    case MessageDlg('Voulez-vous enregistrer les modifications?',
            mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActionEnregistrer.Execute;
      mrCancel: Exit;
    end;
  end;

  MemoTexte.Clear;
  NomFichierActuel := '';
  SetDocumentModifie(False);
end;

// Action Ouvrir
procedure TForm1.ActionOuvrirExecute(Sender: TObject);
begin
  if DocumentModifie then
  begin
    case MessageDlg('Voulez-vous enregistrer les modifications?',
            mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: ActionEnregistrer.Execute;
      mrCancel: Exit;
    end;
  end;

  if OpenDialog1.Execute then
  begin
    MemoTexte.Lines.LoadFromFile(OpenDialog1.FileName);
    NomFichierActuel := OpenDialog1.FileName;
    SetDocumentModifie(False);
  end;
end;

// Action Enregistrer
procedure TForm1.ActionEnregistrerExecute(Sender: TObject);
begin
  if NomFichierActuel = '' then
    ActionEnregistrerSous.Execute
  else
  begin
    MemoTexte.Lines.SaveToFile(NomFichierActuel);
    SetDocumentModifie(False);
  end;
end;

// Action Enregistrer Sous
procedure TForm1.ActionEnregistrerSousExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    MemoTexte.Lines.SaveToFile(SaveDialog1.FileName);
    NomFichierActuel := SaveDialog1.FileName;
    SetDocumentModifie(False);
  end;
end;

// Mise à jour de l'action Enregistrer
procedure TForm1.ActionEnregistrerUpdate(Sender: TObject);
begin
  ActionEnregistrer.Enabled := DocumentModifie;
end;

// Détection des modifications
procedure TForm1.MemoTexteChange(Sender: TObject);
begin
  SetDocumentModifie(True);
end;
```

## ActionManager et barres d'outils/menus

Pour les applications plus complexes, Delphi propose un système encore plus puissant :

### TActionManager

Le `TActionManager` étend le concept d'ActionList en permettant d'organiser les actions en catégories et de gérer automatiquement barres d'outils et menus.

Composants associés :
- `TActionMainMenuBar` : barre de menu pilotée par les actions
- `TActionToolBar` : barre d'outils pilotée par les actions

Avantages :
- Synchronisation automatique menus/barres d'outils
- Personnalisation par l'utilisateur final
- Organisation plus claire pour le développeur

## Astuces et bonnes pratiques

### 1. Organisation des actions

Organisez vos actions en catégories logiques :
- Fichier (Nouveau, Ouvrir, Enregistrer...)
- Édition (Couper, Copier, Coller...)
- Affichage (Zoom, Plein écran...)
- Outils (Options, Préférences...)

### 2. Nommage cohérent

Utilisez un préfixe cohérent pour vos actions :
```
ActionNouveau
ActionOuvrir
ActionEnregistrer
```

### 3. Gestionnaires OnUpdate

N'oubliez pas d'implémenter les gestionnaires `OnUpdate` pour maintenir l'état des actions cohérent avec l'état de l'application.

### 4. Images cohérentes

Utilisez un `TImageList` pour stocker toutes les images de vos actions et assurez-vous qu'elles suivent un style visuel cohérent.

### 5. Raccourcis clavier standards

Respectez les raccourcis clavier standards pour une meilleure expérience utilisateur :
- Ctrl+N : Nouveau
- Ctrl+O : Ouvrir
- Ctrl+S : Enregistrer
- Ctrl+X : Couper
- Ctrl+C : Copier
- Ctrl+V : Coller
- F1 : Aide

## Conclusion

Les boutons et le système d'actions sont des éléments fondamentaux pour créer des interfaces utilisateur interactives et bien organisées. Le système d'actions en particulier, bien qu'il nécessite un peu plus d'effort initial, offre des avantages considérables en termes de maintenabilité et de cohérence pour les applications de taille moyenne à grande.

En maîtrisant ces concepts, vous pourrez créer des applications plus professionnelles et plus faciles à maintenir.

Dans la prochaine section, nous explorerons les listes et grilles, qui permettent d'afficher et de manipuler des ensembles de données.

---

*Exercice pratique : Créez une petite application de prise de notes avec des boutons pour Nouveau, Ouvrir, Enregistrer et Quitter. Implémentez ces fonctionnalités en utilisant des actions et assurez-vous que l'état des boutons reflète correctement l'état de l'application (par exemple, Enregistrer est désactivé si aucune modification n'a été effectuée).*

# 4.3.4 Listes et grilles

Les listes et les grilles sont des composants essentiels pour afficher des collections d'éléments dans votre application. Ils permettent aux utilisateurs de visualiser, sélectionner et parfois modifier des ensembles de données. Dans cette section, nous explorerons les différents types de listes et de grilles disponibles dans Delphi.

## Les listes simples (TListBox)

Le composant `TListBox` permet d'afficher une liste simple d'éléments textuels parmi lesquels l'utilisateur peut sélectionner un ou plusieurs éléments.

### Création d'une liste

Pour ajouter une liste à votre formulaire :
1. Cliquez sur le composant `ListBox` dans l'onglet **Standard** de la palette de composants
2. Dessinez-le à la taille souhaitée sur votre formulaire

### Propriétés importantes

- **Items** : collection d'éléments textuels de la liste
- **ItemIndex** : indice de l'élément sélectionné (-1 si aucun)
- **MultiSelect** : permet la sélection multiple
- **ExtendedSelect** : active la sélection étendue avec Shift et Ctrl
- **Sorted** : trie automatiquement les éléments par ordre alphabétique
- **Style** : style d'affichage (standard, propriétaire, variable)
- **TabStop** : permet de naviguer avec la touche Tab

### Manipulation des éléments

```pascal
// Ajouter des éléments
ListBox1.Items.Add('Élément 1');
ListBox1.Items.Add('Élément 2');

// Ajouter plusieurs éléments d'un coup
ListBox1.Items.AddStrings(['Option A', 'Option B', 'Option C']);

// Insérer un élément à une position spécifique
ListBox1.Items.Insert(1, 'Nouvel élément');

// Supprimer un élément
ListBox1.Items.Delete(2);

// Vider la liste
ListBox1.Items.Clear;

// Sélectionner un élément
ListBox1.ItemIndex := 0;  // Sélectionne le premier élément

// Vérifier si un élément existe
if ListBox1.Items.IndexOf('Option B') >= 0 then
  ShowMessage('L''élément existe');
```

### Gestion des sélections

```pascal
// Obtenir l'élément sélectionné
if ListBox1.ItemIndex >= 0 then
  ShowMessage('Sélectionné: ' + ListBox1.Items[ListBox1.ItemIndex]);

// Dans une liste à sélection multiple
var
  i: Integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    if ListBox1.Selected[i] then
      ShowMessage('Sélectionné: ' + ListBox1.Items[i]);
end;
```

### Événements importants

- **OnClick** : déclenché lorsque l'utilisateur clique sur un élément
- **OnDblClick** : déclenché lors d'un double-clic sur un élément
- **OnSelectionChange** : déclenché lorsque la sélection change
- **OnDrawItem** : pour personnaliser l'affichage (en mode propriétaire)

### Exemple : Liste des fichiers d'un répertoire

```pascal
procedure TForm1.ButtonListerClick(Sender: TObject);
var
  SearchRec: TSearchRec;
  Result: Integer;
begin
  ListBox1.Items.Clear;

  Result := FindFirst('C:\*.*', faAnyFile, SearchRec);
  try
    while Result = 0 do
    begin
      // Exclure . et ..
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        ListBox1.Items.Add(SearchRec.Name);

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;
```

## Les listes à cases à cocher (TCheckListBox)

Le composant `TCheckListBox` étend la fonctionnalité de `TListBox` en ajoutant une case à cocher à côté de chaque élément.

### Propriétés spécifiques

- **Checked[Index]** : état coché/non coché d'un élément
- **State[Index]** : état plus détaillé (non coché, coché, grisé)
- **AllowGrayed** : autorise l'état grisé
- **Header[Index]** : identifie les éléments d'en-tête (non cochables)

### Exemple d'utilisation

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  CheckListBox1.Items.Clear;
  CheckListBox1.Items.Add('Option 1');
  CheckListBox1.Items.Add('Option 2');
  CheckListBox1.Items.Add('Option 3');

  // Cocher par défaut la première option
  CheckListBox1.Checked[0] := True;
end;

procedure TForm1.ButtonAppliquerClick(Sender: TObject);
var
  i: Integer;
  OptionsSelectionnees: string;
begin
  OptionsSelectionnees := 'Options sélectionnées:';

  for i := 0 to CheckListBox1.Items.Count - 1 do
    if CheckListBox1.Checked[i] then
      OptionsSelectionnees := OptionsSelectionnees + #13#10 + '- ' + CheckListBox1.Items[i];

  ShowMessage(OptionsSelectionnees);
end;
```

## Les listes déroulantes (TComboBox)

Nous avons déjà étudié `TComboBox` dans la section sur les contrôles de saisie, mais rappelons qu'il s'agit d'un composant hybride qui combine un champ de texte et une liste déroulante.

## Les vues en liste (TListView)

Le composant `TListView` offre une liste beaucoup plus puissante et flexible que `TListBox`. Il peut afficher des éléments avec des icônes et dans différentes vues : icônes, détails (colonnes), petites icônes, liste.

### Propriétés importantes

- **ViewStyle** : style d'affichage (vsIcon, vsSmallIcon, vsList, vsReport)
- **Items** : collection des éléments de la liste
- **Columns** : collection des colonnes (pour la vue rapport)
- **Icons** et **SmallImages** : ImageLists pour les grandes et petites icônes

### Manipulation des éléments

```pascal
// Ajouter un élément simple
ListItem := ListView1.Items.Add;
ListItem.Caption := 'Élément 1';
ListItem.ImageIndex := 0;  // Indice dans l'ImageList

// Ajouter un élément avec sous-éléments (colonnes)
ListItem := ListView1.Items.Add;
ListItem.Caption := 'Fichier 1';  // Première colonne
ListItem.SubItems.Add('10 Ko');   // Deuxième colonne
ListItem.SubItems.Add('28/04/2025'); // Troisième colonne
```

### Exemple : Liste de fichiers avec détails

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration des colonnes
  ListView1.ViewStyle := vsReport;

  with ListView1.Columns.Add do
  begin
    Caption := 'Nom du fichier';
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
    Caption := 'Date de modification';
    Width := 150;
  end;
end;

procedure TForm1.ButtonListerClick(Sender: TObject);
var
  SearchRec: TSearchRec;
  Result: Integer;
  ListItem: TListItem;
begin
  ListView1.Items.Clear;

  Result := FindFirst('C:\*.*', faAnyFile, SearchRec);
  try
    while Result = 0 do
    begin
      // Exclure . et ..
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        ListItem := ListView1.Items.Add;
        ListItem.Caption := SearchRec.Name;
        ListItem.SubItems.Add(FormatFloat('#,##0 Ko', SearchRec.Size / 1024));
        ListItem.SubItems.Add(DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

        // Définir l'icône en fonction du type de fichier
        if (SearchRec.Attr and faDirectory) = faDirectory then
          ListItem.ImageIndex := 0  // Icône de dossier
        else
          ListItem.ImageIndex := 1; // Icône de fichier
      end;

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;
```

## Les vues en arbre (TTreeView)

Le composant `TTreeView` permet d'afficher des données hiérarchiques sous forme d'arborescence avec des nœuds que l'utilisateur peut développer ou réduire.

### Propriétés importantes

- **Items** : collection des nœuds de l'arbre
- **Selected** : nœud actuellement sélectionné
- **Images** et **StateImages** : ImageLists pour les icônes
- **ShowLines** : affiche des lignes entre les nœuds
- **ShowRoot** : affiche la ligne du nœud racine
- **ShowButtons** : affiche les boutons +/- pour développer/réduire
- **HideSelection** : masque/affiche la sélection quand le contrôle perd le focus

### Manipulation des nœuds

```pascal
// Ajouter un nœud racine
RootNode := TreeView1.Items.Add(nil, 'Racine');

// Ajouter un enfant à un nœud
ChildNode := TreeView1.Items.AddChild(RootNode, 'Enfant 1');

// Ajouter un nœud après un autre
NextNode := TreeView1.Items.Add(ChildNode, 'Frère de Enfant 1');

// Définir l'état déployé/replié
RootNode.Expanded := True;  // Déployer
ChildNode.Expanded := False; // Replier

// Définir une icône
RootNode.ImageIndex := 0;       // Icône normale
RootNode.SelectedIndex := 1;    // Icône quand sélectionné
```

### Exemple : Explorateur de fichiers simple

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  RootNode: TTreeNode;
begin
  TreeView1.Items.Clear;

  // Ajouter le nœud "Poste de travail"
  RootNode := TreeView1.Items.Add(nil, 'Poste de travail');
  RootNode.ImageIndex := 0;
  RootNode.SelectedIndex := 0;

  // Ajouter les lecteurs
  AjouterLecteurs(RootNode);

  // Développer le nœud racine
  RootNode.Expand(False);
end;

procedure TForm1.AjouterLecteurs(ParentNode: TTreeNode);
var
  DriveBits: set of 0..25;
  Drive: Char;
  DriveNode: TTreeNode;
  DriveName: string;
begin
  Integer(DriveBits) := GetLogicalDrives;

  for Drive := 'A' to 'Z' do
    if (Ord(Drive) - Ord('A')) in DriveBits then
    begin
      DriveName := Drive + ':\';

      // Déterminer le type de lecteur
      case GetDriveType(PChar(DriveName)) of
        DRIVE_REMOVABLE: DriveName := DriveName + ' (Disque amovible)';
        DRIVE_FIXED: DriveName := DriveName + ' (Disque local)';
        DRIVE_REMOTE: DriveName := DriveName + ' (Lecteur réseau)';
        DRIVE_CDROM: DriveName := DriveName + ' (CD/DVD)';
      end;

      DriveNode := TreeView1.Items.AddChild(ParentNode, DriveName);
      DriveNode.ImageIndex := 1;  // Icône de disque
      DriveNode.SelectedIndex := 1;

      // Ajouter un nœud factice pour permettre l'expansion
      TreeView1.Items.AddChild(DriveNode, 'Chargement...');
    end;
end;

procedure TForm1.TreeView1Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
begin
  // Si le nœud n'a qu'un enfant "Chargement...", charger le contenu réel
  if (Node.Count = 1) and (Node.Item[0].Text = 'Chargement...') then
  begin
    // Supprimer le nœud factice
    Node.Item[0].Delete;

    // Charger le contenu du répertoire
    ChargerRepertoire(Node);
  end;
end;

procedure TForm1.ChargerRepertoire(ParentNode: TTreeNode);
var
  Path: string;
  SearchRec: TSearchRec;
  Result: Integer;
  NewNode: TTreeNode;
begin
  // Extraire le chemin du nœud parent
  Path := ExtraireCheminNode(ParentNode);

  if Path = '' then Exit;

  // Lister les sous-répertoires
  Result := FindFirst(Path + '*.*', faDirectory, SearchRec);
  try
    while Result = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) = faDirectory) and
         (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        NewNode := TreeView1.Items.AddChild(ParentNode, SearchRec.Name);
        NewNode.ImageIndex := 2;  // Icône de dossier
        NewNode.SelectedIndex := 3;  // Icône de dossier ouvert

        // Ajouter un nœud factice
        TreeView1.Items.AddChild(NewNode, 'Chargement...');
      end;

      Result := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

function TForm1.ExtraireCheminNode(Node: TTreeNode): string;
var
  Path: string;
begin
  // Cas spécial pour le nœud racine
  if Node.Level = 0 then
    Exit('');

  // Pour un lecteur, extraire juste la lettre
  if Node.Level = 1 then
  begin
    Path := Node.Text;
    Result := Path[1] + ':\';
    Exit;
  end;

  // Pour les autres nœuds, reconstruire le chemin complet
  Path := Node.Text;
  Node := Node.Parent;

  while (Node <> nil) and (Node.Level > 0) do
  begin
    if Node.Level = 1 then
      // Cas spécial pour le nœud de lecteur
      Path := Node.Text[1] + ':\'+ Path
    else
      Path := Node.Text + '\' + Path;

    Node := Node.Parent;
  end;

  Result := Path + '\';
end;
```

## Les grilles simples (TStringGrid)

Le composant `TStringGrid` permet d'afficher et d'éditer des données tabulaires (lignes et colonnes) de type texte.

### Propriétés importantes

- **ColCount** et **RowCount** : nombre de colonnes et de lignes
- **FixedCols** et **FixedRows** : nombre de colonnes et lignes fixes (en-têtes)
- **DefaultColWidth** et **DefaultRowHeight** : largeur et hauteur par défaut
- **ColWidths[Index]** et **RowHeights[Index]** : largeur/hauteur spécifiques
- **Options** : ensemble d'options pour personnaliser le comportement
- **Cells[ACol, ARow]** : accès aux cellules individuelles

### Manipulation des cellules

```pascal
// Définir des titres de colonnes
StringGrid1.Cells[0, 0] := 'Nom';
StringGrid1.Cells[1, 0] := 'Prénom';
StringGrid1.Cells[2, 0] := 'Âge';

// Remplir une cellule
StringGrid1.Cells[0, 1] := 'Dupont';
StringGrid1.Cells[1, 1] := 'Jean';
StringGrid1.Cells[2, 1] := '42';

// Lire une cellule
ShowMessage(StringGrid1.Cells[0, 1]);

// Effacer une cellule
StringGrid1.Cells[1, 2] := '';

// Effacer toute la grille
StringGrid1.Clean;
```

### Exemple : Tableau de multiplication

```pascal
procedure TForm1.ButtonGenererClick(Sender: TObject);
var
  i, j: Integer;
begin
  // Configuration de la grille
  StringGrid1.ColCount := 11;  // 0-10
  StringGrid1.RowCount := 11;  // 0-10

  // En-têtes de ligne et colonne
  StringGrid1.Cells[0, 0] := 'X';

  for i := 1 to 10 do
  begin
    StringGrid1.Cells[i, 0] := IntToStr(i);
    StringGrid1.Cells[0, i] := IntToStr(i);
  end;

  // Remplir la table de multiplication
  for i := 1 to 10 do
    for j := 1 to 10 do
      StringGrid1.Cells[i, j] := IntToStr(i * j);
end;
```

## Les grilles liées aux données (TDBGrid)

Pour afficher et modifier des données provenant d'une base de données, Delphi propose le composant `TDBGrid`, que nous étudierons plus en détail dans le chapitre sur l'accès aux bases de données.

## Astuces générales pour les listes et grilles

### 1. Éviter les mises à jour visuelles inutiles

Lorsque vous effectuez plusieurs opérations sur une liste ou une grille, utilisez `BeginUpdate` et `EndUpdate` pour éviter les scintillements et améliorer les performances :

```pascal
ListBox1.Items.BeginUpdate;
try
  // Ajouter beaucoup d'éléments...
finally
  ListBox1.Items.EndUpdate;
end;
```

### 2. Tri personnalisé

Pour trier des éléments selon des critères spécifiques, vous pouvez utiliser la méthode `CustomSort` pour les contrôles qui la supportent, ou trier manuellement la liste source avant de la charger :

```pascal
// Trier une TStringList avant de la charger dans une ListBox
procedure TForm1.ChargerTriAlphabetique;
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Remplir la liste
    Liste.Add('Poire');
    Liste.Add('Pomme');
    Liste.Add('Banane');

    // Trier alphabétiquement
    Liste.Sort;

    // Charger dans la ListBox
    ListBox1.Items.Assign(Liste);
  finally
    Liste.Free;
  end;
end;
```

### 3. Recherche rapide dans les listes

Implementez une recherche incrémentale dans vos listes :

```pascal
procedure TForm1.EditRechercheChange(Sender: TObject);
var
  Recherche: string;
  i: Integer;
begin
  Recherche := LowerCase(EditRecherche.Text);

  if Recherche = '' then Exit;

  // Rechercher le premier élément correspondant
  for i := 0 to ListBox1.Items.Count - 1 do
    if Pos(Recherche, LowerCase(ListBox1.Items[i])) = 1 then
    begin
      ListBox1.ItemIndex := i;
      Break;
    end;
end;
```

### 4. Données associées aux éléments

Pour associer des données supplémentaires aux éléments d'une liste, vous pouvez utiliser la propriété `Objects` :

```pascal
// Associer un ID à chaque élément
procedure TForm1.ChargerClientsAvecID;
var
  ID: Integer;
begin
  // Supposons que nous ayons des données client
  ListBox1.Clear;

  // Client 1
  ID := 101;
  ListBox1.Items.AddObject('Dupont Jean', TObject(ID));

  // Client 2
  ID := 102;
  ListBox1.Items.AddObject('Martin Sophie', TObject(ID));

  // ...
end;

// Récupérer l'ID du client sélectionné
procedure TForm1.ListBox1Click(Sender: TObject);
var
  ClientID: Integer;
begin
  if ListBox1.ItemIndex >= 0 then
  begin
    ClientID := Integer(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    ShowMessage('ID du client: ' + IntToStr(ClientID));
  end;
end;
```

## Conclusion

Les listes et les grilles sont des composants très puissants qui permettent d'afficher et de manipuler des ensembles de données de différentes manières. Chaque type de contrôle a ses propres caractéristiques et cas d'utilisation :

- `TListBox` pour les listes simples
- `TCheckListBox` pour les listes avec cases à cocher
- `TListView` pour les listes plus riches avec des icônes et des colonnes
- `TTreeView` pour les données hiérarchiques
- `TStringGrid` pour les données tabulaires

En maîtrisant ces composants, vous pourrez créer des interfaces utilisateur capables de présenter efficacement différents types d'informations à vos utilisateurs.

Dans la prochaine section, nous explorerons les contrôles avancés de la VCL, qui offrent encore plus de possibilités pour enrichir vos interfaces utilisateur.

---

*Exercice pratique : Créez une application simple qui affiche le contenu d'un répertoire dans un TListView avec trois colonnes (Nom, Taille, Date de modification). Ajoutez un TComboBox pour filtrer les fichiers par extension et un TEdit pour effectuer une recherche par nom.*
