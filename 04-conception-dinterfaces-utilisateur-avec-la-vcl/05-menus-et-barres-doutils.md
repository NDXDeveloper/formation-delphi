# 4.5 Menus et barres d'outils

Les menus et barres d'outils sont des éléments essentiels dans toute application Windows. Ils permettent d'organiser les fonctionnalités de votre application de manière accessible et cohérente. Dans cette section, nous allons découvrir comment créer et personnaliser des menus et des barres d'outils dans Delphi.

## Les menus (TMainMenu et TPopupMenu)

Delphi propose deux types principaux de menus :
- `TMainMenu` : le menu principal, situé en haut de la fenêtre
- `TPopupMenu` : menu contextuel qui apparaît lors d'un clic droit

### Le menu principal (TMainMenu)

#### Création d'un menu principal

Pour ajouter un menu principal à votre formulaire :

1. Cliquez sur le composant `MainMenu` dans l'onglet **Standard** de la palette de composants
2. Placez-le sur votre formulaire (il sera invisible à l'exécution)
3. Double-cliquez sur le composant pour ouvrir l'éditeur de menu

L'éditeur de menu est un outil visuel qui vous permet de créer et d'organiser facilement les entrées de votre menu.

#### Utilisation de l'éditeur de menu

- Cliquez sur "Insérer" pour ajouter une entrée
- Saisissez le texte de l'entrée dans la propriété `Caption`
- Pour créer un sous-menu, cliquez sur la flèche droite
- Pour revenir au niveau supérieur, cliquez sur la flèche gauche
- Pour ajouter une entrée au même niveau, cliquez sur la flèche bas

#### Propriétés importantes des éléments de menu

- **Caption** : texte affiché dans le menu
- **Name** : nom utilisé pour accéder à l'élément dans le code
- **ShortCut** : raccourci clavier associé
- **Enabled** : active ou désactive l'élément
- **Visible** : rend l'élément visible ou invisible
- **Checked** : affiche ou non une coche à côté de l'élément
- **ImageIndex** : index de l'image à afficher (depuis un `TImageList`)
- **Break** : permet de créer un saut de ligne dans le menu

#### Conventions pour les menus

Voici quelques bonnes pratiques pour vos menus :

1. **Structure standard** : suivez la structure habituelle des applications Windows
   - Fichier, Édition, Affichage, Outils, Aide...

2. **Raccourcis clavier standards**
   - Fichier > Nouveau : Ctrl+N
   - Fichier > Ouvrir : Ctrl+O
   - Fichier > Enregistrer : Ctrl+S
   - Édition > Couper : Ctrl+X
   - Édition > Copier : Ctrl+C
   - Édition > Coller : Ctrl+V

3. **Séparateurs** : utilisez des séparateurs (traits horizontaux) pour regrouper logiquement les commandes
   - Pour ajouter un séparateur, créez un élément avec `-` comme Caption

4. **Accélérateurs** : permettent d'accéder à un menu avec le clavier
   - Placez un caractère `&` avant la lettre qui servira d'accélérateur
   - Exemple : `&Fichier` permet d'accéder au menu Fichier avec Alt+F

#### Exemple de code pour gérer les événements de menu

```pascal
procedure TForm1.MenuNouveauClick(Sender: TObject);
begin
  // Code pour créer un nouveau document
  Memo1.Clear;
  Caption := 'Nouveau document - Mon Éditeur';
end;

procedure TForm1.MenuOuvrirClick(Sender: TObject);
begin
  // Ouvrir un fichier
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    Caption := ExtractFileName(OpenDialog1.FileName) + ' - Mon Éditeur';
  end;
end;

procedure TForm1.MenuEnregistrerClick(Sender: TObject);
begin
  // Enregistrer le fichier
  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    Caption := ExtractFileName(SaveDialog1.FileName) + ' - Mon Éditeur';
  end;
end;

procedure TForm1.MenuQuitterClick(Sender: TObject);
begin
  // Fermer l'application
  Close;
end;
```

### Menus contextuels (TPopupMenu)

Les menus contextuels apparaissent lorsque l'utilisateur fait un clic droit sur un contrôle ou une zone de votre application.

#### Création d'un menu contextuel

1. Cliquez sur le composant `PopupMenu` dans l'onglet **Standard** de la palette
2. Placez-le sur votre formulaire
3. Double-cliquez dessus pour ouvrir l'éditeur de menu et créez vos entrées
4. Associez le menu au contrôle souhaité via sa propriété `PopupMenu`

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Associer le menu contextuel au mémo
  Memo1.PopupMenu := PopupMenu1;
end;
```

#### Exemple de menu contextuel pour un éditeur de texte

```pascal
procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
  // Activer/désactiver les options selon le contexte
  MenuContextCouper.Enabled := Memo1.SelLength > 0;
  MenuContextCopier.Enabled := Memo1.SelLength > 0;
  MenuContextColler.Enabled := Clipboard.HasFormat(CF_TEXT);
end;

procedure TForm1.MenuContextCouperClick(Sender: TObject);
begin
  Memo1.CutToClipboard;
end;

procedure TForm1.MenuContextCopierClick(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TForm1.MenuContextCollerClick(Sender: TObject);
begin
  Memo1.PasteFromClipboard;
end;
```

### Menus dynamiques

Parfois, vous aurez besoin de créer ou modifier des menus à l'exécution, par exemple pour afficher une liste de fichiers récents.

#### Exemple : Liste des fichiers récents

```pascal
procedure TForm1.MettreAJourMenuFichiersRecents;
var
  i: Integer;
  MenuItem: TMenuItem;
begin
  // Effacer les anciens éléments
  while MenuFichiersRecents.Count > 0 do
    MenuFichiersRecents.Delete(0);

  // Si la liste est vide, ajouter un élément désactivé
  if FichiersRecents.Count = 0 then
  begin
    MenuItem := TMenuItem.Create(MenuFichiersRecents);
    MenuItem.Caption := '(Aucun fichier récent)';
    MenuItem.Enabled := False;
    MenuFichiersRecents.Add(MenuItem);
  end
  else
  begin
    // Ajouter les fichiers récents
    for i := 0 to FichiersRecents.Count - 1 do
    begin
      MenuItem := TMenuItem.Create(MenuFichiersRecents);
      MenuItem.Caption := '&' + IntToStr(i + 1) + ' ' + FichiersRecents[i];
      MenuItem.Tag := i;  // Stocker l'indice dans Tag
      MenuItem.OnClick := MenuFichierRecentClick;
      MenuFichiersRecents.Add(MenuItem);
    end;

    // Ajouter un séparateur et une option pour effacer la liste
    MenuItem := TMenuItem.Create(MenuFichiersRecents);
    MenuItem.Caption := '-';
    MenuFichiersRecents.Add(MenuItem);

    MenuItem := TMenuItem.Create(MenuFichiersRecents);
    MenuItem.Caption := 'Effacer la liste';
    MenuItem.OnClick := MenuEffacerFichiersRecentsClick;
    MenuFichiersRecents.Add(MenuItem);
  end;
end;

procedure TForm1.MenuFichierRecentClick(Sender: TObject);
var
  Indice: Integer;
begin
  Indice := (Sender as TMenuItem).Tag;
  if (Indice >= 0) and (Indice < FichiersRecents.Count) then
    OuvrirFichier(FichiersRecents[Indice]);
end;
```

## Les barres d'outils (TToolBar et TToolButton)

Les barres d'outils offrent un accès rapide aux fonctions les plus utilisées de votre application. Dans Delphi, elles sont créées principalement avec les composants `TToolBar` et `TToolButton`.

### Création d'une barre d'outils

1. Cliquez sur le composant `ToolBar` dans l'onglet **Win32** de la palette
2. Placez-le en haut de votre formulaire (généralement sous le menu principal)
3. Définissez ses propriétés (par exemple, `ShowCaptions` pour afficher le texte des boutons)
4. Ajoutez des boutons soit par code, soit en cliquant droit sur la barre d'outils et en choisissant "Nouveau bouton"

### Propriétés importantes de TToolBar

- **AutoSize** : ajuste automatiquement la hauteur selon les boutons
- **Flat** : style plat (moderne) ou en relief (classique)
- **Transparent** : rend le fond transparent
- **ShowCaptions** : affiche le texte sous les icônes
- **Images** : ImageList contenant les icônes des boutons
- **EdgeBorders** : bordures à afficher (haut, bas, gauche, droite)
- **EdgeInner** et **EdgeOuter** : style des bordures internes et externes

### Propriétés des boutons de la barre d'outils (TToolButton)

- **Caption** : texte affiché (si ShowCaptions est activé)
- **ImageIndex** : indice de l'icône dans l'ImageList
- **Style** : type de bouton (normal, séparateur, déroulant, etc.)
- **Grouped** et **AllowAllUp** : pour créer des groupes de boutons exclusifs ou non

### Exemple : Création d'une barre d'outils par code

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  Button: TToolButton;
begin
  // Configuration de la barre d'outils
  ToolBar1.Images := ImageList1;
  ToolBar1.ShowCaptions := True;
  ToolBar1.Flat := True;

  // Bouton Nouveau
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Caption := 'Nouveau';
  Button.ImageIndex := 0;  // Indice de l'icône dans l'ImageList
  Button.OnClick := MenuNouveauClick;  // Réutiliser le gestionnaire du menu

  // Bouton Ouvrir
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Caption := 'Ouvrir';
  Button.ImageIndex := 1;
  Button.OnClick := MenuOuvrirClick;

  // Séparateur
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Style := tbsSeparator;
  Button.Width := 8;

  // Bouton Enregistrer
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Caption := 'Enregistrer';
  Button.ImageIndex := 2;
  Button.OnClick := MenuEnregistrerClick;
end;
```

### Synchronisation avec les actions

Si vous utilisez le système d'actions (voir section 4.3.3), vous pouvez associer vos boutons de barre d'outils directement aux actions via la propriété `Action`. Cela permet de synchroniser automatiquement l'état, le texte et l'icône du bouton avec l'action correspondante.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
var
  Button: TToolButton;
begin
  // Configuration de la barre d'outils
  ToolBar1.Images := ImageList1;
  ToolBar1.ShowCaptions := True;

  // Bouton Nouveau associé à une action
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Action := ActionNouveau;  // Association à une action

  // Bouton Ouvrir associé à une action
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Action := ActionOuvrir;

  // Séparateur
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Style := tbsSeparator;
  Button.Width := 8;

  // Bouton Enregistrer associé à une action
  Button := TToolButton.Create(Self);
  Button.Parent := ToolBar1;
  Button.Action := ActionEnregistrer;
end;
```

### Création visuelle (à la conception)

La méthode la plus simple pour créer une barre d'outils consiste à utiliser l'éditeur visuel :

1. Déposez un composant `TToolBar` sur votre formulaire
2. Cliquez avec le bouton droit dessus et choisissez "Nouveau bouton"
3. Configurez le bouton dans l'Inspecteur d'objets
4. Répétez l'opération pour ajouter d'autres boutons
5. Pour ajouter un séparateur, créez un nouveau bouton et changez sa propriété `Style` en `tbsSeparator`

## Barres d'état (TStatusBar)

Une barre d'état, située généralement en bas de la fenêtre, permet d'afficher des informations sur l'état actuel de l'application.

### Création d'une barre d'état

1. Cliquez sur le composant `StatusBar` dans l'onglet **Win32** de la palette
2. Placez-le en bas de votre formulaire

### Propriétés importantes

- **SimplePanel** : mode simple (un seul panneau) ou multi-panneaux
- **SimpleText** : texte affiché en mode simple
- **Panels** : collection de panneaux en mode multi-panneaux

### Utilisation de panneaux multiples

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration de la barre d'état
  StatusBar1.SimplePanel := False;  // Mode multi-panneaux

  // Créer les panneaux
  with StatusBar1.Panels.Add do
  begin
    Width := 200;
    Text := 'Prêt';
    Style := psText;
  end;

  with StatusBar1.Panels.Add do
  begin
    Width := 100;
    Style := psText;  // Style texte
    Alignment := taCenter;  // Centré
  end;

  with StatusBar1.Panels.Add do
  begin
    Width := 80;
    Style := psOwnerDraw;  // Dessin personnalisé
  end;

  // Le dernier panneau s'étend automatiquement
  with StatusBar1.Panels.Add do
  begin
    Style := psText;
    Alignment := taRightJustify;  // Aligné à droite
  end;
end;

// Mise à jour de la barre d'état
procedure TForm1.UpdateStatusBar;
begin
  StatusBar1.Panels[0].Text := 'Lignes: ' + IntToStr(Memo1.Lines.Count);
  StatusBar1.Panels[1].Text := 'Col: ' + IntToStr(Memo1.CaretPos.X + 1);
  StatusBar1.Panels[3].Text := FormatDateTime('hh:nn:ss', Now);
end;

// Gestion du dessin personnalisé
procedure TForm1.StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
  const Rect: TRect);
begin
  if Panel = StatusBar1.Panels[2] then
  begin
    // Dessin d'un indicateur (par exemple, mode INS/OVR)
    with StatusBar.Canvas do
    begin
      if FModeInsert then
      begin
        Brush.Color := clGreen;
        Font.Color := clWhite;
        TextOut(Rect.Left + 5, Rect.Top + 2, 'INS');
      end
      else
      begin
        Brush.Color := clRed;
        Font.Color := clWhite;
        TextOut(Rect.Left + 5, Rect.Top + 2, 'OVR');
      end;
    end;
  end;
end;
```

## Intégration dans un exemple complet

Voici un exemple qui intègre menu, barre d'outils et barre d'état dans une application d'éditeur de texte simple :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  FichiersRecents := TStringList.Create;
  MettreAJourMenuFichiersRecents;

  // Associer le menu contextuel au mémo
  Memo1.PopupMenu := PopupMenu1;

  // Initialiser la barre d'état
  UpdateStatusBar;

  // Démarrer le timer pour mettre à jour l'heure
  Timer1.Enabled := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FichiersRecents.Free;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  // Mettre à jour la barre d'état quand le texte change
  UpdateStatusBar;
end;

procedure TForm1.Memo1Click(Sender: TObject);
begin
  // Mettre à jour la position du curseur dans la barre d'état
  UpdateStatusBar;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Mettre à jour l'heure dans la barre d'état
  StatusBar1.Panels[3].Text := FormatDateTime('hh:nn:ss', Now);
end;

procedure TForm1.AjouterFichierRecent(const FileName: string);
var
  Index: Integer;
begin
  // Éviter les doublons
  Index := FichiersRecents.IndexOf(FileName);
  if Index >= 0 then
    FichiersRecents.Delete(Index);

  // Ajouter au début de la liste
  FichiersRecents.Insert(0, FileName);

  // Limiter à 5 fichiers récents
  while FichiersRecents.Count > 5 do
    FichiersRecents.Delete(FichiersRecents.Count - 1);

  // Mettre à jour le menu
  MettreAJourMenuFichiersRecents;
end;

procedure TForm1.OuvrirFichier(const FileName: string);
begin
  try
    Memo1.Lines.LoadFromFile(FileName);
    Caption := ExtractFileName(FileName) + ' - Mon Éditeur';
    FichierCourant := FileName;
    AjouterFichierRecent(FileName);
    UpdateStatusBar;
  except
    on E: Exception do
      ShowMessage('Erreur lors de l''ouverture du fichier: ' + E.Message);
  end;
end;
```

## Astuces et bonnes pratiques

### 1. Cohérence avec les standards Windows

Suivez les conventions habituelles pour que vos utilisateurs se sentent à l'aise :
- Menu Fichier pour les opérations sur les documents
- Menu Édition pour couper/copier/coller
- Menu Affichage pour les options d'affichage
- Aide et À propos à la fin

### 2. Raccourcis clavier et info-bulles

- Définissez des raccourcis clavier pour les actions fréquentes
- Ajoutez des info-bulles (Hint) à vos boutons de barre d'outils
- Activez les info-bulles avec `Application.ShowHint := True`

### 3. Personnalisation par l'utilisateur

Pour les applications plus avancées, envisagez de permettre aux utilisateurs de personnaliser les barres d'outils et les menus. Delphi propose des composants comme `TActionManager` qui facilitent cette fonctionnalité.

### 4. Menus adaptés au contexte

Modifiez dynamiquement l'état des éléments de menu en fonction du contexte :
- Désactivez les options non applicables
- Cochez/décochez les options d'activation
- Utilisez l'événement `OnPopup` pour mettre à jour avant l'affichage

## Conclusion

Les menus et barres d'outils sont des éléments essentiels de toute interface utilisateur professionnelle. Delphi offre tous les outils nécessaires pour les créer et les personnaliser facilement. En combinant menus, barres d'outils et barres d'état avec le système d'actions, vous pouvez créer des interfaces cohérentes et intuitives pour vos applications.

Dans la prochaine section, nous verrons comment gérer les événements d'une manière plus approfondie pour rendre vos applications encore plus interactives.

---

*Exercice pratique : Créez une petite application d'éditeur de texte avec un menu complet (Fichier, Édition, Format, Aide), une barre d'outils pour les fonctions courantes, et une barre d'état affichant le nombre de lignes, la position du curseur et l'heure actuelle. Ajoutez également un menu contextuel pour les opérations d'édition (couper, copier, coller).*
