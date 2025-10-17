🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.6 Gestion des événements

## Introduction

La gestion des événements est au cœur de la programmation d'interfaces graphiques avec Delphi. Comprendre les événements est essentiel pour créer des applications interactives qui répondent aux actions de l'utilisateur. Dans ce chapitre, nous allons découvrir comment fonctionnent les événements et comment les utiliser efficacement.

## 4.6.1 Qu'est-ce qu'un événement ?

### Définition

Un **événement** est une action ou une occurrence qui se produit dans votre application et à laquelle vous pouvez répondre en exécutant du code. C'est le mécanisme qui permet à votre programme de réagir aux interactions de l'utilisateur ou à des changements d'état.

### Exemples d'événements courants

- L'utilisateur clique sur un bouton
- L'utilisateur tape du texte dans une zone de saisie
- L'utilisateur déplace la souris sur un composant
- Une fenêtre s'ouvre ou se ferme
- Un timer déclenche une action périodique
- Un fichier est chargé
- Une erreur se produit

### Le modèle événementiel

Delphi utilise un modèle de **programmation événementielle** :

1. **L'application attend** un événement (elle est en "boucle d'attente")
2. **Un événement se produit** (clic, frappe clavier, etc.)
3. **Le gestionnaire d'événement est appelé** (votre code s'exécute)
4. **L'application retourne en attente** du prochain événement

```
┌─────────────────────────────────────┐
│   Application en attente            │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│   Événement se produit              │
│   (ex: clic sur bouton)             │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│   Gestionnaire d'événement appelé   │
│   (votre code s'exécute)            │
└──────────────┬──────────────────────┘
               │
               ▼
┌─────────────────────────────────────┐
│   Retour en attente                 │
└─────────────────────────────────────┘
```

---

## 4.6.2 Anatomie d'un événement

### Structure d'un gestionnaire d'événement

Un gestionnaire d'événement (event handler) est une procédure qui s'exécute lorsqu'un événement se produit.

**Syntaxe générale :**
```pascal
procedure TForm1.NomComposantNomEvenement(Sender: TObject);
begin
  // Votre code ici
end;
```

**Exemple concret :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Bouton cliqué !');
end;
```

### Le paramètre Sender

Le paramètre **Sender** représente le composant qui a déclenché l'événement. Il est de type `TObject`, ce qui signifie qu'il peut référencer n'importe quel composant.

**Pourquoi Sender est utile ?**

```pascal
// Un seul gestionnaire pour plusieurs boutons
procedure TForm1.BoutonClick(Sender: TObject);
begin
  if Sender = Button1 then
    ShowMessage('Bouton 1 cliqué')
  else if Sender = Button2 then
    ShowMessage('Bouton 2 cliqué')
  else if Sender = Button3 then
    ShowMessage('Bouton 3 cliqué');
end;
```

**Utilisation avancée avec cast :**
```pascal
procedure TForm1.BoutonClick(Sender: TObject);
var
  Bouton: TButton;
begin
  // Convertir Sender en TButton pour accéder à ses propriétés
  Bouton := Sender as TButton;
  ShowMessage('Vous avez cliqué sur : ' + Bouton.Caption);

  // Modifier le bouton qui a été cliqué
  Bouton.Color := clRed;
end;
```

---

## 4.6.3 Créer un gestionnaire d'événement

### Méthode 1 : Double-clic (la plus simple)

1. Sélectionnez le composant sur le formulaire
2. Double-cliquez dessus
3. Delphi crée automatiquement le gestionnaire pour l'événement par défaut
4. Écrivez votre code entre `begin` et `end`

**Événements par défaut :**
- **TButton** : OnClick
- **TEdit** : OnChange
- **TForm** : OnCreate
- **TTimer** : OnTimer

### Méthode 2 : Via l'Inspecteur d'objets

1. Sélectionnez le composant
2. Allez dans l'Inspecteur d'objets
3. Cliquez sur l'onglet **Événements** (icône éclair ⚡)
4. Double-cliquez dans la colonne de droite à côté du nom de l'événement
5. Delphi crée le gestionnaire et vous y amène

### Méthode 3 : Réutiliser un gestionnaire existant

1. Dans l'onglet Événements de l'Inspecteur d'objets
2. Cliquez sur la liste déroulante à côté de l'événement
3. Sélectionnez un gestionnaire existant dans la liste

**Exemple :** Utiliser le même gestionnaire `Button1Click` pour plusieurs boutons.

### Méthode 4 : Créer manuellement (avancé)

```pascal
// Dans la déclaration de classe (section private ou public)
procedure MonGestionnaire(Sender: TObject);

// Dans le code
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Assigner le gestionnaire
  Button1.OnClick := MonGestionnaire;
end;

procedure TForm1.MonGestionnaire(Sender: TObject);
begin
  ShowMessage('Événement déclenché !');
end;
```

---

## 4.6.4 Les événements de la souris

### OnClick

L'événement le plus utilisé, déclenché lors d'un clic simple.

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Caption := 'Bouton cliqué !';
end;
```

### OnDblClick

Déclenché lors d'un double-clic.

```pascal
procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
    ShowMessage('Vous avez double-cliqué sur : ' +
                ListBox1.Items[ListBox1.ItemIndex]);
end;
```

### OnMouseDown et OnMouseUp

Déclenchés lorsque l'utilisateur appuie ou relâche un bouton de la souris.

```pascal
procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    Panel1.Color := clRed
  else if Button = mbRight then
    Panel1.Color := clBlue;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Panel1.Color := clWhite;
end;
```

**Paramètres :**
- **Button** : Bouton de souris utilisé (`mbLeft`, `mbRight`, `mbMiddle`)
- **Shift** : État des touches modificatrices (Ctrl, Shift, Alt)
- **X, Y** : Position de la souris dans le composant

### OnMouseMove

Déclenché lorsque la souris se déplace sur le composant.

```pascal
procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  // Afficher les coordonnées de la souris
  StatusBar1.SimpleText := Format('Position: X=%d, Y=%d', [X, Y]);

  // Dessiner un point à la position de la souris si bouton gauche enfoncé
  if ssLeft in Shift then
  begin
    Image1.Canvas.Pixels[X, Y] := clBlack;
  end;
end;
```

### OnMouseEnter et OnMouseLeave

Déclenchés lorsque la souris entre ou sort du composant.

```pascal
procedure TForm1.Button1MouseEnter(Sender: TObject);
begin
  Button1.Color := clYellow;
  Button1.Font.Style := [fsBold];
end;

procedure TForm1.Button1MouseLeave(Sender: TObject);
begin
  Button1.Color := clBtnFace;
  Button1.Font.Style := [];
end;
```

### OnMouseWheel

Déclenché lors de l'utilisation de la molette de la souris.

```pascal
procedure TForm1.Image1MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // Zoomer avec la molette
  if WheelDelta > 0 then
    Image1.Width := Image1.Width + 10  // Zoom avant
  else
    Image1.Width := Image1.Width - 10; // Zoom arrière

  Handled := True; // Indiquer que l'événement a été traité
end;
```

---

## 4.6.5 Les événements du clavier

### OnKeyPress

Déclenché lorsqu'une touche de caractère est appuyée.

```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // N'accepter que des chiffres
  if not (Key in ['0'..'9', #8, #13]) then  // #8 = Backspace, #13 = Enter
  begin
    Key := #0; // Annuler la frappe
    Beep; // Son d'alerte
  end;

  // Valider avec Enter
  if Key = #13 then
  begin
    ShowMessage('Valeur saisie : ' + Edit1.Text);
  end;
end;
```

### OnKeyDown et OnKeyUp

Déclenchés pour toutes les touches, y compris les touches spéciales.

```pascal
procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Détection de combinaisons de touches
  if (Key = VK_F5) then
    ActualiserDonnees;

  if (ssCtrl in Shift) and (Key = Ord('S')) then
  begin
    Key := 0; // Empêcher le traitement par défaut
    EnregistrerDocument;
  end;

  // Touches fléchées
  case Key of
    VK_LEFT:  DeplacerGauche;
    VK_RIGHT: DeplacerDroite;
    VK_UP:    DeplacerHaut;
    VK_DOWN:  DeplacerBas;
  end;
end;
```

**Codes de touches courants (Virtual Key Codes) :**

| Code | Touche |
|------|--------|
| `VK_RETURN` ou `VK_ENTER` | Entrée |
| `VK_ESCAPE` | Échap |
| `VK_SPACE` | Espace |
| `VK_BACK` | Retour arrière |
| `VK_DELETE` | Suppr |
| `VK_F1` à `VK_F12` | F1 à F12 |
| `VK_LEFT`, `VK_RIGHT`, `VK_UP`, `VK_DOWN` | Flèches |
| `VK_HOME`, `VK_END` | Début, Fin |
| `VK_PRIOR`, `VK_NEXT` | Page Haut, Page Bas |
| `VK_CONTROL` | Ctrl |
| `VK_SHIFT` | Shift |
| `VK_MENU` | Alt |

**État des modificateurs (Shift) :**
- `ssShift` : Touche Shift enfoncée
- `ssCtrl` : Touche Ctrl enfoncée
- `ssAlt` : Touche Alt enfoncée
- `ssLeft`, `ssRight`, `ssMiddle` : Boutons de souris

### Exemple complet : Calculatrice au clavier

```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    '0'..'9', ',', '.':
      ; // Accepter les chiffres et décimales

    '+', '-', '*', '/':
      begin
        FOprateur := Key;
        FValeur1 := StrToFloatDef(Edit1.Text, 0);
        Edit1.Clear;
      end;

    '=', #13:
      begin
        FValeur2 := StrToFloatDef(Edit1.Text, 0);
        case FOperateur of
          '+': Edit1.Text := FloatToStr(FValeur1 + FValeur2);
          '-': Edit1.Text := FloatToStr(FValeur1 - FValeur2);
          '*': Edit1.Text := FloatToStr(FValeur1 * FValeur2);
          '/': if FValeur2 <> 0 then
                 Edit1.Text := FloatToStr(FValeur1 / FValeur2)
               else
                 ShowMessage('Division par zéro !');
        end;
      end;

    #8: ; // Backspace autorisé

    else
      Key := #0; // Bloquer les autres touches
  end;
end;
```

---

## 4.6.6 Les événements du formulaire

### OnCreate

Déclenché une seule fois, à la création du formulaire. C'est l'endroit idéal pour initialiser vos variables et composants.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisation
  FCompteur := 0;
  ListBox1.Items.Clear;

  // Configuration
  Edit1.Text := '';
  Edit1.MaxLength := 50;

  // Charger des données
  ChargerConfiguration;
  ChargerDonnees;
end;
```

### OnShow

Déclenché chaque fois que le formulaire devient visible.

```pascal
procedure TForm1.FormShow(Sender: TObject);
begin
  // Actualiser les données à chaque affichage
  ActualiserListeProduits;

  // Mettre le focus sur un contrôle
  Edit1.SetFocus;
end;
```

### OnClose et OnCloseQuery

**OnCloseQuery** : Permet de confirmer la fermeture.

```pascal
procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Memo1.Modified then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          EnregistrerDocument;
          CanClose := True;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False; // Annuler la fermeture
    end;
  end
  else
    CanClose := True;
end;
```

**OnClose** : Déclenché juste avant la fermeture effective.

```pascal
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Nettoyer les ressources
  SauvegarderConfiguration;

  // Libérer le formulaire de la mémoire
  Action := caFree;
end;
```

**Actions possibles :**
- `caNone` : Ne rien faire (annule la fermeture)
- `caHide` : Masquer le formulaire
- `caFree` : Libérer le formulaire de la mémoire
- `caMinimize` : Minimiser le formulaire

### OnResize

Déclenché lorsque la taille du formulaire change.

```pascal
procedure TForm1.FormResize(Sender: TObject);
begin
  // Adapter la taille des composants
  Panel1.Width := ClientWidth - 20;
  Memo1.Height := ClientHeight - Panel1.Height - 30;

  // Centrer un composant
  Button1.Left := (ClientWidth - Button1.Width) div 2;
end;
```

### OnActivate et OnDeactivate

Déclenchés lorsque le formulaire devient actif ou perd le focus.

```pascal
procedure TForm1.FormActivate(Sender: TObject);
begin
  // Actualiser quand la fenêtre redevient active
  VerifierMisesAJour;
  StatusBar1.SimpleText := 'Fenêtre active';
end;

procedure TForm1.FormDeactivate(Sender: TObject);
begin
  StatusBar1.SimpleText := 'Fenêtre inactive';
end;
```

---

## 4.6.7 Les événements de contrôles courants

### TEdit : OnChange

Déclenché à chaque modification du texte.

```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // Compter les caractères en temps réel
  Label1.Caption := Format('Caractères : %d / 100', [Length(Edit1.Text)]);

  // Validation en temps réel
  if Length(Edit1.Text) > 100 then
  begin
    Edit1.Color := clRed;
    Label1.Font.Color := clRed;
  end
  else
  begin
    Edit1.Color := clWindow;
    Label1.Font.Color := clWindowText;
  end;
end;
```

### TEdit : OnEnter et OnExit

Déclenchés lorsque le contrôle reçoit ou perd le focus.

```pascal
procedure TForm1.Edit1Enter(Sender: TObject);
begin
  // Sélectionner tout le texte quand on entre dans le champ
  Edit1.SelectAll;
  Edit1.Color := clYellow;
  StatusBar1.SimpleText := 'Saisie du nom...';
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  // Valider et formatter à la sortie
  Edit1.Text := Trim(Edit1.Text); // Enlever les espaces
  Edit1.Color := clWindow;

  if Edit1.Text = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Edit1.SetFocus; // Retourner au champ
  end;
end;
```

### TComboBox : OnChange et OnSelect

```pascal
procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  // Déclenché pour toute modification (même par code)
  Label1.Caption := 'Sélection : ' + ComboBox1.Text;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  // Déclenché uniquement par sélection utilisateur
  case ComboBox1.ItemIndex of
    0: AfficherPage1;
    1: AfficherPage2;
    2: AfficherPage3;
  end;
end;
```

### TCheckBox : OnClick

```pascal
procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
  begin
    Panel1.Visible := True;
    Label1.Caption := 'Options avancées activées';
  end
  else
  begin
    Panel1.Visible := False;
    Label1.Caption := 'Options avancées désactivées';
  end;
end;
```

### TListBox et TListView : OnClick, OnDblClick, OnSelectItem

```pascal
procedure TForm1.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then
  begin
    // Afficher des détails
    Label1.Caption := 'Sélectionné : ' + ListBox1.Items[ListBox1.ItemIndex];
    Button1.Enabled := True;
  end
  else
    Button1.Enabled := False;
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  // Double-clic pour éditer
  if ListBox1.ItemIndex <> -1 then
    EditerElement(ListBox1.ItemIndex);
end;
```

### TTimer : OnTimer

Déclenché périodiquement selon l'intervalle défini.

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 1000; // 1000 ms = 1 seconde
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Mise à jour de l'heure
  Label1.Caption := TimeToStr(Now);

  // Animation simple
  Image1.Left := Image1.Left + 5;
  if Image1.Left > ClientWidth then
    Image1.Left := -Image1.Width;
end;
```

---

## 4.6.8 Ordre d'exécution des événements

Il est important de comprendre l'ordre dans lequel les événements se déclenchent.

### Lors de l'ouverture d'un formulaire

```
1. OnCreate
2. OnShow
3. OnActivate
4. OnPaint
```

### Lors de la fermeture d'un formulaire

```
1. OnCloseQuery (possibilité d'annuler)
2. OnClose
3. OnDeactivate
4. OnDestroy
```

### Lors d'un clic sur un bouton

```
1. OnMouseDown
2. OnClick
3. OnMouseUp
```

### Lors de la saisie dans un Edit

```
1. OnEnter (focus)
2. OnKeyDown
3. OnKeyPress
4. OnChange
5. OnKeyUp
6. OnExit (perte du focus)
```

### Exemple de traçage des événements

```pascal
procedure TForm1.TracerEvenement(const NomEvenement: string);
begin
  Memo1.Lines.Add(FormatDateTime('hh:nn:ss.zzz', Now) + ' - ' + NomEvenement);
end;

procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TracerEvenement('Button1.OnMouseDown');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TracerEvenement('Button1.OnClick');
end;

procedure TForm1.Button1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  TracerEvenement('Button1.OnMouseUp');
end;
```

---

## 4.6.9 Événements avancés

### Événements personnalisés

Vous pouvez créer vos propres événements.

```pascal
type
  // Déclaration du type d'événement
  TNotificationEvent = procedure(Sender: TObject; const Message: string) of object;

  TForm1 = class(TForm)
    // ...
  private
    FOnNotification: TNotificationEvent;
    procedure DeclencherNotification(const Message: string);
  public
    property OnNotification: TNotificationEvent read FOnNotification write FOnNotification;
  end;

implementation

procedure TForm1.DeclencherNotification(const Message: string);
begin
  // Déclencher l'événement s'il est assigné
  if Assigned(FOnNotification) then
    FOnNotification(Self, Message);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DeclencherNotification('Le bouton a été cliqué !');
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);
begin
  OnNotification := GererNotification;
end;

procedure TForm1.GererNotification(Sender: TObject; const Message: string);
begin
  ShowMessage(Message);
end;
```

### Événements anonymes (procédures anonymes)

Disponible à partir de Delphi 2009.

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Créer un événement anonyme
  TThread.CreateAnonymousThread(
    procedure
    begin
      Sleep(2000); // Simuler un traitement long
      TThread.Synchronize(nil,
        procedure
        begin
          ShowMessage('Traitement terminé !');
        end
      );
    end
  ).Start;
end;
```

### Bloquer et débloquer les événements

Parfois, vous devez modifier un contrôle sans déclencher ses événements.

```pascal
procedure TForm1.ChargerDonnees;
var
  EventHandler: TNotifyEvent;
begin
  // Sauvegarder le gestionnaire
  EventHandler := ComboBox1.OnChange;

  // Désactiver temporairement
  ComboBox1.OnChange := nil;

  try
    // Modifier sans déclencher OnChange
    ComboBox1.Items.Clear;
    ComboBox1.Items.Add('Option 1');
    ComboBox1.Items.Add('Option 2');
    ComboBox1.ItemIndex := 0;
  finally
    // Restaurer le gestionnaire
    ComboBox1.OnChange := EventHandler;
  end;
end;
```

---

## 4.6.10 Exemples pratiques complets

### Exemple 1 : Validation de formulaire

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Button1.Enabled := False; // Désactiver jusqu'à validation
end;

procedure TForm1.EditChange(Sender: TObject);
begin
  // Activer le bouton seulement si tous les champs sont remplis
  Button1.Enabled := (Edit1.Text <> '') and
                     (Edit2.Text <> '') and
                     (Edit3.Text <> '');
end;

procedure TForm1.Edit1Exit(Sender: TObject);
begin
  // Validation du nom
  if Trim(Edit1.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    Edit1.SetFocus;
  end;
end;

procedure TForm1.Edit2Exit(Sender: TObject);
begin
  // Validation de l'email
  if Pos('@', Edit2.Text) = 0 then
  begin
    ShowMessage('Email invalide');
    Edit2.SetFocus;
  end;
end;

procedure TForm1.Edit3KeyPress(Sender: TObject; var Key: Char);
begin
  // Accepter uniquement les chiffres pour le téléphone
  if not (Key in ['0'..'9', #8, #13]) then
    Key := #0;
end;
```

### Exemple 2 : Application de dessin simple

```pascal
type
  TForm1 = class(TForm)
    Image1: TImage;
  private
    FDessin: Boolean;
    FDernierX, FDernierY: Integer;
  end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FDessin := False;
  Image1.Canvas.Brush.Color := clWhite;
  Image1.Canvas.FillRect(Image1.ClientRect);
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FDessin := True;
    FDernierX := X;
    FDernierY := Y;
  end;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDessin then
  begin
    // Dessiner une ligne
    Image1.Canvas.Pen.Color := clBlack;
    Image1.Canvas.Pen.Width := 2;
    Image1.Canvas.MoveTo(FDernierX, FDernierY);
    Image1.Canvas.LineTo(X, Y);

    FDernierX := X;
    FDernierY := Y;
  end;
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FDessin := False;
end;
```

### Exemple 3 : Recherche dynamique

```pascal
procedure TForm1.EditRechercheChange(Sender: TObject);
var
  i: Integer;
  Texte: string;
begin
  Texte := LowerCase(EditRecherche.Text);

  // Effacer les résultats précédents
  ListBoxResultats.Items.BeginUpdate;
  try
    ListBoxResultats.Items.Clear;

    // Rechercher dans la liste source
    if Texte <> '' then
    begin
      for i := 0 to ListeSource.Count - 1 do
      begin
        if Pos(Texte, LowerCase(ListeSource[i])) > 0 then
          ListBoxResultats.Items.Add(ListeSource[i]);
      end;
    end;

    // Afficher le nombre de résultats
    LabelResultats.Caption := Format('%d résultat(s) trouvé(s)',
                                     [ListBoxResultats.Items.Count]);
  finally
    ListBoxResultats.Items.EndUpdate;
  end;
end;

procedure TForm1.EditRechercheKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Navigation clavier dans les résultats
  if Key = VK_DOWN then
  begin
    if ListBoxResultats.Items.Count > 0 then
    begin
      ListBoxResultats.SetFocus;
      ListBoxResultats.ItemIndex := 0;
      Key := 0;
    end;
  end;
end;
```

### Exemple 4 : Glisser-déposer (Drag & Drop)

```pascal
procedure TForm1.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    // Démarrer le glisser-déposer
    ListBox1.BeginDrag(False);
  end;
end;

procedure TForm1.ListBox2DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  // Accepter seulement si la source est ListBox1
  Accept := Source = ListBox1;
end;

procedure TForm1.ListBox2DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if (Source = ListBox1) and (ListBox1.ItemIndex <> -1) then
  begin
    // Copier l'élément
    ListBox2.Items.Add(ListBox1.Items[ListBox1.ItemIndex]);

    // Optionnel : supprimer de la source
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  end;
end;
```

---

## 4.6.11 Bonnes pratiques

### 1. Nommage des gestionnaires

```pascal
// Bon : Nom descriptif
procedure TForm1.BoutonValiderClick(Sender: TObject);

// Mauvais : Nom par défaut non modifié
procedure TForm1.Button1Click(Sender: TObject);
```

### 2. Ne pas surcharger les événements

```pascal
// À éviter : Trop de logique dans OnChange
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // 50 lignes de code...
  // Difficile à maintenir !
end;

// Préférer : Découper en fonctions
procedure TForm1.Edit1Change(Sender: TObject);
begin
  ValiderChamp;
  MettreAJourCompteur;
  VerifierLongueur;
end;
```

### 3. Vérifier les conditions avant de traiter

```pascal
procedure TForm1.ListBox1DblClick(Sender: TObject);
begin
  // Toujours vérifier qu'un élément est sélectionné
  if ListBox1.ItemIndex = -1 then
    Exit;

  // Traiter l'élément sélectionné
  EditerElement(ListBox1.ItemIndex);
end;
```

### 4. Utiliser des drapeaux pour éviter les boucles

```pascal
type
  TForm1 = class(TForm)
  private
    FMiseAJourEnCours: Boolean;
  end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if FMiseAJourEnCours then
    Exit; // Éviter la récursion

  FMiseAJourEnCours := True;
  try
    // Traitement qui pourrait modifier Edit1
    MettreAJourAutresChamps;
  finally
    FMiseAJourEnCours := False;
  end;
end;
```

### 5. Gérer les exceptions dans les événements

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  try
    // Code qui pourrait générer une exception
    ConvertirEtAfficher(Edit1.Text);
  except
    on E: Exception do
    begin
      ShowMessage('Erreur : ' + E.Message);
      Edit1.SetFocus;
    end;
  end;
end;
```

### 6. Libérer les ressources correctement

```pascal
procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  try
    // Sauvegarder les données
    SauvegarderConfiguration;

    // Fermer les connexions
    if Assigned(FConnexion) then
      FConnexion.Close;

    // Libérer les objets créés
    FreeAndNil(FListeUtilisateurs);
  finally
    Action := caFree;
  end;
end;
```

### 7. Documenter les événements complexes

```pascal
procedure TForm1.ComplexEventHandler(Sender: TObject);
begin
  {
    Ce gestionnaire d'événement effectue les opérations suivantes :
    1. Valide les données saisies
    2. Met à jour la base de données
    3. Actualise l'interface utilisateur
    4. Envoie une notification par email

    Déclenché par : Button1.OnClick
    Appelé depuis : ValidationForm, AutoSave
  }

  // Votre code ici...
end;
```

---

## 4.6.12 Débogage des événements

### Techniques de débogage

**1. Points d'arrêt**
```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Placer un point d'arrêt ici (F5)
  ShowMessage('Test');
end;
```

**2. Messages de trace**
```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  OutputDebugString(PChar('Edit1.Text = ' + Edit1.Text));
end;
```

**3. Compteur d'appels**
```pascal
var
  FCompteurAppels: Integer = 0;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Inc(FCompteurAppels);
  Caption := Format('Appels : %d', [FCompteurAppels]);
end;
```

### Erreurs courantes

**1. Oublier de vérifier Assigned**
```pascal
// Erreur : Peut causer une exception
FObjet.Methode;

// Correct
if Assigned(FObjet) then
  FObjet.Methode;
```

**2. Modifier un composant pendant son événement**
```pascal
// Attention : Peut causer des problèmes
procedure TForm1.ListBox1Click(Sender: TObject);
begin
  ListBox1.Items.Clear; // Dangereux pendant l'événement Click
end;

// Mieux : Utiliser PostMessage ou un flag
procedure TForm1.ListBox1Click(Sender: TObject);
begin
  PostMessage(Handle, WM_USER + 1, 0, 0);
end;
```

---

## Conclusion

La gestion des événements est la pierre angulaire de la programmation d'interfaces graphiques avec Delphi. En maîtrisant les concepts présentés dans ce chapitre, vous serez capable de créer des applications interactives et réactives qui répondent efficacement aux actions de l'utilisateur.

### Points clés à retenir :

- **Les événements** permettent à votre application de réagir aux actions de l'utilisateur
- **Le paramètre Sender** identifie le composant qui a déclenché l'événement
- **L'ordre des événements** est important et prévisible
- **Les bonnes pratiques** améliorent la maintenabilité et la robustesse du code
- **La validation** des données doit être effectuée dans les événements appropriés
- **Le débogage** des événements nécessite des techniques spécifiques

Avec ces connaissances, vous êtes prêt à créer des interfaces utilisateur riches et interactives dans vos applications Delphi !

⏭️ [Création de dialogues personnalisés](/04-conception-dinterfaces-utilisateur-avec-la-vcl/07-creation-de-dialogues-personnalises.md)
