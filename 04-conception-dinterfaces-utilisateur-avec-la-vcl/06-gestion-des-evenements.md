# 4.6 Gestion des événements

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

La gestion des événements est un concept fondamental dans le développement d'applications Delphi. C'est grâce aux événements que vos applications peuvent réagir aux actions de l'utilisateur et à d'autres déclencheurs. Dans cette section, nous allons explorer comment les événements fonctionnent et comment les gérer efficacement.

## Qu'est-ce qu'un événement ?

Un **événement** est une notification qu'une action particulière s'est produite, comme :
- Un clic de souris
- Une pression sur une touche du clavier
- Un changement de valeur dans un contrôle
- L'ouverture ou la fermeture d'une fenêtre
- L'expiration d'un minuteur

En Delphi, les événements suivent le modèle de **délégation d'événements**, où vous "déléguez" la gestion d'un événement à une méthode spécifique appelée **gestionnaire d'événement** (ou **event handler** en anglais).

## Structure d'un gestionnaire d'événement

Un gestionnaire d'événement en Delphi est une méthode de votre formulaire (ou d'une autre classe) avec une signature spécifique. Voici sa structure générale :

```pascal
procedure TNomFormulaire.NomComposantNomEvenement(Sender: TObject);
begin
  // Code à exécuter quand l'événement se produit
end;
```

Par exemple :

```pascal
procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Le bouton a été cliqué !');
end;
```

Dans cet exemple :
- `TForm1` est la classe de votre formulaire
- `Button1` est le nom du composant (un bouton)
- `Click` est le nom de l'événement
- `Sender: TObject` est un paramètre qui identifie le composant qui a déclenché l'événement

## Création de gestionnaires d'événements

Il existe plusieurs façons de créer un gestionnaire d'événement en Delphi :

### 1. Via l'Inspecteur d'objets (méthode la plus courante)

1. Sélectionnez le composant sur votre formulaire
2. Cliquez sur l'onglet "Événements" (icône en forme d'éclair) dans l'Inspecteur d'objets
3. Double-cliquez à droite de l'événement que vous souhaitez gérer (par exemple, `OnClick`)
4. Delphi crée automatiquement un gestionnaire d'événement vide et vous place dans l'éditeur de code

![Inspecteur d'objets - Onglet Événements](https://via.placeholder.com/300x200)

### 2. Par code (à l'exécution)

Vous pouvez également assigner des gestionnaires d'événements dynamiquement par code :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Assigner un gestionnaire d'événement au bouton
  Button1.OnClick := MonGestionnairePersonnalise;
end;

procedure TForm1.MonGestionnairePersonnalise(Sender: TObject);
begin
  ShowMessage('Gestionnaire personnalisé appelé !');
end;
```

### 3. En utilisant l'éditeur de formulaire

Vous pouvez également double-cliquer directement sur certains composants dans l'éditeur de formulaire pour créer leur gestionnaire d'événement par défaut :
- Double-cliquer sur un bouton crée un gestionnaire `OnClick`
- Double-cliquer sur un formulaire crée un gestionnaire `OnCreate`
- etc.

## Le paramètre Sender

Presque tous les gestionnaires d'événements en Delphi incluent un paramètre `Sender: TObject`. Ce paramètre identifie le composant qui a déclenché l'événement, ce qui est particulièrement utile lorsque vous utilisez le même gestionnaire pour plusieurs composants.

```pascal
procedure TForm1.BoutonCouleurClick(Sender: TObject);
begin
  // Déterminer quel bouton a été cliqué
  if Sender = BoutonRouge then
    Panel1.Color := clRed
  else if Sender = BoutonVert then
    Panel1.Color := clGreen
  else if Sender = BoutonBleu then
    Panel1.Color := clBlue;
end;
```

Pour utiliser ce gestionnaire avec plusieurs boutons :
1. Créez le gestionnaire pour l'un des boutons
2. Dans l'Inspecteur d'objets, sélectionnez les autres boutons
3. Choisissez le même gestionnaire dans la liste déroulante pour l'événement `OnClick`

## Types d'événements courants

### Événements de la souris

- **OnClick** : déclenché par un clic simple
- **OnDblClick** : déclenché par un double-clic
- **OnMouseDown** : déclenché lorsqu'un bouton de la souris est enfoncé
- **OnMouseUp** : déclenché lorsqu'un bouton de la souris est relâché
- **OnMouseMove** : déclenché lorsque la souris se déplace sur le composant
- **OnMouseEnter** : déclenché lorsque la souris entre dans la zone du composant
- **OnMouseLeave** : déclenché lorsque la souris quitte la zone du composant

```pascal
procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  // Afficher les coordonnées de la souris dans une étiquette
  Label1.Caption := Format('X: %d, Y: %d', [X, Y]);
end;
```

Le type `TShiftState` vous indique quelles touches modificatrices (Shift, Ctrl, Alt) et quels boutons de souris sont enfoncés.

### Événements du clavier

- **OnKeyDown** : déclenché lorsqu'une touche est enfoncée
- **OnKeyUp** : déclenché lorsqu'une touche est relâchée
- **OnKeyPress** : déclenché pour les touches de caractères (pas pour les touches spéciales comme les flèches)

```pascal
procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  // Autoriser uniquement les chiffres
  if not (Key in ['0'..'9', #8]) then // #8 est Backspace
    Key := #0; // Annuler la touche
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // Réagir à la touche F1
  if Key = VK_F1 then
    ShowMessage('Aide');

  // Réagir à Ctrl+S
  if (Key = Ord('S')) and (ssCtrl in Shift) then
    ActionEnregistrer.Execute;
end;
```

### Événements de changement

- **OnChange** : déclenché lorsque la valeur ou le contenu d'un contrôle change
- **OnSelect** : déclenché lorsque la sélection change dans certains contrôles

```pascal
procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  // Mettre à jour une étiquette avec la valeur actuelle
  Label1.Caption := 'Valeur: ' + IntToStr(TrackBar1.Position);

  // Changer l'opacité d'un panneau
  Panel1.Color := RGB(255, 0, 0); // Rouge
  Panel1.AlphaBlendValue := TrackBar1.Position;
  Panel1.AlphaBlend := True;
end;
```

### Événements de formulaire

- **OnCreate** : déclenché lors de la création du formulaire
- **OnShow** : déclenché lorsque le formulaire devient visible
- **OnClose** : déclenché lorsque le formulaire est sur le point d'être fermé
- **OnCloseQuery** : permet de valider ou d'annuler la fermeture
- **OnActivate** : déclenché lorsque le formulaire devient actif
- **OnDeactivate** : déclenché lorsque le formulaire perd le focus

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialisation lors de la création du formulaire
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('Option 1');
  ComboBox1.Items.Add('Option 2');
  ComboBox1.ItemIndex := 0;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if DocumentModifie then
  begin
    case MessageDlg('Document non enregistré. Voulez-vous enregistrer avant de quitter ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          ActionEnregistrer.Execute;
          CanClose := True;
        end;
      mrNo: CanClose := True;
      mrCancel: CanClose := False;
    end;
  end;
end;
```

## Événements personnalisés

Vous pouvez également créer vos propres événements dans des composants personnalisés. C'est un sujet plus avancé, mais voici un aperçu simple :

```pascal
type
  // Définir un type pour le gestionnaire d'événement
  TMonEvenementSpecial = procedure(Sender: TObject; Valeur: Integer) of object;

  // Classe avec un événement personnalisé
  TMonComposant = class(TComponent)
  private
    FOnEvenementSpecial: TMonEvenementSpecial;
    FValeur: Integer;
  public
    procedure DeclencherEvenement;
    procedure SetValeur(const Value: Integer);
  published
    property OnEvenementSpecial: TMonEvenementSpecial
                               read FOnEvenementSpecial
                               write FOnEvenementSpecial;
    property Valeur: Integer read FValeur write SetValeur;
  end;

// Implémentation
procedure TMonComposant.SetValeur(const Value: Integer);
begin
  if FValeur <> Value then
  begin
    FValeur := Value;
    // Déclencher l'événement lors du changement de valeur
    DeclencherEvenement;
  end;
end;

procedure TMonComposant.DeclencherEvenement;
begin
  // Vérifier si un gestionnaire est assigné avant de l'appeler
  if Assigned(FOnEvenementSpecial) then
    FOnEvenementSpecial(Self, FValeur);
end;
```

## Techniques avancées et astuces

### 1. Utilisation d'une même méthode pour plusieurs événements

```pascal
procedure TForm1.GestionCommune(Sender: TObject);
begin
  // Déterminer quelle action effectuer selon l'expéditeur
  if Sender = Button1 then
    ShowMessage('Button1 cliqué')
  else if Sender = Button2 then
    ShowMessage('Button2 cliqué')
  else if Sender = Timer1 then
    Label1.Caption := TimeToStr(Now);
end;
```

### 2. Délai d'événements (debouncing)

Parfois, certains événements comme `OnChange` peuvent se déclencher trop fréquemment. Vous pouvez utiliser un timer pour limiter la fréquence des actions :

```pascal
procedure TForm1.Edit1Change(Sender: TObject);
begin
  // Réinitialiser le timer à chaque changement
  TimerDelai.Enabled := False;
  TimerDelai.Enabled := True;
end;

procedure TForm1.TimerDelaiTimer(Sender: TObject);
begin
  // Ce code s'exécute uniquement après une pause dans la saisie
  TimerDelai.Enabled := False;
  Label1.Caption := 'Texte saisi : ' + Edit1.Text;
  // Effectuer une recherche, etc.
end;
```

### 3. Mémorisation d'informations entre les événements

Vous pouvez déclarer des variables dans la section `private` de votre formulaire pour mémoriser des informations entre les appels d'événements :

```pascal
type
  TForm1 = class(TForm)
    // Composants et gestionnaires d'événements déclarés automatiquement
  private
    FDernierePosition: TPoint;
    FGlisserEnCours: Boolean;
  public
    { Déclarations publiques }
  end;

procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Mémoriser le début du glissement
  FDernierePosition := Point(X, Y);
  FGlisserEnCours := True;
end;

procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if FGlisserEnCours then
  begin
    // Déplacer l'objet relativement au mouvement de la souris
    Panel1.Left := Panel1.Left + (X - FDernierePosition.X);
    Panel1.Top := Panel1.Top + (Y - FDernierePosition.Y);
  end;
end;

procedure TForm1.Panel1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Fin du glissement
  FGlisserEnCours := False;
end;
```

## Bonnes pratiques

### 1. Nommage clair des gestionnaires

Utilisez des noms qui indiquent clairement :
- Le composant concerné
- L'événement géré
- L'action effectuée (si nécessaire)

Exemples :
- `BoutonEnregistrerClick`
- `FormulairePrincipalCreate`
- `EditionNomValidation`

### 2. Garder les gestionnaires courts et ciblés

Si un gestionnaire devient trop long, décomposez-le en méthodes auxiliaires :

```pascal
procedure TForm1.ButtonCalculerClick(Sender: TObject);
begin
  // Au lieu de mettre tout le code ici
  EffectuerCalculs;
  AfficherResultats;
end;

procedure TForm1.EffectuerCalculs;
begin
  // Code de calcul
end;

procedure TForm1.AfficherResultats;
begin
  // Code d'affichage
end;
```

### 3. Éviter les effets de bord

Ne modifiez pas de façon inattendue l'état d'autres composants que ceux concernés par l'événement, sauf si c'est clairement nécessaire.

### 4. Gérer les exceptions

Ajoutez des blocs try/except dans les gestionnaires qui pourraient générer des exceptions :

```pascal
procedure TForm1.ButtonDiviserClick(Sender: TObject);
var
  A, B, Resultat: Double;
begin
  try
    A := StrToFloat(EditNombre1.Text);
    B := StrToFloat(EditNombre2.Text);

    if B = 0 then
      raise Exception.Create('Division par zéro impossible');

    Resultat := A / B;
    LabelResultat.Caption := FloatToStr(Resultat);

  except
    on E: EConvertError do
      ShowMessage('Veuillez entrer des nombres valides');
    on E: Exception do
      ShowMessage('Erreur : ' + E.Message);
  end;
end;
```

## Exemple complet : Dessin simple

Voici un exemple qui utilise plusieurs types d'événements pour créer une application de dessin simple :

```pascal
unit UnitDessin;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls;

type
  TFormDessin = class(TForm)
    PanelOutils: TPanel;
    PanelDessin: TPanel;
    ButtonEffacer: TButton;
    LabelTaille: TLabel;
    TrackBarTaille: TTrackBar;
    ColorBoxCouleur: TColorBox;
    LabelCouleur: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure PanelDessinMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PanelDessinMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PanelDessinMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ButtonEffacerClick(Sender: TObject);
    procedure TrackBarTailleChange(Sender: TObject);
    procedure ColorBoxCouleurChange(Sender: TObject);
  private
    FDessin: Boolean;
    FDernierPoint: TPoint;
    FCouleurPinceau: TColor;
    FTaillePinceau: Integer;
    procedure DessinerLigne(FromX, FromY, ToX, ToY: Integer);
    procedure EffacerDessin;
  public
    { Public declarations }
  end;

var
  FormDessin: TFormDessin;

implementation

{$R *.dfm}

procedure TFormDessin.FormCreate(Sender: TObject);
begin
  // Initialisation
  FDessin := False;
  FCouleurPinceau := clBlack;
  FTaillePinceau := 5;

  // Configurer la zone de dessin
  PanelDessin.Color := clWhite;
  PanelDessin.DoubleBuffered := True; // Réduire le scintillement

  // Initialiser les contrôles
  ColorBoxCouleur.Selected := FCouleurPinceau;
  TrackBarTaille.Position := FTaillePinceau;
end;

procedure TFormDessin.PanelDessinMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    // Commencer le dessin
    FDessin := True;
    FDernierPoint := Point(X, Y);

    // Dessiner un point au départ
    with PanelDessin.Canvas do
    begin
      Pen.Color := FCouleurPinceau;
      Pen.Width := FTaillePinceau;
      Ellipse(X - FTaillePinceau div 2, Y - FTaillePinceau div 2,
              X + FTaillePinceau div 2, Y + FTaillePinceau div 2);
    end;
  end;
end;

procedure TFormDessin.PanelDessinMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FDessin and (ssLeft in Shift) then
  begin
    // Dessiner une ligne du dernier point au point actuel
    DessinerLigne(FDernierPoint.X, FDernierPoint.Y, X, Y);
    FDernierPoint := Point(X, Y);
  end;
end;

procedure TFormDessin.PanelDessinMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Arrêter le dessin
  FDessin := False;
end;

procedure TFormDessin.DessinerLigne(FromX, FromY, ToX, ToY: Integer);
begin
  with PanelDessin.Canvas do
  begin
    Pen.Color := FCouleurPinceau;
    Pen.Width := FTaillePinceau;

    // Dessiner une ligne entre les deux points
    MoveTo(FromX, FromY);
    LineTo(ToX, ToY);

    // Ajouter un cercle au point d'arrivée pour un trait plus lisse
    Brush.Color := FCouleurPinceau;
    Ellipse(ToX - FTaillePinceau div 2, ToY - FTaillePinceau div 2,
            ToX + FTaillePinceau div 2, ToY + FTaillePinceau div 2);
  end;
end;

procedure TFormDessin.ButtonEffacerClick(Sender: TObject);
begin
  EffacerDessin;
end;

procedure TFormDessin.EffacerDessin;
begin
  // Effacer toute la zone de dessin
  with PanelDessin.Canvas do
  begin
    Brush.Color := clWhite;
    Pen.Color := clWhite;
    Rectangle(0, 0, PanelDessin.Width, PanelDessin.Height);
  end;
end;

procedure TFormDessin.TrackBarTailleChange(Sender: TObject);
begin
  // Mettre à jour la taille du pinceau
  FTaillePinceau := TrackBarTaille.Position;
  LabelTaille.Caption := 'Taille : ' + IntToStr(FTaillePinceau);
end;

procedure TFormDessin.ColorBoxCouleurChange(Sender: TObject);
begin
  // Mettre à jour la couleur du pinceau
  FCouleurPinceau := ColorBoxCouleur.Selected;
end;

end.
```

## Conclusion

La gestion des événements est au cœur de la programmation Delphi. Elle permet de créer des applications interactives qui répondent aux actions de l'utilisateur. En comprenant bien comment les événements fonctionnent et comment les gérer efficacement, vous pourrez créer des interfaces utilisateur riches et réactives.

Quelques points clés à retenir :
- Les événements sont des notifications d'actions spécifiques
- Les gestionnaires d'événements sont des méthodes avec une signature particulière
- Le paramètre `Sender` vous permet d'identifier la source de l'événement
- Vous pouvez réutiliser un même gestionnaire pour plusieurs composants
- Les variables privées du formulaire permettent de mémoriser des états entre les événements

Dans la prochaine section, nous verrons comment créer des dialogues personnalisés pour enrichir encore davantage l'interaction avec l'utilisateur.

---

*Exercice pratique : Modifiez l'exemple de dessin ci-dessus pour ajouter les fonctionnalités suivantes :
1. Un bouton pour dessiner des formes spécifiques (cercle, rectangle, ligne)
2. Une option pour remplir les formes avec une couleur
3. La possibilité d'annuler la dernière action (fonction "Undo")*

⏭️ [Création de dialogues personnalisés](/04-conception-dinterfaces-utilisateur-avec-la-vcl/07-creation-de-dialogues-personnalises.md)
