# 4.11 Accessibilité des interfaces utilisateur

L'accessibilité des interfaces utilisateur consiste à concevoir des applications qui peuvent être utilisées par tous, y compris les personnes ayant différents types de handicaps. Dans cette section, nous allons découvrir comment rendre vos applications Delphi plus accessibles, assurant ainsi qu'elles peuvent être utilisées par un public plus large, tout en respectant les normes et réglementations relatives à l'accessibilité.

## Pourquoi l'accessibilité est-elle importante ?

L'accessibilité n'est pas seulement une bonne pratique, c'est souvent une obligation légale dans de nombreux pays et secteurs. Voici quelques raisons de prendre l'accessibilité au sérieux :

- **Inclusion** : Permettre à tous les utilisateurs d'accéder à votre application
- **Obligations légales** : Se conformer aux lois comme l'ADA (Americans with Disabilities Act) ou les directives WCAG (Web Content Accessibility Guidelines)
- **Marché plus large** : Toucher un public plus vaste, y compris les personnes âgées et celles ayant des handicaps
- **Meilleure expérience pour tous** : Les fonctionnalités d'accessibilité améliorent souvent l'expérience pour tous les utilisateurs

## Types de handicaps à prendre en compte

Pour créer des applications véritablement accessibles, il faut tenir compte de différents types de handicaps :

- **Handicaps visuels** : Cécité, daltonisme, vision partielle
- **Handicaps moteurs** : Difficultés à utiliser un clavier ou une souris
- **Handicaps auditifs** : Surdité ou malentendance
- **Handicaps cognitifs** : Dyslexie, troubles de l'attention, difficultés d'apprentissage

## Principes fondamentaux d'accessibilité

Avant d'entrer dans les détails techniques, voici quelques principes fondamentaux à garder à l'esprit :

1. **Percevoir** : Les informations doivent être perceptibles par tous les utilisateurs
2. **Utiliser** : L'interface doit être utilisable par différents moyens (clavier, souris, commandes vocales)
3. **Comprendre** : Le contenu et le fonctionnement doivent être clairs et prévisibles
4. **Robustesse** : L'application doit fonctionner avec diverses technologies d'assistance

## Rendre une application Delphi accessible

Delphi propose plusieurs fonctionnalités pour améliorer l'accessibilité de vos applications. Voyons comment les mettre en œuvre.

### 1. Support du lecteur d'écran MSAA (Microsoft Active Accessibility)

Delphi supporte MSAA, ce qui permet aux lecteurs d'écran de lire le contenu de votre application. Pour activer cette fonctionnalité :

```pascal
// Ajouter dans uses
uses
  Vcl.Forms, Winapi.Windows, Winapi.oleacc;

// Dans FormCreate
procedure TForm1.FormCreate(Sender: TObject);
begin
  // Activer l'accessibilité pour le formulaire
  Application.AccessibilityProperties.Active := True;
  Application.AccessibilityProperties.Description := 'Formulaire principal de l''application';

  // Pour chaque contrôle important
  Button1.AccessibilityProperties.Active := True;
  Button1.AccessibilityProperties.Description := 'Bouton pour enregistrer le document';
end;
```

### 2. Textes alternatifs et descriptions

Pour les éléments visuels comme les images, fournissez toujours des textes alternatifs :

```pascal
// Pour une image
Image1.Hint := 'Logo de l''entreprise';
Image1.ShowHint := True;

// Pour un contrôle avec AccessibilityProperties
SpeedButton1.AccessibilityProperties.Active := True;
SpeedButton1.AccessibilityProperties.Name := 'Imprimer';
SpeedButton1.AccessibilityProperties.Description := 'Imprime le document actuel';
```

### 3. Navigation au clavier

Assurez-vous que votre application peut être entièrement utilisée avec le clavier :

```pascal
// Définir un ordre de tabulation logique
Edit1.TabOrder := 0;
Edit2.TabOrder := 1;
ComboBox1.TabOrder := 2;
Button1.TabOrder := 3;

// Ajouter des raccourcis clavier
Button1.Caption := '&Enregistrer';  // Alt+E activera ce bouton
MainMenu1.Items[0].Caption := '&Fichier';  // Alt+F ouvrira ce menu
```

### 4. Taille et couleur du texte

Utilisez des polices lisibles et offrez la possibilité de les redimensionner :

```pascal
procedure TForm1.TrackBarFontSizeChange(Sender: TObject);
begin
  // Ajuster la taille de la police dans toute l'application
  Application.DefaultFont.Size := TrackBarFontSize.Position;

  // Appliquer la modification à tous les contrôles
  RecursiveSetFontSize(Self);
end;

procedure TForm1.RecursiveSetFontSize(Control: TWinControl);
var
  i: Integer;
begin
  // Appliquer la taille de police à ce contrôle
  Control.Font.Size := Application.DefaultFont.Size;

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    // Appliquer aux contrôles simples
    if Control.Controls[i] is TControl then
      (Control.Controls[i] as TControl).Font.Size := Application.DefaultFont.Size;

    // Récursion pour les contrôles conteneurs
    if Control.Controls[i] is TWinControl then
      RecursiveSetFontSize(Control.Controls[i] as TWinControl);
  end;
end;
```

### 5. Contraste et daltonisme

Assurez-vous que votre application est utilisable par les personnes daltoniennes et celles ayant besoin d'un contraste élevé :

```pascal
procedure TForm1.CheckBoxHighContrastClick(Sender: TObject);
begin
  if CheckBoxHighContrast.Checked then
  begin
    // Appliquer le mode haut contraste
    Self.Color := clBlack;
    ApplyHighContrastMode(Self, clBlack, clWhite);
  end
  else
  begin
    // Revenir au mode normal
    Self.Color := clBtnFace;
    RestoreNormalMode(Self);
  end;
end;

procedure TForm1.ApplyHighContrastMode(Control: TWinControl; BackColor, ForeColor: TColor);
var
  i: Integer;
begin
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TLabel then
    begin
      (Control.Controls[i] as TLabel).Font.Color := ForeColor;
      (Control.Controls[i] as TLabel).Transparent := True;
    end
    else if Control.Controls[i] is TEdit then
    begin
      (Control.Controls[i] as TEdit).Color := BackColor;
      (Control.Controls[i] as TEdit).Font.Color := ForeColor;
    end
    // Autres types de contrôles...

    // Récursion pour les contrôles conteneurs
    if Control.Controls[i] is TWinControl then
      ApplyHighContrastMode(Control.Controls[i] as TWinControl, BackColor, ForeColor);
  end;
end;
```

### 6. Retour d'information et messages

Assurez-vous que les messages et retours d'information sont clairs et accessibles :

```pascal
procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  try
    // Code pour sauvegarder

    // Message de succès
    ShowMessage('Document enregistré avec succès.');

    // Indication visuelle
    StatusBar1.Panels[0].Text := 'Document enregistré';

    // Son de notification (pour les utilisateurs voyants)
    if CheckBoxSounds.Checked then
      PlaySound('save.wav', 0, SND_FILENAME or SND_ASYNC);
  except
    on E: Exception do
    begin
      // Message d'erreur accessible
      MessageDlg('Erreur lors de l''enregistrement : ' + E.Message,
                 mtError, [mbOK], 0);

      // Indication visuelle
      StatusBar1.Panels[0].Text := 'Erreur d''enregistrement';

      // Son d'erreur
      if CheckBoxSounds.Checked then
        PlaySound('error.wav', 0, SND_FILENAME or SND_ASYNC);
    end;
  end;
end;
```

## Création d'un formulaire accessible complet

Voici un exemple de formulaire conçu avec l'accessibilité à l'esprit :

```pascal
unit FormAccessible;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, Winapi.oleacc;

type
  TFormContact = class(TForm)
    PanelTop: TPanel;
    LabelTitle: TLabel;
    PanelContent: TPanel;
    LabelName: TLabel;
    EditName: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    LabelPhone: TLabel;
    EditPhone: TEdit;
    LabelMessage: TLabel;
    MemoMessage: TMemo;
    PanelBottom: TPanel;
    ButtonSubmit: TButton;
    ButtonCancel: TButton;
    StatusBar1: TStatusBar;
    CheckBoxHighContrast: TCheckBox;
    TrackBarFontSize: TTrackBar;
    LabelFontSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure CheckBoxHighContrastClick(Sender: TObject);
    procedure TrackBarFontSizeChange(Sender: TObject);
    procedure ButtonSubmitClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
  private
    FOriginalColors: TDictionary<TComponent, TColor>;
    FOriginalFontSizes: TDictionary<TComponent, Integer>;
    procedure ApplyHighContrastMode(Control: TWinControl; BackColor, ForeColor: TColor);
    procedure RestoreNormalMode(Control: TWinControl);
    procedure RecursiveSetFontSize(Control: TWinControl);
    procedure SetupAccessibility;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormContact: TFormContact;

implementation

{$R *.dfm}

constructor TFormContact.Create(AOwner: TComponent);
begin
  inherited;
  FOriginalColors := TDictionary<TComponent, TColor>.Create;
  FOriginalFontSizes := TDictionary<TComponent, Integer>.Create;
end;

destructor TFormContact.Destroy;
begin
  FOriginalColors.Free;
  FOriginalFontSizes.Free;
  inherited;
end;

procedure TFormContact.FormCreate(Sender: TObject);
begin
  // Configuration initiale
  Caption := 'Formulaire de contact accessible';

  // Configurer l'ordre de tabulation (TabOrder)
  EditName.TabOrder := 0;
  EditEmail.TabOrder := 1;
  EditPhone.TabOrder := 2;
  MemoMessage.TabOrder := 3;
  ButtonSubmit.TabOrder := 4;
  ButtonCancel.TabOrder := 5;
  CheckBoxHighContrast.TabOrder := 6;
  TrackBarFontSize.TabOrder := 7;

  // Configurer les raccourcis clavier
  ButtonSubmit.Caption := '&Envoyer';
  ButtonCancel.Caption := '&Annuler';

  // Configurer les infobulles
  EditName.Hint := 'Entrez votre nom complet';
  EditName.ShowHint := True;
  EditEmail.Hint := 'Entrez votre adresse e-mail';
  EditEmail.ShowHint := True;
  EditPhone.Hint := 'Entrez votre numéro de téléphone';
  EditPhone.ShowHint := True;
  MemoMessage.Hint := 'Entrez votre message';
  MemoMessage.ShowHint := True;

  // Configurer le TrackBar
  TrackBarFontSize.Min := 8;
  TrackBarFontSize.Max := 24;
  TrackBarFontSize.Position := 10;
  TrackBarFontSize.Frequency := 2;

  // Configurer les propriétés d'accessibilité
  SetupAccessibility;

  // Créer des couleurs par défaut pour le mode haut contraste
  for Control in FOriginalColors do
    FOriginalColors.Add(Control, TColor(Control.Tag));

  // Stocker les tailles de police originales
  RecursiveStoreOriginalFontSizes(Self);
end;

procedure TFormContact.SetupAccessibility;
begin
  // Activer l'accessibilité pour le formulaire
  Application.AccessibilityProperties.Active := True;
  Application.AccessibilityProperties.Name := 'ContactForm';
  Application.AccessibilityProperties.Description := 'Formulaire pour envoyer un message de contact';

  // Configurer l'accessibilité pour chaque contrôle important
  EditName.AccessibilityProperties.Active := True;
  EditName.AccessibilityProperties.Name := 'NomComplet';
  EditName.AccessibilityProperties.Description := 'Champ pour entrer votre nom complet';

  EditEmail.AccessibilityProperties.Active := True;
  EditEmail.AccessibilityProperties.Name := 'AdresseEmail';
  EditEmail.AccessibilityProperties.Description := 'Champ pour entrer votre adresse e-mail';

  EditPhone.AccessibilityProperties.Active := True;
  EditPhone.AccessibilityProperties.Name := 'NumeroTelephone';
  EditPhone.AccessibilityProperties.Description := 'Champ pour entrer votre numéro de téléphone';

  MemoMessage.AccessibilityProperties.Active := True;
  MemoMessage.AccessibilityProperties.Name := 'Message';
  MemoMessage.AccessibilityProperties.Description := 'Zone pour entrer votre message';

  ButtonSubmit.AccessibilityProperties.Active := True;
  ButtonSubmit.AccessibilityProperties.Name := 'BoutonEnvoyer';
  ButtonSubmit.AccessibilityProperties.Description := 'Bouton pour envoyer le formulaire';

  ButtonCancel.AccessibilityProperties.Active := True;
  ButtonCancel.AccessibilityProperties.Name := 'BoutonAnnuler';
  ButtonCancel.AccessibilityProperties.Description := 'Bouton pour annuler et réinitialiser le formulaire';

  CheckBoxHighContrast.AccessibilityProperties.Active := True;
  CheckBoxHighContrast.AccessibilityProperties.Name := 'OptionHautContraste';
  CheckBoxHighContrast.AccessibilityProperties.Description := 'Option pour activer le mode haut contraste';

  TrackBarFontSize.AccessibilityProperties.Active := True;
  TrackBarFontSize.AccessibilityProperties.Name := 'TaillePolice';
  TrackBarFontSize.AccessibilityProperties.Description := 'Contrôle pour ajuster la taille de la police';
end;

procedure TFormContact.ApplyHighContrastMode(Control: TWinControl; BackColor, ForeColor: TColor);
var
  i: Integer;
  ChildControl: TControl;
begin
  // Appliquer les couleurs au contrôle lui-même
  Control.Color := BackColor;
  Control.Font.Color := ForeColor;

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    ChildControl := Control.Controls[i];

    // Stocker la couleur originale si ce n'est pas déjà fait
    if not FOriginalColors.ContainsKey(ChildControl) then
    begin
      if ChildControl is TEdit then
        FOriginalColors.Add(ChildControl, (ChildControl as TEdit).Color)
      else if ChildControl is TMemo then
        FOriginalColors.Add(ChildControl, (ChildControl as TMemo).Color)
      else if ChildControl is TLabel then
        FOriginalColors.Add(ChildControl, (ChildControl as TLabel).Font.Color)
      else
        FOriginalColors.Add(ChildControl, ChildControl.Brush.Color);
    end;

    // Appliquer les couleurs spécifiques à chaque type de contrôle
    if ChildControl is TLabel then
    begin
      (ChildControl as TLabel).Font.Color := ForeColor;
      (ChildControl as TLabel).Transparent := True;
    end
    else if ChildControl is TEdit then
    begin
      (ChildControl as TEdit).Color := BackColor;
      (ChildControl as TEdit).Font.Color := ForeColor;
    end
    else if ChildControl is TMemo then
    begin
      (ChildControl as TMemo).Color := BackColor;
      (ChildControl as TMemo).Font.Color := ForeColor;
    end
    else if ChildControl is TButton then
    begin
      (ChildControl as TButton).Font.Color := ForeColor;
    end;

    // Récursion pour les contrôles conteneurs
    if ChildControl is TWinControl then
      ApplyHighContrastMode(ChildControl as TWinControl, BackColor, ForeColor);
  end;
end;

procedure TFormContact.RestoreNormalMode(Control: TWinControl);
var
  i: Integer;
  ChildControl: TControl;
  OriginalColor: TColor;
begin
  // Restaurer la couleur du contrôle lui-même
  Control.Color := clBtnFace;
  Control.Font.Color := clWindowText;

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    ChildControl := Control.Controls[i];

    // Récupérer la couleur originale
    if FOriginalColors.TryGetValue(ChildControl, OriginalColor) then
    begin
      if ChildControl is TEdit then
        (ChildControl as TEdit).Color := clWindow
      else if ChildControl is TMemo then
        (ChildControl as TMemo).Color := clWindow
      else if ChildControl is TLabel then
        (ChildControl as TLabel).Font.Color := clWindowText;
    end;

    // Récursion pour les contrôles conteneurs
    if ChildControl is TWinControl then
      RestoreNormalMode(ChildControl as TWinControl);
  end;
end;

procedure TFormContact.RecursiveSetFontSize(Control: TWinControl);
var
  i: Integer;
  NewSize: Integer;
begin
  // Obtenir la nouvelle taille de police
  NewSize := TrackBarFontSize.Position;

  // Appliquer à ce contrôle
  Control.Font.Size := NewSize;

  // Mettre à jour le libellé
  LabelFontSize.Caption := 'Taille de police : ' + IntToStr(NewSize);

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TControl then
      (Control.Controls[i] as TControl).Font.Size := NewSize;

    // Récursion pour les contrôles conteneurs
    if Control.Controls[i] is TWinControl then
      RecursiveSetFontSize(Control.Controls[i] as TWinControl);
  end;
end;

procedure TFormContact.RecursiveStoreOriginalFontSizes(Control: TWinControl);
var
  i: Integer;
  ChildControl: TControl;
begin
  // Stocker la taille de police de ce contrôle
  FOriginalFontSizes.Add(Control, Control.Font.Size);

  // Parcourir tous les contrôles enfants
  for i := 0 to Control.ControlCount - 1 do
  begin
    ChildControl := Control.Controls[i];

    FOriginalFontSizes.Add(ChildControl, ChildControl.Font.Size);

    // Récursion pour les contrôles conteneurs
    if ChildControl is TWinControl then
      RecursiveStoreOriginalFontSizes(ChildControl as TWinControl);
  end;
end;

procedure TFormContact.CheckBoxHighContrastClick(Sender: TObject);
begin
  if CheckBoxHighContrast.Checked then
  begin
    // Appliquer le mode haut contraste
    ApplyHighContrastMode(Self, clBlack, clWhite);
    StatusBar1.SimpleText := 'Mode haut contraste activé';
  end
  else
  begin
    // Restaurer le mode normal
    RestoreNormalMode(Self);
    StatusBar1.SimpleText := 'Mode normal';
  end;
end;

procedure TFormContact.TrackBarFontSizeChange(Sender: TObject);
begin
  // Mettre à jour la taille de la police dans tout le formulaire
  RecursiveSetFontSize(Self);
  StatusBar1.SimpleText := 'Taille de police modifiée : ' + IntToStr(TrackBarFontSize.Position);
end;

procedure TFormContact.EditKeyPress(Sender: TObject; var Key: Char);
begin
  // Validation de champs spécifiques
  if (Sender = EditPhone) and not (Key in ['0'..'9', '+', '-', '(', ')', #8]) then
  begin
    Key := #0;  // Ignorer les caractères non numériques pour le téléphone
    StatusBar1.SimpleText := 'Veuillez entrer uniquement des chiffres dans le champ téléphone';
    Beep;  // Retour audio pour indiquer une erreur
  end;
end;

procedure TFormContact.ButtonSubmitClick(Sender: TObject);
begin
  // Validation avant envoi
  if Trim(EditName.Text) = '' then
  begin
    MessageDlg('Veuillez entrer votre nom.', mtWarning, [mbOK], 0);
    EditName.SetFocus;
    Exit;
  end;

  if Trim(EditEmail.Text) = '' then
  begin
    MessageDlg('Veuillez entrer votre adresse e-mail.', mtWarning, [mbOK], 0);
    EditEmail.SetFocus;
    Exit;
  end;

  if Trim(MemoMessage.Text) = '' then
  begin
    MessageDlg('Veuillez entrer votre message.', mtWarning, [mbOK], 0);
    MemoMessage.SetFocus;
    Exit;
  end;

  // Simulation d'envoi
  StatusBar1.SimpleText := 'Message envoyé avec succès !';
  MessageDlg('Votre message a été envoyé avec succès !', mtInformation, [mbOK], 0);

  // Réinitialiser le formulaire
  EditName.Clear;
  EditEmail.Clear;
  EditPhone.Clear;
  MemoMessage.Clear;
  EditName.SetFocus;
end;

procedure TFormContact.ButtonCancelClick(Sender: TObject);
begin
  // Demander confirmation
  if MessageDlg('Voulez-vous vraiment annuler et effacer tous les champs ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    // Réinitialiser le formulaire
    EditName.Clear;
    EditEmail.Clear;
    EditPhone.Clear;
    MemoMessage.Clear;
    EditName.SetFocus;
    StatusBar1.SimpleText := 'Formulaire réinitialisé';
  end;
end;

end.
```

## Test d'accessibilité

Pour évaluer l'accessibilité de votre application, vous pouvez utiliser plusieurs outils et techniques :

### 1. Testez avec un lecteur d'écran

Utilisez NVDA (gratuit) ou JAWS pour naviguer dans votre application et vérifier que tous les éléments sont correctement annoncés.

### 2. Testez la navigation au clavier

Essayez d'utiliser votre application uniquement avec le clavier (sans souris) pour vérifier que toutes les fonctionnalités sont accessibles.

### 3. Testez le contraste et les couleurs

Utilisez des outils de simulation de daltonisme comme Color Oracle pour voir comment votre application apparaît aux personnes daltoniennes.

### 4. Liste de vérification d'accessibilité

Voici une liste de vérification simple que vous pouvez utiliser pour évaluer votre application :

- [ ] Tous les contrôles ont des textes alternatifs ou des descriptions
- [ ] Toutes les fonctionnalités sont accessibles au clavier
- [ ] L'ordre de tabulation est logique
- [ ] Le contraste des couleurs est suffisant
- [ ] La taille du texte peut être ajustée
- [ ] Les messages d'erreur sont clairs et accessibles
- [ ] Les retours d'information sont disponibles via plusieurs canaux (visuel, audio)
- [ ] L'application reste utilisable avec un lecteur d'écran

## Ressources d'accessibilité

Voici quelques ressources utiles pour approfondir vos connaissances sur l'accessibilité :

- [Microsoft Active Accessibility (MSAA)](https://docs.microsoft.com/en-us/windows/win32/accessibility/microsoft-active-accessibility)
- [Web Content Accessibility Guidelines (WCAG)](https://www.w3.org/WAI/standards-guidelines/wcag/)
- [WebAIM Contrast Checker](https://webaim.org/resources/contrastchecker/)
- [NVDA Screen Reader](https://www.nvaccess.org/)
- [Color Oracle (simulation de daltonisme)](https://colororacle.org/)

## Conclusion

Créer des applications accessibles est à la fois une responsabilité éthique et, dans de nombreux cas, une obligation légale. En suivant les principes et techniques présentés dans cette section, vous pouvez rendre vos applications Delphi plus inclusives et utilisables par un public plus large.

L'accessibilité n'est pas une fonctionnalité à ajouter à la fin du développement, mais plutôt une considération à intégrer dès le début de la conception. En pensant à l'accessibilité dès le départ, vous créerez des applications plus robustes, plus flexibles et plus conviviales pour tous les utilisateurs.

---

*Exercice pratique : Prenez une application Delphi existante et améliorez son accessibilité en ajoutant des propriétés d'accessibilité, en optimisant la navigation au clavier et en ajoutant une option de haut contraste et de redimensionnement de police. Testez votre application avec un lecteur d'écran et uniquement avec le clavier pour vérifier son accessibilité.*
