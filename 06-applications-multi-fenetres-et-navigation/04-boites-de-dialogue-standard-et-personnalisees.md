🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.4 Boîtes de dialogue standard et personnalisées

## Introduction

Les boîtes de dialogue sont des fenêtres modales temporaires qui permettent d'interagir avec l'utilisateur. Elles sont essentielles dans toute application pour :

- Afficher des messages d'information ou d'erreur
- Demander une confirmation avant une action importante
- Recueillir des informations simples
- Permettre la sélection de fichiers, couleurs, polices, etc.

Delphi offre un ensemble complet de boîtes de dialogue prêtes à l'emploi, ainsi que la possibilité de créer vos propres boîtes de dialogue personnalisées.

## Boîtes de dialogue de messages simples

### ShowMessage - Le message le plus simple

La fonction `ShowMessage` affiche un message d'information avec un seul bouton OK.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  ShowMessage('Opération terminée avec succès !');
end;
```

**Caractéristiques :**
- Bloque l'exécution jusqu'à ce que l'utilisateur clique sur OK
- Ne retourne aucune valeur
- Titre de la fenêtre = nom de l'application
- Idéal pour les messages informatifs simples

### MessageDlg - Boîte de dialogue configurable

La fonction `MessageDlg` offre plus de flexibilité avec différents types de messages et boutons.

**Syntaxe de base :**
```pascal
function MessageDlg(const Msg: string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
```

**Exemple simple :**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
begin  
  MessageDlg('Fichier enregistré avec succès', mtInformation, [mbOK], 0);
end;
```

### Types de messages (TMsgDlgType)

```pascal
// Information
MessageDlg('Opération réussie', mtInformation, [mbOK], 0);

// Avertissement
MessageDlg('Attention : cette action est irréversible', mtWarning, [mbOK], 0);

// Erreur
MessageDlg('Une erreur est survenue', mtError, [mbOK], 0);

// Confirmation
MessageDlg('Voulez-vous continuer ?', mtConfirmation, [mbYes, mbNo], 0);

// Message personnalisé (sans icône prédéfinie)
MessageDlg('Message personnalisé', mtCustom, [mbOK], 0);
```

**Icônes affichées :**
- `mtInformation` : Icône "i" bleue
- `mtWarning` : Icône point d'exclamation jaune
- `mtError` : Icône "X" rouge
- `mtConfirmation` : Icône point d'interrogation
- `mtCustom` : Pas d'icône

### Boutons disponibles (TMsgDlgButtons)

```pascal
// Bouton unique
[mbOK]          // OK
[mbCancel]      // Annuler

// Combinaisons courantes
[mbYes, mbNo]                    // Oui / Non
[mbYes, mbNo, mbCancel]          // Oui / Non / Annuler
[mbOK, mbCancel]                 // OK / Annuler
[mbAbort, mbRetry, mbIgnore]     // Abandonner / Réessayer / Ignorer
[mbYesToAll, mbNoToAll]          // Oui pour tout / Non pour tout

// Toutes les options
[mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll,
 mbNoToAll, mbYesToAll, mbHelp, mbClose]
```

### Valeurs de retour (ModalResult)

```pascal
procedure TForm1.SupprimerClick(Sender: TObject);  
var  
  Reponse: Integer;
begin
  Reponse := MessageDlg('Voulez-vous vraiment supprimer cet élément ?',
    mtConfirmation, [mbYes, mbNo], 0);

  if Reponse = mrYes then
  begin
    // Supprimer l'élément
    ShowMessage('Élément supprimé');
  end
  else
  begin
    ShowMessage('Suppression annulée');
  end;
end;
```

**Constantes ModalResult :**
- `mrNone` = 0 : Aucun résultat
- `mrOk` = 1 : OK
- `mrCancel` = 2 : Annuler
- `mrAbort` = 3 : Abandonner
- `mrRetry` = 4 : Réessayer
- `mrIgnore` = 5 : Ignorer
- `mrYes` = 6 : Oui
- `mrNo` = 7 : Non
- `mrAll` = 8 : Tout
- `mrNoToAll` = 9 : Non pour tout
- `mrYesToAll` = 10 : Oui pour tout

### MessageDlgPos - Position personnalisée

Affiche la boîte de dialogue à une position spécifique :

```pascal
MessageDlgPos('Message positionné', mtInformation, [mbOK], 0, 100, 100);
// X = 100, Y = 100
```

## Boîtes de saisie simples

### InputBox - Saisie de texte simple

Demande à l'utilisateur de saisir une chaîne de caractères.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nom: string;
begin
  Nom := InputBox('Saisie', 'Entrez votre nom :', '');

  if Nom <> '' then
    ShowMessage('Bonjour ' + Nom)
  else
    ShowMessage('Aucun nom saisi');
end;
```

**Syntaxe :**
```pascal
function InputBox(const ACaption, APrompt, ADefault: string): string;
```

- `ACaption` : Titre de la boîte de dialogue
- `APrompt` : Message/question affiché
- `ADefault` : Valeur par défaut dans le champ de saisie

**Exemple avec valeur par défaut :**
```pascal
var
  Ville: string;
begin
  Ville := InputBox('Localisation', 'Quelle est votre ville ?', 'Paris');
  ShowMessage('Ville : ' + Ville);
end;
```

### InputQuery - Saisie avec validation

Permet de vérifier si l'utilisateur a cliqué sur OK ou Annuler.

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nom: string;
begin
  Nom := '';

  if InputQuery('Identification', 'Entrez votre nom :', Nom) then
  begin
    if Trim(Nom) <> '' then
      ShowMessage('Bonjour ' + Nom)
    else
      ShowMessage('Nom invalide');
  end
  else
    ShowMessage('Saisie annulée');
end;
```

**Différence avec InputBox :**
- `InputBox` retourne toujours une chaîne (vide si annulé)
- `InputQuery` retourne `True` si OK, `False` si Annuler

### Saisie de mot de passe

```pascal
var
  MotDePasse: string;
begin
  MotDePasse := '';
  if InputQuery('Authentification', 'Mot de passe :', MotDePasse, True) then
  begin
    // Le paramètre True masque le texte saisi (****)
    if MotDePasse = 'secret' then
      ShowMessage('Accès autorisé')
    else
      ShowMessage('Mot de passe incorrect');
  end;
end;
```

## Boîtes de dialogue de fichiers

### TOpenDialog - Ouvrir un fichier

Permet à l'utilisateur de sélectionner un ou plusieurs fichiers.

**Placement du composant :**
1. Dans la Palette d'outils, onglet "Dialogs"
2. Glisser `TOpenDialog` sur le formulaire
3. Le composant est invisible à l'exécution

**Utilisation simple :**
```pascal
procedure TForm1.ButtonOuvrirClick(Sender: TObject);  
begin  
  if OpenDialog1.Execute then
  begin
    // L'utilisateur a sélectionné un fichier
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
    ShowMessage('Fichier ouvert : ' + OpenDialog1.FileName);
  end
  else
  begin
    ShowMessage('Ouverture annulée');
  end;
end;
```

**Configuration des propriétés :**

```pascal
procedure TForm1.FormCreate(Sender: TObject);  
begin  
  with OpenDialog1 do
  begin
    // Titre de la boîte de dialogue
    Title := 'Ouvrir un fichier texte';

    // Filtre de types de fichiers
    Filter := 'Fichiers texte (*.txt)|*.TXT|Tous les fichiers (*.*)|*.*';

    // Index du filtre par défaut (1 = premier filtre)
    FilterIndex := 1;

    // Répertoire initial
    InitialDir := 'C:\Documents';

    // Options
    Options := [ofFileMustExist, ofPathMustExist, ofHideReadOnly];
  end;
end;
```

**Options importantes :**

```pascal
Options := [
  ofReadOnly,           // Affiche une case à cocher "Lecture seule"
  ofOverwritePrompt,    // Demande confirmation si fichier existe
  ofHideReadOnly,       // Cache la case "Lecture seule"
  ofNoChangeDir,        // Ne change pas le répertoire courant
  ofAllowMultiSelect,   // Permet de sélectionner plusieurs fichiers
  ofFileMustExist,      // Le fichier doit exister
  ofPathMustExist,      // Le chemin doit exister
  ofNoValidate,         // Pas de validation du nom
  ofShowHelp            // Affiche un bouton Aide
];
```

**Sélection multiple :**
```pascal
procedure TForm1.ButtonOuvrirPlusieursFichiersClick(Sender: TObject);  
var  
  i: Integer;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];

  if OpenDialog1.Execute then
  begin
    ListBox1.Clear;
    for i := 0 to OpenDialog1.Files.Count - 1 do
      ListBox1.Items.Add(OpenDialog1.Files[i]);

    ShowMessage(IntToStr(OpenDialog1.Files.Count) + ' fichier(s) sélectionné(s)');
  end;
end;
```

**Filtres de fichiers :**
```pascal
// Format : 'Description|Masque|Description|Masque|...'

// Fichiers texte uniquement
Filter := 'Fichiers texte (*.txt)|*.TXT';

// Plusieurs types
Filter := 'Images (*.bmp;*.jpg)|*.BMP;*.JPG|Tous fichiers (*.*)|*.*';

// Séparé pour plus de clarté
Filter := 'Fichiers Word (*.doc;*.docx)|*.DOC;*.DOCX|' +
          'Fichiers Excel (*.xls;*.xlsx)|*.XLS;*.XLSX|' +
          'Tous les fichiers (*.*)|*.*';
```

### TSaveDialog - Enregistrer un fichier

Permet à l'utilisateur de spécifier un nom de fichier pour l'enregistrement.

```pascal
procedure TForm1.ButtonEnregistrerClick(Sender: TObject);  
begin  
  SaveDialog1.FileName := 'MonDocument.txt';
  SaveDialog1.Filter := 'Fichiers texte (*.txt)|*.TXT';
  SaveDialog1.DefaultExt := 'txt';  // Extension par défaut
  SaveDialog1.Options := [ofOverwritePrompt, ofPathMustExist];

  if SaveDialog1.Execute then
  begin
    Memo1.Lines.SaveToFile(SaveDialog1.FileName);
    ShowMessage('Fichier enregistré : ' + SaveDialog1.FileName);
  end;
end;
```

**Option importante pour SaveDialog :**
- `ofOverwritePrompt` : Demande confirmation si le fichier existe déjà

### TOpenPictureDialog et TSavePictureDialog

Boîtes de dialogue spécialisées pour les images avec aperçu.

```pascal
procedure TForm1.ButtonOuvrirImageClick(Sender: TObject);  
begin  
  if OpenPictureDialog1.Execute then
  begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  end;
end;
```

**Filtres automatiques :**
Les filtres pour les formats d'image sont automatiquement configurés (BMP, JPG, PNG, GIF, etc.).

## Autres boîtes de dialogue standard

### TColorDialog - Sélection de couleur

```pascal
procedure TForm1.ButtonCouleurClick(Sender: TObject);  
begin  
  ColorDialog1.Color := Panel1.Color;  // Couleur initiale

  if ColorDialog1.Execute then
  begin
    Panel1.Color := ColorDialog1.Color;
    ShowMessage('Couleur RGB : ' +
      IntToStr(GetRValue(ColorDialog1.Color)) + ', ' +
      IntToStr(GetGValue(ColorDialog1.Color)) + ', ' +
      IntToStr(GetBValue(ColorDialog1.Color)));
  end;
end;
```

**Options du ColorDialog :**
```pascal
ColorDialog1.Options := [
  cdFullOpen,        // Affiche les couleurs personnalisées
  cdSolidColor,      // Seulement les couleurs pleines
  cdAnyColor         // Toutes les couleurs
];
```

### TFontDialog - Sélection de police

```pascal
procedure TForm1.ButtonPoliceClick(Sender: TObject);  
begin  
  FontDialog1.Font := Memo1.Font;  // Police actuelle

  if FontDialog1.Execute then
  begin
    Memo1.Font := FontDialog1.Font;
  end;
end;
```

**Options du FontDialog :**
```pascal
FontDialog1.Options := [
  fdEffects,          // Affiche effets (barré, souligné)
  fdApplyButton,      // Bouton "Appliquer"
  fdForceFontExist,   // La police doit exister
  fdLimitSize,        // Limiter les tailles (MinFontSize, MaxFontSize)
  fdScalableOnly,     // Seulement les polices redimensionnables
  fdTrueTypeOnly,     // Seulement les polices TrueType
  fdFixedPitchOnly,   // Seulement les polices à espacement fixe
  fdShowHelp          // Affiche le bouton Aide
];

FontDialog1.MinFontSize := 8;  
FontDialog1.MaxFontSize := 72;  
```

### TPrinterSetupDialog - Configuration de l'imprimante

```pascal
procedure TForm1.ButtonImprimanteClick(Sender: TObject);  
begin  
  if PrinterSetupDialog1.Execute then
  begin
    ShowMessage('Imprimante configurée');
  end;
end;
```

### TPrintDialog - Boîte d'impression

```pascal
procedure TForm1.ButtonImprimerClick(Sender: TObject);  
begin  
  PrintDialog1.MinPage := 1;
  PrintDialog1.MaxPage := 10;
  PrintDialog1.FromPage := 1;
  PrintDialog1.ToPage := 10;

  if PrintDialog1.Execute then
  begin
    // Impression selon les paramètres choisis
    ShowMessage(Format('Imprimer de la page %d à %d, %d copie(s)',
      [PrintDialog1.FromPage, PrintDialog1.ToPage, PrintDialog1.Copies]));
  end;
end;
```

### TFindDialog et TReplaceDialog - Rechercher et remplacer

```pascal
// Rechercher
procedure TForm1.ButtonRechercherClick(Sender: TObject);  
begin  
  FindDialog1.Execute;
end;

procedure TForm1.FindDialog1Find(Sender: TObject);  
var  
  Position: Integer;
  Texte: string;
begin
  Texte := Memo1.Text;
  Position := Pos(FindDialog1.FindText, Texte);

  if Position > 0 then
  begin
    Memo1.SelStart := Position - 1;
    Memo1.SelLength := Length(FindDialog1.FindText);
    Memo1.SetFocus;
  end
  else
    ShowMessage('Texte non trouvé');
end;

// Remplacer
procedure TForm1.ButtonRemplacerClick(Sender: TObject);  
begin  
  ReplaceDialog1.Execute;
end;

procedure TForm1.ReplaceDialog1Replace(Sender: TObject);  
var  
  Texte: string;
begin
  Texte := Memo1.Text;

  if frReplaceAll in ReplaceDialog1.Options then
  begin
    // Remplacer tout
    Texte := StringReplace(Texte, ReplaceDialog1.FindText,
      ReplaceDialog1.ReplaceText, [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    // Remplacer l'occurrence actuelle
    Texte := StringReplace(Texte, ReplaceDialog1.FindText,
      ReplaceDialog1.ReplaceText, [rfIgnoreCase]);
  end;

  Memo1.Text := Texte;
end;
```

## Créer des boîtes de dialogue personnalisées

### Méthode 1 : Formulaire modal simple

**Étape 1 : Créer le formulaire de dialogue**

```pascal
unit UnitDialogPersonnalise;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormDialog = class(TForm)
    LabelQuestion: TLabel;
    EditReponse: TEdit;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    procedure ButtonOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetReponse: string;
    procedure SetReponse(const Value: string);
  public
    property Reponse: string read GetReponse write SetReponse;
  end;

var
  FormDialog: TFormDialog;

implementation

{$R *.dfm}

procedure TFormDialog.FormCreate(Sender: TObject);  
begin  
  // Configuration du formulaire
  Position := poScreenCenter;
  BorderStyle := bsDialog;  // Pas redimensionnable

  // Configuration des boutons
  ButtonOK.Default := True;        // Bouton par défaut (Entrée)
  ButtonOK.ModalResult := mrOk;
  ButtonAnnuler.Cancel := True;    // Bouton annulation (Échap)
  ButtonAnnuler.ModalResult := mrCancel;
end;

procedure TFormDialog.ButtonOKClick(Sender: TObject);  
begin  
  if Trim(EditReponse.Text) = '' then
  begin
    ShowMessage('Veuillez saisir une réponse');
    ModalResult := mrNone;  // Empêche la fermeture
    EditReponse.SetFocus;
  end;
end;

function TFormDialog.GetReponse: string;  
begin  
  Result := EditReponse.Text;
end;

procedure TFormDialog.SetReponse(const Value: string);  
begin  
  EditReponse.Text := Value;
end;

end.
```

**Étape 2 : Utiliser le dialogue personnalisé**

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Dialog: TFormDialog;
begin
  Dialog := TFormDialog.Create(Self);
  try
    Dialog.Caption := 'Informations personnelles';
    Dialog.LabelQuestion.Caption := 'Quel est votre nom ?';
    Dialog.Reponse := 'Jean Dupont';  // Valeur par défaut

    if Dialog.ShowModal = mrOk then
    begin
      ShowMessage('Nom saisi : ' + Dialog.Reponse);
    end
    else
    begin
      ShowMessage('Saisie annulée');
    end;
  finally
    Dialog.Free;
  end;
end;
```

### Méthode 2 : Fonction utilitaire encapsulée

Créer une fonction qui encapsule la création et l'utilisation du dialogue :

```pascal
function DemanderNomUtilisateur(var Nom: string): Boolean;  
var  
  Dialog: TFormDialog;
begin
  Result := False;
  Dialog := TFormDialog.Create(nil);
  try
    Dialog.Caption := 'Identification';
    Dialog.LabelQuestion.Caption := 'Entrez votre nom :';
    Dialog.Reponse := Nom;

    Result := Dialog.ShowModal = mrOk;
    if Result then
      Nom := Dialog.Reponse;
  finally
    Dialog.Free;
  end;
end;

// Utilisation
procedure TForm1.Button1Click(Sender: TObject);  
var  
  Nom: string;
begin
  Nom := '';
  if DemanderNomUtilisateur(Nom) then
    ShowMessage('Bonjour ' + Nom);
end;
```

### Exemple : Dialogue de connexion

```pascal
type
  TFormConnexion = class(TForm)
    LabelUtilisateur: TLabel;
    EditUtilisateur: TEdit;
    LabelMotDePasse: TLabel;
    EditMotDePasse: TEdit;
    CheckBoxMemoriser: TCheckBox;
    ButtonConnexion: TButton;
    ButtonAnnuler: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonConnexionClick(Sender: TObject);
  private
    function GetUtilisateur: string;
    function GetMotDePasse: string;
    function GetMemoriser: Boolean;
  public
    property Utilisateur: string read GetUtilisateur;
    property MotDePasse: string read GetMotDePasse;
    property Memoriser: Boolean read GetMemoriser;
  end;

implementation

procedure TFormConnexion.FormCreate(Sender: TObject);  
begin  
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  EditMotDePasse.PasswordChar := '*';  // Masquer le mot de passe

  ButtonConnexion.Default := True;
  ButtonConnexion.ModalResult := mrOk;
  ButtonAnnuler.Cancel := True;
  ButtonAnnuler.ModalResult := mrCancel;
end;

procedure TFormConnexion.ButtonConnexionClick(Sender: TObject);  
begin  
  if (Trim(EditUtilisateur.Text) = '') or (Trim(EditMotDePasse.Text) = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs');
    ModalResult := mrNone;
    Exit;
  end;
end;

function TFormConnexion.GetUtilisateur: string;  
begin  
  Result := EditUtilisateur.Text;
end;

function TFormConnexion.GetMotDePasse: string;  
begin  
  Result := EditMotDePasse.Text;
end;

function TFormConnexion.GetMemoriser: Boolean;  
begin  
  Result := CheckBoxMemoriser.Checked;
end;

// Utilisation
procedure TForm1.ConnexionClick(Sender: TObject);  
var  
  FormConn: TFormConnexion;
begin
  FormConn := TFormConnexion.Create(Self);
  try
    if FormConn.ShowModal = mrOk then
    begin
      // Vérifier les identifiants
      if (FormConn.Utilisateur = 'admin') and (FormConn.MotDePasse = 'secret') then
      begin
        ShowMessage('Connexion réussie !');
        // Mémoriser si demandé
        if FormConn.Memoriser then
        begin
          // Sauvegarder les préférences
        end;
      end
      else
      begin
        ShowMessage('Identifiants incorrects');
      end;
    end;
  finally
    FormConn.Free;
  end;
end;
```

### Exemple : Dialogue avec validation complexe

```pascal
type
  TFormInscription = class(TForm)
    EditNom: TEdit;
    EditEmail: TEdit;
    EditAge: TEdit;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    procedure ButtonOKClick(Sender: TObject);
  private
    function ValiderEmail(const Email: string): Boolean;
    function ValiderAge(const Age: string): Boolean;
  end;

implementation

uses
  System.RegularExpressions;

procedure TFormInscription.ButtonOKClick(Sender: TObject);  
begin  
  // Validation du nom
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire');
    EditNom.SetFocus;
    ModalResult := mrNone;
    Exit;
  end;

  // Validation de l'email
  if not ValiderEmail(EditEmail.Text) then
  begin
    ShowMessage('L''adresse email est invalide');
    EditEmail.SetFocus;
    ModalResult := mrNone;
    Exit;
  end;

  // Validation de l'âge
  if not ValiderAge(EditAge.Text) then
  begin
    ShowMessage('L''âge doit être un nombre entre 18 et 120');
    EditAge.SetFocus;
    ModalResult := mrNone;
    Exit;
  end;

  // Toutes les validations sont OK
  ModalResult := mrOk;
end;

function TFormInscription.ValiderEmail(const Email: string): Boolean;  
begin  
  Result := TRegEx.IsMatch(Email, '^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$');
end;

function TFormInscription.ValiderAge(const Age: string): Boolean;  
var  
  AgeNum: Integer;
begin
  Result := TryStrToInt(Age, AgeNum) and (AgeNum >= 18) and (AgeNum <= 120);
end;
```

## Personnalisation avancée

### Changer les couleurs et le style

```pascal
procedure TFormDialog.FormCreate(Sender: TObject);  
begin  
  // Couleur de fond
  Color := clWhite;

  // Style des boutons
  ButtonOK.Font.Style := [fsBold];
  ButtonOK.Height := 30;

  // Bordure personnalisée
  BorderStyle := bsSingle;
  BorderIcons := [biSystemMenu];  // Seulement le bouton fermer
end;
```

### Ajouter une icône personnalisée

```pascal
procedure TFormDialog.FormCreate(Sender: TObject);  
var  
  Image: TImage;
begin
  // Ajouter une image
  Image := TImage.Create(Self);
  Image.Parent := Self;
  Image.Picture.LoadFromFile('icone.png');
  Image.Left := 10;
  Image.Top := 10;
end;
```

### Dialogue avec barre de progression

```pascal
type
  TFormProgression = class(TForm)
    ProgressBar1: TProgressBar;
    LabelMessage: TLabel;
    ButtonAnnuler: TButton;
  private
    FAnnule: Boolean;
  public
    property Annule: Boolean read FAnnule;
    procedure MettreAJour(const Message: string; Position: Integer);
  end;

implementation

procedure TFormProgression.MettreAJour(const Message: string; Position: Integer);  
begin  
  LabelMessage.Caption := Message;
  ProgressBar1.Position := Position;
  Application.ProcessMessages;  // Rafraîchir l'interface
end;

// Utilisation
procedure TForm1.TraitementLongClick(Sender: TObject);  
var  
  FormProg: TFormProgression;
  i: Integer;
begin
  FormProg := TFormProgression.Create(Self);
  try
    FormProg.Show;

    for i := 1 to 100 do
    begin
      if FormProg.Annule then
      begin
        ShowMessage('Traitement annulé');
        Break;
      end;

      FormProg.MettreAJour('Traitement en cours... ' + IntToStr(i) + '%', i);
      Sleep(50);  // Simulation d'un traitement
    end;
  finally
    FormProg.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours libérer les dialogues personnalisés

```pascal
// BON
var
  Dialog: TFormDialog;
begin
  Dialog := TFormDialog.Create(Self);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

// MAUVAIS - Fuite mémoire
begin
  TFormDialog.Create(Self).ShowModal;
  // Le formulaire n'est jamais libéré !
end;
```

### 2. Définir les propriétés Default et Cancel

```pascal
ButtonOK.Default := True;      // Associe à la touche Entrée  
ButtonAnnuler.Cancel := True;  // Associe à la touche Échap  
```

### 3. Utiliser BorderStyle approprié

```pascal
// Pour les dialogues : pas redimensionnable
BorderStyle := bsDialog;

// Désactiver les boutons min/max
BorderIcons := [biSystemMenu];
```

### 4. Valider avant de fermer

```pascal
procedure TFormDialog.ButtonOKClick(Sender: TObject);  
begin  
  if not DonneesValides then
  begin
    ShowMessage('Données invalides');
    ModalResult := mrNone;  // Empêche la fermeture
    Exit;
  end;
  // Si on arrive ici, le ModalResult du bouton fermera le formulaire
end;
```

### 5. Centrer le dialogue

```pascal
Position := poScreenCenter;     // Centre de l'écran
// ou
Position := poMainFormCenter;   // Centre du formulaire principal
```

### 6. Gérer la touche Échap

```pascal
procedure TFormDialog.FormKeyPress(Sender: TObject; var Key: Char);  
begin  
  if Key = #27 then  // Touche Échap
  begin
    ModalResult := mrCancel;
  end;
end;
```

### 7. Donner le focus au bon contrôle

```pascal
procedure TFormDialog.FormShow(Sender: TObject);  
begin  
  EditNom.SetFocus;  // Focus sur le premier champ de saisie
end;
```

## Tableau récapitulatif des dialogues standard

| Dialogue | Utilisation | Composant/Fonction |
|----------|-------------|-------------------|
| Message simple | Information rapide | `ShowMessage()` |
| Message avec options | Confirmation, choix | `MessageDlg()` |
| Saisie texte | Demander une valeur | `InputBox()`, `InputQuery()` |
| Ouvrir fichier | Sélectionner fichier(s) | `TOpenDialog` |
| Enregistrer fichier | Spécifier nom fichier | `TSaveDialog` |
| Ouvrir image | Sélectionner image avec aperçu | `TOpenPictureDialog` |
| Couleur | Choisir une couleur | `TColorDialog` |
| Police | Choisir police et attributs | `TFontDialog` |
| Imprimante | Configurer imprimante | `TPrinterSetupDialog` |
| Imprimer | Options d'impression | `TPrintDialog` |
| Rechercher | Rechercher du texte | `TFindDialog` |
| Remplacer | Rechercher et remplacer | `TReplaceDialog` |

## Résumé

Les boîtes de dialogue sont essentielles pour l'interaction avec l'utilisateur. Les points clés à retenir :

- **Messages simples** : `ShowMessage()` pour l'information, `MessageDlg()` pour les choix
- **Saisie** : `InputBox()` ou `InputQuery()` pour des saisies rapides
- **Dialogues standard** : Utilisez les composants Delphi (TOpenDialog, TColorDialog, etc.)
- **Dialogues personnalisés** : Créez des formulaires modaux avec `BorderStyle := bsDialog` et `ShowModal`
- **Validation** : Validez toujours les données avant de fermer un dialogue
- **ModalResult** : Utilisez les valeurs appropriées (mrOk, mrCancel, etc.)
- **Libération** : Toujours libérer les dialogues personnalisés avec `Free`
- **Expérience utilisateur** : Configurez Default, Cancel, Position et BorderStyle

Une bonne utilisation des boîtes de dialogue rend votre application plus intuitive et professionnelle.

⏭️ [Assistants (Wizards)](/06-applications-multi-fenetres-et-navigation/05-assistants-wizards.md)
