# 6.4 Boîtes de dialogue standard et personnalisées

Les boîtes de dialogue sont des fenêtres spéciales qui facilitent l'interaction avec l'utilisateur. Elles servent à présenter des informations, poser des questions, ou recueillir des données. Dans ce chapitre, nous explorerons comment utiliser les boîtes de dialogue intégrées à Delphi et comment créer vos propres boîtes de dialogue personnalisées.

## Boîtes de dialogue standard

Delphi offre plusieurs boîtes de dialogue prêtes à l'emploi qui vous évitent d'avoir à créer des formulaires pour les interactions courantes.

### Messages simples avec ShowMessage

La fonction la plus simple pour afficher un message est `ShowMessage` :

```pascal
ShowMessage('Opération terminée avec succès !');
```

Cette fonction affiche une boîte de dialogue modale avec un message et un bouton OK. C'est utile pour les notifications simples, mais ses possibilités de personnalisation sont limitées.

### Boîtes de dialogue avancées avec MessageDlg

Pour plus de flexibilité, utilisez la fonction `MessageDlg` :

```pascal
var
  Reponse: Integer;
begin
  Reponse := MessageDlg('Voulez-vous enregistrer les modifications ?',
                        mtConfirmation,
                        [mbYes, mbNo, mbCancel],
                        0);

  case Reponse of
    mrYes: EnregistrerFichier;
    mrNo: // Ne rien faire
    mrCancel: Abort; // Annuler l'opération
  end;
end;
```

Les paramètres de `MessageDlg` sont :
1. Le message à afficher
2. Le type de message (icône) :
   - `mtWarning` - Avertissement (icône triangulaire jaune)
   - `mtError` - Erreur (icône X rouge)
   - `mtInformation` - Information (icône i bleue)
   - `mtConfirmation` - Confirmation (icône ? bleue)
   - `mtCustom` - Personnalisé (sans icône)
3. Les boutons à afficher (un ensemble de constantes `mbXXX`)
4. L'aide contextuelle (0 si non utilisée)

La fonction renvoie une valeur `mrXXX` correspondant au bouton cliqué :
- `mrNone` (0) : Aucun bouton, valeur par défaut
- `mrOk` (1) : Bouton OK
- `mrCancel` (2) : Bouton Annuler
- `mrAbort` (3) : Bouton Abandonner
- `mrRetry` (4) : Bouton Réessayer
- `mrIgnore` (5) : Bouton Ignorer
- `mrYes` (6) : Bouton Oui
- `mrNo` (7) : Bouton Non
- `mrAll` (8) : Bouton Tout
- `mrNoToAll` (9) : Bouton "Non à tout"
- `mrYesToAll` (10) : Bouton "Oui à tout"

### Variante : MessageDlgPos

Si vous souhaitez positionner précisément la boîte de dialogue, utilisez `MessageDlgPos` :

```pascal
Reponse := MessageDlgPos('Voulez-vous continuer ?',
                         mtConfirmation,
                         [mbYes, mbNo],
                         0,
                         Screen.Width div 2,  // Position X
                         Screen.Height div 2); // Position Y
```

### Format personnalisé avec Application.MessageBox

Pour un contrôle encore plus précis, notamment sur le titre de la fenêtre, utilisez `Application.MessageBox` :

```pascal
var
  Reponse: Integer;
begin
  Reponse := Application.MessageBox('Êtes-vous sûr de vouloir supprimer ce fichier ?',
                                    'Confirmation de suppression',
                                    MB_YESNO or MB_ICONQUESTION);

  if Reponse = IDYES then
    SupprimerFichier;
end;
```

Les constantes pour les boutons et icônes incluent :
- `MB_OK` : Un bouton OK
- `MB_OKCANCEL` : Boutons OK et Annuler
- `MB_YESNO` : Boutons Oui et Non
- `MB_YESNOCANCEL` : Boutons Oui, Non et Annuler
- `MB_ICONINFORMATION`, `MB_ICONQUESTION`, `MB_ICONWARNING`, `MB_ICONERROR` : Différentes icônes

Les valeurs de retour sont `IDOK`, `IDCANCEL`, `IDYES`, `IDNO`, etc.

## Boîtes de dialogue standard avec composants

Delphi propose également des composants prêts à l'emploi pour les tâches courantes.

### Ouvrir un fichier (TOpenDialog)

Pour permettre à l'utilisateur de sélectionner un fichier à ouvrir :

```pascal
procedure TForm1.btnOuvrirClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
begin
  OpenDialog := TOpenDialog.Create(Self);
  try
    // Configuration
    OpenDialog.Title := 'Sélectionner un fichier texte';
    OpenDialog.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
    OpenDialog.InitialDir := 'C:\Documents';

    // Affichage
    if OpenDialog.Execute then
    begin
      // Utilisation du fichier sélectionné
      Memo1.Lines.LoadFromFile(OpenDialog.FileName);
      StatusBar1.SimpleText := 'Fichier ouvert : ' + OpenDialog.FileName;
    end;
  finally
    // Nettoyage
    OpenDialog.Free;
  end;
end;
```

Propriétés importantes du `TOpenDialog` :
- `Title` : Titre de la boîte de dialogue
- `Filter` : Définit les types de fichiers visibles
- `FilterIndex` : Index du filtre sélectionné par défaut
- `InitialDir` : Répertoire initial
- `Options` : Options supplémentaires (sélection multiple, etc.)
- `FileName` : Chemin complet du fichier sélectionné
- `Files` : Liste des fichiers sélectionnés (si sélection multiple)

### Enregistrer un fichier (TSaveDialog)

Pour enregistrer un fichier, utilisez `TSaveDialog`, qui fonctionne de façon similaire :

```pascal
procedure TForm1.btnEnregistrerClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.Title := 'Enregistrer le document';
    SaveDialog.Filter := 'Fichiers texte (*.txt)|*.txt|Tous les fichiers (*.*)|*.*';
    SaveDialog.DefaultExt := 'txt';

    if SaveDialog.Execute then
    begin
      Memo1.Lines.SaveToFile(SaveDialog.FileName);
      StatusBar1.SimpleText := 'Fichier enregistré : ' + SaveDialog.FileName;
    end;
  finally
    SaveDialog.Free;
  end;
end;
```

### Sélectionner un répertoire (TSelectDirectoryDialog)

Pour permettre la sélection d'un dossier :

```pascal
procedure TForm1.btnChoisirDossierClick(Sender: TObject);
var
  DirDialog: TSelectDirectoryDialog;
begin
  DirDialog := TSelectDirectoryDialog.Create(Self);
  try
    if DirDialog.Execute then
    begin
      edtDossier.Text := DirDialog.FileName;
    end;
  finally
    DirDialog.Free;
  end;
end;
```

### Sélectionner une police (TFontDialog)

Pour permettre à l'utilisateur de choisir une police :

```pascal
procedure TForm1.btnPoliceClick(Sender: TObject);
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Self);
  try
    FontDialog.Font := Memo1.Font;

    if FontDialog.Execute then
    begin
      Memo1.Font := FontDialog.Font;
    end;
  finally
    FontDialog.Free;
  end;
end;
```

### Sélectionner une couleur (TColorDialog)

Pour sélectionner une couleur :

```pascal
procedure TForm1.btnCouleurClick(Sender: TObject);
var
  ColorDialog: TColorDialog;
begin
  ColorDialog := TColorDialog.Create(Self);
  try
    ColorDialog.Color := Panel1.Color;

    if ColorDialog.Execute then
    begin
      Panel1.Color := ColorDialog.Color;
    end;
  finally
    ColorDialog.Free;
  end;
end;
```

### Optimisation pour les boîtes de dialogue réutilisées

Si vous utilisez souvent la même boîte de dialogue, vous pouvez la déclarer comme composant sur votre formulaire :

1. Glissez un composant (par exemple `TOpenDialog`) depuis la palette de composants
2. Configurez ses propriétés dans l'Object Inspector
3. Utilisez-le directement dans votre code :

```pascal
procedure TForm1.btnOuvrirClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    Memo1.Lines.LoadFromFile(OpenDialog1.FileName);
  end;
end;
```

## Création de boîtes de dialogue personnalisées

Lorsque les boîtes de dialogue standard ne suffisent pas, vous pouvez créer vos propres boîtes de dialogue personnalisées.

### Étapes de base pour créer une boîte de dialogue

1. Créez un nouveau formulaire (File → New → Form)
2. Définissez ses propriétés pour en faire une boîte de dialogue :
   - `BorderStyle` = `bsDialog` (barre de titre fixe, non redimensionnable)
   - `Position` = `poOwnerFormCenter` (centré par rapport au formulaire parent)
   - `FormStyle` = `fsNormal`
3. Ajoutez les contrôles nécessaires (champs, boutons, etc.)
4. Ajoutez des boutons OK et Annuler avec leurs propriétés `ModalResult`

### Exemple : Boîte de dialogue de connexion

Créons une simple boîte de dialogue de connexion :

```pascal
// LoginDlg.pas
unit LoginDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TLoginForm = class(TForm)
    lblUsername: TLabel;
    lblPassword: TLabel;
    edtUsername: TEdit;
    edtPassword: TEdit;
    btnOK: TButton;
    btnCancel: TButton;
    Panel1: TPanel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

procedure TLoginForm.FormCreate(Sender: TObject);
begin
  // Configuration initiale de la boîte de dialogue
  BorderStyle := bsDialog;
  Position := poOwnerFormCenter;

  // Configurer le champ mot de passe pour masquer les caractères
  edtPassword.PasswordChar := '•';

  // Définir les ModalResult des boutons
  btnCancel.ModalResult := mrCancel;
  // btnOK.ModalResult sera défini après validation
end;

procedure TLoginForm.btnOKClick(Sender: TObject);
begin
  // Validation de base
  if (Trim(edtUsername.Text) = '') then
  begin
    ShowMessage('Veuillez entrer un nom d''utilisateur.');
    edtUsername.SetFocus;
    Exit;
  end;

  if (Length(edtPassword.Text) < 4) then
  begin
    ShowMessage('Le mot de passe doit contenir au moins 4 caractères.');
    edtPassword.SetFocus;
    Exit;
  end;

  // Si tout est valide, fermer avec OK
  ModalResult := mrOk;
end;

end.
```

### Utilisation de la boîte de dialogue personnalisée

Dans le formulaire principal, utilisez la boîte de dialogue ainsi :

```pascal
procedure TMainForm.btnLoginClick(Sender: TObject);
var
  LoginForm: TLoginForm;
begin
  LoginForm := TLoginForm.Create(Self);
  try
    // Configuration avant affichage si nécessaire
    LoginForm.edtUsername.Text := 'utilisateur';

    // Affichage modal
    if LoginForm.ShowModal = mrOk then
    begin
      // Traitement si OK
      lblStatus.Caption := 'Connecté en tant que : ' + LoginForm.edtUsername.Text;
    end
    else
    begin
      // Traitement si Annuler
      lblStatus.Caption := 'Connexion annulée';
    end;
  finally
    LoginForm.Free;
  end;
end;
```

### Récupération des données depuis la boîte de dialogue

Il existe plusieurs façons de récupérer les données d'une boîte de dialogue personnalisée :

1. **Accès direct aux composants** (comme dans l'exemple ci-dessus)
2. **Propriétés publiques** :

```pascal
// Dans l'interface de TLoginForm
public
  property Username: string read edtUsername.Text;
  property Password: string read edtPassword.Text;
```

3. **Méthode de récupération de données** :

```pascal
// Dans TLoginForm
public
  procedure GetCredentials(var AUsername, APassword: string);

// Implémentation
procedure TLoginForm.GetCredentials(var AUsername, APassword: string);
begin
  AUsername := edtUsername.Text;
  APassword := edtPassword.Text;
end;

// Dans le formulaire principal
var
  Username, Password: string;
begin
  // ...
  if LoginForm.ShowModal = mrOk then
  begin
    LoginForm.GetCredentials(Username, Password);
    // Utiliser Username et Password
  end;
  // ...
end;
```

### Techniques avancées pour les boîtes de dialogue

#### Validation à la fermeture

Vous pouvez utiliser l'événement `OnCloseQuery` pour valider les données avant de fermer :

```pascal
procedure TLoginForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Si on ferme avec OK et que les données sont invalides
  if (ModalResult = mrOk) and (Trim(edtUsername.Text) = '') then
  begin
    ShowMessage('Veuillez entrer un nom d''utilisateur.');
    edtUsername.SetFocus;
    CanClose := False; // Empêcher la fermeture
  end;
end;
```

#### Boîte de dialogue non-modale

Il est parfois utile d'avoir une boîte de dialogue qui reste ouverte pendant que l'utilisateur interagit avec le reste de l'application :

```pascal
procedure TMainForm.btnShowOptionsClick(Sender: TObject);
begin
  // Vérifier si la forme existe déjà
  if not Assigned(FOptionsForm) then
  begin
    FOptionsForm := TOptionsForm.Create(Self);
    FOptionsForm.OnClose := OptionsFormClose;
  end;

  // Afficher de façon non modale
  FOptionsForm.Show;
end;

procedure TMainForm.OptionsFormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Libérer la mémoire lors de la fermeture
  Action := caFree;
  FOptionsForm := nil;
end;
```

N'oubliez pas de déclarer `FOptionsForm: TOptionsForm;` dans la section `private` de votre formulaire principal.

## Exemple complet : Formulaire de saisie de client

Voici un exemple plus complet d'une boîte de dialogue de saisie de client avec validation et propriétés encapsulées.

### Définition du formulaire de saisie (CustomerDlg.pas)

```pascal
unit CustomerDlg;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls, System.RegularExpressions;

type
  TCustomer = record
    ID: Integer;
    FirstName: string;
    LastName: string;
    Email: string;
    BirthDate: TDate;
    IsActive: Boolean;
  end;

  TCustomerForm = class(TForm)
    lblFirstName: TLabel;
    lblLastName: TLabel;
    lblEmail: TLabel;
    lblBirthDate: TLabel;
    edtFirstName: TEdit;
    edtLastName: TEdit;
    edtEmail: TEdit;
    dtpBirthDate: TDateTimePicker;
    chkActive: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCustomer: TCustomer;
    function ValidateForm: Boolean;
    function IsValidEmail(const Email: string): Boolean;
  public
    procedure SetCustomer(const Value: TCustomer);
    function GetCustomer: TCustomer;
    property Customer: TCustomer read GetCustomer write SetCustomer;
  end;

implementation

{$R *.dfm}

procedure TCustomerForm.FormCreate(Sender: TObject);
begin
  BorderStyle := bsDialog;
  Position := poOwnerFormCenter;

  // Bouton Annuler
  btnCancel.ModalResult := mrCancel;

  // Date par défaut (aujourd'hui)
  dtpBirthDate.Date := Date;
end;

function TCustomerForm.IsValidEmail(const Email: string): Boolean;
begin
  // Validation simple d'email avec expression régulière
  Result := TRegEx.IsMatch(Email, '^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$');
end;

function TCustomerForm.ValidateForm: Boolean;
begin
  Result := False; // Par défaut, échec de validation

  if Trim(edtFirstName.Text) = '' then
  begin
    ShowMessage('Le prénom est obligatoire.');
    edtFirstName.SetFocus;
    Exit;
  end;

  if Trim(edtLastName.Text) = '' then
  begin
    ShowMessage('Le nom est obligatoire.');
    edtLastName.SetFocus;
    Exit;
  end;

  if Trim(edtEmail.Text) = '' then
  begin
    ShowMessage('L''email est obligatoire.');
    edtEmail.SetFocus;
    Exit;
  end;

  if not IsValidEmail(edtEmail.Text) then
  begin
    ShowMessage('Format d''email invalide.');
    edtEmail.SetFocus;
    Exit;
  end;

  if dtpBirthDate.Date > Date then
  begin
    ShowMessage('La date de naissance ne peut pas être dans le futur.');
    dtpBirthDate.SetFocus;
    Exit;
  end;

  Result := True; // Validation réussie
end;

procedure TCustomerForm.btnOKClick(Sender: TObject);
begin
  if ValidateForm then
    ModalResult := mrOk;
end;

procedure TCustomerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) and not ValidateForm then
    CanClose := False;
end;

procedure TCustomerForm.SetCustomer(const Value: TCustomer);
begin
  FCustomer := Value;

  // Mise à jour de l'interface
  edtFirstName.Text := FCustomer.FirstName;
  edtLastName.Text := FCustomer.LastName;
  edtEmail.Text := FCustomer.Email;
  dtpBirthDate.Date := FCustomer.BirthDate;
  chkActive.Checked := FCustomer.IsActive;

  // Configurer le titre
  if FCustomer.ID = 0 then
    Caption := 'Nouveau client'
  else
    Caption := 'Modifier client #' + IntToStr(FCustomer.ID);
end;

function TCustomerForm.GetCustomer: TCustomer;
begin
  // Récupérer les valeurs depuis l'interface
  Result := FCustomer; // Conserver l'ID
  Result.FirstName := Trim(edtFirstName.Text);
  Result.LastName := Trim(edtLastName.Text);
  Result.Email := Trim(edtEmail.Text);
  Result.BirthDate := dtpBirthDate.Date;
  Result.IsActive := chkActive.Checked;
end;

end.
```

### Utilisation dans le formulaire principal

```pascal
procedure TMainForm.btnNewCustomerClick(Sender: TObject);
var
  CustomerForm: TCustomerForm;
  NewCustomer: TCustomer;
begin
  CustomerForm := TCustomerForm.Create(Self);
  try
    // Initialiser un nouveau client
    NewCustomer.ID := 0; // Nouveau client
    NewCustomer.FirstName := '';
    NewCustomer.LastName := '';
    NewCustomer.Email := '';
    NewCustomer.BirthDate := Date;
    NewCustomer.IsActive := True;

    CustomerForm.Customer := NewCustomer;

    if CustomerForm.ShowModal = mrOk then
    begin
      // Récupérer les données
      NewCustomer := CustomerForm.Customer;

      // Attribuer un nouvel ID et ajouter à la liste
      NewCustomer.ID := GetNextCustomerID;
      AddCustomerToList(NewCustomer);

      ShowMessage('Client ajouté avec succès !');
    end;
  finally
    CustomerForm.Free;
  end;
end;

procedure TMainForm.btnEditCustomerClick(Sender: TObject);
var
  CustomerForm: TCustomerForm;
  Customer: TCustomer;
  CustomerIndex: Integer;
begin
  // Trouver le client sélectionné
  CustomerIndex := GetSelectedCustomerIndex;
  if CustomerIndex < 0 then
  begin
    ShowMessage('Veuillez sélectionner un client à modifier.');
    Exit;
  end;

  Customer := Customers[CustomerIndex];

  CustomerForm := TCustomerForm.Create(Self);
  try
    CustomerForm.Customer := Customer;

    if CustomerForm.ShowModal = mrOk then
    begin
      // Mettre à jour les données
      Customer := CustomerForm.Customer;
      UpdateCustomerInList(CustomerIndex, Customer);

      ShowMessage('Client modifié avec succès !');
    end;
  finally
    CustomerForm.Free;
  end;
end;
```

## Bonnes pratiques pour les boîtes de dialogue

1. **Validation robuste** : Vérifiez toujours les données avant de fermer avec OK
2. **Feedback utilisateur** : Donnez des messages d'erreur clairs et précis
3. **Conception intuitive** : Suivez les conventions d'interface utilisateur
4. **Accessibilité** : Assurez-vous que le clavier peut être utilisé pour naviguer
5. **Mémoire** : Libérez toujours les boîtes de dialogue créées dynamiquement
6. **Réutilisation** : Concevez des boîtes de dialogue réutilisables

## Astuces et pièges à éviter

- **Évitez les boîtes de dialogue trop complexes** - Divisez-les si nécessaire
- **Utilisez des valeurs par défaut sensées** pour faciliter la saisie
- **N'oubliez pas le cas Annuler** dans votre code principal
- **Attention aux messages d'erreur intrusifs** - Groupez les validations quand c'est possible
- **Si la boîte est non-modale**, faites attention à la synchronisation des données

## Exercices pratiques

1. **Exercice simple** : Créez une boîte de dialogue pour saisir des informations de contact (nom, téléphone, email)
2. **Exercice intermédiaire** : Ajoutez une validation avancée avec des expressions régulières pour le téléphone et l'email
3. **Exercice avancé** : Créez une boîte de dialogue à plusieurs onglets pour configurer les paramètres d'une application

---

Les boîtes de dialogue sont un outil puissant pour interagir avec l'utilisateur. En maîtrisant à la fois les boîtes standard et personnalisées, vous pourrez créer des applications Delphi intuitives et professionnelles qui offrent une excellente expérience utilisateur.
