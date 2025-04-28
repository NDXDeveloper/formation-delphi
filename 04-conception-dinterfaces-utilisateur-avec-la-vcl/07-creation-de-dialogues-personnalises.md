# 4.7 Création de dialogues personnalisés

Les boîtes de dialogue sont des éléments essentiels dans toute application. Elles permettent d'interagir avec l'utilisateur pour obtenir des informations, afficher des messages ou proposer des options. Dans cette section, nous allons voir comment créer et utiliser des dialogues personnalisés dans Delphi, au-delà des boîtes de dialogue standard.

## Les boîtes de dialogue standards vs personnalisées

Delphi propose des boîtes de dialogue standard prêtes à l'emploi (que nous verrons brièvement), mais pour de nombreuses situations, vous aurez besoin de créer vos propres dialogues personnalisés.

### Les boîtes de dialogue standard

Voici quelques fonctions et composants pour les dialogues standard :

- `ShowMessage` : affiche un message simple
- `MessageDlg` : affiche un message avec différentes icônes et boutons
- `InputBox` : demande une saisie simple de texte
- `TOpenDialog` : pour sélectionner un fichier à ouvrir
- `TSaveDialog` : pour sélectionner un emplacement où enregistrer un fichier
- `TColorDialog` : pour choisir une couleur
- `TFontDialog` : pour choisir une police de caractères
- etc.

Ces dialogues sont faciles à utiliser mais ont des limitations en termes de personnalisation.

### Pourquoi créer des dialogues personnalisés ?

Les dialogues personnalisés vous permettent de :
- Collecter des informations spécifiques à votre application
- Créer une interface utilisateur cohérente avec le reste de votre application
- Proposer une expérience utilisateur plus riche et plus intuitive
- Implémenter des validations complexes

## Créer un formulaire de dialogue de base

### Étape 1 : Créer un nouveau formulaire

1. Cliquez sur **Fichier** > **Nouveau** > **VCL Form**
2. Configurez les propriétés du formulaire :
   - `BorderStyle` : `bsDialog` (style spécifique aux boîtes de dialogue)
   - `Position` : `poScreenCenter` (centré à l'écran)
   - `BorderIcons` : désactivez les icônes non nécessaires (minimiser, maximiser)
   - `Caption` : titre de votre dialogue
   - `FormStyle` : `fsNormal`

### Étape 2 : Ajouter les contrôles nécessaires

Ajoutez les contrôles dont vous avez besoin : champs de saisie, listes déroulantes, boutons, etc.

### Étape 3 : Ajouter les boutons standard

La plupart des dialogues ont au minimum deux boutons : **OK** et **Annuler**.

1. Ajoutez deux boutons (`TButton`) en bas du formulaire
2. Définissez leurs propriétés :
   - Premier bouton :
     - `Name` : `ButtonOK`
     - `Caption` : `OK`
     - `Default` : `True` (activé par la touche Entrée)
     - `ModalResult` : `mrOk`
   - Second bouton :
     - `Name` : `ButtonCancel`
     - `Caption` : `Annuler`
     - `Cancel` : `True` (activé par la touche Échap)
     - `ModalResult` : `mrCancel`

La propriété `ModalResult` est très importante - elle définit la valeur retournée par la méthode `ShowModal` lorsque l'utilisateur clique sur le bouton.

### Étape 4 : Enregistrer le formulaire

Enregistrez le formulaire avec un nom significatif, par exemple `UDialogConfig.pas`.

## Utilisation des dialogues modaux

Les dialogues personnalisés sont généralement affichés en mode **modal**, ce qui signifie que l'utilisateur doit interagir avec le dialogue (en le fermant) avant de pouvoir continuer à utiliser le reste de l'application.

### Afficher un dialogue modal

```pascal
procedure TFormMain.ButtonConfigClick(Sender: TObject);
var
  DialogConfig: TDialogConfig;
begin
  DialogConfig := TDialogConfig.Create(Self);
  try
    // Initialiser les valeurs du dialogue
    DialogConfig.EditNom.Text := FConfiguration.Nom;
    DialogConfig.CheckBoxOption1.Checked := FConfiguration.Option1;

    // Afficher le dialogue et attendre la réponse
    if DialogConfig.ShowModal = mrOk then
    begin
      // L'utilisateur a cliqué sur OK, récupérer les valeurs
      FConfiguration.Nom := DialogConfig.EditNom.Text;
      FConfiguration.Option1 := DialogConfig.CheckBoxOption1.Checked;

      // Mettre à jour l'interface avec les nouvelles valeurs
      MettreAJourInterface;
    end;
    // Si l'utilisateur a cliqué sur Annuler, ne rien faire
  finally
    DialogConfig.Free;  // Ne pas oublier de libérer la mémoire
  end;
end;
```

Cette approche a deux avantages majeurs :
1. Elle évite les fuites de mémoire en libérant toujours le formulaire
2. Elle permet de gérer proprement l'annulation (si l'utilisateur clique sur Annuler)

## L'importance de ModalResult

La propriété `ModalResult` et la fonction `ShowModal` travaillent ensemble :

1. Lorsque l'utilisateur clique sur un bouton avec un `ModalResult` non nul, le dialogue se ferme automatiquement
2. La fonction `ShowModal` retourne la valeur du `ModalResult` du bouton cliqué

Les valeurs courantes de `ModalResult` sont :
- `mrNone` (0) : ne ferme pas le dialogue
- `mrOk` (1) : bouton OK
- `mrCancel` (2) : bouton Annuler
- `mrYes` (6) : bouton Oui
- `mrNo` (7) : bouton Non
- `mrAbort` (3) : bouton Abandonner
- `mrRetry` (4) : bouton Réessayer
- `mrIgnore` (5) : bouton Ignorer

## Validation des données du dialogue

Parfois, vous souhaitez valider les données entrées par l'utilisateur avant de fermer le dialogue. Dans ce cas, vous devez :

1. Définir `ModalResult` à `mrNone` pour vos boutons
2. Gérer l'événement `OnClick` des boutons
3. Effectuer vos validations et définir `ModalResult` uniquement si les validations réussissent

```pascal
procedure TDialogConfig.ButtonOKClick(Sender: TObject);
begin
  // Vérifier que le nom n'est pas vide
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom ne peut pas être vide.');
    EditNom.SetFocus;
    Exit;  // Ne pas fermer le dialogue
  end;

  // Vérifier que l'âge est valide
  if not TryStrToInt(EditAge.Text, FAge) or (FAge <= 0) then
  begin
    ShowMessage('Veuillez entrer un âge valide.');
    EditAge.SetFocus;
    Exit;
  end;

  // Toutes les validations ont réussi, fermer avec OK
  ModalResult := mrOk;
end;
```

## Exemple complet : Dialogue de configuration utilisateur

Voici un exemple complet d'un dialogue personnalisé pour configurer les préférences d'un utilisateur.

### Le formulaire de dialogue (UDialogConfig.pas)

```pascal
unit UDialogConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.ComCtrls;

type
  TDialogConfig = class(TForm)
    LabelNom: TLabel;
    EditNom: TEdit;
    LabelEmail: TLabel;
    EditEmail: TEdit;
    GroupBoxOptions: TGroupBox;
    CheckBoxNotifications: TCheckBox;
    CheckBoxThemeSombre: TCheckBox;
    LabelTaillePolice: TLabel;
    TrackBarTaillePolice: TTrackBar;
    LabelTailleValeur: TLabel;
    PanelBoutons: TPanel;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    ComboBoxLangue: TComboBox;
    LabelLangue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TrackBarTaillePoliceChange(Sender: TObject);
    procedure ButtonOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    function ValiderEmail(const Email: string): Boolean;
  public
    // Propriétés publiques pour faciliter l'accès depuis l'extérieur
    property NomUtilisateur: string read EditNom.Text write EditNom.Text;
    property EmailUtilisateur: string read EditEmail.Text write EditEmail.Text;
    property ActiverNotifications: Boolean read CheckBoxNotifications.Checked
                                  write CheckBoxNotifications.Checked;
    property ThemeSombre: Boolean read CheckBoxThemeSombre.Checked
                        write CheckBoxThemeSombre.Checked;
    property TaillePolice: Integer read TrackBarTaillePolice.Position
                          write TrackBarTaillePolice.Position;
    property Langue: string read ComboBoxLangue.Text write ComboBoxLangue.Text;
  end;

implementation

{$R *.dfm}

procedure TDialogConfig.FormCreate(Sender: TObject);
begin
  // Remplir les langues disponibles
  ComboBoxLangue.Items.Clear;
  ComboBoxLangue.Items.Add('Français');
  ComboBoxLangue.Items.Add('English');
  ComboBoxLangue.Items.Add('Español');
  ComboBoxLangue.Items.Add('Deutsch');
  ComboBoxLangue.ItemIndex := 0;  // Français par défaut

  // Initialiser la valeur de taille de police
  TrackBarTaillePoliceChange(TrackBarTaillePolice);
end;

procedure TDialogConfig.FormShow(Sender: TObject);
begin
  // Donner le focus au premier champ
  EditNom.SetFocus;
end;

procedure TDialogConfig.TrackBarTaillePoliceChange(Sender: TObject);
begin
  // Mettre à jour l'étiquette avec la valeur actuelle
  LabelTailleValeur.Caption := IntToStr(TrackBarTaillePolice.Position) + ' pt';
end;

function TDialogConfig.ValiderEmail(const Email: string): Boolean;
begin
  // Validation basique d'e-mail : contient @ et au moins un point après @
  Result := (Pos('@', Email) > 1) and
            (Pos('.', Email, Pos('@', Email) + 2) > 0);
end;

procedure TDialogConfig.ButtonOKClick(Sender: TObject);
begin
  // Valider le nom
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Veuillez entrer votre nom.');
    EditNom.SetFocus;
    Exit;
  end;

  // Valider l'e-mail
  if (Trim(EditEmail.Text) <> '') and not ValiderEmail(EditEmail.Text) then
  begin
    ShowMessage('Veuillez entrer une adresse e-mail valide.');
    EditEmail.SetFocus;
    Exit;
  end;

  // Toutes les validations ont réussi
  ModalResult := mrOk;
end;

end.
```

### Utilisation du dialogue dans le formulaire principal

```pascal
procedure TFormMain.ButtonConfigurationClick(Sender: TObject);
var
  DialogConfig: TDialogConfig;
begin
  DialogConfig := TDialogConfig.Create(Self);
  try
    // Initialiser les contrôles avec les valeurs actuelles
    DialogConfig.NomUtilisateur := FConfig.Nom;
    DialogConfig.EmailUtilisateur := FConfig.Email;
    DialogConfig.ActiverNotifications := FConfig.Notifications;
    DialogConfig.ThemeSombre := FConfig.ThemeSombre;
    DialogConfig.TaillePolice := FConfig.TaillePolice;
    DialogConfig.Langue := FConfig.Langue;

    // Afficher le dialogue
    if DialogConfig.ShowModal = mrOk then
    begin
      // Récupérer les nouvelles valeurs
      FConfig.Nom := DialogConfig.NomUtilisateur;
      FConfig.Email := DialogConfig.EmailUtilisateur;
      FConfig.Notifications := DialogConfig.ActiverNotifications;
      FConfig.ThemeSombre := DialogConfig.ThemeSombre;
      FConfig.TaillePolice := DialogConfig.TaillePolice;
      FConfig.Langue := DialogConfig.Langue;

      // Appliquer les changements
      AppliquerConfiguration;

      // Sauvegarder la configuration
      SauvegarderConfiguration;
    end;
  finally
    DialogConfig.Free;
  end;
end;
```

## Techniques avancées pour les dialogues

### Réutiliser un même dialogue

Si vous devez ouvrir le même dialogue plusieurs fois, vous pouvez l'optimiser :

```pascal
procedure TFormMain.InitialiserDialogueConfig;
begin
  // Créer le dialogue une seule fois
  if not Assigned(FDialogConfig) then
    FDialogConfig := TDialogConfig.Create(Self);
end;

procedure TFormMain.ButtonConfigurationClick(Sender: TObject);
begin
  InitialiserDialogueConfig;

  // Initialiser les contrôles avec les valeurs actuelles
  FDialogConfig.NomUtilisateur := FConfig.Nom;
  // ...autres initialisations...

  // Afficher le dialogue
  if FDialogConfig.ShowModal = mrOk then
  begin
    // Récupérer les nouvelles valeurs
    FConfig.Nom := FDialogConfig.NomUtilisateur;
    // ...autres récupérations...

    // Appliquer les changements
    AppliquerConfiguration;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  // Libérer la mémoire à la fermeture
  if Assigned(FDialogConfig) then
    FDialogConfig.Free;
end;
```

### Les dialogues non modaux

Dans certains cas, vous pouvez avoir besoin de dialogues **non modaux** qui restent ouverts pendant que l'utilisateur continue à utiliser l'application principale.

```pascal
procedure TFormMain.ButtonStatistiquesClick(Sender: TObject);
begin
  // Vérifier si le formulaire existe déjà
  if not Assigned(FFormStats) then
  begin
    // Créer le formulaire
    FFormStats := TFormStats.Create(Self);
    FFormStats.OnClose := FormStatsClose;  // Événement pour détecter la fermeture
  end;

  // Mettre à jour les données
  FFormStats.MettreAJourDonnees(FDonnees);

  // Afficher le formulaire (non modal)
  FFormStats.Show;
end;

procedure TFormMain.FormStatsClose(Sender: TObject; var Action: TCloseAction);
begin
  // Indiquer que le formulaire doit être libéré lors de la fermeture
  Action := caFree;
  FFormStats := nil;  // Réinitialiser le pointeur
end;
```

Pour un dialogue non modal :
- Utilisez `Show` au lieu de `ShowModal`
- Vous devez gérer manuellement quand et comment récupérer les données
- Utilisez l'événement `OnClose` pour nettoyer

### Passage de données complexes

Pour les dialogues qui manipulent des structures de données complexes, vous pouvez utiliser plusieurs approches :

#### 1. Propriétés publiques

Comme dans l'exemple précédent, définissez des propriétés publiques pour accéder aux données.

#### 2. Paramètre dans le constructeur

```pascal
type
  TDialogEdition = class(TForm)
    // Composants...
  private
    FDonnee: TDonnee;  // Référence à l'objet de données
  public
    constructor Create(AOwner: TComponent; ADonnee: TDonnee); reintroduce;
  end;

constructor TDialogEdition.Create(AOwner: TComponent; ADonnee: TDonnee);
begin
  inherited Create(AOwner);
  FDonnee := ADonnee;

  // Initialiser les contrôles avec les données
  EditNom.Text := FDonnee.Nom;
  // ...
end;

// Utilisation
var
  Dialog: TDialogEdition;
begin
  Dialog := TDialogEdition.Create(Self, MaDonnee);
  try
    if Dialog.ShowModal = mrOk then
    begin
      // Les modifications ont déjà été appliquées à MaDonnee
    end;
  finally
    Dialog.Free;
  end;
end;
```

#### 3. Méthodes dédiées

```pascal
type
  TDialogEdition = class(TForm)
    // Composants...
  public
    procedure ChargerDonnee(const ADonnee: TDonnee);
    procedure SauvegarderDonnee(var ADonnee: TDonnee);
  end;

// Utilisation
var
  Dialog: TDialogEdition;
begin
  Dialog := TDialogEdition.Create(Self);
  try
    Dialog.ChargerDonnee(MaDonnee);

    if Dialog.ShowModal = mrOk then
    begin
      Dialog.SauvegarderDonnee(MaDonnee);
    end;
  finally
    Dialog.Free;
  end;
end;
```

## Astuces pour des dialogues professionnels

### 1. Disposition et alignement

- Utilisez des panneaux (`TPanel`) pour organiser les contrôles
- Alignez correctement les étiquettes et les champs
- Respectez une marge constante (environ 8-10 pixels)
- Utilisez `Align` et `Anchors` pour un redimensionnement correct

### 2. Comportement standard

- La touche Entrée doit valider le dialogue (bouton avec `Default = True`)
- La touche Échap doit annuler le dialogue (bouton avec `Cancel = True`)
- Le premier champ doit recevoir le focus automatiquement
- Respectez l'ordre de tabulation logique (`TabOrder`)

### 3. Feedback visuel

- Utilisez des icônes pour indiquer les erreurs
- Ajoutez des infobulles (propriété `Hint`) pour l'aide contextuelle
- Changez la couleur des champs invalides

### 4. Persistance des préférences

Sauvegardez la position et la taille du dialogue pour les réutiliser :

```pascal
procedure TDialogConfig.FormCreate(Sender: TObject);
begin
  // Charger la position enregistrée
  Left := FRegistry.ReadInteger('DialogConfig', 'Left', Left);
  Top := FRegistry.ReadInteger('DialogConfig', 'Top', Top);
  Width := FRegistry.ReadInteger('DialogConfig', 'Width', Width);
  Height := FRegistry.ReadInteger('DialogConfig', 'Height', Height);
end;

procedure TDialogConfig.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Enregistrer la position actuelle
  FRegistry.WriteInteger('DialogConfig', 'Left', Left);
  FRegistry.WriteInteger('DialogConfig', 'Top', Top);
  FRegistry.WriteInteger('DialogConfig', 'Width', Width);
  FRegistry.WriteInteger('DialogConfig', 'Height', Height);
end;
```

## Conclusion

Les dialogues personnalisés sont essentiels pour créer des applications professionnelles et conviviales. Ils vous permettent de collecter des informations spécifiques auprès de l'utilisateur tout en respectant l'apparence et le comportement de votre application.

Points clés à retenir :
- Utilisez `BorderStyle = bsDialog` pour un aspect visuel de dialogue
- Définissez correctement les propriétés `ModalResult` pour faciliter la gestion des réponses
- Validez toujours les données avant de fermer le dialogue
- Libérez systématiquement la mémoire des dialogues créés dynamiquement
- Suivez les conventions d'interface utilisateur pour une expérience cohérente

En maîtrisant la création de dialogues personnalisés, vous pourrez améliorer considérablement l'expérience utilisateur de vos applications Delphi.

---

*Exercice pratique : Créez un dialogue personnalisé pour saisir les coordonnées d'un contact (nom, prénom, téléphone, email, adresse). Ajoutez des validations pour chaque champ et assurez-vous que le dialogue respecte les bonnes pratiques d'interface utilisateur.*
