🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 4.7 Création de dialogues personnalisés

## Introduction

Les dialogues personnalisés sont des fenêtres secondaires que vous créez pour interagir avec l'utilisateur de manière spécifique à votre application. Contrairement aux dialogues standards de Windows (comme `ShowMessage` ou `InputBox`), les dialogues personnalisés vous permettent de concevoir des interfaces adaptées exactement à vos besoins.

## 4.7.1 Qu'est-ce qu'un dialogue ?

### Définition

Un **dialogue** (ou boîte de dialogue) est une fenêtre secondaire qui :
- Apparaît temporairement au-dessus de la fenêtre principale
- Demande une information ou une confirmation à l'utilisateur
- Se ferme après que l'utilisateur a effectué son choix
- Retourne généralement un résultat à la fenêtre qui l'a appelée

### Types de dialogues

**Dialogues standards Windows :**
```pascal
// Messages simples
ShowMessage('Opération terminée');

// Questions Oui/Non
if MessageDlg('Continuer ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  // Faire quelque chose

// Saisie simple
var
  Nom: string;
begin
  if InputQuery('Identification', 'Entrez votre nom:', Nom) then
    ShowMessage('Bonjour ' + Nom);
end;
```

**Dialogues de fichiers :**
```pascal
// Ouvrir un fichier
if OpenDialog1.Execute then
  ShowMessage('Fichier sélectionné : ' + OpenDialog1.FileName);

// Enregistrer un fichier
if SaveDialog1.Execute then
  Memo1.Lines.SaveToFile(SaveDialog1.FileName);
```

**Pourquoi créer des dialogues personnalisés ?**

Les dialogues standards sont limités. Vous avez besoin de dialogues personnalisés quand :
- Vous devez saisir plusieurs informations en même temps
- Vous voulez une mise en forme spécifique
- Vous devez valider les données avant de les accepter
- Vous avez besoin de contrôles spéciaux (listes, cases à cocher, etc.)
- Vous voulez une apparence cohérente avec votre application

---

## 4.7.2 Créer un dialogue simple

### Étape 1 : Créer un nouveau formulaire

1. Menu **Fichier** → **Nouveau** → **Fiche VCL** (ou **Fichier** → **Nouveau** → **Formulaire**)
2. Un nouveau formulaire vide apparaît
3. Dans l'Inspecteur d'objets, modifiez les propriétés :
   - **Name** : `FormDialogue` (ou un nom significatif)
   - **Caption** : `Saisie d'informations`
   - **BorderStyle** : `bsDialog` (empêche le redimensionnement)
   - **Position** : `poScreenCenter` (centré à l'écran)
   - **Width** : `400`
   - **Height** : `250`

### Étape 2 : Concevoir l'interface

Ajoutez les composants nécessaires :

**Exemple : Dialogue de connexion**
```
┌─────────────────────────────────┐
│  Connexion                   [X]│
├─────────────────────────────────┤
│  Nom d'utilisateur:             │
│  [____________________]         │
│                                 │
│  Mot de passe:                  │
│  [____________________]         │
│                                 │
│  [ ] Se souvenir de moi         │
│                                 │
│     [  OK  ]  [ Annuler ]       │
└─────────────────────────────────┘
```

Composants à ajouter :
- 2 **TLabel** (pour les légendes)
- 2 **TEdit** (pour la saisie)
- 1 **TCheckBox** (option)
- 2 **TButton** (OK et Annuler)

**Configuration des boutons :**

**Bouton OK :**
- Name : `ButtonOK`
- Caption : `OK`
- Default : `True` (s'active avec la touche Entrée)
- ModalResult : `mrOk`

**Bouton Annuler :**
- Name : `ButtonAnnuler`
- Caption : `Annuler`
- Cancel : `True` (s'active avec la touche Échap)
- ModalResult : `mrCancel`

### Étape 3 : Propriété ModalResult

La propriété **ModalResult** est essentielle pour les dialogues :
- Définit la valeur de retour du dialogue
- Ferme automatiquement le dialogue quand un bouton est cliqué
- Pas besoin d'écrire de code pour fermer le dialogue

**Valeurs courantes de ModalResult :**

| Valeur | Description |
|--------|-------------|
| `mrNone` | Aucun résultat (par défaut) |
| `mrOk` | Bouton OK cliqué |
| `mrCancel` | Bouton Annuler cliqué |
| `mrYes` | Bouton Oui cliqué |
| `mrNo` | Bouton Non cliqué |
| `mrAbort` | Bouton Abandonner cliqué |
| `mrRetry` | Bouton Réessayer cliqué |
| `mrIgnore` | Bouton Ignorer cliqué |

---

## 4.7.3 Utiliser un dialogue modal

### Qu'est-ce qu'un dialogue modal ?

Un dialogue **modal** bloque l'interaction avec les autres fenêtres de l'application jusqu'à ce qu'il soit fermé. C'est le comportement standard des boîtes de dialogue.

### Afficher le dialogue

Dans votre formulaire principal :

```pascal
uses
  FormDialogue; // Ajouter dans la clause uses

procedure TFormPrincipal.ButtonConnexionClick(Sender: TObject);
var
  Dialogue: TFormDialogue;
begin
  // Créer l'instance du dialogue
  Dialogue := TFormDialogue.Create(Self);
  try
    // Afficher le dialogue de manière modale
    if Dialogue.ShowModal = mrOk then
    begin
      // L'utilisateur a cliqué sur OK
      ShowMessage('Connexion acceptée');
    end
    else
    begin
      // L'utilisateur a cliqué sur Annuler ou fermé la fenêtre
      ShowMessage('Connexion annulée');
    end;
  finally
    // Toujours libérer la mémoire
    Dialogue.Free;
  end;
end;
```

### Méthode alternative avec FreeOnRelease

```pascal
procedure TFormPrincipal.ButtonConnexionClick(Sender: TObject);
begin
  with TFormDialogue.Create(Self) do
  try
    if ShowModal = mrOk then
      ShowMessage('Connexion acceptée')
    else
      ShowMessage('Connexion annulée');
  finally
    Free;
  end;
end;
```

---

## 4.7.4 Passer des données au dialogue

### Méthode 1 : Propriétés publiques

Dans le dialogue, déclarez des propriétés publiques :

```pascal
// Dans l'unité du dialogue (FormDialogue.pas)
type
  TFormDialogue = class(TForm)
    EditNom: TEdit;
    EditMotDePasse: TEdit;
    CheckBoxSeSouvenir: TCheckBox;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
  private
    { Déclarations privées }
  public
    // Propriétés pour accéder aux données
    function GetNomUtilisateur: string;
    function GetMotDePasse: string;
    function GetSeSouvenir: Boolean;

    property NomUtilisateur: string read GetNomUtilisateur;
    property MotDePasse: string read GetMotDePasse;
    property SeSouvenir: Boolean read GetSeSouvenir;
  end;

implementation

function TFormDialogue.GetNomUtilisateur: string;
begin
  Result := EditNom.Text;
end;

function TFormDialogue.GetMotDePasse: string;
begin
  Result := EditMotDePasse.Text;
end;

function TFormDialogue.GetSeSouvenir: Boolean;
begin
  Result := CheckBoxSeSouvenir.Checked;
end;
```

**Utilisation dans le formulaire principal :**

```pascal
procedure TFormPrincipal.ButtonConnexionClick(Sender: TObject);
var
  Dialogue: TFormDialogue;
begin
  Dialogue := TFormDialogue.Create(Self);
  try
    if Dialogue.ShowModal = mrOk then
    begin
      // Récupérer les données
      ShowMessage('Utilisateur : ' + Dialogue.NomUtilisateur);
      ShowMessage('Se souvenir : ' + BoolToStr(Dialogue.SeSouvenir, True));

      // Utiliser les données pour la connexion
      if SeConnecter(Dialogue.NomUtilisateur, Dialogue.MotDePasse) then
        ShowMessage('Connexion réussie')
      else
        ShowMessage('Échec de la connexion');
    end;
  finally
    Dialogue.Free;
  end;
end;
```

### Méthode 2 : Paramètres d'initialisation

Créez une méthode d'initialisation dans le dialogue :

```pascal
type
  TFormDialogue = class(TForm)
    // ...
  public
    procedure Initialiser(const NomParDefaut: string);
  end;

implementation

procedure TFormDialogue.Initialiser(const NomParDefaut: string);
begin
  EditNom.Text := NomParDefaut;
  EditMotDePasse.Clear;
  CheckBoxSeSouvenir.Checked := False;
  EditNom.SetFocus;
end;
```

**Utilisation :**

```pascal
procedure TFormPrincipal.ButtonConnexionClick(Sender: TObject);
var
  Dialogue: TFormDialogue;
begin
  Dialogue := TFormDialogue.Create(Self);
  try
    // Initialiser avec une valeur par défaut
    Dialogue.Initialiser('admin');

    if Dialogue.ShowModal = mrOk then
      // Traiter les données
      TraiterConnexion(Dialogue);
  finally
    Dialogue.Free;
  end;
end;
```

### Méthode 3 : Fonction d'exécution encapsulée

Créez une fonction qui gère tout :

```pascal
type
  TFormDialogue = class(TForm)
    // ...
  public
    class function Executer(const NomDefaut: string;
                           out Nom, MotDePasse: string;
                           out SeSouvenir: Boolean): Boolean;
  end;

implementation

class function TFormDialogue.Executer(const NomDefaut: string;
  out Nom, MotDePasse: string; out SeSouvenir: Boolean): Boolean;
var
  Dialogue: TFormDialogue;
begin
  Dialogue := TFormDialogue.Create(nil);
  try
    Dialogue.EditNom.Text := NomDefaut;
    Result := Dialogue.ShowModal = mrOk;

    if Result then
    begin
      Nom := Dialogue.EditNom.Text;
      MotDePasse := Dialogue.EditMotDePasse.Text;
      SeSouvenir := Dialogue.CheckBoxSeSouvenir.Checked;
    end;
  finally
    Dialogue.Free;
  end;
end;
```

**Utilisation simplifiée :**

```pascal
procedure TFormPrincipal.ButtonConnexionClick(Sender: TObject);
var
  Nom, MotDePasse: string;
  SeSouvenir: Boolean;
begin
  if TFormDialogue.Executer('', Nom, MotDePasse, SeSouvenir) then
  begin
    ShowMessage('Connexion avec : ' + Nom);
    // Traiter la connexion
  end;
end;
```

---

## 4.7.5 Validation des données

### Validation lors de la fermeture

Empêchez la fermeture du dialogue si les données sont invalides :

```pascal
procedure TFormDialogue.ButtonOKClick(Sender: TObject);
begin
  // Valider le nom d'utilisateur
  if Trim(EditNom.Text) = '' then
  begin
    ShowMessage('Le nom d''utilisateur est obligatoire');
    EditNom.SetFocus;
    ModalResult := mrNone; // Annuler la fermeture
    Exit;
  end;

  // Valider le mot de passe
  if Length(EditMotDePasse.Text) < 6 then
  begin
    ShowMessage('Le mot de passe doit contenir au moins 6 caractères');
    EditMotDePasse.SetFocus;
    ModalResult := mrNone;
    Exit;
  end;

  // Validation réussie, le dialogue se fermera avec mrOk
  ModalResult := mrOk;
end;
```

### Validation en temps réel

```pascal
procedure TFormDialogue.EditNomChange(Sender: TObject);
begin
  // Activer/désactiver le bouton OK selon la validité
  ButtonOK.Enabled := (Trim(EditNom.Text) <> '') and
                      (Length(EditMotDePasse.Text) >= 6);
end;

procedure TFormDialogue.EditMotDePasseChange(Sender: TObject);
begin
  ButtonOK.Enabled := (Trim(EditNom.Text) <> '') and
                      (Length(EditMotDePasse.Text) >= 6);

  // Indicateur visuel de force du mot de passe
  if Length(EditMotDePasse.Text) < 6 then
    LabelForce.Caption := 'Faible'
  else if Length(EditMotDePasse.Text) < 10 then
    LabelForce.Caption := 'Moyen'
  else
    LabelForce.Caption := 'Fort';
end;

procedure TFormDialogue.FormCreate(Sender: TObject);
begin
  // Désactiver le bouton OK au départ
  ButtonOK.Enabled := False;
end;
```

### Événement OnCloseQuery

Pour une validation plus complexe :

```pascal
procedure TFormDialogue.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Ne valider que si l'utilisateur a cliqué sur OK
  if ModalResult = mrOk then
  begin
    // Vérification
    if not ValiderDonnees then
    begin
      CanClose := False; // Empêcher la fermeture
      ShowMessage('Veuillez corriger les erreurs');
    end;
  end
  else
  begin
    // Autoriser la fermeture si Annuler ou fermeture de fenêtre
    CanClose := True;
  end;
end;

function TFormDialogue.ValiderDonnees: Boolean;
begin
  Result := True;

  // Validation du nom
  if Trim(EditNom.Text) = '' then
  begin
    EditNom.SetFocus;
    Result := False;
    Exit;
  end;

  // Validation du mot de passe
  if Length(EditMotDePasse.Text) < 6 then
  begin
    EditMotDePasse.SetFocus;
    Result := False;
    Exit;
  end;
end;
```

---

## 4.7.6 Dialogues non modaux

### Différence modal vs non-modal

**Modal :**
- Bloque les autres fenêtres
- L'utilisateur DOIT fermer le dialogue avant de continuer
- Utilise `ShowModal`

**Non-modal :**
- N'empêche pas l'interaction avec d'autres fenêtres
- L'utilisateur peut basculer entre les fenêtres
- Utilise `Show`

### Créer un dialogue non-modal

```pascal
type
  TFormPrincipal = class(TForm)
  private
    FDialogueNonModal: TFormDialogue;
  public
    procedure AfficherDialogueNonModal;
  end;

implementation

procedure TFormPrincipal.AfficherDialogueNonModal;
begin
  // Créer si nécessaire
  if not Assigned(FDialogueNonModal) then
  begin
    FDialogueNonModal := TFormDialogue.Create(Self);
    FDialogueNonModal.OnClose := DialogueNonModalClose;
  end;

  // Afficher de manière non-modale
  FDialogueNonModal.Show;
end;

procedure TFormPrincipal.DialogueNonModalClose(Sender: TObject;
  var Action: TCloseAction);
begin
  // Libérer automatiquement
  Action := caFree;
  FDialogueNonModal := nil;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  // Nettoyer si le dialogue est encore ouvert
  if Assigned(FDialogueNonModal) then
    FDialogueNonModal.Free;
end;
```

### Communication avec un dialogue non-modal

Le dialogue non-modal peut appeler des méthodes du formulaire parent :

```pascal
type
  TFormDialogue = class(TForm)
  private
    FFormParent: TFormPrincipal;
  public
    property FormParent: TFormPrincipal read FFormParent write FFormParent;
  end;

// Dans le dialogue
procedure TFormDialogue.ButtonAppliquerClick(Sender: TObject);
begin
  if Assigned(FFormParent) then
    FFormParent.AppliquerParametres(EditNom.Text, CheckBox1.Checked);
end;

// Dans le formulaire principal
procedure TFormPrincipal.AfficherDialogueNonModal;
begin
  if not Assigned(FDialogueNonModal) then
  begin
    FDialogueNonModal := TFormDialogue.Create(Self);
    FDialogueNonModal.FormParent := Self; // Définir le parent
  end;

  FDialogueNonModal.Show;
end;

procedure TFormPrincipal.AppliquerParametres(const Nom: string;
  Option: Boolean);
begin
  // Mettre à jour l'interface principale
  Label1.Caption := 'Nom : ' + Nom;
  CheckBox1.Checked := Option;
end;
```

---

## 4.7.7 Dialogues de saisie avancés

### Dialogue avec sélection multiple

**Exemple : Sélectionner des produits**

```pascal
type
  TFormSelectionProduits = class(TForm)
    CheckListBox1: TCheckListBox;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    ButtonToutSelectionner: TButton;
    ButtonToutDeselectionner: TButton;
    LabelNbSelections: TLabel;
  private
    procedure MettreAJourCompteur;
  public
    procedure ChargerProduits(const Produits: TStringList);
    procedure ObtenirSelection(var ProduitsSelectionnes: TStringList);
  end;

implementation

procedure TFormSelectionProduits.ChargerProduits(const Produits: TStringList);
var
  i: Integer;
begin
  CheckListBox1.Items.Clear;
  for i := 0 to Produits.Count - 1 do
    CheckListBox1.Items.Add(Produits[i]);

  MettreAJourCompteur;
end;

procedure TFormSelectionProduits.ObtenirSelection(
  var ProduitsSelectionnes: TStringList);
var
  i: Integer;
begin
  ProduitsSelectionnes.Clear;
  for i := 0 to CheckListBox1.Items.Count - 1 do
  begin
    if CheckListBox1.Checked[i] then
      ProduitsSelectionnes.Add(CheckListBox1.Items[i]);
  end;
end;

procedure TFormSelectionProduits.MettreAJourCompteur;
var
  i, Compte: Integer;
begin
  Compte := 0;
  for i := 0 to CheckListBox1.Items.Count - 1 do
  begin
    if CheckListBox1.Checked[i] then
      Inc(Compte);
  end;

  LabelNbSelections.Caption := Format('%d produit(s) sélectionné(s)', [Compte]);
  ButtonOK.Enabled := Compte > 0;
end;

procedure TFormSelectionProduits.ButtonToutSelectionnerClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to CheckListBox1.Items.Count - 1 do
    CheckListBox1.Checked[i] := True;
  MettreAJourCompteur;
end;

procedure TFormSelectionProduits.ButtonToutDeselectionnerClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to CheckListBox1.Items.Count - 1 do
    CheckListBox1.Checked[i] := False;
  MettreAJourCompteur;
end;

procedure TFormSelectionProduits.CheckListBox1ClickCheck(Sender: TObject);
begin
  MettreAJourCompteur;
end;
```

**Utilisation :**

```pascal
procedure TFormPrincipal.ButtonSelectionnerProduitsClick(Sender: TObject);
var
  Dialogue: TFormSelectionProduits;
  ListeProduits, ProduitsSelectionnes: TStringList;
begin
  ListeProduits := TStringList.Create;
  ProduitsSelectionnes := TStringList.Create;
  try
    // Charger la liste de produits
    ListeProduits.Add('Ordinateur portable');
    ListeProduits.Add('Souris');
    ListeProduits.Add('Clavier');
    ListeProduits.Add('Écran');
    ListeProduits.Add('Imprimante');

    Dialogue := TFormSelectionProduits.Create(Self);
    try
      Dialogue.ChargerProduits(ListeProduits);

      if Dialogue.ShowModal = mrOk then
      begin
        Dialogue.ObtenirSelection(ProduitsSelectionnes);
        Memo1.Lines.Clear;
        Memo1.Lines.Add('Produits sélectionnés :');
        Memo1.Lines.AddStrings(ProduitsSelectionnes);
      end;
    finally
      Dialogue.Free;
    end;
  finally
    ListeProduits.Free;
    ProduitsSelectionnes.Free;
  end;
end;
```

### Dialogue de progression

Pour les opérations longues :

```pascal
type
  TFormProgression = class(TForm)
    ProgressBar1: TProgressBar;
    LabelEtape: TLabel;
    ButtonAnnuler: TButton;
  private
    FAnnule: Boolean;
  public
    procedure Initialiser(Max: Integer);
    procedure MettreAJourProgression(Position: Integer; const Etape: string);
    property Annule: Boolean read FAnnule;
  end;

implementation

procedure TFormProgression.Initialiser(Max: Integer);
begin
  FAnnule := False;
  ProgressBar1.Min := 0;
  ProgressBar1.Max := Max;
  ProgressBar1.Position := 0;
  LabelEtape.Caption := 'Initialisation...';
end;

procedure TFormProgression.MettreAJourProgression(Position: Integer;
  const Etape: string);
begin
  ProgressBar1.Position := Position;
  LabelEtape.Caption := Etape;
  Application.ProcessMessages; // Permettre la mise à jour de l'interface
end;

procedure TFormProgression.ButtonAnnulerClick(Sender: TObject);
begin
  if MessageDlg('Voulez-vous vraiment annuler l''opération ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    FAnnule := True;
    ButtonAnnuler.Enabled := False;
    LabelEtape.Caption := 'Annulation en cours...';
  end;
end;
```

**Utilisation :**

```pascal
procedure TFormPrincipal.TraiterFichiersAvecProgression;
var
  Dialogue: TFormProgression;
  i: Integer;
begin
  Dialogue := TFormProgression.Create(Self);
  try
    Dialogue.Show; // Non-modal pour permettre l'annulation
    Dialogue.Initialiser(100);

    for i := 1 to 100 do
    begin
      // Vérifier l'annulation
      if Dialogue.Annule then
      begin
        ShowMessage('Opération annulée');
        Break;
      end;

      // Simuler un traitement
      Sleep(50);

      // Mettre à jour la progression
      Dialogue.MettreAJourProgression(i,
        Format('Traitement du fichier %d sur 100', [i]));
    end;

    if not Dialogue.Annule then
      ShowMessage('Traitement terminé avec succès');
  finally
    Dialogue.Free;
  end;
end;
```

---

## 4.7.8 Assistants multi-pages (Wizards)

### Structure d'un assistant

Un assistant guide l'utilisateur à travers plusieurs étapes :

```pascal
type
  TFormAssistant = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet; // Page 1 : Bienvenue
    TabSheet2: TTabSheet; // Page 2 : Configuration
    TabSheet3: TTabSheet; // Page 3 : Confirmation
    ButtonPrecedent: TButton;
    ButtonSuivant: TButton;
    ButtonAnnuler: TButton;
    ButtonTerminer: TButton;
  private
    procedure MettreAJourBoutons;
  public
    procedure Initialiser;
  end;

implementation

procedure TFormAssistant.Initialiser;
begin
  PageControl1.ActivePageIndex := 0;
  PageControl1.TabVisible := False; // Masquer les onglets
  MettreAJourBoutons;
end;

procedure TFormAssistant.MettreAJourBoutons;
var
  PageActuelle: Integer;
begin
  PageActuelle := PageControl1.ActivePageIndex;

  // Bouton Précédent
  ButtonPrecedent.Enabled := PageActuelle > 0;

  // Bouton Suivant
  ButtonSuivant.Visible := PageActuelle < PageControl1.PageCount - 1;

  // Bouton Terminer
  ButtonTerminer.Visible := PageActuelle = PageControl1.PageCount - 1;

  // Changer le texte du bouton Suivant
  if PageActuelle = PageControl1.PageCount - 2 then
    ButtonSuivant.Caption := 'Suivant >'
  else
    ButtonSuivant.Caption := 'Suivant >';
end;

procedure TFormAssistant.ButtonSuivantClick(Sender: TObject);
begin
  // Valider la page actuelle avant de continuer
  if not ValiderPageActuelle then
  begin
    ShowMessage('Veuillez corriger les erreurs avant de continuer');
    Exit;
  end;

  // Passer à la page suivante
  if PageControl1.ActivePageIndex < PageControl1.PageCount - 1 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;
    MettreAJourBoutons;
  end;
end;

procedure TFormAssistant.ButtonPrecedentClick(Sender: TObject);
begin
  // Revenir à la page précédente
  if PageControl1.ActivePageIndex > 0 then
  begin
    PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;
    MettreAJourBoutons;
  end;
end;

function TFormAssistant.ValiderPageActuelle: Boolean;
begin
  Result := True;

  case PageControl1.ActivePageIndex of
    0: // Page 1 : Bienvenue - pas de validation
      Result := True;

    1: // Page 2 : Configuration
      begin
        if Trim(EditNom.Text) = '' then
        begin
          ShowMessage('Le nom est obligatoire');
          EditNom.SetFocus;
          Result := False;
        end;
      end;

    2: // Page 3 : Confirmation
      Result := True;
  end;
end;

procedure TFormAssistant.ButtonTerminerClick(Sender: TObject);
begin
  // Valider une dernière fois
  if not ValiderPageActuelle then
  begin
    ShowMessage('Veuillez corriger les erreurs');
    Exit;
  end;

  // Confirmer
  if MessageDlg('Voulez-vous terminer la configuration ?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ModalResult := mrOk;
  end;
end;

procedure TFormAssistant.FormCreate(Sender: TObject);
begin
  Initialiser;
end;
```

### Page de résumé dans l'assistant

```pascal
procedure TFormAssistant.PageControl1Change(Sender: TObject);
begin
  MettreAJourBoutons;

  // Si on arrive sur la page de confirmation, afficher un résumé
  if PageControl1.ActivePageIndex = 2 then
    AfficherResume;
end;

procedure TFormAssistant.AfficherResume;
begin
  MemoResume.Lines.Clear;
  MemoResume.Lines.Add('=== RÉSUMÉ DE LA CONFIGURATION ===');
  MemoResume.Lines.Add('');
  MemoResume.Lines.Add('Nom : ' + EditNom.Text);
  MemoResume.Lines.Add('Email : ' + EditEmail.Text);
  MemoResume.Lines.Add('Type : ' + ComboBoxType.Text);
  MemoResume.Lines.Add('');
  MemoResume.Lines.Add('Options sélectionnées :');
  if CheckBox1.Checked then
    MemoResume.Lines.Add('  - ' + CheckBox1.Caption);
  if CheckBox2.Checked then
    MemoResume.Lines.Add('  - ' + CheckBox2.Caption);
end;
```

---

## 4.7.9 Dialogues avec onglets

Pour organiser beaucoup d'informations :

```pascal
type
  TFormParametres = class(TForm)
    PageControl1: TPageControl;
    TabSheetGeneral: TTabSheet;
    TabSheetAffichage: TTabSheet;
    TabSheetAvance: TTabSheet;
    ButtonOK: TButton;
    ButtonAnnuler: TButton;
    ButtonAppliquer: TButton;
  private
    FModifie: Boolean;
    procedure ChargerParametres;
    procedure EnregistrerParametres;
  end;

implementation

procedure TFormParametres.FormCreate(Sender: TObject);
begin
  FModifie := False;
  ButtonAppliquer.Enabled := False;
  ChargerParametres;
end;

procedure TFormParametres.ChargerParametres;
begin
  // Charger les paramètres depuis un fichier INI ou registre
  EditNom.Text := LireParametre('Nom', 'Utilisateur');
  CheckBoxNotifications.Checked := LireParametreBool('Notifications', True);
  ComboBoxTheme.ItemIndex := LireParametreInt('Theme', 0);
  // etc.
end;

procedure TFormParametres.EnregistrerParametres;
begin
  // Enregistrer les paramètres
  EcrireParametre('Nom', EditNom.Text);
  EcrireParametreBool('Notifications', CheckBoxNotifications.Checked);
  EcrireParametreInt('Theme', ComboBoxTheme.ItemIndex);
  // etc.

  FModifie := False;
  ButtonAppliquer.Enabled := False;
end;

procedure TFormParametres.ControleChange(Sender: TObject);
begin
  // Marquer comme modifié
  FModifie := True;
  ButtonAppliquer.Enabled := True;
end;

procedure TFormParametres.ButtonAppliquerClick(Sender: TObject);
begin
  EnregistrerParametres;
  ShowMessage('Paramètres enregistrés');
end;

procedure TFormParametres.ButtonOKClick(Sender: TObject);
begin
  if FModifie then
    EnregistrerParametres;
  ModalResult := mrOk;
end;

procedure TFormParametres.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if FModifie and (ModalResult = mrOk) then
  begin
    case MessageDlg('Enregistrer les modifications ?',
                    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        begin
          EnregistrerParametres;
          CanClose := True;
        end;
      mrNo:
        CanClose := True;
      mrCancel:
        CanClose := False;
    end;
  end
  else
    CanClose := True;
end;
```

---

## 4.7.10 Bonnes pratiques

### 1. Taille et positionnement

```pascal
procedure TFormDialogue.FormCreate(Sender: TObject);
begin
  // Taille appropriée
  Width := 450;
  Height := 300;

  // Centrer à l'écran ou par rapport au parent
  Position := poScreenCenter; // ou poMainFormCenter

  // Empêcher le redimensionnement
  BorderStyle := bsDialog;

  // Désactiver les boutons min/max
  BorderIcons := [biSystemMenu];
end;
```

### 2. Focus initial

```pascal
procedure TFormDialogue.FormShow(Sender: TObject);
begin
  // Placer le focus sur le premier champ
  EditNom.SetFocus;

  // Ou sélectionner tout le texte
  EditNom.SelectAll;
end;
```

### 3. Touches de raccourci

```pascal
procedure TFormDialogue.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Échap pour annuler
  if Key = VK_ESCAPE then
  begin
    ModalResult := mrCancel;
    Key := 0;
  end;

  // Ctrl+Entrée pour valider
  if (ssCtrl in Shift) and (Key = VK_RETURN) then
  begin
    if ButtonOK.Enabled then
    begin
      ButtonOKClick(ButtonOK);
      Key := 0;
    end;
  end;
end;
```

### 4. Messages d'aide

```pascal
procedure TFormDialogue.LabelAideClick(Sender: TObject);
begin
  ShowMessage(
    'Nom d''utilisateur : 3-20 caractères' + #13#10 +
    'Mot de passe : minimum 6 caractères' + #13#10 +
    'Se souvenir : enregistre vos identifiants localement'
  );
end;

// Ou utiliser des Hints
procedure TFormDialogue.FormCreate(Sender: TObject);
begin
  ShowHint := True;
  EditNom.Hint := 'Entrez votre nom d''utilisateur (3-20 caractères)';
  EditMotDePasse.Hint := 'Minimum 6 caractères, sensible à la casse';
end;
```

### 5. Cohérence visuelle

```pascal
// Utiliser les mêmes marges et espacements
const
  MARGE = 10;
  ESPACEMENT = 5;

procedure TFormDialogue.AlignerControles;
begin
  Label1.Left := MARGE;
  Label1.Top := MARGE;

  Edit1.Left := MARGE;
  Edit1.Top := Label1.Top + Label1.Height + ESPACEMENT;

  Label2.Left := MARGE;
  Label2.Top := Edit1.Top + Edit1.Height + ESPACEMENT;

  // etc.
end;
```

### 6. Gestion de la mémoire

```pascal
// Toujours libérer les dialogues
procedure TFormPrincipal.AfficherDialogue;
var
  Dialogue: TFormDialogue;
begin
  Dialogue := TFormDialogue.Create(Self);
  try
    // Utilisation du dialogue
    Dialogue.ShowModal;
  finally
    Dialogue.Free; // Libération garantie
  end;
end;

// Pour les dialogues non-modaux
procedure TFormPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree; // Libération automatique
end;
```

### 7. Internationalisation

```pascal
// Préparer pour la traduction
procedure TFormDialogue.FormCreate(Sender: TObject);
begin
  Caption := GetText('DLG_LOGIN_CAPTION', 'Connexion');
  LabelNom.Caption := GetText('DLG_LOGIN_USERNAME', 'Nom d''utilisateur');
  LabelMDP.Caption := GetText('DLG_LOGIN_PASSWORD', 'Mot de passe');
  ButtonOK.Caption := GetText('BTN_OK', 'OK');
  ButtonAnnuler.Caption := GetText('BTN_CANCEL', 'Annuler');
end;
```

### 8. Documentation

```pascal
{
  TFormDialogue - Dialogue de connexion utilisateur

  Usage :
    var
      Dialogue: TFormDialogue;
    begin
      Dialogue := TFormDialogue.Create(Self);
      try
        if Dialogue.ShowModal = mrOk then
          Connecter(Dialogue.NomUtilisateur, Dialogue.MotDePasse);
      finally
        Dialogue.Free;
      end;
    end;

  Propriétés publiques :
    - NomUtilisateur : string (lecture seule)
    - MotDePasse : string (lecture seule)
    - SeSouvenir : Boolean (lecture seule)
}
type
  TFormDialogue = class(TForm)
    // ...
  end;
```

---

## 4.7.11 Exemples de dialogues courants

### Dialogue "À propos"

```pascal
procedure TFormAPropos.FormCreate(Sender: TObject);
begin
  BorderStyle := bsDialog;
  Position := poScreenCenter;

  LabelNomApp.Caption := 'Mon Application';
  LabelVersion.Caption := 'Version 1.0.0';
  LabelCopyright.Caption := '© 2025 Mon Entreprise';
  LabelDescription.Caption := 'Description de l''application...';

  ButtonOK.ModalResult := mrOk;
  ButtonOK.Default := True;
end;
```

### Dialogue de recherche

```pascal
type
  TFormRecherche = class(TForm)
    EditTexte: TEdit;
    CheckBoxCasse: TCheckBox;
    CheckBoxMotEntier: TCheckBox;
    RadioGroupDirection: TRadioGroup;
  public
    function ObtenirCriteres(out Texte: string; out Options: TSearchOptions): Boolean;
  end;

function TFormRecherche.ObtenirCriteres(out Texte: string;
  out Options: TSearchOptions): Boolean;
begin
  Result := ShowModal = mrOk;
  if Result then
  begin
    Texte := EditTexte.Text;
    Options := [];
    if CheckBoxCasse.Checked then
      Include(Options, soMatchCase);
    if CheckBoxMotEntier.Checked then
      Include(Options, soWholeWord);
    if RadioGroupDirection.ItemIndex = 1 then
      Include(Options, soBackward);
  end;
end;
```

### Dialogue de confirmation avec détails

```pascal
type
  TFormConfirmation = class(TForm)
    LabelQuestion: TLabel;
    MemoDetails: TMemo;
    CheckBoxNePlusAfficher: TCheckBox;
    ButtonOui: TButton;
    ButtonNon: TButton;
  public
    class function Confirmer(const Question, Details: string): Boolean;
  end;

class function TFormConfirmation.Confirmer(const Question,
  Details: string): Boolean;
var
  Dialogue: TFormConfirmation;
begin
  Dialogue := TFormConfirmation.Create(nil);
  try
    Dialogue.LabelQuestion.Caption := Question;
    Dialogue.MemoDetails.Lines.Text := Details;
    Result := Dialogue.ShowModal = mrYes;

    // Gérer l'option "Ne plus afficher"
    if Dialogue.CheckBoxNePlusAfficher.Checked then
      EnregistrerPreference('NePlusAfficher_' + Question, True);
  finally
    Dialogue.Free;
  end;
end;
```

---

## Conclusion

La création de dialogues personnalisés est une compétence essentielle pour développer des applications professionnelles avec Delphi. Les dialogues permettent d'interagir efficacement avec l'utilisateur tout en maintenant une interface propre et organisée.

### Points clés à retenir :

- **Dialogues modaux** : Bloquent l'interaction (ShowModal)
- **Dialogues non-modaux** : Permettent l'interaction (Show)
- **ModalResult** : Gère automatiquement la fermeture et le résultat
- **Validation** : Vérifiez les données avant de fermer
- **Communication** : Utilisez des propriétés publiques ou des méthodes
- **Libération** : Toujours libérer la mémoire (Free)
- **Cohérence** : Respectez les conventions d'interface
- **Accessibilité** : Focus, raccourcis clavier, messages d'aide

Avec ces connaissances, vous pouvez créer des dialogues riches et adaptés aux besoins spécifiques de vos applications !

⏭️ [Développement de composants personnalisés](/04-conception-dinterfaces-utilisateur-avec-la-vcl/08-developpement-de-composants-personnalises.md)
