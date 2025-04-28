# 6.1 Gestion des formulaires multiples

Les applications réelles dépassent rarement le cadre d'un formulaire unique. Apprenez à créer et gérer plusieurs formulaires pour construire des applications Delphi complètes.

## Comprendre les formulaires en Delphi

Dans Delphi, un formulaire (ou fiche) est une fenêtre de l'application représentée par une classe dérivée de `TForm`. Par défaut, votre projet commence avec un formulaire principal (`Form1`), mais une application professionnelle en contient généralement plusieurs :

- Fenêtre principale
- Boîtes de dialogue
- Écrans de configuration
- Fenêtres d'édition de données
- Assistants (wizards)
- Et bien d'autres...

## Ajouter un nouveau formulaire à votre projet

Pour ajouter un nouveau formulaire à votre projet :

1. Dans le menu, cliquez sur **File** → **New** → **Form**
2. Une nouvelle fiche vide apparaît dans l'éditeur
3. Enregistrez le formulaire (Ctrl+S) en lui donnant un nom significatif (ex: `FormClients.pas`)

Chaque formulaire est composé de :
- Un fichier `.pas` contenant le code
- Un fichier `.dfm` stockant la définition visuelle (propriétés, composants)

## Méthodes de création de formulaires

Il existe plusieurs façons de créer et d'utiliser des formulaires secondaires :

### 1. Création automatique au démarrage

Par défaut, Delphi crée automatiquement le formulaire principal au démarrage de l'application. Vous pouvez configurer d'autres formulaires pour qu'ils soient également créés automatiquement :

1. Ouvrez le **Project Manager** (Ctrl+Alt+P)
2. Cliquez-droit sur le projet → **Options du projet**
3. Dans l'onglet **Formulaires**, vous pouvez ajouter des formulaires à la liste **Auto-create forms**

```pascal
// Dans le fichier .dpr du projet :
Application.CreateForm(TFormPrincipal, FormPrincipal);
Application.CreateForm(TFormClients, FormClients);
```

⚠️ **Attention** : Les formulaires auto-créés restent en mémoire pendant toute la durée de l'application et consomment des ressources. Utilisez cette méthode pour les formulaires principaux ou fréquemment utilisés.

### 2. Création dynamique à la demande

Pour une meilleure gestion de la mémoire, il est préférable de créer les formulaires secondaires uniquement lorsqu'ils sont nécessaires et de les libérer ensuite :

```pascal
procedure TFormPrincipal.btnClientsClick(Sender: TObject);
var
  FormClients: TFormClients;
begin
  FormClients := TFormClients.Create(Self);
  try
    FormClients.ShowModal; // Affiche le formulaire et attend sa fermeture
  finally
    FormClients.Free; // Libère la mémoire
  end;
end;
```

Cette approche est recommandée pour :
- Les boîtes de dialogue
- Les formulaires rarement utilisés
- Les formulaires nécessitant beaucoup de ressources

### 3. Création unique (Singleton)

Pour certains formulaires, vous souhaiterez peut-être les créer une seule fois mais les afficher/masquer selon les besoins :

```pascal
procedure TFormPrincipal.btnParametresClick(Sender: TObject);
begin
  if FFormParametres = nil then
    FFormParametres := TFormParametres.Create(Self);

  FFormParametres.Show; // Affiche le formulaire sans bloquer
  FFormParametres.BringToFront; // Amène au premier plan
end;
```

N'oubliez pas d'ajouter une variable dans la section `private` de votre formulaire principal :

```pascal
private
  FFormParametres: TFormParametres;
```

Et de libérer cette référence dans le destructeur du formulaire principal :

```pascal
procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  if Assigned(FFormParametres) then
    FFormParametres.Free;
end;
```

## Modes d'affichage des formulaires

Il existe deux façons principales d'afficher un formulaire :

### 1. Mode modal (ShowModal)

```pascal
FormClients.ShowModal;
```

- Bloque l'accès aux autres formulaires tant que celui-ci est ouvert
- Renvoie une valeur à la fermeture (mrOk, mrCancel, etc.)
- Idéal pour les boîtes de dialogue nécessitant une réponse utilisateur

### 2. Mode non-modal (Show)

```pascal
FormClients.Show;
```

- L'utilisateur peut interagir avec les autres formulaires
- L'exécution du code continue immédiatement après l'appel
- Recommandé pour les fenêtres secondaires d'information ou de travail parallèle

## Exemple pratique : Application avec liste et détails

Créons une application simple avec deux formulaires :
- Un formulaire principal affichant une liste
- Un formulaire de détail pour éditer un élément

### Étape 1 : Créer le formulaire principal

```pascal
unit FormMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    btnAjouter: TButton;
    btnModifier: TButton;
    btnSupprimer: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAjouterClick(Sender: TObject);
    procedure btnModifierClick(Sender: TObject);
  private
    procedure ChargerDonnees;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses FormDetail; // Inclut l'unité du second formulaire

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ChargerDonnees;
end;

procedure TMainForm.ChargerDonnees;
begin
  // Exemple : charger des données dans la liste
  ListBox1.Items.Clear;
  ListBox1.Items.Add('Produit 1 - 19.99€');
  ListBox1.Items.Add('Produit 2 - 24.99€');
  ListBox1.Items.Add('Produit 3 - 9.99€');
  ListBox1.Items.Add('Produit 4 - 49.99€');
end;

procedure TMainForm.btnAjouterClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(Self);
  try
    DetailForm.Caption := 'Ajouter un produit';
    DetailForm.edtNom.Text := '';
    DetailForm.edtPrix.Text := '';

    if DetailForm.ShowModal = mrOk then
    begin
      // Ajouter le nouveau produit à la liste
      ListBox1.Items.Add(
        DetailForm.edtNom.Text + ' - ' +
        DetailForm.edtPrix.Text + '€'
      );
    end;
  finally
    DetailForm.Free;
  end;
end;

procedure TMainForm.btnModifierClick(Sender: TObject);
var
  DetailForm: TDetailForm;
  Index: Integer;
  Nom, Prix: string;
begin
  Index := ListBox1.ItemIndex;
  if Index = -1 then
  begin
    ShowMessage('Veuillez sélectionner un produit à modifier');
    Exit;
  end;

  // Extraire le nom et le prix (format: "Nom - Prix€")
  SplitNomPrix(ListBox1.Items[Index], Nom, Prix);

  DetailForm := TDetailForm.Create(Self);
  try
    DetailForm.Caption := 'Modifier un produit';
    DetailForm.edtNom.Text := Nom;
    DetailForm.edtPrix.Text := Prix;

    if DetailForm.ShowModal = mrOk then
    begin
      // Mettre à jour le produit dans la liste
      ListBox1.Items[Index] :=
        DetailForm.edtNom.Text + ' - ' +
        DetailForm.edtPrix.Text + '€';
    end;
  finally
    DetailForm.Free;
  end;
end;

// Fonction utilitaire pour extraire le nom et le prix
procedure TMainForm.SplitNomPrix(const Item: string; var Nom, Prix: string);
var
  Pos: Integer;
begin
  Pos := Item.IndexOf(' - ');
  if Pos > 0 then
  begin
    Nom := Item.Substring(0, Pos);
    Prix := Item.Substring(Pos + 3, Item.Length - Pos - 4); // Enlever le '€'
  end
  else
  begin
    Nom := Item;
    Prix := '0';
  end;
end;

end.
```

### Étape 2 : Créer le formulaire de détail

```pascal
unit FormDetail;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TDetailForm = class(TForm)
    edtNom: TEdit;
    edtPrix: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    btnOK: TButton;
    btnAnnuler: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

implementation

{$R *.dfm}

procedure TDetailForm.FormCreate(Sender: TObject);
begin
  // Position centrale par rapport au parent
  Position := poOwnerFormCenter;
end;

procedure TDetailForm.btnOKClick(Sender: TObject);
begin
  // Validation simple
  if edtNom.Text.Trim = '' then
  begin
    ShowMessage('Veuillez saisir un nom de produit');
    edtNom.SetFocus;
    Exit;
  end;

  if not TryStrToFloat(edtPrix.Text.Replace(',', '.'), nil) then
  begin
    ShowMessage('Le prix doit être un nombre valide');
    edtPrix.SetFocus;
    Exit;
  end;

  // Si tout est valide, on ferme avec OK
  ModalResult := mrOk;
end;

end.
```

## Partage de données entre formulaires

Il existe plusieurs façons de partager des données entre formulaires :

### 1. Passage de paramètres au constructeur

Vous pouvez créer un constructeur personnalisé pour passer des données :

```pascal
// Dans l'unité du formulaire de détail
constructor TDetailForm.Create(AOwner: TComponent; AProduitID: Integer);
begin
  inherited Create(AOwner);
  FProduitID := AProduitID;
  // Charger les données du produit
end;

// Dans le formulaire principal
DetailForm := TDetailForm.Create(Self, ProduitID);
```

### 2. Propriétés publiques

Définissez des propriétés publiques dans votre formulaire que vous pourrez manipuler :

```pascal
// Dans l'interface du formulaire de détail
public
  property ProduitID: Integer read FProduitID write SetProduitID;

// Dans le formulaire principal
DetailForm := TDetailForm.Create(Self);
DetailForm.ProduitID := 123;
```

### 3. Une classe de données partagée

Pour les applications plus complexes, créez une classe de données partagée :

```pascal
unit DataModule;

interface

type
  TDM = class(TDataModule)
    // Composants d'accès aux données
  public
    // Méthodes d'accès/modification des données
  end;

var
  DM: TDM; // Variable globale accessible depuis toutes les unités

implementation

end.
```

## Bonnes pratiques

1. **Nommage** : Donnez des noms significatifs à vos formulaires (pas Form1, Form2...)
2. **Responsabilité unique** : Chaque formulaire doit avoir un objectif précis
3. **Libération mémoire** : Utilisez toujours des blocs try/finally pour libérer les formulaires créés
4. **Fermeture propre** : Assurez-vous que tous les formulaires enfants sont fermés quand le formulaire parent se ferme
5. **Code minimal** : Gardez le code dans les formulaires secondaires au minimum, centralisez la logique métier

## Astuces et pièges à éviter

- **Évitez les références circulaires** entre unités (A utilise B qui utilise A)
- **Attention aux formulaires auto-créés** qui restent en mémoire
- **Ne pas abuser de `Application.MainForm`** pour accéder au formulaire principal
- **Utilisez les interfaces** pour découpler les formulaires dans les applications complexes

## Exercices pratiques

1. Créez une application "Carnet d'adresses" avec un formulaire principal (liste) et un formulaire de détail (ajout/modification)
2. Ajoutez un troisième formulaire "Paramètres" accessible depuis le formulaire principal
3. Implémentez le stockage des données (fichier texte ou base de données simple)

---

Maintenant que vous maîtrisez la gestion des formulaires multiples, vous pouvez créer des applications Delphi plus sophistiquées avec différentes fenêtres interagissant entre elles !
