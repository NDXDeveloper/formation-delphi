🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 6.2 Communication entre formulaires

## Introduction

Dans une application comportant plusieurs formulaires, il est fréquent que ces formulaires aient besoin de communiquer entre eux. Par exemple :

- Un formulaire de saisie doit transmettre des données au formulaire principal
- Un formulaire d'options doit informer les autres fenêtres d'un changement de paramètres
- Un formulaire enfant doit notifier son parent d'une action utilisateur

Dans cette section, nous allons explorer les différentes techniques pour faire communiquer vos formulaires de manière efficace et maintenable.

## Les différentes approches de communication

Il existe plusieurs méthodes pour faire communiquer des formulaires :

1. **Accès direct** : Un formulaire accède directement aux propriétés/méthodes d'un autre
2. **Propriétés publiques** : Utilisation de propriétés pour encapsuler les données
3. **Méthodes publiques** : Appel de méthodes définies dans les formulaires
4. **Événements et callbacks** : Communication par notification
5. **Variables globales ou singleton** : Partage de données via un point central

Chaque approche a ses avantages et inconvénients. Nous allons les examiner en détail.

## Méthode 1 : Accès direct aux composants

### Principe

Le formulaire parent accède directement aux composants du formulaire enfant.

### Exemple simple

**Form2.pas (formulaire de saisie)**
```pascal
type
  TForm2 = class(TForm)
    EditNom: TEdit;
    EditPrenom: TEdit;
    ButtonOK: TButton;
  end;
```

**Form1.pas (formulaire principal)**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    // Pré-remplir les champs
    FormSaisie.EditNom.Text := 'Dupont';
    FormSaisie.EditPrenom.Text := 'Jean';

    if FormSaisie.ShowModal = mrOk then
    begin
      // Récupérer les valeurs saisies
      ShowMessage('Nom : ' + FormSaisie.EditNom.Text +
                  ', Prénom : ' + FormSaisie.EditPrenom.Text);
    end;
  finally
    FormSaisie.Free;
  end;
end;
```

### Avantages et inconvénients

**Avantages :**
- Simple et direct
- Facile à comprendre pour les débutants

**Inconvénients :**
- Crée un couplage fort entre les formulaires
- Si vous renommez un composant, vous devez modifier tout le code qui y accède
- Difficile à maintenir dans les grandes applications
- Viole le principe d'encapsulation

**Quand l'utiliser :** Pour des petites applications simples ou des prototypes rapides.

## Méthode 2 : Propriétés publiques

### Principe

Créer des propriétés publiques dans le formulaire pour encapsuler l'accès aux données.

### Exemple

**Form2.pas (formulaire de saisie)**
```pascal
type
  TForm2 = class(TForm)
    EditNom: TEdit;
    EditPrenom: TEdit;
    ButtonOK: TButton;
  private
    function GetNom: string;
    procedure SetNom(const Value: string);
    function GetPrenom: string;
    procedure SetPrenom(const Value: string);
  public
    property Nom: string read GetNom write SetNom;
    property Prenom: string read GetPrenom write SetPrenom;
  end;

implementation

function TForm2.GetNom: string;  
begin  
  Result := EditNom.Text;
end;

procedure TForm2.SetNom(const Value: string);  
begin  
  EditNom.Text := Value;
end;

function TForm2.GetPrenom: string;  
begin  
  Result := EditPrenom.Text;
end;

procedure TForm2.SetPrenom(const Value: string);  
begin  
  EditPrenom.Text := Value;
end;
```

**Form1.pas (formulaire principal)**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    // Utilisation des propriétés
    FormSaisie.Nom := 'Dupont';
    FormSaisie.Prenom := 'Jean';

    if FormSaisie.ShowModal = mrOk then
    begin
      ShowMessage('Nom : ' + FormSaisie.Nom +
                  ', Prénom : ' + FormSaisie.Prenom);
    end;
  finally
    FormSaisie.Free;
  end;
end;
```

### Avantages et inconvénients

**Avantages :**
- Meilleure encapsulation
- Permet de valider ou transformer les données dans les getters/setters
- Facilite les modifications futures
- Le formulaire contrôle comment ses données sont accédées

**Inconvénients :**
- Nécessite plus de code
- Couplage toujours présent (mais réduit)

**Quand l'utiliser :** C'est une bonne pratique générale pour la plupart des applications.

## Méthode 3 : Méthodes publiques

### Principe

Définir des méthodes publiques pour effectuer des actions ou récupérer des données.

### Exemple

**Form2.pas (formulaire de configuration)**
```pascal
type
  TForm2 = class(TForm)
    CheckBoxOption1: TCheckBox;
    CheckBoxOption2: TCheckBox;
    EditServeur: TEdit;
  private
    { Déclarations privées }
  public
    procedure ConfigurerOptions(const Serveur: string; Opt1, Opt2: Boolean);
    function ObtenirConfiguration: string;
    function OptionsValides: Boolean;
  end;

implementation

procedure TForm2.ConfigurerOptions(const Serveur: string; Opt1, Opt2: Boolean);  
begin  
  EditServeur.Text := Serveur;
  CheckBoxOption1.Checked := Opt1;
  CheckBoxOption2.Checked := Opt2;
end;

function TForm2.ObtenirConfiguration: string;  
begin  
  Result := Format('Serveur: %s, Option1: %s, Option2: %s',
    [EditServeur.Text,
     BoolToStr(CheckBoxOption1.Checked, True),
     BoolToStr(CheckBoxOption2.Checked, True)]);
end;

function TForm2.OptionsValides: Boolean;  
begin  
  // Validation des options
  Result := Trim(EditServeur.Text) <> '';
  if not Result then
    ShowMessage('Veuillez saisir un nom de serveur');
end;
```

**Form1.pas (formulaire principal)**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormConfig: TForm2;
begin
  FormConfig := TForm2.Create(Self);
  try
    // Configurer le formulaire
    FormConfig.ConfigurerOptions('localhost', True, False);

    if FormConfig.ShowModal = mrOk then
    begin
      if FormConfig.OptionsValides then
      begin
        MemoLog.Lines.Add(FormConfig.ObtenirConfiguration);
      end;
    end;
  finally
    FormConfig.Free;
  end;
end;
```

### Avantages et inconvénients

**Avantages :**
- Encapsulation complète de la logique
- Validation et traitement centralisés
- Interface claire et documentable
- Facilite les tests unitaires

**Inconvénients :**
- Nécessite plus de conception initiale

**Quand l'utiliser :** Pour des opérations complexes ou nécessitant de la validation.

## Méthode 4 : Événements personnalisés

### Principe

Le formulaire enfant déclenche des événements auxquels le formulaire parent peut s'abonner.

### Exemple

**Form2.pas (formulaire enfant)**
```pascal
type
  // Définir le type d'événement
  TDonneesValideeEvent = procedure(Sender: TObject; const Nom, Prenom: string) of object;

  TForm2 = class(TForm)
    EditNom: TEdit;
    EditPrenom: TEdit;
    ButtonValider: TButton;
    procedure ButtonValiderClick(Sender: TObject);
  private
    FOnDonneesValidee: TDonneesValideeEvent;
  public
    property OnDonneesValidee: TDonneesValideeEvent
      read FOnDonneesValidee write FOnDonneesValidee;
  end;

implementation

procedure TForm2.ButtonValiderClick(Sender: TObject);  
begin  
  // Valider les données
  if (Trim(EditNom.Text) = '') or (Trim(EditPrenom.Text) = '') then
  begin
    ShowMessage('Veuillez remplir tous les champs');
    Exit;
  end;

  // Déclencher l'événement si un gestionnaire est défini
  if Assigned(FOnDonneesValidee) then
    FOnDonneesValidee(Self, EditNom.Text, EditPrenom.Text);

  ModalResult := mrOk;
end;
```

**Form1.pas (formulaire parent)**
```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  private
    procedure GererDonneesValidees(Sender: TObject; const Nom, Prenom: string);
  end;

implementation

procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    // S'abonner à l'événement
    FormSaisie.OnDonneesValidee := GererDonneesValidees;
    FormSaisie.ShowModal;
  finally
    FormSaisie.Free;
  end;
end;

procedure TForm1.GererDonneesValidees(Sender: TObject; const Nom, Prenom: string);  
begin  
  // Traiter les données reçues
  ListBox1.Items.Add(Format('%s %s', [Prenom, Nom]));
  ShowMessage('Données ajoutées avec succès !');
end;
```

### Avantages et inconvénients

**Avantages :**
- Faible couplage : le formulaire enfant ne connaît pas le parent
- Réutilisable : le formulaire enfant peut être utilisé dans différents contextes
- Flexible : plusieurs gestionnaires peuvent s'abonner au même événement
- Suit les conventions Delphi

**Inconvénients :**
- Plus complexe à comprendre pour les débutants
- Nécessite plus de code initial

**Quand l'utiliser :** Pour des composants réutilisables et des architectures propres.

## Méthode 5 : Procédures callback

### Principe

Passer une procédure en paramètre au formulaire enfant.

### Exemple

**Form2.pas (formulaire enfant)**
```pascal
type
  // Définir le type de callback
  TCallbackProcedure = procedure(const Valeur: string) of object;

  TForm2 = class(TForm)
    Edit1: TEdit;
    ButtonOK: TButton;
    procedure ButtonOKClick(Sender: TObject);
  private
    FCallback: TCallbackProcedure;
  public
    property Callback: TCallbackProcedure read FCallback write FCallback;
  end;

implementation

procedure TForm2.ButtonOKClick(Sender: TObject);  
begin  
  if Assigned(FCallback) then
    FCallback(Edit1.Text);
  ModalResult := mrOk;
end;
```

**Form1.pas (formulaire parent)**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    // Définir le callback
    FormSaisie.Callback := procedure(const Valeur: string)
    begin
      Memo1.Lines.Add('Valeur reçue : ' + Valeur);
    end;

    FormSaisie.ShowModal;
  finally
    FormSaisie.Free;
  end;
end;
```

### Note sur les méthodes anonymes

L'exemple ci-dessus utilise une méthode anonyme (introduite depuis Delphi 2009). Voici la version traditionnelle :

```pascal
type
  TForm1 = class(TForm)
  private
    procedure TraiterValeur(const Valeur: string);
  end;

procedure TForm1.TraiterValeur(const Valeur: string);  
begin  
  Memo1.Lines.Add('Valeur reçue : ' + Valeur);
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    FormSaisie.Callback := TraiterValeur;
    FormSaisie.ShowModal;
  finally
    FormSaisie.Free;
  end;
end;
```

## Méthode 6 : Référence au formulaire parent

### Principe

Passer une référence du formulaire parent au formulaire enfant.

### Exemple

**Form2.pas (formulaire enfant)**
```pascal
type
  TForm2 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FFormParent: TForm1;
  public
    constructor Create(AOwner: TComponent; Parent: TForm1); reintroduce;
  end;

implementation

constructor TForm2.Create(AOwner: TComponent; Parent: TForm1);  
begin  
  inherited Create(AOwner);
  FFormParent := Parent;
end;

procedure TForm2.Button1Click(Sender: TObject);  
begin  
  if Assigned(FFormParent) then
  begin
    // Appeler une méthode du parent
    FFormParent.AjouterDansListe('Élément ajouté depuis Form2');
  end;
end;
```

**Form1.pas (formulaire parent)**
```pascal
type
  TForm1 = class(TForm)
    ListBox1: TListBox;
  public
    procedure AjouterDansListe(const Texte: string);
  end;

procedure TForm1.AjouterDansListe(const Texte: string);  
begin  
  ListBox1.Items.Add(Texte);
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormEnfant: TForm2;
begin
  FormEnfant := TForm2.Create(Self, Self);
  try
    FormEnfant.ShowModal;
  finally
    FormEnfant.Free;
  end;
end;
```

### Avantages et inconvénients

**Avantages :**
- Communication directe et simple
- Le formulaire enfant peut facilement accéder au parent

**Inconvénients :**
- Couplage fort
- Le formulaire enfant dépend de la structure du parent
- Réduit la réutilisabilité
- Peut créer des dépendances circulaires

**Quand l'utiliser :** Uniquement pour des formulaires très spécifiques qui ne seront jamais réutilisés ailleurs.

## Méthode 7 : Variables globales ou unité partagée

### Principe

Utiliser une unité commune pour partager des données entre formulaires.

### Exemple

**UnitDonneesPartagees.pas (unité commune)**
```pascal
unit UnitDonneesPartagees;

interface

type
  TDonneesUtilisateur = record
    Nom: string;
    Prenom: string;
    Email: string;
  end;

var
  DonneesUtilisateur: TDonneesUtilisateur;

implementation

end.
```

**Form1.pas**
```pascal
uses UnitDonneesPartagees;

procedure TForm1.Button1Click(Sender: TObject);  
begin  
  DonneesUtilisateur.Nom := EditNom.Text;
  DonneesUtilisateur.Prenom := EditPrenom.Text;
  DonneesUtilisateur.Email := EditEmail.Text;

  Form2.Show;
end;
```

**Form2.pas**
```pascal
uses UnitDonneesPartagees;

procedure TForm2.FormShow(Sender: TObject);  
begin  
  LabelNom.Caption := DonneesUtilisateur.Nom;
  LabelPrenom.Caption := DonneesUtilisateur.Prenom;
  LabelEmail.Caption := DonneesUtilisateur.Email;
end;
```

### Version améliorée avec singleton

```pascal
unit UnitGestionnaireDonnees;

interface

type
  TGestionnaireDonnees = class
  private
    class var FInstance: TGestionnaireDonnees;
    FNomUtilisateur: string;
    FPrenomUtilisateur: string;
  public
    class function Instance: TGestionnaireDonnees;
    class destructor Destroy;

    property NomUtilisateur: string read FNomUtilisateur write FNomUtilisateur;
    property PrenomUtilisateur: string read FPrenomUtilisateur write FPrenomUtilisateur;
  end;

implementation

class function TGestionnaireDonnees.Instance: TGestionnaireDonnees;  
begin  
  if not Assigned(FInstance) then
    FInstance := TGestionnaireDonnees.Create;
  Result := FInstance;
end;

class destructor TGestionnaireDonnees.Destroy;  
begin  
  if Assigned(FInstance) then
    FInstance.Free;
end;

end.
```

**Utilisation :**
```pascal
// Dans Form1
TGestionnaireDonnees.Instance.NomUtilisateur := 'Dupont';

// Dans Form2
ShowMessage(TGestionnaireDonnees.Instance.NomUtilisateur);
```

### Avantages et inconvénients

**Avantages :**
- Facile d'accès depuis n'importe où
- Pratique pour des données vraiment globales
- Le singleton garantit une instance unique

**Inconvénients :**
- Peut devenir difficile à maintenir
- Rend les tests difficiles
- Peut masquer des dépendances
- État global difficile à contrôler

**Quand l'utiliser :** Pour des données véritablement globales (configuration, utilisateur connecté, etc.), mais avec parcimonie.

## Cas pratique : Mise à jour d'un formulaire parent

### Scénario

Un formulaire principal affiche une liste d'éléments. Un formulaire de saisie permet d'ajouter un nouvel élément. Comment mettre à jour la liste du formulaire principal ?

### Solution 1 : Méthode publique

**Form1.pas (principal)**
```pascal
type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  public
    procedure AjouterElement(const Element: string);
  end;

procedure TForm1.AjouterElement(const Element: string);  
begin  
  ListBox1.Items.Add(Element);
end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self, Self);
  try
    FormSaisie.ShowModal;
  finally
    FormSaisie.Free;
  end;
end;
```

**Form2.pas (saisie)**
```pascal
type
  TForm2 = class(TForm)
    Edit1: TEdit;
    ButtonAjouter: TButton;
    procedure ButtonAjouterClick(Sender: TObject);
  private
    FFormPrincipal: TForm1;
  public
    constructor Create(AOwner: TComponent; FormPrincipal: TForm1); reintroduce;
  end;

constructor TForm2.Create(AOwner: TComponent; FormPrincipal: TForm1);  
begin  
  inherited Create(AOwner);
  FFormPrincipal := FormPrincipal;
end;

procedure TForm2.ButtonAjouterClick(Sender: TObject);  
begin  
  if Trim(Edit1.Text) <> '' then
  begin
    FFormPrincipal.AjouterElement(Edit1.Text);
    Edit1.Clear;
    Edit1.SetFocus;
  end;
end;
```

### Solution 2 : Événement (recommandé)

**Form2.pas (saisie)**
```pascal
type
  TElementAjouteEvent = procedure(Sender: TObject; const Element: string) of object;

  TForm2 = class(TForm)
    Edit1: TEdit;
    ButtonAjouter: TButton;
    procedure ButtonAjouterClick(Sender: TObject);
  private
    FOnElementAjoute: TElementAjouteEvent;
  public
    property OnElementAjoute: TElementAjouteEvent
      read FOnElementAjoute write FOnElementAjoute;
  end;

procedure TForm2.ButtonAjouterClick(Sender: TObject);  
begin  
  if Trim(Edit1.Text) <> '' then
  begin
    if Assigned(FOnElementAjoute) then
      FOnElementAjoute(Self, Edit1.Text);
    Edit1.Clear;
    Edit1.SetFocus;
  end;
end;
```

**Form1.pas (principal)**
```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  FormSaisie: TForm2;
begin
  FormSaisie := TForm2.Create(Self);
  try
    FormSaisie.OnElementAjoute := procedure(Sender: TObject; const Element: string)
    begin
      ListBox1.Items.Add(Element);
    end;

    FormSaisie.ShowModal;
  finally
    FormSaisie.Free;
  end;
end;
```

## Bonnes pratiques

### 1. Favoriser le faible couplage

Évitez que les formulaires se connaissent mutuellement directement. Utilisez des événements ou des interfaces.

### 2. Principe de responsabilité unique

Chaque formulaire doit avoir une responsabilité claire et ne pas gérer la logique d'autres formulaires.

### 3. Valider les données au bon endroit

Le formulaire de saisie doit valider ses propres données avant de les transmettre.

```pascal
procedure TForm2.ButtonOKClick(Sender: TObject);  
begin  
  // Validation locale
  if not DonneesValides then
  begin
    ShowMessage('Données invalides');
    Exit;
  end;

  // Déclencher l'événement uniquement si valide
  if Assigned(FOnDonneesValidee) then
    FOnDonneesValidee(Self, ObtenirDonnees);

  ModalResult := mrOk;
end;
```

### 4. Utiliser des types de données structurés

Plutôt que de passer de nombreux paramètres, utilisez des records ou des classes :

```pascal
type
  TPersonne = record
    Nom: string;
    Prenom: string;
    Age: Integer;
    Email: string;
  end;

  TPersonneEvent = procedure(Sender: TObject; const Personne: TPersonne) of object;
```

### 5. Documenter l'interface publique

```pascal
type
  TForm2 = class(TForm)
  public
    /// <summary>
    /// Configure le formulaire avec les données d'une personne
    /// </summary>
    /// <param name="Personne">Données de la personne à afficher</param>
    procedure ConfigurerPersonne(const Personne: TPersonne);

    /// <summary>
    /// Récupère les données saisies par l'utilisateur
    /// </summary>
    /// <returns>Record contenant les données de la personne</returns>
    function ObtenirPersonne: TPersonne;
  end;
```

### 6. Éviter les références circulaires

Si Form1 référence Form2 et Form2 référence Form1, cela crée une dépendance circulaire problématique. Utilisez plutôt :

- Des interfaces
- Des événements
- Une unité commune pour les types partagés
- Le pattern Observer

## Tableau récapitulatif

| Méthode | Couplage | Complexité | Réutilisabilité | Cas d'usage |
|---------|----------|------------|-----------------|-------------|
| Accès direct | Fort | Faible | Faible | Prototypes rapides |
| Propriétés | Moyen | Moyenne | Moyenne | Applications générales |
| Méthodes publiques | Moyen | Moyenne | Bonne | Opérations encapsulées |
| Événements | Faible | Élevée | Excellente | Composants réutilisables |
| Callbacks | Faible | Moyenne | Bonne | Actions ponctuelles |
| Référence parent | Fort | Faible | Faible | Formulaires très spécifiques |
| Variables globales | Faible | Faible | Moyenne | Données vraiment globales |

## Résumé

La communication entre formulaires est essentielle dans le développement d'applications Delphi. Les points clés à retenir :

- **Pour débuter** : utilisez l'accès direct ou les propriétés publiques
- **Pour des applications professionnelles** : privilégiez les événements et les méthodes publiques
- **Réduisez le couplage** : les formulaires ne devraient pas dépendre les uns des autres
- **Encapsulez** : ne donnez pas accès directement aux composants internes
- **Documentez** : expliquez clairement comment utiliser vos formulaires
- **Pensez réutilisable** : un formulaire bien conçu peut servir dans plusieurs contextes

Choisissez la méthode appropriée en fonction de votre contexte, de la taille de votre projet et de vos besoins de réutilisabilité.

⏭️ [Formulaires MDI (Multiple Document Interface)](/06-applications-multi-fenetres-et-navigation/03-formulaires-mdi.md)
