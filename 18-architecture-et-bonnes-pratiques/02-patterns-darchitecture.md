# 18.2 Patterns d'architecture (MVC, MVVM)

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Lorsque vous débutez en programmation, vous créez souvent des applications où tout le code (interface, logique métier, accès aux données) est mélangé dans les mêmes fichiers. Cette approche fonctionne pour de petits projets, mais devient rapidement problématique quand votre application grandit. Les patterns d'architecture sont des solutions éprouvées pour organiser votre code de manière plus structurée, maintenable et évolutive.

## Pourquoi utiliser des patterns d'architecture ?

Avant de plonger dans les détails techniques, comprenons les avantages des patterns d'architecture :

- **Séparation des préoccupations** : Chaque partie du code a une responsabilité unique et bien définie
- **Testabilité** : Vous pouvez tester chaque composant indépendamment
- **Maintenabilité** : Le code est plus facile à comprendre et à modifier
- **Collaboration** : Plusieurs développeurs peuvent travailler sur différentes parties sans se gêner
- **Réutilisabilité** : Les composants bien isolés peuvent être réutilisés dans d'autres projets
- **Évolutivité** : Ajoutez de nouvelles fonctionnalités sans perturber l'existant

## Les principaux patterns en Delphi

Nous allons explorer trois patterns d'architecture particulièrement utiles en Delphi :

1. **MVC** (Model-View-Controller) - Le pattern classique
2. **MVP** (Model-View-Presenter) - Une variation adaptée à Delphi
3. **MVVM** (Model-View-ViewModel) - Un pattern moderne très puissant

## MVC (Model-View-Controller)

Le MVC est l'un des patterns les plus anciens et les plus utilisés. Il divise votre application en trois composants :

### Les composants du MVC

- **Modèle (Model)** : Représente les données et la logique métier
- **Vue (View)** : Affiche les données et capture les interactions utilisateur
- **Contrôleur (Controller)** : Fait le lien entre le modèle et la vue, gère les événements

![Diagramme MVC](https://placeholder.com/MVC_Diagram.png)

### Fonctionnement du MVC

1. L'utilisateur interagit avec la **Vue** (clique sur un bouton, saisit du texte...)
2. La **Vue** notifie le **Contrôleur** de cette action
3. Le **Contrôleur** met à jour le **Modèle** si nécessaire
4. Le **Modèle** notifie ses observateurs (souvent la **Vue**) qu'il a changé
5. La **Vue** interroge le **Modèle** pour obtenir son nouvel état et se met à jour

### Exemple simple en Delphi

Voici comment vous pourriez implémenter un pattern MVC basique pour une application de gestion de contacts :

#### Le Modèle (TContactModel)

```pascal
unit ContactModel;

interface

uses
  System.Classes;

type
  TContact = class
  private
    FId: Integer;
    FName: string;
    FEmail: string;
    FPhone: string;
  public
    property Id: Integer read FId write FId;
    property Name: string read FName write FName;
    property Email: string read FEmail write FEmail;
    property Phone: string read FPhone write FPhone;
  end;

  TContactModel = class
  private
    FContacts: TList;
    FOnChange: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddContact(const Name, Email, Phone: string);
    procedure DeleteContact(Id: Integer);
    function GetContact(Index: Integer): TContact;
    function GetContactCount: Integer;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

// Implémentation des méthodes...

end.
```

#### La Vue (TContactForm)

```pascal
unit ContactView;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Grids, ContactModel,
  ContactController;

type
  TContactForm = class(TForm)
    StringGrid1: TStringGrid;
    EditName: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    ButtonAdd: TButton;
    ButtonDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
  private
    FController: TContactController;
    procedure UpdateView;
  public
    procedure SetController(AController: TContactController);
    procedure ModelChanged(Sender: TObject);
  end;

implementation

{$R *.dfm}

// Implémentation des méthodes...

end.
```

#### Le Contrôleur (TContactController)

```pascal
unit ContactController;

interface

uses
  ContactModel;

type
  TContactController = class
  private
    FModel: TContactModel;
  public
    constructor Create(AModel: TContactModel);

    procedure AddContact(const Name, Email, Phone: string);
    procedure DeleteContact(Id: Integer);

    property Model: TContactModel read FModel;
  end;

implementation

// Implémentation des méthodes...

end.
```

### Avantages du MVC

- Pattern bien connu et documenté
- Séparation claire des responsabilités
- Facilite les tests unitaires
- Structure claire pour les petites et moyennes applications

### Inconvénients du MVC

- La Vue et le Modèle sont parfois trop couplés
- Le Contrôleur peut devenir trop volumineux ("fat controller")
- Moins adapté aux interfaces complexes avec beaucoup d'états

## MVP (Model-View-Presenter)

Le MVP est une évolution du MVC, particulièrement adaptée aux applications Delphi avec interfaces graphiques riches.

### Les composants du MVP

- **Modèle (Model)** : Identique au MVC, gère les données et la logique métier
- **Vue (View)** : Plus passive que dans le MVC, se contente d'afficher les données
- **Présenteur (Presenter)** : Récupère les données du modèle et les formate pour la vue

![Diagramme MVP](https://placeholder.com/MVP_Diagram.png)

### Différences clés avec le MVC

1. La **Vue** est plus passive et ne connaît pas le **Modèle**
2. Toute communication passe par le **Présenteur**
3. La **Vue** expose une interface que le **Présenteur** utilise

### Exemple en Delphi

Voici comment notre exemple de contacts pourrait être réimplémenté en MVP :

#### L'interface de la Vue

```pascal
unit ContactView;

interface

type
  IContactView = interface
    ['{GUID-UNIQUE}']
    procedure SetContactList(const Contacts: TArray<string>);
    function GetName: string;
    function GetEmail: string;
    function GetPhone: string;
    function GetSelectedIndex: Integer;
    procedure ClearInputs;
  end;
```

#### Implémentation de la Vue

```pascal
TContactForm = class(TForm, IContactView)
  // Contrôles visuels...
private
  FPresenter: TContactPresenter;
public
  // Implémentation de IContactView
  procedure SetContactList(const Contacts: TArray<string>);
  function GetName: string;
  function GetEmail: string;
  function GetPhone: string;
  function GetSelectedIndex: Integer;
  procedure ClearInputs;

  procedure SetPresenter(APresenter: TContactPresenter);
end;
```

#### Le Présenteur

```pascal
unit ContactPresenter;

interface

uses
  ContactModel, ContactView;

type
  TContactPresenter = class
  private
    FModel: TContactModel;
    FView: IContactView;
    procedure UpdateView;
  public
    constructor Create(AModel: TContactModel; AView: IContactView);

    procedure AddContact;
    procedure DeleteContact;
    procedure ModelChanged(Sender: TObject);
  end;

implementation

// Implémentation...

end.
```

### Avantages du MVP

- La Vue est plus facilement remplaçable (grâce à l'interface)
- Meilleure testabilité que le MVC
- Meilleure séparation des préoccupations
- Bien adapté aux applications Delphi traditionnelles

### Inconvénients du MVP

- Plus verbeux que le MVC
- Le Présenteur peut devenir complexe
- Nécessite de définir des interfaces pour les Vues

## MVVM (Model-View-ViewModel)

Le MVVM est un pattern plus récent, qui tire parti de la liaison de données (data binding) pour réduire encore le code de "plomberie".

### Les composants du MVVM

- **Modèle (Model)** : Comme dans les autres patterns
- **Vue (View)** : Interface utilisateur, très légère en code
- **ViewModel** : Adaptateur entre le Modèle et la Vue, expose des propriétés et commandes

![Diagramme MVVM](https://placeholder.com/MVVM_Diagram.png)

### Fonctionnement du MVVM

1. La **Vue** se lie (binding) directement aux propriétés du **ViewModel**
2. Quand l'utilisateur interagit avec la **Vue**, les changements sont automatiquement propagés au **ViewModel**
3. Le **ViewModel** met à jour le **Modèle** si nécessaire
4. Quand le **Modèle** change, le **ViewModel** est mis à jour
5. Les changements du **ViewModel** sont automatiquement reflétés dans la **Vue** grâce au binding

### Implémentation en Delphi

Delphi supporte le MVVM via la fonctionnalité LiveBindings (introduite dans Delphi XE2). Voici un exemple simplifié :

#### Le ViewModel

```pascal
unit ContactViewModel;

interface

uses
  System.Classes, System.Generics.Collections, ContactModel;

type
  TContactViewModel = class(TComponent)
  private
    FModel: TContactModel;
    FName: string;
    FEmail: string;
    FPhone: string;
    FSelectedIndex: Integer;
    FContacts: TList<string>;

    procedure SetName(const Value: string);
    procedure SetEmail(const Value: string);
    procedure SetPhone(const Value: string);
    procedure SetSelectedIndex(const Value: Integer);
    procedure ModelChanged(Sender: TObject);
    procedure UpdateContactsList;
  public
    constructor Create(AOwner: TComponent; AModel: TContactModel); reintroduce;
    destructor Destroy; override;

    procedure AddContact;
    procedure DeleteContact;

    property Name: string read FName write SetName;
    property Email: string read FEmail write SetEmail;
    property Phone: string read FPhone write SetPhone;
    property SelectedIndex: Integer read FSelectedIndex write SetSelectedIndex;
    property Contacts: TList<string> read FContacts;
  end;

implementation

// Implémentation...

end.
```

#### La Vue avec LiveBindings

En Delphi, vous configureriez les liaisons via l'éditeur LiveBindings ou en code :

```pascal
// Configuration des liaisons en code
BindingsList1.AddBinding(ContactViewModel1, 'Name', EditName, 'Text',
                        True, True);
BindingsList1.AddBinding(ContactViewModel1, 'Email', EditEmail, 'Text',
                        True, True);
BindingsList1.AddBinding(ContactViewModel1, 'Phone', EditPhone, 'Text',
                        True, True);
BindingsList1.AddBinding(ContactViewModel1, 'Contacts', ListBox1, 'Items');
BindingsList1.AddBinding(ContactViewModel1, 'SelectedIndex', ListBox1, 'ItemIndex',
                        True, True);
```

### Avantages du MVVM

- Réduction significative du code de "plomberie" grâce au binding
- Séparation très claire des responsabilités
- Excellente testabilité
- Bien adapté aux interfaces complexes
- Support des designers UX/UI

### Inconvénients du MVVM

- Courbe d'apprentissage plus importante
- Configuration des bindings parfois complexe
- Performance potentiellement impactée par le mécanisme de binding
- Débogage parfois plus difficile

## Choisir le bon pattern pour votre projet

Le choix du pattern dépend de plusieurs facteurs :

### MVC est recommandé quand :

- Vous débutez avec les patterns d'architecture
- Votre application est simple à moyenne
- Vous préférez une approche traditionnelle et bien documentée

### MVP est recommandé quand :

- Vous développez une application VCL traditionnelle
- Vous avez besoin d'une meilleure testabilité que le MVC
- Vous voulez une séparation claire entre l'interface et la logique

### MVVM est recommandé quand :

- Vous développez une application complexe avec FireMonkey
- Vous utilisez Delphi XE2 ou supérieur avec LiveBindings
- Vous avez une interface utilisateur riche avec beaucoup d'états
- Vous recherchez une séparation maximale entre UI et logique

## Mise en œuvre progressive pour débutants

Si vous débutez avec les patterns d'architecture, voici une approche progressive :

### Étape 1 : Séparer les données

Commencez simplement en extrayant vos classes de données dans des unités séparées :

```pascal
// Avant : Tout dans le formulaire
TCustomer = class
  // Propriétés et méthodes du client
end;

// Après : Classe déplacée dans sa propre unité
unit CustomerModel;

interface

type
  TCustomer = class
    // Propriétés et méthodes du client
  end;

implementation
end.
```

### Étape 2 : Introduire un modèle simple

Créez une classe qui gère vos données et la logique métier :

```pascal
unit CustomerModel;

interface

type
  TCustomerModel = class
  private
    FCustomers: TList<TCustomer>;
    FOnChange: TNotifyEvent;
  public
    procedure AddCustomer(const Name: string);
    // Autres méthodes...

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
```

### Étape 3 : Simplifier le formulaire (Vue)

Faites en sorte que votre formulaire utilise le modèle pour les opérations :

```pascal
procedure TCustomerForm.ButtonAddClick(Sender: TObject);
begin
  // Avant : Logique directement dans l'événement

  // Après : Délégation au modèle
  FCustomerModel.AddCustomer(EditName.Text);
end;

procedure TCustomerForm.ModelChanged(Sender: TObject);
begin
  // Mettre à jour l'interface utilisateur
  UpdateCustomerList;
end;
```

### Étape 4 : Introduire un contrôleur ou présenteur

Ajoutez progressivement une couche intermédiaire :

```pascal
unit CustomerController;

interface

uses
  CustomerModel;

type
  TCustomerController = class
  private
    FModel: TCustomerModel;
  public
    constructor Create(AModel: TCustomerModel);

    procedure AddCustomer(const Name: string);
    // Autres méthodes...
  end;
```

## Exemple complet avec MVC <span style="background-color: #e9f7fe; padding: 2px 5px; border-radius: 3px; font-size: 0.8em;">Nécessite Delphi 10 ou supérieur</span>

Voici un exemple plus complet d'une application Todo List utilisant le pattern MVC :

### Le Modèle (TodoModel.pas)

```pascal
unit TodoModel;

interface

uses
  System.Classes, System.Generics.Collections;

type
  TTodoItem = class
  private
    FDescription: string;
    FCompleted: Boolean;
  public
    constructor Create(const ADescription: string);

    property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
  end;

  TTodoModel = class
  private
    FItems: TObjectList<TTodoItem>;
    FOnChange: TNotifyEvent;
    procedure DoChange;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddItem(const Description: string);
    procedure RemoveItem(Index: Integer);
    procedure ToggleCompleted(Index: Integer);
    function GetItem(Index: Integer): TTodoItem;
    function GetCount: Integer;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

// Implémentation des méthodes...

end.
```

### Le Contrôleur (TodoController.pas)

```pascal
unit TodoController;

interface

uses
  TodoModel;

type
  TTodoController = class
  private
    FModel: TTodoModel;
  public
    constructor Create(AModel: TTodoModel);

    procedure AddTodo(const Description: string);
    procedure RemoveTodo(Index: Integer);
    procedure ToggleTodo(Index: Integer);

    property Model: TTodoModel read FModel;
  end;

implementation

// Implémentation des méthodes...

end.
```

### La Vue (TodoView.pas)

```pascal
unit TodoView;

interface

uses
  Winapi.Windows, System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Vcl.CheckLst, TodoController, TodoModel;

type
  TTodoForm = class(TForm)
    EditNewTodo: TEdit;
    ButtonAdd: TButton;
    ButtonRemove: TButton;
    CheckListBoxTodos: TCheckListBox;
    procedure FormCreate(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonRemoveClick(Sender: TObject);
    procedure CheckListBoxTodosClickCheck(Sender: TObject);
  private
    FController: TTodoController;
    FModel: TTodoModel;
    procedure UpdateView;
    procedure ModelChanged(Sender: TObject);
  public
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

// Implémentation des méthodes...

end.
```

## Conseils pratiques pour débutants

1. **Commencez petit** : Appliquez d'abord les patterns sur une petite partie de votre application
2. **Évitez la sur-ingénierie** : N'utilisez pas de patterns complexes pour des problèmes simples
3. **Apprenez par l'exemple** : Étudiez des projets open source qui utilisent ces patterns
4. **Refactorisez progressivement** : Transformez votre code existant par étapes
5. **Testez régulièrement** : Vérifiez que votre application fonctionne après chaque modification

## Erreurs courantes à éviter

1. **Vues trop intelligentes** : Évitez de mettre de la logique métier dans les formulaires
2. **Modèles trop liés à l'UI** : Le modèle ne doit pas connaître l'interface utilisateur
3. **Couches trop dépendantes** : Limitez les dépendances entre les couches
4. **Sur-complexité** : N'ajoutez pas de couches inutiles pour un projet simple

---

> **Astuce pour débutants** : Les patterns d'architecture peuvent sembler intimidants au début, mais ils deviennent plus naturels avec la pratique. Ne vous inquiétez pas si vous ne les appliquez pas parfaitement du premier coup. Commencez par simplement séparer votre code en fichiers logiques, puis évoluez vers des patterns plus structurés à mesure que vous gagnez en confiance et en expérience. L'objectif principal est d'avoir un code plus clair et plus maintenable, non de suivre rigidement une structure théorique.

⏭️ [Séparation UI / logique métier](18-architecture-et-bonnes-pratiques/03-separation-ui-logique-metier.md)
