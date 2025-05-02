# 6.2 Communication entre formulaires

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

Une application Delphi utilisant plusieurs formulaires nécessite généralement que ces formulaires communiquent entre eux. Cette section vous explique comment établir efficacement cette communication.

## Pourquoi faire communiquer des formulaires ?

Dans une application typique, vous aurez souvent besoin de :
- Transmettre des données d'un formulaire à un autre
- Notifier un formulaire qu'une action s'est produite dans un autre
- Synchroniser l'affichage entre plusieurs fenêtres
- Partager des ressources ou des états

## Méthodes de communication entre formulaires

### 1. Références directes entre formulaires

La méthode la plus simple pour permettre à un formulaire d'accéder à un autre est d'utiliser une référence directe.

#### Accès au formulaire principal

Toutes les applications Delphi ont une référence globale au formulaire principal via `Application.MainForm` :

```pascal
// Depuis n'importe quel formulaire, accéder au formulaire principal
var
  MainForm: TMainForm;
begin
  MainForm := TMainForm(Application.MainForm);
  MainForm.StatusBar1.SimpleText := 'Message du formulaire enfant';
end;
```

Pour que ce code fonctionne, vous devez inclure l'unité du formulaire principal dans la clause `uses` de votre formulaire secondaire.

#### Accès aux formulaires secondaires

Pour un formulaire principal qui doit accéder à un formulaire secondaire déjà créé :

```pascal
// Dans le formulaire principal
var
  ClientForm: TClientForm;
begin
  // Si le formulaire est déjà créé et stocké dans une variable de classe
  if Assigned(FClientForm) then
  begin
    FClientForm.RefreshData;
    FClientForm.Show;
  end;
end;
```

### 2. Passage de paramètres lors de la création

Une approche efficace consiste à passer des informations lors de la création du formulaire.

#### Constructeur personnalisé

Créez un constructeur personnalisé dans votre formulaire enfant :

```pascal
// Dans l'unité du formulaire client (FormClient.pas)
unit FormClient;

interface

type
  TClientForm = class(TForm)
    // ...
  private
    FClientID: Integer;
    procedure LoadClientData;
  public
    constructor Create(AOwner: TComponent; AClientID: Integer); reintroduce;
  end;

implementation

constructor TClientForm.Create(AOwner: TComponent; AClientID: Integer);
begin
  inherited Create(AOwner);  // Appelle le constructeur parent
  FClientID := AClientID;    // Stocke l'ID du client
  LoadClientData;            // Charge les données
end;

procedure TClientForm.LoadClientData;
begin
  // Utilise FClientID pour charger les données du client
  // ...
end;
```

Puis utilisez ce constructeur dans le formulaire principal :

```pascal
// Dans le formulaire principal
procedure TMainForm.btnViewClientClick(Sender: TObject);
var
  ClientForm: TClientForm;
  ClientID: Integer;
begin
  ClientID := GetSelectedClientID; // Obtenir l'ID du client sélectionné

  ClientForm := TClientForm.Create(Self, ClientID);
  try
    ClientForm.ShowModal;
  finally
    ClientForm.Free;
  end;
end;
```

### 3. Propriétés et méthodes publiques

Vous pouvez créer des propriétés et méthodes publiques dans vos formulaires pour faciliter les échanges.

```pascal
// Dans l'unité du formulaire de détail (FormDetail.pas)
unit FormDetail;

interface

type
  TDetailForm = class(TForm)
    // ...
  private
    FProductID: Integer;
    FProductName: string;
    FProductPrice: Double;
    procedure UpdateDisplay;
  public
    // Propriétés publiques
    property ProductID: Integer read FProductID write FProductID;
    property ProductName: string read FProductName write FProductName;
    property ProductPrice: Double read FProductPrice write FProductPrice;

    // Méthode publique
    procedure LoadProduct(AID: Integer);
  end;

implementation

procedure TDetailForm.LoadProduct(AID: Integer);
begin
  FProductID := AID;
  // Charger les données depuis une base de données ou autre source
  // ...
  UpdateDisplay;
end;

procedure TDetailForm.UpdateDisplay;
begin
  // Mise à jour des contrôles visuels
  edtName.Text := FProductName;
  edtPrice.Text := FormatFloat('#,##0.00', FProductPrice);
end;
```

Puis utilisez ces propriétés depuis le formulaire principal :

```pascal
procedure TMainForm.btnEditProductClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(Self);
  try
    // Définir les propriétés avant d'afficher
    DetailForm.ProductID := 101;
    DetailForm.ProductName := 'Écran HD';
    DetailForm.ProductPrice := 299.99;

    // Ou utiliser la méthode de chargement
    // DetailForm.LoadProduct(101);

    if DetailForm.ShowModal = mrOk then
    begin
      // Récupérer les valeurs modifiées
      UpdateProduct(
        DetailForm.ProductID,
        DetailForm.ProductName,
        DetailForm.ProductPrice
      );
    end;
  finally
    DetailForm.Free;
  end;
end;
```

### 4. Utilisation de la modalité et ModalResult

Pour les formulaires modaux (affichés avec `ShowModal`), le résultat de dialogue (`ModalResult`) est un excellent moyen de communiquer une décision.

```pascal
// Dans le formulaire secondaire (un dialogue)
procedure TDialogForm.btnOkClick(Sender: TObject);
begin
  // Valider les données avant de fermer
  if ValidateData then
    ModalResult := mrOk
  else
    ShowMessage('Veuillez corriger les erreurs');
end;

procedure TDialogForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
```

Puis dans le formulaire principal :

```pascal
procedure TMainForm.btnShowDialogClick(Sender: TObject);
var
  DialogForm: TDialogForm;
begin
  DialogForm := TDialogForm.Create(Self);
  try
    // Configuration du dialogue
    DialogForm.Caption := 'Confirmation';
    DialogForm.lblMessage.Caption := 'Voulez-vous continuer ?';

    // Affiche le dialogue et attend la réponse
    if DialogForm.ShowModal = mrOk then
    begin
      // Code exécuté si l'utilisateur a cliqué sur OK
      ProcessConfirmation;
    end
    else
    begin
      // Code exécuté si l'utilisateur a annulé
      CancelOperation;
    end;
  finally
    DialogForm.Free;
  end;
end;
```

Delphi définit plusieurs constantes `mrXXX` pour les résultats courants :
- `mrNone` (0) : Valeur par défaut, pas de décision
- `mrOk` (1) : L'utilisateur a confirmé (OK)
- `mrCancel` (2) : L'utilisateur a annulé
- `mrAbort` (3) : Abandon
- `mrRetry` (4) : Réessayer
- `mrIgnore` (5) : Ignorer
- `mrYes` (6) : Oui
- `mrNo` (7) : Non
- `mrAll` (8) : Tout
- `mrNoToAll` (9) : Non à tout
- `mrYesToAll` (10) : Oui à tout

Vous pouvez aussi définir vos propres valeurs de résultat (supérieures à 10).

### 5. Module de données (DataModule)

Pour les applications orientées données, un `TDataModule` est idéal pour centraliser l'accès aux données et faciliter la communication entre formulaires.

```pascal
// Dans un fichier DataModule.pas
unit DataModule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MySQL,
  FireDAC.Phys.MySQLDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TDM = class(TDataModule)
    Connection: TFDConnection;
    qryClients: TFDQuery;
    qryProducts: TFDQuery;
    // Composants d'accès aux données
    procedure DataModuleCreate(Sender: TObject);
  private
    FCurrentUserID: Integer;
  public
    // Méthodes d'accès aux données
    function GetClientName(ClientID: Integer): string;
    procedure SaveProduct(ProductID: Integer; const Name: string; Price: Double);

    // Propriétés
    property CurrentUserID: Integer read FCurrentUserID write FCurrentUserID;
  end;

var
  DM: TDM; // Variable globale accessible depuis toutes les unités

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  // Initialisation
  Connection.Connected := True;
end;

function TDM.GetClientName(ClientID: Integer): string;
begin
  qryClients.Close;
  qryClients.SQL.Text := 'SELECT name FROM clients WHERE id = :id';
  qryClients.ParamByName('id').AsInteger := ClientID;
  qryClients.Open;

  if qryClients.RecordCount > 0 then
    Result := qryClients.FieldByName('name').AsString
  else
    Result := '';
end;

procedure TDM.SaveProduct(ProductID: Integer; const Name: string; Price: Double);
begin
  // Code pour sauvegarder un produit dans la base de données
end;

end.
```

Pour utiliser ce module de données depuis n'importe quel formulaire :

```pascal
// Assurez-vous d'inclure DataModule dans la clause uses
uses DataModule;

procedure TProductForm.btnSaveClick(Sender: TObject);
begin
  // Accès au module de données global
  DM.SaveProduct(
    StrToIntDef(edtID.Text, 0),
    edtName.Text,
    StrToFloatDef(edtPrice.Text, 0)
  );

  ModalResult := mrOk;
end;
```

### 6. Événements et callbacks

Une approche élégante consiste à utiliser des événements personnalisés pour notifier un formulaire des changements dans un autre.

Dans le formulaire enfant, déclarez un type de callback et une propriété d'événement :

```pascal
// Dans le formulaire enfant (FormDetail.pas)
unit FormDetail;

interface

type
  // Définition du type d'événement
  TOnProductSaved = procedure(ProductID: Integer; const Name: string; Price: Double) of object;

  TDetailForm = class(TForm)
    // ...
  private
    FProductID: Integer;
    FProductName: string;
    FProductPrice: Double;
    FOnProductSaved: TOnProductSaved;
  public
    // Événement qui sera déclenché quand un produit est sauvegardé
    property OnProductSaved: TOnProductSaved read FOnProductSaved write FOnProductSaved;
  end;

implementation

procedure TDetailForm.btnSaveClick(Sender: TObject);
begin
  // Récupère les valeurs des contrôles
  FProductName := edtName.Text;
  FProductPrice := StrToFloatDef(edtPrice.Text, 0);

  // Déclenche l'événement si assigné
  if Assigned(FOnProductSaved) then
    FOnProductSaved(FProductID, FProductName, FProductPrice);

  ModalResult := mrOk;
end;
```

Dans le formulaire principal, assignez un gestionnaire à cet événement :

```pascal
procedure TMainForm.btnEditProductClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(Self);
  try
    // Configurer le formulaire
    DetailForm.ProductID := 101;
    DetailForm.ProductName := 'Écran HD';
    DetailForm.ProductPrice := 299.99;

    // Assigner le gestionnaire d'événement
    DetailForm.OnProductSaved := HandleProductSaved;

    DetailForm.ShowModal;
  finally
    DetailForm.Free;
  end;
end;

// Gestionnaire d'événement
procedure TMainForm.HandleProductSaved(ProductID: Integer; const Name: string; Price: Double);
begin
  // Cette méthode sera appelée quand le produit est sauvegardé
  UpdateProductInList(ProductID, Name, Price);
  ShowMessage('Produit sauvegardé avec succès !');
end;
```

### 7. Interfaces pour un couplage faible

Pour les applications complexes, utilisez des interfaces pour établir une communication sans créer de dépendances circulaires.

```pascal
// Dans une unité séparée (Interfaces.pas)
unit Interfaces;

interface

type
  IProductManager = interface
    ['{A1B2C3D4-E5F6-G7H8-I9J0-K1L2M3N4O5P6}'] // GUID unique
    procedure UpdateProduct(ID: Integer; const Name: string; Price: Double);
    function GetProductCount: Integer;
  end;

implementation

end.
```

Dans le formulaire principal, implémentez cette interface :

```pascal
unit MainForm;

interface

uses
  Vcl.Forms, Interfaces;

type
  TMainForm = class(TForm, IProductManager)
    // ...
  private
    // Implémentation de l'interface
    procedure UpdateProduct(ID: Integer; const Name: string; Price: Double);
    function GetProductCount: Integer;
  public
    // ...
  end;

implementation

procedure TMainForm.UpdateProduct(ID: Integer; const Name: string; Price: Double);
begin
  // Mise à jour de l'interface utilisateur
end;

function TMainForm.GetProductCount: Integer;
begin
  Result := ListView1.Items.Count;
end;
```

Dans le formulaire enfant, utilisez l'interface sans créer de dépendance directe :

```pascal
unit DetailForm;

interface

uses
  Vcl.Forms, Interfaces;

type
  TDetailForm = class(TForm)
    // ...
  private
    FProductManager: IProductManager;
  public
    constructor Create(AOwner: TComponent; AProductManager: IProductManager); reintroduce;
  end;

implementation

constructor TDetailForm.Create(AOwner: TComponent; AProductManager: IProductManager);
begin
  inherited Create(AOwner);
  FProductManager := AProductManager;
end;

procedure TDetailForm.btnSaveClick(Sender: TObject);
begin
  if Assigned(FProductManager) then
    FProductManager.UpdateProduct(
      StrToIntDef(edtID.Text, 0),
      edtName.Text,
      StrToFloatDef(edtPrice.Text, 0)
    );

  ModalResult := mrOk;
end;
```

Utilisation :

```pascal
procedure TMainForm.btnEditClick(Sender: TObject);
var
  DetailForm: TDetailForm;
begin
  DetailForm := TDetailForm.Create(Self, Self as IProductManager);
  try
    DetailForm.ShowModal;
  finally
    DetailForm.Free;
  end;
end;
```

## Exemple complet : Application de gestion de contacts

Voici un exemple plus complet montrant comment utiliser plusieurs de ces techniques dans une application de gestion de contacts.

### Forme principale (MainForm.pas)

```pascal
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons;

type
  TContact = record
    ID: Integer;
    FirstName: string;
    LastName: string;
    Email: string;
    Phone: string;
  end;

  TMainForm = class(TForm)
    ListView1: TListView;
    Panel1: TPanel;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    FContacts: TArray<TContact>;
    procedure AddContactToList(const Contact: TContact);
    procedure UpdateContactInList(const Contact: TContact);
    function GetSelectedContact: TContact;
    procedure HandleContactSaved(const Contact: TContact);
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses ContactForm; // Unité du formulaire de contact

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Configuration de la ListView
  with ListView1 do
  begin
    ViewStyle := vsReport;

    Columns.Add.Caption := 'ID';
    Columns.Add.Caption := 'Prénom';
    Columns.Add.Caption := 'Nom';
    Columns.Add.Caption := 'Email';
    Columns.Add.Caption := 'Téléphone';

    Columns[0].Width := 50;
    Columns[1].Width := 120;
    Columns[2].Width := 120;
    Columns[3].Width := 180;
    Columns[4].Width := 120;
  end;

  // Exemples de contacts
  SetLength(FContacts, 2);

  FContacts[0].ID := 1;
  FContacts[0].FirstName := 'Jean';
  FContacts[0].LastName := 'Dupont';
  FContacts[0].Email := 'jean.dupont@email.com';
  FContacts[0].Phone := '01 23 45 67 89';

  FContacts[1].ID := 2;
  FContacts[1].FirstName := 'Marie';
  FContacts[1].LastName := 'Martin';
  FContacts[1].Email := 'marie.martin@email.com';
  FContacts[1].Phone := '09 87 65 43 21';

  // Affichage des contacts
  for var Contact in FContacts do
    AddContactToList(Contact);
end;

procedure TMainForm.AddContactToList(const Contact: TContact);
var
  Item: TListItem;
begin
  Item := ListView1.Items.Add;
  Item.Caption := IntToStr(Contact.ID);
  Item.SubItems.Add(Contact.FirstName);
  Item.SubItems.Add(Contact.LastName);
  Item.SubItems.Add(Contact.Email);
  Item.SubItems.Add(Contact.Phone);
  Item.Data := Pointer(Contact.ID); // Utilise Data pour stocker l'ID
end;

procedure TMainForm.UpdateContactInList(const Contact: TContact);
var
  I: Integer;
  Item: TListItem;
begin
  // Recherche l'item correspondant
  for I := 0 to ListView1.Items.Count - 1 do
  begin
    Item := ListView1.Items[I];
    if Integer(Item.Data) = Contact.ID then
    begin
      Item.Caption := IntToStr(Contact.ID);
      Item.SubItems[0] := Contact.FirstName;
      Item.SubItems[1] := Contact.LastName;
      Item.SubItems[2] := Contact.Email;
      Item.SubItems[3] := Contact.Phone;
      Break;
    end;
  end;
end;

function TMainForm.GetSelectedContact: TContact;
var
  Selected: TListItem;
  I: Integer;
  ID: Integer;
begin
  Result.ID := 0; // Indique qu'aucun contact n'est sélectionné

  Selected := ListView1.Selected;
  if Selected = nil then
    Exit;

  ID := Integer(Selected.Data);

  for I := 0 to Length(FContacts) - 1 do
    if FContacts[I].ID = ID then
    begin
      Result := FContacts[I];
      Break;
    end;
end;

procedure TMainForm.HandleContactSaved(const Contact: TContact);
var
  I, Index: Integer;
  Found: Boolean;
begin
  // Recherche si le contact existe déjà
  Found := False;
  Index := -1;

  for I := 0 to Length(FContacts) - 1 do
    if FContacts[I].ID = Contact.ID then
    begin
      Found := True;
      Index := I;
      Break;
    end;

  if Found then
  begin
    // Mettre à jour le contact existant
    FContacts[Index] := Contact;
    UpdateContactInList(Contact);
  end
  else
  begin
    // Ajouter un nouveau contact
    Index := Length(FContacts);
    SetLength(FContacts, Index + 1);
    FContacts[Index] := Contact;
    AddContactToList(Contact);
  end;
end;

procedure TMainForm.btnAddClick(Sender: TObject);
var
  ContactForm: TContactForm;
  NewContact: TContact;
begin
  ContactForm := TContactForm.Create(Self);
  try
    // Initialiser un nouveau contact
    NewContact.ID := Length(FContacts) + 1; // ID automatique simple
    NewContact.FirstName := '';
    NewContact.LastName := '';
    NewContact.Email := '';
    NewContact.Phone := '';

    ContactForm.SetContact(NewContact);
    ContactForm.OnContactSaved := HandleContactSaved;

    ContactForm.ShowModal;
  finally
    ContactForm.Free;
  end;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
var
  ContactForm: TContactForm;
  Contact: TContact;
begin
  Contact := GetSelectedContact;
  if Contact.ID = 0 then
  begin
    ShowMessage('Veuillez sélectionner un contact à modifier');
    Exit;
  end;

  ContactForm := TContactForm.Create(Self);
  try
    ContactForm.SetContact(Contact);
    ContactForm.OnContactSaved := HandleContactSaved;

    ContactForm.ShowModal;
  finally
    ContactForm.Free;
  end;
end;

procedure TMainForm.ListView1DblClick(Sender: TObject);
begin
  btnEditClick(Sender);
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
var
  Contact: TContact;
  I, J, Index: Integer;
begin
  Contact := GetSelectedContact;
  if Contact.ID = 0 then
  begin
    ShowMessage('Veuillez sélectionner un contact à supprimer');
    Exit;
  end;

  if MessageDlg('Voulez-vous vraiment supprimer ce contact ?',
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Rechercher l'index du contact
  Index := -1;
  for I := 0 to Length(FContacts) - 1 do
    if FContacts[I].ID = Contact.ID then
    begin
      Index := I;
      Break;
    end;

  if Index >= 0 then
  begin
    // Supprimer le contact du tableau
    for I := Index to Length(FContacts) - 2 do
      FContacts[I] := FContacts[I + 1];

    SetLength(FContacts, Length(FContacts) - 1);

    // Supprimer de la ListView
    for I := 0 to ListView1.Items.Count - 1 do
      if Integer(ListView1.Items[I].Data) = Contact.ID then
      begin
        ListView1.Items.Delete(I);
        Break;
      end;
  end;
end;

end.
```

### Formulaire de contact (ContactForm.pas)

```pascal
unit ContactForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TContact = record
    ID: Integer;
    FirstName: string;
    LastName: string;
    Email: string;
    Phone: string;
  end;

  TOnContactSaved = procedure(const Contact: TContact) of object;

  TContactForm = class(TForm)
    edtFirstName: TEdit;
    edtLastName: TEdit;
    edtEmail: TEdit;
    edtPhone: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    btnSave: TButton;
    btnCancel: TButton;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FContact: TContact;
    FOnContactSaved: TOnContactSaved;
    procedure ValidateInput;
  public
    procedure SetContact(const AContact: TContact);
    property OnContactSaved: TOnContactSaved read FOnContactSaved write FOnContactSaved;
  end;

implementation

{$R *.dfm}

procedure TContactForm.FormCreate(Sender: TObject);
begin
  Position := poOwnerFormCenter;
end;

procedure TContactForm.SetContact(const AContact: TContact);
begin
  FContact := AContact;

  // Mettre à jour les contrôles
  edtFirstName.Text := FContact.FirstName;
  edtLastName.Text := FContact.LastName;
  edtEmail.Text := FContact.Email;
  edtPhone.Text := FContact.Phone;

  // Titre du formulaire
  if FContact.ID = 0 then
    Caption := 'Nouveau contact'
  else
    Caption := 'Modifier le contact: ' + FContact.FirstName + ' ' + FContact.LastName;
end;

procedure TContactForm.ValidateInput;
begin
  if Trim(edtFirstName.Text) = '' then
    raise Exception.Create('Le prénom est obligatoire');

  if Trim(edtLastName.Text) = '' then
    raise Exception.Create('Le nom est obligatoire');

  if Trim(edtEmail.Text) = '' then
    raise Exception.Create('L''email est obligatoire');

  // Validation simple d'email
  if not (Pos('@', edtEmail.Text) > 0) or not (Pos('.', edtEmail.Text) > 0) then
    raise Exception.Create('Format d''email invalide');
end;

procedure TContactForm.btnSaveClick(Sender: TObject);
begin
  try
    ValidateInput;

    // Mettre à jour le contact
    FContact.FirstName := Trim(edtFirstName.Text);
    FContact.LastName := Trim(edtLastName.Text);
    FContact.Email := Trim(edtEmail.Text);
    FContact.Phone := Trim(edtPhone.Text);

    // Notifier le formulaire parent
    if Assigned(FOnContactSaved) then
      FOnContactSaved(FContact);

    ModalResult := mrOk;
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

end.
```

## Points importants à retenir

1. **Choisissez la méthode appropriée** : Utilisez la technique qui convient le mieux à votre application
   - Pour les petites applications : références directes ou propriétés publiques
   - Pour les applications de taille moyenne : événements ou module de données
   - Pour les grandes applications : interfaces ou architecture en couches

2. **Évitez les références circulaires** : Les unités A et B ne doivent pas s'inclure mutuellement dans leur section `interface`

3. **Libérez correctement la mémoire** : Utilisez toujours des blocs `try...finally` lors de la création de formulaires

4. **Centralisez la logique métier** : Évitez de dupliquer le code métier dans plusieurs formulaires

5. **Documentez l'API de vos formulaires** : Commentez clairement les propriétés et méthodes publiques pour faciliter leur utilisation

## Exercices pratiques

1. **Exercice simple** : Créez une application avec un formulaire principal et un formulaire de saisie de texte qui renvoie la valeur saisie au formulaire principal

2. **Exercice intermédiaire** : Créez une application de gestion d'inventaire avec un formulaire principal (liste des produits) et un formulaire de détail (modification des produits)

3. **Exercice avancé** : Développez une application avec trois formulaires (clients, commandes, produits) qui communiquent entre eux via un module de données partagé

---

En maîtrisant ces techniques de communication entre formulaires, vous pourrez développer des applications Delphi plus sophistiquées et modulaires, tout en gardant un code propre et facilement maintenable.

⏭️ [Formulaires MDI (Multiple Document Interface)](/06-applications-multi-fenetres-et-navigation/03-formulaires-mdi.md)
