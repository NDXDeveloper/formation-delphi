# 11.9 Programmation réactive avec le pattern Observer

## Introduction

La programmation réactive est un paradigme axé sur les flux de données et la propagation des changements. Elle permet de construire des applications qui réagissent automatiquement aux modifications de données, d'état ou d'événements. L'un des modèles de conception (design pattern) fondamentaux pour implémenter la programmation réactive est le pattern Observer.

Dans ce chapitre, nous allons explorer le pattern Observer en Delphi et voir comment il peut être utilisé pour créer des applications réactives. Ce pattern est particulièrement utile en complément des techniques de multithreading que nous avons étudiées précédemment.

## Le pattern Observer : principe de base

Le pattern Observer définit une relation un-à-plusieurs entre objets, de sorte que lorsqu'un objet (le sujet ou observable) change d'état, tous ses observateurs sont notifiés et mis à jour automatiquement.

Le pattern Observer implique deux types d'acteurs principaux :

1. **Observable** (ou Sujet) : L'objet qui contient les données à observer et qui notifie ses observateurs des changements.
2. **Observer** (ou Observateur) : Les objets qui "écoutent" l'observable et réagissent aux changements.

## Implémentation classique en Delphi

Voici une implémentation basique du pattern Observer en Delphi :

```pascal
type
  // Interface pour les observateurs
  IObserver = interface
    ['{A1B2C3D4-E5F6-4789-ABCD-EF1234567890}']
    procedure Update(const Message: string);
  end;

  // Classe observable
  TObservable = class
  private
    FObservers: TList<IObserver>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver(Observer: IObserver);
    procedure RemoveObserver(Observer: IObserver);
    procedure NotifyObservers(const Message: string);
  end;

// Implémentation
constructor TObservable.Create;
begin
  inherited;
  FObservers := TList<IObserver>.Create;
end;

destructor TObservable.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TObservable.AddObserver(Observer: IObserver);
begin
  if not FObservers.Contains(Observer) then
    FObservers.Add(Observer);
end;

procedure TObservable.RemoveObserver(Observer: IObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TObservable.NotifyObservers(const Message: string);
var
  Observer: IObserver;
begin
  for Observer in FObservers do
    Observer.Update(Message);
end;
```

## Exemple simple : système de notification

Voici un exemple simple de comment utiliser le pattern Observer pour créer un système de notification :

```pascal
// Classe concrète d'observateur
type
  TConsoleObserver = class(TInterfacedObject, IObserver)
  private
    FName: string;
  public
    constructor Create(const Name: string);
    procedure Update(const Message: string);
  end;

constructor TConsoleObserver.Create(const Name: string);
begin
  inherited Create;
  FName := Name;
end;

procedure TConsoleObserver.Update(const Message: string);
begin
  Writeln(Format('Observateur %s a reçu : %s', [FName, Message]));
end;

// Utilisation
procedure DemoObserver;
var
  Observable: TObservable;
  Observer1, Observer2: TConsoleObserver;
begin
  Observable := TObservable.Create;
  try
    Observer1 := TConsoleObserver.Create('Observateur 1');
    Observer2 := TConsoleObserver.Create('Observateur 2');

    Observable.AddObserver(Observer1);
    Observable.AddObserver(Observer2);

    // Envoyer des notifications
    Observable.NotifyObservers('Premier message');
    Observable.NotifyObservers('Deuxième message');

    // Supprimer un observateur
    Observable.RemoveObserver(Observer1);

    // Envoyer une autre notification
    Observable.NotifyObservers('Troisième message');
  finally
    Observable.Free;
    Observer1.Free;
    Observer2.Free;
  end;
end;
```

## Observer avec des données typées

Dans la pratique, vous voudrez souvent transmettre des informations plus spécifiques que de simples chaînes de caractères. Voici comment adapter le pattern Observer pour travailler avec des types de données spécifiques :

```pascal
type
  // Type de données à observer
  TStockInfo = record
    Symbol: string;
    Price: Currency;
    Change: Currency;
  end;

  // Interface d'observateur typée
  IStockObserver = interface
    ['{B1C2D3E4-F5G6-7890-ABCD-EF1234567890}']
    procedure StockUpdated(const Stock: TStockInfo);
  end;

  // Classe observable typée
  TStockMarket = class
  private
    FObservers: TList<IStockObserver>;
    FStocks: TDictionary<string, TStockInfo>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver(Observer: IStockObserver);
    procedure RemoveObserver(Observer: IStockObserver);
    procedure UpdateStock(const Symbol: string; Price: Currency);
  end;

constructor TStockMarket.Create;
begin
  inherited;
  FObservers := TList<IStockObserver>.Create;
  FStocks := TDictionary<string, TStockInfo>.Create;
end;

destructor TStockMarket.Destroy;
begin
  FObservers.Free;
  FStocks.Free;
  inherited;
end;

procedure TStockMarket.AddObserver(Observer: IStockObserver);
begin
  if not FObservers.Contains(Observer) then
    FObservers.Add(Observer);
end;

procedure TStockMarket.RemoveObserver(Observer: IStockObserver);
begin
  FObservers.Remove(Observer);
end;

procedure TStockMarket.UpdateStock(const Symbol: string; Price: Currency);
var
  Stock: TStockInfo;
  OldPrice: Currency;
  Observer: IStockObserver;
begin
  // Vérifier si le stock existe déjà
  if FStocks.TryGetValue(Symbol, Stock) then
    OldPrice := Stock.Price
  else
  begin
    OldPrice := 0;
    Stock.Symbol := Symbol;
  end;

  // Mettre à jour le prix et calculer le changement
  Stock.Price := Price;
  Stock.Change := Price - OldPrice;

  // Stocker la nouvelle information
  FStocks.AddOrSetValue(Symbol, Stock);

  // Notifier tous les observateurs
  for Observer in FObservers do
    Observer.StockUpdated(Stock);
end;
```

## Utilisation du pattern Observer avec les interfaces graphiques

Le pattern Observer est particulièrement utile pour les interfaces graphiques, où plusieurs composants peuvent avoir besoin d'être mis à jour en réponse à des changements de données. Voici un exemple concret avec une application de suivi boursier :

```pascal
type
  TStockObserverForm = class(TForm, IStockObserver)
  private
    FStockMarket: TStockMarket;
    FSymbol: string;
    FPriceLabel: TLabel;
    FChangeLabel: TLabel;
    FHistoryMemo: TMemo;
  public
    constructor Create(AOwner: TComponent; StockMarket: TStockMarket; const Symbol: string); reintroduce;
    destructor Destroy; override;
    // Implémentation de IStockObserver
    procedure StockUpdated(const Stock: TStockInfo);
  end;

constructor TStockObserverForm.Create(AOwner: TComponent; StockMarket: TStockMarket; const Symbol: string);
begin
  inherited Create(AOwner);

  FStockMarket := StockMarket;
  FSymbol := Symbol;

  // Configuration de l'interface
  Caption := 'Observateur - ' + Symbol;
  ClientWidth := 300;
  ClientHeight := 200;

  FPriceLabel := TLabel.Create(Self);
  FPriceLabel.Parent := Self;
  FPriceLabel.Left := 10;
  FPriceLabel.Top := 10;
  FPriceLabel.AutoSize := True;
  FPriceLabel.Caption := 'Prix: -';

  FChangeLabel := TLabel.Create(Self);
  FChangeLabel.Parent := Self;
  FChangeLabel.Left := 10;
  FChangeLabel.Top := 30;
  FChangeLabel.AutoSize := True;
  FChangeLabel.Caption := 'Variation: -';

  FHistoryMemo := TMemo.Create(Self);
  FHistoryMemo.Parent := Self;
  FHistoryMemo.Left := 10;
  FHistoryMemo.Top := 50;
  FHistoryMemo.Width := 280;
  FHistoryMemo.Height := 140;
  FHistoryMemo.ReadOnly := True;
  FHistoryMemo.ScrollBars := ssBoth;

  // S'abonner aux mises à jour
  FStockMarket.AddObserver(Self as IStockObserver);
end;

destructor TStockObserverForm.Destroy;
begin
  // Se désabonner des mises à jour
  if Assigned(FStockMarket) then
    FStockMarket.RemoveObserver(Self as IStockObserver);

  inherited;
end;

procedure TStockObserverForm.StockUpdated(const Stock: TStockInfo);
begin
  // Vérifier si c'est le symbole que nous suivons
  if Stock.Symbol <> FSymbol then
    Exit;

  // Mettre à jour l'interface utilisateur
  FPriceLabel.Caption := Format('Prix: %.2m', [Stock.Price]);

  // Configurer la couleur en fonction de la variation
  if Stock.Change > 0 then
  begin
    FChangeLabel.Font.Color := clGreen;
    FChangeLabel.Caption := Format('Variation: +%.2m', [Stock.Change]);
  end
  else if Stock.Change < 0 then
  begin
    FChangeLabel.Font.Color := clRed;
    FChangeLabel.Caption := Format('Variation: %.2m', [Stock.Change]);
  end
  else
  begin
    FChangeLabel.Font.Color := clWindowText;
    FChangeLabel.Caption := 'Variation: 0.00';
  end;

  // Ajouter à l'historique
  FHistoryMemo.Lines.Add(Format('[%s] Prix: %.2m, Variation: %s%.2m',
    [DateTimeToStr(Now), Stock.Price,
     IfThen(Stock.Change >= 0, '+', ''), Stock.Change]));
end;
```

Pour utiliser cette classe d'observateur, vous pouvez créer une application avec plusieurs fenêtres observant différentes actions :

```pascal
procedure TMainForm.ButtonCreateObserverClick(Sender: TObject);
var
  Symbol: string;
  ObserverForm: TStockObserverForm;
begin
  Symbol := EditSymbol.Text;
  if Symbol = '' then
  begin
    ShowMessage('Veuillez entrer un symbole boursier');
    Exit;
  end;

  // Créer une nouvelle fenêtre d'observateur
  ObserverForm := TStockObserverForm.Create(Application, FStockMarket, Symbol);
  ObserverForm.Show;
end;

procedure TMainForm.ButtonUpdateClick(Sender: TObject);
var
  Symbol: string;
  Price: Currency;
begin
  Symbol := EditSymbol.Text;
  if Symbol = '' then
  begin
    ShowMessage('Veuillez entrer un symbole boursier');
    Exit;
  end;

  if not TryStrToCurr(EditPrice.Text, Price) then
  begin
    ShowMessage('Prix invalide');
    Exit;
  end;

  // Mettre à jour le prix de l'action
  FStockMarket.UpdateStock(Symbol, Price);
end;
```

## Observer pattern thread-safe

Dans une application multi-threads, il est important de s'assurer que le pattern Observer est thread-safe. Voici une implémentation thread-safe du pattern Observer :

```pascal
type
  // Interface d'observateur thread-safe
  IThreadSafeObserver = interface
    ['{C1D2E3F4-G5H6-8901-ABCD-EF1234567890}']
    procedure Update(const Data: TObject);
  end;

  // Classe observable thread-safe
  TThreadSafeObservable = class
  private
    FObservers: TList<IThreadSafeObserver>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver(Observer: IThreadSafeObserver);
    procedure RemoveObserver(Observer: IThreadSafeObserver);
    procedure NotifyObservers(const Data: TObject);
  end;

constructor TThreadSafeObservable.Create;
begin
  inherited;
  FObservers := TList<IThreadSafeObserver>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeObservable.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TThreadSafeObservable.AddObserver(Observer: IThreadSafeObserver);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeObservable.RemoveObserver(Observer: IThreadSafeObserver);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TThreadSafeObservable.NotifyObservers(const Data: TObject);
var
  ObserversCopy: TArray<IThreadSafeObserver>;
  Observer: IThreadSafeObserver;
begin
  // Créer une copie de la liste des observateurs pour éviter les problèmes
  // si la liste est modifiée pendant la notification
  FLock.Enter;
  try
    ObserversCopy := FObservers.ToArray;
  finally
    FLock.Leave;
  end;

  // Notifier tous les observateurs à partir de la copie
  for Observer in ObserversCopy do
  begin
    try
      Observer.Update(Data);
    except
      // Gérer les exceptions des observateurs pour éviter qu'une erreur
      // dans un observateur n'empêche la notification des autres
    end;
  end;
end;
```

## Notification asynchrone

Pour améliorer davantage la réactivité de votre application, vous pouvez implémenter une notification asynchrone où les observateurs sont notifiés dans un thread séparé :

```pascal
type
  TNotificationType = (ntSynchronous, ntAsynchronous, ntThreadPool);

  TAsyncObservable = class
  private
    FObservers: TList<IThreadSafeObserver>;
    FLock: TCriticalSection;
    FNotificationType: TNotificationType;
  public
    constructor Create(NotificationType: TNotificationType = ntAsynchronous);
    destructor Destroy; override;
    procedure AddObserver(Observer: IThreadSafeObserver);
    procedure RemoveObserver(Observer: IThreadSafeObserver);
    procedure NotifyObservers(const Data: TObject);
  end;

constructor TAsyncObservable.Create(NotificationType: TNotificationType = ntAsynchronous);
begin
  inherited Create;
  FObservers := TList<IThreadSafeObserver>.Create;
  FLock := TCriticalSection.Create;
  FNotificationType := NotificationType;
end;

destructor TAsyncObservable.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TAsyncObservable.AddObserver(Observer: IThreadSafeObserver);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncObservable.RemoveObserver(Observer: IThreadSafeObserver);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TAsyncObservable.NotifyObservers(const Data: TObject);
var
  ObserversCopy: TArray<IThreadSafeObserver>;
begin
  // Créer une copie de la liste des observateurs
  FLock.Enter;
  try
    ObserversCopy := FObservers.ToArray;
  finally
    FLock.Leave;
  end;

  case FNotificationType of
    ntSynchronous:
      begin
        // Notification synchrone (comme avant)
        for var Observer in ObserversCopy do
          Observer.Update(Data);
      end;

    ntAsynchronous:
      begin
        // Notification asynchrone (un thread par notification)
        TThread.CreateAnonymousThread(
          procedure
          begin
            for var Observer in ObserversCopy do
              Observer.Update(Data);
          end
        ).Start;
      end;

    ntThreadPool:
      begin
        // Utiliser TTask pour faire la notification via le pool de threads
        TTask.Run(
          procedure
          begin
            for var Observer in ObserversCopy do
              Observer.Update(Data);
          end
        );
      end;
  end;
end;
```

## Observer avec filtrage

Dans les applications réelles, les observateurs peuvent être intéressés uniquement par certains types de modifications. Voici comment implémenter un pattern Observer avec filtrage :

```pascal
type
  TNotificationKind = (nkCreated, nkUpdated, nkDeleted);

  IFilteredObserver = interface
    ['{D1E2F3G4-H5I6-9012-ABCD-EF1234567890}']
    procedure Update(Kind: TNotificationKind; const Data: TObject);
    function GetInterestedIn: TArray<TNotificationKind>;
  end;

  TFilteredObservable = class
  private
    FObservers: TList<IFilteredObserver>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver(Observer: IFilteredObserver);
    procedure RemoveObserver(Observer: IFilteredObserver);
    procedure NotifyObservers(Kind: TNotificationKind; const Data: TObject);
  end;

constructor TFilteredObservable.Create;
begin
  inherited;
  FObservers := TList<IFilteredObserver>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TFilteredObservable.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TFilteredObservable.AddObserver(Observer: IFilteredObserver);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TFilteredObservable.RemoveObserver(Observer: IFilteredObserver);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TFilteredObservable.NotifyObservers(Kind: TNotificationKind; const Data: TObject);
var
  ObserversCopy: TArray<IFilteredObserver>;
  Observer: IFilteredObserver;
  InterestedTypes: TArray<TNotificationKind>;
begin
  // Créer une copie de la liste des observateurs
  FLock.Enter;
  try
    ObserversCopy := FObservers.ToArray;
  finally
    FLock.Leave;
  end;

  // Notifier seulement les observateurs intéressés par ce type de notification
  for Observer in ObserversCopy do
  begin
    InterestedTypes := Observer.GetInterestedIn;
    if (Length(InterestedTypes) = 0) or (Kind in TArray<TNotificationKind>(InterestedTypes)) then
      Observer.Update(Kind, Data);
  end;
end;
```

## Utilisation du pattern Observer dans un modèle MVC

Le pattern Observer est souvent utilisé dans l'architecture Model-View-Controller (MVC) pour maintenir la séparation des préoccupations tout en permettant aux vues de se mettre à jour automatiquement lorsque le modèle change.

Voici un exemple simplifié de comment implémenter MVC avec le pattern Observer :

```pascal
type
  // Modèle
  TUser = class
    Username: string;
    Email: string;
    IsActive: Boolean;
  end;

  TUserEvent = (ueCreated, ueUpdated, ueDeleted);

  IUserModelObserver = interface
    ['{E1F2G3H4-I5J6-0123-ABCD-EF1234567890}']
    procedure UserChanged(Event: TUserEvent; const User: TUser);
  end;

  TUserModel = class
  private
    FUsers: TObjectList<TUser>;
    FObservers: TList<IUserModelObserver>;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObserver(Observer: IUserModelObserver);
    procedure RemoveObserver(Observer: IUserModelObserver);

    function AddUser(const Username, Email: string): TUser;
    procedure UpdateUser(User: TUser);
    procedure DeleteUser(User: TUser);
    function FindUserByUsername(const Username: string): TUser;
  end;

  // Controleur
  TUserController = class
  private
    FModel: TUserModel;
  public
    constructor Create(Model: TUserModel);
    procedure CreateUser(const Username, Email: string);
    procedure UpdateUserEmail(const Username, NewEmail: string);
    procedure DeactivateUser(const Username: string);
  end;

  // Vue
  TUserListForm = class(TForm, IUserModelObserver)
  private
    FModel: TUserModel;
    FController: TUserController;
    ListView: TListView;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDeactivate: TButton;

    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonDeactivateClick(Sender: TObject);

    // Implémentation IUserModelObserver
    procedure UserChanged(Event: TUserEvent; const User: TUser);
  public
    constructor Create(AOwner: TComponent; Model: TUserModel); reintroduce;
    destructor Destroy; override;
  end;

// Implémentation du modèle
constructor TUserModel.Create;
begin
  inherited;
  FUsers := TObjectList<TUser>.Create(True); // True = Owns objects
  FObservers := TList<IUserModelObserver>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TUserModel.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  FUsers.Free;
  inherited;
end;

procedure TUserModel.AddObserver(Observer: IUserModelObserver);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TUserModel.RemoveObserver(Observer: IUserModelObserver);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

function TUserModel.AddUser(const Username, Email: string): TUser;
begin
  FLock.Enter;
  try
    // Vérifier si l'utilisateur existe déjà
    Result := FindUserByUsername(Username);
    if Result <> nil then
      Exit;

    // Créer un nouvel utilisateur
    Result := TUser.Create;
    Result.Username := Username;
    Result.Email := Email;
    Result.IsActive := True;

    // Ajouter à la liste
    FUsers.Add(Result);
  finally
    FLock.Leave;
  end;

  // Notifier les observateurs
  for var Observer in FObservers do
    Observer.UserChanged(ueCreated, Result);
end;

procedure TUserModel.UpdateUser(User: TUser);
begin
  // Notifier les observateurs
  for var Observer in FObservers do
    Observer.UserChanged(ueUpdated, User);
end;

procedure TUserModel.DeleteUser(User: TUser);
begin
  FLock.Enter;
  try
    // Notifier avant de supprimer (sinon l'objet n'existera plus)
    for var Observer in FObservers do
      Observer.UserChanged(ueDeleted, User);

    // Supprimer de la liste
    FUsers.Remove(User);
  finally
    FLock.Leave;
  end;
end;

function TUserModel.FindUserByUsername(const Username: string): TUser;
begin
  Result := nil;

  FLock.Enter;
  try
    for var User in FUsers do
    begin
      if User.Username = Username then
      begin
        Result := User;
        Break;
      end;
    end;
  finally
    FLock.Leave;
  end;
end;

// Implémentation du contrôleur
constructor TUserController.Create(Model: TUserModel);
begin
  inherited Create;
  FModel := Model;
end;

procedure TUserController.CreateUser(const Username, Email: string);
begin
  FModel.AddUser(Username, Email);
end;

procedure TUserController.UpdateUserEmail(const Username, NewEmail: string);
var
  User: TUser;
begin
  User := FModel.FindUserByUsername(Username);
  if User <> nil then
  begin
    User.Email := NewEmail;
    FModel.UpdateUser(User);
  end;
end;

procedure TUserController.DeactivateUser(const Username: string);
var
  User: TUser;
begin
  User := FModel.FindUserByUsername(Username);
  if User <> nil then
  begin
    User.IsActive := False;
    FModel.UpdateUser(User);
  end;
end;

// Implémentation de la vue
constructor TUserListForm.Create(AOwner: TComponent; Model: TUserModel);
begin
  inherited Create(AOwner);

  FModel := Model;
  FController := TUserController.Create(FModel);

  // S'abonner aux mises à jour du modèle
  FModel.AddObserver(Self as IUserModelObserver);

  // Configurer l'interface
  Caption := 'Liste des utilisateurs';
  Width := 500;
  Height := 400;

  ListView := TListView.Create(Self);
  ListView.Parent := Self;
  ListView.Align := alClient;
  ListView.ViewStyle := vsReport;

  // Ajouter les colonnes
  with ListView.Columns.Add do
  begin
    Caption := 'Nom d''utilisateur';
    Width := 150;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Email';
    Width := 200;
  end;

  with ListView.Columns.Add do
  begin
    Caption := 'Actif';
    Width := 50;
  end;

  // Ajouter les boutons
  ButtonAdd := TButton.Create(Self);
  ButtonAdd.Parent := Self;
  ButtonAdd.Caption := 'Ajouter';
  ButtonAdd.Left := 10;
  ButtonAdd.Top := 10;
  ButtonAdd.OnClick := ButtonAddClick;

  ButtonEdit := TButton.Create(Self);
  ButtonEdit.Parent := Self;
  ButtonEdit.Caption := 'Modifier Email';
  ButtonEdit.Left := ButtonAdd.Left + ButtonAdd.Width + 10;
  ButtonEdit.Top := 10;
  ButtonEdit.OnClick := ButtonEditClick;

  ButtonDeactivate := TButton.Create(Self);
  ButtonDeactivate.Parent := Self;
  ButtonDeactivate.Caption := 'Désactiver';
  ButtonDeactivate.Left := ButtonEdit.Left + ButtonEdit.Width + 10;
  ButtonDeactivate.Top := 10;
  ButtonDeactivate.OnClick := ButtonDeactivateClick;
end;

destructor TUserListForm.Destroy;
begin
  // Se désabonner des mises à jour du modèle
  if Assigned(FModel) then
    FModel.RemoveObserver(Self as IUserModelObserver);

  FController.Free;
  inherited;
end;

procedure TUserListForm.UserChanged(Event: TUserEvent; const User: TUser);
var
  Item: TListItem;
  i: Integer;
  Found: Boolean;
begin
  // Cette méthode est appelée quand le modèle change
  case Event of
    ueCreated:
      begin
        // Ajouter un nouvel élément à la liste
        Item := ListView.Items.Add;
        Item.Caption := User.Username;
        Item.SubItems.Add(User.Email);
        Item.SubItems.Add(IfThen(User.IsActive, 'Oui', 'Non'));
      end;

    ueUpdated:
      begin
        // Mettre à jour un élément existant
        Found := False;
        for i := 0 to ListView.Items.Count - 1 do
        begin
          if ListView.Items[i].Caption = User.Username then
          begin
            ListView.Items[i].SubItems[0] := User.Email;
            ListView.Items[i].SubItems[1] := IfThen(User.IsActive, 'Oui', 'Non');
            Found := True;
            Break;
          end;
        end;

        if not Found then
        begin
          // Si l'élément n'existe pas encore, l'ajouter
          Item := ListView.Items.Add;
          Item.Caption := User.Username;
          Item.SubItems.Add(User.Email);
          Item.SubItems.Add(IfThen(User.IsActive, 'Oui', 'Non'));
        end;
      end;

    ueDeleted:
      begin
        // Supprimer l'élément de la liste
        for i := ListView.Items.Count - 1 downto 0 do
        begin
          if ListView.Items[i].Caption = User.Username then
          begin
            ListView.Items.Delete(i);
            Break;
          end;
        end;
      end;
  end;
end;

procedure TUserListForm.ButtonAddClick(Sender: TObject);
var
  Username, Email: string;
begin
  // Demander le nom d'utilisateur et l'email
  Username := InputBox('Nouvel utilisateur', 'Nom d''utilisateur:', '');
  if Username = '' then
    Exit;

  Email := InputBox('Nouvel utilisateur', 'Email:', '');
  if Email = '' then
    Exit;

  // Demander au contrôleur de créer l'utilisateur
  FController.CreateUser(Username, Email);
end;

procedure TUserListForm.ButtonEditClick(Sender: TObject);
var
  Username, NewEmail: string;
begin
  // Vérifier qu'un élément est sélectionné
  if ListView.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un utilisateur.');
    Exit;
  end;

  // Récupérer le nom d'utilisateur
  Username := ListView.Selected.Caption;

  // Demander le nouvel email
  NewEmail := InputBox('Modifier l''email', 'Nouvel email:', ListView.Selected.SubItems[0]);
  if NewEmail = '' then
    Exit;

  // Demander au contrôleur de mettre à jour l'email
  FController.UpdateUserEmail(Username, NewEmail);
end;

procedure TUserListForm.ButtonDeactivateClick(Sender: TObject);
var
  Username: string;
begin
  // Vérifier qu'un élément est sélectionné
  if ListView.Selected = nil then
  begin
    ShowMessage('Veuillez sélectionner un utilisateur.');
    Exit;
  end;

  // Récupérer le nom d'utilisateur
  Username := ListView.Selected.Caption;

  // Demander au contrôleur de désactiver l'utilisateur
  FController.DeactivateUser(Username);
end;
```

Cet exemple montre comment le pattern Observer peut être utilisé pour implémenter une architecture MVC simple, avec une séparation claire des responsabilités :
- Le modèle (`TUserModel`) gère les données et notifie les observateurs des changements
- Le contrôleur (`TUserController`) traite les actions de l'utilisateur et met à jour le modèle
- La vue (`TUserListForm`) observe le modèle et se met à jour automatiquement lorsque le modèle change

## Observer basé sur les événements

Delphi dispose d'un mécanisme d'événements intégré qui peut être utilisé comme une forme simplifiée du pattern Observer. Voici comment implémenter un système basé sur les événements :

```pascal
type
  // Type de délégué pour l'événement
  TDataChangeEvent = procedure(Sender: TObject; const Data: string) of object;

  // Classe observable avec événement
  TEventObservable = class
  private
    FData: string;
    FOnDataChange: TDataChangeEvent;
  public
    procedure SetData(const Value: string);
    property Data: string read FData write SetData;
    property OnDataChange: TDataChangeEvent read FOnDataChange write FOnDataChange;
  end;

procedure TEventObservable.SetData(const Value: string);
begin
  if FData <> Value then
  begin
    FData := Value;

    // Déclencher l'événement
    if Assigned(FOnDataChange) then
      FOnDataChange(Self, FData);
  end;
end;
```

Utilisation :

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  FObservable := TEventObservable.Create;
  FObservable.OnDataChange := DataChanged;
end;

procedure TForm1.DataChanged(Sender: TObject; const Data: string);
begin
  Memo1.Lines.Add('Données modifiées : ' + Data);
end;

procedure TForm1.ButtonUpdateClick(Sender: TObject);
begin
  FObservable.Data := Edit1.Text;
end;
```

Bien que cette approche soit plus simple, elle a quelques limitations par rapport au pattern Observer complet :
1. Elle ne supporte qu'un seul observateur, à moins d'implémenter une liste d'événements
2. Elle est moins flexible pour les notifications complexes
3. Elle peut être plus difficile à rendre thread-safe

## Programmation réactive moderne avec TObservable<T>

Pour une approche plus moderne de la programmation réactive, nous pouvons créer une classe générique `TObservable<T>` qui encapsule une valeur et notifie les observateurs lorsque cette valeur change :

```pascal
type
  // Délégué pour les observateurs
  TValueChangedEvent<T> = procedure(Sender: TObject; const OldValue, NewValue: T) of object;

  // Classe observable générique
  TObservable<T> = class
  private
    FValue: T;
    FObservers: TList<TValueChangedEvent<T>>;
    FLock: TCriticalSection;
  public
    constructor Create(const InitialValue: T);
    destructor Destroy; override;

    // Méthodes pour s'abonner/se désabonner
    procedure Subscribe(const Observer: TValueChangedEvent<T>);
    procedure Unsubscribe(const Observer: TValueChangedEvent<T>);

    // Propriété pour accéder à la valeur
    function GetValue: T;
    procedure SetValue(const Value: T);
    property Value: T read GetValue write SetValue;
  end;

constructor TObservable<T>.Create(const InitialValue: T);
begin
  inherited Create;
  FValue := InitialValue;
  FObservers := TList<TValueChangedEvent<T>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TObservable<T>.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TObservable<T>.Subscribe(const Observer: TValueChangedEvent<T>);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TObservable<T>.Unsubscribe(const Observer: TValueChangedEvent<T>);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

function TObservable<T>.GetValue: T;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

procedure TObservable<T>.SetValue(const Value: T);
var
  OldValue: T;
  Observer: TValueChangedEvent<T>;
  ObserversCopy: TArray<TValueChangedEvent<T>>;
begin
  // Stocker l'ancienne valeur et mettre à jour
  FLock.Enter;
  try
    if not TEqualityComparer<T>.Default.Equals(FValue, Value) then
    begin
      OldValue := FValue;
      FValue := Value;

      // Créer une copie de la liste des observateurs
      ObserversCopy := FObservers.ToArray;
    end
    else
      Exit; // Aucun changement, sortir
  finally
    FLock.Leave;
  end;

  // Notifier les observateurs
  for Observer in ObserversCopy do
    Observer(Self, OldValue, Value);
end;
```

Utilisation :

```pascal
type
  TMainForm = class(TForm)
    LabelCounter: TLabel;
    EditValue: TEdit;
    ButtonUpdate: TButton;
    ButtonIncrement: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonUpdateClick(Sender: TObject);
    procedure ButtonIncrementClick(Sender: TObject);
  private
    FCounter: TObservable<Integer>;
    procedure CounterChanged(Sender: TObject; const OldValue, NewValue: Integer);
  end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCounter := TObservable<Integer>.Create(0);
  FCounter.Subscribe(CounterChanged);

  LabelCounter.Caption := 'Compteur: 0';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCounter.Free;
end;

procedure TMainForm.ButtonUpdateClick(Sender: TObject);
var
  Value: Integer;
begin
  if TryStrToInt(EditValue.Text, Value) then
    FCounter.Value := Value
  else
    ShowMessage('Valeur invalide. Veuillez entrer un nombre entier.');
end;

procedure TMainForm.ButtonIncrementClick(Sender: TObject);
begin
  FCounter.Value := FCounter.Value + 1;
end;

procedure TMainForm.CounterChanged(Sender: TObject; const OldValue, NewValue: Integer);
begin
  LabelCounter.Caption := Format('Compteur: %d (était %d)', [NewValue, OldValue]);
end;
```

## Combiner les flux d'événements

Dans la programmation réactive avancée, il est souvent utile de combiner plusieurs flux d'événements. Voici comment créer une classe qui combine deux observables :

```pascal
type
  TCombinedObservable<T1, T2, TResult> = class
  private
    FObservable1: TObservable<T1>;
    FObservable2: TObservable<T2>;
    FResult: TObservable<TResult>;
    FCombiner: TFunc<T1, T2, TResult>;

    procedure Observable1Changed(Sender: TObject; const OldValue, NewValue: T1);
    procedure Observable2Changed(Sender: TObject; const OldValue, NewValue: T2);
    procedure UpdateResult;
  public
    constructor Create(Observable1: TObservable<T1>; Observable2: TObservable<T2>;
                     Combiner: TFunc<T1, T2, TResult>);
    destructor Destroy; override;

    property Result: TObservable<TResult> read FResult;
  end;

constructor TCombinedObservable<T1, T2, TResult>.Create(Observable1: TObservable<T1>;
  Observable2: TObservable<T2>; Combiner: TFunc<T1, T2, TResult>);
begin
  inherited Create;

  FObservable1 := Observable1;
  FObservable2 := Observable2;
  FCombiner := Combiner;

  // Créer l'observable de résultat avec la valeur initiale combinée
  FResult := TObservable<TResult>.Create(FCombiner(FObservable1.Value, FObservable2.Value));

  // S'abonner aux deux observables sources
  FObservable1.Subscribe(Observable1Changed);
  FObservable2.Subscribe(Observable2Changed);
end;

destructor TCombinedObservable<T1, T2, TResult>.Destroy;
begin
  // Se désabonner des observables sources
  FObservable1.Unsubscribe(Observable1Changed);
  FObservable2.Unsubscribe(Observable2Changed);

  FResult.Free;
  inherited;
end;

procedure TCombinedObservable<T1, T2, TResult>.Observable1Changed(Sender: TObject;
  const OldValue, NewValue: T1);
begin
  UpdateResult;
end;

procedure TCombinedObservable<T1, T2, TResult>.Observable2Changed(Sender: TObject;
  const OldValue, NewValue: T2);
begin
  UpdateResult;
end;

procedure TCombinedObservable<T1, T2, TResult>.UpdateResult;
begin
  FResult.Value := FCombiner(FObservable1.Value, FObservable2.Value);
end;
```

Exemple d'utilisation :

```pascal
var
  Width, Height, Area: TObservable<Integer>;
  Combined: TCombinedObservable<Integer, Integer, Integer>;
begin
  Width := TObservable<Integer>.Create(10);
  Height := TObservable<Integer>.Create(5);

  // Combiner les deux observables pour calculer la surface
  Combined := TCombinedObservable<Integer, Integer, Integer>.Create(
    Width, Height,
    function(W, H: Integer): Integer
    begin
      Result := W * H;
    end
  );

  // S'abonner au résultat
  Combined.Result.Subscribe(
    procedure(Sender: TObject; const OldValue, NewValue: Integer)
    begin
      LabelArea.Caption := Format('Surface: %d', [NewValue]);
    end
  );

  // Changer les dimensions
  Width.Value := 20;  // Cela mettra automatiquement à jour la surface
  Height.Value := 10; // Cela mettra également à jour la surface
```

## Créer une mini bibliothèque de programmation réactive

En combinant les différentes classes et techniques présentées, nous pouvons créer une petite bibliothèque de programmation réactive pour Delphi :

```pascal
unit ReactiveLib;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults;

type
  // Observable de base
  TObservable<T> = class
  private
    FValue: T;
    FObservers: TList<TProc<T, T>>;
    FLock: TCriticalSection;
  public
    constructor Create(const InitialValue: T);
    destructor Destroy; override;

    procedure Subscribe(const Observer: TProc<T, T>);
    procedure Unsubscribe(const Observer: TProc<T, T>);

    function GetValue: T;
    procedure SetValue(const Value: T);
    property Value: T read GetValue write SetValue;
  end;

  // Opérateur Map: transforme les valeurs d'un observable
  TMapObservable<TSource, TResult> = class(TObservable<TResult>)
  private
    FSource: TObservable<TSource>;
    FMapper: TFunc<TSource, TResult>;
    procedure SourceChanged(OldValue, NewValue: TSource);
  public
    constructor Create(Source: TObservable<TSource>; Mapper: TFunc<TSource, TResult>);
    destructor Destroy; override;
  end;

  // Opérateur Filter: filtre les valeurs d'un observable
  TFilterObservable<T> = class
  private
    FSource: TObservable<T>;
    FPredicate: TPredicate<T>;
    FObservers: TList<TProc<T, T>>;
    FLock: TCriticalSection;
    procedure SourceChanged(OldValue, NewValue: T);
  public
    constructor Create(Source: TObservable<T>; Predicate: TPredicate<T>);
    destructor Destroy; override;

    procedure Subscribe(const Observer: TProc<T, T>);
    procedure Unsubscribe(const Observer: TProc<T, T>);
  end;

  // Opérateur Combine: combine deux observables
  TCombineObservable<T1, T2, TResult> = class(TObservable<TResult>)
  private
    FSource1: TObservable<T1>;
    FSource2: TObservable<T2>;
    FCombiner: TFunc<T1, T2, TResult>;
    procedure Source1Changed(OldValue, NewValue: T1);
    procedure Source2Changed(OldValue, NewValue: T2);
    procedure UpdateResult;
  public
    constructor Create(Source1: TObservable<T1>; Source2: TObservable<T2>;
                      Combiner: TFunc<T1, T2, TResult>);
    destructor Destroy; override;
  end;

  // Extensions pour faciliter l'utilisation
  TObservableExtensions = class
  public
    class function Map<TSource, TResult>(Source: TObservable<TSource>;
                                       Mapper: TFunc<TSource, TResult>): TObservable<TResult>; static;
    class function Filter<T>(Source: TObservable<T>;
                           Predicate: TPredicate<T>): TFilterObservable<T>; static;
    class function Combine<T1, T2, TResult>(Source1: TObservable<T1>; Source2: TObservable<T2>;
                                          Combiner: TFunc<T1, T2, TResult>): TObservable<TResult>; static;
  end;

implementation

// Implémentation de TObservable<T>
constructor TObservable<T>.Create(const InitialValue: T);
begin
  inherited Create;
  FValue := InitialValue;
  FObservers := TList<TProc<T, T>>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TObservable<T>.Destroy;
begin
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TObservable<T>.Subscribe(const Observer: TProc<T, T>);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TObservable<T>.Unsubscribe(const Observer: TProc<T, T>);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

function TObservable<T>.GetValue: T;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

procedure TObservable<T>.SetValue(const Value: T);
var
  OldValue: T;
  Observer: TProc<T, T>;
  ObserversCopy: TArray<TProc<T, T>>;
begin
  FLock.Enter;
  try
    if not TEqualityComparer<T>.Default.Equals(FValue, Value) then
    begin
      OldValue := FValue;
      FValue := Value;
      ObserversCopy := FObservers.ToArray;
    end
    else
      Exit;
  finally
    FLock.Leave;
  end;

  for Observer in ObserversCopy do
    Observer(OldValue, Value);
end;

// Implémentation de TMapObservable<TSource, TResult>
constructor TMapObservable<TSource, TResult>.Create(Source: TObservable<TSource>;
  Mapper: TFunc<TSource, TResult>);
begin
  inherited Create(Mapper(Source.Value));

  FSource := Source;
  FMapper := Mapper;

  FSource.Subscribe(SourceChanged);
end;

destructor TMapObservable<TSource, TResult>.Destroy;
begin
  FSource.Unsubscribe(SourceChanged);
  inherited;
end;

procedure TMapObservable<TSource, TResult>.SourceChanged(OldValue, NewValue: TSource);
begin
  SetValue(FMapper(NewValue));
end;

// Implémentation de TFilterObservable<T>
constructor TFilterObservable<T>.Create(Source: TObservable<T>; Predicate: TPredicate<T>);
begin
  inherited Create;

  FSource := Source;
  FPredicate := Predicate;
  FObservers := TList<TProc<T, T>>.Create;
  FLock := TCriticalSection.Create;

  FSource.Subscribe(SourceChanged);
end;

destructor TFilterObservable<T>.Destroy;
begin
  FSource.Unsubscribe(SourceChanged);
  FLock.Free;
  FObservers.Free;
  inherited;
end;

procedure TFilterObservable<T>.Subscribe(const Observer: TProc<T, T>);
begin
  FLock.Enter;
  try
    if not FObservers.Contains(Observer) then
      FObservers.Add(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TFilterObservable<T>.Unsubscribe(const Observer: TProc<T, T>);
begin
  FLock.Enter;
  try
    FObservers.Remove(Observer);
  finally
    FLock.Leave;
  end;
end;

procedure TFilterObservable<T>.SourceChanged(OldValue, NewValue: T);
var
  Observer: TProc<T, T>;
  ObserversCopy: TArray<TProc<T, T>>;
begin
  // Notifier uniquement si la valeur passe le prédicat
  if FPredicate(NewValue) then
  begin
    FLock.Enter;
    try
      ObserversCopy := FObservers.ToArray;
    finally
      FLock.Leave;
    end;

    for Observer in ObserversCopy do
      Observer(OldValue, NewValue);
  end;
end;

// Implémentation de TCombineObservable<T1, T2, TResult>
constructor TCombineObservable<T1, T2, TResult>.Create(Source1: TObservable<T1>;
  Source2: TObservable<T2>; Combiner: TFunc<T1, T2, TResult>);
begin
  inherited Create(Combiner(Source1.Value, Source2.Value));

  FSource1 := Source1;
  FSource2 := Source2;
  FCombiner := Combiner;

  FSource1.Subscribe(Source1Changed);
  FSource2.Subscribe(Source2Changed);
end;

destructor TCombineObservable<T1, T2, TResult>.Destroy;
begin
  FSource1.Unsubscribe(Source1Changed);
  FSource2.Unsubscribe(Source2Changed);
  inherited;
end;

procedure TCombineObservable<T1, T2, TResult>.Source1Changed(OldValue, NewValue: T1);
begin
  UpdateResult;
end;

procedure TCombineObservable<T1, T2, TResult>.Source2Changed(OldValue, NewValue: T2);
begin
  UpdateResult;
end;

procedure TCombineObservable<T1, T2, TResult>.UpdateResult;
begin
  SetValue(FCombiner(FSource1.Value, FSource2.Value));
end;

// Implémentation de TObservableExtensions
class function TObservableExtensions.Map<TSource, TResult>(Source: TObservable<TSource>;
  Mapper: TFunc<TSource, TResult>): TObservable<TResult>;
begin
  Result := TMapObservable<TSource, TResult>.Create(Source, Mapper);
end;

class function TObservableExtensions.Filter<T>(Source: TObservable<T>;
  Predicate: TPredicate<T>): TFilterObservable<T>;
begin
  Result := TFilterObservable<T>.Create(Source, Predicate);
end;

class function TObservableExtensions.Combine<T1, T2, TResult>(Source1: TObservable<T1>;
  Source2: TObservable<T2>; Combiner: TFunc<T1, T2, TResult>): TObservable<TResult>;
begin
  Result := TCombineObservable<T1, T2, TResult>.Create(Source1, Source2, Combiner);
end;

end.
```

Exemple d'utilisation de notre bibliothèque réactive :

```pascal
uses
  ReactiveLib;

procedure TForm1.FormCreate(Sender: TObject);
var
  CounterObservable: TObservable<Integer>;
  DoubledObservable: TObservable<Integer>;
  EvenNumbersObservable: TFilterObservable<Integer>;
begin
  // Créer un observable pour un compteur
  CounterObservable := TObservable<Integer>.Create(0);

  // Utiliser Map pour créer un observable qui double la valeur
  DoubledObservable := TObservableExtensions.Map<Integer, Integer>(
    CounterObservable,
    function(Value: Integer): Integer
    begin
      Result := Value * 2;
    end
  );

  // Utiliser Filter pour créer un observable qui ne passe que les nombres pairs
  EvenNumbersObservable := TObservableExtensions.Filter<Integer>(
    CounterObservable,
    function(Value: Integer): Boolean
    begin
      Result := Value mod 2 = 0;
    end
  );

  // S'abonner aux observables
  CounterObservable.Subscribe(
    procedure(OldValue, NewValue: Integer)
    begin
      Label1.Caption := Format('Compteur: %d', [NewValue]);
    end
  );

  DoubledObservable.Subscribe(
    procedure(OldValue, NewValue: Integer)
    begin
      Label2.Caption := Format('Double: %d', [NewValue]);
    end
  );

  EvenNumbersObservable.Subscribe(
    procedure(OldValue, NewValue: Integer)
    begin
      Memo1.Lines.Add(Format('Nombre pair détecté: %d', [NewValue]));
    end
  );

  // Stocker l'observable pour l'utiliser plus tard
  FCounter := CounterObservable;
end;

procedure TForm1.ButtonIncrementClick(Sender: TObject);
begin
  // Incrémenter le compteur
  FCounter.Value := FCounter.Value + 1;
end;
```

## Résumé

Dans ce chapitre, nous avons exploré le pattern Observer et comment il peut être utilisé pour implémenter la programmation réactive en Delphi :

1. **Pattern Observer de base** : Nous avons commencé par une implémentation classique du pattern Observer avec des interfaces et des listes d'observateurs.

2. **Observer avec des données typées** : Nous avons vu comment adapter le pattern pour transmettre des données spécifiques aux observateurs.

3. **Observer dans l'architecture MVC** : Nous avons montré comment le pattern Observer facilite l'implémentation d'une architecture MVC propre.

4. **Observer thread-safe** : Nous avons optimisé le pattern pour qu'il fonctionne correctement dans un environnement multi-threads.

5. **Observer avec notification asynchrone** : Nous avons amélioré le pattern pour permettre des notifications asynchrones.

6. **Observer avec filtrage** : Nous avons ajouté le filtrage pour que les observateurs ne soient notifiés que des changements qui les intéressent.

7. **Programmation réactive moderne** : Nous avons créé une classe `TObservable<T>` qui encapsule une valeur et notifie les observateurs des changements.

8. **Combinaison de flux d'événements** : Nous avons montré comment combiner plusieurs observables pour créer des flux d'événements plus complexes.

9. **Mini bibliothèque réactive** : Nous avons créé une petite bibliothèque de programmation réactive pour Delphi, avec les opérateurs Map, Filter et Combine.

Le pattern Observer et la programmation réactive sont des outils puissants pour créer des applications qui réagissent automatiquement aux changements de données et d'état. En les combinant avec les techniques de multithreading que nous avons vues précédemment, vous pouvez créer des applications Delphi modernes, réactives et performantes.

## Exercice pratique

Créez une application de conversion de devises en temps réel qui utilise le pattern Observer :

1. Créez une classe `TCurrencyRate` qui encapsule le taux de change entre deux devises et implémente le pattern Observer.
2. Créez une classe `TCurrencyConverter` qui utilise des instances de `TCurrencyRate` pour convertir des montants d'une devise à une autre.
3. Créez une interface utilisateur qui permet à l'utilisateur de saisir un montant dans une devise et voit automatiquement le montant converti dans d'autres devises.
4. Ajoutez un bouton pour simuler la mise à jour des taux de change (dans une application réelle, ces mises à jour viendraient d'une API externe).
5. Utilisez les techniques de programmation réactive pour combiner et transformer les flux de données.

Cet exercice vous permettra de mettre en pratique les concepts de programmation réactive dans un contexte réel.

Dans le prochain chapitre, nous explorerons les performances et les bonnes pratiques en multithreading, y compris comment optimiser vos applications multithreads
