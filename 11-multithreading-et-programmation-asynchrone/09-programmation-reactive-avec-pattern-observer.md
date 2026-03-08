🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.9 Programmation réactive avec le pattern Observer

## Qu'est-ce que le pattern Observer ?

Le **pattern Observer** (ou Observateur) est un modèle de conception qui permet à des objets de s'abonner à des événements et d'être notifiés automatiquement quand quelque chose change.

### Analogie : La chaîne YouTube

Imaginez une chaîne YouTube :
- **La chaîne** (Subject/Sujet) : C'est l'émetteur qui publie du contenu
- **Les abonnés** (Observers/Observateurs) : Ce sont les spectateurs qui suivent la chaîne
- **La notification** : Quand une nouvelle vidéo est publiée, TOUS les abonnés reçoivent une notification automatiquement

**Principe clé** : Les abonnés ne vérifient pas constamment s'il y a du nouveau contenu. C'est la chaîne qui les prévient automatiquement !

```
         Chaîne YouTube (Subject)
              |
    __________|__________
    |         |         |
 Abonné 1  Abonné 2  Abonné 3  (Observers)
    |         |         |
 [Notifié] [Notifié] [Notifié]
```

## Pourquoi utiliser le pattern Observer ?

### Problème sans Observer

```pascal
// ❌ Code couplé et difficile à maintenir
procedure TDataManager.ModifierPrix(NouveauPrix: Double);  
begin  
  FPrix := NouveauPrix;

  // Doit connaître et appeler manuellement chaque composant
  Form1.AfficherPrix(NouveauPrix);
  Form2.MettreAJourGraphique(NouveauPrix);
  RapportManager.EnregistrerChangement(NouveauPrix);
  EmailService.EnvoyerAlerte(NouveauPrix);
end;
```

**Problèmes** :
- Couplage fort : DataManager doit connaître tous les objets intéressés
- Difficile d'ajouter de nouveaux observateurs
- Code rigide et difficile à maintenir

### Solution avec Observer

```pascal
// ✅ Code découplé et flexible
procedure TDataManager.ModifierPrix(NouveauPrix: Double);  
begin  
  FPrix := NouveauPrix;

  // Notifier tous les observateurs (sans les connaître individuellement)
  NotifierObservateurs;
end;
```

**Avantages** :
- Couplage faible : Le sujet ne connaît pas les observateurs
- Facile d'ajouter ou retirer des observateurs
- Code flexible et maintenable

## Implémentation basique du pattern Observer

### Définir les interfaces

```pascal
type
  // Interface pour les observateurs
  IObserver = interface
    ['{1234-5678-9ABC-DEF0}']
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

  // Interface pour le sujet observé
  ISubject = interface
    ['{ABCD-1234-5678-9012}']
    procedure Attacher(Observer: IObserver);
    procedure Detacher(Observer: IObserver);
    procedure Notifier(const Donnees: TValue);
  end;
```

### Implémenter le Sujet

```pascal
uses
  System.Generics.Collections, System.Rtti;

type
  TSubject = class(TInterfacedObject, ISubject)
  private
    FObservers: TList<IObserver>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attacher(Observer: IObserver);
    procedure Detacher(Observer: IObserver);
    procedure Notifier(const Donnees: TValue);
  end;

constructor TSubject.Create;  
begin  
  inherited;
  FObservers := TList<IObserver>.Create;
end;

destructor TSubject.Destroy;  
begin  
  FObservers.Free;
  inherited;
end;

procedure TSubject.Attacher(Observer: IObserver);  
begin  
  if not FObservers.Contains(Observer) then
    FObservers.Add(Observer);
end;

procedure TSubject.Detacher(Observer: IObserver);  
begin  
  FObservers.Remove(Observer);
end;

procedure TSubject.Notifier(const Donnees: TValue);  
var  
  Observer: IObserver;
begin
  for Observer in FObservers do
    Observer.Actualiser(Self, Donnees);
end;
```

### Implémenter un Observateur

```pascal
type
  TAffichagePrix = class(TInterfacedObject, IObserver)
  private
    FLabel: TLabel;
  public
    constructor Create(ALabel: TLabel);
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

constructor TAffichagePrix.Create(ALabel: TLabel);  
begin  
  inherited Create;
  FLabel := ALabel;
end;

procedure TAffichagePrix.Actualiser(const Sujet: TObject; const Donnees: TValue);  
begin  
  // Mettre à jour l'affichage quand le prix change
  FLabel.Caption := Format('Prix actuel : %.2f €', [Donnees.AsExtended]);
end;
```

### Utilisation

```pascal
var
  GestionnairePrix: TSubject;
  AffichagePrix1, AffichagePrix2: IObserver;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  // Créer le sujet
  GestionnairePrix := TSubject.Create;

  // Créer les observateurs
  AffichagePrix1 := TAffichagePrix.Create(Label1);
  AffichagePrix2 := TAffichagePrix.Create(Label2);

  // S'abonner au sujet
  GestionnairePrix.Attacher(AffichagePrix1);
  GestionnairePrix.Attacher(AffichagePrix2);
end;

procedure TForm1.ButtonChangerPrixClick(Sender: TObject);  
begin  
  // Notifier tous les observateurs du nouveau prix
  GestionnairePrix.Notifier(TValue.From<Double>(19.99));
  // Label1 et Label2 sont automatiquement mis à jour !
end;
```

## Exemple concret : Système de notifications

Créons un système où plusieurs parties de l'application sont notifiées des événements.

```pascal
type
  // Types d'événements
  TTypeEvenement = (teNouveauMessage, teChangementStatut, teAlerte);

  // Données d'événement
  TEvenementData = record
    TypeEvenement: TTypeEvenement;
    Message: string;
    Timestamp: TDateTime;
  end;

  // Gestionnaire d'événements
  TGestionnaireEvenements = class
  private
    FObservateurs: TList<IObserver>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AbonnerObservateur(Observer: IObserver);
    procedure DesabonnerObservateur(Observer: IObserver);
    procedure PublierEvenement(const Evenement: TEvenementData);
  end;

constructor TGestionnaireEvenements.Create;  
begin  
  inherited;
  FObservateurs := TList<IObserver>.Create;
end;

destructor TGestionnaireEvenements.Destroy;  
begin  
  FObservateurs.Free;
  inherited;
end;

procedure TGestionnaireEvenements.AbonnerObservateur(Observer: IObserver);  
begin  
  if not FObservateurs.Contains(Observer) then
    FObservateurs.Add(Observer);
end;

procedure TGestionnaireEvenements.DesabonnerObservateur(Observer: IObserver);  
begin  
  FObservateurs.Remove(Observer);
end;

procedure TGestionnaireEvenements.PublierEvenement(const Evenement: TEvenementData);  
var  
  Observer: IObserver;
begin
  for Observer in FObservateurs do
  begin
    // Notifier de manière asynchrone pour ne pas bloquer
    TTask.Run(
      procedure
      begin
        TThread.Queue(nil,
          procedure
          begin
            Observer.Actualiser(Self, TValue.From<TEvenementData>(Evenement));
          end
        );
      end
    );
  end;
end;

// Observateur : Affichage dans un Memo
type
  TObservateurMemo = class(TInterfacedObject, IObserver)
  private
    FMemo: TMemo;
  public
    constructor Create(AMemo: TMemo);
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

constructor TObservateurMemo.Create(AMemo: TMemo);  
begin  
  inherited Create;
  FMemo := AMemo;
end;

procedure TObservateurMemo.Actualiser(const Sujet: TObject; const Donnees: TValue);  
var  
  Evenement: TEvenementData;
begin
  Evenement := Donnees.AsType<TEvenementData>;
  FMemo.Lines.Add(
    Format('[%s] %s: %s',
      [FormatDateTime('hh:nn:ss', Evenement.Timestamp),
       GetEnumName(TypeInfo(TTypeEvenement), Ord(Evenement.TypeEvenement)),
       Evenement.Message])
  );
end;

// Observateur : Notification système
type
  TObservateurNotification = class(TInterfacedObject, IObserver)
  public
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

procedure TObservateurNotification.Actualiser(const Sujet: TObject; const Donnees: TValue);  
var  
  Evenement: TEvenementData;
begin
  Evenement := Donnees.AsType<TEvenementData>;

  // Afficher une notification système seulement pour les alertes
  if Evenement.TypeEvenement = teAlerte then
  begin
    NotificationCenter1.PresentNotification(
      Evenement.Message,
      'Alerte importante'
    );
  end;
end;

// Utilisation
var
  Gestionnaire: TGestionnaireEvenements;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Gestionnaire := TGestionnaireEvenements.Create;

  // Abonner différents observateurs
  Gestionnaire.AbonnerObservateur(TObservateurMemo.Create(Memo1));
  Gestionnaire.AbonnerObservateur(TObservateurNotification.Create);
end;

procedure TForm1.ButtonEnvoyerMessageClick(Sender: TObject);  
var  
  Evenement: TEvenementData;
begin
  Evenement.TypeEvenement := teNouveauMessage;
  Evenement.Message := 'Nouveau message reçu';
  Evenement.Timestamp := Now;

  Gestionnaire.PublierEvenement(Evenement);
end;
```

## Pattern Observer avec des événements Delphi

Delphi possède déjà un système d'événements intégré qui implémente le pattern Observer.

```pascal
type
  // Définir un type d'événement personnalisé
  TNotificationEvent = procedure(Sender: TObject; const Message: string) of object;

  TNotificationManager = class
  private
    FOnNotification: TNotificationEvent;
  public
    property OnNotification: TNotificationEvent read FOnNotification write FOnNotification;
    procedure EnvoyerNotification(const Message: string);
  end;

procedure TNotificationManager.EnvoyerNotification(const Message: string);  
begin  
  if Assigned(FOnNotification) then
    FOnNotification(Self, Message);
end;

// Utilisation
var
  NotifManager: TNotificationManager;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  NotifManager := TNotificationManager.Create;

  // S'abonner à l'événement
  NotifManager.OnNotification := GererNotification;
end;

procedure TForm1.GererNotification(Sender: TObject; const Message: string);  
begin  
  Memo1.Lines.Add(Message);
  ShowMessage(Message);
end;

procedure TForm1.ButtonEnvoyerClick(Sender: TObject);  
begin  
  NotifManager.EnvoyerNotification('Test de notification');
end;
```

## Liste d'événements multiples

Permettre plusieurs abonnés à un même événement.

```pascal
type
  TNotificationProc = reference to procedure(const Message: string);

  TMultiNotificationManager = class
  private
    FAbonnes: TList<TNotificationProc>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abonner(AProc: TNotificationProc);
    procedure Desabonner(AProc: TNotificationProc);
    procedure Publier(const Message: string);
  end;

constructor TMultiNotificationManager.Create;  
begin  
  inherited;
  FAbonnes := TList<TNotificationProc>.Create;
end;

destructor TMultiNotificationManager.Destroy;  
begin  
  FAbonnes.Free;
  inherited;
end;

procedure TMultiNotificationManager.Abonner(AProc: TNotificationProc);  
begin  
  FAbonnes.Add(AProc);
end;

procedure TMultiNotificationManager.Desabonner(AProc: TNotificationProc);  
begin  
  FAbonnes.Remove(AProc);
end;

procedure TMultiNotificationManager.Publier(const Message: string);  
var  
  Abonne: TNotificationProc;
begin
  for Abonne in FAbonnes do
    Abonne(Message);
end;

// Utilisation
var
  Manager: TMultiNotificationManager;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Manager := TMultiNotificationManager.Create;

  // Abonner plusieurs gestionnaires
  Manager.Abonner(
    procedure(const Message: string)
    begin
      Memo1.Lines.Add('Log: ' + Message);
    end
  );

  Manager.Abonner(
    procedure(const Message: string)
    begin
      Label1.Caption := Message;
    end
  );

  Manager.Abonner(
    procedure(const Message: string)
    begin
      if Pos('Erreur', Message) > 0 then
        ShowMessage('ALERTE: ' + Message);
    end
  );
end;

procedure TForm1.ButtonPublierClick(Sender: TObject);  
begin  
  Manager.Publier('Événement: Traitement terminé');
  // Tous les abonnés sont notifiés automatiquement !
end;
```

## Pattern Observer avec filtrage

Permettre aux observateurs de s'abonner seulement à certains types d'événements.

```pascal
type
  TCategorieEvenement = (ceInfo, ceAvertissement, ceErreur);

  TEvenement = record
    Categorie: TCategorieEvenement;
    Message: string;
    Timestamp: TDateTime;
  end;

  TObservateurFiltre = class(TInterfacedObject, IObserver)
  private
    FCategories: set of TCategorieEvenement;
    FCallback: TProc<TEvenement>;
  public
    constructor Create(ACategories: set of TCategorieEvenement;
                      ACallback: TProc<TEvenement>);
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

constructor TObservateurFiltre.Create(
  ACategories: set of TCategorieEvenement;
  ACallback: TProc<TEvenement>);
begin
  inherited Create;
  FCategories := ACategories;
  FCallback := ACallback;
end;

procedure TObservateurFiltre.Actualiser(const Sujet: TObject; const Donnees: TValue);  
var  
  Evt: TEvenement;
begin
  Evt := Donnees.AsType<TEvenement>;

  // Filtrer selon la catégorie
  if Evt.Categorie in FCategories then
    FCallback(Evt);
end;

// Utilisation
procedure TForm1.FormCreate(Sender: TObject);  
var  
  Gestionnaire: TSubject;
  ObsErreurs, ObsTout: IObserver;
begin
  Gestionnaire := TSubject.Create;

  // Observateur qui ne reçoit QUE les erreurs
  ObsErreurs := TObservateurFiltre.Create(
    [ceErreur],
    procedure(const Evt: TEvenement)
    begin
      MemoErreurs.Lines.Add(Evt.Message);
    end
  );

  // Observateur qui reçoit TOUS les événements
  ObsTout := TObservateurFiltre.Create(
    [ceInfo, ceAvertissement, ceErreur],
    procedure(const Evt: TEvenement)
    begin
      MemoTout.Lines.Add(
        Format('[%s] %s',
          [GetEnumName(TypeInfo(TCategorieEvenement), Ord(Evt.Categorie)),
           Evt.Message])
      );
    end
  );

  Gestionnaire.Attacher(ObsErreurs);
  Gestionnaire.Attacher(ObsTout);
end;
```

## Pattern Observer thread-safe

Pour utiliser le pattern Observer avec le multithreading.

```pascal
type
  TSubjectThreadSafe = class
  private
    FObservateurs: TList<IObserver>;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attacher(Observer: IObserver);
    procedure Detacher(Observer: IObserver);
    procedure Notifier(const Donnees: TValue);
  end;

constructor TSubjectThreadSafe.Create;  
begin  
  inherited;
  FObservateurs := TList<IObserver>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TSubjectThreadSafe.Destroy;  
begin  
  FCS.Free;
  FObservateurs.Free;
  inherited;
end;

procedure TSubjectThreadSafe.Attacher(Observer: IObserver);  
begin  
  FCS.Enter;
  try
    if not FObservateurs.Contains(Observer) then
      FObservateurs.Add(Observer);
  finally
    FCS.Leave;
  end;
end;

procedure TSubjectThreadSafe.Detacher(Observer: IObserver);  
begin  
  FCS.Enter;
  try
    FObservateurs.Remove(Observer);
  finally
    FCS.Leave;
  end;
end;

procedure TSubjectThreadSafe.Notifier(const Donnees: TValue);  
var  
  Observer: IObserver;
  ListeCopie: TArray<IObserver>;
begin
  // Copier la liste pour éviter les problèmes de concurrence
  FCS.Enter;
  try
    ListeCopie := FObservateurs.ToArray;
  finally
    FCS.Leave;
  end;

  // Notifier tous les observateurs
  for Observer in ListeCopie do
  begin
    TThread.Queue(nil,
      procedure
      begin
        Observer.Actualiser(Self, Donnees);
      end
    );
  end;
end;
```

## Exemple complet : Moniteur de téléchargements

```pascal
type
  TStatutTelechargement = (stEnAttente, stEnCours, stTermine, stEchec);

  TInfoTelechargement = record
    ID: Integer;
    Fichier: string;
    Statut: TStatutTelechargement;
    Progression: Integer;
  end;

  TGestionnaireTelechargements = class
  private
    FObservateurs: TList<IObserver>;
    FTelechargements: TDictionary<Integer, TInfoTelechargement>;
    FProchainID: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Abonner(Observer: IObserver);
    procedure Desabonner(Observer: IObserver);
    function AjouterTelechargement(const Fichier: string): Integer;
    procedure DemarrerTelechargement(ID: Integer);
    procedure MettreAJourProgression(ID, Progression: Integer);
    procedure TerminerTelechargement(ID: Integer; Succes: Boolean);
  private
    procedure Notifier(const Info: TInfoTelechargement);
  end;

constructor TGestionnaireTelechargements.Create;  
begin  
  inherited;
  FObservateurs := TList<IObserver>.Create;
  FTelechargements := TDictionary<Integer, TInfoTelechargement>.Create;
  FProchainID := 1;
end;

destructor TGestionnaireTelechargements.Destroy;  
begin  
  FTelechargements.Free;
  FObservateurs.Free;
  inherited;
end;

procedure TGestionnaireTelechargements.Abonner(Observer: IObserver);  
begin  
  if not FObservateurs.Contains(Observer) then
    FObservateurs.Add(Observer);
end;

procedure TGestionnaireTelechargements.Desabonner(Observer: IObserver);  
begin  
  FObservateurs.Remove(Observer);
end;

function TGestionnaireTelechargements.AjouterTelechargement(const Fichier: string): Integer;  
var  
  Info: TInfoTelechargement;
begin
  Result := FProchainID;
  Inc(FProchainID);

  Info.ID := Result;
  Info.Fichier := Fichier;
  Info.Statut := stEnAttente;
  Info.Progression := 0;

  FTelechargements.Add(Result, Info);
  Notifier(Info);
end;

procedure TGestionnaireTelechargements.DemarrerTelechargement(ID: Integer);  
var  
  Info: TInfoTelechargement;
begin
  if FTelechargements.TryGetValue(ID, Info) then
  begin
    Info.Statut := stEnCours;
    FTelechargements[ID] := Info;
    Notifier(Info);
  end;
end;

procedure TGestionnaireTelechargements.MettreAJourProgression(ID, Progression: Integer);  
var  
  Info: TInfoTelechargement;
begin
  if FTelechargements.TryGetValue(ID, Info) then
  begin
    Info.Progression := Progression;
    FTelechargements[ID] := Info;
    Notifier(Info);
  end;
end;

procedure TGestionnaireTelechargements.TerminerTelechargement(ID: Integer; Succes: Boolean);  
var  
  Info: TInfoTelechargement;
begin
  if FTelechargements.TryGetValue(ID, Info) then
  begin
    if Succes then
      Info.Statut := stTermine
    else
      Info.Statut := stEchec;
    Info.Progression := 100;
    FTelechargements[ID] := Info;
    Notifier(Info);
  end;
end;

procedure TGestionnaireTelechargements.Notifier(const Info: TInfoTelechargement);  
var  
  Observer: IObserver;
begin
  for Observer in FObservateurs do
    Observer.Actualiser(Self, TValue.From<TInfoTelechargement>(Info));
end;

// Observateur : Affichage dans ListView
type
  TObservateurListView = class(TInterfacedObject, IObserver)
  private
    FListView: TListView;
    function TrouverItem(ID: Integer): TListItem;
  public
    constructor Create(AListView: TListView);
    procedure Actualiser(const Sujet: TObject; const Donnees: TValue);
  end;

constructor TObservateurListView.Create(AListView: TListView);  
begin  
  inherited Create;
  FListView := AListView;
end;

function TObservateurListView.TrouverItem(ID: Integer): TListItem;  
var  
  i: Integer;
begin
  Result := nil;
  for i := 0 to FListView.Items.Count - 1 do
  begin
    if FListView.Items[i].Caption = IntToStr(ID) then
    begin
      Result := FListView.Items[i];
      Break;
    end;
  end;
end;

procedure TObservateurListView.Actualiser(const Sujet: TObject; const Donnees: TValue);  
var  
  Info: TInfoTelechargement;
  Item: TListItem;
begin
  Info := Donnees.AsType<TInfoTelechargement>;

  Item := TrouverItem(Info.ID);
  if Item = nil then
  begin
    Item := FListView.Items.Add;
    Item.Caption := IntToStr(Info.ID);
    Item.SubItems.Add('');
    Item.SubItems.Add('');
    Item.SubItems.Add('');
  end;

  Item.SubItems[0] := ExtractFileName(Info.Fichier);
  Item.SubItems[1] := GetEnumName(TypeInfo(TStatutTelechargement), Ord(Info.Statut));
  Item.SubItems[2] := IntToStr(Info.Progression) + '%';
end;

// Utilisation
var
  Gestionnaire: TGestionnaireTelechargements;

procedure TForm1.FormCreate(Sender: TObject);  
begin  
  Gestionnaire := TGestionnaireTelechargements.Create;

  // Abonner la ListView
  Gestionnaire.Abonner(TObservateurListView.Create(ListView1));
end;

procedure TForm1.ButtonTelechargerClick(Sender: TObject);  
var  
  ID: Integer;
begin
  ID := Gestionnaire.AjouterTelechargement('http://example.com/file.zip');

  TTask.Run(
    procedure
    var
      i: Integer;
    begin
      Gestionnaire.DemarrerTelechargement(ID);

      for i := 1 to 100 do
      begin
        Sleep(50);
        Gestionnaire.MettreAJourProgression(ID, i);
      end;

      Gestionnaire.TerminerTelechargement(ID, True);
    end
  );
end;
```

## Bonnes pratiques

### 1. Éviter les références circulaires

```pascal
// ❌ MAUVAIS : Référence circulaire
type
  TObservateurA = class(TInterfacedObject, IObserver)
  private
    FSubjet: TSubject; // Garde une référence
  end;

// ✅ BON : Utiliser des interfaces
type
  TObservateurB = class(TInterfacedObject, IObserver)
  private
    FSubjet: ISubject; // Interface, pas de fuite mémoire
  end;
```

### 2. Se désabonner proprement

```pascal
procedure TForm1.FormDestroy(Sender: TObject);  
begin  
  // Toujours se désabonner pour éviter les fuites
  if Assigned(Gestionnaire) and Assigned(FObservateur) then
    Gestionnaire.Desabonner(FObservateur);
end;
```

### 3. Protéger contre les exceptions

```pascal
procedure TSubject.Notifier(const Donnees: TValue);  
var  
  Observer: IObserver;
begin
  for Observer in FObservateurs do
  begin
    try
      Observer.Actualiser(Self, Donnees);
    except
      on E: Exception do
      begin
        // Logger l'erreur mais continuer avec les autres observateurs
        LogError('Erreur dans observateur : ' + E.Message);
      end;
    end;
  end;
end;
```

### 4. Limiter la fréquence des notifications

```pascal
type
  TSubjectAvecThrottle = class
  private
    FDerniereNotification: TDateTime;
    FDelaiMinimum: Integer; // En millisecondes
  public
    procedure Notifier(const Donnees: TValue);
  end;

procedure TSubjectAvecThrottle.Notifier(const Donnees: TValue);  
begin  
  // Ne notifier que si assez de temps s'est écoulé
  if MilliSecondsBetween(Now, FDerniereNotification) >= FDelaiMinimum then
  begin
    // Notifier tous les observateurs
    // ...
    FDerniereNotification := Now;
  end;
end;
```

## Points clés à retenir

- Le **pattern Observer** permet un couplage faible entre objets
- Le **sujet** notifie automatiquement tous les **observateurs** abonnés
- Utilisez des **interfaces** pour plus de flexibilité
- Delphi possède un système d'**événements** natif basé sur ce pattern
- Le pattern est idéal pour les **systèmes de notifications** et les **architectures réactives**
- Pensez **thread-safe** si vous utilisez le multithreading
- Permettez le **filtrage** pour que les observateurs ne reçoivent que ce qui les intéresse
- Toujours **se désabonner** pour éviter les fuites mémoire
- **Protégez** les notifications contre les exceptions
- **Limitez la fréquence** des notifications si nécessaire pour les performances

Le pattern Observer est fondamental en programmation réactive et permet de créer des applications flexibles, maintenables et découplées. C'est un pattern essentiel à maîtriser pour tout développeur Delphi moderne.

⏭️ [Performances et bonnes pratiques en multithreading](/11-multithreading-et-programmation-asynchrone/10-performances-et-bonnes-pratiques-en-multithreading.md)
