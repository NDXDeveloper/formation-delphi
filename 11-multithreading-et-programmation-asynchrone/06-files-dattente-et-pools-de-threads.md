🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.6 Files d'attente et pools de threads

## Qu'est-ce qu'une file d'attente ?

Une **file d'attente** (queue) est une structure de données qui fonctionne selon le principe **FIFO** (First In, First Out) : le premier élément ajouté est le premier à être retiré.

### Analogie : La file au supermarché

Imaginez la file d'attente à la caisse d'un supermarché :
- Les clients arrivent et se mettent **à la fin** de la file (ajout)
- La caissière sert les clients **depuis le début** de la file (retrait)
- Le premier arrivé est le premier servi (FIFO)
- Si la caissière prend sa pause, les clients attendent dans la file

En programmation multithreadée, c'est exactement pareil !

```
Ajout →  [Client 1] → [Client 2] → [Client 3] → [Client 4]  → Retrait
         (Premier)                              (Dernier)
```

## Pourquoi utiliser des files d'attente ?

### 1. Communication entre threads

Les files d'attente permettent à des threads de s'échanger des données de manière sûre et ordonnée.

```pascal
// Thread 1 (Producteur)
FileAttente.Push('Nouvelle tâche');

// Thread 2 (Consommateur)
Tache := FileAttente.Pop; // Récupère 'Nouvelle tâche'
```

### 2. Découplage producteur-consommateur

Un thread produit des données à son rythme, un autre les consomme au sien.

**Exemple** :
- Un thread télécharge des images (producteur)
- Un autre thread les traite (consommateur)
- Ils ne se bloquent pas mutuellement

### 3. Lissage de charge

Si les données arrivent plus vite qu'elles ne peuvent être traitées, elles attendent dans la file.

## TThreadedQueue : File d'attente thread-safe

Delphi fournit `TThreadedQueue`, une file d'attente conçue spécifiquement pour le multithreading.

### Inclusion et déclaration

```pascal
uses
  System.Generics.Collections;

var
  FileMessages: TThreadedQueue<string>;

initialization
  // Créer une file avec capacité illimitée
  FileMessages := TThreadedQueue<string>.Create;

finalization
  FileMessages.Free;
```

### Opérations de base

#### Push : Ajouter un élément

```pascal
// Ajouter un message à la file
FileMessages.PushItem('Message 1');  
FileMessages.PushItem('Message 2');  
FileMessages.PushItem('Message 3');  
```

#### Pop : Retirer un élément

```pascal
var
  Message: string;
begin
  // Retirer le premier message de la file
  FileMessages.PopItem(Message);
  // Message contient maintenant 'Message 1'
end;
```

#### PopItem avec timeout

```pascal
var
  Message: string;
begin
  // Attendre maximum 1000 ms (1 seconde)
  case FileMessages.PopItem(Message, 1000) of
    wrSignaled:
      // Un message a été récupéré
      ShowMessage('Reçu : ' + Message);

    wrTimeout:
      // Timeout : pas de message dans le délai
      ShowMessage('Aucun message reçu');

    wrAbandoned, wrError:
      // Erreur
      ShowMessage('Erreur');
  end;
end;
```

### Exemple complet : Producteur-Consommateur simple

```pascal
uses
  System.Generics.Collections, System.SyncObjs;

var
  FileAttente: TThreadedQueue<Integer>;

type
  // Thread producteur : ajoute des nombres
  TThreadProducteur = class(TThread)
  protected
    procedure Execute; override;
  end;

  // Thread consommateur : traite les nombres
  TThreadConsommateur = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadProducteur.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if Terminated then Exit;

    // Produire un nombre
    FileAttente.PushItem(i);

    TThread.Queue(nil,
      procedure
      begin
        Form1.Memo1.Lines.Add('Produit : ' + IntToStr(i));
      end
    );

    Sleep(100); // Simuler un temps de production
  end;

  // Signal de fin
  FileAttente.PushItem(-1); // -1 = fin
end;

procedure TThreadConsommateur.Execute;  
var  
  Nombre: Integer;
begin
  while not Terminated do
  begin
    // Consommer un nombre (attendre si vide)
    if FileAttente.PopItem(Nombre) = wrSignaled then
    begin
      // Vérifier le signal de fin
      if Nombre = -1 then
        Break;

      // Traiter le nombre
      Sleep(150); // Simuler un traitement

      TThread.Queue(nil,
        procedure
        begin
          Form1.Memo1.Lines.Add('Consommé : ' + IntToStr(Nombre));
        end
      );
    end;
  end;
end;

// Démarrage
procedure TForm1.ButtonDemarrerClick(Sender: TObject);  
begin  
  FileAttente := TThreadedQueue<Integer>.Create;

  TThreadProducteur.Create(False);
  TThreadConsommateur.Create(False);
end;
```

## File d'attente avec capacité limitée

Parfois, vous voulez limiter la taille de la file pour éviter qu'elle ne consomme trop de mémoire.

```pascal
var
  FileLimitee: TThreadedQueue<string>;

// Créer une file avec capacité de 10 éléments maximum
FileLimitee := TThreadedQueue<string>.Create(10, INFINITE, 100);

// Si la file est pleine, PushItem attend qu'une place se libère
FileLimitee.PushItem('Message');
```

### Paramètres du constructeur

```pascal
constructor Create(
  AQueueDepth: Integer;      // Capacité max (0 = illimitée)
  PushTimeout: Cardinal;     // Timeout pour Push en ms
  PopTimeout: Cardinal       // Timeout pour Pop en ms
);
```

## Pools de threads

Un **pool de threads** est un ensemble de threads réutilisables qui attendent des tâches à exécuter.

### Analogie : Le restaurant

Au lieu d'embaucher un nouveau serveur pour chaque client (coûteux et lent), le restaurant maintient une équipe de serveurs prêts à servir :
- Quand un client arrive, un serveur disponible s'en occupe
- Quand le serveur a fini, il devient disponible pour le prochain client
- Le nombre de serveurs s'ajuste selon l'affluence

### Avantages du pool de threads

1. **Performance** : Réutiliser des threads plutôt qu'en créer de nouveaux
2. **Contrôle des ressources** : Limiter le nombre de threads simultanés
3. **Gestion automatique** : Le pool ajuste le nombre de threads selon la charge

## TThreadPool : Le pool de threads de Delphi

Delphi gère automatiquement un pool de threads global utilisé par TTask.

### Configuration du pool

```pascal
uses
  System.Threading;

// Obtenir le pool par défaut
var
  Pool: TThreadPool;
begin
  Pool := TThreadPool.Default;

  // Configurer le nombre minimum de threads
  Pool.SetMinWorkerThreads(2);

  // Configurer le nombre maximum de threads
  Pool.SetMaxWorkerThreads(8);
end;
```

### Utilisation avec TTask

Quand vous utilisez TTask.Run, le pool de threads est utilisé automatiquement :

```pascal
// Ces tâches utilisent le pool de threads
for i := 1 to 100 do  
begin  
  TTask.Run(
    procedure
    begin
      // Traitement
    end
  );
end;
// Le pool gère intelligemment le nombre de threads actifs
```

## Pattern Producteur-Consommateur avancé

Un pattern très courant : plusieurs producteurs alimentent une file, plusieurs consommateurs la traitent.

```pascal
type
  TTravail = record
    ID: Integer;
    Donnees: string;
  end;

var
  FileTravaux: TThreadedQueue<TTravail>;
  NbConsommateurs: Integer;

const
  SIGNAL_FIN: Integer = -1;

// Thread consommateur générique
type
  TThreadConsommateur = class(TThread)
  private
    FNumero: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ANumero: Integer);
  end;

constructor TThreadConsommateur.Create(ANumero: Integer);  
begin  
  inherited Create(False);
  FreeOnTerminate := True;
  FNumero := ANumero;
end;

procedure TThreadConsommateur.Execute;  
var  
  Travail: TTravail;
begin
  while not Terminated do
  begin
    // Attendre un travail (max 5 secondes)
    if FileTravaux.PopItem(Travail, 5000) = wrSignaled then
    begin
      // Vérifier le signal de fin
      if Travail.ID = SIGNAL_FIN then
      begin
        // Remettre le signal pour les autres consommateurs
        FileTravaux.PushItem(Travail);
        Break;
      end;

      // Traiter le travail
      Sleep(Random(200) + 100);

      TThread.Queue(nil,
        procedure
        begin
          Form1.Memo1.Lines.Add(
            Format('Consommateur %d a traité : %d - %s',
              [FNumero, Travail.ID, Travail.Donnees])
          );
        end
      );
    end;
  end;
end;

// Démarrage du système
procedure TForm1.ButtonDemarrerClick(Sender: TObject);  
var  
  i: Integer;
  Travail: TTravail;
begin
  Randomize;
  FileTravaux := TThreadedQueue<TTravail>.Create;

  // Créer 3 consommateurs
  for i := 1 to 3 do
    TThreadConsommateur.Create(i);

  // Producteur (dans le thread principal pour l'exemple)
  TTask.Run(
    procedure
    var
      j: Integer;
      T: TTravail;
    begin
      for j := 1 to 50 do
      begin
        T.ID := j;
        T.Donnees := 'Travail numéro ' + IntToStr(j);
        FileTravaux.PushItem(T);

        Sleep(50); // Rythme de production
      end;

      // Envoyer le signal de fin
      T.ID := SIGNAL_FIN;
      T.Donnees := 'FIN';
      FileTravaux.PushItem(T);
    end
  );
end;
```

## File d'attente de priorité

Pour traiter certains éléments en priorité, vous pouvez créer plusieurs files :

```pascal
var
  FilePrioriteHaute: TThreadedQueue<TTache>;
  FilePrioriteNormale: TThreadedQueue<TTache>;
  FilePrioriteBasse: TThreadedQueue<TTache>;

procedure TThreadConsommateur.Execute;  
var  
  Tache: TTache;
  WaitResult: TWaitResult;
begin
  while not Terminated do
  begin
    // Essayer d'abord la haute priorité (sans attendre)
    WaitResult := FilePrioriteHaute.PopItem(Tache, 0);

    if WaitResult <> wrSignaled then
    begin
      // Sinon, essayer la priorité normale (sans attendre)
      WaitResult := FilePrioriteNormale.PopItem(Tache, 0);
    end;

    if WaitResult <> wrSignaled then
    begin
      // Sinon, attendre sur la priorité basse
      WaitResult := FilePrioriteBasse.PopItem(Tache, 1000);
    end;

    if WaitResult = wrSignaled then
    begin
      // Traiter la tâche
      TraiterTache(Tache);
    end;
  end;
end;
```

## TThreadList : Liste thread-safe

Pour les cas où vous n'avez pas besoin d'une file FIFO, `TThreadList` offre une liste thread-safe.

```pascal
uses
  System.Classes;

var
  ListeThreadSafe: TThreadList<string>;

// Créer la liste
ListeThreadSafe := TThreadList<string>.Create;

// Ajouter des éléments (thread-safe)
ListeThreadSafe.Add('Element 1');

// Accéder à la liste de manière thread-safe
var
  Liste: TList<string>;
begin
  Liste := ListeThreadSafe.LockList;
  try
    // Travailler avec la liste
    for var Item in Liste do
      Memo1.Lines.Add(Item);
  finally
    ListeThreadSafe.UnlockList;
  end;
end;

// Libérer
ListeThreadSafe.Free;
```

## Moniteur de file d'attente

Parfois, vous voulez surveiller l'état de la file.

```pascal
type
  TMoniteurFile = class
  private
    FFile: TThreadedQueue<Integer>;
    FCS: TCriticalSection;
    FNbAjoutes: Integer;
    FNbRetires: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ajouter(Valeur: Integer);
    function Retirer: Integer;
    function TailleFile: Integer;
    function Statistiques: string;
  end;

constructor TMoniteurFile.Create;  
begin  
  FFile := TThreadedQueue<Integer>.Create;
  FCS := TCriticalSection.Create;
  FNbAjoutes := 0;
  FNbRetires := 0;
end;

destructor TMoniteurFile.Destroy;  
begin  
  FFile.Free;
  FCS.Free;
  inherited;
end;

procedure TMoniteurFile.Ajouter(Valeur: Integer);  
begin  
  FFile.PushItem(Valeur);

  FCS.Enter;
  try
    Inc(FNbAjoutes);
  finally
    FCS.Leave;
  end;
end;

function TMoniteurFile.Retirer: Integer;  
begin  
  if FFile.PopItem(Result) = wrSignaled then
  begin
    FCS.Enter;
    try
      Inc(FNbRetires);
    finally
      FCS.Leave;
    end;
  end;
end;

function TMoniteurFile.TailleFile: Integer;  
begin  
  Result := FFile.QueueSize;
end;

function TMoniteurFile.Statistiques: string;  
begin  
  FCS.Enter;
  try
    Result := Format('Ajoutés: %d, Retirés: %d, En attente: %d',
      [FNbAjoutes, FNbRetires, TailleFile]);
  finally
    FCS.Leave;
  end;
end;
```

## Exemple pratique : Traitement de fichiers par lots

```pascal
type
  TFichierATraiter = record
    CheminComplet: string;
    TypeTraitement: Integer;
  end;

var
  FileTraitement: TThreadedQueue<TFichierATraiter>;

// Scanner un dossier et ajouter à la file
procedure TForm1.ScannerDossier(const Dossier: string);  
var  
  Fichiers: TArray<string>;
  Fichier: string;
  Item: TFichierATraiter;
begin
  Fichiers := TDirectory.GetFiles(Dossier, '*.*', TSearchOption.soAllDirectories);

  for Fichier in Fichiers do
  begin
    Item.CheminComplet := Fichier;
    Item.TypeTraitement := DeterminerTypeTraitement(Fichier);

    FileTraitement.PushItem(Item);
  end;

  // Signal de fin
  Item.CheminComplet := '';
  Item.TypeTraitement := -1;
  FileTraitement.PushItem(Item);
end;

// Thread de traitement
type
  TThreadTraitementFichier = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TThreadTraitementFichier.Execute;  
var  
  Item: TFichierATraiter;
begin
  while not Terminated do
  begin
    if FileTraitement.PopItem(Item, 2000) = wrSignaled then
    begin
      // Vérifier la fin
      if Item.TypeTraitement = -1 then
      begin
        // Remettre pour les autres threads
        FileTraitement.PushItem(Item);
        Break;
      end;

      // Traiter le fichier selon son type
      case Item.TypeTraitement of
        0: CompresserFichier(Item.CheminComplet);
        1: ConvertirImage(Item.CheminComplet);
        2: AnalyserDocument(Item.CheminComplet);
      end;

      // Mise à jour de l'interface
      TThread.Queue(nil,
        procedure
        begin
          Form1.ProgressBar1.Position := Form1.ProgressBar1.Position + 1;
          Form1.Label1.Caption := ExtractFileName(Item.CheminComplet);
        end
      );
    end;
  end;
end;

// Lancer le traitement
procedure TForm1.ButtonTraiterClick(Sender: TObject);  
var  
  i: Integer;
begin
  FileTraitement := TThreadedQueue<TFichierATraiter>.Create;

  // Créer 4 threads de traitement
  for i := 1 to 4 do
    TThreadTraitementFichier.Create(False);

  // Scanner et alimenter la file (dans un thread séparé)
  TTask.Run(
    procedure
    begin
      ScannerDossier('C:\Documents');
    end
  );
end;
```

## Gestion de la fermeture propre

Il est important d'arrêter proprement les threads consommateurs.

```pascal
type
  TGestionnaireFile = class
  private
    FFile: TThreadedQueue<Integer>;
    FConsommateurs: TList<TThread>;
    FActif: Boolean;
  public
    constructor Create(NbConsommateurs: Integer);
    destructor Destroy; override;
    procedure Ajouter(Valeur: Integer);
    procedure ArreterProprement;
  end;

constructor TGestionnaireFile.Create(NbConsommateurs: Integer);  
var  
  i: Integer;
begin
  FFile := TThreadedQueue<Integer>.Create;
  FConsommateurs := TList<TThread>.Create;
  FActif := True;

  // Créer les consommateurs
  for i := 1 to NbConsommateurs do
  begin
    var Thread := TThreadConsommateur.Create(FFile);
    Thread.FreeOnTerminate := False;
    FConsommateurs.Add(Thread);
  end;
end;

destructor TGestionnaireFile.Destroy;  
begin  
  ArreterProprement;
  FConsommateurs.Free;
  FFile.Free;
  inherited;
end;

procedure TGestionnaireFile.Ajouter(Valeur: Integer);  
begin  
  if FActif then
    FFile.PushItem(Valeur);
end;

procedure TGestionnaireFile.ArreterProprement;  
var  
  Thread: TThread;
begin
  FActif := False;

  // Envoyer un signal de fin pour chaque consommateur
  for Thread in FConsommateurs do
    FFile.PushItem(-1); // Signal de fin

  // Attendre que tous les threads se terminent
  for Thread in FConsommateurs do
  begin
    Thread.WaitFor;
    Thread.Free;
  end;

  FConsommateurs.Clear;
end;
```

## Bonnes pratiques

### 1. Toujours libérer les files d'attente

```pascal
var
  FileAttente: TThreadedQueue<Integer>;
begin
  FileAttente := TThreadedQueue<Integer>.Create;
  try
    // Utiliser la file
  finally
    FileAttente.Free; // Ne pas oublier !
  end;
end;
```

### 2. Utiliser des signaux de fin clairs

```pascal
const
  SIGNAL_FIN = -1; // Ou une valeur spéciale

// Dans le producteur
FileAttente.PushItem(SIGNAL_FIN);

// Dans le consommateur
if Valeur = SIGNAL_FIN then
  Break;
```

### 3. Gérer les timeouts

```pascal
// ✅ BON : Utiliser un timeout
if FileAttente.PopItem(Item, 1000) = wrSignaled then
  TraiterItem(Item);

// ❌ MAUVAIS : Attente infinie sans vérification
FileAttente.PopItem(Item); // Peut bloquer indéfiniment !
```

### 4. Dimensionner correctement le pool

```pascal
// Règle empirique : Nombre de cœurs * 2
var
  NbThreads: Integer;
begin
  NbThreads := TThread.ProcessorCount * 2;
  TThreadPool.Default.SetMaxWorkerThreads(NbThreads);
end;
```

### 5. Éviter la famine

Assurez-vous qu'aucun thread ne monopolise les ressources :

```pascal
procedure TThreadConsommateur.Execute;  
begin  
  while not Terminated do
  begin
    if FileAttente.PopItem(Item, 100) = wrSignaled then
    begin
      TraiterItem(Item);

      // Laisser respirer le système
      Sleep(10);
    end;
  end;
end;
```

## Points clés à retenir

- Les **files d'attente** permettent une communication ordonnée entre threads
- `TThreadedQueue` est thread-safe et conçue pour le multithreading
- Le pattern **producteur-consommateur** sépare la production de la consommation
- Les **pools de threads** réutilisent les threads pour de meilleures performances
- `TThreadPool` est géré automatiquement par Delphi pour TTask
- Utilisez des **signaux de fin** pour arrêter proprement les consommateurs
- Toujours utiliser des **timeouts** pour éviter les blocages
- `TThreadList` offre une alternative pour les listes non-FIFO
- Dimensionnez les pools selon le nombre de cœurs disponibles
- Nettoyez proprement les ressources à la fermeture

Les files d'attente et les pools de threads sont des outils essentiels pour créer des architectures multithreadées robustes et performantes. Ils permettent de gérer efficacement la charge de travail et d'optimiser l'utilisation des ressources système.

⏭️ [Interface utilisateur réactive](/11-multithreading-et-programmation-asynchrone/07-interface-utilisateur-reactive.md)
