# 11.10 Performances et bonnes pratiques en multithreading

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Nous avons exploré en détail comment implémenter le multithreading dans Delphi. Cependant, le simple fait d'utiliser des threads ne garantit pas de meilleures performances. En fait, une implémentation incorrecte peut conduire à des applications plus lentes, moins stables et plus difficiles à déboguer. Dans ce chapitre, nous allons examiner les bonnes pratiques pour optimiser les performances de vos applications multithreads et éviter les pièges courants.

## Optimisation du nombre de threads

### Comprendre les limites matérielles

Le nombre de cœurs du processeur est un facteur clé pour déterminer combien de threads peuvent s'exécuter véritablement en parallèle. En Delphi, vous pouvez obtenir cette information facilement :

```pascal
var
  NbCoeurs: Integer;
begin
  NbCoeurs := TThread.ProcessorCount;
  ShowMessage('Votre ordinateur possède ' + IntToStr(NbCoeurs) + ' cœurs logiques');
end;
```

### Distinguer les tâches CPU-bound et I/O-bound

- **Tâches CPU-bound** : Ces tâches utilisent intensivement le processeur (calculs complexes, traitement d'images, etc.). Pour ces tâches, le nombre optimal de threads est généralement égal au nombre de cœurs disponibles.

```pascal
// Pour les tâches CPU-bound (intensives en calcul)
var NbThreadsOptimal := TThread.ProcessorCount;
```

- **Tâches I/O-bound** : Ces tâches passent la plupart de leur temps à attendre (lecture/écriture de fichiers, requêtes réseau, etc.). Pour ces tâches, vous pouvez utiliser plus de threads que le nombre de cœurs disponibles.

```pascal
// Pour les tâches I/O-bound (attente d'entrées/sorties)
var NbThreadsOptimal := TThread.ProcessorCount * 2; // ou plus, selon les cas
```

### Éviter de créer trop de threads

La création de trop nombreux threads peut dégrader les performances en raison du surcoût des changements de contexte.

```pascal
// À éviter : création excessive de threads
for i := 1 to 1000 do
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      // Tâche très courte
      DoSmallTask;
    end
  ).Start;
end;

// Préférable : utilisation d'un pool de threads
TParallel.For(1, 1000,
  procedure(Index: Integer)
  begin
    DoSmallTask;
  end
);
```

## Réduction des contentions

### Minimiser la taille des sections critiques

Plus une section critique est longue, plus elle bloque les autres threads. Réduisez au minimum le code protégé.

```pascal
// Moins efficace
FLock.Enter;
try
  // Calcul long et complexe
  Result := CalculateComplexValue;

  // Mise à jour de la variable partagée
  FSharedVariable := Result;
finally
  FLock.Leave;
end;

// Plus efficace
// Calcul en dehors de la section critique
Result := CalculateComplexValue;

// Section critique minimale
FLock.Enter;
try
  FSharedVariable := Result;
finally
  FLock.Leave;
end;
```

### Utiliser des verrous à granularité fine

Créez des verrous distincts pour protéger différentes ressources au lieu d'un verrou global.

```pascal
// Moins efficace
type
  TDataManager = class
  private
    FGlobalLock: TCriticalSection;
    FUsers: TList<TUser>;
    FProducts: TList<TProduct>;
  end;

// Plus efficace
type
  TDataManager = class
  private
    FUserLock: TCriticalSection;
    FProductLock: TCriticalSection;
    FUsers: TList<TUser>;
    FProducts: TList<TProduct>;
  end;
```

### Utiliser des structures de données thread-safe

Delphi propose plusieurs structures de données thread-safe comme `TThreadList` et `TThreadedQueue`. Utilisez-les plutôt que de synchroniser manuellement des structures standard.

```pascal
// Moins efficace
type
  TWorkQueue = class
  private
    FQueue: TQueue<TWorkItem>;
    FLock: TCriticalSection;
  public
    procedure Enqueue(Item: TWorkItem);
    function TryDequeue(out Item: TWorkItem): Boolean;
  end;

// Plus efficace
type
  TWorkQueue = class
  private
    FQueue: TThreadedQueue<TWorkItem>;
  public
    procedure Enqueue(Item: TWorkItem);
    function TryDequeue(out Item: TWorkItem): Boolean;
  end;
```

### Réduire le partage de données entre threads

Quand c'est possible, faites en sorte que chaque thread travaille sur ses propres données et ne partage que les résultats finaux.

```pascal
// Accumulation locale puis mise à jour globale
procedure TThreadProcessor.ProcessChunk;
var
  LocalSum: Integer;
  i: Integer;
begin
  // Accumulation locale (pas de synchronisation nécessaire)
  LocalSum := 0;
  for i := FStartIndex to FEndIndex do
    LocalSum := LocalSum + ProcessItem(FData[i]);

  // Une seule synchronisation pour la mise à jour globale
  FLock.Enter;
  try
    FGlobalSum := FGlobalSum + LocalSum;
  finally
    FLock.Leave;
  end;
end;
```

## Éviter les blocages (deadlocks)

Les blocages surviennent lorsque deux threads ou plus sont bloqués indéfiniment, chacun attendant une ressource détenue par l'autre.

### Respecter un ordre d'acquisition des verrous

Toujours acquérir les verrous dans le même ordre pour éviter les blocages circulaires.

```pascal
// Risque de blocage (deadlock)
// Thread 1 :
FLockA.Enter;
// ... quelque chose se passe ...
FLockB.Enter;

// Thread 2 (simultanément) :
FLockB.Enter;
// ... quelque chose se passe ...
FLockA.Enter;  // Blocage car FLockA est détenu par Thread 1

// Solution : toujours acquérir les verrous dans le même ordre
// Dans tous les threads :
FLockA.Enter;
try
  // ...
  FLockB.Enter;
  try
    // ...
  finally
    FLockB.Leave;
  end;
finally
  FLockA.Leave;
end;
```

### Utiliser TryEnter avec timeout

Pour éviter les blocages indéfinis, utilisez `TryEnter` avec un délai d'attente :

```pascal
// Tentative d'acquisition avec timeout
if FLock.TryEnter(1000) then // 1 seconde maximum d'attente
try
  // Code protégé...
finally
  FLock.Leave;
end
else
begin
  // Gérer l'échec d'acquisition du verrou
  Log('Impossible d''acquérir le verrou après 1 seconde d''attente');
  // Alternative ou nouvelle tentative...
end;
```

### Éviter les appels entre méthodes verrouillées

Évitez d'appeler une méthode qui prend un verrou à partir d'une méthode qui détient déjà ce verrou.

```pascal
// Risque de blocage
procedure TMyClass.MethodA;
begin
  FLock.Enter;
  try
    // ...
    MethodB; // MethodB tente aussi d'acquérir FLock -> Blocage !
  finally
    FLock.Leave;
  end;
end;

procedure TMyClass.MethodB;
begin
  FLock.Enter; // Bloque car FLock est déjà détenu par MethodA
  try
    // ...
  finally
    FLock.Leave;
  end;
end;

// Solution : utiliser des méthodes internes non verrouillées
procedure TMyClass.MethodA;
begin
  FLock.Enter;
  try
    // ...
    InternalMethodB; // Version non verrouillée
  finally
    FLock.Leave;
  end;
end;

procedure TMyClass.MethodB;
begin
  FLock.Enter;
  try
    InternalMethodB;
  finally
    FLock.Leave;
  end;
end;

procedure TMyClass.InternalMethodB;
begin
  // Logique commune sans verrouillage
end;
```

## Gestion de la mémoire et des ressources

### Éviter les fuites de mémoire

Assurez-vous que tous les objets créés dans un thread sont correctement libérés, même en cas d'exception.

```pascal
procedure TMyThread.Execute;
var
  Obj: TObject;
begin
  Obj := TObject.Create;
  try
    // Utiliser Obj...

    // Si une exception se produit ici, Obj sera quand même libéré
  finally
    Obj.Free;
  end;
end;
```

### Utiliser FreeOnTerminate avec précaution

La propriété `FreeOnTerminate` permet à un thread de se libérer automatiquement quand il se termine. C'est pratique, mais cela peut compliquer la gestion des références.

```pascal
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    begin
      // Travail du thread...
    end
  );

  Thread.FreeOnTerminate := True; // Le thread se libérera automatiquement
  Thread.Start;

  // ATTENTION : Thread est maintenant potentiellement invalide !
  // N'utilisez plus Thread après cette ligne, sauf si vous savez
  // qu'il est encore en cours d'exécution
end;
```

### Partage sécurisé d'objets entre threads

Si vous devez partager des objets entre threads, assurez-vous qu'ils ne sont pas libérés tant qu'ils sont utilisés.

```pascal
type
  TSharedObjectManager = class
  private
    FObject: TObject;
    FRefCount: Integer;
    FLock: TCriticalSection;
  public
    constructor Create(AObject: TObject);
    destructor Destroy; override;
    procedure AddReference;
    procedure ReleaseReference;
    function GetObject: TObject;
  end;

procedure TSharedObjectManager.AddReference;
begin
  FLock.Enter;
  try
    Inc(FRefCount);
  finally
    FLock.Leave;
  end;
end;

procedure TSharedObjectManager.ReleaseReference;
var
  ShouldFree: Boolean;
begin
  FLock.Enter;
  try
    Dec(FRefCount);
    ShouldFree := FRefCount <= 0;
  finally
    FLock.Leave;
  end;

  if ShouldFree then
    Free; // Se libère uniquement quand toutes les références sont relâchées
end;
```

## Gestion des performances

### Éviter le thrashing

Le "thrashing" se produit lorsque le système passe plus de temps à changer de contexte entre threads qu'à effectuer un travail utile. Évitez de créer trop de threads ou de les créer/détruire fréquemment.

```pascal
// Mauvaise pratique : création/destruction fréquente de threads
procedure ProcessItems(const Items: TArray<TItem>);
var
  i: Integer;
begin
  for i := 0 to High(Items) do
  begin
    TThread.CreateAnonymousThread(
      procedure
      begin
        ProcessItem(Items[i]);
      end
    ).Start;
  end;
end;

// Bonne pratique : réutilisation des threads via un pool
var
  ThreadPool: TThreadPool;
begin
  ThreadPool := TThreadPool.Create(TThread.ProcessorCount);
  try
    // Utiliser le pool pour toutes les tâches
    for i := 0 to High(Items) do
      ThreadPool.AddTask(procedure
        begin
          ProcessItem(Items[i]);
        end);
  finally
    ThreadPool.Free;
  end;
end;
```

### Équilibrage de charge

Distribuez le travail équitablement entre les threads pour éviter que certains threads finissent trop tôt tandis que d'autres travaillent encore.

```pascal
// Mauvaise répartition
procedure ProcessData(const Data: TArray<TItem>);
var
  ThreadCount, ItemsPerThread, i: Integer;
  Threads: array of TThread;
begin
  ThreadCount := TThread.ProcessorCount;
  SetLength(Threads, ThreadCount);
  ItemsPerThread := Length(Data) div ThreadCount;

  // Chaque thread traite un bloc contigu d'éléments
  for i := 0 to ThreadCount - 1 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(
      procedure
      var
        j: Integer;
      begin
        for j := i * ItemsPerThread to (i + 1) * ItemsPerThread - 1 do
          ProcessItem(Data[j]);
      end
    );
    Threads[i].Start;
  end;

  // Attendre que tous les threads se terminent
  for i := 0 to ThreadCount - 1 do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
end;

// Meilleure répartition : entrelacement
procedure ProcessDataInterleaved(const Data: TArray<TItem>);
var
  ThreadCount, i: Integer;
  Threads: array of TThread;
begin
  ThreadCount := TThread.ProcessorCount;
  SetLength(Threads, ThreadCount);

  // Chaque thread traite des éléments répartis dans le tableau
  for i := 0 to ThreadCount - 1 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(
      procedure
      var
        j: Integer;
      begin
        // Le thread i traite les éléments i, i+ThreadCount, i+2*ThreadCount, etc.
        j := i;
        while j < Length(Data) do
        begin
          ProcessItem(Data[j]);
          Inc(j, ThreadCount); // Sauter au prochain élément pour ce thread
        end;
      end
    );
    Threads[i].Start;
  end;

  // Attendre que tous les threads se terminent
  for i := 0 to ThreadCount - 1 do
  begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
end;
```

### Utiliser TTask et TParallel quand c'est possible

Les classes `TTask` et `TParallel` de Delphi simplifient la gestion des threads et offrent de bonnes performances par défaut.

```pascal
// Utilisation de TParallel.For pour traiter des éléments en parallèle
procedure ProcessItemsParallel(const Items: TArray<TItem>);
begin
  TParallel.For(0, High(Items),
    procedure(Index: Integer)
    begin
      ProcessItem(Items[Index]);
    end
  );
end;

// Utilisation de TTask.WaitForAll pour attendre plusieurs tâches
procedure RunMultipleTasks;
var
  Tasks: array[0..2] of ITask;
begin
  Tasks[0] := TTask.Run(procedure
    begin
      Task1Work;
    end);

  Tasks[1] := TTask.Run(procedure
    begin
      Task2Work;
    end);

  Tasks[2] := TTask.Run(procedure
    begin
      Task3Work;
    end);

  // Attendre que toutes les tâches soient terminées
  TTask.WaitForAll(Tasks);
end;
```

## Débogage des applications multithreads

### Journalisation thread-safe

Implémentez un système de journalisation thread-safe pour suivre l'exécution des threads.

```pascal
type
  TThreadLogger = class
  private
    FLogFile: TextFile;
    FLock: TCriticalSection;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(const ThreadID: Integer; const Message: string);
  end;

constructor TThreadLogger.Create(const FileName: string);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  AssignFile(FLogFile, FileName);
  if FileExists(FileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TThreadLogger.Destroy;
begin
  CloseFile(FLogFile);
  FLock.Free;
  inherited;
end;

procedure TThreadLogger.Log(const ThreadID: Integer; const Message: string);
begin
  FLock.Enter;
  try
    Writeln(FLogFile, Format('[%s] Thread #%d: %s',
                           [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
                            ThreadID, Message]));
    Flush(FLogFile);
  finally
    FLock.Leave;
  end;
end;
```

### Isolation des problèmes liés aux threads

Pour isoler les problèmes liés aux threads :

1. Commencez avec un seul thread pour vérifier que la logique fonctionne correctement
2. Ajoutez des threads un par un pour identifier où les problèmes apparaissent
3. Utilisez des assertions et des vérifications d'état pour détecter les incohérences

```pascal
procedure TMyClass.ThreadSafeMethod;
begin
  FLock.Enter;
  try
    // Vérifier la cohérence des données
    Assert(FCounter >= 0, 'FCounter négatif détecté');

    // Opération thread-safe
    Inc(FCounter);

    // Nouvelle vérification
    Assert(FCounter > 0, 'FCounter incohérent après incrémentation');
  finally
    FLock.Leave;
  end;
end;
```

### Utiliser des outils de profilage

Utilisez des outils de profilage pour identifier les goulots d'étranglement dans votre application multithread. Delphi inclut des outils de profilage dans certaines éditions.

## Tester les applications multithreads

### Tests de concurrence

Testez votre application avec différents niveaux de concurrence pour vous assurer qu'elle fonctionne correctement quel que soit le nombre de threads.

```pascal
procedure TestConcurrency;
const
  DataSize = 1000;
var
  Data: TArray<Integer>;
  ExpectedSum, ActualSum: Integer;
  i, ThreadCount: Integer;
begin
  // Préparer les données
  SetLength(Data, DataSize);
  ExpectedSum := 0;
  for i := 0 to DataSize - 1 do
  begin
    Data[i] := i;
    ExpectedSum := ExpectedSum + i;
  end;

  // Tester avec différents nombres de threads
  for ThreadCount := 1 to TThread.ProcessorCount * 2 do
  begin
    ActualSum := SumArrayParallel(Data, ThreadCount);
    Assert(ActualSum = ExpectedSum,
      Format('Échec avec %d threads: %d != %d', [ThreadCount, ActualSum, ExpectedSum]));
  end;
end;
```

### Tests de stress

Soumettez votre application à des tests de stress pour détecter les problèmes qui n'apparaissent que sous charge élevée.

```pascal
procedure StressTest;
const
  IterationCount = 1000;
var
  Tasks: array of ITask;
  i: Integer;
begin
  // Créer de nombreuses tâches simultanées
  SetLength(Tasks, IterationCount);
  for i := 0 to IterationCount - 1 do
    Tasks[i] := TTask.Run(
      procedure
      begin
        TestThreadSafeOperation;
      end
    );

  // Attendre toutes les tâches
  TTask.WaitForAll(Tasks);

  // Vérifier l'état final
  VerifySystemState;
end;
```

## Bonnes pratiques générales

### Documenter les exigences de thread-safety

Indiquez clairement quelles méthodes et classes sont thread-safe dans votre documentation.

```pascal
/// <summary>
///   Traite un élément de données.
///   Cette méthode est thread-safe et peut être appelée simultanément
///   depuis plusieurs threads.
/// </summary>
procedure ProcessItem(const Item: TItem);

/// <summary>
///   Ajoute un élément à la liste.
///   ATTENTION: Cette méthode n'est PAS thread-safe et doit être
///   appelée uniquement depuis le thread principal.
/// </summary>
procedure AddItem(const Item: TItem);
```

### Encapsuler la logique de synchronisation

Encapsulez la logique de synchronisation dans des classes dédiées pour simplifier son utilisation et réduire les risques d'erreur.

```pascal
type
  TThreadSafeCounter = class
  private
    FValue: Integer;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function Increment: Integer;
    function Decrement: Integer;
    function GetValue: Integer;
  end;

constructor TThreadSafeCounter.Create;
begin
  inherited;
  FValue := 0;
  FLock := TCriticalSection.Create;
end;

destructor TThreadSafeCounter.Destroy;
begin
  FLock.Free;
  inherited;
end;

function TThreadSafeCounter.Increment: Integer;
begin
  FLock.Enter;
  try
    Inc(FValue);
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCounter.Decrement: Integer;
begin
  FLock.Enter;
  try
    Dec(FValue);
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;

function TThreadSafeCounter.GetValue: Integer;
begin
  FLock.Enter;
  try
    Result := FValue;
  finally
    FLock.Leave;
  end;
end;
```

### Éviter le verrouillage excessif

Si une ressource est rarement modifiée mais souvent lue, envisagez d'utiliser un verrou de lecture-écriture plutôt qu'une section critique.

```pascal
type
  TReadWriteLock = class
  private
    FReaderCount: Integer;
    FWriterActive: Boolean;
    FLock: TCriticalSection;
    FReadEvent: TEvent;
    FWriteEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginRead;
    procedure EndRead;
    procedure BeginWrite;
    procedure EndWrite;
  end;
```

### Tester sur différentes machines

Testez votre application sur des ordinateurs avec différents nombres de cœurs pour vous assurer qu'elle s'adapte correctement.

## Résumé

Dans ce chapitre, nous avons exploré les meilleures pratiques pour optimiser les performances et éviter les pièges dans les applications multithreads :

1. **Optimisation du nombre de threads** en fonction des caractéristiques de la tâche et du matériel disponible.
2. **Réduction des contentions** en minimisant les sections critiques et en utilisant des verrous à granularité fine.
3. **Prévention des blocages** en respectant un ordre d'acquisition cohérent des verrous et en utilisant des timeouts.
4. **Gestion sécurisée de la mémoire** pour éviter les fuites et les accès illégaux.
5. **Équilibrage de charge** pour optimiser les performances en répartissant équitablement le travail.
6. **Techniques de débogage et de test** adaptées aux applications multithreads.

En suivant ces bonnes pratiques, vous pourrez créer des applications Delphi multithreads robustes, performantes et faciles à maintenir.

## Exercice pratique

Créez une application qui simule un système de traitement de commandes avec les composants suivants :

1. Un générateur de commandes qui crée des commandes aléatoires
2. Un pool de threads "processeurs de commandes" qui traitent les commandes
3. Un système de journalisation thread-safe pour suivre toutes les opérations
4. Une interface utilisateur qui affiche en temps réel :
   - Le nombre de commandes en attente
   - Le nombre de commandes traitées
   - L'activité de chaque thread

Implémentez ce système en suivant les bonnes pratiques décrites dans ce chapitre. Testez-le avec différents nombres de threads et de commandes pour voir comment les performances évoluent.

⏭️ [Débogage et tests](/12-debogage-et-tests/README.md)
