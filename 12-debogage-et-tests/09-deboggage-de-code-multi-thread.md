# 12.9 Déboggage de code multi-thread

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction au code multi-thread

Le multithreading (ou programmation multithread) permet à une application d'exécuter plusieurs tâches simultanément, améliorant ainsi les performances et la réactivité de l'interface utilisateur. Cependant, le débogage de code multi-thread présente des défis uniques, car les problèmes peuvent être intermittents, difficiles à reproduire et souvent liés à des interactions complexes entre les threads.

Cette section vous guide à travers les techniques et outils spécifiques pour déboguer efficacement du code multi-thread dans Delphi.

## Comprendre les défis du débogage multi-thread

Avant de plonger dans les techniques de débogage, il est important de comprendre les principaux défis :

### 1. Non-déterminisme

Les applications multi-thread s'exécutent de manière non déterministe, ce qui signifie que l'ordre exact d'exécution peut varier d'une exécution à l'autre.

### 2. Problèmes de concurrence

Plusieurs types de problèmes peuvent survenir :

- **Conditions de concurrence (Race conditions)** : Lorsque le comportement du programme dépend de l'ordre d'exécution des threads
- **Interblocages (Deadlocks)** : Quand deux threads ou plus s'attendent mutuellement
- **Famine (Starvation)** : Quand un thread ne peut jamais accéder aux ressources dont il a besoin

### 3. Difficultés d'observation

Le simple fait d'observer un programme multi-thread avec un débogueur peut modifier son comportement (phénomène connu sous le nom d'effet Heisenberg).

## Outils de débogage multi-thread dans Delphi

Delphi offre plusieurs outils spécifiques pour déboguer des applications multi-thread :

### 1. Fenêtre Threads

Cette fenêtre affiche tous les threads actifs dans votre application :

1. Pendant le débogage, allez dans **View > Debug Windows > Threads** (Vue > Fenêtres de débogage > Threads)
2. La fenêtre affiche l'ID de chaque thread, son état et d'autres informations

![Fenêtre Threads dans Delphi](https://via.placeholder.com/600x300)

### 2. Pile d'appels par thread

Vous pouvez examiner la pile d'appels pour chaque thread :

1. Dans la fenêtre Threads, cliquez avec le bouton droit sur un thread
2. Sélectionnez **Show Call Stack** (Afficher la pile d'appels)

Cela vous permet de voir exactement ce que chaque thread est en train d'exécuter.

### 3. Basculer entre les threads

Pendant le débogage, vous pouvez basculer entre les threads pour inspecter leur état :

1. Dans la fenêtre Threads, double-cliquez sur un thread pour en faire le thread actif
2. L'éditeur de code affichera alors la ligne de code en cours d'exécution dans ce thread
3. Vous pouvez maintenant inspecter les variables locales pour ce thread spécifique

### 4. Points d'arrêt spécifiques aux threads

Delphi vous permet de créer des points d'arrêt qui ne s'appliquent qu'à certains threads :

1. Faites un clic droit sur un point d'arrêt existant
2. Sélectionnez **Breakpoint Properties** (Propriétés du point d'arrêt)
3. Dans l'onglet **Advanced** (Avancé), vous pouvez spécifier un ID de thread

![Configuration d'un point d'arrêt par thread](https://via.placeholder.com/500x350)

## Techniques de débogage multi-thread

### 1. Journalisation thread-safe

Une technique fondamentale consiste à intégrer une journalisation détaillée dans votre code :

```pascal
unit ThreadSafeLogger;

interface

uses
  System.Classes, System.SyncObjs, System.SysUtils;

type
  TThreadLogger = class
  private
    FLogFile: TextFile;
    FCS: TCriticalSection; // Pour thread-safety
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(const ThreadID: Integer; const Message: string);
  end;

implementation

constructor TThreadLogger.Create(const FileName: string);
begin
  inherited Create;
  FCS := TCriticalSection.Create;
  AssignFile(FLogFile, FileName);
  if FileExists(FileName) then
    Append(FLogFile)
  else
    Rewrite(FLogFile);
end;

destructor TThreadLogger.Destroy;
begin
  CloseFile(FLogFile);
  FCS.Free;
  inherited;
end;

procedure TThreadLogger.Log(const ThreadID: Integer; const Message: string);
var
  TimeStamp: string;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);

  FCS.Enter;
  try
    WriteLn(FLogFile, Format('[%s] Thread %d: %s', [TimeStamp, ThreadID, Message]));
    Flush(FLogFile); // Écrire immédiatement sur le disque
  finally
    FCS.Leave;
  end;
end;

end.
```

Utilisation dans vos threads :

```pascal
procedure TMonThread.Execute;
begin
  Logger.Log(ThreadID, 'Thread démarré');
  try
    // Code du thread...
    Logger.Log(ThreadID, Format('Traitement de l''élément %d', [ItemIndex]));

    // Plus de code...
  except
    on E: Exception do
      Logger.Log(ThreadID, 'Erreur: ' + E.Message);
  end;
  Logger.Log(ThreadID, 'Thread terminé');
end;
```

### 2. Utilisation des directives de compilation conditionnelle

Les directives de compilation peuvent vous aider à inclure du code de débogage spécifique :

```pascal
{$IFDEF DEBUG}
  Logger.Log(ThreadID, Format('Valeur de MaVariable: %d', [MaVariable]));
{$ENDIF}
```

### 3. Visualisation de l'état des threads

Créez une interface pour visualiser l'état de vos threads en temps réel :

```pascal
procedure TFormMain.UpdateThreadStatus;
begin
  // Afficher l'état de chaque thread dans un ListView
  for var i := 0 to ThreadList.Count - 1 do
  begin
    var Thread := ThreadList[i] as TMonThread;
    ListViewThreads.Items[i].SubItems[0] := Thread.Status;
    ListViewThreads.Items[i].SubItems[1] := IntToStr(Thread.ProcessedItems);
  end;
end;

// Appelez cette méthode périodiquement avec un Timer
procedure TFormMain.TimerUpdateTimer(Sender: TObject);
begin
  UpdateThreadStatus;
end;
```

## Stratégies pour déboguer des problèmes spécifiques

### 1. Conditions de concurrence (Race conditions)

Les conditions de concurrence se produisent lorsque plusieurs threads accèdent simultanément à une ressource partagée.

#### Technique de débogage :

1. **Instrumentez votre code** pour journaliser les accès aux ressources partagées
2. **Utilisez des points d'arrêt conditionnels** sur les accès critiques
3. **Ralentissez intentionnellement certains threads** pour révéler les problèmes

```pascal
// Point d'arrêt conditionnel pour détecter les accès concurrents
// Dans les propriétés du point d'arrêt, définissez la condition:
FAccessCount > 1
```

```pascal
// Code pour détecter les conditions de concurrence
var
  FAccessCount: Integer;
  FAccessCS: TCriticalSection;

procedure IncrementAccessCount;
begin
  FAccessCS.Enter;
  try
    Inc(FAccessCount);
    // Point d'arrêt ici avec condition FAccessCount > 1
    // Si déclenché, plusieurs threads accèdent simultanément

    // Code d'accès à la ressource partagée

    Dec(FAccessCount);
  finally
    FAccessCS.Leave;
  end;
end;
```

### 2. Interblocages (Deadlocks)

Un interblocage se produit lorsque deux threads ou plus s'attendent mutuellement pour libérer des ressources.

#### Technique de débogage :

1. **Utilisez la fenêtre Threads** pour identifier les threads bloqués
2. **Examinez la pile d'appels** de chaque thread bloqué
3. **Ajoutez des délais d'expiration** (timeouts) pour les opérations de verrouillage

```pascal
// Utilisation de délais d'expiration pour détecter les interblocages
if not FCriticalSection.TryEnter(1000) then // Délai d'attente de 1 seconde
begin
  Logger.Log(ThreadID, 'Possible interblocage détecté !');
  // Prendre des mesures pour récupérer ou journaliser des informations supplémentaires
end
else
begin
  try
    // Code protégé
  finally
    FCriticalSection.Leave;
  end;
end;
```

### 3. Fuites de threads

Une fuite de threads se produit lorsque les threads ne sont pas correctement terminés et libérés.

#### Technique de débogage :

1. **Surveillez le nombre de threads** dans la fenêtre Threads
2. **Implémentez un compteur de threads** dans votre application
3. **Utilisez un gestionnaire central de threads** qui suit leur création et leur destruction

```pascal
// Gestionnaire simple de suivi des threads
TThreadManager = class
private
  FThreadList: TList;
  FCS: TCriticalSection;
public
  constructor Create;
  destructor Destroy; override;
  procedure RegisterThread(Thread: TThread);
  procedure UnregisterThread(Thread: TThread);
  function GetThreadCount: Integer;
  procedure LogActiveThreads;
end;

procedure TThreadManager.LogActiveThreads;
begin
  FCS.Enter;
  try
    Logger.Log(MainThreadID, Format('Nombre de threads actifs: %d', [FThreadList.Count]));
    for var i := 0 to FThreadList.Count - 1 do
    begin
      var Thread := TThread(FThreadList[i]);
      Logger.Log(MainThreadID, Format('Thread #%d - État: %s',
        [Thread.ThreadID, GetThreadStateStr(Thread)]));
    end;
  finally
    FCS.Leave;
  end;
end;
```

## Outils avancés pour le débogage multi-thread dans Delphi 12

> 💡 **Nécessite Delphi 12 ou supérieur**

Delphi 12 Athens offre plusieurs améliorations pour le débogage multi-thread :

### 1. Visualisation des threads améliorée

L'IDE fournit une visualisation graphique des threads qui montre les relations entre eux et facilite l'identification des problèmes.

### 2. Analyse des verrous

Un nouvel outil analyse les modèles d'acquisition de verrous pour détecter les interblocages potentiels.

```pascal
// Exemple d'utilisation de l'analyse de verrous
procedure TDebugHelper.AnalyzeDeadlockPotential;
begin
  // Cette fonctionnalité est disponible via le menu Debug > Analyze Locks
  // dans Delphi 12 Athens pendant une session de débogage

  // Le code ici est simplement illustratif, car cette fonctionnalité
  // est intégrée à l'IDE et non accessible via l'API
end;
```

### 3. Points d'arrêt sur accès mémoire

Delphi 12 permet de définir des points d'arrêt qui se déclenchent lorsqu'une variable ou zone mémoire est accédée par n'importe quel thread.

```pascal
// Cette fonctionnalité est accessible via l'interface de l'IDE :
// Debug > Add Watch at Address...
// puis sélectionnez "Break on access" ou "Break on modification"
```

## Dix bonnes pratiques pour le code multi-thread facile à déboguer

### 1. Minimiser le partage d'état

Plus les threads partagent de données, plus les bugs potentiels sont nombreux. Minimisez les variables partagées.

```pascal
// Évitez ceci
var
  SharedCounter: Integer; // Variable globale partagée

// Préférez ceci
TMyThread = class(TThread)
private
  FLocalCounter: Integer; // Chaque thread a sa propre copie
end;
```

### 2. Utiliser des primitives de synchronisation appropriées

Choisissez la bonne primitive de synchronisation selon le cas d'usage :

```pascal
// Pour une protection simple
FCriticalSection := TCriticalSection.Create;

// Pour des scénarios producteur/consommateur
FEvent := TEvent.Create(nil, False, False, '');

// Pour limiter le nombre d'accès concurrents
FSemaphore := TSemaphore.Create(nil, 3, 3, ''); // Max 3 threads

// Pour la lecture parallèle, écriture exclusive
FRWLock := TMultiReadExclusiveWriteSynchronizer.Create;
```

### 3. Adopter un modèle d'accès cohérent

Adoptez un modèle cohérent pour l'accès aux ressources partagées :

```pascal
procedure SomeThreadMethod;
begin
  FCriticalSection.Enter;
  try
    // Opérations sur les ressources partagées
  finally
    FCriticalSection.Leave; // Toujours exécuté, même en cas d'exception
  end;
end;
```

### 4. Isoler le code multi-thread

Isolez le code multi-thread dans des classes ou modules dédiés pour faciliter le débogage :

```pascal
// Unité dédiée au traitement multi-thread
unit ThreadedProcessor;

interface

uses
  System.Classes, System.SyncObjs;

type
  TProcessingResult = record
    Success: Boolean;
    ResultValue: Integer;
  end;

  TThreadedProcessor = class
  private
    FThreads: array of TThread;
    FCS: TCriticalSection;
    FResults: TList<TProcessingResult>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ProcessItems(Items: TArray<TItem>);
    function WaitForCompletion(Timeout: Cardinal): Boolean;
    function GetResults: TArray<TProcessingResult>;
  end;
```

### 5. Utiliser des messages de journalisation significatifs

Incluez des informations essentielles dans vos journaux :

```pascal
Logger.Log(ThreadID, Format('[%s] Étape %d: Traitement de l''élément %d. État: %s',
  [GetCurrentMethodName, StepNumber, ItemID, StateToString]));
```

### 6. Éviter les dépendances cachées

Documentez clairement toutes les dépendances entre threads :

```pascal
// Mauvais : dépendance implicite
procedure TWorkerThread.Execute;
begin
  while not Terminated do
  begin
    if MainData <> nil then // Dépendance cachée à une variable externe
      ProcessData(MainData);
    Sleep(100);
  end;
end;

// Meilleur : dépendance explicite
procedure TWorkerThread.Execute;
begin
  while not Terminated do
  begin
    WaitForMainDataEvent.WaitFor(100); // Attente explicite d'un signal
    if WaitForMainDataEvent.WaitFor(0) = wrSignaled then
    begin
      FDataCS.Enter;
      try
        if FMainData <> nil then
          ProcessData(FMainData);
      finally
        FDataCS.Leave;
      end;
    end;
  end;
end;
```

### 7. Mettre en place des vérifications d'état

Intégrez des vérifications d'état pour détecter les problèmes tôt :

```pascal
procedure ValidateThreadState;
begin
  Assert(FInitialized, 'Thread non initialisé');
  Assert(FDataList <> nil, 'Liste de données nil');

  if not FValidState then
  begin
    Logger.Log(ThreadID, 'État invalide détecté');
    // Journaliser des informations supplémentaires sur l'état
    RaiseException('État de thread invalide');
  end;
end;
```

### 8. Utiliser des identifiants uniques pour les instances

Donnez des identifiants uniques aux objets pour suivre leur cycle de vie :

```pascal
TTrackedThread = class(TThread)
private
  FID: TGUID;
  FName: string;
public
  constructor Create(const Name: string);
  property ID: TGUID read FID;
  property Name: string read FName;
end;

constructor TTrackedThread.Create(const Name: string);
begin
  inherited Create(True); // Créé suspendu
  CreateGUID(FID); // Génère un ID unique
  FName := Name;
  Logger.Log(0, Format('Thread "%s" (ID: %s) créé', [FName, GUIDToString(FID)]));
end;
```

### 9. Mettre en œuvre un mécanisme de timeout

Ajoutez des timeouts pour éviter les blocages indéfinis :

```pascal
function TryPerformOperation(Timeout: Cardinal): Boolean;
var
  StartTime: Cardinal;
begin
  StartTime := GetTickCount;
  Result := False;

  while (GetTickCount - StartTime < Timeout) and not Result do
  begin
    // Tentative d'opération
    Result := TryAcquireResource;

    if not Result then
      Sleep(10); // Pause courte avant nouvelle tentative
  end;

  if not Result then
    Logger.Log(ThreadID, Format('Timeout après %d ms', [Timeout]));
end;
```

### 10. Utiliser des classes d'assistance pour le débogage

Créez des classes spécifiques pour faciliter le débogage :

```pascal
TThreadDebugHelper = class
public
  class procedure DumpThreadInfo(Thread: TThread; const Logger: ILogger);
  class procedure TrackSynchronization(const ResourceName: string; Entering: Boolean);
  class procedure DetectLongOperations(StartTime: Cardinal; Threshold: Cardinal);
end;

class procedure TThreadDebugHelper.DumpThreadInfo(Thread: TThread; const Logger: ILogger);
begin
  Logger.Log(Thread.ThreadID, Format(
    'Infos thread - ID: %d, Priorité: %d, État: %s, Suspendu: %s',
    [Thread.ThreadID, Ord(Thread.Priority),
     GetThreadStateStr(Thread), BoolToStr(Thread.Suspended, True)]));
end;
```

## Exemple complet : débogage d'un pool de threads

Voici un exemple complet illustrant la création d'un pool de threads avec des fonctionnalités de débogage intégrées :

```pascal
unit ThreadPool;

interface

uses
  System.Classes, System.SyncObjs, System.Generics.Collections,
  System.SysUtils;

type
  TWorkItem = class
  private
    FID: Integer;
    FData: TObject;
    FCreatedTime: TDateTime;
  public
    constructor Create(ID: Integer; Data: TObject);
    property ID: Integer read FID;
    property Data: TObject read FData;
    property CreatedTime: TDateTime read FCreatedTime;
  end;

  TWorkerThread = class(TThread)
  private
    FPool: TObject; // Référence au ThreadPool (évite la référence circulaire)
    FThreadID: Cardinal;
    FState: string;
    FCurrentWorkItem: TWorkItem;
    FProcessedCount: Integer;
    FLastErrorMessage: string;
    procedure SetState(const Value: string);
  protected
    procedure Execute; override;
  public
    constructor Create(Pool: TObject);
    property State: string read FState write SetState;
    property ProcessedCount: Integer read FProcessedCount;
    property LastErrorMessage: string read FLastErrorMessage;
    property CurrentWorkItem: TWorkItem read FCurrentWorkItem;
  end;

  TThreadPool = class
  private
    FWorkers: TObjectList<TWorkerThread>;
    FWorkItems: TThreadedQueue<TWorkItem>;
    FLogger: TThreadLogger;
    FCS: TCriticalSection;
    FMaxThreads: Integer;
    FLogFilePath: string;
  public
    constructor Create(MaxThreads: Integer; const LogFilePath: string);
    destructor Destroy; override;
    procedure AddWorkItem(Data: TObject);
    function GetWorkerStatus: TArray<string>;
    procedure DumpPoolStatus;
    property Logger: TThreadLogger read FLogger;
  end;

implementation

{ TWorkItem }

constructor TWorkItem.Create(ID: Integer; Data: TObject);
begin
  FID := ID;
  FData := Data;
  FCreatedTime := Now;
end;

{ TWorkerThread }

constructor TWorkerThread.Create(Pool: TObject);
begin
  inherited Create(True);
  FPool := Pool;
  FThreadID := 0; // Sera défini durant l'exécution
  FState := 'Créé';
  FProcessedCount := 0;
  FreeOnTerminate := False;
end;

procedure TWorkerThread.SetState(const Value: string);
var
  ThreadPool: TThreadPool;
begin
  if FState <> Value then
  begin
    FState := Value;
    ThreadPool := TThreadPool(FPool);
    ThreadPool.Logger.Log(ThreadID, Format('État: %s', [FState]));
  end;
end;

procedure TWorkerThread.Execute;
var
  ThreadPool: TThreadPool;
  WorkItem: TWorkItem;
begin
  ThreadPool := TThreadPool(FPool);
  FThreadID := ThreadID;

  State := 'Démarré';

  try
    while not Terminated do
    begin
      State := 'En attente';

      // Attendre un élément de travail
      if ThreadPool.FWorkItems.PopItem(WorkItem, 1000) then
      begin
        try
          FCurrentWorkItem := WorkItem;

          State := Format('Traitement de l''élément #%d', [WorkItem.ID]);
          ThreadPool.Logger.Log(ThreadID, Format(
            'Début du traitement de l''élément #%d (Temps d''attente: %s)',
            [WorkItem.ID, FormatDateTime('nn:ss.zzz',
             Now - WorkItem.CreatedTime)]));

          // Simuler un traitement
          Sleep(Random(1000));

          // Simuler occasionnellement une exception pour démonstration
          if Random(10) = 0 then
            raise Exception.Create('Erreur simulée');

          Inc(FProcessedCount);
          ThreadPool.Logger.Log(ThreadID, Format(
            'Fin du traitement de l''élément #%d. Total traité: %d',
            [WorkItem.ID, FProcessedCount]));
        except
          on E: Exception do
          begin
            FLastErrorMessage := E.Message;
            State := Format('Erreur: %s', [E.Message]);
            ThreadPool.Logger.Log(ThreadID, Format(
              'Exception durant le traitement de l''élément #%d: %s',
              [WorkItem.ID, E.Message]));
          end;
        end;

        // Libérer l'élément de travail
        WorkItem.Free;
        FCurrentWorkItem := nil;
      end;
    end;
  finally
    State := 'Terminé';
    ThreadPool.Logger.Log(ThreadID, 'Thread terminé');
  end;
end;

{ TThreadPool }

constructor TThreadPool.Create(MaxThreads: Integer; const LogFilePath: string);
var
  i: Integer;
begin
  FMaxThreads := MaxThreads;
  FLogFilePath := LogFilePath;

  FLogger := TThreadLogger.Create(LogFilePath);
  FWorkItems := TThreadedQueue<TWorkItem>.Create(1000, 10, 10);
  FCS := TCriticalSection.Create;

  FLogger.Log(MainThreadID, Format('Pool de threads créé avec %d threads', [MaxThreads]));

  FWorkers := TObjectList<TWorkerThread>.Create(True); // Owns objects

  // Créer les threads de travail
  for i := 1 to MaxThreads do
  begin
    var Worker := TWorkerThread.Create(Self);
    FWorkers.Add(Worker);
  end;

  // Démarrer les threads
  for i := 0 to FWorkers.Count - 1 do
    FWorkers[i].Start;
end;

destructor TThreadPool.Destroy;
var
  i: Integer;
begin
  // Signaler à tous les threads de terminer
  for i := 0 to FWorkers.Count - 1 do
    FWorkers[i].Terminate;

  // Attendre que tous les threads se terminent
  for i := 0 to FWorkers.Count - 1 do
  begin
    if not FWorkers[i].Finished then
    begin
      FLogger.Log(MainThreadID, Format('En attente de la fin du thread #%d',
        [FWorkers[i].ThreadID]));
      FWorkers[i].WaitFor;
    end;
  end;

  FLogger.Log(MainThreadID, 'Tous les threads terminés');

  // Libérer les ressources
  FWorkers.Free;
  FWorkItems.Free;
  FCS.Free;
  FLogger.Free;

  inherited;
end;

procedure TThreadPool.AddWorkItem(Data: TObject);
var
  WorkItem: TWorkItem;
  ItemID: Integer;
begin
  FCS.Enter;
  try
    // Générer un ID unique pour ce travail
    ItemID := FWorkItems.Count + 1;
  finally
    FCS.Leave;
  end;

  WorkItem := TWorkItem.Create(ItemID, Data);

  FLogger.Log(MainThreadID, Format('Ajout de l''élément #%d à la file', [ItemID]));

  // Ajouter l'élément à la file d'attente
  FWorkItems.PushItem(WorkItem);
end;

function TThreadPool.GetWorkerStatus: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, FWorkers.Count);

  FCS.Enter;
  try
    for i := 0 to FWorkers.Count - 1 do
    begin
      var Worker := FWorkers[i];
      Result[i] := Format('Thread #%d: %s, Traités: %d, %s',
        [Worker.ThreadID, Worker.State, Worker.ProcessedCount,
         IfThen(Worker.CurrentWorkItem <> nil,
           Format('Traite l''élément #%d', [Worker.CurrentWorkItem.ID]),
           'Inactif')]);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TThreadPool.DumpPoolStatus;
var
  StatusLines: TArray<string>;
  i: Integer;
begin
  FLogger.Log(MainThreadID, '--- ÉTAT DU POOL DE THREADS ---');
  FLogger.Log(MainThreadID, Format('Éléments en attente: %d', [FWorkItems.Count]));

  StatusLines := GetWorkerStatus;
  for i := 0 to Length(StatusLines) - 1 do
    FLogger.Log(MainThreadID, StatusLines[i]);

  FLogger.Log(MainThreadID, '-------------------------------');
end;

end.
```

### Utilisation du pool de threads avec débogage

```pascal
procedure TFormMain.btnStartClick(Sender: TObject);
var
  i: Integer;
begin
  // Créer le pool avec 5 threads
  FThreadPool := TThreadPool.Create(5, 'thread_pool_log.txt');

  // Ajouter quelques éléments de travail
  for i := 1 to 20 do
    FThreadPool.AddWorkItem(TObject.Create); // Objet simple pour l'exemple

  // Configurer un timer pour surveiller l'état
  TimerStatus.Enabled := True;
end;

procedure TFormMain.TimerStatusTimer(Sender: TObject);
var
  StatusLines: TArray<string>;
  i: Integer;
begin
  if FThreadPool = nil then
    Exit;

  // Obtenir et afficher le statut actuel
  StatusLines := FThreadPool.GetWorkerStatus;

  MemoStatus.Lines.Clear;
  MemoStatus.Lines.Add(Format('Surveillance du pool de threads - %s',
    [FormatDateTime('hh:nn:ss', Now)]));
  MemoStatus.Lines.Add('----------------------------------');

  for i := 0 to Length(StatusLines) - 1 do
    MemoStatus.Lines.Add(StatusLines[i]);

  // Toutes les 10 secondes, faire un dump complet dans le journal
  if (Second(Now) mod 10) = 0 then
    FThreadPool.DumpPoolStatus;
end;
```

## Conclusion

Le débogage de code multi-thread est une compétence avancée mais essentielle pour les développeurs Delphi modernes. En combinant les outils spécifiques de Delphi avec des techniques de journalisation appropriées et en suivant les bonnes pratiques de conception, vous pouvez identifier et résoudre efficacement les problèmes même dans les applications multi-thread les plus complexes.

Souvenez-vous que la meilleure approche est souvent préventive : une bonne conception du code multi-thread peut éliminer de nombreux problèmes avant même qu'ils n'apparaissent. Utilisez les primitives de synchronisation appropriées, minimisez les données partagées et documentez clairement les interactions entre threads.

Dans la prochaine section, nous explorerons la couverture de code et les techniques pour mesurer et améliorer la qualité de votre code Delphi.

⏭️ [Couverture de code et qualité](/12-debogage-et-tests/10-couverture-de-code-et-qualite.md)
