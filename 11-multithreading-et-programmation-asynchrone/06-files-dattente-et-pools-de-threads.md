# 11.6 Files d'attente et pools de threads

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Dans les sections précédentes, nous avons exploré comment créer et gérer des threads individuels ainsi que des tâches asynchrones. Dans ce chapitre, nous allons découvrir des techniques plus avancées pour gérer efficacement de nombreuses tâches : les files d'attente (queues) et les pools de threads.

Ces concepts sont particulièrement utiles lorsque votre application doit traiter un grand nombre de tâches, potentiellement plus nombreuses que le nombre de cœurs disponibles sur le processeur. Une bonne gestion des files d'attente et des pools de threads vous permettra d'optimiser les performances de votre application.

## Files d'attente de tâches

### Qu'est-ce qu'une file d'attente de tâches ?

Une file d'attente de tâches est une structure qui stocke des tâches à exécuter. Ces tâches sont généralement traitées selon le principe "premier entré, premier sorti" (FIFO - First In, First Out). La file d'attente permet de :

- Contrôler le flux de travail
- Éviter de surcharger le système
- Organiser l'exécution des tâches par priorité si nécessaire

### Implémentation d'une file d'attente simple

Voici comment implémenter une file d'attente de tâches simple en Delphi :

```pascal
type
  TTâche = procedure of object;

  TFileAttenteTâches = class
  private
    FTâches: TQueue<TTâche>;
    FVerrou: TCriticalSection;
    FThreadTraitement: TThread;
    FStopper: Boolean;
    procedure ThreadTraitementExécute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterTâche(const Tâche: TTâche);
    procedure Démarrer;
    procedure Arrêter;
  end;

constructor TFileAttenteTâches.Create;
begin
  inherited Create;
  FTâches := TQueue<TTâche>.Create;
  FVerrou := TCriticalSection.Create;
  FStopper := False;
end;

destructor TFileAttenteTâches.Destroy;
begin
  Arrêter;
  FTâches.Free;
  FVerrou.Free;
  inherited;
end;

procedure TFileAttenteTâches.AjouterTâche(const Tâche: TTâche);
begin
  FVerrou.Enter;
  try
    FTâches.Enqueue(Tâche);
  finally
    FVerrou.Leave;
  end;
end;

procedure TFileAttenteTâches.Démarrer;
begin
  FStopper := False;
  FThreadTraitement := TThread.CreateAnonymousThread(ThreadTraitementExécute);
  FThreadTraitement.FreeOnTerminate := False;
  FThreadTraitement.Start;
end;

procedure TFileAttenteTâches.Arrêter;
begin
  if FThreadTraitement <> nil then
  begin
    FStopper := True;
    FThreadTraitement.WaitFor;
    FreeAndNil(FThreadTraitement);
  end;
end;

procedure TFileAttenteTâches.ThreadTraitementExécute;
var
  Tâche: TTâche;
  ATâcheDisponible: Boolean;
begin
  while not FStopper do
  begin
    // Vérifier s'il y a des tâches à traiter
    FVerrou.Enter;
    try
      ATâcheDisponible := FTâches.Count > 0;
      if ATâcheDisponible then
        Tâche := FTâches.Dequeue;
    finally
      FVerrou.Leave;
    end;

    // Exécuter la tâche si disponible
    if ATâcheDisponible then
      Tâche
    else
      Sleep(100); // Pause si aucune tâche n'est disponible
  end;
end;
```

### Utilisation de la file d'attente

```pascal
var
  FileAttente: TFileAttenteTâches;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FileAttente := TFileAttenteTâches.Create;
  FileAttente.Démarrer;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FileAttente.Free;
end;

procedure TForm1.ButtonAddTaskClick(Sender: TObject);
begin
  // Ajouter une tâche à la file d'attente
  FileAttente.AjouterTâche(
    procedure
    begin
      // Simuler un traitement long
      Sleep(2000);

      // Mettre à jour l'interface utilisateur
      TThread.Synchronize(nil,
        procedure
        begin
          Memo1.Lines.Add('Tâche exécutée à ' + TimeToStr(Now));
        end
      );
    end
  );

  Memo1.Lines.Add('Tâche ajoutée à ' + TimeToStr(Now));
end;
```

Cette implémentation simple utilise un thread unique pour traiter les tâches dans la file d'attente. Cependant, dans la pratique, il est souvent plus efficace d'utiliser plusieurs threads pour traiter la file d'attente, ce qui nous amène au concept de pool de threads.

## Pools de threads

### Qu'est-ce qu'un pool de threads ?

Un pool de threads est un groupe de threads qui attendent et traitent des tâches. L'idée principale est de réutiliser les threads plutôt que d'en créer et détruire constamment, ce qui est coûteux en termes de performances. Les avantages des pools de threads sont :

- Réduction des coûts de création/destruction de threads
- Limitation du nombre total de threads actifs
- Meilleure utilisation des ressources du système
- Possibilité d'ajuster le nombre de threads en fonction de la charge

### Utilisation du pool de threads intégré à Delphi

Delphi dispose d'un pool de threads intégré, accessible via la classe `TThreadPool`. Ce pool est utilisé automatiquement lorsque vous utilisez `TTask.Run` ou les autres méthodes de `TTask`.

```pascal
uses
  System.Threading;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  i: Integer;
begin
  // Configurer le pool de threads
  TThreadPool.Default.MaxWorkerThreads := 4; // Limiter à 4 threads

  // Soumettre 10 tâches au pool
  for i := 1 to 10 do
  begin
    TTask.Run(
      procedure
      var
        TaskNum: Integer;
      begin
        TaskNum := i; // Capturer la valeur de i

        // Simuler un traitement
        Sleep(2000);

        // Mettre à jour l'interface utilisateur
        TThread.Synchronize(nil,
          procedure
          begin
            Memo1.Lines.Add('Tâche ' + IntToStr(TaskNum) + ' terminée');
          end
        );
      end
    );
  end;
end;
```

Dans cet exemple, même si nous démarrons 10 tâches, seuls 4 threads maximum seront actifs simultanément (selon la configuration du pool). Les autres tâches attendront qu'un thread se libère.

### Implémentation personnalisée d'un pool de threads

Bien que le pool intégré soit suffisant pour la plupart des cas, vous pourriez avoir besoin de fonctionnalités spécifiques. Voici une implémentation simplifiée d'un pool de threads personnalisé :

```pascal
type
  TTâche = procedure of object;

  TPoolThreads = class
  private
    FTâches: TThreadedQueue<TTâche>;
    FThreads: array of TThread;
    FNombreThreads: Integer;
    FArrêter: Boolean;
    procedure ExécutionThread(Index: Integer);
  public
    constructor Create(NombreThreads: Integer);
    destructor Destroy; override;
    procedure AjouterTâche(const Tâche: TTâche);
    function NombreTâchesEnAttente: Integer;
  end;

constructor TPoolThreads.Create(NombreThreads: Integer);
var
  i: Integer;
begin
  inherited Create;

  FNombreThreads := NombreThreads;
  FTâches := TThreadedQueue<TTâche>.Create(10000, INFINITE, 10); // Capacité, timeout, spin count
  FArrêter := False;

  // Créer les threads du pool
  SetLength(FThreads, FNombreThreads);
  for i := 0 to FNombreThreads - 1 do
  begin
    FThreads[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        ExécutionThread(i);
      end
    );
    FThreads[i].FreeOnTerminate := False;
    FThreads[i].Start;
  end;
end;

destructor TPoolThreads.Destroy;
var
  i: Integer;
  DummyTâche: TTâche;
begin
  // Signaler aux threads de s'arrêter
  FArrêter := True;

  // Ajouter des tâches vides pour débloquer les threads en attente
  for i := 0 to FNombreThreads - 1 do
    FTâches.PushItem(DummyTâche);

  // Attendre que tous les threads se terminent
  for i := 0 to FNombreThreads - 1 do
  begin
    FThreads[i].WaitFor;
    FThreads[i].Free;
  end;

  FTâches.Free;
  inherited;
end;

procedure TPoolThreads.AjouterTâche(const Tâche: TTâche);
begin
  FTâches.PushItem(Tâche);
end;

function TPoolThreads.NombreTâchesEnAttente: Integer;
begin
  Result := FTâches.QueueSize;
end;

procedure TPoolThreads.ExécutionThread(Index: Integer);
var
  Tâche: TTâche;
begin
  // Boucle principale du thread
  while not FArrêter do
  begin
    // Attendre et récupérer une tâche
    if FTâches.PopItem(Tâche) = wrSignaled then
    begin
      // Si le pool est en train d'être détruit, ignorer la tâche
      if FArrêter then
        Break;

      // Exécuter la tâche
      if Assigned(Tâche) then
      begin
        try
          Tâche;
        except
          // Gérer les exceptions pour éviter que le thread ne se termine
          // Dans une application réelle, vous voudriez journaliser l'erreur
        end;
      end;
    end;
  end;
end;
```

Cette implémentation utilise `TThreadedQueue`, une file d'attente thread-safe intégrée à Delphi qui gère automatiquement la synchronisation.

### Utilisation du pool personnalisé

```pascal
var
  Pool: TPoolThreads;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer un pool avec un nombre de threads égal au nombre de cœurs
  Pool := TPoolThreads.Create(TThread.ProcessorCount);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Pool.Free;
end;

procedure TForm1.ButtonAddTasksClick(Sender: TObject);
var
  i: Integer;
begin
  // Ajouter 20 tâches au pool
  for i := 1 to 20 do
  begin
    Pool.AjouterTâche(
      procedure
      var
        TaskNum: Integer;
      begin
        TaskNum := i; // Capturer la valeur actuelle de i

        // Simuler un traitement
        Sleep(Random(3000));

        // Mettre à jour l'interface utilisateur
        TThread.Synchronize(nil,
          procedure
          begin
            Memo1.Lines.Add('Tâche ' + IntToStr(TaskNum) + ' terminée');
            Label1.Caption := 'Tâches en attente : ' + IntToStr(Pool.NombreTâchesEnAttente);
          end
        );
      end
    );
  end;

  Label1.Caption := 'Tâches en attente : ' + IntToStr(Pool.NombreTâchesEnAttente);
end;
```

## Files d'attente avec priorité

Dans certains cas, vous pourriez avoir besoin de traiter certaines tâches avant d'autres, même si elles ont été ajoutées plus tard. Pour cela, vous pouvez implémenter une file d'attente avec priorité.

### Implémentation d'une file d'attente avec priorité

```pascal
type
  TPriorité = (pBasse, pNormale, pHaute, pUrgente);

  TTâcheAvecPriorité = record
    Tâche: TTâche;
    Priorité: TPriorité;
  end;

  TComparateurTâches = class(TComparer<TTâcheAvecPriorité>)
  public
    function Compare(const Left, Right: TTâcheAvecPriorité): Integer; override;
  end;

  TFileAttentePrioritaire = class
  private
    FTâches: TPriorityQueue<TTâcheAvecPriorité>;
    FVerrou: TCriticalSection;
    FThreadTraitement: TThread;
    FStopper: Boolean;
    procedure ThreadTraitementExécute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AjouterTâche(const Tâche: TTâche; Priorité: TPriorité = pNormale);
    procedure Démarrer;
    procedure Arrêter;
  end;

function TComparateurTâches.Compare(const Left, Right: TTâcheAvecPriorité): Integer;
begin
  // Comparer en fonction de la priorité (plus haute priorité = plus petite valeur dans la file)
  Result := Ord(Right.Priorité) - Ord(Left.Priorité);
end;

constructor TFileAttentePrioritaire.Create;
begin
  inherited Create;
  FTâches := TPriorityQueue<TTâcheAvecPriorité>.Create(TComparateurTâches.Create);
  FVerrou := TCriticalSection.Create;
  FStopper := False;
end;

// ... autres méthodes similaires à TFileAttenteTâches ...

procedure TFileAttentePrioritaire.AjouterTâche(const Tâche: TTâche; Priorité: TPriorité = pNormale);
var
  TâcheP: TTâcheAvecPriorité;
begin
  TâcheP.Tâche := Tâche;
  TâcheP.Priorité := Priorité;

  FVerrou.Enter;
  try
    FTâches.Enqueue(TâcheP);
  finally
    FVerrou.Leave;
  end;
end;

procedure TFileAttentePrioritaire.ThreadTraitementExécute;
var
  TâcheP: TTâcheAvecPriorité;
  ATâcheDisponible: Boolean;
begin
  while not FStopper do
  begin
    // Vérifier s'il y a des tâches à traiter
    FVerrou.Enter;
    try
      ATâcheDisponible := FTâches.Count > 0;
      if ATâcheDisponible then
        TâcheP := FTâches.Dequeue;
    finally
      FVerrou.Leave;
    end;

    // Exécuter la tâche si disponible
    if ATâcheDisponible then
      TâcheP.Tâche
    else
      Sleep(100); // Pause si aucune tâche n'est disponible
  end;
end;
```

### Utilisation de la file d'attente prioritaire

```pascal
procedure TForm1.ButtonHighPriorityClick(Sender: TObject);
begin
  FileAttentePrio.AjouterTâche(
    procedure
    begin
      // Code de la tâche
      Sleep(2000);

      TThread.Synchronize(nil,
        procedure
        begin
          Memo1.Lines.Add('Tâche HAUTE PRIORITÉ terminée');
        end
      );
    end,
    pHaute // Priorité haute
  );

  Memo1.Lines.Add('Tâche de haute priorité ajoutée');
end;

procedure TForm1.ButtonNormalPriorityClick(Sender: TObject);
begin
  FileAttentePrio.AjouterTâche(
    procedure
    begin
      // Code de la tâche
      Sleep(2000);

      TThread.Synchronize(nil,
        procedure
        begin
          Memo1.Lines.Add('Tâche priorité normale terminée');
        end
      );
    end
    // La priorité normale est la valeur par défaut
  );

  Memo1.Lines.Add('Tâche de priorité normale ajoutée');
end;
```

## Files d'attente spécialisées en Delphi

Delphi propose plusieurs classes de files d'attente thread-safe que vous pouvez utiliser :

### TThreadedQueue<T>

`TThreadedQueue<T>` est une file d'attente thread-safe qui permet aux threads de se bloquer en attendant que des éléments soient disponibles.

```pascal
var
  FileAttente: TThreadedQueue<string>;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer une file d'attente avec une capacité de 100 éléments
  FileAttente := TThreadedQueue<string>.Create(100, INFINITE);
end;

procedure TForm1.ButtonProducerClick(Sender: TObject);
begin
  // Ajouter un élément à la file d'attente
  FileAttente.PushItem('Élément ' + IntToStr(Random(1000)));
  Memo1.Lines.Add('Élément ajouté à ' + TimeToStr(Now));
end;

procedure TForm1.ButtonConsumerClick(Sender: TObject);
begin
  // Démarrer un thread consommateur
  TThread.CreateAnonymousThread(
    procedure
    var
      Item: string;
    begin
      // Attendre et récupérer un élément (bloque jusqu'à ce qu'un élément soit disponible)
      if FileAttente.PopItem(Item) = wrSignaled then
      begin
        TThread.Synchronize(nil,
          procedure
          begin
            Memo1.Lines.Add('Élément traité : ' + Item + ' à ' + TimeToStr(Now));
          end
        );
      end;
    end
  ).Start;
end;
```

### TThreadedQueue vs TQueue

- `TQueue<T>` est une file d'attente standard, mais elle n'est pas thread-safe.
- `TThreadedQueue<T>` est thread-safe et offre des fonctionnalités de blocage et de timeout.

Utilisez `TThreadedQueue<T>` lorsque vous avez besoin d'une file d'attente partagée entre plusieurs threads. Elle gère automatiquement la synchronisation pour vous.

## Modèle producteur-consommateur avec pool de threads

Le modèle producteur-consommateur est un pattern courant qui utilise des files d'attente et des pools de threads. Dans ce modèle :

1. Des "producteurs" créent des tâches ou des données
2. Ces tâches sont placées dans une file d'attente
3. Des "consommateurs" (généralement un pool de threads) traitent ces tâches

Voici une implémentation complète de ce modèle :

```pascal
type
  TÉlément = record
    ID: Integer;
    Données: string;
  end;

  TModèleProducteurConsommateur = class
  private
    FFileAttente: TThreadedQueue<TÉlément>;
    FThreadsConsommateurs: array of TThread;
    FNombreConsommateurs: Integer;
    FArrêter: Boolean;
    FÉlémentTraités: Integer;
    FVerrouCompteur: TCriticalSection;
    procedure ConsommateurExécute(IndexThread: Integer);
  public
    constructor Create(NombreConsommateurs: Integer);
    destructor Destroy; override;
    procedure Produire(const Élément: TÉlément);
    procedure DémarrerProduction(NombreÉléments: Integer);
    function ÉlémentsTraités: Integer;
  end;

constructor TModèleProducteurConsommateur.Create(NombreConsommateurs: Integer);
var
  i: Integer;
begin
  inherited Create;

  FNombreConsommateurs := NombreConsommateurs;
  FFileAttente := TThreadedQueue<TÉlément>.Create(1000, INFINITE);
  FVerrouCompteur := TCriticalSection.Create;
  FÉlémentTraités := 0;
  FArrêter := False;

  // Créer et démarrer les threads consommateurs
  SetLength(FThreadsConsommateurs, FNombreConsommateurs);
  for i := 0 to FNombreConsommateurs - 1 do
  begin
    FThreadsConsommateurs[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        ConsommateurExécute(i);
      end
    );
    FThreadsConsommateurs[i].FreeOnTerminate := False;
    FThreadsConsommateurs[i].Start;
  end;
end;

destructor TModèleProducteurConsommateur.Destroy;
var
  i: Integer;
  ÉlémentVide: TÉlément;
begin
  // Signaler aux threads de s'arrêter
  FArrêter := True;

  // Ajouter des éléments vides pour débloquer les threads en attente
  for i := 0 to FNombreConsommateurs - 1 do
    FFileAttente.PushItem(ÉlémentVide);

  // Attendre que tous les threads se terminent
  for i := 0 to FNombreConsommateurs - 1 do
  begin
    FThreadsConsommateurs[i].WaitFor;
    FThreadsConsommateurs[i].Free;
  end;

  FFileAttente.Free;
  FVerrouCompteur.Free;
  inherited;
end;

procedure TModèleProducteurConsommateur.Produire(const Élément: TÉlément);
begin
  FFileAttente.PushItem(Élément);
end;

procedure TModèleProducteurConsommateur.DémarrerProduction(NombreÉléments: Integer);
begin
  // Démarrer un thread producteur
  TThread.CreateAnonymousThread(
    procedure
    var
      i: Integer;
      Élément: TÉlément;
    begin
      for i := 1 to NombreÉléments do
      begin
        // Créer un élément
        Élément.ID := i;
        Élément.Données := 'Données ' + IntToStr(i);

        // Ajouter à la file d'attente
        Produire(Élément);

        // Simuler un temps de production variable
        Sleep(Random(100));

        // Mettre à jour l'interface utilisateur
        TThread.Queue(nil,
          procedure
          begin
            Form1.ProgressProduction.Position := (i * 100) div NombreÉléments;
            Form1.LabelProduction.Caption := Format('Production : %d/%d', [i, NombreÉléments]);
          end
        );
      end;
    end
  ).Start;
end;

function TModèleProducteurConsommateur.ÉlémentsTraités: Integer;
begin
  FVerrouCompteur.Enter;
  try
    Result := FÉlémentTraités;
  finally
    FVerrouCompteur.Leave;
  end;
end;

procedure TModèleProducteurConsommateur.ConsommateurExécute(IndexThread: Integer);
var
  Élément: TÉlément;
  Résultat: TWaitResult;
begin
  while not FArrêter do
  begin
    // Attendre et récupérer un élément
    Résultat := FFileAttente.PopItem(Élément);

    if (Résultat = wrSignaled) and (not FArrêter) and (Élément.ID > 0) then
    begin
      // Traiter l'élément (simuler un traitement)
      Sleep(Random(500));

      // Incrémenter le compteur d'éléments traités
      FVerrouCompteur.Enter;
      try
        Inc(FÉlémentTraités);
      finally
        FVerrouCompteur.Leave;
      end;

      // Mettre à jour l'interface utilisateur (pas trop fréquemment)
      if Élément.ID mod 10 = 0 then
      begin
        TThread.Queue(nil,
          procedure
          begin
            Form1.ProgressConsommation.Position := ÉlémentsTraités;
            Form1.LabelConsommation.Caption := 'Éléments traités : ' + IntToStr(ÉlémentsTraités);
          end
        );
      end;
    end;
  end;
end;
```

### Utilisation du modèle producteur-consommateur

```pascal
var
  Modèle: TModèleProducteurConsommateur;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer le modèle avec un nombre de threads égal au nombre de cœurs
  Modèle := TModèleProducteurConsommateur.Create(TThread.ProcessorCount);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Modèle.Free;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
const
  NOMBRE_ÉLÉMENTS = 1000;
begin
  // Configurer les barres de progression
  ProgressProduction.Max := 100;
  ProgressProduction.Position := 0;
  ProgressConsommation.Max := NOMBRE_ÉLÉMENTS;
  ProgressConsommation.Position := 0;

  // Démarrer la production
  Modèle.DémarrerProduction(NOMBRE_ÉLÉMENTS);
end;
```

## Bonnes pratiques

### Optimisation du nombre de threads

Le nombre optimal de threads dépend de plusieurs facteurs :

- **Type de tâches** : Pour les tâches limitées par le CPU, un nombre de threads proche du nombre de cœurs est optimal. Pour les tâches d'I/O, un nombre plus élevé peut être bénéfique.
- **Ressources disponibles** : Chaque thread consomme de la mémoire et d'autres ressources.
- **Coût des changements de contexte** : Trop de threads peuvent entraîner une surcharge due aux changements de contexte fréquents.

Une règle empirique pour les tâches CPU-bound :
```pascal
NombreThreadsOptimal := TThread.ProcessorCount;
```

Pour les tâches I/O-bound (comme les téléchargements) :
```pascal
NombreThreadsOptimal := TThread.ProcessorCount * 2;
```

### Surveillance de la profondeur de la file d'attente

Surveillez la taille de votre file d'attente pour détecter les problèmes de performance :

- **File d'attente qui grandit continuellement** : Les producteurs sont plus rapides que les consommateurs, ce qui peut indiquer un besoin de plus de threads consommateurs.
- **File d'attente toujours vide** : Les consommateurs sont plus rapides que les producteurs, ce qui peut indiquer un surdimensionnement du pool de threads.

### Taille de file d'attente limitée

Considérez l'utilisation d'une file d'attente de taille limitée pour éviter une utilisation excessive de la mémoire, surtout si les producteurs peuvent être beaucoup plus rapides que les consommateurs.

### Gestion des erreurs

Assurez-vous que les exceptions dans les threads de travail sont correctement gérées pour éviter qu'un thread ne se termine de manière inattendue :

```pascal
try
  // Code qui peut générer une exception
  ProcessTask(Task);
except
  on E: Exception do
  begin
    // Journaliser l'erreur
    TThread.Queue(nil,
      procedure
      begin
        Form1.LogError('Erreur dans le thread ' + IntToStr(ThreadID) + ': ' + E.Message);
      end
    );
    // Ne pas laisser l'exception se propager au-delà
  end;
end;
```

## Exemple complet : traitement d'images par lot

Voici un exemple pratique complet qui utilise un pool de threads pour traiter un lot d'images :

```pascal
type
  TTâcheTraitementImage = record
    CheminSource: string;
    CheminDestination: string;
  end;

  TTraitementImagesParLot = class
  private
    FFileAttente: TThreadedQueue<TTâcheTraitementImage>;
    FPool: array of TThread;
    FArrêter: Boolean;
    FNombreTraitées: Integer;
    FNombreTotal: Integer;
    FVerrou: TCriticalSection;
    FOnProgressionMiseAJour: TProc<Integer, Integer>; // Callback: (Traitées, Total)

    procedure ThreadTraitement(IndexThread: Integer);
    procedure TraiterImage(const Tâche: TTâcheTraitementImage);
  public
    constructor Create(NombreThreads: Integer);
    destructor Destroy; override;
    procedure AjouterImage(const CheminSource, CheminDestination: string);
    procedure DémarrerTraitement;

    property OnProgressionMiseAJour: TProc<Integer, Integer> read FOnProgressionMiseAJour write FOnProgressionMiseAJour;
  end;

constructor TTraitementImagesParLot.Create(NombreThreads: Integer);
var
  i: Integer;
begin
  inherited Create;

  FFileAttente := TThreadedQueue<TTâcheTraitementImage>.Create(1000, INFINITE);
  FVerrou := TCriticalSection.Create;
  FNombreTraitées := 0;
  FNombreTotal := 0;
  FArrêter := False;

  // Créer les threads du pool
  SetLength(FPool, NombreThreads);
  for i := 0 to NombreThreads - 1 do
  begin
    FPool[i] := TThread.CreateAnonymousThread(
      procedure
      begin
        ThreadTraitement(i);
      end
    );
    FPool[i].FreeOnTerminate := False;
    FPool[i].Start;
  end;
end;

destructor TTraitementImagesParLot.Destroy;
var
  i: Integer;
  TâcheVide: TTâcheTraitementImage;
begin
  // Signaler aux threads de s'arrêter
  FArrêter := True;

  // Réveiller tous les threads qui attendent
  for i := 0 to Length(FPool) - 1 do
    FFileAttente.PushItem(TâcheVide);

  // Attendre que tous les threads se terminent
  for i := 0 to Length(FPool) - 1 do
  begin
    FPool[i].WaitFor;
    FPool[i].Free;
  end;

  FFileAttente.Free;
  FVerrou.Free;
  inherited;
end;

procedure TTraitementImagesParLot.AjouterImage(const CheminSource, CheminDestination: string);
var
  Tâche: TTâcheTraitementImage;
begin
  Tâche.CheminSource := CheminSource;
  Tâche.CheminDestination := CheminDestination;

  // Ajouter à la file d'attente
  FFileAttente.PushItem(Tâche);

  // Incrémenter le compteur total
  FVerrou.Enter;
  try
    Inc(FNombreTotal);
  finally
    FVerrou.Leave;
  end;
end;

procedure TTraitementImagesParLot.DémarrerTraitement;
begin
  // Mettre à jour la progression initiale
  if Assigned(FOnProgressionMiseAJour) then
    FOnProgressionMiseAJour(0, FNombreTotal);
end;

procedure TTraitementImagesParLot.ThreadTraitement(IndexThread: Integer);
var
  Tâche: TTâcheTraitementImage;
  Résultat: TWaitResult;
begin
  while not FArrêter do
  begin
    // Attendre et récupérer une tâche
    Résultat := FFileAttente.PopItem(Tâche);

    if (Résultat = wrSignaled) and (not FArrêter) and (Tâche.CheminSource <> '') then
    begin
      try
        // Traiter l'image
        TraiterImage(Tâche);

        // Mettre à jour le compteur et la progression
        FVerrou.Enter;
        try
          Inc(FNombreTraitées);

          // Appeler le callback de mise à jour de la progression
          if Assigned(FOnProgressionMiseAJour) then
          begin
            TThread.Queue(nil,
              procedure
              begin
                FOnProgressionMiseAJour(FNombreTraitées, FNombreTotal);
              end
            );
          end;
        finally
          FVerrou.Leave;
        end;
      except
        on E: Exception do
        begin
          // Journaliser l'erreur (dans une application réelle)
          TThread.Queue(nil,
            procedure
            begin
              // Journalisation de l'erreur
            end
          );
        end;
      end;
    end;
  end;
end;

procedure TTraitementImagesParLot.TraiterImage(const Tâche: TTâcheTraitementImage);
var
  Image: TBitmap;
begin
  // Simuler un traitement d'image
  // Dans une application réelle, vous implémenteriez ici votre logique de traitement

  Image := TBitmap.Create;
  try
    // Charger l'image
    Image.LoadFromFile(Tâche.CheminSource);

    // Simuler un traitement qui prend du temps
    Sleep(500 + Random(1000));

    // Sauvegarder l'image traitée
    Image.SaveToFile(Tâche.CheminDestination);
  finally
    Image.Free;
  end;
end;
```

### Utilisation de la classe de traitement d'images par lot

```pascal
var
  TraitementImages: TTraitementImagesParLot;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Créer l'objet de traitement avec un thread par cœur de processeur
  TraitementImages := TTraitementImagesParLot.Create(TThread.ProcessorCount);

  // Définir le callback de progression
  TraitementImages.OnProgressionMiseAJour :=
    procedure(Traitées, Total: Integer)
    begin
      ProgressBar1.Max := Total;
      ProgressBar1.Position := Traitées;
      Label1.Caption := Format('Progression : %d/%d (%d%%)',
                              [Traitées, Total, Round(Traitées / Total * 100)]);

      // Si toutes les images sont traitées
      if Traitées = Total then
        ShowMessage('Traitement terminé !');
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TraitementImages.Free;
end;

procedure TForm1.ButtonAddFolderClick(Sender: TObject);
var
  DossierSource, DossierDestination: string;
  Fichiers: TStringDynArray;
  Fichier: string;
  CheminSource, CheminDestination: string;
begin
  // Dans une application réelle, vous utiliseriez un sélecteur de dossier
  DossierSource := 'C:\Images\Source';
  DossierDestination := 'C:\Images\Destination';

  // Vérifier que le dossier de destination existe
  if not DirectoryExists(DossierDestination) then
    ForceDirectories(DossierDestination);

  // Récupérer tous les fichiers d'image du dossier source
  Fichiers := TDirectory.GetFiles(DossierSource, '*.jpg');

  // Ajouter chaque image à la file d'attente
  for Fichier in Fichiers do
  begin
    CheminSource := Fichier;
    CheminDestination := TPath.Combine(DossierDestination, TPath.GetFileName(Fichier));

    TraitementImages.AjouterImage(CheminSource, CheminDestination);
  end;

  // Démarrer le traitement
  TraitementImages.DémarrerTraitement;
end;
```

## Modèles avancés avec files d'attente et pools de threads

### Modèle de travail parallèle avec dépendances

Dans certains cas, vous pourriez avoir des tâches avec des dépendances (une tâche ne peut commencer que lorsque d'autres sont terminées). Pour gérer ce scénario, vous pouvez implémenter un graphe de tâches :

```pascal
type
  TÉtatTâche = (etEnAttente, etEnCours, etTerminée, etErreur);

  TTâche = class
  private
    FID: Integer;
    FÉtat: TÉtatTâche;
    FDépendances: TList<TTâche>;
    FProc: TProc;
    FVerrou: TCriticalSection;
  public
    constructor Create(ID: Integer; const Proc: TProc);
    destructor Destroy; override;
    procedure AjouterDépendance(const Tâche: TTâche);
    function ToutesDépendancesTerminées: Boolean;
    procedure Exécuter;

    property ID: Integer read FID;
    property État: TÉtatTâche read FÉtat write FÉtat;
  end;

  TGrapheTâches = class
  private
    FTâches: TObjectList<TTâche>;
    FFileAttente: TThreadedQueue<TTâche>;
    FPool: array of TThread;
    FTerminé: Boolean;
    FOnTerminé: TProc;

    procedure ThreadTraitement;
    procedure VérifierTâchesDisponibles;
  public
    constructor Create(NombreThreads: Integer);
    destructor Destroy; override;
    function AjouterTâche(const Proc: TProc): TTâche;
    procedure DémarrerExécution;

    property OnTerminé: TProc read FOnTerminé write FOnTerminé;
  end;
```

Cette implémentation permet de modéliser des flux de travail complexes où certaines tâches dépendent de l'achèvement d'autres tâches.

### Système de priorité dynamique

Dans certains scénarios, vous pourriez vouloir ajuster dynamiquement la priorité des tâches en fonction des changements de conditions. Par exemple, dans une application de traitement multimédia, vous pourriez vouloir donner la priorité aux tâches liées à ce que l'utilisateur est en train de regarder.

Vous pouvez implémenter un système qui réévalue périodiquement les priorités des tâches en attente :

```pascal
procedure TAjusteurPriorité.RéévaluerPriorités;
var
  i: Integer;
  Tâche: TTâcheAvecPriorité;
  NouvellePriorité: TPriorité;
  NouvelleListe: TList<TTâcheAvecPriorité>;
begin
  // Verrouiller la file d'attente
  FVerrou.Enter;
  try
    // Créer une nouvelle liste pour stocker temporairement les tâches
    NouvelleListe := TList<TTâcheAvecPriorité>.Create;
    try
      // Vider la file d'attente dans la liste temporaire
      while FFileAttente.Count > 0 do
        NouvelleListe.Add(FFileAttente.Dequeue);

      // Réévaluer la priorité de chaque tâche
      for i := 0 to NouvelleListe.Count - 1 do
      begin
        Tâche := NouvelleListe[i];

        // Logique pour déterminer la nouvelle priorité
        NouvellePriorité := DéterminerNouvellePriorité(Tâche);

        // Mettre à jour la priorité
        Tâche.Priorité := NouvellePriorité;

        // Remettre dans la file d'attente
        FFileAttente.Enqueue(Tâche);
      end;
    finally
      NouvelleListe.Free;
    end;
  finally
    FVerrou.Leave;
  end;
end;
```

## Optimisation des performances

### Équilibrage de charge adaptatif

Au lieu d'utiliser un nombre fixe de threads, vous pouvez implémenter un équilibrage de charge adaptatif qui ajuste le nombre de threads en fonction de la charge du système :

```pascal
procedure TPoolAdaptatif.AjusterNombreThreads;
var
  TailleFile: Integer;
  ChargeCPU: Double;
  NouveauNombre: Integer;
begin
  // Obtenir la taille actuelle de la file d'attente
  TailleFile := FFileAttente.QueueSize;

  // Obtenir l'utilisation actuelle du CPU (implémentation spécifique au système)
  ChargeCPU := ObtenirChargeCPU;

  // Calculer le nombre optimal de threads
  if (TailleFile > 20) and (ChargeCPU < 70) and (FNombreThreads < FNombreMaxThreads) then
  begin
    // Augmenter le nombre de threads si la file est longue et le CPU n'est pas surchargé
    NouveauNombre := Min(FNombreThreads + 2, FNombreMaxThreads);
    AjusterPoolVers(NouveauNombre);
  end
  else if (TailleFile < 5) and (FNombreThreads > FNombreMinThreads) then
  begin
    // Réduire le nombre de threads si la file est courte
    NouveauNombre := Max(FNombreThreads - 1, FNombreMinThreads);
    AjusterPoolVers(NouveauNombre);
  end;
end;
```

### Tâches de différentes tailles

Lorsque vous avez des tâches de tailles très différentes, les threads peuvent se retrouver déséquilibrés (certains finissent rapidement, d'autres travaillent longtemps). Une stratégie pour améliorer cela est de diviser les grosses tâches en plus petites :

```pascal
procedure TTraitementParallèle.TraiterGrosseTâche(const Tâche: TGrosseTâche);
var
  SousTâches: array of TPetiteTâche;
  i: Integer;
begin
  // Diviser en sous-tâches
  SetLength(SousTâches, 10);
  for i := 0 to 9 do
    SousTâches[i] := DiviserTâche(Tâche, i, 10);

  // Soumettre les sous-tâches au pool
  for i := 0 to 9 do
    FFileAttente.PushItem(SousTâches[i]);
end;
```

### Minimiser les contentions

La contention se produit lorsque plusieurs threads tentent d'accéder simultanément à une ressource partagée. Pour minimiser les contentions :

1. **Réduire la fréquence des verrouillages** : Par exemple, accumuler des résultats localement avant de les ajouter au résultat global.

2. **Utiliser plusieurs files d'attente** : Chaque thread peut avoir sa propre file d'attente, ce qui réduit la contention.

3. **Utiliser des structures de données sans verrou** : Dans certains cas, vous pouvez utiliser des structures conçues pour minimiser les verrouillages.

```pascal
// Exemple : Accumuler localement puis mettre à jour globalement
procedure TThreadTraitement.Exécuter;
var
  i: Integer;
  RésultatLocal: Integer;
  Élément: TÉlément;
begin
  RésultatLocal := 0;

  // Traiter 100 éléments localement sans verrouillage
  for i := 1 to 100 do
  begin
    if FFileAttente.PopItem(Élément) = wrSignaled then
      RésultatLocal := RésultatLocal + TraiterÉlément(Élément);
  end;

  // Mettre à jour le résultat global une seule fois
  FVerrouRésultat.Enter;
  try
    FRésultatGlobal := FRésultatGlobal + RésultatLocal;
  finally
    FVerrouRésultat.Leave;
  end;
end;
```

## Déboguer des applications avec pools de threads

Le débogage d'applications multi-threads peut être complexe. Voici quelques conseils :

### Journalisation des activités

Intégrez un système de journalisation détaillé pour suivre l'activité des threads :

```pascal
procedure TThreadPool.LogThreadActivity(ThreadID: Integer; const Message: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      // Format : [Timestamp] ThreadID: Message
      Form1.Memo1.Lines.Add(Format('[%s] Thread #%d: %s',
                                  [FormatDateTime('hh:nn:ss.zzz', Now),
                                  ThreadID,
                                  Message]));
    end
  );
end;
```

### Suivi des statistiques

Collectez et affichez des statistiques sur l'utilisation du pool :

```pascal
procedure TThreadPool.MettreÀJourStatistiques;
begin
  TThread.Queue(nil,
    procedure
    begin
      Form1.LabelThreadsActifs.Caption := 'Threads actifs : ' + IntToStr(FThreadsActifs);
      Form1.LabelTâchesEnAttente.Caption := 'Tâches en attente : ' + IntToStr(FFileAttente.QueueSize);
      Form1.LabelTâchesTerminées.Caption := 'Tâches terminées : ' + IntToStr(FTâchesTerminées);

      // Calculer le débit (tâches par seconde)
      var TempsCourant := Now;
      var DébitActuel := FTâchesTerminéesDansIntervalle /
                        ((TempsCourant - FDernièreMiseÀJour) * 24 * 60 * 60);
      Form1.LabelDébit.Caption := 'Débit : ' + FormatFloat('0.00', DébitActuel) + ' tâches/sec';

      // Réinitialiser pour le prochain intervalle
      FTâchesTerminéesDansIntervalle := 0;
      FDernièreMiseÀJour := TempsCourant;
    end
  );
end;
```

### Pause et reprise

Implémentez des fonctionnalités pour mettre en pause et reprendre le pool pour faciliter le débogage :

```pascal
procedure TThreadPool.MettreEnPause;
begin
  FEnPause := True;
end;

procedure TThreadPool.Reprendre;
begin
  FEnPause := False;
end;

// Dans la boucle du thread
while not FTerminer do
begin
  // Vérifier si le pool est en pause
  while FEnPause and (not FTerminer) do
    Sleep(100);

  // Code normal du thread...
end;
```

## Exercice pratique

Créez une application qui traite une liste de fichiers texte en parallèle pour compter le nombre d'occurrences de certains mots. L'application devrait :

1. Permettre à l'utilisateur de sélectionner un dossier contenant des fichiers texte
2. Permettre à l'utilisateur de spécifier une liste de mots à rechercher
3. Afficher en temps réel la progression du traitement
4. Afficher les résultats au fur et à mesure qu'ils sont disponibles
5. Permettre à l'utilisateur d'ajuster le nombre de threads utilisés

Cet exercice vous permettra d'appliquer les concepts de files d'attente et de pools de threads dans un contexte pratique.

## Résumé

- Les files d'attente permettent de gérer un flux de tâches à exécuter.
- Les pools de threads permettent d'exécuter efficacement de nombreuses tâches en réutilisant un ensemble fixe de threads.
- Delphi propose des classes intégrées comme `TThreadedQueue<T>` pour faciliter la mise en œuvre de ces patterns.
- Le modèle producteur-consommateur est un pattern courant qui utilise des files d'attente et des pools de threads.
- L'optimisation des performances passe par la gestion du nombre de threads, la réduction des contentions et la division des tâches.
- Le débogage des applications multi-threads nécessite des outils spécifiques comme la journalisation et le suivi des statistiques.

Dans la prochaine section, nous explorerons l'interface utilisateur réactive, qui permettra à vos applications de rester fluides et réactives même pendant l'exécution de tâches intensives en arrière-plan.

⏭️ [Interface utilisateur réactive](11-multithreading-et-programmation-asynchrone/07-interface-utilisateur-reactive.md)
