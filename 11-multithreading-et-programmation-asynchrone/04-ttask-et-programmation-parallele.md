🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.4 TTask et programmation parallèle

## Introduction à TTask

`TTask` est une approche moderne et simplifiée du multithreading introduite dans Delphi. Elle fait partie de la **Parallel Programming Library (PPL)**, qui permet de créer des applications parallèles sans gérer manuellement les threads.

### Pourquoi TTask plutôt que TThread ?

| TThread | TTask |
|---------|-------|
| Gestion manuelle des threads | Gestion automatique |
| Code plus verbeux | Code plus concis |
| Créer une classe pour chaque tâche | Code anonyme (inline) possible |
| Gestion manuelle du pool de threads | Pool de threads intégré |

**TThread** reste utile pour les threads de longue durée, tandis que **TTask** excelle pour les tâches courtes et ponctuelles.

### Analogie simple

Imaginez que vous devez embaucher des personnes pour différents travaux :

- **TThread** : Vous embauchez des employés permanents. Vous gérez leurs contrats, leurs horaires, leur formation.
- **TTask** : Vous faites appel à des intérimaires via une agence. L'agence gère tout, vous donnez juste les tâches à accomplir.

## Première utilisation de TTask

Pour utiliser TTask, vous devez inclure l'unité `System.Threading` :

```pascal
uses
  System.Threading;
```

### Créer et exécuter une tâche simple

```pascal
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  // Créer et démarrer une tâche
  TTask.Run(
    procedure
    begin
      // Ce code s'exécute dans un thread séparé
      Sleep(2000); // Simuler un travail de 2 secondes

      // Pour mettre à jour l'interface, utiliser TThread.Queue
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage('Tâche terminée !');
        end
      );
    end
  );

  // Le code continue immédiatement ici
  // L'interface reste réactive
end;
```

**C'est tout !** Vous venez de créer votre première tâche parallèle avec seulement quelques lignes de code.

## Les différentes façons de créer une TTask

### 1. TTask.Run - Exécution immédiate

La méthode la plus simple :

```pascal
TTask.Run(
  procedure
  begin
    // Votre code ici
  end
);
```

### 2. TTask.Create - Exécution contrôlée

Pour créer une tâche sans la démarrer immédiatement :

```pascal
var
  MaTache: ITask;
begin
  // Créer la tâche
  MaTache := TTask.Create(
    procedure
    begin
      // Votre code ici
    end
  );

  // Démarrer plus tard
  MaTache.Start;
end;
```

### 3. TTask.Future - Récupérer un résultat

Pour obtenir un résultat d'une tâche :

```pascal
var
  Resultat: IFuture<Integer>;
begin
  // Créer une tâche qui retourne un entier
  Resultat := TTask.Future<Integer>(
    function: Integer
    begin
      Sleep(2000); // Calcul long
      Result := 42; // Le résultat
    end
  );

  // Faire autre chose pendant que la tâche s'exécute
  // ...

  // Récupérer le résultat (attend la fin de la tâche si nécessaire)
  ShowMessage('Résultat : ' + IntToStr(Resultat.Value));
end;
```

## Attendre la fin d'une tâche

> ⚠️ **Attention** : `Wait`, `WaitForAll`, `WaitForAny` et `Future.Value` bloquent le thread appelant tant que la tâche n'est pas terminée. Appelés directement depuis le thread UI, ils figent l'interface ! Les exemples ci-dessous illustrent l'API ; en pratique, dans une application UI, enveloppez-les dans un autre `TTask.Run` ou utilisez plutôt des callbacks (voir chapitre 11.5).

### Wait - Attendre la fin

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      Sleep(3000);
    end
  );

  // Attendre que la tâche se termine (BLOQUE le thread appelant !)
  MaTache.Wait;

  ShowMessage('La tâche est terminée !');
end;
```

### Wait avec timeout

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      Sleep(5000);
    end
  );

  // Attendre maximum 2 secondes
  if MaTache.Wait(2000) then
    ShowMessage('Tâche terminée à temps')
  else
    ShowMessage('Timeout ! La tâche continue en arrière-plan');
end;
```

## Attendre plusieurs tâches

### TTask.WaitForAll - Attendre toutes les tâches

```pascal
var
  Taches: array[0..2] of ITask;
  i: Integer;
begin
  // Créer plusieurs tâches
  for i := 0 to 2 do
  begin
    // IMPORTANT : capturer i AVANT TTask.Run pour figer sa valeur
    var Numero := i;
    Taches[i] := TTask.Run(
      procedure
      begin
        Sleep(1000 * (Numero + 1)); // Temps différent pour chaque tâche
        TThread.Queue(nil,
          procedure
          begin
            Memo1.Lines.Add('Tâche ' + IntToStr(Numero) + ' terminée');
          end
        );
      end
    );
  end;

  // Attendre que TOUTES les tâches soient terminées
  TTask.WaitForAll(Taches);

  ShowMessage('Toutes les tâches sont terminées !');
end;
```

> ⚠️ La variable `Numero` est déclarée et capturée **avant** `TTask.Run`. Si on avait écrit `Numero := i;` à l'intérieur de la procédure anonyme, `i` aurait pu déjà avoir changé au moment où la tâche démarre.

### TTask.WaitForAny - Attendre la première tâche

```pascal
var
  Taches: array[0..2] of ITask;
  Indice: Integer;
begin
  // Créer plusieurs tâches
  // ...

  // Attendre que N'IMPORTE QUELLE tâche se termine
  Indice := TTask.WaitForAny(Taches);

  ShowMessage('La tâche ' + IntToStr(Indice) + ' est la première terminée !');
end;
```

## Parallélisation de boucles

Une des fonctionnalités les plus puissantes de la PPL : paralléliser automatiquement les boucles.

### TParallel.For - Boucle parallèle

```pascal
uses
  System.Threading;

procedure TForm1.ButtonClick(Sender: TObject);  
var  
  Temps: TDateTime;
begin
  Temps := Now;

  // Boucle parallèle : les itérations s'exécutent en parallèle
  TParallel.For(1, 100,
    procedure(Index: Integer)
    begin
      // Chaque itération peut s'exécuter dans un thread différent
      // Traitement sur l'élément Index
      Sleep(50); // Simuler un traitement
    end
  );

  ShowMessage('Terminé en ' +
    FormatDateTime('ss.zzz', Now - Temps) + ' secondes');
end;
```

### Comparaison : Boucle séquentielle vs parallèle

```pascal
// SÉQUENTIEL (lent)
procedure TraitementSequentiel;  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    TraiterElement(i); // S'exécute l'un après l'autre
  end;
end;

// PARALLÈLE (rapide sur multi-cœurs)
procedure TraitementParallele;  
begin  
  TParallel.For(1, 1000,
    procedure(Index: Integer)
    begin
      TraiterElement(Index); // Plusieurs éléments traités simultanément
    end
  );
end;
```

### TParallel.For avec un pool de threads dédié

Pour contrôler le nombre de threads utilisés par une boucle parallèle, on passe un `TThreadPool` à `TParallel.For` :

```pascal
var
  MonPool: TThreadPool;
begin
  MonPool := TThreadPool.Create;
  try
    // Limiter à 4 threads simultanés pour cette boucle
    MonPool.SetMaxWorkerThreads(4);
    MonPool.SetMinWorkerThreads(1);

    TParallel.For(1, 1000,
      procedure(Index: Integer)
      begin
        TraiterElement(Index);
      end,
      MonPool
    );
  finally
    MonPool.Free;
  end;
end;
```

> 💡 **Note** : Sans pool spécifique, `TParallel.For` utilise `TThreadPool.Default`, qui se dimensionne automatiquement selon le nombre de cœurs disponibles.

## TParallel.ForEach - Itérer sur des collections

Pour parcourir les éléments d'une liste en parallèle :

```pascal
var
  Liste: TList<string>;
begin
  Liste := TList<string>.Create;
  try
    // Remplir la liste
    Liste.Add('Fichier1.txt');
    Liste.Add('Fichier2.txt');
    Liste.Add('Fichier3.txt');

    // Traiter chaque élément en parallèle
    TParallel.ForEach<string>(Liste,
      procedure(const Element: string)
      begin
        TraiterFichier(Element);
      end
    );
  finally
    Liste.Free;
  end;
end;
```

## Annuler une tâche

La PPL Delphi ne fournit pas de mécanisme d'annulation natif comme certains  
autres environnements (TPL .NET, par exemple). On utilise donc un **drapeau  
booléen partagé** que la tâche consulte régulièrement.  

### Pattern d'annulation avec un drapeau

Le drapeau doit être accessible aussi bien depuis l'intérieur que depuis  
l'extérieur de la tâche — typiquement un champ du formulaire :  

```pascal
type
  TForm1 = class(TForm)
    ButtonLancer: TButton;
    ButtonAnnuler: TButton;
    procedure ButtonLancerClick(Sender: TObject);
    procedure ButtonAnnulerClick(Sender: TObject);
  private
    FAnnulationDemandee: Boolean;
    FTacheEnCours: ITask;
  end;

procedure TForm1.ButtonLancerClick(Sender: TObject);  
begin  
  FAnnulationDemandee := False;

  FTacheEnCours := TTask.Run(
    procedure
    var
      i: Integer;
    begin
      for i := 1 to 1000 do
      begin
        // Vérifier régulièrement si l'annulation est demandée
        if FAnnulationDemandee then
        begin
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Tâche annulée !');
            end
          );
          Exit;
        end;

        // Travail
        Sleep(10);
      end;

      TThread.Queue(nil,
        procedure
        begin
          ShowMessage('Tâche terminée normalement.');
        end
      );
    end
  );
end;

procedure TForm1.ButtonAnnulerClick(Sender: TObject);  
begin  
  // Demander l'annulation : la tâche verra le changement à sa prochaine
  // vérification de FAnnulationDemandee
  FAnnulationDemandee := True;
end;
```

> **Note** : pour les scénarios complexes avec de nombreuses tâches, on peut  
> encapsuler le drapeau dans une petite classe partagée (avec une section  
> critique si nécessaire) — mais pour un simple flag booléen, les lectures  
> et écritures sont déjà atomiques sur les plateformes Delphi supportées.

## Pool de threads

Le pool de threads gère automatiquement un ensemble de threads réutilisables.

### Configuration du pool

```pascal
uses
  System.Threading;

// Définir le nombre minimum et maximum de threads
TThreadPool.Default.SetMinWorkerThreads(2);  
TThreadPool.Default.SetMaxWorkerThreads(8);  
```

### Avantages du pool de threads

1. **Performance** : Réutilise les threads au lieu d'en créer de nouveaux
2. **Gestion automatique** : Ajuste le nombre de threads selon la charge
3. **Simplicité** : Vous n'avez rien à gérer manuellement

## Exemple pratique : Traitement d'images

```pascal
type
  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  end;

procedure TForm1.Button1Click(Sender: TObject);  
var  
  Fichiers: TArray<string>;
begin
  // Liste de fichiers à traiter
  Fichiers := TDirectory.GetFiles('C:\Images', '*.jpg');

  ProgressBar1.Max := Length(Fichiers);
  ProgressBar1.Position := 0;
  Button1.Enabled := False;

  // IMPORTANT : envelopper TParallel.For dans TTask.Run pour ne pas bloquer
  // le thread UI. TParallel.For est synchrone : appelé directement depuis
  // un événement UI, il fige l'interface jusqu'à ce que toutes les itérations
  // soient terminées (et les TThread.Queue ne pourraient même pas s'exécuter).
  TTask.Run(
    procedure
    var
      NbTraites: Integer;
      CS: TCriticalSection;
    begin
      NbTraites := 0;
      CS := TCriticalSection.Create;
      try
        // Traiter les images en parallèle
        TParallel.For(0, Length(Fichiers) - 1,
          procedure(Index: Integer)
          begin
            // Traiter l'image (redimensionner, convertir, etc.)
            TraiterImage(Fichiers[Index]);

            // Incrémenter le compteur de manière thread-safe
            CS.Enter;
            try
              Inc(NbTraites);
            finally
              CS.Leave;
            end;

            // Capture locale pour figer la valeur envoyée à l'UI
            var NbCapture := NbTraites;
            TThread.Queue(nil,
              procedure
              begin
                ProgressBar1.Position := NbCapture;
                Label1.Caption := Format('Traité : %d / %d',
                  [NbCapture, Length(Fichiers)]);
              end
            );
          end
        );

        TThread.Queue(nil,
          procedure
          begin
            ShowMessage('Toutes les images ont été traitées !');
            Button1.Enabled := True;
          end
        );
      finally
        CS.Free;
      end;
    end
  );
end;
```

## Future et résultats multiples

### Lancer plusieurs calculs en parallèle

```pascal
var
  Future1, Future2, Future3: IFuture<Integer>;
  Total: Integer;
begin
  // Lancer trois calculs en parallèle
  Future1 := TTask.Future<Integer>(
    function: Integer
    begin
      Sleep(1000);
      Result := 10 * 10;
    end
  );

  Future2 := TTask.Future<Integer>(
    function: Integer
    begin
      Sleep(1000);
      Result := 20 * 20;
    end
  );

  Future3 := TTask.Future<Integer>(
    function: Integer
    begin
      Sleep(1000);
      Result := 30 * 30;
    end
  );

  // Récupérer les résultats (attend que toutes les tâches soient terminées)
  Total := Future1.Value + Future2.Value + Future3.Value;

  ShowMessage('Total : ' + IntToStr(Total)); // 100 + 400 + 900 = 1400
end;
```

## Gestion des exceptions dans TTask

Les exceptions dans les tâches doivent être gérées avec soin :

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      try
        // Code qui peut lever une exception
        raise Exception.Create('Erreur dans la tâche !');
      except
        on E: Exception do
        begin
          // IMPORTANT : capturer E.Message AVANT TThread.Queue.
          // L'objet E est libéré à la sortie du bloc except,
          // donc E.Message serait invalide quand la Queue s'exécute (asynchrone).
          var MessageErreur := E.Message;
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Erreur : ' + MessageErreur);
            end
          );
        end;
      end;
    end
  );
end;
```

### Vérifier le statut d'une tâche

```pascal
var
  MaTache: ITask;
begin
  MaTache := TTask.Run(
    procedure
    begin
      // ...
    end
  );

  // Attendre la fin
  MaTache.Wait;

  // Vérifier le statut
  case MaTache.Status of
    TTaskStatus.Created:
      ShowMessage('Tâche créée');
    TTaskStatus.Running:
      ShowMessage('Tâche en cours');
    TTaskStatus.Completed:
      ShowMessage('Tâche terminée avec succès');
    TTaskStatus.Canceled:
      ShowMessage('Tâche annulée');
    TTaskStatus.Exception:
      ShowMessage('Tâche terminée avec erreur');
  end;
end;
```

## Bonnes pratiques avec TTask

### 1. Utiliser TTask pour les tâches courtes

```pascal
// ✅ BON : Tâche ponctuelle avec TTask
TTask.Run(
  procedure
  begin
    TelechargerFichier('http://example.com/data.json');
  end
);

// ❌ INADAPTÉ pour TTask : pour les services de longue durée
// (comme surveiller un port réseau en continu), préférer une classe
// TThread dédiée avec une boucle while not Terminated do.
```

### 2. Ne pas capturer de variables locales dangereuses

```pascal
// ❌ DANGEREUX
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    TTask.Run(
      procedure
      begin
        // i peut avoir changé entre le moment de création et l'exécution !
        ShowMessage(IntToStr(i));
      end
    );
  end;
end;

// ✅ CORRECT : Capturer la variable AVANT la procédure anonyme
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    var Numero := i; // Copie locale AVANT TTask.Run
    TTask.Run(
      procedure
      begin
        ShowMessage(IntToStr(Numero));
      end
    );
  end;
end;
```

> ⚠️ **Important** : `Numero := i` doit être **avant** `TTask.Run`. Si on le mettait à l'intérieur de la procédure anonyme (`procedure var Numero: Integer; begin Numero := i; ...`), `i` serait lu au moment de l'exécution de la tâche — pas au moment de sa création — donc le bug serait toujours là.

### 3. Synchroniser l'accès aux ressources partagées

Même avec TTask, vous devez protéger les accès concurrents :

```pascal
var
  CS: TCriticalSection;
  Compteur: Integer;

begin
  CS := TCriticalSection.Create;
  try
    TParallel.For(1, 1000,
      procedure(Index: Integer)
      begin
        CS.Enter;
        try
          Inc(Compteur); // Protégé
        finally
          CS.Leave;
        end;
      end
    );
  finally
    CS.Free;
  end;
end;
```

### 4. Limiter le nombre de tâches simultanées

```pascal
// ❌ MAUVAIS : Créer 10000 tâches d'un coup
for i := 1 to 10000 do
  TTask.Run(procedure begin ... end);

// ✅ BON : Utiliser TParallel.For qui gère le pool
TParallel.For(1, 10000,
  procedure(Index: Integer)
  begin
    // ...
  end
);
```

## Comparaison : TThread vs TTask

| Critère | TThread | TTask |
|---------|---------|-------|
| **Complexité** | Plus complexe | Plus simple |
| **Verbosité** | Nécessite une classe | Code inline possible |
| **Gestion mémoire** | Manuelle | Automatique |
| **Pool de threads** | Non | Oui |
| **Idéal pour** | Threads de longue durée | Tâches ponctuelles |
| **Parallélisation** | Manuelle | Automatique (TParallel) |
| **Courbe d'apprentissage** | Plus longue | Plus courte |

## Points clés à retenir

- **TTask** simplifie grandement la programmation parallèle en Delphi
- Utilisez `TTask.Run` pour les tâches simples et ponctuelles
- `TTask.Future` permet de récupérer un résultat d'une tâche
- `TParallel.For` parallélise automatiquement les boucles sur les processeurs multi-cœurs
- Le pool de threads gère automatiquement la réutilisation des threads
- Toujours protéger les accès aux ressources partagées
- Utiliser `TThread.Queue` ou `TThread.Synchronize` pour mettre à jour l'interface
- TTask est idéal pour les tâches courtes, TThread pour les services de longue durée
- Attention à la capture des variables dans les procédures anonymes

TTask et la Parallel Programming Library rendent la programmation parallèle accessible à tous, même aux débutants. C'est l'approche recommandée pour la plupart des besoins de multithreading dans les applications Delphi modernes.

⏭️ [Tâches asynchrones et callbacks](/11-multithreading-et-programmation-asynchrone/05-taches-asynchrones-et-callbacks.md)
