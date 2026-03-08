🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.10 Performances et bonnes pratiques en multithreading

Cette section finale du chapitre multithreading rassemble les meilleures pratiques, les pièges à éviter, et les conseils d'optimisation pour créer des applications multithreadées performantes et fiables.

## Règles d'or du multithreading

### 1. Le thread principal ne doit JAMAIS être bloqué

**Règle absolue** : Le thread principal (UI) doit toujours rester disponible pour traiter les événements utilisateur.

```pascal
// ❌ MAUVAIS : Bloque l'interface
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  Sleep(5000); // L'interface se fige pendant 5 secondes !
  ShowMessage('Terminé');
end;

// ✅ BON : Interface réactive
procedure TForm1.ButtonClick(Sender: TObject);  
begin  
  TTask.Run(
    procedure
    begin
      Sleep(5000); // Dans un thread séparé
      TThread.Queue(nil,
        procedure
        begin
          ShowMessage('Terminé');
        end
      );
    end
  );
end;
```

### 2. Ne jamais modifier l'interface depuis un thread secondaire

**Règle absolue** : Toujours utiliser `Synchronize` ou `Queue` pour mettre à jour l'interface.

```pascal
// ❌ DANGEREUX : Peut planter l'application
TTask.Run(
  procedure
  begin
    Label1.Caption := 'Nouveau texte'; // ERREUR !
  end
);

// ✅ CORRECT : Via Synchronize ou Queue
TTask.Run(
  procedure
  begin
    TThread.Queue(nil,
      procedure
      begin
        Label1.Caption := 'Nouveau texte'; // Sûr
      end
    );
  end
);
```

### 3. Protéger les ressources partagées

**Règle absolue** : Utiliser des sections critiques pour les données accessibles par plusieurs threads.

```pascal
var
  Compteur: Integer; // Partagé entre threads
  CS: TCriticalSection;

// ❌ DANGEREUX : Race condition
TTask.Run(
  procedure
  begin
    Inc(Compteur); // Non protégé !
  end
);

// ✅ CORRECT : Protégé
TTask.Run(
  procedure
  begin
    CS.Enter;
    try
      Inc(Compteur); // Protégé
    finally
      CS.Leave;
    end;
  end
);
```

## Quand utiliser le multithreading ?

### Scénarios appropriés ✅

1. **Opérations I/O longues** : Lecture/écriture de fichiers, requêtes réseau, accès base de données
2. **Calculs intensifs** : Traitement d'images, analyses de données, cryptographie
3. **Opérations parallélisables** : Traitement de listes, conversion de fichiers multiples
4. **Tâches d'arrière-plan** : Synchronisation, sauvegarde automatique, surveillance
5. **Interface réactive** : Éviter le gel pendant les traitements

### Scénarios inappropriés ❌

1. **Opérations très rapides** : Moins de 100ms, le coût du thread est supérieur au gain
2. **Accès séquentiels obligatoires** : Quand les opérations doivent être dans un ordre strict
3. **Ressources uniques** : Un seul accès possible à la fois (certains drivers matériels)
4. **Code simple** : Si la complexité ajoutée n'en vaut pas la peine

```pascal
// ❌ PAS NÉCESSAIRE : Trop rapide
TTask.Run(
  procedure
  begin
    X := Y + Z; // Calcul instantané
  end
);

// ✅ NÉCESSAIRE : Opération longue
TTask.Run(
  procedure
  begin
    TelechargerGrossFichier('http://example.com/1GB.zip');
  end
);
```

## Dimensionner le nombre de threads

### La règle du nombre de cœurs

**Règle empirique** : Pour les tâches CPU-intensives, limiter à `Nombre de cœurs × 1.5 à 2`

```pascal
var
  NbThreadsOptimal: Integer;
begin
  NbThreadsOptimal := TThread.ProcessorCount * 2;
  TThreadPool.Default.SetMaxWorkerThreads(NbThreadsOptimal);
end;
```

### Trop de threads = Performance dégradée

```pascal
// ❌ MAUVAIS : Trop de threads
for i := 1 to 10000 do  
begin  
  TTask.Run(
    procedure
    begin
      TraiterElement(i);
    end
  );
end;
// Crée 10000 threads ! Le système passe son temps à les gérer

// ✅ BON : Utiliser TParallel qui gère le pool
TParallel.For(1, 10000,
  procedure(Index: Integer)
  begin
    TraiterElement(Index);
  end
);
// Utilise automatiquement le nombre optimal de threads
```

### Règles selon le type de tâche

| Type de tâche | Nombre de threads recommandé |
|---------------|------------------------------|
| CPU-intensive (calculs) | Nombre de cœurs × 1-2 |
| I/O-intensive (réseau, fichiers) | Nombre de cœurs × 2-4 |
| Mixte | Nombre de cœurs × 2 |

## Minimiser le temps dans les sections critiques

Plus une section critique est longue, plus elle crée de contention et ralentit l'application.

```pascal
// ❌ MAUVAIS : Section critique trop longue
CS.Enter;  
try  
  LireFichier(); // Opération lente
  TraiterDonnees(); // Opération lente
  Inc(Compteur); // Opération rapide
finally
  CS.Leave;
end;

// ✅ BON : Minimiser la section critique
LireFichier(); // En dehors  
TraiterDonnees(); // En dehors  

CS.Enter;  
try  
  Inc(Compteur); // Seulement ce qui doit être protégé
finally
  CS.Leave;
end;
```

### Préparer les données avant la section critique

```pascal
// ✅ EXCELLENT : Préparer puis protéger
var
  NouvellesValeursLocales: TArray<Integer>;
begin
  // Préparer les données en dehors de la section critique
  SetLength(NouvellesValeursLocales, 1000);
  for i := 0 to 999 do
    NouvellesValeursLocales[i] := CalculerValeur(i);

  // Section critique minimale pour la mise à jour
  CS.Enter;
  try
    FValeurs := NouvellesValeursLocales;
  finally
    CS.Leave;
  end;
end;
```

## Éviter les deadlocks

Un **deadlock** (étreinte fatale) survient quand deux threads s'attendent mutuellement.

### Cause du deadlock

```pascal
// ❌ DANGEREUX : Deadlock possible
var
  CS1, CS2: TCriticalSection;

// Thread 1
CS1.Enter;
  CS2.Enter; // Attend CS2
    // Traitement
  CS2.Leave;
CS1.Leave;

// Thread 2
CS2.Enter;
  CS1.Enter; // Attend CS1 → DEADLOCK !
    // Traitement
  CS1.Leave;
CS2.Leave;
```

### Solutions

**1. Toujours acquérir les verrous dans le même ordre**

```pascal
// ✅ BON : Ordre cohérent
// Thread 1
CS1.Enter;
  CS2.Enter;
    // Traitement
  CS2.Leave;
CS1.Leave;

// Thread 2 : MÊME ORDRE
CS1.Enter;
  CS2.Enter;
    // Traitement
  CS2.Leave;
CS1.Leave;
```

**2. Utiliser TryEnter avec timeout**

```pascal
// ✅ BON : Éviter le blocage infini
if CS1.TryEnter then  
begin  
  try
    if CS2.TryEnter then
    begin
      try
        // Traitement
      finally
        CS2.Leave;
      end;
    end
    else
    begin
      // CS2 non disponible, réessayer plus tard
    end;
  finally
    CS1.Leave;
  end;
end;
```

**3. Limiter les sections critiques imbriquées**

```pascal
// ✅ BON : Éviter l'imbrication
CS1.Enter;  
try  
  // Traitement 1
finally
  CS1.Leave;
end;

// Séparé
CS2.Enter;  
try  
  // Traitement 2
finally
  CS2.Leave;
end;
```

## Gérer les fuites mémoire

### FreeOnTerminate et gestion mémoire

```pascal
// ❌ DANGEREUX : Fuite mémoire
var
  MonThread: TThread;
begin
  MonThread := TThread.Create(False);
  MonThread.FreeOnTerminate := True;
  // Ne PAS garder de référence !
  // MonThread.Free; ← NE JAMAIS FAIRE !
end;

// ✅ BON : Soit auto, soit manuel, mais pas les deux
var
  MonThread: TThread;
begin
  MonThread := TThread.Create(False);
  MonThread.FreeOnTerminate := False;
  try
    MonThread.WaitFor;
  finally
    MonThread.Free; // Libération manuelle
  end;
end;
```

### Capture de variables dans TTask

```pascal
// ❌ DANGEREUX : Capture d'objet qui peut être libéré
var
  Liste: TStringList;
begin
  Liste := TStringList.Create;

  TTask.Run(
    procedure
    begin
      Sleep(2000);
      Liste.Add('Test'); // Liste peut déjà être libérée !
    end
  );

  Liste.Free; // Libéré avant la fin de la tâche !
end;

// ✅ BON : Créer et libérer dans la tâche
TTask.Run(
  procedure
  var
    Liste: TStringList;
  begin
    Liste := TStringList.Create;
    try
      Sleep(2000);
      Liste.Add('Test');
    finally
      Liste.Free; // Libéré au bon moment
    end;
  end
);
```

## Optimiser les mises à jour d'interface

### Limiter la fréquence des mises à jour

```pascal
// ❌ LENT : Mise à jour à chaque itération
for i := 1 to 100000 do  
begin  
  TraiterElement(i);
  TThread.Queue(nil,
    procedure
    begin
      ProgressBar1.Position := i;
    end
  );
end;

// ✅ RAPIDE : Mise à jour périodique
var
  DerniereMAJ: TDateTime;
begin
  DerniereMAJ := 0;

  for i := 1 to 100000 do
  begin
    TraiterElement(i);

    // Mettre à jour seulement toutes les 100ms
    if MilliSecondsBetween(Now, DerniereMAJ) > 100 then
    begin
      TThread.Queue(nil,
        procedure
        begin
          ProgressBar1.Position := (i * 100) div 100000;
        end
      );
      DerniereMAJ := Now;
    end;
  end;
end;
```

### Queue vs Synchronize

```pascal
// Synchronize : Bloque le thread jusqu'à exécution
TThread.Synchronize(nil,
  procedure
  begin
    Label1.Caption := 'Message';
  end
);
// Le thread attend ici

// Queue : Ne bloque pas, continue immédiatement
TThread.Queue(nil,
  procedure
  begin
    Label1.Caption := 'Message';
  end
);
// Le thread continue sans attendre
```

**Recommandation** : Préférez `Queue` pour de meilleures performances, sauf si vous devez attendre la mise à jour.

## Mesurer les performances

### Utiliser TStopwatch pour mesurer

```pascal
uses
  System.Diagnostics;

var
  Chrono: TStopwatch;
begin
  Chrono := TStopwatch.StartNew;

  // Code à mesurer
  TraiterDonnees;

  Chrono.Stop;
  ShowMessage(Format('Temps écoulé : %d ms', [Chrono.ElapsedMilliseconds]));
end;
```

### Comparer séquentiel vs parallèle

```pascal
procedure TForm1.ComparerPerformances;  
var  
  Chrono: TStopwatch;
  TempsSeq, TempsParallele: Int64;
begin
  // Test séquentiel
  Chrono := TStopwatch.StartNew;
  for i := 1 to 10000 do
    TraiterElement(i);
  TempsSeq := Chrono.ElapsedMilliseconds;

  // Test parallèle
  Chrono := TStopwatch.StartNew;
  TParallel.For(1, 10000,
    procedure(Index: Integer)
    begin
      TraiterElement(Index);
    end
  );
  TempsParallele := Chrono.ElapsedMilliseconds;

  ShowMessage(Format(
    'Séquentiel : %d ms' + sLineBreak +
    'Parallèle : %d ms' + sLineBreak +
    'Accélération : %.2fx',
    [TempsSeq, TempsParallele, TempsSeq / TempsParallele]
  ));
end;
```

## Anti-patterns à éviter

### 1. Polling intensif

```pascal
// ❌ MAUVAIS : Consomme du CPU inutilement
while not FTerminer do  
begin  
  if FNouvelleDonnee then
  begin
    Traiter;
    FNouvelleDonnee := False;
  end;
  // Boucle très rapide qui consomme du CPU !
end;

// ✅ BON : Avec pause
while not FTerminer do  
begin  
  if FNouvelleDonnee then
  begin
    Traiter;
    FNouvelleDonnee := False;
  end;
  Sleep(100); // Laisser respirer le CPU
end;

// ✅ MEILLEUR : Utiliser un Event
while not FTerminer do  
begin  
  if FEvent.WaitFor(1000) = wrSignaled then
  begin
    Traiter;
  end;
end;
```

### 2. Création/destruction excessive de threads

```pascal
// ❌ MAUVAIS : Créer un thread par requête
for i := 1 to 1000 do  
begin  
  TThread.CreateAnonymousThread(
    procedure
    begin
      TraiterRequete(i);
    end
  ).Start;
end;

// ✅ BON : Utiliser le pool de threads
for i := 1 to 1000 do  
begin  
  TTask.Run(
    procedure
    var
      Index: Integer;
    begin
      Index := i;
      TraiterRequete(Index);
    end
  );
end;
```

### 3. Synchronisation excessive

```pascal
// ❌ LENT : Trop de synchronisation
for i := 1 to 10000 do  
begin  
  CS.Enter;
  try
    Inc(Compteur);
  finally
    CS.Leave;
  end;
end;

// ✅ RAPIDE : Compteur local puis synchronisation finale
var
  CompteurLocal: Integer;
begin
  CompteurLocal := 0;
  for i := 1 to 10000 do
    Inc(CompteurLocal);

  CS.Enter;
  try
    Inc(CompteurGlobal, CompteurLocal);
  finally
    CS.Leave;
  end;
end;
```

### 4. Capture de variables de boucle

```pascal
// ❌ DANGEREUX : Variable de boucle capturée
for i := 1 to 10 do  
begin  
  TTask.Run(
    procedure
    begin
      ShowMessage(IntToStr(i)); // i peut avoir changé !
    end
  );
end;

// ✅ CORRECT : Copie locale
for i := 1 to 10 do  
begin  
  TTask.Run(
    procedure
    var
      Index: Integer;
    begin
      Index := i; // Copie locale
      ShowMessage(IntToStr(Index));
    end
  );
end;
```

## Débogage du code multithread

### Activer les options de débogage

Dans les options de projet (Project > Options > Delphi Compiler > Debugging) :
- Activer "Debug information"
- Activer "Use Debug DCUs"
- Dans "Linker", activer "Include TD32 debug info"

### Points d'arrêt conditionnels

```pascal
procedure TMonThread.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // Point d'arrêt conditionnel : s'arrêter seulement quand i = 500
    if i = 500 then
      DebugBreak; // ou mettre un point d'arrêt normal ici

    TraiterElement(i);
  end;
end;
```

### Logger les événements

```pascal
procedure Log(const Msg: string);  
var  
  CS: TCriticalSection; // Global
begin
  CS.Enter;
  try
    TFile.AppendAllText('debug.log',
      Format('[%s] Thread %d: %s' + sLineBreak,
        [FormatDateTime('hh:nn:ss.zzz', Now),
         TThread.CurrentThread.ThreadID,
         Msg]));
  finally
    CS.Leave;
  end;
end;

// Utilisation
procedure TMonThread.Execute;  
begin  
  Log('Thread démarré');
  try
    TraiterDonnees;
    Log('Traitement terminé');
  except
    on E: Exception do
      Log('ERREUR: ' + E.Message);
  end;
end;
```

## Checklist des bonnes pratiques

### ✅ Avant de coder

- [ ] Le multithreading est-il vraiment nécessaire ?
- [ ] L'opération est-elle assez longue pour justifier un thread ?
- [ ] Ai-je identifié les ressources partagées ?
- [ ] Ai-je un plan pour protéger ces ressources ?

### ✅ Pendant le développement

- [ ] Utiliser TTask plutôt que TThread quand possible
- [ ] Toujours protéger les ressources partagées
- [ ] Utiliser Queue/Synchronize pour l'interface
- [ ] Vérifier Terminated dans les boucles
- [ ] Gérer les exceptions dans les threads
- [ ] Ne pas capturer des variables dangereuses

### ✅ Pour les performances

- [ ] Limiter le nombre de threads actifs
- [ ] Minimiser le temps dans les sections critiques
- [ ] Limiter la fréquence des mises à jour d'interface
- [ ] Utiliser TParallel.For pour les boucles
- [ ] Éviter la création/destruction excessive de threads

### ✅ Pour la fiabilité

- [ ] Gérer proprement FreeOnTerminate
- [ ] Se désabonner des événements
- [ ] Vérifier les références nulles
- [ ] Tester avec différentes charges
- [ ] Logger pour le débogage

### ✅ Avant la mise en production

- [ ] Tester sur machines mono-cœur ET multi-cœurs
- [ ] Tester avec beaucoup d'utilisateurs simultanés
- [ ] Vérifier les fuites mémoire (FastMM en mode debug)
- [ ] Profiler les performances
- [ ] Documenter le comportement multithread

## Outils utiles

### FastMM pour détecter les fuites

Activez FastMM en mode full debug dans votre projet :

```pascal
program MonApplication;

{$IFDEF DEBUG}
  {$DEFINE FullDebugMode}
{$ENDIF}

uses
  FastMM4,
  // ... autres units
```

### AQTime ou Sampling Profiler

Pour analyser les performances et identifier les goulots d'étranglement.

### CodeSite pour le logging

```pascal
uses
  CodeSite;

procedure TMonThread.Execute;  
begin  
  CodeSite.Send('Thread démarré', Self);
  // ...
end;
```

## Résumé des performances par approche

| Approche | Performance | Complexité | Quand utiliser |
|----------|-------------|------------|----------------|
| Pas de thread | ⭐⭐⭐⭐⭐ | ⭐ | Opérations rapides (<100ms) |
| Application.ProcessMessages | ⭐⭐⭐ | ⭐⭐ | Petites applications simples |
| TThread | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | Threads de longue durée, contrôle fin |
| TTask.Run | ⭐⭐⭐⭐⭐ | ⭐⭐ | Tâches ponctuelles (recommandé) |
| TParallel.For | ⭐⭐⭐⭐⭐ | ⭐⭐ | Boucles parallélisables (recommandé) |
| TThreadedQueue | ⭐⭐⭐⭐ | ⭐⭐⭐ | Producteur-consommateur |

## Points clés finaux

1. **Simplicité d'abord** : N'ajoutez du multithreading que si nécessaire
2. **TTask est votre ami** : Utilisez-le par défaut pour 80% des cas
3. **Protégez tout** : Les ressources partagées doivent TOUJOURS être protégées
4. **Interface sacrée** : JAMAIS de modification directe depuis un thread secondaire
5. **Mesurez** : Vérifiez que le multithreading apporte réellement un gain
6. **Testez** : Le multithreading crée des bugs subtils, testez intensivement
7. **Documentez** : Expliquez votre stratégie de threading dans les commentaires
8. **Limitez** : Trop de threads = performance dégradée
9. **Gérez les erreurs** : Les exceptions dans les threads doivent être capturées
10. **Nettoyez** : Libérez proprement toutes les ressources

Le multithreading est un outil puissant mais exigeant. Avec les bonnes pratiques et les outils modernes de Delphi (TTask, TParallel), vous pouvez créer des applications performantes, réactives et fiables. La clé est de rester simple, de bien comprendre les principes fondamentaux, et de toujours penser à la sécurité des threads.

⏭️ [Débogage et tests](/12-debogage-et-tests/README.md)
