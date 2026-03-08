🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.9 Débogage de code multi-thread

## Introduction

Imaginez que vous organisez une cuisine professionnelle avec plusieurs chefs qui travaillent simultanément sur différents plats. Tout fonctionne bien jusqu'à ce que deux chefs essaient d'utiliser le même couteau en même temps, ou qu'un chef attende qu'un autre finisse avant de commencer, créant un blocage. Déboguer une application multi-thread, c'est comme résoudre ces conflits dans une cuisine où tout se passe très vite et de manière imprévisible.

Le **multi-threading** permet à votre application d'exécuter plusieurs tâches en parallèle, ce qui améliore les performances et la réactivité. Mais il introduit aussi une complexité considérable, rendant le débogage beaucoup plus difficile qu'avec du code séquentiel classique.

Pour un débutant, le débogage multi-thread peut sembler intimidant, mais en comprenant les problèmes courants et en utilisant les bons outils, vous pouvez résoudre même les bugs les plus obscurs.

## Qu'est-ce que le multi-threading ?

### Définition simple

Le **multi-threading** est une technique qui permet à votre application d'exécuter plusieurs séquences de code (threads) en même temps. Chaque thread est comme un mini-programme qui s'exécute indépendamment des autres.

### Analogie pour comprendre

**Programme mono-thread :**
- Comme une personne qui fait une tâche après l'autre
- Laver la vaisselle → Puis cuisiner → Puis nettoyer
- Simple, prévisible, mais potentiellement lent

**Programme multi-thread :**
- Comme plusieurs personnes qui travaillent en parallèle
- Une personne lave la vaisselle PENDANT qu'une autre cuisine
- Plus rapide, mais nécessite coordination

### Exemple simple en Delphi

```pascal
// Code mono-thread (séquentiel)
procedure ExecuterTaches;  
begin  
  TacheA;  // S'exécute
  TacheB;  // Puis s'exécute
  TacheC;  // Puis s'exécute
end;

// Code multi-thread (parallèle)
procedure ExecuterTachesParalleles;  
begin  
  TTask.Run(procedure begin TacheA; end);  // S'exécute
  TTask.Run(procedure begin TacheB; end);  // S'exécute en même temps
  TTask.Run(procedure begin TacheC; end);  // S'exécute en même temps
end;
```

### Quand utiliser le multi-threading ?

**Cas d'usage courants :**
- Télécharger des fichiers en arrière-plan
- Traiter de grandes quantités de données
- Maintenir l'interface utilisateur réactive pendant des opérations longues
- Serveurs gérant plusieurs clients simultanément
- Calculs parallèles sur plusieurs cœurs CPU

## Pourquoi le débogage multi-thread est difficile

### 1. Non-déterminisme

L'exécution multi-thread est **non-déterministe** : l'ordre dans lequel les threads s'exécutent change à chaque exécution.

**Exemple :**

```pascal
// Thread 1
procedure Thread1;  
begin  
  WriteLn('A');
  WriteLn('B');
end;

// Thread 2
procedure Thread2;  
begin  
  WriteLn('X');
  WriteLn('Y');
end;
```

**Exécution 1 :** A, X, B, Y  
**Exécution 2 :** X, A, Y, B  
**Exécution 3 :** A, B, X, Y  

Chaque exécution peut produire un ordre différent !

### 2. Bugs intermittents

Un bug multi-thread peut :
- Apparaître une fois sur 1000 exécutions
- Se produire uniquement sur certaines machines
- Disparaître quand vous déboguez (effet Heisenberg)
- Changer de comportement selon la charge CPU

**Conséquence :** "Ça marchait hier !" devient votre phrase préférée.

### 3. Conditions de concurrence (Race Conditions)

Deux threads modifient la même donnée en même temps, produisant un résultat incorrect.

```pascal
var
  Compteur: Integer = 0;

// Thread 1
procedure Thread1;  
var i: Integer;  
begin  
  for i := 1 to 1000 do
    Inc(Compteur);  // Compteur = Compteur + 1
end;

// Thread 2
procedure Thread2;  
var i: Integer;  
begin  
  for i := 1 to 1000 do
    Inc(Compteur);  // Compteur = Compteur + 1
end;
```

**Résultat attendu :** 2000  
**Résultat obtenu :** Parfois 1547, parfois 1823, parfois 2000...  

**Pourquoi ?** `Inc(Compteur)` n'est pas atomique :
1. Lire Compteur (ex: 100)
2. Ajouter 1 (101)
3. Écrire Compteur (101)

Si les deux threads font ça en même temps, on peut perdre des incréments.

### 4. Blocages (Deadlocks)

Deux threads s'attendent mutuellement, créant un blocage permanent.

```pascal
var
  Lock1, Lock2: TCriticalSection;

// Thread 1
Lock1.Enter;
  // ... travail ...
  Lock2.Enter;  // Attend Lock2
    // ... travail ...
  Lock2.Leave;
Lock1.Leave;

// Thread 2
Lock2.Enter;
  // ... travail ...
  Lock1.Enter;  // Attend Lock1
    // ... travail ...
  Lock1.Leave;
Lock2.Leave;
```

**Scénario de deadlock :**
1. Thread 1 acquiert Lock1
2. Thread 2 acquiert Lock2
3. Thread 1 attend Lock2 (détenu par Thread 2)
4. Thread 2 attend Lock1 (détenu par Thread 1)
5. **Blocage permanent** : Les deux attendent indéfiniment

### 5. Violations d'accès imprévisibles

Un thread accède à un objet pendant qu'un autre le libère :

```pascal
// Thread 1 : Crée et utilise l'objet
MonObjet := TMonObjet.Create;  
MonObjet.Traiter;  

// Thread 2 : Libère l'objet
MonObjet.Free;

// Thread 1 : CRASH !
MonObjet.Afficher;  // MonObjet est déjà libéré
```

## Problèmes courants du multi-threading

### 1. Race Conditions (Conditions de concurrence)

**Symptômes :**
- Résultats incorrects ou incohérents
- Données corrompues
- Compteurs incorrects

**Exemple détaillé :**

```pascal
type
  TCompteurNonSecurise = class
  private
    FValeur: Integer;
  public
    procedure Incrementer;
    function ObtenirValeur: Integer;
  end;

procedure TCompteurNonSecurise.Incrementer;  
begin  
  FValeur := FValeur + 1;  // ❌ NON thread-safe
end;

function TCompteurNonSecurise.ObtenirValeur: Integer;  
begin  
  Result := FValeur;
end;
```

**Solution : Synchronisation**

```pascal
type
  TCompteurSecurise = class
  private
    FValeur: Integer;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Incrementer;
    function ObtenirValeur: Integer;
  end;

constructor TCompteurSecurise.Create;  
begin  
  FLock := TCriticalSection.Create;
end;

destructor TCompteurSecurise.Destroy;  
begin  
  FLock.Free;
  inherited;
end;

procedure TCompteurSecurise.Incrementer;  
begin  
  FLock.Enter;
  try
    FValeur := FValeur + 1;  // ✓ Thread-safe
  finally
    FLock.Leave;
  end;
end;

function TCompteurSecurise.ObtenirValeur: Integer;  
begin  
  FLock.Enter;
  try
    Result := FValeur;
  finally
    FLock.Leave;
  end;
end;
```

### 2. Deadlocks (Interblocages)

**Symptômes :**
- L'application se fige complètement
- Aucun message d'erreur
- CPU utilisation à 0%
- Impossible de fermer l'application normalement

**Causes fréquentes :**
- Acquisition de locks dans un ordre différent
- Attente circulaire entre threads
- Oubli de libérer un lock

**Prévention :**

```pascal
// ✓ BON : Toujours acquérir les locks dans le même ordre
procedure SafeOperation;  
begin  
  Lock1.Enter;  // Toujours Lock1 d'abord
  try
    Lock2.Enter;  // Puis Lock2
    try
      // Travail...
    finally
      Lock2.Leave;
    end;
  finally
    Lock1.Leave;
  end;
end;
```

### 3. Accès VCL depuis des threads

**Règle d'or en Delphi :** On ne peut **JAMAIS** accéder aux composants VCL (visuels) directement depuis un thread secondaire.

**Code incorrect :**

```pascal
procedure MonThread.Execute;  
begin  
  // ❌ DANGEREUX : Accès VCL depuis un thread
  Label1.Caption := 'Traitement...';
  Memo1.Lines.Add('Log');
end;
```

**Conséquence :** Crashes aléatoires, corruption de l'interface, comportement imprévisible.

**Code correct :**

```pascal
procedure MonThread.Execute;  
begin  
  // ✓ BON : Utiliser Synchronize
  TThread.Synchronize(nil, procedure
  begin
    Label1.Caption := 'Traitement...';
    Memo1.Lines.Add('Log');
  end);
end;
```

`Synchronize` exécute le code dans le thread principal (où la VCL peut être utilisée).

### 4. Fuites mémoire multi-thread

Les objets créés dans un thread doivent être libérés, mais la gestion devient complexe :

```pascal
procedure MonThread.Execute;  
var  
  Liste: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Travail avec Liste...

    // ❌ Problème : Comment passer Liste au thread principal ?
    TThread.Synchronize(nil, procedure
    begin
      AfficherListe(Liste);  // Liste existe encore ?
    end);
  finally
    Liste.Free;  // Libérée avant que Synchronize ne s'exécute ?
  end;
end;
```

**Solution correcte :**

```pascal
procedure MonThread.Execute;  
var  
  Liste: TStringList;
  Copie: TStringList;
begin
  Liste := TStringList.Create;
  try
    // Travail avec Liste...

    // Créer une copie pour le thread principal
    Copie := TStringList.Create;
    Copie.Assign(Liste);

    TThread.Queue(nil, procedure
    begin
      try
        AfficherListe(Copie);
      finally
        Copie.Free;  // Libérée dans le thread principal
      end;
    end);
  finally
    Liste.Free;
  end;
end;
```

### 5. Variables partagées non protégées

```pascal
var
  Resultat: Integer;  // Variable globale

// Thread 1
procedure Thread1;  
begin  
  Resultat := CalculComplexe1;  // Écriture
end;

// Thread 2
procedure Thread2;  
begin  
  Resultat := CalculComplexe2;  // Écriture
end;

// Thread principal
procedure AfficherResultat;  
begin  
  ShowMessage(IntToStr(Resultat));  // Lecture - quelle valeur ?
end;
```

**Problème :** Les deux threads écrivent dans `Resultat`. Le thread principal ne sait pas quand lire une valeur stable.

## Outils de débogage multi-thread dans Delphi

### Fenêtre Threads

La fenêtre **Threads** montre tous les threads actifs dans votre application.

**Comment l'ouvrir :**
1. Démarrez le débogage (F9)
2. Mettez en pause l'exécution (point d'arrêt ou pause manuelle)
3. Menu **View > Debug Windows > Threads**

**Ce que vous verrez :**

```
ID    Status      Location
-------------------------------------------
5432  Running     TMyThread.Execute
5433  Waiting     TWorkerThread.Execute
5434  Suspended   System.SysUtils
7890  Running     FormMain.ButtonClick   (← Thread principal)
```

**Informations affichées :**
- **ID** : Identifiant unique du thread
- **Status** : État actuel (Running, Waiting, Suspended)
- **Location** : Où se trouve le thread dans le code

**Actions possibles :**
- Double-cliquer sur un thread pour voir son code
- Clic droit pour geler/dégeler un thread
- Voir la pile d'appels de chaque thread

### Fenêtre Call Stack par thread

Chaque thread a sa propre pile d'appels.

**Voir la pile d'un thread spécifique :**
1. Sélectionnez le thread dans la fenêtre Threads
2. La fenêtre Call Stack affiche la pile de CE thread
3. Vous voyez la séquence d'appels qui a mené à l'état actuel

**Exemple de Call Stack :**

```
Thread 5432:
  TMyThread.TraiterDonnees (ligne 145)
    TMyThread.Execute (ligne 98)
      ThreadProc (System.Classes)
        BeginThread (System)
```

### Points d'arrêt spécifiques aux threads

Vous pouvez configurer un point d'arrêt pour ne se déclencher que sur un thread spécifique.

**Configuration :**
1. Placez un point d'arrêt (F5 ou clic dans la marge)
2. Clic droit sur le point d'arrêt > **Breakpoint Properties**
3. Dans la fenêtre, section **Thread**
4. Cochez **Break when hit in thread** et sélectionnez le thread

**Usage :** Utile quand vous avez 10 threads et que seul le thread #3 pose problème.

### Geler/Dégeler des threads

Pendant le débogage, vous pouvez "geler" certains threads pour simplifier l'analyse.

**Comment :**
1. Dans la fenêtre Threads
2. Clic droit sur un thread
3. Choisir **Freeze** (geler) ou **Thaw** (dégeler)

**Thread gelé :** Ne s'exécutera plus jusqu'à ce que vous le dégeliez.

**Cas d'usage :**
- Vous avez 5 threads worker et voulez tester seulement le thread #2
- Gelez les threads 1, 3, 4, 5
- Déboguez tranquillement le thread #2

**⚠️ Attention :** Geler un thread peut créer des deadlocks artificiels si d'autres threads attendent ce thread.

### Inspection de variables dans les threads

Les variables locales de chaque thread sont indépendantes.

**Voir les variables d'un thread :**
1. Sélectionnez le thread dans la fenêtre Threads
2. La fenêtre **Local Variables** montre SES variables locales
3. Les variables globales sont partagées entre tous les threads

**Exemple :**

```pascal
procedure TMyThread.Execute;  
var  
  CompteurLocal: Integer;  // Variable LOCALE à ce thread
begin
  CompteurLocal := 0;

  TThread.Synchronize(nil, procedure
  begin
    Inc(CompteurGlobal);  // Variable GLOBALE partagée
  end);
end;
```

- Chaque thread a son propre `CompteurLocal`
- Tous partagent le même `CompteurGlobal`

### Évaluation d'expressions par thread

Dans le **Evaluate/Modify** (Ctrl+F7), les expressions sont évaluées dans le contexte du thread actuellement sélectionné.

```
(Thread 5432 sélectionné)
Evaluate: MonObjet.FValeur  
Result: 42  

(Thread 5433 sélectionné)
Evaluate: MonObjet.FValeur  
Result: 42  (même objet si partagé)  
          ou différent si objet local
```

## Techniques de débogage multi-thread

### 1. Logging thread-safe

Le logging est votre meilleur ami en multi-threading.

**Logger thread-safe simple :**

```pascal
type
  TThreadSafeLogger = class
  private
    FLock: TCriticalSection;
    FLogFile: TextFile;
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure Log(const Message: string);
  end;

constructor TThreadSafeLogger.Create(const FileName: string);  
begin  
  FLock := TCriticalSection.Create;
  AssignFile(FLogFile, FileName);
  Rewrite(FLogFile);
end;

destructor TThreadSafeLogger.Destroy;  
begin  
  CloseFile(FLogFile);
  FLock.Free;
  inherited;
end;

procedure TThreadSafeLogger.Log(const Message: string);  
var  
  ThreadID: TThreadID;
begin
  ThreadID := TThread.CurrentThread.ThreadID;

  FLock.Enter;
  try
    WriteLn(FLogFile, Format('[%s] Thread %d: %s',
                             [FormatDateTime('hh:nn:ss.zzz', Now),
                              ThreadID,
                              Message]));
    Flush(FLogFile);
  finally
    FLock.Leave;
  end;
end;
```

**Usage :**

```pascal
var
  Logger: TThreadSafeLogger;

procedure TMyThread.Execute;  
begin  
  Logger.Log('Thread démarré');

  TraiterDonnees;
  Logger.Log('Traitement terminé');

  Logger.Log('Thread terminé');
end;
```

**Fichier log résultant :**

```
[14:32:15.234] Thread 5432: Thread démarré
[14:32:15.237] Thread 5433: Thread démarré
[14:32:15.891] Thread 5432: Traitement terminé
[14:32:16.103] Thread 5433: Traitement terminé
[14:32:16.104] Thread 5432: Thread terminé
[14:32:16.106] Thread 5433: Thread terminé
```

Vous voyez clairement l'ordre d'exécution et les entrelacements.

### 2. Assertions thread-safe

Utilisez des assertions pour vérifier vos hypothèses :

```pascal
procedure TMyThread.Execute;  
begin  
  Assert(TThread.CurrentThread.ThreadID <> MainThreadID,
         'Cette méthode ne doit PAS être appelée depuis le thread principal');

  // Travail...

  FLock.Enter;
  try
    Assert(FCompteur >= 0, 'Le compteur ne peut pas être négatif');
    Dec(FCompteur);
  finally
    FLock.Leave;
  end;
end;
```

### 3. Simuler la lenteur

Ajoutez des `Sleep()` pour ralentir l'exécution et rendre les race conditions plus visibles :

```pascal
procedure TMyThread.Execute;  
begin  
  FLock.Enter;
  try
    Valeur := Valeur + 1;

    Sleep(10);  // ← Artificiel, pour tester

    Traiter(Valeur);
  finally
    FLock.Leave;
  end;
end;
```

Si un bug apparaît avec `Sleep(10)`, c'est qu'il existe sans Sleep aussi, mais plus rare.

### 4. Tests de stress

Créez de nombreux threads pour augmenter la probabilité de race conditions :

```pascal
procedure TesterAvecStress;  
var  
  i: Integer;
  Threads: array[0..99] of TThread;
begin
  // Créer 100 threads
  for i := 0 to 99 do
  begin
    Threads[i] := TThread.CreateAnonymousThread(procedure
    begin
      // Code à tester
      MonOperationCritique;
    end);
    Threads[i].Start;
  end;

  // Attendre que tous terminent
  for i := 0 to 99 do
    Threads[i].WaitFor;
end;
```

Plus vous avez de threads, plus les problèmes de concurrence apparaîtront rapidement.

### 5. Compteurs de débogage

Utilisez des compteurs pour tracer l'exécution :

```pascal
var
  AppelsEntree: Integer = 0;
  AppelsSortie: Integer = 0;

procedure SectionCritique;  
begin  
  AtomicIncrement(AppelsEntree);
  try
    // Code critique
  finally
    AtomicIncrement(AppelsSortie);
  end;
end;

// À la fin, vérifier
Assert(AppelsEntree = AppelsSortie,
       'Nombre d''entrées doit égaler nombre de sorties');
```

### 6. Thread Sanitizer (conceptuel)

Bien qu'il n'existe pas de Thread Sanitizer natif pour Delphi comme pour C++, vous pouvez implémenter des vérifications similaires :

```pascal
type
  TAccessTracker = class
  private
    FLastAccessThreadID: TThreadID;
    FAccessCount: Integer;
  public
    procedure RecordAccess;
    function DetectRace: Boolean;
  end;

procedure TAccessTracker.RecordAccess;  
var  
  CurrentThread: TThreadID;
begin
  CurrentThread := TThread.CurrentThread.ThreadID;

  if (FLastAccessThreadID <> 0) and
     (FLastAccessThreadID <> CurrentThread) and
     (FAccessCount > 0) then
  begin
    // Accès concurrent détecté !
    raise Exception.Create('Race condition détectée !');
  end;

  FLastAccessThreadID := CurrentThread;
  Inc(FAccessCount);
end;
```

## Détecter les deadlocks

### Symptômes d'un deadlock

- Application complètement figée
- Pas de réponse aux clics
- CPU proche de 0%
- Pas de message d'erreur

### Diagnostic avec le débogueur

**Étapes :**

1. **Mettre en pause** le programme figé (bouton Pause ou Break)

2. **Ouvrir la fenêtre Threads** (View > Debug Windows > Threads)

3. **Examiner chaque thread :**
   - Quels sont en état "Waiting" ?
   - Où attendent-ils ?

4. **Voir les piles d'appels :**
   - Double-cliquer sur chaque thread waiting
   - Regarder la Call Stack

**Exemple de ce que vous pourriez voir :**

```
Thread 5432:
  TCriticalSection.Enter (System.SyncObjs)
    TManager.ProcessA (ligne 145) ← Attend Lock2

Thread 5433:
  TCriticalSection.Enter (System.SyncObjs)
    TManager.ProcessB (ligne 198) ← Attend Lock1
```

**Analyse :** Thread 5432 attend Lock2, Thread 5433 attend Lock1 → **Deadlock !**

### Prévention des deadlocks

**Règle 1 : Ordre d'acquisition cohérent**

```pascal
// ✓ BON : Toujours le même ordre partout
procedure Operation1;  
begin  
  LockA.Enter;
  try
    LockB.Enter;
    try
      // ...
    finally
      LockB.Leave;
    end;
  finally
    LockA.Leave;
  end;
end;

procedure Operation2;  
begin  
  LockA.Enter;  // Même ordre : A puis B
  try
    LockB.Enter;
    try
      // ...
    finally
      LockB.Leave;
    end;
  finally
    LockA.Leave;
  end;
end;
```

**Règle 2 : Timeouts**

```pascal
if Lock.TryEnter(1000) then  // Attendre max 1 seconde  
begin  
  try
    // Travail...
  finally
    Lock.Leave;
  end;
end  
else  
  raise Exception.Create('Timeout : deadlock possible');
```

**Règle 3 : Limiter la portée des locks**

```pascal
// ❌ MAUVAIS : Lock tenu longtemps
Lock.Enter;  
try  
  TraitementLong;  // 10 secondes
  AccesRapide;
finally
  Lock.Leave;
end;

// ✓ BON : Lock minimal
TraitementLong;  // Sans lock  
Lock.Enter;  
try  
  AccesRapide;  // Seulement ici
finally
  Lock.Leave;
end;
```

## Détecter les race conditions

### Symptômes

- Résultats incohérents
- Données corrompues
- Comportement aléatoire
- Difficile à reproduire

### Technique : Ajouter des assertions

```pascal
type
  TCompteThreadSafe = class
  private
    FSolde: Currency;
    FLock: TCriticalSection;
  public
    procedure Crediter(Montant: Currency);
    procedure Debiter(Montant: Currency);
    function ObtenirSolde: Currency;
  end;

procedure TCompteThreadSafe.Debiter(Montant: Currency);  
var  
  SoldeAvant, SoldeApres: Currency;
begin
  FLock.Enter;
  try
    SoldeAvant := FSolde;

    FSolde := FSolde - Montant;

    SoldeApres := FSolde;

    // Assertion pour détecter les incohérences
    Assert(SoldeApres = SoldeAvant - Montant,
           'Race condition : solde incohérent !');
  finally
    FLock.Leave;
  end;
end;
```

### Technique : Compteurs de vérification

```pascal
var
  TotalCredits: Integer = 0;
  TotalDebits: Integer = 0;

procedure Crediter(Montant: Currency);  
begin  
  AtomicIncrement(TotalCredits);
  // ... opération ...
end;

procedure Debiter(Montant: Currency);  
begin  
  AtomicIncrement(TotalDebits);
  // ... opération ...
end;

// Vérification finale
procedure VerifierCoherence;  
begin  
  Assert(TotalCredits + TotalDebits = NombreOperationsAttendu,
         'Certaines opérations ont été perdues (race condition)');
end;
```

### Technique : Instrumenter le code

Ajoutez du code qui détecte les accès concurrents :

```pascal
type
  TProtectedData = class
  private
    FData: Integer;
    FCurrentOwner: TThreadID;
  public
    procedure BeginAccess;
    procedure EndAccess;
    procedure SetValue(Value: Integer);
  end;

procedure TProtectedData.BeginAccess;  
begin  
  if FCurrentOwner <> 0 then
    raise Exception.Create('Accès concurrent détecté !');
  FCurrentOwner := TThread.CurrentThread.ThreadID;
end;

procedure TProtectedData.EndAccess;  
begin  
  FCurrentOwner := 0;
end;

procedure TProtectedData.SetValue(Value: Integer);  
begin  
  BeginAccess;
  try
    FData := Value;
  finally
    EndAccess;
  end;
end;
```

## Outils et techniques avancés

### OutputDebugString thread-safe

Pour un logging rapide pendant le débogage :

```pascal
procedure LogThread(const Message: string);  
begin  
  OutputDebugString(PChar(Format('[Thread %d] %s',
                                 [TThread.CurrentThread.ThreadID,
                                  Message])));
end;

// Usage
procedure TMyThread.Execute;  
begin  
  LogThread('Début du traitement');
  // ...
  LogThread('Fin du traitement');
end;
```

Visible dans la fenêtre **Event Log** de Delphi.

### Analyse post-mortem avec dump

Si l'application plante en production :

1. Configurer Windows pour créer des crash dumps
2. Analyser le dump avec WinDbg ou similaire
3. Voir l'état de tous les threads au moment du crash

```
!threads  # Liste tous les threads
~*k       # Stack trace de tous les threads
```

### Profilage multi-thread

Utilisez des outils de profilage (AQtime, Sampling Profiler) pour :
- Identifier les contentions de locks
- Voir quels threads attendent le plus
- Détecter les déséquilibres de charge

## Patterns de code thread-safe

### Pattern : Lock-Free avec Interlocked

Pour les opérations simples, utilisez les fonctions atomiques :

```pascal
var
  Compteur: Integer;

// ✓ Thread-safe sans lock
procedure Incrementer;  
begin  
  AtomicIncrement(Compteur);
end;

// ✓ Thread-safe
function CompareAndSwap(var Target: Integer;
                        NewValue, Comparand: Integer): Integer;
begin
  Result := TInterlocked.CompareExchange(Target, NewValue, Comparand);
end;
```

**Avantages :**
- Plus rapide que les locks
- Pas de risque de deadlock

**Limitations :**
- Uniquement pour opérations simples
- Entiers et pointeurs seulement

### Pattern : Double-Checked Locking

Pour une initialisation thread-safe paresseuse :

```pascal
var
  Instance: TMonSingleton = nil;
  Lock: TCriticalSection;

function ObtenirInstance: TMonSingleton;  
begin  
  if Instance = nil then  // Premier check (rapide, sans lock)
  begin
    Lock.Enter;
    try
      if Instance = nil then  // Second check (avec lock)
        Instance := TMonSingleton.Create;
    finally
      Lock.Leave;
    end;
  end;
  Result := Instance;
end;
```

### Pattern : Producer-Consumer

Pour communiquer entre threads de manière sûre :

```pascal
type
  TQueueThreadSafe<T> = class
  private
    FQueue: TQueue<T>;
    FLock: TCriticalSection;
    FEvent: TEvent;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Enqueue(Item: T);
    function Dequeue(out Item: T; Timeout: Cardinal): Boolean;
  end;

// Producer thread
procedure Producer;  
var  
  Item: TWorkItem;
begin
  Item := CreerItem;
  Queue.Enqueue(Item);
end;

// Consumer thread
procedure Consumer;  
var  
  Item: TWorkItem;
begin
  if Queue.Dequeue(Item, 1000) then
  begin
    TraiterItem(Item);
  end;
end;
```

### Pattern : Thread Pool

Plutôt que créer/détruire des threads constamment :

```pascal
type
  TThreadPool = class
  private
    FThreads: TList<TThread>;
    FTasks: TThreadedQueue<TProc>;
  public
    constructor Create(NumThreads: Integer);
    destructor Destroy; override;

    procedure QueueTask(Task: TProc);
  end;

// Usage
ThreadPool.QueueTask(procedure  
begin  
  TraiterDonnees;
end);
```

Delphi fournit `TThreadPool` dans System.Threading.

### Pattern : Immutable Data

Les données immuables (non modifiables) sont naturellement thread-safe :

```pascal
type
  TConfigImmutable = class
  private
    FServeur: string;
    FPort: Integer;
  public
    constructor Create(const Serveur: string; Port: Integer);
    property Serveur: string read FServeur;
    property Port: Integer read FPort;
    // Pas de setters - immuable !
  end;
```

Plusieurs threads peuvent lire sans locks.

## Checklist de débogage multi-thread

Utilisez cette checklist pour diagnostiquer les problèmes :

**□ Identification du problème**
- [ ] Le bug est-il intermittent ou systématique ?
- [ ] Apparaît-il uniquement avec plusieurs threads actifs ?
- [ ] L'application se fige-t-elle complètement ou partiellement ?
- [ ] Y a-t-il un message d'erreur ou un crash silencieux ?

**□ Inspection des threads**
- [ ] Combien de threads sont actifs ?
- [ ] Quel est l'état de chaque thread (Running, Waiting) ?
- [ ] Où se trouve chaque thread dans le code ?
- [ ] Y a-t-il des threads en attente infinie ?

**□ Variables partagées**
- [ ] Quelles variables sont partagées entre threads ?
- [ ] Ces variables sont-elles protégées par des locks ?
- [ ] Les locks sont-ils toujours relâchés (finally) ?
- [ ] Y a-t-il des accès VCL depuis des threads ?

**□ Synchronisation**
- [ ] Les locks sont-ils acquis dans le même ordre partout ?
- [ ] Y a-t-il des deadlocks possibles ?
- [ ] TThread.Synchronize est-il utilisé pour accéder à la VCL ?
- [ ] Les objets partagés sont-ils thread-safe ?

**□ Logging et instrumentation**
- [ ] Y a-t-il du logging thread-safe ?
- [ ] Les logs montrent-ils l'ordre d'exécution ?
- [ ] Les assertions sont-elles en place ?
- [ ] Les compteurs de vérification sont-ils corrects ?

**□ Tests**
- [ ] Le problème est-il reproductible avec du stress testing ?
- [ ] L'ajout de Sleep() révèle-t-il le bug ?
- [ ] Les tests avec moins/plus de threads changent-ils le comportement ?

## Conseils pour débutants

### 1. Commencez simple

Ne vous lancez pas immédiatement dans des architectures multi-thread complexes. Commencez par :
- Un seul thread worker
- Des tâches simples
- Synchronisation basique

### 2. Utilisez TTask pour commencer

`TTask` de Delphi gère beaucoup de complexité pour vous :

```pascal
// Simple et sûr
TTask.Run(procedure  
begin  
  // Votre code ici
  TraiterDonnees;

  // Mise à jour UI
  TThread.Synchronize(nil, procedure
  begin
    Label1.Caption := 'Terminé';
  end);
end);
```

### 3. Ne réinventez pas la roue

Utilisez les classes thread-safe existantes :
- `TThreadList<T>` : Liste thread-safe
- `TThreadedQueue<T>` : Queue thread-safe
- `TMonitor` : Alternative aux critical sections

### 4. Testez tôt et souvent

Ne découvrez pas les bugs multi-thread en production. Testez dès le début avec :
- Plusieurs threads
- Stress testing
- Sleep() artificiel

### 5. Documentez vos threads

```pascal
/// <summary>
/// Thread worker qui traite les commandes en arrière-plan.
/// Thread-safe : Oui
/// Accès VCL : Via Synchronize uniquement
/// Durée de vie : Créé au démarrage, détruit à la fermeture
/// </summary>
type
  TCommandeWorkerThread = class(TThread)
```

### 6. Un thread à la fois

Lors du débogage, gelez tous les threads sauf celui qui vous intéresse. Déboguez-le comme du code normal, puis passez au suivant.

### 7. Évitez les variables globales

Les variables globales partagées sont la source n°1 de problèmes multi-thread.

```pascal
// ❌ MAUVAIS
var
  CompteurGlobal: Integer;

// ✓ BON
type
  TManager = class
  private
    FCompteur: Integer;
    FLock: TCriticalSection;
  public
    function IncrementCompteur: Integer;
  end;
```

### 8. Comprenez avant d'optimiser

Le code multi-thread est plus difficile à comprendre et déboguer. N'utilisez le multi-threading que quand :
- Vous en avez vraiment besoin (performance, réactivité)
- Vous comprenez les implications
- Vous pouvez le tester correctement

### 9. Lisez les erreurs attentivement

Les exceptions multi-thread donnent souvent des indices :
- "Access violation" → Accès à objet libéré
- "Cannot call VCL from thread" → Accès VCL incorrect
- Pas d'erreur mais freeze → Deadlock probable

### 10. Demandez de l'aide

Le débogage multi-thread est difficile même pour les experts. N'hésitez pas à :
- Montrer votre code à un collègue
- Poster sur les forums (avec du code minimal reproductible)
- Utiliser le rubber duck debugging (expliquer à haute voix)

## Outils externes utiles

### Visual Studio Concurrency Visualizer

Si vous avez Visual Studio, utilisez son visualiseur de concurrence pour voir :
- Chronologie d'exécution des threads
- Contentions de locks
- Utilisation CPU par thread

### WinDbg

Pour l'analyse post-mortem :
- Analyse de crash dumps
- Inspection détaillée des threads
- Commandes puissantes pour le débogage multi-thread

### Process Monitor (Sysinternals)

Pour voir les interactions système en temps réel :
- Opérations fichiers
- Opérations réseau
- Par thread

## Cas d'étude : Déboguer un deadlock

### Scénario

Votre application se fige aléatoirement après quelques minutes d'utilisation.

### Étapes de diagnostic

**1. Reproduire le problème**

- Exécuter l'application en débogage
- Attendre qu'elle se fige
- Ne pas la fermer !

**2. Mettre en pause**

- Cliquer sur Pause dans Delphi
- Ou Ctrl+Alt+Break

**3. Examiner les threads**

View > Debug Windows > Threads

```
Thread ID    Status      Location
------------------------------------
1234        Waiting     TCriticalSection.Enter
5678        Waiting     TCriticalSection.Enter
9012        Running     Application.ProcessMessages
```

Threads 1234 et 5678 sont en "Waiting" → suspect !

**4. Call Stack du thread 1234**

```
TCriticalSection.Enter  
TOrderManager.ProcessOrder ligne 145  
  → Attend FLockB
```

**5. Call Stack du thread 5678**

```
TCriticalSection.Enter  
TOrderManager.UpdateInventory ligne 287  
  → Attend FLockA
```

**6. Analyse**

Thread 1234 attend LockB et détient LockA  
Thread 5678 attend LockA et détient LockB  

→ **Deadlock classique !**

**7. Solution**

Modifier le code pour acquérir les locks dans le même ordre :

```pascal
// Avant (deadlock possible)
procedure ProcessOrder;  
begin  
  FLockA.Enter;
  try
    FLockB.Enter;  // Ordre différent selon la méthode
    try
      // ...
    finally
      FLockB.Leave;
    end;
  finally
    FLockA.Leave;
  end;
end;

// Après (cohérent)
procedure ProcessOrder;  
begin  
  FLockA.Enter;  // Toujours A puis B
  try
    FLockB.Enter;
    try
      // ...
    finally
      FLockB.Leave;
    end;
  finally
    FLockA.Leave;
  end;
end;

procedure UpdateInventory;  
begin  
  FLockA.Enter;  // Même ordre : A puis B
  try
    FLockB.Enter;
    try
      // ...
    finally
      FLockB.Leave;
    end;
  finally
    FLockA.Leave;
  end;
end;
```

## Ressources pour approfondir

### Documentation Delphi

- DocWiki Embarcadero : Threading et synchronisation
- Exemples de code dans les samples Delphi

### Livres recommandés

- "Parallel Programming with Delphi" par Marco Cantù
- "Delphi High Performance" par Primož Gabrijelčič

### Forums et communautés

- DelphiPraxis.net : Section Threading
- Stack Overflow : Tag [delphi] + [multithreading]
- Embarcadero Forums : Delphi RTL section

### Blogs

- Primož Gabrijelčič : Expert en multi-threading Delphi
- Marco Cantù : Auteur et expert Delphi

## Conclusion

Le débogage de code multi-thread est l'un des défis les plus complexes en programmation, mais avec les bons outils, techniques et patience, même les bugs les plus obscurs peuvent être résolus.

**Points clés à retenir :**

**Non-déterminisme :** L'exécution multi-thread est imprévisible. Acceptez-le et utilisez des techniques qui gèrent cette imprévisibilité.

**Outils Delphi :** Maîtrisez la fenêtre Threads, Call Stack, et les points d'arrêt spécifiques aux threads.

**Logging :** Un bon système de logging thread-safe est essentiel. Loggez généreusement.

**Synchronisation :** Toujours protéger les données partagées avec des locks ou utiliser des opérations atomiques.

**VCL et threads :** Utilisez TOUJOURS Synchronize/Queue pour accéder à la VCL depuis un thread.

**Deadlocks :** Acquérez les locks dans un ordre cohérent et utilisez des timeouts.

**Race conditions :** Utilisez des assertions, des compteurs de vérification et du stress testing.

**Simplicité :** Le meilleur code multi-thread est celui que vous n'avez pas à écrire. Utilisez TTask et les classes thread-safe de Delphi.

**Patience :** Les bugs multi-thread sont difficiles à trouver et à corriger. Prenez votre temps, utilisez une approche méthodique.

**Tests :** Testez avec de nombreux threads, du stress, et des conditions variées. Les bugs multi-thread se cachent bien.

En développant votre expertise en débogage multi-thread, vous devenez capable de créer des applications Delphi performantes et fiables qui exploitent pleinement les capacités des processeurs modernes multi-cœurs. C'est une compétence avancée qui vous distingue en tant que développeur professionnel, capable de résoudre les problèmes les plus difficiles et de livrer des logiciels de haute qualité même dans des environnements concurrents complexes.

⏭️ [Couverture de code et qualité](/12-debogage-et-tests/10-couverture-de-code-et-qualite.md)
