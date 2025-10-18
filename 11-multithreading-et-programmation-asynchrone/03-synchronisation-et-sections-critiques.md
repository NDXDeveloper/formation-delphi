🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.3 Synchronisation et sections critiques

## Le problème de l'accès concurrent

Lorsque plusieurs threads accèdent simultanément aux mêmes données, des problèmes peuvent survenir. Imaginez deux personnes qui essaient de modifier le même document Word en même temps : le chaos est garanti !

### Exemple du problème

Prenons un compteur simple que deux threads incrémentent :

```pascal
var
  Compteur: Integer = 0;

// Thread 1 et Thread 2 exécutent ce code
Inc(Compteur); // Compteur := Compteur + 1;
```

**Ce qui devrait se passer** : Si chaque thread incrémente 1000 fois, on devrait avoir 2000 à la fin.

**Ce qui peut réellement arriver** : On peut obtenir 1847, 1923, ou n'importe quelle valeur inférieure à 2000 !

### Pourquoi ce problème survient-il ?

L'opération `Inc(Compteur)` semble simple, mais en réalité elle se décompose en plusieurs étapes :

1. Lire la valeur actuelle de Compteur (exemple : 100)
2. Ajouter 1 à cette valeur (100 + 1 = 101)
3. Écrire le résultat dans Compteur (101)

**Le problème** : Un autre thread peut s'exécuter entre ces étapes !

```
Thread 1 : Lit Compteur (100)
Thread 2 : Lit Compteur (100)     ← Lit la même valeur !
Thread 1 : Calcule 100 + 1 = 101
Thread 2 : Calcule 100 + 1 = 101  ← Calcule la même chose !
Thread 1 : Écrit 101 dans Compteur
Thread 2 : Écrit 101 dans Compteur ← Écrase avec la même valeur !
```

Résultat : Au lieu de 102, on a 101. Une incrémentation a été "perdue" !

## Qu'est-ce que la synchronisation ?

La **synchronisation** consiste à coordonner l'accès aux ressources partagées pour éviter ces conflits. C'est comme installer un verrou sur une porte : une seule personne peut entrer à la fois.

### Analogie : La salle de bain

Imaginez une salle de bain partagée dans une maison :
- **Sans verrou** : Plusieurs personnes peuvent entrer en même temps → chaos
- **Avec verrou** : Une personne entre, ferme à clé, utilise la salle de bain, puis déverrouille en sortant → ordre

En programmation, c'est exactement le même principe !

## Les sections critiques (TCriticalSection)

Une **section critique** est une zone de code où un seul thread peut s'exécuter à la fois. En Delphi, on utilise la classe `TCriticalSection`.

### Déclaration et utilisation de base

```pascal
uses
  System.SyncObjs; // N'oubliez pas cette unité !

var
  SectionCritique: TCriticalSection;
  Compteur: Integer;

initialization
  SectionCritique := TCriticalSection.Create;
  Compteur := 0;

finalization
  SectionCritique.Free;
```

### Protéger une ressource partagée

```pascal
procedure TMonThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // Entrer dans la section critique
    SectionCritique.Enter;
    try
      // Code protégé : un seul thread peut être ici à la fois
      Inc(Compteur);
    finally
      // Sortir de la section critique (TOUJOURS dans finally !)
      SectionCritique.Leave;
    end;
  end;
end;
```

**Important** : Utilisez toujours `try...finally` pour garantir que `Leave` est appelé, même en cas d'exception.

### Fonctionnement détaillé

```pascal
// Thread 1 arrive
SectionCritique.Enter;  // "Je ferme la porte à clé"
  Inc(Compteur);        // Fait son travail tranquillement
SectionCritique.Leave;  // "Je déverrouille et je sors"

// Thread 2 arrive pendant que Thread 1 est à l'intérieur
SectionCritique.Enter;  // "La porte est verrouillée, j'attends..."
                        // Thread 2 est bloqué ici jusqu'à ce que Thread 1 sorte
```

## Exemple complet avec TCriticalSection

```pascal
type
  TThreadCompteur = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

var
  Form1: TForm1;
  CS: TCriticalSection;
  CompteurGlobal: Integer;

implementation

constructor TThreadCompteur.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
end;

procedure TThreadCompteur.Execute;
var
  i: Integer;
  ValeurLocale: Integer;
begin
  for i := 1 to 1000 do
  begin
    if Terminated then Exit;

    // Protection de l'accès au compteur partagé
    CS.Enter;
    try
      Inc(CompteurGlobal);
      ValeurLocale := CompteurGlobal;
    finally
      CS.Leave;
    end;

    // Mise à jour de l'interface (en dehors de la section critique)
    if (ValeurLocale mod 100) = 0 then
    begin
      Synchronize(
        procedure
        begin
          Form1.Label1.Caption := IntToStr(ValeurLocale);
        end
      );
    end;
  end;
end;

// Dans le formulaire
procedure TForm1.FormCreate(Sender: TObject);
begin
  CS := TCriticalSection.Create;
  CompteurGlobal := 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CS.Free;
end;

procedure TForm1.ButtonDemarrerClick(Sender: TObject);
var
  i: Integer;
begin
  // Créer plusieurs threads qui incrémentent le même compteur
  for i := 1 to 5 do
    TThreadCompteur.Create;
end;
```

## TryEnter : Entrer sans attendre

Parfois, vous ne voulez pas que le thread attende. `TryEnter` tente d'entrer et retourne immédiatement :

```pascal
procedure TMonThread.Execute;
begin
  if SectionCritique.TryEnter then
  begin
    try
      // La section critique est disponible, on fait notre travail
      Inc(Compteur);
    finally
      SectionCritique.Leave;
    end;
  end
  else
  begin
    // La section critique est occupée, on fait autre chose
    // ou on réessaie plus tard
  end;
end;
```

## Autres mécanismes de synchronisation

### TMonitor : Synchronisation simplifiée

Depuis Delphi 2009, `TMonitor` offre une alternative plus moderne :

```pascal
var
  MonObjet: TObject;

// Dans un thread
TMonitor.Enter(MonObjet);
try
  // Section critique
  Inc(Compteur);
finally
  TMonitor.Exit(MonObjet);
end;
```

Avantage : Pas besoin de créer un objet TCriticalSection séparé.

### TMutex : Synchronisation inter-processus

Un **Mutex** (Mutual Exclusion) peut synchroniser des threads de différents processus :

```pascal
uses
  System.SyncObjs;

var
  Mutex: TMutex;

begin
  // Créer un mutex nommé
  Mutex := TMutex.Create(nil, False, 'MonApplicationUnique');
  try
    // Tenter d'acquérir le mutex
    if Mutex.WaitFor(0) = wrSignaled then
    begin
      try
        // Nous avons le mutex, l'application peut démarrer
        Application.Run;
      finally
        Mutex.Release;
      end;
    end
    else
      ShowMessage('L''application est déjà en cours d''exécution !');
  finally
    Mutex.Free;
  end;
end;
```

### TEvent : Signaler entre threads

Un **Event** permet à un thread de signaler un événement à d'autres threads :

```pascal
var
  Event: TEvent;

// Thread 1 : Attendre un signal
procedure TThread1.Execute;
begin
  Event.WaitFor(INFINITE); // Attendre indéfiniment
  // Le signal est reçu, continuer...
end;

// Thread 2 : Envoyer un signal
procedure TThread2.Execute;
begin
  // Faire quelque chose...
  Event.SetEvent; // Signaler aux threads en attente
end;
```

### TSemaphore : Limiter le nombre d'accès

Un **Sémaphore** limite le nombre de threads pouvant accéder à une ressource :

```pascal
var
  Semaphore: TSemaphore;

initialization
  // Permettre à 3 threads maximum d'accéder simultanément
  Semaphore := TSemaphore.Create(nil, 3, 3, '');

// Dans un thread
procedure TMonThread.Execute;
begin
  Semaphore.Acquire; // Attendre une place disponible
  try
    // Maximum 3 threads peuvent être ici en même temps
    // Accéder à la ressource limitée
  finally
    Semaphore.Release; // Libérer une place
  end;
end;
```

## Variables locales aux threads (TThreadLocalStorage)

Parfois, chaque thread doit avoir sa propre copie d'une variable :

```pascal
var
  TLS: TThreadLocalStorage;

initialization
  TLS := TThreadLocalStorage.Create;

// Dans un thread
procedure TMonThread.Execute;
begin
  // Chaque thread a sa propre valeur
  TLS.Value := Pointer(ThreadID);

  // Récupérer la valeur spécifique à ce thread
  MaValeur := Integer(TLS.Value);
end;
```

## Bonnes pratiques de synchronisation

### 1. Minimiser le temps dans les sections critiques

```pascal
// ❌ MAUVAIS : Trop de code dans la section critique
CS.Enter;
try
  Lire_Donnees_Du_Disque();     // Opération lente !
  Traiter_Donnees();             // Opération lente !
  Inc(Compteur);
finally
  CS.Leave;
end;

// ✅ BON : Seulement le nécessaire
Lire_Donnees_Du_Disque();       // En dehors
Traiter_Donnees();               // En dehors

CS.Enter;
try
  Inc(Compteur);                 // Rapide et protégé
finally
  CS.Leave;
end;
```

### 2. Toujours libérer dans un bloc finally

```pascal
// ✅ CORRECT
CS.Enter;
try
  // Code protégé
finally
  CS.Leave; // Garanti d'être appelé
end;
```

### 3. Éviter les deadlocks

Un **deadlock** (étreinte fatale) survient quand deux threads s'attendent mutuellement :

```pascal
// Thread 1
CS1.Enter;
  CS2.Enter;  // Attend CS2
    // ...
  CS2.Leave;
CS1.Leave;

// Thread 2
CS2.Enter;
  CS1.Enter;  // Attend CS1 → DEADLOCK !
    // ...
  CS1.Leave;
CS2.Leave;
```

**Solution** : Toujours acquérir les verrous dans le même ordre :

```pascal
// Les deux threads acquièrent d'abord CS1, puis CS2
// Thread 1
CS1.Enter;
  CS2.Enter;
    // ...
  CS2.Leave;
CS1.Leave;

// Thread 2
CS1.Enter;  // Même ordre !
  CS2.Enter;
    // ...
  CS2.Leave;
CS1.Leave;
```

### 4. Éviter les sections critiques imbriquées

Limitez l'imbrication des sections critiques pour réduire les risques de deadlock.

### 5. Documenter les ressources partagées

```pascal
var
  CompteurGlobal: Integer;     // Protégé par CS_Compteur
  CS_Compteur: TCriticalSection;
```

## Exemple pratique : File d'attente thread-safe

```pascal
type
  TFileThreadSafe = class
  private
    FListe: TList<Integer>;
    FCS: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ajouter(AValeur: Integer);
    function Retirer: Integer;
    function EstVide: Boolean;
  end;

constructor TFileThreadSafe.Create;
begin
  inherited;
  FListe := TList<Integer>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TFileThreadSafe.Destroy;
begin
  FCS.Free;
  FListe.Free;
  inherited;
end;

procedure TFileThreadSafe.Ajouter(AValeur: Integer);
begin
  FCS.Enter;
  try
    FListe.Add(AValeur);
  finally
    FCS.Leave;
  end;
end;

function TFileThreadSafe.Retirer: Integer;
begin
  FCS.Enter;
  try
    if FListe.Count > 0 then
    begin
      Result := FListe[0];
      FListe.Delete(0);
    end
    else
      Result := -1; // Valeur par défaut si vide
  finally
    FCS.Leave;
  end;
end;

function TFileThreadSafe.EstVide: Boolean;
begin
  FCS.Enter;
  try
    Result := FListe.Count = 0;
  finally
    FCS.Leave;
  end;
end;
```

## Quand utiliser la synchronisation ?

### Vous DEVEZ synchroniser si :

- Plusieurs threads modifient la même variable
- Un thread lit pendant qu'un autre modifie
- Vous accédez à des structures de données non thread-safe (listes, dictionnaires, etc.)
- Vous accédez à des ressources externes partagées (fichiers, bases de données)

### Vous n'avez PAS besoin de synchroniser si :

- Chaque thread travaille sur ses propres données
- Les données sont en lecture seule
- Vous utilisez des structures thread-safe (comme `TThreadList`)

## Tableaux de comparaison des mécanismes

| Mécanisme | Usage principal | Portée |
|-----------|----------------|--------|
| TCriticalSection | Protection de sections de code | Même processus |
| TMonitor | Alternative moderne à TCriticalSection | Même processus |
| TMutex | Synchronisation inter-processus | Inter-processus |
| TEvent | Signalisation entre threads | Même processus |
| TSemaphore | Limiter le nombre d'accès | Même/Inter-processus |

## Points clés à retenir

- L'accès concurrent aux données partagées peut causer des bugs imprévisibles
- Utilisez `TCriticalSection` pour protéger les ressources partagées
- Toujours utiliser `try...finally` avec `Enter` et `Leave`
- Minimisez le temps passé dans les sections critiques
- Attention aux deadlocks lors de l'utilisation de plusieurs verrous
- Documentez clairement quelles ressources sont protégées et par quoi
- Préférez les structures thread-safe quand elles existent
- TMonitor offre une alternative moderne à TCriticalSection

Dans la prochaine section, nous verrons comment Delphi simplifie encore la programmation parallèle avec TTask et la bibliothèque Parallel Programming Library (PPL).

⏭️ [TTask et programmation parallèle](/11-multithreading-et-programmation-asynchrone/04-ttask-et-programmation-parallele.md)
