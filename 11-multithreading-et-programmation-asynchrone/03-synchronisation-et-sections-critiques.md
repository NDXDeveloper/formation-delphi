# 11.3 Synchronisation et sections critiques

🔝 Retour à la [Table des matières](/SOMMAIRE.md)

## Introduction

Lorsque vous utilisez plusieurs threads dans votre application, ils peuvent accéder simultanément aux mêmes ressources (variables, fichiers, connexions réseau, etc.). Ces accès concurrents peuvent provoquer des problèmes difficiles à détecter et à résoudre. La synchronisation est un concept essentiel qui vous permet de coordonner les threads et d'éviter ces problèmes.

Dans ce chapitre, nous allons découvrir les techniques de synchronisation en Delphi, en commençant par les sections critiques, l'outil le plus couramment utilisé.

## Le problème de l'accès concurrent

Pour comprendre l'importance de la synchronisation, examinons un problème classique :

Imaginons deux threads qui incrémentent une même variable globale :

```pascal
var
  CompteurGlobal: Integer = 0;

// Dans le Thread 1
CompteurGlobal := CompteurGlobal + 1;

// Dans le Thread 2 (en même temps)
CompteurGlobal := CompteurGlobal + 1;
```

À première vue, on pourrait penser que `CompteurGlobal` finira avec la valeur 2. Mais ce n'est pas toujours le cas ! Voici pourquoi :

1. Le Thread 1 lit la valeur de `CompteurGlobal` (0)
2. Le Thread 2 lit également la valeur de `CompteurGlobal` (0)
3. Le Thread 1 ajoute 1 et écrit 1 dans `CompteurGlobal`
4. Le Thread 2 ajoute 1 à sa valeur lue (0) et écrit également 1 dans `CompteurGlobal`

Résultat final : `CompteurGlobal` vaut 1, et non 2 comme prévu !

Ce phénomène s'appelle une **condition de course** (race condition). Il se produit lorsque le résultat d'une opération dépend de l'ordre d'exécution des threads, qui est imprévisible.

## Sections critiques

Une **section critique** est une zone de code qui ne doit être exécutée que par un seul thread à la fois. C'est l'outil de synchronisation le plus simple et le plus efficace dans Delphi.

### Création et utilisation d'une section critique

```pascal
var
  MaSection: TCriticalSection;

begin
  // Création de la section critique
  MaSection := TCriticalSection.Create;
  try
    // Utilisation de la section critique
    // ...
  finally
    // Libération de la section critique
    MaSection.Free;
  end;
end;
```

### Protection d'un bloc de code

Pour protéger l'accès à une ressource partagée :

```pascal
MaSection.Enter;  // Verrouille la section critique
try
  // Code protégé - un seul thread à la fois peut exécuter ce bloc
  CompteurGlobal := CompteurGlobal + 1;
finally
  MaSection.Leave;  // Déverrouille la section critique
end;
```

Cette structure `try...finally` est cruciale car elle garantit que la section critique sera toujours déverrouillée, même si une exception se produit.

### Exemple complet avec une section critique

Voici un exemple qui corrige le problème de compteur partagé :

```pascal
var
  CompteurGlobal: Integer = 0;
  CompteurSection: TCriticalSection;

type
  TCompteurThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TCompteurThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 1000 do
  begin
    // Protection de l'accès à CompteurGlobal
    CompteurSection.Enter;
    try
      CompteurGlobal := CompteurGlobal + 1;
    finally
      CompteurSection.Leave;
    end;

    // Simuler d'autres traitements
    Sleep(1);
  end;
end;

// Dans le formulaire principal
procedure TForm1.FormCreate(Sender: TObject);
begin
  CompteurSection := TCriticalSection.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CompteurSection.Free;
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  CompteurGlobal := 0;

  // Création de deux threads qui incrémentent le compteur
  TCompteurThread.Create(False);
  TCompteurThread.Create(False);
end;

procedure TForm1.ButtonShowClick(Sender: TObject);
begin
  ShowMessage('Valeur du compteur : ' + IntToStr(CompteurGlobal));
end;
```

## Utilisation de TMonitor

Depuis Delphi 2009, la classe `TMonitor` offre une approche plus moderne et plus sûre pour la synchronisation :

```pascal
var
  CompteurGlobal: Integer = 0;

procedure IncrementCompteur;
begin
  TMonitor.Enter(CompteurGlobal);
  try
    CompteurGlobal := CompteurGlobal + 1;
  finally
    TMonitor.Exit(CompteurGlobal);
  end;
end;
```

L'avantage de `TMonitor` est qu'il peut verrouiller n'importe quel objet, pas seulement une section critique dédiée. Cela rend le code plus élégant dans certains cas.

## TryEnter : éviter les blocages

Les sections critiques offrent une méthode `TryEnter` qui tente de verrouiller la section mais n'attend pas si elle est déjà verrouillée :

```pascal
if MaSection.TryEnter then
begin
  try
    // Code protégé
  finally
    MaSection.Leave;
  end;
end
else
begin
  // La section est déjà verrouillée par un autre thread
  // Faire autre chose en attendant
end;
```

Cette technique est utile pour éviter les blocages lorsqu'un thread ne peut pas attendre indéfiniment.

## Protéger une classe ou un objet

Lorsque vous avez une classe dont les instances peuvent être utilisées par plusieurs threads, vous pouvez intégrer une section critique à la classe :

```pascal
type
  TCompteurThreadSafe = class
  private
    FValeur: Integer;
    FSection: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Incrementer;
    function Lire: Integer;
  end;

constructor TCompteurThreadSafe.Create;
begin
  inherited;
  FValeur := 0;
  FSection := TCriticalSection.Create;
end;

destructor TCompteurThreadSafe.Destroy;
begin
  FSection.Free;
  inherited;
end;

procedure TCompteurThreadSafe.Incrementer;
begin
  FSection.Enter;
  try
    FValeur := FValeur + 1;
  finally
    FSection.Leave;
  end;
end;

function TCompteurThreadSafe.Lire: Integer;
begin
  FSection.Enter;
  try
    Result := FValeur;
  finally
    FSection.Leave;
  end;
end;
```

Cette approche crée une classe "thread-safe" que vous pouvez utiliser sans vous soucier de la synchronisation.

## Autres mécanismes de synchronisation

### TMutex (Mutex)

Un Mutex (Mutual Exclusion) est similaire à une section critique, mais peut être utilisé pour synchroniser des threads entre différentes applications :

```pascal
var
  MonMutex: TMutex;

begin
  // Création d'un mutex nommé
  MonMutex := TMutex.Create(nil, False, 'MonApplication.Mutex');
  try
    // Attendre l'accès
    MonMutex.Acquire;
    try
      // Code protégé
    finally
      MonMutex.Release;
    end;
  finally
    MonMutex.Free;
  end;
end;
```

### TSemaphore (Sémaphore)

Un sémaphore permet à un nombre limité de threads d'accéder simultanément à une ressource :

```pascal
var
  MonSemaphore: TSemaphore;

begin
  // Création d'un sémaphore permettant 3 accès simultanés
  MonSemaphore := TSemaphore.Create(nil, 3, 3, 'MonSemaphore');
  try
    // Attendre un jeton
    MonSemaphore.Acquire;
    try
      // Code protégé (3 threads maximum peuvent être ici en même temps)
    finally
      // Libérer le jeton
      MonSemaphore.Release;
    end;
  finally
    MonSemaphore.Free;
  end;
end;
```

Les sémaphores sont utiles pour limiter l'accès à des ressources comme les connexions de base de données.

### TEvent (Événement)

Un événement permet à un thread de signaler à d'autres threads qu'un certain état a été atteint :

```pascal
var
  MonEvent: TEvent;

// Dans le thread 1
begin
  // Création d'un événement
  MonEvent := TEvent.Create(nil, True, False, 'MonEvent');
  try
    // Faire quelque chose...

    // Signaler que c'est prêt
    MonEvent.SetEvent;

    // ...
  finally
    MonEvent.Free;
  end;
end;

// Dans le thread 2
begin
  // Attendre que l'événement soit signalé
  MonEvent.WaitFor(INFINITE);

  // Continuer le traitement
end;
```

Les événements sont parfaits pour la synchronisation basée sur des conditions.

## Exemple pratique : producteur-consommateur

Un problème classique en programmation concurrente est le "producteur-consommateur", où un thread produit des données et un autre les consomme. Voici une implémentation simple :

```pascal
type
  TFileAttente = class
  private
    FItems: TList<string>;
    FSection: TCriticalSection;
    FEventNouveau: TEvent;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Ajouter(const Item: string);
    function Retirer(var Item: string): Boolean;
  end;

constructor TFileAttente.Create;
begin
  inherited;
  FItems := TList<string>.Create;
  FSection := TCriticalSection.Create;
  FEventNouveau := TEvent.Create(nil, True, False, '');
end;

destructor TFileAttente.Destroy;
begin
  FItems.Free;
  FSection.Free;
  FEventNouveau.Free;
  inherited;
end;

procedure TFileAttente.Ajouter(const Item: string);
begin
  FSection.Enter;
  try
    FItems.Add(Item);
    // Signaler qu'un nouvel élément est disponible
    FEventNouveau.SetEvent;
  finally
    FSection.Leave;
  end;
end;

function TFileAttente.Retirer(var Item: string): Boolean;
begin
  Result := False;
  FSection.Enter;
  try
    if FItems.Count > 0 then
    begin
      Item := FItems[0];
      FItems.Delete(0);
      Result := True;

      // Réinitialiser l'événement si la file est vide
      if FItems.Count = 0 then
        FEventNouveau.ResetEvent;
    end;
  finally
    FSection.Leave;
  end;
end;
```

Utilisation dans les threads producteur et consommateur :

```pascal
// Thread producteur
procedure TProducteurThread.Execute;
var
  i: Integer;
  Item: string;
begin
  for i := 1 to 100 do
  begin
    Item := 'Item ' + IntToStr(i);
    FilePartagee.Ajouter(Item);
    Sleep(Random(100));  // Simuler du travail
  end;
end;

// Thread consommateur
procedure TConsommateurThread.Execute;
var
  Item: string;
begin
  while not Terminated do
  begin
    if FilePartagee.Retirer(Item) then
    begin
      // Traiter l'item
      Synchronize(procedure
        begin
          Form1.Memo1.Lines.Add('Consommé: ' + Item);
        end);
    end
    else
    begin
      // Attendre qu'un nouvel élément soit disponible
      FilePartagee.FEventNouveau.WaitFor(100);
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Minimiser la taille des sections critiques

Plus une section critique est longue, plus elle risque de bloquer d'autres threads. Essayez de minimiser le code à l'intérieur :

```pascal
// Moins efficace
FSection.Enter;
try
  // Calcul complexe qui prend du temps
  // ...
  // Mise à jour de la variable partagée
  VariablePartagee := ResultatCalcul;
finally
  FSection.Leave;
end;

// Plus efficace
// Calcul complexe en dehors de la section critique
ResultatCalcul := CalculerValeur;

// Section critique minimale
FSection.Enter;
try
  VariablePartagee := ResultatCalcul;
finally
  FSection.Leave;
end;
```

### 2. Éviter les sections critiques imbriquées

Les sections critiques imbriquées peuvent facilement conduire à des blocages (deadlocks). Évitez-les si possible, ou utilisez-les avec précaution.

### 3. Utiliser des structures thread-safe

Plutôt que de créer vos propres mécanismes de synchronisation, utilisez des classes déjà thread-safe. Par exemple, Delphi propose `TThreadList<T>` qui encapsule une liste avec sa propre synchronisation :

```pascal
var
  ListePartagee: TThreadList<string>;

begin
  ListePartagee := TThreadList<string>.Create;
  try
    // Pour ajouter un élément
    ListePartagee.Add('Nouvel élément');

    // Pour accéder à la liste en lecture/écriture
    var MaListe := ListePartagee.LockList;
    try
      // Manipuler MaListe en toute sécurité
    finally
      ListePartagee.UnlockList;
    end;
  finally
    ListePartagee.Free;
  end;
end;
```

### 4. Toujours utiliser try...finally

N'oubliez jamais de déverrouiller une section critique, même en cas d'exception. La structure `try...finally` est indispensable.

## Exercice pratique

Créez une application avec :

1. Un compteur partagé entre plusieurs threads
2. Des boutons pour démarrer 1, 2 ou 5 threads qui incrémentent le compteur
3. Un bouton pour afficher la valeur actuelle du compteur
4. Une option pour activer/désactiver la protection par section critique

Cet exercice vous permettra de voir concrètement la différence entre un code protégé et non protégé dans un environnement multithread.

## Résumé

- La synchronisation est essentielle pour éviter les problèmes d'accès concurrent
- Les sections critiques sont le mécanisme de base pour protéger les ressources partagées
- D'autres mécanismes comme les mutex, les sémaphores et les événements offrent des fonctionnalités plus avancées
- Minimisez toujours la taille des sections critiques pour de meilleures performances
- Utilisez systématiquement `try...finally` pour garantir le déverrouillage
- Pensez à utiliser les classes thread-safe intégrées quand c'est possible

Dans le prochain chapitre, nous explorerons `TTask` et la programmation parallèle, qui offrent une approche plus moderne et plus simple pour le multithreading.

⏭️ [TTask et programmation parallèle](11-multithreading-et-programmation-asynchrone/04-ttask-et-programmation-parallele.md)
