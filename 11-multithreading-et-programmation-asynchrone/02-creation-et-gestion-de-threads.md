🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 11.2 Création et gestion de threads

## La classe TThread

En Delphi, la création de threads repose principalement sur la classe **TThread**. C'est une classe abstraite qui encapsule toute la complexité de la gestion des threads au niveau du système d'exploitation.

Pour créer votre propre thread, vous devez :
1. Créer une classe qui hérite de `TThread`
2. Redéfinir la méthode `Execute`
3. Instancier et démarrer votre thread

## Créer votre premier thread

### Structure de base

Voici la structure minimale d'un thread personnalisé :

```pascal
type
  TMonThread = class(TThread)
  protected
    procedure Execute; override;
  end;

implementation

procedure TMonThread.Execute;  
begin  
  // Le code qui s'exécutera dans le thread
  // Cette méthode est appelée automatiquement au démarrage du thread
end;
```

### Exemple simple et commenté

Créons un thread qui effectue un comptage simple :

```pascal
type
  TThreadCompteur = class(TThread)
  private
    FCompteur: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

implementation

constructor TThreadCompteur.Create;  
begin  
  // Appeler le constructeur parent
  // False = le thread démarre immédiatement
  // True = le thread est créé en pause
  inherited Create(False);

  // Initialisation
  FCompteur := 0;

  // Le thread se libérera automatiquement à la fin
  FreeOnTerminate := True;
end;

procedure TThreadCompteur.Execute;  
var  
  i: Integer;
begin
  // Ce code s'exécute dans un thread séparé
  for i := 1 to 100 do
  begin
    // Vérifier si on demande l'arrêt du thread
    if Terminated then
      Exit;

    Inc(FCompteur);

    // Petite pause pour simuler un traitement
    Sleep(100); // 100 millisecondes
  end;
end;
```

### Démarrer le thread

Pour utiliser ce thread dans votre application :

```pascal
procedure TForm1.Button1Click(Sender: TObject);  
var  
  MonThread: TThreadCompteur;
begin
  // Créer et démarrer le thread
  MonThread := TThreadCompteur.Create;
  // Le thread démarre automatiquement car on a passé False au constructeur
end;
```

## Le cycle de vie d'un thread

### 1. Création

Deux façons de créer un thread :

```pascal
// Démarrage immédiat
MonThread := TMonThread.Create(False);

// Création en pause (il faudra appeler Start)
MonThread := TMonThread.Create(True);  
MonThread.Start; // Démarrage manuel  
```

### 2. Exécution

La méthode `Execute` contient le code qui s'exécutera dans le thread. Elle est appelée automatiquement lorsque le thread démarre.

```pascal
procedure TMonThread.Execute;  
begin  
  // Votre code ici
  // Cette méthode s'exécute dans un thread séparé

  // Boucle typique avec vérification d'arrêt
  while not Terminated do
  begin
    // Faire quelque chose
    // ...

    Sleep(100); // Pause pour ne pas saturer le CPU
  end;
end;
```

### 3. Arrêt

Pour arrêter proprement un thread :

```pascal
procedure TForm1.Button2Click(Sender: TObject);  
begin  
  if Assigned(MonThread) then
  begin
    // Demander l'arrêt du thread
    MonThread.Terminate;

    // Attendre que le thread se termine (optionnel)
    // ATTENTION : peut bloquer l'interface !
    // MonThread.WaitFor;
  end;
end;
```

### 4. Destruction

Deux approches pour libérer la mémoire :

**Approche 1 : Libération automatique**
```pascal
constructor TMonThread.Create;  
begin  
  inherited Create(False);
  FreeOnTerminate := True; // Le thread se libère tout seul
end;
```

**Approche 2 : Libération manuelle**
```pascal
constructor TMonThread.Create;  
begin  
  inherited Create(False);
  FreeOnTerminate := False; // On gère manuellement
end;

// Plus tard, dans votre code
MonThread.Terminate;  
MonThread.WaitFor; // Attendre la fin  
MonThread.Free;    // Libérer manuellement  
```

## Communiquer avec l'interface utilisateur

**RAPPEL IMPORTANT** : Un thread ne peut PAS modifier directement l'interface utilisateur !

### La méthode Synchronize

`Synchronize` permet d'exécuter du code dans le thread principal de manière sécurisée :

```pascal
type
  TThreadTelecharge = class(TThread)
  private
    FProgression: Integer;
    FMessage: string;
    procedure MettreAJourInterface;
  protected
    procedure Execute; override;
  end;

procedure TThreadTelecharge.MettreAJourInterface;  
begin  
  // Ce code s'exécute dans le thread principal
  // On peut modifier l'interface en toute sécurité
  Form1.ProgressBar1.Position := FProgression;
  Form1.Label1.Caption := FMessage;
end;

procedure TThreadTelecharge.Execute;  
var  
  i: Integer;
begin
  for i := 1 to 100 do
  begin
    if Terminated then Exit;

    // Simulation de travail
    Sleep(50);

    // Préparer les données à afficher
    FProgression := i;
    FMessage := Format('Progression : %d%%', [i]);

    // Demander au thread principal de mettre à jour l'interface
    Synchronize(MettreAJourInterface);
  end;

  FMessage := 'Terminé !';
  Synchronize(MettreAJourInterface);
end;
```

### La méthode Queue

Similaire à `Synchronize`, mais asynchrone :

```pascal
procedure TMonThread.Execute;  
begin  
  // Queue ne bloque pas le thread
  // La mise à jour se fera "quand le thread principal aura le temps"
  Queue(MettreAJourInterface);

  // Le code continue immédiatement
  // ...
end;
```

**Différence entre Synchronize et Queue** :
- **Synchronize** : Bloque le thread secondaire jusqu'à ce que l'interface soit mise à jour
- **Queue** : Le thread secondaire continue sans attendre

## Propriétés importantes de TThread

### Terminated

Indique si on a demandé l'arrêt du thread :

```pascal
procedure TMonThread.Execute;  
begin  
  while not Terminated do
  begin
    // Traitement
    // Vérifier régulièrement Terminated pour pouvoir s'arrêter proprement
  end;
end;
```

### FreeOnTerminate

Détermine si le thread se libère automatiquement :

```pascal
constructor TMonThread.Create;  
begin  
  inherited Create(False);
  FreeOnTerminate := True; // Libération automatique
end;
```

**Attention** : Si `FreeOnTerminate = True`, ne gardez PAS de référence au thread et ne tentez pas de le libérer manuellement !

### Priority (Priorité)

Définit la priorité du thread :

```pascal
constructor TMonThread.Create;  
begin  
  inherited Create(False);
  Priority := tpNormal; // Priorité par défaut
end;
```

Valeurs possibles :
- `tpIdle` : Priorité très basse
- `tpLowest` : Priorité basse
- `tpLower` : Légèrement en dessous de normal
- `tpNormal` : Priorité normale (par défaut)
- `tpHigher` : Légèrement au-dessus de normal
- `tpHighest` : Priorité haute
- `tpTimeCritical` : Priorité critique (à utiliser avec précaution !)

## Passer des paramètres à un thread

### Méthode 1 : Via le constructeur

```pascal
type
  TThreadCalcul = class(TThread)
  private
    FValeur1: Integer;
    FValeur2: Integer;
    FResultat: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AValeur1, AValeur2: Integer);
    property Resultat: Integer read FResultat;
  end;

constructor TThreadCalcul.Create(AValeur1, AValeur2: Integer);  
begin  
  inherited Create(True); // Créer en pause
  FValeur1 := AValeur1;
  FValeur2 := AValeur2;
  FreeOnTerminate := False; // On veut récupérer le résultat
end;

procedure TThreadCalcul.Execute;  
begin  
  // Simulation d'un calcul long
  Sleep(2000);
  FResultat := FValeur1 + FValeur2;
end;

// Utilisation
procedure TForm1.ButtonClick(Sender: TObject);  
var  
  ThreadCalcul: TThreadCalcul;
begin
  ThreadCalcul := TThreadCalcul.Create(10, 20);
  ThreadCalcul.Start;
  ThreadCalcul.WaitFor; // Attendre la fin
  ShowMessage('Résultat : ' + IntToStr(ThreadCalcul.Resultat));
  ThreadCalcul.Free;
end;
```

### Méthode 2 : Via des propriétés

```pascal
type
  TThreadTraitement = class(TThread)
  private
    FFichier: string;
  protected
    procedure Execute; override;
  public
    property Fichier: string read FFichier write FFichier;
  end;

// Utilisation
MonThread := TThreadTraitement.Create(True);  
MonThread.Fichier := 'C:\data.txt';  
MonThread.Start;  
```

## Gestion des erreurs dans les threads

Les exceptions dans un thread doivent être gérées avec soin :

```pascal
procedure TMonThread.Execute;  
begin  
  try
    // Code qui peut lever une exception
    // ...
  except
    on E: Exception do
    begin
      // Enregistrer l'erreur dans un log
      // Ou notifier l'utilisateur via Synchronize
    end;
  end;
end;
```

## Bonnes pratiques

### 1. Toujours vérifier Terminated

```pascal
procedure TMonThread.Execute;  
begin  
  while not Terminated do
  begin
    // Traitement
  end;
end;
```

### 2. Ne pas bloquer indéfiniment

Évitez les boucles infinies sans condition d'arrêt :

```pascal
// ❌ MAUVAIS
while True do  
begin  
  // ...
end;

// ✅ BON
while not Terminated do  
begin  
  // ...
end;
```

### 3. Gérer proprement la mémoire

Si `FreeOnTerminate = False`, n'oubliez pas de libérer le thread :

```pascal
try
  MonThread := TMonThread.Create(False);
  MonThread.WaitFor;
finally
  MonThread.Free;
end;
```

### 4. Éviter les accès concurrents

Ne modifiez pas les mêmes données depuis plusieurs threads sans protection (nous verrons la synchronisation dans la section suivante).

### 5. Limiter le nombre de threads

Créer trop de threads peut ralentir votre application. Le système d'exploitation doit gérer chaque thread, ce qui a un coût.

## Exemple complet : Téléchargement simulé

```pascal
type
  TThreadTelechargement = class(TThread)
  private
    FNomFichier: string;
    FProgression: Integer;
    procedure AfficherProgression;
  protected
    procedure Execute; override;
  public
    constructor Create(const ANomFichier: string);
  end;

constructor TThreadTelechargement.Create(const ANomFichier: string);  
begin  
  inherited Create(False);
  FNomFichier := ANomFichier;
  FreeOnTerminate := True;
end;

procedure TThreadTelechargement.AfficherProgression;  
begin  
  Form1.ProgressBar1.Position := FProgression;
  Form1.Label1.Caption := Format('Téléchargement de %s : %d%%',
    [FNomFichier, FProgression]);
end;

procedure TThreadTelechargement.Execute;  
var  
  i: Integer;
begin
  for i := 0 to 100 do
  begin
    if Terminated then Exit;

    // Simulation du téléchargement
    Sleep(50);

    FProgression := i;
    Synchronize(AfficherProgression);
  end;

  // Notification de fin
  FProgression := 100;
  Synchronize(
    procedure
    begin
      Form1.Label1.Caption := 'Téléchargement terminé !';
      ShowMessage('Le fichier ' + FNomFichier + ' a été téléchargé.');
    end
  );
end;
```

## Points clés à retenir

- Toujours hériter de `TThread` pour créer un thread personnalisé
- Redéfinir la méthode `Execute` qui contient le code du thread
- Utiliser `Synchronize` ou `Queue` pour mettre à jour l'interface utilisateur
- Vérifier régulièrement `Terminated` pour permettre l'arrêt propre du thread
- Choisir entre `FreeOnTerminate = True` (automatique) ou False (manuel)
- Gérer les exceptions dans la méthode `Execute`
- Ne jamais modifier l'interface directement depuis un thread secondaire

Dans la prochaine section, nous verrons comment synchroniser l'accès aux ressources partagées entre plusieurs threads.

⏭️ [Synchronisation et sections critiques](/11-multithreading-et-programmation-asynchrone/03-synchronisation-et-sections-critiques.md)
