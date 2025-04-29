# 11.2 Création et gestion de threads

## Introduction

Après avoir découvert les principes fondamentaux du multithreading, nous allons maintenant explorer la création et la gestion des threads dans Delphi. Ce chapitre vous donnera les connaissances pratiques pour implémenter vos propres threads et les gérer efficacement.

## La classe TThread

Dans Delphi, la création de threads se fait principalement à l'aide de la classe `TThread`, située dans l'unité `System.Classes`. Cette classe encapsule toute la complexité de la gestion des threads au niveau du système d'exploitation.

### Création d'un thread personnalisé

Pour créer votre propre thread, vous devez :

1. Créer une classe dérivée de `TThread`
2. Remplacer (override) la méthode `Execute`
3. Implémenter votre logique dans cette méthode

Voici la structure de base :

```pascal
type
  TMonThread = class(TThread)
  private
    // Variables et méthodes privées
  protected
    procedure Execute; override;
  public
    constructor Create;
    // Méthodes publiques
  end;
```

### Implémentation du constructeur

Le constructeur de `TThread` accepte un paramètre booléen qui détermine si le thread démarre immédiatement ou non :

```pascal
constructor TMonThread.Create;
begin
  // Initialisation des variables

  // False = démarrer immédiatement, True = thread suspendu
  inherited Create(False);
end;
```

### Implémentation de la méthode Execute

La méthode `Execute` est le cœur de votre thread. C'est ici que vous placerez le code qui s'exécutera dans le thread secondaire :

```pascal
procedure TMonThread.Execute;
begin
  // Vérifier périodiquement si le thread doit s'arrêter
  while not Terminated do
  begin
    // Code à exécuter dans le thread

    // Optionnel : pause courte pour libérer du temps CPU
    Sleep(10);
  end;
end;
```

## Exemple complet d'un thread simple

Voici un exemple de thread qui effectue un décompte :

```pascal
type
  TCountdownThread = class(TThread)
  private
    FCurrentValue: Integer;
    FStartValue: Integer;
    procedure UpdateDisplay;
  protected
    procedure Execute; override;
  public
    constructor Create(StartValue: Integer);
    property CurrentValue: Integer read FCurrentValue;
  end;

constructor TCountdownThread.Create(StartValue: Integer);
begin
  FStartValue := StartValue;
  FCurrentValue := StartValue;
  FreeOnTerminate := True; // Le thread libérera sa mémoire automatiquement
  inherited Create(False); // Démarrer immédiatement
end;

procedure TCountdownThread.UpdateDisplay;
begin
  // Cette méthode sera exécutée dans le thread principal
  Form1.LabelCounter.Caption := IntToStr(FCurrentValue);
  Form1.ProgressBar1.Position := FStartValue - FCurrentValue;
end;

procedure TCountdownThread.Execute;
begin
  while (not Terminated) and (FCurrentValue > 0) do
  begin
    Dec(FCurrentValue);

    // Synchroniser avec le thread principal pour mettre à jour l'interface
    Synchronize(UpdateDisplay);

    // Attendre un peu pour simuler une tâche qui prend du temps
    Sleep(1000); // 1 seconde
  end;

  // Mise à jour finale (si nécessaire)
  if not Terminated then
    Synchronize(UpdateDisplay);
end;
```

## Utilisation du thread dans un formulaire

Voici comment utiliser ce thread depuis les événements d'un formulaire :

```pascal
var
  CountdownThread: TCountdownThread;

procedure TForm1.ButtonStartClick(Sender: TObject);
begin
  // Désactive le bouton pendant l'exécution du thread
  ButtonStart.Enabled := False;
  ButtonStop.Enabled := True;

  // Crée et démarre le thread
  CountdownThread := TCountdownThread.Create(10);
end;

procedure TForm1.ButtonStopClick(Sender: TObject);
begin
  // Demande l'arrêt du thread
  if CountdownThread <> nil then
    CountdownThread.Terminate;

  ButtonStart.Enabled := True;
  ButtonStop.Enabled := False;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // Important : s'assurer que le thread est terminé avant de fermer l'application
  if CountdownThread <> nil then
  begin
    CountdownThread.Terminate;
    // Attendre que le thread se termine
    CountdownThread.WaitFor;
    CountdownThread.Free;
  end;
end;
```

## Options importantes pour la création de threads

### FreeOnTerminate

Cette propriété détermine si le thread doit libérer automatiquement sa mémoire après la fin de son exécution :

```pascal
MonThread.FreeOnTerminate := True; // Libération automatique
```

Quand utiliser chaque option :
- `True`: Utile pour les threads "fire and forget" (lancer et oublier)
- `False`: Quand vous avez besoin de contrôler le cycle de vie du thread ou de récupérer des résultats après son exécution

### Priorité des threads

Vous pouvez ajuster la priorité d'un thread pour influencer l'allocation du temps CPU :

```pascal
MonThread.Priority := tpNormal; // Priorité normale
```

Options disponibles :
- `tpIdle` : Très faible priorité
- `tpLowest` : Priorité la plus basse
- `tpLower` : Priorité basse
- `tpNormal` : Priorité normale (par défaut)
- `tpHigher` : Priorité haute
- `tpHighest` : Priorité la plus haute
- `tpTimeCritical` : Priorité critique (à utiliser avec précaution)

> ⚠️ **Attention** : Utiliser une priorité élevée peut affecter les performances globales du système. Utilisez `tpHighest` et `tpTimeCritical` avec parcimonie.

## Contrôle du cycle de vie d'un thread

### Démarrer un thread suspendu

Si vous créez un thread avec le paramètre `CreateSuspended` à `True`, vous devez explicitement le démarrer :

```pascal
var
  MonThread: TMonThread;
begin
  MonThread := TMonThread.Create(True); // Thread suspendu
  // Configuration du thread...
  MonThread.Start; // Démarrage manuel du thread
end;
```

### Terminer un thread

Pour demander l'arrêt d'un thread :

```pascal
MonThread.Terminate;
```

> ℹ️ **Important** : `Terminate` ne force pas l'arrêt du thread. Il définit simplement la propriété `Terminated` à `True`. Votre thread doit vérifier régulièrement cette propriété et s'arrêter proprement.

### Attendre la fin d'un thread

Pour attendre qu'un thread se termine :

```pascal
MonThread.WaitFor;
```

> ⚠️ **Attention** : N'appelez jamais `WaitFor` depuis le thread principal dans une application avec interface utilisateur, car cela bloquerait l'interface. Utilisez-le uniquement lors de la fermeture de l'application ou dans d'autres threads.

## Passage de paramètres à un thread

Pour passer des paramètres à un thread, ajoutez des propriétés à votre classe et initialisez-les dans le constructeur :

```pascal
type
  TProcessingThread = class(TThread)
  private
    FInputFile: string;
    FOutputFile: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const InputFile, OutputFile: string);
  end;

constructor TProcessingThread.Create(const InputFile, OutputFile: string);
begin
  FInputFile := InputFile;
  FOutputFile := OutputFile;
  inherited Create(False);
end;
```

## Récupération des résultats d'un thread

Pour récupérer des résultats calculés par un thread :

1. Ajoutez des propriétés pour stocker les résultats
2. Utilisez un événement de notification quand le traitement est terminé

```pascal
type
  TCalculationThread = class(TThread)
  private
    FInput: Integer;
    FResult: Integer;
    FOnCalculationComplete: TNotifyEvent;
    procedure DoCalculationComplete;
  protected
    procedure Execute; override;
  public
    constructor Create(Input: Integer);
    property Result: Integer read FResult;
    property OnCalculationComplete: TNotifyEvent write FOnCalculationComplete;
  end;

constructor TCalculationThread.Create(Input: Integer);
begin
  FInput := Input;
  inherited Create(False);
end;

procedure TCalculationThread.Execute;
begin
  // Simuler un calcul complexe
  Sleep(3000);
  FResult := FInput * FInput; // Exemple: calcul du carré

  // Notifier que le calcul est terminé
  if Assigned(FOnCalculationComplete) then
    Synchronize(DoCalculationComplete);
end;

procedure TCalculationThread.DoCalculationComplete;
begin
  if Assigned(FOnCalculationComplete) then
    FOnCalculationComplete(Self);
end;
```

Utilisation du thread avec récupération du résultat :

```pascal
procedure TForm1.ButtonCalculateClick(Sender: TObject);
var
  CalcThread: TCalculationThread;
begin
  CalcThread := TCalculationThread.Create(StrToIntDef(EditInput.Text, 0));
  CalcThread.OnCalculationComplete := CalculationCompleted;
end;

procedure TForm1.CalculationCompleted(Sender: TObject);
begin
  // Récupération et affichage du résultat
  if Sender is TCalculationThread then
    LabelResult.Caption := 'Résultat : ' + IntToStr(TCalculationThread(Sender).Result);
end;
```

## Meilleures pratiques

1. **Toujours vérifier `Terminated`** régulièrement dans la méthode `Execute` pour permettre un arrêt propre du thread.

2. **Protéger les ressources partagées** avec des mécanismes de synchronisation (nous verrons cela en détail dans la prochaine section).

3. **Ne jamais accéder directement aux composants de l'interface** depuis un thread secondaire, utilisez toujours `Synchronize` ou `Queue`.

4. **Libérer correctement les threads** avant de fermer l'application pour éviter les fuites de mémoire.

5. **Éviter de créer trop de threads** : le nombre optimal de threads dépend du nombre de cœurs de processeur disponibles.

## Exemple pratique : téléchargement de fichier

Voici un exemple plus complet qui télécharge un fichier en arrière-plan :

```pascal
type
  TDownloadThread = class(TThread)
  private
    FURL: string;
    FDestination: string;
    FProgress: Integer;
    FTotalSize: Int64;
    FDownloadedSize: Int64;
    procedure UpdateProgress;
  protected
    procedure Execute; override;
  public
    constructor Create(const URL, Destination: string);
    property Progress: Integer read FProgress;
  end;

constructor TDownloadThread.Create(const URL, Destination: string);
begin
  FURL := URL;
  FDestination := Destination;
  FProgress := 0;
  FreeOnTerminate := True;
  inherited Create(False);
end;

procedure TDownloadThread.UpdateProgress;
begin
  Form1.ProgressBar1.Position := FProgress;
  Form1.LabelStatus.Caption := Format('Téléchargement: %d%%', [FProgress]);
end;

procedure TDownloadThread.Execute;
var
  HTTPClient: TNetHTTPClient;
  HTTPRequest: TNetHTTPRequest;
  ResponseStream: TFileStream;
begin
  HTTPClient := TNetHTTPClient.Create(nil);
  HTTPRequest := TNetHTTPRequest.Create(nil);
  try
    HTTPRequest.Client := HTTPClient;

    // Événement pour suivre la progression
    HTTPClient.OnReceiveData := procedure(const Sender: TObject; AContentLength, AReadCount: Int64; var AAbort: Boolean)
    begin
      FTotalSize := AContentLength;
      FDownloadedSize := AReadCount;
      if FTotalSize > 0 then
        FProgress := Round((FDownloadedSize / FTotalSize) * 100);

      Synchronize(UpdateProgress);
      AAbort := Terminated;
    end;

    // Création du fichier de destination
    ResponseStream := TFileStream.Create(FDestination, fmCreate);
    try
      // Téléchargement du fichier
      HTTPRequest.Get(FURL, ResponseStream);
    finally
      ResponseStream.Free;
    end;
  finally
    HTTPRequest.Free;
    HTTPClient.Free;
  end;
end;
```

## Résumé

- La classe `TThread` est la base pour créer des threads dans Delphi
- Remplacez la méthode `Execute` pour définir le comportement du thread
- Utilisez `Synchronize` ou `Queue` pour interagir avec l'interface utilisateur
- Contrôlez le cycle de vie des threads avec `Create`, `Start`, `Terminate` et `WaitFor`
- Passez des paramètres via le constructeur et récupérez les résultats via des propriétés

Dans la prochaine section, nous approfondirons les techniques de synchronisation entre threads pour éviter les problèmes liés à l'accès concurrent aux données.

## Exercice pratique

Pour vous entraîner, essayez de créer une application simple avec :
1. Un bouton pour démarrer un thread qui compte de 1 à 100
2. Un bouton pour arrêter le thread
3. Une barre de progression qui se met à jour pendant le comptage
4. Un label qui affiche la valeur actuelle

Ce petit exercice vous permettra de mettre en pratique les concepts abordés dans ce chapitre.
