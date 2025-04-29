# 11.1 Principes du multithreading

## Introduction

Le multithreading est une technique de programmation qui permet à un programme d'exécuter plusieurs tâches simultanément. Traditionnellement, un programme s'exécute de manière séquentielle, une instruction après l'autre, dans ce qu'on appelle un "thread" (fil d'exécution) unique. Avec le multithreading, votre application peut utiliser plusieurs threads qui s'exécutent en parallèle.

## Pourquoi utiliser le multithreading ?

### 1. Amélioration des performances
Sur les ordinateurs modernes avec plusieurs cœurs de processeur, le multithreading permet d'exploiter pleinement cette puissance en répartissant le travail.

### 2. Interface utilisateur réactive
L'un des avantages les plus importants est de maintenir une interface utilisateur fluide et réactive pendant l'exécution d'opérations longues comme :
- Téléchargement de fichiers
- Traitement de données volumineuses
- Requêtes de base de données complexes
- Calculs intensifs

### 3. Opérations parallèles
Certaines tâches peuvent naturellement être divisées en sous-tâches indépendantes qui peuvent s'exécuter en parallèle.

## Concepts fondamentaux

### Thread principal vs threads secondaires

Dans une application Delphi, il existe par défaut un thread principal :
- Il gère l'interface utilisateur
- Il traite les messages Windows
- Il gère les événements des composants visuels

**Règle d'or** : Ne jamais bloquer le thread principal avec des opérations longues, car cela fige l'interface utilisateur.

### Thread secondaire (worker thread)

Un thread secondaire permet d'exécuter du code en parallèle du thread principal :
- Il peut effectuer des opérations longues sans bloquer l'interface
- Il doit suivre certaines règles pour interagir avec l'interface utilisateur

## Défis du multithreading

### 1. Accès concurrent aux données
Lorsque plusieurs threads accèdent aux mêmes données :
- Risque de corruption des données
- Résultats imprévisibles
- Plantages de l'application

### 2. Synchronisation
La synchronisation entre threads est nécessaire pour :
- Protéger l'accès aux ressources partagées
- Coordonner l'exécution des threads
- Garantir l'intégrité des données

### 3. Course critique (Race condition)
Situation où le résultat dépend de l'ordre d'exécution des threads, ce qui peut varier d'une exécution à l'autre.

### 4. Interblocage (Deadlock)
Situation où deux threads ou plus sont bloqués indéfiniment, chacun attendant une ressource détenue par un autre.

## Règles fondamentales à respecter

### 1. Règle d'accès à l'interface utilisateur
Seul le thread principal peut manipuler les composants visuels. Pour mettre à jour l'interface à partir d'un thread secondaire, vous devez :
- Utiliser `TThread.Synchronize` ou `TThread.Queue`
- Ne jamais accéder directement aux composants visuels depuis un thread secondaire

```pascal
// Exemple d'utilisation de Synchronize
procedure TMonThread.MiseAJourUI;
begin
  // Ce code s'exécutera dans le thread principal
  Form1.Label1.Caption := 'Traitement terminé';
end;

procedure TMonThread.Execute;
begin
  // Effectuer des opérations longues...

  // Mettre à jour l'interface en toute sécurité
  Synchronize(MiseAJourUI);
end;
```

### 2. Protection des données partagées
Toujours protéger l'accès aux données partagées entre threads avec des mécanismes de synchronisation comme :
- Sections critiques (`TCriticalSection`)
- Verrous (`TMutex`)
- Sémaphores (`TSemaphore`)

### 3. Éviter les variables globales
Les variables globales sont particulièrement dangereuses dans un environnement multithread. Préférez :
- Encapsuler les données dans les classes de thread
- Utiliser des techniques de passage de messages
- Concevoir des structures thread-safe

## Application simple du multithreading

Voici un exemple simple illustrant l'utilisation d'un thread pour réaliser un comptage sans bloquer l'interface utilisateur :

```pascal
type
  TCompteurThread = class(TThread)
  private
    FValeurActuelle: Integer;
    FValeurMax: Integer;
    procedure AfficherProgression;
  protected
    procedure Execute; override;
  public
    constructor Create(ValeurMax: Integer);
  end;

constructor TCompteurThread.Create(ValeurMax: Integer);
begin
  FValeurMax := ValeurMax;
  FValeurActuelle := 0;
  FreeOnTerminate := True; // Le thread se libère automatiquement
  inherited Create(False); // False = démarrage immédiat
end;

procedure TCompteurThread.AfficherProgression;
begin
  Form1.ProgressBar1.Position := FValeurActuelle;
  Form1.Label1.Caption := Format('Progression: %d / %d', [FValeurActuelle, FValeurMax]);
end;

procedure TCompteurThread.Execute;
begin
  while (not Terminated) and (FValeurActuelle < FValeurMax) do
  begin
    Inc(FValeurActuelle);
    Synchronize(AfficherProgression);
    Sleep(50); // Simuler un traitement qui prend du temps
  end;
end;
```

Pour utiliser ce thread depuis un bouton de formulaire :

```pascal
procedure TForm1.BtnDemarrerClick(Sender: TObject);
begin
  // Créer et démarrer le thread
  TCompteurThread.Create(100);
  // Le thread se libérera automatiquement grâce à FreeOnTerminate := True
end;
```

## Résumé

- Le multithreading permet d'exécuter plusieurs tâches en parallèle
- Il améliore la réactivité de l'interface utilisateur
- Les défis principaux sont la synchronisation et la protection des données
- Seul le thread principal doit manipuler l'interface utilisateur
- Les mécanismes de synchronisation sont essentiels pour éviter les problèmes

Dans les sections suivantes, nous explorerons plus en détail la création et la gestion des threads, les techniques de synchronisation et les modèles de programmation asynchrone plus avancés disponibles dans Delphi.
