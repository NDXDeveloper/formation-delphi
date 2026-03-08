🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 12.5 Gestion des exceptions et journalisation

## Introduction

Même les meilleurs développeurs écrivent du code qui peut rencontrer des erreurs. Un fichier peut ne pas exister, une connexion réseau peut échouer, un utilisateur peut entrer des données invalides, ou une division par zéro peut se produire. La façon dont votre application gère ces situations d'erreur fait la différence entre un logiciel professionnel et un programme qui plante mystérieusement.

Dans ce chapitre, nous allons explorer deux concepts intimement liés : **les exceptions** (le mécanisme de Delphi pour gérer les erreurs) et **la journalisation** (l'enregistrement de ce qui se passe dans votre application).

Pour un débutant, comprendre ces concepts est essentiel pour créer des applications robustes et fiables qui gèrent élégamment les situations inattendues.

## Qu'est-ce qu'une exception ?

### Définition simple

Une **exception** est un événement anormal qui se produit pendant l'exécution d'un programme et qui interrompt le flux normal des instructions. C'est la façon dont Delphi signale qu'une erreur s'est produite.

### Analogie pour comprendre

Imaginez que vous conduisez sur une route (l'exécution normale de votre programme). Soudain, vous rencontrez un obstacle imprévu (une erreur). Vous avez deux options :

1. **Sans gestion d'exception** : Vous foncez dans l'obstacle et vous crashez (le programme plante)
2. **Avec gestion d'exception** : Vous détectez l'obstacle à temps, vous le contournez ou vous vous arrêtez en toute sécurité, et vous pouvez continuer votre route ou prendre un itinéraire alternatif

Les exceptions permettent à votre programme de "détecter l'obstacle" et de réagir de manière appropriée.

### Que se passe-t-il quand une exception se produit ?

Lorsqu'une erreur survient, Delphi :

1. **Crée un objet exception** contenant des informations sur l'erreur
2. **Interrompt l'exécution normale** du code
3. **Remonte la pile d'appels** à la recherche d'un gestionnaire d'exception
4. Si un gestionnaire est trouvé, il **traite l'erreur**
5. Sinon, l'application affiche un message d'erreur et **peut se terminer**

## Les exceptions en Delphi

### Types d'exceptions courants

Delphi fournit de nombreuses classes d'exceptions pour différents types d'erreurs. Voici les plus courantes :

**EDivByZero** : Division par zéro

```pascal
var
  Resultat: Integer;
begin
  Resultat := 10 div 0;  // Lève EDivByZero
end;
```

**EConvertError** : Erreur de conversion de type

```pascal
var
  Nombre: Integer;
begin
  Nombre := StrToInt('abc');  // Lève EConvertError
end;
```

**EAccessViolation** : Accès mémoire invalide (tentative d'utiliser un objet nil)

```pascal
var
  MonObjet: TMonObjet;
begin
  MonObjet := nil;
  MonObjet.Afficher;  // Lève EAccessViolation
end;
```

**EInOutError** : Erreur d'entrée/sortie fichier

```pascal
begin
  AssignFile(F, 'fichier_inexistant.txt');
  Reset(F);  // Lève EInOutError si le fichier n'existe pas
end;
```

**EFilerError** : Erreur lors de la lecture/écriture de fichiers

**EOutOfMemory** : Mémoire insuffisante

**Exception** : Classe de base de toutes les exceptions

### Hiérarchie des exceptions

Toutes les exceptions héritent de la classe `Exception`. Cette hiérarchie vous permet de capturer des types d'erreurs spécifiques ou généraux :

```
Exception
├── EAbort
├── EAccessViolation
├── EConvertError
├── EDivByZero
├── EInOutError
├── EOutOfMemory
├── EDatabaseError
│   ├── EDBEngineError
│   └── EFDException
└── ... beaucoup d'autres
```

## Gérer les exceptions avec try...except

### Structure de base

La structure `try...except` permet de capturer et de gérer les exceptions :

```pascal
procedure ExempleGestionException;  
var  
  Resultat: Integer;
begin
  try
    // Code qui peut lever une exception
    Resultat := 10 div 0;
  except
    // Code qui gère l'exception
    ShowMessage('Une erreur s''est produite !');
  end;

  // Le programme continue normalement ici
  ShowMessage('Programme toujours en cours d''exécution');
end;
```

**Comment ça fonctionne :**

1. Le code dans le bloc `try` s'exécute normalement
2. Si une exception se produit, l'exécution saute immédiatement au bloc `except`
3. Le code du bloc `except` est exécuté
4. Après le bloc `except`, le programme continue normalement

### Capturer des exceptions spécifiques

Vous pouvez gérer différents types d'exceptions différemment :

```pascal
procedure DivisionSecurisee(A, B: Integer);  
begin  
  try
    ShowMessage(Format('Résultat : %d', [A div B]));
  except
    on E: EDivByZero do
      ShowMessage('Erreur : Division par zéro impossible');

    on E: EOverflow do
      ShowMessage('Erreur : Le résultat est trop grand');

    on E: Exception do
      ShowMessage('Erreur inattendue : ' + E.Message);
  end;
end;
```

**Points importants :**

- Listez les exceptions **du plus spécifique au plus général**
- `E` est une variable locale contenant l'objet exception
- `E.Message` contient le message d'erreur
- La dernière clause `on E: Exception` capture toutes les autres exceptions

### Accéder aux informations de l'exception

L'objet exception contient des informations utiles :

```pascal
try
  // Code à risque
  TraiterFichier('document.txt');
except
  on E: Exception do
  begin
    ShowMessage('Type d''exception : ' + E.ClassName);
    ShowMessage('Message : ' + E.Message);

    // Pour un débogage plus détaillé
    ShowMessage('Stack Trace : ' + E.StackTrace);
  end;
end;
```

**Propriétés utiles :**
- `ClassName` : Le nom de la classe d'exception
- `Message` : Description de l'erreur
- `StackTrace` : Trace de la pile d'appels (disponible avec debug info)

### Relancer une exception

Parfois, vous voulez gérer partiellement une exception puis la relancer pour qu'elle soit traitée à un niveau supérieur :

```pascal
procedure ChargerConfiguration;  
begin  
  try
    LireFichierConfig('config.ini');
  except
    on E: Exception do
    begin
      // Journaliser l'erreur
      EcrireLog('Erreur lors du chargement de la config : ' + E.Message);

      // Relancer l'exception pour que l'appelant la gère aussi
      raise;
    end;
  end;
end;
```

Le mot-clé `raise` sans paramètre relance l'exception actuelle.

### Lever vos propres exceptions

Vous pouvez créer et lever vos propres exceptions :

```pascal
procedure DefinirAge(Age: Integer);  
begin  
  if Age < 0 then
    raise Exception.Create('L''âge ne peut pas être négatif');

  if Age > 150 then
    raise Exception.Create('L''âge ne peut pas dépasser 150 ans');

  FAge := Age;
end;
```

Pour des cas plus spécifiques, créez vos propres classes d'exception :

```pascal
type
  EAgeInvalide = class(Exception);

procedure DefinirAge(Age: Integer);  
begin  
  if (Age < 0) or (Age > 150) then
    raise EAgeInvalide.Create('Âge invalide : ' + IntToStr(Age));

  FAge := Age;
end;
```

## La structure try...finally

### Pourquoi finally ?

Le bloc `finally` garantit qu'un code sera exécuté **quelle que soit** l'issue du bloc `try`, même si une exception se produit. C'est crucial pour libérer les ressources.

### Structure de base

```pascal
procedure TraiterFichier;  
var  
  Fichier: TStringList;
begin
  Fichier := TStringList.Create;
  try
    // Utiliser le fichier
    Fichier.LoadFromFile('donnees.txt');
    TraiterDonnees(Fichier);
  finally
    // TOUJOURS exécuté, même si une exception se produit
    Fichier.Free;
  end;
end;
```

**Règle d'or** : Tout objet que vous créez doit être libéré dans un bloc `finally`.

### Pourquoi c'est important ?

Sans `finally`, si une exception se produit, l'objet ne sera jamais libéré :

```pascal
// MAUVAIS CODE - Fuite mémoire possible
procedure MauvaiseGestion;  
var  
  Fichier: TStringList;
begin
  Fichier := TStringList.Create;
  Fichier.LoadFromFile('donnees.txt');  // Si erreur ici...
  TraiterDonnees(Fichier);
  Fichier.Free;  // ...cette ligne ne sera jamais exécutée
end;

// BON CODE - Pas de fuite mémoire
procedure BonneGestion;  
var  
  Fichier: TStringList;
begin
  Fichier := TStringList.Create;
  try
    Fichier.LoadFromFile('donnees.txt');
    TraiterDonnees(Fichier);
  finally
    Fichier.Free;  // TOUJOURS exécuté
  end;
end;
```

### Combiner try...finally et try...except

Pour une gestion complète, vous pouvez imbriquer les deux structures :

```pascal
procedure GestionComplete;  
var  
  Fichier: TStringList;
begin
  Fichier := TStringList.Create;
  try
    try
      Fichier.LoadFromFile('donnees.txt');
      TraiterDonnees(Fichier);
    except
      on E: Exception do
        ShowMessage('Erreur : ' + E.Message);
    end;
  finally
    Fichier.Free;
  end;
end;
```

Ou plus simplement avec une seule structure :

```pascal
procedure GestionSimplifiee;  
var  
  Fichier: TStringList;
begin
  Fichier := TStringList.Create;
  try
    Fichier.LoadFromFile('donnees.txt');
    TraiterDonnees(Fichier);
  finally
    Fichier.Free;
  end;
  // Les exceptions non gérées remonteront automatiquement
end;
```

**Note** : Si vous ne gérez pas explicitement l'exception avec `except`, elle remontera à l'appelant, mais le bloc `finally` sera quand même exécuté.

## Bonnes pratiques de gestion des exceptions

### 1. Ne capturez que ce que vous pouvez gérer

Ne capturez pas toutes les exceptions "au cas où". Capturez seulement celles que vous pouvez réellement gérer :

```pascal
// MAUVAIS - Trop général
try
  TraiterDonnees;
except
  // Que faire ici ? On ne sait même pas ce qui s'est passé
end;

// BON - Spécifique et géré
try
  Age := StrToInt(Edit1.Text);
except
  on E: EConvertError do
  begin
    ShowMessage('Veuillez entrer un nombre valide');
    Edit1.SetFocus;
  end;
end;
```

### 2. Libérez toujours vos ressources

Utilisez `try...finally` pour tous les objets que vous créez :

```pascal
// Pattern standard pour tout objet
MonObjet := TMonObjet.Create;  
try  
  // Utiliser MonObjet
finally
  MonObjet.Free;
end;
```

**Astuce** : Vous pouvez utiliser `FreeAndNil(MonObjet)` dans le `finally` pour libérer l'objet et mettre la variable à `nil` en une seule opération.

### 3. Utilisez des messages d'erreur clairs

Aidez l'utilisateur (et vous-même) à comprendre ce qui s'est passé :

```pascal
// MAUVAIS
raise Exception.Create('Erreur');

// BON
raise Exception.CreateFmt('Impossible de charger le fichier "%s" : fichier non trouvé',
                         [NomFichier]);
```

### 4. Ne cachez pas les exceptions

Ne capturez pas une exception sans rien faire :

```pascal
// TRÈS MAUVAIS - "Avalage" d'exception
try
  OperationCritique;
except
  // Ne rien faire - L'erreur est ignorée silencieusement
end;

// BON - Au minimum, journalisez
try
  OperationCritique;
except
  on E: Exception do
  begin
    EcrireLog('Erreur dans OperationCritique : ' + E.Message);
    raise;  // Relancez si vous ne savez pas comment gérer
  end;
end;
```

### 5. Contexte de l'erreur

Ajoutez du contexte à vos exceptions pour faciliter le débogage :

```pascal
procedure TraiterClient(ClientID: Integer);  
begin  
  try
    ChargerClient(ClientID);
  except
    on E: Exception do
      raise Exception.Create('Erreur lors du traitement du client ' +
                            IntToStr(ClientID) + ' : ' + E.Message);
  end;
end;
```

### 6. Exceptions vs valeurs de retour

Pour les erreurs attendues, utilisez parfois des valeurs de retour plutôt que des exceptions :

```pascal
// Pour une conversion qui peut échouer souvent
function TryConvertirEnEntier(const Texte: string; out Valeur: Integer): Boolean;  
begin  
  Result := TryStrToInt(Texte, Valeur);
end;

// Usage
if TryConvertirEnEntier(Edit1.Text, Age) then
  // Utiliser Age
else
  ShowMessage('Nombre invalide');
```

Les exceptions sont pour les situations **exceptionnelles**, pas pour le contrôle de flux normal.

### 7. Gestion au bon niveau

Gérez les exceptions au niveau où vous avez suffisamment d'informations et de contexte pour prendre une décision :

```pascal
// Bas niveau - Peut juste journaliser et relancer
procedure ChargerFichier(const Nom: string);  
begin  
  try
    // Charger...
  except
    on E: Exception do
    begin
      Logger.Error('Échec chargement ' + Nom);
      raise;  // Laisser le niveau supérieur décider quoi faire
    end;
  end;
end;

// Haut niveau - Peut gérer et informer l'utilisateur
procedure BoutonChargerClick(Sender: TObject);  
begin  
  try
    ChargerFichier(NomFichier);
  except
    on E: Exception do
      MessageDlg('Impossible de charger le fichier : ' + E.Message,
                 mtError, [mbOK], 0);
  end;
end;
```

## Introduction à la journalisation (Logging)

### Qu'est-ce que la journalisation ?

La **journalisation** (ou **logging**) est le processus d'enregistrement d'informations sur ce qui se passe dans votre application pendant son exécution. C'est comme tenir un journal de bord détaillé de votre programme.

### Pourquoi journaliser ?

**Déboguer plus efficacement** : Les logs vous disent exactement ce qui s'est passé avant une erreur, même en production où vous ne pouvez pas utiliser le débogueur.

**Auditer les actions** : Savoir qui a fait quoi et quand (important pour les applications d'entreprise).

**Analyser les performances** : Identifier les opérations lentes.

**Comprendre le comportement utilisateur** : Voir comment les utilisateurs utilisent réellement votre application.

**Support technique** : Les logs permettent de diagnostiquer des problèmes rapportés par les utilisateurs.

### Niveaux de journalisation

Les systèmes de logging utilisent généralement des niveaux pour catégoriser les messages :

**TRACE** : Informations très détaillées, utilisées uniquement pour le débogage approfondi
- Exemple : "Entrée dans la fonction CalculerTVA avec montant=100"

**DEBUG** : Informations de débogage
- Exemple : "Connexion à la base de données établie"

**INFO** : Informations générales sur le flux de l'application
- Exemple : "Utilisateur Jean Dupont connecté"

**WARNING** : Situations anormales mais pas critiques
- Exemple : "Le cache est plein, certains éléments seront supprimés"

**ERROR** : Erreurs qui permettent quand même à l'application de continuer
- Exemple : "Impossible de charger l'image profil.jpg, utilisation de l'image par défaut"

**FATAL** : Erreurs graves qui peuvent empêcher l'application de continuer
- Exemple : "Impossible de se connecter à la base de données"

**Principe** : En production, vous logguerez généralement INFO et au-dessus. En développement, vous pouvez activer DEBUG ou TRACE.

## Implémentation simple de la journalisation

### Journalisation basique dans un fichier

Voici une implémentation simple pour commencer :

```pascal
unit SimpleLogger;

interface

uses
  System.SysUtils, System.Classes;

type
  TNiveauLog = (nlTrace, nlDebug, nlInfo, nlWarning, nlError, nlFatal);

  TSimpleLogger = class
  private
    FFichierLog: string;
    FNiveauMin: TNiveauLog;
    procedure EcrireLigne(const Niveau, Message: string);
  public
    constructor Create(const FichierLog: string; NiveauMin: TNiveauLog = nlInfo);

    procedure Trace(const Message: string);
    procedure Debug(const Message: string);
    procedure Info(const Message: string);
    procedure Warning(const Message: string);
    procedure Error(const Message: string);
    procedure Fatal(const Message: string);

    procedure LogException(const Contexte: string; E: Exception);
  end;

var
  Logger: TSimpleLogger;

implementation

constructor TSimpleLogger.Create(const FichierLog: string; NiveauMin: TNiveauLog);  
begin  
  inherited Create;
  FFichierLog := FichierLog;
  FNiveauMin := NiveauMin;
end;

procedure TSimpleLogger.EcrireLigne(const Niveau, Message: string);  
var  
  Fichier: TextFile;
  Ligne: string;
begin
  Ligne := Format('[%s] [%s] %s',
                  [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Niveau, Message]);

  AssignFile(Fichier, FFichierLog);
  if FileExists(FFichierLog) then
    Append(Fichier)
  else
    Rewrite(Fichier);
  try
    WriteLn(Fichier, Ligne);
  finally
    CloseFile(Fichier);
  end;
end;

procedure TSimpleLogger.Info(const Message: string);  
begin  
  if FNiveauMin <= nlInfo then
    EcrireLigne('INFO', Message);
end;

procedure TSimpleLogger.Error(const Message: string);  
begin  
  if FNiveauMin <= nlError then
    EcrireLigne('ERROR', Message);
end;

procedure TSimpleLogger.Warning(const Message: string);  
begin  
  if FNiveauMin <= nlWarning then
    EcrireLigne('WARNING', Message);
end;

procedure TSimpleLogger.Debug(const Message: string);  
begin  
  if FNiveauMin <= nlDebug then
    EcrireLigne('DEBUG', Message);
end;

procedure TSimpleLogger.Trace(const Message: string);  
begin  
  if FNiveauMin <= nlTrace then
    EcrireLigne('TRACE', Message);
end;

procedure TSimpleLogger.Fatal(const Message: string);  
begin  
  if FNiveauMin <= nlFatal then
    EcrireLigne('FATAL', Message);
end;

procedure TSimpleLogger.LogException(const Contexte: string; E: Exception);  
begin  
  Error(Format('%s - Exception %s : %s', [Contexte, E.ClassName, E.Message]));
end;

initialization
  Logger := TSimpleLogger.Create('application.log', nlInfo);

finalization
  Logger.Free;

end.
```

### Utilisation du logger

```pascal
procedure TraiterCommande(CommandeID: Integer);  
begin  
  Logger.Info('Début traitement commande ' + IntToStr(CommandeID));

  try
    // Traitement...
    Logger.Debug('Validation de la commande');
    ValiderCommande(CommandeID);

    Logger.Debug('Calcul du montant');
    CalculerMontant(CommandeID);

    Logger.Info('Commande ' + IntToStr(CommandeID) + ' traitée avec succès');
  except
    on E: Exception do
    begin
      Logger.LogException('Erreur traitement commande ' + IntToStr(CommandeID), E);
      raise;
    end;
  end;
end;
```

**Résultat dans application.log :**

```
[2025-10-18 14:30:15] [INFO] Début traitement commande 12345
[2025-10-18 14:30:15] [DEBUG] Validation de la commande
[2025-10-18 14:30:16] [DEBUG] Calcul du montant
[2025-10-18 14:30:16] [INFO] Commande 12345 traitée avec succès
```

## Bibliothèques de journalisation professionnelles

Pour des applications plus complexes, utilisez des bibliothèques éprouvées.

### Log4D

Log4D est un port du célèbre Log4j (Java) pour Delphi.

**Caractéristiques :**
- Hiérarchie de loggers
- Multiples destinations (fichier, console, email, base de données)
- Configuration par fichier
- Filtrage avancé
- Gestion de la rotation des logs

**Installation :**
Téléchargeable depuis GitHub ou GetIt Package Manager.

**Utilisation basique :**

```pascal
uses
  Log4D;

var
  Logger: TLogLogger;

procedure InitialiserLog;  
begin  
  Logger := TLogLogger.GetLogger('MonApplication');
  Logger.Level := Trace;
end;

procedure Exemple;  
begin  
  Logger.Info('Application démarrée');
  Logger.Debug('Configuration chargée');

  try
    TraiterDonnees;
  except
    on E: Exception do
      Logger.Error('Erreur dans TraiterDonnees', E);
  end;
end;
```

### CodeSite

CodeSite est un système de logging et de débogage en temps réel.

**Caractéristiques :**
- Visualisation en temps réel dans une application dédiée
- Envoi de données structurées (objets, listes)
- Capture d'écran et monitoring
- Version Express gratuite et versions payantes

**Utilisation :**

```pascal
uses
  CodeSiteLogging;

procedure Exemple;  
var  
  Client: TClient;
begin
  CodeSite.Send('Application démarrée');
  CodeSite.EnterMethod('TraiterClient');

  Client := ChargerClient(123);
  CodeSite.Send('Client chargé', Client);  // Envoie l'objet entier

  CodeSite.ExitMethod('TraiterClient');
end;
```

### EurekaLog

EurekaLog est principalement connu pour la gestion des crashs, mais inclut aussi des fonctionnalités de logging.

**Caractéristiques :**
- Capture automatique des exceptions non gérées
- Génération de rapports de bug détaillés
- Envoi automatique des rapports par email/web
- Journalisation intégrée

**Avantages :**
- Idéal pour capturer les bugs en production
- Rapports très détaillés avec stack trace complet
- Configuration facile

### mORMot Logging

Le framework mORMot inclut un système de logging très performant.

**Caractéristiques :**
- Extrêmement rapide
- Faible impact sur les performances
- Support du multithreading
- Rotation automatique des logs

**Utilisation :**

```pascal
uses
  mormot.core.log;

procedure Exemple;  
begin  
  TSynLog.Add.Log(sllInfo, 'Application démarrée');
  TSynLog.Add.Log(sllDebug, 'Mode debug activé');

  try
    TraiterDonnees;
  except
    on E: Exception do
      TSynLog.Add.Log(sllError, 'Erreur', E);
  end;
end;
```

## Stratégies de journalisation

### Que journaliser ?

**Toujours journaliser :**
- Démarrage et arrêt de l'application
- Connexions utilisateurs
- Erreurs et exceptions
- Opérations critiques (modifications de données, transactions)
- Changements de configuration

**Éviter de journaliser :**
- Données sensibles (mots de passe, numéros de carte bancaire)
- Informations personnelles excessives (RGPD/GDPR)
- Boucles avec des milliers d'itérations (sauf en mode TRACE temporaire)

### Rotation des logs

Les fichiers de log peuvent devenir très volumineux. Mettez en place une stratégie de rotation :

```pascal
type
  TRotationLog = class
  private
    FFichierBase: string;
    FTailleMax: Int64;  // en octets
    FNombreFichiersMax: Integer;
  public
    procedure VerifierRotation;
    procedure RoterFichiers;
  end;

procedure TRotationLog.VerifierRotation;  
var  
  TailleFichier: Int64;
begin
  if FileExists(FFichierBase) then
  begin
    TailleFichier := GetFileSize(FFichierBase);
    if TailleFichier > FTailleMax then
      RoterFichiers;
  end;
end;

procedure TRotationLog.RoterFichiers;  
var  
  i: Integer;
begin
  // Supprimer le plus ancien
  if FileExists(FFichierBase + '.' + IntToStr(FNombreFichiersMax)) then
    DeleteFile(FFichierBase + '.' + IntToStr(FNombreFichiersMax));

  // Renommer les fichiers existants
  for i := FNombreFichiersMax - 1 downto 1 do
    if FileExists(FFichierBase + '.' + IntToStr(i)) then
      RenameFile(FFichierBase + '.' + IntToStr(i),
                 FFichierBase + '.' + IntToStr(i + 1));

  // Renommer le fichier actuel
  if FileExists(FFichierBase) then
    RenameFile(FFichierBase, FFichierBase + '.1');
end;
```

**Stratégies courantes :**
- Par taille : Nouveau fichier quand le courant dépasse X Mo
- Par date : Un nouveau fichier chaque jour/semaine/mois
- Par nombre : Garder les N derniers fichiers

### Journalisation structurée

Pour faciliter l'analyse, utilisez un format structuré (JSON) :

```pascal
procedure LoggerJSON(const Niveau, Message: string; Donnees: TJSONObject);  
var  
  LogEntry: TJSONObject;
begin
  LogEntry := TJSONObject.Create;
  try
    LogEntry.AddPair('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Now));
    LogEntry.AddPair('level', Niveau);
    LogEntry.AddPair('message', Message);

    if Assigned(Donnees) then
      LogEntry.AddPair('data', Donnees);

    EcrireLigneJSON(LogEntry.ToString);
  finally
    LogEntry.Free;
  end;
end;
```

**Résultat :**

```json
{
  "timestamp": "2025-10-18T14:30:15",
  "level": "ERROR",
  "message": "Échec connexion base de données",
  "data": {
    "server": "db.example.com",
    "database": "production",
    "retry_count": 3
  }
}
```

Les logs JSON sont facilement analysables par des outils comme ELK Stack, Splunk, etc.

### Journalisation asynchrone

Pour minimiser l'impact sur les performances, journalisez de manière asynchrone :

```pascal
type
  TAsyncLogger = class
  private
    FQueue: TThreadedQueue<string>;
    FThread: TThread;
    procedure ProcessQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Message: string);
  end;

constructor TAsyncLogger.Create;  
begin  
  FQueue := TThreadedQueue<string>.Create;

  FThread := TThread.CreateAnonymousThread(ProcessQueue);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

procedure TAsyncLogger.Log(const Message: string);  
begin  
  FQueue.PushItem(Message);
end;

procedure TAsyncLogger.ProcessQueue;  
var  
  Message: string;
begin
  while not TThread.Current.CheckTerminated do
  begin
    if FQueue.PopItem(Message) = wrSignaled then
      EcrireDansFichier(Message);
  end;
end;
```

## Intégration exception + journalisation

### Gestionnaire d'exception global

Delphi permet de capturer toutes les exceptions non gérées au niveau de l'application :

```pascal
procedure TMainForm.FormCreate(Sender: TObject);  
begin  
  // Définir un gestionnaire global d'exceptions
  Application.OnException := GererExceptionGlobale;
end;

procedure TMainForm.GererExceptionGlobale(Sender: TObject; E: Exception);  
begin  
  // Journaliser l'exception
  Logger.Fatal(Format('Exception non gérée : %s - %s',
                     [E.ClassName, E.Message]));

  // Afficher un message utilisateur convivial
  MessageDlg('Une erreur inattendue s''est produite. ' +
             'L''équipe technique a été notifiée.',
             mtError, [mbOK], 0);

  // Optionnel : Envoyer un rapport d'erreur
  EnvoyerRapportErreur(E);
end;
```

### Pattern standard : Try-Except-Finally avec logging

```pascal
procedure OperationComplete;  
var  
  Ressource: TRessource;
begin
  Logger.Info('Début OperationComplete');

  Ressource := TRessource.Create;
  try
    try
      Logger.Debug('Ressource créée');

      // Opérations sur la ressource
      Ressource.Traiter;

      Logger.Info('OperationComplete terminée avec succès');
    except
      on E: Exception do
      begin
        Logger.LogException('Erreur dans OperationComplete', E);
        raise;  // Relancer pour que l'appelant puisse gérer
      end;
    end;
  finally
    Ressource.Free;
    Logger.Debug('Ressource libérée');
  end;
end;
```

### Enrichir les exceptions avec du contexte

Créez une fonction helper pour enrichir les exceptions :

```pascal
procedure RaiseWithContext(const Contexte: string; E: Exception);  
begin  
  Logger.Error(Format('%s - %s: %s', [Contexte, E.ClassName, E.Message]));
  raise Exception.Create(Contexte + ' : ' + E.Message);
end;

// Utilisation
procedure TraiterClient(ClientID: Integer);  
begin  
  try
    ChargerDonneesClient(ClientID);
  except
    on E: Exception do
      RaiseWithContext('Impossible de charger le client ' + IntToStr(ClientID), E);
  end;
end;
```

## Diagnostic et analyse des logs

### Analyser les fichiers de log

Pour analyser vos logs, plusieurs approches :

**Éditeur de texte avec recherche** : Pour des analyses simples

**Outils de ligne de commande** :
```bash
# Compter les erreurs
grep "ERROR" application.log | wc -l

# Afficher les dernières erreurs
grep "ERROR" application.log | tail -20

# Trouver toutes les erreurs d'un utilisateur spécifique
grep "ERROR.*Jean Dupont" application.log
```

**Outils graphiques** :
- LogExpert (gratuit, pour Windows)
- Loggly (service web)
- Papertrail (service web)
- ELK Stack (Elasticsearch, Logstash, Kibana) pour les gros volumes

### Créer des alertes

Pour les erreurs critiques, mettez en place des alertes automatiques :

```pascal
procedure TLogger.Fatal(const Message: string);  
begin  
  // Log normal
  EcrireLigne('FATAL', Message);

  // Envoyer une alerte email
  if FEnvoyerAlertes then
    EnvoyerEmailAlerte('ALERTE CRITIQUE', Message);

  // Ou envoyer vers un service de monitoring
  EnvoyerVersMonitoring('FATAL', Message);
end;
```

### Dashboard de monitoring

Pour les applications critiques, créez un dashboard qui affiche :
- Nombre d'erreurs dans les dernières 24h
- Tendances (augmentation des erreurs ?)
- Types d'erreurs les plus fréquentes
- Utilisateurs affectés

## Conseils pour débutants

### Commencez simple

Ne vous compliquez pas la vie au début. Un simple logger écrivant dans un fichier texte est suffisant pour commencer.

### Loggez aux points clés

Identifiez les moments importants de votre application :
- Démarrage
- Actions utilisateur importantes
- Début et fin de processus longs
- Erreurs

### Utilisez des messages descriptifs

```pascal
// MAUVAIS
Logger.Error('Erreur');

// BON
Logger.Error('Échec de connexion à la base de données MySQL sur srv-prod.example.com:3306');
```

### N'ayez pas peur de trop logger pendant le développement

En phase de développement, loggez généreusement. Vous pourrez toujours réduire le niveau de log en production (INFO au lieu de DEBUG).

### Testez vos logs

Vérifiez régulièrement que vos logs sont lisibles et contiennent les informations nécessaires pour diagnostiquer les problèmes.

### Protégez les données sensibles

```pascal
// MAUVAIS
Logger.Debug('Login : ' + Username + ', Password : ' + Password);

// BON
Logger.Debug('Tentative de connexion pour l''utilisateur : ' + Username);
```

### Rotation automatique

Même en développement, activez la rotation des logs pour éviter des fichiers de plusieurs Go.

### Gardez les logs accessibles

Placez vos fichiers de log dans un endroit facile à trouver, par exemple le dossier de l'application ou un sous-dossier `Logs`.

## Outils pour faciliter le développement

### Macro pour logging automatique

Créez des macros pour simplifier le logging :

```pascal
{$IFDEF DEBUG}
  {$DEFINE LOG_ENTRY_EXIT}
{$ENDIF}

procedure MaFonction;  
begin  
  {$IFDEF LOG_ENTRY_EXIT}
  Logger.Trace('>> MaFonction');
  {$ENDIF}

  try
    // Code de la fonction
  finally
    {$IFDEF LOG_ENTRY_EXIT}
    Logger.Trace('<< MaFonction');
    {$ENDIF}
  end;
end;
```

### Classe helper pour mesurer les performances

```pascal
type
  TLogTimer = class
  private
    FNom: string;
    FChrono: TStopwatch;
  public
    constructor Create(const Nom: string);
    destructor Destroy; override;
  end;

constructor TLogTimer.Create(const Nom: string);  
begin  
  FNom := Nom;
  Logger.Debug(FNom + ' : début');
  FChrono := TStopwatch.StartNew;
end;

destructor TLogTimer.Destroy;  
begin  
  FChrono.Stop;
  Logger.Debug(Format('%s : terminé en %d ms',
                     [FNom, FChrono.ElapsedMilliseconds]));
  inherited;
end;

// Utilisation
procedure TraitementLong;  
var  
  Timer: TLogTimer;
begin
  Timer := TLogTimer.Create('TraitementLong');
  try
    // Votre code ici
    Sleep(1000);
  finally
    Timer.Free;  // Logge automatiquement la durée
  end;
end;
```

## Checklist finale

Pour une gestion robuste des exceptions et de la journalisation :

**□ Gestion des exceptions**
- [ ] Tous les objets créés sont libérés dans des blocs `try...finally`
- [ ] Les exceptions spécifiques sont capturées quand approprié
- [ ] Les messages d'erreur sont clairs et utiles
- [ ] Les exceptions ne sont pas "avalées" silencieusement
- [ ] Un gestionnaire global d'exceptions est configuré
- [ ] Les ressources système sont correctement libérées même en cas d'erreur

**□ Journalisation**
- [ ] Un système de logging est en place
- [ ] Les niveaux de log appropriés sont utilisés
- [ ] Les messages de log contiennent du contexte
- [ ] Les données sensibles ne sont pas loggées
- [ ] La rotation des logs est configurée
- [ ] Les exceptions sont systématiquement loggées
- [ ] Les logs sont accessibles pour le diagnostic

**□ Production**
- [ ] Le niveau de log en production est approprié (INFO ou WARNING)
- [ ] Les alertes critiques sont configurées
- [ ] Un plan existe pour accéder aux logs en production
- [ ] La taille des logs est surveillée

## Conclusion

La gestion des exceptions et la journalisation sont deux piliers d'une application Delphi robuste et maintenable. Bien maîtrisés, ces concepts vous permettent de :

**Créer des applications fiables** qui gèrent élégamment les situations d'erreur plutôt que de planter mystérieusement.

**Diagnostiquer rapidement les problèmes** grâce aux informations détaillées dans les logs.

**Améliorer continuellement** votre application en analysant les erreurs rencontrées en production.

**Gagner du temps** lors du débogage en ayant une trace précise de ce qui s'est passé.

**Points clés à retenir :**

- Utilisez `try...except` pour gérer les erreurs de manière contrôlée
- Utilisez `try...finally` pour garantir la libération des ressources
- Ne capturez que les exceptions que vous pouvez réellement gérer
- Loggez les informations importantes avec des messages clairs et contextualisés
- Utilisez les niveaux de log appropriés (TRACE, DEBUG, INFO, WARNING, ERROR, FATAL)
- Protégez les données sensibles dans vos logs
- Mettez en place une rotation des logs pour gérer la taille des fichiers
- Intégrez un gestionnaire global d'exceptions pour capturer les erreurs non gérées

En appliquant ces principes dès maintenant dans vos projets Delphi, vous poserez les bases d'applications professionnelles, faciles à maintenir et à déboguer. Ces compétences vous distingueront en tant que développeur sérieux et consciencieux.

⏭️ [Débogage à distance](/12-debogage-et-tests/06-debogage-a-distance.md)
