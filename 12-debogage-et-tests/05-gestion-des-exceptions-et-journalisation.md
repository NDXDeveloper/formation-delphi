# 12.5 Gestion des exceptions et journalisation

## Introduction à la gestion des exceptions

Les exceptions représentent un mécanisme puissant pour gérer les erreurs et les situations imprévues dans votre code. Contrairement aux méthodes traditionnelles de gestion d'erreurs basées sur des codes de retour, les exceptions permettent de séparer clairement le code de traitement normal du code de gestion des erreurs.

En parallèle, la journalisation (logging) est essentielle pour suivre ce qui se passe dans votre application, en particulier lorsque des problèmes surviennent. Ensemble, ces deux techniques constituent le fondement d'applications Delphi robustes et faciles à déboguer.

## Comprendre les exceptions en Delphi

### Qu'est-ce qu'une exception ?

Une exception est un objet qui encapsule une information sur une erreur. Lorsqu'une erreur se produit, une exception est "levée" (ou "lancée"). Le programme interrompt alors son exécution normale et recherche un gestionnaire d'exceptions capable de traiter le problème.

### La hiérarchie des exceptions en Delphi

Delphi définit une hiérarchie d'exceptions à partir de la classe de base `Exception` :

```
Exception (classe de base)
  ├── EAbort             - Interruption demandée (non considérée comme erreur)
  ├── EAccessViolation   - Accès mémoire non autorisé
  ├── EConvertError      - Erreur de conversion de type
  ├── EDatabaseError     - Erreurs de base de données
  ├── EIntOverflow       - Dépassement d'entier
  ├── EInvalidOp         - Opération invalide
  ├── EInvalidPointer    - Pointeur invalide
  ├── EMathError         - Erreur mathématique
  │   ├── EZeroDivide    - Division par zéro
  │   └── EOverflow      - Dépassement numérique
  ├── EOutOfMemory       - Mémoire insuffisante
  └── ... et bien d'autres
```

Cette hiérarchie vous permet d'attraper des catégories spécifiques d'exceptions ou de créer vos propres types d'exceptions adaptés à votre application.

## Structure try-except-finally

Delphi utilise la structure `try-except-finally` pour gérer les exceptions :

### Structure try-except

```pascal
try
  // Code qui pourrait lever une exception
  Resultat := 10 div Diviseur; // Pourrait lever EZeroDivide si Diviseur = 0
except
  on E: EZeroDivide do
    ShowMessage('Division par zéro détectée !');
  on E: EMathError do
    ShowMessage('Autre erreur mathématique: ' + E.Message);
  on E: Exception do
    ShowMessage('Erreur: ' + E.Message);
end;
```

Structure :
- Le bloc `try` contient le code qui pourrait générer une exception
- Les clauses `on E: TypeException do` capturent des types spécifiques d'exceptions
- Les gestionnaires sont vérifiés dans l'ordre, donc placez les exceptions plus spécifiques en premier

### Structure try-finally

```pascal
Fichier := TFileStream.Create('data.txt', fmOpenRead);
try
  // Opérations sur le fichier
finally
  // Ce code s'exécute toujours, même si une exception est levée
  Fichier.Free;
end;
```

`try-finally` garantit que le code de nettoyage (comme la libération des ressources) s'exécute toujours, qu'une exception soit levée ou non.

### Structure try-except-finally combinée

Vous pouvez combiner les deux structures pour à la fois gérer les exceptions et garantir le nettoyage :

```pascal
Fichier := nil;
try
  try
    Fichier := TFileStream.Create('data.txt', fmOpenRead);
    // Opérations sur le fichier qui pourraient lever une exception
  except
    on E: EFOpenError do
      ShowMessage('Impossible d''ouvrir le fichier: ' + E.Message);
    on E: Exception do
      ShowMessage('Erreur: ' + E.Message);
  end;
finally
  // Ce code s'exécute toujours
  if Assigned(Fichier) then
    Fichier.Free;
end;
```

## Créer et lever vos propres exceptions

Delphi vous permet de créer vos propres types d'exceptions adaptés à votre application :

### Création d'une classe d'exception personnalisée

```pascal
type
  EConfigurationError = class(Exception)
  private
    FConfigFile: string;
  public
    constructor Create(const Msg, ConfigFile: string);
    property ConfigFile: string read FConfigFile;
  end;

constructor EConfigurationError.Create(const Msg, ConfigFile: string);
begin
  inherited Create(Msg);
  FConfigFile := ConfigFile;
end;
```

### Lever une exception

Pour signaler une erreur dans votre code, utilisez `raise` :

```pascal
procedure ChargerConfiguration(const Fichier: string);
begin
  if not FileExists(Fichier) then
    raise EConfigurationError.Create('Fichier de configuration introuvable', Fichier);

  // Reste du code...
end;
```

## Bonnes pratiques pour la gestion des exceptions

### 1. Spécificité des exceptions

Attrapez les exceptions les plus spécifiques possibles, pas seulement `Exception` :

```pascal
// Moins bon - attrape tout
try
  // Code
except
  // Gestion générique
end;

// Meilleur - exceptions spécifiques
try
  // Code
except
  on E: EDatabaseError do
    // Gestion spécifique des erreurs de base de données
  on E: EFileNotFoundError do
    // Gestion spécifique des fichiers manquants
  on E: Exception do
    // Gestion générique en dernier recours
end;
```

### 2. Limiter la portée des blocs try

Gardez les blocs `try` aussi petits que possible pour cibler précisément la source des exceptions :

```pascal
// Moins bon
try
  Fichier := TFileStream.Create(NomFichier, fmOpenRead);
  // Beaucoup d'opérations...
  DoSomething;
  DoSomethingElse;
except
  // Difficile de savoir d'où vient l'exception
end;

// Meilleur
try
  Fichier := TFileStream.Create(NomFichier, fmOpenRead);
except
  on E: EFOpenError do
    // Gestion spécifique à l'ouverture du fichier
end;

try
  // Opérations avec le fichier
except
  on E: EReadError do
    // Gestion spécifique aux erreurs de lecture
end;
```

### 3. Réfléchir à la récupération

Quand une exception se produit, réfléchissez à la meilleure stratégie :

- **Rattraper et continuer** : Corrigez le problème et continuez
- **Rattraper et arrêter proprement** : Informez l'utilisateur et terminez l'opération
- **Re-lever** : Ajoutez des informations et propagez l'exception à un niveau supérieur
- **Convertir** : Transformez en un type d'exception plus approprié

Exemple de re-levage avec informations supplémentaires :

```pascal
try
  // Opération qui lève une exception
except
  on E: EDatabaseError do
  begin
    Log('Erreur de base de données dans ImporterDonnees: ' + E.Message);
    raise Exception.Create('Échec de l''importation des données : ' + E.Message);
  end;
end;
```

### 4. Utiliser try-finally pour les ressources

Utilisez toujours `try-finally` lors de l'acquisition de ressources :

```pascal
procedure ProcessFichier(const NomFichier: string);
var
  Fichier: TFileStream;
begin
  Fichier := TFileStream.Create(NomFichier, fmOpenRead);
  try
    // Traitement du fichier
  finally
    Fichier.Free; // Garantit la libération du fichier
  end;
end;
```

> 💡 **Astuce** : Dans les versions récentes de Delphi, vous pouvez utiliser les directives `{$REGION}` et `{$ENDREGION}` pour regrouper visuellement les blocs try-except-finally et améliorer la lisibilité du code.

## Introduction à la journalisation (logging)

La journalisation consiste à enregistrer les événements et les informations pertinentes pendant l'exécution de votre application. C'est un complément essentiel à la gestion des exceptions.

### Pourquoi journaliser ?

- **Débogage** : Comprendre ce qui s'est passé avant un crash
- **Diagnostic** : Identifier les problèmes chez les utilisateurs
- **Audit** : Suivre qui a fait quoi et quand
- **Performance** : Mesurer les temps d'exécution
- **Statistiques** : Collecter des données d'utilisation

### Niveaux de journalisation

La plupart des systèmes de journalisation définissent différents niveaux d'importance :

1. **Fatal/Critique** : Erreurs catastrophiques qui provoquent l'arrêt du programme
2. **Erreur** : Erreurs graves mais qui permettent à l'application de continuer
3. **Avertissement** : Situations anormales mais non critiques
4. **Info** : Informations générales sur le déroulement normal
5. **Debug** : Informations détaillées utiles en développement
6. **Trace** : Informations très détaillées (entrée/sortie de méthodes)

## Techniques de journalisation en Delphi

### 1. Journalisation simple avec des fichiers texte

```pascal
procedure Log(const Message: string; const Level: string = 'INFO');
var
  LogFile: TextFile;
  LogFileName: string;
begin
  LogFileName := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'application.log';

  AssignFile(LogFile, LogFileName);
  try
    if FileExists(LogFileName) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    WriteLn(LogFile, Format('%s [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Level, Message]));
  finally
    CloseFile(LogFile);
  end;
end;

// Utilisation
procedure MaFonction;
begin
  Log('Début de MaFonction', 'INFO');
  try
    // Code...
    if SituationProblematique then
      Log('Situation inhabituelle détectée', 'WARNING');
    // Plus de code...
  except
    on E: Exception do
    begin
      Log('Erreur dans MaFonction: ' + E.Message, 'ERROR');
      raise; // Re-lever l'exception
    end;
  end;
  Log('Fin de MaFonction', 'INFO');
end;
```

Cette approche simple a ses limites (performances, flexibilité), mais peut être suffisante pour les petites applications.

### 2. Utilisation de bibliothèques de journalisation

Pour les applications plus complexes, utilisez une bibliothèque de journalisation. Delphi intègre plusieurs options :

#### a. Journalisation avec CodeSite

CodeSite est un système avancé de journalisation et de visualisation inclus dans Delphi :

```pascal
uses
  CodeSiteLogging;

procedure ExempleFonction;
begin
  CodeSite.EnterMethod('ExempleFonction');
  try
    // Code
    CodeSite.Send('Valeur', MaVariable);

    if ConditionProblematique then
      CodeSite.SendWarning('Attention : situation inhabituelle');

    // Plus de code
  except
    on E: Exception do
    begin
      CodeSite.SendException(E);
      raise;
    end;
  end;
  CodeSite.ExitMethod('ExempleFonction');
end;
```

#### b. Log4Delphi ou Log4D

Ces bibliothèques inspirées de Log4j offrent une journalisation flexible et configurable :

```pascal
uses
  Log4D;

procedure ConfigurerLogging;
var
  Appender: TLogFileAppender;
  Layout: TLogPatternLayout;
begin
  Layout := TLogPatternLayout.Create('%d [%p] %m%n');

  Appender := TLogFileAppender.Create('application.log');
  Appender.Layout := Layout;

  TLogLogger.GetRootLogger.AddAppender(Appender);
  TLogLogger.GetRootLogger.Level := INFO;
end;

procedure UtiliserLogging;
var
  Logger: TLogLogger;
begin
  Logger := TLogLogger.GetLogger('MaClasse');

  Logger.Info('Application démarrée');
  Logger.Debug('Valeur X: ' + IntToStr(X));

  try
    // Code qui peut lever une exception
  except
    on E: Exception do
    begin
      Logger.Error('Erreur: ' + E.Message);
      raise;
    end;
  end;
end;
```

### 3. Journalisation avec les EventLog Windows

Pour les applications Windows, vous pouvez utiliser le journal d'événements du système :

```pascal
uses
  Winapi.Windows;

procedure LogToEventLog(const Msg: string; EventType: DWORD);
var
  EventSource: THandle;
  MessagePtr: PChar;
begin
  EventSource := RegisterEventSource(nil, 'MonApplication');
  if EventSource <> 0 then
  begin
    try
      MessagePtr := PChar(Msg);
      ReportEvent(EventSource, EventType, 0, 0, nil, 1, 0, @MessagePtr, nil);
    finally
      DeregisterEventSource(EventSource);
    end;
  end;
end;

// Utilisation
LogToEventLog('Application démarrée', EVENTLOG_INFORMATION_TYPE);
LogToEventLog('Erreur critique!', EVENTLOG_ERROR_TYPE);
```

## Intégration de la gestion des exceptions et de la journalisation

La meilleure approche consiste à combiner gestion des exceptions et journalisation :

### Gestionnaire d'exceptions global

Delphi permet de définir un gestionnaire d'exceptions au niveau de l'application :

```pascal
uses
  Vcl.Forms,
  System.SysUtils;

procedure GestionnaireExceptionsGlobal(Sender: TObject; E: Exception);
var
  ErrorMsg: string;
begin
  ErrorMsg := Format('Erreur non gérée: %s%sStack Trace:%s%s',
    [E.Message, sLineBreak, sLineBreak, ExceptAddr]);

  // Journaliser l'erreur
  Log(ErrorMsg, 'FATAL');

  // Informer l'utilisateur
  MessageDlg('Une erreur imprévue est survenue. ' +
    'L''erreur a été enregistrée. Veuillez contacter le support technique.',
    mtError, [mbOK], 0);
end;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  // Définir le gestionnaire d'exceptions global
  Application.OnException := GestionnaireExceptionsGlobal;

  // Suite du code...
end.
```

### Création d'une unité de journalisation centralisée

Pour plus d'organisation, créez une unité dédiée à la journalisation :

```pascal
// LogUtils.pas
unit LogUtils;

interface

type
  TLogLevel = (llFatal, llError, llWarning, llInfo, llDebug, llTrace);

procedure InitializeLogger(const LogFileName: string = '');
procedure FinalizeLogger;
procedure Log(const Msg: string; Level: TLogLevel = llInfo);
procedure LogException(E: Exception; const ContextInfo: string = '');
function LevelToString(Level: TLogLevel): string;

implementation

uses
  System.SysUtils, System.Classes, System.SyncObjs;

var
  LogFile: TextFile;
  LogFileName: string;
  LogCS: TCriticalSection;

function LevelToString(Level: TLogLevel): string;
begin
  case Level of
    llFatal:   Result := 'FATAL';
    llError:   Result := 'ERROR';
    llWarning: Result := 'WARNING';
    llInfo:    Result := 'INFO';
    llDebug:   Result := 'DEBUG';
    llTrace:   Result := 'TRACE';
  end;
end;

procedure InitializeLogger(const LogFileName: string = '');
begin
  if LogFileName = '' then
    LogUtils.LogFileName := ChangeFileExt(ParamStr(0), '.log')
  else
    LogUtils.LogFileName := LogFileName;

  LogCS := TCriticalSection.Create;

  AssignFile(LogFile, LogUtils.LogFileName);
  if FileExists(LogUtils.LogFileName) then
    Append(LogFile)
  else
    Rewrite(LogFile);

  Log('=== Application démarrée ===', llInfo);
end;

procedure FinalizeLogger;
begin
  Log('=== Application terminée ===', llInfo);
  CloseFile(LogFile);
  LogCS.Free;
end;

procedure Log(const Msg: string; Level: TLogLevel = llInfo);
var
  TimeStamp: string;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);

  LogCS.Enter;
  try
    WriteLn(LogFile, Format('%s [%s] %s', [TimeStamp, LevelToString(Level), Msg]));
    Flush(LogFile);

    // En mode debug, on affiche aussi dans la console de débogage
    {$IFDEF DEBUG}
    OutputDebugString(PChar(Format('%s [%s] %s', [TimeStamp, LevelToString(Level), Msg])));
    {$ENDIF}
  finally
    LogCS.Leave;
  end;
end;

procedure LogException(E: Exception; const ContextInfo: string = '');
var
  ExceptionMessage: string;
begin
  if ContextInfo <> '' then
    ExceptionMessage := Format('%s: %s', [ContextInfo, E.Message])
  else
    ExceptionMessage := E.Message;

  Log(ExceptionMessage, llError);

  // Journaliser des informations supplémentaires si disponibles
  if E is EInOutError then
    Log(Format('Code d''erreur IO: %d', [EInOutError(E).ErrorCode]), llError);
end;

initialization
  // Ne rien faire ici, appeler InitializeLogger explicitement

finalization
  // Si le logger a été initialisé
  if LogCS <> nil then
    FinalizeLogger;

end.
```

### Utilisation dans l'application

```pascal
uses
  LogUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  InitializeLogger;
  Log('Application démarrée', llInfo);

  try
    // Initialisation...
  except
    on E: Exception do
    begin
      LogException(E, 'Erreur lors de l''initialisation');
      raise; // Propager au gestionnaire global
    end;
  end;
end;

procedure TMainForm.BoutonImporterClick(Sender: TObject);
begin
  Log('Début importation', llInfo);
  try
    // Code d'importation...
    Log('Importation réussie', llInfo);
  except
    on E: Exception do
    begin
      Log('Échec de l''importation', llError);
      LogException(E);
      ShowMessage('L''importation a échoué : ' + E.Message);
      // Ne pas relever, l'erreur est gérée localement
    end;
  end;
end;
```

## Journalisation en production vs développement

Il est utile d'adapter le niveau de détail des journaux selon l'environnement :

### Directives de compilation conditionnelles

```pascal
{$IFDEF DEBUG}
  // Journalisation très détaillée en développement
  LogLevel := llTrace;
{$ELSE}
  // Journalisation minimale en production
  LogLevel := llWarning;
{$ENDIF}
```

### Configuration dynamique

Permettez de configurer le niveau de journalisation via un fichier de configuration :

```pascal
procedure ChargerConfigurationLogger;
var
  IniFile: TIniFile;
  LogLevelStr: string;
begin
  IniFile := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    LogLevelStr := IniFile.ReadString('Logging', 'Level', 'INFO');

    // Convertir la chaîne en niveau de journalisation
    if LogLevelStr = 'DEBUG' then
      CurrentLogLevel := llDebug
    else if LogLevelStr = 'TRACE' then
      CurrentLogLevel := llTrace
    // ... autres niveaux ...
    else
      CurrentLogLevel := llInfo;
  finally
    IniFile.Free;
  end;
end;
```

## Journalisation avancée avec Delphi 12 Athens

> 💡 **Nécessite Delphi 12 ou supérieur**

Delphi 12 Athens apporte des améliorations significatives à la journalisation :

### 1. Journalisation structurée

```pascal
TLogger.Log(TLogLevel.Info, 'Paiement reçu', [
  TLogField.Create('ClientID', ClientID),
  TLogField.Create('Montant', Montant),
  TLogField.Create('Date', Now)
]);
```

### 2. Intégration avec les outils d'observabilité

```pascal
// Configuration du logger pour envoyer les données à un service externe
TLoggerFactory.Provider.SetDestination(TLogDestinationType.RemoteService,
  'https://monitoring.exemple.com/api/logs');
```

### 3. Journalisation des exceptions améliorée

```pascal
try
  // Code qui peut lever une exception
except
  on E: Exception do
  begin
    // Journalisation enrichie avec contexte et capture automatique de la stack trace
    TLogger.LogException(E, 'Échec de l''opération X', [
      TLogField.Create('OperationID', OperationID),
      TLogField.Create('ContextData', SommeImportante)
    ]);
    raise;
  end;
end;
```

## Conclusion

Une gestion efficace des exceptions combinée à une stratégie de journalisation bien pensée constituent les fondements d'applications Delphi robustes et maintenables. Ces techniques vous permettent non seulement de réagir de manière élégante aux erreurs mais aussi de les diagnostiquer et de les corriger rapidement.

Souvenez-vous que la journalisation n'est pas seulement utile en cas d'erreur, mais aussi pour comprendre le comportement normal de votre application et suivre son utilisation.

En pratique, commencez simplement avec une journalisation basique, puis évoluez vers des solutions plus sophistiquées au fur et à mesure que votre application grandit en complexité.

Dans la prochaine section, nous aborderons le débogage à distance, une technique puissante pour résoudre les problèmes qui ne se manifestent que dans des environnements spécifiques.
