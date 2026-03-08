🔝 Retour au [Sommaire](/SOMMAIRE.md)

# 19.4 Services Windows et applications de fond

## Introduction

Bienvenue dans le monde des applications qui fonctionnent en coulisses ! Dans ce chapitre, vous allez découvrir comment créer des services Windows et des applications de fond qui s'exécutent sans interface utilisateur, travaillant silencieusement pour accomplir des tâches automatiques.

### Qu'est-ce qu'un service Windows ?

Un service Windows est un type particulier d'application qui :

🔧 **S'exécute en arrière-plan** sans interface utilisateur visible  
🔧 **Démarre automatiquement** avec Windows (si configuré)  
🔧 **Fonctionne indépendamment** de toute connexion utilisateur  
🔧 **Peut s'exécuter avec des privilèges élevés** (compte système)  
🔧 **Continue à fonctionner** même si personne n'est connecté

**Exemples de services Windows** :
- Serveur web (IIS)
- Serveur de bases de données (SQL Server)
- Antivirus
- Service d'impression
- Service de sauvegarde automatique
- Surveillance système

### Pourquoi créer des services Windows ?

Les services Windows sont idéaux pour :

✅ **Tâches automatiques** : Sauvegardes, synchronisations, traitements planifiés  
✅ **Surveillance continue** : Monitoring de fichiers, réseau, système  
✅ **Serveurs** : Web, FTP, bases de données, API  
✅ **Traitement en arrière-plan** : File d'attente de tâches  
✅ **Intégration système** : Interaction avec d'autres services Windows

### Objectifs de ce chapitre

À la fin de ce tutoriel, vous serez capable de :

✅ Comprendre l'architecture des services Windows  
✅ Créer un service Windows avec Delphi  
✅ Gérer le cycle de vie d'un service  
✅ Installer et désinstaller des services  
✅ Déboguer des services  
✅ Implémenter la journalisation  
✅ Gérer la communication inter-processus  
✅ Créer des applications de fond  
✅ Gérer les tâches planifiées

### Prérequis

Avant de commencer, assurez-vous de :

**Connaissances** :
- ✅ Bases de Delphi et Object Pascal
- ✅ Compréhension des threads
- ✅ Notions de gestion de fichiers

**Environnement** :
- ✅ Delphi 13 Florence installé
- ✅ Windows 10/11
- ✅ Droits administrateur sur votre machine
- ✅ Visual Studio Code ou éditeur de logs (optionnel)

### Durée estimée

**10 à 15 heures** de travail, réparties ainsi :
- Compréhension des concepts : 2-3 heures
- Création d'un service de base : 3-4 heures
- Fonctionnalités avancées : 3-4 heures
- Tests et débogage : 2-3 heures
- Déploiement : 1 heure

---

## Partie 1 : Comprendre les services Windows

### 1.1 Architecture d'un service Windows

Un service Windows a une structure particulière différente d'une application normale :

```
┌─────────────────────────────────────┐
│   Windows Service Control Manager   │
│            (SCM)                    │
└────────────┬────────────────────────┘
             │
             │ Contrôle
             ↓
┌─────────────────────────────────────┐
│      Votre Service Windows          │
│                                     │
│  ┌─────────────────────────────┐    │
│  │   Service Thread            │    │
│  │   (Logique métier)          │    │
│  └─────────────────────────────┘    │
│                                     │
│  ┌─────────────────────────────┐    │
│  │   Journalisation            │    │
│  └─────────────────────────────┘    │
└─────────────────────────────────────┘
```

**Composants clés** :

1. **SCM (Service Control Manager)** : Gestionnaire de services Windows
2. **Service Thread** : Thread principal qui exécute le travail
3. **Gestionnaire d'événements** : Répond aux commandes (start, stop, pause)
4. **Journal** : Enregistre les événements et erreurs

### 1.2 Cycle de vie d'un service

Un service passe par plusieurs états :

```
[Installé] → [Arrêté] → [Démarrage] → [En cours d'exécution] → [Arrêt] → [Arrêté]
                              ↓              ↑
                          [Pause] ─────→ [Reprise]
```

**États d'un service** :

| État | Description |
|------|-------------|
| **Arrêté** (Stopped) | Le service n'est pas actif |
| **Démarrage** (Starting) | Le service est en train de démarrer |
| **En cours** (Running) | Le service fonctionne normalement |
| **Arrêt** (Stopping) | Le service est en train de s'arrêter |
| **Pause** (Paused) | Le service est temporairement suspendu |

### 1.3 Services vs Applications normales

#### Tableau comparatif

| Caractéristique | Application normale | Service Windows |
|-----------------|---------------------|-----------------|
| Interface utilisateur | Oui | Non (généralement) |
| Interaction utilisateur | Directe | Indirecte (logs, fichiers) |
| Démarrage | Manuel | Automatique ou manuel |
| Session utilisateur | Requise | Non requise |
| Droits | Utilisateur connecté | Système ou service |
| Visibilité | Visible à l'écran | Invisible (arrière-plan) |
| Débogage | Facile | Plus complexe |

#### Différences de code

**Application normale** :
```pascal
program NormalApp;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas';

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
```

**Service Windows** :
```pascal
program ServiceApp;

uses
  Vcl.SvcMgr,
  ServiceUnit in 'ServiceUnit.pas';

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
```

---

## Partie 2 : Création d'un service de base

### 2.1 Création du projet

**Étape 1 : Nouveau projet de service**

1. Dans Delphi : **Fichier → Nouveau → Autre...**
2. Sélectionnez **Service Application**
3. Donnez un nom au service : `MonitoringService`
4. Sauvegardez le projet

**Étape 2 : Configuration du service**

Delphi génère automatiquement deux fichiers :
- `MonitoringService.dpr` : Fichier projet
- `ServiceUnit.pas` : Unité du service

### 2.2 Structure de base du service

Examinons la structure générée :

```pascal
unit ServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr;

type
  TMonitoringService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
  private
    { Déclarations privées }
    FServiceThread: TThread;
  public
    function GetServiceController: TServiceController; override;
    { Déclarations publiques }
  end;

var
  MonitoringService: TMonitoringService;

implementation

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;  
begin  
  MonitoringService.Controller(CtrlCode);
end;

function TMonitoringService.GetServiceController: TServiceController;  
begin  
  Result := ServiceController;
end;

procedure TMonitoringService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  // Le service démarre
  Started := True;
end;

procedure TMonitoringService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  // Le service s'arrête
  Stopped := True;
end;

procedure TMonitoringService.ServicePause(Sender: TService;
  var Paused: Boolean);
begin
  // Le service se met en pause
  Paused := True;
end;

procedure TMonitoringService.ServiceContinue(Sender: TService;
  var Continued: Boolean);
begin
  // Le service reprend après une pause
  Continued := True;
end;

end.
```

**Explications** :

- **TService** : Classe de base pour tous les services
- **ServiceStart** : Appelé quand le service démarre
- **ServiceStop** : Appelé quand le service s'arrête
- **ServicePause/Continue** : Gestion de la pause
- **GetServiceController** : Point d'entrée du contrôle

### 2.3 Configuration des propriétés du service

Dans l'inspecteur d'objets, configurez :

**Propriétés importantes** :

```pascal
Name = MonitoringService  
DisplayName = 'Service de Monitoring'  
Description = 'Service qui surveille les fichiers système'  

// Type de démarrage
StartType = stAuto  // stManual, stAuto, stDisabled

// Contrôles acceptés
AllowPause = True  
AllowStop = True  

// Compte d'exécution
ServiceStartName = ''  // Vide = LocalSystem
```

**Types de démarrage** :

- **stManual** : Démarrage manuel uniquement
- **stAuto** : Démarrage automatique au démarrage de Windows
- **stDisabled** : Service désactivé

### 2.4 Implémentation d'un service simple

Créons un service qui surveille un dossier et écrit dans un log :

```pascal
unit ServiceUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.SvcMgr, System.IOUtils;

type
  // Thread de travail du service
  TMonitorThread = class(TThread)
  private
    FMonitorPath: string;
    FLogFile: string;
    procedure WriteLog(const AMessage: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const AMonitorPath, ALogFile: string);
  end;

  TMonitoringService = class(TService)
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    FMonitorThread: TMonitorThread;
    FLogFile: string;
    procedure InitializeService;
    procedure WriteLog(const AMessage: string);
  public
    function GetServiceController: TServiceController; override;
  end;

var
  MonitoringService: TMonitoringService;

implementation

{$R *.dfm}

{ TMonitorThread }

constructor TMonitorThread.Create(const AMonitorPath, ALogFile: string);  
begin  
  inherited Create(True); // Créer suspendu
  FreeOnTerminate := False;
  FMonitorPath := AMonitorPath;
  FLogFile := ALogFile;
end;

procedure TMonitorThread.WriteLog(const AMessage: string);  
var  
  LogEntry: string;
begin
  LogEntry := Format('[%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMessage]);

  // Écriture thread-safe
  TThread.Queue(nil,
    procedure
    begin
      TFile.AppendAllText(FLogFile, LogEntry + sLineBreak);
    end);
end;

procedure TMonitorThread.Execute;  
var  
  FileCount: Integer;
begin
  WriteLog('Thread de monitoring démarré');

  while not Terminated do
  begin
    try
      // Vérifier si le dossier existe
      if TDirectory.Exists(FMonitorPath) then
      begin
        // Compter les fichiers
        FileCount := Length(TDirectory.GetFiles(FMonitorPath));

        WriteLog(Format('Dossier surveillé : %d fichiers trouvés',
          [FileCount]));
      end
      else
      begin
        WriteLog('ATTENTION : Dossier non trouvé !');
      end;

      // Attendre 60 secondes avant la prochaine vérification
      if not Terminated then
        Sleep(60000);

    except
      on E: Exception do
        WriteLog('ERREUR: ' + E.Message);
    end;
  end;

  WriteLog('Thread de monitoring arrêté');
end;

{ TMonitoringService }

procedure ServiceController(CtrlCode: DWord); stdcall;  
begin  
  MonitoringService.Controller(CtrlCode);
end;

function TMonitoringService.GetServiceController: TServiceController;  
begin  
  Result := ServiceController;
end;

procedure TMonitoringService.InitializeService;  
var  
  BasePath: string;
begin
  // Chemin de base dans ProgramData
  BasePath := TPath.Combine(
    TPath.GetPublicPath,
    'MonitoringService'
  );

  // Créer le dossier s'il n'existe pas
  if not TDirectory.Exists(BasePath) then
    TDirectory.CreateDirectory(BasePath);

  // Fichier de log
  FLogFile := TPath.Combine(BasePath, 'service.log');
end;

procedure TMonitoringService.WriteLog(const AMessage: string);  
var  
  LogEntry: string;
begin
  if FLogFile = '' then
    Exit;

  LogEntry := Format('[%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMessage]);

  try
    TFile.AppendAllText(FLogFile, LogEntry + sLineBreak);
  except
    // Ignorer les erreurs d'écriture de log
  end;
end;

procedure TMonitoringService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  try
    InitializeService;
    WriteLog('=== Service démarré ===');

    // Créer et démarrer le thread de monitoring
    FMonitorThread := TMonitorThread.Create(
      'C:\Temp',  // Dossier à surveiller
      FLogFile
    );
    FMonitorThread.Start;

    WriteLog('Thread de monitoring lancé');
    Started := True;

  except
    on E: Exception do
    begin
      WriteLog('ERREUR au démarrage: ' + E.Message);
      Started := False;
    end;
  end;
end;

procedure TMonitoringService.ServiceStop(Sender: TService;
  var Stopped: Boolean);
begin
  try
    WriteLog('=== Arrêt du service demandé ===');

    // Arrêter le thread proprement
    if Assigned(FMonitorThread) then
    begin
      FMonitorThread.Terminate;
      FMonitorThread.WaitFor;
      FMonitorThread.Free;
      FMonitorThread := nil;
    end;

    WriteLog('=== Service arrêté ===');
    Stopped := True;

  except
    on E: Exception do
    begin
      WriteLog('ERREUR à l''arrêt: ' + E.Message);
      Stopped := True; // Arrêter quand même
    end;
  end;
end;

end.
```

**Points importants** :

1. **Thread séparé** : Le travail se fait dans un thread pour ne pas bloquer
2. **Journalisation** : Tout est enregistré dans un fichier log
3. **Gestion d'erreurs** : Les exceptions sont capturées et loguées
4. **Arrêt propre** : Le thread est arrêté correctement

---

## Partie 3 : Installation et gestion

### 3.1 Installation du service

Pour installer un service, vous avez plusieurs options :

#### Option 1 : Via ligne de commande (sc.exe)

```batch
REM Installer le service  
sc create MonitoringService binPath= "C:\MonServices\MonitoringService.exe" DisplayName= "Service de Monitoring"  

REM Configurer le démarrage automatique  
sc config MonitoringService start= auto  

REM Démarrer le service  
sc start MonitoringService  

REM Arrêter le service  
sc stop MonitoringService  

REM Désinstaller le service  
sc delete MonitoringService  
```

**Attention** : Notez l'espace après le `=` dans les commandes `sc`.

#### Option 2 : Programme d'installation intégré

Créez une application d'installation :

```pascal
program ServiceInstaller;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.WinSvc;

procedure InstallService(const AServiceName, ADisplayName, AFilePath: string);  
var  
  SCManager, ServiceHandle: SC_HANDLE;
begin
  // Ouvrir le gestionnaire de services
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    raise Exception.Create('Impossible d''ouvrir le gestionnaire de services');

  try
    // Créer le service
    ServiceHandle := CreateService(
      SCManager,
      PChar(AServiceName),
      PChar(ADisplayName),
      SERVICE_ALL_ACCESS,
      SERVICE_WIN32_OWN_PROCESS,
      SERVICE_AUTO_START,
      SERVICE_ERROR_NORMAL,
      PChar(AFilePath),
      nil, nil, nil, nil, nil
    );

    if ServiceHandle = 0 then
      raise Exception.Create('Impossible de créer le service');

    CloseServiceHandle(ServiceHandle);
    WriteLn('Service installé avec succès');

  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure UninstallService(const AServiceName: string);  
var  
  SCManager, ServiceHandle: SC_HANDLE;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then
    raise Exception.Create('Impossible d''ouvrir le gestionnaire de services');

  try
    // Ouvrir le service
    ServiceHandle := OpenService(SCManager, PChar(AServiceName),
      SERVICE_ALL_ACCESS);
    if ServiceHandle = 0 then
      raise Exception.Create('Service non trouvé');

    try
      // Supprimer le service
      if not DeleteService(ServiceHandle) then
        raise Exception.Create('Impossible de supprimer le service');

      WriteLn('Service désinstallé avec succès');
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure StartServiceProc(const AServiceName: string);  
var  
  SCManager, ServiceHandle: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;

  try
    ServiceHandle := OpenService(SCManager, PChar(AServiceName),
      SERVICE_START);
    if ServiceHandle = 0 then Exit;

    try
      if StartService(ServiceHandle, 0, nil) then
        WriteLn('Service démarré')
      else
        WriteLn('Erreur au démarrage du service');
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

procedure StopServiceProc(const AServiceName: string);  
var  
  SCManager, ServiceHandle: SC_HANDLE;
  ServiceStatus: TServiceStatus;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if SCManager = 0 then Exit;

  try
    ServiceHandle := OpenService(SCManager, PChar(AServiceName),
      SERVICE_STOP);
    if ServiceHandle = 0 then Exit;

    try
      if ControlService(ServiceHandle, SERVICE_CONTROL_STOP,
        ServiceStatus) then
        WriteLn('Service arrêté')
      else
        WriteLn('Erreur à l''arrêt du service');
    finally
      CloseServiceHandle(ServiceHandle);
    end;
  finally
    CloseServiceHandle(SCManager);
  end;
end;

var
  Command: string;
begin
  try
    if ParamCount = 0 then
    begin
      WriteLn('Usage:');
      WriteLn('  ServiceInstaller install   - Installe le service');
      WriteLn('  ServiceInstaller uninstall - Désinstalle le service');
      WriteLn('  ServiceInstaller start     - Démarre le service');
      WriteLn('  ServiceInstaller stop      - Arrête le service');
      Exit;
    end;

    Command := LowerCase(ParamStr(1));

    if Command = 'install' then
      InstallService(
        'MonitoringService',
        'Service de Monitoring',
        ParamStr(0).Replace('ServiceInstaller.exe', 'MonitoringService.exe')
      )
    else if Command = 'uninstall' then
      UninstallService('MonitoringService')
    else if Command = 'start' then
      StartServiceProc('MonitoringService')
    else if Command = 'stop' then
      StopServiceProc('MonitoringService')
    else
      WriteLn('Commande inconnue');

  except
    on E: Exception do
      WriteLn('ERREUR: ' + E.Message);
  end;

  ReadLn;
end.
```

**Utilisation** :

```batch
ServiceInstaller.exe install  
ServiceInstaller.exe start  
ServiceInstaller.exe stop  
ServiceInstaller.exe uninstall  
```

### 3.2 Gestion via PowerShell

```powershell
# Installer le service
New-Service -Name "MonitoringService" `
            -BinaryPathName "C:\MonServices\MonitoringService.exe" `
            -DisplayName "Service de Monitoring" `
            -StartupType Automatic

# Démarrer
Start-Service -Name "MonitoringService"

# Arrêter
Stop-Service -Name "MonitoringService"

# Vérifier l'état
Get-Service -Name "MonitoringService"

# Désinstaller
Remove-Service -Name "MonitoringService"
```

### 3.3 Vérification du service

**Via l'interface graphique** :

1. Appuyez sur **Win + R**
2. Tapez `services.msc`
3. Cherchez votre service dans la liste
4. Double-cliquez pour voir les propriétés

**Via PowerShell** :

```powershell
Get-Service | Where-Object {$_.Name -like "*Monitoring*"}
```

---

## Partie 4 : Débogage des services

### 4.1 Le défi du débogage

Les services sont difficiles à déboguer car :

❌ Pas d'interface utilisateur pour afficher des messages  
❌ Ne peuvent pas être lancés directement depuis l'IDE  
❌ S'exécutent dans une session différente  
❌ Nécessitent des privilèges spéciaux

### 4.2 Techniques de débogage

#### Technique 1 : Journalisation extensive

```pascal
type
  TLogger = class
  private
    class var FLogFile: string;
    class var FLock: TObject;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure Log(const ALevel, AMessage: string);
    class procedure Debug(const AMessage: string);
    class procedure Info(const AMessage: string);
    class procedure Warning(const AMessage: string);
    class procedure Error(const AMessage: string);
  end;

class constructor TLogger.Create;  
begin  
  FLock := TObject.Create;
  FLogFile := TPath.Combine(
    TPath.GetPublicPath,
    'MonitoringService\debug.log'
  );
end;

class destructor TLogger.Destroy;  
begin  
  FLock.Free;
end;

class procedure TLogger.Log(const ALevel, AMessage: string);  
var  
  LogEntry: string;
begin
  TMonitor.Enter(FLock);
  try
    LogEntry := Format('[%s] [%s] %s',
      [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
       ALevel,
       AMessage]);

    TFile.AppendAllText(FLogFile, LogEntry + sLineBreak);
  finally
    TMonitor.Exit(FLock);
  end;
end;

class procedure TLogger.Debug(const AMessage: string);  
begin  
  Log('DEBUG', AMessage);
end;

class procedure TLogger.Info(const AMessage: string);  
begin  
  Log('INFO', AMessage);
end;

class procedure TLogger.Warning(const AMessage: string);  
begin  
  Log('WARNING', AMessage);
end;

class procedure TLogger.Error(const AMessage: string);  
begin  
  Log('ERROR', AMessage);
end;

// Utilisation
procedure TMonitoringService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  TLogger.Info('ServiceStart - Début');
  try
    InitializeService;
    TLogger.Debug('InitializeService - OK');

    FMonitorThread := TMonitorThread.Create('C:\Temp', FLogFile);
    TLogger.Debug('Thread créé - OK');

    FMonitorThread.Start;
    TLogger.Debug('Thread démarré - OK');

    Started := True;
    TLogger.Info('ServiceStart - Service démarré avec succès');
  except
    on E: Exception do
    begin
      TLogger.Error('ServiceStart - Exception: ' + E.Message);
      TLogger.Error('ServiceStart - StackTrace: ' + E.StackTrace);
      Started := False;
    end;
  end;
end;
```

#### Technique 2 : Mode console pour développement

```pascal
program MonitoringService;

{$APPTYPE CONSOLE}  // Pour le développement

uses
  Vcl.SvcMgr,
  ServiceUnit in 'ServiceUnit.pas';

{$R *.RES}

var
  ConsoleMode: Boolean;

begin
  // Détecter le mode
  ConsoleMode := FindCmdLineSwitch('console', ['-', '/'], True);

  if ConsoleMode then
  begin
    // Mode console pour développement
    WriteLn('Mode console - Service de Monitoring');
    WriteLn('Appuyez sur ENTER pour arrêter...');

    // Simuler le service
    MonitoringService := TMonitoringService.Create(nil);
    try
      MonitoringService.ServiceStart(MonitoringService, ConsoleMode);
      ReadLn;
      MonitoringService.ServiceStop(MonitoringService, ConsoleMode);
    finally
      MonitoringService.Free;
    end;
  end
  else
  begin
    // Mode service normal
    Application.Initialize;
    Application.CreateForm(TMonitoringService, MonitoringService);
    Application.Run;
  end;
end.
```

**Utilisation** :

```batch
REM En développement  
MonitoringService.exe -console  

REM En production (via SCM)  
sc start MonitoringService  
```

#### Technique 3 : Attachement du débogueur

1. Installez et démarrez le service
2. Dans Delphi : **Exécuter → Attacher au processus**
3. Trouvez `MonitoringService.exe` dans la liste
4. Cliquez **Attacher**
5. Placez des points d'arrêt

**Limitations** : Ne fonctionne pas toujours selon les privilèges.

### 4.3 Journal d'événements Windows

Écrire dans le journal d'événements Windows :

```pascal
uses
  Winapi.Windows;

procedure WriteToEventLog(const AMessage: string; AEventType: Word);  
var  
  EventLog: THandle;
  Strings: array[0..0] of PChar;
begin
  EventLog := RegisterEventSource(nil, 'MonitoringService');
  if EventLog <> 0 then
  begin
    try
      Strings[0] := PChar(AMessage);
      ReportEvent(
        EventLog,
        AEventType,
        0,  // Category
        0,  // EventID
        nil,
        1,
        0,
        @Strings,
        nil
      );
    finally
      DeregisterEventSource(EventLog);
    end;
  end;
end;

// Utilisation
procedure LogInfo(const AMessage: string);  
begin  
  WriteToEventLog(AMessage, EVENTLOG_INFORMATION_TYPE);
end;

procedure LogWarning(const AMessage: string);  
begin  
  WriteToEventLog(AMessage, EVENTLOG_WARNING_TYPE);
end;

procedure LogError(const AMessage: string);  
begin  
  WriteToEventLog(AMessage, EVENTLOG_ERROR_TYPE);
end;

// Dans le service
procedure TMonitoringService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  LogInfo('Service démarré');
  // ... code ...
end;
```

**Consulter les logs** :

1. **Win + R** → `eventvwr.msc`
2. **Journaux Windows → Application**
3. Filtrez par source : `MonitoringService`

---

## Partie 5 : Fonctionnalités avancées

### 5.1 Communication inter-processus (IPC)

Permettre aux applications de communiquer avec le service.

#### Via fichiers de commandes

```pascal
type
  TCommandProcessor = class
  private
    FCommandPath: string;
    procedure ProcessCommand(const ACommand: string);
  public
    constructor Create(const ACommandPath: string);
    procedure CheckCommands;
  end;

constructor TCommandProcessor.Create(const ACommandPath: string);  
begin  
  inherited Create;
  FCommandPath := ACommandPath;

  // Créer le dossier de commandes
  if not TDirectory.Exists(FCommandPath) then
    TDirectory.CreateDirectory(FCommandPath);
end;

procedure TCommandProcessor.CheckCommands;  
var  
  Files: TArray<string>;
  CommandFile: string;
  Command: string;
begin
  Files := TDirectory.GetFiles(FCommandPath, '*.cmd');

  for CommandFile in Files do
  begin
    try
      // Lire la commande
      Command := TFile.ReadAllText(CommandFile);

      // Traiter
      ProcessCommand(Command);

      // Supprimer le fichier de commande
      TFile.Delete(CommandFile);

    except
      on E: Exception do
        TLogger.Error('Erreur traitement commande: ' + E.Message);
    end;
  end;
end;

procedure TCommandProcessor.ProcessCommand(const ACommand: string);  
begin  
  TLogger.Info('Commande reçue: ' + ACommand);

  if ACommand.StartsWith('SCAN:') then
  begin
    // Exemple: SCAN:C:\Temp
    var Path := ACommand.Substring(5);
    TLogger.Info('Scan demandé pour: ' + Path);
    // Effectuer le scan...
  end
  else if ACommand = 'STATUS' then
  begin
    // Écrire le statut
    var StatusFile := TPath.Combine(FCommandPath, 'status.txt');
    TFile.WriteAllText(StatusFile, 'Service actif - OK');
  end;
end;

// Dans le thread du service
procedure TMonitorThread.Execute;  
var  
  CommandProcessor: TCommandProcessor;
begin
  CommandProcessor := TCommandProcessor.Create(
    'C:\ProgramData\MonitoringService\Commands'
  );
  try
    while not Terminated do
    begin
      // Vérifier les commandes toutes les 5 secondes
      CommandProcessor.CheckCommands;

      // Travail normal du service
      DoMonitoring;

      Sleep(5000);
    end;
  finally
    CommandProcessor.Free;
  end;
end;
```

**Application cliente** :

```pascal
procedure SendCommandToService(const ACommand: string);  
var  
  CommandFile: string;
begin
  CommandFile := Format('C:\ProgramData\MonitoringService\Commands\%s.cmd',
    [FormatDateTime('yyyymmddhhnnsszzz', Now)]);

  TFile.WriteAllText(CommandFile, ACommand);
end;

// Utilisation
SendCommandToService('SCAN:C:\Temp');  
SendCommandToService('STATUS');  
```

#### Via Named Pipes

```pascal
uses
  Winapi.Windows, System.Classes;

type
  TNamedPipeServer = class(TThread)
  private
    FPipeName: string;
    FOnCommand: TProc<string>;
  protected
    procedure Execute; override;
  public
    constructor Create(const APipeName: string; AOnCommand: TProc<string>);
  end;

constructor TNamedPipeServer.Create(const APipeName: string;
  AOnCommand: TProc<string>);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FPipeName := '\\.\pipe\' + APipeName;
  FOnCommand := AOnCommand;
end;

procedure TNamedPipeServer.Execute;  
var  
  PipeHandle: THandle;
  Buffer: array[0..1023] of AnsiChar;
  BytesRead: DWORD;
  Command: string;
begin
  while not Terminated do
  begin
    // Créer le pipe
    PipeHandle := CreateNamedPipe(
      PChar(FPipeName),
      PIPE_ACCESS_DUPLEX,
      PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
      1,
      1024,
      1024,
      0,
      nil
    );

    if PipeHandle <> INVALID_HANDLE_VALUE then
    begin
      try
        // Attendre une connexion
        if ConnectNamedPipe(PipeHandle, nil) then
        begin
          // Lire les données
          if ReadFile(PipeHandle, Buffer, SizeOf(Buffer), BytesRead, nil) then
          begin
            SetString(Command, PAnsiChar(@Buffer), BytesRead);

            // Traiter la commande
            if Assigned(FOnCommand) then
            begin
              TThread.Synchronize(nil,
                procedure
                begin
                  FOnCommand(Command);
                end);
            end;
          end;
        end;
      finally
        DisconnectNamedPipe(PipeHandle);
        CloseHandle(PipeHandle);
      end;
    end;
  end;
end;

// Dans le service
var
  PipeServer: TNamedPipeServer;

procedure TMonitoringService.ServiceStart(Sender: TService;
  var Started: Boolean);
begin
  // ...

  PipeServer := TNamedPipeServer.Create('MonitoringService',
    procedure(const ACommand: string)
    begin
      TLogger.Info('Commande reçue via pipe: ' + ACommand);
      ProcessCommand(ACommand);
    end);
  PipeServer.Start;

  Started := True;
end;
```

**Client pour Named Pipe** :

```pascal
function SendCommandViaPipe(const ACommand: string): Boolean;  
var  
  PipeHandle: THandle;
  BytesWritten: DWORD;
  Buffer: AnsiString;
begin
  Result := False;
  Buffer := AnsiString(ACommand);

  PipeHandle := CreateFile(
    '\\.\pipe\MonitoringService',
    GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if PipeHandle <> INVALID_HANDLE_VALUE then
  begin
    try
      Result := WriteFile(PipeHandle, Buffer[1], Length(Buffer),
        BytesWritten, nil);
    finally
      CloseHandle(PipeHandle);
    end;
  end;
end;
```

### 5.2 Configuration dynamique

Permettre la configuration sans redémarrer le service :

```pascal
type
  TServiceConfig = class
  private
    FConfigFile: string;
    FMonitorPath: string;
    FCheckInterval: Integer;
    FMaxFileSize: Int64;
    FLastModified: TDateTime;
    procedure LoadConfig;
  public
    constructor Create(const AConfigFile: string);

    function HasChanged: Boolean;
    procedure Reload;

    property MonitorPath: string read FMonitorPath;
    property CheckInterval: Integer read FCheckInterval;
    property MaxFileSize: Int64 read FMaxFileSize;
  end;

constructor TServiceConfig.Create(const AConfigFile: string);  
begin  
  inherited Create;
  FConfigFile := AConfigFile;
  LoadConfig;
end;

procedure TServiceConfig.LoadConfig;  
var  
  JSON: TJSONObject;
  JSONText: string;
begin
  if TFile.Exists(FConfigFile) then
  begin
    JSONText := TFile.ReadAllText(FConfigFile);
    JSON := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
    try
      FMonitorPath := JSON.GetValue<string>('monitorPath');
      FCheckInterval := JSON.GetValue<Integer>('checkInterval');
      FMaxFileSize := JSON.GetValue<Int64>('maxFileSize');

      FLastModified := TFile.GetLastWriteTime(FConfigFile);
    finally
      JSON.Free;
    end;
  end
  else
  begin
    // Valeurs par défaut
    FMonitorPath := 'C:\Temp';
    FCheckInterval := 60;
    FMaxFileSize := 10485760; // 10 MB
  end;
end;

function TServiceConfig.HasChanged: Boolean;  
begin  
  Result := TFile.GetLastWriteTime(FConfigFile) <> FLastModified;
end;

procedure TServiceConfig.Reload;  
begin  
  LoadConfig;
  TLogger.Info('Configuration rechargée');
end;

// Dans le thread du service
procedure TMonitorThread.Execute;  
begin  
  while not Terminated do
  begin
    // Vérifier si la config a changé
    if FConfig.HasChanged then
    begin
      FConfig.Reload;
      // Ajuster les paramètres
    end;

    // Travail normal
    DoMonitoring;

    Sleep(FConfig.CheckInterval * 1000);
  end;
end;
```

**Fichier config.json** :

```json
{
  "monitorPath": "C:\\Temp",
  "checkInterval": 60,
  "maxFileSize": 10485760,
  "notifications": true,
  "emailAlerts": "admin@example.com"
}
```

### 5.3 Tâches planifiées intégrées

```pascal
type
  TScheduledTask = class
  private
    FName: string;
    FSchedule: TTime;
    FLastRun: TDateTime;
    FAction: TProc;
  public
    constructor Create(const AName: string; ASchedule: TTime;
      AAction: TProc);

    function ShouldRun: Boolean;
    procedure Execute;

    property Name: string read FName;
  end;

  TTaskScheduler = class
  private
    FTasks: TObjectList<TScheduledTask>;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTask(ATask: TScheduledTask);
    procedure CheckAndRun;
  end;

constructor TScheduledTask.Create(const AName: string; ASchedule: TTime;
  AAction: TProc);
begin
  inherited Create;
  FName := AName;
  FSchedule := ASchedule;
  FAction := AAction;
  FLastRun := 0;
end;

function TScheduledTask.ShouldRun: Boolean;  
var  
  CurrentTime: TTime;
  LastRunDate: TDate;
begin
  CurrentTime := Time;
  LastRunDate := DateOf(FLastRun);

  // Doit s'exécuter si:
  // 1. Jamais exécuté (FLastRun = 0)
  // 2. Heure actuelle >= heure planifiée ET pas déjà exécuté aujourd'hui
  Result := (FLastRun = 0) or
            ((CurrentTime >= FSchedule) and (LastRunDate < Date));
end;

procedure TScheduledTask.Execute;  
begin  
  TLogger.Info(Format('Exécution tâche planifiée: %s', [FName]));

  try
    if Assigned(FAction) then
      FAction;

    FLastRun := Now;
    TLogger.Info(Format('Tâche terminée: %s', [FName]));
  except
    on E: Exception do
      TLogger.Error(Format('Erreur dans tâche %s: %s',
        [FName, E.Message]));
  end;
end;

// Configuration des tâches
procedure TMonitoringService.SetupScheduledTasks;  
begin  
  FScheduler := TTaskScheduler.Create;

  // Sauvegarde quotidienne à 2h du matin
  FScheduler.AddTask(
    TScheduledTask.Create(
      'Sauvegarde quotidienne',
      EncodeTime(2, 0, 0, 0),
      procedure
      begin
        PerformDailyBackup;
      end
    )
  );

  // Nettoyage des logs à 3h
  FScheduler.AddTask(
    TScheduledTask.Create(
      'Nettoyage logs',
      EncodeTime(3, 0, 0, 0),
      procedure
      begin
        CleanOldLogs;
      end
    )
  );

  // Rapport hebdomadaire le lundi à 8h
  FScheduler.AddTask(
    TScheduledTask.Create(
      'Rapport hebdomadaire',
      EncodeTime(8, 0, 0, 0),
      procedure
      begin
        if DayOfWeek(Now) = 2 then // Lundi
          GenerateWeeklyReport;
      end
    )
  );
end;

// Dans le thread
procedure TMonitorThread.Execute;  
begin  
  while not Terminated do
  begin
    // Vérifier les tâches planifiées
    FScheduler.CheckAndRun;

    // Travail normal
    DoMonitoring;

    Sleep(60000); // Vérifier chaque minute
  end;
end;
```

---

## Partie 6 : Exemples de services pratiques

### 6.1 Service de surveillance de dossier

Un service qui surveille un dossier et réagit aux changements :

```pascal
uses
  System.IOUtils, System.SysUtils;

type
  TFileMonitorService = class(TService)
  private
    FWatchPath: string;
    FWatcher: TThread;
  protected
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  end;

  TFileWatcherThread = class(TThread)
  private
    FPath: string;
    FLastFiles: TArray<string>;
    procedure CheckForChanges;
    procedure OnFileAdded(const AFileName: string);
    procedure OnFileRemoved(const AFileName: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const APath: string);
  end;

constructor TFileWatcherThread.Create(const APath: string);  
begin  
  inherited Create(True);
  FreeOnTerminate := False;
  FPath := APath;
  FLastFiles := TDirectory.GetFiles(FPath);
end;

procedure TFileWatcherThread.CheckForChanges;  
var  
  CurrentFiles: TArray<string>;
  FileName: string;
  Found: Boolean;
begin
  CurrentFiles := TDirectory.GetFiles(FPath);

  // Chercher les nouveaux fichiers
  for FileName in CurrentFiles do
  begin
    Found := False;
    for var LastFile in FLastFiles do
    begin
      if SameText(FileName, LastFile) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      OnFileAdded(FileName);
  end;

  // Chercher les fichiers supprimés
  for FileName in FLastFiles do
  begin
    Found := False;
    for var CurrentFile in CurrentFiles do
    begin
      if SameText(FileName, CurrentFile) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      OnFileRemoved(FileName);
  end;

  FLastFiles := CurrentFiles;
end;

procedure TFileWatcherThread.OnFileAdded(const AFileName: string);  
begin  
  TLogger.Info('Nouveau fichier détecté: ' + AFileName);

  // Actions possibles:
  // - Copier vers un autre dossier
  // - Scanner pour virus
  // - Indexer le contenu
  // - Envoyer une notification
end;

procedure TFileWatcherThread.OnFileRemoved(const AFileName: string);  
begin  
  TLogger.Info('Fichier supprimé: ' + AFileName);
end;

procedure TFileWatcherThread.Execute;  
begin  
  TLogger.Info('Surveillance démarrée: ' + FPath);

  while not Terminated do
  begin
    try
      CheckForChanges;
    except
      on E: Exception do
        TLogger.Error('Erreur surveillance: ' + E.Message);
    end;

    Sleep(5000); // Vérifier toutes les 5 secondes
  end;
end;
```

### 6.2 Service de synchronisation automatique

```pascal
type
  TSyncService = class(TService)
  private
    FSyncThread: TThread;
  protected
    procedure ServiceStart(Sender: TService; var Started: Boolean);
  end;

  TSyncThread = class(TThread)
  private
    FSourcePath: string;
    FTargetPath: string;
    procedure SyncFolders;
    procedure CopyFile(const ASource, ATarget: string);
  protected
    procedure Execute; override;
  public
    constructor Create(const ASource, ATarget: string);
  end;

procedure TSyncThread.SyncFolders;  
var  
  SourceFiles: TArray<string>;
  SourceFile, TargetFile: string;
  SourceTime, TargetTime: TDateTime;
begin
  TLogger.Info('Début synchronisation');

  SourceFiles := TDirectory.GetFiles(FSourcePath, '*.*',
    TSearchOption.soAllDirectories);

  for SourceFile in SourceFiles do
  begin
    if Terminated then
      Break;

    // Calculer le chemin cible
    TargetFile := SourceFile.Replace(FSourcePath, FTargetPath);

    // Créer les dossiers si nécessaire
    ForceDirectories(TPath.GetDirectoryName(TargetFile));

    // Vérifier si copie nécessaire
    if not TFile.Exists(TargetFile) then
    begin
      CopyFile(SourceFile, TargetFile);
    end
    else
    begin
      SourceTime := TFile.GetLastWriteTime(SourceFile);
      TargetTime := TFile.GetLastWriteTime(TargetFile);

      if SourceTime > TargetTime then
        CopyFile(SourceFile, TargetFile);
    end;
  end;

  TLogger.Info('Synchronisation terminée');
end;

procedure TSyncThread.CopyFile(const ASource, ATarget: string);  
begin  
  try
    TFile.Copy(ASource, ATarget, True);
    TLogger.Debug('Copié: ' + ExtractFileName(ASource));
  except
    on E: Exception do
      TLogger.Error('Erreur copie ' + ASource + ': ' + E.Message);
  end;
end;

procedure TSyncThread.Execute;  
begin  
  while not Terminated do
  begin
    SyncFolders;

    // Synchroniser toutes les heures
    Sleep(3600000);
  end;
end;
```

### 6.3 Service de nettoyage automatique

```pascal
type
  TCleanupService = class(TService)
  private
    FCleanupThread: TThread;
  end;

  TCleanupThread = class(TThread)
  private
    procedure CleanOldFiles(const APath: string; ADaysOld: Integer);
    procedure CleanTempFiles;
    procedure CleanLogFiles;
  protected
    procedure Execute; override;
  end;

procedure TCleanupThread.CleanOldFiles(const APath: string;
  ADaysOld: Integer);
var
  Files: TArray<string>;
  FileName: string;
  FileAge: TDateTime;
  CutoffDate: TDateTime;
  DeletedCount: Integer;
begin
  DeletedCount := 0;
  CutoffDate := Now - ADaysOld;

  Files := TDirectory.GetFiles(APath, '*.*',
    TSearchOption.soAllDirectories);

  for FileName in Files do
  begin
    try
      FileAge := TFile.GetLastWriteTime(FileName);

      if FileAge < CutoffDate then
      begin
        TFile.Delete(FileName);
        Inc(DeletedCount);
        TLogger.Debug('Supprimé: ' + FileName);
      end;
    except
      on E: Exception do
        TLogger.Error('Erreur suppression ' + FileName + ': ' + E.Message);
    end;
  end;

  TLogger.Info(Format('Nettoyage %s: %d fichiers supprimés',
    [APath, DeletedCount]));
end;

procedure TCleanupThread.CleanTempFiles;  
begin  
  TLogger.Info('Nettoyage fichiers temporaires');
  CleanOldFiles(TPath.GetTempPath, 7); // Supprimer > 7 jours
end;

procedure TCleanupThread.CleanLogFiles;  
begin  
  TLogger.Info('Nettoyage anciens logs');
  CleanOldFiles('C:\Logs', 30); // Supprimer > 30 jours
end;

procedure TCleanupThread.Execute;  
begin  
  while not Terminated do
  begin
    try
      CleanTempFiles;
      CleanLogFiles;
    except
      on E: Exception do
        TLogger.Error('Erreur nettoyage: ' + E.Message);
    end;

    // Nettoyage quotidien à 3h du matin
    Sleep(3600000); // Vérifier chaque heure
  end;
end;
```

---

## Partie 7 : Déploiement et maintenance

### 7.1 Package d'installation

Créez un installateur avec Inno Setup :

```ini
[Setup]
AppName=Monitoring Service  
AppVersion=1.0  
DefaultDirName={pf}\MonitoringService  
PrivilegesRequired=admin  
OutputDir=Output  
OutputBaseFilename=MonitoringService_Setup  

[Files]
Source: "MonitoringService.exe"; DestDir: "{app}"  
Source: "ServiceInstaller.exe"; DestDir: "{app}"  
Source: "config.json"; DestDir: "{commonappdata}\MonitoringService"  

[Run]
Filename: "{app}\ServiceInstaller.exe"; Parameters: "install"; \
  StatusMsg: "Installation du service..."; Flags: runhidden

[UninstallRun]
Filename: "{app}\ServiceInstaller.exe"; Parameters: "stop"; \
  Flags: runhidden
Filename: "{app}\ServiceInstaller.exe"; Parameters: "uninstall"; \
  Flags: runhidden
```

### 7.2 Scripts de maintenance

**Vérifier l'état du service** :

```batch
@echo off
echo Verification du service...  
sc query MonitoringService  

echo.  
echo Logs recents:  
type "C:\ProgramData\MonitoringService\service.log" | more  

pause
```

**Redémarrer le service** :

```batch
@echo off
echo Arret du service...  
sc stop MonitoringService  
timeout /t 5  

echo Demarrage du service...  
sc start MonitoringService  

echo.  
echo Etat du service:  
sc query MonitoringService  

pause
```

### 7.3 Monitoring du service

Script PowerShell de surveillance :

```powershell
# MonitorService.ps1

$ServiceName = "MonitoringService"
$LogFile = "C:\Logs\ServiceMonitor.log"

function Write-Log {
    param($Message)
    $Timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    "$Timestamp - $Message" | Out-File -FilePath $LogFile -Append
}

function Check-Service {
    $Service = Get-Service -Name $ServiceName -ErrorAction SilentlyContinue

    if ($null -eq $Service) {
        Write-Log "ERREUR: Service non trouve"
        return $false
    }

    if ($Service.Status -ne "Running") {
        Write-Log "ATTENTION: Service arrete - Tentative redemarrage"

        try {
            Start-Service -Name $ServiceName
            Write-Log "Service redémarre avec succes"
            return $true
        }
        catch {
            Write-Log "ERREUR: Impossible de redemarrer - $($_.Exception.Message)"
            # Envoyer une alerte email ici
            return $false
        }
    }
    else {
        Write-Log "Service OK - En cours d'execution"
        return $true
    }
}

# Boucle de surveillance
while ($true) {
    Check-Service
    Start-Sleep -Seconds 300  # Vérifier toutes les 5 minutes
}
```

**Planifier avec Task Scheduler** :

```powershell
$Action = New-ScheduledTaskAction -Execute "PowerShell.exe" `
    -Argument "-File C:\Scripts\MonitorService.ps1"

$Trigger = New-ScheduledTaskTrigger -AtStartup

$Principal = New-ScheduledTaskPrincipal -UserId "SYSTEM" `
    -LogonType ServiceAccount -RunLevel Highest

Register-ScheduledTask -TaskName "MonitorServiceHealth" `
    -Action $Action -Trigger $Trigger -Principal $Principal
```

### 7.4 Mises à jour du service

```pascal
// Dans le service, vérifier les mises à jour
procedure CheckForUpdates;  
var  
  CurrentVersion: string;
  LatestVersion: string;
begin
  CurrentVersion := '1.0.0';

  // Récupérer la dernière version depuis un serveur
  LatestVersion := GetLatestVersionFromServer;

  if LatestVersion <> CurrentVersion then
  begin
    TLogger.Warning('Mise à jour disponible: ' + LatestVersion);

    // Télécharger et appliquer
    if DownloadUpdate(LatestVersion) then
      ScheduleUpdate; // Planifier pour le prochain redémarrage
  end;
end;

procedure ScheduleUpdate;  
begin  
  // Créer un fichier marqueur
  TFile.WriteAllText(
    'C:\ProgramData\MonitoringService\update.pending',
    'Mise à jour en attente'
  );

  TLogger.Info('Mise à jour planifiée');
end;
```

---

## Partie 8 : Sécurité et bonnes pratiques

### 8.1 Sécurité

#### Exécuter avec des privilèges minimaux

```pascal
// Configurer pour s'exécuter avec un compte spécifique
// plutôt que LocalSystem

// Dans ServiceInstaller
procedure InstallServiceWithAccount(const AUsername, APassword: string);  
begin  
  ServiceHandle := CreateService(
    SCManager,
    PChar('MonitoringService'),
    PChar('Service de Monitoring'),
    SERVICE_ALL_ACCESS,
    SERVICE_WIN32_OWN_PROCESS,
    SERVICE_AUTO_START,
    SERVICE_ERROR_NORMAL,
    PChar(ExePath),
    nil, nil, nil,
    PChar(AUsername),  // Compte utilisateur
    PChar(APassword)   // Mot de passe
  );
end;
```

#### Validation des entrées

```pascal
procedure ProcessCommand(const ACommand: string);  
begin  
  // Valider la commande
  if ACommand.IsEmpty or (Length(ACommand) > 1000) then
  begin
    TLogger.Warning('Commande invalide ignorée');
    Exit;
  end;

  // Nettoyer les caractères dangereux
  var SafeCommand := ACommand.Replace(';', '')
                              .Replace('&', '')
                              .Replace('|', '');

  // Traiter la commande sécurisée
  ExecuteSafeCommand(SafeCommand);
end;
```

#### Chiffrement des données sensibles

```pascal
uses
  System.NetEncoding;

function EncryptData(const AData: string): string;  
begin  
  // Utiliser un vrai algorithme de chiffrement en production
  Result := TNetEncoding.Base64.Encode(AData);
end;

function DecryptData(const AData: string): string;  
begin  
  Result := TNetEncoding.Base64.Decode(AData);
end;
```

### 8.2 Gestion des ressources

```pascal
// Limiter l'utilisation mémoire
procedure CheckMemoryUsage;  
var  
  MemStatus: TMemoryStatus;
begin
  MemStatus.dwLength := SizeOf(TMemoryStatus);
  GlobalMemoryStatus(MemStatus);

  var UsedMemory := MemStatus.dwTotalPhys - MemStatus.dwAvailPhys;
  var MemoryPercent := (UsedMemory * 100) div MemStatus.dwTotalPhys;

  if MemoryPercent > 80 then
  begin
    TLogger.Warning('Utilisation mémoire élevée: ' +
      MemoryPercent.ToString + '%');

    // Libérer des ressources si possible
    FreeUnusedMemory;
  end;
end;

// Rotation des logs
procedure RotateLogFiles;  
var  
  LogFile: string;
  LogSize: Int64;
  MaxSize: Int64;
begin
  MaxSize := 10 * 1024 * 1024; // 10 MB
  LogFile := 'C:\ProgramData\MonitoringService\service.log';

  if TFile.Exists(LogFile) then
  begin
    LogSize := TFile.GetSize(LogFile);

    if LogSize > MaxSize then
    begin
      // Renommer l'ancien log
      var BackupFile := ChangeFileExt(LogFile,
        Format('.%s.log', [FormatDateTime('yyyymmdd', Now)]));
      TFile.Move(LogFile, BackupFile);

      TLogger.Info('Log rotaté: ' + BackupFile);
    end;
  end;
end;
```

### 8.3 Gestion d'erreurs robuste

```pascal
procedure TMonitorThread.Execute;  
var  
  ErrorCount: Integer;
  MaxErrors: Integer;
begin
  ErrorCount := 0;
  MaxErrors := 10;

  while not Terminated do
  begin
    try
      DoWork;
      ErrorCount := 0; // Réinitialiser en cas de succès

    except
      on E: Exception do
      begin
        Inc(ErrorCount);
        TLogger.Error(Format('Erreur (%d/%d): %s',
          [ErrorCount, MaxErrors, E.Message]));

        if ErrorCount >= MaxErrors then
        begin
          TLogger.Error('Trop d''erreurs - Arrêt du service');
          Break;
        end;

        // Attendre avant de réessayer
        Sleep(5000 * ErrorCount); // Backoff exponentiel
      end;
    end;
  end;
end;
```

---

## Conclusion

### Ce que vous avez appris

Félicitations ! Vous maîtrisez maintenant la création et la gestion de services Windows avec Delphi. Vous avez acquis des compétences dans :

✅ **Architecture des services** : Comprendre le cycle de vie et les composants  
✅ **Création de services** : Développer des services fonctionnels  
✅ **Installation/Désinstallation** : Gérer le déploiement  
✅ **Débogage** : Techniques de journalisation et debug  
✅ **Communication IPC** : Fichiers, pipes, et autres méthodes  
✅ **Configuration** : Paramétrage dynamique  
✅ **Tâches planifiées** : Automatisation temporelle  
✅ **Sécurité** : Bonnes pratiques et protection  
✅ **Maintenance** : Monitoring et mises à jour

### Compétences acquises

Vous êtes maintenant capable de :

🎯 Créer des services Windows professionnels  
🎯 Implémenter des tâches automatisées en arrière-plan  
🎯 Gérer le cycle de vie complet d'un service  
🎯 Déboguer efficacement les services  
🎯 Sécuriser et maintenir vos services  
🎯 Intégrer des services dans l'écosystème Windows

### Applications pratiques

Les services que vous pouvez créer :

- **Monitoring système** : Surveillance de ressources
- **Sauvegarde automatique** : Backup planifié
- **Synchronisation** : Réplication de données
- **Serveurs** : HTTP, FTP, bases de données
- **Traitement batch** : Files d'attente de tâches
- **Collecte de logs** : Agrégation centralisée
- **Nettoyage** : Maintenance automatique

### Ressources complémentaires

**Documentation** :
- [MSDN - Windows Services](https://docs.microsoft.com/en-us/windows/win32/services)
- [Delphi Service Application](https://docwiki.embarcadero.com/RADStudio/en/Services)

**Outils** :
- **services.msc** : Gestionnaire de services
- **sc.exe** : Utilitaire en ligne de commande
- **Process Explorer** : Sysinternals
- **Event Viewer** : Journal d'événements

### Prochaines étapes

Maintenant que vous maîtrisez les services Windows, explorez :

- **19.5 Applications cloud et SaaS** : Services web et API
- **19.7 Projets IA et Machine Learning** : Services intelligents
- **Containerisation** : Docker et services modernes

### Message final

Les services Windows sont un pilier de l'infrastructure Windows. Ils permettent de créer des solutions robustes, fiables et automatisées qui fonctionnent 24h/24. Avec Delphi, vous disposez d'outils puissants pour créer des services performants et maintenables.

**Conseils pour réussir** :
- Testez toujours en conditions réelles
- Implémentez une journalisation extensive
- Gérez les erreurs de manière robuste
- Documentez votre service
- Surveillez les performances en production

**Bon développement de services avec Delphi !** ⚙️🔧

---

⏭️ [Applications cloud et SaaS](/19-projets-avances/05-applications-cloud-et-saas.md)
